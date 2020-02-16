#include <stdlib.h>
#include <cstring>
#include <usart.h>
#include <redirect.h>
#include <button.h>
#include <timer.h>
#include <i2c.h>
#include <fifo.h>

using hal::sys_tick;
using hal::sys_clock;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::i2c;
using namespace hal::timer;

typedef button_t<PC13> btn;
typedef hal::timer::timer_t<3> aux;
typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA5> led;
typedef output_t<PD9> probe;
typedef i2c_master_t<1, PB8, PB9> master;
typedef i2c_slave_t<2, PB13, PB14> slave;
typedef fifo_t<uint32_t, 0, 32> fifo;

static uint8_t slave_address = 0x5a;
static uint8_t slave_rxbuf[16], slave_txbuf[16] = { 0xff, 0xff, 0xff, 0xff };

static volatile uint8_t fire = 0;

template<> void handler<interrupt::TIM3>()
{
    aux::clear_uif();
    btn::update();
}

template<> void handler<interrupt::I2C2>()
{
    probe::toggle();
    fifo::put(device::I2C2.ISR);
    slave::isr();
}

static uint8_t slave_callback(uint8_t len)
{
    for (uint32_t i = 0; i < 1; ++i)
        led::toggle();
    fire = len;

    for (uint8_t i = 0; i < len; ++i)
        slave_txbuf[i] = ~slave_rxbuf[i];

    return sizeof(slave_txbuf);
}

static void show_buf(const char *s, const uint8_t *buf, uint8_t len)
{
    printf("%s[] = {", s);
    for (uint8_t i = 0; i < len; ++i)
        printf("%s%02x", i ? "," : "", buf[i]);
    printf("}\n");
}

int main()
{
    serial::setup<115200>();
    stdio_t::bind<serial>();
    led::setup();
    btn::setup<pull_up>();
    probe::setup();
    aux::setup(100, 1000);
    aux::update_interrupt_enable();
    hal::nvic<interrupt::TIM3>::enable();
    master::setup();
    slave::setup(slave_address, slave_callback, slave_rxbuf, sizeof(slave_rxbuf), slave_txbuf);
    hal::nvic<interrupt::I2C2>::enable();
    printf("Welcome to the STM32G070!\n");
    printf("--------------------------------------------\n");

    uint8_t mode = 0;

    static uint8_t txbuf[16] = { 0xd, 0xe, 0xa, 0xd, 0xb, 0xe, 0xe, 0xf }, rxbuf[16];

    for (;;)
    {
        if (btn::read())
        {
            uint8_t rxlen = 0;

            printf("mode = %d\n", mode);

            switch (mode)
            {
                case 1: master::write(slave_address, txbuf, 8); break;
                case 0: master::read(slave_address, rxbuf, rxlen = 2); break;
                case 2: master::write_read(slave_address, txbuf, 2, rxbuf, rxlen = 2); break;
            }

            show_buf("master_rxbuf", rxbuf, rxlen);
            show_buf("slave_rxbuf", slave_rxbuf, sizeof(slave_rxbuf));
            show_buf("slave_txbuf", slave_txbuf, sizeof(slave_txbuf));

            uint32_t s;

            for (int i = 0; fifo::get(s); ++i)
                printf("%02d %lx\n", i, s);

            printf("--------------------------------------------\n");
            mode = mode < 2 ? mode + 1 : 0;

            for (uint8_t i = 0; i < sizeof(txbuf); ++i)
                txbuf[i]++;
        }
    }
}

