#include <stdlib.h>
#include <cstring>
#include <usart.h>
#include <redirect.h>
#include <button.h>
#include <timer.h>
#include <i2c.h>

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

static uint8_t slave_address = 0x5a;
static uint8_t recv_buf[32];

static volatile uint8_t fire = 0;
static volatile uint32_t sts = 0;

template<> void handler<interrupt::TIM3>()
{
    aux::clear_uif();
    btn::update();
}

template<> void handler<interrupt::I2C2>()
{
    probe::toggle();
    sts = slave::isr();
        if (sts != 0)
            printf("sts = %lx\n", sts);
}

static void slave_callback(uint8_t *buf, uint8_t len)
{
    led::toggle();
    fire = len;
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
    slave::setup(slave_address, slave_callback, recv_buf, sizeof(recv_buf));
    hal::nvic<interrupt::I2C2>::enable();
    printf("Welcome to the STM32G070!\n");

    bool b = true;

    for (;;)
    {
        if (btn::read())
        {
            if (b)
            {
                static uint8_t buf[] = { 0xd, 0xe, 0xa, 0xd, 0xb, 0xe, 0xe, 0xf };
                master::write(slave_address, buf, sizeof(buf));
            }
            else
            {
                static uint8_t buf[32], len = 1;

                master::read(slave_address, buf, len);
            }
            b = !b;
        }

        if (fire)
        {
            for (int i = 0; i < fire; ++i)
                printf("%x%s", recv_buf[i], i + 1 == fire ? "\n" : "");
            fire = 0;
        }
    }
}

