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

typedef button_t<PD8> btn;              // not on-board!
typedef hal::timer::timer_t<3> aux;
typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA5> led;
typedef output_t<PD9> probe;
typedef i2c_t<1, PB8, PB9> i2c1;
typedef i2c_t<2, PB13, PB14> i2c2;

static uint8_t slave_address = 0x5a;

static volatile uint32_t x = 0;

template<> void handler<interrupt::TIM3>()
{
    aux::clear_uif();
    btn::update();
}

template<> void handler<interrupt::I2C1>()
{
    probe::toggle();

    if (i2c1::read_flag<i2c1::sts_address_matched>())
    {
        i2c1::clear_flag<i2c1::sts_address_matched>();
        //x = i2c1::matched_address();

        if (!i2c1::read_flag<i2c1::sts_read_transfer_direction>())  // accept writes here
        {
            i2c1::enable_interrupt(i2c_receive);
            led::toggle();
        }
    }
    else if (i2c1::read_flag<i2c1::sts_receive_data_not_empty>())
    {
        i2c1::read();
    }
    else if (i2c1::read_flag<i2c1::sts_stop_detected>())
    {
        i2c1::clear_flag<i2c1::sts_stop_detected>();
        i2c1::disable_interrupt(i2c_receive);
    }
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
    i2c1::setup();
    i2c1::own_address(slave_address);
    i2c1::enable_interrupt(i2c_error | i2c_nack_received | i2c_address_match | i2c_stop_detection);
    hal::nvic<interrupt::I2C1>::enable();
    i2c2::setup();
    printf("Welcome to the STM32G070!\n");

    uint32_t last_x = 4711;

    for (;;)
    {
        if (btn::read())
        {
            static uint8_t buf[4] = { 0xb, 0xe, 0xe, 0xf };
            i2c2::write(slave_address, buf, sizeof(buf));
        }

        if (x != last_x)
        {
            printf("%lx\n", x);
            last_x = x;
        }
    }
}

extern "C" void write_probe(uint8_t x)
{
    probe::write(x != 0);
}

extern "C" void trace(const char *s, uint32_t x)
{
    printf("%s = %lx\n", s, x);
}

extern "C" void write_i2c2(uint8_t addr, const uint8_t *buf, uint8_t nbytes)
{
    i2c2::write(addr, buf, nbytes);
}

