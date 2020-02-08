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
typedef i2c_master_t<2, PB13, PB14> master;
typedef i2c_slave_t<1, PB8, PB9> slave;

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
    slave::isr();
}

static void slave_callback()
{
    led::toggle();
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
    slave::setup(slave_address, slave_callback);
    hal::nvic<interrupt::I2C1>::enable();
    printf("Welcome to the STM32G070!\n");

    uint32_t last_x = 4711;

    for (;;)
    {
        if (btn::read())
        {
            static uint8_t buf[4] = { 0xb, 0xe, 0xe, 0xf };
            master::write(slave_address, buf, sizeof(buf));
        }

        if (x != last_x)
        {
            printf("%lx\n", x);
            last_x = x;
        }
    }
}

