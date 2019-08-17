#include <stdlib.h>
#include <usart.h>
#include <dac.h>
#include <redirect.h>
#include <cstring>

using hal::sys_tick;
using hal::sys_clock;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::dac;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA10> d2;
typedef hal::dac::dac_t<1> dac;


template<> void handler<interrupt::USART2>()
{
    serial::isr();
}

int main()
{
    d2::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    interrupt::enable();

    stdio_t::bind<serial>();
    printf("Welcome to the STM32G431!\n");

    dac::setup();
    dac::enable<1>();
    dac::enable<2>();

    static uint32_t i = 0;

    for (;;)
    {
        uint16_t x = i++ & 0xfff;
    
        d2::toggle();
        dac::write<1>(x);
        d2::toggle();
        dac::write<2>(4095 - x);
    }
}

