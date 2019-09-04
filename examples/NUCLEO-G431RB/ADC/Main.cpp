#include <stdlib.h>
#include <usart.h>
#include <redirect.h>
#include <adc.h>

using hal::sys_tick;
using hal::sys_clock;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::adc;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA5> ld4;
typedef output_t<PA10> d2;
typedef analog_t<PA0> ain;
typedef adc_t<1> adc;

template<> void handler<interrupt::USART2>()
{
    ld4::toggle();
    serial::isr();
}

int main()
{
    ld4::setup();
    d2::setup();
    serial::setup<115200>();
    hal::nvic<interrupt::USART2>::enable();
    interrupt::enable();
    stdio_t::bind<serial>();
    printf("Welcome to the STM32G431!\n");

    ain::setup();
    adc::setup<16>();
    adc::oversample<16>();
    adc::sequence<1>();
    adc::enable();

    for (;;)
    {
        d2::toggle();
        uint16_t y = adc::read();
        d2::toggle();
        printf("%d\n", y);
        sys_tick::delay_ms(1);
    }
}

