#include <stdlib.h>
#include <cstring>
#include <usart.h>
#include <redirect.h>
#include <adc.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::adc;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PB8> led;
typedef hal::adc::adc_t<1> adc;
typedef analog_t<PB0> ain3;

template<> void handler<interrupt::USART2>()
{
    led::toggle();
    serial::isr();
}

int main()
{
    led::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    interrupt::enable();

    stdio_t::bind<serial>();
    printf("Welcome to the STM32G431!\n");

    ain3::setup();

    adc::setup();
    adc::sequence<1, 2, 15>();
    adc::enable();

    for (;;)
    {
        uint16_t x = adc::read();

        printf("%d\n", x);
        sys_tick::delay_ms(50);
    }
}
