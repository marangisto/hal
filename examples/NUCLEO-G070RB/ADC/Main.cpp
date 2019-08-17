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
typedef output_t<PA5> ld4;
typedef hal::adc::adc_t<1> adc;
typedef analog_t<PA0> ain0;
typedef analog_t<PA1> ain1;
typedef output_t<PA10> probe;

void loop();

template<> void handler<interrupt::USART2>()
{
    ld4::toggle();
    serial::isr();
}

int main()
{
    adc::setup();
    adc::sequence<0, 1>();
    ain0::setup();
    ain1::setup();
    probe::setup();
    ld4::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    stdio_t::bind<serial>();
    interrupt::enable();

    printf("Hello STM32G070!\n");

    for (;;)
        loop();
}

void loop()
{
    {
        probe::set();
        uint16_t y = adc::read();
        uint16_t z = adc::read();
        probe::clear();

        printf("%-64.*s  ", y >> 6, "############################################################");
        printf("%-64.*s\n", z >> 6, "############################################################");
        sys_tick::delay_ms(20);
    }
}

