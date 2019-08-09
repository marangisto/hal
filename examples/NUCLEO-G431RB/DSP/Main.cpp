#include <stdlib.h>
#include <usart.h>
#include <redirect.h>
#include <adc.h>
#include <dac.h>

using hal::sys_tick;
using hal::sys_clock;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::adc;
using namespace hal::dac;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA10> d2;
typedef analog_t<PA0> ain;
typedef adc_t adc;
typedef dac_t<1> dac;

void loop();

template<> void handler<interrupt::USART2>()
{
    serial::isr();
}

int main()
{
    dac::setup();
    dac::enable<1>();
    dac::enable<2>();

    adc::setup();
    ain::setup();

    d2::setup();
 
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    interrupt::enable();
    stdio_t::bind<serial>();

    printf("Welcome to the STM32G431!\n");

    for (;;)
        loop();
}

void loop()
{
    d2::toggle();
    adc::start_conversion();
    while (!adc::end_of_conversion());
    d2::toggle();
    uint16_t y = adc::read();
    dac::write<1>(y);
    dac::write<2>(4095-y);
}

