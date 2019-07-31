#include <stdlib.h>
#include <adc.h>
#include <usart.h>
#include <cstring>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::adc;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA5> ld4;

template<> void handler<interrupt::USART2>()
{
    ld4::toggle();
    serial::isr();
}

typedef adc_t adc1;

int main()
{
    ld4::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    interrupt::enable();

    adc1::setup();
    serial::write("ADC ready!\n");

    for (;;)
    {
        static char buf[256];
        uint32_t x = adc1::read();

        serial::write(itoa(x, buf, 16));
        serial::write("\n");
        sys_tick::delay_ms(100);
    }
}

