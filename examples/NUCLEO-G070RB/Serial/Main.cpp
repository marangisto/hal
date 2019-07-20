#include <stdlib.h>
#include <usart.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA5> ld4;

void loop();

template<> void handler<interrupt::USART2>()
{
    ld4::toggle();
    serial::read();
}

int main()
{
    ld4::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    interrupt::enable();

    for (;;)
        loop();
}

void loop()
{
    static uint8_t i = 0;
    static char buf[32];

    serial::write(itoa(i++, buf, 10));
    serial::write("\n");
}

