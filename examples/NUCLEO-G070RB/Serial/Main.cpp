#include <stdlib.h>
#include <usart.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA5> ld4;

void loop();

int main()
{
    ld4::setup();
    serial::setup<230400>();

    for (;;)
        loop();
}

void loop()
{
    static uint8_t i = 0;
    static char buf[32];

    ld4::toggle();
    serial::write(itoa(i++, buf, 10));
    serial::write("\n");
    sys_tick::delay_ms(25);
}

