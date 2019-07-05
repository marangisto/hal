#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;

typedef output_t<PA5> ld4;

void loop();

int main()
{
    ld4::setup();
    for (;;)
        loop();
}

void loop()
{
    ld4::toggle();
    sys_tick::delay_ms(1000);
}

