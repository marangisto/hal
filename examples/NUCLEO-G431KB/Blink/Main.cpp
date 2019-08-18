#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;

typedef output_t<PB8> led;

void loop();

int main()
{
    led::setup();
    for (;;)
        loop();
}

void loop()
{
    led::toggle();
    sys_tick::delay_ms(1000);
}

