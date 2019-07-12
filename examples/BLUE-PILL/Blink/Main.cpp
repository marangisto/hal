#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;

typedef output_t<PC13> led;

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

