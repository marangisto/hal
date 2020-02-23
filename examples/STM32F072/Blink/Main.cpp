#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;

typedef output_t<PB3> led_a;

void loop();

int main()
{
    led_a::setup();

    for (;;)
        loop();
}

void loop()
{
    led_a::toggle();

    sys_tick::delay_ms(250);
}

