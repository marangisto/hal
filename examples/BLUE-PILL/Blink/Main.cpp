#include <gpio.h>

using namespace hal;
using namespace gpio;

typedef output_t<PC13> led;

int main()
{
    led::setup();

    for (;;)
    {
        led::toggle();
        sys_tick::delay_ms(1000);
    }
}

