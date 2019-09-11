#include <gpio.h>
#include <mco.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::mco;

typedef output_t<PC13> led;
typedef mco_t<PA8, mco_hsi> mco;

void loop();

int main()
{
    led::setup();
    mco::setup();

    for (;;)
        loop();
}

void loop()
{
    led::toggle();
    sys_tick::delay_ms(250);
}

