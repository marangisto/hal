#include <gpio.h>
#include <mco.h>

using hal::sys_tick;
using hal::sys_clock;
using namespace hal::gpio;
using namespace hal::mco;

typedef output_t<PB10> ld4;
typedef mco_t<PA8, mco_pll, 16> mco;

void loop();

int main()
{
    ld4::setup();
    mco::setup();

    for (;;)
        loop();
}

void loop()
{
    ld4::toggle();
    sys_tick::delay_ms(1000);
}

