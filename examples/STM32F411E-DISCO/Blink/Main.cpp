#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;

typedef output_t<PD13> ld3;
typedef output_t<PD12> ld4;
typedef output_t<PD14> ld5;
typedef output_t<PD15> ld6;

void loop();

int main()
{
    ld3::setup();
    ld4::setup();
    ld5::setup();
    ld6::setup();

    for (;;)
        loop();
}

void loop()
{
    ld3::toggle();
    if (ld3::read())
    {
        ld4::toggle();
        if (ld4::read())
        {
            ld5::toggle();
            if (ld5::read())
                ld6::toggle();
        }
    }
    sys_tick::delay_ms(1000);
}

