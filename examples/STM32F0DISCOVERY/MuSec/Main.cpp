#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;

typedef output_t<PB8> probe;

void loop();

int main()
{
    probe::setup();

    for (;;)
    {
        probe::set();
        sys_tick::delay_us(5);
        probe::clear();
        sys_tick::delay_us(20);
    }
}

