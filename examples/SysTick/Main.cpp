#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;

typedef output_t<PC8> led_a;
typedef output_t<PC9> led_b;

int main()
{
    led_a::setup();
    led_b::setup();

    for (;;)
    {
        static uint32_t next_a = 0, next_b = 0;
        uint32_t now = sys_tick::count();

        if (now >= next_a)
        {
            led_a::toggle();
            next_a = now + 101;
        }

        if (now >= next_b)
        {
            led_b::toggle();
            next_b = now + 103;
        }
    }
}

