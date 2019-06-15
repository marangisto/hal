#include <gpio.h>

using namespace hal;
using namespace gpio;

typedef output_t<PD13> led_a;
typedef output_t<PD12> led_b;
typedef output_t<PD14> led_c;
typedef output_t<PD15> led_d;

int main()
{
    led_a::setup();
    led_b::setup();
    led_c::setup();
    led_d::setup();

    for (;;)
    {
        static uint32_t next_a = 0, next_b = 0;
        uint32_t now = sys_tick::count();

        if (now >= next_a)
        {
            led_a::toggle();
            if (led_a::read())
                led_c::toggle();
            next_a = now + 101;
        }

        if (now >= next_b)
        {
            led_b::toggle();
            if (led_b::read())
                led_d::toggle();
            next_b = now + 103;
        }
    }
}

