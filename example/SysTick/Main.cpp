#include <gpio.h>

using namespace stm32f0;
using namespace gpio;

typedef output_t<C, 8> led_a;
typedef output_t<C, 9> led_b;

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

