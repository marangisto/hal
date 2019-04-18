#include <gpio.h>

using namespace stm32f0x1::gpio;

typedef output_t<C, 8> led_a;
typedef output_t<C, 9> led_b;

int main()
{
    led_a::setup();
    led_b::setup();

    for (;;)
    {
        led_a::toggle();
        if (led_a::get())
            led_b::toggle();
        sys_tick::delay_ms(250);
    }
}

