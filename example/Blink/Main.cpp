#include <gpio.h>

using namespace stm32f0;
using namespace gpio;

typedef output_t<C, 8> led_a;
typedef output_t<C, 9> led_b;

void loop();

int main()
{
    led_a::setup();
    led_b::setup();

    for (;;)
        loop();
}

void loop()
{
    led_a::toggle();

    if (led_a::get())
        led_b::toggle();

    sys_tick::delay_ms(250);
}

