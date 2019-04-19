#include <gpio.h>

using namespace stm32f0;
using namespace gpio;

typedef input_t<A, 0> btn;
typedef output_t<C, 8> led_a;
typedef output_t<C, 9> led_b;

void loop();

int main()
{
    btn::setup();
    led_a::setup();
    led_b::setup();

    for (;;)
        loop();
}

void loop()
{
    static bool last_btn = false;
    bool b;

    if ((b = btn::read()) != last_btn)
    {
        if (b)
            led_a::toggle();
        last_btn = b;
    }

    led_b::toggle();

    sys_tick::delay_ms(100);
}

