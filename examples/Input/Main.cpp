#include <gpio.h>

using system::sys_tick;
using namespace system::gpio;

typedef input_t<PA0> btn;
typedef output_t<PC8> led_a;
typedef output_t<PC9> led_b;

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

