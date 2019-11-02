#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;

typedef output_t<PB0> led1;
typedef output_t<PE1> led2;
typedef output_t<PB14> led3;

void loop();

int main()
{
    led1::setup();
    led2::setup();
    led3::setup();

    for (;;)
        loop();
}

void loop()
{
    led1::toggle();
    if (led1::read())
    {
        led2::toggle();
        if (led2::read())
            led3::toggle();
    }
    sys_tick::delay_ms(1000);
}

