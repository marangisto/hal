////
//
//      Bit-bang 74HC595 in slow motion
//
////

#include <gpio.h>

using namespace stm32f0;
using namespace gpio;

typedef output_t<PA5> srclk;    // sck
typedef output_t<PA7> ser;      // mosi
typedef output_t<PA2> rclk;     // latch
typedef output_t<PC8> led_a;
typedef output_t<PC9> led_b;

static void send(uint8_t x)
{
    for (uint8_t i = 0; i < 8; ++i)
    {
        led_a::set();
        sys_tick::delay_ms(25);
        ser::write(x & BV(i));
        srclk::set();
        sys_tick::delay_ms(25);
        srclk::clear();
        led_a::clear();
        sys_tick::delay_ms(25);
    }

    led_b::set();
    rclk::set();
    sys_tick::delay_ms(25);
    rclk::clear();
    led_b::clear();
}


int main()
{
    srclk::setup();
    ser::setup();
    rclk::setup();
    led_a::setup();
    led_b::setup();

    uint8_t i = 0;

    for (;;)
    {
        send(i++);
        sys_tick::delay_ms(250);
    }
}

