////
// 
//      Drive 74HC595 using SPI peripheral
//
////

#include <spi.h>
#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::spi;

typedef spi_t<1, PA5, PA7> hc595;
typedef output_t<PA2> latch;
typedef output_t<PC8> led_a;
typedef output_t<PC9> led_b;

void loop();

int main()
{
    hc595::setup<mode_0, lsb_first, fpclk_8, low_speed>();
    latch::setup();
    led_a::setup();
    led_b::setup();

    for (;;)
        loop();
}

void loop()
{
    static uint8_t i = 0;

    if (!(i & 0x0f))
    {
        led_a::toggle();
        if (led_a::read())
            led_b::toggle();
    }

    hc595::write8(i++);
    hc595::wait_idle();
    latch::set();
    latch::clear();
    sys_tick::delay_ms(25);
}

