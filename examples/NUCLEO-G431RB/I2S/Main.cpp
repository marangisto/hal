////
// 
//      Basic I2S example
//
////

#include <i2s.h>
#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::i2s;

typedef i2s_t<2, PA11, PB12, PB13> i2s;
typedef output_t<PA5> ld4;

void loop();

int main()
{
    i2s::setup<philips_i2s, low_level, format_32_32, 55>();
    ld4::setup();

    for (;;)
        loop();
}

void loop()
{
    static uint16_t i = 0;

    ld4::toggle();
    i2s::write32(0xffff - i);
    i2s::write32(i+= 256);
    //sys_tick::delay_ms(100);
}

