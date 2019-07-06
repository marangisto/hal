#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;

typedef output_t<PA5> ld4;
typedef internal::alternate_t<PA8, internal::MCO> mco;

void loop();

int main()
{
    using namespace device;

    typedef rcc_t _;

    ld4::setup();

    RCC.CFGR |= _::CFGR_MCOPRE<0x7>;    // divide by 128
    RCC.CFGR |= _::CFGR_MCOSEL<0x5>;    // PLLRCLK

    mco::setup<high_speed>();

    for (;;)
        loop();
}

void loop()
{
    ld4::toggle();
    sys_tick::delay_ms(1000);
}

