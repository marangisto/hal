#pragma once

#include <hal.h>

namespace hal
{

namespace mco
{

//using namespace gpio;

enum mco_sel_t
    { mco_off       = 0
    , mco_sysclk    = 1
    , mco_hsi16     = 3
    , mco_hse       = 4
    , mco_pll       = 5
    , mco_lsi       = 6
    , mco_lse       = 7
    , mco_hsi48     = 8     // only on STM32G4
    };

template<int> struct mco_prescale {};

#if defined(STM32G070)
template<> struct mco_prescale<1> { static const uint8_t value = 0; };
template<> struct mco_prescale<2> { static const uint8_t value = 1; };
template<> struct mco_prescale<4> { static const uint8_t value = 2; };
template<> struct mco_prescale<8> { static const uint8_t value = 3; };
template<> struct mco_prescale<16> { static const uint8_t value = 4; };
template<> struct mco_prescale<32> { static const uint8_t value = 5; };
template<> struct mco_prescale<64> { static const uint8_t value = 6; };
template<> struct mco_prescale<128> { static const uint8_t value = 7; };
#elif defined(STM32G431)
template<> struct mco_prescale<1> { static const uint8_t value = 0; };
template<> struct mco_prescale<2> { static const uint8_t value = 1; };
template<> struct mco_prescale<4> { static const uint8_t value = 2; };
template<> struct mco_prescale<8> { static const uint8_t value = 3; };
template<> struct mco_prescale<16> { static const uint8_t value = 4; };
#endif

template<gpio::gpio_pin_t PIN, mco_sel_t SEL, int PRESCALE>
struct mco_t
{
    typedef gpio::internal::alternate_t<PIN, gpio::internal::MCO> mco;
    typedef device::rcc_t _;

    static void setup()
    {
        using device::RCC;

        RCC.CFGR |= _::CFGR_MCOPRE<mco_prescale<PRESCALE>::value>;
        RCC.CFGR |= _::CFGR_MCOSEL<SEL>;
        mco::template setup<gpio::high_speed>();
    }
};

} // namespace mco

} // namespace hal
