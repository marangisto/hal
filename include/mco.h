#pragma once

#include <hal.h>

namespace hal
{

namespace mco
{

//using namespace gpio;

enum mco_sel_t
    { mco_off
    , mco_lsi
    , mco_lse
    , mco_sysclk
    , mco_hsi
    , mco_hse
    , mco_pll
    };

template<int> struct mco_sel_traits {};
template<> struct mco_sel_traits<mco_off> { static const uint8_t sel = 0; };
template<> struct mco_sel_traits<mco_lsi> { static const uint8_t sel = 6; };
template<> struct mco_sel_traits<mco_lse> { static const uint8_t sel = 7; };
template<> struct mco_sel_traits<mco_sysclk> { static const uint8_t sel = 1; };
template<> struct mco_sel_traits<mco_hsi> { static const uint8_t sel = 3; };
template<> struct mco_sel_traits<mco_hse> { static const uint8_t sel = 4; };
template<> struct mco_sel_traits<mco_pll> { static const uint8_t sel = 5; };

template<int> struct mco_pre_traits {};
template<> struct mco_pre_traits<1> { static const uint8_t pre = 0; };
template<> struct mco_pre_traits<2> { static const uint8_t pre = 1; };
template<> struct mco_pre_traits<4> { static const uint8_t pre = 2; };
template<> struct mco_pre_traits<8> { static const uint8_t pre = 3; };
template<> struct mco_pre_traits<16> { static const uint8_t pre = 4; };
template<> struct mco_pre_traits<32> { static const uint8_t pre = 5; };
template<> struct mco_pre_traits<64> { static const uint8_t pre = 6; };
template<> struct mco_pre_traits<128> { static const uint8_t pre = 7; };

template<gpio::gpio_pin_t PIN, mco_sel_t SEL, int PRESCALE>
struct mco_t
{
    typedef gpio::internal::alternate_t<PIN, gpio::internal::MCO> mco;
    typedef device::rcc_t _;

    static void setup()
    {
        using device::RCC;

        RCC.CFGR |= _::CFGR_MCOPRE<mco_pre_traits<PRESCALE>::pre>;
        RCC.CFGR |= _::CFGR_MCOSEL<mco_sel_traits<SEL>::sel>;
        mco::template setup<gpio::high_speed>();
    }
};

} // namespace mco

} // namespace hal
