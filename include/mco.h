#pragma once

#include <hal.h>

namespace hal
{

namespace mco
{

#if defined(STM32F103)
enum mco_sel_t
    { mco_off       = 0x0
    , mco_sysclk    = 0x4
    , mco_hsi       = 0x5
    , mco_hse       = 0x6
    , mco_pll       = 0x7   // divided by 2
    , mco_pll2      = 0x8
    , mco_pll3      = 0x9   // divided by 2
    , mco_xt1       = 0xa
    , mco_pll3e     = 0xb   // pll3 for ethernet
    };
#elif defined(STM32F767)
enum mco_sel_t
    { mco_hsi       = 0
    , mco_lse       = 1
    , mco_hse       = 2
    , mco_pll       = 3
    };
#else
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
#endif

template<int PRESCALE> struct mco_prescale
{
    static_assert(always_false_i<PRESCALE>::value, "illegal prescaler value");
};

#if defined(STM32F103)
template<> struct mco_prescale<1> { static const uint8_t value = 0; };
#elif defined(STM32F767)
template<> struct mco_prescale<1> { static const uint8_t value = 0; };
template<> struct mco_prescale<2> { static const uint8_t value = 4; };
template<> struct mco_prescale<3> { static const uint8_t value = 5; };
template<> struct mco_prescale<4> { static const uint8_t value = 6; };
template<> struct mco_prescale<5> { static const uint8_t value = 7; };
#elif defined(STM32G070)
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

template<gpio::gpio_pin_t PIN, mco_sel_t SEL, int PRESCALE = 1>
struct mco_t
{
#if defined(STM32F767)
    typedef gpio::internal::alternate_t<PIN, gpio::internal::MCO1> mco;
#else
    typedef gpio::internal::alternate_t<PIN, gpio::internal::MCO> mco;
#endif
    typedef device::rcc_t _;

    static void setup()
    {
        using device::RCC;

#if defined(STM32F103)
        static_assert(mco_prescale<PRESCALE>::value == 0, "illegal PRESCALE for this MCU");
        RCC.CFGR |= _::CFGR_MCO<SEL>;
#elif defined(STM32F767)
        RCC.CFGR |= _::CFGR_MCO1PRE<mco_prescale<PRESCALE>::value>;
        RCC.CFGR |= _::CFGR_MCO1<SEL>;
#else
        RCC.CFGR |= _::CFGR_MCOPRE<mco_prescale<PRESCALE>::value>;
        RCC.CFGR |= _::CFGR_MCOSEL<SEL>;
#endif
        mco::template setup<gpio::high_speed>();
    }
};

} // namespace mco

} // namespace hal
