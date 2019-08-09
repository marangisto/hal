#pragma once

#include "hal.h"
#include "gpio.h"

namespace hal
{

namespace dac
{

template<uint8_t NO> struct dac_traits {};

template<> struct dac_traits<1>
{
    typedef device::dac1_t T;
    static inline T& DAC() { return device::DAC1; }
    static constexpr gpio::gpio_pin_t ch1_pin = gpio::PA4;
    static constexpr gpio::gpio_pin_t ch2_pin = gpio::PA5;
};

#if defined(HAVE_PERIPHERAL_DAC2)
template<> struct dac_traits<2>
{
    typedef device::dac2_t T;
    static inline T& DAC() { return device::DAC2; }
    static constexpr gpio::gpio_pin_t ch1_pin = gpio::PA7;  // FIXME: check this!
};
#endif

template<uint8_t NO, uint8_t CH> struct dac_channel_traits {};

template<uint8_t NO> struct dac_channel_traits<NO, 1>
{
    typedef typename dac_traits<NO>::T _;
    static constexpr gpio::gpio_pin_t pin = dac_traits<NO>::ch1_pin;
    static constexpr uint32_t CR_EN = _::DAC_CR_EN1;
    static constexpr uint32_t CR_TEN = _::DAC_CR_TEN1;
    template<uint32_t X> static constexpr uint32_t CR_TSEL = _::template DAC_CR_TSEL1<X>;
    static constexpr uint32_t SWTRGR_SWTRIG = _::DAC_SWTRGR_SWTRIG1;
    static constexpr uint32_t CR_DMAEN = _::DAC_CR_DMAEN1;
    static inline void write(uint16_t x) { dac_traits<NO>::DAC().DAC_DHR12R1 = x; }
};

template<uint8_t NO> struct dac_channel_traits<NO, 2>
{
    typedef typename dac_traits<NO>::T _;
    static constexpr gpio::gpio_pin_t pin = dac_traits<NO>::ch2_pin;
    static constexpr uint32_t CR_EN = _::DAC_CR_EN2;
    static constexpr uint32_t CR_TEN = _::DAC_CR_TEN2;
    template<uint32_t X> static constexpr uint32_t CR_TSEL = _::template DAC_CR_TSEL2<X>;
    static constexpr uint32_t SWTRGR_SWTRIG = _::DAC_SWTRGR_SWTRIG2;
    static constexpr uint32_t CR_DMAEN = _::DAC_CR_DMAEN2;
    static inline void write(uint16_t x) { dac_traits<NO>::DAC().DAC_DHR12R2 = x; }
};

template<uint8_t NO>
struct dac_t
{
    typedef typename dac_traits<NO>::T _;
    static inline typename dac_traits<NO>::T& DAC() { return dac_traits<NO>::DAC(); }

    static void setup()
    {
        device::peripheral_traits<_>::enable();                 // enable dac clock

        DAC().DAC_CR = _::DAC_CR_RESET_VALUE;                   // reset control register
        DAC().DAC_MCR = _::DAC_MCR_RESET_VALUE;                 // reset mode control register
    }

    template<uint8_t CH>
    static inline void enable()
    {
        gpio::analog_t<dac_channel_traits<NO, CH>::pin>::template setup<gpio::floating>();

        DAC().DAC_CR |= dac_channel_traits<NO, CH>::CR_EN;      // enable channel
        sys_clock::delay_us(8);                                 // wait for voltage to settle
    }

    template<uint8_t CH, uint8_t SEL = 0>
    static inline void enable_trigger()
    {
        DAC().DAC_CR |= dac_channel_traits<NO, CH>::CR_TEN                  // enable trigger
                     |  dac_channel_traits<NO, CH>::template CR_TSEL<SEL>   // select trigger source
                     ;
    }

    template<uint8_t CH>
    static inline void enable_dma()
    {
        DAC().DAC_CR |= dac_channel_traits<NO, CH>::CR_DMAEN;
    }

    template<uint8_t CH>
    static inline void write(uint16_t x)
    {
        dac_channel_traits<NO, CH>::write(x);
    }

    template<uint8_t CH>
    static inline void trigger()
    {
        DAC().DAC_SWTRGR |= dac_channel_traits<NO, CH>::SWTRGR_SWTRIG;
    }
};

} // namespace adc

} // namespace hal

