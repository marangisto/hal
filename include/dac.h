#pragma once

#include "hal.h"
#include "gpio.h"
#include "dma.h"

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
    static inline typename dac_traits<NO>::T& DAC() { return dac_traits<NO>::DAC(); }
    static constexpr gpio::gpio_pin_t pin = dac_traits<NO>::ch1_pin;
    static constexpr uint32_t CR_EN = _::DAC_CR_EN1;
    static constexpr uint32_t CR_TEN = _::DAC_CR_TEN1;
    static constexpr uint32_t CR_DMAEN = _::DAC_CR_DMAEN1;
    static constexpr uint32_t CR_DMAUDRIE = _::DAC_CR_DMAUDRIE1;
    static constexpr uint32_t SWTRGR_SWTRIG = _::DAC_SWTRGR_SWTRIG1;
    template<uint32_t X> static constexpr uint32_t CR_TSEL = _::template DAC_CR_TSEL1<X>;
    template<uint32_t X> static constexpr uint32_t CR_WAVE = _::template DAC_CR_WAVE1<X>;
    template<uint32_t X> static constexpr uint32_t STMODR_STINCTRIGSEL = _::template DAC_STMODR_STINCTRIGSEL1<X>;
    template<uint32_t X> static constexpr uint32_t STMODR_STRSTTRIGSEL = _::template DAC_STMODR_STRSTTRIGSEL1<X>;
    static inline volatile uint32_t& STR() { return DAC().DAC_STR1; }
    static inline volatile uint32_t& DHR12R() { return DAC().DAC_DHR12R1; }
    static inline void write(uint16_t x) { dac_traits<NO>::DAC().DAC_DHR12R1 = x; }
};

template<uint8_t NO> struct dac_channel_traits<NO, 2>
{
    typedef typename dac_traits<NO>::T _;
    static inline typename dac_traits<NO>::T& DAC() { return dac_traits<NO>::DAC(); }
    static constexpr gpio::gpio_pin_t pin = dac_traits<NO>::ch2_pin;
    static constexpr uint32_t CR_EN = _::DAC_CR_EN2;
    static constexpr uint32_t CR_TEN = _::DAC_CR_TEN2;
    static constexpr uint32_t CR_DMAEN = _::DAC_CR_DMAEN2;
    static constexpr uint32_t CR_DMAUDRIE = _::DAC_CR_DMAUDRIE2;
    static constexpr uint32_t SWTRGR_SWTRIG = _::DAC_SWTRGR_SWTRIG2;
    template<uint32_t X> static constexpr uint32_t CR_TSEL = _::template DAC_CR_TSEL2<X>;
    template<uint32_t X> static constexpr uint32_t CR_WAVE = _::template DAC_CR_WAVE2<X>;
    template<uint32_t X> static constexpr uint32_t STMODR_STINCTRIGSEL = _::template DAC_STMODR_STINCTRIGSEL2<X>;
    template<uint32_t X> static constexpr uint32_t STMODR_STRSTTRIGSEL = _::template DAC_STMODR_STRSTTRIGSEL2<X>;
    static inline volatile uint32_t& STR() { return DAC().DAC_STR2; }
    static inline volatile uint32_t& DHR12R() { return DAC().DAC_DHR12R2; }
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
        DAC().DAC_MCR = _::DAC_MCR_RESET_VALUE                  // reset mode control register
                      | _::template DAC_MCR_HFSEL<0x2>          // high-frequency mode (AHB > 160MHz)
                      ;
    }

    template<uint8_t CH>
    static inline void setup_pin()
    {
        gpio::analog_t<dac_channel_traits<NO, CH>::pin>::template setup<gpio::floating>();
    }

    template<uint8_t CH, uint8_t RST, uint8_t INC>
    static inline void setup()
    {
        DAC().DAC_CR |= dac_channel_traits<NO, CH>::CR_TEN                                  // enable trigger
                     |  dac_channel_traits<NO, CH>::template CR_TSEL<RST>                   // trigger selection
                     ;
        DAC().DAC_STMODR |= dac_channel_traits<NO, CH>::template STMODR_STRSTTRIGSEL<RST>   // reset trigger selection
                         |  dac_channel_traits<NO, CH>::template STMODR_STINCTRIGSEL<INC>   // increment trigger selection
                         ;
    }

    template<uint8_t CH>
    static inline void enable()
    {
        DAC().DAC_CR |= dac_channel_traits<NO, CH>::CR_EN;      // enable dac channel
        sys_clock::delay_us(8);                                 // wait for voltage to settle
    }

    template<uint8_t CH>
    static inline void disable()
    {
        DAC().DAC_CR &= ~dac_channel_traits<NO, CH>::CR_EN;     // disable dac channel
    }

    template<uint8_t CH, typename DMA, uint8_t DMACH, typename T>
    static inline void enable_dma(const T *source, uint16_t nelem)
    {
        volatile uint32_t& reg = dac_channel_traits<NO, CH>::DHR12R();

        DAC().DAC_CR |= dac_channel_traits<NO, CH>::CR_DMAEN;       // enable dac channel dma
        DAC().DAC_CR |= dac_channel_traits<NO, CH>::CR_DMAUDRIE;    // enable dac dma underrun interrupt
        DMA::template disable<DMACH>();                                 // disable dma channel
        DMA::template mem_to_periph<DMACH>(source, nelem, &reg);    // configure dma from memory
        DMA::template enable<DMACH>();                                  // enable dma channel
        dma::dmamux_traits<DMA::INST, DMACH>::CCR() = device::dmamux_t::C0CR_DMAREQ_ID<6>;
        enable<CH>();                                               // enable dac channel
    }

    template<uint8_t CH, typename DMA, uint8_t DMACH>
    static inline void disable_dma()
    {
        DAC().DAC_CR &= ~dac_channel_traits<NO, CH>::CR_DMAEN;      // disable dac channel dma
        disable<CH>();                                              // disable dac channel
        sys_clock::delay_us(1000);                                  // ensure miniumum wait before next enable
        DMA::template abort<DMACH>();                               // stop dma on relevant dma channel
        DAC().DAC_CR &= ~dac_channel_traits<NO, CH>::CR_DMAUDRIE;   // disable dac channel underrun interrupt
    }

    template<uint8_t CH, uint32_t RST, uint32_t INC>
    static inline void enable_wave()
    {
        dac_channel_traits<NO, CH>::STR() |= _::template DAC_STR1_STRSTDATA1<RST>   // reset value
                                          |  _::template DAC_STR1_STINCDATA1<INC>   // increment value
                                          |  _::DAC_STR1_STDIR1                     // direction increment
                                          ;
        DAC().DAC_CR |= dac_channel_traits<NO, CH>::template CR_WAVE<0x3>;          // sawtooth wave enable
    }

    template<uint8_t CH, uint8_t SEL = 0>
    static inline void enable_trigger()
    {
        DAC().DAC_CR |= dac_channel_traits<NO, CH>::CR_TEN                  // enable trigger
                     |  dac_channel_traits<NO, CH>::template CR_TSEL<SEL>   // select trigger source
                     ;
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

