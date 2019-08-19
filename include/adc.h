#pragma once

#include "hal.h"
#include "dma.h"

namespace hal
{

namespace adc
{

template<uint8_t NO> struct adc_traits {};

#if defined(HAVE_PERIPHERAL_ADC2)
template<> struct adc_traits<1>
{
    typedef device::adc1_t T;
    typedef device::adc12_common_t C;
    static inline T& ADC() { return device::ADC1; }
#if defined(HAVE_PERIPHERAL_ADC12_COMMON)
    static inline C& COMMON() { return device::ADC12_COMMON; }
#endif
};

template<> struct adc_traits<2>
{
    typedef device::adc2_t T;
    typedef device::adc12_common_t C;
    static inline T& ADC() { return device::ADC2; }
    static inline C& COMMON() { return device::ADC12_COMMON; }
};
#else
template<> struct adc_traits<1>
{
    typedef device::adc_t T;
    static inline T& ADC() { return device::ADC; }
};
#endif

template<uint8_t NO>
struct adc_t
{
    typedef typename adc_traits<NO>::T _;
    static inline typename adc_traits<NO>::T& ADC() { return adc_traits<NO>::ADC(); }
#if defined(STM32G431)
    typedef typename adc_traits<NO>::C __;
    static inline typename adc_traits<NO>::C& COMMON() { return adc_traits<NO>::COMMON(); }
#endif

    static void setup()
    {
        using namespace device;
        ADC().CR = _::CR_RESET_VALUE;                           // reset control register
        ADC().IER = _::IER_RESET_VALUE;                         // reset interrupt register
#if defined(STM32G070)
        peripheral_traits<_>::enable();                         // enable adc clock
        ADC().CFGR1 = _::CFGR1_RESET_VALUE                      // reset configuration register 1
                    | _::CFGR1_CHSELRMOD                        // use alternate channel selection mode
                    | _::CFGR1_OVRMOD                           // overwrite on overrun
                    ;
        ADC().CFGR2 = _::CFGR2_RESET_VALUE                      // reset configuration register 2
                    | _::template CFGR2_CKMODE<0x3>             // use PCLK synchronous mode
                    ;
#elif defined(STM32G431)
        peripheral_traits<__>::enable();                        // enable common adc clock
        ADC().CFGR = _::CFGR_RESET_VALUE                        // reset configuration register 1
                    | _::CFGR_OVRMOD                            // overwrite on overrun
                    ;
        RCC.CCIPR1 |= rcc_t::template CCIPR1_ADCSEL<0x1>;       // use pll P clock  FIXME: does not seem to have any effect!
        COMMON().CCR = __::CCR_RESET_VALUE                      // reset common control register
                     | __::template CCR_CKMODE<0x3>;            // divide clock by 4
                     ;
        ADC().DIFSEL = _::DIFSEL_RESET_VALUE;                   // differential mode register
        ADC().GCOMP = _::GCOMP_RESET_VALUE;                     // reset gain compensation register
        ADC().CFGR2 = _::CFGR2_RESET_VALUE;                     // reset configuration register 2
#else
        static_assert(false, "ADC driver not implemented");
#endif
    }

    static void enable()
    {
        using namespace device;
#if defined(STM32G070)
        uint32_t saved_dmaen = ADC().CFGR1 & _::CFGR1_DMAEN;    // save DMA flag 

        ADC().CR |= _::CR_ADVREGEN;                             // enable adc voltage regulator
        sys_clock::delay_us(10);                                // wait for regulator to stabilize
        ADC().CFGR1 &= ~_::CFGR1_DMAEN;                         // ensure DMA is disabled during calibration
        ADC().CR |= _::CR_ADCAL;                                // start calibration
        while (ADC().CR & _::CR_ADCAL);                         // wait for calibration to complete
        ADC().CFGR1 |= saved_dmaen;                             // restore DMA setting
        for (volatile uint8_t i = 0; i < 4; ++i);               // cycles between calibration and adc enable
        ADC().CR |= _::CR_ADEN;                                 // enable adc 
        while (!(ADC().ISR & _::ISR_ADRDY));                    // wait for adc ready 
#elif defined(STM32G431)
        uint32_t saved_dmaen = ADC().CFGR & _::CFGR_DMAEN;      // save DMA flag 

        ADC().CR &= ~_::CR_DEEPPWD;                             // disable deep power down mode
        ADC().CR |= _::CR_ADVREGEN;                             // enable adc voltage regulator
        sys_clock::delay_us(10);                                // wait for regulator to stabilize
        ADC().CFGR &= ~_::CFGR_DMAEN;                           // ensure DMA is disabled during calibration
        ADC().CR |= _::CR_ADCAL;                                // start calibration
        while (ADC().CR & _::CR_ADCAL);                         // wait for calibration to complete
        ADC().CFGR |= saved_dmaen;                              // restore DMA setting
        for (volatile uint8_t i = 0; i < 4; ++i);               // cycles between calibration and adc enable
        ADC().ISR |= _::ISR_ADRDY;                              // clear ready flag by writing '1'
        ADC().CR |= _::CR_ADEN;                                 // enable adc 
        while (!(ADC().ISR & _::ISR_ADRDY));                    // wait for adc ready 
#else
        static_assert(false, "ADC driver not implemented");
#endif
    }

#if defined(STM32G070)
    static constexpr uint8_t NO_CHANNEL = 0xf;
#elif defined(STM32G431)
    static constexpr uint8_t NO_CHANNEL = 0x1f;
#endif

    template< uint8_t S1, uint8_t S2 = NO_CHANNEL, uint8_t S3 = NO_CHANNEL, uint8_t S4 = NO_CHANNEL
            , uint8_t S5 = NO_CHANNEL, uint8_t S6 = NO_CHANNEL, uint8_t S7 = NO_CHANNEL, uint8_t S8 = NO_CHANNEL>
    static void sequence()
    {
        using namespace device;

#if defined(STM32G070)
        ADC().ISR &= ~_::ISR_CCRDY;                             // clear channel config ready flag
        ADC().CHSELR = _::template CHSELR_SQ1<S1>               // sequence slot 1
                     | _::template CHSELR_SQ2<S2>               // sequence slot 2
                     | _::template CHSELR_SQ3<S3>               // sequence slot 3
                     | _::template CHSELR_SQ4<S4>               // sequence slot 4
                     | _::template CHSELR_SQ5<S5>               // sequence slot 5
                     | _::template CHSELR_SQ6<S6>               // sequence slot 6
                     | _::template CHSELR_SQ7<S7>               // sequence slot 7
                     | _::template CHSELR_SQ8<S8>               // sequence slot 8
                  ;
        while (!(ADC().ISR & _::ISR_CCRDY));                    // wait for channel selection to be ready
#elif defined(STM32G431)
        static constexpr uint8_t L = (S2 != NO_CHANNEL ? 1 : 0)
                                   + (S3 != NO_CHANNEL ? 1 : 0)
                                   + (S4 != NO_CHANNEL ? 1 : 0)
                                   + (S5 != NO_CHANNEL ? 1 : 0)
                                   + (S6 != NO_CHANNEL ? 1 : 0)
                                   + (S7 != NO_CHANNEL ? 1 : 0)
                                   + (S8 != NO_CHANNEL ? 1 : 0)
                                   ;

        ADC().SQR1 = _::SQR1_RESET_VALUE                        // reset sequence 1 register
                  | _::template SQR1_L<L>                       // sequence length less one
                  | _::template SQR1_SQ1<S1>                    // sequence slot 1
                  | _::template SQR1_SQ2<S2>                    // sequence slot 2
                  | _::template SQR1_SQ3<S3>                    // sequence slot 2
                  | _::template SQR1_SQ4<S4>                    // sequence slot 4
                  ;
        ADC().SQR2 = _::SQR1_RESET_VALUE                        // reset sequence 2 register
                  | _::template SQR2_SQ5<S5>                    // sequence slot 5
                  | _::template SQR2_SQ6<S6>                    // sequence slot 6
                  | _::template SQR2_SQ7<S7>                    // sequence slot 7
                  | _::template SQR2_SQ8<S8>                    // sequence slot 8
                  ;
#else
        static_assert(false, "ADC driver not implemented");
#endif
    }

    template<typename DMA, uint8_t DMACH, typename T>
    static inline void dma(volatile T *dest, uint16_t nelem)
    {
        using namespace device;
#if defined(STM32G070)
        ADC().CFGR1 |= _::CFGR1_DMAEN                               // enable adc channel dma
                    |  _::CFGR1_DMACFG                              // select circular mode
                    ;
#elif defined(STM32G431)
        ADC().CFGR |= _::CFGR_DMAEN                                 // enable adc channel dma
                   |  _::CFGR_DMACFG                                // select circular mode
                   ;
#else
        static_assert(false, "ADC driver not implemented");
#endif
        DMA::template disable<DMACH>();                             // disable dma channel
        DMA::template periph_to_mem<DMACH>(&ADC().DR, dest, nelem); // configure dma from memory
        DMA::template enable<DMACH>();                              // enable dma channel
        dma::dmamux_traits<DMA::INST, DMACH>::CCR() = device::dmamux_t::C0CR_DMAREQ_ID<5>;
        DMA::template enable_interrupt<DMACH, true>();
    }

    template<uint8_t SEL>
    static inline void trigger()
    {
        using namespace device;
#if defined(STM32G070)
        ADC().CFGR1 |= _::template CFGR1_EXTEN<0x1>                 // hardware trigger on rising edge
                    |  _::template CFGR1_EXTSEL<SEL>                // trigger source selection
                    ;
#elif defined(STM32G431)
        ADC().CFGR |= _::template CFGR_EXTEN<0x1>                   // hardware trigger on rising edge
                   |  _::template CFGR_EXTSEL<SEL>                  // trigger source selection
                   ;
#else
        static_assert(false, "ADC driver not implemented");
#endif
    }

    static inline void start_conversion()
    {
        ADC().CR |= _::CR_ADSTART;                              // start conversion
    }

    static inline uint16_t read()
    {
        using namespace device;

        start_conversion();                                     // start conversion
        while (!(ADC().ISR & _::ISR_EOC));                      // conversion complete
        return ADC().DR;                                        // read data register
    }
};

} // namespace adc

} // namespace hal

