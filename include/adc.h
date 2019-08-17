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

#if defined(STM32G431)
template<uint8_t NO>
struct adc_t
{
    typedef typename adc_traits<NO>::T _;
    typedef typename adc_traits<NO>::C __;
    static inline typename adc_traits<NO>::T& ADC() { return adc_traits<NO>::ADC(); }
    static inline typename adc_traits<NO>::C& COMMON() { return adc_traits<NO>::COMMON(); }

    static void setup()
    {
        using namespace device;

        ADC().CR = _::CR_RESET_VALUE;                           // reset control register
 
        peripheral_traits<__>::enable();                        // enable common adc clock

        ADC().CFGR = _::CFGR_RESET_VALUE                        // reset configuration register
                  | _::CFGR_OVRMOD                              // overwrite on overrun
                  ;

        ADC().SQR1 = _::SQR1_RESET_VALUE                        // reset sequence register
                  | _::template SQR1_SQ1<1>                     // sequence channel 1
                  ;

        ADC().SMPR1 = _::SMPR1_RESET_VALUE                      // reset sample time register
                   | _::template SMPR1_SMP1<0x0>                // max sample time    FIXME: enum & parameterize!
                   ;

        ADC().DIFSEL = _::DIFSEL_RESET_VALUE;                   // differential mode register
        ADC().GCOMP = _::GCOMP_RESET_VALUE;                     // reset gain compensation register
        ADC().CFGR2 = _::CFGR2_RESET_VALUE;                     // reset configuration register 2

        RCC.CCIPR1 |= rcc_t::template CCIPR1_ADCSEL<0x1>;       // use pll P clock  FIXME: does not seem to have any effect!

        COMMON().CCR = __::CCR_RESET_VALUE                      // reset common control register
                     | __::template CCR_CKMODE<0x3>;            // divide clock by 4
                     ;

        ADC().IER = _::IER_RESET_VALUE;                         // reset interrupt register
        ADC().CR &= ~_::CR_DEEPPWD;                             // disable deep power down mode
        ADC().CR |= _::CR_ADVREGEN;                             // enable adc voltage regulator
        sys_clock::delay_us(10);                                // wait for regulator to stabilize
        ADC().CR |= _::CR_ADCAL;                                // start calibration
        while (ADC().CR & _::CR_ADCAL);                         // wait for calibration to complete
        for (volatile uint8_t i = 0; i < 4; ++i);               // cycles between calibration and adc enable
        ADC().CR |= _::CR_ADEN;                                 // enable adc 
        while (!(ADC().ISR & _::ISR_ADRDY));                    // wait for adc ready 
    }

    template<typename DMA, uint8_t DMACH, typename T>
    static inline void enable_dma(volatile T *dest, uint16_t nelem)
    {
        ADC().CFGR |= _::CFGR_DMAEN                                 // enable adc channel dma
                   |  _::CFGR_DMACFG                                // select circular mode
                   |  _::CFGR_CONT                                  // continuous conversion mode
                   |  _::CFGR_AUTDLY                                // conversion auto-delay
                   ;
        DMA::template disable<DMACH>();                             // disable dma channel
        DMA::template periph_to_mem<DMACH>(&ADC().DR, dest, nelem); // configure dma from memory
        DMA::template enable<DMACH>();                              // enable dma channel
        dma::dmamux_traits<DMA::INST, DMACH>::CCR() = device::dmamux_t::C0CR_DMAREQ_ID<5>;
    }

    template<uint8_t SEL>
    static inline void enable_trigger()
    {
        ADC().CFGR |= _::template CFGR_EXTEN<0x1>                   // hardware trigger on rising edge
                   |  _::template CFGR_EXTSEL<SEL>                  // trigger source selection
                   ;
    }

    template<typename DMA, uint8_t DMACH>
    static inline void disable_dma()
    {
        ADC().CFGR &= ~_::CFGR_DMAEN;                               // disable adc channel dma
        DMA::template abort<DMACH>();                               // stop dma on relevant dma channel
    }

    static inline void enable_interrupt()
    {
        ADC().IER = 0x7ff;
    }

    static inline uint32_t interrupt_status()
    {
        return ADC().ISR;
    }

    static inline uint32_t clear_interrupt_flags()
    {
        return ADC().ISR = 0x7ff;                   
    }

    static inline void start_conversion()
    {
        using namespace device;

        ADC().CR |= _::CR_ADSTART;                              // start conversions
    }

    static inline bool end_of_conversion()
    {
        using namespace device;

        return (ADC().ISR & _::ISR_EOC) != 0;                   // conversion complete
    }

    static inline uint16_t read()
    {
        using namespace device;

        start_conversion();
        while (!end_of_conversion());
        return ADC().DR;
    }
};
#elif defined(STM32G070)
template<uint8_t NO>
struct adc_t
{
    typedef typename adc_traits<NO>::T _;
    static inline typename adc_traits<NO>::T& ADC() { return adc_traits<NO>::ADC(); }

    static void setup()
    {
        using namespace device;

        peripheral_traits<_>::enable();                         // enable adc clock
        ADC().CR = _::CR_RESET_VALUE;                           // reset control register
        ADC().CFGR1 = _::CFGR1_RESET_VALUE                      // reset configuration register 1
                    | _::CFGR1_CHSELRMOD                        // use alternate channel selection mode
                    | _::CFGR1_OVRMOD                           // overwrite on overrun
                    ;
        ADC().CFGR2 = _::CFGR2_RESET_VALUE                      // reset configuration register 2
                    | _::template CFGR2_CKMODE<0x3>             // use PCLK synchronous mode
                    ;
    }

    template< uint8_t S1, uint8_t S2 = 0xf, uint8_t S3 = 0xf, uint8_t S4 = 0xf
            , uint8_t S5 = 0xf, uint8_t S6 = 0xf, uint8_t S7 = 0xf, uint8_t S8 = 0xf>
    static void sequence()
    {
        using namespace device;

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
    }

    template<typename DMA, uint8_t DMACH, typename T>
    static inline void dma(volatile T *dest, uint16_t nelem)
    {
        using namespace device;

        ADC().CFGR1 |= _::CFGR1_DMAEN                               // enable adc channel dma
                    |  _::CFGR1_DMACFG                              // select circular mode
                    ;
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

        ADC().CFGR1 |= _::template CFGR1_EXTEN<0x1>                 // hardware trigger on rising edge
                    |  _::template CFGR1_EXTSEL<SEL>                // trigger source selection
                    ;
    }

    static void enable()
    {
        using namespace device;
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
#endif

} // namespace adc

} // namespace hal

