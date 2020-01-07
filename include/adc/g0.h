#pragma once

namespace internal
{

template<uint8_t NO>
struct adc_impl_g0
{
    typedef typename adc_traits<NO>::T _;
    static inline typename adc_traits<NO>::T& ADC() { return adc_traits<NO>::ADC(); }

    template<uint16_t PRESCALE = 4>
    static void setup()
    {
        using namespace device;

        peripheral_traits<_>::enable();                         // enable adc clock
        ADC().CR = _::CR_RESET_VALUE;                           // reset control register
        ADC().IER = _::IER_RESET_VALUE;                         // reset interrupt register
        ADC().CFGR1 = _::CFGR1_RESET_VALUE                      // reset configuration register 1
                    | _::CFGR1_CHSELRMOD                        // use alternate channel selection mode
                    | _::CFGR1_OVRMOD                           // overwrite on overrun
                    ;
        ADC().CFGR2 = _::CFGR2_RESET_VALUE                      // reset configuration register 2
                    | _::template CFGR2_CKMODE<0x3>             // use PCLK synchronous mode
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

    static constexpr uint8_t nulch = 0xf;

    template< uint8_t S1, uint8_t S2 = nulch, uint8_t S3 = nulch, uint8_t S4 = nulch
            , uint8_t S5 = nulch, uint8_t S6 = nulch, uint8_t S7 = nulch, uint8_t S8 = nulch>
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

} // namespace internal

