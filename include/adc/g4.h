#pragma once

namespace internal
{

template<> struct adc_traits<1>
{
    typedef device::adc1_t T;
    typedef device::adc12_common_t C;
    static inline T& ADC() { return device::ADC1; }
    static inline C& COMMON() { return device::ADC12_COMMON; }
};

template<> struct adc_traits<2>
{
    typedef device::adc2_t T;
    typedef device::adc12_common_t C;
    static inline T& ADC() { return device::ADC2; }
    static inline C& COMMON() { return device::ADC12_COMMON; }
};

template<uint16_t> struct prescale_traits {};

template<> struct prescale_traits<1> { static const uint8_t presc = 0x0; };
template<> struct prescale_traits<2> { static const uint8_t presc = 0x1; };
template<> struct prescale_traits<4> { static const uint8_t presc = 0x2; };
template<> struct prescale_traits<6> { static const uint8_t presc = 0x3; };
template<> struct prescale_traits<8> { static const uint8_t presc = 0x4; };
template<> struct prescale_traits<10> { static const uint8_t presc = 0x5; };
template<> struct prescale_traits<12> { static const uint8_t presc = 0x6; };
template<> struct prescale_traits<16> { static const uint8_t presc = 0x7; };
template<> struct prescale_traits<32> { static const uint8_t presc = 0x8; };
template<> struct prescale_traits<64> { static const uint8_t presc = 0x9; };
template<> struct prescale_traits<128> { static const uint8_t presc = 0xa; };
template<> struct prescale_traits<256> { static const uint8_t presc = 0xb; };

template<uint16_t> struct oversampling_traits {};

template<> struct oversampling_traits<2> { static const uint8_t ratio = 0x0; };
template<> struct oversampling_traits<4> { static const uint8_t ratio = 0x1; };
template<> struct oversampling_traits<8> { static const uint8_t ratio = 0x2; };
template<> struct oversampling_traits<16> { static const uint8_t ratio = 0x3; };
template<> struct oversampling_traits<32> { static const uint8_t ratio = 0x4; };
template<> struct oversampling_traits<64> { static const uint8_t ratio = 0x5; };
template<> struct oversampling_traits<128> { static const uint8_t ratio = 0x6; };
template<> struct oversampling_traits<256> { static const uint8_t ratio = 0x7; };

template<uint8_t NO>
struct adc_impl_g4
{
    typedef typename adc_traits<NO>::T _;
    static inline typename adc_traits<NO>::T& ADC() { return adc_traits<NO>::ADC(); }

    typedef typename adc_traits<NO>::C __;
    static inline typename adc_traits<NO>::C& COMMON() { return adc_traits<NO>::COMMON(); }

    template<uint16_t PRESCALE = 4>
    static void setup()
    {
        using namespace device;

        peripheral_traits<__>::enable();                        // enable common adc clock
        ADC().CR = _::CR_RESET_VALUE;                           // reset control register
        ADC().IER = _::IER_RESET_VALUE;                         // reset interrupt register
        ADC().CFGR = _::CFGR_RESET_VALUE                        // reset configuration register 1
                    | _::CFGR_OVRMOD                            // overwrite on overrun
                    ;
        RCC.CCIPR1 |= rcc_t::template CCIPR1_ADCSEL<0x1>;       // use pll P clock
        COMMON().CCR = __::CCR_RESET_VALUE                      // reset common control register
                     | __::template CCR_PRESC<prescale_traits<PRESCALE>::presc>
                     ;
        ADC().DIFSEL = _::DIFSEL_RESET_VALUE;                   // differential mode register
        ADC().GCOMP = _::GCOMP_RESET_VALUE;                     // reset gain compensation register
        ADC().CFGR2 = _::CFGR2_RESET_VALUE;                     // reset configuration register 2
    }


    static void enable()
    {
        using namespace device;
        uint32_t saved_dmaen = ADC().CFGR & _::CFGR_DMAEN;      // save DMA flag 

        ADC().CR &= ~_::CR_DEEPPWD;                             // disable deep power down mode
        ADC().CR |= _::CR_ADVREGEN;                             // enable adc voltage regulator
        sys_tick::delay_us(10);                                 // wait for regulator to stabilize
        ADC().CFGR &= ~_::CFGR_DMAEN;                           // ensure DMA is disabled during calibration
        ADC().CR |= _::CR_ADCAL;                                // start calibration
        while (ADC().CR & _::CR_ADCAL);                         // wait for calibration to complete
        ADC().CFGR |= saved_dmaen;                              // restore DMA setting
        for (volatile uint8_t i = 0; i < 4; ++i);               // cycles between calibration and adc enable
        ADC().ISR |= _::ISR_ADRDY;                              // clear ready flag by writing '1'
        ADC().CR |= _::CR_ADEN;                                 // enable adc 
        while (!(ADC().ISR & _::ISR_ADRDY));                    // wait for adc ready 
    }

    template<uint16_t K>
    static void oversample()
    {
        using namespace device;

        ADC().CFGR2 |= _::CFGR2_ROVSE
                    |  _::template CFGR2_OVSR<oversampling_traits<K>::ratio>
                    |  _::template CFGR2_OVSS<oversampling_traits<K>::ratio + 1>
                    ;
    }

    template<uint8_t X>
    static void sample_time()
    {
        ADC().SMPR1 = _::SMPR1_RESET_VALUE
                    | _::template SMPR1_SMP0<X>
                    | _::template SMPR1_SMP1<X>
                    | _::template SMPR1_SMP2<X>
                    | _::template SMPR1_SMP3<X>
                    | _::template SMPR1_SMP4<X>
                    | _::template SMPR1_SMP5<X>
                    | _::template SMPR1_SMP6<X>
                    | _::template SMPR1_SMP7<X>
                    | _::template SMPR1_SMP8<X>
                    | _::template SMPR1_SMP9<X>
                    ;
    }

    static constexpr uint8_t nulch = 0x1f;

    template< uint8_t S1, uint8_t S2 = nulch, uint8_t S3 = nulch, uint8_t S4 = nulch
            , uint8_t S5 = nulch, uint8_t S6 = nulch, uint8_t S7 = nulch, uint8_t S8 = nulch>
    static void sequence()
    {
        using namespace device;

        static constexpr uint8_t L = (S2 != nulch ? 1 : 0)
                                   + (S3 != nulch ? 1 : 0)
                                   + (S4 != nulch ? 1 : 0)
                                   + (S5 != nulch ? 1 : 0)
                                   + (S6 != nulch ? 1 : 0)
                                   + (S7 != nulch ? 1 : 0)
                                   + (S8 != nulch ? 1 : 0)
                                   ;

        ADC().SQR1 = _::SQR1_RESET_VALUE                        // reset sequence 1 register
                  | _::template SQR1_L<L>                       // sequence length less one
                  | _::template SQR1_SQ1<S1>                    // sequence slot 1
                  | _::template SQR1_SQ2<S2>                    // sequence slot 2
                  | _::template SQR1_SQ3<S3>                    // sequence slot 2
                  | _::template SQR1_SQ4<S4>                    // sequence slot 4
                  ;
        ADC().SQR2 = _::SQR2_RESET_VALUE                        // reset sequence 2 register
                  | _::template SQR2_SQ5<S5>                    // sequence slot 5
                  | _::template SQR2_SQ6<S6>                    // sequence slot 6
                  | _::template SQR2_SQ7<S7>                    // sequence slot 7
                  | _::template SQR2_SQ8<S8>                    // sequence slot 8
                  ;
    }

    template<typename DMA, uint8_t DMACH, typename T>
    static inline void dma(volatile T *dest, uint16_t nelem)
    {
        using namespace device;

        ADC().CFGR |= _::CFGR_DMAEN                                 // enable adc channel dma
                   |  _::CFGR_DMACFG                                // select circular mode
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

        ADC().CFGR |= _::template CFGR_EXTEN<0x1>                   // hardware trigger on rising edge
                   |  _::template CFGR_EXTSEL<SEL>                  // trigger source selection
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

