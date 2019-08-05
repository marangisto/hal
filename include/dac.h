#pragma once

#include "hal.h"

namespace hal
{

namespace dac
{

struct dac_t
{
    typedef device::dac1_t _;

    static void setup()
    {
        using namespace device;

        peripheral_traits<dac1_t>::enable();                    // enable dac clock

        /*
        ADC1.CFGR = _::CFGR_RESET_VALUE                         // reset configuration register
                  | _::CFGR_OVRMOD                              // overwrite on overrun
                  ;

        ADC1.SQR1 = _::SQR1_RESET_VALUE                         // reset sequence register
                  | _::SQR1_SQ1<1>                              // sequence channel 1
                  ;

        ADC1.SMPR1 = _::SMPR1_RESET_VALUE                         // reset sample time register
                   | _::SMPR1_SMP1<0x7>                           // max sample time    FIXME: enum & parameterize!
                   ;

        ADC1.DIFSEL = _::DIFSEL_RESET_VALUE;                    // differential mode register
        ADC1.GCOMP = _::GCOMP_RESET_VALUE;                      // reset gain compensation register
        ADC1.CFGR2 = _::CFGR2_RESET_VALUE;                      // reset configuration register 2
        ADC1.CR = _::CR_RESET_VALUE;                            // reset control register
        RCC.CCIPR1 |= rcc_t::CCIPR1_ADCSEL<0x1>;                // use pll P clock  FIXME: does not seem to have any effect!

        ADC12_Common.CCR = adc12_common_t::CCR_RESET_VALUE      // reset common control register
                         | adc12_common_t::CCR_CKMODE<0x3>;     // divide clock by 4
                         ;

        ADC1.IER = _::IER_RESET_VALUE                           // reset register
//                 | _::IER_EOCIE                                 // interrupt on conversion end
//                 | _::IER_OVRIE                                 // interrupt on overrun FIXME: do we really want this?
                 ;

        ADC1.CR &= ~_::CR_DEEPPWD;                              // disable deep power down mode
        ADC1.CR |= _::CR_ADVREGEN;                              // enable adc voltage regulator
        sys_clock::delay_us(10);                                // wait for regulator to stabilize
        ADC1.CR |= _::CR_ADCAL;                                 // start calibration
        while (ADC1.CR & _::CR_ADCAL);                          // wait for calibration to complete
        for (volatile uint8_t i = 0; i < 4; ++i);               // cycles between calibration and adc enable
        ADC1.CR |= _::CR_ADEN;                                  // enable adc 
        while (!(ADC1.ISR & _::ISR_ADRDY));                     // wait for adc ready 
        */
    }

    template<uint8_t CH>
    static inline void write(uint16_t x)
    {
        using namespace device;

        DAC1.DAC_DHR12R1 = x;  // FIXME: traits for channel
        DAC1.DAC_SWTRGR |= 0x1; // FIXME: channel traits
    }
};

} // namespace adc

} // namespace hal

