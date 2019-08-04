#pragma once

#include "hal.h"

template<> struct device::peripheral_traits<device::adc12_common_t>
{
    static void enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_ADC12EN; }
    static void disable() { RCC.AHB2ENR &= ~rcc_t::AHB2ENR_ADC12EN; }
    static void reset() { RCC.AHB2RSTR |= rcc_t::AHB2RSTR_ADC12RST; }
};

namespace hal
{

namespace adc
{

struct adc_t
{
    typedef device::adc1_t _;

    static void setup()
    {
        using namespace device;


        // FIXME: assert adc is not enabled
        peripheral_traits<adc12_common_t>::enable();            // enable adc clock

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

        ADC1.CR = _::CFGR2_RESET_VALUE                          // reset control register
                | _::CR_ADVREGEN;                               // enable adc voltage regulator
                ;

        RCC.CCIPR1 |= rcc_t::CCIPR1_ADCSEL<0x1>;                // use pll P clock  FIXME: does not seem to have any effect!

        ADC12_Common.CCR = adc12_common_t::CCR_RESET_VALUE      // reset common control register
                         | adc12_common_t::CCR_CKMODE<0x3>;     // divide clock by 4
                         ;

        ADC1.IER = _::IER_RESET_VALUE                           // reset register
//                 | _::IER_EOCIE                                 // interrupt on conversion end
                 | _::IER_OVRIE                                 // interrupt on overrun FIXME: do we really want this?
                 ;
        /*
//        ADC12_Common.CCR |= adc12_common_t::CCR_VREFEN;         // enable vref

        ADC1.CR |= _::CR_ADEN;                                  // enable adc 
        while (!(ADC1.ISR & _::ISR_ADRDY));                     // wait for adc ready 

        sys_tick::delay_ms(1);                                  // FIXME: delay 10us for regulator stability

        ADC1.CR |= _::CR_ADCAL;                                 // start calibration
        //while (ADC1.CR & _::CR_ADCAL);                          // wait for calibration to complete

        */

    }

    static inline void start_conversion()
    {
        using namespace device;

        ADC1.CR |= _::CR_ADSTART;                               // start conversions
    }

    static inline bool end_of_conversion()
    {
        using namespace device;

        return (ADC1.ISR & _::ISR_EOC) != 0;                    // start conversions
    }

    static inline uint32_t read()
    {
        using namespace device;

        return ADC1.DR;
    }

    static uint32_t isr() { return device::ADC1.ISR; }
    static uint32_t cr() { return device::ADC1.CR; }
};

} // namespace adc

} // namespace hal

