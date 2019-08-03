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
    static void setup()
    {
        using namespace device;
        typedef adc1_t _;

        RCC.CCIPR1 |= rcc_t::CCIPR1_ADCSEL<0x2>;                // use system clock

        peripheral_traits<syscfg_t>::enable();                  // enable sys-config clock
        peripheral_traits<pwr_t>::enable();                     // enable power control clock
        peripheral_traits<adc12_common_t>::enable();            // enable adc clock


        // FIXME: assert adc is not enabled

        ADC1.CFGR = _::CFGR_RESET_VALUE                         // reset register
                  | _::CFGR_OVRMOD                              // overwrite on overrun
                  ;

        ADC1.SQR1 = _::SQR1_RESET_VALUE                         // reset register
                  | _::SQR1_SQ1<1>                              // sequence channel 1        FIXME: check!
                  ;

        ADC1.CR |= _::CR_ADVREGEN;                              // enable adc voltage regulator

        ADC12_Common.CCR |= adc12_common_t::CCR_CKMODE<0x3>;    // divide clock by 4
//        ADC12_Common.CCR |= adc12_common_t::CCR_VREFEN;         // enable vref

        ADC1.CR |= _::CR_ADEN;                                  // enable adc 
        while (!(ADC1.ISR & _::ISR_ADRDY));                     // wait for adc ready 

        ADC1.IER = _::IER_RESET_VALUE                           // reset register
                 | _::IER_EOCIE                                 // interrupt on conversion end
                 | _::IER_OVRIE                                 // interrupt on overrun
                 ;
        sys_tick::delay_ms(1);                                  // FIXME: delay 10us for regulator stability

        ADC1.CR |= _::CR_ADCAL;                                 // start calibration
        //while (ADC1.CR & _::CR_ADCAL);                          // wait for calibration to complete

//        ADC1.CR |= _::CR_ADSTART;                               // start conversions
    }

    static uint32_t read()
    {
        using namespace device;

        return ADC1.DR;
    }

    static uint32_t isr() { return device::ADC1.ISR; }
    static uint32_t cr() { return device::ADC1.CR; }
};

} // namespace adc

} // namespace hal

