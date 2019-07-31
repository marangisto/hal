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
        peripheral_traits<adc12_common_t>::enable();            // enable adc clock
        ADC12_Common.CCR |= adc12_common_t::CCR_CKMODE<0x3>;    // divide clock by 4
        ADC1.CR |= _::CR_ADEN;                                  // enable adc 
        while (!(ADC1.ISR & _::ISR_ADRDY));                     // wait for adc ready 
    }

    static uint32_t read()
    {
        return 255; // FIXME: channel selection and conversion
    }
};

} // namespace adc

} // namespace hal

