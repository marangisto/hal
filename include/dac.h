#pragma once

#include "hal.h"
#include "gpio.h"

namespace hal
{

namespace dac
{

struct dac_t
{
    typedef device::dac1_t _;

    static void setup()
    {
        using namespace gpio;
        using namespace device;

        analog_t<PA4>::setup<floating>();                       // FIXME: channel traits pin
        peripheral_traits<dac1_t>::enable();                    // enable dac clock

        DAC1.DAC_CR = _::DAC_CR_RESET_VALUE;                    // reset control register
        DAC1.DAC_MCR = _::DAC_MCR_RESET_VALUE;                  // reset mode control register
        DAC1.DAC_CR |= _::DAC_CR_EN1;                           // enable channel 1
        sys_clock::delay_us(8);                                 // wait for voltage to settle
        DAC1.DAC_CR |= _::DAC_CR_TEN1;                          // enable trigger for channel 1
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

