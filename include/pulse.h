#pragma once

namespace hal::gpio
{

template<gpio_pin_t PIN>
struct pulse_t: output_t<PIN>
{
    static void pulse(uint16_t cycles)
    {
        output_t<PIN>::set();
        m_count = cycles;
    }

    static void update()
    {
        if (m_count > 0 && --m_count == 0)
            output_t<PIN>::clear();
    }

    static uint16_t m_count;
};

template<gpio_pin_t PIN>
uint16_t pulse_t<PIN>::m_count = 0;

} // namespace hal::gpio

