#pragma once

#include <stm32f0x1.h>

namespace stm32f0x1
{

namespace gpio
{

enum port_enum_t { A, B, C, D, E, F };
enum output_type_t { push_pull, open_drain };

template<port_enum_t PORT> struct port_traits {};

template<> struct port_traits<A>
{
    typedef gpioa::gpioa_t gpio_t;
    static inline gpio_t& gpio() { return gpioa::GPIOA; }
    static inline void setup() { rcc::RCC.AHBENR |= BV(rcc::AHBENR::IOPAEN); }
};

template<> struct port_traits<C>
{
    typedef gpioc::gpioc_t gpio_t;
    static inline gpio_t& gpio() { return gpioc::GPIOC; }
    static inline void setup() { rcc::RCC.AHBENR |= BV(rcc::AHBENR::IOPCEN); }
};

template<port_enum_t PORT, int BIT>
class output_t
{
public:
    template<output_type_t ot = push_pull>
    static inline void setup()
    {
        port_traits<PORT>::setup();
        gpio().MODER |= output_mode << (BIT*2);
        if (ot == open_drain)
            gpio().OTYPER |= BV(BIT);
    }

    static inline bool get() { return (gpio().ODR & bit_mask) != 0; }
    static inline void set() { gpio().BSRR = bit_mask; }
    static inline void clear() { gpio().BRR = bit_mask; }
    static inline void write(bool x) { x ? set() : clear(); }
    static inline void toggle() { write(!get()); }

private:
    enum moder { input_mode, output_mode, alternate_mode, analog_mode };
    static_assert(BIT < 16, "pin_t bit out of range");
    typedef typename port_traits<PORT>::gpio_t gpio_t;        
    static inline gpio_t& gpio() { return port_traits<PORT>::gpio(); }
    static const uint8_t bit_pos = BIT;
    static const uint32_t bit_mask = BV(BIT);
};

}

}
