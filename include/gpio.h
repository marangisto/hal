#pragma once

#include <stm32f0.h>

namespace stm32f0
{

namespace gpio
{

using namespace device;

enum port_enum_t { A, B, C, D, E, F };
enum output_type_t { push_pull, open_drain };

template<port_enum_t PORT> struct port_traits {};

template<> struct port_traits<A>
{
    typedef gpioa::gpioa_t gpio_t;
    static inline gpio_t& gpio() { return gpioa::GPIOA; }
    static inline void setup() { rcc::RCC.AHBENR |= BV(rcc::AHBENR::IOPAEN); }
};

template<> struct port_traits<B>
{
    typedef gpiob::gpiob_t gpio_t;
    static inline gpio_t& gpio() { return gpiob::GPIOB; }
    static inline void setup() { rcc::RCC.AHBENR |= BV(rcc::AHBENR::IOPBEN); }
};

template<> struct port_traits<C>
{
    typedef gpioc::gpioc_t gpio_t;
    static inline gpio_t& gpio() { return gpioc::GPIOC; }
    static inline void setup() { rcc::RCC.AHBENR |= BV(rcc::AHBENR::IOPCEN); }
};

template<> struct port_traits<D>
{
    typedef gpiod::gpiod_t gpio_t;
    static inline gpio_t& gpio() { return gpiod::GPIOD; }
    static inline void setup() { rcc::RCC.AHBENR |= BV(rcc::AHBENR::IOPDEN); }
};

#if defined(STM32F07x) || defined(STM32F09x)
template<> struct port_traits<E>
{
    typedef gpioe::gpioe_t gpio_t;
    static inline gpio_t& gpio() { return gpioe::GPIOE; }
    static inline void setup() { rcc::RCC.AHBENR |= BV(rcc::AHBENR::IOPEEN); }
};
#endif

template<> struct port_traits<F>
{
    typedef gpiof::gpiof_t gpio_t;
    static inline gpio_t& gpio() { return gpiof::GPIOF; }
    static inline void setup() { rcc::RCC.AHBENR |= BV(rcc::AHBENR::IOPFEN); }
};

template<port_enum_t PORT, int BIT>
struct pin_t
{
    static_assert(BIT < 16, "pin_t bit out of range");
    typedef typename port_traits<PORT>::gpio_t gpio_t;
    static inline gpio_t& gpio() { return port_traits<PORT>::gpio(); }
    static const uint8_t bit_pos = BIT;
    static const uint32_t bit_mask = BV(BIT);
};

template<port_enum_t PORT, int BIT>
class output_t
{
public:
    template<output_type_t ot = push_pull>
    static inline void setup()
    {
        port_traits<PORT>::setup();
        pin::gpio().MODER |= output_mode << (BIT*2);
        if (ot == open_drain)
            pin::gpio().OTYPER |= BV(BIT);
    }

    static inline bool get() { return (pin::gpio().ODR & pin::bit_mask) != 0; }
    static inline void set() { pin::gpio().BSRR = pin::bit_mask; }
    static inline void clear() { pin::gpio().BRR = pin::bit_mask; }
    static inline void write(bool x) { x ? set() : clear(); }
    static inline void toggle() { write(!get()); }

private:
    typedef pin_t<PORT, BIT> pin;
    enum moder { input_mode, output_mode, alternate_mode, analog_mode };
};

}

}

