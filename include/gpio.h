#pragma once

#include <stm32f0.h>

namespace stm32f0
{

namespace gpio
{

using namespace device;

enum port_enum_t { A, B, C, D, E, F };
enum output_type_t { push_pull, open_drain };
enum input_type_t { floating, pull_up, pull_down };

template<port_enum_t PORT> struct port_traits {};

template<> struct port_traits<A>
{
    typedef gpioa_t gpio_t;
    static inline gpio_t& gpio() { return GPIOA; }
    static inline void setup() { RCC.AHBENR |= BV(rcc_t::AHBENR_IOPAEN); }
};

template<> struct port_traits<B>
{
    typedef gpiob_t gpio_t;
    static inline gpio_t& gpio() { return GPIOB; }
    static inline void setup() { RCC.AHBENR |= BV(rcc_t::AHBENR_IOPBEN); }
};

template<> struct port_traits<C>
{
    typedef gpioc_t gpio_t;
    static inline gpio_t& gpio() { return GPIOC; }
    static inline void setup() { RCC.AHBENR |= BV(rcc_t::AHBENR_IOPCEN); }
};

template<> struct port_traits<D>
{
    typedef gpiod_t gpio_t;
    static inline gpio_t& gpio() { return GPIOD; }
    static inline void setup() { RCC.AHBENR |= BV(rcc_t::AHBENR_IOPDEN); }
};

#if defined(STM32F07x) || defined(STM32F09x)
template<> struct port_traits<E>
{
    typedef gpioe_t gpio_t;
    static inline gpio_t& gpio() { return GPIOE; }
    static inline void setup() { RCC.AHBENR |= BV(rcc_t::AHBENR_IOPEEN); }
};
#endif

template<> struct port_traits<F>
{
    typedef gpiof_t gpio_t;
    static inline gpio_t& gpio() { return GPIOF; }
    static inline void setup() { RCC.AHBENR |= BV(rcc_t::AHBENR_IOPFEN); }
};

template<port_enum_t PORT, int BIT>
struct pin_t
{
    enum moder { input_mode, output_mode, alternate_mode, analog_mode };
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
    template<output_type_t output_type = push_pull>
    static inline void setup()
    {
        port_traits<PORT>::setup();
        pin::gpio().MODER |= pin::output_mode << (BIT*2);
        if (output_type == open_drain)
            pin::gpio().OTYPER |= BV(BIT);
    }

    static inline void set() { pin::gpio().BSRR = pin::bit_mask; }
    static inline void clear() { pin::gpio().BRR = pin::bit_mask; }
    static inline bool read() { return (pin::gpio().ODR & pin::bit_mask) != 0; }
    static inline void write(bool x) { x ? set() : clear(); }
    static inline void toggle() { write(!read()); }

private:
    typedef pin_t<PORT, BIT> pin;
};

template<port_enum_t PORT, int BIT>
class input_t
{
public:
    template<input_type_t input_type = floating>
    static inline void setup()
    {
        port_traits<PORT>::setup();
        pin::gpio().MODER |= pin::input_mode << (BIT*2);
        if (input_type != floating)
            pin::gpio().PUPDR |= input_type << (BIT*2);
    }

    static inline bool read() { return (pin::gpio().IDR & pin::bit_mask) != 0; }

private:
    typedef pin_t<PORT, BIT> pin;
};

}

}

