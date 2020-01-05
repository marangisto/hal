#pragma once

#include <hal.h>

namespace hal
{

namespace gpio
{

enum gpio_port_t { PA, PB, PC, PD, PE, PF, PG, PH, PI, PJ, PK };

enum gpio_pin_t
    { PA0, PA1, PA2, PA3, PA4, PA5, PA6, PA7, PA8, PA9, PA10, PA11, PA12, PA13, PA14, PA15
    , PB0, PB1, PB2, PB3, PB4, PB5, PB6, PB7, PB8, PB9, PB10, PB11, PB12, PB13, PB14, PB15
    , PC0, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, PC11, PC12, PC13, PC14, PC15
    , PD0, PD1, PD2, PD3, PD4, PD5, PD6, PD7, PD8, PD9, PD10, PD11, PD12, PD13, PD14, PD15
    , PE0, PE1, PE2, PE3, PE4, PE5, PE6, PE7, PE8, PE9, PE10, PE11, PE12, PE13, PE14, PE15
    , PF0, PF1, PF2, PF3, PF4, PF5, PF6, PF7, PF8, PF9, PF10, PF11, PF12, PF13, PF14, PF15
    , PG0, PG1, PG2, PG3, PG4, PG5, PG6, PG7, PG8, PG9, PG10, PG11, PG12, PG13, PG14, PG15
    , PH0, PH1, PH2, PH3, PH4, PH5, PH6, PH7, PH8, PH9, PH10, PH11, PH12, PH13, PH14, PH15
    , PI0, PI1, PI2, PI3, PI4, PI5, PI6, PI7, PI8, PI9, PI10, PI11, PI12, PI13, PI14, PI15
    , PJ0, PJ1, PJ2, PJ3, PJ4, PJ5, PJ6, PJ7, PJ8, PJ9, PJ10, PJ11, PJ12, PJ13, PJ14, PJ15
    , PK0, PK1, PK2, PK3, PK4, PK5, PK6, PK7, PK8, PK9, PK10, PK11, PK12, PK13, PK14, PK15
    };

static inline constexpr gpio_port_t pin_port(gpio_pin_t p)
{
    return static_cast<gpio_port_t>(static_cast<int>(p) >> 4);
}

static inline constexpr int pin_bit(gpio_pin_t p)
{
    return static_cast<int>(p) & 0xf;
}

enum output_type_t { push_pull, open_drain };
enum output_speed_t { low_speed = 0x0, medium_speed = 0x1, high_speed = 0x3 };
enum input_type_t { floating, pull_up, pull_down };
enum trigger_edge_t { rising_edge, falling_edge, both_edges };

template<gpio_port_t PORT> struct port_traits {};

#if defined(HAVE_PERIPHERAL_GPIOA)
template<> struct port_traits<PA>
{
    typedef device::gpioa_t gpio_t;
    static inline gpio_t& gpio() { return device::GPIOA; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOB)
template<> struct port_traits<PB>
{
    typedef device::gpiob_t gpio_t;
    static inline gpio_t& gpio() { return device::GPIOB; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOC)
template<> struct port_traits<PC>
{
    typedef device::gpioc_t gpio_t;
    static inline gpio_t& gpio() { return device::GPIOC; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOD)
template<> struct port_traits<PD>
{
    typedef device::gpiod_t gpio_t;
    static inline gpio_t& gpio() { return device::GPIOD; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOE)
template<> struct port_traits<PE>
{
    typedef device::gpioe_t gpio_t;
    static inline gpio_t& gpio() { return device::GPIOE; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOF)
template<> struct port_traits<PF>
{
    typedef device::gpiof_t gpio_t;
    static inline gpio_t& gpio() { return device::GPIOF; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOG)
template<> struct port_traits<PG>
{
    typedef device::gpiog_t gpio_t;
    static inline gpio_t& gpio() { return device::GPIOG; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOH)
template<> struct port_traits<PH>
{
    typedef device::gpioh_t gpio_t;
    static inline gpio_t& gpio() { return device::GPIOH; }
};
#endif

template<gpio_pin_t PIN>
struct pin_t
{
    enum moder { input_mode, output_mode, alternate_mode, analog_mode };
    typedef typename port_traits<pin_port(PIN)>::gpio_t gpio_t;

    static_assert(pin_bit(PIN) < 16, "pin_t bit out of range");

    static inline gpio_t& gpio() { return port_traits<pin_port(PIN)>::gpio(); }
    static const uint8_t bit_pos = pin_bit(PIN);
    static const uint32_t bit_mask = 1 << bit_pos;
};

template<gpio_pin_t PIN>
class output_t
{
public:
    template<output_type_t output_type = push_pull, output_speed_t speed = low_speed>
    static inline void setup()
    {
        device::peripheral_traits<typename port_traits<pin_port(PIN)>::gpio_t>::enable();
#if defined(STM32F103)
        volatile uint32_t& CR = pin::bit_pos < 8 ? pin::gpio().CRL : pin::gpio().CRH;
        constexpr uint8_t shift = (pin::bit_pos < 8 ? pin::bit_pos : (pin::bit_pos - 8)) << 2;
        constexpr uint32_t bits = (speed == low_speed ? 0x2 : 0x3) | (output_type == open_drain ? 0x4 : 0);

        CR &= ~(0xf << shift);
        CR |= (bits << shift);
#else
        pin::gpio().MODER &= ~(0x3 << (pin::bit_pos*2));
        pin::gpio().MODER |= pin::output_mode << (pin::bit_pos*2);
        if (speed != low_speed)
            pin::gpio().OSPEEDR |= speed << (pin::bit_pos*2);
        if (output_type == open_drain)
            pin::gpio().OTYPER |= pin::bit_mask;
#endif
    }

    static inline void set() { pin::gpio().BSRR = pin::bit_mask; }
    static inline void clear() { pin::gpio().BSRR = pin::bit_mask << 16; }
    static inline bool read() { return (pin::gpio().ODR & pin::bit_mask) != 0; }
    static inline bool write(bool x) { x ? set() : clear(); return x; }
    static inline void toggle() { write(!read()); }

private:
    typedef pin_t<PIN> pin;
};

#if defined(STM32G431)
template<int POS, typename = is_in_range<true> >
struct syscfg_traits
{
    static_assert(always_false_i<POS>::value, "pin out of range for syscfg exticr");
};

template<int POS>
struct syscfg_traits<POS, is_in_range<(0 <= POS && POS < 4)> >
{
    static volatile uint32_t& EXTICR() { return device::SYSCFG.EXTICR1; }
};

template<int POS>
struct syscfg_traits<POS, is_in_range<(4 <= POS && POS < 8)> >
{
    static volatile uint32_t& EXTICR() { return device::SYSCFG.EXTICR2; }
};

template<int POS>
struct syscfg_traits<POS, is_in_range<(8 <= POS && POS < 12)> >
{
    static volatile uint32_t& EXTICR() { return device::SYSCFG.EXTICR3; }
};

template<int POS>
struct syscfg_traits<POS, is_in_range<(12 <= POS && POS < 16)> >
{
    static volatile uint32_t& EXTICR() { return device::SYSCFG.EXTICR4; }
};
#endif

template<gpio_pin_t PIN>
class input_t
{
public:
    template<input_type_t input_type = floating>
    static inline void setup()
    {
        device::peripheral_traits<typename port_traits<pin_port(PIN)>::gpio_t>::enable();
#if defined(STM32F103)
        volatile uint32_t& CR = pin::bit_pos < 8 ? pin::gpio().CRL : pin::gpio().CRH;
        constexpr uint8_t shift = (pin::bit_pos < 8 ? pin::bit_pos : (pin::bit_pos - 8)) << 2;
        constexpr uint32_t bits = input_type == floating ? 0x4 : 0x8;

        CR &= ~(0xf << shift);
        CR |= (bits << shift);
        if (input_type == pull_up)
            pin::gpio().ODR |= pin::bit_mask;
        else
            pin::gpio().ODR &= ~pin::bit_mask;
#else
        pin::gpio().MODER &= ~(0x3 << (pin::bit_pos*2));
        // pin::gpio().MODER |= pin::input_mode << (pin::bit_pos*2); redundant since == 0
        if (input_type != floating)
            pin::gpio().PUPDR |= input_type << (pin::bit_pos*2);
#endif
    }

    static inline bool read() { return (pin::gpio().IDR & pin::bit_mask) != 0; }

#if defined(STM32G431)
    template<trigger_edge_t EDGE = rising_edge>
    static void enable_interrupt()
    {
        using namespace device;

        peripheral_traits<syscfg_t>::enable();
        constexpr gpio_port_t port = pin_port(PIN);
        constexpr uint8_t shift = (pin_bit(PIN) & 0x3) << 2;
        volatile uint32_t& EXTICR = syscfg_traits<pin_bit(PIN)>::EXTICR();

        EXTICR &= ~(0xf << shift);
        EXTICR |= (port << shift);
        EXTI.IMR1 |= pin::bit_mask;
        if (EDGE == rising_edge || EDGE == both_edges)
            EXTI.RTSR1 |= pin::bit_mask;
        if (EDGE == falling_edge || EDGE == both_edges)
            EXTI.FTSR1 |= pin::bit_mask;

    }

    static inline bool interrupt_pending() { return (device::EXTI.PR1 & pin::bit_mask) != 0; }
    static inline void clear_interrupt() { device::EXTI.PR1 = pin::bit_mask; }
#endif

private:
    typedef pin_t<PIN> pin;
};

template<gpio_pin_t PIN>
class analog_t
{
public:
    template<input_type_t input_type = floating>
    static inline void setup()
    {
        device::peripheral_traits<typename port_traits<pin_port(PIN)>::gpio_t>::enable();
        pin::gpio().MODER |= 0x3 << (pin::bit_pos*2);
        static_assert(input_type != pull_up, "only floating or pull-down modes allowed for analog pins");
        if (input_type != floating)
            pin::gpio().PUPDR |= input_type << (pin::bit_pos*2);
    }

private:
    typedef pin_t<PIN> pin;
};

namespace internal
{

#if defined(STM32F051)
#include "gpio/stm32f051.h"
#elif defined(STM32F103)
#include "gpio/stm32f103.h"
#elif defined(STM32F411)
#include "gpio/stm32f411.h"
#elif defined(STM32F767)
#include "gpio/stm32f767.h"
#elif defined(STM32G070)
#include "gpio/stm32g070.h"
#elif defined(STM32G431)
#include "gpio/stm32g431.h"
#endif

template<gpio_pin_t PIN, alternate_function_t ALT>
class alternate_t
{
public:
    template<output_speed_t speed = low_speed, output_type_t output_type = push_pull>  // FIXME: should we not have output_type option here?
    static inline void setup()
    {
        device::peripheral_traits<typename port_traits<pin_port(PIN)>::gpio_t>::enable();
#if defined(STM32F103)
        volatile uint32_t& CR = pin::bit_pos < 8 ? pin::gpio().CRL : pin::gpio().CRH;
        constexpr uint8_t shift = (pin::bit_pos < 8 ? pin::bit_pos : (pin::bit_pos - 8)) << 2;
        constexpr uint32_t bits = 0x8 | (speed == low_speed ? 0x2 : 0x3) | (output_type == open_drain ? 0x4 : 0);

        static_assert(alt_fun_traits<PIN, ALT>::AF == AF0, "invalid alternate function (remap not yet supported)");

        CR &= ~(0xf << shift);
        CR |= (bits << shift);
#else
        pin::gpio().MODER &= ~(0x3 << (pin::bit_pos*2));
        pin::gpio().MODER |= pin::alternate_mode << (pin::bit_pos*2);
        if (speed != low_speed)
            pin::gpio().OSPEEDR |= speed << (pin::bit_pos*2);
        if (output_type == open_drain)
            pin::gpio().OTYPER |= pin::bit_mask;
        if (pin::bit_pos < 8)
            pin::gpio().AFRL |= alt_fun_traits<PIN, ALT>::AF << (pin::bit_pos*4);
        else
            pin::gpio().AFRH |= alt_fun_traits<PIN, ALT>::AF << ((pin::bit_pos-8)*4);
#endif
    }

    template<input_type_t input_type = floating>
    static inline void setup()
    {
#if defined(STM32F103)
        volatile uint32_t& CR = pin::bit_pos < 8 ? pin::gpio().CRL : pin::gpio().CRH;
        constexpr uint8_t shift = (pin::bit_pos < 8 ? pin::bit_pos : (pin::bit_pos - 8)) << 2;
        constexpr uint32_t bits = input_type == floating ? 0x4 : 0x8;

        static_assert(alt_fun_traits<PIN, ALT>::AF == AF0, "invalid alternate function (remap not yet supported)");

        CR &= ~(0xf << shift);
        CR |= (bits << shift);

        if (input_type == pull_up)
            pin::gpio().ODR |= pin::bit_mask;
        else
            pin::gpio().ODR &= ~pin::bit_mask;
#else
        device::peripheral_traits<typename port_traits<pin_port(PIN)>::gpio_t>::enable();
        pin::gpio().MODER &= ~(0x3 << (pin::bit_pos*2));
        pin::gpio().MODER |= pin::alternate_mode << (pin::bit_pos*2);
        if (input_type != floating)
            pin::gpio().PUPDR |= input_type << (pin::bit_pos*2);
        if (pin::bit_pos < 8)
            pin::gpio().AFRL |= alt_fun_traits<PIN, ALT>::AF << (pin::bit_pos*4);
        else
            pin::gpio().AFRH |= alt_fun_traits<PIN, ALT>::AF << ((pin::bit_pos-8)*4);
#endif
    }

private:
    typedef pin_t<PIN> pin;
};

}

}

}

