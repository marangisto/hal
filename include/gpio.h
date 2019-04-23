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

namespace internal
{

enum alt_fun_t { AF0, AF1, AF2, AF3, AF4, AF5, AF6, AF7 };

enum alternate_function_t
    { CAN_RX
    , CAN_TX
    , CEC
    , COMP1_OUT
    , COMP2_OUT
    , CRS_SYNC
    , EVENTOUT
    , I2C1_SCL
    , I2C1_SDA
    , I2C1_SMBA
    , I2C2_SCL
    , I2C2_SDA
    , I2S1_CK
    , I2S1_MCK
    , I2S1_SD
    , I2S1_WS
    , IR_OUT
    , MCO
    , SPI1_MISO
    , SPI1_MOSI
    , SPI1_NSS
    , SPI1_SCK
    , SPI2_MISO
    , SPI2_MOSI
    , SPI2_NSS
    , SPI2_SCK
    , SWCLK
    , SWDIO
    , TIM14_CH1
    , TIM15_BKIN
    , TIM15_CH1
    , TIM15_CH1N
    , TIM15_CH2
    , TIM16_BKIN
    , TIM16_CH1
    , TIM16_CH1N
    , TIM17_BKIN
    , TIM17_CH1
    , TIM17_CH1N
    , TIM1_BKIN
    , TIM1_CH1
    , TIM1_CH1N
    , TIM1_CH2
    , TIM1_CH2N
    , TIM1_CH3
    , TIM1_CH3N
    , TIM1_CH4
    , TIM1_ETR
    , TIM2_CH1_ETR
    , TIM2_CH2
    , TIM2_CH3
    , TIM2_CH4
    , TIM3_CH1
    , TIM3_CH2
    , TIM3_CH3
    , TIM3_CH4
    , TIM3_ETR
    , TSC_G1_IO1
    , TSC_G1_IO2
    , TSC_G1_IO3
    , TSC_G1_IO4
    , TSC_G2_IO1
    , TSC_G2_IO2
    , TSC_G2_IO3
    , TSC_G2_IO4
    , TSC_G3_IO2
    , TSC_G3_IO3
    , TSC_G3_IO4
    , TSC_G4_IO1
    , TSC_G4_IO2
    , TSC_G4_IO3
    , TSC_G4_IO4
    , TSC_G5_IO1
    , TSC_G5_IO2
    , TSC_G5_IO3
    , TSC_G5_IO4
    , TSC_G6_IO1
    , TSC_G6_IO2
    , TSC_G6_IO3
    , TSC_G6_IO4
    , TSC_SYNC
    , USART1_CK
    , USART1_CTS
    , USART1_RTS
    , USART1_RX
    , USART1_TX
    , USART2_CK
    , USART2_CTS
    , USART2_RTS
    , USART2_RX
    , USART2_TX
    , USART3_CTS
    , USART4_RTS
    , USART4_RX
    , USART4_TX
    , USART6_RX
    , USART6_TX
    };

template<port_enum_t PORT, int BIT, alternate_function_t ALT>
struct alt_fun_traits {};

#define ALT_FUN_TRAIT(PORT, PIN, ALT_FUN, AFNO)         \
template<> struct alt_fun_traits<PORT, PIN, ALT_FUN>    \
{                                                       \
    static inline alt_fun_t AF() { return AFNO; }       \
}

#if defined(STM32F051)
#include "gpio/stm32f051.h"
#endif

template<port_enum_t PORT, int BIT, alternate_function_t ALT>
class alternate_t
{
public:
    static inline void setup()
    {
        port_traits<PORT>::setup();
        pin::gpio().MODER |= pin::alternate_mode << (BIT*2);
        static_assert(PORT != E, "FIXME: use traits to set alternative");
    }

private:
    typedef pin_t<PORT, BIT> pin;
};

}

}

}

