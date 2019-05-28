#pragma once

#include <stm32f0.h>

namespace stm32f0
{

namespace gpio
{

using namespace device;

enum gpio_port_t { PA, PB, PC, PD, PE, PF };

enum gpio_pin_t
    { PA0, PA1, PA2, PA3, PA4, PA5, PA6, PA7, PA8, PA9, PA10, PA11, PA12, PA13, PA14, PA15
    , PB0, PB1, PB2, PB3, PB4, PB5, PB6, PB7, PB8, PB9, PB10, PB11, PB12, PB13, PB14, PB15
    , PC0, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, PC11, PC12, PC13, PC14, PC15
    , PD0, PD1, PD2, PD3, PD4, PD5, PD6, PD7, PD8, PD9, PD10, PD11, PD12, PD13, PD14, PD15
    , PE0, PE1, PE2, PE3, PE4, PE5, PE6, PE7, PE8, PE9, PE10, PE11, PE12, PE13, PE14, PE15
    , PF0, PF1, PF2, PF3, PF4, PF5, PF6, PF7, PF8, PF9, PF10, PF11, PF12, PF13, PF14, PF15
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

template<gpio_port_t PORT> struct port_traits {};

template<> struct port_traits<PA>
{
    typedef gpioa_t gpio_t;
    static inline gpio_t& gpio() { return GPIOA; }
    static inline void setup() { RCC.AHBENR |= rcc_t::AHBENR_IOPAEN; }
};

template<> struct port_traits<PB>
{
    typedef gpiob_t gpio_t;
    static inline gpio_t& gpio() { return GPIOB; }
    static inline void setup() { RCC.AHBENR |= rcc_t::AHBENR_IOPBEN; }
};

template<> struct port_traits<PC>
{
    typedef gpioc_t gpio_t;
    static inline gpio_t& gpio() { return GPIOC; }
    static inline void setup() { RCC.AHBENR |= rcc_t::AHBENR_IOPCEN; }
};

template<> struct port_traits<PD>
{
    typedef gpiod_t gpio_t;
    static inline gpio_t& gpio() { return GPIOD; }
    static inline void setup() { RCC.AHBENR |= rcc_t::AHBENR_IOPDEN; }
};

#if defined(STM32F07x) || defined(STM32F09x)
template<> struct port_traits<PE>
{
    typedef gpioe_t gpio_t;
    static inline gpio_t& gpio() { return GPIOE; }
    static inline void setup() { RCC.AHBENR |= rcc_t::AHBENR_IOPEEN; }
};
#endif

template<> struct port_traits<PF>
{
    typedef gpiof_t gpio_t;
    static inline gpio_t& gpio() { return GPIOF; }
    static inline void setup() { RCC.AHBENR |= rcc_t::AHBENR_IOPFEN; }
};

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
        port_traits<pin_port(PIN)>::setup();
        pin::gpio().MODER |= pin::output_mode << (pin::bit_pos*2);
        if (speed != low_speed)
            pin::gpio().OSPEEDR |= speed << (pin::bit_pos*2);
        if (output_type == open_drain)
            pin::gpio().OTYPER |= pin::bit_mask;
    }

    static inline void set() { pin::gpio().BSRR = pin::bit_mask; }
    static inline void clear() { pin::gpio().BRR = pin::bit_mask; }
    static inline bool read() { return (pin::gpio().ODR & pin::bit_mask) != 0; }
    static inline void write(bool x) { x ? set() : clear(); }
    static inline void toggle() { write(!read()); }

private:
    typedef pin_t<PIN> pin;
};

template<gpio_pin_t PIN>
class input_t
{
public:
    template<input_type_t input_type = floating>
    static inline void setup()
    {
        port_traits<pin_port(PIN)>::setup();
        pin::gpio().MODER |= pin::input_mode << (pin::bit_pos*2);
        if (input_type != floating)
            pin::gpio().PUPDR |= input_type << (pin::bit_pos*2);
    }

    static inline bool read() { return (pin::gpio().IDR & pin::bit_mask) != 0; }

private:
    typedef pin_t<PIN> pin;
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
    , TSC_G3_IO1
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

template<gpio_pin_t PIN, alternate_function_t ALT>
struct alt_fun_traits {};

#define ALT_FUN_TRAIT(PIN, ALT_FUN, AFNO)           \
template<> struct alt_fun_traits<PIN, ALT_FUN>      \
{                                                   \
    static inline alt_fun_t AF() { return AFNO; }   \
}

#if defined(STM32F051)
#include "gpio/stm32f051.h"
#endif

template<gpio_pin_t PIN, alternate_function_t ALT>
class alternate_t
{
public:
    template<output_speed_t speed = low_speed>
    static inline void setup()
    {
        port_traits<pin_port(PIN)>::setup();
        pin::gpio().MODER |= pin::alternate_mode << (pin::bit_pos*2);
        if (speed != low_speed)
            pin::gpio().OSPEEDR |= speed << (pin::bit_pos*2);
        if (pin::bit_pos < 8)
            pin::gpio().AFRL |= alt_fun_traits<PIN, ALT>::AF() << (pin::bit_pos*4);
        else
            pin::gpio().AFRH |= alt_fun_traits<PIN, ALT>::AF() << ((pin::bit_pos-8)*4);
    }

    template<input_type_t input_type = floating>
    static inline void setup()
    {
        port_traits<pin_port(PIN)>::setup();
        pin::gpio().MODER |= pin::alternate_mode << (pin::bit_pos*2);
        if (input_type != floating)
            pin::gpio().PUPDR |= input_type << (pin::bit_pos*2);
        if (pin::bit_pos < 8)
            pin::gpio().AFRL |= alt_fun_traits<PIN, ALT>::AF() << (pin::bit_pos*4);
        else
            pin::gpio().AFRH |= alt_fun_traits<PIN, ALT>::AF() << ((pin::bit_pos-8)*4);
    }

private:
    typedef pin_t<PIN> pin;
};

}

}

}

