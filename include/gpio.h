#pragma once

#include <hal.h>

namespace hal
{

namespace gpio
{

using namespace device;

enum gpio_port_t { PA, PB, PC, PD, PE, PF, PG, PH };

enum gpio_pin_t
    { PA0, PA1, PA2, PA3, PA4, PA5, PA6, PA7, PA8, PA9, PA10, PA11, PA12, PA13, PA14, PA15
    , PB0, PB1, PB2, PB3, PB4, PB5, PB6, PB7, PB8, PB9, PB10, PB11, PB12, PB13, PB14, PB15
    , PC0, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, PC11, PC12, PC13, PC14, PC15
    , PD0, PD1, PD2, PD3, PD4, PD5, PD6, PD7, PD8, PD9, PD10, PD11, PD12, PD13, PD14, PD15
    , PE0, PE1, PE2, PE3, PE4, PE5, PE6, PE7, PE8, PE9, PE10, PE11, PE12, PE13, PE14, PE15
    , PF0, PF1, PF2, PF3, PF4, PF5, PF6, PF7, PF8, PF9, PF10, PF11, PF12, PF13, PF14, PF15
    , PG0, PG1, PG2, PG3, PG4, PG5, PG6, PG7, PG8, PG9, PG10, PG11, PG12, PG13, PG14, PG15
    , PH0, PH1, PH2, PH3, PH4, PH5, PH6, PH7, PH8, PH9, PH10, PH11, PH12, PH13, PH14, PH15
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

#if defined(HAVE_PERIPHERAL_GPIOA)
template<> struct port_traits<PA>
{
    typedef gpioa_t gpio_t;
    static inline gpio_t& gpio() { return GPIOA; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOB)
template<> struct port_traits<PB>
{
    typedef gpiob_t gpio_t;
    static inline gpio_t& gpio() { return GPIOB; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOC)
template<> struct port_traits<PC>
{
    typedef gpioc_t gpio_t;
    static inline gpio_t& gpio() { return GPIOC; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOD)
template<> struct port_traits<PD>
{
    typedef gpiod_t gpio_t;
    static inline gpio_t& gpio() { return GPIOD; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOE)
template<> struct port_traits<PE>
{
    typedef gpioe_t gpio_t;
    static inline gpio_t& gpio() { return GPIOE; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOF)
template<> struct port_traits<PF>
{
    typedef gpiof_t gpio_t;
    static inline gpio_t& gpio() { return GPIOF; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOG)
template<> struct port_traits<PG>
{
    typedef gpiog_t gpio_t;
    static inline gpio_t& gpio() { return GPIOG; }
};
#endif

#if defined(HAVE_PERIPHERAL_GPIOH)
template<> struct port_traits<PH>
{
    typedef gpioh_t gpio_t;
    static inline gpio_t& gpio() { return GPIOH; }
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
        peripheral_traits<typename port_traits<pin_port(PIN)>::gpio_t>::enable();
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

template<gpio_pin_t PIN>
class input_t
{
public:
    template<input_type_t input_type = floating>
    static inline void setup()
    {
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
        peripheral_traits<typename port_traits<pin_port(PIN)>::gpio_t>::enable();
        pin::gpio().MODER &= ~(0x3 << (pin::bit_pos*2));
        // pin::gpio().MODER |= pin::input_mode << (pin::bit_pos*2); redundant since == 0
        if (input_type != floating)
            pin::gpio().PUPDR |= input_type << (pin::bit_pos*2);
#endif
    }

    static inline bool read() { return (pin::gpio().IDR & pin::bit_mask) != 0; }

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
        peripheral_traits<typename port_traits<pin_port(PIN)>::gpio_t>::enable();
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

enum alt_fun_t { AF0, AF1, AF2, AF3, AF4, AF5, AF6, AF7, AF8, AF9, AF10, AF11, AF12, AF13, AF14, AF15 };

enum alternate_function_t
    { ADC12_IN0
    , ADC12_IN1
    , ADC12_IN10
    , ADC12_IN11
    , ADC12_IN12
    , ADC12_IN13
    , ADC12_IN14
    , ADC12_IN15
    , ADC12_IN2
    , ADC12_IN3
    , ADC12_IN4
    , ADC12_IN5
    , ADC12_IN6
    , ADC12_IN7
    , ADC12_IN8
    , ADC12_IN9
    , CAN_RX
    , CAN_TX
    , CEC
    , COMP1_OUT
    , COMP2_OUT
    , COMP3_OUT
    , COMP4_OUT
    , CRS_SYNC
    , EVENT_OUT
    , EVENTOUT
    , FDCAN1_RX
    , FDCAN1_TX
    , I2C1_SCL
    , I2C1_SDA
    , I2C1_SMBA
    , I2C2_SCL
    , I2C2_SDA
    , I2C2_SMBA
    , I2C3_SCL
    , I2C3_SDA
    , I2C3_SMBA
    , I2S_CKIN
    , I2S1_CK
    , I2S1_MCK
    , I2S1_SD
    , I2S1_WS
    , I2S2_CK
    , I2S2_CKIN
    , I2S2_MCK
    , I2S2_SD
    , I2S2_WS
    , I2S2ext_SD
    , I2S3_CK
    , I2S3_MCK
    , I2S3_SD
    , I2S3_WS
    , I2S3ext_SD
    , I2S4_CK
    , I2S4_MCK
    , I2S4_SD
    , I2S4_WS
    , I2S5_CK
    , I2S5_SD
    , I2S5_WS
    , I2SCKIN
    , IR_OUT
    , JTCK
    , JTDI
    , JTDO
    , JTDO_TRACESWO
    , JTMS
    , JTRST
    , LPTIM1_ETR
    , LPTIM1_IN1
    , LPTIM1_IN2
    , LPTIM1_OUT
    , LPUART1_CTS
    , LPUART1_RTS_DE
    , LPUART1_RX
    , LPUART1_TX
    , MCO
    , MCO_1
    , MCO_2
    , OSC_EN
    , OSC32_EN
    , OSC32_IN
    , OSC32_OUT
    , RTC_50H
    , RTC_OUT2
    , RTC_REFIN
    , SAI1_CK1
    , SAI1_CK2
    , SAI1_D1
    , SAI1_D2
    , SAI1_D3
    , SAI1_FS_A
    , SAI1_FS_B
    , SAI1_MCLK_A
    , SAI1_MCLK_B
    , SAI1_SCK_A
    , SAI1_SCK_B
    , SAI1_SD_A
    , SAI1_SD_B
    , SDIO_CK
    , SDIO_CMd
    , SDIO_CMD
    , SDIO_D0
    , SDIO_D1
    , SDIO_D2
    , SDIO_D3
    , SDIO_D4
    , SDIO_D5
    , SDIO_D6
    , SDIO_D7
    , SPI1_MISO
    , SPI1_MOSI
    , SPI1_NSS
    , SPI1_SCK
    , SPI2_MISO
    , SPI2_MOSI
    , SPI2_NSS
    , SPI2_SCK
    , SPI3_MISO
    , SPI3_MOSI
    , SPI3_NSS
    , SPI3_SCK
    , SPI4_MISO
    , SPI4_MOSI
    , SPI4_NSS
    , SPI4_SCK
    , SPI5_MISO
    , SPI5_MOSI
    , SPI5_NSS
    , SPI5_SCK
    , SWCLK
    , SWCLK_JTCK
    , SWDIO
    , SWDIO_JTMS
    , SWO
    , TAMPER_RTC
    , TIM1_BKIN
    , TIM1_BKIN2
    , TIM1_CH1
    , TIM1_CH1N
    , TIM1_CH2
    , TIM1_CH2N
    , TIM1_CH3
    , TIM1_CH3N
    , TIM1_CH4
    , TIM1_CH4N
    , TIM1_ETR
    , TIM10_CH1
    , TIM11_CH1
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
    , TIM2_CH1
    , TIM2_CH1_ETR
    , TIM2_CH2
    , TIM2_CH3
    , TIM2_CH4
    , TIM2_ETR
    , TIM3_CH1
    , TIM3_CH2
    , TIM3_CH3
    , TIM3_CH4
    , TIM3_ETR
    , TIM4_CH1
    , TIM4_CH2
    , TIM4_CH3
    , TIM4_CH4
    , TIM4_ETR
    , TIM5_CH1
    , TIM5_CH2
    , TIM5_CH3
    , TIM5_CH4
    , TIM8_BKIN
    , TIM8_BKIN2
    , TIM8_CH1
    , TIM8_CH1N
    , TIM8_CH2
    , TIM8_CH2N
    , TIM8_CH3
    , TIM8_CH3N
    , TIM8_CH4
    , TIM8_CH4N
    , TIM8_ETR
    , TIM9_CH1
    , TIM9_CH2
    , TRACECK
    , TRACECLK
    , TRACED0
    , TRACED1
    , TRACED2
    , TRACED3
    , TRACESWO
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
    , UART4_CTS
    , UART4_RTS_DE
    , UART4_RX
    , UART4_TX
    , UCPD1_FRSTX
    , USART1_CK
    , USART1_CTS
    , USART1_RTS
    , USART1_RTS_DE
    , USART1_RTS_DE_CK
    , USART1_RX
    , USART1_TX
    , USART2_CK
    , USART2_CTS
    , USART2_RTS
    , USART2_RTS_DE
    , USART2_RTS_DE_CK
    , USART2_RX
    , USART2_TX
    , USART3_CK
    , USART3_CTS
    , USART3_RTS
    , USART3_RTS_DE
    , USART3_RTS_DE_CK
    , USART3_RX
    , USART3_TX
    , USART4_CTS
    , USART4_RTS
    , USART4_RTS_DE_CK
    , USART4_RX
    , USART4_TX
    , USART6_CK
    , USART6_RX
    , USART6_TX
    , USB_CRS_SYNC
    , USB_FS_DM
    , USB_FS_DP
    , USB_FS_ID
    , USB_FS_SOF
    , USB_FS_VBUS
    , USBDM
    , USBDP
    , WKUP
    };

template<gpio_pin_t PIN, alternate_function_t ALT>
struct alt_fun_traits
{
    static_assert(always_false_i<PIN>::value, "selected alternate function is not available on this pin!");
};

#define ALT_FUN_TRAIT(PIN, ALT_FUN, AFNO)           \
template<> struct alt_fun_traits<PIN, ALT_FUN>      \
{                                                   \
    static const alt_fun_t AF = AFNO;               \
}

#if defined(STM32F051)
#include "gpio/stm32f051.h"
#elif defined(STM32F103)
#include "gpio/stm32f103.h"
#elif defined(STM32F411)
#include "gpio/stm32f411.h"
#elif defined(STM32G070)
#include "gpio/stm32g070.h"
#elif defined(STM32G431)
#include "gpio/stm32g431.h"
#endif

template<gpio_pin_t PIN, alternate_function_t ALT>
class alternate_t
{
public:
    template<output_speed_t speed = low_speed>  // FIXME: should we not have output_type option here?
    static inline void setup()
    {
        peripheral_traits<typename port_traits<pin_port(PIN)>::gpio_t>::enable();
#if defined(STM32F103)
        volatile uint32_t& CR = pin::bit_pos < 8 ? pin::gpio().CRL : pin::gpio().CRH;
        constexpr uint8_t shift = (pin::bit_pos < 8 ? pin::bit_pos : (pin::bit_pos - 8)) << 2;
        constexpr uint32_t bits = 0x8 | (speed == low_speed ? 0x2 : 0x3); // | (output_type == open_drain ? 0x4 : 0);

        static_assert(alt_fun_traits<PIN, ALT>::AF == AF0, "invalid alternate function (remap not yet supported)");

        CR &= ~(0xf << shift);
        CR |= (bits << shift);
#else
        pin::gpio().MODER &= ~(0x3 << (pin::bit_pos*2));
        pin::gpio().MODER |= pin::alternate_mode << (pin::bit_pos*2);
        if (speed != low_speed)
            pin::gpio().OSPEEDR |= speed << (pin::bit_pos*2);
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
        peripheral_traits<typename port_traits<pin_port(PIN)>::gpio_t>::enable();
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

