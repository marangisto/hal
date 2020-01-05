enum alt_fun_t
    { AF0
    , AF1
    , AF2
    , AF3
    , AF4
    , AF5
    , AF6
    , AF7
    , AF8
    , AF9
    , AF10
    , AF11
    , AF12
    , AF13
    , AF14
    , AF15
    };

enum alternate_function_t
    { EVENTOUT
    , I2C1_SCL
    , I2C1_SDA
    , I2C1_SMBA
    , I2C2_SCL
    , I2C2_SDA
    , I2S1_CK
    , I2S1_MCK
    , I2S1_SD
    , I2S1_WS
    , I2S_CKIN
    , IR_OUT
    , MCO
    , OSC32_EN
    , OSC_EN
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
    , TIM1_BKIN2
    , TIM1_CH1
    , TIM1_CH1N
    , TIM1_CH2
    , TIM1_CH2N
    , TIM1_CH3
    , TIM1_CH3N
    , TIM1_CH4
    , TIM1_ETR
    , TIM3_CH1
    , TIM3_CH2
    , TIM3_CH3
    , TIM3_CH4
    , TIM3_ETR
    , USART1_CTS
    , USART1_RTS_DE_CK
    , USART1_RX
    , USART1_TX
    , USART2_CTS
    , USART2_RTS_DE_CK
    , USART2_RX
    , USART2_TX
    , USART3_CTS
    , USART3_RTS_DE_CK
    , USART3_RX
    , USART3_TX
    , USART4_CTS
    , USART4_RTS_DE_CK
    , USART4_RX
    , USART4_TX
    };

template<gpio_pin_t PIN, alternate_function_t ALT>
struct alt_fun_traits
{
    static_assert(always_false_i<PIN>::value, "selected alternate function is not available on this pin!");
};

template<> struct alt_fun_traits<PA0, SPI2_SCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA0, USART2_CTS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA0, USART4_TX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA1, SPI1_SCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA1, I2S1_CK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA1, USART2_RTS_DE_CK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA1, USART4_RX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA1, TIM15_CH1N> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA1, I2C1_SMBA> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA1, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA2, SPI1_MOSI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA2, I2S1_SD> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA2, USART2_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA2, TIM15_CH1> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA3, SPI2_MISO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA3, USART2_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA3, TIM15_CH2> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA3, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA4, SPI1_NSS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA4, I2S1_WS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA4, SPI2_MOSI> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA4, TIM14_CH1> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA4, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA5, SPI1_SCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA5, I2S1_CK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA5, USART3_TX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA5, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA6, SPI1_MISO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA6, I2S1_MCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA6, TIM3_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA6, TIM1_BKIN> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA6, USART3_CTS> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA6, TIM16_CH1> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA7, SPI1_MOSI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA7, I2S1_SD> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA7, TIM3_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA7, TIM1_CH1N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA7, TIM14_CH1> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA7, TIM17_CH1> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA8, MCO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA8, SPI2_NSS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA8, TIM1_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA8, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA9, MCO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA9, USART1_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA9, TIM1_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA9, SPI2_MISO> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA9, TIM15_BKIN> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA9, I2C1_SCL> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA9, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA10, SPI2_MOSI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA10, USART1_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA10, TIM1_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA10, TIM17_BKIN> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA10, I2C1_SDA> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA10, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA11, SPI1_MISO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA11, I2S1_MCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA11, USART1_CTS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA11, TIM1_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA11, TIM1_BKIN2> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA11, I2C2_SCL> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA12, SPI1_MOSI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA12, I2S1_SD> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA12, USART1_RTS_DE_CK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA12, TIM1_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA12, I2S_CKIN> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA12, I2C2_SDA> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA13, SWDIO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA13, IR_OUT> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA13, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA14, SWCLK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA14, USART2_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA14, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA15, SPI1_NSS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA15, I2S1_WS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA15, USART2_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA15, USART4_RTS_DE_CK> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA15, USART3_RTS_DE_CK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA15, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB0, SPI1_NSS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB0, I2S1_WS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB0, TIM3_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB0, TIM1_CH2N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB0, USART3_RX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB1, TIM14_CH1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB1, TIM3_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB1, TIM1_CH3N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB1, USART3_RTS_DE_CK> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB1, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB2, SPI2_MISO> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB2, USART3_TX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB2, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB3, SPI1_SCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB3, I2S1_CK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB3, TIM1_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB3, USART1_RTS_DE_CK> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB3, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB4, SPI1_MISO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB4, I2S1_MCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB4, TIM3_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB4, USART1_CTS> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB4, TIM17_BKIN> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB4, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB5, SPI1_MOSI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB5, I2S1_SD> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB5, TIM3_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB5, TIM16_BKIN> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB5, I2C1_SMBA> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB6, USART1_TX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB6, TIM1_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB6, TIM16_CH1N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB6, SPI2_MISO> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB6, I2C1_SCL> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB6, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB7, USART1_RX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB7, SPI2_MOSI> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB7, TIM17_CH1N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB7, USART4_CTS> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB7, I2C1_SDA> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB7, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB8, SPI2_SCK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB8, TIM16_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB8, USART3_TX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB8, TIM15_BKIN> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB8, I2C1_SCL> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB8, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB9, IR_OUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB9, TIM17_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB9, USART3_RX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB9, SPI2_NSS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB9, I2C1_SDA> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB9, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB10, USART3_TX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB10, SPI2_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB10, I2C2_SCL> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB11, SPI2_MOSI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB11, USART3_RX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB11, I2C2_SDA> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB12, SPI2_NSS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB12, TIM1_BKIN> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB12, TIM15_BKIN> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB12, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB13, SPI2_SCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB13, TIM1_CH1N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB13, USART3_CTS> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB13, TIM15_CH1N> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB13, I2C2_SCL> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB13, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB14, SPI2_MISO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB14, TIM1_CH2N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB14, USART3_RTS_DE_CK> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB14, TIM15_CH1> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB14, I2C2_SDA> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB14, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB15, SPI2_MOSI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB15, TIM1_CH3N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB15, TIM15_CH1N> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB15, TIM15_CH2> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB15, EVENTOUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PC1, TIM15_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC2, SPI2_MISO> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC2, TIM15_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC3, SPI2_MOSI> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC4, USART3_TX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC4, USART1_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC5, USART3_RX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC5, USART1_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC6, TIM3_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC7, TIM3_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC8, TIM3_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC8, TIM1_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC9, I2S_CKIN> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC9, TIM3_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC9, TIM1_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC10, USART3_TX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC10, USART4_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC10, TIM1_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC11, USART3_RX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC11, USART4_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC11, TIM1_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC12, TIM14_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC13, TIM1_BKIN> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC14, TIM1_BKIN2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC15, OSC32_EN> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC15, OSC_EN> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC15, TIM15_BKIN> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD0, EVENTOUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PD0, SPI2_NSS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD0, TIM16_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD1, EVENTOUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PD1, SPI2_SCK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD1, TIM17_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD2, USART3_RTS_DE_CK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PD2, TIM3_ETR> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD2, TIM1_CH1N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD3, USART2_CTS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PD3, SPI2_MISO> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD3, TIM1_CH2N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD4, USART2_RTS_DE_CK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PD4, SPI2_MOSI> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD4, TIM1_CH3N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD5, USART2_TX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PD5, SPI1_MISO> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD5, I2S1_MCK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD5, TIM1_BKIN> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD6, USART2_RX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PD6, SPI1_MOSI> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD6, I2S1_SD> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD8, USART3_TX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PD8, SPI1_SCK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD8, I2S1_CK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD9, USART3_RX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PD9, SPI1_NSS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD9, I2S1_WS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD9, TIM1_BKIN2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PF0, TIM14_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PF1, OSC_EN> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PF1, TIM15_CH1N> { static const alt_fun_t AF = AF2; };
