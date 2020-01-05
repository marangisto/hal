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
    { COMP1_OUT
    , COMP2_OUT
    , COMP3_OUT
    , COMP4_OUT
    , EVENT_OUT
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
    , I2S2_CK
    , I2S2_MCK
    , I2S2_SD
    , I2S2_WS
    , I2S3_CK
    , I2S3_MCK
    , I2S3_SD
    , I2S3_WS
    , I2SCKIN
    , IR_OUT
    , JTDI
    , JTDO_TRACESWO
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
    , SWCLK_JTCK
    , SWDIO_JTMS
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
    , TIM1_CH4N
    , TIM1_ETR
    , TIM2_CH1
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
    , TRACECK
    , TRACED0
    , TRACED1
    , TRACED2
    , TRACED3
    , UART4_CTS
    , UART4_RTS_DE
    , UART4_RX
    , UART4_TX
    , UCPD1_FRSTX
    , USART1_CK
    , USART1_CTS
    , USART1_RTS_DE
    , USART1_RX
    , USART1_TX
    , USART2_CK
    , USART2_CTS
    , USART2_RTS_DE
    , USART2_RX
    , USART2_TX
    , USART3_CK
    , USART3_CTS
    , USART3_RTS_DE
    , USART3_RX
    , USART3_TX
    , USB_CRS_SYNC
    };

template<gpio_pin_t PIN, alternate_function_t ALT>
struct alt_fun_traits
{
    static_assert(always_false_i<PIN>::value, "selected alternate function is not available on this pin!");
};

template<> struct alt_fun_traits<PA0, TIM2_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA0, USART2_CTS> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA0, COMP1_OUT> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PA0, TIM8_BKIN> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PA0, TIM8_ETR> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PA0, TIM2_ETR> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PA0, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA1, RTC_REFIN> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA1, TIM2_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA1, USART2_RTS_DE> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA1, TIM15_CH1N> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PA1, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA2, TIM2_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA2, USART2_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA2, COMP2_OUT> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PA2, TIM15_CH1> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PA2, LPUART1_TX> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PA2, UCPD1_FRSTX> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PA2, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA3, TIM2_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA3, SAI1_CK1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA3, USART2_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA3, TIM15_CH2> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PA3, LPUART1_RX> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PA3, SAI1_MCLK_A> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PA3, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA4, TIM3_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA4, SPI1_NSS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA4, SPI3_NSS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA4, I2S3_WS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA4, USART2_CK> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA4, SAI1_FS_B> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PA4, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA5, TIM2_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA5, TIM2_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA5, SPI1_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA5, UCPD1_FRSTX> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PA5, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA6, TIM16_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA6, TIM3_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA6, TIM8_BKIN> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA6, SPI1_MISO> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA6, TIM1_BKIN> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA6, COMP1_OUT> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PA6, LPUART1_CTS> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PA6, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA7, TIM17_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA7, TIM3_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA7, TIM8_CH1N> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA7, SPI1_MOSI> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA7, TIM1_CH1N> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA7, COMP2_OUT> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PA7, UCPD1_FRSTX> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PA7, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA8, MCO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA8, I2C3_SCL> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA8, I2C2_SDA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA8, I2S2_MCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA8, TIM1_CH1> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA8, USART1_CK> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA8, TIM4_ETR> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PA8, SAI1_CK2> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PA8, SAI1_SCK_A> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PA8, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA9, I2C3_SMBA> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA9, I2C2_SCL> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA9, I2S3_MCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA9, TIM1_CH2> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA9, USART1_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA9, TIM15_BKIN> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PA9, TIM2_CH3> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PA9, SAI1_FS_A> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PA9, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA10, TIM17_BKIN> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA10, USB_CRS_SYNC> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA10, I2C2_SMBA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA10, SPI2_MISO> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA10, TIM1_CH3> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA10, USART1_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA10, TIM2_CH4> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PA10, TIM8_BKIN> { static const alt_fun_t AF = AF11; };
template<> struct alt_fun_traits<PA10, SAI1_D1> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PA10, SAI1_SD_A> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PA10, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA11, SPI2_MOSI> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA11, I2S2_SD> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA11, TIM1_CH1N> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA11, USART1_CTS> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA11, COMP1_OUT> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PA11, FDCAN1_RX> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PA11, TIM4_CH1> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PA11, TIM1_CH4> { static const alt_fun_t AF = AF11; };
template<> struct alt_fun_traits<PA11, TIM1_BKIN2> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PA11, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA12, TIM16_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA12, I2SCKIN> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA12, TIM1_CH2N> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA12, USART1_RTS_DE> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA12, COMP2_OUT> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PA12, FDCAN1_TX> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PA12, TIM4_CH2> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PA12, TIM1_ETR> { static const alt_fun_t AF = AF11; };
template<> struct alt_fun_traits<PA12, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA13, SWDIO_JTMS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA13, TIM16_CH1N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA13, I2C1_SCL> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA13, IR_OUT> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA13, USART3_CTS> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA13, TIM4_CH3> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PA13, SAI1_SD_B> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PA13, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA14, SWCLK_JTCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA14, LPTIM1_OUT> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA14, I2C1_SDA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA14, TIM8_CH2> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA14, TIM1_BKIN> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA14, USART2_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA14, SAI1_FS_B> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PA14, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA15, JTDI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA15, TIM2_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA15, TIM8_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA15, I2C1_SCL> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA15, SPI1_NSS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA15, SPI3_NSS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA15, I2S3_WS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA15, USART2_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA15, UART4_RTS_DE> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PA15, TIM1_BKIN> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PA15, TIM2_ETR> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PA15, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB0, TIM3_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB0, TIM8_CH2N> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB0, TIM1_CH2N> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB0, UCPD1_FRSTX> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PB0, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB1, TIM3_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB1, TIM8_CH3N> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB1, TIM1_CH3N> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB1, COMP4_OUT> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PB1, LPUART1_RTS_DE> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PB1, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB2, RTC_OUT2> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB2, LPTIM1_OUT> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB2, I2C3_SMBA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB2, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB3, JTDO_TRACESWO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB3, TIM2_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB3, TIM4_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB3, USB_CRS_SYNC> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB3, TIM8_CH1N> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB3, SPI1_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB3, SPI3_SCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB3, I2S3_CK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB3, USART2_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB3, TIM3_ETR> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PB3, SAI1_SCK_B> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PB3, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB4, JTRST> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB4, TIM16_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB4, TIM3_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB4, TIM8_CH2N> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB4, SPI1_MISO> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB4, SPI3_MISO> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB4, USART2_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB4, TIM17_BKIN> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PB4, SAI1_MCLK_B> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PB4, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB5, TIM16_BKIN> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB5, TIM3_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB5, TIM8_CH3N> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB5, I2C1_SMBA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB5, SPI1_MOSI> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB5, SPI3_MOSI> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB5, I2S3_SD> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB5, USART2_CK> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB5, I2C3_SDA> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PB5, TIM17_CH1> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PB5, LPTIM1_IN1> { static const alt_fun_t AF = AF11; };
template<> struct alt_fun_traits<PB5, SAI1_SD_B> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PB5, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB6, TIM16_CH1N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB6, TIM4_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB6, TIM8_CH1> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB6, TIM8_ETR> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB6, USART1_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB6, COMP4_OUT> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PB6, TIM8_BKIN2> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PB6, LPTIM1_ETR> { static const alt_fun_t AF = AF11; };
template<> struct alt_fun_traits<PB6, SAI1_FS_B> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PB6, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB7, TIM17_CH1N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB7, TIM4_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB7, I2C1_SDA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB7, TIM8_BKIN> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB7, USART1_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB7, COMP3_OUT> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PB7, TIM3_CH4> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PB7, LPTIM1_IN2> { static const alt_fun_t AF = AF11; };
template<> struct alt_fun_traits<PB7, UART4_CTS> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PB7, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB8, TIM16_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB8, TIM4_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB8, SAI1_CK1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB8, I2C1_SCL> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB8, USART3_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB8, COMP1_OUT> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PB8, FDCAN1_RX> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PB8, TIM8_CH2> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PB8, TIM1_BKIN> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PB8, SAI1_MCLK_A> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PB8, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB9, TIM17_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB9, TIM4_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB9, SAI1_D2> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB9, I2C1_SDA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB9, IR_OUT> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB9, USART3_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB9, COMP2_OUT> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PB9, FDCAN1_TX> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PB9, TIM8_CH3> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PB9, TIM1_CH3N> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PB9, SAI1_FS_A> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PB9, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB10, TIM2_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB10, USART3_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB10, LPUART1_RX> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PB10, TIM1_BKIN> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PB10, SAI1_SCK_A> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PB10, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB11, TIM2_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB11, USART3_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB11, LPUART1_TX> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PB11, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB12, I2C2_SMBA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB12, SPI2_NSS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB12, I2S2_WS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB12, TIM1_BKIN> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB12, USART3_CK> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB12, LPUART1_RTS_DE> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PB12, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB13, SPI2_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB13, I2S2_CK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB13, TIM1_CH1N> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB13, USART3_CTS> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB13, LPUART1_CTS> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PB13, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB14, TIM15_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB14, SPI2_MISO> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB14, TIM1_CH2N> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB14, USART3_RTS_DE> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB14, COMP4_OUT> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PB14, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB15, RTC_REFIN> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB15, TIM15_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB15, TIM15_CH1N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB15, COMP3_OUT> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB15, TIM1_CH3N> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB15, SPI2_MOSI> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB15, I2S2_SD> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB15, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC0, LPTIM1_IN1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC0, TIM1_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC0, LPUART1_RX> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PC0, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC1, LPTIM1_OUT> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC1, TIM1_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC1, LPUART1_TX> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PC1, SAI1_SD_A> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PC1, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC2, LPTIM1_IN2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC2, TIM1_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC2, COMP3_OUT> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PC2, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC3, LPTIM1_ETR> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC3, TIM1_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC3, SAI1_D1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PC3, TIM1_BKIN2> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC3, SAI1_SD_A> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PC3, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC4, TIM1_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC4, I2C2_SCL> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PC4, USART1_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PC4, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC5, TIM15_BKIN> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC5, SAI1_D3> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PC5, TIM1_CH4N> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC5, USART1_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PC5, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC6, TIM3_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC6, TIM8_CH1> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PC6, I2S2_MCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC6, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC7, TIM3_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC7, TIM8_CH2> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PC7, I2S3_MCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC7, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC8, TIM3_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC8, TIM8_CH3> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PC8, I2C3_SCL> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PC8, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC9, TIM3_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC9, TIM8_CH4> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PC9, I2SCKIN> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PC9, TIM8_BKIN2> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC9, I2C3_SDA> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PC9, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC10, TIM8_CH1N> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PC10, UART4_TX> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PC10, SPI3_SCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC10, I2S3_CK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC10, USART3_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PC10, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC11, TIM8_CH2N> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PC11, UART4_RX> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PC11, SPI3_MISO> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC11, USART3_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PC11, I2C3_SDA> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PC11, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC12, TIM8_CH3N> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PC12, SPI3_MOSI> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC12, I2S3_SD> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC12, USART3_CK> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PC12, UCPD1_FRSTX> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PC12, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC13, TIM1_BKIN> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC13, TIM1_CH1N> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PC13, TIM8_CH4N> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC13, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC14, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC15, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD0, TIM8_CH4N> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PD0, FDCAN1_RX> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PD0, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD1, TIM8_CH4> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PD1, TIM8_BKIN2> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PD1, FDCAN1_TX> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PD1, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD2, TIM3_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD2, TIM8_BKIN> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PD2, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD3, TIM2_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD3, TIM2_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD3, USART2_CTS> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD3, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD4, TIM2_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD4, USART2_RTS_DE> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD4, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD5, USART2_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD5, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD6, TIM2_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD6, SAI1_D1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PD6, USART2_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD6, SAI1_SD_A> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PD6, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD7, TIM2_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD7, USART2_CK> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD7, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD8, USART3_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD8, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD9, USART3_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD9, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD10, USART3_CK> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD10, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD11, USART3_CTS> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD11, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD12, TIM4_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD12, USART3_RTS_DE> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD12, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD13, TIM4_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD13, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD14, TIM4_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD14, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD15, TIM4_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD15, SPI2_NSS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PD15, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE0, TIM4_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE0, TIM16_CH1> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PE0, USART1_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PE0, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE1, TIM17_CH1> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PE1, USART1_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PE1, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE2, TRACECK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE2, TIM3_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE2, SAI1_CK1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PE2, SAI1_MCLK_A> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PE2, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE3, TRACED0> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE3, TIM3_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE3, SAI1_SD_B> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PE3, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE4, TRACED1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE4, TIM3_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE4, SAI1_D2> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PE4, SAI1_FS_A> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PE4, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE5, TRACED2> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE5, TIM3_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE5, SAI1_CK2> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PE5, SAI1_SCK_A> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PE5, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE6, TRACED3> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE6, SAI1_D1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PE6, SAI1_SD_A> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PE6, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE7, TIM1_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE7, SAI1_SD_B> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PE7, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE8, TIM1_CH1N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE8, SAI1_SCK_B> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PE8, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE9, TIM1_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE9, SAI1_FS_B> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PE9, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE10, TIM1_CH2N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE10, SAI1_MCLK_B> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PE10, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE11, TIM1_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE11, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE12, TIM1_CH3N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE12, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE13, TIM1_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE13, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE14, TIM1_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE14, TIM1_BKIN2> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE14, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE15, TIM1_BKIN> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE15, TIM1_CH4N> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE15, USART3_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PE15, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PF0, I2C2_SDA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PF0, SPI2_NSS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PF0, I2S2_WS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PF0, TIM1_CH3N> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PF0, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PF1, SPI2_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PF1, I2S2_CK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PF1, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PF2, I2C2_SMBA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PF2, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PF9, TIM15_CH1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PF9, SPI2_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PF9, SAI1_FS_B> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PF9, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PF10, TIM15_CH2> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PF10, SPI2_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PF10, SAI1_D3> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PF10, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PG10, MCO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PG10, EVENT_OUT> { static const alt_fun_t AF = AF15; };
