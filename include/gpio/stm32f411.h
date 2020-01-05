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
    { EVENT_OUT
    , I2C1_SCL
    , I2C1_SDA
    , I2C1_SMBA
    , I2C2_SCL
    , I2C2_SDA
    , I2C2_SMBA
    , I2C3_SCL
    , I2C3_SDA
    , I2C3_SMBA
    , I2S1_CK
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
    , I2S4_SD
    , I2S4_WS
    , I2S5_CK
    , I2S5_SD
    , I2S5_WS
    , JTCK
    , JTDI
    , JTDO
    , JTMS
    , JTRST
    , MCO_1
    , MCO_2
    , RTC_50H
    , SDIO_CK
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
    , SWDIO
    , SWO
    , TIM10_CH1
    , TIM11_CH1
    , TIM1_BKIN
    , TIM1_CH1
    , TIM1_CH1N
    , TIM1_CH2
    , TIM1_CH2N
    , TIM1_CH3
    , TIM1_CH3N
    , TIM1_CH4
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
    , TIM5_CH1
    , TIM5_CH2
    , TIM5_CH3
    , TIM5_CH4
    , TIM9_CH1
    , TIM9_CH2
    , TRACECLK
    , TRACED0
    , TRACED1
    , TRACED2
    , TRACED3
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
    , USART6_CK
    , USART6_RX
    , USART6_TX
    , USB_FS_DM
    , USB_FS_DP
    , USB_FS_ID
    , USB_FS_SOF
    , USB_FS_VBUS
    };

template<gpio_pin_t PIN, alternate_function_t ALT>
struct alt_fun_traits
{
    static_assert(always_false_i<PIN>::value, "selected alternate function is not available on this pin!");
};

template<> struct alt_fun_traits<PA0, TIM2_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA0, TIM2_ETR> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA0, TIM5_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA0, USART2_CTS> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA0, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA1, TIM2_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA1, TIM5_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA1, SPI4_MOSI> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA1, I2S4_SD> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA1, USART2_RTS> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA1, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA2, TIM2_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA2, TIM5_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA2, TIM9_CH1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA2, I2S2_CKIN> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA2, USART2_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA2, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA3, TIM2_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA3, TIM5_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA3, TIM9_CH2> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA3, I2S2_MCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA3, USART2_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA3, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA4, SPI1_NSS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA4, I2S1_WS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA4, SPI3_NSS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA4, I2S3_WS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA4, USART2_CK> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA4, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA5, TIM2_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA5, TIM2_ETR> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA5, SPI1_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA5, I2S1_CK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA5, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA6, TIM1_BKIN> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA6, TIM3_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA6, SPI1_MISO> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA6, I2S2_MCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA6, SDIO_CMD> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PA6, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA7, TIM1_CH1N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA7, TIM3_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA7, SPI1_MOSI> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA7, I2S1_SD> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA7, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA8, MCO_1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA8, TIM1_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA8, I2C3_SCL> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA8, USART1_CK> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA8, USB_FS_SOF> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PA8, SDIO_D1> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PA8, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA9, TIM1_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA9, I2C3_SMBA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA9, USART1_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA9, USB_FS_VBUS> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PA9, SDIO_D2> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PA9, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA10, TIM1_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA10, SPI5_MOSI> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA10, I2S5_SD> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA10, USART1_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA10, USB_FS_ID> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PA10, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA11, TIM1_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA11, SPI4_MISO> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA11, USART1_CTS> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA11, USART6_TX> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PA11, USB_FS_DM> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PA11, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA12, TIM1_ETR> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA12, SPI5_MISO> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA12, USART1_RTS> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA12, USART6_RX> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PA12, USB_FS_DP> { static const alt_fun_t AF = AF10; };
template<> struct alt_fun_traits<PA12, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA13, JTMS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA13, SWDIO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA13, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA14, JTCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA14, SWCLK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA14, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PA15, JTDI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA15, TIM2_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA15, TIM2_ETR> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA15, SPI1_NSS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA15, I2S1_WS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA15, SPI3_NSS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA15, I2S3_WS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA15, USART1_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA15, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB0, TIM1_CH2N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB0, TIM3_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB0, SPI5_SCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB0, I2S5_CK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB0, EVENT_OUT> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PB1, TIM1_CH3N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB1, TIM3_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB1, SPI5_NSS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB1, I2S5_WS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB1, EVENT_OUT> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PB2, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB3, JTDO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB3, SWO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB3, TIM2_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB3, SPI1_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB3, I2S1_CK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB3, SPI3_SCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB3, I2S3_CK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB3, USART1_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB3, I2C2_SDA> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PB3, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB4, JTRST> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB4, TIM3_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB4, SPI1_MISO> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB4, SPI3_MISO> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB4, I2S3ext_SD> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB4, I2C3_SDA> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PB4, SDIO_D0> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PB4, EVENT_OUT> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PB5, TIM3_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB5, I2C1_SMBA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB5, SPI1_MOSI> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB5, I2S1_SD> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB5, SPI3_MOSI> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB5, I2S3_SD> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB5, SDIO_D3> { static const alt_fun_t AF = AF11; };
template<> struct alt_fun_traits<PB5, EVENT_OUT> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PB6, TIM4_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB6, I2C1_SCL> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB6, USART1_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB6, EVENT_OUT> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PB7, TIM4_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB7, I2C1_SDA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB7, USART1_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB7, SDIO_D0> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PB7, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB8, TIM4_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB8, TIM10_CH1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB8, I2C1_SCL> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB8, SPI5_MOSI> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB8, I2S5_SD> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB8, I2C3_SDA> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PB8, SDIO_D4> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PB8, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB9, TIM4_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB9, TIM11_CH1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB9, I2C1_SDA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB9, SPI2_NSS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB9, I2S2_WS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB9, I2C2_SDA> { static const alt_fun_t AF = AF9; };
template<> struct alt_fun_traits<PB9, SDIO_D5> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PB9, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB10, TIM2_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB10, I2C2_SCL> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB10, SPI2_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB10, I2S2_CK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB10, I2S3_MCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB10, SDIO_D7> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PB10, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB11, TIM2_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB11, I2C2_SDA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB11, I2S2_CKIN> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB11, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB12, TIM1_BKIN> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB12, I2C2_SMBA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB12, SPI2_NSS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB12, I2S2_WS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB12, SPI4_NSS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB12, I2S4_WS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB12, SPI3_SCK> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB12, I2S3_CK> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PB12, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB13, TIM1_CH1N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB13, SPI2_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB13, I2S2_CK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB13, SPI4_SCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB13, I2S4_CK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB13, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB14, TIM1_CH2N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB14, SPI2_MISO> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB14, I2S2ext_SD> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PB14, SDIO_D6> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PB14, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PB15, RTC_50H> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB15, TIM1_CH3N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB15, SPI2_MOSI> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB15, I2S2_SD> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PB15, SDIO_CK> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PB15, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC0, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC1, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC2, SPI2_MISO> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PC2, I2S2ext_SD> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC2, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC3, SPI2_MOSI> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PC3, I2S2_SD> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PC3, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC4, EVENT_OUT> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PC5, EVENT_OUT> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PC6, TIM3_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC6, I2S2_MCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PC6, USART6_TX> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PC6, SDIO_D6> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PC6, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC7, TIM3_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC7, SPI2_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PC7, I2S2_CK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PC7, I2S3_MCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC7, USART6_RX> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PC7, SDIO_D7> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PC7, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC8, TIM3_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC8, USART6_CK> { static const alt_fun_t AF = AF8; };
template<> struct alt_fun_traits<PC8, SDIO_D0> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PC8, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC9, MCO_2> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC9, TIM3_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PC9, I2C3_SDA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PC9, I2S2_CKIN> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PC9, SDIO_D1> { static const alt_fun_t AF = AF11; };
template<> struct alt_fun_traits<PC9, EVENT_OUT> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PC10, SPI3_SCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC10, I2S3_CK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC10, SDIO_D2> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PC10, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC11, I2S3ext_SD> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PC11, SPI3_MISO> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC11, SDIO_D3> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PC11, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PC12, SPI3_MOSI> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC12, I2S3_SD> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PC12, SDIO_CK> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PC12, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD0, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD1, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD2, TIM3_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD2, SDIO_CMD> { static const alt_fun_t AF = AF12; };
template<> struct alt_fun_traits<PD2, EVENT_OUT> { static const alt_fun_t AF = AF13; };
template<> struct alt_fun_traits<PD3, SPI2_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PD3, I2S2_CK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PD3, USART2_CTS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PD3, EVENT_OUT> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PD4, USART2_RTS> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD4, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD5, USART2_TX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD5, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD6, SPI3_MOSI> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PD6, I2S3_SD> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PD6, USART2_RX> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD6, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD7, USART2_CK> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PD7, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD8, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD9, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD10, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD11, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD12, TIM4_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD12, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD13, TIM4_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD13, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD14, TIM4_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD14, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PD15, TIM4_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PD15, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE0, TIM4_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PE0, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE1, EVENT_OUT> { static const alt_fun_t AF = AF14; };
template<> struct alt_fun_traits<PE2, TRACECLK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE2, SPI4_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE2, I2S4_CK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE2, SPI5_SCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE2, I2S5_CK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE2, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE3, TRACED0> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE3, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE4, TRACED1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE4, SPI4_NSS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE4, I2S4_WS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE4, SPI5_NSS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE4, I2S5_WS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE4, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE5, TRACED2> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE5, TIM9_CH1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PE5, SPI4_MISO> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE5, SPI5_MISO> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE5, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE6, TRACED3> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE6, TIM9_CH2> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PE6, SPI4_MOSI> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE6, I2S4_SD> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE6, SPI5_MOSI> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE6, I2S5_SD> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE6, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE7, TIM1_ETR> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE7, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE8, TIM1_CH1N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE8, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE9, TIM1_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE9, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE10, TIM1_CH2N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE10, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE11, TIM1_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE11, SPI4_NSS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE11, I2S4_WS> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE11, SPI5_NSS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE11, I2S5_WS> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE11, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE12, TIM1_CH3N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE12, SPI4_SCK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE12, I2S4_CK> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE12, SPI5_SCK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE12, I2S5_CK> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE12, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE13, TIM1_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE13, SPI4_MISO> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE13, SPI5_MISO> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE13, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE14, TIM1_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE14, SPI4_MOSI> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE14, I2S4_SD> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PE14, SPI5_MOSI> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE14, I2S5_SD> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PE14, EVENT_OUT> { static const alt_fun_t AF = AF15; };
template<> struct alt_fun_traits<PE15, TIM1_BKIN> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE15, EVENT_OUT> { static const alt_fun_t AF = AF15; };
