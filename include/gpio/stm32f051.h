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
struct alt_fun_traits
{
    static_assert(always_false_i<PIN>::value, "selected alternate function is not available on this pin!");
};

template<> struct alt_fun_traits<PA0, USART2_CTS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA0, TIM2_CH1_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA0, TSC_G1_IO1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA0, USART4_TX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA0, COMP1_OUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA1, EVENTOUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA1, USART2_RTS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA1, TIM2_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA1, TSC_G1_IO2> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA1, USART4_RX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA1, TIM15_CH1N> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA2, TIM15_CH1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA2, USART2_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA2, TIM2_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA2, TSC_G1_IO3> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA2, COMP2_OUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA3, TIM15_CH2> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA3, USART2_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA3, TIM2_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA3, TSC_G1_IO4> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA4, SPI1_NSS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA4, I2S1_WS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA4, USART2_CK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA4, TSC_G2_IO1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA4, TIM14_CH1> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA4, USART6_TX> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA5, SPI1_SCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA5, I2S1_CK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA5, CEC> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA5, TIM2_CH1_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA5, TSC_G2_IO2> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA5, USART6_RX> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA6, SPI1_MISO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA6, I2S1_MCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA6, TIM3_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA6, TIM1_BKIN> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA6, TSC_G2_IO3> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA6, USART3_CTS> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA6, TIM16_CH1> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA6, EVENTOUT> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA6, COMP1_OUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA7, SPI1_MOSI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA7, I2S1_SD> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA7, TIM3_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA7, TIM1_CH1N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA7, TSC_G2_IO4> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA7, TIM14_CH1> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA7, TIM17_CH1> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA7, EVENTOUT> { static const alt_fun_t AF = AF6; };
template<> struct alt_fun_traits<PA7, COMP2_OUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA8, MCO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA8, USART1_CK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA8, TIM1_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA8, EVENTOUT> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA8, CRS_SYNC> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA9, TIM15_BKIN> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA9, USART1_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA9, TIM1_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA9, TSC_G4_IO1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA9, I2C1_SCL> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA9, MCO> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA10, TIM17_BKIN> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA10, USART1_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA10, TIM1_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA10, TSC_G4_IO2> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA10, I2C1_SDA> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA11, EVENTOUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA11, USART1_CTS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA11, TIM1_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA11, TSC_G4_IO3> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA11, CAN_RX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA11, I2C2_SCL> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA11, COMP1_OUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA12, EVENTOUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA12, USART1_RTS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA12, TIM1_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA12, TSC_G4_IO4> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA12, CAN_TX> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PA12, I2C2_SDA> { static const alt_fun_t AF = AF5; };
template<> struct alt_fun_traits<PA12, COMP2_OUT> { static const alt_fun_t AF = AF7; };
template<> struct alt_fun_traits<PA13, SWDIO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA13, IR_OUT> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA14, SWCLK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA14, USART2_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA15, SPI1_NSS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA15, I2S1_WS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA15, USART2_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA15, TIM2_CH1_ETR> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PA15, EVENTOUT> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PA15, USART4_RTS> { static const alt_fun_t AF = AF4; };
template<> struct alt_fun_traits<PB0, EVENTOUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB0, TIM3_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB0, TIM1_CH2N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB0, TSC_G3_IO2> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB1, TIM14_CH1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB1, TIM3_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB1, TIM1_CH3N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB1, TSC_G3_IO3> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB2, TSC_G3_IO4> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB3, SPI1_SCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB3, I2S1_CK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB3, EVENTOUT> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB3, TIM2_CH2> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB3, TSC_G5_IO1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB4, SPI1_MISO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB4, I2S1_MCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB4, TIM3_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB4, EVENTOUT> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB4, TSC_G5_IO2> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB5, SPI1_MOSI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB5, I2S1_SD> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB5, TIM3_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB5, TIM16_BKIN> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB5, I2C1_SMBA> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB6, USART1_TX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB6, I2C1_SCL> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB6, TIM16_CH1N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB6, TSC_G5_IO3> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB7, USART1_RX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB7, I2C1_SDA> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB7, TIM17_CH1N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB7, TSC_G5_IO4> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB8, CEC> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB8, I2C1_SCL> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB8, TIM16_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB8, TSC_SYNC> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB9, IR_OUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB9, I2C1_SDA> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB9, TIM17_CH1> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB9, EVENTOUT> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB10, CEC> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB10, I2C2_SCL> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB10, TIM2_CH3> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB10, TSC_SYNC> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB11, EVENTOUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB11, I2C2_SDA> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB11, TIM2_CH4> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB11, TSC_G6_IO1> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB12, SPI2_NSS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB12, EVENTOUT> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB12, TIM1_BKIN> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB12, TSC_G6_IO2> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB13, SPI2_SCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB13, TIM1_CH1N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB13, TSC_G6_IO3> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB14, SPI2_MISO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB14, TIM15_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB14, TIM1_CH2N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB14, TSC_G6_IO4> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PB15, SPI2_MOSI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB15, TIM15_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB15, TIM1_CH3N> { static const alt_fun_t AF = AF2; };
template<> struct alt_fun_traits<PB15, TIM15_CH1N> { static const alt_fun_t AF = AF3; };
template<> struct alt_fun_traits<PC0, EVENTOUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC1, EVENTOUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC2, EVENTOUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC3, EVENTOUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC4, EVENTOUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC5, TSC_G3_IO1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC6, TIM3_CH1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC7, TIM3_CH2> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC8, TIM3_CH3> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC9, TIM3_CH4> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PD2, TIM3_ETR> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PF6, I2C2_SCL> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PF7, I2C2_SDA> { static const alt_fun_t AF = AF0; };
