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
    , I2C1_SCL
    , I2C1_SDA
    , I2C1_SMBA
    , I2C2_SCL
    , I2C2_SDA
    , I2C2_SMBA
    , MCO
    , OSC32_IN
    , OSC32_OUT
    , SPI1_MISO
    , SPI1_MOSI
    , SPI1_NSS
    , SPI1_SCK
    , SPI2_MISO
    , SPI2_MOSI
    , SPI2_NSS
    , SPI2_SCK
    , TAMPER_RTC
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
    , TIM4_CH1
    , TIM4_CH2
    , TIM4_CH3
    , TIM4_CH4
    , TIM4_ETR
    , TRACECK
    , TRACED0
    , TRACED1
    , TRACED2
    , TRACED3
    , TRACESWO
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
    , USART3_CK
    , USART3_CTS
    , USART3_RTS
    , USART3_RX
    , USART3_TX
    , USBDM
    , USBDP
    , WKUP
    };

template<gpio_pin_t PIN, alternate_function_t ALT>
struct alt_fun_traits
{
    static_assert(always_false_i<PIN>::value, "selected alternate function is not available on this pin!");
};

template<> struct alt_fun_traits<PE2, TRACECK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE3, TRACED0> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE4, TRACED1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE5, TRACED2> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PE6, TRACED3> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC13, TAMPER_RTC> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC14, OSC32_IN> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC15, OSC32_OUT> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC0, ADC12_IN10> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC1, ADC12_IN11> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC2, ADC12_IN12> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC3, ADC12_IN13> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA0, WKUP> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA0, USART2_CTS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA0, ADC12_IN0> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA0, TIM2_CH1_ETR> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA1, USART2_RTS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA1, ADC12_IN1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA1, TIM2_CH2> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA2, USART2_TX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA2, ADC12_IN2> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA2, TIM2_CH3> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA3, USART2_RX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA3, ADC12_IN3> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA3, TIM2_CH4> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA4, SPI1_NSS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA4, USART2_CK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA4, ADC12_IN4> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA5, SPI1_SCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA5, ADC12_IN5> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA6, SPI1_MISO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA6, ADC12_IN6> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA6, TIM3_CH1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA6, TIM1_BKIN> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA7, SPI1_MOSI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA7, ADC12_IN7> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA7, TIM3_CH2> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA7, TIM1_CH1N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC4, ADC12_IN14> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PC5, ADC12_IN15> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB0, ADC12_IN8> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB0, TIM3_CH3> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB0, TIM1_CH2N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB1, ADC12_IN9> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB1, TIM3_CH4> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB1, TIM1_CH3N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE7, TIM1_ETR> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE8, TIM1_CH1N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE9, TIM1_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE10, TIM1_CH2N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE11, TIM1_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE12, TIM1_CH3N> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE13, TIM1_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE14, TIM1_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE15, TIM1_BKIN> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB10, I2C2_SCL> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB10, USART3_TX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB10, TIM2_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB11, I2C2_SDA> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB11, USART3_RX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB11, TIM2_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB12, SPI2_NSS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB12, I2C2_SMBA> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB12, USART3_CK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB12, TIM1_BKIN> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB13, SPI2_SCK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB13, USART3_CTS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB13, TIM1_CH1N> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB14, SPI2_MISO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB14, USART3_RTS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB14, TIM1_CH2N> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB15, SPI2_MOSI> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB15, TIM1_CH3N> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PD8, USART3_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD9, USART3_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD10, USART3_CK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD11, USART3_CTS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD12, TIM4_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD12, USART3_RTS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD13, TIM4_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD14, TIM4_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD15, TIM4_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC6, TIM3_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC7, TIM3_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC8, TIM3_CH3> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC9, TIM3_CH4> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA8, USART1_CK> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA8, TIM1_CH1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA8, MCO> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA9, USART1_TX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA9, TIM1_CH2> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA10, USART1_RX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA10, TIM1_CH3> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA11, USART1_CTS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA11, CAN_RX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA11, USBDM> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA11, TIM1_CH4> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA12, USART1_RTS> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA12, CAN_TX> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA12, USBDP> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA12, TIM1_ETR> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PA15, TIM2_CH1_ETR> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PA15, SPI1_NSS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC10, USART3_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC11, USART3_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PC12, USART3_CK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD0, CAN_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD1, CAN_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD2, TIM3_ETR> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PD3, USART2_CTS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD4, USART2_RTS> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD5, USART2_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD6, USART2_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PD7, USART2_CK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB3, TIM2_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB3, TRACESWO> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB3, SPI1_SCK> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB4, TIM3_CH1> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB4, SPI1_MISO> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB5, I2C1_SMBA> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB5, TIM3_CH2> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB5, SPI1_MOSI> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB6, I2C1_SCL> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB6, TIM4_CH1> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB6, USART1_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB7, I2C1_SDA> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB7, TIM4_CH2> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB7, USART1_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB8, TIM4_CH3> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB8, I2C1_SCL> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB8, CAN_RX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB9, TIM4_CH4> { static const alt_fun_t AF = AF0; };
template<> struct alt_fun_traits<PB9, I2C1_SDA> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PB9, CAN_TX> { static const alt_fun_t AF = AF1; };
template<> struct alt_fun_traits<PE0, TIM4_ETR> { static const alt_fun_t AF = AF0; };
