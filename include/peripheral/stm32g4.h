#pragma once

namespace hal
{

namespace internal
{

using namespace device;

template<bool> struct is_in_range;

template<int POS, typename = is_in_range<true> >
struct nvic {};

template<int POS>
struct nvic<POS, is_in_range<(0 <= POS && POS < 32)> >
{
    static void enable() { NVIC.ISER0 |= 1 << POS; }
};

template<int POS>
struct nvic<POS, is_in_range<(32 <= POS && POS < 64)> >
{
    static void enable() { NVIC.ISER1 |= 1 << (POS - 32); }
};

template<int POS>
struct nvic<POS, is_in_range<(64 <= POS && POS < 96)> >
{
    static void enable() { NVIC.ISER2 |= 1 << (POS - 64); }
};

template<int POS>
struct nvic<POS, is_in_range<(96 <= POS && POS < 128)> >
{
    static void enable() { NVIC.ISER3 |= 1 << (POS - 96); }
};

enum pos_t
    { WWDG = 0
    , PVD_PVM = 1
    , RTC_TAMP_CSS_LSE = 2
    , RTC_WKUP = 3
    , FLASH = 4
    , RCC_ = 5
    , EXTI0 = 6
    , EXTI1 = 7
    , EXTI2 = 8
    , EXTI3 = 9
    , EXTI4 = 10
    , DMA_CH1 = 11
    , DMA_CH2 = 12
    , DMA_CH3 = 13
    , DMA_CH4 = 14
    , DMA_CH5 = 15
    , DMA_CH6 = 16
    , DMA_CH7 = 17
    , ADC1_2 = 18
    , USP_HP = 19
    , USP_LP = 20
    , FDCAN1_INTR1_IT = 21
    , FDCAN1_INTR0_IT = 22
    , EXTI9_5 = 23
    , TIM1_BRK_TIM15 = 24
    , TIM1_UP_TIM16 = 25
    , TIM1_TRG_COM_TIM17 = 26
    , TIM1_CC = 27
    , TIM2 = 28
    , TIM3 = 29
    , TIM4 = 30
    , I2C1_EV = 31
    , I2C1_ER = 32
    , I2C2_EV = 33
    , I2C2_ER = 34
    , SPI1 = 35
    , SPI2 = 36
    , USART1 = 37
    , USART2 = 38
    , USART3 = 39
    , EXTI15_10 = 40
    , RTC_ALARM = 41
    , USB_WAKE_UP = 42
    , TIM8_BRK_TERR_IERR = 43
    , TIM8_TRG_COM_DIR_IDX = 44
    , TIM8_UP = 45
    , TIM8_CC = 46
    , ADC3 = 47
    , FMC = 48
    , LPTIM1 = 49
    , TIM5 = 50
    , SPI3 = 51
    , UART4 = 52
    , UART5 = 53
    , TIM6_DAC1_3_UNDER = 54
    , TIM7_DAC2_4_UNDER = 55
    , DMA2_CH1 = 56
    , DMA2_CH2 = 57
    , DMA2_CH3 = 58
    , DMA2_CH4 = 59
    , DMA2_CH5 = 60
    , ADC4 = 61
    , ADC5 = 62
    , UCPD1 = 63
    , COMP1_2_3 = 64
    , COMP4_5_6 = 65
    , COMP7 = 66
    , HRTIM_MASTER_IRQN = 67
    , HRTIM_TIMA_IRQN = 68
    , HRTIM_TIMB_IRQN = 69
    , HRTIM_TIMC_IRQN = 70
    , HRTIM_TIMD_IRQN = 71
    , HRTIM_TIME_IRQN = 72
    , HRTIM_TIM_FLT_IRQN = 73
    , HRTIM_TIMF_IRQN = 74
    , CRS = 75
    , SAI = 76
    , TIM20_BRK_TERR_IERR = 77
    , TIM20_UP = 78
    , TIM20_TRG_COM_DIR_IDX = 79
    , TIM20_CC = 80
    , FPU = 81
    , I2C4_EV = 82
    , I2C4_ER = 83
    , SPI4 = 84
    , AES = 85
    , FDCAN2_INTR0 = 86
    , FDCAN2_INTR1 = 87
    , FDCAN3_INTR0 = 88
    , FDCAN3_INTR1 = 89
    , RNG = 90
    , LPUART = 91
    , I2C3_EV = 92
    , I2C3_ER = 93
    , DMAMUX_OVR = 94
    , QUADSPI = 95
    , DMA1_CH8 = 96
    , DMA2_CH6 = 97
    , DMA2_CH7 = 98
    , DMA2_CH8 = 99
    , CORDIC = 100
    , FMAC = 101
    };

template<typename PERIPHERAL> struct peripheral_traits {};

template<> struct peripheral_traits<gpioa_t>
{
    static void rcc_enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIOAEN ; }
};

template<> struct peripheral_traits<gpiob_t>
{
    static void rcc_enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIOBEN ; }
};

template<> struct peripheral_traits<gpioc_t>
{
    static void rcc_enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIOCEN ; }
};

template<> struct peripheral_traits<gpiod_t>
{
    static void rcc_enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIODEN ; }
};

template<> struct peripheral_traits<gpioe_t>
{
    static void rcc_enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIOEEN ; }
};

template<> struct peripheral_traits<gpiof_t>
{
    static void rcc_enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIOFEN ; }
};

template<> struct peripheral_traits<gpiog_t>
{
    static void rcc_enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIOGEN ; }
};

template<> struct peripheral_traits<tim1_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM1EN; }
    static void nvic_enable() { nvic<TIM1_UP_TIM16>::enable(); }
};

template<> struct peripheral_traits<tim2_t>
{
    static void rcc_enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_TIM2EN; }
    static void nvic_enable() { nvic<TIM2>::enable(); }
};

template<> struct peripheral_traits<tim3_t>
{
    static void rcc_enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_TIM3EN; }
    static void nvic_enable() { nvic<TIM3>::enable(); }
};

template<> struct peripheral_traits<tim4_t>
{
    static void rcc_enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_TIM4EN; }
    static void nvic_enable() { nvic<TIM4>::enable(); }
};

template<> struct peripheral_traits<tim6_t>
{
    static void rcc_enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_TIM6EN; }
    static void nvic_enable() { nvic<TIM6_DAC1_3_UNDER>::enable(); }
};

template<> struct peripheral_traits<tim7_t>
{
    static void rcc_enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_TIM7EN; }
    static void nvic_enable() { nvic<TIM7_DAC2_4_UNDER>::enable(); }
};

template<> struct peripheral_traits<tim8_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM8EN; }
    static void nvic_enable() { nvic<TIM8_UP>::enable(); }
};

template<> struct peripheral_traits<tim15_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM15EN; }
    static void nvic_enable() { nvic<TIM1_BRK_TIM15>::enable(); }
};

template<> struct peripheral_traits<tim16_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM16EN; }
    static void nvic_enable() { nvic<TIM1_UP_TIM16>::enable(); }
};

template<> struct peripheral_traits<tim17_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM17EN; }
    static void nvic_enable() { nvic<TIM1_TRG_COM_TIM17>::enable(); }
};

template<> struct peripheral_traits<i2c1_t>
{
    static void nvic_enable() { nvic<I2C1_EV>::enable(); }
};

template<> struct peripheral_traits<i2c2_t>
{
    static void nvic_enable() { nvic<I2C2_EV>::enable(); }
};

template<> struct peripheral_traits<spi1_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_SPI1EN; }
    static void nvic_enable() { nvic<SPI1>::enable(); }
};

template<> struct peripheral_traits<spi2_t>
{
    static void rcc_enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_SPI2EN; }
    static void nvic_enable() { nvic<SPI2>::enable(); }
};

template<> struct peripheral_traits<usart1_t>
{
    static void nvic_enable() { nvic<USART1>::enable(); }
};

template<> struct peripheral_traits<usart2_t>
{
    static void nvic_enable() { nvic<USART2>::enable(); }
};

} // namespace internal

} // namespace hal

