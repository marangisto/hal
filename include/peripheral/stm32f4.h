#pragma once

namespace hal
{

namespace internal
{

using namespace device;

enum pos_t
    //                              NVIC.ISER0 = BV(XX)
    { WWDG = 0
    , EXTI16_PVD = 1
    , EXTI21_TAMP_STAMP = 2
    , EXTI22_RTC_WKUP = 3
    , FLASH = 4
    , RCC = 5
    , EXTI0 = 6
    , EXTI1 = 7
    , EXTI2 = 8
    , EXTI3 = 9
    , EXTI4 = 10
    , DMA1_Stream0 = 11
    , DMA1_Stream1 = 12
    , DMA1_Stream2 = 13
    , DMA1_Stream3 = 14
    , DMA1_Stream4 = 15
    , DMA1_Stream5 = 16
    , DMA1_Stream6 = 17
    , ADC = 18
    , EXTI9_5 = 23
    , TIM1_BRK_TIM9 = 24
    , TIM1_UP_TIM10 = 25
    , TIM1_TRG_COM_TIM11 = 26
    , TIM1_CC = 27
    , TIM2 = 28
    , TIM3 = 29
    , TIM4 = 30
    , I2C1_EV = 31
    //                              NVIC.ISER1 = BV(XX-32)
    , I2C1_ER = 32
    , I2C2_EV = 33
    , I2C2_ER = 34
    , SPI1 = 35
    , SPI2 = 36
    , USART1 = 37
    , USART2 = 38
    , EXTI15_10 = 40
    , EXTI17_RTC_Alarm = 41
    , EXTI18_OTG_FS_WKUP = 42
    , DMA1_Stream7 = 47
    , SDIO = 49
    , TIM5 = 50
    , SPI3 = 51
    , DMA2_Stream0 = 56
    , DMA2_Stream1 = 57
    , DMA2_Stream2 = 58
    , DMA2_Stream3 = 59
    , DMA2_Stream4 = 60
    //                              NVIC.ISER2 = BV(XX-64)
    , OTG_FS = 67
    , DMA2_Stream5 = 68
    , DMA2_Stream6 = 69
    , DMA2_Stream7 = 70
    , USART6 = 71
    , I2C3_EV = 72
    , I2C3_ER = 73
    , FPU = 81
    , SPI4 = 84
    , SPI5 = 85
    };

template<typename PERIPHERAL> struct peripheral_traits {};

template<> struct peripheral_traits<gpioa_t>
{
    static void rcc_enable() { device::RCC.AHB1ENR |= rcc_t::AHB1ENR_GPIOAEN; }
};

template<> struct peripheral_traits<gpiob_t>
{
    static void rcc_enable() { device::RCC.AHB1ENR |= rcc_t::AHB1ENR_GPIOBEN; }
};

template<> struct peripheral_traits<gpioc_t>
{
    static void rcc_enable() { device::RCC.AHB1ENR |= rcc_t::AHB1ENR_GPIOCEN; }
};

template<> struct peripheral_traits<gpiod_t>
{
    static void rcc_enable() { device::RCC.AHB1ENR |= rcc_t::AHB1ENR_GPIODEN; }
};

template<> struct peripheral_traits<gpioe_t>
{
    static void rcc_enable() { device::RCC.AHB1ENR |= rcc_t::AHB1ENR_GPIOEEN; }
};

template<> struct peripheral_traits<gpioh_t>
{
    static void rcc_enable() { device::RCC.AHB1ENR |= rcc_t::AHB1ENR_GPIOHEN; }
};

template<> struct peripheral_traits<tim1_t>
{
    static void rcc_enable() { device::RCC.APB2ENR |= rcc_t::APB2ENR_TIM1EN; }
    static void nvic_enable() { NVIC.ISER0 |= 1 << TIM1_UP_TIM10; }
};

template<> struct peripheral_traits<tim2_t>
{
    static void rcc_enable() { device::RCC.APB1ENR |= rcc_t::APB1ENR_TIM2EN; }
    static void nvic_enable() { NVIC.ISER0 |= 1 << TIM2; }
};

template<> struct peripheral_traits<tim3_t>
{
    static void rcc_enable() { device::RCC.APB1ENR |= rcc_t::APB1ENR_TIM3EN; }
    static void nvic_enable() { NVIC.ISER0 |= 1 << TIM3; }
};

template<> struct peripheral_traits<tim4_t>
{
    static void rcc_enable() { device::RCC.APB1ENR |= rcc_t::APB1ENR_TIM4EN; }
    static void nvic_enable() { NVIC.ISER0 |= 1 << TIM4; }
};

template<> struct peripheral_traits<tim5_t>
{
    static void rcc_enable() { device::RCC.APB1ENR |= rcc_t::APB1ENR_TIM5EN; }
    static void nvic_enable() { NVIC.ISER1 |= 1 << (TIM5 - 32); }
};

template<> struct peripheral_traits<tim9_t>
{
    static void rcc_enable() { device::RCC.APB2ENR |= rcc_t::APB2ENR_TIM9EN; }
    static void nvic_enable() { NVIC.ISER0 |= 1 << TIM1_BRK_TIM9; }
};

template<> struct peripheral_traits<tim10_t>
{
    static void rcc_enable() { device::RCC.APB2ENR |= rcc_t::APB2ENR_TIM10EN; }
    static void nvic_enable() { NVIC.ISER0 |= 1 << TIM1_UP_TIM10; }
};

template<> struct peripheral_traits<tim11_t>
{
    static void rcc_enable() { device::RCC.APB2ENR |= rcc_t::APB2ENR_TIM11EN; }
    static void nvic_enable() { NVIC.ISER0 |= 1 << TIM1_TRG_COM_TIM11; }
};

template<> struct peripheral_traits<spi1_t>
{
    static void rcc_enable() { device::RCC.APB2ENR |= rcc_t::APB2ENR_SPI1EN; }
    static void nvic_enable() { NVIC.ISER1 |= 1 << (SPI1 - 32); }
};

template<> struct peripheral_traits<spi2_t>
{
    static void rcc_enable() { device::RCC.APB1ENR |= rcc_t::APB1ENR_SPI2EN; }
    static void nvic_enable() { NVIC.ISER1 |= 1 << (SPI2 - 32); }
};

template<> struct peripheral_traits<usart1_t>
{
    static void nvic_enable() { NVIC.ISER1 |= 1 << (USART1 - 32); }
};

template<> struct peripheral_traits<usart2_t>
{
    static void nvic_enable() { NVIC.ISER1 |= 1 << (USART2 - 32); }
};

} // namespace internal

} // namespace hal

