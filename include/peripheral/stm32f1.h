#pragma once

namespace hal
{

namespace internal
{

using namespace device;

enum pos_t
  { WWDG = 0
  , PVD = 1
  , RTC_TAMP = 2
  , FLASH = 3
  , RCC_ = 4
  , EXTI0_1 = 5
  , EXTI2_3 = 6
  , EXTI4_15 = 7
  , UCPD1_UCPD2 = 8
  , DMA_CH1 = 9
  , DMA_CH2_3 = 10
  , DMA_CH4_5_6_7_DMAMUX = 11
  , ADC_COMP = 12
  , TIM1_BRK_UP_TRG_COM = 13
  , TIM1_CC = 14
  , TIM2 = 15
  , TIM3 = 16
  , TIM6 = 17
  , TIM7 = 18
  , TIM14 = 19
  , TIM15 = 20
  , TIM16 = 21
  , TIM17 = 22
  , I2C1 = 23
  , I2C2 = 24
  , SPI1 = 25
  , SPI2 = 26
  , USART1 = 27
  , USART2 = 28
  , USART3_4_LPUART1 = 29
  , CEC = 30
  , AES_RNG = 31
  };

template<typename PERIPHERAL> struct peripheral_traits {};

template<> struct peripheral_traits<gpioa_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_IOPAEN; }
};

template<> struct peripheral_traits<gpiob_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_IOPBEN; }
};

template<> struct peripheral_traits<gpioc_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_IOPCEN; }
};

template<> struct peripheral_traits<gpiod_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_IOPDEN; }
};

template<> struct peripheral_traits<gpiof_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_IOPFEN; }
};

template<> struct peripheral_traits<tim1_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM1EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM1_BRK_UP_TRG_COM; }
};

template<> struct peripheral_traits<tim2_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM2EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM2; }
};

template<> struct peripheral_traits<tim3_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM3EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM3; }
};

template<> struct peripheral_traits<tim4_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM4EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM4; }
};

template<> struct peripheral_traits<tim5_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM5EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM5; }
};

template<> struct peripheral_traits<tim6_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM6EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM6; }
};

template<> struct peripheral_traits<tim7_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM7EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM7; }
};

template<> struct peripheral_traits<tim8_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM8EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM8; }
};

template<> struct peripheral_traits<tim9_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM9EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM9; }
};

template<> struct peripheral_traits<tim10_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM10EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM10; }
};

template<> struct peripheral_traits<tim11_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM11EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM11; }
};

template<> struct peripheral_traits<tim12_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM12EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM12; }
};

template<> struct peripheral_traits<tim13_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM13EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM13; }
};

template<> struct peripheral_traits<tim14_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM14EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << TIM14; }
};

template<> struct peripheral_traits<i2c1_t>
{
//    static void nvic_enable() { NVIC.ISER |= 1 << I2C1; }
};

template<> struct peripheral_traits<i2c2_t>
{
//    static void nvic_enable() { NVIC.ISER |= 1 << I2C2; }
};

template<> struct peripheral_traits<spi1_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_SPI1EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << SPI1; }
};

template<> struct peripheral_traits<spi2_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_SPI2EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << SPI2; }
};

template<> struct peripheral_traits<spi3_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_SPI3EN; }
//    static void nvic_enable() { NVIC.ISER |= 1 << SPI3; }
};

template<> struct peripheral_traits<usart1_t>
{
//    static void nvic_enable() { NVIC.ISER |= 1 << USART1; }
};

template<> struct peripheral_traits<usart2_t>
{
//    static void nvic_enable() { NVIC.ISER |= 1 << USART2; }
};

template<> struct peripheral_traits<usart3_t>
{
//    static void nvic_enable() { NVIC.ISER |= 1 << USART3; }
};

} // namespace internal

} // namespace hal


