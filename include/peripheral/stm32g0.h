#pragma once

namespace hal
{

namespace internal
{

using namespace device;

enum pos_t
  { WWDG = 0
  , PVD_VDDIO2 = 1
  , RTC = 2
  , FLASH = 3
  , RCC_CRS = 4
  , EXTI0_1 = 5
  , EXTI2_3 = 6
  , EXTI4_15 = 7
  , TSC = 8
  , DMA_CH1 = 9
  , DMA_CH2_3 = 10
  , DMA_CH4_5_6_7 = 11
  , ADC_COMP = 12
  , TIM1_BRK_UP_TRG_COM = 13
  , TIM1_CC = 14
  , TIM2 = 15
  , TIM3 = 16
  , TIM6_DAC = 17
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
  , USART3_4_5_6_7_8 = 29
  , CEC_CAN = 30
  , USB = 31
  };

template<typename PERIPHERAL> struct peripheral_traits {};

template<> struct peripheral_traits<gpioa_t>
{
    static void rcc_enable() { RCC.IOPENR |= rcc_t::IOPENR_IOPAEN; }
};

template<> struct peripheral_traits<gpiob_t>
{
    static void rcc_enable() { RCC.IOPENR |= rcc_t::IOPENR_IOPBEN; }
};

template<> struct peripheral_traits<gpioc_t>
{
    static void rcc_enable() { RCC.IOPENR |= rcc_t::IOPENR_IOPCEN; }
};

template<> struct peripheral_traits<gpiod_t>
{
    static void rcc_enable() { RCC.IOPENR |= rcc_t::IOPENR_IOPDEN; }
};

#if defined(HAVE_PERIPHERAL_GPIOE)
template<> struct peripheral_traits<gpioe_t>
{
    static void rcc_enable() { RCC.IOPENR |= rcc_t::IOPENR_IOPEEN; }
};
#endif

template<> struct peripheral_traits<gpiof_t>
{
    static void rcc_enable() { RCC.IOPENR |= rcc_t::IOPENR_IOPFEN; }
};

template<> struct peripheral_traits<tim1_t>
{
    static void rcc_enable() { RCC.APBENR2 |= rcc_t::APBENR2_TIM1EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << TIM1_BRK_UP_TRG_COM; }
};

template<> struct peripheral_traits<tim2_t>
{
    static void rcc_enable() { RCC.APBENR1 |= rcc_t::APBENR1_TIM2EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << TIM2; }
};

template<> struct peripheral_traits<tim3_t>
{
    static void rcc_enable() { RCC.APBENR1 |= rcc_t::APBENR1_TIM3EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << TIM3; }
};

template<> struct peripheral_traits<tim6_t>
{
    static void rcc_enable() { RCC.APBENR1 |= rcc_t::APBENR1_TIM6EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << TIM6_DAC; }
};

template<> struct peripheral_traits<tim7_t>
{
    static void rcc_enable() { RCC.APBENR1 |= rcc_t::APBENR1_TIM7EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << TIM7; }
};

template<> struct peripheral_traits<tim14_t>
{
    static void rcc_enable() { RCC.APBENR2 |= rcc_t::APBENR2_TIM14EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << TIM14; }
};

template<> struct peripheral_traits<tim15_t>
{
    static void rcc_enable() { RCC.APBENR2 |= rcc_t::APBENR2_TIM15EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << TIM15; }
};

template<> struct peripheral_traits<tim16_t>
{
    static void rcc_enable() { RCC.APBENR2 |= rcc_t::APBENR2_TIM16EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << TIM16; }
};

template<> struct peripheral_traits<tim17_t>
{
    static void rcc_enable() { RCC.APBENR2 |= rcc_t::APBENR2_TIM17EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << TIM17; }
};

template<> struct peripheral_traits<i2c1_t>
{
    static void nvic_enable() { NVIC.ISER |= 1 << I2C1; }
};

template<> struct peripheral_traits<i2c2_t>
{
    static void nvic_enable() { NVIC.ISER |= 1 << I2C2; }
};

template<> struct peripheral_traits<spi1_t>
{
    static void rcc_enable() { RCC.APBENR2 |= rcc_t::APBENR2_SPI1EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << SPI1; }
};

template<> struct peripheral_traits<spi2_t>
{
    static void rcc_enable() { RCC.APBENR1 |= rcc_t::APBENR1_SPI2EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << SPI2; }
};

template<> struct peripheral_traits<usart1_t>
{
    static void nvic_enable() { NVIC.ISER |= 1 << USART1; }
};

template<> struct peripheral_traits<usart2_t>
{
    static void nvic_enable() { NVIC.ISER |= 1 << USART2; }
};

} // namespace internal

} // namespace hal

