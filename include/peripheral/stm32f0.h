#pragma once

namespace system
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

template<> struct peripheral_traits<tim1_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << TIM1_BRK_UP_TRG_COM; } };
template<> struct peripheral_traits<tim2_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << TIM2; } };
template<> struct peripheral_traits<tim3_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << TIM3; } };
#if defined(STM32F05x) || defined(STM32F07x) || defined(STM32F09x)
template<> struct peripheral_traits<tim6_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << TIM6_DAC; } };
#endif
#if defined(STM32F07x) || defined(STM32F09x)
template<> struct peripheral_traits<tim7_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << TIM7; } };
#endif
template<> struct peripheral_traits<tim14_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << TIM14; } };
#if !defined(STM32F03x)
template<> struct peripheral_traits<tim15_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << TIM15; } };
#endif
template<> struct peripheral_traits<tim16_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << TIM16; } };
template<> struct peripheral_traits<tim17_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << TIM17; } };
template<> struct peripheral_traits<i2c1_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << I2C1; } };
template<> struct peripheral_traits<i2c2_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << I2C2; } };
template<> struct peripheral_traits<spi1_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << SPI1; } };
template<> struct peripheral_traits<spi2_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << SPI2; } };
template<> struct peripheral_traits<usart1_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << USART1; } };
template<> struct peripheral_traits<usart2_t> { static void nvic_enable() { device::NVIC.ISER |= 1 << USART2; } };

} // namespace internal

} // namespace system

