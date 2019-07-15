#pragma once

namespace hal
{

namespace internal
{

using namespace device;

template<typename PERIPHERAL> struct peripheral_traits {};

template<> struct peripheral_traits<gpioa_t>
{
    static void rcc_enable() { RCC.AHBENR |= rcc_t::AHBENR_IOPAEN; }
};

template<> struct peripheral_traits<gpiob_t>
{
    static void rcc_enable() { RCC.AHBENR |= rcc_t::AHBENR_IOPBEN; }
};

template<> struct peripheral_traits<gpioc_t>
{
    static void rcc_enable() { RCC.AHBENR |= rcc_t::AHBENR_IOPCEN; }
};

template<> struct peripheral_traits<gpiod_t>
{
    static void rcc_enable() { RCC.AHBENR |= rcc_t::AHBENR_IOPDEN; }
};

#if defined(HAVE_PERIPHERAL_GPIOE)
template<> struct peripheral_traits<gpioe_t>
{
    static void rcc_enable() { RCC.AHBENR |= rcc_t::AHBENR_IOPEEN; }
};
#endif

template<> struct peripheral_traits<gpiof_t>
{
    static void rcc_enable() { RCC.AHBENR |= rcc_t::AHBENR_IOPFEN; }
};

template<> struct peripheral_traits<tim1_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM1EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << tim1_t::TIM1_BRK_UP_TRG_COM; }
};

template<> struct peripheral_traits<tim2_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM2EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << tim2_t::TIM2; }
};

template<> struct peripheral_traits<tim3_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM3EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << tim3_t::TIM3; }
};

#if defined(HAVE_PERIPHERAL_TIM6)
template<> struct peripheral_traits<tim6_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM6EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << dac_t::TIM6_DAC; }
};
#endif

#if defined(HAVE_PERIPHERAL_TIM7)
template<> struct peripheral_traits<tim7_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM7EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << tim7_t::TIM7; }
};
#endif

template<> struct peripheral_traits<tim14_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM14EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << tim14_t::TIM14; }
};

#if defined(HAVE_PERIPHERAL_TIM15)
template<> struct peripheral_traits<tim15_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM15EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << tim15_t::TIM15; }
};
#endif

template<> struct peripheral_traits<tim16_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM16EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << tim16_t::TIM16; }
};

template<> struct peripheral_traits<tim17_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM17EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << tim17_t::TIM17; }
};

template<> struct peripheral_traits<i2c1_t>
{
    static void nvic_enable() { NVIC.ISER |= 1 << i2c1_t::I2C1; }
};

template<> struct peripheral_traits<i2c2_t>
{
    static void nvic_enable() { NVIC.ISER |= 1 << i2c2_t::I2C2; }
};

template<> struct peripheral_traits<spi1_t>
{
    static void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_SPI1EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << spi1_t::SPI1; }
};

template<> struct peripheral_traits<spi2_t>
{
    static void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_SPI2EN; }
    static void nvic_enable() { NVIC.ISER |= 1 << spi2_t::SPI2; }
};

template<> struct peripheral_traits<usart1_t>
{
    static void nvic_enable() { NVIC.ISER |= 1 << usart1_t::USART1; }
};

template<> struct peripheral_traits<usart2_t>
{
    static void nvic_enable() { NVIC.ISER |= 1 << usart2_t::USART2; }
};

} // namespace internal

} // namespace hal

