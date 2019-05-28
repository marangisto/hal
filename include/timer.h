#pragma once

#include <stm32f0.h>
#include <gpio.h>

namespace stm32f0
{

namespace timer
{

using namespace device;

template<int TN> struct timer_traits {};

template<> struct timer_traits<1>
{
    typedef tim1_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM1; }
    static inline void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM1EN; }
    static inline void nvic_enable() { ISR::enable<ISR::TIM1_BRK_UP_TRG_COM>(); }
    static const gpio::internal::alternate_function_t ch1 = gpio::internal::TIM1_CH1;
    static const gpio::internal::alternate_function_t ch2 = gpio::internal::TIM1_CH2;
};

template<> struct timer_traits<2>
{
    typedef tim2_t T;
    typedef uint32_t count_t;
    static inline T& TIM() { return TIM2; }
    static inline void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM2EN; }
    static inline void nvic_enable() { ISR::enable<ISR::TIM2>(); }
    static const gpio::internal::alternate_function_t ch1 = gpio::internal::TIM2_CH1_ETR;
    static const gpio::internal::alternate_function_t ch2 = gpio::internal::TIM2_CH2;
};

template<> struct timer_traits<3>
{
    typedef tim3_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM3; }
    static inline void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM3EN; }
    static inline void nvic_enable() { ISR::enable<ISR::TIM3>(); }
    static const gpio::internal::alternate_function_t ch1 = gpio::internal::TIM3_CH1;
    static const gpio::internal::alternate_function_t ch2 = gpio::internal::TIM3_CH2;
};

#if defined(STM32F05x) || defined(STM32F07x) || defined(STM32F09x)
template<> struct timer_traits<6>
{
    typedef tim6_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM6; }
    static inline void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM6EN; }
    static inline void nvic_enable() { ISR::enable<ISR::TIM6_DAC>(); }
};
#endif

#if defined(STM32F07x) || defined(STM32F09x)
template<> struct timer_traits<7>
{
    typedef tim7_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM7; }
    static inline void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM7EN; }
    static inline void nvic_enable() { ISR::enable<ISR::TIM7>(); }
};
#endif

template<> struct timer_traits<14>
{
    typedef tim14_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM14; }
    static inline void rcc_enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM14EN; }
    static inline void nvic_enable() { ISR::enable<ISR::TIM14>(); }
};

#if !defined(STM32F03x)
template<> struct timer_traits<15>
{
    typedef tim15_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM15; }
    static inline void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM15EN; }
    static inline void nvic_enable() { ISR::enable<ISR::TIM15>(); }
};
#endif

template<> struct timer_traits<16>
{
    typedef tim16_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM16; }
    static inline void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM16EN; }
    static inline void nvic_enable() { ISR::enable<ISR::TIM16>(); }
};

template<> struct timer_traits<17>
{
    typedef tim17_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM17; }
    static inline void rcc_enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM17EN; }
    static inline void nvic_enable() { ISR::enable<ISR::TIM17>(); }
};

template<int TN>
class timer_t
{
public:
    typedef typename timer_traits<TN>::count_t count_t;

    static inline void setup(uint16_t psc, count_t arr)
    {
        timer_traits<TN>::rcc_enable();
        TIM().CR1 = _::CR1_RESET_VALUE;
        TIM().PSC = psc;
        TIM().ARR = arr;
        TIM().CR1 |= _::CR1_ARPE;
        TIM().CR1 |= _::CR1_CEN;
    }

    static inline void update_interrupt_enable()
    {
        TIM().DIER |= _::DIER_UIE;
        timer_traits<TN>::nvic_enable();
    }

    static inline volatile bool uif()
    {
        return (TIM().SR & _::SR_UIF) != 0;
    }

    static inline volatile void clear_uif()
    {
        TIM().SR &= ~_::SR_UIF;
    }

    static inline volatile count_t cnt()
    {
        return TIM().CNT;
    }

private:
    static inline typename timer_traits<TN>::T& TIM() { return timer_traits<TN>::TIM(); }
    typedef typename timer_traits<TN>::T _;
};

template<int TN, gpio::gpio_pin_t CH1, gpio::gpio_pin_t CH2>
class encoder_t
{
public:
    typedef typename timer_traits<TN>::count_t count_t;

    template<gpio::input_type_t input_type>
    static inline void setup(count_t arr)
    {
        using namespace gpio;

        internal::alternate_t<CH1, timer_traits<TN>::ch1>::template setup<input_type>();
        internal::alternate_t<CH2, timer_traits<TN>::ch2>::template setup<input_type>();

        timer_traits<TN>::rcc_enable();
        TIM().CCMR1 = _::CCMR1_RESET_VALUE | _::CCMR1_CC1S(0x1) | _::CCMR1_CC2S(0x1); // TI1 & TI2 as inputs
        TIM().CCER = _::CCER_RESET_VALUE;   // CCER_CC1P CCER_CC2P polarity choices
        TIM().SMCR = _::SMCR_RESET_VALUE | _::SMCR_SMS(0x1);
        TIM().ARR = arr;
        TIM().CNT = _::CNT_RESET_VALUE;;
        TIM().CR1 = _::CR1_RESET_VALUE | _::CR1_CEN;
    }

    static inline volatile count_t count()
    {
        return TIM().CNT;
    }

private:
    static inline typename timer_traits<TN>::T& TIM() { return timer_traits<TN>::TIM(); }
    typedef typename timer_traits<TN>::T _;
};

}

}

