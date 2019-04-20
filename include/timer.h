#pragma once

#include <stm32f0.h>

namespace stm32f0
{

namespace timer
{

using namespace device;

template<int TN> struct timer_traits {};

template<> struct timer_traits<1>
{
    typedef tim1_t T;
    static inline T& TIM() { return TIM1; }
    static inline void rcc_enable() { RCC.APB2ENR |= BV(rcc_t::APB2ENR_TIM1EN); }
    static inline void nvic_enable() { NVIC.ISER |= BV(13); }
};

template<> struct timer_traits<2>
{
    typedef tim2_t T;
    static inline T& TIM() { return TIM2; }
    static inline void rcc_enable() { RCC.APB1ENR |= BV(rcc_t::APB1ENR_TIM2EN); }
    static inline void nvic_enable() { NVIC.ISER |= BV(15); }
};

template<> struct timer_traits<3>
{
    typedef tim3_t T;
    static inline T& TIM() { return TIM3; }
    static inline void rcc_enable() { RCC.APB1ENR |= BV(rcc_t::APB1ENR_TIM3EN); }
    static inline void nvic_enable() { NVIC.ISER |= BV(16); }
};

#if defined(STM32F05x) || defined(STM32F07x) || defined(STM32F09x)
template<> struct timer_traits<6>
{
    typedef tim6_t T;
    static inline T& TIM() { return TIM6; }
    static inline void rcc_enable() { RCC.APB1ENR |= BV(rcc_t::APB1ENR_TIM6EN); }
    static inline void nvic_enable() { NVIC.ISER |= BV(17); }
};
#endif

#if defined(STM32F07x) || defined(STM32F09x)
template<> struct timer_traits<7>
{
    typedef tim7_t T;
    static inline T& TIM() { return TIM7; }
    static inline void rcc_enable() { RCC.APB1ENR |= BV(rcc_t::APB1ENR_TIM7EN); }
    static inline void nvic_enable() { NVIC.ISER |= BV(18); }
};
#endif

template<> struct timer_traits<14>
{
    typedef tim14_t T;
    static inline T& TIM() { return TIM14; }
    static inline void rcc_enable() { RCC.APB1ENR |= BV(rcc_t::APB1ENR_TIM14EN); }
    static inline void nvic_enable() { NVIC.ISER |= BV(19); }
};

#if !defined(STM32F03x)
template<> struct timer_traits<15>
{
    typedef tim15_t T;
    static inline T& TIM() { return TIM15; }
    static inline void rcc_enable() { RCC.APB2ENR |= BV(rcc_t::APB2ENR_TIM15EN); }
    static inline void nvic_enable() { NVIC.ISER |= BV(20); }
};
#endif

template<> struct timer_traits<16>
{
    typedef tim16_t T;
    static inline T& TIM() { return TIM16; }
    static inline void rcc_enable() { RCC.APB2ENR |= BV(rcc_t::APB2ENR_TIM16EN); }
    static inline void nvic_enable() { NVIC.ISER |= BV(21); }
};

template<> struct timer_traits<17>
{
    typedef tim17_t T;
    static inline T& TIM() { return TIM17; }
    static inline void rcc_enable() { RCC.APB2ENR |= BV(rcc_t::APB2ENR_TIM17EN); }
    static inline void nvic_enable() { NVIC.ISER |= BV(22); }
};

template<int TN>
class timer_t
{
public:
    static inline void setup(uint16_t psc, uint16_t arr)
    {
        timer_traits<TN>::rcc_enable();
        TIM().CR1 = _::CR1_RESET_VALUE;
        TIM().PSC = psc;
        TIM().ARR = arr;
        TIM().CR1 |= BV(_::CR1_ARPE);
        TIM().CR1 |= BV(_::CR1_CEN);
    }

    static inline void update_interrupt_enable()
    {
        TIM().DIER |= BV(_::DIER_UIE);
        timer_traits<TN>::nvic_enable();
    }

    static inline volatile bool uif()
    {
        return (TIM().SR & BV(_::SR_UIF)) != 0;
    }

    static inline volatile void clear_uif()
    {
        TIM().SR &= ~BV(_::SR_UIF);
    }

    static inline volatile uint32_t cnt()
    {
        return TIM().CNT;
    }

private:
    static inline typename timer_traits<TN>::T& TIM() { return timer_traits<TN>::TIM(); }
    typedef typename timer_traits<TN>::T _;
};

}

}

