#pragma once

#include <hal.h>
#include <gpio.h>

namespace hal
{

namespace timer
{

using namespace device;

template<int TN> struct timer_traits {};

#if defined(HAVE_PERIPHERAL_TIM1)
template<> struct timer_traits<1>
{
    typedef tim1_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM1; }
    static const gpio::internal::alternate_function_t ch1 = gpio::internal::TIM1_CH1;
    static const gpio::internal::alternate_function_t ch2 = gpio::internal::TIM1_CH2;
};
#endif

#if defined(HAVE_PERIPHERAL_TIM2)
template<> struct timer_traits<2>
{
    typedef tim2_t T;
    typedef uint32_t count_t;
    static inline T& TIM() { return TIM2; }
    static const gpio::internal::alternate_function_t ch1 = gpio::internal::TIM2_CH1_ETR;
    static const gpio::internal::alternate_function_t ch2 = gpio::internal::TIM2_CH2;
};
#endif

#if defined(HAVE_PERIPHERAL_TIM3)
template<> struct timer_traits<3>
{
    typedef tim3_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM3; }
    static const gpio::internal::alternate_function_t ch1 = gpio::internal::TIM3_CH1;
    static const gpio::internal::alternate_function_t ch2 = gpio::internal::TIM3_CH2;
};
#endif

#if defined(HAVE_PERIPHERAL_TIM4)
template<> struct timer_traits<4>
{
    typedef tim4_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM4; }
};
#endif

#if defined(HAVE_PERIPHERAL_TIM5)
template<> struct timer_traits<5>
{
    typedef tim5_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM5; }
};
#endif

#if defined(HAVE_PERIPHERAL_TIM6)
template<> struct timer_traits<6>
{
    typedef tim6_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM6; }
};
#endif

#if defined(HAVE_PERIPHERAL_TIM7)
template<> struct timer_traits<7>
{
    typedef tim7_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM7; }
};
#endif

#if defined(HAVE_PERIPHERAL_TIM9)
template<> struct timer_traits<9>
{
    typedef tim9_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM9; }
};
#endif

#if defined(HAVE_PERIPHERAL_TIM10)
template<> struct timer_traits<10>
{
    typedef tim10_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM10; }
};
#endif

#if defined(HAVE_PERIPHERAL_TIM11)
template<> struct timer_traits<11>
{
    typedef tim11_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM11; }
};
#endif

#if defined(HAVE_PERIPHERAL_TIM14)
template<> struct timer_traits<14>
{
    typedef tim14_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM14; }
};
#endif

#if defined(HAVE_PERIPHERAL_TIM15)
template<> struct timer_traits<15>
{
    typedef tim15_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM15; }
};
#endif

#if defined(HAVE_PERIPHERAL_TIM16)
template<> struct timer_traits<16>
{
    typedef tim16_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM16; }
};
#endif

#if defined(HAVE_PERIPHERAL_TIM17)
template<> struct timer_traits<17>
{
    typedef tim17_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM17; }
};
#endif

template<int TN>
class timer_t
{
public:
    typedef typename timer_traits<TN>::count_t count_t;

    static inline void setup(uint16_t psc, count_t arr)
    {
        peripheral<_>::rcc_enable();
        TIM().CR1 = _::CR1_RESET_VALUE;
        TIM().PSC = psc;
        TIM().ARR = arr;
        TIM().CR1 |= _::CR1_ARPE;
        TIM().CR1 |= _::CR1_CEN;
    }

    static inline void update_interrupt_enable()
    {
        TIM().DIER |= _::DIER_UIE;
        peripheral<_>::nvic_enable();
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
    typedef typename timer_traits<TN>::T _;
    typedef typename timer_traits<TN>::count_t count_t;

    template<gpio::input_type_t input_type>
    static inline void setup(count_t arr)
    {
        using namespace gpio::internal;

        alternate_t<CH1, timer_traits<TN>::ch1>::template setup<input_type>();
        alternate_t<CH2, timer_traits<TN>::ch2>::template setup<input_type>();

        peripheral<_>::rcc_enable();
        TIM().CCMR1 = _::CCMR1_RESET_VALUE
                    | _::template CCMR1_CC1S<0x1>
                    | _::template CCMR1_CC2S<0x1>
                    ; // TI1 & TI2 as inputs
        TIM().CCER = _::CCER_RESET_VALUE;   // CCER_CC1P CCER_CC2P polarity choices
        TIM().SMCR = _::SMCR_RESET_VALUE | _::template SMCR_SMS<0x1>;
        TIM().ARR = arr;
        TIM().CNT = _::CNT_RESET_VALUE;;
        TIM().CR1 = _::CR1_RESET_VALUE | _::CR1_CEN;
    }

    static inline volatile count_t count()
    {
        return TIM().CNT;
    }

    static inline void set_count(count_t x)
    {
        TIM().CNT = x;
    }

private:
    static inline typename timer_traits<TN>::T& TIM() { return timer_traits<TN>::TIM(); }
};

} // namespace timer

} // namespace hal

