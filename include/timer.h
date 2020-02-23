#pragma once

#include <hal.h>
#include <gpio.h>

namespace hal
{

namespace timer
{

using namespace device;
using gpio::internal::alternate_function_t;

template<int TN> struct timer_traits
{
    static_assert(always_false_i<TN>::value, "timer not available on this mcu");
};

template<channel_t CH, typename T> struct timer_channel_traits
{
    static_assert(always_false_t<T>::value, "channel number not available on this timer");
};

template<channel_t CH, typename T> struct timer_altfun_traits {};

template<typename T> struct timer_channel_traits<CH1, T>
{
    static const uint32_t CCER_CCE = T::_::CCER_CC1E;
    static const uint32_t CCMR_OCPE = T::_::CCMR1_OC1PE;
    template<uint32_t X> static const uint32_t CCMR_OCM = T::_::template CCMR1_OC1M<X>;
    static inline volatile uint32_t& CCR() { return T::TIM().CCR1; }
    static inline volatile uint32_t& CCMR() { return T::TIM().CCMR1; }
};

template<typename T> struct timer_channel_traits<CH2, T>
{
    static const uint32_t CCER_CCE = T::_::CCER_CC2E;
    static const uint32_t CCMR_OCPE = T::_::CCMR1_OC2PE;
    template<uint32_t X> static const uint32_t CCMR_OCM = T::_::template CCMR1_OC2M<X>;
    static inline volatile uint32_t& CCR() { return T::TIM().CCR2; }
    static inline volatile uint32_t& CCMR() { return T::TIM().CCMR1; }
};

template<typename T> struct timer_channel_traits<CH3, T>
{
    static const uint32_t CCER_CCE = T::_::CCER_CC3E;
    static const uint32_t CCMR_OCPE = T::_::CCMR2_OC3PE;
    template<uint32_t X> static const uint32_t CCMR_OCM = T::_::template CCMR2_OC3M<X>;
    static inline volatile uint32_t& CCR() { return T::TIM().CCR3; }
    static inline volatile uint32_t& CCMR() { return T::TIM().CCMR2; }
};

template<typename T> struct timer_channel_traits<CH4, T>
{
    static const uint32_t CCER_CCE = T::_::CCER_CC4E;
    static const uint32_t CCMR_OCPE = T::_::CCMR2_OC4PE;
    template<uint32_t X> static const uint32_t CCMR_OCM = T::_::template CCMR2_OC4M<X>;
    static inline volatile uint32_t& CCR() { return T::TIM().CCR4; }
    static inline volatile uint32_t& CCMR() { return T::TIM().CCMR2; }
};

#if defined(HAVE_PERIPHERAL_TIM1)
template<> struct timer_traits<1>
{
    typedef tim1_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM1; }
};

template<> struct timer_altfun_traits<CH1, tim1_t> { static const alternate_function_t altfun = gpio::internal::TIM1_CH1; };
template<> struct timer_altfun_traits<CH2, tim1_t> { static const alternate_function_t altfun = gpio::internal::TIM1_CH2; };
template<> struct timer_altfun_traits<CH3, tim1_t> { static const alternate_function_t altfun = gpio::internal::TIM1_CH3; };
template<> struct timer_altfun_traits<CH4, tim1_t> { static const alternate_function_t altfun = gpio::internal::TIM1_CH4; };
#endif

#if defined(HAVE_PERIPHERAL_TIM2)
template<> struct timer_traits<2>
{
    typedef tim2_t T;
    typedef uint32_t count_t;
    static inline T& TIM() { return TIM2; }
};

template<> struct timer_altfun_traits<CH1, tim2_t> { static const alternate_function_t altfun = gpio::internal::TIM2_CH1; };
template<> struct timer_altfun_traits<CH2, tim2_t> { static const alternate_function_t altfun = gpio::internal::TIM2_CH2; };
template<> struct timer_altfun_traits<CH3, tim2_t> { static const alternate_function_t altfun = gpio::internal::TIM2_CH3; };
template<> struct timer_altfun_traits<CH4, tim2_t> { static const alternate_function_t altfun = gpio::internal::TIM2_CH4; };
#endif

#if defined(HAVE_PERIPHERAL_TIM3)
template<> struct timer_traits<3>
{
    typedef tim3_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM3; }
};

template<> struct timer_altfun_traits<CH1, tim3_t> { static const alternate_function_t altfun = gpio::internal::TIM3_CH1; };
template<> struct timer_altfun_traits<CH2, tim3_t> { static const alternate_function_t altfun = gpio::internal::TIM3_CH2; };
template<> struct timer_altfun_traits<CH3, tim3_t> { static const alternate_function_t altfun = gpio::internal::TIM3_CH3; };
template<> struct timer_altfun_traits<CH4, tim3_t> { static const alternate_function_t altfun = gpio::internal::TIM3_CH4; };
#endif

#if defined(HAVE_PERIPHERAL_TIM4)
template<> struct timer_traits<4>
{
    typedef tim4_t T;
    typedef uint16_t count_t;
    static inline T& TIM() { return TIM4; }
};

template<> struct timer_altfun_traits<CH1, tim4_t> { static const alternate_function_t altfun = gpio::internal::TIM4_CH1; };
template<> struct timer_altfun_traits<CH2, tim4_t> { static const alternate_function_t altfun = gpio::internal::TIM4_CH2; };
template<> struct timer_altfun_traits<CH3, tim4_t> { static const alternate_function_t altfun = gpio::internal::TIM4_CH3; };
template<> struct timer_altfun_traits<CH4, tim4_t> { static const alternate_function_t altfun = gpio::internal::TIM4_CH4; };
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

template<typename, channel_t, gpio::gpio_pin_t> class pwm_t;

template<int TN>
class timer_t
{
public:
    static const int TNO = TN;
    typedef typename timer_traits<TN>::count_t count_t;
    enum master_mode_t { mm_reset, mm_enable, mm_update };

    static inline void setup(uint16_t psc, count_t arr)
    {
        peripheral_traits<_>::enable();
        TIM().CR1 = _::CR1_RESET_VALUE;
        TIM().PSC = psc;
        TIM().ARR = arr;
        TIM().CR1 |= _::CR1_ARPE;
        TIM().CR1 |= _::CR1_CEN;        // FIXME: should this be on by default?
    }

    static inline void enable()
    {
        TIM().CR1 |= _::CR1_CEN;
    }

    static inline void disable()
    {
        TIM().CR1 &= ~_::CR1_CEN;
    }

    static inline void update_interrupt_enable()
    {
        TIM().DIER |= _::DIER_UIE;
    }

    template<master_mode_t MM>
    static inline void master_mode()
    {
        TIM().CR2 |= _::template CR2_MMS<MM>;
    }

    static inline volatile bool uif()
    {
        return (TIM().SR & _::SR_UIF) != 0;
    }

    static inline void clear_uif()
    {
        TIM().SR &= ~_::SR_UIF;
    }

    static inline volatile count_t count()
    {
        return TIM().CNT;
    }

    static inline void set_count(count_t x)
    {
        TIM().CNT = x;
    }

    static inline void main_output_enable()
    {
        TIM().BDTR |= _::BDTR_MOE;
    }

    static inline void set_auto_reload_value(count_t arr)
    {
        TIM().ARR = arr;
    }

//private:
    template<typename, channel_t, gpio::gpio_pin_t> friend class pwm_t;
    static inline typename timer_traits<TN>::T& TIM() { return timer_traits<TN>::TIM(); }
    typedef typename timer_traits<TN>::T _;
};

template<int TN, gpio::gpio_pin_t PIN1, gpio::gpio_pin_t PIN2>
class encoder_t
{
public:
    typedef typename timer_traits<TN>::T _;
    typedef typename timer_traits<TN>::count_t count_t;

    template<gpio::input_type_t input_type>
    static inline void setup(count_t arr)
    {
        using namespace gpio::internal;

        alternate_t<PIN1, timer_altfun_traits<CH1, _>::altfun>::template setup<input_type>();
        alternate_t<PIN2, timer_altfun_traits<CH2, _>::altfun>::template setup<input_type>();

        peripheral_traits<_>::enable();
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

template<int TN, gpio::gpio_pin_t CH1>
class capture_t
{
public:
    typedef typename timer_traits<TN>::T _;
    typedef typename timer_traits<TN>::count_t count_t;

    template<gpio::input_type_t input_type>
    static inline void setup(count_t arr)
    {
        using namespace gpio::internal;

        alternate_t<CH1, timer_traits<TN>::ch1>::template setup<input_type>();

        peripheral_traits<_>::enable();
        TIM().CCMR1 = _::CCMR1_RESET_VALUE          // reset register
                    | _::template CCMR1_CC1S<0x1>   // TI1 as input
                    ;
        TIM().CCER  = _::CCER_RESET_VALUE           // reset register
                    | _::CCER_CC1E                  // enable capture
                    ;
        TIM().DIER  = _::DIER_RESET_VALUE           // reset register
                    | _::DIER_CC1IE                 // interrupt on capture
                    ;
        TIM().CR1   = _::CR1_RESET_VALUE            // reset register
                    | _::CR1_CEN                    // enable timer
                    ;
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

template<typename TIMER, channel_t CH, gpio::gpio_pin_t PIN>
class pwm_t
{
private:
    typedef typename TIMER::_ _;
    typedef timer_channel_traits<CH, TIMER> __;
    typedef gpio::internal::alternate_t<PIN, timer_altfun_traits<CH, _>::altfun> pin;

public:
    static void setup(typename TIMER::count_t initial_duty = 0)
    {
        using namespace gpio::internal;

        pin::template setup<gpio::high_speed>();    // initialize output pin
        __::CCMR() |= __::template CCMR_OCM<0x6>    // channel 4 pwm mode 1
                   |  __::CCMR_OCPE                 // channel preload enable
                   ;
        __::CCR() = initial_duty;                   // set initial duty cycle
        TIMER::TIM().CCER |= __::CCER_CCE;          // channel capture compare output enable
    }

    static typename TIMER::count_t duty()
    {
        return __::CCR();
    }

    static void duty(typename TIMER::count_t x)
    {
        __::CCR() = x;
    }
};

} // namespace timer

} // namespace hal

