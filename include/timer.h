#pragma once

#include <stm32f0.h>

namespace stm32f0
{

namespace timer
{

using namespace device;
using namespace tim6;

template<int TN>
class timer_t
{
public:
    static inline void setup()
    {
        TIM6.CR1 = CR1::RESET_VALUE;
        TIM6.SR = 0;
        TIM6.PSC = 0;                  // FIXME: parametric
        TIM6.ARR = 8000;
        TIM6.CR1 |= BV(CR1::CEN);       // enable counter

            /*
        TIM6.CR1 = CR1::RESET_VALUE;
//        TIM6.PSC = PSC::RESET_VALUE;
        TIM6.ARR = 12000 - 1;           // FIXME: parametric
//        TIM6.CCR1 = 12000 - 1;          // FIXME: parametric
//        TIM6.CCMR1 |= BV(CCMR1::OC1M_0) | BV(CCMR1::OC1M_1);
//        TIM6.CCER |= BV(CCER::CC1E);

//        TIM6.CR1 |= BV(CR1::ARPE);      // auto preload
        TIM6.CR1 |= BV(CR1::CEN);       // enable counter
      */
        rcc::RCC.APB1ENR |= BV(rcc::APB1ENR::TIM6EN);
    }

    static inline volatile bool event()
    {
        return (TIM6.SR & BV(SR::UIF)) != 0;
    }

    static inline volatile uint32_t count()
    {
        return TIM6.CNT;
    }

private:
};

}

}
