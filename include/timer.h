#pragma once

#include <stm32f0.h>

namespace stm32f0
{

namespace timer
{

using namespace device;

template<int TN>
class timer_t
{
public:
    static inline void setup()
    {
    }

    static inline volatile bool event()
    {
        return (TIM6.SR & BV(tim6_t::SR_UIF)) != 0;
    }

    static inline volatile uint32_t count()
    {
        return TIM6.CNT;
    }

private:
};

}

}
