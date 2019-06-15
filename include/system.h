#pragma once

#include <cstddef>

#if defined(STM32F0x1)
#include <device/stm32f0x1.h>
namespace device = stm32f0x1;
#include <peripheral/stm32f0.h>
#endif

namespace system
{

class sys_tick
{
public:
    static void delay_ms(uint32_t ms);
    static inline uint32_t count() { return ms_counter; }

private:
    friend inline void sys_tick_init(uint32_t n);
    friend inline void sys_tick_update();

    static void init(uint32_t reload);
    static inline void update();

    static volatile uint32_t ms_counter;
};

struct interrupt
{
    static inline void enable() { __asm volatile ("cpsie i"); }
    static inline void disable() { __asm volatile ("cpsid i"); }
};

template<typename T>
struct peripheral
{
    static void nvic_enable()
    {
        internal::peripheral_traits<T>::nvic_enable();
    }
};

}

