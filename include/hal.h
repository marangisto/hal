#pragma once

#include <cstddef>

#if defined(STM32F051)
    #include <device/stm32f0x1.h>
    #undef HAVE_PERIPHERAL_GPIOE    // should not be present
    namespace device = stm32f0x1;
    #include <peripheral/stm32f0.h>
#elif defined(STM32F411)
    #include <device/stm32f411.h>
    namespace device = stm32f411;
    #include <peripheral/stm32f4.h>
#elif defined(STM32G070)
    #include <device/stm32g07x.h>
    namespace device = stm32g07x;
    #include <peripheral/stm32g0.h>
#elif defined(STM32G431)
    #include <device/stm32g431.h>
    namespace device = stm32g431xx;
    #include <peripheral/stm32g4.h>
#else
    static_assert(false, "mcu not recognized");
#endif

namespace hal
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
    static void rcc_enable()
    {
        internal::peripheral_traits<T>::rcc_enable();
    }

    static void nvic_enable()
    {
        internal::peripheral_traits<T>::nvic_enable();
    }
};

}

