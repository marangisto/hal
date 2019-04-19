#pragma once

#include <cstddef>

#if defined(STM32F0x1)
#include <stm32f0x1.h>
namespace device = stm32f0x1;
#endif

namespace stm32f0
{

template<int N> class reserved_t { private: uint32_t m_pad[N]; };

static constexpr uint32_t BV(uint8_t x) { return 1 << x; }

static inline void cpsid(void) { __asm volatile ("cpsid i"); }
static inline void cpsie(void) { __asm volatile ("cpsie i"); }

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

}

