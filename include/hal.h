#pragma once

#include <cstddef>

#if defined(STM32F051)
    #include <device/stm32f0x1.h>
//    #undef HAVE_PERIPHERAL_GPIOE    // should not be present
    namespace device = stm32f0x1;
#elif defined(STM32F103)
    #include <device/stm32f103.h>
    namespace device = stm32f103;
#elif defined(STM32F411)
    #include <device/stm32f411.h>
    namespace device = stm32f411;
#elif defined(STM32G070)
    #include <device/stm32g07x.h>
    namespace device = stm32g07x;
#elif defined(STM32G431)
    #include <device/stm32g431.h>
    namespace device = stm32g431xx;
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

class sys_clock
{
public:
    static void init();
    static uint32_t freq() { return m_freq; }

private:
    static uint32_t m_freq;
};

template<bool> struct is_in_range;

template<interrupt::interrupt_t POS, typename = is_in_range<true> >
struct nvic {};

#if defined(STM32F051) || defined(STM32G070)
template<interrupt::interrupt_t POS>
struct nvic<POS, is_in_range<(0 <= POS && POS < 32)> >
{
    static void enable() { device::NVIC.ISER |= 1 << POS; }
};

#elif defined(STM32F411) || defined(STM32G431)
template<interrupt::interrupt_t POS>
struct nvic<POS, is_in_range<(0 <= POS && POS < 32)> >
{
    static void enable() { device::NVIC.ISER0 |= 1 << POS; }
};

template<interrupt::interrupt_t POS>
struct nvic<POS, is_in_range<(32 <= POS && POS < 64)> >
{
    static void enable() { device::NVIC.ISER1 |= 1 << (POS - 32); }
};

template<interrupt::interrupt_t POS>
struct nvic<POS, is_in_range<(64 <= POS && POS < 96)> >
{
    static void enable() { device::NVIC.ISER2 |= 1 << (POS - 64); }
};
#endif

#if defined(STM32G431)
template<interrupt::interrupt_t POS>
struct nvic<POS, is_in_range<(96 <= POS && POS < 128)> >
{
    static void enable() { device::NVIC.ISER3 |= 1 << (POS - 96); }
};
#endif

struct critical_section_t
{
    critical_section_t() { interrupt::disable(); }
    ~critical_section_t() { interrupt::enable(); }
};

} // namespace hal

template<interrupt::interrupt_t> void handler();
