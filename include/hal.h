#pragma once

#include <cstddef>

#if defined(STM32F051)
    #include <device/stm32f0x1.h>
    #undef HAVE_PERIPHERAL_USART3
    namespace device = stm32f0x1;
#elif defined(STM32F103)
    #include <device/stm32f103.h>
    #undef HAVE_PERIPHERAL_SPI3
    namespace device = stm32f103;
#elif defined(STM32F411)
    #include <device/stm32f411.h>
    namespace device = stm32f411;
#elif defined(STM32F767)
    #include <device/stm32f7x7.h>
    namespace device = stm32f7x7;
#elif defined(STM32H743)
    #include <device/stm32h7x3.h>
    namespace device = stm32h7x3;
#elif defined(STM32G070)
    #include <device/stm32g07x.h>
    #undef HAVE_PERIPHERAL_TIM2
    #undef HAVE_PERIPHERAL_SPI3
    namespace device = stm32g07x;
#elif defined(STM32G431)
    #include <device/stm32g431.h>
    namespace device = stm32g431xx;
#else
    static_assert(false, "mcu not recognized");
#endif

#if defined(USE_HAL_DRIVER) || defined(USE_FULL_LL_DRIVER)
extern "C" uint32_t SystemCoreClock;
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

    __attribute__((always_inline))
    static inline void delay_us(uint32_t us)
    {
        volatile uint32_t n = (us * (m_freq / 100000)) / 250;

        while (n > 0)
            --n;
    }

#if defined(USE_HAL_DRIVER) || defined(USE_FULL_LL_DRIVER)
    static void copy_system_core_clock()
    {
        m_freq = SystemCoreClock;
    }
#endif

private:
    static uint32_t m_freq;
};

template<bool> struct is_in_range;

template<typename T>
struct always_false_t
{
    static const bool value = false;
};

template<int I>
struct always_false_i
{
    static const bool value = false;
};

enum channel_t
    { CH1
    , CH2
    , CH3
    , CH4
    };

template<interrupt::interrupt_t POS, typename = is_in_range<true> >
struct nvic
{
    static_assert(always_false_i<POS>::value, "nvic capability undefined for mcu");
};

#if defined(STM32F051) || defined(STM32G070)
template<interrupt::interrupt_t POS>
struct nvic<POS, is_in_range<(0 <= POS && POS < 32)> >
{
    static void enable() { device::NVIC.ISER |= 1 << POS; }
};
#else
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
#endif

#if defined(STM32F411) || defined(STM32G431)
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
