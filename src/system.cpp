#include <stm32f0.h>

using namespace stm32f0x1;

namespace stm32f0
{

void sys_tick::delay_ms(uint32_t ms)
{
    uint32_t now = ms_counter, then = now + ms;

    while (ms_counter >= now && ms_counter < then)
        ;   // busy wait
}

void sys_tick::init(uint32_t n)
{
    using namespace stm32f0;
    typedef stk_t _;

    ms_counter = 0;                                     // start new epoq
    STK.CSR = _::CSR_RESET_VALUE;                       // reset controls
    STK.RVR = n - 1;                                    // reload value
    STK.CVR = _::CVR_RESET_VALUE;                       // current counter value
    STK.CSR |= BV(_::CSR_CLKSOURCE);                    // systick clock source
    STK.CSR |= BV(_::CSR_ENABLE) | BV(_::CSR_TICKINT);  // enable counter & interrupts
}

volatile uint32_t sys_tick::ms_counter = 0;

inline void sys_tick_init(uint32_t n) { sys_tick::init(n); }
inline void sys_tick_update() { ++sys_tick::ms_counter; } // N.B. wraps in 49 days!
}

extern "C" void SysTick_HDLR()
{
    stm32f0::sys_tick_update();
}

extern "C" void system_init(void)
{
    using namespace stm32f0;
    typedef rcc_t _;

    // reset clock control registers

    RCC.CR = _::CR_RESET_VALUE;
    RCC.CFGR = _::CFGR_RESET_VALUE;
    RCC.CFGR2 = _::CFGR2_RESET_VALUE;
    RCC.CFGR3 = _::CFGR3_RESET_VALUE;
    RCC.CR2 = _::CR2_RESET_VALUE;
    RCC.CIR = _::CIR_RESET_VALUE;

    // set system clock to HSI-PLL 48MHz

    Flash.ACR = BV(flash_t::ACR_PRFTBE) | BV(flash_t::ACR_LATENCY);

    RCC.CFGR |= (0xa << _::CFGR_PLLMUL);                // PLL multiplier 12
    RCC.CR |= BV(_::CR_PLLON);                          // enable PLL
    while (!(RCC.CR & _::CR_PLLRDY));                   // wait for PLL to be ready
    RCC.CFGR |= (0x2 << _::CFGR_SW);                    // select PLL as system clock
    while (((RCC.CFGR >> _::CFGR_SWS) & 0x3) != 0x2);   // wait for PLL as system clock

    // initialize sys-tick for milli-second counts

    stm32f0::sys_tick_init(60000);
}

