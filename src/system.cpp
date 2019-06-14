#include <system.h>

namespace system
{

using namespace device;

void sys_tick::delay_ms(uint32_t ms)
{
    uint32_t now = ms_counter, then = now + ms;

    while (ms_counter >= now && ms_counter < then)
        ;   // busy wait
}

void sys_tick::init(uint32_t n)
{
    using namespace system;
    typedef stk_t _;

    ms_counter = 0;                             // start new epoq
    STK.CSR = _::CSR_RESET_VALUE;               // reset controls
    STK.RVR = n - 1;                            // reload value
    STK.CVR = _::CVR_RESET_VALUE;               // current counter value
    STK.CSR |= _::CSR_CLKSOURCE;                // systick clock source
    STK.CSR |= _::CSR_ENABLE | _::CSR_TICKINT;  // enable counter & interrupts
}

volatile uint32_t sys_tick::ms_counter = 0;

inline void sys_tick_init(uint32_t n) { sys_tick::init(n); }
inline void sys_tick_update() { ++sys_tick::ms_counter; } // N.B. wraps in 49 days!
}

extern "C" void SysTick_HDLR()
{
    system::sys_tick_update();
}

extern "C" void system_init(void)
{
    using namespace system;
    typedef rcc_t _;

    // reset clock control registers

    RCC.CR = _::CR_RESET_VALUE;
    RCC.CFGR = _::CFGR_RESET_VALUE;
    RCC.CFGR2 = _::CFGR2_RESET_VALUE;
    RCC.CFGR3 = _::CFGR3_RESET_VALUE;
    RCC.CR2 = _::CR2_RESET_VALUE;
    RCC.CIR = _::CIR_RESET_VALUE;

    // set system clock to HSI-PLL 48MHz

    Flash.ACR = flash_t::ACR_PRFTBE | flash_t::ACR_LATENCY<0x1>;

    RCC.CFGR |= _::CFGR_PLLMUL<0xa>;        // PLL multiplier 12
    RCC.CR |= _::CR_PLLON;                  // enable PLL
    while (!(RCC.CR & _::CR_PLLRDY));       // wait for PLL to be ready
    RCC.CFGR |= _::CFGR_SW<0x2>;            // select PLL as system clock

    // wait for PLL as system clock

    while ((RCC.CFGR & _::CFGR_SWS<0x3>) != _::CFGR_SWS<0x2>);

    // initialize sys-tick for milli-second counts

    system::sys_tick_init(60000);
}

