#include <stm32f0x1.h>

using namespace stm32f0x1;

inline void sys_tick_init(uint32_t n) { sys_tick::init(n); }
inline void sys_tick_update() { ++sys_tick::ms_counter; } // N.B. wraps in 49 days!

extern "C" void system_init(void)
{
    using namespace rcc;

    // reset clock control registers

    RCC.CR = CR::RESET_VALUE;
    RCC.CFGR = CFGR::RESET_VALUE;
    RCC.CFGR2 = CFGR2::RESET_VALUE;
    RCC.CFGR3 = CFGR3::RESET_VALUE;
    RCC.CR2 = CR2::RESET_VALUE;
    RCC.CIR = CIR::RESET_VALUE;

    // set system clock to HSI-PLL 48MHz

    flash::Flash.ACR = BV(flash::ACR::PRFTBE) | BV(flash::ACR::LATENCY);

    RCC.CFGR |= (0xa << CFGR::PLLMUL);              // PLL multiplier 12
    RCC.CR |= BV(CR::PLLON);                        // enable PLL
    while (!(RCC.CR & CR::PLLRDY));                 // wait for PLL to be ready
    RCC.CFGR |= (0x2 << CFGR::SW);                  // select PLL as system clock
    while (((RCC.CFGR >> CFGR::SWS) & 0x3) != 0x2); // wait for PLL as system clock

    // initialize sys-tick for milli-second counts

    sys_tick_init(60000);
}

void sys_tick::delay_ms(uint32_t ms)
{
    uint32_t now = ms_counter, then = now + ms;

    while (ms_counter >= now && ms_counter < then)
        ;   // busy wait
}

void sys_tick::init(uint32_t n)
{
    using namespace stk;

    ms_counter = 0;                                 // start new epoq
    STK.CSR = CSR::RESET_VALUE;                     // reset controls
    STK.RVR = n - 1;                                // reload value
    STK.CVR = CVR::RESET_VALUE;                     // current counter value
    STK.CSR |= BV(CSR::CLKSOURCE);                  // systick clock source
    STK.CSR |= BV(CSR::ENABLE) | BV(CSR::TICKINT);  // enable counter & interrupts
}

volatile uint32_t sys_tick::ms_counter = 0;

extern "C" void SysTick_HDLR()
{
   sys_tick_update();
}

