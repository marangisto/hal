#include <stm32f0x1.h>

using namespace stm32f0x1;

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
}

