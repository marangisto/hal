#include <stm32f0x1.h>

using namespace stm32f0x1;
using namespace rcc;

extern "C" void system_init(void)
{
    // reset clock control registers
    RCC.CR = CR::RESET_VALUE;
    RCC.CFGR = CFGR::RESET_VALUE;
    RCC.CFGR2 = CFGR2::RESET_VALUE;
    RCC.CFGR3 = CFGR3::RESET_VALUE;
    RCC.CR2 = CR2::RESET_VALUE;
    RCC.CIR = CIR::RESET_VALUE;
}

