#include <stm32f0x1.h>

using namespace stm32f0x1;
using namespace gpioc;
using namespace rcc;

int main()
{
    RCC.AHBENR |= BV(AHBENR::IOPCEN);       // IOPCEN - enable clock on GPIOC
    GPIOC.MODER |= BV(MODER::MODER8) | BV(MODER::MODER9); // bit-0 output mode

    for (bool on = true; true; on = !on)
    {
        (on ? GPIOC.BSRR : GPIOC.BRR) = 0x0300;
        sys_tick::delay_ms(500);
    }
}

