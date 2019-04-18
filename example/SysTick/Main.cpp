#include <stm32f0x1.h>

using namespace stm32f0x1;
using namespace gpioc;

int main()
{
    rcc::RCC.AHBENR |= BV(rcc::AHBENR::IOPCEN);       // IOPCEN - enable clock on GPIOC
    GPIOC.MODER |= BV(MODER::MODER8) | BV(MODER::MODER9); // bit-0 output mode

    for (bool on = true; true; on = !on)
    {
        static uint32_t next_a = 0, next_b = 0;
        static bool on_a = false, on_b = false;

        uint32_t now = sys_tick::count();

        if (now >= next_a)
        {
            (on_a ? GPIOC.BSRR : GPIOC.BRR) = 0x0100;
            on_a = !on_a;
            next_a = now + 101;
        }

        if (now >= next_b)
        {
            (on_b ? GPIOC.BSRR : GPIOC.BRR) = 0x0200;
            on_b = !on_b;
            next_b = now + 103;
        }
    }
}

