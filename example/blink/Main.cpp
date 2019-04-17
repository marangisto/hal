#include <stm32f0x1.h>

using namespace stm32f0x1;
using namespace gpioc;
using namespace rcc;

static void delay_ms(uint32_t x)
{
    for (uint32_t j = 0; j < x * 300; ++j)
        GPIOC.BRR = 0x0000;        // forces evaluation but does nothing!
}

int main()
{
    RCC.AHBENR |= BV(AHBENR::IOPCEN);       // IOPCEN - enable clock on GPIOC
    GPIOC.MODER |= BV(MODER::MODER8) | BV(MODER::MODER9); // bit-0 output mode

    bool on = true;

    for (;;)
    {
        delay_ms(500);
        if (on)
            GPIOC.BSRR = 0x0300;
        else
            GPIOC.BRR = 0x0300;
        on = !on;
    }
}

