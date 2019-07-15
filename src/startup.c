#include <stdint.h>

extern uint32_t __sbss, __ebss;
extern uint32_t __sdata, __edata;
extern uint32_t __sidata;
extern uint32_t __estack;

extern void system_init(void);
extern void main(void);

__attribute__ ((section(".text"))) void ISR_RESET(void)
{
    uint32_t *bss = &__sbss;
    uint32_t *data = &__sdata;
    uint32_t *idata = &__sidata;

    while (data < &__edata)
        *data++ = *idata++;

    while (bss < &__ebss)
        *bss++ = 0;

    system_init();

    main();

    while (1)
        ;
}

__attribute__ ((section(".text"), optimize("-O3"))) void __nothing(void) {}

#if defined(STM32F051)
#include "vector/stm32f0x1.c"
#elif defined (STM32F103)
#include "vector/stm32f103.c"
#elif defined (STM32F411)
#include "vector/stm32f411.c"
#elif defined (STM32F412)
#include "vector/stm32f412.c"
#elif defined (STM32G070)
#include "vector/stm32g07x.c"
#elif defined (STM32G431)
#include "vector/stm32g431.c"
#else
    _Static_assert (0, "no startup sequence for MCU");
#endif

