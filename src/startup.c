#include <stdint.h>

extern uint32_t __sbss, __ebss;
extern uint32_t __sdata, __edata;
extern uint32_t __sidata;
extern uint32_t __estack;

extern void system_init(void);
extern void main(void);

__attribute__ ((section(".text"))) void Reset_HDLR(void)
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

void NMI_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void HardFault_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void SVCall_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void PendSV_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void SysTick_HDLR(void) __attribute__ ((weak, alias("__nothing")));

void ISR_WWDG(void) __attribute__ ((weak, alias("__nothing")));
void ISR_PVD_VDDIO2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RTC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FLASH(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RCC_CRS(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI0_1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI2_3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI4_15(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TSC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH2_3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH4_5_6_7(void) __attribute__ ((weak, alias("__nothing")));
void ISR_ADC_COMP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_BRK_UP_TRG_COM(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_CC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM6_DAC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM7(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM14(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM15(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM16(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM17(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USART1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USART2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USART3_4_5_6_7_8(void) __attribute__ ((weak, alias("__nothing")));
void ISR_CEC_CAN(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USB(void) __attribute__ ((weak, alias("__nothing")));

struct __vector_table
{
    uint32_t *sp;
    void (*sys[15])(void);
    void (*irq[32])(void);
};

struct __vector_table vectors __attribute__ ((section(".vectors"))) =
    { &__estack
    , { Reset_HDLR
      , NMI_HDLR
      , HardFault_HDLR
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , SVCall_HDLR
      , 0x0
      , 0x0
      , PendSV_HDLR
      , SysTick_HDLR
      }
    , { ISR_WWDG
      , ISR_PVD_VDDIO2
      , ISR_RTC
      , ISR_FLASH
      , ISR_RCC_CRS
      , ISR_EXTI0_1
      , ISR_EXTI2_3
      , ISR_EXTI4_15
      , ISR_TSC
      , ISR_DMA_CH1
      , ISR_DMA_CH2_3
      , ISR_DMA_CH4_5_6_7
      , ISR_ADC_COMP
      , ISR_TIM1_BRK_UP_TRG_COM
      , ISR_TIM1_CC
      , ISR_TIM2
      , ISR_TIM3
      , ISR_TIM6_DAC
      , ISR_TIM7
      , ISR_TIM14
      , ISR_TIM15
      , ISR_TIM16
      , ISR_TIM17
      , ISR_I2C1
      , ISR_I2C2
      , ISR_SPI1
      , ISR_SPI2
      , ISR_USART1
      , ISR_USART2
      , ISR_USART3_4_5_6_7_8
      , ISR_CEC_CAN
      , ISR_USB
      }
    };

