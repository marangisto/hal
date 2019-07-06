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

#if defined(STM32F051)
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
#elif defined (STM32F411)
void NMI_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void HardFault_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void MemManage_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void BusFault_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void UsageFault_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void SVCall_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void Debug_Monitor_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void PendSV_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void SysTick_HDLR(void) __attribute__ ((weak, alias("__nothing")));

void ISR_WWDG(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI16_PVD(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI21_TAMP_STAMP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI22_RTC_WKUP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FLASH(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RCC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI0(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA1_Stream0(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA1_Stream1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA1_Stream2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA1_Stream3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA1_Stream4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA1_Stream5(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA1_Stream6(void) __attribute__ ((weak, alias("__nothing")));
void ISR_ADC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI9_5(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_BRK_TIM9(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_UP_TIM10(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_TRG_COM_TIM11(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_CC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C1_EV(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C1_ER(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C2_EV(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C2_ER(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USART1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USART2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI15_10(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI17_RTC_Alarm(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI18_OTG_FS_WKUP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA1_Stream7(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SDIO(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM5(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_Stream0(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_Stream1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_Stream2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_Stream3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_Stream4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_OTG_FS(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_Stream5(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_Stream6(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_Stream7(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USART6(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C3_EV(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C3_ER(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FPU(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI5(void) __attribute__ ((weak, alias("__nothing")));

struct __vector_table
{
    uint32_t *sp;
    void (*sys[15])(void);
    void (*irq[64+32])(void);
};

struct __vector_table vectors __attribute__ ((section(".vectors"))) =
    { &__estack
    , { Reset_HDLR
      , NMI_HDLR
      , HardFault_HDLR
      , MemManage_HDLR
      , BusFault_HDLR
      , UsageFault_HDLR
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , SVCall_HDLR
      , Debug_Monitor_HDLR
      , 0x0
      , PendSV_HDLR
      , SysTick_HDLR
      }
    , { ISR_WWDG                // 0 Window Watchdog interrupt 0x0000 0040
      , ISR_EXTI16_PVD          // 1 EXTI Line 16 interrupt / PVD through EXTI line detection interrupt 0x0000 0044
      , ISR_EXTI21_TAMP_STAMP   // 2 EXTI Line 21 interrupt / Tamper and TimeStamp interrupts through the EXTI line 0x0000 0048
      , ISR_EXTI22_RTC_WKUP     // 3 EXTI Line 22 interrupt / RTC Wakeup interrupt through the EXTI line 0x0000 004C
      , ISR_FLASH               // 4 Flash global interrupt 0x0000 0050
      , ISR_RCC                 // 5 RCC global interrupt 0x0000 0054
      , ISR_EXTI0               // 6 EXTI Line0 interrupt 0x0000 0058
      , ISR_EXTI1               // 7 EXTI Line1 interrupt 0x0000 005C
      , ISR_EXTI2               // 8 EXTI Line2 interrupt 0x0000 0060
      , ISR_EXTI3               // 9 EXTI Line3 interrupt 0x0000 0064
      , ISR_EXTI4               // 10 EXTI Line4 interrupt 0x0000 0068
      , ISR_DMA1_Stream0        // 11 DMA1 Stream0 global interrupt 0x0000 006C
      , ISR_DMA1_Stream1        // 12 DMA1 Stream1 global interrupt 0x0000 0070
      , ISR_DMA1_Stream2        // 13 DMA1 Stream2 global interrupt 0x0000 0074
      , ISR_DMA1_Stream3        // 14 DMA1 Stream3 global interrup
      , ISR_DMA1_Stream4        // 15 DMA1 Stream4 global interrupt 0x0000 007C
      , ISR_DMA1_Stream5        // 16 DMA1 Stream5 global interrupt 0x0000 0080
      , ISR_DMA1_Stream6        // 17 DMA1 Stream6 global interrupt 0x0000 0084
      , ISR_ADC                 // 18 ADC1 global interrupts 0x0000 0088
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , ISR_EXTI9_5             // 23 EXTI Line[9:5] interrupts 0x0000 009C
      , ISR_TIM1_BRK_TIM9       // 24 TIM1 Break interrupt and TIM9 global interrupt 0x0000 00A0
      , ISR_TIM1_UP_TIM10       // 25 TIM1 Update interrupt and TIM10 global interrupt 0x0000 00A4
      , ISR_TIM1_TRG_COM_TIM11  // 26 TIM1 Trigger and Commutation interrupts and TIM11 global interrupt 0x0000 00A8
      , ISR_TIM1_CC             // 27 TIM1 Capture Compare interrupt 0x0000 00AC
      , ISR_TIM2                // 28 TIM2 global interrupt 0x0000 00B0
      , ISR_TIM3                // 29 TIM3 global interrupt 0x0000 00B4
      , ISR_TIM4                // 30 TIM4 global interrupt 0x0000 00B8
      , ISR_I2C1_EV             // 31 I2C1 event interrupt 0x0000 00BC
      , ISR_I2C1_ER             // 32 I2C1 error interrupt 0x0000 00C0
      , ISR_I2C2_EV             // 33 I2C2 event interrupt 0x0000 00C4
      , ISR_I2C2_ER             // 34 I2C2 error interrupt 0x0000 00C8
      , ISR_SPI1                // 35 SPI1 global interrupt 0x0000 00CC
      , ISR_SPI2                // 36 SPI2 global interrupt 0x0000 00D0
      , ISR_USART1              // 37 USART1 global interrupt 0x0000 00D4
      , ISR_USART2              // 38 USART2 global interrupt 0x0000 00D8
      , 0x0
      , ISR_EXTI15_10           // 40 EXTI Line[15:10] interrupts 0x0000 00E0
      , ISR_EXTI17_RTC_Alarm    // 41 EXTI Line 17 interrupt / RTC Alarms (A and B) through EXTI line interrupt 0x0000 00E4
      , ISR_EXTI18_OTG_FS_WKUP  // 42 EXTI Line 18 interrupt / USB On-The-Go FS Wakeup through EXTI line interrupt 0x0000 00E8
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , ISR_DMA1_Stream7        // 47 DMA1 Stream7 global interrupt 0x0000 00FC
      , 0x0
      , ISR_SDIO                // 49 SDIO global interrupt 0x0000 0104
      , ISR_TIM5                // 50 TIM5 global interrupt 0x0000 0108
      , ISR_SPI3                // 51 SPI3 global interrupt 0x0000 010C
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , ISR_DMA2_Stream0        // 56 DMA2 Stream0 global interrupt 0x0000 0120
      , ISR_DMA2_Stream1        // 57 DMA2 Stream1 global interrupt 0x0000 0124
      , ISR_DMA2_Stream2        // 58 DMA2 Stream2 global interrupt 0x0000 0128
      , ISR_DMA2_Stream3        // 59 DMA2 Stream3 global interrupt 0x0000 012C
      , ISR_DMA2_Stream4        // 60 DMA2 Stream4 global interrupt 0x0000 0130
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , ISR_OTG_FS              // 67 USB On The Go FS global interrupt 0x0000 014C
      , ISR_DMA2_Stream5        // 68 DMA2 Stream5 global interrupt 0x0000 0150
      , ISR_DMA2_Stream6        // 69 DMA2 Stream6 global interrupt 0x0000 0154
      , ISR_DMA2_Stream7        // 70 DMA2 Stream7 global interrupt 0x0000 0158
      , ISR_USART6              // 71 USART6 global interrupt 0x0000 015C
      , ISR_I2C3_EV             // 72 I2C3 event interrupt 0x0000 0160
      , ISR_I2C3_ER             // 73 I2C3 error interrupt 0x0000 0164
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , ISR_FPU                 // 81 FPU global interrupt 0x0000 0184
      , 0x0
      , 0x0
      , ISR_SPI4                // 84 SPI 4 global interrupt 0x0000 0190
      , ISR_SPI5                // 85 SPI 5 global interrupt 0x0000 0194
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      }
    };
#elif defined (STM32G070)
void NMI_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void HardFault_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void SVCall_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void PendSV_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void SysTick_HDLR(void) __attribute__ ((weak, alias("__nothing")));

void ISR_WWDG(void) __attribute__ ((weak, alias("__nothing")));
void ISR_PVD(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RTC_TAMP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FLASH(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RCC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI0_1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI2_3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI4_15(void) __attribute__ ((weak, alias("__nothing")));
void ISR_UCPD1_UCPD2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TSC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH2_3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH4_5_6_7_DMAMUX(void) __attribute__ ((weak, alias("__nothing")));
void ISR_ADC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_BRK_UP_TRG_COM(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_CC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM6(void) __attribute__ ((weak, alias("__nothing")));
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
void ISR_USART3_4_LPUART1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_CEC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_AES_RNG(void) __attribute__ ((weak, alias("__nothing")));

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
      , ISR_PVD                 // not available on G0x0
      , ISR_RTC_TAMP
      , ISR_FLASH
      , ISR_RCC
      , ISR_EXTI0_1
      , ISR_EXTI2_3
      , ISR_EXTI4_15
      , ISR_UCPD1_UCPD2         // not available on G0x0
      , ISR_DMA_CH1
      , ISR_DMA_CH2_3
      , ISR_DMA_CH4_5_6_7_DMAMUX
      , ISR_ADC
      , ISR_TIM1_BRK_UP_TRG_COM
      , ISR_TIM1_CC
      , ISR_TIM2                // not available on G0x0
      , ISR_TIM3
      , ISR_TIM6
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
      , ISR_USART3_4_LPUART1    // LPUART1 not available on G0x0
      , ISR_CEC                 // not available on G0x0
      , ISR_AES_RNG             // not available on G0x0
      }
    };
#elif defined (STM32G431)
void NMI_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void HardFault_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void MemManage_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void BusFault_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void UsageFault_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void SVCall_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void Debug_Monitor_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void PendSV_HDLR(void) __attribute__ ((weak, alias("__nothing")));
void SysTick_HDLR(void) __attribute__ ((weak, alias("__nothing")));

void ISR_WWDG(void) __attribute__ ((weak, alias("__nothing")));
void ISR_PVD_PVM(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RTC_TAMP_CSS_LSE(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RTC_WKUP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FLASH(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RCC_(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI0(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH5(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH6(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CH7(void) __attribute__ ((weak, alias("__nothing")));
void ISR_ADC1_2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USP_HP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USP_LP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FDCAN1_INTR1_IT(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FDCAN1_INTR0_IT(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI9_5(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_BRK_TIM15(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_UP_TIM16(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_TRG_COM_TIM17(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_CC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C1_EV(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C1_ER(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C2_EV(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C2_ER(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USART1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USART2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USART3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI15_10(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RTC_ALARM(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USB_WAKE_UP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM8_BRK_TERR_IERR(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM8_TRG_COM_DIR_IDX(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM8_UP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM8_CC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_ADC3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FMC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_LPTIM1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM5(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_UART4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_UART5(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM6_DAC1_3_UNDER(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM7_DAC2_4_UNDER(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_CH1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_CH2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_CH3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_CH4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_CH5(void) __attribute__ ((weak, alias("__nothing")));
void ISR_ADC4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_ADC5(void) __attribute__ ((weak, alias("__nothing")));
void ISR_UCPD1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_COMP1_2_3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_COMP4_5_6(void) __attribute__ ((weak, alias("__nothing")));
void ISR_COMP7(void) __attribute__ ((weak, alias("__nothing")));
void ISR_HRTIM_MASTER_IRQN(void) __attribute__ ((weak, alias("__nothing")));
void ISR_HRTIM_TIMA_IRQN(void) __attribute__ ((weak, alias("__nothing")));
void ISR_HRTIM_TIMB_IRQN(void) __attribute__ ((weak, alias("__nothing")));
void ISR_HRTIM_TIMC_IRQN(void) __attribute__ ((weak, alias("__nothing")));
void ISR_HRTIM_TIMD_IRQN(void) __attribute__ ((weak, alias("__nothing")));
void ISR_HRTIM_TIME_IRQN(void) __attribute__ ((weak, alias("__nothing")));
void ISR_HRTIM_TIM_FLT_IRQN(void) __attribute__ ((weak, alias("__nothing")));
void ISR_HRTIM_TIMF_IRQN(void) __attribute__ ((weak, alias("__nothing")));
void ISR_CRS(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SAI(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM20_BRK_TERR_IERR(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM20_UP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM20_TRG_COM_DIR_IDX(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM20_CC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FPU(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C4_EV(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C4_ER(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_AES(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FDCAN2_INTR0(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FDCAN2_INTR1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FDCAN3_INTR0(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FDCAN3_INTR1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RNG(void) __attribute__ ((weak, alias("__nothing")));
void ISR_LPUART(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C3_EV(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C3_ER(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMAMUX_OVR(void) __attribute__ ((weak, alias("__nothing")));
void ISR_QUADSPI(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA1_CH8(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_CH6(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_CH7(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA2_CH8(void) __attribute__ ((weak, alias("__nothing")));
void ISR_CORDIC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FMAC(void) __attribute__ ((weak, alias("__nothing")));

struct __vector_table
  {
      uint32_t *sp;
      void (*sys[15])(void);
      void (*irq[102])(void);
  };

  struct __vector_table vectors __attribute__ ((section(".vectors"))) =
      { &__estack
      , { Reset_HDLR
        , NMI_HDLR
        , HardFault_HDLR
        , MemManage_HDLR
        , BusFault_HDLR
        , UsageFault_HDLR
        , 0x0
        , 0x0
        , 0x0
        , 0x0
        , SVCall_HDLR
        , Debug_Monitor_HDLR
        , 0x0
        , PendSV_HDLR
        , SysTick_HDLR
        }
      , { ISR_WWDG
        , ISR_PVD_PVM
        , ISR_RTC_TAMP_CSS_LSE
        , ISR_RTC_WKUP
        , ISR_FLASH
        , ISR_RCC_
        , ISR_EXTI0
        , ISR_EXTI1
        , ISR_EXTI2
        , ISR_EXTI3
        , ISR_EXTI4
        , ISR_DMA_CH1
        , ISR_DMA_CH2
        , ISR_DMA_CH3
        , ISR_DMA_CH4
        , ISR_DMA_CH5
        , ISR_DMA_CH6
        , ISR_DMA_CH7
        , ISR_ADC1_2
        , ISR_USP_HP
        , ISR_USP_LP
        , ISR_FDCAN1_INTR1_IT
        , ISR_FDCAN1_INTR0_IT
        , ISR_EXTI9_5
        , ISR_TIM1_BRK_TIM15
        , ISR_TIM1_UP_TIM16
        , ISR_TIM1_TRG_COM_TIM17
        , ISR_TIM1_CC
        , ISR_TIM2
        , ISR_TIM3
        , ISR_TIM4
        , ISR_I2C1_EV
        , ISR_I2C1_ER
        , ISR_I2C2_EV
        , ISR_I2C2_ER
        , ISR_SPI1
        , ISR_SPI2
        , ISR_USART1
        , ISR_USART2
        , ISR_USART3
        , ISR_EXTI15_10
        , ISR_RTC_ALARM
        , ISR_USB_WAKE_UP
        , ISR_TIM8_BRK_TERR_IERR
        , ISR_TIM8_TRG_COM_DIR_IDX
        , ISR_TIM8_UP
        , ISR_TIM8_CC
        , ISR_ADC3
        , ISR_FMC
        , ISR_LPTIM1
        , ISR_TIM5
        , ISR_SPI3
        , ISR_UART4
        , ISR_UART5
        , ISR_TIM6_DAC1_3_UNDER
        , ISR_TIM7_DAC2_4_UNDER
        , ISR_DMA2_CH1
        , ISR_DMA2_CH2
        , ISR_DMA2_CH3
        , ISR_DMA2_CH4
        , ISR_DMA2_CH5
        , ISR_ADC4
        , ISR_ADC5
        , ISR_UCPD1
        , ISR_COMP1_2_3
        , ISR_COMP4_5_6
        , ISR_COMP7
        , ISR_HRTIM_MASTER_IRQN
        , ISR_HRTIM_TIMA_IRQN
        , ISR_HRTIM_TIMB_IRQN
        , ISR_HRTIM_TIMC_IRQN
        , ISR_HRTIM_TIMD_IRQN
        , ISR_HRTIM_TIME_IRQN
        , ISR_HRTIM_TIM_FLT_IRQN
        , ISR_HRTIM_TIMF_IRQN
        , ISR_CRS
        , ISR_SAI
        , ISR_TIM20_BRK_TERR_IERR
        , ISR_TIM20_UP
        , ISR_TIM20_TRG_COM_DIR_IDX
        , ISR_TIM20_CC
        , ISR_FPU
        , ISR_I2C4_EV
        , ISR_I2C4_ER
        , ISR_SPI4
        , ISR_AES
        , ISR_FDCAN2_INTR0
        , ISR_FDCAN2_INTR1
        , ISR_FDCAN3_INTR0
        , ISR_FDCAN3_INTR1
        , ISR_RNG
        , ISR_LPUART
        , ISR_I2C3_EV
        , ISR_I2C3_ER
        , ISR_DMAMUX_OVR
        , ISR_QUADSPI
        , ISR_DMA1_CH8
        , ISR_DMA2_CH6
        , ISR_DMA2_CH7
        , ISR_DMA2_CH8
        , ISR_CORDIC
        , ISR_FMAC
        }
      };
#else
    _Static_assert (0, "no startup sequence for MCU");
#endif

