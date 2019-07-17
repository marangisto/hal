////
//
//        STM32F103 vectors
//
////

extern void ISR_NMI(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_HARDFAULT(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_MEMMANAGE(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_BUSFAULT(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_USAGEFAULT(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_SVCALL(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_DEBUG(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_PENDSV(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_SYSTICK(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_WWDG(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_PVD(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TAMPER(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_RTC(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_FLASH(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_RCC(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_EXTI0(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_EXTI1(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_EXTI2(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_EXTI3(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_EXTI4(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_DMA1_CHANNEL1(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_DMA1_CHANNEL2(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_DMA1_CHANNEL3(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_DMA1_CHANNEL4(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_DMA1_CHANNEL5(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_DMA1_CHANNEL6(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_DMA1_CHANNEL7(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_ADC1_2(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_USB_HP_CAN_TX(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_USB_LP_CAN_RX0(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_CAN_RX1(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_CAN_SCE(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_EXTI9_5(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM1_BRK(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM1_UP(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM1_TRG_COM(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM1_CC(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM2(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM3(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM4(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_I2C1_EV(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_I2C1_ER(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_I2C2_EV(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_I2C2_ER(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_SPI1(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_SPI2(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_USART1(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_USART2(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_USART3(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_EXTI15_10(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_RTCALARM(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM8_BRK(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM8_UP(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM8_TRG_COM(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM8_CC(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_ADC3(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_FSMC(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_SDIO(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM5(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_SPI3(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_UART4(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_UART5(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM6(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_TIM7(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_DMA2_CHANNEL1(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_DMA2_CHANNEL2(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_DMA2_CHANNEL3(void) __attribute__ ((weak, alias("_Z9__nothingv")));
extern void ISR_DMA2_CHANNEL4_5(void) __attribute__ ((weak, alias("_Z9__nothingv")));

void (*vectors[])(void) __attribute__ ((section(".vectors"))) =
    { (void(*)(void)) &__estack     // -16: Initial stack pointer
    , ISR_RESET                     // -15: Reset [fixed]
    , ISR_NMI                       // -14: Non maskable interrupt [fixed]
    , ISR_HARDFAULT                 // -13: All class of fault [fixed]
    , ISR_MEMMANAGE                 // -12: Memory management [settable]
    , ISR_BUSFAULT                  // -11: Pre-fetch fault, memory access fault [settable]
    , ISR_USAGEFAULT                // -10: Undefined instruction or illegal state [settable]
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , ISR_SVCALL                    // -5: System service call via SWI instruction [settable]
    , ISR_DEBUG                     // -4: Monitor Debug Monitor [settable]
    , 0x0
    , ISR_PENDSV                    // -2: Pendable request for system service [settable]
    , ISR_SYSTICK                   // -1: System tick timer [settable]
    , ISR_WWDG                      // 0: Window Watchdog interrupt
    , ISR_PVD                       // 1: PVD through EXTI line detection interrupt
    , ISR_TAMPER                    // 2: Tamper interrupt
    , ISR_RTC                       // 3: RTC global interrupt
    , ISR_FLASH                     // 4: Flash global interrupt
    , ISR_RCC                       // 5: RCC global interrupt
    , ISR_EXTI0                     // 6: EXTI Line0 interrupt
    , ISR_EXTI1                     // 7: EXTI Line1 interrupt
    , ISR_EXTI2                     // 8: EXTI Line2 interrupt
    , ISR_EXTI3                     // 9: EXTI Line3 interrupt
    , ISR_EXTI4                     // 10: EXTI Line4 interrupt
    , ISR_DMA1_CHANNEL1             // 11: DMA1 Channel1 global interrupt
    , ISR_DMA1_CHANNEL2             // 12: DMA1 Channel2 global interrupt
    , ISR_DMA1_CHANNEL3             // 13: DMA1 Channel3 global interrupt
    , ISR_DMA1_CHANNEL4             // 14: DMA1 Channel4 global interrupt
    , ISR_DMA1_CHANNEL5             // 15: DMA1 Channel5 global interrupt
    , ISR_DMA1_CHANNEL6             // 16: DMA1 Channel6 global interrupt
    , ISR_DMA1_CHANNEL7             // 17: DMA1 Channel7 global interrupt
    , ISR_ADC1_2                    // 18: ADC1 and ADC2 global interrupt
    , ISR_USB_HP_CAN_TX             // 19: USB High Priority or CAN TX interrupts
    , ISR_USB_LP_CAN_RX0            // 20: USB Low Priority or CAN RX0 interrupts
    , ISR_CAN_RX1                   // 21: CAN RX1 interrupt
    , ISR_CAN_SCE                   // 22: CAN SCE interrupt
    , ISR_EXTI9_5                   // 23: EXTI Line[9:5] interrupts
    , ISR_TIM1_BRK                  // 24: TIM1 Break interrupt
    , ISR_TIM1_UP                   // 25: TIM1 Update interrupt
    , ISR_TIM1_TRG_COM              // 26: TIM1 Trigger and Commutation interrupts
    , ISR_TIM1_CC                   // 27: TIM1 Capture Compare interrupt
    , ISR_TIM2                      // 28: TIM2 global interrupt
    , ISR_TIM3                      // 29: TIM3 global interrupt
    , ISR_TIM4                      // 30: TIM4 global interrupt
    , ISR_I2C1_EV                   // 31: I2C1 event interrupt
    , ISR_I2C1_ER                   // 32: I2C1 error interrupt
    , ISR_I2C2_EV                   // 33: I2C2 event interrupt
    , ISR_I2C2_ER                   // 34: I2C2 error interrupt
    , ISR_SPI1                      // 35: SPI1 global interrupt
    , ISR_SPI2                      // 36: SPI2 global interrupt
    , ISR_USART1                    // 37: USART1 global interrupt
    , ISR_USART2                    // 38: USART2 global interrupt
    , ISR_USART3                    // 39: USART3 global interrupt
    , ISR_EXTI15_10                 // 40: EXTI Line[15:10] interrupts
    , ISR_RTCALARM                  // 41: RTC Alarms through EXTI line interrupt
    , 0x0
    , ISR_TIM8_BRK                  // 43: TIM8 Break interrupt
    , ISR_TIM8_UP                   // 44: TIM8 Update interrupt
    , ISR_TIM8_TRG_COM              // 45: TIM8 Trigger and Commutation interrupts
    , ISR_TIM8_CC                   // 46: TIM8 Capture Compare interrupt
    , ISR_ADC3                      // 47: ADC3 global interrupt
    , ISR_FSMC                      // 48: FSMC global interrupt
    , ISR_SDIO                      // 49: SDIO global interrupt
    , ISR_TIM5                      // 50: TIM5 global interrupt
    , ISR_SPI3                      // 51: SPI3 global interrupt
    , ISR_UART4                     // 52: UART4 global interrupt
    , ISR_UART5                     // 53: UART5 global interrupt
    , ISR_TIM6                      // 54: TIM6 global interrupt
    , ISR_TIM7                      // 55: TIM7 global interrupt
    , ISR_DMA2_CHANNEL1             // 56: DMA2 Channel1 global interrupt
    , ISR_DMA2_CHANNEL2             // 57: DMA2 Channel2 global interrupt
    , ISR_DMA2_CHANNEL3             // 58: DMA2 Channel3 global interrupt
    , ISR_DMA2_CHANNEL4_5           // 59: DMA2 Channel4 and DMA2 Channel5 global interrupt
    };

