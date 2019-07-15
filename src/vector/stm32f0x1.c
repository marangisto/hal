void ISR_NMI(void) __attribute__ ((weak, alias("__nothing")));
void ISR_HARDFAULT(void) __attribute__ ((weak, alias("__nothing")));
void ISR_MEMMANAGE(void) __attribute__ ((weak, alias("__nothing")));
void ISR_BUSFAULT(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USAGEFAULT(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SVCALL(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DEBUG(void) __attribute__ ((weak, alias("__nothing")));
void ISR_PENDSV(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SYSTICK(void) __attribute__ ((weak, alias("__nothing")));
void ISR_WWDG(void) __attribute__ ((weak, alias("__nothing")));
void ISR_PVD(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RTC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FLASH(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RCC_CRS(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI0_1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI2_3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI4_15(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TSC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA1_CH1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA1_CH2_3_DMA2_CH1_2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA1_CH4_5_6_7_DMA2_CH3_4_5(void) __attribute__ ((weak, alias("__nothing")));
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

void (*vectors[])(void) __attribute__ ((section(".vectors"))) =
    { (void(*)(void)) &__estack           // -16: Initial stack pointer
    , ISR_RESET                           // -15: Reset [fixed]
    , ISR_NMI                             // -14: Non maskable interrupt [fixed]
    , ISR_HARDFAULT                       // -13: All class of fault [fixed]
    , ISR_MEMMANAGE                       // -12: Memory management [settable]
    , ISR_BUSFAULT                        // -11: Pre-fetch fault, memory access fault [settable]
    , ISR_USAGEFAULT                      // -10: Undefined instruction or illegal state [settable]
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , ISR_SVCALL                          // -5: System service call via SWI instruction [settable]
    , ISR_DEBUG                           // -4: Monitor Debug Monitor [settable]
    , 0x0
    , ISR_PENDSV                          // -2: Pendable request for system service [settable]
    , ISR_SYSTICK                         // -1: System tick timer [settable]
    , ISR_WWDG                            // 0: Window Watchdog interrupt
    , ISR_PVD                             // 1: PVD and VDDIO2 supply comparator interrupt
    , ISR_RTC                             // 2: RTC interrupts
    , ISR_FLASH                           // 3: Flash global interrupt
    , ISR_RCC_CRS                         // 4: RCC and CRS global interrupts
    , ISR_EXTI0_1                         // 5: EXTI Line[1:0] interrupts
    , ISR_EXTI2_3                         // 6: EXTI Line[3:2] interrupts
    , ISR_EXTI4_15                        // 7: EXTI Line15 and EXTI4 interrupts
    , ISR_TSC                             // 8: Touch sensing interrupt
    , ISR_DMA1_CH1                        // 9: DMA1 channel 1 interrupt
    , ISR_DMA1_CH2_3_DMA2_CH1_2           // 10: DMA1 channel 2 and 3 and DMA2 channel 1 and 2 interrupt
    , ISR_DMA1_CH4_5_6_7_DMA2_CH3_4_5     // 11: DMA1 channel 4, 5, 6 and 7 and DMA2 channel 3, 4 and 5 interrupts
    , ISR_ADC_COMP                        // 12: ADC and comparator interrupts
    , ISR_TIM1_BRK_UP_TRG_COM             // 13: TIM1 break, update, trigger and commutation interrupt
    , ISR_TIM1_CC                         // 14: TIM1 Capture Compare interrupt
    , ISR_TIM2                            // 15: TIM2 global interrupt
    , ISR_TIM3                            // 16: TIM3 global interrupt
    , ISR_TIM6_DAC                        // 17: TIM6 global interrupt and DAC underrun interrupt
    , ISR_TIM7                            // 18: TIM7 global interrupt
    , ISR_TIM14                           // 19: TIM14 global interrupt
    , ISR_TIM15                           // 20: TIM15 global interrupt
    , ISR_TIM16                           // 21: TIM16 global interrupt
    , ISR_TIM17                           // 22: TIM17 global interrupt
    , ISR_I2C1                            // 23: I2C1 global interrupt
    , ISR_I2C2                            // 24: I2C2 global interrupt
    , ISR_SPI1                            // 25: SPI1_global_interrupt
    , ISR_SPI2                            // 26: SPI2 global interrupt
    , ISR_USART1                          // 27: USART1 global interrupt
    , ISR_USART2                          // 28: USART2 global interrupt
    , ISR_USART3_4_5_6_7_8                // 29: USART3, USART4, USART5, USART6, USART7, USART8 global interrupt
    , ISR_CEC_CAN                         // 30: CEC and CAN global interrupt
    , ISR_USB                             // 31: USB global interrupt
    };

