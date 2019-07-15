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
void ISR_RTC_STAMP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FLASH(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RCC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI0_1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI2_3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI4_15(void) __attribute__ ((weak, alias("__nothing")));
void ISR_UCPD1_UCPD2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CHANNEL1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CHANNEL2_3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DMA_CHANNEL4_5_6_7(void) __attribute__ ((weak, alias("__nothing")));
void ISR_ADC_COMP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_BRK_UP_TRG_COMP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_CC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM6_DAC_LPTIM1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM7_LPTIM2(void) __attribute__ ((weak, alias("__nothing")));
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
void ISR_USART3_USART4_LPUART1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_CEC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_AES_RNG(void) __attribute__ ((weak, alias("__nothing")));

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
    , ISR_WWDG                      // 0: Window watchdog interrupt
    , ISR_PVD                       // 1: Power voltage detector interrupt
    , ISR_RTC_STAMP                 // 2: RTC and TAMP interrupts
    , ISR_FLASH                     // 3: Flash global interrupt
    , ISR_RCC                       // 4: RCC global interrupt
    , ISR_EXTI0_1                   // 5: EXTI line 0 &amp; 1 interrupt
    , ISR_EXTI2_3                   // 6: EXTI line 2 &amp; 3 interrupt
    , ISR_EXTI4_15                  // 7: EXTI line 4 to 15 interrupt
    , ISR_UCPD1_UCPD2               // 8: UCPD global interrupt
    , ISR_DMA_CHANNEL1              // 9: DMA channel 1 interrupt
    , ISR_DMA_CHANNEL2_3            // 10: DMA channel 2 &amp; 3 interrupts
    , ISR_DMA_CHANNEL4_5_6_7        // 11: DMA channel 4, 5, 6 &amp; 7 and DMAMUX
    , ISR_ADC_COMP                  // 12: ADC and COMP interrupts
    , ISR_TIM1_BRK_UP_TRG_COMP      // 13: TIM1 break, update, trigger
    , ISR_TIM1_CC                   // 14: TIM1 Capture Compare interrupt
    , ISR_TIM2                      // 15: TIM2 global interrupt
    , ISR_TIM3                      // 16: TIM3 global interrupt
    , ISR_TIM6_DAC_LPTIM1           // 17: TIM6 + LPTIM1 and DAC global interrupt
    , ISR_TIM7_LPTIM2               // 18: TIM7 + LPTIM2 global interrupt
    , ISR_TIM14                     // 19: TIM14 global interrupt
    , ISR_TIM15                     // 20: TIM15 global interrupt
    , ISR_TIM16                     // 21: TIM16 global interrupt
    , ISR_TIM17                     // 22: TIM17 global interrupt
    , ISR_I2C1                      // 23: I2C1 global interrupt
    , ISR_I2C2                      // 24: I2C2 global interrupt
    , ISR_SPI1                      // 25: SPI1 global interrupt
    , ISR_SPI2                      // 26: SPI2 global interrupt
    , ISR_USART1                    // 27: USART1 global interrupt
    , ISR_USART2                    // 28: USART2 global interrupt
    , ISR_USART3_USART4_LPUART1     // 29: USART3 + USART4 + LPUART1
    , ISR_CEC                       // 30: CEC global interrupt
    , ISR_AES_RNG                   // 31: AES and RNG global interrupts
    };

