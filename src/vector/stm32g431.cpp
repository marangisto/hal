////
//
//        STM32G431xx vectors
//
////

extern void ISR_NMI(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_HARDFAULT(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_MEMMANAGE(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_BUSFAULT(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_USAGEFAULT(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_SVCALL(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DEBUG(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_PENDSV(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_SYSTICK(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_WWDG(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_PVD_PVM(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_RTC_TAMP_CSS_LSE(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_RTC_WKUP(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_FLASH(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_RCC(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_EXTI0(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_EXTI1(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_EXTI2(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_EXTI3(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_EXTI4(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMA1_CH1(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMA1_CH2(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMA1_CH3(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMA1_CH4(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMA1_CH5(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMA1_CH6(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_ADC1_2(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_USB_HP(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_USB_LP(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_FDCAN1_INTR1_IT(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_FDCAN1_INTR0_IT(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_EXTI9_5(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM1_BRK_TIM15(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM1_UP_TIM16(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM1_TRG_COM(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM1_CC(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM2(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM3(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM4(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_I2C1_EV(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_I2C1_ER(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_I2C2_EV(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_I2C2_ER(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_SPI1(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_SPI2(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_USART1(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_USART2(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_USART3(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_EXTI15_10(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_RTC_ALARM(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_USBWAKEUP(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM8_BRK(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM8_UP(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM8_TRG_COM(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM8_CC(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_LPTIM1(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_SPI3(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_UART4(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM6_DACUNDER(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_TIM7(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMA2_CH1(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMA2_CH2(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMA2_CH3(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMA2_CH4(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMA2_CH5(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_UCPD1(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_COMP1_2_3(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_COMP4(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_CRS(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_SAI(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_FPU(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_AES(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_RNG(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_LPUART(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_I2C3_EV(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_I2C3_ER(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMAMUX_OVR(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_DMA2_CH6(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_CORDIC(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));
extern void ISR_FMAC(void) __attribute__ ((weak, alias("_Z17__default_handlerv")));

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
    , ISR_PVD_PVM                   // 1: PVD through EXTI line detection
    , ISR_RTC_TAMP_CSS_LSE          // 2: RTC_TAMP_CSS_LSE
    , ISR_RTC_WKUP                  // 3: RTC Wakeup timer
    , ISR_FLASH                     // 4: FLASH
    , ISR_RCC                       // 5: RCC
    , ISR_EXTI0                     // 6: EXTI Line0 interrupt
    , ISR_EXTI1                     // 7: EXTI Line1 interrupt
    , ISR_EXTI2                     // 8: EXTI Line2 interrupt
    , ISR_EXTI3                     // 9: EXTI Line3 interrupt
    , ISR_EXTI4                     // 10: EXTI Line4 interrupt
    , ISR_DMA1_CH1                  // 11: DMA1 channel 1 interrupt
    , ISR_DMA1_CH2                  // 12: DMA1 channel 2 interrupt
    , ISR_DMA1_CH3                  // 13: DMA1 channel 3 interrupt
    , ISR_DMA1_CH4                  // 14: DMA1 channel 4 interrupt
    , ISR_DMA1_CH5                  // 15: DMA1 channel 5 interrupt
    , ISR_DMA1_CH6                  // 16: DMA1 channel 6 interrupt
    , 0x0
    , ISR_ADC1_2                    // 18: ADC1 and ADC2 global interrupt
    , ISR_USB_HP                    // 19: USB_HP
    , ISR_USB_LP                    // 20: USB_LP
    , ISR_FDCAN1_INTR1_IT           // 21: fdcan1_intr1_it
    , ISR_FDCAN1_INTR0_IT           // 22: fdcan1_intr0_it
    , ISR_EXTI9_5                   // 23: EXTI9_5
    , ISR_TIM1_BRK_TIM15            // 24: TIM1_BRK_TIM15
    , ISR_TIM1_UP_TIM16             // 25: TIM1_UP_TIM16
    , ISR_TIM1_TRG_COM              // 26: TIM1_TRG_COM/
    , ISR_TIM1_CC                   // 27: TIM1 capture compare interrupt
    , ISR_TIM2                      // 28: TIM2
    , ISR_TIM3                      // 29: TIM3
    , ISR_TIM4                      // 30: TIM4
    , ISR_I2C1_EV                   // 31: I2C1_EV
    , ISR_I2C1_ER                   // 32: I2C1_ER
    , ISR_I2C2_EV                   // 33: I2C2_EV
    , ISR_I2C2_ER                   // 34: I2C2_ER
    , ISR_SPI1                      // 35: SPI1
    , ISR_SPI2                      // 36: SPI2
    , ISR_USART1                    // 37: USART1
    , ISR_USART2                    // 38: USART2
    , ISR_USART3                    // 39: USART3
    , ISR_EXTI15_10                 // 40: EXTI15_10
    , ISR_RTC_ALARM                 // 41: RTC_ALARM
    , ISR_USBWAKEUP                 // 42: USBWakeUP
    , ISR_TIM8_BRK                  // 43: TIM8_BRK
    , ISR_TIM8_UP                   // 44: TIM8_UP
    , ISR_TIM8_TRG_COM              // 45: TIM8_TRG_COM
    , ISR_TIM8_CC                   // 46: TIM8_CC
    , 0x0
    , 0x0
    , ISR_LPTIM1                    // 49: LPTIM1
    , 0x0
    , ISR_SPI3                      // 51: SPI3
    , ISR_UART4                     // 52: UART4
    , 0x0
    , ISR_TIM6_DACUNDER             // 54: TIM6_DACUNDER
    , ISR_TIM7                      // 55: TIM7
    , ISR_DMA2_CH1                  // 56: DMA2_CH1
    , ISR_DMA2_CH2                  // 57: DMA2_CH2
    , ISR_DMA2_CH3                  // 58: DMA2_CH3
    , ISR_DMA2_CH4                  // 59: DMA2_CH4
    , ISR_DMA2_CH5                  // 60: DMA2_CH5
    , 0x0
    , 0x0
    , ISR_UCPD1                     // 63: UCPD1
    , ISR_COMP1_2_3                 // 64: COMP1_2_3
    , ISR_COMP4                     // 65: COMP4_5_6
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , ISR_CRS                       // 75: CRS
    , ISR_SAI                       // 76: SAI
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , ISR_FPU                       // 81: Floating point unit interrupt
    , 0x0
    , 0x0
    , 0x0
    , ISR_AES                       // 85: AES
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , ISR_RNG                       // 90: RNG
    , ISR_LPUART                    // 91: LPUART
    , ISR_I2C3_EV                   // 92: I2C3_EV
    , ISR_I2C3_ER                   // 93: I2C3_ER
    , ISR_DMAMUX_OVR                // 94: DMAMUX_OVR
    , 0x0
    , 0x0
    , ISR_DMA2_CH6                  // 97: DMA2_CH6
    , 0x0
    , 0x0
    , ISR_CORDIC                    // 100: Cordic
    , ISR_FMAC                      // 101: FMAC
    };

