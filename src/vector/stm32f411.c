////
//
//        STM32F411 vectors
//
////

void ISR_NMI(void) __attribute__ ((weak, alias("__nothing")));
void ISR_HARDFAULT(void) __attribute__ ((weak, alias("__nothing")));
void ISR_MEMMANAGE(void) __attribute__ ((weak, alias("__nothing")));
void ISR_BUSFAULT(void) __attribute__ ((weak, alias("__nothing")));
void ISR_USAGEFAULT(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SVCALL(void) __attribute__ ((weak, alias("__nothing")));
void ISR_DEBUG(void) __attribute__ ((weak, alias("__nothing")));
void ISR_PENDSV(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SYSTICK(void) __attribute__ ((weak, alias("__nothing")));
void ISR_PVD(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TAMP_STAMP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RTC_WKUP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FLASH(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RCC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI0(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI4(void) __attribute__ ((weak, alias("__nothing")));
void ISR_ADC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI9_5(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_BRK_TIM9(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_UP_TIM10(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_TRG_COM_TIM11(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM1_CC(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_TIM3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C1_EV(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C1_ER(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C2_EV(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C2_ER(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI1(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI2(void) __attribute__ ((weak, alias("__nothing")));
void ISR_EXTI15_10(void) __attribute__ ((weak, alias("__nothing")));
void ISR_RTC_ALARM(void) __attribute__ ((weak, alias("__nothing")));
void ISR_OTG_FS_WKUP(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SDIO(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI3(void) __attribute__ ((weak, alias("__nothing")));
void ISR_OTG_FS(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C3_EV(void) __attribute__ ((weak, alias("__nothing")));
void ISR_I2C3_ER(void) __attribute__ ((weak, alias("__nothing")));
void ISR_FPU(void) __attribute__ ((weak, alias("__nothing")));
void ISR_SPI4(void) __attribute__ ((weak, alias("__nothing")));

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
    , 0x0
    , ISR_PVD                       // 1: PVD through EXTI line detection interrupt
    , ISR_TAMP_STAMP                // 2: Tamper and TimeStamp interrupts through the EXTI line
    , ISR_RTC_WKUP                  // 3: RTC Wakeup interrupt through the EXTI line
    , ISR_FLASH                     // 4: FLASH global interrupt
    , ISR_RCC                       // 5: RCC global interrupt
    , ISR_EXTI0                     // 6: EXTI Line0 interrupt
    , ISR_EXTI1                     // 7: EXTI Line1 interrupt
    , ISR_EXTI2                     // 8: EXTI Line2 interrupt
    , ISR_EXTI3                     // 9: EXTI Line3 interrupt
    , ISR_EXTI4                     // 10: EXTI Line4 interrupt
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , ISR_ADC                       // 18: ADC1 global interrupt
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , ISR_EXTI9_5                   // 23: EXTI Line[9:5] interrupts
    , ISR_TIM1_BRK_TIM9             // 24: TIM1 Break interrupt and TIM9 global interrupt
    , ISR_TIM1_UP_TIM10             // 25: TIM1 Update interrupt and TIM10 global interrupt
    , ISR_TIM1_TRG_COM_TIM11        // 26: TIM1 Trigger and Commutation interrupts and TIM11 global interrupt
    , ISR_TIM1_CC                   // 27: TIM1 Capture Compare interrupt
    , ISR_TIM2                      // 28: TIM2 global interrupt
    , ISR_TIM3                      // 29: TIM3 global interrupt
    , 0x0
    , ISR_I2C1_EV                   // 31: I2C1 event interrupt
    , ISR_I2C1_ER                   // 32: I2C1 error interrupt
    , ISR_I2C2_EV                   // 33: I2C2 event interrupt
    , ISR_I2C2_ER                   // 34: I2C2 error interrupt
    , ISR_SPI1                      // 35: SPI1 global interrupt
    , ISR_SPI2                      // 36: SPI2 global interrupt
    , 0x0
    , 0x0
    , 0x0
    , ISR_EXTI15_10                 // 40: EXTI Line[15:10] interrupts
    , ISR_RTC_ALARM                 // 41: RTC Alarms (A and B) through EXTI line interrupt
    , ISR_OTG_FS_WKUP               // 42: USB On-The-Go FS Wakeup through EXTI line interrupt
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , ISR_SDIO                      // 49: SDIO global interrupt
    , 0x0
    , ISR_SPI3                      // 51: SPI3 global interrupt
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
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , ISR_OTG_FS                    // 67: USB On The Go FS global interrupt
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , ISR_I2C3_EV                   // 72: I2C3 event interrupt
    , ISR_I2C3_ER                   // 73: I2C3 error interrupt
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , ISR_FPU                       // 81: FPU interrupt
    , 0x0
    , 0x0
    , ISR_SPI4                      // 84: SPI4 global interrupt
    };

