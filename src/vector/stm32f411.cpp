////
//
//        STM32F411 vectors
//
////

template<> void handler<interrupt::NMI>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::HARDFAULT>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::MEMMANAGE>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::BUSFAULT>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::USAGEFAULT>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SVCALL>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::DEBUG>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::PENDSV>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SYSTICK>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::PVD>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TAMP_STAMP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::RTC_WKUP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::FLASH>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::RCC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::EXTI0>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::EXTI1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::EXTI2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::EXTI3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::EXTI4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::ADC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::EXTI9_5>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIM1_BRK_TIM9>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIM1_UP_TIM10>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIM1_TRG_COM_TIM11>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIM1_CC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIM2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIM3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIM4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::I2C1_EV>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::I2C1_ER>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::I2C2_EV>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::I2C2_ER>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SPI1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SPI2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::USART1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::USART2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::EXTI15_10>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::RTC_ALARM>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::OTG_FS_WKUP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SDIO>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIM5>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SPI3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::OTG_FS>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::I2C3_EV>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::I2C3_ER>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::FPU>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SPI4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));

void (*vectors[])(void) __attribute__ ((section(".vectors"))) =
    { (void(*)(void)) &__estack                     // -16: Initial stack pointer
    , handler<interrupt::RESET>                     // -15: Reset [fixed]
    , handler<interrupt::NMI>                       // -14: Non maskable interrupt [fixed]
    , handler<interrupt::HARDFAULT>                 // -13: All class of fault [fixed]
    , handler<interrupt::MEMMANAGE>                 // -12: Memory management [settable]
    , handler<interrupt::BUSFAULT>                  // -11: Pre-fetch fault, memory access fault [settable]
    , handler<interrupt::USAGEFAULT>                // -10: Undefined instruction or illegal state [settable]
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , handler<interrupt::SVCALL>                    // -5: System service call via SWI instruction [settable]
    , handler<interrupt::DEBUG>                     // -4: Monitor Debug Monitor [settable]
    , 0x0
    , handler<interrupt::PENDSV>                    // -2: Pendable request for system service [settable]
    , handler<interrupt::SYSTICK>                   // -1: System tick timer [settable]
    , 0x0
    , handler<interrupt::PVD>                       // 1: PVD through EXTI line detection interrupt
    , handler<interrupt::TAMP_STAMP>                // 2: Tamper and TimeStamp interrupts through the EXTI line
    , handler<interrupt::RTC_WKUP>                  // 3: RTC Wakeup interrupt through the EXTI line
    , handler<interrupt::FLASH>                     // 4: FLASH global interrupt
    , handler<interrupt::RCC>                       // 5: RCC global interrupt
    , handler<interrupt::EXTI0>                     // 6: EXTI Line0 interrupt
    , handler<interrupt::EXTI1>                     // 7: EXTI Line1 interrupt
    , handler<interrupt::EXTI2>                     // 8: EXTI Line2 interrupt
    , handler<interrupt::EXTI3>                     // 9: EXTI Line3 interrupt
    , handler<interrupt::EXTI4>                     // 10: EXTI Line4 interrupt
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , handler<interrupt::ADC>                       // 18: ADC1 global interrupt
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , handler<interrupt::EXTI9_5>                   // 23: EXTI Line[9:5] interrupts
    , handler<interrupt::TIM1_BRK_TIM9>             // 24: TIM1 Break interrupt and TIM9 global interrupt
    , handler<interrupt::TIM1_UP_TIM10>             // 25: TIM1 Update interrupt and TIM10 global interrupt
    , handler<interrupt::TIM1_TRG_COM_TIM11>        // 26: TIM1 Trigger and Commutation interrupts and TIM11 global interrupt
    , handler<interrupt::TIM1_CC>                   // 27: TIM1 Capture Compare interrupt
    , handler<interrupt::TIM2>                      // 28: TIM2 global interrupt
    , handler<interrupt::TIM3>                      // 29: TIM3 global interrupt
    , handler<interrupt::TIM4>                      // 30: TIM4 global interrupt
    , handler<interrupt::I2C1_EV>                   // 31: I2C1 event interrupt
    , handler<interrupt::I2C1_ER>                   // 32: I2C1 error interrupt
    , handler<interrupt::I2C2_EV>                   // 33: I2C2 event interrupt
    , handler<interrupt::I2C2_ER>                   // 34: I2C2 error interrupt
    , handler<interrupt::SPI1>                      // 35: SPI1 global interrupt
    , handler<interrupt::SPI2>                      // 36: SPI2 global interrupt
    , handler<interrupt::USART1>                    // 37: USART1 event interrupt
    , handler<interrupt::USART2>                    // 38: USART2 event interrupt
    , 0x0
    , handler<interrupt::EXTI15_10>                 // 40: EXTI Line[15:10] interrupts
    , handler<interrupt::RTC_ALARM>                 // 41: RTC Alarms (A and B) through EXTI line interrupt
    , handler<interrupt::OTG_FS_WKUP>               // 42: USB On-The-Go FS Wakeup through EXTI line interrupt
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , handler<interrupt::SDIO>                      // 49: SDIO global interrupt
    , handler<interrupt::TIM5>                      // 50: TIM5 global interrupt
    , handler<interrupt::SPI3>                      // 51: SPI3 global interrupt
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
    , handler<interrupt::OTG_FS>                    // 67: USB On The Go FS global interrupt
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , handler<interrupt::I2C3_EV>                   // 72: I2C3 event interrupt
    , handler<interrupt::I2C3_ER>                   // 73: I2C3 error interrupt
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , handler<interrupt::FPU>                       // 81: FPU interrupt
    , 0x0
    , 0x0
    , handler<interrupt::SPI4>                      // 84: SPI4 global interrupt
    };

