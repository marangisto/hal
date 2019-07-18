////
//
//        STM32F103 vectors
//
////

template<> void handler<isr::NMI>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::HARDFAULT>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::MEMMANAGE>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::BUSFAULT>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USAGEFAULT>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::SVCALL>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DEBUG>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::PENDSV>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::SYSTICK>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::WWDG>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::PVD>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TAMPER>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::RTC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::FLASH>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::RCC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI0>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CHANNEL1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CHANNEL2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CHANNEL3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CHANNEL4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CHANNEL5>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CHANNEL6>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CHANNEL7>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::ADC1_2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USB_HP_CAN_TX>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USB_LP_CAN_RX0>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::CAN_RX1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::CAN_SCE>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI9_5>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM1_BRK>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM1_UP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM1_TRG_COM>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM1_CC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::I2C1_EV>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::I2C1_ER>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::I2C2_EV>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::I2C2_ER>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::SPI1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::SPI2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USART1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USART2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USART3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI15_10>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::RTCALARM>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM8_BRK>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM8_UP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM8_TRG_COM>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM8_CC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::ADC3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::FSMC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::SDIO>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM5>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::SPI3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::UART4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::UART5>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM6>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM7>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA2_CHANNEL1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA2_CHANNEL2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA2_CHANNEL3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA2_CHANNEL4_5>() __attribute__ ((weak, alias("_Z17__default_handlerv")));

void (*vectors[])(void) __attribute__ ((section(".vectors"))) =
    { (void(*)(void)) &__estack     // -16: Initial stack pointer
    , handler<isr::RESET>                     // -15: Reset [fixed]
    , handler<isr::NMI>                       // -14: Non maskable interrupt [fixed]
    , handler<isr::HARDFAULT>                 // -13: All class of fault [fixed]
    , handler<isr::MEMMANAGE>                 // -12: Memory management [settable]
    , handler<isr::BUSFAULT>                  // -11: Pre-fetch fault, memory access fault [settable]
    , handler<isr::USAGEFAULT>                // -10: Undefined instruction or illegal state [settable]
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , handler<isr::SVCALL>                    // -5: System service call via SWI instruction [settable]
    , handler<isr::DEBUG>                     // -4: Monitor Debug Monitor [settable]
    , 0x0
    , handler<isr::PENDSV>                    // -2: Pendable request for system service [settable]
    , handler<isr::SYSTICK>                   // -1: System tick timer [settable]
    , handler<isr::WWDG>                      // 0: Window Watchdog interrupt
    , handler<isr::PVD>                       // 1: PVD through EXTI line detection interrupt
    , handler<isr::TAMPER>                    // 2: Tamper interrupt
    , handler<isr::RTC>                       // 3: RTC global interrupt
    , handler<isr::FLASH>                     // 4: Flash global interrupt
    , handler<isr::RCC>                       // 5: RCC global interrupt
    , handler<isr::EXTI0>                     // 6: EXTI Line0 interrupt
    , handler<isr::EXTI1>                     // 7: EXTI Line1 interrupt
    , handler<isr::EXTI2>                     // 8: EXTI Line2 interrupt
    , handler<isr::EXTI3>                     // 9: EXTI Line3 interrupt
    , handler<isr::EXTI4>                     // 10: EXTI Line4 interrupt
    , handler<isr::DMA1_CHANNEL1>             // 11: DMA1 Channel1 global interrupt
    , handler<isr::DMA1_CHANNEL2>             // 12: DMA1 Channel2 global interrupt
    , handler<isr::DMA1_CHANNEL3>             // 13: DMA1 Channel3 global interrupt
    , handler<isr::DMA1_CHANNEL4>             // 14: DMA1 Channel4 global interrupt
    , handler<isr::DMA1_CHANNEL5>             // 15: DMA1 Channel5 global interrupt
    , handler<isr::DMA1_CHANNEL6>             // 16: DMA1 Channel6 global interrupt
    , handler<isr::DMA1_CHANNEL7>             // 17: DMA1 Channel7 global interrupt
    , handler<isr::ADC1_2>                    // 18: ADC1 and ADC2 global interrupt
    , handler<isr::USB_HP_CAN_TX>             // 19: USB High Priority or CAN TX interrupts
    , handler<isr::USB_LP_CAN_RX0>            // 20: USB Low Priority or CAN RX0 interrupts
    , handler<isr::CAN_RX1>                   // 21: CAN RX1 interrupt
    , handler<isr::CAN_SCE>                   // 22: CAN SCE interrupt
    , handler<isr::EXTI9_5>                   // 23: EXTI Line[9:5] interrupts
    , handler<isr::TIM1_BRK>                  // 24: TIM1 Break interrupt
    , handler<isr::TIM1_UP>                   // 25: TIM1 Update interrupt
    , handler<isr::TIM1_TRG_COM>              // 26: TIM1 Trigger and Commutation interrupts
    , handler<isr::TIM1_CC>                   // 27: TIM1 Capture Compare interrupt
    , handler<isr::TIM2>                      // 28: TIM2 global interrupt
    , handler<isr::TIM3>                      // 29: TIM3 global interrupt
    , handler<isr::TIM4>                      // 30: TIM4 global interrupt
    , handler<isr::I2C1_EV>                   // 31: I2C1 event interrupt
    , handler<isr::I2C1_ER>                   // 32: I2C1 error interrupt
    , handler<isr::I2C2_EV>                   // 33: I2C2 event interrupt
    , handler<isr::I2C2_ER>                   // 34: I2C2 error interrupt
    , handler<isr::SPI1>                      // 35: SPI1 global interrupt
    , handler<isr::SPI2>                      // 36: SPI2 global interrupt
    , handler<isr::USART1>                    // 37: USART1 global interrupt
    , handler<isr::USART2>                    // 38: USART2 global interrupt
    , handler<isr::USART3>                    // 39: USART3 global interrupt
    , handler<isr::EXTI15_10>                 // 40: EXTI Line[15:10] interrupts
    , handler<isr::RTCALARM>                  // 41: RTC Alarms through EXTI line interrupt
    , 0x0
    , handler<isr::TIM8_BRK>                  // 43: TIM8 Break interrupt
    , handler<isr::TIM8_UP>                   // 44: TIM8 Update interrupt
    , handler<isr::TIM8_TRG_COM>              // 45: TIM8 Trigger and Commutation interrupts
    , handler<isr::TIM8_CC>                   // 46: TIM8 Capture Compare interrupt
    , handler<isr::ADC3>                      // 47: ADC3 global interrupt
    , handler<isr::FSMC>                      // 48: FSMC global interrupt
    , handler<isr::SDIO>                      // 49: SDIO global interrupt
    , handler<isr::TIM5>                      // 50: TIM5 global interrupt
    , handler<isr::SPI3>                      // 51: SPI3 global interrupt
    , handler<isr::UART4>                     // 52: UART4 global interrupt
    , handler<isr::UART5>                     // 53: UART5 global interrupt
    , handler<isr::TIM6>                      // 54: TIM6 global interrupt
    , handler<isr::TIM7>                      // 55: TIM7 global interrupt
    , handler<isr::DMA2_CHANNEL1>             // 56: DMA2 Channel1 global interrupt
    , handler<isr::DMA2_CHANNEL2>             // 57: DMA2 Channel2 global interrupt
    , handler<isr::DMA2_CHANNEL3>             // 58: DMA2 Channel3 global interrupt
    , handler<isr::DMA2_CHANNEL4_5>           // 59: DMA2 Channel4 and DMA2 Channel5 global interrupt
    };

