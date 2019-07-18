////
//
//        STM32G431xx vectors
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
template<> void handler<isr::PVD_PVM>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::RTC_TAMP_CSS_LSE>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::RTC_WKUP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::FLASH>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::RCC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI0>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CH1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CH2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CH3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CH4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CH5>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CH6>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::ADC1_2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USB_HP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USB_LP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::FDCAN1_INTR1_IT>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::FDCAN1_INTR0_IT>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI9_5>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM1_BRK_TIM15>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM1_UP_TIM16>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
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
template<> void handler<isr::RTC_ALARM>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USBWAKEUP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM8_BRK>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM8_UP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM8_TRG_COM>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM8_CC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::LPTIM1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::SPI3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::UART4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM6_DACUNDER>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM7>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA2_CH1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA2_CH2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA2_CH3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA2_CH4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA2_CH5>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::UCPD1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::COMP1_2_3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::COMP4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::CRS>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::SAI>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::FPU>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::AES>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::RNG>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::LPUART>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::I2C3_EV>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::I2C3_ER>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMAMUX_OVR>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA2_CH6>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::CORDIC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::FMAC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));

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
    , handler<isr::PVD_PVM>                   // 1: PVD through EXTI line detection
    , handler<isr::RTC_TAMP_CSS_LSE>          // 2: RTC_TAMP_CSS_LSE
    , handler<isr::RTC_WKUP>                  // 3: RTC Wakeup timer
    , handler<isr::FLASH>                     // 4: FLASH
    , handler<isr::RCC>                       // 5: RCC
    , handler<isr::EXTI0>                     // 6: EXTI Line0 interrupt
    , handler<isr::EXTI1>                     // 7: EXTI Line1 interrupt
    , handler<isr::EXTI2>                     // 8: EXTI Line2 interrupt
    , handler<isr::EXTI3>                     // 9: EXTI Line3 interrupt
    , handler<isr::EXTI4>                     // 10: EXTI Line4 interrupt
    , handler<isr::DMA1_CH1>                  // 11: DMA1 channel 1 interrupt
    , handler<isr::DMA1_CH2>                  // 12: DMA1 channel 2 interrupt
    , handler<isr::DMA1_CH3>                  // 13: DMA1 channel 3 interrupt
    , handler<isr::DMA1_CH4>                  // 14: DMA1 channel 4 interrupt
    , handler<isr::DMA1_CH5>                  // 15: DMA1 channel 5 interrupt
    , handler<isr::DMA1_CH6>                  // 16: DMA1 channel 6 interrupt
    , 0x0
    , handler<isr::ADC1_2>                    // 18: ADC1 and ADC2 global interrupt
    , handler<isr::USB_HP>                    // 19: USB_HP
    , handler<isr::USB_LP>                    // 20: USB_LP
    , handler<isr::FDCAN1_INTR1_IT>           // 21: fdcan1_intr1_it
    , handler<isr::FDCAN1_INTR0_IT>           // 22: fdcan1_intr0_it
    , handler<isr::EXTI9_5>                   // 23: EXTI9_5
    , handler<isr::TIM1_BRK_TIM15>            // 24: TIM1_BRK_TIM15
    , handler<isr::TIM1_UP_TIM16>             // 25: TIM1_UP_TIM16
    , handler<isr::TIM1_TRG_COM>              // 26: TIM1_TRG_COM/
    , handler<isr::TIM1_CC>                   // 27: TIM1 capture compare interrupt
    , handler<isr::TIM2>                      // 28: TIM2
    , handler<isr::TIM3>                      // 29: TIM3
    , handler<isr::TIM4>                      // 30: TIM4
    , handler<isr::I2C1_EV>                   // 31: I2C1_EV
    , handler<isr::I2C1_ER>                   // 32: I2C1_ER
    , handler<isr::I2C2_EV>                   // 33: I2C2_EV
    , handler<isr::I2C2_ER>                   // 34: I2C2_ER
    , handler<isr::SPI1>                      // 35: SPI1
    , handler<isr::SPI2>                      // 36: SPI2
    , handler<isr::USART1>                    // 37: USART1
    , handler<isr::USART2>                    // 38: USART2
    , handler<isr::USART3>                    // 39: USART3
    , handler<isr::EXTI15_10>                 // 40: EXTI15_10
    , handler<isr::RTC_ALARM>                 // 41: RTC_ALARM
    , handler<isr::USBWAKEUP>                 // 42: USBWakeUP
    , handler<isr::TIM8_BRK>                  // 43: TIM8_BRK
    , handler<isr::TIM8_UP>                   // 44: TIM8_UP
    , handler<isr::TIM8_TRG_COM>              // 45: TIM8_TRG_COM
    , handler<isr::TIM8_CC>                   // 46: TIM8_CC
    , 0x0
    , 0x0
    , handler<isr::LPTIM1>                    // 49: LPTIM1
    , 0x0
    , handler<isr::SPI3>                      // 51: SPI3
    , handler<isr::UART4>                     // 52: UART4
    , 0x0
    , handler<isr::TIM6_DACUNDER>             // 54: TIM6_DACUNDER
    , handler<isr::TIM7>                      // 55: TIM7
    , handler<isr::DMA2_CH1>                  // 56: DMA2_CH1
    , handler<isr::DMA2_CH2>                  // 57: DMA2_CH2
    , handler<isr::DMA2_CH3>                  // 58: DMA2_CH3
    , handler<isr::DMA2_CH4>                  // 59: DMA2_CH4
    , handler<isr::DMA2_CH5>                  // 60: DMA2_CH5
    , 0x0
    , 0x0
    , handler<isr::UCPD1>                     // 63: UCPD1
    , handler<isr::COMP1_2_3>                 // 64: COMP1_2_3
    , handler<isr::COMP4>                     // 65: COMP4_5_6
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , handler<isr::CRS>                       // 75: CRS
    , handler<isr::SAI>                       // 76: SAI
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , handler<isr::FPU>                       // 81: Floating point unit interrupt
    , 0x0
    , 0x0
    , 0x0
    , handler<isr::AES>                       // 85: AES
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , handler<isr::RNG>                       // 90: RNG
    , handler<isr::LPUART>                    // 91: LPUART
    , handler<isr::I2C3_EV>                   // 92: I2C3_EV
    , handler<isr::I2C3_ER>                   // 93: I2C3_ER
    , handler<isr::DMAMUX_OVR>                // 94: DMAMUX_OVR
    , 0x0
    , 0x0
    , handler<isr::DMA2_CH6>                  // 97: DMA2_CH6
    , 0x0
    , 0x0
    , handler<isr::CORDIC>                    // 100: Cordic
    , handler<isr::FMAC>                      // 101: FMAC
    };

