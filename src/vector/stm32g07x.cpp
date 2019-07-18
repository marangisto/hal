////
//
//        STM32G07x vectors
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
template<> void handler<isr::RTC_STAMP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::FLASH>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::RCC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI0_1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI2_3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI4_15>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::UCPD1_UCPD2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA_CHANNEL1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA_CHANNEL2_3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA_CHANNEL4_5_6_7>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::ADC_COMP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM1_BRK_UP_TRG_COMP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM1_CC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM6_DAC_LPTIM1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM7_LPTIM2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM14>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM15>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM16>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM17>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::I2C1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::I2C2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::SPI1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::SPI2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USART1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USART2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USART3_USART4_LPUART1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::CEC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::AES_RNG>() __attribute__ ((weak, alias("_Z17__default_handlerv")));

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
    , handler<isr::WWDG>                      // 0: Window watchdog interrupt
    , handler<isr::PVD>                       // 1: Power voltage detector interrupt
    , handler<isr::RTC_STAMP>                 // 2: RTC and TAMP interrupts
    , handler<isr::FLASH>                     // 3: Flash global interrupt
    , handler<isr::RCC>                       // 4: RCC global interrupt
    , handler<isr::EXTI0_1>                   // 5: EXTI line 0 &amp; 1 interrupt
    , handler<isr::EXTI2_3>                   // 6: EXTI line 2 &amp; 3 interrupt
    , handler<isr::EXTI4_15>                  // 7: EXTI line 4 to 15 interrupt
    , handler<isr::UCPD1_UCPD2>               // 8: UCPD global interrupt
    , handler<isr::DMA_CHANNEL1>              // 9: DMA channel 1 interrupt
    , handler<isr::DMA_CHANNEL2_3>            // 10: DMA channel 2 &amp; 3 interrupts
    , handler<isr::DMA_CHANNEL4_5_6_7>        // 11: DMA channel 4, 5, 6 &amp; 7 and DMAMUX
    , handler<isr::ADC_COMP>                  // 12: ADC and COMP interrupts
    , handler<isr::TIM1_BRK_UP_TRG_COMP>      // 13: TIM1 break, update, trigger
    , handler<isr::TIM1_CC>                   // 14: TIM1 Capture Compare interrupt
    , handler<isr::TIM2>                      // 15: TIM2 global interrupt
    , handler<isr::TIM3>                      // 16: TIM3 global interrupt
    , handler<isr::TIM6_DAC_LPTIM1>           // 17: TIM6 + LPTIM1 and DAC global interrupt
    , handler<isr::TIM7_LPTIM2>               // 18: TIM7 + LPTIM2 global interrupt
    , handler<isr::TIM14>                     // 19: TIM14 global interrupt
    , handler<isr::TIM15>                     // 20: TIM15 global interrupt
    , handler<isr::TIM16>                     // 21: TIM16 global interrupt
    , handler<isr::TIM17>                     // 22: TIM17 global interrupt
    , handler<isr::I2C1>                      // 23: I2C1 global interrupt
    , handler<isr::I2C2>                      // 24: I2C2 global interrupt
    , handler<isr::SPI1>                      // 25: SPI1 global interrupt
    , handler<isr::SPI2>                      // 26: SPI2 global interrupt
    , handler<isr::USART1>                    // 27: USART1 global interrupt
    , handler<isr::USART2>                    // 28: USART2 global interrupt
    , handler<isr::USART3_USART4_LPUART1>     // 29: USART3 + USART4 + LPUART1
    , handler<isr::CEC>                       // 30: CEC global interrupt
    , handler<isr::AES_RNG>                   // 31: AES and RNG global interrupts
    };

