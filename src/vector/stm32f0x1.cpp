////
//
//        STM32F0x1 vectors
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
template<> void handler<isr::RTC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::FLASH>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::RCC_CRS>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI0_1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI2_3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::EXTI4_15>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TSC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CH1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CH2_3_DMA2_CH1_2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::DMA1_CH4_5_6_7_DMA2_CH3_4_5>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::ADC_COMP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM1_BRK_UP_TRG_COM>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM1_CC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM6_DAC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::TIM7>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
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
template<> void handler<isr::USART3_4_5_6_7_8>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::CEC_CAN>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<isr::USB>() __attribute__ ((weak, alias("_Z17__default_handlerv")));

void (*vectors[])(void) __attribute__ ((section(".vectors"))) =
    { (void(*)(void)) &__estack           // -16: Initial stack pointer
    , handler<isr::RESET>                           // -15: Reset [fixed]
    , handler<isr::NMI>                             // -14: Non maskable interrupt [fixed]
    , handler<isr::HARDFAULT>                       // -13: All class of fault [fixed]
    , handler<isr::MEMMANAGE>                       // -12: Memory management [settable]
    , handler<isr::BUSFAULT>                        // -11: Pre-fetch fault, memory access fault [settable]
    , handler<isr::USAGEFAULT>                      // -10: Undefined instruction or illegal state [settable]
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , handler<isr::SVCALL>                          // -5: System service call via SWI instruction [settable]
    , handler<isr::DEBUG>                           // -4: Monitor Debug Monitor [settable]
    , 0x0
    , handler<isr::PENDSV>                          // -2: Pendable request for system service [settable]
    , handler<isr::SYSTICK>                         // -1: System tick timer [settable]
    , handler<isr::WWDG>                            // 0: Window Watchdog interrupt
    , handler<isr::PVD>                             // 1: PVD and VDDIO2 supply comparator interrupt
    , handler<isr::RTC>                             // 2: RTC interrupts
    , handler<isr::FLASH>                           // 3: Flash global interrupt
    , handler<isr::RCC_CRS>                         // 4: RCC and CRS global interrupts
    , handler<isr::EXTI0_1>                         // 5: EXTI Line[1:0] interrupts
    , handler<isr::EXTI2_3>                         // 6: EXTI Line[3:2] interrupts
    , handler<isr::EXTI4_15>                        // 7: EXTI Line15 and EXTI4 interrupts
    , handler<isr::TSC>                             // 8: Touch sensing interrupt
    , handler<isr::DMA1_CH1>                        // 9: DMA1 channel 1 interrupt
    , handler<isr::DMA1_CH2_3_DMA2_CH1_2>           // 10: DMA1 channel 2 and 3 and DMA2 channel 1 and 2 interrupt
    , handler<isr::DMA1_CH4_5_6_7_DMA2_CH3_4_5>     // 11: DMA1 channel 4, 5, 6 and 7 and DMA2 channel 3, 4 and 5 interrupts
    , handler<isr::ADC_COMP>                        // 12: ADC and comparator interrupts
    , handler<isr::TIM1_BRK_UP_TRG_COM>             // 13: TIM1 break, update, trigger and commutation interrupt
    , handler<isr::TIM1_CC>                         // 14: TIM1 Capture Compare interrupt
    , handler<isr::TIM2>                            // 15: TIM2 global interrupt
    , handler<isr::TIM3>                            // 16: TIM3 global interrupt
    , handler<isr::TIM6_DAC>                        // 17: TIM6 global interrupt and DAC underrun interrupt
    , handler<isr::TIM7>                            // 18: TIM7 global interrupt
    , handler<isr::TIM14>                           // 19: TIM14 global interrupt
    , handler<isr::TIM15>                           // 20: TIM15 global interrupt
    , handler<isr::TIM16>                           // 21: TIM16 global interrupt
    , handler<isr::TIM17>                           // 22: TIM17 global interrupt
    , handler<isr::I2C1>                            // 23: I2C1 global interrupt
    , handler<isr::I2C2>                            // 24: I2C2 global interrupt
    , handler<isr::SPI1>                            // 25: SPI1_global_interrupt
    , handler<isr::SPI2>                            // 26: SPI2 global interrupt
    , handler<isr::USART1>                          // 27: USART1 global interrupt
    , handler<isr::USART2>                          // 28: USART2 global interrupt
    , handler<isr::USART3_4_5_6_7_8>                // 29: USART3, USART4, USART5, USART6, USART7, USART8 global interrupt
    , handler<isr::CEC_CAN>                         // 30: CEC and CAN global interrupt
    , handler<isr::USB>                             // 31: USB global interrupt
    };

