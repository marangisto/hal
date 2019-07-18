////
//
//        nrf52 vectors
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
template<> void handler<interrupt::POWER_CLOCK>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::RADIO>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::UARTE0_UART0>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::NFCT>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::GPIOTE>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SAADC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIMER0>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIMER1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIMER2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::RTC0>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TEMP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::RNG>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::ECB>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::CCM_AAR>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::WDT>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::RTC1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::QDEC>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::COMP_LPCOMP>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SWI0_EGU0>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SWI1_EGU1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SWI2_EGU2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SWI3_EGU3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SWI4_EGU4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SWI5_EGU5>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIMER3>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::TIMER4>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::PWM0>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::PDM>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::MWU>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::PWM1>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::PWM2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::SPIM2_SPIS2_SPI2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::RTC2>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
template<> void handler<interrupt::I2S>() __attribute__ ((weak, alias("_Z17__default_handlerv")));

void (*vectors[])(void) __attribute__ ((section(".vectors"))) =
    { (void(*)(void)) &__estack                                 // -16: Initial stack pointer
    , handler<interrupt::RESET>                                 // -15: Reset [fixed]
    , handler<interrupt::NMI>                                   // -14: Non maskable interrupt [fixed]
    , handler<interrupt::HARDFAULT>                             // -13: All class of fault [fixed]
    , handler<interrupt::MEMMANAGE>                             // -12: Memory management [settable]
    , handler<interrupt::BUSFAULT>                              // -11: Pre-fetch fault, memory access fault [settable]
    , handler<interrupt::USAGEFAULT>                            // -10: Undefined instruction or illegal state [settable]
    , 0x0
    , 0x0
    , 0x0
    , 0x0
    , handler<interrupt::SVCALL>                                // -5: System service call via SWI instruction [settable]
    , handler<interrupt::DEBUG>                                 // -4: Monitor Debug Monitor [settable]
    , 0x0
    , handler<interrupt::PENDSV>                                // -2: Pendable request for system service [settable]
    , handler<interrupt::SYSTICK>                               // -1: System tick timer [settable]
    , handler<interrupt::POWER_CLOCK>                           // 0: 
    , handler<interrupt::RADIO>                                 // 1: 
    , handler<interrupt::UARTE0_UART0>                          // 2: 
    , handler<interrupt::SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0>     // 3: 
    , handler<interrupt::SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1>     // 4: 
    , handler<interrupt::NFCT>                                  // 5: 
    , handler<interrupt::GPIOTE>                                // 6: 
    , handler<interrupt::SAADC>                                 // 7: 
    , handler<interrupt::TIMER0>                                // 8: 
    , handler<interrupt::TIMER1>                                // 9: 
    , handler<interrupt::TIMER2>                                // 10: 
    , handler<interrupt::RTC0>                                  // 11: 
    , handler<interrupt::TEMP>                                  // 12: 
    , handler<interrupt::RNG>                                   // 13: 
    , handler<interrupt::ECB>                                   // 14: 
    , handler<interrupt::CCM_AAR>                               // 15: 
    , handler<interrupt::WDT>                                   // 16: 
    , handler<interrupt::RTC1>                                  // 17: 
    , handler<interrupt::QDEC>                                  // 18: 
    , handler<interrupt::COMP_LPCOMP>                           // 19: 
    , handler<interrupt::SWI0_EGU0>                             // 20: 
    , handler<interrupt::SWI1_EGU1>                             // 21: 
    , handler<interrupt::SWI2_EGU2>                             // 22: 
    , handler<interrupt::SWI3_EGU3>                             // 23: 
    , handler<interrupt::SWI4_EGU4>                             // 24: 
    , handler<interrupt::SWI5_EGU5>                             // 25: 
    , handler<interrupt::TIMER3>                                // 26: 
    , handler<interrupt::TIMER4>                                // 27: 
    , handler<interrupt::PWM0>                                  // 28: 
    , handler<interrupt::PDM>                                   // 29: 
    , 0x0
    , 0x0
    , handler<interrupt::MWU>                                   // 32: 
    , handler<interrupt::PWM1>                                  // 33: 
    , handler<interrupt::PWM2>                                  // 34: 
    , handler<interrupt::SPIM2_SPIS2_SPI2>                      // 35: 
    , handler<interrupt::RTC2>                                  // 36: 
    , handler<interrupt::I2S>                                   // 37: 
    };

