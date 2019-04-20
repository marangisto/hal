#pragma once

#include <stdint.h>

////
//
//    STM32F0x1
//
//       schema-version : 1.1
//       vendor         : 
//       series         : 
//       device-version : 1.2
//       address-unit   : 8 bits
//       device-width   : 32
//       device-size    : 32
//
////

namespace stm32f0x1
{

template<int N> class reserved_t { private: uint32_t m_pad[N]; };

////
//
//    cyclic redundancy check calculation unit
//
////

struct crc_t
{
    volatile uint32_t    DR;                   // [Read-write] Data register
    volatile uint32_t    IDR;                  // [Read-write] Independent data register
    volatile uint32_t    CR;                   // [Read-write] Control register
    volatile uint32_t    INIT;                 // [Read-write] Initial CRC value

    static const uint8_t DR_DR = 0;               // Data register bits (32 bits)
    static const uint32_t DR_RESET_VALUE = 0xffffffff;

    static const uint8_t IDR_IDR = 0;              // General-purpose 8-bit data register bits (8 bits)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static const uint8_t CR_RESET = 0;            // reset bit
    static const uint8_t CR_POLYSIZE = 3;         // Polynomial size (2 bits)
    static const uint8_t CR_REV_IN = 5;           // Reverse input data (2 bits)
    static const uint8_t CR_REV_OUT = 7;          // Reverse output data
    static const uint32_t CR_RESET_VALUE = 0x0;

    static const uint8_t INIT_INIT = 0;             // Programmable initial CRC value (32 bits)
    static const uint32_t INIT_RESET_VALUE = 0xffffffff;
};

static crc_t& CRC = *reinterpret_cast<crc_t*>(0x40023000);


////
//
//    General-purpose I/Os
//
////

struct gpiof_t
{
    volatile uint32_t    MODER;                // [Read-write] GPIO port mode register
    volatile uint32_t    OTYPER;               // [Read-write] GPIO port output type register
    volatile uint32_t    OSPEEDR;              // [Read-write] GPIO port output speed register
    volatile uint32_t    PUPDR;                // [Read-write] GPIO port pull-up/pull-down register
    volatile uint32_t    IDR;                  // [Read-only] GPIO port input data register
    volatile uint32_t    ODR;                  // [Read-write] GPIO port output data register
    volatile uint32_t    BSRR;                 // [Write-only] GPIO port bit set/reset register
    volatile uint32_t    LCKR;                 // [Read-write] GPIO port configuration lock register
    volatile uint32_t    AFRL;                 // [Read-write] GPIO alternate function low register
    volatile uint32_t    AFRH;                 // [Read-write] GPIO alternate function high register
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register

    static const uint8_t MODER_MODER15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t MODER_RESET_VALUE = 0x0;

    static const uint8_t OTYPER_OT15 = 15;            // Port x configuration bit 15
    static const uint8_t OTYPER_OT14 = 14;            // Port x configuration bit 14
    static const uint8_t OTYPER_OT13 = 13;            // Port x configuration bit 13
    static const uint8_t OTYPER_OT12 = 12;            // Port x configuration bit 12
    static const uint8_t OTYPER_OT11 = 11;            // Port x configuration bit 11
    static const uint8_t OTYPER_OT10 = 10;            // Port x configuration bit 10
    static const uint8_t OTYPER_OT9 = 9;              // Port x configuration bit 9
    static const uint8_t OTYPER_OT8 = 8;              // Port x configuration bit 8
    static const uint8_t OTYPER_OT7 = 7;              // Port x configuration bit 7
    static const uint8_t OTYPER_OT6 = 6;              // Port x configuration bit 6
    static const uint8_t OTYPER_OT5 = 5;              // Port x configuration bit 5
    static const uint8_t OTYPER_OT4 = 4;              // Port x configuration bit 4
    static const uint8_t OTYPER_OT3 = 3;              // Port x configuration bit 3
    static const uint8_t OTYPER_OT2 = 2;              // Port x configuration bit 2
    static const uint8_t OTYPER_OT1 = 1;              // Port x configuration bit 1
    static const uint8_t OTYPER_OT0 = 0;              // Port x configuration bit 0
    static const uint32_t OTYPER_RESET_VALUE = 0x0;

    static const uint8_t OSPEEDR_OSPEEDR15 = 30;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR14 = 28;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR13 = 26;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR12 = 24;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR11 = 22;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR10 = 20;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR9 = 18;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR8 = 16;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR7 = 14;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR6 = 12;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR5 = 10;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR4 = 8;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR3 = 6;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR2 = 4;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR1 = 2;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR0 = 0;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t OSPEEDR_RESET_VALUE = 0x0;

    static const uint8_t PUPDR_PUPDR15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t PUPDR_RESET_VALUE = 0x0;

    static const uint8_t IDR_IDR15 = 15;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR14 = 14;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR13 = 13;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR12 = 12;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR11 = 11;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR10 = 10;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR9 = 9;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR8 = 8;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR7 = 7;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR6 = 6;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR5 = 5;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR4 = 4;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR3 = 3;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR2 = 2;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR1 = 1;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR0 = 0;             // Port input data (y = 0..15)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static const uint8_t ODR_ODR15 = 15;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR14 = 14;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR13 = 13;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR12 = 12;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR11 = 11;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR10 = 10;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR9 = 9;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR8 = 8;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR7 = 7;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR6 = 6;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR5 = 5;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR4 = 4;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR3 = 3;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR2 = 2;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR1 = 1;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR0 = 0;             // Port output data (y = 0..15)
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static const uint8_t BSRR_BR15 = 31;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR14 = 30;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR13 = 29;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR12 = 28;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR11 = 27;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR10 = 26;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR9 = 25;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR8 = 24;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR7 = 23;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR6 = 22;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR5 = 21;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR4 = 20;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR3 = 19;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR2 = 18;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR1 = 17;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR0 = 16;             // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS15 = 15;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS14 = 14;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS13 = 13;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS12 = 12;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS11 = 11;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS10 = 10;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS9 = 9;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS8 = 8;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS7 = 7;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS6 = 6;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS5 = 5;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS4 = 4;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS3 = 3;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS2 = 2;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS1 = 1;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS0 = 0;              // Port x set bit y (y= 0..15)
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static const uint8_t LCKR_LCKK = 16;            // Port x lock bit y
    static const uint8_t LCKR_LCK15 = 15;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK14 = 14;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK13 = 13;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK12 = 12;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK11 = 11;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK10 = 10;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK9 = 9;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK8 = 8;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK7 = 7;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK6 = 6;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK5 = 5;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK4 = 4;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK3 = 3;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK2 = 2;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK1 = 1;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK0 = 0;             // Port x lock bit y (y= 0..15)
    static const uint32_t LCKR_RESET_VALUE = 0x0;

    static const uint8_t AFRL_AFRL7 = 28;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL6 = 24;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL5 = 20;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL4 = 16;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL3 = 12;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL2 = 8;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL1 = 4;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL0 = 0;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    static const uint8_t AFRH_AFRH15 = 28;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH14 = 24;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH13 = 20;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH12 = 16;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH11 = 12;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH10 = 8;           // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH9 = 4;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH8 = 0;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static const uint8_t BRR_BR0 = 0;              // Port x Reset bit y
    static const uint8_t BRR_BR1 = 1;              // Port x Reset bit y
    static const uint8_t BRR_BR2 = 2;              // Port x Reset bit y
    static const uint8_t BRR_BR3 = 3;              // Port x Reset bit y
    static const uint8_t BRR_BR4 = 4;              // Port x Reset bit y
    static const uint8_t BRR_BR5 = 5;              // Port x Reset bit y
    static const uint8_t BRR_BR6 = 6;              // Port x Reset bit y
    static const uint8_t BRR_BR7 = 7;              // Port x Reset bit y
    static const uint8_t BRR_BR8 = 8;              // Port x Reset bit y
    static const uint8_t BRR_BR9 = 9;              // Port x Reset bit y
    static const uint8_t BRR_BR10 = 10;            // Port x Reset bit y
    static const uint8_t BRR_BR11 = 11;            // Port x Reset bit y
    static const uint8_t BRR_BR12 = 12;            // Port x Reset bit y
    static const uint8_t BRR_BR13 = 13;            // Port x Reset bit y
    static const uint8_t BRR_BR14 = 14;            // Port x Reset bit y
    static const uint8_t BRR_BR15 = 15;            // Port x Reset bit y
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpiof_t& GPIOF = *reinterpret_cast<gpiof_t*>(0x48001400);


////
//
//    General-purpose I/Os
//
////

struct gpiod_t
{
    volatile uint32_t    MODER;                // [Read-write] GPIO port mode register
    volatile uint32_t    OTYPER;               // [Read-write] GPIO port output type register
    volatile uint32_t    OSPEEDR;              // [Read-write] GPIO port output speed register
    volatile uint32_t    PUPDR;                // [Read-write] GPIO port pull-up/pull-down register
    volatile uint32_t    IDR;                  // [Read-only] GPIO port input data register
    volatile uint32_t    ODR;                  // [Read-write] GPIO port output data register
    volatile uint32_t    BSRR;                 // [Write-only] GPIO port bit set/reset register
    volatile uint32_t    LCKR;                 // [Read-write] GPIO port configuration lock register
    volatile uint32_t    AFRL;                 // [Read-write] GPIO alternate function low register
    volatile uint32_t    AFRH;                 // [Read-write] GPIO alternate function high register
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register

    static const uint8_t MODER_MODER15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t MODER_RESET_VALUE = 0x0;

    static const uint8_t OTYPER_OT15 = 15;            // Port x configuration bit 15
    static const uint8_t OTYPER_OT14 = 14;            // Port x configuration bit 14
    static const uint8_t OTYPER_OT13 = 13;            // Port x configuration bit 13
    static const uint8_t OTYPER_OT12 = 12;            // Port x configuration bit 12
    static const uint8_t OTYPER_OT11 = 11;            // Port x configuration bit 11
    static const uint8_t OTYPER_OT10 = 10;            // Port x configuration bit 10
    static const uint8_t OTYPER_OT9 = 9;              // Port x configuration bit 9
    static const uint8_t OTYPER_OT8 = 8;              // Port x configuration bit 8
    static const uint8_t OTYPER_OT7 = 7;              // Port x configuration bit 7
    static const uint8_t OTYPER_OT6 = 6;              // Port x configuration bit 6
    static const uint8_t OTYPER_OT5 = 5;              // Port x configuration bit 5
    static const uint8_t OTYPER_OT4 = 4;              // Port x configuration bit 4
    static const uint8_t OTYPER_OT3 = 3;              // Port x configuration bit 3
    static const uint8_t OTYPER_OT2 = 2;              // Port x configuration bit 2
    static const uint8_t OTYPER_OT1 = 1;              // Port x configuration bit 1
    static const uint8_t OTYPER_OT0 = 0;              // Port x configuration bit 0
    static const uint32_t OTYPER_RESET_VALUE = 0x0;

    static const uint8_t OSPEEDR_OSPEEDR15 = 30;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR14 = 28;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR13 = 26;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR12 = 24;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR11 = 22;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR10 = 20;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR9 = 18;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR8 = 16;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR7 = 14;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR6 = 12;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR5 = 10;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR4 = 8;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR3 = 6;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR2 = 4;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR1 = 2;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR0 = 0;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t OSPEEDR_RESET_VALUE = 0x0;

    static const uint8_t PUPDR_PUPDR15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t PUPDR_RESET_VALUE = 0x0;

    static const uint8_t IDR_IDR15 = 15;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR14 = 14;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR13 = 13;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR12 = 12;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR11 = 11;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR10 = 10;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR9 = 9;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR8 = 8;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR7 = 7;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR6 = 6;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR5 = 5;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR4 = 4;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR3 = 3;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR2 = 2;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR1 = 1;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR0 = 0;             // Port input data (y = 0..15)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static const uint8_t ODR_ODR15 = 15;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR14 = 14;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR13 = 13;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR12 = 12;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR11 = 11;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR10 = 10;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR9 = 9;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR8 = 8;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR7 = 7;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR6 = 6;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR5 = 5;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR4 = 4;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR3 = 3;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR2 = 2;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR1 = 1;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR0 = 0;             // Port output data (y = 0..15)
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static const uint8_t BSRR_BR15 = 31;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR14 = 30;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR13 = 29;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR12 = 28;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR11 = 27;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR10 = 26;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR9 = 25;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR8 = 24;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR7 = 23;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR6 = 22;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR5 = 21;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR4 = 20;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR3 = 19;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR2 = 18;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR1 = 17;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR0 = 16;             // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS15 = 15;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS14 = 14;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS13 = 13;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS12 = 12;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS11 = 11;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS10 = 10;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS9 = 9;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS8 = 8;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS7 = 7;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS6 = 6;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS5 = 5;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS4 = 4;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS3 = 3;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS2 = 2;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS1 = 1;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS0 = 0;              // Port x set bit y (y= 0..15)
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static const uint8_t LCKR_LCKK = 16;            // Port x lock bit y
    static const uint8_t LCKR_LCK15 = 15;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK14 = 14;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK13 = 13;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK12 = 12;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK11 = 11;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK10 = 10;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK9 = 9;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK8 = 8;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK7 = 7;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK6 = 6;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK5 = 5;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK4 = 4;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK3 = 3;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK2 = 2;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK1 = 1;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK0 = 0;             // Port x lock bit y (y= 0..15)
    static const uint32_t LCKR_RESET_VALUE = 0x0;

    static const uint8_t AFRL_AFRL7 = 28;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL6 = 24;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL5 = 20;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL4 = 16;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL3 = 12;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL2 = 8;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL1 = 4;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL0 = 0;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    static const uint8_t AFRH_AFRH15 = 28;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH14 = 24;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH13 = 20;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH12 = 16;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH11 = 12;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH10 = 8;           // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH9 = 4;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH8 = 0;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static const uint8_t BRR_BR0 = 0;              // Port x Reset bit y
    static const uint8_t BRR_BR1 = 1;              // Port x Reset bit y
    static const uint8_t BRR_BR2 = 2;              // Port x Reset bit y
    static const uint8_t BRR_BR3 = 3;              // Port x Reset bit y
    static const uint8_t BRR_BR4 = 4;              // Port x Reset bit y
    static const uint8_t BRR_BR5 = 5;              // Port x Reset bit y
    static const uint8_t BRR_BR6 = 6;              // Port x Reset bit y
    static const uint8_t BRR_BR7 = 7;              // Port x Reset bit y
    static const uint8_t BRR_BR8 = 8;              // Port x Reset bit y
    static const uint8_t BRR_BR9 = 9;              // Port x Reset bit y
    static const uint8_t BRR_BR10 = 10;            // Port x Reset bit y
    static const uint8_t BRR_BR11 = 11;            // Port x Reset bit y
    static const uint8_t BRR_BR12 = 12;            // Port x Reset bit y
    static const uint8_t BRR_BR13 = 13;            // Port x Reset bit y
    static const uint8_t BRR_BR14 = 14;            // Port x Reset bit y
    static const uint8_t BRR_BR15 = 15;            // Port x Reset bit y
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpiod_t& GPIOD = *reinterpret_cast<gpiod_t*>(0x48000c00);


////
//
//    General-purpose I/Os
//
////

struct gpioc_t
{
    volatile uint32_t    MODER;                // [Read-write] GPIO port mode register
    volatile uint32_t    OTYPER;               // [Read-write] GPIO port output type register
    volatile uint32_t    OSPEEDR;              // [Read-write] GPIO port output speed register
    volatile uint32_t    PUPDR;                // [Read-write] GPIO port pull-up/pull-down register
    volatile uint32_t    IDR;                  // [Read-only] GPIO port input data register
    volatile uint32_t    ODR;                  // [Read-write] GPIO port output data register
    volatile uint32_t    BSRR;                 // [Write-only] GPIO port bit set/reset register
    volatile uint32_t    LCKR;                 // [Read-write] GPIO port configuration lock register
    volatile uint32_t    AFRL;                 // [Read-write] GPIO alternate function low register
    volatile uint32_t    AFRH;                 // [Read-write] GPIO alternate function high register
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register

    static const uint8_t MODER_MODER15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t MODER_RESET_VALUE = 0x0;

    static const uint8_t OTYPER_OT15 = 15;            // Port x configuration bit 15
    static const uint8_t OTYPER_OT14 = 14;            // Port x configuration bit 14
    static const uint8_t OTYPER_OT13 = 13;            // Port x configuration bit 13
    static const uint8_t OTYPER_OT12 = 12;            // Port x configuration bit 12
    static const uint8_t OTYPER_OT11 = 11;            // Port x configuration bit 11
    static const uint8_t OTYPER_OT10 = 10;            // Port x configuration bit 10
    static const uint8_t OTYPER_OT9 = 9;              // Port x configuration bit 9
    static const uint8_t OTYPER_OT8 = 8;              // Port x configuration bit 8
    static const uint8_t OTYPER_OT7 = 7;              // Port x configuration bit 7
    static const uint8_t OTYPER_OT6 = 6;              // Port x configuration bit 6
    static const uint8_t OTYPER_OT5 = 5;              // Port x configuration bit 5
    static const uint8_t OTYPER_OT4 = 4;              // Port x configuration bit 4
    static const uint8_t OTYPER_OT3 = 3;              // Port x configuration bit 3
    static const uint8_t OTYPER_OT2 = 2;              // Port x configuration bit 2
    static const uint8_t OTYPER_OT1 = 1;              // Port x configuration bit 1
    static const uint8_t OTYPER_OT0 = 0;              // Port x configuration bit 0
    static const uint32_t OTYPER_RESET_VALUE = 0x0;

    static const uint8_t OSPEEDR_OSPEEDR15 = 30;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR14 = 28;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR13 = 26;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR12 = 24;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR11 = 22;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR10 = 20;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR9 = 18;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR8 = 16;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR7 = 14;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR6 = 12;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR5 = 10;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR4 = 8;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR3 = 6;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR2 = 4;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR1 = 2;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR0 = 0;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t OSPEEDR_RESET_VALUE = 0x0;

    static const uint8_t PUPDR_PUPDR15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t PUPDR_RESET_VALUE = 0x0;

    static const uint8_t IDR_IDR15 = 15;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR14 = 14;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR13 = 13;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR12 = 12;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR11 = 11;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR10 = 10;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR9 = 9;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR8 = 8;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR7 = 7;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR6 = 6;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR5 = 5;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR4 = 4;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR3 = 3;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR2 = 2;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR1 = 1;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR0 = 0;             // Port input data (y = 0..15)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static const uint8_t ODR_ODR15 = 15;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR14 = 14;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR13 = 13;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR12 = 12;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR11 = 11;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR10 = 10;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR9 = 9;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR8 = 8;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR7 = 7;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR6 = 6;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR5 = 5;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR4 = 4;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR3 = 3;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR2 = 2;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR1 = 1;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR0 = 0;             // Port output data (y = 0..15)
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static const uint8_t BSRR_BR15 = 31;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR14 = 30;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR13 = 29;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR12 = 28;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR11 = 27;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR10 = 26;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR9 = 25;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR8 = 24;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR7 = 23;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR6 = 22;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR5 = 21;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR4 = 20;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR3 = 19;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR2 = 18;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR1 = 17;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR0 = 16;             // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS15 = 15;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS14 = 14;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS13 = 13;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS12 = 12;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS11 = 11;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS10 = 10;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS9 = 9;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS8 = 8;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS7 = 7;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS6 = 6;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS5 = 5;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS4 = 4;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS3 = 3;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS2 = 2;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS1 = 1;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS0 = 0;              // Port x set bit y (y= 0..15)
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static const uint8_t LCKR_LCKK = 16;            // Port x lock bit y
    static const uint8_t LCKR_LCK15 = 15;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK14 = 14;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK13 = 13;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK12 = 12;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK11 = 11;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK10 = 10;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK9 = 9;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK8 = 8;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK7 = 7;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK6 = 6;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK5 = 5;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK4 = 4;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK3 = 3;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK2 = 2;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK1 = 1;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK0 = 0;             // Port x lock bit y (y= 0..15)
    static const uint32_t LCKR_RESET_VALUE = 0x0;

    static const uint8_t AFRL_AFRL7 = 28;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL6 = 24;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL5 = 20;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL4 = 16;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL3 = 12;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL2 = 8;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL1 = 4;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL0 = 0;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    static const uint8_t AFRH_AFRH15 = 28;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH14 = 24;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH13 = 20;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH12 = 16;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH11 = 12;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH10 = 8;           // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH9 = 4;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH8 = 0;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static const uint8_t BRR_BR0 = 0;              // Port x Reset bit y
    static const uint8_t BRR_BR1 = 1;              // Port x Reset bit y
    static const uint8_t BRR_BR2 = 2;              // Port x Reset bit y
    static const uint8_t BRR_BR3 = 3;              // Port x Reset bit y
    static const uint8_t BRR_BR4 = 4;              // Port x Reset bit y
    static const uint8_t BRR_BR5 = 5;              // Port x Reset bit y
    static const uint8_t BRR_BR6 = 6;              // Port x Reset bit y
    static const uint8_t BRR_BR7 = 7;              // Port x Reset bit y
    static const uint8_t BRR_BR8 = 8;              // Port x Reset bit y
    static const uint8_t BRR_BR9 = 9;              // Port x Reset bit y
    static const uint8_t BRR_BR10 = 10;            // Port x Reset bit y
    static const uint8_t BRR_BR11 = 11;            // Port x Reset bit y
    static const uint8_t BRR_BR12 = 12;            // Port x Reset bit y
    static const uint8_t BRR_BR13 = 13;            // Port x Reset bit y
    static const uint8_t BRR_BR14 = 14;            // Port x Reset bit y
    static const uint8_t BRR_BR15 = 15;            // Port x Reset bit y
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpioc_t& GPIOC = *reinterpret_cast<gpioc_t*>(0x48000800);


////
//
//    General-purpose I/Os
//
////

struct gpiob_t
{
    volatile uint32_t    MODER;                // [Read-write] GPIO port mode register
    volatile uint32_t    OTYPER;               // [Read-write] GPIO port output type register
    volatile uint32_t    OSPEEDR;              // [Read-write] GPIO port output speed register
    volatile uint32_t    PUPDR;                // [Read-write] GPIO port pull-up/pull-down register
    volatile uint32_t    IDR;                  // [Read-only] GPIO port input data register
    volatile uint32_t    ODR;                  // [Read-write] GPIO port output data register
    volatile uint32_t    BSRR;                 // [Write-only] GPIO port bit set/reset register
    volatile uint32_t    LCKR;                 // [Read-write] GPIO port configuration lock register
    volatile uint32_t    AFRL;                 // [Read-write] GPIO alternate function low register
    volatile uint32_t    AFRH;                 // [Read-write] GPIO alternate function high register
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register

    static const uint8_t MODER_MODER15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t MODER_RESET_VALUE = 0x0;

    static const uint8_t OTYPER_OT15 = 15;            // Port x configuration bit 15
    static const uint8_t OTYPER_OT14 = 14;            // Port x configuration bit 14
    static const uint8_t OTYPER_OT13 = 13;            // Port x configuration bit 13
    static const uint8_t OTYPER_OT12 = 12;            // Port x configuration bit 12
    static const uint8_t OTYPER_OT11 = 11;            // Port x configuration bit 11
    static const uint8_t OTYPER_OT10 = 10;            // Port x configuration bit 10
    static const uint8_t OTYPER_OT9 = 9;              // Port x configuration bit 9
    static const uint8_t OTYPER_OT8 = 8;              // Port x configuration bit 8
    static const uint8_t OTYPER_OT7 = 7;              // Port x configuration bit 7
    static const uint8_t OTYPER_OT6 = 6;              // Port x configuration bit 6
    static const uint8_t OTYPER_OT5 = 5;              // Port x configuration bit 5
    static const uint8_t OTYPER_OT4 = 4;              // Port x configuration bit 4
    static const uint8_t OTYPER_OT3 = 3;              // Port x configuration bit 3
    static const uint8_t OTYPER_OT2 = 2;              // Port x configuration bit 2
    static const uint8_t OTYPER_OT1 = 1;              // Port x configuration bit 1
    static const uint8_t OTYPER_OT0 = 0;              // Port x configuration bit 0
    static const uint32_t OTYPER_RESET_VALUE = 0x0;

    static const uint8_t OSPEEDR_OSPEEDR15 = 30;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR14 = 28;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR13 = 26;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR12 = 24;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR11 = 22;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR10 = 20;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR9 = 18;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR8 = 16;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR7 = 14;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR6 = 12;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR5 = 10;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR4 = 8;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR3 = 6;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR2 = 4;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR1 = 2;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR0 = 0;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t OSPEEDR_RESET_VALUE = 0x0;

    static const uint8_t PUPDR_PUPDR15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t PUPDR_RESET_VALUE = 0x0;

    static const uint8_t IDR_IDR15 = 15;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR14 = 14;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR13 = 13;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR12 = 12;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR11 = 11;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR10 = 10;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR9 = 9;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR8 = 8;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR7 = 7;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR6 = 6;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR5 = 5;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR4 = 4;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR3 = 3;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR2 = 2;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR1 = 1;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR0 = 0;             // Port input data (y = 0..15)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static const uint8_t ODR_ODR15 = 15;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR14 = 14;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR13 = 13;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR12 = 12;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR11 = 11;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR10 = 10;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR9 = 9;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR8 = 8;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR7 = 7;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR6 = 6;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR5 = 5;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR4 = 4;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR3 = 3;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR2 = 2;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR1 = 1;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR0 = 0;             // Port output data (y = 0..15)
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static const uint8_t BSRR_BR15 = 31;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR14 = 30;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR13 = 29;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR12 = 28;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR11 = 27;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR10 = 26;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR9 = 25;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR8 = 24;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR7 = 23;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR6 = 22;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR5 = 21;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR4 = 20;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR3 = 19;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR2 = 18;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR1 = 17;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR0 = 16;             // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS15 = 15;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS14 = 14;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS13 = 13;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS12 = 12;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS11 = 11;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS10 = 10;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS9 = 9;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS8 = 8;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS7 = 7;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS6 = 6;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS5 = 5;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS4 = 4;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS3 = 3;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS2 = 2;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS1 = 1;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS0 = 0;              // Port x set bit y (y= 0..15)
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static const uint8_t LCKR_LCKK = 16;            // Port x lock bit y
    static const uint8_t LCKR_LCK15 = 15;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK14 = 14;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK13 = 13;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK12 = 12;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK11 = 11;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK10 = 10;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK9 = 9;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK8 = 8;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK7 = 7;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK6 = 6;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK5 = 5;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK4 = 4;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK3 = 3;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK2 = 2;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK1 = 1;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK0 = 0;             // Port x lock bit y (y= 0..15)
    static const uint32_t LCKR_RESET_VALUE = 0x0;

    static const uint8_t AFRL_AFRL7 = 28;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL6 = 24;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL5 = 20;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL4 = 16;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL3 = 12;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL2 = 8;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL1 = 4;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL0 = 0;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    static const uint8_t AFRH_AFRH15 = 28;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH14 = 24;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH13 = 20;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH12 = 16;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH11 = 12;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH10 = 8;           // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH9 = 4;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH8 = 0;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static const uint8_t BRR_BR0 = 0;              // Port x Reset bit y
    static const uint8_t BRR_BR1 = 1;              // Port x Reset bit y
    static const uint8_t BRR_BR2 = 2;              // Port x Reset bit y
    static const uint8_t BRR_BR3 = 3;              // Port x Reset bit y
    static const uint8_t BRR_BR4 = 4;              // Port x Reset bit y
    static const uint8_t BRR_BR5 = 5;              // Port x Reset bit y
    static const uint8_t BRR_BR6 = 6;              // Port x Reset bit y
    static const uint8_t BRR_BR7 = 7;              // Port x Reset bit y
    static const uint8_t BRR_BR8 = 8;              // Port x Reset bit y
    static const uint8_t BRR_BR9 = 9;              // Port x Reset bit y
    static const uint8_t BRR_BR10 = 10;            // Port x Reset bit y
    static const uint8_t BRR_BR11 = 11;            // Port x Reset bit y
    static const uint8_t BRR_BR12 = 12;            // Port x Reset bit y
    static const uint8_t BRR_BR13 = 13;            // Port x Reset bit y
    static const uint8_t BRR_BR14 = 14;            // Port x Reset bit y
    static const uint8_t BRR_BR15 = 15;            // Port x Reset bit y
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpiob_t& GPIOB = *reinterpret_cast<gpiob_t*>(0x48000400);


////
//
//    General-purpose I/Os
//
////

struct gpioe_t
{
    volatile uint32_t    MODER;                // [Read-write] GPIO port mode register
    volatile uint32_t    OTYPER;               // [Read-write] GPIO port output type register
    volatile uint32_t    OSPEEDR;              // [Read-write] GPIO port output speed register
    volatile uint32_t    PUPDR;                // [Read-write] GPIO port pull-up/pull-down register
    volatile uint32_t    IDR;                  // [Read-only] GPIO port input data register
    volatile uint32_t    ODR;                  // [Read-write] GPIO port output data register
    volatile uint32_t    BSRR;                 // [Write-only] GPIO port bit set/reset register
    volatile uint32_t    LCKR;                 // [Read-write] GPIO port configuration lock register
    volatile uint32_t    AFRL;                 // [Read-write] GPIO alternate function low register
    volatile uint32_t    AFRH;                 // [Read-write] GPIO alternate function high register
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register

    static const uint8_t MODER_MODER15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t MODER_RESET_VALUE = 0x0;

    static const uint8_t OTYPER_OT15 = 15;            // Port x configuration bit 15
    static const uint8_t OTYPER_OT14 = 14;            // Port x configuration bit 14
    static const uint8_t OTYPER_OT13 = 13;            // Port x configuration bit 13
    static const uint8_t OTYPER_OT12 = 12;            // Port x configuration bit 12
    static const uint8_t OTYPER_OT11 = 11;            // Port x configuration bit 11
    static const uint8_t OTYPER_OT10 = 10;            // Port x configuration bit 10
    static const uint8_t OTYPER_OT9 = 9;              // Port x configuration bit 9
    static const uint8_t OTYPER_OT8 = 8;              // Port x configuration bit 8
    static const uint8_t OTYPER_OT7 = 7;              // Port x configuration bit 7
    static const uint8_t OTYPER_OT6 = 6;              // Port x configuration bit 6
    static const uint8_t OTYPER_OT5 = 5;              // Port x configuration bit 5
    static const uint8_t OTYPER_OT4 = 4;              // Port x configuration bit 4
    static const uint8_t OTYPER_OT3 = 3;              // Port x configuration bit 3
    static const uint8_t OTYPER_OT2 = 2;              // Port x configuration bit 2
    static const uint8_t OTYPER_OT1 = 1;              // Port x configuration bit 1
    static const uint8_t OTYPER_OT0 = 0;              // Port x configuration bit 0
    static const uint32_t OTYPER_RESET_VALUE = 0x0;

    static const uint8_t OSPEEDR_OSPEEDR15 = 30;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR14 = 28;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR13 = 26;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR12 = 24;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR11 = 22;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR10 = 20;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR9 = 18;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR8 = 16;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR7 = 14;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR6 = 12;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR5 = 10;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR4 = 8;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR3 = 6;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR2 = 4;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR1 = 2;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR0 = 0;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t OSPEEDR_RESET_VALUE = 0x0;

    static const uint8_t PUPDR_PUPDR15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t PUPDR_RESET_VALUE = 0x0;

    static const uint8_t IDR_IDR15 = 15;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR14 = 14;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR13 = 13;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR12 = 12;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR11 = 11;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR10 = 10;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR9 = 9;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR8 = 8;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR7 = 7;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR6 = 6;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR5 = 5;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR4 = 4;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR3 = 3;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR2 = 2;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR1 = 1;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR0 = 0;             // Port input data (y = 0..15)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static const uint8_t ODR_ODR15 = 15;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR14 = 14;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR13 = 13;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR12 = 12;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR11 = 11;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR10 = 10;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR9 = 9;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR8 = 8;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR7 = 7;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR6 = 6;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR5 = 5;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR4 = 4;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR3 = 3;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR2 = 2;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR1 = 1;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR0 = 0;             // Port output data (y = 0..15)
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static const uint8_t BSRR_BR15 = 31;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR14 = 30;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR13 = 29;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR12 = 28;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR11 = 27;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR10 = 26;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR9 = 25;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR8 = 24;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR7 = 23;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR6 = 22;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR5 = 21;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR4 = 20;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR3 = 19;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR2 = 18;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR1 = 17;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR0 = 16;             // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS15 = 15;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS14 = 14;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS13 = 13;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS12 = 12;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS11 = 11;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS10 = 10;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS9 = 9;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS8 = 8;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS7 = 7;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS6 = 6;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS5 = 5;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS4 = 4;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS3 = 3;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS2 = 2;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS1 = 1;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS0 = 0;              // Port x set bit y (y= 0..15)
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static const uint8_t LCKR_LCKK = 16;            // Port x lock bit y
    static const uint8_t LCKR_LCK15 = 15;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK14 = 14;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK13 = 13;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK12 = 12;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK11 = 11;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK10 = 10;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK9 = 9;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK8 = 8;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK7 = 7;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK6 = 6;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK5 = 5;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK4 = 4;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK3 = 3;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK2 = 2;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK1 = 1;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK0 = 0;             // Port x lock bit y (y= 0..15)
    static const uint32_t LCKR_RESET_VALUE = 0x0;

    static const uint8_t AFRL_AFRL7 = 28;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL6 = 24;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL5 = 20;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL4 = 16;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL3 = 12;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL2 = 8;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL1 = 4;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL0 = 0;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    static const uint8_t AFRH_AFRH15 = 28;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH14 = 24;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH13 = 20;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH12 = 16;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH11 = 12;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH10 = 8;           // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH9 = 4;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH8 = 0;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static const uint8_t BRR_BR0 = 0;              // Port x Reset bit y
    static const uint8_t BRR_BR1 = 1;              // Port x Reset bit y
    static const uint8_t BRR_BR2 = 2;              // Port x Reset bit y
    static const uint8_t BRR_BR3 = 3;              // Port x Reset bit y
    static const uint8_t BRR_BR4 = 4;              // Port x Reset bit y
    static const uint8_t BRR_BR5 = 5;              // Port x Reset bit y
    static const uint8_t BRR_BR6 = 6;              // Port x Reset bit y
    static const uint8_t BRR_BR7 = 7;              // Port x Reset bit y
    static const uint8_t BRR_BR8 = 8;              // Port x Reset bit y
    static const uint8_t BRR_BR9 = 9;              // Port x Reset bit y
    static const uint8_t BRR_BR10 = 10;            // Port x Reset bit y
    static const uint8_t BRR_BR11 = 11;            // Port x Reset bit y
    static const uint8_t BRR_BR12 = 12;            // Port x Reset bit y
    static const uint8_t BRR_BR13 = 13;            // Port x Reset bit y
    static const uint8_t BRR_BR14 = 14;            // Port x Reset bit y
    static const uint8_t BRR_BR15 = 15;            // Port x Reset bit y
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpioe_t& GPIOE = *reinterpret_cast<gpioe_t*>(0x48001000);


////
//
//    General-purpose I/Os
//
////

struct gpioa_t
{
    volatile uint32_t    MODER;                // [Read-write] GPIO port mode register
    volatile uint32_t    OTYPER;               // [Read-write] GPIO port output type register
    volatile uint32_t    OSPEEDR;              // [Read-write] GPIO port output speed register
    volatile uint32_t    PUPDR;                // [Read-write] GPIO port pull-up/pull-down register
    volatile uint32_t    IDR;                  // [Read-only] GPIO port input data register
    volatile uint32_t    ODR;                  // [Read-write] GPIO port output data register
    volatile uint32_t    BSRR;                 // [Write-only] GPIO port bit set/reset register
    volatile uint32_t    LCKR;                 // [Read-write] GPIO port configuration lock register
    volatile uint32_t    AFRL;                 // [Read-write] GPIO alternate function low register
    volatile uint32_t    AFRH;                 // [Read-write] GPIO alternate function high register
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register

    static const uint8_t MODER_MODER15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER_MODER0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t MODER_RESET_VALUE = 0x28000000;

    static const uint8_t OTYPER_OT15 = 15;            // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT14 = 14;            // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT13 = 13;            // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT12 = 12;            // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT11 = 11;            // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT10 = 10;            // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT9 = 9;              // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT8 = 8;              // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT7 = 7;              // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT6 = 6;              // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT5 = 5;              // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT4 = 4;              // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT3 = 3;              // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT2 = 2;              // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT1 = 1;              // Port x configuration bits (y = 0..15)
    static const uint8_t OTYPER_OT0 = 0;              // Port x configuration bits (y = 0..15)
    static const uint32_t OTYPER_RESET_VALUE = 0x0;

    static const uint8_t OSPEEDR_OSPEEDR15 = 30;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR14 = 28;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR13 = 26;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR12 = 24;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR11 = 22;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR10 = 20;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR9 = 18;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR8 = 16;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR7 = 14;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR6 = 12;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR5 = 10;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR4 = 8;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR3 = 6;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR2 = 4;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR1 = 2;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR_OSPEEDR0 = 0;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t OSPEEDR_RESET_VALUE = 0x0;

    static const uint8_t PUPDR_PUPDR15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR_PUPDR0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint32_t PUPDR_RESET_VALUE = 0x24000000;

    static const uint8_t IDR_IDR15 = 15;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR14 = 14;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR13 = 13;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR12 = 12;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR11 = 11;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR10 = 10;           // Port input data (y = 0..15)
    static const uint8_t IDR_IDR9 = 9;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR8 = 8;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR7 = 7;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR6 = 6;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR5 = 5;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR4 = 4;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR3 = 3;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR2 = 2;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR1 = 1;             // Port input data (y = 0..15)
    static const uint8_t IDR_IDR0 = 0;             // Port input data (y = 0..15)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static const uint8_t ODR_ODR15 = 15;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR14 = 14;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR13 = 13;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR12 = 12;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR11 = 11;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR10 = 10;           // Port output data (y = 0..15)
    static const uint8_t ODR_ODR9 = 9;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR8 = 8;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR7 = 7;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR6 = 6;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR5 = 5;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR4 = 4;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR3 = 3;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR2 = 2;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR1 = 1;             // Port output data (y = 0..15)
    static const uint8_t ODR_ODR0 = 0;             // Port output data (y = 0..15)
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static const uint8_t BSRR_BR15 = 31;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR14 = 30;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR13 = 29;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR12 = 28;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR11 = 27;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR10 = 26;            // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR9 = 25;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR8 = 24;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR7 = 23;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR6 = 22;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR5 = 21;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR4 = 20;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR3 = 19;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR2 = 18;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR1 = 17;             // Port x reset bit y (y = 0..15)
    static const uint8_t BSRR_BR0 = 16;             // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS15 = 15;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS14 = 14;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS13 = 13;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS12 = 12;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS11 = 11;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS10 = 10;            // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS9 = 9;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS8 = 8;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS7 = 7;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS6 = 6;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS5 = 5;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS4 = 4;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS3 = 3;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS2 = 2;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS1 = 1;              // Port x set bit y (y= 0..15)
    static const uint8_t BSRR_BS0 = 0;              // Port x set bit y (y= 0..15)
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static const uint8_t LCKR_LCKK = 16;            // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK15 = 15;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK14 = 14;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK13 = 13;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK12 = 12;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK11 = 11;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK10 = 10;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK9 = 9;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK8 = 8;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK7 = 7;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK6 = 6;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK5 = 5;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK4 = 4;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK3 = 3;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK2 = 2;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK1 = 1;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCKR_LCK0 = 0;             // Port x lock bit y (y= 0..15)
    static const uint32_t LCKR_RESET_VALUE = 0x0;

    static const uint8_t AFRL_AFRL7 = 28;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL6 = 24;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL5 = 20;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL4 = 16;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL3 = 12;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL2 = 8;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL1 = 4;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL_AFRL0 = 0;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    static const uint8_t AFRH_AFRH15 = 28;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH14 = 24;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH13 = 20;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH12 = 16;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH11 = 12;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH10 = 8;           // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH9 = 4;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH_AFRH8 = 0;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static const uint8_t BRR_BR0 = 0;              // Port x Reset bit y
    static const uint8_t BRR_BR1 = 1;              // Port x Reset bit y
    static const uint8_t BRR_BR2 = 2;              // Port x Reset bit y
    static const uint8_t BRR_BR3 = 3;              // Port x Reset bit y
    static const uint8_t BRR_BR4 = 4;              // Port x Reset bit y
    static const uint8_t BRR_BR5 = 5;              // Port x Reset bit y
    static const uint8_t BRR_BR6 = 6;              // Port x Reset bit y
    static const uint8_t BRR_BR7 = 7;              // Port x Reset bit y
    static const uint8_t BRR_BR8 = 8;              // Port x Reset bit y
    static const uint8_t BRR_BR9 = 9;              // Port x Reset bit y
    static const uint8_t BRR_BR10 = 10;            // Port x Reset bit y
    static const uint8_t BRR_BR11 = 11;            // Port x Reset bit y
    static const uint8_t BRR_BR12 = 12;            // Port x Reset bit y
    static const uint8_t BRR_BR13 = 13;            // Port x Reset bit y
    static const uint8_t BRR_BR14 = 14;            // Port x Reset bit y
    static const uint8_t BRR_BR15 = 15;            // Port x Reset bit y
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpioa_t& GPIOA = *reinterpret_cast<gpioa_t*>(0x48000000);


////
//
//    Serial peripheral interface
//
////

struct spi1_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SR;                   // status register
    volatile uint32_t    DR;                   // [Read-write] data register
    volatile uint32_t    CRCPR;                // [Read-write] CRC polynomial register
    volatile uint32_t    RXCRCR;               // [Read-only] RX CRC register
    volatile uint32_t    TXCRCR;               // [Read-only] TX CRC register
    volatile uint32_t    I2SCFGR;              // [Read-write] I2S configuration register
    volatile uint32_t    I2SPR;                // [Read-write] I2S prescaler register

    static const uint8_t CR1_BIDIMODE = 15;        // Bidirectional data mode enable
    static const uint8_t CR1_BIDIOE = 14;          // Output enable in bidirectional mode
    static const uint8_t CR1_CRCEN = 13;           // Hardware CRC calculation enable
    static const uint8_t CR1_CRCNEXT = 12;         // CRC transfer next
    static const uint8_t CR1_DFF = 11;             // Data frame format
    static const uint8_t CR1_RXONLY = 10;          // Receive only
    static const uint8_t CR1_SSM = 9;              // Software slave management
    static const uint8_t CR1_SSI = 8;              // Internal slave select
    static const uint8_t CR1_LSBFIRST = 7;         // Frame format
    static const uint8_t CR1_SPE = 6;              // SPI enable
    static const uint8_t CR1_BR = 3;               // Baud rate control (3 bits)
    static const uint8_t CR1_MSTR = 2;             // Master selection
    static const uint8_t CR1_CPOL = 1;             // Clock polarity
    static const uint8_t CR1_CPHA = 0;             // Clock phase
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_RXDMAEN = 0;          // Rx buffer DMA enable
    static const uint8_t CR2_TXDMAEN = 1;          // Tx buffer DMA enable
    static const uint8_t CR2_SSOE = 2;             // SS output enable
    static const uint8_t CR2_NSSP = 3;             // NSS pulse management
    static const uint8_t CR2_FRF = 4;              // Frame format
    static const uint8_t CR2_ERRIE = 5;            // Error interrupt enable
    static const uint8_t CR2_RXNEIE = 6;           // RX buffer not empty interrupt enable
    static const uint8_t CR2_TXEIE = 7;            // Tx buffer empty interrupt enable
    static const uint8_t CR2_DS = 8;               // Data size (4 bits)
    static const uint8_t CR2_FRXTH = 12;           // FIFO reception threshold
    static const uint8_t CR2_LDMA_RX = 13;         // Last DMA transfer for reception
    static const uint8_t CR2_LDMA_TX = 14;         // Last DMA transfer for transmission
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t SR_RXNE = 0;             // Receive buffer not empty, Read-only
    static const uint8_t SR_TXE = 1;              // Transmit buffer empty, Read-only
    static const uint8_t SR_CHSIDE = 2;           // Channel side, Read-only
    static const uint8_t SR_UDR = 3;              // Underrun flag, Read-only
    static const uint8_t SR_CRCERR = 4;           // CRC error flag, Read-write
    static const uint8_t SR_MODF = 5;             // Mode fault, Read-only
    static const uint8_t SR_OVR = 6;              // Overrun flag, Read-only
    static const uint8_t SR_BSY = 7;              // Busy flag, Read-only
    static const uint8_t SR_TIFRFE = 8;           // TI frame format error, Read-only
    static const uint8_t SR_FRLVL = 9;            // FIFO reception level (2 bits), Read-only
    static const uint8_t SR_FTLVL = 11;           // FIFO transmission level (2 bits), Read-only
    static const uint32_t SR_RESET_VALUE = 0x2;

    static const uint8_t DR_DR = 0;               // Data register (16 bits)
    static const uint32_t DR_RESET_VALUE = 0x0;

    static const uint8_t CRCPR_CRCPOLY = 0;          // CRC polynomial register (16 bits)
    static const uint32_t CRCPR_RESET_VALUE = 0x7;

    static const uint8_t RXCRCR_RxCRC = 0;            // Rx CRC register (16 bits)
    static const uint32_t RXCRCR_RESET_VALUE = 0x0;

    static const uint8_t TXCRCR_TxCRC = 0;            // Tx CRC register (16 bits)
    static const uint32_t TXCRCR_RESET_VALUE = 0x0;

    static const uint8_t I2SCFGR_I2SMOD = 11;          // I2S mode selection
    static const uint8_t I2SCFGR_I2SE = 10;            // I2S Enable
    static const uint8_t I2SCFGR_I2SCFG = 8;           // I2S configuration mode (2 bits)
    static const uint8_t I2SCFGR_PCMSYNC = 7;          // PCM frame synchronization
    static const uint8_t I2SCFGR_I2SSTD = 4;           // I2S standard selection (2 bits)
    static const uint8_t I2SCFGR_CKPOL = 3;            // Steady state clock polarity
    static const uint8_t I2SCFGR_DATLEN = 1;           // Data length to be transferred (2 bits)
    static const uint8_t I2SCFGR_CHLEN = 0;            // Channel length (number of bits per audio channel)
    static const uint32_t I2SCFGR_RESET_VALUE = 0x0;

    static const uint8_t I2SPR_MCKOE = 9;            // Master clock output enable
    static const uint8_t I2SPR_ODD = 8;              // Odd factor for the prescaler
    static const uint8_t I2SPR_I2SDIV = 0;           // I2S Linear prescaler (8 bits)
    static const uint32_t I2SPR_RESET_VALUE = 0x10;
};

static spi1_t& SPI1 = *reinterpret_cast<spi1_t*>(0x40013000);


////
//
//    Serial peripheral interface
//
////

struct spi2_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SR;                   // status register
    volatile uint32_t    DR;                   // [Read-write] data register
    volatile uint32_t    CRCPR;                // [Read-write] CRC polynomial register
    volatile uint32_t    RXCRCR;               // [Read-only] RX CRC register
    volatile uint32_t    TXCRCR;               // [Read-only] TX CRC register
    volatile uint32_t    I2SCFGR;              // [Read-write] I2S configuration register
    volatile uint32_t    I2SPR;                // [Read-write] I2S prescaler register

    static const uint8_t CR1_BIDIMODE = 15;        // Bidirectional data mode enable
    static const uint8_t CR1_BIDIOE = 14;          // Output enable in bidirectional mode
    static const uint8_t CR1_CRCEN = 13;           // Hardware CRC calculation enable
    static const uint8_t CR1_CRCNEXT = 12;         // CRC transfer next
    static const uint8_t CR1_DFF = 11;             // Data frame format
    static const uint8_t CR1_RXONLY = 10;          // Receive only
    static const uint8_t CR1_SSM = 9;              // Software slave management
    static const uint8_t CR1_SSI = 8;              // Internal slave select
    static const uint8_t CR1_LSBFIRST = 7;         // Frame format
    static const uint8_t CR1_SPE = 6;              // SPI enable
    static const uint8_t CR1_BR = 3;               // Baud rate control (3 bits)
    static const uint8_t CR1_MSTR = 2;             // Master selection
    static const uint8_t CR1_CPOL = 1;             // Clock polarity
    static const uint8_t CR1_CPHA = 0;             // Clock phase
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_RXDMAEN = 0;          // Rx buffer DMA enable
    static const uint8_t CR2_TXDMAEN = 1;          // Tx buffer DMA enable
    static const uint8_t CR2_SSOE = 2;             // SS output enable
    static const uint8_t CR2_NSSP = 3;             // NSS pulse management
    static const uint8_t CR2_FRF = 4;              // Frame format
    static const uint8_t CR2_ERRIE = 5;            // Error interrupt enable
    static const uint8_t CR2_RXNEIE = 6;           // RX buffer not empty interrupt enable
    static const uint8_t CR2_TXEIE = 7;            // Tx buffer empty interrupt enable
    static const uint8_t CR2_DS = 8;               // Data size (4 bits)
    static const uint8_t CR2_FRXTH = 12;           // FIFO reception threshold
    static const uint8_t CR2_LDMA_RX = 13;         // Last DMA transfer for reception
    static const uint8_t CR2_LDMA_TX = 14;         // Last DMA transfer for transmission
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t SR_RXNE = 0;             // Receive buffer not empty, Read-only
    static const uint8_t SR_TXE = 1;              // Transmit buffer empty, Read-only
    static const uint8_t SR_CHSIDE = 2;           // Channel side, Read-only
    static const uint8_t SR_UDR = 3;              // Underrun flag, Read-only
    static const uint8_t SR_CRCERR = 4;           // CRC error flag, Read-write
    static const uint8_t SR_MODF = 5;             // Mode fault, Read-only
    static const uint8_t SR_OVR = 6;              // Overrun flag, Read-only
    static const uint8_t SR_BSY = 7;              // Busy flag, Read-only
    static const uint8_t SR_TIFRFE = 8;           // TI frame format error, Read-only
    static const uint8_t SR_FRLVL = 9;            // FIFO reception level (2 bits), Read-only
    static const uint8_t SR_FTLVL = 11;           // FIFO transmission level (2 bits), Read-only
    static const uint32_t SR_RESET_VALUE = 0x2;

    static const uint8_t DR_DR = 0;               // Data register (16 bits)
    static const uint32_t DR_RESET_VALUE = 0x0;

    static const uint8_t CRCPR_CRCPOLY = 0;          // CRC polynomial register (16 bits)
    static const uint32_t CRCPR_RESET_VALUE = 0x7;

    static const uint8_t RXCRCR_RxCRC = 0;            // Rx CRC register (16 bits)
    static const uint32_t RXCRCR_RESET_VALUE = 0x0;

    static const uint8_t TXCRCR_TxCRC = 0;            // Tx CRC register (16 bits)
    static const uint32_t TXCRCR_RESET_VALUE = 0x0;

    static const uint8_t I2SCFGR_I2SMOD = 11;          // I2S mode selection
    static const uint8_t I2SCFGR_I2SE = 10;            // I2S Enable
    static const uint8_t I2SCFGR_I2SCFG = 8;           // I2S configuration mode (2 bits)
    static const uint8_t I2SCFGR_PCMSYNC = 7;          // PCM frame synchronization
    static const uint8_t I2SCFGR_I2SSTD = 4;           // I2S standard selection (2 bits)
    static const uint8_t I2SCFGR_CKPOL = 3;            // Steady state clock polarity
    static const uint8_t I2SCFGR_DATLEN = 1;           // Data length to be transferred (2 bits)
    static const uint8_t I2SCFGR_CHLEN = 0;            // Channel length (number of bits per audio channel)
    static const uint32_t I2SCFGR_RESET_VALUE = 0x0;

    static const uint8_t I2SPR_MCKOE = 9;            // Master clock output enable
    static const uint8_t I2SPR_ODD = 8;              // Odd factor for the prescaler
    static const uint8_t I2SPR_I2SDIV = 0;           // I2S Linear prescaler (8 bits)
    static const uint32_t I2SPR_RESET_VALUE = 0x10;
};

static spi2_t& SPI2 = *reinterpret_cast<spi2_t*>(0x40003800);


////
//
//    Power control
//
////

struct pwr_t
{
    volatile uint32_t    CR;                   // [Read-write] power control register
    volatile uint32_t    CSR;                  // power control/status register

    static const uint8_t CR_DBP = 8;              // Disable backup domain write protection
    static const uint8_t CR_PLS = 5;              // PVD level selection (3 bits)
    static const uint8_t CR_PVDE = 4;             // Power voltage detector enable
    static const uint8_t CR_CSBF = 3;             // Clear standby flag
    static const uint8_t CR_CWUF = 2;             // Clear wakeup flag
    static const uint8_t CR_PDDS = 1;             // Power down deepsleep
    static const uint8_t CR_LPDS = 0;             // Low-power deep sleep
    static const uint32_t CR_RESET_VALUE = 0x0;

    static const uint8_t CSR_WUF = 0;              // Wakeup flag, Read-only
    static const uint8_t CSR_SBF = 1;              // Standby flag, Read-only
    static const uint8_t CSR_PVDO = 2;             // PVD output, Read-only
    static const uint8_t CSR_VREFINTRDY = 3;       // VREFINT reference voltage ready, Read-only
    static const uint8_t CSR_EWUP1 = 8;            // Enable WKUP pin 1, Read-write
    static const uint8_t CSR_EWUP2 = 9;            // Enable WKUP pin 2, Read-write
    static const uint8_t CSR_EWUP3 = 10;           // Enable WKUP pin 3, Read-write
    static const uint8_t CSR_EWUP4 = 11;           // Enable WKUP pin 4, Read-write
    static const uint8_t CSR_EWUP5 = 12;           // Enable WKUP pin 5, Read-write
    static const uint8_t CSR_EWUP6 = 13;           // Enable WKUP pin 6, Read-write
    static const uint8_t CSR_EWUP7 = 14;           // Enable WKUP pin 7, Read-write
    static const uint8_t CSR_EWUP8 = 15;           // Enable WKUP pin 8, Read-write
    static const uint32_t CSR_RESET_VALUE = 0x0;
};

static pwr_t& PWR = *reinterpret_cast<pwr_t*>(0x40007000);


////
//
//    Inter-integrated circuit
//
////

struct i2c1_t
{
    volatile uint32_t    CR1;                  // Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    OAR1;                 // [Read-write] Own address register 1
    volatile uint32_t    OAR2;                 // [Read-write] Own address register 2
    volatile uint32_t    TIMINGR;              // [Read-write] Timing register
    volatile uint32_t    TIMEOUTR;             // [Read-write] Status register 1
    volatile uint32_t    ISR;                  // Interrupt and Status register
    volatile uint32_t    ICR;                  // [Write-only] Interrupt clear register
    volatile uint32_t    PECR;                 // [Read-only] PEC register
    volatile uint32_t    RXDR;                 // [Read-only] Receive data register
    volatile uint32_t    TXDR;                 // [Read-write] Transmit data register

    static const uint8_t CR1_PE = 0;               // Peripheral enable, Read-write
    static const uint8_t CR1_TXIE = 1;             // TX Interrupt enable, Read-write
    static const uint8_t CR1_RXIE = 2;             // RX Interrupt enable, Read-write
    static const uint8_t CR1_ADDRIE = 3;           // Address match interrupt enable (slave only), Read-write
    static const uint8_t CR1_NACKIE = 4;           // Not acknowledge received interrupt enable, Read-write
    static const uint8_t CR1_STOPIE = 5;           // STOP detection Interrupt enable, Read-write
    static const uint8_t CR1_TCIE = 6;             // Transfer Complete interrupt enable, Read-write
    static const uint8_t CR1_ERRIE = 7;            // Error interrupts enable, Read-write
    static const uint8_t CR1_DNF = 8;              // Digital noise filter (4 bits), Read-write
    static const uint8_t CR1_ANFOFF = 12;          // Analog noise filter OFF, Read-write
    static const uint8_t CR1_SWRST = 13;           // Software reset, Write-only
    static const uint8_t CR1_TXDMAEN = 14;         // DMA transmission requests enable, Read-write
    static const uint8_t CR1_RXDMAEN = 15;         // DMA reception requests enable, Read-write
    static const uint8_t CR1_SBC = 16;             // Slave byte control, Read-write
    static const uint8_t CR1_NOSTRETCH = 17;       // Clock stretching disable, Read-write
    static const uint8_t CR1_WUPEN = 18;           // Wakeup from STOP enable, Read-write
    static const uint8_t CR1_GCEN = 19;            // General call enable, Read-write
    static const uint8_t CR1_SMBHEN = 20;          // SMBus Host address enable, Read-write
    static const uint8_t CR1_SMBDEN = 21;          // SMBus Device Default address enable, Read-write
    static const uint8_t CR1_ALERTEN = 22;         // SMBUS alert enable, Read-write
    static const uint8_t CR1_PECEN = 23;           // PEC enable, Read-write
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_PECBYTE = 26;         // Packet error checking byte
    static const uint8_t CR2_AUTOEND = 25;         // Automatic end mode (master mode)
    static const uint8_t CR2_RELOAD = 24;          // NBYTES reload mode
    static const uint8_t CR2_NBYTES = 16;          // Number of bytes (8 bits)
    static const uint8_t CR2_NACK = 15;            // NACK generation (slave mode)
    static const uint8_t CR2_STOP = 14;            // Stop generation (master mode)
    static const uint8_t CR2_START = 13;           // Start generation
    static const uint8_t CR2_HEAD10R = 12;         // 10-bit address header only read direction (master receiver mode)
    static const uint8_t CR2_ADD10 = 11;           // 10-bit addressing mode (master mode)
    static const uint8_t CR2_RD_WRN = 10;          // Transfer direction (master mode)
    static const uint8_t CR2_SADD8 = 8;            // Slave address bit 9:8 (master mode) (2 bits)
    static const uint8_t CR2_SADD1 = 1;            // Slave address bit 7:1 (master mode) (7 bits)
    static const uint8_t CR2_SADD0 = 0;            // Slave address bit 0 (master mode)
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t OAR1_OA1_0 = 0;            // Interface address
    static const uint8_t OAR1_OA1_1 = 1;            // Interface address (7 bits)
    static const uint8_t OAR1_OA1_8 = 8;            // Interface address (2 bits)
    static const uint8_t OAR1_OA1MODE = 10;         // Own Address 1 10-bit mode
    static const uint8_t OAR1_OA1EN = 15;           // Own Address 1 enable
    static const uint32_t OAR1_RESET_VALUE = 0x0;

    static const uint8_t OAR2_OA2 = 1;              // Interface address (7 bits)
    static const uint8_t OAR2_OA2MSK = 8;           // Own Address 2 masks (3 bits)
    static const uint8_t OAR2_OA2EN = 15;           // Own Address 2 enable
    static const uint32_t OAR2_RESET_VALUE = 0x0;

    static const uint8_t TIMINGR_SCLL = 0;             // SCL low period (master mode) (8 bits)
    static const uint8_t TIMINGR_SCLH = 8;             // SCL high period (master mode) (8 bits)
    static const uint8_t TIMINGR_SDADEL = 16;          // Data hold time (4 bits)
    static const uint8_t TIMINGR_SCLDEL = 20;          // Data setup time (4 bits)
    static const uint8_t TIMINGR_PRESC = 28;           // Timing prescaler (4 bits)
    static const uint32_t TIMINGR_RESET_VALUE = 0x0;

    static const uint8_t TIMEOUTR_TIMEOUTA = 0;         // Bus timeout A (12 bits)
    static const uint8_t TIMEOUTR_TIDLE = 12;           // Idle clock timeout detection
    static const uint8_t TIMEOUTR_TIMOUTEN = 15;        // Clock timeout enable
    static const uint8_t TIMEOUTR_TIMEOUTB = 16;        // Bus timeout B (12 bits)
    static const uint8_t TIMEOUTR_TEXTEN = 31;          // Extended clock timeout enable
    static const uint32_t TIMEOUTR_RESET_VALUE = 0x0;

    static const uint8_t ISR_ADDCODE = 17;         // Address match code (Slave mode) (7 bits), Read-only
    static const uint8_t ISR_DIR = 16;             // Transfer direction (Slave mode), Read-only
    static const uint8_t ISR_BUSY = 15;            // Bus busy, Read-only
    static const uint8_t ISR_ALERT = 13;           // SMBus alert, Read-only
    static const uint8_t ISR_TIMEOUT = 12;         // Timeout or t_low detection flag, Read-only
    static const uint8_t ISR_PECERR = 11;          // PEC Error in reception, Read-only
    static const uint8_t ISR_OVR = 10;             // Overrun/Underrun (slave mode), Read-only
    static const uint8_t ISR_ARLO = 9;             // Arbitration lost, Read-only
    static const uint8_t ISR_BERR = 8;             // Bus error, Read-only
    static const uint8_t ISR_TCR = 7;              // Transfer Complete Reload, Read-only
    static const uint8_t ISR_TC = 6;               // Transfer Complete (master mode), Read-only
    static const uint8_t ISR_STOPF = 5;            // Stop detection flag, Read-only
    static const uint8_t ISR_NACKF = 4;            // Not acknowledge received flag, Read-only
    static const uint8_t ISR_ADDR = 3;             // Address matched (slave mode), Read-only
    static const uint8_t ISR_RXNE = 2;             // Receive data register not empty (receivers), Read-only
    static const uint8_t ISR_TXIS = 1;             // Transmit interrupt status (transmitters), Read-write
    static const uint8_t ISR_TXE = 0;              // Transmit data register empty (transmitters), Read-write
    static const uint32_t ISR_RESET_VALUE = 0x1;

    static const uint8_t ICR_ALERTCF = 13;         // Alert flag clear
    static const uint8_t ICR_TIMOUTCF = 12;        // Timeout detection flag clear
    static const uint8_t ICR_PECCF = 11;           // PEC Error flag clear
    static const uint8_t ICR_OVRCF = 10;           // Overrun/Underrun flag clear
    static const uint8_t ICR_ARLOCF = 9;           // Arbitration lost flag clear
    static const uint8_t ICR_BERRCF = 8;           // Bus error flag clear
    static const uint8_t ICR_STOPCF = 5;           // Stop detection flag clear
    static const uint8_t ICR_NACKCF = 4;           // Not Acknowledge flag clear
    static const uint8_t ICR_ADDRCF = 3;           // Address Matched flag clear
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static const uint8_t PECR_PEC = 0;              // Packet error checking register (8 bits)
    static const uint32_t PECR_RESET_VALUE = 0x0;

    static const uint8_t RXDR_RXDATA = 0;           // 8-bit receive data (8 bits)
    static const uint32_t RXDR_RESET_VALUE = 0x0;

    static const uint8_t TXDR_TXDATA = 0;           // 8-bit transmit data (8 bits)
    static const uint32_t TXDR_RESET_VALUE = 0x0;
};

static i2c1_t& I2C1 = *reinterpret_cast<i2c1_t*>(0x40005400);


////
//
//    Inter-integrated circuit
//
////

struct i2c2_t
{
    volatile uint32_t    CR1;                  // Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    OAR1;                 // [Read-write] Own address register 1
    volatile uint32_t    OAR2;                 // [Read-write] Own address register 2
    volatile uint32_t    TIMINGR;              // [Read-write] Timing register
    volatile uint32_t    TIMEOUTR;             // [Read-write] Status register 1
    volatile uint32_t    ISR;                  // Interrupt and Status register
    volatile uint32_t    ICR;                  // [Write-only] Interrupt clear register
    volatile uint32_t    PECR;                 // [Read-only] PEC register
    volatile uint32_t    RXDR;                 // [Read-only] Receive data register
    volatile uint32_t    TXDR;                 // [Read-write] Transmit data register

    static const uint8_t CR1_PE = 0;               // Peripheral enable, Read-write
    static const uint8_t CR1_TXIE = 1;             // TX Interrupt enable, Read-write
    static const uint8_t CR1_RXIE = 2;             // RX Interrupt enable, Read-write
    static const uint8_t CR1_ADDRIE = 3;           // Address match interrupt enable (slave only), Read-write
    static const uint8_t CR1_NACKIE = 4;           // Not acknowledge received interrupt enable, Read-write
    static const uint8_t CR1_STOPIE = 5;           // STOP detection Interrupt enable, Read-write
    static const uint8_t CR1_TCIE = 6;             // Transfer Complete interrupt enable, Read-write
    static const uint8_t CR1_ERRIE = 7;            // Error interrupts enable, Read-write
    static const uint8_t CR1_DNF = 8;              // Digital noise filter (4 bits), Read-write
    static const uint8_t CR1_ANFOFF = 12;          // Analog noise filter OFF, Read-write
    static const uint8_t CR1_SWRST = 13;           // Software reset, Write-only
    static const uint8_t CR1_TXDMAEN = 14;         // DMA transmission requests enable, Read-write
    static const uint8_t CR1_RXDMAEN = 15;         // DMA reception requests enable, Read-write
    static const uint8_t CR1_SBC = 16;             // Slave byte control, Read-write
    static const uint8_t CR1_NOSTRETCH = 17;       // Clock stretching disable, Read-write
    static const uint8_t CR1_WUPEN = 18;           // Wakeup from STOP enable, Read-write
    static const uint8_t CR1_GCEN = 19;            // General call enable, Read-write
    static const uint8_t CR1_SMBHEN = 20;          // SMBus Host address enable, Read-write
    static const uint8_t CR1_SMBDEN = 21;          // SMBus Device Default address enable, Read-write
    static const uint8_t CR1_ALERTEN = 22;         // SMBUS alert enable, Read-write
    static const uint8_t CR1_PECEN = 23;           // PEC enable, Read-write
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_PECBYTE = 26;         // Packet error checking byte
    static const uint8_t CR2_AUTOEND = 25;         // Automatic end mode (master mode)
    static const uint8_t CR2_RELOAD = 24;          // NBYTES reload mode
    static const uint8_t CR2_NBYTES = 16;          // Number of bytes (8 bits)
    static const uint8_t CR2_NACK = 15;            // NACK generation (slave mode)
    static const uint8_t CR2_STOP = 14;            // Stop generation (master mode)
    static const uint8_t CR2_START = 13;           // Start generation
    static const uint8_t CR2_HEAD10R = 12;         // 10-bit address header only read direction (master receiver mode)
    static const uint8_t CR2_ADD10 = 11;           // 10-bit addressing mode (master mode)
    static const uint8_t CR2_RD_WRN = 10;          // Transfer direction (master mode)
    static const uint8_t CR2_SADD8 = 8;            // Slave address bit 9:8 (master mode) (2 bits)
    static const uint8_t CR2_SADD1 = 1;            // Slave address bit 7:1 (master mode) (7 bits)
    static const uint8_t CR2_SADD0 = 0;            // Slave address bit 0 (master mode)
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t OAR1_OA1_0 = 0;            // Interface address
    static const uint8_t OAR1_OA1_1 = 1;            // Interface address (7 bits)
    static const uint8_t OAR1_OA1_8 = 8;            // Interface address (2 bits)
    static const uint8_t OAR1_OA1MODE = 10;         // Own Address 1 10-bit mode
    static const uint8_t OAR1_OA1EN = 15;           // Own Address 1 enable
    static const uint32_t OAR1_RESET_VALUE = 0x0;

    static const uint8_t OAR2_OA2 = 1;              // Interface address (7 bits)
    static const uint8_t OAR2_OA2MSK = 8;           // Own Address 2 masks (3 bits)
    static const uint8_t OAR2_OA2EN = 15;           // Own Address 2 enable
    static const uint32_t OAR2_RESET_VALUE = 0x0;

    static const uint8_t TIMINGR_SCLL = 0;             // SCL low period (master mode) (8 bits)
    static const uint8_t TIMINGR_SCLH = 8;             // SCL high period (master mode) (8 bits)
    static const uint8_t TIMINGR_SDADEL = 16;          // Data hold time (4 bits)
    static const uint8_t TIMINGR_SCLDEL = 20;          // Data setup time (4 bits)
    static const uint8_t TIMINGR_PRESC = 28;           // Timing prescaler (4 bits)
    static const uint32_t TIMINGR_RESET_VALUE = 0x0;

    static const uint8_t TIMEOUTR_TIMEOUTA = 0;         // Bus timeout A (12 bits)
    static const uint8_t TIMEOUTR_TIDLE = 12;           // Idle clock timeout detection
    static const uint8_t TIMEOUTR_TIMOUTEN = 15;        // Clock timeout enable
    static const uint8_t TIMEOUTR_TIMEOUTB = 16;        // Bus timeout B (12 bits)
    static const uint8_t TIMEOUTR_TEXTEN = 31;          // Extended clock timeout enable
    static const uint32_t TIMEOUTR_RESET_VALUE = 0x0;

    static const uint8_t ISR_ADDCODE = 17;         // Address match code (Slave mode) (7 bits), Read-only
    static const uint8_t ISR_DIR = 16;             // Transfer direction (Slave mode), Read-only
    static const uint8_t ISR_BUSY = 15;            // Bus busy, Read-only
    static const uint8_t ISR_ALERT = 13;           // SMBus alert, Read-only
    static const uint8_t ISR_TIMEOUT = 12;         // Timeout or t_low detection flag, Read-only
    static const uint8_t ISR_PECERR = 11;          // PEC Error in reception, Read-only
    static const uint8_t ISR_OVR = 10;             // Overrun/Underrun (slave mode), Read-only
    static const uint8_t ISR_ARLO = 9;             // Arbitration lost, Read-only
    static const uint8_t ISR_BERR = 8;             // Bus error, Read-only
    static const uint8_t ISR_TCR = 7;              // Transfer Complete Reload, Read-only
    static const uint8_t ISR_TC = 6;               // Transfer Complete (master mode), Read-only
    static const uint8_t ISR_STOPF = 5;            // Stop detection flag, Read-only
    static const uint8_t ISR_NACKF = 4;            // Not acknowledge received flag, Read-only
    static const uint8_t ISR_ADDR = 3;             // Address matched (slave mode), Read-only
    static const uint8_t ISR_RXNE = 2;             // Receive data register not empty (receivers), Read-only
    static const uint8_t ISR_TXIS = 1;             // Transmit interrupt status (transmitters), Read-write
    static const uint8_t ISR_TXE = 0;              // Transmit data register empty (transmitters), Read-write
    static const uint32_t ISR_RESET_VALUE = 0x1;

    static const uint8_t ICR_ALERTCF = 13;         // Alert flag clear
    static const uint8_t ICR_TIMOUTCF = 12;        // Timeout detection flag clear
    static const uint8_t ICR_PECCF = 11;           // PEC Error flag clear
    static const uint8_t ICR_OVRCF = 10;           // Overrun/Underrun flag clear
    static const uint8_t ICR_ARLOCF = 9;           // Arbitration lost flag clear
    static const uint8_t ICR_BERRCF = 8;           // Bus error flag clear
    static const uint8_t ICR_STOPCF = 5;           // Stop detection flag clear
    static const uint8_t ICR_NACKCF = 4;           // Not Acknowledge flag clear
    static const uint8_t ICR_ADDRCF = 3;           // Address Matched flag clear
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static const uint8_t PECR_PEC = 0;              // Packet error checking register (8 bits)
    static const uint32_t PECR_RESET_VALUE = 0x0;

    static const uint8_t RXDR_RXDATA = 0;           // 8-bit receive data (8 bits)
    static const uint32_t RXDR_RESET_VALUE = 0x0;

    static const uint8_t TXDR_TXDATA = 0;           // 8-bit transmit data (8 bits)
    static const uint32_t TXDR_RESET_VALUE = 0x0;
};

static i2c2_t& I2C2 = *reinterpret_cast<i2c2_t*>(0x40005800);


////
//
//    Independent watchdog
//
////

struct iwdg_t
{
    volatile uint32_t    KR;                   // [Write-only] Key register
    volatile uint32_t    PR;                   // [Read-write] Prescaler register
    volatile uint32_t    RLR;                  // [Read-write] Reload register
    volatile uint32_t    SR;                   // [Read-only] Status register
    volatile uint32_t    WINR;                 // [Read-write] Window register

    static const uint8_t KR_KEY = 0;              // Key value (16 bits)
    static const uint32_t KR_RESET_VALUE = 0x0;

    static const uint8_t PR_PR = 0;               // Prescaler divider (3 bits)
    static const uint32_t PR_RESET_VALUE = 0x0;

    static const uint8_t RLR_RL = 0;               // Watchdog counter reload value (12 bits)
    static const uint32_t RLR_RESET_VALUE = 0xfff;

    static const uint8_t SR_PVU = 0;              // Watchdog prescaler value update
    static const uint8_t SR_RVU = 1;              // Watchdog counter reload value update
    static const uint8_t SR_WVU = 2;              // Watchdog counter window value update
    static const uint32_t SR_RESET_VALUE = 0x0;

    static const uint8_t WINR_WIN = 0;              // Watchdog counter window value (12 bits)
    static const uint32_t WINR_RESET_VALUE = 0xfff;
};

static iwdg_t& IWDG = *reinterpret_cast<iwdg_t*>(0x40003000);


////
//
//    Window watchdog
//
////

struct wwdg_t
{
    volatile uint32_t    CR;                   // [Read-write] Control register
    volatile uint32_t    CFR;                  // [Read-write] Configuration register
    volatile uint32_t    SR;                   // [Read-write] Status register

    static const uint8_t CR_WDGA = 7;             // Activation bit
    static const uint8_t CR_T = 0;                // 7-bit counter (7 bits)
    static const uint32_t CR_RESET_VALUE = 0x7f;

    static const uint8_t CFR_EWI = 9;              // Early wakeup interrupt
    static const uint8_t CFR_WDGTB = 7;            // Timer base (2 bits)
    static const uint8_t CFR_W = 0;                // 7-bit window value (7 bits)
    static const uint32_t CFR_RESET_VALUE = 0x7f;

    static const uint8_t SR_EWIF = 0;             // Early wakeup interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;
};

static wwdg_t& WWDG = *reinterpret_cast<wwdg_t*>(0x40002c00);


////
//
//    Advanced-timers
//
////

struct tim1_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMCR;                 // [Read-write] slave mode control register
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1;                // [Read-write] capture/compare mode register (output mode)
    volatile uint32_t    CCMR2;                // [Read-write] capture/compare mode register (output mode)
    volatile uint32_t    CCER;                 // [Read-write] capture/compare enable register
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    volatile uint32_t    RCR;                  // [Read-write] repetition counter register
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    volatile uint32_t    CCR2;                 // [Read-write] capture/compare register 2
    volatile uint32_t    CCR3;                 // [Read-write] capture/compare register 3
    volatile uint32_t    CCR4;                 // [Read-write] capture/compare register 4
    volatile uint32_t    BDTR;                 // [Read-write] break and dead-time register
    volatile uint32_t    DCR;                  // [Read-write] DMA control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static const uint8_t CR1_CKD = 8;              // Clock division (2 bits)
    static const uint8_t CR1_ARPE = 7;             // Auto-reload preload enable
    static const uint8_t CR1_CMS = 5;              // Center-aligned mode selection (2 bits)
    static const uint8_t CR1_DIR = 4;              // Direction
    static const uint8_t CR1_OPM = 3;              // One-pulse mode
    static const uint8_t CR1_URS = 2;              // Update request source
    static const uint8_t CR1_UDIS = 1;             // Update disable
    static const uint8_t CR1_CEN = 0;              // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_OIS4 = 14;            // Output Idle state 4
    static const uint8_t CR2_OIS3N = 13;           // Output Idle state 3
    static const uint8_t CR2_OIS3 = 12;            // Output Idle state 3
    static const uint8_t CR2_OIS2N = 11;           // Output Idle state 2
    static const uint8_t CR2_OIS2 = 10;            // Output Idle state 2
    static const uint8_t CR2_OIS1N = 9;            // Output Idle state 1
    static const uint8_t CR2_OIS1 = 8;             // Output Idle state 1
    static const uint8_t CR2_TI1S = 7;             // TI1 selection
    static const uint8_t CR2_MMS = 4;              // Master mode selection (3 bits)
    static const uint8_t CR2_CCDS = 3;             // Capture/compare DMA selection
    static const uint8_t CR2_CCUS = 2;             // Capture/compare control update selection
    static const uint8_t CR2_CCPC = 0;             // Capture/compare preloaded control
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t SMCR_ETP = 15;             // External trigger polarity
    static const uint8_t SMCR_ECE = 14;             // External clock enable
    static const uint8_t SMCR_ETPS = 12;            // External trigger prescaler (2 bits)
    static const uint8_t SMCR_ETF = 8;              // External trigger filter (4 bits)
    static const uint8_t SMCR_MSM = 7;              // Master/Slave mode
    static const uint8_t SMCR_TS = 4;               // Trigger selection (3 bits)
    static const uint8_t SMCR_SMS = 0;              // Slave mode selection (3 bits)
    static const uint32_t SMCR_RESET_VALUE = 0x0;

    static const uint8_t DIER_TDE = 14;             // Trigger DMA request enable
    static const uint8_t DIER_COMDE = 13;           // COM DMA request enable
    static const uint8_t DIER_CC4DE = 12;           // Capture/Compare 4 DMA request enable
    static const uint8_t DIER_CC3DE = 11;           // Capture/Compare 3 DMA request enable
    static const uint8_t DIER_CC2DE = 10;           // Capture/Compare 2 DMA request enable
    static const uint8_t DIER_CC1DE = 9;            // Capture/Compare 1 DMA request enable
    static const uint8_t DIER_UDE = 8;              // Update DMA request enable
    static const uint8_t DIER_BIE = 7;              // Break interrupt enable
    static const uint8_t DIER_TIE = 6;              // Trigger interrupt enable
    static const uint8_t DIER_COMIE = 5;            // COM interrupt enable
    static const uint8_t DIER_CC4IE = 4;            // Capture/Compare 4 interrupt enable
    static const uint8_t DIER_CC3IE = 3;            // Capture/Compare 3 interrupt enable
    static const uint8_t DIER_CC2IE = 2;            // Capture/Compare 2 interrupt enable
    static const uint8_t DIER_CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t DIER_UIE = 0;              // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static const uint8_t SR_CC4OF = 12;           // Capture/Compare 4 overcapture flag
    static const uint8_t SR_CC3OF = 11;           // Capture/Compare 3 overcapture flag
    static const uint8_t SR_CC2OF = 10;           // Capture/compare 2 overcapture flag
    static const uint8_t SR_CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t SR_BIF = 7;              // Break interrupt flag
    static const uint8_t SR_TIF = 6;              // Trigger interrupt flag
    static const uint8_t SR_COMIF = 5;            // COM interrupt flag
    static const uint8_t SR_CC4IF = 4;            // Capture/Compare 4 interrupt flag
    static const uint8_t SR_CC3IF = 3;            // Capture/Compare 3 interrupt flag
    static const uint8_t SR_CC2IF = 2;            // Capture/Compare 2 interrupt flag
    static const uint8_t SR_CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t SR_UIF = 0;              // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static const uint8_t EGR_BG = 7;               // Break generation
    static const uint8_t EGR_TG = 6;               // Trigger generation
    static const uint8_t EGR_COMG = 5;             // Capture/Compare control update generation
    static const uint8_t EGR_CC4G = 4;             // Capture/compare 4 generation
    static const uint8_t EGR_CC3G = 3;             // Capture/compare 3 generation
    static const uint8_t EGR_CC2G = 2;             // Capture/compare 2 generation
    static const uint8_t EGR_CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t EGR_UG = 0;               // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    static const uint8_t CCMR1_CC1S = 0;             // Capture/Compare 1 selection (2 bits)
    static const uint8_t CCMR1_CC2S = 8;             // Capture/Compare 2 selection (2 bits)
    static const uint8_t CCMR1_IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t CCMR1_IC1PCS = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CCMR1_IC2F = 12;            // Input capture 2 filter (4 bits)
    static const uint8_t CCMR1_IC2PCS = 10;          // Input capture 2 prescaler (2 bits)
    static const uint8_t CCMR1_OC1CE = 7;            // Output Compare 1 clear enable
    static const uint8_t CCMR1_OC1FE = 2;            // Output Compare 1 fast enable
    static const uint8_t CCMR1_OC1M = 4;             // Output Compare 1 mode (3 bits)
    static const uint8_t CCMR1_OC1PE = 3;            // Output Compare 1 preload enable
    static const uint8_t CCMR1_OC2CE = 15;           // Output Compare 2 clear enable
    static const uint8_t CCMR1_OC2FE = 10;           // Output Compare 2 fast enable
    static const uint8_t CCMR1_OC2M = 12;            // Output Compare 2 mode (3 bits)
    static const uint8_t CCMR1_OC2PE = 11;           // Output Compare 2 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static const uint8_t CCMR2_CC3S = 0;             // Capture/Compare 3 selection (2 bits)
    static const uint8_t CCMR2_CC4S = 8;             // Capture/Compare 4 selection (2 bits)
    static const uint8_t CCMR2_IC3F = 4;             // Input capture 3 filter (4 bits)
    static const uint8_t CCMR2_IC3PSC = 2;           // Input capture 3 prescaler (2 bits)
    static const uint8_t CCMR2_IC4F = 12;            // Input capture 4 filter (4 bits)
    static const uint8_t CCMR2_IC4PSC = 10;          // Input capture 4 prescaler (2 bits)
    static const uint8_t CCMR2_OC3CE = 7;            // Output compare 3 clear enable
    static const uint8_t CCMR2_OC3FE = 2;            // Output compare 3 fast enable
    static const uint8_t CCMR2_OC3M = 4;             // Output compare 3 mode (3 bits)
    static const uint8_t CCMR2_OC3PE = 3;            // Output compare 3 preload enable
    static const uint8_t CCMR2_OC4CE = 15;           // Output compare 4 clear enable
    static const uint8_t CCMR2_OC4FE = 10;           // Output compare 4 fast enable
    static const uint8_t CCMR2_OC4M = 12;            // Output compare 4 mode (3 bits)
    static const uint8_t CCMR2_OC4PE = 11;           // Output compare 4 preload enable
    static const uint32_t CCMR2_RESET_VALUE = 0x0;

    static const uint8_t CCER_CC4P = 13;            // Capture/Compare 3 output Polarity
    static const uint8_t CCER_CC4E = 12;            // Capture/Compare 4 output enable
    static const uint8_t CCER_CC3NP = 11;           // Capture/Compare 3 output Polarity
    static const uint8_t CCER_CC3NE = 10;           // Capture/Compare 3 complementary output enable
    static const uint8_t CCER_CC3P = 9;             // Capture/Compare 3 output Polarity
    static const uint8_t CCER_CC3E = 8;             // Capture/Compare 3 output enable
    static const uint8_t CCER_CC2NP = 7;            // Capture/Compare 2 output Polarity
    static const uint8_t CCER_CC2NE = 6;            // Capture/Compare 2 complementary output enable
    static const uint8_t CCER_CC2P = 5;             // Capture/Compare 2 output Polarity
    static const uint8_t CCER_CC2E = 4;             // Capture/Compare 2 output enable
    static const uint8_t CCER_CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1NE = 2;            // Capture/Compare 1 complementary output enable
    static const uint8_t CCER_CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1E = 0;             // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    static const uint8_t CNT_CNT = 0;              // counter value (16 bits)
    static const uint32_t CNT_RESET_VALUE = 0x0;

    static const uint8_t PSC_PSC = 0;              // Prescaler value (16 bits)
    static const uint32_t PSC_RESET_VALUE = 0x0;

    static const uint8_t ARR_ARR = 0;              // Auto-reload value (16 bits)
    static const uint32_t ARR_RESET_VALUE = 0x0;

    static const uint8_t RCR_REP = 0;              // Repetition counter value (8 bits)
    static const uint32_t RCR_RESET_VALUE = 0x0;

    static const uint8_t CCR1_CCR1 = 0;             // Capture/Compare 1 value (16 bits)
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    static const uint8_t CCR2_CCR2 = 0;             // Capture/Compare 2 value (16 bits)
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    static const uint8_t CCR3_CCR3 = 0;             // Capture/Compare 3 value (16 bits)
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    static const uint8_t CCR4_CCR4 = 0;             // Capture/Compare 3 value (16 bits)
    static const uint32_t CCR4_RESET_VALUE = 0x0;

    static const uint8_t BDTR_MOE = 15;             // Main output enable
    static const uint8_t BDTR_AOE = 14;             // Automatic output enable
    static const uint8_t BDTR_BKP = 13;             // Break polarity
    static const uint8_t BDTR_BKE = 12;             // Break enable
    static const uint8_t BDTR_OSSR = 11;            // Off-state selection for Run mode
    static const uint8_t BDTR_OSSI = 10;            // Off-state selection for Idle mode
    static const uint8_t BDTR_LOCK = 8;             // Lock configuration (2 bits)
    static const uint8_t BDTR_DTG = 0;              // Dead-time generator setup (8 bits)
    static const uint32_t BDTR_RESET_VALUE = 0x0;

    static const uint8_t DCR_DBL = 8;              // DMA burst length (5 bits)
    static const uint8_t DCR_DBA = 0;              // DMA base address (5 bits)
    static const uint32_t DCR_RESET_VALUE = 0x0;

    static const uint8_t DMAR_DMAB = 0;             // DMA register for burst accesses (16 bits)
    static const uint32_t DMAR_RESET_VALUE = 0x0;
};

static tim1_t& TIM1 = *reinterpret_cast<tim1_t*>(0x40012c00);


////
//
//    General-purpose-timers
//
////

struct tim2_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMCR;                 // [Read-write] slave mode control register
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1;                // [Read-write] capture/compare mode register 1 (output mode)
    volatile uint32_t    CCMR2;                // [Read-write] capture/compare mode register 2 (output mode)
    volatile uint32_t    CCER;                 // [Read-write] capture/compare enable register
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    reserved_t<1>        _0;
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    volatile uint32_t    CCR2;                 // [Read-write] capture/compare register 2
    volatile uint32_t    CCR3;                 // [Read-write] capture/compare register 3
    volatile uint32_t    CCR4;                 // [Read-write] capture/compare register 4
    reserved_t<1>        _1;
    volatile uint32_t    DCR;                  // [Read-write] DMA control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static const uint8_t CR1_CKD = 8;              // Clock division (2 bits)
    static const uint8_t CR1_ARPE = 7;             // Auto-reload preload enable
    static const uint8_t CR1_CMS = 5;              // Center-aligned mode selection (2 bits)
    static const uint8_t CR1_DIR = 4;              // Direction
    static const uint8_t CR1_OPM = 3;              // One-pulse mode
    static const uint8_t CR1_URS = 2;              // Update request source
    static const uint8_t CR1_UDIS = 1;             // Update disable
    static const uint8_t CR1_CEN = 0;              // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_TI1S = 7;             // TI1 selection
    static const uint8_t CR2_MMS = 4;              // Master mode selection (3 bits)
    static const uint8_t CR2_CCDS = 3;             // Capture/compare DMA selection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t SMCR_ETP = 15;             // External trigger polarity
    static const uint8_t SMCR_ECE = 14;             // External clock enable
    static const uint8_t SMCR_ETPS = 12;            // External trigger prescaler (2 bits)
    static const uint8_t SMCR_ETF = 8;              // External trigger filter (4 bits)
    static const uint8_t SMCR_MSM = 7;              // Master/Slave mode
    static const uint8_t SMCR_TS = 4;               // Trigger selection (3 bits)
    static const uint8_t SMCR_SMS = 0;              // Slave mode selection (3 bits)
    static const uint32_t SMCR_RESET_VALUE = 0x0;

    static const uint8_t DIER_TDE = 14;             // Trigger DMA request enable
    static const uint8_t DIER_COMDE = 13;           // COM DMA request enable
    static const uint8_t DIER_CC4DE = 12;           // Capture/Compare 4 DMA request enable
    static const uint8_t DIER_CC3DE = 11;           // Capture/Compare 3 DMA request enable
    static const uint8_t DIER_CC2DE = 10;           // Capture/Compare 2 DMA request enable
    static const uint8_t DIER_CC1DE = 9;            // Capture/Compare 1 DMA request enable
    static const uint8_t DIER_UDE = 8;              // Update DMA request enable
    static const uint8_t DIER_TIE = 6;              // Trigger interrupt enable
    static const uint8_t DIER_CC4IE = 4;            // Capture/Compare 4 interrupt enable
    static const uint8_t DIER_CC3IE = 3;            // Capture/Compare 3 interrupt enable
    static const uint8_t DIER_CC2IE = 2;            // Capture/Compare 2 interrupt enable
    static const uint8_t DIER_CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t DIER_UIE = 0;              // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static const uint8_t SR_CC4OF = 12;           // Capture/Compare 4 overcapture flag
    static const uint8_t SR_CC3OF = 11;           // Capture/Compare 3 overcapture flag
    static const uint8_t SR_CC2OF = 10;           // Capture/compare 2 overcapture flag
    static const uint8_t SR_CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t SR_TIF = 6;              // Trigger interrupt flag
    static const uint8_t SR_CC4IF = 4;            // Capture/Compare 4 interrupt flag
    static const uint8_t SR_CC3IF = 3;            // Capture/Compare 3 interrupt flag
    static const uint8_t SR_CC2IF = 2;            // Capture/Compare 2 interrupt flag
    static const uint8_t SR_CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t SR_UIF = 0;              // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static const uint8_t EGR_TG = 6;               // Trigger generation
    static const uint8_t EGR_CC4G = 4;             // Capture/compare 4 generation
    static const uint8_t EGR_CC3G = 3;             // Capture/compare 3 generation
    static const uint8_t EGR_CC2G = 2;             // Capture/compare 2 generation
    static const uint8_t EGR_CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t EGR_UG = 0;               // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    static const uint8_t CCMR1_CC1S = 0;             // Capture/Compare 1 selection (2 bits)
    static const uint8_t CCMR1_CC2S = 8;             // Capture/Compare 2 selection (2 bits)
    static const uint8_t CCMR1_IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t CCMR1_IC1PSC = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CCMR1_IC2F = 12;            // Input capture 2 filter (4 bits)
    static const uint8_t CCMR1_IC2PSC = 10;          // Input capture 2 prescaler (2 bits)
    static const uint8_t CCMR1_OC1CE = 7;            // Output compare 1 clear enable
    static const uint8_t CCMR1_OC1FE = 2;            // Output compare 1 fast enable
    static const uint8_t CCMR1_OC1M = 4;             // Output compare 1 mode (3 bits)
    static const uint8_t CCMR1_OC1PE = 3;            // Output compare 1 preload enable
    static const uint8_t CCMR1_OC2CE = 15;           // Output compare 2 clear enable
    static const uint8_t CCMR1_OC2FE = 10;           // Output compare 2 fast enable
    static const uint8_t CCMR1_OC2M = 12;            // Output compare 2 mode (3 bits)
    static const uint8_t CCMR1_OC2PE = 11;           // Output compare 2 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static const uint8_t CCMR2_CC3S = 0;             // Capture/Compare 3 selection (2 bits)
    static const uint8_t CCMR2_CC4S = 8;             // Capture/Compare 4 selection (2 bits)
    static const uint8_t CCMR2_IC3F = 4;             // Input capture 3 filter (4 bits)
    static const uint8_t CCMR2_IC3PSC = 2;           // Input capture 3 prescaler (2 bits)
    static const uint8_t CCMR2_IC4F = 12;            // Input capture 4 filter (4 bits)
    static const uint8_t CCMR2_IC4PSC = 10;          // Input capture 4 prescaler (2 bits)
    static const uint8_t CCMR2_OC3CE = 7;            // Output compare 3 clear enable
    static const uint8_t CCMR2_OC3FE = 2;            // Output compare 3 fast enable
    static const uint8_t CCMR2_OC3M = 4;             // Output compare 3 mode (3 bits)
    static const uint8_t CCMR2_OC3PE = 3;            // Output compare 3 preload enable
    static const uint8_t CCMR2_OC4CE = 15;           // Output compare 4 clear enable
    static const uint8_t CCMR2_OC4FE = 10;           // Output compare 4 fast enable
    static const uint8_t CCMR2_OC4M = 12;            // Output compare 4 mode (3 bits)
    static const uint8_t CCMR2_OC4PE = 11;           // Output compare 4 preload enable
    static const uint32_t CCMR2_RESET_VALUE = 0x0;

    static const uint8_t CCER_CC4NP = 15;           // Capture/Compare 4 output Polarity
    static const uint8_t CCER_CC4P = 13;            // Capture/Compare 3 output Polarity
    static const uint8_t CCER_CC4E = 12;            // Capture/Compare 4 output enable
    static const uint8_t CCER_CC3NP = 11;           // Capture/Compare 3 output Polarity
    static const uint8_t CCER_CC3P = 9;             // Capture/Compare 3 output Polarity
    static const uint8_t CCER_CC3E = 8;             // Capture/Compare 3 output enable
    static const uint8_t CCER_CC2NP = 7;            // Capture/Compare 2 output Polarity
    static const uint8_t CCER_CC2P = 5;             // Capture/Compare 2 output Polarity
    static const uint8_t CCER_CC2E = 4;             // Capture/Compare 2 output enable
    static const uint8_t CCER_CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1E = 0;             // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    static const uint8_t CNT_CNT_H = 16;           // High counter value (TIM2 only) (16 bits)
    static const uint8_t CNT_CNT_L = 0;            // Low counter value (16 bits)
    static const uint32_t CNT_RESET_VALUE = 0x0;

    static const uint8_t PSC_PSC = 0;              // Prescaler value (16 bits)
    static const uint32_t PSC_RESET_VALUE = 0x0;

    static const uint8_t ARR_ARR_H = 16;           // High Auto-reload value (TIM2 only) (16 bits)
    static const uint8_t ARR_ARR_L = 0;            // Low Auto-reload value (16 bits)
    static const uint32_t ARR_RESET_VALUE = 0x0;

    static const uint8_t CCR1_CCR1_H = 16;          // High Capture/Compare 1 value (TIM2 only) (16 bits)
    static const uint8_t CCR1_CCR1_L = 0;           // Low Capture/Compare 1 value (16 bits)
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    static const uint8_t CCR2_CCR2_H = 16;          // High Capture/Compare 2 value (TIM2 only) (16 bits)
    static const uint8_t CCR2_CCR2_L = 0;           // Low Capture/Compare 2 value (16 bits)
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    static const uint8_t CCR3_CCR3_H = 16;          // High Capture/Compare value (TIM2 only) (16 bits)
    static const uint8_t CCR3_CCR3_L = 0;           // Low Capture/Compare value (16 bits)
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    static const uint8_t CCR4_CCR4_H = 16;          // High Capture/Compare value (TIM2 only) (16 bits)
    static const uint8_t CCR4_CCR4_L = 0;           // Low Capture/Compare value (16 bits)
    static const uint32_t CCR4_RESET_VALUE = 0x0;

    static const uint8_t DCR_DBL = 8;              // DMA burst length (5 bits)
    static const uint8_t DCR_DBA = 0;              // DMA base address (5 bits)
    static const uint32_t DCR_RESET_VALUE = 0x0;

    static const uint8_t DMAR_DMAR = 0;             // DMA register for burst accesses (16 bits)
    static const uint32_t DMAR_RESET_VALUE = 0x0;
};

static tim2_t& TIM2 = *reinterpret_cast<tim2_t*>(0x40000000);


////
//
//    General-purpose-timers
//
////

struct tim3_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMCR;                 // [Read-write] slave mode control register
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1;                // [Read-write] capture/compare mode register 1 (output mode)
    volatile uint32_t    CCMR2;                // [Read-write] capture/compare mode register 2 (output mode)
    volatile uint32_t    CCER;                 // [Read-write] capture/compare enable register
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    reserved_t<1>        _0;
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    volatile uint32_t    CCR2;                 // [Read-write] capture/compare register 2
    volatile uint32_t    CCR3;                 // [Read-write] capture/compare register 3
    volatile uint32_t    CCR4;                 // [Read-write] capture/compare register 4
    reserved_t<1>        _1;
    volatile uint32_t    DCR;                  // [Read-write] DMA control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static const uint8_t CR1_CKD = 8;              // Clock division (2 bits)
    static const uint8_t CR1_ARPE = 7;             // Auto-reload preload enable
    static const uint8_t CR1_CMS = 5;              // Center-aligned mode selection (2 bits)
    static const uint8_t CR1_DIR = 4;              // Direction
    static const uint8_t CR1_OPM = 3;              // One-pulse mode
    static const uint8_t CR1_URS = 2;              // Update request source
    static const uint8_t CR1_UDIS = 1;             // Update disable
    static const uint8_t CR1_CEN = 0;              // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_TI1S = 7;             // TI1 selection
    static const uint8_t CR2_MMS = 4;              // Master mode selection (3 bits)
    static const uint8_t CR2_CCDS = 3;             // Capture/compare DMA selection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t SMCR_ETP = 15;             // External trigger polarity
    static const uint8_t SMCR_ECE = 14;             // External clock enable
    static const uint8_t SMCR_ETPS = 12;            // External trigger prescaler (2 bits)
    static const uint8_t SMCR_ETF = 8;              // External trigger filter (4 bits)
    static const uint8_t SMCR_MSM = 7;              // Master/Slave mode
    static const uint8_t SMCR_TS = 4;               // Trigger selection (3 bits)
    static const uint8_t SMCR_SMS = 0;              // Slave mode selection (3 bits)
    static const uint32_t SMCR_RESET_VALUE = 0x0;

    static const uint8_t DIER_TDE = 14;             // Trigger DMA request enable
    static const uint8_t DIER_COMDE = 13;           // COM DMA request enable
    static const uint8_t DIER_CC4DE = 12;           // Capture/Compare 4 DMA request enable
    static const uint8_t DIER_CC3DE = 11;           // Capture/Compare 3 DMA request enable
    static const uint8_t DIER_CC2DE = 10;           // Capture/Compare 2 DMA request enable
    static const uint8_t DIER_CC1DE = 9;            // Capture/Compare 1 DMA request enable
    static const uint8_t DIER_UDE = 8;              // Update DMA request enable
    static const uint8_t DIER_TIE = 6;              // Trigger interrupt enable
    static const uint8_t DIER_CC4IE = 4;            // Capture/Compare 4 interrupt enable
    static const uint8_t DIER_CC3IE = 3;            // Capture/Compare 3 interrupt enable
    static const uint8_t DIER_CC2IE = 2;            // Capture/Compare 2 interrupt enable
    static const uint8_t DIER_CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t DIER_UIE = 0;              // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static const uint8_t SR_CC4OF = 12;           // Capture/Compare 4 overcapture flag
    static const uint8_t SR_CC3OF = 11;           // Capture/Compare 3 overcapture flag
    static const uint8_t SR_CC2OF = 10;           // Capture/compare 2 overcapture flag
    static const uint8_t SR_CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t SR_TIF = 6;              // Trigger interrupt flag
    static const uint8_t SR_CC4IF = 4;            // Capture/Compare 4 interrupt flag
    static const uint8_t SR_CC3IF = 3;            // Capture/Compare 3 interrupt flag
    static const uint8_t SR_CC2IF = 2;            // Capture/Compare 2 interrupt flag
    static const uint8_t SR_CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t SR_UIF = 0;              // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static const uint8_t EGR_TG = 6;               // Trigger generation
    static const uint8_t EGR_CC4G = 4;             // Capture/compare 4 generation
    static const uint8_t EGR_CC3G = 3;             // Capture/compare 3 generation
    static const uint8_t EGR_CC2G = 2;             // Capture/compare 2 generation
    static const uint8_t EGR_CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t EGR_UG = 0;               // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    static const uint8_t CCMR1_CC1S = 0;             // Capture/Compare 1 selection (2 bits)
    static const uint8_t CCMR1_CC2S = 8;             // Capture/Compare 2 selection (2 bits)
    static const uint8_t CCMR1_IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t CCMR1_IC1PSC = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CCMR1_IC2F = 12;            // Input capture 2 filter (4 bits)
    static const uint8_t CCMR1_IC2PSC = 10;          // Input capture 2 prescaler (2 bits)
    static const uint8_t CCMR1_OC1CE = 7;            // Output compare 1 clear enable
    static const uint8_t CCMR1_OC1FE = 2;            // Output compare 1 fast enable
    static const uint8_t CCMR1_OC1M = 4;             // Output compare 1 mode (3 bits)
    static const uint8_t CCMR1_OC1PE = 3;            // Output compare 1 preload enable
    static const uint8_t CCMR1_OC2CE = 15;           // Output compare 2 clear enable
    static const uint8_t CCMR1_OC2FE = 10;           // Output compare 2 fast enable
    static const uint8_t CCMR1_OC2M = 12;            // Output compare 2 mode (3 bits)
    static const uint8_t CCMR1_OC2PE = 11;           // Output compare 2 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static const uint8_t CCMR2_CC3S = 0;             // Capture/Compare 3 selection (2 bits)
    static const uint8_t CCMR2_CC4S = 8;             // Capture/Compare 4 selection (2 bits)
    static const uint8_t CCMR2_IC3F = 4;             // Input capture 3 filter (4 bits)
    static const uint8_t CCMR2_IC3PSC = 2;           // Input capture 3 prescaler (2 bits)
    static const uint8_t CCMR2_IC4F = 12;            // Input capture 4 filter (4 bits)
    static const uint8_t CCMR2_IC4PSC = 10;          // Input capture 4 prescaler (2 bits)
    static const uint8_t CCMR2_OC3CE = 7;            // Output compare 3 clear enable
    static const uint8_t CCMR2_OC3FE = 2;            // Output compare 3 fast enable
    static const uint8_t CCMR2_OC3M = 4;             // Output compare 3 mode (3 bits)
    static const uint8_t CCMR2_OC3PE = 3;            // Output compare 3 preload enable
    static const uint8_t CCMR2_OC4CE = 15;           // Output compare 4 clear enable
    static const uint8_t CCMR2_OC4FE = 10;           // Output compare 4 fast enable
    static const uint8_t CCMR2_OC4M = 12;            // Output compare 4 mode (3 bits)
    static const uint8_t CCMR2_OC4PE = 11;           // Output compare 4 preload enable
    static const uint32_t CCMR2_RESET_VALUE = 0x0;

    static const uint8_t CCER_CC4NP = 15;           // Capture/Compare 4 output Polarity
    static const uint8_t CCER_CC4P = 13;            // Capture/Compare 3 output Polarity
    static const uint8_t CCER_CC4E = 12;            // Capture/Compare 4 output enable
    static const uint8_t CCER_CC3NP = 11;           // Capture/Compare 3 output Polarity
    static const uint8_t CCER_CC3P = 9;             // Capture/Compare 3 output Polarity
    static const uint8_t CCER_CC3E = 8;             // Capture/Compare 3 output enable
    static const uint8_t CCER_CC2NP = 7;            // Capture/Compare 2 output Polarity
    static const uint8_t CCER_CC2P = 5;             // Capture/Compare 2 output Polarity
    static const uint8_t CCER_CC2E = 4;             // Capture/Compare 2 output enable
    static const uint8_t CCER_CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1E = 0;             // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    static const uint8_t CNT_CNT_H = 16;           // High counter value (TIM2 only) (16 bits)
    static const uint8_t CNT_CNT_L = 0;            // Low counter value (16 bits)
    static const uint32_t CNT_RESET_VALUE = 0x0;

    static const uint8_t PSC_PSC = 0;              // Prescaler value (16 bits)
    static const uint32_t PSC_RESET_VALUE = 0x0;

    static const uint8_t ARR_ARR_H = 16;           // High Auto-reload value (TIM2 only) (16 bits)
    static const uint8_t ARR_ARR_L = 0;            // Low Auto-reload value (16 bits)
    static const uint32_t ARR_RESET_VALUE = 0x0;

    static const uint8_t CCR1_CCR1_H = 16;          // High Capture/Compare 1 value (TIM2 only) (16 bits)
    static const uint8_t CCR1_CCR1_L = 0;           // Low Capture/Compare 1 value (16 bits)
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    static const uint8_t CCR2_CCR2_H = 16;          // High Capture/Compare 2 value (TIM2 only) (16 bits)
    static const uint8_t CCR2_CCR2_L = 0;           // Low Capture/Compare 2 value (16 bits)
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    static const uint8_t CCR3_CCR3_H = 16;          // High Capture/Compare value (TIM2 only) (16 bits)
    static const uint8_t CCR3_CCR3_L = 0;           // Low Capture/Compare value (16 bits)
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    static const uint8_t CCR4_CCR4_H = 16;          // High Capture/Compare value (TIM2 only) (16 bits)
    static const uint8_t CCR4_CCR4_L = 0;           // Low Capture/Compare value (16 bits)
    static const uint32_t CCR4_RESET_VALUE = 0x0;

    static const uint8_t DCR_DBL = 8;              // DMA burst length (5 bits)
    static const uint8_t DCR_DBA = 0;              // DMA base address (5 bits)
    static const uint32_t DCR_RESET_VALUE = 0x0;

    static const uint8_t DMAR_DMAR = 0;             // DMA register for burst accesses (16 bits)
    static const uint32_t DMAR_RESET_VALUE = 0x0;
};

static tim3_t& TIM3 = *reinterpret_cast<tim3_t*>(0x40000400);


////
//
//    General-purpose-timers
//
////

struct tim14_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    reserved_t<2>        _0;
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1;                // [Read-write] capture/compare mode register (output mode)
    reserved_t<1>        _1;
    volatile uint32_t    CCER;                 // [Read-write] capture/compare enable register
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    reserved_t<1>        _2;
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    reserved_t<6>        _3;
    volatile uint32_t    OR;                   // [Read-write] option register

    static const uint8_t CR1_CKD = 8;              // Clock division (2 bits)
    static const uint8_t CR1_ARPE = 7;             // Auto-reload preload enable
    static const uint8_t CR1_URS = 2;              // Update request source
    static const uint8_t CR1_UDIS = 1;             // Update disable
    static const uint8_t CR1_CEN = 0;              // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t DIER_CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t DIER_UIE = 0;              // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static const uint8_t SR_CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t SR_CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t SR_UIF = 0;              // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static const uint8_t EGR_CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t EGR_UG = 0;               // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    static const uint8_t CCMR1_CC1S = 0;             // Capture/Compare 1 selection (2 bits)
    static const uint8_t CCMR1_IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t CCMR1_IC1PSC = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CCMR1_OC1FE = 2;            // Output compare 1 fast enable
    static const uint8_t CCMR1_OC1M = 4;             // Output Compare 1 mode (3 bits)
    static const uint8_t CCMR1_OC1PE = 3;            // Output Compare 1 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static const uint8_t CCER_CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1E = 0;             // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    static const uint8_t CNT_CNT = 0;              // counter value (16 bits)
    static const uint32_t CNT_RESET_VALUE = 0x0;

    static const uint8_t PSC_PSC = 0;              // Prescaler value (16 bits)
    static const uint32_t PSC_RESET_VALUE = 0x0;

    static const uint8_t ARR_ARR = 0;              // Auto-reload value (16 bits)
    static const uint32_t ARR_RESET_VALUE = 0x0;

    static const uint8_t CCR1_CCR1 = 0;             // Capture/Compare 1 value (16 bits)
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    static const uint8_t OR_RMP = 0;              // Timer input 1 remap (2 bits)
    static const uint32_t OR_RESET_VALUE = 0x0;
};

static tim14_t& TIM14 = *reinterpret_cast<tim14_t*>(0x40002000);


////
//
//    Basic-timers
//
////

struct tim6_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    reserved_t<1>        _0;
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    reserved_t<3>        _1;
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register

    static const uint8_t CR1_ARPE = 7;             // Auto-reload preload enable
    static const uint8_t CR1_OPM = 3;              // One-pulse mode
    static const uint8_t CR1_URS = 2;              // Update request source
    static const uint8_t CR1_UDIS = 1;             // Update disable
    static const uint8_t CR1_CEN = 0;              // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_MMS = 4;              // Master mode selection (3 bits)
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t DIER_UDE = 8;              // Update DMA request enable
    static const uint8_t DIER_UIE = 0;              // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static const uint8_t SR_UIF = 0;              // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static const uint8_t EGR_UG = 0;               // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    static const uint8_t CNT_CNT = 0;              // Low counter value (16 bits)
    static const uint32_t CNT_RESET_VALUE = 0x0;

    static const uint8_t PSC_PSC = 0;              // Prescaler value (16 bits)
    static const uint32_t PSC_RESET_VALUE = 0x0;

    static const uint8_t ARR_ARR = 0;              // Low Auto-reload value (16 bits)
    static const uint32_t ARR_RESET_VALUE = 0x0;
};

static tim6_t& TIM6 = *reinterpret_cast<tim6_t*>(0x40001000);


////
//
//    Basic-timers
//
////

struct tim7_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    reserved_t<1>        _0;
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    reserved_t<3>        _1;
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register

    static const uint8_t CR1_ARPE = 7;             // Auto-reload preload enable
    static const uint8_t CR1_OPM = 3;              // One-pulse mode
    static const uint8_t CR1_URS = 2;              // Update request source
    static const uint8_t CR1_UDIS = 1;             // Update disable
    static const uint8_t CR1_CEN = 0;              // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_MMS = 4;              // Master mode selection (3 bits)
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t DIER_UDE = 8;              // Update DMA request enable
    static const uint8_t DIER_UIE = 0;              // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static const uint8_t SR_UIF = 0;              // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static const uint8_t EGR_UG = 0;               // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    static const uint8_t CNT_CNT = 0;              // Low counter value (16 bits)
    static const uint32_t CNT_RESET_VALUE = 0x0;

    static const uint8_t PSC_PSC = 0;              // Prescaler value (16 bits)
    static const uint32_t PSC_RESET_VALUE = 0x0;

    static const uint8_t ARR_ARR = 0;              // Low Auto-reload value (16 bits)
    static const uint32_t ARR_RESET_VALUE = 0x0;
};

static tim7_t& TIM7 = *reinterpret_cast<tim7_t*>(0x40001400);


////
//
//    External interrupt/event controller
//
////

struct exti_t
{
    volatile uint32_t    IMR;                  // [Read-write] Interrupt mask register (EXTI_IMR)
    volatile uint32_t    EMR;                  // [Read-write] Event mask register (EXTI_EMR)
    volatile uint32_t    RTSR;                 // [Read-write] Rising Trigger selection register (EXTI_RTSR)
    volatile uint32_t    FTSR;                 // [Read-write] Falling Trigger selection register (EXTI_FTSR)
    volatile uint32_t    SWIER;                // [Read-write] Software interrupt event register (EXTI_SWIER)
    volatile uint32_t    PR;                   // [Read-write] Pending register (EXTI_PR)

    static const uint8_t IMR_MR0 = 0;              // Interrupt Mask on line 0
    static const uint8_t IMR_MR1 = 1;              // Interrupt Mask on line 1
    static const uint8_t IMR_MR2 = 2;              // Interrupt Mask on line 2
    static const uint8_t IMR_MR3 = 3;              // Interrupt Mask on line 3
    static const uint8_t IMR_MR4 = 4;              // Interrupt Mask on line 4
    static const uint8_t IMR_MR5 = 5;              // Interrupt Mask on line 5
    static const uint8_t IMR_MR6 = 6;              // Interrupt Mask on line 6
    static const uint8_t IMR_MR7 = 7;              // Interrupt Mask on line 7
    static const uint8_t IMR_MR8 = 8;              // Interrupt Mask on line 8
    static const uint8_t IMR_MR9 = 9;              // Interrupt Mask on line 9
    static const uint8_t IMR_MR10 = 10;            // Interrupt Mask on line 10
    static const uint8_t IMR_MR11 = 11;            // Interrupt Mask on line 11
    static const uint8_t IMR_MR12 = 12;            // Interrupt Mask on line 12
    static const uint8_t IMR_MR13 = 13;            // Interrupt Mask on line 13
    static const uint8_t IMR_MR14 = 14;            // Interrupt Mask on line 14
    static const uint8_t IMR_MR15 = 15;            // Interrupt Mask on line 15
    static const uint8_t IMR_MR16 = 16;            // Interrupt Mask on line 16
    static const uint8_t IMR_MR17 = 17;            // Interrupt Mask on line 17
    static const uint8_t IMR_MR18 = 18;            // Interrupt Mask on line 18
    static const uint8_t IMR_MR19 = 19;            // Interrupt Mask on line 19
    static const uint8_t IMR_MR20 = 20;            // Interrupt Mask on line 20
    static const uint8_t IMR_MR21 = 21;            // Interrupt Mask on line 21
    static const uint8_t IMR_MR22 = 22;            // Interrupt Mask on line 22
    static const uint8_t IMR_MR23 = 23;            // Interrupt Mask on line 23
    static const uint8_t IMR_MR24 = 24;            // Interrupt Mask on line 24
    static const uint8_t IMR_MR25 = 25;            // Interrupt Mask on line 25
    static const uint8_t IMR_MR26 = 26;            // Interrupt Mask on line 26
    static const uint8_t IMR_MR27 = 27;            // Interrupt Mask on line 27
    static const uint32_t IMR_RESET_VALUE = 0xf940000;

    static const uint8_t EMR_MR0 = 0;              // Event Mask on line 0
    static const uint8_t EMR_MR1 = 1;              // Event Mask on line 1
    static const uint8_t EMR_MR2 = 2;              // Event Mask on line 2
    static const uint8_t EMR_MR3 = 3;              // Event Mask on line 3
    static const uint8_t EMR_MR4 = 4;              // Event Mask on line 4
    static const uint8_t EMR_MR5 = 5;              // Event Mask on line 5
    static const uint8_t EMR_MR6 = 6;              // Event Mask on line 6
    static const uint8_t EMR_MR7 = 7;              // Event Mask on line 7
    static const uint8_t EMR_MR8 = 8;              // Event Mask on line 8
    static const uint8_t EMR_MR9 = 9;              // Event Mask on line 9
    static const uint8_t EMR_MR10 = 10;            // Event Mask on line 10
    static const uint8_t EMR_MR11 = 11;            // Event Mask on line 11
    static const uint8_t EMR_MR12 = 12;            // Event Mask on line 12
    static const uint8_t EMR_MR13 = 13;            // Event Mask on line 13
    static const uint8_t EMR_MR14 = 14;            // Event Mask on line 14
    static const uint8_t EMR_MR15 = 15;            // Event Mask on line 15
    static const uint8_t EMR_MR16 = 16;            // Event Mask on line 16
    static const uint8_t EMR_MR17 = 17;            // Event Mask on line 17
    static const uint8_t EMR_MR18 = 18;            // Event Mask on line 18
    static const uint8_t EMR_MR19 = 19;            // Event Mask on line 19
    static const uint8_t EMR_MR20 = 20;            // Event Mask on line 20
    static const uint8_t EMR_MR21 = 21;            // Event Mask on line 21
    static const uint8_t EMR_MR22 = 22;            // Event Mask on line 22
    static const uint8_t EMR_MR23 = 23;            // Event Mask on line 23
    static const uint8_t EMR_MR24 = 24;            // Event Mask on line 24
    static const uint8_t EMR_MR25 = 25;            // Event Mask on line 25
    static const uint8_t EMR_MR26 = 26;            // Event Mask on line 26
    static const uint8_t EMR_MR27 = 27;            // Event Mask on line 27
    static const uint32_t EMR_RESET_VALUE = 0x0;

    static const uint8_t RTSR_TR0 = 0;              // Rising trigger event configuration of line 0
    static const uint8_t RTSR_TR1 = 1;              // Rising trigger event configuration of line 1
    static const uint8_t RTSR_TR2 = 2;              // Rising trigger event configuration of line 2
    static const uint8_t RTSR_TR3 = 3;              // Rising trigger event configuration of line 3
    static const uint8_t RTSR_TR4 = 4;              // Rising trigger event configuration of line 4
    static const uint8_t RTSR_TR5 = 5;              // Rising trigger event configuration of line 5
    static const uint8_t RTSR_TR6 = 6;              // Rising trigger event configuration of line 6
    static const uint8_t RTSR_TR7 = 7;              // Rising trigger event configuration of line 7
    static const uint8_t RTSR_TR8 = 8;              // Rising trigger event configuration of line 8
    static const uint8_t RTSR_TR9 = 9;              // Rising trigger event configuration of line 9
    static const uint8_t RTSR_TR10 = 10;            // Rising trigger event configuration of line 10
    static const uint8_t RTSR_TR11 = 11;            // Rising trigger event configuration of line 11
    static const uint8_t RTSR_TR12 = 12;            // Rising trigger event configuration of line 12
    static const uint8_t RTSR_TR13 = 13;            // Rising trigger event configuration of line 13
    static const uint8_t RTSR_TR14 = 14;            // Rising trigger event configuration of line 14
    static const uint8_t RTSR_TR15 = 15;            // Rising trigger event configuration of line 15
    static const uint8_t RTSR_TR16 = 16;            // Rising trigger event configuration of line 16
    static const uint8_t RTSR_TR17 = 17;            // Rising trigger event configuration of line 17
    static const uint8_t RTSR_TR19 = 19;            // Rising trigger event configuration of line 19
    static const uint32_t RTSR_RESET_VALUE = 0x0;

    static const uint8_t FTSR_TR0 = 0;              // Falling trigger event configuration of line 0
    static const uint8_t FTSR_TR1 = 1;              // Falling trigger event configuration of line 1
    static const uint8_t FTSR_TR2 = 2;              // Falling trigger event configuration of line 2
    static const uint8_t FTSR_TR3 = 3;              // Falling trigger event configuration of line 3
    static const uint8_t FTSR_TR4 = 4;              // Falling trigger event configuration of line 4
    static const uint8_t FTSR_TR5 = 5;              // Falling trigger event configuration of line 5
    static const uint8_t FTSR_TR6 = 6;              // Falling trigger event configuration of line 6
    static const uint8_t FTSR_TR7 = 7;              // Falling trigger event configuration of line 7
    static const uint8_t FTSR_TR8 = 8;              // Falling trigger event configuration of line 8
    static const uint8_t FTSR_TR9 = 9;              // Falling trigger event configuration of line 9
    static const uint8_t FTSR_TR10 = 10;            // Falling trigger event configuration of line 10
    static const uint8_t FTSR_TR11 = 11;            // Falling trigger event configuration of line 11
    static const uint8_t FTSR_TR12 = 12;            // Falling trigger event configuration of line 12
    static const uint8_t FTSR_TR13 = 13;            // Falling trigger event configuration of line 13
    static const uint8_t FTSR_TR14 = 14;            // Falling trigger event configuration of line 14
    static const uint8_t FTSR_TR15 = 15;            // Falling trigger event configuration of line 15
    static const uint8_t FTSR_TR16 = 16;            // Falling trigger event configuration of line 16
    static const uint8_t FTSR_TR17 = 17;            // Falling trigger event configuration of line 17
    static const uint8_t FTSR_TR19 = 19;            // Falling trigger event configuration of line 19
    static const uint32_t FTSR_RESET_VALUE = 0x0;

    static const uint8_t SWIER_SWIER0 = 0;           // Software Interrupt on line 0
    static const uint8_t SWIER_SWIER1 = 1;           // Software Interrupt on line 1
    static const uint8_t SWIER_SWIER2 = 2;           // Software Interrupt on line 2
    static const uint8_t SWIER_SWIER3 = 3;           // Software Interrupt on line 3
    static const uint8_t SWIER_SWIER4 = 4;           // Software Interrupt on line 4
    static const uint8_t SWIER_SWIER5 = 5;           // Software Interrupt on line 5
    static const uint8_t SWIER_SWIER6 = 6;           // Software Interrupt on line 6
    static const uint8_t SWIER_SWIER7 = 7;           // Software Interrupt on line 7
    static const uint8_t SWIER_SWIER8 = 8;           // Software Interrupt on line 8
    static const uint8_t SWIER_SWIER9 = 9;           // Software Interrupt on line 9
    static const uint8_t SWIER_SWIER10 = 10;         // Software Interrupt on line 10
    static const uint8_t SWIER_SWIER11 = 11;         // Software Interrupt on line 11
    static const uint8_t SWIER_SWIER12 = 12;         // Software Interrupt on line 12
    static const uint8_t SWIER_SWIER13 = 13;         // Software Interrupt on line 13
    static const uint8_t SWIER_SWIER14 = 14;         // Software Interrupt on line 14
    static const uint8_t SWIER_SWIER15 = 15;         // Software Interrupt on line 15
    static const uint8_t SWIER_SWIER16 = 16;         // Software Interrupt on line 16
    static const uint8_t SWIER_SWIER17 = 17;         // Software Interrupt on line 17
    static const uint8_t SWIER_SWIER19 = 19;         // Software Interrupt on line 19
    static const uint32_t SWIER_RESET_VALUE = 0x0;

    static const uint8_t PR_PR0 = 0;              // Pending bit 0
    static const uint8_t PR_PR1 = 1;              // Pending bit 1
    static const uint8_t PR_PR2 = 2;              // Pending bit 2
    static const uint8_t PR_PR3 = 3;              // Pending bit 3
    static const uint8_t PR_PR4 = 4;              // Pending bit 4
    static const uint8_t PR_PR5 = 5;              // Pending bit 5
    static const uint8_t PR_PR6 = 6;              // Pending bit 6
    static const uint8_t PR_PR7 = 7;              // Pending bit 7
    static const uint8_t PR_PR8 = 8;              // Pending bit 8
    static const uint8_t PR_PR9 = 9;              // Pending bit 9
    static const uint8_t PR_PR10 = 10;            // Pending bit 10
    static const uint8_t PR_PR11 = 11;            // Pending bit 11
    static const uint8_t PR_PR12 = 12;            // Pending bit 12
    static const uint8_t PR_PR13 = 13;            // Pending bit 13
    static const uint8_t PR_PR14 = 14;            // Pending bit 14
    static const uint8_t PR_PR15 = 15;            // Pending bit 15
    static const uint8_t PR_PR16 = 16;            // Pending bit 16
    static const uint8_t PR_PR17 = 17;            // Pending bit 17
    static const uint8_t PR_PR19 = 19;            // Pending bit 19
    static const uint32_t PR_RESET_VALUE = 0x0;
};

static exti_t& EXTI = *reinterpret_cast<exti_t*>(0x40010400);


////
//
//    Nested Vectored Interrupt Controller
//
////

struct nvic_t
{
    volatile uint32_t    ISER;                 // [Read-write] Interrupt Set Enable Register
    reserved_t<31>       _0;
    volatile uint32_t    ICER;                 // [Read-write] Interrupt Clear Enable Register
    reserved_t<31>       _1;
    volatile uint32_t    ISPR;                 // [Read-write] Interrupt Set-Pending Register
    reserved_t<31>       _2;
    volatile uint32_t    ICPR;                 // [Read-write] Interrupt Clear-Pending Register
    reserved_t<95>       _3;
    volatile uint32_t    IPR0;                 // [Read-write] Interrupt Priority Register 0
    volatile uint32_t    IPR1;                 // [Read-write] Interrupt Priority Register 1
    volatile uint32_t    IPR2;                 // [Read-write] Interrupt Priority Register 2
    volatile uint32_t    IPR3;                 // [Read-write] Interrupt Priority Register 3
    volatile uint32_t    IPR4;                 // [Read-write] Interrupt Priority Register 4
    volatile uint32_t    IPR5;                 // [Read-write] Interrupt Priority Register 5
    volatile uint32_t    IPR6;                 // [Read-write] Interrupt Priority Register 6
    volatile uint32_t    IPR7;                 // [Read-write] Interrupt Priority Register 7

    static const uint8_t ISER_SETENA = 0;           // SETENA (32 bits)
    static const uint32_t ISER_RESET_VALUE = 0x0;

    static const uint8_t ICER_CLRENA = 0;           // CLRENA (32 bits)
    static const uint32_t ICER_RESET_VALUE = 0x0;

    static const uint8_t ISPR_SETPEND = 0;          // SETPEND (32 bits)
    static const uint32_t ISPR_RESET_VALUE = 0x0;

    static const uint8_t ICPR_CLRPEND = 0;          // CLRPEND (32 bits)
    static const uint32_t ICPR_RESET_VALUE = 0x0;

    static const uint8_t IPR0_PRI_00 = 6;           // PRI_00 (2 bits)
    static const uint8_t IPR0_PRI_01 = 14;          // PRI_01 (2 bits)
    static const uint8_t IPR0_PRI_02 = 22;          // PRI_02 (2 bits)
    static const uint8_t IPR0_PRI_03 = 30;          // PRI_03 (2 bits)
    static const uint32_t IPR0_RESET_VALUE = 0x0;

    static const uint8_t IPR1_PRI_40 = 6;           // PRI_40 (2 bits)
    static const uint8_t IPR1_PRI_41 = 14;          // PRI_41 (2 bits)
    static const uint8_t IPR1_PRI_42 = 22;          // PRI_42 (2 bits)
    static const uint8_t IPR1_PRI_43 = 30;          // PRI_43 (2 bits)
    static const uint32_t IPR1_RESET_VALUE = 0x0;

    static const uint8_t IPR2_PRI_80 = 6;           // PRI_80 (2 bits)
    static const uint8_t IPR2_PRI_81 = 14;          // PRI_81 (2 bits)
    static const uint8_t IPR2_PRI_82 = 22;          // PRI_82 (2 bits)
    static const uint8_t IPR2_PRI_83 = 30;          // PRI_83 (2 bits)
    static const uint32_t IPR2_RESET_VALUE = 0x0;

    static const uint8_t IPR3_PRI_120 = 6;          // PRI_120 (2 bits)
    static const uint8_t IPR3_PRI_121 = 14;         // PRI_121 (2 bits)
    static const uint8_t IPR3_PRI_122 = 22;         // PRI_122 (2 bits)
    static const uint8_t IPR3_PRI_123 = 30;         // PRI_123 (2 bits)
    static const uint32_t IPR3_RESET_VALUE = 0x0;

    static const uint8_t IPR4_PRI_160 = 6;          // PRI_160 (2 bits)
    static const uint8_t IPR4_PRI_161 = 14;         // PRI_161 (2 bits)
    static const uint8_t IPR4_PRI_162 = 22;         // PRI_162 (2 bits)
    static const uint8_t IPR4_PRI_163 = 30;         // PRI_163 (2 bits)
    static const uint32_t IPR4_RESET_VALUE = 0x0;

    static const uint8_t IPR5_PRI_200 = 6;          // PRI_200 (2 bits)
    static const uint8_t IPR5_PRI_201 = 14;         // PRI_201 (2 bits)
    static const uint8_t IPR5_PRI_202 = 22;         // PRI_202 (2 bits)
    static const uint8_t IPR5_PRI_203 = 30;         // PRI_203 (2 bits)
    static const uint32_t IPR5_RESET_VALUE = 0x0;

    static const uint8_t IPR6_PRI_240 = 6;          // PRI_240 (2 bits)
    static const uint8_t IPR6_PRI_241 = 14;         // PRI_241 (2 bits)
    static const uint8_t IPR6_PRI_242 = 22;         // PRI_242 (2 bits)
    static const uint8_t IPR6_PRI_243 = 30;         // PRI_243 (2 bits)
    static const uint32_t IPR6_RESET_VALUE = 0x0;

    static const uint8_t IPR7_PRI_280 = 6;          // PRI_280 (2 bits)
    static const uint8_t IPR7_PRI_281 = 14;         // PRI_281 (2 bits)
    static const uint8_t IPR7_PRI_282 = 22;         // PRI_282 (2 bits)
    static const uint8_t IPR7_PRI_283 = 30;         // PRI_283 (2 bits)
    static const uint32_t IPR7_RESET_VALUE = 0x0;
};

static nvic_t& NVIC = *reinterpret_cast<nvic_t*>(0xe000e100);


////
//
//    DMA controller
//
////

struct dma1_t
{
    volatile uint32_t    ISR;                  // [Read-only] DMA interrupt status register (DMA_ISR)
    volatile uint32_t    IFCR;                 // [Write-only] DMA interrupt flag clear register (DMA_IFCR)
    volatile uint32_t    CCR1;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR1;               // [Read-write] DMA channel 1 number of data register
    volatile uint32_t    CPAR1;                // [Read-write] DMA channel 1 peripheral address register
    volatile uint32_t    CMAR1;                // [Read-write] DMA channel 1 memory address register
    reserved_t<1>        _0;
    volatile uint32_t    CCR2;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR2;               // [Read-write] DMA channel 2 number of data register
    volatile uint32_t    CPAR2;                // [Read-write] DMA channel 2 peripheral address register
    volatile uint32_t    CMAR2;                // [Read-write] DMA channel 2 memory address register
    reserved_t<1>        _1;
    volatile uint32_t    CCR3;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR3;               // [Read-write] DMA channel 3 number of data register
    volatile uint32_t    CPAR3;                // [Read-write] DMA channel 3 peripheral address register
    volatile uint32_t    CMAR3;                // [Read-write] DMA channel 3 memory address register
    reserved_t<1>        _2;
    volatile uint32_t    CCR4;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR4;               // [Read-write] DMA channel 4 number of data register
    volatile uint32_t    CPAR4;                // [Read-write] DMA channel 4 peripheral address register
    volatile uint32_t    CMAR4;                // [Read-write] DMA channel 4 memory address register
    reserved_t<1>        _3;
    volatile uint32_t    CCR5;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR5;               // [Read-write] DMA channel 5 number of data register
    volatile uint32_t    CPAR5;                // [Read-write] DMA channel 5 peripheral address register
    volatile uint32_t    CMAR5;                // [Read-write] DMA channel 5 memory address register
    reserved_t<1>        _4;
    volatile uint32_t    CCR6;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR6;               // [Read-write] DMA channel 6 number of data register
    volatile uint32_t    CPAR6;                // [Read-write] DMA channel 6 peripheral address register
    volatile uint32_t    CMAR6;                // [Read-write] DMA channel 6 memory address register
    reserved_t<1>        _5;
    volatile uint32_t    CCR7;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR7;               // [Read-write] DMA channel 7 number of data register
    volatile uint32_t    CPAR7;                // [Read-write] DMA channel 7 peripheral address register
    volatile uint32_t    CMAR7;                // [Read-write] DMA channel 7 memory address register

    static const uint8_t ISR_GIF1 = 0;             // Channel 1 Global interrupt flag
    static const uint8_t ISR_TCIF1 = 1;            // Channel 1 Transfer Complete flag
    static const uint8_t ISR_HTIF1 = 2;            // Channel 1 Half Transfer Complete flag
    static const uint8_t ISR_TEIF1 = 3;            // Channel 1 Transfer Error flag
    static const uint8_t ISR_GIF2 = 4;             // Channel 2 Global interrupt flag
    static const uint8_t ISR_TCIF2 = 5;            // Channel 2 Transfer Complete flag
    static const uint8_t ISR_HTIF2 = 6;            // Channel 2 Half Transfer Complete flag
    static const uint8_t ISR_TEIF2 = 7;            // Channel 2 Transfer Error flag
    static const uint8_t ISR_GIF3 = 8;             // Channel 3 Global interrupt flag
    static const uint8_t ISR_TCIF3 = 9;            // Channel 3 Transfer Complete flag
    static const uint8_t ISR_HTIF3 = 10;           // Channel 3 Half Transfer Complete flag
    static const uint8_t ISR_TEIF3 = 11;           // Channel 3 Transfer Error flag
    static const uint8_t ISR_GIF4 = 12;            // Channel 4 Global interrupt flag
    static const uint8_t ISR_TCIF4 = 13;           // Channel 4 Transfer Complete flag
    static const uint8_t ISR_HTIF4 = 14;           // Channel 4 Half Transfer Complete flag
    static const uint8_t ISR_TEIF4 = 15;           // Channel 4 Transfer Error flag
    static const uint8_t ISR_GIF5 = 16;            // Channel 5 Global interrupt flag
    static const uint8_t ISR_TCIF5 = 17;           // Channel 5 Transfer Complete flag
    static const uint8_t ISR_HTIF5 = 18;           // Channel 5 Half Transfer Complete flag
    static const uint8_t ISR_TEIF5 = 19;           // Channel 5 Transfer Error flag
    static const uint8_t ISR_GIF6 = 20;            // Channel 6 Global interrupt flag
    static const uint8_t ISR_TCIF6 = 21;           // Channel 6 Transfer Complete flag
    static const uint8_t ISR_HTIF6 = 22;           // Channel 6 Half Transfer Complete flag
    static const uint8_t ISR_TEIF6 = 23;           // Channel 6 Transfer Error flag
    static const uint8_t ISR_GIF7 = 24;            // Channel 7 Global interrupt flag
    static const uint8_t ISR_TCIF7 = 25;           // Channel 7 Transfer Complete flag
    static const uint8_t ISR_HTIF7 = 26;           // Channel 7 Half Transfer Complete flag
    static const uint8_t ISR_TEIF7 = 27;           // Channel 7 Transfer Error flag
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static const uint8_t IFCR_CGIF1 = 0;            // Channel 1 Global interrupt clear
    static const uint8_t IFCR_CTCIF1 = 1;           // Channel 1 Transfer Complete clear
    static const uint8_t IFCR_CHTIF1 = 2;           // Channel 1 Half Transfer clear
    static const uint8_t IFCR_CTEIF1 = 3;           // Channel 1 Transfer Error clear
    static const uint8_t IFCR_CGIF2 = 4;            // Channel 2 Global interrupt clear
    static const uint8_t IFCR_CTCIF2 = 5;           // Channel 2 Transfer Complete clear
    static const uint8_t IFCR_CHTIF2 = 6;           // Channel 2 Half Transfer clear
    static const uint8_t IFCR_CTEIF2 = 7;           // Channel 2 Transfer Error clear
    static const uint8_t IFCR_CGIF3 = 8;            // Channel 3 Global interrupt clear
    static const uint8_t IFCR_CTCIF3 = 9;           // Channel 3 Transfer Complete clear
    static const uint8_t IFCR_CHTIF3 = 10;          // Channel 3 Half Transfer clear
    static const uint8_t IFCR_CTEIF3 = 11;          // Channel 3 Transfer Error clear
    static const uint8_t IFCR_CGIF4 = 12;           // Channel 4 Global interrupt clear
    static const uint8_t IFCR_CTCIF4 = 13;          // Channel 4 Transfer Complete clear
    static const uint8_t IFCR_CHTIF4 = 14;          // Channel 4 Half Transfer clear
    static const uint8_t IFCR_CTEIF4 = 15;          // Channel 4 Transfer Error clear
    static const uint8_t IFCR_CGIF5 = 16;           // Channel 5 Global interrupt clear
    static const uint8_t IFCR_CTCIF5 = 17;          // Channel 5 Transfer Complete clear
    static const uint8_t IFCR_CHTIF5 = 18;          // Channel 5 Half Transfer clear
    static const uint8_t IFCR_CTEIF5 = 19;          // Channel 5 Transfer Error clear
    static const uint8_t IFCR_CGIF6 = 20;           // Channel 6 Global interrupt clear
    static const uint8_t IFCR_CTCIF6 = 21;          // Channel 6 Transfer Complete clear
    static const uint8_t IFCR_CHTIF6 = 22;          // Channel 6 Half Transfer clear
    static const uint8_t IFCR_CTEIF6 = 23;          // Channel 6 Transfer Error clear
    static const uint8_t IFCR_CGIF7 = 24;           // Channel 7 Global interrupt clear
    static const uint8_t IFCR_CTCIF7 = 25;          // Channel 7 Transfer Complete clear
    static const uint8_t IFCR_CHTIF7 = 26;          // Channel 7 Half Transfer clear
    static const uint8_t IFCR_CTEIF7 = 27;          // Channel 7 Transfer Error clear
    static const uint32_t IFCR_RESET_VALUE = 0x0;

    static const uint8_t CCR1_EN = 0;               // Channel enable
    static const uint8_t CCR1_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR1_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR1_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR1_DIR = 4;              // Data transfer direction
    static const uint8_t CCR1_CIRC = 5;             // Circular mode
    static const uint8_t CCR1_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR1_MINC = 7;             // Memory increment mode
    static const uint8_t CCR1_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR1_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR1_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR1_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    static const uint8_t CNDTR1_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR1_RESET_VALUE = 0x0;

    static const uint8_t CPAR1_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR1_RESET_VALUE = 0x0;

    static const uint8_t CMAR1_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR1_RESET_VALUE = 0x0;

    static const uint8_t CCR2_EN = 0;               // Channel enable
    static const uint8_t CCR2_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR2_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR2_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR2_DIR = 4;              // Data transfer direction
    static const uint8_t CCR2_CIRC = 5;             // Circular mode
    static const uint8_t CCR2_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR2_MINC = 7;             // Memory increment mode
    static const uint8_t CCR2_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR2_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR2_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR2_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    static const uint8_t CNDTR2_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR2_RESET_VALUE = 0x0;

    static const uint8_t CPAR2_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR2_RESET_VALUE = 0x0;

    static const uint8_t CMAR2_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR2_RESET_VALUE = 0x0;

    static const uint8_t CCR3_EN = 0;               // Channel enable
    static const uint8_t CCR3_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR3_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR3_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR3_DIR = 4;              // Data transfer direction
    static const uint8_t CCR3_CIRC = 5;             // Circular mode
    static const uint8_t CCR3_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR3_MINC = 7;             // Memory increment mode
    static const uint8_t CCR3_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR3_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR3_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR3_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    static const uint8_t CNDTR3_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR3_RESET_VALUE = 0x0;

    static const uint8_t CPAR3_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR3_RESET_VALUE = 0x0;

    static const uint8_t CMAR3_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR3_RESET_VALUE = 0x0;

    static const uint8_t CCR4_EN = 0;               // Channel enable
    static const uint8_t CCR4_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR4_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR4_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR4_DIR = 4;              // Data transfer direction
    static const uint8_t CCR4_CIRC = 5;             // Circular mode
    static const uint8_t CCR4_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR4_MINC = 7;             // Memory increment mode
    static const uint8_t CCR4_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR4_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR4_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR4_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR4_RESET_VALUE = 0x0;

    static const uint8_t CNDTR4_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR4_RESET_VALUE = 0x0;

    static const uint8_t CPAR4_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR4_RESET_VALUE = 0x0;

    static const uint8_t CMAR4_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR4_RESET_VALUE = 0x0;

    static const uint8_t CCR5_EN = 0;               // Channel enable
    static const uint8_t CCR5_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR5_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR5_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR5_DIR = 4;              // Data transfer direction
    static const uint8_t CCR5_CIRC = 5;             // Circular mode
    static const uint8_t CCR5_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR5_MINC = 7;             // Memory increment mode
    static const uint8_t CCR5_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR5_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR5_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR5_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR5_RESET_VALUE = 0x0;

    static const uint8_t CNDTR5_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR5_RESET_VALUE = 0x0;

    static const uint8_t CPAR5_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR5_RESET_VALUE = 0x0;

    static const uint8_t CMAR5_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR5_RESET_VALUE = 0x0;

    static const uint8_t CCR6_EN = 0;               // Channel enable
    static const uint8_t CCR6_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR6_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR6_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR6_DIR = 4;              // Data transfer direction
    static const uint8_t CCR6_CIRC = 5;             // Circular mode
    static const uint8_t CCR6_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR6_MINC = 7;             // Memory increment mode
    static const uint8_t CCR6_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR6_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR6_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR6_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR6_RESET_VALUE = 0x0;

    static const uint8_t CNDTR6_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR6_RESET_VALUE = 0x0;

    static const uint8_t CPAR6_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR6_RESET_VALUE = 0x0;

    static const uint8_t CMAR6_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR6_RESET_VALUE = 0x0;

    static const uint8_t CCR7_EN = 0;               // Channel enable
    static const uint8_t CCR7_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR7_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR7_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR7_DIR = 4;              // Data transfer direction
    static const uint8_t CCR7_CIRC = 5;             // Circular mode
    static const uint8_t CCR7_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR7_MINC = 7;             // Memory increment mode
    static const uint8_t CCR7_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR7_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR7_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR7_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR7_RESET_VALUE = 0x0;

    static const uint8_t CNDTR7_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR7_RESET_VALUE = 0x0;

    static const uint8_t CPAR7_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR7_RESET_VALUE = 0x0;

    static const uint8_t CMAR7_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR7_RESET_VALUE = 0x0;
};

static dma1_t& DMA1 = *reinterpret_cast<dma1_t*>(0x40020000);


////
//
//    DMA controller
//
////

struct dma2_t
{
    volatile uint32_t    ISR;                  // [Read-only] DMA interrupt status register (DMA_ISR)
    volatile uint32_t    IFCR;                 // [Write-only] DMA interrupt flag clear register (DMA_IFCR)
    volatile uint32_t    CCR1;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR1;               // [Read-write] DMA channel 1 number of data register
    volatile uint32_t    CPAR1;                // [Read-write] DMA channel 1 peripheral address register
    volatile uint32_t    CMAR1;                // [Read-write] DMA channel 1 memory address register
    reserved_t<1>        _0;
    volatile uint32_t    CCR2;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR2;               // [Read-write] DMA channel 2 number of data register
    volatile uint32_t    CPAR2;                // [Read-write] DMA channel 2 peripheral address register
    volatile uint32_t    CMAR2;                // [Read-write] DMA channel 2 memory address register
    reserved_t<1>        _1;
    volatile uint32_t    CCR3;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR3;               // [Read-write] DMA channel 3 number of data register
    volatile uint32_t    CPAR3;                // [Read-write] DMA channel 3 peripheral address register
    volatile uint32_t    CMAR3;                // [Read-write] DMA channel 3 memory address register
    reserved_t<1>        _2;
    volatile uint32_t    CCR4;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR4;               // [Read-write] DMA channel 4 number of data register
    volatile uint32_t    CPAR4;                // [Read-write] DMA channel 4 peripheral address register
    volatile uint32_t    CMAR4;                // [Read-write] DMA channel 4 memory address register
    reserved_t<1>        _3;
    volatile uint32_t    CCR5;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR5;               // [Read-write] DMA channel 5 number of data register
    volatile uint32_t    CPAR5;                // [Read-write] DMA channel 5 peripheral address register
    volatile uint32_t    CMAR5;                // [Read-write] DMA channel 5 memory address register
    reserved_t<1>        _4;
    volatile uint32_t    CCR6;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR6;               // [Read-write] DMA channel 6 number of data register
    volatile uint32_t    CPAR6;                // [Read-write] DMA channel 6 peripheral address register
    volatile uint32_t    CMAR6;                // [Read-write] DMA channel 6 memory address register
    reserved_t<1>        _5;
    volatile uint32_t    CCR7;                 // [Read-write] DMA channel configuration register (DMA_CCR)
    volatile uint32_t    CNDTR7;               // [Read-write] DMA channel 7 number of data register
    volatile uint32_t    CPAR7;                // [Read-write] DMA channel 7 peripheral address register
    volatile uint32_t    CMAR7;                // [Read-write] DMA channel 7 memory address register

    static const uint8_t ISR_GIF1 = 0;             // Channel 1 Global interrupt flag
    static const uint8_t ISR_TCIF1 = 1;            // Channel 1 Transfer Complete flag
    static const uint8_t ISR_HTIF1 = 2;            // Channel 1 Half Transfer Complete flag
    static const uint8_t ISR_TEIF1 = 3;            // Channel 1 Transfer Error flag
    static const uint8_t ISR_GIF2 = 4;             // Channel 2 Global interrupt flag
    static const uint8_t ISR_TCIF2 = 5;            // Channel 2 Transfer Complete flag
    static const uint8_t ISR_HTIF2 = 6;            // Channel 2 Half Transfer Complete flag
    static const uint8_t ISR_TEIF2 = 7;            // Channel 2 Transfer Error flag
    static const uint8_t ISR_GIF3 = 8;             // Channel 3 Global interrupt flag
    static const uint8_t ISR_TCIF3 = 9;            // Channel 3 Transfer Complete flag
    static const uint8_t ISR_HTIF3 = 10;           // Channel 3 Half Transfer Complete flag
    static const uint8_t ISR_TEIF3 = 11;           // Channel 3 Transfer Error flag
    static const uint8_t ISR_GIF4 = 12;            // Channel 4 Global interrupt flag
    static const uint8_t ISR_TCIF4 = 13;           // Channel 4 Transfer Complete flag
    static const uint8_t ISR_HTIF4 = 14;           // Channel 4 Half Transfer Complete flag
    static const uint8_t ISR_TEIF4 = 15;           // Channel 4 Transfer Error flag
    static const uint8_t ISR_GIF5 = 16;            // Channel 5 Global interrupt flag
    static const uint8_t ISR_TCIF5 = 17;           // Channel 5 Transfer Complete flag
    static const uint8_t ISR_HTIF5 = 18;           // Channel 5 Half Transfer Complete flag
    static const uint8_t ISR_TEIF5 = 19;           // Channel 5 Transfer Error flag
    static const uint8_t ISR_GIF6 = 20;            // Channel 6 Global interrupt flag
    static const uint8_t ISR_TCIF6 = 21;           // Channel 6 Transfer Complete flag
    static const uint8_t ISR_HTIF6 = 22;           // Channel 6 Half Transfer Complete flag
    static const uint8_t ISR_TEIF6 = 23;           // Channel 6 Transfer Error flag
    static const uint8_t ISR_GIF7 = 24;            // Channel 7 Global interrupt flag
    static const uint8_t ISR_TCIF7 = 25;           // Channel 7 Transfer Complete flag
    static const uint8_t ISR_HTIF7 = 26;           // Channel 7 Half Transfer Complete flag
    static const uint8_t ISR_TEIF7 = 27;           // Channel 7 Transfer Error flag
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static const uint8_t IFCR_CGIF1 = 0;            // Channel 1 Global interrupt clear
    static const uint8_t IFCR_CTCIF1 = 1;           // Channel 1 Transfer Complete clear
    static const uint8_t IFCR_CHTIF1 = 2;           // Channel 1 Half Transfer clear
    static const uint8_t IFCR_CTEIF1 = 3;           // Channel 1 Transfer Error clear
    static const uint8_t IFCR_CGIF2 = 4;            // Channel 2 Global interrupt clear
    static const uint8_t IFCR_CTCIF2 = 5;           // Channel 2 Transfer Complete clear
    static const uint8_t IFCR_CHTIF2 = 6;           // Channel 2 Half Transfer clear
    static const uint8_t IFCR_CTEIF2 = 7;           // Channel 2 Transfer Error clear
    static const uint8_t IFCR_CGIF3 = 8;            // Channel 3 Global interrupt clear
    static const uint8_t IFCR_CTCIF3 = 9;           // Channel 3 Transfer Complete clear
    static const uint8_t IFCR_CHTIF3 = 10;          // Channel 3 Half Transfer clear
    static const uint8_t IFCR_CTEIF3 = 11;          // Channel 3 Transfer Error clear
    static const uint8_t IFCR_CGIF4 = 12;           // Channel 4 Global interrupt clear
    static const uint8_t IFCR_CTCIF4 = 13;          // Channel 4 Transfer Complete clear
    static const uint8_t IFCR_CHTIF4 = 14;          // Channel 4 Half Transfer clear
    static const uint8_t IFCR_CTEIF4 = 15;          // Channel 4 Transfer Error clear
    static const uint8_t IFCR_CGIF5 = 16;           // Channel 5 Global interrupt clear
    static const uint8_t IFCR_CTCIF5 = 17;          // Channel 5 Transfer Complete clear
    static const uint8_t IFCR_CHTIF5 = 18;          // Channel 5 Half Transfer clear
    static const uint8_t IFCR_CTEIF5 = 19;          // Channel 5 Transfer Error clear
    static const uint8_t IFCR_CGIF6 = 20;           // Channel 6 Global interrupt clear
    static const uint8_t IFCR_CTCIF6 = 21;          // Channel 6 Transfer Complete clear
    static const uint8_t IFCR_CHTIF6 = 22;          // Channel 6 Half Transfer clear
    static const uint8_t IFCR_CTEIF6 = 23;          // Channel 6 Transfer Error clear
    static const uint8_t IFCR_CGIF7 = 24;           // Channel 7 Global interrupt clear
    static const uint8_t IFCR_CTCIF7 = 25;          // Channel 7 Transfer Complete clear
    static const uint8_t IFCR_CHTIF7 = 26;          // Channel 7 Half Transfer clear
    static const uint8_t IFCR_CTEIF7 = 27;          // Channel 7 Transfer Error clear
    static const uint32_t IFCR_RESET_VALUE = 0x0;

    static const uint8_t CCR1_EN = 0;               // Channel enable
    static const uint8_t CCR1_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR1_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR1_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR1_DIR = 4;              // Data transfer direction
    static const uint8_t CCR1_CIRC = 5;             // Circular mode
    static const uint8_t CCR1_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR1_MINC = 7;             // Memory increment mode
    static const uint8_t CCR1_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR1_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR1_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR1_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    static const uint8_t CNDTR1_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR1_RESET_VALUE = 0x0;

    static const uint8_t CPAR1_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR1_RESET_VALUE = 0x0;

    static const uint8_t CMAR1_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR1_RESET_VALUE = 0x0;

    static const uint8_t CCR2_EN = 0;               // Channel enable
    static const uint8_t CCR2_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR2_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR2_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR2_DIR = 4;              // Data transfer direction
    static const uint8_t CCR2_CIRC = 5;             // Circular mode
    static const uint8_t CCR2_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR2_MINC = 7;             // Memory increment mode
    static const uint8_t CCR2_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR2_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR2_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR2_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    static const uint8_t CNDTR2_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR2_RESET_VALUE = 0x0;

    static const uint8_t CPAR2_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR2_RESET_VALUE = 0x0;

    static const uint8_t CMAR2_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR2_RESET_VALUE = 0x0;

    static const uint8_t CCR3_EN = 0;               // Channel enable
    static const uint8_t CCR3_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR3_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR3_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR3_DIR = 4;              // Data transfer direction
    static const uint8_t CCR3_CIRC = 5;             // Circular mode
    static const uint8_t CCR3_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR3_MINC = 7;             // Memory increment mode
    static const uint8_t CCR3_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR3_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR3_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR3_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    static const uint8_t CNDTR3_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR3_RESET_VALUE = 0x0;

    static const uint8_t CPAR3_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR3_RESET_VALUE = 0x0;

    static const uint8_t CMAR3_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR3_RESET_VALUE = 0x0;

    static const uint8_t CCR4_EN = 0;               // Channel enable
    static const uint8_t CCR4_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR4_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR4_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR4_DIR = 4;              // Data transfer direction
    static const uint8_t CCR4_CIRC = 5;             // Circular mode
    static const uint8_t CCR4_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR4_MINC = 7;             // Memory increment mode
    static const uint8_t CCR4_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR4_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR4_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR4_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR4_RESET_VALUE = 0x0;

    static const uint8_t CNDTR4_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR4_RESET_VALUE = 0x0;

    static const uint8_t CPAR4_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR4_RESET_VALUE = 0x0;

    static const uint8_t CMAR4_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR4_RESET_VALUE = 0x0;

    static const uint8_t CCR5_EN = 0;               // Channel enable
    static const uint8_t CCR5_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR5_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR5_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR5_DIR = 4;              // Data transfer direction
    static const uint8_t CCR5_CIRC = 5;             // Circular mode
    static const uint8_t CCR5_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR5_MINC = 7;             // Memory increment mode
    static const uint8_t CCR5_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR5_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR5_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR5_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR5_RESET_VALUE = 0x0;

    static const uint8_t CNDTR5_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR5_RESET_VALUE = 0x0;

    static const uint8_t CPAR5_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR5_RESET_VALUE = 0x0;

    static const uint8_t CMAR5_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR5_RESET_VALUE = 0x0;

    static const uint8_t CCR6_EN = 0;               // Channel enable
    static const uint8_t CCR6_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR6_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR6_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR6_DIR = 4;              // Data transfer direction
    static const uint8_t CCR6_CIRC = 5;             // Circular mode
    static const uint8_t CCR6_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR6_MINC = 7;             // Memory increment mode
    static const uint8_t CCR6_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR6_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR6_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR6_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR6_RESET_VALUE = 0x0;

    static const uint8_t CNDTR6_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR6_RESET_VALUE = 0x0;

    static const uint8_t CPAR6_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR6_RESET_VALUE = 0x0;

    static const uint8_t CMAR6_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR6_RESET_VALUE = 0x0;

    static const uint8_t CCR7_EN = 0;               // Channel enable
    static const uint8_t CCR7_TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t CCR7_HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t CCR7_TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t CCR7_DIR = 4;              // Data transfer direction
    static const uint8_t CCR7_CIRC = 5;             // Circular mode
    static const uint8_t CCR7_PINC = 6;             // Peripheral increment mode
    static const uint8_t CCR7_MINC = 7;             // Memory increment mode
    static const uint8_t CCR7_PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t CCR7_MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t CCR7_PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t CCR7_MEM2MEM = 14;         // Memory to memory mode
    static const uint32_t CCR7_RESET_VALUE = 0x0;

    static const uint8_t CNDTR7_NDT = 0;              // Number of data to transfer (16 bits)
    static const uint32_t CNDTR7_RESET_VALUE = 0x0;

    static const uint8_t CPAR7_PA = 0;               // Peripheral address (32 bits)
    static const uint32_t CPAR7_RESET_VALUE = 0x0;

    static const uint8_t CMAR7_MA = 0;               // Memory address (32 bits)
    static const uint32_t CMAR7_RESET_VALUE = 0x0;
};

static dma2_t& DMA2 = *reinterpret_cast<dma2_t*>(0x40020400);


////
//
//    Reset and clock control
//
////

struct rcc_t
{
    volatile uint32_t    CR;                   // Clock control register
    volatile uint32_t    CFGR;                 // Clock configuration register (RCC_CFGR)
    volatile uint32_t    CIR;                  // Clock interrupt register (RCC_CIR)
    volatile uint32_t    APB2RSTR;             // [Read-write] APB2 peripheral reset register (RCC_APB2RSTR)
    volatile uint32_t    APB1RSTR;             // [Read-write] APB1 peripheral reset register (RCC_APB1RSTR)
    volatile uint32_t    AHBENR;               // [Read-write] AHB Peripheral Clock enable register (RCC_AHBENR)
    volatile uint32_t    APB2ENR;              // [Read-write] APB2 peripheral clock enable register (RCC_APB2ENR)
    volatile uint32_t    APB1ENR;              // [Read-write] APB1 peripheral clock enable register (RCC_APB1ENR)
    volatile uint32_t    BDCR;                 // Backup domain control register (RCC_BDCR)
    volatile uint32_t    CSR;                  // Control/status register (RCC_CSR)
    volatile uint32_t    AHBRSTR;              // [Read-write] AHB peripheral reset register
    volatile uint32_t    CFGR2;                // [Read-write] Clock configuration register 2
    volatile uint32_t    CFGR3;                // [Read-write] Clock configuration register 3
    volatile uint32_t    CR2;                  // Clock control register 2

    static const uint8_t CR_HSION = 0;            // Internal High Speed clock enable, Read-write
    static const uint8_t CR_HSIRDY = 1;           // Internal High Speed clock ready flag, Read-only
    static const uint8_t CR_HSITRIM = 3;          // Internal High Speed clock trimming (5 bits), Read-write
    static const uint8_t CR_HSICAL = 8;           // Internal High Speed clock Calibration (8 bits), Read-only
    static const uint8_t CR_HSEON = 16;           // External High Speed clock enable, Read-write
    static const uint8_t CR_HSERDY = 17;          // External High Speed clock ready flag, Read-only
    static const uint8_t CR_HSEBYP = 18;          // External High Speed clock Bypass, Read-write
    static const uint8_t CR_CSSON = 19;           // Clock Security System enable, Read-write
    static const uint8_t CR_PLLON = 24;           // PLL enable, Read-write
    static const uint8_t CR_PLLRDY = 25;          // PLL clock ready flag, Read-only
    static const uint32_t CR_RESET_VALUE = 0x83;

    static const uint8_t CFGR_SW = 0;               // System clock Switch (2 bits), Read-write
    static const uint8_t CFGR_SWS = 2;              // System Clock Switch Status (2 bits), Read-only
    static const uint8_t CFGR_HPRE = 4;             // AHB prescaler (4 bits), Read-write
    static const uint8_t CFGR_PPRE = 8;             // APB Low speed prescaler (APB1) (3 bits), Read-write
    static const uint8_t CFGR_ADCPRE = 14;          // ADC prescaler, Read-write
    static const uint8_t CFGR_PLLSRC = 15;          // PLL input clock source (2 bits), Read-write
    static const uint8_t CFGR_PLLXTPRE = 17;        // HSE divider for PLL entry, Read-write
    static const uint8_t CFGR_PLLMUL = 18;          // PLL Multiplication Factor (4 bits), Read-write
    static const uint8_t CFGR_MCO = 24;             // Microcontroller clock output (3 bits), Read-write
    static const uint8_t CFGR_MCOPRE = 28;          // Microcontroller Clock Output Prescaler (3 bits), Read-write
    static const uint8_t CFGR_PLLNODIV = 31;        // PLL clock not divided for MCO, Read-write
    static const uint32_t CFGR_RESET_VALUE = 0x0;

    static const uint8_t CIR_LSIRDYF = 0;          // LSI Ready Interrupt flag, Read-only
    static const uint8_t CIR_LSERDYF = 1;          // LSE Ready Interrupt flag, Read-only
    static const uint8_t CIR_HSIRDYF = 2;          // HSI Ready Interrupt flag, Read-only
    static const uint8_t CIR_HSERDYF = 3;          // HSE Ready Interrupt flag, Read-only
    static const uint8_t CIR_PLLRDYF = 4;          // PLL Ready Interrupt flag, Read-only
    static const uint8_t CIR_HSI14RDYF = 5;        // HSI14 ready interrupt flag, Read-only
    static const uint8_t CIR_HSI48RDYF = 6;        // HSI48 ready interrupt flag, Read-only
    static const uint8_t CIR_CSSF = 7;             // Clock Security System Interrupt flag, Read-only
    static const uint8_t CIR_LSIRDYIE = 8;         // LSI Ready Interrupt Enable, Read-write
    static const uint8_t CIR_LSERDYIE = 9;         // LSE Ready Interrupt Enable, Read-write
    static const uint8_t CIR_HSIRDYIE = 10;        // HSI Ready Interrupt Enable, Read-write
    static const uint8_t CIR_HSERDYIE = 11;        // HSE Ready Interrupt Enable, Read-write
    static const uint8_t CIR_PLLRDYIE = 12;        // PLL Ready Interrupt Enable, Read-write
    static const uint8_t CIR_HSI14RDYE = 13;       // HSI14 ready interrupt enable, Read-write
    static const uint8_t CIR_HSI48RDYIE = 14;      // HSI48 ready interrupt enable, Read-write
    static const uint8_t CIR_LSIRDYC = 16;         // LSI Ready Interrupt Clear, Write-only
    static const uint8_t CIR_LSERDYC = 17;         // LSE Ready Interrupt Clear, Write-only
    static const uint8_t CIR_HSIRDYC = 18;         // HSI Ready Interrupt Clear, Write-only
    static const uint8_t CIR_HSERDYC = 19;         // HSE Ready Interrupt Clear, Write-only
    static const uint8_t CIR_PLLRDYC = 20;         // PLL Ready Interrupt Clear, Write-only
    static const uint8_t CIR_HSI14RDYC = 21;       // HSI 14 MHz Ready Interrupt Clear, Write-only
    static const uint8_t CIR_HSI48RDYC = 22;       // HSI48 Ready Interrupt Clear, Write-only
    static const uint8_t CIR_CSSC = 23;            // Clock security system interrupt clear, Write-only
    static const uint32_t CIR_RESET_VALUE = 0x0;

    static const uint8_t APB2RSTR_SYSCFGRST = 0;        // SYSCFG and COMP reset
    static const uint8_t APB2RSTR_ADCRST = 9;           // ADC interface reset
    static const uint8_t APB2RSTR_TIM1RST = 11;         // TIM1 timer reset
    static const uint8_t APB2RSTR_SPI1RST = 12;         // SPI 1 reset
    static const uint8_t APB2RSTR_USART1RST = 14;       // USART1 reset
    static const uint8_t APB2RSTR_TIM15RST = 16;        // TIM15 timer reset
    static const uint8_t APB2RSTR_TIM16RST = 17;        // TIM16 timer reset
    static const uint8_t APB2RSTR_TIM17RST = 18;        // TIM17 timer reset
    static const uint8_t APB2RSTR_DBGMCURST = 22;       // Debug MCU reset
    static const uint32_t APB2RSTR_RESET_VALUE = 0x0;

    static const uint8_t APB1RSTR_TIM2RST = 0;          // Timer 2 reset
    static const uint8_t APB1RSTR_TIM3RST = 1;          // Timer 3 reset
    static const uint8_t APB1RSTR_TIM6RST = 4;          // Timer 6 reset
    static const uint8_t APB1RSTR_TIM7RST = 5;          // TIM7 timer reset
    static const uint8_t APB1RSTR_TIM14RST = 8;         // Timer 14 reset
    static const uint8_t APB1RSTR_WWDGRST = 11;         // Window watchdog reset
    static const uint8_t APB1RSTR_SPI2RST = 14;         // SPI2 reset
    static const uint8_t APB1RSTR_USART2RST = 17;       // USART 2 reset
    static const uint8_t APB1RSTR_USART3RST = 18;       // USART3 reset
    static const uint8_t APB1RSTR_USART4RST = 19;       // USART4 reset
    static const uint8_t APB1RSTR_USART5RST = 20;       // USART5 reset
    static const uint8_t APB1RSTR_I2C1RST = 21;         // I2C1 reset
    static const uint8_t APB1RSTR_I2C2RST = 22;         // I2C2 reset
    static const uint8_t APB1RSTR_USBRST = 23;          // USB interface reset
    static const uint8_t APB1RSTR_CANRST = 25;          // CAN interface reset
    static const uint8_t APB1RSTR_CRSRST = 27;          // Clock Recovery System interface reset
    static const uint8_t APB1RSTR_PWRRST = 28;          // Power interface reset
    static const uint8_t APB1RSTR_DACRST = 29;          // DAC interface reset
    static const uint8_t APB1RSTR_CECRST = 30;          // HDMI CEC reset
    static const uint32_t APB1RSTR_RESET_VALUE = 0x0;

    static const uint8_t AHBENR_DMA1EN = 0;           // DMA1 clock enable
    static const uint8_t AHBENR_DMA2EN = 1;           // DMA2 clock enable
    static const uint8_t AHBENR_SRAMEN = 2;           // SRAM interface clock enable
    static const uint8_t AHBENR_FLITFEN = 4;          // FLITF clock enable
    static const uint8_t AHBENR_CRCEN = 6;            // CRC clock enable
    static const uint8_t AHBENR_IOPAEN = 17;          // I/O port A clock enable
    static const uint8_t AHBENR_IOPBEN = 18;          // I/O port B clock enable
    static const uint8_t AHBENR_IOPCEN = 19;          // I/O port C clock enable
    static const uint8_t AHBENR_IOPDEN = 20;          // I/O port D clock enable
    static const uint8_t AHBENR_IOPFEN = 22;          // I/O port F clock enable
    static const uint8_t AHBENR_TSCEN = 24;           // Touch sensing controller clock enable
    static const uint32_t AHBENR_RESET_VALUE = 0x14;

    static const uint8_t APB2ENR_SYSCFGEN = 0;         // SYSCFG clock enable
    static const uint8_t APB2ENR_ADCEN = 9;            // ADC 1 interface clock enable
    static const uint8_t APB2ENR_TIM1EN = 11;          // TIM1 Timer clock enable
    static const uint8_t APB2ENR_SPI1EN = 12;          // SPI 1 clock enable
    static const uint8_t APB2ENR_USART1EN = 14;        // USART1 clock enable
    static const uint8_t APB2ENR_TIM15EN = 16;         // TIM15 timer clock enable
    static const uint8_t APB2ENR_TIM16EN = 17;         // TIM16 timer clock enable
    static const uint8_t APB2ENR_TIM17EN = 18;         // TIM17 timer clock enable
    static const uint8_t APB2ENR_DBGMCUEN = 22;        // MCU debug module clock enable
    static const uint8_t APB2ENR_USART8EN = 7;         // USART8 clock enable
    static const uint8_t APB2ENR_USART7EN = 6;         // USART7 clock enable
    static const uint8_t APB2ENR_USART6EN = 5;         // USART6 clock enable
    static const uint32_t APB2ENR_RESET_VALUE = 0x0;

    static const uint8_t APB1ENR_TIM2EN = 0;           // Timer 2 clock enable
    static const uint8_t APB1ENR_TIM3EN = 1;           // Timer 3 clock enable
    static const uint8_t APB1ENR_TIM6EN = 4;           // Timer 6 clock enable
    static const uint8_t APB1ENR_TIM7EN = 5;           // TIM7 timer clock enable
    static const uint8_t APB1ENR_TIM14EN = 8;          // Timer 14 clock enable
    static const uint8_t APB1ENR_WWDGEN = 11;          // Window watchdog clock enable
    static const uint8_t APB1ENR_SPI2EN = 14;          // SPI 2 clock enable
    static const uint8_t APB1ENR_USART2EN = 17;        // USART 2 clock enable
    static const uint8_t APB1ENR_USART3EN = 18;        // USART3 clock enable
    static const uint8_t APB1ENR_USART4EN = 19;        // USART4 clock enable
    static const uint8_t APB1ENR_USART5EN = 20;        // USART5 clock enable
    static const uint8_t APB1ENR_I2C1EN = 21;          // I2C 1 clock enable
    static const uint8_t APB1ENR_I2C2EN = 22;          // I2C 2 clock enable
    static const uint8_t APB1ENR_USBRST = 23;          // USB interface clock enable
    static const uint8_t APB1ENR_CANEN = 25;           // CAN interface clock enable
    static const uint8_t APB1ENR_CRSEN = 27;           // Clock Recovery System interface clock enable
    static const uint8_t APB1ENR_PWREN = 28;           // Power interface clock enable
    static const uint8_t APB1ENR_DACEN = 29;           // DAC interface clock enable
    static const uint8_t APB1ENR_CECEN = 30;           // HDMI CEC interface clock enable
    static const uint32_t APB1ENR_RESET_VALUE = 0x0;

    static const uint8_t BDCR_LSEON = 0;            // External Low Speed oscillator enable, Read-write
    static const uint8_t BDCR_LSERDY = 1;           // External Low Speed oscillator ready, Read-only
    static const uint8_t BDCR_LSEBYP = 2;           // External Low Speed oscillator bypass, Read-write
    static const uint8_t BDCR_LSEDRV = 3;           // LSE oscillator drive capability (2 bits), Read-write
    static const uint8_t BDCR_RTCSEL = 8;           // RTC clock source selection (2 bits), Read-write
    static const uint8_t BDCR_RTCEN = 15;           // RTC clock enable, Read-write
    static const uint8_t BDCR_BDRST = 16;           // Backup domain software reset, Read-write
    static const uint32_t BDCR_RESET_VALUE = 0x0;

    static const uint8_t CSR_LSION = 0;            // Internal low speed oscillator enable, Read-write
    static const uint8_t CSR_LSIRDY = 1;           // Internal low speed oscillator ready, Read-only
    static const uint8_t CSR_RMVF = 24;            // Remove reset flag, Read-write
    static const uint8_t CSR_OBLRSTF = 25;         // Option byte loader reset flag, Read-write
    static const uint8_t CSR_PINRSTF = 26;         // PIN reset flag, Read-write
    static const uint8_t CSR_PORRSTF = 27;         // POR/PDR reset flag, Read-write
    static const uint8_t CSR_SFTRSTF = 28;         // Software reset flag, Read-write
    static const uint8_t CSR_IWDGRSTF = 29;        // Independent watchdog reset flag, Read-write
    static const uint8_t CSR_WWDGRSTF = 30;        // Window watchdog reset flag, Read-write
    static const uint8_t CSR_LPWRRSTF = 31;        // Low-power reset flag, Read-write
    static const uint32_t CSR_RESET_VALUE = 0xc000000;

    static const uint8_t AHBRSTR_IOPARST = 17;         // I/O port A reset
    static const uint8_t AHBRSTR_IOPBRST = 18;         // I/O port B reset
    static const uint8_t AHBRSTR_IOPCRST = 19;         // I/O port C reset
    static const uint8_t AHBRSTR_IOPDRST = 20;         // I/O port D reset
    static const uint8_t AHBRSTR_IOPFRST = 22;         // I/O port F reset
    static const uint8_t AHBRSTR_TSCRST = 24;          // Touch sensing controller reset
    static const uint32_t AHBRSTR_RESET_VALUE = 0x0;

    static const uint8_t CFGR2_PREDIV = 0;           // PREDIV division factor (4 bits)
    static const uint32_t CFGR2_RESET_VALUE = 0x0;

    static const uint8_t CFGR3_USART1SW = 0;         // USART1 clock source selection (2 bits)
    static const uint8_t CFGR3_I2C1SW = 4;           // I2C1 clock source selection
    static const uint8_t CFGR3_CECSW = 6;            // HDMI CEC clock source selection
    static const uint8_t CFGR3_USBSW = 7;            // USB clock source selection
    static const uint8_t CFGR3_ADCSW = 8;            // ADC clock source selection
    static const uint8_t CFGR3_USART2SW = 16;        // USART2 clock source selection (2 bits)
    static const uint32_t CFGR3_RESET_VALUE = 0x0;

    static const uint8_t CR2_HSI14ON = 0;          // HSI14 clock enable, Read-write
    static const uint8_t CR2_HSI14RDY = 1;         // HR14 clock ready flag, Read-only
    static const uint8_t CR2_HSI14DIS = 2;         // HSI14 clock request from ADC disable, Read-write
    static const uint8_t CR2_HSI14TRIM = 3;        // HSI14 clock trimming (5 bits), Read-write
    static const uint8_t CR2_HSI14CAL = 8;         // HSI14 clock calibration (8 bits), Read-only
    static const uint8_t CR2_HSI48ON = 16;         // HSI48 clock enable, Read-write
    static const uint8_t CR2_HSI48RDY = 17;        // HSI48 clock ready flag, Read-only
    static const uint8_t CR2_HSI48CAL = 24;        // HSI48 factory clock calibration, Read-only
    static const uint32_t CR2_RESET_VALUE = 0x80;
};

static rcc_t& RCC = *reinterpret_cast<rcc_t*>(0x40021000);


////
//
//    System configuration controller
//
////

struct syscfg_comp_t
{
    volatile uint32_t    SYSCFG_CFGR1;         // [Read-write] configuration register 1
    reserved_t<1>        _0;
    volatile uint32_t    SYSCFG_EXTICR1;       // [Read-write] external interrupt configuration register 1
    volatile uint32_t    SYSCFG_EXTICR2;       // [Read-write] external interrupt configuration register 2
    volatile uint32_t    SYSCFG_EXTICR3;       // [Read-write] external interrupt configuration register 3
    volatile uint32_t    SYSCFG_EXTICR4;       // [Read-write] external interrupt configuration register 4
    volatile uint32_t    SYSCFG_CFGR2;         // [Read-write] configuration register 2
    volatile uint32_t    COMP_CSR;             // control and status register

    static const uint8_t SYSCFG_CFGR1_MEM_MODE = 0;         // Memory mapping selection bits (2 bits)
    static const uint8_t SYSCFG_CFGR1_ADC_DMA_RMP = 8;      // ADC DMA remapping bit
    static const uint8_t SYSCFG_CFGR1_USART1_TX_DMA_RMP = 9;// USART1_TX DMA remapping bit
    static const uint8_t SYSCFG_CFGR1_USART1_RX_DMA_RMP = 10;// USART1_RX DMA request remapping bit
    static const uint8_t SYSCFG_CFGR1_TIM16_DMA_RMP = 11;   // TIM16 DMA request remapping bit
    static const uint8_t SYSCFG_CFGR1_TIM17_DMA_RMP = 12;   // TIM17 DMA request remapping bit
    static const uint8_t SYSCFG_CFGR1_I2C_PB6_FM = 16;      // Fast Mode Plus (FM plus) driving capability activation bits.
    static const uint8_t SYSCFG_CFGR1_I2C_PB7_FM = 17;      // Fast Mode Plus (FM+) driving capability activation bits.
    static const uint8_t SYSCFG_CFGR1_I2C_PB8_FM = 18;      // Fast Mode Plus (FM+) driving capability activation bits.
    static const uint8_t SYSCFG_CFGR1_I2C_PB9_FM = 19;      // Fast Mode Plus (FM+) driving capability activation bits.
    static const uint8_t SYSCFG_CFGR1_I2C1_FM_plus = 20;    // FM+ driving capability activation for I2C1
    static const uint8_t SYSCFG_CFGR1_I2C2_FM_plus = 21;    // FM+ driving capability activation for I2C2
    static const uint8_t SYSCFG_CFGR1_SPI2_DMA_RMP = 24;    // SPI2 DMA request remapping bit
    static const uint8_t SYSCFG_CFGR1_USART2_DMA_RMP = 25;  // USART2 DMA request remapping bit
    static const uint8_t SYSCFG_CFGR1_USART3_DMA_RMP = 26;  // USART3 DMA request remapping bit
    static const uint8_t SYSCFG_CFGR1_I2C1_DMA_RMP = 27;    // I2C1 DMA request remapping bit
    static const uint8_t SYSCFG_CFGR1_TIM1_DMA_RMP = 28;    // TIM1 DMA request remapping bit
    static const uint8_t SYSCFG_CFGR1_TIM2_DMA_RMP = 29;    // TIM2 DMA request remapping bit
    static const uint8_t SYSCFG_CFGR1_TIM3_DMA_RMP = 30;    // TIM3 DMA request remapping bit
    static const uint32_t SYSCFG_CFGR1_RESET_VALUE = 0x0;

    static const uint8_t SYSCFG_EXTICR1_EXTI3 = 12;           // EXTI 3 configuration bits (4 bits)
    static const uint8_t SYSCFG_EXTICR1_EXTI2 = 8;            // EXTI 2 configuration bits (4 bits)
    static const uint8_t SYSCFG_EXTICR1_EXTI1 = 4;            // EXTI 1 configuration bits (4 bits)
    static const uint8_t SYSCFG_EXTICR1_EXTI0 = 0;            // EXTI 0 configuration bits (4 bits)
    static const uint32_t SYSCFG_EXTICR1_RESET_VALUE = 0x0;

    static const uint8_t SYSCFG_EXTICR2_EXTI7 = 12;           // EXTI 7 configuration bits (4 bits)
    static const uint8_t SYSCFG_EXTICR2_EXTI6 = 8;            // EXTI 6 configuration bits (4 bits)
    static const uint8_t SYSCFG_EXTICR2_EXTI5 = 4;            // EXTI 5 configuration bits (4 bits)
    static const uint8_t SYSCFG_EXTICR2_EXTI4 = 0;            // EXTI 4 configuration bits (4 bits)
    static const uint32_t SYSCFG_EXTICR2_RESET_VALUE = 0x0;

    static const uint8_t SYSCFG_EXTICR3_EXTI11 = 12;          // EXTI 11 configuration bits (4 bits)
    static const uint8_t SYSCFG_EXTICR3_EXTI10 = 8;           // EXTI 10 configuration bits (4 bits)
    static const uint8_t SYSCFG_EXTICR3_EXTI9 = 4;            // EXTI 9 configuration bits (4 bits)
    static const uint8_t SYSCFG_EXTICR3_EXTI8 = 0;            // EXTI 8 configuration bits (4 bits)
    static const uint32_t SYSCFG_EXTICR3_RESET_VALUE = 0x0;

    static const uint8_t SYSCFG_EXTICR4_EXTI15 = 12;          // EXTI 15 configuration bits (4 bits)
    static const uint8_t SYSCFG_EXTICR4_EXTI14 = 8;           // EXTI 14 configuration bits (4 bits)
    static const uint8_t SYSCFG_EXTICR4_EXTI13 = 4;           // EXTI 13 configuration bits (4 bits)
    static const uint8_t SYSCFG_EXTICR4_EXTI12 = 0;           // EXTI 12 configuration bits (4 bits)
    static const uint32_t SYSCFG_EXTICR4_RESET_VALUE = 0x0;

    static const uint8_t SYSCFG_CFGR2_SRAM_PEF = 8;         // SRAM parity flag
    static const uint8_t SYSCFG_CFGR2_PVD_LOCK = 2;         // PVD lock enable bit
    static const uint8_t SYSCFG_CFGR2_SRAM_PARITY_LOCK = 1; // SRAM parity lock bit
    static const uint8_t SYSCFG_CFGR2_LOCUP_LOCK = 0;       // Cortex-M0 LOCKUP bit enable bit
    static const uint32_t SYSCFG_CFGR2_RESET_VALUE = 0x0;

    static const uint8_t COMP_CSR_COMP1EN = 0;          // Comparator 1 enable, Read-write
    static const uint8_t COMP_CSR_COMP1_INP_DAC = 1;    // COMP1_INP_DAC, Read-write
    static const uint8_t COMP_CSR_COMP1MODE = 2;        // Comparator 1 mode (2 bits), Read-write
    static const uint8_t COMP_CSR_COMP1INSEL = 4;       // Comparator 1 inverting input selection (3 bits), Read-write
    static const uint8_t COMP_CSR_COMP1OUTSEL = 8;      // Comparator 1 output selection (3 bits), Read-write
    static const uint8_t COMP_CSR_COMP1POL = 11;        // Comparator 1 output polarity, Read-write
    static const uint8_t COMP_CSR_COMP1HYST = 12;       // Comparator 1 hysteresis (2 bits), Read-write
    static const uint8_t COMP_CSR_COMP1OUT = 14;        // Comparator 1 output, Read-only
    static const uint8_t COMP_CSR_COMP1LOCK = 15;       // Comparator 1 lock, Read-write
    static const uint8_t COMP_CSR_COMP2EN = 16;         // Comparator 2 enable, Read-write
    static const uint8_t COMP_CSR_COMP2MODE = 18;       // Comparator 2 mode (2 bits), Read-write
    static const uint8_t COMP_CSR_COMP2INSEL = 20;      // Comparator 2 inverting input selection (3 bits), Read-write
    static const uint8_t COMP_CSR_WNDWEN = 23;          // Window mode enable, Read-write
    static const uint8_t COMP_CSR_COMP2OUTSEL = 24;     // Comparator 2 output selection (3 bits), Read-write
    static const uint8_t COMP_CSR_COMP2POL = 27;        // Comparator 2 output polarity, Read-write
    static const uint8_t COMP_CSR_COMP2HYST = 28;       // Comparator 2 hysteresis (2 bits), Read-write
    static const uint8_t COMP_CSR_COMP2OUT = 30;        // Comparator 2 output, Read-only
    static const uint8_t COMP_CSR_COMP2LOCK = 31;       // Comparator 2 lock, Read-write
    static const uint32_t COMP_CSR_RESET_VALUE = 0x0;
};

static syscfg_comp_t& SYSCFG_COMP = *reinterpret_cast<syscfg_comp_t*>(0x40010000);


////
//
//    Analog-to-digital converter
//
////

struct adc_t
{
    volatile uint32_t    ISR;                  // [Read-write] interrupt and status register
    volatile uint32_t    IER;                  // [Read-write] interrupt enable register
    volatile uint32_t    CR;                   // [Read-write] control register
    volatile uint32_t    CFGR1;                // [Read-write] configuration register 1
    volatile uint32_t    CFGR2;                // [Read-write] configuration register 2
    volatile uint32_t    SMPR;                 // [Read-write] sampling time register
    reserved_t<2>        _0;
    volatile uint32_t    TR;                   // [Read-write] watchdog threshold register
    reserved_t<1>        _1;
    volatile uint32_t    CHSELR;               // [Read-write] channel selection register
    reserved_t<5>        _2;
    volatile uint32_t    DR;                   // [Read-only] data register
    reserved_t<177>      _3;
    volatile uint32_t    CCR;                  // [Read-write] common configuration register

    static const uint8_t ISR_AWD = 7;              // Analog watchdog flag
    static const uint8_t ISR_OVR = 4;              // ADC overrun
    static const uint8_t ISR_EOS = 3;              // End of sequence flag
    static const uint8_t ISR_EOC = 2;              // End of conversion flag
    static const uint8_t ISR_EOSMP = 1;            // End of sampling flag
    static const uint8_t ISR_ADRDY = 0;            // ADC ready
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static const uint8_t IER_AWDIE = 7;            // Analog watchdog interrupt enable
    static const uint8_t IER_OVRIE = 4;            // Overrun interrupt enable
    static const uint8_t IER_EOSIE = 3;            // End of conversion sequence interrupt enable
    static const uint8_t IER_EOCIE = 2;            // End of conversion interrupt enable
    static const uint8_t IER_EOSMPIE = 1;          // End of sampling flag interrupt enable
    static const uint8_t IER_ADRDYIE = 0;          // ADC ready interrupt enable
    static const uint32_t IER_RESET_VALUE = 0x0;

    static const uint8_t CR_ADCAL = 31;           // ADC calibration
    static const uint8_t CR_ADSTP = 4;            // ADC stop conversion command
    static const uint8_t CR_ADSTART = 2;          // ADC start conversion command
    static const uint8_t CR_ADDIS = 1;            // ADC disable command
    static const uint8_t CR_ADEN = 0;             // ADC enable command
    static const uint32_t CR_RESET_VALUE = 0x0;

    static const uint8_t CFGR1_AWDCH = 26;           // Analog watchdog channel selection (5 bits)
    static const uint8_t CFGR1_AWDEN = 23;           // Analog watchdog enable
    static const uint8_t CFGR1_AWDSGL = 22;          // Enable the watchdog on a single channel or on all channels
    static const uint8_t CFGR1_DISCEN = 16;          // Discontinuous mode
    static const uint8_t CFGR1_AUTOFF = 15;          // Auto-off mode
    static const uint8_t CFGR1_AUTDLY = 14;          // Auto-delayed conversion mode
    static const uint8_t CFGR1_CONT = 13;            // Single / continuous conversion mode
    static const uint8_t CFGR1_OVRMOD = 12;          // Overrun management mode
    static const uint8_t CFGR1_EXTEN = 10;           // External trigger enable and polarity selection (2 bits)
    static const uint8_t CFGR1_EXTSEL = 6;           // External trigger selection (3 bits)
    static const uint8_t CFGR1_ALIGN = 5;            // Data alignment
    static const uint8_t CFGR1_RES = 3;              // Data resolution (2 bits)
    static const uint8_t CFGR1_SCANDIR = 2;          // Scan sequence direction
    static const uint8_t CFGR1_DMACFG = 1;           // Direct memery access configuration
    static const uint8_t CFGR1_DMAEN = 0;            // Direct memory access enable
    static const uint32_t CFGR1_RESET_VALUE = 0x0;

    static const uint8_t CFGR2_JITOFF_D4 = 31;       // JITOFF_D4
    static const uint8_t CFGR2_JITOFF_D2 = 30;       // JITOFF_D2
    static const uint32_t CFGR2_RESET_VALUE = 0x8000;

    static const uint8_t SMPR_SMPR = 0;             // Sampling time selection (3 bits)
    static const uint32_t SMPR_RESET_VALUE = 0x0;

    static const uint8_t TR_HT = 16;              // Analog watchdog higher threshold (12 bits)
    static const uint8_t TR_LT = 0;               // Analog watchdog lower threshold (12 bits)
    static const uint32_t TR_RESET_VALUE = 0xfff;

    static const uint8_t CHSELR_CHSEL18 = 18;         // Channel-x selection
    static const uint8_t CHSELR_CHSEL17 = 17;         // Channel-x selection
    static const uint8_t CHSELR_CHSEL16 = 16;         // Channel-x selection
    static const uint8_t CHSELR_CHSEL15 = 15;         // Channel-x selection
    static const uint8_t CHSELR_CHSEL14 = 14;         // Channel-x selection
    static const uint8_t CHSELR_CHSEL13 = 13;         // Channel-x selection
    static const uint8_t CHSELR_CHSEL12 = 12;         // Channel-x selection
    static const uint8_t CHSELR_CHSEL11 = 11;         // Channel-x selection
    static const uint8_t CHSELR_CHSEL10 = 10;         // Channel-x selection
    static const uint8_t CHSELR_CHSEL9 = 9;           // Channel-x selection
    static const uint8_t CHSELR_CHSEL8 = 8;           // Channel-x selection
    static const uint8_t CHSELR_CHSEL7 = 7;           // Channel-x selection
    static const uint8_t CHSELR_CHSEL6 = 6;           // Channel-x selection
    static const uint8_t CHSELR_CHSEL5 = 5;           // Channel-x selection
    static const uint8_t CHSELR_CHSEL4 = 4;           // Channel-x selection
    static const uint8_t CHSELR_CHSEL3 = 3;           // Channel-x selection
    static const uint8_t CHSELR_CHSEL2 = 2;           // Channel-x selection
    static const uint8_t CHSELR_CHSEL1 = 1;           // Channel-x selection
    static const uint8_t CHSELR_CHSEL0 = 0;           // Channel-x selection
    static const uint32_t CHSELR_RESET_VALUE = 0x0;

    static const uint8_t DR_DATA = 0;             // Converted data (16 bits)
    static const uint32_t DR_RESET_VALUE = 0x0;

    static const uint8_t CCR_VBATEN = 24;          // VBAT enable
    static const uint8_t CCR_TSEN = 23;            // Temperature sensor enable
    static const uint8_t CCR_VREFEN = 22;          // Temperature sensor and VREFINT enable
    static const uint32_t CCR_RESET_VALUE = 0x0;
};

static adc_t& ADC = *reinterpret_cast<adc_t*>(0x40012400);


////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

struct usart1_t
{
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    CR3;                  // [Read-write] Control register 3
    volatile uint32_t    BRR;                  // [Read-write] Baud rate register
    volatile uint32_t    GTPR;                 // [Read-write] Guard time and prescaler register
    volatile uint32_t    RTOR;                 // [Read-write] Receiver timeout register
    volatile uint32_t    RQR;                  // [Read-write] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Read-write] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register

    static const uint8_t CR1_UE = 0;               // USART enable
    static const uint8_t CR1_UESM = 1;             // USART enable in Stop mode
    static const uint8_t CR1_RE = 2;               // Receiver enable
    static const uint8_t CR1_TE = 3;               // Transmitter enable
    static const uint8_t CR1_IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t CR1_RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t CR1_TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t CR1_TXEIE = 7;            // interrupt enable
    static const uint8_t CR1_PEIE = 8;             // PE interrupt enable
    static const uint8_t CR1_PS = 9;               // Parity selection
    static const uint8_t CR1_PCE = 10;             // Parity control enable
    static const uint8_t CR1_WAKE = 11;            // Receiver wakeup method
    static const uint8_t CR1_M = 12;               // Word length
    static const uint8_t CR1_MME = 13;             // Mute mode enable
    static const uint8_t CR1_CMIE = 14;            // Character match interrupt enable
    static const uint8_t CR1_OVER8 = 15;           // Oversampling mode
    static const uint8_t CR1_DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t CR1_DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t CR1_RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t CR1_EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t CR1_M1 = 28;              // Word length
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t CR2_ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t CR2_RTOEN = 23;           // Receiver timeout enable
    static const uint8_t CR2_ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t CR2_ABREN = 20;           // Auto baud rate enable
    static const uint8_t CR2_MSBFIRST = 19;        // Most significant bit first
    static const uint8_t CR2_DATAINV = 18;         // Binary data inversion
    static const uint8_t CR2_TXINV = 17;           // TX pin active level inversion
    static const uint8_t CR2_RXINV = 16;           // RX pin active level inversion
    static const uint8_t CR2_SWAP = 15;            // Swap TX/RX pins
    static const uint8_t CR2_LINEN = 14;           // LIN mode enable
    static const uint8_t CR2_STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CR2_CLKEN = 11;           // Clock enable
    static const uint8_t CR2_CPOL = 10;            // Clock polarity
    static const uint8_t CR2_CPHA = 9;             // Clock phase
    static const uint8_t CR2_LBCL = 8;             // Last bit clock pulse
    static const uint8_t CR2_LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t CR2_LBDL = 5;             // LIN break detection length
    static const uint8_t CR2_ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t CR3_WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t CR3_WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t CR3_SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t CR3_DEP = 15;             // Driver enable polarity selection
    static const uint8_t CR3_DEM = 14;             // Driver enable mode
    static const uint8_t CR3_DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t CR3_OVRDIS = 12;          // Overrun Disable
    static const uint8_t CR3_ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CR3_CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CR3_CTSE = 9;             // CTS enable
    static const uint8_t CR3_RTSE = 8;             // RTS enable
    static const uint8_t CR3_DMAT = 7;             // DMA enable transmitter
    static const uint8_t CR3_DMAR = 6;             // DMA enable receiver
    static const uint8_t CR3_SCEN = 5;             // Smartcard mode enable
    static const uint8_t CR3_NACK = 4;             // Smartcard NACK enable
    static const uint8_t CR3_HDSEL = 3;            // Half-duplex selection
    static const uint8_t CR3_IRLP = 2;             // IrDA low-power
    static const uint8_t CR3_IREN = 1;             // IrDA mode enable
    static const uint8_t CR3_EIE = 0;              // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    static const uint8_t BRR_DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t BRR_DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static const uint8_t GTPR_GT = 8;               // Guard time value (8 bits)
    static const uint8_t GTPR_PSC = 0;              // Prescaler value (8 bits)
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    static const uint8_t RTOR_BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTOR_RTO = 0;              // Receiver timeout value (24 bits)
    static const uint32_t RTOR_RESET_VALUE = 0x0;

    static const uint8_t RQR_TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RQR_RXFRQ = 3;            // Receive data flush request
    static const uint8_t RQR_MMRQ = 2;             // Mute mode request
    static const uint8_t RQR_SBKRQ = 1;            // Send break request
    static const uint8_t RQR_ABRRQ = 0;            // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static const uint8_t ISR_REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t ISR_TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t ISR_WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t ISR_RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t ISR_SBKF = 18;            // Send break flag
    static const uint8_t ISR_CMF = 17;             // character match flag
    static const uint8_t ISR_BUSY = 16;            // Busy flag
    static const uint8_t ISR_ABRF = 15;            // Auto baud rate flag
    static const uint8_t ISR_ABRE = 14;            // Auto baud rate error
    static const uint8_t ISR_EOBF = 12;            // End of block flag
    static const uint8_t ISR_RTOF = 11;            // Receiver timeout
    static const uint8_t ISR_CTS = 10;             // CTS flag
    static const uint8_t ISR_CTSIF = 9;            // CTS interrupt flag
    static const uint8_t ISR_LBDF = 8;             // LIN break detection flag
    static const uint8_t ISR_TXE = 7;              // Transmit data register empty
    static const uint8_t ISR_TC = 6;               // Transmission complete
    static const uint8_t ISR_RXNE = 5;             // Read data register not empty
    static const uint8_t ISR_IDLE = 4;             // Idle line detected
    static const uint8_t ISR_ORE = 3;              // Overrun error
    static const uint8_t ISR_NF = 2;               // Noise detected flag
    static const uint8_t ISR_FE = 1;               // Framing error
    static const uint8_t ISR_PE = 0;               // Parity error
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static const uint8_t ICR_WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t ICR_CMCF = 17;            // Character match clear flag
    static const uint8_t ICR_EOBCF = 12;           // End of timeout clear flag
    static const uint8_t ICR_RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t ICR_CTSCF = 9;            // CTS clear flag
    static const uint8_t ICR_LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t ICR_TCCF = 6;             // Transmission complete clear flag
    static const uint8_t ICR_IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ICR_ORECF = 3;            // Overrun error clear flag
    static const uint8_t ICR_NCF = 2;              // Noise detected clear flag
    static const uint8_t ICR_FECF = 1;             // Framing error clear flag
    static const uint8_t ICR_PECF = 0;             // Parity error clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static const uint8_t RDR_RDR = 0;              // Receive data value (9 bits)
    static const uint32_t RDR_RESET_VALUE = 0x0;

    static const uint8_t TDR_TDR = 0;              // Transmit data value (9 bits)
    static const uint32_t TDR_RESET_VALUE = 0x0;
};

static usart1_t& USART1 = *reinterpret_cast<usart1_t*>(0x40013800);


////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

struct usart2_t
{
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    CR3;                  // [Read-write] Control register 3
    volatile uint32_t    BRR;                  // [Read-write] Baud rate register
    volatile uint32_t    GTPR;                 // [Read-write] Guard time and prescaler register
    volatile uint32_t    RTOR;                 // [Read-write] Receiver timeout register
    volatile uint32_t    RQR;                  // [Read-write] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Read-write] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register

    static const uint8_t CR1_UE = 0;               // USART enable
    static const uint8_t CR1_UESM = 1;             // USART enable in Stop mode
    static const uint8_t CR1_RE = 2;               // Receiver enable
    static const uint8_t CR1_TE = 3;               // Transmitter enable
    static const uint8_t CR1_IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t CR1_RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t CR1_TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t CR1_TXEIE = 7;            // interrupt enable
    static const uint8_t CR1_PEIE = 8;             // PE interrupt enable
    static const uint8_t CR1_PS = 9;               // Parity selection
    static const uint8_t CR1_PCE = 10;             // Parity control enable
    static const uint8_t CR1_WAKE = 11;            // Receiver wakeup method
    static const uint8_t CR1_M = 12;               // Word length
    static const uint8_t CR1_MME = 13;             // Mute mode enable
    static const uint8_t CR1_CMIE = 14;            // Character match interrupt enable
    static const uint8_t CR1_OVER8 = 15;           // Oversampling mode
    static const uint8_t CR1_DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t CR1_DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t CR1_RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t CR1_EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t CR1_M1 = 28;              // Word length
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t CR2_ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t CR2_RTOEN = 23;           // Receiver timeout enable
    static const uint8_t CR2_ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t CR2_ABREN = 20;           // Auto baud rate enable
    static const uint8_t CR2_MSBFIRST = 19;        // Most significant bit first
    static const uint8_t CR2_DATAINV = 18;         // Binary data inversion
    static const uint8_t CR2_TXINV = 17;           // TX pin active level inversion
    static const uint8_t CR2_RXINV = 16;           // RX pin active level inversion
    static const uint8_t CR2_SWAP = 15;            // Swap TX/RX pins
    static const uint8_t CR2_LINEN = 14;           // LIN mode enable
    static const uint8_t CR2_STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CR2_CLKEN = 11;           // Clock enable
    static const uint8_t CR2_CPOL = 10;            // Clock polarity
    static const uint8_t CR2_CPHA = 9;             // Clock phase
    static const uint8_t CR2_LBCL = 8;             // Last bit clock pulse
    static const uint8_t CR2_LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t CR2_LBDL = 5;             // LIN break detection length
    static const uint8_t CR2_ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t CR3_WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t CR3_WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t CR3_SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t CR3_DEP = 15;             // Driver enable polarity selection
    static const uint8_t CR3_DEM = 14;             // Driver enable mode
    static const uint8_t CR3_DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t CR3_OVRDIS = 12;          // Overrun Disable
    static const uint8_t CR3_ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CR3_CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CR3_CTSE = 9;             // CTS enable
    static const uint8_t CR3_RTSE = 8;             // RTS enable
    static const uint8_t CR3_DMAT = 7;             // DMA enable transmitter
    static const uint8_t CR3_DMAR = 6;             // DMA enable receiver
    static const uint8_t CR3_SCEN = 5;             // Smartcard mode enable
    static const uint8_t CR3_NACK = 4;             // Smartcard NACK enable
    static const uint8_t CR3_HDSEL = 3;            // Half-duplex selection
    static const uint8_t CR3_IRLP = 2;             // IrDA low-power
    static const uint8_t CR3_IREN = 1;             // IrDA mode enable
    static const uint8_t CR3_EIE = 0;              // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    static const uint8_t BRR_DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t BRR_DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static const uint8_t GTPR_GT = 8;               // Guard time value (8 bits)
    static const uint8_t GTPR_PSC = 0;              // Prescaler value (8 bits)
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    static const uint8_t RTOR_BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTOR_RTO = 0;              // Receiver timeout value (24 bits)
    static const uint32_t RTOR_RESET_VALUE = 0x0;

    static const uint8_t RQR_TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RQR_RXFRQ = 3;            // Receive data flush request
    static const uint8_t RQR_MMRQ = 2;             // Mute mode request
    static const uint8_t RQR_SBKRQ = 1;            // Send break request
    static const uint8_t RQR_ABRRQ = 0;            // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static const uint8_t ISR_REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t ISR_TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t ISR_WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t ISR_RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t ISR_SBKF = 18;            // Send break flag
    static const uint8_t ISR_CMF = 17;             // character match flag
    static const uint8_t ISR_BUSY = 16;            // Busy flag
    static const uint8_t ISR_ABRF = 15;            // Auto baud rate flag
    static const uint8_t ISR_ABRE = 14;            // Auto baud rate error
    static const uint8_t ISR_EOBF = 12;            // End of block flag
    static const uint8_t ISR_RTOF = 11;            // Receiver timeout
    static const uint8_t ISR_CTS = 10;             // CTS flag
    static const uint8_t ISR_CTSIF = 9;            // CTS interrupt flag
    static const uint8_t ISR_LBDF = 8;             // LIN break detection flag
    static const uint8_t ISR_TXE = 7;              // Transmit data register empty
    static const uint8_t ISR_TC = 6;               // Transmission complete
    static const uint8_t ISR_RXNE = 5;             // Read data register not empty
    static const uint8_t ISR_IDLE = 4;             // Idle line detected
    static const uint8_t ISR_ORE = 3;              // Overrun error
    static const uint8_t ISR_NF = 2;               // Noise detected flag
    static const uint8_t ISR_FE = 1;               // Framing error
    static const uint8_t ISR_PE = 0;               // Parity error
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static const uint8_t ICR_WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t ICR_CMCF = 17;            // Character match clear flag
    static const uint8_t ICR_EOBCF = 12;           // End of timeout clear flag
    static const uint8_t ICR_RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t ICR_CTSCF = 9;            // CTS clear flag
    static const uint8_t ICR_LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t ICR_TCCF = 6;             // Transmission complete clear flag
    static const uint8_t ICR_IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ICR_ORECF = 3;            // Overrun error clear flag
    static const uint8_t ICR_NCF = 2;              // Noise detected clear flag
    static const uint8_t ICR_FECF = 1;             // Framing error clear flag
    static const uint8_t ICR_PECF = 0;             // Parity error clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static const uint8_t RDR_RDR = 0;              // Receive data value (9 bits)
    static const uint32_t RDR_RESET_VALUE = 0x0;

    static const uint8_t TDR_TDR = 0;              // Transmit data value (9 bits)
    static const uint32_t TDR_RESET_VALUE = 0x0;
};

static usart2_t& USART2 = *reinterpret_cast<usart2_t*>(0x40004400);


////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

struct usart3_t
{
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    CR3;                  // [Read-write] Control register 3
    volatile uint32_t    BRR;                  // [Read-write] Baud rate register
    volatile uint32_t    GTPR;                 // [Read-write] Guard time and prescaler register
    volatile uint32_t    RTOR;                 // [Read-write] Receiver timeout register
    volatile uint32_t    RQR;                  // [Read-write] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Read-write] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register

    static const uint8_t CR1_UE = 0;               // USART enable
    static const uint8_t CR1_UESM = 1;             // USART enable in Stop mode
    static const uint8_t CR1_RE = 2;               // Receiver enable
    static const uint8_t CR1_TE = 3;               // Transmitter enable
    static const uint8_t CR1_IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t CR1_RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t CR1_TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t CR1_TXEIE = 7;            // interrupt enable
    static const uint8_t CR1_PEIE = 8;             // PE interrupt enable
    static const uint8_t CR1_PS = 9;               // Parity selection
    static const uint8_t CR1_PCE = 10;             // Parity control enable
    static const uint8_t CR1_WAKE = 11;            // Receiver wakeup method
    static const uint8_t CR1_M = 12;               // Word length
    static const uint8_t CR1_MME = 13;             // Mute mode enable
    static const uint8_t CR1_CMIE = 14;            // Character match interrupt enable
    static const uint8_t CR1_OVER8 = 15;           // Oversampling mode
    static const uint8_t CR1_DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t CR1_DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t CR1_RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t CR1_EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t CR1_M1 = 28;              // Word length
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t CR2_ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t CR2_RTOEN = 23;           // Receiver timeout enable
    static const uint8_t CR2_ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t CR2_ABREN = 20;           // Auto baud rate enable
    static const uint8_t CR2_MSBFIRST = 19;        // Most significant bit first
    static const uint8_t CR2_DATAINV = 18;         // Binary data inversion
    static const uint8_t CR2_TXINV = 17;           // TX pin active level inversion
    static const uint8_t CR2_RXINV = 16;           // RX pin active level inversion
    static const uint8_t CR2_SWAP = 15;            // Swap TX/RX pins
    static const uint8_t CR2_LINEN = 14;           // LIN mode enable
    static const uint8_t CR2_STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CR2_CLKEN = 11;           // Clock enable
    static const uint8_t CR2_CPOL = 10;            // Clock polarity
    static const uint8_t CR2_CPHA = 9;             // Clock phase
    static const uint8_t CR2_LBCL = 8;             // Last bit clock pulse
    static const uint8_t CR2_LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t CR2_LBDL = 5;             // LIN break detection length
    static const uint8_t CR2_ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t CR3_WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t CR3_WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t CR3_SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t CR3_DEP = 15;             // Driver enable polarity selection
    static const uint8_t CR3_DEM = 14;             // Driver enable mode
    static const uint8_t CR3_DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t CR3_OVRDIS = 12;          // Overrun Disable
    static const uint8_t CR3_ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CR3_CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CR3_CTSE = 9;             // CTS enable
    static const uint8_t CR3_RTSE = 8;             // RTS enable
    static const uint8_t CR3_DMAT = 7;             // DMA enable transmitter
    static const uint8_t CR3_DMAR = 6;             // DMA enable receiver
    static const uint8_t CR3_SCEN = 5;             // Smartcard mode enable
    static const uint8_t CR3_NACK = 4;             // Smartcard NACK enable
    static const uint8_t CR3_HDSEL = 3;            // Half-duplex selection
    static const uint8_t CR3_IRLP = 2;             // IrDA low-power
    static const uint8_t CR3_IREN = 1;             // IrDA mode enable
    static const uint8_t CR3_EIE = 0;              // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    static const uint8_t BRR_DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t BRR_DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static const uint8_t GTPR_GT = 8;               // Guard time value (8 bits)
    static const uint8_t GTPR_PSC = 0;              // Prescaler value (8 bits)
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    static const uint8_t RTOR_BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTOR_RTO = 0;              // Receiver timeout value (24 bits)
    static const uint32_t RTOR_RESET_VALUE = 0x0;

    static const uint8_t RQR_TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RQR_RXFRQ = 3;            // Receive data flush request
    static const uint8_t RQR_MMRQ = 2;             // Mute mode request
    static const uint8_t RQR_SBKRQ = 1;            // Send break request
    static const uint8_t RQR_ABRRQ = 0;            // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static const uint8_t ISR_REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t ISR_TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t ISR_WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t ISR_RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t ISR_SBKF = 18;            // Send break flag
    static const uint8_t ISR_CMF = 17;             // character match flag
    static const uint8_t ISR_BUSY = 16;            // Busy flag
    static const uint8_t ISR_ABRF = 15;            // Auto baud rate flag
    static const uint8_t ISR_ABRE = 14;            // Auto baud rate error
    static const uint8_t ISR_EOBF = 12;            // End of block flag
    static const uint8_t ISR_RTOF = 11;            // Receiver timeout
    static const uint8_t ISR_CTS = 10;             // CTS flag
    static const uint8_t ISR_CTSIF = 9;            // CTS interrupt flag
    static const uint8_t ISR_LBDF = 8;             // LIN break detection flag
    static const uint8_t ISR_TXE = 7;              // Transmit data register empty
    static const uint8_t ISR_TC = 6;               // Transmission complete
    static const uint8_t ISR_RXNE = 5;             // Read data register not empty
    static const uint8_t ISR_IDLE = 4;             // Idle line detected
    static const uint8_t ISR_ORE = 3;              // Overrun error
    static const uint8_t ISR_NF = 2;               // Noise detected flag
    static const uint8_t ISR_FE = 1;               // Framing error
    static const uint8_t ISR_PE = 0;               // Parity error
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static const uint8_t ICR_WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t ICR_CMCF = 17;            // Character match clear flag
    static const uint8_t ICR_EOBCF = 12;           // End of timeout clear flag
    static const uint8_t ICR_RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t ICR_CTSCF = 9;            // CTS clear flag
    static const uint8_t ICR_LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t ICR_TCCF = 6;             // Transmission complete clear flag
    static const uint8_t ICR_IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ICR_ORECF = 3;            // Overrun error clear flag
    static const uint8_t ICR_NCF = 2;              // Noise detected clear flag
    static const uint8_t ICR_FECF = 1;             // Framing error clear flag
    static const uint8_t ICR_PECF = 0;             // Parity error clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static const uint8_t RDR_RDR = 0;              // Receive data value (9 bits)
    static const uint32_t RDR_RESET_VALUE = 0x0;

    static const uint8_t TDR_TDR = 0;              // Transmit data value (9 bits)
    static const uint32_t TDR_RESET_VALUE = 0x0;
};

static usart3_t& USART3 = *reinterpret_cast<usart3_t*>(0x40004800);


////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

struct usart4_t
{
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    CR3;                  // [Read-write] Control register 3
    volatile uint32_t    BRR;                  // [Read-write] Baud rate register
    volatile uint32_t    GTPR;                 // [Read-write] Guard time and prescaler register
    volatile uint32_t    RTOR;                 // [Read-write] Receiver timeout register
    volatile uint32_t    RQR;                  // [Read-write] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Read-write] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register

    static const uint8_t CR1_UE = 0;               // USART enable
    static const uint8_t CR1_UESM = 1;             // USART enable in Stop mode
    static const uint8_t CR1_RE = 2;               // Receiver enable
    static const uint8_t CR1_TE = 3;               // Transmitter enable
    static const uint8_t CR1_IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t CR1_RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t CR1_TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t CR1_TXEIE = 7;            // interrupt enable
    static const uint8_t CR1_PEIE = 8;             // PE interrupt enable
    static const uint8_t CR1_PS = 9;               // Parity selection
    static const uint8_t CR1_PCE = 10;             // Parity control enable
    static const uint8_t CR1_WAKE = 11;            // Receiver wakeup method
    static const uint8_t CR1_M = 12;               // Word length
    static const uint8_t CR1_MME = 13;             // Mute mode enable
    static const uint8_t CR1_CMIE = 14;            // Character match interrupt enable
    static const uint8_t CR1_OVER8 = 15;           // Oversampling mode
    static const uint8_t CR1_DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t CR1_DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t CR1_RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t CR1_EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t CR1_M1 = 28;              // Word length
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t CR2_ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t CR2_RTOEN = 23;           // Receiver timeout enable
    static const uint8_t CR2_ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t CR2_ABREN = 20;           // Auto baud rate enable
    static const uint8_t CR2_MSBFIRST = 19;        // Most significant bit first
    static const uint8_t CR2_DATAINV = 18;         // Binary data inversion
    static const uint8_t CR2_TXINV = 17;           // TX pin active level inversion
    static const uint8_t CR2_RXINV = 16;           // RX pin active level inversion
    static const uint8_t CR2_SWAP = 15;            // Swap TX/RX pins
    static const uint8_t CR2_LINEN = 14;           // LIN mode enable
    static const uint8_t CR2_STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CR2_CLKEN = 11;           // Clock enable
    static const uint8_t CR2_CPOL = 10;            // Clock polarity
    static const uint8_t CR2_CPHA = 9;             // Clock phase
    static const uint8_t CR2_LBCL = 8;             // Last bit clock pulse
    static const uint8_t CR2_LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t CR2_LBDL = 5;             // LIN break detection length
    static const uint8_t CR2_ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t CR3_WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t CR3_WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t CR3_SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t CR3_DEP = 15;             // Driver enable polarity selection
    static const uint8_t CR3_DEM = 14;             // Driver enable mode
    static const uint8_t CR3_DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t CR3_OVRDIS = 12;          // Overrun Disable
    static const uint8_t CR3_ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CR3_CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CR3_CTSE = 9;             // CTS enable
    static const uint8_t CR3_RTSE = 8;             // RTS enable
    static const uint8_t CR3_DMAT = 7;             // DMA enable transmitter
    static const uint8_t CR3_DMAR = 6;             // DMA enable receiver
    static const uint8_t CR3_SCEN = 5;             // Smartcard mode enable
    static const uint8_t CR3_NACK = 4;             // Smartcard NACK enable
    static const uint8_t CR3_HDSEL = 3;            // Half-duplex selection
    static const uint8_t CR3_IRLP = 2;             // IrDA low-power
    static const uint8_t CR3_IREN = 1;             // IrDA mode enable
    static const uint8_t CR3_EIE = 0;              // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    static const uint8_t BRR_DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t BRR_DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static const uint8_t GTPR_GT = 8;               // Guard time value (8 bits)
    static const uint8_t GTPR_PSC = 0;              // Prescaler value (8 bits)
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    static const uint8_t RTOR_BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTOR_RTO = 0;              // Receiver timeout value (24 bits)
    static const uint32_t RTOR_RESET_VALUE = 0x0;

    static const uint8_t RQR_TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RQR_RXFRQ = 3;            // Receive data flush request
    static const uint8_t RQR_MMRQ = 2;             // Mute mode request
    static const uint8_t RQR_SBKRQ = 1;            // Send break request
    static const uint8_t RQR_ABRRQ = 0;            // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static const uint8_t ISR_REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t ISR_TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t ISR_WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t ISR_RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t ISR_SBKF = 18;            // Send break flag
    static const uint8_t ISR_CMF = 17;             // character match flag
    static const uint8_t ISR_BUSY = 16;            // Busy flag
    static const uint8_t ISR_ABRF = 15;            // Auto baud rate flag
    static const uint8_t ISR_ABRE = 14;            // Auto baud rate error
    static const uint8_t ISR_EOBF = 12;            // End of block flag
    static const uint8_t ISR_RTOF = 11;            // Receiver timeout
    static const uint8_t ISR_CTS = 10;             // CTS flag
    static const uint8_t ISR_CTSIF = 9;            // CTS interrupt flag
    static const uint8_t ISR_LBDF = 8;             // LIN break detection flag
    static const uint8_t ISR_TXE = 7;              // Transmit data register empty
    static const uint8_t ISR_TC = 6;               // Transmission complete
    static const uint8_t ISR_RXNE = 5;             // Read data register not empty
    static const uint8_t ISR_IDLE = 4;             // Idle line detected
    static const uint8_t ISR_ORE = 3;              // Overrun error
    static const uint8_t ISR_NF = 2;               // Noise detected flag
    static const uint8_t ISR_FE = 1;               // Framing error
    static const uint8_t ISR_PE = 0;               // Parity error
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static const uint8_t ICR_WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t ICR_CMCF = 17;            // Character match clear flag
    static const uint8_t ICR_EOBCF = 12;           // End of timeout clear flag
    static const uint8_t ICR_RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t ICR_CTSCF = 9;            // CTS clear flag
    static const uint8_t ICR_LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t ICR_TCCF = 6;             // Transmission complete clear flag
    static const uint8_t ICR_IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ICR_ORECF = 3;            // Overrun error clear flag
    static const uint8_t ICR_NCF = 2;              // Noise detected clear flag
    static const uint8_t ICR_FECF = 1;             // Framing error clear flag
    static const uint8_t ICR_PECF = 0;             // Parity error clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static const uint8_t RDR_RDR = 0;              // Receive data value (9 bits)
    static const uint32_t RDR_RESET_VALUE = 0x0;

    static const uint8_t TDR_TDR = 0;              // Transmit data value (9 bits)
    static const uint32_t TDR_RESET_VALUE = 0x0;
};

static usart4_t& USART4 = *reinterpret_cast<usart4_t*>(0x40004c00);


////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

struct usart6_t
{
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    CR3;                  // [Read-write] Control register 3
    volatile uint32_t    BRR;                  // [Read-write] Baud rate register
    volatile uint32_t    GTPR;                 // [Read-write] Guard time and prescaler register
    volatile uint32_t    RTOR;                 // [Read-write] Receiver timeout register
    volatile uint32_t    RQR;                  // [Read-write] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Read-write] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register

    static const uint8_t CR1_UE = 0;               // USART enable
    static const uint8_t CR1_UESM = 1;             // USART enable in Stop mode
    static const uint8_t CR1_RE = 2;               // Receiver enable
    static const uint8_t CR1_TE = 3;               // Transmitter enable
    static const uint8_t CR1_IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t CR1_RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t CR1_TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t CR1_TXEIE = 7;            // interrupt enable
    static const uint8_t CR1_PEIE = 8;             // PE interrupt enable
    static const uint8_t CR1_PS = 9;               // Parity selection
    static const uint8_t CR1_PCE = 10;             // Parity control enable
    static const uint8_t CR1_WAKE = 11;            // Receiver wakeup method
    static const uint8_t CR1_M = 12;               // Word length
    static const uint8_t CR1_MME = 13;             // Mute mode enable
    static const uint8_t CR1_CMIE = 14;            // Character match interrupt enable
    static const uint8_t CR1_OVER8 = 15;           // Oversampling mode
    static const uint8_t CR1_DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t CR1_DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t CR1_RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t CR1_EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t CR1_M1 = 28;              // Word length
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t CR2_ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t CR2_RTOEN = 23;           // Receiver timeout enable
    static const uint8_t CR2_ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t CR2_ABREN = 20;           // Auto baud rate enable
    static const uint8_t CR2_MSBFIRST = 19;        // Most significant bit first
    static const uint8_t CR2_DATAINV = 18;         // Binary data inversion
    static const uint8_t CR2_TXINV = 17;           // TX pin active level inversion
    static const uint8_t CR2_RXINV = 16;           // RX pin active level inversion
    static const uint8_t CR2_SWAP = 15;            // Swap TX/RX pins
    static const uint8_t CR2_LINEN = 14;           // LIN mode enable
    static const uint8_t CR2_STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CR2_CLKEN = 11;           // Clock enable
    static const uint8_t CR2_CPOL = 10;            // Clock polarity
    static const uint8_t CR2_CPHA = 9;             // Clock phase
    static const uint8_t CR2_LBCL = 8;             // Last bit clock pulse
    static const uint8_t CR2_LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t CR2_LBDL = 5;             // LIN break detection length
    static const uint8_t CR2_ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t CR3_WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t CR3_WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t CR3_SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t CR3_DEP = 15;             // Driver enable polarity selection
    static const uint8_t CR3_DEM = 14;             // Driver enable mode
    static const uint8_t CR3_DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t CR3_OVRDIS = 12;          // Overrun Disable
    static const uint8_t CR3_ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CR3_CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CR3_CTSE = 9;             // CTS enable
    static const uint8_t CR3_RTSE = 8;             // RTS enable
    static const uint8_t CR3_DMAT = 7;             // DMA enable transmitter
    static const uint8_t CR3_DMAR = 6;             // DMA enable receiver
    static const uint8_t CR3_SCEN = 5;             // Smartcard mode enable
    static const uint8_t CR3_NACK = 4;             // Smartcard NACK enable
    static const uint8_t CR3_HDSEL = 3;            // Half-duplex selection
    static const uint8_t CR3_IRLP = 2;             // IrDA low-power
    static const uint8_t CR3_IREN = 1;             // IrDA mode enable
    static const uint8_t CR3_EIE = 0;              // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    static const uint8_t BRR_DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t BRR_DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static const uint8_t GTPR_GT = 8;               // Guard time value (8 bits)
    static const uint8_t GTPR_PSC = 0;              // Prescaler value (8 bits)
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    static const uint8_t RTOR_BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTOR_RTO = 0;              // Receiver timeout value (24 bits)
    static const uint32_t RTOR_RESET_VALUE = 0x0;

    static const uint8_t RQR_TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RQR_RXFRQ = 3;            // Receive data flush request
    static const uint8_t RQR_MMRQ = 2;             // Mute mode request
    static const uint8_t RQR_SBKRQ = 1;            // Send break request
    static const uint8_t RQR_ABRRQ = 0;            // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static const uint8_t ISR_REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t ISR_TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t ISR_WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t ISR_RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t ISR_SBKF = 18;            // Send break flag
    static const uint8_t ISR_CMF = 17;             // character match flag
    static const uint8_t ISR_BUSY = 16;            // Busy flag
    static const uint8_t ISR_ABRF = 15;            // Auto baud rate flag
    static const uint8_t ISR_ABRE = 14;            // Auto baud rate error
    static const uint8_t ISR_EOBF = 12;            // End of block flag
    static const uint8_t ISR_RTOF = 11;            // Receiver timeout
    static const uint8_t ISR_CTS = 10;             // CTS flag
    static const uint8_t ISR_CTSIF = 9;            // CTS interrupt flag
    static const uint8_t ISR_LBDF = 8;             // LIN break detection flag
    static const uint8_t ISR_TXE = 7;              // Transmit data register empty
    static const uint8_t ISR_TC = 6;               // Transmission complete
    static const uint8_t ISR_RXNE = 5;             // Read data register not empty
    static const uint8_t ISR_IDLE = 4;             // Idle line detected
    static const uint8_t ISR_ORE = 3;              // Overrun error
    static const uint8_t ISR_NF = 2;               // Noise detected flag
    static const uint8_t ISR_FE = 1;               // Framing error
    static const uint8_t ISR_PE = 0;               // Parity error
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static const uint8_t ICR_WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t ICR_CMCF = 17;            // Character match clear flag
    static const uint8_t ICR_EOBCF = 12;           // End of timeout clear flag
    static const uint8_t ICR_RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t ICR_CTSCF = 9;            // CTS clear flag
    static const uint8_t ICR_LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t ICR_TCCF = 6;             // Transmission complete clear flag
    static const uint8_t ICR_IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ICR_ORECF = 3;            // Overrun error clear flag
    static const uint8_t ICR_NCF = 2;              // Noise detected clear flag
    static const uint8_t ICR_FECF = 1;             // Framing error clear flag
    static const uint8_t ICR_PECF = 0;             // Parity error clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static const uint8_t RDR_RDR = 0;              // Receive data value (9 bits)
    static const uint32_t RDR_RESET_VALUE = 0x0;

    static const uint8_t TDR_TDR = 0;              // Transmit data value (9 bits)
    static const uint32_t TDR_RESET_VALUE = 0x0;
};

static usart6_t& USART6 = *reinterpret_cast<usart6_t*>(0x40011400);


////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

struct usart7_t
{
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    CR3;                  // [Read-write] Control register 3
    volatile uint32_t    BRR;                  // [Read-write] Baud rate register
    volatile uint32_t    GTPR;                 // [Read-write] Guard time and prescaler register
    volatile uint32_t    RTOR;                 // [Read-write] Receiver timeout register
    volatile uint32_t    RQR;                  // [Read-write] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Read-write] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register

    static const uint8_t CR1_UE = 0;               // USART enable
    static const uint8_t CR1_UESM = 1;             // USART enable in Stop mode
    static const uint8_t CR1_RE = 2;               // Receiver enable
    static const uint8_t CR1_TE = 3;               // Transmitter enable
    static const uint8_t CR1_IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t CR1_RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t CR1_TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t CR1_TXEIE = 7;            // interrupt enable
    static const uint8_t CR1_PEIE = 8;             // PE interrupt enable
    static const uint8_t CR1_PS = 9;               // Parity selection
    static const uint8_t CR1_PCE = 10;             // Parity control enable
    static const uint8_t CR1_WAKE = 11;            // Receiver wakeup method
    static const uint8_t CR1_M = 12;               // Word length
    static const uint8_t CR1_MME = 13;             // Mute mode enable
    static const uint8_t CR1_CMIE = 14;            // Character match interrupt enable
    static const uint8_t CR1_OVER8 = 15;           // Oversampling mode
    static const uint8_t CR1_DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t CR1_DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t CR1_RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t CR1_EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t CR1_M1 = 28;              // Word length
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t CR2_ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t CR2_RTOEN = 23;           // Receiver timeout enable
    static const uint8_t CR2_ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t CR2_ABREN = 20;           // Auto baud rate enable
    static const uint8_t CR2_MSBFIRST = 19;        // Most significant bit first
    static const uint8_t CR2_DATAINV = 18;         // Binary data inversion
    static const uint8_t CR2_TXINV = 17;           // TX pin active level inversion
    static const uint8_t CR2_RXINV = 16;           // RX pin active level inversion
    static const uint8_t CR2_SWAP = 15;            // Swap TX/RX pins
    static const uint8_t CR2_LINEN = 14;           // LIN mode enable
    static const uint8_t CR2_STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CR2_CLKEN = 11;           // Clock enable
    static const uint8_t CR2_CPOL = 10;            // Clock polarity
    static const uint8_t CR2_CPHA = 9;             // Clock phase
    static const uint8_t CR2_LBCL = 8;             // Last bit clock pulse
    static const uint8_t CR2_LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t CR2_LBDL = 5;             // LIN break detection length
    static const uint8_t CR2_ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t CR3_WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t CR3_WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t CR3_SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t CR3_DEP = 15;             // Driver enable polarity selection
    static const uint8_t CR3_DEM = 14;             // Driver enable mode
    static const uint8_t CR3_DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t CR3_OVRDIS = 12;          // Overrun Disable
    static const uint8_t CR3_ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CR3_CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CR3_CTSE = 9;             // CTS enable
    static const uint8_t CR3_RTSE = 8;             // RTS enable
    static const uint8_t CR3_DMAT = 7;             // DMA enable transmitter
    static const uint8_t CR3_DMAR = 6;             // DMA enable receiver
    static const uint8_t CR3_SCEN = 5;             // Smartcard mode enable
    static const uint8_t CR3_NACK = 4;             // Smartcard NACK enable
    static const uint8_t CR3_HDSEL = 3;            // Half-duplex selection
    static const uint8_t CR3_IRLP = 2;             // IrDA low-power
    static const uint8_t CR3_IREN = 1;             // IrDA mode enable
    static const uint8_t CR3_EIE = 0;              // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    static const uint8_t BRR_DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t BRR_DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static const uint8_t GTPR_GT = 8;               // Guard time value (8 bits)
    static const uint8_t GTPR_PSC = 0;              // Prescaler value (8 bits)
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    static const uint8_t RTOR_BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTOR_RTO = 0;              // Receiver timeout value (24 bits)
    static const uint32_t RTOR_RESET_VALUE = 0x0;

    static const uint8_t RQR_TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RQR_RXFRQ = 3;            // Receive data flush request
    static const uint8_t RQR_MMRQ = 2;             // Mute mode request
    static const uint8_t RQR_SBKRQ = 1;            // Send break request
    static const uint8_t RQR_ABRRQ = 0;            // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static const uint8_t ISR_REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t ISR_TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t ISR_WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t ISR_RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t ISR_SBKF = 18;            // Send break flag
    static const uint8_t ISR_CMF = 17;             // character match flag
    static const uint8_t ISR_BUSY = 16;            // Busy flag
    static const uint8_t ISR_ABRF = 15;            // Auto baud rate flag
    static const uint8_t ISR_ABRE = 14;            // Auto baud rate error
    static const uint8_t ISR_EOBF = 12;            // End of block flag
    static const uint8_t ISR_RTOF = 11;            // Receiver timeout
    static const uint8_t ISR_CTS = 10;             // CTS flag
    static const uint8_t ISR_CTSIF = 9;            // CTS interrupt flag
    static const uint8_t ISR_LBDF = 8;             // LIN break detection flag
    static const uint8_t ISR_TXE = 7;              // Transmit data register empty
    static const uint8_t ISR_TC = 6;               // Transmission complete
    static const uint8_t ISR_RXNE = 5;             // Read data register not empty
    static const uint8_t ISR_IDLE = 4;             // Idle line detected
    static const uint8_t ISR_ORE = 3;              // Overrun error
    static const uint8_t ISR_NF = 2;               // Noise detected flag
    static const uint8_t ISR_FE = 1;               // Framing error
    static const uint8_t ISR_PE = 0;               // Parity error
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static const uint8_t ICR_WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t ICR_CMCF = 17;            // Character match clear flag
    static const uint8_t ICR_EOBCF = 12;           // End of timeout clear flag
    static const uint8_t ICR_RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t ICR_CTSCF = 9;            // CTS clear flag
    static const uint8_t ICR_LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t ICR_TCCF = 6;             // Transmission complete clear flag
    static const uint8_t ICR_IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ICR_ORECF = 3;            // Overrun error clear flag
    static const uint8_t ICR_NCF = 2;              // Noise detected clear flag
    static const uint8_t ICR_FECF = 1;             // Framing error clear flag
    static const uint8_t ICR_PECF = 0;             // Parity error clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static const uint8_t RDR_RDR = 0;              // Receive data value (9 bits)
    static const uint32_t RDR_RESET_VALUE = 0x0;

    static const uint8_t TDR_TDR = 0;              // Transmit data value (9 bits)
    static const uint32_t TDR_RESET_VALUE = 0x0;
};

static usart7_t& USART7 = *reinterpret_cast<usart7_t*>(0x40011800);


////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

struct usart8_t
{
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    CR3;                  // [Read-write] Control register 3
    volatile uint32_t    BRR;                  // [Read-write] Baud rate register
    volatile uint32_t    GTPR;                 // [Read-write] Guard time and prescaler register
    volatile uint32_t    RTOR;                 // [Read-write] Receiver timeout register
    volatile uint32_t    RQR;                  // [Read-write] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Read-write] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register

    static const uint8_t CR1_UE = 0;               // USART enable
    static const uint8_t CR1_UESM = 1;             // USART enable in Stop mode
    static const uint8_t CR1_RE = 2;               // Receiver enable
    static const uint8_t CR1_TE = 3;               // Transmitter enable
    static const uint8_t CR1_IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t CR1_RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t CR1_TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t CR1_TXEIE = 7;            // interrupt enable
    static const uint8_t CR1_PEIE = 8;             // PE interrupt enable
    static const uint8_t CR1_PS = 9;               // Parity selection
    static const uint8_t CR1_PCE = 10;             // Parity control enable
    static const uint8_t CR1_WAKE = 11;            // Receiver wakeup method
    static const uint8_t CR1_M = 12;               // Word length
    static const uint8_t CR1_MME = 13;             // Mute mode enable
    static const uint8_t CR1_CMIE = 14;            // Character match interrupt enable
    static const uint8_t CR1_OVER8 = 15;           // Oversampling mode
    static const uint8_t CR1_DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t CR1_DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t CR1_RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t CR1_EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t CR1_M1 = 28;              // Word length
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t CR2_ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t CR2_RTOEN = 23;           // Receiver timeout enable
    static const uint8_t CR2_ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t CR2_ABREN = 20;           // Auto baud rate enable
    static const uint8_t CR2_MSBFIRST = 19;        // Most significant bit first
    static const uint8_t CR2_DATAINV = 18;         // Binary data inversion
    static const uint8_t CR2_TXINV = 17;           // TX pin active level inversion
    static const uint8_t CR2_RXINV = 16;           // RX pin active level inversion
    static const uint8_t CR2_SWAP = 15;            // Swap TX/RX pins
    static const uint8_t CR2_LINEN = 14;           // LIN mode enable
    static const uint8_t CR2_STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CR2_CLKEN = 11;           // Clock enable
    static const uint8_t CR2_CPOL = 10;            // Clock polarity
    static const uint8_t CR2_CPHA = 9;             // Clock phase
    static const uint8_t CR2_LBCL = 8;             // Last bit clock pulse
    static const uint8_t CR2_LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t CR2_LBDL = 5;             // LIN break detection length
    static const uint8_t CR2_ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t CR3_WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t CR3_WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t CR3_SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t CR3_DEP = 15;             // Driver enable polarity selection
    static const uint8_t CR3_DEM = 14;             // Driver enable mode
    static const uint8_t CR3_DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t CR3_OVRDIS = 12;          // Overrun Disable
    static const uint8_t CR3_ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CR3_CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CR3_CTSE = 9;             // CTS enable
    static const uint8_t CR3_RTSE = 8;             // RTS enable
    static const uint8_t CR3_DMAT = 7;             // DMA enable transmitter
    static const uint8_t CR3_DMAR = 6;             // DMA enable receiver
    static const uint8_t CR3_SCEN = 5;             // Smartcard mode enable
    static const uint8_t CR3_NACK = 4;             // Smartcard NACK enable
    static const uint8_t CR3_HDSEL = 3;            // Half-duplex selection
    static const uint8_t CR3_IRLP = 2;             // IrDA low-power
    static const uint8_t CR3_IREN = 1;             // IrDA mode enable
    static const uint8_t CR3_EIE = 0;              // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    static const uint8_t BRR_DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t BRR_DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static const uint8_t GTPR_GT = 8;               // Guard time value (8 bits)
    static const uint8_t GTPR_PSC = 0;              // Prescaler value (8 bits)
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    static const uint8_t RTOR_BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTOR_RTO = 0;              // Receiver timeout value (24 bits)
    static const uint32_t RTOR_RESET_VALUE = 0x0;

    static const uint8_t RQR_TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RQR_RXFRQ = 3;            // Receive data flush request
    static const uint8_t RQR_MMRQ = 2;             // Mute mode request
    static const uint8_t RQR_SBKRQ = 1;            // Send break request
    static const uint8_t RQR_ABRRQ = 0;            // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static const uint8_t ISR_REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t ISR_TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t ISR_WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t ISR_RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t ISR_SBKF = 18;            // Send break flag
    static const uint8_t ISR_CMF = 17;             // character match flag
    static const uint8_t ISR_BUSY = 16;            // Busy flag
    static const uint8_t ISR_ABRF = 15;            // Auto baud rate flag
    static const uint8_t ISR_ABRE = 14;            // Auto baud rate error
    static const uint8_t ISR_EOBF = 12;            // End of block flag
    static const uint8_t ISR_RTOF = 11;            // Receiver timeout
    static const uint8_t ISR_CTS = 10;             // CTS flag
    static const uint8_t ISR_CTSIF = 9;            // CTS interrupt flag
    static const uint8_t ISR_LBDF = 8;             // LIN break detection flag
    static const uint8_t ISR_TXE = 7;              // Transmit data register empty
    static const uint8_t ISR_TC = 6;               // Transmission complete
    static const uint8_t ISR_RXNE = 5;             // Read data register not empty
    static const uint8_t ISR_IDLE = 4;             // Idle line detected
    static const uint8_t ISR_ORE = 3;              // Overrun error
    static const uint8_t ISR_NF = 2;               // Noise detected flag
    static const uint8_t ISR_FE = 1;               // Framing error
    static const uint8_t ISR_PE = 0;               // Parity error
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static const uint8_t ICR_WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t ICR_CMCF = 17;            // Character match clear flag
    static const uint8_t ICR_EOBCF = 12;           // End of timeout clear flag
    static const uint8_t ICR_RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t ICR_CTSCF = 9;            // CTS clear flag
    static const uint8_t ICR_LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t ICR_TCCF = 6;             // Transmission complete clear flag
    static const uint8_t ICR_IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ICR_ORECF = 3;            // Overrun error clear flag
    static const uint8_t ICR_NCF = 2;              // Noise detected clear flag
    static const uint8_t ICR_FECF = 1;             // Framing error clear flag
    static const uint8_t ICR_PECF = 0;             // Parity error clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static const uint8_t RDR_RDR = 0;              // Receive data value (9 bits)
    static const uint32_t RDR_RESET_VALUE = 0x0;

    static const uint8_t TDR_TDR = 0;              // Transmit data value (9 bits)
    static const uint32_t TDR_RESET_VALUE = 0x0;
};

static usart8_t& USART8 = *reinterpret_cast<usart8_t*>(0x40011c00);


////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

struct usart5_t
{
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    CR3;                  // [Read-write] Control register 3
    volatile uint32_t    BRR;                  // [Read-write] Baud rate register
    volatile uint32_t    GTPR;                 // [Read-write] Guard time and prescaler register
    volatile uint32_t    RTOR;                 // [Read-write] Receiver timeout register
    volatile uint32_t    RQR;                  // [Read-write] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Read-write] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register

    static const uint8_t CR1_UE = 0;               // USART enable
    static const uint8_t CR1_UESM = 1;             // USART enable in Stop mode
    static const uint8_t CR1_RE = 2;               // Receiver enable
    static const uint8_t CR1_TE = 3;               // Transmitter enable
    static const uint8_t CR1_IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t CR1_RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t CR1_TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t CR1_TXEIE = 7;            // interrupt enable
    static const uint8_t CR1_PEIE = 8;             // PE interrupt enable
    static const uint8_t CR1_PS = 9;               // Parity selection
    static const uint8_t CR1_PCE = 10;             // Parity control enable
    static const uint8_t CR1_WAKE = 11;            // Receiver wakeup method
    static const uint8_t CR1_M = 12;               // Word length
    static const uint8_t CR1_MME = 13;             // Mute mode enable
    static const uint8_t CR1_CMIE = 14;            // Character match interrupt enable
    static const uint8_t CR1_OVER8 = 15;           // Oversampling mode
    static const uint8_t CR1_DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t CR1_DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t CR1_RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t CR1_EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t CR1_M1 = 28;              // Word length
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t CR2_ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t CR2_RTOEN = 23;           // Receiver timeout enable
    static const uint8_t CR2_ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t CR2_ABREN = 20;           // Auto baud rate enable
    static const uint8_t CR2_MSBFIRST = 19;        // Most significant bit first
    static const uint8_t CR2_DATAINV = 18;         // Binary data inversion
    static const uint8_t CR2_TXINV = 17;           // TX pin active level inversion
    static const uint8_t CR2_RXINV = 16;           // RX pin active level inversion
    static const uint8_t CR2_SWAP = 15;            // Swap TX/RX pins
    static const uint8_t CR2_LINEN = 14;           // LIN mode enable
    static const uint8_t CR2_STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CR2_CLKEN = 11;           // Clock enable
    static const uint8_t CR2_CPOL = 10;            // Clock polarity
    static const uint8_t CR2_CPHA = 9;             // Clock phase
    static const uint8_t CR2_LBCL = 8;             // Last bit clock pulse
    static const uint8_t CR2_LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t CR2_LBDL = 5;             // LIN break detection length
    static const uint8_t CR2_ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t CR3_WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t CR3_WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t CR3_SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t CR3_DEP = 15;             // Driver enable polarity selection
    static const uint8_t CR3_DEM = 14;             // Driver enable mode
    static const uint8_t CR3_DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t CR3_OVRDIS = 12;          // Overrun Disable
    static const uint8_t CR3_ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CR3_CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CR3_CTSE = 9;             // CTS enable
    static const uint8_t CR3_RTSE = 8;             // RTS enable
    static const uint8_t CR3_DMAT = 7;             // DMA enable transmitter
    static const uint8_t CR3_DMAR = 6;             // DMA enable receiver
    static const uint8_t CR3_SCEN = 5;             // Smartcard mode enable
    static const uint8_t CR3_NACK = 4;             // Smartcard NACK enable
    static const uint8_t CR3_HDSEL = 3;            // Half-duplex selection
    static const uint8_t CR3_IRLP = 2;             // IrDA low-power
    static const uint8_t CR3_IREN = 1;             // IrDA mode enable
    static const uint8_t CR3_EIE = 0;              // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    static const uint8_t BRR_DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t BRR_DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static const uint8_t GTPR_GT = 8;               // Guard time value (8 bits)
    static const uint8_t GTPR_PSC = 0;              // Prescaler value (8 bits)
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    static const uint8_t RTOR_BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTOR_RTO = 0;              // Receiver timeout value (24 bits)
    static const uint32_t RTOR_RESET_VALUE = 0x0;

    static const uint8_t RQR_TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RQR_RXFRQ = 3;            // Receive data flush request
    static const uint8_t RQR_MMRQ = 2;             // Mute mode request
    static const uint8_t RQR_SBKRQ = 1;            // Send break request
    static const uint8_t RQR_ABRRQ = 0;            // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static const uint8_t ISR_REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t ISR_TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t ISR_WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t ISR_RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t ISR_SBKF = 18;            // Send break flag
    static const uint8_t ISR_CMF = 17;             // character match flag
    static const uint8_t ISR_BUSY = 16;            // Busy flag
    static const uint8_t ISR_ABRF = 15;            // Auto baud rate flag
    static const uint8_t ISR_ABRE = 14;            // Auto baud rate error
    static const uint8_t ISR_EOBF = 12;            // End of block flag
    static const uint8_t ISR_RTOF = 11;            // Receiver timeout
    static const uint8_t ISR_CTS = 10;             // CTS flag
    static const uint8_t ISR_CTSIF = 9;            // CTS interrupt flag
    static const uint8_t ISR_LBDF = 8;             // LIN break detection flag
    static const uint8_t ISR_TXE = 7;              // Transmit data register empty
    static const uint8_t ISR_TC = 6;               // Transmission complete
    static const uint8_t ISR_RXNE = 5;             // Read data register not empty
    static const uint8_t ISR_IDLE = 4;             // Idle line detected
    static const uint8_t ISR_ORE = 3;              // Overrun error
    static const uint8_t ISR_NF = 2;               // Noise detected flag
    static const uint8_t ISR_FE = 1;               // Framing error
    static const uint8_t ISR_PE = 0;               // Parity error
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static const uint8_t ICR_WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t ICR_CMCF = 17;            // Character match clear flag
    static const uint8_t ICR_EOBCF = 12;           // End of timeout clear flag
    static const uint8_t ICR_RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t ICR_CTSCF = 9;            // CTS clear flag
    static const uint8_t ICR_LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t ICR_TCCF = 6;             // Transmission complete clear flag
    static const uint8_t ICR_IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ICR_ORECF = 3;            // Overrun error clear flag
    static const uint8_t ICR_NCF = 2;              // Noise detected clear flag
    static const uint8_t ICR_FECF = 1;             // Framing error clear flag
    static const uint8_t ICR_PECF = 0;             // Parity error clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static const uint8_t RDR_RDR = 0;              // Receive data value (9 bits)
    static const uint32_t RDR_RESET_VALUE = 0x0;

    static const uint8_t TDR_TDR = 0;              // Transmit data value (9 bits)
    static const uint32_t TDR_RESET_VALUE = 0x0;
};

static usart5_t& USART5 = *reinterpret_cast<usart5_t*>(0x40005000);


////
//
//    Real-time clock
//
////

struct rtc_t
{
    volatile uint32_t    TR;                   // [Read-write] time register
    volatile uint32_t    DR;                   // [Read-write] date register
    volatile uint32_t    CR;                   // control register
    volatile uint32_t    ISR;                  // initialization and status register
    volatile uint32_t    PRER;                 // [Read-write] prescaler register
    reserved_t<2>        _0;
    volatile uint32_t    ALRMAR;               // [Read-write] alarm A register
    reserved_t<1>        _1;
    volatile uint32_t    WPR;                  // [Write-only] write protection register
    volatile uint32_t    SSR;                  // [Read-only] sub second register
    volatile uint32_t    SHIFTR;               // [Write-only] shift control register
    volatile uint32_t    TSTR;                 // [Read-only] timestamp time register
    volatile uint32_t    TSDR;                 // [Read-only] timestamp date register
    volatile uint32_t    TSSSR;                // [Read-only] time-stamp sub second register
    volatile uint32_t    CALR;                 // [Read-write] calibration register
    volatile uint32_t    TAFCR;                // [Read-write] tamper and alternate function configuration register
    volatile uint32_t    ALRMASSR;             // [Read-write] alarm A sub second register
    reserved_t<2>        _2;
    volatile uint32_t    BKP0R;                // [Read-write] backup register
    volatile uint32_t    BKP1R;                // [Read-write] backup register
    volatile uint32_t    BKP2R;                // [Read-write] backup register
    volatile uint32_t    BKP3R;                // [Read-write] backup register
    volatile uint32_t    BKP4R;                // [Read-write] backup register

    static const uint8_t TR_PM = 22;              // AM/PM notation
    static const uint8_t TR_HT = 20;              // Hour tens in BCD format (2 bits)
    static const uint8_t TR_HU = 16;              // Hour units in BCD format (4 bits)
    static const uint8_t TR_MNT = 12;             // Minute tens in BCD format (3 bits)
    static const uint8_t TR_MNU = 8;              // Minute units in BCD format (4 bits)
    static const uint8_t TR_ST = 4;               // Second tens in BCD format (3 bits)
    static const uint8_t TR_SU = 0;               // Second units in BCD format (4 bits)
    static const uint32_t TR_RESET_VALUE = 0x0;

    static const uint8_t DR_YT = 20;              // Year tens in BCD format (4 bits)
    static const uint8_t DR_YU = 16;              // Year units in BCD format (4 bits)
    static const uint8_t DR_WDU = 13;             // Week day units (3 bits)
    static const uint8_t DR_MT = 12;              // Month tens in BCD format
    static const uint8_t DR_MU = 8;               // Month units in BCD format (4 bits)
    static const uint8_t DR_DT = 4;               // Date tens in BCD format (2 bits)
    static const uint8_t DR_DU = 0;               // Date units in BCD format (4 bits)
    static const uint32_t DR_RESET_VALUE = 0x2101;

    static const uint8_t CR_TSEDGE = 3;           // Time-stamp event active edge, Read-write
    static const uint8_t CR_REFCKON = 4;          // RTC_REFIN reference clock detection enable (50 or 60 Hz), Read-write
    static const uint8_t CR_BYPSHAD = 5;          // Bypass the shadow registers, Read-write
    static const uint8_t CR_FMT = 6;              // Hour format, Read-write
    static const uint8_t CR_ALRAE = 8;            // Alarm A enable, Read-write
    static const uint8_t CR_TSE = 11;             // timestamp enable, Read-write
    static const uint8_t CR_ALRAIE = 12;          // Alarm A interrupt enable, Read-write
    static const uint8_t CR_TSIE = 15;            // Time-stamp interrupt enable, Read-write
    static const uint8_t CR_ADD1H = 16;           // Add 1 hour (summer time change), Write-only
    static const uint8_t CR_SUB1H = 17;           // Subtract 1 hour (winter time change), Write-only
    static const uint8_t CR_BKP = 18;             // Backup, Read-write
    static const uint8_t CR_COSEL = 19;           // Calibration output selection, Read-write
    static const uint8_t CR_POL = 20;             // Output polarity, Read-write
    static const uint8_t CR_OSEL = 21;            // Output selection (2 bits), Read-write
    static const uint8_t CR_COE = 23;             // Calibration output enable, Read-write
    static const uint32_t CR_RESET_VALUE = 0x0;

    static const uint8_t ISR_ALRAWF = 0;           // Alarm A write flag, Read-only
    static const uint8_t ISR_SHPF = 3;             // Shift operation pending, Read-write
    static const uint8_t ISR_INITS = 4;            // Initialization status flag, Read-only
    static const uint8_t ISR_RSF = 5;              // Registers synchronization flag, Read-write
    static const uint8_t ISR_INITF = 6;            // Initialization flag, Read-only
    static const uint8_t ISR_INIT = 7;             // Initialization mode, Read-write
    static const uint8_t ISR_ALRAF = 8;            // Alarm A flag, Read-write
    static const uint8_t ISR_TSF = 11;             // Time-stamp flag, Read-write
    static const uint8_t ISR_TSOVF = 12;           // Time-stamp overflow flag, Read-write
    static const uint8_t ISR_TAMP1F = 13;          // RTC_TAMP1 detection flag, Read-write
    static const uint8_t ISR_TAMP2F = 14;          // RTC_TAMP2 detection flag, Read-write
    static const uint8_t ISR_RECALPF = 16;         // Recalibration pending Flag, Read-only
    static const uint32_t ISR_RESET_VALUE = 0x7;

    static const uint8_t PRER_PREDIV_A = 16;        // Asynchronous prescaler factor (7 bits)
    static const uint8_t PRER_PREDIV_S = 0;         // Synchronous prescaler factor (15 bits)
    static const uint32_t PRER_RESET_VALUE = 0x7f00ff;

    static const uint8_t ALRMAR_MSK4 = 31;            // Alarm A date mask
    static const uint8_t ALRMAR_WDSEL = 30;           // Week day selection
    static const uint8_t ALRMAR_DT = 28;              // Date tens in BCD format. (2 bits)
    static const uint8_t ALRMAR_DU = 24;              // Date units or day in BCD format. (4 bits)
    static const uint8_t ALRMAR_MSK3 = 23;            // Alarm A hours mask
    static const uint8_t ALRMAR_PM = 22;              // AM/PM notation
    static const uint8_t ALRMAR_HT = 20;              // Hour tens in BCD format. (2 bits)
    static const uint8_t ALRMAR_HU = 16;              // Hour units in BCD format. (4 bits)
    static const uint8_t ALRMAR_MSK2 = 15;            // Alarm A minutes mask
    static const uint8_t ALRMAR_MNT = 12;             // Minute tens in BCD format. (3 bits)
    static const uint8_t ALRMAR_MNU = 8;              // Minute units in BCD format. (4 bits)
    static const uint8_t ALRMAR_MSK1 = 7;             // Alarm A seconds mask
    static const uint8_t ALRMAR_ST = 4;               // Second tens in BCD format. (3 bits)
    static const uint8_t ALRMAR_SU = 0;               // Second units in BCD format. (4 bits)
    static const uint32_t ALRMAR_RESET_VALUE = 0x0;

    static const uint8_t WPR_KEY = 0;              // Write protection key (8 bits)
    static const uint32_t WPR_RESET_VALUE = 0x0;

    static const uint8_t SSR_SS = 0;               // Sub second value (16 bits)
    static const uint32_t SSR_RESET_VALUE = 0x0;

    static const uint8_t SHIFTR_ADD1S = 31;           // Add one second
    static const uint8_t SHIFTR_SUBFS = 0;            // Subtract a fraction of a second (15 bits)
    static const uint32_t SHIFTR_RESET_VALUE = 0x0;

    static const uint8_t TSTR_PM = 22;              // AM/PM notation
    static const uint8_t TSTR_HT = 20;              // Hour tens in BCD format. (2 bits)
    static const uint8_t TSTR_HU = 16;              // Hour units in BCD format. (4 bits)
    static const uint8_t TSTR_MNT = 12;             // Minute tens in BCD format. (3 bits)
    static const uint8_t TSTR_MNU = 8;              // Minute units in BCD format. (4 bits)
    static const uint8_t TSTR_ST = 4;               // Second tens in BCD format. (3 bits)
    static const uint8_t TSTR_SU = 0;               // Second units in BCD format. (4 bits)
    static const uint32_t TSTR_RESET_VALUE = 0x0;

    static const uint8_t TSDR_WDU = 13;             // Week day units (3 bits)
    static const uint8_t TSDR_MT = 12;              // Month tens in BCD format
    static const uint8_t TSDR_MU = 8;               // Month units in BCD format (4 bits)
    static const uint8_t TSDR_DT = 4;               // Date tens in BCD format (2 bits)
    static const uint8_t TSDR_DU = 0;               // Date units in BCD format (4 bits)
    static const uint32_t TSDR_RESET_VALUE = 0x0;

    static const uint8_t TSSSR_SS = 0;               // Sub second value (16 bits)
    static const uint32_t TSSSR_RESET_VALUE = 0x0;

    static const uint8_t CALR_CALP = 15;            // Increase frequency of RTC by 488.5 ppm
    static const uint8_t CALR_CALW8 = 14;           // Use an 8-second calibration cycle period
    static const uint8_t CALR_CALW16 = 13;          // Use a 16-second calibration cycle period
    static const uint8_t CALR_CALM = 0;             // Calibration minus (9 bits)
    static const uint32_t CALR_RESET_VALUE = 0x0;

    static const uint8_t TAFCR_PC15MODE = 23;        // PC15 mode
    static const uint8_t TAFCR_PC15VALUE = 22;       // PC15 value
    static const uint8_t TAFCR_PC14MODE = 21;        // PC14 mode
    static const uint8_t TAFCR_PC14VALUE = 20;       // PC14 value
    static const uint8_t TAFCR_PC13MODE = 19;        // PC13 mode
    static const uint8_t TAFCR_PC13VALUE = 18;       // RTC_ALARM output type/PC13 value
    static const uint8_t TAFCR_TAMP_PUDIS = 15;      // RTC_TAMPx pull-up disable
    static const uint8_t TAFCR_TAMP_PRCH = 13;       // RTC_TAMPx precharge duration (2 bits)
    static const uint8_t TAFCR_TAMPFLT = 11;         // RTC_TAMPx filter count (2 bits)
    static const uint8_t TAFCR_TAMPFREQ = 8;         // Tamper sampling frequency (3 bits)
    static const uint8_t TAFCR_TAMPTS = 7;           // Activate timestamp on tamper detection event
    static const uint8_t TAFCR_TAMP2_TRG = 4;        // Active level for RTC_TAMP2 input
    static const uint8_t TAFCR_TAMP2E = 3;           // RTC_TAMP2 input detection enable
    static const uint8_t TAFCR_TAMPIE = 2;           // Tamper interrupt enable
    static const uint8_t TAFCR_TAMP1TRG = 1;         // Active level for RTC_TAMP1 input
    static const uint8_t TAFCR_TAMP1E = 0;           // RTC_TAMP1 input detection enable
    static const uint32_t TAFCR_RESET_VALUE = 0x0;

    static const uint8_t ALRMASSR_MASKSS = 24;          // Mask the most-significant bits starting at this bit (4 bits)
    static const uint8_t ALRMASSR_SS = 0;               // Sub seconds value (15 bits)
    static const uint32_t ALRMASSR_RESET_VALUE = 0x0;

    static const uint8_t BKP0R_BKP = 0;              // BKP (32 bits)
    static const uint32_t BKP0R_RESET_VALUE = 0x0;

    static const uint8_t BKP1R_BKP = 0;              // BKP (32 bits)
    static const uint32_t BKP1R_RESET_VALUE = 0x0;

    static const uint8_t BKP2R_BKP = 0;              // BKP (32 bits)
    static const uint32_t BKP2R_RESET_VALUE = 0x0;

    static const uint8_t BKP3R_BKP = 0;              // BKP (32 bits)
    static const uint32_t BKP3R_RESET_VALUE = 0x0;

    static const uint8_t BKP4R_BKP = 0;              // BKP (32 bits)
    static const uint32_t BKP4R_RESET_VALUE = 0x0;
};

static rtc_t& RTC = *reinterpret_cast<rtc_t*>(0x40002800);


////
//
//    General-purpose-timers
//
////

struct tim15_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMCR;                 // [Read-write] slave mode control register
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1;                // [Read-write] capture/compare mode register (output mode)
    reserved_t<1>        _0;
    volatile uint32_t    CCER;                 // [Read-write] capture/compare enable register
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    volatile uint32_t    RCR;                  // [Read-write] repetition counter register
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    volatile uint32_t    CCR2;                 // [Read-write] capture/compare register 2
    reserved_t<2>        _1;
    volatile uint32_t    BDTR;                 // [Read-write] break and dead-time register
    volatile uint32_t    DCR;                  // [Read-write] DMA control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static const uint8_t CR1_CKD = 8;              // Clock division (2 bits)
    static const uint8_t CR1_ARPE = 7;             // Auto-reload preload enable
    static const uint8_t CR1_OPM = 3;              // One-pulse mode
    static const uint8_t CR1_URS = 2;              // Update request source
    static const uint8_t CR1_UDIS = 1;             // Update disable
    static const uint8_t CR1_CEN = 0;              // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_OIS2 = 10;            // Output Idle state 2
    static const uint8_t CR2_OIS1N = 9;            // Output Idle state 1
    static const uint8_t CR2_OIS1 = 8;             // Output Idle state 1
    static const uint8_t CR2_MMS = 4;              // Master mode selection (3 bits)
    static const uint8_t CR2_CCDS = 3;             // Capture/compare DMA selection
    static const uint8_t CR2_CCUS = 2;             // Capture/compare control update selection
    static const uint8_t CR2_CCPC = 0;             // Capture/compare preloaded control
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t SMCR_MSM = 7;              // Master/Slave mode
    static const uint8_t SMCR_TS = 4;               // Trigger selection (3 bits)
    static const uint8_t SMCR_SMS = 0;              // Slave mode selection (3 bits)
    static const uint32_t SMCR_RESET_VALUE = 0x0;

    static const uint8_t DIER_TDE = 14;             // Trigger DMA request enable
    static const uint8_t DIER_CC2DE = 10;           // Capture/Compare 2 DMA request enable
    static const uint8_t DIER_CC1DE = 9;            // Capture/Compare 1 DMA request enable
    static const uint8_t DIER_UDE = 8;              // Update DMA request enable
    static const uint8_t DIER_BIE = 7;              // Break interrupt enable
    static const uint8_t DIER_TIE = 6;              // Trigger interrupt enable
    static const uint8_t DIER_COMIE = 5;            // COM interrupt enable
    static const uint8_t DIER_CC2IE = 2;            // Capture/Compare 2 interrupt enable
    static const uint8_t DIER_CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t DIER_UIE = 0;              // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static const uint8_t SR_CC2OF = 10;           // Capture/compare 2 overcapture flag
    static const uint8_t SR_CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t SR_BIF = 7;              // Break interrupt flag
    static const uint8_t SR_TIF = 6;              // Trigger interrupt flag
    static const uint8_t SR_COMIF = 5;            // COM interrupt flag
    static const uint8_t SR_CC2IF = 2;            // Capture/Compare 2 interrupt flag
    static const uint8_t SR_CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t SR_UIF = 0;              // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static const uint8_t EGR_BG = 7;               // Break generation
    static const uint8_t EGR_TG = 6;               // Trigger generation
    static const uint8_t EGR_COMG = 5;             // Capture/Compare control update generation
    static const uint8_t EGR_CC2G = 2;             // Capture/compare 2 generation
    static const uint8_t EGR_CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t EGR_UG = 0;               // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    static const uint8_t CCMR1_CC1S = 0;             // Capture/Compare 1 selection (2 bits)
    static const uint8_t CCMR1_CC2S = 8;             // Capture/Compare 2 selection (2 bits)
    static const uint8_t CCMR1_IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t CCMR1_IC1PSC = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CCMR1_IC2F = 12;            // Input capture 2 filter (4 bits)
    static const uint8_t CCMR1_IC2PSC = 10;          // Input capture 2 prescaler (2 bits)
    static const uint8_t CCMR1_OC1FE = 2;            // Output Compare 1 fast enable
    static const uint8_t CCMR1_OC1M = 4;             // Output Compare 1 mode (3 bits)
    static const uint8_t CCMR1_OC1PE = 3;            // Output Compare 1 preload enable
    static const uint8_t CCMR1_OC2FE = 10;           // Output Compare 2 fast enable
    static const uint8_t CCMR1_OC2M = 12;            // Output Compare 2 mode (3 bits)
    static const uint8_t CCMR1_OC2PE = 11;           // Output Compare 2 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static const uint8_t CCER_CC2NP = 7;            // Capture/Compare 2 output Polarity
    static const uint8_t CCER_CC2P = 5;             // Capture/Compare 2 output Polarity
    static const uint8_t CCER_CC2E = 4;             // Capture/Compare 2 output enable
    static const uint8_t CCER_CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1NE = 2;            // Capture/Compare 1 complementary output enable
    static const uint8_t CCER_CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1E = 0;             // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    static const uint8_t CNT_CNT = 0;              // counter value (16 bits)
    static const uint32_t CNT_RESET_VALUE = 0x0;

    static const uint8_t PSC_PSC = 0;              // Prescaler value (16 bits)
    static const uint32_t PSC_RESET_VALUE = 0x0;

    static const uint8_t ARR_ARR = 0;              // Auto-reload value (16 bits)
    static const uint32_t ARR_RESET_VALUE = 0x0;

    static const uint8_t RCR_REP = 0;              // Repetition counter value (8 bits)
    static const uint32_t RCR_RESET_VALUE = 0x0;

    static const uint8_t CCR1_CCR1 = 0;             // Capture/Compare 1 value (16 bits)
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    static const uint8_t CCR2_CCR2 = 0;             // Capture/Compare 2 value (16 bits)
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    static const uint8_t BDTR_MOE = 15;             // Main output enable
    static const uint8_t BDTR_AOE = 14;             // Automatic output enable
    static const uint8_t BDTR_BKP = 13;             // Break polarity
    static const uint8_t BDTR_BKE = 12;             // Break enable
    static const uint8_t BDTR_OSSR = 11;            // Off-state selection for Run mode
    static const uint8_t BDTR_OSSI = 10;            // Off-state selection for Idle mode
    static const uint8_t BDTR_LOCK = 8;             // Lock configuration (2 bits)
    static const uint8_t BDTR_DTG = 0;              // Dead-time generator setup (8 bits)
    static const uint32_t BDTR_RESET_VALUE = 0x0;

    static const uint8_t DCR_DBL = 8;              // DMA burst length (5 bits)
    static const uint8_t DCR_DBA = 0;              // DMA base address (5 bits)
    static const uint32_t DCR_RESET_VALUE = 0x0;

    static const uint8_t DMAR_DMAB = 0;             // DMA register for burst accesses (16 bits)
    static const uint32_t DMAR_RESET_VALUE = 0x0;
};

static tim15_t& TIM15 = *reinterpret_cast<tim15_t*>(0x40014000);


////
//
//    General-purpose-timers
//
////

struct tim16_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    reserved_t<1>        _0;
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1;                // [Read-write] capture/compare mode register (output mode)
    reserved_t<1>        _1;
    volatile uint32_t    CCER;                 // [Read-write] capture/compare enable register
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    volatile uint32_t    RCR;                  // [Read-write] repetition counter register
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    reserved_t<3>        _2;
    volatile uint32_t    BDTR;                 // [Read-write] break and dead-time register
    volatile uint32_t    DCR;                  // [Read-write] DMA control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static const uint8_t CR1_CKD = 8;              // Clock division (2 bits)
    static const uint8_t CR1_ARPE = 7;             // Auto-reload preload enable
    static const uint8_t CR1_OPM = 3;              // One-pulse mode
    static const uint8_t CR1_URS = 2;              // Update request source
    static const uint8_t CR1_UDIS = 1;             // Update disable
    static const uint8_t CR1_CEN = 0;              // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_OIS1N = 9;            // Output Idle state 1
    static const uint8_t CR2_OIS1 = 8;             // Output Idle state 1
    static const uint8_t CR2_CCDS = 3;             // Capture/compare DMA selection
    static const uint8_t CR2_CCUS = 2;             // Capture/compare control update selection
    static const uint8_t CR2_CCPC = 0;             // Capture/compare preloaded control
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t DIER_TDE = 14;             // Trigger DMA request enable
    static const uint8_t DIER_CC1DE = 9;            // Capture/Compare 1 DMA request enable
    static const uint8_t DIER_UDE = 8;              // Update DMA request enable
    static const uint8_t DIER_BIE = 7;              // Break interrupt enable
    static const uint8_t DIER_TIE = 6;              // Trigger interrupt enable
    static const uint8_t DIER_COMIE = 5;            // COM interrupt enable
    static const uint8_t DIER_CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t DIER_UIE = 0;              // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static const uint8_t SR_CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t SR_BIF = 7;              // Break interrupt flag
    static const uint8_t SR_TIF = 6;              // Trigger interrupt flag
    static const uint8_t SR_COMIF = 5;            // COM interrupt flag
    static const uint8_t SR_CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t SR_UIF = 0;              // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static const uint8_t EGR_BG = 7;               // Break generation
    static const uint8_t EGR_TG = 6;               // Trigger generation
    static const uint8_t EGR_COMG = 5;             // Capture/Compare control update generation
    static const uint8_t EGR_CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t EGR_UG = 0;               // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    static const uint8_t CCMR1_CC1S = 0;             // Capture/Compare 1 selection (2 bits)
    static const uint8_t CCMR1_IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t CCMR1_IC1PSC = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CCMR1_OC1FE = 2;            // Output Compare 1 fast enable
    static const uint8_t CCMR1_OC1M = 4;             // Output Compare 1 mode (3 bits)
    static const uint8_t CCMR1_OC1PE = 3;            // Output Compare 1 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static const uint8_t CCER_CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1NE = 2;            // Capture/Compare 1 complementary output enable
    static const uint8_t CCER_CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1E = 0;             // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    static const uint8_t CNT_CNT = 0;              // counter value (16 bits)
    static const uint32_t CNT_RESET_VALUE = 0x0;

    static const uint8_t PSC_PSC = 0;              // Prescaler value (16 bits)
    static const uint32_t PSC_RESET_VALUE = 0x0;

    static const uint8_t ARR_ARR = 0;              // Auto-reload value (16 bits)
    static const uint32_t ARR_RESET_VALUE = 0x0;

    static const uint8_t RCR_REP = 0;              // Repetition counter value (8 bits)
    static const uint32_t RCR_RESET_VALUE = 0x0;

    static const uint8_t CCR1_CCR1 = 0;             // Capture/Compare 1 value (16 bits)
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    static const uint8_t BDTR_MOE = 15;             // Main output enable
    static const uint8_t BDTR_AOE = 14;             // Automatic output enable
    static const uint8_t BDTR_BKP = 13;             // Break polarity
    static const uint8_t BDTR_BKE = 12;             // Break enable
    static const uint8_t BDTR_OSSR = 11;            // Off-state selection for Run mode
    static const uint8_t BDTR_OSSI = 10;            // Off-state selection for Idle mode
    static const uint8_t BDTR_LOCK = 8;             // Lock configuration (2 bits)
    static const uint8_t BDTR_DTG = 0;              // Dead-time generator setup (8 bits)
    static const uint32_t BDTR_RESET_VALUE = 0x0;

    static const uint8_t DCR_DBL = 8;              // DMA burst length (5 bits)
    static const uint8_t DCR_DBA = 0;              // DMA base address (5 bits)
    static const uint32_t DCR_RESET_VALUE = 0x0;

    static const uint8_t DMAR_DMAB = 0;             // DMA register for burst accesses (16 bits)
    static const uint32_t DMAR_RESET_VALUE = 0x0;
};

static tim16_t& TIM16 = *reinterpret_cast<tim16_t*>(0x40014400);


////
//
//    General-purpose-timers
//
////

struct tim17_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    reserved_t<1>        _0;
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1;                // [Read-write] capture/compare mode register (output mode)
    reserved_t<1>        _1;
    volatile uint32_t    CCER;                 // [Read-write] capture/compare enable register
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    volatile uint32_t    RCR;                  // [Read-write] repetition counter register
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    reserved_t<3>        _2;
    volatile uint32_t    BDTR;                 // [Read-write] break and dead-time register
    volatile uint32_t    DCR;                  // [Read-write] DMA control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static const uint8_t CR1_CKD = 8;              // Clock division (2 bits)
    static const uint8_t CR1_ARPE = 7;             // Auto-reload preload enable
    static const uint8_t CR1_OPM = 3;              // One-pulse mode
    static const uint8_t CR1_URS = 2;              // Update request source
    static const uint8_t CR1_UDIS = 1;             // Update disable
    static const uint8_t CR1_CEN = 0;              // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static const uint8_t CR2_OIS1N = 9;            // Output Idle state 1
    static const uint8_t CR2_OIS1 = 8;             // Output Idle state 1
    static const uint8_t CR2_CCDS = 3;             // Capture/compare DMA selection
    static const uint8_t CR2_CCUS = 2;             // Capture/compare control update selection
    static const uint8_t CR2_CCPC = 0;             // Capture/compare preloaded control
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static const uint8_t DIER_TDE = 14;             // Trigger DMA request enable
    static const uint8_t DIER_CC1DE = 9;            // Capture/Compare 1 DMA request enable
    static const uint8_t DIER_UDE = 8;              // Update DMA request enable
    static const uint8_t DIER_BIE = 7;              // Break interrupt enable
    static const uint8_t DIER_TIE = 6;              // Trigger interrupt enable
    static const uint8_t DIER_COMIE = 5;            // COM interrupt enable
    static const uint8_t DIER_CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t DIER_UIE = 0;              // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static const uint8_t SR_CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t SR_BIF = 7;              // Break interrupt flag
    static const uint8_t SR_TIF = 6;              // Trigger interrupt flag
    static const uint8_t SR_COMIF = 5;            // COM interrupt flag
    static const uint8_t SR_CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t SR_UIF = 0;              // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static const uint8_t EGR_BG = 7;               // Break generation
    static const uint8_t EGR_TG = 6;               // Trigger generation
    static const uint8_t EGR_COMG = 5;             // Capture/Compare control update generation
    static const uint8_t EGR_CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t EGR_UG = 0;               // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    static const uint8_t CCMR1_CC1S = 0;             // Capture/Compare 1 selection (2 bits)
    static const uint8_t CCMR1_IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t CCMR1_IC1PSC = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CCMR1_OC1FE = 2;            // Output Compare 1 fast enable
    static const uint8_t CCMR1_OC1M = 4;             // Output Compare 1 mode (3 bits)
    static const uint8_t CCMR1_OC1PE = 3;            // Output Compare 1 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static const uint8_t CCER_CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1NE = 2;            // Capture/Compare 1 complementary output enable
    static const uint8_t CCER_CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CCER_CC1E = 0;             // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    static const uint8_t CNT_CNT = 0;              // counter value (16 bits)
    static const uint32_t CNT_RESET_VALUE = 0x0;

    static const uint8_t PSC_PSC = 0;              // Prescaler value (16 bits)
    static const uint32_t PSC_RESET_VALUE = 0x0;

    static const uint8_t ARR_ARR = 0;              // Auto-reload value (16 bits)
    static const uint32_t ARR_RESET_VALUE = 0x0;

    static const uint8_t RCR_REP = 0;              // Repetition counter value (8 bits)
    static const uint32_t RCR_RESET_VALUE = 0x0;

    static const uint8_t CCR1_CCR1 = 0;             // Capture/Compare 1 value (16 bits)
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    static const uint8_t BDTR_MOE = 15;             // Main output enable
    static const uint8_t BDTR_AOE = 14;             // Automatic output enable
    static const uint8_t BDTR_BKP = 13;             // Break polarity
    static const uint8_t BDTR_BKE = 12;             // Break enable
    static const uint8_t BDTR_OSSR = 11;            // Off-state selection for Run mode
    static const uint8_t BDTR_OSSI = 10;            // Off-state selection for Idle mode
    static const uint8_t BDTR_LOCK = 8;             // Lock configuration (2 bits)
    static const uint8_t BDTR_DTG = 0;              // Dead-time generator setup (8 bits)
    static const uint32_t BDTR_RESET_VALUE = 0x0;

    static const uint8_t DCR_DBL = 8;              // DMA burst length (5 bits)
    static const uint8_t DCR_DBA = 0;              // DMA base address (5 bits)
    static const uint32_t DCR_RESET_VALUE = 0x0;

    static const uint8_t DMAR_DMAB = 0;             // DMA register for burst accesses (16 bits)
    static const uint32_t DMAR_RESET_VALUE = 0x0;
};

static tim17_t& TIM17 = *reinterpret_cast<tim17_t*>(0x40014800);


////
//
//    Touch sensing controller
//
////

struct tsc_t
{
    volatile uint32_t    CR;                   // [Read-write] control register
    volatile uint32_t    IER;                  // [Read-write] interrupt enable register
    volatile uint32_t    ICR;                  // [Read-write] interrupt clear register
    volatile uint32_t    ISR;                  // [Read-write] interrupt status register
    volatile uint32_t    IOHCR;                // [Read-write] I/O hysteresis control register
    reserved_t<1>        _0;
    volatile uint32_t    IOASCR;               // [Read-write] I/O analog switch control register
    reserved_t<1>        _1;
    volatile uint32_t    IOSCR;                // [Read-write] I/O sampling control register
    reserved_t<1>        _2;
    volatile uint32_t    IOCCR;                // [Read-write] I/O channel control register
    reserved_t<1>        _3;
    volatile uint32_t    IOGCSR;               // I/O group control status register
    volatile uint32_t    IOG1CR;               // [Read-only] I/O group x counter register
    volatile uint32_t    IOG2CR;               // [Read-only] I/O group x counter register
    volatile uint32_t    IOG3CR;               // [Read-only] I/O group x counter register
    volatile uint32_t    IOG4CR;               // [Read-only] I/O group x counter register
    volatile uint32_t    IOG5CR;               // [Read-only] I/O group x counter register
    volatile uint32_t    IOG6CR;               // [Read-only] I/O group x counter register

    static const uint8_t CR_CTPH = 28;            // Charge transfer pulse high (4 bits)
    static const uint8_t CR_CTPL = 24;            // Charge transfer pulse low (4 bits)
    static const uint8_t CR_SSD = 17;             // Spread spectrum deviation (7 bits)
    static const uint8_t CR_SSE = 16;             // Spread spectrum enable
    static const uint8_t CR_SSPSC = 15;           // Spread spectrum prescaler
    static const uint8_t CR_PGPSC = 12;           // pulse generator prescaler (3 bits)
    static const uint8_t CR_MCV = 5;              // Max count value (3 bits)
    static const uint8_t CR_IODEF = 4;            // I/O Default mode
    static const uint8_t CR_SYNCPOL = 3;          // Synchronization pin polarity
    static const uint8_t CR_AM = 2;               // Acquisition mode
    static const uint8_t CR_START = 1;            // Start a new acquisition
    static const uint8_t CR_TSCE = 0;             // Touch sensing controller enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    static const uint8_t IER_MCEIE = 1;            // Max count error interrupt enable
    static const uint8_t IER_EOAIE = 0;            // End of acquisition interrupt enable
    static const uint32_t IER_RESET_VALUE = 0x0;

    static const uint8_t ICR_MCEIC = 1;            // Max count error interrupt clear
    static const uint8_t ICR_EOAIC = 0;            // End of acquisition interrupt clear
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static const uint8_t ISR_MCEF = 1;             // Max count error flag
    static const uint8_t ISR_EOAF = 0;             // End of acquisition flag
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static const uint8_t IOHCR_G6_IO4 = 23;          // G6_IO4 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G6_IO3 = 22;          // G6_IO3 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G6_IO2 = 21;          // G6_IO2 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G6_IO1 = 20;          // G6_IO1 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G5_IO4 = 19;          // G5_IO4 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G5_IO3 = 18;          // G5_IO3 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G5_IO2 = 17;          // G5_IO2 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G5_IO1 = 16;          // G5_IO1 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G4_IO4 = 15;          // G4_IO4 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G4_IO3 = 14;          // G4_IO3 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G4_IO2 = 13;          // G4_IO2 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G4_IO1 = 12;          // G4_IO1 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G3_IO4 = 11;          // G3_IO4 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G3_IO3 = 10;          // G3_IO3 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G3_IO2 = 9;           // G3_IO2 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G3_IO1 = 8;           // G3_IO1 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G2_IO4 = 7;           // G2_IO4 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G2_IO3 = 6;           // G2_IO3 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G2_IO2 = 5;           // G2_IO2 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G2_IO1 = 4;           // G2_IO1 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G1_IO4 = 3;           // G1_IO4 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G1_IO3 = 2;           // G1_IO3 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G1_IO2 = 1;           // G1_IO2 Schmitt trigger hysteresis mode
    static const uint8_t IOHCR_G1_IO1 = 0;           // G1_IO1 Schmitt trigger hysteresis mode
    static const uint32_t IOHCR_RESET_VALUE = 0xffffffff;

    static const uint8_t IOASCR_G6_IO4 = 23;          // G6_IO4 analog switch enable
    static const uint8_t IOASCR_G6_IO3 = 22;          // G6_IO3 analog switch enable
    static const uint8_t IOASCR_G6_IO2 = 21;          // G6_IO2 analog switch enable
    static const uint8_t IOASCR_G6_IO1 = 20;          // G6_IO1 analog switch enable
    static const uint8_t IOASCR_G5_IO4 = 19;          // G5_IO4 analog switch enable
    static const uint8_t IOASCR_G5_IO3 = 18;          // G5_IO3 analog switch enable
    static const uint8_t IOASCR_G5_IO2 = 17;          // G5_IO2 analog switch enable
    static const uint8_t IOASCR_G5_IO1 = 16;          // G5_IO1 analog switch enable
    static const uint8_t IOASCR_G4_IO4 = 15;          // G4_IO4 analog switch enable
    static const uint8_t IOASCR_G4_IO3 = 14;          // G4_IO3 analog switch enable
    static const uint8_t IOASCR_G4_IO2 = 13;          // G4_IO2 analog switch enable
    static const uint8_t IOASCR_G4_IO1 = 12;          // G4_IO1 analog switch enable
    static const uint8_t IOASCR_G3_IO4 = 11;          // G3_IO4 analog switch enable
    static const uint8_t IOASCR_G3_IO3 = 10;          // G3_IO3 analog switch enable
    static const uint8_t IOASCR_G3_IO2 = 9;           // G3_IO2 analog switch enable
    static const uint8_t IOASCR_G3_IO1 = 8;           // G3_IO1 analog switch enable
    static const uint8_t IOASCR_G2_IO4 = 7;           // G2_IO4 analog switch enable
    static const uint8_t IOASCR_G2_IO3 = 6;           // G2_IO3 analog switch enable
    static const uint8_t IOASCR_G2_IO2 = 5;           // G2_IO2 analog switch enable
    static const uint8_t IOASCR_G2_IO1 = 4;           // G2_IO1 analog switch enable
    static const uint8_t IOASCR_G1_IO4 = 3;           // G1_IO4 analog switch enable
    static const uint8_t IOASCR_G1_IO3 = 2;           // G1_IO3 analog switch enable
    static const uint8_t IOASCR_G1_IO2 = 1;           // G1_IO2 analog switch enable
    static const uint8_t IOASCR_G1_IO1 = 0;           // G1_IO1 analog switch enable
    static const uint32_t IOASCR_RESET_VALUE = 0x0;

    static const uint8_t IOSCR_G6_IO4 = 23;          // G6_IO4 sampling mode
    static const uint8_t IOSCR_G6_IO3 = 22;          // G6_IO3 sampling mode
    static const uint8_t IOSCR_G6_IO2 = 21;          // G6_IO2 sampling mode
    static const uint8_t IOSCR_G6_IO1 = 20;          // G6_IO1 sampling mode
    static const uint8_t IOSCR_G5_IO4 = 19;          // G5_IO4 sampling mode
    static const uint8_t IOSCR_G5_IO3 = 18;          // G5_IO3 sampling mode
    static const uint8_t IOSCR_G5_IO2 = 17;          // G5_IO2 sampling mode
    static const uint8_t IOSCR_G5_IO1 = 16;          // G5_IO1 sampling mode
    static const uint8_t IOSCR_G4_IO4 = 15;          // G4_IO4 sampling mode
    static const uint8_t IOSCR_G4_IO3 = 14;          // G4_IO3 sampling mode
    static const uint8_t IOSCR_G4_IO2 = 13;          // G4_IO2 sampling mode
    static const uint8_t IOSCR_G4_IO1 = 12;          // G4_IO1 sampling mode
    static const uint8_t IOSCR_G3_IO4 = 11;          // G3_IO4 sampling mode
    static const uint8_t IOSCR_G3_IO3 = 10;          // G3_IO3 sampling mode
    static const uint8_t IOSCR_G3_IO2 = 9;           // G3_IO2 sampling mode
    static const uint8_t IOSCR_G3_IO1 = 8;           // G3_IO1 sampling mode
    static const uint8_t IOSCR_G2_IO4 = 7;           // G2_IO4 sampling mode
    static const uint8_t IOSCR_G2_IO3 = 6;           // G2_IO3 sampling mode
    static const uint8_t IOSCR_G2_IO2 = 5;           // G2_IO2 sampling mode
    static const uint8_t IOSCR_G2_IO1 = 4;           // G2_IO1 sampling mode
    static const uint8_t IOSCR_G1_IO4 = 3;           // G1_IO4 sampling mode
    static const uint8_t IOSCR_G1_IO3 = 2;           // G1_IO3 sampling mode
    static const uint8_t IOSCR_G1_IO2 = 1;           // G1_IO2 sampling mode
    static const uint8_t IOSCR_G1_IO1 = 0;           // G1_IO1 sampling mode
    static const uint32_t IOSCR_RESET_VALUE = 0x0;

    static const uint8_t IOCCR_G6_IO4 = 23;          // G6_IO4 channel mode
    static const uint8_t IOCCR_G6_IO3 = 22;          // G6_IO3 channel mode
    static const uint8_t IOCCR_G6_IO2 = 21;          // G6_IO2 channel mode
    static const uint8_t IOCCR_G6_IO1 = 20;          // G6_IO1 channel mode
    static const uint8_t IOCCR_G5_IO4 = 19;          // G5_IO4 channel mode
    static const uint8_t IOCCR_G5_IO3 = 18;          // G5_IO3 channel mode
    static const uint8_t IOCCR_G5_IO2 = 17;          // G5_IO2 channel mode
    static const uint8_t IOCCR_G5_IO1 = 16;          // G5_IO1 channel mode
    static const uint8_t IOCCR_G4_IO4 = 15;          // G4_IO4 channel mode
    static const uint8_t IOCCR_G4_IO3 = 14;          // G4_IO3 channel mode
    static const uint8_t IOCCR_G4_IO2 = 13;          // G4_IO2 channel mode
    static const uint8_t IOCCR_G4_IO1 = 12;          // G4_IO1 channel mode
    static const uint8_t IOCCR_G3_IO4 = 11;          // G3_IO4 channel mode
    static const uint8_t IOCCR_G3_IO3 = 10;          // G3_IO3 channel mode
    static const uint8_t IOCCR_G3_IO2 = 9;           // G3_IO2 channel mode
    static const uint8_t IOCCR_G3_IO1 = 8;           // G3_IO1 channel mode
    static const uint8_t IOCCR_G2_IO4 = 7;           // G2_IO4 channel mode
    static const uint8_t IOCCR_G2_IO3 = 6;           // G2_IO3 channel mode
    static const uint8_t IOCCR_G2_IO2 = 5;           // G2_IO2 channel mode
    static const uint8_t IOCCR_G2_IO1 = 4;           // G2_IO1 channel mode
    static const uint8_t IOCCR_G1_IO4 = 3;           // G1_IO4 channel mode
    static const uint8_t IOCCR_G1_IO3 = 2;           // G1_IO3 channel mode
    static const uint8_t IOCCR_G1_IO2 = 1;           // G1_IO2 channel mode
    static const uint8_t IOCCR_G1_IO1 = 0;           // G1_IO1 channel mode
    static const uint32_t IOCCR_RESET_VALUE = 0x0;

    static const uint8_t IOGCSR_G8S = 23;             // Analog I/O group x status, Read-write
    static const uint8_t IOGCSR_G7S = 22;             // Analog I/O group x status, Read-write
    static const uint8_t IOGCSR_G6S = 21;             // Analog I/O group x status, Read-only
    static const uint8_t IOGCSR_G5S = 20;             // Analog I/O group x status, Read-only
    static const uint8_t IOGCSR_G4S = 19;             // Analog I/O group x status, Read-only
    static const uint8_t IOGCSR_G3S = 18;             // Analog I/O group x status, Read-only
    static const uint8_t IOGCSR_G2S = 17;             // Analog I/O group x status, Read-only
    static const uint8_t IOGCSR_G1S = 16;             // Analog I/O group x status, Read-only
    static const uint8_t IOGCSR_G8E = 7;              // Analog I/O group x enable, Read-write
    static const uint8_t IOGCSR_G7E = 6;              // Analog I/O group x enable, Read-write
    static const uint8_t IOGCSR_G6E = 5;              // Analog I/O group x enable, Read-write
    static const uint8_t IOGCSR_G5E = 4;              // Analog I/O group x enable, Read-write
    static const uint8_t IOGCSR_G4E = 3;              // Analog I/O group x enable, Read-write
    static const uint8_t IOGCSR_G3E = 2;              // Analog I/O group x enable, Read-write
    static const uint8_t IOGCSR_G2E = 1;              // Analog I/O group x enable, Read-write
    static const uint8_t IOGCSR_G1E = 0;              // Analog I/O group x enable, Read-write
    static const uint32_t IOGCSR_RESET_VALUE = 0x0;

    static const uint8_t IOG1CR_CNT = 0;              // Counter value (14 bits)
    static const uint32_t IOG1CR_RESET_VALUE = 0x0;

    static const uint8_t IOG2CR_CNT = 0;              // Counter value (14 bits)
    static const uint32_t IOG2CR_RESET_VALUE = 0x0;

    static const uint8_t IOG3CR_CNT = 0;              // Counter value (14 bits)
    static const uint32_t IOG3CR_RESET_VALUE = 0x0;

    static const uint8_t IOG4CR_CNT = 0;              // Counter value (14 bits)
    static const uint32_t IOG4CR_RESET_VALUE = 0x0;

    static const uint8_t IOG5CR_CNT = 0;              // Counter value (14 bits)
    static const uint32_t IOG5CR_RESET_VALUE = 0x0;

    static const uint8_t IOG6CR_CNT = 0;              // Counter value (14 bits)
    static const uint32_t IOG6CR_RESET_VALUE = 0x0;
};

static tsc_t& TSC = *reinterpret_cast<tsc_t*>(0x40024000);


////
//
//    HDMI-CEC controller
//
////

struct cec_t
{
    volatile uint32_t    CR;                   // [Read-write] control register
    volatile uint32_t    CFGR;                 // [Read-write] configuration register
    volatile uint32_t    TXDR;                 // [Write-only] Tx data register
    volatile uint32_t    RXDR;                 // [Read-only] Rx Data Register
    volatile uint32_t    ISR;                  // [Read-write] Interrupt and Status Register
    volatile uint32_t    IER;                  // [Read-write] interrupt enable register

    static const uint8_t CR_TXEOM = 2;            // Tx End Of Message
    static const uint8_t CR_TXSOM = 1;            // Tx start of message
    static const uint8_t CR_CECEN = 0;            // CEC Enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    static const uint8_t CFGR_LBPEGEN = 11;         // Generate Error-Bit on Long Bit Period Error
    static const uint8_t CFGR_BREGEN = 10;          // Generate error-bit on bit rising error
    static const uint8_t CFGR_BRESTP = 9;           // Rx-stop on bit rising error
    static const uint8_t CFGR_RXTOL = 8;            // Rx-Tolerance
    static const uint8_t CFGR_SFT = 5;              // Signal Free Time (3 bits)
    static const uint8_t CFGR_LSTN = 4;             // Listen mode
    static const uint8_t CFGR_OAR = 0;              // Own Address (4 bits)
    static const uint32_t CFGR_RESET_VALUE = 0x0;

    static const uint8_t TXDR_TXD = 0;              // Tx Data register (8 bits)
    static const uint32_t TXDR_RESET_VALUE = 0x0;

    static const uint8_t RXDR_RXDR = 0;             // CEC Rx Data Register (8 bits)
    static const uint32_t RXDR_RESET_VALUE = 0x0;

    static const uint8_t ISR_TXACKE = 12;          // Tx-Missing acknowledge error
    static const uint8_t ISR_TXERR = 11;           // Tx-Error
    static const uint8_t ISR_TXUDR = 10;           // Tx-Buffer Underrun
    static const uint8_t ISR_TXEND = 9;            // End of Transmission
    static const uint8_t ISR_TXBR = 8;             // Tx-Byte Request
    static const uint8_t ISR_ARBLST = 7;           // Arbitration Lost
    static const uint8_t ISR_RXACKE = 6;           // Rx-Missing Acknowledge
    static const uint8_t ISR_LBPE = 5;             // Rx-Long Bit Period Error
    static const uint8_t ISR_SBPE = 4;             // Rx-Short Bit period error
    static const uint8_t ISR_BRE = 3;              // Rx-Bit rising error
    static const uint8_t ISR_RXOVR = 2;            // Rx-Overrun
    static const uint8_t ISR_RXEND = 1;            // End Of Reception
    static const uint8_t ISR_RXBR = 0;             // Rx-Byte Received
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static const uint8_t IER_TXACKIE = 12;         // Tx-Missing Acknowledge Error Interrupt Enable
    static const uint8_t IER_TXERRIE = 11;         // Tx-Error Interrupt Enable
    static const uint8_t IER_TXUDRIE = 10;         // Tx-Underrun interrupt enable
    static const uint8_t IER_TXENDIE = 9;          // Tx-End of message interrupt enable
    static const uint8_t IER_TXBRIE = 8;           // Tx-Byte Request Interrupt Enable
    static const uint8_t IER_ARBLSTIE = 7;         // Arbitration Lost Interrupt Enable
    static const uint8_t IER_RXACKIE = 6;          // Rx-Missing Acknowledge Error Interrupt Enable
    static const uint8_t IER_LBPEIE = 5;           // Long Bit Period Error Interrupt Enable
    static const uint8_t IER_SBPEIE = 4;           // Short Bit Period Error Interrupt Enable
    static const uint8_t IER_BREIE = 3;            // Bit Rising Error Interrupt Enable
    static const uint8_t IER_RXOVRIE = 2;          // Rx-Buffer Overrun Interrupt Enable
    static const uint8_t IER_RXENDIE = 1;          // End Of Reception Interrupt Enable
    static const uint8_t IER_RXBRIE = 0;           // Rx-Byte Received Interrupt Enable
    static const uint32_t IER_RESET_VALUE = 0x0;
};

static cec_t& CEC = *reinterpret_cast<cec_t*>(0x40007800);


////
//
//    Flash
//
////

struct flash_t
{
    volatile uint32_t    ACR;                  // Flash access control register
    volatile uint32_t    KEYR;                 // [Write-only] Flash key register
    volatile uint32_t    OPTKEYR;              // [Write-only] Flash option key register
    volatile uint32_t    SR;                   // Flash status register
    volatile uint32_t    CR;                   // [Read-write] Flash control register
    volatile uint32_t    AR;                   // [Write-only] Flash address register
    reserved_t<1>        _0;
    volatile uint32_t    OBR;                  // [Read-only] Option byte register
    volatile uint32_t    WRPR;                 // [Read-only] Write protection register

    static const uint8_t ACR_LATENCY = 0;          // LATENCY (3 bits), Read-write
    static const uint8_t ACR_PRFTBE = 4;           // PRFTBE, Read-write
    static const uint8_t ACR_PRFTBS = 5;           // PRFTBS, Read-only
    static const uint32_t ACR_RESET_VALUE = 0x30;

    static const uint8_t KEYR_FKEYR = 0;            // Flash Key (32 bits)
    static const uint32_t KEYR_RESET_VALUE = 0x0;

    static const uint8_t OPTKEYR_OPTKEYR = 0;          // Option byte key (32 bits)
    static const uint32_t OPTKEYR_RESET_VALUE = 0x0;

    static const uint8_t SR_EOP = 5;              // End of operation, Read-write
    static const uint8_t SR_WRPRT = 4;            // Write protection error, Read-write
    static const uint8_t SR_PGERR = 2;            // Programming error, Read-write
    static const uint8_t SR_BSY = 0;              // Busy, Read-only
    static const uint32_t SR_RESET_VALUE = 0x0;

    static const uint8_t CR_FORCE_OPTLOAD = 13;   // Force option byte loading
    static const uint8_t CR_EOPIE = 12;           // End of operation interrupt enable
    static const uint8_t CR_ERRIE = 10;           // Error interrupt enable
    static const uint8_t CR_OPTWRE = 9;           // Option bytes write enable
    static const uint8_t CR_LOCK = 7;             // Lock
    static const uint8_t CR_STRT = 6;             // Start
    static const uint8_t CR_OPTER = 5;            // Option byte erase
    static const uint8_t CR_OPTPG = 4;            // Option byte programming
    static const uint8_t CR_MER = 2;              // Mass erase
    static const uint8_t CR_PER = 1;              // Page erase
    static const uint8_t CR_PG = 0;               // Programming
    static const uint32_t CR_RESET_VALUE = 0x80;

    static const uint8_t AR_FAR = 0;              // Flash address (32 bits)
    static const uint32_t AR_RESET_VALUE = 0x0;

    static const uint8_t OBR_OPTERR = 0;           // Option byte error
    static const uint8_t OBR_RDPRT = 1;            // Read protection level status (2 bits)
    static const uint8_t OBR_WDG_SW = 8;           // WDG_SW
    static const uint8_t OBR_nRST_STOP = 9;        // nRST_STOP
    static const uint8_t OBR_nRST_STDBY = 10;      // nRST_STDBY
    static const uint8_t OBR_nBOOT0 = 11;          // nBOOT0
    static const uint8_t OBR_nBOOT1 = 12;          // BOOT1
    static const uint8_t OBR_VDDA_MONITOR = 13;    // VDDA_MONITOR
    static const uint8_t OBR_RAM_PARITY_CHECK = 14;// RAM_PARITY_CHECK (0 bits)
    static const uint8_t OBR_BOOT_SEL = 15;        // BOOT_SEL
    static const uint8_t OBR_Data0 = 16;           // Data0 (8 bits)
    static const uint8_t OBR_Data1 = 24;           // Data1 (8 bits)
    static const uint32_t OBR_RESET_VALUE = 0x3fffff2;

    static const uint8_t WRPR_WRP = 0;              // Write protect (32 bits)
    static const uint32_t WRPR_RESET_VALUE = 0xffffffff;
};

static flash_t& Flash = *reinterpret_cast<flash_t*>(0x40022000);


////
//
//    Debug support
//
////

struct dbgmcu_t
{
    volatile uint32_t    IDCODE;               // [Read-only] MCU Device ID Code Register
    volatile uint32_t    CR;                   // [Read-write] Debug MCU Configuration Register
    volatile uint32_t    APB1_FZ;              // [Read-write] Debug MCU APB1 freeze register
    volatile uint32_t    APB2_FZ;              // [Read-write] Debug MCU APB2 freeze register

    static const uint8_t IDCODE_DEV_ID = 0;           // Device Identifier (12 bits)
    static const uint8_t IDCODE_DIV_ID = 12;          // Division Identifier (4 bits)
    static const uint8_t IDCODE_REV_ID = 16;          // Revision Identifier (16 bits)
    static const uint32_t IDCODE_RESET_VALUE = 0x0;

    static const uint8_t CR_DBG_STOP = 1;         // Debug Stop Mode
    static const uint8_t CR_DBG_STANDBY = 2;      // Debug Standby Mode
    static const uint32_t CR_RESET_VALUE = 0x0;

    static const uint8_t APB1_FZ_DBG_TIM2_STOP = 0;    // TIM2 counter stopped when core is halted
    static const uint8_t APB1_FZ_DBG_TIM3_STOP = 1;    // TIM3 counter stopped when core is halted
    static const uint8_t APB1_FZ_TIM3_counter_stopped_when_core_is_halted = 4;// TIM6 counter stopped when core is halted
    static const uint8_t APB1_FZ_DBG_TIM7_STOP = 5;    // TIM7 counter stopped when core is halted
    static const uint8_t APB1_FZ_DBG_TIM14_STOP = 8;   // TIM14 counter stopped when core is halted
    static const uint8_t APB1_FZ_DBG_RTC_STOP = 10;    // Debug RTC stopped when core is halted
    static const uint8_t APB1_FZ_DBG_WWDG_STOP = 11;   // Debug window watchdog stopped when core is halted
    static const uint8_t APB1_FZ_DBG_IWDG_STOP = 12;   // Debug independent watchdog stopped when core is halted
    static const uint8_t APB1_FZ_DBG_I2C1_SMBUS_TIMEOUT = 21;// SMBUS timeout mode stopped when core is halted
    static const uint8_t APB1_FZ_DBG_CAN_STOP = 25;    // CAN stopped when core is halted
    static const uint32_t APB1_FZ_RESET_VALUE = 0x0;

    static const uint8_t APB2_FZ_DBG_TIM1_STOP = 11;   // TIM1 counter stopped when core is halted
    static const uint8_t APB2_FZ_DBG_TIM15_STOP = 16;  // TIM15 counter stopped when core is halted
    static const uint8_t APB2_FZ_DBG_TIM16_STOP = 17;  // TIM16 counter stopped when core is halted
    static const uint8_t APB2_FZ_DBG_TIM17_STOP = 18;  // TIM17 counter stopped when core is halted
    static const uint32_t APB2_FZ_RESET_VALUE = 0x0;
};

static dbgmcu_t& DBGMCU = *reinterpret_cast<dbgmcu_t*>(0x40015800);


////
//
//    Universal serial bus full-speed device interface
//
////

struct usb_t
{
    volatile uint32_t    EP0R;                 // [Read-write] endpoint 0 register
    volatile uint32_t    EP1R;                 // [Read-write] endpoint 1 register
    volatile uint32_t    EP2R;                 // [Read-write] endpoint 2 register
    volatile uint32_t    EP3R;                 // [Read-write] endpoint 3 register
    volatile uint32_t    EP4R;                 // [Read-write] endpoint 4 register
    volatile uint32_t    EP5R;                 // [Read-write] endpoint 5 register
    volatile uint32_t    EP6R;                 // [Read-write] endpoint 6 register
    volatile uint32_t    EP7R;                 // [Read-write] endpoint 7 register
    reserved_t<8>        _0;
    volatile uint32_t    CNTR;                 // [Read-write] control register
    volatile uint32_t    ISTR;                 // interrupt status register
    volatile uint32_t    FNR;                  // [Read-only] frame number register
    volatile uint32_t    DADDR;                // [Read-write] device address
    volatile uint32_t    BTABLE;               // [Read-write] Buffer table address
    volatile uint32_t    LPMCSR;               // LPM control and status register
    volatile uint32_t    BCDR;                 // Battery charging detector

    static const uint8_t EP0R_EA = 0;               // Endpoint address (4 bits)
    static const uint8_t EP0R_STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t EP0R_DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t EP0R_CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP0R_EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP0R_EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t EP0R_SETUP = 11;           // Setup transaction completed
    static const uint8_t EP0R_STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t EP0R_DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t EP0R_CTR_RX = 15;          // Correct transfer for reception
    static const uint32_t EP0R_RESET_VALUE = 0x0;

    static const uint8_t EP1R_EA = 0;               // Endpoint address (4 bits)
    static const uint8_t EP1R_STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t EP1R_DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t EP1R_CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP1R_EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP1R_EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t EP1R_SETUP = 11;           // Setup transaction completed
    static const uint8_t EP1R_STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t EP1R_DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t EP1R_CTR_RX = 15;          // Correct transfer for reception
    static const uint32_t EP1R_RESET_VALUE = 0x0;

    static const uint8_t EP2R_EA = 0;               // Endpoint address (4 bits)
    static const uint8_t EP2R_STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t EP2R_DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t EP2R_CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP2R_EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP2R_EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t EP2R_SETUP = 11;           // Setup transaction completed
    static const uint8_t EP2R_STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t EP2R_DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t EP2R_CTR_RX = 15;          // Correct transfer for reception
    static const uint32_t EP2R_RESET_VALUE = 0x0;

    static const uint8_t EP3R_EA = 0;               // Endpoint address (4 bits)
    static const uint8_t EP3R_STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t EP3R_DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t EP3R_CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP3R_EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP3R_EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t EP3R_SETUP = 11;           // Setup transaction completed
    static const uint8_t EP3R_STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t EP3R_DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t EP3R_CTR_RX = 15;          // Correct transfer for reception
    static const uint32_t EP3R_RESET_VALUE = 0x0;

    static const uint8_t EP4R_EA = 0;               // Endpoint address (4 bits)
    static const uint8_t EP4R_STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t EP4R_DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t EP4R_CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP4R_EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP4R_EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t EP4R_SETUP = 11;           // Setup transaction completed
    static const uint8_t EP4R_STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t EP4R_DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t EP4R_CTR_RX = 15;          // Correct transfer for reception
    static const uint32_t EP4R_RESET_VALUE = 0x0;

    static const uint8_t EP5R_EA = 0;               // Endpoint address (4 bits)
    static const uint8_t EP5R_STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t EP5R_DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t EP5R_CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP5R_EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP5R_EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t EP5R_SETUP = 11;           // Setup transaction completed
    static const uint8_t EP5R_STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t EP5R_DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t EP5R_CTR_RX = 15;          // Correct transfer for reception
    static const uint32_t EP5R_RESET_VALUE = 0x0;

    static const uint8_t EP6R_EA = 0;               // Endpoint address (4 bits)
    static const uint8_t EP6R_STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t EP6R_DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t EP6R_CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP6R_EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP6R_EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t EP6R_SETUP = 11;           // Setup transaction completed
    static const uint8_t EP6R_STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t EP6R_DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t EP6R_CTR_RX = 15;          // Correct transfer for reception
    static const uint32_t EP6R_RESET_VALUE = 0x0;

    static const uint8_t EP7R_EA = 0;               // Endpoint address (4 bits)
    static const uint8_t EP7R_STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t EP7R_DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t EP7R_CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP7R_EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP7R_EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t EP7R_SETUP = 11;           // Setup transaction completed
    static const uint8_t EP7R_STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t EP7R_DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t EP7R_CTR_RX = 15;          // Correct transfer for reception
    static const uint32_t EP7R_RESET_VALUE = 0x0;

    static const uint8_t CNTR_FRES = 0;             // Force USB Reset
    static const uint8_t CNTR_PDWN = 1;             // Power down
    static const uint8_t CNTR_LPMODE = 2;           // Low-power mode
    static const uint8_t CNTR_FSUSP = 3;            // Force suspend
    static const uint8_t CNTR_RESUME = 4;           // Resume request
    static const uint8_t CNTR_L1RESUME = 5;         // LPM L1 Resume request
    static const uint8_t CNTR_L1REQM = 7;           // LPM L1 state request interrupt mask
    static const uint8_t CNTR_ESOFM = 8;            // Expected start of frame interrupt mask
    static const uint8_t CNTR_SOFM = 9;             // Start of frame interrupt mask
    static const uint8_t CNTR_RESETM = 10;          // USB reset interrupt mask
    static const uint8_t CNTR_SUSPM = 11;           // Suspend mode interrupt mask
    static const uint8_t CNTR_WKUPM = 12;           // Wakeup interrupt mask
    static const uint8_t CNTR_ERRM = 13;            // Error interrupt mask
    static const uint8_t CNTR_PMAOVRM = 14;         // Packet memory area over / underrun interrupt mask
    static const uint8_t CNTR_CTRM = 15;            // Correct transfer interrupt mask
    static const uint32_t CNTR_RESET_VALUE = 0x3;

    static const uint8_t ISTR_EP_ID = 0;            // Endpoint Identifier (4 bits), Read-only
    static const uint8_t ISTR_DIR = 4;              // Direction of transaction, Read-only
    static const uint8_t ISTR_L1REQ = 7;            // LPM L1 state request, Read-write
    static const uint8_t ISTR_ESOF = 8;             // Expected start frame, Read-write
    static const uint8_t ISTR_SOF = 9;              // start of frame, Read-write
    static const uint8_t ISTR_RESET = 10;           // reset request, Read-write
    static const uint8_t ISTR_SUSP = 11;            // Suspend mode request, Read-write
    static const uint8_t ISTR_WKUP = 12;            // Wakeup, Read-write
    static const uint8_t ISTR_ERR = 13;             // Error, Read-write
    static const uint8_t ISTR_PMAOVR = 14;          // Packet memory area over / underrun, Read-write
    static const uint8_t ISTR_CTR = 15;             // Correct transfer, Read-only
    static const uint32_t ISTR_RESET_VALUE = 0x0;

    static const uint8_t FNR_FN = 0;               // Frame number (11 bits)
    static const uint8_t FNR_LSOF = 11;            // Lost SOF (2 bits)
    static const uint8_t FNR_LCK = 13;             // Locked
    static const uint8_t FNR_RXDM = 14;            // Receive data - line status
    static const uint8_t FNR_RXDP = 15;            // Receive data + line status
    static const uint32_t FNR_RESET_VALUE = 0x0;

    static const uint8_t DADDR_ADD = 0;              // Device address (7 bits)
    static const uint8_t DADDR_EF = 7;               // Enable function
    static const uint32_t DADDR_RESET_VALUE = 0x0;

    static const uint8_t BTABLE_BTABLE = 3;           // Buffer table (13 bits)
    static const uint32_t BTABLE_RESET_VALUE = 0x0;

    static const uint8_t LPMCSR_LPMEN = 0;            // LPM support enable, Read-write
    static const uint8_t LPMCSR_LPMACK = 1;           // LPM Token acknowledge enable, Read-write
    static const uint8_t LPMCSR_REMWAKE = 3;          // bRemoteWake value, Read-only
    static const uint8_t LPMCSR_BESL = 4;             // BESL value (4 bits), Read-only
    static const uint32_t LPMCSR_RESET_VALUE = 0x0;

    static const uint8_t BCDR_BCDEN = 0;            // Battery charging detector (BCD) enable, Read-write
    static const uint8_t BCDR_DCDEN = 1;            // Data contact detection (DCD) mode enable, Read-write
    static const uint8_t BCDR_PDEN = 2;             // Primary detection (PD) mode enable, Read-write
    static const uint8_t BCDR_SDEN = 3;             // Secondary detection (SD) mode enable, Read-write
    static const uint8_t BCDR_DCDET = 4;            // Data contact detection (DCD) status, Read-only
    static const uint8_t BCDR_PDET = 5;             // Primary detection (PD) status, Read-only
    static const uint8_t BCDR_SDET = 6;             // Secondary detection (SD) status, Read-only
    static const uint8_t BCDR_PS2DET = 7;           // DM pull-up detection status, Read-only
    static const uint8_t BCDR_DPPU = 15;            // DP pull-up control, Read-write
    static const uint32_t BCDR_RESET_VALUE = 0x0;
};

static usb_t& USB = *reinterpret_cast<usb_t*>(0x40005c00);


////
//
//    Clock recovery system
//
////

struct crs_t
{
    volatile uint32_t    CR;                   // [Read-write] control register
    volatile uint32_t    CFGR;                 // [Read-write] configuration register
    volatile uint32_t    ISR;                  // [Read-only] interrupt and status register
    volatile uint32_t    ICR;                  // [Read-write] interrupt flag clear register

    static const uint8_t CR_TRIM = 8;             // HSI48 oscillator smooth trimming (6 bits)
    static const uint8_t CR_SWSYNC = 7;           // Generate software SYNC event
    static const uint8_t CR_AUTOTRIMEN = 6;       // Automatic trimming enable
    static const uint8_t CR_CEN = 5;              // Frequency error counter enable
    static const uint8_t CR_ESYNCIE = 3;          // Expected SYNC interrupt enable
    static const uint8_t CR_ERRIE = 2;            // Synchronization or trimming error interrupt enable
    static const uint8_t CR_SYNCWARNIE = 1;       // SYNC warning interrupt enable
    static const uint8_t CR_SYNCOKIE = 0;         // SYNC event OK interrupt enable
    static const uint32_t CR_RESET_VALUE = 0x2000;

    static const uint8_t CFGR_SYNCPOL = 31;         // SYNC polarity selection
    static const uint8_t CFGR_SYNCSRC = 28;         // SYNC signal source selection (2 bits)
    static const uint8_t CFGR_SYNCDIV = 24;         // SYNC divider (3 bits)
    static const uint8_t CFGR_FELIM = 16;           // Frequency error limit (8 bits)
    static const uint8_t CFGR_RELOAD = 0;           // Counter reload value (16 bits)
    static const uint32_t CFGR_RESET_VALUE = 0x2022bb7f;

    static const uint8_t ISR_FECAP = 16;           // Frequency error capture (16 bits)
    static const uint8_t ISR_FEDIR = 15;           // Frequency error direction
    static const uint8_t ISR_TRIMOVF = 10;         // Trimming overflow or underflow
    static const uint8_t ISR_SYNCMISS = 9;         // SYNC missed
    static const uint8_t ISR_SYNCERR = 8;          // SYNC error
    static const uint8_t ISR_ESYNCF = 3;           // Expected SYNC flag
    static const uint8_t ISR_ERRF = 2;             // Error flag
    static const uint8_t ISR_SYNCWARNF = 1;        // SYNC warning flag
    static const uint8_t ISR_SYNCOKF = 0;          // SYNC event OK flag
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static const uint8_t ICR_ESYNCC = 3;           // Expected SYNC clear flag
    static const uint8_t ICR_ERRC = 2;             // Error clear flag
    static const uint8_t ICR_SYNCWARNC = 1;        // SYNC warning clear flag
    static const uint8_t ICR_SYNCOKC = 0;          // SYNC event OK clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;
};

static crs_t& CRS = *reinterpret_cast<crs_t*>(0x40006c00);


////
//
//    Controller area network
//
////

struct can_t
{
    volatile uint32_t    CAN_MCR;              // [Read-write] CAN_MCR
    volatile uint32_t    CAN_MSR;              // CAN_MSR
    volatile uint32_t    CAN_TSR;              // CAN_TSR
    volatile uint32_t    CAN_RF0R;             // CAN_RF0R
    volatile uint32_t    CAN_RF1R;             // CAN_RF1R
    volatile uint32_t    CAN_IER;              // [Read-write] CAN_IER
    volatile uint32_t    CAN_ESR;              // CAN_ESR
    volatile uint32_t    CAN_BTR;              // [Read-write] CAN BTR
    reserved_t<88>       _0;
    volatile uint32_t    CAN_TI0R;             // [Read-write] CAN_TI0R
    volatile uint32_t    CAN_TDT0R;            // [Read-write] CAN_TDT0R
    volatile uint32_t    CAN_TDL0R;            // [Read-write] CAN_TDL0R
    volatile uint32_t    CAN_TDH0R;            // [Read-write] CAN_TDH0R
    volatile uint32_t    CAN_TI1R;             // [Read-write] CAN_TI1R
    volatile uint32_t    CAN_TDT1R;            // [Read-write] CAN_TDT1R
    volatile uint32_t    CAN_TDL1R;            // [Read-write] CAN_TDL1R
    volatile uint32_t    CAN_TDH1R;            // [Read-write] CAN_TDH1R
    volatile uint32_t    CAN_TI2R;             // [Read-write] CAN_TI2R
    volatile uint32_t    CAN_TDT2R;            // [Read-write] CAN_TDT2R
    volatile uint32_t    CAN_TDL2R;            // [Read-write] CAN_TDL2R
    volatile uint32_t    CAN_TDH2R;            // [Read-write] CAN_TDH2R
    volatile uint32_t    CAN_RI0R;             // [Read-only] CAN_RI0R
    volatile uint32_t    CAN_RDT0R;            // [Read-only] CAN_RDT0R
    volatile uint32_t    CAN_RDL0R;            // [Read-only] CAN_RDL0R
    volatile uint32_t    CAN_RDH0R;            // [Read-only] CAN_RDH0R
    volatile uint32_t    CAN_RI1R;             // [Read-only] CAN_RI1R
    volatile uint32_t    CAN_RDT1R;            // [Read-only] CAN_RDT1R
    volatile uint32_t    CAN_RDL1R;            // [Read-only] CAN_RDL1R
    volatile uint32_t    CAN_RDH1R;            // [Read-only] CAN_RDH1R
    reserved_t<12>       _1;
    volatile uint32_t    CAN_FMR;              // [Read-write] CAN_FMR
    volatile uint32_t    CAN_FM1R;             // [Read-write] CAN_FM1R
    reserved_t<1>        _2;
    volatile uint32_t    CAN_FS1R;             // [Read-write] CAN_FS1R
    reserved_t<1>        _3;
    volatile uint32_t    CAN_FFA1R;            // [Read-write] CAN_FFA1R
    reserved_t<1>        _4;
    volatile uint32_t    CAN_FA1R;             // [Read-write] CAN_FA1R
    reserved_t<8>        _5;
    volatile uint32_t    F0R1;                 // [Read-write] Filter bank 0 register 1
    volatile uint32_t    F0R2;                 // [Read-write] Filter bank 0 register 2
    volatile uint32_t    F1R1;                 // [Read-write] Filter bank 1 register 1
    volatile uint32_t    F1R2;                 // [Read-write] Filter bank 1 register 2
    volatile uint32_t    F2R1;                 // [Read-write] Filter bank 2 register 1
    volatile uint32_t    F2R2;                 // [Read-write] Filter bank 2 register 2
    volatile uint32_t    F3R1;                 // [Read-write] Filter bank 3 register 1
    volatile uint32_t    F3R2;                 // [Read-write] Filter bank 3 register 2
    volatile uint32_t    F4R1;                 // [Read-write] Filter bank 4 register 1
    volatile uint32_t    F4R2;                 // [Read-write] Filter bank 4 register 2
    volatile uint32_t    F5R1;                 // [Read-write] Filter bank 5 register 1
    volatile uint32_t    F5R2;                 // [Read-write] Filter bank 5 register 2
    volatile uint32_t    F6R1;                 // [Read-write] Filter bank 6 register 1
    volatile uint32_t    F6R2;                 // [Read-write] Filter bank 6 register 2
    volatile uint32_t    F7R1;                 // [Read-write] Filter bank 7 register 1
    volatile uint32_t    F7R2;                 // [Read-write] Filter bank 7 register 2
    volatile uint32_t    F8R1;                 // [Read-write] Filter bank 8 register 1
    volatile uint32_t    F8R2;                 // [Read-write] Filter bank 8 register 2
    volatile uint32_t    F9R1;                 // [Read-write] Filter bank 9 register 1
    volatile uint32_t    F9R2;                 // [Read-write] Filter bank 9 register 2
    volatile uint32_t    F10R1;                // [Read-write] Filter bank 10 register 1
    volatile uint32_t    F10R2;                // [Read-write] Filter bank 10 register 2
    volatile uint32_t    F11R1;                // [Read-write] Filter bank 11 register 1
    volatile uint32_t    F11R2;                // [Read-write] Filter bank 11 register 2
    volatile uint32_t    F12R1;                // [Read-write] Filter bank 4 register 1
    volatile uint32_t    F12R2;                // [Read-write] Filter bank 12 register 2
    volatile uint32_t    F13R1;                // [Read-write] Filter bank 13 register 1
    volatile uint32_t    F13R2;                // [Read-write] Filter bank 13 register 2
    volatile uint32_t    F14R1;                // [Read-write] Filter bank 14 register 1
    volatile uint32_t    F14R2;                // [Read-write] Filter bank 14 register 2
    volatile uint32_t    F15R1;                // [Read-write] Filter bank 15 register 1
    volatile uint32_t    F15R2;                // [Read-write] Filter bank 15 register 2
    volatile uint32_t    F16R1;                // [Read-write] Filter bank 16 register 1
    volatile uint32_t    F16R2;                // [Read-write] Filter bank 16 register 2
    volatile uint32_t    F17R1;                // [Read-write] Filter bank 17 register 1
    volatile uint32_t    F17R2;                // [Read-write] Filter bank 17 register 2
    volatile uint32_t    F18R1;                // [Read-write] Filter bank 18 register 1
    volatile uint32_t    F18R2;                // [Read-write] Filter bank 18 register 2
    volatile uint32_t    F19R1;                // [Read-write] Filter bank 19 register 1
    volatile uint32_t    F19R2;                // [Read-write] Filter bank 19 register 2
    volatile uint32_t    F20R1;                // [Read-write] Filter bank 20 register 1
    volatile uint32_t    F20R2;                // [Read-write] Filter bank 20 register 2
    volatile uint32_t    F21R1;                // [Read-write] Filter bank 21 register 1
    volatile uint32_t    F21R2;                // [Read-write] Filter bank 21 register 2
    volatile uint32_t    F22R1;                // [Read-write] Filter bank 22 register 1
    volatile uint32_t    F22R2;                // [Read-write] Filter bank 22 register 2
    volatile uint32_t    F23R1;                // [Read-write] Filter bank 23 register 1
    volatile uint32_t    F23R2;                // [Read-write] Filter bank 23 register 2
    volatile uint32_t    F24R1;                // [Read-write] Filter bank 24 register 1
    volatile uint32_t    F24R2;                // [Read-write] Filter bank 24 register 2
    volatile uint32_t    F25R1;                // [Read-write] Filter bank 25 register 1
    volatile uint32_t    F25R2;                // [Read-write] Filter bank 25 register 2
    volatile uint32_t    F26R1;                // [Read-write] Filter bank 26 register 1
    volatile uint32_t    F26R2;                // [Read-write] Filter bank 26 register 2
    volatile uint32_t    F27R1;                // [Read-write] Filter bank 27 register 1
    volatile uint32_t    F27R2;                // [Read-write] Filter bank 27 register 2

    static const uint8_t CAN_MCR_DBF = 16;             // DBF
    static const uint8_t CAN_MCR_RESET = 15;           // RESET
    static const uint8_t CAN_MCR_TTCM = 7;             // TTCM
    static const uint8_t CAN_MCR_ABOM = 6;             // ABOM
    static const uint8_t CAN_MCR_AWUM = 5;             // AWUM
    static const uint8_t CAN_MCR_NART = 4;             // NART
    static const uint8_t CAN_MCR_RFLM = 3;             // RFLM
    static const uint8_t CAN_MCR_TXFP = 2;             // TXFP
    static const uint8_t CAN_MCR_SLEEP = 1;            // SLEEP
    static const uint8_t CAN_MCR_INRQ = 0;             // INRQ
    static const uint32_t CAN_MCR_RESET_VALUE = 0x0;

    static const uint8_t CAN_MSR_RX = 11;              // RX, Read-only
    static const uint8_t CAN_MSR_SAMP = 10;            // SAMP, Read-only
    static const uint8_t CAN_MSR_RXM = 9;              // RXM, Read-only
    static const uint8_t CAN_MSR_TXM = 8;              // TXM, Read-only
    static const uint8_t CAN_MSR_SLAKI = 4;            // SLAKI, Read-write
    static const uint8_t CAN_MSR_WKUI = 3;             // WKUI, Read-write
    static const uint8_t CAN_MSR_ERRI = 2;             // ERRI, Read-write
    static const uint8_t CAN_MSR_SLAK = 1;             // SLAK, Read-only
    static const uint8_t CAN_MSR_INAK = 0;             // INAK, Read-only
    static const uint32_t CAN_MSR_RESET_VALUE = 0x0;

    static const uint8_t CAN_TSR_LOW2 = 31;            // Lowest priority flag for mailbox 2, Read-only
    static const uint8_t CAN_TSR_LOW1 = 30;            // Lowest priority flag for mailbox 1, Read-only
    static const uint8_t CAN_TSR_LOW0 = 29;            // Lowest priority flag for mailbox 0, Read-only
    static const uint8_t CAN_TSR_TME2 = 28;            // Lowest priority flag for mailbox 2, Read-only
    static const uint8_t CAN_TSR_TME1 = 27;            // Lowest priority flag for mailbox 1, Read-only
    static const uint8_t CAN_TSR_TME0 = 26;            // Lowest priority flag for mailbox 0, Read-only
    static const uint8_t CAN_TSR_CODE = 24;            // CODE (2 bits), Read-only
    static const uint8_t CAN_TSR_ABRQ2 = 23;           // ABRQ2, Read-write
    static const uint8_t CAN_TSR_TERR2 = 19;           // TERR2, Read-write
    static const uint8_t CAN_TSR_ALST2 = 18;           // ALST2, Read-write
    static const uint8_t CAN_TSR_TXOK2 = 17;           // TXOK2, Read-write
    static const uint8_t CAN_TSR_RQCP2 = 16;           // RQCP2, Read-write
    static const uint8_t CAN_TSR_ABRQ1 = 15;           // ABRQ1, Read-write
    static const uint8_t CAN_TSR_TERR1 = 11;           // TERR1, Read-write
    static const uint8_t CAN_TSR_ALST1 = 10;           // ALST1, Read-write
    static const uint8_t CAN_TSR_TXOK1 = 9;            // TXOK1, Read-write
    static const uint8_t CAN_TSR_RQCP1 = 8;            // RQCP1, Read-write
    static const uint8_t CAN_TSR_ABRQ0 = 7;            // ABRQ0, Read-write
    static const uint8_t CAN_TSR_TERR0 = 3;            // TERR0, Read-write
    static const uint8_t CAN_TSR_ALST0 = 2;            // ALST0, Read-write
    static const uint8_t CAN_TSR_TXOK0 = 1;            // TXOK0, Read-write
    static const uint8_t CAN_TSR_RQCP0 = 0;            // RQCP0, Read-write
    static const uint32_t CAN_TSR_RESET_VALUE = 0x0;

    static const uint8_t CAN_RF0R_RFOM0 = 5;            // RFOM0, Read-write
    static const uint8_t CAN_RF0R_FOVR0 = 4;            // FOVR0, Read-write
    static const uint8_t CAN_RF0R_FULL0 = 3;            // FULL0, Read-write
    static const uint8_t CAN_RF0R_FMP0 = 0;             // FMP0 (2 bits), Read-only
    static const uint32_t CAN_RF0R_RESET_VALUE = 0x0;

    static const uint8_t CAN_RF1R_RFOM1 = 5;            // RFOM1, Read-write
    static const uint8_t CAN_RF1R_FOVR1 = 4;            // FOVR1, Read-write
    static const uint8_t CAN_RF1R_FULL1 = 3;            // FULL1, Read-write
    static const uint8_t CAN_RF1R_FMP1 = 0;             // FMP1 (2 bits), Read-only
    static const uint32_t CAN_RF1R_RESET_VALUE = 0x0;

    static const uint8_t CAN_IER_SLKIE = 17;           // SLKIE
    static const uint8_t CAN_IER_WKUIE = 16;           // WKUIE
    static const uint8_t CAN_IER_ERRIE = 15;           // ERRIE
    static const uint8_t CAN_IER_LECIE = 11;           // LECIE
    static const uint8_t CAN_IER_BOFIE = 10;           // BOFIE
    static const uint8_t CAN_IER_EPVIE = 9;            // EPVIE
    static const uint8_t CAN_IER_EWGIE = 8;            // EWGIE
    static const uint8_t CAN_IER_FOVIE1 = 6;           // FOVIE1
    static const uint8_t CAN_IER_FFIE1 = 5;            // FFIE1
    static const uint8_t CAN_IER_FMPIE1 = 4;           // FMPIE1
    static const uint8_t CAN_IER_FOVIE0 = 3;           // FOVIE0
    static const uint8_t CAN_IER_FFIE0 = 2;            // FFIE0
    static const uint8_t CAN_IER_FMPIE0 = 1;           // FMPIE0
    static const uint8_t CAN_IER_TMEIE = 0;            // TMEIE
    static const uint32_t CAN_IER_RESET_VALUE = 0x0;

    static const uint8_t CAN_ESR_REC = 24;             // REC (8 bits), Read-only
    static const uint8_t CAN_ESR_TEC = 16;             // TEC (8 bits), Read-only
    static const uint8_t CAN_ESR_LEC = 4;              // LEC (3 bits), Read-write
    static const uint8_t CAN_ESR_BOFF = 2;             // BOFF, Read-only
    static const uint8_t CAN_ESR_EPVF = 1;             // EPVF, Read-only
    static const uint8_t CAN_ESR_EWGF = 0;             // EWGF, Read-only
    static const uint32_t CAN_ESR_RESET_VALUE = 0x0;

    static const uint8_t CAN_BTR_SILM = 31;            // SILM
    static const uint8_t CAN_BTR_LBKM = 30;            // LBKM
    static const uint8_t CAN_BTR_SJW = 24;             // SJW (2 bits)
    static const uint8_t CAN_BTR_TS2 = 20;             // TS2 (3 bits)
    static const uint8_t CAN_BTR_TS1 = 16;             // TS1 (4 bits)
    static const uint8_t CAN_BTR_BRP = 0;              // BRP (10 bits)
    static const uint32_t CAN_BTR_RESET_VALUE = 0x0;

    static const uint8_t CAN_TI0R_STID = 21;            // STID (11 bits)
    static const uint8_t CAN_TI0R_EXID = 3;             // EXID (18 bits)
    static const uint8_t CAN_TI0R_IDE = 2;              // IDE
    static const uint8_t CAN_TI0R_RTR = 1;              // RTR
    static const uint8_t CAN_TI0R_TXRQ = 0;             // TXRQ
    static const uint32_t CAN_TI0R_RESET_VALUE = 0x0;

    static const uint8_t CAN_TDT0R_TIME = 16;            // TIME (16 bits)
    static const uint8_t CAN_TDT0R_TGT = 8;              // TGT
    static const uint8_t CAN_TDT0R_DLC = 0;              // DLC (4 bits)
    static const uint32_t CAN_TDT0R_RESET_VALUE = 0x0;

    static const uint8_t CAN_TDL0R_DATA3 = 24;           // DATA3 (8 bits)
    static const uint8_t CAN_TDL0R_DATA2 = 16;           // DATA2 (8 bits)
    static const uint8_t CAN_TDL0R_DATA1 = 8;            // DATA1 (8 bits)
    static const uint8_t CAN_TDL0R_DATA0 = 0;            // DATA0 (8 bits)
    static const uint32_t CAN_TDL0R_RESET_VALUE = 0x0;

    static const uint8_t CAN_TDH0R_DATA7 = 24;           // DATA7 (8 bits)
    static const uint8_t CAN_TDH0R_DATA6 = 16;           // DATA6 (8 bits)
    static const uint8_t CAN_TDH0R_DATA5 = 8;            // DATA5 (8 bits)
    static const uint8_t CAN_TDH0R_DATA4 = 0;            // DATA4 (8 bits)
    static const uint32_t CAN_TDH0R_RESET_VALUE = 0x0;

    static const uint8_t CAN_TI1R_STID = 21;            // STID (11 bits)
    static const uint8_t CAN_TI1R_EXID = 3;             // EXID (18 bits)
    static const uint8_t CAN_TI1R_IDE = 2;              // IDE
    static const uint8_t CAN_TI1R_RTR = 1;              // RTR
    static const uint8_t CAN_TI1R_TXRQ = 0;             // TXRQ
    static const uint32_t CAN_TI1R_RESET_VALUE = 0x0;

    static const uint8_t CAN_TDT1R_TIME = 16;            // TIME (16 bits)
    static const uint8_t CAN_TDT1R_TGT = 8;              // TGT
    static const uint8_t CAN_TDT1R_DLC = 0;              // DLC (4 bits)
    static const uint32_t CAN_TDT1R_RESET_VALUE = 0x0;

    static const uint8_t CAN_TDL1R_DATA3 = 24;           // DATA3 (8 bits)
    static const uint8_t CAN_TDL1R_DATA2 = 16;           // DATA2 (8 bits)
    static const uint8_t CAN_TDL1R_DATA1 = 8;            // DATA1 (8 bits)
    static const uint8_t CAN_TDL1R_DATA0 = 0;            // DATA0 (8 bits)
    static const uint32_t CAN_TDL1R_RESET_VALUE = 0x0;

    static const uint8_t CAN_TDH1R_DATA7 = 24;           // DATA7 (8 bits)
    static const uint8_t CAN_TDH1R_DATA6 = 16;           // DATA6 (8 bits)
    static const uint8_t CAN_TDH1R_DATA5 = 8;            // DATA5 (8 bits)
    static const uint8_t CAN_TDH1R_DATA4 = 0;            // DATA4 (8 bits)
    static const uint32_t CAN_TDH1R_RESET_VALUE = 0x0;

    static const uint8_t CAN_TI2R_STID = 21;            // STID (11 bits)
    static const uint8_t CAN_TI2R_EXID = 3;             // EXID (18 bits)
    static const uint8_t CAN_TI2R_IDE = 2;              // IDE
    static const uint8_t CAN_TI2R_RTR = 1;              // RTR
    static const uint8_t CAN_TI2R_TXRQ = 0;             // TXRQ
    static const uint32_t CAN_TI2R_RESET_VALUE = 0x0;

    static const uint8_t CAN_TDT2R_TIME = 16;            // TIME (16 bits)
    static const uint8_t CAN_TDT2R_TGT = 8;              // TGT
    static const uint8_t CAN_TDT2R_DLC = 0;              // DLC (4 bits)
    static const uint32_t CAN_TDT2R_RESET_VALUE = 0x0;

    static const uint8_t CAN_TDL2R_DATA3 = 24;           // DATA3 (8 bits)
    static const uint8_t CAN_TDL2R_DATA2 = 16;           // DATA2 (8 bits)
    static const uint8_t CAN_TDL2R_DATA1 = 8;            // DATA1 (8 bits)
    static const uint8_t CAN_TDL2R_DATA0 = 0;            // DATA0 (8 bits)
    static const uint32_t CAN_TDL2R_RESET_VALUE = 0x0;

    static const uint8_t CAN_TDH2R_DATA7 = 24;           // DATA7 (8 bits)
    static const uint8_t CAN_TDH2R_DATA6 = 16;           // DATA6 (8 bits)
    static const uint8_t CAN_TDH2R_DATA5 = 8;            // DATA5 (8 bits)
    static const uint8_t CAN_TDH2R_DATA4 = 0;            // DATA4 (8 bits)
    static const uint32_t CAN_TDH2R_RESET_VALUE = 0x0;

    static const uint8_t CAN_RI0R_STID = 21;            // STID (11 bits)
    static const uint8_t CAN_RI0R_EXID = 3;             // EXID (18 bits)
    static const uint8_t CAN_RI0R_IDE = 2;              // IDE
    static const uint8_t CAN_RI0R_RTR = 1;              // RTR
    static const uint32_t CAN_RI0R_RESET_VALUE = 0x0;

    static const uint8_t CAN_RDT0R_TIME = 16;            // TIME (16 bits)
    static const uint8_t CAN_RDT0R_FMI = 8;              // FMI (8 bits)
    static const uint8_t CAN_RDT0R_DLC = 0;              // DLC (4 bits)
    static const uint32_t CAN_RDT0R_RESET_VALUE = 0x0;

    static const uint8_t CAN_RDL0R_DATA3 = 24;           // DATA3 (8 bits)
    static const uint8_t CAN_RDL0R_DATA2 = 16;           // DATA2 (8 bits)
    static const uint8_t CAN_RDL0R_DATA1 = 8;            // DATA1 (8 bits)
    static const uint8_t CAN_RDL0R_DATA0 = 0;            // DATA0 (8 bits)
    static const uint32_t CAN_RDL0R_RESET_VALUE = 0x0;

    static const uint8_t CAN_RDH0R_DATA7 = 24;           // DATA7 (8 bits)
    static const uint8_t CAN_RDH0R_DATA6 = 16;           // DATA6 (8 bits)
    static const uint8_t CAN_RDH0R_DATA5 = 8;            // DATA5 (8 bits)
    static const uint8_t CAN_RDH0R_DATA4 = 0;            // DATA4 (8 bits)
    static const uint32_t CAN_RDH0R_RESET_VALUE = 0x0;

    static const uint8_t CAN_RI1R_STID = 21;            // STID (11 bits)
    static const uint8_t CAN_RI1R_EXID = 3;             // EXID (18 bits)
    static const uint8_t CAN_RI1R_IDE = 2;              // IDE
    static const uint8_t CAN_RI1R_RTR = 1;              // RTR
    static const uint32_t CAN_RI1R_RESET_VALUE = 0x0;

    static const uint8_t CAN_RDT1R_TIME = 16;            // TIME (16 bits)
    static const uint8_t CAN_RDT1R_FMI = 8;              // FMI (8 bits)
    static const uint8_t CAN_RDT1R_DLC = 0;              // DLC (4 bits)
    static const uint32_t CAN_RDT1R_RESET_VALUE = 0x0;

    static const uint8_t CAN_RDL1R_DATA3 = 24;           // DATA3 (8 bits)
    static const uint8_t CAN_RDL1R_DATA2 = 16;           // DATA2 (8 bits)
    static const uint8_t CAN_RDL1R_DATA1 = 8;            // DATA1 (8 bits)
    static const uint8_t CAN_RDL1R_DATA0 = 0;            // DATA0 (8 bits)
    static const uint32_t CAN_RDL1R_RESET_VALUE = 0x0;

    static const uint8_t CAN_RDH1R_DATA7 = 24;           // DATA7 (8 bits)
    static const uint8_t CAN_RDH1R_DATA6 = 16;           // DATA6 (8 bits)
    static const uint8_t CAN_RDH1R_DATA5 = 8;            // DATA5 (8 bits)
    static const uint8_t CAN_RDH1R_DATA4 = 0;            // DATA4 (8 bits)
    static const uint32_t CAN_RDH1R_RESET_VALUE = 0x0;

    static const uint8_t CAN_FMR_CAN2SB = 8;           // CAN2SB (6 bits)
    static const uint8_t CAN_FMR_FINIT = 0;            // FINIT
    static const uint32_t CAN_FMR_RESET_VALUE = 0x0;

    static const uint8_t CAN_FM1R_FBM0 = 0;             // Filter mode
    static const uint8_t CAN_FM1R_FBM1 = 1;             // Filter mode
    static const uint8_t CAN_FM1R_FBM2 = 2;             // Filter mode
    static const uint8_t CAN_FM1R_FBM3 = 3;             // Filter mode
    static const uint8_t CAN_FM1R_FBM4 = 4;             // Filter mode
    static const uint8_t CAN_FM1R_FBM5 = 5;             // Filter mode
    static const uint8_t CAN_FM1R_FBM6 = 6;             // Filter mode
    static const uint8_t CAN_FM1R_FBM7 = 7;             // Filter mode
    static const uint8_t CAN_FM1R_FBM8 = 8;             // Filter mode
    static const uint8_t CAN_FM1R_FBM9 = 9;             // Filter mode
    static const uint8_t CAN_FM1R_FBM10 = 10;           // Filter mode
    static const uint8_t CAN_FM1R_FBM11 = 11;           // Filter mode
    static const uint8_t CAN_FM1R_FBM12 = 12;           // Filter mode
    static const uint8_t CAN_FM1R_FBM13 = 13;           // Filter mode
    static const uint8_t CAN_FM1R_FBM14 = 14;           // Filter mode
    static const uint8_t CAN_FM1R_FBM15 = 15;           // Filter mode
    static const uint8_t CAN_FM1R_FBM16 = 16;           // Filter mode
    static const uint8_t CAN_FM1R_FBM17 = 17;           // Filter mode
    static const uint8_t CAN_FM1R_FBM18 = 18;           // Filter mode
    static const uint8_t CAN_FM1R_FBM19 = 19;           // Filter mode
    static const uint8_t CAN_FM1R_FBM20 = 20;           // Filter mode
    static const uint8_t CAN_FM1R_FBM21 = 21;           // Filter mode
    static const uint8_t CAN_FM1R_FBM22 = 22;           // Filter mode
    static const uint8_t CAN_FM1R_FBM23 = 23;           // Filter mode
    static const uint8_t CAN_FM1R_FBM24 = 24;           // Filter mode
    static const uint8_t CAN_FM1R_FBM25 = 25;           // Filter mode
    static const uint8_t CAN_FM1R_FBM26 = 26;           // Filter mode
    static const uint8_t CAN_FM1R_FBM27 = 27;           // Filter mode
    static const uint32_t CAN_FM1R_RESET_VALUE = 0x0;

    static const uint8_t CAN_FS1R_FSC0 = 0;             // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC1 = 1;             // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC2 = 2;             // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC3 = 3;             // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC4 = 4;             // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC5 = 5;             // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC6 = 6;             // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC7 = 7;             // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC8 = 8;             // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC9 = 9;             // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC10 = 10;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC11 = 11;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC12 = 12;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC13 = 13;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC14 = 14;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC15 = 15;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC16 = 16;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC17 = 17;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC18 = 18;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC19 = 19;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC20 = 20;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC21 = 21;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC22 = 22;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC23 = 23;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC24 = 24;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC25 = 25;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC26 = 26;           // Filter scale configuration
    static const uint8_t CAN_FS1R_FSC27 = 27;           // Filter scale configuration
    static const uint32_t CAN_FS1R_RESET_VALUE = 0x0;

    static const uint8_t CAN_FFA1R_FFA0 = 0;             // Filter FIFO assignment for filter 0
    static const uint8_t CAN_FFA1R_FFA1 = 1;             // Filter FIFO assignment for filter 1
    static const uint8_t CAN_FFA1R_FFA2 = 2;             // Filter FIFO assignment for filter 2
    static const uint8_t CAN_FFA1R_FFA3 = 3;             // Filter FIFO assignment for filter 3
    static const uint8_t CAN_FFA1R_FFA4 = 4;             // Filter FIFO assignment for filter 4
    static const uint8_t CAN_FFA1R_FFA5 = 5;             // Filter FIFO assignment for filter 5
    static const uint8_t CAN_FFA1R_FFA6 = 6;             // Filter FIFO assignment for filter 6
    static const uint8_t CAN_FFA1R_FFA7 = 7;             // Filter FIFO assignment for filter 7
    static const uint8_t CAN_FFA1R_FFA8 = 8;             // Filter FIFO assignment for filter 8
    static const uint8_t CAN_FFA1R_FFA9 = 9;             // Filter FIFO assignment for filter 9
    static const uint8_t CAN_FFA1R_FFA10 = 10;           // Filter FIFO assignment for filter 10
    static const uint8_t CAN_FFA1R_FFA11 = 11;           // Filter FIFO assignment for filter 11
    static const uint8_t CAN_FFA1R_FFA12 = 12;           // Filter FIFO assignment for filter 12
    static const uint8_t CAN_FFA1R_FFA13 = 13;           // Filter FIFO assignment for filter 13
    static const uint8_t CAN_FFA1R_FFA14 = 14;           // Filter FIFO assignment for filter 14
    static const uint8_t CAN_FFA1R_FFA15 = 15;           // Filter FIFO assignment for filter 15
    static const uint8_t CAN_FFA1R_FFA16 = 16;           // Filter FIFO assignment for filter 16
    static const uint8_t CAN_FFA1R_FFA17 = 17;           // Filter FIFO assignment for filter 17
    static const uint8_t CAN_FFA1R_FFA18 = 18;           // Filter FIFO assignment for filter 18
    static const uint8_t CAN_FFA1R_FFA19 = 19;           // Filter FIFO assignment for filter 19
    static const uint8_t CAN_FFA1R_FFA20 = 20;           // Filter FIFO assignment for filter 20
    static const uint8_t CAN_FFA1R_FFA21 = 21;           // Filter FIFO assignment for filter 21
    static const uint8_t CAN_FFA1R_FFA22 = 22;           // Filter FIFO assignment for filter 22
    static const uint8_t CAN_FFA1R_FFA23 = 23;           // Filter FIFO assignment for filter 23
    static const uint8_t CAN_FFA1R_FFA24 = 24;           // Filter FIFO assignment for filter 24
    static const uint8_t CAN_FFA1R_FFA25 = 25;           // Filter FIFO assignment for filter 25
    static const uint8_t CAN_FFA1R_FFA26 = 26;           // Filter FIFO assignment for filter 26
    static const uint8_t CAN_FFA1R_FFA27 = 27;           // Filter FIFO assignment for filter 27
    static const uint32_t CAN_FFA1R_RESET_VALUE = 0x0;

    static const uint8_t CAN_FA1R_FACT0 = 0;            // Filter active
    static const uint8_t CAN_FA1R_FACT1 = 1;            // Filter active
    static const uint8_t CAN_FA1R_FACT2 = 2;            // Filter active
    static const uint8_t CAN_FA1R_FACT3 = 3;            // Filter active
    static const uint8_t CAN_FA1R_FACT4 = 4;            // Filter active
    static const uint8_t CAN_FA1R_FACT5 = 5;            // Filter active
    static const uint8_t CAN_FA1R_FACT6 = 6;            // Filter active
    static const uint8_t CAN_FA1R_FACT7 = 7;            // Filter active
    static const uint8_t CAN_FA1R_FACT8 = 8;            // Filter active
    static const uint8_t CAN_FA1R_FACT9 = 9;            // Filter active
    static const uint8_t CAN_FA1R_FACT10 = 10;          // Filter active
    static const uint8_t CAN_FA1R_FACT11 = 11;          // Filter active
    static const uint8_t CAN_FA1R_FACT12 = 12;          // Filter active
    static const uint8_t CAN_FA1R_FACT13 = 13;          // Filter active
    static const uint8_t CAN_FA1R_FACT14 = 14;          // Filter active
    static const uint8_t CAN_FA1R_FACT15 = 15;          // Filter active
    static const uint8_t CAN_FA1R_FACT16 = 16;          // Filter active
    static const uint8_t CAN_FA1R_FACT17 = 17;          // Filter active
    static const uint8_t CAN_FA1R_FACT18 = 18;          // Filter active
    static const uint8_t CAN_FA1R_FACT19 = 19;          // Filter active
    static const uint8_t CAN_FA1R_FACT20 = 20;          // Filter active
    static const uint8_t CAN_FA1R_FACT21 = 21;          // Filter active
    static const uint8_t CAN_FA1R_FACT22 = 22;          // Filter active
    static const uint8_t CAN_FA1R_FACT23 = 23;          // Filter active
    static const uint8_t CAN_FA1R_FACT24 = 24;          // Filter active
    static const uint8_t CAN_FA1R_FACT25 = 25;          // Filter active
    static const uint8_t CAN_FA1R_FACT26 = 26;          // Filter active
    static const uint8_t CAN_FA1R_FACT27 = 27;          // Filter active
    static const uint32_t CAN_FA1R_RESET_VALUE = 0x0;

    static const uint8_t F0R1_FB0 = 0;              // Filter bits
    static const uint8_t F0R1_FB1 = 1;              // Filter bits
    static const uint8_t F0R1_FB2 = 2;              // Filter bits
    static const uint8_t F0R1_FB3 = 3;              // Filter bits
    static const uint8_t F0R1_FB4 = 4;              // Filter bits
    static const uint8_t F0R1_FB5 = 5;              // Filter bits
    static const uint8_t F0R1_FB6 = 6;              // Filter bits
    static const uint8_t F0R1_FB7 = 7;              // Filter bits
    static const uint8_t F0R1_FB8 = 8;              // Filter bits
    static const uint8_t F0R1_FB9 = 9;              // Filter bits
    static const uint8_t F0R1_FB10 = 10;            // Filter bits
    static const uint8_t F0R1_FB11 = 11;            // Filter bits
    static const uint8_t F0R1_FB12 = 12;            // Filter bits
    static const uint8_t F0R1_FB13 = 13;            // Filter bits
    static const uint8_t F0R1_FB14 = 14;            // Filter bits
    static const uint8_t F0R1_FB15 = 15;            // Filter bits
    static const uint8_t F0R1_FB16 = 16;            // Filter bits
    static const uint8_t F0R1_FB17 = 17;            // Filter bits
    static const uint8_t F0R1_FB18 = 18;            // Filter bits
    static const uint8_t F0R1_FB19 = 19;            // Filter bits
    static const uint8_t F0R1_FB20 = 20;            // Filter bits
    static const uint8_t F0R1_FB21 = 21;            // Filter bits
    static const uint8_t F0R1_FB22 = 22;            // Filter bits
    static const uint8_t F0R1_FB23 = 23;            // Filter bits
    static const uint8_t F0R1_FB24 = 24;            // Filter bits
    static const uint8_t F0R1_FB25 = 25;            // Filter bits
    static const uint8_t F0R1_FB26 = 26;            // Filter bits
    static const uint8_t F0R1_FB27 = 27;            // Filter bits
    static const uint8_t F0R1_FB28 = 28;            // Filter bits
    static const uint8_t F0R1_FB29 = 29;            // Filter bits
    static const uint8_t F0R1_FB30 = 30;            // Filter bits
    static const uint8_t F0R1_FB31 = 31;            // Filter bits
    static const uint32_t F0R1_RESET_VALUE = 0x0;

    static const uint8_t F0R2_FB0 = 0;              // Filter bits
    static const uint8_t F0R2_FB1 = 1;              // Filter bits
    static const uint8_t F0R2_FB2 = 2;              // Filter bits
    static const uint8_t F0R2_FB3 = 3;              // Filter bits
    static const uint8_t F0R2_FB4 = 4;              // Filter bits
    static const uint8_t F0R2_FB5 = 5;              // Filter bits
    static const uint8_t F0R2_FB6 = 6;              // Filter bits
    static const uint8_t F0R2_FB7 = 7;              // Filter bits
    static const uint8_t F0R2_FB8 = 8;              // Filter bits
    static const uint8_t F0R2_FB9 = 9;              // Filter bits
    static const uint8_t F0R2_FB10 = 10;            // Filter bits
    static const uint8_t F0R2_FB11 = 11;            // Filter bits
    static const uint8_t F0R2_FB12 = 12;            // Filter bits
    static const uint8_t F0R2_FB13 = 13;            // Filter bits
    static const uint8_t F0R2_FB14 = 14;            // Filter bits
    static const uint8_t F0R2_FB15 = 15;            // Filter bits
    static const uint8_t F0R2_FB16 = 16;            // Filter bits
    static const uint8_t F0R2_FB17 = 17;            // Filter bits
    static const uint8_t F0R2_FB18 = 18;            // Filter bits
    static const uint8_t F0R2_FB19 = 19;            // Filter bits
    static const uint8_t F0R2_FB20 = 20;            // Filter bits
    static const uint8_t F0R2_FB21 = 21;            // Filter bits
    static const uint8_t F0R2_FB22 = 22;            // Filter bits
    static const uint8_t F0R2_FB23 = 23;            // Filter bits
    static const uint8_t F0R2_FB24 = 24;            // Filter bits
    static const uint8_t F0R2_FB25 = 25;            // Filter bits
    static const uint8_t F0R2_FB26 = 26;            // Filter bits
    static const uint8_t F0R2_FB27 = 27;            // Filter bits
    static const uint8_t F0R2_FB28 = 28;            // Filter bits
    static const uint8_t F0R2_FB29 = 29;            // Filter bits
    static const uint8_t F0R2_FB30 = 30;            // Filter bits
    static const uint8_t F0R2_FB31 = 31;            // Filter bits
    static const uint32_t F0R2_RESET_VALUE = 0x0;

    static const uint8_t F1R1_FB0 = 0;              // Filter bits
    static const uint8_t F1R1_FB1 = 1;              // Filter bits
    static const uint8_t F1R1_FB2 = 2;              // Filter bits
    static const uint8_t F1R1_FB3 = 3;              // Filter bits
    static const uint8_t F1R1_FB4 = 4;              // Filter bits
    static const uint8_t F1R1_FB5 = 5;              // Filter bits
    static const uint8_t F1R1_FB6 = 6;              // Filter bits
    static const uint8_t F1R1_FB7 = 7;              // Filter bits
    static const uint8_t F1R1_FB8 = 8;              // Filter bits
    static const uint8_t F1R1_FB9 = 9;              // Filter bits
    static const uint8_t F1R1_FB10 = 10;            // Filter bits
    static const uint8_t F1R1_FB11 = 11;            // Filter bits
    static const uint8_t F1R1_FB12 = 12;            // Filter bits
    static const uint8_t F1R1_FB13 = 13;            // Filter bits
    static const uint8_t F1R1_FB14 = 14;            // Filter bits
    static const uint8_t F1R1_FB15 = 15;            // Filter bits
    static const uint8_t F1R1_FB16 = 16;            // Filter bits
    static const uint8_t F1R1_FB17 = 17;            // Filter bits
    static const uint8_t F1R1_FB18 = 18;            // Filter bits
    static const uint8_t F1R1_FB19 = 19;            // Filter bits
    static const uint8_t F1R1_FB20 = 20;            // Filter bits
    static const uint8_t F1R1_FB21 = 21;            // Filter bits
    static const uint8_t F1R1_FB22 = 22;            // Filter bits
    static const uint8_t F1R1_FB23 = 23;            // Filter bits
    static const uint8_t F1R1_FB24 = 24;            // Filter bits
    static const uint8_t F1R1_FB25 = 25;            // Filter bits
    static const uint8_t F1R1_FB26 = 26;            // Filter bits
    static const uint8_t F1R1_FB27 = 27;            // Filter bits
    static const uint8_t F1R1_FB28 = 28;            // Filter bits
    static const uint8_t F1R1_FB29 = 29;            // Filter bits
    static const uint8_t F1R1_FB30 = 30;            // Filter bits
    static const uint8_t F1R1_FB31 = 31;            // Filter bits
    static const uint32_t F1R1_RESET_VALUE = 0x0;

    static const uint8_t F1R2_FB0 = 0;              // Filter bits
    static const uint8_t F1R2_FB1 = 1;              // Filter bits
    static const uint8_t F1R2_FB2 = 2;              // Filter bits
    static const uint8_t F1R2_FB3 = 3;              // Filter bits
    static const uint8_t F1R2_FB4 = 4;              // Filter bits
    static const uint8_t F1R2_FB5 = 5;              // Filter bits
    static const uint8_t F1R2_FB6 = 6;              // Filter bits
    static const uint8_t F1R2_FB7 = 7;              // Filter bits
    static const uint8_t F1R2_FB8 = 8;              // Filter bits
    static const uint8_t F1R2_FB9 = 9;              // Filter bits
    static const uint8_t F1R2_FB10 = 10;            // Filter bits
    static const uint8_t F1R2_FB11 = 11;            // Filter bits
    static const uint8_t F1R2_FB12 = 12;            // Filter bits
    static const uint8_t F1R2_FB13 = 13;            // Filter bits
    static const uint8_t F1R2_FB14 = 14;            // Filter bits
    static const uint8_t F1R2_FB15 = 15;            // Filter bits
    static const uint8_t F1R2_FB16 = 16;            // Filter bits
    static const uint8_t F1R2_FB17 = 17;            // Filter bits
    static const uint8_t F1R2_FB18 = 18;            // Filter bits
    static const uint8_t F1R2_FB19 = 19;            // Filter bits
    static const uint8_t F1R2_FB20 = 20;            // Filter bits
    static const uint8_t F1R2_FB21 = 21;            // Filter bits
    static const uint8_t F1R2_FB22 = 22;            // Filter bits
    static const uint8_t F1R2_FB23 = 23;            // Filter bits
    static const uint8_t F1R2_FB24 = 24;            // Filter bits
    static const uint8_t F1R2_FB25 = 25;            // Filter bits
    static const uint8_t F1R2_FB26 = 26;            // Filter bits
    static const uint8_t F1R2_FB27 = 27;            // Filter bits
    static const uint8_t F1R2_FB28 = 28;            // Filter bits
    static const uint8_t F1R2_FB29 = 29;            // Filter bits
    static const uint8_t F1R2_FB30 = 30;            // Filter bits
    static const uint8_t F1R2_FB31 = 31;            // Filter bits
    static const uint32_t F1R2_RESET_VALUE = 0x0;

    static const uint8_t F2R1_FB0 = 0;              // Filter bits
    static const uint8_t F2R1_FB1 = 1;              // Filter bits
    static const uint8_t F2R1_FB2 = 2;              // Filter bits
    static const uint8_t F2R1_FB3 = 3;              // Filter bits
    static const uint8_t F2R1_FB4 = 4;              // Filter bits
    static const uint8_t F2R1_FB5 = 5;              // Filter bits
    static const uint8_t F2R1_FB6 = 6;              // Filter bits
    static const uint8_t F2R1_FB7 = 7;              // Filter bits
    static const uint8_t F2R1_FB8 = 8;              // Filter bits
    static const uint8_t F2R1_FB9 = 9;              // Filter bits
    static const uint8_t F2R1_FB10 = 10;            // Filter bits
    static const uint8_t F2R1_FB11 = 11;            // Filter bits
    static const uint8_t F2R1_FB12 = 12;            // Filter bits
    static const uint8_t F2R1_FB13 = 13;            // Filter bits
    static const uint8_t F2R1_FB14 = 14;            // Filter bits
    static const uint8_t F2R1_FB15 = 15;            // Filter bits
    static const uint8_t F2R1_FB16 = 16;            // Filter bits
    static const uint8_t F2R1_FB17 = 17;            // Filter bits
    static const uint8_t F2R1_FB18 = 18;            // Filter bits
    static const uint8_t F2R1_FB19 = 19;            // Filter bits
    static const uint8_t F2R1_FB20 = 20;            // Filter bits
    static const uint8_t F2R1_FB21 = 21;            // Filter bits
    static const uint8_t F2R1_FB22 = 22;            // Filter bits
    static const uint8_t F2R1_FB23 = 23;            // Filter bits
    static const uint8_t F2R1_FB24 = 24;            // Filter bits
    static const uint8_t F2R1_FB25 = 25;            // Filter bits
    static const uint8_t F2R1_FB26 = 26;            // Filter bits
    static const uint8_t F2R1_FB27 = 27;            // Filter bits
    static const uint8_t F2R1_FB28 = 28;            // Filter bits
    static const uint8_t F2R1_FB29 = 29;            // Filter bits
    static const uint8_t F2R1_FB30 = 30;            // Filter bits
    static const uint8_t F2R1_FB31 = 31;            // Filter bits
    static const uint32_t F2R1_RESET_VALUE = 0x0;

    static const uint8_t F2R2_FB0 = 0;              // Filter bits
    static const uint8_t F2R2_FB1 = 1;              // Filter bits
    static const uint8_t F2R2_FB2 = 2;              // Filter bits
    static const uint8_t F2R2_FB3 = 3;              // Filter bits
    static const uint8_t F2R2_FB4 = 4;              // Filter bits
    static const uint8_t F2R2_FB5 = 5;              // Filter bits
    static const uint8_t F2R2_FB6 = 6;              // Filter bits
    static const uint8_t F2R2_FB7 = 7;              // Filter bits
    static const uint8_t F2R2_FB8 = 8;              // Filter bits
    static const uint8_t F2R2_FB9 = 9;              // Filter bits
    static const uint8_t F2R2_FB10 = 10;            // Filter bits
    static const uint8_t F2R2_FB11 = 11;            // Filter bits
    static const uint8_t F2R2_FB12 = 12;            // Filter bits
    static const uint8_t F2R2_FB13 = 13;            // Filter bits
    static const uint8_t F2R2_FB14 = 14;            // Filter bits
    static const uint8_t F2R2_FB15 = 15;            // Filter bits
    static const uint8_t F2R2_FB16 = 16;            // Filter bits
    static const uint8_t F2R2_FB17 = 17;            // Filter bits
    static const uint8_t F2R2_FB18 = 18;            // Filter bits
    static const uint8_t F2R2_FB19 = 19;            // Filter bits
    static const uint8_t F2R2_FB20 = 20;            // Filter bits
    static const uint8_t F2R2_FB21 = 21;            // Filter bits
    static const uint8_t F2R2_FB22 = 22;            // Filter bits
    static const uint8_t F2R2_FB23 = 23;            // Filter bits
    static const uint8_t F2R2_FB24 = 24;            // Filter bits
    static const uint8_t F2R2_FB25 = 25;            // Filter bits
    static const uint8_t F2R2_FB26 = 26;            // Filter bits
    static const uint8_t F2R2_FB27 = 27;            // Filter bits
    static const uint8_t F2R2_FB28 = 28;            // Filter bits
    static const uint8_t F2R2_FB29 = 29;            // Filter bits
    static const uint8_t F2R2_FB30 = 30;            // Filter bits
    static const uint8_t F2R2_FB31 = 31;            // Filter bits
    static const uint32_t F2R2_RESET_VALUE = 0x0;

    static const uint8_t F3R1_FB0 = 0;              // Filter bits
    static const uint8_t F3R1_FB1 = 1;              // Filter bits
    static const uint8_t F3R1_FB2 = 2;              // Filter bits
    static const uint8_t F3R1_FB3 = 3;              // Filter bits
    static const uint8_t F3R1_FB4 = 4;              // Filter bits
    static const uint8_t F3R1_FB5 = 5;              // Filter bits
    static const uint8_t F3R1_FB6 = 6;              // Filter bits
    static const uint8_t F3R1_FB7 = 7;              // Filter bits
    static const uint8_t F3R1_FB8 = 8;              // Filter bits
    static const uint8_t F3R1_FB9 = 9;              // Filter bits
    static const uint8_t F3R1_FB10 = 10;            // Filter bits
    static const uint8_t F3R1_FB11 = 11;            // Filter bits
    static const uint8_t F3R1_FB12 = 12;            // Filter bits
    static const uint8_t F3R1_FB13 = 13;            // Filter bits
    static const uint8_t F3R1_FB14 = 14;            // Filter bits
    static const uint8_t F3R1_FB15 = 15;            // Filter bits
    static const uint8_t F3R1_FB16 = 16;            // Filter bits
    static const uint8_t F3R1_FB17 = 17;            // Filter bits
    static const uint8_t F3R1_FB18 = 18;            // Filter bits
    static const uint8_t F3R1_FB19 = 19;            // Filter bits
    static const uint8_t F3R1_FB20 = 20;            // Filter bits
    static const uint8_t F3R1_FB21 = 21;            // Filter bits
    static const uint8_t F3R1_FB22 = 22;            // Filter bits
    static const uint8_t F3R1_FB23 = 23;            // Filter bits
    static const uint8_t F3R1_FB24 = 24;            // Filter bits
    static const uint8_t F3R1_FB25 = 25;            // Filter bits
    static const uint8_t F3R1_FB26 = 26;            // Filter bits
    static const uint8_t F3R1_FB27 = 27;            // Filter bits
    static const uint8_t F3R1_FB28 = 28;            // Filter bits
    static const uint8_t F3R1_FB29 = 29;            // Filter bits
    static const uint8_t F3R1_FB30 = 30;            // Filter bits
    static const uint8_t F3R1_FB31 = 31;            // Filter bits
    static const uint32_t F3R1_RESET_VALUE = 0x0;

    static const uint8_t F3R2_FB0 = 0;              // Filter bits
    static const uint8_t F3R2_FB1 = 1;              // Filter bits
    static const uint8_t F3R2_FB2 = 2;              // Filter bits
    static const uint8_t F3R2_FB3 = 3;              // Filter bits
    static const uint8_t F3R2_FB4 = 4;              // Filter bits
    static const uint8_t F3R2_FB5 = 5;              // Filter bits
    static const uint8_t F3R2_FB6 = 6;              // Filter bits
    static const uint8_t F3R2_FB7 = 7;              // Filter bits
    static const uint8_t F3R2_FB8 = 8;              // Filter bits
    static const uint8_t F3R2_FB9 = 9;              // Filter bits
    static const uint8_t F3R2_FB10 = 10;            // Filter bits
    static const uint8_t F3R2_FB11 = 11;            // Filter bits
    static const uint8_t F3R2_FB12 = 12;            // Filter bits
    static const uint8_t F3R2_FB13 = 13;            // Filter bits
    static const uint8_t F3R2_FB14 = 14;            // Filter bits
    static const uint8_t F3R2_FB15 = 15;            // Filter bits
    static const uint8_t F3R2_FB16 = 16;            // Filter bits
    static const uint8_t F3R2_FB17 = 17;            // Filter bits
    static const uint8_t F3R2_FB18 = 18;            // Filter bits
    static const uint8_t F3R2_FB19 = 19;            // Filter bits
    static const uint8_t F3R2_FB20 = 20;            // Filter bits
    static const uint8_t F3R2_FB21 = 21;            // Filter bits
    static const uint8_t F3R2_FB22 = 22;            // Filter bits
    static const uint8_t F3R2_FB23 = 23;            // Filter bits
    static const uint8_t F3R2_FB24 = 24;            // Filter bits
    static const uint8_t F3R2_FB25 = 25;            // Filter bits
    static const uint8_t F3R2_FB26 = 26;            // Filter bits
    static const uint8_t F3R2_FB27 = 27;            // Filter bits
    static const uint8_t F3R2_FB28 = 28;            // Filter bits
    static const uint8_t F3R2_FB29 = 29;            // Filter bits
    static const uint8_t F3R2_FB30 = 30;            // Filter bits
    static const uint8_t F3R2_FB31 = 31;            // Filter bits
    static const uint32_t F3R2_RESET_VALUE = 0x0;

    static const uint8_t F4R1_FB0 = 0;              // Filter bits
    static const uint8_t F4R1_FB1 = 1;              // Filter bits
    static const uint8_t F4R1_FB2 = 2;              // Filter bits
    static const uint8_t F4R1_FB3 = 3;              // Filter bits
    static const uint8_t F4R1_FB4 = 4;              // Filter bits
    static const uint8_t F4R1_FB5 = 5;              // Filter bits
    static const uint8_t F4R1_FB6 = 6;              // Filter bits
    static const uint8_t F4R1_FB7 = 7;              // Filter bits
    static const uint8_t F4R1_FB8 = 8;              // Filter bits
    static const uint8_t F4R1_FB9 = 9;              // Filter bits
    static const uint8_t F4R1_FB10 = 10;            // Filter bits
    static const uint8_t F4R1_FB11 = 11;            // Filter bits
    static const uint8_t F4R1_FB12 = 12;            // Filter bits
    static const uint8_t F4R1_FB13 = 13;            // Filter bits
    static const uint8_t F4R1_FB14 = 14;            // Filter bits
    static const uint8_t F4R1_FB15 = 15;            // Filter bits
    static const uint8_t F4R1_FB16 = 16;            // Filter bits
    static const uint8_t F4R1_FB17 = 17;            // Filter bits
    static const uint8_t F4R1_FB18 = 18;            // Filter bits
    static const uint8_t F4R1_FB19 = 19;            // Filter bits
    static const uint8_t F4R1_FB20 = 20;            // Filter bits
    static const uint8_t F4R1_FB21 = 21;            // Filter bits
    static const uint8_t F4R1_FB22 = 22;            // Filter bits
    static const uint8_t F4R1_FB23 = 23;            // Filter bits
    static const uint8_t F4R1_FB24 = 24;            // Filter bits
    static const uint8_t F4R1_FB25 = 25;            // Filter bits
    static const uint8_t F4R1_FB26 = 26;            // Filter bits
    static const uint8_t F4R1_FB27 = 27;            // Filter bits
    static const uint8_t F4R1_FB28 = 28;            // Filter bits
    static const uint8_t F4R1_FB29 = 29;            // Filter bits
    static const uint8_t F4R1_FB30 = 30;            // Filter bits
    static const uint8_t F4R1_FB31 = 31;            // Filter bits
    static const uint32_t F4R1_RESET_VALUE = 0x0;

    static const uint8_t F4R2_FB0 = 0;              // Filter bits
    static const uint8_t F4R2_FB1 = 1;              // Filter bits
    static const uint8_t F4R2_FB2 = 2;              // Filter bits
    static const uint8_t F4R2_FB3 = 3;              // Filter bits
    static const uint8_t F4R2_FB4 = 4;              // Filter bits
    static const uint8_t F4R2_FB5 = 5;              // Filter bits
    static const uint8_t F4R2_FB6 = 6;              // Filter bits
    static const uint8_t F4R2_FB7 = 7;              // Filter bits
    static const uint8_t F4R2_FB8 = 8;              // Filter bits
    static const uint8_t F4R2_FB9 = 9;              // Filter bits
    static const uint8_t F4R2_FB10 = 10;            // Filter bits
    static const uint8_t F4R2_FB11 = 11;            // Filter bits
    static const uint8_t F4R2_FB12 = 12;            // Filter bits
    static const uint8_t F4R2_FB13 = 13;            // Filter bits
    static const uint8_t F4R2_FB14 = 14;            // Filter bits
    static const uint8_t F4R2_FB15 = 15;            // Filter bits
    static const uint8_t F4R2_FB16 = 16;            // Filter bits
    static const uint8_t F4R2_FB17 = 17;            // Filter bits
    static const uint8_t F4R2_FB18 = 18;            // Filter bits
    static const uint8_t F4R2_FB19 = 19;            // Filter bits
    static const uint8_t F4R2_FB20 = 20;            // Filter bits
    static const uint8_t F4R2_FB21 = 21;            // Filter bits
    static const uint8_t F4R2_FB22 = 22;            // Filter bits
    static const uint8_t F4R2_FB23 = 23;            // Filter bits
    static const uint8_t F4R2_FB24 = 24;            // Filter bits
    static const uint8_t F4R2_FB25 = 25;            // Filter bits
    static const uint8_t F4R2_FB26 = 26;            // Filter bits
    static const uint8_t F4R2_FB27 = 27;            // Filter bits
    static const uint8_t F4R2_FB28 = 28;            // Filter bits
    static const uint8_t F4R2_FB29 = 29;            // Filter bits
    static const uint8_t F4R2_FB30 = 30;            // Filter bits
    static const uint8_t F4R2_FB31 = 31;            // Filter bits
    static const uint32_t F4R2_RESET_VALUE = 0x0;

    static const uint8_t F5R1_FB0 = 0;              // Filter bits
    static const uint8_t F5R1_FB1 = 1;              // Filter bits
    static const uint8_t F5R1_FB2 = 2;              // Filter bits
    static const uint8_t F5R1_FB3 = 3;              // Filter bits
    static const uint8_t F5R1_FB4 = 4;              // Filter bits
    static const uint8_t F5R1_FB5 = 5;              // Filter bits
    static const uint8_t F5R1_FB6 = 6;              // Filter bits
    static const uint8_t F5R1_FB7 = 7;              // Filter bits
    static const uint8_t F5R1_FB8 = 8;              // Filter bits
    static const uint8_t F5R1_FB9 = 9;              // Filter bits
    static const uint8_t F5R1_FB10 = 10;            // Filter bits
    static const uint8_t F5R1_FB11 = 11;            // Filter bits
    static const uint8_t F5R1_FB12 = 12;            // Filter bits
    static const uint8_t F5R1_FB13 = 13;            // Filter bits
    static const uint8_t F5R1_FB14 = 14;            // Filter bits
    static const uint8_t F5R1_FB15 = 15;            // Filter bits
    static const uint8_t F5R1_FB16 = 16;            // Filter bits
    static const uint8_t F5R1_FB17 = 17;            // Filter bits
    static const uint8_t F5R1_FB18 = 18;            // Filter bits
    static const uint8_t F5R1_FB19 = 19;            // Filter bits
    static const uint8_t F5R1_FB20 = 20;            // Filter bits
    static const uint8_t F5R1_FB21 = 21;            // Filter bits
    static const uint8_t F5R1_FB22 = 22;            // Filter bits
    static const uint8_t F5R1_FB23 = 23;            // Filter bits
    static const uint8_t F5R1_FB24 = 24;            // Filter bits
    static const uint8_t F5R1_FB25 = 25;            // Filter bits
    static const uint8_t F5R1_FB26 = 26;            // Filter bits
    static const uint8_t F5R1_FB27 = 27;            // Filter bits
    static const uint8_t F5R1_FB28 = 28;            // Filter bits
    static const uint8_t F5R1_FB29 = 29;            // Filter bits
    static const uint8_t F5R1_FB30 = 30;            // Filter bits
    static const uint8_t F5R1_FB31 = 31;            // Filter bits
    static const uint32_t F5R1_RESET_VALUE = 0x0;

    static const uint8_t F5R2_FB0 = 0;              // Filter bits
    static const uint8_t F5R2_FB1 = 1;              // Filter bits
    static const uint8_t F5R2_FB2 = 2;              // Filter bits
    static const uint8_t F5R2_FB3 = 3;              // Filter bits
    static const uint8_t F5R2_FB4 = 4;              // Filter bits
    static const uint8_t F5R2_FB5 = 5;              // Filter bits
    static const uint8_t F5R2_FB6 = 6;              // Filter bits
    static const uint8_t F5R2_FB7 = 7;              // Filter bits
    static const uint8_t F5R2_FB8 = 8;              // Filter bits
    static const uint8_t F5R2_FB9 = 9;              // Filter bits
    static const uint8_t F5R2_FB10 = 10;            // Filter bits
    static const uint8_t F5R2_FB11 = 11;            // Filter bits
    static const uint8_t F5R2_FB12 = 12;            // Filter bits
    static const uint8_t F5R2_FB13 = 13;            // Filter bits
    static const uint8_t F5R2_FB14 = 14;            // Filter bits
    static const uint8_t F5R2_FB15 = 15;            // Filter bits
    static const uint8_t F5R2_FB16 = 16;            // Filter bits
    static const uint8_t F5R2_FB17 = 17;            // Filter bits
    static const uint8_t F5R2_FB18 = 18;            // Filter bits
    static const uint8_t F5R2_FB19 = 19;            // Filter bits
    static const uint8_t F5R2_FB20 = 20;            // Filter bits
    static const uint8_t F5R2_FB21 = 21;            // Filter bits
    static const uint8_t F5R2_FB22 = 22;            // Filter bits
    static const uint8_t F5R2_FB23 = 23;            // Filter bits
    static const uint8_t F5R2_FB24 = 24;            // Filter bits
    static const uint8_t F5R2_FB25 = 25;            // Filter bits
    static const uint8_t F5R2_FB26 = 26;            // Filter bits
    static const uint8_t F5R2_FB27 = 27;            // Filter bits
    static const uint8_t F5R2_FB28 = 28;            // Filter bits
    static const uint8_t F5R2_FB29 = 29;            // Filter bits
    static const uint8_t F5R2_FB30 = 30;            // Filter bits
    static const uint8_t F5R2_FB31 = 31;            // Filter bits
    static const uint32_t F5R2_RESET_VALUE = 0x0;

    static const uint8_t F6R1_FB0 = 0;              // Filter bits
    static const uint8_t F6R1_FB1 = 1;              // Filter bits
    static const uint8_t F6R1_FB2 = 2;              // Filter bits
    static const uint8_t F6R1_FB3 = 3;              // Filter bits
    static const uint8_t F6R1_FB4 = 4;              // Filter bits
    static const uint8_t F6R1_FB5 = 5;              // Filter bits
    static const uint8_t F6R1_FB6 = 6;              // Filter bits
    static const uint8_t F6R1_FB7 = 7;              // Filter bits
    static const uint8_t F6R1_FB8 = 8;              // Filter bits
    static const uint8_t F6R1_FB9 = 9;              // Filter bits
    static const uint8_t F6R1_FB10 = 10;            // Filter bits
    static const uint8_t F6R1_FB11 = 11;            // Filter bits
    static const uint8_t F6R1_FB12 = 12;            // Filter bits
    static const uint8_t F6R1_FB13 = 13;            // Filter bits
    static const uint8_t F6R1_FB14 = 14;            // Filter bits
    static const uint8_t F6R1_FB15 = 15;            // Filter bits
    static const uint8_t F6R1_FB16 = 16;            // Filter bits
    static const uint8_t F6R1_FB17 = 17;            // Filter bits
    static const uint8_t F6R1_FB18 = 18;            // Filter bits
    static const uint8_t F6R1_FB19 = 19;            // Filter bits
    static const uint8_t F6R1_FB20 = 20;            // Filter bits
    static const uint8_t F6R1_FB21 = 21;            // Filter bits
    static const uint8_t F6R1_FB22 = 22;            // Filter bits
    static const uint8_t F6R1_FB23 = 23;            // Filter bits
    static const uint8_t F6R1_FB24 = 24;            // Filter bits
    static const uint8_t F6R1_FB25 = 25;            // Filter bits
    static const uint8_t F6R1_FB26 = 26;            // Filter bits
    static const uint8_t F6R1_FB27 = 27;            // Filter bits
    static const uint8_t F6R1_FB28 = 28;            // Filter bits
    static const uint8_t F6R1_FB29 = 29;            // Filter bits
    static const uint8_t F6R1_FB30 = 30;            // Filter bits
    static const uint8_t F6R1_FB31 = 31;            // Filter bits
    static const uint32_t F6R1_RESET_VALUE = 0x0;

    static const uint8_t F6R2_FB0 = 0;              // Filter bits
    static const uint8_t F6R2_FB1 = 1;              // Filter bits
    static const uint8_t F6R2_FB2 = 2;              // Filter bits
    static const uint8_t F6R2_FB3 = 3;              // Filter bits
    static const uint8_t F6R2_FB4 = 4;              // Filter bits
    static const uint8_t F6R2_FB5 = 5;              // Filter bits
    static const uint8_t F6R2_FB6 = 6;              // Filter bits
    static const uint8_t F6R2_FB7 = 7;              // Filter bits
    static const uint8_t F6R2_FB8 = 8;              // Filter bits
    static const uint8_t F6R2_FB9 = 9;              // Filter bits
    static const uint8_t F6R2_FB10 = 10;            // Filter bits
    static const uint8_t F6R2_FB11 = 11;            // Filter bits
    static const uint8_t F6R2_FB12 = 12;            // Filter bits
    static const uint8_t F6R2_FB13 = 13;            // Filter bits
    static const uint8_t F6R2_FB14 = 14;            // Filter bits
    static const uint8_t F6R2_FB15 = 15;            // Filter bits
    static const uint8_t F6R2_FB16 = 16;            // Filter bits
    static const uint8_t F6R2_FB17 = 17;            // Filter bits
    static const uint8_t F6R2_FB18 = 18;            // Filter bits
    static const uint8_t F6R2_FB19 = 19;            // Filter bits
    static const uint8_t F6R2_FB20 = 20;            // Filter bits
    static const uint8_t F6R2_FB21 = 21;            // Filter bits
    static const uint8_t F6R2_FB22 = 22;            // Filter bits
    static const uint8_t F6R2_FB23 = 23;            // Filter bits
    static const uint8_t F6R2_FB24 = 24;            // Filter bits
    static const uint8_t F6R2_FB25 = 25;            // Filter bits
    static const uint8_t F6R2_FB26 = 26;            // Filter bits
    static const uint8_t F6R2_FB27 = 27;            // Filter bits
    static const uint8_t F6R2_FB28 = 28;            // Filter bits
    static const uint8_t F6R2_FB29 = 29;            // Filter bits
    static const uint8_t F6R2_FB30 = 30;            // Filter bits
    static const uint8_t F6R2_FB31 = 31;            // Filter bits
    static const uint32_t F6R2_RESET_VALUE = 0x0;

    static const uint8_t F7R1_FB0 = 0;              // Filter bits
    static const uint8_t F7R1_FB1 = 1;              // Filter bits
    static const uint8_t F7R1_FB2 = 2;              // Filter bits
    static const uint8_t F7R1_FB3 = 3;              // Filter bits
    static const uint8_t F7R1_FB4 = 4;              // Filter bits
    static const uint8_t F7R1_FB5 = 5;              // Filter bits
    static const uint8_t F7R1_FB6 = 6;              // Filter bits
    static const uint8_t F7R1_FB7 = 7;              // Filter bits
    static const uint8_t F7R1_FB8 = 8;              // Filter bits
    static const uint8_t F7R1_FB9 = 9;              // Filter bits
    static const uint8_t F7R1_FB10 = 10;            // Filter bits
    static const uint8_t F7R1_FB11 = 11;            // Filter bits
    static const uint8_t F7R1_FB12 = 12;            // Filter bits
    static const uint8_t F7R1_FB13 = 13;            // Filter bits
    static const uint8_t F7R1_FB14 = 14;            // Filter bits
    static const uint8_t F7R1_FB15 = 15;            // Filter bits
    static const uint8_t F7R1_FB16 = 16;            // Filter bits
    static const uint8_t F7R1_FB17 = 17;            // Filter bits
    static const uint8_t F7R1_FB18 = 18;            // Filter bits
    static const uint8_t F7R1_FB19 = 19;            // Filter bits
    static const uint8_t F7R1_FB20 = 20;            // Filter bits
    static const uint8_t F7R1_FB21 = 21;            // Filter bits
    static const uint8_t F7R1_FB22 = 22;            // Filter bits
    static const uint8_t F7R1_FB23 = 23;            // Filter bits
    static const uint8_t F7R1_FB24 = 24;            // Filter bits
    static const uint8_t F7R1_FB25 = 25;            // Filter bits
    static const uint8_t F7R1_FB26 = 26;            // Filter bits
    static const uint8_t F7R1_FB27 = 27;            // Filter bits
    static const uint8_t F7R1_FB28 = 28;            // Filter bits
    static const uint8_t F7R1_FB29 = 29;            // Filter bits
    static const uint8_t F7R1_FB30 = 30;            // Filter bits
    static const uint8_t F7R1_FB31 = 31;            // Filter bits
    static const uint32_t F7R1_RESET_VALUE = 0x0;

    static const uint8_t F7R2_FB0 = 0;              // Filter bits
    static const uint8_t F7R2_FB1 = 1;              // Filter bits
    static const uint8_t F7R2_FB2 = 2;              // Filter bits
    static const uint8_t F7R2_FB3 = 3;              // Filter bits
    static const uint8_t F7R2_FB4 = 4;              // Filter bits
    static const uint8_t F7R2_FB5 = 5;              // Filter bits
    static const uint8_t F7R2_FB6 = 6;              // Filter bits
    static const uint8_t F7R2_FB7 = 7;              // Filter bits
    static const uint8_t F7R2_FB8 = 8;              // Filter bits
    static const uint8_t F7R2_FB9 = 9;              // Filter bits
    static const uint8_t F7R2_FB10 = 10;            // Filter bits
    static const uint8_t F7R2_FB11 = 11;            // Filter bits
    static const uint8_t F7R2_FB12 = 12;            // Filter bits
    static const uint8_t F7R2_FB13 = 13;            // Filter bits
    static const uint8_t F7R2_FB14 = 14;            // Filter bits
    static const uint8_t F7R2_FB15 = 15;            // Filter bits
    static const uint8_t F7R2_FB16 = 16;            // Filter bits
    static const uint8_t F7R2_FB17 = 17;            // Filter bits
    static const uint8_t F7R2_FB18 = 18;            // Filter bits
    static const uint8_t F7R2_FB19 = 19;            // Filter bits
    static const uint8_t F7R2_FB20 = 20;            // Filter bits
    static const uint8_t F7R2_FB21 = 21;            // Filter bits
    static const uint8_t F7R2_FB22 = 22;            // Filter bits
    static const uint8_t F7R2_FB23 = 23;            // Filter bits
    static const uint8_t F7R2_FB24 = 24;            // Filter bits
    static const uint8_t F7R2_FB25 = 25;            // Filter bits
    static const uint8_t F7R2_FB26 = 26;            // Filter bits
    static const uint8_t F7R2_FB27 = 27;            // Filter bits
    static const uint8_t F7R2_FB28 = 28;            // Filter bits
    static const uint8_t F7R2_FB29 = 29;            // Filter bits
    static const uint8_t F7R2_FB30 = 30;            // Filter bits
    static const uint8_t F7R2_FB31 = 31;            // Filter bits
    static const uint32_t F7R2_RESET_VALUE = 0x0;

    static const uint8_t F8R1_FB0 = 0;              // Filter bits
    static const uint8_t F8R1_FB1 = 1;              // Filter bits
    static const uint8_t F8R1_FB2 = 2;              // Filter bits
    static const uint8_t F8R1_FB3 = 3;              // Filter bits
    static const uint8_t F8R1_FB4 = 4;              // Filter bits
    static const uint8_t F8R1_FB5 = 5;              // Filter bits
    static const uint8_t F8R1_FB6 = 6;              // Filter bits
    static const uint8_t F8R1_FB7 = 7;              // Filter bits
    static const uint8_t F8R1_FB8 = 8;              // Filter bits
    static const uint8_t F8R1_FB9 = 9;              // Filter bits
    static const uint8_t F8R1_FB10 = 10;            // Filter bits
    static const uint8_t F8R1_FB11 = 11;            // Filter bits
    static const uint8_t F8R1_FB12 = 12;            // Filter bits
    static const uint8_t F8R1_FB13 = 13;            // Filter bits
    static const uint8_t F8R1_FB14 = 14;            // Filter bits
    static const uint8_t F8R1_FB15 = 15;            // Filter bits
    static const uint8_t F8R1_FB16 = 16;            // Filter bits
    static const uint8_t F8R1_FB17 = 17;            // Filter bits
    static const uint8_t F8R1_FB18 = 18;            // Filter bits
    static const uint8_t F8R1_FB19 = 19;            // Filter bits
    static const uint8_t F8R1_FB20 = 20;            // Filter bits
    static const uint8_t F8R1_FB21 = 21;            // Filter bits
    static const uint8_t F8R1_FB22 = 22;            // Filter bits
    static const uint8_t F8R1_FB23 = 23;            // Filter bits
    static const uint8_t F8R1_FB24 = 24;            // Filter bits
    static const uint8_t F8R1_FB25 = 25;            // Filter bits
    static const uint8_t F8R1_FB26 = 26;            // Filter bits
    static const uint8_t F8R1_FB27 = 27;            // Filter bits
    static const uint8_t F8R1_FB28 = 28;            // Filter bits
    static const uint8_t F8R1_FB29 = 29;            // Filter bits
    static const uint8_t F8R1_FB30 = 30;            // Filter bits
    static const uint8_t F8R1_FB31 = 31;            // Filter bits
    static const uint32_t F8R1_RESET_VALUE = 0x0;

    static const uint8_t F8R2_FB0 = 0;              // Filter bits
    static const uint8_t F8R2_FB1 = 1;              // Filter bits
    static const uint8_t F8R2_FB2 = 2;              // Filter bits
    static const uint8_t F8R2_FB3 = 3;              // Filter bits
    static const uint8_t F8R2_FB4 = 4;              // Filter bits
    static const uint8_t F8R2_FB5 = 5;              // Filter bits
    static const uint8_t F8R2_FB6 = 6;              // Filter bits
    static const uint8_t F8R2_FB7 = 7;              // Filter bits
    static const uint8_t F8R2_FB8 = 8;              // Filter bits
    static const uint8_t F8R2_FB9 = 9;              // Filter bits
    static const uint8_t F8R2_FB10 = 10;            // Filter bits
    static const uint8_t F8R2_FB11 = 11;            // Filter bits
    static const uint8_t F8R2_FB12 = 12;            // Filter bits
    static const uint8_t F8R2_FB13 = 13;            // Filter bits
    static const uint8_t F8R2_FB14 = 14;            // Filter bits
    static const uint8_t F8R2_FB15 = 15;            // Filter bits
    static const uint8_t F8R2_FB16 = 16;            // Filter bits
    static const uint8_t F8R2_FB17 = 17;            // Filter bits
    static const uint8_t F8R2_FB18 = 18;            // Filter bits
    static const uint8_t F8R2_FB19 = 19;            // Filter bits
    static const uint8_t F8R2_FB20 = 20;            // Filter bits
    static const uint8_t F8R2_FB21 = 21;            // Filter bits
    static const uint8_t F8R2_FB22 = 22;            // Filter bits
    static const uint8_t F8R2_FB23 = 23;            // Filter bits
    static const uint8_t F8R2_FB24 = 24;            // Filter bits
    static const uint8_t F8R2_FB25 = 25;            // Filter bits
    static const uint8_t F8R2_FB26 = 26;            // Filter bits
    static const uint8_t F8R2_FB27 = 27;            // Filter bits
    static const uint8_t F8R2_FB28 = 28;            // Filter bits
    static const uint8_t F8R2_FB29 = 29;            // Filter bits
    static const uint8_t F8R2_FB30 = 30;            // Filter bits
    static const uint8_t F8R2_FB31 = 31;            // Filter bits
    static const uint32_t F8R2_RESET_VALUE = 0x0;

    static const uint8_t F9R1_FB0 = 0;              // Filter bits
    static const uint8_t F9R1_FB1 = 1;              // Filter bits
    static const uint8_t F9R1_FB2 = 2;              // Filter bits
    static const uint8_t F9R1_FB3 = 3;              // Filter bits
    static const uint8_t F9R1_FB4 = 4;              // Filter bits
    static const uint8_t F9R1_FB5 = 5;              // Filter bits
    static const uint8_t F9R1_FB6 = 6;              // Filter bits
    static const uint8_t F9R1_FB7 = 7;              // Filter bits
    static const uint8_t F9R1_FB8 = 8;              // Filter bits
    static const uint8_t F9R1_FB9 = 9;              // Filter bits
    static const uint8_t F9R1_FB10 = 10;            // Filter bits
    static const uint8_t F9R1_FB11 = 11;            // Filter bits
    static const uint8_t F9R1_FB12 = 12;            // Filter bits
    static const uint8_t F9R1_FB13 = 13;            // Filter bits
    static const uint8_t F9R1_FB14 = 14;            // Filter bits
    static const uint8_t F9R1_FB15 = 15;            // Filter bits
    static const uint8_t F9R1_FB16 = 16;            // Filter bits
    static const uint8_t F9R1_FB17 = 17;            // Filter bits
    static const uint8_t F9R1_FB18 = 18;            // Filter bits
    static const uint8_t F9R1_FB19 = 19;            // Filter bits
    static const uint8_t F9R1_FB20 = 20;            // Filter bits
    static const uint8_t F9R1_FB21 = 21;            // Filter bits
    static const uint8_t F9R1_FB22 = 22;            // Filter bits
    static const uint8_t F9R1_FB23 = 23;            // Filter bits
    static const uint8_t F9R1_FB24 = 24;            // Filter bits
    static const uint8_t F9R1_FB25 = 25;            // Filter bits
    static const uint8_t F9R1_FB26 = 26;            // Filter bits
    static const uint8_t F9R1_FB27 = 27;            // Filter bits
    static const uint8_t F9R1_FB28 = 28;            // Filter bits
    static const uint8_t F9R1_FB29 = 29;            // Filter bits
    static const uint8_t F9R1_FB30 = 30;            // Filter bits
    static const uint8_t F9R1_FB31 = 31;            // Filter bits
    static const uint32_t F9R1_RESET_VALUE = 0x0;

    static const uint8_t F9R2_FB0 = 0;              // Filter bits
    static const uint8_t F9R2_FB1 = 1;              // Filter bits
    static const uint8_t F9R2_FB2 = 2;              // Filter bits
    static const uint8_t F9R2_FB3 = 3;              // Filter bits
    static const uint8_t F9R2_FB4 = 4;              // Filter bits
    static const uint8_t F9R2_FB5 = 5;              // Filter bits
    static const uint8_t F9R2_FB6 = 6;              // Filter bits
    static const uint8_t F9R2_FB7 = 7;              // Filter bits
    static const uint8_t F9R2_FB8 = 8;              // Filter bits
    static const uint8_t F9R2_FB9 = 9;              // Filter bits
    static const uint8_t F9R2_FB10 = 10;            // Filter bits
    static const uint8_t F9R2_FB11 = 11;            // Filter bits
    static const uint8_t F9R2_FB12 = 12;            // Filter bits
    static const uint8_t F9R2_FB13 = 13;            // Filter bits
    static const uint8_t F9R2_FB14 = 14;            // Filter bits
    static const uint8_t F9R2_FB15 = 15;            // Filter bits
    static const uint8_t F9R2_FB16 = 16;            // Filter bits
    static const uint8_t F9R2_FB17 = 17;            // Filter bits
    static const uint8_t F9R2_FB18 = 18;            // Filter bits
    static const uint8_t F9R2_FB19 = 19;            // Filter bits
    static const uint8_t F9R2_FB20 = 20;            // Filter bits
    static const uint8_t F9R2_FB21 = 21;            // Filter bits
    static const uint8_t F9R2_FB22 = 22;            // Filter bits
    static const uint8_t F9R2_FB23 = 23;            // Filter bits
    static const uint8_t F9R2_FB24 = 24;            // Filter bits
    static const uint8_t F9R2_FB25 = 25;            // Filter bits
    static const uint8_t F9R2_FB26 = 26;            // Filter bits
    static const uint8_t F9R2_FB27 = 27;            // Filter bits
    static const uint8_t F9R2_FB28 = 28;            // Filter bits
    static const uint8_t F9R2_FB29 = 29;            // Filter bits
    static const uint8_t F9R2_FB30 = 30;            // Filter bits
    static const uint8_t F9R2_FB31 = 31;            // Filter bits
    static const uint32_t F9R2_RESET_VALUE = 0x0;

    static const uint8_t F10R1_FB0 = 0;              // Filter bits
    static const uint8_t F10R1_FB1 = 1;              // Filter bits
    static const uint8_t F10R1_FB2 = 2;              // Filter bits
    static const uint8_t F10R1_FB3 = 3;              // Filter bits
    static const uint8_t F10R1_FB4 = 4;              // Filter bits
    static const uint8_t F10R1_FB5 = 5;              // Filter bits
    static const uint8_t F10R1_FB6 = 6;              // Filter bits
    static const uint8_t F10R1_FB7 = 7;              // Filter bits
    static const uint8_t F10R1_FB8 = 8;              // Filter bits
    static const uint8_t F10R1_FB9 = 9;              // Filter bits
    static const uint8_t F10R1_FB10 = 10;            // Filter bits
    static const uint8_t F10R1_FB11 = 11;            // Filter bits
    static const uint8_t F10R1_FB12 = 12;            // Filter bits
    static const uint8_t F10R1_FB13 = 13;            // Filter bits
    static const uint8_t F10R1_FB14 = 14;            // Filter bits
    static const uint8_t F10R1_FB15 = 15;            // Filter bits
    static const uint8_t F10R1_FB16 = 16;            // Filter bits
    static const uint8_t F10R1_FB17 = 17;            // Filter bits
    static const uint8_t F10R1_FB18 = 18;            // Filter bits
    static const uint8_t F10R1_FB19 = 19;            // Filter bits
    static const uint8_t F10R1_FB20 = 20;            // Filter bits
    static const uint8_t F10R1_FB21 = 21;            // Filter bits
    static const uint8_t F10R1_FB22 = 22;            // Filter bits
    static const uint8_t F10R1_FB23 = 23;            // Filter bits
    static const uint8_t F10R1_FB24 = 24;            // Filter bits
    static const uint8_t F10R1_FB25 = 25;            // Filter bits
    static const uint8_t F10R1_FB26 = 26;            // Filter bits
    static const uint8_t F10R1_FB27 = 27;            // Filter bits
    static const uint8_t F10R1_FB28 = 28;            // Filter bits
    static const uint8_t F10R1_FB29 = 29;            // Filter bits
    static const uint8_t F10R1_FB30 = 30;            // Filter bits
    static const uint8_t F10R1_FB31 = 31;            // Filter bits
    static const uint32_t F10R1_RESET_VALUE = 0x0;

    static const uint8_t F10R2_FB0 = 0;              // Filter bits
    static const uint8_t F10R2_FB1 = 1;              // Filter bits
    static const uint8_t F10R2_FB2 = 2;              // Filter bits
    static const uint8_t F10R2_FB3 = 3;              // Filter bits
    static const uint8_t F10R2_FB4 = 4;              // Filter bits
    static const uint8_t F10R2_FB5 = 5;              // Filter bits
    static const uint8_t F10R2_FB6 = 6;              // Filter bits
    static const uint8_t F10R2_FB7 = 7;              // Filter bits
    static const uint8_t F10R2_FB8 = 8;              // Filter bits
    static const uint8_t F10R2_FB9 = 9;              // Filter bits
    static const uint8_t F10R2_FB10 = 10;            // Filter bits
    static const uint8_t F10R2_FB11 = 11;            // Filter bits
    static const uint8_t F10R2_FB12 = 12;            // Filter bits
    static const uint8_t F10R2_FB13 = 13;            // Filter bits
    static const uint8_t F10R2_FB14 = 14;            // Filter bits
    static const uint8_t F10R2_FB15 = 15;            // Filter bits
    static const uint8_t F10R2_FB16 = 16;            // Filter bits
    static const uint8_t F10R2_FB17 = 17;            // Filter bits
    static const uint8_t F10R2_FB18 = 18;            // Filter bits
    static const uint8_t F10R2_FB19 = 19;            // Filter bits
    static const uint8_t F10R2_FB20 = 20;            // Filter bits
    static const uint8_t F10R2_FB21 = 21;            // Filter bits
    static const uint8_t F10R2_FB22 = 22;            // Filter bits
    static const uint8_t F10R2_FB23 = 23;            // Filter bits
    static const uint8_t F10R2_FB24 = 24;            // Filter bits
    static const uint8_t F10R2_FB25 = 25;            // Filter bits
    static const uint8_t F10R2_FB26 = 26;            // Filter bits
    static const uint8_t F10R2_FB27 = 27;            // Filter bits
    static const uint8_t F10R2_FB28 = 28;            // Filter bits
    static const uint8_t F10R2_FB29 = 29;            // Filter bits
    static const uint8_t F10R2_FB30 = 30;            // Filter bits
    static const uint8_t F10R2_FB31 = 31;            // Filter bits
    static const uint32_t F10R2_RESET_VALUE = 0x0;

    static const uint8_t F11R1_FB0 = 0;              // Filter bits
    static const uint8_t F11R1_FB1 = 1;              // Filter bits
    static const uint8_t F11R1_FB2 = 2;              // Filter bits
    static const uint8_t F11R1_FB3 = 3;              // Filter bits
    static const uint8_t F11R1_FB4 = 4;              // Filter bits
    static const uint8_t F11R1_FB5 = 5;              // Filter bits
    static const uint8_t F11R1_FB6 = 6;              // Filter bits
    static const uint8_t F11R1_FB7 = 7;              // Filter bits
    static const uint8_t F11R1_FB8 = 8;              // Filter bits
    static const uint8_t F11R1_FB9 = 9;              // Filter bits
    static const uint8_t F11R1_FB10 = 10;            // Filter bits
    static const uint8_t F11R1_FB11 = 11;            // Filter bits
    static const uint8_t F11R1_FB12 = 12;            // Filter bits
    static const uint8_t F11R1_FB13 = 13;            // Filter bits
    static const uint8_t F11R1_FB14 = 14;            // Filter bits
    static const uint8_t F11R1_FB15 = 15;            // Filter bits
    static const uint8_t F11R1_FB16 = 16;            // Filter bits
    static const uint8_t F11R1_FB17 = 17;            // Filter bits
    static const uint8_t F11R1_FB18 = 18;            // Filter bits
    static const uint8_t F11R1_FB19 = 19;            // Filter bits
    static const uint8_t F11R1_FB20 = 20;            // Filter bits
    static const uint8_t F11R1_FB21 = 21;            // Filter bits
    static const uint8_t F11R1_FB22 = 22;            // Filter bits
    static const uint8_t F11R1_FB23 = 23;            // Filter bits
    static const uint8_t F11R1_FB24 = 24;            // Filter bits
    static const uint8_t F11R1_FB25 = 25;            // Filter bits
    static const uint8_t F11R1_FB26 = 26;            // Filter bits
    static const uint8_t F11R1_FB27 = 27;            // Filter bits
    static const uint8_t F11R1_FB28 = 28;            // Filter bits
    static const uint8_t F11R1_FB29 = 29;            // Filter bits
    static const uint8_t F11R1_FB30 = 30;            // Filter bits
    static const uint8_t F11R1_FB31 = 31;            // Filter bits
    static const uint32_t F11R1_RESET_VALUE = 0x0;

    static const uint8_t F11R2_FB0 = 0;              // Filter bits
    static const uint8_t F11R2_FB1 = 1;              // Filter bits
    static const uint8_t F11R2_FB2 = 2;              // Filter bits
    static const uint8_t F11R2_FB3 = 3;              // Filter bits
    static const uint8_t F11R2_FB4 = 4;              // Filter bits
    static const uint8_t F11R2_FB5 = 5;              // Filter bits
    static const uint8_t F11R2_FB6 = 6;              // Filter bits
    static const uint8_t F11R2_FB7 = 7;              // Filter bits
    static const uint8_t F11R2_FB8 = 8;              // Filter bits
    static const uint8_t F11R2_FB9 = 9;              // Filter bits
    static const uint8_t F11R2_FB10 = 10;            // Filter bits
    static const uint8_t F11R2_FB11 = 11;            // Filter bits
    static const uint8_t F11R2_FB12 = 12;            // Filter bits
    static const uint8_t F11R2_FB13 = 13;            // Filter bits
    static const uint8_t F11R2_FB14 = 14;            // Filter bits
    static const uint8_t F11R2_FB15 = 15;            // Filter bits
    static const uint8_t F11R2_FB16 = 16;            // Filter bits
    static const uint8_t F11R2_FB17 = 17;            // Filter bits
    static const uint8_t F11R2_FB18 = 18;            // Filter bits
    static const uint8_t F11R2_FB19 = 19;            // Filter bits
    static const uint8_t F11R2_FB20 = 20;            // Filter bits
    static const uint8_t F11R2_FB21 = 21;            // Filter bits
    static const uint8_t F11R2_FB22 = 22;            // Filter bits
    static const uint8_t F11R2_FB23 = 23;            // Filter bits
    static const uint8_t F11R2_FB24 = 24;            // Filter bits
    static const uint8_t F11R2_FB25 = 25;            // Filter bits
    static const uint8_t F11R2_FB26 = 26;            // Filter bits
    static const uint8_t F11R2_FB27 = 27;            // Filter bits
    static const uint8_t F11R2_FB28 = 28;            // Filter bits
    static const uint8_t F11R2_FB29 = 29;            // Filter bits
    static const uint8_t F11R2_FB30 = 30;            // Filter bits
    static const uint8_t F11R2_FB31 = 31;            // Filter bits
    static const uint32_t F11R2_RESET_VALUE = 0x0;

    static const uint8_t F12R1_FB0 = 0;              // Filter bits
    static const uint8_t F12R1_FB1 = 1;              // Filter bits
    static const uint8_t F12R1_FB2 = 2;              // Filter bits
    static const uint8_t F12R1_FB3 = 3;              // Filter bits
    static const uint8_t F12R1_FB4 = 4;              // Filter bits
    static const uint8_t F12R1_FB5 = 5;              // Filter bits
    static const uint8_t F12R1_FB6 = 6;              // Filter bits
    static const uint8_t F12R1_FB7 = 7;              // Filter bits
    static const uint8_t F12R1_FB8 = 8;              // Filter bits
    static const uint8_t F12R1_FB9 = 9;              // Filter bits
    static const uint8_t F12R1_FB10 = 10;            // Filter bits
    static const uint8_t F12R1_FB11 = 11;            // Filter bits
    static const uint8_t F12R1_FB12 = 12;            // Filter bits
    static const uint8_t F12R1_FB13 = 13;            // Filter bits
    static const uint8_t F12R1_FB14 = 14;            // Filter bits
    static const uint8_t F12R1_FB15 = 15;            // Filter bits
    static const uint8_t F12R1_FB16 = 16;            // Filter bits
    static const uint8_t F12R1_FB17 = 17;            // Filter bits
    static const uint8_t F12R1_FB18 = 18;            // Filter bits
    static const uint8_t F12R1_FB19 = 19;            // Filter bits
    static const uint8_t F12R1_FB20 = 20;            // Filter bits
    static const uint8_t F12R1_FB21 = 21;            // Filter bits
    static const uint8_t F12R1_FB22 = 22;            // Filter bits
    static const uint8_t F12R1_FB23 = 23;            // Filter bits
    static const uint8_t F12R1_FB24 = 24;            // Filter bits
    static const uint8_t F12R1_FB25 = 25;            // Filter bits
    static const uint8_t F12R1_FB26 = 26;            // Filter bits
    static const uint8_t F12R1_FB27 = 27;            // Filter bits
    static const uint8_t F12R1_FB28 = 28;            // Filter bits
    static const uint8_t F12R1_FB29 = 29;            // Filter bits
    static const uint8_t F12R1_FB30 = 30;            // Filter bits
    static const uint8_t F12R1_FB31 = 31;            // Filter bits
    static const uint32_t F12R1_RESET_VALUE = 0x0;

    static const uint8_t F12R2_FB0 = 0;              // Filter bits
    static const uint8_t F12R2_FB1 = 1;              // Filter bits
    static const uint8_t F12R2_FB2 = 2;              // Filter bits
    static const uint8_t F12R2_FB3 = 3;              // Filter bits
    static const uint8_t F12R2_FB4 = 4;              // Filter bits
    static const uint8_t F12R2_FB5 = 5;              // Filter bits
    static const uint8_t F12R2_FB6 = 6;              // Filter bits
    static const uint8_t F12R2_FB7 = 7;              // Filter bits
    static const uint8_t F12R2_FB8 = 8;              // Filter bits
    static const uint8_t F12R2_FB9 = 9;              // Filter bits
    static const uint8_t F12R2_FB10 = 10;            // Filter bits
    static const uint8_t F12R2_FB11 = 11;            // Filter bits
    static const uint8_t F12R2_FB12 = 12;            // Filter bits
    static const uint8_t F12R2_FB13 = 13;            // Filter bits
    static const uint8_t F12R2_FB14 = 14;            // Filter bits
    static const uint8_t F12R2_FB15 = 15;            // Filter bits
    static const uint8_t F12R2_FB16 = 16;            // Filter bits
    static const uint8_t F12R2_FB17 = 17;            // Filter bits
    static const uint8_t F12R2_FB18 = 18;            // Filter bits
    static const uint8_t F12R2_FB19 = 19;            // Filter bits
    static const uint8_t F12R2_FB20 = 20;            // Filter bits
    static const uint8_t F12R2_FB21 = 21;            // Filter bits
    static const uint8_t F12R2_FB22 = 22;            // Filter bits
    static const uint8_t F12R2_FB23 = 23;            // Filter bits
    static const uint8_t F12R2_FB24 = 24;            // Filter bits
    static const uint8_t F12R2_FB25 = 25;            // Filter bits
    static const uint8_t F12R2_FB26 = 26;            // Filter bits
    static const uint8_t F12R2_FB27 = 27;            // Filter bits
    static const uint8_t F12R2_FB28 = 28;            // Filter bits
    static const uint8_t F12R2_FB29 = 29;            // Filter bits
    static const uint8_t F12R2_FB30 = 30;            // Filter bits
    static const uint8_t F12R2_FB31 = 31;            // Filter bits
    static const uint32_t F12R2_RESET_VALUE = 0x0;

    static const uint8_t F13R1_FB0 = 0;              // Filter bits
    static const uint8_t F13R1_FB1 = 1;              // Filter bits
    static const uint8_t F13R1_FB2 = 2;              // Filter bits
    static const uint8_t F13R1_FB3 = 3;              // Filter bits
    static const uint8_t F13R1_FB4 = 4;              // Filter bits
    static const uint8_t F13R1_FB5 = 5;              // Filter bits
    static const uint8_t F13R1_FB6 = 6;              // Filter bits
    static const uint8_t F13R1_FB7 = 7;              // Filter bits
    static const uint8_t F13R1_FB8 = 8;              // Filter bits
    static const uint8_t F13R1_FB9 = 9;              // Filter bits
    static const uint8_t F13R1_FB10 = 10;            // Filter bits
    static const uint8_t F13R1_FB11 = 11;            // Filter bits
    static const uint8_t F13R1_FB12 = 12;            // Filter bits
    static const uint8_t F13R1_FB13 = 13;            // Filter bits
    static const uint8_t F13R1_FB14 = 14;            // Filter bits
    static const uint8_t F13R1_FB15 = 15;            // Filter bits
    static const uint8_t F13R1_FB16 = 16;            // Filter bits
    static const uint8_t F13R1_FB17 = 17;            // Filter bits
    static const uint8_t F13R1_FB18 = 18;            // Filter bits
    static const uint8_t F13R1_FB19 = 19;            // Filter bits
    static const uint8_t F13R1_FB20 = 20;            // Filter bits
    static const uint8_t F13R1_FB21 = 21;            // Filter bits
    static const uint8_t F13R1_FB22 = 22;            // Filter bits
    static const uint8_t F13R1_FB23 = 23;            // Filter bits
    static const uint8_t F13R1_FB24 = 24;            // Filter bits
    static const uint8_t F13R1_FB25 = 25;            // Filter bits
    static const uint8_t F13R1_FB26 = 26;            // Filter bits
    static const uint8_t F13R1_FB27 = 27;            // Filter bits
    static const uint8_t F13R1_FB28 = 28;            // Filter bits
    static const uint8_t F13R1_FB29 = 29;            // Filter bits
    static const uint8_t F13R1_FB30 = 30;            // Filter bits
    static const uint8_t F13R1_FB31 = 31;            // Filter bits
    static const uint32_t F13R1_RESET_VALUE = 0x0;

    static const uint8_t F13R2_FB0 = 0;              // Filter bits
    static const uint8_t F13R2_FB1 = 1;              // Filter bits
    static const uint8_t F13R2_FB2 = 2;              // Filter bits
    static const uint8_t F13R2_FB3 = 3;              // Filter bits
    static const uint8_t F13R2_FB4 = 4;              // Filter bits
    static const uint8_t F13R2_FB5 = 5;              // Filter bits
    static const uint8_t F13R2_FB6 = 6;              // Filter bits
    static const uint8_t F13R2_FB7 = 7;              // Filter bits
    static const uint8_t F13R2_FB8 = 8;              // Filter bits
    static const uint8_t F13R2_FB9 = 9;              // Filter bits
    static const uint8_t F13R2_FB10 = 10;            // Filter bits
    static const uint8_t F13R2_FB11 = 11;            // Filter bits
    static const uint8_t F13R2_FB12 = 12;            // Filter bits
    static const uint8_t F13R2_FB13 = 13;            // Filter bits
    static const uint8_t F13R2_FB14 = 14;            // Filter bits
    static const uint8_t F13R2_FB15 = 15;            // Filter bits
    static const uint8_t F13R2_FB16 = 16;            // Filter bits
    static const uint8_t F13R2_FB17 = 17;            // Filter bits
    static const uint8_t F13R2_FB18 = 18;            // Filter bits
    static const uint8_t F13R2_FB19 = 19;            // Filter bits
    static const uint8_t F13R2_FB20 = 20;            // Filter bits
    static const uint8_t F13R2_FB21 = 21;            // Filter bits
    static const uint8_t F13R2_FB22 = 22;            // Filter bits
    static const uint8_t F13R2_FB23 = 23;            // Filter bits
    static const uint8_t F13R2_FB24 = 24;            // Filter bits
    static const uint8_t F13R2_FB25 = 25;            // Filter bits
    static const uint8_t F13R2_FB26 = 26;            // Filter bits
    static const uint8_t F13R2_FB27 = 27;            // Filter bits
    static const uint8_t F13R2_FB28 = 28;            // Filter bits
    static const uint8_t F13R2_FB29 = 29;            // Filter bits
    static const uint8_t F13R2_FB30 = 30;            // Filter bits
    static const uint8_t F13R2_FB31 = 31;            // Filter bits
    static const uint32_t F13R2_RESET_VALUE = 0x0;

    static const uint8_t F14R1_FB0 = 0;              // Filter bits
    static const uint8_t F14R1_FB1 = 1;              // Filter bits
    static const uint8_t F14R1_FB2 = 2;              // Filter bits
    static const uint8_t F14R1_FB3 = 3;              // Filter bits
    static const uint8_t F14R1_FB4 = 4;              // Filter bits
    static const uint8_t F14R1_FB5 = 5;              // Filter bits
    static const uint8_t F14R1_FB6 = 6;              // Filter bits
    static const uint8_t F14R1_FB7 = 7;              // Filter bits
    static const uint8_t F14R1_FB8 = 8;              // Filter bits
    static const uint8_t F14R1_FB9 = 9;              // Filter bits
    static const uint8_t F14R1_FB10 = 10;            // Filter bits
    static const uint8_t F14R1_FB11 = 11;            // Filter bits
    static const uint8_t F14R1_FB12 = 12;            // Filter bits
    static const uint8_t F14R1_FB13 = 13;            // Filter bits
    static const uint8_t F14R1_FB14 = 14;            // Filter bits
    static const uint8_t F14R1_FB15 = 15;            // Filter bits
    static const uint8_t F14R1_FB16 = 16;            // Filter bits
    static const uint8_t F14R1_FB17 = 17;            // Filter bits
    static const uint8_t F14R1_FB18 = 18;            // Filter bits
    static const uint8_t F14R1_FB19 = 19;            // Filter bits
    static const uint8_t F14R1_FB20 = 20;            // Filter bits
    static const uint8_t F14R1_FB21 = 21;            // Filter bits
    static const uint8_t F14R1_FB22 = 22;            // Filter bits
    static const uint8_t F14R1_FB23 = 23;            // Filter bits
    static const uint8_t F14R1_FB24 = 24;            // Filter bits
    static const uint8_t F14R1_FB25 = 25;            // Filter bits
    static const uint8_t F14R1_FB26 = 26;            // Filter bits
    static const uint8_t F14R1_FB27 = 27;            // Filter bits
    static const uint8_t F14R1_FB28 = 28;            // Filter bits
    static const uint8_t F14R1_FB29 = 29;            // Filter bits
    static const uint8_t F14R1_FB30 = 30;            // Filter bits
    static const uint8_t F14R1_FB31 = 31;            // Filter bits
    static const uint32_t F14R1_RESET_VALUE = 0x0;

    static const uint8_t F14R2_FB0 = 0;              // Filter bits
    static const uint8_t F14R2_FB1 = 1;              // Filter bits
    static const uint8_t F14R2_FB2 = 2;              // Filter bits
    static const uint8_t F14R2_FB3 = 3;              // Filter bits
    static const uint8_t F14R2_FB4 = 4;              // Filter bits
    static const uint8_t F14R2_FB5 = 5;              // Filter bits
    static const uint8_t F14R2_FB6 = 6;              // Filter bits
    static const uint8_t F14R2_FB7 = 7;              // Filter bits
    static const uint8_t F14R2_FB8 = 8;              // Filter bits
    static const uint8_t F14R2_FB9 = 9;              // Filter bits
    static const uint8_t F14R2_FB10 = 10;            // Filter bits
    static const uint8_t F14R2_FB11 = 11;            // Filter bits
    static const uint8_t F14R2_FB12 = 12;            // Filter bits
    static const uint8_t F14R2_FB13 = 13;            // Filter bits
    static const uint8_t F14R2_FB14 = 14;            // Filter bits
    static const uint8_t F14R2_FB15 = 15;            // Filter bits
    static const uint8_t F14R2_FB16 = 16;            // Filter bits
    static const uint8_t F14R2_FB17 = 17;            // Filter bits
    static const uint8_t F14R2_FB18 = 18;            // Filter bits
    static const uint8_t F14R2_FB19 = 19;            // Filter bits
    static const uint8_t F14R2_FB20 = 20;            // Filter bits
    static const uint8_t F14R2_FB21 = 21;            // Filter bits
    static const uint8_t F14R2_FB22 = 22;            // Filter bits
    static const uint8_t F14R2_FB23 = 23;            // Filter bits
    static const uint8_t F14R2_FB24 = 24;            // Filter bits
    static const uint8_t F14R2_FB25 = 25;            // Filter bits
    static const uint8_t F14R2_FB26 = 26;            // Filter bits
    static const uint8_t F14R2_FB27 = 27;            // Filter bits
    static const uint8_t F14R2_FB28 = 28;            // Filter bits
    static const uint8_t F14R2_FB29 = 29;            // Filter bits
    static const uint8_t F14R2_FB30 = 30;            // Filter bits
    static const uint8_t F14R2_FB31 = 31;            // Filter bits
    static const uint32_t F14R2_RESET_VALUE = 0x0;

    static const uint8_t F15R1_FB0 = 0;              // Filter bits
    static const uint8_t F15R1_FB1 = 1;              // Filter bits
    static const uint8_t F15R1_FB2 = 2;              // Filter bits
    static const uint8_t F15R1_FB3 = 3;              // Filter bits
    static const uint8_t F15R1_FB4 = 4;              // Filter bits
    static const uint8_t F15R1_FB5 = 5;              // Filter bits
    static const uint8_t F15R1_FB6 = 6;              // Filter bits
    static const uint8_t F15R1_FB7 = 7;              // Filter bits
    static const uint8_t F15R1_FB8 = 8;              // Filter bits
    static const uint8_t F15R1_FB9 = 9;              // Filter bits
    static const uint8_t F15R1_FB10 = 10;            // Filter bits
    static const uint8_t F15R1_FB11 = 11;            // Filter bits
    static const uint8_t F15R1_FB12 = 12;            // Filter bits
    static const uint8_t F15R1_FB13 = 13;            // Filter bits
    static const uint8_t F15R1_FB14 = 14;            // Filter bits
    static const uint8_t F15R1_FB15 = 15;            // Filter bits
    static const uint8_t F15R1_FB16 = 16;            // Filter bits
    static const uint8_t F15R1_FB17 = 17;            // Filter bits
    static const uint8_t F15R1_FB18 = 18;            // Filter bits
    static const uint8_t F15R1_FB19 = 19;            // Filter bits
    static const uint8_t F15R1_FB20 = 20;            // Filter bits
    static const uint8_t F15R1_FB21 = 21;            // Filter bits
    static const uint8_t F15R1_FB22 = 22;            // Filter bits
    static const uint8_t F15R1_FB23 = 23;            // Filter bits
    static const uint8_t F15R1_FB24 = 24;            // Filter bits
    static const uint8_t F15R1_FB25 = 25;            // Filter bits
    static const uint8_t F15R1_FB26 = 26;            // Filter bits
    static const uint8_t F15R1_FB27 = 27;            // Filter bits
    static const uint8_t F15R1_FB28 = 28;            // Filter bits
    static const uint8_t F15R1_FB29 = 29;            // Filter bits
    static const uint8_t F15R1_FB30 = 30;            // Filter bits
    static const uint8_t F15R1_FB31 = 31;            // Filter bits
    static const uint32_t F15R1_RESET_VALUE = 0x0;

    static const uint8_t F15R2_FB0 = 0;              // Filter bits
    static const uint8_t F15R2_FB1 = 1;              // Filter bits
    static const uint8_t F15R2_FB2 = 2;              // Filter bits
    static const uint8_t F15R2_FB3 = 3;              // Filter bits
    static const uint8_t F15R2_FB4 = 4;              // Filter bits
    static const uint8_t F15R2_FB5 = 5;              // Filter bits
    static const uint8_t F15R2_FB6 = 6;              // Filter bits
    static const uint8_t F15R2_FB7 = 7;              // Filter bits
    static const uint8_t F15R2_FB8 = 8;              // Filter bits
    static const uint8_t F15R2_FB9 = 9;              // Filter bits
    static const uint8_t F15R2_FB10 = 10;            // Filter bits
    static const uint8_t F15R2_FB11 = 11;            // Filter bits
    static const uint8_t F15R2_FB12 = 12;            // Filter bits
    static const uint8_t F15R2_FB13 = 13;            // Filter bits
    static const uint8_t F15R2_FB14 = 14;            // Filter bits
    static const uint8_t F15R2_FB15 = 15;            // Filter bits
    static const uint8_t F15R2_FB16 = 16;            // Filter bits
    static const uint8_t F15R2_FB17 = 17;            // Filter bits
    static const uint8_t F15R2_FB18 = 18;            // Filter bits
    static const uint8_t F15R2_FB19 = 19;            // Filter bits
    static const uint8_t F15R2_FB20 = 20;            // Filter bits
    static const uint8_t F15R2_FB21 = 21;            // Filter bits
    static const uint8_t F15R2_FB22 = 22;            // Filter bits
    static const uint8_t F15R2_FB23 = 23;            // Filter bits
    static const uint8_t F15R2_FB24 = 24;            // Filter bits
    static const uint8_t F15R2_FB25 = 25;            // Filter bits
    static const uint8_t F15R2_FB26 = 26;            // Filter bits
    static const uint8_t F15R2_FB27 = 27;            // Filter bits
    static const uint8_t F15R2_FB28 = 28;            // Filter bits
    static const uint8_t F15R2_FB29 = 29;            // Filter bits
    static const uint8_t F15R2_FB30 = 30;            // Filter bits
    static const uint8_t F15R2_FB31 = 31;            // Filter bits
    static const uint32_t F15R2_RESET_VALUE = 0x0;

    static const uint8_t F16R1_FB0 = 0;              // Filter bits
    static const uint8_t F16R1_FB1 = 1;              // Filter bits
    static const uint8_t F16R1_FB2 = 2;              // Filter bits
    static const uint8_t F16R1_FB3 = 3;              // Filter bits
    static const uint8_t F16R1_FB4 = 4;              // Filter bits
    static const uint8_t F16R1_FB5 = 5;              // Filter bits
    static const uint8_t F16R1_FB6 = 6;              // Filter bits
    static const uint8_t F16R1_FB7 = 7;              // Filter bits
    static const uint8_t F16R1_FB8 = 8;              // Filter bits
    static const uint8_t F16R1_FB9 = 9;              // Filter bits
    static const uint8_t F16R1_FB10 = 10;            // Filter bits
    static const uint8_t F16R1_FB11 = 11;            // Filter bits
    static const uint8_t F16R1_FB12 = 12;            // Filter bits
    static const uint8_t F16R1_FB13 = 13;            // Filter bits
    static const uint8_t F16R1_FB14 = 14;            // Filter bits
    static const uint8_t F16R1_FB15 = 15;            // Filter bits
    static const uint8_t F16R1_FB16 = 16;            // Filter bits
    static const uint8_t F16R1_FB17 = 17;            // Filter bits
    static const uint8_t F16R1_FB18 = 18;            // Filter bits
    static const uint8_t F16R1_FB19 = 19;            // Filter bits
    static const uint8_t F16R1_FB20 = 20;            // Filter bits
    static const uint8_t F16R1_FB21 = 21;            // Filter bits
    static const uint8_t F16R1_FB22 = 22;            // Filter bits
    static const uint8_t F16R1_FB23 = 23;            // Filter bits
    static const uint8_t F16R1_FB24 = 24;            // Filter bits
    static const uint8_t F16R1_FB25 = 25;            // Filter bits
    static const uint8_t F16R1_FB26 = 26;            // Filter bits
    static const uint8_t F16R1_FB27 = 27;            // Filter bits
    static const uint8_t F16R1_FB28 = 28;            // Filter bits
    static const uint8_t F16R1_FB29 = 29;            // Filter bits
    static const uint8_t F16R1_FB30 = 30;            // Filter bits
    static const uint8_t F16R1_FB31 = 31;            // Filter bits
    static const uint32_t F16R1_RESET_VALUE = 0x0;

    static const uint8_t F16R2_FB0 = 0;              // Filter bits
    static const uint8_t F16R2_FB1 = 1;              // Filter bits
    static const uint8_t F16R2_FB2 = 2;              // Filter bits
    static const uint8_t F16R2_FB3 = 3;              // Filter bits
    static const uint8_t F16R2_FB4 = 4;              // Filter bits
    static const uint8_t F16R2_FB5 = 5;              // Filter bits
    static const uint8_t F16R2_FB6 = 6;              // Filter bits
    static const uint8_t F16R2_FB7 = 7;              // Filter bits
    static const uint8_t F16R2_FB8 = 8;              // Filter bits
    static const uint8_t F16R2_FB9 = 9;              // Filter bits
    static const uint8_t F16R2_FB10 = 10;            // Filter bits
    static const uint8_t F16R2_FB11 = 11;            // Filter bits
    static const uint8_t F16R2_FB12 = 12;            // Filter bits
    static const uint8_t F16R2_FB13 = 13;            // Filter bits
    static const uint8_t F16R2_FB14 = 14;            // Filter bits
    static const uint8_t F16R2_FB15 = 15;            // Filter bits
    static const uint8_t F16R2_FB16 = 16;            // Filter bits
    static const uint8_t F16R2_FB17 = 17;            // Filter bits
    static const uint8_t F16R2_FB18 = 18;            // Filter bits
    static const uint8_t F16R2_FB19 = 19;            // Filter bits
    static const uint8_t F16R2_FB20 = 20;            // Filter bits
    static const uint8_t F16R2_FB21 = 21;            // Filter bits
    static const uint8_t F16R2_FB22 = 22;            // Filter bits
    static const uint8_t F16R2_FB23 = 23;            // Filter bits
    static const uint8_t F16R2_FB24 = 24;            // Filter bits
    static const uint8_t F16R2_FB25 = 25;            // Filter bits
    static const uint8_t F16R2_FB26 = 26;            // Filter bits
    static const uint8_t F16R2_FB27 = 27;            // Filter bits
    static const uint8_t F16R2_FB28 = 28;            // Filter bits
    static const uint8_t F16R2_FB29 = 29;            // Filter bits
    static const uint8_t F16R2_FB30 = 30;            // Filter bits
    static const uint8_t F16R2_FB31 = 31;            // Filter bits
    static const uint32_t F16R2_RESET_VALUE = 0x0;

    static const uint8_t F17R1_FB0 = 0;              // Filter bits
    static const uint8_t F17R1_FB1 = 1;              // Filter bits
    static const uint8_t F17R1_FB2 = 2;              // Filter bits
    static const uint8_t F17R1_FB3 = 3;              // Filter bits
    static const uint8_t F17R1_FB4 = 4;              // Filter bits
    static const uint8_t F17R1_FB5 = 5;              // Filter bits
    static const uint8_t F17R1_FB6 = 6;              // Filter bits
    static const uint8_t F17R1_FB7 = 7;              // Filter bits
    static const uint8_t F17R1_FB8 = 8;              // Filter bits
    static const uint8_t F17R1_FB9 = 9;              // Filter bits
    static const uint8_t F17R1_FB10 = 10;            // Filter bits
    static const uint8_t F17R1_FB11 = 11;            // Filter bits
    static const uint8_t F17R1_FB12 = 12;            // Filter bits
    static const uint8_t F17R1_FB13 = 13;            // Filter bits
    static const uint8_t F17R1_FB14 = 14;            // Filter bits
    static const uint8_t F17R1_FB15 = 15;            // Filter bits
    static const uint8_t F17R1_FB16 = 16;            // Filter bits
    static const uint8_t F17R1_FB17 = 17;            // Filter bits
    static const uint8_t F17R1_FB18 = 18;            // Filter bits
    static const uint8_t F17R1_FB19 = 19;            // Filter bits
    static const uint8_t F17R1_FB20 = 20;            // Filter bits
    static const uint8_t F17R1_FB21 = 21;            // Filter bits
    static const uint8_t F17R1_FB22 = 22;            // Filter bits
    static const uint8_t F17R1_FB23 = 23;            // Filter bits
    static const uint8_t F17R1_FB24 = 24;            // Filter bits
    static const uint8_t F17R1_FB25 = 25;            // Filter bits
    static const uint8_t F17R1_FB26 = 26;            // Filter bits
    static const uint8_t F17R1_FB27 = 27;            // Filter bits
    static const uint8_t F17R1_FB28 = 28;            // Filter bits
    static const uint8_t F17R1_FB29 = 29;            // Filter bits
    static const uint8_t F17R1_FB30 = 30;            // Filter bits
    static const uint8_t F17R1_FB31 = 31;            // Filter bits
    static const uint32_t F17R1_RESET_VALUE = 0x0;

    static const uint8_t F17R2_FB0 = 0;              // Filter bits
    static const uint8_t F17R2_FB1 = 1;              // Filter bits
    static const uint8_t F17R2_FB2 = 2;              // Filter bits
    static const uint8_t F17R2_FB3 = 3;              // Filter bits
    static const uint8_t F17R2_FB4 = 4;              // Filter bits
    static const uint8_t F17R2_FB5 = 5;              // Filter bits
    static const uint8_t F17R2_FB6 = 6;              // Filter bits
    static const uint8_t F17R2_FB7 = 7;              // Filter bits
    static const uint8_t F17R2_FB8 = 8;              // Filter bits
    static const uint8_t F17R2_FB9 = 9;              // Filter bits
    static const uint8_t F17R2_FB10 = 10;            // Filter bits
    static const uint8_t F17R2_FB11 = 11;            // Filter bits
    static const uint8_t F17R2_FB12 = 12;            // Filter bits
    static const uint8_t F17R2_FB13 = 13;            // Filter bits
    static const uint8_t F17R2_FB14 = 14;            // Filter bits
    static const uint8_t F17R2_FB15 = 15;            // Filter bits
    static const uint8_t F17R2_FB16 = 16;            // Filter bits
    static const uint8_t F17R2_FB17 = 17;            // Filter bits
    static const uint8_t F17R2_FB18 = 18;            // Filter bits
    static const uint8_t F17R2_FB19 = 19;            // Filter bits
    static const uint8_t F17R2_FB20 = 20;            // Filter bits
    static const uint8_t F17R2_FB21 = 21;            // Filter bits
    static const uint8_t F17R2_FB22 = 22;            // Filter bits
    static const uint8_t F17R2_FB23 = 23;            // Filter bits
    static const uint8_t F17R2_FB24 = 24;            // Filter bits
    static const uint8_t F17R2_FB25 = 25;            // Filter bits
    static const uint8_t F17R2_FB26 = 26;            // Filter bits
    static const uint8_t F17R2_FB27 = 27;            // Filter bits
    static const uint8_t F17R2_FB28 = 28;            // Filter bits
    static const uint8_t F17R2_FB29 = 29;            // Filter bits
    static const uint8_t F17R2_FB30 = 30;            // Filter bits
    static const uint8_t F17R2_FB31 = 31;            // Filter bits
    static const uint32_t F17R2_RESET_VALUE = 0x0;

    static const uint8_t F18R1_FB0 = 0;              // Filter bits
    static const uint8_t F18R1_FB1 = 1;              // Filter bits
    static const uint8_t F18R1_FB2 = 2;              // Filter bits
    static const uint8_t F18R1_FB3 = 3;              // Filter bits
    static const uint8_t F18R1_FB4 = 4;              // Filter bits
    static const uint8_t F18R1_FB5 = 5;              // Filter bits
    static const uint8_t F18R1_FB6 = 6;              // Filter bits
    static const uint8_t F18R1_FB7 = 7;              // Filter bits
    static const uint8_t F18R1_FB8 = 8;              // Filter bits
    static const uint8_t F18R1_FB9 = 9;              // Filter bits
    static const uint8_t F18R1_FB10 = 10;            // Filter bits
    static const uint8_t F18R1_FB11 = 11;            // Filter bits
    static const uint8_t F18R1_FB12 = 12;            // Filter bits
    static const uint8_t F18R1_FB13 = 13;            // Filter bits
    static const uint8_t F18R1_FB14 = 14;            // Filter bits
    static const uint8_t F18R1_FB15 = 15;            // Filter bits
    static const uint8_t F18R1_FB16 = 16;            // Filter bits
    static const uint8_t F18R1_FB17 = 17;            // Filter bits
    static const uint8_t F18R1_FB18 = 18;            // Filter bits
    static const uint8_t F18R1_FB19 = 19;            // Filter bits
    static const uint8_t F18R1_FB20 = 20;            // Filter bits
    static const uint8_t F18R1_FB21 = 21;            // Filter bits
    static const uint8_t F18R1_FB22 = 22;            // Filter bits
    static const uint8_t F18R1_FB23 = 23;            // Filter bits
    static const uint8_t F18R1_FB24 = 24;            // Filter bits
    static const uint8_t F18R1_FB25 = 25;            // Filter bits
    static const uint8_t F18R1_FB26 = 26;            // Filter bits
    static const uint8_t F18R1_FB27 = 27;            // Filter bits
    static const uint8_t F18R1_FB28 = 28;            // Filter bits
    static const uint8_t F18R1_FB29 = 29;            // Filter bits
    static const uint8_t F18R1_FB30 = 30;            // Filter bits
    static const uint8_t F18R1_FB31 = 31;            // Filter bits
    static const uint32_t F18R1_RESET_VALUE = 0x0;

    static const uint8_t F18R2_FB0 = 0;              // Filter bits
    static const uint8_t F18R2_FB1 = 1;              // Filter bits
    static const uint8_t F18R2_FB2 = 2;              // Filter bits
    static const uint8_t F18R2_FB3 = 3;              // Filter bits
    static const uint8_t F18R2_FB4 = 4;              // Filter bits
    static const uint8_t F18R2_FB5 = 5;              // Filter bits
    static const uint8_t F18R2_FB6 = 6;              // Filter bits
    static const uint8_t F18R2_FB7 = 7;              // Filter bits
    static const uint8_t F18R2_FB8 = 8;              // Filter bits
    static const uint8_t F18R2_FB9 = 9;              // Filter bits
    static const uint8_t F18R2_FB10 = 10;            // Filter bits
    static const uint8_t F18R2_FB11 = 11;            // Filter bits
    static const uint8_t F18R2_FB12 = 12;            // Filter bits
    static const uint8_t F18R2_FB13 = 13;            // Filter bits
    static const uint8_t F18R2_FB14 = 14;            // Filter bits
    static const uint8_t F18R2_FB15 = 15;            // Filter bits
    static const uint8_t F18R2_FB16 = 16;            // Filter bits
    static const uint8_t F18R2_FB17 = 17;            // Filter bits
    static const uint8_t F18R2_FB18 = 18;            // Filter bits
    static const uint8_t F18R2_FB19 = 19;            // Filter bits
    static const uint8_t F18R2_FB20 = 20;            // Filter bits
    static const uint8_t F18R2_FB21 = 21;            // Filter bits
    static const uint8_t F18R2_FB22 = 22;            // Filter bits
    static const uint8_t F18R2_FB23 = 23;            // Filter bits
    static const uint8_t F18R2_FB24 = 24;            // Filter bits
    static const uint8_t F18R2_FB25 = 25;            // Filter bits
    static const uint8_t F18R2_FB26 = 26;            // Filter bits
    static const uint8_t F18R2_FB27 = 27;            // Filter bits
    static const uint8_t F18R2_FB28 = 28;            // Filter bits
    static const uint8_t F18R2_FB29 = 29;            // Filter bits
    static const uint8_t F18R2_FB30 = 30;            // Filter bits
    static const uint8_t F18R2_FB31 = 31;            // Filter bits
    static const uint32_t F18R2_RESET_VALUE = 0x0;

    static const uint8_t F19R1_FB0 = 0;              // Filter bits
    static const uint8_t F19R1_FB1 = 1;              // Filter bits
    static const uint8_t F19R1_FB2 = 2;              // Filter bits
    static const uint8_t F19R1_FB3 = 3;              // Filter bits
    static const uint8_t F19R1_FB4 = 4;              // Filter bits
    static const uint8_t F19R1_FB5 = 5;              // Filter bits
    static const uint8_t F19R1_FB6 = 6;              // Filter bits
    static const uint8_t F19R1_FB7 = 7;              // Filter bits
    static const uint8_t F19R1_FB8 = 8;              // Filter bits
    static const uint8_t F19R1_FB9 = 9;              // Filter bits
    static const uint8_t F19R1_FB10 = 10;            // Filter bits
    static const uint8_t F19R1_FB11 = 11;            // Filter bits
    static const uint8_t F19R1_FB12 = 12;            // Filter bits
    static const uint8_t F19R1_FB13 = 13;            // Filter bits
    static const uint8_t F19R1_FB14 = 14;            // Filter bits
    static const uint8_t F19R1_FB15 = 15;            // Filter bits
    static const uint8_t F19R1_FB16 = 16;            // Filter bits
    static const uint8_t F19R1_FB17 = 17;            // Filter bits
    static const uint8_t F19R1_FB18 = 18;            // Filter bits
    static const uint8_t F19R1_FB19 = 19;            // Filter bits
    static const uint8_t F19R1_FB20 = 20;            // Filter bits
    static const uint8_t F19R1_FB21 = 21;            // Filter bits
    static const uint8_t F19R1_FB22 = 22;            // Filter bits
    static const uint8_t F19R1_FB23 = 23;            // Filter bits
    static const uint8_t F19R1_FB24 = 24;            // Filter bits
    static const uint8_t F19R1_FB25 = 25;            // Filter bits
    static const uint8_t F19R1_FB26 = 26;            // Filter bits
    static const uint8_t F19R1_FB27 = 27;            // Filter bits
    static const uint8_t F19R1_FB28 = 28;            // Filter bits
    static const uint8_t F19R1_FB29 = 29;            // Filter bits
    static const uint8_t F19R1_FB30 = 30;            // Filter bits
    static const uint8_t F19R1_FB31 = 31;            // Filter bits
    static const uint32_t F19R1_RESET_VALUE = 0x0;

    static const uint8_t F19R2_FB0 = 0;              // Filter bits
    static const uint8_t F19R2_FB1 = 1;              // Filter bits
    static const uint8_t F19R2_FB2 = 2;              // Filter bits
    static const uint8_t F19R2_FB3 = 3;              // Filter bits
    static const uint8_t F19R2_FB4 = 4;              // Filter bits
    static const uint8_t F19R2_FB5 = 5;              // Filter bits
    static const uint8_t F19R2_FB6 = 6;              // Filter bits
    static const uint8_t F19R2_FB7 = 7;              // Filter bits
    static const uint8_t F19R2_FB8 = 8;              // Filter bits
    static const uint8_t F19R2_FB9 = 9;              // Filter bits
    static const uint8_t F19R2_FB10 = 10;            // Filter bits
    static const uint8_t F19R2_FB11 = 11;            // Filter bits
    static const uint8_t F19R2_FB12 = 12;            // Filter bits
    static const uint8_t F19R2_FB13 = 13;            // Filter bits
    static const uint8_t F19R2_FB14 = 14;            // Filter bits
    static const uint8_t F19R2_FB15 = 15;            // Filter bits
    static const uint8_t F19R2_FB16 = 16;            // Filter bits
    static const uint8_t F19R2_FB17 = 17;            // Filter bits
    static const uint8_t F19R2_FB18 = 18;            // Filter bits
    static const uint8_t F19R2_FB19 = 19;            // Filter bits
    static const uint8_t F19R2_FB20 = 20;            // Filter bits
    static const uint8_t F19R2_FB21 = 21;            // Filter bits
    static const uint8_t F19R2_FB22 = 22;            // Filter bits
    static const uint8_t F19R2_FB23 = 23;            // Filter bits
    static const uint8_t F19R2_FB24 = 24;            // Filter bits
    static const uint8_t F19R2_FB25 = 25;            // Filter bits
    static const uint8_t F19R2_FB26 = 26;            // Filter bits
    static const uint8_t F19R2_FB27 = 27;            // Filter bits
    static const uint8_t F19R2_FB28 = 28;            // Filter bits
    static const uint8_t F19R2_FB29 = 29;            // Filter bits
    static const uint8_t F19R2_FB30 = 30;            // Filter bits
    static const uint8_t F19R2_FB31 = 31;            // Filter bits
    static const uint32_t F19R2_RESET_VALUE = 0x0;

    static const uint8_t F20R1_FB0 = 0;              // Filter bits
    static const uint8_t F20R1_FB1 = 1;              // Filter bits
    static const uint8_t F20R1_FB2 = 2;              // Filter bits
    static const uint8_t F20R1_FB3 = 3;              // Filter bits
    static const uint8_t F20R1_FB4 = 4;              // Filter bits
    static const uint8_t F20R1_FB5 = 5;              // Filter bits
    static const uint8_t F20R1_FB6 = 6;              // Filter bits
    static const uint8_t F20R1_FB7 = 7;              // Filter bits
    static const uint8_t F20R1_FB8 = 8;              // Filter bits
    static const uint8_t F20R1_FB9 = 9;              // Filter bits
    static const uint8_t F20R1_FB10 = 10;            // Filter bits
    static const uint8_t F20R1_FB11 = 11;            // Filter bits
    static const uint8_t F20R1_FB12 = 12;            // Filter bits
    static const uint8_t F20R1_FB13 = 13;            // Filter bits
    static const uint8_t F20R1_FB14 = 14;            // Filter bits
    static const uint8_t F20R1_FB15 = 15;            // Filter bits
    static const uint8_t F20R1_FB16 = 16;            // Filter bits
    static const uint8_t F20R1_FB17 = 17;            // Filter bits
    static const uint8_t F20R1_FB18 = 18;            // Filter bits
    static const uint8_t F20R1_FB19 = 19;            // Filter bits
    static const uint8_t F20R1_FB20 = 20;            // Filter bits
    static const uint8_t F20R1_FB21 = 21;            // Filter bits
    static const uint8_t F20R1_FB22 = 22;            // Filter bits
    static const uint8_t F20R1_FB23 = 23;            // Filter bits
    static const uint8_t F20R1_FB24 = 24;            // Filter bits
    static const uint8_t F20R1_FB25 = 25;            // Filter bits
    static const uint8_t F20R1_FB26 = 26;            // Filter bits
    static const uint8_t F20R1_FB27 = 27;            // Filter bits
    static const uint8_t F20R1_FB28 = 28;            // Filter bits
    static const uint8_t F20R1_FB29 = 29;            // Filter bits
    static const uint8_t F20R1_FB30 = 30;            // Filter bits
    static const uint8_t F20R1_FB31 = 31;            // Filter bits
    static const uint32_t F20R1_RESET_VALUE = 0x0;

    static const uint8_t F20R2_FB0 = 0;              // Filter bits
    static const uint8_t F20R2_FB1 = 1;              // Filter bits
    static const uint8_t F20R2_FB2 = 2;              // Filter bits
    static const uint8_t F20R2_FB3 = 3;              // Filter bits
    static const uint8_t F20R2_FB4 = 4;              // Filter bits
    static const uint8_t F20R2_FB5 = 5;              // Filter bits
    static const uint8_t F20R2_FB6 = 6;              // Filter bits
    static const uint8_t F20R2_FB7 = 7;              // Filter bits
    static const uint8_t F20R2_FB8 = 8;              // Filter bits
    static const uint8_t F20R2_FB9 = 9;              // Filter bits
    static const uint8_t F20R2_FB10 = 10;            // Filter bits
    static const uint8_t F20R2_FB11 = 11;            // Filter bits
    static const uint8_t F20R2_FB12 = 12;            // Filter bits
    static const uint8_t F20R2_FB13 = 13;            // Filter bits
    static const uint8_t F20R2_FB14 = 14;            // Filter bits
    static const uint8_t F20R2_FB15 = 15;            // Filter bits
    static const uint8_t F20R2_FB16 = 16;            // Filter bits
    static const uint8_t F20R2_FB17 = 17;            // Filter bits
    static const uint8_t F20R2_FB18 = 18;            // Filter bits
    static const uint8_t F20R2_FB19 = 19;            // Filter bits
    static const uint8_t F20R2_FB20 = 20;            // Filter bits
    static const uint8_t F20R2_FB21 = 21;            // Filter bits
    static const uint8_t F20R2_FB22 = 22;            // Filter bits
    static const uint8_t F20R2_FB23 = 23;            // Filter bits
    static const uint8_t F20R2_FB24 = 24;            // Filter bits
    static const uint8_t F20R2_FB25 = 25;            // Filter bits
    static const uint8_t F20R2_FB26 = 26;            // Filter bits
    static const uint8_t F20R2_FB27 = 27;            // Filter bits
    static const uint8_t F20R2_FB28 = 28;            // Filter bits
    static const uint8_t F20R2_FB29 = 29;            // Filter bits
    static const uint8_t F20R2_FB30 = 30;            // Filter bits
    static const uint8_t F20R2_FB31 = 31;            // Filter bits
    static const uint32_t F20R2_RESET_VALUE = 0x0;

    static const uint8_t F21R1_FB0 = 0;              // Filter bits
    static const uint8_t F21R1_FB1 = 1;              // Filter bits
    static const uint8_t F21R1_FB2 = 2;              // Filter bits
    static const uint8_t F21R1_FB3 = 3;              // Filter bits
    static const uint8_t F21R1_FB4 = 4;              // Filter bits
    static const uint8_t F21R1_FB5 = 5;              // Filter bits
    static const uint8_t F21R1_FB6 = 6;              // Filter bits
    static const uint8_t F21R1_FB7 = 7;              // Filter bits
    static const uint8_t F21R1_FB8 = 8;              // Filter bits
    static const uint8_t F21R1_FB9 = 9;              // Filter bits
    static const uint8_t F21R1_FB10 = 10;            // Filter bits
    static const uint8_t F21R1_FB11 = 11;            // Filter bits
    static const uint8_t F21R1_FB12 = 12;            // Filter bits
    static const uint8_t F21R1_FB13 = 13;            // Filter bits
    static const uint8_t F21R1_FB14 = 14;            // Filter bits
    static const uint8_t F21R1_FB15 = 15;            // Filter bits
    static const uint8_t F21R1_FB16 = 16;            // Filter bits
    static const uint8_t F21R1_FB17 = 17;            // Filter bits
    static const uint8_t F21R1_FB18 = 18;            // Filter bits
    static const uint8_t F21R1_FB19 = 19;            // Filter bits
    static const uint8_t F21R1_FB20 = 20;            // Filter bits
    static const uint8_t F21R1_FB21 = 21;            // Filter bits
    static const uint8_t F21R1_FB22 = 22;            // Filter bits
    static const uint8_t F21R1_FB23 = 23;            // Filter bits
    static const uint8_t F21R1_FB24 = 24;            // Filter bits
    static const uint8_t F21R1_FB25 = 25;            // Filter bits
    static const uint8_t F21R1_FB26 = 26;            // Filter bits
    static const uint8_t F21R1_FB27 = 27;            // Filter bits
    static const uint8_t F21R1_FB28 = 28;            // Filter bits
    static const uint8_t F21R1_FB29 = 29;            // Filter bits
    static const uint8_t F21R1_FB30 = 30;            // Filter bits
    static const uint8_t F21R1_FB31 = 31;            // Filter bits
    static const uint32_t F21R1_RESET_VALUE = 0x0;

    static const uint8_t F21R2_FB0 = 0;              // Filter bits
    static const uint8_t F21R2_FB1 = 1;              // Filter bits
    static const uint8_t F21R2_FB2 = 2;              // Filter bits
    static const uint8_t F21R2_FB3 = 3;              // Filter bits
    static const uint8_t F21R2_FB4 = 4;              // Filter bits
    static const uint8_t F21R2_FB5 = 5;              // Filter bits
    static const uint8_t F21R2_FB6 = 6;              // Filter bits
    static const uint8_t F21R2_FB7 = 7;              // Filter bits
    static const uint8_t F21R2_FB8 = 8;              // Filter bits
    static const uint8_t F21R2_FB9 = 9;              // Filter bits
    static const uint8_t F21R2_FB10 = 10;            // Filter bits
    static const uint8_t F21R2_FB11 = 11;            // Filter bits
    static const uint8_t F21R2_FB12 = 12;            // Filter bits
    static const uint8_t F21R2_FB13 = 13;            // Filter bits
    static const uint8_t F21R2_FB14 = 14;            // Filter bits
    static const uint8_t F21R2_FB15 = 15;            // Filter bits
    static const uint8_t F21R2_FB16 = 16;            // Filter bits
    static const uint8_t F21R2_FB17 = 17;            // Filter bits
    static const uint8_t F21R2_FB18 = 18;            // Filter bits
    static const uint8_t F21R2_FB19 = 19;            // Filter bits
    static const uint8_t F21R2_FB20 = 20;            // Filter bits
    static const uint8_t F21R2_FB21 = 21;            // Filter bits
    static const uint8_t F21R2_FB22 = 22;            // Filter bits
    static const uint8_t F21R2_FB23 = 23;            // Filter bits
    static const uint8_t F21R2_FB24 = 24;            // Filter bits
    static const uint8_t F21R2_FB25 = 25;            // Filter bits
    static const uint8_t F21R2_FB26 = 26;            // Filter bits
    static const uint8_t F21R2_FB27 = 27;            // Filter bits
    static const uint8_t F21R2_FB28 = 28;            // Filter bits
    static const uint8_t F21R2_FB29 = 29;            // Filter bits
    static const uint8_t F21R2_FB30 = 30;            // Filter bits
    static const uint8_t F21R2_FB31 = 31;            // Filter bits
    static const uint32_t F21R2_RESET_VALUE = 0x0;

    static const uint8_t F22R1_FB0 = 0;              // Filter bits
    static const uint8_t F22R1_FB1 = 1;              // Filter bits
    static const uint8_t F22R1_FB2 = 2;              // Filter bits
    static const uint8_t F22R1_FB3 = 3;              // Filter bits
    static const uint8_t F22R1_FB4 = 4;              // Filter bits
    static const uint8_t F22R1_FB5 = 5;              // Filter bits
    static const uint8_t F22R1_FB6 = 6;              // Filter bits
    static const uint8_t F22R1_FB7 = 7;              // Filter bits
    static const uint8_t F22R1_FB8 = 8;              // Filter bits
    static const uint8_t F22R1_FB9 = 9;              // Filter bits
    static const uint8_t F22R1_FB10 = 10;            // Filter bits
    static const uint8_t F22R1_FB11 = 11;            // Filter bits
    static const uint8_t F22R1_FB12 = 12;            // Filter bits
    static const uint8_t F22R1_FB13 = 13;            // Filter bits
    static const uint8_t F22R1_FB14 = 14;            // Filter bits
    static const uint8_t F22R1_FB15 = 15;            // Filter bits
    static const uint8_t F22R1_FB16 = 16;            // Filter bits
    static const uint8_t F22R1_FB17 = 17;            // Filter bits
    static const uint8_t F22R1_FB18 = 18;            // Filter bits
    static const uint8_t F22R1_FB19 = 19;            // Filter bits
    static const uint8_t F22R1_FB20 = 20;            // Filter bits
    static const uint8_t F22R1_FB21 = 21;            // Filter bits
    static const uint8_t F22R1_FB22 = 22;            // Filter bits
    static const uint8_t F22R1_FB23 = 23;            // Filter bits
    static const uint8_t F22R1_FB24 = 24;            // Filter bits
    static const uint8_t F22R1_FB25 = 25;            // Filter bits
    static const uint8_t F22R1_FB26 = 26;            // Filter bits
    static const uint8_t F22R1_FB27 = 27;            // Filter bits
    static const uint8_t F22R1_FB28 = 28;            // Filter bits
    static const uint8_t F22R1_FB29 = 29;            // Filter bits
    static const uint8_t F22R1_FB30 = 30;            // Filter bits
    static const uint8_t F22R1_FB31 = 31;            // Filter bits
    static const uint32_t F22R1_RESET_VALUE = 0x0;

    static const uint8_t F22R2_FB0 = 0;              // Filter bits
    static const uint8_t F22R2_FB1 = 1;              // Filter bits
    static const uint8_t F22R2_FB2 = 2;              // Filter bits
    static const uint8_t F22R2_FB3 = 3;              // Filter bits
    static const uint8_t F22R2_FB4 = 4;              // Filter bits
    static const uint8_t F22R2_FB5 = 5;              // Filter bits
    static const uint8_t F22R2_FB6 = 6;              // Filter bits
    static const uint8_t F22R2_FB7 = 7;              // Filter bits
    static const uint8_t F22R2_FB8 = 8;              // Filter bits
    static const uint8_t F22R2_FB9 = 9;              // Filter bits
    static const uint8_t F22R2_FB10 = 10;            // Filter bits
    static const uint8_t F22R2_FB11 = 11;            // Filter bits
    static const uint8_t F22R2_FB12 = 12;            // Filter bits
    static const uint8_t F22R2_FB13 = 13;            // Filter bits
    static const uint8_t F22R2_FB14 = 14;            // Filter bits
    static const uint8_t F22R2_FB15 = 15;            // Filter bits
    static const uint8_t F22R2_FB16 = 16;            // Filter bits
    static const uint8_t F22R2_FB17 = 17;            // Filter bits
    static const uint8_t F22R2_FB18 = 18;            // Filter bits
    static const uint8_t F22R2_FB19 = 19;            // Filter bits
    static const uint8_t F22R2_FB20 = 20;            // Filter bits
    static const uint8_t F22R2_FB21 = 21;            // Filter bits
    static const uint8_t F22R2_FB22 = 22;            // Filter bits
    static const uint8_t F22R2_FB23 = 23;            // Filter bits
    static const uint8_t F22R2_FB24 = 24;            // Filter bits
    static const uint8_t F22R2_FB25 = 25;            // Filter bits
    static const uint8_t F22R2_FB26 = 26;            // Filter bits
    static const uint8_t F22R2_FB27 = 27;            // Filter bits
    static const uint8_t F22R2_FB28 = 28;            // Filter bits
    static const uint8_t F22R2_FB29 = 29;            // Filter bits
    static const uint8_t F22R2_FB30 = 30;            // Filter bits
    static const uint8_t F22R2_FB31 = 31;            // Filter bits
    static const uint32_t F22R2_RESET_VALUE = 0x0;

    static const uint8_t F23R1_FB0 = 0;              // Filter bits
    static const uint8_t F23R1_FB1 = 1;              // Filter bits
    static const uint8_t F23R1_FB2 = 2;              // Filter bits
    static const uint8_t F23R1_FB3 = 3;              // Filter bits
    static const uint8_t F23R1_FB4 = 4;              // Filter bits
    static const uint8_t F23R1_FB5 = 5;              // Filter bits
    static const uint8_t F23R1_FB6 = 6;              // Filter bits
    static const uint8_t F23R1_FB7 = 7;              // Filter bits
    static const uint8_t F23R1_FB8 = 8;              // Filter bits
    static const uint8_t F23R1_FB9 = 9;              // Filter bits
    static const uint8_t F23R1_FB10 = 10;            // Filter bits
    static const uint8_t F23R1_FB11 = 11;            // Filter bits
    static const uint8_t F23R1_FB12 = 12;            // Filter bits
    static const uint8_t F23R1_FB13 = 13;            // Filter bits
    static const uint8_t F23R1_FB14 = 14;            // Filter bits
    static const uint8_t F23R1_FB15 = 15;            // Filter bits
    static const uint8_t F23R1_FB16 = 16;            // Filter bits
    static const uint8_t F23R1_FB17 = 17;            // Filter bits
    static const uint8_t F23R1_FB18 = 18;            // Filter bits
    static const uint8_t F23R1_FB19 = 19;            // Filter bits
    static const uint8_t F23R1_FB20 = 20;            // Filter bits
    static const uint8_t F23R1_FB21 = 21;            // Filter bits
    static const uint8_t F23R1_FB22 = 22;            // Filter bits
    static const uint8_t F23R1_FB23 = 23;            // Filter bits
    static const uint8_t F23R1_FB24 = 24;            // Filter bits
    static const uint8_t F23R1_FB25 = 25;            // Filter bits
    static const uint8_t F23R1_FB26 = 26;            // Filter bits
    static const uint8_t F23R1_FB27 = 27;            // Filter bits
    static const uint8_t F23R1_FB28 = 28;            // Filter bits
    static const uint8_t F23R1_FB29 = 29;            // Filter bits
    static const uint8_t F23R1_FB30 = 30;            // Filter bits
    static const uint8_t F23R1_FB31 = 31;            // Filter bits
    static const uint32_t F23R1_RESET_VALUE = 0x0;

    static const uint8_t F23R2_FB0 = 0;              // Filter bits
    static const uint8_t F23R2_FB1 = 1;              // Filter bits
    static const uint8_t F23R2_FB2 = 2;              // Filter bits
    static const uint8_t F23R2_FB3 = 3;              // Filter bits
    static const uint8_t F23R2_FB4 = 4;              // Filter bits
    static const uint8_t F23R2_FB5 = 5;              // Filter bits
    static const uint8_t F23R2_FB6 = 6;              // Filter bits
    static const uint8_t F23R2_FB7 = 7;              // Filter bits
    static const uint8_t F23R2_FB8 = 8;              // Filter bits
    static const uint8_t F23R2_FB9 = 9;              // Filter bits
    static const uint8_t F23R2_FB10 = 10;            // Filter bits
    static const uint8_t F23R2_FB11 = 11;            // Filter bits
    static const uint8_t F23R2_FB12 = 12;            // Filter bits
    static const uint8_t F23R2_FB13 = 13;            // Filter bits
    static const uint8_t F23R2_FB14 = 14;            // Filter bits
    static const uint8_t F23R2_FB15 = 15;            // Filter bits
    static const uint8_t F23R2_FB16 = 16;            // Filter bits
    static const uint8_t F23R2_FB17 = 17;            // Filter bits
    static const uint8_t F23R2_FB18 = 18;            // Filter bits
    static const uint8_t F23R2_FB19 = 19;            // Filter bits
    static const uint8_t F23R2_FB20 = 20;            // Filter bits
    static const uint8_t F23R2_FB21 = 21;            // Filter bits
    static const uint8_t F23R2_FB22 = 22;            // Filter bits
    static const uint8_t F23R2_FB23 = 23;            // Filter bits
    static const uint8_t F23R2_FB24 = 24;            // Filter bits
    static const uint8_t F23R2_FB25 = 25;            // Filter bits
    static const uint8_t F23R2_FB26 = 26;            // Filter bits
    static const uint8_t F23R2_FB27 = 27;            // Filter bits
    static const uint8_t F23R2_FB28 = 28;            // Filter bits
    static const uint8_t F23R2_FB29 = 29;            // Filter bits
    static const uint8_t F23R2_FB30 = 30;            // Filter bits
    static const uint8_t F23R2_FB31 = 31;            // Filter bits
    static const uint32_t F23R2_RESET_VALUE = 0x0;

    static const uint8_t F24R1_FB0 = 0;              // Filter bits
    static const uint8_t F24R1_FB1 = 1;              // Filter bits
    static const uint8_t F24R1_FB2 = 2;              // Filter bits
    static const uint8_t F24R1_FB3 = 3;              // Filter bits
    static const uint8_t F24R1_FB4 = 4;              // Filter bits
    static const uint8_t F24R1_FB5 = 5;              // Filter bits
    static const uint8_t F24R1_FB6 = 6;              // Filter bits
    static const uint8_t F24R1_FB7 = 7;              // Filter bits
    static const uint8_t F24R1_FB8 = 8;              // Filter bits
    static const uint8_t F24R1_FB9 = 9;              // Filter bits
    static const uint8_t F24R1_FB10 = 10;            // Filter bits
    static const uint8_t F24R1_FB11 = 11;            // Filter bits
    static const uint8_t F24R1_FB12 = 12;            // Filter bits
    static const uint8_t F24R1_FB13 = 13;            // Filter bits
    static const uint8_t F24R1_FB14 = 14;            // Filter bits
    static const uint8_t F24R1_FB15 = 15;            // Filter bits
    static const uint8_t F24R1_FB16 = 16;            // Filter bits
    static const uint8_t F24R1_FB17 = 17;            // Filter bits
    static const uint8_t F24R1_FB18 = 18;            // Filter bits
    static const uint8_t F24R1_FB19 = 19;            // Filter bits
    static const uint8_t F24R1_FB20 = 20;            // Filter bits
    static const uint8_t F24R1_FB21 = 21;            // Filter bits
    static const uint8_t F24R1_FB22 = 22;            // Filter bits
    static const uint8_t F24R1_FB23 = 23;            // Filter bits
    static const uint8_t F24R1_FB24 = 24;            // Filter bits
    static const uint8_t F24R1_FB25 = 25;            // Filter bits
    static const uint8_t F24R1_FB26 = 26;            // Filter bits
    static const uint8_t F24R1_FB27 = 27;            // Filter bits
    static const uint8_t F24R1_FB28 = 28;            // Filter bits
    static const uint8_t F24R1_FB29 = 29;            // Filter bits
    static const uint8_t F24R1_FB30 = 30;            // Filter bits
    static const uint8_t F24R1_FB31 = 31;            // Filter bits
    static const uint32_t F24R1_RESET_VALUE = 0x0;

    static const uint8_t F24R2_FB0 = 0;              // Filter bits
    static const uint8_t F24R2_FB1 = 1;              // Filter bits
    static const uint8_t F24R2_FB2 = 2;              // Filter bits
    static const uint8_t F24R2_FB3 = 3;              // Filter bits
    static const uint8_t F24R2_FB4 = 4;              // Filter bits
    static const uint8_t F24R2_FB5 = 5;              // Filter bits
    static const uint8_t F24R2_FB6 = 6;              // Filter bits
    static const uint8_t F24R2_FB7 = 7;              // Filter bits
    static const uint8_t F24R2_FB8 = 8;              // Filter bits
    static const uint8_t F24R2_FB9 = 9;              // Filter bits
    static const uint8_t F24R2_FB10 = 10;            // Filter bits
    static const uint8_t F24R2_FB11 = 11;            // Filter bits
    static const uint8_t F24R2_FB12 = 12;            // Filter bits
    static const uint8_t F24R2_FB13 = 13;            // Filter bits
    static const uint8_t F24R2_FB14 = 14;            // Filter bits
    static const uint8_t F24R2_FB15 = 15;            // Filter bits
    static const uint8_t F24R2_FB16 = 16;            // Filter bits
    static const uint8_t F24R2_FB17 = 17;            // Filter bits
    static const uint8_t F24R2_FB18 = 18;            // Filter bits
    static const uint8_t F24R2_FB19 = 19;            // Filter bits
    static const uint8_t F24R2_FB20 = 20;            // Filter bits
    static const uint8_t F24R2_FB21 = 21;            // Filter bits
    static const uint8_t F24R2_FB22 = 22;            // Filter bits
    static const uint8_t F24R2_FB23 = 23;            // Filter bits
    static const uint8_t F24R2_FB24 = 24;            // Filter bits
    static const uint8_t F24R2_FB25 = 25;            // Filter bits
    static const uint8_t F24R2_FB26 = 26;            // Filter bits
    static const uint8_t F24R2_FB27 = 27;            // Filter bits
    static const uint8_t F24R2_FB28 = 28;            // Filter bits
    static const uint8_t F24R2_FB29 = 29;            // Filter bits
    static const uint8_t F24R2_FB30 = 30;            // Filter bits
    static const uint8_t F24R2_FB31 = 31;            // Filter bits
    static const uint32_t F24R2_RESET_VALUE = 0x0;

    static const uint8_t F25R1_FB0 = 0;              // Filter bits
    static const uint8_t F25R1_FB1 = 1;              // Filter bits
    static const uint8_t F25R1_FB2 = 2;              // Filter bits
    static const uint8_t F25R1_FB3 = 3;              // Filter bits
    static const uint8_t F25R1_FB4 = 4;              // Filter bits
    static const uint8_t F25R1_FB5 = 5;              // Filter bits
    static const uint8_t F25R1_FB6 = 6;              // Filter bits
    static const uint8_t F25R1_FB7 = 7;              // Filter bits
    static const uint8_t F25R1_FB8 = 8;              // Filter bits
    static const uint8_t F25R1_FB9 = 9;              // Filter bits
    static const uint8_t F25R1_FB10 = 10;            // Filter bits
    static const uint8_t F25R1_FB11 = 11;            // Filter bits
    static const uint8_t F25R1_FB12 = 12;            // Filter bits
    static const uint8_t F25R1_FB13 = 13;            // Filter bits
    static const uint8_t F25R1_FB14 = 14;            // Filter bits
    static const uint8_t F25R1_FB15 = 15;            // Filter bits
    static const uint8_t F25R1_FB16 = 16;            // Filter bits
    static const uint8_t F25R1_FB17 = 17;            // Filter bits
    static const uint8_t F25R1_FB18 = 18;            // Filter bits
    static const uint8_t F25R1_FB19 = 19;            // Filter bits
    static const uint8_t F25R1_FB20 = 20;            // Filter bits
    static const uint8_t F25R1_FB21 = 21;            // Filter bits
    static const uint8_t F25R1_FB22 = 22;            // Filter bits
    static const uint8_t F25R1_FB23 = 23;            // Filter bits
    static const uint8_t F25R1_FB24 = 24;            // Filter bits
    static const uint8_t F25R1_FB25 = 25;            // Filter bits
    static const uint8_t F25R1_FB26 = 26;            // Filter bits
    static const uint8_t F25R1_FB27 = 27;            // Filter bits
    static const uint8_t F25R1_FB28 = 28;            // Filter bits
    static const uint8_t F25R1_FB29 = 29;            // Filter bits
    static const uint8_t F25R1_FB30 = 30;            // Filter bits
    static const uint8_t F25R1_FB31 = 31;            // Filter bits
    static const uint32_t F25R1_RESET_VALUE = 0x0;

    static const uint8_t F25R2_FB0 = 0;              // Filter bits
    static const uint8_t F25R2_FB1 = 1;              // Filter bits
    static const uint8_t F25R2_FB2 = 2;              // Filter bits
    static const uint8_t F25R2_FB3 = 3;              // Filter bits
    static const uint8_t F25R2_FB4 = 4;              // Filter bits
    static const uint8_t F25R2_FB5 = 5;              // Filter bits
    static const uint8_t F25R2_FB6 = 6;              // Filter bits
    static const uint8_t F25R2_FB7 = 7;              // Filter bits
    static const uint8_t F25R2_FB8 = 8;              // Filter bits
    static const uint8_t F25R2_FB9 = 9;              // Filter bits
    static const uint8_t F25R2_FB10 = 10;            // Filter bits
    static const uint8_t F25R2_FB11 = 11;            // Filter bits
    static const uint8_t F25R2_FB12 = 12;            // Filter bits
    static const uint8_t F25R2_FB13 = 13;            // Filter bits
    static const uint8_t F25R2_FB14 = 14;            // Filter bits
    static const uint8_t F25R2_FB15 = 15;            // Filter bits
    static const uint8_t F25R2_FB16 = 16;            // Filter bits
    static const uint8_t F25R2_FB17 = 17;            // Filter bits
    static const uint8_t F25R2_FB18 = 18;            // Filter bits
    static const uint8_t F25R2_FB19 = 19;            // Filter bits
    static const uint8_t F25R2_FB20 = 20;            // Filter bits
    static const uint8_t F25R2_FB21 = 21;            // Filter bits
    static const uint8_t F25R2_FB22 = 22;            // Filter bits
    static const uint8_t F25R2_FB23 = 23;            // Filter bits
    static const uint8_t F25R2_FB24 = 24;            // Filter bits
    static const uint8_t F25R2_FB25 = 25;            // Filter bits
    static const uint8_t F25R2_FB26 = 26;            // Filter bits
    static const uint8_t F25R2_FB27 = 27;            // Filter bits
    static const uint8_t F25R2_FB28 = 28;            // Filter bits
    static const uint8_t F25R2_FB29 = 29;            // Filter bits
    static const uint8_t F25R2_FB30 = 30;            // Filter bits
    static const uint8_t F25R2_FB31 = 31;            // Filter bits
    static const uint32_t F25R2_RESET_VALUE = 0x0;

    static const uint8_t F26R1_FB0 = 0;              // Filter bits
    static const uint8_t F26R1_FB1 = 1;              // Filter bits
    static const uint8_t F26R1_FB2 = 2;              // Filter bits
    static const uint8_t F26R1_FB3 = 3;              // Filter bits
    static const uint8_t F26R1_FB4 = 4;              // Filter bits
    static const uint8_t F26R1_FB5 = 5;              // Filter bits
    static const uint8_t F26R1_FB6 = 6;              // Filter bits
    static const uint8_t F26R1_FB7 = 7;              // Filter bits
    static const uint8_t F26R1_FB8 = 8;              // Filter bits
    static const uint8_t F26R1_FB9 = 9;              // Filter bits
    static const uint8_t F26R1_FB10 = 10;            // Filter bits
    static const uint8_t F26R1_FB11 = 11;            // Filter bits
    static const uint8_t F26R1_FB12 = 12;            // Filter bits
    static const uint8_t F26R1_FB13 = 13;            // Filter bits
    static const uint8_t F26R1_FB14 = 14;            // Filter bits
    static const uint8_t F26R1_FB15 = 15;            // Filter bits
    static const uint8_t F26R1_FB16 = 16;            // Filter bits
    static const uint8_t F26R1_FB17 = 17;            // Filter bits
    static const uint8_t F26R1_FB18 = 18;            // Filter bits
    static const uint8_t F26R1_FB19 = 19;            // Filter bits
    static const uint8_t F26R1_FB20 = 20;            // Filter bits
    static const uint8_t F26R1_FB21 = 21;            // Filter bits
    static const uint8_t F26R1_FB22 = 22;            // Filter bits
    static const uint8_t F26R1_FB23 = 23;            // Filter bits
    static const uint8_t F26R1_FB24 = 24;            // Filter bits
    static const uint8_t F26R1_FB25 = 25;            // Filter bits
    static const uint8_t F26R1_FB26 = 26;            // Filter bits
    static const uint8_t F26R1_FB27 = 27;            // Filter bits
    static const uint8_t F26R1_FB28 = 28;            // Filter bits
    static const uint8_t F26R1_FB29 = 29;            // Filter bits
    static const uint8_t F26R1_FB30 = 30;            // Filter bits
    static const uint8_t F26R1_FB31 = 31;            // Filter bits
    static const uint32_t F26R1_RESET_VALUE = 0x0;

    static const uint8_t F26R2_FB0 = 0;              // Filter bits
    static const uint8_t F26R2_FB1 = 1;              // Filter bits
    static const uint8_t F26R2_FB2 = 2;              // Filter bits
    static const uint8_t F26R2_FB3 = 3;              // Filter bits
    static const uint8_t F26R2_FB4 = 4;              // Filter bits
    static const uint8_t F26R2_FB5 = 5;              // Filter bits
    static const uint8_t F26R2_FB6 = 6;              // Filter bits
    static const uint8_t F26R2_FB7 = 7;              // Filter bits
    static const uint8_t F26R2_FB8 = 8;              // Filter bits
    static const uint8_t F26R2_FB9 = 9;              // Filter bits
    static const uint8_t F26R2_FB10 = 10;            // Filter bits
    static const uint8_t F26R2_FB11 = 11;            // Filter bits
    static const uint8_t F26R2_FB12 = 12;            // Filter bits
    static const uint8_t F26R2_FB13 = 13;            // Filter bits
    static const uint8_t F26R2_FB14 = 14;            // Filter bits
    static const uint8_t F26R2_FB15 = 15;            // Filter bits
    static const uint8_t F26R2_FB16 = 16;            // Filter bits
    static const uint8_t F26R2_FB17 = 17;            // Filter bits
    static const uint8_t F26R2_FB18 = 18;            // Filter bits
    static const uint8_t F26R2_FB19 = 19;            // Filter bits
    static const uint8_t F26R2_FB20 = 20;            // Filter bits
    static const uint8_t F26R2_FB21 = 21;            // Filter bits
    static const uint8_t F26R2_FB22 = 22;            // Filter bits
    static const uint8_t F26R2_FB23 = 23;            // Filter bits
    static const uint8_t F26R2_FB24 = 24;            // Filter bits
    static const uint8_t F26R2_FB25 = 25;            // Filter bits
    static const uint8_t F26R2_FB26 = 26;            // Filter bits
    static const uint8_t F26R2_FB27 = 27;            // Filter bits
    static const uint8_t F26R2_FB28 = 28;            // Filter bits
    static const uint8_t F26R2_FB29 = 29;            // Filter bits
    static const uint8_t F26R2_FB30 = 30;            // Filter bits
    static const uint8_t F26R2_FB31 = 31;            // Filter bits
    static const uint32_t F26R2_RESET_VALUE = 0x0;

    static const uint8_t F27R1_FB0 = 0;              // Filter bits
    static const uint8_t F27R1_FB1 = 1;              // Filter bits
    static const uint8_t F27R1_FB2 = 2;              // Filter bits
    static const uint8_t F27R1_FB3 = 3;              // Filter bits
    static const uint8_t F27R1_FB4 = 4;              // Filter bits
    static const uint8_t F27R1_FB5 = 5;              // Filter bits
    static const uint8_t F27R1_FB6 = 6;              // Filter bits
    static const uint8_t F27R1_FB7 = 7;              // Filter bits
    static const uint8_t F27R1_FB8 = 8;              // Filter bits
    static const uint8_t F27R1_FB9 = 9;              // Filter bits
    static const uint8_t F27R1_FB10 = 10;            // Filter bits
    static const uint8_t F27R1_FB11 = 11;            // Filter bits
    static const uint8_t F27R1_FB12 = 12;            // Filter bits
    static const uint8_t F27R1_FB13 = 13;            // Filter bits
    static const uint8_t F27R1_FB14 = 14;            // Filter bits
    static const uint8_t F27R1_FB15 = 15;            // Filter bits
    static const uint8_t F27R1_FB16 = 16;            // Filter bits
    static const uint8_t F27R1_FB17 = 17;            // Filter bits
    static const uint8_t F27R1_FB18 = 18;            // Filter bits
    static const uint8_t F27R1_FB19 = 19;            // Filter bits
    static const uint8_t F27R1_FB20 = 20;            // Filter bits
    static const uint8_t F27R1_FB21 = 21;            // Filter bits
    static const uint8_t F27R1_FB22 = 22;            // Filter bits
    static const uint8_t F27R1_FB23 = 23;            // Filter bits
    static const uint8_t F27R1_FB24 = 24;            // Filter bits
    static const uint8_t F27R1_FB25 = 25;            // Filter bits
    static const uint8_t F27R1_FB26 = 26;            // Filter bits
    static const uint8_t F27R1_FB27 = 27;            // Filter bits
    static const uint8_t F27R1_FB28 = 28;            // Filter bits
    static const uint8_t F27R1_FB29 = 29;            // Filter bits
    static const uint8_t F27R1_FB30 = 30;            // Filter bits
    static const uint8_t F27R1_FB31 = 31;            // Filter bits
    static const uint32_t F27R1_RESET_VALUE = 0x0;

    static const uint8_t F27R2_FB0 = 0;              // Filter bits
    static const uint8_t F27R2_FB1 = 1;              // Filter bits
    static const uint8_t F27R2_FB2 = 2;              // Filter bits
    static const uint8_t F27R2_FB3 = 3;              // Filter bits
    static const uint8_t F27R2_FB4 = 4;              // Filter bits
    static const uint8_t F27R2_FB5 = 5;              // Filter bits
    static const uint8_t F27R2_FB6 = 6;              // Filter bits
    static const uint8_t F27R2_FB7 = 7;              // Filter bits
    static const uint8_t F27R2_FB8 = 8;              // Filter bits
    static const uint8_t F27R2_FB9 = 9;              // Filter bits
    static const uint8_t F27R2_FB10 = 10;            // Filter bits
    static const uint8_t F27R2_FB11 = 11;            // Filter bits
    static const uint8_t F27R2_FB12 = 12;            // Filter bits
    static const uint8_t F27R2_FB13 = 13;            // Filter bits
    static const uint8_t F27R2_FB14 = 14;            // Filter bits
    static const uint8_t F27R2_FB15 = 15;            // Filter bits
    static const uint8_t F27R2_FB16 = 16;            // Filter bits
    static const uint8_t F27R2_FB17 = 17;            // Filter bits
    static const uint8_t F27R2_FB18 = 18;            // Filter bits
    static const uint8_t F27R2_FB19 = 19;            // Filter bits
    static const uint8_t F27R2_FB20 = 20;            // Filter bits
    static const uint8_t F27R2_FB21 = 21;            // Filter bits
    static const uint8_t F27R2_FB22 = 22;            // Filter bits
    static const uint8_t F27R2_FB23 = 23;            // Filter bits
    static const uint8_t F27R2_FB24 = 24;            // Filter bits
    static const uint8_t F27R2_FB25 = 25;            // Filter bits
    static const uint8_t F27R2_FB26 = 26;            // Filter bits
    static const uint8_t F27R2_FB27 = 27;            // Filter bits
    static const uint8_t F27R2_FB28 = 28;            // Filter bits
    static const uint8_t F27R2_FB29 = 29;            // Filter bits
    static const uint8_t F27R2_FB30 = 30;            // Filter bits
    static const uint8_t F27R2_FB31 = 31;            // Filter bits
    static const uint32_t F27R2_RESET_VALUE = 0x0;
};

static can_t& CAN = *reinterpret_cast<can_t*>(0x40006400);


////
//
//    Digital-to-analog converter
//
////

struct dac_t
{
    volatile uint32_t    CR;                   // [Read-write] control register
    volatile uint32_t    SWTRIGR;              // [Write-only] software trigger register
    volatile uint32_t    DHR12R1;              // [Read-write] channel1 12-bit right-aligned data holding register
    volatile uint32_t    DHR12L1;              // [Read-write] channel1 12-bit left aligned data holding register
    volatile uint32_t    DHR8R1;               // [Read-write] channel1 8-bit right aligned data holding register
    volatile uint32_t    DHR12R2;              // [Read-write] DAC channel2 12-bit right-aligned data holding register
    volatile uint32_t    DHR12L2;              // [Read-write] DAC channel2 12-bit left-aligned data holding register
    volatile uint32_t    DHR8R2;               // [Read-write] DAC channel2 8-bit right-aligned data holding register
    volatile uint32_t    DHR12RD;              // [Read-write] DHR12RD
    volatile uint32_t    DHR12LD;              // [Read-write] Dual DAC 12-bit left-aligned data holding register
    volatile uint32_t    DHR8RD;               // [Read-write] Dual DAC 8-bit right-aligned data holding register
    volatile uint32_t    DOR1;                 // [Read-only] channel1 data output register
    volatile uint32_t    DOR2;                 // [Read-only] DAC channel2 data output register
    volatile uint32_t    SR;                   // [Read-write] status register

    static const uint8_t CR_EN1 = 0;              // DAC channel1 enable
    static const uint8_t CR_BOFF1 = 1;            // DAC channel1 output buffer disable
    static const uint8_t CR_TEN1 = 2;             // DAC channel1 trigger enable
    static const uint8_t CR_TSEL1 = 3;            // DAC channel1 trigger selection (3 bits)
    static const uint8_t CR_WAVE1 = 6;            // DAC channel1 noise/triangle wave generation enable (2 bits)
    static const uint8_t CR_MAMP1 = 8;            // DAC channel1 mask/amplitude selector (4 bits)
    static const uint8_t CR_DMAEN1 = 12;          // DAC channel1 DMA enable
    static const uint8_t CR_DMAUDRIE1 = 13;       // DAC channel1 DMA Underrun Interrupt enable
    static const uint8_t CR_EN2 = 16;             // DAC channel2 enable
    static const uint8_t CR_BOFF2 = 17;           // DAC channel2 output buffer disable
    static const uint8_t CR_TEN2 = 18;            // DAC channel2 trigger enable
    static const uint8_t CR_TSEL2 = 19;           // DAC channel2 trigger selection (3 bits)
    static const uint8_t CR_WAVE2 = 22;           // DAC channel2 noise/triangle wave generation enable (2 bits)
    static const uint8_t CR_MAMP2 = 24;           // DAC channel2 mask/amplitude selector (4 bits)
    static const uint8_t CR_DMAEN2 = 28;          // DAC channel2 DMA enable
    static const uint8_t CR_DMAUDRIE2 = 29;       // DAC channel2 DMA underrun interrupt enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    static const uint8_t SWTRIGR_SWTRIG1 = 0;          // DAC channel1 software trigger
    static const uint8_t SWTRIGR_SWTRIG2 = 1;          // DAC channel2 software trigger
    static const uint32_t SWTRIGR_RESET_VALUE = 0x0;

    static const uint8_t DHR12R1_DACC1DHR = 0;         // DAC channel1 12-bit right-aligned data (12 bits)
    static const uint32_t DHR12R1_RESET_VALUE = 0x0;

    static const uint8_t DHR12L1_DACC1DHR = 4;         // DAC channel1 12-bit left-aligned data (12 bits)
    static const uint32_t DHR12L1_RESET_VALUE = 0x0;

    static const uint8_t DHR8R1_DACC1DHR = 0;         // DAC channel1 8-bit right-aligned data (8 bits)
    static const uint32_t DHR8R1_RESET_VALUE = 0x0;

    static const uint8_t DHR12R2_DACC2DHR = 0;         // DAC channel2 12-bit right-aligned data (12 bits)
    static const uint32_t DHR12R2_RESET_VALUE = 0x0;

    static const uint8_t DHR12L2_DACC2DHR = 4;         // DAC channel2 12-bit left-aligned data (12 bits)
    static const uint32_t DHR12L2_RESET_VALUE = 0x0;

    static const uint8_t DHR8R2_DACC2DHR = 0;         // DAC channel2 8-bit right-aligned data (8 bits)
    static const uint32_t DHR8R2_RESET_VALUE = 0x0;

    static const uint8_t DHR12RD_DACC1DHR = 0;         // DAC channel1 12-bit right-aligned data (12 bits)
    static const uint8_t DHR12RD_DACC2DHR = 16;        // DAC channel2 12-bit right-aligned data (12 bits)
    static const uint32_t DHR12RD_RESET_VALUE = 0x0;

    static const uint8_t DHR12LD_DACC1DHR = 4;         // DAC channel1 12-bit left-aligned data (12 bits)
    static const uint8_t DHR12LD_DACC2DHR = 20;        // DAC channel2 12-bit left-aligned data (12 bits)
    static const uint32_t DHR12LD_RESET_VALUE = 0x0;

    static const uint8_t DHR8RD_DACC2DHR = 8;         // DAC channel2 8-bit right-aligned data (8 bits)
    static const uint8_t DHR8RD_DACC1DHR = 0;         // DAC channel1 8-bit right-aligned data (8 bits)
    static const uint32_t DHR8RD_RESET_VALUE = 0x0;

    static const uint8_t DOR1_DACC1DOR = 0;         // DAC channel1 data output (12 bits)
    static const uint32_t DOR1_RESET_VALUE = 0x0;

    static const uint8_t DOR2_DACC2DOR = 0;         // DAC channel2 data output (12 bits)
    static const uint32_t DOR2_RESET_VALUE = 0x0;

    static const uint8_t SR_DMAUDR2 = 29;         // DAC channel2 DMA underrun flag
    static const uint8_t SR_DMAUDR1 = 13;         // DAC channel1 DMA underrun flag
    static const uint32_t SR_RESET_VALUE = 0x0;
};

static dac_t& DAC = *reinterpret_cast<dac_t*>(0x40007400);


////
//
//    System control block
//
////

struct scb_t
{
    volatile uint32_t    CPUID;                // [Read-only] CPUID base register
    volatile uint32_t    ICSR;                 // [Read-write] Interrupt control and state register
    reserved_t<1>        _0;
    volatile uint32_t    AIRCR;                // [Read-write] Application interrupt and reset control register
    volatile uint32_t    SCR;                  // [Read-write] System control register
    volatile uint32_t    CCR;                  // [Read-write] Configuration and control register
    reserved_t<1>        _1;
    volatile uint32_t    SHPR2;                // [Read-write] System handler priority registers
    volatile uint32_t    SHPR3;                // [Read-write] System handler priority registers

    static const uint8_t CPUID_Revision = 0;         // Revision number (4 bits)
    static const uint8_t CPUID_PartNo = 4;           // Part number of the processor (12 bits)
    static const uint8_t CPUID_Constant = 16;        // Reads as 0xF (4 bits)
    static const uint8_t CPUID_Variant = 20;         // Variant number (4 bits)
    static const uint8_t CPUID_Implementer = 24;     // Implementer code (8 bits)
    static const uint32_t CPUID_RESET_VALUE = 0x410fc241;

    static const uint8_t ICSR_VECTACTIVE = 0;       // Active vector (6 bits)
    static const uint8_t ICSR_VECTPENDING = 12;     // Pending vector (6 bits)
    static const uint8_t ICSR_ISRPENDING = 22;      // Interrupt pending flag
    static const uint8_t ICSR_PENDSTCLR = 25;       // SysTick exception clear-pending bit
    static const uint8_t ICSR_PENDSTSET = 26;       // SysTick exception set-pending bit
    static const uint8_t ICSR_PENDSVCLR = 27;       // PendSV clear-pending bit
    static const uint8_t ICSR_PENDSVSET = 28;       // PendSV set-pending bit
    static const uint8_t ICSR_NMIPENDSET = 31;      // NMI set-pending bit.
    static const uint32_t ICSR_RESET_VALUE = 0x0;

    static const uint8_t AIRCR_VECTCLRACTIVE = 1;    // VECTCLRACTIVE
    static const uint8_t AIRCR_SYSRESETREQ = 2;      // SYSRESETREQ
    static const uint8_t AIRCR_ENDIANESS = 15;       // ENDIANESS
    static const uint8_t AIRCR_VECTKEYSTAT = 16;     // Register key (16 bits)
    static const uint32_t AIRCR_RESET_VALUE = 0x0;

    static const uint8_t SCR_SLEEPONEXIT = 1;      // SLEEPONEXIT
    static const uint8_t SCR_SLEEPDEEP = 2;        // SLEEPDEEP
    static const uint8_t SCR_SEVEONPEND = 4;       // Send Event on Pending bit
    static const uint32_t SCR_RESET_VALUE = 0x0;

    static const uint8_t CCR_UNALIGN__TRP = 3;     // UNALIGN_ TRP
    static const uint8_t CCR_STKALIGN = 9;         // STKALIGN
    static const uint32_t CCR_RESET_VALUE = 0x0;

    static const uint8_t SHPR2_PRI_11 = 24;          // Priority of system handler 11 (8 bits)
    static const uint32_t SHPR2_RESET_VALUE = 0x0;

    static const uint8_t SHPR3_PRI_14 = 16;          // Priority of system handler 14 (8 bits)
    static const uint8_t SHPR3_PRI_15 = 24;          // Priority of system handler 15 (8 bits)
    static const uint32_t SHPR3_RESET_VALUE = 0x0;
};

static scb_t& SCB = *reinterpret_cast<scb_t*>(0xe000ed00);


////
//
//    SysTick timer
//
////

struct stk_t
{
    volatile uint32_t    CSR;                  // [Read-write] SysTick control and status register
    volatile uint32_t    RVR;                  // [Read-write] SysTick reload value register
    volatile uint32_t    CVR;                  // [Read-write] SysTick current value register
    volatile uint32_t    CALIB;                // [Read-write] SysTick calibration value register

    static const uint8_t CSR_ENABLE = 0;           // Counter enable
    static const uint8_t CSR_TICKINT = 1;          // SysTick exception request enable
    static const uint8_t CSR_CLKSOURCE = 2;        // Clock source selection
    static const uint8_t CSR_COUNTFLAG = 16;       // COUNTFLAG
    static const uint32_t CSR_RESET_VALUE = 0x0;

    static const uint8_t RVR_RELOAD = 0;           // RELOAD value (24 bits)
    static const uint32_t RVR_RESET_VALUE = 0x0;

    static const uint8_t CVR_CURRENT = 0;          // Current counter value (24 bits)
    static const uint32_t CVR_RESET_VALUE = 0x0;

    static const uint8_t CALIB_TENMS = 0;            // Calibration value (24 bits)
    static const uint8_t CALIB_SKEW = 30;            // SKEW flag: Indicates whether the TENMS value is exact
    static const uint8_t CALIB_NOREF = 31;           // NOREF flag. Reads as zero
    static const uint32_t CALIB_RESET_VALUE = 0x0;
};

static stk_t& STK = *reinterpret_cast<stk_t*>(0xe000e010);


}


