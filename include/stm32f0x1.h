#pragma once
#include "stm32.h"

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

////
//
//    cyclic redundancy check calculation unit
//
////

namespace crc
{

struct crc_t
{
    volatile uint32_t    DR;                   // [Read-write] Data register
    volatile uint32_t    IDR;                  // [Read-write] Independent data register
    volatile uint32_t    CR;                   // [Read-write] Control register
    volatile uint32_t    INIT;                 // [Read-write] Initial CRC value
};

crc_t& CRC = *reinterpret_cast<crc_t*>(0x40023000);

namespace DR // Data register fields
{
    static const uint8_t DR = 0;               // Data register bits (32 bits)
}

namespace IDR // Independent data register fields
{
    static const uint8_t IDR = 0;              // General-purpose 8-bit data register bits (8 bits)
}

namespace CR // Control register fields
{
    static const uint8_t RESET = 0;            // reset bit
    static const uint8_t POLYSIZE = 3;         // Polynomial size (2 bits)
    static const uint8_t REV_IN = 5;           // Reverse input data (2 bits)
    static const uint8_t REV_OUT = 7;          // Reverse output data
}

namespace INIT // Initial CRC value fields
{
    static const uint8_t INIT = 0;             // Programmable initial CRC value (32 bits)
}

}

////
//
//    General-purpose I/Os
//
////

namespace gpiof
{

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
};

gpiof_t& GPIOF = *reinterpret_cast<gpiof_t*>(0x48001400);

namespace MODER // GPIO port mode register fields
{
    static const uint8_t MODER15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
}

namespace OTYPER // GPIO port output type register fields
{
    static const uint8_t OT15 = 15;            // Port x configuration bit 15
    static const uint8_t OT14 = 14;            // Port x configuration bit 14
    static const uint8_t OT13 = 13;            // Port x configuration bit 13
    static const uint8_t OT12 = 12;            // Port x configuration bit 12
    static const uint8_t OT11 = 11;            // Port x configuration bit 11
    static const uint8_t OT10 = 10;            // Port x configuration bit 10
    static const uint8_t OT9 = 9;              // Port x configuration bit 9
    static const uint8_t OT8 = 8;              // Port x configuration bit 8
    static const uint8_t OT7 = 7;              // Port x configuration bit 7
    static const uint8_t OT6 = 6;              // Port x configuration bit 6
    static const uint8_t OT5 = 5;              // Port x configuration bit 5
    static const uint8_t OT4 = 4;              // Port x configuration bit 4
    static const uint8_t OT3 = 3;              // Port x configuration bit 3
    static const uint8_t OT2 = 2;              // Port x configuration bit 2
    static const uint8_t OT1 = 1;              // Port x configuration bit 1
    static const uint8_t OT0 = 0;              // Port x configuration bit 0
}

namespace OSPEEDR // GPIO port output speed register fields
{
    static const uint8_t OSPEEDR15 = 30;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR14 = 28;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR13 = 26;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR12 = 24;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR11 = 22;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR10 = 20;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR9 = 18;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR8 = 16;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR7 = 14;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR6 = 12;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR5 = 10;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR4 = 8;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR3 = 6;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR2 = 4;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR1 = 2;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR0 = 0;         // Port x configuration bits (y = 0..15) (2 bits)
}

namespace PUPDR // GPIO port pull-up/pull-down register fields
{
    static const uint8_t PUPDR15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
}

namespace IDR // GPIO port input data register fields
{
    static const uint8_t IDR15 = 15;           // Port input data (y = 0..15)
    static const uint8_t IDR14 = 14;           // Port input data (y = 0..15)
    static const uint8_t IDR13 = 13;           // Port input data (y = 0..15)
    static const uint8_t IDR12 = 12;           // Port input data (y = 0..15)
    static const uint8_t IDR11 = 11;           // Port input data (y = 0..15)
    static const uint8_t IDR10 = 10;           // Port input data (y = 0..15)
    static const uint8_t IDR9 = 9;             // Port input data (y = 0..15)
    static const uint8_t IDR8 = 8;             // Port input data (y = 0..15)
    static const uint8_t IDR7 = 7;             // Port input data (y = 0..15)
    static const uint8_t IDR6 = 6;             // Port input data (y = 0..15)
    static const uint8_t IDR5 = 5;             // Port input data (y = 0..15)
    static const uint8_t IDR4 = 4;             // Port input data (y = 0..15)
    static const uint8_t IDR3 = 3;             // Port input data (y = 0..15)
    static const uint8_t IDR2 = 2;             // Port input data (y = 0..15)
    static const uint8_t IDR1 = 1;             // Port input data (y = 0..15)
    static const uint8_t IDR0 = 0;             // Port input data (y = 0..15)
}

namespace ODR // GPIO port output data register fields
{
    static const uint8_t ODR15 = 15;           // Port output data (y = 0..15)
    static const uint8_t ODR14 = 14;           // Port output data (y = 0..15)
    static const uint8_t ODR13 = 13;           // Port output data (y = 0..15)
    static const uint8_t ODR12 = 12;           // Port output data (y = 0..15)
    static const uint8_t ODR11 = 11;           // Port output data (y = 0..15)
    static const uint8_t ODR10 = 10;           // Port output data (y = 0..15)
    static const uint8_t ODR9 = 9;             // Port output data (y = 0..15)
    static const uint8_t ODR8 = 8;             // Port output data (y = 0..15)
    static const uint8_t ODR7 = 7;             // Port output data (y = 0..15)
    static const uint8_t ODR6 = 6;             // Port output data (y = 0..15)
    static const uint8_t ODR5 = 5;             // Port output data (y = 0..15)
    static const uint8_t ODR4 = 4;             // Port output data (y = 0..15)
    static const uint8_t ODR3 = 3;             // Port output data (y = 0..15)
    static const uint8_t ODR2 = 2;             // Port output data (y = 0..15)
    static const uint8_t ODR1 = 1;             // Port output data (y = 0..15)
    static const uint8_t ODR0 = 0;             // Port output data (y = 0..15)
}

namespace BSRR // GPIO port bit set/reset register fields
{
    static const uint8_t BR15 = 31;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR14 = 30;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR13 = 29;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR12 = 28;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR11 = 27;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR10 = 26;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR9 = 25;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR8 = 24;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR7 = 23;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR6 = 22;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR5 = 21;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR4 = 20;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR3 = 19;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR2 = 18;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR1 = 17;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR0 = 16;             // Port x set bit y (y= 0..15)
    static const uint8_t BS15 = 15;            // Port x set bit y (y= 0..15)
    static const uint8_t BS14 = 14;            // Port x set bit y (y= 0..15)
    static const uint8_t BS13 = 13;            // Port x set bit y (y= 0..15)
    static const uint8_t BS12 = 12;            // Port x set bit y (y= 0..15)
    static const uint8_t BS11 = 11;            // Port x set bit y (y= 0..15)
    static const uint8_t BS10 = 10;            // Port x set bit y (y= 0..15)
    static const uint8_t BS9 = 9;              // Port x set bit y (y= 0..15)
    static const uint8_t BS8 = 8;              // Port x set bit y (y= 0..15)
    static const uint8_t BS7 = 7;              // Port x set bit y (y= 0..15)
    static const uint8_t BS6 = 6;              // Port x set bit y (y= 0..15)
    static const uint8_t BS5 = 5;              // Port x set bit y (y= 0..15)
    static const uint8_t BS4 = 4;              // Port x set bit y (y= 0..15)
    static const uint8_t BS3 = 3;              // Port x set bit y (y= 0..15)
    static const uint8_t BS2 = 2;              // Port x set bit y (y= 0..15)
    static const uint8_t BS1 = 1;              // Port x set bit y (y= 0..15)
    static const uint8_t BS0 = 0;              // Port x set bit y (y= 0..15)
}

namespace LCKR // GPIO port configuration lock register fields
{
    static const uint8_t LCKK = 16;            // Port x lock bit y
    static const uint8_t LCK15 = 15;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK14 = 14;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK13 = 13;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK12 = 12;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK11 = 11;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK10 = 10;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK9 = 9;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK8 = 8;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK7 = 7;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK6 = 6;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK5 = 5;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK4 = 4;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK3 = 3;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK2 = 2;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK1 = 1;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK0 = 0;             // Port x lock bit y (y= 0..15)
}

namespace AFRL // GPIO alternate function low register fields
{
    static const uint8_t AFRL7 = 28;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL6 = 24;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL5 = 20;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL4 = 16;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL3 = 12;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL2 = 8;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL1 = 4;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL0 = 0;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
}

namespace AFRH // GPIO alternate function high register fields
{
    static const uint8_t AFRH15 = 28;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH14 = 24;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH13 = 20;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH12 = 16;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH11 = 12;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH10 = 8;           // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH9 = 4;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH8 = 0;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
}

namespace BRR // Port bit reset register fields
{
    static const uint8_t BR0 = 0;              // Port x Reset bit y
    static const uint8_t BR1 = 1;              // Port x Reset bit y
    static const uint8_t BR2 = 2;              // Port x Reset bit y
    static const uint8_t BR3 = 3;              // Port x Reset bit y
    static const uint8_t BR4 = 4;              // Port x Reset bit y
    static const uint8_t BR5 = 5;              // Port x Reset bit y
    static const uint8_t BR6 = 6;              // Port x Reset bit y
    static const uint8_t BR7 = 7;              // Port x Reset bit y
    static const uint8_t BR8 = 8;              // Port x Reset bit y
    static const uint8_t BR9 = 9;              // Port x Reset bit y
    static const uint8_t BR10 = 10;            // Port x Reset bit y
    static const uint8_t BR11 = 11;            // Port x Reset bit y
    static const uint8_t BR12 = 12;            // Port x Reset bit y
    static const uint8_t BR13 = 13;            // Port x Reset bit y
    static const uint8_t BR14 = 14;            // Port x Reset bit y
    static const uint8_t BR15 = 15;            // Port x Reset bit y
}

}

////
//
//    General-purpose I/Os
//
////

namespace gpiod
{

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
};

gpiod_t& GPIOD = *reinterpret_cast<gpiod_t*>(0x48000c00);

namespace MODER // GPIO port mode register fields
{
    static const uint8_t MODER15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
}

namespace OTYPER // GPIO port output type register fields
{
    static const uint8_t OT15 = 15;            // Port x configuration bit 15
    static const uint8_t OT14 = 14;            // Port x configuration bit 14
    static const uint8_t OT13 = 13;            // Port x configuration bit 13
    static const uint8_t OT12 = 12;            // Port x configuration bit 12
    static const uint8_t OT11 = 11;            // Port x configuration bit 11
    static const uint8_t OT10 = 10;            // Port x configuration bit 10
    static const uint8_t OT9 = 9;              // Port x configuration bit 9
    static const uint8_t OT8 = 8;              // Port x configuration bit 8
    static const uint8_t OT7 = 7;              // Port x configuration bit 7
    static const uint8_t OT6 = 6;              // Port x configuration bit 6
    static const uint8_t OT5 = 5;              // Port x configuration bit 5
    static const uint8_t OT4 = 4;              // Port x configuration bit 4
    static const uint8_t OT3 = 3;              // Port x configuration bit 3
    static const uint8_t OT2 = 2;              // Port x configuration bit 2
    static const uint8_t OT1 = 1;              // Port x configuration bit 1
    static const uint8_t OT0 = 0;              // Port x configuration bit 0
}

namespace OSPEEDR // GPIO port output speed register fields
{
    static const uint8_t OSPEEDR15 = 30;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR14 = 28;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR13 = 26;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR12 = 24;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR11 = 22;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR10 = 20;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR9 = 18;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR8 = 16;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR7 = 14;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR6 = 12;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR5 = 10;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR4 = 8;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR3 = 6;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR2 = 4;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR1 = 2;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR0 = 0;         // Port x configuration bits (y = 0..15) (2 bits)
}

namespace PUPDR // GPIO port pull-up/pull-down register fields
{
    static const uint8_t PUPDR15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
}

namespace IDR // GPIO port input data register fields
{
    static const uint8_t IDR15 = 15;           // Port input data (y = 0..15)
    static const uint8_t IDR14 = 14;           // Port input data (y = 0..15)
    static const uint8_t IDR13 = 13;           // Port input data (y = 0..15)
    static const uint8_t IDR12 = 12;           // Port input data (y = 0..15)
    static const uint8_t IDR11 = 11;           // Port input data (y = 0..15)
    static const uint8_t IDR10 = 10;           // Port input data (y = 0..15)
    static const uint8_t IDR9 = 9;             // Port input data (y = 0..15)
    static const uint8_t IDR8 = 8;             // Port input data (y = 0..15)
    static const uint8_t IDR7 = 7;             // Port input data (y = 0..15)
    static const uint8_t IDR6 = 6;             // Port input data (y = 0..15)
    static const uint8_t IDR5 = 5;             // Port input data (y = 0..15)
    static const uint8_t IDR4 = 4;             // Port input data (y = 0..15)
    static const uint8_t IDR3 = 3;             // Port input data (y = 0..15)
    static const uint8_t IDR2 = 2;             // Port input data (y = 0..15)
    static const uint8_t IDR1 = 1;             // Port input data (y = 0..15)
    static const uint8_t IDR0 = 0;             // Port input data (y = 0..15)
}

namespace ODR // GPIO port output data register fields
{
    static const uint8_t ODR15 = 15;           // Port output data (y = 0..15)
    static const uint8_t ODR14 = 14;           // Port output data (y = 0..15)
    static const uint8_t ODR13 = 13;           // Port output data (y = 0..15)
    static const uint8_t ODR12 = 12;           // Port output data (y = 0..15)
    static const uint8_t ODR11 = 11;           // Port output data (y = 0..15)
    static const uint8_t ODR10 = 10;           // Port output data (y = 0..15)
    static const uint8_t ODR9 = 9;             // Port output data (y = 0..15)
    static const uint8_t ODR8 = 8;             // Port output data (y = 0..15)
    static const uint8_t ODR7 = 7;             // Port output data (y = 0..15)
    static const uint8_t ODR6 = 6;             // Port output data (y = 0..15)
    static const uint8_t ODR5 = 5;             // Port output data (y = 0..15)
    static const uint8_t ODR4 = 4;             // Port output data (y = 0..15)
    static const uint8_t ODR3 = 3;             // Port output data (y = 0..15)
    static const uint8_t ODR2 = 2;             // Port output data (y = 0..15)
    static const uint8_t ODR1 = 1;             // Port output data (y = 0..15)
    static const uint8_t ODR0 = 0;             // Port output data (y = 0..15)
}

namespace BSRR // GPIO port bit set/reset register fields
{
    static const uint8_t BR15 = 31;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR14 = 30;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR13 = 29;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR12 = 28;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR11 = 27;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR10 = 26;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR9 = 25;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR8 = 24;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR7 = 23;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR6 = 22;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR5 = 21;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR4 = 20;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR3 = 19;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR2 = 18;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR1 = 17;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR0 = 16;             // Port x set bit y (y= 0..15)
    static const uint8_t BS15 = 15;            // Port x set bit y (y= 0..15)
    static const uint8_t BS14 = 14;            // Port x set bit y (y= 0..15)
    static const uint8_t BS13 = 13;            // Port x set bit y (y= 0..15)
    static const uint8_t BS12 = 12;            // Port x set bit y (y= 0..15)
    static const uint8_t BS11 = 11;            // Port x set bit y (y= 0..15)
    static const uint8_t BS10 = 10;            // Port x set bit y (y= 0..15)
    static const uint8_t BS9 = 9;              // Port x set bit y (y= 0..15)
    static const uint8_t BS8 = 8;              // Port x set bit y (y= 0..15)
    static const uint8_t BS7 = 7;              // Port x set bit y (y= 0..15)
    static const uint8_t BS6 = 6;              // Port x set bit y (y= 0..15)
    static const uint8_t BS5 = 5;              // Port x set bit y (y= 0..15)
    static const uint8_t BS4 = 4;              // Port x set bit y (y= 0..15)
    static const uint8_t BS3 = 3;              // Port x set bit y (y= 0..15)
    static const uint8_t BS2 = 2;              // Port x set bit y (y= 0..15)
    static const uint8_t BS1 = 1;              // Port x set bit y (y= 0..15)
    static const uint8_t BS0 = 0;              // Port x set bit y (y= 0..15)
}

namespace LCKR // GPIO port configuration lock register fields
{
    static const uint8_t LCKK = 16;            // Port x lock bit y
    static const uint8_t LCK15 = 15;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK14 = 14;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK13 = 13;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK12 = 12;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK11 = 11;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK10 = 10;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK9 = 9;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK8 = 8;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK7 = 7;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK6 = 6;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK5 = 5;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK4 = 4;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK3 = 3;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK2 = 2;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK1 = 1;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK0 = 0;             // Port x lock bit y (y= 0..15)
}

namespace AFRL // GPIO alternate function low register fields
{
    static const uint8_t AFRL7 = 28;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL6 = 24;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL5 = 20;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL4 = 16;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL3 = 12;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL2 = 8;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL1 = 4;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL0 = 0;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
}

namespace AFRH // GPIO alternate function high register fields
{
    static const uint8_t AFRH15 = 28;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH14 = 24;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH13 = 20;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH12 = 16;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH11 = 12;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH10 = 8;           // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH9 = 4;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH8 = 0;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
}

namespace BRR // Port bit reset register fields
{
    static const uint8_t BR0 = 0;              // Port x Reset bit y
    static const uint8_t BR1 = 1;              // Port x Reset bit y
    static const uint8_t BR2 = 2;              // Port x Reset bit y
    static const uint8_t BR3 = 3;              // Port x Reset bit y
    static const uint8_t BR4 = 4;              // Port x Reset bit y
    static const uint8_t BR5 = 5;              // Port x Reset bit y
    static const uint8_t BR6 = 6;              // Port x Reset bit y
    static const uint8_t BR7 = 7;              // Port x Reset bit y
    static const uint8_t BR8 = 8;              // Port x Reset bit y
    static const uint8_t BR9 = 9;              // Port x Reset bit y
    static const uint8_t BR10 = 10;            // Port x Reset bit y
    static const uint8_t BR11 = 11;            // Port x Reset bit y
    static const uint8_t BR12 = 12;            // Port x Reset bit y
    static const uint8_t BR13 = 13;            // Port x Reset bit y
    static const uint8_t BR14 = 14;            // Port x Reset bit y
    static const uint8_t BR15 = 15;            // Port x Reset bit y
}

}

////
//
//    General-purpose I/Os
//
////

namespace gpioc
{

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
};

gpioc_t& GPIOC = *reinterpret_cast<gpioc_t*>(0x48000800);

namespace MODER // GPIO port mode register fields
{
    static const uint8_t MODER15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
}

namespace OTYPER // GPIO port output type register fields
{
    static const uint8_t OT15 = 15;            // Port x configuration bit 15
    static const uint8_t OT14 = 14;            // Port x configuration bit 14
    static const uint8_t OT13 = 13;            // Port x configuration bit 13
    static const uint8_t OT12 = 12;            // Port x configuration bit 12
    static const uint8_t OT11 = 11;            // Port x configuration bit 11
    static const uint8_t OT10 = 10;            // Port x configuration bit 10
    static const uint8_t OT9 = 9;              // Port x configuration bit 9
    static const uint8_t OT8 = 8;              // Port x configuration bit 8
    static const uint8_t OT7 = 7;              // Port x configuration bit 7
    static const uint8_t OT6 = 6;              // Port x configuration bit 6
    static const uint8_t OT5 = 5;              // Port x configuration bit 5
    static const uint8_t OT4 = 4;              // Port x configuration bit 4
    static const uint8_t OT3 = 3;              // Port x configuration bit 3
    static const uint8_t OT2 = 2;              // Port x configuration bit 2
    static const uint8_t OT1 = 1;              // Port x configuration bit 1
    static const uint8_t OT0 = 0;              // Port x configuration bit 0
}

namespace OSPEEDR // GPIO port output speed register fields
{
    static const uint8_t OSPEEDR15 = 30;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR14 = 28;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR13 = 26;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR12 = 24;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR11 = 22;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR10 = 20;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR9 = 18;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR8 = 16;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR7 = 14;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR6 = 12;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR5 = 10;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR4 = 8;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR3 = 6;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR2 = 4;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR1 = 2;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR0 = 0;         // Port x configuration bits (y = 0..15) (2 bits)
}

namespace PUPDR // GPIO port pull-up/pull-down register fields
{
    static const uint8_t PUPDR15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
}

namespace IDR // GPIO port input data register fields
{
    static const uint8_t IDR15 = 15;           // Port input data (y = 0..15)
    static const uint8_t IDR14 = 14;           // Port input data (y = 0..15)
    static const uint8_t IDR13 = 13;           // Port input data (y = 0..15)
    static const uint8_t IDR12 = 12;           // Port input data (y = 0..15)
    static const uint8_t IDR11 = 11;           // Port input data (y = 0..15)
    static const uint8_t IDR10 = 10;           // Port input data (y = 0..15)
    static const uint8_t IDR9 = 9;             // Port input data (y = 0..15)
    static const uint8_t IDR8 = 8;             // Port input data (y = 0..15)
    static const uint8_t IDR7 = 7;             // Port input data (y = 0..15)
    static const uint8_t IDR6 = 6;             // Port input data (y = 0..15)
    static const uint8_t IDR5 = 5;             // Port input data (y = 0..15)
    static const uint8_t IDR4 = 4;             // Port input data (y = 0..15)
    static const uint8_t IDR3 = 3;             // Port input data (y = 0..15)
    static const uint8_t IDR2 = 2;             // Port input data (y = 0..15)
    static const uint8_t IDR1 = 1;             // Port input data (y = 0..15)
    static const uint8_t IDR0 = 0;             // Port input data (y = 0..15)
}

namespace ODR // GPIO port output data register fields
{
    static const uint8_t ODR15 = 15;           // Port output data (y = 0..15)
    static const uint8_t ODR14 = 14;           // Port output data (y = 0..15)
    static const uint8_t ODR13 = 13;           // Port output data (y = 0..15)
    static const uint8_t ODR12 = 12;           // Port output data (y = 0..15)
    static const uint8_t ODR11 = 11;           // Port output data (y = 0..15)
    static const uint8_t ODR10 = 10;           // Port output data (y = 0..15)
    static const uint8_t ODR9 = 9;             // Port output data (y = 0..15)
    static const uint8_t ODR8 = 8;             // Port output data (y = 0..15)
    static const uint8_t ODR7 = 7;             // Port output data (y = 0..15)
    static const uint8_t ODR6 = 6;             // Port output data (y = 0..15)
    static const uint8_t ODR5 = 5;             // Port output data (y = 0..15)
    static const uint8_t ODR4 = 4;             // Port output data (y = 0..15)
    static const uint8_t ODR3 = 3;             // Port output data (y = 0..15)
    static const uint8_t ODR2 = 2;             // Port output data (y = 0..15)
    static const uint8_t ODR1 = 1;             // Port output data (y = 0..15)
    static const uint8_t ODR0 = 0;             // Port output data (y = 0..15)
}

namespace BSRR // GPIO port bit set/reset register fields
{
    static const uint8_t BR15 = 31;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR14 = 30;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR13 = 29;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR12 = 28;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR11 = 27;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR10 = 26;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR9 = 25;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR8 = 24;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR7 = 23;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR6 = 22;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR5 = 21;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR4 = 20;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR3 = 19;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR2 = 18;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR1 = 17;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR0 = 16;             // Port x set bit y (y= 0..15)
    static const uint8_t BS15 = 15;            // Port x set bit y (y= 0..15)
    static const uint8_t BS14 = 14;            // Port x set bit y (y= 0..15)
    static const uint8_t BS13 = 13;            // Port x set bit y (y= 0..15)
    static const uint8_t BS12 = 12;            // Port x set bit y (y= 0..15)
    static const uint8_t BS11 = 11;            // Port x set bit y (y= 0..15)
    static const uint8_t BS10 = 10;            // Port x set bit y (y= 0..15)
    static const uint8_t BS9 = 9;              // Port x set bit y (y= 0..15)
    static const uint8_t BS8 = 8;              // Port x set bit y (y= 0..15)
    static const uint8_t BS7 = 7;              // Port x set bit y (y= 0..15)
    static const uint8_t BS6 = 6;              // Port x set bit y (y= 0..15)
    static const uint8_t BS5 = 5;              // Port x set bit y (y= 0..15)
    static const uint8_t BS4 = 4;              // Port x set bit y (y= 0..15)
    static const uint8_t BS3 = 3;              // Port x set bit y (y= 0..15)
    static const uint8_t BS2 = 2;              // Port x set bit y (y= 0..15)
    static const uint8_t BS1 = 1;              // Port x set bit y (y= 0..15)
    static const uint8_t BS0 = 0;              // Port x set bit y (y= 0..15)
}

namespace LCKR // GPIO port configuration lock register fields
{
    static const uint8_t LCKK = 16;            // Port x lock bit y
    static const uint8_t LCK15 = 15;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK14 = 14;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK13 = 13;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK12 = 12;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK11 = 11;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK10 = 10;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK9 = 9;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK8 = 8;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK7 = 7;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK6 = 6;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK5 = 5;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK4 = 4;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK3 = 3;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK2 = 2;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK1 = 1;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK0 = 0;             // Port x lock bit y (y= 0..15)
}

namespace AFRL // GPIO alternate function low register fields
{
    static const uint8_t AFRL7 = 28;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL6 = 24;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL5 = 20;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL4 = 16;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL3 = 12;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL2 = 8;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL1 = 4;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL0 = 0;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
}

namespace AFRH // GPIO alternate function high register fields
{
    static const uint8_t AFRH15 = 28;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH14 = 24;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH13 = 20;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH12 = 16;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH11 = 12;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH10 = 8;           // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH9 = 4;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH8 = 0;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
}

namespace BRR // Port bit reset register fields
{
    static const uint8_t BR0 = 0;              // Port x Reset bit y
    static const uint8_t BR1 = 1;              // Port x Reset bit y
    static const uint8_t BR2 = 2;              // Port x Reset bit y
    static const uint8_t BR3 = 3;              // Port x Reset bit y
    static const uint8_t BR4 = 4;              // Port x Reset bit y
    static const uint8_t BR5 = 5;              // Port x Reset bit y
    static const uint8_t BR6 = 6;              // Port x Reset bit y
    static const uint8_t BR7 = 7;              // Port x Reset bit y
    static const uint8_t BR8 = 8;              // Port x Reset bit y
    static const uint8_t BR9 = 9;              // Port x Reset bit y
    static const uint8_t BR10 = 10;            // Port x Reset bit y
    static const uint8_t BR11 = 11;            // Port x Reset bit y
    static const uint8_t BR12 = 12;            // Port x Reset bit y
    static const uint8_t BR13 = 13;            // Port x Reset bit y
    static const uint8_t BR14 = 14;            // Port x Reset bit y
    static const uint8_t BR15 = 15;            // Port x Reset bit y
}

}

////
//
//    General-purpose I/Os
//
////

namespace gpiob
{

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
};

gpiob_t& GPIOB = *reinterpret_cast<gpiob_t*>(0x48000400);

namespace MODER // GPIO port mode register fields
{
    static const uint8_t MODER15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
}

namespace OTYPER // GPIO port output type register fields
{
    static const uint8_t OT15 = 15;            // Port x configuration bit 15
    static const uint8_t OT14 = 14;            // Port x configuration bit 14
    static const uint8_t OT13 = 13;            // Port x configuration bit 13
    static const uint8_t OT12 = 12;            // Port x configuration bit 12
    static const uint8_t OT11 = 11;            // Port x configuration bit 11
    static const uint8_t OT10 = 10;            // Port x configuration bit 10
    static const uint8_t OT9 = 9;              // Port x configuration bit 9
    static const uint8_t OT8 = 8;              // Port x configuration bit 8
    static const uint8_t OT7 = 7;              // Port x configuration bit 7
    static const uint8_t OT6 = 6;              // Port x configuration bit 6
    static const uint8_t OT5 = 5;              // Port x configuration bit 5
    static const uint8_t OT4 = 4;              // Port x configuration bit 4
    static const uint8_t OT3 = 3;              // Port x configuration bit 3
    static const uint8_t OT2 = 2;              // Port x configuration bit 2
    static const uint8_t OT1 = 1;              // Port x configuration bit 1
    static const uint8_t OT0 = 0;              // Port x configuration bit 0
}

namespace OSPEEDR // GPIO port output speed register fields
{
    static const uint8_t OSPEEDR15 = 30;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR14 = 28;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR13 = 26;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR12 = 24;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR11 = 22;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR10 = 20;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR9 = 18;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR8 = 16;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR7 = 14;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR6 = 12;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR5 = 10;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR4 = 8;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR3 = 6;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR2 = 4;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR1 = 2;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR0 = 0;         // Port x configuration bits (y = 0..15) (2 bits)
}

namespace PUPDR // GPIO port pull-up/pull-down register fields
{
    static const uint8_t PUPDR15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
}

namespace IDR // GPIO port input data register fields
{
    static const uint8_t IDR15 = 15;           // Port input data (y = 0..15)
    static const uint8_t IDR14 = 14;           // Port input data (y = 0..15)
    static const uint8_t IDR13 = 13;           // Port input data (y = 0..15)
    static const uint8_t IDR12 = 12;           // Port input data (y = 0..15)
    static const uint8_t IDR11 = 11;           // Port input data (y = 0..15)
    static const uint8_t IDR10 = 10;           // Port input data (y = 0..15)
    static const uint8_t IDR9 = 9;             // Port input data (y = 0..15)
    static const uint8_t IDR8 = 8;             // Port input data (y = 0..15)
    static const uint8_t IDR7 = 7;             // Port input data (y = 0..15)
    static const uint8_t IDR6 = 6;             // Port input data (y = 0..15)
    static const uint8_t IDR5 = 5;             // Port input data (y = 0..15)
    static const uint8_t IDR4 = 4;             // Port input data (y = 0..15)
    static const uint8_t IDR3 = 3;             // Port input data (y = 0..15)
    static const uint8_t IDR2 = 2;             // Port input data (y = 0..15)
    static const uint8_t IDR1 = 1;             // Port input data (y = 0..15)
    static const uint8_t IDR0 = 0;             // Port input data (y = 0..15)
}

namespace ODR // GPIO port output data register fields
{
    static const uint8_t ODR15 = 15;           // Port output data (y = 0..15)
    static const uint8_t ODR14 = 14;           // Port output data (y = 0..15)
    static const uint8_t ODR13 = 13;           // Port output data (y = 0..15)
    static const uint8_t ODR12 = 12;           // Port output data (y = 0..15)
    static const uint8_t ODR11 = 11;           // Port output data (y = 0..15)
    static const uint8_t ODR10 = 10;           // Port output data (y = 0..15)
    static const uint8_t ODR9 = 9;             // Port output data (y = 0..15)
    static const uint8_t ODR8 = 8;             // Port output data (y = 0..15)
    static const uint8_t ODR7 = 7;             // Port output data (y = 0..15)
    static const uint8_t ODR6 = 6;             // Port output data (y = 0..15)
    static const uint8_t ODR5 = 5;             // Port output data (y = 0..15)
    static const uint8_t ODR4 = 4;             // Port output data (y = 0..15)
    static const uint8_t ODR3 = 3;             // Port output data (y = 0..15)
    static const uint8_t ODR2 = 2;             // Port output data (y = 0..15)
    static const uint8_t ODR1 = 1;             // Port output data (y = 0..15)
    static const uint8_t ODR0 = 0;             // Port output data (y = 0..15)
}

namespace BSRR // GPIO port bit set/reset register fields
{
    static const uint8_t BR15 = 31;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR14 = 30;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR13 = 29;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR12 = 28;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR11 = 27;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR10 = 26;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR9 = 25;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR8 = 24;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR7 = 23;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR6 = 22;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR5 = 21;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR4 = 20;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR3 = 19;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR2 = 18;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR1 = 17;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR0 = 16;             // Port x set bit y (y= 0..15)
    static const uint8_t BS15 = 15;            // Port x set bit y (y= 0..15)
    static const uint8_t BS14 = 14;            // Port x set bit y (y= 0..15)
    static const uint8_t BS13 = 13;            // Port x set bit y (y= 0..15)
    static const uint8_t BS12 = 12;            // Port x set bit y (y= 0..15)
    static const uint8_t BS11 = 11;            // Port x set bit y (y= 0..15)
    static const uint8_t BS10 = 10;            // Port x set bit y (y= 0..15)
    static const uint8_t BS9 = 9;              // Port x set bit y (y= 0..15)
    static const uint8_t BS8 = 8;              // Port x set bit y (y= 0..15)
    static const uint8_t BS7 = 7;              // Port x set bit y (y= 0..15)
    static const uint8_t BS6 = 6;              // Port x set bit y (y= 0..15)
    static const uint8_t BS5 = 5;              // Port x set bit y (y= 0..15)
    static const uint8_t BS4 = 4;              // Port x set bit y (y= 0..15)
    static const uint8_t BS3 = 3;              // Port x set bit y (y= 0..15)
    static const uint8_t BS2 = 2;              // Port x set bit y (y= 0..15)
    static const uint8_t BS1 = 1;              // Port x set bit y (y= 0..15)
    static const uint8_t BS0 = 0;              // Port x set bit y (y= 0..15)
}

namespace LCKR // GPIO port configuration lock register fields
{
    static const uint8_t LCKK = 16;            // Port x lock bit y
    static const uint8_t LCK15 = 15;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK14 = 14;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK13 = 13;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK12 = 12;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK11 = 11;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK10 = 10;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK9 = 9;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK8 = 8;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK7 = 7;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK6 = 6;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK5 = 5;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK4 = 4;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK3 = 3;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK2 = 2;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK1 = 1;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK0 = 0;             // Port x lock bit y (y= 0..15)
}

namespace AFRL // GPIO alternate function low register fields
{
    static const uint8_t AFRL7 = 28;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL6 = 24;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL5 = 20;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL4 = 16;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL3 = 12;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL2 = 8;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL1 = 4;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL0 = 0;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
}

namespace AFRH // GPIO alternate function high register fields
{
    static const uint8_t AFRH15 = 28;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH14 = 24;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH13 = 20;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH12 = 16;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH11 = 12;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH10 = 8;           // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH9 = 4;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH8 = 0;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
}

namespace BRR // Port bit reset register fields
{
    static const uint8_t BR0 = 0;              // Port x Reset bit y
    static const uint8_t BR1 = 1;              // Port x Reset bit y
    static const uint8_t BR2 = 2;              // Port x Reset bit y
    static const uint8_t BR3 = 3;              // Port x Reset bit y
    static const uint8_t BR4 = 4;              // Port x Reset bit y
    static const uint8_t BR5 = 5;              // Port x Reset bit y
    static const uint8_t BR6 = 6;              // Port x Reset bit y
    static const uint8_t BR7 = 7;              // Port x Reset bit y
    static const uint8_t BR8 = 8;              // Port x Reset bit y
    static const uint8_t BR9 = 9;              // Port x Reset bit y
    static const uint8_t BR10 = 10;            // Port x Reset bit y
    static const uint8_t BR11 = 11;            // Port x Reset bit y
    static const uint8_t BR12 = 12;            // Port x Reset bit y
    static const uint8_t BR13 = 13;            // Port x Reset bit y
    static const uint8_t BR14 = 14;            // Port x Reset bit y
    static const uint8_t BR15 = 15;            // Port x Reset bit y
}

}

////
//
//    General-purpose I/Os
//
////

namespace gpioe
{

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
};

gpioe_t& GPIOE = *reinterpret_cast<gpioe_t*>(0x48001000);

namespace MODER // GPIO port mode register fields
{
    static const uint8_t MODER15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
}

namespace OTYPER // GPIO port output type register fields
{
    static const uint8_t OT15 = 15;            // Port x configuration bit 15
    static const uint8_t OT14 = 14;            // Port x configuration bit 14
    static const uint8_t OT13 = 13;            // Port x configuration bit 13
    static const uint8_t OT12 = 12;            // Port x configuration bit 12
    static const uint8_t OT11 = 11;            // Port x configuration bit 11
    static const uint8_t OT10 = 10;            // Port x configuration bit 10
    static const uint8_t OT9 = 9;              // Port x configuration bit 9
    static const uint8_t OT8 = 8;              // Port x configuration bit 8
    static const uint8_t OT7 = 7;              // Port x configuration bit 7
    static const uint8_t OT6 = 6;              // Port x configuration bit 6
    static const uint8_t OT5 = 5;              // Port x configuration bit 5
    static const uint8_t OT4 = 4;              // Port x configuration bit 4
    static const uint8_t OT3 = 3;              // Port x configuration bit 3
    static const uint8_t OT2 = 2;              // Port x configuration bit 2
    static const uint8_t OT1 = 1;              // Port x configuration bit 1
    static const uint8_t OT0 = 0;              // Port x configuration bit 0
}

namespace OSPEEDR // GPIO port output speed register fields
{
    static const uint8_t OSPEEDR15 = 30;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR14 = 28;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR13 = 26;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR12 = 24;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR11 = 22;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR10 = 20;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR9 = 18;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR8 = 16;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR7 = 14;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR6 = 12;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR5 = 10;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR4 = 8;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR3 = 6;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR2 = 4;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR1 = 2;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR0 = 0;         // Port x configuration bits (y = 0..15) (2 bits)
}

namespace PUPDR // GPIO port pull-up/pull-down register fields
{
    static const uint8_t PUPDR15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
}

namespace IDR // GPIO port input data register fields
{
    static const uint8_t IDR15 = 15;           // Port input data (y = 0..15)
    static const uint8_t IDR14 = 14;           // Port input data (y = 0..15)
    static const uint8_t IDR13 = 13;           // Port input data (y = 0..15)
    static const uint8_t IDR12 = 12;           // Port input data (y = 0..15)
    static const uint8_t IDR11 = 11;           // Port input data (y = 0..15)
    static const uint8_t IDR10 = 10;           // Port input data (y = 0..15)
    static const uint8_t IDR9 = 9;             // Port input data (y = 0..15)
    static const uint8_t IDR8 = 8;             // Port input data (y = 0..15)
    static const uint8_t IDR7 = 7;             // Port input data (y = 0..15)
    static const uint8_t IDR6 = 6;             // Port input data (y = 0..15)
    static const uint8_t IDR5 = 5;             // Port input data (y = 0..15)
    static const uint8_t IDR4 = 4;             // Port input data (y = 0..15)
    static const uint8_t IDR3 = 3;             // Port input data (y = 0..15)
    static const uint8_t IDR2 = 2;             // Port input data (y = 0..15)
    static const uint8_t IDR1 = 1;             // Port input data (y = 0..15)
    static const uint8_t IDR0 = 0;             // Port input data (y = 0..15)
}

namespace ODR // GPIO port output data register fields
{
    static const uint8_t ODR15 = 15;           // Port output data (y = 0..15)
    static const uint8_t ODR14 = 14;           // Port output data (y = 0..15)
    static const uint8_t ODR13 = 13;           // Port output data (y = 0..15)
    static const uint8_t ODR12 = 12;           // Port output data (y = 0..15)
    static const uint8_t ODR11 = 11;           // Port output data (y = 0..15)
    static const uint8_t ODR10 = 10;           // Port output data (y = 0..15)
    static const uint8_t ODR9 = 9;             // Port output data (y = 0..15)
    static const uint8_t ODR8 = 8;             // Port output data (y = 0..15)
    static const uint8_t ODR7 = 7;             // Port output data (y = 0..15)
    static const uint8_t ODR6 = 6;             // Port output data (y = 0..15)
    static const uint8_t ODR5 = 5;             // Port output data (y = 0..15)
    static const uint8_t ODR4 = 4;             // Port output data (y = 0..15)
    static const uint8_t ODR3 = 3;             // Port output data (y = 0..15)
    static const uint8_t ODR2 = 2;             // Port output data (y = 0..15)
    static const uint8_t ODR1 = 1;             // Port output data (y = 0..15)
    static const uint8_t ODR0 = 0;             // Port output data (y = 0..15)
}

namespace BSRR // GPIO port bit set/reset register fields
{
    static const uint8_t BR15 = 31;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR14 = 30;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR13 = 29;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR12 = 28;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR11 = 27;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR10 = 26;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR9 = 25;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR8 = 24;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR7 = 23;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR6 = 22;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR5 = 21;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR4 = 20;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR3 = 19;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR2 = 18;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR1 = 17;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR0 = 16;             // Port x set bit y (y= 0..15)
    static const uint8_t BS15 = 15;            // Port x set bit y (y= 0..15)
    static const uint8_t BS14 = 14;            // Port x set bit y (y= 0..15)
    static const uint8_t BS13 = 13;            // Port x set bit y (y= 0..15)
    static const uint8_t BS12 = 12;            // Port x set bit y (y= 0..15)
    static const uint8_t BS11 = 11;            // Port x set bit y (y= 0..15)
    static const uint8_t BS10 = 10;            // Port x set bit y (y= 0..15)
    static const uint8_t BS9 = 9;              // Port x set bit y (y= 0..15)
    static const uint8_t BS8 = 8;              // Port x set bit y (y= 0..15)
    static const uint8_t BS7 = 7;              // Port x set bit y (y= 0..15)
    static const uint8_t BS6 = 6;              // Port x set bit y (y= 0..15)
    static const uint8_t BS5 = 5;              // Port x set bit y (y= 0..15)
    static const uint8_t BS4 = 4;              // Port x set bit y (y= 0..15)
    static const uint8_t BS3 = 3;              // Port x set bit y (y= 0..15)
    static const uint8_t BS2 = 2;              // Port x set bit y (y= 0..15)
    static const uint8_t BS1 = 1;              // Port x set bit y (y= 0..15)
    static const uint8_t BS0 = 0;              // Port x set bit y (y= 0..15)
}

namespace LCKR // GPIO port configuration lock register fields
{
    static const uint8_t LCKK = 16;            // Port x lock bit y
    static const uint8_t LCK15 = 15;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK14 = 14;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK13 = 13;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK12 = 12;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK11 = 11;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK10 = 10;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK9 = 9;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK8 = 8;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK7 = 7;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK6 = 6;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK5 = 5;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK4 = 4;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK3 = 3;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK2 = 2;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK1 = 1;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK0 = 0;             // Port x lock bit y (y= 0..15)
}

namespace AFRL // GPIO alternate function low register fields
{
    static const uint8_t AFRL7 = 28;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL6 = 24;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL5 = 20;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL4 = 16;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL3 = 12;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL2 = 8;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL1 = 4;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL0 = 0;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
}

namespace AFRH // GPIO alternate function high register fields
{
    static const uint8_t AFRH15 = 28;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH14 = 24;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH13 = 20;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH12 = 16;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH11 = 12;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH10 = 8;           // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH9 = 4;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH8 = 0;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
}

namespace BRR // Port bit reset register fields
{
    static const uint8_t BR0 = 0;              // Port x Reset bit y
    static const uint8_t BR1 = 1;              // Port x Reset bit y
    static const uint8_t BR2 = 2;              // Port x Reset bit y
    static const uint8_t BR3 = 3;              // Port x Reset bit y
    static const uint8_t BR4 = 4;              // Port x Reset bit y
    static const uint8_t BR5 = 5;              // Port x Reset bit y
    static const uint8_t BR6 = 6;              // Port x Reset bit y
    static const uint8_t BR7 = 7;              // Port x Reset bit y
    static const uint8_t BR8 = 8;              // Port x Reset bit y
    static const uint8_t BR9 = 9;              // Port x Reset bit y
    static const uint8_t BR10 = 10;            // Port x Reset bit y
    static const uint8_t BR11 = 11;            // Port x Reset bit y
    static const uint8_t BR12 = 12;            // Port x Reset bit y
    static const uint8_t BR13 = 13;            // Port x Reset bit y
    static const uint8_t BR14 = 14;            // Port x Reset bit y
    static const uint8_t BR15 = 15;            // Port x Reset bit y
}

}

////
//
//    General-purpose I/Os
//
////

namespace gpioa
{

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
};

gpioa_t& GPIOA = *reinterpret_cast<gpioa_t*>(0x48000000);

namespace MODER // GPIO port mode register fields
{
    static const uint8_t MODER15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t MODER0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
}

namespace OTYPER // GPIO port output type register fields
{
    static const uint8_t OT15 = 15;            // Port x configuration bits (y = 0..15)
    static const uint8_t OT14 = 14;            // Port x configuration bits (y = 0..15)
    static const uint8_t OT13 = 13;            // Port x configuration bits (y = 0..15)
    static const uint8_t OT12 = 12;            // Port x configuration bits (y = 0..15)
    static const uint8_t OT11 = 11;            // Port x configuration bits (y = 0..15)
    static const uint8_t OT10 = 10;            // Port x configuration bits (y = 0..15)
    static const uint8_t OT9 = 9;              // Port x configuration bits (y = 0..15)
    static const uint8_t OT8 = 8;              // Port x configuration bits (y = 0..15)
    static const uint8_t OT7 = 7;              // Port x configuration bits (y = 0..15)
    static const uint8_t OT6 = 6;              // Port x configuration bits (y = 0..15)
    static const uint8_t OT5 = 5;              // Port x configuration bits (y = 0..15)
    static const uint8_t OT4 = 4;              // Port x configuration bits (y = 0..15)
    static const uint8_t OT3 = 3;              // Port x configuration bits (y = 0..15)
    static const uint8_t OT2 = 2;              // Port x configuration bits (y = 0..15)
    static const uint8_t OT1 = 1;              // Port x configuration bits (y = 0..15)
    static const uint8_t OT0 = 0;              // Port x configuration bits (y = 0..15)
}

namespace OSPEEDR // GPIO port output speed register fields
{
    static const uint8_t OSPEEDR15 = 30;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR14 = 28;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR13 = 26;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR12 = 24;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR11 = 22;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR10 = 20;       // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR9 = 18;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR8 = 16;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR7 = 14;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR6 = 12;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR5 = 10;        // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR4 = 8;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR3 = 6;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR2 = 4;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR1 = 2;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t OSPEEDR0 = 0;         // Port x configuration bits (y = 0..15) (2 bits)
}

namespace PUPDR // GPIO port pull-up/pull-down register fields
{
    static const uint8_t PUPDR15 = 30;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR14 = 28;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR13 = 26;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR12 = 24;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR11 = 22;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR10 = 20;         // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR9 = 18;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR8 = 16;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR7 = 14;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR6 = 12;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR5 = 10;          // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR4 = 8;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR3 = 6;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR2 = 4;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR1 = 2;           // Port x configuration bits (y = 0..15) (2 bits)
    static const uint8_t PUPDR0 = 0;           // Port x configuration bits (y = 0..15) (2 bits)
}

namespace IDR // GPIO port input data register fields
{
    static const uint8_t IDR15 = 15;           // Port input data (y = 0..15)
    static const uint8_t IDR14 = 14;           // Port input data (y = 0..15)
    static const uint8_t IDR13 = 13;           // Port input data (y = 0..15)
    static const uint8_t IDR12 = 12;           // Port input data (y = 0..15)
    static const uint8_t IDR11 = 11;           // Port input data (y = 0..15)
    static const uint8_t IDR10 = 10;           // Port input data (y = 0..15)
    static const uint8_t IDR9 = 9;             // Port input data (y = 0..15)
    static const uint8_t IDR8 = 8;             // Port input data (y = 0..15)
    static const uint8_t IDR7 = 7;             // Port input data (y = 0..15)
    static const uint8_t IDR6 = 6;             // Port input data (y = 0..15)
    static const uint8_t IDR5 = 5;             // Port input data (y = 0..15)
    static const uint8_t IDR4 = 4;             // Port input data (y = 0..15)
    static const uint8_t IDR3 = 3;             // Port input data (y = 0..15)
    static const uint8_t IDR2 = 2;             // Port input data (y = 0..15)
    static const uint8_t IDR1 = 1;             // Port input data (y = 0..15)
    static const uint8_t IDR0 = 0;             // Port input data (y = 0..15)
}

namespace ODR // GPIO port output data register fields
{
    static const uint8_t ODR15 = 15;           // Port output data (y = 0..15)
    static const uint8_t ODR14 = 14;           // Port output data (y = 0..15)
    static const uint8_t ODR13 = 13;           // Port output data (y = 0..15)
    static const uint8_t ODR12 = 12;           // Port output data (y = 0..15)
    static const uint8_t ODR11 = 11;           // Port output data (y = 0..15)
    static const uint8_t ODR10 = 10;           // Port output data (y = 0..15)
    static const uint8_t ODR9 = 9;             // Port output data (y = 0..15)
    static const uint8_t ODR8 = 8;             // Port output data (y = 0..15)
    static const uint8_t ODR7 = 7;             // Port output data (y = 0..15)
    static const uint8_t ODR6 = 6;             // Port output data (y = 0..15)
    static const uint8_t ODR5 = 5;             // Port output data (y = 0..15)
    static const uint8_t ODR4 = 4;             // Port output data (y = 0..15)
    static const uint8_t ODR3 = 3;             // Port output data (y = 0..15)
    static const uint8_t ODR2 = 2;             // Port output data (y = 0..15)
    static const uint8_t ODR1 = 1;             // Port output data (y = 0..15)
    static const uint8_t ODR0 = 0;             // Port output data (y = 0..15)
}

namespace BSRR // GPIO port bit set/reset register fields
{
    static const uint8_t BR15 = 31;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR14 = 30;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR13 = 29;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR12 = 28;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR11 = 27;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR10 = 26;            // Port x reset bit y (y = 0..15)
    static const uint8_t BR9 = 25;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR8 = 24;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR7 = 23;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR6 = 22;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR5 = 21;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR4 = 20;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR3 = 19;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR2 = 18;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR1 = 17;             // Port x reset bit y (y = 0..15)
    static const uint8_t BR0 = 16;             // Port x set bit y (y= 0..15)
    static const uint8_t BS15 = 15;            // Port x set bit y (y= 0..15)
    static const uint8_t BS14 = 14;            // Port x set bit y (y= 0..15)
    static const uint8_t BS13 = 13;            // Port x set bit y (y= 0..15)
    static const uint8_t BS12 = 12;            // Port x set bit y (y= 0..15)
    static const uint8_t BS11 = 11;            // Port x set bit y (y= 0..15)
    static const uint8_t BS10 = 10;            // Port x set bit y (y= 0..15)
    static const uint8_t BS9 = 9;              // Port x set bit y (y= 0..15)
    static const uint8_t BS8 = 8;              // Port x set bit y (y= 0..15)
    static const uint8_t BS7 = 7;              // Port x set bit y (y= 0..15)
    static const uint8_t BS6 = 6;              // Port x set bit y (y= 0..15)
    static const uint8_t BS5 = 5;              // Port x set bit y (y= 0..15)
    static const uint8_t BS4 = 4;              // Port x set bit y (y= 0..15)
    static const uint8_t BS3 = 3;              // Port x set bit y (y= 0..15)
    static const uint8_t BS2 = 2;              // Port x set bit y (y= 0..15)
    static const uint8_t BS1 = 1;              // Port x set bit y (y= 0..15)
    static const uint8_t BS0 = 0;              // Port x set bit y (y= 0..15)
}

namespace LCKR // GPIO port configuration lock register fields
{
    static const uint8_t LCKK = 16;            // Port x lock bit y (y= 0..15)
    static const uint8_t LCK15 = 15;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK14 = 14;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK13 = 13;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK12 = 12;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK11 = 11;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK10 = 10;           // Port x lock bit y (y= 0..15)
    static const uint8_t LCK9 = 9;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK8 = 8;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK7 = 7;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK6 = 6;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK5 = 5;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK4 = 4;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK3 = 3;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK2 = 2;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK1 = 1;             // Port x lock bit y (y= 0..15)
    static const uint8_t LCK0 = 0;             // Port x lock bit y (y= 0..15)
}

namespace AFRL // GPIO alternate function low register fields
{
    static const uint8_t AFRL7 = 28;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL6 = 24;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL5 = 20;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL4 = 16;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL3 = 12;           // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL2 = 8;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL1 = 4;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
    static const uint8_t AFRL0 = 0;            // Alternate function selection for port x bit y (y = 0..7) (4 bits)
}

namespace AFRH // GPIO alternate function high register fields
{
    static const uint8_t AFRH15 = 28;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH14 = 24;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH13 = 20;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH12 = 16;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH11 = 12;          // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH10 = 8;           // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH9 = 4;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
    static const uint8_t AFRH8 = 0;            // Alternate function selection for port x bit y (y = 8..15) (4 bits)
}

namespace BRR // Port bit reset register fields
{
    static const uint8_t BR0 = 0;              // Port x Reset bit y
    static const uint8_t BR1 = 1;              // Port x Reset bit y
    static const uint8_t BR2 = 2;              // Port x Reset bit y
    static const uint8_t BR3 = 3;              // Port x Reset bit y
    static const uint8_t BR4 = 4;              // Port x Reset bit y
    static const uint8_t BR5 = 5;              // Port x Reset bit y
    static const uint8_t BR6 = 6;              // Port x Reset bit y
    static const uint8_t BR7 = 7;              // Port x Reset bit y
    static const uint8_t BR8 = 8;              // Port x Reset bit y
    static const uint8_t BR9 = 9;              // Port x Reset bit y
    static const uint8_t BR10 = 10;            // Port x Reset bit y
    static const uint8_t BR11 = 11;            // Port x Reset bit y
    static const uint8_t BR12 = 12;            // Port x Reset bit y
    static const uint8_t BR13 = 13;            // Port x Reset bit y
    static const uint8_t BR14 = 14;            // Port x Reset bit y
    static const uint8_t BR15 = 15;            // Port x Reset bit y
}

}

////
//
//    Serial peripheral interface
//
////

namespace spi1
{

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
};

spi1_t& SPI1 = *reinterpret_cast<spi1_t*>(0x40013000);

namespace CR1 // control register 1 fields
{
    static const uint8_t BIDIMODE = 15;        // Bidirectional data mode enable
    static const uint8_t BIDIOE = 14;          // Output enable in bidirectional mode
    static const uint8_t CRCEN = 13;           // Hardware CRC calculation enable
    static const uint8_t CRCNEXT = 12;         // CRC transfer next
    static const uint8_t DFF = 11;             // Data frame format
    static const uint8_t RXONLY = 10;          // Receive only
    static const uint8_t SSM = 9;              // Software slave management
    static const uint8_t SSI = 8;              // Internal slave select
    static const uint8_t LSBFIRST = 7;         // Frame format
    static const uint8_t SPE = 6;              // SPI enable
    static const uint8_t BR = 3;               // Baud rate control (3 bits)
    static const uint8_t MSTR = 2;             // Master selection
    static const uint8_t CPOL = 1;             // Clock polarity
    static const uint8_t CPHA = 0;             // Clock phase
}

namespace CR2 // control register 2 fields
{
    static const uint8_t RXDMAEN = 0;          // Rx buffer DMA enable
    static const uint8_t TXDMAEN = 1;          // Tx buffer DMA enable
    static const uint8_t SSOE = 2;             // SS output enable
    static const uint8_t NSSP = 3;             // NSS pulse management
    static const uint8_t FRF = 4;              // Frame format
    static const uint8_t ERRIE = 5;            // Error interrupt enable
    static const uint8_t RXNEIE = 6;           // RX buffer not empty interrupt enable
    static const uint8_t TXEIE = 7;            // Tx buffer empty interrupt enable
    static const uint8_t DS = 8;               // Data size (4 bits)
    static const uint8_t FRXTH = 12;           // FIFO reception threshold
    static const uint8_t LDMA_RX = 13;         // Last DMA transfer for reception
    static const uint8_t LDMA_TX = 14;         // Last DMA transfer for transmission
}

namespace SR // status register fields
{
    static const uint8_t RXNE = 0;             // Receive buffer not empty, Read-only
    static const uint8_t TXE = 1;              // Transmit buffer empty, Read-only
    static const uint8_t CHSIDE = 2;           // Channel side, Read-only
    static const uint8_t UDR = 3;              // Underrun flag, Read-only
    static const uint8_t CRCERR = 4;           // CRC error flag, Read-write
    static const uint8_t MODF = 5;             // Mode fault, Read-only
    static const uint8_t OVR = 6;              // Overrun flag, Read-only
    static const uint8_t BSY = 7;              // Busy flag, Read-only
    static const uint8_t TIFRFE = 8;           // TI frame format error, Read-only
    static const uint8_t FRLVL = 9;            // FIFO reception level (2 bits), Read-only
    static const uint8_t FTLVL = 11;           // FIFO transmission level (2 bits), Read-only
}

namespace DR // data register fields
{
    static const uint8_t DR = 0;               // Data register (16 bits)
}

namespace CRCPR // CRC polynomial register fields
{
    static const uint8_t CRCPOLY = 0;          // CRC polynomial register (16 bits)
}

namespace RXCRCR // RX CRC register fields
{
    static const uint8_t RxCRC = 0;            // Rx CRC register (16 bits)
}

namespace TXCRCR // TX CRC register fields
{
    static const uint8_t TxCRC = 0;            // Tx CRC register (16 bits)
}

namespace I2SCFGR // I2S configuration register fields
{
    static const uint8_t I2SMOD = 11;          // I2S mode selection
    static const uint8_t I2SE = 10;            // I2S Enable
    static const uint8_t I2SCFG = 8;           // I2S configuration mode (2 bits)
    static const uint8_t PCMSYNC = 7;          // PCM frame synchronization
    static const uint8_t I2SSTD = 4;           // I2S standard selection (2 bits)
    static const uint8_t CKPOL = 3;            // Steady state clock polarity
    static const uint8_t DATLEN = 1;           // Data length to be transferred (2 bits)
    static const uint8_t CHLEN = 0;            // Channel length (number of bits per audio channel)
}

namespace I2SPR // I2S prescaler register fields
{
    static const uint8_t MCKOE = 9;            // Master clock output enable
    static const uint8_t ODD = 8;              // Odd factor for the prescaler
    static const uint8_t I2SDIV = 0;           // I2S Linear prescaler (8 bits)
}

}

////
//
//    Serial peripheral interface
//
////

namespace spi2
{

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
};

spi2_t& SPI2 = *reinterpret_cast<spi2_t*>(0x40003800);

namespace CR1 // control register 1 fields
{
    static const uint8_t BIDIMODE = 15;        // Bidirectional data mode enable
    static const uint8_t BIDIOE = 14;          // Output enable in bidirectional mode
    static const uint8_t CRCEN = 13;           // Hardware CRC calculation enable
    static const uint8_t CRCNEXT = 12;         // CRC transfer next
    static const uint8_t DFF = 11;             // Data frame format
    static const uint8_t RXONLY = 10;          // Receive only
    static const uint8_t SSM = 9;              // Software slave management
    static const uint8_t SSI = 8;              // Internal slave select
    static const uint8_t LSBFIRST = 7;         // Frame format
    static const uint8_t SPE = 6;              // SPI enable
    static const uint8_t BR = 3;               // Baud rate control (3 bits)
    static const uint8_t MSTR = 2;             // Master selection
    static const uint8_t CPOL = 1;             // Clock polarity
    static const uint8_t CPHA = 0;             // Clock phase
}

namespace CR2 // control register 2 fields
{
    static const uint8_t RXDMAEN = 0;          // Rx buffer DMA enable
    static const uint8_t TXDMAEN = 1;          // Tx buffer DMA enable
    static const uint8_t SSOE = 2;             // SS output enable
    static const uint8_t NSSP = 3;             // NSS pulse management
    static const uint8_t FRF = 4;              // Frame format
    static const uint8_t ERRIE = 5;            // Error interrupt enable
    static const uint8_t RXNEIE = 6;           // RX buffer not empty interrupt enable
    static const uint8_t TXEIE = 7;            // Tx buffer empty interrupt enable
    static const uint8_t DS = 8;               // Data size (4 bits)
    static const uint8_t FRXTH = 12;           // FIFO reception threshold
    static const uint8_t LDMA_RX = 13;         // Last DMA transfer for reception
    static const uint8_t LDMA_TX = 14;         // Last DMA transfer for transmission
}

namespace SR // status register fields
{
    static const uint8_t RXNE = 0;             // Receive buffer not empty, Read-only
    static const uint8_t TXE = 1;              // Transmit buffer empty, Read-only
    static const uint8_t CHSIDE = 2;           // Channel side, Read-only
    static const uint8_t UDR = 3;              // Underrun flag, Read-only
    static const uint8_t CRCERR = 4;           // CRC error flag, Read-write
    static const uint8_t MODF = 5;             // Mode fault, Read-only
    static const uint8_t OVR = 6;              // Overrun flag, Read-only
    static const uint8_t BSY = 7;              // Busy flag, Read-only
    static const uint8_t TIFRFE = 8;           // TI frame format error, Read-only
    static const uint8_t FRLVL = 9;            // FIFO reception level (2 bits), Read-only
    static const uint8_t FTLVL = 11;           // FIFO transmission level (2 bits), Read-only
}

namespace DR // data register fields
{
    static const uint8_t DR = 0;               // Data register (16 bits)
}

namespace CRCPR // CRC polynomial register fields
{
    static const uint8_t CRCPOLY = 0;          // CRC polynomial register (16 bits)
}

namespace RXCRCR // RX CRC register fields
{
    static const uint8_t RxCRC = 0;            // Rx CRC register (16 bits)
}

namespace TXCRCR // TX CRC register fields
{
    static const uint8_t TxCRC = 0;            // Tx CRC register (16 bits)
}

namespace I2SCFGR // I2S configuration register fields
{
    static const uint8_t I2SMOD = 11;          // I2S mode selection
    static const uint8_t I2SE = 10;            // I2S Enable
    static const uint8_t I2SCFG = 8;           // I2S configuration mode (2 bits)
    static const uint8_t PCMSYNC = 7;          // PCM frame synchronization
    static const uint8_t I2SSTD = 4;           // I2S standard selection (2 bits)
    static const uint8_t CKPOL = 3;            // Steady state clock polarity
    static const uint8_t DATLEN = 1;           // Data length to be transferred (2 bits)
    static const uint8_t CHLEN = 0;            // Channel length (number of bits per audio channel)
}

namespace I2SPR // I2S prescaler register fields
{
    static const uint8_t MCKOE = 9;            // Master clock output enable
    static const uint8_t ODD = 8;              // Odd factor for the prescaler
    static const uint8_t I2SDIV = 0;           // I2S Linear prescaler (8 bits)
}

}

////
//
//    Power control
//
////

namespace pwr
{

struct pwr_t
{
    volatile uint32_t    CR;                   // [Read-write] power control register
    volatile uint32_t    CSR;                  // power control/status register
};

pwr_t& PWR = *reinterpret_cast<pwr_t*>(0x40007000);

namespace CR // power control register fields
{
    static const uint8_t DBP = 8;              // Disable backup domain write protection
    static const uint8_t PLS = 5;              // PVD level selection (3 bits)
    static const uint8_t PVDE = 4;             // Power voltage detector enable
    static const uint8_t CSBF = 3;             // Clear standby flag
    static const uint8_t CWUF = 2;             // Clear wakeup flag
    static const uint8_t PDDS = 1;             // Power down deepsleep
    static const uint8_t LPDS = 0;             // Low-power deep sleep
}

namespace CSR // power control/status register fields
{
    static const uint8_t WUF = 0;              // Wakeup flag, Read-only
    static const uint8_t SBF = 1;              // Standby flag, Read-only
    static const uint8_t PVDO = 2;             // PVD output, Read-only
    static const uint8_t VREFINTRDY = 3;       // VREFINT reference voltage ready, Read-only
    static const uint8_t EWUP1 = 8;            // Enable WKUP pin 1, Read-write
    static const uint8_t EWUP2 = 9;            // Enable WKUP pin 2, Read-write
    static const uint8_t EWUP3 = 10;           // Enable WKUP pin 3, Read-write
    static const uint8_t EWUP4 = 11;           // Enable WKUP pin 4, Read-write
    static const uint8_t EWUP5 = 12;           // Enable WKUP pin 5, Read-write
    static const uint8_t EWUP6 = 13;           // Enable WKUP pin 6, Read-write
    static const uint8_t EWUP7 = 14;           // Enable WKUP pin 7, Read-write
    static const uint8_t EWUP8 = 15;           // Enable WKUP pin 8, Read-write
}

}

////
//
//    Inter-integrated circuit
//
////

namespace i2c1
{

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
};

i2c1_t& I2C1 = *reinterpret_cast<i2c1_t*>(0x40005400);

namespace CR1 // Control register 1 fields
{
    static const uint8_t PE = 0;               // Peripheral enable, Read-write
    static const uint8_t TXIE = 1;             // TX Interrupt enable, Read-write
    static const uint8_t RXIE = 2;             // RX Interrupt enable, Read-write
    static const uint8_t ADDRIE = 3;           // Address match interrupt enable (slave only), Read-write
    static const uint8_t NACKIE = 4;           // Not acknowledge received interrupt enable, Read-write
    static const uint8_t STOPIE = 5;           // STOP detection Interrupt enable, Read-write
    static const uint8_t TCIE = 6;             // Transfer Complete interrupt enable, Read-write
    static const uint8_t ERRIE = 7;            // Error interrupts enable, Read-write
    static const uint8_t DNF = 8;              // Digital noise filter (4 bits), Read-write
    static const uint8_t ANFOFF = 12;          // Analog noise filter OFF, Read-write
    static const uint8_t SWRST = 13;           // Software reset, Write-only
    static const uint8_t TXDMAEN = 14;         // DMA transmission requests enable, Read-write
    static const uint8_t RXDMAEN = 15;         // DMA reception requests enable, Read-write
    static const uint8_t SBC = 16;             // Slave byte control, Read-write
    static const uint8_t NOSTRETCH = 17;       // Clock stretching disable, Read-write
    static const uint8_t WUPEN = 18;           // Wakeup from STOP enable, Read-write
    static const uint8_t GCEN = 19;            // General call enable, Read-write
    static const uint8_t SMBHEN = 20;          // SMBus Host address enable, Read-write
    static const uint8_t SMBDEN = 21;          // SMBus Device Default address enable, Read-write
    static const uint8_t ALERTEN = 22;         // SMBUS alert enable, Read-write
    static const uint8_t PECEN = 23;           // PEC enable, Read-write
}

namespace CR2 // Control register 2 fields
{
    static const uint8_t PECBYTE = 26;         // Packet error checking byte
    static const uint8_t AUTOEND = 25;         // Automatic end mode (master mode)
    static const uint8_t RELOAD = 24;          // NBYTES reload mode
    static const uint8_t NBYTES = 16;          // Number of bytes (8 bits)
    static const uint8_t NACK = 15;            // NACK generation (slave mode)
    static const uint8_t STOP = 14;            // Stop generation (master mode)
    static const uint8_t START = 13;           // Start generation
    static const uint8_t HEAD10R = 12;         // 10-bit address header only read direction (master receiver mode)
    static const uint8_t ADD10 = 11;           // 10-bit addressing mode (master mode)
    static const uint8_t RD_WRN = 10;          // Transfer direction (master mode)
    static const uint8_t SADD8 = 8;            // Slave address bit 9:8 (master mode) (2 bits)
    static const uint8_t SADD1 = 1;            // Slave address bit 7:1 (master mode) (7 bits)
    static const uint8_t SADD0 = 0;            // Slave address bit 0 (master mode)
}

namespace OAR1 // Own address register 1 fields
{
    static const uint8_t OA1_0 = 0;            // Interface address
    static const uint8_t OA1_1 = 1;            // Interface address (7 bits)
    static const uint8_t OA1_8 = 8;            // Interface address (2 bits)
    static const uint8_t OA1MODE = 10;         // Own Address 1 10-bit mode
    static const uint8_t OA1EN = 15;           // Own Address 1 enable
}

namespace OAR2 // Own address register 2 fields
{
    static const uint8_t OA2 = 1;              // Interface address (7 bits)
    static const uint8_t OA2MSK = 8;           // Own Address 2 masks (3 bits)
    static const uint8_t OA2EN = 15;           // Own Address 2 enable
}

namespace TIMINGR // Timing register fields
{
    static const uint8_t SCLL = 0;             // SCL low period (master mode) (8 bits)
    static const uint8_t SCLH = 8;             // SCL high period (master mode) (8 bits)
    static const uint8_t SDADEL = 16;          // Data hold time (4 bits)
    static const uint8_t SCLDEL = 20;          // Data setup time (4 bits)
    static const uint8_t PRESC = 28;           // Timing prescaler (4 bits)
}

namespace TIMEOUTR // Status register 1 fields
{
    static const uint8_t TIMEOUTA = 0;         // Bus timeout A (12 bits)
    static const uint8_t TIDLE = 12;           // Idle clock timeout detection
    static const uint8_t TIMOUTEN = 15;        // Clock timeout enable
    static const uint8_t TIMEOUTB = 16;        // Bus timeout B (12 bits)
    static const uint8_t TEXTEN = 31;          // Extended clock timeout enable
}

namespace ISR // Interrupt and Status register fields
{
    static const uint8_t ADDCODE = 17;         // Address match code (Slave mode) (7 bits), Read-only
    static const uint8_t DIR = 16;             // Transfer direction (Slave mode), Read-only
    static const uint8_t BUSY = 15;            // Bus busy, Read-only
    static const uint8_t ALERT = 13;           // SMBus alert, Read-only
    static const uint8_t TIMEOUT = 12;         // Timeout or t_low detection flag, Read-only
    static const uint8_t PECERR = 11;          // PEC Error in reception, Read-only
    static const uint8_t OVR = 10;             // Overrun/Underrun (slave mode), Read-only
    static const uint8_t ARLO = 9;             // Arbitration lost, Read-only
    static const uint8_t BERR = 8;             // Bus error, Read-only
    static const uint8_t TCR = 7;              // Transfer Complete Reload, Read-only
    static const uint8_t TC = 6;               // Transfer Complete (master mode), Read-only
    static const uint8_t STOPF = 5;            // Stop detection flag, Read-only
    static const uint8_t NACKF = 4;            // Not acknowledge received flag, Read-only
    static const uint8_t ADDR = 3;             // Address matched (slave mode), Read-only
    static const uint8_t RXNE = 2;             // Receive data register not empty (receivers), Read-only
    static const uint8_t TXIS = 1;             // Transmit interrupt status (transmitters), Read-write
    static const uint8_t TXE = 0;              // Transmit data register empty (transmitters), Read-write
}

namespace ICR // Interrupt clear register fields
{
    static const uint8_t ALERTCF = 13;         // Alert flag clear
    static const uint8_t TIMOUTCF = 12;        // Timeout detection flag clear
    static const uint8_t PECCF = 11;           // PEC Error flag clear
    static const uint8_t OVRCF = 10;           // Overrun/Underrun flag clear
    static const uint8_t ARLOCF = 9;           // Arbitration lost flag clear
    static const uint8_t BERRCF = 8;           // Bus error flag clear
    static const uint8_t STOPCF = 5;           // Stop detection flag clear
    static const uint8_t NACKCF = 4;           // Not Acknowledge flag clear
    static const uint8_t ADDRCF = 3;           // Address Matched flag clear
}

namespace PECR // PEC register fields
{
    static const uint8_t PEC = 0;              // Packet error checking register (8 bits)
}

namespace RXDR // Receive data register fields
{
    static const uint8_t RXDATA = 0;           // 8-bit receive data (8 bits)
}

namespace TXDR // Transmit data register fields
{
    static const uint8_t TXDATA = 0;           // 8-bit transmit data (8 bits)
}

}

////
//
//    Inter-integrated circuit
//
////

namespace i2c2
{

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
};

i2c2_t& I2C2 = *reinterpret_cast<i2c2_t*>(0x40005800);

namespace CR1 // Control register 1 fields
{
    static const uint8_t PE = 0;               // Peripheral enable, Read-write
    static const uint8_t TXIE = 1;             // TX Interrupt enable, Read-write
    static const uint8_t RXIE = 2;             // RX Interrupt enable, Read-write
    static const uint8_t ADDRIE = 3;           // Address match interrupt enable (slave only), Read-write
    static const uint8_t NACKIE = 4;           // Not acknowledge received interrupt enable, Read-write
    static const uint8_t STOPIE = 5;           // STOP detection Interrupt enable, Read-write
    static const uint8_t TCIE = 6;             // Transfer Complete interrupt enable, Read-write
    static const uint8_t ERRIE = 7;            // Error interrupts enable, Read-write
    static const uint8_t DNF = 8;              // Digital noise filter (4 bits), Read-write
    static const uint8_t ANFOFF = 12;          // Analog noise filter OFF, Read-write
    static const uint8_t SWRST = 13;           // Software reset, Write-only
    static const uint8_t TXDMAEN = 14;         // DMA transmission requests enable, Read-write
    static const uint8_t RXDMAEN = 15;         // DMA reception requests enable, Read-write
    static const uint8_t SBC = 16;             // Slave byte control, Read-write
    static const uint8_t NOSTRETCH = 17;       // Clock stretching disable, Read-write
    static const uint8_t WUPEN = 18;           // Wakeup from STOP enable, Read-write
    static const uint8_t GCEN = 19;            // General call enable, Read-write
    static const uint8_t SMBHEN = 20;          // SMBus Host address enable, Read-write
    static const uint8_t SMBDEN = 21;          // SMBus Device Default address enable, Read-write
    static const uint8_t ALERTEN = 22;         // SMBUS alert enable, Read-write
    static const uint8_t PECEN = 23;           // PEC enable, Read-write
}

namespace CR2 // Control register 2 fields
{
    static const uint8_t PECBYTE = 26;         // Packet error checking byte
    static const uint8_t AUTOEND = 25;         // Automatic end mode (master mode)
    static const uint8_t RELOAD = 24;          // NBYTES reload mode
    static const uint8_t NBYTES = 16;          // Number of bytes (8 bits)
    static const uint8_t NACK = 15;            // NACK generation (slave mode)
    static const uint8_t STOP = 14;            // Stop generation (master mode)
    static const uint8_t START = 13;           // Start generation
    static const uint8_t HEAD10R = 12;         // 10-bit address header only read direction (master receiver mode)
    static const uint8_t ADD10 = 11;           // 10-bit addressing mode (master mode)
    static const uint8_t RD_WRN = 10;          // Transfer direction (master mode)
    static const uint8_t SADD8 = 8;            // Slave address bit 9:8 (master mode) (2 bits)
    static const uint8_t SADD1 = 1;            // Slave address bit 7:1 (master mode) (7 bits)
    static const uint8_t SADD0 = 0;            // Slave address bit 0 (master mode)
}

namespace OAR1 // Own address register 1 fields
{
    static const uint8_t OA1_0 = 0;            // Interface address
    static const uint8_t OA1_1 = 1;            // Interface address (7 bits)
    static const uint8_t OA1_8 = 8;            // Interface address (2 bits)
    static const uint8_t OA1MODE = 10;         // Own Address 1 10-bit mode
    static const uint8_t OA1EN = 15;           // Own Address 1 enable
}

namespace OAR2 // Own address register 2 fields
{
    static const uint8_t OA2 = 1;              // Interface address (7 bits)
    static const uint8_t OA2MSK = 8;           // Own Address 2 masks (3 bits)
    static const uint8_t OA2EN = 15;           // Own Address 2 enable
}

namespace TIMINGR // Timing register fields
{
    static const uint8_t SCLL = 0;             // SCL low period (master mode) (8 bits)
    static const uint8_t SCLH = 8;             // SCL high period (master mode) (8 bits)
    static const uint8_t SDADEL = 16;          // Data hold time (4 bits)
    static const uint8_t SCLDEL = 20;          // Data setup time (4 bits)
    static const uint8_t PRESC = 28;           // Timing prescaler (4 bits)
}

namespace TIMEOUTR // Status register 1 fields
{
    static const uint8_t TIMEOUTA = 0;         // Bus timeout A (12 bits)
    static const uint8_t TIDLE = 12;           // Idle clock timeout detection
    static const uint8_t TIMOUTEN = 15;        // Clock timeout enable
    static const uint8_t TIMEOUTB = 16;        // Bus timeout B (12 bits)
    static const uint8_t TEXTEN = 31;          // Extended clock timeout enable
}

namespace ISR // Interrupt and Status register fields
{
    static const uint8_t ADDCODE = 17;         // Address match code (Slave mode) (7 bits), Read-only
    static const uint8_t DIR = 16;             // Transfer direction (Slave mode), Read-only
    static const uint8_t BUSY = 15;            // Bus busy, Read-only
    static const uint8_t ALERT = 13;           // SMBus alert, Read-only
    static const uint8_t TIMEOUT = 12;         // Timeout or t_low detection flag, Read-only
    static const uint8_t PECERR = 11;          // PEC Error in reception, Read-only
    static const uint8_t OVR = 10;             // Overrun/Underrun (slave mode), Read-only
    static const uint8_t ARLO = 9;             // Arbitration lost, Read-only
    static const uint8_t BERR = 8;             // Bus error, Read-only
    static const uint8_t TCR = 7;              // Transfer Complete Reload, Read-only
    static const uint8_t TC = 6;               // Transfer Complete (master mode), Read-only
    static const uint8_t STOPF = 5;            // Stop detection flag, Read-only
    static const uint8_t NACKF = 4;            // Not acknowledge received flag, Read-only
    static const uint8_t ADDR = 3;             // Address matched (slave mode), Read-only
    static const uint8_t RXNE = 2;             // Receive data register not empty (receivers), Read-only
    static const uint8_t TXIS = 1;             // Transmit interrupt status (transmitters), Read-write
    static const uint8_t TXE = 0;              // Transmit data register empty (transmitters), Read-write
}

namespace ICR // Interrupt clear register fields
{
    static const uint8_t ALERTCF = 13;         // Alert flag clear
    static const uint8_t TIMOUTCF = 12;        // Timeout detection flag clear
    static const uint8_t PECCF = 11;           // PEC Error flag clear
    static const uint8_t OVRCF = 10;           // Overrun/Underrun flag clear
    static const uint8_t ARLOCF = 9;           // Arbitration lost flag clear
    static const uint8_t BERRCF = 8;           // Bus error flag clear
    static const uint8_t STOPCF = 5;           // Stop detection flag clear
    static const uint8_t NACKCF = 4;           // Not Acknowledge flag clear
    static const uint8_t ADDRCF = 3;           // Address Matched flag clear
}

namespace PECR // PEC register fields
{
    static const uint8_t PEC = 0;              // Packet error checking register (8 bits)
}

namespace RXDR // Receive data register fields
{
    static const uint8_t RXDATA = 0;           // 8-bit receive data (8 bits)
}

namespace TXDR // Transmit data register fields
{
    static const uint8_t TXDATA = 0;           // 8-bit transmit data (8 bits)
}

}

////
//
//    Independent watchdog
//
////

namespace iwdg
{

struct iwdg_t
{
    volatile uint32_t    KR;                   // [Write-only] Key register
    volatile uint32_t    PR;                   // [Read-write] Prescaler register
    volatile uint32_t    RLR;                  // [Read-write] Reload register
    volatile uint32_t    SR;                   // [Read-only] Status register
    volatile uint32_t    WINR;                 // [Read-write] Window register
};

iwdg_t& IWDG = *reinterpret_cast<iwdg_t*>(0x40003000);

namespace KR // Key register fields
{
    static const uint8_t KEY = 0;              // Key value (16 bits)
}

namespace PR // Prescaler register fields
{
    static const uint8_t PR = 0;               // Prescaler divider (3 bits)
}

namespace RLR // Reload register fields
{
    static const uint8_t RL = 0;               // Watchdog counter reload value (12 bits)
}

namespace SR // Status register fields
{
    static const uint8_t PVU = 0;              // Watchdog prescaler value update
    static const uint8_t RVU = 1;              // Watchdog counter reload value update
    static const uint8_t WVU = 2;              // Watchdog counter window value update
}

namespace WINR // Window register fields
{
    static const uint8_t WIN = 0;              // Watchdog counter window value (12 bits)
}

}

////
//
//    Window watchdog
//
////

namespace wwdg
{

struct wwdg_t
{
    volatile uint32_t    CR;                   // [Read-write] Control register
    volatile uint32_t    CFR;                  // [Read-write] Configuration register
    volatile uint32_t    SR;                   // [Read-write] Status register
};

wwdg_t& WWDG = *reinterpret_cast<wwdg_t*>(0x40002c00);

namespace CR // Control register fields
{
    static const uint8_t WDGA = 7;             // Activation bit
    static const uint8_t T = 0;                // 7-bit counter (7 bits)
}

namespace CFR // Configuration register fields
{
    static const uint8_t EWI = 9;              // Early wakeup interrupt
    static const uint8_t WDGTB = 7;            // Timer base (2 bits)
    static const uint8_t W = 0;                // 7-bit window value (7 bits)
}

namespace SR // Status register fields
{
    static const uint8_t EWIF = 0;             // Early wakeup interrupt flag
}

}

////
//
//    Advanced-timers
//
////

namespace tim1
{

struct tim1_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMCR;                 // [Read-write] slave mode control register
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1_Output;         // [Read-write] capture/compare mode register (output mode)
    volatile uint32_t    CCMR1_Input;          // [Read-write] capture/compare mode register 1 (input mode)
    volatile uint32_t    CCMR2_Output;         // [Read-write] capture/compare mode register (output mode)
    volatile uint32_t    CCMR2_Input;          // [Read-write] capture/compare mode register 2 (input mode)
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
};

tim1_t& TIM1 = *reinterpret_cast<tim1_t*>(0x40012c00);

namespace CR1 // control register 1 fields
{
    static const uint8_t CKD = 8;              // Clock division (2 bits)
    static const uint8_t ARPE = 7;             // Auto-reload preload enable
    static const uint8_t CMS = 5;              // Center-aligned mode selection (2 bits)
    static const uint8_t DIR = 4;              // Direction
    static const uint8_t OPM = 3;              // One-pulse mode
    static const uint8_t URS = 2;              // Update request source
    static const uint8_t UDIS = 1;             // Update disable
    static const uint8_t CEN = 0;              // Counter enable
}

namespace CR2 // control register 2 fields
{
    static const uint8_t OIS4 = 14;            // Output Idle state 4
    static const uint8_t OIS3N = 13;           // Output Idle state 3
    static const uint8_t OIS3 = 12;            // Output Idle state 3
    static const uint8_t OIS2N = 11;           // Output Idle state 2
    static const uint8_t OIS2 = 10;            // Output Idle state 2
    static const uint8_t OIS1N = 9;            // Output Idle state 1
    static const uint8_t OIS1 = 8;             // Output Idle state 1
    static const uint8_t TI1S = 7;             // TI1 selection
    static const uint8_t MMS = 4;              // Master mode selection (3 bits)
    static const uint8_t CCDS = 3;             // Capture/compare DMA selection
    static const uint8_t CCUS = 2;             // Capture/compare control update selection
    static const uint8_t CCPC = 0;             // Capture/compare preloaded control
}

namespace SMCR // slave mode control register fields
{
    static const uint8_t ETP = 15;             // External trigger polarity
    static const uint8_t ECE = 14;             // External clock enable
    static const uint8_t ETPS = 12;            // External trigger prescaler (2 bits)
    static const uint8_t ETF = 8;              // External trigger filter (4 bits)
    static const uint8_t MSM = 7;              // Master/Slave mode
    static const uint8_t TS = 4;               // Trigger selection (3 bits)
    static const uint8_t SMS = 0;              // Slave mode selection (3 bits)
}

namespace DIER // DMA/Interrupt enable register fields
{
    static const uint8_t TDE = 14;             // Trigger DMA request enable
    static const uint8_t COMDE = 13;           // COM DMA request enable
    static const uint8_t CC4DE = 12;           // Capture/Compare 4 DMA request enable
    static const uint8_t CC3DE = 11;           // Capture/Compare 3 DMA request enable
    static const uint8_t CC2DE = 10;           // Capture/Compare 2 DMA request enable
    static const uint8_t CC1DE = 9;            // Capture/Compare 1 DMA request enable
    static const uint8_t UDE = 8;              // Update DMA request enable
    static const uint8_t BIE = 7;              // Break interrupt enable
    static const uint8_t TIE = 6;              // Trigger interrupt enable
    static const uint8_t COMIE = 5;            // COM interrupt enable
    static const uint8_t CC4IE = 4;            // Capture/Compare 4 interrupt enable
    static const uint8_t CC3IE = 3;            // Capture/Compare 3 interrupt enable
    static const uint8_t CC2IE = 2;            // Capture/Compare 2 interrupt enable
    static const uint8_t CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t UIE = 0;              // Update interrupt enable
}

namespace SR // status register fields
{
    static const uint8_t CC4OF = 12;           // Capture/Compare 4 overcapture flag
    static const uint8_t CC3OF = 11;           // Capture/Compare 3 overcapture flag
    static const uint8_t CC2OF = 10;           // Capture/compare 2 overcapture flag
    static const uint8_t CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t BIF = 7;              // Break interrupt flag
    static const uint8_t TIF = 6;              // Trigger interrupt flag
    static const uint8_t COMIF = 5;            // COM interrupt flag
    static const uint8_t CC4IF = 4;            // Capture/Compare 4 interrupt flag
    static const uint8_t CC3IF = 3;            // Capture/Compare 3 interrupt flag
    static const uint8_t CC2IF = 2;            // Capture/Compare 2 interrupt flag
    static const uint8_t CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t UIF = 0;              // Update interrupt flag
}

namespace EGR // event generation register fields
{
    static const uint8_t BG = 7;               // Break generation
    static const uint8_t TG = 6;               // Trigger generation
    static const uint8_t COMG = 5;             // Capture/Compare control update generation
    static const uint8_t CC4G = 4;             // Capture/compare 4 generation
    static const uint8_t CC3G = 3;             // Capture/compare 3 generation
    static const uint8_t CC2G = 2;             // Capture/compare 2 generation
    static const uint8_t CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t UG = 0;               // Update generation
}

namespace CCMR1_Output // capture/compare mode register (output mode) fields
{
    static const uint8_t OC2CE = 15;           // Output Compare 2 clear enable
    static const uint8_t OC2M = 12;            // Output Compare 2 mode (3 bits)
    static const uint8_t OC2PE = 11;           // Output Compare 2 preload enable
    static const uint8_t OC2FE = 10;           // Output Compare 2 fast enable
    static const uint8_t CC2S = 8;             // Capture/Compare 2 selection (2 bits)
    static const uint8_t OC1CE = 7;            // Output Compare 1 clear enable
    static const uint8_t OC1M = 4;             // Output Compare 1 mode (3 bits)
    static const uint8_t OC1PE = 3;            // Output Compare 1 preload enable
    static const uint8_t OC1FE = 2;            // Output Compare 1 fast enable
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCMR1_Input // capture/compare mode register 1 (input mode) fields
{
    static const uint8_t IC2F = 12;            // Input capture 2 filter (4 bits)
    static const uint8_t IC2PCS = 10;          // Input capture 2 prescaler (2 bits)
    static const uint8_t CC2S = 8;             // Capture/Compare 2 selection (2 bits)
    static const uint8_t IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t IC1PCS = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCMR2_Output // capture/compare mode register (output mode) fields
{
    static const uint8_t OC4CE = 15;           // Output compare 4 clear enable
    static const uint8_t OC4M = 12;            // Output compare 4 mode (3 bits)
    static const uint8_t OC4PE = 11;           // Output compare 4 preload enable
    static const uint8_t OC4FE = 10;           // Output compare 4 fast enable
    static const uint8_t CC4S = 8;             // Capture/Compare 4 selection (2 bits)
    static const uint8_t OC3CE = 7;            // Output compare 3 clear enable
    static const uint8_t OC3M = 4;             // Output compare 3 mode (3 bits)
    static const uint8_t OC3PE = 3;            // Output compare 3 preload enable
    static const uint8_t OC3FE = 2;            // Output compare 3 fast enable
    static const uint8_t CC3S = 0;             // Capture/Compare 3 selection (2 bits)
}

namespace CCMR2_Input // capture/compare mode register 2 (input mode) fields
{
    static const uint8_t IC4F = 12;            // Input capture 4 filter (4 bits)
    static const uint8_t IC4PSC = 10;          // Input capture 4 prescaler (2 bits)
    static const uint8_t CC4S = 8;             // Capture/Compare 4 selection (2 bits)
    static const uint8_t IC3F = 4;             // Input capture 3 filter (4 bits)
    static const uint8_t IC3PSC = 2;           // Input capture 3 prescaler (2 bits)
    static const uint8_t CC3S = 0;             // Capture/compare 3 selection (2 bits)
}

namespace CCER // capture/compare enable register fields
{
    static const uint8_t CC4P = 13;            // Capture/Compare 3 output Polarity
    static const uint8_t CC4E = 12;            // Capture/Compare 4 output enable
    static const uint8_t CC3NP = 11;           // Capture/Compare 3 output Polarity
    static const uint8_t CC3NE = 10;           // Capture/Compare 3 complementary output enable
    static const uint8_t CC3P = 9;             // Capture/Compare 3 output Polarity
    static const uint8_t CC3E = 8;             // Capture/Compare 3 output enable
    static const uint8_t CC2NP = 7;            // Capture/Compare 2 output Polarity
    static const uint8_t CC2NE = 6;            // Capture/Compare 2 complementary output enable
    static const uint8_t CC2P = 5;             // Capture/Compare 2 output Polarity
    static const uint8_t CC2E = 4;             // Capture/Compare 2 output enable
    static const uint8_t CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CC1NE = 2;            // Capture/Compare 1 complementary output enable
    static const uint8_t CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CC1E = 0;             // Capture/Compare 1 output enable
}

namespace CNT // counter fields
{
    static const uint8_t CNT = 0;              // counter value (16 bits)
}

namespace PSC // prescaler fields
{
    static const uint8_t PSC = 0;              // Prescaler value (16 bits)
}

namespace ARR // auto-reload register fields
{
    static const uint8_t ARR = 0;              // Auto-reload value (16 bits)
}

namespace RCR // repetition counter register fields
{
    static const uint8_t REP = 0;              // Repetition counter value (8 bits)
}

namespace CCR1 // capture/compare register 1 fields
{
    static const uint8_t CCR1 = 0;             // Capture/Compare 1 value (16 bits)
}

namespace CCR2 // capture/compare register 2 fields
{
    static const uint8_t CCR2 = 0;             // Capture/Compare 2 value (16 bits)
}

namespace CCR3 // capture/compare register 3 fields
{
    static const uint8_t CCR3 = 0;             // Capture/Compare 3 value (16 bits)
}

namespace CCR4 // capture/compare register 4 fields
{
    static const uint8_t CCR4 = 0;             // Capture/Compare 3 value (16 bits)
}

namespace BDTR // break and dead-time register fields
{
    static const uint8_t MOE = 15;             // Main output enable
    static const uint8_t AOE = 14;             // Automatic output enable
    static const uint8_t BKP = 13;             // Break polarity
    static const uint8_t BKE = 12;             // Break enable
    static const uint8_t OSSR = 11;            // Off-state selection for Run mode
    static const uint8_t OSSI = 10;            // Off-state selection for Idle mode
    static const uint8_t LOCK = 8;             // Lock configuration (2 bits)
    static const uint8_t DTG = 0;              // Dead-time generator setup (8 bits)
}

namespace DCR // DMA control register fields
{
    static const uint8_t DBL = 8;              // DMA burst length (5 bits)
    static const uint8_t DBA = 0;              // DMA base address (5 bits)
}

namespace DMAR // DMA address for full transfer fields
{
    static const uint8_t DMAB = 0;             // DMA register for burst accesses (16 bits)
}

}

////
//
//    General-purpose-timers
//
////

namespace tim2
{

struct tim2_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMCR;                 // [Read-write] slave mode control register
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1_Output;         // [Read-write] capture/compare mode register 1 (output mode)
    volatile uint32_t    CCMR1_Input;          // [Read-write] capture/compare mode register 1 (input mode)
    volatile uint32_t    CCMR2_Output;         // [Read-write] capture/compare mode register 2 (output mode)
    volatile uint32_t    CCMR2_Input;          // [Read-write] capture/compare mode register 2 (input mode)
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
};

tim2_t& TIM2 = *reinterpret_cast<tim2_t*>(0x40000000);

namespace CR1 // control register 1 fields
{
    static const uint8_t CKD = 8;              // Clock division (2 bits)
    static const uint8_t ARPE = 7;             // Auto-reload preload enable
    static const uint8_t CMS = 5;              // Center-aligned mode selection (2 bits)
    static const uint8_t DIR = 4;              // Direction
    static const uint8_t OPM = 3;              // One-pulse mode
    static const uint8_t URS = 2;              // Update request source
    static const uint8_t UDIS = 1;             // Update disable
    static const uint8_t CEN = 0;              // Counter enable
}

namespace CR2 // control register 2 fields
{
    static const uint8_t TI1S = 7;             // TI1 selection
    static const uint8_t MMS = 4;              // Master mode selection (3 bits)
    static const uint8_t CCDS = 3;             // Capture/compare DMA selection
}

namespace SMCR // slave mode control register fields
{
    static const uint8_t ETP = 15;             // External trigger polarity
    static const uint8_t ECE = 14;             // External clock enable
    static const uint8_t ETPS = 12;            // External trigger prescaler (2 bits)
    static const uint8_t ETF = 8;              // External trigger filter (4 bits)
    static const uint8_t MSM = 7;              // Master/Slave mode
    static const uint8_t TS = 4;               // Trigger selection (3 bits)
    static const uint8_t SMS = 0;              // Slave mode selection (3 bits)
}

namespace DIER // DMA/Interrupt enable register fields
{
    static const uint8_t TDE = 14;             // Trigger DMA request enable
    static const uint8_t COMDE = 13;           // COM DMA request enable
    static const uint8_t CC4DE = 12;           // Capture/Compare 4 DMA request enable
    static const uint8_t CC3DE = 11;           // Capture/Compare 3 DMA request enable
    static const uint8_t CC2DE = 10;           // Capture/Compare 2 DMA request enable
    static const uint8_t CC1DE = 9;            // Capture/Compare 1 DMA request enable
    static const uint8_t UDE = 8;              // Update DMA request enable
    static const uint8_t TIE = 6;              // Trigger interrupt enable
    static const uint8_t CC4IE = 4;            // Capture/Compare 4 interrupt enable
    static const uint8_t CC3IE = 3;            // Capture/Compare 3 interrupt enable
    static const uint8_t CC2IE = 2;            // Capture/Compare 2 interrupt enable
    static const uint8_t CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t UIE = 0;              // Update interrupt enable
}

namespace SR // status register fields
{
    static const uint8_t CC4OF = 12;           // Capture/Compare 4 overcapture flag
    static const uint8_t CC3OF = 11;           // Capture/Compare 3 overcapture flag
    static const uint8_t CC2OF = 10;           // Capture/compare 2 overcapture flag
    static const uint8_t CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t TIF = 6;              // Trigger interrupt flag
    static const uint8_t CC4IF = 4;            // Capture/Compare 4 interrupt flag
    static const uint8_t CC3IF = 3;            // Capture/Compare 3 interrupt flag
    static const uint8_t CC2IF = 2;            // Capture/Compare 2 interrupt flag
    static const uint8_t CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t UIF = 0;              // Update interrupt flag
}

namespace EGR // event generation register fields
{
    static const uint8_t TG = 6;               // Trigger generation
    static const uint8_t CC4G = 4;             // Capture/compare 4 generation
    static const uint8_t CC3G = 3;             // Capture/compare 3 generation
    static const uint8_t CC2G = 2;             // Capture/compare 2 generation
    static const uint8_t CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t UG = 0;               // Update generation
}

namespace CCMR1_Output // capture/compare mode register 1 (output mode) fields
{
    static const uint8_t OC2CE = 15;           // Output compare 2 clear enable
    static const uint8_t OC2M = 12;            // Output compare 2 mode (3 bits)
    static const uint8_t OC2PE = 11;           // Output compare 2 preload enable
    static const uint8_t OC2FE = 10;           // Output compare 2 fast enable
    static const uint8_t CC2S = 8;             // Capture/Compare 2 selection (2 bits)
    static const uint8_t OC1CE = 7;            // Output compare 1 clear enable
    static const uint8_t OC1M = 4;             // Output compare 1 mode (3 bits)
    static const uint8_t OC1PE = 3;            // Output compare 1 preload enable
    static const uint8_t OC1FE = 2;            // Output compare 1 fast enable
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCMR1_Input // capture/compare mode register 1 (input mode) fields
{
    static const uint8_t IC2F = 12;            // Input capture 2 filter (4 bits)
    static const uint8_t IC2PSC = 10;          // Input capture 2 prescaler (2 bits)
    static const uint8_t CC2S = 8;             // Capture/compare 2 selection (2 bits)
    static const uint8_t IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t IC1PSC = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCMR2_Output // capture/compare mode register 2 (output mode) fields
{
    static const uint8_t OC4CE = 15;           // Output compare 4 clear enable
    static const uint8_t OC4M = 12;            // Output compare 4 mode (3 bits)
    static const uint8_t OC4PE = 11;           // Output compare 4 preload enable
    static const uint8_t OC4FE = 10;           // Output compare 4 fast enable
    static const uint8_t CC4S = 8;             // Capture/Compare 4 selection (2 bits)
    static const uint8_t OC3CE = 7;            // Output compare 3 clear enable
    static const uint8_t OC3M = 4;             // Output compare 3 mode (3 bits)
    static const uint8_t OC3PE = 3;            // Output compare 3 preload enable
    static const uint8_t OC3FE = 2;            // Output compare 3 fast enable
    static const uint8_t CC3S = 0;             // Capture/Compare 3 selection (2 bits)
}

namespace CCMR2_Input // capture/compare mode register 2 (input mode) fields
{
    static const uint8_t IC4F = 12;            // Input capture 4 filter (4 bits)
    static const uint8_t IC4PSC = 10;          // Input capture 4 prescaler (2 bits)
    static const uint8_t CC4S = 8;             // Capture/Compare 4 selection (2 bits)
    static const uint8_t IC3F = 4;             // Input capture 3 filter (4 bits)
    static const uint8_t IC3PSC = 2;           // Input capture 3 prescaler (2 bits)
    static const uint8_t CC3S = 0;             // Capture/Compare 3 selection (2 bits)
}

namespace CCER // capture/compare enable register fields
{
    static const uint8_t CC4NP = 15;           // Capture/Compare 4 output Polarity
    static const uint8_t CC4P = 13;            // Capture/Compare 3 output Polarity
    static const uint8_t CC4E = 12;            // Capture/Compare 4 output enable
    static const uint8_t CC3NP = 11;           // Capture/Compare 3 output Polarity
    static const uint8_t CC3P = 9;             // Capture/Compare 3 output Polarity
    static const uint8_t CC3E = 8;             // Capture/Compare 3 output enable
    static const uint8_t CC2NP = 7;            // Capture/Compare 2 output Polarity
    static const uint8_t CC2P = 5;             // Capture/Compare 2 output Polarity
    static const uint8_t CC2E = 4;             // Capture/Compare 2 output enable
    static const uint8_t CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CC1E = 0;             // Capture/Compare 1 output enable
}

namespace CNT // counter fields
{
    static const uint8_t CNT_H = 16;           // High counter value (TIM2 only) (16 bits)
    static const uint8_t CNT_L = 0;            // Low counter value (16 bits)
}

namespace PSC // prescaler fields
{
    static const uint8_t PSC = 0;              // Prescaler value (16 bits)
}

namespace ARR // auto-reload register fields
{
    static const uint8_t ARR_H = 16;           // High Auto-reload value (TIM2 only) (16 bits)
    static const uint8_t ARR_L = 0;            // Low Auto-reload value (16 bits)
}

namespace CCR1 // capture/compare register 1 fields
{
    static const uint8_t CCR1_H = 16;          // High Capture/Compare 1 value (TIM2 only) (16 bits)
    static const uint8_t CCR1_L = 0;           // Low Capture/Compare 1 value (16 bits)
}

namespace CCR2 // capture/compare register 2 fields
{
    static const uint8_t CCR2_H = 16;          // High Capture/Compare 2 value (TIM2 only) (16 bits)
    static const uint8_t CCR2_L = 0;           // Low Capture/Compare 2 value (16 bits)
}

namespace CCR3 // capture/compare register 3 fields
{
    static const uint8_t CCR3_H = 16;          // High Capture/Compare value (TIM2 only) (16 bits)
    static const uint8_t CCR3_L = 0;           // Low Capture/Compare value (16 bits)
}

namespace CCR4 // capture/compare register 4 fields
{
    static const uint8_t CCR4_H = 16;          // High Capture/Compare value (TIM2 only) (16 bits)
    static const uint8_t CCR4_L = 0;           // Low Capture/Compare value (16 bits)
}

namespace DCR // DMA control register fields
{
    static const uint8_t DBL = 8;              // DMA burst length (5 bits)
    static const uint8_t DBA = 0;              // DMA base address (5 bits)
}

namespace DMAR // DMA address for full transfer fields
{
    static const uint8_t DMAR = 0;             // DMA register for burst accesses (16 bits)
}

}

////
//
//    General-purpose-timers
//
////

namespace tim3
{

struct tim3_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMCR;                 // [Read-write] slave mode control register
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1_Output;         // [Read-write] capture/compare mode register 1 (output mode)
    volatile uint32_t    CCMR1_Input;          // [Read-write] capture/compare mode register 1 (input mode)
    volatile uint32_t    CCMR2_Output;         // [Read-write] capture/compare mode register 2 (output mode)
    volatile uint32_t    CCMR2_Input;          // [Read-write] capture/compare mode register 2 (input mode)
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
};

tim3_t& TIM3 = *reinterpret_cast<tim3_t*>(0x40000400);

namespace CR1 // control register 1 fields
{
    static const uint8_t CKD = 8;              // Clock division (2 bits)
    static const uint8_t ARPE = 7;             // Auto-reload preload enable
    static const uint8_t CMS = 5;              // Center-aligned mode selection (2 bits)
    static const uint8_t DIR = 4;              // Direction
    static const uint8_t OPM = 3;              // One-pulse mode
    static const uint8_t URS = 2;              // Update request source
    static const uint8_t UDIS = 1;             // Update disable
    static const uint8_t CEN = 0;              // Counter enable
}

namespace CR2 // control register 2 fields
{
    static const uint8_t TI1S = 7;             // TI1 selection
    static const uint8_t MMS = 4;              // Master mode selection (3 bits)
    static const uint8_t CCDS = 3;             // Capture/compare DMA selection
}

namespace SMCR // slave mode control register fields
{
    static const uint8_t ETP = 15;             // External trigger polarity
    static const uint8_t ECE = 14;             // External clock enable
    static const uint8_t ETPS = 12;            // External trigger prescaler (2 bits)
    static const uint8_t ETF = 8;              // External trigger filter (4 bits)
    static const uint8_t MSM = 7;              // Master/Slave mode
    static const uint8_t TS = 4;               // Trigger selection (3 bits)
    static const uint8_t SMS = 0;              // Slave mode selection (3 bits)
}

namespace DIER // DMA/Interrupt enable register fields
{
    static const uint8_t TDE = 14;             // Trigger DMA request enable
    static const uint8_t COMDE = 13;           // COM DMA request enable
    static const uint8_t CC4DE = 12;           // Capture/Compare 4 DMA request enable
    static const uint8_t CC3DE = 11;           // Capture/Compare 3 DMA request enable
    static const uint8_t CC2DE = 10;           // Capture/Compare 2 DMA request enable
    static const uint8_t CC1DE = 9;            // Capture/Compare 1 DMA request enable
    static const uint8_t UDE = 8;              // Update DMA request enable
    static const uint8_t TIE = 6;              // Trigger interrupt enable
    static const uint8_t CC4IE = 4;            // Capture/Compare 4 interrupt enable
    static const uint8_t CC3IE = 3;            // Capture/Compare 3 interrupt enable
    static const uint8_t CC2IE = 2;            // Capture/Compare 2 interrupt enable
    static const uint8_t CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t UIE = 0;              // Update interrupt enable
}

namespace SR // status register fields
{
    static const uint8_t CC4OF = 12;           // Capture/Compare 4 overcapture flag
    static const uint8_t CC3OF = 11;           // Capture/Compare 3 overcapture flag
    static const uint8_t CC2OF = 10;           // Capture/compare 2 overcapture flag
    static const uint8_t CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t TIF = 6;              // Trigger interrupt flag
    static const uint8_t CC4IF = 4;            // Capture/Compare 4 interrupt flag
    static const uint8_t CC3IF = 3;            // Capture/Compare 3 interrupt flag
    static const uint8_t CC2IF = 2;            // Capture/Compare 2 interrupt flag
    static const uint8_t CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t UIF = 0;              // Update interrupt flag
}

namespace EGR // event generation register fields
{
    static const uint8_t TG = 6;               // Trigger generation
    static const uint8_t CC4G = 4;             // Capture/compare 4 generation
    static const uint8_t CC3G = 3;             // Capture/compare 3 generation
    static const uint8_t CC2G = 2;             // Capture/compare 2 generation
    static const uint8_t CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t UG = 0;               // Update generation
}

namespace CCMR1_Output // capture/compare mode register 1 (output mode) fields
{
    static const uint8_t OC2CE = 15;           // Output compare 2 clear enable
    static const uint8_t OC2M = 12;            // Output compare 2 mode (3 bits)
    static const uint8_t OC2PE = 11;           // Output compare 2 preload enable
    static const uint8_t OC2FE = 10;           // Output compare 2 fast enable
    static const uint8_t CC2S = 8;             // Capture/Compare 2 selection (2 bits)
    static const uint8_t OC1CE = 7;            // Output compare 1 clear enable
    static const uint8_t OC1M = 4;             // Output compare 1 mode (3 bits)
    static const uint8_t OC1PE = 3;            // Output compare 1 preload enable
    static const uint8_t OC1FE = 2;            // Output compare 1 fast enable
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCMR1_Input // capture/compare mode register 1 (input mode) fields
{
    static const uint8_t IC2F = 12;            // Input capture 2 filter (4 bits)
    static const uint8_t IC2PSC = 10;          // Input capture 2 prescaler (2 bits)
    static const uint8_t CC2S = 8;             // Capture/compare 2 selection (2 bits)
    static const uint8_t IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t IC1PSC = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCMR2_Output // capture/compare mode register 2 (output mode) fields
{
    static const uint8_t OC4CE = 15;           // Output compare 4 clear enable
    static const uint8_t OC4M = 12;            // Output compare 4 mode (3 bits)
    static const uint8_t OC4PE = 11;           // Output compare 4 preload enable
    static const uint8_t OC4FE = 10;           // Output compare 4 fast enable
    static const uint8_t CC4S = 8;             // Capture/Compare 4 selection (2 bits)
    static const uint8_t OC3CE = 7;            // Output compare 3 clear enable
    static const uint8_t OC3M = 4;             // Output compare 3 mode (3 bits)
    static const uint8_t OC3PE = 3;            // Output compare 3 preload enable
    static const uint8_t OC3FE = 2;            // Output compare 3 fast enable
    static const uint8_t CC3S = 0;             // Capture/Compare 3 selection (2 bits)
}

namespace CCMR2_Input // capture/compare mode register 2 (input mode) fields
{
    static const uint8_t IC4F = 12;            // Input capture 4 filter (4 bits)
    static const uint8_t IC4PSC = 10;          // Input capture 4 prescaler (2 bits)
    static const uint8_t CC4S = 8;             // Capture/Compare 4 selection (2 bits)
    static const uint8_t IC3F = 4;             // Input capture 3 filter (4 bits)
    static const uint8_t IC3PSC = 2;           // Input capture 3 prescaler (2 bits)
    static const uint8_t CC3S = 0;             // Capture/Compare 3 selection (2 bits)
}

namespace CCER // capture/compare enable register fields
{
    static const uint8_t CC4NP = 15;           // Capture/Compare 4 output Polarity
    static const uint8_t CC4P = 13;            // Capture/Compare 3 output Polarity
    static const uint8_t CC4E = 12;            // Capture/Compare 4 output enable
    static const uint8_t CC3NP = 11;           // Capture/Compare 3 output Polarity
    static const uint8_t CC3P = 9;             // Capture/Compare 3 output Polarity
    static const uint8_t CC3E = 8;             // Capture/Compare 3 output enable
    static const uint8_t CC2NP = 7;            // Capture/Compare 2 output Polarity
    static const uint8_t CC2P = 5;             // Capture/Compare 2 output Polarity
    static const uint8_t CC2E = 4;             // Capture/Compare 2 output enable
    static const uint8_t CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CC1E = 0;             // Capture/Compare 1 output enable
}

namespace CNT // counter fields
{
    static const uint8_t CNT_H = 16;           // High counter value (TIM2 only) (16 bits)
    static const uint8_t CNT_L = 0;            // Low counter value (16 bits)
}

namespace PSC // prescaler fields
{
    static const uint8_t PSC = 0;              // Prescaler value (16 bits)
}

namespace ARR // auto-reload register fields
{
    static const uint8_t ARR_H = 16;           // High Auto-reload value (TIM2 only) (16 bits)
    static const uint8_t ARR_L = 0;            // Low Auto-reload value (16 bits)
}

namespace CCR1 // capture/compare register 1 fields
{
    static const uint8_t CCR1_H = 16;          // High Capture/Compare 1 value (TIM2 only) (16 bits)
    static const uint8_t CCR1_L = 0;           // Low Capture/Compare 1 value (16 bits)
}

namespace CCR2 // capture/compare register 2 fields
{
    static const uint8_t CCR2_H = 16;          // High Capture/Compare 2 value (TIM2 only) (16 bits)
    static const uint8_t CCR2_L = 0;           // Low Capture/Compare 2 value (16 bits)
}

namespace CCR3 // capture/compare register 3 fields
{
    static const uint8_t CCR3_H = 16;          // High Capture/Compare value (TIM2 only) (16 bits)
    static const uint8_t CCR3_L = 0;           // Low Capture/Compare value (16 bits)
}

namespace CCR4 // capture/compare register 4 fields
{
    static const uint8_t CCR4_H = 16;          // High Capture/Compare value (TIM2 only) (16 bits)
    static const uint8_t CCR4_L = 0;           // Low Capture/Compare value (16 bits)
}

namespace DCR // DMA control register fields
{
    static const uint8_t DBL = 8;              // DMA burst length (5 bits)
    static const uint8_t DBA = 0;              // DMA base address (5 bits)
}

namespace DMAR // DMA address for full transfer fields
{
    static const uint8_t DMAR = 0;             // DMA register for burst accesses (16 bits)
}

}

////
//
//    General-purpose-timers
//
////

namespace tim14
{

struct tim14_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    reserved_t<2>        _0;
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1_Output;         // [Read-write] capture/compare mode register (output mode)
    volatile uint32_t    CCMR1_Input;          // [Read-write] capture/compare mode register (input mode)
    reserved_t<1>        _1;
    volatile uint32_t    CCER;                 // [Read-write] capture/compare enable register
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    reserved_t<1>        _2;
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    reserved_t<6>        _3;
    volatile uint32_t    OR;                   // [Read-write] option register
};

tim14_t& TIM14 = *reinterpret_cast<tim14_t*>(0x40002000);

namespace CR1 // control register 1 fields
{
    static const uint8_t CKD = 8;              // Clock division (2 bits)
    static const uint8_t ARPE = 7;             // Auto-reload preload enable
    static const uint8_t URS = 2;              // Update request source
    static const uint8_t UDIS = 1;             // Update disable
    static const uint8_t CEN = 0;              // Counter enable
}

namespace DIER // DMA/Interrupt enable register fields
{
    static const uint8_t CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t UIE = 0;              // Update interrupt enable
}

namespace SR // status register fields
{
    static const uint8_t CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t UIF = 0;              // Update interrupt flag
}

namespace EGR // event generation register fields
{
    static const uint8_t CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t UG = 0;               // Update generation
}

namespace CCMR1_Output // capture/compare mode register (output mode) fields
{
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
    static const uint8_t OC1FE = 2;            // Output compare 1 fast enable
    static const uint8_t OC1PE = 3;            // Output Compare 1 preload enable
    static const uint8_t OC1M = 4;             // Output Compare 1 mode (3 bits)
}

namespace CCMR1_Input // capture/compare mode register (input mode) fields
{
    static const uint8_t IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t IC1PSC = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCER // capture/compare enable register fields
{
    static const uint8_t CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CC1E = 0;             // Capture/Compare 1 output enable
}

namespace CNT // counter fields
{
    static const uint8_t CNT = 0;              // counter value (16 bits)
}

namespace PSC // prescaler fields
{
    static const uint8_t PSC = 0;              // Prescaler value (16 bits)
}

namespace ARR // auto-reload register fields
{
    static const uint8_t ARR = 0;              // Auto-reload value (16 bits)
}

namespace CCR1 // capture/compare register 1 fields
{
    static const uint8_t CCR1 = 0;             // Capture/Compare 1 value (16 bits)
}

namespace OR // option register fields
{
    static const uint8_t RMP = 0;              // Timer input 1 remap (2 bits)
}

}

////
//
//    Basic-timers
//
////

namespace tim6
{

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
};

tim6_t& TIM6 = *reinterpret_cast<tim6_t*>(0x40001000);

namespace CR1 // control register 1 fields
{
    static const uint8_t ARPE = 7;             // Auto-reload preload enable
    static const uint8_t OPM = 3;              // One-pulse mode
    static const uint8_t URS = 2;              // Update request source
    static const uint8_t UDIS = 1;             // Update disable
    static const uint8_t CEN = 0;              // Counter enable
}

namespace CR2 // control register 2 fields
{
    static const uint8_t MMS = 4;              // Master mode selection (3 bits)
}

namespace DIER // DMA/Interrupt enable register fields
{
    static const uint8_t UDE = 8;              // Update DMA request enable
    static const uint8_t UIE = 0;              // Update interrupt enable
}

namespace SR // status register fields
{
    static const uint8_t UIF = 0;              // Update interrupt flag
}

namespace EGR // event generation register fields
{
    static const uint8_t UG = 0;               // Update generation
}

namespace CNT // counter fields
{
    static const uint8_t CNT = 0;              // Low counter value (16 bits)
}

namespace PSC // prescaler fields
{
    static const uint8_t PSC = 0;              // Prescaler value (16 bits)
}

namespace ARR // auto-reload register fields
{
    static const uint8_t ARR = 0;              // Low Auto-reload value (16 bits)
}

}

////
//
//    Basic-timers
//
////

namespace tim7
{

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
};

tim7_t& TIM7 = *reinterpret_cast<tim7_t*>(0x40001400);

namespace CR1 // control register 1 fields
{
    static const uint8_t ARPE = 7;             // Auto-reload preload enable
    static const uint8_t OPM = 3;              // One-pulse mode
    static const uint8_t URS = 2;              // Update request source
    static const uint8_t UDIS = 1;             // Update disable
    static const uint8_t CEN = 0;              // Counter enable
}

namespace CR2 // control register 2 fields
{
    static const uint8_t MMS = 4;              // Master mode selection (3 bits)
}

namespace DIER // DMA/Interrupt enable register fields
{
    static const uint8_t UDE = 8;              // Update DMA request enable
    static const uint8_t UIE = 0;              // Update interrupt enable
}

namespace SR // status register fields
{
    static const uint8_t UIF = 0;              // Update interrupt flag
}

namespace EGR // event generation register fields
{
    static const uint8_t UG = 0;               // Update generation
}

namespace CNT // counter fields
{
    static const uint8_t CNT = 0;              // Low counter value (16 bits)
}

namespace PSC // prescaler fields
{
    static const uint8_t PSC = 0;              // Prescaler value (16 bits)
}

namespace ARR // auto-reload register fields
{
    static const uint8_t ARR = 0;              // Low Auto-reload value (16 bits)
}

}

////
//
//    External interrupt/event controller
//
////

namespace exti
{

struct exti_t
{
    volatile uint32_t    IMR;                  // [Read-write] Interrupt mask register (EXTI_IMR)
    volatile uint32_t    EMR;                  // [Read-write] Event mask register (EXTI_EMR)
    volatile uint32_t    RTSR;                 // [Read-write] Rising Trigger selection register (EXTI_RTSR)
    volatile uint32_t    FTSR;                 // [Read-write] Falling Trigger selection register (EXTI_FTSR)
    volatile uint32_t    SWIER;                // [Read-write] Software interrupt event register (EXTI_SWIER)
    volatile uint32_t    PR;                   // [Read-write] Pending register (EXTI_PR)
};

exti_t& EXTI = *reinterpret_cast<exti_t*>(0x40010400);

namespace IMR // Interrupt mask register (EXTI_IMR) fields
{
    static const uint8_t MR0 = 0;              // Interrupt Mask on line 0
    static const uint8_t MR1 = 1;              // Interrupt Mask on line 1
    static const uint8_t MR2 = 2;              // Interrupt Mask on line 2
    static const uint8_t MR3 = 3;              // Interrupt Mask on line 3
    static const uint8_t MR4 = 4;              // Interrupt Mask on line 4
    static const uint8_t MR5 = 5;              // Interrupt Mask on line 5
    static const uint8_t MR6 = 6;              // Interrupt Mask on line 6
    static const uint8_t MR7 = 7;              // Interrupt Mask on line 7
    static const uint8_t MR8 = 8;              // Interrupt Mask on line 8
    static const uint8_t MR9 = 9;              // Interrupt Mask on line 9
    static const uint8_t MR10 = 10;            // Interrupt Mask on line 10
    static const uint8_t MR11 = 11;            // Interrupt Mask on line 11
    static const uint8_t MR12 = 12;            // Interrupt Mask on line 12
    static const uint8_t MR13 = 13;            // Interrupt Mask on line 13
    static const uint8_t MR14 = 14;            // Interrupt Mask on line 14
    static const uint8_t MR15 = 15;            // Interrupt Mask on line 15
    static const uint8_t MR16 = 16;            // Interrupt Mask on line 16
    static const uint8_t MR17 = 17;            // Interrupt Mask on line 17
    static const uint8_t MR18 = 18;            // Interrupt Mask on line 18
    static const uint8_t MR19 = 19;            // Interrupt Mask on line 19
    static const uint8_t MR20 = 20;            // Interrupt Mask on line 20
    static const uint8_t MR21 = 21;            // Interrupt Mask on line 21
    static const uint8_t MR22 = 22;            // Interrupt Mask on line 22
    static const uint8_t MR23 = 23;            // Interrupt Mask on line 23
    static const uint8_t MR24 = 24;            // Interrupt Mask on line 24
    static const uint8_t MR25 = 25;            // Interrupt Mask on line 25
    static const uint8_t MR26 = 26;            // Interrupt Mask on line 26
    static const uint8_t MR27 = 27;            // Interrupt Mask on line 27
}

namespace EMR // Event mask register (EXTI_EMR) fields
{
    static const uint8_t MR0 = 0;              // Event Mask on line 0
    static const uint8_t MR1 = 1;              // Event Mask on line 1
    static const uint8_t MR2 = 2;              // Event Mask on line 2
    static const uint8_t MR3 = 3;              // Event Mask on line 3
    static const uint8_t MR4 = 4;              // Event Mask on line 4
    static const uint8_t MR5 = 5;              // Event Mask on line 5
    static const uint8_t MR6 = 6;              // Event Mask on line 6
    static const uint8_t MR7 = 7;              // Event Mask on line 7
    static const uint8_t MR8 = 8;              // Event Mask on line 8
    static const uint8_t MR9 = 9;              // Event Mask on line 9
    static const uint8_t MR10 = 10;            // Event Mask on line 10
    static const uint8_t MR11 = 11;            // Event Mask on line 11
    static const uint8_t MR12 = 12;            // Event Mask on line 12
    static const uint8_t MR13 = 13;            // Event Mask on line 13
    static const uint8_t MR14 = 14;            // Event Mask on line 14
    static const uint8_t MR15 = 15;            // Event Mask on line 15
    static const uint8_t MR16 = 16;            // Event Mask on line 16
    static const uint8_t MR17 = 17;            // Event Mask on line 17
    static const uint8_t MR18 = 18;            // Event Mask on line 18
    static const uint8_t MR19 = 19;            // Event Mask on line 19
    static const uint8_t MR20 = 20;            // Event Mask on line 20
    static const uint8_t MR21 = 21;            // Event Mask on line 21
    static const uint8_t MR22 = 22;            // Event Mask on line 22
    static const uint8_t MR23 = 23;            // Event Mask on line 23
    static const uint8_t MR24 = 24;            // Event Mask on line 24
    static const uint8_t MR25 = 25;            // Event Mask on line 25
    static const uint8_t MR26 = 26;            // Event Mask on line 26
    static const uint8_t MR27 = 27;            // Event Mask on line 27
}

namespace RTSR // Rising Trigger selection register (EXTI_RTSR) fields
{
    static const uint8_t TR0 = 0;              // Rising trigger event configuration of line 0
    static const uint8_t TR1 = 1;              // Rising trigger event configuration of line 1
    static const uint8_t TR2 = 2;              // Rising trigger event configuration of line 2
    static const uint8_t TR3 = 3;              // Rising trigger event configuration of line 3
    static const uint8_t TR4 = 4;              // Rising trigger event configuration of line 4
    static const uint8_t TR5 = 5;              // Rising trigger event configuration of line 5
    static const uint8_t TR6 = 6;              // Rising trigger event configuration of line 6
    static const uint8_t TR7 = 7;              // Rising trigger event configuration of line 7
    static const uint8_t TR8 = 8;              // Rising trigger event configuration of line 8
    static const uint8_t TR9 = 9;              // Rising trigger event configuration of line 9
    static const uint8_t TR10 = 10;            // Rising trigger event configuration of line 10
    static const uint8_t TR11 = 11;            // Rising trigger event configuration of line 11
    static const uint8_t TR12 = 12;            // Rising trigger event configuration of line 12
    static const uint8_t TR13 = 13;            // Rising trigger event configuration of line 13
    static const uint8_t TR14 = 14;            // Rising trigger event configuration of line 14
    static const uint8_t TR15 = 15;            // Rising trigger event configuration of line 15
    static const uint8_t TR16 = 16;            // Rising trigger event configuration of line 16
    static const uint8_t TR17 = 17;            // Rising trigger event configuration of line 17
    static const uint8_t TR19 = 19;            // Rising trigger event configuration of line 19
}

namespace FTSR // Falling Trigger selection register (EXTI_FTSR) fields
{
    static const uint8_t TR0 = 0;              // Falling trigger event configuration of line 0
    static const uint8_t TR1 = 1;              // Falling trigger event configuration of line 1
    static const uint8_t TR2 = 2;              // Falling trigger event configuration of line 2
    static const uint8_t TR3 = 3;              // Falling trigger event configuration of line 3
    static const uint8_t TR4 = 4;              // Falling trigger event configuration of line 4
    static const uint8_t TR5 = 5;              // Falling trigger event configuration of line 5
    static const uint8_t TR6 = 6;              // Falling trigger event configuration of line 6
    static const uint8_t TR7 = 7;              // Falling trigger event configuration of line 7
    static const uint8_t TR8 = 8;              // Falling trigger event configuration of line 8
    static const uint8_t TR9 = 9;              // Falling trigger event configuration of line 9
    static const uint8_t TR10 = 10;            // Falling trigger event configuration of line 10
    static const uint8_t TR11 = 11;            // Falling trigger event configuration of line 11
    static const uint8_t TR12 = 12;            // Falling trigger event configuration of line 12
    static const uint8_t TR13 = 13;            // Falling trigger event configuration of line 13
    static const uint8_t TR14 = 14;            // Falling trigger event configuration of line 14
    static const uint8_t TR15 = 15;            // Falling trigger event configuration of line 15
    static const uint8_t TR16 = 16;            // Falling trigger event configuration of line 16
    static const uint8_t TR17 = 17;            // Falling trigger event configuration of line 17
    static const uint8_t TR19 = 19;            // Falling trigger event configuration of line 19
}

namespace SWIER // Software interrupt event register (EXTI_SWIER) fields
{
    static const uint8_t SWIER0 = 0;           // Software Interrupt on line 0
    static const uint8_t SWIER1 = 1;           // Software Interrupt on line 1
    static const uint8_t SWIER2 = 2;           // Software Interrupt on line 2
    static const uint8_t SWIER3 = 3;           // Software Interrupt on line 3
    static const uint8_t SWIER4 = 4;           // Software Interrupt on line 4
    static const uint8_t SWIER5 = 5;           // Software Interrupt on line 5
    static const uint8_t SWIER6 = 6;           // Software Interrupt on line 6
    static const uint8_t SWIER7 = 7;           // Software Interrupt on line 7
    static const uint8_t SWIER8 = 8;           // Software Interrupt on line 8
    static const uint8_t SWIER9 = 9;           // Software Interrupt on line 9
    static const uint8_t SWIER10 = 10;         // Software Interrupt on line 10
    static const uint8_t SWIER11 = 11;         // Software Interrupt on line 11
    static const uint8_t SWIER12 = 12;         // Software Interrupt on line 12
    static const uint8_t SWIER13 = 13;         // Software Interrupt on line 13
    static const uint8_t SWIER14 = 14;         // Software Interrupt on line 14
    static const uint8_t SWIER15 = 15;         // Software Interrupt on line 15
    static const uint8_t SWIER16 = 16;         // Software Interrupt on line 16
    static const uint8_t SWIER17 = 17;         // Software Interrupt on line 17
    static const uint8_t SWIER19 = 19;         // Software Interrupt on line 19
}

namespace PR // Pending register (EXTI_PR) fields
{
    static const uint8_t PR0 = 0;              // Pending bit 0
    static const uint8_t PR1 = 1;              // Pending bit 1
    static const uint8_t PR2 = 2;              // Pending bit 2
    static const uint8_t PR3 = 3;              // Pending bit 3
    static const uint8_t PR4 = 4;              // Pending bit 4
    static const uint8_t PR5 = 5;              // Pending bit 5
    static const uint8_t PR6 = 6;              // Pending bit 6
    static const uint8_t PR7 = 7;              // Pending bit 7
    static const uint8_t PR8 = 8;              // Pending bit 8
    static const uint8_t PR9 = 9;              // Pending bit 9
    static const uint8_t PR10 = 10;            // Pending bit 10
    static const uint8_t PR11 = 11;            // Pending bit 11
    static const uint8_t PR12 = 12;            // Pending bit 12
    static const uint8_t PR13 = 13;            // Pending bit 13
    static const uint8_t PR14 = 14;            // Pending bit 14
    static const uint8_t PR15 = 15;            // Pending bit 15
    static const uint8_t PR16 = 16;            // Pending bit 16
    static const uint8_t PR17 = 17;            // Pending bit 17
    static const uint8_t PR19 = 19;            // Pending bit 19
}

}

////
//
//    Nested Vectored Interrupt Controller
//
////

namespace nvic
{

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
};

nvic_t& NVIC = *reinterpret_cast<nvic_t*>(0xe000e100);

namespace ISER // Interrupt Set Enable Register fields
{
    static const uint8_t SETENA = 0;           // SETENA (32 bits)
}

namespace ICER // Interrupt Clear Enable Register fields
{
    static const uint8_t CLRENA = 0;           // CLRENA (32 bits)
}

namespace ISPR // Interrupt Set-Pending Register fields
{
    static const uint8_t SETPEND = 0;          // SETPEND (32 bits)
}

namespace ICPR // Interrupt Clear-Pending Register fields
{
    static const uint8_t CLRPEND = 0;          // CLRPEND (32 bits)
}

namespace IPR0 // Interrupt Priority Register 0 fields
{
    static const uint8_t PRI_00 = 6;           // PRI_00 (2 bits)
    static const uint8_t PRI_01 = 14;          // PRI_01 (2 bits)
    static const uint8_t PRI_02 = 22;          // PRI_02 (2 bits)
    static const uint8_t PRI_03 = 30;          // PRI_03 (2 bits)
}

namespace IPR1 // Interrupt Priority Register 1 fields
{
    static const uint8_t PRI_40 = 6;           // PRI_40 (2 bits)
    static const uint8_t PRI_41 = 14;          // PRI_41 (2 bits)
    static const uint8_t PRI_42 = 22;          // PRI_42 (2 bits)
    static const uint8_t PRI_43 = 30;          // PRI_43 (2 bits)
}

namespace IPR2 // Interrupt Priority Register 2 fields
{
    static const uint8_t PRI_80 = 6;           // PRI_80 (2 bits)
    static const uint8_t PRI_81 = 14;          // PRI_81 (2 bits)
    static const uint8_t PRI_82 = 22;          // PRI_82 (2 bits)
    static const uint8_t PRI_83 = 30;          // PRI_83 (2 bits)
}

namespace IPR3 // Interrupt Priority Register 3 fields
{
    static const uint8_t PRI_120 = 6;          // PRI_120 (2 bits)
    static const uint8_t PRI_121 = 14;         // PRI_121 (2 bits)
    static const uint8_t PRI_122 = 22;         // PRI_122 (2 bits)
    static const uint8_t PRI_123 = 30;         // PRI_123 (2 bits)
}

namespace IPR4 // Interrupt Priority Register 4 fields
{
    static const uint8_t PRI_160 = 6;          // PRI_160 (2 bits)
    static const uint8_t PRI_161 = 14;         // PRI_161 (2 bits)
    static const uint8_t PRI_162 = 22;         // PRI_162 (2 bits)
    static const uint8_t PRI_163 = 30;         // PRI_163 (2 bits)
}

namespace IPR5 // Interrupt Priority Register 5 fields
{
    static const uint8_t PRI_200 = 6;          // PRI_200 (2 bits)
    static const uint8_t PRI_201 = 14;         // PRI_201 (2 bits)
    static const uint8_t PRI_202 = 22;         // PRI_202 (2 bits)
    static const uint8_t PRI_203 = 30;         // PRI_203 (2 bits)
}

namespace IPR6 // Interrupt Priority Register 6 fields
{
    static const uint8_t PRI_240 = 6;          // PRI_240 (2 bits)
    static const uint8_t PRI_241 = 14;         // PRI_241 (2 bits)
    static const uint8_t PRI_242 = 22;         // PRI_242 (2 bits)
    static const uint8_t PRI_243 = 30;         // PRI_243 (2 bits)
}

namespace IPR7 // Interrupt Priority Register 7 fields
{
    static const uint8_t PRI_280 = 6;          // PRI_280 (2 bits)
    static const uint8_t PRI_281 = 14;         // PRI_281 (2 bits)
    static const uint8_t PRI_282 = 22;         // PRI_282 (2 bits)
    static const uint8_t PRI_283 = 30;         // PRI_283 (2 bits)
}

}

////
//
//    DMA controller
//
////

namespace dma1
{

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
};

dma1_t& DMA1 = *reinterpret_cast<dma1_t*>(0x40020000);

namespace ISR // DMA interrupt status register (DMA_ISR) fields
{
    static const uint8_t GIF1 = 0;             // Channel 1 Global interrupt flag
    static const uint8_t TCIF1 = 1;            // Channel 1 Transfer Complete flag
    static const uint8_t HTIF1 = 2;            // Channel 1 Half Transfer Complete flag
    static const uint8_t TEIF1 = 3;            // Channel 1 Transfer Error flag
    static const uint8_t GIF2 = 4;             // Channel 2 Global interrupt flag
    static const uint8_t TCIF2 = 5;            // Channel 2 Transfer Complete flag
    static const uint8_t HTIF2 = 6;            // Channel 2 Half Transfer Complete flag
    static const uint8_t TEIF2 = 7;            // Channel 2 Transfer Error flag
    static const uint8_t GIF3 = 8;             // Channel 3 Global interrupt flag
    static const uint8_t TCIF3 = 9;            // Channel 3 Transfer Complete flag
    static const uint8_t HTIF3 = 10;           // Channel 3 Half Transfer Complete flag
    static const uint8_t TEIF3 = 11;           // Channel 3 Transfer Error flag
    static const uint8_t GIF4 = 12;            // Channel 4 Global interrupt flag
    static const uint8_t TCIF4 = 13;           // Channel 4 Transfer Complete flag
    static const uint8_t HTIF4 = 14;           // Channel 4 Half Transfer Complete flag
    static const uint8_t TEIF4 = 15;           // Channel 4 Transfer Error flag
    static const uint8_t GIF5 = 16;            // Channel 5 Global interrupt flag
    static const uint8_t TCIF5 = 17;           // Channel 5 Transfer Complete flag
    static const uint8_t HTIF5 = 18;           // Channel 5 Half Transfer Complete flag
    static const uint8_t TEIF5 = 19;           // Channel 5 Transfer Error flag
    static const uint8_t GIF6 = 20;            // Channel 6 Global interrupt flag
    static const uint8_t TCIF6 = 21;           // Channel 6 Transfer Complete flag
    static const uint8_t HTIF6 = 22;           // Channel 6 Half Transfer Complete flag
    static const uint8_t TEIF6 = 23;           // Channel 6 Transfer Error flag
    static const uint8_t GIF7 = 24;            // Channel 7 Global interrupt flag
    static const uint8_t TCIF7 = 25;           // Channel 7 Transfer Complete flag
    static const uint8_t HTIF7 = 26;           // Channel 7 Half Transfer Complete flag
    static const uint8_t TEIF7 = 27;           // Channel 7 Transfer Error flag
}

namespace IFCR // DMA interrupt flag clear register (DMA_IFCR) fields
{
    static const uint8_t CGIF1 = 0;            // Channel 1 Global interrupt clear
    static const uint8_t CTCIF1 = 1;           // Channel 1 Transfer Complete clear
    static const uint8_t CHTIF1 = 2;           // Channel 1 Half Transfer clear
    static const uint8_t CTEIF1 = 3;           // Channel 1 Transfer Error clear
    static const uint8_t CGIF2 = 4;            // Channel 2 Global interrupt clear
    static const uint8_t CTCIF2 = 5;           // Channel 2 Transfer Complete clear
    static const uint8_t CHTIF2 = 6;           // Channel 2 Half Transfer clear
    static const uint8_t CTEIF2 = 7;           // Channel 2 Transfer Error clear
    static const uint8_t CGIF3 = 8;            // Channel 3 Global interrupt clear
    static const uint8_t CTCIF3 = 9;           // Channel 3 Transfer Complete clear
    static const uint8_t CHTIF3 = 10;          // Channel 3 Half Transfer clear
    static const uint8_t CTEIF3 = 11;          // Channel 3 Transfer Error clear
    static const uint8_t CGIF4 = 12;           // Channel 4 Global interrupt clear
    static const uint8_t CTCIF4 = 13;          // Channel 4 Transfer Complete clear
    static const uint8_t CHTIF4 = 14;          // Channel 4 Half Transfer clear
    static const uint8_t CTEIF4 = 15;          // Channel 4 Transfer Error clear
    static const uint8_t CGIF5 = 16;           // Channel 5 Global interrupt clear
    static const uint8_t CTCIF5 = 17;          // Channel 5 Transfer Complete clear
    static const uint8_t CHTIF5 = 18;          // Channel 5 Half Transfer clear
    static const uint8_t CTEIF5 = 19;          // Channel 5 Transfer Error clear
    static const uint8_t CGIF6 = 20;           // Channel 6 Global interrupt clear
    static const uint8_t CTCIF6 = 21;          // Channel 6 Transfer Complete clear
    static const uint8_t CHTIF6 = 22;          // Channel 6 Half Transfer clear
    static const uint8_t CTEIF6 = 23;          // Channel 6 Transfer Error clear
    static const uint8_t CGIF7 = 24;           // Channel 7 Global interrupt clear
    static const uint8_t CTCIF7 = 25;          // Channel 7 Transfer Complete clear
    static const uint8_t CHTIF7 = 26;          // Channel 7 Half Transfer clear
    static const uint8_t CTEIF7 = 27;          // Channel 7 Transfer Error clear
}

namespace CCR1 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR1 // DMA channel 1 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR1 // DMA channel 1 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR1 // DMA channel 1 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

namespace CCR2 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR2 // DMA channel 2 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR2 // DMA channel 2 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR2 // DMA channel 2 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

namespace CCR3 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR3 // DMA channel 3 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR3 // DMA channel 3 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR3 // DMA channel 3 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

namespace CCR4 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR4 // DMA channel 4 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR4 // DMA channel 4 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR4 // DMA channel 4 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

namespace CCR5 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR5 // DMA channel 5 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR5 // DMA channel 5 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR5 // DMA channel 5 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

namespace CCR6 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR6 // DMA channel 6 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR6 // DMA channel 6 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR6 // DMA channel 6 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

namespace CCR7 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR7 // DMA channel 7 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR7 // DMA channel 7 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR7 // DMA channel 7 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

}

////
//
//    DMA controller
//
////

namespace dma2
{

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
};

dma2_t& DMA2 = *reinterpret_cast<dma2_t*>(0x40020400);

namespace ISR // DMA interrupt status register (DMA_ISR) fields
{
    static const uint8_t GIF1 = 0;             // Channel 1 Global interrupt flag
    static const uint8_t TCIF1 = 1;            // Channel 1 Transfer Complete flag
    static const uint8_t HTIF1 = 2;            // Channel 1 Half Transfer Complete flag
    static const uint8_t TEIF1 = 3;            // Channel 1 Transfer Error flag
    static const uint8_t GIF2 = 4;             // Channel 2 Global interrupt flag
    static const uint8_t TCIF2 = 5;            // Channel 2 Transfer Complete flag
    static const uint8_t HTIF2 = 6;            // Channel 2 Half Transfer Complete flag
    static const uint8_t TEIF2 = 7;            // Channel 2 Transfer Error flag
    static const uint8_t GIF3 = 8;             // Channel 3 Global interrupt flag
    static const uint8_t TCIF3 = 9;            // Channel 3 Transfer Complete flag
    static const uint8_t HTIF3 = 10;           // Channel 3 Half Transfer Complete flag
    static const uint8_t TEIF3 = 11;           // Channel 3 Transfer Error flag
    static const uint8_t GIF4 = 12;            // Channel 4 Global interrupt flag
    static const uint8_t TCIF4 = 13;           // Channel 4 Transfer Complete flag
    static const uint8_t HTIF4 = 14;           // Channel 4 Half Transfer Complete flag
    static const uint8_t TEIF4 = 15;           // Channel 4 Transfer Error flag
    static const uint8_t GIF5 = 16;            // Channel 5 Global interrupt flag
    static const uint8_t TCIF5 = 17;           // Channel 5 Transfer Complete flag
    static const uint8_t HTIF5 = 18;           // Channel 5 Half Transfer Complete flag
    static const uint8_t TEIF5 = 19;           // Channel 5 Transfer Error flag
    static const uint8_t GIF6 = 20;            // Channel 6 Global interrupt flag
    static const uint8_t TCIF6 = 21;           // Channel 6 Transfer Complete flag
    static const uint8_t HTIF6 = 22;           // Channel 6 Half Transfer Complete flag
    static const uint8_t TEIF6 = 23;           // Channel 6 Transfer Error flag
    static const uint8_t GIF7 = 24;            // Channel 7 Global interrupt flag
    static const uint8_t TCIF7 = 25;           // Channel 7 Transfer Complete flag
    static const uint8_t HTIF7 = 26;           // Channel 7 Half Transfer Complete flag
    static const uint8_t TEIF7 = 27;           // Channel 7 Transfer Error flag
}

namespace IFCR // DMA interrupt flag clear register (DMA_IFCR) fields
{
    static const uint8_t CGIF1 = 0;            // Channel 1 Global interrupt clear
    static const uint8_t CTCIF1 = 1;           // Channel 1 Transfer Complete clear
    static const uint8_t CHTIF1 = 2;           // Channel 1 Half Transfer clear
    static const uint8_t CTEIF1 = 3;           // Channel 1 Transfer Error clear
    static const uint8_t CGIF2 = 4;            // Channel 2 Global interrupt clear
    static const uint8_t CTCIF2 = 5;           // Channel 2 Transfer Complete clear
    static const uint8_t CHTIF2 = 6;           // Channel 2 Half Transfer clear
    static const uint8_t CTEIF2 = 7;           // Channel 2 Transfer Error clear
    static const uint8_t CGIF3 = 8;            // Channel 3 Global interrupt clear
    static const uint8_t CTCIF3 = 9;           // Channel 3 Transfer Complete clear
    static const uint8_t CHTIF3 = 10;          // Channel 3 Half Transfer clear
    static const uint8_t CTEIF3 = 11;          // Channel 3 Transfer Error clear
    static const uint8_t CGIF4 = 12;           // Channel 4 Global interrupt clear
    static const uint8_t CTCIF4 = 13;          // Channel 4 Transfer Complete clear
    static const uint8_t CHTIF4 = 14;          // Channel 4 Half Transfer clear
    static const uint8_t CTEIF4 = 15;          // Channel 4 Transfer Error clear
    static const uint8_t CGIF5 = 16;           // Channel 5 Global interrupt clear
    static const uint8_t CTCIF5 = 17;          // Channel 5 Transfer Complete clear
    static const uint8_t CHTIF5 = 18;          // Channel 5 Half Transfer clear
    static const uint8_t CTEIF5 = 19;          // Channel 5 Transfer Error clear
    static const uint8_t CGIF6 = 20;           // Channel 6 Global interrupt clear
    static const uint8_t CTCIF6 = 21;          // Channel 6 Transfer Complete clear
    static const uint8_t CHTIF6 = 22;          // Channel 6 Half Transfer clear
    static const uint8_t CTEIF6 = 23;          // Channel 6 Transfer Error clear
    static const uint8_t CGIF7 = 24;           // Channel 7 Global interrupt clear
    static const uint8_t CTCIF7 = 25;          // Channel 7 Transfer Complete clear
    static const uint8_t CHTIF7 = 26;          // Channel 7 Half Transfer clear
    static const uint8_t CTEIF7 = 27;          // Channel 7 Transfer Error clear
}

namespace CCR1 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR1 // DMA channel 1 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR1 // DMA channel 1 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR1 // DMA channel 1 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

namespace CCR2 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR2 // DMA channel 2 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR2 // DMA channel 2 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR2 // DMA channel 2 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

namespace CCR3 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR3 // DMA channel 3 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR3 // DMA channel 3 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR3 // DMA channel 3 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

namespace CCR4 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR4 // DMA channel 4 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR4 // DMA channel 4 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR4 // DMA channel 4 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

namespace CCR5 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR5 // DMA channel 5 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR5 // DMA channel 5 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR5 // DMA channel 5 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

namespace CCR6 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR6 // DMA channel 6 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR6 // DMA channel 6 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR6 // DMA channel 6 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

namespace CCR7 // DMA channel configuration register (DMA_CCR) fields
{
    static const uint8_t EN = 0;               // Channel enable
    static const uint8_t TCIE = 1;             // Transfer complete interrupt enable
    static const uint8_t HTIE = 2;             // Half Transfer interrupt enable
    static const uint8_t TEIE = 3;             // Transfer error interrupt enable
    static const uint8_t DIR = 4;              // Data transfer direction
    static const uint8_t CIRC = 5;             // Circular mode
    static const uint8_t PINC = 6;             // Peripheral increment mode
    static const uint8_t MINC = 7;             // Memory increment mode
    static const uint8_t PSIZE = 8;            // Peripheral size (2 bits)
    static const uint8_t MSIZE = 10;           // Memory size (2 bits)
    static const uint8_t PL = 12;              // Channel Priority level (2 bits)
    static const uint8_t MEM2MEM = 14;         // Memory to memory mode
}

namespace CNDTR7 // DMA channel 7 number of data register fields
{
    static const uint8_t NDT = 0;              // Number of data to transfer (16 bits)
}

namespace CPAR7 // DMA channel 7 peripheral address register fields
{
    static const uint8_t PA = 0;               // Peripheral address (32 bits)
}

namespace CMAR7 // DMA channel 7 memory address register fields
{
    static const uint8_t MA = 0;               // Memory address (32 bits)
}

}

////
//
//    Reset and clock control
//
////

namespace rcc
{

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
};

rcc_t& RCC = *reinterpret_cast<rcc_t*>(0x40021000);

namespace CR // Clock control register fields
{
    static const uint8_t HSION = 0;            // Internal High Speed clock enable, Read-write
    static const uint8_t HSIRDY = 1;           // Internal High Speed clock ready flag, Read-only
    static const uint8_t HSITRIM = 3;          // Internal High Speed clock trimming (5 bits), Read-write
    static const uint8_t HSICAL = 8;           // Internal High Speed clock Calibration (8 bits), Read-only
    static const uint8_t HSEON = 16;           // External High Speed clock enable, Read-write
    static const uint8_t HSERDY = 17;          // External High Speed clock ready flag, Read-only
    static const uint8_t HSEBYP = 18;          // External High Speed clock Bypass, Read-write
    static const uint8_t CSSON = 19;           // Clock Security System enable, Read-write
    static const uint8_t PLLON = 24;           // PLL enable, Read-write
    static const uint8_t PLLRDY = 25;          // PLL clock ready flag, Read-only
}

namespace CFGR // Clock configuration register (RCC_CFGR) fields
{
    static const uint8_t SW = 0;               // System clock Switch (2 bits), Read-write
    static const uint8_t SWS = 2;              // System Clock Switch Status (2 bits), Read-only
    static const uint8_t HPRE = 4;             // AHB prescaler (4 bits), Read-write
    static const uint8_t PPRE = 8;             // APB Low speed prescaler (APB1) (3 bits), Read-write
    static const uint8_t ADCPRE = 14;          // ADC prescaler, Read-write
    static const uint8_t PLLSRC = 15;          // PLL input clock source (2 bits), Read-write
    static const uint8_t PLLXTPRE = 17;        // HSE divider for PLL entry, Read-write
    static const uint8_t PLLMUL = 18;          // PLL Multiplication Factor (4 bits), Read-write
    static const uint8_t MCO = 24;             // Microcontroller clock output (3 bits), Read-write
    static const uint8_t MCOPRE = 28;          // Microcontroller Clock Output Prescaler (3 bits), Read-write
    static const uint8_t PLLNODIV = 31;        // PLL clock not divided for MCO, Read-write
}

namespace CIR // Clock interrupt register (RCC_CIR) fields
{
    static const uint8_t LSIRDYF = 0;          // LSI Ready Interrupt flag, Read-only
    static const uint8_t LSERDYF = 1;          // LSE Ready Interrupt flag, Read-only
    static const uint8_t HSIRDYF = 2;          // HSI Ready Interrupt flag, Read-only
    static const uint8_t HSERDYF = 3;          // HSE Ready Interrupt flag, Read-only
    static const uint8_t PLLRDYF = 4;          // PLL Ready Interrupt flag, Read-only
    static const uint8_t HSI14RDYF = 5;        // HSI14 ready interrupt flag, Read-only
    static const uint8_t HSI48RDYF = 6;        // HSI48 ready interrupt flag, Read-only
    static const uint8_t CSSF = 7;             // Clock Security System Interrupt flag, Read-only
    static const uint8_t LSIRDYIE = 8;         // LSI Ready Interrupt Enable, Read-write
    static const uint8_t LSERDYIE = 9;         // LSE Ready Interrupt Enable, Read-write
    static const uint8_t HSIRDYIE = 10;        // HSI Ready Interrupt Enable, Read-write
    static const uint8_t HSERDYIE = 11;        // HSE Ready Interrupt Enable, Read-write
    static const uint8_t PLLRDYIE = 12;        // PLL Ready Interrupt Enable, Read-write
    static const uint8_t HSI14RDYE = 13;       // HSI14 ready interrupt enable, Read-write
    static const uint8_t HSI48RDYIE = 14;      // HSI48 ready interrupt enable, Read-write
    static const uint8_t LSIRDYC = 16;         // LSI Ready Interrupt Clear, Write-only
    static const uint8_t LSERDYC = 17;         // LSE Ready Interrupt Clear, Write-only
    static const uint8_t HSIRDYC = 18;         // HSI Ready Interrupt Clear, Write-only
    static const uint8_t HSERDYC = 19;         // HSE Ready Interrupt Clear, Write-only
    static const uint8_t PLLRDYC = 20;         // PLL Ready Interrupt Clear, Write-only
    static const uint8_t HSI14RDYC = 21;       // HSI 14 MHz Ready Interrupt Clear, Write-only
    static const uint8_t HSI48RDYC = 22;       // HSI48 Ready Interrupt Clear, Write-only
    static const uint8_t CSSC = 23;            // Clock security system interrupt clear, Write-only
}

namespace APB2RSTR // APB2 peripheral reset register (RCC_APB2RSTR) fields
{
    static const uint8_t SYSCFGRST = 0;        // SYSCFG and COMP reset
    static const uint8_t ADCRST = 9;           // ADC interface reset
    static const uint8_t TIM1RST = 11;         // TIM1 timer reset
    static const uint8_t SPI1RST = 12;         // SPI 1 reset
    static const uint8_t USART1RST = 14;       // USART1 reset
    static const uint8_t TIM15RST = 16;        // TIM15 timer reset
    static const uint8_t TIM16RST = 17;        // TIM16 timer reset
    static const uint8_t TIM17RST = 18;        // TIM17 timer reset
    static const uint8_t DBGMCURST = 22;       // Debug MCU reset
}

namespace APB1RSTR // APB1 peripheral reset register (RCC_APB1RSTR) fields
{
    static const uint8_t TIM2RST = 0;          // Timer 2 reset
    static const uint8_t TIM3RST = 1;          // Timer 3 reset
    static const uint8_t TIM6RST = 4;          // Timer 6 reset
    static const uint8_t TIM7RST = 5;          // TIM7 timer reset
    static const uint8_t TIM14RST = 8;         // Timer 14 reset
    static const uint8_t WWDGRST = 11;         // Window watchdog reset
    static const uint8_t SPI2RST = 14;         // SPI2 reset
    static const uint8_t USART2RST = 17;       // USART 2 reset
    static const uint8_t USART3RST = 18;       // USART3 reset
    static const uint8_t USART4RST = 19;       // USART4 reset
    static const uint8_t USART5RST = 20;       // USART5 reset
    static const uint8_t I2C1RST = 21;         // I2C1 reset
    static const uint8_t I2C2RST = 22;         // I2C2 reset
    static const uint8_t USBRST = 23;          // USB interface reset
    static const uint8_t CANRST = 25;          // CAN interface reset
    static const uint8_t CRSRST = 27;          // Clock Recovery System interface reset
    static const uint8_t PWRRST = 28;          // Power interface reset
    static const uint8_t DACRST = 29;          // DAC interface reset
    static const uint8_t CECRST = 30;          // HDMI CEC reset
}

namespace AHBENR // AHB Peripheral Clock enable register (RCC_AHBENR) fields
{
    static const uint8_t DMA1EN = 0;           // DMA1 clock enable
    static const uint8_t DMA2EN = 1;           // DMA2 clock enable
    static const uint8_t SRAMEN = 2;           // SRAM interface clock enable
    static const uint8_t FLITFEN = 4;          // FLITF clock enable
    static const uint8_t CRCEN = 6;            // CRC clock enable
    static const uint8_t IOPAEN = 17;          // I/O port A clock enable
    static const uint8_t IOPBEN = 18;          // I/O port B clock enable
    static const uint8_t IOPCEN = 19;          // I/O port C clock enable
    static const uint8_t IOPDEN = 20;          // I/O port D clock enable
    static const uint8_t IOPFEN = 22;          // I/O port F clock enable
    static const uint8_t TSCEN = 24;           // Touch sensing controller clock enable
}

namespace APB2ENR // APB2 peripheral clock enable register (RCC_APB2ENR) fields
{
    static const uint8_t SYSCFGEN = 0;         // SYSCFG clock enable
    static const uint8_t ADCEN = 9;            // ADC 1 interface clock enable
    static const uint8_t TIM1EN = 11;          // TIM1 Timer clock enable
    static const uint8_t SPI1EN = 12;          // SPI 1 clock enable
    static const uint8_t USART1EN = 14;        // USART1 clock enable
    static const uint8_t TIM15EN = 16;         // TIM15 timer clock enable
    static const uint8_t TIM16EN = 17;         // TIM16 timer clock enable
    static const uint8_t TIM17EN = 18;         // TIM17 timer clock enable
    static const uint8_t DBGMCUEN = 22;        // MCU debug module clock enable
    static const uint8_t USART8EN = 7;         // USART8 clock enable
    static const uint8_t USART7EN = 6;         // USART7 clock enable
    static const uint8_t USART6EN = 5;         // USART6 clock enable
}

namespace APB1ENR // APB1 peripheral clock enable register (RCC_APB1ENR) fields
{
    static const uint8_t TIM2EN = 0;           // Timer 2 clock enable
    static const uint8_t TIM3EN = 1;           // Timer 3 clock enable
    static const uint8_t TIM6EN = 4;           // Timer 6 clock enable
    static const uint8_t TIM7EN = 5;           // TIM7 timer clock enable
    static const uint8_t TIM14EN = 8;          // Timer 14 clock enable
    static const uint8_t WWDGEN = 11;          // Window watchdog clock enable
    static const uint8_t SPI2EN = 14;          // SPI 2 clock enable
    static const uint8_t USART2EN = 17;        // USART 2 clock enable
    static const uint8_t USART3EN = 18;        // USART3 clock enable
    static const uint8_t USART4EN = 19;        // USART4 clock enable
    static const uint8_t USART5EN = 20;        // USART5 clock enable
    static const uint8_t I2C1EN = 21;          // I2C 1 clock enable
    static const uint8_t I2C2EN = 22;          // I2C 2 clock enable
    static const uint8_t USBRST = 23;          // USB interface clock enable
    static const uint8_t CANEN = 25;           // CAN interface clock enable
    static const uint8_t CRSEN = 27;           // Clock Recovery System interface clock enable
    static const uint8_t PWREN = 28;           // Power interface clock enable
    static const uint8_t DACEN = 29;           // DAC interface clock enable
    static const uint8_t CECEN = 30;           // HDMI CEC interface clock enable
}

namespace BDCR // Backup domain control register (RCC_BDCR) fields
{
    static const uint8_t LSEON = 0;            // External Low Speed oscillator enable, Read-write
    static const uint8_t LSERDY = 1;           // External Low Speed oscillator ready, Read-only
    static const uint8_t LSEBYP = 2;           // External Low Speed oscillator bypass, Read-write
    static const uint8_t LSEDRV = 3;           // LSE oscillator drive capability (2 bits), Read-write
    static const uint8_t RTCSEL = 8;           // RTC clock source selection (2 bits), Read-write
    static const uint8_t RTCEN = 15;           // RTC clock enable, Read-write
    static const uint8_t BDRST = 16;           // Backup domain software reset, Read-write
}

namespace CSR // Control/status register (RCC_CSR) fields
{
    static const uint8_t LSION = 0;            // Internal low speed oscillator enable, Read-write
    static const uint8_t LSIRDY = 1;           // Internal low speed oscillator ready, Read-only
    static const uint8_t RMVF = 24;            // Remove reset flag, Read-write
    static const uint8_t OBLRSTF = 25;         // Option byte loader reset flag, Read-write
    static const uint8_t PINRSTF = 26;         // PIN reset flag, Read-write
    static const uint8_t PORRSTF = 27;         // POR/PDR reset flag, Read-write
    static const uint8_t SFTRSTF = 28;         // Software reset flag, Read-write
    static const uint8_t IWDGRSTF = 29;        // Independent watchdog reset flag, Read-write
    static const uint8_t WWDGRSTF = 30;        // Window watchdog reset flag, Read-write
    static const uint8_t LPWRRSTF = 31;        // Low-power reset flag, Read-write
}

namespace AHBRSTR // AHB peripheral reset register fields
{
    static const uint8_t IOPARST = 17;         // I/O port A reset
    static const uint8_t IOPBRST = 18;         // I/O port B reset
    static const uint8_t IOPCRST = 19;         // I/O port C reset
    static const uint8_t IOPDRST = 20;         // I/O port D reset
    static const uint8_t IOPFRST = 22;         // I/O port F reset
    static const uint8_t TSCRST = 24;          // Touch sensing controller reset
}

namespace CFGR2 // Clock configuration register 2 fields
{
    static const uint8_t PREDIV = 0;           // PREDIV division factor (4 bits)
}

namespace CFGR3 // Clock configuration register 3 fields
{
    static const uint8_t USART1SW = 0;         // USART1 clock source selection (2 bits)
    static const uint8_t I2C1SW = 4;           // I2C1 clock source selection
    static const uint8_t CECSW = 6;            // HDMI CEC clock source selection
    static const uint8_t USBSW = 7;            // USB clock source selection
    static const uint8_t ADCSW = 8;            // ADC clock source selection
    static const uint8_t USART2SW = 16;        // USART2 clock source selection (2 bits)
}

namespace CR2 // Clock control register 2 fields
{
    static const uint8_t HSI14ON = 0;          // HSI14 clock enable, Read-write
    static const uint8_t HSI14RDY = 1;         // HR14 clock ready flag, Read-only
    static const uint8_t HSI14DIS = 2;         // HSI14 clock request from ADC disable, Read-write
    static const uint8_t HSI14TRIM = 3;        // HSI14 clock trimming (5 bits), Read-write
    static const uint8_t HSI14CAL = 8;         // HSI14 clock calibration (8 bits), Read-only
    static const uint8_t HSI48ON = 16;         // HSI48 clock enable, Read-write
    static const uint8_t HSI48RDY = 17;        // HSI48 clock ready flag, Read-only
    static const uint8_t HSI48CAL = 24;        // HSI48 factory clock calibration, Read-only
}

}

////
//
//    System configuration controller
//
////

namespace syscfg_comp
{

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
};

syscfg_comp_t& SYSCFG_COMP = *reinterpret_cast<syscfg_comp_t*>(0x40010000);

namespace SYSCFG_CFGR1 // configuration register 1 fields
{
    static const uint8_t MEM_MODE = 0;         // Memory mapping selection bits (2 bits)
    static const uint8_t ADC_DMA_RMP = 8;      // ADC DMA remapping bit
    static const uint8_t USART1_TX_DMA_RMP = 9;// USART1_TX DMA remapping bit
    static const uint8_t USART1_RX_DMA_RMP = 10;// USART1_RX DMA request remapping bit
    static const uint8_t TIM16_DMA_RMP = 11;   // TIM16 DMA request remapping bit
    static const uint8_t TIM17_DMA_RMP = 12;   // TIM17 DMA request remapping bit
    static const uint8_t I2C_PB6_FM = 16;      // Fast Mode Plus (FM plus) driving capability activation bits.
    static const uint8_t I2C_PB7_FM = 17;      // Fast Mode Plus (FM+) driving capability activation bits.
    static const uint8_t I2C_PB8_FM = 18;      // Fast Mode Plus (FM+) driving capability activation bits.
    static const uint8_t I2C_PB9_FM = 19;      // Fast Mode Plus (FM+) driving capability activation bits.
    static const uint8_t I2C1_FM_plus = 20;    // FM+ driving capability activation for I2C1
    static const uint8_t I2C2_FM_plus = 21;    // FM+ driving capability activation for I2C2
    static const uint8_t SPI2_DMA_RMP = 24;    // SPI2 DMA request remapping bit
    static const uint8_t USART2_DMA_RMP = 25;  // USART2 DMA request remapping bit
    static const uint8_t USART3_DMA_RMP = 26;  // USART3 DMA request remapping bit
    static const uint8_t I2C1_DMA_RMP = 27;    // I2C1 DMA request remapping bit
    static const uint8_t TIM1_DMA_RMP = 28;    // TIM1 DMA request remapping bit
    static const uint8_t TIM2_DMA_RMP = 29;    // TIM2 DMA request remapping bit
    static const uint8_t TIM3_DMA_RMP = 30;    // TIM3 DMA request remapping bit
}

namespace SYSCFG_EXTICR1 // external interrupt configuration register 1 fields
{
    static const uint8_t EXTI3 = 12;           // EXTI 3 configuration bits (4 bits)
    static const uint8_t EXTI2 = 8;            // EXTI 2 configuration bits (4 bits)
    static const uint8_t EXTI1 = 4;            // EXTI 1 configuration bits (4 bits)
    static const uint8_t EXTI0 = 0;            // EXTI 0 configuration bits (4 bits)
}

namespace SYSCFG_EXTICR2 // external interrupt configuration register 2 fields
{
    static const uint8_t EXTI7 = 12;           // EXTI 7 configuration bits (4 bits)
    static const uint8_t EXTI6 = 8;            // EXTI 6 configuration bits (4 bits)
    static const uint8_t EXTI5 = 4;            // EXTI 5 configuration bits (4 bits)
    static const uint8_t EXTI4 = 0;            // EXTI 4 configuration bits (4 bits)
}

namespace SYSCFG_EXTICR3 // external interrupt configuration register 3 fields
{
    static const uint8_t EXTI11 = 12;          // EXTI 11 configuration bits (4 bits)
    static const uint8_t EXTI10 = 8;           // EXTI 10 configuration bits (4 bits)
    static const uint8_t EXTI9 = 4;            // EXTI 9 configuration bits (4 bits)
    static const uint8_t EXTI8 = 0;            // EXTI 8 configuration bits (4 bits)
}

namespace SYSCFG_EXTICR4 // external interrupt configuration register 4 fields
{
    static const uint8_t EXTI15 = 12;          // EXTI 15 configuration bits (4 bits)
    static const uint8_t EXTI14 = 8;           // EXTI 14 configuration bits (4 bits)
    static const uint8_t EXTI13 = 4;           // EXTI 13 configuration bits (4 bits)
    static const uint8_t EXTI12 = 0;           // EXTI 12 configuration bits (4 bits)
}

namespace SYSCFG_CFGR2 // configuration register 2 fields
{
    static const uint8_t SRAM_PEF = 8;         // SRAM parity flag
    static const uint8_t PVD_LOCK = 2;         // PVD lock enable bit
    static const uint8_t SRAM_PARITY_LOCK = 1; // SRAM parity lock bit
    static const uint8_t LOCUP_LOCK = 0;       // Cortex-M0 LOCKUP bit enable bit
}

namespace COMP_CSR // control and status register fields
{
    static const uint8_t COMP1EN = 0;          // Comparator 1 enable, Read-write
    static const uint8_t COMP1_INP_DAC = 1;    // COMP1_INP_DAC, Read-write
    static const uint8_t COMP1MODE = 2;        // Comparator 1 mode (2 bits), Read-write
    static const uint8_t COMP1INSEL = 4;       // Comparator 1 inverting input selection (3 bits), Read-write
    static const uint8_t COMP1OUTSEL = 8;      // Comparator 1 output selection (3 bits), Read-write
    static const uint8_t COMP1POL = 11;        // Comparator 1 output polarity, Read-write
    static const uint8_t COMP1HYST = 12;       // Comparator 1 hysteresis (2 bits), Read-write
    static const uint8_t COMP1OUT = 14;        // Comparator 1 output, Read-only
    static const uint8_t COMP1LOCK = 15;       // Comparator 1 lock, Read-write
    static const uint8_t COMP2EN = 16;         // Comparator 2 enable, Read-write
    static const uint8_t COMP2MODE = 18;       // Comparator 2 mode (2 bits), Read-write
    static const uint8_t COMP2INSEL = 20;      // Comparator 2 inverting input selection (3 bits), Read-write
    static const uint8_t WNDWEN = 23;          // Window mode enable, Read-write
    static const uint8_t COMP2OUTSEL = 24;     // Comparator 2 output selection (3 bits), Read-write
    static const uint8_t COMP2POL = 27;        // Comparator 2 output polarity, Read-write
    static const uint8_t COMP2HYST = 28;       // Comparator 2 hysteresis (2 bits), Read-write
    static const uint8_t COMP2OUT = 30;        // Comparator 2 output, Read-only
    static const uint8_t COMP2LOCK = 31;       // Comparator 2 lock, Read-write
}

}

////
//
//    Analog-to-digital converter
//
////

namespace adc
{

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
};

adc_t& ADC = *reinterpret_cast<adc_t*>(0x40012400);

namespace ISR // interrupt and status register fields
{
    static const uint8_t AWD = 7;              // Analog watchdog flag
    static const uint8_t OVR = 4;              // ADC overrun
    static const uint8_t EOS = 3;              // End of sequence flag
    static const uint8_t EOC = 2;              // End of conversion flag
    static const uint8_t EOSMP = 1;            // End of sampling flag
    static const uint8_t ADRDY = 0;            // ADC ready
}

namespace IER // interrupt enable register fields
{
    static const uint8_t AWDIE = 7;            // Analog watchdog interrupt enable
    static const uint8_t OVRIE = 4;            // Overrun interrupt enable
    static const uint8_t EOSIE = 3;            // End of conversion sequence interrupt enable
    static const uint8_t EOCIE = 2;            // End of conversion interrupt enable
    static const uint8_t EOSMPIE = 1;          // End of sampling flag interrupt enable
    static const uint8_t ADRDYIE = 0;          // ADC ready interrupt enable
}

namespace CR // control register fields
{
    static const uint8_t ADCAL = 31;           // ADC calibration
    static const uint8_t ADSTP = 4;            // ADC stop conversion command
    static const uint8_t ADSTART = 2;          // ADC start conversion command
    static const uint8_t ADDIS = 1;            // ADC disable command
    static const uint8_t ADEN = 0;             // ADC enable command
}

namespace CFGR1 // configuration register 1 fields
{
    static const uint8_t AWDCH = 26;           // Analog watchdog channel selection (5 bits)
    static const uint8_t AWDEN = 23;           // Analog watchdog enable
    static const uint8_t AWDSGL = 22;          // Enable the watchdog on a single channel or on all channels
    static const uint8_t DISCEN = 16;          // Discontinuous mode
    static const uint8_t AUTOFF = 15;          // Auto-off mode
    static const uint8_t AUTDLY = 14;          // Auto-delayed conversion mode
    static const uint8_t CONT = 13;            // Single / continuous conversion mode
    static const uint8_t OVRMOD = 12;          // Overrun management mode
    static const uint8_t EXTEN = 10;           // External trigger enable and polarity selection (2 bits)
    static const uint8_t EXTSEL = 6;           // External trigger selection (3 bits)
    static const uint8_t ALIGN = 5;            // Data alignment
    static const uint8_t RES = 3;              // Data resolution (2 bits)
    static const uint8_t SCANDIR = 2;          // Scan sequence direction
    static const uint8_t DMACFG = 1;           // Direct memery access configuration
    static const uint8_t DMAEN = 0;            // Direct memory access enable
}

namespace CFGR2 // configuration register 2 fields
{
    static const uint8_t JITOFF_D4 = 31;       // JITOFF_D4
    static const uint8_t JITOFF_D2 = 30;       // JITOFF_D2
}

namespace SMPR // sampling time register fields
{
    static const uint8_t SMPR = 0;             // Sampling time selection (3 bits)
}

namespace TR // watchdog threshold register fields
{
    static const uint8_t HT = 16;              // Analog watchdog higher threshold (12 bits)
    static const uint8_t LT = 0;               // Analog watchdog lower threshold (12 bits)
}

namespace CHSELR // channel selection register fields
{
    static const uint8_t CHSEL18 = 18;         // Channel-x selection
    static const uint8_t CHSEL17 = 17;         // Channel-x selection
    static const uint8_t CHSEL16 = 16;         // Channel-x selection
    static const uint8_t CHSEL15 = 15;         // Channel-x selection
    static const uint8_t CHSEL14 = 14;         // Channel-x selection
    static const uint8_t CHSEL13 = 13;         // Channel-x selection
    static const uint8_t CHSEL12 = 12;         // Channel-x selection
    static const uint8_t CHSEL11 = 11;         // Channel-x selection
    static const uint8_t CHSEL10 = 10;         // Channel-x selection
    static const uint8_t CHSEL9 = 9;           // Channel-x selection
    static const uint8_t CHSEL8 = 8;           // Channel-x selection
    static const uint8_t CHSEL7 = 7;           // Channel-x selection
    static const uint8_t CHSEL6 = 6;           // Channel-x selection
    static const uint8_t CHSEL5 = 5;           // Channel-x selection
    static const uint8_t CHSEL4 = 4;           // Channel-x selection
    static const uint8_t CHSEL3 = 3;           // Channel-x selection
    static const uint8_t CHSEL2 = 2;           // Channel-x selection
    static const uint8_t CHSEL1 = 1;           // Channel-x selection
    static const uint8_t CHSEL0 = 0;           // Channel-x selection
}

namespace DR // data register fields
{
    static const uint8_t DATA = 0;             // Converted data (16 bits)
}

namespace CCR // common configuration register fields
{
    static const uint8_t VBATEN = 24;          // VBAT enable
    static const uint8_t TSEN = 23;            // Temperature sensor enable
    static const uint8_t VREFEN = 22;          // Temperature sensor and VREFINT enable
}

}

////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

namespace usart1
{

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
};

usart1_t& USART1 = *reinterpret_cast<usart1_t*>(0x40013800);

namespace CR1 // Control register 1 fields
{
    static const uint8_t UE = 0;               // USART enable
    static const uint8_t UESM = 1;             // USART enable in Stop mode
    static const uint8_t RE = 2;               // Receiver enable
    static const uint8_t TE = 3;               // Transmitter enable
    static const uint8_t IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t TXEIE = 7;            // interrupt enable
    static const uint8_t PEIE = 8;             // PE interrupt enable
    static const uint8_t PS = 9;               // Parity selection
    static const uint8_t PCE = 10;             // Parity control enable
    static const uint8_t WAKE = 11;            // Receiver wakeup method
    static const uint8_t M = 12;               // Word length
    static const uint8_t MME = 13;             // Mute mode enable
    static const uint8_t CMIE = 14;            // Character match interrupt enable
    static const uint8_t OVER8 = 15;           // Oversampling mode
    static const uint8_t DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t M1 = 28;              // Word length
}

namespace CR2 // Control register 2 fields
{
    static const uint8_t ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t RTOEN = 23;           // Receiver timeout enable
    static const uint8_t ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t ABREN = 20;           // Auto baud rate enable
    static const uint8_t MSBFIRST = 19;        // Most significant bit first
    static const uint8_t DATAINV = 18;         // Binary data inversion
    static const uint8_t TXINV = 17;           // TX pin active level inversion
    static const uint8_t RXINV = 16;           // RX pin active level inversion
    static const uint8_t SWAP = 15;            // Swap TX/RX pins
    static const uint8_t LINEN = 14;           // LIN mode enable
    static const uint8_t STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CLKEN = 11;           // Clock enable
    static const uint8_t CPOL = 10;            // Clock polarity
    static const uint8_t CPHA = 9;             // Clock phase
    static const uint8_t LBCL = 8;             // Last bit clock pulse
    static const uint8_t LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t LBDL = 5;             // LIN break detection length
    static const uint8_t ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
}

namespace CR3 // Control register 3 fields
{
    static const uint8_t WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t DEP = 15;             // Driver enable polarity selection
    static const uint8_t DEM = 14;             // Driver enable mode
    static const uint8_t DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t OVRDIS = 12;          // Overrun Disable
    static const uint8_t ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CTSE = 9;             // CTS enable
    static const uint8_t RTSE = 8;             // RTS enable
    static const uint8_t DMAT = 7;             // DMA enable transmitter
    static const uint8_t DMAR = 6;             // DMA enable receiver
    static const uint8_t SCEN = 5;             // Smartcard mode enable
    static const uint8_t NACK = 4;             // Smartcard NACK enable
    static const uint8_t HDSEL = 3;            // Half-duplex selection
    static const uint8_t IRLP = 2;             // IrDA low-power
    static const uint8_t IREN = 1;             // IrDA mode enable
    static const uint8_t EIE = 0;              // Error interrupt enable
}

namespace BRR // Baud rate register fields
{
    static const uint8_t DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
}

namespace GTPR // Guard time and prescaler register fields
{
    static const uint8_t GT = 8;               // Guard time value (8 bits)
    static const uint8_t PSC = 0;              // Prescaler value (8 bits)
}

namespace RTOR // Receiver timeout register fields
{
    static const uint8_t BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTO = 0;              // Receiver timeout value (24 bits)
}

namespace RQR // Request register fields
{
    static const uint8_t TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RXFRQ = 3;            // Receive data flush request
    static const uint8_t MMRQ = 2;             // Mute mode request
    static const uint8_t SBKRQ = 1;            // Send break request
    static const uint8_t ABRRQ = 0;            // Auto baud rate request
}

namespace ISR // Interrupt &amp; status register fields
{
    static const uint8_t REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t SBKF = 18;            // Send break flag
    static const uint8_t CMF = 17;             // character match flag
    static const uint8_t BUSY = 16;            // Busy flag
    static const uint8_t ABRF = 15;            // Auto baud rate flag
    static const uint8_t ABRE = 14;            // Auto baud rate error
    static const uint8_t EOBF = 12;            // End of block flag
    static const uint8_t RTOF = 11;            // Receiver timeout
    static const uint8_t CTS = 10;             // CTS flag
    static const uint8_t CTSIF = 9;            // CTS interrupt flag
    static const uint8_t LBDF = 8;             // LIN break detection flag
    static const uint8_t TXE = 7;              // Transmit data register empty
    static const uint8_t TC = 6;               // Transmission complete
    static const uint8_t RXNE = 5;             // Read data register not empty
    static const uint8_t IDLE = 4;             // Idle line detected
    static const uint8_t ORE = 3;              // Overrun error
    static const uint8_t NF = 2;               // Noise detected flag
    static const uint8_t FE = 1;               // Framing error
    static const uint8_t PE = 0;               // Parity error
}

namespace ICR // Interrupt flag clear register fields
{
    static const uint8_t WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t CMCF = 17;            // Character match clear flag
    static const uint8_t EOBCF = 12;           // End of timeout clear flag
    static const uint8_t RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t CTSCF = 9;            // CTS clear flag
    static const uint8_t LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t TCCF = 6;             // Transmission complete clear flag
    static const uint8_t IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ORECF = 3;            // Overrun error clear flag
    static const uint8_t NCF = 2;              // Noise detected clear flag
    static const uint8_t FECF = 1;             // Framing error clear flag
    static const uint8_t PECF = 0;             // Parity error clear flag
}

namespace RDR // Receive data register fields
{
    static const uint8_t RDR = 0;              // Receive data value (9 bits)
}

namespace TDR // Transmit data register fields
{
    static const uint8_t TDR = 0;              // Transmit data value (9 bits)
}

}

////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

namespace usart2
{

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
};

usart2_t& USART2 = *reinterpret_cast<usart2_t*>(0x40004400);

namespace CR1 // Control register 1 fields
{
    static const uint8_t UE = 0;               // USART enable
    static const uint8_t UESM = 1;             // USART enable in Stop mode
    static const uint8_t RE = 2;               // Receiver enable
    static const uint8_t TE = 3;               // Transmitter enable
    static const uint8_t IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t TXEIE = 7;            // interrupt enable
    static const uint8_t PEIE = 8;             // PE interrupt enable
    static const uint8_t PS = 9;               // Parity selection
    static const uint8_t PCE = 10;             // Parity control enable
    static const uint8_t WAKE = 11;            // Receiver wakeup method
    static const uint8_t M = 12;               // Word length
    static const uint8_t MME = 13;             // Mute mode enable
    static const uint8_t CMIE = 14;            // Character match interrupt enable
    static const uint8_t OVER8 = 15;           // Oversampling mode
    static const uint8_t DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t M1 = 28;              // Word length
}

namespace CR2 // Control register 2 fields
{
    static const uint8_t ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t RTOEN = 23;           // Receiver timeout enable
    static const uint8_t ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t ABREN = 20;           // Auto baud rate enable
    static const uint8_t MSBFIRST = 19;        // Most significant bit first
    static const uint8_t DATAINV = 18;         // Binary data inversion
    static const uint8_t TXINV = 17;           // TX pin active level inversion
    static const uint8_t RXINV = 16;           // RX pin active level inversion
    static const uint8_t SWAP = 15;            // Swap TX/RX pins
    static const uint8_t LINEN = 14;           // LIN mode enable
    static const uint8_t STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CLKEN = 11;           // Clock enable
    static const uint8_t CPOL = 10;            // Clock polarity
    static const uint8_t CPHA = 9;             // Clock phase
    static const uint8_t LBCL = 8;             // Last bit clock pulse
    static const uint8_t LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t LBDL = 5;             // LIN break detection length
    static const uint8_t ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
}

namespace CR3 // Control register 3 fields
{
    static const uint8_t WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t DEP = 15;             // Driver enable polarity selection
    static const uint8_t DEM = 14;             // Driver enable mode
    static const uint8_t DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t OVRDIS = 12;          // Overrun Disable
    static const uint8_t ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CTSE = 9;             // CTS enable
    static const uint8_t RTSE = 8;             // RTS enable
    static const uint8_t DMAT = 7;             // DMA enable transmitter
    static const uint8_t DMAR = 6;             // DMA enable receiver
    static const uint8_t SCEN = 5;             // Smartcard mode enable
    static const uint8_t NACK = 4;             // Smartcard NACK enable
    static const uint8_t HDSEL = 3;            // Half-duplex selection
    static const uint8_t IRLP = 2;             // IrDA low-power
    static const uint8_t IREN = 1;             // IrDA mode enable
    static const uint8_t EIE = 0;              // Error interrupt enable
}

namespace BRR // Baud rate register fields
{
    static const uint8_t DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
}

namespace GTPR // Guard time and prescaler register fields
{
    static const uint8_t GT = 8;               // Guard time value (8 bits)
    static const uint8_t PSC = 0;              // Prescaler value (8 bits)
}

namespace RTOR // Receiver timeout register fields
{
    static const uint8_t BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTO = 0;              // Receiver timeout value (24 bits)
}

namespace RQR // Request register fields
{
    static const uint8_t TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RXFRQ = 3;            // Receive data flush request
    static const uint8_t MMRQ = 2;             // Mute mode request
    static const uint8_t SBKRQ = 1;            // Send break request
    static const uint8_t ABRRQ = 0;            // Auto baud rate request
}

namespace ISR // Interrupt &amp; status register fields
{
    static const uint8_t REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t SBKF = 18;            // Send break flag
    static const uint8_t CMF = 17;             // character match flag
    static const uint8_t BUSY = 16;            // Busy flag
    static const uint8_t ABRF = 15;            // Auto baud rate flag
    static const uint8_t ABRE = 14;            // Auto baud rate error
    static const uint8_t EOBF = 12;            // End of block flag
    static const uint8_t RTOF = 11;            // Receiver timeout
    static const uint8_t CTS = 10;             // CTS flag
    static const uint8_t CTSIF = 9;            // CTS interrupt flag
    static const uint8_t LBDF = 8;             // LIN break detection flag
    static const uint8_t TXE = 7;              // Transmit data register empty
    static const uint8_t TC = 6;               // Transmission complete
    static const uint8_t RXNE = 5;             // Read data register not empty
    static const uint8_t IDLE = 4;             // Idle line detected
    static const uint8_t ORE = 3;              // Overrun error
    static const uint8_t NF = 2;               // Noise detected flag
    static const uint8_t FE = 1;               // Framing error
    static const uint8_t PE = 0;               // Parity error
}

namespace ICR // Interrupt flag clear register fields
{
    static const uint8_t WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t CMCF = 17;            // Character match clear flag
    static const uint8_t EOBCF = 12;           // End of timeout clear flag
    static const uint8_t RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t CTSCF = 9;            // CTS clear flag
    static const uint8_t LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t TCCF = 6;             // Transmission complete clear flag
    static const uint8_t IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ORECF = 3;            // Overrun error clear flag
    static const uint8_t NCF = 2;              // Noise detected clear flag
    static const uint8_t FECF = 1;             // Framing error clear flag
    static const uint8_t PECF = 0;             // Parity error clear flag
}

namespace RDR // Receive data register fields
{
    static const uint8_t RDR = 0;              // Receive data value (9 bits)
}

namespace TDR // Transmit data register fields
{
    static const uint8_t TDR = 0;              // Transmit data value (9 bits)
}

}

////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

namespace usart3
{

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
};

usart3_t& USART3 = *reinterpret_cast<usart3_t*>(0x40004800);

namespace CR1 // Control register 1 fields
{
    static const uint8_t UE = 0;               // USART enable
    static const uint8_t UESM = 1;             // USART enable in Stop mode
    static const uint8_t RE = 2;               // Receiver enable
    static const uint8_t TE = 3;               // Transmitter enable
    static const uint8_t IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t TXEIE = 7;            // interrupt enable
    static const uint8_t PEIE = 8;             // PE interrupt enable
    static const uint8_t PS = 9;               // Parity selection
    static const uint8_t PCE = 10;             // Parity control enable
    static const uint8_t WAKE = 11;            // Receiver wakeup method
    static const uint8_t M = 12;               // Word length
    static const uint8_t MME = 13;             // Mute mode enable
    static const uint8_t CMIE = 14;            // Character match interrupt enable
    static const uint8_t OVER8 = 15;           // Oversampling mode
    static const uint8_t DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t M1 = 28;              // Word length
}

namespace CR2 // Control register 2 fields
{
    static const uint8_t ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t RTOEN = 23;           // Receiver timeout enable
    static const uint8_t ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t ABREN = 20;           // Auto baud rate enable
    static const uint8_t MSBFIRST = 19;        // Most significant bit first
    static const uint8_t DATAINV = 18;         // Binary data inversion
    static const uint8_t TXINV = 17;           // TX pin active level inversion
    static const uint8_t RXINV = 16;           // RX pin active level inversion
    static const uint8_t SWAP = 15;            // Swap TX/RX pins
    static const uint8_t LINEN = 14;           // LIN mode enable
    static const uint8_t STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CLKEN = 11;           // Clock enable
    static const uint8_t CPOL = 10;            // Clock polarity
    static const uint8_t CPHA = 9;             // Clock phase
    static const uint8_t LBCL = 8;             // Last bit clock pulse
    static const uint8_t LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t LBDL = 5;             // LIN break detection length
    static const uint8_t ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
}

namespace CR3 // Control register 3 fields
{
    static const uint8_t WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t DEP = 15;             // Driver enable polarity selection
    static const uint8_t DEM = 14;             // Driver enable mode
    static const uint8_t DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t OVRDIS = 12;          // Overrun Disable
    static const uint8_t ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CTSE = 9;             // CTS enable
    static const uint8_t RTSE = 8;             // RTS enable
    static const uint8_t DMAT = 7;             // DMA enable transmitter
    static const uint8_t DMAR = 6;             // DMA enable receiver
    static const uint8_t SCEN = 5;             // Smartcard mode enable
    static const uint8_t NACK = 4;             // Smartcard NACK enable
    static const uint8_t HDSEL = 3;            // Half-duplex selection
    static const uint8_t IRLP = 2;             // IrDA low-power
    static const uint8_t IREN = 1;             // IrDA mode enable
    static const uint8_t EIE = 0;              // Error interrupt enable
}

namespace BRR // Baud rate register fields
{
    static const uint8_t DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
}

namespace GTPR // Guard time and prescaler register fields
{
    static const uint8_t GT = 8;               // Guard time value (8 bits)
    static const uint8_t PSC = 0;              // Prescaler value (8 bits)
}

namespace RTOR // Receiver timeout register fields
{
    static const uint8_t BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTO = 0;              // Receiver timeout value (24 bits)
}

namespace RQR // Request register fields
{
    static const uint8_t TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RXFRQ = 3;            // Receive data flush request
    static const uint8_t MMRQ = 2;             // Mute mode request
    static const uint8_t SBKRQ = 1;            // Send break request
    static const uint8_t ABRRQ = 0;            // Auto baud rate request
}

namespace ISR // Interrupt &amp; status register fields
{
    static const uint8_t REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t SBKF = 18;            // Send break flag
    static const uint8_t CMF = 17;             // character match flag
    static const uint8_t BUSY = 16;            // Busy flag
    static const uint8_t ABRF = 15;            // Auto baud rate flag
    static const uint8_t ABRE = 14;            // Auto baud rate error
    static const uint8_t EOBF = 12;            // End of block flag
    static const uint8_t RTOF = 11;            // Receiver timeout
    static const uint8_t CTS = 10;             // CTS flag
    static const uint8_t CTSIF = 9;            // CTS interrupt flag
    static const uint8_t LBDF = 8;             // LIN break detection flag
    static const uint8_t TXE = 7;              // Transmit data register empty
    static const uint8_t TC = 6;               // Transmission complete
    static const uint8_t RXNE = 5;             // Read data register not empty
    static const uint8_t IDLE = 4;             // Idle line detected
    static const uint8_t ORE = 3;              // Overrun error
    static const uint8_t NF = 2;               // Noise detected flag
    static const uint8_t FE = 1;               // Framing error
    static const uint8_t PE = 0;               // Parity error
}

namespace ICR // Interrupt flag clear register fields
{
    static const uint8_t WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t CMCF = 17;            // Character match clear flag
    static const uint8_t EOBCF = 12;           // End of timeout clear flag
    static const uint8_t RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t CTSCF = 9;            // CTS clear flag
    static const uint8_t LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t TCCF = 6;             // Transmission complete clear flag
    static const uint8_t IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ORECF = 3;            // Overrun error clear flag
    static const uint8_t NCF = 2;              // Noise detected clear flag
    static const uint8_t FECF = 1;             // Framing error clear flag
    static const uint8_t PECF = 0;             // Parity error clear flag
}

namespace RDR // Receive data register fields
{
    static const uint8_t RDR = 0;              // Receive data value (9 bits)
}

namespace TDR // Transmit data register fields
{
    static const uint8_t TDR = 0;              // Transmit data value (9 bits)
}

}

////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

namespace usart4
{

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
};

usart4_t& USART4 = *reinterpret_cast<usart4_t*>(0x40004c00);

namespace CR1 // Control register 1 fields
{
    static const uint8_t UE = 0;               // USART enable
    static const uint8_t UESM = 1;             // USART enable in Stop mode
    static const uint8_t RE = 2;               // Receiver enable
    static const uint8_t TE = 3;               // Transmitter enable
    static const uint8_t IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t TXEIE = 7;            // interrupt enable
    static const uint8_t PEIE = 8;             // PE interrupt enable
    static const uint8_t PS = 9;               // Parity selection
    static const uint8_t PCE = 10;             // Parity control enable
    static const uint8_t WAKE = 11;            // Receiver wakeup method
    static const uint8_t M = 12;               // Word length
    static const uint8_t MME = 13;             // Mute mode enable
    static const uint8_t CMIE = 14;            // Character match interrupt enable
    static const uint8_t OVER8 = 15;           // Oversampling mode
    static const uint8_t DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t M1 = 28;              // Word length
}

namespace CR2 // Control register 2 fields
{
    static const uint8_t ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t RTOEN = 23;           // Receiver timeout enable
    static const uint8_t ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t ABREN = 20;           // Auto baud rate enable
    static const uint8_t MSBFIRST = 19;        // Most significant bit first
    static const uint8_t DATAINV = 18;         // Binary data inversion
    static const uint8_t TXINV = 17;           // TX pin active level inversion
    static const uint8_t RXINV = 16;           // RX pin active level inversion
    static const uint8_t SWAP = 15;            // Swap TX/RX pins
    static const uint8_t LINEN = 14;           // LIN mode enable
    static const uint8_t STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CLKEN = 11;           // Clock enable
    static const uint8_t CPOL = 10;            // Clock polarity
    static const uint8_t CPHA = 9;             // Clock phase
    static const uint8_t LBCL = 8;             // Last bit clock pulse
    static const uint8_t LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t LBDL = 5;             // LIN break detection length
    static const uint8_t ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
}

namespace CR3 // Control register 3 fields
{
    static const uint8_t WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t DEP = 15;             // Driver enable polarity selection
    static const uint8_t DEM = 14;             // Driver enable mode
    static const uint8_t DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t OVRDIS = 12;          // Overrun Disable
    static const uint8_t ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CTSE = 9;             // CTS enable
    static const uint8_t RTSE = 8;             // RTS enable
    static const uint8_t DMAT = 7;             // DMA enable transmitter
    static const uint8_t DMAR = 6;             // DMA enable receiver
    static const uint8_t SCEN = 5;             // Smartcard mode enable
    static const uint8_t NACK = 4;             // Smartcard NACK enable
    static const uint8_t HDSEL = 3;            // Half-duplex selection
    static const uint8_t IRLP = 2;             // IrDA low-power
    static const uint8_t IREN = 1;             // IrDA mode enable
    static const uint8_t EIE = 0;              // Error interrupt enable
}

namespace BRR // Baud rate register fields
{
    static const uint8_t DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
}

namespace GTPR // Guard time and prescaler register fields
{
    static const uint8_t GT = 8;               // Guard time value (8 bits)
    static const uint8_t PSC = 0;              // Prescaler value (8 bits)
}

namespace RTOR // Receiver timeout register fields
{
    static const uint8_t BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTO = 0;              // Receiver timeout value (24 bits)
}

namespace RQR // Request register fields
{
    static const uint8_t TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RXFRQ = 3;            // Receive data flush request
    static const uint8_t MMRQ = 2;             // Mute mode request
    static const uint8_t SBKRQ = 1;            // Send break request
    static const uint8_t ABRRQ = 0;            // Auto baud rate request
}

namespace ISR // Interrupt &amp; status register fields
{
    static const uint8_t REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t SBKF = 18;            // Send break flag
    static const uint8_t CMF = 17;             // character match flag
    static const uint8_t BUSY = 16;            // Busy flag
    static const uint8_t ABRF = 15;            // Auto baud rate flag
    static const uint8_t ABRE = 14;            // Auto baud rate error
    static const uint8_t EOBF = 12;            // End of block flag
    static const uint8_t RTOF = 11;            // Receiver timeout
    static const uint8_t CTS = 10;             // CTS flag
    static const uint8_t CTSIF = 9;            // CTS interrupt flag
    static const uint8_t LBDF = 8;             // LIN break detection flag
    static const uint8_t TXE = 7;              // Transmit data register empty
    static const uint8_t TC = 6;               // Transmission complete
    static const uint8_t RXNE = 5;             // Read data register not empty
    static const uint8_t IDLE = 4;             // Idle line detected
    static const uint8_t ORE = 3;              // Overrun error
    static const uint8_t NF = 2;               // Noise detected flag
    static const uint8_t FE = 1;               // Framing error
    static const uint8_t PE = 0;               // Parity error
}

namespace ICR // Interrupt flag clear register fields
{
    static const uint8_t WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t CMCF = 17;            // Character match clear flag
    static const uint8_t EOBCF = 12;           // End of timeout clear flag
    static const uint8_t RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t CTSCF = 9;            // CTS clear flag
    static const uint8_t LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t TCCF = 6;             // Transmission complete clear flag
    static const uint8_t IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ORECF = 3;            // Overrun error clear flag
    static const uint8_t NCF = 2;              // Noise detected clear flag
    static const uint8_t FECF = 1;             // Framing error clear flag
    static const uint8_t PECF = 0;             // Parity error clear flag
}

namespace RDR // Receive data register fields
{
    static const uint8_t RDR = 0;              // Receive data value (9 bits)
}

namespace TDR // Transmit data register fields
{
    static const uint8_t TDR = 0;              // Transmit data value (9 bits)
}

}

////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

namespace usart6
{

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
};

usart6_t& USART6 = *reinterpret_cast<usart6_t*>(0x40011400);

namespace CR1 // Control register 1 fields
{
    static const uint8_t UE = 0;               // USART enable
    static const uint8_t UESM = 1;             // USART enable in Stop mode
    static const uint8_t RE = 2;               // Receiver enable
    static const uint8_t TE = 3;               // Transmitter enable
    static const uint8_t IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t TXEIE = 7;            // interrupt enable
    static const uint8_t PEIE = 8;             // PE interrupt enable
    static const uint8_t PS = 9;               // Parity selection
    static const uint8_t PCE = 10;             // Parity control enable
    static const uint8_t WAKE = 11;            // Receiver wakeup method
    static const uint8_t M = 12;               // Word length
    static const uint8_t MME = 13;             // Mute mode enable
    static const uint8_t CMIE = 14;            // Character match interrupt enable
    static const uint8_t OVER8 = 15;           // Oversampling mode
    static const uint8_t DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t M1 = 28;              // Word length
}

namespace CR2 // Control register 2 fields
{
    static const uint8_t ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t RTOEN = 23;           // Receiver timeout enable
    static const uint8_t ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t ABREN = 20;           // Auto baud rate enable
    static const uint8_t MSBFIRST = 19;        // Most significant bit first
    static const uint8_t DATAINV = 18;         // Binary data inversion
    static const uint8_t TXINV = 17;           // TX pin active level inversion
    static const uint8_t RXINV = 16;           // RX pin active level inversion
    static const uint8_t SWAP = 15;            // Swap TX/RX pins
    static const uint8_t LINEN = 14;           // LIN mode enable
    static const uint8_t STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CLKEN = 11;           // Clock enable
    static const uint8_t CPOL = 10;            // Clock polarity
    static const uint8_t CPHA = 9;             // Clock phase
    static const uint8_t LBCL = 8;             // Last bit clock pulse
    static const uint8_t LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t LBDL = 5;             // LIN break detection length
    static const uint8_t ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
}

namespace CR3 // Control register 3 fields
{
    static const uint8_t WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t DEP = 15;             // Driver enable polarity selection
    static const uint8_t DEM = 14;             // Driver enable mode
    static const uint8_t DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t OVRDIS = 12;          // Overrun Disable
    static const uint8_t ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CTSE = 9;             // CTS enable
    static const uint8_t RTSE = 8;             // RTS enable
    static const uint8_t DMAT = 7;             // DMA enable transmitter
    static const uint8_t DMAR = 6;             // DMA enable receiver
    static const uint8_t SCEN = 5;             // Smartcard mode enable
    static const uint8_t NACK = 4;             // Smartcard NACK enable
    static const uint8_t HDSEL = 3;            // Half-duplex selection
    static const uint8_t IRLP = 2;             // IrDA low-power
    static const uint8_t IREN = 1;             // IrDA mode enable
    static const uint8_t EIE = 0;              // Error interrupt enable
}

namespace BRR // Baud rate register fields
{
    static const uint8_t DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
}

namespace GTPR // Guard time and prescaler register fields
{
    static const uint8_t GT = 8;               // Guard time value (8 bits)
    static const uint8_t PSC = 0;              // Prescaler value (8 bits)
}

namespace RTOR // Receiver timeout register fields
{
    static const uint8_t BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTO = 0;              // Receiver timeout value (24 bits)
}

namespace RQR // Request register fields
{
    static const uint8_t TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RXFRQ = 3;            // Receive data flush request
    static const uint8_t MMRQ = 2;             // Mute mode request
    static const uint8_t SBKRQ = 1;            // Send break request
    static const uint8_t ABRRQ = 0;            // Auto baud rate request
}

namespace ISR // Interrupt &amp; status register fields
{
    static const uint8_t REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t SBKF = 18;            // Send break flag
    static const uint8_t CMF = 17;             // character match flag
    static const uint8_t BUSY = 16;            // Busy flag
    static const uint8_t ABRF = 15;            // Auto baud rate flag
    static const uint8_t ABRE = 14;            // Auto baud rate error
    static const uint8_t EOBF = 12;            // End of block flag
    static const uint8_t RTOF = 11;            // Receiver timeout
    static const uint8_t CTS = 10;             // CTS flag
    static const uint8_t CTSIF = 9;            // CTS interrupt flag
    static const uint8_t LBDF = 8;             // LIN break detection flag
    static const uint8_t TXE = 7;              // Transmit data register empty
    static const uint8_t TC = 6;               // Transmission complete
    static const uint8_t RXNE = 5;             // Read data register not empty
    static const uint8_t IDLE = 4;             // Idle line detected
    static const uint8_t ORE = 3;              // Overrun error
    static const uint8_t NF = 2;               // Noise detected flag
    static const uint8_t FE = 1;               // Framing error
    static const uint8_t PE = 0;               // Parity error
}

namespace ICR // Interrupt flag clear register fields
{
    static const uint8_t WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t CMCF = 17;            // Character match clear flag
    static const uint8_t EOBCF = 12;           // End of timeout clear flag
    static const uint8_t RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t CTSCF = 9;            // CTS clear flag
    static const uint8_t LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t TCCF = 6;             // Transmission complete clear flag
    static const uint8_t IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ORECF = 3;            // Overrun error clear flag
    static const uint8_t NCF = 2;              // Noise detected clear flag
    static const uint8_t FECF = 1;             // Framing error clear flag
    static const uint8_t PECF = 0;             // Parity error clear flag
}

namespace RDR // Receive data register fields
{
    static const uint8_t RDR = 0;              // Receive data value (9 bits)
}

namespace TDR // Transmit data register fields
{
    static const uint8_t TDR = 0;              // Transmit data value (9 bits)
}

}

////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

namespace usart7
{

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
};

usart7_t& USART7 = *reinterpret_cast<usart7_t*>(0x40011800);

namespace CR1 // Control register 1 fields
{
    static const uint8_t UE = 0;               // USART enable
    static const uint8_t UESM = 1;             // USART enable in Stop mode
    static const uint8_t RE = 2;               // Receiver enable
    static const uint8_t TE = 3;               // Transmitter enable
    static const uint8_t IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t TXEIE = 7;            // interrupt enable
    static const uint8_t PEIE = 8;             // PE interrupt enable
    static const uint8_t PS = 9;               // Parity selection
    static const uint8_t PCE = 10;             // Parity control enable
    static const uint8_t WAKE = 11;            // Receiver wakeup method
    static const uint8_t M = 12;               // Word length
    static const uint8_t MME = 13;             // Mute mode enable
    static const uint8_t CMIE = 14;            // Character match interrupt enable
    static const uint8_t OVER8 = 15;           // Oversampling mode
    static const uint8_t DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t M1 = 28;              // Word length
}

namespace CR2 // Control register 2 fields
{
    static const uint8_t ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t RTOEN = 23;           // Receiver timeout enable
    static const uint8_t ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t ABREN = 20;           // Auto baud rate enable
    static const uint8_t MSBFIRST = 19;        // Most significant bit first
    static const uint8_t DATAINV = 18;         // Binary data inversion
    static const uint8_t TXINV = 17;           // TX pin active level inversion
    static const uint8_t RXINV = 16;           // RX pin active level inversion
    static const uint8_t SWAP = 15;            // Swap TX/RX pins
    static const uint8_t LINEN = 14;           // LIN mode enable
    static const uint8_t STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CLKEN = 11;           // Clock enable
    static const uint8_t CPOL = 10;            // Clock polarity
    static const uint8_t CPHA = 9;             // Clock phase
    static const uint8_t LBCL = 8;             // Last bit clock pulse
    static const uint8_t LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t LBDL = 5;             // LIN break detection length
    static const uint8_t ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
}

namespace CR3 // Control register 3 fields
{
    static const uint8_t WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t DEP = 15;             // Driver enable polarity selection
    static const uint8_t DEM = 14;             // Driver enable mode
    static const uint8_t DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t OVRDIS = 12;          // Overrun Disable
    static const uint8_t ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CTSE = 9;             // CTS enable
    static const uint8_t RTSE = 8;             // RTS enable
    static const uint8_t DMAT = 7;             // DMA enable transmitter
    static const uint8_t DMAR = 6;             // DMA enable receiver
    static const uint8_t SCEN = 5;             // Smartcard mode enable
    static const uint8_t NACK = 4;             // Smartcard NACK enable
    static const uint8_t HDSEL = 3;            // Half-duplex selection
    static const uint8_t IRLP = 2;             // IrDA low-power
    static const uint8_t IREN = 1;             // IrDA mode enable
    static const uint8_t EIE = 0;              // Error interrupt enable
}

namespace BRR // Baud rate register fields
{
    static const uint8_t DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
}

namespace GTPR // Guard time and prescaler register fields
{
    static const uint8_t GT = 8;               // Guard time value (8 bits)
    static const uint8_t PSC = 0;              // Prescaler value (8 bits)
}

namespace RTOR // Receiver timeout register fields
{
    static const uint8_t BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTO = 0;              // Receiver timeout value (24 bits)
}

namespace RQR // Request register fields
{
    static const uint8_t TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RXFRQ = 3;            // Receive data flush request
    static const uint8_t MMRQ = 2;             // Mute mode request
    static const uint8_t SBKRQ = 1;            // Send break request
    static const uint8_t ABRRQ = 0;            // Auto baud rate request
}

namespace ISR // Interrupt &amp; status register fields
{
    static const uint8_t REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t SBKF = 18;            // Send break flag
    static const uint8_t CMF = 17;             // character match flag
    static const uint8_t BUSY = 16;            // Busy flag
    static const uint8_t ABRF = 15;            // Auto baud rate flag
    static const uint8_t ABRE = 14;            // Auto baud rate error
    static const uint8_t EOBF = 12;            // End of block flag
    static const uint8_t RTOF = 11;            // Receiver timeout
    static const uint8_t CTS = 10;             // CTS flag
    static const uint8_t CTSIF = 9;            // CTS interrupt flag
    static const uint8_t LBDF = 8;             // LIN break detection flag
    static const uint8_t TXE = 7;              // Transmit data register empty
    static const uint8_t TC = 6;               // Transmission complete
    static const uint8_t RXNE = 5;             // Read data register not empty
    static const uint8_t IDLE = 4;             // Idle line detected
    static const uint8_t ORE = 3;              // Overrun error
    static const uint8_t NF = 2;               // Noise detected flag
    static const uint8_t FE = 1;               // Framing error
    static const uint8_t PE = 0;               // Parity error
}

namespace ICR // Interrupt flag clear register fields
{
    static const uint8_t WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t CMCF = 17;            // Character match clear flag
    static const uint8_t EOBCF = 12;           // End of timeout clear flag
    static const uint8_t RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t CTSCF = 9;            // CTS clear flag
    static const uint8_t LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t TCCF = 6;             // Transmission complete clear flag
    static const uint8_t IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ORECF = 3;            // Overrun error clear flag
    static const uint8_t NCF = 2;              // Noise detected clear flag
    static const uint8_t FECF = 1;             // Framing error clear flag
    static const uint8_t PECF = 0;             // Parity error clear flag
}

namespace RDR // Receive data register fields
{
    static const uint8_t RDR = 0;              // Receive data value (9 bits)
}

namespace TDR // Transmit data register fields
{
    static const uint8_t TDR = 0;              // Transmit data value (9 bits)
}

}

////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

namespace usart8
{

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
};

usart8_t& USART8 = *reinterpret_cast<usart8_t*>(0x40011c00);

namespace CR1 // Control register 1 fields
{
    static const uint8_t UE = 0;               // USART enable
    static const uint8_t UESM = 1;             // USART enable in Stop mode
    static const uint8_t RE = 2;               // Receiver enable
    static const uint8_t TE = 3;               // Transmitter enable
    static const uint8_t IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t TXEIE = 7;            // interrupt enable
    static const uint8_t PEIE = 8;             // PE interrupt enable
    static const uint8_t PS = 9;               // Parity selection
    static const uint8_t PCE = 10;             // Parity control enable
    static const uint8_t WAKE = 11;            // Receiver wakeup method
    static const uint8_t M = 12;               // Word length
    static const uint8_t MME = 13;             // Mute mode enable
    static const uint8_t CMIE = 14;            // Character match interrupt enable
    static const uint8_t OVER8 = 15;           // Oversampling mode
    static const uint8_t DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t M1 = 28;              // Word length
}

namespace CR2 // Control register 2 fields
{
    static const uint8_t ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t RTOEN = 23;           // Receiver timeout enable
    static const uint8_t ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t ABREN = 20;           // Auto baud rate enable
    static const uint8_t MSBFIRST = 19;        // Most significant bit first
    static const uint8_t DATAINV = 18;         // Binary data inversion
    static const uint8_t TXINV = 17;           // TX pin active level inversion
    static const uint8_t RXINV = 16;           // RX pin active level inversion
    static const uint8_t SWAP = 15;            // Swap TX/RX pins
    static const uint8_t LINEN = 14;           // LIN mode enable
    static const uint8_t STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CLKEN = 11;           // Clock enable
    static const uint8_t CPOL = 10;            // Clock polarity
    static const uint8_t CPHA = 9;             // Clock phase
    static const uint8_t LBCL = 8;             // Last bit clock pulse
    static const uint8_t LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t LBDL = 5;             // LIN break detection length
    static const uint8_t ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
}

namespace CR3 // Control register 3 fields
{
    static const uint8_t WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t DEP = 15;             // Driver enable polarity selection
    static const uint8_t DEM = 14;             // Driver enable mode
    static const uint8_t DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t OVRDIS = 12;          // Overrun Disable
    static const uint8_t ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CTSE = 9;             // CTS enable
    static const uint8_t RTSE = 8;             // RTS enable
    static const uint8_t DMAT = 7;             // DMA enable transmitter
    static const uint8_t DMAR = 6;             // DMA enable receiver
    static const uint8_t SCEN = 5;             // Smartcard mode enable
    static const uint8_t NACK = 4;             // Smartcard NACK enable
    static const uint8_t HDSEL = 3;            // Half-duplex selection
    static const uint8_t IRLP = 2;             // IrDA low-power
    static const uint8_t IREN = 1;             // IrDA mode enable
    static const uint8_t EIE = 0;              // Error interrupt enable
}

namespace BRR // Baud rate register fields
{
    static const uint8_t DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
}

namespace GTPR // Guard time and prescaler register fields
{
    static const uint8_t GT = 8;               // Guard time value (8 bits)
    static const uint8_t PSC = 0;              // Prescaler value (8 bits)
}

namespace RTOR // Receiver timeout register fields
{
    static const uint8_t BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTO = 0;              // Receiver timeout value (24 bits)
}

namespace RQR // Request register fields
{
    static const uint8_t TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RXFRQ = 3;            // Receive data flush request
    static const uint8_t MMRQ = 2;             // Mute mode request
    static const uint8_t SBKRQ = 1;            // Send break request
    static const uint8_t ABRRQ = 0;            // Auto baud rate request
}

namespace ISR // Interrupt &amp; status register fields
{
    static const uint8_t REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t SBKF = 18;            // Send break flag
    static const uint8_t CMF = 17;             // character match flag
    static const uint8_t BUSY = 16;            // Busy flag
    static const uint8_t ABRF = 15;            // Auto baud rate flag
    static const uint8_t ABRE = 14;            // Auto baud rate error
    static const uint8_t EOBF = 12;            // End of block flag
    static const uint8_t RTOF = 11;            // Receiver timeout
    static const uint8_t CTS = 10;             // CTS flag
    static const uint8_t CTSIF = 9;            // CTS interrupt flag
    static const uint8_t LBDF = 8;             // LIN break detection flag
    static const uint8_t TXE = 7;              // Transmit data register empty
    static const uint8_t TC = 6;               // Transmission complete
    static const uint8_t RXNE = 5;             // Read data register not empty
    static const uint8_t IDLE = 4;             // Idle line detected
    static const uint8_t ORE = 3;              // Overrun error
    static const uint8_t NF = 2;               // Noise detected flag
    static const uint8_t FE = 1;               // Framing error
    static const uint8_t PE = 0;               // Parity error
}

namespace ICR // Interrupt flag clear register fields
{
    static const uint8_t WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t CMCF = 17;            // Character match clear flag
    static const uint8_t EOBCF = 12;           // End of timeout clear flag
    static const uint8_t RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t CTSCF = 9;            // CTS clear flag
    static const uint8_t LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t TCCF = 6;             // Transmission complete clear flag
    static const uint8_t IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ORECF = 3;            // Overrun error clear flag
    static const uint8_t NCF = 2;              // Noise detected clear flag
    static const uint8_t FECF = 1;             // Framing error clear flag
    static const uint8_t PECF = 0;             // Parity error clear flag
}

namespace RDR // Receive data register fields
{
    static const uint8_t RDR = 0;              // Receive data value (9 bits)
}

namespace TDR // Transmit data register fields
{
    static const uint8_t TDR = 0;              // Transmit data value (9 bits)
}

}

////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

namespace usart5
{

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
};

usart5_t& USART5 = *reinterpret_cast<usart5_t*>(0x40005000);

namespace CR1 // Control register 1 fields
{
    static const uint8_t UE = 0;               // USART enable
    static const uint8_t UESM = 1;             // USART enable in Stop mode
    static const uint8_t RE = 2;               // Receiver enable
    static const uint8_t TE = 3;               // Transmitter enable
    static const uint8_t IDLEIE = 4;           // IDLE interrupt enable
    static const uint8_t RXNEIE = 5;           // RXNE interrupt enable
    static const uint8_t TCIE = 6;             // Transmission complete interrupt enable
    static const uint8_t TXEIE = 7;            // interrupt enable
    static const uint8_t PEIE = 8;             // PE interrupt enable
    static const uint8_t PS = 9;               // Parity selection
    static const uint8_t PCE = 10;             // Parity control enable
    static const uint8_t WAKE = 11;            // Receiver wakeup method
    static const uint8_t M = 12;               // Word length
    static const uint8_t MME = 13;             // Mute mode enable
    static const uint8_t CMIE = 14;            // Character match interrupt enable
    static const uint8_t OVER8 = 15;           // Oversampling mode
    static const uint8_t DEDT = 16;            // Driver Enable deassertion time (5 bits)
    static const uint8_t DEAT = 21;            // Driver Enable assertion time (5 bits)
    static const uint8_t RTOIE = 26;           // Receiver timeout interrupt enable
    static const uint8_t EOBIE = 27;           // End of Block interrupt enable
    static const uint8_t M1 = 28;              // Word length
}

namespace CR2 // Control register 2 fields
{
    static const uint8_t ADD4 = 28;            // Address of the USART node (4 bits)
    static const uint8_t ADD0 = 24;            // Address of the USART node (4 bits)
    static const uint8_t RTOEN = 23;           // Receiver timeout enable
    static const uint8_t ABRMOD = 21;          // Auto baud rate mode (2 bits)
    static const uint8_t ABREN = 20;           // Auto baud rate enable
    static const uint8_t MSBFIRST = 19;        // Most significant bit first
    static const uint8_t DATAINV = 18;         // Binary data inversion
    static const uint8_t TXINV = 17;           // TX pin active level inversion
    static const uint8_t RXINV = 16;           // RX pin active level inversion
    static const uint8_t SWAP = 15;            // Swap TX/RX pins
    static const uint8_t LINEN = 14;           // LIN mode enable
    static const uint8_t STOP = 12;            // STOP bits (2 bits)
    static const uint8_t CLKEN = 11;           // Clock enable
    static const uint8_t CPOL = 10;            // Clock polarity
    static const uint8_t CPHA = 9;             // Clock phase
    static const uint8_t LBCL = 8;             // Last bit clock pulse
    static const uint8_t LBDIE = 6;            // LIN break detection interrupt enable
    static const uint8_t LBDL = 5;             // LIN break detection length
    static const uint8_t ADDM7 = 4;            // 7-bit Address Detection/4-bit Address Detection
}

namespace CR3 // Control register 3 fields
{
    static const uint8_t WUFIE = 22;           // Wakeup from Stop mode interrupt enable
    static const uint8_t WUS = 20;             // Wakeup from Stop mode interrupt flag selection (2 bits)
    static const uint8_t SCARCNT = 17;         // Smartcard auto-retry count (3 bits)
    static const uint8_t DEP = 15;             // Driver enable polarity selection
    static const uint8_t DEM = 14;             // Driver enable mode
    static const uint8_t DDRE = 13;            // DMA Disable on Reception Error
    static const uint8_t OVRDIS = 12;          // Overrun Disable
    static const uint8_t ONEBIT = 11;          // One sample bit method enable
    static const uint8_t CTSIE = 10;           // CTS interrupt enable
    static const uint8_t CTSE = 9;             // CTS enable
    static const uint8_t RTSE = 8;             // RTS enable
    static const uint8_t DMAT = 7;             // DMA enable transmitter
    static const uint8_t DMAR = 6;             // DMA enable receiver
    static const uint8_t SCEN = 5;             // Smartcard mode enable
    static const uint8_t NACK = 4;             // Smartcard NACK enable
    static const uint8_t HDSEL = 3;            // Half-duplex selection
    static const uint8_t IRLP = 2;             // IrDA low-power
    static const uint8_t IREN = 1;             // IrDA mode enable
    static const uint8_t EIE = 0;              // Error interrupt enable
}

namespace BRR // Baud rate register fields
{
    static const uint8_t DIV_Mantissa = 4;     // mantissa of USARTDIV (12 bits)
    static const uint8_t DIV_Fraction = 0;     // fraction of USARTDIV (4 bits)
}

namespace GTPR // Guard time and prescaler register fields
{
    static const uint8_t GT = 8;               // Guard time value (8 bits)
    static const uint8_t PSC = 0;              // Prescaler value (8 bits)
}

namespace RTOR // Receiver timeout register fields
{
    static const uint8_t BLEN = 24;            // Block Length (8 bits)
    static const uint8_t RTO = 0;              // Receiver timeout value (24 bits)
}

namespace RQR // Request register fields
{
    static const uint8_t TXFRQ = 4;            // Transmit data flush request
    static const uint8_t RXFRQ = 3;            // Receive data flush request
    static const uint8_t MMRQ = 2;             // Mute mode request
    static const uint8_t SBKRQ = 1;            // Send break request
    static const uint8_t ABRRQ = 0;            // Auto baud rate request
}

namespace ISR // Interrupt &amp; status register fields
{
    static const uint8_t REACK = 22;           // Receive enable acknowledge flag
    static const uint8_t TEACK = 21;           // Transmit enable acknowledge flag
    static const uint8_t WUF = 20;             // Wakeup from Stop mode flag
    static const uint8_t RWU = 19;             // Receiver wakeup from Mute mode
    static const uint8_t SBKF = 18;            // Send break flag
    static const uint8_t CMF = 17;             // character match flag
    static const uint8_t BUSY = 16;            // Busy flag
    static const uint8_t ABRF = 15;            // Auto baud rate flag
    static const uint8_t ABRE = 14;            // Auto baud rate error
    static const uint8_t EOBF = 12;            // End of block flag
    static const uint8_t RTOF = 11;            // Receiver timeout
    static const uint8_t CTS = 10;             // CTS flag
    static const uint8_t CTSIF = 9;            // CTS interrupt flag
    static const uint8_t LBDF = 8;             // LIN break detection flag
    static const uint8_t TXE = 7;              // Transmit data register empty
    static const uint8_t TC = 6;               // Transmission complete
    static const uint8_t RXNE = 5;             // Read data register not empty
    static const uint8_t IDLE = 4;             // Idle line detected
    static const uint8_t ORE = 3;              // Overrun error
    static const uint8_t NF = 2;               // Noise detected flag
    static const uint8_t FE = 1;               // Framing error
    static const uint8_t PE = 0;               // Parity error
}

namespace ICR // Interrupt flag clear register fields
{
    static const uint8_t WUCF = 20;            // Wakeup from Stop mode clear flag
    static const uint8_t CMCF = 17;            // Character match clear flag
    static const uint8_t EOBCF = 12;           // End of timeout clear flag
    static const uint8_t RTOCF = 11;           // Receiver timeout clear flag
    static const uint8_t CTSCF = 9;            // CTS clear flag
    static const uint8_t LBDCF = 8;            // LIN break detection clear flag
    static const uint8_t TCCF = 6;             // Transmission complete clear flag
    static const uint8_t IDLECF = 4;           // Idle line detected clear flag
    static const uint8_t ORECF = 3;            // Overrun error clear flag
    static const uint8_t NCF = 2;              // Noise detected clear flag
    static const uint8_t FECF = 1;             // Framing error clear flag
    static const uint8_t PECF = 0;             // Parity error clear flag
}

namespace RDR // Receive data register fields
{
    static const uint8_t RDR = 0;              // Receive data value (9 bits)
}

namespace TDR // Transmit data register fields
{
    static const uint8_t TDR = 0;              // Transmit data value (9 bits)
}

}

////
//
//    Real-time clock
//
////

namespace rtc
{

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
};

rtc_t& RTC = *reinterpret_cast<rtc_t*>(0x40002800);

namespace TR // time register fields
{
    static const uint8_t PM = 22;              // AM/PM notation
    static const uint8_t HT = 20;              // Hour tens in BCD format (2 bits)
    static const uint8_t HU = 16;              // Hour units in BCD format (4 bits)
    static const uint8_t MNT = 12;             // Minute tens in BCD format (3 bits)
    static const uint8_t MNU = 8;              // Minute units in BCD format (4 bits)
    static const uint8_t ST = 4;               // Second tens in BCD format (3 bits)
    static const uint8_t SU = 0;               // Second units in BCD format (4 bits)
}

namespace DR // date register fields
{
    static const uint8_t YT = 20;              // Year tens in BCD format (4 bits)
    static const uint8_t YU = 16;              // Year units in BCD format (4 bits)
    static const uint8_t WDU = 13;             // Week day units (3 bits)
    static const uint8_t MT = 12;              // Month tens in BCD format
    static const uint8_t MU = 8;               // Month units in BCD format (4 bits)
    static const uint8_t DT = 4;               // Date tens in BCD format (2 bits)
    static const uint8_t DU = 0;               // Date units in BCD format (4 bits)
}

namespace CR // control register fields
{
    static const uint8_t TSEDGE = 3;           // Time-stamp event active edge, Read-write
    static const uint8_t REFCKON = 4;          // RTC_REFIN reference clock detection enable (50 or 60 Hz), Read-write
    static const uint8_t BYPSHAD = 5;          // Bypass the shadow registers, Read-write
    static const uint8_t FMT = 6;              // Hour format, Read-write
    static const uint8_t ALRAE = 8;            // Alarm A enable, Read-write
    static const uint8_t TSE = 11;             // timestamp enable, Read-write
    static const uint8_t ALRAIE = 12;          // Alarm A interrupt enable, Read-write
    static const uint8_t TSIE = 15;            // Time-stamp interrupt enable, Read-write
    static const uint8_t ADD1H = 16;           // Add 1 hour (summer time change), Write-only
    static const uint8_t SUB1H = 17;           // Subtract 1 hour (winter time change), Write-only
    static const uint8_t BKP = 18;             // Backup, Read-write
    static const uint8_t COSEL = 19;           // Calibration output selection, Read-write
    static const uint8_t POL = 20;             // Output polarity, Read-write
    static const uint8_t OSEL = 21;            // Output selection (2 bits), Read-write
    static const uint8_t COE = 23;             // Calibration output enable, Read-write
}

namespace ISR // initialization and status register fields
{
    static const uint8_t ALRAWF = 0;           // Alarm A write flag, Read-only
    static const uint8_t SHPF = 3;             // Shift operation pending, Read-write
    static const uint8_t INITS = 4;            // Initialization status flag, Read-only
    static const uint8_t RSF = 5;              // Registers synchronization flag, Read-write
    static const uint8_t INITF = 6;            // Initialization flag, Read-only
    static const uint8_t INIT = 7;             // Initialization mode, Read-write
    static const uint8_t ALRAF = 8;            // Alarm A flag, Read-write
    static const uint8_t TSF = 11;             // Time-stamp flag, Read-write
    static const uint8_t TSOVF = 12;           // Time-stamp overflow flag, Read-write
    static const uint8_t TAMP1F = 13;          // RTC_TAMP1 detection flag, Read-write
    static const uint8_t TAMP2F = 14;          // RTC_TAMP2 detection flag, Read-write
    static const uint8_t RECALPF = 16;         // Recalibration pending Flag, Read-only
}

namespace PRER // prescaler register fields
{
    static const uint8_t PREDIV_A = 16;        // Asynchronous prescaler factor (7 bits)
    static const uint8_t PREDIV_S = 0;         // Synchronous prescaler factor (15 bits)
}

namespace ALRMAR // alarm A register fields
{
    static const uint8_t MSK4 = 31;            // Alarm A date mask
    static const uint8_t WDSEL = 30;           // Week day selection
    static const uint8_t DT = 28;              // Date tens in BCD format. (2 bits)
    static const uint8_t DU = 24;              // Date units or day in BCD format. (4 bits)
    static const uint8_t MSK3 = 23;            // Alarm A hours mask
    static const uint8_t PM = 22;              // AM/PM notation
    static const uint8_t HT = 20;              // Hour tens in BCD format. (2 bits)
    static const uint8_t HU = 16;              // Hour units in BCD format. (4 bits)
    static const uint8_t MSK2 = 15;            // Alarm A minutes mask
    static const uint8_t MNT = 12;             // Minute tens in BCD format. (3 bits)
    static const uint8_t MNU = 8;              // Minute units in BCD format. (4 bits)
    static const uint8_t MSK1 = 7;             // Alarm A seconds mask
    static const uint8_t ST = 4;               // Second tens in BCD format. (3 bits)
    static const uint8_t SU = 0;               // Second units in BCD format. (4 bits)
}

namespace WPR // write protection register fields
{
    static const uint8_t KEY = 0;              // Write protection key (8 bits)
}

namespace SSR // sub second register fields
{
    static const uint8_t SS = 0;               // Sub second value (16 bits)
}

namespace SHIFTR // shift control register fields
{
    static const uint8_t ADD1S = 31;           // Add one second
    static const uint8_t SUBFS = 0;            // Subtract a fraction of a second (15 bits)
}

namespace TSTR // timestamp time register fields
{
    static const uint8_t PM = 22;              // AM/PM notation
    static const uint8_t HT = 20;              // Hour tens in BCD format. (2 bits)
    static const uint8_t HU = 16;              // Hour units in BCD format. (4 bits)
    static const uint8_t MNT = 12;             // Minute tens in BCD format. (3 bits)
    static const uint8_t MNU = 8;              // Minute units in BCD format. (4 bits)
    static const uint8_t ST = 4;               // Second tens in BCD format. (3 bits)
    static const uint8_t SU = 0;               // Second units in BCD format. (4 bits)
}

namespace TSDR // timestamp date register fields
{
    static const uint8_t WDU = 13;             // Week day units (3 bits)
    static const uint8_t MT = 12;              // Month tens in BCD format
    static const uint8_t MU = 8;               // Month units in BCD format (4 bits)
    static const uint8_t DT = 4;               // Date tens in BCD format (2 bits)
    static const uint8_t DU = 0;               // Date units in BCD format (4 bits)
}

namespace TSSSR // time-stamp sub second register fields
{
    static const uint8_t SS = 0;               // Sub second value (16 bits)
}

namespace CALR // calibration register fields
{
    static const uint8_t CALP = 15;            // Increase frequency of RTC by 488.5 ppm
    static const uint8_t CALW8 = 14;           // Use an 8-second calibration cycle period
    static const uint8_t CALW16 = 13;          // Use a 16-second calibration cycle period
    static const uint8_t CALM = 0;             // Calibration minus (9 bits)
}

namespace TAFCR // tamper and alternate function configuration register fields
{
    static const uint8_t PC15MODE = 23;        // PC15 mode
    static const uint8_t PC15VALUE = 22;       // PC15 value
    static const uint8_t PC14MODE = 21;        // PC14 mode
    static const uint8_t PC14VALUE = 20;       // PC14 value
    static const uint8_t PC13MODE = 19;        // PC13 mode
    static const uint8_t PC13VALUE = 18;       // RTC_ALARM output type/PC13 value
    static const uint8_t TAMP_PUDIS = 15;      // RTC_TAMPx pull-up disable
    static const uint8_t TAMP_PRCH = 13;       // RTC_TAMPx precharge duration (2 bits)
    static const uint8_t TAMPFLT = 11;         // RTC_TAMPx filter count (2 bits)
    static const uint8_t TAMPFREQ = 8;         // Tamper sampling frequency (3 bits)
    static const uint8_t TAMPTS = 7;           // Activate timestamp on tamper detection event
    static const uint8_t TAMP2_TRG = 4;        // Active level for RTC_TAMP2 input
    static const uint8_t TAMP2E = 3;           // RTC_TAMP2 input detection enable
    static const uint8_t TAMPIE = 2;           // Tamper interrupt enable
    static const uint8_t TAMP1TRG = 1;         // Active level for RTC_TAMP1 input
    static const uint8_t TAMP1E = 0;           // RTC_TAMP1 input detection enable
}

namespace ALRMASSR // alarm A sub second register fields
{
    static const uint8_t MASKSS = 24;          // Mask the most-significant bits starting at this bit (4 bits)
    static const uint8_t SS = 0;               // Sub seconds value (15 bits)
}

namespace BKP0R // backup register fields
{
    static const uint8_t BKP = 0;              // BKP (32 bits)
}

namespace BKP1R // backup register fields
{
    static const uint8_t BKP = 0;              // BKP (32 bits)
}

namespace BKP2R // backup register fields
{
    static const uint8_t BKP = 0;              // BKP (32 bits)
}

namespace BKP3R // backup register fields
{
    static const uint8_t BKP = 0;              // BKP (32 bits)
}

namespace BKP4R // backup register fields
{
    static const uint8_t BKP = 0;              // BKP (32 bits)
}

}

////
//
//    General-purpose-timers
//
////

namespace tim15
{

struct tim15_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMCR;                 // [Read-write] slave mode control register
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1_Output;         // [Read-write] capture/compare mode register (output mode)
    volatile uint32_t    CCMR1_Input;          // [Read-write] capture/compare mode register 1 (input mode)
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
};

tim15_t& TIM15 = *reinterpret_cast<tim15_t*>(0x40014000);

namespace CR1 // control register 1 fields
{
    static const uint8_t CKD = 8;              // Clock division (2 bits)
    static const uint8_t ARPE = 7;             // Auto-reload preload enable
    static const uint8_t OPM = 3;              // One-pulse mode
    static const uint8_t URS = 2;              // Update request source
    static const uint8_t UDIS = 1;             // Update disable
    static const uint8_t CEN = 0;              // Counter enable
}

namespace CR2 // control register 2 fields
{
    static const uint8_t OIS2 = 10;            // Output Idle state 2
    static const uint8_t OIS1N = 9;            // Output Idle state 1
    static const uint8_t OIS1 = 8;             // Output Idle state 1
    static const uint8_t MMS = 4;              // Master mode selection (3 bits)
    static const uint8_t CCDS = 3;             // Capture/compare DMA selection
    static const uint8_t CCUS = 2;             // Capture/compare control update selection
    static const uint8_t CCPC = 0;             // Capture/compare preloaded control
}

namespace SMCR // slave mode control register fields
{
    static const uint8_t MSM = 7;              // Master/Slave mode
    static const uint8_t TS = 4;               // Trigger selection (3 bits)
    static const uint8_t SMS = 0;              // Slave mode selection (3 bits)
}

namespace DIER // DMA/Interrupt enable register fields
{
    static const uint8_t TDE = 14;             // Trigger DMA request enable
    static const uint8_t CC2DE = 10;           // Capture/Compare 2 DMA request enable
    static const uint8_t CC1DE = 9;            // Capture/Compare 1 DMA request enable
    static const uint8_t UDE = 8;              // Update DMA request enable
    static const uint8_t BIE = 7;              // Break interrupt enable
    static const uint8_t TIE = 6;              // Trigger interrupt enable
    static const uint8_t COMIE = 5;            // COM interrupt enable
    static const uint8_t CC2IE = 2;            // Capture/Compare 2 interrupt enable
    static const uint8_t CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t UIE = 0;              // Update interrupt enable
}

namespace SR // status register fields
{
    static const uint8_t CC2OF = 10;           // Capture/compare 2 overcapture flag
    static const uint8_t CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t BIF = 7;              // Break interrupt flag
    static const uint8_t TIF = 6;              // Trigger interrupt flag
    static const uint8_t COMIF = 5;            // COM interrupt flag
    static const uint8_t CC2IF = 2;            // Capture/Compare 2 interrupt flag
    static const uint8_t CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t UIF = 0;              // Update interrupt flag
}

namespace EGR // event generation register fields
{
    static const uint8_t BG = 7;               // Break generation
    static const uint8_t TG = 6;               // Trigger generation
    static const uint8_t COMG = 5;             // Capture/Compare control update generation
    static const uint8_t CC2G = 2;             // Capture/compare 2 generation
    static const uint8_t CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t UG = 0;               // Update generation
}

namespace CCMR1_Output // capture/compare mode register (output mode) fields
{
    static const uint8_t OC2M = 12;            // Output Compare 2 mode (3 bits)
    static const uint8_t OC2PE = 11;           // Output Compare 2 preload enable
    static const uint8_t OC2FE = 10;           // Output Compare 2 fast enable
    static const uint8_t CC2S = 8;             // Capture/Compare 2 selection (2 bits)
    static const uint8_t OC1M = 4;             // Output Compare 1 mode (3 bits)
    static const uint8_t OC1PE = 3;            // Output Compare 1 preload enable
    static const uint8_t OC1FE = 2;            // Output Compare 1 fast enable
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCMR1_Input // capture/compare mode register 1 (input mode) fields
{
    static const uint8_t IC2F = 12;            // Input capture 2 filter (4 bits)
    static const uint8_t IC2PSC = 10;          // Input capture 2 prescaler (2 bits)
    static const uint8_t CC2S = 8;             // Capture/Compare 2 selection (2 bits)
    static const uint8_t IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t IC1PSC = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCER // capture/compare enable register fields
{
    static const uint8_t CC2NP = 7;            // Capture/Compare 2 output Polarity
    static const uint8_t CC2P = 5;             // Capture/Compare 2 output Polarity
    static const uint8_t CC2E = 4;             // Capture/Compare 2 output enable
    static const uint8_t CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CC1NE = 2;            // Capture/Compare 1 complementary output enable
    static const uint8_t CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CC1E = 0;             // Capture/Compare 1 output enable
}

namespace CNT // counter fields
{
    static const uint8_t CNT = 0;              // counter value (16 bits)
}

namespace PSC // prescaler fields
{
    static const uint8_t PSC = 0;              // Prescaler value (16 bits)
}

namespace ARR // auto-reload register fields
{
    static const uint8_t ARR = 0;              // Auto-reload value (16 bits)
}

namespace RCR // repetition counter register fields
{
    static const uint8_t REP = 0;              // Repetition counter value (8 bits)
}

namespace CCR1 // capture/compare register 1 fields
{
    static const uint8_t CCR1 = 0;             // Capture/Compare 1 value (16 bits)
}

namespace CCR2 // capture/compare register 2 fields
{
    static const uint8_t CCR2 = 0;             // Capture/Compare 2 value (16 bits)
}

namespace BDTR // break and dead-time register fields
{
    static const uint8_t MOE = 15;             // Main output enable
    static const uint8_t AOE = 14;             // Automatic output enable
    static const uint8_t BKP = 13;             // Break polarity
    static const uint8_t BKE = 12;             // Break enable
    static const uint8_t OSSR = 11;            // Off-state selection for Run mode
    static const uint8_t OSSI = 10;            // Off-state selection for Idle mode
    static const uint8_t LOCK = 8;             // Lock configuration (2 bits)
    static const uint8_t DTG = 0;              // Dead-time generator setup (8 bits)
}

namespace DCR // DMA control register fields
{
    static const uint8_t DBL = 8;              // DMA burst length (5 bits)
    static const uint8_t DBA = 0;              // DMA base address (5 bits)
}

namespace DMAR // DMA address for full transfer fields
{
    static const uint8_t DMAB = 0;             // DMA register for burst accesses (16 bits)
}

}

////
//
//    General-purpose-timers
//
////

namespace tim16
{

struct tim16_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    reserved_t<1>        _0;
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1_Output;         // [Read-write] capture/compare mode register (output mode)
    volatile uint32_t    CCMR1_Input;          // [Read-write] capture/compare mode register 1 (input mode)
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
};

tim16_t& TIM16 = *reinterpret_cast<tim16_t*>(0x40014400);

namespace CR1 // control register 1 fields
{
    static const uint8_t CKD = 8;              // Clock division (2 bits)
    static const uint8_t ARPE = 7;             // Auto-reload preload enable
    static const uint8_t OPM = 3;              // One-pulse mode
    static const uint8_t URS = 2;              // Update request source
    static const uint8_t UDIS = 1;             // Update disable
    static const uint8_t CEN = 0;              // Counter enable
}

namespace CR2 // control register 2 fields
{
    static const uint8_t OIS1N = 9;            // Output Idle state 1
    static const uint8_t OIS1 = 8;             // Output Idle state 1
    static const uint8_t CCDS = 3;             // Capture/compare DMA selection
    static const uint8_t CCUS = 2;             // Capture/compare control update selection
    static const uint8_t CCPC = 0;             // Capture/compare preloaded control
}

namespace DIER // DMA/Interrupt enable register fields
{
    static const uint8_t TDE = 14;             // Trigger DMA request enable
    static const uint8_t CC1DE = 9;            // Capture/Compare 1 DMA request enable
    static const uint8_t UDE = 8;              // Update DMA request enable
    static const uint8_t BIE = 7;              // Break interrupt enable
    static const uint8_t TIE = 6;              // Trigger interrupt enable
    static const uint8_t COMIE = 5;            // COM interrupt enable
    static const uint8_t CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t UIE = 0;              // Update interrupt enable
}

namespace SR // status register fields
{
    static const uint8_t CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t BIF = 7;              // Break interrupt flag
    static const uint8_t TIF = 6;              // Trigger interrupt flag
    static const uint8_t COMIF = 5;            // COM interrupt flag
    static const uint8_t CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t UIF = 0;              // Update interrupt flag
}

namespace EGR // event generation register fields
{
    static const uint8_t BG = 7;               // Break generation
    static const uint8_t TG = 6;               // Trigger generation
    static const uint8_t COMG = 5;             // Capture/Compare control update generation
    static const uint8_t CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t UG = 0;               // Update generation
}

namespace CCMR1_Output // capture/compare mode register (output mode) fields
{
    static const uint8_t OC1M = 4;             // Output Compare 1 mode (3 bits)
    static const uint8_t OC1PE = 3;            // Output Compare 1 preload enable
    static const uint8_t OC1FE = 2;            // Output Compare 1 fast enable
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCMR1_Input // capture/compare mode register 1 (input mode) fields
{
    static const uint8_t IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t IC1PSC = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCER // capture/compare enable register fields
{
    static const uint8_t CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CC1NE = 2;            // Capture/Compare 1 complementary output enable
    static const uint8_t CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CC1E = 0;             // Capture/Compare 1 output enable
}

namespace CNT // counter fields
{
    static const uint8_t CNT = 0;              // counter value (16 bits)
}

namespace PSC // prescaler fields
{
    static const uint8_t PSC = 0;              // Prescaler value (16 bits)
}

namespace ARR // auto-reload register fields
{
    static const uint8_t ARR = 0;              // Auto-reload value (16 bits)
}

namespace RCR // repetition counter register fields
{
    static const uint8_t REP = 0;              // Repetition counter value (8 bits)
}

namespace CCR1 // capture/compare register 1 fields
{
    static const uint8_t CCR1 = 0;             // Capture/Compare 1 value (16 bits)
}

namespace BDTR // break and dead-time register fields
{
    static const uint8_t MOE = 15;             // Main output enable
    static const uint8_t AOE = 14;             // Automatic output enable
    static const uint8_t BKP = 13;             // Break polarity
    static const uint8_t BKE = 12;             // Break enable
    static const uint8_t OSSR = 11;            // Off-state selection for Run mode
    static const uint8_t OSSI = 10;            // Off-state selection for Idle mode
    static const uint8_t LOCK = 8;             // Lock configuration (2 bits)
    static const uint8_t DTG = 0;              // Dead-time generator setup (8 bits)
}

namespace DCR // DMA control register fields
{
    static const uint8_t DBL = 8;              // DMA burst length (5 bits)
    static const uint8_t DBA = 0;              // DMA base address (5 bits)
}

namespace DMAR // DMA address for full transfer fields
{
    static const uint8_t DMAB = 0;             // DMA register for burst accesses (16 bits)
}

}

////
//
//    General-purpose-timers
//
////

namespace tim17
{

struct tim17_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    reserved_t<1>        _0;
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1_Output;         // [Read-write] capture/compare mode register (output mode)
    volatile uint32_t    CCMR1_Input;          // [Read-write] capture/compare mode register 1 (input mode)
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
};

tim17_t& TIM17 = *reinterpret_cast<tim17_t*>(0x40014800);

namespace CR1 // control register 1 fields
{
    static const uint8_t CKD = 8;              // Clock division (2 bits)
    static const uint8_t ARPE = 7;             // Auto-reload preload enable
    static const uint8_t OPM = 3;              // One-pulse mode
    static const uint8_t URS = 2;              // Update request source
    static const uint8_t UDIS = 1;             // Update disable
    static const uint8_t CEN = 0;              // Counter enable
}

namespace CR2 // control register 2 fields
{
    static const uint8_t OIS1N = 9;            // Output Idle state 1
    static const uint8_t OIS1 = 8;             // Output Idle state 1
    static const uint8_t CCDS = 3;             // Capture/compare DMA selection
    static const uint8_t CCUS = 2;             // Capture/compare control update selection
    static const uint8_t CCPC = 0;             // Capture/compare preloaded control
}

namespace DIER // DMA/Interrupt enable register fields
{
    static const uint8_t TDE = 14;             // Trigger DMA request enable
    static const uint8_t CC1DE = 9;            // Capture/Compare 1 DMA request enable
    static const uint8_t UDE = 8;              // Update DMA request enable
    static const uint8_t BIE = 7;              // Break interrupt enable
    static const uint8_t TIE = 6;              // Trigger interrupt enable
    static const uint8_t COMIE = 5;            // COM interrupt enable
    static const uint8_t CC1IE = 1;            // Capture/Compare 1 interrupt enable
    static const uint8_t UIE = 0;              // Update interrupt enable
}

namespace SR // status register fields
{
    static const uint8_t CC1OF = 9;            // Capture/Compare 1 overcapture flag
    static const uint8_t BIF = 7;              // Break interrupt flag
    static const uint8_t TIF = 6;              // Trigger interrupt flag
    static const uint8_t COMIF = 5;            // COM interrupt flag
    static const uint8_t CC1IF = 1;            // Capture/compare 1 interrupt flag
    static const uint8_t UIF = 0;              // Update interrupt flag
}

namespace EGR // event generation register fields
{
    static const uint8_t BG = 7;               // Break generation
    static const uint8_t TG = 6;               // Trigger generation
    static const uint8_t COMG = 5;             // Capture/Compare control update generation
    static const uint8_t CC1G = 1;             // Capture/compare 1 generation
    static const uint8_t UG = 0;               // Update generation
}

namespace CCMR1_Output // capture/compare mode register (output mode) fields
{
    static const uint8_t OC1M = 4;             // Output Compare 1 mode (3 bits)
    static const uint8_t OC1PE = 3;            // Output Compare 1 preload enable
    static const uint8_t OC1FE = 2;            // Output Compare 1 fast enable
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCMR1_Input // capture/compare mode register 1 (input mode) fields
{
    static const uint8_t IC1F = 4;             // Input capture 1 filter (4 bits)
    static const uint8_t IC1PSC = 2;           // Input capture 1 prescaler (2 bits)
    static const uint8_t CC1S = 0;             // Capture/Compare 1 selection (2 bits)
}

namespace CCER // capture/compare enable register fields
{
    static const uint8_t CC1NP = 3;            // Capture/Compare 1 output Polarity
    static const uint8_t CC1NE = 2;            // Capture/Compare 1 complementary output enable
    static const uint8_t CC1P = 1;             // Capture/Compare 1 output Polarity
    static const uint8_t CC1E = 0;             // Capture/Compare 1 output enable
}

namespace CNT // counter fields
{
    static const uint8_t CNT = 0;              // counter value (16 bits)
}

namespace PSC // prescaler fields
{
    static const uint8_t PSC = 0;              // Prescaler value (16 bits)
}

namespace ARR // auto-reload register fields
{
    static const uint8_t ARR = 0;              // Auto-reload value (16 bits)
}

namespace RCR // repetition counter register fields
{
    static const uint8_t REP = 0;              // Repetition counter value (8 bits)
}

namespace CCR1 // capture/compare register 1 fields
{
    static const uint8_t CCR1 = 0;             // Capture/Compare 1 value (16 bits)
}

namespace BDTR // break and dead-time register fields
{
    static const uint8_t MOE = 15;             // Main output enable
    static const uint8_t AOE = 14;             // Automatic output enable
    static const uint8_t BKP = 13;             // Break polarity
    static const uint8_t BKE = 12;             // Break enable
    static const uint8_t OSSR = 11;            // Off-state selection for Run mode
    static const uint8_t OSSI = 10;            // Off-state selection for Idle mode
    static const uint8_t LOCK = 8;             // Lock configuration (2 bits)
    static const uint8_t DTG = 0;              // Dead-time generator setup (8 bits)
}

namespace DCR // DMA control register fields
{
    static const uint8_t DBL = 8;              // DMA burst length (5 bits)
    static const uint8_t DBA = 0;              // DMA base address (5 bits)
}

namespace DMAR // DMA address for full transfer fields
{
    static const uint8_t DMAB = 0;             // DMA register for burst accesses (16 bits)
}

}

////
//
//    Touch sensing controller
//
////

namespace tsc
{

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
};

tsc_t& TSC = *reinterpret_cast<tsc_t*>(0x40024000);

namespace CR // control register fields
{
    static const uint8_t CTPH = 28;            // Charge transfer pulse high (4 bits)
    static const uint8_t CTPL = 24;            // Charge transfer pulse low (4 bits)
    static const uint8_t SSD = 17;             // Spread spectrum deviation (7 bits)
    static const uint8_t SSE = 16;             // Spread spectrum enable
    static const uint8_t SSPSC = 15;           // Spread spectrum prescaler
    static const uint8_t PGPSC = 12;           // pulse generator prescaler (3 bits)
    static const uint8_t MCV = 5;              // Max count value (3 bits)
    static const uint8_t IODEF = 4;            // I/O Default mode
    static const uint8_t SYNCPOL = 3;          // Synchronization pin polarity
    static const uint8_t AM = 2;               // Acquisition mode
    static const uint8_t START = 1;            // Start a new acquisition
    static const uint8_t TSCE = 0;             // Touch sensing controller enable
}

namespace IER // interrupt enable register fields
{
    static const uint8_t MCEIE = 1;            // Max count error interrupt enable
    static const uint8_t EOAIE = 0;            // End of acquisition interrupt enable
}

namespace ICR // interrupt clear register fields
{
    static const uint8_t MCEIC = 1;            // Max count error interrupt clear
    static const uint8_t EOAIC = 0;            // End of acquisition interrupt clear
}

namespace ISR // interrupt status register fields
{
    static const uint8_t MCEF = 1;             // Max count error flag
    static const uint8_t EOAF = 0;             // End of acquisition flag
}

namespace IOHCR // I/O hysteresis control register fields
{
    static const uint8_t G6_IO4 = 23;          // G6_IO4 Schmitt trigger hysteresis mode
    static const uint8_t G6_IO3 = 22;          // G6_IO3 Schmitt trigger hysteresis mode
    static const uint8_t G6_IO2 = 21;          // G6_IO2 Schmitt trigger hysteresis mode
    static const uint8_t G6_IO1 = 20;          // G6_IO1 Schmitt trigger hysteresis mode
    static const uint8_t G5_IO4 = 19;          // G5_IO4 Schmitt trigger hysteresis mode
    static const uint8_t G5_IO3 = 18;          // G5_IO3 Schmitt trigger hysteresis mode
    static const uint8_t G5_IO2 = 17;          // G5_IO2 Schmitt trigger hysteresis mode
    static const uint8_t G5_IO1 = 16;          // G5_IO1 Schmitt trigger hysteresis mode
    static const uint8_t G4_IO4 = 15;          // G4_IO4 Schmitt trigger hysteresis mode
    static const uint8_t G4_IO3 = 14;          // G4_IO3 Schmitt trigger hysteresis mode
    static const uint8_t G4_IO2 = 13;          // G4_IO2 Schmitt trigger hysteresis mode
    static const uint8_t G4_IO1 = 12;          // G4_IO1 Schmitt trigger hysteresis mode
    static const uint8_t G3_IO4 = 11;          // G3_IO4 Schmitt trigger hysteresis mode
    static const uint8_t G3_IO3 = 10;          // G3_IO3 Schmitt trigger hysteresis mode
    static const uint8_t G3_IO2 = 9;           // G3_IO2 Schmitt trigger hysteresis mode
    static const uint8_t G3_IO1 = 8;           // G3_IO1 Schmitt trigger hysteresis mode
    static const uint8_t G2_IO4 = 7;           // G2_IO4 Schmitt trigger hysteresis mode
    static const uint8_t G2_IO3 = 6;           // G2_IO3 Schmitt trigger hysteresis mode
    static const uint8_t G2_IO2 = 5;           // G2_IO2 Schmitt trigger hysteresis mode
    static const uint8_t G2_IO1 = 4;           // G2_IO1 Schmitt trigger hysteresis mode
    static const uint8_t G1_IO4 = 3;           // G1_IO4 Schmitt trigger hysteresis mode
    static const uint8_t G1_IO3 = 2;           // G1_IO3 Schmitt trigger hysteresis mode
    static const uint8_t G1_IO2 = 1;           // G1_IO2 Schmitt trigger hysteresis mode
    static const uint8_t G1_IO1 = 0;           // G1_IO1 Schmitt trigger hysteresis mode
}

namespace IOASCR // I/O analog switch control register fields
{
    static const uint8_t G6_IO4 = 23;          // G6_IO4 analog switch enable
    static const uint8_t G6_IO3 = 22;          // G6_IO3 analog switch enable
    static const uint8_t G6_IO2 = 21;          // G6_IO2 analog switch enable
    static const uint8_t G6_IO1 = 20;          // G6_IO1 analog switch enable
    static const uint8_t G5_IO4 = 19;          // G5_IO4 analog switch enable
    static const uint8_t G5_IO3 = 18;          // G5_IO3 analog switch enable
    static const uint8_t G5_IO2 = 17;          // G5_IO2 analog switch enable
    static const uint8_t G5_IO1 = 16;          // G5_IO1 analog switch enable
    static const uint8_t G4_IO4 = 15;          // G4_IO4 analog switch enable
    static const uint8_t G4_IO3 = 14;          // G4_IO3 analog switch enable
    static const uint8_t G4_IO2 = 13;          // G4_IO2 analog switch enable
    static const uint8_t G4_IO1 = 12;          // G4_IO1 analog switch enable
    static const uint8_t G3_IO4 = 11;          // G3_IO4 analog switch enable
    static const uint8_t G3_IO3 = 10;          // G3_IO3 analog switch enable
    static const uint8_t G3_IO2 = 9;           // G3_IO2 analog switch enable
    static const uint8_t G3_IO1 = 8;           // G3_IO1 analog switch enable
    static const uint8_t G2_IO4 = 7;           // G2_IO4 analog switch enable
    static const uint8_t G2_IO3 = 6;           // G2_IO3 analog switch enable
    static const uint8_t G2_IO2 = 5;           // G2_IO2 analog switch enable
    static const uint8_t G2_IO1 = 4;           // G2_IO1 analog switch enable
    static const uint8_t G1_IO4 = 3;           // G1_IO4 analog switch enable
    static const uint8_t G1_IO3 = 2;           // G1_IO3 analog switch enable
    static const uint8_t G1_IO2 = 1;           // G1_IO2 analog switch enable
    static const uint8_t G1_IO1 = 0;           // G1_IO1 analog switch enable
}

namespace IOSCR // I/O sampling control register fields
{
    static const uint8_t G6_IO4 = 23;          // G6_IO4 sampling mode
    static const uint8_t G6_IO3 = 22;          // G6_IO3 sampling mode
    static const uint8_t G6_IO2 = 21;          // G6_IO2 sampling mode
    static const uint8_t G6_IO1 = 20;          // G6_IO1 sampling mode
    static const uint8_t G5_IO4 = 19;          // G5_IO4 sampling mode
    static const uint8_t G5_IO3 = 18;          // G5_IO3 sampling mode
    static const uint8_t G5_IO2 = 17;          // G5_IO2 sampling mode
    static const uint8_t G5_IO1 = 16;          // G5_IO1 sampling mode
    static const uint8_t G4_IO4 = 15;          // G4_IO4 sampling mode
    static const uint8_t G4_IO3 = 14;          // G4_IO3 sampling mode
    static const uint8_t G4_IO2 = 13;          // G4_IO2 sampling mode
    static const uint8_t G4_IO1 = 12;          // G4_IO1 sampling mode
    static const uint8_t G3_IO4 = 11;          // G3_IO4 sampling mode
    static const uint8_t G3_IO3 = 10;          // G3_IO3 sampling mode
    static const uint8_t G3_IO2 = 9;           // G3_IO2 sampling mode
    static const uint8_t G3_IO1 = 8;           // G3_IO1 sampling mode
    static const uint8_t G2_IO4 = 7;           // G2_IO4 sampling mode
    static const uint8_t G2_IO3 = 6;           // G2_IO3 sampling mode
    static const uint8_t G2_IO2 = 5;           // G2_IO2 sampling mode
    static const uint8_t G2_IO1 = 4;           // G2_IO1 sampling mode
    static const uint8_t G1_IO4 = 3;           // G1_IO4 sampling mode
    static const uint8_t G1_IO3 = 2;           // G1_IO3 sampling mode
    static const uint8_t G1_IO2 = 1;           // G1_IO2 sampling mode
    static const uint8_t G1_IO1 = 0;           // G1_IO1 sampling mode
}

namespace IOCCR // I/O channel control register fields
{
    static const uint8_t G6_IO4 = 23;          // G6_IO4 channel mode
    static const uint8_t G6_IO3 = 22;          // G6_IO3 channel mode
    static const uint8_t G6_IO2 = 21;          // G6_IO2 channel mode
    static const uint8_t G6_IO1 = 20;          // G6_IO1 channel mode
    static const uint8_t G5_IO4 = 19;          // G5_IO4 channel mode
    static const uint8_t G5_IO3 = 18;          // G5_IO3 channel mode
    static const uint8_t G5_IO2 = 17;          // G5_IO2 channel mode
    static const uint8_t G5_IO1 = 16;          // G5_IO1 channel mode
    static const uint8_t G4_IO4 = 15;          // G4_IO4 channel mode
    static const uint8_t G4_IO3 = 14;          // G4_IO3 channel mode
    static const uint8_t G4_IO2 = 13;          // G4_IO2 channel mode
    static const uint8_t G4_IO1 = 12;          // G4_IO1 channel mode
    static const uint8_t G3_IO4 = 11;          // G3_IO4 channel mode
    static const uint8_t G3_IO3 = 10;          // G3_IO3 channel mode
    static const uint8_t G3_IO2 = 9;           // G3_IO2 channel mode
    static const uint8_t G3_IO1 = 8;           // G3_IO1 channel mode
    static const uint8_t G2_IO4 = 7;           // G2_IO4 channel mode
    static const uint8_t G2_IO3 = 6;           // G2_IO3 channel mode
    static const uint8_t G2_IO2 = 5;           // G2_IO2 channel mode
    static const uint8_t G2_IO1 = 4;           // G2_IO1 channel mode
    static const uint8_t G1_IO4 = 3;           // G1_IO4 channel mode
    static const uint8_t G1_IO3 = 2;           // G1_IO3 channel mode
    static const uint8_t G1_IO2 = 1;           // G1_IO2 channel mode
    static const uint8_t G1_IO1 = 0;           // G1_IO1 channel mode
}

namespace IOGCSR // I/O group control status register fields
{
    static const uint8_t G8S = 23;             // Analog I/O group x status, Read-write
    static const uint8_t G7S = 22;             // Analog I/O group x status, Read-write
    static const uint8_t G6S = 21;             // Analog I/O group x status, Read-only
    static const uint8_t G5S = 20;             // Analog I/O group x status, Read-only
    static const uint8_t G4S = 19;             // Analog I/O group x status, Read-only
    static const uint8_t G3S = 18;             // Analog I/O group x status, Read-only
    static const uint8_t G2S = 17;             // Analog I/O group x status, Read-only
    static const uint8_t G1S = 16;             // Analog I/O group x status, Read-only
    static const uint8_t G8E = 7;              // Analog I/O group x enable, Read-write
    static const uint8_t G7E = 6;              // Analog I/O group x enable, Read-write
    static const uint8_t G6E = 5;              // Analog I/O group x enable, Read-write
    static const uint8_t G5E = 4;              // Analog I/O group x enable, Read-write
    static const uint8_t G4E = 3;              // Analog I/O group x enable, Read-write
    static const uint8_t G3E = 2;              // Analog I/O group x enable, Read-write
    static const uint8_t G2E = 1;              // Analog I/O group x enable, Read-write
    static const uint8_t G1E = 0;              // Analog I/O group x enable, Read-write
}

namespace IOG1CR // I/O group x counter register fields
{
    static const uint8_t CNT = 0;              // Counter value (14 bits)
}

namespace IOG2CR // I/O group x counter register fields
{
    static const uint8_t CNT = 0;              // Counter value (14 bits)
}

namespace IOG3CR // I/O group x counter register fields
{
    static const uint8_t CNT = 0;              // Counter value (14 bits)
}

namespace IOG4CR // I/O group x counter register fields
{
    static const uint8_t CNT = 0;              // Counter value (14 bits)
}

namespace IOG5CR // I/O group x counter register fields
{
    static const uint8_t CNT = 0;              // Counter value (14 bits)
}

namespace IOG6CR // I/O group x counter register fields
{
    static const uint8_t CNT = 0;              // Counter value (14 bits)
}

}

////
//
//    HDMI-CEC controller
//
////

namespace cec
{

struct cec_t
{
    volatile uint32_t    CR;                   // [Read-write] control register
    volatile uint32_t    CFGR;                 // [Read-write] configuration register
    volatile uint32_t    TXDR;                 // [Write-only] Tx data register
    volatile uint32_t    RXDR;                 // [Read-only] Rx Data Register
    volatile uint32_t    ISR;                  // [Read-write] Interrupt and Status Register
    volatile uint32_t    IER;                  // [Read-write] interrupt enable register
};

cec_t& CEC = *reinterpret_cast<cec_t*>(0x40007800);

namespace CR // control register fields
{
    static const uint8_t TXEOM = 2;            // Tx End Of Message
    static const uint8_t TXSOM = 1;            // Tx start of message
    static const uint8_t CECEN = 0;            // CEC Enable
}

namespace CFGR // configuration register fields
{
    static const uint8_t LBPEGEN = 11;         // Generate Error-Bit on Long Bit Period Error
    static const uint8_t BREGEN = 10;          // Generate error-bit on bit rising error
    static const uint8_t BRESTP = 9;           // Rx-stop on bit rising error
    static const uint8_t RXTOL = 8;            // Rx-Tolerance
    static const uint8_t SFT = 5;              // Signal Free Time (3 bits)
    static const uint8_t LSTN = 4;             // Listen mode
    static const uint8_t OAR = 0;              // Own Address (4 bits)
}

namespace TXDR // Tx data register fields
{
    static const uint8_t TXD = 0;              // Tx Data register (8 bits)
}

namespace RXDR // Rx Data Register fields
{
    static const uint8_t RXDR = 0;             // CEC Rx Data Register (8 bits)
}

namespace ISR // Interrupt and Status Register fields
{
    static const uint8_t TXACKE = 12;          // Tx-Missing acknowledge error
    static const uint8_t TXERR = 11;           // Tx-Error
    static const uint8_t TXUDR = 10;           // Tx-Buffer Underrun
    static const uint8_t TXEND = 9;            // End of Transmission
    static const uint8_t TXBR = 8;             // Tx-Byte Request
    static const uint8_t ARBLST = 7;           // Arbitration Lost
    static const uint8_t RXACKE = 6;           // Rx-Missing Acknowledge
    static const uint8_t LBPE = 5;             // Rx-Long Bit Period Error
    static const uint8_t SBPE = 4;             // Rx-Short Bit period error
    static const uint8_t BRE = 3;              // Rx-Bit rising error
    static const uint8_t RXOVR = 2;            // Rx-Overrun
    static const uint8_t RXEND = 1;            // End Of Reception
    static const uint8_t RXBR = 0;             // Rx-Byte Received
}

namespace IER // interrupt enable register fields
{
    static const uint8_t TXACKIE = 12;         // Tx-Missing Acknowledge Error Interrupt Enable
    static const uint8_t TXERRIE = 11;         // Tx-Error Interrupt Enable
    static const uint8_t TXUDRIE = 10;         // Tx-Underrun interrupt enable
    static const uint8_t TXENDIE = 9;          // Tx-End of message interrupt enable
    static const uint8_t TXBRIE = 8;           // Tx-Byte Request Interrupt Enable
    static const uint8_t ARBLSTIE = 7;         // Arbitration Lost Interrupt Enable
    static const uint8_t RXACKIE = 6;          // Rx-Missing Acknowledge Error Interrupt Enable
    static const uint8_t LBPEIE = 5;           // Long Bit Period Error Interrupt Enable
    static const uint8_t SBPEIE = 4;           // Short Bit Period Error Interrupt Enable
    static const uint8_t BREIE = 3;            // Bit Rising Error Interrupt Enable
    static const uint8_t RXOVRIE = 2;          // Rx-Buffer Overrun Interrupt Enable
    static const uint8_t RXENDIE = 1;          // End Of Reception Interrupt Enable
    static const uint8_t RXBRIE = 0;           // Rx-Byte Received Interrupt Enable
}

}

////
//
//    Flash
//
////

namespace flash
{

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
};

flash_t& Flash = *reinterpret_cast<flash_t*>(0x40022000);

namespace ACR // Flash access control register fields
{
    static const uint8_t LATENCY = 0;          // LATENCY (3 bits), Read-write
    static const uint8_t PRFTBE = 4;           // PRFTBE, Read-write
    static const uint8_t PRFTBS = 5;           // PRFTBS, Read-only
}

namespace KEYR // Flash key register fields
{
    static const uint8_t FKEYR = 0;            // Flash Key (32 bits)
}

namespace OPTKEYR // Flash option key register fields
{
    static const uint8_t OPTKEYR = 0;          // Option byte key (32 bits)
}

namespace SR // Flash status register fields
{
    static const uint8_t EOP = 5;              // End of operation, Read-write
    static const uint8_t WRPRT = 4;            // Write protection error, Read-write
    static const uint8_t PGERR = 2;            // Programming error, Read-write
    static const uint8_t BSY = 0;              // Busy, Read-only
}

namespace CR // Flash control register fields
{
    static const uint8_t FORCE_OPTLOAD = 13;   // Force option byte loading
    static const uint8_t EOPIE = 12;           // End of operation interrupt enable
    static const uint8_t ERRIE = 10;           // Error interrupt enable
    static const uint8_t OPTWRE = 9;           // Option bytes write enable
    static const uint8_t LOCK = 7;             // Lock
    static const uint8_t STRT = 6;             // Start
    static const uint8_t OPTER = 5;            // Option byte erase
    static const uint8_t OPTPG = 4;            // Option byte programming
    static const uint8_t MER = 2;              // Mass erase
    static const uint8_t PER = 1;              // Page erase
    static const uint8_t PG = 0;               // Programming
}

namespace AR // Flash address register fields
{
    static const uint8_t FAR = 0;              // Flash address (32 bits)
}

namespace OBR // Option byte register fields
{
    static const uint8_t OPTERR = 0;           // Option byte error
    static const uint8_t RDPRT = 1;            // Read protection level status (2 bits)
    static const uint8_t WDG_SW = 8;           // WDG_SW
    static const uint8_t nRST_STOP = 9;        // nRST_STOP
    static const uint8_t nRST_STDBY = 10;      // nRST_STDBY
    static const uint8_t nBOOT0 = 11;          // nBOOT0
    static const uint8_t nBOOT1 = 12;          // BOOT1
    static const uint8_t VDDA_MONITOR = 13;    // VDDA_MONITOR
    static const uint8_t RAM_PARITY_CHECK = 14;// RAM_PARITY_CHECK (0 bits)
    static const uint8_t BOOT_SEL = 15;        // BOOT_SEL
    static const uint8_t Data0 = 16;           // Data0 (8 bits)
    static const uint8_t Data1 = 24;           // Data1 (8 bits)
}

namespace WRPR // Write protection register fields
{
    static const uint8_t WRP = 0;              // Write protect (32 bits)
}

}

////
//
//    Debug support
//
////

namespace dbgmcu
{

struct dbgmcu_t
{
    volatile uint32_t    IDCODE;               // [Read-only] MCU Device ID Code Register
    volatile uint32_t    CR;                   // [Read-write] Debug MCU Configuration Register
    volatile uint32_t    APB1_FZ;              // [Read-write] Debug MCU APB1 freeze register
    volatile uint32_t    APB2_FZ;              // [Read-write] Debug MCU APB2 freeze register
};

dbgmcu_t& DBGMCU = *reinterpret_cast<dbgmcu_t*>(0x40015800);

namespace IDCODE // MCU Device ID Code Register fields
{
    static const uint8_t DEV_ID = 0;           // Device Identifier (12 bits)
    static const uint8_t DIV_ID = 12;          // Division Identifier (4 bits)
    static const uint8_t REV_ID = 16;          // Revision Identifier (16 bits)
}

namespace CR // Debug MCU Configuration Register fields
{
    static const uint8_t DBG_STOP = 1;         // Debug Stop Mode
    static const uint8_t DBG_STANDBY = 2;      // Debug Standby Mode
}

namespace APB1_FZ // Debug MCU APB1 freeze register fields
{
    static const uint8_t DBG_TIM2_STOP = 0;    // TIM2 counter stopped when core is halted
    static const uint8_t DBG_TIM3_STOP = 1;    // TIM3 counter stopped when core is halted
    static const uint8_t TIM3_counter_stopped_when_core_is_halted = 4;// TIM6 counter stopped when core is halted
    static const uint8_t DBG_TIM7_STOP = 5;    // TIM7 counter stopped when core is halted
    static const uint8_t DBG_TIM14_STOP = 8;   // TIM14 counter stopped when core is halted
    static const uint8_t DBG_RTC_STOP = 10;    // Debug RTC stopped when core is halted
    static const uint8_t DBG_WWDG_STOP = 11;   // Debug window watchdog stopped when core is halted
    static const uint8_t DBG_IWDG_STOP = 12;   // Debug independent watchdog stopped when core is halted
    static const uint8_t DBG_I2C1_SMBUS_TIMEOUT = 21;// SMBUS timeout mode stopped when core is halted
    static const uint8_t DBG_CAN_STOP = 25;    // CAN stopped when core is halted
}

namespace APB2_FZ // Debug MCU APB2 freeze register fields
{
    static const uint8_t DBG_TIM1_STOP = 11;   // TIM1 counter stopped when core is halted
    static const uint8_t DBG_TIM15_STOP = 16;  // TIM15 counter stopped when core is halted
    static const uint8_t DBG_TIM16_STOP = 17;  // TIM16 counter stopped when core is halted
    static const uint8_t DBG_TIM17_STOP = 18;  // TIM17 counter stopped when core is halted
}

}

////
//
//    Universal serial bus full-speed device interface
//
////

namespace usb
{

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
};

usb_t& USB = *reinterpret_cast<usb_t*>(0x40005c00);

namespace EP0R // endpoint 0 register fields
{
    static const uint8_t EA = 0;               // Endpoint address (4 bits)
    static const uint8_t STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t SETUP = 11;           // Setup transaction completed
    static const uint8_t STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t CTR_RX = 15;          // Correct transfer for reception
}

namespace EP1R // endpoint 1 register fields
{
    static const uint8_t EA = 0;               // Endpoint address (4 bits)
    static const uint8_t STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t SETUP = 11;           // Setup transaction completed
    static const uint8_t STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t CTR_RX = 15;          // Correct transfer for reception
}

namespace EP2R // endpoint 2 register fields
{
    static const uint8_t EA = 0;               // Endpoint address (4 bits)
    static const uint8_t STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t SETUP = 11;           // Setup transaction completed
    static const uint8_t STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t CTR_RX = 15;          // Correct transfer for reception
}

namespace EP3R // endpoint 3 register fields
{
    static const uint8_t EA = 0;               // Endpoint address (4 bits)
    static const uint8_t STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t SETUP = 11;           // Setup transaction completed
    static const uint8_t STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t CTR_RX = 15;          // Correct transfer for reception
}

namespace EP4R // endpoint 4 register fields
{
    static const uint8_t EA = 0;               // Endpoint address (4 bits)
    static const uint8_t STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t SETUP = 11;           // Setup transaction completed
    static const uint8_t STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t CTR_RX = 15;          // Correct transfer for reception
}

namespace EP5R // endpoint 5 register fields
{
    static const uint8_t EA = 0;               // Endpoint address (4 bits)
    static const uint8_t STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t SETUP = 11;           // Setup transaction completed
    static const uint8_t STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t CTR_RX = 15;          // Correct transfer for reception
}

namespace EP6R // endpoint 6 register fields
{
    static const uint8_t EA = 0;               // Endpoint address (4 bits)
    static const uint8_t STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t SETUP = 11;           // Setup transaction completed
    static const uint8_t STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t CTR_RX = 15;          // Correct transfer for reception
}

namespace EP7R // endpoint 7 register fields
{
    static const uint8_t EA = 0;               // Endpoint address (4 bits)
    static const uint8_t STAT_TX = 4;          // Status bits, for transmission transfers (2 bits)
    static const uint8_t DTOG_TX = 6;          // Data Toggle, for transmission transfers
    static const uint8_t CTR_TX = 7;           // Correct Transfer for transmission
    static const uint8_t EP_KIND = 8;          // Endpoint kind
    static const uint8_t EP_TYPE = 9;          // Endpoint type (2 bits)
    static const uint8_t SETUP = 11;           // Setup transaction completed
    static const uint8_t STAT_RX = 12;         // Status bits, for reception transfers (2 bits)
    static const uint8_t DTOG_RX = 14;         // Data Toggle, for reception transfers
    static const uint8_t CTR_RX = 15;          // Correct transfer for reception
}

namespace CNTR // control register fields
{
    static const uint8_t FRES = 0;             // Force USB Reset
    static const uint8_t PDWN = 1;             // Power down
    static const uint8_t LPMODE = 2;           // Low-power mode
    static const uint8_t FSUSP = 3;            // Force suspend
    static const uint8_t RESUME = 4;           // Resume request
    static const uint8_t L1RESUME = 5;         // LPM L1 Resume request
    static const uint8_t L1REQM = 7;           // LPM L1 state request interrupt mask
    static const uint8_t ESOFM = 8;            // Expected start of frame interrupt mask
    static const uint8_t SOFM = 9;             // Start of frame interrupt mask
    static const uint8_t RESETM = 10;          // USB reset interrupt mask
    static const uint8_t SUSPM = 11;           // Suspend mode interrupt mask
    static const uint8_t WKUPM = 12;           // Wakeup interrupt mask
    static const uint8_t ERRM = 13;            // Error interrupt mask
    static const uint8_t PMAOVRM = 14;         // Packet memory area over / underrun interrupt mask
    static const uint8_t CTRM = 15;            // Correct transfer interrupt mask
}

namespace ISTR // interrupt status register fields
{
    static const uint8_t EP_ID = 0;            // Endpoint Identifier (4 bits), Read-only
    static const uint8_t DIR = 4;              // Direction of transaction, Read-only
    static const uint8_t L1REQ = 7;            // LPM L1 state request, Read-write
    static const uint8_t ESOF = 8;             // Expected start frame, Read-write
    static const uint8_t SOF = 9;              // start of frame, Read-write
    static const uint8_t RESET = 10;           // reset request, Read-write
    static const uint8_t SUSP = 11;            // Suspend mode request, Read-write
    static const uint8_t WKUP = 12;            // Wakeup, Read-write
    static const uint8_t ERR = 13;             // Error, Read-write
    static const uint8_t PMAOVR = 14;          // Packet memory area over / underrun, Read-write
    static const uint8_t CTR = 15;             // Correct transfer, Read-only
}

namespace FNR // frame number register fields
{
    static const uint8_t FN = 0;               // Frame number (11 bits)
    static const uint8_t LSOF = 11;            // Lost SOF (2 bits)
    static const uint8_t LCK = 13;             // Locked
    static const uint8_t RXDM = 14;            // Receive data - line status
    static const uint8_t RXDP = 15;            // Receive data + line status
}

namespace DADDR // device address fields
{
    static const uint8_t ADD = 0;              // Device address (7 bits)
    static const uint8_t EF = 7;               // Enable function
}

namespace BTABLE // Buffer table address fields
{
    static const uint8_t BTABLE = 3;           // Buffer table (13 bits)
}

namespace LPMCSR // LPM control and status register fields
{
    static const uint8_t LPMEN = 0;            // LPM support enable, Read-write
    static const uint8_t LPMACK = 1;           // LPM Token acknowledge enable, Read-write
    static const uint8_t REMWAKE = 3;          // bRemoteWake value, Read-only
    static const uint8_t BESL = 4;             // BESL value (4 bits), Read-only
}

namespace BCDR // Battery charging detector fields
{
    static const uint8_t BCDEN = 0;            // Battery charging detector (BCD) enable, Read-write
    static const uint8_t DCDEN = 1;            // Data contact detection (DCD) mode enable, Read-write
    static const uint8_t PDEN = 2;             // Primary detection (PD) mode enable, Read-write
    static const uint8_t SDEN = 3;             // Secondary detection (SD) mode enable, Read-write
    static const uint8_t DCDET = 4;            // Data contact detection (DCD) status, Read-only
    static const uint8_t PDET = 5;             // Primary detection (PD) status, Read-only
    static const uint8_t SDET = 6;             // Secondary detection (SD) status, Read-only
    static const uint8_t PS2DET = 7;           // DM pull-up detection status, Read-only
    static const uint8_t DPPU = 15;            // DP pull-up control, Read-write
}

}

////
//
//    Clock recovery system
//
////

namespace crs
{

struct crs_t
{
    volatile uint32_t    CR;                   // [Read-write] control register
    volatile uint32_t    CFGR;                 // [Read-write] configuration register
    volatile uint32_t    ISR;                  // [Read-only] interrupt and status register
    volatile uint32_t    ICR;                  // [Read-write] interrupt flag clear register
};

crs_t& CRS = *reinterpret_cast<crs_t*>(0x40006c00);

namespace CR // control register fields
{
    static const uint8_t TRIM = 8;             // HSI48 oscillator smooth trimming (6 bits)
    static const uint8_t SWSYNC = 7;           // Generate software SYNC event
    static const uint8_t AUTOTRIMEN = 6;       // Automatic trimming enable
    static const uint8_t CEN = 5;              // Frequency error counter enable
    static const uint8_t ESYNCIE = 3;          // Expected SYNC interrupt enable
    static const uint8_t ERRIE = 2;            // Synchronization or trimming error interrupt enable
    static const uint8_t SYNCWARNIE = 1;       // SYNC warning interrupt enable
    static const uint8_t SYNCOKIE = 0;         // SYNC event OK interrupt enable
}

namespace CFGR // configuration register fields
{
    static const uint8_t SYNCPOL = 31;         // SYNC polarity selection
    static const uint8_t SYNCSRC = 28;         // SYNC signal source selection (2 bits)
    static const uint8_t SYNCDIV = 24;         // SYNC divider (3 bits)
    static const uint8_t FELIM = 16;           // Frequency error limit (8 bits)
    static const uint8_t RELOAD = 0;           // Counter reload value (16 bits)
}

namespace ISR // interrupt and status register fields
{
    static const uint8_t FECAP = 16;           // Frequency error capture (16 bits)
    static const uint8_t FEDIR = 15;           // Frequency error direction
    static const uint8_t TRIMOVF = 10;         // Trimming overflow or underflow
    static const uint8_t SYNCMISS = 9;         // SYNC missed
    static const uint8_t SYNCERR = 8;          // SYNC error
    static const uint8_t ESYNCF = 3;           // Expected SYNC flag
    static const uint8_t ERRF = 2;             // Error flag
    static const uint8_t SYNCWARNF = 1;        // SYNC warning flag
    static const uint8_t SYNCOKF = 0;          // SYNC event OK flag
}

namespace ICR // interrupt flag clear register fields
{
    static const uint8_t ESYNCC = 3;           // Expected SYNC clear flag
    static const uint8_t ERRC = 2;             // Error clear flag
    static const uint8_t SYNCWARNC = 1;        // SYNC warning clear flag
    static const uint8_t SYNCOKC = 0;          // SYNC event OK clear flag
}

}

////
//
//    Controller area network
//
////

namespace can
{

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
};

can_t& CAN = *reinterpret_cast<can_t*>(0x40006400);

namespace CAN_MCR // CAN_MCR fields
{
    static const uint8_t DBF = 16;             // DBF
    static const uint8_t RESET = 15;           // RESET
    static const uint8_t TTCM = 7;             // TTCM
    static const uint8_t ABOM = 6;             // ABOM
    static const uint8_t AWUM = 5;             // AWUM
    static const uint8_t NART = 4;             // NART
    static const uint8_t RFLM = 3;             // RFLM
    static const uint8_t TXFP = 2;             // TXFP
    static const uint8_t SLEEP = 1;            // SLEEP
    static const uint8_t INRQ = 0;             // INRQ
}

namespace CAN_MSR // CAN_MSR fields
{
    static const uint8_t RX = 11;              // RX, Read-only
    static const uint8_t SAMP = 10;            // SAMP, Read-only
    static const uint8_t RXM = 9;              // RXM, Read-only
    static const uint8_t TXM = 8;              // TXM, Read-only
    static const uint8_t SLAKI = 4;            // SLAKI, Read-write
    static const uint8_t WKUI = 3;             // WKUI, Read-write
    static const uint8_t ERRI = 2;             // ERRI, Read-write
    static const uint8_t SLAK = 1;             // SLAK, Read-only
    static const uint8_t INAK = 0;             // INAK, Read-only
}

namespace CAN_TSR // CAN_TSR fields
{
    static const uint8_t LOW2 = 31;            // Lowest priority flag for mailbox 2, Read-only
    static const uint8_t LOW1 = 30;            // Lowest priority flag for mailbox 1, Read-only
    static const uint8_t LOW0 = 29;            // Lowest priority flag for mailbox 0, Read-only
    static const uint8_t TME2 = 28;            // Lowest priority flag for mailbox 2, Read-only
    static const uint8_t TME1 = 27;            // Lowest priority flag for mailbox 1, Read-only
    static const uint8_t TME0 = 26;            // Lowest priority flag for mailbox 0, Read-only
    static const uint8_t CODE = 24;            // CODE (2 bits), Read-only
    static const uint8_t ABRQ2 = 23;           // ABRQ2, Read-write
    static const uint8_t TERR2 = 19;           // TERR2, Read-write
    static const uint8_t ALST2 = 18;           // ALST2, Read-write
    static const uint8_t TXOK2 = 17;           // TXOK2, Read-write
    static const uint8_t RQCP2 = 16;           // RQCP2, Read-write
    static const uint8_t ABRQ1 = 15;           // ABRQ1, Read-write
    static const uint8_t TERR1 = 11;           // TERR1, Read-write
    static const uint8_t ALST1 = 10;           // ALST1, Read-write
    static const uint8_t TXOK1 = 9;            // TXOK1, Read-write
    static const uint8_t RQCP1 = 8;            // RQCP1, Read-write
    static const uint8_t ABRQ0 = 7;            // ABRQ0, Read-write
    static const uint8_t TERR0 = 3;            // TERR0, Read-write
    static const uint8_t ALST0 = 2;            // ALST0, Read-write
    static const uint8_t TXOK0 = 1;            // TXOK0, Read-write
    static const uint8_t RQCP0 = 0;            // RQCP0, Read-write
}

namespace CAN_RF0R // CAN_RF0R fields
{
    static const uint8_t RFOM0 = 5;            // RFOM0, Read-write
    static const uint8_t FOVR0 = 4;            // FOVR0, Read-write
    static const uint8_t FULL0 = 3;            // FULL0, Read-write
    static const uint8_t FMP0 = 0;             // FMP0 (2 bits), Read-only
}

namespace CAN_RF1R // CAN_RF1R fields
{
    static const uint8_t RFOM1 = 5;            // RFOM1, Read-write
    static const uint8_t FOVR1 = 4;            // FOVR1, Read-write
    static const uint8_t FULL1 = 3;            // FULL1, Read-write
    static const uint8_t FMP1 = 0;             // FMP1 (2 bits), Read-only
}

namespace CAN_IER // CAN_IER fields
{
    static const uint8_t SLKIE = 17;           // SLKIE
    static const uint8_t WKUIE = 16;           // WKUIE
    static const uint8_t ERRIE = 15;           // ERRIE
    static const uint8_t LECIE = 11;           // LECIE
    static const uint8_t BOFIE = 10;           // BOFIE
    static const uint8_t EPVIE = 9;            // EPVIE
    static const uint8_t EWGIE = 8;            // EWGIE
    static const uint8_t FOVIE1 = 6;           // FOVIE1
    static const uint8_t FFIE1 = 5;            // FFIE1
    static const uint8_t FMPIE1 = 4;           // FMPIE1
    static const uint8_t FOVIE0 = 3;           // FOVIE0
    static const uint8_t FFIE0 = 2;            // FFIE0
    static const uint8_t FMPIE0 = 1;           // FMPIE0
    static const uint8_t TMEIE = 0;            // TMEIE
}

namespace CAN_ESR // CAN_ESR fields
{
    static const uint8_t REC = 24;             // REC (8 bits), Read-only
    static const uint8_t TEC = 16;             // TEC (8 bits), Read-only
    static const uint8_t LEC = 4;              // LEC (3 bits), Read-write
    static const uint8_t BOFF = 2;             // BOFF, Read-only
    static const uint8_t EPVF = 1;             // EPVF, Read-only
    static const uint8_t EWGF = 0;             // EWGF, Read-only
}

namespace CAN_BTR // CAN BTR fields
{
    static const uint8_t SILM = 31;            // SILM
    static const uint8_t LBKM = 30;            // LBKM
    static const uint8_t SJW = 24;             // SJW (2 bits)
    static const uint8_t TS2 = 20;             // TS2 (3 bits)
    static const uint8_t TS1 = 16;             // TS1 (4 bits)
    static const uint8_t BRP = 0;              // BRP (10 bits)
}

namespace CAN_TI0R // CAN_TI0R fields
{
    static const uint8_t STID = 21;            // STID (11 bits)
    static const uint8_t EXID = 3;             // EXID (18 bits)
    static const uint8_t IDE = 2;              // IDE
    static const uint8_t RTR = 1;              // RTR
    static const uint8_t TXRQ = 0;             // TXRQ
}

namespace CAN_TDT0R // CAN_TDT0R fields
{
    static const uint8_t TIME = 16;            // TIME (16 bits)
    static const uint8_t TGT = 8;              // TGT
    static const uint8_t DLC = 0;              // DLC (4 bits)
}

namespace CAN_TDL0R // CAN_TDL0R fields
{
    static const uint8_t DATA3 = 24;           // DATA3 (8 bits)
    static const uint8_t DATA2 = 16;           // DATA2 (8 bits)
    static const uint8_t DATA1 = 8;            // DATA1 (8 bits)
    static const uint8_t DATA0 = 0;            // DATA0 (8 bits)
}

namespace CAN_TDH0R // CAN_TDH0R fields
{
    static const uint8_t DATA7 = 24;           // DATA7 (8 bits)
    static const uint8_t DATA6 = 16;           // DATA6 (8 bits)
    static const uint8_t DATA5 = 8;            // DATA5 (8 bits)
    static const uint8_t DATA4 = 0;            // DATA4 (8 bits)
}

namespace CAN_TI1R // CAN_TI1R fields
{
    static const uint8_t STID = 21;            // STID (11 bits)
    static const uint8_t EXID = 3;             // EXID (18 bits)
    static const uint8_t IDE = 2;              // IDE
    static const uint8_t RTR = 1;              // RTR
    static const uint8_t TXRQ = 0;             // TXRQ
}

namespace CAN_TDT1R // CAN_TDT1R fields
{
    static const uint8_t TIME = 16;            // TIME (16 bits)
    static const uint8_t TGT = 8;              // TGT
    static const uint8_t DLC = 0;              // DLC (4 bits)
}

namespace CAN_TDL1R // CAN_TDL1R fields
{
    static const uint8_t DATA3 = 24;           // DATA3 (8 bits)
    static const uint8_t DATA2 = 16;           // DATA2 (8 bits)
    static const uint8_t DATA1 = 8;            // DATA1 (8 bits)
    static const uint8_t DATA0 = 0;            // DATA0 (8 bits)
}

namespace CAN_TDH1R // CAN_TDH1R fields
{
    static const uint8_t DATA7 = 24;           // DATA7 (8 bits)
    static const uint8_t DATA6 = 16;           // DATA6 (8 bits)
    static const uint8_t DATA5 = 8;            // DATA5 (8 bits)
    static const uint8_t DATA4 = 0;            // DATA4 (8 bits)
}

namespace CAN_TI2R // CAN_TI2R fields
{
    static const uint8_t STID = 21;            // STID (11 bits)
    static const uint8_t EXID = 3;             // EXID (18 bits)
    static const uint8_t IDE = 2;              // IDE
    static const uint8_t RTR = 1;              // RTR
    static const uint8_t TXRQ = 0;             // TXRQ
}

namespace CAN_TDT2R // CAN_TDT2R fields
{
    static const uint8_t TIME = 16;            // TIME (16 bits)
    static const uint8_t TGT = 8;              // TGT
    static const uint8_t DLC = 0;              // DLC (4 bits)
}

namespace CAN_TDL2R // CAN_TDL2R fields
{
    static const uint8_t DATA3 = 24;           // DATA3 (8 bits)
    static const uint8_t DATA2 = 16;           // DATA2 (8 bits)
    static const uint8_t DATA1 = 8;            // DATA1 (8 bits)
    static const uint8_t DATA0 = 0;            // DATA0 (8 bits)
}

namespace CAN_TDH2R // CAN_TDH2R fields
{
    static const uint8_t DATA7 = 24;           // DATA7 (8 bits)
    static const uint8_t DATA6 = 16;           // DATA6 (8 bits)
    static const uint8_t DATA5 = 8;            // DATA5 (8 bits)
    static const uint8_t DATA4 = 0;            // DATA4 (8 bits)
}

namespace CAN_RI0R // CAN_RI0R fields
{
    static const uint8_t STID = 21;            // STID (11 bits)
    static const uint8_t EXID = 3;             // EXID (18 bits)
    static const uint8_t IDE = 2;              // IDE
    static const uint8_t RTR = 1;              // RTR
}

namespace CAN_RDT0R // CAN_RDT0R fields
{
    static const uint8_t TIME = 16;            // TIME (16 bits)
    static const uint8_t FMI = 8;              // FMI (8 bits)
    static const uint8_t DLC = 0;              // DLC (4 bits)
}

namespace CAN_RDL0R // CAN_RDL0R fields
{
    static const uint8_t DATA3 = 24;           // DATA3 (8 bits)
    static const uint8_t DATA2 = 16;           // DATA2 (8 bits)
    static const uint8_t DATA1 = 8;            // DATA1 (8 bits)
    static const uint8_t DATA0 = 0;            // DATA0 (8 bits)
}

namespace CAN_RDH0R // CAN_RDH0R fields
{
    static const uint8_t DATA7 = 24;           // DATA7 (8 bits)
    static const uint8_t DATA6 = 16;           // DATA6 (8 bits)
    static const uint8_t DATA5 = 8;            // DATA5 (8 bits)
    static const uint8_t DATA4 = 0;            // DATA4 (8 bits)
}

namespace CAN_RI1R // CAN_RI1R fields
{
    static const uint8_t STID = 21;            // STID (11 bits)
    static const uint8_t EXID = 3;             // EXID (18 bits)
    static const uint8_t IDE = 2;              // IDE
    static const uint8_t RTR = 1;              // RTR
}

namespace CAN_RDT1R // CAN_RDT1R fields
{
    static const uint8_t TIME = 16;            // TIME (16 bits)
    static const uint8_t FMI = 8;              // FMI (8 bits)
    static const uint8_t DLC = 0;              // DLC (4 bits)
}

namespace CAN_RDL1R // CAN_RDL1R fields
{
    static const uint8_t DATA3 = 24;           // DATA3 (8 bits)
    static const uint8_t DATA2 = 16;           // DATA2 (8 bits)
    static const uint8_t DATA1 = 8;            // DATA1 (8 bits)
    static const uint8_t DATA0 = 0;            // DATA0 (8 bits)
}

namespace CAN_RDH1R // CAN_RDH1R fields
{
    static const uint8_t DATA7 = 24;           // DATA7 (8 bits)
    static const uint8_t DATA6 = 16;           // DATA6 (8 bits)
    static const uint8_t DATA5 = 8;            // DATA5 (8 bits)
    static const uint8_t DATA4 = 0;            // DATA4 (8 bits)
}

namespace CAN_FMR // CAN_FMR fields
{
    static const uint8_t CAN2SB = 8;           // CAN2SB (6 bits)
    static const uint8_t FINIT = 0;            // FINIT
}

namespace CAN_FM1R // CAN_FM1R fields
{
    static const uint8_t FBM0 = 0;             // Filter mode
    static const uint8_t FBM1 = 1;             // Filter mode
    static const uint8_t FBM2 = 2;             // Filter mode
    static const uint8_t FBM3 = 3;             // Filter mode
    static const uint8_t FBM4 = 4;             // Filter mode
    static const uint8_t FBM5 = 5;             // Filter mode
    static const uint8_t FBM6 = 6;             // Filter mode
    static const uint8_t FBM7 = 7;             // Filter mode
    static const uint8_t FBM8 = 8;             // Filter mode
    static const uint8_t FBM9 = 9;             // Filter mode
    static const uint8_t FBM10 = 10;           // Filter mode
    static const uint8_t FBM11 = 11;           // Filter mode
    static const uint8_t FBM12 = 12;           // Filter mode
    static const uint8_t FBM13 = 13;           // Filter mode
    static const uint8_t FBM14 = 14;           // Filter mode
    static const uint8_t FBM15 = 15;           // Filter mode
    static const uint8_t FBM16 = 16;           // Filter mode
    static const uint8_t FBM17 = 17;           // Filter mode
    static const uint8_t FBM18 = 18;           // Filter mode
    static const uint8_t FBM19 = 19;           // Filter mode
    static const uint8_t FBM20 = 20;           // Filter mode
    static const uint8_t FBM21 = 21;           // Filter mode
    static const uint8_t FBM22 = 22;           // Filter mode
    static const uint8_t FBM23 = 23;           // Filter mode
    static const uint8_t FBM24 = 24;           // Filter mode
    static const uint8_t FBM25 = 25;           // Filter mode
    static const uint8_t FBM26 = 26;           // Filter mode
    static const uint8_t FBM27 = 27;           // Filter mode
}

namespace CAN_FS1R // CAN_FS1R fields
{
    static const uint8_t FSC0 = 0;             // Filter scale configuration
    static const uint8_t FSC1 = 1;             // Filter scale configuration
    static const uint8_t FSC2 = 2;             // Filter scale configuration
    static const uint8_t FSC3 = 3;             // Filter scale configuration
    static const uint8_t FSC4 = 4;             // Filter scale configuration
    static const uint8_t FSC5 = 5;             // Filter scale configuration
    static const uint8_t FSC6 = 6;             // Filter scale configuration
    static const uint8_t FSC7 = 7;             // Filter scale configuration
    static const uint8_t FSC8 = 8;             // Filter scale configuration
    static const uint8_t FSC9 = 9;             // Filter scale configuration
    static const uint8_t FSC10 = 10;           // Filter scale configuration
    static const uint8_t FSC11 = 11;           // Filter scale configuration
    static const uint8_t FSC12 = 12;           // Filter scale configuration
    static const uint8_t FSC13 = 13;           // Filter scale configuration
    static const uint8_t FSC14 = 14;           // Filter scale configuration
    static const uint8_t FSC15 = 15;           // Filter scale configuration
    static const uint8_t FSC16 = 16;           // Filter scale configuration
    static const uint8_t FSC17 = 17;           // Filter scale configuration
    static const uint8_t FSC18 = 18;           // Filter scale configuration
    static const uint8_t FSC19 = 19;           // Filter scale configuration
    static const uint8_t FSC20 = 20;           // Filter scale configuration
    static const uint8_t FSC21 = 21;           // Filter scale configuration
    static const uint8_t FSC22 = 22;           // Filter scale configuration
    static const uint8_t FSC23 = 23;           // Filter scale configuration
    static const uint8_t FSC24 = 24;           // Filter scale configuration
    static const uint8_t FSC25 = 25;           // Filter scale configuration
    static const uint8_t FSC26 = 26;           // Filter scale configuration
    static const uint8_t FSC27 = 27;           // Filter scale configuration
}

namespace CAN_FFA1R // CAN_FFA1R fields
{
    static const uint8_t FFA0 = 0;             // Filter FIFO assignment for filter 0
    static const uint8_t FFA1 = 1;             // Filter FIFO assignment for filter 1
    static const uint8_t FFA2 = 2;             // Filter FIFO assignment for filter 2
    static const uint8_t FFA3 = 3;             // Filter FIFO assignment for filter 3
    static const uint8_t FFA4 = 4;             // Filter FIFO assignment for filter 4
    static const uint8_t FFA5 = 5;             // Filter FIFO assignment for filter 5
    static const uint8_t FFA6 = 6;             // Filter FIFO assignment for filter 6
    static const uint8_t FFA7 = 7;             // Filter FIFO assignment for filter 7
    static const uint8_t FFA8 = 8;             // Filter FIFO assignment for filter 8
    static const uint8_t FFA9 = 9;             // Filter FIFO assignment for filter 9
    static const uint8_t FFA10 = 10;           // Filter FIFO assignment for filter 10
    static const uint8_t FFA11 = 11;           // Filter FIFO assignment for filter 11
    static const uint8_t FFA12 = 12;           // Filter FIFO assignment for filter 12
    static const uint8_t FFA13 = 13;           // Filter FIFO assignment for filter 13
    static const uint8_t FFA14 = 14;           // Filter FIFO assignment for filter 14
    static const uint8_t FFA15 = 15;           // Filter FIFO assignment for filter 15
    static const uint8_t FFA16 = 16;           // Filter FIFO assignment for filter 16
    static const uint8_t FFA17 = 17;           // Filter FIFO assignment for filter 17
    static const uint8_t FFA18 = 18;           // Filter FIFO assignment for filter 18
    static const uint8_t FFA19 = 19;           // Filter FIFO assignment for filter 19
    static const uint8_t FFA20 = 20;           // Filter FIFO assignment for filter 20
    static const uint8_t FFA21 = 21;           // Filter FIFO assignment for filter 21
    static const uint8_t FFA22 = 22;           // Filter FIFO assignment for filter 22
    static const uint8_t FFA23 = 23;           // Filter FIFO assignment for filter 23
    static const uint8_t FFA24 = 24;           // Filter FIFO assignment for filter 24
    static const uint8_t FFA25 = 25;           // Filter FIFO assignment for filter 25
    static const uint8_t FFA26 = 26;           // Filter FIFO assignment for filter 26
    static const uint8_t FFA27 = 27;           // Filter FIFO assignment for filter 27
}

namespace CAN_FA1R // CAN_FA1R fields
{
    static const uint8_t FACT0 = 0;            // Filter active
    static const uint8_t FACT1 = 1;            // Filter active
    static const uint8_t FACT2 = 2;            // Filter active
    static const uint8_t FACT3 = 3;            // Filter active
    static const uint8_t FACT4 = 4;            // Filter active
    static const uint8_t FACT5 = 5;            // Filter active
    static const uint8_t FACT6 = 6;            // Filter active
    static const uint8_t FACT7 = 7;            // Filter active
    static const uint8_t FACT8 = 8;            // Filter active
    static const uint8_t FACT9 = 9;            // Filter active
    static const uint8_t FACT10 = 10;          // Filter active
    static const uint8_t FACT11 = 11;          // Filter active
    static const uint8_t FACT12 = 12;          // Filter active
    static const uint8_t FACT13 = 13;          // Filter active
    static const uint8_t FACT14 = 14;          // Filter active
    static const uint8_t FACT15 = 15;          // Filter active
    static const uint8_t FACT16 = 16;          // Filter active
    static const uint8_t FACT17 = 17;          // Filter active
    static const uint8_t FACT18 = 18;          // Filter active
    static const uint8_t FACT19 = 19;          // Filter active
    static const uint8_t FACT20 = 20;          // Filter active
    static const uint8_t FACT21 = 21;          // Filter active
    static const uint8_t FACT22 = 22;          // Filter active
    static const uint8_t FACT23 = 23;          // Filter active
    static const uint8_t FACT24 = 24;          // Filter active
    static const uint8_t FACT25 = 25;          // Filter active
    static const uint8_t FACT26 = 26;          // Filter active
    static const uint8_t FACT27 = 27;          // Filter active
}

namespace F0R1 // Filter bank 0 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F0R2 // Filter bank 0 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F1R1 // Filter bank 1 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F1R2 // Filter bank 1 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F2R1 // Filter bank 2 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F2R2 // Filter bank 2 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F3R1 // Filter bank 3 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F3R2 // Filter bank 3 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F4R1 // Filter bank 4 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F4R2 // Filter bank 4 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F5R1 // Filter bank 5 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F5R2 // Filter bank 5 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F6R1 // Filter bank 6 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F6R2 // Filter bank 6 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F7R1 // Filter bank 7 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F7R2 // Filter bank 7 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F8R1 // Filter bank 8 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F8R2 // Filter bank 8 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F9R1 // Filter bank 9 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F9R2 // Filter bank 9 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F10R1 // Filter bank 10 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F10R2 // Filter bank 10 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F11R1 // Filter bank 11 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F11R2 // Filter bank 11 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F12R1 // Filter bank 4 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F12R2 // Filter bank 12 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F13R1 // Filter bank 13 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F13R2 // Filter bank 13 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F14R1 // Filter bank 14 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F14R2 // Filter bank 14 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F15R1 // Filter bank 15 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F15R2 // Filter bank 15 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F16R1 // Filter bank 16 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F16R2 // Filter bank 16 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F17R1 // Filter bank 17 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F17R2 // Filter bank 17 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F18R1 // Filter bank 18 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F18R2 // Filter bank 18 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F19R1 // Filter bank 19 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F19R2 // Filter bank 19 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F20R1 // Filter bank 20 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F20R2 // Filter bank 20 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F21R1 // Filter bank 21 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F21R2 // Filter bank 21 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F22R1 // Filter bank 22 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F22R2 // Filter bank 22 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F23R1 // Filter bank 23 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F23R2 // Filter bank 23 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F24R1 // Filter bank 24 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F24R2 // Filter bank 24 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F25R1 // Filter bank 25 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F25R2 // Filter bank 25 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F26R1 // Filter bank 26 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F26R2 // Filter bank 26 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F27R1 // Filter bank 27 register 1 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

namespace F27R2 // Filter bank 27 register 2 fields
{
    static const uint8_t FB0 = 0;              // Filter bits
    static const uint8_t FB1 = 1;              // Filter bits
    static const uint8_t FB2 = 2;              // Filter bits
    static const uint8_t FB3 = 3;              // Filter bits
    static const uint8_t FB4 = 4;              // Filter bits
    static const uint8_t FB5 = 5;              // Filter bits
    static const uint8_t FB6 = 6;              // Filter bits
    static const uint8_t FB7 = 7;              // Filter bits
    static const uint8_t FB8 = 8;              // Filter bits
    static const uint8_t FB9 = 9;              // Filter bits
    static const uint8_t FB10 = 10;            // Filter bits
    static const uint8_t FB11 = 11;            // Filter bits
    static const uint8_t FB12 = 12;            // Filter bits
    static const uint8_t FB13 = 13;            // Filter bits
    static const uint8_t FB14 = 14;            // Filter bits
    static const uint8_t FB15 = 15;            // Filter bits
    static const uint8_t FB16 = 16;            // Filter bits
    static const uint8_t FB17 = 17;            // Filter bits
    static const uint8_t FB18 = 18;            // Filter bits
    static const uint8_t FB19 = 19;            // Filter bits
    static const uint8_t FB20 = 20;            // Filter bits
    static const uint8_t FB21 = 21;            // Filter bits
    static const uint8_t FB22 = 22;            // Filter bits
    static const uint8_t FB23 = 23;            // Filter bits
    static const uint8_t FB24 = 24;            // Filter bits
    static const uint8_t FB25 = 25;            // Filter bits
    static const uint8_t FB26 = 26;            // Filter bits
    static const uint8_t FB27 = 27;            // Filter bits
    static const uint8_t FB28 = 28;            // Filter bits
    static const uint8_t FB29 = 29;            // Filter bits
    static const uint8_t FB30 = 30;            // Filter bits
    static const uint8_t FB31 = 31;            // Filter bits
}

}

////
//
//    Digital-to-analog converter
//
////

namespace dac
{

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
};

dac_t& DAC = *reinterpret_cast<dac_t*>(0x40007400);

namespace CR // control register fields
{
    static const uint8_t EN1 = 0;              // DAC channel1 enable
    static const uint8_t BOFF1 = 1;            // DAC channel1 output buffer disable
    static const uint8_t TEN1 = 2;             // DAC channel1 trigger enable
    static const uint8_t TSEL1 = 3;            // DAC channel1 trigger selection (3 bits)
    static const uint8_t WAVE1 = 6;            // DAC channel1 noise/triangle wave generation enable (2 bits)
    static const uint8_t MAMP1 = 8;            // DAC channel1 mask/amplitude selector (4 bits)
    static const uint8_t DMAEN1 = 12;          // DAC channel1 DMA enable
    static const uint8_t DMAUDRIE1 = 13;       // DAC channel1 DMA Underrun Interrupt enable
    static const uint8_t EN2 = 16;             // DAC channel2 enable
    static const uint8_t BOFF2 = 17;           // DAC channel2 output buffer disable
    static const uint8_t TEN2 = 18;            // DAC channel2 trigger enable
    static const uint8_t TSEL2 = 19;           // DAC channel2 trigger selection (3 bits)
    static const uint8_t WAVE2 = 22;           // DAC channel2 noise/triangle wave generation enable (2 bits)
    static const uint8_t MAMP2 = 24;           // DAC channel2 mask/amplitude selector (4 bits)
    static const uint8_t DMAEN2 = 28;          // DAC channel2 DMA enable
    static const uint8_t DMAUDRIE2 = 29;       // DAC channel2 DMA underrun interrupt enable
}

namespace SWTRIGR // software trigger register fields
{
    static const uint8_t SWTRIG1 = 0;          // DAC channel1 software trigger
    static const uint8_t SWTRIG2 = 1;          // DAC channel2 software trigger
}

namespace DHR12R1 // channel1 12-bit right-aligned data holding register fields
{
    static const uint8_t DACC1DHR = 0;         // DAC channel1 12-bit right-aligned data (12 bits)
}

namespace DHR12L1 // channel1 12-bit left aligned data holding register fields
{
    static const uint8_t DACC1DHR = 4;         // DAC channel1 12-bit left-aligned data (12 bits)
}

namespace DHR8R1 // channel1 8-bit right aligned data holding register fields
{
    static const uint8_t DACC1DHR = 0;         // DAC channel1 8-bit right-aligned data (8 bits)
}

namespace DOR1 // channel1 data output register fields
{
    static const uint8_t DACC1DOR = 0;         // DAC channel1 data output (12 bits)
}

namespace SR // status register fields
{
    static const uint8_t DMAUDR2 = 29;         // DAC channel2 DMA underrun flag
    static const uint8_t DMAUDR1 = 13;         // DAC channel1 DMA underrun flag
}

namespace DHR12R2 // DAC channel2 12-bit right-aligned data holding register fields
{
    static const uint8_t DACC2DHR = 0;         // DAC channel2 12-bit right-aligned data (12 bits)
}

namespace DHR12L2 // DAC channel2 12-bit left-aligned data holding register fields
{
    static const uint8_t DACC2DHR = 4;         // DAC channel2 12-bit left-aligned data (12 bits)
}

namespace DHR8R2 // DAC channel2 8-bit right-aligned data holding register fields
{
    static const uint8_t DACC2DHR = 0;         // DAC channel2 8-bit right-aligned data (8 bits)
}

namespace DHR12RD // DHR12RD fields
{
    static const uint8_t DACC1DHR = 0;         // DAC channel1 12-bit right-aligned data (12 bits)
    static const uint8_t DACC2DHR = 16;        // DAC channel2 12-bit right-aligned data (12 bits)
}

namespace DHR12LD // Dual DAC 12-bit left-aligned data holding register fields
{
    static const uint8_t DACC1DHR = 4;         // DAC channel1 12-bit left-aligned data (12 bits)
    static const uint8_t DACC2DHR = 20;        // DAC channel2 12-bit left-aligned data (12 bits)
}

namespace DHR8RD // Dual DAC 8-bit right-aligned data holding register fields
{
    static const uint8_t DACC2DHR = 8;         // DAC channel2 8-bit right-aligned data (8 bits)
    static const uint8_t DACC1DHR = 0;         // DAC channel1 8-bit right-aligned data (8 bits)
}

namespace DOR2 // DAC channel2 data output register fields
{
    static const uint8_t DACC2DOR = 0;         // DAC channel2 data output (12 bits)
}

}

////
//
//    System control block
//
////

namespace scb
{

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
};

scb_t& SCB = *reinterpret_cast<scb_t*>(0xe000ed00);

namespace CPUID // CPUID base register fields
{
    static const uint8_t Revision = 0;         // Revision number (4 bits)
    static const uint8_t PartNo = 4;           // Part number of the processor (12 bits)
    static const uint8_t Constant = 16;        // Reads as 0xF (4 bits)
    static const uint8_t Variant = 20;         // Variant number (4 bits)
    static const uint8_t Implementer = 24;     // Implementer code (8 bits)
}

namespace ICSR // Interrupt control and state register fields
{
    static const uint8_t VECTACTIVE = 0;       // Active vector (6 bits)
    static const uint8_t VECTPENDING = 12;     // Pending vector (6 bits)
    static const uint8_t ISRPENDING = 22;      // Interrupt pending flag
    static const uint8_t PENDSTCLR = 25;       // SysTick exception clear-pending bit
    static const uint8_t PENDSTSET = 26;       // SysTick exception set-pending bit
    static const uint8_t PENDSVCLR = 27;       // PendSV clear-pending bit
    static const uint8_t PENDSVSET = 28;       // PendSV set-pending bit
    static const uint8_t NMIPENDSET = 31;      // NMI set-pending bit.
}

namespace AIRCR // Application interrupt and reset control register fields
{
    static const uint8_t VECTCLRACTIVE = 1;    // VECTCLRACTIVE
    static const uint8_t SYSRESETREQ = 2;      // SYSRESETREQ
    static const uint8_t ENDIANESS = 15;       // ENDIANESS
    static const uint8_t VECTKEYSTAT = 16;     // Register key (16 bits)
}

namespace SCR // System control register fields
{
    static const uint8_t SLEEPONEXIT = 1;      // SLEEPONEXIT
    static const uint8_t SLEEPDEEP = 2;        // SLEEPDEEP
    static const uint8_t SEVEONPEND = 4;       // Send Event on Pending bit
}

namespace CCR // Configuration and control register fields
{
    static const uint8_t UNALIGN__TRP = 3;     // UNALIGN_ TRP
    static const uint8_t STKALIGN = 9;         // STKALIGN
}

namespace SHPR2 // System handler priority registers fields
{
    static const uint8_t PRI_11 = 24;          // Priority of system handler 11 (8 bits)
}

namespace SHPR3 // System handler priority registers fields
{
    static const uint8_t PRI_14 = 16;          // Priority of system handler 14 (8 bits)
    static const uint8_t PRI_15 = 24;          // Priority of system handler 15 (8 bits)
}

}

////
//
//    SysTick timer
//
////

namespace stk
{

struct stk_t
{
    volatile uint32_t    CSR;                  // [Read-write] SysTick control and status register
    volatile uint32_t    RVR;                  // [Read-write] SysTick reload value register
    volatile uint32_t    CVR;                  // [Read-write] SysTick current value register
    volatile uint32_t    CALIB;                // [Read-write] SysTick calibration value register
};

stk_t& STK = *reinterpret_cast<stk_t*>(0xe000e010);

namespace CSR // SysTick control and status register fields
{
    static const uint8_t ENABLE = 0;           // Counter enable
    static const uint8_t TICKINT = 1;          // SysTick exception request enable
    static const uint8_t CLKSOURCE = 2;        // Clock source selection
    static const uint8_t COUNTFLAG = 16;       // COUNTFLAG
}

namespace RVR // SysTick reload value register fields
{
    static const uint8_t RELOAD = 0;           // RELOAD value (24 bits)
}

namespace CVR // SysTick current value register fields
{
    static const uint8_t CURRENT = 0;          // Current counter value (24 bits)
}

namespace CALIB // SysTick calibration value register fields
{
    static const uint8_t TENMS = 0;            // Calibration value (24 bits)
    static const uint8_t SKEW = 30;            // SKEW flag: Indicates whether the TENMS value is exact
    static const uint8_t NOREF = 31;           // NOREF flag. Reads as zero
}

}

}


