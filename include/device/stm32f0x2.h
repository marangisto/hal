#pragma once

#include <stdint.h>

////
//
//    STM32F0x2
//
//       schema-version : 1.1
//       vendor         : 
//       series         : 
//       device-version : 1.3
//       address-unit   : 8 bits
//       device-width   : 32
//       device-size    : 32
//
////

namespace stm32f0x2
{

template<int N> class reserved_t { private: uint32_t m_pad[N]; };

template<uint8_t POS, uint32_t MASK>
struct bit_field_t
{
    template <uint32_t X>
    static constexpr uint32_t value()
    {
        static_assert((X & ~MASK) == 0, "field value too large");
        return X << POS;
    }
};

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


    static const uint32_t DR_RESET_VALUE = 0xffffffff;

    template<uint32_t X>
    static constexpr uint32_t IDR_IDR =                 // General-purpose 8-bit data register bits (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_RESET = 0x1;          // reset bit
    template<uint32_t X>
    static constexpr uint32_t CR_POLYSIZE =            // Polynomial size (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_REV_IN =              // Reverse input data (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t CR_REV_OUT = 0x80;       // Reverse output data
    static const uint32_t CR_RESET_VALUE = 0x0;


    static const uint32_t INIT_RESET_VALUE = 0xffffffff;
};

static crc_t& CRC = *reinterpret_cast<crc_t*>(0x40023000);

#define HAVE_PERIPHERAL_CRC


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

    template<uint32_t X>
    static constexpr uint32_t MODER_MODER15 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER14 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER13 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER12 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER11 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER10 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER9 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER8 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER7 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER6 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER5 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER4 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER3 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER2 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER1 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER0 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t MODER_RESET_VALUE = 0x0;

    static constexpr uint32_t OTYPER_OT15 = 0x8000;        // Port x configuration bit 15
    static constexpr uint32_t OTYPER_OT14 = 0x4000;        // Port x configuration bit 14
    static constexpr uint32_t OTYPER_OT13 = 0x2000;        // Port x configuration bit 13
    static constexpr uint32_t OTYPER_OT12 = 0x1000;        // Port x configuration bit 12
    static constexpr uint32_t OTYPER_OT11 = 0x800;         // Port x configuration bit 11
    static constexpr uint32_t OTYPER_OT10 = 0x400;         // Port x configuration bit 10
    static constexpr uint32_t OTYPER_OT9 = 0x200;          // Port x configuration bit 9
    static constexpr uint32_t OTYPER_OT8 = 0x100;          // Port x configuration bit 8
    static constexpr uint32_t OTYPER_OT7 = 0x80;           // Port x configuration bit 7
    static constexpr uint32_t OTYPER_OT6 = 0x40;           // Port x configuration bit 6
    static constexpr uint32_t OTYPER_OT5 = 0x20;           // Port x configuration bit 5
    static constexpr uint32_t OTYPER_OT4 = 0x10;           // Port x configuration bit 4
    static constexpr uint32_t OTYPER_OT3 = 0x8;            // Port x configuration bit 3
    static constexpr uint32_t OTYPER_OT2 = 0x4;            // Port x configuration bit 2
    static constexpr uint32_t OTYPER_OT1 = 0x2;            // Port x configuration bit 1
    static constexpr uint32_t OTYPER_OT0 = 0x1;            // Port x configuration bit 0
    static const uint32_t OTYPER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR15 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR14 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR13 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR12 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR11 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR10 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR9 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR8 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR7 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR6 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR5 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR4 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR3 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR2 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR1 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR0 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t OSPEEDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR15 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR14 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR13 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR12 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR11 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR10 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR9 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR8 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR7 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR6 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR5 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR4 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR3 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR2 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR1 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR0 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t PUPDR_RESET_VALUE = 0x0;

    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data (y = 0..15)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data (y = 0..15)
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS0 = 0x1;            // Port x set bit y (y= 0..15)
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Port x lock bit y
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port x lock bit y (y= 0..15)
    static const uint32_t LCKR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL7 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL6 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL5 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL4 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL3 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL2 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL1 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL0 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH15 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH14 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH13 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH12 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH11 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH10 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH9 =               // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH8 =               // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR1 = 0x2;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR2 = 0x4;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR3 = 0x8;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR4 = 0x10;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR5 = 0x20;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR6 = 0x40;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR7 = 0x80;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR8 = 0x100;          // Port x Reset bit y
    static constexpr uint32_t BRR_BR9 = 0x200;          // Port x Reset bit y
    static constexpr uint32_t BRR_BR10 = 0x400;         // Port x Reset bit y
    static constexpr uint32_t BRR_BR11 = 0x800;         // Port x Reset bit y
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Port x Reset bit y
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpiof_t& GPIOF = *reinterpret_cast<gpiof_t*>(0x48001400);

#define HAVE_PERIPHERAL_GPIOF


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

    template<uint32_t X>
    static constexpr uint32_t MODER_MODER15 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER14 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER13 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER12 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER11 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER10 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER9 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER8 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER7 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER6 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER5 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER4 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER3 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER2 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER1 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER0 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t MODER_RESET_VALUE = 0x0;

    static constexpr uint32_t OTYPER_OT15 = 0x8000;        // Port x configuration bit 15
    static constexpr uint32_t OTYPER_OT14 = 0x4000;        // Port x configuration bit 14
    static constexpr uint32_t OTYPER_OT13 = 0x2000;        // Port x configuration bit 13
    static constexpr uint32_t OTYPER_OT12 = 0x1000;        // Port x configuration bit 12
    static constexpr uint32_t OTYPER_OT11 = 0x800;         // Port x configuration bit 11
    static constexpr uint32_t OTYPER_OT10 = 0x400;         // Port x configuration bit 10
    static constexpr uint32_t OTYPER_OT9 = 0x200;          // Port x configuration bit 9
    static constexpr uint32_t OTYPER_OT8 = 0x100;          // Port x configuration bit 8
    static constexpr uint32_t OTYPER_OT7 = 0x80;           // Port x configuration bit 7
    static constexpr uint32_t OTYPER_OT6 = 0x40;           // Port x configuration bit 6
    static constexpr uint32_t OTYPER_OT5 = 0x20;           // Port x configuration bit 5
    static constexpr uint32_t OTYPER_OT4 = 0x10;           // Port x configuration bit 4
    static constexpr uint32_t OTYPER_OT3 = 0x8;            // Port x configuration bit 3
    static constexpr uint32_t OTYPER_OT2 = 0x4;            // Port x configuration bit 2
    static constexpr uint32_t OTYPER_OT1 = 0x2;            // Port x configuration bit 1
    static constexpr uint32_t OTYPER_OT0 = 0x1;            // Port x configuration bit 0
    static const uint32_t OTYPER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR15 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR14 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR13 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR12 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR11 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR10 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR9 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR8 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR7 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR6 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR5 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR4 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR3 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR2 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR1 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR0 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t OSPEEDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR15 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR14 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR13 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR12 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR11 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR10 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR9 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR8 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR7 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR6 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR5 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR4 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR3 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR2 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR1 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR0 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t PUPDR_RESET_VALUE = 0x0;

    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data (y = 0..15)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data (y = 0..15)
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS0 = 0x1;            // Port x set bit y (y= 0..15)
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Port x lock bit y
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port x lock bit y (y= 0..15)
    static const uint32_t LCKR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL7 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL6 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL5 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL4 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL3 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL2 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL1 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL0 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH15 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH14 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH13 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH12 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH11 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH10 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH9 =               // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH8 =               // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR1 = 0x2;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR2 = 0x4;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR3 = 0x8;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR4 = 0x10;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR5 = 0x20;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR6 = 0x40;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR7 = 0x80;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR8 = 0x100;          // Port x Reset bit y
    static constexpr uint32_t BRR_BR9 = 0x200;          // Port x Reset bit y
    static constexpr uint32_t BRR_BR10 = 0x400;         // Port x Reset bit y
    static constexpr uint32_t BRR_BR11 = 0x800;         // Port x Reset bit y
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Port x Reset bit y
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpiod_t& GPIOD = *reinterpret_cast<gpiod_t*>(0x48000c00);

#define HAVE_PERIPHERAL_GPIOD


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

    template<uint32_t X>
    static constexpr uint32_t MODER_MODER15 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER14 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER13 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER12 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER11 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER10 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER9 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER8 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER7 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER6 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER5 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER4 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER3 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER2 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER1 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER0 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t MODER_RESET_VALUE = 0x0;

    static constexpr uint32_t OTYPER_OT15 = 0x8000;        // Port x configuration bit 15
    static constexpr uint32_t OTYPER_OT14 = 0x4000;        // Port x configuration bit 14
    static constexpr uint32_t OTYPER_OT13 = 0x2000;        // Port x configuration bit 13
    static constexpr uint32_t OTYPER_OT12 = 0x1000;        // Port x configuration bit 12
    static constexpr uint32_t OTYPER_OT11 = 0x800;         // Port x configuration bit 11
    static constexpr uint32_t OTYPER_OT10 = 0x400;         // Port x configuration bit 10
    static constexpr uint32_t OTYPER_OT9 = 0x200;          // Port x configuration bit 9
    static constexpr uint32_t OTYPER_OT8 = 0x100;          // Port x configuration bit 8
    static constexpr uint32_t OTYPER_OT7 = 0x80;           // Port x configuration bit 7
    static constexpr uint32_t OTYPER_OT6 = 0x40;           // Port x configuration bit 6
    static constexpr uint32_t OTYPER_OT5 = 0x20;           // Port x configuration bit 5
    static constexpr uint32_t OTYPER_OT4 = 0x10;           // Port x configuration bit 4
    static constexpr uint32_t OTYPER_OT3 = 0x8;            // Port x configuration bit 3
    static constexpr uint32_t OTYPER_OT2 = 0x4;            // Port x configuration bit 2
    static constexpr uint32_t OTYPER_OT1 = 0x2;            // Port x configuration bit 1
    static constexpr uint32_t OTYPER_OT0 = 0x1;            // Port x configuration bit 0
    static const uint32_t OTYPER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR15 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR14 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR13 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR12 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR11 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR10 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR9 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR8 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR7 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR6 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR5 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR4 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR3 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR2 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR1 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR0 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t OSPEEDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR15 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR14 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR13 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR12 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR11 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR10 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR9 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR8 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR7 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR6 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR5 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR4 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR3 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR2 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR1 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR0 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t PUPDR_RESET_VALUE = 0x0;

    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data (y = 0..15)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data (y = 0..15)
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS0 = 0x1;            // Port x set bit y (y= 0..15)
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Port x lock bit y
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port x lock bit y (y= 0..15)
    static const uint32_t LCKR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL7 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL6 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL5 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL4 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL3 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL2 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL1 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL0 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH15 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH14 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH13 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH12 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH11 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH10 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH9 =               // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH8 =               // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR1 = 0x2;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR2 = 0x4;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR3 = 0x8;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR4 = 0x10;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR5 = 0x20;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR6 = 0x40;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR7 = 0x80;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR8 = 0x100;          // Port x Reset bit y
    static constexpr uint32_t BRR_BR9 = 0x200;          // Port x Reset bit y
    static constexpr uint32_t BRR_BR10 = 0x400;         // Port x Reset bit y
    static constexpr uint32_t BRR_BR11 = 0x800;         // Port x Reset bit y
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Port x Reset bit y
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpioc_t& GPIOC = *reinterpret_cast<gpioc_t*>(0x48000800);

#define HAVE_PERIPHERAL_GPIOC


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

    template<uint32_t X>
    static constexpr uint32_t MODER_MODER15 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER14 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER13 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER12 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER11 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER10 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER9 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER8 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER7 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER6 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER5 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER4 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER3 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER2 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER1 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER0 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t MODER_RESET_VALUE = 0x0;

    static constexpr uint32_t OTYPER_OT15 = 0x8000;        // Port x configuration bit 15
    static constexpr uint32_t OTYPER_OT14 = 0x4000;        // Port x configuration bit 14
    static constexpr uint32_t OTYPER_OT13 = 0x2000;        // Port x configuration bit 13
    static constexpr uint32_t OTYPER_OT12 = 0x1000;        // Port x configuration bit 12
    static constexpr uint32_t OTYPER_OT11 = 0x800;         // Port x configuration bit 11
    static constexpr uint32_t OTYPER_OT10 = 0x400;         // Port x configuration bit 10
    static constexpr uint32_t OTYPER_OT9 = 0x200;          // Port x configuration bit 9
    static constexpr uint32_t OTYPER_OT8 = 0x100;          // Port x configuration bit 8
    static constexpr uint32_t OTYPER_OT7 = 0x80;           // Port x configuration bit 7
    static constexpr uint32_t OTYPER_OT6 = 0x40;           // Port x configuration bit 6
    static constexpr uint32_t OTYPER_OT5 = 0x20;           // Port x configuration bit 5
    static constexpr uint32_t OTYPER_OT4 = 0x10;           // Port x configuration bit 4
    static constexpr uint32_t OTYPER_OT3 = 0x8;            // Port x configuration bit 3
    static constexpr uint32_t OTYPER_OT2 = 0x4;            // Port x configuration bit 2
    static constexpr uint32_t OTYPER_OT1 = 0x2;            // Port x configuration bit 1
    static constexpr uint32_t OTYPER_OT0 = 0x1;            // Port x configuration bit 0
    static const uint32_t OTYPER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR15 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR14 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR13 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR12 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR11 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR10 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR9 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR8 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR7 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR6 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR5 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR4 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR3 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR2 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR1 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR0 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t OSPEEDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR15 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR14 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR13 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR12 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR11 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR10 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR9 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR8 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR7 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR6 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR5 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR4 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR3 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR2 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR1 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR0 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t PUPDR_RESET_VALUE = 0x0;

    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data (y = 0..15)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data (y = 0..15)
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS0 = 0x1;            // Port x set bit y (y= 0..15)
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Port x lock bit y
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port x lock bit y (y= 0..15)
    static const uint32_t LCKR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL7 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL6 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL5 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL4 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL3 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL2 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL1 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL0 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH15 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH14 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH13 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH12 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH11 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH10 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH9 =               // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH8 =               // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR1 = 0x2;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR2 = 0x4;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR3 = 0x8;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR4 = 0x10;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR5 = 0x20;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR6 = 0x40;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR7 = 0x80;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR8 = 0x100;          // Port x Reset bit y
    static constexpr uint32_t BRR_BR9 = 0x200;          // Port x Reset bit y
    static constexpr uint32_t BRR_BR10 = 0x400;         // Port x Reset bit y
    static constexpr uint32_t BRR_BR11 = 0x800;         // Port x Reset bit y
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Port x Reset bit y
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpiob_t& GPIOB = *reinterpret_cast<gpiob_t*>(0x48000400);

#define HAVE_PERIPHERAL_GPIOB


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

    template<uint32_t X>
    static constexpr uint32_t MODER_MODER15 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER14 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER13 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER12 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER11 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER10 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER9 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER8 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER7 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER6 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER5 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER4 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER3 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER2 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER1 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER0 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t MODER_RESET_VALUE = 0x0;

    static constexpr uint32_t OTYPER_OT15 = 0x8000;        // Port x configuration bit 15
    static constexpr uint32_t OTYPER_OT14 = 0x4000;        // Port x configuration bit 14
    static constexpr uint32_t OTYPER_OT13 = 0x2000;        // Port x configuration bit 13
    static constexpr uint32_t OTYPER_OT12 = 0x1000;        // Port x configuration bit 12
    static constexpr uint32_t OTYPER_OT11 = 0x800;         // Port x configuration bit 11
    static constexpr uint32_t OTYPER_OT10 = 0x400;         // Port x configuration bit 10
    static constexpr uint32_t OTYPER_OT9 = 0x200;          // Port x configuration bit 9
    static constexpr uint32_t OTYPER_OT8 = 0x100;          // Port x configuration bit 8
    static constexpr uint32_t OTYPER_OT7 = 0x80;           // Port x configuration bit 7
    static constexpr uint32_t OTYPER_OT6 = 0x40;           // Port x configuration bit 6
    static constexpr uint32_t OTYPER_OT5 = 0x20;           // Port x configuration bit 5
    static constexpr uint32_t OTYPER_OT4 = 0x10;           // Port x configuration bit 4
    static constexpr uint32_t OTYPER_OT3 = 0x8;            // Port x configuration bit 3
    static constexpr uint32_t OTYPER_OT2 = 0x4;            // Port x configuration bit 2
    static constexpr uint32_t OTYPER_OT1 = 0x2;            // Port x configuration bit 1
    static constexpr uint32_t OTYPER_OT0 = 0x1;            // Port x configuration bit 0
    static const uint32_t OTYPER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR15 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR14 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR13 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR12 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR11 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR10 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR9 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR8 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR7 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR6 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR5 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR4 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR3 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR2 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR1 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR0 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t OSPEEDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR15 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR14 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR13 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR12 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR11 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR10 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR9 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR8 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR7 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR6 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR5 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR4 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR3 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR2 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR1 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR0 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t PUPDR_RESET_VALUE = 0x0;

    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data (y = 0..15)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data (y = 0..15)
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS0 = 0x1;            // Port x set bit y (y= 0..15)
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Port x lock bit y
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port x lock bit y (y= 0..15)
    static const uint32_t LCKR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL7 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL6 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL5 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL4 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL3 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL2 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL1 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL0 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH15 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH14 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH13 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH12 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH11 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH10 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH9 =               // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH8 =               // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR1 = 0x2;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR2 = 0x4;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR3 = 0x8;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR4 = 0x10;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR5 = 0x20;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR6 = 0x40;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR7 = 0x80;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR8 = 0x100;          // Port x Reset bit y
    static constexpr uint32_t BRR_BR9 = 0x200;          // Port x Reset bit y
    static constexpr uint32_t BRR_BR10 = 0x400;         // Port x Reset bit y
    static constexpr uint32_t BRR_BR11 = 0x800;         // Port x Reset bit y
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Port x Reset bit y
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpioe_t& GPIOE = *reinterpret_cast<gpioe_t*>(0x48001000);

#define HAVE_PERIPHERAL_GPIOE


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

    template<uint32_t X>
    static constexpr uint32_t MODER_MODER15 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER14 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER13 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER12 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER11 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER10 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER9 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER8 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER7 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER6 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER5 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER4 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER3 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER2 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER1 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MODER_MODER0 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t MODER_RESET_VALUE = 0x28000000;

    static constexpr uint32_t OTYPER_OT15 = 0x8000;        // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT14 = 0x4000;        // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT13 = 0x2000;        // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT12 = 0x1000;        // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT11 = 0x800;         // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT10 = 0x400;         // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT9 = 0x200;          // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT8 = 0x100;          // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT7 = 0x80;           // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT6 = 0x40;           // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT5 = 0x20;           // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT4 = 0x10;           // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT3 = 0x8;            // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT2 = 0x4;            // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT1 = 0x2;            // Port x configuration bits (y = 0..15)
    static constexpr uint32_t OTYPER_OT0 = 0x1;            // Port x configuration bits (y = 0..15)
    static const uint32_t OTYPER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR15 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR14 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR13 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR12 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR11 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR10 =           // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR9 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR8 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR7 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR6 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR5 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR4 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR3 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR2 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR1 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OSPEEDR_OSPEEDR0 =            // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t OSPEEDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR15 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR14 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR13 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR12 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR11 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR10 =             // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR9 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR8 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR7 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR6 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR5 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR4 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR3 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR2 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR1 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PUPDR_PUPDR0 =              // Port x configuration bits (y = 0..15) (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t PUPDR_RESET_VALUE = 0x24000000;

    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data (y = 0..15)
    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data (y = 0..15)
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data (y = 0..15)
    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data (y = 0..15)
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Port x reset bit y (y = 0..15)
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Port x set bit y (y= 0..15)
    static constexpr uint32_t BSRR_BS0 = 0x1;            // Port x set bit y (y= 0..15)
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port x lock bit y (y= 0..15)
    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port x lock bit y (y= 0..15)
    static const uint32_t LCKR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL7 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL6 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL5 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL4 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL3 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL2 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL1 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFRL0 =               // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH15 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH14 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH13 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH12 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH11 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH10 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH9 =               // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFRH8 =               // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR1 = 0x2;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR2 = 0x4;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR3 = 0x8;            // Port x Reset bit y
    static constexpr uint32_t BRR_BR4 = 0x10;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR5 = 0x20;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR6 = 0x40;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR7 = 0x80;           // Port x Reset bit y
    static constexpr uint32_t BRR_BR8 = 0x100;          // Port x Reset bit y
    static constexpr uint32_t BRR_BR9 = 0x200;          // Port x Reset bit y
    static constexpr uint32_t BRR_BR10 = 0x400;         // Port x Reset bit y
    static constexpr uint32_t BRR_BR11 = 0x800;         // Port x Reset bit y
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Port x Reset bit y
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Port x Reset bit y
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpioa_t& GPIOA = *reinterpret_cast<gpioa_t*>(0x48000000);

#define HAVE_PERIPHERAL_GPIOA


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

    static constexpr uint32_t CR1_BIDIMODE = 0x8000;    // Bidirectional data mode enable
    static constexpr uint32_t CR1_BIDIOE = 0x4000;      // Output enable in bidirectional mode
    static constexpr uint32_t CR1_CRCEN = 0x2000;       // Hardware CRC calculation enable
    static constexpr uint32_t CR1_CRCNEXT = 0x1000;     // CRC transfer next
    static constexpr uint32_t CR1_DFF = 0x800;          // Data frame format
    static constexpr uint32_t CR1_RXONLY = 0x400;       // Receive only
    static constexpr uint32_t CR1_SSM = 0x200;          // Software slave management
    static constexpr uint32_t CR1_SSI = 0x100;          // Internal slave select
    static constexpr uint32_t CR1_LSBFIRST = 0x80;      // Frame format
    static constexpr uint32_t CR1_SPE = 0x40;           // SPI enable
    template<uint32_t X>
    static constexpr uint32_t CR1_BR =                  // Baud rate control (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    static constexpr uint32_t CR1_MSTR = 0x4;           // Master selection
    static constexpr uint32_t CR1_CPOL = 0x2;           // Clock polarity
    static constexpr uint32_t CR1_CPHA = 0x1;           // Clock phase
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_RXDMAEN = 0x1;        // Rx buffer DMA enable
    static constexpr uint32_t CR2_TXDMAEN = 0x2;        // Tx buffer DMA enable
    static constexpr uint32_t CR2_SSOE = 0x4;           // SS output enable
    static constexpr uint32_t CR2_NSSP = 0x8;           // NSS pulse management
    static constexpr uint32_t CR2_FRF = 0x10;           // Frame format
    static constexpr uint32_t CR2_ERRIE = 0x20;         // Error interrupt enable
    static constexpr uint32_t CR2_RXNEIE = 0x40;        // RX buffer not empty interrupt enable
    static constexpr uint32_t CR2_TXEIE = 0x80;         // Tx buffer empty interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR2_DS =                  // Data size (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t CR2_FRXTH = 0x1000;       // FIFO reception threshold
    static constexpr uint32_t CR2_LDMA_RX = 0x2000;     // Last DMA transfer for reception
    static constexpr uint32_t CR2_LDMA_TX = 0x4000;     // Last DMA transfer for transmission
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_RXNE = 0x1;           // Receive buffer not empty, Read-only
    static constexpr uint32_t SR_TXE = 0x2;            // Transmit buffer empty, Read-only
    static constexpr uint32_t SR_CHSIDE = 0x4;         // Channel side, Read-only
    static constexpr uint32_t SR_UDR = 0x8;            // Underrun flag, Read-only
    static constexpr uint32_t SR_CRCERR = 0x10;        // CRC error flag, Read-write
    static constexpr uint32_t SR_MODF = 0x20;          // Mode fault, Read-only
    static constexpr uint32_t SR_OVR = 0x40;           // Overrun flag, Read-only
    static constexpr uint32_t SR_BSY = 0x80;           // Busy flag, Read-only
    static constexpr uint32_t SR_TIFRFE = 0x100;       // TI frame format error, Read-only
    template<uint32_t X>
    static constexpr uint32_t SR_FRLVL =               // FIFO reception level (2 bits), Read-only
        bit_field_t<9, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SR_FTLVL =               // FIFO transmission level (2 bits), Read-only
        bit_field_t<11, 0x3>::value<X>();
    static const uint32_t SR_RESET_VALUE = 0x2;

    template<uint32_t X>
    static constexpr uint32_t DR_DR =                  // Data register (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CRCPR_CRCPOLY =             // CRC polynomial register (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CRCPR_RESET_VALUE = 0x7;

    template<uint32_t X>
    static constexpr uint32_t RXCRCR_RxCRC =               // Rx CRC register (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t RXCRCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXCRCR_TxCRC =               // Tx CRC register (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t TXCRCR_RESET_VALUE = 0x0;

    static constexpr uint32_t I2SCFGR_I2SMOD = 0x800;       // I2S mode selection
    static constexpr uint32_t I2SCFGR_I2SE = 0x400;         // I2S Enable
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SCFG =              // I2S configuration mode (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_PCMSYNC = 0x80;       // PCM frame synchronization
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SSTD =              // I2S standard selection (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_CKPOL = 0x8;          // Steady state clock polarity
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_DATLEN =              // Data length to be transferred (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_CHLEN = 0x1;          // Channel length (number of bits per audio channel)
    static const uint32_t I2SCFGR_RESET_VALUE = 0x0;

    static constexpr uint32_t I2SPR_MCKOE = 0x200;        // Master clock output enable
    static constexpr uint32_t I2SPR_ODD = 0x100;          // Odd factor for the prescaler
    template<uint32_t X>
    static constexpr uint32_t I2SPR_I2SDIV =              // I2S Linear prescaler (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t I2SPR_RESET_VALUE = 0x10;

    static constexpr uint8_t SPI1 = 25; // SPI1_global_interrupt
};

static spi1_t& SPI1 = *reinterpret_cast<spi1_t*>(0x40013000);

#define HAVE_PERIPHERAL_SPI1


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

    static constexpr uint32_t CR1_BIDIMODE = 0x8000;    // Bidirectional data mode enable
    static constexpr uint32_t CR1_BIDIOE = 0x4000;      // Output enable in bidirectional mode
    static constexpr uint32_t CR1_CRCEN = 0x2000;       // Hardware CRC calculation enable
    static constexpr uint32_t CR1_CRCNEXT = 0x1000;     // CRC transfer next
    static constexpr uint32_t CR1_DFF = 0x800;          // Data frame format
    static constexpr uint32_t CR1_RXONLY = 0x400;       // Receive only
    static constexpr uint32_t CR1_SSM = 0x200;          // Software slave management
    static constexpr uint32_t CR1_SSI = 0x100;          // Internal slave select
    static constexpr uint32_t CR1_LSBFIRST = 0x80;      // Frame format
    static constexpr uint32_t CR1_SPE = 0x40;           // SPI enable
    template<uint32_t X>
    static constexpr uint32_t CR1_BR =                  // Baud rate control (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    static constexpr uint32_t CR1_MSTR = 0x4;           // Master selection
    static constexpr uint32_t CR1_CPOL = 0x2;           // Clock polarity
    static constexpr uint32_t CR1_CPHA = 0x1;           // Clock phase
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_RXDMAEN = 0x1;        // Rx buffer DMA enable
    static constexpr uint32_t CR2_TXDMAEN = 0x2;        // Tx buffer DMA enable
    static constexpr uint32_t CR2_SSOE = 0x4;           // SS output enable
    static constexpr uint32_t CR2_NSSP = 0x8;           // NSS pulse management
    static constexpr uint32_t CR2_FRF = 0x10;           // Frame format
    static constexpr uint32_t CR2_ERRIE = 0x20;         // Error interrupt enable
    static constexpr uint32_t CR2_RXNEIE = 0x40;        // RX buffer not empty interrupt enable
    static constexpr uint32_t CR2_TXEIE = 0x80;         // Tx buffer empty interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR2_DS =                  // Data size (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t CR2_FRXTH = 0x1000;       // FIFO reception threshold
    static constexpr uint32_t CR2_LDMA_RX = 0x2000;     // Last DMA transfer for reception
    static constexpr uint32_t CR2_LDMA_TX = 0x4000;     // Last DMA transfer for transmission
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_RXNE = 0x1;           // Receive buffer not empty, Read-only
    static constexpr uint32_t SR_TXE = 0x2;            // Transmit buffer empty, Read-only
    static constexpr uint32_t SR_CHSIDE = 0x4;         // Channel side, Read-only
    static constexpr uint32_t SR_UDR = 0x8;            // Underrun flag, Read-only
    static constexpr uint32_t SR_CRCERR = 0x10;        // CRC error flag, Read-write
    static constexpr uint32_t SR_MODF = 0x20;          // Mode fault, Read-only
    static constexpr uint32_t SR_OVR = 0x40;           // Overrun flag, Read-only
    static constexpr uint32_t SR_BSY = 0x80;           // Busy flag, Read-only
    static constexpr uint32_t SR_TIFRFE = 0x100;       // TI frame format error, Read-only
    template<uint32_t X>
    static constexpr uint32_t SR_FRLVL =               // FIFO reception level (2 bits), Read-only
        bit_field_t<9, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SR_FTLVL =               // FIFO transmission level (2 bits), Read-only
        bit_field_t<11, 0x3>::value<X>();
    static const uint32_t SR_RESET_VALUE = 0x2;

    template<uint32_t X>
    static constexpr uint32_t DR_DR =                  // Data register (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CRCPR_CRCPOLY =             // CRC polynomial register (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CRCPR_RESET_VALUE = 0x7;

    template<uint32_t X>
    static constexpr uint32_t RXCRCR_RxCRC =               // Rx CRC register (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t RXCRCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXCRCR_TxCRC =               // Tx CRC register (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t TXCRCR_RESET_VALUE = 0x0;

    static constexpr uint32_t I2SCFGR_I2SMOD = 0x800;       // I2S mode selection
    static constexpr uint32_t I2SCFGR_I2SE = 0x400;         // I2S Enable
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SCFG =              // I2S configuration mode (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_PCMSYNC = 0x80;       // PCM frame synchronization
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SSTD =              // I2S standard selection (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_CKPOL = 0x8;          // Steady state clock polarity
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_DATLEN =              // Data length to be transferred (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_CHLEN = 0x1;          // Channel length (number of bits per audio channel)
    static const uint32_t I2SCFGR_RESET_VALUE = 0x0;

    static constexpr uint32_t I2SPR_MCKOE = 0x200;        // Master clock output enable
    static constexpr uint32_t I2SPR_ODD = 0x100;          // Odd factor for the prescaler
    template<uint32_t X>
    static constexpr uint32_t I2SPR_I2SDIV =              // I2S Linear prescaler (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t I2SPR_RESET_VALUE = 0x10;

    static constexpr uint8_t SPI2 = 26; // SPI2 global interrupt
};

static spi2_t& SPI2 = *reinterpret_cast<spi2_t*>(0x40003800);

#define HAVE_PERIPHERAL_SPI2


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

    static constexpr uint32_t CR_EN1 = 0x1;            // DAC channel1 enable
    static constexpr uint32_t CR_BOFF1 = 0x2;          // DAC channel1 output buffer disable
    static constexpr uint32_t CR_TEN1 = 0x4;           // DAC channel1 trigger enable
    template<uint32_t X>
    static constexpr uint32_t CR_TSEL1 =               // DAC channel1 trigger selection (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_WAVE1 =               // DAC channel1 noise/triangle wave generation enable (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_MAMP1 =               // DAC channel1 mask/amplitude selector (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t CR_DMAEN1 = 0x1000;      // DAC channel1 DMA enable
    static constexpr uint32_t CR_DMAUDRIE1 = 0x2000;   // DAC channel1 DMA Underrun Interrupt enable
    static constexpr uint32_t CR_EN2 = 0x10000;        // DAC channel2 enable
    static constexpr uint32_t CR_BOFF2 = 0x20000;      // DAC channel2 output buffer disable
    static constexpr uint32_t CR_TEN2 = 0x40000;       // DAC channel2 trigger enable
    template<uint32_t X>
    static constexpr uint32_t CR_TSEL2 =               // DAC channel2 trigger selection (3 bits)
        bit_field_t<19, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_WAVE2 =               // DAC channel2 noise/triangle wave generation enable (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_MAMP2 =               // DAC channel2 mask/amplitude selector (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t CR_DMAEN2 = 0x10000000;  // DAC channel2 DMA enable
    static constexpr uint32_t CR_DMAUDRIE2 = 0x20000000;// DAC channel2 DMA underrun interrupt enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t SWTRIGR_SWTRIG1 = 0x1;        // DAC channel1 software trigger
    static constexpr uint32_t SWTRIGR_SWTRIG2 = 0x2;        // DAC channel2 software trigger
    static const uint32_t SWTRIGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR12R1_DACC1DHR =            // DAC channel1 12-bit right-aligned data (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t DHR12R1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR12L1_DACC1DHR =            // DAC channel1 12-bit left-aligned data (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    static const uint32_t DHR12L1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR8R1_DACC1DHR =            // DAC channel1 8-bit right-aligned data (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t DHR8R1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR12R2_DACC2DHR =            // DAC channel2 12-bit right-aligned data (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t DHR12R2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR12L2_DACC2DHR =            // DAC channel2 12-bit left-aligned data (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    static const uint32_t DHR12L2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR8R2_DACC2DHR =            // DAC channel2 8-bit right-aligned data (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t DHR8R2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR12RD_DACC1DHR =            // DAC channel1 12-bit right-aligned data (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DHR12RD_DACC2DHR =            // DAC channel2 12-bit right-aligned data (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DHR12RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR12LD_DACC1DHR =            // DAC channel1 12-bit left-aligned data (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DHR12LD_DACC2DHR =            // DAC channel2 12-bit left-aligned data (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DHR12LD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR8RD_DACC2DHR =            // DAC channel2 8-bit right-aligned data (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DHR8RD_DACC1DHR =            // DAC channel1 8-bit right-aligned data (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t DHR8RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DOR1_DACC1DOR =            // DAC channel1 data output (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t DOR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DOR2_DACC2DOR =            // DAC channel2 data output (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t DOR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_DMAUDR2 = 0x20000000; // DAC channel2 DMA underrun flag
    static constexpr uint32_t SR_DMAUDR1 = 0x2000;     // DAC channel1 DMA underrun flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM6_DAC = 17; // TIM6 global interrupt and DAC underrun interrupt
};

static dac_t& DAC = *reinterpret_cast<dac_t*>(0x40007400);

#define HAVE_PERIPHERAL_DAC


////
//
//    Power control
//
////

struct pwr_t
{
    volatile uint32_t    CR;                   // [Read-write] power control register
    volatile uint32_t    CSR;                  // power control/status register

    static constexpr uint32_t CR_DBP = 0x100;          // Disable backup domain write protection
    template<uint32_t X>
    static constexpr uint32_t CR_PLS =                 // PVD level selection (3 bits)
        bit_field_t<5, 0x7>::value<X>();
    static constexpr uint32_t CR_PVDE = 0x10;          // Power voltage detector enable
    static constexpr uint32_t CR_CSBF = 0x8;           // Clear standby flag
    static constexpr uint32_t CR_CWUF = 0x4;           // Clear wakeup flag
    static constexpr uint32_t CR_PDDS = 0x2;           // Power down deepsleep
    static constexpr uint32_t CR_LPDS = 0x1;           // Low-power deep sleep
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t CSR_WUF = 0x1;            // Wakeup flag, Read-only
    static constexpr uint32_t CSR_SBF = 0x2;            // Standby flag, Read-only
    static constexpr uint32_t CSR_PVDO = 0x4;           // PVD output, Read-only
    static constexpr uint32_t CSR_VREFINTRDY = 0x8;     // VREFINT reference voltage ready, Read-only
    static constexpr uint32_t CSR_EWUP1 = 0x100;        // Enable WKUP pin 1, Read-write
    static constexpr uint32_t CSR_EWUP2 = 0x200;        // Enable WKUP pin 2, Read-write
    static constexpr uint32_t CSR_EWUP3 = 0x400;        // Enable WKUP pin 3, Read-write
    static constexpr uint32_t CSR_EWUP4 = 0x800;        // Enable WKUP pin 4, Read-write
    static constexpr uint32_t CSR_EWUP5 = 0x1000;       // Enable WKUP pin 5, Read-write
    static constexpr uint32_t CSR_EWUP6 = 0x2000;       // Enable WKUP pin 6, Read-write
    static constexpr uint32_t CSR_EWUP7 = 0x4000;       // Enable WKUP pin 7, Read-write
    static constexpr uint32_t CSR_EWUP8 = 0x8000;       // Enable WKUP pin 8, Read-write
    static const uint32_t CSR_RESET_VALUE = 0x0;
};

static pwr_t& PWR = *reinterpret_cast<pwr_t*>(0x40007000);

#define HAVE_PERIPHERAL_PWR


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

    static constexpr uint32_t CR1_PE = 0x1;             // Peripheral enable, Read-write
    static constexpr uint32_t CR1_TXIE = 0x2;           // TX Interrupt enable, Read-write
    static constexpr uint32_t CR1_RXIE = 0x4;           // RX Interrupt enable, Read-write
    static constexpr uint32_t CR1_ADDRIE = 0x8;         // Address match interrupt enable (slave only), Read-write
    static constexpr uint32_t CR1_NACKIE = 0x10;        // Not acknowledge received interrupt enable, Read-write
    static constexpr uint32_t CR1_STOPIE = 0x20;        // STOP detection Interrupt enable, Read-write
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transfer Complete interrupt enable, Read-write
    static constexpr uint32_t CR1_ERRIE = 0x80;         // Error interrupts enable, Read-write
    template<uint32_t X>
    static constexpr uint32_t CR1_DNF =                 // Digital noise filter (4 bits), Read-write
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t CR1_ANFOFF = 0x1000;      // Analog noise filter OFF, Read-write
    static constexpr uint32_t CR1_SWRST = 0x2000;       // Software reset, Write-only
    static constexpr uint32_t CR1_TXDMAEN = 0x4000;     // DMA transmission requests enable, Read-write
    static constexpr uint32_t CR1_RXDMAEN = 0x8000;     // DMA reception requests enable, Read-write
    static constexpr uint32_t CR1_SBC = 0x10000;        // Slave byte control, Read-write
    static constexpr uint32_t CR1_NOSTRETCH = 0x20000;  // Clock stretching disable, Read-write
    static constexpr uint32_t CR1_WUPEN = 0x40000;      // Wakeup from STOP enable, Read-write
    static constexpr uint32_t CR1_GCEN = 0x80000;       // General call enable, Read-write
    static constexpr uint32_t CR1_SMBHEN = 0x100000;    // SMBus Host address enable, Read-write
    static constexpr uint32_t CR1_SMBDEN = 0x200000;    // SMBus Device Default address enable, Read-write
    static constexpr uint32_t CR1_ALERTEN = 0x400000;   // SMBUS alert enable, Read-write
    static constexpr uint32_t CR1_PECEN = 0x800000;     // PEC enable, Read-write
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_PECBYTE = 0x4000000;  // Packet error checking byte
    static constexpr uint32_t CR2_AUTOEND = 0x2000000;  // Automatic end mode (master mode)
    static constexpr uint32_t CR2_RELOAD = 0x1000000;   // NBYTES reload mode
    template<uint32_t X>
    static constexpr uint32_t CR2_NBYTES =              // Number of bytes (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static constexpr uint32_t CR2_NACK = 0x8000;        // NACK generation (slave mode)
    static constexpr uint32_t CR2_STOP = 0x4000;        // Stop generation (master mode)
    static constexpr uint32_t CR2_START = 0x2000;       // Start generation
    static constexpr uint32_t CR2_HEAD10R = 0x1000;     // 10-bit address header only read direction (master receiver mode)
    static constexpr uint32_t CR2_ADD10 = 0x800;        // 10-bit addressing mode (master mode)
    static constexpr uint32_t CR2_RD_WRN = 0x400;       // Transfer direction (master mode)
    template<uint32_t X>
    static constexpr uint32_t CR2_SADD8 =               // Slave address bit 9:8 (master mode) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_SADD1 =               // Slave address bit 7:1 (master mode) (7 bits)
        bit_field_t<1, 0x7f>::value<X>();
    static constexpr uint32_t CR2_SADD0 = 0x1;          // Slave address bit 0 (master mode)
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t OAR1_OA1_0 = 0x1;          // Interface address
    template<uint32_t X>
    static constexpr uint32_t OAR1_OA1_1 =               // Interface address (7 bits)
        bit_field_t<1, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OAR1_OA1_8 =               // Interface address (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t OAR1_OA1MODE = 0x400;      // Own Address 1 10-bit mode
    static constexpr uint32_t OAR1_OA1EN = 0x8000;       // Own Address 1 enable
    static const uint32_t OAR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OAR2_OA2 =                 // Interface address (7 bits)
        bit_field_t<1, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OAR2_OA2MSK =              // Own Address 2 masks (3 bits)
        bit_field_t<8, 0x7>::value<X>();
    static constexpr uint32_t OAR2_OA2EN = 0x8000;       // Own Address 2 enable
    static const uint32_t OAR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TIMINGR_SCLL =                // SCL low period (master mode) (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TIMINGR_SCLH =                // SCL high period (master mode) (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TIMINGR_SDADEL =              // Data hold time (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TIMINGR_SCLDEL =              // Data setup time (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TIMINGR_PRESC =               // Timing prescaler (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    static const uint32_t TIMINGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TIMEOUTR_TIMEOUTA =            // Bus timeout A (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static constexpr uint32_t TIMEOUTR_TIDLE = 0x1000;       // Idle clock timeout detection
    static constexpr uint32_t TIMEOUTR_TIMOUTEN = 0x8000;    // Clock timeout enable
    template<uint32_t X>
    static constexpr uint32_t TIMEOUTR_TIMEOUTB =            // Bus timeout B (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static constexpr uint32_t TIMEOUTR_TEXTEN = 0x80000000;  // Extended clock timeout enable
    static const uint32_t TIMEOUTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ISR_ADDCODE =             // Address match code (Slave mode) (7 bits), Read-only
        bit_field_t<17, 0x7f>::value<X>();
    static constexpr uint32_t ISR_DIR = 0x10000;        // Transfer direction (Slave mode), Read-only
    static constexpr uint32_t ISR_BUSY = 0x8000;        // Bus busy, Read-only
    static constexpr uint32_t ISR_ALERT = 0x2000;       // SMBus alert, Read-only
    static constexpr uint32_t ISR_TIMEOUT = 0x1000;     // Timeout or t_low detection flag, Read-only
    static constexpr uint32_t ISR_PECERR = 0x800;       // PEC Error in reception, Read-only
    static constexpr uint32_t ISR_OVR = 0x400;          // Overrun/Underrun (slave mode), Read-only
    static constexpr uint32_t ISR_ARLO = 0x200;         // Arbitration lost, Read-only
    static constexpr uint32_t ISR_BERR = 0x100;         // Bus error, Read-only
    static constexpr uint32_t ISR_TCR = 0x80;           // Transfer Complete Reload, Read-only
    static constexpr uint32_t ISR_TC = 0x40;            // Transfer Complete (master mode), Read-only
    static constexpr uint32_t ISR_STOPF = 0x20;         // Stop detection flag, Read-only
    static constexpr uint32_t ISR_NACKF = 0x10;         // Not acknowledge received flag, Read-only
    static constexpr uint32_t ISR_ADDR = 0x8;           // Address matched (slave mode), Read-only
    static constexpr uint32_t ISR_RXNE = 0x4;           // Receive data register not empty (receivers), Read-only
    static constexpr uint32_t ISR_TXIS = 0x2;           // Transmit interrupt status (transmitters), Read-write
    static constexpr uint32_t ISR_TXE = 0x1;            // Transmit data register empty (transmitters), Read-write
    static const uint32_t ISR_RESET_VALUE = 0x1;

    static constexpr uint32_t ICR_ALERTCF = 0x2000;     // Alert flag clear
    static constexpr uint32_t ICR_TIMOUTCF = 0x1000;    // Timeout detection flag clear
    static constexpr uint32_t ICR_PECCF = 0x800;        // PEC Error flag clear
    static constexpr uint32_t ICR_OVRCF = 0x400;        // Overrun/Underrun flag clear
    static constexpr uint32_t ICR_ARLOCF = 0x200;       // Arbitration lost flag clear
    static constexpr uint32_t ICR_BERRCF = 0x100;       // Bus error flag clear
    static constexpr uint32_t ICR_STOPCF = 0x20;        // Stop detection flag clear
    static constexpr uint32_t ICR_NACKCF = 0x10;        // Not Acknowledge flag clear
    static constexpr uint32_t ICR_ADDRCF = 0x8;         // Address Matched flag clear
    static const uint32_t ICR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PECR_PEC =                 // Packet error checking register (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PECR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXDR_RXDATA =              // 8-bit receive data (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RXDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXDR_TXDATA =              // 8-bit transmit data (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TXDR_RESET_VALUE = 0x0;

    static constexpr uint8_t I2C1 = 23; // I2C1 global interrupt
};

static i2c1_t& I2C1 = *reinterpret_cast<i2c1_t*>(0x40005400);

#define HAVE_PERIPHERAL_I2C1


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

    static constexpr uint32_t CR1_PE = 0x1;             // Peripheral enable, Read-write
    static constexpr uint32_t CR1_TXIE = 0x2;           // TX Interrupt enable, Read-write
    static constexpr uint32_t CR1_RXIE = 0x4;           // RX Interrupt enable, Read-write
    static constexpr uint32_t CR1_ADDRIE = 0x8;         // Address match interrupt enable (slave only), Read-write
    static constexpr uint32_t CR1_NACKIE = 0x10;        // Not acknowledge received interrupt enable, Read-write
    static constexpr uint32_t CR1_STOPIE = 0x20;        // STOP detection Interrupt enable, Read-write
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transfer Complete interrupt enable, Read-write
    static constexpr uint32_t CR1_ERRIE = 0x80;         // Error interrupts enable, Read-write
    template<uint32_t X>
    static constexpr uint32_t CR1_DNF =                 // Digital noise filter (4 bits), Read-write
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t CR1_ANFOFF = 0x1000;      // Analog noise filter OFF, Read-write
    static constexpr uint32_t CR1_SWRST = 0x2000;       // Software reset, Write-only
    static constexpr uint32_t CR1_TXDMAEN = 0x4000;     // DMA transmission requests enable, Read-write
    static constexpr uint32_t CR1_RXDMAEN = 0x8000;     // DMA reception requests enable, Read-write
    static constexpr uint32_t CR1_SBC = 0x10000;        // Slave byte control, Read-write
    static constexpr uint32_t CR1_NOSTRETCH = 0x20000;  // Clock stretching disable, Read-write
    static constexpr uint32_t CR1_WUPEN = 0x40000;      // Wakeup from STOP enable, Read-write
    static constexpr uint32_t CR1_GCEN = 0x80000;       // General call enable, Read-write
    static constexpr uint32_t CR1_SMBHEN = 0x100000;    // SMBus Host address enable, Read-write
    static constexpr uint32_t CR1_SMBDEN = 0x200000;    // SMBus Device Default address enable, Read-write
    static constexpr uint32_t CR1_ALERTEN = 0x400000;   // SMBUS alert enable, Read-write
    static constexpr uint32_t CR1_PECEN = 0x800000;     // PEC enable, Read-write
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_PECBYTE = 0x4000000;  // Packet error checking byte
    static constexpr uint32_t CR2_AUTOEND = 0x2000000;  // Automatic end mode (master mode)
    static constexpr uint32_t CR2_RELOAD = 0x1000000;   // NBYTES reload mode
    template<uint32_t X>
    static constexpr uint32_t CR2_NBYTES =              // Number of bytes (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static constexpr uint32_t CR2_NACK = 0x8000;        // NACK generation (slave mode)
    static constexpr uint32_t CR2_STOP = 0x4000;        // Stop generation (master mode)
    static constexpr uint32_t CR2_START = 0x2000;       // Start generation
    static constexpr uint32_t CR2_HEAD10R = 0x1000;     // 10-bit address header only read direction (master receiver mode)
    static constexpr uint32_t CR2_ADD10 = 0x800;        // 10-bit addressing mode (master mode)
    static constexpr uint32_t CR2_RD_WRN = 0x400;       // Transfer direction (master mode)
    template<uint32_t X>
    static constexpr uint32_t CR2_SADD8 =               // Slave address bit 9:8 (master mode) (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_SADD1 =               // Slave address bit 7:1 (master mode) (7 bits)
        bit_field_t<1, 0x7f>::value<X>();
    static constexpr uint32_t CR2_SADD0 = 0x1;          // Slave address bit 0 (master mode)
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t OAR1_OA1_0 = 0x1;          // Interface address
    template<uint32_t X>
    static constexpr uint32_t OAR1_OA1_1 =               // Interface address (7 bits)
        bit_field_t<1, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OAR1_OA1_8 =               // Interface address (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t OAR1_OA1MODE = 0x400;      // Own Address 1 10-bit mode
    static constexpr uint32_t OAR1_OA1EN = 0x8000;       // Own Address 1 enable
    static const uint32_t OAR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OAR2_OA2 =                 // Interface address (7 bits)
        bit_field_t<1, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OAR2_OA2MSK =              // Own Address 2 masks (3 bits)
        bit_field_t<8, 0x7>::value<X>();
    static constexpr uint32_t OAR2_OA2EN = 0x8000;       // Own Address 2 enable
    static const uint32_t OAR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TIMINGR_SCLL =                // SCL low period (master mode) (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TIMINGR_SCLH =                // SCL high period (master mode) (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TIMINGR_SDADEL =              // Data hold time (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TIMINGR_SCLDEL =              // Data setup time (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TIMINGR_PRESC =               // Timing prescaler (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    static const uint32_t TIMINGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TIMEOUTR_TIMEOUTA =            // Bus timeout A (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static constexpr uint32_t TIMEOUTR_TIDLE = 0x1000;       // Idle clock timeout detection
    static constexpr uint32_t TIMEOUTR_TIMOUTEN = 0x8000;    // Clock timeout enable
    template<uint32_t X>
    static constexpr uint32_t TIMEOUTR_TIMEOUTB =            // Bus timeout B (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static constexpr uint32_t TIMEOUTR_TEXTEN = 0x80000000;  // Extended clock timeout enable
    static const uint32_t TIMEOUTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ISR_ADDCODE =             // Address match code (Slave mode) (7 bits), Read-only
        bit_field_t<17, 0x7f>::value<X>();
    static constexpr uint32_t ISR_DIR = 0x10000;        // Transfer direction (Slave mode), Read-only
    static constexpr uint32_t ISR_BUSY = 0x8000;        // Bus busy, Read-only
    static constexpr uint32_t ISR_ALERT = 0x2000;       // SMBus alert, Read-only
    static constexpr uint32_t ISR_TIMEOUT = 0x1000;     // Timeout or t_low detection flag, Read-only
    static constexpr uint32_t ISR_PECERR = 0x800;       // PEC Error in reception, Read-only
    static constexpr uint32_t ISR_OVR = 0x400;          // Overrun/Underrun (slave mode), Read-only
    static constexpr uint32_t ISR_ARLO = 0x200;         // Arbitration lost, Read-only
    static constexpr uint32_t ISR_BERR = 0x100;         // Bus error, Read-only
    static constexpr uint32_t ISR_TCR = 0x80;           // Transfer Complete Reload, Read-only
    static constexpr uint32_t ISR_TC = 0x40;            // Transfer Complete (master mode), Read-only
    static constexpr uint32_t ISR_STOPF = 0x20;         // Stop detection flag, Read-only
    static constexpr uint32_t ISR_NACKF = 0x10;         // Not acknowledge received flag, Read-only
    static constexpr uint32_t ISR_ADDR = 0x8;           // Address matched (slave mode), Read-only
    static constexpr uint32_t ISR_RXNE = 0x4;           // Receive data register not empty (receivers), Read-only
    static constexpr uint32_t ISR_TXIS = 0x2;           // Transmit interrupt status (transmitters), Read-write
    static constexpr uint32_t ISR_TXE = 0x1;            // Transmit data register empty (transmitters), Read-write
    static const uint32_t ISR_RESET_VALUE = 0x1;

    static constexpr uint32_t ICR_ALERTCF = 0x2000;     // Alert flag clear
    static constexpr uint32_t ICR_TIMOUTCF = 0x1000;    // Timeout detection flag clear
    static constexpr uint32_t ICR_PECCF = 0x800;        // PEC Error flag clear
    static constexpr uint32_t ICR_OVRCF = 0x400;        // Overrun/Underrun flag clear
    static constexpr uint32_t ICR_ARLOCF = 0x200;       // Arbitration lost flag clear
    static constexpr uint32_t ICR_BERRCF = 0x100;       // Bus error flag clear
    static constexpr uint32_t ICR_STOPCF = 0x20;        // Stop detection flag clear
    static constexpr uint32_t ICR_NACKCF = 0x10;        // Not Acknowledge flag clear
    static constexpr uint32_t ICR_ADDRCF = 0x8;         // Address Matched flag clear
    static const uint32_t ICR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PECR_PEC =                 // Packet error checking register (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PECR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXDR_RXDATA =              // 8-bit receive data (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RXDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXDR_TXDATA =              // 8-bit transmit data (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TXDR_RESET_VALUE = 0x0;

    static constexpr uint8_t I2C2 = 24; // I2C2 global interrupt
};

static i2c2_t& I2C2 = *reinterpret_cast<i2c2_t*>(0x40005800);

#define HAVE_PERIPHERAL_I2C2


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

    template<uint32_t X>
    static constexpr uint32_t KR_KEY =                 // Key value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t KR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PR_PR =                  // Prescaler divider (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t PR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RLR_RL =                  // Watchdog counter reload value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t RLR_RESET_VALUE = 0xfff;

    static constexpr uint32_t SR_PVU = 0x1;            // Watchdog prescaler value update
    static constexpr uint32_t SR_RVU = 0x2;            // Watchdog counter reload value update
    static constexpr uint32_t SR_WVU = 0x4;            // Watchdog counter window value update
    static const uint32_t SR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t WINR_WIN =                 // Watchdog counter window value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t WINR_RESET_VALUE = 0xfff;
};

static iwdg_t& IWDG = *reinterpret_cast<iwdg_t*>(0x40003000);

#define HAVE_PERIPHERAL_IWDG


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

    static constexpr uint32_t CR_WDGA = 0x80;          // Activation bit
    template<uint32_t X>
    static constexpr uint32_t CR_T =                   // 7-bit counter (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t CR_RESET_VALUE = 0x7f;

    static constexpr uint32_t CFR_EWI = 0x200;          // Early wakeup interrupt
    template<uint32_t X>
    static constexpr uint32_t CFR_WDGTB =               // Timer base (2 bits)
        bit_field_t<7, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFR_W =                   // 7-bit window value (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t CFR_RESET_VALUE = 0x7f;

    static constexpr uint32_t SR_EWIF = 0x1;           // Early wakeup interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint8_t WWDG = 0; // Window Watchdog interrupt
};

static wwdg_t& WWDG = *reinterpret_cast<wwdg_t*>(0x40002c00);

#define HAVE_PERIPHERAL_WWDG


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

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    template<uint32_t X>
    static constexpr uint32_t CR1_CMS =                 // Center-aligned mode selection (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t CR1_DIR = 0x10;           // Direction
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_OIS4 = 0x4000;        // Output Idle state 4
    static constexpr uint32_t CR2_OIS3N = 0x2000;       // Output Idle state 3
    static constexpr uint32_t CR2_OIS3 = 0x1000;        // Output Idle state 3
    static constexpr uint32_t CR2_OIS2N = 0x800;        // Output Idle state 2
    static constexpr uint32_t CR2_OIS2 = 0x400;         // Output Idle state 2
    static constexpr uint32_t CR2_OIS1N = 0x200;        // Output Idle state 1
    static constexpr uint32_t CR2_OIS1 = 0x100;         // Output Idle state 1
    static constexpr uint32_t CR2_TI1S = 0x80;          // TI1 selection
    template<uint32_t X>
    static constexpr uint32_t CR2_MMS =                 // Master mode selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CR2_CCDS = 0x8;           // Capture/compare DMA selection
    static constexpr uint32_t CR2_CCUS = 0x4;           // Capture/compare control update selection
    static constexpr uint32_t CR2_CCPC = 0x1;           // Capture/compare preloaded control
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SMCR_ETP = 0x8000;         // External trigger polarity
    static constexpr uint32_t SMCR_ECE = 0x4000;         // External clock enable
    template<uint32_t X>
    static constexpr uint32_t SMCR_ETPS =                // External trigger prescaler (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMCR_ETF =                 // External trigger filter (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t SMCR_MSM = 0x80;           // Master/Slave mode
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS =                  // Trigger selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMCR_SMS =                 // Slave mode selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t SMCR_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_TDE = 0x4000;         // Trigger DMA request enable
    static constexpr uint32_t DIER_COMDE = 0x2000;       // COM DMA request enable
    static constexpr uint32_t DIER_CC4DE = 0x1000;       // Capture/Compare 4 DMA request enable
    static constexpr uint32_t DIER_CC3DE = 0x800;        // Capture/Compare 3 DMA request enable
    static constexpr uint32_t DIER_CC2DE = 0x400;        // Capture/Compare 2 DMA request enable
    static constexpr uint32_t DIER_CC1DE = 0x200;        // Capture/Compare 1 DMA request enable
    static constexpr uint32_t DIER_UDE = 0x100;          // Update DMA request enable
    static constexpr uint32_t DIER_BIE = 0x80;           // Break interrupt enable
    static constexpr uint32_t DIER_TIE = 0x40;           // Trigger interrupt enable
    static constexpr uint32_t DIER_COMIE = 0x20;         // COM interrupt enable
    static constexpr uint32_t DIER_CC4IE = 0x10;         // Capture/Compare 4 interrupt enable
    static constexpr uint32_t DIER_CC3IE = 0x8;          // Capture/Compare 3 interrupt enable
    static constexpr uint32_t DIER_CC2IE = 0x4;          // Capture/Compare 2 interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_CC4OF = 0x1000;       // Capture/Compare 4 overcapture flag
    static constexpr uint32_t SR_CC3OF = 0x800;        // Capture/Compare 3 overcapture flag
    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/compare 2 overcapture flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC4IF = 0x10;         // Capture/Compare 4 interrupt flag
    static constexpr uint32_t SR_CC3IF = 0x8;          // Capture/Compare 3 interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/Compare 2 interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_BG = 0x80;            // Break generation
    static constexpr uint32_t EGR_TG = 0x40;            // Trigger generation
    static constexpr uint32_t EGR_COMG = 0x20;          // Capture/Compare control update generation
    static constexpr uint32_t EGR_CC4G = 0x10;          // Capture/compare 4 generation
    static constexpr uint32_t EGR_CC3G = 0x8;           // Capture/compare 3 generation
    static constexpr uint32_t EGR_CC2G = 0x4;           // Capture/compare 2 generation
    static constexpr uint32_t EGR_CC1G = 0x2;           // Capture/compare 1 generation
    static constexpr uint32_t EGR_UG = 0x1;             // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC1S =                // Capture/Compare 1 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC2S =                // Capture/Compare 2 selection (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1F =                // Input capture 1 filter (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1PCS =              // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC2F =                // Input capture 2 filter (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC2PCS =              // Input capture 2 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t CCMR1_OC1CE = 0x80;         // Output Compare 1 clear enable
    static constexpr uint32_t CCMR1_OC1FE = 0x4;          // Output Compare 1 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC1M =                // Output Compare 1 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output Compare 1 preload enable
    static constexpr uint32_t CCMR1_OC2CE = 0x8000;       // Output Compare 2 clear enable
    static constexpr uint32_t CCMR1_OC2FE = 0x400;        // Output Compare 2 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC2M =                // Output Compare 2 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC2PE = 0x800;        // Output Compare 2 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCMR2_CC3S =                // Capture/Compare 3 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_CC4S =                // Capture/Compare 4 selection (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_IC3F =                // Input capture 3 filter (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_IC3PSC =              // Input capture 3 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_IC4F =                // Input capture 4 filter (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_IC4PSC =              // Input capture 4 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t CCMR2_OC3CE = 0x80;         // Output compare 3 clear enable
    static constexpr uint32_t CCMR2_OC3FE = 0x4;          // Output compare 3 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC3M =                // Output compare 3 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC3PE = 0x8;          // Output compare 3 preload enable
    static constexpr uint32_t CCMR2_OC4CE = 0x8000;       // Output compare 4 clear enable
    static constexpr uint32_t CCMR2_OC4FE = 0x400;        // Output compare 4 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC4M =                // Output compare 4 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC4PE = 0x800;        // Output compare 4 preload enable
    static const uint32_t CCMR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC4P = 0x2000;        // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC4E = 0x1000;        // Capture/Compare 4 output enable
    static constexpr uint32_t CCER_CC3NP = 0x800;        // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC3NE = 0x400;        // Capture/Compare 3 complementary output enable
    static constexpr uint32_t CCER_CC3P = 0x200;         // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC3E = 0x100;         // Capture/Compare 3 output enable
    static constexpr uint32_t CCER_CC2NP = 0x80;         // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2NE = 0x40;         // Capture/Compare 2 complementary output enable
    static constexpr uint32_t CCER_CC2P = 0x20;          // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2E = 0x10;          // Capture/Compare 2 output enable
    static constexpr uint32_t CCER_CC1NP = 0x8;          // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1NE = 0x4;          // Capture/Compare 1 complementary output enable
    static constexpr uint32_t CCER_CC1P = 0x2;           // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1E = 0x1;           // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RCR_REP =                 // Repetition counter value (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR1_CCR1 =                // Capture/Compare 1 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR2_CCR2 =                // Capture/Compare 2 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR3_CCR3 =                // Capture/Compare 3 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR4_CCR4 =                // Capture/Compare 3 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR4_RESET_VALUE = 0x0;

    static constexpr uint32_t BDTR_MOE = 0x8000;         // Main output enable
    static constexpr uint32_t BDTR_AOE = 0x4000;         // Automatic output enable
    static constexpr uint32_t BDTR_BKP = 0x2000;         // Break polarity
    static constexpr uint32_t BDTR_BKE = 0x1000;         // Break enable
    static constexpr uint32_t BDTR_OSSR = 0x800;         // Off-state selection for Run mode
    static constexpr uint32_t BDTR_OSSI = 0x400;         // Off-state selection for Idle mode
    template<uint32_t X>
    static constexpr uint32_t BDTR_LOCK =                // Lock configuration (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_DTG =                 // Dead-time generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t BDTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DMAR_DMAB =                // DMA register for burst accesses (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM1_BRK_UP_TRG_COM = 13; // TIM1 break, update, trigger and commutation interrupt
    static constexpr uint8_t TIM1_CC = 14; // TIM1 Capture Compare interrupt
};

static tim1_t& TIM1 = *reinterpret_cast<tim1_t*>(0x40012c00);

#define HAVE_PERIPHERAL_TIM1


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

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    template<uint32_t X>
    static constexpr uint32_t CR1_CMS =                 // Center-aligned mode selection (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t CR1_DIR = 0x10;           // Direction
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_TI1S = 0x80;          // TI1 selection
    template<uint32_t X>
    static constexpr uint32_t CR2_MMS =                 // Master mode selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CR2_CCDS = 0x8;           // Capture/compare DMA selection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SMCR_ETP = 0x8000;         // External trigger polarity
    static constexpr uint32_t SMCR_ECE = 0x4000;         // External clock enable
    template<uint32_t X>
    static constexpr uint32_t SMCR_ETPS =                // External trigger prescaler (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMCR_ETF =                 // External trigger filter (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t SMCR_MSM = 0x80;           // Master/Slave mode
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS =                  // Trigger selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMCR_SMS =                 // Slave mode selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t SMCR_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_TDE = 0x4000;         // Trigger DMA request enable
    static constexpr uint32_t DIER_COMDE = 0x2000;       // COM DMA request enable
    static constexpr uint32_t DIER_CC4DE = 0x1000;       // Capture/Compare 4 DMA request enable
    static constexpr uint32_t DIER_CC3DE = 0x800;        // Capture/Compare 3 DMA request enable
    static constexpr uint32_t DIER_CC2DE = 0x400;        // Capture/Compare 2 DMA request enable
    static constexpr uint32_t DIER_CC1DE = 0x200;        // Capture/Compare 1 DMA request enable
    static constexpr uint32_t DIER_UDE = 0x100;          // Update DMA request enable
    static constexpr uint32_t DIER_TIE = 0x40;           // Trigger interrupt enable
    static constexpr uint32_t DIER_CC4IE = 0x10;         // Capture/Compare 4 interrupt enable
    static constexpr uint32_t DIER_CC3IE = 0x8;          // Capture/Compare 3 interrupt enable
    static constexpr uint32_t DIER_CC2IE = 0x4;          // Capture/Compare 2 interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_CC4OF = 0x1000;       // Capture/Compare 4 overcapture flag
    static constexpr uint32_t SR_CC3OF = 0x800;        // Capture/Compare 3 overcapture flag
    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/compare 2 overcapture flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_CC4IF = 0x10;         // Capture/Compare 4 interrupt flag
    static constexpr uint32_t SR_CC3IF = 0x8;          // Capture/Compare 3 interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/Compare 2 interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_TG = 0x40;            // Trigger generation
    static constexpr uint32_t EGR_CC4G = 0x10;          // Capture/compare 4 generation
    static constexpr uint32_t EGR_CC3G = 0x8;           // Capture/compare 3 generation
    static constexpr uint32_t EGR_CC2G = 0x4;           // Capture/compare 2 generation
    static constexpr uint32_t EGR_CC1G = 0x2;           // Capture/compare 1 generation
    static constexpr uint32_t EGR_UG = 0x1;             // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC1S =                // Capture/Compare 1 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC2S =                // Capture/Compare 2 selection (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1F =                // Input capture 1 filter (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1PSC =              // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC2F =                // Input capture 2 filter (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC2PSC =              // Input capture 2 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t CCMR1_OC1CE = 0x80;         // Output compare 1 clear enable
    static constexpr uint32_t CCMR1_OC1FE = 0x4;          // Output compare 1 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC1M =                // Output compare 1 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output compare 1 preload enable
    static constexpr uint32_t CCMR1_OC2CE = 0x8000;       // Output compare 2 clear enable
    static constexpr uint32_t CCMR1_OC2FE = 0x400;        // Output compare 2 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC2M =                // Output compare 2 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC2PE = 0x800;        // Output compare 2 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCMR2_CC3S =                // Capture/Compare 3 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_CC4S =                // Capture/Compare 4 selection (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_IC3F =                // Input capture 3 filter (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_IC3PSC =              // Input capture 3 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_IC4F =                // Input capture 4 filter (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_IC4PSC =              // Input capture 4 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t CCMR2_OC3CE = 0x80;         // Output compare 3 clear enable
    static constexpr uint32_t CCMR2_OC3FE = 0x4;          // Output compare 3 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC3M =                // Output compare 3 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC3PE = 0x8;          // Output compare 3 preload enable
    static constexpr uint32_t CCMR2_OC4CE = 0x8000;       // Output compare 4 clear enable
    static constexpr uint32_t CCMR2_OC4FE = 0x400;        // Output compare 4 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC4M =                // Output compare 4 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC4PE = 0x800;        // Output compare 4 preload enable
    static const uint32_t CCMR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC4NP = 0x8000;       // Capture/Compare 4 output Polarity
    static constexpr uint32_t CCER_CC4P = 0x2000;        // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC4E = 0x1000;        // Capture/Compare 4 output enable
    static constexpr uint32_t CCER_CC3NP = 0x800;        // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC3P = 0x200;         // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC3E = 0x100;         // Capture/Compare 3 output enable
    static constexpr uint32_t CCER_CC2NP = 0x80;         // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2P = 0x20;          // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2E = 0x10;          // Capture/Compare 2 output enable
    static constexpr uint32_t CCER_CC1NP = 0x8;          // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1P = 0x2;           // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1E = 0x1;           // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT_H =               // High counter value (TIM2 only) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CNT_CNT_L =               // Low counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR_H =               // High Auto-reload value (TIM2 only) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ARR_ARR_L =               // Low Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR1_CCR1_H =              // High Capture/Compare 1 value (TIM2 only) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR1_CCR1_L =              // Low Capture/Compare 1 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR2_CCR2_H =              // High Capture/Compare 2 value (TIM2 only) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR2_CCR2_L =              // Low Capture/Compare 2 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR3_CCR3_H =              // High Capture/Compare value (TIM2 only) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR3_CCR3_L =              // Low Capture/Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR4_CCR4_H =              // High Capture/Compare value (TIM2 only) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR4_CCR4_L =              // Low Capture/Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DMAR_DMAR =                // DMA register for burst accesses (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM2 = 15; // TIM2 global interrupt
};

static tim2_t& TIM2 = *reinterpret_cast<tim2_t*>(0x40000000);

#define HAVE_PERIPHERAL_TIM2


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

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    template<uint32_t X>
    static constexpr uint32_t CR1_CMS =                 // Center-aligned mode selection (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t CR1_DIR = 0x10;           // Direction
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_TI1S = 0x80;          // TI1 selection
    template<uint32_t X>
    static constexpr uint32_t CR2_MMS =                 // Master mode selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CR2_CCDS = 0x8;           // Capture/compare DMA selection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SMCR_ETP = 0x8000;         // External trigger polarity
    static constexpr uint32_t SMCR_ECE = 0x4000;         // External clock enable
    template<uint32_t X>
    static constexpr uint32_t SMCR_ETPS =                // External trigger prescaler (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMCR_ETF =                 // External trigger filter (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t SMCR_MSM = 0x80;           // Master/Slave mode
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS =                  // Trigger selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMCR_SMS =                 // Slave mode selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t SMCR_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_TDE = 0x4000;         // Trigger DMA request enable
    static constexpr uint32_t DIER_COMDE = 0x2000;       // COM DMA request enable
    static constexpr uint32_t DIER_CC4DE = 0x1000;       // Capture/Compare 4 DMA request enable
    static constexpr uint32_t DIER_CC3DE = 0x800;        // Capture/Compare 3 DMA request enable
    static constexpr uint32_t DIER_CC2DE = 0x400;        // Capture/Compare 2 DMA request enable
    static constexpr uint32_t DIER_CC1DE = 0x200;        // Capture/Compare 1 DMA request enable
    static constexpr uint32_t DIER_UDE = 0x100;          // Update DMA request enable
    static constexpr uint32_t DIER_TIE = 0x40;           // Trigger interrupt enable
    static constexpr uint32_t DIER_CC4IE = 0x10;         // Capture/Compare 4 interrupt enable
    static constexpr uint32_t DIER_CC3IE = 0x8;          // Capture/Compare 3 interrupt enable
    static constexpr uint32_t DIER_CC2IE = 0x4;          // Capture/Compare 2 interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_CC4OF = 0x1000;       // Capture/Compare 4 overcapture flag
    static constexpr uint32_t SR_CC3OF = 0x800;        // Capture/Compare 3 overcapture flag
    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/compare 2 overcapture flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_CC4IF = 0x10;         // Capture/Compare 4 interrupt flag
    static constexpr uint32_t SR_CC3IF = 0x8;          // Capture/Compare 3 interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/Compare 2 interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_TG = 0x40;            // Trigger generation
    static constexpr uint32_t EGR_CC4G = 0x10;          // Capture/compare 4 generation
    static constexpr uint32_t EGR_CC3G = 0x8;           // Capture/compare 3 generation
    static constexpr uint32_t EGR_CC2G = 0x4;           // Capture/compare 2 generation
    static constexpr uint32_t EGR_CC1G = 0x2;           // Capture/compare 1 generation
    static constexpr uint32_t EGR_UG = 0x1;             // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC1S =                // Capture/Compare 1 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC2S =                // Capture/Compare 2 selection (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1F =                // Input capture 1 filter (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1PSC =              // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC2F =                // Input capture 2 filter (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC2PSC =              // Input capture 2 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t CCMR1_OC1CE = 0x80;         // Output compare 1 clear enable
    static constexpr uint32_t CCMR1_OC1FE = 0x4;          // Output compare 1 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC1M =                // Output compare 1 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output compare 1 preload enable
    static constexpr uint32_t CCMR1_OC2CE = 0x8000;       // Output compare 2 clear enable
    static constexpr uint32_t CCMR1_OC2FE = 0x400;        // Output compare 2 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC2M =                // Output compare 2 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC2PE = 0x800;        // Output compare 2 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCMR2_CC3S =                // Capture/Compare 3 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_CC4S =                // Capture/Compare 4 selection (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_IC3F =                // Input capture 3 filter (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_IC3PSC =              // Input capture 3 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_IC4F =                // Input capture 4 filter (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_IC4PSC =              // Input capture 4 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t CCMR2_OC3CE = 0x80;         // Output compare 3 clear enable
    static constexpr uint32_t CCMR2_OC3FE = 0x4;          // Output compare 3 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC3M =                // Output compare 3 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC3PE = 0x8;          // Output compare 3 preload enable
    static constexpr uint32_t CCMR2_OC4CE = 0x8000;       // Output compare 4 clear enable
    static constexpr uint32_t CCMR2_OC4FE = 0x400;        // Output compare 4 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC4M =                // Output compare 4 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC4PE = 0x800;        // Output compare 4 preload enable
    static const uint32_t CCMR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC4NP = 0x8000;       // Capture/Compare 4 output Polarity
    static constexpr uint32_t CCER_CC4P = 0x2000;        // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC4E = 0x1000;        // Capture/Compare 4 output enable
    static constexpr uint32_t CCER_CC3NP = 0x800;        // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC3P = 0x200;         // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC3E = 0x100;         // Capture/Compare 3 output enable
    static constexpr uint32_t CCER_CC2NP = 0x80;         // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2P = 0x20;          // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2E = 0x10;          // Capture/Compare 2 output enable
    static constexpr uint32_t CCER_CC1NP = 0x8;          // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1P = 0x2;           // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1E = 0x1;           // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT_H =               // High counter value (TIM2 only) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CNT_CNT_L =               // Low counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR_H =               // High Auto-reload value (TIM2 only) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ARR_ARR_L =               // Low Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR1_CCR1_H =              // High Capture/Compare 1 value (TIM2 only) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR1_CCR1_L =              // Low Capture/Compare 1 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR2_CCR2_H =              // High Capture/Compare 2 value (TIM2 only) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR2_CCR2_L =              // Low Capture/Compare 2 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR3_CCR3_H =              // High Capture/Compare value (TIM2 only) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR3_CCR3_L =              // Low Capture/Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR4_CCR4_H =              // High Capture/Compare value (TIM2 only) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR4_CCR4_L =              // Low Capture/Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DMAR_DMAR =                // DMA register for burst accesses (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM3 = 16; // TIM3 global interrupt
};

static tim3_t& TIM3 = *reinterpret_cast<tim3_t*>(0x40000400);

#define HAVE_PERIPHERAL_TIM3


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

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_CC1G = 0x2;           // Capture/compare 1 generation
    static constexpr uint32_t EGR_UG = 0x1;             // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC1S =                // Capture/Compare 1 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1F =                // Input capture 1 filter (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1PSC =              // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    static constexpr uint32_t CCMR1_OC1FE = 0x4;          // Output compare 1 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC1M =                // Output Compare 1 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output Compare 1 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC1NP = 0x8;          // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1P = 0x2;           // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1E = 0x1;           // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR1_CCR1 =                // Capture/Compare 1 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OR_RMP =                 // Timer input 1 remap (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t OR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM14 = 19; // TIM14 global interrupt
};

static tim14_t& TIM14 = *reinterpret_cast<tim14_t*>(0x40002000);

#define HAVE_PERIPHERAL_TIM14


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

    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_MMS =                 // Master mode selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_UDE = 0x100;          // Update DMA request enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_UG = 0x1;             // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // Low counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Low Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x0;
};

static tim6_t& TIM6 = *reinterpret_cast<tim6_t*>(0x40001000);

#define HAVE_PERIPHERAL_TIM6


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

    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_MMS =                 // Master mode selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_UDE = 0x100;          // Update DMA request enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_UG = 0x1;             // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // Low counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Low Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM7 = 18; // TIM7 global interrupt
};

static tim7_t& TIM7 = *reinterpret_cast<tim7_t*>(0x40001400);

#define HAVE_PERIPHERAL_TIM7


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

    static constexpr uint32_t IMR_MR0 = 0x1;            // Interrupt Mask on line 0
    static constexpr uint32_t IMR_MR1 = 0x2;            // Interrupt Mask on line 1
    static constexpr uint32_t IMR_MR2 = 0x4;            // Interrupt Mask on line 2
    static constexpr uint32_t IMR_MR3 = 0x8;            // Interrupt Mask on line 3
    static constexpr uint32_t IMR_MR4 = 0x10;           // Interrupt Mask on line 4
    static constexpr uint32_t IMR_MR5 = 0x20;           // Interrupt Mask on line 5
    static constexpr uint32_t IMR_MR6 = 0x40;           // Interrupt Mask on line 6
    static constexpr uint32_t IMR_MR7 = 0x80;           // Interrupt Mask on line 7
    static constexpr uint32_t IMR_MR8 = 0x100;          // Interrupt Mask on line 8
    static constexpr uint32_t IMR_MR9 = 0x200;          // Interrupt Mask on line 9
    static constexpr uint32_t IMR_MR10 = 0x400;         // Interrupt Mask on line 10
    static constexpr uint32_t IMR_MR11 = 0x800;         // Interrupt Mask on line 11
    static constexpr uint32_t IMR_MR12 = 0x1000;        // Interrupt Mask on line 12
    static constexpr uint32_t IMR_MR13 = 0x2000;        // Interrupt Mask on line 13
    static constexpr uint32_t IMR_MR14 = 0x4000;        // Interrupt Mask on line 14
    static constexpr uint32_t IMR_MR15 = 0x8000;        // Interrupt Mask on line 15
    static constexpr uint32_t IMR_MR16 = 0x10000;       // Interrupt Mask on line 16
    static constexpr uint32_t IMR_MR17 = 0x20000;       // Interrupt Mask on line 17
    static constexpr uint32_t IMR_MR18 = 0x40000;       // Interrupt Mask on line 18
    static constexpr uint32_t IMR_MR19 = 0x80000;       // Interrupt Mask on line 19
    static constexpr uint32_t IMR_MR20 = 0x100000;      // Interrupt Mask on line 20
    static constexpr uint32_t IMR_MR21 = 0x200000;      // Interrupt Mask on line 21
    static constexpr uint32_t IMR_MR22 = 0x400000;      // Interrupt Mask on line 22
    static constexpr uint32_t IMR_MR23 = 0x800000;      // Interrupt Mask on line 23
    static constexpr uint32_t IMR_MR24 = 0x1000000;     // Interrupt Mask on line 24
    static constexpr uint32_t IMR_MR25 = 0x2000000;     // Interrupt Mask on line 25
    static constexpr uint32_t IMR_MR26 = 0x4000000;     // Interrupt Mask on line 26
    static constexpr uint32_t IMR_MR27 = 0x8000000;     // Interrupt Mask on line 27
    static const uint32_t IMR_RESET_VALUE = 0xf940000;

    static constexpr uint32_t EMR_MR0 = 0x1;            // Event Mask on line 0
    static constexpr uint32_t EMR_MR1 = 0x2;            // Event Mask on line 1
    static constexpr uint32_t EMR_MR2 = 0x4;            // Event Mask on line 2
    static constexpr uint32_t EMR_MR3 = 0x8;            // Event Mask on line 3
    static constexpr uint32_t EMR_MR4 = 0x10;           // Event Mask on line 4
    static constexpr uint32_t EMR_MR5 = 0x20;           // Event Mask on line 5
    static constexpr uint32_t EMR_MR6 = 0x40;           // Event Mask on line 6
    static constexpr uint32_t EMR_MR7 = 0x80;           // Event Mask on line 7
    static constexpr uint32_t EMR_MR8 = 0x100;          // Event Mask on line 8
    static constexpr uint32_t EMR_MR9 = 0x200;          // Event Mask on line 9
    static constexpr uint32_t EMR_MR10 = 0x400;         // Event Mask on line 10
    static constexpr uint32_t EMR_MR11 = 0x800;         // Event Mask on line 11
    static constexpr uint32_t EMR_MR12 = 0x1000;        // Event Mask on line 12
    static constexpr uint32_t EMR_MR13 = 0x2000;        // Event Mask on line 13
    static constexpr uint32_t EMR_MR14 = 0x4000;        // Event Mask on line 14
    static constexpr uint32_t EMR_MR15 = 0x8000;        // Event Mask on line 15
    static constexpr uint32_t EMR_MR16 = 0x10000;       // Event Mask on line 16
    static constexpr uint32_t EMR_MR17 = 0x20000;       // Event Mask on line 17
    static constexpr uint32_t EMR_MR18 = 0x40000;       // Event Mask on line 18
    static constexpr uint32_t EMR_MR19 = 0x80000;       // Event Mask on line 19
    static constexpr uint32_t EMR_MR20 = 0x100000;      // Event Mask on line 20
    static constexpr uint32_t EMR_MR21 = 0x200000;      // Event Mask on line 21
    static constexpr uint32_t EMR_MR22 = 0x400000;      // Event Mask on line 22
    static constexpr uint32_t EMR_MR23 = 0x800000;      // Event Mask on line 23
    static constexpr uint32_t EMR_MR24 = 0x1000000;     // Event Mask on line 24
    static constexpr uint32_t EMR_MR25 = 0x2000000;     // Event Mask on line 25
    static constexpr uint32_t EMR_MR26 = 0x4000000;     // Event Mask on line 26
    static constexpr uint32_t EMR_MR27 = 0x8000000;     // Event Mask on line 27
    static const uint32_t EMR_RESET_VALUE = 0x0;

    static constexpr uint32_t RTSR_TR0 = 0x1;            // Rising trigger event configuration of line 0
    static constexpr uint32_t RTSR_TR1 = 0x2;            // Rising trigger event configuration of line 1
    static constexpr uint32_t RTSR_TR2 = 0x4;            // Rising trigger event configuration of line 2
    static constexpr uint32_t RTSR_TR3 = 0x8;            // Rising trigger event configuration of line 3
    static constexpr uint32_t RTSR_TR4 = 0x10;           // Rising trigger event configuration of line 4
    static constexpr uint32_t RTSR_TR5 = 0x20;           // Rising trigger event configuration of line 5
    static constexpr uint32_t RTSR_TR6 = 0x40;           // Rising trigger event configuration of line 6
    static constexpr uint32_t RTSR_TR7 = 0x80;           // Rising trigger event configuration of line 7
    static constexpr uint32_t RTSR_TR8 = 0x100;          // Rising trigger event configuration of line 8
    static constexpr uint32_t RTSR_TR9 = 0x200;          // Rising trigger event configuration of line 9
    static constexpr uint32_t RTSR_TR10 = 0x400;         // Rising trigger event configuration of line 10
    static constexpr uint32_t RTSR_TR11 = 0x800;         // Rising trigger event configuration of line 11
    static constexpr uint32_t RTSR_TR12 = 0x1000;        // Rising trigger event configuration of line 12
    static constexpr uint32_t RTSR_TR13 = 0x2000;        // Rising trigger event configuration of line 13
    static constexpr uint32_t RTSR_TR14 = 0x4000;        // Rising trigger event configuration of line 14
    static constexpr uint32_t RTSR_TR15 = 0x8000;        // Rising trigger event configuration of line 15
    static constexpr uint32_t RTSR_TR16 = 0x10000;       // Rising trigger event configuration of line 16
    static constexpr uint32_t RTSR_TR17 = 0x20000;       // Rising trigger event configuration of line 17
    static constexpr uint32_t RTSR_TR19 = 0x80000;       // Rising trigger event configuration of line 19
    static const uint32_t RTSR_RESET_VALUE = 0x0;

    static constexpr uint32_t FTSR_TR0 = 0x1;            // Falling trigger event configuration of line 0
    static constexpr uint32_t FTSR_TR1 = 0x2;            // Falling trigger event configuration of line 1
    static constexpr uint32_t FTSR_TR2 = 0x4;            // Falling trigger event configuration of line 2
    static constexpr uint32_t FTSR_TR3 = 0x8;            // Falling trigger event configuration of line 3
    static constexpr uint32_t FTSR_TR4 = 0x10;           // Falling trigger event configuration of line 4
    static constexpr uint32_t FTSR_TR5 = 0x20;           // Falling trigger event configuration of line 5
    static constexpr uint32_t FTSR_TR6 = 0x40;           // Falling trigger event configuration of line 6
    static constexpr uint32_t FTSR_TR7 = 0x80;           // Falling trigger event configuration of line 7
    static constexpr uint32_t FTSR_TR8 = 0x100;          // Falling trigger event configuration of line 8
    static constexpr uint32_t FTSR_TR9 = 0x200;          // Falling trigger event configuration of line 9
    static constexpr uint32_t FTSR_TR10 = 0x400;         // Falling trigger event configuration of line 10
    static constexpr uint32_t FTSR_TR11 = 0x800;         // Falling trigger event configuration of line 11
    static constexpr uint32_t FTSR_TR12 = 0x1000;        // Falling trigger event configuration of line 12
    static constexpr uint32_t FTSR_TR13 = 0x2000;        // Falling trigger event configuration of line 13
    static constexpr uint32_t FTSR_TR14 = 0x4000;        // Falling trigger event configuration of line 14
    static constexpr uint32_t FTSR_TR15 = 0x8000;        // Falling trigger event configuration of line 15
    static constexpr uint32_t FTSR_TR16 = 0x10000;       // Falling trigger event configuration of line 16
    static constexpr uint32_t FTSR_TR17 = 0x20000;       // Falling trigger event configuration of line 17
    static constexpr uint32_t FTSR_TR19 = 0x80000;       // Falling trigger event configuration of line 19
    static const uint32_t FTSR_RESET_VALUE = 0x0;

    static constexpr uint32_t SWIER_SWIER0 = 0x1;         // Software Interrupt on line 0
    static constexpr uint32_t SWIER_SWIER1 = 0x2;         // Software Interrupt on line 1
    static constexpr uint32_t SWIER_SWIER2 = 0x4;         // Software Interrupt on line 2
    static constexpr uint32_t SWIER_SWIER3 = 0x8;         // Software Interrupt on line 3
    static constexpr uint32_t SWIER_SWIER4 = 0x10;        // Software Interrupt on line 4
    static constexpr uint32_t SWIER_SWIER5 = 0x20;        // Software Interrupt on line 5
    static constexpr uint32_t SWIER_SWIER6 = 0x40;        // Software Interrupt on line 6
    static constexpr uint32_t SWIER_SWIER7 = 0x80;        // Software Interrupt on line 7
    static constexpr uint32_t SWIER_SWIER8 = 0x100;       // Software Interrupt on line 8
    static constexpr uint32_t SWIER_SWIER9 = 0x200;       // Software Interrupt on line 9
    static constexpr uint32_t SWIER_SWIER10 = 0x400;      // Software Interrupt on line 10
    static constexpr uint32_t SWIER_SWIER11 = 0x800;      // Software Interrupt on line 11
    static constexpr uint32_t SWIER_SWIER12 = 0x1000;     // Software Interrupt on line 12
    static constexpr uint32_t SWIER_SWIER13 = 0x2000;     // Software Interrupt on line 13
    static constexpr uint32_t SWIER_SWIER14 = 0x4000;     // Software Interrupt on line 14
    static constexpr uint32_t SWIER_SWIER15 = 0x8000;     // Software Interrupt on line 15
    static constexpr uint32_t SWIER_SWIER16 = 0x10000;    // Software Interrupt on line 16
    static constexpr uint32_t SWIER_SWIER17 = 0x20000;    // Software Interrupt on line 17
    static constexpr uint32_t SWIER_SWIER19 = 0x80000;    // Software Interrupt on line 19
    static const uint32_t SWIER_RESET_VALUE = 0x0;

    static constexpr uint32_t PR_PIF31 = 0x80000000;   // Pending interrupt flag on line 31
    static constexpr uint32_t PR_PIF22 = 0x400000;     // Pending interrupt flag on line 22
    static constexpr uint32_t PR_PIF21 = 0x200000;     // Pending interrupt flag on line 21
    static constexpr uint32_t PR_PIF20 = 0x100000;     // Pending interrupt flag on line 20
    static constexpr uint32_t PR_PIF19 = 0x80000;      // Pending interrupt flag on line 19
    static constexpr uint32_t PR_PIF17 = 0x20000;      // Pending interrupt flag on line 17
    static constexpr uint32_t PR_PIF16 = 0x10000;      // Pending interrupt flag on line 16
    static constexpr uint32_t PR_PIF15 = 0x8000;       // Pending interrupt flag on line 15
    static constexpr uint32_t PR_PIF14 = 0x4000;       // Pending interrupt flag on line 14
    static constexpr uint32_t PR_PIF13 = 0x2000;       // Pending interrupt flag on line 13
    static constexpr uint32_t PR_PIF12 = 0x1000;       // Pending interrupt flag on line 12
    static constexpr uint32_t PR_PIF11 = 0x800;        // Pending interrupt flag on line 11
    static constexpr uint32_t PR_PIF10 = 0x400;        // Pending interrupt flag on line 10
    static constexpr uint32_t PR_PIF9 = 0x200;         // Pending interrupt flag on line 9
    static constexpr uint32_t PR_PIF8 = 0x100;         // Pending interrupt flag on line 8
    static constexpr uint32_t PR_PIF7 = 0x80;          // Pending interrupt flag on line 7
    static constexpr uint32_t PR_PIF6 = 0x40;          // Pending interrupt flag on line 6
    static constexpr uint32_t PR_PIF5 = 0x20;          // Pending interrupt flag on line 5
    static constexpr uint32_t PR_PIF4 = 0x10;          // Pending interrupt flag on line 4
    static constexpr uint32_t PR_PIF3 = 0x8;           // Pending interrupt flag on line 3
    static constexpr uint32_t PR_PIF2 = 0x4;           // Pending interrupt flag on line 2
    static constexpr uint32_t PR_PIF1 = 0x2;           // Pending interrupt flag on line 1
    static constexpr uint32_t PR_PIF0 = 0x1;           // Pending interrupt flag on line 0
    static const uint32_t PR_RESET_VALUE = 0x0;

    static constexpr uint8_t EXTI0_1 = 5; // EXTI Line[1:0] interrupts
    static constexpr uint8_t EXTI2_3 = 6; // EXTI Line[3:2] interrupts
    static constexpr uint8_t EXTI4_15 = 7; // EXTI Line15 and EXTI4 interrupts
    static constexpr uint8_t PVD = 1; // PVD and VDDIO2 supply comparator interrupt
};

static exti_t& EXTI = *reinterpret_cast<exti_t*>(0x40010400);

#define HAVE_PERIPHERAL_EXTI


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


    static const uint32_t ISER_RESET_VALUE = 0x0;


    static const uint32_t ICER_RESET_VALUE = 0x0;


    static const uint32_t ISPR_RESET_VALUE = 0x0;


    static const uint32_t ICPR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR0_PRI_00 =              // PRI_00 (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR0_PRI_01 =              // PRI_01 (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR0_PRI_02 =              // PRI_02 (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR0_PRI_03 =              // PRI_03 (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t IPR0_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR1_PRI_40 =              // PRI_40 (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR1_PRI_41 =              // PRI_41 (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR1_PRI_42 =              // PRI_42 (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR1_PRI_43 =              // PRI_43 (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t IPR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR2_PRI_80 =              // PRI_80 (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR2_PRI_81 =              // PRI_81 (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR2_PRI_82 =              // PRI_82 (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR2_PRI_83 =              // PRI_83 (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t IPR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR3_PRI_120 =             // PRI_120 (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR3_PRI_121 =             // PRI_121 (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR3_PRI_122 =             // PRI_122 (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR3_PRI_123 =             // PRI_123 (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t IPR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR4_PRI_160 =             // PRI_160 (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR4_PRI_161 =             // PRI_161 (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR4_PRI_162 =             // PRI_162 (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR4_PRI_163 =             // PRI_163 (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t IPR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR5_PRI_200 =             // PRI_200 (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR5_PRI_201 =             // PRI_201 (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR5_PRI_202 =             // PRI_202 (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR5_PRI_203 =             // PRI_203 (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t IPR5_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR6_PRI_240 =             // PRI_240 (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR6_PRI_241 =             // PRI_241 (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR6_PRI_242 =             // PRI_242 (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR6_PRI_243 =             // PRI_243 (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t IPR6_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR7_PRI_280 =             // PRI_280 (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR7_PRI_281 =             // PRI_281 (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR7_PRI_282 =             // PRI_282 (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR7_PRI_283 =             // PRI_283 (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t IPR7_RESET_VALUE = 0x0;
};

static nvic_t& NVIC = *reinterpret_cast<nvic_t*>(0xe000e100);

#define HAVE_PERIPHERAL_NVIC


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

    static constexpr uint32_t ISR_GIF1 = 0x1;           // Channel 1 Global interrupt flag
    static constexpr uint32_t ISR_TCIF1 = 0x2;          // Channel 1 Transfer Complete flag
    static constexpr uint32_t ISR_HTIF1 = 0x4;          // Channel 1 Half Transfer Complete flag
    static constexpr uint32_t ISR_TEIF1 = 0x8;          // Channel 1 Transfer Error flag
    static constexpr uint32_t ISR_GIF2 = 0x10;          // Channel 2 Global interrupt flag
    static constexpr uint32_t ISR_TCIF2 = 0x20;         // Channel 2 Transfer Complete flag
    static constexpr uint32_t ISR_HTIF2 = 0x40;         // Channel 2 Half Transfer Complete flag
    static constexpr uint32_t ISR_TEIF2 = 0x80;         // Channel 2 Transfer Error flag
    static constexpr uint32_t ISR_GIF3 = 0x100;         // Channel 3 Global interrupt flag
    static constexpr uint32_t ISR_TCIF3 = 0x200;        // Channel 3 Transfer Complete flag
    static constexpr uint32_t ISR_HTIF3 = 0x400;        // Channel 3 Half Transfer Complete flag
    static constexpr uint32_t ISR_TEIF3 = 0x800;        // Channel 3 Transfer Error flag
    static constexpr uint32_t ISR_GIF4 = 0x1000;        // Channel 4 Global interrupt flag
    static constexpr uint32_t ISR_TCIF4 = 0x2000;       // Channel 4 Transfer Complete flag
    static constexpr uint32_t ISR_HTIF4 = 0x4000;       // Channel 4 Half Transfer Complete flag
    static constexpr uint32_t ISR_TEIF4 = 0x8000;       // Channel 4 Transfer Error flag
    static constexpr uint32_t ISR_GIF5 = 0x10000;       // Channel 5 Global interrupt flag
    static constexpr uint32_t ISR_TCIF5 = 0x20000;      // Channel 5 Transfer Complete flag
    static constexpr uint32_t ISR_HTIF5 = 0x40000;      // Channel 5 Half Transfer Complete flag
    static constexpr uint32_t ISR_TEIF5 = 0x80000;      // Channel 5 Transfer Error flag
    static constexpr uint32_t ISR_GIF6 = 0x100000;      // Channel 6 Global interrupt flag
    static constexpr uint32_t ISR_TCIF6 = 0x200000;     // Channel 6 Transfer Complete flag
    static constexpr uint32_t ISR_HTIF6 = 0x400000;     // Channel 6 Half Transfer Complete flag
    static constexpr uint32_t ISR_TEIF6 = 0x800000;     // Channel 6 Transfer Error flag
    static constexpr uint32_t ISR_GIF7 = 0x1000000;     // Channel 7 Global interrupt flag
    static constexpr uint32_t ISR_TCIF7 = 0x2000000;    // Channel 7 Transfer Complete flag
    static constexpr uint32_t ISR_HTIF7 = 0x4000000;    // Channel 7 Half Transfer Complete flag
    static constexpr uint32_t ISR_TEIF7 = 0x8000000;    // Channel 7 Transfer Error flag
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t IFCR_CGIF1 = 0x1;          // Channel 1 Global interrupt clear
    static constexpr uint32_t IFCR_CTCIF1 = 0x2;         // Channel 1 Transfer Complete clear
    static constexpr uint32_t IFCR_CHTIF1 = 0x4;         // Channel 1 Half Transfer clear
    static constexpr uint32_t IFCR_CTEIF1 = 0x8;         // Channel 1 Transfer Error clear
    static constexpr uint32_t IFCR_CGIF2 = 0x10;         // Channel 2 Global interrupt clear
    static constexpr uint32_t IFCR_CTCIF2 = 0x20;        // Channel 2 Transfer Complete clear
    static constexpr uint32_t IFCR_CHTIF2 = 0x40;        // Channel 2 Half Transfer clear
    static constexpr uint32_t IFCR_CTEIF2 = 0x80;        // Channel 2 Transfer Error clear
    static constexpr uint32_t IFCR_CGIF3 = 0x100;        // Channel 3 Global interrupt clear
    static constexpr uint32_t IFCR_CTCIF3 = 0x200;       // Channel 3 Transfer Complete clear
    static constexpr uint32_t IFCR_CHTIF3 = 0x400;       // Channel 3 Half Transfer clear
    static constexpr uint32_t IFCR_CTEIF3 = 0x800;       // Channel 3 Transfer Error clear
    static constexpr uint32_t IFCR_CGIF4 = 0x1000;       // Channel 4 Global interrupt clear
    static constexpr uint32_t IFCR_CTCIF4 = 0x2000;      // Channel 4 Transfer Complete clear
    static constexpr uint32_t IFCR_CHTIF4 = 0x4000;      // Channel 4 Half Transfer clear
    static constexpr uint32_t IFCR_CTEIF4 = 0x8000;      // Channel 4 Transfer Error clear
    static constexpr uint32_t IFCR_CGIF5 = 0x10000;      // Channel 5 Global interrupt clear
    static constexpr uint32_t IFCR_CTCIF5 = 0x20000;     // Channel 5 Transfer Complete clear
    static constexpr uint32_t IFCR_CHTIF5 = 0x40000;     // Channel 5 Half Transfer clear
    static constexpr uint32_t IFCR_CTEIF5 = 0x80000;     // Channel 5 Transfer Error clear
    static constexpr uint32_t IFCR_CGIF6 = 0x100000;     // Channel 6 Global interrupt clear
    static constexpr uint32_t IFCR_CTCIF6 = 0x200000;    // Channel 6 Transfer Complete clear
    static constexpr uint32_t IFCR_CHTIF6 = 0x400000;    // Channel 6 Half Transfer clear
    static constexpr uint32_t IFCR_CTEIF6 = 0x800000;    // Channel 6 Transfer Error clear
    static constexpr uint32_t IFCR_CGIF7 = 0x1000000;    // Channel 7 Global interrupt clear
    static constexpr uint32_t IFCR_CTCIF7 = 0x2000000;   // Channel 7 Transfer Complete clear
    static constexpr uint32_t IFCR_CHTIF7 = 0x4000000;   // Channel 7 Half Transfer clear
    static constexpr uint32_t IFCR_CTEIF7 = 0x8000000;   // Channel 7 Transfer Error clear
    static const uint32_t IFCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR1_EN = 0x1;             // Channel enable
    static constexpr uint32_t CCR1_TCIE = 0x2;           // Transfer complete interrupt enable
    static constexpr uint32_t CCR1_HTIE = 0x4;           // Half Transfer interrupt enable
    static constexpr uint32_t CCR1_TEIE = 0x8;           // Transfer error interrupt enable
    static constexpr uint32_t CCR1_DIR = 0x10;           // Data transfer direction
    static constexpr uint32_t CCR1_CIRC = 0x20;          // Circular mode
    static constexpr uint32_t CCR1_PINC = 0x40;          // Peripheral increment mode
    static constexpr uint32_t CCR1_MINC = 0x80;          // Memory increment mode
    template<uint32_t X>
    static constexpr uint32_t CCR1_PSIZE =               // Peripheral size (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR1_MSIZE =               // Memory size (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR1_PL =                  // Channel Priority level (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR1_MEM2MEM = 0x4000;     // Memory to memory mode
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR1_NDT =                 // Number of data to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR1_RESET_VALUE = 0x0;


    static const uint32_t CPAR1_RESET_VALUE = 0x0;


    static const uint32_t CMAR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR2_EN = 0x1;             // Channel enable
    static constexpr uint32_t CCR2_TCIE = 0x2;           // Transfer complete interrupt enable
    static constexpr uint32_t CCR2_HTIE = 0x4;           // Half Transfer interrupt enable
    static constexpr uint32_t CCR2_TEIE = 0x8;           // Transfer error interrupt enable
    static constexpr uint32_t CCR2_DIR = 0x10;           // Data transfer direction
    static constexpr uint32_t CCR2_CIRC = 0x20;          // Circular mode
    static constexpr uint32_t CCR2_PINC = 0x40;          // Peripheral increment mode
    static constexpr uint32_t CCR2_MINC = 0x80;          // Memory increment mode
    template<uint32_t X>
    static constexpr uint32_t CCR2_PSIZE =               // Peripheral size (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR2_MSIZE =               // Memory size (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR2_PL =                  // Channel Priority level (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR2_MEM2MEM = 0x4000;     // Memory to memory mode
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR2_NDT =                 // Number of data to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR2_RESET_VALUE = 0x0;


    static const uint32_t CPAR2_RESET_VALUE = 0x0;


    static const uint32_t CMAR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR3_EN = 0x1;             // Channel enable
    static constexpr uint32_t CCR3_TCIE = 0x2;           // Transfer complete interrupt enable
    static constexpr uint32_t CCR3_HTIE = 0x4;           // Half Transfer interrupt enable
    static constexpr uint32_t CCR3_TEIE = 0x8;           // Transfer error interrupt enable
    static constexpr uint32_t CCR3_DIR = 0x10;           // Data transfer direction
    static constexpr uint32_t CCR3_CIRC = 0x20;          // Circular mode
    static constexpr uint32_t CCR3_PINC = 0x40;          // Peripheral increment mode
    static constexpr uint32_t CCR3_MINC = 0x80;          // Memory increment mode
    template<uint32_t X>
    static constexpr uint32_t CCR3_PSIZE =               // Peripheral size (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR3_MSIZE =               // Memory size (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR3_PL =                  // Channel Priority level (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR3_MEM2MEM = 0x4000;     // Memory to memory mode
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR3_NDT =                 // Number of data to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR3_RESET_VALUE = 0x0;


    static const uint32_t CPAR3_RESET_VALUE = 0x0;


    static const uint32_t CMAR3_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR4_EN = 0x1;             // Channel enable
    static constexpr uint32_t CCR4_TCIE = 0x2;           // Transfer complete interrupt enable
    static constexpr uint32_t CCR4_HTIE = 0x4;           // Half Transfer interrupt enable
    static constexpr uint32_t CCR4_TEIE = 0x8;           // Transfer error interrupt enable
    static constexpr uint32_t CCR4_DIR = 0x10;           // Data transfer direction
    static constexpr uint32_t CCR4_CIRC = 0x20;          // Circular mode
    static constexpr uint32_t CCR4_PINC = 0x40;          // Peripheral increment mode
    static constexpr uint32_t CCR4_MINC = 0x80;          // Memory increment mode
    template<uint32_t X>
    static constexpr uint32_t CCR4_PSIZE =               // Peripheral size (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR4_MSIZE =               // Memory size (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR4_PL =                  // Channel Priority level (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR4_MEM2MEM = 0x4000;     // Memory to memory mode
    static const uint32_t CCR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR4_NDT =                 // Number of data to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR4_RESET_VALUE = 0x0;


    static const uint32_t CPAR4_RESET_VALUE = 0x0;


    static const uint32_t CMAR4_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR5_EN = 0x1;             // Channel enable
    static constexpr uint32_t CCR5_TCIE = 0x2;           // Transfer complete interrupt enable
    static constexpr uint32_t CCR5_HTIE = 0x4;           // Half Transfer interrupt enable
    static constexpr uint32_t CCR5_TEIE = 0x8;           // Transfer error interrupt enable
    static constexpr uint32_t CCR5_DIR = 0x10;           // Data transfer direction
    static constexpr uint32_t CCR5_CIRC = 0x20;          // Circular mode
    static constexpr uint32_t CCR5_PINC = 0x40;          // Peripheral increment mode
    static constexpr uint32_t CCR5_MINC = 0x80;          // Memory increment mode
    template<uint32_t X>
    static constexpr uint32_t CCR5_PSIZE =               // Peripheral size (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR5_MSIZE =               // Memory size (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR5_PL =                  // Channel Priority level (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR5_MEM2MEM = 0x4000;     // Memory to memory mode
    static const uint32_t CCR5_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR5_NDT =                 // Number of data to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR5_RESET_VALUE = 0x0;


    static const uint32_t CPAR5_RESET_VALUE = 0x0;


    static const uint32_t CMAR5_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR6_EN = 0x1;             // Channel enable
    static constexpr uint32_t CCR6_TCIE = 0x2;           // Transfer complete interrupt enable
    static constexpr uint32_t CCR6_HTIE = 0x4;           // Half Transfer interrupt enable
    static constexpr uint32_t CCR6_TEIE = 0x8;           // Transfer error interrupt enable
    static constexpr uint32_t CCR6_DIR = 0x10;           // Data transfer direction
    static constexpr uint32_t CCR6_CIRC = 0x20;          // Circular mode
    static constexpr uint32_t CCR6_PINC = 0x40;          // Peripheral increment mode
    static constexpr uint32_t CCR6_MINC = 0x80;          // Memory increment mode
    template<uint32_t X>
    static constexpr uint32_t CCR6_PSIZE =               // Peripheral size (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR6_MSIZE =               // Memory size (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR6_PL =                  // Channel Priority level (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR6_MEM2MEM = 0x4000;     // Memory to memory mode
    static const uint32_t CCR6_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR6_NDT =                 // Number of data to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR6_RESET_VALUE = 0x0;


    static const uint32_t CPAR6_RESET_VALUE = 0x0;


    static const uint32_t CMAR6_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR7_EN = 0x1;             // Channel enable
    static constexpr uint32_t CCR7_TCIE = 0x2;           // Transfer complete interrupt enable
    static constexpr uint32_t CCR7_HTIE = 0x4;           // Half Transfer interrupt enable
    static constexpr uint32_t CCR7_TEIE = 0x8;           // Transfer error interrupt enable
    static constexpr uint32_t CCR7_DIR = 0x10;           // Data transfer direction
    static constexpr uint32_t CCR7_CIRC = 0x20;          // Circular mode
    static constexpr uint32_t CCR7_PINC = 0x40;          // Peripheral increment mode
    static constexpr uint32_t CCR7_MINC = 0x80;          // Memory increment mode
    template<uint32_t X>
    static constexpr uint32_t CCR7_PSIZE =               // Peripheral size (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR7_MSIZE =               // Memory size (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR7_PL =                  // Channel Priority level (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR7_MEM2MEM = 0x4000;     // Memory to memory mode
    static const uint32_t CCR7_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR7_NDT =                 // Number of data to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR7_RESET_VALUE = 0x0;


    static const uint32_t CPAR7_RESET_VALUE = 0x0;


    static const uint32_t CMAR7_RESET_VALUE = 0x0;

    static constexpr uint8_t DMA1_CH1 = 9; // DMA1 channel 1 interrupt
};

static dma1_t& DMA1 = *reinterpret_cast<dma1_t*>(0x40020000);

#define HAVE_PERIPHERAL_DMA1


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

    static constexpr uint32_t CR_HSION = 0x1;          // Internal High Speed clock enable, Read-write
    static constexpr uint32_t CR_HSIRDY = 0x2;         // Internal High Speed clock ready flag, Read-only
    template<uint32_t X>
    static constexpr uint32_t CR_HSITRIM =             // Internal High Speed clock trimming (5 bits), Read-write
        bit_field_t<3, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_HSICAL =              // Internal High Speed clock Calibration (8 bits), Read-only
        bit_field_t<8, 0xff>::value<X>();
    static constexpr uint32_t CR_HSEON = 0x10000;      // External High Speed clock enable, Read-write
    static constexpr uint32_t CR_HSERDY = 0x20000;     // External High Speed clock ready flag, Read-only
    static constexpr uint32_t CR_HSEBYP = 0x40000;     // External High Speed clock Bypass, Read-write
    static constexpr uint32_t CR_CSSON = 0x80000;      // Clock Security System enable, Read-write
    static constexpr uint32_t CR_PLLON = 0x1000000;    // PLL enable, Read-write
    static constexpr uint32_t CR_PLLRDY = 0x2000000;   // PLL clock ready flag, Read-only
    static const uint32_t CR_RESET_VALUE = 0x83;

    template<uint32_t X>
    static constexpr uint32_t CFGR_SW =                  // System clock Switch (2 bits), Read-write
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_SWS =                 // System Clock Switch Status (2 bits), Read-only
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_HPRE =                // AHB prescaler (4 bits), Read-write
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_PPRE =                // APB Low speed prescaler (APB1) (3 bits), Read-write
        bit_field_t<8, 0x7>::value<X>();
    static constexpr uint32_t CFGR_ADCPRE = 0x4000;      // ADC prescaler, Read-write
    template<uint32_t X>
    static constexpr uint32_t CFGR_PLLSRC =              // PLL input clock source (2 bits), Read-write
        bit_field_t<15, 0x3>::value<X>();
    static constexpr uint32_t CFGR_PLLXTPRE = 0x20000;   // HSE divider for PLL entry, Read-write
    template<uint32_t X>
    static constexpr uint32_t CFGR_PLLMUL =              // PLL Multiplication Factor (4 bits), Read-write
        bit_field_t<18, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_MCO =                 // Microcontroller clock output (3 bits), Read-write
        bit_field_t<24, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_MCOPRE =              // Microcontroller Clock Output Prescaler (3 bits), Read-write
        bit_field_t<28, 0x7>::value<X>();
    static constexpr uint32_t CFGR_PLLNODIV = 0x80000000;// PLL clock not divided for MCO, Read-write
    static const uint32_t CFGR_RESET_VALUE = 0x0;

    static constexpr uint32_t CIR_LSIRDYF = 0x1;        // LSI Ready Interrupt flag, Read-only
    static constexpr uint32_t CIR_LSERDYF = 0x2;        // LSE Ready Interrupt flag, Read-only
    static constexpr uint32_t CIR_HSIRDYF = 0x4;        // HSI Ready Interrupt flag, Read-only
    static constexpr uint32_t CIR_HSERDYF = 0x8;        // HSE Ready Interrupt flag, Read-only
    static constexpr uint32_t CIR_PLLRDYF = 0x10;       // PLL Ready Interrupt flag, Read-only
    static constexpr uint32_t CIR_HSI14RDYF = 0x20;     // HSI14 ready interrupt flag, Read-only
    static constexpr uint32_t CIR_HSI48RDYF = 0x40;     // HSI48 ready interrupt flag, Read-only
    static constexpr uint32_t CIR_CSSF = 0x80;          // Clock Security System Interrupt flag, Read-only
    static constexpr uint32_t CIR_LSIRDYIE = 0x100;     // LSI Ready Interrupt Enable, Read-write
    static constexpr uint32_t CIR_LSERDYIE = 0x200;     // LSE Ready Interrupt Enable, Read-write
    static constexpr uint32_t CIR_HSIRDYIE = 0x400;     // HSI Ready Interrupt Enable, Read-write
    static constexpr uint32_t CIR_HSERDYIE = 0x800;     // HSE Ready Interrupt Enable, Read-write
    static constexpr uint32_t CIR_PLLRDYIE = 0x1000;    // PLL Ready Interrupt Enable, Read-write
    static constexpr uint32_t CIR_HSI14RDYE = 0x2000;   // HSI14 ready interrupt enable, Read-write
    static constexpr uint32_t CIR_HSI48RDYIE = 0x4000;  // HSI48 ready interrupt enable, Read-write
    static constexpr uint32_t CIR_LSIRDYC = 0x10000;    // LSI Ready Interrupt Clear, Write-only
    static constexpr uint32_t CIR_LSERDYC = 0x20000;    // LSE Ready Interrupt Clear, Write-only
    static constexpr uint32_t CIR_HSIRDYC = 0x40000;    // HSI Ready Interrupt Clear, Write-only
    static constexpr uint32_t CIR_HSERDYC = 0x80000;    // HSE Ready Interrupt Clear, Write-only
    static constexpr uint32_t CIR_PLLRDYC = 0x100000;   // PLL Ready Interrupt Clear, Write-only
    static constexpr uint32_t CIR_HSI14RDYC = 0x200000; // HSI 14 MHz Ready Interrupt Clear, Write-only
    static constexpr uint32_t CIR_HSI48RDYC = 0x400000; // HSI48 Ready Interrupt Clear, Write-only
    static constexpr uint32_t CIR_CSSC = 0x800000;      // Clock security system interrupt clear, Write-only
    static const uint32_t CIR_RESET_VALUE = 0x0;

    static constexpr uint32_t APB2RSTR_SYSCFGRST = 0x1;      // SYSCFG and COMP reset
    static constexpr uint32_t APB2RSTR_ADCRST = 0x200;       // ADC interface reset
    static constexpr uint32_t APB2RSTR_TIM1RST = 0x800;      // TIM1 timer reset
    static constexpr uint32_t APB2RSTR_SPI1RST = 0x1000;     // SPI 1 reset
    static constexpr uint32_t APB2RSTR_USART1RST = 0x4000;   // USART1 reset
    static constexpr uint32_t APB2RSTR_TIM15RST = 0x10000;   // TIM15 timer reset
    static constexpr uint32_t APB2RSTR_TIM16RST = 0x20000;   // TIM16 timer reset
    static constexpr uint32_t APB2RSTR_TIM17RST = 0x40000;   // TIM17 timer reset
    static constexpr uint32_t APB2RSTR_DBGMCURST = 0x400000; // Debug MCU reset
    static const uint32_t APB2RSTR_RESET_VALUE = 0x0;

    static constexpr uint32_t APB1RSTR_TIM2RST = 0x1;        // Timer 2 reset
    static constexpr uint32_t APB1RSTR_TIM3RST = 0x2;        // Timer 3 reset
    static constexpr uint32_t APB1RSTR_TIM6RST = 0x10;       // Timer 6 reset
    static constexpr uint32_t APB1RSTR_TIM7RST = 0x20;       // TIM7 timer reset
    static constexpr uint32_t APB1RSTR_TIM14RST = 0x100;     // Timer 14 reset
    static constexpr uint32_t APB1RSTR_WWDGRST = 0x800;      // Window watchdog reset
    static constexpr uint32_t APB1RSTR_SPI2RST = 0x4000;     // SPI2 reset
    static constexpr uint32_t APB1RSTR_USART2RST = 0x20000;  // USART 2 reset
    static constexpr uint32_t APB1RSTR_USART3RST = 0x40000;  // USART3 reset
    static constexpr uint32_t APB1RSTR_USART4RST = 0x80000;  // USART4 reset
    static constexpr uint32_t APB1RSTR_I2C1RST = 0x200000;   // I2C1 reset
    static constexpr uint32_t APB1RSTR_I2C2RST = 0x400000;   // I2C2 reset
    static constexpr uint32_t APB1RSTR_USBRST = 0x800000;    // USB interface reset
    static constexpr uint32_t APB1RSTR_CANRST = 0x2000000;   // CAN interface reset
    static constexpr uint32_t APB1RSTR_CRSRST = 0x8000000;   // Clock Recovery System interface reset
    static constexpr uint32_t APB1RSTR_PWRRST = 0x10000000;  // Power interface reset
    static constexpr uint32_t APB1RSTR_DACRST = 0x20000000;  // DAC interface reset
    static constexpr uint32_t APB1RSTR_CECRST = 0x40000000;  // HDMI CEC reset
    static const uint32_t APB1RSTR_RESET_VALUE = 0x0;

    static constexpr uint32_t AHBENR_DMA1EN = 0x1;         // DMA1 clock enable
    static constexpr uint32_t AHBENR_SRAMEN = 0x4;         // SRAM interface clock enable
    static constexpr uint32_t AHBENR_FLITFEN = 0x10;       // FLITF clock enable
    static constexpr uint32_t AHBENR_CRCEN = 0x40;         // CRC clock enable
    static constexpr uint32_t AHBENR_IOPAEN = 0x20000;     // I/O port A clock enable
    static constexpr uint32_t AHBENR_IOPBEN = 0x40000;     // I/O port B clock enable
    static constexpr uint32_t AHBENR_IOPCEN = 0x80000;     // I/O port C clock enable
    static constexpr uint32_t AHBENR_IOPDEN = 0x100000;    // I/O port D clock enable
    static constexpr uint32_t AHBENR_IOPFEN = 0x400000;    // I/O port F clock enable
    static constexpr uint32_t AHBENR_TSCEN = 0x1000000;    // Touch sensing controller clock enable
    static const uint32_t AHBENR_RESET_VALUE = 0x14;

    static constexpr uint32_t APB2ENR_SYSCFGEN = 0x1;       // SYSCFG clock enable
    static constexpr uint32_t APB2ENR_ADCEN = 0x200;        // ADC 1 interface clock enable
    static constexpr uint32_t APB2ENR_TIM1EN = 0x800;       // TIM1 Timer clock enable
    static constexpr uint32_t APB2ENR_SPI1EN = 0x1000;      // SPI 1 clock enable
    static constexpr uint32_t APB2ENR_USART1EN = 0x4000;    // USART1 clock enable
    static constexpr uint32_t APB2ENR_TIM15EN = 0x10000;    // TIM15 timer clock enable
    static constexpr uint32_t APB2ENR_TIM16EN = 0x20000;    // TIM16 timer clock enable
    static constexpr uint32_t APB2ENR_TIM17EN = 0x40000;    // TIM17 timer clock enable
    static constexpr uint32_t APB2ENR_DBGMCUEN = 0x400000;  // MCU debug module clock enable
    static const uint32_t APB2ENR_RESET_VALUE = 0x0;

    static constexpr uint32_t APB1ENR_TIM2EN = 0x1;         // Timer 2 clock enable
    static constexpr uint32_t APB1ENR_TIM3EN = 0x2;         // Timer 3 clock enable
    static constexpr uint32_t APB1ENR_TIM6EN = 0x10;        // Timer 6 clock enable
    static constexpr uint32_t APB1ENR_TIM7EN = 0x20;        // TIM7 timer clock enable
    static constexpr uint32_t APB1ENR_TIM14EN = 0x100;      // Timer 14 clock enable
    static constexpr uint32_t APB1ENR_WWDGEN = 0x800;       // Window watchdog clock enable
    static constexpr uint32_t APB1ENR_SPI2EN = 0x4000;      // SPI 2 clock enable
    static constexpr uint32_t APB1ENR_USART2EN = 0x20000;   // USART 2 clock enable
    static constexpr uint32_t APB1ENR_USART3EN = 0x40000;   // USART3 clock enable
    static constexpr uint32_t APB1ENR_USART4EN = 0x80000;   // USART4 clock enable
    static constexpr uint32_t APB1ENR_I2C1EN = 0x200000;    // I2C 1 clock enable
    static constexpr uint32_t APB1ENR_I2C2EN = 0x400000;    // I2C 2 clock enable
    static constexpr uint32_t APB1ENR_USBRST = 0x800000;    // USB interface clock enable
    static constexpr uint32_t APB1ENR_CANEN = 0x2000000;    // CAN interface clock enable
    static constexpr uint32_t APB1ENR_CRSEN = 0x8000000;    // Clock Recovery System interface clock enable
    static constexpr uint32_t APB1ENR_PWREN = 0x10000000;   // Power interface clock enable
    static constexpr uint32_t APB1ENR_DACEN = 0x20000000;   // DAC interface clock enable
    static constexpr uint32_t APB1ENR_CECEN = 0x40000000;   // HDMI CEC interface clock enable
    static const uint32_t APB1ENR_RESET_VALUE = 0x0;

    static constexpr uint32_t BDCR_LSEON = 0x1;          // External Low Speed oscillator enable, Read-write
    static constexpr uint32_t BDCR_LSERDY = 0x2;         // External Low Speed oscillator ready, Read-only
    static constexpr uint32_t BDCR_LSEBYP = 0x4;         // External Low Speed oscillator bypass, Read-write
    template<uint32_t X>
    static constexpr uint32_t BDCR_LSEDRV =              // LSE oscillator drive capability (2 bits), Read-write
        bit_field_t<3, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDCR_RTCSEL =              // RTC clock source selection (2 bits), Read-write
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t BDCR_RTCEN = 0x8000;       // RTC clock enable, Read-write
    static constexpr uint32_t BDCR_BDRST = 0x10000;      // Backup domain software reset, Read-write
    static const uint32_t BDCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CSR_LSION = 0x1;          // Internal low speed oscillator enable, Read-write
    static constexpr uint32_t CSR_LSIRDY = 0x2;         // Internal low speed oscillator ready, Read-only
    static constexpr uint32_t CSR_RMVF = 0x1000000;     // Remove reset flag, Read-write
    static constexpr uint32_t CSR_OBLRSTF = 0x2000000;  // Option byte loader reset flag, Read-write
    static constexpr uint32_t CSR_PINRSTF = 0x4000000;  // PIN reset flag, Read-write
    static constexpr uint32_t CSR_PORRSTF = 0x8000000;  // POR/PDR reset flag, Read-write
    static constexpr uint32_t CSR_SFTRSTF = 0x10000000; // Software reset flag, Read-write
    static constexpr uint32_t CSR_IWDGRSTF = 0x20000000;// Independent watchdog reset flag, Read-write
    static constexpr uint32_t CSR_WWDGRSTF = 0x40000000;// Window watchdog reset flag, Read-write
    static constexpr uint32_t CSR_LPWRRSTF = 0x80000000;// Low-power reset flag, Read-write
    static const uint32_t CSR_RESET_VALUE = 0xc000000;

    static constexpr uint32_t AHBRSTR_IOPARST = 0x20000;    // I/O port A reset
    static constexpr uint32_t AHBRSTR_IOPBRST = 0x40000;    // I/O port B reset
    static constexpr uint32_t AHBRSTR_IOPCRST = 0x80000;    // I/O port C reset
    static constexpr uint32_t AHBRSTR_IOPDRST = 0x100000;   // I/O port D reset
    static constexpr uint32_t AHBRSTR_IOPFRST = 0x400000;   // I/O port F reset
    static constexpr uint32_t AHBRSTR_TSCRST = 0x1000000;   // Touch sensing controller reset
    static const uint32_t AHBRSTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CFGR2_PREDIV =              // PREDIV division factor (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CFGR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CFGR3_USART1SW =            // USART1 clock source selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static constexpr uint32_t CFGR3_I2C1SW = 0x10;        // I2C1 clock source selection
    static constexpr uint32_t CFGR3_CECSW = 0x40;         // HDMI CEC clock source selection
    static constexpr uint32_t CFGR3_USBSW = 0x80;         // USB clock source selection
    static constexpr uint32_t CFGR3_ADCSW = 0x100;        // ADC clock source selection
    template<uint32_t X>
    static constexpr uint32_t CFGR3_USART2SW =            // USART2 clock source selection (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    static const uint32_t CFGR3_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_HSI14ON = 0x1;        // HSI14 clock enable, Read-write
    static constexpr uint32_t CR2_HSI14RDY = 0x2;       // HR14 clock ready flag, Read-only
    static constexpr uint32_t CR2_HSI14DIS = 0x4;       // HSI14 clock request from ADC disable, Read-write
    template<uint32_t X>
    static constexpr uint32_t CR2_HSI14TRIM =           // HSI14 clock trimming (5 bits), Read-write
        bit_field_t<3, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_HSI14CAL =            // HSI14 clock calibration (8 bits), Read-only
        bit_field_t<8, 0xff>::value<X>();
    static constexpr uint32_t CR2_HSI48ON = 0x10000;    // HSI48 clock enable, Read-write
    static constexpr uint32_t CR2_HSI48RDY = 0x20000;   // HSI48 clock ready flag, Read-only
    static constexpr uint32_t CR2_HSI48CAL = 0x1000000; // HSI48 factory clock calibration, Read-only
    static const uint32_t CR2_RESET_VALUE = 0x80;

    static constexpr uint8_t RCC_CRS = 4; // RCC and CRS global interrupts
};

static rcc_t& RCC = *reinterpret_cast<rcc_t*>(0x40021000);

#define HAVE_PERIPHERAL_RCC


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

    template<uint32_t X>
    static constexpr uint32_t SYSCFG_CFGR1_MEM_MODE =            // Memory mapping selection bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static constexpr uint32_t SYSCFG_CFGR1_ADC_DMA_RMP = 0x100;  // ADC DMA remapping bit
    static constexpr uint32_t SYSCFG_CFGR1_USART1_TX_DMA_RMP = 0x200;// USART1_TX DMA remapping bit
    static constexpr uint32_t SYSCFG_CFGR1_USART1_RX_DMA_RMP = 0x400;// USART1_RX DMA request remapping bit
    static constexpr uint32_t SYSCFG_CFGR1_TIM16_DMA_RMP = 0x800;// TIM16 DMA request remapping bit
    static constexpr uint32_t SYSCFG_CFGR1_TIM17_DMA_RMP = 0x1000;// TIM17 DMA request remapping bit
    static constexpr uint32_t SYSCFG_CFGR1_I2C_PB6_FM = 0x10000; // Fast Mode Plus (FM plus) driving capability activation bits.
    static constexpr uint32_t SYSCFG_CFGR1_I2C_PB7_FM = 0x20000; // Fast Mode Plus (FM+) driving capability activation bits.
    static constexpr uint32_t SYSCFG_CFGR1_I2C_PB8_FM = 0x40000; // Fast Mode Plus (FM+) driving capability activation bits.
    static constexpr uint32_t SYSCFG_CFGR1_I2C_PB9_FM = 0x80000; // Fast Mode Plus (FM+) driving capability activation bits.
    static constexpr uint32_t SYSCFG_CFGR1_I2C1_FM_plus = 0x100000;// FM+ driving capability activation for I2C1
    static constexpr uint32_t SYSCFG_CFGR1_I2C2_FM_plus = 0x200000;// FM+ driving capability activation for I2C2
    static constexpr uint32_t SYSCFG_CFGR1_SPI2_DMA_RMP = 0x1000000;// SPI2 DMA request remapping bit
    static constexpr uint32_t SYSCFG_CFGR1_USART2_DMA_RMP = 0x2000000;// USART2 DMA request remapping bit
    static constexpr uint32_t SYSCFG_CFGR1_USART3_DMA_RMP = 0x4000000;// USART3 DMA request remapping bit
    static constexpr uint32_t SYSCFG_CFGR1_I2C1_DMA_RMP = 0x8000000;// I2C1 DMA request remapping bit
    static constexpr uint32_t SYSCFG_CFGR1_TIM1_DMA_RMP = 0x10000000;// TIM1 DMA request remapping bit
    static constexpr uint32_t SYSCFG_CFGR1_TIM2_DMA_RMP = 0x20000000;// TIM2 DMA request remapping bit
    static constexpr uint32_t SYSCFG_CFGR1_TIM3_DMA_RMP = 0x40000000;// TIM3 DMA request remapping bit
    static const uint32_t SYSCFG_CFGR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR1_EXTI3 =               // EXTI 3 configuration bits (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR1_EXTI2 =               // EXTI 2 configuration bits (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR1_EXTI1 =               // EXTI 1 configuration bits (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR1_EXTI0 =               // EXTI 0 configuration bits (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t SYSCFG_EXTICR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR2_EXTI7 =               // EXTI 7 configuration bits (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR2_EXTI6 =               // EXTI 6 configuration bits (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR2_EXTI5 =               // EXTI 5 configuration bits (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR2_EXTI4 =               // EXTI 4 configuration bits (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t SYSCFG_EXTICR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR3_EXTI11 =              // EXTI 11 configuration bits (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR3_EXTI10 =              // EXTI 10 configuration bits (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR3_EXTI9 =               // EXTI 9 configuration bits (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR3_EXTI8 =               // EXTI 8 configuration bits (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t SYSCFG_EXTICR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR4_EXTI15 =              // EXTI 15 configuration bits (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR4_EXTI14 =              // EXTI 14 configuration bits (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR4_EXTI13 =              // EXTI 13 configuration bits (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SYSCFG_EXTICR4_EXTI12 =              // EXTI 12 configuration bits (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t SYSCFG_EXTICR4_RESET_VALUE = 0x0;

    static constexpr uint32_t SYSCFG_CFGR2_SRAM_PEF = 0x100;     // SRAM parity flag
    static constexpr uint32_t SYSCFG_CFGR2_PVD_LOCK = 0x4;       // PVD lock enable bit
    static constexpr uint32_t SYSCFG_CFGR2_SRAM_PARITY_LOCK = 0x2;// SRAM parity lock bit
    static constexpr uint32_t SYSCFG_CFGR2_LOCUP_LOCK = 0x1;     // Cortex-M0 LOCKUP bit enable bit
    static const uint32_t SYSCFG_CFGR2_RESET_VALUE = 0x0;

    static constexpr uint32_t COMP_CSR_COMP1EN = 0x1;        // Comparator 1 enable, Read-write
    static constexpr uint32_t COMP_CSR_COMP1_INP_DAC = 0x2;  // COMP1_INP_DAC, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_CSR_COMP1MODE =           // Comparator 1 mode (2 bits), Read-write
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP_CSR_COMP1INSEL =          // Comparator 1 inverting input selection (3 bits), Read-write
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP_CSR_COMP1OUTSEL =         // Comparator 1 output selection (3 bits), Read-write
        bit_field_t<8, 0x7>::value<X>();
    static constexpr uint32_t COMP_CSR_COMP1POL = 0x800;     // Comparator 1 output polarity, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_CSR_COMP1HYST =           // Comparator 1 hysteresis (2 bits), Read-write
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t COMP_CSR_COMP1OUT = 0x4000;    // Comparator 1 output, Read-only
    static constexpr uint32_t COMP_CSR_COMP1LOCK = 0x8000;   // Comparator 1 lock, Read-write
    static constexpr uint32_t COMP_CSR_COMP2EN = 0x10000;    // Comparator 2 enable, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_CSR_COMP2MODE =           // Comparator 2 mode (2 bits), Read-write
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP_CSR_COMP2INSEL =          // Comparator 2 inverting input selection (3 bits), Read-write
        bit_field_t<20, 0x7>::value<X>();
    static constexpr uint32_t COMP_CSR_WNDWEN = 0x800000;    // Window mode enable, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_CSR_COMP2OUTSEL =         // Comparator 2 output selection (3 bits), Read-write
        bit_field_t<24, 0x7>::value<X>();
    static constexpr uint32_t COMP_CSR_COMP2POL = 0x8000000; // Comparator 2 output polarity, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_CSR_COMP2HYST =           // Comparator 2 hysteresis (2 bits), Read-write
        bit_field_t<28, 0x3>::value<X>();
    static constexpr uint32_t COMP_CSR_COMP2OUT = 0x40000000;// Comparator 2 output, Read-only
    static constexpr uint32_t COMP_CSR_COMP2LOCK = 0x80000000;// Comparator 2 lock, Read-write
    static const uint32_t COMP_CSR_RESET_VALUE = 0x0;
};

static syscfg_comp_t& SYSCFG_COMP = *reinterpret_cast<syscfg_comp_t*>(0x40010000);

#define HAVE_PERIPHERAL_SYSCFG_COMP


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

    static constexpr uint32_t ISR_AWD = 0x80;           // Analog watchdog flag
    static constexpr uint32_t ISR_OVR = 0x10;           // ADC overrun
    static constexpr uint32_t ISR_EOS = 0x8;            // End of sequence flag
    static constexpr uint32_t ISR_EOC = 0x4;            // End of conversion flag
    static constexpr uint32_t ISR_EOSMP = 0x2;          // End of sampling flag
    static constexpr uint32_t ISR_ADRDY = 0x1;          // ADC ready
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t IER_AWDIE = 0x80;         // Analog watchdog interrupt enable
    static constexpr uint32_t IER_OVRIE = 0x10;         // Overrun interrupt enable
    static constexpr uint32_t IER_EOSIE = 0x8;          // End of conversion sequence interrupt enable
    static constexpr uint32_t IER_EOCIE = 0x4;          // End of conversion interrupt enable
    static constexpr uint32_t IER_EOSMPIE = 0x2;        // End of sampling flag interrupt enable
    static constexpr uint32_t IER_ADRDYIE = 0x1;        // ADC ready interrupt enable
    static const uint32_t IER_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_ADCAL = 0x80000000;   // ADC calibration
    static constexpr uint32_t CR_ADSTP = 0x10;         // ADC stop conversion command
    static constexpr uint32_t CR_ADSTART = 0x4;        // ADC start conversion command
    static constexpr uint32_t CR_ADDIS = 0x2;          // ADC disable command
    static constexpr uint32_t CR_ADEN = 0x1;           // ADC enable command
    static const uint32_t CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CFGR1_AWDCH =               // Analog watchdog channel selection (5 bits)
        bit_field_t<26, 0x1f>::value<X>();
    static constexpr uint32_t CFGR1_AWDEN = 0x800000;     // Analog watchdog enable
    static constexpr uint32_t CFGR1_AWDSGL = 0x400000;    // Enable the watchdog on a single channel or on all channels
    static constexpr uint32_t CFGR1_DISCEN = 0x10000;     // Discontinuous mode
    static constexpr uint32_t CFGR1_AUTOFF = 0x8000;      // Auto-off mode
    static constexpr uint32_t CFGR1_AUTDLY = 0x4000;      // Auto-delayed conversion mode
    static constexpr uint32_t CFGR1_CONT = 0x2000;        // Single / continuous conversion mode
    static constexpr uint32_t CFGR1_OVRMOD = 0x1000;      // Overrun management mode
    template<uint32_t X>
    static constexpr uint32_t CFGR1_EXTEN =               // External trigger enable and polarity selection (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR1_EXTSEL =              // External trigger selection (3 bits)
        bit_field_t<6, 0x7>::value<X>();
    static constexpr uint32_t CFGR1_ALIGN = 0x20;         // Data alignment
    template<uint32_t X>
    static constexpr uint32_t CFGR1_RES =                 // Data resolution (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t CFGR1_SCANDIR = 0x4;        // Scan sequence direction
    static constexpr uint32_t CFGR1_DMACFG = 0x2;         // Direct memery access configuration
    static constexpr uint32_t CFGR1_DMAEN = 0x1;          // Direct memory access enable
    static const uint32_t CFGR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CFGR2_JITOFF_D4 = 0x80000000;// JITOFF_D4
    static constexpr uint32_t CFGR2_JITOFF_D2 = 0x40000000;// JITOFF_D2
    static const uint32_t CFGR2_RESET_VALUE = 0x8000;

    template<uint32_t X>
    static constexpr uint32_t SMPR_SMPR =                // Sampling time selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t SMPR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TR_HT =                  // Analog watchdog higher threshold (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR_LT =                  // Analog watchdog lower threshold (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t TR_RESET_VALUE = 0xfff;

    static constexpr uint32_t CHSELR_CHSEL18 = 0x40000;    // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL17 = 0x20000;    // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL16 = 0x10000;    // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL15 = 0x8000;     // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL14 = 0x4000;     // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL13 = 0x2000;     // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL12 = 0x1000;     // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL11 = 0x800;      // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL10 = 0x400;      // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL9 = 0x200;       // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL8 = 0x100;       // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL7 = 0x80;        // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL6 = 0x40;        // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL5 = 0x20;        // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL4 = 0x10;        // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL3 = 0x8;         // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL2 = 0x4;         // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL1 = 0x2;         // Channel-x selection
    static constexpr uint32_t CHSELR_CHSEL0 = 0x1;         // Channel-x selection
    static const uint32_t CHSELR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR_DATA =                // Converted data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR_VBATEN = 0x1000000;   // VBAT enable
    static constexpr uint32_t CCR_TSEN = 0x800000;      // Temperature sensor enable
    static constexpr uint32_t CCR_VREFEN = 0x400000;    // Temperature sensor and VREFINT enable
    static const uint32_t CCR_RESET_VALUE = 0x0;

    static constexpr uint8_t ADC_COMP = 12; // ADC and comparator interrupts
};

static adc_t& ADC = *reinterpret_cast<adc_t*>(0x40012400);

#define HAVE_PERIPHERAL_ADC


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

    static constexpr uint32_t CR1_UE = 0x1;             // USART enable
    static constexpr uint32_t CR1_UESM = 0x2;           // USART enable in Stop mode
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // interrupt enable
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_WAKE = 0x800;         // Receiver wakeup method
    static constexpr uint32_t CR1_M = 0x1000;           // Word length
    static constexpr uint32_t CR1_MME = 0x2000;         // Mute mode enable
    static constexpr uint32_t CR1_CMIE = 0x4000;        // Character match interrupt enable
    static constexpr uint32_t CR1_OVER8 = 0x8000;       // Oversampling mode
    template<uint32_t X>
    static constexpr uint32_t CR1_DEDT =                // Driver Enable deassertion time (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR1_DEAT =                // Driver Enable assertion time (5 bits)
        bit_field_t<21, 0x1f>::value<X>();
    static constexpr uint32_t CR1_RTOIE = 0x4000000;    // Receiver timeout interrupt enable
    static constexpr uint32_t CR1_EOBIE = 0x8000000;    // End of Block interrupt enable
    static constexpr uint32_t CR1_M1 = 0x10000000;      // Word length
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_ADD4 =                // Address of the USART node (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_ADD0 =                // Address of the USART node (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t CR2_RTOEN = 0x800000;     // Receiver timeout enable
    template<uint32_t X>
    static constexpr uint32_t CR2_ABRMOD =              // Auto baud rate mode (2 bits)
        bit_field_t<21, 0x3>::value<X>();
    static constexpr uint32_t CR2_ABREN = 0x100000;     // Auto baud rate enable
    static constexpr uint32_t CR2_MSBFIRST = 0x80000;   // Most significant bit first
    static constexpr uint32_t CR2_DATAINV = 0x40000;    // Binary data inversion
    static constexpr uint32_t CR2_TXINV = 0x20000;      // TX pin active level inversion
    static constexpr uint32_t CR2_RXINV = 0x10000;      // RX pin active level inversion
    static constexpr uint32_t CR2_SWAP = 0x8000;        // Swap TX/RX pins
    static constexpr uint32_t CR2_LINEN = 0x4000;       // LIN mode enable
    template<uint32_t X>
    static constexpr uint32_t CR2_STOP =                // STOP bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CR2_CLKEN = 0x800;        // Clock enable
    static constexpr uint32_t CR2_CPOL = 0x400;         // Clock polarity
    static constexpr uint32_t CR2_CPHA = 0x200;         // Clock phase
    static constexpr uint32_t CR2_LBCL = 0x100;         // Last bit clock pulse
    static constexpr uint32_t CR2_LBDIE = 0x40;         // LIN break detection interrupt enable
    static constexpr uint32_t CR2_LBDL = 0x20;          // LIN break detection length
    static constexpr uint32_t CR2_ADDM7 = 0x10;         // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CR3_WUFIE = 0x400000;     // Wakeup from Stop mode interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR3_WUS =                 // Wakeup from Stop mode interrupt flag selection (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR3_SCARCNT =             // Smartcard auto-retry count (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    static constexpr uint32_t CR3_DEP = 0x8000;         // Driver enable polarity selection
    static constexpr uint32_t CR3_DEM = 0x4000;         // Driver enable mode
    static constexpr uint32_t CR3_DDRE = 0x2000;        // DMA Disable on Reception Error
    static constexpr uint32_t CR3_OVRDIS = 0x1000;      // Overrun Disable
    static constexpr uint32_t CR3_ONEBIT = 0x800;       // One sample bit method enable
    static constexpr uint32_t CR3_CTSIE = 0x400;        // CTS interrupt enable
    static constexpr uint32_t CR3_CTSE = 0x200;         // CTS enable
    static constexpr uint32_t CR3_RTSE = 0x100;         // RTS enable
    static constexpr uint32_t CR3_DMAT = 0x80;          // DMA enable transmitter
    static constexpr uint32_t CR3_DMAR = 0x40;          // DMA enable receiver
    static constexpr uint32_t CR3_SCEN = 0x20;          // Smartcard mode enable
    static constexpr uint32_t CR3_NACK = 0x10;          // Smartcard NACK enable
    static constexpr uint32_t CR3_HDSEL = 0x8;          // Half-duplex selection
    static constexpr uint32_t CR3_IRLP = 0x4;           // IrDA low-power
    static constexpr uint32_t CR3_IREN = 0x2;           // IrDA mode enable
    static constexpr uint32_t CR3_EIE = 0x1;            // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Mantissa =        // mantissa of USARTDIV (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // fraction of USARTDIV (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BRR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t GTPR_GT =                  // Guard time value (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t GTPR_PSC =                 // Prescaler value (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RTOR_BLEN =                // Block Length (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RTOR_RTO =                 // Receiver timeout value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();
    static const uint32_t RTOR_RESET_VALUE = 0x0;

    static constexpr uint32_t RQR_TXFRQ = 0x10;         // Transmit data flush request
    static constexpr uint32_t RQR_RXFRQ = 0x8;          // Receive data flush request
    static constexpr uint32_t RQR_MMRQ = 0x4;           // Mute mode request
    static constexpr uint32_t RQR_SBKRQ = 0x2;          // Send break request
    static constexpr uint32_t RQR_ABRRQ = 0x1;          // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static constexpr uint32_t ISR_REACK = 0x400000;     // Receive enable acknowledge flag
    static constexpr uint32_t ISR_TEACK = 0x200000;     // Transmit enable acknowledge flag
    static constexpr uint32_t ISR_WUF = 0x100000;       // Wakeup from Stop mode flag
    static constexpr uint32_t ISR_RWU = 0x80000;        // Receiver wakeup from Mute mode
    static constexpr uint32_t ISR_SBKF = 0x40000;       // Send break flag
    static constexpr uint32_t ISR_CMF = 0x20000;        // character match flag
    static constexpr uint32_t ISR_BUSY = 0x10000;       // Busy flag
    static constexpr uint32_t ISR_ABRF = 0x8000;        // Auto baud rate flag
    static constexpr uint32_t ISR_ABRE = 0x4000;        // Auto baud rate error
    static constexpr uint32_t ISR_EOBF = 0x1000;        // End of block flag
    static constexpr uint32_t ISR_RTOF = 0x800;         // Receiver timeout
    static constexpr uint32_t ISR_CTS = 0x400;          // CTS flag
    static constexpr uint32_t ISR_CTSIF = 0x200;        // CTS interrupt flag
    static constexpr uint32_t ISR_LBDF = 0x100;         // LIN break detection flag
    static constexpr uint32_t ISR_TXE = 0x80;           // Transmit data register empty
    static constexpr uint32_t ISR_TC = 0x40;            // Transmission complete
    static constexpr uint32_t ISR_RXNE = 0x20;          // Read data register not empty
    static constexpr uint32_t ISR_IDLE = 0x10;          // Idle line detected
    static constexpr uint32_t ISR_ORE = 0x8;            // Overrun error
    static constexpr uint32_t ISR_NF = 0x4;             // Noise detected flag
    static constexpr uint32_t ISR_FE = 0x2;             // Framing error
    static constexpr uint32_t ISR_PE = 0x1;             // Parity error
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static constexpr uint32_t ICR_WUCF = 0x100000;      // Wakeup from Stop mode clear flag
    static constexpr uint32_t ICR_CMCF = 0x20000;       // Character match clear flag
    static constexpr uint32_t ICR_EOBCF = 0x1000;       // End of timeout clear flag
    static constexpr uint32_t ICR_RTOCF = 0x800;        // Receiver timeout clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
    static constexpr uint32_t ICR_LBDCF = 0x100;        // LIN break detection clear flag
    static constexpr uint32_t ICR_TCCF = 0x40;          // Transmission complete clear flag
    static constexpr uint32_t ICR_IDLECF = 0x10;        // Idle line detected clear flag
    static constexpr uint32_t ICR_ORECF = 0x8;          // Overrun error clear flag
    static constexpr uint32_t ICR_NCF = 0x4;            // Noise detected clear flag
    static constexpr uint32_t ICR_FECF = 0x2;           // Framing error clear flag
    static constexpr uint32_t ICR_PECF = 0x1;           // Parity error clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RDR_RDR =                 // Receive data value (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t RDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDR_TDR =                 // Transmit data value (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t TDR_RESET_VALUE = 0x0;

    static constexpr uint8_t USART1 = 27; // USART1 global interrupt
};

static usart1_t& USART1 = *reinterpret_cast<usart1_t*>(0x40013800);

#define HAVE_PERIPHERAL_USART1


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

    static constexpr uint32_t CR1_UE = 0x1;             // USART enable
    static constexpr uint32_t CR1_UESM = 0x2;           // USART enable in Stop mode
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // interrupt enable
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_WAKE = 0x800;         // Receiver wakeup method
    static constexpr uint32_t CR1_M = 0x1000;           // Word length
    static constexpr uint32_t CR1_MME = 0x2000;         // Mute mode enable
    static constexpr uint32_t CR1_CMIE = 0x4000;        // Character match interrupt enable
    static constexpr uint32_t CR1_OVER8 = 0x8000;       // Oversampling mode
    template<uint32_t X>
    static constexpr uint32_t CR1_DEDT =                // Driver Enable deassertion time (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR1_DEAT =                // Driver Enable assertion time (5 bits)
        bit_field_t<21, 0x1f>::value<X>();
    static constexpr uint32_t CR1_RTOIE = 0x4000000;    // Receiver timeout interrupt enable
    static constexpr uint32_t CR1_EOBIE = 0x8000000;    // End of Block interrupt enable
    static constexpr uint32_t CR1_M1 = 0x10000000;      // Word length
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_ADD4 =                // Address of the USART node (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_ADD0 =                // Address of the USART node (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t CR2_RTOEN = 0x800000;     // Receiver timeout enable
    template<uint32_t X>
    static constexpr uint32_t CR2_ABRMOD =              // Auto baud rate mode (2 bits)
        bit_field_t<21, 0x3>::value<X>();
    static constexpr uint32_t CR2_ABREN = 0x100000;     // Auto baud rate enable
    static constexpr uint32_t CR2_MSBFIRST = 0x80000;   // Most significant bit first
    static constexpr uint32_t CR2_DATAINV = 0x40000;    // Binary data inversion
    static constexpr uint32_t CR2_TXINV = 0x20000;      // TX pin active level inversion
    static constexpr uint32_t CR2_RXINV = 0x10000;      // RX pin active level inversion
    static constexpr uint32_t CR2_SWAP = 0x8000;        // Swap TX/RX pins
    static constexpr uint32_t CR2_LINEN = 0x4000;       // LIN mode enable
    template<uint32_t X>
    static constexpr uint32_t CR2_STOP =                // STOP bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CR2_CLKEN = 0x800;        // Clock enable
    static constexpr uint32_t CR2_CPOL = 0x400;         // Clock polarity
    static constexpr uint32_t CR2_CPHA = 0x200;         // Clock phase
    static constexpr uint32_t CR2_LBCL = 0x100;         // Last bit clock pulse
    static constexpr uint32_t CR2_LBDIE = 0x40;         // LIN break detection interrupt enable
    static constexpr uint32_t CR2_LBDL = 0x20;          // LIN break detection length
    static constexpr uint32_t CR2_ADDM7 = 0x10;         // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CR3_WUFIE = 0x400000;     // Wakeup from Stop mode interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR3_WUS =                 // Wakeup from Stop mode interrupt flag selection (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR3_SCARCNT =             // Smartcard auto-retry count (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    static constexpr uint32_t CR3_DEP = 0x8000;         // Driver enable polarity selection
    static constexpr uint32_t CR3_DEM = 0x4000;         // Driver enable mode
    static constexpr uint32_t CR3_DDRE = 0x2000;        // DMA Disable on Reception Error
    static constexpr uint32_t CR3_OVRDIS = 0x1000;      // Overrun Disable
    static constexpr uint32_t CR3_ONEBIT = 0x800;       // One sample bit method enable
    static constexpr uint32_t CR3_CTSIE = 0x400;        // CTS interrupt enable
    static constexpr uint32_t CR3_CTSE = 0x200;         // CTS enable
    static constexpr uint32_t CR3_RTSE = 0x100;         // RTS enable
    static constexpr uint32_t CR3_DMAT = 0x80;          // DMA enable transmitter
    static constexpr uint32_t CR3_DMAR = 0x40;          // DMA enable receiver
    static constexpr uint32_t CR3_SCEN = 0x20;          // Smartcard mode enable
    static constexpr uint32_t CR3_NACK = 0x10;          // Smartcard NACK enable
    static constexpr uint32_t CR3_HDSEL = 0x8;          // Half-duplex selection
    static constexpr uint32_t CR3_IRLP = 0x4;           // IrDA low-power
    static constexpr uint32_t CR3_IREN = 0x2;           // IrDA mode enable
    static constexpr uint32_t CR3_EIE = 0x1;            // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Mantissa =        // mantissa of USARTDIV (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // fraction of USARTDIV (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BRR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t GTPR_GT =                  // Guard time value (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t GTPR_PSC =                 // Prescaler value (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RTOR_BLEN =                // Block Length (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RTOR_RTO =                 // Receiver timeout value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();
    static const uint32_t RTOR_RESET_VALUE = 0x0;

    static constexpr uint32_t RQR_TXFRQ = 0x10;         // Transmit data flush request
    static constexpr uint32_t RQR_RXFRQ = 0x8;          // Receive data flush request
    static constexpr uint32_t RQR_MMRQ = 0x4;           // Mute mode request
    static constexpr uint32_t RQR_SBKRQ = 0x2;          // Send break request
    static constexpr uint32_t RQR_ABRRQ = 0x1;          // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static constexpr uint32_t ISR_REACK = 0x400000;     // Receive enable acknowledge flag
    static constexpr uint32_t ISR_TEACK = 0x200000;     // Transmit enable acknowledge flag
    static constexpr uint32_t ISR_WUF = 0x100000;       // Wakeup from Stop mode flag
    static constexpr uint32_t ISR_RWU = 0x80000;        // Receiver wakeup from Mute mode
    static constexpr uint32_t ISR_SBKF = 0x40000;       // Send break flag
    static constexpr uint32_t ISR_CMF = 0x20000;        // character match flag
    static constexpr uint32_t ISR_BUSY = 0x10000;       // Busy flag
    static constexpr uint32_t ISR_ABRF = 0x8000;        // Auto baud rate flag
    static constexpr uint32_t ISR_ABRE = 0x4000;        // Auto baud rate error
    static constexpr uint32_t ISR_EOBF = 0x1000;        // End of block flag
    static constexpr uint32_t ISR_RTOF = 0x800;         // Receiver timeout
    static constexpr uint32_t ISR_CTS = 0x400;          // CTS flag
    static constexpr uint32_t ISR_CTSIF = 0x200;        // CTS interrupt flag
    static constexpr uint32_t ISR_LBDF = 0x100;         // LIN break detection flag
    static constexpr uint32_t ISR_TXE = 0x80;           // Transmit data register empty
    static constexpr uint32_t ISR_TC = 0x40;            // Transmission complete
    static constexpr uint32_t ISR_RXNE = 0x20;          // Read data register not empty
    static constexpr uint32_t ISR_IDLE = 0x10;          // Idle line detected
    static constexpr uint32_t ISR_ORE = 0x8;            // Overrun error
    static constexpr uint32_t ISR_NF = 0x4;             // Noise detected flag
    static constexpr uint32_t ISR_FE = 0x2;             // Framing error
    static constexpr uint32_t ISR_PE = 0x1;             // Parity error
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static constexpr uint32_t ICR_WUCF = 0x100000;      // Wakeup from Stop mode clear flag
    static constexpr uint32_t ICR_CMCF = 0x20000;       // Character match clear flag
    static constexpr uint32_t ICR_EOBCF = 0x1000;       // End of timeout clear flag
    static constexpr uint32_t ICR_RTOCF = 0x800;        // Receiver timeout clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
    static constexpr uint32_t ICR_LBDCF = 0x100;        // LIN break detection clear flag
    static constexpr uint32_t ICR_TCCF = 0x40;          // Transmission complete clear flag
    static constexpr uint32_t ICR_IDLECF = 0x10;        // Idle line detected clear flag
    static constexpr uint32_t ICR_ORECF = 0x8;          // Overrun error clear flag
    static constexpr uint32_t ICR_NCF = 0x4;            // Noise detected clear flag
    static constexpr uint32_t ICR_FECF = 0x2;           // Framing error clear flag
    static constexpr uint32_t ICR_PECF = 0x1;           // Parity error clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RDR_RDR =                 // Receive data value (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t RDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDR_TDR =                 // Transmit data value (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t TDR_RESET_VALUE = 0x0;

    static constexpr uint8_t USART2 = 28; // USART2 global interrupt
};

static usart2_t& USART2 = *reinterpret_cast<usart2_t*>(0x40004400);

#define HAVE_PERIPHERAL_USART2


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

    static constexpr uint32_t CR1_UE = 0x1;             // USART enable
    static constexpr uint32_t CR1_UESM = 0x2;           // USART enable in Stop mode
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // interrupt enable
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_WAKE = 0x800;         // Receiver wakeup method
    static constexpr uint32_t CR1_M = 0x1000;           // Word length
    static constexpr uint32_t CR1_MME = 0x2000;         // Mute mode enable
    static constexpr uint32_t CR1_CMIE = 0x4000;        // Character match interrupt enable
    static constexpr uint32_t CR1_OVER8 = 0x8000;       // Oversampling mode
    template<uint32_t X>
    static constexpr uint32_t CR1_DEDT =                // Driver Enable deassertion time (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR1_DEAT =                // Driver Enable assertion time (5 bits)
        bit_field_t<21, 0x1f>::value<X>();
    static constexpr uint32_t CR1_RTOIE = 0x4000000;    // Receiver timeout interrupt enable
    static constexpr uint32_t CR1_EOBIE = 0x8000000;    // End of Block interrupt enable
    static constexpr uint32_t CR1_M1 = 0x10000000;      // Word length
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_ADD4 =                // Address of the USART node (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_ADD0 =                // Address of the USART node (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t CR2_RTOEN = 0x800000;     // Receiver timeout enable
    template<uint32_t X>
    static constexpr uint32_t CR2_ABRMOD =              // Auto baud rate mode (2 bits)
        bit_field_t<21, 0x3>::value<X>();
    static constexpr uint32_t CR2_ABREN = 0x100000;     // Auto baud rate enable
    static constexpr uint32_t CR2_MSBFIRST = 0x80000;   // Most significant bit first
    static constexpr uint32_t CR2_DATAINV = 0x40000;    // Binary data inversion
    static constexpr uint32_t CR2_TXINV = 0x20000;      // TX pin active level inversion
    static constexpr uint32_t CR2_RXINV = 0x10000;      // RX pin active level inversion
    static constexpr uint32_t CR2_SWAP = 0x8000;        // Swap TX/RX pins
    static constexpr uint32_t CR2_LINEN = 0x4000;       // LIN mode enable
    template<uint32_t X>
    static constexpr uint32_t CR2_STOP =                // STOP bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CR2_CLKEN = 0x800;        // Clock enable
    static constexpr uint32_t CR2_CPOL = 0x400;         // Clock polarity
    static constexpr uint32_t CR2_CPHA = 0x200;         // Clock phase
    static constexpr uint32_t CR2_LBCL = 0x100;         // Last bit clock pulse
    static constexpr uint32_t CR2_LBDIE = 0x40;         // LIN break detection interrupt enable
    static constexpr uint32_t CR2_LBDL = 0x20;          // LIN break detection length
    static constexpr uint32_t CR2_ADDM7 = 0x10;         // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CR3_WUFIE = 0x400000;     // Wakeup from Stop mode interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR3_WUS =                 // Wakeup from Stop mode interrupt flag selection (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR3_SCARCNT =             // Smartcard auto-retry count (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    static constexpr uint32_t CR3_DEP = 0x8000;         // Driver enable polarity selection
    static constexpr uint32_t CR3_DEM = 0x4000;         // Driver enable mode
    static constexpr uint32_t CR3_DDRE = 0x2000;        // DMA Disable on Reception Error
    static constexpr uint32_t CR3_OVRDIS = 0x1000;      // Overrun Disable
    static constexpr uint32_t CR3_ONEBIT = 0x800;       // One sample bit method enable
    static constexpr uint32_t CR3_CTSIE = 0x400;        // CTS interrupt enable
    static constexpr uint32_t CR3_CTSE = 0x200;         // CTS enable
    static constexpr uint32_t CR3_RTSE = 0x100;         // RTS enable
    static constexpr uint32_t CR3_DMAT = 0x80;          // DMA enable transmitter
    static constexpr uint32_t CR3_DMAR = 0x40;          // DMA enable receiver
    static constexpr uint32_t CR3_SCEN = 0x20;          // Smartcard mode enable
    static constexpr uint32_t CR3_NACK = 0x10;          // Smartcard NACK enable
    static constexpr uint32_t CR3_HDSEL = 0x8;          // Half-duplex selection
    static constexpr uint32_t CR3_IRLP = 0x4;           // IrDA low-power
    static constexpr uint32_t CR3_IREN = 0x2;           // IrDA mode enable
    static constexpr uint32_t CR3_EIE = 0x1;            // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Mantissa =        // mantissa of USARTDIV (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // fraction of USARTDIV (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BRR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t GTPR_GT =                  // Guard time value (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t GTPR_PSC =                 // Prescaler value (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RTOR_BLEN =                // Block Length (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RTOR_RTO =                 // Receiver timeout value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();
    static const uint32_t RTOR_RESET_VALUE = 0x0;

    static constexpr uint32_t RQR_TXFRQ = 0x10;         // Transmit data flush request
    static constexpr uint32_t RQR_RXFRQ = 0x8;          // Receive data flush request
    static constexpr uint32_t RQR_MMRQ = 0x4;           // Mute mode request
    static constexpr uint32_t RQR_SBKRQ = 0x2;          // Send break request
    static constexpr uint32_t RQR_ABRRQ = 0x1;          // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static constexpr uint32_t ISR_REACK = 0x400000;     // Receive enable acknowledge flag
    static constexpr uint32_t ISR_TEACK = 0x200000;     // Transmit enable acknowledge flag
    static constexpr uint32_t ISR_WUF = 0x100000;       // Wakeup from Stop mode flag
    static constexpr uint32_t ISR_RWU = 0x80000;        // Receiver wakeup from Mute mode
    static constexpr uint32_t ISR_SBKF = 0x40000;       // Send break flag
    static constexpr uint32_t ISR_CMF = 0x20000;        // character match flag
    static constexpr uint32_t ISR_BUSY = 0x10000;       // Busy flag
    static constexpr uint32_t ISR_ABRF = 0x8000;        // Auto baud rate flag
    static constexpr uint32_t ISR_ABRE = 0x4000;        // Auto baud rate error
    static constexpr uint32_t ISR_EOBF = 0x1000;        // End of block flag
    static constexpr uint32_t ISR_RTOF = 0x800;         // Receiver timeout
    static constexpr uint32_t ISR_CTS = 0x400;          // CTS flag
    static constexpr uint32_t ISR_CTSIF = 0x200;        // CTS interrupt flag
    static constexpr uint32_t ISR_LBDF = 0x100;         // LIN break detection flag
    static constexpr uint32_t ISR_TXE = 0x80;           // Transmit data register empty
    static constexpr uint32_t ISR_TC = 0x40;            // Transmission complete
    static constexpr uint32_t ISR_RXNE = 0x20;          // Read data register not empty
    static constexpr uint32_t ISR_IDLE = 0x10;          // Idle line detected
    static constexpr uint32_t ISR_ORE = 0x8;            // Overrun error
    static constexpr uint32_t ISR_NF = 0x4;             // Noise detected flag
    static constexpr uint32_t ISR_FE = 0x2;             // Framing error
    static constexpr uint32_t ISR_PE = 0x1;             // Parity error
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static constexpr uint32_t ICR_WUCF = 0x100000;      // Wakeup from Stop mode clear flag
    static constexpr uint32_t ICR_CMCF = 0x20000;       // Character match clear flag
    static constexpr uint32_t ICR_EOBCF = 0x1000;       // End of timeout clear flag
    static constexpr uint32_t ICR_RTOCF = 0x800;        // Receiver timeout clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
    static constexpr uint32_t ICR_LBDCF = 0x100;        // LIN break detection clear flag
    static constexpr uint32_t ICR_TCCF = 0x40;          // Transmission complete clear flag
    static constexpr uint32_t ICR_IDLECF = 0x10;        // Idle line detected clear flag
    static constexpr uint32_t ICR_ORECF = 0x8;          // Overrun error clear flag
    static constexpr uint32_t ICR_NCF = 0x4;            // Noise detected clear flag
    static constexpr uint32_t ICR_FECF = 0x2;           // Framing error clear flag
    static constexpr uint32_t ICR_PECF = 0x1;           // Parity error clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RDR_RDR =                 // Receive data value (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t RDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDR_TDR =                 // Transmit data value (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t TDR_RESET_VALUE = 0x0;

    static constexpr uint8_t USART3_4 = 29; // USART3 and USART4 global interrupt
};

static usart3_t& USART3 = *reinterpret_cast<usart3_t*>(0x40004800);

#define HAVE_PERIPHERAL_USART3


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

    static constexpr uint32_t CR1_UE = 0x1;             // USART enable
    static constexpr uint32_t CR1_UESM = 0x2;           // USART enable in Stop mode
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // interrupt enable
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_WAKE = 0x800;         // Receiver wakeup method
    static constexpr uint32_t CR1_M = 0x1000;           // Word length
    static constexpr uint32_t CR1_MME = 0x2000;         // Mute mode enable
    static constexpr uint32_t CR1_CMIE = 0x4000;        // Character match interrupt enable
    static constexpr uint32_t CR1_OVER8 = 0x8000;       // Oversampling mode
    template<uint32_t X>
    static constexpr uint32_t CR1_DEDT =                // Driver Enable deassertion time (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR1_DEAT =                // Driver Enable assertion time (5 bits)
        bit_field_t<21, 0x1f>::value<X>();
    static constexpr uint32_t CR1_RTOIE = 0x4000000;    // Receiver timeout interrupt enable
    static constexpr uint32_t CR1_EOBIE = 0x8000000;    // End of Block interrupt enable
    static constexpr uint32_t CR1_M1 = 0x10000000;      // Word length
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_ADD4 =                // Address of the USART node (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_ADD0 =                // Address of the USART node (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t CR2_RTOEN = 0x800000;     // Receiver timeout enable
    template<uint32_t X>
    static constexpr uint32_t CR2_ABRMOD =              // Auto baud rate mode (2 bits)
        bit_field_t<21, 0x3>::value<X>();
    static constexpr uint32_t CR2_ABREN = 0x100000;     // Auto baud rate enable
    static constexpr uint32_t CR2_MSBFIRST = 0x80000;   // Most significant bit first
    static constexpr uint32_t CR2_DATAINV = 0x40000;    // Binary data inversion
    static constexpr uint32_t CR2_TXINV = 0x20000;      // TX pin active level inversion
    static constexpr uint32_t CR2_RXINV = 0x10000;      // RX pin active level inversion
    static constexpr uint32_t CR2_SWAP = 0x8000;        // Swap TX/RX pins
    static constexpr uint32_t CR2_LINEN = 0x4000;       // LIN mode enable
    template<uint32_t X>
    static constexpr uint32_t CR2_STOP =                // STOP bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CR2_CLKEN = 0x800;        // Clock enable
    static constexpr uint32_t CR2_CPOL = 0x400;         // Clock polarity
    static constexpr uint32_t CR2_CPHA = 0x200;         // Clock phase
    static constexpr uint32_t CR2_LBCL = 0x100;         // Last bit clock pulse
    static constexpr uint32_t CR2_LBDIE = 0x40;         // LIN break detection interrupt enable
    static constexpr uint32_t CR2_LBDL = 0x20;          // LIN break detection length
    static constexpr uint32_t CR2_ADDM7 = 0x10;         // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CR3_WUFIE = 0x400000;     // Wakeup from Stop mode interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR3_WUS =                 // Wakeup from Stop mode interrupt flag selection (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR3_SCARCNT =             // Smartcard auto-retry count (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    static constexpr uint32_t CR3_DEP = 0x8000;         // Driver enable polarity selection
    static constexpr uint32_t CR3_DEM = 0x4000;         // Driver enable mode
    static constexpr uint32_t CR3_DDRE = 0x2000;        // DMA Disable on Reception Error
    static constexpr uint32_t CR3_OVRDIS = 0x1000;      // Overrun Disable
    static constexpr uint32_t CR3_ONEBIT = 0x800;       // One sample bit method enable
    static constexpr uint32_t CR3_CTSIE = 0x400;        // CTS interrupt enable
    static constexpr uint32_t CR3_CTSE = 0x200;         // CTS enable
    static constexpr uint32_t CR3_RTSE = 0x100;         // RTS enable
    static constexpr uint32_t CR3_DMAT = 0x80;          // DMA enable transmitter
    static constexpr uint32_t CR3_DMAR = 0x40;          // DMA enable receiver
    static constexpr uint32_t CR3_SCEN = 0x20;          // Smartcard mode enable
    static constexpr uint32_t CR3_NACK = 0x10;          // Smartcard NACK enable
    static constexpr uint32_t CR3_HDSEL = 0x8;          // Half-duplex selection
    static constexpr uint32_t CR3_IRLP = 0x4;           // IrDA low-power
    static constexpr uint32_t CR3_IREN = 0x2;           // IrDA mode enable
    static constexpr uint32_t CR3_EIE = 0x1;            // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Mantissa =        // mantissa of USARTDIV (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // fraction of USARTDIV (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BRR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t GTPR_GT =                  // Guard time value (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t GTPR_PSC =                 // Prescaler value (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RTOR_BLEN =                // Block Length (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RTOR_RTO =                 // Receiver timeout value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();
    static const uint32_t RTOR_RESET_VALUE = 0x0;

    static constexpr uint32_t RQR_TXFRQ = 0x10;         // Transmit data flush request
    static constexpr uint32_t RQR_RXFRQ = 0x8;          // Receive data flush request
    static constexpr uint32_t RQR_MMRQ = 0x4;           // Mute mode request
    static constexpr uint32_t RQR_SBKRQ = 0x2;          // Send break request
    static constexpr uint32_t RQR_ABRRQ = 0x1;          // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static constexpr uint32_t ISR_REACK = 0x400000;     // Receive enable acknowledge flag
    static constexpr uint32_t ISR_TEACK = 0x200000;     // Transmit enable acknowledge flag
    static constexpr uint32_t ISR_WUF = 0x100000;       // Wakeup from Stop mode flag
    static constexpr uint32_t ISR_RWU = 0x80000;        // Receiver wakeup from Mute mode
    static constexpr uint32_t ISR_SBKF = 0x40000;       // Send break flag
    static constexpr uint32_t ISR_CMF = 0x20000;        // character match flag
    static constexpr uint32_t ISR_BUSY = 0x10000;       // Busy flag
    static constexpr uint32_t ISR_ABRF = 0x8000;        // Auto baud rate flag
    static constexpr uint32_t ISR_ABRE = 0x4000;        // Auto baud rate error
    static constexpr uint32_t ISR_EOBF = 0x1000;        // End of block flag
    static constexpr uint32_t ISR_RTOF = 0x800;         // Receiver timeout
    static constexpr uint32_t ISR_CTS = 0x400;          // CTS flag
    static constexpr uint32_t ISR_CTSIF = 0x200;        // CTS interrupt flag
    static constexpr uint32_t ISR_LBDF = 0x100;         // LIN break detection flag
    static constexpr uint32_t ISR_TXE = 0x80;           // Transmit data register empty
    static constexpr uint32_t ISR_TC = 0x40;            // Transmission complete
    static constexpr uint32_t ISR_RXNE = 0x20;          // Read data register not empty
    static constexpr uint32_t ISR_IDLE = 0x10;          // Idle line detected
    static constexpr uint32_t ISR_ORE = 0x8;            // Overrun error
    static constexpr uint32_t ISR_NF = 0x4;             // Noise detected flag
    static constexpr uint32_t ISR_FE = 0x2;             // Framing error
    static constexpr uint32_t ISR_PE = 0x1;             // Parity error
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static constexpr uint32_t ICR_WUCF = 0x100000;      // Wakeup from Stop mode clear flag
    static constexpr uint32_t ICR_CMCF = 0x20000;       // Character match clear flag
    static constexpr uint32_t ICR_EOBCF = 0x1000;       // End of timeout clear flag
    static constexpr uint32_t ICR_RTOCF = 0x800;        // Receiver timeout clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
    static constexpr uint32_t ICR_LBDCF = 0x100;        // LIN break detection clear flag
    static constexpr uint32_t ICR_TCCF = 0x40;          // Transmission complete clear flag
    static constexpr uint32_t ICR_IDLECF = 0x10;        // Idle line detected clear flag
    static constexpr uint32_t ICR_ORECF = 0x8;          // Overrun error clear flag
    static constexpr uint32_t ICR_NCF = 0x4;            // Noise detected clear flag
    static constexpr uint32_t ICR_FECF = 0x2;           // Framing error clear flag
    static constexpr uint32_t ICR_PECF = 0x1;           // Parity error clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RDR_RDR =                 // Receive data value (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t RDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDR_TDR =                 // Transmit data value (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t TDR_RESET_VALUE = 0x0;
};

static usart4_t& USART4 = *reinterpret_cast<usart4_t*>(0x40004c00);

#define HAVE_PERIPHERAL_USART4


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

    static constexpr uint32_t TR_PM = 0x400000;        // AM/PM notation
    template<uint32_t X>
    static constexpr uint32_t TR_HT =                  // Hour tens in BCD format (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR_HU =                  // Hour units in BCD format (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR_MNT =                 // Minute tens in BCD format (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR_MNU =                 // Minute units in BCD format (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR_ST =                  // Second tens in BCD format (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR_SU =                  // Second units in BCD format (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t TR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR_YT =                  // Year tens in BCD format (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DR_YU =                  // Year units in BCD format (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DR_WDU =                 // Week day units (3 bits)
        bit_field_t<13, 0x7>::value<X>();
    static constexpr uint32_t DR_MT = 0x1000;          // Month tens in BCD format
    template<uint32_t X>
    static constexpr uint32_t DR_MU =                  // Month units in BCD format (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DR_DT =                  // Date tens in BCD format (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DR_DU =                  // Date units in BCD format (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x2101;

    static constexpr uint32_t CR_TSEDGE = 0x8;         // Time-stamp event active edge, Read-write
    static constexpr uint32_t CR_REFCKON = 0x10;       // RTC_REFIN reference clock detection enable (50 or 60 Hz), Read-write
    static constexpr uint32_t CR_BYPSHAD = 0x20;       // Bypass the shadow registers, Read-write
    static constexpr uint32_t CR_FMT = 0x40;           // Hour format, Read-write
    static constexpr uint32_t CR_ALRAE = 0x100;        // Alarm A enable, Read-write
    static constexpr uint32_t CR_TSE = 0x800;          // timestamp enable, Read-write
    static constexpr uint32_t CR_ALRAIE = 0x1000;      // Alarm A interrupt enable, Read-write
    static constexpr uint32_t CR_TSIE = 0x8000;        // Time-stamp interrupt enable, Read-write
    static constexpr uint32_t CR_ADD1H = 0x10000;      // Add 1 hour (summer time change), Write-only
    static constexpr uint32_t CR_SUB1H = 0x20000;      // Subtract 1 hour (winter time change), Write-only
    static constexpr uint32_t CR_BKP = 0x40000;        // Backup, Read-write
    static constexpr uint32_t CR_COSEL = 0x80000;      // Calibration output selection, Read-write
    static constexpr uint32_t CR_POL = 0x100000;       // Output polarity, Read-write
    template<uint32_t X>
    static constexpr uint32_t CR_OSEL =                // Output selection (2 bits), Read-write
        bit_field_t<21, 0x3>::value<X>();
    static constexpr uint32_t CR_COE = 0x800000;       // Calibration output enable, Read-write
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t ISR_ALRAWF = 0x1;         // Alarm A write flag, Read-only
    static constexpr uint32_t ISR_SHPF = 0x8;           // Shift operation pending, Read-write
    static constexpr uint32_t ISR_INITS = 0x10;         // Initialization status flag, Read-only
    static constexpr uint32_t ISR_RSF = 0x20;           // Registers synchronization flag, Read-write
    static constexpr uint32_t ISR_INITF = 0x40;         // Initialization flag, Read-only
    static constexpr uint32_t ISR_INIT = 0x80;          // Initialization mode, Read-write
    static constexpr uint32_t ISR_ALRAF = 0x100;        // Alarm A flag, Read-write
    static constexpr uint32_t ISR_TSF = 0x800;          // Time-stamp flag, Read-write
    static constexpr uint32_t ISR_TSOVF = 0x1000;       // Time-stamp overflow flag, Read-write
    static constexpr uint32_t ISR_TAMP1F = 0x2000;      // RTC_TAMP1 detection flag, Read-write
    static constexpr uint32_t ISR_TAMP2F = 0x4000;      // RTC_TAMP2 detection flag, Read-write
    static constexpr uint32_t ISR_RECALPF = 0x10000;    // Recalibration pending Flag, Read-only
    static const uint32_t ISR_RESET_VALUE = 0x7;

    template<uint32_t X>
    static constexpr uint32_t PRER_PREDIV_A =            // Asynchronous prescaler factor (7 bits)
        bit_field_t<16, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PRER_PREDIV_S =            // Synchronous prescaler factor (15 bits)
        bit_field_t<0, 0x7fff>::value<X>();
    static const uint32_t PRER_RESET_VALUE = 0x7f00ff;

    static constexpr uint32_t ALRMAR_MSK4 = 0x80000000;    // Alarm A date mask
    static constexpr uint32_t ALRMAR_WDSEL = 0x40000000;   // Week day selection
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_DT =                  // Date tens in BCD format. (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_DU =                  // Date units or day in BCD format. (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t ALRMAR_MSK3 = 0x800000;      // Alarm A hours mask
    static constexpr uint32_t ALRMAR_PM = 0x400000;        // AM/PM notation
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_HT =                  // Hour tens in BCD format. (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_HU =                  // Hour units in BCD format. (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    static constexpr uint32_t ALRMAR_MSK2 = 0x8000;        // Alarm A minutes mask
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_MNT =                 // Minute tens in BCD format. (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_MNU =                 // Minute units in BCD format. (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t ALRMAR_MSK1 = 0x80;          // Alarm A seconds mask
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_ST =                  // Second tens in BCD format. (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_SU =                  // Second units in BCD format. (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t ALRMAR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t WPR_KEY =                 // Write protection key (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t WPR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SSR_SS =                  // Sub second value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t SSR_RESET_VALUE = 0x0;

    static constexpr uint32_t SHIFTR_ADD1S = 0x80000000;   // Add one second
    template<uint32_t X>
    static constexpr uint32_t SHIFTR_SUBFS =               // Subtract a fraction of a second (15 bits)
        bit_field_t<0, 0x7fff>::value<X>();
    static const uint32_t SHIFTR_RESET_VALUE = 0x0;

    static constexpr uint32_t TSTR_PM = 0x400000;        // AM/PM notation
    template<uint32_t X>
    static constexpr uint32_t TSTR_HT =                  // Hour tens in BCD format. (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSTR_HU =                  // Hour units in BCD format. (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSTR_MNT =                 // Minute tens in BCD format. (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSTR_MNU =                 // Minute units in BCD format. (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSTR_ST =                  // Second tens in BCD format. (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSTR_SU =                  // Second units in BCD format. (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t TSTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TSDR_WDU =                 // Week day units (3 bits)
        bit_field_t<13, 0x7>::value<X>();
    static constexpr uint32_t TSDR_MT = 0x1000;          // Month tens in BCD format
    template<uint32_t X>
    static constexpr uint32_t TSDR_MU =                  // Month units in BCD format (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSDR_DT =                  // Date tens in BCD format (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSDR_DU =                  // Date units in BCD format (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t TSDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TSSSR_SS =                  // Sub second value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t TSSSR_RESET_VALUE = 0x0;

    static constexpr uint32_t CALR_CALP = 0x8000;        // Increase frequency of RTC by 488.5 ppm
    static constexpr uint32_t CALR_CALW8 = 0x4000;       // Use an 8-second calibration cycle period
    static constexpr uint32_t CALR_CALW16 = 0x2000;      // Use a 16-second calibration cycle period
    template<uint32_t X>
    static constexpr uint32_t CALR_CALM =                // Calibration minus (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t CALR_RESET_VALUE = 0x0;

    static constexpr uint32_t TAFCR_PC15MODE = 0x800000;  // PC15 mode
    static constexpr uint32_t TAFCR_PC15VALUE = 0x400000; // PC15 value
    static constexpr uint32_t TAFCR_PC14MODE = 0x200000;  // PC14 mode
    static constexpr uint32_t TAFCR_PC14VALUE = 0x100000; // PC14 value
    static constexpr uint32_t TAFCR_PC13MODE = 0x80000;   // PC13 mode
    static constexpr uint32_t TAFCR_PC13VALUE = 0x40000;  // RTC_ALARM output type/PC13 value
    static constexpr uint32_t TAFCR_TAMP_PUDIS = 0x8000;  // RTC_TAMPx pull-up disable
    template<uint32_t X>
    static constexpr uint32_t TAFCR_TAMP_PRCH =           // RTC_TAMPx precharge duration (2 bits)
        bit_field_t<13, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TAFCR_TAMPFLT =             // RTC_TAMPx filter count (2 bits)
        bit_field_t<11, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TAFCR_TAMPFREQ =            // Tamper sampling frequency (3 bits)
        bit_field_t<8, 0x7>::value<X>();
    static constexpr uint32_t TAFCR_TAMPTS = 0x80;        // Activate timestamp on tamper detection event
    static constexpr uint32_t TAFCR_TAMP2_TRG = 0x10;     // Active level for RTC_TAMP2 input
    static constexpr uint32_t TAFCR_TAMP2E = 0x8;         // RTC_TAMP2 input detection enable
    static constexpr uint32_t TAFCR_TAMPIE = 0x4;         // Tamper interrupt enable
    static constexpr uint32_t TAFCR_TAMP1TRG = 0x2;       // Active level for RTC_TAMP1 input
    static constexpr uint32_t TAFCR_TAMP1E = 0x1;         // RTC_TAMP1 input detection enable
    static const uint32_t TAFCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ALRMASSR_MASKSS =              // Mask the most-significant bits starting at this bit (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMASSR_SS =                  // Sub seconds value (15 bits)
        bit_field_t<0, 0x7fff>::value<X>();
    static const uint32_t ALRMASSR_RESET_VALUE = 0x0;


    static const uint32_t BKP0R_RESET_VALUE = 0x0;


    static const uint32_t BKP1R_RESET_VALUE = 0x0;


    static const uint32_t BKP2R_RESET_VALUE = 0x0;


    static const uint32_t BKP3R_RESET_VALUE = 0x0;


    static const uint32_t BKP4R_RESET_VALUE = 0x0;

    static constexpr uint8_t RTC = 2; // RTC interrupts
};

static rtc_t& RTC = *reinterpret_cast<rtc_t*>(0x40002800);

#define HAVE_PERIPHERAL_RTC


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

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_OIS2 = 0x400;         // Output Idle state 2
    static constexpr uint32_t CR2_OIS1N = 0x200;        // Output Idle state 1
    static constexpr uint32_t CR2_OIS1 = 0x100;         // Output Idle state 1
    template<uint32_t X>
    static constexpr uint32_t CR2_MMS =                 // Master mode selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CR2_CCDS = 0x8;           // Capture/compare DMA selection
    static constexpr uint32_t CR2_CCUS = 0x4;           // Capture/compare control update selection
    static constexpr uint32_t CR2_CCPC = 0x1;           // Capture/compare preloaded control
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SMCR_MSM = 0x80;           // Master/Slave mode
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS =                  // Trigger selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMCR_SMS =                 // Slave mode selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t SMCR_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_TDE = 0x4000;         // Trigger DMA request enable
    static constexpr uint32_t DIER_CC2DE = 0x400;        // Capture/Compare 2 DMA request enable
    static constexpr uint32_t DIER_CC1DE = 0x200;        // Capture/Compare 1 DMA request enable
    static constexpr uint32_t DIER_UDE = 0x100;          // Update DMA request enable
    static constexpr uint32_t DIER_BIE = 0x80;           // Break interrupt enable
    static constexpr uint32_t DIER_TIE = 0x40;           // Trigger interrupt enable
    static constexpr uint32_t DIER_COMIE = 0x20;         // COM interrupt enable
    static constexpr uint32_t DIER_CC2IE = 0x4;          // Capture/Compare 2 interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/compare 2 overcapture flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/Compare 2 interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_BG = 0x80;            // Break generation
    static constexpr uint32_t EGR_TG = 0x40;            // Trigger generation
    static constexpr uint32_t EGR_COMG = 0x20;          // Capture/Compare control update generation
    static constexpr uint32_t EGR_CC2G = 0x4;           // Capture/compare 2 generation
    static constexpr uint32_t EGR_CC1G = 0x2;           // Capture/compare 1 generation
    static constexpr uint32_t EGR_UG = 0x1;             // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC1S =                // Capture/Compare 1 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC2S =                // Capture/Compare 2 selection (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1F =                // Input capture 1 filter (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1PSC =              // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC2F =                // Input capture 2 filter (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC2PSC =              // Input capture 2 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t CCMR1_OC1FE = 0x4;          // Output Compare 1 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC1M =                // Output Compare 1 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output Compare 1 preload enable
    static constexpr uint32_t CCMR1_OC2FE = 0x400;        // Output Compare 2 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC2M =                // Output Compare 2 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC2PE = 0x800;        // Output Compare 2 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC2NP = 0x80;         // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2P = 0x20;          // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2E = 0x10;          // Capture/Compare 2 output enable
    static constexpr uint32_t CCER_CC1NP = 0x8;          // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1NE = 0x4;          // Capture/Compare 1 complementary output enable
    static constexpr uint32_t CCER_CC1P = 0x2;           // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1E = 0x1;           // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RCR_REP =                 // Repetition counter value (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR1_CCR1 =                // Capture/Compare 1 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR2_CCR2 =                // Capture/Compare 2 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    static constexpr uint32_t BDTR_MOE = 0x8000;         // Main output enable
    static constexpr uint32_t BDTR_AOE = 0x4000;         // Automatic output enable
    static constexpr uint32_t BDTR_BKP = 0x2000;         // Break polarity
    static constexpr uint32_t BDTR_BKE = 0x1000;         // Break enable
    static constexpr uint32_t BDTR_OSSR = 0x800;         // Off-state selection for Run mode
    static constexpr uint32_t BDTR_OSSI = 0x400;         // Off-state selection for Idle mode
    template<uint32_t X>
    static constexpr uint32_t BDTR_LOCK =                // Lock configuration (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_DTG =                 // Dead-time generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t BDTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DMAR_DMAB =                // DMA register for burst accesses (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM15 = 20; // TIM15 global interrupt
};

static tim15_t& TIM15 = *reinterpret_cast<tim15_t*>(0x40014000);

#define HAVE_PERIPHERAL_TIM15


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

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_OIS1N = 0x200;        // Output Idle state 1
    static constexpr uint32_t CR2_OIS1 = 0x100;         // Output Idle state 1
    static constexpr uint32_t CR2_CCDS = 0x8;           // Capture/compare DMA selection
    static constexpr uint32_t CR2_CCUS = 0x4;           // Capture/compare control update selection
    static constexpr uint32_t CR2_CCPC = 0x1;           // Capture/compare preloaded control
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_TDE = 0x4000;         // Trigger DMA request enable
    static constexpr uint32_t DIER_CC1DE = 0x200;        // Capture/Compare 1 DMA request enable
    static constexpr uint32_t DIER_UDE = 0x100;          // Update DMA request enable
    static constexpr uint32_t DIER_BIE = 0x80;           // Break interrupt enable
    static constexpr uint32_t DIER_TIE = 0x40;           // Trigger interrupt enable
    static constexpr uint32_t DIER_COMIE = 0x20;         // COM interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_BG = 0x80;            // Break generation
    static constexpr uint32_t EGR_TG = 0x40;            // Trigger generation
    static constexpr uint32_t EGR_COMG = 0x20;          // Capture/Compare control update generation
    static constexpr uint32_t EGR_CC1G = 0x2;           // Capture/compare 1 generation
    static constexpr uint32_t EGR_UG = 0x1;             // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC1S =                // Capture/Compare 1 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1F =                // Input capture 1 filter (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1PSC =              // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    static constexpr uint32_t CCMR1_OC1FE = 0x4;          // Output Compare 1 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC1M =                // Output Compare 1 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output Compare 1 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC1NP = 0x8;          // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1NE = 0x4;          // Capture/Compare 1 complementary output enable
    static constexpr uint32_t CCER_CC1P = 0x2;           // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1E = 0x1;           // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RCR_REP =                 // Repetition counter value (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR1_CCR1 =                // Capture/Compare 1 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    static constexpr uint32_t BDTR_MOE = 0x8000;         // Main output enable
    static constexpr uint32_t BDTR_AOE = 0x4000;         // Automatic output enable
    static constexpr uint32_t BDTR_BKP = 0x2000;         // Break polarity
    static constexpr uint32_t BDTR_BKE = 0x1000;         // Break enable
    static constexpr uint32_t BDTR_OSSR = 0x800;         // Off-state selection for Run mode
    static constexpr uint32_t BDTR_OSSI = 0x400;         // Off-state selection for Idle mode
    template<uint32_t X>
    static constexpr uint32_t BDTR_LOCK =                // Lock configuration (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_DTG =                 // Dead-time generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t BDTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DMAR_DMAB =                // DMA register for burst accesses (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM16 = 21; // TIM16 global interrupt
};

static tim16_t& TIM16 = *reinterpret_cast<tim16_t*>(0x40014400);

#define HAVE_PERIPHERAL_TIM16


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

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_OIS1N = 0x200;        // Output Idle state 1
    static constexpr uint32_t CR2_OIS1 = 0x100;         // Output Idle state 1
    static constexpr uint32_t CR2_CCDS = 0x8;           // Capture/compare DMA selection
    static constexpr uint32_t CR2_CCUS = 0x4;           // Capture/compare control update selection
    static constexpr uint32_t CR2_CCPC = 0x1;           // Capture/compare preloaded control
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_TDE = 0x4000;         // Trigger DMA request enable
    static constexpr uint32_t DIER_CC1DE = 0x200;        // Capture/Compare 1 DMA request enable
    static constexpr uint32_t DIER_UDE = 0x100;          // Update DMA request enable
    static constexpr uint32_t DIER_BIE = 0x80;           // Break interrupt enable
    static constexpr uint32_t DIER_TIE = 0x40;           // Trigger interrupt enable
    static constexpr uint32_t DIER_COMIE = 0x20;         // COM interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_BG = 0x80;            // Break generation
    static constexpr uint32_t EGR_TG = 0x40;            // Trigger generation
    static constexpr uint32_t EGR_COMG = 0x20;          // Capture/Compare control update generation
    static constexpr uint32_t EGR_CC1G = 0x2;           // Capture/compare 1 generation
    static constexpr uint32_t EGR_UG = 0x1;             // Update generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC1S =                // Capture/Compare 1 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1F =                // Input capture 1 filter (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1PSC =              // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    static constexpr uint32_t CCMR1_OC1FE = 0x4;          // Output Compare 1 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC1M =                // Output Compare 1 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output Compare 1 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC1NP = 0x8;          // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1NE = 0x4;          // Capture/Compare 1 complementary output enable
    static constexpr uint32_t CCER_CC1P = 0x2;           // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1E = 0x1;           // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RCR_REP =                 // Repetition counter value (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR1_CCR1 =                // Capture/Compare 1 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    static constexpr uint32_t BDTR_MOE = 0x8000;         // Main output enable
    static constexpr uint32_t BDTR_AOE = 0x4000;         // Automatic output enable
    static constexpr uint32_t BDTR_BKP = 0x2000;         // Break polarity
    static constexpr uint32_t BDTR_BKE = 0x1000;         // Break enable
    static constexpr uint32_t BDTR_OSSR = 0x800;         // Off-state selection for Run mode
    static constexpr uint32_t BDTR_OSSI = 0x400;         // Off-state selection for Idle mode
    template<uint32_t X>
    static constexpr uint32_t BDTR_LOCK =                // Lock configuration (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_DTG =                 // Dead-time generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t BDTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DMAR_DMAB =                // DMA register for burst accesses (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM17 = 22; // TIM17 global interrupt
};

static tim17_t& TIM17 = *reinterpret_cast<tim17_t*>(0x40014800);

#define HAVE_PERIPHERAL_TIM17


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

    template<uint32_t X>
    static constexpr uint32_t CR_CTPH =                // Charge transfer pulse high (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_CTPL =                // Charge transfer pulse low (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_SSD =                 // Spread spectrum deviation (7 bits)
        bit_field_t<17, 0x7f>::value<X>();
    static constexpr uint32_t CR_SSE = 0x10000;        // Spread spectrum enable
    static constexpr uint32_t CR_SSPSC = 0x8000;       // Spread spectrum prescaler
    template<uint32_t X>
    static constexpr uint32_t CR_PGPSC =               // pulse generator prescaler (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_MCV =                 // Max count value (3 bits)
        bit_field_t<5, 0x7>::value<X>();
    static constexpr uint32_t CR_IODEF = 0x10;         // I/O Default mode
    static constexpr uint32_t CR_SYNCPOL = 0x8;        // Synchronization pin polarity
    static constexpr uint32_t CR_AM = 0x4;             // Acquisition mode
    static constexpr uint32_t CR_START = 0x2;          // Start a new acquisition
    static constexpr uint32_t CR_TSCE = 0x1;           // Touch sensing controller enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t IER_MCEIE = 0x2;          // Max count error interrupt enable
    static constexpr uint32_t IER_EOAIE = 0x1;          // End of acquisition interrupt enable
    static const uint32_t IER_RESET_VALUE = 0x0;

    static constexpr uint32_t ICR_MCEIC = 0x2;          // Max count error interrupt clear
    static constexpr uint32_t ICR_EOAIC = 0x1;          // End of acquisition interrupt clear
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static constexpr uint32_t ISR_MCEF = 0x2;           // Max count error flag
    static constexpr uint32_t ISR_EOAF = 0x1;           // End of acquisition flag
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t IOHCR_G6_IO4 = 0x800000;    // G6_IO4 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G6_IO3 = 0x400000;    // G6_IO3 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G6_IO2 = 0x200000;    // G6_IO2 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G6_IO1 = 0x100000;    // G6_IO1 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G5_IO4 = 0x80000;     // G5_IO4 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G5_IO3 = 0x40000;     // G5_IO3 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G5_IO2 = 0x20000;     // G5_IO2 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G5_IO1 = 0x10000;     // G5_IO1 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G4_IO4 = 0x8000;      // G4_IO4 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G4_IO3 = 0x4000;      // G4_IO3 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G4_IO2 = 0x2000;      // G4_IO2 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G4_IO1 = 0x1000;      // G4_IO1 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G3_IO4 = 0x800;       // G3_IO4 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G3_IO3 = 0x400;       // G3_IO3 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G3_IO2 = 0x200;       // G3_IO2 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G3_IO1 = 0x100;       // G3_IO1 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G2_IO4 = 0x80;        // G2_IO4 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G2_IO3 = 0x40;        // G2_IO3 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G2_IO2 = 0x20;        // G2_IO2 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G2_IO1 = 0x10;        // G2_IO1 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G1_IO4 = 0x8;         // G1_IO4 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G1_IO3 = 0x4;         // G1_IO3 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G1_IO2 = 0x2;         // G1_IO2 Schmitt trigger hysteresis mode
    static constexpr uint32_t IOHCR_G1_IO1 = 0x1;         // G1_IO1 Schmitt trigger hysteresis mode
    static const uint32_t IOHCR_RESET_VALUE = 0xffffffff;

    static constexpr uint32_t IOASCR_G6_IO4 = 0x800000;    // G6_IO4 analog switch enable
    static constexpr uint32_t IOASCR_G6_IO3 = 0x400000;    // G6_IO3 analog switch enable
    static constexpr uint32_t IOASCR_G6_IO2 = 0x200000;    // G6_IO2 analog switch enable
    static constexpr uint32_t IOASCR_G6_IO1 = 0x100000;    // G6_IO1 analog switch enable
    static constexpr uint32_t IOASCR_G5_IO4 = 0x80000;     // G5_IO4 analog switch enable
    static constexpr uint32_t IOASCR_G5_IO3 = 0x40000;     // G5_IO3 analog switch enable
    static constexpr uint32_t IOASCR_G5_IO2 = 0x20000;     // G5_IO2 analog switch enable
    static constexpr uint32_t IOASCR_G5_IO1 = 0x10000;     // G5_IO1 analog switch enable
    static constexpr uint32_t IOASCR_G4_IO4 = 0x8000;      // G4_IO4 analog switch enable
    static constexpr uint32_t IOASCR_G4_IO3 = 0x4000;      // G4_IO3 analog switch enable
    static constexpr uint32_t IOASCR_G4_IO2 = 0x2000;      // G4_IO2 analog switch enable
    static constexpr uint32_t IOASCR_G4_IO1 = 0x1000;      // G4_IO1 analog switch enable
    static constexpr uint32_t IOASCR_G3_IO4 = 0x800;       // G3_IO4 analog switch enable
    static constexpr uint32_t IOASCR_G3_IO3 = 0x400;       // G3_IO3 analog switch enable
    static constexpr uint32_t IOASCR_G3_IO2 = 0x200;       // G3_IO2 analog switch enable
    static constexpr uint32_t IOASCR_G3_IO1 = 0x100;       // G3_IO1 analog switch enable
    static constexpr uint32_t IOASCR_G2_IO4 = 0x80;        // G2_IO4 analog switch enable
    static constexpr uint32_t IOASCR_G2_IO3 = 0x40;        // G2_IO3 analog switch enable
    static constexpr uint32_t IOASCR_G2_IO2 = 0x20;        // G2_IO2 analog switch enable
    static constexpr uint32_t IOASCR_G2_IO1 = 0x10;        // G2_IO1 analog switch enable
    static constexpr uint32_t IOASCR_G1_IO4 = 0x8;         // G1_IO4 analog switch enable
    static constexpr uint32_t IOASCR_G1_IO3 = 0x4;         // G1_IO3 analog switch enable
    static constexpr uint32_t IOASCR_G1_IO2 = 0x2;         // G1_IO2 analog switch enable
    static constexpr uint32_t IOASCR_G1_IO1 = 0x1;         // G1_IO1 analog switch enable
    static const uint32_t IOASCR_RESET_VALUE = 0x0;

    static constexpr uint32_t IOSCR_G6_IO4 = 0x800000;    // G6_IO4 sampling mode
    static constexpr uint32_t IOSCR_G6_IO3 = 0x400000;    // G6_IO3 sampling mode
    static constexpr uint32_t IOSCR_G6_IO2 = 0x200000;    // G6_IO2 sampling mode
    static constexpr uint32_t IOSCR_G6_IO1 = 0x100000;    // G6_IO1 sampling mode
    static constexpr uint32_t IOSCR_G5_IO4 = 0x80000;     // G5_IO4 sampling mode
    static constexpr uint32_t IOSCR_G5_IO3 = 0x40000;     // G5_IO3 sampling mode
    static constexpr uint32_t IOSCR_G5_IO2 = 0x20000;     // G5_IO2 sampling mode
    static constexpr uint32_t IOSCR_G5_IO1 = 0x10000;     // G5_IO1 sampling mode
    static constexpr uint32_t IOSCR_G4_IO4 = 0x8000;      // G4_IO4 sampling mode
    static constexpr uint32_t IOSCR_G4_IO3 = 0x4000;      // G4_IO3 sampling mode
    static constexpr uint32_t IOSCR_G4_IO2 = 0x2000;      // G4_IO2 sampling mode
    static constexpr uint32_t IOSCR_G4_IO1 = 0x1000;      // G4_IO1 sampling mode
    static constexpr uint32_t IOSCR_G3_IO4 = 0x800;       // G3_IO4 sampling mode
    static constexpr uint32_t IOSCR_G3_IO3 = 0x400;       // G3_IO3 sampling mode
    static constexpr uint32_t IOSCR_G3_IO2 = 0x200;       // G3_IO2 sampling mode
    static constexpr uint32_t IOSCR_G3_IO1 = 0x100;       // G3_IO1 sampling mode
    static constexpr uint32_t IOSCR_G2_IO4 = 0x80;        // G2_IO4 sampling mode
    static constexpr uint32_t IOSCR_G2_IO3 = 0x40;        // G2_IO3 sampling mode
    static constexpr uint32_t IOSCR_G2_IO2 = 0x20;        // G2_IO2 sampling mode
    static constexpr uint32_t IOSCR_G2_IO1 = 0x10;        // G2_IO1 sampling mode
    static constexpr uint32_t IOSCR_G1_IO4 = 0x8;         // G1_IO4 sampling mode
    static constexpr uint32_t IOSCR_G1_IO3 = 0x4;         // G1_IO3 sampling mode
    static constexpr uint32_t IOSCR_G1_IO2 = 0x2;         // G1_IO2 sampling mode
    static constexpr uint32_t IOSCR_G1_IO1 = 0x1;         // G1_IO1 sampling mode
    static const uint32_t IOSCR_RESET_VALUE = 0x0;

    static constexpr uint32_t IOCCR_G6_IO4 = 0x800000;    // G6_IO4 channel mode
    static constexpr uint32_t IOCCR_G6_IO3 = 0x400000;    // G6_IO3 channel mode
    static constexpr uint32_t IOCCR_G6_IO2 = 0x200000;    // G6_IO2 channel mode
    static constexpr uint32_t IOCCR_G6_IO1 = 0x100000;    // G6_IO1 channel mode
    static constexpr uint32_t IOCCR_G5_IO4 = 0x80000;     // G5_IO4 channel mode
    static constexpr uint32_t IOCCR_G5_IO3 = 0x40000;     // G5_IO3 channel mode
    static constexpr uint32_t IOCCR_G5_IO2 = 0x20000;     // G5_IO2 channel mode
    static constexpr uint32_t IOCCR_G5_IO1 = 0x10000;     // G5_IO1 channel mode
    static constexpr uint32_t IOCCR_G4_IO4 = 0x8000;      // G4_IO4 channel mode
    static constexpr uint32_t IOCCR_G4_IO3 = 0x4000;      // G4_IO3 channel mode
    static constexpr uint32_t IOCCR_G4_IO2 = 0x2000;      // G4_IO2 channel mode
    static constexpr uint32_t IOCCR_G4_IO1 = 0x1000;      // G4_IO1 channel mode
    static constexpr uint32_t IOCCR_G3_IO4 = 0x800;       // G3_IO4 channel mode
    static constexpr uint32_t IOCCR_G3_IO3 = 0x400;       // G3_IO3 channel mode
    static constexpr uint32_t IOCCR_G3_IO2 = 0x200;       // G3_IO2 channel mode
    static constexpr uint32_t IOCCR_G3_IO1 = 0x100;       // G3_IO1 channel mode
    static constexpr uint32_t IOCCR_G2_IO4 = 0x80;        // G2_IO4 channel mode
    static constexpr uint32_t IOCCR_G2_IO3 = 0x40;        // G2_IO3 channel mode
    static constexpr uint32_t IOCCR_G2_IO2 = 0x20;        // G2_IO2 channel mode
    static constexpr uint32_t IOCCR_G2_IO1 = 0x10;        // G2_IO1 channel mode
    static constexpr uint32_t IOCCR_G1_IO4 = 0x8;         // G1_IO4 channel mode
    static constexpr uint32_t IOCCR_G1_IO3 = 0x4;         // G1_IO3 channel mode
    static constexpr uint32_t IOCCR_G1_IO2 = 0x2;         // G1_IO2 channel mode
    static constexpr uint32_t IOCCR_G1_IO1 = 0x1;         // G1_IO1 channel mode
    static const uint32_t IOCCR_RESET_VALUE = 0x0;

    static constexpr uint32_t IOGCSR_G8S = 0x800000;       // Analog I/O group x status, Read-write
    static constexpr uint32_t IOGCSR_G7S = 0x400000;       // Analog I/O group x status, Read-write
    static constexpr uint32_t IOGCSR_G6S = 0x200000;       // Analog I/O group x status, Read-only
    static constexpr uint32_t IOGCSR_G5S = 0x100000;       // Analog I/O group x status, Read-only
    static constexpr uint32_t IOGCSR_G4S = 0x80000;        // Analog I/O group x status, Read-only
    static constexpr uint32_t IOGCSR_G3S = 0x40000;        // Analog I/O group x status, Read-only
    static constexpr uint32_t IOGCSR_G2S = 0x20000;        // Analog I/O group x status, Read-only
    static constexpr uint32_t IOGCSR_G1S = 0x10000;        // Analog I/O group x status, Read-only
    static constexpr uint32_t IOGCSR_G8E = 0x80;           // Analog I/O group x enable, Read-write
    static constexpr uint32_t IOGCSR_G7E = 0x40;           // Analog I/O group x enable, Read-write
    static constexpr uint32_t IOGCSR_G6E = 0x20;           // Analog I/O group x enable, Read-write
    static constexpr uint32_t IOGCSR_G5E = 0x10;           // Analog I/O group x enable, Read-write
    static constexpr uint32_t IOGCSR_G4E = 0x8;            // Analog I/O group x enable, Read-write
    static constexpr uint32_t IOGCSR_G3E = 0x4;            // Analog I/O group x enable, Read-write
    static constexpr uint32_t IOGCSR_G2E = 0x2;            // Analog I/O group x enable, Read-write
    static constexpr uint32_t IOGCSR_G1E = 0x1;            // Analog I/O group x enable, Read-write
    static const uint32_t IOGCSR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IOG1CR_CNT =                 // Counter value (14 bits)
        bit_field_t<0, 0x3fff>::value<X>();
    static const uint32_t IOG1CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IOG2CR_CNT =                 // Counter value (14 bits)
        bit_field_t<0, 0x3fff>::value<X>();
    static const uint32_t IOG2CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IOG3CR_CNT =                 // Counter value (14 bits)
        bit_field_t<0, 0x3fff>::value<X>();
    static const uint32_t IOG3CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IOG4CR_CNT =                 // Counter value (14 bits)
        bit_field_t<0, 0x3fff>::value<X>();
    static const uint32_t IOG4CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IOG5CR_CNT =                 // Counter value (14 bits)
        bit_field_t<0, 0x3fff>::value<X>();
    static const uint32_t IOG5CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IOG6CR_CNT =                 // Counter value (14 bits)
        bit_field_t<0, 0x3fff>::value<X>();
    static const uint32_t IOG6CR_RESET_VALUE = 0x0;

    static constexpr uint8_t TSC = 8; // Touch sensing interrupt
};

static tsc_t& TSC = *reinterpret_cast<tsc_t*>(0x40024000);

#define HAVE_PERIPHERAL_TSC


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

    static constexpr uint32_t CR_TXEOM = 0x4;          // Tx End Of Message
    static constexpr uint32_t CR_TXSOM = 0x2;          // Tx start of message
    static constexpr uint32_t CR_CECEN = 0x1;          // CEC Enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t CFGR_LBPEGEN = 0x800;      // Generate Error-Bit on Long Bit Period Error
    static constexpr uint32_t CFGR_BREGEN = 0x400;       // Generate error-bit on bit rising error
    static constexpr uint32_t CFGR_BRESTP = 0x200;       // Rx-stop on bit rising error
    static constexpr uint32_t CFGR_RXTOL = 0x100;        // Rx-Tolerance
    template<uint32_t X>
    static constexpr uint32_t CFGR_SFT =                 // Signal Free Time (3 bits)
        bit_field_t<5, 0x7>::value<X>();
    static constexpr uint32_t CFGR_LSTN = 0x10;          // Listen mode
    template<uint32_t X>
    static constexpr uint32_t CFGR_OAR =                 // Own Address (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CFGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXDR_TXD =                 // Tx Data register (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TXDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXDR_RXDR =                // CEC Rx Data Register (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RXDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ISR_TXACKE = 0x1000;      // Tx-Missing acknowledge error
    static constexpr uint32_t ISR_TXERR = 0x800;        // Tx-Error
    static constexpr uint32_t ISR_TXUDR = 0x400;        // Tx-Buffer Underrun
    static constexpr uint32_t ISR_TXEND = 0x200;        // End of Transmission
    static constexpr uint32_t ISR_TXBR = 0x100;         // Tx-Byte Request
    static constexpr uint32_t ISR_ARBLST = 0x80;        // Arbitration Lost
    static constexpr uint32_t ISR_RXACKE = 0x40;        // Rx-Missing Acknowledge
    static constexpr uint32_t ISR_LBPE = 0x20;          // Rx-Long Bit Period Error
    static constexpr uint32_t ISR_SBPE = 0x10;          // Rx-Short Bit period error
    static constexpr uint32_t ISR_BRE = 0x8;            // Rx-Bit rising error
    static constexpr uint32_t ISR_RXOVR = 0x4;          // Rx-Overrun
    static constexpr uint32_t ISR_RXEND = 0x2;          // End Of Reception
    static constexpr uint32_t ISR_RXBR = 0x1;           // Rx-Byte Received
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t IER_TXACKIE = 0x1000;     // Tx-Missing Acknowledge Error Interrupt Enable
    static constexpr uint32_t IER_TXERRIE = 0x800;      // Tx-Error Interrupt Enable
    static constexpr uint32_t IER_TXUDRIE = 0x400;      // Tx-Underrun interrupt enable
    static constexpr uint32_t IER_TXENDIE = 0x200;      // Tx-End of message interrupt enable
    static constexpr uint32_t IER_TXBRIE = 0x100;       // Tx-Byte Request Interrupt Enable
    static constexpr uint32_t IER_ARBLSTIE = 0x80;      // Arbitration Lost Interrupt Enable
    static constexpr uint32_t IER_RXACKIE = 0x40;       // Rx-Missing Acknowledge Error Interrupt Enable
    static constexpr uint32_t IER_LBPEIE = 0x20;        // Long Bit Period Error Interrupt Enable
    static constexpr uint32_t IER_SBPEIE = 0x10;        // Short Bit Period Error Interrupt Enable
    static constexpr uint32_t IER_BREIE = 0x8;          // Bit Rising Error Interrupt Enable
    static constexpr uint32_t IER_RXOVRIE = 0x4;        // Rx-Buffer Overrun Interrupt Enable
    static constexpr uint32_t IER_RXENDIE = 0x2;        // End Of Reception Interrupt Enable
    static constexpr uint32_t IER_RXBRIE = 0x1;         // Rx-Byte Received Interrupt Enable
    static const uint32_t IER_RESET_VALUE = 0x0;

    static constexpr uint8_t CEC_CAN = 30; // CEC and CAN global interrupt
};

static cec_t& CEC = *reinterpret_cast<cec_t*>(0x40007800);

#define HAVE_PERIPHERAL_CEC


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

    template<uint32_t X>
    static constexpr uint32_t ACR_LATENCY =             // LATENCY (3 bits), Read-write
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t ACR_PRFTBE = 0x10;        // PRFTBE, Read-write
    static constexpr uint32_t ACR_PRFTBS = 0x20;        // PRFTBS, Read-only
    static const uint32_t ACR_RESET_VALUE = 0x30;


    static const uint32_t KEYR_RESET_VALUE = 0x0;


    static const uint32_t OPTKEYR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_EOP = 0x20;           // End of operation, Read-write
    static constexpr uint32_t SR_WRPRT = 0x10;         // Write protection error, Read-write
    static constexpr uint32_t SR_PGERR = 0x4;          // Programming error, Read-write
    static constexpr uint32_t SR_BSY = 0x1;            // Busy, Read-only
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_FORCE_OPTLOAD = 0x2000;// Force option byte loading
    static constexpr uint32_t CR_EOPIE = 0x1000;       // End of operation interrupt enable
    static constexpr uint32_t CR_ERRIE = 0x400;        // Error interrupt enable
    static constexpr uint32_t CR_OPTWRE = 0x200;       // Option bytes write enable
    static constexpr uint32_t CR_LOCK = 0x80;          // Lock
    static constexpr uint32_t CR_STRT = 0x40;          // Start
    static constexpr uint32_t CR_OPTER = 0x20;         // Option byte erase
    static constexpr uint32_t CR_OPTPG = 0x10;         // Option byte programming
    static constexpr uint32_t CR_MER = 0x4;            // Mass erase
    static constexpr uint32_t CR_PER = 0x2;            // Page erase
    static constexpr uint32_t CR_PG = 0x1;             // Programming
    static const uint32_t CR_RESET_VALUE = 0x80;


    static const uint32_t AR_RESET_VALUE = 0x0;

    static constexpr uint32_t OBR_OPTERR = 0x1;         // Option byte error
    template<uint32_t X>
    static constexpr uint32_t OBR_RDPRT =               // Read protection level status (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t OBR_WDG_SW = 0x100;       // WDG_SW
    static constexpr uint32_t OBR_nRST_STOP = 0x200;    // nRST_STOP
    static constexpr uint32_t OBR_nRST_STDBY = 0x400;   // nRST_STDBY
    static constexpr uint32_t OBR_nBOOT0 = 0x800;       // nBOOT0
    static constexpr uint32_t OBR_nBOOT1 = 0x1000;      // BOOT1
    static constexpr uint32_t OBR_VDDA_MONITOR = 0x2000;// VDDA_MONITOR
    static constexpr uint32_t OBR_RAM_PARITY_CHECK = 0x4000;// RAM_PARITY_CHECK
    static constexpr uint32_t OBR_BOOT_SEL = 0x8000;    // BOOT_SEL
    template<uint32_t X>
    static constexpr uint32_t OBR_Data0 =               // Data0 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OBR_Data1 =               // Data1 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t OBR_RESET_VALUE = 0x3fffff2;


    static const uint32_t WRPR_RESET_VALUE = 0xffffffff;

    static constexpr uint8_t FLASH = 3; // Flash global interrupt
};

static flash_t& FLASH = *reinterpret_cast<flash_t*>(0x40022000);

#define HAVE_PERIPHERAL_FLASH


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

    template<uint32_t X>
    static constexpr uint32_t IDCODE_DEV_ID =              // Device Identifier (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IDCODE_DIV_ID =              // Division Identifier (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IDCODE_REV_ID =              // Revision Identifier (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t IDCODE_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_DBG_STOP = 0x2;       // Debug Stop Mode
    static constexpr uint32_t CR_DBG_STANDBY = 0x4;    // Debug Standby Mode
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t APB1_FZ_DBG_TIM2_STOP = 0x1;  // TIM2 counter stopped when core is halted
    static constexpr uint32_t APB1_FZ_DBG_TIM3_STOP = 0x2;  // TIM3 counter stopped when core is halted
    static constexpr uint32_t APB1_FZ_DBG_TIM6_STOP = 0x10; // TIM6 counter stopped when core is halted
    static constexpr uint32_t APB1_FZ_DBG_TIM7_STOP = 0x20; // TIM7 counter stopped when core is halted
    static constexpr uint32_t APB1_FZ_DBG_TIM14_STOP = 0x100;// TIM14 counter stopped when core is halted
    static constexpr uint32_t APB1_FZ_DBG_RTC_STOP = 0x400; // Debug RTC stopped when core is halted
    static constexpr uint32_t APB1_FZ_DBG_WWDG_STOP = 0x800;// Debug window watchdog stopped when core is halted
    static constexpr uint32_t APB1_FZ_DBG_IWDG_STOP = 0x1000;// Debug independent watchdog stopped when core is halted
    static constexpr uint32_t APB1_FZ_DBG_I2C1_SMBUS_TIMEOUT = 0x200000;// SMBUS timeout mode stopped when core is halted
    static constexpr uint32_t APB1_FZ_DBG_CAN_STOP = 0x2000000;// CAN stopped when core is halted
    static const uint32_t APB1_FZ_RESET_VALUE = 0x0;

    static constexpr uint32_t APB2_FZ_DBG_TIM1_STOP = 0x800;// TIM1 counter stopped when core is halted
    static constexpr uint32_t APB2_FZ_DBG_TIM15_STOP = 0x10000;// TIM15 counter stopped when core is halted
    static constexpr uint32_t APB2_FZ_DBG_TIM16_STOP = 0x20000;// TIM16 counter stopped when core is halted
    static constexpr uint32_t APB2_FZ_DBG_TIM17_STOP = 0x40000;// TIM17 counter stopped when core is halted
    static const uint32_t APB2_FZ_RESET_VALUE = 0x0;
};

static dbgmcu_t& DBGMCU = *reinterpret_cast<dbgmcu_t*>(0x40015800);

#define HAVE_PERIPHERAL_DBGMCU


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

    template<uint32_t X>
    static constexpr uint32_t EP0R_EA =                  // Endpoint address (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP0R_STAT_TX =             // Status bits, for transmission transfers (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP0R_DTOG_TX = 0x40;       // Data Toggle, for transmission transfers
    static constexpr uint32_t EP0R_CTR_TX = 0x80;        // Correct Transfer for transmission
    static constexpr uint32_t EP0R_EP_KIND = 0x100;      // Endpoint kind
    template<uint32_t X>
    static constexpr uint32_t EP0R_EP_TYPE =             // Endpoint type (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP0R_SETUP = 0x800;        // Setup transaction completed
    template<uint32_t X>
    static constexpr uint32_t EP0R_STAT_RX =             // Status bits, for reception transfers (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP0R_DTOG_RX = 0x4000;     // Data Toggle, for reception transfers
    static constexpr uint32_t EP0R_CTR_RX = 0x8000;      // Correct transfer for reception
    static const uint32_t EP0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP1R_EA =                  // Endpoint address (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP1R_STAT_TX =             // Status bits, for transmission transfers (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP1R_DTOG_TX = 0x40;       // Data Toggle, for transmission transfers
    static constexpr uint32_t EP1R_CTR_TX = 0x80;        // Correct Transfer for transmission
    static constexpr uint32_t EP1R_EP_KIND = 0x100;      // Endpoint kind
    template<uint32_t X>
    static constexpr uint32_t EP1R_EP_TYPE =             // Endpoint type (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP1R_SETUP = 0x800;        // Setup transaction completed
    template<uint32_t X>
    static constexpr uint32_t EP1R_STAT_RX =             // Status bits, for reception transfers (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP1R_DTOG_RX = 0x4000;     // Data Toggle, for reception transfers
    static constexpr uint32_t EP1R_CTR_RX = 0x8000;      // Correct transfer for reception
    static const uint32_t EP1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP2R_EA =                  // Endpoint address (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP2R_STAT_TX =             // Status bits, for transmission transfers (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP2R_DTOG_TX = 0x40;       // Data Toggle, for transmission transfers
    static constexpr uint32_t EP2R_CTR_TX = 0x80;        // Correct Transfer for transmission
    static constexpr uint32_t EP2R_EP_KIND = 0x100;      // Endpoint kind
    template<uint32_t X>
    static constexpr uint32_t EP2R_EP_TYPE =             // Endpoint type (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP2R_SETUP = 0x800;        // Setup transaction completed
    template<uint32_t X>
    static constexpr uint32_t EP2R_STAT_RX =             // Status bits, for reception transfers (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP2R_DTOG_RX = 0x4000;     // Data Toggle, for reception transfers
    static constexpr uint32_t EP2R_CTR_RX = 0x8000;      // Correct transfer for reception
    static const uint32_t EP2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP3R_EA =                  // Endpoint address (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP3R_STAT_TX =             // Status bits, for transmission transfers (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP3R_DTOG_TX = 0x40;       // Data Toggle, for transmission transfers
    static constexpr uint32_t EP3R_CTR_TX = 0x80;        // Correct Transfer for transmission
    static constexpr uint32_t EP3R_EP_KIND = 0x100;      // Endpoint kind
    template<uint32_t X>
    static constexpr uint32_t EP3R_EP_TYPE =             // Endpoint type (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP3R_SETUP = 0x800;        // Setup transaction completed
    template<uint32_t X>
    static constexpr uint32_t EP3R_STAT_RX =             // Status bits, for reception transfers (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP3R_DTOG_RX = 0x4000;     // Data Toggle, for reception transfers
    static constexpr uint32_t EP3R_CTR_RX = 0x8000;      // Correct transfer for reception
    static const uint32_t EP3R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP4R_EA =                  // Endpoint address (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP4R_STAT_TX =             // Status bits, for transmission transfers (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP4R_DTOG_TX = 0x40;       // Data Toggle, for transmission transfers
    static constexpr uint32_t EP4R_CTR_TX = 0x80;        // Correct Transfer for transmission
    static constexpr uint32_t EP4R_EP_KIND = 0x100;      // Endpoint kind
    template<uint32_t X>
    static constexpr uint32_t EP4R_EP_TYPE =             // Endpoint type (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP4R_SETUP = 0x800;        // Setup transaction completed
    template<uint32_t X>
    static constexpr uint32_t EP4R_STAT_RX =             // Status bits, for reception transfers (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP4R_DTOG_RX = 0x4000;     // Data Toggle, for reception transfers
    static constexpr uint32_t EP4R_CTR_RX = 0x8000;      // Correct transfer for reception
    static const uint32_t EP4R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP5R_EA =                  // Endpoint address (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP5R_STAT_TX =             // Status bits, for transmission transfers (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP5R_DTOG_TX = 0x40;       // Data Toggle, for transmission transfers
    static constexpr uint32_t EP5R_CTR_TX = 0x80;        // Correct Transfer for transmission
    static constexpr uint32_t EP5R_EP_KIND = 0x100;      // Endpoint kind
    template<uint32_t X>
    static constexpr uint32_t EP5R_EP_TYPE =             // Endpoint type (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP5R_SETUP = 0x800;        // Setup transaction completed
    template<uint32_t X>
    static constexpr uint32_t EP5R_STAT_RX =             // Status bits, for reception transfers (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP5R_DTOG_RX = 0x4000;     // Data Toggle, for reception transfers
    static constexpr uint32_t EP5R_CTR_RX = 0x8000;      // Correct transfer for reception
    static const uint32_t EP5R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP6R_EA =                  // Endpoint address (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP6R_STAT_TX =             // Status bits, for transmission transfers (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP6R_DTOG_TX = 0x40;       // Data Toggle, for transmission transfers
    static constexpr uint32_t EP6R_CTR_TX = 0x80;        // Correct Transfer for transmission
    static constexpr uint32_t EP6R_EP_KIND = 0x100;      // Endpoint kind
    template<uint32_t X>
    static constexpr uint32_t EP6R_EP_TYPE =             // Endpoint type (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP6R_SETUP = 0x800;        // Setup transaction completed
    template<uint32_t X>
    static constexpr uint32_t EP6R_STAT_RX =             // Status bits, for reception transfers (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP6R_DTOG_RX = 0x4000;     // Data Toggle, for reception transfers
    static constexpr uint32_t EP6R_CTR_RX = 0x8000;      // Correct transfer for reception
    static const uint32_t EP6R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP7R_EA =                  // Endpoint address (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP7R_STAT_TX =             // Status bits, for transmission transfers (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP7R_DTOG_TX = 0x40;       // Data Toggle, for transmission transfers
    static constexpr uint32_t EP7R_CTR_TX = 0x80;        // Correct Transfer for transmission
    static constexpr uint32_t EP7R_EP_KIND = 0x100;      // Endpoint kind
    template<uint32_t X>
    static constexpr uint32_t EP7R_EP_TYPE =             // Endpoint type (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP7R_SETUP = 0x800;        // Setup transaction completed
    template<uint32_t X>
    static constexpr uint32_t EP7R_STAT_RX =             // Status bits, for reception transfers (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP7R_DTOG_RX = 0x4000;     // Data Toggle, for reception transfers
    static constexpr uint32_t EP7R_CTR_RX = 0x8000;      // Correct transfer for reception
    static const uint32_t EP7R_RESET_VALUE = 0x0;

    static constexpr uint32_t CNTR_FRES = 0x1;           // Force USB Reset
    static constexpr uint32_t CNTR_PDWN = 0x2;           // Power down
    static constexpr uint32_t CNTR_LPMODE = 0x4;         // Low-power mode
    static constexpr uint32_t CNTR_FSUSP = 0x8;          // Force suspend
    static constexpr uint32_t CNTR_RESUME = 0x10;        // Resume request
    static constexpr uint32_t CNTR_L1RESUME = 0x20;      // LPM L1 Resume request
    static constexpr uint32_t CNTR_L1REQM = 0x80;        // LPM L1 state request interrupt mask
    static constexpr uint32_t CNTR_ESOFM = 0x100;        // Expected start of frame interrupt mask
    static constexpr uint32_t CNTR_SOFM = 0x200;         // Start of frame interrupt mask
    static constexpr uint32_t CNTR_RESETM = 0x400;       // USB reset interrupt mask
    static constexpr uint32_t CNTR_SUSPM = 0x800;        // Suspend mode interrupt mask
    static constexpr uint32_t CNTR_WKUPM = 0x1000;       // Wakeup interrupt mask
    static constexpr uint32_t CNTR_ERRM = 0x2000;        // Error interrupt mask
    static constexpr uint32_t CNTR_PMAOVRM = 0x4000;     // Packet memory area over / underrun interrupt mask
    static constexpr uint32_t CNTR_CTRM = 0x8000;        // Correct transfer interrupt mask
    static const uint32_t CNTR_RESET_VALUE = 0x3;

    template<uint32_t X>
    static constexpr uint32_t ISTR_EP_ID =               // Endpoint Identifier (4 bits), Read-only
        bit_field_t<0, 0xf>::value<X>();
    static constexpr uint32_t ISTR_DIR = 0x10;           // Direction of transaction, Read-only
    static constexpr uint32_t ISTR_L1REQ = 0x80;         // LPM L1 state request, Read-write
    static constexpr uint32_t ISTR_ESOF = 0x100;         // Expected start frame, Read-write
    static constexpr uint32_t ISTR_SOF = 0x200;          // start of frame, Read-write
    static constexpr uint32_t ISTR_RESET = 0x400;        // reset request, Read-write
    static constexpr uint32_t ISTR_SUSP = 0x800;         // Suspend mode request, Read-write
    static constexpr uint32_t ISTR_WKUP = 0x1000;        // Wakeup, Read-write
    static constexpr uint32_t ISTR_ERR = 0x2000;         // Error, Read-write
    static constexpr uint32_t ISTR_PMAOVR = 0x4000;      // Packet memory area over / underrun, Read-write
    static constexpr uint32_t ISTR_CTR = 0x8000;         // Correct transfer, Read-only
    static const uint32_t ISTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FNR_FN =                  // Frame number (11 bits)
        bit_field_t<0, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FNR_LSOF =                // Lost SOF (2 bits)
        bit_field_t<11, 0x3>::value<X>();
    static constexpr uint32_t FNR_LCK = 0x2000;         // Locked
    static constexpr uint32_t FNR_RXDM = 0x4000;        // Receive data - line status
    static constexpr uint32_t FNR_RXDP = 0x8000;        // Receive data + line status
    static const uint32_t FNR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DADDR_ADD =                 // Device address (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t DADDR_EF = 0x80;            // Enable function
    static const uint32_t DADDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BTABLE_BTABLE =              // Buffer table (13 bits)
        bit_field_t<3, 0x1fff>::value<X>();
    static const uint32_t BTABLE_RESET_VALUE = 0x0;

    static constexpr uint32_t LPMCSR_LPMEN = 0x1;          // LPM support enable, Read-write
    static constexpr uint32_t LPMCSR_LPMACK = 0x2;         // LPM Token acknowledge enable, Read-write
    static constexpr uint32_t LPMCSR_REMWAKE = 0x8;        // bRemoteWake value, Read-only
    template<uint32_t X>
    static constexpr uint32_t LPMCSR_BESL =                // BESL value (4 bits), Read-only
        bit_field_t<4, 0xf>::value<X>();
    static const uint32_t LPMCSR_RESET_VALUE = 0x0;

    static constexpr uint32_t BCDR_BCDEN = 0x1;          // Battery charging detector (BCD) enable, Read-write
    static constexpr uint32_t BCDR_DCDEN = 0x2;          // Data contact detection (DCD) mode enable, Read-write
    static constexpr uint32_t BCDR_PDEN = 0x4;           // Primary detection (PD) mode enable, Read-write
    static constexpr uint32_t BCDR_SDEN = 0x8;           // Secondary detection (SD) mode enable, Read-write
    static constexpr uint32_t BCDR_DCDET = 0x10;         // Data contact detection (DCD) status, Read-only
    static constexpr uint32_t BCDR_PDET = 0x20;          // Primary detection (PD) status, Read-only
    static constexpr uint32_t BCDR_SDET = 0x40;          // Secondary detection (SD) status, Read-only
    static constexpr uint32_t BCDR_PS2DET = 0x80;        // DM pull-up detection status, Read-only
    static constexpr uint32_t BCDR_DPPU = 0x8000;        // DP pull-up control, Read-write
    static const uint32_t BCDR_RESET_VALUE = 0x0;

    static constexpr uint8_t USB = 31; // USB global interrupt
};

static usb_t& USB = *reinterpret_cast<usb_t*>(0x40005c00);

#define HAVE_PERIPHERAL_USB


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

    template<uint32_t X>
    static constexpr uint32_t CR_TRIM =                // HSI48 oscillator smooth trimming (6 bits)
        bit_field_t<8, 0x3f>::value<X>();
    static constexpr uint32_t CR_SWSYNC = 0x80;        // Generate software SYNC event
    static constexpr uint32_t CR_AUTOTRIMEN = 0x40;    // Automatic trimming enable
    static constexpr uint32_t CR_CEN = 0x20;           // Frequency error counter enable
    static constexpr uint32_t CR_ESYNCIE = 0x8;        // Expected SYNC interrupt enable
    static constexpr uint32_t CR_ERRIE = 0x4;          // Synchronization or trimming error interrupt enable
    static constexpr uint32_t CR_SYNCWARNIE = 0x2;     // SYNC warning interrupt enable
    static constexpr uint32_t CR_SYNCOKIE = 0x1;       // SYNC event OK interrupt enable
    static const uint32_t CR_RESET_VALUE = 0x2000;

    static constexpr uint32_t CFGR_SYNCPOL = 0x80000000; // SYNC polarity selection
    template<uint32_t X>
    static constexpr uint32_t CFGR_SYNCSRC =             // SYNC signal source selection (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_SYNCDIV =             // SYNC divider (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_FELIM =               // Frequency error limit (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_RELOAD =              // Counter reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CFGR_RESET_VALUE = 0x2022bb7f;

    template<uint32_t X>
    static constexpr uint32_t ISR_FECAP =               // Frequency error capture (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static constexpr uint32_t ISR_FEDIR = 0x8000;       // Frequency error direction
    static constexpr uint32_t ISR_TRIMOVF = 0x400;      // Trimming overflow or underflow
    static constexpr uint32_t ISR_SYNCMISS = 0x200;     // SYNC missed
    static constexpr uint32_t ISR_SYNCERR = 0x100;      // SYNC error
    static constexpr uint32_t ISR_ESYNCF = 0x8;         // Expected SYNC flag
    static constexpr uint32_t ISR_ERRF = 0x4;           // Error flag
    static constexpr uint32_t ISR_SYNCWARNF = 0x2;      // SYNC warning flag
    static constexpr uint32_t ISR_SYNCOKF = 0x1;        // SYNC event OK flag
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t ICR_ESYNCC = 0x8;         // Expected SYNC clear flag
    static constexpr uint32_t ICR_ERRC = 0x4;           // Error clear flag
    static constexpr uint32_t ICR_SYNCWARNC = 0x2;      // SYNC warning clear flag
    static constexpr uint32_t ICR_SYNCOKC = 0x1;        // SYNC event OK clear flag
    static const uint32_t ICR_RESET_VALUE = 0x0;
};

static crs_t& CRS = *reinterpret_cast<crs_t*>(0x40006c00);

#define HAVE_PERIPHERAL_CRS


////
//
//    Controller area network
//
////

struct can_t
{
    volatile uint32_t    MCR;                  // [Read-write] CAN_MCR
    volatile uint32_t    MSR;                  // CAN_MSR
    volatile uint32_t    TSR;                  // CAN_TSR
    volatile uint32_t    RF0R;                 // CAN_RF0R
    volatile uint32_t    RF1R;                 // CAN_RF1R
    volatile uint32_t    IER;                  // [Read-write] CAN_IER
    volatile uint32_t    ESR;                  // CAN_ESR
    volatile uint32_t    BTR;                  // [Read-write] CAN BTR
    reserved_t<88>       _0;
    volatile uint32_t    TI0R;                 // [Read-write] CAN_TI0R
    volatile uint32_t    TDT0R;                // [Read-write] CAN_TDT0R
    volatile uint32_t    TDL0R;                // [Read-write] CAN_TDL0R
    volatile uint32_t    TDH0R;                // [Read-write] CAN_TDH0R
    volatile uint32_t    TI1R;                 // [Read-write] CAN_TI1R
    volatile uint32_t    TDT1R;                // [Read-write] CAN_TDT1R
    volatile uint32_t    TDL1R;                // [Read-write] CAN_TDL1R
    volatile uint32_t    TDH1R;                // [Read-write] CAN_TDH1R
    volatile uint32_t    TI2R;                 // [Read-write] CAN_TI2R
    volatile uint32_t    TDT2R;                // [Read-write] CAN_TDT2R
    volatile uint32_t    TDL2R;                // [Read-write] CAN_TDL2R
    volatile uint32_t    TDH2R;                // [Read-write] CAN_TDH2R
    volatile uint32_t    RI0R;                 // [Read-only] CAN_RI0R
    volatile uint32_t    RDT0R;                // [Read-only] CAN_RDT0R
    volatile uint32_t    RDL0R;                // [Read-only] CAN_RDL0R
    volatile uint32_t    RDH0R;                // [Read-only] CAN_RDH0R
    volatile uint32_t    RI1R;                 // [Read-only] CAN_RI1R
    volatile uint32_t    RDT1R;                // [Read-only] CAN_RDT1R
    volatile uint32_t    RDL1R;                // [Read-only] CAN_RDL1R
    volatile uint32_t    RDH1R;                // [Read-only] CAN_RDH1R
    reserved_t<12>       _1;
    volatile uint32_t    FMR;                  // [Read-write] CAN_FMR
    volatile uint32_t    FM1R;                 // [Read-write] CAN_FM1R
    reserved_t<1>        _2;
    volatile uint32_t    FS1R;                 // [Read-write] CAN_FS1R
    reserved_t<1>        _3;
    volatile uint32_t    FFA1R;                // [Read-write] CAN_FFA1R
    reserved_t<1>        _4;
    volatile uint32_t    FA1R;                 // [Read-write] CAN_FA1R
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

    static constexpr uint32_t MCR_DBF = 0x10000;        // DBF
    static constexpr uint32_t MCR_RESET = 0x8000;       // RESET
    static constexpr uint32_t MCR_TTCM = 0x80;          // TTCM
    static constexpr uint32_t MCR_ABOM = 0x40;          // ABOM
    static constexpr uint32_t MCR_AWUM = 0x20;          // AWUM
    static constexpr uint32_t MCR_NART = 0x10;          // NART
    static constexpr uint32_t MCR_RFLM = 0x8;           // RFLM
    static constexpr uint32_t MCR_TXFP = 0x4;           // TXFP
    static constexpr uint32_t MCR_SLEEP = 0x2;          // SLEEP
    static constexpr uint32_t MCR_INRQ = 0x1;           // INRQ
    static const uint32_t MCR_RESET_VALUE = 0x0;

    static constexpr uint32_t MSR_RX = 0x800;           // RX, Read-only
    static constexpr uint32_t MSR_SAMP = 0x400;         // SAMP, Read-only
    static constexpr uint32_t MSR_RXM = 0x200;          // RXM, Read-only
    static constexpr uint32_t MSR_TXM = 0x100;          // TXM, Read-only
    static constexpr uint32_t MSR_SLAKI = 0x10;         // SLAKI, Read-write
    static constexpr uint32_t MSR_WKUI = 0x8;           // WKUI, Read-write
    static constexpr uint32_t MSR_ERRI = 0x4;           // ERRI, Read-write
    static constexpr uint32_t MSR_SLAK = 0x2;           // SLAK, Read-only
    static constexpr uint32_t MSR_INAK = 0x1;           // INAK, Read-only
    static const uint32_t MSR_RESET_VALUE = 0x0;

    static constexpr uint32_t TSR_LOW2 = 0x80000000;    // Lowest priority flag for mailbox 2, Read-only
    static constexpr uint32_t TSR_LOW1 = 0x40000000;    // Lowest priority flag for mailbox 1, Read-only
    static constexpr uint32_t TSR_LOW0 = 0x20000000;    // Lowest priority flag for mailbox 0, Read-only
    static constexpr uint32_t TSR_TME2 = 0x10000000;    // Lowest priority flag for mailbox 2, Read-only
    static constexpr uint32_t TSR_TME1 = 0x8000000;     // Lowest priority flag for mailbox 1, Read-only
    static constexpr uint32_t TSR_TME0 = 0x4000000;     // Lowest priority flag for mailbox 0, Read-only
    template<uint32_t X>
    static constexpr uint32_t TSR_CODE =                // CODE (2 bits), Read-only
        bit_field_t<24, 0x3>::value<X>();
    static constexpr uint32_t TSR_ABRQ2 = 0x800000;     // ABRQ2, Read-write
    static constexpr uint32_t TSR_TERR2 = 0x80000;      // TERR2, Read-write
    static constexpr uint32_t TSR_ALST2 = 0x40000;      // ALST2, Read-write
    static constexpr uint32_t TSR_TXOK2 = 0x20000;      // TXOK2, Read-write
    static constexpr uint32_t TSR_RQCP2 = 0x10000;      // RQCP2, Read-write
    static constexpr uint32_t TSR_ABRQ1 = 0x8000;       // ABRQ1, Read-write
    static constexpr uint32_t TSR_TERR1 = 0x800;        // TERR1, Read-write
    static constexpr uint32_t TSR_ALST1 = 0x400;        // ALST1, Read-write
    static constexpr uint32_t TSR_TXOK1 = 0x200;        // TXOK1, Read-write
    static constexpr uint32_t TSR_RQCP1 = 0x100;        // RQCP1, Read-write
    static constexpr uint32_t TSR_ABRQ0 = 0x80;         // ABRQ0, Read-write
    static constexpr uint32_t TSR_TERR0 = 0x8;          // TERR0, Read-write
    static constexpr uint32_t TSR_ALST0 = 0x4;          // ALST0, Read-write
    static constexpr uint32_t TSR_TXOK0 = 0x2;          // TXOK0, Read-write
    static constexpr uint32_t TSR_RQCP0 = 0x1;          // RQCP0, Read-write
    static const uint32_t TSR_RESET_VALUE = 0x0;

    static constexpr uint32_t RF0R_RFOM0 = 0x20;         // RFOM0, Read-write
    static constexpr uint32_t RF0R_FOVR0 = 0x10;         // FOVR0, Read-write
    static constexpr uint32_t RF0R_FULL0 = 0x8;          // FULL0, Read-write
    template<uint32_t X>
    static constexpr uint32_t RF0R_FMP0 =                // FMP0 (2 bits), Read-only
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t RF0R_RESET_VALUE = 0x0;

    static constexpr uint32_t RF1R_RFOM1 = 0x20;         // RFOM1, Read-write
    static constexpr uint32_t RF1R_FOVR1 = 0x10;         // FOVR1, Read-write
    static constexpr uint32_t RF1R_FULL1 = 0x8;          // FULL1, Read-write
    template<uint32_t X>
    static constexpr uint32_t RF1R_FMP1 =                // FMP1 (2 bits), Read-only
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t RF1R_RESET_VALUE = 0x0;

    static constexpr uint32_t IER_SLKIE = 0x20000;      // SLKIE
    static constexpr uint32_t IER_WKUIE = 0x10000;      // WKUIE
    static constexpr uint32_t IER_ERRIE = 0x8000;       // ERRIE
    static constexpr uint32_t IER_LECIE = 0x800;        // LECIE
    static constexpr uint32_t IER_BOFIE = 0x400;        // BOFIE
    static constexpr uint32_t IER_EPVIE = 0x200;        // EPVIE
    static constexpr uint32_t IER_EWGIE = 0x100;        // EWGIE
    static constexpr uint32_t IER_FOVIE1 = 0x40;        // FOVIE1
    static constexpr uint32_t IER_FFIE1 = 0x20;         // FFIE1
    static constexpr uint32_t IER_FMPIE1 = 0x10;        // FMPIE1
    static constexpr uint32_t IER_FOVIE0 = 0x8;         // FOVIE0
    static constexpr uint32_t IER_FFIE0 = 0x4;          // FFIE0
    static constexpr uint32_t IER_FMPIE0 = 0x2;         // FMPIE0
    static constexpr uint32_t IER_TMEIE = 0x1;          // TMEIE
    static const uint32_t IER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ESR_REC =                 // REC (8 bits), Read-only
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ESR_TEC =                 // TEC (8 bits), Read-only
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ESR_LEC =                 // LEC (3 bits), Read-write
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t ESR_BOFF = 0x4;           // BOFF, Read-only
    static constexpr uint32_t ESR_EPVF = 0x2;           // EPVF, Read-only
    static constexpr uint32_t ESR_EWGF = 0x1;           // EWGF, Read-only
    static const uint32_t ESR_RESET_VALUE = 0x0;

    static constexpr uint32_t BTR_SILM = 0x80000000;    // SILM
    static constexpr uint32_t BTR_LBKM = 0x40000000;    // LBKM
    template<uint32_t X>
    static constexpr uint32_t BTR_SJW =                 // SJW (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR_TS2 =                 // TS2 (3 bits)
        bit_field_t<20, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR_TS1 =                 // TS1 (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR_BRP =                 // BRP (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t BTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TI0R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TI0R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t TI0R_IDE = 0x4;            // IDE
    static constexpr uint32_t TI0R_RTR = 0x2;            // RTR
    static constexpr uint32_t TI0R_TXRQ = 0x1;           // TXRQ
    static const uint32_t TI0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDT0R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static constexpr uint32_t TDT0R_TGT = 0x100;          // TGT
    template<uint32_t X>
    static constexpr uint32_t TDT0R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t TDT0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDL0R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDL0R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDL0R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDL0R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TDL0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDH0R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDH0R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDH0R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDH0R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TDH0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TI1R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TI1R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t TI1R_IDE = 0x4;            // IDE
    static constexpr uint32_t TI1R_RTR = 0x2;            // RTR
    static constexpr uint32_t TI1R_TXRQ = 0x1;           // TXRQ
    static const uint32_t TI1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDT1R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static constexpr uint32_t TDT1R_TGT = 0x100;          // TGT
    template<uint32_t X>
    static constexpr uint32_t TDT1R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t TDT1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDL1R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDL1R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDL1R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDL1R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TDL1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDH1R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDH1R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDH1R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDH1R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TDH1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TI2R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TI2R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t TI2R_IDE = 0x4;            // IDE
    static constexpr uint32_t TI2R_RTR = 0x2;            // RTR
    static constexpr uint32_t TI2R_TXRQ = 0x1;           // TXRQ
    static const uint32_t TI2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDT2R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static constexpr uint32_t TDT2R_TGT = 0x100;          // TGT
    template<uint32_t X>
    static constexpr uint32_t TDT2R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t TDT2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDL2R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDL2R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDL2R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDL2R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TDL2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TDH2R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDH2R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDH2R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDH2R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TDH2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RI0R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RI0R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t RI0R_IDE = 0x4;            // IDE
    static constexpr uint32_t RI0R_RTR = 0x2;            // RTR
    static const uint32_t RI0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RDT0R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDT0R_FMI =                 // FMI (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDT0R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t RDT0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RDL0R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDL0R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDL0R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDL0R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RDL0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RDH0R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDH0R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDH0R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDH0R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RDH0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RI1R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RI1R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t RI1R_IDE = 0x4;            // IDE
    static constexpr uint32_t RI1R_RTR = 0x2;            // RTR
    static const uint32_t RI1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RDT1R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDT1R_FMI =                 // FMI (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDT1R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t RDT1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RDL1R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDL1R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDL1R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDL1R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RDL1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RDH1R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDH1R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDH1R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RDH1R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RDH1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FMR_CAN2SB =              // CAN2SB (6 bits)
        bit_field_t<8, 0x3f>::value<X>();
    static constexpr uint32_t FMR_FINIT = 0x1;          // FINIT
    static const uint32_t FMR_RESET_VALUE = 0x0;

    static constexpr uint32_t FM1R_FBM0 = 0x1;           // Filter mode
    static constexpr uint32_t FM1R_FBM1 = 0x2;           // Filter mode
    static constexpr uint32_t FM1R_FBM2 = 0x4;           // Filter mode
    static constexpr uint32_t FM1R_FBM3 = 0x8;           // Filter mode
    static constexpr uint32_t FM1R_FBM4 = 0x10;          // Filter mode
    static constexpr uint32_t FM1R_FBM5 = 0x20;          // Filter mode
    static constexpr uint32_t FM1R_FBM6 = 0x40;          // Filter mode
    static constexpr uint32_t FM1R_FBM7 = 0x80;          // Filter mode
    static constexpr uint32_t FM1R_FBM8 = 0x100;         // Filter mode
    static constexpr uint32_t FM1R_FBM9 = 0x200;         // Filter mode
    static constexpr uint32_t FM1R_FBM10 = 0x400;        // Filter mode
    static constexpr uint32_t FM1R_FBM11 = 0x800;        // Filter mode
    static constexpr uint32_t FM1R_FBM12 = 0x1000;       // Filter mode
    static constexpr uint32_t FM1R_FBM13 = 0x2000;       // Filter mode
    static constexpr uint32_t FM1R_FBM14 = 0x4000;       // Filter mode
    static constexpr uint32_t FM1R_FBM15 = 0x8000;       // Filter mode
    static constexpr uint32_t FM1R_FBM16 = 0x10000;      // Filter mode
    static constexpr uint32_t FM1R_FBM17 = 0x20000;      // Filter mode
    static constexpr uint32_t FM1R_FBM18 = 0x40000;      // Filter mode
    static constexpr uint32_t FM1R_FBM19 = 0x80000;      // Filter mode
    static constexpr uint32_t FM1R_FBM20 = 0x100000;     // Filter mode
    static constexpr uint32_t FM1R_FBM21 = 0x200000;     // Filter mode
    static constexpr uint32_t FM1R_FBM22 = 0x400000;     // Filter mode
    static constexpr uint32_t FM1R_FBM23 = 0x800000;     // Filter mode
    static constexpr uint32_t FM1R_FBM24 = 0x1000000;    // Filter mode
    static constexpr uint32_t FM1R_FBM25 = 0x2000000;    // Filter mode
    static constexpr uint32_t FM1R_FBM26 = 0x4000000;    // Filter mode
    static constexpr uint32_t FM1R_FBM27 = 0x8000000;    // Filter mode
    static const uint32_t FM1R_RESET_VALUE = 0x0;

    static constexpr uint32_t FS1R_FSC0 = 0x1;           // Filter scale configuration
    static constexpr uint32_t FS1R_FSC1 = 0x2;           // Filter scale configuration
    static constexpr uint32_t FS1R_FSC2 = 0x4;           // Filter scale configuration
    static constexpr uint32_t FS1R_FSC3 = 0x8;           // Filter scale configuration
    static constexpr uint32_t FS1R_FSC4 = 0x10;          // Filter scale configuration
    static constexpr uint32_t FS1R_FSC5 = 0x20;          // Filter scale configuration
    static constexpr uint32_t FS1R_FSC6 = 0x40;          // Filter scale configuration
    static constexpr uint32_t FS1R_FSC7 = 0x80;          // Filter scale configuration
    static constexpr uint32_t FS1R_FSC8 = 0x100;         // Filter scale configuration
    static constexpr uint32_t FS1R_FSC9 = 0x200;         // Filter scale configuration
    static constexpr uint32_t FS1R_FSC10 = 0x400;        // Filter scale configuration
    static constexpr uint32_t FS1R_FSC11 = 0x800;        // Filter scale configuration
    static constexpr uint32_t FS1R_FSC12 = 0x1000;       // Filter scale configuration
    static constexpr uint32_t FS1R_FSC13 = 0x2000;       // Filter scale configuration
    static constexpr uint32_t FS1R_FSC14 = 0x4000;       // Filter scale configuration
    static constexpr uint32_t FS1R_FSC15 = 0x8000;       // Filter scale configuration
    static constexpr uint32_t FS1R_FSC16 = 0x10000;      // Filter scale configuration
    static constexpr uint32_t FS1R_FSC17 = 0x20000;      // Filter scale configuration
    static constexpr uint32_t FS1R_FSC18 = 0x40000;      // Filter scale configuration
    static constexpr uint32_t FS1R_FSC19 = 0x80000;      // Filter scale configuration
    static constexpr uint32_t FS1R_FSC20 = 0x100000;     // Filter scale configuration
    static constexpr uint32_t FS1R_FSC21 = 0x200000;     // Filter scale configuration
    static constexpr uint32_t FS1R_FSC22 = 0x400000;     // Filter scale configuration
    static constexpr uint32_t FS1R_FSC23 = 0x800000;     // Filter scale configuration
    static constexpr uint32_t FS1R_FSC24 = 0x1000000;    // Filter scale configuration
    static constexpr uint32_t FS1R_FSC25 = 0x2000000;    // Filter scale configuration
    static constexpr uint32_t FS1R_FSC26 = 0x4000000;    // Filter scale configuration
    static constexpr uint32_t FS1R_FSC27 = 0x8000000;    // Filter scale configuration
    static const uint32_t FS1R_RESET_VALUE = 0x0;

    static constexpr uint32_t FFA1R_FFA0 = 0x1;           // Filter FIFO assignment for filter 0
    static constexpr uint32_t FFA1R_FFA1 = 0x2;           // Filter FIFO assignment for filter 1
    static constexpr uint32_t FFA1R_FFA2 = 0x4;           // Filter FIFO assignment for filter 2
    static constexpr uint32_t FFA1R_FFA3 = 0x8;           // Filter FIFO assignment for filter 3
    static constexpr uint32_t FFA1R_FFA4 = 0x10;          // Filter FIFO assignment for filter 4
    static constexpr uint32_t FFA1R_FFA5 = 0x20;          // Filter FIFO assignment for filter 5
    static constexpr uint32_t FFA1R_FFA6 = 0x40;          // Filter FIFO assignment for filter 6
    static constexpr uint32_t FFA1R_FFA7 = 0x80;          // Filter FIFO assignment for filter 7
    static constexpr uint32_t FFA1R_FFA8 = 0x100;         // Filter FIFO assignment for filter 8
    static constexpr uint32_t FFA1R_FFA9 = 0x200;         // Filter FIFO assignment for filter 9
    static constexpr uint32_t FFA1R_FFA10 = 0x400;        // Filter FIFO assignment for filter 10
    static constexpr uint32_t FFA1R_FFA11 = 0x800;        // Filter FIFO assignment for filter 11
    static constexpr uint32_t FFA1R_FFA12 = 0x1000;       // Filter FIFO assignment for filter 12
    static constexpr uint32_t FFA1R_FFA13 = 0x2000;       // Filter FIFO assignment for filter 13
    static constexpr uint32_t FFA1R_FFA14 = 0x4000;       // Filter FIFO assignment for filter 14
    static constexpr uint32_t FFA1R_FFA15 = 0x8000;       // Filter FIFO assignment for filter 15
    static constexpr uint32_t FFA1R_FFA16 = 0x10000;      // Filter FIFO assignment for filter 16
    static constexpr uint32_t FFA1R_FFA17 = 0x20000;      // Filter FIFO assignment for filter 17
    static constexpr uint32_t FFA1R_FFA18 = 0x40000;      // Filter FIFO assignment for filter 18
    static constexpr uint32_t FFA1R_FFA19 = 0x80000;      // Filter FIFO assignment for filter 19
    static constexpr uint32_t FFA1R_FFA20 = 0x100000;     // Filter FIFO assignment for filter 20
    static constexpr uint32_t FFA1R_FFA21 = 0x200000;     // Filter FIFO assignment for filter 21
    static constexpr uint32_t FFA1R_FFA22 = 0x400000;     // Filter FIFO assignment for filter 22
    static constexpr uint32_t FFA1R_FFA23 = 0x800000;     // Filter FIFO assignment for filter 23
    static constexpr uint32_t FFA1R_FFA24 = 0x1000000;    // Filter FIFO assignment for filter 24
    static constexpr uint32_t FFA1R_FFA25 = 0x2000000;    // Filter FIFO assignment for filter 25
    static constexpr uint32_t FFA1R_FFA26 = 0x4000000;    // Filter FIFO assignment for filter 26
    static constexpr uint32_t FFA1R_FFA27 = 0x8000000;    // Filter FIFO assignment for filter 27
    static const uint32_t FFA1R_RESET_VALUE = 0x0;

    static constexpr uint32_t FA1R_FACT0 = 0x1;          // Filter active
    static constexpr uint32_t FA1R_FACT1 = 0x2;          // Filter active
    static constexpr uint32_t FA1R_FACT2 = 0x4;          // Filter active
    static constexpr uint32_t FA1R_FACT3 = 0x8;          // Filter active
    static constexpr uint32_t FA1R_FACT4 = 0x10;         // Filter active
    static constexpr uint32_t FA1R_FACT5 = 0x20;         // Filter active
    static constexpr uint32_t FA1R_FACT6 = 0x40;         // Filter active
    static constexpr uint32_t FA1R_FACT7 = 0x80;         // Filter active
    static constexpr uint32_t FA1R_FACT8 = 0x100;        // Filter active
    static constexpr uint32_t FA1R_FACT9 = 0x200;        // Filter active
    static constexpr uint32_t FA1R_FACT10 = 0x400;       // Filter active
    static constexpr uint32_t FA1R_FACT11 = 0x800;       // Filter active
    static constexpr uint32_t FA1R_FACT12 = 0x1000;      // Filter active
    static constexpr uint32_t FA1R_FACT13 = 0x2000;      // Filter active
    static constexpr uint32_t FA1R_FACT14 = 0x4000;      // Filter active
    static constexpr uint32_t FA1R_FACT15 = 0x8000;      // Filter active
    static constexpr uint32_t FA1R_FACT16 = 0x10000;     // Filter active
    static constexpr uint32_t FA1R_FACT17 = 0x20000;     // Filter active
    static constexpr uint32_t FA1R_FACT18 = 0x40000;     // Filter active
    static constexpr uint32_t FA1R_FACT19 = 0x80000;     // Filter active
    static constexpr uint32_t FA1R_FACT20 = 0x100000;    // Filter active
    static constexpr uint32_t FA1R_FACT21 = 0x200000;    // Filter active
    static constexpr uint32_t FA1R_FACT22 = 0x400000;    // Filter active
    static constexpr uint32_t FA1R_FACT23 = 0x800000;    // Filter active
    static constexpr uint32_t FA1R_FACT24 = 0x1000000;   // Filter active
    static constexpr uint32_t FA1R_FACT25 = 0x2000000;   // Filter active
    static constexpr uint32_t FA1R_FACT26 = 0x4000000;   // Filter active
    static constexpr uint32_t FA1R_FACT27 = 0x8000000;   // Filter active
    static const uint32_t FA1R_RESET_VALUE = 0x0;

    static constexpr uint32_t F0R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F0R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F0R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F0R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F0R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F0R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F0R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F0R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F0R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F0R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F0R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F0R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F0R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F0R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F0R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F0R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F0R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F0R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F0R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F0R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F0R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F0R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F0R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F0R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F0R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F0R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F0R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F0R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F0R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F0R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F0R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F0R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F0R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F0R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F0R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F0R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F0R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F0R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F0R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F0R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F0R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F0R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F0R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F0R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F0R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F0R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F0R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F0R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F0R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F0R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F0R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F0R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F0R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F0R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F0R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F0R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F0R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F0R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F0R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F0R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F0R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F0R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F0R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F0R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F0R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F0R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F1R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F1R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F1R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F1R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F1R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F1R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F1R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F1R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F1R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F1R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F1R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F1R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F1R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F1R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F1R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F1R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F1R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F1R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F1R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F1R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F1R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F1R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F1R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F1R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F1R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F1R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F1R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F1R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F1R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F1R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F1R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F1R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F1R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F1R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F1R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F1R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F1R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F1R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F1R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F1R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F1R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F1R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F1R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F1R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F1R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F1R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F1R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F1R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F1R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F1R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F1R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F1R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F1R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F1R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F1R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F1R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F1R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F1R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F1R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F1R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F1R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F1R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F1R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F1R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F1R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F1R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F2R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F2R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F2R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F2R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F2R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F2R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F2R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F2R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F2R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F2R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F2R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F2R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F2R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F2R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F2R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F2R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F2R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F2R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F2R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F2R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F2R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F2R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F2R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F2R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F2R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F2R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F2R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F2R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F2R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F2R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F2R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F2R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F2R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F2R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F2R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F2R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F2R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F2R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F2R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F2R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F2R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F2R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F2R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F2R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F2R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F2R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F2R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F2R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F2R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F2R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F2R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F2R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F2R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F2R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F2R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F2R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F2R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F2R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F2R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F2R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F2R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F2R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F2R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F2R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F2R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F2R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F3R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F3R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F3R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F3R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F3R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F3R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F3R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F3R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F3R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F3R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F3R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F3R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F3R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F3R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F3R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F3R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F3R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F3R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F3R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F3R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F3R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F3R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F3R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F3R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F3R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F3R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F3R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F3R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F3R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F3R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F3R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F3R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F3R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F3R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F3R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F3R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F3R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F3R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F3R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F3R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F3R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F3R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F3R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F3R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F3R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F3R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F3R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F3R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F3R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F3R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F3R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F3R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F3R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F3R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F3R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F3R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F3R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F3R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F3R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F3R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F3R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F3R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F3R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F3R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F3R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F3R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F4R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F4R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F4R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F4R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F4R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F4R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F4R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F4R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F4R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F4R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F4R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F4R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F4R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F4R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F4R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F4R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F4R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F4R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F4R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F4R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F4R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F4R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F4R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F4R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F4R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F4R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F4R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F4R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F4R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F4R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F4R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F4R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F4R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F4R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F4R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F4R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F4R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F4R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F4R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F4R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F4R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F4R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F4R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F4R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F4R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F4R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F4R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F4R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F4R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F4R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F4R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F4R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F4R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F4R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F4R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F4R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F4R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F4R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F4R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F4R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F4R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F4R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F4R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F4R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F4R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F4R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F5R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F5R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F5R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F5R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F5R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F5R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F5R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F5R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F5R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F5R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F5R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F5R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F5R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F5R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F5R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F5R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F5R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F5R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F5R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F5R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F5R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F5R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F5R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F5R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F5R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F5R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F5R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F5R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F5R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F5R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F5R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F5R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F5R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F5R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F5R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F5R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F5R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F5R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F5R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F5R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F5R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F5R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F5R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F5R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F5R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F5R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F5R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F5R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F5R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F5R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F5R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F5R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F5R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F5R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F5R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F5R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F5R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F5R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F5R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F5R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F5R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F5R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F5R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F5R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F5R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F5R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F6R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F6R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F6R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F6R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F6R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F6R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F6R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F6R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F6R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F6R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F6R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F6R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F6R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F6R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F6R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F6R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F6R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F6R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F6R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F6R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F6R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F6R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F6R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F6R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F6R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F6R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F6R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F6R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F6R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F6R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F6R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F6R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F6R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F6R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F6R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F6R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F6R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F6R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F6R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F6R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F6R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F6R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F6R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F6R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F6R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F6R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F6R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F6R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F6R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F6R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F6R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F6R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F6R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F6R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F6R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F6R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F6R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F6R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F6R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F6R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F6R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F6R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F6R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F6R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F6R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F6R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F7R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F7R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F7R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F7R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F7R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F7R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F7R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F7R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F7R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F7R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F7R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F7R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F7R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F7R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F7R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F7R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F7R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F7R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F7R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F7R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F7R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F7R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F7R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F7R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F7R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F7R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F7R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F7R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F7R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F7R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F7R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F7R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F7R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F7R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F7R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F7R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F7R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F7R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F7R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F7R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F7R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F7R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F7R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F7R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F7R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F7R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F7R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F7R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F7R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F7R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F7R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F7R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F7R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F7R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F7R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F7R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F7R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F7R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F7R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F7R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F7R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F7R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F7R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F7R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F7R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F7R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F8R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F8R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F8R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F8R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F8R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F8R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F8R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F8R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F8R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F8R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F8R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F8R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F8R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F8R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F8R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F8R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F8R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F8R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F8R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F8R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F8R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F8R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F8R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F8R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F8R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F8R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F8R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F8R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F8R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F8R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F8R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F8R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F8R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F8R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F8R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F8R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F8R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F8R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F8R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F8R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F8R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F8R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F8R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F8R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F8R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F8R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F8R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F8R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F8R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F8R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F8R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F8R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F8R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F8R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F8R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F8R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F8R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F8R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F8R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F8R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F8R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F8R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F8R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F8R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F8R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F8R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F9R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F9R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F9R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F9R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F9R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F9R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F9R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F9R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F9R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F9R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F9R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F9R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F9R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F9R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F9R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F9R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F9R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F9R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F9R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F9R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F9R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F9R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F9R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F9R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F9R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F9R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F9R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F9R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F9R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F9R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F9R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F9R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F9R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F9R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F9R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F9R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F9R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F9R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F9R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F9R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F9R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F9R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F9R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F9R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F9R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F9R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F9R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F9R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F9R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F9R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F9R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F9R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F9R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F9R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F9R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F9R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F9R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F9R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F9R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F9R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F9R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F9R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F9R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F9R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F9R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F9R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F10R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F10R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F10R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F10R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F10R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F10R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F10R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F10R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F10R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F10R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F10R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F10R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F10R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F10R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F10R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F10R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F10R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F10R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F10R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F10R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F10R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F10R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F10R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F10R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F10R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F10R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F10R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F10R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F10R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F10R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F10R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F10R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F10R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F10R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F10R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F10R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F10R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F10R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F10R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F10R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F10R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F10R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F10R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F10R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F10R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F10R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F10R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F10R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F10R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F10R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F10R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F10R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F10R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F10R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F10R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F10R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F10R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F10R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F10R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F10R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F10R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F10R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F10R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F10R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F10R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F10R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F11R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F11R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F11R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F11R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F11R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F11R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F11R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F11R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F11R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F11R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F11R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F11R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F11R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F11R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F11R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F11R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F11R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F11R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F11R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F11R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F11R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F11R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F11R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F11R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F11R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F11R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F11R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F11R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F11R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F11R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F11R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F11R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F11R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F11R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F11R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F11R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F11R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F11R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F11R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F11R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F11R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F11R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F11R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F11R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F11R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F11R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F11R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F11R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F11R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F11R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F11R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F11R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F11R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F11R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F11R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F11R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F11R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F11R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F11R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F11R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F11R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F11R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F11R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F11R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F11R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F11R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F12R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F12R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F12R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F12R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F12R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F12R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F12R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F12R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F12R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F12R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F12R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F12R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F12R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F12R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F12R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F12R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F12R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F12R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F12R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F12R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F12R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F12R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F12R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F12R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F12R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F12R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F12R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F12R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F12R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F12R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F12R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F12R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F12R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F12R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F12R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F12R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F12R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F12R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F12R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F12R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F12R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F12R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F12R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F12R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F12R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F12R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F12R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F12R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F12R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F12R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F12R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F12R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F12R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F12R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F12R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F12R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F12R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F12R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F12R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F12R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F12R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F12R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F12R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F12R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F12R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F12R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F13R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F13R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F13R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F13R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F13R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F13R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F13R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F13R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F13R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F13R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F13R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F13R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F13R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F13R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F13R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F13R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F13R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F13R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F13R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F13R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F13R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F13R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F13R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F13R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F13R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F13R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F13R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F13R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F13R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F13R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F13R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F13R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F13R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F13R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F13R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F13R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F13R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F13R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F13R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F13R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F13R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F13R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F13R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F13R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F13R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F13R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F13R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F13R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F13R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F13R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F13R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F13R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F13R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F13R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F13R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F13R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F13R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F13R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F13R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F13R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F13R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F13R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F13R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F13R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F13R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F13R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F14R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F14R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F14R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F14R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F14R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F14R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F14R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F14R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F14R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F14R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F14R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F14R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F14R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F14R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F14R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F14R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F14R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F14R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F14R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F14R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F14R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F14R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F14R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F14R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F14R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F14R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F14R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F14R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F14R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F14R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F14R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F14R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F14R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F14R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F14R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F14R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F14R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F14R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F14R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F14R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F14R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F14R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F14R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F14R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F14R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F14R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F14R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F14R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F14R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F14R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F14R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F14R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F14R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F14R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F14R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F14R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F14R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F14R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F14R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F14R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F14R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F14R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F14R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F14R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F14R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F14R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F15R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F15R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F15R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F15R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F15R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F15R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F15R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F15R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F15R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F15R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F15R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F15R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F15R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F15R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F15R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F15R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F15R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F15R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F15R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F15R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F15R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F15R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F15R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F15R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F15R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F15R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F15R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F15R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F15R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F15R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F15R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F15R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F15R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F15R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F15R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F15R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F15R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F15R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F15R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F15R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F15R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F15R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F15R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F15R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F15R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F15R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F15R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F15R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F15R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F15R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F15R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F15R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F15R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F15R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F15R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F15R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F15R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F15R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F15R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F15R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F15R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F15R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F15R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F15R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F15R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F15R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F16R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F16R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F16R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F16R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F16R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F16R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F16R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F16R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F16R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F16R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F16R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F16R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F16R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F16R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F16R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F16R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F16R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F16R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F16R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F16R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F16R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F16R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F16R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F16R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F16R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F16R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F16R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F16R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F16R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F16R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F16R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F16R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F16R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F16R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F16R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F16R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F16R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F16R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F16R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F16R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F16R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F16R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F16R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F16R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F16R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F16R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F16R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F16R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F16R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F16R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F16R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F16R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F16R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F16R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F16R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F16R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F16R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F16R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F16R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F16R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F16R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F16R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F16R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F16R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F16R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F16R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F17R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F17R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F17R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F17R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F17R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F17R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F17R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F17R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F17R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F17R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F17R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F17R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F17R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F17R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F17R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F17R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F17R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F17R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F17R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F17R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F17R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F17R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F17R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F17R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F17R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F17R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F17R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F17R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F17R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F17R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F17R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F17R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F17R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F17R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F17R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F17R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F17R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F17R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F17R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F17R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F17R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F17R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F17R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F17R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F17R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F17R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F17R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F17R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F17R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F17R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F17R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F17R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F17R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F17R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F17R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F17R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F17R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F17R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F17R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F17R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F17R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F17R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F17R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F17R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F17R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F17R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F18R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F18R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F18R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F18R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F18R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F18R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F18R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F18R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F18R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F18R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F18R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F18R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F18R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F18R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F18R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F18R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F18R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F18R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F18R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F18R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F18R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F18R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F18R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F18R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F18R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F18R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F18R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F18R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F18R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F18R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F18R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F18R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F18R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F18R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F18R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F18R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F18R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F18R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F18R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F18R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F18R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F18R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F18R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F18R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F18R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F18R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F18R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F18R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F18R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F18R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F18R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F18R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F18R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F18R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F18R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F18R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F18R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F18R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F18R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F18R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F18R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F18R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F18R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F18R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F18R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F18R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F19R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F19R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F19R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F19R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F19R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F19R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F19R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F19R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F19R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F19R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F19R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F19R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F19R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F19R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F19R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F19R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F19R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F19R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F19R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F19R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F19R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F19R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F19R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F19R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F19R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F19R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F19R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F19R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F19R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F19R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F19R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F19R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F19R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F19R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F19R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F19R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F19R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F19R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F19R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F19R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F19R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F19R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F19R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F19R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F19R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F19R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F19R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F19R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F19R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F19R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F19R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F19R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F19R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F19R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F19R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F19R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F19R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F19R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F19R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F19R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F19R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F19R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F19R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F19R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F19R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F19R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F20R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F20R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F20R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F20R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F20R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F20R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F20R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F20R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F20R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F20R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F20R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F20R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F20R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F20R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F20R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F20R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F20R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F20R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F20R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F20R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F20R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F20R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F20R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F20R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F20R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F20R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F20R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F20R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F20R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F20R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F20R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F20R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F20R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F20R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F20R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F20R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F20R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F20R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F20R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F20R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F20R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F20R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F20R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F20R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F20R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F20R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F20R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F20R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F20R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F20R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F20R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F20R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F20R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F20R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F20R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F20R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F20R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F20R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F20R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F20R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F20R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F20R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F20R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F20R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F20R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F20R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F21R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F21R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F21R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F21R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F21R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F21R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F21R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F21R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F21R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F21R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F21R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F21R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F21R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F21R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F21R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F21R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F21R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F21R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F21R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F21R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F21R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F21R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F21R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F21R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F21R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F21R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F21R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F21R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F21R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F21R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F21R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F21R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F21R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F21R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F21R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F21R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F21R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F21R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F21R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F21R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F21R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F21R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F21R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F21R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F21R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F21R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F21R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F21R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F21R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F21R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F21R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F21R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F21R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F21R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F21R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F21R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F21R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F21R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F21R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F21R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F21R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F21R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F21R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F21R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F21R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F21R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F22R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F22R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F22R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F22R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F22R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F22R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F22R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F22R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F22R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F22R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F22R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F22R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F22R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F22R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F22R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F22R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F22R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F22R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F22R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F22R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F22R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F22R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F22R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F22R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F22R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F22R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F22R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F22R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F22R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F22R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F22R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F22R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F22R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F22R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F22R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F22R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F22R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F22R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F22R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F22R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F22R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F22R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F22R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F22R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F22R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F22R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F22R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F22R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F22R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F22R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F22R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F22R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F22R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F22R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F22R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F22R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F22R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F22R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F22R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F22R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F22R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F22R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F22R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F22R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F22R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F22R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F23R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F23R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F23R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F23R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F23R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F23R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F23R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F23R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F23R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F23R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F23R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F23R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F23R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F23R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F23R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F23R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F23R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F23R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F23R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F23R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F23R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F23R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F23R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F23R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F23R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F23R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F23R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F23R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F23R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F23R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F23R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F23R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F23R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F23R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F23R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F23R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F23R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F23R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F23R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F23R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F23R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F23R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F23R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F23R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F23R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F23R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F23R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F23R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F23R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F23R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F23R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F23R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F23R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F23R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F23R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F23R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F23R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F23R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F23R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F23R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F23R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F23R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F23R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F23R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F23R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F23R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F24R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F24R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F24R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F24R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F24R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F24R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F24R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F24R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F24R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F24R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F24R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F24R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F24R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F24R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F24R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F24R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F24R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F24R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F24R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F24R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F24R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F24R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F24R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F24R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F24R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F24R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F24R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F24R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F24R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F24R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F24R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F24R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F24R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F24R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F24R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F24R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F24R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F24R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F24R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F24R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F24R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F24R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F24R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F24R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F24R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F24R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F24R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F24R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F24R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F24R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F24R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F24R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F24R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F24R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F24R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F24R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F24R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F24R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F24R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F24R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F24R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F24R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F24R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F24R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F24R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F24R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F25R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F25R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F25R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F25R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F25R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F25R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F25R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F25R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F25R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F25R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F25R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F25R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F25R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F25R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F25R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F25R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F25R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F25R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F25R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F25R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F25R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F25R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F25R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F25R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F25R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F25R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F25R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F25R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F25R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F25R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F25R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F25R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F25R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F25R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F25R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F25R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F25R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F25R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F25R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F25R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F25R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F25R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F25R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F25R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F25R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F25R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F25R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F25R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F25R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F25R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F25R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F25R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F25R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F25R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F25R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F25R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F25R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F25R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F25R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F25R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F25R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F25R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F25R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F25R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F25R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F25R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F26R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F26R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F26R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F26R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F26R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F26R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F26R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F26R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F26R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F26R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F26R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F26R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F26R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F26R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F26R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F26R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F26R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F26R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F26R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F26R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F26R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F26R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F26R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F26R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F26R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F26R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F26R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F26R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F26R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F26R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F26R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F26R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F26R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F26R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F26R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F26R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F26R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F26R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F26R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F26R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F26R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F26R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F26R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F26R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F26R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F26R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F26R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F26R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F26R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F26R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F26R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F26R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F26R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F26R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F26R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F26R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F26R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F26R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F26R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F26R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F26R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F26R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F26R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F26R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F26R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F26R2_RESET_VALUE = 0x0;

    static constexpr uint32_t F27R1_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F27R1_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F27R1_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F27R1_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F27R1_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F27R1_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F27R1_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F27R1_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F27R1_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F27R1_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F27R1_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F27R1_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F27R1_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F27R1_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F27R1_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F27R1_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F27R1_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F27R1_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F27R1_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F27R1_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F27R1_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F27R1_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F27R1_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F27R1_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F27R1_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F27R1_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F27R1_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F27R1_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F27R1_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F27R1_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F27R1_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F27R1_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F27R1_RESET_VALUE = 0x0;

    static constexpr uint32_t F27R2_FB0 = 0x1;            // Filter bits
    static constexpr uint32_t F27R2_FB1 = 0x2;            // Filter bits
    static constexpr uint32_t F27R2_FB2 = 0x4;            // Filter bits
    static constexpr uint32_t F27R2_FB3 = 0x8;            // Filter bits
    static constexpr uint32_t F27R2_FB4 = 0x10;           // Filter bits
    static constexpr uint32_t F27R2_FB5 = 0x20;           // Filter bits
    static constexpr uint32_t F27R2_FB6 = 0x40;           // Filter bits
    static constexpr uint32_t F27R2_FB7 = 0x80;           // Filter bits
    static constexpr uint32_t F27R2_FB8 = 0x100;          // Filter bits
    static constexpr uint32_t F27R2_FB9 = 0x200;          // Filter bits
    static constexpr uint32_t F27R2_FB10 = 0x400;         // Filter bits
    static constexpr uint32_t F27R2_FB11 = 0x800;         // Filter bits
    static constexpr uint32_t F27R2_FB12 = 0x1000;        // Filter bits
    static constexpr uint32_t F27R2_FB13 = 0x2000;        // Filter bits
    static constexpr uint32_t F27R2_FB14 = 0x4000;        // Filter bits
    static constexpr uint32_t F27R2_FB15 = 0x8000;        // Filter bits
    static constexpr uint32_t F27R2_FB16 = 0x10000;       // Filter bits
    static constexpr uint32_t F27R2_FB17 = 0x20000;       // Filter bits
    static constexpr uint32_t F27R2_FB18 = 0x40000;       // Filter bits
    static constexpr uint32_t F27R2_FB19 = 0x80000;       // Filter bits
    static constexpr uint32_t F27R2_FB20 = 0x100000;      // Filter bits
    static constexpr uint32_t F27R2_FB21 = 0x200000;      // Filter bits
    static constexpr uint32_t F27R2_FB22 = 0x400000;      // Filter bits
    static constexpr uint32_t F27R2_FB23 = 0x800000;      // Filter bits
    static constexpr uint32_t F27R2_FB24 = 0x1000000;     // Filter bits
    static constexpr uint32_t F27R2_FB25 = 0x2000000;     // Filter bits
    static constexpr uint32_t F27R2_FB26 = 0x4000000;     // Filter bits
    static constexpr uint32_t F27R2_FB27 = 0x8000000;     // Filter bits
    static constexpr uint32_t F27R2_FB28 = 0x10000000;    // Filter bits
    static constexpr uint32_t F27R2_FB29 = 0x20000000;    // Filter bits
    static constexpr uint32_t F27R2_FB30 = 0x40000000;    // Filter bits
    static constexpr uint32_t F27R2_FB31 = 0x80000000;    // Filter bits
    static const uint32_t F27R2_RESET_VALUE = 0x0;
};

static can_t& CAN = *reinterpret_cast<can_t*>(0x40006400);

#define HAVE_PERIPHERAL_CAN


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

    template<uint32_t X>
    static constexpr uint32_t CPUID_Revision =            // Revision number (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CPUID_PartNo =              // Part number of the processor (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CPUID_Constant =            // Reads as 0xF (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CPUID_Variant =             // Variant number (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CPUID_Implementer =         // Implementer code (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t CPUID_RESET_VALUE = 0x410fc241;

    template<uint32_t X>
    static constexpr uint32_t ICSR_VECTACTIVE =          // Active vector (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ICSR_VECTPENDING =         // Pending vector (6 bits)
        bit_field_t<12, 0x3f>::value<X>();
    static constexpr uint32_t ICSR_ISRPENDING = 0x400000;// Interrupt pending flag
    static constexpr uint32_t ICSR_PENDSTCLR = 0x2000000;// SysTick exception clear-pending bit
    static constexpr uint32_t ICSR_PENDSTSET = 0x4000000;// SysTick exception set-pending bit
    static constexpr uint32_t ICSR_PENDSVCLR = 0x8000000;// PendSV clear-pending bit
    static constexpr uint32_t ICSR_PENDSVSET = 0x10000000;// PendSV set-pending bit
    static constexpr uint32_t ICSR_NMIPENDSET = 0x80000000;// NMI set-pending bit.
    static const uint32_t ICSR_RESET_VALUE = 0x0;

    static constexpr uint32_t AIRCR_VECTCLRACTIVE = 0x2;  // VECTCLRACTIVE
    static constexpr uint32_t AIRCR_SYSRESETREQ = 0x4;    // SYSRESETREQ
    static constexpr uint32_t AIRCR_ENDIANESS = 0x8000;   // ENDIANESS
    template<uint32_t X>
    static constexpr uint32_t AIRCR_VECTKEYSTAT =         // Register key (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t AIRCR_RESET_VALUE = 0x0;

    static constexpr uint32_t SCR_SLEEPONEXIT = 0x2;    // SLEEPONEXIT
    static constexpr uint32_t SCR_SLEEPDEEP = 0x4;      // SLEEPDEEP
    static constexpr uint32_t SCR_SEVEONPEND = 0x10;    // Send Event on Pending bit
    static const uint32_t SCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR_UNALIGN__TRP = 0x8;   // UNALIGN_ TRP
    static constexpr uint32_t CCR_STKALIGN = 0x200;     // STKALIGN
    static const uint32_t CCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SHPR2_PRI_11 =              // Priority of system handler 11 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t SHPR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SHPR3_PRI_14 =              // Priority of system handler 14 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SHPR3_PRI_15 =              // Priority of system handler 15 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t SHPR3_RESET_VALUE = 0x0;
};

static scb_t& SCB = *reinterpret_cast<scb_t*>(0xe000ed00);

#define HAVE_PERIPHERAL_SCB


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

    static constexpr uint32_t CSR_ENABLE = 0x1;         // Counter enable
    static constexpr uint32_t CSR_TICKINT = 0x2;        // SysTick exception request enable
    static constexpr uint32_t CSR_CLKSOURCE = 0x4;      // Clock source selection
    static constexpr uint32_t CSR_COUNTFLAG = 0x10000;  // COUNTFLAG
    static const uint32_t CSR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RVR_RELOAD =              // RELOAD value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();
    static const uint32_t RVR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CVR_CURRENT =             // Current counter value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();
    static const uint32_t CVR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CALIB_TENMS =               // Calibration value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();
    static constexpr uint32_t CALIB_SKEW = 0x40000000;    // SKEW flag: Indicates whether the TENMS value is exact
    static constexpr uint32_t CALIB_NOREF = 0x80000000;   // NOREF flag. Reads as zero
    static const uint32_t CALIB_RESET_VALUE = 0x0;
};

static stk_t& STK = *reinterpret_cast<stk_t*>(0xe000e010);

#define HAVE_PERIPHERAL_STK


template<typename PERIPHERAL> struct peripheral_traits {};

template<> struct peripheral_traits<crc_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_CRCEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_CRCEN; }
};

template<> struct peripheral_traits<gpiof_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_IOPFEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_IOPFEN; }
    static void reset() { RCC.AHBRSTR |= rcc_t::AHBRSTR_IOPFRST; }
};

template<> struct peripheral_traits<gpiod_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_IOPDEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_IOPDEN; }
    static void reset() { RCC.AHBRSTR |= rcc_t::AHBRSTR_IOPDRST; }
};

template<> struct peripheral_traits<gpioc_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_IOPCEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_IOPCEN; }
    static void reset() { RCC.AHBRSTR |= rcc_t::AHBRSTR_IOPCRST; }
};

template<> struct peripheral_traits<gpiob_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_IOPBEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_IOPBEN; }
    static void reset() { RCC.AHBRSTR |= rcc_t::AHBRSTR_IOPBRST; }
};

template<> struct peripheral_traits<gpioa_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_IOPAEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_IOPAEN; }
    static void reset() { RCC.AHBRSTR |= rcc_t::AHBRSTR_IOPARST; }
};

template<> struct peripheral_traits<spi1_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_SPI1EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_SPI1EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_SPI1RST; }
};

template<> struct peripheral_traits<spi2_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_SPI2EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_SPI2EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_SPI2RST; }
};

template<> struct peripheral_traits<dac_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_DACEN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_DACEN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_DACRST; }
};

template<> struct peripheral_traits<pwr_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_PWREN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_PWREN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_PWRRST; }
};

template<> struct peripheral_traits<i2c1_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_I2C1EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_I2C1EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_I2C1RST; }
};

template<> struct peripheral_traits<i2c2_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_I2C2EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_I2C2EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_I2C2RST; }
};

template<> struct peripheral_traits<wwdg_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_WWDGEN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_WWDGEN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_WWDGRST; }
};

template<> struct peripheral_traits<tim1_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM1EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_TIM1EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_TIM1RST; }
};

template<> struct peripheral_traits<tim2_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM2EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_TIM2EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_TIM2RST; }
};

template<> struct peripheral_traits<tim3_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM3EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_TIM3EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_TIM3RST; }
};

template<> struct peripheral_traits<tim14_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM14EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_TIM14EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_TIM14RST; }
};

template<> struct peripheral_traits<tim6_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM6EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_TIM6EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_TIM6RST; }
};

template<> struct peripheral_traits<tim7_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM7EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_TIM7EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_TIM7RST; }
};

template<> struct peripheral_traits<dma1_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_DMA1EN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_DMA1EN; }
};

template<> struct peripheral_traits<syscfg_comp_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_SYSCFGEN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_SYSCFGEN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_SYSCFGRST; }
};

template<> struct peripheral_traits<adc_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_ADCEN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_ADCEN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_ADCRST; }
};

template<> struct peripheral_traits<usart1_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_USART1EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_USART1EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_USART1RST; }
};

template<> struct peripheral_traits<usart2_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_USART2EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_USART2EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_USART2RST; }
};

template<> struct peripheral_traits<usart3_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_USART3EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_USART3EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_USART3RST; }
};

template<> struct peripheral_traits<usart4_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_USART4EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_USART4EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_USART4RST; }
};

template<> struct peripheral_traits<tim15_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM15EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_TIM15EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_TIM15RST; }
};

template<> struct peripheral_traits<tim16_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM16EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_TIM16EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_TIM16RST; }
};

template<> struct peripheral_traits<tim17_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM17EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_TIM17EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_TIM17RST; }
};

template<> struct peripheral_traits<tsc_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_TSCEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_TSCEN; }
    static void reset() { RCC.AHBRSTR |= rcc_t::AHBRSTR_TSCRST; }
};

template<> struct peripheral_traits<cec_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_CECEN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_CECEN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_CECRST; }
};

template<> struct peripheral_traits<dbgmcu_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_DBGMCUEN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_DBGMCUEN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_DBGMCURST; }
};

template<> struct peripheral_traits<usb_t>
{
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_USBRST; }
};

template<> struct peripheral_traits<crs_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_CRSEN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_CRSEN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_CRSRST; }
};

template<> struct peripheral_traits<can_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_CANEN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_CANEN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_CANRST; }
};

}

struct interrupt
{
    static inline void enable() { __asm volatile ("cpsie i"); }
    static inline void disable() { __asm volatile ("cpsid i"); }

    enum interrupt_t
    { RESET = -15
    , NMI = -14
    , HARDFAULT = -13
    , MEMMANAGE = -12
    , BUSFAULT = -11
    , USAGEFAULT = -10
    , SVCALL = -5
    , DEBUG = -4
    , PENDSV = -2
    , SYSTICK = -1
    , WWDG = 0
    , PVD = 1
    , RTC = 2
    , FLASH = 3
    , RCC_CRS = 4
    , EXTI0_1 = 5
    , EXTI2_3 = 6
    , EXTI4_15 = 7
    , TSC = 8
    , DMA1_CH1 = 9
    , ADC_COMP = 12
    , TIM1_BRK_UP_TRG_COM = 13
    , TIM1_CC = 14
    , TIM2 = 15
    , TIM3 = 16
    , TIM6_DAC = 17
    , TIM7 = 18
    , TIM14 = 19
    , TIM15 = 20
    , TIM16 = 21
    , TIM17 = 22
    , I2C1 = 23
    , I2C2 = 24
    , SPI1 = 25
    , SPI2 = 26
    , USART1 = 27
    , USART2 = 28
    , USART3_4 = 29
    , CEC_CAN = 30
    , USB = 31
    };
};
