#pragma once

#include <stdint.h>

////
//
//    STM32F103
//
//       schema-version : 1.1
//       vendor         : 
//       series         : 
//       device-version : 1.1
//       address-unit   : 8 bits
//       device-width   : 32
//       device-size    : 32
//
////

namespace stm32f103
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
//    Flexible static memory controller
//
////

struct fsmc_t
{
    volatile uint32_t    BCR1;                 // [Read-write] SRAM/NOR-Flash chip-select control register 1
    volatile uint32_t    BTR1;                 // [Read-write] SRAM/NOR-Flash chip-select timing register 1
    volatile uint32_t    BCR2;                 // [Read-write] SRAM/NOR-Flash chip-select control register 2
    volatile uint32_t    BTR2;                 // [Read-write] SRAM/NOR-Flash chip-select timing register 2
    volatile uint32_t    BCR3;                 // [Read-write] SRAM/NOR-Flash chip-select control register 3
    volatile uint32_t    BTR3;                 // [Read-write] SRAM/NOR-Flash chip-select timing register 3
    volatile uint32_t    BCR4;                 // [Read-write] SRAM/NOR-Flash chip-select control register 4
    volatile uint32_t    BTR4;                 // [Read-write] SRAM/NOR-Flash chip-select timing register 4
    reserved_t<16>       _0;
    volatile uint32_t    PCR2;                 // [Read-write] PC Card/NAND Flash control register 2
    volatile uint32_t    SR2;                  // FIFO status and interrupt register 2
    volatile uint32_t    PMEM2;                // [Read-write] Common memory space timing register 2
    volatile uint32_t    PATT2;                // [Read-write] Attribute memory space timing register 2
    reserved_t<1>        _1;
    volatile uint32_t    ECCR2;                // [Read-only] ECC result register 2
    reserved_t<2>        _2;
    volatile uint32_t    PCR3;                 // [Read-write] PC Card/NAND Flash control register 3
    volatile uint32_t    SR3;                  // FIFO status and interrupt register 3
    volatile uint32_t    PMEM3;                // [Read-write] Common memory space timing register 3
    volatile uint32_t    PATT3;                // [Read-write] Attribute memory space timing register 3
    reserved_t<1>        _3;
    volatile uint32_t    ECCR3;                // [Read-only] ECC result register 3
    reserved_t<2>        _4;
    volatile uint32_t    PCR4;                 // [Read-write] PC Card/NAND Flash control register 4
    volatile uint32_t    SR4;                  // FIFO status and interrupt register 4
    volatile uint32_t    PMEM4;                // [Read-write] Common memory space timing register 4
    volatile uint32_t    PATT4;                // [Read-write] Attribute memory space timing register 4
    volatile uint32_t    PIO4;                 // [Read-write] I/O space timing register 4
    reserved_t<20>       _5;
    volatile uint32_t    BWTR1;                // [Read-write] SRAM/NOR-Flash write timing registers 1
    reserved_t<1>        _6;
    volatile uint32_t    BWTR2;                // [Read-write] SRAM/NOR-Flash write timing registers 2
    reserved_t<1>        _7;
    volatile uint32_t    BWTR3;                // [Read-write] SRAM/NOR-Flash write timing registers 3
    reserved_t<1>        _8;
    volatile uint32_t    BWTR4;                // [Read-write] SRAM/NOR-Flash write timing registers 4

    static constexpr uint32_t BCR1_CBURSTRW = 0x80000;   // CBURSTRW
    static constexpr uint32_t BCR1_ASYNCWAIT = 0x8000;   // ASYNCWAIT
    static constexpr uint32_t BCR1_EXTMOD = 0x4000;      // EXTMOD
    static constexpr uint32_t BCR1_WAITEN = 0x2000;      // WAITEN
    static constexpr uint32_t BCR1_WREN = 0x1000;        // WREN
    static constexpr uint32_t BCR1_WAITCFG = 0x800;      // WAITCFG
    static constexpr uint32_t BCR1_WAITPOL = 0x200;      // WAITPOL
    static constexpr uint32_t BCR1_BURSTEN = 0x100;      // BURSTEN
    static constexpr uint32_t BCR1_FACCEN = 0x40;        // FACCEN
    template<uint32_t X>
    static constexpr uint32_t BCR1_MWID =                // MWID (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BCR1_MTYP =                // MTYP (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    static constexpr uint32_t BCR1_MUXEN = 0x2;          // MUXEN
    static constexpr uint32_t BCR1_MBKEN = 0x1;          // MBKEN
    static const uint32_t BCR1_RESET_VALUE = 0x30d0;

    template<uint32_t X>
    static constexpr uint32_t BTR1_ACCMOD =              // ACCMOD (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR1_DATLAT =              // DATLAT (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR1_CLKDIV =              // CLKDIV (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR1_BUSTURN =             // BUSTURN (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR1_DATAST =              // DATAST (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR1_ADDHLD =              // ADDHLD (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR1_ADDSET =              // ADDSET (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BTR1_RESET_VALUE = 0xffffffff;

    static constexpr uint32_t BCR2_CBURSTRW = 0x80000;   // CBURSTRW
    static constexpr uint32_t BCR2_ASYNCWAIT = 0x8000;   // ASYNCWAIT
    static constexpr uint32_t BCR2_EXTMOD = 0x4000;      // EXTMOD
    static constexpr uint32_t BCR2_WAITEN = 0x2000;      // WAITEN
    static constexpr uint32_t BCR2_WREN = 0x1000;        // WREN
    static constexpr uint32_t BCR2_WAITCFG = 0x800;      // WAITCFG
    static constexpr uint32_t BCR2_WRAPMOD = 0x400;      // WRAPMOD
    static constexpr uint32_t BCR2_WAITPOL = 0x200;      // WAITPOL
    static constexpr uint32_t BCR2_BURSTEN = 0x100;      // BURSTEN
    static constexpr uint32_t BCR2_FACCEN = 0x40;        // FACCEN
    template<uint32_t X>
    static constexpr uint32_t BCR2_MWID =                // MWID (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BCR2_MTYP =                // MTYP (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    static constexpr uint32_t BCR2_MUXEN = 0x2;          // MUXEN
    static constexpr uint32_t BCR2_MBKEN = 0x1;          // MBKEN
    static const uint32_t BCR2_RESET_VALUE = 0x30d0;

    template<uint32_t X>
    static constexpr uint32_t BTR2_ACCMOD =              // ACCMOD (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR2_DATLAT =              // DATLAT (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR2_CLKDIV =              // CLKDIV (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR2_BUSTURN =             // BUSTURN (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR2_DATAST =              // DATAST (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR2_ADDHLD =              // ADDHLD (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR2_ADDSET =              // ADDSET (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BTR2_RESET_VALUE = 0xffffffff;

    static constexpr uint32_t BCR3_CBURSTRW = 0x80000;   // CBURSTRW
    static constexpr uint32_t BCR3_ASYNCWAIT = 0x8000;   // ASYNCWAIT
    static constexpr uint32_t BCR3_EXTMOD = 0x4000;      // EXTMOD
    static constexpr uint32_t BCR3_WAITEN = 0x2000;      // WAITEN
    static constexpr uint32_t BCR3_WREN = 0x1000;        // WREN
    static constexpr uint32_t BCR3_WAITCFG = 0x800;      // WAITCFG
    static constexpr uint32_t BCR3_WRAPMOD = 0x400;      // WRAPMOD
    static constexpr uint32_t BCR3_WAITPOL = 0x200;      // WAITPOL
    static constexpr uint32_t BCR3_BURSTEN = 0x100;      // BURSTEN
    static constexpr uint32_t BCR3_FACCEN = 0x40;        // FACCEN
    template<uint32_t X>
    static constexpr uint32_t BCR3_MWID =                // MWID (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BCR3_MTYP =                // MTYP (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    static constexpr uint32_t BCR3_MUXEN = 0x2;          // MUXEN
    static constexpr uint32_t BCR3_MBKEN = 0x1;          // MBKEN
    static const uint32_t BCR3_RESET_VALUE = 0x30d0;

    template<uint32_t X>
    static constexpr uint32_t BTR3_ACCMOD =              // ACCMOD (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR3_DATLAT =              // DATLAT (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR3_CLKDIV =              // CLKDIV (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR3_BUSTURN =             // BUSTURN (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR3_DATAST =              // DATAST (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR3_ADDHLD =              // ADDHLD (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR3_ADDSET =              // ADDSET (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BTR3_RESET_VALUE = 0xffffffff;

    static constexpr uint32_t BCR4_CBURSTRW = 0x80000;   // CBURSTRW
    static constexpr uint32_t BCR4_ASYNCWAIT = 0x8000;   // ASYNCWAIT
    static constexpr uint32_t BCR4_EXTMOD = 0x4000;      // EXTMOD
    static constexpr uint32_t BCR4_WAITEN = 0x2000;      // WAITEN
    static constexpr uint32_t BCR4_WREN = 0x1000;        // WREN
    static constexpr uint32_t BCR4_WAITCFG = 0x800;      // WAITCFG
    static constexpr uint32_t BCR4_WRAPMOD = 0x400;      // WRAPMOD
    static constexpr uint32_t BCR4_WAITPOL = 0x200;      // WAITPOL
    static constexpr uint32_t BCR4_BURSTEN = 0x100;      // BURSTEN
    static constexpr uint32_t BCR4_FACCEN = 0x40;        // FACCEN
    template<uint32_t X>
    static constexpr uint32_t BCR4_MWID =                // MWID (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BCR4_MTYP =                // MTYP (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    static constexpr uint32_t BCR4_MUXEN = 0x2;          // MUXEN
    static constexpr uint32_t BCR4_MBKEN = 0x1;          // MBKEN
    static const uint32_t BCR4_RESET_VALUE = 0x30d0;

    template<uint32_t X>
    static constexpr uint32_t BTR4_ACCMOD =              // ACCMOD (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR4_DATLAT =              // DATLAT (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR4_CLKDIV =              // CLKDIV (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR4_BUSTURN =             // BUSTURN (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR4_DATAST =              // DATAST (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR4_ADDHLD =              // ADDHLD (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BTR4_ADDSET =              // ADDSET (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BTR4_RESET_VALUE = 0xffffffff;

    template<uint32_t X>
    static constexpr uint32_t PCR2_ECCPS =               // ECCPS (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PCR2_TAR =                 // TAR (4 bits)
        bit_field_t<13, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PCR2_TCLR =                // TCLR (4 bits)
        bit_field_t<9, 0xf>::value<X>();
    static constexpr uint32_t PCR2_ECCEN = 0x40;         // ECCEN
    template<uint32_t X>
    static constexpr uint32_t PCR2_PWID =                // PWID (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t PCR2_PTYP = 0x8;           // PTYP
    static constexpr uint32_t PCR2_PBKEN = 0x4;          // PBKEN
    static constexpr uint32_t PCR2_PWAITEN = 0x2;        // PWAITEN
    static const uint32_t PCR2_RESET_VALUE = 0x18;

    static constexpr uint32_t SR2_FEMPT = 0x40;         // FEMPT, Read-only
    static constexpr uint32_t SR2_IFEN = 0x20;          // IFEN, Read-write
    static constexpr uint32_t SR2_ILEN = 0x10;          // ILEN, Read-write
    static constexpr uint32_t SR2_IREN = 0x8;           // IREN, Read-write
    static constexpr uint32_t SR2_IFS = 0x4;            // IFS, Read-write
    static constexpr uint32_t SR2_ILS = 0x2;            // ILS, Read-write
    static constexpr uint32_t SR2_IRS = 0x1;            // IRS, Read-write
    static const uint32_t SR2_RESET_VALUE = 0x40;

    template<uint32_t X>
    static constexpr uint32_t PMEM2_MEMHIZx =             // MEMHIZx (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PMEM2_MEMHOLDx =            // MEMHOLDx (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PMEM2_MEMWAITx =            // MEMWAITx (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PMEM2_MEMSETx =             // MEMSETx (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PMEM2_RESET_VALUE = 0xfcfcfcfc;

    template<uint32_t X>
    static constexpr uint32_t PATT2_ATTHIZx =             // Attribute memory x databus HiZ time (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PATT2_ATTHOLDx =            // Attribute memory x hold time (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PATT2_ATTWAITx =            // Attribute memory x wait time (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PATT2_ATTSETx =             // Attribute memory x setup time (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PATT2_RESET_VALUE = 0xfcfcfcfc;


    static const uint32_t ECCR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PCR3_ECCPS =               // ECCPS (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PCR3_TAR =                 // TAR (4 bits)
        bit_field_t<13, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PCR3_TCLR =                // TCLR (4 bits)
        bit_field_t<9, 0xf>::value<X>();
    static constexpr uint32_t PCR3_ECCEN = 0x40;         // ECCEN
    template<uint32_t X>
    static constexpr uint32_t PCR3_PWID =                // PWID (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t PCR3_PTYP = 0x8;           // PTYP
    static constexpr uint32_t PCR3_PBKEN = 0x4;          // PBKEN
    static constexpr uint32_t PCR3_PWAITEN = 0x2;        // PWAITEN
    static const uint32_t PCR3_RESET_VALUE = 0x18;

    static constexpr uint32_t SR3_FEMPT = 0x40;         // FEMPT, Read-only
    static constexpr uint32_t SR3_IFEN = 0x20;          // IFEN, Read-write
    static constexpr uint32_t SR3_ILEN = 0x10;          // ILEN, Read-write
    static constexpr uint32_t SR3_IREN = 0x8;           // IREN, Read-write
    static constexpr uint32_t SR3_IFS = 0x4;            // IFS, Read-write
    static constexpr uint32_t SR3_ILS = 0x2;            // ILS, Read-write
    static constexpr uint32_t SR3_IRS = 0x1;            // IRS, Read-write
    static const uint32_t SR3_RESET_VALUE = 0x40;

    template<uint32_t X>
    static constexpr uint32_t PMEM3_MEMHIZx =             // MEMHIZx (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PMEM3_MEMHOLDx =            // MEMHOLDx (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PMEM3_MEMWAITx =            // MEMWAITx (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PMEM3_MEMSETx =             // MEMSETx (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PMEM3_RESET_VALUE = 0xfcfcfcfc;

    template<uint32_t X>
    static constexpr uint32_t PATT3_ATTHIZx =             // ATTHIZx (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PATT3_ATTHOLDx =            // ATTHOLDx (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PATT3_ATTWAITx =            // ATTWAITx (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PATT3_ATTSETx =             // ATTSETx (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PATT3_RESET_VALUE = 0xfcfcfcfc;


    static const uint32_t ECCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PCR4_ECCPS =               // ECCPS (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PCR4_TAR =                 // TAR (4 bits)
        bit_field_t<13, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PCR4_TCLR =                // TCLR (4 bits)
        bit_field_t<9, 0xf>::value<X>();
    static constexpr uint32_t PCR4_ECCEN = 0x40;         // ECCEN
    template<uint32_t X>
    static constexpr uint32_t PCR4_PWID =                // PWID (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t PCR4_PTYP = 0x8;           // PTYP
    static constexpr uint32_t PCR4_PBKEN = 0x4;          // PBKEN
    static constexpr uint32_t PCR4_PWAITEN = 0x2;        // PWAITEN
    static const uint32_t PCR4_RESET_VALUE = 0x18;

    static constexpr uint32_t SR4_FEMPT = 0x40;         // FEMPT, Read-only
    static constexpr uint32_t SR4_IFEN = 0x20;          // IFEN, Read-write
    static constexpr uint32_t SR4_ILEN = 0x10;          // ILEN, Read-write
    static constexpr uint32_t SR4_IREN = 0x8;           // IREN, Read-write
    static constexpr uint32_t SR4_IFS = 0x4;            // IFS, Read-write
    static constexpr uint32_t SR4_ILS = 0x2;            // ILS, Read-write
    static constexpr uint32_t SR4_IRS = 0x1;            // IRS, Read-write
    static const uint32_t SR4_RESET_VALUE = 0x40;

    template<uint32_t X>
    static constexpr uint32_t PMEM4_MEMHIZx =             // MEMHIZx (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PMEM4_MEMHOLDx =            // MEMHOLDx (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PMEM4_MEMWAITx =            // MEMWAITx (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PMEM4_MEMSETx =             // MEMSETx (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PMEM4_RESET_VALUE = 0xfcfcfcfc;

    template<uint32_t X>
    static constexpr uint32_t PATT4_ATTHIZx =             // ATTHIZx (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PATT4_ATTHOLDx =            // ATTHOLDx (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PATT4_ATTWAITx =            // ATTWAITx (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PATT4_ATTSETx =             // ATTSETx (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PATT4_RESET_VALUE = 0xfcfcfcfc;

    template<uint32_t X>
    static constexpr uint32_t PIO4_IOHIZx =              // IOHIZx (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PIO4_IOHOLDx =             // IOHOLDx (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PIO4_IOWAITx =             // IOWAITx (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PIO4_IOSETx =              // IOSETx (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PIO4_RESET_VALUE = 0xfcfcfcfc;

    template<uint32_t X>
    static constexpr uint32_t BWTR1_ACCMOD =              // ACCMOD (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR1_DATLAT =              // DATLAT (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR1_CLKDIV =              // CLKDIV (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR1_DATAST =              // DATAST (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR1_ADDHLD =              // ADDHLD (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR1_ADDSET =              // ADDSET (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BWTR1_RESET_VALUE = 0xfffffff;

    template<uint32_t X>
    static constexpr uint32_t BWTR2_ACCMOD =              // ACCMOD (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR2_DATLAT =              // DATLAT (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR2_CLKDIV =              // CLKDIV (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR2_DATAST =              // DATAST (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR2_ADDHLD =              // ADDHLD (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR2_ADDSET =              // ADDSET (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BWTR2_RESET_VALUE = 0xfffffff;

    template<uint32_t X>
    static constexpr uint32_t BWTR3_ACCMOD =              // ACCMOD (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR3_DATLAT =              // DATLAT (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR3_CLKDIV =              // CLKDIV (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR3_DATAST =              // DATAST (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR3_ADDHLD =              // ADDHLD (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR3_ADDSET =              // ADDSET (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BWTR3_RESET_VALUE = 0xfffffff;

    template<uint32_t X>
    static constexpr uint32_t BWTR4_ACCMOD =              // ACCMOD (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR4_DATLAT =              // DATLAT (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR4_CLKDIV =              // CLKDIV (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR4_DATAST =              // DATAST (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR4_ADDHLD =              // ADDHLD (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BWTR4_ADDSET =              // ADDSET (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BWTR4_RESET_VALUE = 0xfffffff;

    static constexpr uint8_t FSMC = 48; // FSMC global interrupt
};

static fsmc_t& FSMC = *reinterpret_cast<fsmc_t*>(0xa0000000);

#define HAVE_PERIPHERAL_FSMC


////
//
//    Power control
//
////

struct pwr_t
{
    volatile uint32_t    CR;                   // [Read-write] Power control register (PWR_CR)
    volatile uint32_t    CSR;                  // Power control register (PWR_CR)

    static constexpr uint32_t CR_LPDS = 0x1;           // Low Power Deep Sleep
    static constexpr uint32_t CR_PDDS = 0x2;           // Power Down Deep Sleep
    static constexpr uint32_t CR_CWUF = 0x4;           // Clear Wake-up Flag
    static constexpr uint32_t CR_CSBF = 0x8;           // Clear STANDBY Flag
    static constexpr uint32_t CR_PVDE = 0x10;          // Power Voltage Detector Enable
    template<uint32_t X>
    static constexpr uint32_t CR_PLS =                 // PVD Level Selection (3 bits)
        bit_field_t<5, 0x7>::value<X>();
    static constexpr uint32_t CR_DBP = 0x100;          // Disable Backup Domain write protection
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t CSR_WUF = 0x1;            // Wake-Up Flag, Read-only
    static constexpr uint32_t CSR_SBF = 0x2;            // STANDBY Flag, Read-only
    static constexpr uint32_t CSR_PVDO = 0x4;           // PVD Output, Read-only
    static constexpr uint32_t CSR_EWUP = 0x100;         // Enable WKUP pin, Read-write
    static const uint32_t CSR_RESET_VALUE = 0x0;

    static constexpr uint8_t PVD = 1; // PVD through EXTI line detection interrupt
};

static pwr_t& PWR = *reinterpret_cast<pwr_t*>(0x40007000);

#define HAVE_PERIPHERAL_PWR


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
    static constexpr uint32_t CFGR_PPRE1 =               // APB Low speed prescaler (APB1) (3 bits), Read-write
        bit_field_t<8, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_PPRE2 =               // APB High speed prescaler (APB2) (3 bits), Read-write
        bit_field_t<11, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_ADCPRE =              // ADC prescaler (2 bits), Read-write
        bit_field_t<14, 0x3>::value<X>();
    static constexpr uint32_t CFGR_PLLSRC = 0x10000;     // PLL entry clock source, Read-write
    static constexpr uint32_t CFGR_PLLXTPRE = 0x20000;   // HSE divider for PLL entry, Read-write
    template<uint32_t X>
    static constexpr uint32_t CFGR_PLLMUL =              // PLL Multiplication Factor (4 bits), Read-write
        bit_field_t<18, 0xf>::value<X>();
    static constexpr uint32_t CFGR_OTGFSPRE = 0x400000;  // USB OTG FS prescaler, Read-write
    template<uint32_t X>
    static constexpr uint32_t CFGR_MCO =                 // Microcontroller clock output (3 bits), Read-write
        bit_field_t<24, 0x7>::value<X>();
    static const uint32_t CFGR_RESET_VALUE = 0x0;

    static constexpr uint32_t CIR_LSIRDYF = 0x1;        // LSI Ready Interrupt flag, Read-only
    static constexpr uint32_t CIR_LSERDYF = 0x2;        // LSE Ready Interrupt flag, Read-only
    static constexpr uint32_t CIR_HSIRDYF = 0x4;        // HSI Ready Interrupt flag, Read-only
    static constexpr uint32_t CIR_HSERDYF = 0x8;        // HSE Ready Interrupt flag, Read-only
    static constexpr uint32_t CIR_PLLRDYF = 0x10;       // PLL Ready Interrupt flag, Read-only
    static constexpr uint32_t CIR_CSSF = 0x80;          // Clock Security System Interrupt flag, Read-only
    static constexpr uint32_t CIR_LSIRDYIE = 0x100;     // LSI Ready Interrupt Enable, Read-write
    static constexpr uint32_t CIR_LSERDYIE = 0x200;     // LSE Ready Interrupt Enable, Read-write
    static constexpr uint32_t CIR_HSIRDYIE = 0x400;     // HSI Ready Interrupt Enable, Read-write
    static constexpr uint32_t CIR_HSERDYIE = 0x800;     // HSE Ready Interrupt Enable, Read-write
    static constexpr uint32_t CIR_PLLRDYIE = 0x1000;    // PLL Ready Interrupt Enable, Read-write
    static constexpr uint32_t CIR_LSIRDYC = 0x10000;    // LSI Ready Interrupt Clear, Write-only
    static constexpr uint32_t CIR_LSERDYC = 0x20000;    // LSE Ready Interrupt Clear, Write-only
    static constexpr uint32_t CIR_HSIRDYC = 0x40000;    // HSI Ready Interrupt Clear, Write-only
    static constexpr uint32_t CIR_HSERDYC = 0x80000;    // HSE Ready Interrupt Clear, Write-only
    static constexpr uint32_t CIR_PLLRDYC = 0x100000;   // PLL Ready Interrupt Clear, Write-only
    static constexpr uint32_t CIR_CSSC = 0x800000;      // Clock security system interrupt clear, Write-only
    static const uint32_t CIR_RESET_VALUE = 0x0;

    static constexpr uint32_t APB2RSTR_AFIORST = 0x1;        // Alternate function I/O reset
    static constexpr uint32_t APB2RSTR_IOPARST = 0x4;        // IO port A reset
    static constexpr uint32_t APB2RSTR_IOPBRST = 0x8;        // IO port B reset
    static constexpr uint32_t APB2RSTR_IOPCRST = 0x10;       // IO port C reset
    static constexpr uint32_t APB2RSTR_IOPDRST = 0x20;       // IO port D reset
    static constexpr uint32_t APB2RSTR_IOPERST = 0x40;       // IO port E reset
    static constexpr uint32_t APB2RSTR_IOPFRST = 0x80;       // IO port F reset
    static constexpr uint32_t APB2RSTR_IOPGRST = 0x100;      // IO port G reset
    static constexpr uint32_t APB2RSTR_ADC1RST = 0x200;      // ADC 1 interface reset
    static constexpr uint32_t APB2RSTR_ADC2RST = 0x400;      // ADC 2 interface reset
    static constexpr uint32_t APB2RSTR_TIM1RST = 0x800;      // TIM1 timer reset
    static constexpr uint32_t APB2RSTR_SPI1RST = 0x1000;     // SPI 1 reset
    static constexpr uint32_t APB2RSTR_TIM8RST = 0x2000;     // TIM8 timer reset
    static constexpr uint32_t APB2RSTR_USART1RST = 0x4000;   // USART1 reset
    static constexpr uint32_t APB2RSTR_ADC3RST = 0x8000;     // ADC 3 interface reset
    static constexpr uint32_t APB2RSTR_TIM9RST = 0x80000;    // TIM9 timer reset
    static constexpr uint32_t APB2RSTR_TIM10RST = 0x100000;  // TIM10 timer reset
    static constexpr uint32_t APB2RSTR_TIM11RST = 0x200000;  // TIM11 timer reset
    static const uint32_t APB2RSTR_RESET_VALUE = 0x0;

    static constexpr uint32_t APB1RSTR_TIM2RST = 0x1;        // Timer 2 reset
    static constexpr uint32_t APB1RSTR_TIM3RST = 0x2;        // Timer 3 reset
    static constexpr uint32_t APB1RSTR_TIM4RST = 0x4;        // Timer 4 reset
    static constexpr uint32_t APB1RSTR_TIM5RST = 0x8;        // Timer 5 reset
    static constexpr uint32_t APB1RSTR_TIM6RST = 0x10;       // Timer 6 reset
    static constexpr uint32_t APB1RSTR_TIM7RST = 0x20;       // Timer 7 reset
    static constexpr uint32_t APB1RSTR_TIM12RST = 0x40;      // Timer 12 reset
    static constexpr uint32_t APB1RSTR_TIM13RST = 0x80;      // Timer 13 reset
    static constexpr uint32_t APB1RSTR_TIM14RST = 0x100;     // Timer 14 reset
    static constexpr uint32_t APB1RSTR_WWDGRST = 0x800;      // Window watchdog reset
    static constexpr uint32_t APB1RSTR_SPI2RST = 0x4000;     // SPI2 reset
    static constexpr uint32_t APB1RSTR_SPI3RST = 0x8000;     // SPI3 reset
    static constexpr uint32_t APB1RSTR_USART2RST = 0x20000;  // USART 2 reset
    static constexpr uint32_t APB1RSTR_USART3RST = 0x40000;  // USART 3 reset
    static constexpr uint32_t APB1RSTR_UART4RST = 0x80000;   // UART 4 reset
    static constexpr uint32_t APB1RSTR_UART5RST = 0x100000;  // UART 5 reset
    static constexpr uint32_t APB1RSTR_I2C1RST = 0x200000;   // I2C1 reset
    static constexpr uint32_t APB1RSTR_I2C2RST = 0x400000;   // I2C2 reset
    static constexpr uint32_t APB1RSTR_USBRST = 0x800000;    // USB reset
    static constexpr uint32_t APB1RSTR_CANRST = 0x2000000;   // CAN reset
    static constexpr uint32_t APB1RSTR_BKPRST = 0x8000000;   // Backup interface reset
    static constexpr uint32_t APB1RSTR_PWRRST = 0x10000000;  // Power interface reset
    static constexpr uint32_t APB1RSTR_DACRST = 0x20000000;  // DAC interface reset
    static const uint32_t APB1RSTR_RESET_VALUE = 0x0;

    static constexpr uint32_t AHBENR_DMA1EN = 0x1;         // DMA1 clock enable
    static constexpr uint32_t AHBENR_DMA2EN = 0x2;         // DMA2 clock enable
    static constexpr uint32_t AHBENR_SRAMEN = 0x4;         // SRAM interface clock enable
    static constexpr uint32_t AHBENR_FLITFEN = 0x10;       // FLITF clock enable
    static constexpr uint32_t AHBENR_CRCEN = 0x40;         // CRC clock enable
    static constexpr uint32_t AHBENR_FSMCEN = 0x100;       // FSMC clock enable
    static constexpr uint32_t AHBENR_SDIOEN = 0x400;       // SDIO clock enable
    static const uint32_t AHBENR_RESET_VALUE = 0x14;

    static constexpr uint32_t APB2ENR_AFIOEN = 0x1;         // Alternate function I/O clock enable
    static constexpr uint32_t APB2ENR_IOPAEN = 0x4;         // I/O port A clock enable
    static constexpr uint32_t APB2ENR_IOPBEN = 0x8;         // I/O port B clock enable
    static constexpr uint32_t APB2ENR_IOPCEN = 0x10;        // I/O port C clock enable
    static constexpr uint32_t APB2ENR_IOPDEN = 0x20;        // I/O port D clock enable
    static constexpr uint32_t APB2ENR_IOPEEN = 0x40;        // I/O port E clock enable
    static constexpr uint32_t APB2ENR_IOPFEN = 0x80;        // I/O port F clock enable
    static constexpr uint32_t APB2ENR_IOPGEN = 0x100;       // I/O port G clock enable
    static constexpr uint32_t APB2ENR_ADC1EN = 0x200;       // ADC 1 interface clock enable
    static constexpr uint32_t APB2ENR_ADC2EN = 0x400;       // ADC 2 interface clock enable
    static constexpr uint32_t APB2ENR_TIM1EN = 0x800;       // TIM1 Timer clock enable
    static constexpr uint32_t APB2ENR_SPI1EN = 0x1000;      // SPI 1 clock enable
    static constexpr uint32_t APB2ENR_TIM8EN = 0x2000;      // TIM8 Timer clock enable
    static constexpr uint32_t APB2ENR_USART1EN = 0x4000;    // USART1 clock enable
    static constexpr uint32_t APB2ENR_ADC3EN = 0x8000;      // ADC3 interface clock enable
    static constexpr uint32_t APB2ENR_TIM9EN = 0x80000;     // TIM9 Timer clock enable
    static constexpr uint32_t APB2ENR_TIM10EN = 0x100000;   // TIM10 Timer clock enable
    static constexpr uint32_t APB2ENR_TIM11EN = 0x200000;   // TIM11 Timer clock enable
    static const uint32_t APB2ENR_RESET_VALUE = 0x0;

    static constexpr uint32_t APB1ENR_TIM2EN = 0x1;         // Timer 2 clock enable
    static constexpr uint32_t APB1ENR_TIM3EN = 0x2;         // Timer 3 clock enable
    static constexpr uint32_t APB1ENR_TIM4EN = 0x4;         // Timer 4 clock enable
    static constexpr uint32_t APB1ENR_TIM5EN = 0x8;         // Timer 5 clock enable
    static constexpr uint32_t APB1ENR_TIM6EN = 0x10;        // Timer 6 clock enable
    static constexpr uint32_t APB1ENR_TIM7EN = 0x20;        // Timer 7 clock enable
    static constexpr uint32_t APB1ENR_TIM12EN = 0x40;       // Timer 12 clock enable
    static constexpr uint32_t APB1ENR_TIM13EN = 0x80;       // Timer 13 clock enable
    static constexpr uint32_t APB1ENR_TIM14EN = 0x100;      // Timer 14 clock enable
    static constexpr uint32_t APB1ENR_WWDGEN = 0x800;       // Window watchdog clock enable
    static constexpr uint32_t APB1ENR_SPI2EN = 0x4000;      // SPI 2 clock enable
    static constexpr uint32_t APB1ENR_SPI3EN = 0x8000;      // SPI 3 clock enable
    static constexpr uint32_t APB1ENR_USART2EN = 0x20000;   // USART 2 clock enable
    static constexpr uint32_t APB1ENR_USART3EN = 0x40000;   // USART 3 clock enable
    static constexpr uint32_t APB1ENR_UART4EN = 0x80000;    // UART 4 clock enable
    static constexpr uint32_t APB1ENR_UART5EN = 0x100000;   // UART 5 clock enable
    static constexpr uint32_t APB1ENR_I2C1EN = 0x200000;    // I2C 1 clock enable
    static constexpr uint32_t APB1ENR_I2C2EN = 0x400000;    // I2C 2 clock enable
    static constexpr uint32_t APB1ENR_USBEN = 0x800000;     // USB clock enable
    static constexpr uint32_t APB1ENR_CANEN = 0x2000000;    // CAN clock enable
    static constexpr uint32_t APB1ENR_BKPEN = 0x8000000;    // Backup interface clock enable
    static constexpr uint32_t APB1ENR_PWREN = 0x10000000;   // Power interface clock enable
    static constexpr uint32_t APB1ENR_DACEN = 0x20000000;   // DAC interface clock enable
    static const uint32_t APB1ENR_RESET_VALUE = 0x0;

    static constexpr uint32_t BDCR_LSEON = 0x1;          // External Low Speed oscillator enable, Read-write
    static constexpr uint32_t BDCR_LSERDY = 0x2;         // External Low Speed oscillator ready, Read-only
    static constexpr uint32_t BDCR_LSEBYP = 0x4;         // External Low Speed oscillator bypass, Read-write
    template<uint32_t X>
    static constexpr uint32_t BDCR_RTCSEL =              // RTC clock source selection (2 bits), Read-write
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t BDCR_RTCEN = 0x8000;       // RTC clock enable, Read-write
    static constexpr uint32_t BDCR_BDRST = 0x10000;      // Backup domain software reset, Read-write
    static const uint32_t BDCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CSR_LSION = 0x1;          // Internal low speed oscillator enable, Read-write
    static constexpr uint32_t CSR_LSIRDY = 0x2;         // Internal low speed oscillator ready, Read-only
    static constexpr uint32_t CSR_RMVF = 0x1000000;     // Remove reset flag, Read-write
    static constexpr uint32_t CSR_PINRSTF = 0x4000000;  // PIN reset flag, Read-write
    static constexpr uint32_t CSR_PORRSTF = 0x8000000;  // POR/PDR reset flag, Read-write
    static constexpr uint32_t CSR_SFTRSTF = 0x10000000; // Software reset flag, Read-write
    static constexpr uint32_t CSR_IWDGRSTF = 0x20000000;// Independent watchdog reset flag, Read-write
    static constexpr uint32_t CSR_WWDGRSTF = 0x40000000;// Window watchdog reset flag, Read-write
    static constexpr uint32_t CSR_LPWRRSTF = 0x80000000;// Low-power reset flag, Read-write
    static const uint32_t CSR_RESET_VALUE = 0xc000000;

    static constexpr uint8_t RCC = 5; // RCC global interrupt
};

static rcc_t& RCC = *reinterpret_cast<rcc_t*>(0x40021000);

#define HAVE_PERIPHERAL_RCC


////
//
//    General purpose I/O
//
////

struct gpioa_t
{
    volatile uint32_t    CRL;                  // [Read-write] Port configuration register low (GPIOn_CRL)
    volatile uint32_t    CRH;                  // [Read-write] Port configuration register high (GPIOn_CRL)
    volatile uint32_t    IDR;                  // [Read-only] Port input data register (GPIOn_IDR)
    volatile uint32_t    ODR;                  // [Read-write] Port output data register (GPIOn_ODR)
    volatile uint32_t    BSRR;                 // [Write-only] Port bit set/reset register (GPIOn_BSRR)
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register (GPIOn_BRR)
    volatile uint32_t    LCKR;                 // [Read-write] Port configuration lock register

    template<uint32_t X>
    static constexpr uint32_t CRL_MODE0 =               // Port n.0 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF0 =                // Port n.0 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE1 =               // Port n.1 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF1 =                // Port n.1 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE2 =               // Port n.2 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF2 =                // Port n.2 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE3 =               // Port n.3 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF3 =                // Port n.3 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE4 =               // Port n.4 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF4 =                // Port n.4 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE5 =               // Port n.5 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF5 =                // Port n.5 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE6 =               // Port n.6 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF6 =                // Port n.6 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE7 =               // Port n.7 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF7 =                // Port n.7 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRL_RESET_VALUE = 0x44444444;

    template<uint32_t X>
    static constexpr uint32_t CRH_MODE8 =               // Port n.8 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF8 =                // Port n.8 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE9 =               // Port n.9 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF9 =                // Port n.9 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE10 =              // Port n.10 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF10 =               // Port n.10 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE11 =              // Port n.11 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF11 =               // Port n.11 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE12 =              // Port n.12 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF12 =               // Port n.12 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE13 =              // Port n.13 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF13 =               // Port n.13 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE14 =              // Port n.14 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF14 =               // Port n.14 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE15 =              // Port n.15 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF15 =               // Port n.15 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRH_RESET_VALUE = 0x44444444;

    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data
    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data
    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BS0 = 0x1;            // Set bit 0
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Set bit 1
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Set bit 1
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Set bit 3
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Set bit 4
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Set bit 5
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Set bit 6
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Set bit 7
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Set bit 8
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Set bit 9
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Set bit 10
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Set bit 11
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Set bit 12
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Set bit 13
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Set bit 14
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Set bit 15
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Reset bit 0
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Reset bit 1
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Reset bit 2
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Reset bit 3
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Reset bit 4
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Reset bit 5
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Reset bit 6
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Reset bit 7
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Reset bit 8
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Reset bit 9
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Reset bit 10
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Reset bit 11
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Reset bit 12
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Reset bit 13
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Reset bit 14
    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Reset bit 15
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Reset bit 0
    static constexpr uint32_t BRR_BR1 = 0x2;            // Reset bit 1
    static constexpr uint32_t BRR_BR2 = 0x4;            // Reset bit 1
    static constexpr uint32_t BRR_BR3 = 0x8;            // Reset bit 3
    static constexpr uint32_t BRR_BR4 = 0x10;           // Reset bit 4
    static constexpr uint32_t BRR_BR5 = 0x20;           // Reset bit 5
    static constexpr uint32_t BRR_BR6 = 0x40;           // Reset bit 6
    static constexpr uint32_t BRR_BR7 = 0x80;           // Reset bit 7
    static constexpr uint32_t BRR_BR8 = 0x100;          // Reset bit 8
    static constexpr uint32_t BRR_BR9 = 0x200;          // Reset bit 9
    static constexpr uint32_t BRR_BR10 = 0x400;         // Reset bit 10
    static constexpr uint32_t BRR_BR11 = 0x800;         // Reset bit 11
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Reset bit 12
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Reset bit 13
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Reset bit 14
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Reset bit 15
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port A Lock bit 0
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port A Lock bit 1
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port A Lock bit 2
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port A Lock bit 3
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port A Lock bit 4
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port A Lock bit 5
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port A Lock bit 6
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port A Lock bit 7
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port A Lock bit 8
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port A Lock bit 9
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port A Lock bit 10
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port A Lock bit 11
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port A Lock bit 12
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port A Lock bit 13
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port A Lock bit 14
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port A Lock bit 15
    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Lock key
    static const uint32_t LCKR_RESET_VALUE = 0x0;
};

static gpioa_t& GPIOA = *reinterpret_cast<gpioa_t*>(0x40010800);

#define HAVE_PERIPHERAL_GPIOA


////
//
//    General purpose I/O
//
////

struct gpiob_t
{
    volatile uint32_t    CRL;                  // [Read-write] Port configuration register low (GPIOn_CRL)
    volatile uint32_t    CRH;                  // [Read-write] Port configuration register high (GPIOn_CRL)
    volatile uint32_t    IDR;                  // [Read-only] Port input data register (GPIOn_IDR)
    volatile uint32_t    ODR;                  // [Read-write] Port output data register (GPIOn_ODR)
    volatile uint32_t    BSRR;                 // [Write-only] Port bit set/reset register (GPIOn_BSRR)
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register (GPIOn_BRR)
    volatile uint32_t    LCKR;                 // [Read-write] Port configuration lock register

    template<uint32_t X>
    static constexpr uint32_t CRL_MODE0 =               // Port n.0 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF0 =                // Port n.0 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE1 =               // Port n.1 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF1 =                // Port n.1 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE2 =               // Port n.2 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF2 =                // Port n.2 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE3 =               // Port n.3 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF3 =                // Port n.3 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE4 =               // Port n.4 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF4 =                // Port n.4 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE5 =               // Port n.5 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF5 =                // Port n.5 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE6 =               // Port n.6 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF6 =                // Port n.6 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE7 =               // Port n.7 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF7 =                // Port n.7 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRL_RESET_VALUE = 0x44444444;

    template<uint32_t X>
    static constexpr uint32_t CRH_MODE8 =               // Port n.8 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF8 =                // Port n.8 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE9 =               // Port n.9 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF9 =                // Port n.9 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE10 =              // Port n.10 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF10 =               // Port n.10 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE11 =              // Port n.11 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF11 =               // Port n.11 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE12 =              // Port n.12 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF12 =               // Port n.12 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE13 =              // Port n.13 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF13 =               // Port n.13 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE14 =              // Port n.14 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF14 =               // Port n.14 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE15 =              // Port n.15 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF15 =               // Port n.15 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRH_RESET_VALUE = 0x44444444;

    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data
    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data
    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BS0 = 0x1;            // Set bit 0
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Set bit 1
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Set bit 1
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Set bit 3
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Set bit 4
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Set bit 5
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Set bit 6
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Set bit 7
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Set bit 8
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Set bit 9
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Set bit 10
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Set bit 11
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Set bit 12
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Set bit 13
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Set bit 14
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Set bit 15
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Reset bit 0
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Reset bit 1
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Reset bit 2
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Reset bit 3
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Reset bit 4
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Reset bit 5
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Reset bit 6
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Reset bit 7
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Reset bit 8
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Reset bit 9
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Reset bit 10
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Reset bit 11
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Reset bit 12
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Reset bit 13
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Reset bit 14
    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Reset bit 15
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Reset bit 0
    static constexpr uint32_t BRR_BR1 = 0x2;            // Reset bit 1
    static constexpr uint32_t BRR_BR2 = 0x4;            // Reset bit 1
    static constexpr uint32_t BRR_BR3 = 0x8;            // Reset bit 3
    static constexpr uint32_t BRR_BR4 = 0x10;           // Reset bit 4
    static constexpr uint32_t BRR_BR5 = 0x20;           // Reset bit 5
    static constexpr uint32_t BRR_BR6 = 0x40;           // Reset bit 6
    static constexpr uint32_t BRR_BR7 = 0x80;           // Reset bit 7
    static constexpr uint32_t BRR_BR8 = 0x100;          // Reset bit 8
    static constexpr uint32_t BRR_BR9 = 0x200;          // Reset bit 9
    static constexpr uint32_t BRR_BR10 = 0x400;         // Reset bit 10
    static constexpr uint32_t BRR_BR11 = 0x800;         // Reset bit 11
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Reset bit 12
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Reset bit 13
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Reset bit 14
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Reset bit 15
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port A Lock bit 0
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port A Lock bit 1
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port A Lock bit 2
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port A Lock bit 3
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port A Lock bit 4
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port A Lock bit 5
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port A Lock bit 6
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port A Lock bit 7
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port A Lock bit 8
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port A Lock bit 9
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port A Lock bit 10
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port A Lock bit 11
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port A Lock bit 12
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port A Lock bit 13
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port A Lock bit 14
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port A Lock bit 15
    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Lock key
    static const uint32_t LCKR_RESET_VALUE = 0x0;
};

static gpiob_t& GPIOB = *reinterpret_cast<gpiob_t*>(0x40010c00);

#define HAVE_PERIPHERAL_GPIOB


////
//
//    General purpose I/O
//
////

struct gpioc_t
{
    volatile uint32_t    CRL;                  // [Read-write] Port configuration register low (GPIOn_CRL)
    volatile uint32_t    CRH;                  // [Read-write] Port configuration register high (GPIOn_CRL)
    volatile uint32_t    IDR;                  // [Read-only] Port input data register (GPIOn_IDR)
    volatile uint32_t    ODR;                  // [Read-write] Port output data register (GPIOn_ODR)
    volatile uint32_t    BSRR;                 // [Write-only] Port bit set/reset register (GPIOn_BSRR)
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register (GPIOn_BRR)
    volatile uint32_t    LCKR;                 // [Read-write] Port configuration lock register

    template<uint32_t X>
    static constexpr uint32_t CRL_MODE0 =               // Port n.0 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF0 =                // Port n.0 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE1 =               // Port n.1 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF1 =                // Port n.1 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE2 =               // Port n.2 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF2 =                // Port n.2 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE3 =               // Port n.3 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF3 =                // Port n.3 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE4 =               // Port n.4 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF4 =                // Port n.4 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE5 =               // Port n.5 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF5 =                // Port n.5 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE6 =               // Port n.6 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF6 =                // Port n.6 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE7 =               // Port n.7 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF7 =                // Port n.7 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRL_RESET_VALUE = 0x44444444;

    template<uint32_t X>
    static constexpr uint32_t CRH_MODE8 =               // Port n.8 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF8 =                // Port n.8 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE9 =               // Port n.9 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF9 =                // Port n.9 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE10 =              // Port n.10 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF10 =               // Port n.10 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE11 =              // Port n.11 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF11 =               // Port n.11 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE12 =              // Port n.12 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF12 =               // Port n.12 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE13 =              // Port n.13 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF13 =               // Port n.13 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE14 =              // Port n.14 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF14 =               // Port n.14 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE15 =              // Port n.15 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF15 =               // Port n.15 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRH_RESET_VALUE = 0x44444444;

    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data
    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data
    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BS0 = 0x1;            // Set bit 0
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Set bit 1
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Set bit 1
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Set bit 3
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Set bit 4
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Set bit 5
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Set bit 6
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Set bit 7
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Set bit 8
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Set bit 9
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Set bit 10
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Set bit 11
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Set bit 12
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Set bit 13
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Set bit 14
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Set bit 15
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Reset bit 0
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Reset bit 1
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Reset bit 2
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Reset bit 3
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Reset bit 4
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Reset bit 5
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Reset bit 6
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Reset bit 7
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Reset bit 8
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Reset bit 9
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Reset bit 10
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Reset bit 11
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Reset bit 12
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Reset bit 13
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Reset bit 14
    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Reset bit 15
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Reset bit 0
    static constexpr uint32_t BRR_BR1 = 0x2;            // Reset bit 1
    static constexpr uint32_t BRR_BR2 = 0x4;            // Reset bit 1
    static constexpr uint32_t BRR_BR3 = 0x8;            // Reset bit 3
    static constexpr uint32_t BRR_BR4 = 0x10;           // Reset bit 4
    static constexpr uint32_t BRR_BR5 = 0x20;           // Reset bit 5
    static constexpr uint32_t BRR_BR6 = 0x40;           // Reset bit 6
    static constexpr uint32_t BRR_BR7 = 0x80;           // Reset bit 7
    static constexpr uint32_t BRR_BR8 = 0x100;          // Reset bit 8
    static constexpr uint32_t BRR_BR9 = 0x200;          // Reset bit 9
    static constexpr uint32_t BRR_BR10 = 0x400;         // Reset bit 10
    static constexpr uint32_t BRR_BR11 = 0x800;         // Reset bit 11
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Reset bit 12
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Reset bit 13
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Reset bit 14
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Reset bit 15
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port A Lock bit 0
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port A Lock bit 1
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port A Lock bit 2
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port A Lock bit 3
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port A Lock bit 4
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port A Lock bit 5
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port A Lock bit 6
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port A Lock bit 7
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port A Lock bit 8
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port A Lock bit 9
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port A Lock bit 10
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port A Lock bit 11
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port A Lock bit 12
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port A Lock bit 13
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port A Lock bit 14
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port A Lock bit 15
    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Lock key
    static const uint32_t LCKR_RESET_VALUE = 0x0;
};

static gpioc_t& GPIOC = *reinterpret_cast<gpioc_t*>(0x40011000);

#define HAVE_PERIPHERAL_GPIOC


////
//
//    General purpose I/O
//
////

struct gpiod_t
{
    volatile uint32_t    CRL;                  // [Read-write] Port configuration register low (GPIOn_CRL)
    volatile uint32_t    CRH;                  // [Read-write] Port configuration register high (GPIOn_CRL)
    volatile uint32_t    IDR;                  // [Read-only] Port input data register (GPIOn_IDR)
    volatile uint32_t    ODR;                  // [Read-write] Port output data register (GPIOn_ODR)
    volatile uint32_t    BSRR;                 // [Write-only] Port bit set/reset register (GPIOn_BSRR)
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register (GPIOn_BRR)
    volatile uint32_t    LCKR;                 // [Read-write] Port configuration lock register

    template<uint32_t X>
    static constexpr uint32_t CRL_MODE0 =               // Port n.0 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF0 =                // Port n.0 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE1 =               // Port n.1 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF1 =                // Port n.1 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE2 =               // Port n.2 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF2 =                // Port n.2 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE3 =               // Port n.3 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF3 =                // Port n.3 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE4 =               // Port n.4 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF4 =                // Port n.4 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE5 =               // Port n.5 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF5 =                // Port n.5 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE6 =               // Port n.6 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF6 =                // Port n.6 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE7 =               // Port n.7 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF7 =                // Port n.7 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRL_RESET_VALUE = 0x44444444;

    template<uint32_t X>
    static constexpr uint32_t CRH_MODE8 =               // Port n.8 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF8 =                // Port n.8 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE9 =               // Port n.9 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF9 =                // Port n.9 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE10 =              // Port n.10 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF10 =               // Port n.10 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE11 =              // Port n.11 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF11 =               // Port n.11 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE12 =              // Port n.12 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF12 =               // Port n.12 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE13 =              // Port n.13 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF13 =               // Port n.13 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE14 =              // Port n.14 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF14 =               // Port n.14 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE15 =              // Port n.15 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF15 =               // Port n.15 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRH_RESET_VALUE = 0x44444444;

    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data
    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data
    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BS0 = 0x1;            // Set bit 0
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Set bit 1
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Set bit 1
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Set bit 3
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Set bit 4
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Set bit 5
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Set bit 6
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Set bit 7
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Set bit 8
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Set bit 9
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Set bit 10
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Set bit 11
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Set bit 12
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Set bit 13
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Set bit 14
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Set bit 15
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Reset bit 0
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Reset bit 1
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Reset bit 2
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Reset bit 3
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Reset bit 4
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Reset bit 5
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Reset bit 6
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Reset bit 7
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Reset bit 8
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Reset bit 9
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Reset bit 10
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Reset bit 11
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Reset bit 12
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Reset bit 13
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Reset bit 14
    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Reset bit 15
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Reset bit 0
    static constexpr uint32_t BRR_BR1 = 0x2;            // Reset bit 1
    static constexpr uint32_t BRR_BR2 = 0x4;            // Reset bit 1
    static constexpr uint32_t BRR_BR3 = 0x8;            // Reset bit 3
    static constexpr uint32_t BRR_BR4 = 0x10;           // Reset bit 4
    static constexpr uint32_t BRR_BR5 = 0x20;           // Reset bit 5
    static constexpr uint32_t BRR_BR6 = 0x40;           // Reset bit 6
    static constexpr uint32_t BRR_BR7 = 0x80;           // Reset bit 7
    static constexpr uint32_t BRR_BR8 = 0x100;          // Reset bit 8
    static constexpr uint32_t BRR_BR9 = 0x200;          // Reset bit 9
    static constexpr uint32_t BRR_BR10 = 0x400;         // Reset bit 10
    static constexpr uint32_t BRR_BR11 = 0x800;         // Reset bit 11
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Reset bit 12
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Reset bit 13
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Reset bit 14
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Reset bit 15
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port A Lock bit 0
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port A Lock bit 1
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port A Lock bit 2
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port A Lock bit 3
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port A Lock bit 4
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port A Lock bit 5
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port A Lock bit 6
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port A Lock bit 7
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port A Lock bit 8
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port A Lock bit 9
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port A Lock bit 10
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port A Lock bit 11
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port A Lock bit 12
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port A Lock bit 13
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port A Lock bit 14
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port A Lock bit 15
    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Lock key
    static const uint32_t LCKR_RESET_VALUE = 0x0;
};

static gpiod_t& GPIOD = *reinterpret_cast<gpiod_t*>(0x40011400);

#define HAVE_PERIPHERAL_GPIOD


////
//
//    General purpose I/O
//
////

struct gpioe_t
{
    volatile uint32_t    CRL;                  // [Read-write] Port configuration register low (GPIOn_CRL)
    volatile uint32_t    CRH;                  // [Read-write] Port configuration register high (GPIOn_CRL)
    volatile uint32_t    IDR;                  // [Read-only] Port input data register (GPIOn_IDR)
    volatile uint32_t    ODR;                  // [Read-write] Port output data register (GPIOn_ODR)
    volatile uint32_t    BSRR;                 // [Write-only] Port bit set/reset register (GPIOn_BSRR)
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register (GPIOn_BRR)
    volatile uint32_t    LCKR;                 // [Read-write] Port configuration lock register

    template<uint32_t X>
    static constexpr uint32_t CRL_MODE0 =               // Port n.0 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF0 =                // Port n.0 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE1 =               // Port n.1 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF1 =                // Port n.1 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE2 =               // Port n.2 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF2 =                // Port n.2 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE3 =               // Port n.3 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF3 =                // Port n.3 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE4 =               // Port n.4 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF4 =                // Port n.4 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE5 =               // Port n.5 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF5 =                // Port n.5 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE6 =               // Port n.6 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF6 =                // Port n.6 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE7 =               // Port n.7 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF7 =                // Port n.7 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRL_RESET_VALUE = 0x44444444;

    template<uint32_t X>
    static constexpr uint32_t CRH_MODE8 =               // Port n.8 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF8 =                // Port n.8 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE9 =               // Port n.9 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF9 =                // Port n.9 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE10 =              // Port n.10 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF10 =               // Port n.10 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE11 =              // Port n.11 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF11 =               // Port n.11 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE12 =              // Port n.12 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF12 =               // Port n.12 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE13 =              // Port n.13 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF13 =               // Port n.13 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE14 =              // Port n.14 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF14 =               // Port n.14 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE15 =              // Port n.15 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF15 =               // Port n.15 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRH_RESET_VALUE = 0x44444444;

    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data
    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data
    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BS0 = 0x1;            // Set bit 0
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Set bit 1
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Set bit 1
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Set bit 3
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Set bit 4
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Set bit 5
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Set bit 6
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Set bit 7
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Set bit 8
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Set bit 9
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Set bit 10
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Set bit 11
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Set bit 12
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Set bit 13
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Set bit 14
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Set bit 15
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Reset bit 0
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Reset bit 1
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Reset bit 2
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Reset bit 3
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Reset bit 4
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Reset bit 5
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Reset bit 6
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Reset bit 7
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Reset bit 8
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Reset bit 9
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Reset bit 10
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Reset bit 11
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Reset bit 12
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Reset bit 13
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Reset bit 14
    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Reset bit 15
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Reset bit 0
    static constexpr uint32_t BRR_BR1 = 0x2;            // Reset bit 1
    static constexpr uint32_t BRR_BR2 = 0x4;            // Reset bit 1
    static constexpr uint32_t BRR_BR3 = 0x8;            // Reset bit 3
    static constexpr uint32_t BRR_BR4 = 0x10;           // Reset bit 4
    static constexpr uint32_t BRR_BR5 = 0x20;           // Reset bit 5
    static constexpr uint32_t BRR_BR6 = 0x40;           // Reset bit 6
    static constexpr uint32_t BRR_BR7 = 0x80;           // Reset bit 7
    static constexpr uint32_t BRR_BR8 = 0x100;          // Reset bit 8
    static constexpr uint32_t BRR_BR9 = 0x200;          // Reset bit 9
    static constexpr uint32_t BRR_BR10 = 0x400;         // Reset bit 10
    static constexpr uint32_t BRR_BR11 = 0x800;         // Reset bit 11
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Reset bit 12
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Reset bit 13
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Reset bit 14
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Reset bit 15
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port A Lock bit 0
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port A Lock bit 1
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port A Lock bit 2
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port A Lock bit 3
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port A Lock bit 4
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port A Lock bit 5
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port A Lock bit 6
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port A Lock bit 7
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port A Lock bit 8
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port A Lock bit 9
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port A Lock bit 10
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port A Lock bit 11
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port A Lock bit 12
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port A Lock bit 13
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port A Lock bit 14
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port A Lock bit 15
    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Lock key
    static const uint32_t LCKR_RESET_VALUE = 0x0;
};

static gpioe_t& GPIOE = *reinterpret_cast<gpioe_t*>(0x40011800);

#define HAVE_PERIPHERAL_GPIOE


////
//
//    General purpose I/O
//
////

struct gpiof_t
{
    volatile uint32_t    CRL;                  // [Read-write] Port configuration register low (GPIOn_CRL)
    volatile uint32_t    CRH;                  // [Read-write] Port configuration register high (GPIOn_CRL)
    volatile uint32_t    IDR;                  // [Read-only] Port input data register (GPIOn_IDR)
    volatile uint32_t    ODR;                  // [Read-write] Port output data register (GPIOn_ODR)
    volatile uint32_t    BSRR;                 // [Write-only] Port bit set/reset register (GPIOn_BSRR)
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register (GPIOn_BRR)
    volatile uint32_t    LCKR;                 // [Read-write] Port configuration lock register

    template<uint32_t X>
    static constexpr uint32_t CRL_MODE0 =               // Port n.0 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF0 =                // Port n.0 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE1 =               // Port n.1 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF1 =                // Port n.1 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE2 =               // Port n.2 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF2 =                // Port n.2 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE3 =               // Port n.3 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF3 =                // Port n.3 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE4 =               // Port n.4 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF4 =                // Port n.4 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE5 =               // Port n.5 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF5 =                // Port n.5 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE6 =               // Port n.6 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF6 =                // Port n.6 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE7 =               // Port n.7 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF7 =                // Port n.7 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRL_RESET_VALUE = 0x44444444;

    template<uint32_t X>
    static constexpr uint32_t CRH_MODE8 =               // Port n.8 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF8 =                // Port n.8 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE9 =               // Port n.9 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF9 =                // Port n.9 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE10 =              // Port n.10 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF10 =               // Port n.10 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE11 =              // Port n.11 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF11 =               // Port n.11 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE12 =              // Port n.12 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF12 =               // Port n.12 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE13 =              // Port n.13 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF13 =               // Port n.13 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE14 =              // Port n.14 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF14 =               // Port n.14 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE15 =              // Port n.15 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF15 =               // Port n.15 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRH_RESET_VALUE = 0x44444444;

    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data
    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data
    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BS0 = 0x1;            // Set bit 0
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Set bit 1
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Set bit 1
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Set bit 3
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Set bit 4
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Set bit 5
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Set bit 6
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Set bit 7
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Set bit 8
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Set bit 9
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Set bit 10
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Set bit 11
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Set bit 12
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Set bit 13
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Set bit 14
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Set bit 15
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Reset bit 0
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Reset bit 1
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Reset bit 2
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Reset bit 3
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Reset bit 4
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Reset bit 5
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Reset bit 6
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Reset bit 7
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Reset bit 8
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Reset bit 9
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Reset bit 10
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Reset bit 11
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Reset bit 12
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Reset bit 13
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Reset bit 14
    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Reset bit 15
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Reset bit 0
    static constexpr uint32_t BRR_BR1 = 0x2;            // Reset bit 1
    static constexpr uint32_t BRR_BR2 = 0x4;            // Reset bit 1
    static constexpr uint32_t BRR_BR3 = 0x8;            // Reset bit 3
    static constexpr uint32_t BRR_BR4 = 0x10;           // Reset bit 4
    static constexpr uint32_t BRR_BR5 = 0x20;           // Reset bit 5
    static constexpr uint32_t BRR_BR6 = 0x40;           // Reset bit 6
    static constexpr uint32_t BRR_BR7 = 0x80;           // Reset bit 7
    static constexpr uint32_t BRR_BR8 = 0x100;          // Reset bit 8
    static constexpr uint32_t BRR_BR9 = 0x200;          // Reset bit 9
    static constexpr uint32_t BRR_BR10 = 0x400;         // Reset bit 10
    static constexpr uint32_t BRR_BR11 = 0x800;         // Reset bit 11
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Reset bit 12
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Reset bit 13
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Reset bit 14
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Reset bit 15
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port A Lock bit 0
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port A Lock bit 1
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port A Lock bit 2
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port A Lock bit 3
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port A Lock bit 4
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port A Lock bit 5
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port A Lock bit 6
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port A Lock bit 7
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port A Lock bit 8
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port A Lock bit 9
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port A Lock bit 10
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port A Lock bit 11
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port A Lock bit 12
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port A Lock bit 13
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port A Lock bit 14
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port A Lock bit 15
    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Lock key
    static const uint32_t LCKR_RESET_VALUE = 0x0;
};

static gpiof_t& GPIOF = *reinterpret_cast<gpiof_t*>(0x40011c00);

#define HAVE_PERIPHERAL_GPIOF


////
//
//    General purpose I/O
//
////

struct gpiog_t
{
    volatile uint32_t    CRL;                  // [Read-write] Port configuration register low (GPIOn_CRL)
    volatile uint32_t    CRH;                  // [Read-write] Port configuration register high (GPIOn_CRL)
    volatile uint32_t    IDR;                  // [Read-only] Port input data register (GPIOn_IDR)
    volatile uint32_t    ODR;                  // [Read-write] Port output data register (GPIOn_ODR)
    volatile uint32_t    BSRR;                 // [Write-only] Port bit set/reset register (GPIOn_BSRR)
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register (GPIOn_BRR)
    volatile uint32_t    LCKR;                 // [Read-write] Port configuration lock register

    template<uint32_t X>
    static constexpr uint32_t CRL_MODE0 =               // Port n.0 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF0 =                // Port n.0 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE1 =               // Port n.1 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF1 =                // Port n.1 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE2 =               // Port n.2 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF2 =                // Port n.2 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE3 =               // Port n.3 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF3 =                // Port n.3 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE4 =               // Port n.4 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF4 =                // Port n.4 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE5 =               // Port n.5 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF5 =                // Port n.5 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE6 =               // Port n.6 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF6 =                // Port n.6 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_MODE7 =               // Port n.7 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRL_CNF7 =                // Port n.7 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRL_RESET_VALUE = 0x44444444;

    template<uint32_t X>
    static constexpr uint32_t CRH_MODE8 =               // Port n.8 mode bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF8 =                // Port n.8 configuration bits (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE9 =               // Port n.9 mode bits (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF9 =                // Port n.9 configuration bits (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE10 =              // Port n.10 mode bits (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF10 =               // Port n.10 configuration bits (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE11 =              // Port n.11 mode bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF11 =               // Port n.11 configuration bits (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE12 =              // Port n.12 mode bits (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF12 =               // Port n.12 configuration bits (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE13 =              // Port n.13 mode bits (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF13 =               // Port n.13 configuration bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE14 =              // Port n.14 mode bits (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF14 =               // Port n.14 configuration bits (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_MODE15 =              // Port n.15 mode bits (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CRH_CNF15 =               // Port n.15 configuration bits (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CRH_RESET_VALUE = 0x44444444;

    static constexpr uint32_t IDR_IDR0 = 0x1;           // Port input data
    static constexpr uint32_t IDR_IDR1 = 0x2;           // Port input data
    static constexpr uint32_t IDR_IDR2 = 0x4;           // Port input data
    static constexpr uint32_t IDR_IDR3 = 0x8;           // Port input data
    static constexpr uint32_t IDR_IDR4 = 0x10;          // Port input data
    static constexpr uint32_t IDR_IDR5 = 0x20;          // Port input data
    static constexpr uint32_t IDR_IDR6 = 0x40;          // Port input data
    static constexpr uint32_t IDR_IDR7 = 0x80;          // Port input data
    static constexpr uint32_t IDR_IDR8 = 0x100;         // Port input data
    static constexpr uint32_t IDR_IDR9 = 0x200;         // Port input data
    static constexpr uint32_t IDR_IDR10 = 0x400;        // Port input data
    static constexpr uint32_t IDR_IDR11 = 0x800;        // Port input data
    static constexpr uint32_t IDR_IDR12 = 0x1000;       // Port input data
    static constexpr uint32_t IDR_IDR13 = 0x2000;       // Port input data
    static constexpr uint32_t IDR_IDR14 = 0x4000;       // Port input data
    static constexpr uint32_t IDR_IDR15 = 0x8000;       // Port input data
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t ODR_ODR0 = 0x1;           // Port output data
    static constexpr uint32_t ODR_ODR1 = 0x2;           // Port output data
    static constexpr uint32_t ODR_ODR2 = 0x4;           // Port output data
    static constexpr uint32_t ODR_ODR3 = 0x8;           // Port output data
    static constexpr uint32_t ODR_ODR4 = 0x10;          // Port output data
    static constexpr uint32_t ODR_ODR5 = 0x20;          // Port output data
    static constexpr uint32_t ODR_ODR6 = 0x40;          // Port output data
    static constexpr uint32_t ODR_ODR7 = 0x80;          // Port output data
    static constexpr uint32_t ODR_ODR8 = 0x100;         // Port output data
    static constexpr uint32_t ODR_ODR9 = 0x200;         // Port output data
    static constexpr uint32_t ODR_ODR10 = 0x400;        // Port output data
    static constexpr uint32_t ODR_ODR11 = 0x800;        // Port output data
    static constexpr uint32_t ODR_ODR12 = 0x1000;       // Port output data
    static constexpr uint32_t ODR_ODR13 = 0x2000;       // Port output data
    static constexpr uint32_t ODR_ODR14 = 0x4000;       // Port output data
    static constexpr uint32_t ODR_ODR15 = 0x8000;       // Port output data
    static const uint32_t ODR_RESET_VALUE = 0x0;

    static constexpr uint32_t BSRR_BS0 = 0x1;            // Set bit 0
    static constexpr uint32_t BSRR_BS1 = 0x2;            // Set bit 1
    static constexpr uint32_t BSRR_BS2 = 0x4;            // Set bit 1
    static constexpr uint32_t BSRR_BS3 = 0x8;            // Set bit 3
    static constexpr uint32_t BSRR_BS4 = 0x10;           // Set bit 4
    static constexpr uint32_t BSRR_BS5 = 0x20;           // Set bit 5
    static constexpr uint32_t BSRR_BS6 = 0x40;           // Set bit 6
    static constexpr uint32_t BSRR_BS7 = 0x80;           // Set bit 7
    static constexpr uint32_t BSRR_BS8 = 0x100;          // Set bit 8
    static constexpr uint32_t BSRR_BS9 = 0x200;          // Set bit 9
    static constexpr uint32_t BSRR_BS10 = 0x400;         // Set bit 10
    static constexpr uint32_t BSRR_BS11 = 0x800;         // Set bit 11
    static constexpr uint32_t BSRR_BS12 = 0x1000;        // Set bit 12
    static constexpr uint32_t BSRR_BS13 = 0x2000;        // Set bit 13
    static constexpr uint32_t BSRR_BS14 = 0x4000;        // Set bit 14
    static constexpr uint32_t BSRR_BS15 = 0x8000;        // Set bit 15
    static constexpr uint32_t BSRR_BR0 = 0x10000;        // Reset bit 0
    static constexpr uint32_t BSRR_BR1 = 0x20000;        // Reset bit 1
    static constexpr uint32_t BSRR_BR2 = 0x40000;        // Reset bit 2
    static constexpr uint32_t BSRR_BR3 = 0x80000;        // Reset bit 3
    static constexpr uint32_t BSRR_BR4 = 0x100000;       // Reset bit 4
    static constexpr uint32_t BSRR_BR5 = 0x200000;       // Reset bit 5
    static constexpr uint32_t BSRR_BR6 = 0x400000;       // Reset bit 6
    static constexpr uint32_t BSRR_BR7 = 0x800000;       // Reset bit 7
    static constexpr uint32_t BSRR_BR8 = 0x1000000;      // Reset bit 8
    static constexpr uint32_t BSRR_BR9 = 0x2000000;      // Reset bit 9
    static constexpr uint32_t BSRR_BR10 = 0x4000000;     // Reset bit 10
    static constexpr uint32_t BSRR_BR11 = 0x8000000;     // Reset bit 11
    static constexpr uint32_t BSRR_BR12 = 0x10000000;    // Reset bit 12
    static constexpr uint32_t BSRR_BR13 = 0x20000000;    // Reset bit 13
    static constexpr uint32_t BSRR_BR14 = 0x40000000;    // Reset bit 14
    static constexpr uint32_t BSRR_BR15 = 0x80000000;    // Reset bit 15
    static const uint32_t BSRR_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Reset bit 0
    static constexpr uint32_t BRR_BR1 = 0x2;            // Reset bit 1
    static constexpr uint32_t BRR_BR2 = 0x4;            // Reset bit 1
    static constexpr uint32_t BRR_BR3 = 0x8;            // Reset bit 3
    static constexpr uint32_t BRR_BR4 = 0x10;           // Reset bit 4
    static constexpr uint32_t BRR_BR5 = 0x20;           // Reset bit 5
    static constexpr uint32_t BRR_BR6 = 0x40;           // Reset bit 6
    static constexpr uint32_t BRR_BR7 = 0x80;           // Reset bit 7
    static constexpr uint32_t BRR_BR8 = 0x100;          // Reset bit 8
    static constexpr uint32_t BRR_BR9 = 0x200;          // Reset bit 9
    static constexpr uint32_t BRR_BR10 = 0x400;         // Reset bit 10
    static constexpr uint32_t BRR_BR11 = 0x800;         // Reset bit 11
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Reset bit 12
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Reset bit 13
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Reset bit 14
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Reset bit 15
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t LCKR_LCK0 = 0x1;           // Port A Lock bit 0
    static constexpr uint32_t LCKR_LCK1 = 0x2;           // Port A Lock bit 1
    static constexpr uint32_t LCKR_LCK2 = 0x4;           // Port A Lock bit 2
    static constexpr uint32_t LCKR_LCK3 = 0x8;           // Port A Lock bit 3
    static constexpr uint32_t LCKR_LCK4 = 0x10;          // Port A Lock bit 4
    static constexpr uint32_t LCKR_LCK5 = 0x20;          // Port A Lock bit 5
    static constexpr uint32_t LCKR_LCK6 = 0x40;          // Port A Lock bit 6
    static constexpr uint32_t LCKR_LCK7 = 0x80;          // Port A Lock bit 7
    static constexpr uint32_t LCKR_LCK8 = 0x100;         // Port A Lock bit 8
    static constexpr uint32_t LCKR_LCK9 = 0x200;         // Port A Lock bit 9
    static constexpr uint32_t LCKR_LCK10 = 0x400;        // Port A Lock bit 10
    static constexpr uint32_t LCKR_LCK11 = 0x800;        // Port A Lock bit 11
    static constexpr uint32_t LCKR_LCK12 = 0x1000;       // Port A Lock bit 12
    static constexpr uint32_t LCKR_LCK13 = 0x2000;       // Port A Lock bit 13
    static constexpr uint32_t LCKR_LCK14 = 0x4000;       // Port A Lock bit 14
    static constexpr uint32_t LCKR_LCK15 = 0x8000;       // Port A Lock bit 15
    static constexpr uint32_t LCKR_LCKK = 0x10000;       // Lock key
    static const uint32_t LCKR_RESET_VALUE = 0x0;
};

static gpiog_t& GPIOG = *reinterpret_cast<gpiog_t*>(0x40012000);

#define HAVE_PERIPHERAL_GPIOG


////
//
//    Alternate function I/O
//
////

struct afio_t
{
    volatile uint32_t    EVCR;                 // [Read-write] Event Control Register (AFIO_EVCR)
    volatile uint32_t    MAPR;                 // AF remap and debug I/O configuration register (AFIO_MAPR)
    volatile uint32_t    EXTICR1;              // [Read-write] External interrupt configuration register 1 (AFIO_EXTICR1)
    volatile uint32_t    EXTICR2;              // [Read-write] External interrupt configuration register 2 (AFIO_EXTICR2)
    volatile uint32_t    EXTICR3;              // [Read-write] External interrupt configuration register 3 (AFIO_EXTICR3)
    volatile uint32_t    EXTICR4;              // [Read-write] External interrupt configuration register 4 (AFIO_EXTICR4)
    reserved_t<1>        _0;
    volatile uint32_t    MAPR2;                // [Read-write] AF remap and debug I/O configuration register

    template<uint32_t X>
    static constexpr uint32_t EVCR_PIN =                 // Pin selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EVCR_PORT =                // Port selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t EVCR_EVOE = 0x80;          // Event Output Enable
    static const uint32_t EVCR_RESET_VALUE = 0x0;

    static constexpr uint32_t MAPR_SPI1_REMAP = 0x1;     // SPI1 remapping, Read-write
    static constexpr uint32_t MAPR_I2C1_REMAP = 0x2;     // I2C1 remapping, Read-write
    static constexpr uint32_t MAPR_USART1_REMAP = 0x4;   // USART1 remapping, Read-write
    static constexpr uint32_t MAPR_USART2_REMAP = 0x8;   // USART2 remapping, Read-write
    template<uint32_t X>
    static constexpr uint32_t MAPR_USART3_REMAP =        // USART3 remapping (2 bits), Read-write
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MAPR_TIM1_REMAP =          // TIM1 remapping (2 bits), Read-write
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MAPR_TIM2_REMAP =          // TIM2 remapping (2 bits), Read-write
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MAPR_TIM3_REMAP =          // TIM3 remapping (2 bits), Read-write
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t MAPR_TIM4_REMAP = 0x1000;  // TIM4 remapping, Read-write
    template<uint32_t X>
    static constexpr uint32_t MAPR_CAN_REMAP =           // CAN1 remapping (2 bits), Read-write
        bit_field_t<13, 0x3>::value<X>();
    static constexpr uint32_t MAPR_PD01_REMAP = 0x8000;  // Port D0/Port D1 mapping on OSCIN/OSCOUT, Read-write
    static constexpr uint32_t MAPR_TIM5CH4_IREMAP = 0x10000;// Set and cleared by software, Read-write
    static constexpr uint32_t MAPR_ADC1_ETRGINJ_REMAP = 0x20000;// ADC 1 External trigger injected conversion remapping, Read-write
    static constexpr uint32_t MAPR_ADC1_ETRGREG_REMAP = 0x40000;// ADC 1 external trigger regular conversion remapping, Read-write
    static constexpr uint32_t MAPR_ADC2_ETRGINJ_REMAP = 0x80000;// ADC 2 external trigger injected conversion remapping, Read-write
    static constexpr uint32_t MAPR_ADC2_ETRGREG_REMAP = 0x100000;// ADC 2 external trigger regular conversion remapping, Read-write
    template<uint32_t X>
    static constexpr uint32_t MAPR_SWJ_CFG =             // Serial wire JTAG configuration (3 bits), Write-only
        bit_field_t<24, 0x7>::value<X>();
    static const uint32_t MAPR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EXTICR1_EXTI0 =               // EXTI0 configuration (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR1_EXTI1 =               // EXTI1 configuration (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR1_EXTI2 =               // EXTI2 configuration (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR1_EXTI3 =               // EXTI3 configuration (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    static const uint32_t EXTICR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EXTICR2_EXTI4 =               // EXTI4 configuration (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR2_EXTI5 =               // EXTI5 configuration (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR2_EXTI6 =               // EXTI6 configuration (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR2_EXTI7 =               // EXTI7 configuration (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    static const uint32_t EXTICR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EXTICR3_EXTI8 =               // EXTI8 configuration (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR3_EXTI9 =               // EXTI9 configuration (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR3_EXTI10 =              // EXTI10 configuration (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR3_EXTI11 =              // EXTI11 configuration (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    static const uint32_t EXTICR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EXTICR4_EXTI12 =              // EXTI12 configuration (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR4_EXTI13 =              // EXTI13 configuration (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR4_EXTI14 =              // EXTI14 configuration (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR4_EXTI15 =              // EXTI15 configuration (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    static const uint32_t EXTICR4_RESET_VALUE = 0x0;

    static constexpr uint32_t MAPR2_TIM9_REMAP = 0x20;    // TIM9 remapping
    static constexpr uint32_t MAPR2_TIM10_REMAP = 0x40;   // TIM10 remapping
    static constexpr uint32_t MAPR2_TIM11_REMAP = 0x80;   // TIM11 remapping
    static constexpr uint32_t MAPR2_TIM13_REMAP = 0x100;  // TIM13 remapping
    static constexpr uint32_t MAPR2_TIM14_REMAP = 0x200;  // TIM14 remapping
    static constexpr uint32_t MAPR2_FSMC_NADV = 0x400;    // NADV connect/disconnect
    static const uint32_t MAPR2_RESET_VALUE = 0x0;
};

static afio_t& AFIO = *reinterpret_cast<afio_t*>(0x40010000);

#define HAVE_PERIPHERAL_AFIO


////
//
//    EXTI
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
    static const uint32_t IMR_RESET_VALUE = 0x0;

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
    static constexpr uint32_t RTSR_TR18 = 0x40000;       // Rising trigger event configuration of line 18
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
    static constexpr uint32_t FTSR_TR18 = 0x40000;       // Falling trigger event configuration of line 18
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
    static constexpr uint32_t SWIER_SWIER18 = 0x40000;    // Software Interrupt on line 18
    static const uint32_t SWIER_RESET_VALUE = 0x0;

    static constexpr uint32_t PR_PR0 = 0x1;            // Pending bit 0
    static constexpr uint32_t PR_PR1 = 0x2;            // Pending bit 1
    static constexpr uint32_t PR_PR2 = 0x4;            // Pending bit 2
    static constexpr uint32_t PR_PR3 = 0x8;            // Pending bit 3
    static constexpr uint32_t PR_PR4 = 0x10;           // Pending bit 4
    static constexpr uint32_t PR_PR5 = 0x20;           // Pending bit 5
    static constexpr uint32_t PR_PR6 = 0x40;           // Pending bit 6
    static constexpr uint32_t PR_PR7 = 0x80;           // Pending bit 7
    static constexpr uint32_t PR_PR8 = 0x100;          // Pending bit 8
    static constexpr uint32_t PR_PR9 = 0x200;          // Pending bit 9
    static constexpr uint32_t PR_PR10 = 0x400;         // Pending bit 10
    static constexpr uint32_t PR_PR11 = 0x800;         // Pending bit 11
    static constexpr uint32_t PR_PR12 = 0x1000;        // Pending bit 12
    static constexpr uint32_t PR_PR13 = 0x2000;        // Pending bit 13
    static constexpr uint32_t PR_PR14 = 0x4000;        // Pending bit 14
    static constexpr uint32_t PR_PR15 = 0x8000;        // Pending bit 15
    static constexpr uint32_t PR_PR16 = 0x10000;       // Pending bit 16
    static constexpr uint32_t PR_PR17 = 0x20000;       // Pending bit 17
    static constexpr uint32_t PR_PR18 = 0x40000;       // Pending bit 18
    static const uint32_t PR_RESET_VALUE = 0x0;

    static constexpr uint8_t EXTI0 = 6; // EXTI Line0 interrupt
    static constexpr uint8_t EXTI1 = 7; // EXTI Line1 interrupt
    static constexpr uint8_t EXTI15_10 = 40; // EXTI Line[15:10] interrupts
    static constexpr uint8_t EXTI2 = 8; // EXTI Line2 interrupt
    static constexpr uint8_t EXTI3 = 9; // EXTI Line3 interrupt
    static constexpr uint8_t EXTI4 = 10; // EXTI Line4 interrupt
    static constexpr uint8_t EXTI9_5 = 23; // EXTI Line[9:5] interrupts
    static constexpr uint8_t TAMPER = 2; // Tamper interrupt
};

static exti_t& EXTI = *reinterpret_cast<exti_t*>(0x40010400);

#define HAVE_PERIPHERAL_EXTI


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
    static constexpr uint32_t IFCR_CGIF2 = 0x10;         // Channel 2 Global interrupt clear
    static constexpr uint32_t IFCR_CGIF3 = 0x100;        // Channel 3 Global interrupt clear
    static constexpr uint32_t IFCR_CGIF4 = 0x1000;       // Channel 4 Global interrupt clear
    static constexpr uint32_t IFCR_CGIF5 = 0x10000;      // Channel 5 Global interrupt clear
    static constexpr uint32_t IFCR_CGIF6 = 0x100000;     // Channel 6 Global interrupt clear
    static constexpr uint32_t IFCR_CGIF7 = 0x1000000;    // Channel 7 Global interrupt clear
    static constexpr uint32_t IFCR_CTCIF1 = 0x2;         // Channel 1 Transfer Complete clear
    static constexpr uint32_t IFCR_CTCIF2 = 0x20;        // Channel 2 Transfer Complete clear
    static constexpr uint32_t IFCR_CTCIF3 = 0x200;       // Channel 3 Transfer Complete clear
    static constexpr uint32_t IFCR_CTCIF4 = 0x2000;      // Channel 4 Transfer Complete clear
    static constexpr uint32_t IFCR_CTCIF5 = 0x20000;     // Channel 5 Transfer Complete clear
    static constexpr uint32_t IFCR_CTCIF6 = 0x200000;    // Channel 6 Transfer Complete clear
    static constexpr uint32_t IFCR_CTCIF7 = 0x2000000;   // Channel 7 Transfer Complete clear
    static constexpr uint32_t IFCR_CHTIF1 = 0x4;         // Channel 1 Half Transfer clear
    static constexpr uint32_t IFCR_CHTIF2 = 0x40;        // Channel 2 Half Transfer clear
    static constexpr uint32_t IFCR_CHTIF3 = 0x400;       // Channel 3 Half Transfer clear
    static constexpr uint32_t IFCR_CHTIF4 = 0x4000;      // Channel 4 Half Transfer clear
    static constexpr uint32_t IFCR_CHTIF5 = 0x40000;     // Channel 5 Half Transfer clear
    static constexpr uint32_t IFCR_CHTIF6 = 0x400000;    // Channel 6 Half Transfer clear
    static constexpr uint32_t IFCR_CHTIF7 = 0x4000000;   // Channel 7 Half Transfer clear
    static constexpr uint32_t IFCR_CTEIF1 = 0x8;         // Channel 1 Transfer Error clear
    static constexpr uint32_t IFCR_CTEIF2 = 0x80;        // Channel 2 Transfer Error clear
    static constexpr uint32_t IFCR_CTEIF3 = 0x800;       // Channel 3 Transfer Error clear
    static constexpr uint32_t IFCR_CTEIF4 = 0x8000;      // Channel 4 Transfer Error clear
    static constexpr uint32_t IFCR_CTEIF5 = 0x80000;     // Channel 5 Transfer Error clear
    static constexpr uint32_t IFCR_CTEIF6 = 0x800000;    // Channel 6 Transfer Error clear
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

    static constexpr uint8_t DMA1_CHANNEL1 = 11; // DMA1 Channel1 global interrupt
    static constexpr uint8_t DMA1_CHANNEL2 = 12; // DMA1 Channel2 global interrupt
    static constexpr uint8_t DMA1_CHANNEL3 = 13; // DMA1 Channel3 global interrupt
    static constexpr uint8_t DMA1_CHANNEL4 = 14; // DMA1 Channel4 global interrupt
    static constexpr uint8_t DMA1_CHANNEL5 = 15; // DMA1 Channel5 global interrupt
    static constexpr uint8_t DMA1_CHANNEL6 = 16; // DMA1 Channel6 global interrupt
    static constexpr uint8_t DMA1_CHANNEL7 = 17; // DMA1 Channel7 global interrupt
};

static dma1_t& DMA1 = *reinterpret_cast<dma1_t*>(0x40020000);

#define HAVE_PERIPHERAL_DMA1


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
    static constexpr uint32_t IFCR_CGIF2 = 0x10;         // Channel 2 Global interrupt clear
    static constexpr uint32_t IFCR_CGIF3 = 0x100;        // Channel 3 Global interrupt clear
    static constexpr uint32_t IFCR_CGIF4 = 0x1000;       // Channel 4 Global interrupt clear
    static constexpr uint32_t IFCR_CGIF5 = 0x10000;      // Channel 5 Global interrupt clear
    static constexpr uint32_t IFCR_CGIF6 = 0x100000;     // Channel 6 Global interrupt clear
    static constexpr uint32_t IFCR_CGIF7 = 0x1000000;    // Channel 7 Global interrupt clear
    static constexpr uint32_t IFCR_CTCIF1 = 0x2;         // Channel 1 Transfer Complete clear
    static constexpr uint32_t IFCR_CTCIF2 = 0x20;        // Channel 2 Transfer Complete clear
    static constexpr uint32_t IFCR_CTCIF3 = 0x200;       // Channel 3 Transfer Complete clear
    static constexpr uint32_t IFCR_CTCIF4 = 0x2000;      // Channel 4 Transfer Complete clear
    static constexpr uint32_t IFCR_CTCIF5 = 0x20000;     // Channel 5 Transfer Complete clear
    static constexpr uint32_t IFCR_CTCIF6 = 0x200000;    // Channel 6 Transfer Complete clear
    static constexpr uint32_t IFCR_CTCIF7 = 0x2000000;   // Channel 7 Transfer Complete clear
    static constexpr uint32_t IFCR_CHTIF1 = 0x4;         // Channel 1 Half Transfer clear
    static constexpr uint32_t IFCR_CHTIF2 = 0x40;        // Channel 2 Half Transfer clear
    static constexpr uint32_t IFCR_CHTIF3 = 0x400;       // Channel 3 Half Transfer clear
    static constexpr uint32_t IFCR_CHTIF4 = 0x4000;      // Channel 4 Half Transfer clear
    static constexpr uint32_t IFCR_CHTIF5 = 0x40000;     // Channel 5 Half Transfer clear
    static constexpr uint32_t IFCR_CHTIF6 = 0x400000;    // Channel 6 Half Transfer clear
    static constexpr uint32_t IFCR_CHTIF7 = 0x4000000;   // Channel 7 Half Transfer clear
    static constexpr uint32_t IFCR_CTEIF1 = 0x8;         // Channel 1 Transfer Error clear
    static constexpr uint32_t IFCR_CTEIF2 = 0x80;        // Channel 2 Transfer Error clear
    static constexpr uint32_t IFCR_CTEIF3 = 0x800;       // Channel 3 Transfer Error clear
    static constexpr uint32_t IFCR_CTEIF4 = 0x8000;      // Channel 4 Transfer Error clear
    static constexpr uint32_t IFCR_CTEIF5 = 0x80000;     // Channel 5 Transfer Error clear
    static constexpr uint32_t IFCR_CTEIF6 = 0x800000;    // Channel 6 Transfer Error clear
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

    static constexpr uint8_t DMA2_CHANNEL1 = 56; // DMA2 Channel1 global interrupt
    static constexpr uint8_t DMA2_CHANNEL2 = 57; // DMA2 Channel2 global interrupt
    static constexpr uint8_t DMA2_CHANNEL3 = 58; // DMA2 Channel3 global interrupt
    static constexpr uint8_t DMA2_CHANNEL4_5 = 59; // DMA2 Channel4 and DMA2 Channel5 global interrupt
};

static dma2_t& DMA2 = *reinterpret_cast<dma2_t*>(0x40020400);

#define HAVE_PERIPHERAL_DMA2


////
//
//    Secure digital input/output interface
//
////

struct sdio_t
{
    volatile uint32_t    POWER;                // [Read-write] Bits 1:0 = PWRCTRL: Power supply control bits
    volatile uint32_t    CLKCR;                // [Read-write] SDI clock control register (SDIO_CLKCR)
    volatile uint32_t    ARG;                  // [Read-write] Bits 31:0 = : Command argument
    volatile uint32_t    CMD;                  // [Read-write] SDIO command register (SDIO_CMD)
    volatile uint32_t    RESPCMD;              // [Read-only] SDIO command register
    volatile uint32_t    RESPI1;               // [Read-only] Bits 31:0 = CARDSTATUS1
    volatile uint32_t    RESP2;                // [Read-only] Bits 31:0 = CARDSTATUS2
    volatile uint32_t    RESP3;                // [Read-only] Bits 31:0 = CARDSTATUS3
    volatile uint32_t    RESP4;                // [Read-only] Bits 31:0 = CARDSTATUS4
    volatile uint32_t    DTIMER;               // [Read-write] Bits 31:0 = DATATIME: Data timeout period
    volatile uint32_t    DLEN;                 // [Read-write] Bits 24:0 = DATALENGTH: Data length value
    volatile uint32_t    DCTRL;                // [Read-write] SDIO data control register (SDIO_DCTRL)
    volatile uint32_t    DCOUNT;               // [Read-only] Bits 24:0 = DATACOUNT: Data count value
    volatile uint32_t    STA;                  // [Read-only] SDIO status register (SDIO_STA)
    volatile uint32_t    ICR;                  // [Read-write] SDIO interrupt clear register (SDIO_ICR)
    volatile uint32_t    MASK;                 // [Read-write] SDIO mask register (SDIO_MASK)
    reserved_t<2>        _0;
    volatile uint32_t    FIFOCNT;              // [Read-only] Bits 23:0 = FIFOCOUNT: Remaining number of words to be written to or read from the FIFO
    reserved_t<13>       _1;
    volatile uint32_t    FIFO;                 // [Read-write] bits 31:0 = FIFOData: Receive and transmit FIFO data

    template<uint32_t X>
    static constexpr uint32_t POWER_PWRCTRL =             // PWRCTRL (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t POWER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CLKCR_CLKDIV =              // Clock divide factor (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t CLKCR_CLKEN = 0x100;        // Clock enable bit
    static constexpr uint32_t CLKCR_PWRSAV = 0x200;       // Power saving configuration bit
    static constexpr uint32_t CLKCR_BYPASS = 0x400;       // Clock divider bypass enable bit
    template<uint32_t X>
    static constexpr uint32_t CLKCR_WIDBUS =              // Wide bus mode enable bit (2 bits)
        bit_field_t<11, 0x3>::value<X>();
    static constexpr uint32_t CLKCR_NEGEDGE = 0x2000;     // SDIO_CK dephasing selection bit
    static constexpr uint32_t CLKCR_HWFC_EN = 0x4000;     // HW Flow Control enable
    static const uint32_t CLKCR_RESET_VALUE = 0x0;


    static const uint32_t ARG_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CMD_CMDINDEX =            // CMDINDEX (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CMD_WAITRESP =            // WAITRESP (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    static constexpr uint32_t CMD_WAITINT = 0x100;      // WAITINT
    static constexpr uint32_t CMD_WAITPEND = 0x200;     // WAITPEND
    static constexpr uint32_t CMD_CPSMEN = 0x400;       // CPSMEN
    static constexpr uint32_t CMD_SDIOSuspend = 0x800;  // SDIOSuspend
    static constexpr uint32_t CMD_ENCMDcompl = 0x1000;  // ENCMDcompl
    static constexpr uint32_t CMD_nIEN = 0x2000;        // nIEN
    static constexpr uint32_t CMD_CE_ATACMD = 0x4000;   // CE_ATACMD
    static const uint32_t CMD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RESPCMD_RESPCMD =             // RESPCMD (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    static const uint32_t RESPCMD_RESET_VALUE = 0x0;


    static const uint32_t RESPI1_RESET_VALUE = 0x0;


    static const uint32_t RESP2_RESET_VALUE = 0x0;


    static const uint32_t RESP3_RESET_VALUE = 0x0;


    static const uint32_t RESP4_RESET_VALUE = 0x0;


    static const uint32_t DTIMER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DLEN_DATALENGTH =          // Data length value (25 bits)
        bit_field_t<0, 0x1ffffff>::value<X>();
    static const uint32_t DLEN_RESET_VALUE = 0x0;

    static constexpr uint32_t DCTRL_DTEN = 0x1;           // DTEN
    static constexpr uint32_t DCTRL_DTDIR = 0x2;          // DTDIR
    static constexpr uint32_t DCTRL_DTMODE = 0x4;         // DTMODE
    static constexpr uint32_t DCTRL_DMAEN = 0x8;          // DMAEN
    template<uint32_t X>
    static constexpr uint32_t DCTRL_DBLOCKSIZE =          // DBLOCKSIZE (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    static constexpr uint32_t DCTRL_PWSTART = 0x100;      // PWSTART
    static constexpr uint32_t DCTRL_PWSTOP = 0x200;       // PWSTOP
    static constexpr uint32_t DCTRL_RWMOD = 0x400;        // RWMOD
    static constexpr uint32_t DCTRL_SDIOEN = 0x800;       // SDIOEN
    static const uint32_t DCTRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCOUNT_DATACOUNT =           // Data count value (25 bits)
        bit_field_t<0, 0x1ffffff>::value<X>();
    static const uint32_t DCOUNT_RESET_VALUE = 0x0;

    static constexpr uint32_t STA_CCRCFAIL = 0x1;       // CCRCFAIL
    static constexpr uint32_t STA_DCRCFAIL = 0x2;       // DCRCFAIL
    static constexpr uint32_t STA_CTIMEOUT = 0x4;       // CTIMEOUT
    static constexpr uint32_t STA_DTIMEOUT = 0x8;       // DTIMEOUT
    static constexpr uint32_t STA_TXUNDERR = 0x10;      // TXUNDERR
    static constexpr uint32_t STA_RXOVERR = 0x20;       // RXOVERR
    static constexpr uint32_t STA_CMDREND = 0x40;       // CMDREND
    static constexpr uint32_t STA_CMDSENT = 0x80;       // CMDSENT
    static constexpr uint32_t STA_DATAEND = 0x100;      // DATAEND
    static constexpr uint32_t STA_STBITERR = 0x200;     // STBITERR
    static constexpr uint32_t STA_DBCKEND = 0x400;      // DBCKEND
    static constexpr uint32_t STA_CMDACT = 0x800;       // CMDACT
    static constexpr uint32_t STA_TXACT = 0x1000;       // TXACT
    static constexpr uint32_t STA_RXACT = 0x2000;       // RXACT
    static constexpr uint32_t STA_TXFIFOHE = 0x4000;    // TXFIFOHE
    static constexpr uint32_t STA_RXFIFOHF = 0x8000;    // RXFIFOHF
    static constexpr uint32_t STA_TXFIFOF = 0x10000;    // TXFIFOF
    static constexpr uint32_t STA_RXFIFOF = 0x20000;    // RXFIFOF
    static constexpr uint32_t STA_TXFIFOE = 0x40000;    // TXFIFOE
    static constexpr uint32_t STA_RXFIFOE = 0x80000;    // RXFIFOE
    static constexpr uint32_t STA_TXDAVL = 0x100000;    // TXDAVL
    static constexpr uint32_t STA_RXDAVL = 0x200000;    // RXDAVL
    static constexpr uint32_t STA_SDIOIT = 0x400000;    // SDIOIT
    static constexpr uint32_t STA_CEATAEND = 0x800000;  // CEATAEND
    static const uint32_t STA_RESET_VALUE = 0x0;

    static constexpr uint32_t ICR_CCRCFAILC = 0x1;      // CCRCFAILC
    static constexpr uint32_t ICR_DCRCFAILC = 0x2;      // DCRCFAILC
    static constexpr uint32_t ICR_CTIMEOUTC = 0x4;      // CTIMEOUTC
    static constexpr uint32_t ICR_DTIMEOUTC = 0x8;      // DTIMEOUTC
    static constexpr uint32_t ICR_TXUNDERRC = 0x10;     // TXUNDERRC
    static constexpr uint32_t ICR_RXOVERRC = 0x20;      // RXOVERRC
    static constexpr uint32_t ICR_CMDRENDC = 0x40;      // CMDRENDC
    static constexpr uint32_t ICR_CMDSENTC = 0x80;      // CMDSENTC
    static constexpr uint32_t ICR_DATAENDC = 0x100;     // DATAENDC
    static constexpr uint32_t ICR_STBITERRC = 0x200;    // STBITERRC
    static constexpr uint32_t ICR_DBCKENDC = 0x400;     // DBCKENDC
    static constexpr uint32_t ICR_SDIOITC = 0x400000;   // SDIOITC
    static constexpr uint32_t ICR_CEATAENDC = 0x800000; // CEATAENDC
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static constexpr uint32_t MASK_CCRCFAILIE = 0x1;     // CCRCFAILIE
    static constexpr uint32_t MASK_DCRCFAILIE = 0x2;     // DCRCFAILIE
    static constexpr uint32_t MASK_CTIMEOUTIE = 0x4;     // CTIMEOUTIE
    static constexpr uint32_t MASK_DTIMEOUTIE = 0x8;     // DTIMEOUTIE
    static constexpr uint32_t MASK_TXUNDERRIE = 0x10;    // TXUNDERRIE
    static constexpr uint32_t MASK_RXOVERRIE = 0x20;     // RXOVERRIE
    static constexpr uint32_t MASK_CMDRENDIE = 0x40;     // CMDRENDIE
    static constexpr uint32_t MASK_CMDSENTIE = 0x80;     // CMDSENTIE
    static constexpr uint32_t MASK_DATAENDIE = 0x100;    // DATAENDIE
    static constexpr uint32_t MASK_STBITERRIE = 0x200;   // STBITERRIE
    static constexpr uint32_t MASK_DBACKENDIE = 0x400;   // DBACKENDIE
    static constexpr uint32_t MASK_CMDACTIE = 0x800;     // CMDACTIE
    static constexpr uint32_t MASK_TXACTIE = 0x1000;     // TXACTIE
    static constexpr uint32_t MASK_RXACTIE = 0x2000;     // RXACTIE
    static constexpr uint32_t MASK_TXFIFOHEIE = 0x4000;  // TXFIFOHEIE
    static constexpr uint32_t MASK_RXFIFOHFIE = 0x8000;  // RXFIFOHFIE
    static constexpr uint32_t MASK_TXFIFOFIE = 0x10000;  // TXFIFOFIE
    static constexpr uint32_t MASK_RXFIFOFIE = 0x20000;  // RXFIFOFIE
    static constexpr uint32_t MASK_TXFIFOEIE = 0x40000;  // TXFIFOEIE
    static constexpr uint32_t MASK_RXFIFOEIE = 0x80000;  // RXFIFOEIE
    static constexpr uint32_t MASK_TXDAVLIE = 0x100000;  // TXDAVLIE
    static constexpr uint32_t MASK_RXDAVLIE = 0x200000;  // RXDAVLIE
    static constexpr uint32_t MASK_SDIOITIE = 0x400000;  // SDIOITIE
    static constexpr uint32_t MASK_CEATENDIE = 0x800000; // CEATENDIE
    static const uint32_t MASK_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FIFOCNT_FIF0COUNT =           // FIF0COUNT (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();
    static const uint32_t FIFOCNT_RESET_VALUE = 0x0;


    static const uint32_t FIFO_RESET_VALUE = 0x0;

    static constexpr uint8_t SDIO = 49; // SDIO global interrupt
};

static sdio_t& SDIO = *reinterpret_cast<sdio_t*>(0x40018000);

#define HAVE_PERIPHERAL_SDIO


////
//
//    Real time clock
//
////

struct rtc_t
{
    volatile uint32_t    CRH;                  // [Read-write] RTC Control Register High
    volatile uint32_t    CRL;                  // RTC Control Register Low
    volatile uint32_t    PRLH;                 // [Write-only] RTC Prescaler Load Register High
    volatile uint32_t    PRLL;                 // [Write-only] RTC Prescaler Load Register Low
    volatile uint32_t    DIVH;                 // [Read-only] RTC Prescaler Divider Register High
    volatile uint32_t    DIVL;                 // [Read-only] RTC Prescaler Divider Register Low
    volatile uint32_t    CNTH;                 // [Read-write] RTC Counter Register High
    volatile uint32_t    CNTL;                 // [Read-write] RTC Counter Register Low
    volatile uint32_t    ALRH;                 // [Write-only] RTC Alarm Register High
    volatile uint32_t    ALRL;                 // [Write-only] RTC Alarm Register Low

    static constexpr uint32_t CRH_SECIE = 0x1;          // Second interrupt Enable
    static constexpr uint32_t CRH_ALRIE = 0x2;          // Alarm interrupt Enable
    static constexpr uint32_t CRH_OWIE = 0x4;           // Overflow interrupt Enable
    static const uint32_t CRH_RESET_VALUE = 0x0;

    static constexpr uint32_t CRL_SECF = 0x1;           // Second Flag, Read-write
    static constexpr uint32_t CRL_ALRF = 0x2;           // Alarm Flag, Read-write
    static constexpr uint32_t CRL_OWF = 0x4;            // Overflow Flag, Read-write
    static constexpr uint32_t CRL_RSF = 0x8;            // Registers Synchronized Flag, Read-write
    static constexpr uint32_t CRL_CNF = 0x10;           // Configuration Flag, Read-write
    static constexpr uint32_t CRL_RTOFF = 0x20;         // RTC operation OFF, Read-only
    static const uint32_t CRL_RESET_VALUE = 0x20;

    template<uint32_t X>
    static constexpr uint32_t PRLH_PRLH =                // RTC Prescaler Load Register High (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRLH_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PRLL_PRLL =                // RTC Prescaler Divider Register Low (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PRLL_RESET_VALUE = 0x8000;

    template<uint32_t X>
    static constexpr uint32_t DIVH_DIVH =                // RTC prescaler divider register high (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t DIVH_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DIVL_DIVL =                // RTC prescaler divider register Low (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DIVL_RESET_VALUE = 0x8000;

    template<uint32_t X>
    static constexpr uint32_t CNTH_CNTH =                // RTC counter register high (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNTH_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNTL_CNTL =                // RTC counter register Low (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNTL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ALRH_ALRH =                // RTC alarm register high (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ALRH_RESET_VALUE = 0xffff;

    template<uint32_t X>
    static constexpr uint32_t ALRL_ALRL =                // RTC alarm register low (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ALRL_RESET_VALUE = 0xffff;

    static constexpr uint8_t RTC = 3; // RTC global interrupt
    static constexpr uint8_t RTCALARM = 41; // RTC Alarms through EXTI line interrupt
};

static rtc_t& RTC = *reinterpret_cast<rtc_t*>(0x40002800);

#define HAVE_PERIPHERAL_RTC


////
//
//    Backup registers
//
////

struct bkp_t
{
    volatile uint32_t    DR1;                  // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR2;                  // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR3;                  // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR4;                  // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR5;                  // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR6;                  // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR7;                  // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR8;                  // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR9;                  // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR10;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    RTCCR;                // [Read-write] RTC clock calibration register (BKP_RTCCR)
    volatile uint32_t    CR;                   // [Read-write] Backup control register (BKP_CR)
    volatile uint32_t    CSR;                  // BKP_CSR control/status register (BKP_CSR)
    reserved_t<2>        _0;
    volatile uint32_t    DR11;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR12;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR13;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR14;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR15;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR16;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR17;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR18;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR19;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR20;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR21;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR22;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR23;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR24;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR25;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR26;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR27;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR28;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR29;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR30;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR31;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR32;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR33;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR34;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR35;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR36;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR37;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR38;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR39;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR40;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR41;                 // [Read-write] Backup data register (BKP_DR)
    volatile uint32_t    DR42;                 // [Read-write] Backup data register (BKP_DR)

    template<uint32_t X>
    static constexpr uint32_t DR1_D1 =                  // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR2_D2 =                  // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR3_D3 =                  // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR4_D4 =                  // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR5_D5 =                  // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR5_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR6_D6 =                  // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR6_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR7_D7 =                  // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR7_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR8_D8 =                  // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR8_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR9_D9 =                  // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR9_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR10_D10 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR10_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RTCCR_CAL =                 // Calibration value (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t RTCCR_CCO = 0x80;           // Calibration Clock Output
    static constexpr uint32_t RTCCR_ASOE = 0x100;         // Alarm or second output enable
    static constexpr uint32_t RTCCR_ASOS = 0x200;         // Alarm or second output selection
    static const uint32_t RTCCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_TPE = 0x1;            // Tamper pin enable
    static constexpr uint32_t CR_TPAL = 0x2;           // Tamper pin active level
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t CSR_CTE = 0x1;            // Clear Tamper event, Write-only
    static constexpr uint32_t CSR_CTI = 0x2;            // Clear Tamper Interrupt, Write-only
    static constexpr uint32_t CSR_TPIE = 0x4;           // Tamper Pin interrupt enable, Read-write
    static constexpr uint32_t CSR_TEF = 0x100;          // Tamper Event Flag, Read-only
    static constexpr uint32_t CSR_TIF = 0x200;          // Tamper Interrupt Flag, Read-only
    static const uint32_t CSR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR11_DR11 =                // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR11_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR12_DR12 =                // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR12_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR13_DR13 =                // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR13_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR14_D14 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR14_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR15_D15 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR15_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR16_D16 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR16_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR17_D17 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR17_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR18_D18 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR18_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR19_D19 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR19_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR20_D20 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR20_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR21_D21 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR21_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR22_D22 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR22_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR23_D23 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR23_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR24_D24 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR24_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR25_D25 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR25_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR26_D26 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR26_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR27_D27 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR27_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR28_D28 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR28_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR29_D29 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR29_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR30_D30 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR30_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR31_D31 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR31_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR32_D32 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR32_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR33_D33 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR33_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR34_D34 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR34_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR35_D35 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR35_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR36_D36 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR36_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR37_D37 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR37_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR38_D38 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR38_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR39_D39 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR39_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR40_D40 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR40_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR41_D41 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR41_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR42_D42 =                 // Backup data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR42_RESET_VALUE = 0x0;
};

static bkp_t& BKP = *reinterpret_cast<bkp_t*>(0x40006c00);

#define HAVE_PERIPHERAL_BKP


////
//
//    Independent watchdog
//
////

struct iwdg_t
{
    volatile uint32_t    KR;                   // [Write-only] Key register (IWDG_KR)
    volatile uint32_t    PR;                   // [Read-write] Prescaler register (IWDG_PR)
    volatile uint32_t    RLR;                  // [Read-write] Reload register (IWDG_RLR)
    volatile uint32_t    SR;                   // [Read-only] Status register (IWDG_SR)

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
    static const uint32_t SR_RESET_VALUE = 0x0;
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
    volatile uint32_t    CR;                   // [Read-write] Control register (WWDG_CR)
    volatile uint32_t    CFR;                  // [Read-write] Configuration register (WWDG_CFR)
    volatile uint32_t    SR;                   // [Read-write] Status register (WWDG_SR)

    template<uint32_t X>
    static constexpr uint32_t CR_T =                   // 7-bit counter (MSB to LSB) (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t CR_WDGA = 0x80;          // Activation bit
    static const uint32_t CR_RESET_VALUE = 0x7f;

    template<uint32_t X>
    static constexpr uint32_t CFR_W =                   // 7-bit window value (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFR_WDGTB =               // Timer Base (2 bits)
        bit_field_t<7, 0x3>::value<X>();
    static constexpr uint32_t CFR_EWI = 0x200;          // Early Wakeup Interrupt
    static const uint32_t CFR_RESET_VALUE = 0x7f;

    static constexpr uint32_t SR_EWI = 0x1;            // Early Wakeup Interrupt
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint8_t WWDG = 0; // Window Watchdog interrupt
};

static wwdg_t& WWDG = *reinterpret_cast<wwdg_t*>(0x40002c00);

#define HAVE_PERIPHERAL_WWDG


////
//
//    Advanced timer
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
    static constexpr uint32_t DIER_TIE = 0x40;           // Trigger interrupt enable
    static constexpr uint32_t DIER_CC4IE = 0x10;         // Capture/Compare 4 interrupt enable
    static constexpr uint32_t DIER_CC3IE = 0x8;          // Capture/Compare 3 interrupt enable
    static constexpr uint32_t DIER_CC2IE = 0x4;          // Capture/Compare 2 interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static constexpr uint32_t DIER_BIE = 0x80;           // Break interrupt enable
    static constexpr uint32_t DIER_COMIE = 0x20;         // COM interrupt enable
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
    static constexpr uint32_t CCMR1_IC2F =                // Input capture 2 filter (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC2PCS =              // Input capture 2 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_ICPCS =               // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
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
    static constexpr uint32_t CCR3_CCR3 =                // Capture/Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR4_CCR4 =                // Capture/Compare value (16 bits)
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

    static constexpr uint8_t TIM1_BRK = 24; // TIM1 Break interrupt
    static constexpr uint8_t TIM1_CC = 27; // TIM1 Capture Compare interrupt
};

static tim1_t& TIM1 = *reinterpret_cast<tim1_t*>(0x40012c00);

#define HAVE_PERIPHERAL_TIM1


////
//
//    Advanced timer
//
////

struct tim8_t
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
    static constexpr uint32_t DIER_TIE = 0x40;           // Trigger interrupt enable
    static constexpr uint32_t DIER_CC4IE = 0x10;         // Capture/Compare 4 interrupt enable
    static constexpr uint32_t DIER_CC3IE = 0x8;          // Capture/Compare 3 interrupt enable
    static constexpr uint32_t DIER_CC2IE = 0x4;          // Capture/Compare 2 interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static constexpr uint32_t DIER_BIE = 0x80;           // Break interrupt enable
    static constexpr uint32_t DIER_COMIE = 0x20;         // COM interrupt enable
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
    static constexpr uint32_t CCMR1_IC2F =                // Input capture 2 filter (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC2PCS =              // Input capture 2 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_ICPCS =               // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
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
    static constexpr uint32_t CCR3_CCR3 =                // Capture/Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR4_CCR4 =                // Capture/Compare value (16 bits)
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

    static constexpr uint8_t TIM8_BRK = 43; // TIM8 Break interrupt
    static constexpr uint8_t TIM8_CC = 46; // TIM8 Capture Compare interrupt
    static constexpr uint8_t TIM8_TRG_COM = 45; // TIM8 Trigger and Commutation interrupts
    static constexpr uint8_t TIM8_UP = 44; // TIM8 Update interrupt
};

static tim8_t& TIM8 = *reinterpret_cast<tim8_t*>(0x40013400);

#define HAVE_PERIPHERAL_TIM8


////
//
//    General purpose timer
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
    static constexpr uint32_t CCMR2_O24CE = 0x8000;       // Output compare 4 clear enable
    static constexpr uint32_t CCMR2_OC3CE = 0x80;         // Output compare 3 clear enable
    static constexpr uint32_t CCMR2_OC3FE = 0x4;          // Output compare 3 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC3M =                // Output compare 3 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC3PE = 0x8;          // Output compare 3 preload enable
    static constexpr uint32_t CCMR2_OC4FE = 0x400;        // Output compare 4 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC4M =                // Output compare 4 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC4PE = 0x800;        // Output compare 4 preload enable
    static const uint32_t CCMR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC4P = 0x2000;        // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC4E = 0x1000;        // Capture/Compare 4 output enable
    static constexpr uint32_t CCER_CC3P = 0x200;         // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC3E = 0x100;         // Capture/Compare 3 output enable
    static constexpr uint32_t CCER_CC2P = 0x20;          // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2E = 0x10;          // Capture/Compare 2 output enable
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
    static constexpr uint32_t CCR2_CCR2 =                // Capture/Compare 2 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR3_CCR3 =                // Capture/Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR4_CCR4 =                // Capture/Compare value (16 bits)
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
    static constexpr uint32_t DMAR_DMAB =                // DMA register for burst accesses (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM2 = 28; // TIM2 global interrupt
};

static tim2_t& TIM2 = *reinterpret_cast<tim2_t*>(0x40000000);

#define HAVE_PERIPHERAL_TIM2


////
//
//    General purpose timer
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
    static constexpr uint32_t CCMR2_O24CE = 0x8000;       // Output compare 4 clear enable
    static constexpr uint32_t CCMR2_OC3CE = 0x80;         // Output compare 3 clear enable
    static constexpr uint32_t CCMR2_OC3FE = 0x4;          // Output compare 3 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC3M =                // Output compare 3 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC3PE = 0x8;          // Output compare 3 preload enable
    static constexpr uint32_t CCMR2_OC4FE = 0x400;        // Output compare 4 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC4M =                // Output compare 4 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC4PE = 0x800;        // Output compare 4 preload enable
    static const uint32_t CCMR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC4P = 0x2000;        // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC4E = 0x1000;        // Capture/Compare 4 output enable
    static constexpr uint32_t CCER_CC3P = 0x200;         // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC3E = 0x100;         // Capture/Compare 3 output enable
    static constexpr uint32_t CCER_CC2P = 0x20;          // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2E = 0x10;          // Capture/Compare 2 output enable
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
    static constexpr uint32_t CCR2_CCR2 =                // Capture/Compare 2 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR3_CCR3 =                // Capture/Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR4_CCR4 =                // Capture/Compare value (16 bits)
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
    static constexpr uint32_t DMAR_DMAB =                // DMA register for burst accesses (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM3 = 29; // TIM3 global interrupt
};

static tim3_t& TIM3 = *reinterpret_cast<tim3_t*>(0x40000400);

#define HAVE_PERIPHERAL_TIM3


////
//
//    General purpose timer
//
////

struct tim4_t
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
    static constexpr uint32_t CCMR2_O24CE = 0x8000;       // Output compare 4 clear enable
    static constexpr uint32_t CCMR2_OC3CE = 0x80;         // Output compare 3 clear enable
    static constexpr uint32_t CCMR2_OC3FE = 0x4;          // Output compare 3 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC3M =                // Output compare 3 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC3PE = 0x8;          // Output compare 3 preload enable
    static constexpr uint32_t CCMR2_OC4FE = 0x400;        // Output compare 4 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC4M =                // Output compare 4 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC4PE = 0x800;        // Output compare 4 preload enable
    static const uint32_t CCMR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC4P = 0x2000;        // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC4E = 0x1000;        // Capture/Compare 4 output enable
    static constexpr uint32_t CCER_CC3P = 0x200;         // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC3E = 0x100;         // Capture/Compare 3 output enable
    static constexpr uint32_t CCER_CC2P = 0x20;          // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2E = 0x10;          // Capture/Compare 2 output enable
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
    static constexpr uint32_t CCR2_CCR2 =                // Capture/Compare 2 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR3_CCR3 =                // Capture/Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR4_CCR4 =                // Capture/Compare value (16 bits)
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
    static constexpr uint32_t DMAR_DMAB =                // DMA register for burst accesses (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM4 = 30; // TIM4 global interrupt
};

static tim4_t& TIM4 = *reinterpret_cast<tim4_t*>(0x40000800);

#define HAVE_PERIPHERAL_TIM4


////
//
//    General purpose timer
//
////

struct tim5_t
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
    static constexpr uint32_t CCMR2_O24CE = 0x8000;       // Output compare 4 clear enable
    static constexpr uint32_t CCMR2_OC3CE = 0x80;         // Output compare 3 clear enable
    static constexpr uint32_t CCMR2_OC3FE = 0x4;          // Output compare 3 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC3M =                // Output compare 3 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC3PE = 0x8;          // Output compare 3 preload enable
    static constexpr uint32_t CCMR2_OC4FE = 0x400;        // Output compare 4 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC4M =                // Output compare 4 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC4PE = 0x800;        // Output compare 4 preload enable
    static const uint32_t CCMR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC4P = 0x2000;        // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC4E = 0x1000;        // Capture/Compare 4 output enable
    static constexpr uint32_t CCER_CC3P = 0x200;         // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC3E = 0x100;         // Capture/Compare 3 output enable
    static constexpr uint32_t CCER_CC2P = 0x20;          // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2E = 0x10;          // Capture/Compare 2 output enable
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
    static constexpr uint32_t CCR2_CCR2 =                // Capture/Compare 2 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR3_CCR3 =                // Capture/Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR4_CCR4 =                // Capture/Compare value (16 bits)
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
    static constexpr uint32_t DMAR_DMAB =                // DMA register for burst accesses (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM5 = 50; // TIM5 global interrupt
};

static tim5_t& TIM5 = *reinterpret_cast<tim5_t*>(0x40000c00);

#define HAVE_PERIPHERAL_TIM5


////
//
//    General purpose timer
//
////

struct tim9_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMCR;                 // [Read-write] slave mode control register
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1;                // [Read-write] capture/compare mode register 1 (output mode)
    reserved_t<1>        _0;
    volatile uint32_t    CCER;                 // [Read-write] capture/compare enable register
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    reserved_t<1>        _1;
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    volatile uint32_t    CCR2;                 // [Read-write] capture/compare register 2

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
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

    static constexpr uint32_t SMCR_MSM = 0x80;           // Master/Slave mode
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS =                  // Trigger selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMCR_SMS =                 // Slave mode selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t SMCR_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_TIE = 0x40;           // Trigger interrupt enable
    static constexpr uint32_t DIER_CC2IE = 0x4;          // Capture/Compare 2 interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/compare 2 overcapture flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/Compare 2 interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_TG = 0x40;            // Trigger generation
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
    static constexpr uint32_t CCR2_CCR2 =                // Capture/Compare 2 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR2_RESET_VALUE = 0x0;
};

static tim9_t& TIM9 = *reinterpret_cast<tim9_t*>(0x40014c00);

#define HAVE_PERIPHERAL_TIM9


////
//
//    General purpose timer
//
////

struct tim12_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMCR;                 // [Read-write] slave mode control register
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1;                // [Read-write] capture/compare mode register 1 (output mode)
    reserved_t<1>        _0;
    volatile uint32_t    CCER;                 // [Read-write] capture/compare enable register
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    reserved_t<1>        _1;
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    volatile uint32_t    CCR2;                 // [Read-write] capture/compare register 2

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
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

    static constexpr uint32_t SMCR_MSM = 0x80;           // Master/Slave mode
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS =                  // Trigger selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMCR_SMS =                 // Slave mode selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t SMCR_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_TIE = 0x40;           // Trigger interrupt enable
    static constexpr uint32_t DIER_CC2IE = 0x4;          // Capture/Compare 2 interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/compare 2 overcapture flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/Compare 2 interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_TG = 0x40;            // Trigger generation
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
    static constexpr uint32_t CCR2_CCR2 =                // Capture/Compare 2 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR2_RESET_VALUE = 0x0;
};

static tim12_t& TIM12 = *reinterpret_cast<tim12_t*>(0x40001800);

#define HAVE_PERIPHERAL_TIM12


////
//
//    General purpose timer
//
////

struct tim10_t
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
    reserved_t<1>        _2;
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_MMS =                 // Master mode selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

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

    static constexpr uint8_t TIM1_UP = 25; // TIM1 Update interrupt
};

static tim10_t& TIM10 = *reinterpret_cast<tim10_t*>(0x40015000);

#define HAVE_PERIPHERAL_TIM10


////
//
//    General purpose timer
//
////

struct tim11_t
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
    reserved_t<1>        _2;
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_MMS =                 // Master mode selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

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

    static constexpr uint8_t TIM1_TRG_COM = 26; // TIM1 Trigger and Commutation interrupts
};

static tim11_t& TIM11 = *reinterpret_cast<tim11_t*>(0x40015400);

#define HAVE_PERIPHERAL_TIM11


////
//
//    General purpose timer
//
////

struct tim13_t
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
    reserved_t<1>        _2;
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_MMS =                 // Master mode selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

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
};

static tim13_t& TIM13 = *reinterpret_cast<tim13_t*>(0x40001c00);

#define HAVE_PERIPHERAL_TIM13


////
//
//    General purpose timer
//
////

struct tim14_t
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
    reserved_t<1>        _2;
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1

    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_MMS =                 // Master mode selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

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
};

static tim14_t& TIM14 = *reinterpret_cast<tim14_t*>(0x40002000);

#define HAVE_PERIPHERAL_TIM14


////
//
//    Basic timer
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

    static constexpr uint8_t TIM6 = 54; // TIM6 global interrupt
};

static tim6_t& TIM6 = *reinterpret_cast<tim6_t*>(0x40001000);

#define HAVE_PERIPHERAL_TIM6


////
//
//    Basic timer
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

    static constexpr uint8_t TIM7 = 55; // TIM7 global interrupt
};

static tim7_t& TIM7 = *reinterpret_cast<tim7_t*>(0x40001400);

#define HAVE_PERIPHERAL_TIM7


////
//
//    Inter integrated circuit
//
////

struct i2c1_t
{
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    OAR1;                 // [Read-write] Own address register 1
    volatile uint32_t    OAR2;                 // [Read-write] Own address register 2
    volatile uint32_t    DR;                   // [Read-write] Data register
    volatile uint32_t    SR1;                  // Status register 1
    volatile uint32_t    SR2;                  // [Read-only] Status register 2
    volatile uint32_t    CCR;                  // [Read-write] Clock control register
    volatile uint32_t    TRISE;                // [Read-write] TRISE register

    static constexpr uint32_t CR1_SWRST = 0x8000;       // Software reset
    static constexpr uint32_t CR1_ALERT = 0x2000;       // SMBus alert
    static constexpr uint32_t CR1_PEC = 0x1000;         // Packet error checking
    static constexpr uint32_t CR1_POS = 0x800;          // Acknowledge/PEC Position (for data reception)
    static constexpr uint32_t CR1_ACK = 0x400;          // Acknowledge enable
    static constexpr uint32_t CR1_STOP = 0x200;         // Stop generation
    static constexpr uint32_t CR1_START = 0x100;        // Start generation
    static constexpr uint32_t CR1_NOSTRETCH = 0x80;     // Clock stretching disable (Slave mode)
    static constexpr uint32_t CR1_ENGC = 0x40;          // General call enable
    static constexpr uint32_t CR1_ENPEC = 0x20;         // PEC enable
    static constexpr uint32_t CR1_ENARP = 0x10;         // ARP enable
    static constexpr uint32_t CR1_SMBTYPE = 0x8;        // SMBus type
    static constexpr uint32_t CR1_SMBUS = 0x2;          // SMBus mode
    static constexpr uint32_t CR1_PE = 0x1;             // Peripheral enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_LAST = 0x1000;        // DMA last transfer
    static constexpr uint32_t CR2_DMAEN = 0x800;        // DMA requests enable
    static constexpr uint32_t CR2_ITBUFEN = 0x400;      // Buffer interrupt enable
    static constexpr uint32_t CR2_ITEVTEN = 0x200;      // Event interrupt enable
    static constexpr uint32_t CR2_ITERREN = 0x100;      // Error interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR2_FREQ =                // Peripheral clock frequency (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t OAR1_ADDMODE = 0x8000;     // Addressing mode (slave mode)
    template<uint32_t X>
    static constexpr uint32_t OAR1_ADD10 =               // Interface address (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OAR1_ADD7 =                // Interface address (7 bits)
        bit_field_t<1, 0x7f>::value<X>();
    static constexpr uint32_t OAR1_ADD0 = 0x1;           // Interface address
    static const uint32_t OAR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OAR2_ADD2 =                // Interface address (7 bits)
        bit_field_t<1, 0x7f>::value<X>();
    static constexpr uint32_t OAR2_ENDUAL = 0x1;         // Dual addressing mode enable
    static const uint32_t OAR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR_DR =                  // 8-bit data register (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR1_SMBALERT = 0x8000;    // SMBus alert, Read-write
    static constexpr uint32_t SR1_TIMEOUT = 0x4000;     // Timeout or Tlow error, Read-write
    static constexpr uint32_t SR1_PECERR = 0x1000;      // PEC Error in reception, Read-write
    static constexpr uint32_t SR1_OVR = 0x800;          // Overrun/Underrun, Read-write
    static constexpr uint32_t SR1_AF = 0x400;           // Acknowledge failure, Read-write
    static constexpr uint32_t SR1_ARLO = 0x200;         // Arbitration lost (master mode), Read-write
    static constexpr uint32_t SR1_BERR = 0x100;         // Bus error, Read-write
    static constexpr uint32_t SR1_TxE = 0x80;           // Data register empty (transmitters), Read-only
    static constexpr uint32_t SR1_RxNE = 0x40;          // Data register not empty (receivers), Read-only
    static constexpr uint32_t SR1_STOPF = 0x10;         // Stop detection (slave mode), Read-only
    static constexpr uint32_t SR1_ADD10 = 0x8;          // 10-bit header sent (Master mode), Read-only
    static constexpr uint32_t SR1_BTF = 0x4;            // Byte transfer finished, Read-only
    static constexpr uint32_t SR1_ADDR = 0x2;           // Address sent (master mode)/matched (slave mode), Read-only
    static constexpr uint32_t SR1_SB = 0x1;             // Start bit (Master mode), Read-only
    static const uint32_t SR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SR2_PEC =                 // acket error checking register (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static constexpr uint32_t SR2_DUALF = 0x80;         // Dual flag (Slave mode)
    static constexpr uint32_t SR2_SMBHOST = 0x40;       // SMBus host header (Slave mode)
    static constexpr uint32_t SR2_SMBDEFAULT = 0x20;    // SMBus device default address (Slave mode)
    static constexpr uint32_t SR2_GENCALL = 0x10;       // General call address (Slave mode)
    static constexpr uint32_t SR2_TRA = 0x4;            // Transmitter/receiver
    static constexpr uint32_t SR2_BUSY = 0x2;           // Bus busy
    static constexpr uint32_t SR2_MSL = 0x1;            // Master/slave
    static const uint32_t SR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR_F_S = 0x8000;         // I2C master mode selection
    static constexpr uint32_t CCR_DUTY = 0x4000;        // Fast mode duty cycle
    template<uint32_t X>
    static constexpr uint32_t CCR_CCR =                 // Clock control register in Fast/Standard mode (Master mode) (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t CCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TRISE_TRISE =               // Maximum rise time in Fast/Standard mode (Master mode) (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    static const uint32_t TRISE_RESET_VALUE = 0x2;

    static constexpr uint8_t I2C1_ER = 32; // I2C1 error interrupt
    static constexpr uint8_t I2C1_EV = 31; // I2C1 event interrupt
};

static i2c1_t& I2C1 = *reinterpret_cast<i2c1_t*>(0x40005400);

#define HAVE_PERIPHERAL_I2C1


////
//
//    Inter integrated circuit
//
////

struct i2c2_t
{
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    OAR1;                 // [Read-write] Own address register 1
    volatile uint32_t    OAR2;                 // [Read-write] Own address register 2
    volatile uint32_t    DR;                   // [Read-write] Data register
    volatile uint32_t    SR1;                  // Status register 1
    volatile uint32_t    SR2;                  // [Read-only] Status register 2
    volatile uint32_t    CCR;                  // [Read-write] Clock control register
    volatile uint32_t    TRISE;                // [Read-write] TRISE register

    static constexpr uint32_t CR1_SWRST = 0x8000;       // Software reset
    static constexpr uint32_t CR1_ALERT = 0x2000;       // SMBus alert
    static constexpr uint32_t CR1_PEC = 0x1000;         // Packet error checking
    static constexpr uint32_t CR1_POS = 0x800;          // Acknowledge/PEC Position (for data reception)
    static constexpr uint32_t CR1_ACK = 0x400;          // Acknowledge enable
    static constexpr uint32_t CR1_STOP = 0x200;         // Stop generation
    static constexpr uint32_t CR1_START = 0x100;        // Start generation
    static constexpr uint32_t CR1_NOSTRETCH = 0x80;     // Clock stretching disable (Slave mode)
    static constexpr uint32_t CR1_ENGC = 0x40;          // General call enable
    static constexpr uint32_t CR1_ENPEC = 0x20;         // PEC enable
    static constexpr uint32_t CR1_ENARP = 0x10;         // ARP enable
    static constexpr uint32_t CR1_SMBTYPE = 0x8;        // SMBus type
    static constexpr uint32_t CR1_SMBUS = 0x2;          // SMBus mode
    static constexpr uint32_t CR1_PE = 0x1;             // Peripheral enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_LAST = 0x1000;        // DMA last transfer
    static constexpr uint32_t CR2_DMAEN = 0x800;        // DMA requests enable
    static constexpr uint32_t CR2_ITBUFEN = 0x400;      // Buffer interrupt enable
    static constexpr uint32_t CR2_ITEVTEN = 0x200;      // Event interrupt enable
    static constexpr uint32_t CR2_ITERREN = 0x100;      // Error interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR2_FREQ =                // Peripheral clock frequency (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t OAR1_ADDMODE = 0x8000;     // Addressing mode (slave mode)
    template<uint32_t X>
    static constexpr uint32_t OAR1_ADD10 =               // Interface address (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OAR1_ADD7 =                // Interface address (7 bits)
        bit_field_t<1, 0x7f>::value<X>();
    static constexpr uint32_t OAR1_ADD0 = 0x1;           // Interface address
    static const uint32_t OAR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OAR2_ADD2 =                // Interface address (7 bits)
        bit_field_t<1, 0x7f>::value<X>();
    static constexpr uint32_t OAR2_ENDUAL = 0x1;         // Dual addressing mode enable
    static const uint32_t OAR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR_DR =                  // 8-bit data register (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR1_SMBALERT = 0x8000;    // SMBus alert, Read-write
    static constexpr uint32_t SR1_TIMEOUT = 0x4000;     // Timeout or Tlow error, Read-write
    static constexpr uint32_t SR1_PECERR = 0x1000;      // PEC Error in reception, Read-write
    static constexpr uint32_t SR1_OVR = 0x800;          // Overrun/Underrun, Read-write
    static constexpr uint32_t SR1_AF = 0x400;           // Acknowledge failure, Read-write
    static constexpr uint32_t SR1_ARLO = 0x200;         // Arbitration lost (master mode), Read-write
    static constexpr uint32_t SR1_BERR = 0x100;         // Bus error, Read-write
    static constexpr uint32_t SR1_TxE = 0x80;           // Data register empty (transmitters), Read-only
    static constexpr uint32_t SR1_RxNE = 0x40;          // Data register not empty (receivers), Read-only
    static constexpr uint32_t SR1_STOPF = 0x10;         // Stop detection (slave mode), Read-only
    static constexpr uint32_t SR1_ADD10 = 0x8;          // 10-bit header sent (Master mode), Read-only
    static constexpr uint32_t SR1_BTF = 0x4;            // Byte transfer finished, Read-only
    static constexpr uint32_t SR1_ADDR = 0x2;           // Address sent (master mode)/matched (slave mode), Read-only
    static constexpr uint32_t SR1_SB = 0x1;             // Start bit (Master mode), Read-only
    static const uint32_t SR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SR2_PEC =                 // acket error checking register (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static constexpr uint32_t SR2_DUALF = 0x80;         // Dual flag (Slave mode)
    static constexpr uint32_t SR2_SMBHOST = 0x40;       // SMBus host header (Slave mode)
    static constexpr uint32_t SR2_SMBDEFAULT = 0x20;    // SMBus device default address (Slave mode)
    static constexpr uint32_t SR2_GENCALL = 0x10;       // General call address (Slave mode)
    static constexpr uint32_t SR2_TRA = 0x4;            // Transmitter/receiver
    static constexpr uint32_t SR2_BUSY = 0x2;           // Bus busy
    static constexpr uint32_t SR2_MSL = 0x1;            // Master/slave
    static const uint32_t SR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR_F_S = 0x8000;         // I2C master mode selection
    static constexpr uint32_t CCR_DUTY = 0x4000;        // Fast mode duty cycle
    template<uint32_t X>
    static constexpr uint32_t CCR_CCR =                 // Clock control register in Fast/Standard mode (Master mode) (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t CCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TRISE_TRISE =               // Maximum rise time in Fast/Standard mode (Master mode) (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    static const uint32_t TRISE_RESET_VALUE = 0x2;

    static constexpr uint8_t I2C2_ER = 34; // I2C2 error interrupt
    static constexpr uint8_t I2C2_EV = 33; // I2C2 event interrupt
};

static i2c2_t& I2C2 = *reinterpret_cast<i2c2_t*>(0x40005800);

#define HAVE_PERIPHERAL_I2C2


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

    static constexpr uint32_t CR2_TXEIE = 0x80;         // Tx buffer empty interrupt enable
    static constexpr uint32_t CR2_RXNEIE = 0x40;        // RX buffer not empty interrupt enable
    static constexpr uint32_t CR2_ERRIE = 0x20;         // Error interrupt enable
    static constexpr uint32_t CR2_SSOE = 0x4;           // SS output enable
    static constexpr uint32_t CR2_TXDMAEN = 0x2;        // Tx buffer DMA enable
    static constexpr uint32_t CR2_RXDMAEN = 0x1;        // Rx buffer DMA enable
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_BSY = 0x80;           // Busy flag, Read-only
    static constexpr uint32_t SR_OVR = 0x40;           // Overrun flag, Read-only
    static constexpr uint32_t SR_MODF = 0x20;          // Mode fault, Read-only
    static constexpr uint32_t SR_CRCERR = 0x10;        // CRC error flag, Read-write
    static constexpr uint32_t SR_UDR = 0x8;            // Underrun flag, Read-only
    static constexpr uint32_t SR_CHSIDE = 0x4;         // Channel side, Read-only
    static constexpr uint32_t SR_TXE = 0x2;            // Transmit buffer empty, Read-only
    static constexpr uint32_t SR_RXNE = 0x1;           // Receive buffer not empty, Read-only
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
    static const uint32_t I2SPR_RESET_VALUE = 0xa;

    static constexpr uint8_t SPI1 = 35; // SPI1 global interrupt
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

    static constexpr uint32_t CR2_TXEIE = 0x80;         // Tx buffer empty interrupt enable
    static constexpr uint32_t CR2_RXNEIE = 0x40;        // RX buffer not empty interrupt enable
    static constexpr uint32_t CR2_ERRIE = 0x20;         // Error interrupt enable
    static constexpr uint32_t CR2_SSOE = 0x4;           // SS output enable
    static constexpr uint32_t CR2_TXDMAEN = 0x2;        // Tx buffer DMA enable
    static constexpr uint32_t CR2_RXDMAEN = 0x1;        // Rx buffer DMA enable
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_BSY = 0x80;           // Busy flag, Read-only
    static constexpr uint32_t SR_OVR = 0x40;           // Overrun flag, Read-only
    static constexpr uint32_t SR_MODF = 0x20;          // Mode fault, Read-only
    static constexpr uint32_t SR_CRCERR = 0x10;        // CRC error flag, Read-write
    static constexpr uint32_t SR_UDR = 0x8;            // Underrun flag, Read-only
    static constexpr uint32_t SR_CHSIDE = 0x4;         // Channel side, Read-only
    static constexpr uint32_t SR_TXE = 0x2;            // Transmit buffer empty, Read-only
    static constexpr uint32_t SR_RXNE = 0x1;           // Receive buffer not empty, Read-only
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
    static const uint32_t I2SPR_RESET_VALUE = 0xa;

    static constexpr uint8_t SPI2 = 36; // SPI2 global interrupt
};

static spi2_t& SPI2 = *reinterpret_cast<spi2_t*>(0x40003800);

#define HAVE_PERIPHERAL_SPI2


////
//
//    Serial peripheral interface
//
////

struct spi3_t
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

    static constexpr uint32_t CR2_TXEIE = 0x80;         // Tx buffer empty interrupt enable
    static constexpr uint32_t CR2_RXNEIE = 0x40;        // RX buffer not empty interrupt enable
    static constexpr uint32_t CR2_ERRIE = 0x20;         // Error interrupt enable
    static constexpr uint32_t CR2_SSOE = 0x4;           // SS output enable
    static constexpr uint32_t CR2_TXDMAEN = 0x2;        // Tx buffer DMA enable
    static constexpr uint32_t CR2_RXDMAEN = 0x1;        // Rx buffer DMA enable
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_BSY = 0x80;           // Busy flag, Read-only
    static constexpr uint32_t SR_OVR = 0x40;           // Overrun flag, Read-only
    static constexpr uint32_t SR_MODF = 0x20;          // Mode fault, Read-only
    static constexpr uint32_t SR_CRCERR = 0x10;        // CRC error flag, Read-write
    static constexpr uint32_t SR_UDR = 0x8;            // Underrun flag, Read-only
    static constexpr uint32_t SR_CHSIDE = 0x4;         // Channel side, Read-only
    static constexpr uint32_t SR_TXE = 0x2;            // Transmit buffer empty, Read-only
    static constexpr uint32_t SR_RXNE = 0x1;           // Receive buffer not empty, Read-only
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
    static const uint32_t I2SPR_RESET_VALUE = 0xa;

    static constexpr uint8_t SPI3 = 51; // SPI3 global interrupt
};

static spi3_t& SPI3 = *reinterpret_cast<spi3_t*>(0x40003c00);

#define HAVE_PERIPHERAL_SPI3


////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

struct usart1_t
{
    volatile uint32_t    SR;                   // Status register
    volatile uint32_t    DR;                   // [Read-write] Data register
    volatile uint32_t    BRR;                  // [Read-write] Baud rate register
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    CR3;                  // [Read-write] Control register 3
    volatile uint32_t    GTPR;                 // [Read-write] Guard time and prescaler register

    static constexpr uint32_t SR_CTS = 0x200;          // CTS flag, Read-write
    static constexpr uint32_t SR_LBD = 0x100;          // LIN break detection flag, Read-write
    static constexpr uint32_t SR_TXE = 0x80;           // Transmit data register empty, Read-only
    static constexpr uint32_t SR_TC = 0x40;            // Transmission complete, Read-write
    static constexpr uint32_t SR_RXNE = 0x20;          // Read data register not empty, Read-write
    static constexpr uint32_t SR_IDLE = 0x10;          // IDLE line detected, Read-only
    static constexpr uint32_t SR_ORE = 0x8;            // Overrun error, Read-only
    static constexpr uint32_t SR_NE = 0x4;             // Noise error flag, Read-only
    static constexpr uint32_t SR_FE = 0x2;             // Framing error, Read-only
    static constexpr uint32_t SR_PE = 0x1;             // Parity error, Read-only
    static const uint32_t SR_RESET_VALUE = 0xc0;

    template<uint32_t X>
    static constexpr uint32_t DR_DR =                  // Data value (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Mantissa =        // mantissa of USARTDIV (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // fraction of USARTDIV (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR1_UE = 0x2000;          // USART enable
    static constexpr uint32_t CR1_M = 0x1000;           // Word length
    static constexpr uint32_t CR1_WAKE = 0x800;         // Wakeup method
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // TXE interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_RWU = 0x2;            // Receiver wakeup
    static constexpr uint32_t CR1_SBK = 0x1;            // Send break
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_LINEN = 0x4000;       // LIN mode enable
    template<uint32_t X>
    static constexpr uint32_t CR2_STOP =                // STOP bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CR2_CLKEN = 0x800;        // Clock enable
    static constexpr uint32_t CR2_CPOL = 0x400;         // Clock polarity
    static constexpr uint32_t CR2_CPHA = 0x200;         // Clock phase
    static constexpr uint32_t CR2_LBCL = 0x100;         // Last bit clock pulse
    static constexpr uint32_t CR2_LBDIE = 0x40;         // LIN break detection interrupt enable
    static constexpr uint32_t CR2_LBDL = 0x20;          // lin break detection length
    template<uint32_t X>
    static constexpr uint32_t CR2_ADD =                 // Address of the USART node (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

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
    static constexpr uint32_t GTPR_GT =                  // Guard time value (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t GTPR_PSC =                 // Prescaler value (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    static constexpr uint8_t USART1 = 37; // USART1 global interrupt
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
    volatile uint32_t    SR;                   // Status register
    volatile uint32_t    DR;                   // [Read-write] Data register
    volatile uint32_t    BRR;                  // [Read-write] Baud rate register
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    CR3;                  // [Read-write] Control register 3
    volatile uint32_t    GTPR;                 // [Read-write] Guard time and prescaler register

    static constexpr uint32_t SR_CTS = 0x200;          // CTS flag, Read-write
    static constexpr uint32_t SR_LBD = 0x100;          // LIN break detection flag, Read-write
    static constexpr uint32_t SR_TXE = 0x80;           // Transmit data register empty, Read-only
    static constexpr uint32_t SR_TC = 0x40;            // Transmission complete, Read-write
    static constexpr uint32_t SR_RXNE = 0x20;          // Read data register not empty, Read-write
    static constexpr uint32_t SR_IDLE = 0x10;          // IDLE line detected, Read-only
    static constexpr uint32_t SR_ORE = 0x8;            // Overrun error, Read-only
    static constexpr uint32_t SR_NE = 0x4;             // Noise error flag, Read-only
    static constexpr uint32_t SR_FE = 0x2;             // Framing error, Read-only
    static constexpr uint32_t SR_PE = 0x1;             // Parity error, Read-only
    static const uint32_t SR_RESET_VALUE = 0xc0;

    template<uint32_t X>
    static constexpr uint32_t DR_DR =                  // Data value (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Mantissa =        // mantissa of USARTDIV (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // fraction of USARTDIV (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR1_UE = 0x2000;          // USART enable
    static constexpr uint32_t CR1_M = 0x1000;           // Word length
    static constexpr uint32_t CR1_WAKE = 0x800;         // Wakeup method
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // TXE interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_RWU = 0x2;            // Receiver wakeup
    static constexpr uint32_t CR1_SBK = 0x1;            // Send break
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_LINEN = 0x4000;       // LIN mode enable
    template<uint32_t X>
    static constexpr uint32_t CR2_STOP =                // STOP bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CR2_CLKEN = 0x800;        // Clock enable
    static constexpr uint32_t CR2_CPOL = 0x400;         // Clock polarity
    static constexpr uint32_t CR2_CPHA = 0x200;         // Clock phase
    static constexpr uint32_t CR2_LBCL = 0x100;         // Last bit clock pulse
    static constexpr uint32_t CR2_LBDIE = 0x40;         // LIN break detection interrupt enable
    static constexpr uint32_t CR2_LBDL = 0x20;          // lin break detection length
    template<uint32_t X>
    static constexpr uint32_t CR2_ADD =                 // Address of the USART node (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

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
    static constexpr uint32_t GTPR_GT =                  // Guard time value (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t GTPR_PSC =                 // Prescaler value (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    static constexpr uint8_t USART2 = 38; // USART2 global interrupt
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
    volatile uint32_t    SR;                   // Status register
    volatile uint32_t    DR;                   // [Read-write] Data register
    volatile uint32_t    BRR;                  // [Read-write] Baud rate register
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    CR3;                  // [Read-write] Control register 3
    volatile uint32_t    GTPR;                 // [Read-write] Guard time and prescaler register

    static constexpr uint32_t SR_CTS = 0x200;          // CTS flag, Read-write
    static constexpr uint32_t SR_LBD = 0x100;          // LIN break detection flag, Read-write
    static constexpr uint32_t SR_TXE = 0x80;           // Transmit data register empty, Read-only
    static constexpr uint32_t SR_TC = 0x40;            // Transmission complete, Read-write
    static constexpr uint32_t SR_RXNE = 0x20;          // Read data register not empty, Read-write
    static constexpr uint32_t SR_IDLE = 0x10;          // IDLE line detected, Read-only
    static constexpr uint32_t SR_ORE = 0x8;            // Overrun error, Read-only
    static constexpr uint32_t SR_NE = 0x4;             // Noise error flag, Read-only
    static constexpr uint32_t SR_FE = 0x2;             // Framing error, Read-only
    static constexpr uint32_t SR_PE = 0x1;             // Parity error, Read-only
    static const uint32_t SR_RESET_VALUE = 0xc0;

    template<uint32_t X>
    static constexpr uint32_t DR_DR =                  // Data value (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Mantissa =        // mantissa of USARTDIV (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // fraction of USARTDIV (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR1_UE = 0x2000;          // USART enable
    static constexpr uint32_t CR1_M = 0x1000;           // Word length
    static constexpr uint32_t CR1_WAKE = 0x800;         // Wakeup method
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // TXE interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_RWU = 0x2;            // Receiver wakeup
    static constexpr uint32_t CR1_SBK = 0x1;            // Send break
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_LINEN = 0x4000;       // LIN mode enable
    template<uint32_t X>
    static constexpr uint32_t CR2_STOP =                // STOP bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CR2_CLKEN = 0x800;        // Clock enable
    static constexpr uint32_t CR2_CPOL = 0x400;         // Clock polarity
    static constexpr uint32_t CR2_CPHA = 0x200;         // Clock phase
    static constexpr uint32_t CR2_LBCL = 0x100;         // Last bit clock pulse
    static constexpr uint32_t CR2_LBDIE = 0x40;         // LIN break detection interrupt enable
    static constexpr uint32_t CR2_LBDL = 0x20;          // lin break detection length
    template<uint32_t X>
    static constexpr uint32_t CR2_ADD =                 // Address of the USART node (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

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
    static constexpr uint32_t GTPR_GT =                  // Guard time value (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t GTPR_PSC =                 // Prescaler value (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t GTPR_RESET_VALUE = 0x0;

    static constexpr uint8_t USART3 = 39; // USART3 global interrupt
};

static usart3_t& USART3 = *reinterpret_cast<usart3_t*>(0x40004800);

#define HAVE_PERIPHERAL_USART3


////
//
//    Analog to digital converter
//
////

struct adc1_t
{
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMPR1;                // [Read-write] sample time register 1
    volatile uint32_t    SMPR2;                // [Read-write] sample time register 2
    volatile uint32_t    JOFR1;                // [Read-write] injected channel data offset register x
    volatile uint32_t    JOFR2;                // [Read-write] injected channel data offset register x
    volatile uint32_t    JOFR3;                // [Read-write] injected channel data offset register x
    volatile uint32_t    JOFR4;                // [Read-write] injected channel data offset register x
    volatile uint32_t    HTR;                  // [Read-write] watchdog higher threshold register
    volatile uint32_t    LTR;                  // [Read-write] watchdog lower threshold register
    volatile uint32_t    SQR1;                 // [Read-write] regular sequence register 1
    volatile uint32_t    SQR2;                 // [Read-write] regular sequence register 2
    volatile uint32_t    SQR3;                 // [Read-write] regular sequence register 3
    volatile uint32_t    JSQR;                 // [Read-write] injected sequence register
    volatile uint32_t    JDR1;                 // [Read-only] injected data register x
    volatile uint32_t    JDR2;                 // [Read-only] injected data register x
    volatile uint32_t    JDR3;                 // [Read-only] injected data register x
    volatile uint32_t    JDR4;                 // [Read-only] injected data register x
    volatile uint32_t    DR;                   // [Read-only] regular data register

    static constexpr uint32_t SR_STRT = 0x10;          // Regular channel start flag
    static constexpr uint32_t SR_JSTRT = 0x8;          // Injected channel start flag
    static constexpr uint32_t SR_JEOC = 0x4;           // Injected channel end of conversion
    static constexpr uint32_t SR_EOC = 0x2;            // Regular channel end of conversion
    static constexpr uint32_t SR_AWD = 0x1;            // Analog watchdog flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR1_AWDEN = 0x800000;     // Analog watchdog enable on regular channels
    static constexpr uint32_t CR1_JAWDEN = 0x400000;    // Analog watchdog enable on injected channels
    template<uint32_t X>
    static constexpr uint32_t CR1_DUALMOD =             // Dual mode selection (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR1_DISCNUM =             // Discontinuous mode channel count (3 bits)
        bit_field_t<13, 0x7>::value<X>();
    static constexpr uint32_t CR1_JDISCEN = 0x1000;     // Discontinuous mode on injected channels
    static constexpr uint32_t CR1_DISCEN = 0x800;       // Discontinuous mode on regular channels
    static constexpr uint32_t CR1_JAUTO = 0x400;        // Automatic injected group conversion
    static constexpr uint32_t CR1_AWDSGL = 0x200;       // Enable the watchdog on a single channel in scan mode
    static constexpr uint32_t CR1_SCAN = 0x100;         // Scan mode
    static constexpr uint32_t CR1_JEOCIE = 0x80;        // Interrupt enable for injected channels
    static constexpr uint32_t CR1_AWDIE = 0x40;         // Analog watchdog interrupt enable
    static constexpr uint32_t CR1_EOCIE = 0x20;         // Interrupt enable for EOC
    template<uint32_t X>
    static constexpr uint32_t CR1_AWDCH =               // Analog watchdog channel select bits (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_TSVREFE = 0x800000;   // Temperature sensor and VREFINT enable
    static constexpr uint32_t CR2_SWSTART = 0x400000;   // Start conversion of regular channels
    static constexpr uint32_t CR2_JSWSTART = 0x200000;  // Start conversion of injected channels
    static constexpr uint32_t CR2_EXTTRIG = 0x100000;   // External trigger conversion mode for regular channels
    template<uint32_t X>
    static constexpr uint32_t CR2_EXTSEL =              // External event select for regular group (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    static constexpr uint32_t CR2_JEXTTRIG = 0x8000;    // External trigger conversion mode for injected channels
    template<uint32_t X>
    static constexpr uint32_t CR2_JEXTSEL =             // External event select for injected group (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CR2_ALIGN = 0x800;        // Data alignment
    static constexpr uint32_t CR2_DMA = 0x100;          // Direct memory access mode
    static constexpr uint32_t CR2_RSTCAL = 0x8;         // Reset calibration
    static constexpr uint32_t CR2_CAL = 0x4;            // A/D calibration
    static constexpr uint32_t CR2_CONT = 0x2;           // Continuous conversion
    static constexpr uint32_t CR2_ADON = 0x1;           // A/D converter ON / OFF
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP10 =               // Channel 10 sample time selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP11 =               // Channel 11 sample time selection (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP12 =               // Channel 12 sample time selection (3 bits)
        bit_field_t<6, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP13 =               // Channel 13 sample time selection (3 bits)
        bit_field_t<9, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP14 =               // Channel 14 sample time selection (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP15 =               // Channel 15 sample time selection (3 bits)
        bit_field_t<15, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP16 =               // Channel 16 sample time selection (3 bits)
        bit_field_t<18, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP17 =               // Channel 17 sample time selection (3 bits)
        bit_field_t<21, 0x7>::value<X>();
    static const uint32_t SMPR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP0 =                // Channel 0 sample time selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP1 =                // Channel 1 sample time selection (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP2 =                // Channel 2 sample time selection (3 bits)
        bit_field_t<6, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP3 =                // Channel 3 sample time selection (3 bits)
        bit_field_t<9, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP4 =                // Channel 4 sample time selection (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP5 =                // Channel 5 sample time selection (3 bits)
        bit_field_t<15, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP6 =                // Channel 6 sample time selection (3 bits)
        bit_field_t<18, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP7 =                // Channel 7 sample time selection (3 bits)
        bit_field_t<21, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP8 =                // Channel 8 sample time selection (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP9 =                // Channel 9 sample time selection (3 bits)
        bit_field_t<27, 0x7>::value<X>();
    static const uint32_t SMPR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JOFR1_JOFFSET1 =            // Data offset for injected channel x (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t JOFR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JOFR2_JOFFSET2 =            // Data offset for injected channel x (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t JOFR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JOFR3_JOFFSET3 =            // Data offset for injected channel x (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t JOFR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JOFR4_JOFFSET4 =            // Data offset for injected channel x (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t JOFR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HTR_HT =                  // Analog watchdog higher threshold (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t HTR_RESET_VALUE = 0xfff;

    template<uint32_t X>
    static constexpr uint32_t LTR_LT =                  // Analog watchdog lower threshold (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t LTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR1_L =                   // Regular channel sequence length (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ16 =                // 16th conversion in regular sequence (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ15 =                // 15th conversion in regular sequence (5 bits)
        bit_field_t<10, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ14 =                // 14th conversion in regular sequence (5 bits)
        bit_field_t<5, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ13 =                // 13th conversion in regular sequence (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ12 =                // 12th conversion in regular sequence (5 bits)
        bit_field_t<25, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ11 =                // 11th conversion in regular sequence (5 bits)
        bit_field_t<20, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ10 =                // 10th conversion in regular sequence (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ9 =                 // 9th conversion in regular sequence (5 bits)
        bit_field_t<10, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ8 =                 // 8th conversion in regular sequence (5 bits)
        bit_field_t<5, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ7 =                 // 7th conversion in regular sequence (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ6 =                 // 6th conversion in regular sequence (5 bits)
        bit_field_t<25, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ5 =                 // 5th conversion in regular sequence (5 bits)
        bit_field_t<20, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ4 =                 // 4th conversion in regular sequence (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ3 =                 // 3rd conversion in regular sequence (5 bits)
        bit_field_t<10, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ2 =                 // 2nd conversion in regular sequence (5 bits)
        bit_field_t<5, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ1 =                 // 1st conversion in regular sequence (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JSQR_JL =                  // Injected sequence length (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ4 =                // 4th conversion in injected sequence (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ3 =                // 3rd conversion in injected sequence (5 bits)
        bit_field_t<10, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ2 =                // 2nd conversion in injected sequence (5 bits)
        bit_field_t<5, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ1 =                // 1st conversion in injected sequence (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t JSQR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR1_JDATA =               // Injected data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR2_JDATA =               // Injected data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR3_JDATA =               // Injected data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR4_JDATA =               // Injected data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR_DATA =                // Regular data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DR_ADC2DATA =            // ADC2 data (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    static constexpr uint8_t ADC1_2 = 18; // ADC1 and ADC2 global interrupt
};

static adc1_t& ADC1 = *reinterpret_cast<adc1_t*>(0x40012400);

#define HAVE_PERIPHERAL_ADC1


////
//
//    Analog to digital converter
//
////

struct adc2_t
{
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMPR1;                // [Read-write] sample time register 1
    volatile uint32_t    SMPR2;                // [Read-write] sample time register 2
    volatile uint32_t    JOFR1;                // [Read-write] injected channel data offset register x
    volatile uint32_t    JOFR2;                // [Read-write] injected channel data offset register x
    volatile uint32_t    JOFR3;                // [Read-write] injected channel data offset register x
    volatile uint32_t    JOFR4;                // [Read-write] injected channel data offset register x
    volatile uint32_t    HTR;                  // [Read-write] watchdog higher threshold register
    volatile uint32_t    LTR;                  // [Read-write] watchdog lower threshold register
    volatile uint32_t    SQR1;                 // [Read-write] regular sequence register 1
    volatile uint32_t    SQR2;                 // [Read-write] regular sequence register 2
    volatile uint32_t    SQR3;                 // [Read-write] regular sequence register 3
    volatile uint32_t    JSQR;                 // [Read-write] injected sequence register
    volatile uint32_t    JDR1;                 // [Read-only] injected data register x
    volatile uint32_t    JDR2;                 // [Read-only] injected data register x
    volatile uint32_t    JDR3;                 // [Read-only] injected data register x
    volatile uint32_t    JDR4;                 // [Read-only] injected data register x
    volatile uint32_t    DR;                   // [Read-only] regular data register

    static constexpr uint32_t SR_STRT = 0x10;          // Regular channel start flag
    static constexpr uint32_t SR_JSTRT = 0x8;          // Injected channel start flag
    static constexpr uint32_t SR_JEOC = 0x4;           // Injected channel end of conversion
    static constexpr uint32_t SR_EOC = 0x2;            // Regular channel end of conversion
    static constexpr uint32_t SR_AWD = 0x1;            // Analog watchdog flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR1_AWDEN = 0x800000;     // Analog watchdog enable on regular channels
    static constexpr uint32_t CR1_JAWDEN = 0x400000;    // Analog watchdog enable on injected channels
    template<uint32_t X>
    static constexpr uint32_t CR1_DISCNUM =             // Discontinuous mode channel count (3 bits)
        bit_field_t<13, 0x7>::value<X>();
    static constexpr uint32_t CR1_JDISCEN = 0x1000;     // Discontinuous mode on injected channels
    static constexpr uint32_t CR1_DISCEN = 0x800;       // Discontinuous mode on regular channels
    static constexpr uint32_t CR1_JAUTO = 0x400;        // Automatic injected group conversion
    static constexpr uint32_t CR1_AWDSGL = 0x200;       // Enable the watchdog on a single channel in scan mode
    static constexpr uint32_t CR1_SCAN = 0x100;         // Scan mode
    static constexpr uint32_t CR1_JEOCIE = 0x80;        // Interrupt enable for injected channels
    static constexpr uint32_t CR1_AWDIE = 0x40;         // Analog watchdog interrupt enable
    static constexpr uint32_t CR1_EOCIE = 0x20;         // Interrupt enable for EOC
    template<uint32_t X>
    static constexpr uint32_t CR1_AWDCH =               // Analog watchdog channel select bits (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_TSVREFE = 0x800000;   // Temperature sensor and VREFINT enable
    static constexpr uint32_t CR2_SWSTART = 0x400000;   // Start conversion of regular channels
    static constexpr uint32_t CR2_JSWSTART = 0x200000;  // Start conversion of injected channels
    static constexpr uint32_t CR2_EXTTRIG = 0x100000;   // External trigger conversion mode for regular channels
    template<uint32_t X>
    static constexpr uint32_t CR2_EXTSEL =              // External event select for regular group (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    static constexpr uint32_t CR2_JEXTTRIG = 0x8000;    // External trigger conversion mode for injected channels
    template<uint32_t X>
    static constexpr uint32_t CR2_JEXTSEL =             // External event select for injected group (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CR2_ALIGN = 0x800;        // Data alignment
    static constexpr uint32_t CR2_DMA = 0x100;          // Direct memory access mode
    static constexpr uint32_t CR2_RSTCAL = 0x8;         // Reset calibration
    static constexpr uint32_t CR2_CAL = 0x4;            // A/D calibration
    static constexpr uint32_t CR2_CONT = 0x2;           // Continuous conversion
    static constexpr uint32_t CR2_ADON = 0x1;           // A/D converter ON / OFF
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP10 =               // Channel 10 sample time selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP11 =               // Channel 11 sample time selection (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP12 =               // Channel 12 sample time selection (3 bits)
        bit_field_t<6, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP13 =               // Channel 13 sample time selection (3 bits)
        bit_field_t<9, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP14 =               // Channel 14 sample time selection (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP15 =               // Channel 15 sample time selection (3 bits)
        bit_field_t<15, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP16 =               // Channel 16 sample time selection (3 bits)
        bit_field_t<18, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP17 =               // Channel 17 sample time selection (3 bits)
        bit_field_t<21, 0x7>::value<X>();
    static const uint32_t SMPR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP0 =                // Channel 0 sample time selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP1 =                // Channel 1 sample time selection (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP2 =                // Channel 2 sample time selection (3 bits)
        bit_field_t<6, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP3 =                // Channel 3 sample time selection (3 bits)
        bit_field_t<9, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP4 =                // Channel 4 sample time selection (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP5 =                // Channel 5 sample time selection (3 bits)
        bit_field_t<15, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP6 =                // Channel 6 sample time selection (3 bits)
        bit_field_t<18, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP7 =                // Channel 7 sample time selection (3 bits)
        bit_field_t<21, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP8 =                // Channel 8 sample time selection (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP9 =                // Channel 9 sample time selection (3 bits)
        bit_field_t<27, 0x7>::value<X>();
    static const uint32_t SMPR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JOFR1_JOFFSET1 =            // Data offset for injected channel x (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t JOFR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JOFR2_JOFFSET2 =            // Data offset for injected channel x (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t JOFR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JOFR3_JOFFSET3 =            // Data offset for injected channel x (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t JOFR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JOFR4_JOFFSET4 =            // Data offset for injected channel x (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t JOFR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HTR_HT =                  // Analog watchdog higher threshold (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t HTR_RESET_VALUE = 0xfff;

    template<uint32_t X>
    static constexpr uint32_t LTR_LT =                  // Analog watchdog lower threshold (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t LTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR1_L =                   // Regular channel sequence length (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ16 =                // 16th conversion in regular sequence (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ15 =                // 15th conversion in regular sequence (5 bits)
        bit_field_t<10, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ14 =                // 14th conversion in regular sequence (5 bits)
        bit_field_t<5, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ13 =                // 13th conversion in regular sequence (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ12 =                // 12th conversion in regular sequence (5 bits)
        bit_field_t<25, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ11 =                // 11th conversion in regular sequence (5 bits)
        bit_field_t<20, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ10 =                // 10th conversion in regular sequence (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ9 =                 // 9th conversion in regular sequence (5 bits)
        bit_field_t<10, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ8 =                 // 8th conversion in regular sequence (5 bits)
        bit_field_t<5, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ7 =                 // 7th conversion in regular sequence (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ6 =                 // 6th conversion in regular sequence (5 bits)
        bit_field_t<25, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ5 =                 // 5th conversion in regular sequence (5 bits)
        bit_field_t<20, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ4 =                 // 4th conversion in regular sequence (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ3 =                 // 3rd conversion in regular sequence (5 bits)
        bit_field_t<10, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ2 =                 // 2nd conversion in regular sequence (5 bits)
        bit_field_t<5, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ1 =                 // 1st conversion in regular sequence (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JSQR_JL =                  // Injected sequence length (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ4 =                // 4th conversion in injected sequence (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ3 =                // 3rd conversion in injected sequence (5 bits)
        bit_field_t<10, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ2 =                // 2nd conversion in injected sequence (5 bits)
        bit_field_t<5, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ1 =                // 1st conversion in injected sequence (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t JSQR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR1_JDATA =               // Injected data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR2_JDATA =               // Injected data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR3_JDATA =               // Injected data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR4_JDATA =               // Injected data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR_DATA =                // Regular data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;
};

static adc2_t& ADC2 = *reinterpret_cast<adc2_t*>(0x40012800);

#define HAVE_PERIPHERAL_ADC2


////
//
//    Analog to digital converter
//
////

struct adc3_t
{
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    volatile uint32_t    SMPR1;                // [Read-write] sample time register 1
    volatile uint32_t    SMPR2;                // [Read-write] sample time register 2
    volatile uint32_t    JOFR1;                // [Read-write] injected channel data offset register x
    volatile uint32_t    JOFR2;                // [Read-write] injected channel data offset register x
    volatile uint32_t    JOFR3;                // [Read-write] injected channel data offset register x
    volatile uint32_t    JOFR4;                // [Read-write] injected channel data offset register x
    volatile uint32_t    HTR;                  // [Read-write] watchdog higher threshold register
    volatile uint32_t    LTR;                  // [Read-write] watchdog lower threshold register
    volatile uint32_t    SQR1;                 // [Read-write] regular sequence register 1
    volatile uint32_t    SQR2;                 // [Read-write] regular sequence register 2
    volatile uint32_t    SQR3;                 // [Read-write] regular sequence register 3
    volatile uint32_t    JSQR;                 // [Read-write] injected sequence register
    volatile uint32_t    JDR1;                 // [Read-only] injected data register x
    volatile uint32_t    JDR2;                 // [Read-only] injected data register x
    volatile uint32_t    JDR3;                 // [Read-only] injected data register x
    volatile uint32_t    JDR4;                 // [Read-only] injected data register x
    volatile uint32_t    DR;                   // [Read-only] regular data register

    static constexpr uint32_t SR_STRT = 0x10;          // Regular channel start flag
    static constexpr uint32_t SR_JSTRT = 0x8;          // Injected channel start flag
    static constexpr uint32_t SR_JEOC = 0x4;           // Injected channel end of conversion
    static constexpr uint32_t SR_EOC = 0x2;            // Regular channel end of conversion
    static constexpr uint32_t SR_AWD = 0x1;            // Analog watchdog flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR1_AWDEN = 0x800000;     // Analog watchdog enable on regular channels
    static constexpr uint32_t CR1_JAWDEN = 0x400000;    // Analog watchdog enable on injected channels
    template<uint32_t X>
    static constexpr uint32_t CR1_DISCNUM =             // Discontinuous mode channel count (3 bits)
        bit_field_t<13, 0x7>::value<X>();
    static constexpr uint32_t CR1_JDISCEN = 0x1000;     // Discontinuous mode on injected channels
    static constexpr uint32_t CR1_DISCEN = 0x800;       // Discontinuous mode on regular channels
    static constexpr uint32_t CR1_JAUTO = 0x400;        // Automatic injected group conversion
    static constexpr uint32_t CR1_AWDSGL = 0x200;       // Enable the watchdog on a single channel in scan mode
    static constexpr uint32_t CR1_SCAN = 0x100;         // Scan mode
    static constexpr uint32_t CR1_JEOCIE = 0x80;        // Interrupt enable for injected channels
    static constexpr uint32_t CR1_AWDIE = 0x40;         // Analog watchdog interrupt enable
    static constexpr uint32_t CR1_EOCIE = 0x20;         // Interrupt enable for EOC
    template<uint32_t X>
    static constexpr uint32_t CR1_AWDCH =               // Analog watchdog channel select bits (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_TSVREFE = 0x800000;   // Temperature sensor and VREFINT enable
    static constexpr uint32_t CR2_SWSTART = 0x400000;   // Start conversion of regular channels
    static constexpr uint32_t CR2_JSWSTART = 0x200000;  // Start conversion of injected channels
    static constexpr uint32_t CR2_EXTTRIG = 0x100000;   // External trigger conversion mode for regular channels
    template<uint32_t X>
    static constexpr uint32_t CR2_EXTSEL =              // External event select for regular group (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    static constexpr uint32_t CR2_JEXTTRIG = 0x8000;    // External trigger conversion mode for injected channels
    template<uint32_t X>
    static constexpr uint32_t CR2_JEXTSEL =             // External event select for injected group (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CR2_ALIGN = 0x800;        // Data alignment
    static constexpr uint32_t CR2_DMA = 0x100;          // Direct memory access mode
    static constexpr uint32_t CR2_RSTCAL = 0x8;         // Reset calibration
    static constexpr uint32_t CR2_CAL = 0x4;            // A/D calibration
    static constexpr uint32_t CR2_CONT = 0x2;           // Continuous conversion
    static constexpr uint32_t CR2_ADON = 0x1;           // A/D converter ON / OFF
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP10 =               // Channel 10 sample time selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP11 =               // Channel 11 sample time selection (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP12 =               // Channel 12 sample time selection (3 bits)
        bit_field_t<6, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP13 =               // Channel 13 sample time selection (3 bits)
        bit_field_t<9, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP14 =               // Channel 14 sample time selection (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP15 =               // Channel 15 sample time selection (3 bits)
        bit_field_t<15, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP16 =               // Channel 16 sample time selection (3 bits)
        bit_field_t<18, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP17 =               // Channel 17 sample time selection (3 bits)
        bit_field_t<21, 0x7>::value<X>();
    static const uint32_t SMPR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP0 =                // Channel 0 sample time selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP1 =                // Channel 1 sample time selection (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP2 =                // Channel 2 sample time selection (3 bits)
        bit_field_t<6, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP3 =                // Channel 3 sample time selection (3 bits)
        bit_field_t<9, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP4 =                // Channel 4 sample time selection (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP5 =                // Channel 5 sample time selection (3 bits)
        bit_field_t<15, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP6 =                // Channel 6 sample time selection (3 bits)
        bit_field_t<18, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP7 =                // Channel 7 sample time selection (3 bits)
        bit_field_t<21, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP8 =                // Channel 8 sample time selection (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP9 =                // Channel 9 sample time selection (3 bits)
        bit_field_t<27, 0x7>::value<X>();
    static const uint32_t SMPR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JOFR1_JOFFSET1 =            // Data offset for injected channel x (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t JOFR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JOFR2_JOFFSET2 =            // Data offset for injected channel x (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t JOFR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JOFR3_JOFFSET3 =            // Data offset for injected channel x (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t JOFR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JOFR4_JOFFSET4 =            // Data offset for injected channel x (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t JOFR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HTR_HT =                  // Analog watchdog higher threshold (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t HTR_RESET_VALUE = 0xfff;

    template<uint32_t X>
    static constexpr uint32_t LTR_LT =                  // Analog watchdog lower threshold (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t LTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR1_L =                   // Regular channel sequence length (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ16 =                // 16th conversion in regular sequence (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ15 =                // 15th conversion in regular sequence (5 bits)
        bit_field_t<10, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ14 =                // 14th conversion in regular sequence (5 bits)
        bit_field_t<5, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ13 =                // 13th conversion in regular sequence (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ12 =                // 12th conversion in regular sequence (5 bits)
        bit_field_t<25, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ11 =                // 11th conversion in regular sequence (5 bits)
        bit_field_t<20, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ10 =                // 10th conversion in regular sequence (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ9 =                 // 9th conversion in regular sequence (5 bits)
        bit_field_t<10, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ8 =                 // 8th conversion in regular sequence (5 bits)
        bit_field_t<5, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ7 =                 // 7th conversion in regular sequence (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ6 =                 // 6th conversion in regular sequence (5 bits)
        bit_field_t<25, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ5 =                 // 5th conversion in regular sequence (5 bits)
        bit_field_t<20, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ4 =                 // 4th conversion in regular sequence (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ3 =                 // 3rd conversion in regular sequence (5 bits)
        bit_field_t<10, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ2 =                 // 2nd conversion in regular sequence (5 bits)
        bit_field_t<5, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ1 =                 // 1st conversion in regular sequence (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JSQR_JL =                  // Injected sequence length (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ4 =                // 4th conversion in injected sequence (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ3 =                // 3rd conversion in injected sequence (5 bits)
        bit_field_t<10, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ2 =                // 2nd conversion in injected sequence (5 bits)
        bit_field_t<5, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ1 =                // 1st conversion in injected sequence (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t JSQR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR1_JDATA =               // Injected data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR2_JDATA =               // Injected data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR3_JDATA =               // Injected data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR4_JDATA =               // Injected data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR_DATA =                // Regular data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    static constexpr uint8_t ADC3 = 47; // ADC3 global interrupt
};

static adc3_t& ADC3 = *reinterpret_cast<adc3_t*>(0x40013c00);

#define HAVE_PERIPHERAL_ADC3


////
//
//    Controller area network
//
////

struct can1_t
{
    volatile uint32_t    CAN_MCR;              // [Read-write] CAN_MCR
    volatile uint32_t    CAN_MSR;              // CAN_MSR
    volatile uint32_t    CAN_TSR;              // CAN_TSR
    volatile uint32_t    CAN_RF0R;             // CAN_RF0R
    volatile uint32_t    CAN_RF1R;             // CAN_RF1R
    volatile uint32_t    CAN_IER;              // [Read-write] CAN_IER
    volatile uint32_t    CAN_ESR;              // CAN_ESR
    volatile uint32_t    CAN_BTR;              // [Read-write] CAN_BTR
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

    static constexpr uint32_t CAN_MCR_DBF = 0x10000;        // DBF
    static constexpr uint32_t CAN_MCR_RESET = 0x8000;       // RESET
    static constexpr uint32_t CAN_MCR_TTCM = 0x80;          // TTCM
    static constexpr uint32_t CAN_MCR_ABOM = 0x40;          // ABOM
    static constexpr uint32_t CAN_MCR_AWUM = 0x20;          // AWUM
    static constexpr uint32_t CAN_MCR_NART = 0x10;          // NART
    static constexpr uint32_t CAN_MCR_RFLM = 0x8;           // RFLM
    static constexpr uint32_t CAN_MCR_TXFP = 0x4;           // TXFP
    static constexpr uint32_t CAN_MCR_SLEEP = 0x2;          // SLEEP
    static constexpr uint32_t CAN_MCR_INRQ = 0x1;           // INRQ
    static const uint32_t CAN_MCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_MSR_RX = 0x800;           // RX, Read-only
    static constexpr uint32_t CAN_MSR_SAMP = 0x400;         // SAMP, Read-only
    static constexpr uint32_t CAN_MSR_RXM = 0x200;          // RXM, Read-only
    static constexpr uint32_t CAN_MSR_TXM = 0x100;          // TXM, Read-only
    static constexpr uint32_t CAN_MSR_SLAKI = 0x10;         // SLAKI, Read-write
    static constexpr uint32_t CAN_MSR_WKUI = 0x8;           // WKUI, Read-write
    static constexpr uint32_t CAN_MSR_ERRI = 0x4;           // ERRI, Read-write
    static constexpr uint32_t CAN_MSR_SLAK = 0x2;           // SLAK, Read-only
    static constexpr uint32_t CAN_MSR_INAK = 0x1;           // INAK, Read-only
    static const uint32_t CAN_MSR_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_TSR_LOW2 = 0x80000000;    // Lowest priority flag for mailbox 2, Read-only
    static constexpr uint32_t CAN_TSR_LOW1 = 0x40000000;    // Lowest priority flag for mailbox 1, Read-only
    static constexpr uint32_t CAN_TSR_LOW0 = 0x20000000;    // Lowest priority flag for mailbox 0, Read-only
    static constexpr uint32_t CAN_TSR_TME2 = 0x10000000;    // Lowest priority flag for mailbox 2, Read-only
    static constexpr uint32_t CAN_TSR_TME1 = 0x8000000;     // Lowest priority flag for mailbox 1, Read-only
    static constexpr uint32_t CAN_TSR_TME0 = 0x4000000;     // Lowest priority flag for mailbox 0, Read-only
    template<uint32_t X>
    static constexpr uint32_t CAN_TSR_CODE =                // CODE (2 bits), Read-only
        bit_field_t<24, 0x3>::value<X>();
    static constexpr uint32_t CAN_TSR_ABRQ2 = 0x800000;     // ABRQ2, Read-write
    static constexpr uint32_t CAN_TSR_TERR2 = 0x80000;      // TERR2, Read-write
    static constexpr uint32_t CAN_TSR_ALST2 = 0x40000;      // ALST2, Read-write
    static constexpr uint32_t CAN_TSR_TXOK2 = 0x20000;      // TXOK2, Read-write
    static constexpr uint32_t CAN_TSR_RQCP2 = 0x10000;      // RQCP2, Read-write
    static constexpr uint32_t CAN_TSR_ABRQ1 = 0x8000;       // ABRQ1, Read-write
    static constexpr uint32_t CAN_TSR_TERR1 = 0x800;        // TERR1, Read-write
    static constexpr uint32_t CAN_TSR_ALST1 = 0x400;        // ALST1, Read-write
    static constexpr uint32_t CAN_TSR_TXOK1 = 0x200;        // TXOK1, Read-write
    static constexpr uint32_t CAN_TSR_RQCP1 = 0x100;        // RQCP1, Read-write
    static constexpr uint32_t CAN_TSR_ABRQ0 = 0x80;         // ABRQ0, Read-write
    static constexpr uint32_t CAN_TSR_TERR0 = 0x8;          // TERR0, Read-write
    static constexpr uint32_t CAN_TSR_ALST0 = 0x4;          // ALST0, Read-write
    static constexpr uint32_t CAN_TSR_TXOK0 = 0x2;          // TXOK0, Read-write
    static constexpr uint32_t CAN_TSR_RQCP0 = 0x1;          // RQCP0, Read-write
    static const uint32_t CAN_TSR_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_RF0R_RFOM0 = 0x20;         // RFOM0, Read-write
    static constexpr uint32_t CAN_RF0R_FOVR0 = 0x10;         // FOVR0, Read-write
    static constexpr uint32_t CAN_RF0R_FULL0 = 0x8;          // FULL0, Read-write
    template<uint32_t X>
    static constexpr uint32_t CAN_RF0R_FMP0 =                // FMP0 (2 bits), Read-only
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t CAN_RF0R_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_RF1R_RFOM1 = 0x20;         // RFOM1, Read-write
    static constexpr uint32_t CAN_RF1R_FOVR1 = 0x10;         // FOVR1, Read-write
    static constexpr uint32_t CAN_RF1R_FULL1 = 0x8;          // FULL1, Read-write
    template<uint32_t X>
    static constexpr uint32_t CAN_RF1R_FMP1 =                // FMP1 (2 bits), Read-only
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t CAN_RF1R_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_IER_SLKIE = 0x20000;      // SLKIE
    static constexpr uint32_t CAN_IER_WKUIE = 0x10000;      // WKUIE
    static constexpr uint32_t CAN_IER_ERRIE = 0x8000;       // ERRIE
    static constexpr uint32_t CAN_IER_LECIE = 0x800;        // LECIE
    static constexpr uint32_t CAN_IER_BOFIE = 0x400;        // BOFIE
    static constexpr uint32_t CAN_IER_EPVIE = 0x200;        // EPVIE
    static constexpr uint32_t CAN_IER_EWGIE = 0x100;        // EWGIE
    static constexpr uint32_t CAN_IER_FOVIE1 = 0x40;        // FOVIE1
    static constexpr uint32_t CAN_IER_FFIE1 = 0x20;         // FFIE1
    static constexpr uint32_t CAN_IER_FMPIE1 = 0x10;        // FMPIE1
    static constexpr uint32_t CAN_IER_FOVIE0 = 0x8;         // FOVIE0
    static constexpr uint32_t CAN_IER_FFIE0 = 0x4;          // FFIE0
    static constexpr uint32_t CAN_IER_FMPIE0 = 0x2;         // FMPIE0
    static constexpr uint32_t CAN_IER_TMEIE = 0x1;          // TMEIE
    static const uint32_t CAN_IER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_ESR_REC =                 // REC (8 bits), Read-only
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_ESR_TEC =                 // TEC (8 bits), Read-only
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_ESR_LEC =                 // LEC (3 bits), Read-write
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CAN_ESR_BOFF = 0x4;           // BOFF, Read-only
    static constexpr uint32_t CAN_ESR_EPVF = 0x2;           // EPVF, Read-only
    static constexpr uint32_t CAN_ESR_EWGF = 0x1;           // EWGF, Read-only
    static const uint32_t CAN_ESR_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_BTR_SILM = 0x80000000;    // SILM
    static constexpr uint32_t CAN_BTR_LBKM = 0x40000000;    // LBKM
    template<uint32_t X>
    static constexpr uint32_t CAN_BTR_SJW =                 // SJW (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_BTR_TS2 =                 // TS2 (3 bits)
        bit_field_t<20, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_BTR_TS1 =                 // TS1 (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_BTR_BRP =                 // BRP (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t CAN_BTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TI0R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TI0R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t CAN_TI0R_IDE = 0x4;            // IDE
    static constexpr uint32_t CAN_TI0R_RTR = 0x2;            // RTR
    static constexpr uint32_t CAN_TI0R_TXRQ = 0x1;           // TXRQ
    static const uint32_t CAN_TI0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDT0R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static constexpr uint32_t CAN_TDT0R_TGT = 0x100;          // TGT
    template<uint32_t X>
    static constexpr uint32_t CAN_TDT0R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CAN_TDT0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDL0R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL0R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL0R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL0R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_TDL0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDH0R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH0R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH0R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH0R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_TDH0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TI1R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TI1R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t CAN_TI1R_IDE = 0x4;            // IDE
    static constexpr uint32_t CAN_TI1R_RTR = 0x2;            // RTR
    static constexpr uint32_t CAN_TI1R_TXRQ = 0x1;           // TXRQ
    static const uint32_t CAN_TI1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDT1R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static constexpr uint32_t CAN_TDT1R_TGT = 0x100;          // TGT
    template<uint32_t X>
    static constexpr uint32_t CAN_TDT1R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CAN_TDT1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDL1R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL1R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL1R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL1R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_TDL1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDH1R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH1R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH1R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH1R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_TDH1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TI2R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TI2R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t CAN_TI2R_IDE = 0x4;            // IDE
    static constexpr uint32_t CAN_TI2R_RTR = 0x2;            // RTR
    static constexpr uint32_t CAN_TI2R_TXRQ = 0x1;           // TXRQ
    static const uint32_t CAN_TI2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDT2R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static constexpr uint32_t CAN_TDT2R_TGT = 0x100;          // TGT
    template<uint32_t X>
    static constexpr uint32_t CAN_TDT2R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CAN_TDT2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDL2R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL2R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL2R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL2R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_TDL2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDH2R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH2R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH2R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH2R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_TDH2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RI0R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RI0R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t CAN_RI0R_IDE = 0x4;            // IDE
    static constexpr uint32_t CAN_RI0R_RTR = 0x2;            // RTR
    static const uint32_t CAN_RI0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RDT0R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDT0R_FMI =                 // FMI (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDT0R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CAN_RDT0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RDL0R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDL0R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDL0R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDL0R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_RDL0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RDH0R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDH0R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDH0R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDH0R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_RDH0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RI1R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RI1R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t CAN_RI1R_IDE = 0x4;            // IDE
    static constexpr uint32_t CAN_RI1R_RTR = 0x2;            // RTR
    static const uint32_t CAN_RI1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RDT1R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDT1R_FMI =                 // FMI (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDT1R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CAN_RDT1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RDL1R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDL1R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDL1R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDL1R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_RDL1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RDH1R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDH1R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDH1R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDH1R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_RDH1R_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_FMR_FINIT = 0x1;          // FINIT
    static const uint32_t CAN_FMR_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_FM1R_FBM0 = 0x1;           // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM1 = 0x2;           // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM2 = 0x4;           // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM3 = 0x8;           // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM4 = 0x10;          // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM5 = 0x20;          // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM6 = 0x40;          // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM7 = 0x80;          // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM8 = 0x100;         // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM9 = 0x200;         // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM10 = 0x400;        // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM11 = 0x800;        // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM12 = 0x1000;       // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM13 = 0x2000;       // Filter mode
    static const uint32_t CAN_FM1R_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_FS1R_FSC0 = 0x1;           // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC1 = 0x2;           // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC2 = 0x4;           // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC3 = 0x8;           // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC4 = 0x10;          // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC5 = 0x20;          // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC6 = 0x40;          // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC7 = 0x80;          // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC8 = 0x100;         // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC9 = 0x200;         // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC10 = 0x400;        // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC11 = 0x800;        // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC12 = 0x1000;       // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC13 = 0x2000;       // Filter scale configuration
    static const uint32_t CAN_FS1R_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_FFA1R_FFA0 = 0x1;           // Filter FIFO assignment for filter 0
    static constexpr uint32_t CAN_FFA1R_FFA1 = 0x2;           // Filter FIFO assignment for filter 1
    static constexpr uint32_t CAN_FFA1R_FFA2 = 0x4;           // Filter FIFO assignment for filter 2
    static constexpr uint32_t CAN_FFA1R_FFA3 = 0x8;           // Filter FIFO assignment for filter 3
    static constexpr uint32_t CAN_FFA1R_FFA4 = 0x10;          // Filter FIFO assignment for filter 4
    static constexpr uint32_t CAN_FFA1R_FFA5 = 0x20;          // Filter FIFO assignment for filter 5
    static constexpr uint32_t CAN_FFA1R_FFA6 = 0x40;          // Filter FIFO assignment for filter 6
    static constexpr uint32_t CAN_FFA1R_FFA7 = 0x80;          // Filter FIFO assignment for filter 7
    static constexpr uint32_t CAN_FFA1R_FFA8 = 0x100;         // Filter FIFO assignment for filter 8
    static constexpr uint32_t CAN_FFA1R_FFA9 = 0x200;         // Filter FIFO assignment for filter 9
    static constexpr uint32_t CAN_FFA1R_FFA10 = 0x400;        // Filter FIFO assignment for filter 10
    static constexpr uint32_t CAN_FFA1R_FFA11 = 0x800;        // Filter FIFO assignment for filter 11
    static constexpr uint32_t CAN_FFA1R_FFA12 = 0x1000;       // Filter FIFO assignment for filter 12
    static constexpr uint32_t CAN_FFA1R_FFA13 = 0x2000;       // Filter FIFO assignment for filter 13
    static const uint32_t CAN_FFA1R_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_FA1R_FACT0 = 0x1;          // Filter active
    static constexpr uint32_t CAN_FA1R_FACT1 = 0x2;          // Filter active
    static constexpr uint32_t CAN_FA1R_FACT2 = 0x4;          // Filter active
    static constexpr uint32_t CAN_FA1R_FACT3 = 0x8;          // Filter active
    static constexpr uint32_t CAN_FA1R_FACT4 = 0x10;         // Filter active
    static constexpr uint32_t CAN_FA1R_FACT5 = 0x20;         // Filter active
    static constexpr uint32_t CAN_FA1R_FACT6 = 0x40;         // Filter active
    static constexpr uint32_t CAN_FA1R_FACT7 = 0x80;         // Filter active
    static constexpr uint32_t CAN_FA1R_FACT8 = 0x100;        // Filter active
    static constexpr uint32_t CAN_FA1R_FACT9 = 0x200;        // Filter active
    static constexpr uint32_t CAN_FA1R_FACT10 = 0x400;       // Filter active
    static constexpr uint32_t CAN_FA1R_FACT11 = 0x800;       // Filter active
    static constexpr uint32_t CAN_FA1R_FACT12 = 0x1000;      // Filter active
    static constexpr uint32_t CAN_FA1R_FACT13 = 0x2000;      // Filter active
    static const uint32_t CAN_FA1R_RESET_VALUE = 0x0;

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

    static constexpr uint8_t CAN_RX1 = 21; // CAN RX1 interrupt
    static constexpr uint8_t CAN_SCE = 22; // CAN SCE interrupt
};

static can1_t& CAN1 = *reinterpret_cast<can1_t*>(0x40006400);

#define HAVE_PERIPHERAL_CAN1


////
//
//    Controller area network
//
////

struct can2_t
{
    volatile uint32_t    CAN_MCR;              // [Read-write] CAN_MCR
    volatile uint32_t    CAN_MSR;              // CAN_MSR
    volatile uint32_t    CAN_TSR;              // CAN_TSR
    volatile uint32_t    CAN_RF0R;             // CAN_RF0R
    volatile uint32_t    CAN_RF1R;             // CAN_RF1R
    volatile uint32_t    CAN_IER;              // [Read-write] CAN_IER
    volatile uint32_t    CAN_ESR;              // CAN_ESR
    volatile uint32_t    CAN_BTR;              // [Read-write] CAN_BTR
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

    static constexpr uint32_t CAN_MCR_DBF = 0x10000;        // DBF
    static constexpr uint32_t CAN_MCR_RESET = 0x8000;       // RESET
    static constexpr uint32_t CAN_MCR_TTCM = 0x80;          // TTCM
    static constexpr uint32_t CAN_MCR_ABOM = 0x40;          // ABOM
    static constexpr uint32_t CAN_MCR_AWUM = 0x20;          // AWUM
    static constexpr uint32_t CAN_MCR_NART = 0x10;          // NART
    static constexpr uint32_t CAN_MCR_RFLM = 0x8;           // RFLM
    static constexpr uint32_t CAN_MCR_TXFP = 0x4;           // TXFP
    static constexpr uint32_t CAN_MCR_SLEEP = 0x2;          // SLEEP
    static constexpr uint32_t CAN_MCR_INRQ = 0x1;           // INRQ
    static const uint32_t CAN_MCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_MSR_RX = 0x800;           // RX, Read-only
    static constexpr uint32_t CAN_MSR_SAMP = 0x400;         // SAMP, Read-only
    static constexpr uint32_t CAN_MSR_RXM = 0x200;          // RXM, Read-only
    static constexpr uint32_t CAN_MSR_TXM = 0x100;          // TXM, Read-only
    static constexpr uint32_t CAN_MSR_SLAKI = 0x10;         // SLAKI, Read-write
    static constexpr uint32_t CAN_MSR_WKUI = 0x8;           // WKUI, Read-write
    static constexpr uint32_t CAN_MSR_ERRI = 0x4;           // ERRI, Read-write
    static constexpr uint32_t CAN_MSR_SLAK = 0x2;           // SLAK, Read-only
    static constexpr uint32_t CAN_MSR_INAK = 0x1;           // INAK, Read-only
    static const uint32_t CAN_MSR_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_TSR_LOW2 = 0x80000000;    // Lowest priority flag for mailbox 2, Read-only
    static constexpr uint32_t CAN_TSR_LOW1 = 0x40000000;    // Lowest priority flag for mailbox 1, Read-only
    static constexpr uint32_t CAN_TSR_LOW0 = 0x20000000;    // Lowest priority flag for mailbox 0, Read-only
    static constexpr uint32_t CAN_TSR_TME2 = 0x10000000;    // Lowest priority flag for mailbox 2, Read-only
    static constexpr uint32_t CAN_TSR_TME1 = 0x8000000;     // Lowest priority flag for mailbox 1, Read-only
    static constexpr uint32_t CAN_TSR_TME0 = 0x4000000;     // Lowest priority flag for mailbox 0, Read-only
    template<uint32_t X>
    static constexpr uint32_t CAN_TSR_CODE =                // CODE (2 bits), Read-only
        bit_field_t<24, 0x3>::value<X>();
    static constexpr uint32_t CAN_TSR_ABRQ2 = 0x800000;     // ABRQ2, Read-write
    static constexpr uint32_t CAN_TSR_TERR2 = 0x80000;      // TERR2, Read-write
    static constexpr uint32_t CAN_TSR_ALST2 = 0x40000;      // ALST2, Read-write
    static constexpr uint32_t CAN_TSR_TXOK2 = 0x20000;      // TXOK2, Read-write
    static constexpr uint32_t CAN_TSR_RQCP2 = 0x10000;      // RQCP2, Read-write
    static constexpr uint32_t CAN_TSR_ABRQ1 = 0x8000;       // ABRQ1, Read-write
    static constexpr uint32_t CAN_TSR_TERR1 = 0x800;        // TERR1, Read-write
    static constexpr uint32_t CAN_TSR_ALST1 = 0x400;        // ALST1, Read-write
    static constexpr uint32_t CAN_TSR_TXOK1 = 0x200;        // TXOK1, Read-write
    static constexpr uint32_t CAN_TSR_RQCP1 = 0x100;        // RQCP1, Read-write
    static constexpr uint32_t CAN_TSR_ABRQ0 = 0x80;         // ABRQ0, Read-write
    static constexpr uint32_t CAN_TSR_TERR0 = 0x8;          // TERR0, Read-write
    static constexpr uint32_t CAN_TSR_ALST0 = 0x4;          // ALST0, Read-write
    static constexpr uint32_t CAN_TSR_TXOK0 = 0x2;          // TXOK0, Read-write
    static constexpr uint32_t CAN_TSR_RQCP0 = 0x1;          // RQCP0, Read-write
    static const uint32_t CAN_TSR_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_RF0R_RFOM0 = 0x20;         // RFOM0, Read-write
    static constexpr uint32_t CAN_RF0R_FOVR0 = 0x10;         // FOVR0, Read-write
    static constexpr uint32_t CAN_RF0R_FULL0 = 0x8;          // FULL0, Read-write
    template<uint32_t X>
    static constexpr uint32_t CAN_RF0R_FMP0 =                // FMP0 (2 bits), Read-only
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t CAN_RF0R_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_RF1R_RFOM1 = 0x20;         // RFOM1, Read-write
    static constexpr uint32_t CAN_RF1R_FOVR1 = 0x10;         // FOVR1, Read-write
    static constexpr uint32_t CAN_RF1R_FULL1 = 0x8;          // FULL1, Read-write
    template<uint32_t X>
    static constexpr uint32_t CAN_RF1R_FMP1 =                // FMP1 (2 bits), Read-only
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t CAN_RF1R_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_IER_SLKIE = 0x20000;      // SLKIE
    static constexpr uint32_t CAN_IER_WKUIE = 0x10000;      // WKUIE
    static constexpr uint32_t CAN_IER_ERRIE = 0x8000;       // ERRIE
    static constexpr uint32_t CAN_IER_LECIE = 0x800;        // LECIE
    static constexpr uint32_t CAN_IER_BOFIE = 0x400;        // BOFIE
    static constexpr uint32_t CAN_IER_EPVIE = 0x200;        // EPVIE
    static constexpr uint32_t CAN_IER_EWGIE = 0x100;        // EWGIE
    static constexpr uint32_t CAN_IER_FOVIE1 = 0x40;        // FOVIE1
    static constexpr uint32_t CAN_IER_FFIE1 = 0x20;         // FFIE1
    static constexpr uint32_t CAN_IER_FMPIE1 = 0x10;        // FMPIE1
    static constexpr uint32_t CAN_IER_FOVIE0 = 0x8;         // FOVIE0
    static constexpr uint32_t CAN_IER_FFIE0 = 0x4;          // FFIE0
    static constexpr uint32_t CAN_IER_FMPIE0 = 0x2;         // FMPIE0
    static constexpr uint32_t CAN_IER_TMEIE = 0x1;          // TMEIE
    static const uint32_t CAN_IER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_ESR_REC =                 // REC (8 bits), Read-only
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_ESR_TEC =                 // TEC (8 bits), Read-only
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_ESR_LEC =                 // LEC (3 bits), Read-write
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CAN_ESR_BOFF = 0x4;           // BOFF, Read-only
    static constexpr uint32_t CAN_ESR_EPVF = 0x2;           // EPVF, Read-only
    static constexpr uint32_t CAN_ESR_EWGF = 0x1;           // EWGF, Read-only
    static const uint32_t CAN_ESR_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_BTR_SILM = 0x80000000;    // SILM
    static constexpr uint32_t CAN_BTR_LBKM = 0x40000000;    // LBKM
    template<uint32_t X>
    static constexpr uint32_t CAN_BTR_SJW =                 // SJW (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_BTR_TS2 =                 // TS2 (3 bits)
        bit_field_t<20, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_BTR_TS1 =                 // TS1 (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_BTR_BRP =                 // BRP (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t CAN_BTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TI0R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TI0R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t CAN_TI0R_IDE = 0x4;            // IDE
    static constexpr uint32_t CAN_TI0R_RTR = 0x2;            // RTR
    static constexpr uint32_t CAN_TI0R_TXRQ = 0x1;           // TXRQ
    static const uint32_t CAN_TI0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDT0R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static constexpr uint32_t CAN_TDT0R_TGT = 0x100;          // TGT
    template<uint32_t X>
    static constexpr uint32_t CAN_TDT0R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CAN_TDT0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDL0R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL0R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL0R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL0R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_TDL0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDH0R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH0R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH0R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH0R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_TDH0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TI1R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TI1R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t CAN_TI1R_IDE = 0x4;            // IDE
    static constexpr uint32_t CAN_TI1R_RTR = 0x2;            // RTR
    static constexpr uint32_t CAN_TI1R_TXRQ = 0x1;           // TXRQ
    static const uint32_t CAN_TI1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDT1R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static constexpr uint32_t CAN_TDT1R_TGT = 0x100;          // TGT
    template<uint32_t X>
    static constexpr uint32_t CAN_TDT1R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CAN_TDT1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDL1R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL1R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL1R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL1R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_TDL1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDH1R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH1R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH1R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH1R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_TDH1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TI2R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TI2R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t CAN_TI2R_IDE = 0x4;            // IDE
    static constexpr uint32_t CAN_TI2R_RTR = 0x2;            // RTR
    static constexpr uint32_t CAN_TI2R_TXRQ = 0x1;           // TXRQ
    static const uint32_t CAN_TI2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDT2R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static constexpr uint32_t CAN_TDT2R_TGT = 0x100;          // TGT
    template<uint32_t X>
    static constexpr uint32_t CAN_TDT2R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CAN_TDT2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDL2R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL2R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL2R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDL2R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_TDL2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_TDH2R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH2R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH2R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_TDH2R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_TDH2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RI0R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RI0R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t CAN_RI0R_IDE = 0x4;            // IDE
    static constexpr uint32_t CAN_RI0R_RTR = 0x2;            // RTR
    static const uint32_t CAN_RI0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RDT0R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDT0R_FMI =                 // FMI (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDT0R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CAN_RDT0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RDL0R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDL0R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDL0R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDL0R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_RDL0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RDH0R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDH0R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDH0R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDH0R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_RDH0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RI1R_STID =                // STID (11 bits)
        bit_field_t<21, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RI1R_EXID =                // EXID (18 bits)
        bit_field_t<3, 0x3ffff>::value<X>();
    static constexpr uint32_t CAN_RI1R_IDE = 0x4;            // IDE
    static constexpr uint32_t CAN_RI1R_RTR = 0x2;            // RTR
    static const uint32_t CAN_RI1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RDT1R_TIME =                // TIME (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDT1R_FMI =                 // FMI (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDT1R_DLC =                 // DLC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CAN_RDT1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RDL1R_DATA3 =               // DATA3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDL1R_DATA2 =               // DATA2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDL1R_DATA1 =               // DATA1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDL1R_DATA0 =               // DATA0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_RDL1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CAN_RDH1R_DATA7 =               // DATA7 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDH1R_DATA6 =               // DATA6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDH1R_DATA5 =               // DATA5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CAN_RDH1R_DATA4 =               // DATA4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CAN_RDH1R_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_FMR_FINIT = 0x1;          // FINIT
    static const uint32_t CAN_FMR_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_FM1R_FBM0 = 0x1;           // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM1 = 0x2;           // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM2 = 0x4;           // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM3 = 0x8;           // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM4 = 0x10;          // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM5 = 0x20;          // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM6 = 0x40;          // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM7 = 0x80;          // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM8 = 0x100;         // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM9 = 0x200;         // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM10 = 0x400;        // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM11 = 0x800;        // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM12 = 0x1000;       // Filter mode
    static constexpr uint32_t CAN_FM1R_FBM13 = 0x2000;       // Filter mode
    static const uint32_t CAN_FM1R_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_FS1R_FSC0 = 0x1;           // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC1 = 0x2;           // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC2 = 0x4;           // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC3 = 0x8;           // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC4 = 0x10;          // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC5 = 0x20;          // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC6 = 0x40;          // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC7 = 0x80;          // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC8 = 0x100;         // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC9 = 0x200;         // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC10 = 0x400;        // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC11 = 0x800;        // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC12 = 0x1000;       // Filter scale configuration
    static constexpr uint32_t CAN_FS1R_FSC13 = 0x2000;       // Filter scale configuration
    static const uint32_t CAN_FS1R_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_FFA1R_FFA0 = 0x1;           // Filter FIFO assignment for filter 0
    static constexpr uint32_t CAN_FFA1R_FFA1 = 0x2;           // Filter FIFO assignment for filter 1
    static constexpr uint32_t CAN_FFA1R_FFA2 = 0x4;           // Filter FIFO assignment for filter 2
    static constexpr uint32_t CAN_FFA1R_FFA3 = 0x8;           // Filter FIFO assignment for filter 3
    static constexpr uint32_t CAN_FFA1R_FFA4 = 0x10;          // Filter FIFO assignment for filter 4
    static constexpr uint32_t CAN_FFA1R_FFA5 = 0x20;          // Filter FIFO assignment for filter 5
    static constexpr uint32_t CAN_FFA1R_FFA6 = 0x40;          // Filter FIFO assignment for filter 6
    static constexpr uint32_t CAN_FFA1R_FFA7 = 0x80;          // Filter FIFO assignment for filter 7
    static constexpr uint32_t CAN_FFA1R_FFA8 = 0x100;         // Filter FIFO assignment for filter 8
    static constexpr uint32_t CAN_FFA1R_FFA9 = 0x200;         // Filter FIFO assignment for filter 9
    static constexpr uint32_t CAN_FFA1R_FFA10 = 0x400;        // Filter FIFO assignment for filter 10
    static constexpr uint32_t CAN_FFA1R_FFA11 = 0x800;        // Filter FIFO assignment for filter 11
    static constexpr uint32_t CAN_FFA1R_FFA12 = 0x1000;       // Filter FIFO assignment for filter 12
    static constexpr uint32_t CAN_FFA1R_FFA13 = 0x2000;       // Filter FIFO assignment for filter 13
    static const uint32_t CAN_FFA1R_RESET_VALUE = 0x0;

    static constexpr uint32_t CAN_FA1R_FACT0 = 0x1;          // Filter active
    static constexpr uint32_t CAN_FA1R_FACT1 = 0x2;          // Filter active
    static constexpr uint32_t CAN_FA1R_FACT2 = 0x4;          // Filter active
    static constexpr uint32_t CAN_FA1R_FACT3 = 0x8;          // Filter active
    static constexpr uint32_t CAN_FA1R_FACT4 = 0x10;         // Filter active
    static constexpr uint32_t CAN_FA1R_FACT5 = 0x20;         // Filter active
    static constexpr uint32_t CAN_FA1R_FACT6 = 0x40;         // Filter active
    static constexpr uint32_t CAN_FA1R_FACT7 = 0x80;         // Filter active
    static constexpr uint32_t CAN_FA1R_FACT8 = 0x100;        // Filter active
    static constexpr uint32_t CAN_FA1R_FACT9 = 0x200;        // Filter active
    static constexpr uint32_t CAN_FA1R_FACT10 = 0x400;       // Filter active
    static constexpr uint32_t CAN_FA1R_FACT11 = 0x800;       // Filter active
    static constexpr uint32_t CAN_FA1R_FACT12 = 0x1000;      // Filter active
    static constexpr uint32_t CAN_FA1R_FACT13 = 0x2000;      // Filter active
    static const uint32_t CAN_FA1R_RESET_VALUE = 0x0;

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
};

static can2_t& CAN2 = *reinterpret_cast<can2_t*>(0x40006800);

#define HAVE_PERIPHERAL_CAN2


////
//
//    Digital to analog converter
//
////

struct dac_t
{
    volatile uint32_t    CR;                   // [Read-write] Control register (DAC_CR)
    volatile uint32_t    SWTRIGR;              // [Write-only] DAC software trigger register (DAC_SWTRIGR)
    volatile uint32_t    DHR12R1;              // [Read-write] DAC channel1 12-bit right-aligned data holding register(DAC_DHR12R1)
    volatile uint32_t    DHR12L1;              // [Read-write] DAC channel1 12-bit left aligned data holding register (DAC_DHR12L1)
    volatile uint32_t    DHR8R1;               // [Read-write] DAC channel1 8-bit right aligned data holding register (DAC_DHR8R1)
    volatile uint32_t    DHR12R2;              // [Read-write] DAC channel2 12-bit right aligned data holding register (DAC_DHR12R2)
    volatile uint32_t    DHR12L2;              // [Read-write] DAC channel2 12-bit left aligned data holding register (DAC_DHR12L2)
    volatile uint32_t    DHR8R2;               // [Read-write] DAC channel2 8-bit right-aligned data holding register (DAC_DHR8R2)
    volatile uint32_t    DHR12RD;              // [Read-write] Dual DAC 12-bit right-aligned data holding register (DAC_DHR12RD), Bits 31:28 Reserved, Bits 15:12 Reserved
    volatile uint32_t    DHR12LD;              // [Read-write] DUAL DAC 12-bit left aligned data holding register (DAC_DHR12LD), Bits 19:16 Reserved, Bits 3:0 Reserved
    volatile uint32_t    DHR8RD;               // [Read-write] DUAL DAC 8-bit right aligned data holding register (DAC_DHR8RD), Bits 31:16 Reserved
    volatile uint32_t    DOR1;                 // [Read-only] DAC channel1 data output register (DAC_DOR1)
    volatile uint32_t    DOR2;                 // [Read-only] DAC channel2 data output register (DAC_DOR2)

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
    static constexpr uint32_t DHR12LD_DACC2DHR =            // DAC channel2 12-bit right-aligned data (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DHR12LD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR8RD_DACC1DHR =            // DAC channel1 8-bit right-aligned data (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DHR8RD_DACC2DHR =            // DAC channel2 8-bit right-aligned data (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DHR8RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DOR1_DACC1DOR =            // DAC channel1 data output (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t DOR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DOR2_DACC2DOR =            // DAC channel2 data output (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t DOR2_RESET_VALUE = 0x0;
};

static dac_t& DAC = *reinterpret_cast<dac_t*>(0x40007400);

#define HAVE_PERIPHERAL_DAC


////
//
//    Debug support
//
////

struct dbg_t
{
    volatile uint32_t    IDCODE;               // [Read-only] DBGMCU_IDCODE
    volatile uint32_t    CR;                   // [Read-write] DBGMCU_CR

    template<uint32_t X>
    static constexpr uint32_t IDCODE_DEV_ID =              // DEV_ID (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IDCODE_REV_ID =              // REV_ID (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t IDCODE_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_DBG_SLEEP = 0x1;      // DBG_SLEEP
    static constexpr uint32_t CR_DBG_STOP = 0x2;       // DBG_STOP
    static constexpr uint32_t CR_DBG_STANDBY = 0x4;    // DBG_STANDBY
    static constexpr uint32_t CR_TRACE_IOEN = 0x20;    // TRACE_IOEN
    template<uint32_t X>
    static constexpr uint32_t CR_TRACE_MODE =          // TRACE_MODE (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    static constexpr uint32_t CR_DBG_IWDG_STOP = 0x100;// DBG_IWDG_STOP
    static constexpr uint32_t CR_DBG_WWDG_STOP = 0x200;// DBG_WWDG_STOP
    static constexpr uint32_t CR_DBG_TIM1_STOP = 0x400;// DBG_TIM1_STOP
    static constexpr uint32_t CR_DBG_TIM2_STOP = 0x800;// DBG_TIM2_STOP
    static constexpr uint32_t CR_DBG_TIM3_STOP = 0x1000;// DBG_TIM3_STOP
    static constexpr uint32_t CR_DBG_TIM4_STOP = 0x2000;// DBG_TIM4_STOP
    static constexpr uint32_t CR_DBG_CAN1_STOP = 0x4000;// DBG_CAN1_STOP
    static constexpr uint32_t CR_DBG_I2C1_SMBUS_TIMEOUT = 0x8000;// DBG_I2C1_SMBUS_TIMEOUT
    static constexpr uint32_t CR_DBG_I2C2_SMBUS_TIMEOUT = 0x10000;// DBG_I2C2_SMBUS_TIMEOUT
    static constexpr uint32_t CR_DBG_TIM8_STOP = 0x20000;// DBG_TIM8_STOP
    static constexpr uint32_t CR_DBG_TIM5_STOP = 0x40000;// DBG_TIM5_STOP
    static constexpr uint32_t CR_DBG_TIM6_STOP = 0x80000;// DBG_TIM6_STOP
    static constexpr uint32_t CR_DBG_TIM7_STOP = 0x100000;// DBG_TIM7_STOP
    static constexpr uint32_t CR_DBG_CAN2_STOP = 0x200000;// DBG_CAN2_STOP
    static const uint32_t CR_RESET_VALUE = 0x0;
};

static dbg_t& DBG = *reinterpret_cast<dbg_t*>(0xe0042000);

#define HAVE_PERIPHERAL_DBG


////
//
//    Universal asynchronous receiver transmitter
//
////

struct uart4_t
{
    volatile uint32_t    SR;                   // UART4_SR
    volatile uint32_t    DR;                   // [Read-write] UART4_DR
    volatile uint32_t    BRR;                  // [Read-write] UART4_BRR
    volatile uint32_t    CR1;                  // [Read-write] UART4_CR1
    volatile uint32_t    CR2;                  // [Read-write] UART4_CR2
    volatile uint32_t    CR3;                  // [Read-write] UART4_CR3

    static constexpr uint32_t SR_PE = 0x1;             // Parity error, Read-only
    static constexpr uint32_t SR_FE = 0x2;             // Framing error, Read-only
    static constexpr uint32_t SR_NE = 0x4;             // Noise error flag, Read-only
    static constexpr uint32_t SR_ORE = 0x8;            // Overrun error, Read-only
    static constexpr uint32_t SR_IDLE = 0x10;          // IDLE line detected, Read-only
    static constexpr uint32_t SR_RXNE = 0x20;          // Read data register not empty, Read-write
    static constexpr uint32_t SR_TC = 0x40;            // Transmission complete, Read-write
    static constexpr uint32_t SR_TXE = 0x80;           // Transmit data register empty, Read-only
    static constexpr uint32_t SR_LBD = 0x100;          // LIN break detection flag, Read-write
    static const uint32_t SR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR_DR =                  // DR (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // DIV_Fraction (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Mantissa =        // DIV_Mantissa (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR1_SBK = 0x1;            // Send break
    static constexpr uint32_t CR1_RWU = 0x2;            // Receiver wakeup
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // TXE interrupt enable
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_WAKE = 0x800;         // Wakeup method
    static constexpr uint32_t CR1_M = 0x1000;           // Word length
    static constexpr uint32_t CR1_UE = 0x2000;          // USART enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_ADD =                 // Address of the USART node (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static constexpr uint32_t CR2_LBDL = 0x20;          // lin break detection length
    static constexpr uint32_t CR2_LBDIE = 0x40;         // LIN break detection interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR2_STOP =                // STOP bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CR2_LINEN = 0x4000;       // LIN mode enable
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CR3_EIE = 0x1;            // Error interrupt enable
    static constexpr uint32_t CR3_IREN = 0x2;           // IrDA mode enable
    static constexpr uint32_t CR3_IRLP = 0x4;           // IrDA low-power
    static constexpr uint32_t CR3_HDSEL = 0x8;          // Half-duplex selection
    static constexpr uint32_t CR3_DMAR = 0x40;          // DMA enable receiver
    static constexpr uint32_t CR3_DMAT = 0x80;          // DMA enable transmitter
    static const uint32_t CR3_RESET_VALUE = 0x0;

    static constexpr uint8_t UART4 = 52; // UART4 global interrupt
};

static uart4_t& UART4 = *reinterpret_cast<uart4_t*>(0x40004c00);

#define HAVE_PERIPHERAL_UART4


////
//
//    Universal asynchronous receiver transmitter
//
////

struct uart5_t
{
    volatile uint32_t    SR;                   // UART4_SR
    volatile uint32_t    DR;                   // [Read-write] UART4_DR
    volatile uint32_t    BRR;                  // [Read-write] UART4_BRR
    volatile uint32_t    CR1;                  // [Read-write] UART4_CR1
    volatile uint32_t    CR2;                  // [Read-write] UART4_CR2
    volatile uint32_t    CR3;                  // [Read-write] UART4_CR3

    static constexpr uint32_t SR_PE = 0x1;             // PE, Read-only
    static constexpr uint32_t SR_FE = 0x2;             // FE, Read-only
    static constexpr uint32_t SR_NE = 0x4;             // NE, Read-only
    static constexpr uint32_t SR_ORE = 0x8;            // ORE, Read-only
    static constexpr uint32_t SR_IDLE = 0x10;          // IDLE, Read-only
    static constexpr uint32_t SR_RXNE = 0x20;          // RXNE, Read-write
    static constexpr uint32_t SR_TC = 0x40;            // TC, Read-write
    static constexpr uint32_t SR_TXE = 0x80;           // TXE, Read-only
    static constexpr uint32_t SR_LBD = 0x100;          // LBD, Read-write
    static const uint32_t SR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR_DR =                  // DR (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // DIV_Fraction (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Mantissa =        // DIV_Mantissa (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR1_SBK = 0x1;            // SBK
    static constexpr uint32_t CR1_RWU = 0x2;            // RWU
    static constexpr uint32_t CR1_RE = 0x4;             // RE
    static constexpr uint32_t CR1_TE = 0x8;             // TE
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLEIE
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNEIE
    static constexpr uint32_t CR1_TCIE = 0x40;          // TCIE
    static constexpr uint32_t CR1_TXEIE = 0x80;         // TXEIE
    static constexpr uint32_t CR1_PEIE = 0x100;         // PEIE
    static constexpr uint32_t CR1_PS = 0x200;           // PS
    static constexpr uint32_t CR1_PCE = 0x400;          // PCE
    static constexpr uint32_t CR1_WAKE = 0x800;         // WAKE
    static constexpr uint32_t CR1_M = 0x1000;           // M
    static constexpr uint32_t CR1_UE = 0x2000;          // UE
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_ADD =                 // ADD (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static constexpr uint32_t CR2_LBDL = 0x20;          // LBDL
    static constexpr uint32_t CR2_LBDIE = 0x40;         // LBDIE
    template<uint32_t X>
    static constexpr uint32_t CR2_STOP =                // STOP (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CR2_LINEN = 0x4000;       // LINEN
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CR3_EIE = 0x1;            // Error interrupt enable
    static constexpr uint32_t CR3_IREN = 0x2;           // IrDA mode enable
    static constexpr uint32_t CR3_IRLP = 0x4;           // IrDA low-power
    static constexpr uint32_t CR3_HDSEL = 0x8;          // Half-duplex selection
    static constexpr uint32_t CR3_DMAT = 0x80;          // DMA enable transmitter
    static const uint32_t CR3_RESET_VALUE = 0x0;

    static constexpr uint8_t UART5 = 53; // UART5 global interrupt
};

static uart5_t& UART5 = *reinterpret_cast<uart5_t*>(0x40005000);

#define HAVE_PERIPHERAL_UART5


////
//
//    CRC calculation unit
//
////

struct crc_t
{
    volatile uint32_t    DR;                   // [Read-write] Data register
    volatile uint32_t    IDR;                  // [Read-write] Independent Data register
    volatile uint32_t    CR;                   // [Write-only] Control register


    static const uint32_t DR_RESET_VALUE = 0xffffffff;

    template<uint32_t X>
    static constexpr uint32_t IDR_IDR =                 // Independent Data register (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_RESET = 0x1;          // Reset bit
    static const uint32_t CR_RESET_VALUE = 0x0;
};

static crc_t& CRC = *reinterpret_cast<crc_t*>(0x40023000);

#define HAVE_PERIPHERAL_CRC


////
//
//    FLASH
//
////

struct flash_t
{
    volatile uint32_t    ACR;                  // Flash access control register
    volatile uint32_t    KEYR;                 // [Write-only] Flash key register
    volatile uint32_t    OPTKEYR;              // [Write-only] Flash option key register
    volatile uint32_t    SR;                   // Status register
    volatile uint32_t    CR;                   // [Read-write] Control register
    volatile uint32_t    AR;                   // [Write-only] Flash address register
    reserved_t<1>        _0;
    volatile uint32_t    OBR;                  // [Read-only] Option byte register
    volatile uint32_t    WRPR;                 // [Read-only] Write protection register

    template<uint32_t X>
    static constexpr uint32_t ACR_LATENCY =             // Latency (3 bits), Read-write
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t ACR_HLFCYA = 0x8;         // Flash half cycle access enable, Read-write
    static constexpr uint32_t ACR_PRFTBE = 0x10;        // Prefetch buffer enable, Read-write
    static constexpr uint32_t ACR_PRFTBS = 0x20;        // Prefetch buffer status, Read-only
    static const uint32_t ACR_RESET_VALUE = 0x30;


    static const uint32_t KEYR_RESET_VALUE = 0x0;


    static const uint32_t OPTKEYR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_EOP = 0x20;           // End of operation, Read-write
    static constexpr uint32_t SR_WRPRTERR = 0x10;      // Write protection error, Read-write
    static constexpr uint32_t SR_PGERR = 0x4;          // Programming error, Read-write
    static constexpr uint32_t SR_BSY = 0x1;            // Busy, Read-only
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_PG = 0x1;             // Programming
    static constexpr uint32_t CR_PER = 0x2;            // Page Erase
    static constexpr uint32_t CR_MER = 0x4;            // Mass Erase
    static constexpr uint32_t CR_OPTPG = 0x10;         // Option byte programming
    static constexpr uint32_t CR_OPTER = 0x20;         // Option byte erase
    static constexpr uint32_t CR_STRT = 0x40;          // Start
    static constexpr uint32_t CR_LOCK = 0x80;          // Lock
    static constexpr uint32_t CR_OPTWRE = 0x200;       // Option bytes write enable
    static constexpr uint32_t CR_ERRIE = 0x400;        // Error interrupt enable
    static constexpr uint32_t CR_EOPIE = 0x1000;       // End of operation interrupt enable
    static const uint32_t CR_RESET_VALUE = 0x80;


    static const uint32_t AR_RESET_VALUE = 0x0;

    static constexpr uint32_t OBR_OPTERR = 0x1;         // Option byte error
    static constexpr uint32_t OBR_RDPRT = 0x2;          // Read protection
    static constexpr uint32_t OBR_WDG_SW = 0x4;         // WDG_SW
    static constexpr uint32_t OBR_nRST_STOP = 0x8;      // nRST_STOP
    static constexpr uint32_t OBR_nRST_STDBY = 0x10;    // nRST_STDBY
    template<uint32_t X>
    static constexpr uint32_t OBR_Data0 =               // Data0 (8 bits)
        bit_field_t<10, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OBR_Data1 =               // Data1 (8 bits)
        bit_field_t<18, 0xff>::value<X>();
    static const uint32_t OBR_RESET_VALUE = 0x3fffffc;


    static const uint32_t WRPR_RESET_VALUE = 0xffffffff;

    static constexpr uint8_t FLASH = 4; // Flash global interrupt
};

static flash_t& FLASH = *reinterpret_cast<flash_t*>(0x40022000);

#define HAVE_PERIPHERAL_FLASH


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
    volatile uint32_t    ISTR;                 // [Read-write] interrupt status register
    volatile uint32_t    FNR;                  // [Read-only] frame number register
    volatile uint32_t    DADDR;                // [Read-write] device address
    volatile uint32_t    BTABLE;               // [Read-write] Buffer table address

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
    static constexpr uint32_t ISTR_EP_ID =               // Endpoint Identifier (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static constexpr uint32_t ISTR_DIR = 0x10;           // Direction of transaction
    static constexpr uint32_t ISTR_ESOF = 0x100;         // Expected start frame
    static constexpr uint32_t ISTR_SOF = 0x200;          // start of frame
    static constexpr uint32_t ISTR_RESET = 0x400;        // reset request
    static constexpr uint32_t ISTR_SUSP = 0x800;         // Suspend mode request
    static constexpr uint32_t ISTR_WKUP = 0x1000;        // Wakeup
    static constexpr uint32_t ISTR_ERR = 0x2000;         // Error
    static constexpr uint32_t ISTR_PMAOVR = 0x4000;      // Packet memory area over / underrun
    static constexpr uint32_t ISTR_CTR = 0x8000;         // Correct transfer
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

    static constexpr uint8_t USB_HP_CAN_TX = 19; // USB High Priority or CAN TX interrupts
    static constexpr uint8_t USB_LP_CAN_RX0 = 20; // USB Low Priority or CAN RX0 interrupts
};

static usb_t& USB = *reinterpret_cast<usb_t*>(0x40005c00);

#define HAVE_PERIPHERAL_USB


////
//
//    USB on the go full speed
//
////

struct otg_fs_device_t
{
    volatile uint32_t    FS_DCFG;              // [Read-write] OTG_FS device configuration register (OTG_FS_DCFG)
    volatile uint32_t    FS_DCTL;              // OTG_FS device control register (OTG_FS_DCTL)
    volatile uint32_t    FS_DSTS;              // [Read-only] OTG_FS device status register (OTG_FS_DSTS)
    reserved_t<1>        _0;
    volatile uint32_t    FS_DIEPMSK;           // [Read-write] OTG_FS device IN endpoint common interrupt mask register (OTG_FS_DIEPMSK)
    volatile uint32_t    FS_DOEPMSK;           // [Read-write] OTG_FS device OUT endpoint common interrupt mask register (OTG_FS_DOEPMSK)
    volatile uint32_t    FS_DAINT;             // [Read-only] OTG_FS device all endpoints interrupt register (OTG_FS_DAINT)
    volatile uint32_t    FS_DAINTMSK;          // [Read-write] OTG_FS all endpoints interrupt mask register (OTG_FS_DAINTMSK)
    reserved_t<2>        _1;
    volatile uint32_t    DVBUSDIS;             // [Read-write] OTG_FS device VBUS discharge time register
    volatile uint32_t    DVBUSPULSE;           // [Read-write] OTG_FS device VBUS pulsing time register
    reserved_t<1>        _2;
    volatile uint32_t    DIEPEMPMSK;           // [Read-write] OTG_FS device IN endpoint FIFO empty interrupt mask register
    reserved_t<50>       _3;
    volatile uint32_t    FS_DIEPCTL0;          // OTG_FS device control IN endpoint 0 control register (OTG_FS_DIEPCTL0)
    reserved_t<1>        _4;
    volatile uint32_t    DIEPINT0;             // device endpoint-x interrupt register
    reserved_t<1>        _5;
    volatile uint32_t    DIEPTSIZ0;            // [Read-write] device endpoint-0 transfer size register
    reserved_t<1>        _6;
    volatile uint32_t    DTXFSTS0;             // [Read-only] OTG_FS device IN endpoint transmit FIFO status register
    reserved_t<1>        _7;
    volatile uint32_t    DIEPCTL1;             // OTG device endpoint-1 control register
    reserved_t<1>        _8;
    volatile uint32_t    DIEPINT1;             // device endpoint-1 interrupt register
    reserved_t<1>        _9;
    volatile uint32_t    DIEPTSIZ1;            // [Read-write] device endpoint-1 transfer size register
    reserved_t<1>        _10;
    volatile uint32_t    DTXFSTS1;             // [Read-only] OTG_FS device IN endpoint transmit FIFO status register
    reserved_t<1>        _11;
    volatile uint32_t    DIEPCTL2;             // OTG device endpoint-2 control register
    reserved_t<1>        _12;
    volatile uint32_t    DIEPINT2;             // device endpoint-2 interrupt register
    reserved_t<1>        _13;
    volatile uint32_t    DIEPTSIZ2;            // [Read-write] device endpoint-2 transfer size register
    reserved_t<1>        _14;
    volatile uint32_t    DTXFSTS2;             // [Read-only] OTG_FS device IN endpoint transmit FIFO status register
    reserved_t<1>        _15;
    volatile uint32_t    DIEPCTL3;             // OTG device endpoint-3 control register
    reserved_t<1>        _16;
    volatile uint32_t    DIEPINT3;             // device endpoint-3 interrupt register
    reserved_t<1>        _17;
    volatile uint32_t    DIEPTSIZ3;            // [Read-write] device endpoint-3 transfer size register
    reserved_t<1>        _18;
    volatile uint32_t    DTXFSTS3;             // [Read-only] OTG_FS device IN endpoint transmit FIFO status register
    reserved_t<97>       _19;
    volatile uint32_t    DOEPCTL0;             // device endpoint-0 control register
    reserved_t<1>        _20;
    volatile uint32_t    DOEPINT0;             // [Read-write] device endpoint-0 interrupt register
    reserved_t<1>        _21;
    volatile uint32_t    DOEPTSIZ0;            // [Read-write] device OUT endpoint-0 transfer size register
    reserved_t<3>        _22;
    volatile uint32_t    DOEPCTL1;             // device endpoint-1 control register
    reserved_t<1>        _23;
    volatile uint32_t    DOEPINT1;             // [Read-write] device endpoint-1 interrupt register
    reserved_t<1>        _24;
    volatile uint32_t    DOEPTSIZ1;            // [Read-write] device OUT endpoint-1 transfer size register
    reserved_t<3>        _25;
    volatile uint32_t    DOEPCTL2;             // device endpoint-2 control register
    reserved_t<1>        _26;
    volatile uint32_t    DOEPINT2;             // [Read-write] device endpoint-2 interrupt register
    reserved_t<1>        _27;
    volatile uint32_t    DOEPTSIZ2;            // [Read-write] device OUT endpoint-2 transfer size register
    reserved_t<3>        _28;
    volatile uint32_t    DOEPCTL3;             // device endpoint-3 control register
    reserved_t<1>        _29;
    volatile uint32_t    DOEPINT3;             // [Read-write] device endpoint-3 interrupt register
    reserved_t<1>        _30;
    volatile uint32_t    DOEPTSIZ3;            // [Read-write] device OUT endpoint-3 transfer size register

    template<uint32_t X>
    static constexpr uint32_t FS_DCFG_DSPD =                // Device speed (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static constexpr uint32_t FS_DCFG_NZLSOHSK = 0x4;       // Non-zero-length status OUT handshake
    template<uint32_t X>
    static constexpr uint32_t FS_DCFG_DAD =                 // Device address (7 bits)
        bit_field_t<4, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_DCFG_PFIVL =               // Periodic frame interval (2 bits)
        bit_field_t<11, 0x3>::value<X>();
    static const uint32_t FS_DCFG_RESET_VALUE = 0x2200000;

    static constexpr uint32_t FS_DCTL_RWUSIG = 0x1;         // Remote wakeup signaling, Read-write
    static constexpr uint32_t FS_DCTL_SDIS = 0x2;           // Soft disconnect, Read-write
    static constexpr uint32_t FS_DCTL_GINSTS = 0x4;         // Global IN NAK status, Read-only
    static constexpr uint32_t FS_DCTL_GONSTS = 0x8;         // Global OUT NAK status, Read-only
    template<uint32_t X>
    static constexpr uint32_t FS_DCTL_TCTL =                // Test control (3 bits), Read-write
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t FS_DCTL_SGINAK = 0x80;        // Set global IN NAK, Read-write
    static constexpr uint32_t FS_DCTL_CGINAK = 0x100;       // Clear global IN NAK, Read-write
    static constexpr uint32_t FS_DCTL_SGONAK = 0x200;       // Set global OUT NAK, Read-write
    static constexpr uint32_t FS_DCTL_CGONAK = 0x400;       // Clear global OUT NAK, Read-write
    static constexpr uint32_t FS_DCTL_POPRGDNE = 0x800;     // Power-on programming done, Read-write
    static const uint32_t FS_DCTL_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_DSTS_SUSPSTS = 0x1;        // Suspend status
    template<uint32_t X>
    static constexpr uint32_t FS_DSTS_ENUMSPD =             // Enumerated speed (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t FS_DSTS_EERR = 0x8;           // Erratic error
    template<uint32_t X>
    static constexpr uint32_t FS_DSTS_FNSOF =               // Frame number of the received SOF (14 bits)
        bit_field_t<8, 0x3fff>::value<X>();
    static const uint32_t FS_DSTS_RESET_VALUE = 0x10;

    static constexpr uint32_t FS_DIEPMSK_XFRCM = 0x1;          // Transfer completed interrupt mask
    static constexpr uint32_t FS_DIEPMSK_EPDM = 0x2;           // Endpoint disabled interrupt mask
    static constexpr uint32_t FS_DIEPMSK_TOM = 0x8;            // Timeout condition mask (Non-isochronous endpoints)
    static constexpr uint32_t FS_DIEPMSK_ITTXFEMSK = 0x10;     // IN token received when TxFIFO empty mask
    static constexpr uint32_t FS_DIEPMSK_INEPNMM = 0x20;       // IN token received with EP mismatch mask
    static constexpr uint32_t FS_DIEPMSK_INEPNEM = 0x40;       // IN endpoint NAK effective mask
    static const uint32_t FS_DIEPMSK_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_DOEPMSK_XFRCM = 0x1;          // Transfer completed interrupt mask
    static constexpr uint32_t FS_DOEPMSK_EPDM = 0x2;           // Endpoint disabled interrupt mask
    static constexpr uint32_t FS_DOEPMSK_STUPM = 0x8;          // SETUP phase done mask
    static constexpr uint32_t FS_DOEPMSK_OTEPDM = 0x10;        // OUT token received when endpoint disabled mask
    static const uint32_t FS_DOEPMSK_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_DAINT_IEPINT =              // IN endpoint interrupt bits (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_DAINT_OEPINT =              // OUT endpoint interrupt bits (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t FS_DAINT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_DAINTMSK_IEPM =                // IN EP interrupt mask bits (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_DAINTMSK_OEPINT =              // OUT endpoint interrupt bits (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t FS_DAINTMSK_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DVBUSDIS_VBUSDT =              // Device VBUS discharge time (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DVBUSDIS_RESET_VALUE = 0x17d7;

    template<uint32_t X>
    static constexpr uint32_t DVBUSPULSE_DVBUSP =              // Device VBUS pulsing time (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t DVBUSPULSE_RESET_VALUE = 0x5b8;

    template<uint32_t X>
    static constexpr uint32_t DIEPEMPMSK_INEPTXFEM =           // IN EP Tx FIFO empty interrupt mask bits (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DIEPEMPMSK_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_DIEPCTL0_MPSIZ =               // Maximum packet size (2 bits), Read-write
        bit_field_t<0, 0x3>::value<X>();
    static constexpr uint32_t FS_DIEPCTL0_USBAEP = 0x8000;      // USB active endpoint, Read-only
    static constexpr uint32_t FS_DIEPCTL0_NAKSTS = 0x20000;     // NAK status, Read-only
    template<uint32_t X>
    static constexpr uint32_t FS_DIEPCTL0_EPTYP =               // Endpoint type (2 bits), Read-only
        bit_field_t<18, 0x3>::value<X>();
    static constexpr uint32_t FS_DIEPCTL0_STALL = 0x200000;     // STALL handshake, Read-write
    template<uint32_t X>
    static constexpr uint32_t FS_DIEPCTL0_TXFNUM =              // TxFIFO number (4 bits), Read-write
        bit_field_t<22, 0xf>::value<X>();
    static constexpr uint32_t FS_DIEPCTL0_CNAK = 0x4000000;     // Clear NAK, Write-only
    static constexpr uint32_t FS_DIEPCTL0_SNAK = 0x8000000;     // Set NAK, Write-only
    static constexpr uint32_t FS_DIEPCTL0_EPDIS = 0x40000000;   // Endpoint disable, Read-only
    static constexpr uint32_t FS_DIEPCTL0_EPENA = 0x80000000;   // Endpoint enable, Read-only
    static const uint32_t FS_DIEPCTL0_RESET_VALUE = 0x0;

    static constexpr uint32_t DIEPINT0_TXFE = 0x80;          // TXFE, Read-only
    static constexpr uint32_t DIEPINT0_INEPNE = 0x40;        // INEPNE, Read-write
    static constexpr uint32_t DIEPINT0_ITTXFE = 0x10;        // ITTXFE, Read-write
    static constexpr uint32_t DIEPINT0_TOC = 0x8;            // TOC, Read-write
    static constexpr uint32_t DIEPINT0_EPDISD = 0x2;         // EPDISD, Read-write
    static constexpr uint32_t DIEPINT0_XFRC = 0x1;           // XFRC, Read-write
    static const uint32_t DIEPINT0_RESET_VALUE = 0x80;

    template<uint32_t X>
    static constexpr uint32_t DIEPTSIZ0_PKTCNT =              // Packet count (2 bits)
        bit_field_t<19, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DIEPTSIZ0_XFRSIZ =              // Transfer size (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t DIEPTSIZ0_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DTXFSTS0_INEPTFSAV =           // IN endpoint TxFIFO space available (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DTXFSTS0_RESET_VALUE = 0x0;

    static constexpr uint32_t DIEPCTL1_EPENA = 0x80000000;   // EPENA, Read-write
    static constexpr uint32_t DIEPCTL1_EPDIS = 0x40000000;   // EPDIS, Read-write
    static constexpr uint32_t DIEPCTL1_SODDFRM_SD1PID = 0x20000000;// SODDFRM/SD1PID, Write-only
    static constexpr uint32_t DIEPCTL1_SD0PID_SEVNFRM = 0x10000000;// SD0PID/SEVNFRM, Write-only
    static constexpr uint32_t DIEPCTL1_SNAK = 0x8000000;     // SNAK, Write-only
    static constexpr uint32_t DIEPCTL1_CNAK = 0x4000000;     // CNAK, Write-only
    template<uint32_t X>
    static constexpr uint32_t DIEPCTL1_TXFNUM =              // TXFNUM (4 bits), Read-write
        bit_field_t<22, 0xf>::value<X>();
    static constexpr uint32_t DIEPCTL1_Stall = 0x200000;     // Stall, Read-write
    template<uint32_t X>
    static constexpr uint32_t DIEPCTL1_EPTYP =               // EPTYP (2 bits), Read-write
        bit_field_t<18, 0x3>::value<X>();
    static constexpr uint32_t DIEPCTL1_NAKSTS = 0x20000;     // NAKSTS, Read-only
    static constexpr uint32_t DIEPCTL1_EONUM_DPID = 0x10000; // EONUM/DPID, Read-only
    static constexpr uint32_t DIEPCTL1_USBAEP = 0x8000;      // USBAEP, Read-write
    template<uint32_t X>
    static constexpr uint32_t DIEPCTL1_MPSIZ =               // MPSIZ (11 bits), Read-write
        bit_field_t<0, 0x7ff>::value<X>();
    static const uint32_t DIEPCTL1_RESET_VALUE = 0x0;

    static constexpr uint32_t DIEPINT1_TXFE = 0x80;          // TXFE, Read-only
    static constexpr uint32_t DIEPINT1_INEPNE = 0x40;        // INEPNE, Read-write
    static constexpr uint32_t DIEPINT1_ITTXFE = 0x10;        // ITTXFE, Read-write
    static constexpr uint32_t DIEPINT1_TOC = 0x8;            // TOC, Read-write
    static constexpr uint32_t DIEPINT1_EPDISD = 0x2;         // EPDISD, Read-write
    static constexpr uint32_t DIEPINT1_XFRC = 0x1;           // XFRC, Read-write
    static const uint32_t DIEPINT1_RESET_VALUE = 0x80;

    template<uint32_t X>
    static constexpr uint32_t DIEPTSIZ1_MCNT =                // Multi count (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DIEPTSIZ1_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DIEPTSIZ1_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    static const uint32_t DIEPTSIZ1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DTXFSTS1_INEPTFSAV =           // IN endpoint TxFIFO space available (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DTXFSTS1_RESET_VALUE = 0x0;

    static constexpr uint32_t DIEPCTL2_EPENA = 0x80000000;   // EPENA, Read-write
    static constexpr uint32_t DIEPCTL2_EPDIS = 0x40000000;   // EPDIS, Read-write
    static constexpr uint32_t DIEPCTL2_SODDFRM = 0x20000000; // SODDFRM, Write-only
    static constexpr uint32_t DIEPCTL2_SD0PID_SEVNFRM = 0x10000000;// SD0PID/SEVNFRM, Write-only
    static constexpr uint32_t DIEPCTL2_SNAK = 0x8000000;     // SNAK, Write-only
    static constexpr uint32_t DIEPCTL2_CNAK = 0x4000000;     // CNAK, Write-only
    template<uint32_t X>
    static constexpr uint32_t DIEPCTL2_TXFNUM =              // TXFNUM (4 bits), Read-write
        bit_field_t<22, 0xf>::value<X>();
    static constexpr uint32_t DIEPCTL2_Stall = 0x200000;     // Stall, Read-write
    template<uint32_t X>
    static constexpr uint32_t DIEPCTL2_EPTYP =               // EPTYP (2 bits), Read-write
        bit_field_t<18, 0x3>::value<X>();
    static constexpr uint32_t DIEPCTL2_NAKSTS = 0x20000;     // NAKSTS, Read-only
    static constexpr uint32_t DIEPCTL2_EONUM_DPID = 0x10000; // EONUM/DPID, Read-only
    static constexpr uint32_t DIEPCTL2_USBAEP = 0x8000;      // USBAEP, Read-write
    template<uint32_t X>
    static constexpr uint32_t DIEPCTL2_MPSIZ =               // MPSIZ (11 bits), Read-write
        bit_field_t<0, 0x7ff>::value<X>();
    static const uint32_t DIEPCTL2_RESET_VALUE = 0x0;

    static constexpr uint32_t DIEPINT2_TXFE = 0x80;          // TXFE, Read-only
    static constexpr uint32_t DIEPINT2_INEPNE = 0x40;        // INEPNE, Read-write
    static constexpr uint32_t DIEPINT2_ITTXFE = 0x10;        // ITTXFE, Read-write
    static constexpr uint32_t DIEPINT2_TOC = 0x8;            // TOC, Read-write
    static constexpr uint32_t DIEPINT2_EPDISD = 0x2;         // EPDISD, Read-write
    static constexpr uint32_t DIEPINT2_XFRC = 0x1;           // XFRC, Read-write
    static const uint32_t DIEPINT2_RESET_VALUE = 0x80;

    template<uint32_t X>
    static constexpr uint32_t DIEPTSIZ2_MCNT =                // Multi count (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DIEPTSIZ2_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DIEPTSIZ2_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    static const uint32_t DIEPTSIZ2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DTXFSTS2_INEPTFSAV =           // IN endpoint TxFIFO space available (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DTXFSTS2_RESET_VALUE = 0x0;

    static constexpr uint32_t DIEPCTL3_EPENA = 0x80000000;   // EPENA, Read-write
    static constexpr uint32_t DIEPCTL3_EPDIS = 0x40000000;   // EPDIS, Read-write
    static constexpr uint32_t DIEPCTL3_SODDFRM = 0x20000000; // SODDFRM, Write-only
    static constexpr uint32_t DIEPCTL3_SD0PID_SEVNFRM = 0x10000000;// SD0PID/SEVNFRM, Write-only
    static constexpr uint32_t DIEPCTL3_SNAK = 0x8000000;     // SNAK, Write-only
    static constexpr uint32_t DIEPCTL3_CNAK = 0x4000000;     // CNAK, Write-only
    template<uint32_t X>
    static constexpr uint32_t DIEPCTL3_TXFNUM =              // TXFNUM (4 bits), Read-write
        bit_field_t<22, 0xf>::value<X>();
    static constexpr uint32_t DIEPCTL3_Stall = 0x200000;     // Stall, Read-write
    template<uint32_t X>
    static constexpr uint32_t DIEPCTL3_EPTYP =               // EPTYP (2 bits), Read-write
        bit_field_t<18, 0x3>::value<X>();
    static constexpr uint32_t DIEPCTL3_NAKSTS = 0x20000;     // NAKSTS, Read-only
    static constexpr uint32_t DIEPCTL3_EONUM_DPID = 0x10000; // EONUM/DPID, Read-only
    static constexpr uint32_t DIEPCTL3_USBAEP = 0x8000;      // USBAEP, Read-write
    template<uint32_t X>
    static constexpr uint32_t DIEPCTL3_MPSIZ =               // MPSIZ (11 bits), Read-write
        bit_field_t<0, 0x7ff>::value<X>();
    static const uint32_t DIEPCTL3_RESET_VALUE = 0x0;

    static constexpr uint32_t DIEPINT3_TXFE = 0x80;          // TXFE, Read-only
    static constexpr uint32_t DIEPINT3_INEPNE = 0x40;        // INEPNE, Read-write
    static constexpr uint32_t DIEPINT3_ITTXFE = 0x10;        // ITTXFE, Read-write
    static constexpr uint32_t DIEPINT3_TOC = 0x8;            // TOC, Read-write
    static constexpr uint32_t DIEPINT3_EPDISD = 0x2;         // EPDISD, Read-write
    static constexpr uint32_t DIEPINT3_XFRC = 0x1;           // XFRC, Read-write
    static const uint32_t DIEPINT3_RESET_VALUE = 0x80;

    template<uint32_t X>
    static constexpr uint32_t DIEPTSIZ3_MCNT =                // Multi count (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DIEPTSIZ3_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DIEPTSIZ3_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    static const uint32_t DIEPTSIZ3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DTXFSTS3_INEPTFSAV =           // IN endpoint TxFIFO space available (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DTXFSTS3_RESET_VALUE = 0x0;

    static constexpr uint32_t DOEPCTL0_EPENA = 0x80000000;   // EPENA, Write-only
    static constexpr uint32_t DOEPCTL0_EPDIS = 0x40000000;   // EPDIS, Read-only
    static constexpr uint32_t DOEPCTL0_SNAK = 0x8000000;     // SNAK, Write-only
    static constexpr uint32_t DOEPCTL0_CNAK = 0x4000000;     // CNAK, Write-only
    static constexpr uint32_t DOEPCTL0_Stall = 0x200000;     // Stall, Read-write
    static constexpr uint32_t DOEPCTL0_SNPM = 0x100000;      // SNPM, Read-write
    template<uint32_t X>
    static constexpr uint32_t DOEPCTL0_EPTYP =               // EPTYP (2 bits), Read-only
        bit_field_t<18, 0x3>::value<X>();
    static constexpr uint32_t DOEPCTL0_NAKSTS = 0x20000;     // NAKSTS, Read-only
    static constexpr uint32_t DOEPCTL0_USBAEP = 0x8000;      // USBAEP, Read-only
    template<uint32_t X>
    static constexpr uint32_t DOEPCTL0_MPSIZ =               // MPSIZ (2 bits), Read-only
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t DOEPCTL0_RESET_VALUE = 0x8000;

    static constexpr uint32_t DOEPINT0_B2BSTUP = 0x40;       // B2BSTUP
    static constexpr uint32_t DOEPINT0_OTEPDIS = 0x10;       // OTEPDIS
    static constexpr uint32_t DOEPINT0_STUP = 0x8;           // STUP
    static constexpr uint32_t DOEPINT0_EPDISD = 0x2;         // EPDISD
    static constexpr uint32_t DOEPINT0_XFRC = 0x1;           // XFRC
    static const uint32_t DOEPINT0_RESET_VALUE = 0x80;

    template<uint32_t X>
    static constexpr uint32_t DOEPTSIZ0_STUPCNT =             // SETUP packet count (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    static constexpr uint32_t DOEPTSIZ0_PKTCNT = 0x80000;     // Packet count
    template<uint32_t X>
    static constexpr uint32_t DOEPTSIZ0_XFRSIZ =              // Transfer size (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t DOEPTSIZ0_RESET_VALUE = 0x0;

    static constexpr uint32_t DOEPCTL1_EPENA = 0x80000000;   // EPENA, Read-write
    static constexpr uint32_t DOEPCTL1_EPDIS = 0x40000000;   // EPDIS, Read-write
    static constexpr uint32_t DOEPCTL1_SODDFRM = 0x20000000; // SODDFRM, Write-only
    static constexpr uint32_t DOEPCTL1_SD0PID_SEVNFRM = 0x10000000;// SD0PID/SEVNFRM, Write-only
    static constexpr uint32_t DOEPCTL1_SNAK = 0x8000000;     // SNAK, Write-only
    static constexpr uint32_t DOEPCTL1_CNAK = 0x4000000;     // CNAK, Write-only
    static constexpr uint32_t DOEPCTL1_Stall = 0x200000;     // Stall, Read-write
    static constexpr uint32_t DOEPCTL1_SNPM = 0x100000;      // SNPM, Read-write
    template<uint32_t X>
    static constexpr uint32_t DOEPCTL1_EPTYP =               // EPTYP (2 bits), Read-write
        bit_field_t<18, 0x3>::value<X>();
    static constexpr uint32_t DOEPCTL1_NAKSTS = 0x20000;     // NAKSTS, Read-only
    static constexpr uint32_t DOEPCTL1_EONUM_DPID = 0x10000; // EONUM/DPID, Read-only
    static constexpr uint32_t DOEPCTL1_USBAEP = 0x8000;      // USBAEP, Read-write
    template<uint32_t X>
    static constexpr uint32_t DOEPCTL1_MPSIZ =               // MPSIZ (11 bits), Read-write
        bit_field_t<0, 0x7ff>::value<X>();
    static const uint32_t DOEPCTL1_RESET_VALUE = 0x0;

    static constexpr uint32_t DOEPINT1_B2BSTUP = 0x40;       // B2BSTUP
    static constexpr uint32_t DOEPINT1_OTEPDIS = 0x10;       // OTEPDIS
    static constexpr uint32_t DOEPINT1_STUP = 0x8;           // STUP
    static constexpr uint32_t DOEPINT1_EPDISD = 0x2;         // EPDISD
    static constexpr uint32_t DOEPINT1_XFRC = 0x1;           // XFRC
    static const uint32_t DOEPINT1_RESET_VALUE = 0x80;

    template<uint32_t X>
    static constexpr uint32_t DOEPTSIZ1_RXDPID_STUPCNT =      // Received data PID/SETUP packet count (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DOEPTSIZ1_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DOEPTSIZ1_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    static const uint32_t DOEPTSIZ1_RESET_VALUE = 0x0;

    static constexpr uint32_t DOEPCTL2_EPENA = 0x80000000;   // EPENA, Read-write
    static constexpr uint32_t DOEPCTL2_EPDIS = 0x40000000;   // EPDIS, Read-write
    static constexpr uint32_t DOEPCTL2_SODDFRM = 0x20000000; // SODDFRM, Write-only
    static constexpr uint32_t DOEPCTL2_SD0PID_SEVNFRM = 0x10000000;// SD0PID/SEVNFRM, Write-only
    static constexpr uint32_t DOEPCTL2_SNAK = 0x8000000;     // SNAK, Write-only
    static constexpr uint32_t DOEPCTL2_CNAK = 0x4000000;     // CNAK, Write-only
    static constexpr uint32_t DOEPCTL2_Stall = 0x200000;     // Stall, Read-write
    static constexpr uint32_t DOEPCTL2_SNPM = 0x100000;      // SNPM, Read-write
    template<uint32_t X>
    static constexpr uint32_t DOEPCTL2_EPTYP =               // EPTYP (2 bits), Read-write
        bit_field_t<18, 0x3>::value<X>();
    static constexpr uint32_t DOEPCTL2_NAKSTS = 0x20000;     // NAKSTS, Read-only
    static constexpr uint32_t DOEPCTL2_EONUM_DPID = 0x10000; // EONUM/DPID, Read-only
    static constexpr uint32_t DOEPCTL2_USBAEP = 0x8000;      // USBAEP, Read-write
    template<uint32_t X>
    static constexpr uint32_t DOEPCTL2_MPSIZ =               // MPSIZ (11 bits), Read-write
        bit_field_t<0, 0x7ff>::value<X>();
    static const uint32_t DOEPCTL2_RESET_VALUE = 0x0;

    static constexpr uint32_t DOEPINT2_B2BSTUP = 0x40;       // B2BSTUP
    static constexpr uint32_t DOEPINT2_OTEPDIS = 0x10;       // OTEPDIS
    static constexpr uint32_t DOEPINT2_STUP = 0x8;           // STUP
    static constexpr uint32_t DOEPINT2_EPDISD = 0x2;         // EPDISD
    static constexpr uint32_t DOEPINT2_XFRC = 0x1;           // XFRC
    static const uint32_t DOEPINT2_RESET_VALUE = 0x80;

    template<uint32_t X>
    static constexpr uint32_t DOEPTSIZ2_RXDPID_STUPCNT =      // Received data PID/SETUP packet count (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DOEPTSIZ2_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DOEPTSIZ2_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    static const uint32_t DOEPTSIZ2_RESET_VALUE = 0x0;

    static constexpr uint32_t DOEPCTL3_EPENA = 0x80000000;   // EPENA, Read-write
    static constexpr uint32_t DOEPCTL3_EPDIS = 0x40000000;   // EPDIS, Read-write
    static constexpr uint32_t DOEPCTL3_SODDFRM = 0x20000000; // SODDFRM, Write-only
    static constexpr uint32_t DOEPCTL3_SD0PID_SEVNFRM = 0x10000000;// SD0PID/SEVNFRM, Write-only
    static constexpr uint32_t DOEPCTL3_SNAK = 0x8000000;     // SNAK, Write-only
    static constexpr uint32_t DOEPCTL3_CNAK = 0x4000000;     // CNAK, Write-only
    static constexpr uint32_t DOEPCTL3_Stall = 0x200000;     // Stall, Read-write
    static constexpr uint32_t DOEPCTL3_SNPM = 0x100000;      // SNPM, Read-write
    template<uint32_t X>
    static constexpr uint32_t DOEPCTL3_EPTYP =               // EPTYP (2 bits), Read-write
        bit_field_t<18, 0x3>::value<X>();
    static constexpr uint32_t DOEPCTL3_NAKSTS = 0x20000;     // NAKSTS, Read-only
    static constexpr uint32_t DOEPCTL3_EONUM_DPID = 0x10000; // EONUM/DPID, Read-only
    static constexpr uint32_t DOEPCTL3_USBAEP = 0x8000;      // USBAEP, Read-write
    template<uint32_t X>
    static constexpr uint32_t DOEPCTL3_MPSIZ =               // MPSIZ (11 bits), Read-write
        bit_field_t<0, 0x7ff>::value<X>();
    static const uint32_t DOEPCTL3_RESET_VALUE = 0x0;

    static constexpr uint32_t DOEPINT3_B2BSTUP = 0x40;       // B2BSTUP
    static constexpr uint32_t DOEPINT3_OTEPDIS = 0x10;       // OTEPDIS
    static constexpr uint32_t DOEPINT3_STUP = 0x8;           // STUP
    static constexpr uint32_t DOEPINT3_EPDISD = 0x2;         // EPDISD
    static constexpr uint32_t DOEPINT3_XFRC = 0x1;           // XFRC
    static const uint32_t DOEPINT3_RESET_VALUE = 0x80;

    template<uint32_t X>
    static constexpr uint32_t DOEPTSIZ3_RXDPID_STUPCNT =      // Received data PID/SETUP packet count (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DOEPTSIZ3_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DOEPTSIZ3_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    static const uint32_t DOEPTSIZ3_RESET_VALUE = 0x0;
};

static otg_fs_device_t& OTG_FS_DEVICE = *reinterpret_cast<otg_fs_device_t*>(0x50000800);

#define HAVE_PERIPHERAL_OTG_FS_DEVICE


////
//
//    USB on the go full speed
//
////

struct otg_fs_global_t
{
    volatile uint32_t    FS_GOTGCTL;           // OTG_FS control and status register (OTG_FS_GOTGCTL)
    volatile uint32_t    FS_GOTGINT;           // [Read-write] OTG_FS interrupt register (OTG_FS_GOTGINT)
    volatile uint32_t    FS_GAHBCFG;           // [Read-write] OTG_FS AHB configuration register (OTG_FS_GAHBCFG)
    volatile uint32_t    FS_GUSBCFG;           // OTG_FS USB configuration register (OTG_FS_GUSBCFG)
    volatile uint32_t    FS_GRSTCTL;           // OTG_FS reset register (OTG_FS_GRSTCTL)
    volatile uint32_t    FS_GINTSTS;           // OTG_FS core interrupt register (OTG_FS_GINTSTS)
    volatile uint32_t    FS_GINTMSK;           // OTG_FS interrupt mask register (OTG_FS_GINTMSK)
    volatile uint32_t    FS_GRXSTSR_Device;    // [Read-only] OTG_FS Receive status debug read(Device mode)
    reserved_t<1>        _0;
    volatile uint32_t    FS_GRXFSIZ;           // [Read-write] OTG_FS Receive FIFO size register (OTG_FS_GRXFSIZ)
    volatile uint32_t    FS_GNPTXFSIZ_Device;  // [Read-write] OTG_FS non-periodic transmit FIFO size register (Device mode)
    volatile uint32_t    FS_GNPTXSTS;          // [Read-only] OTG_FS non-periodic transmit FIFO/queue status register (OTG_FS_GNPTXSTS)
    reserved_t<2>        _1;
    volatile uint32_t    FS_GCCFG;             // [Read-write] OTG_FS general core configuration register (OTG_FS_GCCFG)
    volatile uint32_t    FS_CID;               // [Read-write] core ID register
    reserved_t<48>       _2;
    volatile uint32_t    FS_HPTXFSIZ;          // [Read-write] OTG_FS Host periodic transmit FIFO size register (OTG_FS_HPTXFSIZ)
    volatile uint32_t    FS_DIEPTXF1;          // [Read-write] OTG_FS device IN endpoint transmit FIFO size register (OTG_FS_DIEPTXF2)
    volatile uint32_t    FS_DIEPTXF2;          // [Read-write] OTG_FS device IN endpoint transmit FIFO size register (OTG_FS_DIEPTXF3)
    volatile uint32_t    FS_DIEPTXF3;          // [Read-write] OTG_FS device IN endpoint transmit FIFO size register (OTG_FS_DIEPTXF4)

    static constexpr uint32_t FS_GOTGCTL_SRQSCS = 0x1;         // Session request success, Read-only
    static constexpr uint32_t FS_GOTGCTL_SRQ = 0x2;            // Session request, Read-write
    static constexpr uint32_t FS_GOTGCTL_HNGSCS = 0x100;       // Host negotiation success, Read-only
    static constexpr uint32_t FS_GOTGCTL_HNPRQ = 0x200;        // HNP request, Read-write
    static constexpr uint32_t FS_GOTGCTL_HSHNPEN = 0x400;      // Host set HNP enable, Read-write
    static constexpr uint32_t FS_GOTGCTL_DHNPEN = 0x800;       // Device HNP enabled, Read-write
    static constexpr uint32_t FS_GOTGCTL_CIDSTS = 0x10000;     // Connector ID status, Read-only
    static constexpr uint32_t FS_GOTGCTL_DBCT = 0x20000;       // Long/short debounce time, Read-only
    static constexpr uint32_t FS_GOTGCTL_ASVLD = 0x40000;      // A-session valid, Read-only
    static constexpr uint32_t FS_GOTGCTL_BSVLD = 0x80000;      // B-session valid, Read-only
    static const uint32_t FS_GOTGCTL_RESET_VALUE = 0x800;

    static constexpr uint32_t FS_GOTGINT_SEDET = 0x4;          // Session end detected
    static constexpr uint32_t FS_GOTGINT_SRSSCHG = 0x100;      // Session request success status change
    static constexpr uint32_t FS_GOTGINT_HNSSCHG = 0x200;      // Host negotiation success status change
    static constexpr uint32_t FS_GOTGINT_HNGDET = 0x20000;     // Host negotiation detected
    static constexpr uint32_t FS_GOTGINT_ADTOCHG = 0x40000;    // A-device timeout change
    static constexpr uint32_t FS_GOTGINT_DBCDNE = 0x80000;     // Debounce done
    static const uint32_t FS_GOTGINT_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_GAHBCFG_GINT = 0x1;           // Global interrupt mask
    static constexpr uint32_t FS_GAHBCFG_TXFELVL = 0x80;       // TxFIFO empty level
    static constexpr uint32_t FS_GAHBCFG_PTXFELVL = 0x100;     // Periodic TxFIFO empty level
    static const uint32_t FS_GAHBCFG_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_GUSBCFG_TOCAL =               // FS timeout calibration (3 bits), Read-write
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t FS_GUSBCFG_PHYSEL = 0x40;        // Full Speed serial transceiver select, Write-only
    static constexpr uint32_t FS_GUSBCFG_SRPCAP = 0x100;       // SRP-capable, Read-write
    static constexpr uint32_t FS_GUSBCFG_HNPCAP = 0x200;       // HNP-capable, Read-write
    template<uint32_t X>
    static constexpr uint32_t FS_GUSBCFG_TRDT =                // USB turnaround time (4 bits), Read-write
        bit_field_t<10, 0xf>::value<X>();
    static constexpr uint32_t FS_GUSBCFG_FHMOD = 0x20000000;   // Force host mode, Read-write
    static constexpr uint32_t FS_GUSBCFG_FDMOD = 0x40000000;   // Force device mode, Read-write
    static constexpr uint32_t FS_GUSBCFG_CTXPKT = 0x80000000;  // Corrupt Tx packet, Read-write
    static const uint32_t FS_GUSBCFG_RESET_VALUE = 0xa00;

    static constexpr uint32_t FS_GRSTCTL_CSRST = 0x1;          // Core soft reset, Read-write
    static constexpr uint32_t FS_GRSTCTL_HSRST = 0x2;          // HCLK soft reset, Read-write
    static constexpr uint32_t FS_GRSTCTL_FCRST = 0x4;          // Host frame counter reset, Read-write
    static constexpr uint32_t FS_GRSTCTL_RXFFLSH = 0x10;       // RxFIFO flush, Read-write
    static constexpr uint32_t FS_GRSTCTL_TXFFLSH = 0x20;       // TxFIFO flush, Read-write
    template<uint32_t X>
    static constexpr uint32_t FS_GRSTCTL_TXFNUM =              // TxFIFO number (5 bits), Read-write
        bit_field_t<6, 0x1f>::value<X>();
    static constexpr uint32_t FS_GRSTCTL_AHBIDL = 0x80000000;  // AHB master idle, Read-only
    static const uint32_t FS_GRSTCTL_RESET_VALUE = 0x20000000;

    static constexpr uint32_t FS_GINTSTS_CMOD = 0x1;           // Current mode of operation, Read-only
    static constexpr uint32_t FS_GINTSTS_MMIS = 0x2;           // Mode mismatch interrupt, Read-write
    static constexpr uint32_t FS_GINTSTS_OTGINT = 0x4;         // OTG interrupt, Read-only
    static constexpr uint32_t FS_GINTSTS_SOF = 0x8;            // Start of frame, Read-write
    static constexpr uint32_t FS_GINTSTS_RXFLVL = 0x10;        // RxFIFO non-empty, Read-only
    static constexpr uint32_t FS_GINTSTS_NPTXFE = 0x20;        // Non-periodic TxFIFO empty, Read-only
    static constexpr uint32_t FS_GINTSTS_GINAKEFF = 0x40;      // Global IN non-periodic NAK effective, Read-only
    static constexpr uint32_t FS_GINTSTS_GOUTNAKEFF = 0x80;    // Global OUT NAK effective, Read-only
    static constexpr uint32_t FS_GINTSTS_ESUSP = 0x400;        // Early suspend, Read-write
    static constexpr uint32_t FS_GINTSTS_USBSUSP = 0x800;      // USB suspend, Read-write
    static constexpr uint32_t FS_GINTSTS_USBRST = 0x1000;      // USB reset, Read-write
    static constexpr uint32_t FS_GINTSTS_ENUMDNE = 0x2000;     // Enumeration done, Read-write
    static constexpr uint32_t FS_GINTSTS_ISOODRP = 0x4000;     // Isochronous OUT packet dropped interrupt, Read-write
    static constexpr uint32_t FS_GINTSTS_EOPF = 0x8000;        // End of periodic frame interrupt, Read-write
    static constexpr uint32_t FS_GINTSTS_IEPINT = 0x40000;     // IN endpoint interrupt, Read-only
    static constexpr uint32_t FS_GINTSTS_OEPINT = 0x80000;     // OUT endpoint interrupt, Read-only
    static constexpr uint32_t FS_GINTSTS_IISOIXFR = 0x100000;  // Incomplete isochronous IN transfer, Read-write
    static constexpr uint32_t FS_GINTSTS_IPXFR_INCOMPISOOUT = 0x200000;// Incomplete periodic transfer(Host mode)/Incomplete isochronous OUT transfer(Device mode), Read-write
    static constexpr uint32_t FS_GINTSTS_HPRTINT = 0x1000000;  // Host port interrupt, Read-only
    static constexpr uint32_t FS_GINTSTS_HCINT = 0x2000000;    // Host channels interrupt, Read-only
    static constexpr uint32_t FS_GINTSTS_PTXFE = 0x4000000;    // Periodic TxFIFO empty, Read-only
    static constexpr uint32_t FS_GINTSTS_CIDSCHG = 0x10000000; // Connector ID status change, Read-write
    static constexpr uint32_t FS_GINTSTS_DISCINT = 0x20000000; // Disconnect detected interrupt, Read-write
    static constexpr uint32_t FS_GINTSTS_SRQINT = 0x40000000;  // Session request/new session detected interrupt, Read-write
    static constexpr uint32_t FS_GINTSTS_WKUPINT = 0x80000000; // Resume/remote wakeup detected interrupt, Read-write
    static const uint32_t FS_GINTSTS_RESET_VALUE = 0x4000020;

    static constexpr uint32_t FS_GINTMSK_MMISM = 0x2;          // Mode mismatch interrupt mask, Read-write
    static constexpr uint32_t FS_GINTMSK_OTGINT = 0x4;         // OTG interrupt mask, Read-write
    static constexpr uint32_t FS_GINTMSK_SOFM = 0x8;           // Start of frame mask, Read-write
    static constexpr uint32_t FS_GINTMSK_RXFLVLM = 0x10;       // Receive FIFO non-empty mask, Read-write
    static constexpr uint32_t FS_GINTMSK_NPTXFEM = 0x20;       // Non-periodic TxFIFO empty mask, Read-write
    static constexpr uint32_t FS_GINTMSK_GINAKEFFM = 0x40;     // Global non-periodic IN NAK effective mask, Read-write
    static constexpr uint32_t FS_GINTMSK_GONAKEFFM = 0x80;     // Global OUT NAK effective mask, Read-write
    static constexpr uint32_t FS_GINTMSK_ESUSPM = 0x400;       // Early suspend mask, Read-write
    static constexpr uint32_t FS_GINTMSK_USBSUSPM = 0x800;     // USB suspend mask, Read-write
    static constexpr uint32_t FS_GINTMSK_USBRST = 0x1000;      // USB reset mask, Read-write
    static constexpr uint32_t FS_GINTMSK_ENUMDNEM = 0x2000;    // Enumeration done mask, Read-write
    static constexpr uint32_t FS_GINTMSK_ISOODRPM = 0x4000;    // Isochronous OUT packet dropped interrupt mask, Read-write
    static constexpr uint32_t FS_GINTMSK_EOPFM = 0x8000;       // End of periodic frame interrupt mask, Read-write
    static constexpr uint32_t FS_GINTMSK_EPMISM = 0x20000;     // Endpoint mismatch interrupt mask, Read-write
    static constexpr uint32_t FS_GINTMSK_IEPINT = 0x40000;     // IN endpoints interrupt mask, Read-write
    static constexpr uint32_t FS_GINTMSK_OEPINT = 0x80000;     // OUT endpoints interrupt mask, Read-write
    static constexpr uint32_t FS_GINTMSK_IISOIXFRM = 0x100000; // Incomplete isochronous IN transfer mask, Read-write
    static constexpr uint32_t FS_GINTMSK_IPXFRM_IISOOXFRM = 0x200000;// Incomplete periodic transfer mask(Host mode)/Incomplete isochronous OUT transfer mask(Device mode), Read-write
    static constexpr uint32_t FS_GINTMSK_PRTIM = 0x1000000;    // Host port interrupt mask, Read-only
    static constexpr uint32_t FS_GINTMSK_HCIM = 0x2000000;     // Host channels interrupt mask, Read-write
    static constexpr uint32_t FS_GINTMSK_PTXFEM = 0x4000000;   // Periodic TxFIFO empty mask, Read-write
    static constexpr uint32_t FS_GINTMSK_CIDSCHGM = 0x10000000;// Connector ID status change mask, Read-write
    static constexpr uint32_t FS_GINTMSK_DISCINT = 0x20000000; // Disconnect detected interrupt mask, Read-write
    static constexpr uint32_t FS_GINTMSK_SRQIM = 0x40000000;   // Session request/new session detected interrupt mask, Read-write
    static constexpr uint32_t FS_GINTMSK_WUIM = 0x80000000;    // Resume/remote wakeup detected interrupt mask, Read-write
    static const uint32_t FS_GINTMSK_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_GRXSTSR_Device_BCNT =                // Byte count (11 bits)
        bit_field_t<4, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_GRXSTSR_Device_DPID =                // Data PID (2 bits)
        bit_field_t<15, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_GRXSTSR_Device_EPNUM =               // Endpoint number (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_GRXSTSR_Device_FRMNUM =              // Frame number (4 bits)
        bit_field_t<21, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_GRXSTSR_Device_PKTSTS =              // Packet status (4 bits)
        bit_field_t<17, 0xf>::value<X>();
    static const uint32_t FS_GRXSTSR_Device_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_GRXFSIZ_RXFD =                // RxFIFO depth (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t FS_GRXFSIZ_RESET_VALUE = 0x200;

    template<uint32_t X>
    static constexpr uint32_t FS_GNPTXFSIZ_Device_NPTXFD =              // Non-periodic TxFIFO depth (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_GNPTXFSIZ_Device_NPTXFSA =             // Non-periodic transmit RAM start address (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_GNPTXFSIZ_Device_TX0FD =               // Endpoint 0 TxFIFO depth (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_GNPTXFSIZ_Device_TX0FSA =              // Endpoint 0 transmit RAM start address (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t FS_GNPTXFSIZ_Device_RESET_VALUE = 0x200;

    template<uint32_t X>
    static constexpr uint32_t FS_GNPTXSTS_NPTXFSAV =            // Non-periodic TxFIFO space available (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_GNPTXSTS_NPTQXSAV =            // Non-periodic transmit request queue space available (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_GNPTXSTS_NPTXQTOP =            // Top of the non-periodic transmit request queue (7 bits)
        bit_field_t<24, 0x7f>::value<X>();
    static const uint32_t FS_GNPTXSTS_RESET_VALUE = 0x80200;

    static constexpr uint32_t FS_GCCFG_PWRDWN = 0x10000;     // Power down
    static constexpr uint32_t FS_GCCFG_VBUSASEN = 0x40000;   // Enable the VBUS sensing device
    static constexpr uint32_t FS_GCCFG_VBUSBSEN = 0x80000;   // Enable the VBUS sensing device
    static constexpr uint32_t FS_GCCFG_SOFOUTEN = 0x100000;  // SOF output enable
    static const uint32_t FS_GCCFG_RESET_VALUE = 0x0;


    static const uint32_t FS_CID_RESET_VALUE = 0x1000;

    template<uint32_t X>
    static constexpr uint32_t FS_HPTXFSIZ_PTXSA =               // Host periodic TxFIFO start address (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HPTXFSIZ_PTXFSIZ =             // Host periodic TxFIFO depth (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t FS_HPTXFSIZ_RESET_VALUE = 0x2000600;

    template<uint32_t X>
    static constexpr uint32_t FS_DIEPTXF1_INEPTXSA =            // IN endpoint FIFO2 transmit RAM start address (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_DIEPTXF1_INEPTXFD =            // IN endpoint TxFIFO depth (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t FS_DIEPTXF1_RESET_VALUE = 0x2000400;

    template<uint32_t X>
    static constexpr uint32_t FS_DIEPTXF2_INEPTXSA =            // IN endpoint FIFO3 transmit RAM start address (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_DIEPTXF2_INEPTXFD =            // IN endpoint TxFIFO depth (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t FS_DIEPTXF2_RESET_VALUE = 0x2000400;

    template<uint32_t X>
    static constexpr uint32_t FS_DIEPTXF3_INEPTXSA =            // IN endpoint FIFO4 transmit RAM start address (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_DIEPTXF3_INEPTXFD =            // IN endpoint TxFIFO depth (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t FS_DIEPTXF3_RESET_VALUE = 0x2000400;
};

static otg_fs_global_t& OTG_FS_GLOBAL = *reinterpret_cast<otg_fs_global_t*>(0x50000000);

#define HAVE_PERIPHERAL_OTG_FS_GLOBAL


////
//
//    USB on the go full speed
//
////

struct otg_fs_host_t
{
    volatile uint32_t    FS_HCFG;              // OTG_FS host configuration register (OTG_FS_HCFG)
    volatile uint32_t    HFIR;                 // [Read-write] OTG_FS Host frame interval register
    volatile uint32_t    FS_HFNUM;             // [Read-only] OTG_FS host frame number/frame time remaining register (OTG_FS_HFNUM)
    reserved_t<1>        _0;
    volatile uint32_t    FS_HPTXSTS;           // OTG_FS_Host periodic transmit FIFO/queue status register (OTG_FS_HPTXSTS)
    volatile uint32_t    HAINT;                // [Read-only] OTG_FS Host all channels interrupt register
    volatile uint32_t    HAINTMSK;             // [Read-write] OTG_FS host all channels interrupt mask register
    reserved_t<9>        _1;
    volatile uint32_t    FS_HPRT;              // OTG_FS host port control and status register (OTG_FS_HPRT)
    reserved_t<47>       _2;
    volatile uint32_t    FS_HCCHAR0;           // [Read-write] OTG_FS host channel-0 characteristics register (OTG_FS_HCCHAR0)
    reserved_t<1>        _3;
    volatile uint32_t    FS_HCINT0;            // [Read-write] OTG_FS host channel-0 interrupt register (OTG_FS_HCINT0)
    volatile uint32_t    FS_HCINTMSK0;         // [Read-write] OTG_FS host channel-0 mask register (OTG_FS_HCINTMSK0)
    volatile uint32_t    FS_HCTSIZ0;           // [Read-write] OTG_FS host channel-0 transfer size register
    reserved_t<3>        _4;
    volatile uint32_t    FS_HCCHAR1;           // [Read-write] OTG_FS host channel-1 characteristics register (OTG_FS_HCCHAR1)
    reserved_t<1>        _5;
    volatile uint32_t    FS_HCINT1;            // [Read-write] OTG_FS host channel-1 interrupt register (OTG_FS_HCINT1)
    volatile uint32_t    FS_HCINTMSK1;         // [Read-write] OTG_FS host channel-1 mask register (OTG_FS_HCINTMSK1)
    volatile uint32_t    FS_HCTSIZ1;           // [Read-write] OTG_FS host channel-1 transfer size register
    reserved_t<3>        _6;
    volatile uint32_t    FS_HCCHAR2;           // [Read-write] OTG_FS host channel-2 characteristics register (OTG_FS_HCCHAR2)
    reserved_t<1>        _7;
    volatile uint32_t    FS_HCINT2;            // [Read-write] OTG_FS host channel-2 interrupt register (OTG_FS_HCINT2)
    volatile uint32_t    FS_HCINTMSK2;         // [Read-write] OTG_FS host channel-2 mask register (OTG_FS_HCINTMSK2)
    volatile uint32_t    FS_HCTSIZ2;           // [Read-write] OTG_FS host channel-2 transfer size register
    reserved_t<3>        _8;
    volatile uint32_t    FS_HCCHAR3;           // [Read-write] OTG_FS host channel-3 characteristics register (OTG_FS_HCCHAR3)
    reserved_t<1>        _9;
    volatile uint32_t    FS_HCINT3;            // [Read-write] OTG_FS host channel-3 interrupt register (OTG_FS_HCINT3)
    volatile uint32_t    FS_HCINTMSK3;         // [Read-write] OTG_FS host channel-3 mask register (OTG_FS_HCINTMSK3)
    volatile uint32_t    FS_HCTSIZ3;           // [Read-write] OTG_FS host channel-3 transfer size register
    reserved_t<3>        _10;
    volatile uint32_t    FS_HCCHAR4;           // [Read-write] OTG_FS host channel-4 characteristics register (OTG_FS_HCCHAR4)
    reserved_t<1>        _11;
    volatile uint32_t    FS_HCINT4;            // [Read-write] OTG_FS host channel-4 interrupt register (OTG_FS_HCINT4)
    volatile uint32_t    FS_HCINTMSK4;         // [Read-write] OTG_FS host channel-4 mask register (OTG_FS_HCINTMSK4)
    volatile uint32_t    FS_HCTSIZ4;           // [Read-write] OTG_FS host channel-x transfer size register
    reserved_t<3>        _12;
    volatile uint32_t    FS_HCCHAR5;           // [Read-write] OTG_FS host channel-5 characteristics register (OTG_FS_HCCHAR5)
    reserved_t<1>        _13;
    volatile uint32_t    FS_HCINT5;            // [Read-write] OTG_FS host channel-5 interrupt register (OTG_FS_HCINT5)
    volatile uint32_t    FS_HCINTMSK5;         // [Read-write] OTG_FS host channel-5 mask register (OTG_FS_HCINTMSK5)
    volatile uint32_t    FS_HCTSIZ5;           // [Read-write] OTG_FS host channel-5 transfer size register
    reserved_t<3>        _14;
    volatile uint32_t    FS_HCCHAR6;           // [Read-write] OTG_FS host channel-6 characteristics register (OTG_FS_HCCHAR6)
    reserved_t<1>        _15;
    volatile uint32_t    FS_HCINT6;            // [Read-write] OTG_FS host channel-6 interrupt register (OTG_FS_HCINT6)
    volatile uint32_t    FS_HCINTMSK6;         // [Read-write] OTG_FS host channel-6 mask register (OTG_FS_HCINTMSK6)
    volatile uint32_t    FS_HCTSIZ6;           // [Read-write] OTG_FS host channel-6 transfer size register
    reserved_t<3>        _16;
    volatile uint32_t    FS_HCCHAR7;           // [Read-write] OTG_FS host channel-7 characteristics register (OTG_FS_HCCHAR7)
    reserved_t<1>        _17;
    volatile uint32_t    FS_HCINT7;            // [Read-write] OTG_FS host channel-7 interrupt register (OTG_FS_HCINT7)
    volatile uint32_t    FS_HCINTMSK7;         // [Read-write] OTG_FS host channel-7 mask register (OTG_FS_HCINTMSK7)
    volatile uint32_t    FS_HCTSIZ7;           // [Read-write] OTG_FS host channel-7 transfer size register

    template<uint32_t X>
    static constexpr uint32_t FS_HCFG_FSLSPCS =             // FS/LS PHY clock select (2 bits), Read-write
        bit_field_t<0, 0x3>::value<X>();
    static constexpr uint32_t FS_HCFG_FSLSS = 0x4;          // FS- and LS-only support, Read-only
    static const uint32_t FS_HCFG_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HFIR_FRIVL =               // Frame interval (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t HFIR_RESET_VALUE = 0xea60;

    template<uint32_t X>
    static constexpr uint32_t FS_HFNUM_FRNUM =               // Frame number (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HFNUM_FTREM =               // Frame time remaining (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t FS_HFNUM_RESET_VALUE = 0x3fff;

    template<uint32_t X>
    static constexpr uint32_t FS_HPTXSTS_PTXFSAVL =            // Periodic transmit data FIFO space available (16 bits), Read-write
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HPTXSTS_PTXQSAV =             // Periodic transmit request queue space available (8 bits), Read-only
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HPTXSTS_PTXQTOP =             // Top of the periodic transmit request queue (8 bits), Read-only
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t FS_HPTXSTS_RESET_VALUE = 0x80100;

    template<uint32_t X>
    static constexpr uint32_t HAINT_HAINT =               // Channel interrupts (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t HAINT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HAINTMSK_HAINTM =              // Channel interrupt mask (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t HAINTMSK_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HPRT_PCSTS = 0x1;          // Port connect status, Read-only
    static constexpr uint32_t FS_HPRT_PCDET = 0x2;          // Port connect detected, Read-write
    static constexpr uint32_t FS_HPRT_PENA = 0x4;           // Port enable, Read-write
    static constexpr uint32_t FS_HPRT_PENCHNG = 0x8;        // Port enable/disable change, Read-write
    static constexpr uint32_t FS_HPRT_POCA = 0x10;          // Port overcurrent active, Read-only
    static constexpr uint32_t FS_HPRT_POCCHNG = 0x20;       // Port overcurrent change, Read-write
    static constexpr uint32_t FS_HPRT_PRES = 0x40;          // Port resume, Read-write
    static constexpr uint32_t FS_HPRT_PSUSP = 0x80;         // Port suspend, Read-write
    static constexpr uint32_t FS_HPRT_PRST = 0x100;         // Port reset, Read-write
    template<uint32_t X>
    static constexpr uint32_t FS_HPRT_PLSTS =               // Port line status (2 bits), Read-only
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t FS_HPRT_PPWR = 0x1000;        // Port power, Read-write
    template<uint32_t X>
    static constexpr uint32_t FS_HPRT_PTCTL =               // Port test control (4 bits), Read-write
        bit_field_t<13, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HPRT_PSPD =                // Port speed (2 bits), Read-only
        bit_field_t<17, 0x3>::value<X>();
    static const uint32_t FS_HPRT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR0_MPSIZ =               // Maximum packet size (11 bits)
        bit_field_t<0, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR0_EPNUM =               // Endpoint number (4 bits)
        bit_field_t<11, 0xf>::value<X>();
    static constexpr uint32_t FS_HCCHAR0_EPDIR = 0x8000;       // Endpoint direction
    static constexpr uint32_t FS_HCCHAR0_LSDEV = 0x20000;      // Low-speed device
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR0_EPTYP =               // Endpoint type (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR0_MCNT =                // Multicount (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR0_DAD =                 // Device address (7 bits)
        bit_field_t<22, 0x7f>::value<X>();
    static constexpr uint32_t FS_HCCHAR0_ODDFRM = 0x20000000;  // Odd frame
    static constexpr uint32_t FS_HCCHAR0_CHDIS = 0x40000000;   // Channel disable
    static constexpr uint32_t FS_HCCHAR0_CHENA = 0x80000000;   // Channel enable
    static const uint32_t FS_HCCHAR0_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINT0_XFRC = 0x1;           // Transfer completed
    static constexpr uint32_t FS_HCINT0_CHH = 0x2;            // Channel halted
    static constexpr uint32_t FS_HCINT0_STALL = 0x8;          // STALL response received interrupt
    static constexpr uint32_t FS_HCINT0_NAK = 0x10;           // NAK response received interrupt
    static constexpr uint32_t FS_HCINT0_ACK = 0x20;           // ACK response received/transmitted interrupt
    static constexpr uint32_t FS_HCINT0_TXERR = 0x80;         // Transaction error
    static constexpr uint32_t FS_HCINT0_BBERR = 0x100;        // Babble error
    static constexpr uint32_t FS_HCINT0_FRMOR = 0x200;        // Frame overrun
    static constexpr uint32_t FS_HCINT0_DTERR = 0x400;        // Data toggle error
    static const uint32_t FS_HCINT0_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINTMSK0_XFRCM = 0x1;          // Transfer completed mask
    static constexpr uint32_t FS_HCINTMSK0_CHHM = 0x2;           // Channel halted mask
    static constexpr uint32_t FS_HCINTMSK0_STALLM = 0x8;         // STALL response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK0_NAKM = 0x10;          // NAK response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK0_ACKM = 0x20;          // ACK response received/transmitted interrupt mask
    static constexpr uint32_t FS_HCINTMSK0_NYET = 0x40;          // response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK0_TXERRM = 0x80;        // Transaction error mask
    static constexpr uint32_t FS_HCINTMSK0_BBERRM = 0x100;       // Babble error mask
    static constexpr uint32_t FS_HCINTMSK0_FRMORM = 0x200;       // Frame overrun mask
    static constexpr uint32_t FS_HCINTMSK0_DTERRM = 0x400;       // Data toggle error mask
    static const uint32_t FS_HCINTMSK0_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ0_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ0_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ0_DPID =                // Data PID (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    static const uint32_t FS_HCTSIZ0_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR1_MPSIZ =               // Maximum packet size (11 bits)
        bit_field_t<0, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR1_EPNUM =               // Endpoint number (4 bits)
        bit_field_t<11, 0xf>::value<X>();
    static constexpr uint32_t FS_HCCHAR1_EPDIR = 0x8000;       // Endpoint direction
    static constexpr uint32_t FS_HCCHAR1_LSDEV = 0x20000;      // Low-speed device
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR1_EPTYP =               // Endpoint type (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR1_MCNT =                // Multicount (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR1_DAD =                 // Device address (7 bits)
        bit_field_t<22, 0x7f>::value<X>();
    static constexpr uint32_t FS_HCCHAR1_ODDFRM = 0x20000000;  // Odd frame
    static constexpr uint32_t FS_HCCHAR1_CHDIS = 0x40000000;   // Channel disable
    static constexpr uint32_t FS_HCCHAR1_CHENA = 0x80000000;   // Channel enable
    static const uint32_t FS_HCCHAR1_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINT1_XFRC = 0x1;           // Transfer completed
    static constexpr uint32_t FS_HCINT1_CHH = 0x2;            // Channel halted
    static constexpr uint32_t FS_HCINT1_STALL = 0x8;          // STALL response received interrupt
    static constexpr uint32_t FS_HCINT1_NAK = 0x10;           // NAK response received interrupt
    static constexpr uint32_t FS_HCINT1_ACK = 0x20;           // ACK response received/transmitted interrupt
    static constexpr uint32_t FS_HCINT1_TXERR = 0x80;         // Transaction error
    static constexpr uint32_t FS_HCINT1_BBERR = 0x100;        // Babble error
    static constexpr uint32_t FS_HCINT1_FRMOR = 0x200;        // Frame overrun
    static constexpr uint32_t FS_HCINT1_DTERR = 0x400;        // Data toggle error
    static const uint32_t FS_HCINT1_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINTMSK1_XFRCM = 0x1;          // Transfer completed mask
    static constexpr uint32_t FS_HCINTMSK1_CHHM = 0x2;           // Channel halted mask
    static constexpr uint32_t FS_HCINTMSK1_STALLM = 0x8;         // STALL response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK1_NAKM = 0x10;          // NAK response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK1_ACKM = 0x20;          // ACK response received/transmitted interrupt mask
    static constexpr uint32_t FS_HCINTMSK1_NYET = 0x40;          // response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK1_TXERRM = 0x80;        // Transaction error mask
    static constexpr uint32_t FS_HCINTMSK1_BBERRM = 0x100;       // Babble error mask
    static constexpr uint32_t FS_HCINTMSK1_FRMORM = 0x200;       // Frame overrun mask
    static constexpr uint32_t FS_HCINTMSK1_DTERRM = 0x400;       // Data toggle error mask
    static const uint32_t FS_HCINTMSK1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ1_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ1_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ1_DPID =                // Data PID (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    static const uint32_t FS_HCTSIZ1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR2_MPSIZ =               // Maximum packet size (11 bits)
        bit_field_t<0, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR2_EPNUM =               // Endpoint number (4 bits)
        bit_field_t<11, 0xf>::value<X>();
    static constexpr uint32_t FS_HCCHAR2_EPDIR = 0x8000;       // Endpoint direction
    static constexpr uint32_t FS_HCCHAR2_LSDEV = 0x20000;      // Low-speed device
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR2_EPTYP =               // Endpoint type (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR2_MCNT =                // Multicount (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR2_DAD =                 // Device address (7 bits)
        bit_field_t<22, 0x7f>::value<X>();
    static constexpr uint32_t FS_HCCHAR2_ODDFRM = 0x20000000;  // Odd frame
    static constexpr uint32_t FS_HCCHAR2_CHDIS = 0x40000000;   // Channel disable
    static constexpr uint32_t FS_HCCHAR2_CHENA = 0x80000000;   // Channel enable
    static const uint32_t FS_HCCHAR2_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINT2_XFRC = 0x1;           // Transfer completed
    static constexpr uint32_t FS_HCINT2_CHH = 0x2;            // Channel halted
    static constexpr uint32_t FS_HCINT2_STALL = 0x8;          // STALL response received interrupt
    static constexpr uint32_t FS_HCINT2_NAK = 0x10;           // NAK response received interrupt
    static constexpr uint32_t FS_HCINT2_ACK = 0x20;           // ACK response received/transmitted interrupt
    static constexpr uint32_t FS_HCINT2_TXERR = 0x80;         // Transaction error
    static constexpr uint32_t FS_HCINT2_BBERR = 0x100;        // Babble error
    static constexpr uint32_t FS_HCINT2_FRMOR = 0x200;        // Frame overrun
    static constexpr uint32_t FS_HCINT2_DTERR = 0x400;        // Data toggle error
    static const uint32_t FS_HCINT2_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINTMSK2_XFRCM = 0x1;          // Transfer completed mask
    static constexpr uint32_t FS_HCINTMSK2_CHHM = 0x2;           // Channel halted mask
    static constexpr uint32_t FS_HCINTMSK2_STALLM = 0x8;         // STALL response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK2_NAKM = 0x10;          // NAK response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK2_ACKM = 0x20;          // ACK response received/transmitted interrupt mask
    static constexpr uint32_t FS_HCINTMSK2_NYET = 0x40;          // response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK2_TXERRM = 0x80;        // Transaction error mask
    static constexpr uint32_t FS_HCINTMSK2_BBERRM = 0x100;       // Babble error mask
    static constexpr uint32_t FS_HCINTMSK2_FRMORM = 0x200;       // Frame overrun mask
    static constexpr uint32_t FS_HCINTMSK2_DTERRM = 0x400;       // Data toggle error mask
    static const uint32_t FS_HCINTMSK2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ2_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ2_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ2_DPID =                // Data PID (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    static const uint32_t FS_HCTSIZ2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR3_MPSIZ =               // Maximum packet size (11 bits)
        bit_field_t<0, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR3_EPNUM =               // Endpoint number (4 bits)
        bit_field_t<11, 0xf>::value<X>();
    static constexpr uint32_t FS_HCCHAR3_EPDIR = 0x8000;       // Endpoint direction
    static constexpr uint32_t FS_HCCHAR3_LSDEV = 0x20000;      // Low-speed device
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR3_EPTYP =               // Endpoint type (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR3_MCNT =                // Multicount (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR3_DAD =                 // Device address (7 bits)
        bit_field_t<22, 0x7f>::value<X>();
    static constexpr uint32_t FS_HCCHAR3_ODDFRM = 0x20000000;  // Odd frame
    static constexpr uint32_t FS_HCCHAR3_CHDIS = 0x40000000;   // Channel disable
    static constexpr uint32_t FS_HCCHAR3_CHENA = 0x80000000;   // Channel enable
    static const uint32_t FS_HCCHAR3_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINT3_XFRC = 0x1;           // Transfer completed
    static constexpr uint32_t FS_HCINT3_CHH = 0x2;            // Channel halted
    static constexpr uint32_t FS_HCINT3_STALL = 0x8;          // STALL response received interrupt
    static constexpr uint32_t FS_HCINT3_NAK = 0x10;           // NAK response received interrupt
    static constexpr uint32_t FS_HCINT3_ACK = 0x20;           // ACK response received/transmitted interrupt
    static constexpr uint32_t FS_HCINT3_TXERR = 0x80;         // Transaction error
    static constexpr uint32_t FS_HCINT3_BBERR = 0x100;        // Babble error
    static constexpr uint32_t FS_HCINT3_FRMOR = 0x200;        // Frame overrun
    static constexpr uint32_t FS_HCINT3_DTERR = 0x400;        // Data toggle error
    static const uint32_t FS_HCINT3_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINTMSK3_XFRCM = 0x1;          // Transfer completed mask
    static constexpr uint32_t FS_HCINTMSK3_CHHM = 0x2;           // Channel halted mask
    static constexpr uint32_t FS_HCINTMSK3_STALLM = 0x8;         // STALL response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK3_NAKM = 0x10;          // NAK response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK3_ACKM = 0x20;          // ACK response received/transmitted interrupt mask
    static constexpr uint32_t FS_HCINTMSK3_NYET = 0x40;          // response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK3_TXERRM = 0x80;        // Transaction error mask
    static constexpr uint32_t FS_HCINTMSK3_BBERRM = 0x100;       // Babble error mask
    static constexpr uint32_t FS_HCINTMSK3_FRMORM = 0x200;       // Frame overrun mask
    static constexpr uint32_t FS_HCINTMSK3_DTERRM = 0x400;       // Data toggle error mask
    static const uint32_t FS_HCINTMSK3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ3_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ3_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ3_DPID =                // Data PID (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    static const uint32_t FS_HCTSIZ3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR4_MPSIZ =               // Maximum packet size (11 bits)
        bit_field_t<0, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR4_EPNUM =               // Endpoint number (4 bits)
        bit_field_t<11, 0xf>::value<X>();
    static constexpr uint32_t FS_HCCHAR4_EPDIR = 0x8000;       // Endpoint direction
    static constexpr uint32_t FS_HCCHAR4_LSDEV = 0x20000;      // Low-speed device
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR4_EPTYP =               // Endpoint type (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR4_MCNT =                // Multicount (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR4_DAD =                 // Device address (7 bits)
        bit_field_t<22, 0x7f>::value<X>();
    static constexpr uint32_t FS_HCCHAR4_ODDFRM = 0x20000000;  // Odd frame
    static constexpr uint32_t FS_HCCHAR4_CHDIS = 0x40000000;   // Channel disable
    static constexpr uint32_t FS_HCCHAR4_CHENA = 0x80000000;   // Channel enable
    static const uint32_t FS_HCCHAR4_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINT4_XFRC = 0x1;           // Transfer completed
    static constexpr uint32_t FS_HCINT4_CHH = 0x2;            // Channel halted
    static constexpr uint32_t FS_HCINT4_STALL = 0x8;          // STALL response received interrupt
    static constexpr uint32_t FS_HCINT4_NAK = 0x10;           // NAK response received interrupt
    static constexpr uint32_t FS_HCINT4_ACK = 0x20;           // ACK response received/transmitted interrupt
    static constexpr uint32_t FS_HCINT4_TXERR = 0x80;         // Transaction error
    static constexpr uint32_t FS_HCINT4_BBERR = 0x100;        // Babble error
    static constexpr uint32_t FS_HCINT4_FRMOR = 0x200;        // Frame overrun
    static constexpr uint32_t FS_HCINT4_DTERR = 0x400;        // Data toggle error
    static const uint32_t FS_HCINT4_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINTMSK4_XFRCM = 0x1;          // Transfer completed mask
    static constexpr uint32_t FS_HCINTMSK4_CHHM = 0x2;           // Channel halted mask
    static constexpr uint32_t FS_HCINTMSK4_STALLM = 0x8;         // STALL response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK4_NAKM = 0x10;          // NAK response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK4_ACKM = 0x20;          // ACK response received/transmitted interrupt mask
    static constexpr uint32_t FS_HCINTMSK4_NYET = 0x40;          // response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK4_TXERRM = 0x80;        // Transaction error mask
    static constexpr uint32_t FS_HCINTMSK4_BBERRM = 0x100;       // Babble error mask
    static constexpr uint32_t FS_HCINTMSK4_FRMORM = 0x200;       // Frame overrun mask
    static constexpr uint32_t FS_HCINTMSK4_DTERRM = 0x400;       // Data toggle error mask
    static const uint32_t FS_HCINTMSK4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ4_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ4_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ4_DPID =                // Data PID (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    static const uint32_t FS_HCTSIZ4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR5_MPSIZ =               // Maximum packet size (11 bits)
        bit_field_t<0, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR5_EPNUM =               // Endpoint number (4 bits)
        bit_field_t<11, 0xf>::value<X>();
    static constexpr uint32_t FS_HCCHAR5_EPDIR = 0x8000;       // Endpoint direction
    static constexpr uint32_t FS_HCCHAR5_LSDEV = 0x20000;      // Low-speed device
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR5_EPTYP =               // Endpoint type (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR5_MCNT =                // Multicount (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR5_DAD =                 // Device address (7 bits)
        bit_field_t<22, 0x7f>::value<X>();
    static constexpr uint32_t FS_HCCHAR5_ODDFRM = 0x20000000;  // Odd frame
    static constexpr uint32_t FS_HCCHAR5_CHDIS = 0x40000000;   // Channel disable
    static constexpr uint32_t FS_HCCHAR5_CHENA = 0x80000000;   // Channel enable
    static const uint32_t FS_HCCHAR5_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINT5_XFRC = 0x1;           // Transfer completed
    static constexpr uint32_t FS_HCINT5_CHH = 0x2;            // Channel halted
    static constexpr uint32_t FS_HCINT5_STALL = 0x8;          // STALL response received interrupt
    static constexpr uint32_t FS_HCINT5_NAK = 0x10;           // NAK response received interrupt
    static constexpr uint32_t FS_HCINT5_ACK = 0x20;           // ACK response received/transmitted interrupt
    static constexpr uint32_t FS_HCINT5_TXERR = 0x80;         // Transaction error
    static constexpr uint32_t FS_HCINT5_BBERR = 0x100;        // Babble error
    static constexpr uint32_t FS_HCINT5_FRMOR = 0x200;        // Frame overrun
    static constexpr uint32_t FS_HCINT5_DTERR = 0x400;        // Data toggle error
    static const uint32_t FS_HCINT5_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINTMSK5_XFRCM = 0x1;          // Transfer completed mask
    static constexpr uint32_t FS_HCINTMSK5_CHHM = 0x2;           // Channel halted mask
    static constexpr uint32_t FS_HCINTMSK5_STALLM = 0x8;         // STALL response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK5_NAKM = 0x10;          // NAK response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK5_ACKM = 0x20;          // ACK response received/transmitted interrupt mask
    static constexpr uint32_t FS_HCINTMSK5_NYET = 0x40;          // response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK5_TXERRM = 0x80;        // Transaction error mask
    static constexpr uint32_t FS_HCINTMSK5_BBERRM = 0x100;       // Babble error mask
    static constexpr uint32_t FS_HCINTMSK5_FRMORM = 0x200;       // Frame overrun mask
    static constexpr uint32_t FS_HCINTMSK5_DTERRM = 0x400;       // Data toggle error mask
    static const uint32_t FS_HCINTMSK5_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ5_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ5_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ5_DPID =                // Data PID (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    static const uint32_t FS_HCTSIZ5_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR6_MPSIZ =               // Maximum packet size (11 bits)
        bit_field_t<0, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR6_EPNUM =               // Endpoint number (4 bits)
        bit_field_t<11, 0xf>::value<X>();
    static constexpr uint32_t FS_HCCHAR6_EPDIR = 0x8000;       // Endpoint direction
    static constexpr uint32_t FS_HCCHAR6_LSDEV = 0x20000;      // Low-speed device
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR6_EPTYP =               // Endpoint type (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR6_MCNT =                // Multicount (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR6_DAD =                 // Device address (7 bits)
        bit_field_t<22, 0x7f>::value<X>();
    static constexpr uint32_t FS_HCCHAR6_ODDFRM = 0x20000000;  // Odd frame
    static constexpr uint32_t FS_HCCHAR6_CHDIS = 0x40000000;   // Channel disable
    static constexpr uint32_t FS_HCCHAR6_CHENA = 0x80000000;   // Channel enable
    static const uint32_t FS_HCCHAR6_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINT6_XFRC = 0x1;           // Transfer completed
    static constexpr uint32_t FS_HCINT6_CHH = 0x2;            // Channel halted
    static constexpr uint32_t FS_HCINT6_STALL = 0x8;          // STALL response received interrupt
    static constexpr uint32_t FS_HCINT6_NAK = 0x10;           // NAK response received interrupt
    static constexpr uint32_t FS_HCINT6_ACK = 0x20;           // ACK response received/transmitted interrupt
    static constexpr uint32_t FS_HCINT6_TXERR = 0x80;         // Transaction error
    static constexpr uint32_t FS_HCINT6_BBERR = 0x100;        // Babble error
    static constexpr uint32_t FS_HCINT6_FRMOR = 0x200;        // Frame overrun
    static constexpr uint32_t FS_HCINT6_DTERR = 0x400;        // Data toggle error
    static const uint32_t FS_HCINT6_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINTMSK6_XFRCM = 0x1;          // Transfer completed mask
    static constexpr uint32_t FS_HCINTMSK6_CHHM = 0x2;           // Channel halted mask
    static constexpr uint32_t FS_HCINTMSK6_STALLM = 0x8;         // STALL response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK6_NAKM = 0x10;          // NAK response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK6_ACKM = 0x20;          // ACK response received/transmitted interrupt mask
    static constexpr uint32_t FS_HCINTMSK6_NYET = 0x40;          // response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK6_TXERRM = 0x80;        // Transaction error mask
    static constexpr uint32_t FS_HCINTMSK6_BBERRM = 0x100;       // Babble error mask
    static constexpr uint32_t FS_HCINTMSK6_FRMORM = 0x200;       // Frame overrun mask
    static constexpr uint32_t FS_HCINTMSK6_DTERRM = 0x400;       // Data toggle error mask
    static const uint32_t FS_HCINTMSK6_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ6_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ6_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ6_DPID =                // Data PID (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    static const uint32_t FS_HCTSIZ6_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR7_MPSIZ =               // Maximum packet size (11 bits)
        bit_field_t<0, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR7_EPNUM =               // Endpoint number (4 bits)
        bit_field_t<11, 0xf>::value<X>();
    static constexpr uint32_t FS_HCCHAR7_EPDIR = 0x8000;       // Endpoint direction
    static constexpr uint32_t FS_HCCHAR7_LSDEV = 0x20000;      // Low-speed device
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR7_EPTYP =               // Endpoint type (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR7_MCNT =                // Multicount (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCCHAR7_DAD =                 // Device address (7 bits)
        bit_field_t<22, 0x7f>::value<X>();
    static constexpr uint32_t FS_HCCHAR7_ODDFRM = 0x20000000;  // Odd frame
    static constexpr uint32_t FS_HCCHAR7_CHDIS = 0x40000000;   // Channel disable
    static constexpr uint32_t FS_HCCHAR7_CHENA = 0x80000000;   // Channel enable
    static const uint32_t FS_HCCHAR7_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINT7_XFRC = 0x1;           // Transfer completed
    static constexpr uint32_t FS_HCINT7_CHH = 0x2;            // Channel halted
    static constexpr uint32_t FS_HCINT7_STALL = 0x8;          // STALL response received interrupt
    static constexpr uint32_t FS_HCINT7_NAK = 0x10;           // NAK response received interrupt
    static constexpr uint32_t FS_HCINT7_ACK = 0x20;           // ACK response received/transmitted interrupt
    static constexpr uint32_t FS_HCINT7_TXERR = 0x80;         // Transaction error
    static constexpr uint32_t FS_HCINT7_BBERR = 0x100;        // Babble error
    static constexpr uint32_t FS_HCINT7_FRMOR = 0x200;        // Frame overrun
    static constexpr uint32_t FS_HCINT7_DTERR = 0x400;        // Data toggle error
    static const uint32_t FS_HCINT7_RESET_VALUE = 0x0;

    static constexpr uint32_t FS_HCINTMSK7_XFRCM = 0x1;          // Transfer completed mask
    static constexpr uint32_t FS_HCINTMSK7_CHHM = 0x2;           // Channel halted mask
    static constexpr uint32_t FS_HCINTMSK7_STALLM = 0x8;         // STALL response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK7_NAKM = 0x10;          // NAK response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK7_ACKM = 0x20;          // ACK response received/transmitted interrupt mask
    static constexpr uint32_t FS_HCINTMSK7_NYET = 0x40;          // response received interrupt mask
    static constexpr uint32_t FS_HCINTMSK7_TXERRM = 0x80;        // Transaction error mask
    static constexpr uint32_t FS_HCINTMSK7_BBERRM = 0x100;       // Babble error mask
    static constexpr uint32_t FS_HCINTMSK7_FRMORM = 0x200;       // Frame overrun mask
    static constexpr uint32_t FS_HCINTMSK7_DTERRM = 0x400;       // Data toggle error mask
    static const uint32_t FS_HCINTMSK7_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ7_XFRSIZ =              // Transfer size (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ7_PKTCNT =              // Packet count (10 bits)
        bit_field_t<19, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FS_HCTSIZ7_DPID =                // Data PID (2 bits)
        bit_field_t<29, 0x3>::value<X>();
    static const uint32_t FS_HCTSIZ7_RESET_VALUE = 0x0;
};

static otg_fs_host_t& OTG_FS_HOST = *reinterpret_cast<otg_fs_host_t*>(0x50000400);

#define HAVE_PERIPHERAL_OTG_FS_HOST


////
//
//    USB on the go full speed
//
////

struct otg_fs_pwrclk_t
{
    volatile uint32_t    FS_PCGCCTL;           // [Read-write] OTG_FS power and clock gating control register

    static constexpr uint32_t FS_PCGCCTL_STPPCLK = 0x1;        // Stop PHY clock
    static constexpr uint32_t FS_PCGCCTL_GATEHCLK = 0x2;       // Gate HCLK
    static constexpr uint32_t FS_PCGCCTL_PHYSUSP = 0x10;       // PHY Suspended
    static const uint32_t FS_PCGCCTL_RESET_VALUE = 0x0;
};

static otg_fs_pwrclk_t& OTG_FS_PWRCLK = *reinterpret_cast<otg_fs_pwrclk_t*>(0x50000e00);

#define HAVE_PERIPHERAL_OTG_FS_PWRCLK


////
//
//    Ethernet: MAC management counters
//
////

struct ethernet_mmc_t
{
    volatile uint32_t    MMCCR;                // [Read-write] Ethernet MMC control register (ETH_MMCCR)
    volatile uint32_t    MMCRIR;               // [Read-write] Ethernet MMC receive interrupt register (ETH_MMCRIR)
    volatile uint32_t    MMCTIR;               // [Read-write] Ethernet MMC transmit interrupt register (ETH_MMCTIR)
    volatile uint32_t    MMCRIMR;              // [Read-write] Ethernet MMC receive interrupt mask register (ETH_MMCRIMR)
    volatile uint32_t    MMCTIMR;              // [Read-write] Ethernet MMC transmit interrupt mask register (ETH_MMCTIMR)
    reserved_t<14>       _0;
    volatile uint32_t    MMCTGFSCCR;           // [Read-only] Ethernet MMC transmitted good frames after a single collision counter
    volatile uint32_t    MMCTGFMSCCR;          // [Read-only] Ethernet MMC transmitted good frames after more than a single collision
    reserved_t<5>        _1;
    volatile uint32_t    MMCTGFCR;             // [Read-only] Ethernet MMC transmitted good frames counter register
    reserved_t<10>       _2;
    volatile uint32_t    MMCRFCECR;            // [Read-only] Ethernet MMC received frames with CRC error counter register
    volatile uint32_t    MMCRFAECR;            // [Read-only] Ethernet MMC received frames with alignment error counter register
    reserved_t<10>       _3;
    volatile uint32_t    MMCRGUFCR;            // [Read-only] MMC received good unicast frames counter register

    static constexpr uint32_t MMCCR_CR = 0x1;             // Counter reset
    static constexpr uint32_t MMCCR_CSR = 0x2;            // Counter stop rollover
    static constexpr uint32_t MMCCR_ROR = 0x4;            // Reset on read
    static constexpr uint32_t MMCCR_MCF = 0x80000000;     // MMC counter freeze
    static const uint32_t MMCCR_RESET_VALUE = 0x0;

    static constexpr uint32_t MMCRIR_RFCES = 0x20;         // Received frames CRC error status
    static constexpr uint32_t MMCRIR_RFAES = 0x40;         // Received frames alignment error status
    static constexpr uint32_t MMCRIR_RGUFS = 0x20000;      // Received Good Unicast Frames Status
    static const uint32_t MMCRIR_RESET_VALUE = 0x0;

    static constexpr uint32_t MMCTIR_TGFSCS = 0x4000;      // Transmitted good frames single collision status
    static constexpr uint32_t MMCTIR_TGFMSCS = 0x8000;     // Transmitted good frames more single collision status
    static constexpr uint32_t MMCTIR_TGFS = 0x200000;      // Transmitted good frames status
    static const uint32_t MMCTIR_RESET_VALUE = 0x0;

    static constexpr uint32_t MMCRIMR_RFCEM = 0x20;         // Received frame CRC error mask
    static constexpr uint32_t MMCRIMR_RFAEM = 0x40;         // Received frames alignment error mask
    static constexpr uint32_t MMCRIMR_RGUFM = 0x20000;      // Received good unicast frames mask
    static const uint32_t MMCRIMR_RESET_VALUE = 0x0;

    static constexpr uint32_t MMCTIMR_TGFSCM = 0x4000;      // Transmitted good frames single collision mask
    static constexpr uint32_t MMCTIMR_TGFMSCM = 0x8000;     // Transmitted good frames more single collision mask
    static constexpr uint32_t MMCTIMR_TGFM = 0x200000;      // Transmitted good frames mask
    static const uint32_t MMCTIMR_RESET_VALUE = 0x0;


    static const uint32_t MMCTGFSCCR_RESET_VALUE = 0x0;


    static const uint32_t MMCTGFMSCCR_RESET_VALUE = 0x0;


    static const uint32_t MMCTGFCR_RESET_VALUE = 0x0;


    static const uint32_t MMCRFCECR_RESET_VALUE = 0x0;


    static const uint32_t MMCRFAECR_RESET_VALUE = 0x0;


    static const uint32_t MMCRGUFCR_RESET_VALUE = 0x0;
};

static ethernet_mmc_t& ETHERNET_MMC = *reinterpret_cast<ethernet_mmc_t*>(0x40028100);

#define HAVE_PERIPHERAL_ETHERNET_MMC


////
//
//    Ethernet: media access control
//
////

struct ethernet_mac_t
{
    volatile uint32_t    MACCR;                // [Read-write] Ethernet MAC configuration register (ETH_MACCR)
    volatile uint32_t    MACFFR;               // [Read-write] Ethernet MAC frame filter register (ETH_MACCFFR)
    volatile uint32_t    MACHTHR;              // [Read-write] Ethernet MAC hash table high register
    volatile uint32_t    MACHTLR;              // [Read-write] Ethernet MAC hash table low register
    volatile uint32_t    MACMIIAR;             // [Read-write] Ethernet MAC MII address register (ETH_MACMIIAR)
    volatile uint32_t    MACMIIDR;             // [Read-write] Ethernet MAC MII data register (ETH_MACMIIDR)
    volatile uint32_t    MACFCR;               // [Read-write] Ethernet MAC flow control register (ETH_MACFCR)
    volatile uint32_t    MACVLANTR;            // [Read-write] Ethernet MAC VLAN tag register (ETH_MACVLANTR)
    reserved_t<2>        _0;
    volatile uint32_t    MACRWUFFR;            // [Read-write] Ethernet MAC remote wakeup frame filter register (ETH_MACRWUFFR)
    volatile uint32_t    MACPMTCSR;            // [Read-write] Ethernet MAC PMT control and status register (ETH_MACPMTCSR)
    reserved_t<2>        _1;
    volatile uint32_t    MACSR;                // [Read-write] Ethernet MAC interrupt status register (ETH_MACSR)
    volatile uint32_t    MACIMR;               // [Read-write] Ethernet MAC interrupt mask register (ETH_MACIMR)
    volatile uint32_t    MACA0HR;              // Ethernet MAC address 0 high register (ETH_MACA0HR)
    volatile uint32_t    MACA0LR;              // [Read-write] Ethernet MAC address 0 low register
    volatile uint32_t    MACA1HR;              // [Read-write] Ethernet MAC address 1 high register (ETH_MACA1HR)
    volatile uint32_t    MACA1LR;              // [Read-write] Ethernet MAC address1 low register
    volatile uint32_t    MACA2HR;              // [Read-write] Ethernet MAC address 2 high register (ETH_MACA2HR)
    volatile uint32_t    MACA2LR;              // [Read-write] Ethernet MAC address 2 low register
    volatile uint32_t    MACA3HR;              // [Read-write] Ethernet MAC address 3 high register (ETH_MACA3HR)
    volatile uint32_t    MACA3LR;              // [Read-write] Ethernet MAC address 3 low register

    static constexpr uint32_t MACCR_RE = 0x4;             // Receiver enable
    static constexpr uint32_t MACCR_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t MACCR_DC = 0x10;            // Deferral check
    template<uint32_t X>
    static constexpr uint32_t MACCR_BL =                  // Back-off limit (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t MACCR_APCS = 0x80;          // Automatic pad/CRC stripping
    static constexpr uint32_t MACCR_RD = 0x200;           // Retry disable
    static constexpr uint32_t MACCR_IPCO = 0x400;         // IPv4 checksum offload
    static constexpr uint32_t MACCR_DM = 0x800;           // Duplex mode
    static constexpr uint32_t MACCR_LM = 0x1000;          // Loopback mode
    static constexpr uint32_t MACCR_ROD = 0x2000;         // Receive own disable
    static constexpr uint32_t MACCR_FES = 0x4000;         // Fast Ethernet speed
    static constexpr uint32_t MACCR_CSD = 0x10000;        // Carrier sense disable
    template<uint32_t X>
    static constexpr uint32_t MACCR_IFG =                 // Interframe gap (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    static constexpr uint32_t MACCR_JD = 0x400000;        // Jabber disable
    static constexpr uint32_t MACCR_WD = 0x800000;        // Watchdog disable
    static const uint32_t MACCR_RESET_VALUE = 0x8000;

    static constexpr uint32_t MACFFR_PM = 0x1;             // Promiscuous mode
    static constexpr uint32_t MACFFR_HU = 0x2;             // Hash unicast
    static constexpr uint32_t MACFFR_HM = 0x4;             // Hash multicast
    static constexpr uint32_t MACFFR_DAIF = 0x8;           // Destination address inverse filtering
    static constexpr uint32_t MACFFR_PAM = 0x10;           // Pass all multicast
    static constexpr uint32_t MACFFR_BFD = 0x20;           // Broadcast frames disable
    template<uint32_t X>
    static constexpr uint32_t MACFFR_PCF =                 // Pass control frames (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    static constexpr uint32_t MACFFR_SAIF = 0x100;         // Source address inverse filtering
    static constexpr uint32_t MACFFR_SAF = 0x200;          // Source address filter
    static constexpr uint32_t MACFFR_HPF = 0x400;          // Hash or perfect filter
    static constexpr uint32_t MACFFR_RA = 0x80000000;      // Receive all
    static const uint32_t MACFFR_RESET_VALUE = 0x0;


    static const uint32_t MACHTHR_RESET_VALUE = 0x0;


    static const uint32_t MACHTLR_RESET_VALUE = 0x0;

    static constexpr uint32_t MACMIIAR_MB = 0x1;             // MII busy
    static constexpr uint32_t MACMIIAR_MW = 0x2;             // MII write
    template<uint32_t X>
    static constexpr uint32_t MACMIIAR_CR =                  // Clock range (3 bits)
        bit_field_t<2, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MACMIIAR_MR =                  // MII register (5 bits)
        bit_field_t<6, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MACMIIAR_PA =                  // PHY address (5 bits)
        bit_field_t<11, 0x1f>::value<X>();
    static const uint32_t MACMIIAR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t MACMIIDR_MD =                  // MII data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t MACMIIDR_RESET_VALUE = 0x0;

    static constexpr uint32_t MACFCR_FCB_BPA = 0x1;        // Flow control busy/back pressure activate
    static constexpr uint32_t MACFCR_TFCE = 0x2;           // Transmit flow control enable
    static constexpr uint32_t MACFCR_RFCE = 0x4;           // Receive flow control enable
    static constexpr uint32_t MACFCR_UPFD = 0x8;           // Unicast pause frame detect
    template<uint32_t X>
    static constexpr uint32_t MACFCR_PLT =                 // Pause low threshold (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t MACFCR_ZQPD = 0x80;          // Zero-quanta pause disable
    template<uint32_t X>
    static constexpr uint32_t MACFCR_PT =                  // Pass control frames (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t MACFCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t MACVLANTR_VLANTI =              // VLAN tag identifier (for receive frames) (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static constexpr uint32_t MACVLANTR_VLANTC = 0x10000;     // 12-bit VLAN tag comparison
    static const uint32_t MACVLANTR_RESET_VALUE = 0x0;

    static const uint32_t MACRWUFFR_RESET_VALUE = 0x0;

    static constexpr uint32_t MACPMTCSR_PD = 0x1;             // Power down
    static constexpr uint32_t MACPMTCSR_MPE = 0x2;            // Magic Packet enable
    static constexpr uint32_t MACPMTCSR_WFE = 0x4;            // Wakeup frame enable
    static constexpr uint32_t MACPMTCSR_MPR = 0x20;           // Magic packet received
    static constexpr uint32_t MACPMTCSR_WFR = 0x40;           // Wakeup frame received
    static constexpr uint32_t MACPMTCSR_GU = 0x200;           // Global unicast
    static constexpr uint32_t MACPMTCSR_WFFRPR = 0x80000000;  // Wakeup frame filter register pointer reset
    static const uint32_t MACPMTCSR_RESET_VALUE = 0x0;

    static constexpr uint32_t MACSR_PMTS = 0x8;           // PMT status
    static constexpr uint32_t MACSR_MMCS = 0x10;          // MMC status
    static constexpr uint32_t MACSR_MMCRS = 0x20;         // MMC receive status
    static constexpr uint32_t MACSR_MMCTS = 0x40;         // MMC transmit status
    static constexpr uint32_t MACSR_TSTS = 0x200;         // Time stamp trigger status
    static const uint32_t MACSR_RESET_VALUE = 0x0;

    static constexpr uint32_t MACIMR_PMTIM = 0x8;          // PMT interrupt mask
    static constexpr uint32_t MACIMR_TSTIM = 0x200;        // Time stamp trigger interrupt mask
    static const uint32_t MACIMR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t MACA0HR_MACA0H =              // MAC address0 high (16 bits), Read-write
        bit_field_t<0, 0xffff>::value<X>();
    static constexpr uint32_t MACA0HR_MO = 0x80000000;      // Always 1, Read-only
    static const uint32_t MACA0HR_RESET_VALUE = 0x10ffff;


    static const uint32_t MACA0LR_RESET_VALUE = 0xffffffff;

    template<uint32_t X>
    static constexpr uint32_t MACA1HR_MACA1H =              // MAC address1 high (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MACA1HR_MBC =                 // Mask byte control (6 bits)
        bit_field_t<24, 0x3f>::value<X>();
    static constexpr uint32_t MACA1HR_SA = 0x40000000;      // Source address
    static constexpr uint32_t MACA1HR_AE = 0x80000000;      // Address enable
    static const uint32_t MACA1HR_RESET_VALUE = 0xffff;


    static const uint32_t MACA1LR_RESET_VALUE = 0xffffffff;

    template<uint32_t X>
    static constexpr uint32_t MACA2HR_ETH_MACA2HR =         // Ethernet MAC address 2 high register (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MACA2HR_MBC =                 // Mask byte control (6 bits)
        bit_field_t<24, 0x3f>::value<X>();
    static constexpr uint32_t MACA2HR_SA = 0x40000000;      // Source address
    static constexpr uint32_t MACA2HR_AE = 0x80000000;      // Address enable
    static const uint32_t MACA2HR_RESET_VALUE = 0x50;

    template<uint32_t X>
    static constexpr uint32_t MACA2LR_MACA2L =              // MAC address2 low (31 bits)
        bit_field_t<0, 0x7fffffff>::value<X>();
    static const uint32_t MACA2LR_RESET_VALUE = 0xffffffff;

    template<uint32_t X>
    static constexpr uint32_t MACA3HR_MACA3H =              // MAC address3 high (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MACA3HR_MBC =                 // Mask byte control (6 bits)
        bit_field_t<24, 0x3f>::value<X>();
    static constexpr uint32_t MACA3HR_SA = 0x40000000;      // Source address
    static constexpr uint32_t MACA3HR_AE = 0x80000000;      // Address enable
    static const uint32_t MACA3HR_RESET_VALUE = 0xffff;


    static const uint32_t MACA3LR_RESET_VALUE = 0xffffffff;
};

static ethernet_mac_t& ETHERNET_MAC = *reinterpret_cast<ethernet_mac_t*>(0x40028000);

#define HAVE_PERIPHERAL_ETHERNET_MAC


////
//
//    Ethernet: Precision time protocol
//
////

struct ethernet_ptp_t
{
    volatile uint32_t    PTPTSCR;              // [Read-write] Ethernet PTP time stamp control register (ETH_PTPTSCR)
    volatile uint32_t    PTPSSIR;              // [Read-write] Ethernet PTP subsecond increment register
    volatile uint32_t    PTPTSHR;              // [Read-only] Ethernet PTP time stamp high register
    volatile uint32_t    PTPTSLR;              // [Read-only] Ethernet PTP time stamp low register (ETH_PTPTSLR)
    volatile uint32_t    PTPTSHUR;             // [Read-write] Ethernet PTP time stamp high update register
    volatile uint32_t    PTPTSLUR;             // [Read-write] Ethernet PTP time stamp low update register (ETH_PTPTSLUR)
    volatile uint32_t    PTPTSAR;              // [Read-write] Ethernet PTP time stamp addend register
    volatile uint32_t    PTPTTHR;              // [Read-write] Ethernet PTP target time high register
    volatile uint32_t    PTPTTLR;              // [Read-write] Ethernet PTP target time low register

    static constexpr uint32_t PTPTSCR_TSE = 0x1;            // Time stamp enable
    static constexpr uint32_t PTPTSCR_TSFCU = 0x2;          // Time stamp fine or coarse update
    static constexpr uint32_t PTPTSCR_TSSTI = 0x4;          // Time stamp system time initialize
    static constexpr uint32_t PTPTSCR_TSSTU = 0x8;          // Time stamp system time update
    static constexpr uint32_t PTPTSCR_TSITE = 0x10;         // Time stamp interrupt trigger enable
    static constexpr uint32_t PTPTSCR_TSARU = 0x20;         // Time stamp addend register update
    static const uint32_t PTPTSCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PTPSSIR_STSSI =               // System time subsecond increment (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PTPSSIR_RESET_VALUE = 0x0;


    static const uint32_t PTPTSHR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PTPTSLR_STSS =                // System time subseconds (31 bits)
        bit_field_t<0, 0x7fffffff>::value<X>();
    static constexpr uint32_t PTPTSLR_STPNS = 0x80000000;   // System time positive or negative sign
    static const uint32_t PTPTSLR_RESET_VALUE = 0x0;


    static const uint32_t PTPTSHUR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PTPTSLUR_TSUSS =               // Time stamp update subseconds (31 bits)
        bit_field_t<0, 0x7fffffff>::value<X>();
    static constexpr uint32_t PTPTSLUR_TSUPNS = 0x80000000;  // Time stamp update positive or negative sign
    static const uint32_t PTPTSLUR_RESET_VALUE = 0x0;


    static const uint32_t PTPTSAR_RESET_VALUE = 0x0;


    static const uint32_t PTPTTHR_RESET_VALUE = 0x0;


    static const uint32_t PTPTTLR_RESET_VALUE = 0x0;
};

static ethernet_ptp_t& ETHERNET_PTP = *reinterpret_cast<ethernet_ptp_t*>(0x40028700);

#define HAVE_PERIPHERAL_ETHERNET_PTP


////
//
//    Ethernet: DMA controller operation
//
////

struct ethernet_dma_t
{
    volatile uint32_t    DMABMR;               // [Read-write] Ethernet DMA bus mode register
    volatile uint32_t    DMATPDR;              // [Read-write] Ethernet DMA transmit poll demand register
    volatile uint32_t    DMARPDR;              // [Read-write] EHERNET DMA receive poll demand register
    volatile uint32_t    DMARDLAR;             // [Read-write] Ethernet DMA receive descriptor list address register
    volatile uint32_t    DMATDLAR;             // [Read-write] Ethernet DMA transmit descriptor list address register
    volatile uint32_t    DMASR;                // Ethernet DMA status register
    volatile uint32_t    DMAOMR;               // [Read-write] Ethernet DMA operation mode register
    volatile uint32_t    DMAIER;               // [Read-write] Ethernet DMA interrupt enable register
    volatile uint32_t    DMAMFBOCR;            // [Read-only] Ethernet DMA missed frame and buffer overflow counter register
    reserved_t<9>        _0;
    volatile uint32_t    DMACHTDR;             // [Read-only] Ethernet DMA current host transmit descriptor register
    volatile uint32_t    DMACHRDR;             // [Read-only] Ethernet DMA current host receive descriptor register
    volatile uint32_t    DMACHTBAR;            // [Read-only] Ethernet DMA current host transmit buffer address register
    volatile uint32_t    DMACHRBAR;            // [Read-only] Ethernet DMA current host receive buffer address register

    static constexpr uint32_t DMABMR_SR = 0x1;             // Software reset
    static constexpr uint32_t DMABMR_DA = 0x2;             // DMA Arbitration
    template<uint32_t X>
    static constexpr uint32_t DMABMR_DSL =                 // Descriptor skip length (5 bits)
        bit_field_t<2, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DMABMR_PBL =                 // Programmable burst length (6 bits)
        bit_field_t<8, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DMABMR_RTPR =                // Rx Tx priority ratio (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    static constexpr uint32_t DMABMR_FB = 0x10000;         // Fixed burst
    template<uint32_t X>
    static constexpr uint32_t DMABMR_RDP =                 // Rx DMA PBL (6 bits)
        bit_field_t<17, 0x3f>::value<X>();
    static constexpr uint32_t DMABMR_USP = 0x800000;       // Use separate PBL
    static constexpr uint32_t DMABMR_FPM = 0x1000000;      // 4xPBL mode
    static constexpr uint32_t DMABMR_AAB = 0x2000000;      // Address-aligned beats
    static const uint32_t DMABMR_RESET_VALUE = 0x20101;


    static const uint32_t DMATPDR_RESET_VALUE = 0x0;


    static const uint32_t DMARPDR_RESET_VALUE = 0x0;


    static const uint32_t DMARDLAR_RESET_VALUE = 0x0;


    static const uint32_t DMATDLAR_RESET_VALUE = 0x0;

    static constexpr uint32_t DMASR_TS = 0x1;             // Transmit status, Read-write
    static constexpr uint32_t DMASR_TPSS = 0x2;           // Transmit process stopped status, Read-write
    static constexpr uint32_t DMASR_TBUS = 0x4;           // Transmit buffer unavailable status, Read-write
    static constexpr uint32_t DMASR_TJTS = 0x8;           // Transmit jabber timeout status, Read-write
    static constexpr uint32_t DMASR_ROS = 0x10;           // Receive overflow status, Read-write
    static constexpr uint32_t DMASR_TUS = 0x20;           // Transmit underflow status, Read-write
    static constexpr uint32_t DMASR_RS = 0x40;            // Receive status, Read-write
    static constexpr uint32_t DMASR_RBUS = 0x80;          // Receive buffer unavailable status, Read-write
    static constexpr uint32_t DMASR_RPSS = 0x100;         // Receive process stopped status, Read-write
    static constexpr uint32_t DMASR_PWTS = 0x200;         // Receive watchdog timeout status, Read-write
    static constexpr uint32_t DMASR_ETS = 0x400;          // Early transmit status, Read-write
    static constexpr uint32_t DMASR_FBES = 0x2000;        // Fatal bus error status, Read-write
    static constexpr uint32_t DMASR_ERS = 0x4000;         // Early receive status, Read-write
    static constexpr uint32_t DMASR_AIS = 0x8000;         // Abnormal interrupt summary, Read-write
    static constexpr uint32_t DMASR_NIS = 0x10000;        // Normal interrupt summary, Read-write
    template<uint32_t X>
    static constexpr uint32_t DMASR_RPS =                 // Receive process state (3 bits), Read-only
        bit_field_t<17, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DMASR_TPS =                 // Transmit process state (3 bits), Read-only
        bit_field_t<20, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DMASR_EBS =                 // Error bits status (3 bits), Read-only
        bit_field_t<23, 0x7>::value<X>();
    static constexpr uint32_t DMASR_MMCS = 0x8000000;     // MMC status, Read-only
    static constexpr uint32_t DMASR_PMTS = 0x10000000;    // PMT status, Read-only
    static constexpr uint32_t DMASR_TSTS = 0x20000000;    // Time stamp trigger status, Read-only
    static const uint32_t DMASR_RESET_VALUE = 0x0;

    static constexpr uint32_t DMAOMR_SR = 0x2;             // SR
    static constexpr uint32_t DMAOMR_OSF = 0x4;            // OSF
    template<uint32_t X>
    static constexpr uint32_t DMAOMR_RTC =                 // RTC (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t DMAOMR_FUGF = 0x40;          // FUGF
    static constexpr uint32_t DMAOMR_FEF = 0x80;           // FEF
    static constexpr uint32_t DMAOMR_ST = 0x2000;          // ST
    template<uint32_t X>
    static constexpr uint32_t DMAOMR_TTC =                 // TTC (3 bits)
        bit_field_t<14, 0x7>::value<X>();
    static constexpr uint32_t DMAOMR_FTF = 0x100000;       // FTF
    static constexpr uint32_t DMAOMR_TSF = 0x200000;       // TSF
    static constexpr uint32_t DMAOMR_DFRF = 0x1000000;     // DFRF
    static constexpr uint32_t DMAOMR_RSF = 0x2000000;      // RSF
    static constexpr uint32_t DMAOMR_DTCEFD = 0x4000000;   // DTCEFD
    static const uint32_t DMAOMR_RESET_VALUE = 0x0;

    static constexpr uint32_t DMAIER_TIE = 0x1;            // Transmit interrupt enable
    static constexpr uint32_t DMAIER_TPSIE = 0x2;          // Transmit process stopped interrupt enable
    static constexpr uint32_t DMAIER_TBUIE = 0x4;          // Transmit buffer unavailable interrupt enable
    static constexpr uint32_t DMAIER_TJTIE = 0x8;          // Transmit jabber timeout interrupt enable
    static constexpr uint32_t DMAIER_ROIE = 0x10;          // Overflow interrupt enable
    static constexpr uint32_t DMAIER_TUIE = 0x20;          // Underflow interrupt enable
    static constexpr uint32_t DMAIER_RIE = 0x40;           // Receive interrupt enable
    static constexpr uint32_t DMAIER_RBUIE = 0x80;         // Receive buffer unavailable interrupt enable
    static constexpr uint32_t DMAIER_RPSIE = 0x100;        // Receive process stopped interrupt enable
    static constexpr uint32_t DMAIER_RWTIE = 0x200;        // receive watchdog timeout interrupt enable
    static constexpr uint32_t DMAIER_ETIE = 0x400;         // Early transmit interrupt enable
    static constexpr uint32_t DMAIER_FBEIE = 0x2000;       // Fatal bus error interrupt enable
    static constexpr uint32_t DMAIER_ERIE = 0x4000;        // Early receive interrupt enable
    static constexpr uint32_t DMAIER_AISE = 0x8000;        // Abnormal interrupt summary enable
    static constexpr uint32_t DMAIER_NISE = 0x10000;       // Normal interrupt summary enable
    static const uint32_t DMAIER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DMAMFBOCR_MFC =                 // Missed frames by the controller (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static constexpr uint32_t DMAMFBOCR_OMFC = 0x10000;       // Overflow bit for missed frame counter
    template<uint32_t X>
    static constexpr uint32_t DMAMFBOCR_MFA =                 // Missed frames by the application (11 bits)
        bit_field_t<17, 0x7ff>::value<X>();
    static constexpr uint32_t DMAMFBOCR_OFOC = 0x10000000;    // Overflow bit for FIFO overflow counter
    static const uint32_t DMAMFBOCR_RESET_VALUE = 0x0;


    static const uint32_t DMACHTDR_RESET_VALUE = 0x0;


    static const uint32_t DMACHRDR_RESET_VALUE = 0x0;


    static const uint32_t DMACHTBAR_RESET_VALUE = 0x0;


    static const uint32_t DMACHRBAR_RESET_VALUE = 0x0;
};

static ethernet_dma_t& ETHERNET_DMA = *reinterpret_cast<ethernet_dma_t*>(0x40029000);

#define HAVE_PERIPHERAL_ETHERNET_DMA


////
//
//    Nested Vectored Interrupt Controller
//
////

struct nvic_t
{
    volatile uint32_t    ISER0;                // [Read-write] Interrupt Set-Enable Register
    volatile uint32_t    ISER1;                // [Read-write] Interrupt Set-Enable Register
    reserved_t<30>       _0;
    volatile uint32_t    ICER0;                // [Read-write] Interrupt Clear-Enable Register
    volatile uint32_t    ICER1;                // [Read-write] Interrupt Clear-Enable Register
    reserved_t<30>       _1;
    volatile uint32_t    ISPR0;                // [Read-write] Interrupt Set-Pending Register
    volatile uint32_t    ISPR1;                // [Read-write] Interrupt Set-Pending Register
    reserved_t<30>       _2;
    volatile uint32_t    ICPR0;                // [Read-write] Interrupt Clear-Pending Register
    volatile uint32_t    ICPR1;                // [Read-write] Interrupt Clear-Pending Register
    reserved_t<30>       _3;
    volatile uint32_t    IABR0;                // [Read-only] Interrupt Active Bit Register
    volatile uint32_t    IABR1;                // [Read-only] Interrupt Active Bit Register
    reserved_t<62>       _4;
    volatile uint32_t    IPR0;                 // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR1;                 // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR2;                 // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR3;                 // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR4;                 // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR5;                 // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR6;                 // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR7;                 // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR8;                 // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR9;                 // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR10;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR11;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR12;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR13;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR14;                // [Read-write] Interrupt Priority Register


    static const uint32_t ISER0_RESET_VALUE = 0x0;


    static const uint32_t ISER1_RESET_VALUE = 0x0;


    static const uint32_t ICER0_RESET_VALUE = 0x0;


    static const uint32_t ICER1_RESET_VALUE = 0x0;


    static const uint32_t ISPR0_RESET_VALUE = 0x0;


    static const uint32_t ISPR1_RESET_VALUE = 0x0;


    static const uint32_t ICPR0_RESET_VALUE = 0x0;


    static const uint32_t ICPR1_RESET_VALUE = 0x0;


    static const uint32_t IABR0_RESET_VALUE = 0x0;


    static const uint32_t IABR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR0_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR0_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR0_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR0_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR0_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR1_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR1_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR1_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR1_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR2_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR2_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR2_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR2_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR3_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR3_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR3_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR3_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR4_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR4_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR4_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR4_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR5_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR5_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR5_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR5_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR5_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR6_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR6_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR6_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR6_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR6_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR7_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR7_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR7_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR7_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR7_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR8_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR8_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR8_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR8_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR8_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR9_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR9_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR9_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR9_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR9_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR10_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR10_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR10_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR10_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR10_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR11_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR11_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR11_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR11_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR11_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR12_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR12_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR12_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR12_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR12_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR13_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR13_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR13_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR13_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR13_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR14_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR14_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR14_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR14_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR14_RESET_VALUE = 0x0;
};

static nvic_t& NVIC = *reinterpret_cast<nvic_t*>(0xe000e100);

#define HAVE_PERIPHERAL_NVIC


////
//
//    Memory protection unit
//
////

struct mpu_t
{
    volatile uint32_t    MPU_TYPER;            // [Read-only] MPU type register
    volatile uint32_t    MPU_CTRL;             // [Read-only] MPU control register
    volatile uint32_t    MPU_RNR;              // [Read-write] MPU region number register
    volatile uint32_t    MPU_RBAR;             // [Read-write] MPU region base address register
    volatile uint32_t    MPU_RASR;             // [Read-write] MPU region attribute and size register

    static constexpr uint32_t MPU_TYPER_SEPARATE = 0x1;       // Separate flag
    template<uint32_t X>
    static constexpr uint32_t MPU_TYPER_DREGION =             // Number of MPU data regions (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MPU_TYPER_IREGION =             // Number of MPU instruction regions (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static const uint32_t MPU_TYPER_RESET_VALUE = 0x800;

    static constexpr uint32_t MPU_CTRL_ENABLE = 0x1;         // Enables the MPU
    static constexpr uint32_t MPU_CTRL_HFNMIENA = 0x2;       // Enables the operation of MPU during hard fault
    static constexpr uint32_t MPU_CTRL_PRIVDEFENA = 0x4;     // Enable priviliged software access to default memory map
    static const uint32_t MPU_CTRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t MPU_RNR_REGION =              // MPU region (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t MPU_RNR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t MPU_RBAR_REGION =              // MPU region field (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static constexpr uint32_t MPU_RBAR_VALID = 0x10;         // MPU region number valid
    template<uint32_t X>
    static constexpr uint32_t MPU_RBAR_ADDR =                // Region base address field (27 bits)
        bit_field_t<5, 0x7ffffff>::value<X>();
    static const uint32_t MPU_RBAR_RESET_VALUE = 0x0;

    static constexpr uint32_t MPU_RASR_ENABLE = 0x1;         // Region enable bit.
    template<uint32_t X>
    static constexpr uint32_t MPU_RASR_SIZE =                // Size of the MPU protection region (5 bits)
        bit_field_t<1, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MPU_RASR_SRD =                 // Subregion disable bits (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static constexpr uint32_t MPU_RASR_B = 0x10000;          // memory attribute
    static constexpr uint32_t MPU_RASR_C = 0x20000;          // memory attribute
    static constexpr uint32_t MPU_RASR_S = 0x40000;          // Shareable memory attribute
    template<uint32_t X>
    static constexpr uint32_t MPU_RASR_TEX =                 // memory attribute (3 bits)
        bit_field_t<19, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MPU_RASR_AP =                  // Access permission (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    static constexpr uint32_t MPU_RASR_XN = 0x10000000;      // Instruction access disable bit
    static const uint32_t MPU_RASR_RESET_VALUE = 0x0;
};

static mpu_t& MPU = *reinterpret_cast<mpu_t*>(0xe000ed90);

#define HAVE_PERIPHERAL_MPU


////
//
//    System control block ACTLR
//
////

struct scb_actrl_t
{
    volatile uint32_t    ACTRL;                // [Read-write] Auxiliary control register

    static constexpr uint32_t ACTRL_DISFOLD = 0x4;        // DISFOLD
    static constexpr uint32_t ACTRL_FPEXCODIS = 0x400;    // FPEXCODIS
    static constexpr uint32_t ACTRL_DISRAMODE = 0x800;    // DISRAMODE
    static constexpr uint32_t ACTRL_DISITMATBFLUSH = 0x1000;// DISITMATBFLUSH
    static const uint32_t ACTRL_RESET_VALUE = 0x0;
};

static scb_actrl_t& SCB_ACTRL = *reinterpret_cast<scb_actrl_t*>(0xe000e008);

#define HAVE_PERIPHERAL_SCB_ACTRL


////
//
//    Nested vectored interrupt controller
//
////

struct nvic_stir_t
{
    volatile uint32_t    STIR;                 // [Read-write] Software trigger interrupt register

    template<uint32_t X>
    static constexpr uint32_t STIR_INTID =               // Software generated interrupt ID (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t STIR_RESET_VALUE = 0x0;
};

static nvic_stir_t& NVIC_STIR = *reinterpret_cast<nvic_stir_t*>(0xe000ef00);

#define HAVE_PERIPHERAL_NVIC_STIR


////
//
//    System control block
//
////

struct scb_t
{
    volatile uint32_t    CPUID;                // [Read-only] CPUID base register
    volatile uint32_t    ICSR;                 // [Read-write] Interrupt control and state register
    volatile uint32_t    VTOR;                 // [Read-write] Vector table offset register
    volatile uint32_t    AIRCR;                // [Read-write] Application interrupt and reset control register
    volatile uint32_t    SCR;                  // [Read-write] System control register
    volatile uint32_t    CCR;                  // [Read-write] Configuration and control register
    volatile uint32_t    SHPR1;                // [Read-write] System handler priority registers
    volatile uint32_t    SHPR2;                // [Read-write] System handler priority registers
    volatile uint32_t    SHPR3;                // [Read-write] System handler priority registers
    volatile uint32_t    SHCRS;                // [Read-write] System handler control and state register
    volatile uint32_t    CFSR_UFSR_BFSR_MMFSR; // [Read-write] Configurable fault status register
    volatile uint32_t    HFSR;                 // [Read-write] Hard fault status register
    reserved_t<1>        _0;
    volatile uint32_t    MMFAR;                // [Read-write] Memory management fault address register
    volatile uint32_t    BFAR;                 // [Read-write] Bus fault address register

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
    static constexpr uint32_t ICSR_VECTACTIVE =          // Active vector (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static constexpr uint32_t ICSR_RETTOBASE = 0x800;    // Return to base level
    template<uint32_t X>
    static constexpr uint32_t ICSR_VECTPENDING =         // Pending vector (7 bits)
        bit_field_t<12, 0x7f>::value<X>();
    static constexpr uint32_t ICSR_ISRPENDING = 0x400000;// Interrupt pending flag
    static constexpr uint32_t ICSR_PENDSTCLR = 0x2000000;// SysTick exception clear-pending bit
    static constexpr uint32_t ICSR_PENDSTSET = 0x4000000;// SysTick exception set-pending bit
    static constexpr uint32_t ICSR_PENDSVCLR = 0x8000000;// PendSV clear-pending bit
    static constexpr uint32_t ICSR_PENDSVSET = 0x10000000;// PendSV set-pending bit
    static constexpr uint32_t ICSR_NMIPENDSET = 0x80000000;// NMI set-pending bit.
    static const uint32_t ICSR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t VTOR_TBLOFF =              // Vector table base offset field (21 bits)
        bit_field_t<9, 0x1fffff>::value<X>();
    static const uint32_t VTOR_RESET_VALUE = 0x0;

    static constexpr uint32_t AIRCR_VECTRESET = 0x1;      // VECTRESET
    static constexpr uint32_t AIRCR_VECTCLRACTIVE = 0x2;  // VECTCLRACTIVE
    static constexpr uint32_t AIRCR_SYSRESETREQ = 0x4;    // SYSRESETREQ
    template<uint32_t X>
    static constexpr uint32_t AIRCR_PRIGROUP =            // PRIGROUP (3 bits)
        bit_field_t<8, 0x7>::value<X>();
    static constexpr uint32_t AIRCR_ENDIANESS = 0x8000;   // ENDIANESS
    template<uint32_t X>
    static constexpr uint32_t AIRCR_VECTKEYSTAT =         // Register key (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t AIRCR_RESET_VALUE = 0x0;

    static constexpr uint32_t SCR_SLEEPONEXIT = 0x2;    // SLEEPONEXIT
    static constexpr uint32_t SCR_SLEEPDEEP = 0x4;      // SLEEPDEEP
    static constexpr uint32_t SCR_SEVEONPEND = 0x10;    // Send Event on Pending bit
    static const uint32_t SCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR_NONBASETHRDENA = 0x1; // Configures how the processor enters Thread mode
    static constexpr uint32_t CCR_USERSETMPEND = 0x2;   // USERSETMPEND
    static constexpr uint32_t CCR_UNALIGN__TRP = 0x8;   // UNALIGN_ TRP
    static constexpr uint32_t CCR_DIV_0_TRP = 0x10;     // DIV_0_TRP
    static constexpr uint32_t CCR_BFHFNMIGN = 0x100;    // BFHFNMIGN
    static constexpr uint32_t CCR_STKALIGN = 0x200;     // STKALIGN
    static const uint32_t CCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SHPR1_PRI_4 =               // Priority of system handler 4 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SHPR1_PRI_5 =               // Priority of system handler 5 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SHPR1_PRI_6 =               // Priority of system handler 6 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static const uint32_t SHPR1_RESET_VALUE = 0x0;

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

    static constexpr uint32_t SHCRS_MEMFAULTACT = 0x1;    // Memory management fault exception active bit
    static constexpr uint32_t SHCRS_BUSFAULTACT = 0x2;    // Bus fault exception active bit
    static constexpr uint32_t SHCRS_USGFAULTACT = 0x8;    // Usage fault exception active bit
    static constexpr uint32_t SHCRS_SVCALLACT = 0x80;     // SVC call active bit
    static constexpr uint32_t SHCRS_MONITORACT = 0x100;   // Debug monitor active bit
    static constexpr uint32_t SHCRS_PENDSVACT = 0x400;    // PendSV exception active bit
    static constexpr uint32_t SHCRS_SYSTICKACT = 0x800;   // SysTick exception active bit
    static constexpr uint32_t SHCRS_USGFAULTPENDED = 0x1000;// Usage fault exception pending bit
    static constexpr uint32_t SHCRS_MEMFAULTPENDED = 0x2000;// Memory management fault exception pending bit
    static constexpr uint32_t SHCRS_BUSFAULTPENDED = 0x4000;// Bus fault exception pending bit
    static constexpr uint32_t SHCRS_SVCALLPENDED = 0x8000;// SVC call pending bit
    static constexpr uint32_t SHCRS_MEMFAULTENA = 0x10000;// Memory management fault enable bit
    static constexpr uint32_t SHCRS_BUSFAULTENA = 0x20000;// Bus fault enable bit
    static constexpr uint32_t SHCRS_USGFAULTENA = 0x40000;// Usage fault enable bit
    static const uint32_t SHCRS_RESET_VALUE = 0x0;

    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_IACCVIOL = 0x1;       // IACCVIOL
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_DACCVIOL = 0x2;       // DACCVIOL
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_MUNSTKERR = 0x8;      // MUNSTKERR
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_MSTKERR = 0x10;       // MSTKERR
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_MLSPERR = 0x20;       // MLSPERR
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_MMARVALID = 0x80;     // MMARVALID
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_IBUSERR = 0x100;      // Instruction bus error
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_PRECISERR = 0x200;    // Precise data bus error
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_IMPRECISERR = 0x400;  // Imprecise data bus error
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_UNSTKERR = 0x800;     // Bus fault on unstacking for a return from exception
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_STKERR = 0x1000;      // Bus fault on stacking for exception entry
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_LSPERR = 0x2000;      // Bus fault on floating-point lazy state preservation
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_BFARVALID = 0x8000;   // Bus Fault Address Register (BFAR) valid flag
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_UNDEFINSTR = 0x10000; // Undefined instruction usage fault
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_INVSTATE = 0x20000;   // Invalid state usage fault
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_INVPC = 0x40000;      // Invalid PC load usage fault
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_NOCP = 0x80000;       // No coprocessor usage fault.
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_UNALIGNED = 0x1000000;// Unaligned access usage fault
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_DIVBYZERO = 0x2000000;// Divide by zero usage fault
    static const uint32_t CFSR_UFSR_BFSR_MMFSR_RESET_VALUE = 0x0;

    static constexpr uint32_t HFSR_VECTTBL = 0x2;        // Vector table hard fault
    static constexpr uint32_t HFSR_FORCED = 0x40000000;  // Forced hard fault
    static constexpr uint32_t HFSR_DEBUG_VT = 0x80000000;// Reserved for Debug use
    static const uint32_t HFSR_RESET_VALUE = 0x0;


    static const uint32_t MMFAR_RESET_VALUE = 0x0;


    static const uint32_t BFAR_RESET_VALUE = 0x0;
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
    volatile uint32_t    CTRL;                 // [Read-write] SysTick control and status register
    volatile uint32_t    LOAD;                 // [Read-write] SysTick reload value register
    volatile uint32_t    VAL;                  // [Read-write] SysTick current value register
    volatile uint32_t    CALIB;                // [Read-write] SysTick calibration value register

    static constexpr uint32_t CTRL_ENABLE = 0x1;         // Counter enable
    static constexpr uint32_t CTRL_TICKINT = 0x2;        // SysTick exception request enable
    static constexpr uint32_t CTRL_CLKSOURCE = 0x4;      // Clock source selection
    static constexpr uint32_t CTRL_COUNTFLAG = 0x10000;  // COUNTFLAG
    static const uint32_t CTRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t LOAD_RELOAD =              // RELOAD value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();
    static const uint32_t LOAD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t VAL_CURRENT =             // Current counter value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();
    static const uint32_t VAL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CALIB_TENMS =               // Calibration value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();
    static const uint32_t CALIB_RESET_VALUE = 0x0;
};

static stk_t& STK = *reinterpret_cast<stk_t*>(0xe000e010);

#define HAVE_PERIPHERAL_STK


template<typename PERIPHERAL> struct peripheral_traits {};

template<> struct peripheral_traits<fsmc_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_FSMCEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_FSMCEN; }
};

template<> struct peripheral_traits<pwr_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_PWREN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_PWREN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_PWRRST; }
};

template<> struct peripheral_traits<gpioa_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_IOPAEN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_IOPAEN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_IOPARST; }
};

template<> struct peripheral_traits<gpiob_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_IOPBEN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_IOPBEN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_IOPBRST; }
};

template<> struct peripheral_traits<gpioc_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_IOPCEN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_IOPCEN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_IOPCRST; }
};

template<> struct peripheral_traits<gpiod_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_IOPDEN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_IOPDEN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_IOPDRST; }
};

template<> struct peripheral_traits<gpioe_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_IOPEEN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_IOPEEN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_IOPERST; }
};

template<> struct peripheral_traits<gpiof_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_IOPFEN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_IOPFEN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_IOPFRST; }
};

template<> struct peripheral_traits<gpiog_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_IOPGEN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_IOPGEN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_IOPGRST; }
};

template<> struct peripheral_traits<afio_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_AFIOEN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_AFIOEN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_AFIORST; }
};

template<> struct peripheral_traits<dma1_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_DMA1EN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_DMA1EN; }
};

template<> struct peripheral_traits<dma2_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_DMA2EN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_DMA2EN; }
};

template<> struct peripheral_traits<sdio_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_SDIOEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_SDIOEN; }
};

template<> struct peripheral_traits<bkp_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_BKPEN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_BKPEN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_BKPRST; }
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

template<> struct peripheral_traits<tim8_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM8EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_TIM8EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_TIM8RST; }
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

template<> struct peripheral_traits<tim4_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM4EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_TIM4EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_TIM4RST; }
};

template<> struct peripheral_traits<tim5_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM5EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_TIM5EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_TIM5RST; }
};

template<> struct peripheral_traits<tim9_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM9EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_TIM9EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_TIM9RST; }
};

template<> struct peripheral_traits<tim12_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM12EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_TIM12EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_TIM12RST; }
};

template<> struct peripheral_traits<tim10_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM10EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_TIM10EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_TIM10RST; }
};

template<> struct peripheral_traits<tim11_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_TIM11EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_TIM11EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_TIM11RST; }
};

template<> struct peripheral_traits<tim13_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_TIM13EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_TIM13EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_TIM13RST; }
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

template<> struct peripheral_traits<spi3_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_SPI3EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_SPI3EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_SPI3RST; }
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

template<> struct peripheral_traits<adc1_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_ADC1EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_ADC1EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_ADC1RST; }
};

template<> struct peripheral_traits<adc2_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_ADC2EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_ADC2EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_ADC2RST; }
};

template<> struct peripheral_traits<adc3_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_ADC3EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_ADC3EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_ADC3RST; }
};

template<> struct peripheral_traits<dac_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_DACEN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_DACEN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_DACRST; }
};

template<> struct peripheral_traits<uart4_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_UART4EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_UART4EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_UART4RST; }
};

template<> struct peripheral_traits<uart5_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_UART5EN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_UART5EN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_UART5RST; }
};

template<> struct peripheral_traits<crc_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_CRCEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_CRCEN; }
};

template<> struct peripheral_traits<usb_t>
{
    static void enable() { RCC.APB1ENR |= rcc_t::APB1ENR_USBEN; }
    static void disable() { RCC.APB1ENR &= ~rcc_t::APB1ENR_USBEN; }
    static void reset() { RCC.APB1RSTR |= rcc_t::APB1RSTR_USBRST; }
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
    , TAMPER = 2
    , RTC = 3
    , FLASH = 4
    , RCC = 5
    , EXTI0 = 6
    , EXTI1 = 7
    , EXTI2 = 8
    , EXTI3 = 9
    , EXTI4 = 10
    , DMA1_CHANNEL1 = 11
    , DMA1_CHANNEL2 = 12
    , DMA1_CHANNEL3 = 13
    , DMA1_CHANNEL4 = 14
    , DMA1_CHANNEL5 = 15
    , DMA1_CHANNEL6 = 16
    , DMA1_CHANNEL7 = 17
    , ADC1_2 = 18
    , USB_HP_CAN_TX = 19
    , USB_LP_CAN_RX0 = 20
    , CAN_RX1 = 21
    , CAN_SCE = 22
    , EXTI9_5 = 23
    , TIM1_BRK = 24
    , TIM1_UP = 25
    , TIM1_TRG_COM = 26
    , TIM1_CC = 27
    , TIM2 = 28
    , TIM3 = 29
    , TIM4 = 30
    , I2C1_EV = 31
    , I2C1_ER = 32
    , I2C2_EV = 33
    , I2C2_ER = 34
    , SPI1 = 35
    , SPI2 = 36
    , USART1 = 37
    , USART2 = 38
    , USART3 = 39
    , EXTI15_10 = 40
    , RTCALARM = 41
    , TIM8_BRK = 43
    , TIM8_UP = 44
    , TIM8_TRG_COM = 45
    , TIM8_CC = 46
    , ADC3 = 47
    , FSMC = 48
    , SDIO = 49
    , TIM5 = 50
    , SPI3 = 51
    , UART4 = 52
    , UART5 = 53
    , TIM6 = 54
    , TIM7 = 55
    , DMA2_CHANNEL1 = 56
    , DMA2_CHANNEL2 = 57
    , DMA2_CHANNEL3 = 58
    , DMA2_CHANNEL4_5 = 59
    };
};
