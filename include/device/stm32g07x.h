#pragma once

#include <stdint.h>

////
//
//    STM32G07x
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

namespace stm32g07x
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
    reserved_t<247>      _0;
    volatile uint32_t    HWCFGR;               // [Read-write] hardware configuration register
    volatile uint32_t    VERR;                 // [Read-only] EXTI IP Version register
    volatile uint32_t    IPIDR;                // [Read-only] EXTI Identification register
    volatile uint32_t    SIDR;                 // [Read-only] EXTI Size ID register

    template<uint32_t X>
    static constexpr uint32_t KR_KEY =                 // Key value (write only, read 0x0000) (16 bits)
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

    static constexpr uint32_t SR_WVU = 0x4;            // Watchdog counter window value update
    static constexpr uint32_t SR_RVU = 0x2;            // Watchdog counter reload value update
    static constexpr uint32_t SR_PVU = 0x1;            // Watchdog prescaler value update
    static const uint32_t SR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t WINR_WIN =                 // Watchdog counter window value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t WINR_RESET_VALUE = 0xfff;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR_WINDOW =              // Support of Window function (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_PR_DEFAULT =          // Prescaler default value (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    static const uint32_t HWCFGR_RESET_VALUE = 0x71;

    template<uint32_t X>
    static constexpr uint32_t VERR_MINREV =              // Minor Revision number (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t VERR_MAJREV =              // Major Revision number (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    static const uint32_t VERR_RESET_VALUE = 0x23;


    static const uint32_t IPIDR_RESET_VALUE = 0x120041;


    static const uint32_t SIDR_RESET_VALUE = 0xa3c5dd01;
};

static iwdg_t& IWDG = *reinterpret_cast<iwdg_t*>(0x40003000);

#define HAVE_PERIPHERAL_IWDG


////
//
//    System window watchdog
//
////

struct wwdg_t
{
    volatile uint32_t    CR;                   // [Read-write] Control register
    volatile uint32_t    CFR;                  // [Read-write] Configuration register
    volatile uint32_t    SR;                   // [Read-write] Status register

    static constexpr uint32_t CR_WDGA = 0x80;          // Activation bit
    template<uint32_t X>
    static constexpr uint32_t CR_T =                   // 7-bit counter (MSB to LSB) (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t CR_RESET_VALUE = 0x7f;

    template<uint32_t X>
    static constexpr uint32_t CFR_WDGTB =               // Timer base (3 bits)
        bit_field_t<11, 0x7>::value<X>();
    static constexpr uint32_t CFR_EWI = 0x200;          // Early wakeup interrupt
    template<uint32_t X>
    static constexpr uint32_t CFR_W =                   // 7-bit window value (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t CFR_RESET_VALUE = 0x7f;

    static constexpr uint32_t SR_EWIF = 0x1;           // Early wakeup interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint8_t WWDG = 0; // Window watchdog interrupt
};

static wwdg_t& WWDG = *reinterpret_cast<wwdg_t*>(0x40002c00);

#define HAVE_PERIPHERAL_WWDG


////
//
//    Flash
//
////

struct flash_t
{
    volatile uint32_t    ACR;                  // [Read-write] Access control register
    reserved_t<1>        _0;
    volatile uint32_t    KEYR;                 // [Write-only] Flash key register
    volatile uint32_t    OPTKEYR;              // [Write-only] Option byte key register
    volatile uint32_t    SR;                   // [Read-write] Status register
    volatile uint32_t    CR;                   // [Read-write] Flash control register
    volatile uint32_t    ECCR;                 // Flash ECC register
    reserved_t<1>        _1;
    volatile uint32_t    OPTR;                 // [Read-write] Flash option register
    volatile uint32_t    PCROP1ASR;            // [Read-only] Flash PCROP zone A Start address register
    volatile uint32_t    PCROP1AER;            // [Read-only] Flash PCROP zone A End address register
    volatile uint32_t    WRP1AR;               // [Read-only] Flash WRP area A address register
    volatile uint32_t    WRP1BR;               // [Read-only] Flash WRP area B address register
    volatile uint32_t    PCROP1BSR;            // [Read-only] Flash PCROP zone B Start address register
    volatile uint32_t    PCROP1BER;            // [Read-only] Flash PCROP zone B End address register
    reserved_t<17>       _2;
    volatile uint32_t    SECR;                 // [Read-only] Flash Security register

    template<uint32_t X>
    static constexpr uint32_t ACR_LATENCY =             // Latency (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t ACR_PRFTEN = 0x100;       // Prefetch enable
    static constexpr uint32_t ACR_ICEN = 0x200;         // Instruction cache enable
    static constexpr uint32_t ACR_ICRST = 0x800;        // Instruction cache reset
    static constexpr uint32_t ACR_EMPTY = 0x10000;      // Flash User area empty
    static constexpr uint32_t ACR_DBG_SWEN = 0x40000;   // Debug access software enable
    static const uint32_t ACR_RESET_VALUE = 0x600;


    static const uint32_t KEYR_RESET_VALUE = 0x0;


    static const uint32_t OPTKEYR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_EOP = 0x1;            // End of operation
    static constexpr uint32_t SR_OPERR = 0x2;          // Operation error
    static constexpr uint32_t SR_PROGERR = 0x8;        // Programming error
    static constexpr uint32_t SR_WRPERR = 0x10;        // Write protected error
    static constexpr uint32_t SR_PGAERR = 0x20;        // Programming alignment error
    static constexpr uint32_t SR_SIZERR = 0x40;        // Size error
    static constexpr uint32_t SR_PGSERR = 0x80;        // Programming sequence error
    static constexpr uint32_t SR_MISERR = 0x100;       // Fast programming data miss error
    static constexpr uint32_t SR_FASTERR = 0x200;      // Fast programming error
    static constexpr uint32_t SR_RDERR = 0x4000;       // PCROP read error
    static constexpr uint32_t SR_OPTVERR = 0x8000;     // Option and Engineering bits loading validity error
    static constexpr uint32_t SR_BSY = 0x10000;        // Busy
    static constexpr uint32_t SR_CFGBSY = 0x40000;     // Programming or erase configuration busy.
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_PG = 0x1;             // Programming
    static constexpr uint32_t CR_PER = 0x2;            // Page erase
    static constexpr uint32_t CR_MER = 0x4;            // Mass erase
    template<uint32_t X>
    static constexpr uint32_t CR_PNB =                 // Page number (6 bits)
        bit_field_t<3, 0x3f>::value<X>();
    static constexpr uint32_t CR_STRT = 0x10000;       // Start
    static constexpr uint32_t CR_OPTSTRT = 0x20000;    // Options modification start
    static constexpr uint32_t CR_FSTPG = 0x40000;      // Fast programming
    static constexpr uint32_t CR_EOPIE = 0x1000000;    // End of operation interrupt enable
    static constexpr uint32_t CR_ERRIE = 0x2000000;    // Error interrupt enable
    static constexpr uint32_t CR_RDERRIE = 0x4000000;  // PCROP read error interrupt enable
    static constexpr uint32_t CR_OBL_LAUNCH = 0x8000000;// Force the option byte loading
    static constexpr uint32_t CR_SEC_PROT = 0x10000000;// Securable memory area protection enable
    static constexpr uint32_t CR_OPTLOCK = 0x40000000; // Options Lock
    static constexpr uint32_t CR_LOCK = 0x80000000;    // FLASH_CR Lock
    static const uint32_t CR_RESET_VALUE = 0xc0000000;

    template<uint32_t X>
    static constexpr uint32_t ECCR_ADDR_ECC =            // ECC fail address (14 bits), Read-only
        bit_field_t<0, 0x3fff>::value<X>();
    static constexpr uint32_t ECCR_SYSF_ECC = 0x100000;  // ECC fail for Corrected ECC Error or Double ECC Error in info block, Read-only
    static constexpr uint32_t ECCR_ECCIE = 0x1000000;    // ECC correction interrupt enable, Read-write
    static constexpr uint32_t ECCR_ECCC = 0x40000000;    // ECC correction, Read-write
    static constexpr uint32_t ECCR_ECCD = 0x80000000;    // ECC detection, Read-write
    static const uint32_t ECCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OPTR_RDP =                 // Read protection level (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t OPTR_BOREN = 0x100;        // BOR reset Level
    template<uint32_t X>
    static constexpr uint32_t OPTR_BORF_LEV =            // These bits contain the VDD supply level threshold that activates the reset (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OPTR_BORR_LEV =            // These bits contain the VDD supply level threshold that releases the reset. (2 bits)
        bit_field_t<11, 0x3>::value<X>();
    static constexpr uint32_t OPTR_nRST_STOP = 0x2000;   // nRST_STOP
    static constexpr uint32_t OPTR_nRST_STDBY = 0x4000;  // nRST_STDBY
    static constexpr uint32_t OPTR_nRSTS_HDW = 0x8000;   // nRSTS_HDW
    static constexpr uint32_t OPTR_IDWG_SW = 0x10000;    // Independent watchdog selection
    static constexpr uint32_t OPTR_IWDG_STOP = 0x20000;  // Independent watchdog counter freeze in Stop mode
    static constexpr uint32_t OPTR_IWDG_STDBY = 0x40000; // Independent watchdog counter freeze in Standby mode
    static constexpr uint32_t OPTR_WWDG_SW = 0x80000;    // Window watchdog selection
    static constexpr uint32_t OPTR_RAM_PARITY_CHECK = 0x400000;// SRAM parity check control
    static constexpr uint32_t OPTR_nBOOT_SEL = 0x1000000;// nBOOT_SEL
    static constexpr uint32_t OPTR_nBOOT1 = 0x2000000;   // Boot configuration
    static constexpr uint32_t OPTR_nBOOT0 = 0x4000000;   // nBOOT0 option bit
    template<uint32_t X>
    static constexpr uint32_t OPTR_NRST_MODE =           // NRST_MODE (2 bits)
        bit_field_t<27, 0x3>::value<X>();
    static constexpr uint32_t OPTR_IRHEN = 0x20000000;   // Internal reset holder enable bit
    static const uint32_t OPTR_RESET_VALUE = 0xf0000000;

    template<uint32_t X>
    static constexpr uint32_t PCROP1ASR_PCROP1A_STRT =        // PCROP1A area start offset (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PCROP1ASR_RESET_VALUE = 0xf0000000;

    template<uint32_t X>
    static constexpr uint32_t PCROP1AER_PCROP1A_END =         // PCROP1A area end offset (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t PCROP1AER_PCROP_RDP = 0x80000000;// PCROP area preserved when RDP level decreased
    static const uint32_t PCROP1AER_RESET_VALUE = 0xf0000000;

    template<uint32_t X>
    static constexpr uint32_t WRP1AR_WRP1A_STRT =          // WRP area A start offset (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t WRP1AR_WRP1A_END =           // WRP area A end offset (6 bits)
        bit_field_t<16, 0x3f>::value<X>();
    static const uint32_t WRP1AR_RESET_VALUE = 0xf0000000;

    template<uint32_t X>
    static constexpr uint32_t WRP1BR_WRP1B_STRT =          // WRP area B start offset (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t WRP1BR_WRP1B_END =           // WRP area B end offset (6 bits)
        bit_field_t<16, 0x3f>::value<X>();
    static const uint32_t WRP1BR_RESET_VALUE = 0xf0000000;

    template<uint32_t X>
    static constexpr uint32_t PCROP1BSR_PCROP1B_STRT =        // PCROP1B area start offset (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PCROP1BSR_RESET_VALUE = 0xf0000000;

    template<uint32_t X>
    static constexpr uint32_t PCROP1BER_PCROP1B_END =         // PCROP1B area end offset (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PCROP1BER_RESET_VALUE = 0xf0000000;

    template<uint32_t X>
    static constexpr uint32_t SECR_SEC_SIZE =            // Securable memory area size (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t SECR_BOOT_LOCK = 0x10000;  // used to force boot from user area
    static const uint32_t SECR_RESET_VALUE = 0xf0000000;

    static constexpr uint8_t FLASH = 3; // Flash global interrupt
};

static flash_t& FLASH = *reinterpret_cast<flash_t*>(0x40022000);

#define HAVE_PERIPHERAL_FLASH


////
//
//    Debug support
//
////

struct dbg_t
{
    volatile uint32_t    IDCODE;               // [Read-only] MCU Device ID Code Register
    volatile uint32_t    CR;                   // [Read-write] Debug MCU Configuration Register
    volatile uint32_t    APB_FZ1;              // [Read-write] DBG APB freeze register 1
    volatile uint32_t    APB_FZ2;              // [Read-write] DBG APB freeze register 2

    template<uint32_t X>
    static constexpr uint32_t IDCODE_DEV_ID =              // Device Identifier (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IDCODE_REV_ID =              // Revision Identifier (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t IDCODE_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_DBG_STOP = 0x2;       // Debug Stop Mode
    static constexpr uint32_t CR_DBG_STANDBY = 0x4;    // Debug Standby Mode
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t APB_FZ1_DBG_TIMER2_STOP = 0x1;// Debug Timer 2 stopped when Core is halted
    static constexpr uint32_t APB_FZ1_DBG_TIM3_STOP = 0x2;  // TIM3 counter stopped when core is halted
    static constexpr uint32_t APB_FZ1_DBG_TIMER6_STOP = 0x10;// Debug Timer 6 stopped when Core is halted
    static constexpr uint32_t APB_FZ1_DBG_TIM7_STOP = 0x20; // TIM7 counter stopped when core is halted
    static constexpr uint32_t APB_FZ1_DBG_RTC_STOP = 0x400; // Debug RTC stopped when Core is halted
    static constexpr uint32_t APB_FZ1_DBG_WWDG_STOP = 0x800;// Debug Window Wachdog stopped when Core is halted
    static constexpr uint32_t APB_FZ1_DBG_IWDG_STOP = 0x1000;// Debug Independent Wachdog stopped when Core is halted
    static constexpr uint32_t APB_FZ1_DBG_I2C1_STOP = 0x200000;// I2C1 SMBUS timeout mode stopped when core is halted
    static constexpr uint32_t APB_FZ1_DBG_LPTIM2_STOP = 0x40000000;// Clocking of LPTIMER2 counter when the core is halted
    static constexpr uint32_t APB_FZ1_DBG_LPTIM1_STOP = 0x80000000;// Clocking of LPTIMER1 counter when the core is halted
    static const uint32_t APB_FZ1_RESET_VALUE = 0x0;

    static constexpr uint32_t APB_FZ2_DBG_TIM1_STOP = 0x800;// DBG_TIM1_STOP
    static constexpr uint32_t APB_FZ2_DBG_TIM14_STOP = 0x8000;// DBG_TIM14_STOP
    static constexpr uint32_t APB_FZ2_DBG_TIM15_STOP = 0x10000;// DBG_TIM15_STOP
    static constexpr uint32_t APB_FZ2_DBG_TIM16_STOP = 0x20000;// DBG_TIM16_STOP
    static constexpr uint32_t APB_FZ2_DBG_TIM17_STOP = 0x40000;// DBG_TIM17_STOP
    static const uint32_t APB_FZ2_RESET_VALUE = 0x0;
};

static dbg_t& DBG = *reinterpret_cast<dbg_t*>(0x40015800);

#define HAVE_PERIPHERAL_DBG


////
//
//    Reset and clock control
//
////

struct rcc_t
{
    volatile uint32_t    CR;                   // [Read-write] Clock control register
    volatile uint32_t    ICSCR;                // Internal clock sources calibration register
    volatile uint32_t    CFGR;                 // Clock configuration register
    volatile uint32_t    PLLSYSCFGR;           // [Read-write] PLL configuration register
    reserved_t<2>        _0;
    volatile uint32_t    CIER;                 // [Read-write] Clock interrupt enable register
    volatile uint32_t    CIFR;                 // [Read-only] Clock interrupt flag register
    volatile uint32_t    CICR;                 // [Write-only] Clock interrupt clear register
    volatile uint32_t    IOPRSTR;              // [Read-write] GPIO reset register
    volatile uint32_t    AHBRSTR;              // [Read-write] AHB peripheral reset register
    volatile uint32_t    APBRSTR1;             // [Read-write] APB peripheral reset register 1
    volatile uint32_t    APBRSTR2;             // [Read-write] APB peripheral reset register 2
    volatile uint32_t    IOPENR;               // [Read-write] GPIO clock enable register
    volatile uint32_t    AHBENR;               // [Read-write] AHB peripheral clock enable register
    volatile uint32_t    APBENR1;              // [Read-write] APB peripheral clock enable register 1
    volatile uint32_t    APBENR2;              // [Read-write] APB peripheral clock enable register 2
    volatile uint32_t    IOPSMENR;             // [Read-write] GPIO in Sleep mode clock enable register
    volatile uint32_t    AHBSMENR;             // [Read-write] AHB peripheral clock enable in Sleep mode register
    volatile uint32_t    APBSMENR1;            // [Read-write] APB peripheral clock enable in Sleep mode register 1
    volatile uint32_t    APBSMENR2;            // [Read-write] APB peripheral clock enable in Sleep mode register 2
    volatile uint32_t    CCIPR;                // [Read-write] Peripherals independent clock configuration register
    reserved_t<1>        _1;
    volatile uint32_t    BDCR;                 // [Read-write] RTC domain control register
    volatile uint32_t    CSR;                  // [Read-write] Control/status register

    static constexpr uint32_t CR_HSION = 0x100;        // HSI16 clock enable
    static constexpr uint32_t CR_HSIKERON = 0x200;     // HSI16 always enable for peripheral kernels
    static constexpr uint32_t CR_HSIRDY = 0x400;       // HSI16 clock ready flag
    template<uint32_t X>
    static constexpr uint32_t CR_HSIDIV =              // HSI16 clock division factor (3 bits)
        bit_field_t<11, 0x7>::value<X>();
    static constexpr uint32_t CR_HSEON = 0x10000;      // HSE clock enable
    static constexpr uint32_t CR_HSERDY = 0x20000;     // HSE clock ready flag
    static constexpr uint32_t CR_HSEBYP = 0x40000;     // HSE crystal oscillator bypass
    static constexpr uint32_t CR_CSSON = 0x80000;      // Clock security system enable
    static constexpr uint32_t CR_PLLON = 0x1000000;    // PLL enable
    static constexpr uint32_t CR_PLLRDY = 0x2000000;   // PLL clock ready flag
    static const uint32_t CR_RESET_VALUE = 0x63;

    template<uint32_t X>
    static constexpr uint32_t ICSCR_HSICAL =              // HSI16 clock calibration (8 bits), Read-only
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ICSCR_HSITRIM =             // HSI16 clock trimming (7 bits), Read-write
        bit_field_t<8, 0x7f>::value<X>();
    static const uint32_t ICSCR_RESET_VALUE = 0x10000000;

    template<uint32_t X>
    static constexpr uint32_t CFGR_MCOPRE =              // Microcontroller clock output prescaler (3 bits), Read-only
        bit_field_t<28, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_MCOSEL =              // Microcontroller clock output (3 bits), Read-write
        bit_field_t<24, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_PPRE =                // APB prescaler (3 bits), Read-write
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_HPRE =                // AHB prescaler (4 bits), Read-write
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_SWS =                 // System clock switch status (3 bits), Read-only
        bit_field_t<3, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_SW =                  // System clock switch (3 bits), Read-write
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t CFGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PLLSYSCFGR_PLLSRC =              // PLL input clock source (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PLLSYSCFGR_PLLM =                // Division factor M of the PLL input clock divider (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PLLSYSCFGR_PLLN =                // PLL frequency multiplication factor N (7 bits)
        bit_field_t<8, 0x7f>::value<X>();
    static constexpr uint32_t PLLSYSCFGR_PLLPEN = 0x10000;     // PLLPCLK clock output enable
    template<uint32_t X>
    static constexpr uint32_t PLLSYSCFGR_PLLP =                // PLL VCO division factor P for PLLPCLK clock output (5 bits)
        bit_field_t<17, 0x1f>::value<X>();
    static constexpr uint32_t PLLSYSCFGR_PLLQEN = 0x1000000;   // PLLQCLK clock output enable
    template<uint32_t X>
    static constexpr uint32_t PLLSYSCFGR_PLLQ =                // PLL VCO division factor Q for PLLQCLK clock output (3 bits)
        bit_field_t<25, 0x7>::value<X>();
    static constexpr uint32_t PLLSYSCFGR_PLLREN = 0x10000000;  // PLLRCLK clock output enable
    template<uint32_t X>
    static constexpr uint32_t PLLSYSCFGR_PLLR =                // PLL VCO division factor R for PLLRCLK clock output (3 bits)
        bit_field_t<29, 0x7>::value<X>();
    static const uint32_t PLLSYSCFGR_RESET_VALUE = 0x1000;

    static constexpr uint32_t CIER_LSIRDYIE = 0x1;       // LSI ready interrupt enable
    static constexpr uint32_t CIER_LSERDYIE = 0x2;       // LSE ready interrupt enable
    static constexpr uint32_t CIER_HSIRDYIE = 0x8;       // HSI ready interrupt enable
    static constexpr uint32_t CIER_HSERDYIE = 0x10;      // HSE ready interrupt enable
    static constexpr uint32_t CIER_PLLSYSRDYIE = 0x20;   // PLL ready interrupt enable
    static const uint32_t CIER_RESET_VALUE = 0x0;

    static constexpr uint32_t CIFR_LSIRDYF = 0x1;        // LSI ready interrupt flag
    static constexpr uint32_t CIFR_LSERDYF = 0x2;        // LSE ready interrupt flag
    static constexpr uint32_t CIFR_HSIRDYF = 0x8;        // HSI ready interrupt flag
    static constexpr uint32_t CIFR_HSERDYF = 0x10;       // HSE ready interrupt flag
    static constexpr uint32_t CIFR_PLLSYSRDYF = 0x20;    // PLL ready interrupt flag
    static constexpr uint32_t CIFR_CSSF = 0x100;         // Clock security system interrupt flag
    static constexpr uint32_t CIFR_LSECSSF = 0x200;      // LSE Clock security system interrupt flag
    static const uint32_t CIFR_RESET_VALUE = 0x0;

    static constexpr uint32_t CICR_LSIRDYC = 0x1;        // LSI ready interrupt clear
    static constexpr uint32_t CICR_LSERDYC = 0x2;        // LSE ready interrupt clear
    static constexpr uint32_t CICR_HSIRDYC = 0x8;        // HSI ready interrupt clear
    static constexpr uint32_t CICR_HSERDYC = 0x10;       // HSE ready interrupt clear
    static constexpr uint32_t CICR_PLLSYSRDYC = 0x20;    // PLL ready interrupt clear
    static constexpr uint32_t CICR_CSSC = 0x100;         // Clock security system interrupt clear
    static constexpr uint32_t CICR_LSECSSC = 0x200;      // LSE Clock security system interrupt clear
    static const uint32_t CICR_RESET_VALUE = 0x0;

    static constexpr uint32_t IOPRSTR_IOPARST = 0x1;        // I/O port A reset
    static constexpr uint32_t IOPRSTR_IOPBRST = 0x2;        // I/O port B reset
    static constexpr uint32_t IOPRSTR_IOPCRST = 0x4;        // I/O port C reset
    static constexpr uint32_t IOPRSTR_IOPDRST = 0x8;        // I/O port D reset
    static constexpr uint32_t IOPRSTR_IOPFRST = 0x20;       // I/O port F reset
    static const uint32_t IOPRSTR_RESET_VALUE = 0x0;

    static constexpr uint32_t AHBRSTR_DMARST = 0x1;         // DMA1 reset
    static constexpr uint32_t AHBRSTR_FLASHRST = 0x100;     // FLITF reset
    static constexpr uint32_t AHBRSTR_CRCRST = 0x1000;      // CRC reset
    static constexpr uint32_t AHBRSTR_AESRST = 0x10000;     // AES hardware accelerator reset
    static constexpr uint32_t AHBRSTR_RNGRST = 0x40000;     // Random number generator reset
    static const uint32_t AHBRSTR_RESET_VALUE = 0x0;

    static constexpr uint32_t APBRSTR1_TIM2RST = 0x1;        // TIM2 timer reset
    static constexpr uint32_t APBRSTR1_TIM3RST = 0x2;        // TIM3 timer reset
    static constexpr uint32_t APBRSTR1_TIM6RST = 0x10;       // TIM6 timer reset
    static constexpr uint32_t APBRSTR1_TIM7RST = 0x20;       // TIM7 timer reset
    static constexpr uint32_t APBRSTR1_SPI2RST = 0x4000;     // SPI2 reset
    static constexpr uint32_t APBRSTR1_USART2RST = 0x20000;  // USART2 reset
    static constexpr uint32_t APBRSTR1_USART3RST = 0x40000;  // USART3 reset
    static constexpr uint32_t APBRSTR1_USART4RST = 0x80000;  // USART4 reset
    static constexpr uint32_t APBRSTR1_LPUART1RST = 0x100000;// LPUART1 reset
    static constexpr uint32_t APBRSTR1_I2C1RST = 0x200000;   // I2C1 reset
    static constexpr uint32_t APBRSTR1_I2C2RST = 0x400000;   // I2C2 reset
    static constexpr uint32_t APBRSTR1_CECRST = 0x1000000;   // HDMI CEC reset
    static constexpr uint32_t APBRSTR1_UCPD1RST = 0x2000000; // UCPD1 reset
    static constexpr uint32_t APBRSTR1_UCPD2RST = 0x4000000; // UCPD2 reset
    static constexpr uint32_t APBRSTR1_DBGRST = 0x8000000;   // Debug support reset
    static constexpr uint32_t APBRSTR1_PWRRST = 0x10000000;  // Power interface reset
    static constexpr uint32_t APBRSTR1_DAC1RST = 0x20000000; // DAC1 interface reset
    static constexpr uint32_t APBRSTR1_LPTIM2RST = 0x40000000;// Low Power Timer 2 reset
    static constexpr uint32_t APBRSTR1_LPTIM1RST = 0x80000000;// Low Power Timer 1 reset
    static const uint32_t APBRSTR1_RESET_VALUE = 0x0;

    static constexpr uint32_t APBRSTR2_SYSCFGRST = 0x1;      // SYSCFG, COMP and VREFBUF reset
    static constexpr uint32_t APBRSTR2_TIM1RST = 0x800;      // TIM1 timer reset
    static constexpr uint32_t APBRSTR2_SPI1RST = 0x1000;     // SPI1 reset
    static constexpr uint32_t APBRSTR2_USART1RST = 0x4000;   // USART1 reset
    static constexpr uint32_t APBRSTR2_TIM14RST = 0x8000;    // TIM14 timer reset
    static constexpr uint32_t APBRSTR2_TIM15RST = 0x10000;   // TIM15 timer reset
    static constexpr uint32_t APBRSTR2_TIM16RST = 0x20000;   // TIM16 timer reset
    static constexpr uint32_t APBRSTR2_TIM17RST = 0x40000;   // TIM17 timer reset
    static constexpr uint32_t APBRSTR2_ADCRST = 0x100000;    // ADC reset
    static const uint32_t APBRSTR2_RESET_VALUE = 0x0;

    static constexpr uint32_t IOPENR_IOPAEN = 0x1;         // I/O port A clock enable
    static constexpr uint32_t IOPENR_IOPBEN = 0x2;         // I/O port B clock enable
    static constexpr uint32_t IOPENR_IOPCEN = 0x4;         // I/O port C clock enable
    static constexpr uint32_t IOPENR_IOPDEN = 0x8;         // I/O port D clock enable
    static constexpr uint32_t IOPENR_IOPFEN = 0x20;        // I/O port F clock enable
    static const uint32_t IOPENR_RESET_VALUE = 0x0;

    static constexpr uint32_t AHBENR_DMAEN = 0x1;          // DMA clock enable
    static constexpr uint32_t AHBENR_FLASHEN = 0x100;      // Flash memory interface clock enable
    static constexpr uint32_t AHBENR_CRCEN = 0x1000;       // CRC clock enable
    static constexpr uint32_t AHBENR_AESEN = 0x10000;      // AES hardware accelerator
    static constexpr uint32_t AHBENR_RNGEN = 0x40000;      // Random number generator clock enable
    static const uint32_t AHBENR_RESET_VALUE = 0x0;

    static constexpr uint32_t APBENR1_TIM2EN = 0x1;         // TIM2 timer clock enable
    static constexpr uint32_t APBENR1_TIM3EN = 0x2;         // TIM3 timer clock enable
    static constexpr uint32_t APBENR1_TIM6EN = 0x10;        // TIM6 timer clock enable
    static constexpr uint32_t APBENR1_TIM7EN = 0x20;        // TIM7 timer clock enable
    static constexpr uint32_t APBENR1_RTCAPBEN = 0x400;     // RTC APB clock enable
    static constexpr uint32_t APBENR1_WWDGEN = 0x800;       // WWDG clock enable
    static constexpr uint32_t APBENR1_SPI2EN = 0x4000;      // SPI2 clock enable
    static constexpr uint32_t APBENR1_USART2EN = 0x20000;   // USART2 clock enable
    static constexpr uint32_t APBENR1_USART3EN = 0x40000;   // USART3 clock enable
    static constexpr uint32_t APBENR1_USART4EN = 0x80000;   // USART4 clock enable
    static constexpr uint32_t APBENR1_LPUART1EN = 0x100000; // LPUART1 clock enable
    static constexpr uint32_t APBENR1_I2C1EN = 0x200000;    // I2C1 clock enable
    static constexpr uint32_t APBENR1_I2C2EN = 0x400000;    // I2C2 clock enable
    static constexpr uint32_t APBENR1_CECEN = 0x1000000;    // HDMI CEC clock enable
    static constexpr uint32_t APBENR1_UCPD1EN = 0x2000000;  // UCPD1 clock enable
    static constexpr uint32_t APBENR1_UCPD2EN = 0x4000000;  // UCPD2 clock enable
    static constexpr uint32_t APBENR1_DBGEN = 0x8000000;    // Debug support clock enable
    static constexpr uint32_t APBENR1_PWREN = 0x10000000;   // Power interface clock enable
    static constexpr uint32_t APBENR1_DAC1EN = 0x20000000;  // DAC1 interface clock enable
    static constexpr uint32_t APBENR1_LPTIM2EN = 0x40000000;// LPTIM2 clock enable
    static constexpr uint32_t APBENR1_LPTIM1EN = 0x80000000;// LPTIM1 clock enable
    static const uint32_t APBENR1_RESET_VALUE = 0x0;

    static constexpr uint32_t APBENR2_SYSCFGEN = 0x1;       // SYSCFG, COMP and VREFBUF clock enable
    static constexpr uint32_t APBENR2_TIM1EN = 0x800;       // TIM1 timer clock enable
    static constexpr uint32_t APBENR2_SPI1EN = 0x1000;      // SPI1 clock enable
    static constexpr uint32_t APBENR2_USART1EN = 0x4000;    // USART1 clock enable
    static constexpr uint32_t APBENR2_TIM14EN = 0x8000;     // TIM14 timer clock enable
    static constexpr uint32_t APBENR2_TIM15EN = 0x10000;    // TIM15 timer clock enable
    static constexpr uint32_t APBENR2_TIM16EN = 0x20000;    // TIM16 timer clock enable
    static constexpr uint32_t APBENR2_TIM17EN = 0x40000;    // TIM16 timer clock enable
    static constexpr uint32_t APBENR2_ADCEN = 0x100000;     // ADC clock enable
    static const uint32_t APBENR2_RESET_VALUE = 0x0;

    static constexpr uint32_t IOPSMENR_IOPASMEN = 0x1;       // I/O port A clock enable during Sleep mode
    static constexpr uint32_t IOPSMENR_IOPBSMEN = 0x2;       // I/O port B clock enable during Sleep mode
    static constexpr uint32_t IOPSMENR_IOPCSMEN = 0x4;       // I/O port C clock enable during Sleep mode
    static constexpr uint32_t IOPSMENR_IOPDSMEN = 0x8;       // I/O port D clock enable during Sleep mode
    static constexpr uint32_t IOPSMENR_IOPFSMEN = 0x20;      // I/O port F clock enable during Sleep mode
    static const uint32_t IOPSMENR_RESET_VALUE = 0x0;

    static constexpr uint32_t AHBSMENR_DMASMEN = 0x1;        // DMA clock enable during Sleep mode
    static constexpr uint32_t AHBSMENR_FLASHSMEN = 0x100;    // Flash memory interface clock enable during Sleep mode
    static constexpr uint32_t AHBSMENR_SRAMSMEN = 0x200;     // SRAM clock enable during Sleep mode
    static constexpr uint32_t AHBSMENR_CRCSMEN = 0x1000;     // CRC clock enable during Sleep mode
    static constexpr uint32_t AHBSMENR_AESSMEN = 0x10000;    // AES hardware accelerator clock enable during Sleep mode
    static constexpr uint32_t AHBSMENR_RNGSMEN = 0x40000;    // Random number generator clock enable during Sleep mode
    static const uint32_t AHBSMENR_RESET_VALUE = 0x0;

    static constexpr uint32_t APBSMENR1_TIM2SMEN = 0x1;       // TIM2 timer clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_TIM3SMEN = 0x2;       // TIM3 timer clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_TIM6SMEN = 0x10;      // TIM6 timer clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_TIM7SMEN = 0x20;      // TIM7 timer clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_RTCAPBSMEN = 0x400;   // RTC APB clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_WWDGSMEN = 0x800;     // WWDG clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_SPI2SMEN = 0x4000;    // SPI2 clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_USART2SMEN = 0x20000; // USART2 clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_USART3SMEN = 0x40000; // USART3 clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_USART4SMEN = 0x80000; // USART4 clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_LPUART1SMEN = 0x100000;// LPUART1 clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_I2C1SMEN = 0x200000;  // I2C1 clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_I2C2SMEN = 0x400000;  // I2C2 clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_CECSMEN = 0x1000000;  // HDMI CEC clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_UCPD1SMEN = 0x2000000;// UCPD1 clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_UCPD2SMEN = 0x4000000;// UCPD2 clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_DBGSMEN = 0x8000000;  // Debug support clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_PWRSMEN = 0x10000000; // Power interface clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_DAC1SMEN = 0x20000000;// DAC1 interface clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_LPTIM2SMEN = 0x40000000;// Low Power Timer 2 clock enable during Sleep mode
    static constexpr uint32_t APBSMENR1_LPTIM1SMEN = 0x80000000;// Low Power Timer 1 clock enable during Sleep mode
    static const uint32_t APBSMENR1_RESET_VALUE = 0x0;

    static constexpr uint32_t APBSMENR2_SYSCFGSMEN = 0x1;     // SYSCFG, COMP and VREFBUF clock enable during Sleep mode
    static constexpr uint32_t APBSMENR2_TIM1SMEN = 0x800;     // TIM1 timer clock enable during Sleep mode
    static constexpr uint32_t APBSMENR2_SPI1SMEN = 0x1000;    // SPI1 clock enable during Sleep mode
    static constexpr uint32_t APBSMENR2_USART1SMEN = 0x4000;  // USART1 clock enable during Sleep mode
    static constexpr uint32_t APBSMENR2_TIM14SMEN = 0x8000;   // TIM14 timer clock enable during Sleep mode
    static constexpr uint32_t APBSMENR2_TIM15SMEN = 0x10000;  // TIM15 timer clock enable during Sleep mode
    static constexpr uint32_t APBSMENR2_TIM16SMEN = 0x20000;  // TIM16 timer clock enable during Sleep mode
    static constexpr uint32_t APBSMENR2_TIM17SMEN = 0x40000;  // TIM16 timer clock enable during Sleep mode
    static constexpr uint32_t APBSMENR2_ADCSMEN = 0x100000;   // ADC clock enable during Sleep mode
    static const uint32_t APBSMENR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCIPR_USART1SEL =           // USART1 clock source selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR_USART2SEL =           // USART2 clock source selection (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    static constexpr uint32_t CCIPR_CECSEL = 0x40;        // HDMI CEC clock source selection
    template<uint32_t X>
    static constexpr uint32_t CCIPR_LPUART1SEL =          // LPUART1 clock source selection (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR_I2C1SEL =             // I2C1 clock source selection (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR_I2S2SEL =             // I2S1 clock source selection (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR_LPTIM1SEL =           // LPTIM1 clock source selection (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR_LPTIM2SEL =           // LPTIM2 clock source selection (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    static constexpr uint32_t CCIPR_TIM1SEL = 0x400000;   // TIM1 clock source selection
    static constexpr uint32_t CCIPR_TIM15SEL = 0x1000000; // TIM15 clock source selection
    template<uint32_t X>
    static constexpr uint32_t CCIPR_RNGSEL =              // RNG clock source selection (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR_RNGDIV =              // Division factor of RNG clock divider (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR_ADCSEL =              // ADCs clock source selection (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t CCIPR_RESET_VALUE = 0x0;

    static constexpr uint32_t BDCR_LSEON = 0x1;          // LSE oscillator enable
    static constexpr uint32_t BDCR_LSERDY = 0x2;         // LSE oscillator ready
    static constexpr uint32_t BDCR_LSEBYP = 0x4;         // LSE oscillator bypass
    template<uint32_t X>
    static constexpr uint32_t BDCR_LSEDRV =              // LSE oscillator drive capability (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t BDCR_LSECSSON = 0x20;      // CSS on LSE enable
    static constexpr uint32_t BDCR_LSECSSD = 0x40;       // CSS on LSE failure Detection
    template<uint32_t X>
    static constexpr uint32_t BDCR_RTCSEL =              // RTC clock source selection (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t BDCR_RTCEN = 0x8000;       // RTC clock enable
    static constexpr uint32_t BDCR_BDRST = 0x10000;      // RTC domain software reset
    static constexpr uint32_t BDCR_LSCOEN = 0x1000000;   // Low-speed clock output (LSCO) enable
    static constexpr uint32_t BDCR_LSCOSEL = 0x2000000;  // Low-speed clock output selection
    static const uint32_t BDCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CSR_LSION = 0x1;          // LSI oscillator enable
    static constexpr uint32_t CSR_LSIRDY = 0x2;         // LSI oscillator ready
    static constexpr uint32_t CSR_RMVF = 0x800000;      // Remove reset flags
    static constexpr uint32_t CSR_OBLRSTF = 0x2000000;  // Option byte loader reset flag
    static constexpr uint32_t CSR_PINRSTF = 0x4000000;  // Pin reset flag
    static constexpr uint32_t CSR_PWRRSTF = 0x8000000;  // BOR or POR/PDR flag
    static constexpr uint32_t CSR_SFTRSTF = 0x10000000; // Software reset flag
    static constexpr uint32_t CSR_IWDGRSTF = 0x20000000;// Independent window watchdog reset flag
    static constexpr uint32_t CSR_WWDGRSTF = 0x40000000;// Window watchdog reset flag
    static constexpr uint32_t CSR_LPWRRSTF = 0x80000000;// Low-power reset flag
    static const uint32_t CSR_RESET_VALUE = 0x0;

    static constexpr uint8_t RCC = 4; // RCC global interrupt
};

static rcc_t& RCC = *reinterpret_cast<rcc_t*>(0x40021000);

#define HAVE_PERIPHERAL_RCC


////
//
//    Power control
//
////

struct pwr_t
{
    volatile uint32_t    CR1;                  // [Read-write] Power control register 1
    volatile uint32_t    CR2;                  // [Read-write] Power control register 2
    volatile uint32_t    CR3;                  // [Read-write] Power control register 3
    volatile uint32_t    CR4;                  // [Read-write] Power control register 4
    volatile uint32_t    SR1;                  // [Read-only] Power status register 1
    volatile uint32_t    SR2;                  // [Read-only] Power status register 2
    volatile uint32_t    SCR;                  // [Write-only] Power status clear register
    reserved_t<1>        _0;
    volatile uint32_t    PUCRA;                // [Read-write] Power Port A pull-up control register
    volatile uint32_t    PDCRA;                // [Read-write] Power Port A pull-down control register
    volatile uint32_t    PUCRB;                // [Read-write] Power Port B pull-up control register
    volatile uint32_t    PDCRB;                // [Read-write] Power Port B pull-down control register
    volatile uint32_t    PUCRC;                // [Read-write] Power Port C pull-up control register
    volatile uint32_t    PDCRC;                // [Read-write] Power Port C pull-down control register
    volatile uint32_t    PUCRD;                // [Read-write] Power Port D pull-up control register
    volatile uint32_t    PDCRD;                // [Read-write] Power Port D pull-down control register
    reserved_t<2>        _1;
    volatile uint32_t    PUCRF;                // [Read-write] Power Port F pull-up control register
    volatile uint32_t    PDCRF;                // [Read-write] Power Port F pull-down control register

    static constexpr uint32_t CR1_LPR = 0x4000;         // Low-power run
    template<uint32_t X>
    static constexpr uint32_t CR1_VOS =                 // Voltage scaling range selection (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t CR1_DBP = 0x100;          // Disable backup domain write protection
    static constexpr uint32_t CR1_FPD_LPSLP = 0x20;     // Flash memory powered down during Low-power sleep mode
    static constexpr uint32_t CR1_FPD_LPRUN = 0x10;     // Flash memory powered down during Low-power run mode
    static constexpr uint32_t CR1_FPD_STOP = 0x8;       // Flash memory powered down during Stop mode
    template<uint32_t X>
    static constexpr uint32_t CR1_LPMS =                // Low-power mode selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t CR1_RESET_VALUE = 0x200;

    static constexpr uint32_t CR2_PVDE = 0x1;           // Power voltage detector enable
    template<uint32_t X>
    static constexpr uint32_t CR2_PVDFT =               // Power voltage detector falling threshold selection (3 bits)
        bit_field_t<1, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_PVDRT =               // Power voltage detector rising threshold selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CR3_EWUP1 = 0x1;          // Enable Wakeup pin WKUP1
    static constexpr uint32_t CR3_EWUP2 = 0x2;          // Enable Wakeup pin WKUP2
    static constexpr uint32_t CR3_EWUP4 = 0x8;          // Enable Wakeup pin WKUP4
    static constexpr uint32_t CR3_EWUP5 = 0x10;         // Enable WKUP5 wakeup pin
    static constexpr uint32_t CR3_EWUP6 = 0x20;         // Enable WKUP6 wakeup pin
    static constexpr uint32_t CR3_RRS = 0x100;          // SRAM retention in Standby mode
    static constexpr uint32_t CR3_ULPEN = 0x200;        // Enable the periodical sampling mode for PDR detection
    static constexpr uint32_t CR3_APC = 0x400;          // Apply pull-up and pull-down configuration
    static constexpr uint32_t CR3_EIWUL = 0x8000;       // Enable internal wakeup line
    static const uint32_t CR3_RESET_VALUE = 0x8000;

    static constexpr uint32_t CR4_WP1 = 0x1;            // Wakeup pin WKUP1 polarity
    static constexpr uint32_t CR4_WP2 = 0x2;            // Wakeup pin WKUP2 polarity
    static constexpr uint32_t CR4_WP4 = 0x8;            // Wakeup pin WKUP4 polarity
    static constexpr uint32_t CR4_WP5 = 0x10;           // Wakeup pin WKUP5 polarity
    static constexpr uint32_t CR4_WP6 = 0x20;           // WKUP6 wakeup pin polarity
    static constexpr uint32_t CR4_VBE = 0x100;          // VBAT battery charging enable
    static constexpr uint32_t CR4_VBRS = 0x200;         // VBAT battery charging resistor selection
    static const uint32_t CR4_RESET_VALUE = 0x0;

    static constexpr uint32_t SR1_WUF1 = 0x1;           // Wakeup flag 1
    static constexpr uint32_t SR1_WUF2 = 0x2;           // Wakeup flag 2
    static constexpr uint32_t SR1_WUF4 = 0x8;           // Wakeup flag 4
    static constexpr uint32_t SR1_WUF5 = 0x10;          // Wakeup flag 5
    static constexpr uint32_t SR1_WUF6 = 0x20;          // Wakeup flag 6
    static constexpr uint32_t SR1_SBF = 0x100;          // Standby flag
    static constexpr uint32_t SR1_WUFI = 0x8000;        // Wakeup flag internal
    static const uint32_t SR1_RESET_VALUE = 0x0;

    static constexpr uint32_t SR2_PVDO = 0x800;         // Power voltage detector output
    static constexpr uint32_t SR2_VOSF = 0x400;         // Voltage scaling flag
    static constexpr uint32_t SR2_REGLPF = 0x200;       // Low-power regulator flag
    static constexpr uint32_t SR2_REGLPS = 0x100;       // Low-power regulator started
    static constexpr uint32_t SR2_FLASH_RDY = 0x80;     // Flash ready flag
    static const uint32_t SR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SCR_CSBF = 0x100;         // Clear standby flag
    static constexpr uint32_t SCR_CWUF6 = 0x20;         // Clear wakeup flag 6
    static constexpr uint32_t SCR_CWUF5 = 0x10;         // Clear wakeup flag 5
    static constexpr uint32_t SCR_CWUF4 = 0x8;          // Clear wakeup flag 4
    static constexpr uint32_t SCR_CWUF2 = 0x2;          // Clear wakeup flag 2
    static constexpr uint32_t SCR_CWUF1 = 0x1;          // Clear wakeup flag 1
    static const uint32_t SCR_RESET_VALUE = 0x0;

    static constexpr uint32_t PUCRA_PU15 = 0x8000;        // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU14 = 0x4000;        // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU13 = 0x2000;        // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU12 = 0x1000;        // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU11 = 0x800;         // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU10 = 0x400;         // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU9 = 0x200;          // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU8 = 0x100;          // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU7 = 0x80;           // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU6 = 0x40;           // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU5 = 0x20;           // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU4 = 0x10;           // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU3 = 0x8;            // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU2 = 0x4;            // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU1 = 0x2;            // Port A pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRA_PU0 = 0x1;            // Port A pull-up bit y (y=0..15)
    static const uint32_t PUCRA_RESET_VALUE = 0x0;

    static constexpr uint32_t PDCRA_PD15 = 0x8000;        // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD14 = 0x4000;        // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD13 = 0x2000;        // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD12 = 0x1000;        // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD11 = 0x800;         // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD10 = 0x400;         // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD9 = 0x200;          // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD8 = 0x100;          // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD7 = 0x80;           // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD6 = 0x40;           // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD5 = 0x20;           // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD4 = 0x10;           // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD3 = 0x8;            // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD2 = 0x4;            // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD1 = 0x2;            // Port A pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRA_PD0 = 0x1;            // Port A pull-down bit y (y=0..15)
    static const uint32_t PDCRA_RESET_VALUE = 0x0;

    static constexpr uint32_t PUCRB_PU15 = 0x8000;        // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU14 = 0x4000;        // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU13 = 0x2000;        // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU12 = 0x1000;        // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU11 = 0x800;         // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU10 = 0x400;         // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU9 = 0x200;          // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU8 = 0x100;          // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU7 = 0x80;           // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU6 = 0x40;           // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU5 = 0x20;           // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU4 = 0x10;           // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU3 = 0x8;            // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU2 = 0x4;            // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU1 = 0x2;            // Port B pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRB_PU0 = 0x1;            // Port B pull-up bit y (y=0..15)
    static const uint32_t PUCRB_RESET_VALUE = 0x0;

    static constexpr uint32_t PDCRB_PD15 = 0x8000;        // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD14 = 0x4000;        // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD13 = 0x2000;        // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD12 = 0x1000;        // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD11 = 0x800;         // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD10 = 0x400;         // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD9 = 0x200;          // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD8 = 0x100;          // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD7 = 0x80;           // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD6 = 0x40;           // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD5 = 0x20;           // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD4 = 0x10;           // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD3 = 0x8;            // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD2 = 0x4;            // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD1 = 0x2;            // Port B pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRB_PD0 = 0x1;            // Port B pull-down bit y (y=0..15)
    static const uint32_t PDCRB_RESET_VALUE = 0x0;

    static constexpr uint32_t PUCRC_PU15 = 0x8000;        // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU14 = 0x4000;        // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU13 = 0x2000;        // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU12 = 0x1000;        // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU11 = 0x800;         // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU10 = 0x400;         // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU9 = 0x200;          // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU8 = 0x100;          // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU7 = 0x80;           // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU6 = 0x40;           // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU5 = 0x20;           // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU4 = 0x10;           // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU3 = 0x8;            // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU2 = 0x4;            // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU1 = 0x2;            // Port C pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRC_PU0 = 0x1;            // Port C pull-up bit y (y=0..15)
    static const uint32_t PUCRC_RESET_VALUE = 0x0;

    static constexpr uint32_t PDCRC_PD15 = 0x8000;        // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD14 = 0x4000;        // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD13 = 0x2000;        // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD12 = 0x1000;        // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD11 = 0x800;         // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD10 = 0x400;         // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD9 = 0x200;          // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD8 = 0x100;          // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD7 = 0x80;           // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD6 = 0x40;           // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD5 = 0x20;           // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD4 = 0x10;           // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD3 = 0x8;            // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD2 = 0x4;            // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD1 = 0x2;            // Port C pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRC_PD0 = 0x1;            // Port C pull-down bit y (y=0..15)
    static const uint32_t PDCRC_RESET_VALUE = 0x0;

    static constexpr uint32_t PUCRD_PU9 = 0x200;          // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU8 = 0x100;          // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU6 = 0x40;           // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU5 = 0x20;           // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU4 = 0x10;           // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU3 = 0x8;            // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU2 = 0x4;            // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU1 = 0x2;            // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU0 = 0x1;            // Port D pull-up bit y (y=0..15)
    static const uint32_t PUCRD_RESET_VALUE = 0x0;

    static constexpr uint32_t PDCRD_PD9 = 0x200;          // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD8 = 0x100;          // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD6 = 0x40;           // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD5 = 0x20;           // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD4 = 0x10;           // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD3 = 0x8;            // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD2 = 0x4;            // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD1 = 0x2;            // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD0 = 0x1;            // Port D pull-down bit y (y=0..15)
    static const uint32_t PDCRD_RESET_VALUE = 0x0;

    static constexpr uint32_t PUCRF_PU2 = 0x4;            // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU1 = 0x2;            // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU0 = 0x1;            // Port F pull-up bit y (y=0..15)
    static const uint32_t PUCRF_RESET_VALUE = 0x0;

    static constexpr uint32_t PDCRF_PD2 = 0x4;            // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD1 = 0x2;            // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD0 = 0x1;            // Port F pull-down bit y (y=0..15)
    static const uint32_t PDCRF_RESET_VALUE = 0x0;

    static constexpr uint8_t PVD = 1; // Power voltage detector interrupt
};

static pwr_t& PWR = *reinterpret_cast<pwr_t*>(0x40007000);

#define HAVE_PERIPHERAL_PWR


////
//
//    DMA controller
//
////

struct dma_t
{
    volatile uint32_t    ISR;                  // [Read-only] low interrupt status register
    volatile uint32_t    IFCR;                 // [Read-only] high interrupt status register
    volatile uint32_t    CCR1;                 // [Read-write] DMA channel x configuration register
    volatile uint32_t    CNDTR1;               // [Read-write] DMA channel x number of data register
    volatile uint32_t    CPAR1;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR1;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _0;
    volatile uint32_t    CCR2;                 // [Read-write] DMA channel x configuration register
    volatile uint32_t    CNDTR2;               // [Read-write] DMA channel x number of data register
    volatile uint32_t    CPAR2;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR2;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _1;
    volatile uint32_t    CCR3;                 // [Read-write] DMA channel x configuration register
    volatile uint32_t    CNDTR3;               // [Read-write] DMA channel x configuration register
    volatile uint32_t    CPAR3;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR3;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _2;
    volatile uint32_t    CCR4;                 // [Read-write] DMA channel x configuration register
    volatile uint32_t    CNDTR4;               // [Read-write] DMA channel x configuration register
    volatile uint32_t    CPAR4;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR4;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _3;
    volatile uint32_t    CCR5;                 // [Read-write] DMA channel x configuration register
    volatile uint32_t    CNDTR5;               // [Read-write] DMA channel x configuration register
    volatile uint32_t    CPAR5;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR5;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _4;
    volatile uint32_t    CCR6;                 // [Read-write] DMA channel x configuration register
    volatile uint32_t    CNDTR6;               // [Read-write] DMA channel x configuration register
    volatile uint32_t    CPAR6;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR6;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _5;
    volatile uint32_t    CCR7;                 // [Read-write] DMA channel x configuration register
    volatile uint32_t    CNDTR7;               // [Read-write] DMA channel x configuration register
    volatile uint32_t    CPAR7;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR7;                // [Read-write] DMA channel x memory address register

    static constexpr uint32_t ISR_GIF0 = 0x1;           // Channel global interrupt flag
    static constexpr uint32_t ISR_TCIF1 = 0x2;          // Channel transfer complete flag
    static constexpr uint32_t ISR_HTIF2 = 0x4;          // Channel half transfer flag
    static constexpr uint32_t ISR_TEIF3 = 0x8;          // Channel transfer error flag
    static constexpr uint32_t ISR_GIF4 = 0x10;          // Channel global interrupt flag
    static constexpr uint32_t ISR_TCIF5 = 0x20;         // Channel transfer complete flag
    static constexpr uint32_t ISR_HTIF6 = 0x40;         // Channel half transfer flag
    static constexpr uint32_t ISR_TEIF7 = 0x80;         // Channel transfer error flag
    static constexpr uint32_t ISR_GIF8 = 0x100;         // Channel global interrupt flag
    static constexpr uint32_t ISR_TCIF9 = 0x200;        // Channel transfer complete flag
    static constexpr uint32_t ISR_HTIF10 = 0x400;       // Channel half transfer flag
    static constexpr uint32_t ISR_TEIF11 = 0x800;       // Channel transfer error flag
    static constexpr uint32_t ISR_GIF12 = 0x1000;       // Channel global interrupt flag
    static constexpr uint32_t ISR_TCIF13 = 0x2000;      // Channel transfer complete flag
    static constexpr uint32_t ISR_HTIF14 = 0x4000;      // Channel half transfer flag
    static constexpr uint32_t ISR_TEIF15 = 0x8000;      // Channel transfer error flag
    static constexpr uint32_t ISR_GIF16 = 0x10000;      // Channel global interrupt flag
    static constexpr uint32_t ISR_TCIF17 = 0x20000;     // Channel transfer complete flag
    static constexpr uint32_t ISR_HTIF18 = 0x40000;     // Channel half transfer flag
    static constexpr uint32_t ISR_TEIF19 = 0x80000;     // Channel transfer error flag
    static constexpr uint32_t ISR_GIF20 = 0x100000;     // Channel global interrupt flag
    static constexpr uint32_t ISR_TCIF21 = 0x200000;    // Channel transfer complete flag
    static constexpr uint32_t ISR_HTIF22 = 0x400000;    // Channel half transfer flag
    static constexpr uint32_t ISR_TEIF23 = 0x800000;    // Channel transfer error flag
    static constexpr uint32_t ISR_GIF24 = 0x1000000;    // Channel global interrupt flag
    static constexpr uint32_t ISR_TCIF25 = 0x2000000;   // Channel transfer complete flag
    static constexpr uint32_t ISR_HTIF26 = 0x4000000;   // Channel half transfer flag
    static constexpr uint32_t ISR_TEIF27 = 0x8000000;   // Channel transfer error flag
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t IFCR_CGIF0 = 0x1;          // Channel global interrupt flag
    static constexpr uint32_t IFCR_CTCIF1 = 0x2;         // Channel transfer complete flag
    static constexpr uint32_t IFCR_CHTIF2 = 0x4;         // Channel half transfer flag
    static constexpr uint32_t IFCR_CTEIF3 = 0x8;         // Channel transfer error flag
    static constexpr uint32_t IFCR_CGIF4 = 0x10;         // Channel global interrupt flag
    static constexpr uint32_t IFCR_CTCIF5 = 0x20;        // Channel transfer complete flag
    static constexpr uint32_t IFCR_CHTIF6 = 0x40;        // Channel half transfer flag
    static constexpr uint32_t IFCR_CTEIF7 = 0x80;        // Channel transfer error flag
    static constexpr uint32_t IFCR_CGIF8 = 0x100;        // Channel global interrupt flag
    static constexpr uint32_t IFCR_CTCIF9 = 0x200;       // Channel transfer complete flag
    static constexpr uint32_t IFCR_CHTIF10 = 0x400;      // Channel half transfer flag
    static constexpr uint32_t IFCR_CTEIF11 = 0x800;      // Channel transfer error flag
    static constexpr uint32_t IFCR_CGIF12 = 0x1000;      // Channel global interrupt flag
    static constexpr uint32_t IFCR_CTCIF13 = 0x2000;     // Channel transfer complete flag
    static constexpr uint32_t IFCR_CHTIF14 = 0x4000;     // Channel half transfer flag
    static constexpr uint32_t IFCR_CTEIF15 = 0x8000;     // Channel transfer error flag
    static constexpr uint32_t IFCR_CGIF16 = 0x10000;     // Channel global interrupt flag
    static constexpr uint32_t IFCR_CTCIF17 = 0x20000;    // Channel transfer complete flag
    static constexpr uint32_t IFCR_CHTIF18 = 0x40000;    // Channel half transfer flag
    static constexpr uint32_t IFCR_CTEIF19 = 0x80000;    // Channel transfer error flag
    static constexpr uint32_t IFCR_CGIF20 = 0x100000;    // Channel global interrupt flag
    static constexpr uint32_t IFCR_CTCIF21 = 0x200000;   // Channel transfer complete flag
    static constexpr uint32_t IFCR_CHTIF22 = 0x400000;   // Channel half transfer flag
    static constexpr uint32_t IFCR_CTEIF23 = 0x800000;   // Channel transfer error flag
    static constexpr uint32_t IFCR_CGIF24 = 0x1000000;   // Channel global interrupt flag
    static constexpr uint32_t IFCR_CTCIF25 = 0x2000000;  // Channel transfer complete flag
    static constexpr uint32_t IFCR_CHTIF26 = 0x4000000;  // Channel half transfer flag
    static constexpr uint32_t IFCR_CTEIF27 = 0x8000000;  // Channel transfer error flag
    static const uint32_t IFCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR1_EN = 0x1;             // Channel enable
    static constexpr uint32_t CCR1_TCIE = 0x2;           // Transfer complete interrupt enable
    static constexpr uint32_t CCR1_HTIE = 0x4;           // Half transfer interrupt enable
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
    static constexpr uint32_t CCR1_PL =                  // Channel priority level (2 bits)
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
    static constexpr uint32_t CCR2_HTIE = 0x4;           // Half transfer interrupt enable
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
    static constexpr uint32_t CCR2_PL =                  // Channel priority level (2 bits)
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
    static constexpr uint32_t CCR3_HTIE = 0x4;           // Half transfer interrupt enable
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
    static constexpr uint32_t CCR3_PL =                  // Channel priority level (2 bits)
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
    static constexpr uint32_t CCR4_HTIE = 0x4;           // Half transfer interrupt enable
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
    static constexpr uint32_t CCR4_PL =                  // Channel priority level (2 bits)
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
    static constexpr uint32_t CCR5_HTIE = 0x4;           // Half transfer interrupt enable
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
    static constexpr uint32_t CCR5_PL =                  // Channel priority level (2 bits)
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
    static constexpr uint32_t CCR6_HTIE = 0x4;           // Half transfer interrupt enable
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
    static constexpr uint32_t CCR6_PL =                  // Channel priority level (2 bits)
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
    static constexpr uint32_t CCR7_HTIE = 0x4;           // Half transfer interrupt enable
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
    static constexpr uint32_t CCR7_PL =                  // Channel priority level (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR7_MEM2MEM = 0x4000;     // Memory to memory mode
    static const uint32_t CCR7_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR7_NDT =                 // Number of data to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR7_RESET_VALUE = 0x0;


    static const uint32_t CPAR7_RESET_VALUE = 0x0;


    static const uint32_t CMAR7_RESET_VALUE = 0x0;

    static constexpr uint8_t DMA_CHANNEL1 = 9; // DMA channel 1 interrupt
    static constexpr uint8_t DMA_CHANNEL2_3 = 10; // DMA channel 2 &amp; 3 interrupts
};

static dma_t& DMA = *reinterpret_cast<dma_t*>(0x40020000);

#define HAVE_PERIPHERAL_DMA


////
//
//    DMAMUX
//
////

struct dmamux_t
{
    volatile uint32_t    C0CR;                 // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C1CR;                 // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C2CR;                 // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C3CR;                 // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C4CR;                 // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C5CR;                 // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C6CR;                 // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    reserved_t<25>       _0;
    volatile uint32_t    CSR;                  // [Read-only] DMAMUX request line multiplexer interrupt channel status register
    volatile uint32_t    CFR;                  // [Write-only] DMAMUX request line multiplexer interrupt clear flag register
    reserved_t<30>       _1;
    volatile uint32_t    RG0CR;                // [Read-write] DMAMux - DMA request generator channel x control register
    volatile uint32_t    RG1CR;                // [Read-write] DMAMux - DMA request generator channel x control register
    volatile uint32_t    RG2CR;                // [Read-write] DMAMux - DMA request generator channel x control register
    volatile uint32_t    RG3CR;                // [Read-write] DMAMux - DMA request generator channel x control register
    reserved_t<12>       _2;
    volatile uint32_t    RGSR;                 // [Read-only] DMAMux - DMA request generator status register
    volatile uint32_t    RGCFR;                // [Write-only] DMAMux - DMA request generator clear flag register
    reserved_t<169>      _3;
    volatile uint32_t    HWCFGR2;              // [Read-only] DMAMUX hardware configuration 2 register
    volatile uint32_t    HWCFGR1;              // [Read-only] DMAMUX hardware configuration 1 register
    volatile uint32_t    VERR;                 // [Read-only] DMAMUX version register
    volatile uint32_t    IPIDR;                // [Read-only] DMAMUX IP identification register
    volatile uint32_t    SIDR;                 // [Read-only] DMAMUX size identification register

    template<uint32_t X>
    static constexpr uint32_t C0CR_DMAREQ_ID =           // Input DMA request line selected (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t C0CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C0CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C0CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C0CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C0CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C0CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C0CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C1CR_DMAREQ_ID =           // Input DMA request line selected (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t C1CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C1CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C1CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C1CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C1CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C1CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C1CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C2CR_DMAREQ_ID =           // Input DMA request line selected (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t C2CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C2CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C2CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C2CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C2CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C2CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C2CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C3CR_DMAREQ_ID =           // Input DMA request line selected (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t C3CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C3CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C3CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C3CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C3CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C3CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C3CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C4CR_DMAREQ_ID =           // Input DMA request line selected (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t C4CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C4CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C4CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C4CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C4CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C4CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C4CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C5CR_DMAREQ_ID =           // Input DMA request line selected (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t C5CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C5CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C5CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C5CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C5CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C5CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C5CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C6CR_DMAREQ_ID =           // Input DMA request line selected (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t C6CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C6CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C6CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C6CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C6CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C6CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C6CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CSR_SOF =                 // Synchronization overrun event flag (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t CSR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CFR_CSOF =                // Clear synchronization overrun event flag (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t CFR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RG0CR_SIG_ID =              // DMA request trigger input selected (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static constexpr uint32_t RG0CR_OIE = 0x100;          // Interrupt enable at trigger event overrun
    static constexpr uint32_t RG0CR_GE = 0x10000;         // DMA request generator channel enable/disable
    template<uint32_t X>
    static constexpr uint32_t RG0CR_GPOL =                // DMA request generator trigger event type selection Defines the trigger event on the selected DMA request trigger input (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RG0CR_GNBREQ =              // Number of DMA requests to generate Defines the number of DMA requests generated after a trigger event, then stop generating. The actual number of generated DMA requests is GNBREQ+1. Note: This field can only be written when GE bit is reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    static const uint32_t RG0CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RG1CR_SIG_ID =              // DMA request trigger input selected (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static constexpr uint32_t RG1CR_OIE = 0x100;          // Interrupt enable at trigger event overrun
    static constexpr uint32_t RG1CR_GE = 0x10000;         // DMA request generator channel enable/disable
    template<uint32_t X>
    static constexpr uint32_t RG1CR_GPOL =                // DMA request generator trigger event type selection Defines the trigger event on the selected DMA request trigger input (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RG1CR_GNBREQ =              // Number of DMA requests to generate Defines the number of DMA requests generated after a trigger event, then stop generating. The actual number of generated DMA requests is GNBREQ+1. Note: This field can only be written when GE bit is reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    static const uint32_t RG1CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RG2CR_SIG_ID =              // DMA request trigger input selected (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static constexpr uint32_t RG2CR_OIE = 0x100;          // Interrupt enable at trigger event overrun
    static constexpr uint32_t RG2CR_GE = 0x10000;         // DMA request generator channel enable/disable
    template<uint32_t X>
    static constexpr uint32_t RG2CR_GPOL =                // DMA request generator trigger event type selection Defines the trigger event on the selected DMA request trigger input (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RG2CR_GNBREQ =              // Number of DMA requests to generate Defines the number of DMA requests generated after a trigger event, then stop generating. The actual number of generated DMA requests is GNBREQ+1. Note: This field can only be written when GE bit is reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    static const uint32_t RG2CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RG3CR_SIG_ID =              // DMA request trigger input selected (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static constexpr uint32_t RG3CR_OIE = 0x100;          // Interrupt enable at trigger event overrun
    static constexpr uint32_t RG3CR_GE = 0x10000;         // DMA request generator channel enable/disable
    template<uint32_t X>
    static constexpr uint32_t RG3CR_GPOL =                // DMA request generator trigger event type selection Defines the trigger event on the selected DMA request trigger input (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RG3CR_GNBREQ =              // Number of DMA requests to generate Defines the number of DMA requests generated after a trigger event, then stop generating. The actual number of generated DMA requests is GNBREQ+1. Note: This field can only be written when GE bit is reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    static const uint32_t RG3CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RGSR_OF =                  // Trigger event overrun flag The flag is set when a trigger event occurs on DMA request generator channel x, while the DMA request generator counter value is lower than GNBREQ. The flag is cleared by writing 1 to the corresponding COFx bit in DMAMUX_RGCFR register. (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t RGSR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RGCFR_COF =                 // Clear trigger event overrun flag Upon setting, this bit clears the corresponding overrun flag OFx in the DMAMUX_RGCSR register. (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t RGCFR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR2_NUM_DMA_EXT_REQ =     // Number of DMA request trigger inputs (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t HWCFGR2_RESET_VALUE = 0x17;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_NUM_DMA_STREAMS =     // number of DMA request line multiplexer (output) channels (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_NUM_DMA_PERIPH_REQ =  // number of DMA request lines from peripherals (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_NUM_DMA_TRIG =        // number of synchronization inputs (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_NUM_DMA_REQGEN =      // number of DMA request generator channels (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t HWCFGR1_RESET_VALUE = 0x4173907;

    template<uint32_t X>
    static constexpr uint32_t VERR_MINREV =              // Minor IP revision (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t VERR_MAJREV =              // Major IP revision (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    static const uint32_t VERR_RESET_VALUE = 0x11;


    static const uint32_t IPIDR_RESET_VALUE = 0x100011;


    static const uint32_t SIDR_RESET_VALUE = 0xa3c5dd01;

    static constexpr uint8_t DMA_CHANNEL4_5_6_7 = 11; // DMA channel 4, 5, 6 &amp; 7 and DMAMUX
};

static dmamux_t& DMAMUX = *reinterpret_cast<dmamux_t*>(0x40020800);

#define HAVE_PERIPHERAL_DMAMUX


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
    volatile uint32_t    BRR;                  // [Write-only] port bit reset register

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
    static const uint32_t MODER_RESET_VALUE = 0xebffffff;

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
    static const uint32_t OSPEEDR_RESET_VALUE = 0xc000000;

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
    static constexpr uint32_t AFRL_AFSEL7 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL6 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL5 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL4 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL3 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL2 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL1 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL0 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL15 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL14 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL13 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL12 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL11 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL10 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL9 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL8 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Port Reset bit
    static constexpr uint32_t BRR_BR1 = 0x2;            // Port Reset bit
    static constexpr uint32_t BRR_BR2 = 0x4;            // Port Reset bit
    static constexpr uint32_t BRR_BR3 = 0x8;            // Port Reset bit
    static constexpr uint32_t BRR_BR4 = 0x10;           // Port Reset bit
    static constexpr uint32_t BRR_BR5 = 0x20;           // Port Reset bit
    static constexpr uint32_t BRR_BR6 = 0x40;           // Port Reset bit
    static constexpr uint32_t BRR_BR7 = 0x80;           // Port Reset bit
    static constexpr uint32_t BRR_BR8 = 0x100;          // Port Reset bit
    static constexpr uint32_t BRR_BR9 = 0x200;          // Port Reset bit
    static constexpr uint32_t BRR_BR10 = 0x400;         // Port Reset bit
    static constexpr uint32_t BRR_BR11 = 0x800;         // Port Reset bit
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Port Reset bit
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Port Reset bit
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Port Reset bit
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Port Reset bit
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpioa_t& GPIOA = *reinterpret_cast<gpioa_t*>(0x50000000);

#define HAVE_PERIPHERAL_GPIOA


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
    volatile uint32_t    BRR;                  // [Write-only] port bit reset register

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
    static const uint32_t MODER_RESET_VALUE = 0xffffffff;

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
    static constexpr uint32_t AFRL_AFSEL7 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL6 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL5 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL4 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL3 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL2 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL1 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL0 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL15 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL14 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL13 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL12 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL11 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL10 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL9 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL8 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Port Reset bit
    static constexpr uint32_t BRR_BR1 = 0x2;            // Port Reset bit
    static constexpr uint32_t BRR_BR2 = 0x4;            // Port Reset bit
    static constexpr uint32_t BRR_BR3 = 0x8;            // Port Reset bit
    static constexpr uint32_t BRR_BR4 = 0x10;           // Port Reset bit
    static constexpr uint32_t BRR_BR5 = 0x20;           // Port Reset bit
    static constexpr uint32_t BRR_BR6 = 0x40;           // Port Reset bit
    static constexpr uint32_t BRR_BR7 = 0x80;           // Port Reset bit
    static constexpr uint32_t BRR_BR8 = 0x100;          // Port Reset bit
    static constexpr uint32_t BRR_BR9 = 0x200;          // Port Reset bit
    static constexpr uint32_t BRR_BR10 = 0x400;         // Port Reset bit
    static constexpr uint32_t BRR_BR11 = 0x800;         // Port Reset bit
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Port Reset bit
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Port Reset bit
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Port Reset bit
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Port Reset bit
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpiob_t& GPIOB = *reinterpret_cast<gpiob_t*>(0x50000400);

#define HAVE_PERIPHERAL_GPIOB


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
    volatile uint32_t    BRR;                  // [Write-only] port bit reset register

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
    static const uint32_t MODER_RESET_VALUE = 0xffffffff;

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
    static constexpr uint32_t AFRL_AFSEL7 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL6 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL5 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL4 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL3 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL2 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL1 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL0 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL15 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL14 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL13 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL12 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL11 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL10 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL9 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL8 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Port Reset bit
    static constexpr uint32_t BRR_BR1 = 0x2;            // Port Reset bit
    static constexpr uint32_t BRR_BR2 = 0x4;            // Port Reset bit
    static constexpr uint32_t BRR_BR3 = 0x8;            // Port Reset bit
    static constexpr uint32_t BRR_BR4 = 0x10;           // Port Reset bit
    static constexpr uint32_t BRR_BR5 = 0x20;           // Port Reset bit
    static constexpr uint32_t BRR_BR6 = 0x40;           // Port Reset bit
    static constexpr uint32_t BRR_BR7 = 0x80;           // Port Reset bit
    static constexpr uint32_t BRR_BR8 = 0x100;          // Port Reset bit
    static constexpr uint32_t BRR_BR9 = 0x200;          // Port Reset bit
    static constexpr uint32_t BRR_BR10 = 0x400;         // Port Reset bit
    static constexpr uint32_t BRR_BR11 = 0x800;         // Port Reset bit
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Port Reset bit
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Port Reset bit
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Port Reset bit
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Port Reset bit
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpioc_t& GPIOC = *reinterpret_cast<gpioc_t*>(0x50000800);

#define HAVE_PERIPHERAL_GPIOC


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
    volatile uint32_t    BRR;                  // [Write-only] port bit reset register

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
    static const uint32_t MODER_RESET_VALUE = 0xffffffff;

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
    static constexpr uint32_t AFRL_AFSEL7 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL6 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL5 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL4 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL3 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL2 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL1 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL0 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL15 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL14 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL13 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL12 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL11 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL10 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL9 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL8 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Port Reset bit
    static constexpr uint32_t BRR_BR1 = 0x2;            // Port Reset bit
    static constexpr uint32_t BRR_BR2 = 0x4;            // Port Reset bit
    static constexpr uint32_t BRR_BR3 = 0x8;            // Port Reset bit
    static constexpr uint32_t BRR_BR4 = 0x10;           // Port Reset bit
    static constexpr uint32_t BRR_BR5 = 0x20;           // Port Reset bit
    static constexpr uint32_t BRR_BR6 = 0x40;           // Port Reset bit
    static constexpr uint32_t BRR_BR7 = 0x80;           // Port Reset bit
    static constexpr uint32_t BRR_BR8 = 0x100;          // Port Reset bit
    static constexpr uint32_t BRR_BR9 = 0x200;          // Port Reset bit
    static constexpr uint32_t BRR_BR10 = 0x400;         // Port Reset bit
    static constexpr uint32_t BRR_BR11 = 0x800;         // Port Reset bit
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Port Reset bit
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Port Reset bit
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Port Reset bit
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Port Reset bit
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpiod_t& GPIOD = *reinterpret_cast<gpiod_t*>(0x50000c00);

#define HAVE_PERIPHERAL_GPIOD


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
    volatile uint32_t    BRR;                  // [Write-only] port bit reset register

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
    static const uint32_t MODER_RESET_VALUE = 0xffffffff;

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
    static constexpr uint32_t AFRL_AFSEL7 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL6 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL5 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL4 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL3 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL2 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL1 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRL_AFSEL0 =              // Alternate function selection for port x bit y (y = 0..7) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL15 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL14 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL13 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL12 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL11 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL10 =             // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL9 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRH_AFSEL8 =              // Alternate function selection for port x bit y (y = 8..15) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t AFRH_RESET_VALUE = 0x0;

    static constexpr uint32_t BRR_BR0 = 0x1;            // Port Reset bit
    static constexpr uint32_t BRR_BR1 = 0x2;            // Port Reset bit
    static constexpr uint32_t BRR_BR2 = 0x4;            // Port Reset bit
    static constexpr uint32_t BRR_BR3 = 0x8;            // Port Reset bit
    static constexpr uint32_t BRR_BR4 = 0x10;           // Port Reset bit
    static constexpr uint32_t BRR_BR5 = 0x20;           // Port Reset bit
    static constexpr uint32_t BRR_BR6 = 0x40;           // Port Reset bit
    static constexpr uint32_t BRR_BR7 = 0x80;           // Port Reset bit
    static constexpr uint32_t BRR_BR8 = 0x100;          // Port Reset bit
    static constexpr uint32_t BRR_BR9 = 0x200;          // Port Reset bit
    static constexpr uint32_t BRR_BR10 = 0x400;         // Port Reset bit
    static constexpr uint32_t BRR_BR11 = 0x800;         // Port Reset bit
    static constexpr uint32_t BRR_BR12 = 0x1000;        // Port Reset bit
    static constexpr uint32_t BRR_BR13 = 0x2000;        // Port Reset bit
    static constexpr uint32_t BRR_BR14 = 0x4000;        // Port Reset bit
    static constexpr uint32_t BRR_BR15 = 0x8000;        // Port Reset bit
    static const uint32_t BRR_RESET_VALUE = 0x0;
};

static gpiof_t& GPIOF = *reinterpret_cast<gpiof_t*>(0x50001400);

#define HAVE_PERIPHERAL_GPIOF


////
//
//    Advanced encryption standard hardware accelerator 1
//
////

struct aes_t
{
    volatile uint32_t    CR;                   // [Read-write] control register
    volatile uint32_t    SR;                   // [Read-only] status register
    volatile uint32_t    DINR;                 // [Read-write] data input register
    volatile uint32_t    DOUTR;                // [Read-only] data output register
    volatile uint32_t    KEYR0;                // [Read-write] key register 0
    volatile uint32_t    KEYR1;                // [Read-write] key register 1
    volatile uint32_t    KEYR2;                // [Read-write] key register 2
    volatile uint32_t    KEYR3;                // [Read-write] key register 3
    volatile uint32_t    IVR0;                 // [Read-write] initialization vector register 0
    volatile uint32_t    IVR1;                 // [Read-write] initialization vector register 1
    volatile uint32_t    IVR2;                 // [Read-write] initialization vector register 2
    volatile uint32_t    IVR3;                 // [Read-write] initialization vector register 3
    volatile uint32_t    KEYR4;                // [Read-write] key register 4
    volatile uint32_t    KEYR5;                // [Read-write] key register 5
    volatile uint32_t    KEYR6;                // [Read-write] key register 6
    volatile uint32_t    KEYR7;                // [Read-write] key register 7
    volatile uint32_t    SUSP0R;               // [Read-write] AES suspend register 0
    volatile uint32_t    SUSP1R;               // [Read-write] AES suspend register 1
    volatile uint32_t    SUSP2R;               // [Read-write] AES suspend register 2
    volatile uint32_t    SUSP3R;               // [Read-write] AES suspend register 3
    volatile uint32_t    SUSP4R;               // [Read-write] AES suspend register 4
    volatile uint32_t    SUSP5R;               // [Read-write] AES suspend register 5
    volatile uint32_t    SUSP6R;               // [Read-write] AES suspend register 6
    volatile uint32_t    SUSP7R;               // [Read-write] AES suspend register 7
    reserved_t<228>      _0;
    volatile uint32_t    HWCFR;                // [Read-only] AES hardware configuration register
    volatile uint32_t    VERR;                 // [Read-only] AES version register
    volatile uint32_t    IPIDR;                // [Read-only] AES identification register
    volatile uint32_t    SIDR;                 // [Read-only] AES size ID register

    template<uint32_t X>
    static constexpr uint32_t CR_NPBLB =               // Number of padding bytes in last block of payload (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    static constexpr uint32_t CR_KEYSIZE = 0x40000;    // Key size selection
    static constexpr uint32_t CR_CHMOD2 = 0x10000;     // AES chaining mode Bit2
    template<uint32_t X>
    static constexpr uint32_t CR_GCMPH =               // Used only for GCM, CCM and GMAC algorithms and has no effect when other algorithms are selected (2 bits)
        bit_field_t<13, 0x3>::value<X>();
    static constexpr uint32_t CR_DMAOUTEN = 0x1000;    // Enable DMA management of data output phase
    static constexpr uint32_t CR_DMAINEN = 0x800;      // Enable DMA management of data input phase
    static constexpr uint32_t CR_ERRIE = 0x400;        // Error interrupt enable
    static constexpr uint32_t CR_CCFIE = 0x200;        // CCF flag interrupt enable
    static constexpr uint32_t CR_ERRC = 0x100;         // Error clear
    static constexpr uint32_t CR_CCFC = 0x80;          // Computation Complete Flag Clear
    template<uint32_t X>
    static constexpr uint32_t CR_CHMOD10 =             // AES chaining mode Bit1 Bit0 (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_MODE =                // AES operating mode (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_DATATYPE =            // Data type selection (for data in and data out to/from the cryptographic block) (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t CR_EN = 0x1;             // AES enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_BUSY = 0x8;           // Busy flag
    static constexpr uint32_t SR_WRERR = 0x4;          // Write error flag
    static constexpr uint32_t SR_RDERR = 0x2;          // Read error flag
    static constexpr uint32_t SR_CCF = 0x1;            // Computation complete flag
    static const uint32_t SR_RESET_VALUE = 0x0;


    static const uint32_t DINR_RESET_VALUE = 0x0;


    static const uint32_t DOUTR_RESET_VALUE = 0x0;


    static const uint32_t KEYR0_RESET_VALUE = 0x0;


    static const uint32_t KEYR1_RESET_VALUE = 0x0;


    static const uint32_t KEYR2_RESET_VALUE = 0x0;


    static const uint32_t KEYR3_RESET_VALUE = 0x0;


    static const uint32_t IVR0_RESET_VALUE = 0x0;


    static const uint32_t IVR1_RESET_VALUE = 0x0;


    static const uint32_t IVR2_RESET_VALUE = 0x0;


    static const uint32_t IVR3_RESET_VALUE = 0x0;


    static const uint32_t KEYR4_RESET_VALUE = 0x0;


    static const uint32_t KEYR5_RESET_VALUE = 0x0;


    static const uint32_t KEYR6_RESET_VALUE = 0x0;


    static const uint32_t KEYR7_RESET_VALUE = 0x0;


    static const uint32_t SUSP0R_RESET_VALUE = 0x0;


    static const uint32_t SUSP1R_RESET_VALUE = 0x0;


    static const uint32_t SUSP2R_RESET_VALUE = 0x0;


    static const uint32_t SUSP3R_RESET_VALUE = 0x0;


    static const uint32_t SUSP4R_RESET_VALUE = 0x0;


    static const uint32_t SUSP5R_RESET_VALUE = 0x0;


    static const uint32_t SUSP6R_RESET_VALUE = 0x0;


    static const uint32_t SUSP7R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HWCFR_CFG4 =                // HW Generic 4 (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFR_CFG3 =                // HW Generic 3 (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFR_CFG2 =                // HW Generic 2 (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFR_CFG1 =                // HW Generic 1 (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t HWCFR_RESET_VALUE = 0x2;

    template<uint32_t X>
    static constexpr uint32_t VERR_MAJREV =              // Major revision (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t VERR_MINREV =              // Minor revision (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t VERR_RESET_VALUE = 0x10;


    static const uint32_t IPIDR_RESET_VALUE = 0x170023;


    static const uint32_t SIDR_RESET_VALUE = 0x170023;

    static constexpr uint8_t AES_RNG = 31; // AES and RNG global interrupts
};

static aes_t& AES = *reinterpret_cast<aes_t*>(0x40026000);

#define HAVE_PERIPHERAL_AES


////
//
//    Random number generator
//
////

struct rng_t
{
    volatile uint32_t    CR;                   // [Read-write] control register
    volatile uint32_t    SR;                   // status register
    volatile uint32_t    DR;                   // [Read-only] data register

    static constexpr uint32_t CR_RNGEN = 0x4;          // Random number generator enable
    static constexpr uint32_t CR_IE = 0x8;             // Interrupt enable
    static constexpr uint32_t CR_CED = 0x20;           // Clock error detection
    static constexpr uint32_t CR_BYP = 0x40;           // Bypass mode enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_SEIS = 0x40;          // Seed error interrupt status, Read-write
    static constexpr uint32_t SR_CEIS = 0x20;          // Clock error interrupt status, Read-write
    static constexpr uint32_t SR_SECS = 0x4;           // Seed error current status, Read-only
    static constexpr uint32_t SR_CECS = 0x2;           // Clock error current status, Read-only
    static constexpr uint32_t SR_DRDY = 0x1;           // Data ready, Read-only
    static const uint32_t SR_RESET_VALUE = 0x0;


    static const uint32_t DR_RESET_VALUE = 0x0;
};

static rng_t& RNG = *reinterpret_cast<rng_t*>(0x40025000);

#define HAVE_PERIPHERAL_RNG


////
//
//    Cyclic redundancy check calculation unit
//
////

struct crc_t
{
    volatile uint32_t    DR;                   // [Read-write] Data register
    volatile uint32_t    IDR;                  // [Read-write] Independent data register
    volatile uint32_t    CR;                   // Control register
    reserved_t<1>        _0;
    volatile uint32_t    INIT;                 // [Read-write] Initial CRC value
    volatile uint32_t    POL;                  // [Read-write] polynomial


    static const uint32_t DR_RESET_VALUE = 0xffffffff;


    static const uint32_t IDR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_REV_OUT = 0x80;       // Reverse output data, Read-write
    template<uint32_t X>
    static constexpr uint32_t CR_REV_IN =              // Reverse input data (2 bits), Read-write
        bit_field_t<5, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_POLYSIZE =            // Polynomial size (2 bits), Read-write
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t CR_RESET = 0x1;          // RESET bit, Write-only
    static const uint32_t CR_RESET_VALUE = 0x0;


    static const uint32_t INIT_RESET_VALUE = 0xffffffff;


    static const uint32_t POL_RESET_VALUE = 0x4c11db7;

    static constexpr uint8_t CEC = 30; // CEC global interrupt
};

static crc_t& CRC = *reinterpret_cast<crc_t*>(0x40023000);

#define HAVE_PERIPHERAL_CRC


////
//
//    External interrupt/event controller
//
////

struct exti_t
{
    volatile uint32_t    RTSR1;                // [Read-write] EXTI rising trigger selection register
    volatile uint32_t    FTSR1;                // [Read-write] EXTI falling trigger selection register
    volatile uint32_t    SWIER1;               // [Read-write] EXTI software interrupt event register
    volatile uint32_t    RPR1;                 // [Read-write] EXTI rising edge pending register
    volatile uint32_t    FPR1;                 // [Read-write] EXTI falling edge pending register
    reserved_t<19>       _0;
    volatile uint32_t    EXTICR1;              // [Read-write] EXTI external interrupt selection register
    volatile uint32_t    EXTICR2;              // [Read-write] EXTI external interrupt selection register
    volatile uint32_t    EXTICR3;              // [Read-write] EXTI external interrupt selection register
    volatile uint32_t    EXTICR4;              // [Read-write] EXTI external interrupt selection register
    reserved_t<4>        _1;
    volatile uint32_t    IMR1;                 // [Read-write] EXTI CPU wakeup with interrupt mask register
    reserved_t<3>        _2;
    volatile uint32_t    IMR2;                 // [Read-write] EXTI CPU wakeup with interrupt mask register
    volatile uint32_t    EMR2;                 // [Read-write] EXTI CPU wakeup with event mask register
    reserved_t<208>      _3;
    volatile uint32_t    HWCFGR7;              // [Read-write] Hardware configuration registers
    volatile uint32_t    HWCFGR6;              // [Read-write] Hardware configuration registers
    volatile uint32_t    HWCFGR5;              // [Read-write] Hardware configuration registers
    volatile uint32_t    HWCFGR4;              // [Read-write] Hardware configuration registers
    volatile uint32_t    HWCFGR3;              // [Read-write] Hardware configuration registers
    volatile uint32_t    HWCFGR2;              // [Read-write] Hardware configuration registers
    volatile uint32_t    HWCFGR1;              // [Read-only] Hardware configuration registers
    volatile uint32_t    VERR;                 // [Read-only] AES version register
    volatile uint32_t    IPIDR;                // [Read-only] AES identification register
    volatile uint32_t    SIDR;                 // [Read-only] AES size ID register

    static constexpr uint32_t RTSR1_TR0 = 0x1;            // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR1 = 0x2;            // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR2 = 0x4;            // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR3 = 0x8;            // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR4 = 0x10;           // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR5 = 0x20;           // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR6 = 0x40;           // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR7 = 0x80;           // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR8 = 0x100;          // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR9 = 0x200;          // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR10 = 0x400;         // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR11 = 0x800;         // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR12 = 0x1000;        // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR13 = 0x2000;        // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR14 = 0x4000;        // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR15 = 0x8000;        // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR16 = 0x10000;       // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR17 = 0x20000;       // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t RTSR1_TR18 = 0x40000;       // Rising trigger event configuration bit of Configurable Event input
    static const uint32_t RTSR1_RESET_VALUE = 0x0;

    static constexpr uint32_t FTSR1_TR0 = 0x1;            // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR1 = 0x2;            // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR2 = 0x4;            // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR3 = 0x8;            // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR4 = 0x10;           // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR5 = 0x20;           // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR6 = 0x40;           // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR7 = 0x80;           // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR8 = 0x100;          // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR9 = 0x200;          // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR10 = 0x400;         // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR11 = 0x800;         // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR12 = 0x1000;        // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR13 = 0x2000;        // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR14 = 0x4000;        // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR15 = 0x8000;        // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR16 = 0x10000;       // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR17 = 0x20000;       // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t FTSR1_TR18 = 0x40000;       // Rising trigger event configuration bit of Configurable Event input
    static const uint32_t FTSR1_RESET_VALUE = 0x0;

    static constexpr uint32_t SWIER1_SWIER0 = 0x1;         // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER1 = 0x2;         // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER2 = 0x4;         // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER3 = 0x8;         // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER4 = 0x10;        // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER5 = 0x20;        // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER6 = 0x40;        // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER7 = 0x80;        // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER8 = 0x100;       // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER9 = 0x200;       // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER10 = 0x400;      // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER11 = 0x800;      // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER12 = 0x1000;     // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER13 = 0x2000;     // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER14 = 0x4000;     // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER15 = 0x8000;     // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER16 = 0x10000;    // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER17 = 0x20000;    // Rising trigger event configuration bit of Configurable Event input
    static constexpr uint32_t SWIER1_SWIER18 = 0x40000;    // Rising trigger event configuration bit of Configurable Event input
    static const uint32_t SWIER1_RESET_VALUE = 0x0;

    static constexpr uint32_t RPR1_RPIF0 = 0x1;          // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF1 = 0x2;          // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF2 = 0x4;          // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF3 = 0x8;          // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF4 = 0x10;         // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF5 = 0x20;         // configurable event inputs x rising edge Pending bit
    static constexpr uint32_t RPR1_RPIF6 = 0x40;         // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF7 = 0x80;         // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF8 = 0x100;        // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF9 = 0x200;        // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF10 = 0x400;       // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF11 = 0x800;       // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF12 = 0x1000;      // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF13 = 0x2000;      // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF14 = 0x4000;      // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF15 = 0x8000;      // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF16 = 0x10000;     // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF17 = 0x20000;     // configurable event inputs x rising edge Pending bit.
    static constexpr uint32_t RPR1_RPIF18 = 0x40000;     // configurable event inputs x rising edge Pending bit.
    static const uint32_t RPR1_RESET_VALUE = 0x0;

    static constexpr uint32_t FPR1_FPIF0 = 0x1;          // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF1 = 0x2;          // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF2 = 0x4;          // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF3 = 0x8;          // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF4 = 0x10;         // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF5 = 0x20;         // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF6 = 0x40;         // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF7 = 0x80;         // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF8 = 0x100;        // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF9 = 0x200;        // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF10 = 0x400;       // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF11 = 0x800;       // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF12 = 0x1000;      // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF13 = 0x2000;      // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF14 = 0x4000;      // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF15 = 0x8000;      // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF16 = 0x10000;     // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF17 = 0x20000;     // configurable event inputs x falling edge pending bit.
    static constexpr uint32_t FPR1_FPIF18 = 0x40000;     // configurable event inputs x falling edge pending bit.
    static const uint32_t FPR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EXTICR1_EXTI0_7 =             // GPIO port selection (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR1_EXTI8_15 =            // GPIO port selection (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR1_EXTI16_23 =           // GPIO port selection (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR1_EXTI24_31 =           // GPIO port selection (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t EXTICR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EXTICR2_EXTI0_7 =             // GPIO port selection (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR2_EXTI8_15 =            // GPIO port selection (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR2_EXTI16_23 =           // GPIO port selection (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR2_EXTI24_31 =           // GPIO port selection (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t EXTICR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EXTICR3_EXTI0_7 =             // GPIO port selection (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR3_EXTI8_15 =            // GPIO port selection (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR3_EXTI16_23 =           // GPIO port selection (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR3_EXTI24_31 =           // GPIO port selection (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t EXTICR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EXTICR4_EXTI0_7 =             // GPIO port selection (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR4_EXTI8_15 =            // GPIO port selection (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR4_EXTI16_23 =           // GPIO port selection (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR4_EXTI24_31 =           // GPIO port selection (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t EXTICR4_RESET_VALUE = 0x0;

    static constexpr uint32_t IMR1_EM0 = 0x1;            // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM1 = 0x2;            // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM10 = 0x400;         // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM11 = 0x800;         // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM12 = 0x1000;        // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM13 = 0x2000;        // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM14 = 0x4000;        // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM15 = 0x8000;        // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM16 = 0x10000;       // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM17 = 0x20000;       // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM18 = 0x40000;       // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM19 = 0x80000;       // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM2 = 0x4;            // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM21 = 0x200000;      // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM23 = 0x800000;      // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM25 = 0x2000000;     // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM26 = 0x4000000;     // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM27 = 0x8000000;     // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM28 = 0x10000000;    // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM29 = 0x20000000;    // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM3 = 0x8;            // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM30 = 0x40000000;    // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM31 = 0x80000000;    // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM4 = 0x10;           // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM5 = 0x20;           // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM6 = 0x40;           // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM7 = 0x80;           // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM8 = 0x100;          // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_EM9 = 0x200;          // CPU wakeup with event mask on event input
    static constexpr uint32_t IMR1_IM0 = 0x1;            // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM1 = 0x2;            // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM10 = 0x400;         // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM11 = 0x800;         // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM12 = 0x1000;        // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM13 = 0x2000;        // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM14 = 0x4000;        // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM15 = 0x8000;        // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM16 = 0x10000;       // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM17 = 0x20000;       // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM18 = 0x40000;       // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM19 = 0x80000;       // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM2 = 0x4;            // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM20 = 0x100000;      // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM21 = 0x200000;      // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM22 = 0x400000;      // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM23 = 0x800000;      // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM24 = 0x1000000;     // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM25 = 0x2000000;     // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM26 = 0x4000000;     // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM27 = 0x8000000;     // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM28 = 0x10000000;    // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM29 = 0x20000000;    // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM3 = 0x8;            // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM30 = 0x40000000;    // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM31 = 0x80000000;    // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM4 = 0x10;           // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM5 = 0x20;           // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM6 = 0x40;           // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM7 = 0x80;           // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM8 = 0x100;          // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR1_IM9 = 0x200;          // CPU wakeup with interrupt mask on event input
    static const uint32_t IMR1_RESET_VALUE = 0xfff80000;

    static constexpr uint32_t IMR2_IM32 = 0x1;           // CPU wakeup with interrupt mask on event input
    static constexpr uint32_t IMR2_IM33 = 0x2;           // CPU wakeup with interrupt mask on event input
    static const uint32_t IMR2_RESET_VALUE = 0xffffffff;

    static constexpr uint32_t EMR2_EM32 = 0x1;           // CPU wakeup with event mask on event input
    static constexpr uint32_t EMR2_EM33 = 0x2;           // CPU wakeup with event mask on event input
    static const uint32_t EMR2_RESET_VALUE = 0x0;


    static const uint32_t HWCFGR7_RESET_VALUE = 0x0;


    static const uint32_t HWCFGR6_RESET_VALUE = 0x3;


    static const uint32_t HWCFGR5_RESET_VALUE = 0xfeafffff;


    static const uint32_t HWCFGR4_RESET_VALUE = 0x0;


    static const uint32_t HWCFGR3_RESET_VALUE = 0x0;


    static const uint32_t HWCFGR2_RESET_VALUE = 0x7ffff;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_NBIOPORT =            // HW configuration of number of IO ports (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CPUEVTEN =            // HW configuration of CPU event output enable (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_NBCPUS =              // configuration number of CPUs (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_NBEVENTS =            // configuration number of event (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t HWCFGR1_RESET_VALUE = 0x51021;

    template<uint32_t X>
    static constexpr uint32_t VERR_MAJREV =              // Major revision (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t VERR_MINREV =              // Minor revision (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t VERR_RESET_VALUE = 0x30;


    static const uint32_t IPIDR_RESET_VALUE = 0xe0001;


    static const uint32_t SIDR_RESET_VALUE = 0xa3c5dd01;

    static constexpr uint8_t EXTI0_1 = 5; // EXTI line 0 &amp; 1 interrupt
    static constexpr uint8_t EXTI2_3 = 6; // EXTI line 2 &amp; 3 interrupt
    static constexpr uint8_t EXTI4_15 = 7; // EXTI line 4 to 15 interrupt
};

static exti_t& EXTI = *reinterpret_cast<exti_t*>(0x40021800);

#define HAVE_PERIPHERAL_EXTI


////
//
//    General purpose timers
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
    volatile uint32_t    CNT;                  // counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    volatile uint32_t    RCR;                  // [Read-write] repetition counter register
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    reserved_t<3>        _2;
    volatile uint32_t    BDTR;                 // [Read-write] break and dead-time register
    volatile uint32_t    DCR;                  // [Read-write] DMA control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer
    reserved_t<4>        _3;
    volatile uint32_t    AF1;                  // [Read-write] TIM17 option register 1
    reserved_t<1>        _4;
    volatile uint32_t    TISEL;                // [Read-write] input selection register

    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_UIFREMAP = 0x800;     // UIF status bit remapping
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_OIS1N = 0x200;        // Output Idle state 1
    static constexpr uint32_t CR2_OIS1 = 0x100;         // Output Idle state 1
    static constexpr uint32_t CR2_CCDS = 0x8;           // Capture/compare DMA selection
    static constexpr uint32_t CR2_CCUS = 0x4;           // Capture/compare control update selection
    static constexpr uint32_t CR2_CCPC = 0x1;           // Capture/compare preloaded control
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_COMDE = 0x2000;       // COM DMA request enable
    static constexpr uint32_t DIER_CC1DE = 0x200;        // Capture/Compare 1 DMA request enable
    static constexpr uint32_t DIER_UDE = 0x100;          // Update DMA request enable
    static constexpr uint32_t DIER_BIE = 0x80;           // Break interrupt enable
    static constexpr uint32_t DIER_COMIE = 0x20;         // COM interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_BG = 0x80;            // Break generation
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
    static constexpr uint32_t CCMR1_OC1M_2 = 0x10000;     // Output Compare 1 mode
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output Compare 1 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC1NP = 0x8;          // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1NE = 0x4;          // Capture/Compare 1 complementary output enable
    static constexpr uint32_t CCER_CC1P = 0x2;           // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1E = 0x1;           // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // counter value (16 bits), Read-write
        bit_field_t<0, 0xffff>::value<X>();
    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIF Copy, Read-only
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
    static constexpr uint32_t BDTR_DTG =                 // Dead-time generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_LOCK =                // Lock configuration (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t BDTR_OSSI = 0x400;         // Off-state selection for Idle mode
    static constexpr uint32_t BDTR_OSSR = 0x800;         // Off-state selection for Run mode
    static constexpr uint32_t BDTR_BKE = 0x1000;         // Break enable
    static constexpr uint32_t BDTR_BKP = 0x2000;         // Break polarity
    static constexpr uint32_t BDTR_AOE = 0x4000;         // Automatic output enable
    static constexpr uint32_t BDTR_MOE = 0x8000;         // Main output enable
    template<uint32_t X>
    static constexpr uint32_t BDTR_BKF =                 // Break filter (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    static constexpr uint32_t BDTR_BKDSRM = 0x4000000;   // Break Disarm
    static constexpr uint32_t BDTR_BKBID = 0x10000000;   // Break Bidirectional
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

    static constexpr uint32_t AF1_BKINE = 0x1;          // BRK BKIN input enable
    static constexpr uint32_t AF1_BKCMP1E = 0x2;        // BRK COMP1 enable
    static constexpr uint32_t AF1_BKCMP2E = 0x4;        // BRK COMP2 enable
    static constexpr uint32_t AF1_BKDFBK1E = 0x100;     // BRK DFSDM_BREAK1 enable
    static constexpr uint32_t AF1_BKINP = 0x200;        // BRK BKIN input polarity
    static constexpr uint32_t AF1_BKCMP1P = 0x400;      // BRK COMP1 input polarity
    static constexpr uint32_t AF1_BKCMP2P = 0x800;      // BRK COMP2 input polarit
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // selects input (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM16 = 21; // TIM16 global interrupt
};

static tim16_t& TIM16 = *reinterpret_cast<tim16_t*>(0x40014400);

#define HAVE_PERIPHERAL_TIM16


////
//
//    General purpose timers
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
    volatile uint32_t    CNT;                  // counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    volatile uint32_t    RCR;                  // [Read-write] repetition counter register
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    reserved_t<3>        _2;
    volatile uint32_t    BDTR;                 // [Read-write] break and dead-time register
    volatile uint32_t    DCR;                  // [Read-write] DMA control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer
    reserved_t<4>        _3;
    volatile uint32_t    AF1;                  // [Read-write] TIM17 option register 1
    reserved_t<1>        _4;
    volatile uint32_t    TISEL;                // [Read-write] input selection register

    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_UIFREMAP = 0x800;     // UIF status bit remapping
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_OIS1N = 0x200;        // Output Idle state 1
    static constexpr uint32_t CR2_OIS1 = 0x100;         // Output Idle state 1
    static constexpr uint32_t CR2_CCDS = 0x8;           // Capture/compare DMA selection
    static constexpr uint32_t CR2_CCUS = 0x4;           // Capture/compare control update selection
    static constexpr uint32_t CR2_CCPC = 0x1;           // Capture/compare preloaded control
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_COMDE = 0x2000;       // COM DMA request enable
    static constexpr uint32_t DIER_CC1DE = 0x200;        // Capture/Compare 1 DMA request enable
    static constexpr uint32_t DIER_UDE = 0x100;          // Update DMA request enable
    static constexpr uint32_t DIER_BIE = 0x80;           // Break interrupt enable
    static constexpr uint32_t DIER_COMIE = 0x20;         // COM interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_BG = 0x80;            // Break generation
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
    static constexpr uint32_t CCMR1_OC1M_2 = 0x10000;     // Output Compare 1 mode
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output Compare 1 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC1NP = 0x8;          // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1NE = 0x4;          // Capture/Compare 1 complementary output enable
    static constexpr uint32_t CCER_CC1P = 0x2;           // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1E = 0x1;           // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // counter value (16 bits), Read-write
        bit_field_t<0, 0xffff>::value<X>();
    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIF Copy, Read-only
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
    static constexpr uint32_t BDTR_DTG =                 // Dead-time generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_LOCK =                // Lock configuration (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t BDTR_OSSI = 0x400;         // Off-state selection for Idle mode
    static constexpr uint32_t BDTR_OSSR = 0x800;         // Off-state selection for Run mode
    static constexpr uint32_t BDTR_BKE = 0x1000;         // Break enable
    static constexpr uint32_t BDTR_BKP = 0x2000;         // Break polarity
    static constexpr uint32_t BDTR_AOE = 0x4000;         // Automatic output enable
    static constexpr uint32_t BDTR_MOE = 0x8000;         // Main output enable
    template<uint32_t X>
    static constexpr uint32_t BDTR_BKF =                 // Break filter (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    static constexpr uint32_t BDTR_BKDSRM = 0x4000000;   // Break Disarm
    static constexpr uint32_t BDTR_BKBID = 0x10000000;   // Break Bidirectional
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

    static constexpr uint32_t AF1_BKINE = 0x1;          // BRK BKIN input enable
    static constexpr uint32_t AF1_BKCMP1E = 0x2;        // BRK COMP1 enable
    static constexpr uint32_t AF1_BKCMP2E = 0x4;        // BRK COMP2 enable
    static constexpr uint32_t AF1_BKDFBK1E = 0x100;     // BRK DFSDM_BREAK1 enable
    static constexpr uint32_t AF1_BKINP = 0x200;        // BRK BKIN input polarity
    static constexpr uint32_t AF1_BKCMP1P = 0x400;      // BRK COMP1 input polarity
    static constexpr uint32_t AF1_BKCMP2P = 0x800;      // BRK COMP2 input polarit
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // selects input (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM17 = 22; // TIM17 global interrupt
};

static tim17_t& TIM17 = *reinterpret_cast<tim17_t*>(0x40014800);

#define HAVE_PERIPHERAL_TIM17


////
//
//    General purpose timers
//
////

struct tim15_t
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
    volatile uint32_t    CNT;                  // counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    volatile uint32_t    RCR;                  // [Read-write] repetition counter register
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    reserved_t<3>        _2;
    volatile uint32_t    BDTR;                 // [Read-write] break and dead-time register
    volatile uint32_t    DCR;                  // [Read-write] DMA control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer
    reserved_t<4>        _3;
    volatile uint32_t    AF1;                  // [Read-write] TIM17 option register 1
    reserved_t<1>        _4;
    volatile uint32_t    TISEL;                // [Read-write] input selection register

    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_UIFREMAP = 0x800;     // UIF status bit remapping
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_OIS1N = 0x200;        // Output Idle state 1
    static constexpr uint32_t CR2_OIS1 = 0x100;         // Output Idle state 1
    static constexpr uint32_t CR2_CCDS = 0x8;           // Capture/compare DMA selection
    static constexpr uint32_t CR2_CCUS = 0x4;           // Capture/compare control update selection
    static constexpr uint32_t CR2_CCPC = 0x1;           // Capture/compare preloaded control
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_COMDE = 0x2000;       // COM DMA request enable
    static constexpr uint32_t DIER_CC1DE = 0x200;        // Capture/Compare 1 DMA request enable
    static constexpr uint32_t DIER_UDE = 0x100;          // Update DMA request enable
    static constexpr uint32_t DIER_BIE = 0x80;           // Break interrupt enable
    static constexpr uint32_t DIER_COMIE = 0x20;         // COM interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_BG = 0x80;            // Break generation
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
    static constexpr uint32_t CCMR1_OC1M_2 = 0x10000;     // Output Compare 1 mode
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output Compare 1 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC1NP = 0x8;          // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1NE = 0x4;          // Capture/Compare 1 complementary output enable
    static constexpr uint32_t CCER_CC1P = 0x2;           // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1E = 0x1;           // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // counter value (16 bits), Read-write
        bit_field_t<0, 0xffff>::value<X>();
    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIF Copy, Read-only
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
    static constexpr uint32_t BDTR_DTG =                 // Dead-time generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_LOCK =                // Lock configuration (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t BDTR_OSSI = 0x400;         // Off-state selection for Idle mode
    static constexpr uint32_t BDTR_OSSR = 0x800;         // Off-state selection for Run mode
    static constexpr uint32_t BDTR_BKE = 0x1000;         // Break enable
    static constexpr uint32_t BDTR_BKP = 0x2000;         // Break polarity
    static constexpr uint32_t BDTR_AOE = 0x4000;         // Automatic output enable
    static constexpr uint32_t BDTR_MOE = 0x8000;         // Main output enable
    template<uint32_t X>
    static constexpr uint32_t BDTR_BKF =                 // Break filter (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    static constexpr uint32_t BDTR_BKDSRM = 0x4000000;   // Break Disarm
    static constexpr uint32_t BDTR_BKBID = 0x10000000;   // Break Bidirectional
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

    static constexpr uint32_t AF1_BKINE = 0x1;          // BRK BKIN input enable
    static constexpr uint32_t AF1_BKCMP1E = 0x2;        // BRK COMP1 enable
    static constexpr uint32_t AF1_BKCMP2E = 0x4;        // BRK COMP2 enable
    static constexpr uint32_t AF1_BKDFBK1E = 0x100;     // BRK DFSDM_BREAK1 enable
    static constexpr uint32_t AF1_BKINP = 0x200;        // BRK BKIN input polarity
    static constexpr uint32_t AF1_BKCMP1P = 0x400;      // BRK COMP1 input polarity
    static constexpr uint32_t AF1_BKCMP2P = 0x800;      // BRK COMP2 input polarit
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // selects input (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM15 = 20; // TIM15 global interrupt
};

static tim15_t& TIM15 = *reinterpret_cast<tim15_t*>(0x40014000);

#define HAVE_PERIPHERAL_TIM15


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
    volatile uint32_t    RQR;                  // [Write-only] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Write-only] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register
    volatile uint32_t    PRESC;                // [Read-write] Prescaler register

    static constexpr uint32_t CR1_RXFFIE = 0x80000000;  // RXFIFO Full interrupt enable
    static constexpr uint32_t CR1_TXFEIE = 0x40000000;  // TXFIFO empty interrupt enable
    static constexpr uint32_t CR1_FIFOEN = 0x20000000;  // FIFO mode enable
    static constexpr uint32_t CR1_M1 = 0x10000000;      // Word length
    static constexpr uint32_t CR1_EOBIE = 0x8000000;    // End of Block interrupt enable
    static constexpr uint32_t CR1_RTOIE = 0x4000000;    // Receiver timeout interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR1_DEAT =                // DEAT (5 bits)
        bit_field_t<21, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR1_DEDT =                // DEDT (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static constexpr uint32_t CR1_OVER8 = 0x8000;       // Oversampling mode
    static constexpr uint32_t CR1_CMIE = 0x4000;        // Character match interrupt enable
    static constexpr uint32_t CR1_MME = 0x2000;         // Mute mode enable
    static constexpr uint32_t CR1_M0 = 0x1000;          // Word length
    static constexpr uint32_t CR1_WAKE = 0x800;         // Receiver wakeup method
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_UESM = 0x2;           // USART enable in Stop mode
    static constexpr uint32_t CR1_UE = 0x1;             // USART enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_ADD4_7 =              // Address of the USART node (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_ADD0_3 =              // Address of the USART node (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t CR2_RTOEN = 0x800000;     // Receiver timeout enable
    template<uint32_t X>
    static constexpr uint32_t CR2_ABRMOD =              // Auto baud rate mode (2 bits)
        bit_field_t<21, 0x3>::value<X>();
    static constexpr uint32_t CR2_ABREN = 0x100000;     // Auto baud rate enable
    static constexpr uint32_t CR2_MSBFIRST = 0x80000;   // Most significant bit first
    static constexpr uint32_t CR2_TAINV = 0x40000;      // Binary data inversion
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
    static constexpr uint32_t CR2_DIS_NSS = 0x8;        // When the DSI_NSS bit is set, the NSS pin input will be ignored
    static constexpr uint32_t CR2_SLVEN = 0x1;          // Synchronous Slave mode enable
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR3_TXFTCFG =             // TXFIFO threshold configuration (3 bits)
        bit_field_t<29, 0x7>::value<X>();
    static constexpr uint32_t CR3_RXFTIE = 0x10000000;  // RXFIFO threshold interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR3_RXFTCFG =             // Receive FIFO threshold configuration (3 bits)
        bit_field_t<25, 0x7>::value<X>();
    static constexpr uint32_t CR3_TCBGTIE = 0x1000000;  // Tr Complete before guard time, interrupt enable
    static constexpr uint32_t CR3_TXFTIE = 0x800000;    // threshold interrupt enable
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
    static constexpr uint32_t CR3_IRLP = 0x4;           // Ir low-power
    static constexpr uint32_t CR3_IREN = 0x2;           // Ir mode enable
    static constexpr uint32_t CR3_EIE = 0x1;            // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_BRR_4_15 =            // BRR_4_15 (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_BRR_0_3 =             // BRR_0_3 (4 bits)
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

    static constexpr uint32_t ISR_TXFT = 0x8000000;     // TXFIFO threshold flag
    static constexpr uint32_t ISR_RXFT = 0x4000000;     // RXFIFO threshold flag
    static constexpr uint32_t ISR_TCBGT = 0x2000000;    // Transmission complete before guard time flag
    static constexpr uint32_t ISR_RXFF = 0x1000000;     // RXFIFO Full
    static constexpr uint32_t ISR_TXFE = 0x800000;      // TXFIFO Empty
    static constexpr uint32_t ISR_REACK = 0x400000;     // REACK
    static constexpr uint32_t ISR_TEACK = 0x200000;     // TEACK
    static constexpr uint32_t ISR_WUF = 0x100000;       // WUF
    static constexpr uint32_t ISR_RWU = 0x80000;        // RWU
    static constexpr uint32_t ISR_SBKF = 0x40000;       // SBKF
    static constexpr uint32_t ISR_CMF = 0x20000;        // CMF
    static constexpr uint32_t ISR_BUSY = 0x10000;       // BUSY
    static constexpr uint32_t ISR_ABRF = 0x8000;        // ABRF
    static constexpr uint32_t ISR_ABRE = 0x4000;        // ABRE
    static constexpr uint32_t ISR_UDR = 0x2000;         // SPI slave underrun error flag
    static constexpr uint32_t ISR_EOBF = 0x1000;        // EOBF
    static constexpr uint32_t ISR_RTOF = 0x800;         // RTOF
    static constexpr uint32_t ISR_CTS = 0x400;          // CTS
    static constexpr uint32_t ISR_CTSIF = 0x200;        // CTSIF
    static constexpr uint32_t ISR_LBDF = 0x100;         // LBDF
    static constexpr uint32_t ISR_TXE = 0x80;           // TXE
    static constexpr uint32_t ISR_TC = 0x40;            // TC
    static constexpr uint32_t ISR_RXNE = 0x20;          // RXNE
    static constexpr uint32_t ISR_IDLE = 0x10;          // IDLE
    static constexpr uint32_t ISR_ORE = 0x8;            // ORE
    static constexpr uint32_t ISR_NF = 0x4;             // NF
    static constexpr uint32_t ISR_FE = 0x2;             // FE
    static constexpr uint32_t ISR_PE = 0x1;             // PE
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static constexpr uint32_t ICR_WUCF = 0x100000;      // Wakeup from Stop mode clear flag
    static constexpr uint32_t ICR_CMCF = 0x20000;       // Character match clear flag
    static constexpr uint32_t ICR_UDRCF = 0x2000;       // SPI slave underrun clear flag
    static constexpr uint32_t ICR_EOBCF = 0x1000;       // End of block clear flag
    static constexpr uint32_t ICR_RTOCF = 0x800;        // Receiver timeout clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
    static constexpr uint32_t ICR_LBDCF = 0x100;        // LIN break detection clear flag
    static constexpr uint32_t ICR_TCBGTCF = 0x80;       // Transmission complete before Guard time clear flag
    static constexpr uint32_t ICR_TCCF = 0x40;          // Transmission complete clear flag
    static constexpr uint32_t ICR_TXFECF = 0x20;        // TXFIFO empty clear flag
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

    template<uint32_t X>
    static constexpr uint32_t PRESC_PRESCALER =           // Clock prescaler (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESC_RESET_VALUE = 0x0;

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
    volatile uint32_t    RQR;                  // [Write-only] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Write-only] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register
    volatile uint32_t    PRESC;                // [Read-write] Prescaler register

    static constexpr uint32_t CR1_RXFFIE = 0x80000000;  // RXFIFO Full interrupt enable
    static constexpr uint32_t CR1_TXFEIE = 0x40000000;  // TXFIFO empty interrupt enable
    static constexpr uint32_t CR1_FIFOEN = 0x20000000;  // FIFO mode enable
    static constexpr uint32_t CR1_M1 = 0x10000000;      // Word length
    static constexpr uint32_t CR1_EOBIE = 0x8000000;    // End of Block interrupt enable
    static constexpr uint32_t CR1_RTOIE = 0x4000000;    // Receiver timeout interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR1_DEAT =                // DEAT (5 bits)
        bit_field_t<21, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR1_DEDT =                // DEDT (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static constexpr uint32_t CR1_OVER8 = 0x8000;       // Oversampling mode
    static constexpr uint32_t CR1_CMIE = 0x4000;        // Character match interrupt enable
    static constexpr uint32_t CR1_MME = 0x2000;         // Mute mode enable
    static constexpr uint32_t CR1_M0 = 0x1000;          // Word length
    static constexpr uint32_t CR1_WAKE = 0x800;         // Receiver wakeup method
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_UESM = 0x2;           // USART enable in Stop mode
    static constexpr uint32_t CR1_UE = 0x1;             // USART enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_ADD4_7 =              // Address of the USART node (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_ADD0_3 =              // Address of the USART node (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t CR2_RTOEN = 0x800000;     // Receiver timeout enable
    template<uint32_t X>
    static constexpr uint32_t CR2_ABRMOD =              // Auto baud rate mode (2 bits)
        bit_field_t<21, 0x3>::value<X>();
    static constexpr uint32_t CR2_ABREN = 0x100000;     // Auto baud rate enable
    static constexpr uint32_t CR2_MSBFIRST = 0x80000;   // Most significant bit first
    static constexpr uint32_t CR2_TAINV = 0x40000;      // Binary data inversion
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
    static constexpr uint32_t CR2_DIS_NSS = 0x8;        // When the DSI_NSS bit is set, the NSS pin input will be ignored
    static constexpr uint32_t CR2_SLVEN = 0x1;          // Synchronous Slave mode enable
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR3_TXFTCFG =             // TXFIFO threshold configuration (3 bits)
        bit_field_t<29, 0x7>::value<X>();
    static constexpr uint32_t CR3_RXFTIE = 0x10000000;  // RXFIFO threshold interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR3_RXFTCFG =             // Receive FIFO threshold configuration (3 bits)
        bit_field_t<25, 0x7>::value<X>();
    static constexpr uint32_t CR3_TCBGTIE = 0x1000000;  // Tr Complete before guard time, interrupt enable
    static constexpr uint32_t CR3_TXFTIE = 0x800000;    // threshold interrupt enable
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
    static constexpr uint32_t CR3_IRLP = 0x4;           // Ir low-power
    static constexpr uint32_t CR3_IREN = 0x2;           // Ir mode enable
    static constexpr uint32_t CR3_EIE = 0x1;            // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_BRR_4_15 =            // BRR_4_15 (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_BRR_0_3 =             // BRR_0_3 (4 bits)
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

    static constexpr uint32_t ISR_TXFT = 0x8000000;     // TXFIFO threshold flag
    static constexpr uint32_t ISR_RXFT = 0x4000000;     // RXFIFO threshold flag
    static constexpr uint32_t ISR_TCBGT = 0x2000000;    // Transmission complete before guard time flag
    static constexpr uint32_t ISR_RXFF = 0x1000000;     // RXFIFO Full
    static constexpr uint32_t ISR_TXFE = 0x800000;      // TXFIFO Empty
    static constexpr uint32_t ISR_REACK = 0x400000;     // REACK
    static constexpr uint32_t ISR_TEACK = 0x200000;     // TEACK
    static constexpr uint32_t ISR_WUF = 0x100000;       // WUF
    static constexpr uint32_t ISR_RWU = 0x80000;        // RWU
    static constexpr uint32_t ISR_SBKF = 0x40000;       // SBKF
    static constexpr uint32_t ISR_CMF = 0x20000;        // CMF
    static constexpr uint32_t ISR_BUSY = 0x10000;       // BUSY
    static constexpr uint32_t ISR_ABRF = 0x8000;        // ABRF
    static constexpr uint32_t ISR_ABRE = 0x4000;        // ABRE
    static constexpr uint32_t ISR_UDR = 0x2000;         // SPI slave underrun error flag
    static constexpr uint32_t ISR_EOBF = 0x1000;        // EOBF
    static constexpr uint32_t ISR_RTOF = 0x800;         // RTOF
    static constexpr uint32_t ISR_CTS = 0x400;          // CTS
    static constexpr uint32_t ISR_CTSIF = 0x200;        // CTSIF
    static constexpr uint32_t ISR_LBDF = 0x100;         // LBDF
    static constexpr uint32_t ISR_TXE = 0x80;           // TXE
    static constexpr uint32_t ISR_TC = 0x40;            // TC
    static constexpr uint32_t ISR_RXNE = 0x20;          // RXNE
    static constexpr uint32_t ISR_IDLE = 0x10;          // IDLE
    static constexpr uint32_t ISR_ORE = 0x8;            // ORE
    static constexpr uint32_t ISR_NF = 0x4;             // NF
    static constexpr uint32_t ISR_FE = 0x2;             // FE
    static constexpr uint32_t ISR_PE = 0x1;             // PE
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static constexpr uint32_t ICR_WUCF = 0x100000;      // Wakeup from Stop mode clear flag
    static constexpr uint32_t ICR_CMCF = 0x20000;       // Character match clear flag
    static constexpr uint32_t ICR_UDRCF = 0x2000;       // SPI slave underrun clear flag
    static constexpr uint32_t ICR_EOBCF = 0x1000;       // End of block clear flag
    static constexpr uint32_t ICR_RTOCF = 0x800;        // Receiver timeout clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
    static constexpr uint32_t ICR_LBDCF = 0x100;        // LIN break detection clear flag
    static constexpr uint32_t ICR_TCBGTCF = 0x80;       // Transmission complete before Guard time clear flag
    static constexpr uint32_t ICR_TCCF = 0x40;          // Transmission complete clear flag
    static constexpr uint32_t ICR_TXFECF = 0x20;        // TXFIFO empty clear flag
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

    template<uint32_t X>
    static constexpr uint32_t PRESC_PRESCALER =           // Clock prescaler (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESC_RESET_VALUE = 0x0;

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
    volatile uint32_t    RQR;                  // [Write-only] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Write-only] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register
    volatile uint32_t    PRESC;                // [Read-write] Prescaler register

    static constexpr uint32_t CR1_RXFFIE = 0x80000000;  // RXFIFO Full interrupt enable
    static constexpr uint32_t CR1_TXFEIE = 0x40000000;  // TXFIFO empty interrupt enable
    static constexpr uint32_t CR1_FIFOEN = 0x20000000;  // FIFO mode enable
    static constexpr uint32_t CR1_M1 = 0x10000000;      // Word length
    static constexpr uint32_t CR1_EOBIE = 0x8000000;    // End of Block interrupt enable
    static constexpr uint32_t CR1_RTOIE = 0x4000000;    // Receiver timeout interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR1_DEAT =                // DEAT (5 bits)
        bit_field_t<21, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR1_DEDT =                // DEDT (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static constexpr uint32_t CR1_OVER8 = 0x8000;       // Oversampling mode
    static constexpr uint32_t CR1_CMIE = 0x4000;        // Character match interrupt enable
    static constexpr uint32_t CR1_MME = 0x2000;         // Mute mode enable
    static constexpr uint32_t CR1_M0 = 0x1000;          // Word length
    static constexpr uint32_t CR1_WAKE = 0x800;         // Receiver wakeup method
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_UESM = 0x2;           // USART enable in Stop mode
    static constexpr uint32_t CR1_UE = 0x1;             // USART enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_ADD4_7 =              // Address of the USART node (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_ADD0_3 =              // Address of the USART node (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t CR2_RTOEN = 0x800000;     // Receiver timeout enable
    template<uint32_t X>
    static constexpr uint32_t CR2_ABRMOD =              // Auto baud rate mode (2 bits)
        bit_field_t<21, 0x3>::value<X>();
    static constexpr uint32_t CR2_ABREN = 0x100000;     // Auto baud rate enable
    static constexpr uint32_t CR2_MSBFIRST = 0x80000;   // Most significant bit first
    static constexpr uint32_t CR2_TAINV = 0x40000;      // Binary data inversion
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
    static constexpr uint32_t CR2_DIS_NSS = 0x8;        // When the DSI_NSS bit is set, the NSS pin input will be ignored
    static constexpr uint32_t CR2_SLVEN = 0x1;          // Synchronous Slave mode enable
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR3_TXFTCFG =             // TXFIFO threshold configuration (3 bits)
        bit_field_t<29, 0x7>::value<X>();
    static constexpr uint32_t CR3_RXFTIE = 0x10000000;  // RXFIFO threshold interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR3_RXFTCFG =             // Receive FIFO threshold configuration (3 bits)
        bit_field_t<25, 0x7>::value<X>();
    static constexpr uint32_t CR3_TCBGTIE = 0x1000000;  // Tr Complete before guard time, interrupt enable
    static constexpr uint32_t CR3_TXFTIE = 0x800000;    // threshold interrupt enable
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
    static constexpr uint32_t CR3_IRLP = 0x4;           // Ir low-power
    static constexpr uint32_t CR3_IREN = 0x2;           // Ir mode enable
    static constexpr uint32_t CR3_EIE = 0x1;            // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_BRR_4_15 =            // BRR_4_15 (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_BRR_0_3 =             // BRR_0_3 (4 bits)
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

    static constexpr uint32_t ISR_TXFT = 0x8000000;     // TXFIFO threshold flag
    static constexpr uint32_t ISR_RXFT = 0x4000000;     // RXFIFO threshold flag
    static constexpr uint32_t ISR_TCBGT = 0x2000000;    // Transmission complete before guard time flag
    static constexpr uint32_t ISR_RXFF = 0x1000000;     // RXFIFO Full
    static constexpr uint32_t ISR_TXFE = 0x800000;      // TXFIFO Empty
    static constexpr uint32_t ISR_REACK = 0x400000;     // REACK
    static constexpr uint32_t ISR_TEACK = 0x200000;     // TEACK
    static constexpr uint32_t ISR_WUF = 0x100000;       // WUF
    static constexpr uint32_t ISR_RWU = 0x80000;        // RWU
    static constexpr uint32_t ISR_SBKF = 0x40000;       // SBKF
    static constexpr uint32_t ISR_CMF = 0x20000;        // CMF
    static constexpr uint32_t ISR_BUSY = 0x10000;       // BUSY
    static constexpr uint32_t ISR_ABRF = 0x8000;        // ABRF
    static constexpr uint32_t ISR_ABRE = 0x4000;        // ABRE
    static constexpr uint32_t ISR_UDR = 0x2000;         // SPI slave underrun error flag
    static constexpr uint32_t ISR_EOBF = 0x1000;        // EOBF
    static constexpr uint32_t ISR_RTOF = 0x800;         // RTOF
    static constexpr uint32_t ISR_CTS = 0x400;          // CTS
    static constexpr uint32_t ISR_CTSIF = 0x200;        // CTSIF
    static constexpr uint32_t ISR_LBDF = 0x100;         // LBDF
    static constexpr uint32_t ISR_TXE = 0x80;           // TXE
    static constexpr uint32_t ISR_TC = 0x40;            // TC
    static constexpr uint32_t ISR_RXNE = 0x20;          // RXNE
    static constexpr uint32_t ISR_IDLE = 0x10;          // IDLE
    static constexpr uint32_t ISR_ORE = 0x8;            // ORE
    static constexpr uint32_t ISR_NF = 0x4;             // NF
    static constexpr uint32_t ISR_FE = 0x2;             // FE
    static constexpr uint32_t ISR_PE = 0x1;             // PE
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static constexpr uint32_t ICR_WUCF = 0x100000;      // Wakeup from Stop mode clear flag
    static constexpr uint32_t ICR_CMCF = 0x20000;       // Character match clear flag
    static constexpr uint32_t ICR_UDRCF = 0x2000;       // SPI slave underrun clear flag
    static constexpr uint32_t ICR_EOBCF = 0x1000;       // End of block clear flag
    static constexpr uint32_t ICR_RTOCF = 0x800;        // Receiver timeout clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
    static constexpr uint32_t ICR_LBDCF = 0x100;        // LIN break detection clear flag
    static constexpr uint32_t ICR_TCBGTCF = 0x80;       // Transmission complete before Guard time clear flag
    static constexpr uint32_t ICR_TCCF = 0x40;          // Transmission complete clear flag
    static constexpr uint32_t ICR_TXFECF = 0x20;        // TXFIFO empty clear flag
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

    template<uint32_t X>
    static constexpr uint32_t PRESC_PRESCALER =           // Clock prescaler (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESC_RESET_VALUE = 0x0;

    static constexpr uint8_t USART3_USART4_LPUART1 = 29; // USART3 + USART4 + LPUART1
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
    volatile uint32_t    RQR;                  // [Write-only] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Write-only] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register
    volatile uint32_t    PRESC;                // [Read-write] Prescaler register

    static constexpr uint32_t CR1_RXFFIE = 0x80000000;  // RXFIFO Full interrupt enable
    static constexpr uint32_t CR1_TXFEIE = 0x40000000;  // TXFIFO empty interrupt enable
    static constexpr uint32_t CR1_FIFOEN = 0x20000000;  // FIFO mode enable
    static constexpr uint32_t CR1_M1 = 0x10000000;      // Word length
    static constexpr uint32_t CR1_EOBIE = 0x8000000;    // End of Block interrupt enable
    static constexpr uint32_t CR1_RTOIE = 0x4000000;    // Receiver timeout interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR1_DEAT =                // DEAT (5 bits)
        bit_field_t<21, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR1_DEDT =                // DEDT (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static constexpr uint32_t CR1_OVER8 = 0x8000;       // Oversampling mode
    static constexpr uint32_t CR1_CMIE = 0x4000;        // Character match interrupt enable
    static constexpr uint32_t CR1_MME = 0x2000;         // Mute mode enable
    static constexpr uint32_t CR1_M0 = 0x1000;          // Word length
    static constexpr uint32_t CR1_WAKE = 0x800;         // Receiver wakeup method
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_UESM = 0x2;           // USART enable in Stop mode
    static constexpr uint32_t CR1_UE = 0x1;             // USART enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_ADD4_7 =              // Address of the USART node (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_ADD0_3 =              // Address of the USART node (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t CR2_RTOEN = 0x800000;     // Receiver timeout enable
    template<uint32_t X>
    static constexpr uint32_t CR2_ABRMOD =              // Auto baud rate mode (2 bits)
        bit_field_t<21, 0x3>::value<X>();
    static constexpr uint32_t CR2_ABREN = 0x100000;     // Auto baud rate enable
    static constexpr uint32_t CR2_MSBFIRST = 0x80000;   // Most significant bit first
    static constexpr uint32_t CR2_TAINV = 0x40000;      // Binary data inversion
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
    static constexpr uint32_t CR2_DIS_NSS = 0x8;        // When the DSI_NSS bit is set, the NSS pin input will be ignored
    static constexpr uint32_t CR2_SLVEN = 0x1;          // Synchronous Slave mode enable
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR3_TXFTCFG =             // TXFIFO threshold configuration (3 bits)
        bit_field_t<29, 0x7>::value<X>();
    static constexpr uint32_t CR3_RXFTIE = 0x10000000;  // RXFIFO threshold interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR3_RXFTCFG =             // Receive FIFO threshold configuration (3 bits)
        bit_field_t<25, 0x7>::value<X>();
    static constexpr uint32_t CR3_TCBGTIE = 0x1000000;  // Tr Complete before guard time, interrupt enable
    static constexpr uint32_t CR3_TXFTIE = 0x800000;    // threshold interrupt enable
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
    static constexpr uint32_t CR3_IRLP = 0x4;           // Ir low-power
    static constexpr uint32_t CR3_IREN = 0x2;           // Ir mode enable
    static constexpr uint32_t CR3_EIE = 0x1;            // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_BRR_4_15 =            // BRR_4_15 (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_BRR_0_3 =             // BRR_0_3 (4 bits)
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

    static constexpr uint32_t ISR_TXFT = 0x8000000;     // TXFIFO threshold flag
    static constexpr uint32_t ISR_RXFT = 0x4000000;     // RXFIFO threshold flag
    static constexpr uint32_t ISR_TCBGT = 0x2000000;    // Transmission complete before guard time flag
    static constexpr uint32_t ISR_RXFF = 0x1000000;     // RXFIFO Full
    static constexpr uint32_t ISR_TXFE = 0x800000;      // TXFIFO Empty
    static constexpr uint32_t ISR_REACK = 0x400000;     // REACK
    static constexpr uint32_t ISR_TEACK = 0x200000;     // TEACK
    static constexpr uint32_t ISR_WUF = 0x100000;       // WUF
    static constexpr uint32_t ISR_RWU = 0x80000;        // RWU
    static constexpr uint32_t ISR_SBKF = 0x40000;       // SBKF
    static constexpr uint32_t ISR_CMF = 0x20000;        // CMF
    static constexpr uint32_t ISR_BUSY = 0x10000;       // BUSY
    static constexpr uint32_t ISR_ABRF = 0x8000;        // ABRF
    static constexpr uint32_t ISR_ABRE = 0x4000;        // ABRE
    static constexpr uint32_t ISR_UDR = 0x2000;         // SPI slave underrun error flag
    static constexpr uint32_t ISR_EOBF = 0x1000;        // EOBF
    static constexpr uint32_t ISR_RTOF = 0x800;         // RTOF
    static constexpr uint32_t ISR_CTS = 0x400;          // CTS
    static constexpr uint32_t ISR_CTSIF = 0x200;        // CTSIF
    static constexpr uint32_t ISR_LBDF = 0x100;         // LBDF
    static constexpr uint32_t ISR_TXE = 0x80;           // TXE
    static constexpr uint32_t ISR_TC = 0x40;            // TC
    static constexpr uint32_t ISR_RXNE = 0x20;          // RXNE
    static constexpr uint32_t ISR_IDLE = 0x10;          // IDLE
    static constexpr uint32_t ISR_ORE = 0x8;            // ORE
    static constexpr uint32_t ISR_NF = 0x4;             // NF
    static constexpr uint32_t ISR_FE = 0x2;             // FE
    static constexpr uint32_t ISR_PE = 0x1;             // PE
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static constexpr uint32_t ICR_WUCF = 0x100000;      // Wakeup from Stop mode clear flag
    static constexpr uint32_t ICR_CMCF = 0x20000;       // Character match clear flag
    static constexpr uint32_t ICR_UDRCF = 0x2000;       // SPI slave underrun clear flag
    static constexpr uint32_t ICR_EOBCF = 0x1000;       // End of block clear flag
    static constexpr uint32_t ICR_RTOCF = 0x800;        // Receiver timeout clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
    static constexpr uint32_t ICR_LBDCF = 0x100;        // LIN break detection clear flag
    static constexpr uint32_t ICR_TCBGTCF = 0x80;       // Transmission complete before Guard time clear flag
    static constexpr uint32_t ICR_TCCF = 0x40;          // Transmission complete clear flag
    static constexpr uint32_t ICR_TXFECF = 0x20;        // TXFIFO empty clear flag
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

    template<uint32_t X>
    static constexpr uint32_t PRESC_PRESCALER =           // Clock prescaler (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESC_RESET_VALUE = 0x0;
};

static usart4_t& USART4 = *reinterpret_cast<usart4_t*>(0x40004c00);

#define HAVE_PERIPHERAL_USART4


////
//
//    Serial peripheral interface/Inter-IC sound
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
    volatile uint32_t    I2SCFGR;              // [Read-write] configuration register
    volatile uint32_t    I2SPR;                // [Read-write] prescaler register
    reserved_t<243>      _0;
    volatile uint32_t    HWCFGR;               // [Read-only] hardware configuration register
    volatile uint32_t    VERR;                 // [Read-only] EXTI IP Version register
    volatile uint32_t    IPIDR;                // [Read-only] EXTI Identification register
    volatile uint32_t    SIDR;                 // [Read-only] EXTI Size ID register

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

    static constexpr uint32_t I2SCFGR_CHLEN = 0x1;          // Channel length (number of bits per audio channel)
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_DATLEN =              // Data length to be transferred (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_CKPOL = 0x8;          // Inactive state clock polarity
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SSTD =              // standard selection (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_PCMSYNC = 0x80;       // PCM frame synchronization
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SCFG =              // I2S configuration mode (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_SE2 = 0x400;          // I2S enable
    static constexpr uint32_t I2SCFGR_I2SMOD = 0x800;       // I2S mode selection
    static const uint32_t I2SCFGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t I2SPR_I2SDIV =              // linear prescaler (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t I2SPR_ODD = 0x100;          // Odd factor for the prescaler
    static constexpr uint32_t I2SPR_MCKOE = 0x200;        // Master clock output enable
    static const uint32_t I2SPR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR_CRCCFG =              // CRC capable at SPI mode (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_I2SCFG =              // I2S mode implementation (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_I2SCKCFG =            // I2S master clock generator at I2S mode (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_DSCFG =               // SPI data size configuration (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_NSSCFG =              // NSS pulse feature enhancement at SPI master (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    static const uint32_t HWCFGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t VERR_MINREV =              // Minor Revision number (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t VERR_MAJREV =              // Major Revision number (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    static const uint32_t VERR_RESET_VALUE = 0x0;


    static const uint32_t IPIDR_RESET_VALUE = 0x0;


    static const uint32_t SIDR_RESET_VALUE = 0x0;

    static constexpr uint8_t SPI1 = 25; // SPI1 global interrupt
};

static spi1_t& SPI1 = *reinterpret_cast<spi1_t*>(0x40013000);

#define HAVE_PERIPHERAL_SPI1


////
//
//    Serial peripheral interface/Inter-IC sound
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
    volatile uint32_t    I2SCFGR;              // [Read-write] configuration register
    volatile uint32_t    I2SPR;                // [Read-write] prescaler register
    reserved_t<243>      _0;
    volatile uint32_t    HWCFGR;               // [Read-only] hardware configuration register
    volatile uint32_t    VERR;                 // [Read-only] EXTI IP Version register
    volatile uint32_t    IPIDR;                // [Read-only] EXTI Identification register
    volatile uint32_t    SIDR;                 // [Read-only] EXTI Size ID register

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

    static constexpr uint32_t I2SCFGR_CHLEN = 0x1;          // Channel length (number of bits per audio channel)
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_DATLEN =              // Data length to be transferred (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_CKPOL = 0x8;          // Inactive state clock polarity
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SSTD =              // standard selection (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_PCMSYNC = 0x80;       // PCM frame synchronization
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SCFG =              // I2S configuration mode (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_SE2 = 0x400;          // I2S enable
    static constexpr uint32_t I2SCFGR_I2SMOD = 0x800;       // I2S mode selection
    static const uint32_t I2SCFGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t I2SPR_I2SDIV =              // linear prescaler (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t I2SPR_ODD = 0x100;          // Odd factor for the prescaler
    static constexpr uint32_t I2SPR_MCKOE = 0x200;        // Master clock output enable
    static const uint32_t I2SPR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR_CRCCFG =              // CRC capable at SPI mode (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_I2SCFG =              // I2S mode implementation (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_I2SCKCFG =            // I2S master clock generator at I2S mode (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_DSCFG =               // SPI data size configuration (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_NSSCFG =              // NSS pulse feature enhancement at SPI master (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    static const uint32_t HWCFGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t VERR_MINREV =              // Minor Revision number (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t VERR_MAJREV =              // Major Revision number (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    static const uint32_t VERR_RESET_VALUE = 0x0;


    static const uint32_t IPIDR_RESET_VALUE = 0x0;


    static const uint32_t SIDR_RESET_VALUE = 0x0;

    static constexpr uint8_t SPI2 = 26; // SPI2 global interrupt
};

static spi2_t& SPI2 = *reinterpret_cast<spi2_t*>(0x40003800);

#define HAVE_PERIPHERAL_SPI2


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
    volatile uint32_t    CCMR1;                // [Read-write] capture/compare mode register 1 (output mode)
    volatile uint32_t    CCMR2;                // [Read-write] capture/compare mode register 2 (output mode)
    volatile uint32_t    CCER;                 // [Read-write] capture/compare enable register
    volatile uint32_t    CNT;                  // counter
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
    volatile uint32_t    OR1;                  // [Read-write] option register 1
    volatile uint32_t    CCMR3_Output;         // [Read-write] capture/compare mode register 2 (output mode)
    volatile uint32_t    CCR5;                 // [Read-write] capture/compare register 4
    volatile uint32_t    CCR6;                 // [Read-write] capture/compare register 4
    volatile uint32_t    AF1;                  // [Read-write] DMA address for full transfer
    volatile uint32_t    AF2;                  // [Read-write] DMA address for full transfer

    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_DIR = 0x10;           // Direction
    template<uint32_t X>
    static constexpr uint32_t CR1_CMS =                 // Center-aligned mode selection (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_UIFREMAP = 0x800;     // UIF status bit remapping
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_MMS2 =                // Master mode selection 2 (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    static constexpr uint32_t CR2_OIS6 = 0x40000;       // Output Idle state 6 (OC6 output)
    static constexpr uint32_t CR2_OIS5 = 0x10000;       // Output Idle state 5 (OC5 output)
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

    template<uint32_t X>
    static constexpr uint32_t SMCR_SMS =                 // Slave mode selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t SMCR_OCCS = 0x8;           // OCREF clear selection
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS_4 =                // Trigger selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t SMCR_MSM = 0x80;           // Master/Slave mode
    template<uint32_t X>
    static constexpr uint32_t SMCR_ETF =                 // External trigger filter (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMCR_ETPS =                // External trigger prescaler (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t SMCR_ECE = 0x4000;         // External clock enable
    static constexpr uint32_t SMCR_ETP = 0x8000;         // External trigger polarity
    static constexpr uint32_t SMCR_SMS_3 = 0x10000;      // Slave mode selection - bit 3
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS =                  // Trigger selection (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    static const uint32_t SMCR_RESET_VALUE = 0x0;

    static constexpr uint32_t DIER_UIE = 0x1;            // Update interrupt enable
    static constexpr uint32_t DIER_CC1IE = 0x2;          // Capture/Compare 1 interrupt enable
    static constexpr uint32_t DIER_CC2IE = 0x4;          // Capture/Compare 2 interrupt enable
    static constexpr uint32_t DIER_CC3IE = 0x8;          // Capture/Compare 3 interrupt enable
    static constexpr uint32_t DIER_CC4IE = 0x10;         // Capture/Compare 4 interrupt enable
    static constexpr uint32_t DIER_COMIE = 0x20;         // COM interrupt enable
    static constexpr uint32_t DIER_TIE = 0x40;           // Trigger interrupt enable
    static constexpr uint32_t DIER_BIE = 0x80;           // Break interrupt enable
    static constexpr uint32_t DIER_UDE = 0x100;          // Update DMA request enable
    static constexpr uint32_t DIER_CC1DE = 0x200;        // Capture/Compare 1 DMA request enable
    static constexpr uint32_t DIER_CC2DE = 0x400;        // Capture/Compare 2 DMA request enable
    static constexpr uint32_t DIER_CC3DE = 0x800;        // Capture/Compare 3 DMA request enable
    static constexpr uint32_t DIER_CC4DE = 0x1000;       // Capture/Compare 4 DMA request enable
    static constexpr uint32_t DIER_COMDE = 0x2000;       // COM DMA request enable
    static constexpr uint32_t DIER_TDE = 0x4000;         // Trigger DMA request enable
    static const uint32_t DIER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/Compare 2 interrupt flag
    static constexpr uint32_t SR_CC3IF = 0x8;          // Capture/Compare 3 interrupt flag
    static constexpr uint32_t SR_CC4IF = 0x10;         // Capture/Compare 4 interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_B2IF = 0x100;         // Break 2 interrupt flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/compare 2 overcapture flag
    static constexpr uint32_t SR_CC3OF = 0x800;        // Capture/Compare 3 overcapture flag
    static constexpr uint32_t SR_CC4OF = 0x1000;       // Capture/Compare 4 overcapture flag
    static constexpr uint32_t SR_SBIF = 0x2000;        // System Break interrupt flag
    static constexpr uint32_t SR_CC5IF = 0x10000;      // Compare 5 interrupt flag
    static constexpr uint32_t SR_CC6IF = 0x20000;      // Compare 6 interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_UG = 0x1;             // Update generation
    static constexpr uint32_t EGR_CC1G = 0x2;           // Capture/compare 1 generation
    static constexpr uint32_t EGR_CC2G = 0x4;           // Capture/compare 2 generation
    static constexpr uint32_t EGR_CC3G = 0x8;           // Capture/compare 3 generation
    static constexpr uint32_t EGR_CC4G = 0x10;          // Capture/compare 4 generation
    static constexpr uint32_t EGR_COMG = 0x20;          // Capture/Compare control update generation
    static constexpr uint32_t EGR_TG = 0x40;            // Trigger generation
    static constexpr uint32_t EGR_BG = 0x80;            // Break generation
    static constexpr uint32_t EGR_B2G = 0x100;          // Break 2 generation
    static const uint32_t EGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC1S =                // Capture/Compare 1 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_CC2S =                // Capture/Compare 2 selection (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CCMR1_OC1CE = 0x80;         // Output Compare 1 clear enable
    static constexpr uint32_t CCMR1_OC1FE = 0x4;          // Output Compare 1 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC1M =                // Output Compare 1 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC1M_3 = 0x10000;     // Output Compare 1 mode - bit 3
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output Compare 1 preload enable
    static constexpr uint32_t CCMR1_OC2CE = 0x8000;       // Output Compare 2 clear enable
    static constexpr uint32_t CCMR1_OC2FE = 0x400;        // Output Compare 2 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC2M =                // Output Compare 2 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC2M_3 = 0x1000000;   // Output Compare 2 mode - bit 3
    static constexpr uint32_t CCMR1_OC2PE = 0x800;        // Output Compare 2 preload enable
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCMR2_CC3S =                // Capture/Compare 3 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR2_CC4S =                // Capture/Compare 4 selection (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CCMR2_OC3CE = 0x80;         // Output compare 3 clear enable
    static constexpr uint32_t CCMR2_OC3FE = 0x4;          // Output compare 3 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC3M =                // Output compare 3 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC3M_3 = 0x10000;     // Output Compare 3 mode - bit 3
    static constexpr uint32_t CCMR2_OC3PE = 0x8;          // Output compare 3 preload enable
    static constexpr uint32_t CCMR2_OC4CE = 0x8000;       // Output compare 4 clear enable
    static constexpr uint32_t CCMR2_OC4FE = 0x400;        // Output compare 4 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC4M =                // Output compare 4 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC4M_3 = 0x1000000;   // Output Compare 4 mode - bit 3
    static constexpr uint32_t CCMR2_OC4PE = 0x800;        // Output compare 4 preload enable
    static const uint32_t CCMR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC1E = 0x1;           // Capture/Compare 1 output enable
    static constexpr uint32_t CCER_CC1P = 0x2;           // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1NE = 0x4;          // Capture/Compare 1 complementary output enable
    static constexpr uint32_t CCER_CC1NP = 0x8;          // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC2E = 0x10;          // Capture/Compare 2 output enable
    static constexpr uint32_t CCER_CC2P = 0x20;          // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC2NE = 0x40;         // Capture/Compare 2 complementary output enable
    static constexpr uint32_t CCER_CC2NP = 0x80;         // Capture/Compare 2 output Polarity
    static constexpr uint32_t CCER_CC3E = 0x100;         // Capture/Compare 3 output enable
    static constexpr uint32_t CCER_CC3P = 0x200;         // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC3NE = 0x400;        // Capture/Compare 3 complementary output enable
    static constexpr uint32_t CCER_CC3NP = 0x800;        // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC4E = 0x1000;        // Capture/Compare 4 output enable
    static constexpr uint32_t CCER_CC4P = 0x2000;        // Capture/Compare 3 output Polarity
    static constexpr uint32_t CCER_CC4NP = 0x8000;       // Capture/Compare 4 complementary output polarity
    static constexpr uint32_t CCER_CC5E = 0x10000;       // Capture/Compare 5 output enable
    static constexpr uint32_t CCER_CC5P = 0x20000;       // Capture/Compare 5 output polarity
    static constexpr uint32_t CCER_CC6E = 0x100000;      // Capture/Compare 6 output enable
    static constexpr uint32_t CCER_CC6P = 0x200000;      // Capture/Compare 6 output polarity
    static const uint32_t CCER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // counter value (16 bits), Read-write
        bit_field_t<0, 0xffff>::value<X>();
    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIF copy, Read-only
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
    static constexpr uint32_t RCR_REP =                 // Repetition counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
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

    template<uint32_t X>
    static constexpr uint32_t BDTR_DTG =                 // Dead-time generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_LOCK =                // Lock configuration (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t BDTR_OSSI = 0x400;         // Off-state selection for Idle mode
    static constexpr uint32_t BDTR_OSSR = 0x800;         // Off-state selection for Run mode
    static constexpr uint32_t BDTR_BKE = 0x1000;         // Break enable
    static constexpr uint32_t BDTR_BKP = 0x2000;         // Break polarity
    static constexpr uint32_t BDTR_AOE = 0x4000;         // Automatic output enable
    static constexpr uint32_t BDTR_MOE = 0x8000;         // Main output enable
    template<uint32_t X>
    static constexpr uint32_t BDTR_BKF =                 // Break filter (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_BK2F =                // Break 2 filter (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    static constexpr uint32_t BDTR_BK2E = 0x1000000;     // Break 2 enable
    static constexpr uint32_t BDTR_BK2P = 0x2000000;     // Break 2 polarity
    static constexpr uint32_t BDTR_BKDSRM = 0x4000000;   // Break Disarm
    static constexpr uint32_t BDTR_BK2DSRM = 0x8000000;  // Break2 Disarm
    static constexpr uint32_t BDTR_BKBID = 0x10000000;   // Break Bidirectional
    static constexpr uint32_t BDTR_BK2ID = 0x20000000;   // Break2 bidirectional
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

    static constexpr uint32_t OR1_OCREF_CLR = 0x1;      // Ocref_clr source selection
    static const uint32_t OR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCMR3_Output_OC6M_bit3 = 0x1000000;// Output Compare 6 mode bit 3
    static constexpr uint32_t CCMR3_Output_OC5M_bit3 = 0x10000;  // Output Compare 5 mode bit 3
    static constexpr uint32_t CCMR3_Output_OC6CE = 0x8000;       // Output compare 6 clear enable
    template<uint32_t X>
    static constexpr uint32_t CCMR3_Output_OC6M =                // Output compare 6 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR3_Output_OC6PE = 0x800;        // Output compare 6 preload enable
    static constexpr uint32_t CCMR3_Output_OC6FE = 0x400;        // Output compare 6 fast enable
    static constexpr uint32_t CCMR3_Output_OC5CE = 0x80;         // Output compare 5 clear enable
    template<uint32_t X>
    static constexpr uint32_t CCMR3_Output_OC5M =                // Output compare 5 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR3_Output_OC5PE = 0x8;          // Output compare 5 preload enable
    static constexpr uint32_t CCMR3_Output_OC5FE = 0x4;          // Output compare 5 fast enable
    static const uint32_t CCMR3_Output_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR5_CCR5 =                // Capture/Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static constexpr uint32_t CCR5_GC5C1 = 0x20000000;   // Group Channel 5 and Channel 1
    static constexpr uint32_t CCR5_GC5C2 = 0x40000000;   // Group Channel 5 and Channel 2
    static constexpr uint32_t CCR5_GC5C3 = 0x80000000;   // Group Channel 5 and Channel 3
    static const uint32_t CCR5_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR6_CCR6 =                // Capture/Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR6_RESET_VALUE = 0x0;

    static constexpr uint32_t AF1_BKINE = 0x1;          // BRK BKIN input enable
    static constexpr uint32_t AF1_BKCMP1E = 0x2;        // BRK COMP1 enable
    static constexpr uint32_t AF1_BKCMP2E = 0x4;        // BRK COMP2 enable
    static constexpr uint32_t AF1_BKINP = 0x200;        // BRK BKIN input polarity
    static constexpr uint32_t AF1_BKCMP1P = 0x400;      // BRK COMP1 input polarity
    static constexpr uint32_t AF1_BKCMP2P = 0x800;      // BRK COMP2 input polarity
    template<uint32_t X>
    static constexpr uint32_t AF1_ETRSEL =              // ETR source selection (3 bits)
        bit_field_t<14, 0x7>::value<X>();
    static const uint32_t AF1_RESET_VALUE = 0x1;

    static constexpr uint32_t AF2_BK2INE = 0x1;         // BRK2 BKIN input enable
    static constexpr uint32_t AF2_BK2CMP1E = 0x2;       // BRK2 COMP1 enable
    static constexpr uint32_t AF2_BK2CMP2E = 0x4;       // BRK2 COMP2 enable
    static constexpr uint32_t AF2_BK2DFBK0E = 0x100;    // BRK2 DFSDM_BREAK0 enable
    static constexpr uint32_t AF2_BK2INP = 0x200;       // BRK2 BKIN input polarity
    static constexpr uint32_t AF2_BK2CMP1P = 0x400;     // BRK2 COMP1 input polarity
    static constexpr uint32_t AF2_BK2CMP2P = 0x800;     // BRK2 COMP2 input polarity
    static const uint32_t AF2_RESET_VALUE = 0x1;

    static constexpr uint8_t TIM1_BRK_UP_TRG_COMP = 13; // TIM1 break, update, trigger
    static constexpr uint8_t TIM1_CC = 14; // TIM1 Capture Compare interrupt
};

static tim1_t& TIM1 = *reinterpret_cast<tim1_t*>(0x40012c00);

#define HAVE_PERIPHERAL_TIM1


////
//
//    Analog to Digital Converter instance 1
//
////

struct adc_t
{
    volatile uint32_t    ISR;                  // [Read-write] ADC interrupt and status register
    volatile uint32_t    IER;                  // [Read-write] ADC interrupt enable register
    volatile uint32_t    CR;                   // [Read-write] ADC control register
    volatile uint32_t    CFGR1;                // [Read-write] ADC configuration register 1
    volatile uint32_t    CFGR2;                // [Read-write] ADC configuration register 2
    volatile uint32_t    SMPR;                 // [Read-write] ADC sampling time register
    reserved_t<2>        _0;
    volatile uint32_t    AWD1TR;               // [Read-write] watchdog threshold register
    volatile uint32_t    AWD2TR;               // [Read-write] watchdog threshold register
    volatile uint32_t    CHSELR;               // [Read-write] channel selection register
    volatile uint32_t    AWD3TR;               // [Read-write] watchdog threshold register
    reserved_t<4>        _1;
    volatile uint32_t    DR;                   // [Read-only] ADC group regular conversion data register
    reserved_t<23>       _2;
    volatile uint32_t    AWD2CR;               // [Read-write] ADC analog watchdog 2 configuration register
    volatile uint32_t    AWD3CR;               // [Read-write] ADC analog watchdog 3 configuration register
    reserved_t<3>        _3;
    volatile uint32_t    CALFACT;              // [Read-write] ADC calibration factors register
    reserved_t<148>      _4;
    volatile uint32_t    CCR;                  // [Read-write] ADC common control register
    reserved_t<51>       _5;
    volatile uint32_t    HWCFGR6;              // [Read-write] Hardware Configuration Register
    volatile uint32_t    HWCFGR5;              // [Read-write] Hardware Configuration Register
    volatile uint32_t    HWCFGR4;              // [Read-write] Hardware Configuration Register
    volatile uint32_t    HWCFGR3;              // [Read-write] Hardware Configuration Register
    volatile uint32_t    HWCFGR2;              // [Read-write] Hardware Configuration Register
    volatile uint32_t    HWCFGR1;              // [Read-write] Hardware Configuration Register
    volatile uint32_t    HWCFGR0;              // [Read-only] Hardware Configuration Register
    volatile uint32_t    VERR;                 // [Read-only] EXTI IP Version register
    volatile uint32_t    IPIDR;                // [Read-only] EXTI Identification register
    volatile uint32_t    SIDR;                 // [Read-only] EXTI Size ID register

    static constexpr uint32_t ISR_CCRDY = 0x2000;       // Channel Configuration Ready flag
    static constexpr uint32_t ISR_EOCAL = 0x800;        // End Of Calibration flag
    static constexpr uint32_t ISR_AWD3 = 0x200;         // ADC analog watchdog 3 flag
    static constexpr uint32_t ISR_AWD2 = 0x100;         // ADC analog watchdog 2 flag
    static constexpr uint32_t ISR_AWD1 = 0x80;          // ADC analog watchdog 1 flag
    static constexpr uint32_t ISR_OVR = 0x10;           // ADC group regular overrun flag
    static constexpr uint32_t ISR_EOS = 0x8;            // ADC group regular end of sequence conversions flag
    static constexpr uint32_t ISR_EOC = 0x4;            // ADC group regular end of unitary conversion flag
    static constexpr uint32_t ISR_EOSMP = 0x2;          // ADC group regular end of sampling flag
    static constexpr uint32_t ISR_ADRDY = 0x1;          // ADC ready flag
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t IER_CCRDYIE = 0x2000;     // Channel Configuration Ready Interrupt enable
    static constexpr uint32_t IER_EOCALIE = 0x800;      // End of calibration interrupt enable
    static constexpr uint32_t IER_AWD3IE = 0x200;       // ADC analog watchdog 3 interrupt
    static constexpr uint32_t IER_AWD2IE = 0x100;       // ADC analog watchdog 2 interrupt
    static constexpr uint32_t IER_AWD1IE = 0x80;        // ADC analog watchdog 1 interrupt
    static constexpr uint32_t IER_OVRIE = 0x10;         // ADC group regular overrun interrupt
    static constexpr uint32_t IER_EOSIE = 0x8;          // ADC group regular end of sequence conversions interrupt
    static constexpr uint32_t IER_EOCIE = 0x4;          // ADC group regular end of unitary conversion interrupt
    static constexpr uint32_t IER_EOSMPIE = 0x2;        // ADC group regular end of sampling interrupt
    static constexpr uint32_t IER_ADRDYIE = 0x1;        // ADC ready interrupt
    static const uint32_t IER_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_ADCAL = 0x80000000;   // ADC calibration
    static constexpr uint32_t CR_ADVREGEN = 0x10000000;// ADC voltage regulator enable
    static constexpr uint32_t CR_ADSTP = 0x10;         // ADC group regular conversion stop
    static constexpr uint32_t CR_ADSTART = 0x4;        // ADC group regular conversion start
    static constexpr uint32_t CR_ADDIS = 0x2;          // ADC disable
    static constexpr uint32_t CR_ADEN = 0x1;           // ADC enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CFGR1_AWDCH1CH =            // ADC analog watchdog 1 monitored channel selection (5 bits)
        bit_field_t<26, 0x1f>::value<X>();
    static constexpr uint32_t CFGR1_AWD1EN = 0x800000;    // ADC analog watchdog 1 enable on scope ADC group regular
    static constexpr uint32_t CFGR1_AWD1SGL = 0x400000;   // ADC analog watchdog 1 monitoring a single channel or all channels
    static constexpr uint32_t CFGR1_CHSELRMOD = 0x200000; // Mode selection of the ADC_CHSELR register
    static constexpr uint32_t CFGR1_DISCEN = 0x10000;     // ADC group regular sequencer discontinuous mode
    static constexpr uint32_t CFGR1_AUTOFF = 0x8000;      // Auto-off mode
    static constexpr uint32_t CFGR1_WAIT = 0x4000;        // Wait conversion mode
    static constexpr uint32_t CFGR1_CONT = 0x2000;        // ADC group regular continuous conversion mode
    static constexpr uint32_t CFGR1_OVRMOD = 0x1000;      // ADC group regular overrun configuration
    template<uint32_t X>
    static constexpr uint32_t CFGR1_EXTEN =               // ADC group regular external trigger polarity (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR1_EXTSEL =              // ADC group regular external trigger source (3 bits)
        bit_field_t<6, 0x7>::value<X>();
    static constexpr uint32_t CFGR1_ALIGN = 0x20;         // ADC data alignement
    template<uint32_t X>
    static constexpr uint32_t CFGR1_RES =                 // ADC data resolution (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t CFGR1_SCANDIR = 0x4;        // Scan sequence direction
    static constexpr uint32_t CFGR1_DMACFG = 0x2;         // ADC DMA transfer configuration
    static constexpr uint32_t CFGR1_DMAEN = 0x1;          // ADC DMA transfer enable
    static const uint32_t CFGR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CFGR2_CKMODE =              // ADC clock mode (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static constexpr uint32_t CFGR2_LFTRIG = 0x20000000;  // Low frequency trigger mode enable
    static constexpr uint32_t CFGR2_TOVS = 0x200;         // ADC oversampling discontinuous mode (triggered mode) for ADC group regular
    template<uint32_t X>
    static constexpr uint32_t CFGR2_OVSS =                // ADC oversampling shift (4 bits)
        bit_field_t<5, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR2_OVSR =                // ADC oversampling ratio (3 bits)
        bit_field_t<2, 0x7>::value<X>();
    static constexpr uint32_t CFGR2_OVSE = 0x1;           // ADC oversampler enable on scope ADC group regular
    static const uint32_t CFGR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SMPR_SMP1 =                // Sampling time selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR_SMP2 =                // Sampling time selection (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR_SMPSEL =              // Channel sampling time selection (19 bits)
        bit_field_t<8, 0x7ffff>::value<X>();
    static const uint32_t SMPR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AWD1TR_HT1 =                 // ADC analog watchdog 1 threshold high (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AWD1TR_LT1 =                 // ADC analog watchdog 1 threshold low (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t AWD1TR_RESET_VALUE = 0xfff0000;

    template<uint32_t X>
    static constexpr uint32_t AWD2TR_HT2 =                 // ADC analog watchdog 2 threshold high (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AWD2TR_LT2 =                 // ADC analog watchdog 2 threshold low (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t AWD2TR_RESET_VALUE = 0xfff0000;

    template<uint32_t X>
    static constexpr uint32_t CHSELR_CHSEL =               // Channel-x selection (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CHSELR_SQ1 =                 // conversion of the sequence (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CHSELR_SQ2 =                 // conversion of the sequence (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CHSELR_SQ3 =                 // conversion of the sequence (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CHSELR_SQ4 =                 // conversion of the sequence (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CHSELR_SQ5 =                 // conversion of the sequence (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CHSELR_SQ6 =                 // conversion of the sequence (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CHSELR_SQ7 =                 // conversion of the sequence (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CHSELR_SQ8 =                 // conversion of the sequence (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    static const uint32_t CHSELR_RESET_VALUE = 0xfff0000;

    template<uint32_t X>
    static constexpr uint32_t AWD3TR_HT3 =                 // ADC analog watchdog 3 threshold high (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AWD3TR_LT3 =                 // ADC analog watchdog 3 threshold high (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t AWD3TR_RESET_VALUE = 0xfff0000;

    template<uint32_t X>
    static constexpr uint32_t DR_regularDATA =         // ADC group regular conversion data (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AWD2CR_AWD2CH =              // ADC analog watchdog 2 monitored channel selection (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    static const uint32_t AWD2CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AWD3CR_AWD3CH =              // ADC analog watchdog 3 monitored channel selection (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    static const uint32_t AWD3CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CALFACT_CALFACT =             // ADC calibration factor in single-ended mode (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t CALFACT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR_PRESC =               // ADC prescaler (4 bits)
        bit_field_t<18, 0xf>::value<X>();
    static constexpr uint32_t CCR_VREFEN = 0x400000;    // VREFINT enable
    static constexpr uint32_t CCR_TSEN = 0x800000;      // Temperature sensor enable
    static constexpr uint32_t CCR_VBATEN = 0x1000000;   // VBAT enable
    static const uint32_t CCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR6_CHMAP20 =             // Input channel mapping (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR6_CHMAP21 =             // Input channel mapping (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR6_CHMAP22 =             // Input channel mapping (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR6_CHMAP23 =             // Input channel mapping (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t HWCFGR6_RESET_VALUE = 0x1f1f1f1f;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR5_CHMAP19 =             // Input channel mapping (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR5_CHMAP18 =             // Input channel mapping (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR5_CHMAP17 =             // Input channel mapping (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR5_CHMAP16 =             // Input channel mapping (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t HWCFGR5_RESET_VALUE = 0x1f080807;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR4_CHMAP15 =             // Input channel mapping (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR4_CHMAP14 =             // Input channel mapping (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR4_CHMAP13 =             // Input channel mapping (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR4_CHMAP12 =             // Input channel mapping (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t HWCFGR4_RESET_VALUE = 0x70b0a09;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR3_CHMAP11 =             // Input channel mapping (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR3_CHMAP10 =             // Input channel mapping (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR3_CHMAP9 =              // Input channel mapping (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR3_CHMAP8 =              // Input channel mapping (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t HWCFGR3_RESET_VALUE = 0x7060605;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR2_CHMAP7 =              // Input channel mapping (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR2_CHMAP6 =              // Input channel mapping (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR2_CHMAP5 =              // Input channel mapping (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR2_CHMAP4 =              // Input channel mapping (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t HWCFGR2_RESET_VALUE = 0x5050404;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CHMAP3 =              // Input channel mapping (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CHMAP2 =              // Input channel mapping (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CHMAP1 =              // Input channel mapping (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CHMAP0 =              // Input channel mapping (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t HWCFGR1_RESET_VALUE = 0x3020100;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR0_NUM_CHAN_24 =         // NUM_CHAN_24 (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR0_EXTRA_AWDS =          // Extra analog watchdog (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR0_OVS =                 // Oversampling (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static const uint32_t HWCFGR0_RESET_VALUE = 0x110;

    template<uint32_t X>
    static constexpr uint32_t VERR_MINREV =              // Minor Revision number (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t VERR_MAJREV =              // Major Revision number (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    static const uint32_t VERR_RESET_VALUE = 0x0;


    static const uint32_t IPIDR_RESET_VALUE = 0x0;


    static const uint32_t SIDR_RESET_VALUE = 0x0;

    static constexpr uint8_t ADC_COMP = 12; // ADC and COMP interrupts
};

static adc_t& ADC = *reinterpret_cast<adc_t*>(0x40012400);

#define HAVE_PERIPHERAL_ADC


////
//
//    COMP1
//
////

struct comp_t
{
    volatile uint32_t    COMP1_CSR;            // [Read-write] Comparator 1 control and status register
    volatile uint32_t    COMP2_CSR;            // [Read-write] Comparator 2 control and status register

    static constexpr uint32_t COMP1_CSR_EN = 0x1;             // COMP channel 1 enable bit
    template<uint32_t X>
    static constexpr uint32_t COMP1_CSR_INMSEL =              // Comparator 2 signal selector for inverting input INM (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP1_CSR_INPSEL =              // Comparator 2 signal selector for non-inverting input (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t COMP1_CSR_WINMODE = 0x800;      // Comparator 2 non-inverting input selector for window mode
    static constexpr uint32_t COMP1_CSR_WINOUT = 0x4000;      // Comparator 2 output selector
    static constexpr uint32_t COMP1_CSR_POLARITY = 0x8000;    // Comparator 2 polarity selector
    template<uint32_t X>
    static constexpr uint32_t COMP1_CSR_HYST =                // Comparator 2 hysteresis selector (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP1_CSR_PWRMODE =             // Comparator 2 power mode selector (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP1_CSR_BLANKSEL =            // Comparator 2 blanking source selector (5 bits)
        bit_field_t<20, 0x1f>::value<X>();
    static constexpr uint32_t COMP1_CSR_VALUE = 0x40000000;   // Comparator 2 output status
    static constexpr uint32_t COMP1_CSR_LOCK = 0x80000000;    // COMP2_CSR register lock
    static const uint32_t COMP1_CSR_RESET_VALUE = 0x0;

    static constexpr uint32_t COMP2_CSR_EN = 0x1;             // COMP channel 1 enable bit
    template<uint32_t X>
    static constexpr uint32_t COMP2_CSR_INMSEL =              // Comparator 2 signal selector for inverting input INM (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP2_CSR_INPSEL =              // Comparator 2 signal selector for non-inverting input (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t COMP2_CSR_WINMODE = 0x800;      // Comparator 2 non-inverting input selector for window mode
    static constexpr uint32_t COMP2_CSR_WINOUT = 0x4000;      // Comparator 2 output selector
    static constexpr uint32_t COMP2_CSR_POLARITY = 0x8000;    // Comparator 2 polarity selector
    template<uint32_t X>
    static constexpr uint32_t COMP2_CSR_HYST =                // Comparator 2 hysteresis selector (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP2_CSR_PWRMODE =             // Comparator 2 power mode selector (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP2_CSR_BLANKSEL =            // Comparator 2 blanking source selector (5 bits)
        bit_field_t<20, 0x1f>::value<X>();
    static constexpr uint32_t COMP2_CSR_VALUE = 0x40000000;   // Comparator 2 output status
    static constexpr uint32_t COMP2_CSR_LOCK = 0x80000000;    // COMP2_CSR register lock
    static const uint32_t COMP2_CSR_RESET_VALUE = 0x0;
};

static comp_t& COMP = *reinterpret_cast<comp_t*>(0x40010200);

#define HAVE_PERIPHERAL_COMP


////
//
//    System configuration controller
//
////

struct syscfg_vrefbuf_t
{
    volatile uint32_t    CFGR1;                // [Read-write] SYSCFG configuration register 1
    reserved_t<5>        _0;
    volatile uint32_t    CFGR2;                // [Read-write] SYSCFG configuration register 1
    reserved_t<5>        _1;
    volatile uint32_t    VREFBUF_CSR;          // VREFBUF control and status register
    volatile uint32_t    VREFBUF_CCR;          // [Read-write] VREFBUF calibration control register
    reserved_t<18>       _2;
    volatile uint32_t    ITLINE0;              // [Read-only] interrupt line 0 status register
    volatile uint32_t    ITLINE1;              // [Read-only] interrupt line 1 status register
    volatile uint32_t    ITLINE2;              // [Read-only] interrupt line 2 status register
    volatile uint32_t    ITLINE3;              // [Read-only] interrupt line 3 status register
    volatile uint32_t    ITLINE4;              // [Read-only] interrupt line 4 status register
    volatile uint32_t    ITLINE5;              // [Read-only] interrupt line 5 status register
    volatile uint32_t    ITLINE6;              // [Read-only] interrupt line 6 status register
    volatile uint32_t    ITLINE7;              // [Read-only] interrupt line 7 status register
    volatile uint32_t    ITLINE8;              // [Read-only] interrupt line 8 status register
    volatile uint32_t    ITLINE9;              // [Read-only] interrupt line 9 status register
    volatile uint32_t    ITLINE10;             // [Read-only] interrupt line 10 status register
    volatile uint32_t    ITLINE11;             // [Read-only] interrupt line 11 status register
    volatile uint32_t    ITLINE12;             // [Read-only] interrupt line 12 status register
    volatile uint32_t    ITLINE13;             // [Read-only] interrupt line 13 status register
    volatile uint32_t    ITLINE14;             // [Read-only] interrupt line 14 status register
    volatile uint32_t    ITLINE15;             // [Read-only] interrupt line 15 status register
    volatile uint32_t    ITLINE16;             // [Read-only] interrupt line 16 status register
    volatile uint32_t    ITLINE17;             // [Read-only] interrupt line 17 status register
    volatile uint32_t    ITLINE18;             // [Read-only] interrupt line 18 status register
    volatile uint32_t    ITLINE19;             // [Read-only] interrupt line 19 status register
    volatile uint32_t    ITLINE20;             // [Read-only] interrupt line 20 status register
    volatile uint32_t    ITLINE21;             // [Read-only] interrupt line 21 status register
    volatile uint32_t    ITLINE22;             // [Read-only] interrupt line 22 status register
    volatile uint32_t    ITLINE23;             // [Read-only] interrupt line 23 status register
    volatile uint32_t    ITLINE24;             // [Read-only] interrupt line 24 status register
    volatile uint32_t    ITLINE25;             // [Read-only] interrupt line 25 status register
    volatile uint32_t    ITLINE26;             // [Read-only] interrupt line 26 status register
    volatile uint32_t    ITLINE27;             // [Read-only] interrupt line 27 status register
    volatile uint32_t    ITLINE28;             // [Read-only] interrupt line 28 status register
    volatile uint32_t    ITLINE29;             // [Read-only] interrupt line 29 status register
    volatile uint32_t    ITLINE30;             // [Read-only] interrupt line 30 status register
    volatile uint32_t    ITLINE31;             // [Read-only] interrupt line 31 status register

    template<uint32_t X>
    static constexpr uint32_t CFGR1_I2C_PAx_FMP =         // Fast Mode Plus (FM+) driving capability activation bits (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    static constexpr uint32_t CFGR1_I2C2_FMP = 0x200000;  // FM+ driving capability activation for I2C2
    static constexpr uint32_t CFGR1_I2C1_FMP = 0x100000;  // FM+ driving capability activation for I2C1
    template<uint32_t X>
    static constexpr uint32_t CFGR1_I2C_PBx_FMP =         // Fast Mode Plus (FM+) driving capability activation bits (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    static constexpr uint32_t CFGR1_UCPD2_STROBE = 0x400; // Strobe signal bit for UCPD2
    static constexpr uint32_t CFGR1_UCPD1_STROBE = 0x200; // Strobe signal bit for UCPD1
    static constexpr uint32_t CFGR1_BOOSTEN = 0x100;      // I/O analog switch voltage booster enable
    template<uint32_t X>
    static constexpr uint32_t CFGR1_IR_MOD =              // IR Modulation Envelope signal selection. (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    static constexpr uint32_t CFGR1_IR_POL = 0x20;        // IR output polarity selection
    static constexpr uint32_t CFGR1_PA11_PA12_RMP = 0x10; // PA11 and PA12 remapping bit.
    template<uint32_t X>
    static constexpr uint32_t CFGR1_MEM_MODE =            // Memory mapping selection bits (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t CFGR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CFGR2_LOCKUP_LOCK = 0x1;    // Cortex-M0+ LOCKUP bit enable bit
    static constexpr uint32_t CFGR2_SRAM_PARITY_LOCK = 0x2;// SRAM parity lock bit
    static constexpr uint32_t CFGR2_PVD_LOCK = 0x4;       // PVD lock enable bit
    static constexpr uint32_t CFGR2_ECC_LOCK = 0x8;       // ECC error lock bit
    static constexpr uint32_t CFGR2_SRAM_PEF = 0x100;     // SRAM parity error flag
    static const uint32_t CFGR2_RESET_VALUE = 0x0;

    static constexpr uint32_t VREFBUF_CSR_ENVR = 0x1;           // Voltage reference buffer mode enable This bit is used to enable the voltage reference buffer mode., Read-write
    static constexpr uint32_t VREFBUF_CSR_HIZ = 0x2;            // High impedance mode This bit controls the analog switch to connect or not the VREF+ pin. Refer to Table196: VREF buffer modes for the mode descriptions depending on ENVR bit configuration., Read-write
    static constexpr uint32_t VREFBUF_CSR_VRR = 0x8;            // Voltage reference buffer ready, Read-only
    template<uint32_t X>
    static constexpr uint32_t VREFBUF_CSR_VRS =                 // Voltage reference scale These bits select the value generated by the voltage reference buffer. Other: Reserved (3 bits), Read-write
        bit_field_t<4, 0x7>::value<X>();
    static const uint32_t VREFBUF_CSR_RESET_VALUE = 0x2;

    template<uint32_t X>
    static constexpr uint32_t VREFBUF_CCR_TRIM =                // Trimming code These bits are automatically initialized after reset with the trimming value stored in the Flash memory during the production test. Writing into these bits allows to tune the internal reference buffer voltage. (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    static const uint32_t VREFBUF_CCR_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE0_WWDG = 0x1;           // Window watchdog interrupt pending flag
    static const uint32_t ITLINE0_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE1_PVDOUT = 0x1;         // PVD supply monitoring interrupt request pending (EXTI line 16).
    static const uint32_t ITLINE1_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE2_TAMP = 0x1;           // TAMP
    static constexpr uint32_t ITLINE2_RTC = 0x2;            // RTC
    static const uint32_t ITLINE2_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE3_FLASH_ITF = 0x1;      // FLASH_ITF
    static constexpr uint32_t ITLINE3_FLASH_ECC = 0x2;      // FLASH_ECC
    static const uint32_t ITLINE3_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE4_RCC = 0x1;            // RCC
    static const uint32_t ITLINE4_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE5_EXTI0 = 0x1;          // EXTI0
    static constexpr uint32_t ITLINE5_EXTI1 = 0x2;          // EXTI1
    static const uint32_t ITLINE5_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE6_EXTI2 = 0x1;          // EXTI2
    static constexpr uint32_t ITLINE6_EXTI3 = 0x2;          // EXTI3
    static const uint32_t ITLINE6_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE7_EXTI4 = 0x1;          // EXTI4
    static constexpr uint32_t ITLINE7_EXTI5 = 0x2;          // EXTI5
    static constexpr uint32_t ITLINE7_EXTI6 = 0x4;          // EXTI6
    static constexpr uint32_t ITLINE7_EXTI7 = 0x8;          // EXTI7
    static constexpr uint32_t ITLINE7_EXTI8 = 0x10;         // EXTI8
    static constexpr uint32_t ITLINE7_EXTI9 = 0x20;         // EXTI9
    static constexpr uint32_t ITLINE7_EXTI10 = 0x40;        // EXTI10
    static constexpr uint32_t ITLINE7_EXTI11 = 0x80;        // EXTI11
    static constexpr uint32_t ITLINE7_EXTI12 = 0x100;       // EXTI12
    static constexpr uint32_t ITLINE7_EXTI13 = 0x200;       // EXTI13
    static constexpr uint32_t ITLINE7_EXTI14 = 0x400;       // EXTI14
    static constexpr uint32_t ITLINE7_EXTI15 = 0x800;       // EXTI15
    static const uint32_t ITLINE7_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE8_UCPD1 = 0x1;          // UCPD1
    static constexpr uint32_t ITLINE8_UCPD2 = 0x2;          // UCPD2
    static const uint32_t ITLINE8_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE9_DMA1_CH1 = 0x1;       // DMA1_CH1
    static const uint32_t ITLINE9_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE10_DMA1_CH2 = 0x1;       // DMA1_CH1
    static constexpr uint32_t ITLINE10_DMA1_CH3 = 0x2;       // DMA1_CH3
    static const uint32_t ITLINE10_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE11_DMAMUX = 0x1;         // DMAMUX
    static constexpr uint32_t ITLINE11_DMA1_CH4 = 0x2;       // DMA1_CH4
    static constexpr uint32_t ITLINE11_DMA1_CH5 = 0x4;       // DMA1_CH5
    static constexpr uint32_t ITLINE11_DMA1_CH6 = 0x8;       // DMA1_CH6
    static constexpr uint32_t ITLINE11_DMA1_CH7 = 0x10;      // DMA1_CH7
    static const uint32_t ITLINE11_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE12_ADC = 0x1;            // ADC
    static constexpr uint32_t ITLINE12_COMP1 = 0x2;          // COMP1
    static constexpr uint32_t ITLINE12_COMP2 = 0x4;          // COMP2
    static const uint32_t ITLINE12_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE13_TIM1_CCU = 0x1;       // TIM1_CCU
    static constexpr uint32_t ITLINE13_TIM1_TRG = 0x2;       // TIM1_TRG
    static constexpr uint32_t ITLINE13_TIM1_UPD = 0x4;       // TIM1_UPD
    static constexpr uint32_t ITLINE13_TIM1_BRK = 0x8;       // TIM1_BRK
    static const uint32_t ITLINE13_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE14_TIM1_CC = 0x1;        // TIM1_CC
    static const uint32_t ITLINE14_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE15_TIM2 = 0x1;           // TIM2
    static const uint32_t ITLINE15_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE16_TIM3 = 0x1;           // TIM3
    static const uint32_t ITLINE16_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE17_TIM6 = 0x1;           // TIM6
    static constexpr uint32_t ITLINE17_DAC = 0x2;            // DAC
    static constexpr uint32_t ITLINE17_LPTIM1 = 0x4;         // LPTIM1
    static const uint32_t ITLINE17_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE18_TIM7 = 0x1;           // TIM7
    static constexpr uint32_t ITLINE18_LPTIM2 = 0x2;         // LPTIM2
    static const uint32_t ITLINE18_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE19_TIM14 = 0x1;          // TIM14
    static const uint32_t ITLINE19_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE20_TIM15 = 0x1;          // TIM15
    static const uint32_t ITLINE20_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE21_TIM16 = 0x1;          // TIM16
    static const uint32_t ITLINE21_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE22_TIM17 = 0x1;          // TIM17
    static const uint32_t ITLINE22_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE23_I2C1 = 0x1;           // I2C1
    static const uint32_t ITLINE23_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE24_I2C2 = 0x1;           // I2C2
    static const uint32_t ITLINE24_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE25_SPI1 = 0x1;           // SPI1
    static const uint32_t ITLINE25_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE26_SPI2 = 0x1;           // SPI2
    static const uint32_t ITLINE26_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE27_USART1 = 0x1;         // USART1
    static const uint32_t ITLINE27_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE28_USART2 = 0x1;         // USART2
    static const uint32_t ITLINE28_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE29_USART3 = 0x1;         // USART3
    static constexpr uint32_t ITLINE29_USART4 = 0x2;         // USART4
    static constexpr uint32_t ITLINE29_USART5 = 0x4;         // USART5
    static const uint32_t ITLINE29_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE30_USART2 = 0x1;         // CEC
    static const uint32_t ITLINE30_RESET_VALUE = 0x0;

    static constexpr uint32_t ITLINE31_RNG = 0x1;            // RNG
    static constexpr uint32_t ITLINE31_AES = 0x2;            // AES
    static const uint32_t ITLINE31_RESET_VALUE = 0x0;
};

static syscfg_vrefbuf_t& SYSCFG_VREFBUF = *reinterpret_cast<syscfg_vrefbuf_t*>(0x40010000);

#define HAVE_PERIPHERAL_SYSCFG_VREFBUF


////
//
//    Tamper and backup registers
//
////

struct tamp_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    volatile uint32_t    CR2;                  // [Read-write] control register 2
    reserved_t<1>        _0;
    volatile uint32_t    FLTCR;                // [Read-write] TAMP filter control register
    reserved_t<7>        _1;
    volatile uint32_t    IER;                  // [Read-write] TAMP interrupt enable register
    volatile uint32_t    SR;                   // [Read-only] TAMP status register
    volatile uint32_t    MISR;                 // [Read-only] TAMP masked interrupt status register
    reserved_t<1>        _2;
    volatile uint32_t    SCR;                  // [Write-only] TAMP status clear register
    reserved_t<48>       _3;
    volatile uint32_t    BKP0R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP1R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP2R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP3R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP4R;                // [Read-write] TAMP backup register
    reserved_t<182>      _4;
    volatile uint32_t    HWCFGR2;              // [Read-only] TAMP hardware configuration register 2
    volatile uint32_t    HWCFGR1;              // [Read-only] TAMP hardware configuration register 1
    volatile uint32_t    VERR;                 // [Read-only] EXTI IP Version register
    volatile uint32_t    IPIDR;                // [Read-only] EXTI Identification register
    volatile uint32_t    SIDR;                 // [Read-only] EXTI Size ID register

    static constexpr uint32_t CR1_TAMP1E = 0x1;         // TAMP1E
    static constexpr uint32_t CR1_TAMP2E = 0x2;         // TAMP2E
    static constexpr uint32_t CR1_ITAMP1E = 0x10000;    // ITAMP1E
    static constexpr uint32_t CR1_ITAMP3E = 0x40000;    // ITAMP3E
    static constexpr uint32_t CR1_ITAMP4E = 0x80000;    // ITAMP4E
    static constexpr uint32_t CR1_ITAMP5E = 0x100000;   // ITAMP5E
    static constexpr uint32_t CR1_ITAMP6E = 0x200000;   // ITAMP6E
    static const uint32_t CR1_RESET_VALUE = 0xffff0000;

    static constexpr uint32_t CR2_TAMP1NOER = 0x1;      // TAMP1NOER
    static constexpr uint32_t CR2_TAMP2NOER = 0x2;      // TAMP2NOER
    static constexpr uint32_t CR2_TAMP1MSK = 0x10000;   // TAMP1MSK
    static constexpr uint32_t CR2_TAMP2MSK = 0x20000;   // TAMP2MSK
    static constexpr uint32_t CR2_TAMP1TRG = 0x1000000; // TAMP1TRG
    static constexpr uint32_t CR2_TAMP2TRG = 0x2000000; // TAMP2TRG
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FLTCR_TAMPFREQ =            // TAMPFREQ (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FLTCR_TAMPFLT =             // TAMPFLT (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FLTCR_TAMPPRCH =            // TAMPPRCH (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t FLTCR_TAMPPUDIS = 0x80;     // TAMPPUDIS
    static const uint32_t FLTCR_RESET_VALUE = 0x0;

    static constexpr uint32_t IER_TAMP1IE = 0x1;        // TAMP1IE
    static constexpr uint32_t IER_TAMP2IE = 0x2;        // TAMP2IE
    static constexpr uint32_t IER_ITAMP1IE = 0x10000;   // ITAMP1IE
    static constexpr uint32_t IER_ITAMP3IE = 0x40000;   // ITAMP3IE
    static constexpr uint32_t IER_ITAMP4IE = 0x80000;   // ITAMP4IE
    static constexpr uint32_t IER_ITAMP5IE = 0x100000;  // ITAMP5IE
    static constexpr uint32_t IER_ITAMP6IE = 0x200000;  // ITAMP6IE
    static const uint32_t IER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_TAMP1F = 0x1;         // TAMP1F
    static constexpr uint32_t SR_TAMP2F = 0x2;         // TAMP2F
    static constexpr uint32_t SR_ITAMP1F = 0x10000;    // ITAMP1F
    static constexpr uint32_t SR_ITAMP3F = 0x40000;    // ITAMP3F
    static constexpr uint32_t SR_ITAMP4F = 0x80000;    // ITAMP4F
    static constexpr uint32_t SR_ITAMP5F = 0x100000;   // ITAMP5F
    static constexpr uint32_t SR_ITAMP6F = 0x200000;   // ITAMP6F
    static constexpr uint32_t SR_ITAMP7F = 0x400000;   // ITAMP7F
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t MISR_TAMP1MF = 0x1;        // TAMP1MF:
    static constexpr uint32_t MISR_TAMP2MF = 0x2;        // TAMP2MF
    static constexpr uint32_t MISR_ITAMP1MF = 0x10000;   // ITAMP1MF
    static constexpr uint32_t MISR_ITAMP3MF = 0x40000;   // ITAMP3MF
    static constexpr uint32_t MISR_ITAMP4MF = 0x80000;   // ITAMP4MF
    static constexpr uint32_t MISR_ITAMP5MF = 0x100000;  // ITAMP5MF
    static constexpr uint32_t MISR_ITAMP6MF = 0x200000;  // ITAMP6MF
    static const uint32_t MISR_RESET_VALUE = 0x0;

    static constexpr uint32_t SCR_CTAMP1F = 0x1;        // CTAMP1F
    static constexpr uint32_t SCR_CTAMP2F = 0x2;        // CTAMP2F
    static constexpr uint32_t SCR_CITAMP1F = 0x10000;   // CITAMP1F
    static constexpr uint32_t SCR_CITAMP3F = 0x40000;   // CITAMP3F
    static constexpr uint32_t SCR_CITAMP4F = 0x80000;   // CITAMP4F
    static constexpr uint32_t SCR_CITAMP5F = 0x100000;  // CITAMP5F
    static constexpr uint32_t SCR_CITAMP6F = 0x200000;  // CITAMP6F
    static constexpr uint32_t SCR_CITAMP7F = 0x400000;  // CITAMP7F
    static const uint32_t SCR_RESET_VALUE = 0x0;


    static const uint32_t BKP0R_RESET_VALUE = 0x0;


    static const uint32_t BKP1R_RESET_VALUE = 0x0;


    static const uint32_t BKP2R_RESET_VALUE = 0x0;


    static const uint32_t BKP3R_RESET_VALUE = 0x0;


    static const uint32_t BKP4R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR2_PTIONREG_OUT =        // PTIONREG_OUT (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR2_TRUST_ZONE =          // TRUST_ZONE (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static const uint32_t HWCFGR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_BACKUP_REGS =         // BACKUP_REGS (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_TAMPER =              // TAMPER (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_ACTIVE_TAMPER =       // ACTIVE_TAMPER (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_INT_TAMPER =          // INT_TAMPER (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t HWCFGR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t VERR_MINREV =              // Minor Revision number (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t VERR_MAJREV =              // Major Revision number (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    static const uint32_t VERR_RESET_VALUE = 0x0;


    static const uint32_t IPIDR_RESET_VALUE = 0x0;


    static const uint32_t SIDR_RESET_VALUE = 0x0;
};

static tamp_t& TAMP = *reinterpret_cast<tamp_t*>(0x4000b000);

#define HAVE_PERIPHERAL_TAMP


////
//
//    USB Power Delivery interface
//
////

struct ucpd1_t
{
    volatile uint32_t    CFG1;                 // [Read-write] UCPD configuration register
    volatile uint32_t    CFG2;                 // [Read-write] UCPD configuration register 2
    volatile uint32_t    CFG3;                 // [Read-write] UCPD configuration register 3
    volatile uint32_t    CR;                   // [Read-write] UCPD control register
    volatile uint32_t    IMR;                  // [Read-write] UCPD Interrupt Mask Register
    volatile uint32_t    SR;                   // [Read-only] UCPD Status Register
    volatile uint32_t    ICR;                  // [Read-write] UCPD Interrupt Clear Register
    volatile uint32_t    TX_ORDSET;            // [Read-write] UCPD Tx Ordered Set Type Register
    volatile uint32_t    TX_PAYSZ;             // [Read-write] UCPD Tx Paysize Register
    volatile uint32_t    TXDR;                 // [Read-write] UCPD Tx Data Register
    volatile uint32_t    RX_ORDSET;            // [Read-only] UCPD Rx Ordered Set Register
    volatile uint32_t    RX_PAYSZ;             // [Read-write] UCPD Rx Paysize Register
    volatile uint32_t    RXDR;                 // [Read-only] UCPD Receive Data Register
    volatile uint32_t    RX_ORDEXT1;           // [Read-write] UCPD Rx Ordered Set Extension Register
    volatile uint32_t    RX_ORDEXT2;           // [Read-write] UCPD Rx Ordered Set Extension Register
    reserved_t<238>      _0;
    volatile uint32_t    IPVER;                // [Read-only] UCPD IP ID register
    volatile uint32_t    IPID;                 // [Read-only] UCPD IP ID register
    volatile uint32_t    MID;                  // [Read-only] UCPD IP ID register

    template<uint32_t X>
    static constexpr uint32_t CFG1_HBITCLKDIV =          // HBITCLKDIV (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG1_IFRGAP =              // IFRGAP (5 bits)
        bit_field_t<6, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG1_TRANSWIN =            // TRANSWIN (5 bits)
        bit_field_t<11, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG1_PSC_USBPDCLK =        // PSC_USBPDCLK (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG1_RXORDSETEN =          // RXORDSETEN (9 bits)
        bit_field_t<20, 0x1ff>::value<X>();
    static constexpr uint32_t CFG1_TXDMAEN = 0x20000000; // TXDMAEN
    static constexpr uint32_t CFG1_RXDMAEN = 0x40000000; // RXDMAEN:
    static constexpr uint32_t CFG1_UCPDEN = 0x80000000;  // UCPDEN
    static const uint32_t CFG1_RESET_VALUE = 0x0;

    static constexpr uint32_t CFG2_RXFILTDIS = 0x1;      // RXFILTDIS
    static constexpr uint32_t CFG2_RXFILT2N3 = 0x2;      // RXFILT2N3
    static constexpr uint32_t CFG2_FORCECLK = 0x4;       // FORCECLK
    static constexpr uint32_t CFG2_WUPEN = 0x8;          // WUPEN
    static const uint32_t CFG2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CFG3_TRIM1_NG_CCRPD =      // TRIM1_NG_CCRPD (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG3_TRIM1_NG_CC1A5 =      // TRIM1_NG_CC1A5 (5 bits)
        bit_field_t<4, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG3_TRIM1_NG_CC3A0 =      // TRIM1_NG_CC3A0 (4 bits)
        bit_field_t<9, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG3_TRIM2_NG_CCRPD =      // TRIM2_NG_CCRPD (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG3_TRIM2_NG_CC1A5 =      // TRIM2_NG_CC1A5 (5 bits)
        bit_field_t<20, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG3_TRIM2_NG_CC3A0 =      // TRIM2_NG_CC3A0 (4 bits)
        bit_field_t<25, 0xf>::value<X>();
    static const uint32_t CFG3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR_TXMODE =              // TXMODE (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static constexpr uint32_t CR_TXSEND = 0x4;         // TXSEND
    static constexpr uint32_t CR_TXHRST = 0x8;         // TXHRST
    static constexpr uint32_t CR_RXMODE = 0x10;        // RXMODE
    static constexpr uint32_t CR_PHYRXEN = 0x20;       // PHYRXEN
    static constexpr uint32_t CR_PHYCCSEL = 0x40;      // PHYCCSEL
    template<uint32_t X>
    static constexpr uint32_t CR_ANASUBMODE =          // ANASUBMODE (2 bits)
        bit_field_t<7, 0x3>::value<X>();
    static constexpr uint32_t CR_ANAMODE = 0x200;      // ANAMODE
    template<uint32_t X>
    static constexpr uint32_t CR_CCENABLE =            // CCENABLE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t CR_DBATTEN = 0x8000;     // DBATTEN
    static constexpr uint32_t CR_FRSRXEN = 0x10000;    // FRSRXEN
    static constexpr uint32_t CR_FRSTX = 0x20000;      // FRSTX
    static constexpr uint32_t CR_RDCH = 0x40000;       // RDCH
    static constexpr uint32_t CR_CC1TCDIS = 0x100000;  // CC1TCDIS
    static constexpr uint32_t CR_CC2TCDIS = 0x200000;  // CC2TCDIS
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t IMR_TXISIE = 0x1;         // TXISIE
    static constexpr uint32_t IMR_TXMSGDISCIE = 0x2;    // TXMSGDISCIE
    static constexpr uint32_t IMR_TXMSGSENTIE = 0x4;    // TXMSGSENTIE
    static constexpr uint32_t IMR_TXMSGABTIE = 0x8;     // TXMSGABTIE
    static constexpr uint32_t IMR_HRSTDISCIE = 0x10;    // HRSTDISCIE
    static constexpr uint32_t IMR_HRSTSENTIE = 0x20;    // HRSTSENTIE
    static constexpr uint32_t IMR_TXUNDIE = 0x40;       // TXUNDIE
    static constexpr uint32_t IMR_RXNEIE = 0x100;       // RXNEIE
    static constexpr uint32_t IMR_RXORDDETIE = 0x200;   // RXORDDETIE
    static constexpr uint32_t IMR_RXHRSTDETIE = 0x400;  // RXHRSTDETIE
    static constexpr uint32_t IMR_RXOVRIE = 0x800;      // RXOVRIE
    static constexpr uint32_t IMR_RXMSGENDIE = 0x1000;  // RXMSGENDIE
    static constexpr uint32_t IMR_TYPECEVT1IE = 0x4000; // TYPECEVT1IE
    static constexpr uint32_t IMR_TYPECEVT2IE = 0x8000; // TYPECEVT2IE
    static constexpr uint32_t IMR_FRSEVTIE = 0x100000;  // FRSEVTIE
    static const uint32_t IMR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_TXIS = 0x1;           // TXIS
    static constexpr uint32_t SR_TXMSGDISC = 0x2;      // TXMSGDISC
    static constexpr uint32_t SR_TXMSGSENT = 0x4;      // TXMSGSENT
    static constexpr uint32_t SR_TXMSGABT = 0x8;       // TXMSGABT
    static constexpr uint32_t SR_HRSTDISC = 0x10;      // HRSTDISC
    static constexpr uint32_t SR_HRSTSENT = 0x20;      // HRSTSENT
    static constexpr uint32_t SR_TXUND = 0x40;         // TXUND
    static constexpr uint32_t SR_RXNE = 0x100;         // RXNE
    static constexpr uint32_t SR_RXORDDET = 0x200;     // RXORDDET
    static constexpr uint32_t SR_RXHRSTDET = 0x400;    // RXHRSTDET
    static constexpr uint32_t SR_RXOVR = 0x800;        // RXOVR
    static constexpr uint32_t SR_RXMSGEND = 0x1000;    // RXMSGEND
    static constexpr uint32_t SR_RXERR = 0x2000;       // RXERR
    static constexpr uint32_t SR_TYPECEVT1 = 0x4000;   // TYPECEVT1
    static constexpr uint32_t SR_TYPECEVT2 = 0x8000;   // TYPECEVT2
    template<uint32_t X>
    static constexpr uint32_t SR_TYPEC_VSTATE_CC1 =    // TYPEC_VSTATE_CC1 (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SR_TYPEC_VSTATE_CC2 =    // TYPEC_VSTATE_CC2 (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    static constexpr uint32_t SR_FRSEVT = 0x100000;    // FRSEVT
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t ICR_TXMSGDISCCF = 0x2;    // TXMSGDISCCF
    static constexpr uint32_t ICR_TXMSGSENTCF = 0x4;    // TXMSGSENTCF
    static constexpr uint32_t ICR_TXMSGABTCF = 0x8;     // TXMSGABTCF
    static constexpr uint32_t ICR_HRSTDISCCF = 0x10;    // HRSTDISCCF
    static constexpr uint32_t ICR_HRSTSENTCF = 0x20;    // HRSTSENTCF
    static constexpr uint32_t ICR_TXUNDCF = 0x40;       // TXUNDCF
    static constexpr uint32_t ICR_RXORDDETCF = 0x200;   // RXORDDETCF
    static constexpr uint32_t ICR_RXHRSTDETCF = 0x400;  // RXHRSTDETCF
    static constexpr uint32_t ICR_RXOVRCF = 0x800;      // RXOVRCF
    static constexpr uint32_t ICR_RXMSGENDCF = 0x1000;  // RXMSGENDCF
    static constexpr uint32_t ICR_TYPECEVT1CF = 0x4000; // TYPECEVT1CF
    static constexpr uint32_t ICR_TYPECEVT2CF = 0x8000; // TYPECEVT2CF
    static constexpr uint32_t ICR_FRSEVTCF = 0x100000;  // FRSEVTCF
    static const uint32_t ICR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TX_ORDSET_TXORDSET =            // TXORDSET (20 bits)
        bit_field_t<0, 0xfffff>::value<X>();
    static const uint32_t TX_ORDSET_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TX_PAYSZ_TXPAYSZ =             // TXPAYSZ (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t TX_PAYSZ_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXDR_TXDATA =              // TXDATA (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TXDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RX_ORDSET_RXORDSET =            // RXORDSET (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t RX_ORDSET_RXSOP3OF4 = 0x8;      // RXSOP3OF4
    template<uint32_t X>
    static constexpr uint32_t RX_ORDSET_RXSOPKINVALID =       // RXSOPKINVALID (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static const uint32_t RX_ORDSET_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RX_PAYSZ_RXPAYSZ =             // RXPAYSZ (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t RX_PAYSZ_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXDR_RXDATA =              // RXDATA (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RXDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RX_ORDEXT1_RXSOPX1 =             // RXSOPX1 (20 bits)
        bit_field_t<0, 0xfffff>::value<X>();
    static const uint32_t RX_ORDEXT1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RX_ORDEXT2_RXSOPX2 =             // RXSOPX2 (20 bits)
        bit_field_t<0, 0xfffff>::value<X>();
    static const uint32_t RX_ORDEXT2_RESET_VALUE = 0x0;


    static const uint32_t IPVER_RESET_VALUE = 0x10;


    static const uint32_t IPID_RESET_VALUE = 0x150021;


    static const uint32_t MID_RESET_VALUE = 0xa3c5dd01;

    static constexpr uint8_t UCPD1_UCPD2 = 8; // UCPD global interrupt
};

static ucpd1_t& UCPD1 = *reinterpret_cast<ucpd1_t*>(0x4000a000);

#define HAVE_PERIPHERAL_UCPD1


////
//
//    USB Power Delivery interface
//
////

struct ucpd2_t
{
    volatile uint32_t    CFG1;                 // [Read-write] UCPD configuration register
    volatile uint32_t    CFG2;                 // [Read-write] UCPD configuration register 2
    volatile uint32_t    CFG3;                 // [Read-write] UCPD configuration register 3
    volatile uint32_t    CR;                   // [Read-write] UCPD control register
    volatile uint32_t    IMR;                  // [Read-write] UCPD Interrupt Mask Register
    volatile uint32_t    SR;                   // [Read-only] UCPD Status Register
    volatile uint32_t    ICR;                  // [Read-write] UCPD Interrupt Clear Register
    volatile uint32_t    TX_ORDSET;            // [Read-write] UCPD Tx Ordered Set Type Register
    volatile uint32_t    TX_PAYSZ;             // [Read-write] UCPD Tx Paysize Register
    volatile uint32_t    TXDR;                 // [Read-write] UCPD Tx Data Register
    volatile uint32_t    RX_ORDSET;            // [Read-only] UCPD Rx Ordered Set Register
    volatile uint32_t    RX_PAYSZ;             // [Read-write] UCPD Rx Paysize Register
    volatile uint32_t    RXDR;                 // [Read-only] UCPD Receive Data Register
    volatile uint32_t    RX_ORDEXT1;           // [Read-write] UCPD Rx Ordered Set Extension Register
    volatile uint32_t    RX_ORDEXT2;           // [Read-write] UCPD Rx Ordered Set Extension Register
    reserved_t<238>      _0;
    volatile uint32_t    IPVER;                // [Read-only] UCPD IP ID register
    volatile uint32_t    IPID;                 // [Read-only] UCPD IP ID register
    volatile uint32_t    MID;                  // [Read-only] UCPD IP ID register

    template<uint32_t X>
    static constexpr uint32_t CFG1_HBITCLKDIV =          // HBITCLKDIV (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG1_IFRGAP =              // IFRGAP (5 bits)
        bit_field_t<6, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG1_TRANSWIN =            // TRANSWIN (5 bits)
        bit_field_t<11, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG1_PSC_USBPDCLK =        // PSC_USBPDCLK (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG1_RXORDSETEN =          // RXORDSETEN (9 bits)
        bit_field_t<20, 0x1ff>::value<X>();
    static constexpr uint32_t CFG1_TXDMAEN = 0x20000000; // TXDMAEN
    static constexpr uint32_t CFG1_RXDMAEN = 0x40000000; // RXDMAEN:
    static constexpr uint32_t CFG1_UCPDEN = 0x80000000;  // UCPDEN
    static const uint32_t CFG1_RESET_VALUE = 0x0;

    static constexpr uint32_t CFG2_RXFILTDIS = 0x1;      // RXFILTDIS
    static constexpr uint32_t CFG2_RXFILT2N3 = 0x2;      // RXFILT2N3
    static constexpr uint32_t CFG2_FORCECLK = 0x4;       // FORCECLK
    static constexpr uint32_t CFG2_WUPEN = 0x8;          // WUPEN
    static const uint32_t CFG2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CFG3_TRIM1_NG_CCRPD =      // TRIM1_NG_CCRPD (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG3_TRIM1_NG_CC1A5 =      // TRIM1_NG_CC1A5 (5 bits)
        bit_field_t<4, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG3_TRIM1_NG_CC3A0 =      // TRIM1_NG_CC3A0 (4 bits)
        bit_field_t<9, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG3_TRIM2_NG_CCRPD =      // TRIM2_NG_CCRPD (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG3_TRIM2_NG_CC1A5 =      // TRIM2_NG_CC1A5 (5 bits)
        bit_field_t<20, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFG3_TRIM2_NG_CC3A0 =      // TRIM2_NG_CC3A0 (4 bits)
        bit_field_t<25, 0xf>::value<X>();
    static const uint32_t CFG3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR_TXMODE =              // TXMODE (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static constexpr uint32_t CR_TXSEND = 0x4;         // TXSEND
    static constexpr uint32_t CR_TXHRST = 0x8;         // TXHRST
    static constexpr uint32_t CR_RXMODE = 0x10;        // RXMODE
    static constexpr uint32_t CR_PHYRXEN = 0x20;       // PHYRXEN
    static constexpr uint32_t CR_PHYCCSEL = 0x40;      // PHYCCSEL
    template<uint32_t X>
    static constexpr uint32_t CR_ANASUBMODE =          // ANASUBMODE (2 bits)
        bit_field_t<7, 0x3>::value<X>();
    static constexpr uint32_t CR_ANAMODE = 0x200;      // ANAMODE
    template<uint32_t X>
    static constexpr uint32_t CR_CCENABLE =            // CCENABLE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t CR_DBATTEN = 0x8000;     // DBATTEN
    static constexpr uint32_t CR_FRSRXEN = 0x10000;    // FRSRXEN
    static constexpr uint32_t CR_FRSTX = 0x20000;      // FRSTX
    static constexpr uint32_t CR_RDCH = 0x40000;       // RDCH
    static constexpr uint32_t CR_CC1TCDIS = 0x100000;  // CC1TCDIS
    static constexpr uint32_t CR_CC2TCDIS = 0x200000;  // CC2TCDIS
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t IMR_TXISIE = 0x1;         // TXISIE
    static constexpr uint32_t IMR_TXMSGDISCIE = 0x2;    // TXMSGDISCIE
    static constexpr uint32_t IMR_TXMSGSENTIE = 0x4;    // TXMSGSENTIE
    static constexpr uint32_t IMR_TXMSGABTIE = 0x8;     // TXMSGABTIE
    static constexpr uint32_t IMR_HRSTDISCIE = 0x10;    // HRSTDISCIE
    static constexpr uint32_t IMR_HRSTSENTIE = 0x20;    // HRSTSENTIE
    static constexpr uint32_t IMR_TXUNDIE = 0x40;       // TXUNDIE
    static constexpr uint32_t IMR_RXNEIE = 0x100;       // RXNEIE
    static constexpr uint32_t IMR_RXORDDETIE = 0x200;   // RXORDDETIE
    static constexpr uint32_t IMR_RXHRSTDETIE = 0x400;  // RXHRSTDETIE
    static constexpr uint32_t IMR_RXOVRIE = 0x800;      // RXOVRIE
    static constexpr uint32_t IMR_RXMSGENDIE = 0x1000;  // RXMSGENDIE
    static constexpr uint32_t IMR_TYPECEVT1IE = 0x4000; // TYPECEVT1IE
    static constexpr uint32_t IMR_TYPECEVT2IE = 0x8000; // TYPECEVT2IE
    static constexpr uint32_t IMR_FRSEVTIE = 0x100000;  // FRSEVTIE
    static const uint32_t IMR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_TXIS = 0x1;           // TXIS
    static constexpr uint32_t SR_TXMSGDISC = 0x2;      // TXMSGDISC
    static constexpr uint32_t SR_TXMSGSENT = 0x4;      // TXMSGSENT
    static constexpr uint32_t SR_TXMSGABT = 0x8;       // TXMSGABT
    static constexpr uint32_t SR_HRSTDISC = 0x10;      // HRSTDISC
    static constexpr uint32_t SR_HRSTSENT = 0x20;      // HRSTSENT
    static constexpr uint32_t SR_TXUND = 0x40;         // TXUND
    static constexpr uint32_t SR_RXNE = 0x100;         // RXNE
    static constexpr uint32_t SR_RXORDDET = 0x200;     // RXORDDET
    static constexpr uint32_t SR_RXHRSTDET = 0x400;    // RXHRSTDET
    static constexpr uint32_t SR_RXOVR = 0x800;        // RXOVR
    static constexpr uint32_t SR_RXMSGEND = 0x1000;    // RXMSGEND
    static constexpr uint32_t SR_RXERR = 0x2000;       // RXERR
    static constexpr uint32_t SR_TYPECEVT1 = 0x4000;   // TYPECEVT1
    static constexpr uint32_t SR_TYPECEVT2 = 0x8000;   // TYPECEVT2
    template<uint32_t X>
    static constexpr uint32_t SR_TYPEC_VSTATE_CC1 =    // TYPEC_VSTATE_CC1 (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SR_TYPEC_VSTATE_CC2 =    // TYPEC_VSTATE_CC2 (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    static constexpr uint32_t SR_FRSEVT = 0x100000;    // FRSEVT
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t ICR_TXMSGDISCCF = 0x2;    // TXMSGDISCCF
    static constexpr uint32_t ICR_TXMSGSENTCF = 0x4;    // TXMSGSENTCF
    static constexpr uint32_t ICR_TXMSGABTCF = 0x8;     // TXMSGABTCF
    static constexpr uint32_t ICR_HRSTDISCCF = 0x10;    // HRSTDISCCF
    static constexpr uint32_t ICR_HRSTSENTCF = 0x20;    // HRSTSENTCF
    static constexpr uint32_t ICR_TXUNDCF = 0x40;       // TXUNDCF
    static constexpr uint32_t ICR_RXORDDETCF = 0x200;   // RXORDDETCF
    static constexpr uint32_t ICR_RXHRSTDETCF = 0x400;  // RXHRSTDETCF
    static constexpr uint32_t ICR_RXOVRCF = 0x800;      // RXOVRCF
    static constexpr uint32_t ICR_RXMSGENDCF = 0x1000;  // RXMSGENDCF
    static constexpr uint32_t ICR_TYPECEVT1CF = 0x4000; // TYPECEVT1CF
    static constexpr uint32_t ICR_TYPECEVT2CF = 0x8000; // TYPECEVT2CF
    static constexpr uint32_t ICR_FRSEVTCF = 0x100000;  // FRSEVTCF
    static const uint32_t ICR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TX_ORDSET_TXORDSET =            // TXORDSET (20 bits)
        bit_field_t<0, 0xfffff>::value<X>();
    static const uint32_t TX_ORDSET_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TX_PAYSZ_TXPAYSZ =             // TXPAYSZ (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t TX_PAYSZ_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXDR_TXDATA =              // TXDATA (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TXDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RX_ORDSET_RXORDSET =            // RXORDSET (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t RX_ORDSET_RXSOP3OF4 = 0x8;      // RXSOP3OF4
    template<uint32_t X>
    static constexpr uint32_t RX_ORDSET_RXSOPKINVALID =       // RXSOPKINVALID (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static const uint32_t RX_ORDSET_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RX_PAYSZ_RXPAYSZ =             // RXPAYSZ (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t RX_PAYSZ_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXDR_RXDATA =              // RXDATA (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RXDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RX_ORDEXT1_RXSOPX1 =             // RXSOPX1 (20 bits)
        bit_field_t<0, 0xfffff>::value<X>();
    static const uint32_t RX_ORDEXT1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RX_ORDEXT2_RXSOPX2 =             // RXSOPX2 (20 bits)
        bit_field_t<0, 0xfffff>::value<X>();
    static const uint32_t RX_ORDEXT2_RESET_VALUE = 0x0;


    static const uint32_t IPVER_RESET_VALUE = 0x10;


    static const uint32_t IPID_RESET_VALUE = 0x150021;


    static const uint32_t MID_RESET_VALUE = 0xa3c5dd01;
};

static ucpd2_t& UCPD2 = *reinterpret_cast<ucpd2_t*>(0x4000a400);

#define HAVE_PERIPHERAL_UCPD2


////
//
//    Low power timer
//
////

struct lptim1_t
{
    volatile uint32_t    ISR;                  // [Read-only] Interrupt and Status Register
    volatile uint32_t    ICR;                  // [Write-only] Interrupt Clear Register
    volatile uint32_t    IER;                  // [Read-write] Interrupt Enable Register
    volatile uint32_t    CFGR;                 // [Read-write] Configuration Register
    volatile uint32_t    CR;                   // [Read-write] Control Register
    volatile uint32_t    CMP;                  // [Read-write] Compare Register
    volatile uint32_t    ARR;                  // [Read-write] Autoreload Register
    volatile uint32_t    CNT;                  // [Read-only] Counter Register
    reserved_t<1>        _0;
    volatile uint32_t    CFGR2;                // [Read-write] LPTIM configuration register 2

    static constexpr uint32_t ISR_DOWN = 0x40;          // Counter direction change up to down
    static constexpr uint32_t ISR_UP = 0x20;            // Counter direction change down to up
    static constexpr uint32_t ISR_ARROK = 0x10;         // Autoreload register update OK
    static constexpr uint32_t ISR_CMPOK = 0x8;          // Compare register update OK
    static constexpr uint32_t ISR_EXTTRIG = 0x4;        // External trigger edge event
    static constexpr uint32_t ISR_ARRM = 0x2;           // Autoreload match
    static constexpr uint32_t ISR_CMPM = 0x1;           // Compare match
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t ICR_DOWNCF = 0x40;        // Direction change to down Clear Flag
    static constexpr uint32_t ICR_UPCF = 0x20;          // Direction change to UP Clear Flag
    static constexpr uint32_t ICR_ARROKCF = 0x10;       // Autoreload register update OK Clear Flag
    static constexpr uint32_t ICR_CMPOKCF = 0x8;        // Compare register update OK Clear Flag
    static constexpr uint32_t ICR_EXTTRIGCF = 0x4;      // External trigger valid edge Clear Flag
    static constexpr uint32_t ICR_ARRMCF = 0x2;         // Autoreload match Clear Flag
    static constexpr uint32_t ICR_CMPMCF = 0x1;         // compare match Clear Flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static constexpr uint32_t IER_DOWNIE = 0x40;        // Direction change to down Interrupt Enable
    static constexpr uint32_t IER_UPIE = 0x20;          // Direction change to UP Interrupt Enable
    static constexpr uint32_t IER_ARROKIE = 0x10;       // Autoreload register update OK Interrupt Enable
    static constexpr uint32_t IER_CMPOKIE = 0x8;        // Compare register update OK Interrupt Enable
    static constexpr uint32_t IER_EXTTRIGIE = 0x4;      // External trigger valid edge Interrupt Enable
    static constexpr uint32_t IER_ARRMIE = 0x2;         // Autoreload match Interrupt Enable
    static constexpr uint32_t IER_CMPMIE = 0x1;         // Compare match Interrupt Enable
    static const uint32_t IER_RESET_VALUE = 0x0;

    static constexpr uint32_t CFGR_ENC = 0x1000000;      // Encoder mode enable
    static constexpr uint32_t CFGR_COUNTMODE = 0x800000; // counter mode enabled
    static constexpr uint32_t CFGR_PRELOAD = 0x400000;   // Registers update mode
    static constexpr uint32_t CFGR_WAVPOL = 0x200000;    // Waveform shape polarity
    static constexpr uint32_t CFGR_WAVE = 0x100000;      // Waveform shape
    static constexpr uint32_t CFGR_TIMOUT = 0x80000;     // Timeout enable
    template<uint32_t X>
    static constexpr uint32_t CFGR_TRIGEN =              // Trigger enable and polarity (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_TRIGSEL =             // Trigger selector (3 bits)
        bit_field_t<13, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_PRESC =               // Clock prescaler (3 bits)
        bit_field_t<9, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_TRGFLT =              // Configurable digital filter for trigger (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_CKFLT =               // Configurable digital filter for external clock (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_CKPOL =               // Clock Polarity (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t CFGR_CKSEL = 0x1;          // Clock selector
    static const uint32_t CFGR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_RSTARE = 0x10;        // Reset after read enable
    static constexpr uint32_t CR_COUNTRST = 0x8;       // Counter reset
    static constexpr uint32_t CR_CNTSTRT = 0x4;        // Timer start in continuous mode
    static constexpr uint32_t CR_SNGSTRT = 0x2;        // LPTIM start in single mode
    static constexpr uint32_t CR_ENABLE = 0x1;         // LPTIM Enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CMP_CMP =                 // Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CMP_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Auto reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x1;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // Counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CFGR2_IN2SEL =              // LPTIM1 Input 2 selection (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR2_IN1SEL =              // LPTIMx Input 1 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t CFGR2_RESET_VALUE = 0x0;
};

static lptim1_t& LPTIM1 = *reinterpret_cast<lptim1_t*>(0x40007c00);

#define HAVE_PERIPHERAL_LPTIM1


////
//
//    Low power timer
//
////

struct lptim2_t
{
    volatile uint32_t    ISR;                  // [Read-only] Interrupt and Status Register
    volatile uint32_t    ICR;                  // [Write-only] Interrupt Clear Register
    volatile uint32_t    IER;                  // [Read-write] Interrupt Enable Register
    volatile uint32_t    CFGR;                 // [Read-write] Configuration Register
    volatile uint32_t    CR;                   // [Read-write] Control Register
    volatile uint32_t    CMP;                  // [Read-write] Compare Register
    volatile uint32_t    ARR;                  // [Read-write] Autoreload Register
    volatile uint32_t    CNT;                  // [Read-only] Counter Register
    reserved_t<1>        _0;
    volatile uint32_t    CFGR2;                // [Read-write] LPTIM configuration register 2

    static constexpr uint32_t ISR_DOWN = 0x40;          // Counter direction change up to down
    static constexpr uint32_t ISR_UP = 0x20;            // Counter direction change down to up
    static constexpr uint32_t ISR_ARROK = 0x10;         // Autoreload register update OK
    static constexpr uint32_t ISR_CMPOK = 0x8;          // Compare register update OK
    static constexpr uint32_t ISR_EXTTRIG = 0x4;        // External trigger edge event
    static constexpr uint32_t ISR_ARRM = 0x2;           // Autoreload match
    static constexpr uint32_t ISR_CMPM = 0x1;           // Compare match
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t ICR_DOWNCF = 0x40;        // Direction change to down Clear Flag
    static constexpr uint32_t ICR_UPCF = 0x20;          // Direction change to UP Clear Flag
    static constexpr uint32_t ICR_ARROKCF = 0x10;       // Autoreload register update OK Clear Flag
    static constexpr uint32_t ICR_CMPOKCF = 0x8;        // Compare register update OK Clear Flag
    static constexpr uint32_t ICR_EXTTRIGCF = 0x4;      // External trigger valid edge Clear Flag
    static constexpr uint32_t ICR_ARRMCF = 0x2;         // Autoreload match Clear Flag
    static constexpr uint32_t ICR_CMPMCF = 0x1;         // compare match Clear Flag
    static const uint32_t ICR_RESET_VALUE = 0x0;

    static constexpr uint32_t IER_DOWNIE = 0x40;        // Direction change to down Interrupt Enable
    static constexpr uint32_t IER_UPIE = 0x20;          // Direction change to UP Interrupt Enable
    static constexpr uint32_t IER_ARROKIE = 0x10;       // Autoreload register update OK Interrupt Enable
    static constexpr uint32_t IER_CMPOKIE = 0x8;        // Compare register update OK Interrupt Enable
    static constexpr uint32_t IER_EXTTRIGIE = 0x4;      // External trigger valid edge Interrupt Enable
    static constexpr uint32_t IER_ARRMIE = 0x2;         // Autoreload match Interrupt Enable
    static constexpr uint32_t IER_CMPMIE = 0x1;         // Compare match Interrupt Enable
    static const uint32_t IER_RESET_VALUE = 0x0;

    static constexpr uint32_t CFGR_ENC = 0x1000000;      // Encoder mode enable
    static constexpr uint32_t CFGR_COUNTMODE = 0x800000; // counter mode enabled
    static constexpr uint32_t CFGR_PRELOAD = 0x400000;   // Registers update mode
    static constexpr uint32_t CFGR_WAVPOL = 0x200000;    // Waveform shape polarity
    static constexpr uint32_t CFGR_WAVE = 0x100000;      // Waveform shape
    static constexpr uint32_t CFGR_TIMOUT = 0x80000;     // Timeout enable
    template<uint32_t X>
    static constexpr uint32_t CFGR_TRIGEN =              // Trigger enable and polarity (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_TRIGSEL =             // Trigger selector (3 bits)
        bit_field_t<13, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_PRESC =               // Clock prescaler (3 bits)
        bit_field_t<9, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_TRGFLT =              // Configurable digital filter for trigger (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_CKFLT =               // Configurable digital filter for external clock (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_CKPOL =               // Clock Polarity (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t CFGR_CKSEL = 0x1;          // Clock selector
    static const uint32_t CFGR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_RSTARE = 0x10;        // Reset after read enable
    static constexpr uint32_t CR_COUNTRST = 0x8;       // Counter reset
    static constexpr uint32_t CR_CNTSTRT = 0x4;        // Timer start in continuous mode
    static constexpr uint32_t CR_SNGSTRT = 0x2;        // LPTIM start in single mode
    static constexpr uint32_t CR_ENABLE = 0x1;         // LPTIM Enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CMP_CMP =                 // Compare value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CMP_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Auto reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x1;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // Counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CFGR2_IN2SEL =              // LPTIM1 Input 2 selection (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR2_IN1SEL =              // LPTIMx Input 1 selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t CFGR2_RESET_VALUE = 0x0;
};

static lptim2_t& LPTIM2 = *reinterpret_cast<lptim2_t*>(0x40009400);

#define HAVE_PERIPHERAL_LPTIM2


////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

struct lpuart_t
{
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
    volatile uint32_t    CR2;                  // [Read-write] Control register 2
    volatile uint32_t    CR3;                  // [Read-write] Control register 3
    volatile uint32_t    BRR;                  // [Read-write] Baud rate register
    reserved_t<2>        _0;
    volatile uint32_t    RQR;                  // [Write-only] Request register
    volatile uint32_t    ISR;                  // [Read-only] Interrupt &amp; status register
    volatile uint32_t    ICR;                  // [Write-only] Interrupt flag clear register
    volatile uint32_t    RDR;                  // [Read-only] Receive data register
    volatile uint32_t    TDR;                  // [Read-write] Transmit data register
    volatile uint32_t    PRESC;                // [Read-write] Prescaler register
    reserved_t<239>      _1;
    volatile uint32_t    HWCFGR2;              // [Read-write] LPUART Hardware Configuration register 2
    volatile uint32_t    HWCFGR1;              // [Read-write] LPUART Hardware Configuration register 1
    volatile uint32_t    VERR;                 // [Read-only] EXTI IP Version register
    volatile uint32_t    IPIDR;                // [Read-only] EXTI Identification register
    volatile uint32_t    SIDR;                 // [Read-only] EXTI Size ID register

    static constexpr uint32_t CR1_RXFFIE = 0x80000000;  // RXFIFO Full interrupt enable
    static constexpr uint32_t CR1_TXFEIE = 0x40000000;  // TXFIFO empty interrupt enable
    static constexpr uint32_t CR1_FIFOEN = 0x20000000;  // FIFO mode enable
    static constexpr uint32_t CR1_M1 = 0x10000000;      // Word length
    template<uint32_t X>
    static constexpr uint32_t CR1_DEAT =                // DEAT0 (5 bits)
        bit_field_t<21, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR1_DEDT0 =               // DEDT0 (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static constexpr uint32_t CR1_CMIE = 0x4000;        // Character match interrupt enable
    static constexpr uint32_t CR1_MME = 0x2000;         // Mute mode enable
    static constexpr uint32_t CR1_M0 = 0x1000;          // Word length
    static constexpr uint32_t CR1_WAKE = 0x800;         // Receiver wakeup method
    static constexpr uint32_t CR1_PCE = 0x400;          // Parity control enable
    static constexpr uint32_t CR1_PS = 0x200;           // Parity selection
    static constexpr uint32_t CR1_PEIE = 0x100;         // PE interrupt enable
    static constexpr uint32_t CR1_TXEIE = 0x80;         // interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transmission complete interrupt enable
    static constexpr uint32_t CR1_RXNEIE = 0x20;        // RXNE interrupt enable
    static constexpr uint32_t CR1_IDLEIE = 0x10;        // IDLE interrupt enable
    static constexpr uint32_t CR1_TE = 0x8;             // Transmitter enable
    static constexpr uint32_t CR1_RE = 0x4;             // Receiver enable
    static constexpr uint32_t CR1_UESM = 0x2;           // USART enable in Stop mode
    static constexpr uint32_t CR1_UE = 0x1;             // USART enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR2_ADD4_7 =              // Address of the USART node (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR2_ADD0_3 =              // Address of the USART node (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t CR2_MSBFIRST = 0x80000;   // Most significant bit first
    static constexpr uint32_t CR2_TAINV = 0x40000;      // Binary data inversion
    static constexpr uint32_t CR2_TXINV = 0x20000;      // TX pin active level inversion
    static constexpr uint32_t CR2_RXINV = 0x10000;      // RX pin active level inversion
    static constexpr uint32_t CR2_SWAP = 0x8000;        // Swap TX/RX pins
    template<uint32_t X>
    static constexpr uint32_t CR2_STOP =                // STOP bits (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CR2_ADDM7 = 0x10;         // 7-bit Address Detection/4-bit Address Detection
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR3_TXFTCFG =             // TXFIFO threshold configuration (3 bits)
        bit_field_t<29, 0x7>::value<X>();
    static constexpr uint32_t CR3_RXFTIE = 0x10000000;  // RXFIFO threshold interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR3_RXFTCFG =             // Receive FIFO threshold configuration (3 bits)
        bit_field_t<25, 0x7>::value<X>();
    static constexpr uint32_t CR3_TXFTIE = 0x800000;    // threshold interrupt enable
    static constexpr uint32_t CR3_WUFIE = 0x400000;     // Wakeup from Stop mode interrupt enable
    template<uint32_t X>
    static constexpr uint32_t CR3_WUS =                 // Wakeup from Stop mode interrupt flag selection (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    static constexpr uint32_t CR3_DEP = 0x8000;         // Driver enable polarity selection
    static constexpr uint32_t CR3_DEM = 0x4000;         // Driver enable mode
    static constexpr uint32_t CR3_DDRE = 0x2000;        // DMA Disable on Reception Error
    static constexpr uint32_t CR3_OVRDIS = 0x1000;      // Overrun Disable
    static constexpr uint32_t CR3_CTSIE = 0x400;        // CTS interrupt enable
    static constexpr uint32_t CR3_CTSE = 0x200;         // CTS enable
    static constexpr uint32_t CR3_RTSE = 0x100;         // RTS enable
    static constexpr uint32_t CR3_DMAT = 0x80;          // DMA enable transmitter
    static constexpr uint32_t CR3_DMAR = 0x40;          // DMA enable receiver
    static constexpr uint32_t CR3_HDSEL = 0x8;          // Half-duplex selection
    static constexpr uint32_t CR3_EIE = 0x1;            // Error interrupt enable
    static const uint32_t CR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BRR_BRR =                 // BRR (20 bits)
        bit_field_t<0, 0xfffff>::value<X>();
    static const uint32_t BRR_RESET_VALUE = 0x0;

    static constexpr uint32_t RQR_TXFRQ = 0x10;         // Transmit data flush request
    static constexpr uint32_t RQR_RXFRQ = 0x8;          // Receive data flush request
    static constexpr uint32_t RQR_MMRQ = 0x4;           // Mute mode request
    static constexpr uint32_t RQR_SBKRQ = 0x2;          // Send break request
    static constexpr uint32_t RQR_ABRRQ = 0x1;          // Auto baud rate request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static constexpr uint32_t ISR_TXFT = 0x8000000;     // TXFIFO threshold flag
    static constexpr uint32_t ISR_RXFT = 0x4000000;     // RXFIFO threshold flag
    static constexpr uint32_t ISR_RXFF = 0x1000000;     // RXFIFO Full
    static constexpr uint32_t ISR_TXFE = 0x800000;      // TXFIFO Empty
    static constexpr uint32_t ISR_REACK = 0x400000;     // REACK
    static constexpr uint32_t ISR_TEACK = 0x200000;     // TEACK
    static constexpr uint32_t ISR_WUF = 0x100000;       // WUF
    static constexpr uint32_t ISR_RWU = 0x80000;        // RWU
    static constexpr uint32_t ISR_SBKF = 0x40000;       // SBKF
    static constexpr uint32_t ISR_CMF = 0x20000;        // CMF
    static constexpr uint32_t ISR_BUSY = 0x10000;       // BUSY
    static constexpr uint32_t ISR_CTS = 0x400;          // CTS
    static constexpr uint32_t ISR_CTSIF = 0x200;        // CTSIF
    static constexpr uint32_t ISR_TXE = 0x80;           // TXE
    static constexpr uint32_t ISR_TC = 0x40;            // TC
    static constexpr uint32_t ISR_RXNE = 0x20;          // RXNE
    static constexpr uint32_t ISR_IDLE = 0x10;          // IDLE
    static constexpr uint32_t ISR_ORE = 0x8;            // ORE
    static constexpr uint32_t ISR_NF = 0x4;             // NF
    static constexpr uint32_t ISR_FE = 0x2;             // FE
    static constexpr uint32_t ISR_PE = 0x1;             // PE
    static const uint32_t ISR_RESET_VALUE = 0xc0;

    static constexpr uint32_t ICR_WUCF = 0x100000;      // Wakeup from Stop mode clear flag
    static constexpr uint32_t ICR_CMCF = 0x20000;       // Character match clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
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

    template<uint32_t X>
    static constexpr uint32_t PRESC_PRESCALER =           // Clock prescaler (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR2_CFG1 =                // LUART hardware configuration 1 (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR2_CFG2 =                // LUART hardware configuration 2 (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    static const uint32_t HWCFGR2_RESET_VALUE = 0x13;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CFG1 =                // LUART hardware configuration 1 (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CFG2 =                // LUART hardware configuration 2 (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CFG3 =                // LUART hardware configuration 1 (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CFG4 =                // LUART hardware configuration 2 (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CFG5 =                // LUART hardware configuration 2 (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CFG6 =                // LUART hardware configuration 2 (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CFG7 =                // LUART hardware configuration 2 (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR1_CFG8 =                // LUART hardware configuration 2 (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    static const uint32_t HWCFGR1_RESET_VALUE = 0x31100000;

    template<uint32_t X>
    static constexpr uint32_t VERR_MINREV =              // Minor Revision number (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t VERR_MAJREV =              // Major Revision number (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    static const uint32_t VERR_RESET_VALUE = 0x23;


    static const uint32_t IPIDR_RESET_VALUE = 0x130003;


    static const uint32_t SIDR_RESET_VALUE = 0xa3c5dd01;
};

static lpuart_t& LPUART = *reinterpret_cast<lpuart_t*>(0x40008000);

#define HAVE_PERIPHERAL_LPUART


////
//
//    HDMI-CEC
//
////

struct hdmi_cec_t
{
    volatile uint32_t    CEC_CR;               // [Read-write] CEC control register
    volatile uint32_t    CEC_CFGR;             // [Read-write] This register is used to configure the HDMI-CEC controller. It is mandatory to write CEC_CFGR only when CECEN=0.
    volatile uint32_t    CEC_TXDR;             // [Write-only] CEC Tx data register
    volatile uint32_t    CEC_RXDR;             // [Read-only] CEC Rx Data Register
    volatile uint32_t    CEC_ISR;              // [Read-write] CEC Interrupt and Status Register
    volatile uint32_t    CEC_IER;              // [Read-write] CEC interrupt enable register

    static constexpr uint32_t CEC_CR_CECEN = 0x1;          // CEC Enable The CECEN bit is set and cleared by software. CECEN=1 starts message reception and enables the TXSOM control. CECEN=0 disables the CEC peripheral, clears all bits of CEC_CR register and aborts any on-going reception or transmission.
    static constexpr uint32_t CEC_CR_TXSOM = 0x2;          // Tx Start Of Message TXSOM is set by software to command transmission of the first byte of a CEC message. If the CEC message consists of only one byte, TXEOM must be set before of TXSOM. Start-Bit is effectively started on the CEC line after SFT is counted. If TXSOM is set while a message reception is ongoing, transmission will start after the end of reception. TXSOM is cleared by hardware after the last byte of the message is sent with a positive acknowledge (TXEND=1), in case of transmission underrun (TXUDR=1), negative acknowledge (TXACKE=1), and transmission error (TXERR=1). It is also cleared by CECEN=0. It is not cleared and transmission is automatically retried in case of arbitration lost (ARBLST=1). TXSOM can be also used as a status bit informing application whether any transmission request is pending or under execution. The application can abort a transmission request at any time by clearing the CECEN bit. Note: TXSOM must be set when CECEN=1 TXSOM must be set when transmission data is available into TXDR HEADERs first four bits containing own peripheral address are taken from TXDR[7:4], not from CEC_CFGR.OAR which is used only for reception
    static constexpr uint32_t CEC_CR_TXEOM = 0x4;          // Tx End Of Message The TXEOM bit is set by software to command transmission of the last byte of a CEC message. TXEOM is cleared by hardware at the same time and under the same conditions as for TXSOM. Note: TXEOM must be set when CECEN=1 TXEOM must be set before writing transmission data to TXDR If TXEOM is set when TXSOM=0, transmitted message will consist of 1 byte (HEADER) only (PING message)
    static const uint32_t CEC_CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CEC_CFGR_SFT =                 // Signal Free Time SFT bits are set by software. In the SFT=0x0 configuration the number of nominal data bit periods waited before transmission is ruled by hardware according to the transmission history. In all the other configurations the SFT number is determined by software. * 0x0 ** 2.5 Data-Bit periods if CEC is the last bus initiator with unsuccessful transmission (ARBLST=1, TXERR=1, TXUDR=1 or TXACKE= 1) ** 4 Data-Bit periods if CEC is the new bus initiator ** 6 Data-Bit periods if CEC is the last bus initiator with successful transmission (TXEOM=1) * 0x1: 0.5 nominal data bit periods * 0x2: 1.5 nominal data bit periods * 0x3: 2.5 nominal data bit periods * 0x4: 3.5 nominal data bit periods * 0x5: 4.5 nominal data bit periods * 0x6: 5.5 nominal data bit periods * 0x7: 6.5 nominal data bit periods (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t CEC_CFGR_RXTOL = 0x8;          // Rx-Tolerance The RXTOL bit is set and cleared by software. ** Start-Bit, +/- 200 s rise, +/- 200 s fall. ** Data-Bit: +/- 200 s rise. +/- 350 s fall. ** Start-Bit: +/- 400 s rise, +/- 400 s fall ** Data-Bit: +/-300 s rise, +/- 500 s fall
    static constexpr uint32_t CEC_CFGR_BRESTP = 0x10;        // Rx-Stop on Bit Rising Error The BRESTP bit is set and cleared by software.
    static constexpr uint32_t CEC_CFGR_BREGEN = 0x20;        // Generate Error-Bit on Bit Rising Error The BREGEN bit is set and cleared by software. Note: If BRDNOGEN=0, an Error-bit is generated upon BRE detection with BRESTP=1 in broadcast even if BREGEN=0
    static constexpr uint32_t CEC_CFGR_LBPEGEN = 0x40;       // Generate Error-Bit on Long Bit Period Error The LBPEGEN bit is set and cleared by software. Note: If BRDNOGEN=0, an Error-bit is generated upon LBPE detection in broadcast even if LBPEGEN=0
    static constexpr uint32_t CEC_CFGR_BRDNOGEN = 0x80;      // Avoid Error-Bit Generation in Broadcast The BRDNOGEN bit is set and cleared by software.
    static constexpr uint32_t CEC_CFGR_SFTOPT = 0x100;       // SFT Option Bit The SFTOPT bit is set and cleared by software.
    template<uint32_t X>
    static constexpr uint32_t CEC_CFGR_OAR =                 // Own addresses configuration The OAR bits are set by software to select which destination logical addresses has to be considered in receive mode. Each bit, when set, enables the CEC logical address identified by the given bit position. At the end of HEADER reception, the received destination address is compared with the enabled addresses. In case of matching address, the incoming message is acknowledged and received. In case of non-matching address, the incoming message is received only in listen mode (LSTN=1), but without acknowledge sent. Broadcast messages are always received. Example: OAR = 0b000 0000 0010 0001 means that CEC acknowledges addresses 0x0 and 0x5. Consequently, each message directed to one of these addresses is received. (15 bits)
        bit_field_t<16, 0x7fff>::value<X>();
    static constexpr uint32_t CEC_CFGR_LSTN = 0x80000000;    // Listen mode LSTN bit is set and cleared by software.
    static const uint32_t CEC_CFGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CEC_TXDR_TXD =                 // Tx Data register. TXD is a write-only register containing the data byte to be transmitted. Note: TXD must be written when TXSTART=1 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CEC_TXDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CEC_RXDR_RXD =                 // Rx Data register. RXD is read-only and contains the last data byte which has been received from the CEC line. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t CEC_RXDR_RESET_VALUE = 0x0;

    static constexpr uint32_t CEC_ISR_RXBR = 0x1;           // Rx-Byte Received The RXBR bit is set by hardware to inform application that a new byte has been received from the CEC line and stored into the RXD buffer. RXBR is cleared by software write at 1.
    static constexpr uint32_t CEC_ISR_RXEND = 0x2;          // End Of Reception RXEND is set by hardware to inform application that the last byte of a CEC message is received from the CEC line and stored into the RXD buffer. RXEND is set at the same time of RXBR. RXEND is cleared by software write at 1.
    static constexpr uint32_t CEC_ISR_RXOVR = 0x4;          // Rx-Overrun RXOVR is set by hardware if RXBR is not yet cleared at the time a new byte is received on the CEC line and stored into RXD. RXOVR assertion stops message reception so that no acknowledge is sent. In case of broadcast, a negative acknowledge is sent. RXOVR is cleared by software write at 1.
    static constexpr uint32_t CEC_ISR_BRE = 0x8;            // Rx-Bit Rising Error BRE is set by hardware in case a Data-Bit waveform is detected with Bit Rising Error. BRE is set either at the time the misplaced rising edge occurs, or at the end of the maximum BRE tolerance allowed by RXTOL, in case rising edge is still longing. BRE stops message reception if BRESTP=1. BRE generates an Error-Bit on the CEC line if BREGEN=1. BRE is cleared by software write at 1.
    static constexpr uint32_t CEC_ISR_SBPE = 0x10;          // Rx-Short Bit Period Error SBPE is set by hardware in case a Data-Bit waveform is detected with Short Bit Period Error. SBPE is set at the time the anticipated falling edge occurs. SBPE generates an Error-Bit on the CEC line. SBPE is cleared by software write at 1.
    static constexpr uint32_t CEC_ISR_LBPE = 0x20;          // Rx-Long Bit Period Error LBPE is set by hardware in case a Data-Bit waveform is detected with Long Bit Period Error. LBPE is set at the end of the maximum bit-extension tolerance allowed by RXTOL, in case falling edge is still longing. LBPE always stops reception of the CEC message. LBPE generates an Error-Bit on the CEC line if LBPEGEN=1. In case of broadcast, Error-Bit is generated even in case of LBPEGEN=0. LBPE is cleared by software write at 1.
    static constexpr uint32_t CEC_ISR_RXACKE = 0x40;        // Rx-Missing Acknowledge In receive mode, RXACKE is set by hardware to inform application that no acknowledge was seen on the CEC line. RXACKE applies only for broadcast messages and in listen mode also for not directly addressed messages (destination address not enabled in OAR). RXACKE aborts message reception. RXACKE is cleared by software write at 1.
    static constexpr uint32_t CEC_ISR_ARBLST = 0x80;        // Arbitration Lost ARBLST is set by hardware to inform application that CEC device is switching to reception due to arbitration lost event following the TXSOM command. ARBLST can be due either to a contending CEC device starting earlier or starting at the same time but with higher HEADER priority. After ARBLST assertion TXSOM bit keeps pending for next transmission attempt. ARBLST is cleared by software write at 1.
    static constexpr uint32_t CEC_ISR_TXBR = 0x100;         // Tx-Byte Request TXBR is set by hardware to inform application that the next transmission data has to be written to TXDR. TXBR is set when the 4th bit of currently transmitted byte is sent. Application must write the next byte to TXDR within 6 nominal data-bit periods before transmission underrun error occurs (TXUDR). TXBR is cleared by software write at 1.
    static constexpr uint32_t CEC_ISR_TXEND = 0x200;        // End of Transmission TXEND is set by hardware to inform application that the last byte of the CEC message has been successfully transmitted. TXEND clears the TXSOM and TXEOM control bits. TXEND is cleared by software write at 1.
    static constexpr uint32_t CEC_ISR_TXUDR = 0x400;        // Tx-Buffer Underrun In transmission mode, TXUDR is set by hardware if application was not in time to load TXDR before of next byte transmission. TXUDR aborts message transmission and clears TXSOM and TXEOM control bits. TXUDR is cleared by software write at 1
    static constexpr uint32_t CEC_ISR_TXERR = 0x800;        // Tx-Error In transmission mode, TXERR is set by hardware if the CEC initiator detects low impedance on the CEC line while it is released. TXERR aborts message transmission and clears TXSOM and TXEOM controls. TXERR is cleared by software write at 1.
    static constexpr uint32_t CEC_ISR_TXACKE = 0x1000;      // Tx-Missing Acknowledge Error In transmission mode, TXACKE is set by hardware to inform application that no acknowledge was received. In case of broadcast transmission, TXACKE informs application that a negative acknowledge was received. TXACKE aborts message transmission and clears TXSOM and TXEOM controls. TXACKE is cleared by software write at 1.
    static const uint32_t CEC_ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t CEC_IER_RXBRIE = 0x1;         // Rx-Byte Received Interrupt Enable The RXBRIE bit is set and cleared by software.
    static constexpr uint32_t CEC_IER_RXENDIE = 0x2;        // End Of Reception Interrupt Enable The RXENDIE bit is set and cleared by software.
    static constexpr uint32_t CEC_IER_RXOVRIE = 0x4;        // Rx-Buffer Overrun Interrupt Enable The RXOVRIE bit is set and cleared by software.
    static constexpr uint32_t CEC_IER_BREIE = 0x8;          // Bit Rising Error Interrupt Enable The BREIE bit is set and cleared by software.
    static constexpr uint32_t CEC_IER_SBPEIE = 0x10;        // Short Bit Period Error Interrupt Enable The SBPEIE bit is set and cleared by software.
    static constexpr uint32_t CEC_IER_LBPEIE = 0x20;        // Long Bit Period Error Interrupt Enable The LBPEIE bit is set and cleared by software.
    static constexpr uint32_t CEC_IER_RXACKIE = 0x40;       // Rx-Missing Acknowledge Error Interrupt Enable The RXACKIE bit is set and cleared by software.
    static constexpr uint32_t CEC_IER_ARBLSTIE = 0x80;      // Arbitration Lost Interrupt Enable The ARBLSTIE bit is set and cleared by software.
    static constexpr uint32_t CEC_IER_TXBRIE = 0x100;       // Tx-Byte Request Interrupt Enable The TXBRIE bit is set and cleared by software.
    static constexpr uint32_t CEC_IER_TXENDIE = 0x200;      // Tx-End Of Message Interrupt Enable The TXENDIE bit is set and cleared by software.
    static constexpr uint32_t CEC_IER_TXUDRIE = 0x400;      // Tx-Underrun Interrupt Enable The TXUDRIE bit is set and cleared by software.
    static constexpr uint32_t CEC_IER_TXERRIE = 0x800;      // Tx-Error Interrupt Enable The TXERRIE bit is set and cleared by software.
    static constexpr uint32_t CEC_IER_TXACKIE = 0x1000;     // Tx-Missing Acknowledge Error Interrupt Enable The TXACKEIE bit is set and cleared by software.
    static const uint32_t CEC_IER_RESET_VALUE = 0x0;
};

static hdmi_cec_t& HDMI_CEC = *reinterpret_cast<hdmi_cec_t*>(0x40007800);

#define HAVE_PERIPHERAL_HDMI_CEC


////
//
//    DAC
//
////

struct dac_t
{
    volatile uint32_t    CR;                   // [Read-write] DAC control register
    volatile uint32_t    SWTRGR;               // [Write-only] DAC software trigger register
    volatile uint32_t    DHR12R1;              // [Read-write] DAC channel1 12-bit right-aligned data holding register
    volatile uint32_t    DHR12L1;              // [Read-write] DAC channel1 12-bit left aligned data holding register
    volatile uint32_t    DHR8R1;               // [Read-write] DAC channel1 8-bit right aligned data holding register
    volatile uint32_t    DHR12R2;              // [Read-write] DAC channel2 12-bit right aligned data holding register
    volatile uint32_t    DHR12L2;              // [Read-write] DAC channel2 12-bit left aligned data holding register
    volatile uint32_t    DHR8R2;               // [Read-write] DAC channel2 8-bit right-aligned data holding register
    volatile uint32_t    DHR12RD;              // [Read-write] Dual DAC 12-bit right-aligned data holding register
    volatile uint32_t    DHR12LD;              // [Read-write] DUAL DAC 12-bit left aligned data holding register
    volatile uint32_t    DHR8RD;               // [Read-write] DUAL DAC 8-bit right aligned data holding register
    volatile uint32_t    DOR1;                 // [Read-only] DAC channel1 data output register
    volatile uint32_t    DOR2;                 // [Read-only] DAC channel2 data output register
    volatile uint32_t    SR;                   // DAC status register
    volatile uint32_t    CCR;                  // [Read-write] DAC calibration control register
    volatile uint32_t    MCR;                  // [Read-write] DAC mode control register
    volatile uint32_t    SHSR1;                // [Read-write] DAC Sample and Hold sample time register 1
    volatile uint32_t    SHSR2;                // [Read-write] DAC Sample and Hold sample time register 2
    volatile uint32_t    SHHR;                 // [Read-write] DAC Sample and Hold hold time register
    volatile uint32_t    SHRR;                 // [Read-write] DAC Sample and Hold refresh time register
    reserved_t<232>      _0;
    volatile uint32_t    IP_HWCFGR0;           // [Read-write] DAC IP Hardware Configuration Register
    volatile uint32_t    VERR;                 // [Read-only] EXTI IP Version register
    volatile uint32_t    IPIDR;                // [Read-only] EXTI Identification register
    volatile uint32_t    SIDR;                 // [Read-only] EXTI Size ID register

    static constexpr uint32_t CR_EN1 = 0x1;            // DAC channel1 enable This bit is set and cleared by software to enable/disable DAC channel1.
    static constexpr uint32_t CR_TEN1 = 0x2;           // DAC channel1 trigger enable
    template<uint32_t X>
    static constexpr uint32_t CR_TSEL1 =               // DAC channel1 trigger selection These bits select the external event used to trigger DAC channel1. Note: Only used if bit TEN1 = 1 (DAC channel1 trigger enabled). (4 bits)
        bit_field_t<2, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_WAVE1 =               // DAC channel1 noise/triangle wave generation enable These bits are set and cleared by software. Note: Only used if bit TEN1 = 1 (DAC channel1 trigger enabled). (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_MAMP1 =               // DAC channel1 mask/amplitude selector These bits are written by software to select mask in wave generation mode or amplitude in triangle generation mode. = 1011: Unmask bits[11:0] of LFSR/ triangle amplitude equal to 4095 (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t CR_DMAEN1 = 0x1000;      // DAC channel1 DMA enable This bit is set and cleared by software.
    static constexpr uint32_t CR_DMAUDRIE1 = 0x2000;   // DAC channel1 DMA Underrun Interrupt enable This bit is set and cleared by software.
    static constexpr uint32_t CR_CEN1 = 0x4000;        // DAC Channel 1 calibration enable This bit is set and cleared by software to enable/disable DAC channel 1 calibration, it can be written only if bit EN1=0 into DAC_CR (the calibration mode can be entered/exit only when the DAC channel is disabled) Otherwise, the write operation is ignored.
    static constexpr uint32_t CR_EN2 = 0x10000;        // DAC channel2 enable This bit is set and cleared by software to enable/disable DAC channel2.
    static constexpr uint32_t CR_TEN2 = 0x20000;       // DAC channel2 trigger enable
    template<uint32_t X>
    static constexpr uint32_t CR_TSEL2 =               // DAC channel2 trigger selection These bits select the external event used to trigger DAC channel2 Note: Only used if bit TEN2 = 1 (DAC channel2 trigger enabled). (4 bits)
        bit_field_t<18, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_WAVE2 =               // DAC channel2 noise/triangle wave generation enable These bits are set/reset by software. 1x: Triangle wave generation enabled Note: Only used if bit TEN2 = 1 (DAC channel2 trigger enabled) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_MAMP2 =               // DAC channel2 mask/amplitude selector These bits are written by software to select mask in wave generation mode or amplitude in triangle generation mode. = 1011: Unmask bits[11:0] of LFSR/ triangle amplitude equal to 4095 (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t CR_DMAEN2 = 0x10000000;  // DAC channel2 DMA enable This bit is set and cleared by software.
    static constexpr uint32_t CR_DMAUDRIE2 = 0x20000000;// DAC channel2 DMA underrun interrupt enable This bit is set and cleared by software.
    static constexpr uint32_t CR_CEN2 = 0x40000000;    // DAC Channel 2 calibration enable This bit is set and cleared by software to enable/disable DAC channel 2 calibration, it can be written only if bit EN2=0 into DAC_CR (the calibration mode can be entered/exit only when the DAC channel is disabled) Otherwise, the write operation is ignored.
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t SWTRGR_SWTRIG1 = 0x1;        // DAC channel1 software trigger This bit is set by software to trigger the DAC in software trigger mode. Note: This bit is cleared by hardware (one APB1 clock cycle later) once the DAC_DHR1 register value has been loaded into the DAC_DOR1 register.
    static constexpr uint32_t SWTRGR_SWTRIG2 = 0x2;        // DAC channel2 software trigger This bit is set by software to trigger the DAC in software trigger mode. Note: This bit is cleared by hardware (one APB1 clock cycle later) once the DAC_DHR2 register value has been loaded into the DAC_DOR2 register.
    static const uint32_t SWTRGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR12R1_DACC1DHR =            // DAC channel1 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t DHR12R1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR12L1_DACC1DHR =            // DAC channel1 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    static const uint32_t DHR12L1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR8R1_DACC1DHR =            // DAC channel1 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel1. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t DHR8R1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR12R2_DACC2DHR =            // DAC channel2 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t DHR12R2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR12L2_DACC2DHR =            // DAC channel2 12-bit left-aligned data These bits are written by software which specify 12-bit data for DAC channel2. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    static const uint32_t DHR12L2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR8R2_DACC2DHR =            // DAC channel2 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel2. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t DHR8R2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR12RD_DACC1DHR =            // DAC channel1 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DHR12RD_DACC2DHR =            // DAC channel2 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DHR12RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR12LD_DACC1DHR =            // DAC channel1 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DHR12LD_DACC2DHR =            // DAC channel2 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DHR12LD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DHR8RD_DACC1DHR =            // DAC channel1 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel1. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DHR8RD_DACC2DHR =            // DAC channel2 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel2. (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DHR8RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DOR1_DACC1DOR =            // DAC channel1 data output These bits are read-only, they contain data output for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t DOR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DOR2_DACC2DOR =            // DAC channel2 data output These bits are read-only, they contain data output for DAC channel2. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t DOR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_DMAUDR1 = 0x2000;     // DAC channel1 DMA underrun flag This bit is set by hardware and cleared by software (by writing it to 1)., Read-write
    static constexpr uint32_t SR_CAL_FLAG1 = 0x4000;   // DAC Channel 1 calibration offset status This bit is set and cleared by hardware, Read-only
    static constexpr uint32_t SR_BWST1 = 0x8000;       // DAC Channel 1 busy writing sample time flag This bit is systematically set just after Sample &amp; Hold mode enable and is set each time the software writes the register DAC_SHSR1, It is cleared by hardware when the write operation of DAC_SHSR1 is complete. (It takes about 3LSI periods of synchronization)., Read-only
    static constexpr uint32_t SR_DMAUDR2 = 0x20000000; // DAC channel2 DMA underrun flag This bit is set by hardware and cleared by software (by writing it to 1)., Read-write
    static constexpr uint32_t SR_CAL_FLAG2 = 0x40000000;// DAC Channel 2 calibration offset status This bit is set and cleared by hardware, Read-only
    static constexpr uint32_t SR_BWST2 = 0x80000000;   // DAC Channel 2 busy writing sample time flag This bit is systematically set just after Sample &amp; Hold mode enable and is set each time the software writes the register DAC_SHSR2, It is cleared by hardware when the write operation of DAC_SHSR2 is complete. (It takes about 3 LSI periods of synchronization)., Read-only
    static const uint32_t SR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR_OTRIM1 =              // DAC Channel 1 offset trimming value (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR_OTRIM2 =              // DAC Channel 2 offset trimming value (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static const uint32_t CCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t MCR_MODE1 =               // DAC Channel 1 mode These bits can be written only when the DAC is disabled and not in the calibration mode (when bit EN1=0 and bit CEN1 =0 in the DAC_CR register). If EN1=1 or CEN1 =1 the write operation is ignored. They can be set and cleared by software to select the DAC Channel 1 mode: DAC Channel 1 in normal Mode DAC Channel 1 in sample &amp;amp; hold mode (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t MCR_MODE2 =               // DAC Channel 2 mode These bits can be written only when the DAC is disabled and not in the calibration mode (when bit EN2=0 and bit CEN2 =0 in the DAC_CR register). If EN2=1 or CEN2 =1 the write operation is ignored. They can be set and cleared by software to select the DAC Channel 2 mode: DAC Channel 2 in normal Mode DAC Channel 2 in sample &amp;amp; hold mode (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static const uint32_t MCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SHSR1_TSAMPLE1 =            // DAC Channel 1 sample Time (only valid in sample &amp;amp; hold mode) These bits can be written when the DAC channel1 is disabled or also during normal operation. in the latter case, the write can be done only when BWSTx of DAC_SR register is low, If BWSTx=1, the write operation is ignored. (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t SHSR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SHSR2_TSAMPLE2 =            // DAC Channel 2 sample Time (only valid in sample &amp;amp; hold mode) These bits can be written when the DAC channel2 is disabled or also during normal operation. in the latter case, the write can be done only when BWSTx of DAC_SR register is low, if BWSTx=1, the write operation is ignored. (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t SHSR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SHHR_THOLD1 =              // DAC Channel 1 hold Time (only valid in sample &amp;amp; hold mode) Hold time= (THOLD[9:0]) x T LSI (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SHHR_THOLD2 =              // DAC Channel 2 hold time (only valid in sample &amp;amp; hold mode). Hold time= (THOLD[9:0]) x T LSI (10 bits)
        bit_field_t<16, 0x3ff>::value<X>();
    static const uint32_t SHHR_RESET_VALUE = 0x10001;

    template<uint32_t X>
    static constexpr uint32_t SHRR_TREFRESH1 =           // DAC Channel 1 refresh Time (only valid in sample &amp;amp; hold mode) Refresh time= (TREFRESH[7:0]) x T LSI (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SHRR_TREFRESH2 =           // DAC Channel 2 refresh Time (only valid in sample &amp;amp; hold mode) Refresh time= (TREFRESH[7:0]) x T LSI (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static const uint32_t SHRR_RESET_VALUE = 0x10001;

    template<uint32_t X>
    static constexpr uint32_t IP_HWCFGR0_DUAL =                // Dual DAC capability (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IP_HWCFGR0_LFSR =                // Pseudonoise wave generation capability (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IP_HWCFGR0_TRIANGLE =            // Triangle wave generation capability (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IP_HWCFGR0_SAMPLE =              // Sample and hold mode capability (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IP_HWCFGR0_OR_CFG =              // option register bit width (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static const uint32_t IP_HWCFGR0_RESET_VALUE = 0x1111;

    template<uint32_t X>
    static constexpr uint32_t VERR_MINREV =              // Minor Revision number (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t VERR_MAJREV =              // Major Revision number (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    static const uint32_t VERR_RESET_VALUE = 0x31;


    static const uint32_t IPIDR_RESET_VALUE = 0x110011;


    static const uint32_t SIDR_RESET_VALUE = 0xa3c5dd01;
};

static dac_t& DAC = *reinterpret_cast<dac_t*>(0x40007400);

#define HAVE_PERIPHERAL_DAC


////
//
//    Inter-integrated circuit
//
////

struct i2c1_t
{
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
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

    static constexpr uint32_t CR1_PE = 0x1;             // Peripheral enable
    static constexpr uint32_t CR1_TXIE = 0x2;           // TX Interrupt enable
    static constexpr uint32_t CR1_RXIE = 0x4;           // RX Interrupt enable
    static constexpr uint32_t CR1_ADDRIE = 0x8;         // Address match interrupt enable (slave only)
    static constexpr uint32_t CR1_NACKIE = 0x10;        // Not acknowledge received interrupt enable
    static constexpr uint32_t CR1_STOPIE = 0x20;        // STOP detection Interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transfer Complete interrupt enable
    static constexpr uint32_t CR1_ERRIE = 0x80;         // Error interrupts enable
    template<uint32_t X>
    static constexpr uint32_t CR1_DNF =                 // Digital noise filter (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t CR1_ANFOFF = 0x1000;      // Analog noise filter OFF
    static constexpr uint32_t CR1_TXDMAEN = 0x4000;     // DMA transmission requests enable
    static constexpr uint32_t CR1_RXDMAEN = 0x8000;     // DMA reception requests enable
    static constexpr uint32_t CR1_SBC = 0x10000;        // Slave byte control
    static constexpr uint32_t CR1_NOSTRETCH = 0x20000;  // Clock stretching disable
    static constexpr uint32_t CR1_WUPEN = 0x40000;      // Wakeup from STOP enable
    static constexpr uint32_t CR1_GCEN = 0x80000;       // General call enable
    static constexpr uint32_t CR1_SMBHEN = 0x100000;    // SMBus Host address enable
    static constexpr uint32_t CR1_SMBDEN = 0x200000;    // SMBus Device Default address enable
    static constexpr uint32_t CR1_ALERTEN = 0x400000;   // SMBUS alert enable
    static constexpr uint32_t CR1_PECEN = 0x800000;     // PEC enable
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
    static constexpr uint32_t CR2_SADD =                // Slave address bit (master mode) (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t OAR1_OA1_0 = 0x1;          // Interface address
    template<uint32_t X>
    static constexpr uint32_t OAR1_OA1_7_1 =             // Interface address (7 bits)
        bit_field_t<1, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OAR1_OA1_8_9 =             // Interface address (2 bits)
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
    volatile uint32_t    CR1;                  // [Read-write] Control register 1
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

    static constexpr uint32_t CR1_PE = 0x1;             // Peripheral enable
    static constexpr uint32_t CR1_TXIE = 0x2;           // TX Interrupt enable
    static constexpr uint32_t CR1_RXIE = 0x4;           // RX Interrupt enable
    static constexpr uint32_t CR1_ADDRIE = 0x8;         // Address match interrupt enable (slave only)
    static constexpr uint32_t CR1_NACKIE = 0x10;        // Not acknowledge received interrupt enable
    static constexpr uint32_t CR1_STOPIE = 0x20;        // STOP detection Interrupt enable
    static constexpr uint32_t CR1_TCIE = 0x40;          // Transfer Complete interrupt enable
    static constexpr uint32_t CR1_ERRIE = 0x80;         // Error interrupts enable
    template<uint32_t X>
    static constexpr uint32_t CR1_DNF =                 // Digital noise filter (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t CR1_ANFOFF = 0x1000;      // Analog noise filter OFF
    static constexpr uint32_t CR1_TXDMAEN = 0x4000;     // DMA transmission requests enable
    static constexpr uint32_t CR1_RXDMAEN = 0x8000;     // DMA reception requests enable
    static constexpr uint32_t CR1_SBC = 0x10000;        // Slave byte control
    static constexpr uint32_t CR1_NOSTRETCH = 0x20000;  // Clock stretching disable
    static constexpr uint32_t CR1_WUPEN = 0x40000;      // Wakeup from STOP enable
    static constexpr uint32_t CR1_GCEN = 0x80000;       // General call enable
    static constexpr uint32_t CR1_SMBHEN = 0x100000;    // SMBus Host address enable
    static constexpr uint32_t CR1_SMBDEN = 0x200000;    // SMBus Device Default address enable
    static constexpr uint32_t CR1_ALERTEN = 0x400000;   // SMBUS alert enable
    static constexpr uint32_t CR1_PECEN = 0x800000;     // PEC enable
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
    static constexpr uint32_t CR2_SADD =                // Slave address bit (master mode) (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t OAR1_OA1_0 = 0x1;          // Interface address
    template<uint32_t X>
    static constexpr uint32_t OAR1_OA1_7_1 =             // Interface address (7 bits)
        bit_field_t<1, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OAR1_OA1_8_9 =             // Interface address (2 bits)
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
//    Real-time clock
//
////

struct rtc_t
{
    volatile uint32_t    TR;                   // [Read-write] time register
    volatile uint32_t    DR;                   // [Read-write] date register
    volatile uint32_t    SSR;                  // [Read-only] sub second register
    volatile uint32_t    ICSR;                 // initialization and status register
    volatile uint32_t    PRER;                 // [Read-write] prescaler register
    volatile uint32_t    WUTR;                 // [Read-write] wakeup timer register
    volatile uint32_t    CR;                   // [Read-write] control register
    reserved_t<2>        _0;
    volatile uint32_t    WPR;                  // [Write-only] write protection register
    volatile uint32_t    CALR;                 // [Read-write] calibration register
    volatile uint32_t    SHIFTR;               // [Write-only] shift control register
    volatile uint32_t    TSTR;                 // [Read-only] time stamp time register
    volatile uint32_t    TSDR;                 // [Read-only] time stamp date register
    volatile uint32_t    TSSSR;                // [Read-only] timestamp sub second register
    reserved_t<1>        _1;
    volatile uint32_t    ALRMAR;               // [Read-write] alarm A register
    volatile uint32_t    ALRMASSR;             // [Read-write] alarm A sub second register
    volatile uint32_t    ALRMBR;               // [Read-write] alarm B register
    volatile uint32_t    ALRMBSSR;             // [Read-write] alarm B sub second register
    volatile uint32_t    SR;                   // [Read-only] status register
    volatile uint32_t    MISR;                 // [Read-only] masked interrupt status register
    reserved_t<1>        _2;
    volatile uint32_t    SCR;                  // [Read-write] status clear register
    reserved_t<228>      _3;
    volatile uint32_t    HWCFGR;               // [Read-write] hardware configuration register
    volatile uint32_t    VERR;                 // [Read-only] EXTI IP Version register
    volatile uint32_t    IPIDR;                // [Read-only] EXTI Identification register
    volatile uint32_t    SIDR;                 // [Read-only] EXTI Size ID register

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

    template<uint32_t X>
    static constexpr uint32_t SSR_SS =                  // Sub second value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t SSR_RESET_VALUE = 0x0;

    static constexpr uint32_t ICSR_ALRAWF = 0x1;         // Alarm A write flag, Read-only
    static constexpr uint32_t ICSR_ALRBWF = 0x2;         // Alarm B write flag, Read-only
    static constexpr uint32_t ICSR_WUTWF = 0x4;          // Wakeup timer write flag, Read-only
    static constexpr uint32_t ICSR_SHPF = 0x8;           // Shift operation pending, Read-write
    static constexpr uint32_t ICSR_INITS = 0x10;         // Initialization status flag, Read-only
    static constexpr uint32_t ICSR_RSF = 0x20;           // Registers synchronization flag, Read-write
    static constexpr uint32_t ICSR_INITF = 0x40;         // Initialization flag, Read-only
    static constexpr uint32_t ICSR_INIT = 0x80;          // Initialization mode, Read-write
    static constexpr uint32_t ICSR_RECALPF = 0x10000;    // Recalibration pending Flag, Read-only
    static const uint32_t ICSR_RESET_VALUE = 0x7;

    template<uint32_t X>
    static constexpr uint32_t PRER_PREDIV_A =            // Asynchronous prescaler factor (7 bits)
        bit_field_t<16, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PRER_PREDIV_S =            // Synchronous prescaler factor (15 bits)
        bit_field_t<0, 0x7fff>::value<X>();
    static const uint32_t PRER_RESET_VALUE = 0x7f00ff;

    template<uint32_t X>
    static constexpr uint32_t WUTR_WUT =                 // Wakeup auto-reload value bits (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t WUTR_RESET_VALUE = 0xffff;

    template<uint32_t X>
    static constexpr uint32_t CR_WUCKSEL =             // WUCKSEL (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t CR_TSEDGE = 0x8;         // TSEDGE
    static constexpr uint32_t CR_REFCKON = 0x10;       // REFCKON
    static constexpr uint32_t CR_BYPSHAD = 0x20;       // BYPSHAD
    static constexpr uint32_t CR_FMT = 0x40;           // FMT
    static constexpr uint32_t CR_ALRAE = 0x100;        // ALRAE
    static constexpr uint32_t CR_ALRBE = 0x200;        // ALRBE
    static constexpr uint32_t CR_WUTE = 0x400;         // WUTE
    static constexpr uint32_t CR_TSE = 0x800;          // TSE
    static constexpr uint32_t CR_ALRAIE = 0x1000;      // ALRAIE
    static constexpr uint32_t CR_ALRBIE = 0x2000;      // ALRBIE
    static constexpr uint32_t CR_WUTIE = 0x4000;       // WUTIE
    static constexpr uint32_t CR_TSIE = 0x8000;        // TSIE
    static constexpr uint32_t CR_ADD1H = 0x10000;      // ADD1H
    static constexpr uint32_t CR_SUB1H = 0x20000;      // SUB1H
    static constexpr uint32_t CR_BKP = 0x40000;        // BKP
    static constexpr uint32_t CR_COSEL = 0x80000;      // COSEL
    static constexpr uint32_t CR_POL = 0x100000;       // POL
    template<uint32_t X>
    static constexpr uint32_t CR_OSEL =                // OSEL (2 bits)
        bit_field_t<21, 0x3>::value<X>();
    static constexpr uint32_t CR_COE = 0x800000;       // COE
    static constexpr uint32_t CR_ITSE = 0x1000000;     // ITSE
    static constexpr uint32_t CR_TAMPTS = 0x2000000;   // TAMPTS
    static constexpr uint32_t CR_TAMPOE = 0x4000000;   // TAMPOE
    static constexpr uint32_t CR_TAMPALRM_PU = 0x20000000;// TAMPALRM_PU
    static constexpr uint32_t CR_TAMPALRM_TYPE = 0x40000000;// TAMPALRM_TYPE
    static constexpr uint32_t CR_OUT2EN = 0x80000000;  // OUT2EN
    static const uint32_t CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t WPR_KEY =                 // Write protection key (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t WPR_RESET_VALUE = 0x0;

    static constexpr uint32_t CALR_CALP = 0x8000;        // Increase frequency of RTC by 488.5 ppm
    static constexpr uint32_t CALR_CALW8 = 0x4000;       // Use an 8-second calibration cycle period
    static constexpr uint32_t CALR_CALW16 = 0x2000;      // Use a 16-second calibration cycle period
    template<uint32_t X>
    static constexpr uint32_t CALR_CALM =                // Calibration minus (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t CALR_RESET_VALUE = 0x0;

    static constexpr uint32_t SHIFTR_ADD1S = 0x80000000;   // Add one second
    template<uint32_t X>
    static constexpr uint32_t SHIFTR_SUBFS =               // Subtract a fraction of a second (15 bits)
        bit_field_t<0, 0x7fff>::value<X>();
    static const uint32_t SHIFTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TSTR_SU =                  // Second units in BCD format (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSTR_ST =                  // Second tens in BCD format (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSTR_MNU =                 // Minute units in BCD format (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSTR_MNT =                 // Minute tens in BCD format (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSTR_HU =                  // Hour units in BCD format (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSTR_HT =                  // Hour tens in BCD format (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    static constexpr uint32_t TSTR_PM = 0x400000;        // AM/PM notation
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

    static constexpr uint32_t ALRMAR_MSK4 = 0x80000000;    // Alarm A date mask
    static constexpr uint32_t ALRMAR_WDSEL = 0x40000000;   // Week day selection
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_DT =                  // Date tens in BCD format (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_DU =                  // Date units or day in BCD format (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t ALRMAR_MSK3 = 0x800000;      // Alarm A hours mask
    static constexpr uint32_t ALRMAR_PM = 0x400000;        // AM/PM notation
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_HT =                  // Hour tens in BCD format (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_HU =                  // Hour units in BCD format (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    static constexpr uint32_t ALRMAR_MSK2 = 0x8000;        // Alarm A minutes mask
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_MNT =                 // Minute tens in BCD format (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_MNU =                 // Minute units in BCD format (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t ALRMAR_MSK1 = 0x80;          // Alarm A seconds mask
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_ST =                  // Second tens in BCD format (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMAR_SU =                  // Second units in BCD format (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t ALRMAR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ALRMASSR_MASKSS =              // Mask the most-significant bits starting at this bit (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMASSR_SS =                  // Sub seconds value (15 bits)
        bit_field_t<0, 0x7fff>::value<X>();
    static const uint32_t ALRMASSR_RESET_VALUE = 0x0;

    static constexpr uint32_t ALRMBR_MSK4 = 0x80000000;    // Alarm B date mask
    static constexpr uint32_t ALRMBR_WDSEL = 0x40000000;   // Week day selection
    template<uint32_t X>
    static constexpr uint32_t ALRMBR_DT =                  // Date tens in BCD format (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMBR_DU =                  // Date units or day in BCD format (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t ALRMBR_MSK3 = 0x800000;      // Alarm B hours mask
    static constexpr uint32_t ALRMBR_PM = 0x400000;        // AM/PM notation
    template<uint32_t X>
    static constexpr uint32_t ALRMBR_HT =                  // Hour tens in BCD format (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMBR_HU =                  // Hour units in BCD format (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    static constexpr uint32_t ALRMBR_MSK2 = 0x8000;        // Alarm B minutes mask
    template<uint32_t X>
    static constexpr uint32_t ALRMBR_MNT =                 // Minute tens in BCD format (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMBR_MNU =                 // Minute units in BCD format (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t ALRMBR_MSK1 = 0x80;          // Alarm B seconds mask
    template<uint32_t X>
    static constexpr uint32_t ALRMBR_ST =                  // Second tens in BCD format (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMBR_SU =                  // Second units in BCD format (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t ALRMBR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ALRMBSSR_MASKSS =              // Mask the most-significant bits starting at this bit (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ALRMBSSR_SS =                  // Sub seconds value (15 bits)
        bit_field_t<0, 0x7fff>::value<X>();
    static const uint32_t ALRMBSSR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_ALRAF = 0x1;          // ALRAF
    static constexpr uint32_t SR_ALRBF = 0x2;          // ALRBF
    static constexpr uint32_t SR_WUTF = 0x4;           // WUTF
    static constexpr uint32_t SR_TSF = 0x8;            // TSF
    static constexpr uint32_t SR_TSOVF = 0x10;         // TSOVF
    static constexpr uint32_t SR_ITSF = 0x20;          // ITSF
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t MISR_ALRAMF = 0x1;         // ALRAMF
    static constexpr uint32_t MISR_ALRBMF = 0x2;         // ALRBMF
    static constexpr uint32_t MISR_WUTMF = 0x4;          // WUTMF
    static constexpr uint32_t MISR_TSMF = 0x8;           // TSMF
    static constexpr uint32_t MISR_TSOVMF = 0x10;        // TSOVMF
    static constexpr uint32_t MISR_ITSMF = 0x20;         // ITSMF
    static const uint32_t MISR_RESET_VALUE = 0x0;

    static constexpr uint32_t SCR_CALRAF = 0x1;         // CALRAF
    static constexpr uint32_t SCR_CALRBF = 0x2;         // CALRBF
    static constexpr uint32_t SCR_CWUTF = 0x4;          // CWUTF
    static constexpr uint32_t SCR_CTSF = 0x8;           // CTSF
    static constexpr uint32_t SCR_CTSOVF = 0x10;        // CTSOVF
    static constexpr uint32_t SCR_CITSF = 0x20;         // CITSF
    static const uint32_t SCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t HWCFGR_ALARMB =              // ALARMB (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_WAKEUP =              // WAKEUP (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_SMOOTH_CALIB =        // SMOOTH_CALIB (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_TIMESTAMP =           // TIMESTAMP (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_OPTIONREG_OUT =       // OPTIONREG_OUT (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HWCFGR_TRUST_ZONE =          // TRUST_ZONE (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static const uint32_t HWCFGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t VERR_MINREV =              // Minor Revision number (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t VERR_MAJREV =              // Major Revision number (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    static const uint32_t VERR_RESET_VALUE = 0x10;


    static const uint32_t IPIDR_RESET_VALUE = 0x120033;


    static const uint32_t SIDR_RESET_VALUE = 0xa3c5dd01;

    static constexpr uint8_t RTC_STAMP = 2; // RTC and TAMP interrupts
};

static rtc_t& RTC = *reinterpret_cast<rtc_t*>(0x40002800);

#define HAVE_PERIPHERAL_RTC


////
//
//    General purpose timers
//
////

struct tim14_t
{
    volatile uint32_t    CR1;                  // [Read-write] control register 1
    reserved_t<2>        _0;
    volatile uint32_t    DIER;                 // [Read-write] DMA/Interrupt enable register
    volatile uint32_t    SR;                   // [Read-write] status register
    volatile uint32_t    EGR;                  // [Write-only] event generation register
    volatile uint32_t    CCMR1;                // [Read-write] capture/compare mode register 1 (output mode)
    reserved_t<1>        _1;
    volatile uint32_t    CCER;                 // [Read-write] capture/compare enable register
    volatile uint32_t    CNT;                  // [Read-write] counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    reserved_t<1>        _2;
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    reserved_t<12>       _3;
    volatile uint32_t    TISEL;                // [Read-write] TIM timer input selection register

    static constexpr uint32_t CR1_UIFREMAP = 0x800;     // UIF status bit remapping
    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
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
    static constexpr uint32_t CCMR1_CC1S =                // CC1S (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1F =                // Input capture 1 filter (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_ICPCS =               // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    static constexpr uint32_t CCMR1_OC1CE = 0x80;         // OC1CE
    static constexpr uint32_t CCMR1_OC1FE = 0x4;          // OC1FE
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC1M =                // OC1M (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC1M_3 = 0x10000;     // Output Compare 1 mode - bit 3
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // OC1PE
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC1NP = 0x8;          // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1P = 0x2;           // Capture/Compare 1 output Polarity
    static constexpr uint32_t CCER_CC1E = 0x1;           // Capture/Compare 1 output enable
    static const uint32_t CCER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // low counter value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIF Copy
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Low Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR1_CCR1 =                // Low Capture/Compare 1 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TISEL =               // TI1[0] to TI1[15] input selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM14 = 19; // TIM14 global interrupt
};

static tim14_t& TIM14 = *reinterpret_cast<tim14_t*>(0x40002000);

#define HAVE_PERIPHERAL_TIM14


////
//
//    Basic timers
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

    static constexpr uint32_t CR1_UIFREMAP = 0x800;     // UIF status bit remapping
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
    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIF Copy
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Low Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM6_DAC_LPTIM1 = 17; // TIM6 + LPTIM1 and DAC global interrupt
};

static tim6_t& TIM6 = *reinterpret_cast<tim6_t*>(0x40001000);

#define HAVE_PERIPHERAL_TIM6


////
//
//    Basic timers
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

    static constexpr uint32_t CR1_UIFREMAP = 0x800;     // UIF status bit remapping
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
    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIF Copy
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Low Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM7_LPTIM2 = 18; // TIM7 + LPTIM2 global interrupt
};

static tim7_t& TIM7 = *reinterpret_cast<tim7_t*>(0x40001400);

#define HAVE_PERIPHERAL_TIM7


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
    volatile uint32_t    OR1;                  // [Read-write] TIM option register
    reserved_t<3>        _2;
    volatile uint32_t    AF1;                  // [Read-write] TIM alternate function option register 1
    reserved_t<1>        _3;
    volatile uint32_t    TISEL;                // [Read-write] TIM alternate function option register 1

    static constexpr uint32_t CR1_UIFREMAP = 0x800;     // UIF status bit remapping
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

    template<uint32_t X>
    static constexpr uint32_t SMCR_TS_4_3 =              // Trigger selection (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    static constexpr uint32_t SMCR_SMS_3 = 0x10000;      // Slave mode selection - bit 3
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
    static constexpr uint32_t SMCR_OCCS = 0x8;           // OCREF clear selection
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
    static constexpr uint32_t CCMR1_OC1M_3 = 0x10000;     // Output Compare 1 mode - bit 3
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output compare 1 preload enable
    static constexpr uint32_t CCMR1_OC2CE = 0x8000;       // Output compare 2 clear enable
    static constexpr uint32_t CCMR1_OC2FE = 0x400;        // Output compare 2 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC2M =                // Output compare 2 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC2M_3 = 0x1000000;   // Output Compare 2 mode - bit 3
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
    static constexpr uint32_t CCMR2_OC3M_3 = 0x10000;     // Output Compare 3 mode - bit 3
    static constexpr uint32_t CCMR2_OC3PE = 0x8;          // Output compare 3 preload enable
    static constexpr uint32_t CCMR2_OC4CE = 0x8000;       // Output compare 4 clear enable
    static constexpr uint32_t CCMR2_OC4FE = 0x400;        // Output compare 4 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC4M =                // Output compare 4 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC4M_3 = 0x1000000;   // Output Compare 4 mode - bit 3
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
    static constexpr uint32_t DMAR_DMAB =                // DMA register for burst accesses (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint32_t OR1_IOCREF_CLR = 0x1;     // IOCREF_CLR
    static const uint32_t OR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF1_ETRSEL =              // External trigger source selection (4 bits)
        bit_field_t<14, 0xf>::value<X>();
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // TI1SEL (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI2SEL =              // TI2SEL (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM2 = 15; // TIM2 global interrupt
    static constexpr uint8_t TIM3 = 16; // TIM3 global interrupt
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
    volatile uint32_t    OR1;                  // [Read-write] TIM option register
    reserved_t<3>        _2;
    volatile uint32_t    AF1;                  // [Read-write] TIM alternate function option register 1
    reserved_t<1>        _3;
    volatile uint32_t    TISEL;                // [Read-write] TIM alternate function option register 1

    static constexpr uint32_t CR1_UIFREMAP = 0x800;     // UIF status bit remapping
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

    template<uint32_t X>
    static constexpr uint32_t SMCR_TS_4_3 =              // Trigger selection (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    static constexpr uint32_t SMCR_SMS_3 = 0x10000;      // Slave mode selection - bit 3
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
    static constexpr uint32_t SMCR_OCCS = 0x8;           // OCREF clear selection
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
    static constexpr uint32_t CCMR1_OC1M_3 = 0x10000;     // Output Compare 1 mode - bit 3
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output compare 1 preload enable
    static constexpr uint32_t CCMR1_OC2CE = 0x8000;       // Output compare 2 clear enable
    static constexpr uint32_t CCMR1_OC2FE = 0x400;        // Output compare 2 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC2M =                // Output compare 2 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC2M_3 = 0x1000000;   // Output Compare 2 mode - bit 3
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
    static constexpr uint32_t CCMR2_OC3M_3 = 0x10000;     // Output Compare 3 mode - bit 3
    static constexpr uint32_t CCMR2_OC3PE = 0x8;          // Output compare 3 preload enable
    static constexpr uint32_t CCMR2_OC4CE = 0x8000;       // Output compare 4 clear enable
    static constexpr uint32_t CCMR2_OC4FE = 0x400;        // Output compare 4 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR2_OC4M =                // Output compare 4 mode (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR2_OC4M_3 = 0x1000000;   // Output Compare 4 mode - bit 3
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
    static constexpr uint32_t DMAR_DMAB =                // DMA register for burst accesses (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint32_t OR1_IOCREF_CLR = 0x1;     // IOCREF_CLR
    static const uint32_t OR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF1_ETRSEL =              // External trigger source selection (4 bits)
        bit_field_t<14, 0xf>::value<X>();
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // TI1SEL (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI2SEL =              // TI2SEL (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;
};

static tim3_t& TIM3 = *reinterpret_cast<tim3_t*>(0x40000400);

#define HAVE_PERIPHERAL_TIM3


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
    static constexpr uint32_t IPR0_PRI_0 =               // priority for interrupt 0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR0_PRI_1 =               // priority for interrupt 1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR0_PRI_2 =               // priority for interrupt 2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR0_PRI_3 =               // priority for interrupt 3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR0_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR1_PRI_4 =               // priority for interrupt n (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR1_PRI_5 =               // priority for interrupt n (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR1_PRI_6 =               // priority for interrupt n (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR1_PRI_7 =               // priority for interrupt n (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR2_PRI_8 =               // priority for interrupt n (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR2_PRI_9 =               // priority for interrupt n (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR2_PRI_10 =              // priority for interrupt n (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR2_PRI_11 =              // priority for interrupt n (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR3_PRI_12 =              // priority for interrupt n (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR3_PRI_13 =              // priority for interrupt n (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR3_PRI_14 =              // priority for interrupt n (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR3_PRI_15 =              // priority for interrupt n (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR4_PRI_16 =              // priority for interrupt n (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR4_PRI_17 =              // priority for interrupt n (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR4_PRI_18 =              // priority for interrupt n (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR4_PRI_19 =              // priority for interrupt n (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR5_PRI_20 =              // priority for interrupt n (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR5_PRI_21 =              // priority for interrupt n (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR5_PRI_22 =              // priority for interrupt n (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR5_PRI_23 =              // priority for interrupt n (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR5_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR6_PRI_24 =              // priority for interrupt n (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR6_PRI_25 =              // priority for interrupt n (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR6_PRI_26 =              // priority for interrupt n (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR6_PRI_27 =              // priority for interrupt n (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR6_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR7_PRI_28 =              // priority for interrupt n (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR7_PRI_29 =              // priority for interrupt n (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR7_PRI_30 =              // priority for interrupt n (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR7_PRI_31 =              // priority for interrupt n (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR7_RESET_VALUE = 0x0;
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
    volatile uint32_t    TYPER;                // [Read-only] MPU type register
    volatile uint32_t    CTRL;                 // [Read-only] MPU control register
    volatile uint32_t    RNR;                  // [Read-write] MPU region number register
    volatile uint32_t    RBAR;                 // [Read-write] MPU region base address register
    volatile uint32_t    RASR;                 // [Read-write] MPU region attribute and size register

    static constexpr uint32_t TYPER_SEPARATE = 0x1;       // Separate flag
    template<uint32_t X>
    static constexpr uint32_t TYPER_DREGION =             // Number of MPU data regions (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TYPER_IREGION =             // Number of MPU instruction regions (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static const uint32_t TYPER_RESET_VALUE = 0x800;

    static constexpr uint32_t CTRL_ENABLE = 0x1;         // Enables the MPU
    static constexpr uint32_t CTRL_HFNMIENA = 0x2;       // Enables the operation of MPU during hard fault
    static constexpr uint32_t CTRL_PRIVDEFENA = 0x4;     // Enable priviliged software access to default memory map
    static const uint32_t CTRL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RNR_REGION =              // MPU region (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RNR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RBAR_REGION =              // MPU region field (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static constexpr uint32_t RBAR_VALID = 0x10;         // MPU region number valid
    template<uint32_t X>
    static constexpr uint32_t RBAR_ADDR =                // Region base address field (27 bits)
        bit_field_t<5, 0x7ffffff>::value<X>();
    static const uint32_t RBAR_RESET_VALUE = 0x0;

    static constexpr uint32_t RASR_ENABLE = 0x1;         // Region enable bit.
    template<uint32_t X>
    static constexpr uint32_t RASR_SIZE =                // Size of the MPU protection region (5 bits)
        bit_field_t<1, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RASR_SRD =                 // Subregion disable bits (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static constexpr uint32_t RASR_B = 0x10000;          // memory attribute
    static constexpr uint32_t RASR_C = 0x20000;          // memory attribute
    static constexpr uint32_t RASR_S = 0x40000;          // Shareable memory attribute
    template<uint32_t X>
    static constexpr uint32_t RASR_TEX =                 // memory attribute (3 bits)
        bit_field_t<19, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RASR_AP =                  // Access permission (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    static constexpr uint32_t RASR_XN = 0x10000000;      // Instruction access disable bit
    static const uint32_t RASR_RESET_VALUE = 0x0;
};

static mpu_t& MPU = *reinterpret_cast<mpu_t*>(0xe000ed90);

#define HAVE_PERIPHERAL_MPU


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
    reserved_t<1>        _0;
    volatile uint32_t    SHPR2;                // [Read-write] System handler priority registers
    volatile uint32_t    SHPR3;                // [Read-write] System handler priority registers

    template<uint32_t X>
    static constexpr uint32_t CPUID_Revision =            // Revision number (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CPUID_PartNo =              // Part number of the processor (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CPUID_Architecture =        // Reads as 0xF (4 bits)
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
    static constexpr uint32_t VTOR_TBLOFF =              // Vector table base offset field (25 bits)
        bit_field_t<7, 0x1ffffff>::value<X>();
    static const uint32_t VTOR_RESET_VALUE = 0x0;

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

    static constexpr uint32_t CCR_NONBASETHRDENA = 0x1; // Configures how the processor enters Thread mode
    static constexpr uint32_t CCR_USERSETMPEND = 0x2;   // USERSETMPEND
    static constexpr uint32_t CCR_UNALIGN__TRP = 0x8;   // UNALIGN_ TRP
    static constexpr uint32_t CCR_DIV_0_TRP = 0x10;     // DIV_0_TRP
    static constexpr uint32_t CCR_BFHFNMIGN = 0x100;    // BFHFNMIGN
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


template<typename PERIPHERAL> struct peripheral_traits {};

template<> struct peripheral_traits<wwdg_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_WWDGEN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_WWDGEN; }
};

template<> struct peripheral_traits<flash_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_FLASHEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_FLASHEN; }
    static void reset() { RCC.AHBRSTR |= rcc_t::AHBRSTR_FLASHRST; }
};

template<> struct peripheral_traits<dbg_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_DBGEN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_DBGEN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_DBGRST; }
};

template<> struct peripheral_traits<pwr_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_PWREN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_PWREN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_PWRRST; }
};

template<> struct peripheral_traits<dma_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_DMAEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_DMAEN; }
    static void reset() { RCC.AHBRSTR |= rcc_t::AHBRSTR_DMARST; }
};

template<> struct peripheral_traits<gpioa_t>
{
    static void enable() { RCC.IOPENR |= rcc_t::IOPENR_IOPAEN; }
    static void disable() { RCC.IOPENR &= ~rcc_t::IOPENR_IOPAEN; }
    static void reset() { RCC.IOPRSTR |= rcc_t::IOPRSTR_IOPARST; }
};

template<> struct peripheral_traits<gpiob_t>
{
    static void enable() { RCC.IOPENR |= rcc_t::IOPENR_IOPBEN; }
    static void disable() { RCC.IOPENR &= ~rcc_t::IOPENR_IOPBEN; }
    static void reset() { RCC.IOPRSTR |= rcc_t::IOPRSTR_IOPBRST; }
};

template<> struct peripheral_traits<gpioc_t>
{
    static void enable() { RCC.IOPENR |= rcc_t::IOPENR_IOPCEN; }
    static void disable() { RCC.IOPENR &= ~rcc_t::IOPENR_IOPCEN; }
    static void reset() { RCC.IOPRSTR |= rcc_t::IOPRSTR_IOPCRST; }
};

template<> struct peripheral_traits<gpiod_t>
{
    static void enable() { RCC.IOPENR |= rcc_t::IOPENR_IOPDEN; }
    static void disable() { RCC.IOPENR &= ~rcc_t::IOPENR_IOPDEN; }
    static void reset() { RCC.IOPRSTR |= rcc_t::IOPRSTR_IOPDRST; }
};

template<> struct peripheral_traits<gpiof_t>
{
    static void enable() { RCC.IOPENR |= rcc_t::IOPENR_IOPFEN; }
    static void disable() { RCC.IOPENR &= ~rcc_t::IOPENR_IOPFEN; }
    static void reset() { RCC.IOPRSTR |= rcc_t::IOPRSTR_IOPFRST; }
};

template<> struct peripheral_traits<aes_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_AESEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_AESEN; }
    static void reset() { RCC.AHBRSTR |= rcc_t::AHBRSTR_AESRST; }
};

template<> struct peripheral_traits<rng_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_RNGEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_RNGEN; }
    static void reset() { RCC.AHBRSTR |= rcc_t::AHBRSTR_RNGRST; }
};

template<> struct peripheral_traits<crc_t>
{
    static void enable() { RCC.AHBENR |= rcc_t::AHBENR_CRCEN; }
    static void disable() { RCC.AHBENR &= ~rcc_t::AHBENR_CRCEN; }
    static void reset() { RCC.AHBRSTR |= rcc_t::AHBRSTR_CRCRST; }
};

template<> struct peripheral_traits<tim16_t>
{
    static void enable() { RCC.APBENR2 |= rcc_t::APBENR2_TIM16EN; }
    static void disable() { RCC.APBENR2 &= ~rcc_t::APBENR2_TIM16EN; }
    static void reset() { RCC.APBRSTR2 |= rcc_t::APBRSTR2_TIM16RST; }
};

template<> struct peripheral_traits<tim17_t>
{
    static void enable() { RCC.APBENR2 |= rcc_t::APBENR2_TIM17EN; }
    static void disable() { RCC.APBENR2 &= ~rcc_t::APBENR2_TIM17EN; }
    static void reset() { RCC.APBRSTR2 |= rcc_t::APBRSTR2_TIM17RST; }
};

template<> struct peripheral_traits<tim15_t>
{
    static void enable() { RCC.APBENR2 |= rcc_t::APBENR2_TIM15EN; }
    static void disable() { RCC.APBENR2 &= ~rcc_t::APBENR2_TIM15EN; }
    static void reset() { RCC.APBRSTR2 |= rcc_t::APBRSTR2_TIM15RST; }
};

template<> struct peripheral_traits<usart1_t>
{
    static void enable() { RCC.APBENR2 |= rcc_t::APBENR2_USART1EN; }
    static void disable() { RCC.APBENR2 &= ~rcc_t::APBENR2_USART1EN; }
    static void reset() { RCC.APBRSTR2 |= rcc_t::APBRSTR2_USART1RST; }
};

template<> struct peripheral_traits<usart2_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_USART2EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_USART2EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_USART2RST; }
};

template<> struct peripheral_traits<usart3_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_USART3EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_USART3EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_USART3RST; }
};

template<> struct peripheral_traits<usart4_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_USART4EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_USART4EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_USART4RST; }
};

template<> struct peripheral_traits<spi1_t>
{
    static void enable() { RCC.APBENR2 |= rcc_t::APBENR2_SPI1EN; }
    static void disable() { RCC.APBENR2 &= ~rcc_t::APBENR2_SPI1EN; }
    static void reset() { RCC.APBRSTR2 |= rcc_t::APBRSTR2_SPI1RST; }
};

template<> struct peripheral_traits<spi2_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_SPI2EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_SPI2EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_SPI2RST; }
};

template<> struct peripheral_traits<tim1_t>
{
    static void enable() { RCC.APBENR2 |= rcc_t::APBENR2_TIM1EN; }
    static void disable() { RCC.APBENR2 &= ~rcc_t::APBENR2_TIM1EN; }
    static void reset() { RCC.APBRSTR2 |= rcc_t::APBRSTR2_TIM1RST; }
};

template<> struct peripheral_traits<adc_t>
{
    static void enable() { RCC.APBENR2 |= rcc_t::APBENR2_ADCEN; }
    static void disable() { RCC.APBENR2 &= ~rcc_t::APBENR2_ADCEN; }
    static void reset() { RCC.APBRSTR2 |= rcc_t::APBRSTR2_ADCRST; }
};

template<> struct peripheral_traits<ucpd1_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_UCPD1EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_UCPD1EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_UCPD1RST; }
};

template<> struct peripheral_traits<ucpd2_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_UCPD2EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_UCPD2EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_UCPD2RST; }
};

template<> struct peripheral_traits<lptim1_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_LPTIM1EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_LPTIM1EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_LPTIM1RST; }
};

template<> struct peripheral_traits<lptim2_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_LPTIM2EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_LPTIM2EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_LPTIM2RST; }
};

template<> struct peripheral_traits<i2c1_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_I2C1EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_I2C1EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_I2C1RST; }
};

template<> struct peripheral_traits<i2c2_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_I2C2EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_I2C2EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_I2C2RST; }
};

template<> struct peripheral_traits<tim14_t>
{
    static void enable() { RCC.APBENR2 |= rcc_t::APBENR2_TIM14EN; }
    static void disable() { RCC.APBENR2 &= ~rcc_t::APBENR2_TIM14EN; }
    static void reset() { RCC.APBRSTR2 |= rcc_t::APBRSTR2_TIM14RST; }
};

template<> struct peripheral_traits<tim6_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_TIM6EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_TIM6EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_TIM6RST; }
};

template<> struct peripheral_traits<tim7_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_TIM7EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_TIM7EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_TIM7RST; }
};

template<> struct peripheral_traits<tim2_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_TIM2EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_TIM2EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_TIM2RST; }
};

template<> struct peripheral_traits<tim3_t>
{
    static void enable() { RCC.APBENR1 |= rcc_t::APBENR1_TIM3EN; }
    static void disable() { RCC.APBENR1 &= ~rcc_t::APBENR1_TIM3EN; }
    static void reset() { RCC.APBRSTR1 |= rcc_t::APBRSTR1_TIM3RST; }
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
    , RTC_STAMP = 2
    , FLASH = 3
    , RCC = 4
    , EXTI0_1 = 5
    , EXTI2_3 = 6
    , EXTI4_15 = 7
    , UCPD1_UCPD2 = 8
    , DMA_CHANNEL1 = 9
    , DMA_CHANNEL2_3 = 10
    , DMA_CHANNEL4_5_6_7 = 11
    , ADC_COMP = 12
    , TIM1_BRK_UP_TRG_COMP = 13
    , TIM1_CC = 14
    , TIM2 = 15
    , TIM3 = 16
    , TIM6_DAC_LPTIM1 = 17
    , TIM7_LPTIM2 = 18
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
    , USART3_USART4_LPUART1 = 29
    , CEC = 30
    , AES_RNG = 31
    };
};
