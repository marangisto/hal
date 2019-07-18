#pragma once

#include <stdint.h>

////
//
//    STM32G431xx
//
//       schema-version : 1.1
//       vendor         : 
//       series         : 
//       device-version : 1.0
//       address-unit   : 8 bits
//       device-width   : 32
//       device-size    : 32
//
////

namespace stm32g431xx
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
};

static crc_t& CRC = *reinterpret_cast<crc_t*>(0x40023000);

#define HAVE_PERIPHERAL_CRC


////
//
//    WinWATCHDOG
//
////

struct wwdg_t
{
    volatile uint32_t    KR;                   // [Write-only] Key register
    volatile uint32_t    PR;                   // [Read-write] Prescaler register
    volatile uint32_t    RLR;                  // [Read-write] Reload register
    volatile uint32_t    SR;                   // [Read-only] Status register
    volatile uint32_t    WINR;                 // [Read-write] Window register

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
};

static wwdg_t& WWDG = *reinterpret_cast<wwdg_t*>(0x40002c00);

#define HAVE_PERIPHERAL_WWDG


////
//
//    System window watchdog
//
////

struct iwdg_t
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
};

static iwdg_t& IWDG = *reinterpret_cast<iwdg_t*>(0x40003000);

#define HAVE_PERIPHERAL_IWDG


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

    template<uint32_t X>
    static constexpr uint32_t OAR1_OA1 =                 // Interface address (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
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

    static constexpr uint8_t I2C1_ER = 32; // I2C1_ER
    static constexpr uint8_t I2C1_EV = 31; // I2C1_EV
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

    template<uint32_t X>
    static constexpr uint32_t OAR1_OA1 =                 // Interface address (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
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

    static constexpr uint8_t I2C2_ER = 34; // I2C2_ER
    static constexpr uint8_t I2C2_EV = 33; // I2C2_EV
    static constexpr uint8_t WWDG = 0; // Window Watchdog interrupt
};

static i2c2_t& I2C2 = *reinterpret_cast<i2c2_t*>(0x40005800);

#define HAVE_PERIPHERAL_I2C2


////
//
//    Inter-integrated circuit
//
////

struct i2c3_t
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

    template<uint32_t X>
    static constexpr uint32_t OAR1_OA1 =                 // Interface address (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
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

    static constexpr uint8_t I2C3_ER = 93; // I2C3_ER
    static constexpr uint8_t I2C3_EV = 92; // I2C3_EV
};

static i2c3_t& I2C3 = *reinterpret_cast<i2c3_t*>(0x40007800);

#define HAVE_PERIPHERAL_I2C3


////
//
//    Flash
//
////

struct flash_t
{
    volatile uint32_t    ACR;                  // [Read-write] Access control register
    volatile uint32_t    PDKEYR;               // [Write-only] Power down key register
    volatile uint32_t    KEYR;                 // [Write-only] Flash key register
    volatile uint32_t    OPTKEYR;              // [Write-only] Option byte key register
    volatile uint32_t    SR;                   // Status register
    volatile uint32_t    CR;                   // [Read-write] Flash control register
    volatile uint32_t    ECCR;                 // Flash ECC register
    reserved_t<1>        _0;
    volatile uint32_t    OPTR;                 // [Read-write] Flash option register
    volatile uint32_t    PCROP1SR;             // [Read-write] Flash Bank 1 PCROP Start address register
    volatile uint32_t    PCROP1ER;             // [Read-write] Flash Bank 1 PCROP End address register
    volatile uint32_t    WRP1AR;               // [Read-write] Flash Bank 1 WRP area A address register
    volatile uint32_t    WRP1BR;               // [Read-write] Flash Bank 1 WRP area B address register
    reserved_t<15>       _1;
    volatile uint32_t    SEC1R;                // [Read-write] securable area bank1 register

    template<uint32_t X>
    static constexpr uint32_t ACR_LATENCY =             // Latency (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static constexpr uint32_t ACR_PRFTEN = 0x100;       // Prefetch enable
    static constexpr uint32_t ACR_ICEN = 0x200;         // Instruction cache enable
    static constexpr uint32_t ACR_DCEN = 0x400;         // Data cache enable
    static constexpr uint32_t ACR_ICRST = 0x800;        // Instruction cache reset
    static constexpr uint32_t ACR_DCRST = 0x1000;       // Data cache reset
    static constexpr uint32_t ACR_RUN_PD = 0x2000;      // Flash Power-down mode during Low-power run mode
    static constexpr uint32_t ACR_SLEEP_PD = 0x4000;    // Flash Power-down mode during Low-power sleep mode
    static constexpr uint32_t ACR_DBG_SWEN = 0x40000;   // Debug software enable
    static const uint32_t ACR_RESET_VALUE = 0x600;


    static const uint32_t PDKEYR_RESET_VALUE = 0x0;


    static const uint32_t KEYR_RESET_VALUE = 0x0;


    static const uint32_t OPTKEYR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_EOP = 0x1;            // End of operation, Read-write
    static constexpr uint32_t SR_OPERR = 0x2;          // Operation error, Read-write
    static constexpr uint32_t SR_PROGERR = 0x8;        // Programming error, Read-write
    static constexpr uint32_t SR_WRPERR = 0x10;        // Write protected error, Read-write
    static constexpr uint32_t SR_PGAERR = 0x20;        // Programming alignment error, Read-write
    static constexpr uint32_t SR_SIZERR = 0x40;        // Size error, Read-write
    static constexpr uint32_t SR_PGSERR = 0x80;        // Programming sequence error, Read-write
    static constexpr uint32_t SR_MISERR = 0x100;       // Fast programming data miss error, Read-write
    static constexpr uint32_t SR_FASTERR = 0x200;      // Fast programming error, Read-write
    static constexpr uint32_t SR_RDERR = 0x4000;       // PCROP read error, Read-write
    static constexpr uint32_t SR_OPTVERR = 0x8000;     // Option validity error, Read-write
    static constexpr uint32_t SR_BSY = 0x10000;        // Busy, Read-only
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_PG = 0x1;             // Programming
    static constexpr uint32_t CR_PER = 0x2;            // Page erase
    static constexpr uint32_t CR_MER1 = 0x4;           // Bank 1 Mass erase
    template<uint32_t X>
    static constexpr uint32_t CR_PNB =                 // Page number (7 bits)
        bit_field_t<3, 0x7f>::value<X>();
    static constexpr uint32_t CR_STRT = 0x10000;       // Start
    static constexpr uint32_t CR_OPTSTRT = 0x20000;    // Options modification start
    static constexpr uint32_t CR_FSTPG = 0x40000;      // Fast programming
    static constexpr uint32_t CR_EOPIE = 0x1000000;    // End of operation interrupt enable
    static constexpr uint32_t CR_ERRIE = 0x2000000;    // Error interrupt enable
    static constexpr uint32_t CR_RDERRIE = 0x4000000;  // PCROP read error interrupt enable
    static constexpr uint32_t CR_OBL_LAUNCH = 0x8000000;// Force the option byte loading
    static constexpr uint32_t CR_SEC_PROT1 = 0x10000000;// SEC_PROT1
    static constexpr uint32_t CR_OPTLOCK = 0x40000000; // Options Lock
    static constexpr uint32_t CR_LOCK = 0x80000000;    // FLASH_CR Lock
    static const uint32_t CR_RESET_VALUE = 0xc0000000;

    template<uint32_t X>
    static constexpr uint32_t ECCR_ADDR_ECC =            // ECC fail address (19 bits), Read-only
        bit_field_t<0, 0x7ffff>::value<X>();
    static constexpr uint32_t ECCR_BK_ECC = 0x200000;    // BK_ECC, Read-only
    static constexpr uint32_t ECCR_SYSF_ECC = 0x400000;  // SYSF_ECC, Read-only
    static constexpr uint32_t ECCR_ECCIE = 0x1000000;    // ECCIE, Read-write
    static constexpr uint32_t ECCR_ECCC2 = 0x10000000;   // ECC correction, Read-write
    static constexpr uint32_t ECCR_ECCD2 = 0x20000000;   // ECC2 detection, Read-write
    static constexpr uint32_t ECCR_ECCC = 0x40000000;    // ECC correction, Read-write
    static constexpr uint32_t ECCR_ECCD = 0x80000000;    // ECC detection, Read-write
    static const uint32_t ECCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t OPTR_RDP =                 // Read protection level (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OPTR_BOR_LEV =             // BOR reset Level (3 bits)
        bit_field_t<8, 0x7>::value<X>();
    static constexpr uint32_t OPTR_nRST_STOP = 0x1000;   // nRST_STOP
    static constexpr uint32_t OPTR_nRST_STDBY = 0x2000;  // nRST_STDBY
    static constexpr uint32_t OPTR_nRST_SHDW = 0x4000;   // nRST_SHDW
    static constexpr uint32_t OPTR_IDWG_SW = 0x10000;    // Independent watchdog selection
    static constexpr uint32_t OPTR_IWDG_STOP = 0x20000;  // Independent watchdog counter freeze in Stop mode
    static constexpr uint32_t OPTR_IWDG_STDBY = 0x40000; // Independent watchdog counter freeze in Standby mode
    static constexpr uint32_t OPTR_WWDG_SW = 0x80000;    // Window watchdog selection
    static constexpr uint32_t OPTR_nBOOT1 = 0x800000;    // Boot configuration
    static constexpr uint32_t OPTR_SRAM2_PE = 0x1000000; // SRAM2 parity check enable
    static constexpr uint32_t OPTR_SRAM2_RST = 0x2000000;// SRAM2 Erase when system reset
    static constexpr uint32_t OPTR_nSWBOOT0 = 0x4000000; // nSWBOOT0
    static constexpr uint32_t OPTR_nBOOT0 = 0x8000000;   // nBOOT0
    template<uint32_t X>
    static constexpr uint32_t OPTR_NRST_MODE =           // NRST_MODE (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    static constexpr uint32_t OPTR_IRHEN = 0x40000000;   // IRHEN
    static const uint32_t OPTR_RESET_VALUE = 0xf0000000;

    template<uint32_t X>
    static constexpr uint32_t PCROP1SR_PCROP1_STRT =         // Bank 1 PCROP area start offset (15 bits)
        bit_field_t<0, 0x7fff>::value<X>();
    static const uint32_t PCROP1SR_RESET_VALUE = 0xffff0000;

    template<uint32_t X>
    static constexpr uint32_t PCROP1ER_PCROP1_END =          // Bank 1 PCROP area end offset (15 bits)
        bit_field_t<0, 0x7fff>::value<X>();
    static constexpr uint32_t PCROP1ER_PCROP_RDP = 0x80000000;// PCROP area preserved when RDP level decreased
    static const uint32_t PCROP1ER_RESET_VALUE = 0xfff0000;

    template<uint32_t X>
    static constexpr uint32_t WRP1AR_WRP1A_STRT =          // Bank 1 WRP first area start offset (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t WRP1AR_WRP1A_END =           // Bank 1 WRP first area A end offset (7 bits)
        bit_field_t<16, 0x7f>::value<X>();
    static const uint32_t WRP1AR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t WRP1BR_WRP1B_STRT =          // Bank 1 WRP second area B end offset (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t WRP1BR_WRP1B_END =           // Bank 1 WRP second area B start offset (7 bits)
        bit_field_t<16, 0x7f>::value<X>();
    static const uint32_t WRP1BR_RESET_VALUE = 0x0;

    static constexpr uint32_t SEC1R_BOOT_LOCK = 0x10000;  // BOOT_LOCK
    template<uint32_t X>
    static constexpr uint32_t SEC1R_SEC_SIZE1 =           // SEC_SIZE1 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t SEC1R_RESET_VALUE = 0xff00ff00;

    static constexpr uint8_t FLASH = 4; // FLASH
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
    volatile uint32_t    APB1L_FZ;             // [Read-write] APB Low Freeze Register 1
    volatile uint32_t    APB1H_FZ;             // [Read-write] APB Low Freeze Register 2
    volatile uint32_t    APB2_FZ;              // [Read-write] APB High Freeze Register

    template<uint32_t X>
    static constexpr uint32_t IDCODE_DEV_ID =              // Device Identifier (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IDCODE_REV_ID =              // Revision Identifier (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t IDCODE_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_DBG_SLEEP = 0x1;      // Debug Sleep Mode
    static constexpr uint32_t CR_DBG_STOP = 0x2;       // Debug Stop Mode
    static constexpr uint32_t CR_DBG_STANDBY = 0x4;    // Debug Standby Mode
    static constexpr uint32_t CR_TRACE_IOEN = 0x20;    // Trace pin assignment control
    template<uint32_t X>
    static constexpr uint32_t CR_TRACE_MODE =          // Trace pin assignment control (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t APB1L_FZ_DBG_TIMER2_STOP = 0x1;// Debug Timer 2 stopped when Core is halted
    static constexpr uint32_t APB1L_FZ_DBG_TIM3_STOP = 0x2;  // TIM3 counter stopped when core is halted
    static constexpr uint32_t APB1L_FZ_DBG_TIM4_STOP = 0x4;  // TIM4 counter stopped when core is halted
    static constexpr uint32_t APB1L_FZ_DBG_TIM5_STOP = 0x8;  // TIM5 counter stopped when core is halted
    static constexpr uint32_t APB1L_FZ_DBG_TIMER6_STOP = 0x10;// Debug Timer 6 stopped when Core is halted
    static constexpr uint32_t APB1L_FZ_DBG_TIM7_STOP = 0x20; // TIM7 counter stopped when core is halted
    static constexpr uint32_t APB1L_FZ_DBG_RTC_STOP = 0x400; // Debug RTC stopped when Core is halted
    static constexpr uint32_t APB1L_FZ_DBG_WWDG_STOP = 0x800;// Debug Window Wachdog stopped when Core is halted
    static constexpr uint32_t APB1L_FZ_DBG_IWDG_STOP = 0x1000;// Debug Independent Wachdog stopped when Core is halted
    static constexpr uint32_t APB1L_FZ_DBG_I2C1_STOP = 0x200000;// I2C1 SMBUS timeout mode stopped when core is halted
    static constexpr uint32_t APB1L_FZ_DBG_I2C2_STOP = 0x400000;// I2C2 SMBUS timeout mode stopped when core is halted
    static constexpr uint32_t APB1L_FZ_DBG_I2C3_STOP = 0x40000000;// I2C3 SMBUS timeout mode stopped when core is halted
    static constexpr uint32_t APB1L_FZ_DBG_LPTIMER_STOP = 0x80000000;// LPTIM1 counter stopped when core is halted
    static const uint32_t APB1L_FZ_RESET_VALUE = 0x0;

    static constexpr uint32_t APB1H_FZ_DBG_I2C4_STOP = 0x2;  // DBG_I2C4_STOP
    static const uint32_t APB1H_FZ_RESET_VALUE = 0x0;

    static constexpr uint32_t APB2_FZ_DBG_TIM1_STOP = 0x800;// TIM1 counter stopped when core is halted
    static constexpr uint32_t APB2_FZ_DBG_TIM8_STOP = 0x2000;// TIM8 counter stopped when core is halted
    static constexpr uint32_t APB2_FZ_DBG_TIM15_STOP = 0x10000;// TIM15 counter stopped when core is halted
    static constexpr uint32_t APB2_FZ_DBG_TIM16_STOP = 0x20000;// TIM16 counter stopped when core is halted
    static constexpr uint32_t APB2_FZ_DBG_TIM17_STOP = 0x40000;// TIM17 counter stopped when core is halted
    static constexpr uint32_t APB2_FZ_DBG_TIM20_STOP = 0x100000;// TIM20counter stopped when core is halted
    static constexpr uint32_t APB2_FZ_DBG_HRTIM0_STOP = 0x4000000;// DBG_HRTIM0_STOP
    static constexpr uint32_t APB2_FZ_DBG_HRTIM1_STOP = 0x8000000;// DBG_HRTIM0_STOP
    static constexpr uint32_t APB2_FZ_DBG_HRTIM2_STOP = 0x10000000;// DBG_HRTIM0_STOP
    static constexpr uint32_t APB2_FZ_DBG_HRTIM3_STOP = 0x20000000;// DBG_HRTIM0_STOP
    static const uint32_t APB2_FZ_RESET_VALUE = 0x0;
};

static dbgmcu_t& DBGMCU = *reinterpret_cast<dbgmcu_t*>(0xe0042000);

#define HAVE_PERIPHERAL_DBGMCU


////
//
//    Reset and clock control
//
////

struct rcc_t
{
    volatile uint32_t    CR;                   // Clock control register
    volatile uint32_t    ICSCR;                // Internal clock sources calibration register
    volatile uint32_t    CFGR;                 // Clock configuration register
    volatile uint32_t    PLLSYSCFGR;           // [Read-write] PLL configuration register
    reserved_t<2>        _0;
    volatile uint32_t    CIER;                 // [Read-write] Clock interrupt enable register
    volatile uint32_t    CIFR;                 // [Read-only] Clock interrupt flag register
    volatile uint32_t    CICR;                 // [Write-only] Clock interrupt clear register
    reserved_t<1>        _1;
    volatile uint32_t    AHB1RSTR;             // [Read-write] AHB1 peripheral reset register
    volatile uint32_t    AHB2RSTR;             // [Read-write] AHB2 peripheral reset register
    volatile uint32_t    AHB3RSTR;             // [Read-write] AHB3 peripheral reset register
    reserved_t<1>        _2;
    volatile uint32_t    APB1RSTR1;            // [Read-write] APB1 peripheral reset register 1
    volatile uint32_t    APB1RSTR2;            // [Read-write] APB1 peripheral reset register 2
    volatile uint32_t    APB2RSTR;             // [Read-write] APB2 peripheral reset register
    reserved_t<1>        _3;
    volatile uint32_t    AHB1ENR;              // [Read-write] AHB1 peripheral clock enable register
    volatile uint32_t    AHB2ENR;              // [Read-write] AHB2 peripheral clock enable register
    volatile uint32_t    AHB3ENR;              // [Read-write] AHB3 peripheral clock enable register
    reserved_t<1>        _4;
    volatile uint32_t    APB1ENR1;             // [Read-write] APB1ENR1
    volatile uint32_t    APB1ENR2;             // [Read-write] APB1 peripheral clock enable register 2
    volatile uint32_t    APB2ENR;              // [Read-write] APB2ENR
    reserved_t<1>        _5;
    volatile uint32_t    AHB1SMENR;            // [Read-write] AHB1 peripheral clocks enable in Sleep and Stop modes register
    volatile uint32_t    AHB2SMENR;            // [Read-write] AHB2 peripheral clocks enable in Sleep and Stop modes register
    volatile uint32_t    AHB3SMENR;            // [Read-write] AHB3 peripheral clocks enable in Sleep and Stop modes register
    reserved_t<1>        _6;
    volatile uint32_t    APB1SMENR1;           // [Read-write] APB1SMENR1
    volatile uint32_t    APB1SMENR2;           // [Read-write] APB1 peripheral clocks enable in Sleep and Stop modes register 2
    volatile uint32_t    APB2SMENR;            // [Read-write] APB2SMENR
    reserved_t<1>        _7;
    volatile uint32_t    CCIPR1;               // [Read-write] CCIPR
    reserved_t<1>        _8;
    volatile uint32_t    BDCR;                 // BDCR
    volatile uint32_t    CSR;                  // CSR
    volatile uint32_t    CRRCR;                // Clock recovery RC register
    volatile uint32_t    CCIPR2;               // [Read-write] Peripherals independent clock configuration register

    static constexpr uint32_t CR_PLLSYSRDY = 0x2000000;// Main PLL clock ready flag, Read-only
    static constexpr uint32_t CR_PLLSYSON = 0x1000000; // Main PLL enable, Read-write
    static constexpr uint32_t CR_HSECSSON = 0x80000;   // Clock security system enable, Write-only
    static constexpr uint32_t CR_HSEBYP = 0x40000;     // HSE crystal oscillator bypass, Read-write
    static constexpr uint32_t CR_HSERDY = 0x20000;     // HSE clock ready flag, Read-only
    static constexpr uint32_t CR_HSEON = 0x10000;      // HSE clock enable, Read-write
    static constexpr uint32_t CR_HSIRDY = 0x400;       // HSI clock ready flag, Read-only
    static constexpr uint32_t CR_HSIKERON = 0x200;     // HSI always enable for peripheral kernels, Read-write
    static constexpr uint32_t CR_HSION = 0x100;        // HSI clock enable, Read-write
    static const uint32_t CR_RESET_VALUE = 0x63;

    template<uint32_t X>
    static constexpr uint32_t ICSCR_HSICAL0 =             // Internal High Speed clock Calibration (8 bits), Read-only
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ICSCR_HSITRIM =             // Internal High Speed clock trimming (7 bits), Read-write
        bit_field_t<24, 0x7f>::value<X>();
    static const uint32_t ICSCR_RESET_VALUE = 0x40000000;

    template<uint32_t X>
    static constexpr uint32_t CFGR_MCOPRE =              // Microcontroller clock output prescaler (3 bits), Read-write
        bit_field_t<28, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_MCOSEL =              // Microcontroller clock output (4 bits), Read-write
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_PPRE2 =               // APB high-speed prescaler (APB2) (3 bits), Read-write
        bit_field_t<11, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_PPRE1 =               // PB low-speed prescaler (APB1) (3 bits), Read-write
        bit_field_t<8, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_HPRE =                // AHB prescaler (4 bits), Read-write
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_SWS =                 // System clock switch status (2 bits), Read-only
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_SW =                  // System clock switch (2 bits), Read-write
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t CFGR_RESET_VALUE = 0x5;

    template<uint32_t X>
    static constexpr uint32_t PLLSYSCFGR_PLLSYSPDIV =          // Main PLL division factor for PLLSAI2CLK (5 bits)
        bit_field_t<27, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PLLSYSCFGR_PLLSYSR =             // Main PLL division factor for PLLCLK (system clock) (2 bits)
        bit_field_t<25, 0x3>::value<X>();
    static constexpr uint32_t PLLSYSCFGR_PLLSYSREN = 0x1000000;// Main PLL PLLCLK output enable
    template<uint32_t X>
    static constexpr uint32_t PLLSYSCFGR_PLLSYSQ =             // Main PLL division factor for PLLUSB1CLK(48 MHz clock) (2 bits)
        bit_field_t<21, 0x3>::value<X>();
    static constexpr uint32_t PLLSYSCFGR_PLLSYSQEN = 0x100000; // Main PLL PLLUSB1CLK output enable
    static constexpr uint32_t PLLSYSCFGR_PLLSYSP = 0x20000;    // Main PLL division factor for PLLSAI3CLK (SAI1 and SAI2 clock)
    static constexpr uint32_t PLLSYSCFGR_PLLPEN = 0x10000;     // Main PLL PLLSAI3CLK output enable
    template<uint32_t X>
    static constexpr uint32_t PLLSYSCFGR_PLLSYSN =             // Main PLL multiplication factor for VCO (7 bits)
        bit_field_t<8, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PLLSYSCFGR_PLLSYSM =             // Division factor for the main PLL and audio PLL (PLLSAI1 and PLLSAI2) input clock (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PLLSYSCFGR_PLLSRC =              // Main PLL, PLLSAI1 and PLLSAI2 entry clock source (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t PLLSYSCFGR_RESET_VALUE = 0x1000;

    static constexpr uint32_t CIER_LSIRDYIE = 0x1;       // LSI ready interrupt enable
    static constexpr uint32_t CIER_LSERDYIE = 0x2;       // LSE ready interrupt enable
    static constexpr uint32_t CIER_HSIRDYIE = 0x8;       // HSI ready interrupt enable
    static constexpr uint32_t CIER_HSERDYIE = 0x10;      // HSE ready interrupt enable
    static constexpr uint32_t CIER_PLLSYSRDYIE = 0x20;   // PLL ready interrupt enable
    static constexpr uint32_t CIER_LSECSSIE = 0x200;     // LSE clock security system interrupt enable
    static constexpr uint32_t CIER_RC48RDYIE = 0x400;    // HSI48 ready interrupt enable
    static const uint32_t CIER_RESET_VALUE = 0x0;

    static constexpr uint32_t CIFR_LSIRDYF = 0x1;        // LSI ready interrupt flag
    static constexpr uint32_t CIFR_LSERDYF = 0x2;        // LSE ready interrupt flag
    static constexpr uint32_t CIFR_HSIRDYF = 0x8;        // HSI ready interrupt flag
    static constexpr uint32_t CIFR_HSERDYF = 0x10;       // HSE ready interrupt flag
    static constexpr uint32_t CIFR_PLLSYSRDYF = 0x20;    // PLL ready interrupt flag
    static constexpr uint32_t CIFR_HSECSSF = 0x100;      // Clock security system interrupt flag
    static constexpr uint32_t CIFR_LSECSSF = 0x200;      // LSE Clock security system interrupt flag
    static constexpr uint32_t CIFR_RC48RDYF = 0x400;     // HSI48 ready interrupt flag
    static const uint32_t CIFR_RESET_VALUE = 0x0;

    static constexpr uint32_t CICR_LSIRDYC = 0x1;        // LSI ready interrupt clear
    static constexpr uint32_t CICR_LSERDYC = 0x2;        // LSE ready interrupt clear
    static constexpr uint32_t CICR_HSIRDYC = 0x8;        // HSI ready interrupt clear
    static constexpr uint32_t CICR_HSERDYC = 0x10;       // HSE ready interrupt clear
    static constexpr uint32_t CICR_PLLSYSRDYC = 0x20;    // PLL ready interrupt clear
    static constexpr uint32_t CICR_HSECSSC = 0x100;      // Clock security system interrupt clear
    static constexpr uint32_t CICR_LSECSSC = 0x200;      // LSE Clock security system interrupt clear
    static constexpr uint32_t CICR_RC48RDYC = 0x400;     // HSI48 oscillator ready interrupt clear
    static const uint32_t CICR_RESET_VALUE = 0x0;

    static constexpr uint32_t AHB1RSTR_DMA1RST = 0x1;        // DMA1 reset
    static constexpr uint32_t AHB1RSTR_DMA2RST = 0x2;        // DMA2 reset
    static constexpr uint32_t AHB1RSTR_DMAMUX1RST = 0x4;     // DMAMUXRST
    static constexpr uint32_t AHB1RSTR_CORDICRST = 0x8;      // CORDIC reset
    static constexpr uint32_t AHB1RSTR_MATRIXRST = 0x10;     // MATRIX reset
    static constexpr uint32_t AHB1RSTR_FLITFRST_ = 0x100;    // FLITF reset
    static constexpr uint32_t AHB1RSTR_CRCRST = 0x1000;      // CRC reset
    static const uint32_t AHB1RSTR_RESET_VALUE = 0x0;

    static constexpr uint32_t AHB2RSTR_GPIOARST = 0x1;       // IO port A reset
    static constexpr uint32_t AHB2RSTR_GPIOBRST = 0x2;       // IO port B reset
    static constexpr uint32_t AHB2RSTR_GPIOCRST = 0x4;       // IO port C reset
    static constexpr uint32_t AHB2RSTR_GPIODRST = 0x8;       // IO port D reset
    static constexpr uint32_t AHB2RSTR_GPIOERST = 0x10;      // IO port E reset
    static constexpr uint32_t AHB2RSTR_GPIOFRST = 0x20;      // IO port F reset
    static constexpr uint32_t AHB2RSTR_GPIOGRST = 0x40;      // IO port G reset
    static constexpr uint32_t AHB2RSTR_ADC12RST = 0x2000;    // ADC reset
    static constexpr uint32_t AHB2RSTR_ADC345RST_ = 0x4000;  // SAR ADC345 interface reset
    static constexpr uint32_t AHB2RSTR_DAC1RST_ = 0x10000;   // DAC1 interface reset
    static constexpr uint32_t AHB2RSTR_DAC2RST = 0x20000;    // DAC2 interface reset
    static constexpr uint32_t AHB2RSTR_DAC3RST = 0x40000;    // DAC3 interface reset
    static constexpr uint32_t AHB2RSTR_DAC4RST = 0x80000;    // DAC4 interface reset
    static constexpr uint32_t AHB2RSTR_CRYPTRST = 0x1000000; // Cryptography module reset
    static constexpr uint32_t AHB2RSTR_RNGRST = 0x4000000;   // Random Number Generator module reset
    static const uint32_t AHB2RSTR_RESET_VALUE = 0x0;

    static constexpr uint32_t AHB3RSTR_FMCRST = 0x1;         // Flexible memory controller reset
    static constexpr uint32_t AHB3RSTR_QUADSPI1RST = 0x100;  // Quad SPI 1 module reset
    static const uint32_t AHB3RSTR_RESET_VALUE = 0x0;

    static constexpr uint32_t APB1RSTR1_LPTIM1RST = 0x80000000;// Low Power Timer 1 reset
    static constexpr uint32_t APB1RSTR1_I2C3 = 0x40000000;    // I2C3 interface reset
    static constexpr uint32_t APB1RSTR1_PWRRST = 0x10000000;  // Power interface reset
    static constexpr uint32_t APB1RSTR1_FDCANRST = 0x2000000; // FDCAN reset
    static constexpr uint32_t APB1RSTR1_USBDRST = 0x800000;   // USBD reset
    static constexpr uint32_t APB1RSTR1_I2C2RST = 0x400000;   // I2C2 reset
    static constexpr uint32_t APB1RSTR1_I2C1RST = 0x200000;   // I2C1 reset
    static constexpr uint32_t APB1RSTR1_UART5RST = 0x100000;  // UART5 reset
    static constexpr uint32_t APB1RSTR1_UART4RST = 0x80000;   // UART4 reset
    static constexpr uint32_t APB1RSTR1_USART3RST = 0x40000;  // USART3 reset
    static constexpr uint32_t APB1RSTR1_USART2RST = 0x20000;  // USART2 reset
    static constexpr uint32_t APB1RSTR1_SPI3RST = 0x8000;     // SPI3 reset
    static constexpr uint32_t APB1RSTR1_SPI2RST = 0x4000;     // SPI2 reset
    static constexpr uint32_t APB1RSTR1_CRSRST = 0x100;       // Clock recovery system reset
    static constexpr uint32_t APB1RSTR1_TIM7RST = 0x20;       // TIM7 timer reset
    static constexpr uint32_t APB1RSTR1_TIM6RST = 0x10;       // TIM6 timer reset
    static constexpr uint32_t APB1RSTR1_TIM5RST = 0x8;        // TIM5 timer reset
    static constexpr uint32_t APB1RSTR1_TIM4RST = 0x4;        // TIM3 timer reset
    static constexpr uint32_t APB1RSTR1_TIM3RST = 0x2;        // TIM3 timer reset
    static constexpr uint32_t APB1RSTR1_TIM2RST = 0x1;        // TIM2 timer reset
    static const uint32_t APB1RSTR1_RESET_VALUE = 0x0;

    static constexpr uint32_t APB1RSTR2_LPUART1RST = 0x1;     // Low-power UART 1 reset
    static constexpr uint32_t APB1RSTR2_I2C4RST = 0x2;        // I2C4 reset
    static constexpr uint32_t APB1RSTR2_USBPDRST = 0x100;     // USBPD reset
    static const uint32_t APB1RSTR2_RESET_VALUE = 0x0;

    static constexpr uint32_t APB2RSTR_SYSCFGRST = 0x1;      // System configuration (SYSCFG) reset
    static constexpr uint32_t APB2RSTR_TIM1RST = 0x800;      // TIM1 timer reset
    static constexpr uint32_t APB2RSTR_SPI1RST = 0x1000;     // SPI1 reset
    static constexpr uint32_t APB2RSTR_TIM8RST = 0x2000;     // TIM8 timer reset
    static constexpr uint32_t APB2RSTR_USART1RST = 0x4000;   // USART1 reset
    static constexpr uint32_t APB2RSTR_SPI4RST = 0x8000;     // SPI 4 reset
    static constexpr uint32_t APB2RSTR_TIM15RST = 0x10000;   // TIM15 timer reset
    static constexpr uint32_t APB2RSTR_TIM16RST = 0x20000;   // TIM16 timer reset
    static constexpr uint32_t APB2RSTR_TIM17RST = 0x40000;   // TIM17 timer reset
    static constexpr uint32_t APB2RSTR_TIM20RST = 0x100000;  // Timer 20 reset
    static constexpr uint32_t APB2RSTR_SAI1RST = 0x200000;   // Serial audio interface 1 (SAI1) reset
    static constexpr uint32_t APB2RSTR_HRTIM1RST = 0x4000000;// HRTIMER reset
    static const uint32_t APB2RSTR_RESET_VALUE = 0x0;

    static constexpr uint32_t AHB1ENR_DMA1EN = 0x1;         // DMA1 clock enable
    static constexpr uint32_t AHB1ENR_DMA2EN = 0x2;         // DMA2 clock enable
    static constexpr uint32_t AHB1ENR_DMAMUXEN = 0x4;       // DMAMUX clock enable
    static constexpr uint32_t AHB1ENR_CORDICEN = 0x8;       // CORDIC clock enable
    static constexpr uint32_t AHB1ENR_FMACEN = 0x10;        // FMAC clock enable
    static constexpr uint32_t AHB1ENR_FLITFEN = 0x100;      // FLITF clock enable
    static constexpr uint32_t AHB1ENR_CRCEN = 0x1000;       // CRC clock enable
    static const uint32_t AHB1ENR_RESET_VALUE = 0x100;

    static constexpr uint32_t AHB2ENR_GPIOAEN = 0x1;        // IO port A clock enable
    static constexpr uint32_t AHB2ENR_GPIOBEN = 0x2;        // IO port B clock enable
    static constexpr uint32_t AHB2ENR_GPIOCEN = 0x4;        // IO port C clock enable
    static constexpr uint32_t AHB2ENR_GPIODEN = 0x8;        // IO port D clock enable
    static constexpr uint32_t AHB2ENR_GPIOEEN = 0x10;       // IO port E clock enable
    static constexpr uint32_t AHB2ENR_GPIOFEN = 0x20;       // IO port F clock enable
    static constexpr uint32_t AHB2ENR_GPIOGEN = 0x40;       // IO port G clock enable
    static constexpr uint32_t AHB2ENR_ADC12EN = 0x2000;     // ADC clock enable
    static constexpr uint32_t AHB2ENR_ADC345EN = 0x4000;    // DCMI clock enable
    static constexpr uint32_t AHB2ENR_DAC1 = 0x10000;       // AES accelerator clock enable
    static constexpr uint32_t AHB2ENR_DAC2 = 0x20000;       // HASH clock enable
    static constexpr uint32_t AHB2ENR_DAC3 = 0x40000;       // Random Number Generator clock enable
    static constexpr uint32_t AHB2ENR_DAC4 = 0x80000;       // DAC4 clock enable
    static constexpr uint32_t AHB2ENR_CRYPTEN = 0x1000000;  // Cryptography clock enable
    static constexpr uint32_t AHB2ENR_RNGEN = 0x4000000;    // Random Number Generator clock enable
    static const uint32_t AHB2ENR_RESET_VALUE = 0x0;

    static constexpr uint32_t AHB3ENR_FMCEN = 0x1;          // Flexible memory controller clock enable
    static constexpr uint32_t AHB3ENR_QUADSPI1EN = 0x100;   // Quad SPI 1 module clock enable
    static const uint32_t AHB3ENR_RESET_VALUE = 0x0;

    static constexpr uint32_t APB1ENR1_TIM2EN = 0x1;         // TIM2 timer clock enable
    static constexpr uint32_t APB1ENR1_TIM3EN = 0x2;         // TIM3 timer clock enable
    static constexpr uint32_t APB1ENR1_TIM4EN = 0x4;         // TIM4 timer clock enable
    static constexpr uint32_t APB1ENR1_TIM5EN = 0x8;         // TIM5 timer clock enable
    static constexpr uint32_t APB1ENR1_TIM6EN = 0x10;        // TIM6 timer clock enable
    static constexpr uint32_t APB1ENR1_TIM7EN = 0x20;        // TIM7 timer clock enable
    static constexpr uint32_t APB1ENR1_CRSEN = 0x100;        // CRSclock enable
    static constexpr uint32_t APB1ENR1_RTCAPBEN = 0x400;     // RTC APB clock enable
    static constexpr uint32_t APB1ENR1_WWDGEN = 0x800;       // Window watchdog clock enable
    static constexpr uint32_t APB1ENR1_SPI2EN = 0x4000;      // SPI2 clock enable
    static constexpr uint32_t APB1ENR1_SP3EN = 0x8000;       // SPI3 clock enable
    static constexpr uint32_t APB1ENR1_USART2EN = 0x20000;   // USART2 clock enable
    static constexpr uint32_t APB1ENR1_USART3EN = 0x40000;   // USART3 clock enable
    static constexpr uint32_t APB1ENR1_UART4EN = 0x80000;    // UART4 clock enable
    static constexpr uint32_t APB1ENR1_UART5EN = 0x100000;   // UART5 clock enable
    static constexpr uint32_t APB1ENR1_I2C1EN = 0x200000;    // I2C1 clock enable
    static constexpr uint32_t APB1ENR1_I2C2EN = 0x400000;    // I2C2 clock enable
    static constexpr uint32_t APB1ENR1_USBDEN = 0x800000;    // USBDclock enable
    static constexpr uint32_t APB1ENR1_FDCANEN = 0x2000000;  // FDCAN clock enable
    static constexpr uint32_t APB1ENR1_PWREN = 0x10000000;   // Power interface clock enable
    static constexpr uint32_t APB1ENR1_I2C3 = 0x40000000;    // OPAMP interface clock enable
    static constexpr uint32_t APB1ENR1_LPTIM1EN = 0x80000000;// Low power timer 1 clock enable
    static const uint32_t APB1ENR1_RESET_VALUE = 0x0;

    static constexpr uint32_t APB1ENR2_LPUART1EN = 0x1;      // Low power UART 1 clock enable
    static constexpr uint32_t APB1ENR2_I2C4EN = 0x2;         // I2C4 clock enable
    static constexpr uint32_t APB1ENR2_USBPDEN = 0x100;      // USBPD clock enable
    static const uint32_t APB1ENR2_RESET_VALUE = 0x0;

    static constexpr uint32_t APB2ENR_SYSCFGEN = 0x1;       // SYSCFG clock enable
    static constexpr uint32_t APB2ENR_TIM1EN = 0x800;       // TIM1 timer clock enable
    static constexpr uint32_t APB2ENR_SPI1EN = 0x1000;      // SPI1 clock enable
    static constexpr uint32_t APB2ENR_TIM8EN = 0x2000;      // TIM8 timer clock enable
    static constexpr uint32_t APB2ENR_USART1EN = 0x4000;    // USART1clock enable
    static constexpr uint32_t APB2ENR_SPI4EN = 0x8000;      // SPI 4 clock enable
    static constexpr uint32_t APB2ENR_TIM15EN = 0x10000;    // TIM15 timer clock enable
    static constexpr uint32_t APB2ENR_TIM16EN = 0x20000;    // TIM16 timer clock enable
    static constexpr uint32_t APB2ENR_TIM17EN = 0x40000;    // TIM17 timer clock enable
    static constexpr uint32_t APB2ENR_TIM20EN = 0x100000;   // Timer 20 clock enable
    static constexpr uint32_t APB2ENR_SAI1EN = 0x200000;    // SAI1 clock enable
    static constexpr uint32_t APB2ENR_HRTIMEREN = 0x4000000;// HRTIMER clock enable
    static const uint32_t APB2ENR_RESET_VALUE = 0x0;

    static constexpr uint32_t AHB1SMENR_DMA1SMEN = 0x1;       // DMA1 clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB1SMENR_DMA2SMEN = 0x2;       // DMA2 clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB1SMENR_DMAMUX1SMEN = 0x4;    // DMAMUX clock enable during Sleep and Stop modes
    static constexpr uint32_t AHB1SMENR_CORDICSMEN = 0x8;     // CORDIC clock enable during sleep mode
    static constexpr uint32_t AHB1SMENR_FLASHSMEN = 0x100;    // Flash memory interface clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB1SMENR_SRAM1SMEN = 0x200;    // SRAM1 interface clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB1SMENR_CRCSMEN = 0x1000;     // CRCSMEN
    static constexpr uint32_t AHB1SMENR_FMACSMEN = 0x10;      // FMACSM clock enable
    static const uint32_t AHB1SMENR_RESET_VALUE = 0x130f;

    static constexpr uint32_t AHB2SMENR_GPIOASMEN = 0x1;      // IO port A clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_GPIOBSMEN = 0x2;      // IO port B clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_GPIOCSMEN = 0x4;      // IO port C clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_GPIODSMEN = 0x8;      // IO port D clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_GPIOESMEN = 0x10;     // IO port E clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_GPIOFSMEN = 0x20;     // IO port F clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_GPIOGSMEN = 0x40;     // IO port G clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_SRAM2SMEN = 0x200;    // SRAM2 interface clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_SRAM3SMEN = 0x400;    // SRAM2 interface clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_AD12CSMEN = 0x2000;   // ADC clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_ADC345SMEN = 0x4000;  // DCMI clock enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_DAC1SMEN = 0x10000;   // AES accelerator clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_DAC2SMEN = 0x20000;   // HASH clock enable during Sleep and Stop modes
    static constexpr uint32_t AHB2SMENR_DAC3SMEN = 0x40000;   // DAC3 clock enable during sleep mode
    static constexpr uint32_t AHB2SMENR_DAC4SMEN = 0x80000;   // DAC4 clock enable during sleep mode
    static constexpr uint32_t AHB2SMENR_CRYPTSMEN = 0x1000000;// Cryptography clock enable during sleep mode
    static constexpr uint32_t AHB2SMENR_RNGSMEN = 0x4000000;  // Random Number Generator clock enable during sleep mode
    static const uint32_t AHB2SMENR_RESET_VALUE = 0x50f667f;

    static constexpr uint32_t AHB3SMENR_FMCSMEN = 0x1;        // Flexible memory controller clocks enable during Sleep and Stop modes
    static constexpr uint32_t AHB3SMENR_QUADSPI1SMEN = 0x100; // QUAD SPI 1 module clock enable during sleep mode
    static const uint32_t AHB3SMENR_RESET_VALUE = 0x101;

    static constexpr uint32_t APB1SMENR1_TIM2SMEN = 0x1;       // TIM2 timer clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_TIM3SMEN = 0x2;       // TIM3 timer clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_TIM4SMEN = 0x4;       // TIM4 timer clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_TIM5SMEN = 0x8;       // TIM5 timer clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_TIM6SMEN = 0x10;      // TIM6 timer clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_TIM7SMEN = 0x20;      // TIM7 timer clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_CRSSMEN = 0x100;      // CRS clock enable during sleep mode
    static constexpr uint32_t APB1SMENR1_RTCAPBSMEN = 0x400;   // RTC APB clock enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_WWDGSMEN = 0x800;     // Window watchdog clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_SPI2SMEN = 0x4000;    // SPI2 clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_SP3SMEN = 0x8000;     // SPI3 clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_USART2SMEN = 0x20000; // USART2 clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_USART3SMEN = 0x40000; // USART3 clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_UART4SMEN = 0x80000;  // UART4 clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_UART5SMEN = 0x100000; // UART5 clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_I2C1SMEN = 0x200000;  // I2C1 clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_I2C2SMEN = 0x400000;  // I2C2 clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_I2C3SMEN = 0x800000;  // I2C3 clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_FDCANSMEN = 0x2000000;// FDCAN clock enable during sleep mode
    static constexpr uint32_t APB1SMENR1_PWRSMEN = 0x10000000; // Power interface clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR1_I2C3SMEN_3 = 0x40000000;// I2C 3 interface clock enable during sleep mode
    static constexpr uint32_t APB1SMENR1_LPTIM1SMEN = 0x80000000;// Low Power Timer1 clock enable during sleep mode
    static const uint32_t APB1SMENR1_RESET_VALUE = 0xd2fecd3f;

    static constexpr uint32_t APB1SMENR2_LPUART1SMEN = 0x1;    // Low power UART 1 clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR2_I2C4SMEN = 0x2;       // I2C4 clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB1SMENR2_USBPDSMEN = 0x100;    // USB PD clock enable during sleep mode
    static const uint32_t APB1SMENR2_RESET_VALUE = 0x103;

    static constexpr uint32_t APB2SMENR_SYSCFGSMEN = 0x1;     // SYSCFG clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB2SMENR_TIM1SMEN = 0x800;     // TIM1 timer clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB2SMENR_SPI1SMEN = 0x1000;    // SPI1 clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB2SMENR_TIM8SMEN = 0x2000;    // TIM8 timer clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB2SMENR_USART1SMEN = 0x4000;  // USART1clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB2SMENR_SPI4SMEN = 0x8000;    // SPI4 timer clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB2SMENR_TIM15SMEN = 0x10000;  // TIM15 timer clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB2SMENR_TIM16SMEN = 0x20000;  // TIM16 timer clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB2SMENR_TIM17SMEN = 0x40000;  // TIM17 timer clocks enable during Sleep and Stop modes
    static constexpr uint32_t APB2SMENR_TIM20SMEN = 0x100000; // Timer 20clock enable during sleep mode
    static constexpr uint32_t APB2SMENR_SAI1SMEN = 0x200000;  // SAI1 clock enable during sleep mode
    static constexpr uint32_t APB2SMENR_HRTIMERSMEN = 0x4000000;// HRTIMER clock enable during sleep mode
    static const uint32_t APB2SMENR_RESET_VALUE = 0x437f801;

    template<uint32_t X>
    static constexpr uint32_t CCIPR1_ADC345SEL =           // ADC3/4/5 clock source selection (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_ADCSEL =              // ADCs clock source selection (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_CLK48SEL =            // 48 MHz clock source selection (2 bits)
        bit_field_t<26, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_FDCANSEL =            // SAI2 clock source selection (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_SPISEL_ =             // SAI1 clock source selection (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_SAISEL =              // Low power timer 2 clock source selection (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_LPTIM1SEL =           // Low power timer 1 clock source selection (2 bits)
        bit_field_t<18, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_I2C3SEL =             // I2C3 clock source selection (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_I2C2SEL =             // I2C2 clock source selection (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_I2C1SEL =             // I2C1 clock source selection (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_LPUART1SEL =          // LPUART1 clock source selection (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_UART5SEL =            // UART5 clock source selection (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_UART4SEL =            // UART4 clock source selection (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_USART3SEL =           // USART3 clock source selection (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_USART2SEL =           // USART2 clock source selection (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR1_USART1SEL =           // USART1 clock source selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t CCIPR1_RESET_VALUE = 0x0;

    static constexpr uint32_t BDCR_LSCOSEL = 0x2000000;  // Low speed clock output selection, Read-write
    static constexpr uint32_t BDCR_LSCCOEN = 0x1000000;  // Low speed clock output enable, Read-write
    static constexpr uint32_t BDCR_VSWRST = 0x10000;     // Vswitch domain software reset, Read-write
    static constexpr uint32_t BDCR_RTCEN = 0x8000;       // RTC clock enable, Read-write
    template<uint32_t X>
    static constexpr uint32_t BDCR_RTCSEL =              // RTC clock source selection (2 bits), Read-write
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t BDCR_LSECSSD = 0x40;       // LSECSSD, Read-only
    static constexpr uint32_t BDCR_LSECSSON = 0x20;      // LSECSSON, Read-write
    template<uint32_t X>
    static constexpr uint32_t BDCR_LSEDRV =              // SE oscillator drive capability (2 bits), Read-write
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t BDCR_LSEBYP = 0x4;         // LSE oscillator bypass, Read-write
    static constexpr uint32_t BDCR_LSERDY = 0x2;         // LSE oscillator ready, Read-only
    static constexpr uint32_t BDCR_LSEON = 0x1;          // LSE oscillator enable, Read-write
    static const uint32_t BDCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CSR_LPWRSTF = 0x80000000; // Low-power reset flag, Read-only
    static constexpr uint32_t CSR_WWDGRSTF = 0x40000000;// Window watchdog reset flag, Read-only
    static constexpr uint32_t CSR_WDGRSTF = 0x20000000; // Independent window watchdog reset flag, Read-only
    static constexpr uint32_t CSR_SFTRSTF = 0x10000000; // Software reset flag, Read-only
    static constexpr uint32_t CSR_BORRSTF = 0x8000000;  // BOR flag, Read-only
    static constexpr uint32_t CSR_PADRSTF = 0x4000000;  // Pad reset flag, Read-only
    static constexpr uint32_t CSR_OBLRSTF = 0x2000000;  // Option byte loader reset flag, Read-only
    static constexpr uint32_t CSR_RMVF = 0x800000;      // Remove reset flag, Read-write
    static constexpr uint32_t CSR_LSIRDY = 0x2;         // LSI oscillator ready, Read-only
    static constexpr uint32_t CSR_LSION = 0x1;          // LSI oscillator enable, Read-write
    static const uint32_t CSR_RESET_VALUE = 0xc000000;

    static constexpr uint32_t CRRCR_RC48ON = 0x1;         // HSI48 clock enable, Read-write
    static constexpr uint32_t CRRCR_RC48RDY = 0x2;        // HSI48 clock ready flag, Read-only
    template<uint32_t X>
    static constexpr uint32_t CRRCR_RC48CAL =             // HSI48 clock calibration (9 bits), Read-only
        bit_field_t<7, 0x1ff>::value<X>();
    static const uint32_t CRRCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCIPR2_I2C4SEL =             // I2C4 clock source selection (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCIPR2_QUADSPISEL =          // Octospi clock source selection (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    static const uint32_t CCIPR2_RESET_VALUE = 0x0;

    static constexpr uint8_t RCC = 5; // RCC
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
    volatile uint32_t    PUCRE;                // [Read-write] Power Port E pull-up control register
    volatile uint32_t    PDCRE;                // [Read-write] Power Port E pull-down control register
    volatile uint32_t    PUCRF;                // [Read-write] Power Port F pull-up control register
    volatile uint32_t    PDCRF;                // [Read-write] Power Port F pull-down control register
    volatile uint32_t    PUCRG;                // [Read-write] Power Port G pull-up control register
    volatile uint32_t    PDCRG;                // [Read-write] Power Port G pull-down control register
    reserved_t<10>       _1;
    volatile uint32_t    CR5;                  // [Read-write] Power control register 5

    static constexpr uint32_t CR1_LPR = 0x4000;         // Low-power run
    template<uint32_t X>
    static constexpr uint32_t CR1_VOS =                 // Voltage scaling range selection (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t CR1_DBP = 0x100;          // Disable backup domain write protection
    template<uint32_t X>
    static constexpr uint32_t CR1_LPMS =                // Low-power mode selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t CR1_RESET_VALUE = 0x200;

    static constexpr uint32_t CR2_PVMEN1 = 0x10;        // Peripheral voltage monitoring 1 enable: VDDA vs. COMP min voltage
    template<uint32_t X>
    static constexpr uint32_t CR2_PLS =                 // Power voltage detector level selection (3 bits)
        bit_field_t<1, 0x7>::value<X>();
    static constexpr uint32_t CR2_PVDE = 0x1;           // Power voltage detector enable
    static constexpr uint32_t CR2_PVMEN2 = 0x20;        // Peripheral voltage monitoring 2 enable: VDDA vs. Fast DAC min voltage
    static constexpr uint32_t CR2_PVMEN3 = 0x40;        // Peripheral voltage monitoring 3 enable: VDDA vs. ADC min voltage 1.62V
    static constexpr uint32_t CR2_PVMEN4 = 0x80;        // Peripheral voltage monitoring 4 enable: VDDA vs. OPAMP/DAC min voltage
    static const uint32_t CR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CR3_EWUP1 = 0x1;          // Enable Wakeup pin WKUP1
    static constexpr uint32_t CR3_EWUP2 = 0x2;          // Enable Wakeup pin WKUP2
    static constexpr uint32_t CR3_EWUP3 = 0x4;          // Enable Wakeup pin WKUP3
    static constexpr uint32_t CR3_EWUP4 = 0x8;          // Enable Wakeup pin WKUP4
    static constexpr uint32_t CR3_EWUP5 = 0x10;         // Enable Wakeup pin WKUP5
    static constexpr uint32_t CR3_RRS = 0x100;          // SRAM2 retention in Standby mode
    static constexpr uint32_t CR3_APC = 0x400;          // Apply pull-up and pull-down configuration
    static constexpr uint32_t CR3_UCPD1_STDBY = 0x2000; // STDBY
    static constexpr uint32_t CR3_UCPD1_DBDIS = 0x4000; // DBDIS
    static constexpr uint32_t CR3_EIWUL = 0x8000;       // Enable external WakeUp line
    static const uint32_t CR3_RESET_VALUE = 0x8000;

    static constexpr uint32_t CR4_VBRS = 0x200;         // VBAT battery charging resistor selection
    static constexpr uint32_t CR4_VBE = 0x100;          // VBAT battery charging enable
    static constexpr uint32_t CR4_WP5 = 0x10;           // Wakeup pin WKUP5 polarity
    static constexpr uint32_t CR4_WP4 = 0x8;            // Wakeup pin WKUP4 polarity
    static constexpr uint32_t CR4_WP3 = 0x4;            // Wakeup pin WKUP3 polarity
    static constexpr uint32_t CR4_WP2 = 0x2;            // Wakeup pin WKUP2 polarity
    static constexpr uint32_t CR4_WP1 = 0x1;            // Wakeup pin WKUP1 polarity
    static const uint32_t CR4_RESET_VALUE = 0x0;

    static constexpr uint32_t SR1_WUFI = 0x8000;        // Wakeup flag internal
    static constexpr uint32_t SR1_SBF = 0x100;          // Standby flag
    static constexpr uint32_t SR1_WUF5 = 0x10;          // Wakeup flag 5
    static constexpr uint32_t SR1_WUF4 = 0x8;           // Wakeup flag 4
    static constexpr uint32_t SR1_WUF3 = 0x4;           // Wakeup flag 3
    static constexpr uint32_t SR1_WUF2 = 0x2;           // Wakeup flag 2
    static constexpr uint32_t SR1_WUF1 = 0x1;           // Wakeup flag 1
    static const uint32_t SR1_RESET_VALUE = 0x0;

    static constexpr uint32_t SR2_PVMO4 = 0x8000;       // Peripheral voltage monitoring output: VDDA vs. 2.2 V
    static constexpr uint32_t SR2_PVMO3 = 0x4000;       // Peripheral voltage monitoring output: VDDA vs. 1.62 V
    static constexpr uint32_t SR2_PVMO2 = 0x2000;       // Peripheral voltage monitoring output: VDDIO2 vs. 0.9 V
    static constexpr uint32_t SR2_PVMO1 = 0x1000;       // Peripheral voltage monitoring output: VDDUSB vs. 1.2 V
    static constexpr uint32_t SR2_PVDO = 0x800;         // Power voltage detector output
    static constexpr uint32_t SR2_VOSF = 0x400;         // Voltage scaling flag
    static constexpr uint32_t SR2_REGLPF = 0x200;       // Low-power regulator flag
    static constexpr uint32_t SR2_REGLPS = 0x100;       // Low-power regulator started
    static const uint32_t SR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SCR_CSBF = 0x100;         // Clear standby flag
    static constexpr uint32_t SCR_CWUF5 = 0x10;         // Clear wakeup flag 5
    static constexpr uint32_t SCR_CWUF4 = 0x8;          // Clear wakeup flag 4
    static constexpr uint32_t SCR_CWUF3 = 0x4;          // Clear wakeup flag 3
    static constexpr uint32_t SCR_CWUF2 = 0x2;          // Clear wakeup flag 2
    static constexpr uint32_t SCR_CWUF1 = 0x1;          // Clear wakeup flag 1
    static const uint32_t SCR_RESET_VALUE = 0x0;

    static constexpr uint32_t PUCRA_PU15 = 0x8000;        // Port A pull-up bit y (y=0..15)
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

    static constexpr uint32_t PDCRA_PD14 = 0x4000;        // Port A pull-down bit y (y=0..15)
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

    static constexpr uint32_t PUCRD_PU15 = 0x8000;        // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU14 = 0x4000;        // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU13 = 0x2000;        // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU12 = 0x1000;        // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU11 = 0x800;         // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU10 = 0x400;         // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU9 = 0x200;          // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU8 = 0x100;          // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU7 = 0x80;           // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU6 = 0x40;           // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU5 = 0x20;           // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU4 = 0x10;           // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU3 = 0x8;            // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU2 = 0x4;            // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU1 = 0x2;            // Port D pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRD_PU0 = 0x1;            // Port D pull-up bit y (y=0..15)
    static const uint32_t PUCRD_RESET_VALUE = 0x0;

    static constexpr uint32_t PDCRD_PD15 = 0x8000;        // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD14 = 0x4000;        // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD13 = 0x2000;        // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD12 = 0x1000;        // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD11 = 0x800;         // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD10 = 0x400;         // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD9 = 0x200;          // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD8 = 0x100;          // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD7 = 0x80;           // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD6 = 0x40;           // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD5 = 0x20;           // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD4 = 0x10;           // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD3 = 0x8;            // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD2 = 0x4;            // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD1 = 0x2;            // Port D pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRD_PD0 = 0x1;            // Port D pull-down bit y (y=0..15)
    static const uint32_t PDCRD_RESET_VALUE = 0x0;

    static constexpr uint32_t PUCRE_PU15 = 0x8000;        // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU14 = 0x4000;        // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU13 = 0x2000;        // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU12 = 0x1000;        // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU11 = 0x800;         // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU10 = 0x400;         // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU9 = 0x200;          // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU8 = 0x100;          // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU7 = 0x80;           // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU6 = 0x40;           // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU5 = 0x20;           // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU4 = 0x10;           // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU3 = 0x8;            // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU2 = 0x4;            // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU1 = 0x2;            // Port E pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRE_PU0 = 0x1;            // Port E pull-up bit y (y=0..15)
    static const uint32_t PUCRE_RESET_VALUE = 0x0;

    static constexpr uint32_t PDCRE_PD15 = 0x8000;        // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD14 = 0x4000;        // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD13 = 0x2000;        // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD12 = 0x1000;        // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD11 = 0x800;         // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD10 = 0x400;         // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD9 = 0x200;          // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD8 = 0x100;          // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD7 = 0x80;           // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD6 = 0x40;           // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD5 = 0x20;           // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD4 = 0x10;           // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD3 = 0x8;            // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD2 = 0x4;            // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD1 = 0x2;            // Port E pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRE_PD0 = 0x1;            // Port E pull-down bit y (y=0..15)
    static const uint32_t PDCRE_RESET_VALUE = 0x0;

    static constexpr uint32_t PUCRF_PU15 = 0x8000;        // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU14 = 0x4000;        // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU13 = 0x2000;        // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU12 = 0x1000;        // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU11 = 0x800;         // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU10 = 0x400;         // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU9 = 0x200;          // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU8 = 0x100;          // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU7 = 0x80;           // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU6 = 0x40;           // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU5 = 0x20;           // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU4 = 0x10;           // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU3 = 0x8;            // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU2 = 0x4;            // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU1 = 0x2;            // Port F pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRF_PU0 = 0x1;            // Port F pull-up bit y (y=0..15)
    static const uint32_t PUCRF_RESET_VALUE = 0x0;

    static constexpr uint32_t PDCRF_PD15 = 0x8000;        // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD14 = 0x4000;        // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD13 = 0x2000;        // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD12 = 0x1000;        // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD11 = 0x800;         // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD10 = 0x400;         // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD9 = 0x200;          // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD8 = 0x100;          // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD7 = 0x80;           // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD6 = 0x40;           // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD5 = 0x20;           // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD4 = 0x10;           // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD3 = 0x8;            // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD2 = 0x4;            // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD1 = 0x2;            // Port F pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRF_PD0 = 0x1;            // Port F pull-down bit y (y=0..15)
    static const uint32_t PDCRF_RESET_VALUE = 0x0;

    static constexpr uint32_t PUCRG_PU10 = 0x400;         // Port G pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRG_PU9 = 0x200;          // Port G pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRG_PU8 = 0x100;          // Port G pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRG_PU7 = 0x80;           // Port G pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRG_PU6 = 0x40;           // Port G pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRG_PU5 = 0x20;           // Port G pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRG_PU4 = 0x10;           // Port G pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRG_PU3 = 0x8;            // Port G pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRG_PU2 = 0x4;            // Port G pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRG_PU1 = 0x2;            // Port G pull-up bit y (y=0..15)
    static constexpr uint32_t PUCRG_PU0 = 0x1;            // Port G pull-up bit y (y=0..15)
    static const uint32_t PUCRG_RESET_VALUE = 0x0;

    static constexpr uint32_t PDCRG_PD10 = 0x400;         // Port G pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRG_PD9 = 0x200;          // Port G pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRG_PD8 = 0x100;          // Port G pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRG_PD7 = 0x80;           // Port G pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRG_PD6 = 0x40;           // Port G pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRG_PD5 = 0x20;           // Port G pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRG_PD4 = 0x10;           // Port G pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRG_PD3 = 0x8;            // Port G pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRG_PD2 = 0x4;            // Port G pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRG_PD1 = 0x2;            // Port G pull-down bit y (y=0..15)
    static constexpr uint32_t PDCRG_PD0 = 0x1;            // Port G pull-down bit y (y=0..15)
    static const uint32_t PDCRG_RESET_VALUE = 0x0;

    static constexpr uint32_t CR5_R1MODE = 0x1;         // Main regular range 1 mode
    static const uint32_t CR5_RESET_VALUE = 0x100;
};

static pwr_t& PWR = *reinterpret_cast<pwr_t*>(0x40007000);

#define HAVE_PERIPHERAL_PWR


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

    static constexpr uint32_t CR_CED = 0x20;           // Clock error detection
    static constexpr uint32_t CR_IE = 0x8;             // Interrupt enable
    static constexpr uint32_t CR_RNGEN = 0x4;          // Random number generator enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_SEIS = 0x40;          // Seed error interrupt status, Read-write
    static constexpr uint32_t SR_CEIS = 0x20;          // Clock error interrupt status, Read-write
    static constexpr uint32_t SR_SECS = 0x4;           // Seed error current status, Read-only
    static constexpr uint32_t SR_CECS = 0x2;           // Clock error current status, Read-only
    static constexpr uint32_t SR_DRDY = 0x1;           // Data ready, Read-only
    static const uint32_t SR_RESET_VALUE = 0x0;


    static const uint32_t DR_RESET_VALUE = 0x0;

    static constexpr uint8_t RNG = 90; // RNG
};

static rng_t& RNG = *reinterpret_cast<rng_t*>(0x50060800);

#define HAVE_PERIPHERAL_RNG


////
//
//    Advanced encryption standard hardware accelerator
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
    volatile uint32_t    SUSP0R;               // [Read-write] suspend registers
    volatile uint32_t    SUSP1R;               // [Read-write] suspend registers
    volatile uint32_t    SUSP2R;               // [Read-write] suspend registers
    volatile uint32_t    SUSP3R;               // [Read-write] suspend registers
    volatile uint32_t    SUSP4R;               // [Read-write] suspend registers
    volatile uint32_t    SUSP5R;               // [Read-write] suspend registers
    volatile uint32_t    SUSP6R;               // [Read-write] suspend registers
    volatile uint32_t    SUSP7R;               // [Read-write] suspend registers

    template<uint32_t X>
    static constexpr uint32_t CR_NPBLB =               // NPBLB (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    static constexpr uint32_t CR_KEYSIZE = 0x40000;    // KEYSIZE
    static constexpr uint32_t CR_CHMOD_2 = 0x10000;    // CHMOD_2
    template<uint32_t X>
    static constexpr uint32_t CR_GCMPH =               // GCMPH (2 bits)
        bit_field_t<13, 0x3>::value<X>();
    static constexpr uint32_t CR_DMAOUTEN = 0x1000;    // Enable DMA management of data output phase
    static constexpr uint32_t CR_DMAINEN = 0x800;      // Enable DMA management of data input phase
    static constexpr uint32_t CR_ERRIE = 0x400;        // Error interrupt enable
    static constexpr uint32_t CR_CCFIE = 0x200;        // CCF flag interrupt enable
    static constexpr uint32_t CR_ERRC = 0x100;         // Error clear
    static constexpr uint32_t CR_CCFC = 0x80;          // Computation Complete Flag Clear
    template<uint32_t X>
    static constexpr uint32_t CR_CHMOD =               // AES chaining mode (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_MODE =                // AES operating mode (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CR_DATATYPE =            // Data type selection (for data in and data out to/from the cryptographic block) (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t CR_EN = 0x1;             // AES enable
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_BUSY = 0x8;           // BUSY
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

    static constexpr uint8_t AES = 85; // AES
};

static aes_t& AES = *reinterpret_cast<aes_t*>(0x50060000);

#define HAVE_PERIPHERAL_AES


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
    volatile uint32_t    BRR;                  // [Write-only] GPIO port bit reset register

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
    static const uint32_t MODER_RESET_VALUE = 0xabffffff;

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
    static const uint32_t PUPDR_RESET_VALUE = 0x64000000;

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

static gpioa_t& GPIOA = *reinterpret_cast<gpioa_t*>(0x48000000);

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
    volatile uint32_t    BRR;                  // [Write-only] GPIO port bit reset register

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
    static const uint32_t MODER_RESET_VALUE = 0xfffffebf;

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
    static const uint32_t OSPEEDR_RESET_VALUE = 0xc0;

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
    static const uint32_t PUPDR_RESET_VALUE = 0x100;

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

static gpiob_t& GPIOB = *reinterpret_cast<gpiob_t*>(0x48000400);

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
    volatile uint32_t    BRR;                  // [Write-only] GPIO port bit reset register

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

static gpioc_t& GPIOC = *reinterpret_cast<gpioc_t*>(0x48000800);

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
    volatile uint32_t    BRR;                  // [Write-only] GPIO port bit reset register

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

static gpiod_t& GPIOD = *reinterpret_cast<gpiod_t*>(0x48000c00);

#define HAVE_PERIPHERAL_GPIOD


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
    volatile uint32_t    BRR;                  // [Write-only] GPIO port bit reset register

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

static gpioe_t& GPIOE = *reinterpret_cast<gpioe_t*>(0x48001000);

#define HAVE_PERIPHERAL_GPIOE


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
    volatile uint32_t    BRR;                  // [Write-only] GPIO port bit reset register

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

static gpiof_t& GPIOF = *reinterpret_cast<gpiof_t*>(0x48001400);

#define HAVE_PERIPHERAL_GPIOF


////
//
//    General-purpose I/Os
//
////

struct gpiog_t
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
    volatile uint32_t    BRR;                  // [Write-only] GPIO port bit reset register

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

static gpiog_t& GPIOG = *reinterpret_cast<gpiog_t*>(0x48001800);

#define HAVE_PERIPHERAL_GPIOG


////
//
//    General purpose timers
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
    volatile uint32_t    CNT;                  // counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register
    volatile uint32_t    RCR;                  // [Read-write] repetition counter register
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    volatile uint32_t    CCR2;                 // [Read-write] capture/compare register 2
    reserved_t<2>        _1;
    volatile uint32_t    BDTR;                 // [Read-write] break and dead-time register
    reserved_t<3>        _2;
    volatile uint32_t    DTR2;                 // [Read-write] timer Deadtime Register 2
    reserved_t<1>        _3;
    volatile uint32_t    TISEL;                // [Read-write] TIM timer input selection register
    volatile uint32_t    AF1;                  // [Read-write] TIM alternate function option register 1
    volatile uint32_t    AF2;                  // [Read-write] TIM alternate function option register 2
    reserved_t<221>      _4;
    volatile uint32_t    DCR;                  // [Read-write] DMA control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_UIFREMAP = 0x800;     // UIF status bit remapping
    static constexpr uint32_t CR1_DITHEN = 0x1000;      // Dithering Enable
    static const uint32_t CR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CR2_OIS2 = 0x400;         // Output idle state 2 (OC2 output)
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
    static constexpr uint32_t SMCR_TS_4_3 =              // Trigger selection - bit 4:3 (2 bits)
        bit_field_t<20, 0x3>::value<X>();
    static constexpr uint32_t SMCR_SMS_3 = 0x10000;      // Slave mode selection - bit 3
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

    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/Compare 2 overcapture flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/compare 2 interrupt flag
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
    static constexpr uint32_t CCMR1_CC2S =                // CC2S (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1F =                // Input capture 1 filter (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC1PSC =              // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC2F =                // IC2F (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_IC2PSC =              // IC2PSC (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t CCMR1_OC1CE = 0x80;         // OC1CE
    static constexpr uint32_t CCMR1_OC1FE = 0x4;          // Output Compare 1 fast enable
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC1M =                // Output Compare 1 mode (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC1M_3 = 0x10000;     // Output Compare 1 mode
    static constexpr uint32_t CCMR1_OC1PE = 0x8;          // Output Compare 1 preload enable
    static constexpr uint32_t CCMR1_OC2FE = 0x400;        // OC2FE
    template<uint32_t X>
    static constexpr uint32_t CCMR1_OC2M =                // OC2M (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    static constexpr uint32_t CCMR1_OC2M_3 = 0x1000000;   // Output Compare 2 mode - bit 3
    static constexpr uint32_t CCMR1_OC2PE = 0x800;        // OC2PE
    static const uint32_t CCMR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCER_CC2NP = 0x80;         // Capture/Compare 2 complementary output polarity
    static constexpr uint32_t CCER_CC2P = 0x20;          // Capture/Compare 2 output polarity
    static constexpr uint32_t CCER_CC2E = 0x10;          // Capture/Compare 2 output enable
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
    static const uint32_t ARR_RESET_VALUE = 0xffff;

    template<uint32_t X>
    static constexpr uint32_t RCR_REP =                 // Repetition counter value (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t RCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR1_CCR1 =                // Capture/Compare 1 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR2_CCR2 =                // Capture/Compare 1 value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CCR2_RESET_VALUE = 0x0;

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
    static constexpr uint32_t BDTR_BKDSRM = 0x4000000;   // BKDSRM
    static constexpr uint32_t BDTR_BKBID = 0x10000000;   // BKBID
    static const uint32_t BDTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DTR2_DTGF =                // Dead-time generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t DTR2_DTAE = 0x10000;       // Deadtime Asymmetric Enable
    static constexpr uint32_t DTR2_DTPE = 0x20000;       // Deadtime Preload Enable
    static const uint32_t DTR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // TI1[0] to TI1[15] input selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI2SEL =              // TI2[0] to TI2[15] input selection (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    static constexpr uint32_t AF1_BKCMP4P = 0x2000;     // BRK COMP4 input polarity
    static constexpr uint32_t AF1_BKCMP3P = 0x1000;     // BRK COMP3 input polarity
    static constexpr uint32_t AF1_BKCMP2P = 0x800;      // BRK COMP2 input polarity
    static constexpr uint32_t AF1_BKCMP1P = 0x400;      // BRK COMP1 input polarity
    static constexpr uint32_t AF1_BKINP = 0x200;        // BRK BKIN input polarity
    static constexpr uint32_t AF1_BKCMP7E = 0x80;       // BRK COMP7 enable
    static constexpr uint32_t AF1_BKCMP6E = 0x40;       // BRK COMP6 enable
    static constexpr uint32_t AF1_BKCMP5E = 0x20;       // BRK COMP5 enable
    static constexpr uint32_t AF1_BKCMP4E = 0x10;       // BRK COMP4 enable
    static constexpr uint32_t AF1_BKCMP3E = 0x8;        // BRK COMP3 enable
    static constexpr uint32_t AF1_BKCMP2E = 0x4;        // BRK COMP2 enable
    static constexpr uint32_t AF1_BKCMP1E = 0x2;        // BRK COMP1 enable
    static constexpr uint32_t AF1_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF2_OCRSEL =              // OCREF_CLR source selection (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static const uint32_t AF2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;


    static const uint32_t DMAR_RESET_VALUE = 0x0;
};

static tim15_t& TIM15 = *reinterpret_cast<tim15_t*>(0x40014000);

#define HAVE_PERIPHERAL_TIM15


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
    reserved_t<3>        _3;
    volatile uint32_t    DTR2;                 // [Read-write] timer Deadtime Register 2
    reserved_t<1>        _4;
    volatile uint32_t    TISEL;                // [Read-write] TIM timer input selection register
    volatile uint32_t    AF1;                  // [Read-write] TIM alternate function option register 1
    volatile uint32_t    AF2;                  // [Read-write] TIM alternate function option register 2
    reserved_t<221>      _5;
    volatile uint32_t    DCR;                  // [Read-write] DMA control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_UIFREMAP = 0x800;     // UIF status bit remapping
    static constexpr uint32_t CR1_DITHEN = 0x1000;      // Dithering Enable
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
    static constexpr uint32_t CCMR1_OC1M_3 = 0x10000;     // Output Compare 1 mode
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
    static const uint32_t ARR_RESET_VALUE = 0xffff;

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
    static constexpr uint32_t BDTR_BKDSRM = 0x4000000;   // BKDSRM
    static constexpr uint32_t BDTR_BKBID = 0x10000000;   // BKBID
    static const uint32_t BDTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DTR2_DTGF =                // Dead-time generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t DTR2_DTAE = 0x10000;       // Deadtime Asymmetric Enable
    static constexpr uint32_t DTR2_DTPE = 0x20000;       // Deadtime Preload Enable
    static const uint32_t DTR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // TI1[0] to TI1[15] input selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    static constexpr uint32_t AF1_BKCMP4P = 0x2000;     // BRK COMP4 input polarity
    static constexpr uint32_t AF1_BKCMP3P = 0x1000;     // BRK COMP3 input polarity
    static constexpr uint32_t AF1_BKCMP2P = 0x800;      // BRK COMP2 input polarity
    static constexpr uint32_t AF1_BKCMP1P = 0x400;      // BRK COMP1 input polarity
    static constexpr uint32_t AF1_BKINP = 0x200;        // BRK BKIN input polarity
    static constexpr uint32_t AF1_BKCMP7E = 0x80;       // BRK COMP7 enable
    static constexpr uint32_t AF1_BKCMP6E = 0x40;       // BRK COMP6 enable
    static constexpr uint32_t AF1_BKCMP5E = 0x20;       // BRK COMP5 enable
    static constexpr uint32_t AF1_BKCMP4E = 0x10;       // BRK COMP4 enable
    static constexpr uint32_t AF1_BKCMP3E = 0x8;        // BRK COMP3 enable
    static constexpr uint32_t AF1_BKCMP2E = 0x4;        // BRK COMP2 enable
    static constexpr uint32_t AF1_BKCMP1E = 0x2;        // BRK COMP1 enable
    static constexpr uint32_t AF1_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF2_OCRSEL =              // OCREF_CLR source selection (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static const uint32_t AF2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;


    static const uint32_t DMAR_RESET_VALUE = 0x0;
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
    reserved_t<3>        _3;
    volatile uint32_t    DTR2;                 // [Read-write] timer Deadtime Register 2
    reserved_t<1>        _4;
    volatile uint32_t    TISEL;                // [Read-write] TIM timer input selection register
    volatile uint32_t    AF1;                  // [Read-write] TIM alternate function option register 1
    volatile uint32_t    AF2;                  // [Read-write] TIM alternate function option register 2
    reserved_t<221>      _5;
    volatile uint32_t    DCR;                  // [Read-write] DMA control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static constexpr uint32_t CR1_CEN = 0x1;            // Counter enable
    static constexpr uint32_t CR1_UDIS = 0x2;           // Update disable
    static constexpr uint32_t CR1_URS = 0x4;            // Update request source
    static constexpr uint32_t CR1_OPM = 0x8;            // One-pulse mode
    static constexpr uint32_t CR1_ARPE = 0x80;          // Auto-reload preload enable
    template<uint32_t X>
    static constexpr uint32_t CR1_CKD =                 // Clock division (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t CR1_UIFREMAP = 0x800;     // UIF status bit remapping
    static constexpr uint32_t CR1_DITHEN = 0x1000;      // Dithering Enable
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
    static constexpr uint32_t CCMR1_OC1M_3 = 0x10000;     // Output Compare 1 mode
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
    static const uint32_t ARR_RESET_VALUE = 0xffff;

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
    static constexpr uint32_t BDTR_BKDSRM = 0x4000000;   // BKDSRM
    static constexpr uint32_t BDTR_BKBID = 0x10000000;   // BKBID
    static const uint32_t BDTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DTR2_DTGF =                // Dead-time generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t DTR2_DTAE = 0x10000;       // Deadtime Asymmetric Enable
    static constexpr uint32_t DTR2_DTPE = 0x20000;       // Deadtime Preload Enable
    static const uint32_t DTR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // TI1[0] to TI1[15] input selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    static constexpr uint32_t AF1_BKCMP4P = 0x2000;     // BRK COMP4 input polarity
    static constexpr uint32_t AF1_BKCMP3P = 0x1000;     // BRK COMP3 input polarity
    static constexpr uint32_t AF1_BKCMP2P = 0x800;      // BRK COMP2 input polarity
    static constexpr uint32_t AF1_BKCMP1P = 0x400;      // BRK COMP1 input polarity
    static constexpr uint32_t AF1_BKINP = 0x200;        // BRK BKIN input polarity
    static constexpr uint32_t AF1_BKCMP7E = 0x80;       // BRK COMP7 enable
    static constexpr uint32_t AF1_BKCMP6E = 0x40;       // BRK COMP6 enable
    static constexpr uint32_t AF1_BKCMP5E = 0x20;       // BRK COMP5 enable
    static constexpr uint32_t AF1_BKCMP4E = 0x10;       // BRK COMP4 enable
    static constexpr uint32_t AF1_BKCMP3E = 0x8;        // BRK COMP3 enable
    static constexpr uint32_t AF1_BKCMP2E = 0x4;        // BRK COMP2 enable
    static constexpr uint32_t AF1_BKCMP1E = 0x2;        // BRK COMP1 enable
    static constexpr uint32_t AF1_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF2_OCRSEL =              // OCREF_CLR source selection (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static const uint32_t AF2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;


    static const uint32_t DMAR_RESET_VALUE = 0x0;
};

static tim17_t& TIM17 = *reinterpret_cast<tim17_t*>(0x40014800);

#define HAVE_PERIPHERAL_TIM17


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
    volatile uint32_t    CCR5;                 // [Read-write] capture/compare register 4
    volatile uint32_t    CCR6;                 // [Read-write] capture/compare register 4
    volatile uint32_t    CCMR3_Output;         // [Read-write] capture/compare mode register 2 (output mode)
    volatile uint32_t    DTR2;                 // [Read-write] timer Deadtime Register 2
    volatile uint32_t    ECR;                  // [Read-write] DMA control register
    volatile uint32_t    TISEL;                // [Read-write] TIM timer input selection register
    volatile uint32_t    AF1;                  // [Read-write] TIM alternate function option register 1
    volatile uint32_t    AF2;                  // [Read-write] TIM alternate function option register 2
    reserved_t<221>      _0;
    volatile uint32_t    DCR;                  // [Read-write] control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static constexpr uint32_t CR1_DITHEN = 0x1000;      // Dithering Enable
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

    static constexpr uint32_t CR2_MMS_3 = 0x2000000;    // Master mode selection - bit 3
    template<uint32_t X>
    static constexpr uint32_t CR2_MMS2 =                // Master mode selection 2 (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    static constexpr uint32_t CR2_OIS6 = 0x40000;       // Output Idle state 6 (OC6 output)
    static constexpr uint32_t CR2_OIS5 = 0x10000;       // Output Idle state 5 (OC5 output)
    static constexpr uint32_t CR2_OIS4N = 0x8000;       // Output Idle state 4 (OC4N output)
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

    static constexpr uint32_t SMCR_SMSPS = 0x2000000;    // SMS Preload Source
    static constexpr uint32_t SMCR_SMSPE = 0x1000000;    // SMS Preload Enable
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS_4_3 =              // Trigger selection - bit 4:3 (2 bits)
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

    static constexpr uint32_t DIER_TERRIE = 0x800000;    // Transition Error interrupt enable
    static constexpr uint32_t DIER_IERRIE = 0x400000;    // Index Error interrupt enable
    static constexpr uint32_t DIER_DIRIE = 0x200000;     // Direction Change interrupt enable
    static constexpr uint32_t DIER_IDXIE = 0x100000;     // Index interrupt enable
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

    static constexpr uint32_t SR_TERRF = 0x800000;     // Transition Error interrupt flag
    static constexpr uint32_t SR_IERRF = 0x400000;     // Index Error interrupt flag
    static constexpr uint32_t SR_DIRF = 0x200000;      // Direction Change interrupt flag
    static constexpr uint32_t SR_IDXF = 0x100000;      // Index interrupt flag
    static constexpr uint32_t SR_CC6IF = 0x20000;      // Compare 6 interrupt flag
    static constexpr uint32_t SR_CC5IF = 0x10000;      // Compare 5 interrupt flag
    static constexpr uint32_t SR_SBIF = 0x2000;        // System Break interrupt flag
    static constexpr uint32_t SR_CC4OF = 0x1000;       // Capture/Compare 4 overcapture flag
    static constexpr uint32_t SR_CC3OF = 0x800;        // Capture/Compare 3 overcapture flag
    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/compare 2 overcapture flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_B2IF = 0x100;         // Break 2 interrupt flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC4IF = 0x10;         // Capture/Compare 4 interrupt flag
    static constexpr uint32_t SR_CC3IF = 0x8;          // Capture/Compare 3 interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/Compare 2 interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_B2G = 0x100;          // Break 2 generation
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
    static constexpr uint32_t CCMR1_IC2PSC =              // Input capture 2 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_ICPCS =               // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
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

    static constexpr uint32_t CCER_CC6P = 0x200000;      // Capture/Compare 6 output polarity
    static constexpr uint32_t CCER_CC6E = 0x100000;      // Capture/Compare 6 output enable
    static constexpr uint32_t CCER_CC5P = 0x20000;       // Capture/Compare 5 output polarity
    static constexpr uint32_t CCER_CC5E = 0x10000;       // Capture/Compare 5 output enable
    static constexpr uint32_t CCER_CC4NP = 0x8000;       // Capture/Compare 4 complementary output polarity
    static constexpr uint32_t CCER_CC4NE = 0x4000;       // Capture/Compare 4 complementary output enable
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

    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIFCPY, Read-only
    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // counter value (16 bits), Read-write
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0xffff;

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

    static constexpr uint32_t BDTR_BK2ID = 0x20000000;   // BK2ID
    static constexpr uint32_t BDTR_BKBID = 0x10000000;   // BKBID
    static constexpr uint32_t BDTR_BK2DSRM = 0x8000000;  // BK2DSRM
    static constexpr uint32_t BDTR_BKDSRM = 0x4000000;   // BKDSRM
    static constexpr uint32_t BDTR_BK2P = 0x2000000;     // Break 2 polarity
    static constexpr uint32_t BDTR_BK2E = 0x1000000;     // Break 2 Enable
    template<uint32_t X>
    static constexpr uint32_t BDTR_BK2F =                // Break 2 filter (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_BKF =                 // Break filter (4 bits)
        bit_field_t<16, 0xf>::value<X>();
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

    static constexpr uint32_t CCMR3_Output_OC6M_bit3 = 0x1000000;// Output Compare 6 mode bit 3
    template<uint32_t X>
    static constexpr uint32_t CCMR3_Output_OC5M_bit3 =           // Output Compare 5 mode bit 3 (3 bits)
        bit_field_t<16, 0x7>::value<X>();
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

    static constexpr uint32_t DTR2_DTPE = 0x20000;       // Deadtime Preload Enable
    static constexpr uint32_t DTR2_DTAE = 0x10000;       // Deadtime Asymmetric Enable
    template<uint32_t X>
    static constexpr uint32_t DTR2_DTGF =                // Dead-time falling edge generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t DTR2_RESET_VALUE = 0x0;

    static constexpr uint32_t ECR_IE = 0x1;             // Index Enable
    template<uint32_t X>
    static constexpr uint32_t ECR_IDIR =                // Index Direction (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_IBLK =                // Index Blanking (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t ECR_FIDX = 0x20;          // First Index
    template<uint32_t X>
    static constexpr uint32_t ECR_IPOS =                // Index Positioning (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_PW =                  // Pulse width (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_PWPRSC =              // Pulse Width prescaler (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    static const uint32_t ECR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // TI1[0] to TI1[15] input selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI2SEL =              // TI2[0] to TI2[15] input selection (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI3SEL =              // TI3[0] to TI3[15] input selection (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI4SEL =              // TI4[0] to TI4[15] input selection (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF1_ETRSEL =              // ETR source selection (4 bits)
        bit_field_t<14, 0xf>::value<X>();
    static constexpr uint32_t AF1_BKCMP4P = 0x2000;     // BRK COMP4 input polarity
    static constexpr uint32_t AF1_BKCMP3P = 0x1000;     // BRK COMP3 input polarity
    static constexpr uint32_t AF1_BKCMP2P = 0x800;      // BRK COMP2 input polarity
    static constexpr uint32_t AF1_BKCMP1P = 0x400;      // BRK COMP1 input polarity
    static constexpr uint32_t AF1_BKINP = 0x200;        // BRK BKIN input polarity
    static constexpr uint32_t AF1_BKCMP7E = 0x80;       // BRK COMP7 enable
    static constexpr uint32_t AF1_BKCMP6E = 0x40;       // BRK COMP6 enable
    static constexpr uint32_t AF1_BKCMP5E = 0x20;       // BRK COMP5 enable
    static constexpr uint32_t AF1_BKCMP4E = 0x10;       // BRK COMP4 enable
    static constexpr uint32_t AF1_BKCMP3E = 0x8;        // BRK COMP3 enable
    static constexpr uint32_t AF1_BKCMP2E = 0x4;        // BRK COMP2 enable
    static constexpr uint32_t AF1_BKCMP1E = 0x2;        // BRK COMP1 enable
    static constexpr uint32_t AF1_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF2_OCRSEL =              // OCREF_CLR source selection (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static constexpr uint32_t AF2_BK2CMP4P = 0x2000;    // BRK2 COMP4 input polarity
    static constexpr uint32_t AF2_BK2CMP3P = 0x1000;    // BRK2 COMP3 input polarity
    static constexpr uint32_t AF2_BK2CMP2P = 0x800;     // BRK2 COMP2 input polarity
    static constexpr uint32_t AF2_BK2CMP1P = 0x400;     // BRK2 COMP1 input polarity
    static constexpr uint32_t AF2_BK2INP = 0x200;       // BRK2 BKIN input polarity
    static constexpr uint32_t AF2_BK2CMP7E = 0x80;      // BRK2 COMP7 enable
    static constexpr uint32_t AF2_BK2CMP6E = 0x40;      // BRK2 COMP6 enable
    static constexpr uint32_t AF2_BK2CMP5E = 0x20;      // BRK2 COMP5 enable
    static constexpr uint32_t AF2_BK2CMP4E = 0x10;      // BRK2 COMP4 enable
    static constexpr uint32_t AF2_BK2CMP3E = 0x8;       // BRK2 COMP3 enable
    static constexpr uint32_t AF2_BK2CMP2E = 0x4;       // BRK2 COMP2 enable
    static constexpr uint32_t AF2_BK2CMP1E = 0x2;       // BRK2 COMP1 enable
    static constexpr uint32_t AF2_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;


    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM1_BRK_TIM15 = 24; // TIM1_BRK_TIM15
    static constexpr uint8_t TIM1_CC = 27; // TIM1 capture compare interrupt
    static constexpr uint8_t TIM1_TRG_COM = 26; // TIM1_TRG_COM/
    static constexpr uint8_t TIM1_UP_TIM16 = 25; // TIM1_UP_TIM16
    static constexpr uint8_t TIM8_CC = 46; // TIM8_CC
};

static tim1_t& TIM1 = *reinterpret_cast<tim1_t*>(0x40012c00);

#define HAVE_PERIPHERAL_TIM1


////
//
//    Advanced-timers
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
    volatile uint32_t    CCR5;                 // [Read-write] capture/compare register 4
    volatile uint32_t    CCR6;                 // [Read-write] capture/compare register 4
    volatile uint32_t    CCMR3_Output;         // [Read-write] capture/compare mode register 2 (output mode)
    volatile uint32_t    DTR2;                 // [Read-write] timer Deadtime Register 2
    volatile uint32_t    ECR;                  // [Read-write] DMA control register
    volatile uint32_t    TISEL;                // [Read-write] TIM timer input selection register
    volatile uint32_t    AF1;                  // [Read-write] TIM alternate function option register 1
    volatile uint32_t    AF2;                  // [Read-write] TIM alternate function option register 2
    reserved_t<221>      _0;
    volatile uint32_t    DCR;                  // [Read-write] control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static constexpr uint32_t CR1_DITHEN = 0x1000;      // Dithering Enable
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

    static constexpr uint32_t CR2_MMS_3 = 0x2000000;    // Master mode selection - bit 3
    template<uint32_t X>
    static constexpr uint32_t CR2_MMS2 =                // Master mode selection 2 (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    static constexpr uint32_t CR2_OIS6 = 0x40000;       // Output Idle state 6 (OC6 output)
    static constexpr uint32_t CR2_OIS5 = 0x10000;       // Output Idle state 5 (OC5 output)
    static constexpr uint32_t CR2_OIS4N = 0x8000;       // Output Idle state 4 (OC4N output)
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

    static constexpr uint32_t SMCR_SMSPS = 0x2000000;    // SMS Preload Source
    static constexpr uint32_t SMCR_SMSPE = 0x1000000;    // SMS Preload Enable
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS_4_3 =              // Trigger selection - bit 4:3 (2 bits)
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

    static constexpr uint32_t DIER_TERRIE = 0x800000;    // Transition Error interrupt enable
    static constexpr uint32_t DIER_IERRIE = 0x400000;    // Index Error interrupt enable
    static constexpr uint32_t DIER_DIRIE = 0x200000;     // Direction Change interrupt enable
    static constexpr uint32_t DIER_IDXIE = 0x100000;     // Index interrupt enable
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

    static constexpr uint32_t SR_TERRF = 0x800000;     // Transition Error interrupt flag
    static constexpr uint32_t SR_IERRF = 0x400000;     // Index Error interrupt flag
    static constexpr uint32_t SR_DIRF = 0x200000;      // Direction Change interrupt flag
    static constexpr uint32_t SR_IDXF = 0x100000;      // Index interrupt flag
    static constexpr uint32_t SR_CC6IF = 0x20000;      // Compare 6 interrupt flag
    static constexpr uint32_t SR_CC5IF = 0x10000;      // Compare 5 interrupt flag
    static constexpr uint32_t SR_SBIF = 0x2000;        // System Break interrupt flag
    static constexpr uint32_t SR_CC4OF = 0x1000;       // Capture/Compare 4 overcapture flag
    static constexpr uint32_t SR_CC3OF = 0x800;        // Capture/Compare 3 overcapture flag
    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/compare 2 overcapture flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_B2IF = 0x100;         // Break 2 interrupt flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC4IF = 0x10;         // Capture/Compare 4 interrupt flag
    static constexpr uint32_t SR_CC3IF = 0x8;          // Capture/Compare 3 interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/Compare 2 interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_B2G = 0x100;          // Break 2 generation
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
    static constexpr uint32_t CCMR1_IC2PSC =              // Input capture 2 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_ICPCS =               // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
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

    static constexpr uint32_t CCER_CC6P = 0x200000;      // Capture/Compare 6 output polarity
    static constexpr uint32_t CCER_CC6E = 0x100000;      // Capture/Compare 6 output enable
    static constexpr uint32_t CCER_CC5P = 0x20000;       // Capture/Compare 5 output polarity
    static constexpr uint32_t CCER_CC5E = 0x10000;       // Capture/Compare 5 output enable
    static constexpr uint32_t CCER_CC4NP = 0x8000;       // Capture/Compare 4 complementary output polarity
    static constexpr uint32_t CCER_CC4NE = 0x4000;       // Capture/Compare 4 complementary output enable
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

    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIFCPY, Read-only
    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // counter value (16 bits), Read-write
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0xffff;

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

    static constexpr uint32_t BDTR_BK2ID = 0x20000000;   // BK2ID
    static constexpr uint32_t BDTR_BKBID = 0x10000000;   // BKBID
    static constexpr uint32_t BDTR_BK2DSRM = 0x8000000;  // BK2DSRM
    static constexpr uint32_t BDTR_BKDSRM = 0x4000000;   // BKDSRM
    static constexpr uint32_t BDTR_BK2P = 0x2000000;     // Break 2 polarity
    static constexpr uint32_t BDTR_BK2E = 0x1000000;     // Break 2 Enable
    template<uint32_t X>
    static constexpr uint32_t BDTR_BK2F =                // Break 2 filter (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_BKF =                 // Break filter (4 bits)
        bit_field_t<16, 0xf>::value<X>();
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

    static constexpr uint32_t CCMR3_Output_OC6M_bit3 = 0x1000000;// Output Compare 6 mode bit 3
    template<uint32_t X>
    static constexpr uint32_t CCMR3_Output_OC5M_bit3 =           // Output Compare 5 mode bit 3 (3 bits)
        bit_field_t<16, 0x7>::value<X>();
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

    static constexpr uint32_t DTR2_DTPE = 0x20000;       // Deadtime Preload Enable
    static constexpr uint32_t DTR2_DTAE = 0x10000;       // Deadtime Asymmetric Enable
    template<uint32_t X>
    static constexpr uint32_t DTR2_DTGF =                // Dead-time falling edge generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t DTR2_RESET_VALUE = 0x0;

    static constexpr uint32_t ECR_IE = 0x1;             // Index Enable
    template<uint32_t X>
    static constexpr uint32_t ECR_IDIR =                // Index Direction (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_IBLK =                // Index Blanking (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t ECR_FIDX = 0x20;          // First Index
    template<uint32_t X>
    static constexpr uint32_t ECR_IPOS =                // Index Positioning (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_PW =                  // Pulse width (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_PWPRSC =              // Pulse Width prescaler (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    static const uint32_t ECR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // TI1[0] to TI1[15] input selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI2SEL =              // TI2[0] to TI2[15] input selection (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI3SEL =              // TI3[0] to TI3[15] input selection (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI4SEL =              // TI4[0] to TI4[15] input selection (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF1_ETRSEL =              // ETR source selection (4 bits)
        bit_field_t<14, 0xf>::value<X>();
    static constexpr uint32_t AF1_BKCMP4P = 0x2000;     // BRK COMP4 input polarity
    static constexpr uint32_t AF1_BKCMP3P = 0x1000;     // BRK COMP3 input polarity
    static constexpr uint32_t AF1_BKCMP2P = 0x800;      // BRK COMP2 input polarity
    static constexpr uint32_t AF1_BKCMP1P = 0x400;      // BRK COMP1 input polarity
    static constexpr uint32_t AF1_BKINP = 0x200;        // BRK BKIN input polarity
    static constexpr uint32_t AF1_BKCMP7E = 0x80;       // BRK COMP7 enable
    static constexpr uint32_t AF1_BKCMP6E = 0x40;       // BRK COMP6 enable
    static constexpr uint32_t AF1_BKCMP5E = 0x20;       // BRK COMP5 enable
    static constexpr uint32_t AF1_BKCMP4E = 0x10;       // BRK COMP4 enable
    static constexpr uint32_t AF1_BKCMP3E = 0x8;        // BRK COMP3 enable
    static constexpr uint32_t AF1_BKCMP2E = 0x4;        // BRK COMP2 enable
    static constexpr uint32_t AF1_BKCMP1E = 0x2;        // BRK COMP1 enable
    static constexpr uint32_t AF1_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF2_OCRSEL =              // OCREF_CLR source selection (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static constexpr uint32_t AF2_BK2CMP4P = 0x2000;    // BRK2 COMP4 input polarity
    static constexpr uint32_t AF2_BK2CMP3P = 0x1000;    // BRK2 COMP3 input polarity
    static constexpr uint32_t AF2_BK2CMP2P = 0x800;     // BRK2 COMP2 input polarity
    static constexpr uint32_t AF2_BK2CMP1P = 0x400;     // BRK2 COMP1 input polarity
    static constexpr uint32_t AF2_BK2INP = 0x200;       // BRK2 BKIN input polarity
    static constexpr uint32_t AF2_BK2CMP7E = 0x80;      // BRK2 COMP7 enable
    static constexpr uint32_t AF2_BK2CMP6E = 0x40;      // BRK2 COMP6 enable
    static constexpr uint32_t AF2_BK2CMP5E = 0x20;      // BRK2 COMP5 enable
    static constexpr uint32_t AF2_BK2CMP4E = 0x10;      // BRK2 COMP4 enable
    static constexpr uint32_t AF2_BK2CMP3E = 0x8;       // BRK2 COMP3 enable
    static constexpr uint32_t AF2_BK2CMP2E = 0x4;       // BRK2 COMP2 enable
    static constexpr uint32_t AF2_BK2CMP1E = 0x2;       // BRK2 COMP1 enable
    static constexpr uint32_t AF2_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;


    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM8_BRK = 43; // TIM8_BRK
    static constexpr uint8_t TIM8_TRG_COM = 45; // TIM8_TRG_COM
    static constexpr uint8_t TIM8_UP = 44; // TIM8_UP
};

static tim8_t& TIM8 = *reinterpret_cast<tim8_t*>(0x40013400);

#define HAVE_PERIPHERAL_TIM8


////
//
//    Advanced-timers
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
    volatile uint32_t    RCR;                  // [Read-write] repetition counter register
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    volatile uint32_t    CCR2;                 // [Read-write] capture/compare register 2
    volatile uint32_t    CCR3;                 // [Read-write] capture/compare register 3
    volatile uint32_t    CCR4;                 // [Read-write] capture/compare register 4
    volatile uint32_t    BDTR;                 // [Read-write] break and dead-time register
    volatile uint32_t    CCR5;                 // [Read-write] capture/compare register 4
    volatile uint32_t    CCR6;                 // [Read-write] capture/compare register 4
    volatile uint32_t    CCMR3_Output;         // [Read-write] capture/compare mode register 2 (output mode)
    volatile uint32_t    DTR2;                 // [Read-write] timer Deadtime Register 2
    volatile uint32_t    ECR;                  // [Read-write] DMA control register
    volatile uint32_t    TISEL;                // [Read-write] TIM timer input selection register
    volatile uint32_t    AF1;                  // [Read-write] TIM alternate function option register 1
    volatile uint32_t    AF2;                  // [Read-write] TIM alternate function option register 2
    reserved_t<221>      _0;
    volatile uint32_t    DCR;                  // [Read-write] control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static constexpr uint32_t CR1_DITHEN = 0x1000;      // Dithering Enable
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

    static constexpr uint32_t CR2_MMS_3 = 0x2000000;    // Master mode selection - bit 3
    template<uint32_t X>
    static constexpr uint32_t CR2_MMS2 =                // Master mode selection 2 (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    static constexpr uint32_t CR2_OIS6 = 0x40000;       // Output Idle state 6 (OC6 output)
    static constexpr uint32_t CR2_OIS5 = 0x10000;       // Output Idle state 5 (OC5 output)
    static constexpr uint32_t CR2_OIS4N = 0x8000;       // Output Idle state 4 (OC4N output)
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

    static constexpr uint32_t SMCR_SMSPS = 0x2000000;    // SMS Preload Source
    static constexpr uint32_t SMCR_SMSPE = 0x1000000;    // SMS Preload Enable
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS_4_3 =              // Trigger selection - bit 4:3 (2 bits)
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

    static constexpr uint32_t DIER_TERRIE = 0x800000;    // Transition Error interrupt enable
    static constexpr uint32_t DIER_IERRIE = 0x400000;    // Index Error interrupt enable
    static constexpr uint32_t DIER_DIRIE = 0x200000;     // Direction Change interrupt enable
    static constexpr uint32_t DIER_IDXIE = 0x100000;     // Index interrupt enable
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

    static constexpr uint32_t SR_TERRF = 0x800000;     // Transition Error interrupt flag
    static constexpr uint32_t SR_IERRF = 0x400000;     // Index Error interrupt flag
    static constexpr uint32_t SR_DIRF = 0x200000;      // Direction Change interrupt flag
    static constexpr uint32_t SR_IDXF = 0x100000;      // Index interrupt flag
    static constexpr uint32_t SR_CC6IF = 0x20000;      // Compare 6 interrupt flag
    static constexpr uint32_t SR_CC5IF = 0x10000;      // Compare 5 interrupt flag
    static constexpr uint32_t SR_SBIF = 0x2000;        // System Break interrupt flag
    static constexpr uint32_t SR_CC4OF = 0x1000;       // Capture/Compare 4 overcapture flag
    static constexpr uint32_t SR_CC3OF = 0x800;        // Capture/Compare 3 overcapture flag
    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/compare 2 overcapture flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_B2IF = 0x100;         // Break 2 interrupt flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC4IF = 0x10;         // Capture/Compare 4 interrupt flag
    static constexpr uint32_t SR_CC3IF = 0x8;          // Capture/Compare 3 interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/Compare 2 interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_B2G = 0x100;          // Break 2 generation
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
    static constexpr uint32_t CCMR1_IC2PSC =              // Input capture 2 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_ICPCS =               // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
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

    static constexpr uint32_t CCER_CC6P = 0x200000;      // Capture/Compare 6 output polarity
    static constexpr uint32_t CCER_CC6E = 0x100000;      // Capture/Compare 6 output enable
    static constexpr uint32_t CCER_CC5P = 0x20000;       // Capture/Compare 5 output polarity
    static constexpr uint32_t CCER_CC5E = 0x10000;       // Capture/Compare 5 output enable
    static constexpr uint32_t CCER_CC4NP = 0x8000;       // Capture/Compare 4 complementary output polarity
    static constexpr uint32_t CCER_CC4NE = 0x4000;       // Capture/Compare 4 complementary output enable
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

    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIFCPY
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
    static const uint32_t ARR_RESET_VALUE = 0xffffffff;

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

    static constexpr uint32_t BDTR_BK2ID = 0x20000000;   // BK2ID
    static constexpr uint32_t BDTR_BKBID = 0x10000000;   // BKBID
    static constexpr uint32_t BDTR_BK2DSRM = 0x8000000;  // BK2DSRM
    static constexpr uint32_t BDTR_BKDSRM = 0x4000000;   // BKDSRM
    static constexpr uint32_t BDTR_BK2P = 0x2000000;     // Break 2 polarity
    static constexpr uint32_t BDTR_BK2E = 0x1000000;     // Break 2 Enable
    template<uint32_t X>
    static constexpr uint32_t BDTR_BK2F =                // Break 2 filter (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_BKF =                 // Break filter (4 bits)
        bit_field_t<16, 0xf>::value<X>();
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

    static constexpr uint32_t CCMR3_Output_OC6M_bit3 = 0x1000000;// Output Compare 6 mode bit 3
    template<uint32_t X>
    static constexpr uint32_t CCMR3_Output_OC5M_bit3 =           // Output Compare 5 mode bit 3 (3 bits)
        bit_field_t<16, 0x7>::value<X>();
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

    static constexpr uint32_t DTR2_DTPE = 0x20000;       // Deadtime Preload Enable
    static constexpr uint32_t DTR2_DTAE = 0x10000;       // Deadtime Asymmetric Enable
    template<uint32_t X>
    static constexpr uint32_t DTR2_DTGF =                // Dead-time falling edge generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t DTR2_RESET_VALUE = 0x0;

    static constexpr uint32_t ECR_IE = 0x1;             // Index Enable
    template<uint32_t X>
    static constexpr uint32_t ECR_IDIR =                // Index Direction (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_IBLK =                // Index Blanking (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t ECR_FIDX = 0x20;          // First Index
    template<uint32_t X>
    static constexpr uint32_t ECR_IPOS =                // Index Positioning (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_PW =                  // Pulse width (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_PWPRSC =              // Pulse Width prescaler (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    static const uint32_t ECR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // TI1[0] to TI1[15] input selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI2SEL =              // TI2[0] to TI2[15] input selection (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI3SEL =              // TI3[0] to TI3[15] input selection (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI4SEL =              // TI4[0] to TI4[15] input selection (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF1_ETRSEL =              // ETR source selection (4 bits)
        bit_field_t<14, 0xf>::value<X>();
    static constexpr uint32_t AF1_BKCMP4P = 0x2000;     // BRK COMP4 input polarity
    static constexpr uint32_t AF1_BKCMP3P = 0x1000;     // BRK COMP3 input polarity
    static constexpr uint32_t AF1_BKCMP2P = 0x800;      // BRK COMP2 input polarity
    static constexpr uint32_t AF1_BKCMP1P = 0x400;      // BRK COMP1 input polarity
    static constexpr uint32_t AF1_BKINP = 0x200;        // BRK BKIN input polarity
    static constexpr uint32_t AF1_BKCMP7E = 0x80;       // BRK COMP7 enable
    static constexpr uint32_t AF1_BKCMP6E = 0x40;       // BRK COMP6 enable
    static constexpr uint32_t AF1_BKCMP5E = 0x20;       // BRK COMP5 enable
    static constexpr uint32_t AF1_BKCMP4E = 0x10;       // BRK COMP4 enable
    static constexpr uint32_t AF1_BKCMP3E = 0x8;        // BRK COMP3 enable
    static constexpr uint32_t AF1_BKCMP2E = 0x4;        // BRK COMP2 enable
    static constexpr uint32_t AF1_BKCMP1E = 0x2;        // BRK COMP1 enable
    static constexpr uint32_t AF1_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF2_OCRSEL =              // OCREF_CLR source selection (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static constexpr uint32_t AF2_BK2CMP4P = 0x2000;    // BRK2 COMP4 input polarity
    static constexpr uint32_t AF2_BK2CMP3P = 0x1000;    // BRK2 COMP3 input polarity
    static constexpr uint32_t AF2_BK2CMP2P = 0x800;     // BRK2 COMP2 input polarity
    static constexpr uint32_t AF2_BK2CMP1P = 0x400;     // BRK2 COMP1 input polarity
    static constexpr uint32_t AF2_BK2INP = 0x200;       // BRK2 BKIN input polarity
    static constexpr uint32_t AF2_BK2CMP7E = 0x80;      // BRK2 COMP7 enable
    static constexpr uint32_t AF2_BK2CMP6E = 0x40;      // BRK2 COMP6 enable
    static constexpr uint32_t AF2_BK2CMP5E = 0x20;      // BRK2 COMP5 enable
    static constexpr uint32_t AF2_BK2CMP4E = 0x10;      // BRK2 COMP4 enable
    static constexpr uint32_t AF2_BK2CMP3E = 0x8;       // BRK2 COMP3 enable
    static constexpr uint32_t AF2_BK2CMP2E = 0x4;       // BRK2 COMP2 enable
    static constexpr uint32_t AF2_BK2CMP1E = 0x2;       // BRK2 COMP1 enable
    static constexpr uint32_t AF2_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;


    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM2 = 28; // TIM2
};

static tim2_t& TIM2 = *reinterpret_cast<tim2_t*>(0x40000000);

#define HAVE_PERIPHERAL_TIM2


////
//
//    Advanced-timers
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
    volatile uint32_t    RCR;                  // [Read-write] repetition counter register
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    volatile uint32_t    CCR2;                 // [Read-write] capture/compare register 2
    volatile uint32_t    CCR3;                 // [Read-write] capture/compare register 3
    volatile uint32_t    CCR4;                 // [Read-write] capture/compare register 4
    volatile uint32_t    BDTR;                 // [Read-write] break and dead-time register
    volatile uint32_t    CCR5;                 // [Read-write] capture/compare register 4
    volatile uint32_t    CCR6;                 // [Read-write] capture/compare register 4
    volatile uint32_t    CCMR3_Output;         // [Read-write] capture/compare mode register 2 (output mode)
    volatile uint32_t    DTR2;                 // [Read-write] timer Deadtime Register 2
    volatile uint32_t    ECR;                  // [Read-write] DMA control register
    volatile uint32_t    TISEL;                // [Read-write] TIM timer input selection register
    volatile uint32_t    AF1;                  // [Read-write] TIM alternate function option register 1
    volatile uint32_t    AF2;                  // [Read-write] TIM alternate function option register 2
    reserved_t<221>      _0;
    volatile uint32_t    DCR;                  // [Read-write] control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static constexpr uint32_t CR1_DITHEN = 0x1000;      // Dithering Enable
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

    static constexpr uint32_t CR2_MMS_3 = 0x2000000;    // Master mode selection - bit 3
    template<uint32_t X>
    static constexpr uint32_t CR2_MMS2 =                // Master mode selection 2 (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    static constexpr uint32_t CR2_OIS6 = 0x40000;       // Output Idle state 6 (OC6 output)
    static constexpr uint32_t CR2_OIS5 = 0x10000;       // Output Idle state 5 (OC5 output)
    static constexpr uint32_t CR2_OIS4N = 0x8000;       // Output Idle state 4 (OC4N output)
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

    static constexpr uint32_t SMCR_SMSPS = 0x2000000;    // SMS Preload Source
    static constexpr uint32_t SMCR_SMSPE = 0x1000000;    // SMS Preload Enable
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS_4_3 =              // Trigger selection - bit 4:3 (2 bits)
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

    static constexpr uint32_t DIER_TERRIE = 0x800000;    // Transition Error interrupt enable
    static constexpr uint32_t DIER_IERRIE = 0x400000;    // Index Error interrupt enable
    static constexpr uint32_t DIER_DIRIE = 0x200000;     // Direction Change interrupt enable
    static constexpr uint32_t DIER_IDXIE = 0x100000;     // Index interrupt enable
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

    static constexpr uint32_t SR_TERRF = 0x800000;     // Transition Error interrupt flag
    static constexpr uint32_t SR_IERRF = 0x400000;     // Index Error interrupt flag
    static constexpr uint32_t SR_DIRF = 0x200000;      // Direction Change interrupt flag
    static constexpr uint32_t SR_IDXF = 0x100000;      // Index interrupt flag
    static constexpr uint32_t SR_CC6IF = 0x20000;      // Compare 6 interrupt flag
    static constexpr uint32_t SR_CC5IF = 0x10000;      // Compare 5 interrupt flag
    static constexpr uint32_t SR_SBIF = 0x2000;        // System Break interrupt flag
    static constexpr uint32_t SR_CC4OF = 0x1000;       // Capture/Compare 4 overcapture flag
    static constexpr uint32_t SR_CC3OF = 0x800;        // Capture/Compare 3 overcapture flag
    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/compare 2 overcapture flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_B2IF = 0x100;         // Break 2 interrupt flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC4IF = 0x10;         // Capture/Compare 4 interrupt flag
    static constexpr uint32_t SR_CC3IF = 0x8;          // Capture/Compare 3 interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/Compare 2 interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_B2G = 0x100;          // Break 2 generation
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
    static constexpr uint32_t CCMR1_IC2PSC =              // Input capture 2 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_ICPCS =               // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
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

    static constexpr uint32_t CCER_CC6P = 0x200000;      // Capture/Compare 6 output polarity
    static constexpr uint32_t CCER_CC6E = 0x100000;      // Capture/Compare 6 output enable
    static constexpr uint32_t CCER_CC5P = 0x20000;       // Capture/Compare 5 output polarity
    static constexpr uint32_t CCER_CC5E = 0x10000;       // Capture/Compare 5 output enable
    static constexpr uint32_t CCER_CC4NP = 0x8000;       // Capture/Compare 4 complementary output polarity
    static constexpr uint32_t CCER_CC4NE = 0x4000;       // Capture/Compare 4 complementary output enable
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

    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIFCPY
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
    static const uint32_t ARR_RESET_VALUE = 0xffffffff;

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

    static constexpr uint32_t BDTR_BK2ID = 0x20000000;   // BK2ID
    static constexpr uint32_t BDTR_BKBID = 0x10000000;   // BKBID
    static constexpr uint32_t BDTR_BK2DSRM = 0x8000000;  // BK2DSRM
    static constexpr uint32_t BDTR_BKDSRM = 0x4000000;   // BKDSRM
    static constexpr uint32_t BDTR_BK2P = 0x2000000;     // Break 2 polarity
    static constexpr uint32_t BDTR_BK2E = 0x1000000;     // Break 2 Enable
    template<uint32_t X>
    static constexpr uint32_t BDTR_BK2F =                // Break 2 filter (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_BKF =                 // Break filter (4 bits)
        bit_field_t<16, 0xf>::value<X>();
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

    static constexpr uint32_t CCMR3_Output_OC6M_bit3 = 0x1000000;// Output Compare 6 mode bit 3
    template<uint32_t X>
    static constexpr uint32_t CCMR3_Output_OC5M_bit3 =           // Output Compare 5 mode bit 3 (3 bits)
        bit_field_t<16, 0x7>::value<X>();
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

    static constexpr uint32_t DTR2_DTPE = 0x20000;       // Deadtime Preload Enable
    static constexpr uint32_t DTR2_DTAE = 0x10000;       // Deadtime Asymmetric Enable
    template<uint32_t X>
    static constexpr uint32_t DTR2_DTGF =                // Dead-time falling edge generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t DTR2_RESET_VALUE = 0x0;

    static constexpr uint32_t ECR_IE = 0x1;             // Index Enable
    template<uint32_t X>
    static constexpr uint32_t ECR_IDIR =                // Index Direction (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_IBLK =                // Index Blanking (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t ECR_FIDX = 0x20;          // First Index
    template<uint32_t X>
    static constexpr uint32_t ECR_IPOS =                // Index Positioning (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_PW =                  // Pulse width (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_PWPRSC =              // Pulse Width prescaler (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    static const uint32_t ECR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // TI1[0] to TI1[15] input selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI2SEL =              // TI2[0] to TI2[15] input selection (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI3SEL =              // TI3[0] to TI3[15] input selection (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI4SEL =              // TI4[0] to TI4[15] input selection (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF1_ETRSEL =              // ETR source selection (4 bits)
        bit_field_t<14, 0xf>::value<X>();
    static constexpr uint32_t AF1_BKCMP4P = 0x2000;     // BRK COMP4 input polarity
    static constexpr uint32_t AF1_BKCMP3P = 0x1000;     // BRK COMP3 input polarity
    static constexpr uint32_t AF1_BKCMP2P = 0x800;      // BRK COMP2 input polarity
    static constexpr uint32_t AF1_BKCMP1P = 0x400;      // BRK COMP1 input polarity
    static constexpr uint32_t AF1_BKINP = 0x200;        // BRK BKIN input polarity
    static constexpr uint32_t AF1_BKCMP7E = 0x80;       // BRK COMP7 enable
    static constexpr uint32_t AF1_BKCMP6E = 0x40;       // BRK COMP6 enable
    static constexpr uint32_t AF1_BKCMP5E = 0x20;       // BRK COMP5 enable
    static constexpr uint32_t AF1_BKCMP4E = 0x10;       // BRK COMP4 enable
    static constexpr uint32_t AF1_BKCMP3E = 0x8;        // BRK COMP3 enable
    static constexpr uint32_t AF1_BKCMP2E = 0x4;        // BRK COMP2 enable
    static constexpr uint32_t AF1_BKCMP1E = 0x2;        // BRK COMP1 enable
    static constexpr uint32_t AF1_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF2_OCRSEL =              // OCREF_CLR source selection (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static constexpr uint32_t AF2_BK2CMP4P = 0x2000;    // BRK2 COMP4 input polarity
    static constexpr uint32_t AF2_BK2CMP3P = 0x1000;    // BRK2 COMP3 input polarity
    static constexpr uint32_t AF2_BK2CMP2P = 0x800;     // BRK2 COMP2 input polarity
    static constexpr uint32_t AF2_BK2CMP1P = 0x400;     // BRK2 COMP1 input polarity
    static constexpr uint32_t AF2_BK2INP = 0x200;       // BRK2 BKIN input polarity
    static constexpr uint32_t AF2_BK2CMP7E = 0x80;      // BRK2 COMP7 enable
    static constexpr uint32_t AF2_BK2CMP6E = 0x40;      // BRK2 COMP6 enable
    static constexpr uint32_t AF2_BK2CMP5E = 0x20;      // BRK2 COMP5 enable
    static constexpr uint32_t AF2_BK2CMP4E = 0x10;      // BRK2 COMP4 enable
    static constexpr uint32_t AF2_BK2CMP3E = 0x8;       // BRK2 COMP3 enable
    static constexpr uint32_t AF2_BK2CMP2E = 0x4;       // BRK2 COMP2 enable
    static constexpr uint32_t AF2_BK2CMP1E = 0x2;       // BRK2 COMP1 enable
    static constexpr uint32_t AF2_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;


    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM3 = 29; // TIM3
};

static tim3_t& TIM3 = *reinterpret_cast<tim3_t*>(0x40000400);

#define HAVE_PERIPHERAL_TIM3


////
//
//    Advanced-timers
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
    volatile uint32_t    RCR;                  // [Read-write] repetition counter register
    volatile uint32_t    CCR1;                 // [Read-write] capture/compare register 1
    volatile uint32_t    CCR2;                 // [Read-write] capture/compare register 2
    volatile uint32_t    CCR3;                 // [Read-write] capture/compare register 3
    volatile uint32_t    CCR4;                 // [Read-write] capture/compare register 4
    volatile uint32_t    BDTR;                 // [Read-write] break and dead-time register
    volatile uint32_t    CCR5;                 // [Read-write] capture/compare register 4
    volatile uint32_t    CCR6;                 // [Read-write] capture/compare register 4
    volatile uint32_t    CCMR3_Output;         // [Read-write] capture/compare mode register 2 (output mode)
    volatile uint32_t    DTR2;                 // [Read-write] timer Deadtime Register 2
    volatile uint32_t    ECR;                  // [Read-write] DMA control register
    volatile uint32_t    TISEL;                // [Read-write] TIM timer input selection register
    volatile uint32_t    AF1;                  // [Read-write] TIM alternate function option register 1
    volatile uint32_t    AF2;                  // [Read-write] TIM alternate function option register 2
    reserved_t<221>      _0;
    volatile uint32_t    DCR;                  // [Read-write] control register
    volatile uint32_t    DMAR;                 // [Read-write] DMA address for full transfer

    static constexpr uint32_t CR1_DITHEN = 0x1000;      // Dithering Enable
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

    static constexpr uint32_t CR2_MMS_3 = 0x2000000;    // Master mode selection - bit 3
    template<uint32_t X>
    static constexpr uint32_t CR2_MMS2 =                // Master mode selection 2 (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    static constexpr uint32_t CR2_OIS6 = 0x40000;       // Output Idle state 6 (OC6 output)
    static constexpr uint32_t CR2_OIS5 = 0x10000;       // Output Idle state 5 (OC5 output)
    static constexpr uint32_t CR2_OIS4N = 0x8000;       // Output Idle state 4 (OC4N output)
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

    static constexpr uint32_t SMCR_SMSPS = 0x2000000;    // SMS Preload Source
    static constexpr uint32_t SMCR_SMSPE = 0x1000000;    // SMS Preload Enable
    template<uint32_t X>
    static constexpr uint32_t SMCR_TS_4_3 =              // Trigger selection - bit 4:3 (2 bits)
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

    static constexpr uint32_t DIER_TERRIE = 0x800000;    // Transition Error interrupt enable
    static constexpr uint32_t DIER_IERRIE = 0x400000;    // Index Error interrupt enable
    static constexpr uint32_t DIER_DIRIE = 0x200000;     // Direction Change interrupt enable
    static constexpr uint32_t DIER_IDXIE = 0x100000;     // Index interrupt enable
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

    static constexpr uint32_t SR_TERRF = 0x800000;     // Transition Error interrupt flag
    static constexpr uint32_t SR_IERRF = 0x400000;     // Index Error interrupt flag
    static constexpr uint32_t SR_DIRF = 0x200000;      // Direction Change interrupt flag
    static constexpr uint32_t SR_IDXF = 0x100000;      // Index interrupt flag
    static constexpr uint32_t SR_CC6IF = 0x20000;      // Compare 6 interrupt flag
    static constexpr uint32_t SR_CC5IF = 0x10000;      // Compare 5 interrupt flag
    static constexpr uint32_t SR_SBIF = 0x2000;        // System Break interrupt flag
    static constexpr uint32_t SR_CC4OF = 0x1000;       // Capture/Compare 4 overcapture flag
    static constexpr uint32_t SR_CC3OF = 0x800;        // Capture/Compare 3 overcapture flag
    static constexpr uint32_t SR_CC2OF = 0x400;        // Capture/compare 2 overcapture flag
    static constexpr uint32_t SR_CC1OF = 0x200;        // Capture/Compare 1 overcapture flag
    static constexpr uint32_t SR_B2IF = 0x100;         // Break 2 interrupt flag
    static constexpr uint32_t SR_BIF = 0x80;           // Break interrupt flag
    static constexpr uint32_t SR_TIF = 0x40;           // Trigger interrupt flag
    static constexpr uint32_t SR_COMIF = 0x20;         // COM interrupt flag
    static constexpr uint32_t SR_CC4IF = 0x10;         // Capture/Compare 4 interrupt flag
    static constexpr uint32_t SR_CC3IF = 0x8;          // Capture/Compare 3 interrupt flag
    static constexpr uint32_t SR_CC2IF = 0x4;          // Capture/Compare 2 interrupt flag
    static constexpr uint32_t SR_CC1IF = 0x2;          // Capture/compare 1 interrupt flag
    static constexpr uint32_t SR_UIF = 0x1;            // Update interrupt flag
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t EGR_B2G = 0x100;          // Break 2 generation
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
    static constexpr uint32_t CCMR1_IC2PSC =              // Input capture 2 prescaler (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCMR1_ICPCS =               // Input capture 1 prescaler (2 bits)
        bit_field_t<2, 0x3>::value<X>();
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

    static constexpr uint32_t CCER_CC6P = 0x200000;      // Capture/Compare 6 output polarity
    static constexpr uint32_t CCER_CC6E = 0x100000;      // Capture/Compare 6 output enable
    static constexpr uint32_t CCER_CC5P = 0x20000;       // Capture/Compare 5 output polarity
    static constexpr uint32_t CCER_CC5E = 0x10000;       // Capture/Compare 5 output enable
    static constexpr uint32_t CCER_CC4NP = 0x8000;       // Capture/Compare 4 complementary output polarity
    static constexpr uint32_t CCER_CC4NE = 0x4000;       // Capture/Compare 4 complementary output enable
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

    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIFCPY
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
    static const uint32_t ARR_RESET_VALUE = 0xffffffff;

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

    static constexpr uint32_t BDTR_BK2ID = 0x20000000;   // BK2ID
    static constexpr uint32_t BDTR_BKBID = 0x10000000;   // BKBID
    static constexpr uint32_t BDTR_BK2DSRM = 0x8000000;  // BK2DSRM
    static constexpr uint32_t BDTR_BKDSRM = 0x4000000;   // BKDSRM
    static constexpr uint32_t BDTR_BK2P = 0x2000000;     // Break 2 polarity
    static constexpr uint32_t BDTR_BK2E = 0x1000000;     // Break 2 Enable
    template<uint32_t X>
    static constexpr uint32_t BDTR_BK2F =                // Break 2 filter (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BDTR_BKF =                 // Break filter (4 bits)
        bit_field_t<16, 0xf>::value<X>();
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

    static constexpr uint32_t CCMR3_Output_OC6M_bit3 = 0x1000000;// Output Compare 6 mode bit 3
    template<uint32_t X>
    static constexpr uint32_t CCMR3_Output_OC5M_bit3 =           // Output Compare 5 mode bit 3 (3 bits)
        bit_field_t<16, 0x7>::value<X>();
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

    static constexpr uint32_t DTR2_DTPE = 0x20000;       // Deadtime Preload Enable
    static constexpr uint32_t DTR2_DTAE = 0x10000;       // Deadtime Asymmetric Enable
    template<uint32_t X>
    static constexpr uint32_t DTR2_DTGF =                // Dead-time falling edge generator setup (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t DTR2_RESET_VALUE = 0x0;

    static constexpr uint32_t ECR_IE = 0x1;             // Index Enable
    template<uint32_t X>
    static constexpr uint32_t ECR_IDIR =                // Index Direction (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_IBLK =                // Index Blanking (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t ECR_FIDX = 0x20;          // First Index
    template<uint32_t X>
    static constexpr uint32_t ECR_IPOS =                // Index Positioning (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_PW =                  // Pulse width (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_PWPRSC =              // Pulse Width prescaler (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    static const uint32_t ECR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TISEL_TI1SEL =              // TI1[0] to TI1[15] input selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI2SEL =              // TI2[0] to TI2[15] input selection (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI3SEL =              // TI3[0] to TI3[15] input selection (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TISEL_TI4SEL =              // TI4[0] to TI4[15] input selection (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static const uint32_t TISEL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF1_ETRSEL =              // ETR source selection (4 bits)
        bit_field_t<14, 0xf>::value<X>();
    static constexpr uint32_t AF1_BKCMP4P = 0x2000;     // BRK COMP4 input polarity
    static constexpr uint32_t AF1_BKCMP3P = 0x1000;     // BRK COMP3 input polarity
    static constexpr uint32_t AF1_BKCMP2P = 0x800;      // BRK COMP2 input polarity
    static constexpr uint32_t AF1_BKCMP1P = 0x400;      // BRK COMP1 input polarity
    static constexpr uint32_t AF1_BKINP = 0x200;        // BRK BKIN input polarity
    static constexpr uint32_t AF1_BKCMP7E = 0x80;       // BRK COMP7 enable
    static constexpr uint32_t AF1_BKCMP6E = 0x40;       // BRK COMP6 enable
    static constexpr uint32_t AF1_BKCMP5E = 0x20;       // BRK COMP5 enable
    static constexpr uint32_t AF1_BKCMP4E = 0x10;       // BRK COMP4 enable
    static constexpr uint32_t AF1_BKCMP3E = 0x8;        // BRK COMP3 enable
    static constexpr uint32_t AF1_BKCMP2E = 0x4;        // BRK COMP2 enable
    static constexpr uint32_t AF1_BKCMP1E = 0x2;        // BRK COMP1 enable
    static constexpr uint32_t AF1_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AF2_OCRSEL =              // OCREF_CLR source selection (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static constexpr uint32_t AF2_BK2CMP4P = 0x2000;    // BRK2 COMP4 input polarity
    static constexpr uint32_t AF2_BK2CMP3P = 0x1000;    // BRK2 COMP3 input polarity
    static constexpr uint32_t AF2_BK2CMP2P = 0x800;     // BRK2 COMP2 input polarity
    static constexpr uint32_t AF2_BK2CMP1P = 0x400;     // BRK2 COMP1 input polarity
    static constexpr uint32_t AF2_BK2INP = 0x200;       // BRK2 BKIN input polarity
    static constexpr uint32_t AF2_BK2CMP7E = 0x80;      // BRK2 COMP7 enable
    static constexpr uint32_t AF2_BK2CMP6E = 0x40;      // BRK2 COMP6 enable
    static constexpr uint32_t AF2_BK2CMP5E = 0x20;      // BRK2 COMP5 enable
    static constexpr uint32_t AF2_BK2CMP4E = 0x10;      // BRK2 COMP4 enable
    static constexpr uint32_t AF2_BK2CMP3E = 0x8;       // BRK2 COMP3 enable
    static constexpr uint32_t AF2_BK2CMP2E = 0x4;       // BRK2 COMP2 enable
    static constexpr uint32_t AF2_BK2CMP1E = 0x2;       // BRK2 COMP1 enable
    static constexpr uint32_t AF2_BKINE = 0x1;          // BRK BKIN input enable
    static const uint32_t AF2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DCR_DBL =                 // DMA burst length (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DCR_DBA =                 // DMA base address (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t DCR_RESET_VALUE = 0x0;


    static const uint32_t DMAR_RESET_VALUE = 0x0;

    static constexpr uint8_t TIM4 = 30; // TIM4
};

static tim4_t& TIM4 = *reinterpret_cast<tim4_t*>(0x40000800);

#define HAVE_PERIPHERAL_TIM4


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
    volatile uint32_t    CNT;                  // counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register

    static constexpr uint32_t CR1_DITHEN = 0x1000;      // Dithering Enable
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

    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIF Copy, Read-only
    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // Low counter value (16 bits), Read-write
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Low Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0xffff;

    static constexpr uint8_t TIM6_DACUNDER = 54; // TIM6_DACUNDER
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
    volatile uint32_t    CNT;                  // counter
    volatile uint32_t    PSC;                  // [Read-write] prescaler
    volatile uint32_t    ARR;                  // [Read-write] auto-reload register

    static constexpr uint32_t CR1_DITHEN = 0x1000;      // Dithering Enable
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

    static constexpr uint32_t CNT_UIFCPY = 0x80000000;  // UIF Copy, Read-only
    template<uint32_t X>
    static constexpr uint32_t CNT_CNT =                 // Low counter value (16 bits), Read-write
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSC_PSC =                 // Prescaler value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t PSC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ARR_ARR =                 // Low Auto-reload value (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t ARR_RESET_VALUE = 0xffff;

    static constexpr uint8_t TIM7 = 55; // TIM7
};

static tim7_t& TIM7 = *reinterpret_cast<tim7_t*>(0x40001400);

#define HAVE_PERIPHERAL_TIM7


////
//
//    Low power timer
//
////

struct lptimer1_t
{
    volatile uint32_t    ISR;                  // [Read-only] Interrupt and Status Register
    volatile uint32_t    ICR;                  // [Write-only] Interrupt Clear Register
    volatile uint32_t    IER;                  // [Read-write] Interrupt Enable Register
    volatile uint32_t    CFGR;                 // [Read-write] Configuration Register
    volatile uint32_t    CR;                   // [Read-write] Control Register
    volatile uint32_t    CMP;                  // [Read-write] Compare Register
    volatile uint32_t    ARR;                  // [Read-write] Autoreload Register
    volatile uint32_t    CNT;                  // [Read-only] Counter Register
    volatile uint32_t    OR;                   // [Read-write] option register

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
    static constexpr uint32_t CFGR_TRIGSEL =             // Trigger selector (4 bits)
        bit_field_t<13, 0xf>::value<X>();
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

    static constexpr uint32_t CR_RSTARE = 0x10;        // RSTARE
    static constexpr uint32_t CR_COUNTRST = 0x8;       // COUNTRST
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

    static constexpr uint32_t OR_IN1 = 0x1;            // IN1
    static constexpr uint32_t OR_IN2 = 0x2;            // IN2
    template<uint32_t X>
    static constexpr uint32_t OR_IN1_2_1 =             // IN1_2_1 (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OR_IN2_2_1 =             // IN2_2_1 (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static const uint32_t OR_RESET_VALUE = 0x0;
};

static lptimer1_t& LPTIMER1 = *reinterpret_cast<lptimer1_t*>(0x40007c00);

#define HAVE_PERIPHERAL_LPTIMER1


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
    volatile uint32_t    PRESC;                // [Read-write] USART prescaler register

    static constexpr uint32_t CR1_RXFFIE = 0x80000000;  // RXFFIE
    static constexpr uint32_t CR1_TXFEIE = 0x40000000;  // TXFEIE
    static constexpr uint32_t CR1_FIFOEN = 0x20000000;  // FIFOEN
    static constexpr uint32_t CR1_M1 = 0x10000000;      // M1
    static constexpr uint32_t CR1_EOBIE = 0x8000000;    // End of Block interrupt enable
    static constexpr uint32_t CR1_RTOIE = 0x4000000;    // Receiver timeout interrupt enable
    static constexpr uint32_t CR1_DEAT4 = 0x2000000;    // Driver Enable assertion time
    static constexpr uint32_t CR1_DEAT3 = 0x1000000;    // DEAT3
    static constexpr uint32_t CR1_DEAT2 = 0x800000;     // DEAT2
    static constexpr uint32_t CR1_DEAT1 = 0x400000;     // DEAT1
    static constexpr uint32_t CR1_DEAT0 = 0x200000;     // DEAT0
    static constexpr uint32_t CR1_DEDT4 = 0x100000;     // Driver Enable de-assertion time
    static constexpr uint32_t CR1_DEDT3 = 0x80000;      // DEDT3
    static constexpr uint32_t CR1_DEDT2 = 0x40000;      // DEDT2
    static constexpr uint32_t CR1_DEDT1 = 0x20000;      // DEDT1
    static constexpr uint32_t CR1_DEDT0 = 0x10000;      // DEDT0
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
    static constexpr uint32_t CR2_ABRMOD1 = 0x400000;   // Auto baud rate mode
    static constexpr uint32_t CR2_ABRMOD0 = 0x200000;   // ABRMOD0
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
    static constexpr uint32_t CR2_DIS_NSS = 0x8;        // DIS_NSS
    static constexpr uint32_t CR2_SLVEN = 0x1;          // SLVEN
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR3_TXFTCFG =             // TXFTCFG (3 bits)
        bit_field_t<29, 0x7>::value<X>();
    static constexpr uint32_t CR3_RXFTIE = 0x10000000;  // RXFTIE
    template<uint32_t X>
    static constexpr uint32_t CR3_RXFTCFG =             // RXFTCFG (3 bits)
        bit_field_t<25, 0x7>::value<X>();
    static constexpr uint32_t CR3_TCBGTIE = 0x1000000;  // TCBGTIE
    static constexpr uint32_t CR3_TXFTIE = 0x800000;    // TXFTIE
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
    static constexpr uint32_t BRR_DIV_Mantissa =        // DIV_Mantissa (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // DIV_Fraction (4 bits)
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

    static constexpr uint32_t ISR_TXFT = 0x8000000;     // TXFT
    static constexpr uint32_t ISR_RXFT = 0x4000000;     // RXFT
    static constexpr uint32_t ISR_TCBGT = 0x2000000;    // TCBGT
    static constexpr uint32_t ISR_RXFF = 0x1000000;     // RXFF
    static constexpr uint32_t ISR_TXFE = 0x800000;      // TXFE
    static constexpr uint32_t ISR_REACK = 0x400000;     // REACK
    static constexpr uint32_t ISR_TEACK = 0x200000;     // TEACK
    static constexpr uint32_t ISR_WUF = 0x100000;       // WUF
    static constexpr uint32_t ISR_RWU = 0x80000;        // RWU
    static constexpr uint32_t ISR_SBKF = 0x40000;       // SBKF
    static constexpr uint32_t ISR_CMF = 0x20000;        // CMF
    static constexpr uint32_t ISR_BUSY = 0x10000;       // BUSY
    static constexpr uint32_t ISR_ABRF = 0x8000;        // ABRF
    static constexpr uint32_t ISR_ABRE = 0x4000;        // ABRE
    static constexpr uint32_t ISR_UDR = 0x2000;         // UDR
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
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t ICR_WUCF = 0x100000;      // Wakeup from Stop mode clear flag
    static constexpr uint32_t ICR_CMCF = 0x20000;       // Character match clear flag
    static constexpr uint32_t ICR_UDRCF = 0x2000;       // UDRCF
    static constexpr uint32_t ICR_EOBCF = 0x1000;       // End of block clear flag
    static constexpr uint32_t ICR_RTOCF = 0x800;        // Receiver timeout clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
    static constexpr uint32_t ICR_LBDCF = 0x100;        // LIN break detection clear flag
    static constexpr uint32_t ICR_TCBGTCF = 0x80;       // TCBGTCF
    static constexpr uint32_t ICR_TCCF = 0x40;          // Transmission complete clear flag
    static constexpr uint32_t ICR_TXFECF = 0x20;        // TXFECF
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
    static constexpr uint32_t PRESC_PRESCALER =           // PRESCALER (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESC_RESET_VALUE = 0x0;

    static constexpr uint8_t USART1 = 37; // USART1
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
    volatile uint32_t    PRESC;                // [Read-write] USART prescaler register

    static constexpr uint32_t CR1_RXFFIE = 0x80000000;  // RXFFIE
    static constexpr uint32_t CR1_TXFEIE = 0x40000000;  // TXFEIE
    static constexpr uint32_t CR1_FIFOEN = 0x20000000;  // FIFOEN
    static constexpr uint32_t CR1_M1 = 0x10000000;      // M1
    static constexpr uint32_t CR1_EOBIE = 0x8000000;    // End of Block interrupt enable
    static constexpr uint32_t CR1_RTOIE = 0x4000000;    // Receiver timeout interrupt enable
    static constexpr uint32_t CR1_DEAT4 = 0x2000000;    // Driver Enable assertion time
    static constexpr uint32_t CR1_DEAT3 = 0x1000000;    // DEAT3
    static constexpr uint32_t CR1_DEAT2 = 0x800000;     // DEAT2
    static constexpr uint32_t CR1_DEAT1 = 0x400000;     // DEAT1
    static constexpr uint32_t CR1_DEAT0 = 0x200000;     // DEAT0
    static constexpr uint32_t CR1_DEDT4 = 0x100000;     // Driver Enable de-assertion time
    static constexpr uint32_t CR1_DEDT3 = 0x80000;      // DEDT3
    static constexpr uint32_t CR1_DEDT2 = 0x40000;      // DEDT2
    static constexpr uint32_t CR1_DEDT1 = 0x20000;      // DEDT1
    static constexpr uint32_t CR1_DEDT0 = 0x10000;      // DEDT0
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
    static constexpr uint32_t CR2_ABRMOD1 = 0x400000;   // Auto baud rate mode
    static constexpr uint32_t CR2_ABRMOD0 = 0x200000;   // ABRMOD0
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
    static constexpr uint32_t CR2_DIS_NSS = 0x8;        // DIS_NSS
    static constexpr uint32_t CR2_SLVEN = 0x1;          // SLVEN
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR3_TXFTCFG =             // TXFTCFG (3 bits)
        bit_field_t<29, 0x7>::value<X>();
    static constexpr uint32_t CR3_RXFTIE = 0x10000000;  // RXFTIE
    template<uint32_t X>
    static constexpr uint32_t CR3_RXFTCFG =             // RXFTCFG (3 bits)
        bit_field_t<25, 0x7>::value<X>();
    static constexpr uint32_t CR3_TCBGTIE = 0x1000000;  // TCBGTIE
    static constexpr uint32_t CR3_TXFTIE = 0x800000;    // TXFTIE
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
    static constexpr uint32_t BRR_DIV_Mantissa =        // DIV_Mantissa (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // DIV_Fraction (4 bits)
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

    static constexpr uint32_t ISR_TXFT = 0x8000000;     // TXFT
    static constexpr uint32_t ISR_RXFT = 0x4000000;     // RXFT
    static constexpr uint32_t ISR_TCBGT = 0x2000000;    // TCBGT
    static constexpr uint32_t ISR_RXFF = 0x1000000;     // RXFF
    static constexpr uint32_t ISR_TXFE = 0x800000;      // TXFE
    static constexpr uint32_t ISR_REACK = 0x400000;     // REACK
    static constexpr uint32_t ISR_TEACK = 0x200000;     // TEACK
    static constexpr uint32_t ISR_WUF = 0x100000;       // WUF
    static constexpr uint32_t ISR_RWU = 0x80000;        // RWU
    static constexpr uint32_t ISR_SBKF = 0x40000;       // SBKF
    static constexpr uint32_t ISR_CMF = 0x20000;        // CMF
    static constexpr uint32_t ISR_BUSY = 0x10000;       // BUSY
    static constexpr uint32_t ISR_ABRF = 0x8000;        // ABRF
    static constexpr uint32_t ISR_ABRE = 0x4000;        // ABRE
    static constexpr uint32_t ISR_UDR = 0x2000;         // UDR
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
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t ICR_WUCF = 0x100000;      // Wakeup from Stop mode clear flag
    static constexpr uint32_t ICR_CMCF = 0x20000;       // Character match clear flag
    static constexpr uint32_t ICR_UDRCF = 0x2000;       // UDRCF
    static constexpr uint32_t ICR_EOBCF = 0x1000;       // End of block clear flag
    static constexpr uint32_t ICR_RTOCF = 0x800;        // Receiver timeout clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
    static constexpr uint32_t ICR_LBDCF = 0x100;        // LIN break detection clear flag
    static constexpr uint32_t ICR_TCBGTCF = 0x80;       // TCBGTCF
    static constexpr uint32_t ICR_TCCF = 0x40;          // Transmission complete clear flag
    static constexpr uint32_t ICR_TXFECF = 0x20;        // TXFECF
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
    static constexpr uint32_t PRESC_PRESCALER =           // PRESCALER (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESC_RESET_VALUE = 0x0;

    static constexpr uint8_t USART2 = 38; // USART2
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
    volatile uint32_t    PRESC;                // [Read-write] USART prescaler register

    static constexpr uint32_t CR1_RXFFIE = 0x80000000;  // RXFFIE
    static constexpr uint32_t CR1_TXFEIE = 0x40000000;  // TXFEIE
    static constexpr uint32_t CR1_FIFOEN = 0x20000000;  // FIFOEN
    static constexpr uint32_t CR1_M1 = 0x10000000;      // M1
    static constexpr uint32_t CR1_EOBIE = 0x8000000;    // End of Block interrupt enable
    static constexpr uint32_t CR1_RTOIE = 0x4000000;    // Receiver timeout interrupt enable
    static constexpr uint32_t CR1_DEAT4 = 0x2000000;    // Driver Enable assertion time
    static constexpr uint32_t CR1_DEAT3 = 0x1000000;    // DEAT3
    static constexpr uint32_t CR1_DEAT2 = 0x800000;     // DEAT2
    static constexpr uint32_t CR1_DEAT1 = 0x400000;     // DEAT1
    static constexpr uint32_t CR1_DEAT0 = 0x200000;     // DEAT0
    static constexpr uint32_t CR1_DEDT4 = 0x100000;     // Driver Enable de-assertion time
    static constexpr uint32_t CR1_DEDT3 = 0x80000;      // DEDT3
    static constexpr uint32_t CR1_DEDT2 = 0x40000;      // DEDT2
    static constexpr uint32_t CR1_DEDT1 = 0x20000;      // DEDT1
    static constexpr uint32_t CR1_DEDT0 = 0x10000;      // DEDT0
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
    static constexpr uint32_t CR2_ABRMOD1 = 0x400000;   // Auto baud rate mode
    static constexpr uint32_t CR2_ABRMOD0 = 0x200000;   // ABRMOD0
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
    static constexpr uint32_t CR2_DIS_NSS = 0x8;        // DIS_NSS
    static constexpr uint32_t CR2_SLVEN = 0x1;          // SLVEN
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR3_TXFTCFG =             // TXFTCFG (3 bits)
        bit_field_t<29, 0x7>::value<X>();
    static constexpr uint32_t CR3_RXFTIE = 0x10000000;  // RXFTIE
    template<uint32_t X>
    static constexpr uint32_t CR3_RXFTCFG =             // RXFTCFG (3 bits)
        bit_field_t<25, 0x7>::value<X>();
    static constexpr uint32_t CR3_TCBGTIE = 0x1000000;  // TCBGTIE
    static constexpr uint32_t CR3_TXFTIE = 0x800000;    // TXFTIE
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
    static constexpr uint32_t BRR_DIV_Mantissa =        // DIV_Mantissa (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // DIV_Fraction (4 bits)
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

    static constexpr uint32_t ISR_TXFT = 0x8000000;     // TXFT
    static constexpr uint32_t ISR_RXFT = 0x4000000;     // RXFT
    static constexpr uint32_t ISR_TCBGT = 0x2000000;    // TCBGT
    static constexpr uint32_t ISR_RXFF = 0x1000000;     // RXFF
    static constexpr uint32_t ISR_TXFE = 0x800000;      // TXFE
    static constexpr uint32_t ISR_REACK = 0x400000;     // REACK
    static constexpr uint32_t ISR_TEACK = 0x200000;     // TEACK
    static constexpr uint32_t ISR_WUF = 0x100000;       // WUF
    static constexpr uint32_t ISR_RWU = 0x80000;        // RWU
    static constexpr uint32_t ISR_SBKF = 0x40000;       // SBKF
    static constexpr uint32_t ISR_CMF = 0x20000;        // CMF
    static constexpr uint32_t ISR_BUSY = 0x10000;       // BUSY
    static constexpr uint32_t ISR_ABRF = 0x8000;        // ABRF
    static constexpr uint32_t ISR_ABRE = 0x4000;        // ABRE
    static constexpr uint32_t ISR_UDR = 0x2000;         // UDR
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
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t ICR_WUCF = 0x100000;      // Wakeup from Stop mode clear flag
    static constexpr uint32_t ICR_CMCF = 0x20000;       // Character match clear flag
    static constexpr uint32_t ICR_UDRCF = 0x2000;       // UDRCF
    static constexpr uint32_t ICR_EOBCF = 0x1000;       // End of block clear flag
    static constexpr uint32_t ICR_RTOCF = 0x800;        // Receiver timeout clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
    static constexpr uint32_t ICR_LBDCF = 0x100;        // LIN break detection clear flag
    static constexpr uint32_t ICR_TCBGTCF = 0x80;       // TCBGTCF
    static constexpr uint32_t ICR_TCCF = 0x40;          // Transmission complete clear flag
    static constexpr uint32_t ICR_TXFECF = 0x20;        // TXFECF
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
    static constexpr uint32_t PRESC_PRESCALER =           // PRESCALER (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESC_RESET_VALUE = 0x0;

    static constexpr uint8_t USART3 = 39; // USART3
};

static usart3_t& USART3 = *reinterpret_cast<usart3_t*>(0x40004800);

#define HAVE_PERIPHERAL_USART3


////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

struct uart4_t
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
    volatile uint32_t    PRESC;                // [Read-write] USART prescaler register

    static constexpr uint32_t CR1_RXFFIE = 0x80000000;  // RXFFIE
    static constexpr uint32_t CR1_TXFEIE = 0x40000000;  // TXFEIE
    static constexpr uint32_t CR1_FIFOEN = 0x20000000;  // FIFOEN
    static constexpr uint32_t CR1_M1 = 0x10000000;      // M1
    static constexpr uint32_t CR1_EOBIE = 0x8000000;    // End of Block interrupt enable
    static constexpr uint32_t CR1_RTOIE = 0x4000000;    // Receiver timeout interrupt enable
    static constexpr uint32_t CR1_DEAT4 = 0x2000000;    // Driver Enable assertion time
    static constexpr uint32_t CR1_DEAT3 = 0x1000000;    // DEAT3
    static constexpr uint32_t CR1_DEAT2 = 0x800000;     // DEAT2
    static constexpr uint32_t CR1_DEAT1 = 0x400000;     // DEAT1
    static constexpr uint32_t CR1_DEAT0 = 0x200000;     // DEAT0
    static constexpr uint32_t CR1_DEDT4 = 0x100000;     // Driver Enable de-assertion time
    static constexpr uint32_t CR1_DEDT3 = 0x80000;      // DEDT3
    static constexpr uint32_t CR1_DEDT2 = 0x40000;      // DEDT2
    static constexpr uint32_t CR1_DEDT1 = 0x20000;      // DEDT1
    static constexpr uint32_t CR1_DEDT0 = 0x10000;      // DEDT0
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
    static constexpr uint32_t CR2_ABRMOD1 = 0x400000;   // Auto baud rate mode
    static constexpr uint32_t CR2_ABRMOD0 = 0x200000;   // ABRMOD0
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
    static constexpr uint32_t CR2_DIS_NSS = 0x8;        // DIS_NSS
    static constexpr uint32_t CR2_SLVEN = 0x1;          // SLVEN
    static const uint32_t CR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CR3_TXFTCFG =             // TXFTCFG (3 bits)
        bit_field_t<29, 0x7>::value<X>();
    static constexpr uint32_t CR3_RXFTIE = 0x10000000;  // RXFTIE
    template<uint32_t X>
    static constexpr uint32_t CR3_RXFTCFG =             // RXFTCFG (3 bits)
        bit_field_t<25, 0x7>::value<X>();
    static constexpr uint32_t CR3_TCBGTIE = 0x1000000;  // TCBGTIE
    static constexpr uint32_t CR3_TXFTIE = 0x800000;    // TXFTIE
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
    static constexpr uint32_t BRR_DIV_Mantissa =        // DIV_Mantissa (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BRR_DIV_Fraction =        // DIV_Fraction (4 bits)
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

    static constexpr uint32_t ISR_TXFT = 0x8000000;     // TXFT
    static constexpr uint32_t ISR_RXFT = 0x4000000;     // RXFT
    static constexpr uint32_t ISR_TCBGT = 0x2000000;    // TCBGT
    static constexpr uint32_t ISR_RXFF = 0x1000000;     // RXFF
    static constexpr uint32_t ISR_TXFE = 0x800000;      // TXFE
    static constexpr uint32_t ISR_REACK = 0x400000;     // REACK
    static constexpr uint32_t ISR_TEACK = 0x200000;     // TEACK
    static constexpr uint32_t ISR_WUF = 0x100000;       // WUF
    static constexpr uint32_t ISR_RWU = 0x80000;        // RWU
    static constexpr uint32_t ISR_SBKF = 0x40000;       // SBKF
    static constexpr uint32_t ISR_CMF = 0x20000;        // CMF
    static constexpr uint32_t ISR_BUSY = 0x10000;       // BUSY
    static constexpr uint32_t ISR_ABRF = 0x8000;        // ABRF
    static constexpr uint32_t ISR_ABRE = 0x4000;        // ABRE
    static constexpr uint32_t ISR_UDR = 0x2000;         // UDR
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
    static constexpr uint32_t ICR_UDRCF = 0x2000;       // UDRCF
    static constexpr uint32_t ICR_EOBCF = 0x1000;       // End of block clear flag
    static constexpr uint32_t ICR_RTOCF = 0x800;        // Receiver timeout clear flag
    static constexpr uint32_t ICR_CTSCF = 0x200;        // CTS clear flag
    static constexpr uint32_t ICR_LBDCF = 0x100;        // LIN break detection clear flag
    static constexpr uint32_t ICR_TCBGTCF = 0x80;       // TCBGTCF
    static constexpr uint32_t ICR_TCCF = 0x40;          // Transmission complete clear flag
    static constexpr uint32_t ICR_TXFECF = 0x20;        // TXFECF
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
    static constexpr uint32_t PRESC_PRESCALER =           // PRESCALER (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESC_RESET_VALUE = 0x0;

    static constexpr uint8_t UART4 = 52; // UART4
};

static uart4_t& UART4 = *reinterpret_cast<uart4_t*>(0x40004c00);

#define HAVE_PERIPHERAL_UART4


////
//
//    Universal synchronous asynchronous receiver transmitter
//
////

struct lpuart1_t
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

    static constexpr uint32_t CR1_RXFFIE = 0x80000000;  // RXFFIE
    static constexpr uint32_t CR1_TXFEIE = 0x40000000;  // TXFEIE
    static constexpr uint32_t CR1_FIFOEN = 0x20000000;  // FIFOEN
    static constexpr uint32_t CR1_M1 = 0x10000000;      // Word length
    static constexpr uint32_t CR1_DEAT4 = 0x2000000;    // Driver Enable assertion time
    static constexpr uint32_t CR1_DEAT3 = 0x1000000;    // DEAT3
    static constexpr uint32_t CR1_DEAT2 = 0x800000;     // DEAT2
    static constexpr uint32_t CR1_DEAT1 = 0x400000;     // DEAT1
    static constexpr uint32_t CR1_DEAT0 = 0x200000;     // DEAT0
    static constexpr uint32_t CR1_DEDT4 = 0x100000;     // Driver Enable de-assertion time
    static constexpr uint32_t CR1_DEDT3 = 0x80000;      // DEDT3
    static constexpr uint32_t CR1_DEDT2 = 0x40000;      // DEDT2
    static constexpr uint32_t CR1_DEDT1 = 0x20000;      // DEDT1
    static constexpr uint32_t CR1_DEDT0 = 0x10000;      // DEDT0
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
    static constexpr uint32_t CR3_TXFTCFG =             // TXFTCFG (3 bits)
        bit_field_t<29, 0x7>::value<X>();
    static constexpr uint32_t CR3_RXFTIE = 0x10000000;  // RXFTIE
    template<uint32_t X>
    static constexpr uint32_t CR3_RXFTCFG =             // RXFTCFG (3 bits)
        bit_field_t<25, 0x7>::value<X>();
    static constexpr uint32_t CR3_TXFTIE = 0x800000;    // TXFTIE
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

    static constexpr uint32_t RQR_TXFRQ = 0x10;         // TXFRQ
    static constexpr uint32_t RQR_RXFRQ = 0x8;          // Receive data flush request
    static constexpr uint32_t RQR_MMRQ = 0x4;           // Mute mode request
    static constexpr uint32_t RQR_SBKRQ = 0x2;          // Send break request
    static const uint32_t RQR_RESET_VALUE = 0x0;

    static constexpr uint32_t ISR_TXFT = 0x8000000;     // TXFT
    static constexpr uint32_t ISR_RXFT = 0x4000000;     // RXFT
    static constexpr uint32_t ISR_RXFF = 0x1000000;     // RXFF
    static constexpr uint32_t ISR_TXFE = 0x800000;      // TXFE
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
    static constexpr uint32_t PRESC_PRESCALER =           // PRESCALER (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESC_RESET_VALUE = 0x0;

    static constexpr uint8_t LPTIM1 = 49; // LPTIM1
    static constexpr uint8_t LPUART = 91; // LPUART
};

static lpuart1_t& LPUART1 = *reinterpret_cast<lpuart1_t*>(0x40008000);

#define HAVE_PERIPHERAL_LPUART1


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
    static const uint32_t CR2_RESET_VALUE = 0x700;

    static constexpr uint32_t SR_RXNE = 0x1;           // Receive buffer not empty, Read-only
    static constexpr uint32_t SR_TXE = 0x2;            // Transmit buffer empty, Read-only
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

    static constexpr uint32_t I2SCFGR_CHLEN = 0x1;          // CHLEN
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_DATLEN =              // DATLEN (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_CKPOL = 0x8;          // CKPOL
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SSTD =              // I2SSTD (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_PCMSYNC = 0x80;       // PCMSYNC
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SCFG =              // I2SCFG (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_I2SE = 0x400;         // I2SE
    static constexpr uint32_t I2SCFGR_I2SMOD = 0x800;       // I2SMOD
    static const uint32_t I2SCFGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t I2SPR_I2SDIV =              // I2SDIV (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t I2SPR_ODD = 0x100;          // ODD
    static constexpr uint32_t I2SPR_MCKOE = 0x200;        // MCKOE
    static const uint32_t I2SPR_RESET_VALUE = 0x2;

    static constexpr uint8_t SPI1 = 35; // SPI1
};

static spi1_t& SPI1 = *reinterpret_cast<spi1_t*>(0x40013000);

#define HAVE_PERIPHERAL_SPI1


////
//
//    Serial peripheral interface/Inter-IC sound
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
    volatile uint32_t    I2SCFGR;              // [Read-write] configuration register
    volatile uint32_t    I2SPR;                // [Read-write] prescaler register

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
    static const uint32_t CR2_RESET_VALUE = 0x700;

    static constexpr uint32_t SR_RXNE = 0x1;           // Receive buffer not empty, Read-only
    static constexpr uint32_t SR_TXE = 0x2;            // Transmit buffer empty, Read-only
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

    static constexpr uint32_t I2SCFGR_CHLEN = 0x1;          // CHLEN
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_DATLEN =              // DATLEN (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_CKPOL = 0x8;          // CKPOL
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SSTD =              // I2SSTD (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_PCMSYNC = 0x80;       // PCMSYNC
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SCFG =              // I2SCFG (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_I2SE = 0x400;         // I2SE
    static constexpr uint32_t I2SCFGR_I2SMOD = 0x800;       // I2SMOD
    static const uint32_t I2SCFGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t I2SPR_I2SDIV =              // I2SDIV (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t I2SPR_ODD = 0x100;          // ODD
    static constexpr uint32_t I2SPR_MCKOE = 0x200;        // MCKOE
    static const uint32_t I2SPR_RESET_VALUE = 0x2;

    static constexpr uint8_t SPI3 = 51; // SPI3
};

static spi3_t& SPI3 = *reinterpret_cast<spi3_t*>(0x40003c00);

#define HAVE_PERIPHERAL_SPI3


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
    static const uint32_t CR2_RESET_VALUE = 0x700;

    static constexpr uint32_t SR_RXNE = 0x1;           // Receive buffer not empty, Read-only
    static constexpr uint32_t SR_TXE = 0x2;            // Transmit buffer empty, Read-only
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

    static constexpr uint32_t I2SCFGR_CHLEN = 0x1;          // CHLEN
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_DATLEN =              // DATLEN (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_CKPOL = 0x8;          // CKPOL
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SSTD =              // I2SSTD (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_PCMSYNC = 0x80;       // PCMSYNC
    template<uint32_t X>
    static constexpr uint32_t I2SCFGR_I2SCFG =              // I2SCFG (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static constexpr uint32_t I2SCFGR_I2SE = 0x400;         // I2SE
    static constexpr uint32_t I2SCFGR_I2SMOD = 0x800;       // I2SMOD
    static const uint32_t I2SCFGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t I2SPR_I2SDIV =              // I2SDIV (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static constexpr uint32_t I2SPR_ODD = 0x100;          // ODD
    static constexpr uint32_t I2SPR_MCKOE = 0x200;        // MCKOE
    static const uint32_t I2SPR_RESET_VALUE = 0x2;

    static constexpr uint8_t SPI2 = 36; // SPI2
};

static spi2_t& SPI2 = *reinterpret_cast<spi2_t*>(0x40003800);

#define HAVE_PERIPHERAL_SPI2


////
//
//    External interrupt/event controller
//
////

struct exti_t
{
    volatile uint32_t    IMR1;                 // [Read-write] Interrupt mask register
    volatile uint32_t    EMR1;                 // [Read-write] Event mask register
    volatile uint32_t    RTSR1;                // [Read-write] Rising Trigger selection register
    volatile uint32_t    FTSR1;                // [Read-write] Falling Trigger selection register
    volatile uint32_t    SWIER1;               // [Read-write] Software interrupt event register
    volatile uint32_t    PR1;                  // [Read-write] Pending register
    reserved_t<2>        _0;
    volatile uint32_t    IMR2;                 // [Read-write] Interrupt mask register
    volatile uint32_t    EMR2;                 // [Read-write] Event mask register
    volatile uint32_t    RTSR2;                // [Read-write] Rising Trigger selection register
    volatile uint32_t    FTSR2;                // [Read-write] Falling Trigger selection register
    volatile uint32_t    SWIER2;               // [Read-write] Software interrupt event register
    volatile uint32_t    PR2;                  // [Read-write] Pending register

    static constexpr uint32_t IMR1_IM0 = 0x1;            // Interrupt Mask on line 0
    static constexpr uint32_t IMR1_IM1 = 0x2;            // Interrupt Mask on line 1
    static constexpr uint32_t IMR1_IM2 = 0x4;            // Interrupt Mask on line 2
    static constexpr uint32_t IMR1_IM3 = 0x8;            // Interrupt Mask on line 3
    static constexpr uint32_t IMR1_IM4 = 0x10;           // Interrupt Mask on line 4
    static constexpr uint32_t IMR1_IM5 = 0x20;           // Interrupt Mask on line 5
    static constexpr uint32_t IMR1_IM6 = 0x40;           // Interrupt Mask on line 6
    static constexpr uint32_t IMR1_IM7 = 0x80;           // Interrupt Mask on line 7
    static constexpr uint32_t IMR1_IM8 = 0x100;          // Interrupt Mask on line 8
    static constexpr uint32_t IMR1_IM9 = 0x200;          // Interrupt Mask on line 9
    static constexpr uint32_t IMR1_IM10 = 0x400;         // Interrupt Mask on line 10
    static constexpr uint32_t IMR1_IM11 = 0x800;         // Interrupt Mask on line 11
    static constexpr uint32_t IMR1_IM12 = 0x1000;        // Interrupt Mask on line 12
    static constexpr uint32_t IMR1_IM13 = 0x2000;        // Interrupt Mask on line 13
    static constexpr uint32_t IMR1_IM14 = 0x4000;        // Interrupt Mask on line 14
    static constexpr uint32_t IMR1_IM15 = 0x8000;        // Interrupt Mask on line 15
    static constexpr uint32_t IMR1_IM16 = 0x10000;       // Interrupt Mask on line 16
    static constexpr uint32_t IMR1_IM17 = 0x20000;       // Interrupt Mask on line 17
    static constexpr uint32_t IMR1_IM18 = 0x40000;       // Interrupt Mask on line 18
    static constexpr uint32_t IMR1_IM19 = 0x80000;       // Interrupt Mask on line 19
    static constexpr uint32_t IMR1_IM20 = 0x100000;      // Interrupt Mask on line 20
    static constexpr uint32_t IMR1_IM21 = 0x200000;      // Interrupt Mask on line 21
    static constexpr uint32_t IMR1_IM22 = 0x400000;      // Interrupt Mask on line 22
    static constexpr uint32_t IMR1_IM23 = 0x800000;      // Interrupt Mask on line 23
    static constexpr uint32_t IMR1_IM24 = 0x1000000;     // Interrupt Mask on line 24
    static constexpr uint32_t IMR1_IM25 = 0x2000000;     // Interrupt Mask on line 25
    static constexpr uint32_t IMR1_IM26 = 0x4000000;     // Interrupt Mask on line 26
    static constexpr uint32_t IMR1_IM27 = 0x8000000;     // Interrupt Mask on line 27
    static constexpr uint32_t IMR1_IM28 = 0x10000000;    // Interrupt Mask on line 28
    static constexpr uint32_t IMR1_IM29 = 0x20000000;    // Interrupt Mask on line 29
    static constexpr uint32_t IMR1_IM30 = 0x40000000;    // Interrupt Mask on line 30
    static constexpr uint32_t IMR1_IM31 = 0x80000000;    // Interrupt Mask on line 31
    static const uint32_t IMR1_RESET_VALUE = 0xff820000;

    static constexpr uint32_t EMR1_EM0 = 0x1;            // Event Mask on line 0
    static constexpr uint32_t EMR1_EM1 = 0x2;            // Event Mask on line 1
    static constexpr uint32_t EMR1_EM2 = 0x4;            // Event Mask on line 2
    static constexpr uint32_t EMR1_EM3 = 0x8;            // Event Mask on line 3
    static constexpr uint32_t EMR1_EM4 = 0x10;           // Event Mask on line 4
    static constexpr uint32_t EMR1_EM5 = 0x20;           // Event Mask on line 5
    static constexpr uint32_t EMR1_EM6 = 0x40;           // Event Mask on line 6
    static constexpr uint32_t EMR1_EM7 = 0x80;           // Event Mask on line 7
    static constexpr uint32_t EMR1_EM8 = 0x100;          // Event Mask on line 8
    static constexpr uint32_t EMR1_EM9 = 0x200;          // Event Mask on line 9
    static constexpr uint32_t EMR1_EM10 = 0x400;         // Event Mask on line 10
    static constexpr uint32_t EMR1_EM11 = 0x800;         // Event Mask on line 11
    static constexpr uint32_t EMR1_EM12 = 0x1000;        // Event Mask on line 12
    static constexpr uint32_t EMR1_EM13 = 0x2000;        // Event Mask on line 13
    static constexpr uint32_t EMR1_EM14 = 0x4000;        // Event Mask on line 14
    static constexpr uint32_t EMR1_EM15 = 0x8000;        // Event Mask on line 15
    static constexpr uint32_t EMR1_EM16 = 0x10000;       // Event Mask on line 16
    static constexpr uint32_t EMR1_EM17 = 0x20000;       // Event Mask on line 17
    static constexpr uint32_t EMR1_EM18 = 0x40000;       // Event Mask on line 18
    static constexpr uint32_t EMR1_EM19 = 0x80000;       // Event Mask on line 19
    static constexpr uint32_t EMR1_EM20 = 0x100000;      // Event Mask on line 20
    static constexpr uint32_t EMR1_EM21 = 0x200000;      // Event Mask on line 21
    static constexpr uint32_t EMR1_EM22 = 0x400000;      // Event Mask on line 22
    static constexpr uint32_t EMR1_EM23 = 0x800000;      // Event Mask on line 23
    static constexpr uint32_t EMR1_EM24 = 0x1000000;     // Event Mask on line 24
    static constexpr uint32_t EMR1_EM25 = 0x2000000;     // Event Mask on line 25
    static constexpr uint32_t EMR1_EM26 = 0x4000000;     // Event Mask on line 26
    static constexpr uint32_t EMR1_EM27 = 0x8000000;     // Event Mask on line 27
    static constexpr uint32_t EMR1_EM28 = 0x10000000;    // Event Mask on line 28
    static constexpr uint32_t EMR1_EM29 = 0x20000000;    // Event Mask on line 29
    static constexpr uint32_t EMR1_EM30 = 0x40000000;    // Event Mask on line 30
    static constexpr uint32_t EMR1_EM31 = 0x80000000;    // Event Mask on line 31
    static const uint32_t EMR1_RESET_VALUE = 0x0;

    static constexpr uint32_t RTSR1_RT0 = 0x1;            // Rising trigger event configuration of line 0
    static constexpr uint32_t RTSR1_RT1 = 0x2;            // Rising trigger event configuration of line 1
    static constexpr uint32_t RTSR1_RT2 = 0x4;            // Rising trigger event configuration of line 2
    static constexpr uint32_t RTSR1_RT3 = 0x8;            // Rising trigger event configuration of line 3
    static constexpr uint32_t RTSR1_RT4 = 0x10;           // Rising trigger event configuration of line 4
    static constexpr uint32_t RTSR1_RT5 = 0x20;           // Rising trigger event configuration of line 5
    static constexpr uint32_t RTSR1_RT6 = 0x40;           // Rising trigger event configuration of line 6
    static constexpr uint32_t RTSR1_RT7 = 0x80;           // Rising trigger event configuration of line 7
    static constexpr uint32_t RTSR1_RT8 = 0x100;          // Rising trigger event configuration of line 8
    static constexpr uint32_t RTSR1_RT9 = 0x200;          // Rising trigger event configuration of line 9
    static constexpr uint32_t RTSR1_RT10 = 0x400;         // Rising trigger event configuration of line 10
    static constexpr uint32_t RTSR1_RT11 = 0x800;         // Rising trigger event configuration of line 11
    static constexpr uint32_t RTSR1_RT12 = 0x1000;        // Rising trigger event configuration of line 12
    static constexpr uint32_t RTSR1_RT13 = 0x2000;        // Rising trigger event configuration of line 13
    static constexpr uint32_t RTSR1_RT14 = 0x4000;        // Rising trigger event configuration of line 14
    static constexpr uint32_t RTSR1_RT15 = 0x8000;        // Rising trigger event configuration of line 15
    static constexpr uint32_t RTSR1_RT16 = 0x10000;       // Rising trigger event configuration of line 16
    static constexpr uint32_t RTSR1_RT18 = 0x40000;       // Rising trigger event configuration of line 18
    static constexpr uint32_t RTSR1_RT19 = 0x80000;       // Rising trigger event configuration of line 19
    static constexpr uint32_t RTSR1_RT20 = 0x100000;      // Rising trigger event configuration of line 20
    static constexpr uint32_t RTSR1_RT21 = 0x200000;      // Rising trigger event configuration of line 21
    static constexpr uint32_t RTSR1_RT22 = 0x400000;      // Rising trigger event configuration of line 22
    template<uint32_t X>
    static constexpr uint32_t RTSR1_RT =                  // RT (3 bits)
        bit_field_t<29, 0x7>::value<X>();
    static const uint32_t RTSR1_RESET_VALUE = 0x0;

    static constexpr uint32_t FTSR1_FT0 = 0x1;            // Falling trigger event configuration of line 0
    static constexpr uint32_t FTSR1_FT1 = 0x2;            // Falling trigger event configuration of line 1
    static constexpr uint32_t FTSR1_FT2 = 0x4;            // Falling trigger event configuration of line 2
    static constexpr uint32_t FTSR1_FT3 = 0x8;            // Falling trigger event configuration of line 3
    static constexpr uint32_t FTSR1_FT4 = 0x10;           // Falling trigger event configuration of line 4
    static constexpr uint32_t FTSR1_FT5 = 0x20;           // Falling trigger event configuration of line 5
    static constexpr uint32_t FTSR1_FT6 = 0x40;           // Falling trigger event configuration of line 6
    static constexpr uint32_t FTSR1_FT7 = 0x80;           // Falling trigger event configuration of line 7
    static constexpr uint32_t FTSR1_FT8 = 0x100;          // Falling trigger event configuration of line 8
    static constexpr uint32_t FTSR1_FT9 = 0x200;          // Falling trigger event configuration of line 9
    static constexpr uint32_t FTSR1_FT10 = 0x400;         // Falling trigger event configuration of line 10
    static constexpr uint32_t FTSR1_FT11 = 0x800;         // Falling trigger event configuration of line 11
    static constexpr uint32_t FTSR1_FT12 = 0x1000;        // Falling trigger event configuration of line 12
    static constexpr uint32_t FTSR1_FT13 = 0x2000;        // Falling trigger event configuration of line 13
    static constexpr uint32_t FTSR1_FT14 = 0x4000;        // Falling trigger event configuration of line 14
    static constexpr uint32_t FTSR1_FT15 = 0x8000;        // Falling trigger event configuration of line 15
    static constexpr uint32_t FTSR1_FT16 = 0x10000;       // Falling trigger event configuration of line 16
    static constexpr uint32_t FTSR1_FT18 = 0x40000;       // Falling trigger event configuration of line 18
    static constexpr uint32_t FTSR1_FT19 = 0x80000;       // Falling trigger event configuration of line 19
    static constexpr uint32_t FTSR1_FT20 = 0x100000;      // Falling trigger event configuration of line 20
    static constexpr uint32_t FTSR1_FT21 = 0x200000;      // Falling trigger event configuration of line 21
    static constexpr uint32_t FTSR1_FT22 = 0x400000;      // Falling trigger event configuration of line 22
    static const uint32_t FTSR1_RESET_VALUE = 0x0;

    static constexpr uint32_t SWIER1_SWI0 = 0x1;           // Software Interrupt on line 0
    static constexpr uint32_t SWIER1_SWI1 = 0x2;           // Software Interrupt on line 1
    static constexpr uint32_t SWIER1_SWI2 = 0x4;           // Software Interrupt on line 2
    static constexpr uint32_t SWIER1_SWI3 = 0x8;           // Software Interrupt on line 3
    static constexpr uint32_t SWIER1_SWI4 = 0x10;          // Software Interrupt on line 4
    static constexpr uint32_t SWIER1_SWI5 = 0x20;          // Software Interrupt on line 5
    static constexpr uint32_t SWIER1_SWI6 = 0x40;          // Software Interrupt on line 6
    static constexpr uint32_t SWIER1_SWI7 = 0x80;          // Software Interrupt on line 7
    static constexpr uint32_t SWIER1_SWI8 = 0x100;         // Software Interrupt on line 8
    static constexpr uint32_t SWIER1_SWI9 = 0x200;         // Software Interrupt on line 9
    static constexpr uint32_t SWIER1_SWI10 = 0x400;        // Software Interrupt on line 10
    static constexpr uint32_t SWIER1_SWI11 = 0x800;        // Software Interrupt on line 11
    static constexpr uint32_t SWIER1_SWI12 = 0x1000;       // Software Interrupt on line 12
    static constexpr uint32_t SWIER1_SWI13 = 0x2000;       // Software Interrupt on line 13
    static constexpr uint32_t SWIER1_SWI14 = 0x4000;       // Software Interrupt on line 14
    static constexpr uint32_t SWIER1_SWI15 = 0x8000;       // Software Interrupt on line 15
    static constexpr uint32_t SWIER1_SWI16 = 0x10000;      // Software Interrupt on line 16
    static constexpr uint32_t SWIER1_SWI18 = 0x40000;      // Software Interrupt on line 18
    static constexpr uint32_t SWIER1_SWI19 = 0x80000;      // Software Interrupt on line 19
    static constexpr uint32_t SWIER1_SWI20 = 0x100000;     // Software Interrupt on line 20
    static constexpr uint32_t SWIER1_SWI21 = 0x200000;     // Software Interrupt on line 21
    static constexpr uint32_t SWIER1_SWI22 = 0x400000;     // Software Interrupt on line 22
    static const uint32_t SWIER1_RESET_VALUE = 0x0;

    static constexpr uint32_t PR1_PIF0 = 0x1;           // Pending bit 0
    static constexpr uint32_t PR1_PIF1 = 0x2;           // Pending bit 1
    static constexpr uint32_t PR1_PIF2 = 0x4;           // Pending bit 2
    static constexpr uint32_t PR1_PIF3 = 0x8;           // Pending bit 3
    static constexpr uint32_t PR1_PIF4 = 0x10;          // Pending bit 4
    static constexpr uint32_t PR1_PIF5 = 0x20;          // Pending bit 5
    static constexpr uint32_t PR1_PIF6 = 0x40;          // Pending bit 6
    static constexpr uint32_t PR1_PIF7 = 0x80;          // Pending bit 7
    static constexpr uint32_t PR1_PIF8 = 0x100;         // Pending bit 8
    static constexpr uint32_t PR1_PIF9 = 0x200;         // Pending bit 9
    static constexpr uint32_t PR1_PIF10 = 0x400;        // Pending bit 10
    static constexpr uint32_t PR1_PIF11 = 0x800;        // Pending bit 11
    static constexpr uint32_t PR1_PIF12 = 0x1000;       // Pending bit 12
    static constexpr uint32_t PR1_PIF13 = 0x2000;       // Pending bit 13
    static constexpr uint32_t PR1_PIF14 = 0x4000;       // Pending bit 14
    static constexpr uint32_t PR1_PIF15 = 0x8000;       // Pending bit 15
    static constexpr uint32_t PR1_PIF16 = 0x10000;      // Pending bit 16
    static constexpr uint32_t PR1_PIF18 = 0x40000;      // Pending bit 18
    static constexpr uint32_t PR1_PIF19 = 0x80000;      // Pending bit 19
    static constexpr uint32_t PR1_PIF20 = 0x100000;     // Pending bit 20
    static constexpr uint32_t PR1_PIF21 = 0x200000;     // Pending bit 21
    static constexpr uint32_t PR1_PIF22 = 0x400000;     // Pending bit 22
    static const uint32_t PR1_RESET_VALUE = 0x0;

    static constexpr uint32_t IMR2_IM32 = 0x1;           // Interrupt Mask on external/internal line 32
    static constexpr uint32_t IMR2_IM33 = 0x2;           // Interrupt Mask on external/internal line 33
    static constexpr uint32_t IMR2_IM34 = 0x4;           // Interrupt Mask on external/internal line 34
    static constexpr uint32_t IMR2_IM35 = 0x8;           // Interrupt Mask on external/internal line 35
    static constexpr uint32_t IMR2_IM36 = 0x10;          // Interrupt Mask on external/internal line 36
    static constexpr uint32_t IMR2_IM37 = 0x20;          // Interrupt Mask on external/internal line 37
    static constexpr uint32_t IMR2_IM38 = 0x40;          // Interrupt Mask on external/internal line 38
    static constexpr uint32_t IMR2_IM39 = 0x80;          // Interrupt Mask on external/internal line 39
    static constexpr uint32_t IMR2_IM40 = 0x100;         // Interrupt Mask on external/internal line 40
    static constexpr uint32_t IMR2_IM41 = 0x200;         // Interrupt Mask on external/internal line 41
    static constexpr uint32_t IMR2_IM42 = 0x400;         // Interrupt Mask on external/internal line 42
    static constexpr uint32_t IMR2_IM43 = 0x800;         // Interrupt Mask on external/internal line 43
    static const uint32_t IMR2_RESET_VALUE = 0xffffff87;

    static constexpr uint32_t EMR2_EM32 = 0x1;           // Event mask on external/internal line 32
    static constexpr uint32_t EMR2_EM33 = 0x2;           // Event mask on external/internal line 33
    static constexpr uint32_t EMR2_EM34 = 0x4;           // Event mask on external/internal line 34
    static constexpr uint32_t EMR2_EM35 = 0x8;           // Event mask on external/internal line 35
    static constexpr uint32_t EMR2_EM36 = 0x10;          // Event mask on external/internal line 36
    static constexpr uint32_t EMR2_EM37 = 0x20;          // Event mask on external/internal line 37
    static constexpr uint32_t EMR2_EM38 = 0x40;          // Event mask on external/internal line 38
    static constexpr uint32_t EMR2_EM39 = 0x80;          // Event mask on external/internal line 39
    static constexpr uint32_t EMR2_EM40 = 0x100;         // Event mask on external/internal line 40
    static const uint32_t EMR2_RESET_VALUE = 0x0;

    static constexpr uint32_t RTSR2_RT32 = 0x1;           // Rising trigger event configuration bit of line 32
    static constexpr uint32_t RTSR2_RT33 = 0x2;           // Rising trigger event configuration bit of line 32
    static constexpr uint32_t RTSR2_RT38 = 0x40;          // Rising trigger event configuration bit of line 38
    static constexpr uint32_t RTSR2_RT39 = 0x80;          // Rising trigger event configuration bit of line 39
    static constexpr uint32_t RTSR2_RT40 = 0x100;         // Rising trigger event configuration bit of line 40
    static constexpr uint32_t RTSR2_RT41 = 0x200;         // Rising trigger event configuration bit of line 41
    static const uint32_t RTSR2_RESET_VALUE = 0x0;

    static constexpr uint32_t FTSR2_FT35 = 0x8;           // Falling trigger event configuration bit of line 35
    static constexpr uint32_t FTSR2_FT36 = 0x10;          // Falling trigger event configuration bit of line 36
    static constexpr uint32_t FTSR2_FT37 = 0x20;          // Falling trigger event configuration bit of line 37
    static constexpr uint32_t FTSR2_FT38 = 0x40;          // Falling trigger event configuration bit of line 38
    static const uint32_t FTSR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SWIER2_SWI35 = 0x8;          // Software interrupt on line 35
    static constexpr uint32_t SWIER2_SWI36 = 0x10;         // Software interrupt on line 36
    static constexpr uint32_t SWIER2_SWI37 = 0x20;         // Software interrupt on line 37
    static constexpr uint32_t SWIER2_SWI38 = 0x40;         // Software interrupt on line 38
    static const uint32_t SWIER2_RESET_VALUE = 0x0;

    static constexpr uint32_t PR2_PIF35 = 0x8;          // Pending interrupt flag on line 35
    static constexpr uint32_t PR2_PIF36 = 0x10;         // Pending interrupt flag on line 36
    static constexpr uint32_t PR2_PIF37 = 0x20;         // Pending interrupt flag on line 37
    static constexpr uint32_t PR2_PIF38 = 0x40;         // Pending interrupt flag on line 38
    static const uint32_t PR2_RESET_VALUE = 0x0;

    static constexpr uint8_t CRS = 75; // CRS
    static constexpr uint8_t EXTI0 = 6; // EXTI Line0 interrupt
    static constexpr uint8_t EXTI1 = 7; // EXTI Line1 interrupt
    static constexpr uint8_t EXTI15_10 = 40; // EXTI15_10
    static constexpr uint8_t EXTI2 = 8; // EXTI Line2 interrupt
    static constexpr uint8_t EXTI3 = 9; // EXTI Line3 interrupt
    static constexpr uint8_t EXTI4 = 10; // EXTI Line4 interrupt
    static constexpr uint8_t EXTI9_5 = 23; // EXTI9_5
    static constexpr uint8_t PVD_PVM = 1; // PVD through EXTI line detection
    static constexpr uint8_t USBWAKEUP = 42; // USBWakeUP
    static constexpr uint8_t USB_HP = 19; // USB_HP
    static constexpr uint8_t USB_LP = 20; // USB_LP
    static constexpr uint8_t FDCAN1_INTR0_IT = 22; // fdcan1_intr0_it
    static constexpr uint8_t FDCAN1_INTR1_IT = 21; // fdcan1_intr1_it
};

static exti_t& EXTI = *reinterpret_cast<exti_t*>(0x40010400);

#define HAVE_PERIPHERAL_EXTI


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
    volatile uint32_t    MISR;                 // [Read-only] status register
    reserved_t<1>        _2;
    volatile uint32_t    SCR;                  // [Write-only] status register

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
    static constexpr uint32_t CR_WCKSEL =              // Wakeup clock selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t CR_TSEDGE = 0x8;         // Time-stamp event active edge
    static constexpr uint32_t CR_REFCKON = 0x10;       // Reference clock detection enable (50 or 60 Hz)
    static constexpr uint32_t CR_BYPSHAD = 0x20;       // Bypass the shadow registers
    static constexpr uint32_t CR_FMT = 0x40;           // Hour format
    static constexpr uint32_t CR_ALRAE = 0x100;        // Alarm A enable
    static constexpr uint32_t CR_ALRBE = 0x200;        // Alarm B enable
    static constexpr uint32_t CR_WUTE = 0x400;         // Wakeup timer enable
    static constexpr uint32_t CR_TSE = 0x800;          // Time stamp enable
    static constexpr uint32_t CR_ALRAIE = 0x1000;      // Alarm A interrupt enable
    static constexpr uint32_t CR_ALRBIE = 0x2000;      // Alarm B interrupt enable
    static constexpr uint32_t CR_WUTIE = 0x4000;       // Wakeup timer interrupt enable
    static constexpr uint32_t CR_TSIE = 0x8000;        // Time-stamp interrupt enable
    static constexpr uint32_t CR_ADD1H = 0x10000;      // Add 1 hour (summer time change)
    static constexpr uint32_t CR_SUB1H = 0x20000;      // Subtract 1 hour (winter time change)
    static constexpr uint32_t CR_BKP = 0x40000;        // Backup
    static constexpr uint32_t CR_COSEL = 0x80000;      // Calibration output selection
    static constexpr uint32_t CR_POL = 0x100000;       // Output polarity
    template<uint32_t X>
    static constexpr uint32_t CR_OSEL =                // Output selection (2 bits)
        bit_field_t<21, 0x3>::value<X>();
    static constexpr uint32_t CR_COE = 0x800000;       // Calibration output enable
    static constexpr uint32_t CR_ITSE = 0x1000000;     // timestamp on internal event enable
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

    static constexpr uint8_t RTC_ALARM = 41; // RTC_ALARM
    static constexpr uint8_t RTC_TAMP_CSS_LSE = 2; // RTC_TAMP_CSS_LSE
    static constexpr uint8_t RTC_WKUP = 3; // RTC Wakeup timer
};

static rtc_t& RTC = *reinterpret_cast<rtc_t*>(0x40002800);

#define HAVE_PERIPHERAL_RTC


////
//
//    DMA controller
//
////

struct dma1_t
{
    volatile uint32_t    ISR;                  // [Read-only] interrupt status register
    volatile uint32_t    IFCR;                 // [Write-only] DMA interrupt flag clear register
    volatile uint32_t    CCR1;                 // [Read-write] DMA channel 1 configuration register
    volatile uint32_t    CNDTR1;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR1;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR1;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _0;
    volatile uint32_t    CCR2;                 // [Read-write] DMA channel 2 configuration register
    volatile uint32_t    CNDTR2;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR2;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR2;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _1;
    volatile uint32_t    CCR3;                 // [Read-write] DMA channel 3 configuration register
    volatile uint32_t    CNDTR3;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR3;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR3;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _2;
    volatile uint32_t    CCR4;                 // [Read-write] DMA channel 3 configuration register
    volatile uint32_t    CNDTR4;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR4;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR4;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _3;
    volatile uint32_t    CCR5;                 // [Read-write] DMA channel 4 configuration register
    volatile uint32_t    CNDTR5;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR5;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR5;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _4;
    volatile uint32_t    CCR6;                 // [Read-write] DMA channel 5 configuration register
    volatile uint32_t    CNDTR6;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR6;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR6;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _5;
    volatile uint32_t    CCR7;                 // [Read-write] DMA channel 6 configuration register
    volatile uint32_t    CNDTR7;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR7;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR7;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _6;
    volatile uint32_t    CCR8;                 // [Read-write] DMA channel 7 configuration register
    volatile uint32_t    CNDTR8;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR8;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR8;                // [Read-write] DMA channel x memory address register

    static constexpr uint32_t ISR_TEIF8 = 0x80000000;   // TEIF8
    static constexpr uint32_t ISR_HTIF8 = 0x40000000;   // HTIF8
    static constexpr uint32_t ISR_TCIF8 = 0x20000000;   // TCIF8
    static constexpr uint32_t ISR_GIF8 = 0x10000000;    // GIF8
    static constexpr uint32_t ISR_TEIF7 = 0x8000000;    // TEIF7
    static constexpr uint32_t ISR_HTIF7 = 0x4000000;    // HTIF7
    static constexpr uint32_t ISR_TCIF7 = 0x2000000;    // TCIF7
    static constexpr uint32_t ISR_GIF7 = 0x1000000;     // GIF7
    static constexpr uint32_t ISR_TEIF6 = 0x800000;     // TEIF6
    static constexpr uint32_t ISR_HTIF6 = 0x400000;     // HTIF6
    static constexpr uint32_t ISR_TCIF6 = 0x200000;     // TCIF6
    static constexpr uint32_t ISR_GIF6 = 0x100000;      // GIF6
    static constexpr uint32_t ISR_TEIF5 = 0x80000;      // TEIF5
    static constexpr uint32_t ISR_HTIF5 = 0x40000;      // HTIF5
    static constexpr uint32_t ISR_TCIF5 = 0x20000;      // TCIF5
    static constexpr uint32_t ISR_GIF5 = 0x10000;       // GIF5
    static constexpr uint32_t ISR_TEIF4 = 0x8000;       // TEIF4
    static constexpr uint32_t ISR_HTIF4 = 0x4000;       // HTIF4
    static constexpr uint32_t ISR_TCIF4 = 0x2000;       // TCIF4
    static constexpr uint32_t ISR_GIF4 = 0x1000;        // GIF4
    static constexpr uint32_t ISR_TEIF3 = 0x800;        // TEIF3
    static constexpr uint32_t ISR_HTIF3 = 0x400;        // HTIF3
    static constexpr uint32_t ISR_TCIF3 = 0x200;        // TCIF3
    static constexpr uint32_t ISR_GIF3 = 0x100;         // GIF3
    static constexpr uint32_t ISR_TEIF2 = 0x80;         // TEIF2
    static constexpr uint32_t ISR_HTIF2 = 0x40;         // HTIF2
    static constexpr uint32_t ISR_TCIF2 = 0x20;         // TCIF2
    static constexpr uint32_t ISR_GIF2 = 0x10;          // GIF2
    static constexpr uint32_t ISR_TEIF1 = 0x8;          // TEIF1
    static constexpr uint32_t ISR_HTIF1 = 0x4;          // HTIF1
    static constexpr uint32_t ISR_TCIF1 = 0x2;          // TCIF1
    static constexpr uint32_t ISR_GIF1 = 0x1;           // GIF1
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t IFCR_TEIF8 = 0x80000000;   // TEIF8
    static constexpr uint32_t IFCR_HTIF8 = 0x40000000;   // HTIF8
    static constexpr uint32_t IFCR_TCIF8 = 0x20000000;   // TCIF8
    static constexpr uint32_t IFCR_GIF8 = 0x10000000;    // GIF8
    static constexpr uint32_t IFCR_TEIF7 = 0x8000000;    // TEIF7
    static constexpr uint32_t IFCR_HTIF7 = 0x4000000;    // HTIF7
    static constexpr uint32_t IFCR_TCIF7 = 0x2000000;    // TCIF7
    static constexpr uint32_t IFCR_GIF7 = 0x1000000;     // GIF7
    static constexpr uint32_t IFCR_TEIF6 = 0x800000;     // TEIF6
    static constexpr uint32_t IFCR_HTIF6 = 0x400000;     // HTIF6
    static constexpr uint32_t IFCR_TCIF6 = 0x200000;     // TCIF6
    static constexpr uint32_t IFCR_GIF6 = 0x100000;      // GIF6
    static constexpr uint32_t IFCR_TEIF5 = 0x80000;      // TEIF5
    static constexpr uint32_t IFCR_HTIF5 = 0x40000;      // HTIF5
    static constexpr uint32_t IFCR_TCIF5 = 0x20000;      // TCIF5
    static constexpr uint32_t IFCR_GIF5 = 0x10000;       // GIF5
    static constexpr uint32_t IFCR_TEIF4 = 0x8000;       // TEIF4
    static constexpr uint32_t IFCR_HTIF4 = 0x4000;       // HTIF4
    static constexpr uint32_t IFCR_TCIF4 = 0x2000;       // TCIF4
    static constexpr uint32_t IFCR_GIF4 = 0x1000;        // GIF4
    static constexpr uint32_t IFCR_TEIF3 = 0x800;        // TEIF3
    static constexpr uint32_t IFCR_HTIF3 = 0x400;        // HTIF3
    static constexpr uint32_t IFCR_TCIF3 = 0x200;        // TCIF3
    static constexpr uint32_t IFCR_GIF3 = 0x100;         // GIF3
    static constexpr uint32_t IFCR_TEIF2 = 0x80;         // TEIF2
    static constexpr uint32_t IFCR_HTIF2 = 0x40;         // HTIF2
    static constexpr uint32_t IFCR_TCIF2 = 0x20;         // TCIF2
    static constexpr uint32_t IFCR_GIF2 = 0x10;          // GIF2
    static constexpr uint32_t IFCR_TEIF1 = 0x8;          // TEIF1
    static constexpr uint32_t IFCR_HTIF1 = 0x4;          // HTIF1
    static constexpr uint32_t IFCR_TCIF1 = 0x2;          // TCIF1
    static constexpr uint32_t IFCR_GIF1 = 0x1;           // GIF1
    static const uint32_t IFCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR1_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR1_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR1_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR1_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR1_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR1_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR1_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR1_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR1_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR1_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR1_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR1_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR1_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR1_RESET_VALUE = 0x0;


    static const uint32_t CPAR1_RESET_VALUE = 0x0;


    static const uint32_t CMAR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR2_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR2_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR2_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR2_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR2_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR2_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR2_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR2_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR2_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR2_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR2_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR2_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR2_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR2_RESET_VALUE = 0x0;


    static const uint32_t CPAR2_RESET_VALUE = 0x0;


    static const uint32_t CMAR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR3_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR3_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR3_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR3_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR3_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR3_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR3_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR3_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR3_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR3_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR3_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR3_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR3_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR3_RESET_VALUE = 0x0;


    static const uint32_t CPAR3_RESET_VALUE = 0x0;


    static const uint32_t CMAR3_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR4_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR4_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR4_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR4_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR4_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR4_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR4_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR4_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR4_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR4_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR4_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR4_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR4_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR4_RESET_VALUE = 0x0;


    static const uint32_t CPAR4_RESET_VALUE = 0x0;


    static const uint32_t CMAR4_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR5_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR5_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR5_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR5_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR5_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR5_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR5_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR5_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR5_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR5_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR5_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR5_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR5_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR5_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR5_RESET_VALUE = 0x0;


    static const uint32_t CPAR5_RESET_VALUE = 0x0;


    static const uint32_t CMAR5_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR6_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR6_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR6_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR6_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR6_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR6_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR6_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR6_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR6_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR6_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR6_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR6_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR6_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR6_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR6_RESET_VALUE = 0x0;


    static const uint32_t CPAR6_RESET_VALUE = 0x0;


    static const uint32_t CMAR6_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR7_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR7_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR7_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR7_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR7_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR7_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR7_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR7_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR7_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR7_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR7_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR7_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR7_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR7_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR7_RESET_VALUE = 0x0;


    static const uint32_t CPAR7_RESET_VALUE = 0x0;


    static const uint32_t CMAR7_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR8_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR8_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR8_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR8_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR8_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR8_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR8_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR8_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR8_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR8_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR8_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR8_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR8_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR8_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR8_RESET_VALUE = 0x0;


    static const uint32_t CPAR8_RESET_VALUE = 0x0;


    static const uint32_t CMAR8_RESET_VALUE = 0x0;

    static constexpr uint8_t DMA1_CH1 = 11; // DMA1 channel 1 interrupt
    static constexpr uint8_t DMA1_CH2 = 12; // DMA1 channel 2 interrupt
    static constexpr uint8_t DMA1_CH3 = 13; // DMA1 channel 3 interrupt
    static constexpr uint8_t DMA1_CH4 = 14; // DMA1 channel 4 interrupt
    static constexpr uint8_t DMA1_CH5 = 15; // DMA1 channel 5 interrupt
    static constexpr uint8_t DMA1_CH6 = 16; // DMA1 channel 6 interrupt
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
    volatile uint32_t    ISR;                  // [Read-only] interrupt status register
    volatile uint32_t    IFCR;                 // [Write-only] DMA interrupt flag clear register
    volatile uint32_t    CCR1;                 // [Read-write] DMA channel 1 configuration register
    volatile uint32_t    CNDTR1;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR1;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR1;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _0;
    volatile uint32_t    CCR2;                 // [Read-write] DMA channel 2 configuration register
    volatile uint32_t    CNDTR2;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR2;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR2;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _1;
    volatile uint32_t    CCR3;                 // [Read-write] DMA channel 3 configuration register
    volatile uint32_t    CNDTR3;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR3;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR3;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _2;
    volatile uint32_t    CCR4;                 // [Read-write] DMA channel 3 configuration register
    volatile uint32_t    CNDTR4;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR4;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR4;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _3;
    volatile uint32_t    CCR5;                 // [Read-write] DMA channel 4 configuration register
    volatile uint32_t    CNDTR5;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR5;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR5;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _4;
    volatile uint32_t    CCR6;                 // [Read-write] DMA channel 5 configuration register
    volatile uint32_t    CNDTR6;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR6;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR6;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _5;
    volatile uint32_t    CCR7;                 // [Read-write] DMA channel 6 configuration register
    volatile uint32_t    CNDTR7;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR7;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR7;                // [Read-write] DMA channel x memory address register
    reserved_t<1>        _6;
    volatile uint32_t    CCR8;                 // [Read-write] DMA channel 7 configuration register
    volatile uint32_t    CNDTR8;               // [Read-write] channel x number of data to transfer register
    volatile uint32_t    CPAR8;                // [Read-write] DMA channel x peripheral address register
    volatile uint32_t    CMAR8;                // [Read-write] DMA channel x memory address register

    static constexpr uint32_t ISR_TEIF8 = 0x80000000;   // TEIF8
    static constexpr uint32_t ISR_HTIF8 = 0x40000000;   // HTIF8
    static constexpr uint32_t ISR_TCIF8 = 0x20000000;   // TCIF8
    static constexpr uint32_t ISR_GIF8 = 0x10000000;    // GIF8
    static constexpr uint32_t ISR_TEIF7 = 0x8000000;    // TEIF7
    static constexpr uint32_t ISR_HTIF7 = 0x4000000;    // HTIF7
    static constexpr uint32_t ISR_TCIF7 = 0x2000000;    // TCIF7
    static constexpr uint32_t ISR_GIF7 = 0x1000000;     // GIF7
    static constexpr uint32_t ISR_TEIF6 = 0x800000;     // TEIF6
    static constexpr uint32_t ISR_HTIF6 = 0x400000;     // HTIF6
    static constexpr uint32_t ISR_TCIF6 = 0x200000;     // TCIF6
    static constexpr uint32_t ISR_GIF6 = 0x100000;      // GIF6
    static constexpr uint32_t ISR_TEIF5 = 0x80000;      // TEIF5
    static constexpr uint32_t ISR_HTIF5 = 0x40000;      // HTIF5
    static constexpr uint32_t ISR_TCIF5 = 0x20000;      // TCIF5
    static constexpr uint32_t ISR_GIF5 = 0x10000;       // GIF5
    static constexpr uint32_t ISR_TEIF4 = 0x8000;       // TEIF4
    static constexpr uint32_t ISR_HTIF4 = 0x4000;       // HTIF4
    static constexpr uint32_t ISR_TCIF4 = 0x2000;       // TCIF4
    static constexpr uint32_t ISR_GIF4 = 0x1000;        // GIF4
    static constexpr uint32_t ISR_TEIF3 = 0x800;        // TEIF3
    static constexpr uint32_t ISR_HTIF3 = 0x400;        // HTIF3
    static constexpr uint32_t ISR_TCIF3 = 0x200;        // TCIF3
    static constexpr uint32_t ISR_GIF3 = 0x100;         // GIF3
    static constexpr uint32_t ISR_TEIF2 = 0x80;         // TEIF2
    static constexpr uint32_t ISR_HTIF2 = 0x40;         // HTIF2
    static constexpr uint32_t ISR_TCIF2 = 0x20;         // TCIF2
    static constexpr uint32_t ISR_GIF2 = 0x10;          // GIF2
    static constexpr uint32_t ISR_TEIF1 = 0x8;          // TEIF1
    static constexpr uint32_t ISR_HTIF1 = 0x4;          // HTIF1
    static constexpr uint32_t ISR_TCIF1 = 0x2;          // TCIF1
    static constexpr uint32_t ISR_GIF1 = 0x1;           // GIF1
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t IFCR_TEIF8 = 0x80000000;   // TEIF8
    static constexpr uint32_t IFCR_HTIF8 = 0x40000000;   // HTIF8
    static constexpr uint32_t IFCR_TCIF8 = 0x20000000;   // TCIF8
    static constexpr uint32_t IFCR_GIF8 = 0x10000000;    // GIF8
    static constexpr uint32_t IFCR_TEIF7 = 0x8000000;    // TEIF7
    static constexpr uint32_t IFCR_HTIF7 = 0x4000000;    // HTIF7
    static constexpr uint32_t IFCR_TCIF7 = 0x2000000;    // TCIF7
    static constexpr uint32_t IFCR_GIF7 = 0x1000000;     // GIF7
    static constexpr uint32_t IFCR_TEIF6 = 0x800000;     // TEIF6
    static constexpr uint32_t IFCR_HTIF6 = 0x400000;     // HTIF6
    static constexpr uint32_t IFCR_TCIF6 = 0x200000;     // TCIF6
    static constexpr uint32_t IFCR_GIF6 = 0x100000;      // GIF6
    static constexpr uint32_t IFCR_TEIF5 = 0x80000;      // TEIF5
    static constexpr uint32_t IFCR_HTIF5 = 0x40000;      // HTIF5
    static constexpr uint32_t IFCR_TCIF5 = 0x20000;      // TCIF5
    static constexpr uint32_t IFCR_GIF5 = 0x10000;       // GIF5
    static constexpr uint32_t IFCR_TEIF4 = 0x8000;       // TEIF4
    static constexpr uint32_t IFCR_HTIF4 = 0x4000;       // HTIF4
    static constexpr uint32_t IFCR_TCIF4 = 0x2000;       // TCIF4
    static constexpr uint32_t IFCR_GIF4 = 0x1000;        // GIF4
    static constexpr uint32_t IFCR_TEIF3 = 0x800;        // TEIF3
    static constexpr uint32_t IFCR_HTIF3 = 0x400;        // HTIF3
    static constexpr uint32_t IFCR_TCIF3 = 0x200;        // TCIF3
    static constexpr uint32_t IFCR_GIF3 = 0x100;         // GIF3
    static constexpr uint32_t IFCR_TEIF2 = 0x80;         // TEIF2
    static constexpr uint32_t IFCR_HTIF2 = 0x40;         // HTIF2
    static constexpr uint32_t IFCR_TCIF2 = 0x20;         // TCIF2
    static constexpr uint32_t IFCR_GIF2 = 0x10;          // GIF2
    static constexpr uint32_t IFCR_TEIF1 = 0x8;          // TEIF1
    static constexpr uint32_t IFCR_HTIF1 = 0x4;          // HTIF1
    static constexpr uint32_t IFCR_TCIF1 = 0x2;          // TCIF1
    static constexpr uint32_t IFCR_GIF1 = 0x1;           // GIF1
    static const uint32_t IFCR_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR1_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR1_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR1_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR1_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR1_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR1_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR1_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR1_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR1_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR1_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR1_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR1_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR1_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR1_RESET_VALUE = 0x0;


    static const uint32_t CPAR1_RESET_VALUE = 0x0;


    static const uint32_t CMAR1_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR2_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR2_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR2_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR2_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR2_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR2_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR2_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR2_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR2_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR2_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR2_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR2_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR2_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR2_RESET_VALUE = 0x0;


    static const uint32_t CPAR2_RESET_VALUE = 0x0;


    static const uint32_t CMAR2_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR3_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR3_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR3_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR3_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR3_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR3_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR3_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR3_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR3_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR3_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR3_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR3_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR3_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR3_RESET_VALUE = 0x0;


    static const uint32_t CPAR3_RESET_VALUE = 0x0;


    static const uint32_t CMAR3_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR4_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR4_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR4_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR4_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR4_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR4_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR4_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR4_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR4_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR4_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR4_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR4_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR4_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR4_RESET_VALUE = 0x0;


    static const uint32_t CPAR4_RESET_VALUE = 0x0;


    static const uint32_t CMAR4_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR5_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR5_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR5_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR5_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR5_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR5_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR5_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR5_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR5_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR5_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR5_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR5_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR5_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR5_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR5_RESET_VALUE = 0x0;


    static const uint32_t CPAR5_RESET_VALUE = 0x0;


    static const uint32_t CMAR5_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR6_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR6_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR6_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR6_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR6_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR6_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR6_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR6_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR6_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR6_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR6_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR6_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR6_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR6_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR6_RESET_VALUE = 0x0;


    static const uint32_t CPAR6_RESET_VALUE = 0x0;


    static const uint32_t CMAR6_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR7_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR7_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR7_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR7_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR7_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR7_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR7_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR7_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR7_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR7_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR7_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR7_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR7_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR7_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR7_RESET_VALUE = 0x0;


    static const uint32_t CPAR7_RESET_VALUE = 0x0;


    static const uint32_t CMAR7_RESET_VALUE = 0x0;

    static constexpr uint32_t CCR8_EN = 0x1;             // channel enable
    static constexpr uint32_t CCR8_TCIE = 0x2;           // TCIE
    static constexpr uint32_t CCR8_HTIE = 0x4;           // HTIE
    static constexpr uint32_t CCR8_TEIE = 0x8;           // TEIE
    static constexpr uint32_t CCR8_DIR = 0x10;           // DIR
    static constexpr uint32_t CCR8_CIRC = 0x20;          // CIRC
    static constexpr uint32_t CCR8_PINC = 0x40;          // PINC
    static constexpr uint32_t CCR8_MINC = 0x80;          // MINC
    template<uint32_t X>
    static constexpr uint32_t CCR8_PSIZE =               // PSIZE (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR8_MSIZE =               // MSIZE (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR8_PL =                  // PL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t CCR8_MEM2MEM = 0x4000;     // MEM2MEM
    static const uint32_t CCR8_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CNDTR8_NDT =                 // Number of data items to transfer (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CNDTR8_RESET_VALUE = 0x0;


    static const uint32_t CPAR8_RESET_VALUE = 0x0;


    static const uint32_t CMAR8_RESET_VALUE = 0x0;

    static constexpr uint8_t DMA2_CH1 = 56; // DMA2_CH1
    static constexpr uint8_t DMA2_CH2 = 57; // DMA2_CH2
    static constexpr uint8_t DMA2_CH3 = 58; // DMA2_CH3
    static constexpr uint8_t DMA2_CH4 = 59; // DMA2_CH4
    static constexpr uint8_t DMA2_CH5 = 60; // DMA2_CH5
    static constexpr uint8_t DMA2_CH6 = 97; // DMA2_CH6
};

static dma2_t& DMA2 = *reinterpret_cast<dma2_t*>(0x40020400);

#define HAVE_PERIPHERAL_DMA2


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
    volatile uint32_t    C7CR;                 // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C8CR;                 // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C9CR;                 // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C10CR;                // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C11CR;                // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C12CR;                // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C13CR;                // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C14CR;                // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    volatile uint32_t    C15CR;                // [Read-write] DMAMux - DMA request line multiplexer channel x control register
    reserved_t<16>       _0;
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

    template<uint32_t X>
    static constexpr uint32_t C0CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
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
    static constexpr uint32_t C1CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
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
    static constexpr uint32_t C2CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
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
    static constexpr uint32_t C3CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
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
    static constexpr uint32_t C4CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
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
    static constexpr uint32_t C5CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
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
    static constexpr uint32_t C6CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
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
    static constexpr uint32_t C7CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t C7CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C7CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C7CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C7CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C7CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C7CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C7CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C8CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t C8CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C8CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C8CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C8CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C8CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C8CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C8CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C9CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t C9CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C9CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C9CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C9CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C9CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C9CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C9CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C10CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t C10CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C10CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C10CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C10CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C10CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C10CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C10CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C11CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t C11CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C11CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C11CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C11CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C11CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C11CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C11CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C12CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t C12CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C12CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C12CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C12CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C12CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C12CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C12CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C13CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t C13CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C13CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C13CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C13CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C13CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C13CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C13CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C14CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t C14CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C14CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C14CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C14CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C14CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C14CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C14CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t C15CR_DMAREQ_ID =           // Input DMA request line selected (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t C15CR_SOIE = 0x100;         // Interrupt enable at synchronization event overrun
    static constexpr uint32_t C15CR_EGE = 0x200;          // Event generation enable/disable
    static constexpr uint32_t C15CR_SE = 0x10000;         // Synchronous operating mode enable/disable
    template<uint32_t X>
    static constexpr uint32_t C15CR_SPOL =                // Synchronization event type selector Defines the synchronization event on the selected synchronization input: (2 bits)
        bit_field_t<17, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C15CR_NBREQ =               // Number of DMA requests to forward Defines the number of DMA requests forwarded before output event is generated. In synchronous mode, it also defines the number of DMA requests to forward after a synchronization event, then stop forwarding. The actual number of DMA requests forwarded is NBREQ+1. Note: This field can only be written when both SE and EGE bits are reset. (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t C15CR_SYNC_ID =             // Synchronization input selected (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static const uint32_t C15CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CSR_SOF =                 // Synchronization overrun event flag (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CSR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CFR_CSOF =                // Clear synchronization overrun event flag (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
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

    static constexpr uint8_t DMAMUX_OVR = 94; // DMAMUX_OVR
};

static dmamux_t& DMAMUX = *reinterpret_cast<dmamux_t*>(0x40020800);

#define HAVE_PERIPHERAL_DMAMUX


////
//
//    System configuration controller
//
////

struct syscfg_t
{
    volatile uint32_t    MEMRMP;               // [Read-write] Remap Memory register
    volatile uint32_t    CFGR1;                // [Read-write] peripheral mode configuration register
    volatile uint32_t    EXTICR1;              // [Read-write] external interrupt configuration register 1
    volatile uint32_t    EXTICR2;              // [Read-write] external interrupt configuration register 2
    volatile uint32_t    EXTICR3;              // [Read-write] external interrupt configuration register 3
    volatile uint32_t    EXTICR4;              // [Read-write] external interrupt configuration register 4
    volatile uint32_t    SCSR;                 // CCM SRAM control and status register
    volatile uint32_t    CFGR2;                // [Read-write] configuration register 2
    volatile uint32_t    SWPR;                 // [Read-write] SRAM Write protection register 1
    volatile uint32_t    SKR;                  // [Write-only] SRAM2 Key Register

    template<uint32_t X>
    static constexpr uint32_t MEMRMP_MEM_MODE =            // Memory mapping selection (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t MEMRMP_FB_mode = 0x100;      // User Flash Bank mode
    static const uint32_t MEMRMP_RESET_VALUE = 0x0;

    static constexpr uint32_t CFGR1_BOOSTEN = 0x100;      // BOOSTEN
    static constexpr uint32_t CFGR1_ANASWVDD = 0x200;     // GPIO analog switch control voltage selection
    static constexpr uint32_t CFGR1_I2C_PB6_FMP = 0x10000;// FM+ drive capability on PB6
    static constexpr uint32_t CFGR1_I2C_PB7_FMP = 0x20000;// FM+ drive capability on PB6
    static constexpr uint32_t CFGR1_I2C_PB8_FMP = 0x40000;// FM+ drive capability on PB6
    static constexpr uint32_t CFGR1_I2C_PB9_FMP = 0x80000;// FM+ drive capability on PB6
    static constexpr uint32_t CFGR1_I2C1_FMP = 0x100000;  // I2C1 FM+ drive capability enable
    static constexpr uint32_t CFGR1_I2C2_FMP = 0x200000;  // I2C1 FM+ drive capability enable
    static constexpr uint32_t CFGR1_I2C3_FMP = 0x400000;  // I2C1 FM+ drive capability enable
    static constexpr uint32_t CFGR1_I2C4_FMP = 0x800000;  // I2C1 FM+ drive capability enable
    template<uint32_t X>
    static constexpr uint32_t CFGR1_FPU_IE =              // FPU Interrupts Enable (6 bits)
        bit_field_t<26, 0x3f>::value<X>();
    static const uint32_t CFGR1_RESET_VALUE = 0x7c000001;

    template<uint32_t X>
    static constexpr uint32_t EXTICR1_EXTI3 =               // EXTI x configuration (x = 0 to 3) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR1_EXTI2 =               // EXTI x configuration (x = 0 to 3) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR1_EXTI1 =               // EXTI x configuration (x = 0 to 3) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR1_EXTI0 =               // EXTI x configuration (x = 0 to 3) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t EXTICR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EXTICR2_EXTI7 =               // EXTI x configuration (x = 4 to 7) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR2_EXTI6 =               // EXTI x configuration (x = 4 to 7) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR2_EXTI5 =               // EXTI x configuration (x = 4 to 7) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR2_EXTI4 =               // EXTI x configuration (x = 4 to 7) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t EXTICR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EXTICR3_EXTI11 =              // EXTI x configuration (x = 8 to 11) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR3_EXTI10 =              // EXTI10 (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR3_EXTI9 =               // EXTI x configuration (x = 8 to 11) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR3_EXTI8 =               // EXTI x configuration (x = 8 to 11) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t EXTICR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EXTICR4_EXTI15 =              // EXTI x configuration (x = 12 to 15) (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR4_EXTI14 =              // EXTI x configuration (x = 12 to 15) (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR4_EXTI13 =              // EXTI x configuration (x = 12 to 15) (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EXTICR4_EXTI12 =              // EXTI x configuration (x = 12 to 15) (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t EXTICR4_RESET_VALUE = 0x0;

    static constexpr uint32_t SCSR_CCMER = 0x1;          // CCM SRAM Erase, Read-write
    static constexpr uint32_t SCSR_CCMBSY = 0x2;         // CCM SRAM busy by erase operation, Read-only
    static const uint32_t SCSR_RESET_VALUE = 0x0;

    static constexpr uint32_t CFGR2_CLL = 0x1;            // Core Lockup Lock
    static constexpr uint32_t CFGR2_SPL = 0x2;            // SRAM Parity Lock
    static constexpr uint32_t CFGR2_PVDL = 0x4;           // PVD Lock
    static constexpr uint32_t CFGR2_ECCL = 0x8;           // ECC Lock
    static constexpr uint32_t CFGR2_SPF = 0x100;          // SRAM Parity Flag
    static const uint32_t CFGR2_RESET_VALUE = 0x0;

    static constexpr uint32_t SWPR_Page0_WP = 0x1;       // Write protection
    static constexpr uint32_t SWPR_Page1_WP = 0x2;       // Write protection
    static constexpr uint32_t SWPR_Page2_WP = 0x4;       // Write protection
    static constexpr uint32_t SWPR_Page3_WP = 0x8;       // Write protection
    static constexpr uint32_t SWPR_Page4_WP = 0x10;      // Write protection
    static constexpr uint32_t SWPR_Page5_WP = 0x20;      // Write protection
    static constexpr uint32_t SWPR_Page6_WP = 0x40;      // Write protection
    static constexpr uint32_t SWPR_Page7_WP = 0x80;      // Write protection
    static constexpr uint32_t SWPR_Page8_WP = 0x100;     // Write protection
    static constexpr uint32_t SWPR_Page9_WP = 0x200;     // Write protection
    static constexpr uint32_t SWPR_Page10_WP = 0x400;    // Write protection
    static constexpr uint32_t SWPR_Page11_WP = 0x800;    // Write protection
    static constexpr uint32_t SWPR_Page12_WP = 0x1000;   // Write protection
    static constexpr uint32_t SWPR_Page13_WP = 0x2000;   // Write protection
    static constexpr uint32_t SWPR_Page14_WP = 0x4000;   // Write protection
    static constexpr uint32_t SWPR_Page15_WP = 0x8000;   // Write protection
    static constexpr uint32_t SWPR_Page16_WP = 0x10000;  // Write protection
    static constexpr uint32_t SWPR_Page17_WP = 0x20000;  // Write protection
    static constexpr uint32_t SWPR_Page18_WP = 0x40000;  // Write protection
    static constexpr uint32_t SWPR_Page19_WP = 0x80000;  // Write protection
    static constexpr uint32_t SWPR_Page20_WP = 0x100000; // Write protection
    static constexpr uint32_t SWPR_Page21_WP = 0x200000; // Write protection
    static constexpr uint32_t SWPR_Page22_WP = 0x400000; // Write protection
    static constexpr uint32_t SWPR_Page23_WP = 0x800000; // Write protection
    static constexpr uint32_t SWPR_Page24_WP = 0x1000000;// Write protection
    static constexpr uint32_t SWPR_Page25_WP = 0x2000000;// Write protection
    static constexpr uint32_t SWPR_Page26_WP = 0x4000000;// Write protection
    static constexpr uint32_t SWPR_Page27_WP = 0x8000000;// Write protection
    static constexpr uint32_t SWPR_Page28_WP = 0x10000000;// Write protection
    static constexpr uint32_t SWPR_Page29_WP = 0x20000000;// Write protection
    static constexpr uint32_t SWPR_Page30_WP = 0x40000000;// Write protection
    static constexpr uint32_t SWPR_Page31_WP = 0x80000000;// Write protection
    static const uint32_t SWPR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SKR_KEY =                 // SRAM2 Key for software erase (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t SKR_RESET_VALUE = 0x0;
};

static syscfg_t& SYSCFG = *reinterpret_cast<syscfg_t*>(0x40010000);

#define HAVE_PERIPHERAL_SYSCFG


////
//
//    Voltage reference buffer
//
////

struct vrefbuf_t
{
    volatile uint32_t    VREFBUF_CSR;          // VREF_BUF Control and Status Register
    volatile uint32_t    VREFBUF_CCR;          // [Read-write] VREF_BUF Calibration Control Register

    static constexpr uint32_t VREFBUF_CSR_ENVR = 0x1;           // Enable Voltage Reference, Read-write
    static constexpr uint32_t VREFBUF_CSR_HIZ = 0x2;            // High impedence mode for the VREF_BUF, Read-write
    static constexpr uint32_t VREFBUF_CSR_VRR = 0x8;            // Voltage reference buffer ready, Read-only
    template<uint32_t X>
    static constexpr uint32_t VREFBUF_CSR_VRS =                 // Voltage reference scale (2 bits), Read-write
        bit_field_t<4, 0x3>::value<X>();
    static const uint32_t VREFBUF_CSR_RESET_VALUE = 0x2;

    template<uint32_t X>
    static constexpr uint32_t VREFBUF_CCR_TRIM =                // Trimming code (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    static const uint32_t VREFBUF_CCR_RESET_VALUE = 0x0;
};

static vrefbuf_t& VREFBUF = *reinterpret_cast<vrefbuf_t*>(0x40010030);

#define HAVE_PERIPHERAL_VREFBUF


////
//
//    Comparator control and status register
//
////

struct comp_t
{
    volatile uint32_t    COMP_C1CSR;           // Comparator control/status register
    volatile uint32_t    COMP_C2CSR;           // Comparator control/status register
    volatile uint32_t    COMP_C3CSR;           // Comparator control/status register
    reserved_t<1>        _0;
    volatile uint32_t    COMP_C4CSR;           // Comparator control/status register

    static constexpr uint32_t COMP_C1CSR_EN = 0x1;             // EN, Read-write
    static constexpr uint32_t COMP_C1CSR_COMP_DEGLITCH_EN = 0x2;// COMP_DEGLITCH_EN, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_C1CSR_INMSEL =              // INMSEL (3 bits), Read-write
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t COMP_C1CSR_INPSEL = 0x100;       // INPSEL, Read-write
    static constexpr uint32_t COMP_C1CSR_POL = 0x8000;         // POL, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_C1CSR_HYST =                // HYST (3 bits), Read-write
        bit_field_t<16, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP_C1CSR_BLANKSEL =            // BLANKSEL (3 bits), Read-write
        bit_field_t<19, 0x7>::value<X>();
    static constexpr uint32_t COMP_C1CSR_BRGEN = 0x400000;     // BRGEN, Read-write
    static constexpr uint32_t COMP_C1CSR_SCALEN = 0x800000;    // SCALEN, Read-write
    static constexpr uint32_t COMP_C1CSR_VALUE = 0x40000000;   // VALUE, Read-only
    static constexpr uint32_t COMP_C1CSR_LOCK = 0x80000000;    // LOCK, Read-write
    static const uint32_t COMP_C1CSR_RESET_VALUE = 0x0;

    static constexpr uint32_t COMP_C2CSR_EN = 0x1;             // EN, Read-write
    static constexpr uint32_t COMP_C2CSR_COMP_DEGLITCH_EN = 0x2;// COMP_DEGLITCH_EN, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_C2CSR_INMSEL =              // INMSEL (3 bits), Read-write
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t COMP_C2CSR_INPSEL = 0x100;       // INPSEL, Read-write
    static constexpr uint32_t COMP_C2CSR_POL = 0x8000;         // POL, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_C2CSR_HYST =                // HYST (3 bits), Read-write
        bit_field_t<16, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP_C2CSR_BLANKSEL =            // BLANKSEL (3 bits), Read-write
        bit_field_t<19, 0x7>::value<X>();
    static constexpr uint32_t COMP_C2CSR_BRGEN = 0x400000;     // BRGEN, Read-write
    static constexpr uint32_t COMP_C2CSR_SCALEN = 0x800000;    // SCALEN, Read-write
    static constexpr uint32_t COMP_C2CSR_VALUE = 0x40000000;   // VALUE, Read-only
    static constexpr uint32_t COMP_C2CSR_LOCK = 0x80000000;    // LOCK, Read-write
    static const uint32_t COMP_C2CSR_RESET_VALUE = 0x0;

    static constexpr uint32_t COMP_C3CSR_EN = 0x1;             // EN, Read-write
    static constexpr uint32_t COMP_C3CSR_COMP_DEGLITCH_EN = 0x2;// COMP_DEGLITCH_EN, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_C3CSR_INMSEL =              // INMSEL (3 bits), Read-write
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t COMP_C3CSR_INPSEL = 0x100;       // INPSEL, Read-write
    static constexpr uint32_t COMP_C3CSR_POL = 0x8000;         // POL, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_C3CSR_HYST =                // HYST (3 bits), Read-write
        bit_field_t<16, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP_C3CSR_BLANKSEL =            // BLANKSEL (3 bits), Read-write
        bit_field_t<19, 0x7>::value<X>();
    static constexpr uint32_t COMP_C3CSR_BRGEN = 0x400000;     // BRGEN, Read-write
    static constexpr uint32_t COMP_C3CSR_SCALEN = 0x800000;    // SCALEN, Read-write
    static constexpr uint32_t COMP_C3CSR_VALUE = 0x40000000;   // VALUE, Read-only
    static constexpr uint32_t COMP_C3CSR_LOCK = 0x80000000;    // LOCK, Read-write
    static const uint32_t COMP_C3CSR_RESET_VALUE = 0x0;

    static constexpr uint32_t COMP_C4CSR_EN = 0x1;             // EN, Read-write
    static constexpr uint32_t COMP_C4CSR_COMP_DEGLITCH_EN = 0x2;// COMP_DEGLITCH_EN, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_C4CSR_INMSEL =              // INMSEL (3 bits), Read-write
        bit_field_t<4, 0x7>::value<X>();
    static constexpr uint32_t COMP_C4CSR_INPSEL = 0x100;       // INPSEL, Read-write
    static constexpr uint32_t COMP_C4CSR_POL = 0x8000;         // POL, Read-write
    template<uint32_t X>
    static constexpr uint32_t COMP_C4CSR_HYST =                // HYST (3 bits), Read-write
        bit_field_t<16, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t COMP_C4CSR_BLANKSEL =            // BLANKSEL (3 bits), Read-write
        bit_field_t<19, 0x7>::value<X>();
    static constexpr uint32_t COMP_C4CSR_BRGEN = 0x400000;     // BRGEN, Read-write
    static constexpr uint32_t COMP_C4CSR_SCALEN = 0x800000;    // SCALEN, Read-write
    static constexpr uint32_t COMP_C4CSR_VALUE = 0x40000000;   // VALUE, Read-only
    static constexpr uint32_t COMP_C4CSR_LOCK = 0x80000000;    // LOCK, Read-write
    static const uint32_t COMP_C4CSR_RESET_VALUE = 0x0;

    static constexpr uint8_t COMP1_2_3 = 64; // COMP1_2_3
    static constexpr uint8_t COMP4 = 65; // COMP4_5_6
};

static comp_t& COMP = *reinterpret_cast<comp_t*>(0x40010200);

#define HAVE_PERIPHERAL_COMP


////
//
//    Operational amplifiers
//
////

struct opamp_t
{
    volatile uint32_t    OPAMP1_CSR;           // [Read-write] OPAMP1 control/status register
    volatile uint32_t    OPAMP2_CSR;           // [Read-write] OPAMP2 control/status register
    volatile uint32_t    OPAMP3_CSR;           // [Read-write] OPAMP3 control/status register
    reserved_t<3>        _0;
    volatile uint32_t    OPAMP1_TCMR;          // [Read-write] OPAMP1 control/status register
    volatile uint32_t    OPAMP2_TCMR;          // [Read-write] OPAMP2 control/status register
    volatile uint32_t    OPAMP3_TCMR;          // [Read-write] OPAMP3 control/status register

    static constexpr uint32_t OPAMP1_CSR_OPAEN = 0x1;          // Operational amplifier Enable
    static constexpr uint32_t OPAMP1_CSR_FORCE_VP = 0x2;       // FORCE_VP
    template<uint32_t X>
    static constexpr uint32_t OPAMP1_CSR_VP_SEL =              // VP_SEL (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    static constexpr uint32_t OPAMP1_CSR_USERTRIM = 0x10;      // USERTRIM
    template<uint32_t X>
    static constexpr uint32_t OPAMP1_CSR_VM_SEL =              // VM_SEL (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t OPAMP1_CSR_OPAHSM = 0x80;        // OPAHSM
    static constexpr uint32_t OPAMP1_CSR_OPAINTOEN = 0x100;    // OPAINTOEN
    static constexpr uint32_t OPAMP1_CSR_CALON = 0x800;        // CALON
    template<uint32_t X>
    static constexpr uint32_t OPAMP1_CSR_CALSEL =              // CALSEL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OPAMP1_CSR_PGA_GAIN =            // PGA_GAIN (5 bits)
        bit_field_t<14, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OPAMP1_CSR_TRIMOFFSETP =         // TRIMOFFSETP (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OPAMP1_CSR_TRIMOFFSETN =         // TRIMOFFSETN (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static constexpr uint32_t OPAMP1_CSR_CALOUT = 0x40000000;  // CALOUT
    static constexpr uint32_t OPAMP1_CSR_LOCK = 0x80000000;    // LOCK
    static const uint32_t OPAMP1_CSR_RESET_VALUE = 0x0;

    static constexpr uint32_t OPAMP2_CSR_OPAEN = 0x1;          // Operational amplifier Enable
    static constexpr uint32_t OPAMP2_CSR_FORCE_VP = 0x2;       // FORCE_VP
    template<uint32_t X>
    static constexpr uint32_t OPAMP2_CSR_VP_SEL =              // VP_SEL (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    static constexpr uint32_t OPAMP2_CSR_USERTRIM = 0x10;      // USERTRIM
    template<uint32_t X>
    static constexpr uint32_t OPAMP2_CSR_VM_SEL =              // VM_SEL (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t OPAMP2_CSR_OPAHSM = 0x80;        // OPAHSM
    static constexpr uint32_t OPAMP2_CSR_OPAINTOEN = 0x100;    // OPAINTOEN
    static constexpr uint32_t OPAMP2_CSR_CALON = 0x800;        // CALON
    template<uint32_t X>
    static constexpr uint32_t OPAMP2_CSR_CALSEL =              // CALSEL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OPAMP2_CSR_PGA_GAIN =            // PGA_GAIN (5 bits)
        bit_field_t<14, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OPAMP2_CSR_TRIMOFFSETP =         // TRIMOFFSETP (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OPAMP2_CSR_TRIMOFFSETN =         // TRIMOFFSETN (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static constexpr uint32_t OPAMP2_CSR_CALOUT = 0x40000000;  // CALOUT
    static constexpr uint32_t OPAMP2_CSR_LOCK = 0x80000000;    // LOCK
    static const uint32_t OPAMP2_CSR_RESET_VALUE = 0x0;

    static constexpr uint32_t OPAMP3_CSR_OPAEN = 0x1;          // Operational amplifier Enable
    static constexpr uint32_t OPAMP3_CSR_FORCE_VP = 0x2;       // FORCE_VP
    template<uint32_t X>
    static constexpr uint32_t OPAMP3_CSR_VP_SEL =              // VP_SEL (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    static constexpr uint32_t OPAMP3_CSR_USERTRIM = 0x10;      // USERTRIM
    template<uint32_t X>
    static constexpr uint32_t OPAMP3_CSR_VM_SEL =              // VM_SEL (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t OPAMP3_CSR_OPAHSM = 0x80;        // OPAHSM
    static constexpr uint32_t OPAMP3_CSR_OPAINTOEN = 0x100;    // OPAINTOEN
    static constexpr uint32_t OPAMP3_CSR_CALON = 0x800;        // CALON
    template<uint32_t X>
    static constexpr uint32_t OPAMP3_CSR_CALSEL =              // CALSEL (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OPAMP3_CSR_PGA_GAIN =            // PGA_GAIN (5 bits)
        bit_field_t<14, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OPAMP3_CSR_TRIMOFFSETP =         // TRIMOFFSETP (5 bits)
        bit_field_t<19, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t OPAMP3_CSR_TRIMOFFSETN =         // TRIMOFFSETN (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    static constexpr uint32_t OPAMP3_CSR_CALOUT = 0x40000000;  // CALOUT
    static constexpr uint32_t OPAMP3_CSR_LOCK = 0x80000000;    // LOCK
    static const uint32_t OPAMP3_CSR_RESET_VALUE = 0x0;

    static constexpr uint32_t OPAMP1_TCMR_VMS_SEL = 0x1;        // VMS_SEL
    template<uint32_t X>
    static constexpr uint32_t OPAMP1_TCMR_VPS_SEL =             // VPS_SEL (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t OPAMP1_TCMR_T1CM_EN = 0x8;        // T1CM_EN
    static constexpr uint32_t OPAMP1_TCMR_T8CM_EN = 0x10;       // T8CM_EN
    static constexpr uint32_t OPAMP1_TCMR_T20CM_EN = 0x20;      // T20CM_EN
    static constexpr uint32_t OPAMP1_TCMR_LOCK = 0x80000000;    // LOCK
    static const uint32_t OPAMP1_TCMR_RESET_VALUE = 0x0;

    static constexpr uint32_t OPAMP2_TCMR_VMS_SEL = 0x1;        // VMS_SEL
    template<uint32_t X>
    static constexpr uint32_t OPAMP2_TCMR_VPS_SEL =             // VPS_SEL (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t OPAMP2_TCMR_T1CM_EN = 0x8;        // T1CM_EN
    static constexpr uint32_t OPAMP2_TCMR_T8CM_EN = 0x10;       // T8CM_EN
    static constexpr uint32_t OPAMP2_TCMR_T20CM_EN = 0x20;      // T20CM_EN
    static constexpr uint32_t OPAMP2_TCMR_LOCK = 0x80000000;    // LOCK
    static const uint32_t OPAMP2_TCMR_RESET_VALUE = 0x0;

    static constexpr uint32_t OPAMP3_TCMR_VMS_SEL = 0x1;        // VMS_SEL
    template<uint32_t X>
    static constexpr uint32_t OPAMP3_TCMR_VPS_SEL =             // VPS_SEL (2 bits)
        bit_field_t<1, 0x3>::value<X>();
    static constexpr uint32_t OPAMP3_TCMR_T1CM_EN = 0x8;        // T1CM_EN
    static constexpr uint32_t OPAMP3_TCMR_T8CM_EN = 0x10;       // T8CM_EN
    static constexpr uint32_t OPAMP3_TCMR_T20CM_EN = 0x20;      // T20CM_EN
    static constexpr uint32_t OPAMP3_TCMR_LOCK = 0x80000000;    // LOCK
    static const uint32_t OPAMP3_TCMR_RESET_VALUE = 0x0;
};

static opamp_t& OPAMP = *reinterpret_cast<opamp_t*>(0x40010300);

#define HAVE_PERIPHERAL_OPAMP


////
//
//    Digital-to-analog converter
//
////

struct dac1_t
{
    volatile uint32_t    DAC_CR;               // [Read-write] DAC control register
    volatile uint32_t    DAC_SWTRGR;           // [Write-only] DAC software trigger register
    volatile uint32_t    DAC_DHR12R1;          // [Read-write] DAC channel1 12-bit right-aligned data holding register
    volatile uint32_t    DAC_DHR12L1;          // [Read-write] DAC channel1 12-bit left aligned data holding register
    volatile uint32_t    DAC_DHR8R1;           // [Read-write] DAC channel1 8-bit right aligned data holding register
    volatile uint32_t    DAC_DHR12R2;          // [Read-write] DAC channel2 12-bit right aligned data holding register
    volatile uint32_t    DAC_DHR12L2;          // [Read-write] DAC channel2 12-bit left aligned data holding register
    volatile uint32_t    DAC_DHR8R2;           // [Read-write] DAC channel2 8-bit right-aligned data holding register
    volatile uint32_t    DAC_DHR12RD;          // [Read-write] Dual DAC 12-bit right-aligned data holding register
    volatile uint32_t    DAC_DHR12LD;          // [Read-write] DUAL DAC 12-bit left aligned data holding register
    volatile uint32_t    DAC_DHR8RD;           // [Read-write] DUAL DAC 8-bit right aligned data holding register
    volatile uint32_t    DAC_DOR1;             // [Read-only] DAC channel1 data output register
    volatile uint32_t    DAC_DOR2;             // [Read-only] DAC channel2 data output register
    volatile uint32_t    DAC_SR;               // DAC status register
    volatile uint32_t    DAC_CCR;              // [Read-write] DAC calibration control register
    volatile uint32_t    DAC_MCR;              // [Read-write] DAC mode control register
    volatile uint32_t    DAC_SHSR1;            // [Read-write] DAC Sample and Hold sample time register 1
    volatile uint32_t    DAC_SHSR2;            // [Read-write] DAC Sample and Hold sample time register 2
    volatile uint32_t    DAC_SHHR;             // [Read-write] DAC Sample and Hold hold time register
    volatile uint32_t    DAC_SHRR;             // [Read-write] DAC Sample and Hold refresh time register
    reserved_t<2>        _0;
    volatile uint32_t    DAC_STR1;             // [Read-write] Sawtooth register
    volatile uint32_t    DAC_STR2;             // [Read-write] Sawtooth register
    volatile uint32_t    DAC_STMODR;           // [Read-write] Sawtooth Mode register

    static constexpr uint32_t DAC_CR_EN1 = 0x1;            // DAC channel1 enable This bit is set and cleared by software to enable/disable DAC channel1.
    static constexpr uint32_t DAC_CR_TEN1 = 0x2;           // DAC channel1 trigger enable
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_TSEL1 =               // DAC channel1 trigger selection These bits select the external event used to trigger DAC channel1. Note: Only used if bit TEN1 = 1 (DAC channel1 trigger enabled). (4 bits)
        bit_field_t<2, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_WAVE1 =               // DAC channel1 noise/triangle wave generation enable These bits are set and cleared by software. Note: Only used if bit TEN1 = 1 (DAC channel1 trigger enabled). (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_MAMP1 =               // DAC channel1 mask/amplitude selector These bits are written by software to select mask in wave generation mode or amplitude in triangle generation mode. = 1011: Unmask bits[11:0] of LFSR/ triangle amplitude equal to 4095 (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t DAC_CR_DMAEN1 = 0x1000;      // DAC channel1 DMA enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_DMAUDRIE1 = 0x2000;   // DAC channel1 DMA Underrun Interrupt enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_CEN1 = 0x4000;        // DAC Channel 1 calibration enable This bit is set and cleared by software to enable/disable DAC channel 1 calibration, it can be written only if bit EN1=0 into DAC_CR (the calibration mode can be entered/exit only when the DAC channel is disabled) Otherwise, the write operation is ignored.
    static constexpr uint32_t DAC_CR_EN2 = 0x10000;        // DAC channel2 enable This bit is set and cleared by software to enable/disable DAC channel2.
    static constexpr uint32_t DAC_CR_TEN2 = 0x20000;       // DAC channel2 trigger enable
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_TSEL2 =               // DAC channel2 trigger selection These bits select the external event used to trigger DAC channel2 Note: Only used if bit TEN2 = 1 (DAC channel2 trigger enabled). (4 bits)
        bit_field_t<18, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_WAVE2 =               // DAC channel2 noise/triangle wave generation enable These bits are set/reset by software. 1x: Triangle wave generation enabled Note: Only used if bit TEN2 = 1 (DAC channel2 trigger enabled) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_MAMP2 =               // DAC channel2 mask/amplitude selector These bits are written by software to select mask in wave generation mode or amplitude in triangle generation mode. = 1011: Unmask bits[11:0] of LFSR/ triangle amplitude equal to 4095 (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t DAC_CR_DMAEN2 = 0x10000000;  // DAC channel2 DMA enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_DMAUDRIE2 = 0x20000000;// DAC channel2 DMA underrun interrupt enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_CEN2 = 0x40000000;    // DAC Channel 2 calibration enable This bit is set and cleared by software to enable/disable DAC channel 2 calibration, it can be written only if bit EN2=0 into DAC_CR (the calibration mode can be entered/exit only when the DAC channel is disabled) Otherwise, the write operation is ignored.
    static const uint32_t DAC_CR_RESET_VALUE = 0x0;

    static constexpr uint32_t DAC_SWTRGR_SWTRIG1 = 0x1;        // DAC channel1 software trigger This bit is set by software to trigger the DAC in software trigger mode. Note: This bit is cleared by hardware (one APB1 clock cycle later) once the DAC_DHR1 register value has been loaded into the DAC_DOR1 register.
    static constexpr uint32_t DAC_SWTRGR_SWTRIG2 = 0x2;        // DAC channel2 software trigger This bit is set by software to trigger the DAC in software trigger mode. Note: This bit is cleared by hardware (one APB1 clock cycle later) once the DAC_DHR2 register value has been loaded into the DAC_DOR2 register.
    static constexpr uint32_t DAC_SWTRGR_SWTRIGB1 = 0x10000;   // DAC channel1 software trigger B
    static constexpr uint32_t DAC_SWTRGR_SWTRIGB2 = 0x20000;   // DAC channel2 software trigger B
    static const uint32_t DAC_SWTRGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R1_DACC1DHR =            // DAC channel1 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R1_DACC1DHRB =           // DAC channel1 12-bit right-aligned data B (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12R1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L1_DACC1DHR =            // DAC channel1 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L1_DACC1DHRB =           // DAC channel1 12-bit left-aligned data B (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12L1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R1_DACC1DHR =            // DAC channel1 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel1. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R1_DACC1DHRB =           // DAC channel1 8-bit right-aligned data (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DAC_DHR8R1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R2_DACC2DHR =            // DAC channel2 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R2_DACC2DHRB =           // DAC channel2 12-bit right-aligned data (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12R2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L2_DACC2DHR =            // DAC channel2 12-bit left-aligned data These bits are written by software which specify 12-bit data for DAC channel2. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L2_DACC2DHRB =           // DAC channel2 12-bit left-aligned data B (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12L2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R2_DACC2DHR =            // DAC channel2 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel2. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R2_DACC2DHRB =           // DAC channel2 8-bit right-aligned data (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DAC_DHR8R2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12RD_DACC1DHR =            // DAC channel1 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12RD_DACC2DHR =            // DAC channel2 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12LD_DACC1DHR =            // DAC channel1 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12LD_DACC2DHR =            // DAC channel2 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12LD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8RD_DACC1DHR =            // DAC channel1 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel1. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8RD_DACC2DHR =            // DAC channel2 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel2. (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DAC_DHR8RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DOR1_DACC1DOR =            // DAC channel1 data output These bits are read-only, they contain data output for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DOR1_DACC1DORB =           // DAC channel1 data output (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DOR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DOR2_DACC2DOR =            // DAC channel2 data output These bits are read-only, they contain data output for DAC channel2. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DOR2_DACC2DORB =           // DAC channel2 data output (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DOR2_RESET_VALUE = 0x0;

    static constexpr uint32_t DAC_SR_DAC1RDY = 0x800;      // DAC channel1 ready status bit, Read-write
    static constexpr uint32_t DAC_SR_DORSTAT1 = 0x1000;    // DAC channel1 output register status bit, Read-write
    static constexpr uint32_t DAC_SR_DMAUDR1 = 0x2000;     // DAC channel1 DMA underrun flag This bit is set by hardware and cleared by software (by writing it to 1)., Read-write
    static constexpr uint32_t DAC_SR_CAL_FLAG1 = 0x4000;   // DAC Channel 1 calibration offset status This bit is set and cleared by hardware, Read-only
    static constexpr uint32_t DAC_SR_BWST1 = 0x8000;       // DAC Channel 1 busy writing sample time flag This bit is systematically set just after Sample &amp; Hold mode enable and is set each time the software writes the register DAC_SHSR1, It is cleared by hardware when the write operation of DAC_SHSR1 is complete. (It takes about 3LSI periods of synchronization)., Read-only
    static constexpr uint32_t DAC_SR_DAC2RDY = 0x8000000;  // DAC channel 2 ready status bit, Read-write
    static constexpr uint32_t DAC_SR_DORSTAT2 = 0x10000000;// DAC channel 2 output register status bit, Read-write
    static constexpr uint32_t DAC_SR_DMAUDR2 = 0x20000000; // DAC channel2 DMA underrun flag This bit is set by hardware and cleared by software (by writing it to 1)., Read-write
    static constexpr uint32_t DAC_SR_CAL_FLAG2 = 0x40000000;// DAC Channel 2 calibration offset status This bit is set and cleared by hardware, Read-only
    static constexpr uint32_t DAC_SR_BWST2 = 0x80000000;   // DAC Channel 2 busy writing sample time flag This bit is systematically set just after Sample &amp; Hold mode enable and is set each time the software writes the register DAC_SHSR2, It is cleared by hardware when the write operation of DAC_SHSR2 is complete. (It takes about 3 LSI periods of synchronization)., Read-only
    static const uint32_t DAC_SR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_CCR_OTRIM1 =              // DAC Channel 1 offset trimming value (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CCR_OTRIM2 =              // DAC Channel 2 offset trimming value (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static const uint32_t DAC_CCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_MCR_MODE1 =               // DAC Channel 1 mode These bits can be written only when the DAC is disabled and not in the calibration mode (when bit EN1=0 and bit CEN1 =0 in the DAC_CR register). If EN1=1 or CEN1 =1 the write operation is ignored. They can be set and cleared by software to select the DAC Channel 1 mode: DAC Channel 1 in normal Mode DAC Channel 1 in sample &amp;amp; hold mode (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t DAC_MCR_DMADOUBLE1 = 0x100;   // DAC Channel1 DMA double data mode
    static constexpr uint32_t DAC_MCR_SINFORMAT1 = 0x200;   // Enable signed format for DAC channel1
    template<uint32_t X>
    static constexpr uint32_t DAC_MCR_HFSEL =               // High frequency interface mode selection (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_MCR_MODE2 =               // DAC Channel 2 mode These bits can be written only when the DAC is disabled and not in the calibration mode (when bit EN2=0 and bit CEN2 =0 in the DAC_CR register). If EN2=1 or CEN2 =1 the write operation is ignored. They can be set and cleared by software to select the DAC Channel 2 mode: DAC Channel 2 in normal Mode DAC Channel 2 in sample &amp;amp; hold mode (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static constexpr uint32_t DAC_MCR_DMADOUBLE2 = 0x1000000;// DAC Channel2 DMA double data mode
    static constexpr uint32_t DAC_MCR_SINFORMAT2 = 0x2000000;// Enable signed format for DAC channel2
    static const uint32_t DAC_MCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHSR1_TSAMPLE1 =            // DAC Channel 1 sample Time (only valid in sample &amp;amp; hold mode) These bits can be written when the DAC channel1 is disabled or also during normal operation. in the latter case, the write can be done only when BWSTx of DAC_SR register is low, If BWSTx=1, the write operation is ignored. (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t DAC_SHSR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHSR2_TSAMPLE2 =            // DAC Channel 2 sample Time (only valid in sample &amp;amp; hold mode) These bits can be written when the DAC channel2 is disabled or also during normal operation. in the latter case, the write can be done only when BWSTx of DAC_SR register is low, if BWSTx=1, the write operation is ignored. (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t DAC_SHSR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHHR_THOLD1 =              // DAC Channel 1 hold Time (only valid in sample &amp;amp; hold mode) Hold time= (THOLD[9:0]) x T LSI (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_SHHR_THOLD2 =              // DAC Channel 2 hold time (only valid in sample &amp;amp; hold mode). Hold time= (THOLD[9:0]) x T LSI (10 bits)
        bit_field_t<16, 0x3ff>::value<X>();
    static const uint32_t DAC_SHHR_RESET_VALUE = 0x10001;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHRR_TREFRESH1 =           // DAC Channel 1 refresh Time (only valid in sample &amp;amp; hold mode) Refresh time= (TREFRESH[7:0]) x T LSI (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_SHRR_TREFRESH2 =           // DAC Channel 2 refresh Time (only valid in sample &amp;amp; hold mode) Refresh time= (TREFRESH[7:0]) x T LSI (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static const uint32_t DAC_SHRR_RESET_VALUE = 0x10001;

    template<uint32_t X>
    static constexpr uint32_t DAC_STR1_STRSTDATA1 =          // DAC Channel 1 Sawtooth reset value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static constexpr uint32_t DAC_STR1_STDIR1 = 0x1000;      // DAC Channel1 Sawtooth direction setting
    template<uint32_t X>
    static constexpr uint32_t DAC_STR1_STINCDATA1 =          // DAC CH1 Sawtooth increment value (12.4 bit format) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t DAC_STR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_STR2_STRSTDATA2 =          // DAC Channel 2 Sawtooth reset value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static constexpr uint32_t DAC_STR2_STDIR2 = 0x1000;      // DAC Channel2 Sawtooth direction setting
    template<uint32_t X>
    static constexpr uint32_t DAC_STR2_STINCDATA2 =          // DAC CH2 Sawtooth increment value (12.4 bit format) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t DAC_STR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STRSTTRIGSEL1 =       // DAC Channel 1 Sawtooth Reset trigger selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STINCTRIGSEL1 =       // DAC Channel 1 Sawtooth Increment trigger selection (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STRSTTRIGSEL2 =       // DAC Channel 1 Sawtooth Reset trigger selection (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STINCTRIGSEL2 =       // DAC Channel 2 Sawtooth Increment trigger selection (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static const uint32_t DAC_STMODR_RESET_VALUE = 0x0;
};

static dac1_t& DAC1 = *reinterpret_cast<dac1_t*>(0x50000800);

#define HAVE_PERIPHERAL_DAC1


////
//
//    Digital-to-analog converter
//
////

struct dac2_t
{
    volatile uint32_t    DAC_CR;               // [Read-write] DAC control register
    volatile uint32_t    DAC_SWTRGR;           // [Write-only] DAC software trigger register
    volatile uint32_t    DAC_DHR12R1;          // [Read-write] DAC channel1 12-bit right-aligned data holding register
    volatile uint32_t    DAC_DHR12L1;          // [Read-write] DAC channel1 12-bit left aligned data holding register
    volatile uint32_t    DAC_DHR8R1;           // [Read-write] DAC channel1 8-bit right aligned data holding register
    volatile uint32_t    DAC_DHR12R2;          // [Read-write] DAC channel2 12-bit right aligned data holding register
    volatile uint32_t    DAC_DHR12L2;          // [Read-write] DAC channel2 12-bit left aligned data holding register
    volatile uint32_t    DAC_DHR8R2;           // [Read-write] DAC channel2 8-bit right-aligned data holding register
    volatile uint32_t    DAC_DHR12RD;          // [Read-write] Dual DAC 12-bit right-aligned data holding register
    volatile uint32_t    DAC_DHR12LD;          // [Read-write] DUAL DAC 12-bit left aligned data holding register
    volatile uint32_t    DAC_DHR8RD;           // [Read-write] DUAL DAC 8-bit right aligned data holding register
    volatile uint32_t    DAC_DOR1;             // [Read-only] DAC channel1 data output register
    volatile uint32_t    DAC_DOR2;             // [Read-only] DAC channel2 data output register
    volatile uint32_t    DAC_SR;               // DAC status register
    volatile uint32_t    DAC_CCR;              // [Read-write] DAC calibration control register
    volatile uint32_t    DAC_MCR;              // [Read-write] DAC mode control register
    volatile uint32_t    DAC_SHSR1;            // [Read-write] DAC Sample and Hold sample time register 1
    volatile uint32_t    DAC_SHSR2;            // [Read-write] DAC Sample and Hold sample time register 2
    volatile uint32_t    DAC_SHHR;             // [Read-write] DAC Sample and Hold hold time register
    volatile uint32_t    DAC_SHRR;             // [Read-write] DAC Sample and Hold refresh time register
    reserved_t<2>        _0;
    volatile uint32_t    DAC_STR1;             // [Read-write] Sawtooth register
    volatile uint32_t    DAC_STR2;             // [Read-write] Sawtooth register
    volatile uint32_t    DAC_STMODR;           // [Read-write] Sawtooth Mode register

    static constexpr uint32_t DAC_CR_EN1 = 0x1;            // DAC channel1 enable This bit is set and cleared by software to enable/disable DAC channel1.
    static constexpr uint32_t DAC_CR_TEN1 = 0x2;           // DAC channel1 trigger enable
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_TSEL1 =               // DAC channel1 trigger selection These bits select the external event used to trigger DAC channel1. Note: Only used if bit TEN1 = 1 (DAC channel1 trigger enabled). (4 bits)
        bit_field_t<2, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_WAVE1 =               // DAC channel1 noise/triangle wave generation enable These bits are set and cleared by software. Note: Only used if bit TEN1 = 1 (DAC channel1 trigger enabled). (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_MAMP1 =               // DAC channel1 mask/amplitude selector These bits are written by software to select mask in wave generation mode or amplitude in triangle generation mode. = 1011: Unmask bits[11:0] of LFSR/ triangle amplitude equal to 4095 (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t DAC_CR_DMAEN1 = 0x1000;      // DAC channel1 DMA enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_DMAUDRIE1 = 0x2000;   // DAC channel1 DMA Underrun Interrupt enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_CEN1 = 0x4000;        // DAC Channel 1 calibration enable This bit is set and cleared by software to enable/disable DAC channel 1 calibration, it can be written only if bit EN1=0 into DAC_CR (the calibration mode can be entered/exit only when the DAC channel is disabled) Otherwise, the write operation is ignored.
    static constexpr uint32_t DAC_CR_EN2 = 0x10000;        // DAC channel2 enable This bit is set and cleared by software to enable/disable DAC channel2.
    static constexpr uint32_t DAC_CR_TEN2 = 0x20000;       // DAC channel2 trigger enable
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_TSEL2 =               // DAC channel2 trigger selection These bits select the external event used to trigger DAC channel2 Note: Only used if bit TEN2 = 1 (DAC channel2 trigger enabled). (4 bits)
        bit_field_t<18, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_WAVE2 =               // DAC channel2 noise/triangle wave generation enable These bits are set/reset by software. 1x: Triangle wave generation enabled Note: Only used if bit TEN2 = 1 (DAC channel2 trigger enabled) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_MAMP2 =               // DAC channel2 mask/amplitude selector These bits are written by software to select mask in wave generation mode or amplitude in triangle generation mode. = 1011: Unmask bits[11:0] of LFSR/ triangle amplitude equal to 4095 (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t DAC_CR_DMAEN2 = 0x10000000;  // DAC channel2 DMA enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_DMAUDRIE2 = 0x20000000;// DAC channel2 DMA underrun interrupt enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_CEN2 = 0x40000000;    // DAC Channel 2 calibration enable This bit is set and cleared by software to enable/disable DAC channel 2 calibration, it can be written only if bit EN2=0 into DAC_CR (the calibration mode can be entered/exit only when the DAC channel is disabled) Otherwise, the write operation is ignored.
    static const uint32_t DAC_CR_RESET_VALUE = 0x0;

    static constexpr uint32_t DAC_SWTRGR_SWTRIG1 = 0x1;        // DAC channel1 software trigger This bit is set by software to trigger the DAC in software trigger mode. Note: This bit is cleared by hardware (one APB1 clock cycle later) once the DAC_DHR1 register value has been loaded into the DAC_DOR1 register.
    static constexpr uint32_t DAC_SWTRGR_SWTRIG2 = 0x2;        // DAC channel2 software trigger This bit is set by software to trigger the DAC in software trigger mode. Note: This bit is cleared by hardware (one APB1 clock cycle later) once the DAC_DHR2 register value has been loaded into the DAC_DOR2 register.
    static constexpr uint32_t DAC_SWTRGR_SWTRIGB1 = 0x10000;   // DAC channel1 software trigger B
    static constexpr uint32_t DAC_SWTRGR_SWTRIGB2 = 0x20000;   // DAC channel2 software trigger B
    static const uint32_t DAC_SWTRGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R1_DACC1DHR =            // DAC channel1 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R1_DACC1DHRB =           // DAC channel1 12-bit right-aligned data B (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12R1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L1_DACC1DHR =            // DAC channel1 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L1_DACC1DHRB =           // DAC channel1 12-bit left-aligned data B (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12L1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R1_DACC1DHR =            // DAC channel1 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel1. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R1_DACC1DHRB =           // DAC channel1 8-bit right-aligned data (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DAC_DHR8R1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R2_DACC2DHR =            // DAC channel2 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R2_DACC2DHRB =           // DAC channel2 12-bit right-aligned data (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12R2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L2_DACC2DHR =            // DAC channel2 12-bit left-aligned data These bits are written by software which specify 12-bit data for DAC channel2. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L2_DACC2DHRB =           // DAC channel2 12-bit left-aligned data B (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12L2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R2_DACC2DHR =            // DAC channel2 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel2. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R2_DACC2DHRB =           // DAC channel2 8-bit right-aligned data (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DAC_DHR8R2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12RD_DACC1DHR =            // DAC channel1 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12RD_DACC2DHR =            // DAC channel2 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12LD_DACC1DHR =            // DAC channel1 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12LD_DACC2DHR =            // DAC channel2 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12LD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8RD_DACC1DHR =            // DAC channel1 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel1. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8RD_DACC2DHR =            // DAC channel2 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel2. (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DAC_DHR8RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DOR1_DACC1DOR =            // DAC channel1 data output These bits are read-only, they contain data output for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DOR1_DACC1DORB =           // DAC channel1 data output (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DOR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DOR2_DACC2DOR =            // DAC channel2 data output These bits are read-only, they contain data output for DAC channel2. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DOR2_DACC2DORB =           // DAC channel2 data output (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DOR2_RESET_VALUE = 0x0;

    static constexpr uint32_t DAC_SR_DAC1RDY = 0x800;      // DAC channel1 ready status bit, Read-write
    static constexpr uint32_t DAC_SR_DORSTAT1 = 0x1000;    // DAC channel1 output register status bit, Read-write
    static constexpr uint32_t DAC_SR_DMAUDR1 = 0x2000;     // DAC channel1 DMA underrun flag This bit is set by hardware and cleared by software (by writing it to 1)., Read-write
    static constexpr uint32_t DAC_SR_CAL_FLAG1 = 0x4000;   // DAC Channel 1 calibration offset status This bit is set and cleared by hardware, Read-only
    static constexpr uint32_t DAC_SR_BWST1 = 0x8000;       // DAC Channel 1 busy writing sample time flag This bit is systematically set just after Sample &amp; Hold mode enable and is set each time the software writes the register DAC_SHSR1, It is cleared by hardware when the write operation of DAC_SHSR1 is complete. (It takes about 3LSI periods of synchronization)., Read-only
    static constexpr uint32_t DAC_SR_DAC2RDY = 0x8000000;  // DAC channel 2 ready status bit, Read-write
    static constexpr uint32_t DAC_SR_DORSTAT2 = 0x10000000;// DAC channel 2 output register status bit, Read-write
    static constexpr uint32_t DAC_SR_DMAUDR2 = 0x20000000; // DAC channel2 DMA underrun flag This bit is set by hardware and cleared by software (by writing it to 1)., Read-write
    static constexpr uint32_t DAC_SR_CAL_FLAG2 = 0x40000000;// DAC Channel 2 calibration offset status This bit is set and cleared by hardware, Read-only
    static constexpr uint32_t DAC_SR_BWST2 = 0x80000000;   // DAC Channel 2 busy writing sample time flag This bit is systematically set just after Sample &amp; Hold mode enable and is set each time the software writes the register DAC_SHSR2, It is cleared by hardware when the write operation of DAC_SHSR2 is complete. (It takes about 3 LSI periods of synchronization)., Read-only
    static const uint32_t DAC_SR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_CCR_OTRIM1 =              // DAC Channel 1 offset trimming value (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CCR_OTRIM2 =              // DAC Channel 2 offset trimming value (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static const uint32_t DAC_CCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_MCR_MODE1 =               // DAC Channel 1 mode These bits can be written only when the DAC is disabled and not in the calibration mode (when bit EN1=0 and bit CEN1 =0 in the DAC_CR register). If EN1=1 or CEN1 =1 the write operation is ignored. They can be set and cleared by software to select the DAC Channel 1 mode: DAC Channel 1 in normal Mode DAC Channel 1 in sample &amp;amp; hold mode (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t DAC_MCR_DMADOUBLE1 = 0x100;   // DAC Channel1 DMA double data mode
    static constexpr uint32_t DAC_MCR_SINFORMAT1 = 0x200;   // Enable signed format for DAC channel1
    template<uint32_t X>
    static constexpr uint32_t DAC_MCR_HFSEL =               // High frequency interface mode selection (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_MCR_MODE2 =               // DAC Channel 2 mode These bits can be written only when the DAC is disabled and not in the calibration mode (when bit EN2=0 and bit CEN2 =0 in the DAC_CR register). If EN2=1 or CEN2 =1 the write operation is ignored. They can be set and cleared by software to select the DAC Channel 2 mode: DAC Channel 2 in normal Mode DAC Channel 2 in sample &amp;amp; hold mode (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static constexpr uint32_t DAC_MCR_DMADOUBLE2 = 0x1000000;// DAC Channel2 DMA double data mode
    static constexpr uint32_t DAC_MCR_SINFORMAT2 = 0x2000000;// Enable signed format for DAC channel2
    static const uint32_t DAC_MCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHSR1_TSAMPLE1 =            // DAC Channel 1 sample Time (only valid in sample &amp;amp; hold mode) These bits can be written when the DAC channel1 is disabled or also during normal operation. in the latter case, the write can be done only when BWSTx of DAC_SR register is low, If BWSTx=1, the write operation is ignored. (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t DAC_SHSR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHSR2_TSAMPLE2 =            // DAC Channel 2 sample Time (only valid in sample &amp;amp; hold mode) These bits can be written when the DAC channel2 is disabled or also during normal operation. in the latter case, the write can be done only when BWSTx of DAC_SR register is low, if BWSTx=1, the write operation is ignored. (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t DAC_SHSR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHHR_THOLD1 =              // DAC Channel 1 hold Time (only valid in sample &amp;amp; hold mode) Hold time= (THOLD[9:0]) x T LSI (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_SHHR_THOLD2 =              // DAC Channel 2 hold time (only valid in sample &amp;amp; hold mode). Hold time= (THOLD[9:0]) x T LSI (10 bits)
        bit_field_t<16, 0x3ff>::value<X>();
    static const uint32_t DAC_SHHR_RESET_VALUE = 0x10001;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHRR_TREFRESH1 =           // DAC Channel 1 refresh Time (only valid in sample &amp;amp; hold mode) Refresh time= (TREFRESH[7:0]) x T LSI (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_SHRR_TREFRESH2 =           // DAC Channel 2 refresh Time (only valid in sample &amp;amp; hold mode) Refresh time= (TREFRESH[7:0]) x T LSI (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static const uint32_t DAC_SHRR_RESET_VALUE = 0x10001;

    template<uint32_t X>
    static constexpr uint32_t DAC_STR1_STRSTDATA1 =          // DAC Channel 1 Sawtooth reset value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static constexpr uint32_t DAC_STR1_STDIR1 = 0x1000;      // DAC Channel1 Sawtooth direction setting
    template<uint32_t X>
    static constexpr uint32_t DAC_STR1_STINCDATA1 =          // DAC CH1 Sawtooth increment value (12.4 bit format) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t DAC_STR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_STR2_STRSTDATA2 =          // DAC Channel 2 Sawtooth reset value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static constexpr uint32_t DAC_STR2_STDIR2 = 0x1000;      // DAC Channel2 Sawtooth direction setting
    template<uint32_t X>
    static constexpr uint32_t DAC_STR2_STINCDATA2 =          // DAC CH2 Sawtooth increment value (12.4 bit format) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t DAC_STR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STRSTTRIGSEL1 =       // DAC Channel 1 Sawtooth Reset trigger selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STINCTRIGSEL1 =       // DAC Channel 1 Sawtooth Increment trigger selection (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STRSTTRIGSEL2 =       // DAC Channel 1 Sawtooth Reset trigger selection (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STINCTRIGSEL2 =       // DAC Channel 2 Sawtooth Increment trigger selection (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static const uint32_t DAC_STMODR_RESET_VALUE = 0x0;
};

static dac2_t& DAC2 = *reinterpret_cast<dac2_t*>(0x50000c00);

#define HAVE_PERIPHERAL_DAC2


////
//
//    Digital-to-analog converter
//
////

struct dac3_t
{
    volatile uint32_t    DAC_CR;               // [Read-write] DAC control register
    volatile uint32_t    DAC_SWTRGR;           // [Write-only] DAC software trigger register
    volatile uint32_t    DAC_DHR12R1;          // [Read-write] DAC channel1 12-bit right-aligned data holding register
    volatile uint32_t    DAC_DHR12L1;          // [Read-write] DAC channel1 12-bit left aligned data holding register
    volatile uint32_t    DAC_DHR8R1;           // [Read-write] DAC channel1 8-bit right aligned data holding register
    volatile uint32_t    DAC_DHR12R2;          // [Read-write] DAC channel2 12-bit right aligned data holding register
    volatile uint32_t    DAC_DHR12L2;          // [Read-write] DAC channel2 12-bit left aligned data holding register
    volatile uint32_t    DAC_DHR8R2;           // [Read-write] DAC channel2 8-bit right-aligned data holding register
    volatile uint32_t    DAC_DHR12RD;          // [Read-write] Dual DAC 12-bit right-aligned data holding register
    volatile uint32_t    DAC_DHR12LD;          // [Read-write] DUAL DAC 12-bit left aligned data holding register
    volatile uint32_t    DAC_DHR8RD;           // [Read-write] DUAL DAC 8-bit right aligned data holding register
    volatile uint32_t    DAC_DOR1;             // [Read-only] DAC channel1 data output register
    volatile uint32_t    DAC_DOR2;             // [Read-only] DAC channel2 data output register
    volatile uint32_t    DAC_SR;               // DAC status register
    volatile uint32_t    DAC_CCR;              // [Read-write] DAC calibration control register
    volatile uint32_t    DAC_MCR;              // [Read-write] DAC mode control register
    volatile uint32_t    DAC_SHSR1;            // [Read-write] DAC Sample and Hold sample time register 1
    volatile uint32_t    DAC_SHSR2;            // [Read-write] DAC Sample and Hold sample time register 2
    volatile uint32_t    DAC_SHHR;             // [Read-write] DAC Sample and Hold hold time register
    volatile uint32_t    DAC_SHRR;             // [Read-write] DAC Sample and Hold refresh time register
    reserved_t<2>        _0;
    volatile uint32_t    DAC_STR1;             // [Read-write] Sawtooth register
    volatile uint32_t    DAC_STR2;             // [Read-write] Sawtooth register
    volatile uint32_t    DAC_STMODR;           // [Read-write] Sawtooth Mode register

    static constexpr uint32_t DAC_CR_EN1 = 0x1;            // DAC channel1 enable This bit is set and cleared by software to enable/disable DAC channel1.
    static constexpr uint32_t DAC_CR_TEN1 = 0x2;           // DAC channel1 trigger enable
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_TSEL1 =               // DAC channel1 trigger selection These bits select the external event used to trigger DAC channel1. Note: Only used if bit TEN1 = 1 (DAC channel1 trigger enabled). (4 bits)
        bit_field_t<2, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_WAVE1 =               // DAC channel1 noise/triangle wave generation enable These bits are set and cleared by software. Note: Only used if bit TEN1 = 1 (DAC channel1 trigger enabled). (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_MAMP1 =               // DAC channel1 mask/amplitude selector These bits are written by software to select mask in wave generation mode or amplitude in triangle generation mode. = 1011: Unmask bits[11:0] of LFSR/ triangle amplitude equal to 4095 (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t DAC_CR_DMAEN1 = 0x1000;      // DAC channel1 DMA enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_DMAUDRIE1 = 0x2000;   // DAC channel1 DMA Underrun Interrupt enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_CEN1 = 0x4000;        // DAC Channel 1 calibration enable This bit is set and cleared by software to enable/disable DAC channel 1 calibration, it can be written only if bit EN1=0 into DAC_CR (the calibration mode can be entered/exit only when the DAC channel is disabled) Otherwise, the write operation is ignored.
    static constexpr uint32_t DAC_CR_EN2 = 0x10000;        // DAC channel2 enable This bit is set and cleared by software to enable/disable DAC channel2.
    static constexpr uint32_t DAC_CR_TEN2 = 0x20000;       // DAC channel2 trigger enable
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_TSEL2 =               // DAC channel2 trigger selection These bits select the external event used to trigger DAC channel2 Note: Only used if bit TEN2 = 1 (DAC channel2 trigger enabled). (4 bits)
        bit_field_t<18, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_WAVE2 =               // DAC channel2 noise/triangle wave generation enable These bits are set/reset by software. 1x: Triangle wave generation enabled Note: Only used if bit TEN2 = 1 (DAC channel2 trigger enabled) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_MAMP2 =               // DAC channel2 mask/amplitude selector These bits are written by software to select mask in wave generation mode or amplitude in triangle generation mode. = 1011: Unmask bits[11:0] of LFSR/ triangle amplitude equal to 4095 (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t DAC_CR_DMAEN2 = 0x10000000;  // DAC channel2 DMA enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_DMAUDRIE2 = 0x20000000;// DAC channel2 DMA underrun interrupt enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_CEN2 = 0x40000000;    // DAC Channel 2 calibration enable This bit is set and cleared by software to enable/disable DAC channel 2 calibration, it can be written only if bit EN2=0 into DAC_CR (the calibration mode can be entered/exit only when the DAC channel is disabled) Otherwise, the write operation is ignored.
    static const uint32_t DAC_CR_RESET_VALUE = 0x0;

    static constexpr uint32_t DAC_SWTRGR_SWTRIG1 = 0x1;        // DAC channel1 software trigger This bit is set by software to trigger the DAC in software trigger mode. Note: This bit is cleared by hardware (one APB1 clock cycle later) once the DAC_DHR1 register value has been loaded into the DAC_DOR1 register.
    static constexpr uint32_t DAC_SWTRGR_SWTRIG2 = 0x2;        // DAC channel2 software trigger This bit is set by software to trigger the DAC in software trigger mode. Note: This bit is cleared by hardware (one APB1 clock cycle later) once the DAC_DHR2 register value has been loaded into the DAC_DOR2 register.
    static constexpr uint32_t DAC_SWTRGR_SWTRIGB1 = 0x10000;   // DAC channel1 software trigger B
    static constexpr uint32_t DAC_SWTRGR_SWTRIGB2 = 0x20000;   // DAC channel2 software trigger B
    static const uint32_t DAC_SWTRGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R1_DACC1DHR =            // DAC channel1 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R1_DACC1DHRB =           // DAC channel1 12-bit right-aligned data B (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12R1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L1_DACC1DHR =            // DAC channel1 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L1_DACC1DHRB =           // DAC channel1 12-bit left-aligned data B (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12L1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R1_DACC1DHR =            // DAC channel1 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel1. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R1_DACC1DHRB =           // DAC channel1 8-bit right-aligned data (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DAC_DHR8R1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R2_DACC2DHR =            // DAC channel2 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R2_DACC2DHRB =           // DAC channel2 12-bit right-aligned data (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12R2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L2_DACC2DHR =            // DAC channel2 12-bit left-aligned data These bits are written by software which specify 12-bit data for DAC channel2. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L2_DACC2DHRB =           // DAC channel2 12-bit left-aligned data B (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12L2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R2_DACC2DHR =            // DAC channel2 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel2. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R2_DACC2DHRB =           // DAC channel2 8-bit right-aligned data (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DAC_DHR8R2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12RD_DACC1DHR =            // DAC channel1 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12RD_DACC2DHR =            // DAC channel2 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12LD_DACC1DHR =            // DAC channel1 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12LD_DACC2DHR =            // DAC channel2 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12LD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8RD_DACC1DHR =            // DAC channel1 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel1. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8RD_DACC2DHR =            // DAC channel2 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel2. (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DAC_DHR8RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DOR1_DACC1DOR =            // DAC channel1 data output These bits are read-only, they contain data output for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DOR1_DACC1DORB =           // DAC channel1 data output (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DOR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DOR2_DACC2DOR =            // DAC channel2 data output These bits are read-only, they contain data output for DAC channel2. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DOR2_DACC2DORB =           // DAC channel2 data output (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DOR2_RESET_VALUE = 0x0;

    static constexpr uint32_t DAC_SR_DAC1RDY = 0x800;      // DAC channel1 ready status bit, Read-write
    static constexpr uint32_t DAC_SR_DORSTAT1 = 0x1000;    // DAC channel1 output register status bit, Read-write
    static constexpr uint32_t DAC_SR_DMAUDR1 = 0x2000;     // DAC channel1 DMA underrun flag This bit is set by hardware and cleared by software (by writing it to 1)., Read-write
    static constexpr uint32_t DAC_SR_CAL_FLAG1 = 0x4000;   // DAC Channel 1 calibration offset status This bit is set and cleared by hardware, Read-only
    static constexpr uint32_t DAC_SR_BWST1 = 0x8000;       // DAC Channel 1 busy writing sample time flag This bit is systematically set just after Sample &amp; Hold mode enable and is set each time the software writes the register DAC_SHSR1, It is cleared by hardware when the write operation of DAC_SHSR1 is complete. (It takes about 3LSI periods of synchronization)., Read-only
    static constexpr uint32_t DAC_SR_DAC2RDY = 0x8000000;  // DAC channel 2 ready status bit, Read-write
    static constexpr uint32_t DAC_SR_DORSTAT2 = 0x10000000;// DAC channel 2 output register status bit, Read-write
    static constexpr uint32_t DAC_SR_DMAUDR2 = 0x20000000; // DAC channel2 DMA underrun flag This bit is set by hardware and cleared by software (by writing it to 1)., Read-write
    static constexpr uint32_t DAC_SR_CAL_FLAG2 = 0x40000000;// DAC Channel 2 calibration offset status This bit is set and cleared by hardware, Read-only
    static constexpr uint32_t DAC_SR_BWST2 = 0x80000000;   // DAC Channel 2 busy writing sample time flag This bit is systematically set just after Sample &amp; Hold mode enable and is set each time the software writes the register DAC_SHSR2, It is cleared by hardware when the write operation of DAC_SHSR2 is complete. (It takes about 3 LSI periods of synchronization)., Read-only
    static const uint32_t DAC_SR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_CCR_OTRIM1 =              // DAC Channel 1 offset trimming value (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CCR_OTRIM2 =              // DAC Channel 2 offset trimming value (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static const uint32_t DAC_CCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_MCR_MODE1 =               // DAC Channel 1 mode These bits can be written only when the DAC is disabled and not in the calibration mode (when bit EN1=0 and bit CEN1 =0 in the DAC_CR register). If EN1=1 or CEN1 =1 the write operation is ignored. They can be set and cleared by software to select the DAC Channel 1 mode: DAC Channel 1 in normal Mode DAC Channel 1 in sample &amp;amp; hold mode (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t DAC_MCR_DMADOUBLE1 = 0x100;   // DAC Channel1 DMA double data mode
    static constexpr uint32_t DAC_MCR_SINFORMAT1 = 0x200;   // Enable signed format for DAC channel1
    template<uint32_t X>
    static constexpr uint32_t DAC_MCR_HFSEL =               // High frequency interface mode selection (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_MCR_MODE2 =               // DAC Channel 2 mode These bits can be written only when the DAC is disabled and not in the calibration mode (when bit EN2=0 and bit CEN2 =0 in the DAC_CR register). If EN2=1 or CEN2 =1 the write operation is ignored. They can be set and cleared by software to select the DAC Channel 2 mode: DAC Channel 2 in normal Mode DAC Channel 2 in sample &amp;amp; hold mode (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static constexpr uint32_t DAC_MCR_DMADOUBLE2 = 0x1000000;// DAC Channel2 DMA double data mode
    static constexpr uint32_t DAC_MCR_SINFORMAT2 = 0x2000000;// Enable signed format for DAC channel2
    static const uint32_t DAC_MCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHSR1_TSAMPLE1 =            // DAC Channel 1 sample Time (only valid in sample &amp;amp; hold mode) These bits can be written when the DAC channel1 is disabled or also during normal operation. in the latter case, the write can be done only when BWSTx of DAC_SR register is low, If BWSTx=1, the write operation is ignored. (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t DAC_SHSR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHSR2_TSAMPLE2 =            // DAC Channel 2 sample Time (only valid in sample &amp;amp; hold mode) These bits can be written when the DAC channel2 is disabled or also during normal operation. in the latter case, the write can be done only when BWSTx of DAC_SR register is low, if BWSTx=1, the write operation is ignored. (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t DAC_SHSR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHHR_THOLD1 =              // DAC Channel 1 hold Time (only valid in sample &amp;amp; hold mode) Hold time= (THOLD[9:0]) x T LSI (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_SHHR_THOLD2 =              // DAC Channel 2 hold time (only valid in sample &amp;amp; hold mode). Hold time= (THOLD[9:0]) x T LSI (10 bits)
        bit_field_t<16, 0x3ff>::value<X>();
    static const uint32_t DAC_SHHR_RESET_VALUE = 0x10001;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHRR_TREFRESH1 =           // DAC Channel 1 refresh Time (only valid in sample &amp;amp; hold mode) Refresh time= (TREFRESH[7:0]) x T LSI (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_SHRR_TREFRESH2 =           // DAC Channel 2 refresh Time (only valid in sample &amp;amp; hold mode) Refresh time= (TREFRESH[7:0]) x T LSI (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static const uint32_t DAC_SHRR_RESET_VALUE = 0x10001;

    template<uint32_t X>
    static constexpr uint32_t DAC_STR1_STRSTDATA1 =          // DAC Channel 1 Sawtooth reset value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static constexpr uint32_t DAC_STR1_STDIR1 = 0x1000;      // DAC Channel1 Sawtooth direction setting
    template<uint32_t X>
    static constexpr uint32_t DAC_STR1_STINCDATA1 =          // DAC CH1 Sawtooth increment value (12.4 bit format) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t DAC_STR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_STR2_STRSTDATA2 =          // DAC Channel 2 Sawtooth reset value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static constexpr uint32_t DAC_STR2_STDIR2 = 0x1000;      // DAC Channel2 Sawtooth direction setting
    template<uint32_t X>
    static constexpr uint32_t DAC_STR2_STINCDATA2 =          // DAC CH2 Sawtooth increment value (12.4 bit format) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t DAC_STR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STRSTTRIGSEL1 =       // DAC Channel 1 Sawtooth Reset trigger selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STINCTRIGSEL1 =       // DAC Channel 1 Sawtooth Increment trigger selection (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STRSTTRIGSEL2 =       // DAC Channel 1 Sawtooth Reset trigger selection (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STINCTRIGSEL2 =       // DAC Channel 2 Sawtooth Increment trigger selection (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static const uint32_t DAC_STMODR_RESET_VALUE = 0x0;
};

static dac3_t& DAC3 = *reinterpret_cast<dac3_t*>(0x50001000);

#define HAVE_PERIPHERAL_DAC3


////
//
//    Digital-to-analog converter
//
////

struct dac4_t
{
    volatile uint32_t    DAC_CR;               // [Read-write] DAC control register
    volatile uint32_t    DAC_SWTRGR;           // [Write-only] DAC software trigger register
    volatile uint32_t    DAC_DHR12R1;          // [Read-write] DAC channel1 12-bit right-aligned data holding register
    volatile uint32_t    DAC_DHR12L1;          // [Read-write] DAC channel1 12-bit left aligned data holding register
    volatile uint32_t    DAC_DHR8R1;           // [Read-write] DAC channel1 8-bit right aligned data holding register
    volatile uint32_t    DAC_DHR12R2;          // [Read-write] DAC channel2 12-bit right aligned data holding register
    volatile uint32_t    DAC_DHR12L2;          // [Read-write] DAC channel2 12-bit left aligned data holding register
    volatile uint32_t    DAC_DHR8R2;           // [Read-write] DAC channel2 8-bit right-aligned data holding register
    volatile uint32_t    DAC_DHR12RD;          // [Read-write] Dual DAC 12-bit right-aligned data holding register
    volatile uint32_t    DAC_DHR12LD;          // [Read-write] DUAL DAC 12-bit left aligned data holding register
    volatile uint32_t    DAC_DHR8RD;           // [Read-write] DUAL DAC 8-bit right aligned data holding register
    volatile uint32_t    DAC_DOR1;             // [Read-only] DAC channel1 data output register
    volatile uint32_t    DAC_DOR2;             // [Read-only] DAC channel2 data output register
    volatile uint32_t    DAC_SR;               // DAC status register
    volatile uint32_t    DAC_CCR;              // [Read-write] DAC calibration control register
    volatile uint32_t    DAC_MCR;              // [Read-write] DAC mode control register
    volatile uint32_t    DAC_SHSR1;            // [Read-write] DAC Sample and Hold sample time register 1
    volatile uint32_t    DAC_SHSR2;            // [Read-write] DAC Sample and Hold sample time register 2
    volatile uint32_t    DAC_SHHR;             // [Read-write] DAC Sample and Hold hold time register
    volatile uint32_t    DAC_SHRR;             // [Read-write] DAC Sample and Hold refresh time register
    reserved_t<2>        _0;
    volatile uint32_t    DAC_STR1;             // [Read-write] Sawtooth register
    volatile uint32_t    DAC_STR2;             // [Read-write] Sawtooth register
    volatile uint32_t    DAC_STMODR;           // [Read-write] Sawtooth Mode register

    static constexpr uint32_t DAC_CR_EN1 = 0x1;            // DAC channel1 enable This bit is set and cleared by software to enable/disable DAC channel1.
    static constexpr uint32_t DAC_CR_TEN1 = 0x2;           // DAC channel1 trigger enable
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_TSEL1 =               // DAC channel1 trigger selection These bits select the external event used to trigger DAC channel1. Note: Only used if bit TEN1 = 1 (DAC channel1 trigger enabled). (4 bits)
        bit_field_t<2, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_WAVE1 =               // DAC channel1 noise/triangle wave generation enable These bits are set and cleared by software. Note: Only used if bit TEN1 = 1 (DAC channel1 trigger enabled). (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_MAMP1 =               // DAC channel1 mask/amplitude selector These bits are written by software to select mask in wave generation mode or amplitude in triangle generation mode. = 1011: Unmask bits[11:0] of LFSR/ triangle amplitude equal to 4095 (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t DAC_CR_DMAEN1 = 0x1000;      // DAC channel1 DMA enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_DMAUDRIE1 = 0x2000;   // DAC channel1 DMA Underrun Interrupt enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_CEN1 = 0x4000;        // DAC Channel 1 calibration enable This bit is set and cleared by software to enable/disable DAC channel 1 calibration, it can be written only if bit EN1=0 into DAC_CR (the calibration mode can be entered/exit only when the DAC channel is disabled) Otherwise, the write operation is ignored.
    static constexpr uint32_t DAC_CR_EN2 = 0x10000;        // DAC channel2 enable This bit is set and cleared by software to enable/disable DAC channel2.
    static constexpr uint32_t DAC_CR_TEN2 = 0x20000;       // DAC channel2 trigger enable
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_TSEL2 =               // DAC channel2 trigger selection These bits select the external event used to trigger DAC channel2 Note: Only used if bit TEN2 = 1 (DAC channel2 trigger enabled). (4 bits)
        bit_field_t<18, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_WAVE2 =               // DAC channel2 noise/triangle wave generation enable These bits are set/reset by software. 1x: Triangle wave generation enabled Note: Only used if bit TEN2 = 1 (DAC channel2 trigger enabled) (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CR_MAMP2 =               // DAC channel2 mask/amplitude selector These bits are written by software to select mask in wave generation mode or amplitude in triangle generation mode. = 1011: Unmask bits[11:0] of LFSR/ triangle amplitude equal to 4095 (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static constexpr uint32_t DAC_CR_DMAEN2 = 0x10000000;  // DAC channel2 DMA enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_DMAUDRIE2 = 0x20000000;// DAC channel2 DMA underrun interrupt enable This bit is set and cleared by software.
    static constexpr uint32_t DAC_CR_CEN2 = 0x40000000;    // DAC Channel 2 calibration enable This bit is set and cleared by software to enable/disable DAC channel 2 calibration, it can be written only if bit EN2=0 into DAC_CR (the calibration mode can be entered/exit only when the DAC channel is disabled) Otherwise, the write operation is ignored.
    static const uint32_t DAC_CR_RESET_VALUE = 0x0;

    static constexpr uint32_t DAC_SWTRGR_SWTRIG1 = 0x1;        // DAC channel1 software trigger This bit is set by software to trigger the DAC in software trigger mode. Note: This bit is cleared by hardware (one APB1 clock cycle later) once the DAC_DHR1 register value has been loaded into the DAC_DOR1 register.
    static constexpr uint32_t DAC_SWTRGR_SWTRIG2 = 0x2;        // DAC channel2 software trigger This bit is set by software to trigger the DAC in software trigger mode. Note: This bit is cleared by hardware (one APB1 clock cycle later) once the DAC_DHR2 register value has been loaded into the DAC_DOR2 register.
    static constexpr uint32_t DAC_SWTRGR_SWTRIGB1 = 0x10000;   // DAC channel1 software trigger B
    static constexpr uint32_t DAC_SWTRGR_SWTRIGB2 = 0x20000;   // DAC channel2 software trigger B
    static const uint32_t DAC_SWTRGR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R1_DACC1DHR =            // DAC channel1 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R1_DACC1DHRB =           // DAC channel1 12-bit right-aligned data B (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12R1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L1_DACC1DHR =            // DAC channel1 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L1_DACC1DHRB =           // DAC channel1 12-bit left-aligned data B (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12L1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R1_DACC1DHR =            // DAC channel1 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel1. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R1_DACC1DHRB =           // DAC channel1 8-bit right-aligned data (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DAC_DHR8R1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R2_DACC2DHR =            // DAC channel2 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12R2_DACC2DHRB =           // DAC channel2 12-bit right-aligned data (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12R2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L2_DACC2DHR =            // DAC channel2 12-bit left-aligned data These bits are written by software which specify 12-bit data for DAC channel2. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12L2_DACC2DHRB =           // DAC channel2 12-bit left-aligned data B (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12L2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R2_DACC2DHR =            // DAC channel2 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel2. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8R2_DACC2DHRB =           // DAC channel2 8-bit right-aligned data (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DAC_DHR8R2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12RD_DACC1DHR =            // DAC channel1 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12RD_DACC2DHR =            // DAC channel2 12-bit right-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12LD_DACC1DHR =            // DAC channel1 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel1. (12 bits)
        bit_field_t<4, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR12LD_DACC2DHR =            // DAC channel2 12-bit left-aligned data These bits are written by software which specifies 12-bit data for DAC channel2. (12 bits)
        bit_field_t<20, 0xfff>::value<X>();
    static const uint32_t DAC_DHR12LD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8RD_DACC1DHR =            // DAC channel1 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel1. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DHR8RD_DACC2DHR =            // DAC channel2 8-bit right-aligned data These bits are written by software which specifies 8-bit data for DAC channel2. (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t DAC_DHR8RD_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DOR1_DACC1DOR =            // DAC channel1 data output These bits are read-only, they contain data output for DAC channel1. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DOR1_DACC1DORB =           // DAC channel1 data output (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DOR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_DOR2_DACC2DOR =            // DAC channel2 data output These bits are read-only, they contain data output for DAC channel2. (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_DOR2_DACC2DORB =           // DAC channel2 data output (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    static const uint32_t DAC_DOR2_RESET_VALUE = 0x0;

    static constexpr uint32_t DAC_SR_DAC1RDY = 0x800;      // DAC channel1 ready status bit, Read-write
    static constexpr uint32_t DAC_SR_DORSTAT1 = 0x1000;    // DAC channel1 output register status bit, Read-write
    static constexpr uint32_t DAC_SR_DMAUDR1 = 0x2000;     // DAC channel1 DMA underrun flag This bit is set by hardware and cleared by software (by writing it to 1)., Read-write
    static constexpr uint32_t DAC_SR_CAL_FLAG1 = 0x4000;   // DAC Channel 1 calibration offset status This bit is set and cleared by hardware, Read-only
    static constexpr uint32_t DAC_SR_BWST1 = 0x8000;       // DAC Channel 1 busy writing sample time flag This bit is systematically set just after Sample &amp; Hold mode enable and is set each time the software writes the register DAC_SHSR1, It is cleared by hardware when the write operation of DAC_SHSR1 is complete. (It takes about 3LSI periods of synchronization)., Read-only
    static constexpr uint32_t DAC_SR_DAC2RDY = 0x8000000;  // DAC channel 2 ready status bit, Read-write
    static constexpr uint32_t DAC_SR_DORSTAT2 = 0x10000000;// DAC channel 2 output register status bit, Read-write
    static constexpr uint32_t DAC_SR_DMAUDR2 = 0x20000000; // DAC channel2 DMA underrun flag This bit is set by hardware and cleared by software (by writing it to 1)., Read-write
    static constexpr uint32_t DAC_SR_CAL_FLAG2 = 0x40000000;// DAC Channel 2 calibration offset status This bit is set and cleared by hardware, Read-only
    static constexpr uint32_t DAC_SR_BWST2 = 0x80000000;   // DAC Channel 2 busy writing sample time flag This bit is systematically set just after Sample &amp; Hold mode enable and is set each time the software writes the register DAC_SHSR2, It is cleared by hardware when the write operation of DAC_SHSR2 is complete. (It takes about 3 LSI periods of synchronization)., Read-only
    static const uint32_t DAC_SR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_CCR_OTRIM1 =              // DAC Channel 1 offset trimming value (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_CCR_OTRIM2 =              // DAC Channel 2 offset trimming value (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static const uint32_t DAC_CCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_MCR_MODE1 =               // DAC Channel 1 mode These bits can be written only when the DAC is disabled and not in the calibration mode (when bit EN1=0 and bit CEN1 =0 in the DAC_CR register). If EN1=1 or CEN1 =1 the write operation is ignored. They can be set and cleared by software to select the DAC Channel 1 mode: DAC Channel 1 in normal Mode DAC Channel 1 in sample &amp;amp; hold mode (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t DAC_MCR_DMADOUBLE1 = 0x100;   // DAC Channel1 DMA double data mode
    static constexpr uint32_t DAC_MCR_SINFORMAT1 = 0x200;   // Enable signed format for DAC channel1
    template<uint32_t X>
    static constexpr uint32_t DAC_MCR_HFSEL =               // High frequency interface mode selection (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_MCR_MODE2 =               // DAC Channel 2 mode These bits can be written only when the DAC is disabled and not in the calibration mode (when bit EN2=0 and bit CEN2 =0 in the DAC_CR register). If EN2=1 or CEN2 =1 the write operation is ignored. They can be set and cleared by software to select the DAC Channel 2 mode: DAC Channel 2 in normal Mode DAC Channel 2 in sample &amp;amp; hold mode (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static constexpr uint32_t DAC_MCR_DMADOUBLE2 = 0x1000000;// DAC Channel2 DMA double data mode
    static constexpr uint32_t DAC_MCR_SINFORMAT2 = 0x2000000;// Enable signed format for DAC channel2
    static const uint32_t DAC_MCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHSR1_TSAMPLE1 =            // DAC Channel 1 sample Time (only valid in sample &amp;amp; hold mode) These bits can be written when the DAC channel1 is disabled or also during normal operation. in the latter case, the write can be done only when BWSTx of DAC_SR register is low, If BWSTx=1, the write operation is ignored. (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t DAC_SHSR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHSR2_TSAMPLE2 =            // DAC Channel 2 sample Time (only valid in sample &amp;amp; hold mode) These bits can be written when the DAC channel2 is disabled or also during normal operation. in the latter case, the write can be done only when BWSTx of DAC_SR register is low, if BWSTx=1, the write operation is ignored. (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    static const uint32_t DAC_SHSR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHHR_THOLD1 =              // DAC Channel 1 hold Time (only valid in sample &amp;amp; hold mode) Hold time= (THOLD[9:0]) x T LSI (10 bits)
        bit_field_t<0, 0x3ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_SHHR_THOLD2 =              // DAC Channel 2 hold time (only valid in sample &amp;amp; hold mode). Hold time= (THOLD[9:0]) x T LSI (10 bits)
        bit_field_t<16, 0x3ff>::value<X>();
    static const uint32_t DAC_SHHR_RESET_VALUE = 0x10001;

    template<uint32_t X>
    static constexpr uint32_t DAC_SHRR_TREFRESH1 =           // DAC Channel 1 refresh Time (only valid in sample &amp;amp; hold mode) Refresh time= (TREFRESH[7:0]) x T LSI (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_SHRR_TREFRESH2 =           // DAC Channel 2 refresh Time (only valid in sample &amp;amp; hold mode) Refresh time= (TREFRESH[7:0]) x T LSI (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static const uint32_t DAC_SHRR_RESET_VALUE = 0x10001;

    template<uint32_t X>
    static constexpr uint32_t DAC_STR1_STRSTDATA1 =          // DAC Channel 1 Sawtooth reset value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static constexpr uint32_t DAC_STR1_STDIR1 = 0x1000;      // DAC Channel1 Sawtooth direction setting
    template<uint32_t X>
    static constexpr uint32_t DAC_STR1_STINCDATA1 =          // DAC CH1 Sawtooth increment value (12.4 bit format) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t DAC_STR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_STR2_STRSTDATA2 =          // DAC Channel 2 Sawtooth reset value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static constexpr uint32_t DAC_STR2_STDIR2 = 0x1000;      // DAC Channel2 Sawtooth direction setting
    template<uint32_t X>
    static constexpr uint32_t DAC_STR2_STINCDATA2 =          // DAC CH2 Sawtooth increment value (12.4 bit format) (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t DAC_STR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STRSTTRIGSEL1 =       // DAC Channel 1 Sawtooth Reset trigger selection (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STINCTRIGSEL1 =       // DAC Channel 1 Sawtooth Increment trigger selection (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STRSTTRIGSEL2 =       // DAC Channel 1 Sawtooth Reset trigger selection (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DAC_STMODR_STINCTRIGSEL2 =       // DAC Channel 2 Sawtooth Increment trigger selection (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    static const uint32_t DAC_STMODR_RESET_VALUE = 0x0;
};

static dac4_t& DAC4 = *reinterpret_cast<dac4_t*>(0x50001400);

#define HAVE_PERIPHERAL_DAC4


////
//
//    Analog-to-Digital Converter
//
////

struct adc1_t
{
    volatile uint32_t    ISR;                  // [Read-write] interrupt and status register
    volatile uint32_t    IER;                  // [Read-write] interrupt enable register
    volatile uint32_t    CR;                   // [Read-write] control register
    volatile uint32_t    CFGR;                 // [Read-write] configuration register
    volatile uint32_t    CFGR2;                // [Read-write] configuration register
    volatile uint32_t    SMPR1;                // [Read-write] sample time register 1
    volatile uint32_t    SMPR2;                // [Read-write] sample time register 2
    reserved_t<1>        _0;
    volatile uint32_t    TR1;                  // [Read-write] watchdog threshold register 1
    volatile uint32_t    TR2;                  // [Read-write] watchdog threshold register
    volatile uint32_t    TR3;                  // [Read-write] watchdog threshold register 3
    reserved_t<1>        _1;
    volatile uint32_t    SQR1;                 // [Read-write] regular sequence register 1
    volatile uint32_t    SQR2;                 // [Read-write] regular sequence register 2
    volatile uint32_t    SQR3;                 // [Read-write] regular sequence register 3
    volatile uint32_t    SQR4;                 // [Read-write] regular sequence register 4
    volatile uint32_t    DR;                   // [Read-only] regular Data Register
    reserved_t<2>        _2;
    volatile uint32_t    JSQR;                 // [Read-write] injected sequence register
    reserved_t<4>        _3;
    volatile uint32_t    OFR1;                 // [Read-write] offset register 1
    volatile uint32_t    OFR2;                 // [Read-write] offset register 2
    volatile uint32_t    OFR3;                 // [Read-write] offset register 3
    volatile uint32_t    OFR4;                 // [Read-write] offset register 4
    reserved_t<4>        _4;
    volatile uint32_t    JDR1;                 // [Read-only] injected data register 1
    volatile uint32_t    JDR2;                 // [Read-only] injected data register 2
    volatile uint32_t    JDR3;                 // [Read-only] injected data register 3
    volatile uint32_t    JDR4;                 // [Read-only] injected data register 4
    reserved_t<4>        _5;
    volatile uint32_t    AWD2CR;               // [Read-write] Analog Watchdog 2 Configuration Register
    volatile uint32_t    AWD3CR;               // [Read-write] Analog Watchdog 3 Configuration Register
    reserved_t<2>        _6;
    volatile uint32_t    DIFSEL;               // Differential Mode Selection Register 2
    volatile uint32_t    CALFACT;              // [Read-write] Calibration Factors
    reserved_t<2>        _7;
    volatile uint32_t    GCOMP;                // [Read-write] Gain compensation Register

    static constexpr uint32_t ISR_JQOVF = 0x400;        // JQOVF
    static constexpr uint32_t ISR_AWD3 = 0x200;         // AWD3
    static constexpr uint32_t ISR_AWD2 = 0x100;         // AWD2
    static constexpr uint32_t ISR_AWD1 = 0x80;          // AWD1
    static constexpr uint32_t ISR_JEOS = 0x40;          // JEOS
    static constexpr uint32_t ISR_JEOC = 0x20;          // JEOC
    static constexpr uint32_t ISR_OVR = 0x10;           // OVR
    static constexpr uint32_t ISR_EOS = 0x8;            // EOS
    static constexpr uint32_t ISR_EOC = 0x4;            // EOC
    static constexpr uint32_t ISR_EOSMP = 0x2;          // EOSMP
    static constexpr uint32_t ISR_ADRDY = 0x1;          // ADRDY
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t IER_JQOVFIE = 0x400;      // JQOVFIE
    static constexpr uint32_t IER_AWD3IE = 0x200;       // AWD3IE
    static constexpr uint32_t IER_AWD2IE = 0x100;       // AWD2IE
    static constexpr uint32_t IER_AWD1IE = 0x80;        // AWD1IE
    static constexpr uint32_t IER_JEOSIE = 0x40;        // JEOSIE
    static constexpr uint32_t IER_JEOCIE = 0x20;        // JEOCIE
    static constexpr uint32_t IER_OVRIE = 0x10;         // OVRIE
    static constexpr uint32_t IER_EOSIE = 0x8;          // EOSIE
    static constexpr uint32_t IER_EOCIE = 0x4;          // EOCIE
    static constexpr uint32_t IER_EOSMPIE = 0x2;        // EOSMPIE
    static constexpr uint32_t IER_ADRDYIE = 0x1;        // ADRDYIE
    static const uint32_t IER_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_ADCAL = 0x80000000;   // ADCAL
    static constexpr uint32_t CR_ADCALDIF = 0x40000000;// ADCALDIF
    static constexpr uint32_t CR_DEEPPWD = 0x20000000; // DEEPPWD
    static constexpr uint32_t CR_ADVREGEN = 0x10000000;// ADVREGEN
    static constexpr uint32_t CR_JADSTP = 0x20;        // JADSTP
    static constexpr uint32_t CR_ADSTP = 0x10;         // ADSTP
    static constexpr uint32_t CR_JADSTART = 0x8;       // JADSTART
    static constexpr uint32_t CR_ADSTART = 0x4;        // ADSTART
    static constexpr uint32_t CR_ADDIS = 0x2;          // ADDIS
    static constexpr uint32_t CR_ADEN = 0x1;           // ADEN
    static const uint32_t CR_RESET_VALUE = 0x20000000;

    static constexpr uint32_t CFGR_JQDIS = 0x80000000;   // Injected Queue disable
    template<uint32_t X>
    static constexpr uint32_t CFGR_AWDCH1CH =            // AWDCH1CH (5 bits)
        bit_field_t<26, 0x1f>::value<X>();
    static constexpr uint32_t CFGR_JAUTO = 0x2000000;    // JAUTO
    static constexpr uint32_t CFGR_JAWD1EN = 0x1000000;  // JAWD1EN
    static constexpr uint32_t CFGR_AWD1EN = 0x800000;    // AWD1EN
    static constexpr uint32_t CFGR_AWD1SGL = 0x400000;   // AWD1SGL
    static constexpr uint32_t CFGR_JQM = 0x200000;       // JQM
    static constexpr uint32_t CFGR_JDISCEN = 0x100000;   // JDISCEN
    template<uint32_t X>
    static constexpr uint32_t CFGR_DISCNUM =             // DISCNUM (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    static constexpr uint32_t CFGR_DISCEN = 0x10000;     // DISCEN
    static constexpr uint32_t CFGR_ALIGN = 0x8000;       // ALIGN
    static constexpr uint32_t CFGR_AUTDLY = 0x4000;      // AUTDLY
    static constexpr uint32_t CFGR_CONT = 0x2000;        // CONT
    static constexpr uint32_t CFGR_OVRMOD = 0x1000;      // OVRMOD
    template<uint32_t X>
    static constexpr uint32_t CFGR_EXTEN =               // EXTEN (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_EXTSEL =              // EXTSEL (4 bits)
        bit_field_t<6, 0xf>::value<X>();
    static constexpr uint32_t CFGR_ALIGN_5 = 0x20;       // ALIGN_5
    template<uint32_t X>
    static constexpr uint32_t CFGR_RES =                 // RES (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t CFGR_DMACFG = 0x2;         // DMACFG
    static constexpr uint32_t CFGR_DMAEN = 0x1;          // DMAEN
    static const uint32_t CFGR_RESET_VALUE = 0x80000000;

    static constexpr uint32_t CFGR2_SMPTRIG = 0x8000000;  // SMPTRIG
    static constexpr uint32_t CFGR2_BULB = 0x4000000;     // BULB
    static constexpr uint32_t CFGR2_SWTRIG = 0x2000000;   // SWTRIG
    static constexpr uint32_t CFGR2_GCOMP = 0x10000;      // GCOMP
    static constexpr uint32_t CFGR2_ROVSM = 0x400;        // EXTEN
    static constexpr uint32_t CFGR2_TROVS = 0x200;        // Triggered Regular Oversampling
    template<uint32_t X>
    static constexpr uint32_t CFGR2_OVSS =                // ALIGN (4 bits)
        bit_field_t<5, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR2_OVSR =                // RES (3 bits)
        bit_field_t<2, 0x7>::value<X>();
    static constexpr uint32_t CFGR2_JOVSE = 0x2;          // DMACFG
    static constexpr uint32_t CFGR2_ROVSE = 0x1;          // DMAEN
    static const uint32_t CFGR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP9 =                // SMP9 (3 bits)
        bit_field_t<27, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP8 =                // SMP8 (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP7 =                // SMP7 (3 bits)
        bit_field_t<21, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP6 =                // SMP6 (3 bits)
        bit_field_t<18, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP5 =                // SMP5 (3 bits)
        bit_field_t<15, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP4 =                // SMP4 (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP3 =                // SMP3 (3 bits)
        bit_field_t<9, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP2 =                // SMP2 (3 bits)
        bit_field_t<6, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP1 =                // SMP1 (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    static constexpr uint32_t SMPR1_SMPPLUS = 0x80000000; // Addition of one clock cycle to the sampling time
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP0 =                // SMP0 (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t SMPR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP18 =               // SMP18 (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP17 =               // SMP17 (3 bits)
        bit_field_t<21, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP16 =               // SMP16 (3 bits)
        bit_field_t<18, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP15 =               // SMP15 (3 bits)
        bit_field_t<15, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP14 =               // SMP14 (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP13 =               // SMP13 (3 bits)
        bit_field_t<9, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP12 =               // SMP12 (3 bits)
        bit_field_t<6, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP11 =               // SMP11 (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP10 =               // SMP10 (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t SMPR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TR1_HT1 =                 // HT1 (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR1_AWDFILT =             // AWDFILT (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR1_LT1 =                 // LT1 (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t TR1_RESET_VALUE = 0xfff0000;

    template<uint32_t X>
    static constexpr uint32_t TR2_HT2 =                 // HT2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR2_LT2 =                 // LT2 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TR2_RESET_VALUE = 0xff0000;

    template<uint32_t X>
    static constexpr uint32_t TR3_HT3 =                 // HT3 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR3_LT3 =                 // LT3 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TR3_RESET_VALUE = 0xff0000;

    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ4 =                 // SQ4 (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ3 =                 // SQ3 (5 bits)
        bit_field_t<18, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ2 =                 // SQ2 (5 bits)
        bit_field_t<12, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ1 =                 // SQ1 (5 bits)
        bit_field_t<6, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_L =                   // Regular channel sequence length (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t SQR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ9 =                 // SQ9 (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ8 =                 // SQ8 (5 bits)
        bit_field_t<18, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ7 =                 // SQ7 (5 bits)
        bit_field_t<12, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ6 =                 // SQ6 (5 bits)
        bit_field_t<6, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ5 =                 // SQ5 (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ14 =                // SQ14 (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ13 =                // SQ13 (5 bits)
        bit_field_t<18, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ12 =                // SQ12 (5 bits)
        bit_field_t<12, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ11 =                // SQ11 (5 bits)
        bit_field_t<6, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ10 =                // SQ10 (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR4_SQ16 =                // SQ16 (5 bits)
        bit_field_t<6, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR4_SQ15 =                // SQ15 (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR_RDATA =               // Regular Data converted (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ4 =                // JSQ4 (5 bits)
        bit_field_t<27, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ3 =                // JSQ3 (5 bits)
        bit_field_t<21, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ2 =                // JSQ2 (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ1 =                // JSQ1 (5 bits)
        bit_field_t<9, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JEXTEN =              // JEXTEN (2 bits)
        bit_field_t<7, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JEXTSEL =             // JEXTSEL (5 bits)
        bit_field_t<2, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JL =                  // JL (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t JSQR_RESET_VALUE = 0x0;

    static constexpr uint32_t OFR1_OFFSET1_EN = 0x80000000;// OFFSET1_EN
    template<uint32_t X>
    static constexpr uint32_t OFR1_OFFSET1_CH =          // OFFSET1_CH (5 bits)
        bit_field_t<26, 0x1f>::value<X>();
    static constexpr uint32_t OFR1_SATEN = 0x2000000;    // SATEN
    static constexpr uint32_t OFR1_OFFSETPOS = 0x1000000;// OFFSETPOS
    template<uint32_t X>
    static constexpr uint32_t OFR1_OFFSET1 =             // OFFSET1 (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t OFR1_RESET_VALUE = 0x0;

    static constexpr uint32_t OFR2_OFFSET1_EN = 0x80000000;// OFFSET1_EN
    template<uint32_t X>
    static constexpr uint32_t OFR2_OFFSET1_CH =          // OFFSET1_CH (5 bits)
        bit_field_t<26, 0x1f>::value<X>();
    static constexpr uint32_t OFR2_SATEN = 0x2000000;    // SATEN
    static constexpr uint32_t OFR2_OFFSETPOS = 0x1000000;// OFFSETPOS
    template<uint32_t X>
    static constexpr uint32_t OFR2_OFFSET1 =             // OFFSET1 (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t OFR2_RESET_VALUE = 0x0;

    static constexpr uint32_t OFR3_OFFSET1_EN = 0x80000000;// OFFSET1_EN
    template<uint32_t X>
    static constexpr uint32_t OFR3_OFFSET1_CH =          // OFFSET1_CH (5 bits)
        bit_field_t<26, 0x1f>::value<X>();
    static constexpr uint32_t OFR3_SATEN = 0x2000000;    // SATEN
    static constexpr uint32_t OFR3_OFFSETPOS = 0x1000000;// OFFSETPOS
    template<uint32_t X>
    static constexpr uint32_t OFR3_OFFSET1 =             // OFFSET1 (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t OFR3_RESET_VALUE = 0x0;

    static constexpr uint32_t OFR4_OFFSET1_EN = 0x80000000;// OFFSET1_EN
    template<uint32_t X>
    static constexpr uint32_t OFR4_OFFSET1_CH =          // OFFSET1_CH (5 bits)
        bit_field_t<26, 0x1f>::value<X>();
    static constexpr uint32_t OFR4_SATEN = 0x2000000;    // SATEN
    static constexpr uint32_t OFR4_OFFSETPOS = 0x1000000;// OFFSETPOS
    template<uint32_t X>
    static constexpr uint32_t OFR4_OFFSET1 =             // OFFSET1 (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t OFR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR1_JDATA1 =              // JDATA1 (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR2_JDATA2 =              // JDATA2 (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR3_JDATA3 =              // JDATA3 (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR4_JDATA4 =              // JDATA4 (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AWD2CR_AWD2CH =              // AWD2CH (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    static const uint32_t AWD2CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AWD3CR_AWD3CH =              // AWD3CH (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    static const uint32_t AWD3CR_RESET_VALUE = 0x0;

    static constexpr uint32_t DIFSEL_DIFSEL_0 = 0x1;       // Differential mode for channels 0, Read-only
    template<uint32_t X>
    static constexpr uint32_t DIFSEL_DIFSEL_1_18 =         // Differential mode for channels 15 to 1 (18 bits), Read-write
        bit_field_t<1, 0x3ffff>::value<X>();
    static const uint32_t DIFSEL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CALFACT_CALFACT_D =           // CALFACT_D (7 bits)
        bit_field_t<16, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CALFACT_CALFACT_S =           // CALFACT_S (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t CALFACT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t GCOMP_GCOMPCOEFF =          // GCOMPCOEFF (14 bits)
        bit_field_t<0, 0x3fff>::value<X>();
    static const uint32_t GCOMP_RESET_VALUE = 0x0;

    static constexpr uint8_t ADC1_2 = 18; // ADC1 and ADC2 global interrupt
};

static adc1_t& ADC1 = *reinterpret_cast<adc1_t*>(0x50000000);

#define HAVE_PERIPHERAL_ADC1


////
//
//    Analog-to-Digital Converter
//
////

struct adc2_t
{
    volatile uint32_t    ISR;                  // [Read-write] interrupt and status register
    volatile uint32_t    IER;                  // [Read-write] interrupt enable register
    volatile uint32_t    CR;                   // [Read-write] control register
    volatile uint32_t    CFGR;                 // [Read-write] configuration register
    volatile uint32_t    CFGR2;                // [Read-write] configuration register
    volatile uint32_t    SMPR1;                // [Read-write] sample time register 1
    volatile uint32_t    SMPR2;                // [Read-write] sample time register 2
    reserved_t<1>        _0;
    volatile uint32_t    TR1;                  // [Read-write] watchdog threshold register 1
    volatile uint32_t    TR2;                  // [Read-write] watchdog threshold register
    volatile uint32_t    TR3;                  // [Read-write] watchdog threshold register 3
    reserved_t<1>        _1;
    volatile uint32_t    SQR1;                 // [Read-write] regular sequence register 1
    volatile uint32_t    SQR2;                 // [Read-write] regular sequence register 2
    volatile uint32_t    SQR3;                 // [Read-write] regular sequence register 3
    volatile uint32_t    SQR4;                 // [Read-write] regular sequence register 4
    volatile uint32_t    DR;                   // [Read-only] regular Data Register
    reserved_t<2>        _2;
    volatile uint32_t    JSQR;                 // [Read-write] injected sequence register
    reserved_t<4>        _3;
    volatile uint32_t    OFR1;                 // [Read-write] offset register 1
    volatile uint32_t    OFR2;                 // [Read-write] offset register 2
    volatile uint32_t    OFR3;                 // [Read-write] offset register 3
    volatile uint32_t    OFR4;                 // [Read-write] offset register 4
    reserved_t<4>        _4;
    volatile uint32_t    JDR1;                 // [Read-only] injected data register 1
    volatile uint32_t    JDR2;                 // [Read-only] injected data register 2
    volatile uint32_t    JDR3;                 // [Read-only] injected data register 3
    volatile uint32_t    JDR4;                 // [Read-only] injected data register 4
    reserved_t<4>        _5;
    volatile uint32_t    AWD2CR;               // [Read-write] Analog Watchdog 2 Configuration Register
    volatile uint32_t    AWD3CR;               // [Read-write] Analog Watchdog 3 Configuration Register
    reserved_t<2>        _6;
    volatile uint32_t    DIFSEL;               // Differential Mode Selection Register 2
    volatile uint32_t    CALFACT;              // [Read-write] Calibration Factors
    reserved_t<2>        _7;
    volatile uint32_t    GCOMP;                // [Read-write] Gain compensation Register

    static constexpr uint32_t ISR_JQOVF = 0x400;        // JQOVF
    static constexpr uint32_t ISR_AWD3 = 0x200;         // AWD3
    static constexpr uint32_t ISR_AWD2 = 0x100;         // AWD2
    static constexpr uint32_t ISR_AWD1 = 0x80;          // AWD1
    static constexpr uint32_t ISR_JEOS = 0x40;          // JEOS
    static constexpr uint32_t ISR_JEOC = 0x20;          // JEOC
    static constexpr uint32_t ISR_OVR = 0x10;           // OVR
    static constexpr uint32_t ISR_EOS = 0x8;            // EOS
    static constexpr uint32_t ISR_EOC = 0x4;            // EOC
    static constexpr uint32_t ISR_EOSMP = 0x2;          // EOSMP
    static constexpr uint32_t ISR_ADRDY = 0x1;          // ADRDY
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t IER_JQOVFIE = 0x400;      // JQOVFIE
    static constexpr uint32_t IER_AWD3IE = 0x200;       // AWD3IE
    static constexpr uint32_t IER_AWD2IE = 0x100;       // AWD2IE
    static constexpr uint32_t IER_AWD1IE = 0x80;        // AWD1IE
    static constexpr uint32_t IER_JEOSIE = 0x40;        // JEOSIE
    static constexpr uint32_t IER_JEOCIE = 0x20;        // JEOCIE
    static constexpr uint32_t IER_OVRIE = 0x10;         // OVRIE
    static constexpr uint32_t IER_EOSIE = 0x8;          // EOSIE
    static constexpr uint32_t IER_EOCIE = 0x4;          // EOCIE
    static constexpr uint32_t IER_EOSMPIE = 0x2;        // EOSMPIE
    static constexpr uint32_t IER_ADRDYIE = 0x1;        // ADRDYIE
    static const uint32_t IER_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_ADCAL = 0x80000000;   // ADCAL
    static constexpr uint32_t CR_ADCALDIF = 0x40000000;// ADCALDIF
    static constexpr uint32_t CR_DEEPPWD = 0x20000000; // DEEPPWD
    static constexpr uint32_t CR_ADVREGEN = 0x10000000;// ADVREGEN
    static constexpr uint32_t CR_JADSTP = 0x20;        // JADSTP
    static constexpr uint32_t CR_ADSTP = 0x10;         // ADSTP
    static constexpr uint32_t CR_JADSTART = 0x8;       // JADSTART
    static constexpr uint32_t CR_ADSTART = 0x4;        // ADSTART
    static constexpr uint32_t CR_ADDIS = 0x2;          // ADDIS
    static constexpr uint32_t CR_ADEN = 0x1;           // ADEN
    static const uint32_t CR_RESET_VALUE = 0x20000000;

    static constexpr uint32_t CFGR_JQDIS = 0x80000000;   // Injected Queue disable
    template<uint32_t X>
    static constexpr uint32_t CFGR_AWDCH1CH =            // AWDCH1CH (5 bits)
        bit_field_t<26, 0x1f>::value<X>();
    static constexpr uint32_t CFGR_JAUTO = 0x2000000;    // JAUTO
    static constexpr uint32_t CFGR_JAWD1EN = 0x1000000;  // JAWD1EN
    static constexpr uint32_t CFGR_AWD1EN = 0x800000;    // AWD1EN
    static constexpr uint32_t CFGR_AWD1SGL = 0x400000;   // AWD1SGL
    static constexpr uint32_t CFGR_JQM = 0x200000;       // JQM
    static constexpr uint32_t CFGR_JDISCEN = 0x100000;   // JDISCEN
    template<uint32_t X>
    static constexpr uint32_t CFGR_DISCNUM =             // DISCNUM (3 bits)
        bit_field_t<17, 0x7>::value<X>();
    static constexpr uint32_t CFGR_DISCEN = 0x10000;     // DISCEN
    static constexpr uint32_t CFGR_ALIGN = 0x8000;       // ALIGN
    static constexpr uint32_t CFGR_AUTDLY = 0x4000;      // AUTDLY
    static constexpr uint32_t CFGR_CONT = 0x2000;        // CONT
    static constexpr uint32_t CFGR_OVRMOD = 0x1000;      // OVRMOD
    template<uint32_t X>
    static constexpr uint32_t CFGR_EXTEN =               // EXTEN (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_EXTSEL =              // EXTSEL (4 bits)
        bit_field_t<6, 0xf>::value<X>();
    static constexpr uint32_t CFGR_ALIGN_5 = 0x20;       // ALIGN_5
    template<uint32_t X>
    static constexpr uint32_t CFGR_RES =                 // RES (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t CFGR_DMACFG = 0x2;         // DMACFG
    static constexpr uint32_t CFGR_DMAEN = 0x1;          // DMAEN
    static const uint32_t CFGR_RESET_VALUE = 0x80000000;

    static constexpr uint32_t CFGR2_SMPTRIG = 0x8000000;  // SMPTRIG
    static constexpr uint32_t CFGR2_BULB = 0x4000000;     // BULB
    static constexpr uint32_t CFGR2_SWTRIG = 0x2000000;   // SWTRIG
    static constexpr uint32_t CFGR2_GCOMP = 0x10000;      // GCOMP
    static constexpr uint32_t CFGR2_ROVSM = 0x400;        // EXTEN
    static constexpr uint32_t CFGR2_TROVS = 0x200;        // Triggered Regular Oversampling
    template<uint32_t X>
    static constexpr uint32_t CFGR2_OVSS =                // ALIGN (4 bits)
        bit_field_t<5, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR2_OVSR =                // RES (3 bits)
        bit_field_t<2, 0x7>::value<X>();
    static constexpr uint32_t CFGR2_JOVSE = 0x2;          // DMACFG
    static constexpr uint32_t CFGR2_ROVSE = 0x1;          // DMAEN
    static const uint32_t CFGR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP9 =                // SMP9 (3 bits)
        bit_field_t<27, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP8 =                // SMP8 (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP7 =                // SMP7 (3 bits)
        bit_field_t<21, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP6 =                // SMP6 (3 bits)
        bit_field_t<18, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP5 =                // SMP5 (3 bits)
        bit_field_t<15, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP4 =                // SMP4 (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP3 =                // SMP3 (3 bits)
        bit_field_t<9, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP2 =                // SMP2 (3 bits)
        bit_field_t<6, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP1 =                // SMP1 (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    static constexpr uint32_t SMPR1_SMPPLUS = 0x80000000; // Addition of one clock cycle to the sampling time
    template<uint32_t X>
    static constexpr uint32_t SMPR1_SMP0 =                // SMP0 (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t SMPR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP18 =               // SMP18 (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP17 =               // SMP17 (3 bits)
        bit_field_t<21, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP16 =               // SMP16 (3 bits)
        bit_field_t<18, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP15 =               // SMP15 (3 bits)
        bit_field_t<15, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP14 =               // SMP14 (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP13 =               // SMP13 (3 bits)
        bit_field_t<9, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP12 =               // SMP12 (3 bits)
        bit_field_t<6, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP11 =               // SMP11 (3 bits)
        bit_field_t<3, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SMPR2_SMP10 =               // SMP10 (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t SMPR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TR1_HT1 =                 // HT1 (12 bits)
        bit_field_t<16, 0xfff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR1_AWDFILT =             // AWDFILT (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR1_LT1 =                 // LT1 (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t TR1_RESET_VALUE = 0xfff0000;

    template<uint32_t X>
    static constexpr uint32_t TR2_HT2 =                 // HT2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR2_LT2 =                 // LT2 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TR2_RESET_VALUE = 0xff0000;

    template<uint32_t X>
    static constexpr uint32_t TR3_HT3 =                 // HT3 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TR3_LT3 =                 // LT3 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t TR3_RESET_VALUE = 0xff0000;

    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ4 =                 // SQ4 (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ3 =                 // SQ3 (5 bits)
        bit_field_t<18, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ2 =                 // SQ2 (5 bits)
        bit_field_t<12, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_SQ1 =                 // SQ1 (5 bits)
        bit_field_t<6, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR1_L =                   // Regular channel sequence length (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t SQR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ9 =                 // SQ9 (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ8 =                 // SQ8 (5 bits)
        bit_field_t<18, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ7 =                 // SQ7 (5 bits)
        bit_field_t<12, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ6 =                 // SQ6 (5 bits)
        bit_field_t<6, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR2_SQ5 =                 // SQ5 (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ14 =                // SQ14 (5 bits)
        bit_field_t<24, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ13 =                // SQ13 (5 bits)
        bit_field_t<18, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ12 =                // SQ12 (5 bits)
        bit_field_t<12, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ11 =                // SQ11 (5 bits)
        bit_field_t<6, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR3_SQ10 =                // SQ10 (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t SQR4_SQ16 =                // SQ16 (5 bits)
        bit_field_t<6, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SQR4_SQ15 =                // SQ15 (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t SQR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DR_RDATA =               // Regular Data converted (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t DR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ4 =                // JSQ4 (5 bits)
        bit_field_t<27, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ3 =                // JSQ3 (5 bits)
        bit_field_t<21, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ2 =                // JSQ2 (5 bits)
        bit_field_t<15, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JSQ1 =                // JSQ1 (5 bits)
        bit_field_t<9, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JEXTEN =              // JEXTEN (2 bits)
        bit_field_t<7, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JEXTSEL =             // JEXTSEL (5 bits)
        bit_field_t<2, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t JSQR_JL =                  // JL (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t JSQR_RESET_VALUE = 0x0;

    static constexpr uint32_t OFR1_OFFSET1_EN = 0x80000000;// OFFSET1_EN
    template<uint32_t X>
    static constexpr uint32_t OFR1_OFFSET1_CH =          // OFFSET1_CH (5 bits)
        bit_field_t<26, 0x1f>::value<X>();
    static constexpr uint32_t OFR1_SATEN = 0x2000000;    // SATEN
    static constexpr uint32_t OFR1_OFFSETPOS = 0x1000000;// OFFSETPOS
    template<uint32_t X>
    static constexpr uint32_t OFR1_OFFSET1 =             // OFFSET1 (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t OFR1_RESET_VALUE = 0x0;

    static constexpr uint32_t OFR2_OFFSET1_EN = 0x80000000;// OFFSET1_EN
    template<uint32_t X>
    static constexpr uint32_t OFR2_OFFSET1_CH =          // OFFSET1_CH (5 bits)
        bit_field_t<26, 0x1f>::value<X>();
    static constexpr uint32_t OFR2_SATEN = 0x2000000;    // SATEN
    static constexpr uint32_t OFR2_OFFSETPOS = 0x1000000;// OFFSETPOS
    template<uint32_t X>
    static constexpr uint32_t OFR2_OFFSET1 =             // OFFSET1 (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t OFR2_RESET_VALUE = 0x0;

    static constexpr uint32_t OFR3_OFFSET1_EN = 0x80000000;// OFFSET1_EN
    template<uint32_t X>
    static constexpr uint32_t OFR3_OFFSET1_CH =          // OFFSET1_CH (5 bits)
        bit_field_t<26, 0x1f>::value<X>();
    static constexpr uint32_t OFR3_SATEN = 0x2000000;    // SATEN
    static constexpr uint32_t OFR3_OFFSETPOS = 0x1000000;// OFFSETPOS
    template<uint32_t X>
    static constexpr uint32_t OFR3_OFFSET1 =             // OFFSET1 (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t OFR3_RESET_VALUE = 0x0;

    static constexpr uint32_t OFR4_OFFSET1_EN = 0x80000000;// OFFSET1_EN
    template<uint32_t X>
    static constexpr uint32_t OFR4_OFFSET1_CH =          // OFFSET1_CH (5 bits)
        bit_field_t<26, 0x1f>::value<X>();
    static constexpr uint32_t OFR4_SATEN = 0x2000000;    // SATEN
    static constexpr uint32_t OFR4_OFFSETPOS = 0x1000000;// OFFSETPOS
    template<uint32_t X>
    static constexpr uint32_t OFR4_OFFSET1 =             // OFFSET1 (12 bits)
        bit_field_t<0, 0xfff>::value<X>();
    static const uint32_t OFR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR1_JDATA1 =              // JDATA1 (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR1_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR2_JDATA2 =              // JDATA2 (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR2_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR3_JDATA3 =              // JDATA3 (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR3_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t JDR4_JDATA4 =              // JDATA4 (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t JDR4_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AWD2CR_AWD2CH =              // AWD2CH (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    static const uint32_t AWD2CR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t AWD3CR_AWD3CH =              // AWD3CH (19 bits)
        bit_field_t<0, 0x7ffff>::value<X>();
    static const uint32_t AWD3CR_RESET_VALUE = 0x0;

    static constexpr uint32_t DIFSEL_DIFSEL_0 = 0x1;       // Differential mode for channels 0, Read-only
    template<uint32_t X>
    static constexpr uint32_t DIFSEL_DIFSEL_1_18 =         // Differential mode for channels 15 to 1 (18 bits), Read-write
        bit_field_t<1, 0x3ffff>::value<X>();
    static const uint32_t DIFSEL_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CALFACT_CALFACT_D =           // CALFACT_D (7 bits)
        bit_field_t<16, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CALFACT_CALFACT_S =           // CALFACT_S (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t CALFACT_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t GCOMP_GCOMPCOEFF =          // GCOMPCOEFF (14 bits)
        bit_field_t<0, 0x3fff>::value<X>();
    static const uint32_t GCOMP_RESET_VALUE = 0x0;
};

static adc2_t& ADC2 = *reinterpret_cast<adc2_t*>(0x50000100);

#define HAVE_PERIPHERAL_ADC2


////
//
//    Analog-to-Digital Converter
//
////

struct adc12_common_t
{
    volatile uint32_t    CSR;                  // [Read-only] ADC Common status register
    reserved_t<1>        _0;
    volatile uint32_t    CCR;                  // [Read-write] ADC common control register
    volatile uint32_t    CDR;                  // [Read-only] ADC common regular data register for dual and triple modes

    static constexpr uint32_t CSR_ADDRDY_MST = 0x1;     // ADDRDY_MST
    static constexpr uint32_t CSR_EOSMP_MST = 0x2;      // EOSMP_MST
    static constexpr uint32_t CSR_EOC_MST = 0x4;        // EOC_MST
    static constexpr uint32_t CSR_EOS_MST = 0x8;        // EOS_MST
    static constexpr uint32_t CSR_OVR_MST = 0x10;       // OVR_MST
    static constexpr uint32_t CSR_JEOC_MST = 0x20;      // JEOC_MST
    static constexpr uint32_t CSR_JEOS_MST = 0x40;      // JEOS_MST
    static constexpr uint32_t CSR_AWD1_MST = 0x80;      // AWD1_MST
    static constexpr uint32_t CSR_AWD2_MST = 0x100;     // AWD2_MST
    static constexpr uint32_t CSR_AWD3_MST = 0x200;     // AWD3_MST
    static constexpr uint32_t CSR_JQOVF_MST = 0x400;    // JQOVF_MST
    static constexpr uint32_t CSR_ADRDY_SLV = 0x10000;  // ADRDY_SLV
    static constexpr uint32_t CSR_EOSMP_SLV = 0x20000;  // EOSMP_SLV
    static constexpr uint32_t CSR_EOC_SLV = 0x40000;    // End of regular conversion of the slave ADC
    static constexpr uint32_t CSR_EOS_SLV = 0x80000;    // End of regular sequence flag of the slave ADC
    static constexpr uint32_t CSR_OVR_SLV = 0x100000;   // Overrun flag of the slave ADC
    static constexpr uint32_t CSR_JEOC_SLV = 0x200000;  // End of injected conversion flag of the slave ADC
    static constexpr uint32_t CSR_JEOS_SLV = 0x400000;  // End of injected sequence flag of the slave ADC
    static constexpr uint32_t CSR_AWD1_SLV = 0x800000;  // Analog watchdog 1 flag of the slave ADC
    static constexpr uint32_t CSR_AWD2_SLV = 0x1000000; // Analog watchdog 2 flag of the slave ADC
    static constexpr uint32_t CSR_AWD3_SLV = 0x2000000; // Analog watchdog 3 flag of the slave ADC
    static constexpr uint32_t CSR_JQOVF_SLV = 0x4000000;// Injected Context Queue Overflow flag of the slave ADC
    static const uint32_t CSR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR_DUAL =                // Dual ADC mode selection (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR_DELAY =               // Delay between 2 sampling phases (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t CCR_DMACFG = 0x2000;      // DMA configuration (for multi-ADC mode)
    template<uint32_t X>
    static constexpr uint32_t CCR_MDMA =                // Direct memory access mode for multi ADC mode (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR_CKMODE =              // ADC clock mode (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    static constexpr uint32_t CCR_VREFEN = 0x400000;    // VREFINT enable
    static constexpr uint32_t CCR_CH17SEL = 0x800000;   // CH17 selection
    static constexpr uint32_t CCR_CH18SEL = 0x1000000;  // CH18 selection
    template<uint32_t X>
    static constexpr uint32_t CCR_PRESC =               // ADC prescaler (4 bits)
        bit_field_t<18, 0xf>::value<X>();
    static const uint32_t CCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CDR_RDATA_SLV =           // Regular data of the slave ADC (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CDR_RDATA_MST =           // Regular data of the master ADC (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CDR_RESET_VALUE = 0x0;
};

static adc12_common_t& ADC12_Common = *reinterpret_cast<adc12_common_t*>(0x50000200);

#define HAVE_PERIPHERAL_ADC12_Common


////
//
//    Analog-to-Digital Converter
//
////

struct adc345_common_t
{
    volatile uint32_t    CSR;                  // [Read-only] ADC Common status register
    reserved_t<1>        _0;
    volatile uint32_t    CCR;                  // [Read-write] ADC common control register
    volatile uint32_t    CDR;                  // [Read-only] ADC common regular data register for dual and triple modes

    static constexpr uint32_t CSR_ADDRDY_MST = 0x1;     // ADDRDY_MST
    static constexpr uint32_t CSR_EOSMP_MST = 0x2;      // EOSMP_MST
    static constexpr uint32_t CSR_EOC_MST = 0x4;        // EOC_MST
    static constexpr uint32_t CSR_EOS_MST = 0x8;        // EOS_MST
    static constexpr uint32_t CSR_OVR_MST = 0x10;       // OVR_MST
    static constexpr uint32_t CSR_JEOC_MST = 0x20;      // JEOC_MST
    static constexpr uint32_t CSR_JEOS_MST = 0x40;      // JEOS_MST
    static constexpr uint32_t CSR_AWD1_MST = 0x80;      // AWD1_MST
    static constexpr uint32_t CSR_AWD2_MST = 0x100;     // AWD2_MST
    static constexpr uint32_t CSR_AWD3_MST = 0x200;     // AWD3_MST
    static constexpr uint32_t CSR_JQOVF_MST = 0x400;    // JQOVF_MST
    static constexpr uint32_t CSR_ADRDY_SLV = 0x10000;  // ADRDY_SLV
    static constexpr uint32_t CSR_EOSMP_SLV = 0x20000;  // EOSMP_SLV
    static constexpr uint32_t CSR_EOC_SLV = 0x40000;    // End of regular conversion of the slave ADC
    static constexpr uint32_t CSR_EOS_SLV = 0x80000;    // End of regular sequence flag of the slave ADC
    static constexpr uint32_t CSR_OVR_SLV = 0x100000;   // Overrun flag of the slave ADC
    static constexpr uint32_t CSR_JEOC_SLV = 0x200000;  // End of injected conversion flag of the slave ADC
    static constexpr uint32_t CSR_JEOS_SLV = 0x400000;  // End of injected sequence flag of the slave ADC
    static constexpr uint32_t CSR_AWD1_SLV = 0x800000;  // Analog watchdog 1 flag of the slave ADC
    static constexpr uint32_t CSR_AWD2_SLV = 0x1000000; // Analog watchdog 2 flag of the slave ADC
    static constexpr uint32_t CSR_AWD3_SLV = 0x2000000; // Analog watchdog 3 flag of the slave ADC
    static constexpr uint32_t CSR_JQOVF_SLV = 0x4000000;// Injected Context Queue Overflow flag of the slave ADC
    static const uint32_t CSR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CCR_DUAL =                // Dual ADC mode selection (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR_DELAY =               // Delay between 2 sampling phases (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    static constexpr uint32_t CCR_DMACFG = 0x2000;      // DMA configuration (for multi-ADC mode)
    template<uint32_t X>
    static constexpr uint32_t CCR_MDMA =                // Direct memory access mode for multi ADC mode (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CCR_CKMODE =              // ADC clock mode (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    static constexpr uint32_t CCR_VREFEN = 0x400000;    // VREFINT enable
    static constexpr uint32_t CCR_CH17SEL = 0x800000;   // CH17 selection
    static constexpr uint32_t CCR_CH18SEL = 0x1000000;  // CH18 selection
    template<uint32_t X>
    static constexpr uint32_t CCR_PRESC =               // ADC prescaler (4 bits)
        bit_field_t<18, 0xf>::value<X>();
    static const uint32_t CCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CDR_RDATA_SLV =           // Regular data of the slave ADC (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CDR_RDATA_MST =           // Regular data of the master ADC (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t CDR_RESET_VALUE = 0x0;
};

static adc345_common_t& ADC345_Common = *reinterpret_cast<adc345_common_t*>(0x50000700);

#define HAVE_PERIPHERAL_ADC345_Common


////
//
//    Filter Math Accelerator
//
////

struct fmac_t
{
    volatile uint32_t    X1BUFCFG;             // [Read-write] FMAC X1 Buffer Configuration register
    volatile uint32_t    X2BUFCFG;             // [Read-write] FMAC X2 Buffer Configuration register
    volatile uint32_t    YBUFCFG;              // [Read-write] FMAC Y Buffer Configuration register
    volatile uint32_t    PARAM;                // [Read-write] FMAC Parameter register
    volatile uint32_t    CR;                   // [Read-write] FMAC Control register
    volatile uint32_t    SR;                   // [Read-only] FMAC Status register
    volatile uint32_t    WDATA;                // [Write-only] FMAC Write Data register
    volatile uint32_t    RDATA;                // [Read-only] FMAC Read Data register

    template<uint32_t X>
    static constexpr uint32_t X1BUFCFG_X1_BASE =             // X1_BASE (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t X1BUFCFG_X1_BUF_SIZE =         // X1_BUF_SIZE (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t X1BUFCFG_FULL_WM =             // FULL_WM (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    static const uint32_t X1BUFCFG_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t X2BUFCFG_X2_BASE =             // X1_BASE (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t X2BUFCFG_X2_BUF_SIZE =         // X1_BUF_SIZE (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t X2BUFCFG_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t YBUFCFG_Y_BASE =              // X1_BASE (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t YBUFCFG_Y_BUF_SIZE =          // X1_BUF_SIZE (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t YBUFCFG_EMPTY_WM =            // EMPTY_WM (2 bits)
        bit_field_t<24, 0x3>::value<X>();
    static const uint32_t YBUFCFG_RESET_VALUE = 0x0;

    static constexpr uint32_t PARAM_START = 0x80000000;   // START
    template<uint32_t X>
    static constexpr uint32_t PARAM_FUNC =                // FUNC (7 bits)
        bit_field_t<24, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PARAM_R =                   // R (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PARAM_Q =                   // Q (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PARAM_P =                   // P (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t PARAM_RESET_VALUE = 0x0;

    static constexpr uint32_t CR_RESET = 0x10000;      // RESET
    static constexpr uint32_t CR_CLIPEN = 0x8000;      // CLIPEN
    static constexpr uint32_t CR_DMAWEN = 0x200;       // DMAWEN
    static constexpr uint32_t CR_DMAREN = 0x100;       // DMAREN
    static constexpr uint32_t CR_SATIEN = 0x10;        // SATIEN
    static constexpr uint32_t CR_UNFLIEN = 0x8;        // UNFLIEN
    static constexpr uint32_t CR_OVFLIEN = 0x4;        // OVFLIEN
    static constexpr uint32_t CR_WIEN = 0x2;           // WIEN
    static constexpr uint32_t CR_RIEN = 0x1;           // RIEN
    static const uint32_t CR_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_YEMPTY = 0x1;         // YEMPTY
    static constexpr uint32_t SR_X1FULL = 0x2;         // X1FULL
    static constexpr uint32_t SR_OVFL = 0x100;         // OVFL
    static constexpr uint32_t SR_UNFL = 0x200;         // UNFL
    static constexpr uint32_t SR_SAT = 0x400;          // SAT
    static const uint32_t SR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t WDATA_WDATA =               // WDATA (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t WDATA_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RDATA_RDATA =               // RDATA (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t RDATA_RESET_VALUE = 0x0;

    static constexpr uint8_t FMAC = 101; // FMAC
};

static fmac_t& FMAC = *reinterpret_cast<fmac_t*>(0x40021400);

#define HAVE_PERIPHERAL_FMAC


////
//
//    CORDIC Co-processor
//
////

struct cordic_t
{
    volatile uint32_t    CSR;                  // [Read-write] CORDIC Control Status register
    volatile uint32_t    WDATA;                // [Read-write] FMAC Write Data register
    volatile uint32_t    RDATA;                // [Read-only] FMAC Read Data register

    template<uint32_t X>
    static constexpr uint32_t CSR_FUNC =                // FUNC (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CSR_PRECISION =           // PRECISION (4 bits)
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CSR_SCALE =               // SCALE (3 bits)
        bit_field_t<8, 0x7>::value<X>();
    static constexpr uint32_t CSR_IEN = 0x10000;        // IEN
    static constexpr uint32_t CSR_DMAREN = 0x20000;     // DMAREN
    static constexpr uint32_t CSR_DMAWEN = 0x40000;     // DMAWEN
    static constexpr uint32_t CSR_NRES = 0x80000;       // NRES
    static constexpr uint32_t CSR_NARGS = 0x100000;     // NARGS
    static constexpr uint32_t CSR_RESSIZE = 0x200000;   // RESSIZE
    static constexpr uint32_t CSR_ARGSIZE = 0x400000;   // ARGSIZE
    static constexpr uint32_t CSR_RRDY = 0x80000000;    // RRDY
    static const uint32_t CSR_RESET_VALUE = 0x0;


    static const uint32_t WDATA_RESET_VALUE = 0x0;


    static const uint32_t RDATA_RESET_VALUE = 0x0;

    static constexpr uint8_t CORDIC = 100; // Cordic
};

static cordic_t& CORDIC = *reinterpret_cast<cordic_t*>(0x40020c00);

#define HAVE_PERIPHERAL_CORDIC


////
//
//    Serial audio interface
//
////

struct sai_t
{
    volatile uint32_t    ACR1;                 // [Read-write] AConfiguration register 1
    volatile uint32_t    ACR2;                 // [Read-write] AConfiguration register 2
    volatile uint32_t    AFRCR;                // [Read-write] AFRCR
    volatile uint32_t    ASLOTR;               // [Read-write] ASlot register
    volatile uint32_t    AIM;                  // [Read-write] AInterrupt mask register2
    volatile uint32_t    ASR;                  // [Read-write] AStatus register
    volatile uint32_t    ACLRFR;               // [Read-write] AClear flag register
    volatile uint32_t    ADR;                  // [Read-write] AData register
    volatile uint32_t    BCR1;                 // [Read-write] BConfiguration register 1
    volatile uint32_t    BCR2;                 // [Read-write] BConfiguration register 2
    volatile uint32_t    BFRCR;                // [Read-write] BFRCR
    volatile uint32_t    BSLOTR;               // [Read-write] BSlot register
    volatile uint32_t    BIM;                  // [Read-write] BInterrupt mask register2
    volatile uint32_t    BSR;                  // [Read-only] BStatus register
    volatile uint32_t    BCLRFR;               // [Write-only] BClear flag register
    volatile uint32_t    BDR;                  // [Read-write] BData register
    volatile uint32_t    PDMCR;                // [Read-write] PDM control register
    volatile uint32_t    PDMDLY;               // [Read-write] PDM delay register

    static constexpr uint32_t ACR1_MCKEN = 0x8000000;    // MCKEN
    static constexpr uint32_t ACR1_OSR = 0x4000000;      // OSR
    template<uint32_t X>
    static constexpr uint32_t ACR1_MCJDIV =              // Master clock divider (6 bits)
        bit_field_t<20, 0x3f>::value<X>();
    static constexpr uint32_t ACR1_NODIV = 0x80000;      // No divider
    static constexpr uint32_t ACR1_DMAEN = 0x20000;      // DMA enable
    static constexpr uint32_t ACR1_SAIAEN = 0x10000;     // Audio block A enable
    static constexpr uint32_t ACR1_OutDri = 0x2000;      // Output drive
    static constexpr uint32_t ACR1_MONO = 0x1000;        // Mono mode
    template<uint32_t X>
    static constexpr uint32_t ACR1_SYNCEN =              // Synchronization enable (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t ACR1_CKSTR = 0x200;        // Clock strobing edge
    static constexpr uint32_t ACR1_LSBFIRST = 0x100;     // Least significant bit first
    template<uint32_t X>
    static constexpr uint32_t ACR1_DS =                  // Data size (3 bits)
        bit_field_t<5, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ACR1_PRTCFG =              // Protocol configuration (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ACR1_MODE =                // Audio block mode (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t ACR1_RESET_VALUE = 0x40;

    template<uint32_t X>
    static constexpr uint32_t ACR2_COMP =                // Companding mode (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    static constexpr uint32_t ACR2_CPL = 0x2000;         // Complement bit
    template<uint32_t X>
    static constexpr uint32_t ACR2_MUTECN =              // Mute counter (6 bits)
        bit_field_t<7, 0x3f>::value<X>();
    static constexpr uint32_t ACR2_MUTEVAL = 0x40;       // Mute value
    static constexpr uint32_t ACR2_MUTE = 0x20;          // Mute
    static constexpr uint32_t ACR2_TRIS = 0x10;          // Tristate management on data line
    static constexpr uint32_t ACR2_FFLUS = 0x8;          // FIFO flush
    template<uint32_t X>
    static constexpr uint32_t ACR2_FTH =                 // FIFO threshold (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t ACR2_RESET_VALUE = 0x0;

    static constexpr uint32_t AFRCR_FSOFF = 0x40000;      // Frame synchronization offset
    static constexpr uint32_t AFRCR_FSPOL = 0x20000;      // Frame synchronization polarity
    static constexpr uint32_t AFRCR_FSDEF = 0x10000;      // Frame synchronization definition
    template<uint32_t X>
    static constexpr uint32_t AFRCR_FSALL =               // Frame synchronization active level length (7 bits)
        bit_field_t<8, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t AFRCR_FRL =                 // Frame length (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t AFRCR_RESET_VALUE = 0x7;

    template<uint32_t X>
    static constexpr uint32_t ASLOTR_SLOTEN =              // Slot enable (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ASLOTR_NBSLOT =              // Number of slots in an audio frame (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ASLOTR_SLOTSZ =              // Slot size (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ASLOTR_FBOFF =               // First bit offset (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t ASLOTR_RESET_VALUE = 0x0;

    static constexpr uint32_t AIM_LFSDET = 0x40;        // Late frame synchronization detection interrupt enable
    static constexpr uint32_t AIM_AFSDETIE = 0x20;      // Anticipated frame synchronization detection interrupt enable
    static constexpr uint32_t AIM_CNRDYIE = 0x10;       // Codec not ready interrupt enable
    static constexpr uint32_t AIM_FREQIE = 0x8;         // FIFO request interrupt enable
    static constexpr uint32_t AIM_WCKCFG = 0x4;         // Wrong clock configuration interrupt enable
    static constexpr uint32_t AIM_MUTEDET = 0x2;        // Mute detection interrupt enable
    static constexpr uint32_t AIM_OVRUDRIE = 0x1;       // Overrun/underrun interrupt enable
    static const uint32_t AIM_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ASR_FLVL =                // FIFO level threshold (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static constexpr uint32_t ASR_LFSDET = 0x40;        // Late frame synchronization detection
    static constexpr uint32_t ASR_AFSDET = 0x20;        // Anticipated frame synchronization detection
    static constexpr uint32_t ASR_CNRDY = 0x10;         // Codec not ready
    static constexpr uint32_t ASR_FREQ = 0x8;           // FIFO request
    static constexpr uint32_t ASR_WCKCFG = 0x4;         // Wrong clock configuration flag. This bit is read only
    static constexpr uint32_t ASR_MUTEDET = 0x2;        // Mute detection
    static constexpr uint32_t ASR_OVRUDR = 0x1;         // Overrun / underrun
    static const uint32_t ASR_RESET_VALUE = 0x0;

    static constexpr uint32_t ACLRFR_LFSDET = 0x40;        // Clear late frame synchronization detection flag
    static constexpr uint32_t ACLRFR_CAFSDET = 0x20;       // Clear anticipated frame synchronization detection flag
    static constexpr uint32_t ACLRFR_CNRDY = 0x10;         // Clear codec not ready flag
    static constexpr uint32_t ACLRFR_WCKCFG = 0x4;         // Clear wrong clock configuration flag
    static constexpr uint32_t ACLRFR_MUTEDET = 0x2;        // Mute detection flag
    static constexpr uint32_t ACLRFR_OVRUDR = 0x1;         // Clear overrun / underrun
    static const uint32_t ACLRFR_RESET_VALUE = 0x0;


    static const uint32_t ADR_RESET_VALUE = 0x0;

    static constexpr uint32_t BCR1_MCKEN = 0x8000000;    // MCKEN
    static constexpr uint32_t BCR1_OSR = 0x4000000;      // OSR
    template<uint32_t X>
    static constexpr uint32_t BCR1_MCJDIV =              // Master clock divider (6 bits)
        bit_field_t<20, 0x3f>::value<X>();
    static constexpr uint32_t BCR1_NODIV = 0x80000;      // No divider
    static constexpr uint32_t BCR1_DMAEN = 0x20000;      // DMA enable
    static constexpr uint32_t BCR1_SAIBEN = 0x10000;     // Audio block B enable
    static constexpr uint32_t BCR1_OutDri = 0x2000;      // Output drive
    static constexpr uint32_t BCR1_MONO = 0x1000;        // Mono mode
    template<uint32_t X>
    static constexpr uint32_t BCR1_SYNCEN =              // Synchronization enable (2 bits)
        bit_field_t<10, 0x3>::value<X>();
    static constexpr uint32_t BCR1_CKSTR = 0x200;        // Clock strobing edge
    static constexpr uint32_t BCR1_LSBFIRST = 0x100;     // Least significant bit first
    template<uint32_t X>
    static constexpr uint32_t BCR1_DS =                  // Data size (3 bits)
        bit_field_t<5, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BCR1_PRTCFG =              // Protocol configuration (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BCR1_MODE =                // Audio block mode (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t BCR1_RESET_VALUE = 0x40;

    template<uint32_t X>
    static constexpr uint32_t BCR2_COMP =                // Companding mode (2 bits)
        bit_field_t<14, 0x3>::value<X>();
    static constexpr uint32_t BCR2_CPL = 0x2000;         // Complement bit
    template<uint32_t X>
    static constexpr uint32_t BCR2_MUTECN =              // Mute counter (6 bits)
        bit_field_t<7, 0x3f>::value<X>();
    static constexpr uint32_t BCR2_MUTEVAL = 0x40;       // Mute value
    static constexpr uint32_t BCR2_MUTE = 0x20;          // Mute
    static constexpr uint32_t BCR2_TRIS = 0x10;          // Tristate management on data line
    static constexpr uint32_t BCR2_FFLUS = 0x8;          // FIFO flush
    template<uint32_t X>
    static constexpr uint32_t BCR2_FTH =                 // FIFO threshold (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t BCR2_RESET_VALUE = 0x0;

    static constexpr uint32_t BFRCR_FSOFF = 0x40000;      // Frame synchronization offset
    static constexpr uint32_t BFRCR_FSPOL = 0x20000;      // Frame synchronization polarity
    static constexpr uint32_t BFRCR_FSDEF = 0x10000;      // Frame synchronization definition
    template<uint32_t X>
    static constexpr uint32_t BFRCR_FSALL =               // Frame synchronization active level length (7 bits)
        bit_field_t<8, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BFRCR_FRL =                 // Frame length (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t BFRCR_RESET_VALUE = 0x7;

    template<uint32_t X>
    static constexpr uint32_t BSLOTR_SLOTEN =              // Slot enable (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BSLOTR_NBSLOT =              // Number of slots in an audio frame (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BSLOTR_SLOTSZ =              // Slot size (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t BSLOTR_FBOFF =               // First bit offset (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t BSLOTR_RESET_VALUE = 0x0;

    static constexpr uint32_t BIM_LFSDETIE = 0x40;      // Late frame synchronization detection interrupt enable
    static constexpr uint32_t BIM_AFSDETIE = 0x20;      // Anticipated frame synchronization detection interrupt enable
    static constexpr uint32_t BIM_CNRDYIE = 0x10;       // Codec not ready interrupt enable
    static constexpr uint32_t BIM_FREQIE = 0x8;         // FIFO request interrupt enable
    static constexpr uint32_t BIM_WCKCFG = 0x4;         // Wrong clock configuration interrupt enable
    static constexpr uint32_t BIM_MUTEDET = 0x2;        // Mute detection interrupt enable
    static constexpr uint32_t BIM_OVRUDRIE = 0x1;       // Overrun/underrun interrupt enable
    static const uint32_t BIM_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BSR_FLVL =                // FIFO level threshold (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static constexpr uint32_t BSR_LFSDET = 0x40;        // Late frame synchronization detection
    static constexpr uint32_t BSR_AFSDET = 0x20;        // Anticipated frame synchronization detection
    static constexpr uint32_t BSR_CNRDY = 0x10;         // Codec not ready
    static constexpr uint32_t BSR_FREQ = 0x8;           // FIFO request
    static constexpr uint32_t BSR_WCKCFG = 0x4;         // Wrong clock configuration flag
    static constexpr uint32_t BSR_MUTEDET = 0x2;        // Mute detection
    static constexpr uint32_t BSR_OVRUDR = 0x1;         // Overrun / underrun
    static const uint32_t BSR_RESET_VALUE = 0x0;

    static constexpr uint32_t BCLRFR_LFSDET = 0x40;        // Clear late frame synchronization detection flag
    static constexpr uint32_t BCLRFR_CAFSDET = 0x20;       // Clear anticipated frame synchronization detection flag
    static constexpr uint32_t BCLRFR_CNRDY = 0x10;         // Clear codec not ready flag
    static constexpr uint32_t BCLRFR_WCKCFG = 0x4;         // Clear wrong clock configuration flag
    static constexpr uint32_t BCLRFR_MUTEDET = 0x2;        // Mute detection flag
    static constexpr uint32_t BCLRFR_OVRUDR = 0x1;         // Clear overrun / underrun
    static const uint32_t BCLRFR_RESET_VALUE = 0x0;


    static const uint32_t BDR_RESET_VALUE = 0x0;

    static constexpr uint32_t PDMCR_PDMEN = 0x1;          // PDMEN
    template<uint32_t X>
    static constexpr uint32_t PDMCR_MICNBR =              // MICNBR (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t PDMCR_CKEN1 = 0x100;        // CKEN1
    static constexpr uint32_t PDMCR_CKEN2 = 0x200;        // CKEN2
    static constexpr uint32_t PDMCR_CKEN3 = 0x400;        // CKEN3
    static constexpr uint32_t PDMCR_CKEN4 = 0x800;        // CKEN4
    static const uint32_t PDMCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PDMDLY_DLYM1L =              // DLYM1L (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PDMDLY_DLYM1R =              // DLYM1R (3 bits)
        bit_field_t<4, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PDMDLY_DLYM2L =              // DLYM2L (3 bits)
        bit_field_t<8, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PDMDLY_DLYM2R =              // DLYM2R (3 bits)
        bit_field_t<12, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PDMDLY_DLYM3L =              // DLYM3L (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PDMDLY_DLYM3R =              // DLYM3R (3 bits)
        bit_field_t<20, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PDMDLY_DLYM4L =              // DLYM4L (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PDMDLY_DLYM4R =              // DLYM4R (3 bits)
        bit_field_t<28, 0x7>::value<X>();
    static const uint32_t PDMDLY_RESET_VALUE = 0x0;

    static constexpr uint8_t SAI = 76; // SAI
};

static sai_t& SAI = *reinterpret_cast<sai_t*>(0x40015400);

#define HAVE_PERIPHERAL_SAI


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
    volatile uint32_t    SCR;                  // [Read-write] TAMP status clear register
    reserved_t<48>       _3;
    volatile uint32_t    BKP0R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP1R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP2R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP3R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP4R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP5R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP6R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP7R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP8R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP9R;                // [Read-write] TAMP backup register
    volatile uint32_t    BKP10R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP11R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP12R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP13R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP14R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP15R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP16R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP17R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP18R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP19R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP20R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP21R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP22R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP23R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP24R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP25R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP26R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP27R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP28R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP29R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP30R;               // [Read-write] TAMP backup register
    volatile uint32_t    BKP31R;               // [Read-write] TAMP backup register

    static constexpr uint32_t CR1_TAMP1E = 0x1;         // TAMP1E
    static constexpr uint32_t CR1_TAMP2E = 0x2;         // TAMP2E
    static constexpr uint32_t CR1_TAMP3E = 0x4;         // TAMP2E
    static constexpr uint32_t CR1_ITAMP3E = 0x40000;    // ITAMP3E
    static constexpr uint32_t CR1_ITAMP4E = 0x80000;    // ITAMP4E
    static constexpr uint32_t CR1_ITAMP5E = 0x100000;   // ITAMP5E
    static constexpr uint32_t CR1_ITAMP6E = 0x200000;   // ITAMP6E
    static const uint32_t CR1_RESET_VALUE = 0xffff0000;

    static constexpr uint32_t CR2_TAMP1NOER = 0x1;      // TAMP1NOER
    static constexpr uint32_t CR2_TAMP2NOER = 0x2;      // TAMP2NOER
    static constexpr uint32_t CR2_TAMP3NOER = 0x4;      // TAMP3NOER
    static constexpr uint32_t CR2_TAMP1MSK = 0x10000;   // TAMP1MSK
    static constexpr uint32_t CR2_TAMP2MSK = 0x20000;   // TAMP2MSK
    static constexpr uint32_t CR2_TAMP3MSK = 0x40000;   // TAMP3MSK
    static constexpr uint32_t CR2_TAMP1TRG = 0x1000000; // TAMP1TRG
    static constexpr uint32_t CR2_TAMP2TRG = 0x2000000; // TAMP2TRG
    static constexpr uint32_t CR2_TAMP3TRG = 0x4000000; // TAMP3TRG
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
    static constexpr uint32_t IER_TAMP3IE = 0x4;        // TAMP3IE
    static constexpr uint32_t IER_ITAMP3IE = 0x40000;   // ITAMP3IE
    static constexpr uint32_t IER_ITAMP4IE = 0x80000;   // ITAMP4IE
    static constexpr uint32_t IER_ITAMP5IE = 0x100000;  // ITAMP5IE
    static constexpr uint32_t IER_ITAMP6IE = 0x200000;  // ITAMP6IE
    static const uint32_t IER_RESET_VALUE = 0x0;

    static constexpr uint32_t SR_TAMP1F = 0x1;         // TAMP1F
    static constexpr uint32_t SR_TAMP2F = 0x2;         // TAMP2F
    static constexpr uint32_t SR_TAMP3F = 0x4;         // TAMP3F
    static constexpr uint32_t SR_ITAMP3F = 0x40000;    // ITAMP3F
    static constexpr uint32_t SR_ITAMP4F = 0x80000;    // ITAMP4F
    static constexpr uint32_t SR_ITAMP5F = 0x100000;   // ITAMP5F
    static constexpr uint32_t SR_ITAMP6F = 0x200000;   // ITAMP6F
    static const uint32_t SR_RESET_VALUE = 0x0;

    static constexpr uint32_t MISR_TAMP1MF = 0x1;        // TAMP1MF:
    static constexpr uint32_t MISR_TAMP2MF = 0x2;        // TAMP2MF
    static constexpr uint32_t MISR_TAMP3MF = 0x4;        // TAMP3MF
    static constexpr uint32_t MISR_ITAMP3MF = 0x40000;   // ITAMP3MF
    static constexpr uint32_t MISR_ITAMP4MF = 0x80000;   // ITAMP4MF
    static constexpr uint32_t MISR_ITAMP5MF = 0x100000;  // ITAMP5MF
    static constexpr uint32_t MISR_ITAMP6MF = 0x200000;  // ITAMP6MF
    static const uint32_t MISR_RESET_VALUE = 0x0;

    static constexpr uint32_t SCR_CTAMP1F = 0x1;        // CTAMP1F
    static constexpr uint32_t SCR_CTAMP2F = 0x2;        // CTAMP2F
    static constexpr uint32_t SCR_CTAMP3F = 0x4;        // CTAMP3F
    static constexpr uint32_t SCR_CITAMP3F = 0x40000;   // CITAMP3F
    static constexpr uint32_t SCR_CITAMP4F = 0x80000;   // CITAMP4F
    static constexpr uint32_t SCR_CITAMP5F = 0x100000;  // CITAMP5F
    static constexpr uint32_t SCR_CITAMP6F = 0x200000;  // CITAMP6F
    static const uint32_t SCR_RESET_VALUE = 0x0;


    static const uint32_t BKP0R_RESET_VALUE = 0x0;


    static const uint32_t BKP1R_RESET_VALUE = 0x0;


    static const uint32_t BKP2R_RESET_VALUE = 0x0;


    static const uint32_t BKP3R_RESET_VALUE = 0x0;


    static const uint32_t BKP4R_RESET_VALUE = 0x0;


    static const uint32_t BKP5R_RESET_VALUE = 0x0;


    static const uint32_t BKP6R_RESET_VALUE = 0x0;


    static const uint32_t BKP7R_RESET_VALUE = 0x0;


    static const uint32_t BKP8R_RESET_VALUE = 0x0;


    static const uint32_t BKP9R_RESET_VALUE = 0x0;


    static const uint32_t BKP10R_RESET_VALUE = 0x0;


    static const uint32_t BKP11R_RESET_VALUE = 0x0;


    static const uint32_t BKP12R_RESET_VALUE = 0x0;


    static const uint32_t BKP13R_RESET_VALUE = 0x0;


    static const uint32_t BKP14R_RESET_VALUE = 0x0;


    static const uint32_t BKP15R_RESET_VALUE = 0x0;


    static const uint32_t BKP16R_RESET_VALUE = 0x0;


    static const uint32_t BKP17R_RESET_VALUE = 0x0;


    static const uint32_t BKP18R_RESET_VALUE = 0x0;


    static const uint32_t BKP19R_RESET_VALUE = 0x0;


    static const uint32_t BKP20R_RESET_VALUE = 0x0;


    static const uint32_t BKP21R_RESET_VALUE = 0x0;


    static const uint32_t BKP22R_RESET_VALUE = 0x0;


    static const uint32_t BKP23R_RESET_VALUE = 0x0;


    static const uint32_t BKP24R_RESET_VALUE = 0x0;


    static const uint32_t BKP25R_RESET_VALUE = 0x0;


    static const uint32_t BKP26R_RESET_VALUE = 0x0;


    static const uint32_t BKP27R_RESET_VALUE = 0x0;


    static const uint32_t BKP28R_RESET_VALUE = 0x0;


    static const uint32_t BKP29R_RESET_VALUE = 0x0;


    static const uint32_t BKP30R_RESET_VALUE = 0x0;


    static const uint32_t BKP31R_RESET_VALUE = 0x0;
};

static tamp_t& TAMP = *reinterpret_cast<tamp_t*>(0x40002400);

#define HAVE_PERIPHERAL_TAMP


////
//
//    Floting point unit
//
////

struct fpu_t
{
    volatile uint32_t    FPCCR;                // [Read-write] Floating-point context control register
    volatile uint32_t    FPCAR;                // [Read-write] Floating-point context address register
    volatile uint32_t    FPSCR;                // [Read-write] Floating-point status control register

    static constexpr uint32_t FPCCR_LSPACT = 0x1;         // LSPACT
    static constexpr uint32_t FPCCR_USER = 0x2;           // USER
    static constexpr uint32_t FPCCR_THREAD = 0x8;         // THREAD
    static constexpr uint32_t FPCCR_HFRDY = 0x10;         // HFRDY
    static constexpr uint32_t FPCCR_MMRDY = 0x20;         // MMRDY
    static constexpr uint32_t FPCCR_BFRDY = 0x40;         // BFRDY
    static constexpr uint32_t FPCCR_MONRDY = 0x100;       // MONRDY
    static constexpr uint32_t FPCCR_LSPEN = 0x40000000;   // LSPEN
    static constexpr uint32_t FPCCR_ASPEN = 0x80000000;   // ASPEN
    static const uint32_t FPCCR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FPCAR_ADDRESS =             // Location of unpopulated floating-point (29 bits)
        bit_field_t<3, 0x1fffffff>::value<X>();
    static const uint32_t FPCAR_RESET_VALUE = 0x0;

    static constexpr uint32_t FPSCR_IOC = 0x1;            // Invalid operation cumulative exception bit
    static constexpr uint32_t FPSCR_DZC = 0x2;            // Division by zero cumulative exception bit.
    static constexpr uint32_t FPSCR_OFC = 0x4;            // Overflow cumulative exception bit
    static constexpr uint32_t FPSCR_UFC = 0x8;            // Underflow cumulative exception bit
    static constexpr uint32_t FPSCR_IXC = 0x10;           // Inexact cumulative exception bit
    static constexpr uint32_t FPSCR_IDC = 0x80;           // Input denormal cumulative exception bit.
    template<uint32_t X>
    static constexpr uint32_t FPSCR_RMode =               // Rounding Mode control field (2 bits)
        bit_field_t<22, 0x3>::value<X>();
    static constexpr uint32_t FPSCR_FZ = 0x1000000;       // Flush-to-zero mode control bit:
    static constexpr uint32_t FPSCR_DN = 0x2000000;       // Default NaN mode control bit
    static constexpr uint32_t FPSCR_AHP = 0x4000000;      // Alternative half-precision control bit
    static constexpr uint32_t FPSCR_V = 0x10000000;       // Overflow condition code flag
    static constexpr uint32_t FPSCR_C = 0x20000000;       // Carry condition code flag
    static constexpr uint32_t FPSCR_Z = 0x40000000;       // Zero condition code flag
    static constexpr uint32_t FPSCR_N = 0x80000000;       // Negative condition code flag
    static const uint32_t FPSCR_RESET_VALUE = 0x0;

    static constexpr uint8_t FPU = 81; // Floating point unit interrupt
};

static fpu_t& FPU = *reinterpret_cast<fpu_t*>(0xe000ef34);

#define HAVE_PERIPHERAL_FPU


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

static mpu_t& MPU = *reinterpret_cast<mpu_t*>(0xe000e084);

#define HAVE_PERIPHERAL_MPU


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
    volatile uint32_t    SHPR1;                // [Read-write] System handler priority registers
    volatile uint32_t    SHPR2;                // [Read-write] System handler priority registers
    volatile uint32_t    SHPR3;                // [Read-write] System handler priority registers
    volatile uint32_t    SHCRS;                // [Read-write] System handler control and state register
    volatile uint32_t    CFSR_UFSR_BFSR_MMFSR; // [Read-write] Configurable fault status register
    volatile uint32_t    HFSR;                 // [Read-write] Hard fault status register
    reserved_t<1>        _0;
    volatile uint32_t    MMFAR;                // [Read-write] Memory management fault address register
    volatile uint32_t    BFAR;                 // [Read-write] Bus fault address register
    volatile uint32_t    AFSR;                 // [Read-write] Auxiliary fault status register

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

    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_IACCVIOL = 0x2;       // Instruction access violation flag
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_MUNSTKERR = 0x8;      // Memory manager fault on unstacking for a return from exception
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_MSTKERR = 0x10;       // Memory manager fault on stacking for exception entry.
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_MLSPERR = 0x20;       // MLSPERR
    static constexpr uint32_t CFSR_UFSR_BFSR_MMFSR_MMARVALID = 0x80;     // Memory Management Fault Address Register (MMAR) valid flag
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


    static const uint32_t AFSR_RESET_VALUE = 0x0;
};

static scb_t& SCB = *reinterpret_cast<scb_t*>(0xe000e040);

#define HAVE_PERIPHERAL_SCB


////
//
//    Nested Vectored Interrupt Controller
//
////

struct nvic_t
{
    volatile uint32_t    ISER0;                // [Read-write] Interrupt Set-Enable Register
    volatile uint32_t    ISER1;                // [Read-write] Interrupt Set-Enable Register
    volatile uint32_t    ISER2;                // [Read-write] Interrupt Set-Enable Register
    volatile uint32_t    ISER3;                // [Read-write] Interrupt Set-Enable Register
    reserved_t<28>       _0;
    volatile uint32_t    ICER0;                // [Read-write] Interrupt Clear-Enable Register
    volatile uint32_t    ICER1;                // [Read-write] Interrupt Clear-Enable Register
    volatile uint32_t    ICER2;                // [Read-write] Interrupt Clear-Enable Register
    volatile uint32_t    ICER3;                // [Read-write] Interrupt Clear-Enable Register
    reserved_t<28>       _1;
    volatile uint32_t    ISPR0;                // [Read-write] Interrupt Set-Pending Register
    volatile uint32_t    ISPR1;                // [Read-write] Interrupt Set-Pending Register
    volatile uint32_t    ISPR2;                // [Read-write] Interrupt Set-Pending Register
    volatile uint32_t    ISPR3;                // [Read-write] Interrupt Set-Pending Register
    reserved_t<28>       _2;
    volatile uint32_t    ICPR0;                // [Read-write] Interrupt Clear-Pending Register
    volatile uint32_t    ICPR1;                // [Read-write] Interrupt Clear-Pending Register
    volatile uint32_t    ICPR2;                // [Read-write] Interrupt Clear-Pending Register
    volatile uint32_t    ICPR3;                // [Read-write] Interrupt Clear-Pending Register
    reserved_t<28>       _3;
    volatile uint32_t    IABR0;                // [Read-only] Interrupt Active Bit Register
    volatile uint32_t    IABR1;                // [Read-only] Interrupt Active Bit Register
    volatile uint32_t    IABR2;                // [Read-only] Interrupt Active Bit Register
    volatile uint32_t    IABR3;                // [Read-only] Interrupt Active Bit Register
    reserved_t<60>       _4;
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
    volatile uint32_t    IPR15;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR16;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR17;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR18;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR19;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR20;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR21;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR22;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR23;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR24;                // [Read-write] Interrupt Priority Register
    volatile uint32_t    IPR25;                // [Read-write] Interrupt Priority Register
    reserved_t<678>      _5;
    volatile uint32_t    STIR;                 // [Read-write] Software trigger interrupt register


    static const uint32_t ISER0_RESET_VALUE = 0x0;


    static const uint32_t ISER1_RESET_VALUE = 0x0;


    static const uint32_t ISER2_RESET_VALUE = 0x0;


    static const uint32_t ISER3_RESET_VALUE = 0x0;


    static const uint32_t ICER0_RESET_VALUE = 0x0;


    static const uint32_t ICER1_RESET_VALUE = 0x0;


    static const uint32_t ICER2_RESET_VALUE = 0x0;


    static const uint32_t ICER3_RESET_VALUE = 0x0;


    static const uint32_t ISPR0_RESET_VALUE = 0x0;


    static const uint32_t ISPR1_RESET_VALUE = 0x0;


    static const uint32_t ISPR2_RESET_VALUE = 0x0;


    static const uint32_t ISPR3_RESET_VALUE = 0x0;


    static const uint32_t ICPR0_RESET_VALUE = 0x0;


    static const uint32_t ICPR1_RESET_VALUE = 0x0;


    static const uint32_t ICPR2_RESET_VALUE = 0x0;


    static const uint32_t ICPR3_RESET_VALUE = 0x0;


    static const uint32_t IABR0_RESET_VALUE = 0x0;


    static const uint32_t IABR1_RESET_VALUE = 0x0;


    static const uint32_t IABR2_RESET_VALUE = 0x0;


    static const uint32_t IABR3_RESET_VALUE = 0x0;

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

    template<uint32_t X>
    static constexpr uint32_t IPR15_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR15_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR15_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR15_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR15_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR16_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR16_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR16_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR16_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR16_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR17_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR17_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR17_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR17_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR17_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR18_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR18_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR18_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR18_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR18_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR19_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR19_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR19_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR19_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR19_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t IPR20_IPR_N0 =              // IPR_N0 (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR20_IPR_N1 =              // IPR_N1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR20_IPR_N2 =              // IPR_N2 (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t IPR20_IPR_N3 =              // IPR_N3 (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t IPR20_RESET_VALUE = 0x0;

    static const uint32_t IPR21_RESET_VALUE = 0x0;

    static const uint32_t IPR22_RESET_VALUE = 0x0;

    static const uint32_t IPR23_RESET_VALUE = 0x0;

    static const uint32_t IPR24_RESET_VALUE = 0x0;

    static const uint32_t IPR25_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t STIR_INTID =               // Software generated interrupt ID (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t STIR_RESET_VALUE = 0x0;
};

static nvic_t& NVIC = *reinterpret_cast<nvic_t*>(0xe000e100);

#define HAVE_PERIPHERAL_NVIC


////
//
//    Floating point unit CPACR
//
////

struct fpu_cpacr_t
{
    volatile uint32_t    CPACR;                // [Read-write] Coprocessor access control register

    template<uint32_t X>
    static constexpr uint32_t CPACR_CP =                  // CP (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    static const uint32_t CPACR_RESET_VALUE = 0x0;
};

static fpu_cpacr_t& FPU_CPACR = *reinterpret_cast<fpu_cpacr_t*>(0xe000ef08);

#define HAVE_PERIPHERAL_FPU_CPACR


////
//
//    System control block ACTLR
//
////

struct scb_actrl_t
{
    volatile uint32_t    ACTRL;                // [Read-write] Auxiliary control register

    static constexpr uint32_t ACTRL_DISMCYCINT = 0x1;     // DISMCYCINT
    static constexpr uint32_t ACTRL_DISDEFWBUF = 0x2;     // DISDEFWBUF
    static constexpr uint32_t ACTRL_DISFOLD = 0x4;        // DISFOLD
    static constexpr uint32_t ACTRL_DISFPCA = 0x100;      // DISFPCA
    static constexpr uint32_t ACTRL_DISOOFP = 0x200;      // DISOOFP
    static const uint32_t ACTRL_RESET_VALUE = 0x0;
};

static scb_actrl_t& SCB_ACTRL = *reinterpret_cast<scb_actrl_t*>(0xe000e008);

#define HAVE_PERIPHERAL_SCB_ACTRL


////
//
//    FDCAN
//
////

struct fdcan_t
{
    volatile uint32_t    CREL;                 // [Read-only] FDCAN Core Release Register
    volatile uint32_t    ENDN;                 // [Read-only] FDCAN Core Release Register
    reserved_t<1>        _0;
    volatile uint32_t    DBTP;                 // This register is only writable if bits CCCR.CCE and CCCR.INIT are set. The CAN bit time may be programed in the range of 4 to 25 time quanta. The CAN time quantum may be programmed in the range of 1 to 1024 FDCAN clock periods. tq = (DBRP + 1) FDCAN clock period. DTSEG1 is the sum of Prop_Seg and Phase_Seg1. DTSEG2 is Phase_Seg2. Therefore the length of the bit time is (programmed values) [DTSEG1 + DTSEG2 + 3] tq or (functional values) [Sync_Seg + Prop_Seg + Phase_Seg1 + Phase_Seg2] tq. The Information Processing Time (IPT) is zero, meaning the data for the next bit is available at the first clock edge after the sample point.
    volatile uint32_t    TEST;                 // [Read-write] Write access to the Test Register has to be enabled by setting bit CCCR[TEST] to 1 . All Test Register functions are set to their reset values when bit CCCR[TEST] is reset. Loop Back mode and software control of Tx pin FDCANx_TX are hardware test modes. Programming TX differently from 00 may disturb the message transfer on the CAN bus.
    volatile uint32_t    RWD;                  // The RAM Watchdog monitors the READY output of the Message RAM. A Message RAM access starts the Message RAM Watchdog Counter with the value configured by the RWD[WDC] bits. The counter is reloaded with RWD[WDC] bits when the Message RAM signals successful completion by activating its READY output. In case there is no response from the Message RAM until the counter has counted down to 0, the counter stops and interrupt flag IR[WDI] bit is set. The RAM Watchdog Counter is clocked by the fdcan_pclk clock.
    volatile uint32_t    CCCR;                 // [Read-write] For details about setting and resetting of single bits see Software initialization.
    volatile uint32_t    NBTP;                 // [Read-write] FDCAN_NBTP
    volatile uint32_t    TSCC;                 // [Read-write] FDCAN Timestamp Counter Configuration Register
    volatile uint32_t    TSCV;                 // [Read-only] FDCAN Timestamp Counter Value Register
    volatile uint32_t    TOCC;                 // FDCAN Timeout Counter Configuration Register
    volatile uint32_t    TOCV;                 // [Read-only] FDCAN Timeout Counter Value Register
    reserved_t<4>        _1;
    volatile uint32_t    ECR;                  // [Read-only] FDCAN Error Counter Register
    volatile uint32_t    PSR;                  // FDCAN Protocol Status Register
    volatile uint32_t    TDCR;                 // [Read-write] FDCAN Transmitter Delay Compensation Register
    reserved_t<1>        _2;
    volatile uint32_t    IR;                   // [Read-write] The flags are set when one of the listed conditions is detected (edge-sensitive). The flags remain set until the Host clears them. A flag is cleared by writing a 1 to the corresponding bit position. Writing a 0 has no effect. A hard reset will clear the register. The configuration of IE controls whether an interrupt is generated. The configuration of ILS controls on which interrupt line an interrupt is signaled.
    volatile uint32_t    IE;                   // [Read-write] The settings in the Interrupt Enable register determine which status changes in the Interrupt Register will be signaled on an interrupt line.
    volatile uint32_t    ILS;                  // [Read-write] The Interrupt Line Select register assigns an interrupt generated by a specific interrupt flag from the Interrupt Register to one of the two module interrupt lines. For interrupt generation the respective interrupt line has to be enabled via ILE[EINT0] and ILE[EINT1].
    volatile uint32_t    ILE;                  // [Read-write] Each of the two interrupt lines to the CPU can be enabled/disabled separately by programming bits EINT0 and EINT1.
    reserved_t<8>        _3;
    volatile uint32_t    RXGFC;                // Global settings for Message ID filtering. The Global Filter Configuration controls the filter path for standard and extended messages as described in Figure706: Standard Message ID filter path and Figure707: Extended Message ID filter path.
    volatile uint32_t    XIDAM;                // [Read-write] FDCAN Extended ID and Mask Register
    volatile uint32_t    HPMS;                 // [Read-only] This register is updated every time a Message ID filter element configured to generate a priority event match. This can be used to monitor the status of incoming high priority messages and to enable fast access to these messages.
    reserved_t<1>        _4;
    volatile uint32_t    RXF0S;                // [Read-write] FDCAN Rx FIFO 0 Status Register
    volatile uint32_t    RXF0A;                // [Read-write] CAN Rx FIFO 0 Acknowledge Register
    volatile uint32_t    RXF1S;                // [Read-only] FDCAN Rx FIFO 1 Status Register
    volatile uint32_t    RXF1A;                // [Read-write] FDCAN Rx FIFO 1 Acknowledge Register
    reserved_t<8>        _5;
    volatile uint32_t    TXBC;                 // [Read-write] FDCAN Tx Buffer Configuration Register
    volatile uint32_t    TXFQS;                // [Read-only] The Tx FIFO/Queue status is related to the pending Tx requests listed in register TXBRP. Therefore the effect of Add/Cancellation requests may be delayed due to a running Tx scan (TXBRP not yet updated).
    volatile uint32_t    TXBRP;                // [Read-only] FDCAN Tx Buffer Request Pending Register
    volatile uint32_t    TXBAR;                // [Read-write] FDCAN Tx Buffer Add Request Register
    volatile uint32_t    TXBCR;                // [Read-write] FDCAN Tx Buffer Cancellation Request Register
    volatile uint32_t    TXBTO;                // [Read-only] FDCAN Tx Buffer Transmission Occurred Register
    volatile uint32_t    TXBCF;                // [Read-only] FDCAN Tx Buffer Cancellation Finished Register
    volatile uint32_t    TXBTIE;               // [Read-write] FDCAN Tx Buffer Transmission Interrupt Enable Register
    volatile uint32_t    TXBCIE;               // [Read-write] FDCAN Tx Buffer Cancellation Finished Interrupt Enable Register
    volatile uint32_t    TXEFS;                // [Read-only] FDCAN Tx Event FIFO Status Register
    volatile uint32_t    TXEFA;                // [Read-write] FDCAN Tx Event FIFO Acknowledge Register
    reserved_t<5>        _6;
    volatile uint32_t    CKDIV;                // [Read-write] FDCAN CFG clock divider register

    template<uint32_t X>
    static constexpr uint32_t CREL_DAY =                 // DAY (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CREL_MON =                 // MON (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CREL_YEAR =                // YEAR (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CREL_SUBSTEP =             // SUBSTEP (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CREL_STEP =                // STEP (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CREL_REL =                 // REL (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    static const uint32_t CREL_RESET_VALUE = 0x11111111;


    static const uint32_t ENDN_RESET_VALUE = 0x87654321;

    template<uint32_t X>
    static constexpr uint32_t DBTP_DSJW =                // DSJW (4 bits), Read-write
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DBTP_DTSEG2 =              // DTSEG2 (4 bits), Read-write
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DBTP_DTSEG1 =              // DTSEG1 (5 bits), Write-only
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DBTP_DBRP =                // DBRP (5 bits), Read-write
        bit_field_t<16, 0x1f>::value<X>();
    static constexpr uint32_t DBTP_TDC = 0x800000;       // TDC, Read-only
    static const uint32_t DBTP_RESET_VALUE = 0xa33;

    static constexpr uint32_t TEST_LBCK = 0x10;          // LBCK
    template<uint32_t X>
    static constexpr uint32_t TEST_TX =                  // TX (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t TEST_RX = 0x80;            // RX
    static const uint32_t TEST_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RWD_WDC =                 // WDC (8 bits), Read-write
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RWD_WDV =                 // WDV (8 bits), Read-only
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t RWD_RESET_VALUE = 0x0;

    static constexpr uint32_t CCCR_INIT = 0x1;           // INIT
    static constexpr uint32_t CCCR_CCE = 0x2;            // CCE
    static constexpr uint32_t CCCR_ASM = 0x4;            // ASM
    static constexpr uint32_t CCCR_CSA = 0x8;            // CSA
    static constexpr uint32_t CCCR_CSR = 0x10;           // CSR
    static constexpr uint32_t CCCR_MON = 0x20;           // MON
    static constexpr uint32_t CCCR_DAR = 0x40;           // DAR
    static constexpr uint32_t CCCR_TEST = 0x80;          // TEST
    static constexpr uint32_t CCCR_FDOE = 0x100;         // FDOE
    static constexpr uint32_t CCCR_BRSE = 0x200;         // BRSE
    static constexpr uint32_t CCCR_PXHD = 0x1000;        // PXHD
    static constexpr uint32_t CCCR_EFBI = 0x2000;        // EFBI
    static constexpr uint32_t CCCR_TXP = 0x4000;         // TXP
    static constexpr uint32_t CCCR_NISO = 0x8000;        // NISO
    static const uint32_t CCCR_RESET_VALUE = 0x1;

    template<uint32_t X>
    static constexpr uint32_t NBTP_TSEG2 =               // TSEG2 (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NBTP_NTSEG1 =              // NTSEG1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NBTP_NBRP =                // NBRP (9 bits)
        bit_field_t<16, 0x1ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NBTP_NSJW =                // NSJW (7 bits)
        bit_field_t<25, 0x7f>::value<X>();
    static const uint32_t NBTP_RESET_VALUE = 0xa33;

    template<uint32_t X>
    static constexpr uint32_t TSCC_TSS =                 // TSS (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSCC_TCP =                 // TCP (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    static const uint32_t TSCC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TSCV_TSC =                 // TSC (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t TSCV_RESET_VALUE = 0x0;

    static constexpr uint32_t TOCC_ETOC = 0x1;           // ETOC, Read-write
    template<uint32_t X>
    static constexpr uint32_t TOCC_TOS =                 // TOS (2 bits), Write-only
        bit_field_t<1, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TOCC_TOP =                 // TOP (16 bits), Read-write
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t TOCC_RESET_VALUE = 0xffff0000;

    template<uint32_t X>
    static constexpr uint32_t TOCV_TOC =                 // TOC (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t TOCV_RESET_VALUE = 0xffff;

    template<uint32_t X>
    static constexpr uint32_t ECR_TEC =                 // TEC (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_TREC =                // TREC (7 bits)
        bit_field_t<8, 0x7f>::value<X>();
    static constexpr uint32_t ECR_RP = 0x8000;          // RP
    template<uint32_t X>
    static constexpr uint32_t ECR_CEL =                 // CEL (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static const uint32_t ECR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSR_LEC =                 // LEC (3 bits), Read-write
        bit_field_t<0, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PSR_ACT =                 // ACT (2 bits), Write-only
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t PSR_EP = 0x20;            // EP, Read-write
    static constexpr uint32_t PSR_EW = 0x40;            // EW, Read-write
    static constexpr uint32_t PSR_BO = 0x80;            // BO, Read-write
    template<uint32_t X>
    static constexpr uint32_t PSR_DLEC =                // DLEC (3 bits), Write-only
        bit_field_t<8, 0x7>::value<X>();
    static constexpr uint32_t PSR_RESI = 0x800;         // RESI, Read-write
    static constexpr uint32_t PSR_RBRS = 0x1000;        // RBRS, Read-write
    static constexpr uint32_t PSR_REDL = 0x2000;        // REDL, Read-write
    static constexpr uint32_t PSR_PXE = 0x4000;         // PXE, Read-write
    template<uint32_t X>
    static constexpr uint32_t PSR_TDCV =                // TDCV (7 bits), Read-write
        bit_field_t<16, 0x7f>::value<X>();
    static const uint32_t PSR_RESET_VALUE = 0x707;

    template<uint32_t X>
    static constexpr uint32_t TDCR_TDCF =                // TDCF (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDCR_TDCO =                // TDCO (7 bits)
        bit_field_t<8, 0x7f>::value<X>();
    static const uint32_t TDCR_RESET_VALUE = 0x0;

    static constexpr uint32_t IR_RF0N = 0x1;           // RF0N
    static constexpr uint32_t IR_RF0W = 0x2;           // RF0W
    static constexpr uint32_t IR_RF0F = 0x4;           // RF0F
    static constexpr uint32_t IR_RF0L = 0x8;           // RF0L
    static constexpr uint32_t IR_RF1N = 0x10;          // RF1N
    static constexpr uint32_t IR_RF1W = 0x20;          // RF1W
    static constexpr uint32_t IR_RF1F = 0x40;          // RF1F
    static constexpr uint32_t IR_RF1L = 0x80;          // RF1L
    static constexpr uint32_t IR_HPM = 0x100;          // HPM
    static constexpr uint32_t IR_TC = 0x200;           // TC
    static constexpr uint32_t IR_TCF = 0x400;          // TCF
    static constexpr uint32_t IR_TFE = 0x800;          // TFE
    static constexpr uint32_t IR_TEFN = 0x1000;        // TEFN
    static constexpr uint32_t IR_TEFW = 0x2000;        // TEFW
    static constexpr uint32_t IR_TEFF = 0x4000;        // TEFF
    static constexpr uint32_t IR_TEFL = 0x8000;        // TEFL
    static constexpr uint32_t IR_TSW = 0x10000;        // TSW
    static constexpr uint32_t IR_MRAF = 0x20000;       // MRAF
    static constexpr uint32_t IR_TOO = 0x40000;        // TOO
    static constexpr uint32_t IR_DRX = 0x80000;        // DRX
    static constexpr uint32_t IR_ELO = 0x400000;       // ELO
    static constexpr uint32_t IR_EP = 0x800000;        // EP
    static constexpr uint32_t IR_EW = 0x1000000;       // EW
    static constexpr uint32_t IR_BO = 0x2000000;       // BO
    static constexpr uint32_t IR_WDI = 0x4000000;      // WDI
    static constexpr uint32_t IR_PEA = 0x8000000;      // PEA
    static constexpr uint32_t IR_PED = 0x10000000;     // PED
    static constexpr uint32_t IR_ARA = 0x20000000;     // ARA
    static const uint32_t IR_RESET_VALUE = 0x0;

    static constexpr uint32_t IE_RF0NE = 0x1;          // RF0NE
    static constexpr uint32_t IE_RF0WE = 0x2;          // RF0WE
    static constexpr uint32_t IE_RF0FE = 0x4;          // RF0FE
    static constexpr uint32_t IE_RF0LE = 0x8;          // RF0LE
    static constexpr uint32_t IE_RF1NE = 0x10;         // RF1NE
    static constexpr uint32_t IE_RF1WE = 0x20;         // RF1WE
    static constexpr uint32_t IE_RF1FE = 0x40;         // RF1FE
    static constexpr uint32_t IE_RF1LE = 0x80;         // RF1LE
    static constexpr uint32_t IE_HPME = 0x100;         // HPME
    static constexpr uint32_t IE_TCE = 0x200;          // TCE
    static constexpr uint32_t IE_TCFE = 0x400;         // TCFE
    static constexpr uint32_t IE_TFEE = 0x800;         // TFEE
    static constexpr uint32_t IE_TEFNE = 0x1000;       // TEFNE
    static constexpr uint32_t IE_TEFWE = 0x2000;       // TEFWE
    static constexpr uint32_t IE_TEFFE = 0x4000;       // TEFFE
    static constexpr uint32_t IE_TEFLE = 0x8000;       // TEFLE
    static constexpr uint32_t IE_TSWE = 0x10000;       // TSWE
    static constexpr uint32_t IE_MRAFE = 0x20000;      // MRAFE
    static constexpr uint32_t IE_TOOE = 0x40000;       // TOOE
    static constexpr uint32_t IE_DRX = 0x80000;        // DRX
    static constexpr uint32_t IE_BECE = 0x100000;      // BECE
    static constexpr uint32_t IE_BEUE = 0x200000;      // BEUE
    static constexpr uint32_t IE_ELOE = 0x400000;      // ELOE
    static constexpr uint32_t IE_EPE = 0x800000;       // EPE
    static constexpr uint32_t IE_EWE = 0x1000000;      // EWE
    static constexpr uint32_t IE_BOE = 0x2000000;      // BOE
    static constexpr uint32_t IE_WDIE = 0x4000000;     // WDIE
    static constexpr uint32_t IE_PEAE = 0x8000000;     // PEAE
    static constexpr uint32_t IE_PEDE = 0x10000000;    // PEDE
    static constexpr uint32_t IE_ARAE = 0x20000000;    // ARAE
    static const uint32_t IE_RESET_VALUE = 0x0;

    static constexpr uint32_t ILS_RF0NL = 0x1;          // RF0NL
    static constexpr uint32_t ILS_RF0WL = 0x2;          // RF0WL
    static constexpr uint32_t ILS_RF0FL = 0x4;          // RF0FL
    static constexpr uint32_t ILS_RF0LL = 0x8;          // RF0LL
    static constexpr uint32_t ILS_RF1NL = 0x10;         // RF1NL
    static constexpr uint32_t ILS_RF1WL = 0x20;         // RF1WL
    static constexpr uint32_t ILS_RF1FL = 0x40;         // RF1FL
    static constexpr uint32_t ILS_RF1LL = 0x80;         // RF1LL
    static constexpr uint32_t ILS_HPML = 0x100;         // HPML
    static constexpr uint32_t ILS_TCL = 0x200;          // TCL
    static constexpr uint32_t ILS_TCFL = 0x400;         // TCFL
    static constexpr uint32_t ILS_TFEL = 0x800;         // TFEL
    static constexpr uint32_t ILS_TEFNL = 0x1000;       // TEFNL
    static constexpr uint32_t ILS_TEFWL = 0x2000;       // TEFWL
    static constexpr uint32_t ILS_TEFFL = 0x4000;       // TEFFL
    static constexpr uint32_t ILS_TEFLL = 0x8000;       // TEFLL
    static constexpr uint32_t ILS_TSWL = 0x10000;       // TSWL
    static constexpr uint32_t ILS_MRAFL = 0x20000;      // MRAFL
    static constexpr uint32_t ILS_TOOL = 0x40000;       // TOOL
    static constexpr uint32_t ILS_DRXL = 0x80000;       // DRXL
    static constexpr uint32_t ILS_BECL = 0x100000;      // BECL
    static constexpr uint32_t ILS_BEUL = 0x200000;      // BEUL
    static constexpr uint32_t ILS_ELOL = 0x400000;      // ELOL
    static constexpr uint32_t ILS_EPL = 0x800000;       // EPL
    static constexpr uint32_t ILS_EWL = 0x1000000;      // EWL
    static constexpr uint32_t ILS_BOL = 0x2000000;      // BOL
    static constexpr uint32_t ILS_WDIL = 0x4000000;     // WDIL
    static constexpr uint32_t ILS_PEAL = 0x8000000;     // PEAL
    static constexpr uint32_t ILS_PEDL = 0x10000000;    // PEDL
    static constexpr uint32_t ILS_ARAL = 0x20000000;    // ARAL
    static const uint32_t ILS_RESET_VALUE = 0x0;

    static constexpr uint32_t ILE_EINT0 = 0x1;          // EINT0
    static constexpr uint32_t ILE_EINT1 = 0x2;          // EINT1
    static const uint32_t ILE_RESET_VALUE = 0x0;

    static constexpr uint32_t RXGFC_RRFE = 0x1;           // RRFE, Read-write
    static constexpr uint32_t RXGFC_RRFS = 0x2;           // RRFS, Read-write
    template<uint32_t X>
    static constexpr uint32_t RXGFC_ANFE =                // ANFE (2 bits), Write-only
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RXGFC_ANFS =                // ANFS (2 bits), Write-only
        bit_field_t<4, 0x3>::value<X>();
    static const uint32_t RXGFC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t XIDAM_EIDM =                // EIDM (29 bits)
        bit_field_t<0, 0x1fffffff>::value<X>();
    static const uint32_t XIDAM_RESET_VALUE = 0x1fffffff;

    template<uint32_t X>
    static constexpr uint32_t HPMS_BIDX =                // BIDX (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HPMS_MSI =                 // MSI (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HPMS_FIDX =                // FIDX (7 bits)
        bit_field_t<8, 0x7f>::value<X>();
    static constexpr uint32_t HPMS_FLST = 0x8000;        // FLST
    static const uint32_t HPMS_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXF0S_F0FL =                // F0FL (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RXF0S_F0GI =                // F0GI (6 bits)
        bit_field_t<8, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RXF0S_F0PI =                // F0PI (6 bits)
        bit_field_t<16, 0x3f>::value<X>();
    static constexpr uint32_t RXF0S_F0F = 0x1000000;      // F0F
    static constexpr uint32_t RXF0S_RF0L = 0x2000000;     // RF0L
    static const uint32_t RXF0S_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXF0A_F0AI =                // F0AI (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    static const uint32_t RXF0A_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXF1S_F1FL =                // F1FL (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RXF1S_F1GI =                // F1GI (6 bits)
        bit_field_t<8, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RXF1S_F1PI =                // F1PI (6 bits)
        bit_field_t<16, 0x3f>::value<X>();
    static constexpr uint32_t RXF1S_F1F = 0x1000000;      // F1F
    static constexpr uint32_t RXF1S_RF1L = 0x2000000;     // RF1L
    template<uint32_t X>
    static constexpr uint32_t RXF1S_DMS =                 // DMS (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t RXF1S_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXF1A_F1AI =                // F1AI (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    static const uint32_t RXF1A_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXBC_TBSA =                // TBSA (14 bits)
        bit_field_t<2, 0x3fff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TXBC_NDTB =                // NDTB (6 bits)
        bit_field_t<16, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TXBC_TFQS =                // TFQS (6 bits)
        bit_field_t<24, 0x3f>::value<X>();
    static constexpr uint32_t TXBC_TFQM = 0x40000000;    // TFQM
    static const uint32_t TXBC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXFQS_TFFL =                // TFFL (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TXFQS_TFGI =                // TFGI (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TXFQS_TFQPI =               // TFQPI (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static constexpr uint32_t TXFQS_TFQF = 0x200000;      // TFQF
    static const uint32_t TXFQS_RESET_VALUE = 0x0;


    static const uint32_t TXBRP_RESET_VALUE = 0x0;


    static const uint32_t TXBAR_RESET_VALUE = 0x0;


    static const uint32_t TXBCR_RESET_VALUE = 0x0;


    static const uint32_t TXBTO_RESET_VALUE = 0x0;


    static const uint32_t TXBCF_RESET_VALUE = 0x0;


    static const uint32_t TXBTIE_RESET_VALUE = 0x0;


    static const uint32_t TXBCIE_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXEFS_EFFL =                // EFFL (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TXEFS_EFGI =                // EFGI (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TXEFS_EFPI =                // EFPI (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static constexpr uint32_t TXEFS_EFF = 0x1000000;      // EFF
    static constexpr uint32_t TXEFS_TEFL = 0x2000000;     // TEFL
    static const uint32_t TXEFS_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXEFA_EFAI =                // EFAI (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t TXEFA_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CKDIV_PDIV =                // input clock divider. the APB clock could be divided prior to be used by the CAN sub (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CKDIV_RESET_VALUE = 0x0;
};

static fdcan_t& FDCAN = *reinterpret_cast<fdcan_t*>(0x4000a400);

#define HAVE_PERIPHERAL_FDCAN


////
//
//    FDCAN
//
////

struct fdcan1_t
{
    volatile uint32_t    CREL;                 // [Read-only] FDCAN Core Release Register
    volatile uint32_t    ENDN;                 // [Read-only] FDCAN Core Release Register
    reserved_t<1>        _0;
    volatile uint32_t    DBTP;                 // This register is only writable if bits CCCR.CCE and CCCR.INIT are set. The CAN bit time may be programed in the range of 4 to 25 time quanta. The CAN time quantum may be programmed in the range of 1 to 1024 FDCAN clock periods. tq = (DBRP + 1) FDCAN clock period. DTSEG1 is the sum of Prop_Seg and Phase_Seg1. DTSEG2 is Phase_Seg2. Therefore the length of the bit time is (programmed values) [DTSEG1 + DTSEG2 + 3] tq or (functional values) [Sync_Seg + Prop_Seg + Phase_Seg1 + Phase_Seg2] tq. The Information Processing Time (IPT) is zero, meaning the data for the next bit is available at the first clock edge after the sample point.
    volatile uint32_t    TEST;                 // [Read-write] Write access to the Test Register has to be enabled by setting bit CCCR[TEST] to 1 . All Test Register functions are set to their reset values when bit CCCR[TEST] is reset. Loop Back mode and software control of Tx pin FDCANx_TX are hardware test modes. Programming TX differently from 00 may disturb the message transfer on the CAN bus.
    volatile uint32_t    RWD;                  // The RAM Watchdog monitors the READY output of the Message RAM. A Message RAM access starts the Message RAM Watchdog Counter with the value configured by the RWD[WDC] bits. The counter is reloaded with RWD[WDC] bits when the Message RAM signals successful completion by activating its READY output. In case there is no response from the Message RAM until the counter has counted down to 0, the counter stops and interrupt flag IR[WDI] bit is set. The RAM Watchdog Counter is clocked by the fdcan_pclk clock.
    volatile uint32_t    CCCR;                 // [Read-write] For details about setting and resetting of single bits see Software initialization.
    volatile uint32_t    NBTP;                 // [Read-write] FDCAN_NBTP
    volatile uint32_t    TSCC;                 // [Read-write] FDCAN Timestamp Counter Configuration Register
    volatile uint32_t    TSCV;                 // [Read-only] FDCAN Timestamp Counter Value Register
    volatile uint32_t    TOCC;                 // FDCAN Timeout Counter Configuration Register
    volatile uint32_t    TOCV;                 // [Read-only] FDCAN Timeout Counter Value Register
    reserved_t<4>        _1;
    volatile uint32_t    ECR;                  // [Read-only] FDCAN Error Counter Register
    volatile uint32_t    PSR;                  // FDCAN Protocol Status Register
    volatile uint32_t    TDCR;                 // [Read-write] FDCAN Transmitter Delay Compensation Register
    reserved_t<1>        _2;
    volatile uint32_t    IR;                   // [Read-write] The flags are set when one of the listed conditions is detected (edge-sensitive). The flags remain set until the Host clears them. A flag is cleared by writing a 1 to the corresponding bit position. Writing a 0 has no effect. A hard reset will clear the register. The configuration of IE controls whether an interrupt is generated. The configuration of ILS controls on which interrupt line an interrupt is signaled.
    volatile uint32_t    IE;                   // [Read-write] The settings in the Interrupt Enable register determine which status changes in the Interrupt Register will be signaled on an interrupt line.
    volatile uint32_t    ILS;                  // [Read-write] The Interrupt Line Select register assigns an interrupt generated by a specific interrupt flag from the Interrupt Register to one of the two module interrupt lines. For interrupt generation the respective interrupt line has to be enabled via ILE[EINT0] and ILE[EINT1].
    volatile uint32_t    ILE;                  // [Read-write] Each of the two interrupt lines to the CPU can be enabled/disabled separately by programming bits EINT0 and EINT1.
    reserved_t<8>        _3;
    volatile uint32_t    RXGFC;                // Global settings for Message ID filtering. The Global Filter Configuration controls the filter path for standard and extended messages as described in Figure706: Standard Message ID filter path and Figure707: Extended Message ID filter path.
    volatile uint32_t    XIDAM;                // [Read-write] FDCAN Extended ID and Mask Register
    volatile uint32_t    HPMS;                 // [Read-only] This register is updated every time a Message ID filter element configured to generate a priority event match. This can be used to monitor the status of incoming high priority messages and to enable fast access to these messages.
    reserved_t<1>        _4;
    volatile uint32_t    RXF0S;                // [Read-write] FDCAN Rx FIFO 0 Status Register
    volatile uint32_t    RXF0A;                // [Read-write] CAN Rx FIFO 0 Acknowledge Register
    volatile uint32_t    RXF1S;                // [Read-only] FDCAN Rx FIFO 1 Status Register
    volatile uint32_t    RXF1A;                // [Read-write] FDCAN Rx FIFO 1 Acknowledge Register
    reserved_t<8>        _5;
    volatile uint32_t    TXBC;                 // [Read-write] FDCAN Tx Buffer Configuration Register
    volatile uint32_t    TXFQS;                // [Read-only] The Tx FIFO/Queue status is related to the pending Tx requests listed in register TXBRP. Therefore the effect of Add/Cancellation requests may be delayed due to a running Tx scan (TXBRP not yet updated).
    volatile uint32_t    TXBRP;                // [Read-only] FDCAN Tx Buffer Request Pending Register
    volatile uint32_t    TXBAR;                // [Read-write] FDCAN Tx Buffer Add Request Register
    volatile uint32_t    TXBCR;                // [Read-write] FDCAN Tx Buffer Cancellation Request Register
    volatile uint32_t    TXBTO;                // [Read-only] FDCAN Tx Buffer Transmission Occurred Register
    volatile uint32_t    TXBCF;                // [Read-only] FDCAN Tx Buffer Cancellation Finished Register
    volatile uint32_t    TXBTIE;               // [Read-write] FDCAN Tx Buffer Transmission Interrupt Enable Register
    volatile uint32_t    TXBCIE;               // [Read-write] FDCAN Tx Buffer Cancellation Finished Interrupt Enable Register
    volatile uint32_t    TXEFS;                // [Read-only] FDCAN Tx Event FIFO Status Register
    volatile uint32_t    TXEFA;                // [Read-write] FDCAN Tx Event FIFO Acknowledge Register
    reserved_t<5>        _6;
    volatile uint32_t    CKDIV;                // [Read-write] FDCAN CFG clock divider register

    template<uint32_t X>
    static constexpr uint32_t CREL_DAY =                 // DAY (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CREL_MON =                 // MON (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CREL_YEAR =                // YEAR (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CREL_SUBSTEP =             // SUBSTEP (4 bits)
        bit_field_t<20, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CREL_STEP =                // STEP (4 bits)
        bit_field_t<24, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CREL_REL =                 // REL (4 bits)
        bit_field_t<28, 0xf>::value<X>();
    static const uint32_t CREL_RESET_VALUE = 0x11111111;


    static const uint32_t ENDN_RESET_VALUE = 0x87654321;

    template<uint32_t X>
    static constexpr uint32_t DBTP_DSJW =                // DSJW (4 bits), Read-write
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DBTP_DTSEG2 =              // DTSEG2 (4 bits), Read-write
        bit_field_t<4, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DBTP_DTSEG1 =              // DTSEG1 (5 bits), Write-only
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t DBTP_DBRP =                // DBRP (5 bits), Read-write
        bit_field_t<16, 0x1f>::value<X>();
    static constexpr uint32_t DBTP_TDC = 0x800000;       // TDC, Read-only
    static const uint32_t DBTP_RESET_VALUE = 0xa33;

    static constexpr uint32_t TEST_LBCK = 0x10;          // LBCK
    template<uint32_t X>
    static constexpr uint32_t TEST_TX =                  // TX (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t TEST_RX = 0x80;            // RX
    static const uint32_t TEST_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RWD_WDC =                 // WDC (8 bits), Read-write
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RWD_WDV =                 // WDV (8 bits), Read-only
        bit_field_t<8, 0xff>::value<X>();
    static const uint32_t RWD_RESET_VALUE = 0x0;

    static constexpr uint32_t CCCR_INIT = 0x1;           // INIT
    static constexpr uint32_t CCCR_CCE = 0x2;            // CCE
    static constexpr uint32_t CCCR_ASM = 0x4;            // ASM
    static constexpr uint32_t CCCR_CSA = 0x8;            // CSA
    static constexpr uint32_t CCCR_CSR = 0x10;           // CSR
    static constexpr uint32_t CCCR_MON = 0x20;           // MON
    static constexpr uint32_t CCCR_DAR = 0x40;           // DAR
    static constexpr uint32_t CCCR_TEST = 0x80;          // TEST
    static constexpr uint32_t CCCR_FDOE = 0x100;         // FDOE
    static constexpr uint32_t CCCR_BRSE = 0x200;         // BRSE
    static constexpr uint32_t CCCR_PXHD = 0x1000;        // PXHD
    static constexpr uint32_t CCCR_EFBI = 0x2000;        // EFBI
    static constexpr uint32_t CCCR_TXP = 0x4000;         // TXP
    static constexpr uint32_t CCCR_NISO = 0x8000;        // NISO
    static const uint32_t CCCR_RESET_VALUE = 0x1;

    template<uint32_t X>
    static constexpr uint32_t NBTP_TSEG2 =               // TSEG2 (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NBTP_NTSEG1 =              // NTSEG1 (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NBTP_NBRP =                // NBRP (9 bits)
        bit_field_t<16, 0x1ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NBTP_NSJW =                // NSJW (7 bits)
        bit_field_t<25, 0x7f>::value<X>();
    static const uint32_t NBTP_RESET_VALUE = 0xa33;

    template<uint32_t X>
    static constexpr uint32_t TSCC_TSS =                 // TSS (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TSCC_TCP =                 // TCP (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    static const uint32_t TSCC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TSCV_TSC =                 // TSC (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t TSCV_RESET_VALUE = 0x0;

    static constexpr uint32_t TOCC_ETOC = 0x1;           // ETOC, Read-write
    template<uint32_t X>
    static constexpr uint32_t TOCC_TOS =                 // TOS (2 bits), Write-only
        bit_field_t<1, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TOCC_TOP =                 // TOP (16 bits), Read-write
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t TOCC_RESET_VALUE = 0xffff0000;

    template<uint32_t X>
    static constexpr uint32_t TOCV_TOC =                 // TOC (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t TOCV_RESET_VALUE = 0xffff;

    template<uint32_t X>
    static constexpr uint32_t ECR_TEC =                 // TEC (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t ECR_TREC =                // TREC (7 bits)
        bit_field_t<8, 0x7f>::value<X>();
    static constexpr uint32_t ECR_RP = 0x8000;          // RP
    template<uint32_t X>
    static constexpr uint32_t ECR_CEL =                 // CEL (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    static const uint32_t ECR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t PSR_LEC =                 // LEC (3 bits), Read-write
        bit_field_t<0, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PSR_ACT =                 // ACT (2 bits), Write-only
        bit_field_t<3, 0x3>::value<X>();
    static constexpr uint32_t PSR_EP = 0x20;            // EP, Read-write
    static constexpr uint32_t PSR_EW = 0x40;            // EW, Read-write
    static constexpr uint32_t PSR_BO = 0x80;            // BO, Read-write
    template<uint32_t X>
    static constexpr uint32_t PSR_DLEC =                // DLEC (3 bits), Write-only
        bit_field_t<8, 0x7>::value<X>();
    static constexpr uint32_t PSR_RESI = 0x800;         // RESI, Read-write
    static constexpr uint32_t PSR_RBRS = 0x1000;        // RBRS, Read-write
    static constexpr uint32_t PSR_REDL = 0x2000;        // REDL, Read-write
    static constexpr uint32_t PSR_PXE = 0x4000;         // PXE, Read-write
    template<uint32_t X>
    static constexpr uint32_t PSR_TDCV =                // TDCV (7 bits), Read-write
        bit_field_t<16, 0x7f>::value<X>();
    static const uint32_t PSR_RESET_VALUE = 0x707;

    template<uint32_t X>
    static constexpr uint32_t TDCR_TDCF =                // TDCF (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TDCR_TDCO =                // TDCO (7 bits)
        bit_field_t<8, 0x7f>::value<X>();
    static const uint32_t TDCR_RESET_VALUE = 0x0;

    static constexpr uint32_t IR_RF0N = 0x1;           // RF0N
    static constexpr uint32_t IR_RF0W = 0x2;           // RF0W
    static constexpr uint32_t IR_RF0F = 0x4;           // RF0F
    static constexpr uint32_t IR_RF0L = 0x8;           // RF0L
    static constexpr uint32_t IR_RF1N = 0x10;          // RF1N
    static constexpr uint32_t IR_RF1W = 0x20;          // RF1W
    static constexpr uint32_t IR_RF1F = 0x40;          // RF1F
    static constexpr uint32_t IR_RF1L = 0x80;          // RF1L
    static constexpr uint32_t IR_HPM = 0x100;          // HPM
    static constexpr uint32_t IR_TC = 0x200;           // TC
    static constexpr uint32_t IR_TCF = 0x400;          // TCF
    static constexpr uint32_t IR_TFE = 0x800;          // TFE
    static constexpr uint32_t IR_TEFN = 0x1000;        // TEFN
    static constexpr uint32_t IR_TEFW = 0x2000;        // TEFW
    static constexpr uint32_t IR_TEFF = 0x4000;        // TEFF
    static constexpr uint32_t IR_TEFL = 0x8000;        // TEFL
    static constexpr uint32_t IR_TSW = 0x10000;        // TSW
    static constexpr uint32_t IR_MRAF = 0x20000;       // MRAF
    static constexpr uint32_t IR_TOO = 0x40000;        // TOO
    static constexpr uint32_t IR_DRX = 0x80000;        // DRX
    static constexpr uint32_t IR_ELO = 0x400000;       // ELO
    static constexpr uint32_t IR_EP = 0x800000;        // EP
    static constexpr uint32_t IR_EW = 0x1000000;       // EW
    static constexpr uint32_t IR_BO = 0x2000000;       // BO
    static constexpr uint32_t IR_WDI = 0x4000000;      // WDI
    static constexpr uint32_t IR_PEA = 0x8000000;      // PEA
    static constexpr uint32_t IR_PED = 0x10000000;     // PED
    static constexpr uint32_t IR_ARA = 0x20000000;     // ARA
    static const uint32_t IR_RESET_VALUE = 0x0;

    static constexpr uint32_t IE_RF0NE = 0x1;          // RF0NE
    static constexpr uint32_t IE_RF0WE = 0x2;          // RF0WE
    static constexpr uint32_t IE_RF0FE = 0x4;          // RF0FE
    static constexpr uint32_t IE_RF0LE = 0x8;          // RF0LE
    static constexpr uint32_t IE_RF1NE = 0x10;         // RF1NE
    static constexpr uint32_t IE_RF1WE = 0x20;         // RF1WE
    static constexpr uint32_t IE_RF1FE = 0x40;         // RF1FE
    static constexpr uint32_t IE_RF1LE = 0x80;         // RF1LE
    static constexpr uint32_t IE_HPME = 0x100;         // HPME
    static constexpr uint32_t IE_TCE = 0x200;          // TCE
    static constexpr uint32_t IE_TCFE = 0x400;         // TCFE
    static constexpr uint32_t IE_TFEE = 0x800;         // TFEE
    static constexpr uint32_t IE_TEFNE = 0x1000;       // TEFNE
    static constexpr uint32_t IE_TEFWE = 0x2000;       // TEFWE
    static constexpr uint32_t IE_TEFFE = 0x4000;       // TEFFE
    static constexpr uint32_t IE_TEFLE = 0x8000;       // TEFLE
    static constexpr uint32_t IE_TSWE = 0x10000;       // TSWE
    static constexpr uint32_t IE_MRAFE = 0x20000;      // MRAFE
    static constexpr uint32_t IE_TOOE = 0x40000;       // TOOE
    static constexpr uint32_t IE_DRX = 0x80000;        // DRX
    static constexpr uint32_t IE_BECE = 0x100000;      // BECE
    static constexpr uint32_t IE_BEUE = 0x200000;      // BEUE
    static constexpr uint32_t IE_ELOE = 0x400000;      // ELOE
    static constexpr uint32_t IE_EPE = 0x800000;       // EPE
    static constexpr uint32_t IE_EWE = 0x1000000;      // EWE
    static constexpr uint32_t IE_BOE = 0x2000000;      // BOE
    static constexpr uint32_t IE_WDIE = 0x4000000;     // WDIE
    static constexpr uint32_t IE_PEAE = 0x8000000;     // PEAE
    static constexpr uint32_t IE_PEDE = 0x10000000;    // PEDE
    static constexpr uint32_t IE_ARAE = 0x20000000;    // ARAE
    static const uint32_t IE_RESET_VALUE = 0x0;

    static constexpr uint32_t ILS_RF0NL = 0x1;          // RF0NL
    static constexpr uint32_t ILS_RF0WL = 0x2;          // RF0WL
    static constexpr uint32_t ILS_RF0FL = 0x4;          // RF0FL
    static constexpr uint32_t ILS_RF0LL = 0x8;          // RF0LL
    static constexpr uint32_t ILS_RF1NL = 0x10;         // RF1NL
    static constexpr uint32_t ILS_RF1WL = 0x20;         // RF1WL
    static constexpr uint32_t ILS_RF1FL = 0x40;         // RF1FL
    static constexpr uint32_t ILS_RF1LL = 0x80;         // RF1LL
    static constexpr uint32_t ILS_HPML = 0x100;         // HPML
    static constexpr uint32_t ILS_TCL = 0x200;          // TCL
    static constexpr uint32_t ILS_TCFL = 0x400;         // TCFL
    static constexpr uint32_t ILS_TFEL = 0x800;         // TFEL
    static constexpr uint32_t ILS_TEFNL = 0x1000;       // TEFNL
    static constexpr uint32_t ILS_TEFWL = 0x2000;       // TEFWL
    static constexpr uint32_t ILS_TEFFL = 0x4000;       // TEFFL
    static constexpr uint32_t ILS_TEFLL = 0x8000;       // TEFLL
    static constexpr uint32_t ILS_TSWL = 0x10000;       // TSWL
    static constexpr uint32_t ILS_MRAFL = 0x20000;      // MRAFL
    static constexpr uint32_t ILS_TOOL = 0x40000;       // TOOL
    static constexpr uint32_t ILS_DRXL = 0x80000;       // DRXL
    static constexpr uint32_t ILS_BECL = 0x100000;      // BECL
    static constexpr uint32_t ILS_BEUL = 0x200000;      // BEUL
    static constexpr uint32_t ILS_ELOL = 0x400000;      // ELOL
    static constexpr uint32_t ILS_EPL = 0x800000;       // EPL
    static constexpr uint32_t ILS_EWL = 0x1000000;      // EWL
    static constexpr uint32_t ILS_BOL = 0x2000000;      // BOL
    static constexpr uint32_t ILS_WDIL = 0x4000000;     // WDIL
    static constexpr uint32_t ILS_PEAL = 0x8000000;     // PEAL
    static constexpr uint32_t ILS_PEDL = 0x10000000;    // PEDL
    static constexpr uint32_t ILS_ARAL = 0x20000000;    // ARAL
    static const uint32_t ILS_RESET_VALUE = 0x0;

    static constexpr uint32_t ILE_EINT0 = 0x1;          // EINT0
    static constexpr uint32_t ILE_EINT1 = 0x2;          // EINT1
    static const uint32_t ILE_RESET_VALUE = 0x0;

    static constexpr uint32_t RXGFC_RRFE = 0x1;           // RRFE, Read-write
    static constexpr uint32_t RXGFC_RRFS = 0x2;           // RRFS, Read-write
    template<uint32_t X>
    static constexpr uint32_t RXGFC_ANFE =                // ANFE (2 bits), Write-only
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RXGFC_ANFS =                // ANFS (2 bits), Write-only
        bit_field_t<4, 0x3>::value<X>();
    static const uint32_t RXGFC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t XIDAM_EIDM =                // EIDM (29 bits)
        bit_field_t<0, 0x1fffffff>::value<X>();
    static const uint32_t XIDAM_RESET_VALUE = 0x1fffffff;

    template<uint32_t X>
    static constexpr uint32_t HPMS_BIDX =                // BIDX (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HPMS_MSI =                 // MSI (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t HPMS_FIDX =                // FIDX (7 bits)
        bit_field_t<8, 0x7f>::value<X>();
    static constexpr uint32_t HPMS_FLST = 0x8000;        // FLST
    static const uint32_t HPMS_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXF0S_F0FL =                // F0FL (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RXF0S_F0GI =                // F0GI (6 bits)
        bit_field_t<8, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RXF0S_F0PI =                // F0PI (6 bits)
        bit_field_t<16, 0x3f>::value<X>();
    static constexpr uint32_t RXF0S_F0F = 0x1000000;      // F0F
    static constexpr uint32_t RXF0S_RF0L = 0x2000000;     // RF0L
    static const uint32_t RXF0S_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXF0A_F0AI =                // F0AI (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    static const uint32_t RXF0A_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXF1S_F1FL =                // F1FL (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RXF1S_F1GI =                // F1GI (6 bits)
        bit_field_t<8, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t RXF1S_F1PI =                // F1PI (6 bits)
        bit_field_t<16, 0x3f>::value<X>();
    static constexpr uint32_t RXF1S_F1F = 0x1000000;      // F1F
    static constexpr uint32_t RXF1S_RF1L = 0x2000000;     // RF1L
    template<uint32_t X>
    static constexpr uint32_t RXF1S_DMS =                 // DMS (2 bits)
        bit_field_t<30, 0x3>::value<X>();
    static const uint32_t RXF1S_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t RXF1A_F1AI =                // F1AI (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    static const uint32_t RXF1A_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXBC_TBSA =                // TBSA (14 bits)
        bit_field_t<2, 0x3fff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TXBC_NDTB =                // NDTB (6 bits)
        bit_field_t<16, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TXBC_TFQS =                // TFQS (6 bits)
        bit_field_t<24, 0x3f>::value<X>();
    static constexpr uint32_t TXBC_TFQM = 0x40000000;    // TFQM
    static const uint32_t TXBC_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXFQS_TFFL =                // TFFL (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TXFQS_TFGI =                // TFGI (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TXFQS_TFQPI =               // TFQPI (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static constexpr uint32_t TXFQS_TFQF = 0x200000;      // TFQF
    static const uint32_t TXFQS_RESET_VALUE = 0x0;


    static const uint32_t TXBRP_RESET_VALUE = 0x0;


    static const uint32_t TXBAR_RESET_VALUE = 0x0;


    static const uint32_t TXBCR_RESET_VALUE = 0x0;


    static const uint32_t TXBTO_RESET_VALUE = 0x0;


    static const uint32_t TXBCF_RESET_VALUE = 0x0;


    static const uint32_t TXBTIE_RESET_VALUE = 0x0;


    static const uint32_t TXBCIE_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXEFS_EFFL =                // EFFL (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TXEFS_EFGI =                // EFGI (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TXEFS_EFPI =                // EFPI (5 bits)
        bit_field_t<16, 0x1f>::value<X>();
    static constexpr uint32_t TXEFS_EFF = 0x1000000;      // EFF
    static constexpr uint32_t TXEFS_TEFL = 0x2000000;     // TEFL
    static const uint32_t TXEFS_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t TXEFA_EFAI =                // EFAI (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t TXEFA_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CKDIV_PDIV =                // input clock divider. the APB clock could be divided prior to be used by the CAN sub (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t CKDIV_RESET_VALUE = 0x0;
};

static fdcan1_t& FDCAN1 = *reinterpret_cast<fdcan1_t*>(0x40006400);

#define HAVE_PERIPHERAL_FDCAN1


////
//
//    UCPD1
//
////

struct ucpd1_t
{
    volatile uint32_t    CFG1;                 // [Read-write] UCPD configuration register 1
    volatile uint32_t    CFG2;                 // [Read-write] UCPD configuration register 2
    reserved_t<1>        _0;
    volatile uint32_t    CR;                   // [Read-write] UCPD configuration register 2
    volatile uint32_t    IMR;                  // [Read-write] UCPD Interrupt Mask Register
    volatile uint32_t    SR;                   // [Read-write] UCPD Status Register
    volatile uint32_t    ICR;                  // [Read-write] UCPD Interrupt Clear Register
    volatile uint32_t    TX_ORDSET;            // [Read-write] UCPD Tx Ordered Set Type Register
    volatile uint32_t    TX_PAYSZ;             // [Read-write] UCPD Tx Paysize Register
    volatile uint32_t    TXDR;                 // [Read-write] UCPD Tx Data Register
    volatile uint32_t    RX_ORDSET;            // [Read-only] UCPD Rx Ordered Set Register
    volatile uint32_t    RX_PAYSZ;             // [Read-only] UCPD Rx Paysize Register
    volatile uint32_t    RXDR;                 // [Read-only] UCPD Rx Data Register
    volatile uint32_t    RX_ORDEXT1;           // [Read-write] UCPD Rx Ordered Set Extension Register 1
    volatile uint32_t    RX_ORDEXT2;           // [Read-write] UCPD Rx Ordered Set Extension Register 2

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
    static constexpr uint32_t CFG1_RXDMAEN = 0x40000000; // RXDMAEN
    static constexpr uint32_t CFG1_UCPDEN = 0x80000000;  // UCPDEN
    static const uint32_t CFG1_RESET_VALUE = 0x0;

    static constexpr uint32_t CFG2_RXFILTDIS = 0x1;      // RXFILTDIS
    static constexpr uint32_t CFG2_RXFILT2N3 = 0x2;      // RXFILT2N3
    static constexpr uint32_t CFG2_FORCECLK = 0x4;       // FORCECLK
    static constexpr uint32_t CFG2_WUPEN = 0x8;          // WUPEN
    static const uint32_t CFG2_RESET_VALUE = 0x0;

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

    static constexpr uint8_t UCPD1 = 63; // UCPD1
};

static ucpd1_t& UCPD1 = *reinterpret_cast<ucpd1_t*>(0x4000a000);

#define HAVE_PERIPHERAL_UCPD1


////
//
//    USB_FS_device
//
////

struct usb_fs_device_t
{
    volatile uint32_t    EP0R;                 // [Read-write] USB endpoint n register
    volatile uint32_t    EP1R;                 // [Read-write] USB endpoint n register
    volatile uint32_t    EP2R;                 // [Read-write] USB endpoint n register
    volatile uint32_t    EP3R;                 // [Read-write] USB endpoint n register
    volatile uint32_t    EP4R;                 // [Read-write] USB endpoint n register
    volatile uint32_t    EP5R;                 // [Read-write] USB endpoint n register
    volatile uint32_t    EP6R;                 // [Read-write] USB endpoint n register
    volatile uint32_t    EP7R;                 // [Read-write] USB endpoint n register
    reserved_t<8>        _0;
    volatile uint32_t    CNTR;                 // [Read-write] USB control register
    volatile uint32_t    ISTR;                 // [Read-write] USB interrupt status register
    volatile uint32_t    FNR;                  // [Read-only] USB frame number register
    volatile uint32_t    DADDR;                // [Read-write] USB device address
    volatile uint32_t    BTABLE;               // [Read-write] Buffer table address

    template<uint32_t X>
    static constexpr uint32_t EP0R_EA =                  // EA (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP0R_STAT_TX =             // STAT_TX (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP0R_DTOG_TX = 0x40;       // DTOG_TX
    static constexpr uint32_t EP0R_CTR_TX = 0x80;        // CTR_TX
    static constexpr uint32_t EP0R_EP_KIND = 0x100;      // EP_KIND
    template<uint32_t X>
    static constexpr uint32_t EP0R_EP_TYPE =             // EP_TYPE (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP0R_SETUP = 0x800;        // SETUP
    template<uint32_t X>
    static constexpr uint32_t EP0R_STAT_RX =             // STAT_RX (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP0R_DTOG_RX = 0x4000;     // DTOG_RX
    static constexpr uint32_t EP0R_CTR_RX = 0x8000;      // CTR_RX
    static const uint32_t EP0R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP1R_EA =                  // EA (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP1R_STAT_TX =             // STAT_TX (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP1R_DTOG_TX = 0x40;       // DTOG_TX
    static constexpr uint32_t EP1R_CTR_TX = 0x80;        // CTR_TX
    static constexpr uint32_t EP1R_EP_KIND = 0x100;      // EP_KIND
    template<uint32_t X>
    static constexpr uint32_t EP1R_EP_TYPE =             // EP_TYPE (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP1R_SETUP = 0x800;        // SETUP
    template<uint32_t X>
    static constexpr uint32_t EP1R_STAT_RX =             // STAT_RX (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP1R_DTOG_RX = 0x4000;     // DTOG_RX
    static constexpr uint32_t EP1R_CTR_RX = 0x8000;      // CTR_RX
    static const uint32_t EP1R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP2R_EA =                  // EA (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP2R_STAT_TX =             // STAT_TX (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP2R_DTOG_TX = 0x40;       // DTOG_TX
    static constexpr uint32_t EP2R_CTR_TX = 0x80;        // CTR_TX
    static constexpr uint32_t EP2R_EP_KIND = 0x100;      // EP_KIND
    template<uint32_t X>
    static constexpr uint32_t EP2R_EP_TYPE =             // EP_TYPE (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP2R_SETUP = 0x800;        // SETUP
    template<uint32_t X>
    static constexpr uint32_t EP2R_STAT_RX =             // STAT_RX (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP2R_DTOG_RX = 0x4000;     // DTOG_RX
    static constexpr uint32_t EP2R_CTR_RX = 0x8000;      // CTR_RX
    static const uint32_t EP2R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP3R_EA =                  // EA (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP3R_STAT_TX =             // STAT_TX (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP3R_DTOG_TX = 0x40;       // DTOG_TX
    static constexpr uint32_t EP3R_CTR_TX = 0x80;        // CTR_TX
    static constexpr uint32_t EP3R_EP_KIND = 0x100;      // EP_KIND
    template<uint32_t X>
    static constexpr uint32_t EP3R_EP_TYPE =             // EP_TYPE (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP3R_SETUP = 0x800;        // SETUP
    template<uint32_t X>
    static constexpr uint32_t EP3R_STAT_RX =             // STAT_RX (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP3R_DTOG_RX = 0x4000;     // DTOG_RX
    static constexpr uint32_t EP3R_CTR_RX = 0x8000;      // CTR_RX
    static const uint32_t EP3R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP4R_EA =                  // EA (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP4R_STAT_TX =             // STAT_TX (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP4R_DTOG_TX = 0x40;       // DTOG_TX
    static constexpr uint32_t EP4R_CTR_TX = 0x80;        // CTR_TX
    static constexpr uint32_t EP4R_EP_KIND = 0x100;      // EP_KIND
    template<uint32_t X>
    static constexpr uint32_t EP4R_EP_TYPE =             // EP_TYPE (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP4R_SETUP = 0x800;        // SETUP
    template<uint32_t X>
    static constexpr uint32_t EP4R_STAT_RX =             // STAT_RX (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP4R_DTOG_RX = 0x4000;     // DTOG_RX
    static constexpr uint32_t EP4R_CTR_RX = 0x8000;      // CTR_RX
    static const uint32_t EP4R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP5R_EA =                  // EA (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP5R_STAT_TX =             // STAT_TX (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP5R_DTOG_TX = 0x40;       // DTOG_TX
    static constexpr uint32_t EP5R_CTR_TX = 0x80;        // CTR_TX
    static constexpr uint32_t EP5R_EP_KIND = 0x100;      // EP_KIND
    template<uint32_t X>
    static constexpr uint32_t EP5R_EP_TYPE =             // EP_TYPE (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP5R_SETUP = 0x800;        // SETUP
    template<uint32_t X>
    static constexpr uint32_t EP5R_STAT_RX =             // STAT_RX (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP5R_DTOG_RX = 0x4000;     // DTOG_RX
    static constexpr uint32_t EP5R_CTR_RX = 0x8000;      // CTR_RX
    static const uint32_t EP5R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP6R_EA =                  // EA (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP6R_STAT_TX =             // STAT_TX (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP6R_DTOG_TX = 0x40;       // DTOG_TX
    static constexpr uint32_t EP6R_CTR_TX = 0x80;        // CTR_TX
    static constexpr uint32_t EP6R_EP_KIND = 0x100;      // EP_KIND
    template<uint32_t X>
    static constexpr uint32_t EP6R_EP_TYPE =             // EP_TYPE (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP6R_SETUP = 0x800;        // SETUP
    template<uint32_t X>
    static constexpr uint32_t EP6R_STAT_RX =             // STAT_RX (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP6R_DTOG_RX = 0x4000;     // DTOG_RX
    static constexpr uint32_t EP6R_CTR_RX = 0x8000;      // CTR_RX
    static const uint32_t EP6R_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t EP7R_EA =                  // EA (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t EP7R_STAT_TX =             // STAT_TX (2 bits)
        bit_field_t<4, 0x3>::value<X>();
    static constexpr uint32_t EP7R_DTOG_TX = 0x40;       // DTOG_TX
    static constexpr uint32_t EP7R_CTR_TX = 0x80;        // CTR_TX
    static constexpr uint32_t EP7R_EP_KIND = 0x100;      // EP_KIND
    template<uint32_t X>
    static constexpr uint32_t EP7R_EP_TYPE =             // EP_TYPE (2 bits)
        bit_field_t<9, 0x3>::value<X>();
    static constexpr uint32_t EP7R_SETUP = 0x800;        // SETUP
    template<uint32_t X>
    static constexpr uint32_t EP7R_STAT_RX =             // STAT_RX (2 bits)
        bit_field_t<12, 0x3>::value<X>();
    static constexpr uint32_t EP7R_DTOG_RX = 0x4000;     // DTOG_RX
    static constexpr uint32_t EP7R_CTR_RX = 0x8000;      // CTR_RX
    static const uint32_t EP7R_RESET_VALUE = 0x0;

    static constexpr uint32_t CNTR_FRES = 0x1;           // FRES
    static constexpr uint32_t CNTR_PDWN = 0x2;           // PDWN
    static constexpr uint32_t CNTR_LP_MODE = 0x4;        // LP_MODE
    static constexpr uint32_t CNTR_FSUSP = 0x8;          // FSUSP
    static constexpr uint32_t CNTR_RESUME = 0x10;        // RESUME
    static constexpr uint32_t CNTR_L1RESUME = 0x20;      // L1RESUME
    static constexpr uint32_t CNTR_L1REQM = 0x80;        // L1REQM
    static constexpr uint32_t CNTR_ESOFM = 0x100;        // ESOFM
    static constexpr uint32_t CNTR_SOFM = 0x200;         // SOFM
    static constexpr uint32_t CNTR_RESETM = 0x400;       // RESETM
    static constexpr uint32_t CNTR_SUSPM = 0x800;        // SUSPM
    static constexpr uint32_t CNTR_WKUPM = 0x1000;       // WKUPM
    static constexpr uint32_t CNTR_ERRM = 0x2000;        // ERRM
    static constexpr uint32_t CNTR_PMAOVRM = 0x4000;     // PMAOVRM
    static constexpr uint32_t CNTR_CTRM = 0x8000;        // CTRM
    static const uint32_t CNTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t ISTR_EP_ID =               // EP_ID (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static constexpr uint32_t ISTR_DIR = 0x10;           // DIR
    static constexpr uint32_t ISTR_L1REQ = 0x80;         // L1REQ
    static constexpr uint32_t ISTR_ESOF = 0x100;         // ESOF
    static constexpr uint32_t ISTR_SOF = 0x200;          // SOF
    static constexpr uint32_t ISTR_RESET = 0x400;        // RESET
    static constexpr uint32_t ISTR_SUSP = 0x800;         // SUSP
    static constexpr uint32_t ISTR_WKUP = 0x1000;        // WKUP
    static constexpr uint32_t ISTR_ERR = 0x2000;         // ERR
    static constexpr uint32_t ISTR_PMAOVR = 0x4000;      // PMAOVR
    static constexpr uint32_t ISTR_CTR = 0x8000;         // CTR
    static const uint32_t ISTR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t FNR_FN =                  // FN (11 bits)
        bit_field_t<0, 0x7ff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t FNR_LSOF =                // LSOF (2 bits)
        bit_field_t<11, 0x3>::value<X>();
    static constexpr uint32_t FNR_LCK = 0x2000;         // LCK
    static constexpr uint32_t FNR_RXDM = 0x4000;        // RXDM
    static constexpr uint32_t FNR_RXDP = 0x8000;        // RXDP
    static const uint32_t FNR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DADDR_ADD =                 // ADD (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static constexpr uint32_t DADDR_EF = 0x80;            // EF
    static const uint32_t DADDR_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t BTABLE_BTABLE =              // BTABLE (13 bits)
        bit_field_t<3, 0x1fff>::value<X>();
    static const uint32_t BTABLE_RESET_VALUE = 0x0;
};

static usb_fs_device_t& USB_FS_device = *reinterpret_cast<usb_fs_device_t*>(0x40005c00);

#define HAVE_PERIPHERAL_USB_FS_device


////
//
//    CRS
//
////

struct crs_t
{
    volatile uint32_t    CR;                   // CRS control register
    volatile uint32_t    CFGR;                 // [Read-write] This register can be written only when the frequency error counter is disabled (CEN bit is cleared in CRS_CR). When the counter is enabled, this register is write-protected.
    volatile uint32_t    ISR;                  // [Read-only] CRS interrupt and status register
    volatile uint32_t    ICR;                  // [Read-write] CRS interrupt flag clear register

    static constexpr uint32_t CR_SYNCOKIE = 0x1;       // SYNC event OK interrupt enable, Read-write
    static constexpr uint32_t CR_SYNCWARNIE = 0x2;     // SYNC warning interrupt enable, Read-write
    static constexpr uint32_t CR_ERRIE = 0x4;          // Synchronization or trimming error interrupt enable, Read-write
    static constexpr uint32_t CR_ESYNCIE = 0x8;        // Expected SYNC interrupt enable, Read-write
    static constexpr uint32_t CR_CEN = 0x20;           // Frequency error counter enable This bit enables the oscillator clock for the frequency error counter. When this bit is set, the CRS_CFGR register is write-protected and cannot be modified., Read-write
    static constexpr uint32_t CR_AUTOTRIMEN = 0x40;    // Automatic trimming enable This bit enables the automatic hardware adjustment of TRIM bits according to the measured frequency error between two SYNC events. If this bit is set, the TRIM bits are read-only. The TRIM value can be adjusted by hardware by one or two steps at a time, depending on the measured frequency error value. Refer to Section7.3.4: Frequency error evaluation and automatic trimming for more details., Read-write
    static constexpr uint32_t CR_SWSYNC = 0x80;        // Generate software SYNC event This bit is set by software in order to generate a software SYNC event. It is automatically cleared by hardware., Read-write
    template<uint32_t X>
    static constexpr uint32_t CR_TRIM =                // HSI48 oscillator smooth trimming These bits provide a user-programmable trimming value to the HSI48 oscillator. They can be programmed to adjust to variations in voltage and temperature that influence the frequency of the HSI48. The default value is 32, which corresponds to the middle of the trimming interval. The trimming step is around 67 kHz between two consecutive TRIM steps. A higher TRIM value corresponds to a higher output frequency. When the AUTOTRIMEN bit is set, this field is controlled by hardware and is read-only. (7 bits), Read-write
        bit_field_t<8, 0x7f>::value<X>();
    static const uint32_t CR_RESET_VALUE = 0x4000;

    template<uint32_t X>
    static constexpr uint32_t CFGR_RELOAD =              // Counter reload value RELOAD is the value to be loaded in the frequency error counter with each SYNC event. Refer to Section7.3.3: Frequency error measurement for more details about counter behavior. (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_FELIM =               // Frequency error limit FELIM contains the value to be used to evaluate the captured frequency error value latched in the FECAP[15:0] bits of the CRS_ISR register. Refer to Section7.3.4: Frequency error evaluation and automatic trimming for more details about FECAP evaluation. (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_SYNCDIV =             // SYNC divider These bits are set and cleared by software to control the division factor of the SYNC signal. (3 bits)
        bit_field_t<24, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CFGR_SYNCSRC =             // SYNC signal source selection These bits are set and cleared by software to select the SYNC signal source. Note: When using USB LPM (Link Power Management) and the device is in Sleep mode, the periodic USB SOF will not be generated by the host. No SYNC signal will therefore be provided to the CRS to calibrate the HSI48 on the run. To guarantee the required clock precision after waking up from Sleep mode, the LSE or reference clock on the GPIOs should be used as SYNC signal. (2 bits)
        bit_field_t<28, 0x3>::value<X>();
    static constexpr uint32_t CFGR_SYNCPOL = 0x80000000; // SYNC polarity selection This bit is set and cleared by software to select the input polarity for the SYNC signal source.
    static const uint32_t CFGR_RESET_VALUE = 0x2022bb7f;

    static constexpr uint32_t ISR_SYNCOKF = 0x1;        // SYNC event OK flag This flag is set by hardware when the measured frequency error is smaller than FELIM * 3. This means that either no adjustment of the TRIM value is needed or that an adjustment by one trimming step is enough to compensate the frequency error. An interrupt is generated if the SYNCOKIE bit is set in the CRS_CR register. It is cleared by software by setting the SYNCOKC bit in the CRS_ICR register.
    static constexpr uint32_t ISR_SYNCWARNF = 0x2;      // SYNC warning flag This flag is set by hardware when the measured frequency error is greater than or equal to FELIM * 3, but smaller than FELIM * 128. This means that to compensate the frequency error, the TRIM value must be adjusted by two steps or more. An interrupt is generated if the SYNCWARNIE bit is set in the CRS_CR register. It is cleared by software by setting the SYNCWARNC bit in the CRS_ICR register.
    static constexpr uint32_t ISR_ERRF = 0x4;           // Error flag This flag is set by hardware in case of any synchronization or trimming error. It is the logical OR of the TRIMOVF, SYNCMISS and SYNCERR bits. An interrupt is generated if the ERRIE bit is set in the CRS_CR register. It is cleared by software in reaction to setting the ERRC bit in the CRS_ICR register, which clears the TRIMOVF, SYNCMISS and SYNCERR bits.
    static constexpr uint32_t ISR_ESYNCF = 0x8;         // Expected SYNC flag This flag is set by hardware when the frequency error counter reached a zero value. An interrupt is generated if the ESYNCIE bit is set in the CRS_CR register. It is cleared by software by setting the ESYNCC bit in the CRS_ICR register.
    static constexpr uint32_t ISR_SYNCERR = 0x100;      // SYNC error This flag is set by hardware when the SYNC pulse arrives before the ESYNC event and the measured frequency error is greater than or equal to FELIM * 128. This means that the frequency error is too big (internal frequency too low) to be compensated by adjusting the TRIM value, and that some other action should be taken. An interrupt is generated if the ERRIE bit is set in the CRS_CR register. It is cleared by software by setting the ERRC bit in the CRS_ICR register.
    static constexpr uint32_t ISR_SYNCMISS = 0x200;     // SYNC missed This flag is set by hardware when the frequency error counter reached value FELIM * 128 and no SYNC was detected, meaning either that a SYNC pulse was missed or that the frequency error is too big (internal frequency too high) to be compensated by adjusting the TRIM value, and that some other action should be taken. At this point, the frequency error counter is stopped (waiting for a next SYNC) and an interrupt is generated if the ERRIE bit is set in the CRS_CR register. It is cleared by software by setting the ERRC bit in the CRS_ICR register.
    static constexpr uint32_t ISR_TRIMOVF = 0x400;      // Trimming overflow or underflow This flag is set by hardware when the automatic trimming tries to over- or under-flow the TRIM value. An interrupt is generated if the ERRIE bit is set in the CRS_CR register. It is cleared by software by setting the ERRC bit in the CRS_ICR register.
    static constexpr uint32_t ISR_FEDIR = 0x8000;       // Frequency error direction FEDIR is the counting direction of the frequency error counter latched in the time of the last SYNC event. It shows whether the actual frequency is below or above the target.
    template<uint32_t X>
    static constexpr uint32_t ISR_FECAP =               // Frequency error capture FECAP is the frequency error counter value latched in the time ofthe last SYNC event. Refer to Section7.3.4: Frequency error evaluation and automatic trimming for more details about FECAP usage. (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t ISR_RESET_VALUE = 0x0;

    static constexpr uint32_t ICR_SYNCOKC = 0x1;        // SYNC event OK clear flag Writing 1 to this bit clears the SYNCOKF flag in the CRS_ISR register.
    static constexpr uint32_t ICR_SYNCWARNC = 0x2;      // SYNC warning clear flag Writing 1 to this bit clears the SYNCWARNF flag in the CRS_ISR register.
    static constexpr uint32_t ICR_ERRC = 0x4;           // Error clear flag Writing 1 to this bit clears TRIMOVF, SYNCMISS and SYNCERR bits and consequently also the ERRF flag in the CRS_ISR register.
    static constexpr uint32_t ICR_ESYNCC = 0x8;         // Expected SYNC clear flag Writing 1 to this bit clears the ESYNCF flag in the CRS_ISR register.
    static const uint32_t ICR_RESET_VALUE = 0x0;
};

static crs_t& CRS = *reinterpret_cast<crs_t*>(0x40002000);

#define HAVE_PERIPHERAL_CRS


template<typename PERIPHERAL> struct peripheral_traits {};

template<> struct peripheral_traits<crc_t>
{
    static void enable() { RCC.AHB1ENR |= rcc_t::AHB1ENR_CRCEN; }
    static void disable() { RCC.AHB1ENR &= ~rcc_t::AHB1ENR_CRCEN; }
    static void reset() { RCC.AHB1RSTR |= rcc_t::AHB1RSTR_CRCRST; }
};

template<> struct peripheral_traits<wwdg_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_WWDGEN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_WWDGEN; }
};

template<> struct peripheral_traits<i2c1_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_I2C1EN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_I2C1EN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_I2C1RST; }
};

template<> struct peripheral_traits<i2c2_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_I2C2EN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_I2C2EN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_I2C2RST; }
};

template<> struct peripheral_traits<pwr_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_PWREN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_PWREN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_PWRRST; }
};

template<> struct peripheral_traits<rng_t>
{
    static void enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_RNGEN; }
    static void disable() { RCC.AHB2ENR &= ~rcc_t::AHB2ENR_RNGEN; }
    static void reset() { RCC.AHB2RSTR |= rcc_t::AHB2RSTR_RNGRST; }
};

template<> struct peripheral_traits<gpioa_t>
{
    static void enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIOAEN; }
    static void disable() { RCC.AHB2ENR &= ~rcc_t::AHB2ENR_GPIOAEN; }
    static void reset() { RCC.AHB2RSTR |= rcc_t::AHB2RSTR_GPIOARST; }
};

template<> struct peripheral_traits<gpiob_t>
{
    static void enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIOBEN; }
    static void disable() { RCC.AHB2ENR &= ~rcc_t::AHB2ENR_GPIOBEN; }
    static void reset() { RCC.AHB2RSTR |= rcc_t::AHB2RSTR_GPIOBRST; }
};

template<> struct peripheral_traits<gpioc_t>
{
    static void enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIOCEN; }
    static void disable() { RCC.AHB2ENR &= ~rcc_t::AHB2ENR_GPIOCEN; }
    static void reset() { RCC.AHB2RSTR |= rcc_t::AHB2RSTR_GPIOCRST; }
};

template<> struct peripheral_traits<gpiod_t>
{
    static void enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIODEN; }
    static void disable() { RCC.AHB2ENR &= ~rcc_t::AHB2ENR_GPIODEN; }
    static void reset() { RCC.AHB2RSTR |= rcc_t::AHB2RSTR_GPIODRST; }
};

template<> struct peripheral_traits<gpioe_t>
{
    static void enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIOEEN; }
    static void disable() { RCC.AHB2ENR &= ~rcc_t::AHB2ENR_GPIOEEN; }
    static void reset() { RCC.AHB2RSTR |= rcc_t::AHB2RSTR_GPIOERST; }
};

template<> struct peripheral_traits<gpiof_t>
{
    static void enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIOFEN; }
    static void disable() { RCC.AHB2ENR &= ~rcc_t::AHB2ENR_GPIOFEN; }
    static void reset() { RCC.AHB2RSTR |= rcc_t::AHB2RSTR_GPIOFRST; }
};

template<> struct peripheral_traits<gpiog_t>
{
    static void enable() { RCC.AHB2ENR |= rcc_t::AHB2ENR_GPIOGEN; }
    static void disable() { RCC.AHB2ENR &= ~rcc_t::AHB2ENR_GPIOGEN; }
    static void reset() { RCC.AHB2RSTR |= rcc_t::AHB2RSTR_GPIOGRST; }
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
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_TIM2EN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_TIM2EN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_TIM2RST; }
};

template<> struct peripheral_traits<tim3_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_TIM3EN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_TIM3EN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_TIM3RST; }
};

template<> struct peripheral_traits<tim4_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_TIM4EN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_TIM4EN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_TIM4RST; }
};

template<> struct peripheral_traits<tim6_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_TIM6EN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_TIM6EN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_TIM6RST; }
};

template<> struct peripheral_traits<tim7_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_TIM7EN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_TIM7EN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_TIM7RST; }
};

template<> struct peripheral_traits<usart1_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_USART1EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_USART1EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_USART1RST; }
};

template<> struct peripheral_traits<usart2_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_USART2EN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_USART2EN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_USART2RST; }
};

template<> struct peripheral_traits<usart3_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_USART3EN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_USART3EN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_USART3RST; }
};

template<> struct peripheral_traits<uart4_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_UART4EN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_UART4EN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_UART4RST; }
};

template<> struct peripheral_traits<lpuart1_t>
{
    static void enable() { RCC.APB1ENR2 |= rcc_t::APB1ENR2_LPUART1EN; }
    static void disable() { RCC.APB1ENR2 &= ~rcc_t::APB1ENR2_LPUART1EN; }
    static void reset() { RCC.APB1RSTR2 |= rcc_t::APB1RSTR2_LPUART1RST; }
};

template<> struct peripheral_traits<spi1_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_SPI1EN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_SPI1EN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_SPI1RST; }
};

template<> struct peripheral_traits<spi3_t>
{
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_SPI3RST; }
};

template<> struct peripheral_traits<spi2_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_SPI2EN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_SPI2EN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_SPI2RST; }
};

template<> struct peripheral_traits<dma1_t>
{
    static void enable() { RCC.AHB1ENR |= rcc_t::AHB1ENR_DMA1EN; }
    static void disable() { RCC.AHB1ENR &= ~rcc_t::AHB1ENR_DMA1EN; }
    static void reset() { RCC.AHB1RSTR |= rcc_t::AHB1RSTR_DMA1RST; }
};

template<> struct peripheral_traits<dma2_t>
{
    static void enable() { RCC.AHB1ENR |= rcc_t::AHB1ENR_DMA2EN; }
    static void disable() { RCC.AHB1ENR &= ~rcc_t::AHB1ENR_DMA2EN; }
    static void reset() { RCC.AHB1RSTR |= rcc_t::AHB1RSTR_DMA2RST; }
};

template<> struct peripheral_traits<dmamux_t>
{
    static void enable() { RCC.AHB1ENR |= rcc_t::AHB1ENR_DMAMUXEN; }
    static void disable() { RCC.AHB1ENR &= ~rcc_t::AHB1ENR_DMAMUXEN; }
};

template<> struct peripheral_traits<syscfg_t>
{
    static void enable() { RCC.APB2ENR |= rcc_t::APB2ENR_SYSCFGEN; }
    static void disable() { RCC.APB2ENR &= ~rcc_t::APB2ENR_SYSCFGEN; }
    static void reset() { RCC.APB2RSTR |= rcc_t::APB2RSTR_SYSCFGRST; }
};

template<> struct peripheral_traits<dac2_t>
{
    static void reset() { RCC.AHB2RSTR |= rcc_t::AHB2RSTR_DAC2RST; }
};

template<> struct peripheral_traits<dac3_t>
{
    static void reset() { RCC.AHB2RSTR |= rcc_t::AHB2RSTR_DAC3RST; }
};

template<> struct peripheral_traits<dac4_t>
{
    static void reset() { RCC.AHB2RSTR |= rcc_t::AHB2RSTR_DAC4RST; }
};

template<> struct peripheral_traits<fmac_t>
{
    static void enable() { RCC.AHB1ENR |= rcc_t::AHB1ENR_FMACEN; }
    static void disable() { RCC.AHB1ENR &= ~rcc_t::AHB1ENR_FMACEN; }
};

template<> struct peripheral_traits<cordic_t>
{
    static void enable() { RCC.AHB1ENR |= rcc_t::AHB1ENR_CORDICEN; }
    static void disable() { RCC.AHB1ENR &= ~rcc_t::AHB1ENR_CORDICEN; }
    static void reset() { RCC.AHB1RSTR |= rcc_t::AHB1RSTR_CORDICRST; }
};

template<> struct peripheral_traits<fdcan_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_FDCANEN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_FDCANEN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_FDCANRST; }
};

template<> struct peripheral_traits<crs_t>
{
    static void enable() { RCC.APB1ENR1 |= rcc_t::APB1ENR1_CRSEN; }
    static void disable() { RCC.APB1ENR1 &= ~rcc_t::APB1ENR1_CRSEN; }
    static void reset() { RCC.APB1RSTR1 |= rcc_t::APB1RSTR1_CRSRST; }
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
    , PVD_PVM = 1
    , RTC_TAMP_CSS_LSE = 2
    , RTC_WKUP = 3
    , FLASH = 4
    , RCC = 5
    , EXTI0 = 6
    , EXTI1 = 7
    , EXTI2 = 8
    , EXTI3 = 9
    , EXTI4 = 10
    , DMA1_CH1 = 11
    , DMA1_CH2 = 12
    , DMA1_CH3 = 13
    , DMA1_CH4 = 14
    , DMA1_CH5 = 15
    , DMA1_CH6 = 16
    , ADC1_2 = 18
    , USB_HP = 19
    , USB_LP = 20
    , FDCAN1_INTR1_IT = 21
    , FDCAN1_INTR0_IT = 22
    , EXTI9_5 = 23
    , TIM1_BRK_TIM15 = 24
    , TIM1_UP_TIM16 = 25
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
    , RTC_ALARM = 41
    , USBWAKEUP = 42
    , TIM8_BRK = 43
    , TIM8_UP = 44
    , TIM8_TRG_COM = 45
    , TIM8_CC = 46
    , LPTIM1 = 49
    , SPI3 = 51
    , UART4 = 52
    , TIM6_DACUNDER = 54
    , TIM7 = 55
    , DMA2_CH1 = 56
    , DMA2_CH2 = 57
    , DMA2_CH3 = 58
    , DMA2_CH4 = 59
    , DMA2_CH5 = 60
    , UCPD1 = 63
    , COMP1_2_3 = 64
    , COMP4 = 65
    , CRS = 75
    , SAI = 76
    , FPU = 81
    , AES = 85
    , RNG = 90
    , LPUART = 91
    , I2C3_EV = 92
    , I2C3_ER = 93
    , DMAMUX_OVR = 94
    , DMA2_CH6 = 97
    , CORDIC = 100
    , FMAC = 101
    };
};
