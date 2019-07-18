#pragma once

#include <stdint.h>

////
//
//    nrf52
//
//       schema-version : 1.1
//       vendor         : Nordic Semiconductor
//       series         : nrf52
//       device-version : 1
//       address-unit   : 8 bits
//       device-width   : 32
//       device-size    : 32
//
////

namespace nrf52
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
//    Factory Information Configuration Registers
//
////

struct ficr_t
{
    volatile uint32_t    CODEPAGESIZE;         // [Read-only] Code memory page size
    volatile uint32_t    CODESIZE;             // [Read-only] Code memory size
    reserved_t<17>       _0;
    volatile uint32_t    CONFIGID;             // [Read-only] Configuration identifier
    volatile uint32_t    DEVICEID[s];          // [Read-only] Description collection[0]: Device identifier
    reserved_t<7>        _1;
    volatile uint32_t    ER[s];                // [Read-only] Description collection[0]: Encryption Root, word 0
    reserved_t<3>        _2;
    volatile uint32_t    IR[s];                // [Read-only] Description collection[0]: Identity Root, word 0
    reserved_t<3>        _3;
    volatile uint32_t    DEVICEADDRTYPE;       // [Read-only] Device address type
    volatile uint32_t    DEVICEADDR[s];        // [Read-only] Description collection[0]: Device address 0


    static const uint32_t CODEPAGESIZE_RESET_VALUE = 0xffffffff;


    static const uint32_t CODESIZE_RESET_VALUE = 0xffffffff;

    template<uint32_t X>
    static constexpr uint32_t CONFIGID_HWID =                // Identification number for the HW (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CONFIGID_FWID =                // Deprecated field - Identification number for the FW that is pre-loaded into the chip (16 bits)
        bit_field_t<16, 0xffff>::value<X>();
    static const uint32_t CONFIGID_RESET_VALUE = 0xffffffff;


    static const uint32_t DEVICEID[%s]_RESET_VALUE = 0xffffffff;


    static const uint32_t ER[%s]_RESET_VALUE = 0xffffffff;


    static const uint32_t IR[%s]_RESET_VALUE = 0xffffffff;

    static constexpr uint32_t DEVICEADDRTYPE_DEVICEADDRTYPE = 0x1; // Device address type
    static const uint32_t DEVICEADDRTYPE_RESET_VALUE = 0xffffffff;


    static const uint32_t DEVICEADDR[%s]_RESET_VALUE = 0xffffffff;
};

static ficr_t& FICR = *reinterpret_cast<ficr_t*>(0x10000000);

#define HAVE_PERIPHERAL_FICR


////
//
//    User Information Configuration Registers
//
////

struct uicr_t
{
    volatile uint32_t    UNUSED0;              // [Read-write] Unspecified
    volatile uint32_t    UNUSED1;              // [Read-write] Unspecified
    volatile uint32_t    UNUSED2;              // [Read-write] Unspecified
    reserved_t<1>        _0;
    volatile uint32_t    UNUSED3;              // [Read-write] Unspecified
    volatile uint32_t    NRFFW[s];             // [Read-write] Description collection[0]: Reserved for Nordic firmware design
    reserved_t<14>       _1;
    volatile uint32_t    NRFHW[s];             // [Read-write] Description collection[0]: Reserved for Nordic hardware design
    reserved_t<11>       _2;
    volatile uint32_t    CUSTOMER[s];          // [Read-write] Description collection[0]: Reserved for customer
    reserved_t<95>       _3;
    volatile uint32_t    PSELRESET[s];         // [Read-write] Description collection[0]: Mapping of the nRESET function (see POWER chapter for details)
    reserved_t<1>        _4;
    volatile uint32_t    APPROTECT;            // [Read-write] Access port protection
    volatile uint32_t    NFCPINS;              // [Read-write] Setting of pins dedicated to NFC functionality: NFC antenna or GPIO






    static const uint32_t NRFFW[%s]_RESET_VALUE = 0xffffffff;


    static const uint32_t NRFHW[%s]_RESET_VALUE = 0xffffffff;


    static const uint32_t CUSTOMER[%s]_RESET_VALUE = 0xffffffff;

    template<uint32_t X>
    static constexpr uint32_t PSELRESET[%s]_PIN =                 // GPIO number P0.n onto which Reset is exposed (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static constexpr uint32_t PSELRESET[%s]_CONNECT = 0x80000000; // Connection
    static const uint32_t PSELRESET[%s]_RESET_VALUE = 0xffffffff;

    template<uint32_t X>
    static constexpr uint32_t APPROTECT_PALL =                // Blocks debugger read/write access to all CPU registers and memory mapped addresses except for the Control Access Port registers. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t APPROTECT_RESET_VALUE = 0xffffffff;

    static constexpr uint32_t NFCPINS_PROTECT = 0x1;        // Setting of pins dedicated to NFC functionality
    static const uint32_t NFCPINS_RESET_VALUE = 0xffffffff;
};

static uicr_t& UICR = *reinterpret_cast<uicr_t*>(0x10001000);

#define HAVE_PERIPHERAL_UICR


////
//
//    Block Protect
//
////

struct bprot_t
{
    volatile uint32_t    CONFIG0;              // [Read-write] Block protect configuration register 0
    volatile uint32_t    CONFIG1;              // [Read-write] Block protect configuration register 1
    volatile uint32_t    DISABLEINDEBUG;       // [Read-write] Disable protection mechanism in debug mode
    volatile uint32_t    UNUSED0;              // [Read-write] Unspecified
    volatile uint32_t    CONFIG2;              // [Read-write] Block protect configuration register 2
    volatile uint32_t    CONFIG3;              // [Read-write] Block protect configuration register 3

    static constexpr uint32_t CONFIG0_REGION0 = 0x1;        // Enable protection for region 0. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION1 = 0x2;        // Enable protection for region 1. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION2 = 0x4;        // Enable protection for region 2. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION3 = 0x8;        // Enable protection for region 3. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION4 = 0x10;       // Enable protection for region 4. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION5 = 0x20;       // Enable protection for region 5. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION6 = 0x40;       // Enable protection for region 6. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION7 = 0x80;       // Enable protection for region 7. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION8 = 0x100;      // Enable protection for region 8. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION9 = 0x200;      // Enable protection for region 9. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION10 = 0x400;     // Enable protection for region 10. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION11 = 0x800;     // Enable protection for region 11. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION12 = 0x1000;    // Enable protection for region 12. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION13 = 0x2000;    // Enable protection for region 13. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION14 = 0x4000;    // Enable protection for region 14. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION15 = 0x8000;    // Enable protection for region 15. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION16 = 0x10000;   // Enable protection for region 16. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION17 = 0x20000;   // Enable protection for region 17. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION18 = 0x40000;   // Enable protection for region 18. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION19 = 0x80000;   // Enable protection for region 19. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION20 = 0x100000;  // Enable protection for region 20. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION21 = 0x200000;  // Enable protection for region 21. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION22 = 0x400000;  // Enable protection for region 22. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION23 = 0x800000;  // Enable protection for region 23. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION24 = 0x1000000; // Enable protection for region 24. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION25 = 0x2000000; // Enable protection for region 25. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION26 = 0x4000000; // Enable protection for region 26. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION27 = 0x8000000; // Enable protection for region 27. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION28 = 0x10000000;// Enable protection for region 28. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION29 = 0x20000000;// Enable protection for region 29. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION30 = 0x40000000;// Enable protection for region 30. Write '0' has no effect.
    static constexpr uint32_t CONFIG0_REGION31 = 0x80000000;// Enable protection for region 31. Write '0' has no effect.

    static constexpr uint32_t CONFIG1_REGION32 = 0x1;       // Enable protection for region 32. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION33 = 0x2;       // Enable protection for region 33. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION34 = 0x4;       // Enable protection for region 34. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION35 = 0x8;       // Enable protection for region 35. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION36 = 0x10;      // Enable protection for region 36. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION37 = 0x20;      // Enable protection for region 37. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION38 = 0x40;      // Enable protection for region 38. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION39 = 0x80;      // Enable protection for region 39. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION40 = 0x100;     // Enable protection for region 40. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION41 = 0x200;     // Enable protection for region 41. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION42 = 0x400;     // Enable protection for region 42. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION43 = 0x800;     // Enable protection for region 43. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION44 = 0x1000;    // Enable protection for region 44. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION45 = 0x2000;    // Enable protection for region 45. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION46 = 0x4000;    // Enable protection for region 46. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION47 = 0x8000;    // Enable protection for region 47. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION48 = 0x10000;   // Enable protection for region 48. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION49 = 0x20000;   // Enable protection for region 49. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION50 = 0x40000;   // Enable protection for region 50. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION51 = 0x80000;   // Enable protection for region 51. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION52 = 0x100000;  // Enable protection for region 52. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION53 = 0x200000;  // Enable protection for region 53. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION54 = 0x400000;  // Enable protection for region 54. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION55 = 0x800000;  // Enable protection for region 55. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION56 = 0x1000000; // Enable protection for region 56. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION57 = 0x2000000; // Enable protection for region 57. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION58 = 0x4000000; // Enable protection for region 58. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION59 = 0x8000000; // Enable protection for region 59. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION60 = 0x10000000;// Enable protection for region 60. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION61 = 0x20000000;// Enable protection for region 61. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION62 = 0x40000000;// Enable protection for region 62. Write '0' has no effect.
    static constexpr uint32_t CONFIG1_REGION63 = 0x80000000;// Enable protection for region 63. Write '0' has no effect.

    static constexpr uint32_t DISABLEINDEBUG_DISABLEINDEBUG = 0x1; // Disable the protection mechanism for NVM regions while in debug mode. This register will only disable the protection mechanism if the device is in debug mode.
    static const uint32_t DISABLEINDEBUG_RESET_VALUE = 0x1;


    static constexpr uint32_t CONFIG2_REGION64 = 0x1;       // Enable protection for region 64. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION65 = 0x2;       // Enable protection for region 65. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION66 = 0x4;       // Enable protection for region 66. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION67 = 0x8;       // Enable protection for region 67. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION68 = 0x10;      // Enable protection for region 68. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION69 = 0x20;      // Enable protection for region 69. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION70 = 0x40;      // Enable protection for region 70. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION71 = 0x80;      // Enable protection for region 71. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION72 = 0x100;     // Enable protection for region 72. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION73 = 0x200;     // Enable protection for region 73. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION74 = 0x400;     // Enable protection for region 74. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION75 = 0x800;     // Enable protection for region 75. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION76 = 0x1000;    // Enable protection for region 76. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION77 = 0x2000;    // Enable protection for region 77. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION78 = 0x4000;    // Enable protection for region 78. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION79 = 0x8000;    // Enable protection for region 79. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION80 = 0x10000;   // Enable protection for region 80. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION81 = 0x20000;   // Enable protection for region 81. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION82 = 0x40000;   // Enable protection for region 82. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION83 = 0x80000;   // Enable protection for region 83. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION84 = 0x100000;  // Enable protection for region 84. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION85 = 0x200000;  // Enable protection for region 85. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION86 = 0x400000;  // Enable protection for region 86. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION87 = 0x800000;  // Enable protection for region 87. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION88 = 0x1000000; // Enable protection for region 88. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION89 = 0x2000000; // Enable protection for region 89. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION90 = 0x4000000; // Enable protection for region 90. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION91 = 0x8000000; // Enable protection for region 91. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION92 = 0x10000000;// Enable protection for region 92. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION93 = 0x20000000;// Enable protection for region 93. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION94 = 0x40000000;// Enable protection for region 94. Write '0' has no effect.
    static constexpr uint32_t CONFIG2_REGION95 = 0x80000000;// Enable protection for region 95. Write '0' has no effect.

    static constexpr uint32_t CONFIG3_REGION96 = 0x1;       // Enable protection for region 96. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION97 = 0x2;       // Enable protection for region 97. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION98 = 0x4;       // Enable protection for region 98. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION99 = 0x8;       // Enable protection for region 99. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION100 = 0x10;     // Enable protection for region 100. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION101 = 0x20;     // Enable protection for region 101. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION102 = 0x40;     // Enable protection for region 102. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION103 = 0x80;     // Enable protection for region 103. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION104 = 0x100;    // Enable protection for region 104. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION105 = 0x200;    // Enable protection for region 105. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION106 = 0x400;    // Enable protection for region 106. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION107 = 0x800;    // Enable protection for region 107. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION108 = 0x1000;   // Enable protection for region 108. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION109 = 0x2000;   // Enable protection for region 109. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION110 = 0x4000;   // Enable protection for region 110. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION111 = 0x8000;   // Enable protection for region 111. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION112 = 0x10000;  // Enable protection for region 112. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION113 = 0x20000;  // Enable protection for region 113. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION114 = 0x40000;  // Enable protection for region 114. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION115 = 0x80000;  // Enable protection for region 115. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION116 = 0x100000; // Enable protection for region 116. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION117 = 0x200000; // Enable protection for region 117. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION118 = 0x400000; // Enable protection for region 118. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION119 = 0x800000; // Enable protection for region 119. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION120 = 0x1000000;// Enable protection for region 120. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION121 = 0x2000000;// Enable protection for region 121. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION122 = 0x4000000;// Enable protection for region 122. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION123 = 0x8000000;// Enable protection for region 123. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION124 = 0x10000000;// Enable protection for region 124. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION125 = 0x20000000;// Enable protection for region 125. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION126 = 0x40000000;// Enable protection for region 126. Write '0' has no effect.
    static constexpr uint32_t CONFIG3_REGION127 = 0x80000000;// Enable protection for region 127. Write '0' has no effect.
};

static bprot_t& BPROT = *reinterpret_cast<bprot_t*>(0x40000000);

#define HAVE_PERIPHERAL_BPROT


////
//
//    Power control
//
////

struct power_t
{
    volatile uint32_t    TASKS_CONSTLAT;       // [Write-only] Enable constant latency mode
    volatile uint32_t    TASKS_LOWPWR;         // [Write-only] Enable low power mode (variable latency)
    reserved_t<34>       _0;
    volatile uint32_t    EVENTS_POFWARN;       // [Read-write] Power failure warning
    reserved_t<2>        _1;
    volatile uint32_t    EVENTS_SLEEPENTER;    // [Read-write] CPU entered WFI/WFE sleep
    volatile uint32_t    EVENTS_SLEEPEXIT;     // [Read-write] CPU exited WFI/WFE sleep
    reserved_t<122>      _2;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<61>       _3;
    volatile uint32_t    RESETREAS;            // [Read-write] Reset reason
    reserved_t<9>        _4;
    volatile uint32_t    RAMSTATUS;            // [Read-only] Deprecated register - RAM status register
    reserved_t<53>       _5;
    volatile uint32_t    SYSTEMOFF;            // [Write-only] System OFF register
    reserved_t<3>        _6;
    volatile uint32_t    POFCON;               // [Read-write] Power failure comparator configuration
    reserved_t<2>        _7;
    volatile uint32_t    GPREGRET;             // [Read-write] General purpose retention register
    volatile uint32_t    GPREGRET2;            // [Read-write] General purpose retention register
    volatile uint32_t    RAMON;                // [Read-write] Deprecated register - RAM on/off register (this register is retained)
    reserved_t<11>       _8;
    volatile uint32_t    RAMONB;               // [Read-write] Deprecated register - RAM on/off register (this register is retained)
    reserved_t<8>        _9;
    volatile uint32_t    DCDCEN;               // [Read-write] DC/DC enable register






    static constexpr uint32_t INTENSET_POFWARN = 0x4;        // Write '1' to Enable interrupt on EVENTS_POFWARN event
    static constexpr uint32_t INTENSET_SLEEPENTER = 0x20;    // Write '1' to Enable interrupt on EVENTS_SLEEPENTER event
    static constexpr uint32_t INTENSET_SLEEPEXIT = 0x40;     // Write '1' to Enable interrupt on EVENTS_SLEEPEXIT event

    static constexpr uint32_t INTENCLR_POFWARN = 0x4;        // Write '1' to Clear interrupt on EVENTS_POFWARN event
    static constexpr uint32_t INTENCLR_SLEEPENTER = 0x20;    // Write '1' to Clear interrupt on EVENTS_SLEEPENTER event
    static constexpr uint32_t INTENCLR_SLEEPEXIT = 0x40;     // Write '1' to Clear interrupt on EVENTS_SLEEPEXIT event

    static constexpr uint32_t RESETREAS_RESETPIN = 0x1;       // Reset from pin-reset detected
    static constexpr uint32_t RESETREAS_DOG = 0x2;            // Reset from watchdog detected
    static constexpr uint32_t RESETREAS_SREQ = 0x4;           // Reset from AIRCR.SYSRESETREQ detected
    static constexpr uint32_t RESETREAS_LOCKUP = 0x8;         // Reset from CPU lock-up detected
    static constexpr uint32_t RESETREAS_OFF = 0x10000;        // Reset due to wake up from System OFF mode when wakeup is triggered from DETECT signal from GPIO
    static constexpr uint32_t RESETREAS_LPCOMP = 0x20000;     // Reset due to wake up from System OFF mode when wakeup is triggered from ANADETECT signal from LPCOMP
    static constexpr uint32_t RESETREAS_DIF = 0x40000;        // Reset due to wake up from System OFF mode when wakeup is triggered from entering into debug interface mode
    static constexpr uint32_t RESETREAS_NFC = 0x80000;        // Reset due to wake up from System OFF mode by NFC field detect

    static constexpr uint32_t RAMSTATUS_RAMBLOCK0 = 0x1;      // RAM block 0 is on or off/powering up
    static constexpr uint32_t RAMSTATUS_RAMBLOCK1 = 0x2;      // RAM block 1 is on or off/powering up
    static constexpr uint32_t RAMSTATUS_RAMBLOCK2 = 0x4;      // RAM block 2 is on or off/powering up
    static constexpr uint32_t RAMSTATUS_RAMBLOCK3 = 0x8;      // RAM block 3 is on or off/powering up
    static const uint32_t RAMSTATUS_RESET_VALUE = 0x0;

    static constexpr uint32_t SYSTEMOFF_SYSTEMOFF = 0x1;      // Enable System OFF mode

    static constexpr uint32_t POFCON_POF = 0x1;            // Enable or disable power failure comparator
    template<uint32_t X>
    static constexpr uint32_t POFCON_THRESHOLD =           // Power failure comparator threshold setting (4 bits)
        bit_field_t<1, 0xf>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t GPREGRET_GPREGRET =            // General purpose retention register (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t GPREGRET2_GPREGRET =            // General purpose retention register (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    static constexpr uint32_t RAMON_ONRAM0 = 0x1;         // Keep RAM block 0 on or off in system ON Mode
    static constexpr uint32_t RAMON_ONRAM1 = 0x2;         // Keep RAM block 1 on or off in system ON Mode
    static constexpr uint32_t RAMON_OFFRAM0 = 0x10000;    // Keep retention on RAM block 0 when RAM block is switched off
    static constexpr uint32_t RAMON_OFFRAM1 = 0x20000;    // Keep retention on RAM block 1 when RAM block is switched off
    static const uint32_t RAMON_RESET_VALUE = 0x3;

    static constexpr uint32_t RAMONB_ONRAM2 = 0x1;         // Keep RAM block 2 on or off in system ON Mode
    static constexpr uint32_t RAMONB_ONRAM3 = 0x2;         // Keep RAM block 3 on or off in system ON Mode
    static constexpr uint32_t RAMONB_OFFRAM2 = 0x10000;    // Keep retention on RAM block 2 when RAM block is switched off
    static constexpr uint32_t RAMONB_OFFRAM3 = 0x20000;    // Keep retention on RAM block 3 when RAM block is switched off
    static const uint32_t RAMONB_RESET_VALUE = 0x3;

    static constexpr uint32_t DCDCEN_DCDCEN = 0x1;         // Enable or disable DC/DC converter

    static constexpr uint8_t POWER_CLOCK = 0; // 
};

static power_t& POWER = *reinterpret_cast<power_t*>(0x40000000);

#define HAVE_PERIPHERAL_POWER


////
//
//    Clock control
//
////

struct clock_t
{
    volatile uint32_t    TASKS_HFCLKSTART;     // [Write-only] Start HFCLK crystal oscillator
    volatile uint32_t    TASKS_HFCLKSTOP;      // [Write-only] Stop HFCLK crystal oscillator
    volatile uint32_t    TASKS_LFCLKSTART;     // [Write-only] Start LFCLK source
    volatile uint32_t    TASKS_LFCLKSTOP;      // [Write-only] Stop LFCLK source
    volatile uint32_t    TASKS_CAL;            // [Write-only] Start calibration of LFRC or LFULP oscillator
    volatile uint32_t    TASKS_CTSTART;        // [Write-only] Start calibration timer
    volatile uint32_t    TASKS_CTSTOP;         // [Write-only] Stop calibration timer
    reserved_t<57>       _0;
    volatile uint32_t    EVENTS_HFCLKSTARTED;  // [Read-write] HFCLK oscillator started
    volatile uint32_t    EVENTS_LFCLKSTARTED;  // [Read-write] LFCLK started
    reserved_t<1>        _1;
    volatile uint32_t    EVENTS_DONE;          // [Read-write] Calibration of LFCLK RC oscillator complete event
    volatile uint32_t    EVENTS_CTTO;          // [Read-write] Calibration timer timeout
    reserved_t<124>      _2;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<63>       _3;
    volatile uint32_t    HFCLKRUN;             // [Read-only] Status indicating that HFCLKSTART task has been triggered
    volatile uint32_t    HFCLKSTAT;            // [Read-only] Which HFCLK source is running
    reserved_t<1>        _4;
    volatile uint32_t    LFCLKRUN;             // [Read-only] Status indicating that LFCLKSTART task has been triggered
    volatile uint32_t    LFCLKSTAT;            // [Read-only] Which LFCLK source is running
    volatile uint32_t    LFCLKSRCCOPY;         // [Read-only] Copy of LFCLKSRC register, set when LFCLKSTART task was triggered
    reserved_t<62>       _5;
    volatile uint32_t    LFCLKSRC;             // [Read-write] Clock source for the LFCLK
    reserved_t<7>        _6;
    volatile uint32_t    CTIV;                 // [Read-write] Calibration timer interval (retained register, same reset behaviour as RESETREAS)
    reserved_t<8>        _7;
    volatile uint32_t    TRACECONFIG;          // [Read-write] Clocking options for the Trace Port debug interface












    static constexpr uint32_t INTENSET_HFCLKSTARTED = 0x1;   // Write '1' to Enable interrupt on EVENTS_HFCLKSTARTED event
    static constexpr uint32_t INTENSET_LFCLKSTARTED = 0x2;   // Write '1' to Enable interrupt on EVENTS_LFCLKSTARTED event
    static constexpr uint32_t INTENSET_DONE = 0x8;           // Write '1' to Enable interrupt on EVENTS_DONE event
    static constexpr uint32_t INTENSET_CTTO = 0x10;          // Write '1' to Enable interrupt on EVENTS_CTTO event

    static constexpr uint32_t INTENCLR_HFCLKSTARTED = 0x1;   // Write '1' to Clear interrupt on EVENTS_HFCLKSTARTED event
    static constexpr uint32_t INTENCLR_LFCLKSTARTED = 0x2;   // Write '1' to Clear interrupt on EVENTS_LFCLKSTARTED event
    static constexpr uint32_t INTENCLR_DONE = 0x8;           // Write '1' to Clear interrupt on EVENTS_DONE event
    static constexpr uint32_t INTENCLR_CTTO = 0x10;          // Write '1' to Clear interrupt on EVENTS_CTTO event

    static constexpr uint32_t HFCLKRUN_STATUS = 0x1;         // HFCLKSTART task triggered or not

    static constexpr uint32_t HFCLKSTAT_SRC = 0x1;            // Active clock source
    static constexpr uint32_t HFCLKSTAT_STATE = 0x10000;      // HFCLK state

    static constexpr uint32_t LFCLKRUN_STATUS = 0x1;         // LFCLKSTART task triggered or not

    template<uint32_t X>
    static constexpr uint32_t LFCLKSTAT_SRC =                 // Active clock source (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static constexpr uint32_t LFCLKSTAT_STATE = 0x10000;      // LFCLK state

    template<uint32_t X>
    static constexpr uint32_t LFCLKSRCCOPY_SRC =                 // Clock source (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t LFCLKSRC_SRC =                 // Clock source (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t CTIV_CTIV =                // Calibration timer interval in multiple of 0.25 seconds. Range: 0.25 seconds to 31.75 seconds. (7 bits)
        bit_field_t<0, 0x7f>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t TRACECONFIG_TRACEPORTSPEED =      // Speed of Trace Port clock. Note that the TRACECLK pin will output this clock divided by two. (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TRACECONFIG_TRACEMUX =            // Pin multiplexing of trace signals. (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    static const uint32_t TRACECONFIG_RESET_VALUE = 0x0;

    static constexpr uint8_t POWER_CLOCK = 0; // 
};

static clock_t& CLOCK = *reinterpret_cast<clock_t*>(0x40000000);

#define HAVE_PERIPHERAL_CLOCK


////
//
//    2.4 GHz Radio
//
////

struct radio_t
{
    volatile uint32_t    TASKS_TXEN;           // [Write-only] Enable RADIO in TX mode
    volatile uint32_t    TASKS_RXEN;           // [Write-only] Enable RADIO in RX mode
    volatile uint32_t    TASKS_START;          // [Write-only] Start RADIO
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop RADIO
    volatile uint32_t    TASKS_DISABLE;        // [Write-only] Disable RADIO
    volatile uint32_t    TASKS_RSSISTART;      // [Write-only] Start the RSSI and take one single sample of the receive signal strength.
    volatile uint32_t    TASKS_RSSISTOP;       // [Write-only] Stop the RSSI measurement
    volatile uint32_t    TASKS_BCSTART;        // [Write-only] Start the bit counter
    volatile uint32_t    TASKS_BCSTOP;         // [Write-only] Stop the bit counter
    reserved_t<55>       _0;
    volatile uint32_t    EVENTS_READY;         // [Read-write] RADIO has ramped up and is ready to be started
    volatile uint32_t    EVENTS_ADDRESS;       // [Read-write] Address sent or received
    volatile uint32_t    EVENTS_PAYLOAD;       // [Read-write] Packet payload sent or received
    volatile uint32_t    EVENTS_END;           // [Read-write] Packet sent or received
    volatile uint32_t    EVENTS_DISABLED;      // [Read-write] RADIO has been disabled
    volatile uint32_t    EVENTS_DEVMATCH;      // [Read-write] A device address match occurred on the last received packet
    volatile uint32_t    EVENTS_DEVMISS;       // [Read-write] No device address match occurred on the last received packet
    volatile uint32_t    EVENTS_RSSIEND;       // [Read-write] Sampling of receive signal strength complete. A new RSSI sample is ready for readout from the "RADIO.RSSISAMPLE" register
    reserved_t<2>        _1;
    volatile uint32_t    EVENTS_BCMATCH;       // [Read-write] Bit counter reached bit count value specified in the "RADIO.BCC" register
    reserved_t<1>        _2;
    volatile uint32_t    EVENTS_CRCOK;         // [Read-write] Packet received with CRC ok
    volatile uint32_t    EVENTS_CRCERROR;      // [Read-write] Packet received with CRC error
    reserved_t<50>       _3;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _4;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<61>       _5;
    volatile uint32_t    CRCSTATUS;            // [Read-only] CRC status
    reserved_t<1>        _6;
    volatile uint32_t    RXMATCH;              // [Read-only] Received address
    volatile uint32_t    RXCRC;                // [Read-only] CRC field of previously received packet
    volatile uint32_t    DAI;                  // [Read-only] Device address match index
    reserved_t<60>       _7;
    volatile uint32_t    PACKETPTR;            // [Read-write] Packet pointer
    volatile uint32_t    FREQUENCY;            // [Read-write] Frequency
    volatile uint32_t    TXPOWER;              // [Read-write] Output power
    volatile uint32_t    MODE;                 // [Read-write] Data rate and modulation
    volatile uint32_t    PCNF0;                // [Read-write] Packet configuration register 0
    volatile uint32_t    PCNF1;                // [Read-write] Packet configuration register 1
    volatile uint32_t    BASE0;                // [Read-write] Base address 0
    volatile uint32_t    BASE1;                // [Read-write] Base address 1
    volatile uint32_t    PREFIX0;              // [Read-write] Prefixes bytes for logical addresses 0-3
    volatile uint32_t    PREFIX1;              // [Read-write] Prefixes bytes for logical addresses 4-7
    volatile uint32_t    TXADDRESS;            // [Read-write] Transmit address select
    volatile uint32_t    RXADDRESSES;          // [Read-write] Receive address select
    volatile uint32_t    CRCCNF;               // [Read-write] CRC configuration
    volatile uint32_t    CRCPOLY;              // [Read-write] CRC polynomial
    volatile uint32_t    CRCINIT;              // [Read-write] CRC initial value
    volatile uint32_t    UNUSED0;              // [Read-write] Unspecified
    volatile uint32_t    TIFS;                 // [Read-write] Inter Frame Spacing in us
    volatile uint32_t    RSSISAMPLE;           // [Read-only] RSSI sample
    reserved_t<1>        _8;
    volatile uint32_t    STATE;                // [Read-only] Current radio state
    volatile uint32_t    DATAWHITEIV;          // [Read-write] Data whitening initial value
    reserved_t<2>        _9;
    volatile uint32_t    BCC;                  // [Read-write] Bit counter compare
    reserved_t<39>       _10;
    volatile uint32_t    DAB[s];               // [Read-write] Description collection[0]: Device address base segment 0
    reserved_t<7>        _11;
    volatile uint32_t    DAP[s];               // [Read-write] Description collection[0]: Device address prefix 0
    reserved_t<7>        _12;
    volatile uint32_t    DACNF;                // [Read-write] Device address match configuration
    reserved_t<3>        _13;
    volatile uint32_t    MODECNF0;             // [Read-write] Radio mode configuration register 0
    reserved_t<618>      _14;
    volatile uint32_t    POWER;                // [Read-write] Peripheral power control





















    static constexpr uint32_t SHORTS_READY_START = 0x1;    // Shortcut between EVENTS_READY event and TASKS_START task
    static constexpr uint32_t SHORTS_END_DISABLE = 0x2;    // Shortcut between EVENTS_END event and TASKS_DISABLE task
    static constexpr uint32_t SHORTS_DISABLED_TXEN = 0x4;  // Shortcut between EVENTS_DISABLED event and TASKS_TXEN task
    static constexpr uint32_t SHORTS_DISABLED_RXEN = 0x8;  // Shortcut between EVENTS_DISABLED event and TASKS_RXEN task
    static constexpr uint32_t SHORTS_ADDRESS_RSSISTART = 0x10;// Shortcut between EVENTS_ADDRESS event and TASKS_RSSISTART task
    static constexpr uint32_t SHORTS_END_START = 0x20;     // Shortcut between EVENTS_END event and TASKS_START task
    static constexpr uint32_t SHORTS_ADDRESS_BCSTART = 0x40;// Shortcut between EVENTS_ADDRESS event and TASKS_BCSTART task
    static constexpr uint32_t SHORTS_DISABLED_RSSISTOP = 0x100;// Shortcut between EVENTS_DISABLED event and TASKS_RSSISTOP task

    static constexpr uint32_t INTENSET_READY = 0x1;          // Write '1' to Enable interrupt on EVENTS_READY event
    static constexpr uint32_t INTENSET_ADDRESS = 0x2;        // Write '1' to Enable interrupt on EVENTS_ADDRESS event
    static constexpr uint32_t INTENSET_PAYLOAD = 0x4;        // Write '1' to Enable interrupt on EVENTS_PAYLOAD event
    static constexpr uint32_t INTENSET_END = 0x8;            // Write '1' to Enable interrupt on EVENTS_END event
    static constexpr uint32_t INTENSET_DISABLED = 0x10;      // Write '1' to Enable interrupt on EVENTS_DISABLED event
    static constexpr uint32_t INTENSET_DEVMATCH = 0x20;      // Write '1' to Enable interrupt on EVENTS_DEVMATCH event
    static constexpr uint32_t INTENSET_DEVMISS = 0x40;       // Write '1' to Enable interrupt on EVENTS_DEVMISS event
    static constexpr uint32_t INTENSET_RSSIEND = 0x80;       // Write '1' to Enable interrupt on EVENTS_RSSIEND event
    static constexpr uint32_t INTENSET_BCMATCH = 0x400;      // Write '1' to Enable interrupt on EVENTS_BCMATCH event
    static constexpr uint32_t INTENSET_CRCOK = 0x1000;       // Write '1' to Enable interrupt on EVENTS_CRCOK event
    static constexpr uint32_t INTENSET_CRCERROR = 0x2000;    // Write '1' to Enable interrupt on EVENTS_CRCERROR event

    static constexpr uint32_t INTENCLR_READY = 0x1;          // Write '1' to Clear interrupt on EVENTS_READY event
    static constexpr uint32_t INTENCLR_ADDRESS = 0x2;        // Write '1' to Clear interrupt on EVENTS_ADDRESS event
    static constexpr uint32_t INTENCLR_PAYLOAD = 0x4;        // Write '1' to Clear interrupt on EVENTS_PAYLOAD event
    static constexpr uint32_t INTENCLR_END = 0x8;            // Write '1' to Clear interrupt on EVENTS_END event
    static constexpr uint32_t INTENCLR_DISABLED = 0x10;      // Write '1' to Clear interrupt on EVENTS_DISABLED event
    static constexpr uint32_t INTENCLR_DEVMATCH = 0x20;      // Write '1' to Clear interrupt on EVENTS_DEVMATCH event
    static constexpr uint32_t INTENCLR_DEVMISS = 0x40;       // Write '1' to Clear interrupt on EVENTS_DEVMISS event
    static constexpr uint32_t INTENCLR_RSSIEND = 0x80;       // Write '1' to Clear interrupt on EVENTS_RSSIEND event
    static constexpr uint32_t INTENCLR_BCMATCH = 0x400;      // Write '1' to Clear interrupt on EVENTS_BCMATCH event
    static constexpr uint32_t INTENCLR_CRCOK = 0x1000;       // Write '1' to Clear interrupt on EVENTS_CRCOK event
    static constexpr uint32_t INTENCLR_CRCERROR = 0x2000;    // Write '1' to Clear interrupt on EVENTS_CRCERROR event

    static constexpr uint32_t CRCSTATUS_CRCSTATUS = 0x1;      // CRC status of packet received

    template<uint32_t X>
    static constexpr uint32_t RXMATCH_RXMATCH =             // Received address (3 bits)
        bit_field_t<0, 0x7>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t RXCRC_RXCRC =               // CRC field of previously received packet (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t DAI_DAI =                 // Device address match index (3 bits)
        bit_field_t<0, 0x7>::value<X>();



    template<uint32_t X>
    static constexpr uint32_t FREQUENCY_FREQUENCY =           // Radio channel frequency (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t FREQUENCY_RESET_VALUE = 0x2;

    template<uint32_t X>
    static constexpr uint32_t TXPOWER_TXPOWER =             // RADIO output power. (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t MODE_MODE =                // Radio data rate and modulation setting. The radio supports Frequency-shift Keying (FSK) modulation. (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t PCNF0_LFLEN =               // Length on air of LENGTH field in number of bits. (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static constexpr uint32_t PCNF0_S0LEN = 0x100;        // Length on air of S0 field in number of bytes.
    template<uint32_t X>
    static constexpr uint32_t PCNF0_S1LEN =               // Length on air of S1 field in number of bits. (4 bits)
        bit_field_t<16, 0xf>::value<X>();
    static constexpr uint32_t PCNF0_S1INCL = 0x100000;    // Include or exclude S1 field in RAM
    static constexpr uint32_t PCNF0_PLEN = 0x1000000;     // Length of preamble on air. Decision point: "RADIO.TASKS_START" task

    template<uint32_t X>
    static constexpr uint32_t PCNF1_MAXLEN =              // Maximum length of packet payload. If the packet payload is larger than MAXLEN, the radio will truncate the payload to MAXLEN. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PCNF1_STATLEN =             // Static length in number of bytes (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PCNF1_BALEN =               // Base address length in number of bytes (3 bits)
        bit_field_t<16, 0x7>::value<X>();
    static constexpr uint32_t PCNF1_ENDIAN = 0x1000000;   // On air endianness of packet, this applies to the S0, LENGTH, S1 and the PAYLOAD fields.
    static constexpr uint32_t PCNF1_WHITEEN = 0x2000000;  // Enable or disable packet whitening





    template<uint32_t X>
    static constexpr uint32_t PREFIX0_AP0 =                 // Address prefix 0. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PREFIX0_AP1 =                 // Address prefix 1. (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PREFIX0_AP2 =                 // Address prefix 2. (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PREFIX0_AP3 =                 // Address prefix 3. (8 bits)
        bit_field_t<24, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t PREFIX1_AP4 =                 // Address prefix 4. (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PREFIX1_AP5 =                 // Address prefix 5. (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PREFIX1_AP6 =                 // Address prefix 6. (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PREFIX1_AP7 =                 // Address prefix 7. (8 bits)
        bit_field_t<24, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t TXADDRESS_TXADDRESS =           // Transmit address select (3 bits)
        bit_field_t<0, 0x7>::value<X>();

    static constexpr uint32_t RXADDRESSES_ADDR0 = 0x1;          // Enable or disable reception on logical address 0.
    static constexpr uint32_t RXADDRESSES_ADDR1 = 0x2;          // Enable or disable reception on logical address 1.
    static constexpr uint32_t RXADDRESSES_ADDR2 = 0x4;          // Enable or disable reception on logical address 2.
    static constexpr uint32_t RXADDRESSES_ADDR3 = 0x8;          // Enable or disable reception on logical address 3.
    static constexpr uint32_t RXADDRESSES_ADDR4 = 0x10;         // Enable or disable reception on logical address 4.
    static constexpr uint32_t RXADDRESSES_ADDR5 = 0x20;         // Enable or disable reception on logical address 5.
    static constexpr uint32_t RXADDRESSES_ADDR6 = 0x40;         // Enable or disable reception on logical address 6.
    static constexpr uint32_t RXADDRESSES_ADDR7 = 0x80;         // Enable or disable reception on logical address 7.

    template<uint32_t X>
    static constexpr uint32_t CRCCNF_LEN =                 // CRC length in number of bytes. (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static constexpr uint32_t CRCCNF_SKIPADDR = 0x100;     // Include or exclude packet address field out of CRC calculation.

    template<uint32_t X>
    static constexpr uint32_t CRCPOLY_CRCPOLY =             // CRC polynomial (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();
    static const uint32_t CRCPOLY_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t CRCINIT_CRCINIT =             // CRC initial value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();


    template<uint32_t X>
    static constexpr uint32_t TIFS_TIFS =                // Inter Frame Spacing in us (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t RSSISAMPLE_RSSISAMPLE =          // RSSI sample (7 bits)
        bit_field_t<0, 0x7f>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t STATE_STATE =               // Current radio state (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t DATAWHITEIV_DATAWHITEIV =         // Data whitening initial value. Bit 6 is hard-wired to '1', writing '0' to it has no effect, and it will always be read back and used by the device as '1'. (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t DATAWHITEIV_RESET_VALUE = 0x40;





    template<uint32_t X>
    static constexpr uint32_t DAP[%s]_DAP =                 // Device address prefix 0 (16 bits)
        bit_field_t<0, 0xffff>::value<X>();

    static constexpr uint32_t DACNF_ENA0 = 0x1;           // Enable or disable device address matching using device address 0
    static constexpr uint32_t DACNF_ENA1 = 0x2;           // Enable or disable device address matching using device address 1
    static constexpr uint32_t DACNF_ENA2 = 0x4;           // Enable or disable device address matching using device address 2
    static constexpr uint32_t DACNF_ENA3 = 0x8;           // Enable or disable device address matching using device address 3
    static constexpr uint32_t DACNF_ENA4 = 0x10;          // Enable or disable device address matching using device address 4
    static constexpr uint32_t DACNF_ENA5 = 0x20;          // Enable or disable device address matching using device address 5
    static constexpr uint32_t DACNF_ENA6 = 0x40;          // Enable or disable device address matching using device address 6
    static constexpr uint32_t DACNF_ENA7 = 0x80;          // Enable or disable device address matching using device address 7
    static constexpr uint32_t DACNF_TXADD0 = 0x100;       // TxAdd for device address 0
    static constexpr uint32_t DACNF_TXADD1 = 0x200;       // TxAdd for device address 1
    static constexpr uint32_t DACNF_TXADD2 = 0x400;       // TxAdd for device address 2
    static constexpr uint32_t DACNF_TXADD3 = 0x800;       // TxAdd for device address 3
    static constexpr uint32_t DACNF_TXADD4 = 0x1000;      // TxAdd for device address 4
    static constexpr uint32_t DACNF_TXADD5 = 0x2000;      // TxAdd for device address 5
    static constexpr uint32_t DACNF_TXADD6 = 0x4000;      // TxAdd for device address 6
    static constexpr uint32_t DACNF_TXADD7 = 0x8000;      // TxAdd for device address 7

    static constexpr uint32_t MODECNF0_RU = 0x1;             // Radio ramp-up time
    template<uint32_t X>
    static constexpr uint32_t MODECNF0_DTX =                 // Default TX value (2 bits)
        bit_field_t<8, 0x3>::value<X>();
    static const uint32_t MODECNF0_RESET_VALUE = 0x200;

    static constexpr uint32_t POWER_POWER = 0x1;          // Peripheral power control. The peripheral and its registers will be reset to its initial state by switching the peripheral off and then back on again.
    static const uint32_t POWER_RESET_VALUE = 0x1;

    static constexpr uint8_t RADIO = 1; // 
};

static radio_t& RADIO = *reinterpret_cast<radio_t*>(0x40001000);

#define HAVE_PERIPHERAL_RADIO


////
//
//    UART with EasyDMA
//
////

struct uarte0_t
{
    volatile uint32_t    TASKS_STARTRX;        // [Write-only] Start UART receiver
    volatile uint32_t    TASKS_STOPRX;         // [Write-only] Stop UART receiver
    volatile uint32_t    TASKS_STARTTX;        // [Write-only] Start UART transmitter
    volatile uint32_t    TASKS_STOPTX;         // [Write-only] Stop UART transmitter
    reserved_t<7>        _0;
    volatile uint32_t    TASKS_FLUSHRX;        // [Write-only] Flush RX FIFO into RX buffer
    reserved_t<52>       _1;
    volatile uint32_t    EVENTS_CTS;           // [Read-write] CTS is activated (set low). Clear To Send.
    volatile uint32_t    EVENTS_NCTS;          // [Read-write] CTS is deactivated (set high). Not Clear To Send.
    reserved_t<2>        _2;
    volatile uint32_t    EVENTS_ENDRX;         // [Read-write] Receive buffer is filled up
    reserved_t<3>        _3;
    volatile uint32_t    EVENTS_ENDTX;         // [Read-write] Last TX byte transmitted
    volatile uint32_t    EVENTS_ERROR;         // [Read-write] Error detected
    reserved_t<7>        _4;
    volatile uint32_t    EVENTS_RXTO;          // [Read-write] Receiver timeout
    reserved_t<1>        _5;
    volatile uint32_t    EVENTS_RXSTARTED;     // [Read-write] UART receiver has started
    volatile uint32_t    EVENTS_TXSTARTED;     // [Read-write] UART transmitter has started
    reserved_t<1>        _6;
    volatile uint32_t    EVENTS_TXSTOPPED;     // [Read-write] Transmitter stopped
    reserved_t<41>       _7;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<63>       _8;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<93>       _9;
    volatile uint32_t    ERRORSRC;             // [Read-write] Error source
    reserved_t<31>       _10;
    volatile uint32_t    ENABLE;               // [Read-write] Enable UART
    reserved_t<8>        _11;
    volatile uint32_t    BAUDRATE;             // [Read-write] Baud rate
    reserved_t<17>       _12;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration of parity and hardware flow control















    static constexpr uint32_t SHORTS_ENDRX_STARTRX = 0x20; // Shortcut between EVENTS_ENDRX event and TASKS_STARTRX task
    static constexpr uint32_t SHORTS_ENDRX_STOPRX = 0x40;  // Shortcut between EVENTS_ENDRX event and TASKS_STOPRX task

    static constexpr uint32_t INTEN_CTS = 0x1;            // Enable or disable interrupt on EVENTS_CTS event
    static constexpr uint32_t INTEN_NCTS = 0x2;           // Enable or disable interrupt on EVENTS_NCTS event
    static constexpr uint32_t INTEN_ENDRX = 0x10;         // Enable or disable interrupt on EVENTS_ENDRX event
    static constexpr uint32_t INTEN_ENDTX = 0x100;        // Enable or disable interrupt on EVENTS_ENDTX event
    static constexpr uint32_t INTEN_ERROR = 0x200;        // Enable or disable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTEN_RXTO = 0x20000;       // Enable or disable interrupt on EVENTS_RXTO event
    static constexpr uint32_t INTEN_RXSTARTED = 0x80000;  // Enable or disable interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTEN_TXSTARTED = 0x100000; // Enable or disable interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTEN_TXSTOPPED = 0x400000; // Enable or disable interrupt on EVENTS_TXSTOPPED event

    static constexpr uint32_t INTENSET_CTS = 0x1;            // Write '1' to Enable interrupt on EVENTS_CTS event
    static constexpr uint32_t INTENSET_NCTS = 0x2;           // Write '1' to Enable interrupt on EVENTS_NCTS event
    static constexpr uint32_t INTENSET_ENDRX = 0x10;         // Write '1' to Enable interrupt on EVENTS_ENDRX event
    static constexpr uint32_t INTENSET_ENDTX = 0x100;        // Write '1' to Enable interrupt on EVENTS_ENDTX event
    static constexpr uint32_t INTENSET_ERROR = 0x200;        // Write '1' to Enable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENSET_RXTO = 0x20000;       // Write '1' to Enable interrupt on EVENTS_RXTO event
    static constexpr uint32_t INTENSET_RXSTARTED = 0x80000;  // Write '1' to Enable interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTENSET_TXSTARTED = 0x100000; // Write '1' to Enable interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTENSET_TXSTOPPED = 0x400000; // Write '1' to Enable interrupt on EVENTS_TXSTOPPED event

    static constexpr uint32_t INTENCLR_CTS = 0x1;            // Write '1' to Clear interrupt on EVENTS_CTS event
    static constexpr uint32_t INTENCLR_NCTS = 0x2;           // Write '1' to Clear interrupt on EVENTS_NCTS event
    static constexpr uint32_t INTENCLR_ENDRX = 0x10;         // Write '1' to Clear interrupt on EVENTS_ENDRX event
    static constexpr uint32_t INTENCLR_ENDTX = 0x100;        // Write '1' to Clear interrupt on EVENTS_ENDTX event
    static constexpr uint32_t INTENCLR_ERROR = 0x200;        // Write '1' to Clear interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENCLR_RXTO = 0x20000;       // Write '1' to Clear interrupt on EVENTS_RXTO event
    static constexpr uint32_t INTENCLR_RXSTARTED = 0x80000;  // Write '1' to Clear interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTENCLR_TXSTARTED = 0x100000; // Write '1' to Clear interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTENCLR_TXSTOPPED = 0x400000; // Write '1' to Clear interrupt on EVENTS_TXSTOPPED event

    static constexpr uint32_t ERRORSRC_OVERRUN = 0x1;        // Overrun error
    static constexpr uint32_t ERRORSRC_PARITY = 0x2;         // Parity error
    static constexpr uint32_t ERRORSRC_FRAMING = 0x4;        // Framing error occurred
    static constexpr uint32_t ERRORSRC_BREAK = 0x8;          // Break condition

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable UARTE (4 bits)
        bit_field_t<0, 0xf>::value<X>();


    static const uint32_t BAUDRATE_RESET_VALUE = 0x4000000;

    static constexpr uint32_t CONFIG_HWFC = 0x1;           // Hardware flow control
    template<uint32_t X>
    static constexpr uint32_t CONFIG_PARITY =              // Parity (3 bits)
        bit_field_t<1, 0x7>::value<X>();

    static constexpr uint8_t UARTE0_UART0 = 2; // 
};

static uarte0_t& UARTE0 = *reinterpret_cast<uarte0_t*>(0x40002000);

#define HAVE_PERIPHERAL_UARTE0


////
//
//    Serial Peripheral Interface Master with EasyDMA 0
//
////

struct spim0_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start SPI transaction
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop SPI transaction
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_SUSPEND;        // [Write-only] Suspend SPI transaction
    volatile uint32_t    TASKS_RESUME;         // [Write-only] Resume SPI transaction
    reserved_t<56>       _1;
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] SPI transaction has stopped
    reserved_t<2>        _2;
    volatile uint32_t    EVENTS_ENDRX;         // [Read-write] End of RXD buffer reached
    reserved_t<1>        _3;
    volatile uint32_t    EVENTS_END;           // [Read-write] End of RXD buffer and TXD buffer reached
    reserved_t<1>        _4;
    volatile uint32_t    EVENTS_ENDTX;         // [Read-write] End of TXD buffer reached
    reserved_t<10>       _5;
    volatile uint32_t    EVENTS_STARTED;       // [Read-write] Transaction started
    reserved_t<44>       _6;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _7;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<125>      _8;
    volatile uint32_t    ENABLE;               // [Read-write] Enable SPIM
    reserved_t<8>        _9;
    volatile uint32_t    FREQUENCY;            // [Read-write] SPI frequency
    reserved_t<11>       _10;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register
    reserved_t<26>       _11;
    volatile uint32_t    ORC;                  // [Read-write] Over-read character. Character clocked out in case and over-read of the TXD buffer.










    static constexpr uint32_t SHORTS_END_START = 0x20000;  // Shortcut between EVENTS_END event and TASKS_START task

    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_ENDRX = 0x10;         // Write '1' to Enable interrupt on EVENTS_ENDRX event
    static constexpr uint32_t INTENSET_END = 0x40;           // Write '1' to Enable interrupt on EVENTS_END event
    static constexpr uint32_t INTENSET_ENDTX = 0x100;        // Write '1' to Enable interrupt on EVENTS_ENDTX event
    static constexpr uint32_t INTENSET_STARTED = 0x80000;    // Write '1' to Enable interrupt on EVENTS_STARTED event

    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_ENDRX = 0x10;         // Write '1' to Clear interrupt on EVENTS_ENDRX event
    static constexpr uint32_t INTENCLR_END = 0x40;           // Write '1' to Clear interrupt on EVENTS_END event
    static constexpr uint32_t INTENCLR_ENDTX = 0x100;        // Write '1' to Clear interrupt on EVENTS_ENDTX event
    static constexpr uint32_t INTENCLR_STARTED = 0x80000;    // Write '1' to Clear interrupt on EVENTS_STARTED event

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable SPIM (4 bits)
        bit_field_t<0, 0xf>::value<X>();


    static const uint32_t FREQUENCY_RESET_VALUE = 0x4000000;

    static constexpr uint32_t CONFIG_ORDER = 0x1;          // Bit order
    static constexpr uint32_t CONFIG_CPHA = 0x2;           // Serial clock (SCK) phase
    static constexpr uint32_t CONFIG_CPOL = 0x4;           // Serial clock (SCK) polarity

    template<uint32_t X>
    static constexpr uint32_t ORC_ORC =                 // Over-read character. Character clocked out in case and over-read of the TXD buffer. (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    static constexpr uint8_t SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0 = 3; // 
};

static spim0_t& SPIM0 = *reinterpret_cast<spim0_t*>(0x40003000);

#define HAVE_PERIPHERAL_SPIM0


////
//
//    SPI Slave 0
//
////

struct spis0_t
{
    volatile uint32_t    TASKS_ACQUIRE;        // [Write-only] Acquire SPI semaphore
    volatile uint32_t    TASKS_RELEASE;        // [Write-only] Release SPI semaphore, enabling the SPI slave to acquire it
    reserved_t<54>       _0;
    volatile uint32_t    EVENTS_END;           // [Read-write] Granted transaction completed
    reserved_t<8>        _1;
    volatile uint32_t    EVENTS_ACQUIRED;      // [Read-write] Semaphore acquired
    reserved_t<53>       _2;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _3;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<61>       _4;
    volatile uint32_t    SEMSTAT;              // [Read-only] Semaphore status register
    reserved_t<15>       _5;
    volatile uint32_t    STATUS;               // [Read-write] Status from last transaction
    reserved_t<47>       _6;
    volatile uint32_t    ENABLE;               // [Read-write] Enable SPI slave
    reserved_t<20>       _7;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register
    reserved_t<1>        _8;
    volatile uint32_t    DEF;                  // [Read-write] Default character. Character clocked out in case of an ignored transaction.
    reserved_t<24>       _9;
    volatile uint32_t    ORC;                  // [Read-write] Over-read character





    static constexpr uint32_t SHORTS_END_ACQUIRE = 0x4;    // Shortcut between EVENTS_END event and TASKS_ACQUIRE task

    static constexpr uint32_t INTENSET_END = 0x2;            // Write '1' to Enable interrupt on EVENTS_END event
    static constexpr uint32_t INTENSET_ACQUIRED = 0x400;     // Write '1' to Enable interrupt on EVENTS_ACQUIRED event

    static constexpr uint32_t INTENCLR_END = 0x2;            // Write '1' to Clear interrupt on EVENTS_END event
    static constexpr uint32_t INTENCLR_ACQUIRED = 0x400;     // Write '1' to Clear interrupt on EVENTS_ACQUIRED event

    template<uint32_t X>
    static constexpr uint32_t SEMSTAT_SEMSTAT =             // Semaphore status (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t SEMSTAT_RESET_VALUE = 0x1;

    static constexpr uint32_t STATUS_OVERREAD = 0x1;       // TX buffer over-read detected, and prevented
    static constexpr uint32_t STATUS_OVERFLOW = 0x2;       // RX buffer overflow detected, and prevented

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable SPI slave (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    static constexpr uint32_t CONFIG_ORDER = 0x1;          // Bit order
    static constexpr uint32_t CONFIG_CPHA = 0x2;           // Serial clock (SCK) phase
    static constexpr uint32_t CONFIG_CPOL = 0x4;           // Serial clock (SCK) polarity

    template<uint32_t X>
    static constexpr uint32_t DEF_DEF =                 // Default character. Character clocked out in case of an ignored transaction. (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t ORC_ORC =                 // Over-read character. Character clocked out after an over-read of the transmit buffer. (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    static constexpr uint8_t SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0 = 3; // 
};

static spis0_t& SPIS0 = *reinterpret_cast<spis0_t*>(0x40003000);

#define HAVE_PERIPHERAL_SPIS0


////
//
//    I2C compatible Two-Wire Master Interface with EasyDMA 0
//
////

struct twim0_t
{
    volatile uint32_t    TASKS_STARTRX;        // [Write-only] Start TWI receive sequence
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_STARTTX;        // [Write-only] Start TWI transmit sequence
    reserved_t<2>        _1;
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop TWI transaction
    reserved_t<1>        _2;
    volatile uint32_t    TASKS_SUSPEND;        // [Write-only] Suspend TWI transaction
    volatile uint32_t    TASKS_RESUME;         // [Write-only] Resume TWI transaction
    reserved_t<56>       _3;
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] TWI stopped
    reserved_t<7>        _4;
    volatile uint32_t    EVENTS_ERROR;         // [Read-write] TWI error
    reserved_t<9>        _5;
    volatile uint32_t    EVENTS_RXSTARTED;     // [Read-write] Receive sequence started
    volatile uint32_t    EVENTS_TXSTARTED;     // [Read-write] Transmit sequence started
    reserved_t<2>        _6;
    volatile uint32_t    EVENTS_LASTRX;        // [Read-write] Byte boundary, starting to receive the last byte
    volatile uint32_t    EVENTS_LASTTX;        // [Read-write] Byte boundary, starting to transmit the last byte
    reserved_t<39>       _7;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<63>       _8;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<110>      _9;
    volatile uint32_t    ERRORSRC;             // [Read-write] Error source
    reserved_t<14>       _10;
    volatile uint32_t    ENABLE;               // [Read-write] Enable TWIM
    reserved_t<8>        _11;
    volatile uint32_t    FREQUENCY;            // [Read-write] TWI frequency
    reserved_t<24>       _12;
    volatile uint32_t    ADDRESS;              // [Read-write] Address used in the TWI transfer












    static constexpr uint32_t SHORTS_LASTTX_STARTRX = 0x80;// Shortcut between EVENTS_LASTTX event and TASKS_STARTRX task
    static constexpr uint32_t SHORTS_LASTTX_SUSPEND = 0x100;// Shortcut between EVENTS_LASTTX event and TASKS_SUSPEND task
    static constexpr uint32_t SHORTS_LASTTX_STOP = 0x200;  // Shortcut between EVENTS_LASTTX event and TASKS_STOP task
    static constexpr uint32_t SHORTS_LASTRX_STARTTX = 0x400;// Shortcut between EVENTS_LASTRX event and TASKS_STARTTX task
    static constexpr uint32_t SHORTS_LASTRX_STOP = 0x1000; // Shortcut between EVENTS_LASTRX event and TASKS_STOP task

    static constexpr uint32_t INTEN_STOPPED = 0x2;        // Enable or disable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTEN_ERROR = 0x200;        // Enable or disable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTEN_RXSTARTED = 0x80000;  // Enable or disable interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTEN_TXSTARTED = 0x100000; // Enable or disable interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTEN_LASTRX = 0x800000;    // Enable or disable interrupt on EVENTS_LASTRX event
    static constexpr uint32_t INTEN_LASTTX = 0x1000000;   // Enable or disable interrupt on EVENTS_LASTTX event

    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_ERROR = 0x200;        // Write '1' to Enable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENSET_RXSTARTED = 0x80000;  // Write '1' to Enable interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTENSET_TXSTARTED = 0x100000; // Write '1' to Enable interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTENSET_LASTRX = 0x800000;    // Write '1' to Enable interrupt on EVENTS_LASTRX event
    static constexpr uint32_t INTENSET_LASTTX = 0x1000000;   // Write '1' to Enable interrupt on EVENTS_LASTTX event

    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_ERROR = 0x200;        // Write '1' to Clear interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENCLR_RXSTARTED = 0x80000;  // Write '1' to Clear interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTENCLR_TXSTARTED = 0x100000; // Write '1' to Clear interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTENCLR_LASTRX = 0x800000;    // Write '1' to Clear interrupt on EVENTS_LASTRX event
    static constexpr uint32_t INTENCLR_LASTTX = 0x1000000;   // Write '1' to Clear interrupt on EVENTS_LASTTX event

    static constexpr uint32_t ERRORSRC_ANACK = 0x2;          // NACK received after sending the address (write '1' to clear)
    static constexpr uint32_t ERRORSRC_DNACK = 0x4;          // NACK received after sending a data byte (write '1' to clear)

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable TWIM (4 bits)
        bit_field_t<0, 0xf>::value<X>();


    static const uint32_t FREQUENCY_RESET_VALUE = 0x4000000;

    template<uint32_t X>
    static constexpr uint32_t ADDRESS_ADDRESS =             // Address used in the TWI transfer (7 bits)
        bit_field_t<0, 0x7f>::value<X>();

    static constexpr uint8_t SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0 = 3; // 
};

static twim0_t& TWIM0 = *reinterpret_cast<twim0_t*>(0x40003000);

#define HAVE_PERIPHERAL_TWIM0


////
//
//    I2C compatible Two-Wire Slave Interface with EasyDMA 0
//
////

struct twis0_t
{
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop TWI transaction
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_SUSPEND;        // [Write-only] Suspend TWI transaction
    volatile uint32_t    TASKS_RESUME;         // [Write-only] Resume TWI transaction
    reserved_t<3>        _1;
    volatile uint32_t    TASKS_PREPARERX;      // [Write-only] Prepare the TWI slave to respond to a write command
    volatile uint32_t    TASKS_PREPARETX;      // [Write-only] Prepare the TWI slave to respond to a read command
    reserved_t<51>       _2;
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] TWI stopped
    reserved_t<7>        _3;
    volatile uint32_t    EVENTS_ERROR;         // [Read-write] TWI error
    reserved_t<9>        _4;
    volatile uint32_t    EVENTS_RXSTARTED;     // [Read-write] Receive sequence started
    volatile uint32_t    EVENTS_TXSTARTED;     // [Read-write] Transmit sequence started
    reserved_t<4>        _5;
    volatile uint32_t    EVENTS_WRITE;         // [Read-write] Write command received
    volatile uint32_t    EVENTS_READ;          // [Read-write] Read command received
    reserved_t<37>       _6;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<63>       _7;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<113>      _8;
    volatile uint32_t    ERRORSRC;             // [Read-write] Error source
    volatile uint32_t    MATCH;                // [Read-only] Status register indicating which address had a match
    reserved_t<10>       _9;
    volatile uint32_t    ENABLE;               // [Read-write] Enable TWIS
    reserved_t<33>       _10;
    volatile uint32_t    ADDRESS[s];           // [Read-write] Description collection[0]: TWI slave address 0
    reserved_t<2>        _11;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register for the address match mechanism
    reserved_t<10>       _12;
    volatile uint32_t    ORC;                  // [Read-write] Over-read character. Character sent out in case of an over-read of the transmit buffer.












    static constexpr uint32_t SHORTS_WRITE_SUSPEND = 0x2000;// Shortcut between EVENTS_WRITE event and TASKS_SUSPEND task
    static constexpr uint32_t SHORTS_READ_SUSPEND = 0x4000;// Shortcut between EVENTS_READ event and TASKS_SUSPEND task

    static constexpr uint32_t INTEN_STOPPED = 0x2;        // Enable or disable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTEN_ERROR = 0x200;        // Enable or disable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTEN_RXSTARTED = 0x80000;  // Enable or disable interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTEN_TXSTARTED = 0x100000; // Enable or disable interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTEN_WRITE = 0x2000000;    // Enable or disable interrupt on EVENTS_WRITE event
    static constexpr uint32_t INTEN_READ = 0x4000000;     // Enable or disable interrupt on EVENTS_READ event

    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_ERROR = 0x200;        // Write '1' to Enable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENSET_RXSTARTED = 0x80000;  // Write '1' to Enable interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTENSET_TXSTARTED = 0x100000; // Write '1' to Enable interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTENSET_WRITE = 0x2000000;    // Write '1' to Enable interrupt on EVENTS_WRITE event
    static constexpr uint32_t INTENSET_READ = 0x4000000;     // Write '1' to Enable interrupt on EVENTS_READ event

    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_ERROR = 0x200;        // Write '1' to Clear interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENCLR_RXSTARTED = 0x80000;  // Write '1' to Clear interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTENCLR_TXSTARTED = 0x100000; // Write '1' to Clear interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTENCLR_WRITE = 0x2000000;    // Write '1' to Clear interrupt on EVENTS_WRITE event
    static constexpr uint32_t INTENCLR_READ = 0x4000000;     // Write '1' to Clear interrupt on EVENTS_READ event

    static constexpr uint32_t ERRORSRC_OVERFLOW = 0x1;       // RX buffer overflow detected, and prevented
    static constexpr uint32_t ERRORSRC_DNACK = 0x4;          // NACK sent after receiving a data byte
    static constexpr uint32_t ERRORSRC_OVERREAD = 0x8;       // TX buffer over-read detected, and prevented

    static constexpr uint32_t MATCH_MATCH = 0x1;          // Which of the addresses in {ADDRESS} matched the incoming address

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable TWIS (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t ADDRESS[%s]_ADDRESS =             // TWI slave address (7 bits)
        bit_field_t<0, 0x7f>::value<X>();

    static constexpr uint32_t CONFIG_ADDRESS0 = 0x1;       // Enable or disable address matching on ADDRESS[0]
    static constexpr uint32_t CONFIG_ADDRESS1 = 0x2;       // Enable or disable address matching on ADDRESS[1]
    static const uint32_t CONFIG_RESET_VALUE = 0x1;

    template<uint32_t X>
    static constexpr uint32_t ORC_ORC =                 // Over-read character. Character sent out in case of an over-read of the transmit buffer. (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    static constexpr uint8_t SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0 = 3; // 
};

static twis0_t& TWIS0 = *reinterpret_cast<twis0_t*>(0x40003000);

#define HAVE_PERIPHERAL_TWIS0


////
//
//    Serial Peripheral Interface 0
//
////

struct spi0_t
{
    volatile uint32_t    EVENTS_READY;         // [Read-write] TXD byte sent and RXD byte received
    reserved_t<126>      _0;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<125>      _1;
    volatile uint32_t    ENABLE;               // [Read-write] Enable SPI
    reserved_t<5>        _2;
    volatile uint32_t    RXD;                  // [Read-only] RXD register
    volatile uint32_t    TXD;                  // [Read-write] TXD register
    reserved_t<1>        _3;
    volatile uint32_t    FREQUENCY;            // [Read-write] SPI frequency
    reserved_t<11>       _4;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register


    static constexpr uint32_t INTENSET_READY = 0x4;          // Write '1' to Enable interrupt on EVENTS_READY event

    static constexpr uint32_t INTENCLR_READY = 0x4;          // Write '1' to Clear interrupt on EVENTS_READY event

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable SPI (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t RXD_RXD =                 // RX data received. Double buffered (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t TXD_TXD =                 // TX data to send. Double buffered (8 bits)
        bit_field_t<0, 0xff>::value<X>();


    static const uint32_t FREQUENCY_RESET_VALUE = 0x4000000;

    static constexpr uint32_t CONFIG_ORDER = 0x1;          // Bit order
    static constexpr uint32_t CONFIG_CPHA = 0x2;           // Serial clock (SCK) phase
    static constexpr uint32_t CONFIG_CPOL = 0x4;           // Serial clock (SCK) polarity

    static constexpr uint8_t SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0 = 3; // 
};

static spi0_t& SPI0 = *reinterpret_cast<spi0_t*>(0x40003000);

#define HAVE_PERIPHERAL_SPI0


////
//
//    I2C compatible Two-Wire Interface 0
//
////

struct twi0_t
{
    volatile uint32_t    TASKS_STARTRX;        // [Write-only] Start TWI receive sequence
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_STARTTX;        // [Write-only] Start TWI transmit sequence
    reserved_t<2>        _1;
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop TWI transaction
    reserved_t<1>        _2;
    volatile uint32_t    TASKS_SUSPEND;        // [Write-only] Suspend TWI transaction
    volatile uint32_t    TASKS_RESUME;         // [Write-only] Resume TWI transaction
    reserved_t<56>       _3;
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] TWI stopped
    volatile uint32_t    EVENTS_RXDREADY;      // [Read-write] TWI RXD byte received
    reserved_t<4>        _4;
    volatile uint32_t    EVENTS_TXDSENT;       // [Read-write] TWI TXD byte sent
    reserved_t<1>        _5;
    volatile uint32_t    EVENTS_ERROR;         // [Read-write] TWI error
    reserved_t<4>        _6;
    volatile uint32_t    EVENTS_BB;            // [Read-write] TWI byte boundary, generated before each byte that is sent or received
    reserved_t<3>        _7;
    volatile uint32_t    EVENTS_SUSPENDED;     // [Read-write] TWI entered the suspended state
    reserved_t<45>       _8;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _9;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<110>      _10;
    volatile uint32_t    ERRORSRC;             // [Read-write] Error source
    reserved_t<14>       _11;
    volatile uint32_t    ENABLE;               // [Read-write] Enable TWI
    reserved_t<1>        _12;
    volatile uint32_t    PSELSCL;              // [Read-write] Pin select for SCL
    volatile uint32_t    PSELSDA;              // [Read-write] Pin select for SDA
    reserved_t<2>        _13;
    volatile uint32_t    RXD;                  // [Read-only] RXD register
    volatile uint32_t    TXD;                  // [Read-write] TXD register
    reserved_t<1>        _14;
    volatile uint32_t    FREQUENCY;            // [Read-write] TWI frequency
    reserved_t<24>       _15;
    volatile uint32_t    ADDRESS;              // [Read-write] Address used in the TWI transfer












    static constexpr uint32_t SHORTS_BB_SUSPEND = 0x1;     // Shortcut between EVENTS_BB event and TASKS_SUSPEND task
    static constexpr uint32_t SHORTS_BB_STOP = 0x2;        // Shortcut between EVENTS_BB event and TASKS_STOP task

    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_RXDREADY = 0x4;       // Write '1' to Enable interrupt on EVENTS_RXDREADY event
    static constexpr uint32_t INTENSET_TXDSENT = 0x80;       // Write '1' to Enable interrupt on EVENTS_TXDSENT event
    static constexpr uint32_t INTENSET_ERROR = 0x200;        // Write '1' to Enable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENSET_BB = 0x4000;          // Write '1' to Enable interrupt on EVENTS_BB event
    static constexpr uint32_t INTENSET_SUSPENDED = 0x40000;  // Write '1' to Enable interrupt on EVENTS_SUSPENDED event

    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_RXDREADY = 0x4;       // Write '1' to Clear interrupt on EVENTS_RXDREADY event
    static constexpr uint32_t INTENCLR_TXDSENT = 0x80;       // Write '1' to Clear interrupt on EVENTS_TXDSENT event
    static constexpr uint32_t INTENCLR_ERROR = 0x200;        // Write '1' to Clear interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENCLR_BB = 0x4000;          // Write '1' to Clear interrupt on EVENTS_BB event
    static constexpr uint32_t INTENCLR_SUSPENDED = 0x40000;  // Write '1' to Clear interrupt on EVENTS_SUSPENDED event

    static constexpr uint32_t ERRORSRC_OVERRUN = 0x1;        // Overrun error
    static constexpr uint32_t ERRORSRC_ANACK = 0x2;          // NACK received after sending the address (write '1' to clear)
    static constexpr uint32_t ERRORSRC_DNACK = 0x4;          // NACK received after sending a data byte (write '1' to clear)

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable TWI (4 bits)
        bit_field_t<0, 0xf>::value<X>();


    static const uint32_t PSELSCL_RESET_VALUE = 0xffffffff;


    static const uint32_t PSELSDA_RESET_VALUE = 0xffffffff;

    template<uint32_t X>
    static constexpr uint32_t RXD_RXD =                 // RXD register (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t TXD_TXD =                 // TXD register (8 bits)
        bit_field_t<0, 0xff>::value<X>();


    static const uint32_t FREQUENCY_RESET_VALUE = 0x4000000;

    template<uint32_t X>
    static constexpr uint32_t ADDRESS_ADDRESS =             // Address used in the TWI transfer (7 bits)
        bit_field_t<0, 0x7f>::value<X>();

    static constexpr uint8_t SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0 = 3; // 
};

static twi0_t& TWI0 = *reinterpret_cast<twi0_t*>(0x40003000);

#define HAVE_PERIPHERAL_TWI0


////
//
//    Serial Peripheral Interface Master with EasyDMA 0
//
////

struct spim1_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start SPI transaction
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop SPI transaction
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_SUSPEND;        // [Write-only] Suspend SPI transaction
    volatile uint32_t    TASKS_RESUME;         // [Write-only] Resume SPI transaction
    reserved_t<56>       _1;
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] SPI transaction has stopped
    reserved_t<2>        _2;
    volatile uint32_t    EVENTS_ENDRX;         // [Read-write] End of RXD buffer reached
    reserved_t<1>        _3;
    volatile uint32_t    EVENTS_END;           // [Read-write] End of RXD buffer and TXD buffer reached
    reserved_t<1>        _4;
    volatile uint32_t    EVENTS_ENDTX;         // [Read-write] End of TXD buffer reached
    reserved_t<10>       _5;
    volatile uint32_t    EVENTS_STARTED;       // [Read-write] Transaction started
    reserved_t<44>       _6;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _7;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<125>      _8;
    volatile uint32_t    ENABLE;               // [Read-write] Enable SPIM
    reserved_t<8>        _9;
    volatile uint32_t    FREQUENCY;            // [Read-write] SPI frequency
    reserved_t<11>       _10;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register
    reserved_t<26>       _11;
    volatile uint32_t    ORC;                  // [Read-write] Over-read character. Character clocked out in case and over-read of the TXD buffer.










    static constexpr uint32_t SHORTS_END_START = 0x20000;  // Shortcut between EVENTS_END event and TASKS_START task

    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_ENDRX = 0x10;         // Write '1' to Enable interrupt on EVENTS_ENDRX event
    static constexpr uint32_t INTENSET_END = 0x40;           // Write '1' to Enable interrupt on EVENTS_END event
    static constexpr uint32_t INTENSET_ENDTX = 0x100;        // Write '1' to Enable interrupt on EVENTS_ENDTX event
    static constexpr uint32_t INTENSET_STARTED = 0x80000;    // Write '1' to Enable interrupt on EVENTS_STARTED event

    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_ENDRX = 0x10;         // Write '1' to Clear interrupt on EVENTS_ENDRX event
    static constexpr uint32_t INTENCLR_END = 0x40;           // Write '1' to Clear interrupt on EVENTS_END event
    static constexpr uint32_t INTENCLR_ENDTX = 0x100;        // Write '1' to Clear interrupt on EVENTS_ENDTX event
    static constexpr uint32_t INTENCLR_STARTED = 0x80000;    // Write '1' to Clear interrupt on EVENTS_STARTED event

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable SPIM (4 bits)
        bit_field_t<0, 0xf>::value<X>();


    static const uint32_t FREQUENCY_RESET_VALUE = 0x4000000;

    static constexpr uint32_t CONFIG_ORDER = 0x1;          // Bit order
    static constexpr uint32_t CONFIG_CPHA = 0x2;           // Serial clock (SCK) phase
    static constexpr uint32_t CONFIG_CPOL = 0x4;           // Serial clock (SCK) polarity

    template<uint32_t X>
    static constexpr uint32_t ORC_ORC =                 // Over-read character. Character clocked out in case and over-read of the TXD buffer. (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    static constexpr uint8_t SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1 = 4; // 
};

static spim1_t& SPIM1 = *reinterpret_cast<spim1_t*>(0x40004000);

#define HAVE_PERIPHERAL_SPIM1


////
//
//    SPI Slave 0
//
////

struct spis1_t
{
    volatile uint32_t    TASKS_ACQUIRE;        // [Write-only] Acquire SPI semaphore
    volatile uint32_t    TASKS_RELEASE;        // [Write-only] Release SPI semaphore, enabling the SPI slave to acquire it
    reserved_t<54>       _0;
    volatile uint32_t    EVENTS_END;           // [Read-write] Granted transaction completed
    reserved_t<8>        _1;
    volatile uint32_t    EVENTS_ACQUIRED;      // [Read-write] Semaphore acquired
    reserved_t<53>       _2;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _3;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<61>       _4;
    volatile uint32_t    SEMSTAT;              // [Read-only] Semaphore status register
    reserved_t<15>       _5;
    volatile uint32_t    STATUS;               // [Read-write] Status from last transaction
    reserved_t<47>       _6;
    volatile uint32_t    ENABLE;               // [Read-write] Enable SPI slave
    reserved_t<20>       _7;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register
    reserved_t<1>        _8;
    volatile uint32_t    DEF;                  // [Read-write] Default character. Character clocked out in case of an ignored transaction.
    reserved_t<24>       _9;
    volatile uint32_t    ORC;                  // [Read-write] Over-read character





    static constexpr uint32_t SHORTS_END_ACQUIRE = 0x4;    // Shortcut between EVENTS_END event and TASKS_ACQUIRE task

    static constexpr uint32_t INTENSET_END = 0x2;            // Write '1' to Enable interrupt on EVENTS_END event
    static constexpr uint32_t INTENSET_ACQUIRED = 0x400;     // Write '1' to Enable interrupt on EVENTS_ACQUIRED event

    static constexpr uint32_t INTENCLR_END = 0x2;            // Write '1' to Clear interrupt on EVENTS_END event
    static constexpr uint32_t INTENCLR_ACQUIRED = 0x400;     // Write '1' to Clear interrupt on EVENTS_ACQUIRED event

    template<uint32_t X>
    static constexpr uint32_t SEMSTAT_SEMSTAT =             // Semaphore status (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t SEMSTAT_RESET_VALUE = 0x1;

    static constexpr uint32_t STATUS_OVERREAD = 0x1;       // TX buffer over-read detected, and prevented
    static constexpr uint32_t STATUS_OVERFLOW = 0x2;       // RX buffer overflow detected, and prevented

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable SPI slave (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    static constexpr uint32_t CONFIG_ORDER = 0x1;          // Bit order
    static constexpr uint32_t CONFIG_CPHA = 0x2;           // Serial clock (SCK) phase
    static constexpr uint32_t CONFIG_CPOL = 0x4;           // Serial clock (SCK) polarity

    template<uint32_t X>
    static constexpr uint32_t DEF_DEF =                 // Default character. Character clocked out in case of an ignored transaction. (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t ORC_ORC =                 // Over-read character. Character clocked out after an over-read of the transmit buffer. (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    static constexpr uint8_t SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1 = 4; // 
};

static spis1_t& SPIS1 = *reinterpret_cast<spis1_t*>(0x40004000);

#define HAVE_PERIPHERAL_SPIS1


////
//
//    I2C compatible Two-Wire Master Interface with EasyDMA 0
//
////

struct twim1_t
{
    volatile uint32_t    TASKS_STARTRX;        // [Write-only] Start TWI receive sequence
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_STARTTX;        // [Write-only] Start TWI transmit sequence
    reserved_t<2>        _1;
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop TWI transaction
    reserved_t<1>        _2;
    volatile uint32_t    TASKS_SUSPEND;        // [Write-only] Suspend TWI transaction
    volatile uint32_t    TASKS_RESUME;         // [Write-only] Resume TWI transaction
    reserved_t<56>       _3;
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] TWI stopped
    reserved_t<7>        _4;
    volatile uint32_t    EVENTS_ERROR;         // [Read-write] TWI error
    reserved_t<9>        _5;
    volatile uint32_t    EVENTS_RXSTARTED;     // [Read-write] Receive sequence started
    volatile uint32_t    EVENTS_TXSTARTED;     // [Read-write] Transmit sequence started
    reserved_t<2>        _6;
    volatile uint32_t    EVENTS_LASTRX;        // [Read-write] Byte boundary, starting to receive the last byte
    volatile uint32_t    EVENTS_LASTTX;        // [Read-write] Byte boundary, starting to transmit the last byte
    reserved_t<39>       _7;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<63>       _8;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<110>      _9;
    volatile uint32_t    ERRORSRC;             // [Read-write] Error source
    reserved_t<14>       _10;
    volatile uint32_t    ENABLE;               // [Read-write] Enable TWIM
    reserved_t<8>        _11;
    volatile uint32_t    FREQUENCY;            // [Read-write] TWI frequency
    reserved_t<24>       _12;
    volatile uint32_t    ADDRESS;              // [Read-write] Address used in the TWI transfer












    static constexpr uint32_t SHORTS_LASTTX_STARTRX = 0x80;// Shortcut between EVENTS_LASTTX event and TASKS_STARTRX task
    static constexpr uint32_t SHORTS_LASTTX_SUSPEND = 0x100;// Shortcut between EVENTS_LASTTX event and TASKS_SUSPEND task
    static constexpr uint32_t SHORTS_LASTTX_STOP = 0x200;  // Shortcut between EVENTS_LASTTX event and TASKS_STOP task
    static constexpr uint32_t SHORTS_LASTRX_STARTTX = 0x400;// Shortcut between EVENTS_LASTRX event and TASKS_STARTTX task
    static constexpr uint32_t SHORTS_LASTRX_STOP = 0x1000; // Shortcut between EVENTS_LASTRX event and TASKS_STOP task

    static constexpr uint32_t INTEN_STOPPED = 0x2;        // Enable or disable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTEN_ERROR = 0x200;        // Enable or disable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTEN_RXSTARTED = 0x80000;  // Enable or disable interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTEN_TXSTARTED = 0x100000; // Enable or disable interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTEN_LASTRX = 0x800000;    // Enable or disable interrupt on EVENTS_LASTRX event
    static constexpr uint32_t INTEN_LASTTX = 0x1000000;   // Enable or disable interrupt on EVENTS_LASTTX event

    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_ERROR = 0x200;        // Write '1' to Enable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENSET_RXSTARTED = 0x80000;  // Write '1' to Enable interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTENSET_TXSTARTED = 0x100000; // Write '1' to Enable interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTENSET_LASTRX = 0x800000;    // Write '1' to Enable interrupt on EVENTS_LASTRX event
    static constexpr uint32_t INTENSET_LASTTX = 0x1000000;   // Write '1' to Enable interrupt on EVENTS_LASTTX event

    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_ERROR = 0x200;        // Write '1' to Clear interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENCLR_RXSTARTED = 0x80000;  // Write '1' to Clear interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTENCLR_TXSTARTED = 0x100000; // Write '1' to Clear interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTENCLR_LASTRX = 0x800000;    // Write '1' to Clear interrupt on EVENTS_LASTRX event
    static constexpr uint32_t INTENCLR_LASTTX = 0x1000000;   // Write '1' to Clear interrupt on EVENTS_LASTTX event

    static constexpr uint32_t ERRORSRC_ANACK = 0x2;          // NACK received after sending the address (write '1' to clear)
    static constexpr uint32_t ERRORSRC_DNACK = 0x4;          // NACK received after sending a data byte (write '1' to clear)

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable TWIM (4 bits)
        bit_field_t<0, 0xf>::value<X>();


    static const uint32_t FREQUENCY_RESET_VALUE = 0x4000000;

    template<uint32_t X>
    static constexpr uint32_t ADDRESS_ADDRESS =             // Address used in the TWI transfer (7 bits)
        bit_field_t<0, 0x7f>::value<X>();

    static constexpr uint8_t SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1 = 4; // 
};

static twim1_t& TWIM1 = *reinterpret_cast<twim1_t*>(0x40004000);

#define HAVE_PERIPHERAL_TWIM1


////
//
//    I2C compatible Two-Wire Slave Interface with EasyDMA 0
//
////

struct twis1_t
{
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop TWI transaction
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_SUSPEND;        // [Write-only] Suspend TWI transaction
    volatile uint32_t    TASKS_RESUME;         // [Write-only] Resume TWI transaction
    reserved_t<3>        _1;
    volatile uint32_t    TASKS_PREPARERX;      // [Write-only] Prepare the TWI slave to respond to a write command
    volatile uint32_t    TASKS_PREPARETX;      // [Write-only] Prepare the TWI slave to respond to a read command
    reserved_t<51>       _2;
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] TWI stopped
    reserved_t<7>        _3;
    volatile uint32_t    EVENTS_ERROR;         // [Read-write] TWI error
    reserved_t<9>        _4;
    volatile uint32_t    EVENTS_RXSTARTED;     // [Read-write] Receive sequence started
    volatile uint32_t    EVENTS_TXSTARTED;     // [Read-write] Transmit sequence started
    reserved_t<4>        _5;
    volatile uint32_t    EVENTS_WRITE;         // [Read-write] Write command received
    volatile uint32_t    EVENTS_READ;          // [Read-write] Read command received
    reserved_t<37>       _6;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<63>       _7;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<113>      _8;
    volatile uint32_t    ERRORSRC;             // [Read-write] Error source
    volatile uint32_t    MATCH;                // [Read-only] Status register indicating which address had a match
    reserved_t<10>       _9;
    volatile uint32_t    ENABLE;               // [Read-write] Enable TWIS
    reserved_t<33>       _10;
    volatile uint32_t    ADDRESS[s];           // [Read-write] Description collection[0]: TWI slave address 0
    reserved_t<2>        _11;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register for the address match mechanism
    reserved_t<10>       _12;
    volatile uint32_t    ORC;                  // [Read-write] Over-read character. Character sent out in case of an over-read of the transmit buffer.












    static constexpr uint32_t SHORTS_WRITE_SUSPEND = 0x2000;// Shortcut between EVENTS_WRITE event and TASKS_SUSPEND task
    static constexpr uint32_t SHORTS_READ_SUSPEND = 0x4000;// Shortcut between EVENTS_READ event and TASKS_SUSPEND task

    static constexpr uint32_t INTEN_STOPPED = 0x2;        // Enable or disable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTEN_ERROR = 0x200;        // Enable or disable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTEN_RXSTARTED = 0x80000;  // Enable or disable interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTEN_TXSTARTED = 0x100000; // Enable or disable interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTEN_WRITE = 0x2000000;    // Enable or disable interrupt on EVENTS_WRITE event
    static constexpr uint32_t INTEN_READ = 0x4000000;     // Enable or disable interrupt on EVENTS_READ event

    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_ERROR = 0x200;        // Write '1' to Enable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENSET_RXSTARTED = 0x80000;  // Write '1' to Enable interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTENSET_TXSTARTED = 0x100000; // Write '1' to Enable interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTENSET_WRITE = 0x2000000;    // Write '1' to Enable interrupt on EVENTS_WRITE event
    static constexpr uint32_t INTENSET_READ = 0x4000000;     // Write '1' to Enable interrupt on EVENTS_READ event

    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_ERROR = 0x200;        // Write '1' to Clear interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENCLR_RXSTARTED = 0x80000;  // Write '1' to Clear interrupt on EVENTS_RXSTARTED event
    static constexpr uint32_t INTENCLR_TXSTARTED = 0x100000; // Write '1' to Clear interrupt on EVENTS_TXSTARTED event
    static constexpr uint32_t INTENCLR_WRITE = 0x2000000;    // Write '1' to Clear interrupt on EVENTS_WRITE event
    static constexpr uint32_t INTENCLR_READ = 0x4000000;     // Write '1' to Clear interrupt on EVENTS_READ event

    static constexpr uint32_t ERRORSRC_OVERFLOW = 0x1;       // RX buffer overflow detected, and prevented
    static constexpr uint32_t ERRORSRC_DNACK = 0x4;          // NACK sent after receiving a data byte
    static constexpr uint32_t ERRORSRC_OVERREAD = 0x8;       // TX buffer over-read detected, and prevented

    static constexpr uint32_t MATCH_MATCH = 0x1;          // Which of the addresses in {ADDRESS} matched the incoming address

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable TWIS (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t ADDRESS[%s]_ADDRESS =             // TWI slave address (7 bits)
        bit_field_t<0, 0x7f>::value<X>();

    static constexpr uint32_t CONFIG_ADDRESS0 = 0x1;       // Enable or disable address matching on ADDRESS[0]
    static constexpr uint32_t CONFIG_ADDRESS1 = 0x2;       // Enable or disable address matching on ADDRESS[1]
    static const uint32_t CONFIG_RESET_VALUE = 0x1;

    template<uint32_t X>
    static constexpr uint32_t ORC_ORC =                 // Over-read character. Character sent out in case of an over-read of the transmit buffer. (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    static constexpr uint8_t SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1 = 4; // 
};

static twis1_t& TWIS1 = *reinterpret_cast<twis1_t*>(0x40004000);

#define HAVE_PERIPHERAL_TWIS1


////
//
//    Serial Peripheral Interface 0
//
////

struct spi1_t
{
    volatile uint32_t    EVENTS_READY;         // [Read-write] TXD byte sent and RXD byte received
    reserved_t<126>      _0;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<125>      _1;
    volatile uint32_t    ENABLE;               // [Read-write] Enable SPI
    reserved_t<5>        _2;
    volatile uint32_t    RXD;                  // [Read-only] RXD register
    volatile uint32_t    TXD;                  // [Read-write] TXD register
    reserved_t<1>        _3;
    volatile uint32_t    FREQUENCY;            // [Read-write] SPI frequency
    reserved_t<11>       _4;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register


    static constexpr uint32_t INTENSET_READY = 0x4;          // Write '1' to Enable interrupt on EVENTS_READY event

    static constexpr uint32_t INTENCLR_READY = 0x4;          // Write '1' to Clear interrupt on EVENTS_READY event

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable SPI (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t RXD_RXD =                 // RX data received. Double buffered (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t TXD_TXD =                 // TX data to send. Double buffered (8 bits)
        bit_field_t<0, 0xff>::value<X>();


    static const uint32_t FREQUENCY_RESET_VALUE = 0x4000000;

    static constexpr uint32_t CONFIG_ORDER = 0x1;          // Bit order
    static constexpr uint32_t CONFIG_CPHA = 0x2;           // Serial clock (SCK) phase
    static constexpr uint32_t CONFIG_CPOL = 0x4;           // Serial clock (SCK) polarity

    static constexpr uint8_t SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1 = 4; // 
};

static spi1_t& SPI1 = *reinterpret_cast<spi1_t*>(0x40004000);

#define HAVE_PERIPHERAL_SPI1


////
//
//    NFC-A compatible radio
//
////

struct nfct_t
{
    volatile uint32_t    TASKS_ACTIVATE;       // [Write-only] Activate NFC peripheral for incoming and outgoing frames, change state to activated
    volatile uint32_t    TASKS_DISABLE;        // [Write-only] Disable NFC peripheral
    volatile uint32_t    TASKS_SENSE;          // [Write-only] Enable NFC sense field mode, change state to sense mode
    volatile uint32_t    TASKS_STARTTX;        // [Write-only] Start transmission of a outgoing frame, change state to transmit
    reserved_t<3>        _0;
    volatile uint32_t    TASKS_ENABLERXDATA;   // [Write-only] Initializes the EasyDMA for receive.
    reserved_t<1>        _1;
    volatile uint32_t    TASKS_GOIDLE;         // [Write-only] Force state machine to IDLE state
    volatile uint32_t    TASKS_GOSLEEP;        // [Write-only] Force state machine to SLEEP_A state
    reserved_t<53>       _2;
    volatile uint32_t    EVENTS_READY;         // [Read-write] The NFC peripheral is ready to receive and send frames
    volatile uint32_t    EVENTS_FIELDDETECTED; // [Read-write] Remote NFC field detected
    volatile uint32_t    EVENTS_FIELDLOST;     // [Read-write] Remote NFC field lost
    volatile uint32_t    EVENTS_TXFRAMESTART;  // [Read-write] Marks the start of the first symbol of a transmitted frame
    volatile uint32_t    EVENTS_TXFRAMEEND;    // [Read-write] Marks the end of the last transmitted on-air symbol of a frame
    volatile uint32_t    EVENTS_RXFRAMESTART;  // [Read-write] Marks the end of the first symbol of a received frame
    volatile uint32_t    EVENTS_RXFRAMEEND;    // [Read-write] Received data have been checked (CRC, parity) and transferred to RAM, and EasyDMA has ended accessing the RX buffer
    volatile uint32_t    EVENTS_ERROR;         // [Read-write] NFC error reported. The ERRORSTATUS register contains details on the source of the error.
    reserved_t<2>        _3;
    volatile uint32_t    EVENTS_RXERROR;       // [Read-write] NFC RX frame error reported. The FRAMESTATUS.RX register contains details on the source of the error.
    volatile uint32_t    EVENTS_ENDRX;         // [Read-write] RX buffer (as defined by PACKETPTR and MAXLEN) in Data RAM full.
    volatile uint32_t    EVENTS_ENDTX;         // [Read-write] Transmission of data in RAM has ended, and EasyDMA has ended accessing the TX buffer
    reserved_t<1>        _4;
    volatile uint32_t    EVENTS_AUTOCOLRESSTARTED;// [Read-write] Auto collision resolution process has started
    reserved_t<3>        _5;
    volatile uint32_t    EVENTS_COLLISION;     // [Read-write] NFC Auto collision resolution error reported.
    volatile uint32_t    EVENTS_SELECTED;      // [Read-write] NFC Auto collision resolution successfully completed
    volatile uint32_t    EVENTS_STARTED;       // [Read-write] EasyDMA is ready to receive or send frames.
    reserved_t<43>       _6;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<63>       _7;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<62>       _8;
    volatile uint32_t    ERRORSTATUS;          // [Read-write] NFC Error Status register
    reserved_t<10>       _9;
    volatile uint32_t    CURRENTLOADCTRL;      // [Read-only] Current value driven to the NFC Load Control
    reserved_t<2>        _10;
    volatile uint32_t    FIELDPRESENT;         // [Read-only] Indicates the presence or not of a valid field
    reserved_t<49>       _11;
    volatile uint32_t    FRAMEDELAYMIN;        // [Read-write] Minimum frame delay
    volatile uint32_t    FRAMEDELAYMAX;        // [Read-write] Maximum frame delay
    volatile uint32_t    FRAMEDELAYMODE;       // [Read-write] Configuration register for the Frame Delay Timer
    volatile uint32_t    PACKETPTR;            // [Read-write] Packet pointer for TXD and RXD data storage in Data RAM
    volatile uint32_t    MAXLEN;               // [Read-write] Size of allocated for TXD and RXD data storage buffer in Data RAM
    reserved_t<30>       _12;
    volatile uint32_t    NFCID1_LAST;          // [Read-write] Last NFCID1 part (4, 7 or 10 bytes ID)
    volatile uint32_t    NFCID1_2ND_LAST;      // [Read-write] Second last NFCID1 part (7 or 10 bytes ID)
    volatile uint32_t    NFCID1_3RD_LAST;      // [Read-write] Third last NFCID1 part (10 bytes ID)
    volatile uint32_t    AUTOCOLRESCONFIG;     // [Read-write] Controls the Auto collision resolution function. This setting must be done before the NFCT peripheral is enabled.
    volatile uint32_t    SENSRES;              // [Read-write] NFC-A SENS_RES auto-response settings
    volatile uint32_t    SELRES;               // [Read-write] NFC-A SEL_RES auto-response settings























    static constexpr uint32_t SHORTS_FIELDDETECTED_ACTIVATE = 0x1;// Shortcut between EVENTS_FIELDDETECTED event and TASKS_ACTIVATE task
    static constexpr uint32_t SHORTS_FIELDLOST_SENSE = 0x2;// Shortcut between EVENTS_FIELDLOST event and TASKS_SENSE task

    static constexpr uint32_t INTEN_READY = 0x1;          // Enable or disable interrupt on EVENTS_READY event
    static constexpr uint32_t INTEN_FIELDDETECTED = 0x2;  // Enable or disable interrupt on EVENTS_FIELDDETECTED event
    static constexpr uint32_t INTEN_FIELDLOST = 0x4;      // Enable or disable interrupt on EVENTS_FIELDLOST event
    static constexpr uint32_t INTEN_TXFRAMESTART = 0x8;   // Enable or disable interrupt on EVENTS_TXFRAMESTART event
    static constexpr uint32_t INTEN_TXFRAMEEND = 0x10;    // Enable or disable interrupt on EVENTS_TXFRAMEEND event
    static constexpr uint32_t INTEN_RXFRAMESTART = 0x20;  // Enable or disable interrupt on EVENTS_RXFRAMESTART event
    static constexpr uint32_t INTEN_RXFRAMEEND = 0x40;    // Enable or disable interrupt on EVENTS_RXFRAMEEND event
    static constexpr uint32_t INTEN_ERROR = 0x80;         // Enable or disable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTEN_RXERROR = 0x400;      // Enable or disable interrupt on EVENTS_RXERROR event
    static constexpr uint32_t INTEN_ENDRX = 0x800;        // Enable or disable interrupt on EVENTS_ENDRX event
    static constexpr uint32_t INTEN_ENDTX = 0x1000;       // Enable or disable interrupt on EVENTS_ENDTX event
    static constexpr uint32_t INTEN_AUTOCOLRESSTARTED = 0x4000;// Enable or disable interrupt on EVENTS_AUTOCOLRESSTARTED event
    static constexpr uint32_t INTEN_COLLISION = 0x40000;  // Enable or disable interrupt on EVENTS_COLLISION event
    static constexpr uint32_t INTEN_SELECTED = 0x80000;   // Enable or disable interrupt on EVENTS_SELECTED event
    static constexpr uint32_t INTEN_STARTED = 0x100000;   // Enable or disable interrupt on EVENTS_STARTED event

    static constexpr uint32_t INTENSET_READY = 0x1;          // Write '1' to Enable interrupt on EVENTS_READY event
    static constexpr uint32_t INTENSET_FIELDDETECTED = 0x2;  // Write '1' to Enable interrupt on EVENTS_FIELDDETECTED event
    static constexpr uint32_t INTENSET_FIELDLOST = 0x4;      // Write '1' to Enable interrupt on EVENTS_FIELDLOST event
    static constexpr uint32_t INTENSET_TXFRAMESTART = 0x8;   // Write '1' to Enable interrupt on EVENTS_TXFRAMESTART event
    static constexpr uint32_t INTENSET_TXFRAMEEND = 0x10;    // Write '1' to Enable interrupt on EVENTS_TXFRAMEEND event
    static constexpr uint32_t INTENSET_RXFRAMESTART = 0x20;  // Write '1' to Enable interrupt on EVENTS_RXFRAMESTART event
    static constexpr uint32_t INTENSET_RXFRAMEEND = 0x40;    // Write '1' to Enable interrupt on EVENTS_RXFRAMEEND event
    static constexpr uint32_t INTENSET_ERROR = 0x80;         // Write '1' to Enable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENSET_RXERROR = 0x400;      // Write '1' to Enable interrupt on EVENTS_RXERROR event
    static constexpr uint32_t INTENSET_ENDRX = 0x800;        // Write '1' to Enable interrupt on EVENTS_ENDRX event
    static constexpr uint32_t INTENSET_ENDTX = 0x1000;       // Write '1' to Enable interrupt on EVENTS_ENDTX event
    static constexpr uint32_t INTENSET_AUTOCOLRESSTARTED = 0x4000;// Write '1' to Enable interrupt on EVENTS_AUTOCOLRESSTARTED event
    static constexpr uint32_t INTENSET_COLLISION = 0x40000;  // Write '1' to Enable interrupt on EVENTS_COLLISION event
    static constexpr uint32_t INTENSET_SELECTED = 0x80000;   // Write '1' to Enable interrupt on EVENTS_SELECTED event
    static constexpr uint32_t INTENSET_STARTED = 0x100000;   // Write '1' to Enable interrupt on EVENTS_STARTED event

    static constexpr uint32_t INTENCLR_READY = 0x1;          // Write '1' to Clear interrupt on EVENTS_READY event
    static constexpr uint32_t INTENCLR_FIELDDETECTED = 0x2;  // Write '1' to Clear interrupt on EVENTS_FIELDDETECTED event
    static constexpr uint32_t INTENCLR_FIELDLOST = 0x4;      // Write '1' to Clear interrupt on EVENTS_FIELDLOST event
    static constexpr uint32_t INTENCLR_TXFRAMESTART = 0x8;   // Write '1' to Clear interrupt on EVENTS_TXFRAMESTART event
    static constexpr uint32_t INTENCLR_TXFRAMEEND = 0x10;    // Write '1' to Clear interrupt on EVENTS_TXFRAMEEND event
    static constexpr uint32_t INTENCLR_RXFRAMESTART = 0x20;  // Write '1' to Clear interrupt on EVENTS_RXFRAMESTART event
    static constexpr uint32_t INTENCLR_RXFRAMEEND = 0x40;    // Write '1' to Clear interrupt on EVENTS_RXFRAMEEND event
    static constexpr uint32_t INTENCLR_ERROR = 0x80;         // Write '1' to Clear interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENCLR_RXERROR = 0x400;      // Write '1' to Clear interrupt on EVENTS_RXERROR event
    static constexpr uint32_t INTENCLR_ENDRX = 0x800;        // Write '1' to Clear interrupt on EVENTS_ENDRX event
    static constexpr uint32_t INTENCLR_ENDTX = 0x1000;       // Write '1' to Clear interrupt on EVENTS_ENDTX event
    static constexpr uint32_t INTENCLR_AUTOCOLRESSTARTED = 0x4000;// Write '1' to Clear interrupt on EVENTS_AUTOCOLRESSTARTED event
    static constexpr uint32_t INTENCLR_COLLISION = 0x40000;  // Write '1' to Clear interrupt on EVENTS_COLLISION event
    static constexpr uint32_t INTENCLR_SELECTED = 0x80000;   // Write '1' to Clear interrupt on EVENTS_SELECTED event
    static constexpr uint32_t INTENCLR_STARTED = 0x100000;   // Write '1' to Clear interrupt on EVENTS_STARTED event

    static constexpr uint32_t ERRORSTATUS_FRAMEDELAYTIMEOUT = 0x1;// No STARTTX task triggered before expiration of the time set in FRAMEDELAYMAX
    static constexpr uint32_t ERRORSTATUS_INVALIDNFCSYMBOL = 0x2;// The received pulse does not match a valid NFC-A symbol
    static constexpr uint32_t ERRORSTATUS_NFCFIELDTOOSTRONG = 0x4;// Field level is too high at max load resistance
    static constexpr uint32_t ERRORSTATUS_NFCFIELDTOOWEAK = 0x8;// Field level is too low at min load resistance
    static constexpr uint32_t ERRORSTATUS_EOFERROR = 0x40;      // No valid End of Frame detected

    template<uint32_t X>
    static constexpr uint32_t CURRENTLOADCTRL_CURRENTLOADCTRL =     // Current value driven to the NFC Load Control (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    static const uint32_t CURRENTLOADCTRL_RESET_VALUE = 0x8;

    static constexpr uint32_t FIELDPRESENT_FIELDPRESENT = 0x1;   // Indicates the presence or not of a valid field. Linked to the FIELDDETECTED and FIELDLOST events.
    static constexpr uint32_t FIELDPRESENT_LOCKDETECT = 0x2;     // Indicates if the low level has locked to the field

    template<uint32_t X>
    static constexpr uint32_t FRAMEDELAYMIN_FRAMEDELAYMIN =       // Minimum frame delay in number of 13.56 MHz clocks (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t FRAMEDELAYMIN_RESET_VALUE = 0x480;

    template<uint32_t X>
    static constexpr uint32_t FRAMEDELAYMAX_FRAMEDELAYMAX =       // Maximum frame delay in number of 13.56 MHz clocks (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t FRAMEDELAYMAX_RESET_VALUE = 0x1000;

    template<uint32_t X>
    static constexpr uint32_t FRAMEDELAYMODE_FRAMEDELAYMODE =      // Configuration register for the Frame Delay Timer (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t FRAMEDELAYMODE_RESET_VALUE = 0x1;



    template<uint32_t X>
    static constexpr uint32_t MAXLEN_MAXLEN =              // Size of allocated for TXD and RXD data storage buffer in Data RAM (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t NFCID1_LAST_NFCID1_Z =            // NFCID1 byte Z (very last byte sent) (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NFCID1_LAST_NFCID1_Y =            // NFCID1 byte Y (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NFCID1_LAST_NFCID1_X =            // NFCID1 byte X (8 bits)
        bit_field_t<16, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NFCID1_LAST_NFCID1_W =            // NFCID1 byte W (8 bits)
        bit_field_t<24, 0xff>::value<X>();
    static const uint32_t NFCID1_LAST_RESET_VALUE = 0x6363;

    template<uint32_t X>
    static constexpr uint32_t NFCID1_2ND_LAST_NFCID1_V =            // NFCID1 byte V (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NFCID1_2ND_LAST_NFCID1_U =            // NFCID1 byte U (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NFCID1_2ND_LAST_NFCID1_T =            // NFCID1 byte T (8 bits)
        bit_field_t<16, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t NFCID1_3RD_LAST_NFCID1_S =            // NFCID1 byte S (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NFCID1_3RD_LAST_NFCID1_R =            // NFCID1 byte R (8 bits)
        bit_field_t<8, 0xff>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t NFCID1_3RD_LAST_NFCID1_Q =            // NFCID1 byte Q (8 bits)
        bit_field_t<16, 0xff>::value<X>();

    static constexpr uint32_t AUTOCOLRESCONFIG_MODE = 0x1;           // Enables/disables Auto collision resolution
    static constexpr uint32_t AUTOCOLRESCONFIG_FILTER = 0x2;         // Enables/disables Auto collision resolution short frame (any frames less than 7 bits) noise filter
    static const uint32_t AUTOCOLRESCONFIG_RESET_VALUE = 0x2;

    template<uint32_t X>
    static constexpr uint32_t SENSRES_BITFRAMESDD =         // Bit frame SDD as defined by the b5:b1 of byte 1 in SENS_RES response in the NFC Forum, NFC Digital Protocol Technical Specification (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static constexpr uint32_t SENSRES_RFU5 = 0x20;          // Reserved for future use. Shall be 0.
    template<uint32_t X>
    static constexpr uint32_t SENSRES_NFCIDSIZE =           // NFCID1 size. This value is used by the Auto collision resolution engine. (2 bits)
        bit_field_t<6, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SENSRES_PLATFCONFIG =         // Tag platform configuration as defined by the b4:b1 of byte 2 in SENS_RES response in the NFC Forum, NFC Digital Protocol Technical Specification (4 bits)
        bit_field_t<8, 0xf>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SENSRES_RFU74 =               // Reserved for future use. Shall be 0. (4 bits)
        bit_field_t<12, 0xf>::value<X>();
    static const uint32_t SENSRES_RESET_VALUE = 0x1;

    template<uint32_t X>
    static constexpr uint32_t SELRES_RFU10 =               // Reserved for future use. Shall be 0. (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static constexpr uint32_t SELRES_CASCADE = 0x4;        // Cascade bit (controlled by hardware, write has no effect)
    template<uint32_t X>
    static constexpr uint32_t SELRES_RFU43 =               // Reserved for future use. Shall be 0. (2 bits)
        bit_field_t<3, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t SELRES_PROTOCOL =            // Protocol as defined by the b7:b6 of SEL_RES response in the NFC Forum, NFC Digital Protocol Technical Specification (2 bits)
        bit_field_t<5, 0x3>::value<X>();
    static constexpr uint32_t SELRES_RFU7 = 0x80;          // Reserved for future use. Shall be 0.

    static constexpr uint8_t NFCT = 5; // 
};

static nfct_t& NFCT = *reinterpret_cast<nfct_t*>(0x40005000);

#define HAVE_PERIPHERAL_NFCT


////
//
//    GPIO Tasks and Events
//
////

struct gpiote_t
{
    volatile uint32_t    TASKS_OUT[s];         // [Write-only] Description collection[0]: Task for writing to pin specified in CONFIG[0].PSEL. Action on pin is configured in CONFIG[0].POLARITY.
    reserved_t<11>       _0;
    volatile uint32_t    TASKS_SET[s];         // [Write-only] Description collection[0]: Task for writing to pin specified in CONFIG[0].PSEL. Action on pin is to set it high.
    reserved_t<11>       _1;
    volatile uint32_t    TASKS_CLR[s];         // [Write-only] Description collection[0]: Task for writing to pin specified in CONFIG[0].PSEL. Action on pin is to set it low.
    reserved_t<39>       _2;
    volatile uint32_t    EVENTS_IN[s];         // [Read-write] Description collection[0]: Event generated from pin specified in CONFIG[0].PSEL
    reserved_t<30>       _3;
    volatile uint32_t    EVENTS_PORT;          // [Read-write] Event generated from multiple input GPIO pins with SENSE mechanism enabled
    reserved_t<97>       _4;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<129>      _5;
    volatile uint32_t    CONFIG[s];            // [Read-write] Description collection[0]: Configuration for OUT[n], SET[n] and CLR[n] tasks and IN[n] event






    static constexpr uint32_t INTENSET_IN0 = 0x1;            // Write '1' to Enable interrupt on EVENTS_IN[0] event
    static constexpr uint32_t INTENSET_IN1 = 0x2;            // Write '1' to Enable interrupt on EVENTS_IN[1] event
    static constexpr uint32_t INTENSET_IN2 = 0x4;            // Write '1' to Enable interrupt on EVENTS_IN[2] event
    static constexpr uint32_t INTENSET_IN3 = 0x8;            // Write '1' to Enable interrupt on EVENTS_IN[3] event
    static constexpr uint32_t INTENSET_IN4 = 0x10;           // Write '1' to Enable interrupt on EVENTS_IN[4] event
    static constexpr uint32_t INTENSET_IN5 = 0x20;           // Write '1' to Enable interrupt on EVENTS_IN[5] event
    static constexpr uint32_t INTENSET_IN6 = 0x40;           // Write '1' to Enable interrupt on EVENTS_IN[6] event
    static constexpr uint32_t INTENSET_IN7 = 0x80;           // Write '1' to Enable interrupt on EVENTS_IN[7] event
    static constexpr uint32_t INTENSET_PORT = 0x80000000;    // Write '1' to Enable interrupt on EVENTS_PORT event

    static constexpr uint32_t INTENCLR_IN0 = 0x1;            // Write '1' to Clear interrupt on EVENTS_IN[0] event
    static constexpr uint32_t INTENCLR_IN1 = 0x2;            // Write '1' to Clear interrupt on EVENTS_IN[1] event
    static constexpr uint32_t INTENCLR_IN2 = 0x4;            // Write '1' to Clear interrupt on EVENTS_IN[2] event
    static constexpr uint32_t INTENCLR_IN3 = 0x8;            // Write '1' to Clear interrupt on EVENTS_IN[3] event
    static constexpr uint32_t INTENCLR_IN4 = 0x10;           // Write '1' to Clear interrupt on EVENTS_IN[4] event
    static constexpr uint32_t INTENCLR_IN5 = 0x20;           // Write '1' to Clear interrupt on EVENTS_IN[5] event
    static constexpr uint32_t INTENCLR_IN6 = 0x40;           // Write '1' to Clear interrupt on EVENTS_IN[6] event
    static constexpr uint32_t INTENCLR_IN7 = 0x80;           // Write '1' to Clear interrupt on EVENTS_IN[7] event
    static constexpr uint32_t INTENCLR_PORT = 0x80000000;    // Write '1' to Clear interrupt on EVENTS_PORT event

    template<uint32_t X>
    static constexpr uint32_t CONFIG[%s]_MODE =                // Mode (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CONFIG[%s]_PSEL =                // GPIO number associated with SET[n], CLR[n] and OUT[n] tasks and IN[n] event (5 bits)
        bit_field_t<8, 0x1f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t CONFIG[%s]_POLARITY =            // When In task mode: Operation to be performed on output when OUT[n] task is triggered. When In event mode: Operation on input that shall trigger IN[n] event. (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    static constexpr uint32_t CONFIG[%s]_OUTINIT = 0x100000;   // When in task mode: Initial value of the output when the GPIOTE channel is configured. When in event mode: No effect.

    static constexpr uint8_t GPIOTE = 6; // 
};

static gpiote_t& GPIOTE = *reinterpret_cast<gpiote_t*>(0x40006000);

#define HAVE_PERIPHERAL_GPIOTE


////
//
//    Analog to Digital Converter
//
////

struct saadc_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start the ADC and prepare the result buffer in RAM
    volatile uint32_t    TASKS_SAMPLE;         // [Write-only] Take one ADC sample, if scan is enabled all channels are sampled
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop the ADC and terminate any on-going conversion
    volatile uint32_t    TASKS_CALIBRATEOFFSET;// [Write-only] Starts offset auto-calibration
    reserved_t<60>       _0;
    volatile uint32_t    EVENTS_STARTED;       // [Read-write] The ADC has started
    volatile uint32_t    EVENTS_END;           // [Read-write] The ADC has filled up the Result buffer
    volatile uint32_t    EVENTS_DONE;          // [Read-write] A conversion task has been completed. Depending on the mode, multiple conversions might be needed for a result to be transferred to RAM.
    volatile uint32_t    EVENTS_RESULTDONE;    // [Read-write] A result is ready to get transferred to RAM
    volatile uint32_t    EVENTS_CALIBRATEDONE; // [Read-write] Calibration is complete
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] The ADC has stopped
    reserved_t<122>      _1;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<61>       _2;
    volatile uint32_t    STATUS;               // [Read-only] Status
    reserved_t<63>       _3;
    volatile uint32_t    ENABLE;               // [Read-write] Enable or disable ADC
    reserved_t<59>       _4;
    volatile uint32_t    RESOLUTION;           // [Read-write] Resolution configuration
    volatile uint32_t    OVERSAMPLE;           // [Read-write] Oversampling configuration. OVERSAMPLE should not be combined with SCAN. The RESOLUTION is applied before averaging, thus for high OVERSAMPLE a higher RESOLUTION should be used.
    volatile uint32_t    SAMPLERATE;           // [Read-write] Controls normal or continuous sample rate











    static constexpr uint32_t INTEN_STARTED = 0x1;        // Enable or disable interrupt on EVENTS_STARTED event
    static constexpr uint32_t INTEN_END = 0x2;            // Enable or disable interrupt on EVENTS_END event
    static constexpr uint32_t INTEN_DONE = 0x4;           // Enable or disable interrupt on EVENTS_DONE event
    static constexpr uint32_t INTEN_RESULTDONE = 0x8;     // Enable or disable interrupt on EVENTS_RESULTDONE event
    static constexpr uint32_t INTEN_CALIBRATEDONE = 0x10; // Enable or disable interrupt on EVENTS_CALIBRATEDONE event
    static constexpr uint32_t INTEN_STOPPED = 0x20;       // Enable or disable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTEN_CH0LIMITH = 0x40;     // Enable or disable interrupt on EVENTS_CH[0].LIMITH event
    static constexpr uint32_t INTEN_CH0LIMITL = 0x80;     // Enable or disable interrupt on EVENTS_CH[0].LIMITL event
    static constexpr uint32_t INTEN_CH1LIMITH = 0x100;    // Enable or disable interrupt on EVENTS_CH[1].LIMITH event
    static constexpr uint32_t INTEN_CH1LIMITL = 0x200;    // Enable or disable interrupt on EVENTS_CH[1].LIMITL event
    static constexpr uint32_t INTEN_CH2LIMITH = 0x400;    // Enable or disable interrupt on EVENTS_CH[2].LIMITH event
    static constexpr uint32_t INTEN_CH2LIMITL = 0x800;    // Enable or disable interrupt on EVENTS_CH[2].LIMITL event
    static constexpr uint32_t INTEN_CH3LIMITH = 0x1000;   // Enable or disable interrupt on EVENTS_CH[3].LIMITH event
    static constexpr uint32_t INTEN_CH3LIMITL = 0x2000;   // Enable or disable interrupt on EVENTS_CH[3].LIMITL event
    static constexpr uint32_t INTEN_CH4LIMITH = 0x4000;   // Enable or disable interrupt on EVENTS_CH[4].LIMITH event
    static constexpr uint32_t INTEN_CH4LIMITL = 0x8000;   // Enable or disable interrupt on EVENTS_CH[4].LIMITL event
    static constexpr uint32_t INTEN_CH5LIMITH = 0x10000;  // Enable or disable interrupt on EVENTS_CH[5].LIMITH event
    static constexpr uint32_t INTEN_CH5LIMITL = 0x20000;  // Enable or disable interrupt on EVENTS_CH[5].LIMITL event
    static constexpr uint32_t INTEN_CH6LIMITH = 0x40000;  // Enable or disable interrupt on EVENTS_CH[6].LIMITH event
    static constexpr uint32_t INTEN_CH6LIMITL = 0x80000;  // Enable or disable interrupt on EVENTS_CH[6].LIMITL event
    static constexpr uint32_t INTEN_CH7LIMITH = 0x100000; // Enable or disable interrupt on EVENTS_CH[7].LIMITH event
    static constexpr uint32_t INTEN_CH7LIMITL = 0x200000; // Enable or disable interrupt on EVENTS_CH[7].LIMITL event

    static constexpr uint32_t INTENSET_STARTED = 0x1;        // Write '1' to Enable interrupt on EVENTS_STARTED event
    static constexpr uint32_t INTENSET_END = 0x2;            // Write '1' to Enable interrupt on EVENTS_END event
    static constexpr uint32_t INTENSET_DONE = 0x4;           // Write '1' to Enable interrupt on EVENTS_DONE event
    static constexpr uint32_t INTENSET_RESULTDONE = 0x8;     // Write '1' to Enable interrupt on EVENTS_RESULTDONE event
    static constexpr uint32_t INTENSET_CALIBRATEDONE = 0x10; // Write '1' to Enable interrupt on EVENTS_CALIBRATEDONE event
    static constexpr uint32_t INTENSET_STOPPED = 0x20;       // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_CH0LIMITH = 0x40;     // Write '1' to Enable interrupt on EVENTS_CH[0].LIMITH event
    static constexpr uint32_t INTENSET_CH0LIMITL = 0x80;     // Write '1' to Enable interrupt on EVENTS_CH[0].LIMITL event
    static constexpr uint32_t INTENSET_CH1LIMITH = 0x100;    // Write '1' to Enable interrupt on EVENTS_CH[1].LIMITH event
    static constexpr uint32_t INTENSET_CH1LIMITL = 0x200;    // Write '1' to Enable interrupt on EVENTS_CH[1].LIMITL event
    static constexpr uint32_t INTENSET_CH2LIMITH = 0x400;    // Write '1' to Enable interrupt on EVENTS_CH[2].LIMITH event
    static constexpr uint32_t INTENSET_CH2LIMITL = 0x800;    // Write '1' to Enable interrupt on EVENTS_CH[2].LIMITL event
    static constexpr uint32_t INTENSET_CH3LIMITH = 0x1000;   // Write '1' to Enable interrupt on EVENTS_CH[3].LIMITH event
    static constexpr uint32_t INTENSET_CH3LIMITL = 0x2000;   // Write '1' to Enable interrupt on EVENTS_CH[3].LIMITL event
    static constexpr uint32_t INTENSET_CH4LIMITH = 0x4000;   // Write '1' to Enable interrupt on EVENTS_CH[4].LIMITH event
    static constexpr uint32_t INTENSET_CH4LIMITL = 0x8000;   // Write '1' to Enable interrupt on EVENTS_CH[4].LIMITL event
    static constexpr uint32_t INTENSET_CH5LIMITH = 0x10000;  // Write '1' to Enable interrupt on EVENTS_CH[5].LIMITH event
    static constexpr uint32_t INTENSET_CH5LIMITL = 0x20000;  // Write '1' to Enable interrupt on EVENTS_CH[5].LIMITL event
    static constexpr uint32_t INTENSET_CH6LIMITH = 0x40000;  // Write '1' to Enable interrupt on EVENTS_CH[6].LIMITH event
    static constexpr uint32_t INTENSET_CH6LIMITL = 0x80000;  // Write '1' to Enable interrupt on EVENTS_CH[6].LIMITL event
    static constexpr uint32_t INTENSET_CH7LIMITH = 0x100000; // Write '1' to Enable interrupt on EVENTS_CH[7].LIMITH event
    static constexpr uint32_t INTENSET_CH7LIMITL = 0x200000; // Write '1' to Enable interrupt on EVENTS_CH[7].LIMITL event

    static constexpr uint32_t INTENCLR_STARTED = 0x1;        // Write '1' to Clear interrupt on EVENTS_STARTED event
    static constexpr uint32_t INTENCLR_END = 0x2;            // Write '1' to Clear interrupt on EVENTS_END event
    static constexpr uint32_t INTENCLR_DONE = 0x4;           // Write '1' to Clear interrupt on EVENTS_DONE event
    static constexpr uint32_t INTENCLR_RESULTDONE = 0x8;     // Write '1' to Clear interrupt on EVENTS_RESULTDONE event
    static constexpr uint32_t INTENCLR_CALIBRATEDONE = 0x10; // Write '1' to Clear interrupt on EVENTS_CALIBRATEDONE event
    static constexpr uint32_t INTENCLR_STOPPED = 0x20;       // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_CH0LIMITH = 0x40;     // Write '1' to Clear interrupt on EVENTS_CH[0].LIMITH event
    static constexpr uint32_t INTENCLR_CH0LIMITL = 0x80;     // Write '1' to Clear interrupt on EVENTS_CH[0].LIMITL event
    static constexpr uint32_t INTENCLR_CH1LIMITH = 0x100;    // Write '1' to Clear interrupt on EVENTS_CH[1].LIMITH event
    static constexpr uint32_t INTENCLR_CH1LIMITL = 0x200;    // Write '1' to Clear interrupt on EVENTS_CH[1].LIMITL event
    static constexpr uint32_t INTENCLR_CH2LIMITH = 0x400;    // Write '1' to Clear interrupt on EVENTS_CH[2].LIMITH event
    static constexpr uint32_t INTENCLR_CH2LIMITL = 0x800;    // Write '1' to Clear interrupt on EVENTS_CH[2].LIMITL event
    static constexpr uint32_t INTENCLR_CH3LIMITH = 0x1000;   // Write '1' to Clear interrupt on EVENTS_CH[3].LIMITH event
    static constexpr uint32_t INTENCLR_CH3LIMITL = 0x2000;   // Write '1' to Clear interrupt on EVENTS_CH[3].LIMITL event
    static constexpr uint32_t INTENCLR_CH4LIMITH = 0x4000;   // Write '1' to Clear interrupt on EVENTS_CH[4].LIMITH event
    static constexpr uint32_t INTENCLR_CH4LIMITL = 0x8000;   // Write '1' to Clear interrupt on EVENTS_CH[4].LIMITL event
    static constexpr uint32_t INTENCLR_CH5LIMITH = 0x10000;  // Write '1' to Clear interrupt on EVENTS_CH[5].LIMITH event
    static constexpr uint32_t INTENCLR_CH5LIMITL = 0x20000;  // Write '1' to Clear interrupt on EVENTS_CH[5].LIMITL event
    static constexpr uint32_t INTENCLR_CH6LIMITH = 0x40000;  // Write '1' to Clear interrupt on EVENTS_CH[6].LIMITH event
    static constexpr uint32_t INTENCLR_CH6LIMITL = 0x80000;  // Write '1' to Clear interrupt on EVENTS_CH[6].LIMITL event
    static constexpr uint32_t INTENCLR_CH7LIMITH = 0x100000; // Write '1' to Clear interrupt on EVENTS_CH[7].LIMITH event
    static constexpr uint32_t INTENCLR_CH7LIMITL = 0x200000; // Write '1' to Clear interrupt on EVENTS_CH[7].LIMITL event

    static constexpr uint32_t STATUS_STATUS = 0x1;         // Status

    static constexpr uint32_t ENABLE_ENABLE = 0x1;         // Enable or disable ADC

    template<uint32_t X>
    static constexpr uint32_t RESOLUTION_VAL =                 // Set the resolution (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t RESOLUTION_RESET_VALUE = 0x1;

    template<uint32_t X>
    static constexpr uint32_t OVERSAMPLE_OVERSAMPLE =          // Oversample control (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t SAMPLERATE_CC =                  // Capture and compare value. Sample rate is 16 MHz/CC (11 bits)
        bit_field_t<0, 0x7ff>::value<X>();
    static constexpr uint32_t SAMPLERATE_MODE = 0x1000;        // Select mode for sample rate control

    static constexpr uint8_t SAADC = 7; // 
};

static saadc_t& SAADC = *reinterpret_cast<saadc_t*>(0x40007000);

#define HAVE_PERIPHERAL_SAADC


////
//
//    Timer/Counter 0
//
////

struct timer0_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start Timer
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop Timer
    volatile uint32_t    TASKS_COUNT;          // [Write-only] Increment Timer (Counter mode only)
    volatile uint32_t    TASKS_CLEAR;          // [Write-only] Clear time
    volatile uint32_t    TASKS_SHUTDOWN;       // [Write-only] Shut down timer
    reserved_t<11>       _0;
    volatile uint32_t    TASKS_CAPTURE[s];     // [Write-only] Description collection[0]: Capture Timer value to CC[0] register
    reserved_t<63>       _1;
    volatile uint32_t    EVENTS_COMPARE[s];    // [Read-write] Description collection[0]: Compare event on CC[0] match
    reserved_t<47>       _2;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _3;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<126>      _4;
    volatile uint32_t    MODE;                 // [Read-write] Timer mode selection
    volatile uint32_t    BITMODE;              // [Read-write] Configure the number of bits used by the TIMER
    reserved_t<1>        _5;
    volatile uint32_t    PRESCALER;            // [Read-write] Timer prescaler register
    reserved_t<11>       _6;
    volatile uint32_t    CC[s];                // [Read-write] Description collection[0]: Capture/Compare register 0








    static constexpr uint32_t SHORTS_COMPARE0_CLEAR = 0x1; // Shortcut between EVENTS_COMPARE[0] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE1_CLEAR = 0x2; // Shortcut between EVENTS_COMPARE[1] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE2_CLEAR = 0x4; // Shortcut between EVENTS_COMPARE[2] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE3_CLEAR = 0x8; // Shortcut between EVENTS_COMPARE[3] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE4_CLEAR = 0x10;// Shortcut between EVENTS_COMPARE[4] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE5_CLEAR = 0x20;// Shortcut between EVENTS_COMPARE[5] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE0_STOP = 0x100;// Shortcut between EVENTS_COMPARE[0] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE1_STOP = 0x200;// Shortcut between EVENTS_COMPARE[1] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE2_STOP = 0x400;// Shortcut between EVENTS_COMPARE[2] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE3_STOP = 0x800;// Shortcut between EVENTS_COMPARE[3] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE4_STOP = 0x1000;// Shortcut between EVENTS_COMPARE[4] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE5_STOP = 0x2000;// Shortcut between EVENTS_COMPARE[5] event and TASKS_STOP task

    static constexpr uint32_t INTENSET_COMPARE0 = 0x10000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENSET_COMPARE1 = 0x20000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENSET_COMPARE2 = 0x40000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENSET_COMPARE3 = 0x80000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[3] event
    static constexpr uint32_t INTENSET_COMPARE4 = 0x100000;  // Write '1' to Enable interrupt on EVENTS_COMPARE[4] event
    static constexpr uint32_t INTENSET_COMPARE5 = 0x200000;  // Write '1' to Enable interrupt on EVENTS_COMPARE[5] event

    static constexpr uint32_t INTENCLR_COMPARE0 = 0x10000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENCLR_COMPARE1 = 0x20000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENCLR_COMPARE2 = 0x40000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENCLR_COMPARE3 = 0x80000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[3] event
    static constexpr uint32_t INTENCLR_COMPARE4 = 0x100000;  // Write '1' to Clear interrupt on EVENTS_COMPARE[4] event
    static constexpr uint32_t INTENCLR_COMPARE5 = 0x200000;  // Write '1' to Clear interrupt on EVENTS_COMPARE[5] event

    template<uint32_t X>
    static constexpr uint32_t MODE_MODE =                // Timer mode (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t BITMODE_BITMODE =             // Timer bit width (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t PRESCALER_PRESCALER =           // Prescaler value (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESCALER_RESET_VALUE = 0x4;



    static constexpr uint8_t TIMER0 = 8; // 
};

static timer0_t& TIMER0 = *reinterpret_cast<timer0_t*>(0x40008000);

#define HAVE_PERIPHERAL_TIMER0


////
//
//    Timer/Counter 0
//
////

struct timer1_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start Timer
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop Timer
    volatile uint32_t    TASKS_COUNT;          // [Write-only] Increment Timer (Counter mode only)
    volatile uint32_t    TASKS_CLEAR;          // [Write-only] Clear time
    volatile uint32_t    TASKS_SHUTDOWN;       // [Write-only] Shut down timer
    reserved_t<11>       _0;
    volatile uint32_t    TASKS_CAPTURE[s];     // [Write-only] Description collection[0]: Capture Timer value to CC[0] register
    reserved_t<63>       _1;
    volatile uint32_t    EVENTS_COMPARE[s];    // [Read-write] Description collection[0]: Compare event on CC[0] match
    reserved_t<47>       _2;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _3;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<126>      _4;
    volatile uint32_t    MODE;                 // [Read-write] Timer mode selection
    volatile uint32_t    BITMODE;              // [Read-write] Configure the number of bits used by the TIMER
    reserved_t<1>        _5;
    volatile uint32_t    PRESCALER;            // [Read-write] Timer prescaler register
    reserved_t<11>       _6;
    volatile uint32_t    CC[s];                // [Read-write] Description collection[0]: Capture/Compare register 0








    static constexpr uint32_t SHORTS_COMPARE0_CLEAR = 0x1; // Shortcut between EVENTS_COMPARE[0] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE1_CLEAR = 0x2; // Shortcut between EVENTS_COMPARE[1] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE2_CLEAR = 0x4; // Shortcut between EVENTS_COMPARE[2] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE3_CLEAR = 0x8; // Shortcut between EVENTS_COMPARE[3] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE4_CLEAR = 0x10;// Shortcut between EVENTS_COMPARE[4] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE5_CLEAR = 0x20;// Shortcut between EVENTS_COMPARE[5] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE0_STOP = 0x100;// Shortcut between EVENTS_COMPARE[0] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE1_STOP = 0x200;// Shortcut between EVENTS_COMPARE[1] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE2_STOP = 0x400;// Shortcut between EVENTS_COMPARE[2] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE3_STOP = 0x800;// Shortcut between EVENTS_COMPARE[3] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE4_STOP = 0x1000;// Shortcut between EVENTS_COMPARE[4] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE5_STOP = 0x2000;// Shortcut between EVENTS_COMPARE[5] event and TASKS_STOP task

    static constexpr uint32_t INTENSET_COMPARE0 = 0x10000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENSET_COMPARE1 = 0x20000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENSET_COMPARE2 = 0x40000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENSET_COMPARE3 = 0x80000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[3] event
    static constexpr uint32_t INTENSET_COMPARE4 = 0x100000;  // Write '1' to Enable interrupt on EVENTS_COMPARE[4] event
    static constexpr uint32_t INTENSET_COMPARE5 = 0x200000;  // Write '1' to Enable interrupt on EVENTS_COMPARE[5] event

    static constexpr uint32_t INTENCLR_COMPARE0 = 0x10000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENCLR_COMPARE1 = 0x20000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENCLR_COMPARE2 = 0x40000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENCLR_COMPARE3 = 0x80000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[3] event
    static constexpr uint32_t INTENCLR_COMPARE4 = 0x100000;  // Write '1' to Clear interrupt on EVENTS_COMPARE[4] event
    static constexpr uint32_t INTENCLR_COMPARE5 = 0x200000;  // Write '1' to Clear interrupt on EVENTS_COMPARE[5] event

    template<uint32_t X>
    static constexpr uint32_t MODE_MODE =                // Timer mode (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t BITMODE_BITMODE =             // Timer bit width (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t PRESCALER_PRESCALER =           // Prescaler value (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESCALER_RESET_VALUE = 0x4;



    static constexpr uint8_t TIMER1 = 9; // 
};

static timer1_t& TIMER1 = *reinterpret_cast<timer1_t*>(0x40009000);

#define HAVE_PERIPHERAL_TIMER1


////
//
//    Timer/Counter 0
//
////

struct timer2_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start Timer
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop Timer
    volatile uint32_t    TASKS_COUNT;          // [Write-only] Increment Timer (Counter mode only)
    volatile uint32_t    TASKS_CLEAR;          // [Write-only] Clear time
    volatile uint32_t    TASKS_SHUTDOWN;       // [Write-only] Shut down timer
    reserved_t<11>       _0;
    volatile uint32_t    TASKS_CAPTURE[s];     // [Write-only] Description collection[0]: Capture Timer value to CC[0] register
    reserved_t<63>       _1;
    volatile uint32_t    EVENTS_COMPARE[s];    // [Read-write] Description collection[0]: Compare event on CC[0] match
    reserved_t<47>       _2;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _3;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<126>      _4;
    volatile uint32_t    MODE;                 // [Read-write] Timer mode selection
    volatile uint32_t    BITMODE;              // [Read-write] Configure the number of bits used by the TIMER
    reserved_t<1>        _5;
    volatile uint32_t    PRESCALER;            // [Read-write] Timer prescaler register
    reserved_t<11>       _6;
    volatile uint32_t    CC[s];                // [Read-write] Description collection[0]: Capture/Compare register 0








    static constexpr uint32_t SHORTS_COMPARE0_CLEAR = 0x1; // Shortcut between EVENTS_COMPARE[0] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE1_CLEAR = 0x2; // Shortcut between EVENTS_COMPARE[1] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE2_CLEAR = 0x4; // Shortcut between EVENTS_COMPARE[2] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE3_CLEAR = 0x8; // Shortcut between EVENTS_COMPARE[3] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE4_CLEAR = 0x10;// Shortcut between EVENTS_COMPARE[4] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE5_CLEAR = 0x20;// Shortcut between EVENTS_COMPARE[5] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE0_STOP = 0x100;// Shortcut between EVENTS_COMPARE[0] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE1_STOP = 0x200;// Shortcut between EVENTS_COMPARE[1] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE2_STOP = 0x400;// Shortcut between EVENTS_COMPARE[2] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE3_STOP = 0x800;// Shortcut between EVENTS_COMPARE[3] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE4_STOP = 0x1000;// Shortcut between EVENTS_COMPARE[4] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE5_STOP = 0x2000;// Shortcut between EVENTS_COMPARE[5] event and TASKS_STOP task

    static constexpr uint32_t INTENSET_COMPARE0 = 0x10000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENSET_COMPARE1 = 0x20000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENSET_COMPARE2 = 0x40000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENSET_COMPARE3 = 0x80000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[3] event
    static constexpr uint32_t INTENSET_COMPARE4 = 0x100000;  // Write '1' to Enable interrupt on EVENTS_COMPARE[4] event
    static constexpr uint32_t INTENSET_COMPARE5 = 0x200000;  // Write '1' to Enable interrupt on EVENTS_COMPARE[5] event

    static constexpr uint32_t INTENCLR_COMPARE0 = 0x10000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENCLR_COMPARE1 = 0x20000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENCLR_COMPARE2 = 0x40000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENCLR_COMPARE3 = 0x80000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[3] event
    static constexpr uint32_t INTENCLR_COMPARE4 = 0x100000;  // Write '1' to Clear interrupt on EVENTS_COMPARE[4] event
    static constexpr uint32_t INTENCLR_COMPARE5 = 0x200000;  // Write '1' to Clear interrupt on EVENTS_COMPARE[5] event

    template<uint32_t X>
    static constexpr uint32_t MODE_MODE =                // Timer mode (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t BITMODE_BITMODE =             // Timer bit width (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t PRESCALER_PRESCALER =           // Prescaler value (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESCALER_RESET_VALUE = 0x4;



    static constexpr uint8_t TIMER2 = 10; // 
};

static timer2_t& TIMER2 = *reinterpret_cast<timer2_t*>(0x4000a000);

#define HAVE_PERIPHERAL_TIMER2


////
//
//    Real time counter 0
//
////

struct rtc0_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start RTC COUNTER
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop RTC COUNTER
    volatile uint32_t    TASKS_CLEAR;          // [Write-only] Clear RTC COUNTER
    volatile uint32_t    TASKS_TRIGOVRFLW;     // [Write-only] Set COUNTER to 0xFFFFF0
    reserved_t<60>       _0;
    volatile uint32_t    EVENTS_TICK;          // [Read-write] Event on COUNTER increment
    volatile uint32_t    EVENTS_OVRFLW;        // [Read-write] Event on COUNTER overflow
    reserved_t<14>       _1;
    volatile uint32_t    EVENTS_COMPARE[s];    // [Read-write] Description collection[0]: Compare event on CC[0] match
    reserved_t<112>      _2;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<13>       _3;
    volatile uint32_t    EVTEN;                // [Read-write] Enable or disable event routing
    volatile uint32_t    EVTENSET;             // [Read-write] Enable event routing
    volatile uint32_t    EVTENCLR;             // [Read-write] Disable event routing
    reserved_t<110>      _4;
    volatile uint32_t    COUNTER;              // [Read-only] Current COUNTER value
    volatile uint32_t    PRESCALER;            // [Read-write] 12 bit prescaler for COUNTER frequency (32768/(PRESCALER+1)).Must be written when RTC is stopped
    reserved_t<13>       _5;
    volatile uint32_t    CC[s];                // [Read-write] Description collection[0]: Compare register 0








    static constexpr uint32_t INTENSET_TICK = 0x1;           // Write '1' to Enable interrupt on EVENTS_TICK event
    static constexpr uint32_t INTENSET_OVRFLW = 0x2;         // Write '1' to Enable interrupt on EVENTS_OVRFLW event
    static constexpr uint32_t INTENSET_COMPARE0 = 0x10000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENSET_COMPARE1 = 0x20000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENSET_COMPARE2 = 0x40000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENSET_COMPARE3 = 0x80000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[3] event

    static constexpr uint32_t INTENCLR_TICK = 0x1;           // Write '1' to Clear interrupt on EVENTS_TICK event
    static constexpr uint32_t INTENCLR_OVRFLW = 0x2;         // Write '1' to Clear interrupt on EVENTS_OVRFLW event
    static constexpr uint32_t INTENCLR_COMPARE0 = 0x10000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENCLR_COMPARE1 = 0x20000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENCLR_COMPARE2 = 0x40000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENCLR_COMPARE3 = 0x80000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[3] event

    static constexpr uint32_t EVTEN_TICK = 0x1;           // Enable or disable event routing on EVENTS_TICK event
    static constexpr uint32_t EVTEN_OVRFLW = 0x2;         // Enable or disable event routing on EVENTS_OVRFLW event
    static constexpr uint32_t EVTEN_COMPARE0 = 0x10000;   // Enable or disable event routing on EVENTS_COMPARE[0] event
    static constexpr uint32_t EVTEN_COMPARE1 = 0x20000;   // Enable or disable event routing on EVENTS_COMPARE[1] event
    static constexpr uint32_t EVTEN_COMPARE2 = 0x40000;   // Enable or disable event routing on EVENTS_COMPARE[2] event
    static constexpr uint32_t EVTEN_COMPARE3 = 0x80000;   // Enable or disable event routing on EVENTS_COMPARE[3] event

    static constexpr uint32_t EVTENSET_TICK = 0x1;           // Write '1' to Enable event routing on EVENTS_TICK event
    static constexpr uint32_t EVTENSET_OVRFLW = 0x2;         // Write '1' to Enable event routing on EVENTS_OVRFLW event
    static constexpr uint32_t EVTENSET_COMPARE0 = 0x10000;   // Write '1' to Enable event routing on EVENTS_COMPARE[0] event
    static constexpr uint32_t EVTENSET_COMPARE1 = 0x20000;   // Write '1' to Enable event routing on EVENTS_COMPARE[1] event
    static constexpr uint32_t EVTENSET_COMPARE2 = 0x40000;   // Write '1' to Enable event routing on EVENTS_COMPARE[2] event
    static constexpr uint32_t EVTENSET_COMPARE3 = 0x80000;   // Write '1' to Enable event routing on EVENTS_COMPARE[3] event

    static constexpr uint32_t EVTENCLR_TICK = 0x1;           // Write '1' to Clear event routing on EVENTS_TICK event
    static constexpr uint32_t EVTENCLR_OVRFLW = 0x2;         // Write '1' to Clear event routing on EVENTS_OVRFLW event
    static constexpr uint32_t EVTENCLR_COMPARE0 = 0x10000;   // Write '1' to Clear event routing on EVENTS_COMPARE[0] event
    static constexpr uint32_t EVTENCLR_COMPARE1 = 0x20000;   // Write '1' to Clear event routing on EVENTS_COMPARE[1] event
    static constexpr uint32_t EVTENCLR_COMPARE2 = 0x40000;   // Write '1' to Clear event routing on EVENTS_COMPARE[2] event
    static constexpr uint32_t EVTENCLR_COMPARE3 = 0x80000;   // Write '1' to Clear event routing on EVENTS_COMPARE[3] event

    template<uint32_t X>
    static constexpr uint32_t COUNTER_COUNTER =             // Counter value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t PRESCALER_PRESCALER =           // Prescaler value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t CC[%s]_COMPARE =             // Compare value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();

    static constexpr uint8_t RTC0 = 11; // 
};

static rtc0_t& RTC0 = *reinterpret_cast<rtc0_t*>(0x4000b000);

#define HAVE_PERIPHERAL_RTC0


////
//
//    Temperature Sensor
//
////

struct temp_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start temperature measurement
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop temperature measurement
    reserved_t<62>       _0;
    volatile uint32_t    EVENTS_DATARDY;       // [Read-write] Temperature measurement complete, data ready
    reserved_t<128>      _1;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<127>      _2;
    volatile uint32_t    TEMP;                 // [Read-only] Temperature in degC




    static constexpr uint32_t INTENSET_DATARDY = 0x1;        // Write '1' to Enable interrupt on EVENTS_DATARDY event

    static constexpr uint32_t INTENCLR_DATARDY = 0x1;        // Write '1' to Clear interrupt on EVENTS_DATARDY event



    static constexpr uint8_t TEMP = 12; // 
};

static temp_t& TEMP = *reinterpret_cast<temp_t*>(0x4000c000);

#define HAVE_PERIPHERAL_TEMP


////
//
//    Random Number Generator
//
////

struct rng_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Task starting the random number generator
    volatile uint32_t    TASKS_STOP;           // [Write-only] Task stopping the random number generator
    reserved_t<62>       _0;
    volatile uint32_t    EVENTS_VALRDY;        // [Read-write] Event being generated for every new random number written to the VALUE register
    reserved_t<63>       _1;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _2;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<126>      _3;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register
    volatile uint32_t    VALUE;                // [Read-only] Output random number




    static constexpr uint32_t SHORTS_VALRDY_STOP = 0x1;    // Shortcut between EVENTS_VALRDY event and TASKS_STOP task

    static constexpr uint32_t INTENSET_VALRDY = 0x1;         // Write '1' to Enable interrupt on EVENTS_VALRDY event

    static constexpr uint32_t INTENCLR_VALRDY = 0x1;         // Write '1' to Clear interrupt on EVENTS_VALRDY event

    static constexpr uint32_t CONFIG_DERCEN = 0x1;         // Bias correction

    template<uint32_t X>
    static constexpr uint32_t VALUE_VALUE =               // Generated random number (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    static constexpr uint8_t RNG = 13; // 
};

static rng_t& RNG = *reinterpret_cast<rng_t*>(0x4000d000);

#define HAVE_PERIPHERAL_RNG


////
//
//    AES ECB Mode Encryption
//
////

struct ecb_t
{
    volatile uint32_t    TASKS_STARTECB;       // [Write-only] Start ECB block encrypt
    volatile uint32_t    TASKS_STOPECB;        // [Write-only] Abort a possible executing ECB operation
    reserved_t<62>       _0;
    volatile uint32_t    EVENTS_ENDECB;        // [Read-write] ECB block encrypt complete
    volatile uint32_t    EVENTS_ERRORECB;      // [Read-write] ECB block encrypt aborted because of a STOPECB task or due to an error
    reserved_t<127>      _1;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<126>      _2;
    volatile uint32_t    ECBDATAPTR;           // [Read-write] ECB block encrypt memory pointers





    static constexpr uint32_t INTENSET_ENDECB = 0x1;         // Write '1' to Enable interrupt on EVENTS_ENDECB event
    static constexpr uint32_t INTENSET_ERRORECB = 0x2;       // Write '1' to Enable interrupt on EVENTS_ERRORECB event

    static constexpr uint32_t INTENCLR_ENDECB = 0x1;         // Write '1' to Clear interrupt on EVENTS_ENDECB event
    static constexpr uint32_t INTENCLR_ERRORECB = 0x2;       // Write '1' to Clear interrupt on EVENTS_ERRORECB event



    static constexpr uint8_t ECB = 14; // 
};

static ecb_t& ECB = *reinterpret_cast<ecb_t*>(0x4000e000);

#define HAVE_PERIPHERAL_ECB


////
//
//    AES CCM Mode Encryption
//
////

struct ccm_t
{
    volatile uint32_t    TASKS_KSGEN;          // [Write-only] Start generation of key-stream. This operation will stop by itself when completed.
    volatile uint32_t    TASKS_CRYPT;          // [Write-only] Start encryption/decryption. This operation will stop by itself when completed.
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop encryption/decryption
    reserved_t<61>       _0;
    volatile uint32_t    EVENTS_ENDKSGEN;      // [Read-write] Key-stream generation complete
    volatile uint32_t    EVENTS_ENDCRYPT;      // [Read-write] Encrypt/decrypt complete
    volatile uint32_t    EVENTS_ERROR;         // [Read-write] CCM error event
    reserved_t<61>       _1;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _2;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<61>       _3;
    volatile uint32_t    MICSTATUS;            // [Read-only] MIC check result
    reserved_t<63>       _4;
    volatile uint32_t    ENABLE;               // [Read-write] Enable
    volatile uint32_t    MODE;                 // [Read-write] Operation mode
    volatile uint32_t    CNFPTR;               // [Read-write] Pointer to data structure holding AES key and NONCE vector
    volatile uint32_t    INPTR;                // [Read-write] Input pointer
    volatile uint32_t    OUTPTR;               // [Read-write] Output pointer
    volatile uint32_t    SCRATCHPTR;           // [Read-write] Pointer to data area used for temporary storage







    static constexpr uint32_t SHORTS_ENDKSGEN_CRYPT = 0x1; // Shortcut between EVENTS_ENDKSGEN event and TASKS_CRYPT task

    static constexpr uint32_t INTENSET_ENDKSGEN = 0x1;       // Write '1' to Enable interrupt on EVENTS_ENDKSGEN event
    static constexpr uint32_t INTENSET_ENDCRYPT = 0x2;       // Write '1' to Enable interrupt on EVENTS_ENDCRYPT event
    static constexpr uint32_t INTENSET_ERROR = 0x4;          // Write '1' to Enable interrupt on EVENTS_ERROR event

    static constexpr uint32_t INTENCLR_ENDKSGEN = 0x1;       // Write '1' to Clear interrupt on EVENTS_ENDKSGEN event
    static constexpr uint32_t INTENCLR_ENDCRYPT = 0x2;       // Write '1' to Clear interrupt on EVENTS_ENDCRYPT event
    static constexpr uint32_t INTENCLR_ERROR = 0x4;          // Write '1' to Clear interrupt on EVENTS_ERROR event

    static constexpr uint32_t MICSTATUS_MICSTATUS = 0x1;      // The result of the MIC check performed during the previous decryption operation

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable CCM (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    static constexpr uint32_t MODE_MODE = 0x1;           // The mode of operation to be used
    static constexpr uint32_t MODE_DATARATE = 0x10000;   // Data rate that the CCM shall run in synch with
    static constexpr uint32_t MODE_LENGTH = 0x1000000;   // Packet length configuration
    static const uint32_t MODE_RESET_VALUE = 0x1;









    static constexpr uint8_t CCM_AAR = 15; // 
};

static ccm_t& CCM = *reinterpret_cast<ccm_t*>(0x4000f000);

#define HAVE_PERIPHERAL_CCM


////
//
//    Accelerated Address Resolver
//
////

struct aar_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start resolving addresses based on IRKs specified in the IRK data structure
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop resolving addresses
    reserved_t<61>       _1;
    volatile uint32_t    EVENTS_END;           // [Read-write] Address resolution procedure complete
    volatile uint32_t    EVENTS_RESOLVED;      // [Read-write] Address resolved
    volatile uint32_t    EVENTS_NOTRESOLVED;   // [Read-write] Address not resolved
    reserved_t<126>      _2;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<61>       _3;
    volatile uint32_t    STATUS;               // [Read-only] Resolution status
    reserved_t<63>       _4;
    volatile uint32_t    ENABLE;               // [Read-write] Enable AAR
    volatile uint32_t    NIRK;                 // [Read-write] Number of IRKs
    volatile uint32_t    IRKPTR;               // [Read-write] Pointer to IRK data structure
    reserved_t<1>        _5;
    volatile uint32_t    ADDRPTR;              // [Read-write] Pointer to the resolvable address
    volatile uint32_t    SCRATCHPTR;           // [Read-write] Pointer to data area used for temporary storage






    static constexpr uint32_t INTENSET_END = 0x1;            // Write '1' to Enable interrupt on EVENTS_END event
    static constexpr uint32_t INTENSET_RESOLVED = 0x2;       // Write '1' to Enable interrupt on EVENTS_RESOLVED event
    static constexpr uint32_t INTENSET_NOTRESOLVED = 0x4;    // Write '1' to Enable interrupt on EVENTS_NOTRESOLVED event

    static constexpr uint32_t INTENCLR_END = 0x1;            // Write '1' to Clear interrupt on EVENTS_END event
    static constexpr uint32_t INTENCLR_RESOLVED = 0x2;       // Write '1' to Clear interrupt on EVENTS_RESOLVED event
    static constexpr uint32_t INTENCLR_NOTRESOLVED = 0x4;    // Write '1' to Clear interrupt on EVENTS_NOTRESOLVED event

    template<uint32_t X>
    static constexpr uint32_t STATUS_STATUS =              // The IRK that was used last time an address was resolved (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable AAR (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t NIRK_NIRK =                // Number of Identity root keys available in the IRK data structure (5 bits)
        bit_field_t<0, 0x1f>::value<X>();
    static const uint32_t NIRK_RESET_VALUE = 0x1;







    static constexpr uint8_t CCM_AAR = 15; // 
};

static aar_t& AAR = *reinterpret_cast<aar_t*>(0x4000f000);

#define HAVE_PERIPHERAL_AAR


////
//
//    Watchdog Timer
//
////

struct wdt_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start the watchdog
    reserved_t<63>       _0;
    volatile uint32_t    EVENTS_TIMEOUT;       // [Read-write] Watchdog timeout
    reserved_t<128>      _1;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<61>       _2;
    volatile uint32_t    RUNSTATUS;            // [Read-only] Run status
    volatile uint32_t    REQSTATUS;            // [Read-only] Request status
    reserved_t<63>       _3;
    volatile uint32_t    CRV;                  // [Read-write] Counter reload value
    volatile uint32_t    RREN;                 // [Read-write] Enable register for reload request registers
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register
    reserved_t<60>       _4;
    volatile uint32_t    RR[s];                // [Write-only] Description collection[0]: Reload request 0



    static constexpr uint32_t INTENSET_TIMEOUT = 0x1;        // Write '1' to Enable interrupt on EVENTS_TIMEOUT event

    static constexpr uint32_t INTENCLR_TIMEOUT = 0x1;        // Write '1' to Clear interrupt on EVENTS_TIMEOUT event

    static constexpr uint32_t RUNSTATUS_RUNSTATUS = 0x1;      // Indicates whether or not the watchdog is running

    static constexpr uint32_t REQSTATUS_RR0 = 0x1;            // Request status for RR[0] register
    static constexpr uint32_t REQSTATUS_RR1 = 0x2;            // Request status for RR[1] register
    static constexpr uint32_t REQSTATUS_RR2 = 0x4;            // Request status for RR[2] register
    static constexpr uint32_t REQSTATUS_RR3 = 0x8;            // Request status for RR[3] register
    static constexpr uint32_t REQSTATUS_RR4 = 0x10;           // Request status for RR[4] register
    static constexpr uint32_t REQSTATUS_RR5 = 0x20;           // Request status for RR[5] register
    static constexpr uint32_t REQSTATUS_RR6 = 0x40;           // Request status for RR[6] register
    static constexpr uint32_t REQSTATUS_RR7 = 0x80;           // Request status for RR[7] register
    static const uint32_t REQSTATUS_RESET_VALUE = 0x1;


    static const uint32_t CRV_RESET_VALUE = 0xffffffff;

    static constexpr uint32_t RREN_RR0 = 0x1;            // Enable or disable RR[0] register
    static constexpr uint32_t RREN_RR1 = 0x2;            // Enable or disable RR[1] register
    static constexpr uint32_t RREN_RR2 = 0x4;            // Enable or disable RR[2] register
    static constexpr uint32_t RREN_RR3 = 0x8;            // Enable or disable RR[3] register
    static constexpr uint32_t RREN_RR4 = 0x10;           // Enable or disable RR[4] register
    static constexpr uint32_t RREN_RR5 = 0x20;           // Enable or disable RR[5] register
    static constexpr uint32_t RREN_RR6 = 0x40;           // Enable or disable RR[6] register
    static constexpr uint32_t RREN_RR7 = 0x80;           // Enable or disable RR[7] register
    static const uint32_t RREN_RESET_VALUE = 0x1;

    static constexpr uint32_t CONFIG_SLEEP = 0x1;          // Configure the watchdog to either be paused, or kept running, while the CPU is sleeping
    static constexpr uint32_t CONFIG_HALT = 0x8;           // Configure the watchdog to either be paused, or kept running, while the CPU is halted by the debugger
    static const uint32_t CONFIG_RESET_VALUE = 0x1;



    static constexpr uint8_t WDT = 16; // 
};

static wdt_t& WDT = *reinterpret_cast<wdt_t*>(0x40010000);

#define HAVE_PERIPHERAL_WDT


////
//
//    Real time counter 0
//
////

struct rtc1_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start RTC COUNTER
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop RTC COUNTER
    volatile uint32_t    TASKS_CLEAR;          // [Write-only] Clear RTC COUNTER
    volatile uint32_t    TASKS_TRIGOVRFLW;     // [Write-only] Set COUNTER to 0xFFFFF0
    reserved_t<60>       _0;
    volatile uint32_t    EVENTS_TICK;          // [Read-write] Event on COUNTER increment
    volatile uint32_t    EVENTS_OVRFLW;        // [Read-write] Event on COUNTER overflow
    reserved_t<14>       _1;
    volatile uint32_t    EVENTS_COMPARE[s];    // [Read-write] Description collection[0]: Compare event on CC[0] match
    reserved_t<112>      _2;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<13>       _3;
    volatile uint32_t    EVTEN;                // [Read-write] Enable or disable event routing
    volatile uint32_t    EVTENSET;             // [Read-write] Enable event routing
    volatile uint32_t    EVTENCLR;             // [Read-write] Disable event routing
    reserved_t<110>      _4;
    volatile uint32_t    COUNTER;              // [Read-only] Current COUNTER value
    volatile uint32_t    PRESCALER;            // [Read-write] 12 bit prescaler for COUNTER frequency (32768/(PRESCALER+1)).Must be written when RTC is stopped
    reserved_t<13>       _5;
    volatile uint32_t    CC[s];                // [Read-write] Description collection[0]: Compare register 0








    static constexpr uint32_t INTENSET_TICK = 0x1;           // Write '1' to Enable interrupt on EVENTS_TICK event
    static constexpr uint32_t INTENSET_OVRFLW = 0x2;         // Write '1' to Enable interrupt on EVENTS_OVRFLW event
    static constexpr uint32_t INTENSET_COMPARE0 = 0x10000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENSET_COMPARE1 = 0x20000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENSET_COMPARE2 = 0x40000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENSET_COMPARE3 = 0x80000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[3] event

    static constexpr uint32_t INTENCLR_TICK = 0x1;           // Write '1' to Clear interrupt on EVENTS_TICK event
    static constexpr uint32_t INTENCLR_OVRFLW = 0x2;         // Write '1' to Clear interrupt on EVENTS_OVRFLW event
    static constexpr uint32_t INTENCLR_COMPARE0 = 0x10000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENCLR_COMPARE1 = 0x20000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENCLR_COMPARE2 = 0x40000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENCLR_COMPARE3 = 0x80000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[3] event

    static constexpr uint32_t EVTEN_TICK = 0x1;           // Enable or disable event routing on EVENTS_TICK event
    static constexpr uint32_t EVTEN_OVRFLW = 0x2;         // Enable or disable event routing on EVENTS_OVRFLW event
    static constexpr uint32_t EVTEN_COMPARE0 = 0x10000;   // Enable or disable event routing on EVENTS_COMPARE[0] event
    static constexpr uint32_t EVTEN_COMPARE1 = 0x20000;   // Enable or disable event routing on EVENTS_COMPARE[1] event
    static constexpr uint32_t EVTEN_COMPARE2 = 0x40000;   // Enable or disable event routing on EVENTS_COMPARE[2] event
    static constexpr uint32_t EVTEN_COMPARE3 = 0x80000;   // Enable or disable event routing on EVENTS_COMPARE[3] event

    static constexpr uint32_t EVTENSET_TICK = 0x1;           // Write '1' to Enable event routing on EVENTS_TICK event
    static constexpr uint32_t EVTENSET_OVRFLW = 0x2;         // Write '1' to Enable event routing on EVENTS_OVRFLW event
    static constexpr uint32_t EVTENSET_COMPARE0 = 0x10000;   // Write '1' to Enable event routing on EVENTS_COMPARE[0] event
    static constexpr uint32_t EVTENSET_COMPARE1 = 0x20000;   // Write '1' to Enable event routing on EVENTS_COMPARE[1] event
    static constexpr uint32_t EVTENSET_COMPARE2 = 0x40000;   // Write '1' to Enable event routing on EVENTS_COMPARE[2] event
    static constexpr uint32_t EVTENSET_COMPARE3 = 0x80000;   // Write '1' to Enable event routing on EVENTS_COMPARE[3] event

    static constexpr uint32_t EVTENCLR_TICK = 0x1;           // Write '1' to Clear event routing on EVENTS_TICK event
    static constexpr uint32_t EVTENCLR_OVRFLW = 0x2;         // Write '1' to Clear event routing on EVENTS_OVRFLW event
    static constexpr uint32_t EVTENCLR_COMPARE0 = 0x10000;   // Write '1' to Clear event routing on EVENTS_COMPARE[0] event
    static constexpr uint32_t EVTENCLR_COMPARE1 = 0x20000;   // Write '1' to Clear event routing on EVENTS_COMPARE[1] event
    static constexpr uint32_t EVTENCLR_COMPARE2 = 0x40000;   // Write '1' to Clear event routing on EVENTS_COMPARE[2] event
    static constexpr uint32_t EVTENCLR_COMPARE3 = 0x80000;   // Write '1' to Clear event routing on EVENTS_COMPARE[3] event

    template<uint32_t X>
    static constexpr uint32_t COUNTER_COUNTER =             // Counter value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t PRESCALER_PRESCALER =           // Prescaler value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t CC[%s]_COMPARE =             // Compare value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();

    static constexpr uint8_t RTC1 = 17; // 
};

static rtc1_t& RTC1 = *reinterpret_cast<rtc1_t*>(0x40011000);

#define HAVE_PERIPHERAL_RTC1


////
//
//    Quadrature Decoder
//
////

struct qdec_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Task starting the quadrature decoder
    volatile uint32_t    TASKS_STOP;           // [Write-only] Task stopping the quadrature decoder
    volatile uint32_t    TASKS_READCLRACC;     // [Write-only] Read and clear ACC and ACCDBL
    volatile uint32_t    TASKS_RDCLRACC;       // [Write-only] Read and clear ACC
    volatile uint32_t    TASKS_RDCLRDBL;       // [Write-only] Read and clear ACCDBL
    reserved_t<59>       _0;
    volatile uint32_t    EVENTS_SAMPLERDY;     // [Read-write] Event being generated for every new sample value written to the SAMPLE register
    volatile uint32_t    EVENTS_REPORTRDY;     // [Read-write] Non-null report ready
    volatile uint32_t    EVENTS_ACCOF;         // [Read-write] ACC or ACCDBL register overflow
    volatile uint32_t    EVENTS_DBLRDY;        // [Read-write] Double displacement(s) detected
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] QDEC has been stopped
    reserved_t<59>       _1;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _2;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<125>      _3;
    volatile uint32_t    ENABLE;               // [Read-write] Enable the quadrature decoder
    volatile uint32_t    LEDPOL;               // [Read-write] LED output pin polarity
    volatile uint32_t    SAMPLEPER;            // [Read-write] Sample period
    volatile uint32_t    SAMPLE;               // [Read-only] Motion sample value
    volatile uint32_t    REPORTPER;            // [Read-write] Number of samples to be taken before REPORTRDY and DBLRDY events can be generated
    volatile uint32_t    ACC;                  // [Read-only] Register accumulating the valid transitions
    volatile uint32_t    ACCREAD;              // [Read-only] Snapshot of the ACC register, updated by the READCLRACC or RDCLRACC task
    reserved_t<3>        _4;
    volatile uint32_t    DBFEN;                // [Read-write] Enable input debounce filters
    reserved_t<5>        _5;
    volatile uint32_t    LEDPRE;               // [Read-write] Time period the LED is switched ON prior to sampling
    volatile uint32_t    ACCDBL;               // [Read-only] Register accumulating the number of detected double transitions
    volatile uint32_t    ACCDBLREAD;           // [Read-only] Snapshot of the ACCDBL, updated by the READCLRACC or RDCLRDBL task











    static constexpr uint32_t SHORTS_REPORTRDY_READCLRACC = 0x1;// Shortcut between EVENTS_REPORTRDY event and TASKS_READCLRACC task
    static constexpr uint32_t SHORTS_SAMPLERDY_STOP = 0x2; // Shortcut between EVENTS_SAMPLERDY event and TASKS_STOP task
    static constexpr uint32_t SHORTS_REPORTRDY_RDCLRACC = 0x4;// Shortcut between EVENTS_REPORTRDY event and TASKS_RDCLRACC task
    static constexpr uint32_t SHORTS_REPORTRDY_STOP = 0x8; // Shortcut between EVENTS_REPORTRDY event and TASKS_STOP task
    static constexpr uint32_t SHORTS_DBLRDY_RDCLRDBL = 0x10;// Shortcut between EVENTS_DBLRDY event and TASKS_RDCLRDBL task
    static constexpr uint32_t SHORTS_DBLRDY_STOP = 0x20;   // Shortcut between EVENTS_DBLRDY event and TASKS_STOP task
    static constexpr uint32_t SHORTS_SAMPLERDY_READCLRACC = 0x40;// Shortcut between EVENTS_SAMPLERDY event and TASKS_READCLRACC task

    static constexpr uint32_t INTENSET_SAMPLERDY = 0x1;      // Write '1' to Enable interrupt on EVENTS_SAMPLERDY event
    static constexpr uint32_t INTENSET_REPORTRDY = 0x2;      // Write '1' to Enable interrupt on EVENTS_REPORTRDY event
    static constexpr uint32_t INTENSET_ACCOF = 0x4;          // Write '1' to Enable interrupt on EVENTS_ACCOF event
    static constexpr uint32_t INTENSET_DBLRDY = 0x8;         // Write '1' to Enable interrupt on EVENTS_DBLRDY event
    static constexpr uint32_t INTENSET_STOPPED = 0x10;       // Write '1' to Enable interrupt on EVENTS_STOPPED event

    static constexpr uint32_t INTENCLR_SAMPLERDY = 0x1;      // Write '1' to Clear interrupt on EVENTS_SAMPLERDY event
    static constexpr uint32_t INTENCLR_REPORTRDY = 0x2;      // Write '1' to Clear interrupt on EVENTS_REPORTRDY event
    static constexpr uint32_t INTENCLR_ACCOF = 0x4;          // Write '1' to Clear interrupt on EVENTS_ACCOF event
    static constexpr uint32_t INTENCLR_DBLRDY = 0x8;         // Write '1' to Clear interrupt on EVENTS_DBLRDY event
    static constexpr uint32_t INTENCLR_STOPPED = 0x10;       // Write '1' to Clear interrupt on EVENTS_STOPPED event

    static constexpr uint32_t ENABLE_ENABLE = 0x1;         // Enable or disable the quadrature decoder

    static constexpr uint32_t LEDPOL_LEDPOL = 0x1;         // LED output pin polarity

    template<uint32_t X>
    static constexpr uint32_t SAMPLEPER_SAMPLEPER =           // Sample period. The SAMPLE register will be updated for every new sample (4 bits)
        bit_field_t<0, 0xf>::value<X>();



    template<uint32_t X>
    static constexpr uint32_t REPORTPER_REPORTPER =           // Specifies the number of samples to be accumulated in the ACC register before the REPORTRDY and DBLRDY events can be generated (4 bits)
        bit_field_t<0, 0xf>::value<X>();





    static constexpr uint32_t DBFEN_DBFEN = 0x1;          // Enable input debounce filters

    template<uint32_t X>
    static constexpr uint32_t LEDPRE_LEDPRE =              // Period in us the LED is switched on prior to sampling (9 bits)
        bit_field_t<0, 0x1ff>::value<X>();
    static const uint32_t LEDPRE_RESET_VALUE = 0x10;

    template<uint32_t X>
    static constexpr uint32_t ACCDBL_ACCDBL =              // Register accumulating the number of detected double or illegal transitions. ( SAMPLE = 2 ). (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t ACCDBLREAD_ACCDBLREAD =          // Snapshot of the ACCDBL register. This field is updated when the READCLRACC or RDCLRDBL task is triggered. (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    static constexpr uint8_t QDEC = 18; // 
};

static qdec_t& QDEC = *reinterpret_cast<qdec_t*>(0x40012000);

#define HAVE_PERIPHERAL_QDEC


////
//
//    Comparator
//
////

struct comp_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start comparator
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop comparator
    volatile uint32_t    TASKS_SAMPLE;         // [Write-only] Sample comparator value
    reserved_t<61>       _0;
    volatile uint32_t    EVENTS_READY;         // [Read-write] COMP is ready and output is valid
    volatile uint32_t    EVENTS_DOWN;          // [Read-write] Downward crossing
    volatile uint32_t    EVENTS_UP;            // [Read-write] Upward crossing
    volatile uint32_t    EVENTS_CROSS;         // [Read-write] Downward or upward crossing
    reserved_t<60>       _1;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<63>       _2;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<61>       _3;
    volatile uint32_t    RESULT;               // [Read-only] Compare result
    reserved_t<63>       _4;
    volatile uint32_t    ENABLE;               // [Read-write] COMP enable
    volatile uint32_t    PSEL;                 // [Read-write] Pin select
    volatile uint32_t    REFSEL;               // [Read-write] Reference source select
    volatile uint32_t    EXTREFSEL;            // [Read-write] External reference select
    reserved_t<8>        _5;
    volatile uint32_t    TH;                   // [Read-write] Threshold configuration for hysteresis unit
    volatile uint32_t    MODE;                 // [Read-write] Mode configuration
    volatile uint32_t    HYST;                 // [Read-write] Comparator hysteresis enable
    volatile uint32_t    ISOURCE;              // [Read-write] Current source select on analog input








    static constexpr uint32_t SHORTS_READY_SAMPLE = 0x1;   // Shortcut between EVENTS_READY event and TASKS_SAMPLE task
    static constexpr uint32_t SHORTS_READY_STOP = 0x2;     // Shortcut between EVENTS_READY event and TASKS_STOP task
    static constexpr uint32_t SHORTS_DOWN_STOP = 0x4;      // Shortcut between EVENTS_DOWN event and TASKS_STOP task
    static constexpr uint32_t SHORTS_UP_STOP = 0x8;        // Shortcut between EVENTS_UP event and TASKS_STOP task
    static constexpr uint32_t SHORTS_CROSS_STOP = 0x10;    // Shortcut between EVENTS_CROSS event and TASKS_STOP task

    static constexpr uint32_t INTEN_READY = 0x1;          // Enable or disable interrupt on EVENTS_READY event
    static constexpr uint32_t INTEN_DOWN = 0x2;           // Enable or disable interrupt on EVENTS_DOWN event
    static constexpr uint32_t INTEN_UP = 0x4;             // Enable or disable interrupt on EVENTS_UP event
    static constexpr uint32_t INTEN_CROSS = 0x8;          // Enable or disable interrupt on EVENTS_CROSS event

    static constexpr uint32_t INTENSET_READY = 0x1;          // Write '1' to Enable interrupt on EVENTS_READY event
    static constexpr uint32_t INTENSET_DOWN = 0x2;           // Write '1' to Enable interrupt on EVENTS_DOWN event
    static constexpr uint32_t INTENSET_UP = 0x4;             // Write '1' to Enable interrupt on EVENTS_UP event
    static constexpr uint32_t INTENSET_CROSS = 0x8;          // Write '1' to Enable interrupt on EVENTS_CROSS event

    static constexpr uint32_t INTENCLR_READY = 0x1;          // Write '1' to Clear interrupt on EVENTS_READY event
    static constexpr uint32_t INTENCLR_DOWN = 0x2;           // Write '1' to Clear interrupt on EVENTS_DOWN event
    static constexpr uint32_t INTENCLR_UP = 0x4;             // Write '1' to Clear interrupt on EVENTS_UP event
    static constexpr uint32_t INTENCLR_CROSS = 0x8;          // Write '1' to Clear interrupt on EVENTS_CROSS event

    static constexpr uint32_t RESULT_RESULT = 0x1;         // Result of last compare. Decision point SAMPLE task.

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable COMP (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t PSEL_PSEL =                // Analog pin select (3 bits)
        bit_field_t<0, 0x7>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t REFSEL_REFSEL =              // Reference select (3 bits)
        bit_field_t<0, 0x7>::value<X>();

    static constexpr uint32_t EXTREFSEL_EXTREFSEL = 0x1;      // External analog reference select

    template<uint32_t X>
    static constexpr uint32_t TH_THUP =                // VUP = (THUP+1)/64*VREF (6 bits)
        bit_field_t<0, 0x3f>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t TH_THDOWN =              // VDOWN = (THDOWN+1)/64*VREF (6 bits)
        bit_field_t<8, 0x3f>::value<X>();
    static const uint32_t TH_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t MODE_SP =                  // Speed and power mode (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static constexpr uint32_t MODE_MAIN = 0x100;         // Main operation mode

    static constexpr uint32_t HYST_HYST = 0x1;           // Comparator hysteresis

    template<uint32_t X>
    static constexpr uint32_t ISOURCE_ISOURCE =             // Comparator hysteresis (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    static constexpr uint8_t COMP_LPCOMP = 19; // 
};

static comp_t& COMP = *reinterpret_cast<comp_t*>(0x40013000);

#define HAVE_PERIPHERAL_COMP


////
//
//    Low Power Comparator
//
////

struct lpcomp_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start comparator
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop comparator
    volatile uint32_t    TASKS_SAMPLE;         // [Write-only] Sample comparator value
    reserved_t<61>       _0;
    volatile uint32_t    EVENTS_READY;         // [Read-write] LPCOMP is ready and output is valid
    volatile uint32_t    EVENTS_DOWN;          // [Read-write] Downward crossing
    volatile uint32_t    EVENTS_UP;            // [Read-write] Upward crossing
    volatile uint32_t    EVENTS_CROSS;         // [Read-write] Downward or upward crossing
    reserved_t<60>       _1;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _2;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<61>       _3;
    volatile uint32_t    RESULT;               // [Read-only] Compare result
    reserved_t<63>       _4;
    volatile uint32_t    ENABLE;               // [Read-write] Enable LPCOMP
    volatile uint32_t    PSEL;                 // [Read-write] Input pin select
    volatile uint32_t    REFSEL;               // [Read-write] Reference select
    volatile uint32_t    EXTREFSEL;            // [Read-write] External reference select
    reserved_t<4>        _5;
    volatile uint32_t    ANADETECT;            // [Read-write] Analog detect configuration
    reserved_t<5>        _6;
    volatile uint32_t    HYST;                 // [Read-write] Comparator hysteresis enable








    static constexpr uint32_t SHORTS_READY_SAMPLE = 0x1;   // Shortcut between EVENTS_READY event and TASKS_SAMPLE task
    static constexpr uint32_t SHORTS_READY_STOP = 0x2;     // Shortcut between EVENTS_READY event and TASKS_STOP task
    static constexpr uint32_t SHORTS_DOWN_STOP = 0x4;      // Shortcut between EVENTS_DOWN event and TASKS_STOP task
    static constexpr uint32_t SHORTS_UP_STOP = 0x8;        // Shortcut between EVENTS_UP event and TASKS_STOP task
    static constexpr uint32_t SHORTS_CROSS_STOP = 0x10;    // Shortcut between EVENTS_CROSS event and TASKS_STOP task

    static constexpr uint32_t INTENSET_READY = 0x1;          // Write '1' to Enable interrupt on EVENTS_READY event
    static constexpr uint32_t INTENSET_DOWN = 0x2;           // Write '1' to Enable interrupt on EVENTS_DOWN event
    static constexpr uint32_t INTENSET_UP = 0x4;             // Write '1' to Enable interrupt on EVENTS_UP event
    static constexpr uint32_t INTENSET_CROSS = 0x8;          // Write '1' to Enable interrupt on EVENTS_CROSS event

    static constexpr uint32_t INTENCLR_READY = 0x1;          // Write '1' to Clear interrupt on EVENTS_READY event
    static constexpr uint32_t INTENCLR_DOWN = 0x2;           // Write '1' to Clear interrupt on EVENTS_DOWN event
    static constexpr uint32_t INTENCLR_UP = 0x4;             // Write '1' to Clear interrupt on EVENTS_UP event
    static constexpr uint32_t INTENCLR_CROSS = 0x8;          // Write '1' to Clear interrupt on EVENTS_CROSS event

    static constexpr uint32_t RESULT_RESULT = 0x1;         // Result of last compare. Decision point SAMPLE task.

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable LPCOMP (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t PSEL_PSEL =                // Analog pin select (3 bits)
        bit_field_t<0, 0x7>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t REFSEL_REFSEL =              // Reference select (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    static constexpr uint32_t EXTREFSEL_EXTREFSEL = 0x1;      // External analog reference select

    template<uint32_t X>
    static constexpr uint32_t ANADETECT_ANADETECT =           // Analog detect configuration (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    static constexpr uint32_t HYST_HYST = 0x1;           // Comparator hysteresis enable

    static constexpr uint8_t COMP_LPCOMP = 19; // 
};

static lpcomp_t& LPCOMP = *reinterpret_cast<lpcomp_t*>(0x40013000);

#define HAVE_PERIPHERAL_LPCOMP


////
//
//    Software interrupt 0
//
////

struct swi0_t
{
    volatile uint32_t    UNUSED;               // [Read-only] Unused.

    static const uint32_t UNUSED_RESET_VALUE = 0x0;

    static constexpr uint8_t SWI0_EGU0 = 20; // 
};

static swi0_t& SWI0 = *reinterpret_cast<swi0_t*>(0x40014000);

#define HAVE_PERIPHERAL_SWI0


////
//
//    Event Generator Unit 0
//
////

struct egu0_t
{
    volatile uint32_t    TASKS_TRIGGER[s];     // [Write-only] Description collection[0]: Trigger 0 for triggering the corresponding TRIGGERED[0] event
    reserved_t<63>       _0;
    volatile uint32_t    EVENTS_TRIGGERED[s];  // [Read-write] Description collection[0]: Event number 0 generated by triggering the corresponding TRIGGER[0] task
    reserved_t<127>      _1;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt



    static constexpr uint32_t INTEN_TRIGGERED0 = 0x1;     // Enable or disable interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTEN_TRIGGERED1 = 0x2;     // Enable or disable interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTEN_TRIGGERED2 = 0x4;     // Enable or disable interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTEN_TRIGGERED3 = 0x8;     // Enable or disable interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTEN_TRIGGERED4 = 0x10;    // Enable or disable interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTEN_TRIGGERED5 = 0x20;    // Enable or disable interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTEN_TRIGGERED6 = 0x40;    // Enable or disable interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTEN_TRIGGERED7 = 0x80;    // Enable or disable interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTEN_TRIGGERED8 = 0x100;   // Enable or disable interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTEN_TRIGGERED9 = 0x200;   // Enable or disable interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTEN_TRIGGERED10 = 0x400;  // Enable or disable interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTEN_TRIGGERED11 = 0x800;  // Enable or disable interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTEN_TRIGGERED12 = 0x1000; // Enable or disable interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTEN_TRIGGERED13 = 0x2000; // Enable or disable interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTEN_TRIGGERED14 = 0x4000; // Enable or disable interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTEN_TRIGGERED15 = 0x8000; // Enable or disable interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint32_t INTENSET_TRIGGERED0 = 0x1;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTENSET_TRIGGERED1 = 0x2;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTENSET_TRIGGERED2 = 0x4;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTENSET_TRIGGERED3 = 0x8;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTENSET_TRIGGERED4 = 0x10;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTENSET_TRIGGERED5 = 0x20;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTENSET_TRIGGERED6 = 0x40;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTENSET_TRIGGERED7 = 0x80;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTENSET_TRIGGERED8 = 0x100;   // Write '1' to Enable interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTENSET_TRIGGERED9 = 0x200;   // Write '1' to Enable interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTENSET_TRIGGERED10 = 0x400;  // Write '1' to Enable interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTENSET_TRIGGERED11 = 0x800;  // Write '1' to Enable interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTENSET_TRIGGERED12 = 0x1000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTENSET_TRIGGERED13 = 0x2000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTENSET_TRIGGERED14 = 0x4000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTENSET_TRIGGERED15 = 0x8000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint32_t INTENCLR_TRIGGERED0 = 0x1;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTENCLR_TRIGGERED1 = 0x2;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTENCLR_TRIGGERED2 = 0x4;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTENCLR_TRIGGERED3 = 0x8;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTENCLR_TRIGGERED4 = 0x10;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTENCLR_TRIGGERED5 = 0x20;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTENCLR_TRIGGERED6 = 0x40;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTENCLR_TRIGGERED7 = 0x80;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTENCLR_TRIGGERED8 = 0x100;   // Write '1' to Clear interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTENCLR_TRIGGERED9 = 0x200;   // Write '1' to Clear interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTENCLR_TRIGGERED10 = 0x400;  // Write '1' to Clear interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTENCLR_TRIGGERED11 = 0x800;  // Write '1' to Clear interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTENCLR_TRIGGERED12 = 0x1000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTENCLR_TRIGGERED13 = 0x2000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTENCLR_TRIGGERED14 = 0x4000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTENCLR_TRIGGERED15 = 0x8000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint8_t SWI0_EGU0 = 20; // 
};

static egu0_t& EGU0 = *reinterpret_cast<egu0_t*>(0x40014000);

#define HAVE_PERIPHERAL_EGU0


////
//
//    Software interrupt 0
//
////

struct swi1_t
{
    volatile uint32_t    UNUSED;               // [Read-only] Unused.

    static const uint32_t UNUSED_RESET_VALUE = 0x0;

    static constexpr uint8_t SWI1_EGU1 = 21; // 
};

static swi1_t& SWI1 = *reinterpret_cast<swi1_t*>(0x40015000);

#define HAVE_PERIPHERAL_SWI1


////
//
//    Event Generator Unit 0
//
////

struct egu1_t
{
    volatile uint32_t    TASKS_TRIGGER[s];     // [Write-only] Description collection[0]: Trigger 0 for triggering the corresponding TRIGGERED[0] event
    reserved_t<63>       _0;
    volatile uint32_t    EVENTS_TRIGGERED[s];  // [Read-write] Description collection[0]: Event number 0 generated by triggering the corresponding TRIGGER[0] task
    reserved_t<127>      _1;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt



    static constexpr uint32_t INTEN_TRIGGERED0 = 0x1;     // Enable or disable interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTEN_TRIGGERED1 = 0x2;     // Enable or disable interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTEN_TRIGGERED2 = 0x4;     // Enable or disable interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTEN_TRIGGERED3 = 0x8;     // Enable or disable interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTEN_TRIGGERED4 = 0x10;    // Enable or disable interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTEN_TRIGGERED5 = 0x20;    // Enable or disable interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTEN_TRIGGERED6 = 0x40;    // Enable or disable interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTEN_TRIGGERED7 = 0x80;    // Enable or disable interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTEN_TRIGGERED8 = 0x100;   // Enable or disable interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTEN_TRIGGERED9 = 0x200;   // Enable or disable interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTEN_TRIGGERED10 = 0x400;  // Enable or disable interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTEN_TRIGGERED11 = 0x800;  // Enable or disable interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTEN_TRIGGERED12 = 0x1000; // Enable or disable interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTEN_TRIGGERED13 = 0x2000; // Enable or disable interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTEN_TRIGGERED14 = 0x4000; // Enable or disable interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTEN_TRIGGERED15 = 0x8000; // Enable or disable interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint32_t INTENSET_TRIGGERED0 = 0x1;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTENSET_TRIGGERED1 = 0x2;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTENSET_TRIGGERED2 = 0x4;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTENSET_TRIGGERED3 = 0x8;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTENSET_TRIGGERED4 = 0x10;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTENSET_TRIGGERED5 = 0x20;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTENSET_TRIGGERED6 = 0x40;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTENSET_TRIGGERED7 = 0x80;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTENSET_TRIGGERED8 = 0x100;   // Write '1' to Enable interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTENSET_TRIGGERED9 = 0x200;   // Write '1' to Enable interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTENSET_TRIGGERED10 = 0x400;  // Write '1' to Enable interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTENSET_TRIGGERED11 = 0x800;  // Write '1' to Enable interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTENSET_TRIGGERED12 = 0x1000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTENSET_TRIGGERED13 = 0x2000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTENSET_TRIGGERED14 = 0x4000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTENSET_TRIGGERED15 = 0x8000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint32_t INTENCLR_TRIGGERED0 = 0x1;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTENCLR_TRIGGERED1 = 0x2;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTENCLR_TRIGGERED2 = 0x4;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTENCLR_TRIGGERED3 = 0x8;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTENCLR_TRIGGERED4 = 0x10;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTENCLR_TRIGGERED5 = 0x20;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTENCLR_TRIGGERED6 = 0x40;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTENCLR_TRIGGERED7 = 0x80;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTENCLR_TRIGGERED8 = 0x100;   // Write '1' to Clear interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTENCLR_TRIGGERED9 = 0x200;   // Write '1' to Clear interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTENCLR_TRIGGERED10 = 0x400;  // Write '1' to Clear interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTENCLR_TRIGGERED11 = 0x800;  // Write '1' to Clear interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTENCLR_TRIGGERED12 = 0x1000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTENCLR_TRIGGERED13 = 0x2000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTENCLR_TRIGGERED14 = 0x4000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTENCLR_TRIGGERED15 = 0x8000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint8_t SWI1_EGU1 = 21; // 
};

static egu1_t& EGU1 = *reinterpret_cast<egu1_t*>(0x40015000);

#define HAVE_PERIPHERAL_EGU1


////
//
//    Software interrupt 0
//
////

struct swi2_t
{
    volatile uint32_t    UNUSED;               // [Read-only] Unused.

    static const uint32_t UNUSED_RESET_VALUE = 0x0;

    static constexpr uint8_t SWI2_EGU2 = 22; // 
};

static swi2_t& SWI2 = *reinterpret_cast<swi2_t*>(0x40016000);

#define HAVE_PERIPHERAL_SWI2


////
//
//    Event Generator Unit 0
//
////

struct egu2_t
{
    volatile uint32_t    TASKS_TRIGGER[s];     // [Write-only] Description collection[0]: Trigger 0 for triggering the corresponding TRIGGERED[0] event
    reserved_t<63>       _0;
    volatile uint32_t    EVENTS_TRIGGERED[s];  // [Read-write] Description collection[0]: Event number 0 generated by triggering the corresponding TRIGGER[0] task
    reserved_t<127>      _1;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt



    static constexpr uint32_t INTEN_TRIGGERED0 = 0x1;     // Enable or disable interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTEN_TRIGGERED1 = 0x2;     // Enable or disable interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTEN_TRIGGERED2 = 0x4;     // Enable or disable interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTEN_TRIGGERED3 = 0x8;     // Enable or disable interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTEN_TRIGGERED4 = 0x10;    // Enable or disable interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTEN_TRIGGERED5 = 0x20;    // Enable or disable interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTEN_TRIGGERED6 = 0x40;    // Enable or disable interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTEN_TRIGGERED7 = 0x80;    // Enable or disable interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTEN_TRIGGERED8 = 0x100;   // Enable or disable interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTEN_TRIGGERED9 = 0x200;   // Enable or disable interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTEN_TRIGGERED10 = 0x400;  // Enable or disable interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTEN_TRIGGERED11 = 0x800;  // Enable or disable interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTEN_TRIGGERED12 = 0x1000; // Enable or disable interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTEN_TRIGGERED13 = 0x2000; // Enable or disable interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTEN_TRIGGERED14 = 0x4000; // Enable or disable interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTEN_TRIGGERED15 = 0x8000; // Enable or disable interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint32_t INTENSET_TRIGGERED0 = 0x1;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTENSET_TRIGGERED1 = 0x2;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTENSET_TRIGGERED2 = 0x4;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTENSET_TRIGGERED3 = 0x8;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTENSET_TRIGGERED4 = 0x10;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTENSET_TRIGGERED5 = 0x20;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTENSET_TRIGGERED6 = 0x40;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTENSET_TRIGGERED7 = 0x80;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTENSET_TRIGGERED8 = 0x100;   // Write '1' to Enable interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTENSET_TRIGGERED9 = 0x200;   // Write '1' to Enable interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTENSET_TRIGGERED10 = 0x400;  // Write '1' to Enable interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTENSET_TRIGGERED11 = 0x800;  // Write '1' to Enable interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTENSET_TRIGGERED12 = 0x1000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTENSET_TRIGGERED13 = 0x2000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTENSET_TRIGGERED14 = 0x4000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTENSET_TRIGGERED15 = 0x8000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint32_t INTENCLR_TRIGGERED0 = 0x1;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTENCLR_TRIGGERED1 = 0x2;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTENCLR_TRIGGERED2 = 0x4;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTENCLR_TRIGGERED3 = 0x8;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTENCLR_TRIGGERED4 = 0x10;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTENCLR_TRIGGERED5 = 0x20;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTENCLR_TRIGGERED6 = 0x40;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTENCLR_TRIGGERED7 = 0x80;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTENCLR_TRIGGERED8 = 0x100;   // Write '1' to Clear interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTENCLR_TRIGGERED9 = 0x200;   // Write '1' to Clear interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTENCLR_TRIGGERED10 = 0x400;  // Write '1' to Clear interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTENCLR_TRIGGERED11 = 0x800;  // Write '1' to Clear interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTENCLR_TRIGGERED12 = 0x1000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTENCLR_TRIGGERED13 = 0x2000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTENCLR_TRIGGERED14 = 0x4000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTENCLR_TRIGGERED15 = 0x8000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint8_t SWI2_EGU2 = 22; // 
};

static egu2_t& EGU2 = *reinterpret_cast<egu2_t*>(0x40016000);

#define HAVE_PERIPHERAL_EGU2


////
//
//    Software interrupt 0
//
////

struct swi3_t
{
    volatile uint32_t    UNUSED;               // [Read-only] Unused.

    static const uint32_t UNUSED_RESET_VALUE = 0x0;

    static constexpr uint8_t SWI3_EGU3 = 23; // 
};

static swi3_t& SWI3 = *reinterpret_cast<swi3_t*>(0x40017000);

#define HAVE_PERIPHERAL_SWI3


////
//
//    Event Generator Unit 0
//
////

struct egu3_t
{
    volatile uint32_t    TASKS_TRIGGER[s];     // [Write-only] Description collection[0]: Trigger 0 for triggering the corresponding TRIGGERED[0] event
    reserved_t<63>       _0;
    volatile uint32_t    EVENTS_TRIGGERED[s];  // [Read-write] Description collection[0]: Event number 0 generated by triggering the corresponding TRIGGER[0] task
    reserved_t<127>      _1;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt



    static constexpr uint32_t INTEN_TRIGGERED0 = 0x1;     // Enable or disable interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTEN_TRIGGERED1 = 0x2;     // Enable or disable interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTEN_TRIGGERED2 = 0x4;     // Enable or disable interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTEN_TRIGGERED3 = 0x8;     // Enable or disable interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTEN_TRIGGERED4 = 0x10;    // Enable or disable interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTEN_TRIGGERED5 = 0x20;    // Enable or disable interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTEN_TRIGGERED6 = 0x40;    // Enable or disable interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTEN_TRIGGERED7 = 0x80;    // Enable or disable interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTEN_TRIGGERED8 = 0x100;   // Enable or disable interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTEN_TRIGGERED9 = 0x200;   // Enable or disable interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTEN_TRIGGERED10 = 0x400;  // Enable or disable interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTEN_TRIGGERED11 = 0x800;  // Enable or disable interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTEN_TRIGGERED12 = 0x1000; // Enable or disable interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTEN_TRIGGERED13 = 0x2000; // Enable or disable interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTEN_TRIGGERED14 = 0x4000; // Enable or disable interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTEN_TRIGGERED15 = 0x8000; // Enable or disable interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint32_t INTENSET_TRIGGERED0 = 0x1;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTENSET_TRIGGERED1 = 0x2;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTENSET_TRIGGERED2 = 0x4;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTENSET_TRIGGERED3 = 0x8;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTENSET_TRIGGERED4 = 0x10;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTENSET_TRIGGERED5 = 0x20;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTENSET_TRIGGERED6 = 0x40;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTENSET_TRIGGERED7 = 0x80;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTENSET_TRIGGERED8 = 0x100;   // Write '1' to Enable interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTENSET_TRIGGERED9 = 0x200;   // Write '1' to Enable interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTENSET_TRIGGERED10 = 0x400;  // Write '1' to Enable interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTENSET_TRIGGERED11 = 0x800;  // Write '1' to Enable interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTENSET_TRIGGERED12 = 0x1000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTENSET_TRIGGERED13 = 0x2000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTENSET_TRIGGERED14 = 0x4000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTENSET_TRIGGERED15 = 0x8000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint32_t INTENCLR_TRIGGERED0 = 0x1;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTENCLR_TRIGGERED1 = 0x2;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTENCLR_TRIGGERED2 = 0x4;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTENCLR_TRIGGERED3 = 0x8;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTENCLR_TRIGGERED4 = 0x10;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTENCLR_TRIGGERED5 = 0x20;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTENCLR_TRIGGERED6 = 0x40;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTENCLR_TRIGGERED7 = 0x80;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTENCLR_TRIGGERED8 = 0x100;   // Write '1' to Clear interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTENCLR_TRIGGERED9 = 0x200;   // Write '1' to Clear interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTENCLR_TRIGGERED10 = 0x400;  // Write '1' to Clear interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTENCLR_TRIGGERED11 = 0x800;  // Write '1' to Clear interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTENCLR_TRIGGERED12 = 0x1000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTENCLR_TRIGGERED13 = 0x2000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTENCLR_TRIGGERED14 = 0x4000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTENCLR_TRIGGERED15 = 0x8000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint8_t SWI3_EGU3 = 23; // 
};

static egu3_t& EGU3 = *reinterpret_cast<egu3_t*>(0x40017000);

#define HAVE_PERIPHERAL_EGU3


////
//
//    Software interrupt 0
//
////

struct swi4_t
{
    volatile uint32_t    UNUSED;               // [Read-only] Unused.

    static const uint32_t UNUSED_RESET_VALUE = 0x0;

    static constexpr uint8_t SWI4_EGU4 = 24; // 
};

static swi4_t& SWI4 = *reinterpret_cast<swi4_t*>(0x40018000);

#define HAVE_PERIPHERAL_SWI4


////
//
//    Event Generator Unit 0
//
////

struct egu4_t
{
    volatile uint32_t    TASKS_TRIGGER[s];     // [Write-only] Description collection[0]: Trigger 0 for triggering the corresponding TRIGGERED[0] event
    reserved_t<63>       _0;
    volatile uint32_t    EVENTS_TRIGGERED[s];  // [Read-write] Description collection[0]: Event number 0 generated by triggering the corresponding TRIGGER[0] task
    reserved_t<127>      _1;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt



    static constexpr uint32_t INTEN_TRIGGERED0 = 0x1;     // Enable or disable interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTEN_TRIGGERED1 = 0x2;     // Enable or disable interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTEN_TRIGGERED2 = 0x4;     // Enable or disable interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTEN_TRIGGERED3 = 0x8;     // Enable or disable interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTEN_TRIGGERED4 = 0x10;    // Enable or disable interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTEN_TRIGGERED5 = 0x20;    // Enable or disable interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTEN_TRIGGERED6 = 0x40;    // Enable or disable interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTEN_TRIGGERED7 = 0x80;    // Enable or disable interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTEN_TRIGGERED8 = 0x100;   // Enable or disable interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTEN_TRIGGERED9 = 0x200;   // Enable or disable interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTEN_TRIGGERED10 = 0x400;  // Enable or disable interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTEN_TRIGGERED11 = 0x800;  // Enable or disable interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTEN_TRIGGERED12 = 0x1000; // Enable or disable interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTEN_TRIGGERED13 = 0x2000; // Enable or disable interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTEN_TRIGGERED14 = 0x4000; // Enable or disable interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTEN_TRIGGERED15 = 0x8000; // Enable or disable interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint32_t INTENSET_TRIGGERED0 = 0x1;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTENSET_TRIGGERED1 = 0x2;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTENSET_TRIGGERED2 = 0x4;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTENSET_TRIGGERED3 = 0x8;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTENSET_TRIGGERED4 = 0x10;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTENSET_TRIGGERED5 = 0x20;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTENSET_TRIGGERED6 = 0x40;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTENSET_TRIGGERED7 = 0x80;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTENSET_TRIGGERED8 = 0x100;   // Write '1' to Enable interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTENSET_TRIGGERED9 = 0x200;   // Write '1' to Enable interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTENSET_TRIGGERED10 = 0x400;  // Write '1' to Enable interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTENSET_TRIGGERED11 = 0x800;  // Write '1' to Enable interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTENSET_TRIGGERED12 = 0x1000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTENSET_TRIGGERED13 = 0x2000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTENSET_TRIGGERED14 = 0x4000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTENSET_TRIGGERED15 = 0x8000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint32_t INTENCLR_TRIGGERED0 = 0x1;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTENCLR_TRIGGERED1 = 0x2;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTENCLR_TRIGGERED2 = 0x4;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTENCLR_TRIGGERED3 = 0x8;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTENCLR_TRIGGERED4 = 0x10;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTENCLR_TRIGGERED5 = 0x20;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTENCLR_TRIGGERED6 = 0x40;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTENCLR_TRIGGERED7 = 0x80;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTENCLR_TRIGGERED8 = 0x100;   // Write '1' to Clear interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTENCLR_TRIGGERED9 = 0x200;   // Write '1' to Clear interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTENCLR_TRIGGERED10 = 0x400;  // Write '1' to Clear interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTENCLR_TRIGGERED11 = 0x800;  // Write '1' to Clear interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTENCLR_TRIGGERED12 = 0x1000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTENCLR_TRIGGERED13 = 0x2000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTENCLR_TRIGGERED14 = 0x4000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTENCLR_TRIGGERED15 = 0x8000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint8_t SWI4_EGU4 = 24; // 
};

static egu4_t& EGU4 = *reinterpret_cast<egu4_t*>(0x40018000);

#define HAVE_PERIPHERAL_EGU4


////
//
//    Software interrupt 0
//
////

struct swi5_t
{
    volatile uint32_t    UNUSED;               // [Read-only] Unused.

    static const uint32_t UNUSED_RESET_VALUE = 0x0;

    static constexpr uint8_t SWI5_EGU5 = 25; // 
};

static swi5_t& SWI5 = *reinterpret_cast<swi5_t*>(0x40019000);

#define HAVE_PERIPHERAL_SWI5


////
//
//    Event Generator Unit 0
//
////

struct egu5_t
{
    volatile uint32_t    TASKS_TRIGGER[s];     // [Write-only] Description collection[0]: Trigger 0 for triggering the corresponding TRIGGERED[0] event
    reserved_t<63>       _0;
    volatile uint32_t    EVENTS_TRIGGERED[s];  // [Read-write] Description collection[0]: Event number 0 generated by triggering the corresponding TRIGGER[0] task
    reserved_t<127>      _1;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt



    static constexpr uint32_t INTEN_TRIGGERED0 = 0x1;     // Enable or disable interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTEN_TRIGGERED1 = 0x2;     // Enable or disable interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTEN_TRIGGERED2 = 0x4;     // Enable or disable interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTEN_TRIGGERED3 = 0x8;     // Enable or disable interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTEN_TRIGGERED4 = 0x10;    // Enable or disable interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTEN_TRIGGERED5 = 0x20;    // Enable or disable interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTEN_TRIGGERED6 = 0x40;    // Enable or disable interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTEN_TRIGGERED7 = 0x80;    // Enable or disable interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTEN_TRIGGERED8 = 0x100;   // Enable or disable interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTEN_TRIGGERED9 = 0x200;   // Enable or disable interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTEN_TRIGGERED10 = 0x400;  // Enable or disable interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTEN_TRIGGERED11 = 0x800;  // Enable or disable interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTEN_TRIGGERED12 = 0x1000; // Enable or disable interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTEN_TRIGGERED13 = 0x2000; // Enable or disable interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTEN_TRIGGERED14 = 0x4000; // Enable or disable interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTEN_TRIGGERED15 = 0x8000; // Enable or disable interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint32_t INTENSET_TRIGGERED0 = 0x1;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTENSET_TRIGGERED1 = 0x2;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTENSET_TRIGGERED2 = 0x4;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTENSET_TRIGGERED3 = 0x8;     // Write '1' to Enable interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTENSET_TRIGGERED4 = 0x10;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTENSET_TRIGGERED5 = 0x20;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTENSET_TRIGGERED6 = 0x40;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTENSET_TRIGGERED7 = 0x80;    // Write '1' to Enable interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTENSET_TRIGGERED8 = 0x100;   // Write '1' to Enable interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTENSET_TRIGGERED9 = 0x200;   // Write '1' to Enable interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTENSET_TRIGGERED10 = 0x400;  // Write '1' to Enable interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTENSET_TRIGGERED11 = 0x800;  // Write '1' to Enable interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTENSET_TRIGGERED12 = 0x1000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTENSET_TRIGGERED13 = 0x2000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTENSET_TRIGGERED14 = 0x4000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTENSET_TRIGGERED15 = 0x8000; // Write '1' to Enable interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint32_t INTENCLR_TRIGGERED0 = 0x1;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[0] event
    static constexpr uint32_t INTENCLR_TRIGGERED1 = 0x2;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[1] event
    static constexpr uint32_t INTENCLR_TRIGGERED2 = 0x4;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[2] event
    static constexpr uint32_t INTENCLR_TRIGGERED3 = 0x8;     // Write '1' to Clear interrupt on EVENTS_TRIGGERED[3] event
    static constexpr uint32_t INTENCLR_TRIGGERED4 = 0x10;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[4] event
    static constexpr uint32_t INTENCLR_TRIGGERED5 = 0x20;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[5] event
    static constexpr uint32_t INTENCLR_TRIGGERED6 = 0x40;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[6] event
    static constexpr uint32_t INTENCLR_TRIGGERED7 = 0x80;    // Write '1' to Clear interrupt on EVENTS_TRIGGERED[7] event
    static constexpr uint32_t INTENCLR_TRIGGERED8 = 0x100;   // Write '1' to Clear interrupt on EVENTS_TRIGGERED[8] event
    static constexpr uint32_t INTENCLR_TRIGGERED9 = 0x200;   // Write '1' to Clear interrupt on EVENTS_TRIGGERED[9] event
    static constexpr uint32_t INTENCLR_TRIGGERED10 = 0x400;  // Write '1' to Clear interrupt on EVENTS_TRIGGERED[10] event
    static constexpr uint32_t INTENCLR_TRIGGERED11 = 0x800;  // Write '1' to Clear interrupt on EVENTS_TRIGGERED[11] event
    static constexpr uint32_t INTENCLR_TRIGGERED12 = 0x1000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[12] event
    static constexpr uint32_t INTENCLR_TRIGGERED13 = 0x2000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[13] event
    static constexpr uint32_t INTENCLR_TRIGGERED14 = 0x4000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[14] event
    static constexpr uint32_t INTENCLR_TRIGGERED15 = 0x8000; // Write '1' to Clear interrupt on EVENTS_TRIGGERED[15] event

    static constexpr uint8_t SWI5_EGU5 = 25; // 
};

static egu5_t& EGU5 = *reinterpret_cast<egu5_t*>(0x40019000);

#define HAVE_PERIPHERAL_EGU5


////
//
//    Timer/Counter 0
//
////

struct timer3_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start Timer
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop Timer
    volatile uint32_t    TASKS_COUNT;          // [Write-only] Increment Timer (Counter mode only)
    volatile uint32_t    TASKS_CLEAR;          // [Write-only] Clear time
    volatile uint32_t    TASKS_SHUTDOWN;       // [Write-only] Shut down timer
    reserved_t<11>       _0;
    volatile uint32_t    TASKS_CAPTURE[s];     // [Write-only] Description collection[0]: Capture Timer value to CC[0] register
    reserved_t<63>       _1;
    volatile uint32_t    EVENTS_COMPARE[s];    // [Read-write] Description collection[0]: Compare event on CC[0] match
    reserved_t<47>       _2;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _3;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<126>      _4;
    volatile uint32_t    MODE;                 // [Read-write] Timer mode selection
    volatile uint32_t    BITMODE;              // [Read-write] Configure the number of bits used by the TIMER
    reserved_t<1>        _5;
    volatile uint32_t    PRESCALER;            // [Read-write] Timer prescaler register
    reserved_t<11>       _6;
    volatile uint32_t    CC[s];                // [Read-write] Description collection[0]: Capture/Compare register 0








    static constexpr uint32_t SHORTS_COMPARE0_CLEAR = 0x1; // Shortcut between EVENTS_COMPARE[0] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE1_CLEAR = 0x2; // Shortcut between EVENTS_COMPARE[1] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE2_CLEAR = 0x4; // Shortcut between EVENTS_COMPARE[2] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE3_CLEAR = 0x8; // Shortcut between EVENTS_COMPARE[3] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE4_CLEAR = 0x10;// Shortcut between EVENTS_COMPARE[4] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE5_CLEAR = 0x20;// Shortcut between EVENTS_COMPARE[5] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE0_STOP = 0x100;// Shortcut between EVENTS_COMPARE[0] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE1_STOP = 0x200;// Shortcut between EVENTS_COMPARE[1] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE2_STOP = 0x400;// Shortcut between EVENTS_COMPARE[2] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE3_STOP = 0x800;// Shortcut between EVENTS_COMPARE[3] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE4_STOP = 0x1000;// Shortcut between EVENTS_COMPARE[4] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE5_STOP = 0x2000;// Shortcut between EVENTS_COMPARE[5] event and TASKS_STOP task

    static constexpr uint32_t INTENSET_COMPARE0 = 0x10000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENSET_COMPARE1 = 0x20000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENSET_COMPARE2 = 0x40000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENSET_COMPARE3 = 0x80000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[3] event
    static constexpr uint32_t INTENSET_COMPARE4 = 0x100000;  // Write '1' to Enable interrupt on EVENTS_COMPARE[4] event
    static constexpr uint32_t INTENSET_COMPARE5 = 0x200000;  // Write '1' to Enable interrupt on EVENTS_COMPARE[5] event

    static constexpr uint32_t INTENCLR_COMPARE0 = 0x10000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENCLR_COMPARE1 = 0x20000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENCLR_COMPARE2 = 0x40000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENCLR_COMPARE3 = 0x80000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[3] event
    static constexpr uint32_t INTENCLR_COMPARE4 = 0x100000;  // Write '1' to Clear interrupt on EVENTS_COMPARE[4] event
    static constexpr uint32_t INTENCLR_COMPARE5 = 0x200000;  // Write '1' to Clear interrupt on EVENTS_COMPARE[5] event

    template<uint32_t X>
    static constexpr uint32_t MODE_MODE =                // Timer mode (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t BITMODE_BITMODE =             // Timer bit width (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t PRESCALER_PRESCALER =           // Prescaler value (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESCALER_RESET_VALUE = 0x4;



    static constexpr uint8_t TIMER3 = 26; // 
};

static timer3_t& TIMER3 = *reinterpret_cast<timer3_t*>(0x4001a000);

#define HAVE_PERIPHERAL_TIMER3


////
//
//    Timer/Counter 0
//
////

struct timer4_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start Timer
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop Timer
    volatile uint32_t    TASKS_COUNT;          // [Write-only] Increment Timer (Counter mode only)
    volatile uint32_t    TASKS_CLEAR;          // [Write-only] Clear time
    volatile uint32_t    TASKS_SHUTDOWN;       // [Write-only] Shut down timer
    reserved_t<11>       _0;
    volatile uint32_t    TASKS_CAPTURE[s];     // [Write-only] Description collection[0]: Capture Timer value to CC[0] register
    reserved_t<63>       _1;
    volatile uint32_t    EVENTS_COMPARE[s];    // [Read-write] Description collection[0]: Compare event on CC[0] match
    reserved_t<47>       _2;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _3;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<126>      _4;
    volatile uint32_t    MODE;                 // [Read-write] Timer mode selection
    volatile uint32_t    BITMODE;              // [Read-write] Configure the number of bits used by the TIMER
    reserved_t<1>        _5;
    volatile uint32_t    PRESCALER;            // [Read-write] Timer prescaler register
    reserved_t<11>       _6;
    volatile uint32_t    CC[s];                // [Read-write] Description collection[0]: Capture/Compare register 0








    static constexpr uint32_t SHORTS_COMPARE0_CLEAR = 0x1; // Shortcut between EVENTS_COMPARE[0] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE1_CLEAR = 0x2; // Shortcut between EVENTS_COMPARE[1] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE2_CLEAR = 0x4; // Shortcut between EVENTS_COMPARE[2] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE3_CLEAR = 0x8; // Shortcut between EVENTS_COMPARE[3] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE4_CLEAR = 0x10;// Shortcut between EVENTS_COMPARE[4] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE5_CLEAR = 0x20;// Shortcut between EVENTS_COMPARE[5] event and TASKS_CLEAR task
    static constexpr uint32_t SHORTS_COMPARE0_STOP = 0x100;// Shortcut between EVENTS_COMPARE[0] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE1_STOP = 0x200;// Shortcut between EVENTS_COMPARE[1] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE2_STOP = 0x400;// Shortcut between EVENTS_COMPARE[2] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE3_STOP = 0x800;// Shortcut between EVENTS_COMPARE[3] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE4_STOP = 0x1000;// Shortcut between EVENTS_COMPARE[4] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_COMPARE5_STOP = 0x2000;// Shortcut between EVENTS_COMPARE[5] event and TASKS_STOP task

    static constexpr uint32_t INTENSET_COMPARE0 = 0x10000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENSET_COMPARE1 = 0x20000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENSET_COMPARE2 = 0x40000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENSET_COMPARE3 = 0x80000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[3] event
    static constexpr uint32_t INTENSET_COMPARE4 = 0x100000;  // Write '1' to Enable interrupt on EVENTS_COMPARE[4] event
    static constexpr uint32_t INTENSET_COMPARE5 = 0x200000;  // Write '1' to Enable interrupt on EVENTS_COMPARE[5] event

    static constexpr uint32_t INTENCLR_COMPARE0 = 0x10000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENCLR_COMPARE1 = 0x20000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENCLR_COMPARE2 = 0x40000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENCLR_COMPARE3 = 0x80000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[3] event
    static constexpr uint32_t INTENCLR_COMPARE4 = 0x100000;  // Write '1' to Clear interrupt on EVENTS_COMPARE[4] event
    static constexpr uint32_t INTENCLR_COMPARE5 = 0x200000;  // Write '1' to Clear interrupt on EVENTS_COMPARE[5] event

    template<uint32_t X>
    static constexpr uint32_t MODE_MODE =                // Timer mode (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t BITMODE_BITMODE =             // Timer bit width (2 bits)
        bit_field_t<0, 0x3>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t PRESCALER_PRESCALER =           // Prescaler value (4 bits)
        bit_field_t<0, 0xf>::value<X>();
    static const uint32_t PRESCALER_RESET_VALUE = 0x4;



    static constexpr uint8_t TIMER4 = 27; // 
};

static timer4_t& TIMER4 = *reinterpret_cast<timer4_t*>(0x4001b000);

#define HAVE_PERIPHERAL_TIMER4


////
//
//    Pulse Width Modulation Unit 0
//
////

struct pwm0_t
{
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stops PWM pulse generation on all channels at the end of current PWM period, and stops sequence playback
    volatile uint32_t    TASKS_SEQSTART[s];    // [Write-only] Description collection[0]: Loads the first PWM value on all enabled channels from sequence 0, and starts playing that sequence at the rate defined in SEQ[0]REFRESH and/or DECODER.MODE. Causes PWM generation to start it was not running.
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_NEXTSTEP;       // [Write-only] Steps by one value in the current sequence on all enabled channels if DECODER.MODE=NextStep. Does not cause PWM generation to start it was not running.
    reserved_t<60>       _1;
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] Response to STOP task, emitted when PWM pulses are no longer generated
    volatile uint32_t    EVENTS_SEQSTARTED[s]; // [Read-write] Description collection[0]: First PWM period started on sequence 0
    reserved_t<1>        _2;
    volatile uint32_t    EVENTS_SEQEND[s];     // [Read-write] Description collection[0]: Emitted at end of every sequence 0, when last value from RAM has been applied to wave counter
    reserved_t<1>        _3;
    volatile uint32_t    EVENTS_PWMPERIODEND;  // [Read-write] Emitted at the end of each PWM period
    volatile uint32_t    EVENTS_LOOPSDONE;     // [Read-write] Concatenated sequences have been played the amount of times defined in LOOP.CNT
    reserved_t<56>       _4;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<63>       _5;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<125>      _6;
    volatile uint32_t    ENABLE;               // [Read-write] PWM module enable register
    volatile uint32_t    MODE;                 // [Read-write] Selects operating mode of the wave counter
    volatile uint32_t    COUNTERTOP;           // [Read-write] Value up to which the pulse generator counter counts
    volatile uint32_t    PRESCALER;            // [Read-write] Configuration for PWM_CLK
    volatile uint32_t    DECODER;              // [Read-write] Configuration of the decoder
    volatile uint32_t    LOOP;                 // [Read-write] Amount of playback of a loop









    static constexpr uint32_t SHORTS_SEQEND0_STOP = 0x1;   // Shortcut between EVENTS_SEQEND[0] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_SEQEND1_STOP = 0x2;   // Shortcut between EVENTS_SEQEND[1] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_LOOPSDONE_SEQSTART0 = 0x4;// Shortcut between EVENTS_LOOPSDONE event and TASKS_SEQSTART[0] task
    static constexpr uint32_t SHORTS_LOOPSDONE_SEQSTART1 = 0x8;// Shortcut between EVENTS_LOOPSDONE event and TASKS_SEQSTART[1] task
    static constexpr uint32_t SHORTS_LOOPSDONE_STOP = 0x10;// Shortcut between EVENTS_LOOPSDONE event and TASKS_STOP task

    static constexpr uint32_t INTEN_STOPPED = 0x2;        // Enable or disable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTEN_SEQSTARTED0 = 0x4;    // Enable or disable interrupt on EVENTS_SEQSTARTED[0] event
    static constexpr uint32_t INTEN_SEQSTARTED1 = 0x8;    // Enable or disable interrupt on EVENTS_SEQSTARTED[1] event
    static constexpr uint32_t INTEN_SEQEND0 = 0x10;       // Enable or disable interrupt on EVENTS_SEQEND[0] event
    static constexpr uint32_t INTEN_SEQEND1 = 0x20;       // Enable or disable interrupt on EVENTS_SEQEND[1] event
    static constexpr uint32_t INTEN_PWMPERIODEND = 0x40;  // Enable or disable interrupt on EVENTS_PWMPERIODEND event
    static constexpr uint32_t INTEN_LOOPSDONE = 0x80;     // Enable or disable interrupt on EVENTS_LOOPSDONE event

    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_SEQSTARTED0 = 0x4;    // Write '1' to Enable interrupt on EVENTS_SEQSTARTED[0] event
    static constexpr uint32_t INTENSET_SEQSTARTED1 = 0x8;    // Write '1' to Enable interrupt on EVENTS_SEQSTARTED[1] event
    static constexpr uint32_t INTENSET_SEQEND0 = 0x10;       // Write '1' to Enable interrupt on EVENTS_SEQEND[0] event
    static constexpr uint32_t INTENSET_SEQEND1 = 0x20;       // Write '1' to Enable interrupt on EVENTS_SEQEND[1] event
    static constexpr uint32_t INTENSET_PWMPERIODEND = 0x40;  // Write '1' to Enable interrupt on EVENTS_PWMPERIODEND event
    static constexpr uint32_t INTENSET_LOOPSDONE = 0x80;     // Write '1' to Enable interrupt on EVENTS_LOOPSDONE event

    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_SEQSTARTED0 = 0x4;    // Write '1' to Clear interrupt on EVENTS_SEQSTARTED[0] event
    static constexpr uint32_t INTENCLR_SEQSTARTED1 = 0x8;    // Write '1' to Clear interrupt on EVENTS_SEQSTARTED[1] event
    static constexpr uint32_t INTENCLR_SEQEND0 = 0x10;       // Write '1' to Clear interrupt on EVENTS_SEQEND[0] event
    static constexpr uint32_t INTENCLR_SEQEND1 = 0x20;       // Write '1' to Clear interrupt on EVENTS_SEQEND[1] event
    static constexpr uint32_t INTENCLR_PWMPERIODEND = 0x40;  // Write '1' to Clear interrupt on EVENTS_PWMPERIODEND event
    static constexpr uint32_t INTENCLR_LOOPSDONE = 0x80;     // Write '1' to Clear interrupt on EVENTS_LOOPSDONE event

    static constexpr uint32_t ENABLE_ENABLE = 0x1;         // Enable or disable PWM module
    static const uint32_t ENABLE_RESET_VALUE = 0x0;

    static constexpr uint32_t MODE_UPDOWN = 0x1;         // Selects up or up and down as wave counter mode
    static const uint32_t MODE_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t COUNTERTOP_COUNTERTOP =          // Value up to which the pulse generator counter counts. This register is ignored when DECODER.MODE=WaveForm and only values from RAM will be used. (15 bits)
        bit_field_t<0, 0x7fff>::value<X>();
    static const uint32_t COUNTERTOP_RESET_VALUE = 0x3ff;

    template<uint32_t X>
    static constexpr uint32_t PRESCALER_PRESCALER =           // Pre-scaler of PWM_CLK (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t PRESCALER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DECODER_LOAD =                // How a sequence is read from RAM and spread to the compare register (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t DECODER_MODE = 0x100;         // Selects source for advancing the active sequence
    static const uint32_t DECODER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t LOOP_CNT =                 // Amount of playback of pattern cycles (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t LOOP_RESET_VALUE = 0x0;

    static constexpr uint8_t PWM0 = 28; // 
};

static pwm0_t& PWM0 = *reinterpret_cast<pwm0_t*>(0x4001c000);

#define HAVE_PERIPHERAL_PWM0


////
//
//    Pulse Density Modulation (Digital Microphone) Interface
//
////

struct pdm_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Starts continuous PDM transfer
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stops PDM transfer
    reserved_t<62>       _0;
    volatile uint32_t    EVENTS_STARTED;       // [Read-write] PDM transfer has started
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] PDM transfer has finished
    volatile uint32_t    EVENTS_END;           // [Read-write] The PDM has written the last sample specified by SAMPLE.MAXCNT (or the last sample after a STOP task has been received) to Data RAM
    reserved_t<125>      _1;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<125>      _2;
    volatile uint32_t    ENABLE;               // [Read-write] PDM module enable register
    volatile uint32_t    PDMCLKCTRL;           // [Read-write] PDM clock generator control
    volatile uint32_t    MODE;                 // [Read-write] Defines the routing of the connected PDM microphones' signals
    reserved_t<3>        _3;
    volatile uint32_t    GAINL;                // [Read-write] Left output gain adjustment
    volatile uint32_t    GAINR;                // [Read-write] Right output gain adjustment






    static constexpr uint32_t INTEN_STARTED = 0x1;        // Enable or disable interrupt on EVENTS_STARTED event
    static constexpr uint32_t INTEN_STOPPED = 0x2;        // Enable or disable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTEN_END = 0x4;            // Enable or disable interrupt on EVENTS_END event

    static constexpr uint32_t INTENSET_STARTED = 0x1;        // Write '1' to Enable interrupt on EVENTS_STARTED event
    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_END = 0x4;            // Write '1' to Enable interrupt on EVENTS_END event

    static constexpr uint32_t INTENCLR_STARTED = 0x1;        // Write '1' to Clear interrupt on EVENTS_STARTED event
    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_END = 0x4;            // Write '1' to Clear interrupt on EVENTS_END event

    static constexpr uint32_t ENABLE_ENABLE = 0x1;         // Enable or disable PDM reception
    static const uint32_t ENABLE_RESET_VALUE = 0x0;


    static const uint32_t PDMCLKCTRL_RESET_VALUE = 0x8400000;

    static constexpr uint32_t MODE_MONO = 0x1;           // Mono or stereo operation
    static constexpr uint32_t MODE_EDGE = 0x2;           // Defines on which PDM_CLK edge Left (or mono) is sampled
    static const uint32_t MODE_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t GAINL_GAINL =               // Left output gain adjustment, in 0.5 dB steps, around the requirement that 0dB gain adjustment corresponds to 2500 RMS output samples (16-bit) with 1 kHz 90dBA signal into a -26dBFS sensitivity PDM microphone. 0x00 -20 dB gain 0x01 -19.5 dB gain (...) 0x27 -0.5 dB gain 0x28 0 dB gain 0x29 +0.5 dB gain (...) 0x4F +19.5 dB gain 0x50 +20 dB gain (7 bits)
        bit_field_t<0, 0x7f>::value<X>();
    static const uint32_t GAINL_RESET_VALUE = 0x28;

    template<uint32_t X>
    static constexpr uint32_t GAINR_GAINR =               // Right output gain adjustment, in 0.5 dB steps, around the requirement that 0dB gain adjustment corresponds to 2500 RMS output samples (16-bit) with 1 kHz 90dBA signal into a -26dBFS sensitivity PDM microphone. (same encoding as GAINL) (8 bits)
        bit_field_t<0, 0xff>::value<X>();
    static const uint32_t GAINR_RESET_VALUE = 0x28;

    static constexpr uint8_t PDM = 29; // 
};

static pdm_t& PDM = *reinterpret_cast<pdm_t*>(0x4001d000);

#define HAVE_PERIPHERAL_PDM


////
//
//    Non Volatile Memory Controller
//
////

struct nvmc_t
{
    volatile uint32_t    READY;                // [Read-only] Ready flag
    reserved_t<64>       _0;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register
    volatile uint32_t    ERASEPAGE;            // [Read-write] Register for erasing a page in Code area
    volatile uint32_t    ERASEALL;             // [Read-write] Register for erasing all non-volatile user memory
    volatile uint32_t    ERASEPCR0;            // [Read-write] Deprecated register - Register for erasing a page in Code area. Equivalent to ERASEPAGE.
    volatile uint32_t    ERASEUICR;            // [Read-write] Register for erasing User Information Configuration Registers
    reserved_t<10>       _1;
    volatile uint32_t    ICACHECNF;            // [Read-write] I-Code cache configuration register.
    reserved_t<1>        _2;
    volatile uint32_t    IHIT;                 // [Read-write] I-Code cache hit counter.
    volatile uint32_t    IMISS;                // [Read-write] I-Code cache miss counter.

    static constexpr uint32_t READY_READY = 0x1;          // NVMC is ready or busy

    template<uint32_t X>
    static constexpr uint32_t CONFIG_WEN =                 // Program memory access mode. It is strongly recommended to only activate erase and write modes when they are actively used. Enabling write or erase will invalidate the cache and keep it invalidated. (2 bits)
        bit_field_t<0, 0x3>::value<X>();




    static constexpr uint32_t ERASEALL_ERASEALL = 0x1;       // Erase all non-volatile memory including UICR registers. Note that code erase has to be enabled by CONFIG.EEN before the UICR can be erased.



    static constexpr uint32_t ERASEUICR_ERASEUICR = 0x1;      // Register starting erase of all User Information Configuration Registers. Note that code erase has to be enabled by CONFIG.EEN before the UICR can be erased.

    static constexpr uint32_t ICACHECNF_CACHEEN = 0x1;        // Cache enable
    static constexpr uint32_t ICACHECNF_CACHEPROFEN = 0x100;  // Cache profiling enable
    static const uint32_t ICACHECNF_RESET_VALUE = 0x0;




};

static nvmc_t& NVMC = *reinterpret_cast<nvmc_t*>(0x4001e000);

#define HAVE_PERIPHERAL_NVMC


////
//
//    Programmable Peripheral Interconnect
//
////

struct ppi_t
{
    volatile uint32_t    CHEN;                 // [Read-write] Channel enable register
    volatile uint32_t    CHENSET;              // [Read-write] Channel enable set register
    volatile uint32_t    CHENCLR;              // [Read-write] Channel enable clear register
    reserved_t<189>      _0;
    volatile uint32_t    CHG[s];               // [Read-write] Description collection[0]: Channel group 0

    static constexpr uint32_t CHEN_CH0 = 0x1;            // Enable or disable channel 0
    static constexpr uint32_t CHEN_CH1 = 0x2;            // Enable or disable channel 1
    static constexpr uint32_t CHEN_CH2 = 0x4;            // Enable or disable channel 2
    static constexpr uint32_t CHEN_CH3 = 0x8;            // Enable or disable channel 3
    static constexpr uint32_t CHEN_CH4 = 0x10;           // Enable or disable channel 4
    static constexpr uint32_t CHEN_CH5 = 0x20;           // Enable or disable channel 5
    static constexpr uint32_t CHEN_CH6 = 0x40;           // Enable or disable channel 6
    static constexpr uint32_t CHEN_CH7 = 0x80;           // Enable or disable channel 7
    static constexpr uint32_t CHEN_CH8 = 0x100;          // Enable or disable channel 8
    static constexpr uint32_t CHEN_CH9 = 0x200;          // Enable or disable channel 9
    static constexpr uint32_t CHEN_CH10 = 0x400;         // Enable or disable channel 10
    static constexpr uint32_t CHEN_CH11 = 0x800;         // Enable or disable channel 11
    static constexpr uint32_t CHEN_CH12 = 0x1000;        // Enable or disable channel 12
    static constexpr uint32_t CHEN_CH13 = 0x2000;        // Enable or disable channel 13
    static constexpr uint32_t CHEN_CH14 = 0x4000;        // Enable or disable channel 14
    static constexpr uint32_t CHEN_CH15 = 0x8000;        // Enable or disable channel 15
    static constexpr uint32_t CHEN_CH16 = 0x10000;       // Enable or disable channel 16
    static constexpr uint32_t CHEN_CH17 = 0x20000;       // Enable or disable channel 17
    static constexpr uint32_t CHEN_CH18 = 0x40000;       // Enable or disable channel 18
    static constexpr uint32_t CHEN_CH19 = 0x80000;       // Enable or disable channel 19
    static constexpr uint32_t CHEN_CH20 = 0x100000;      // Enable or disable channel 20
    static constexpr uint32_t CHEN_CH21 = 0x200000;      // Enable or disable channel 21
    static constexpr uint32_t CHEN_CH22 = 0x400000;      // Enable or disable channel 22
    static constexpr uint32_t CHEN_CH23 = 0x800000;      // Enable or disable channel 23
    static constexpr uint32_t CHEN_CH24 = 0x1000000;     // Enable or disable channel 24
    static constexpr uint32_t CHEN_CH25 = 0x2000000;     // Enable or disable channel 25
    static constexpr uint32_t CHEN_CH26 = 0x4000000;     // Enable or disable channel 26
    static constexpr uint32_t CHEN_CH27 = 0x8000000;     // Enable or disable channel 27
    static constexpr uint32_t CHEN_CH28 = 0x10000000;    // Enable or disable channel 28
    static constexpr uint32_t CHEN_CH29 = 0x20000000;    // Enable or disable channel 29
    static constexpr uint32_t CHEN_CH30 = 0x40000000;    // Enable or disable channel 30
    static constexpr uint32_t CHEN_CH31 = 0x80000000;    // Enable or disable channel 31

    static constexpr uint32_t CHENSET_CH0 = 0x1;            // Channel 0 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH1 = 0x2;            // Channel 1 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH2 = 0x4;            // Channel 2 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH3 = 0x8;            // Channel 3 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH4 = 0x10;           // Channel 4 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH5 = 0x20;           // Channel 5 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH6 = 0x40;           // Channel 6 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH7 = 0x80;           // Channel 7 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH8 = 0x100;          // Channel 8 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH9 = 0x200;          // Channel 9 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH10 = 0x400;         // Channel 10 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH11 = 0x800;         // Channel 11 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH12 = 0x1000;        // Channel 12 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH13 = 0x2000;        // Channel 13 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH14 = 0x4000;        // Channel 14 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH15 = 0x8000;        // Channel 15 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH16 = 0x10000;       // Channel 16 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH17 = 0x20000;       // Channel 17 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH18 = 0x40000;       // Channel 18 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH19 = 0x80000;       // Channel 19 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH20 = 0x100000;      // Channel 20 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH21 = 0x200000;      // Channel 21 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH22 = 0x400000;      // Channel 22 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH23 = 0x800000;      // Channel 23 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH24 = 0x1000000;     // Channel 24 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH25 = 0x2000000;     // Channel 25 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH26 = 0x4000000;     // Channel 26 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH27 = 0x8000000;     // Channel 27 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH28 = 0x10000000;    // Channel 28 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH29 = 0x20000000;    // Channel 29 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH30 = 0x40000000;    // Channel 30 enable set register. Writing '0' has no effect
    static constexpr uint32_t CHENSET_CH31 = 0x80000000;    // Channel 31 enable set register. Writing '0' has no effect

    static constexpr uint32_t CHENCLR_CH0 = 0x1;            // Channel 0 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH1 = 0x2;            // Channel 1 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH2 = 0x4;            // Channel 2 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH3 = 0x8;            // Channel 3 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH4 = 0x10;           // Channel 4 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH5 = 0x20;           // Channel 5 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH6 = 0x40;           // Channel 6 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH7 = 0x80;           // Channel 7 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH8 = 0x100;          // Channel 8 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH9 = 0x200;          // Channel 9 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH10 = 0x400;         // Channel 10 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH11 = 0x800;         // Channel 11 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH12 = 0x1000;        // Channel 12 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH13 = 0x2000;        // Channel 13 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH14 = 0x4000;        // Channel 14 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH15 = 0x8000;        // Channel 15 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH16 = 0x10000;       // Channel 16 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH17 = 0x20000;       // Channel 17 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH18 = 0x40000;       // Channel 18 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH19 = 0x80000;       // Channel 19 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH20 = 0x100000;      // Channel 20 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH21 = 0x200000;      // Channel 21 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH22 = 0x400000;      // Channel 22 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH23 = 0x800000;      // Channel 23 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH24 = 0x1000000;     // Channel 24 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH25 = 0x2000000;     // Channel 25 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH26 = 0x4000000;     // Channel 26 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH27 = 0x8000000;     // Channel 27 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH28 = 0x10000000;    // Channel 28 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH29 = 0x20000000;    // Channel 29 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH30 = 0x40000000;    // Channel 30 enable clear register. Writing '0' has no effect
    static constexpr uint32_t CHENCLR_CH31 = 0x80000000;    // Channel 31 enable clear register. Writing '0' has no effect

    static constexpr uint32_t CHG[%s]_CH0 = 0x1;            // Include or exclude channel 0
    static constexpr uint32_t CHG[%s]_CH1 = 0x2;            // Include or exclude channel 1
    static constexpr uint32_t CHG[%s]_CH2 = 0x4;            // Include or exclude channel 2
    static constexpr uint32_t CHG[%s]_CH3 = 0x8;            // Include or exclude channel 3
    static constexpr uint32_t CHG[%s]_CH4 = 0x10;           // Include or exclude channel 4
    static constexpr uint32_t CHG[%s]_CH5 = 0x20;           // Include or exclude channel 5
    static constexpr uint32_t CHG[%s]_CH6 = 0x40;           // Include or exclude channel 6
    static constexpr uint32_t CHG[%s]_CH7 = 0x80;           // Include or exclude channel 7
    static constexpr uint32_t CHG[%s]_CH8 = 0x100;          // Include or exclude channel 8
    static constexpr uint32_t CHG[%s]_CH9 = 0x200;          // Include or exclude channel 9
    static constexpr uint32_t CHG[%s]_CH10 = 0x400;         // Include or exclude channel 10
    static constexpr uint32_t CHG[%s]_CH11 = 0x800;         // Include or exclude channel 11
    static constexpr uint32_t CHG[%s]_CH12 = 0x1000;        // Include or exclude channel 12
    static constexpr uint32_t CHG[%s]_CH13 = 0x2000;        // Include or exclude channel 13
    static constexpr uint32_t CHG[%s]_CH14 = 0x4000;        // Include or exclude channel 14
    static constexpr uint32_t CHG[%s]_CH15 = 0x8000;        // Include or exclude channel 15
    static constexpr uint32_t CHG[%s]_CH16 = 0x10000;       // Include or exclude channel 16
    static constexpr uint32_t CHG[%s]_CH17 = 0x20000;       // Include or exclude channel 17
    static constexpr uint32_t CHG[%s]_CH18 = 0x40000;       // Include or exclude channel 18
    static constexpr uint32_t CHG[%s]_CH19 = 0x80000;       // Include or exclude channel 19
    static constexpr uint32_t CHG[%s]_CH20 = 0x100000;      // Include or exclude channel 20
    static constexpr uint32_t CHG[%s]_CH21 = 0x200000;      // Include or exclude channel 21
    static constexpr uint32_t CHG[%s]_CH22 = 0x400000;      // Include or exclude channel 22
    static constexpr uint32_t CHG[%s]_CH23 = 0x800000;      // Include or exclude channel 23
    static constexpr uint32_t CHG[%s]_CH24 = 0x1000000;     // Include or exclude channel 24
    static constexpr uint32_t CHG[%s]_CH25 = 0x2000000;     // Include or exclude channel 25
    static constexpr uint32_t CHG[%s]_CH26 = 0x4000000;     // Include or exclude channel 26
    static constexpr uint32_t CHG[%s]_CH27 = 0x8000000;     // Include or exclude channel 27
    static constexpr uint32_t CHG[%s]_CH28 = 0x10000000;    // Include or exclude channel 28
    static constexpr uint32_t CHG[%s]_CH29 = 0x20000000;    // Include or exclude channel 29
    static constexpr uint32_t CHG[%s]_CH30 = 0x40000000;    // Include or exclude channel 30
    static constexpr uint32_t CHG[%s]_CH31 = 0x80000000;    // Include or exclude channel 31
};

static ppi_t& PPI = *reinterpret_cast<ppi_t*>(0x4001f000);

#define HAVE_PERIPHERAL_PPI


////
//
//    Memory Watch Unit
//
////

struct mwu_t
{
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<5>        _0;
    volatile uint32_t    NMIEN;                // [Read-write] Enable or disable non-maskable interrupt
    volatile uint32_t    NMIENSET;             // [Read-write] Enable non-maskable interrupt
    volatile uint32_t    NMIENCLR;             // [Read-write] Disable non-maskable interrupt
    reserved_t<121>      _1;
    volatile uint32_t    REGIONEN;             // [Read-write] Enable/disable regions watch
    volatile uint32_t    REGIONENSET;          // [Read-write] Enable regions watch
    volatile uint32_t    REGIONENCLR;          // [Read-write] Disable regions watch

    static constexpr uint32_t INTEN_REGION0WA = 0x1;      // Enable or disable interrupt on EVENTS_REGION[0].WA event
    static constexpr uint32_t INTEN_REGION0RA = 0x2;      // Enable or disable interrupt on EVENTS_REGION[0].RA event
    static constexpr uint32_t INTEN_REGION1WA = 0x4;      // Enable or disable interrupt on EVENTS_REGION[1].WA event
    static constexpr uint32_t INTEN_REGION1RA = 0x8;      // Enable or disable interrupt on EVENTS_REGION[1].RA event
    static constexpr uint32_t INTEN_REGION2WA = 0x10;     // Enable or disable interrupt on EVENTS_REGION[2].WA event
    static constexpr uint32_t INTEN_REGION2RA = 0x20;     // Enable or disable interrupt on EVENTS_REGION[2].RA event
    static constexpr uint32_t INTEN_REGION3WA = 0x40;     // Enable or disable interrupt on EVENTS_REGION[3].WA event
    static constexpr uint32_t INTEN_REGION3RA = 0x80;     // Enable or disable interrupt on EVENTS_REGION[3].RA event
    static constexpr uint32_t INTEN_PREGION0WA = 0x1000000;// Enable or disable interrupt on EVENTS_PREGION[0].WA event
    static constexpr uint32_t INTEN_PREGION0RA = 0x2000000;// Enable or disable interrupt on EVENTS_PREGION[0].RA event
    static constexpr uint32_t INTEN_PREGION1WA = 0x4000000;// Enable or disable interrupt on EVENTS_PREGION[1].WA event
    static constexpr uint32_t INTEN_PREGION1RA = 0x8000000;// Enable or disable interrupt on EVENTS_PREGION[1].RA event

    static constexpr uint32_t INTENSET_REGION0WA = 0x1;      // Write '1' to Enable interrupt on EVENTS_REGION[0].WA event
    static constexpr uint32_t INTENSET_REGION0RA = 0x2;      // Write '1' to Enable interrupt on EVENTS_REGION[0].RA event
    static constexpr uint32_t INTENSET_REGION1WA = 0x4;      // Write '1' to Enable interrupt on EVENTS_REGION[1].WA event
    static constexpr uint32_t INTENSET_REGION1RA = 0x8;      // Write '1' to Enable interrupt on EVENTS_REGION[1].RA event
    static constexpr uint32_t INTENSET_REGION2WA = 0x10;     // Write '1' to Enable interrupt on EVENTS_REGION[2].WA event
    static constexpr uint32_t INTENSET_REGION2RA = 0x20;     // Write '1' to Enable interrupt on EVENTS_REGION[2].RA event
    static constexpr uint32_t INTENSET_REGION3WA = 0x40;     // Write '1' to Enable interrupt on EVENTS_REGION[3].WA event
    static constexpr uint32_t INTENSET_REGION3RA = 0x80;     // Write '1' to Enable interrupt on EVENTS_REGION[3].RA event
    static constexpr uint32_t INTENSET_PREGION0WA = 0x1000000;// Write '1' to Enable interrupt on EVENTS_PREGION[0].WA event
    static constexpr uint32_t INTENSET_PREGION0RA = 0x2000000;// Write '1' to Enable interrupt on EVENTS_PREGION[0].RA event
    static constexpr uint32_t INTENSET_PREGION1WA = 0x4000000;// Write '1' to Enable interrupt on EVENTS_PREGION[1].WA event
    static constexpr uint32_t INTENSET_PREGION1RA = 0x8000000;// Write '1' to Enable interrupt on EVENTS_PREGION[1].RA event

    static constexpr uint32_t INTENCLR_REGION0WA = 0x1;      // Write '1' to Clear interrupt on EVENTS_REGION[0].WA event
    static constexpr uint32_t INTENCLR_REGION0RA = 0x2;      // Write '1' to Clear interrupt on EVENTS_REGION[0].RA event
    static constexpr uint32_t INTENCLR_REGION1WA = 0x4;      // Write '1' to Clear interrupt on EVENTS_REGION[1].WA event
    static constexpr uint32_t INTENCLR_REGION1RA = 0x8;      // Write '1' to Clear interrupt on EVENTS_REGION[1].RA event
    static constexpr uint32_t INTENCLR_REGION2WA = 0x10;     // Write '1' to Clear interrupt on EVENTS_REGION[2].WA event
    static constexpr uint32_t INTENCLR_REGION2RA = 0x20;     // Write '1' to Clear interrupt on EVENTS_REGION[2].RA event
    static constexpr uint32_t INTENCLR_REGION3WA = 0x40;     // Write '1' to Clear interrupt on EVENTS_REGION[3].WA event
    static constexpr uint32_t INTENCLR_REGION3RA = 0x80;     // Write '1' to Clear interrupt on EVENTS_REGION[3].RA event
    static constexpr uint32_t INTENCLR_PREGION0WA = 0x1000000;// Write '1' to Clear interrupt on EVENTS_PREGION[0].WA event
    static constexpr uint32_t INTENCLR_PREGION0RA = 0x2000000;// Write '1' to Clear interrupt on EVENTS_PREGION[0].RA event
    static constexpr uint32_t INTENCLR_PREGION1WA = 0x4000000;// Write '1' to Clear interrupt on EVENTS_PREGION[1].WA event
    static constexpr uint32_t INTENCLR_PREGION1RA = 0x8000000;// Write '1' to Clear interrupt on EVENTS_PREGION[1].RA event

    static constexpr uint32_t NMIEN_REGION0WA = 0x1;      // Enable or disable non-maskable interrupt on EVENTS_REGION[0].WA event
    static constexpr uint32_t NMIEN_REGION0RA = 0x2;      // Enable or disable non-maskable interrupt on EVENTS_REGION[0].RA event
    static constexpr uint32_t NMIEN_REGION1WA = 0x4;      // Enable or disable non-maskable interrupt on EVENTS_REGION[1].WA event
    static constexpr uint32_t NMIEN_REGION1RA = 0x8;      // Enable or disable non-maskable interrupt on EVENTS_REGION[1].RA event
    static constexpr uint32_t NMIEN_REGION2WA = 0x10;     // Enable or disable non-maskable interrupt on EVENTS_REGION[2].WA event
    static constexpr uint32_t NMIEN_REGION2RA = 0x20;     // Enable or disable non-maskable interrupt on EVENTS_REGION[2].RA event
    static constexpr uint32_t NMIEN_REGION3WA = 0x40;     // Enable or disable non-maskable interrupt on EVENTS_REGION[3].WA event
    static constexpr uint32_t NMIEN_REGION3RA = 0x80;     // Enable or disable non-maskable interrupt on EVENTS_REGION[3].RA event
    static constexpr uint32_t NMIEN_PREGION0WA = 0x1000000;// Enable or disable non-maskable interrupt on EVENTS_PREGION[0].WA event
    static constexpr uint32_t NMIEN_PREGION0RA = 0x2000000;// Enable or disable non-maskable interrupt on EVENTS_PREGION[0].RA event
    static constexpr uint32_t NMIEN_PREGION1WA = 0x4000000;// Enable or disable non-maskable interrupt on EVENTS_PREGION[1].WA event
    static constexpr uint32_t NMIEN_PREGION1RA = 0x8000000;// Enable or disable non-maskable interrupt on EVENTS_PREGION[1].RA event

    static constexpr uint32_t NMIENSET_REGION0WA = 0x1;      // Write '1' to Enable non-maskable interrupt on EVENTS_REGION[0].WA event
    static constexpr uint32_t NMIENSET_REGION0RA = 0x2;      // Write '1' to Enable non-maskable interrupt on EVENTS_REGION[0].RA event
    static constexpr uint32_t NMIENSET_REGION1WA = 0x4;      // Write '1' to Enable non-maskable interrupt on EVENTS_REGION[1].WA event
    static constexpr uint32_t NMIENSET_REGION1RA = 0x8;      // Write '1' to Enable non-maskable interrupt on EVENTS_REGION[1].RA event
    static constexpr uint32_t NMIENSET_REGION2WA = 0x10;     // Write '1' to Enable non-maskable interrupt on EVENTS_REGION[2].WA event
    static constexpr uint32_t NMIENSET_REGION2RA = 0x20;     // Write '1' to Enable non-maskable interrupt on EVENTS_REGION[2].RA event
    static constexpr uint32_t NMIENSET_REGION3WA = 0x40;     // Write '1' to Enable non-maskable interrupt on EVENTS_REGION[3].WA event
    static constexpr uint32_t NMIENSET_REGION3RA = 0x80;     // Write '1' to Enable non-maskable interrupt on EVENTS_REGION[3].RA event
    static constexpr uint32_t NMIENSET_PREGION0WA = 0x1000000;// Write '1' to Enable non-maskable interrupt on EVENTS_PREGION[0].WA event
    static constexpr uint32_t NMIENSET_PREGION0RA = 0x2000000;// Write '1' to Enable non-maskable interrupt on EVENTS_PREGION[0].RA event
    static constexpr uint32_t NMIENSET_PREGION1WA = 0x4000000;// Write '1' to Enable non-maskable interrupt on EVENTS_PREGION[1].WA event
    static constexpr uint32_t NMIENSET_PREGION1RA = 0x8000000;// Write '1' to Enable non-maskable interrupt on EVENTS_PREGION[1].RA event

    static constexpr uint32_t NMIENCLR_REGION0WA = 0x1;      // Write '1' to Clear non-maskable interrupt on EVENTS_REGION[0].WA event
    static constexpr uint32_t NMIENCLR_REGION0RA = 0x2;      // Write '1' to Clear non-maskable interrupt on EVENTS_REGION[0].RA event
    static constexpr uint32_t NMIENCLR_REGION1WA = 0x4;      // Write '1' to Clear non-maskable interrupt on EVENTS_REGION[1].WA event
    static constexpr uint32_t NMIENCLR_REGION1RA = 0x8;      // Write '1' to Clear non-maskable interrupt on EVENTS_REGION[1].RA event
    static constexpr uint32_t NMIENCLR_REGION2WA = 0x10;     // Write '1' to Clear non-maskable interrupt on EVENTS_REGION[2].WA event
    static constexpr uint32_t NMIENCLR_REGION2RA = 0x20;     // Write '1' to Clear non-maskable interrupt on EVENTS_REGION[2].RA event
    static constexpr uint32_t NMIENCLR_REGION3WA = 0x40;     // Write '1' to Clear non-maskable interrupt on EVENTS_REGION[3].WA event
    static constexpr uint32_t NMIENCLR_REGION3RA = 0x80;     // Write '1' to Clear non-maskable interrupt on EVENTS_REGION[3].RA event
    static constexpr uint32_t NMIENCLR_PREGION0WA = 0x1000000;// Write '1' to Clear non-maskable interrupt on EVENTS_PREGION[0].WA event
    static constexpr uint32_t NMIENCLR_PREGION0RA = 0x2000000;// Write '1' to Clear non-maskable interrupt on EVENTS_PREGION[0].RA event
    static constexpr uint32_t NMIENCLR_PREGION1WA = 0x4000000;// Write '1' to Clear non-maskable interrupt on EVENTS_PREGION[1].WA event
    static constexpr uint32_t NMIENCLR_PREGION1RA = 0x8000000;// Write '1' to Clear non-maskable interrupt on EVENTS_PREGION[1].RA event

    static constexpr uint32_t REGIONEN_RGN0WA = 0x1;         // Enable/disable write access watch in region[0]
    static constexpr uint32_t REGIONEN_RGN0RA = 0x2;         // Enable/disable read access watch in region[0]
    static constexpr uint32_t REGIONEN_RGN1WA = 0x4;         // Enable/disable write access watch in region[1]
    static constexpr uint32_t REGIONEN_RGN1RA = 0x8;         // Enable/disable read access watch in region[1]
    static constexpr uint32_t REGIONEN_RGN2WA = 0x10;        // Enable/disable write access watch in region[2]
    static constexpr uint32_t REGIONEN_RGN2RA = 0x20;        // Enable/disable read access watch in region[2]
    static constexpr uint32_t REGIONEN_RGN3WA = 0x40;        // Enable/disable write access watch in region[3]
    static constexpr uint32_t REGIONEN_RGN3RA = 0x80;        // Enable/disable read access watch in region[3]
    static constexpr uint32_t REGIONEN_PRGN0WA = 0x1000000;  // Enable/disable write access watch in PREGION[0]
    static constexpr uint32_t REGIONEN_PRGN0RA = 0x2000000;  // Enable/disable read access watch in PREGION[0]
    static constexpr uint32_t REGIONEN_PRGN1WA = 0x4000000;  // Enable/disable write access watch in PREGION[1]
    static constexpr uint32_t REGIONEN_PRGN1RA = 0x8000000;  // Enable/disable read access watch in PREGION[1]

    static constexpr uint32_t REGIONENSET_RGN0WA = 0x1;         // Enable write access watch in region[0]
    static constexpr uint32_t REGIONENSET_RGN0RA = 0x2;         // Enable read access watch in region[0]
    static constexpr uint32_t REGIONENSET_RGN1WA = 0x4;         // Enable write access watch in region[1]
    static constexpr uint32_t REGIONENSET_RGN1RA = 0x8;         // Enable read access watch in region[1]
    static constexpr uint32_t REGIONENSET_RGN2WA = 0x10;        // Enable write access watch in region[2]
    static constexpr uint32_t REGIONENSET_RGN2RA = 0x20;        // Enable read access watch in region[2]
    static constexpr uint32_t REGIONENSET_RGN3WA = 0x40;        // Enable write access watch in region[3]
    static constexpr uint32_t REGIONENSET_RGN3RA = 0x80;        // Enable read access watch in region[3]
    static constexpr uint32_t REGIONENSET_PRGN0WA = 0x1000000;  // Enable write access watch in PREGION[0]
    static constexpr uint32_t REGIONENSET_PRGN0RA = 0x2000000;  // Enable read access watch in PREGION[0]
    static constexpr uint32_t REGIONENSET_PRGN1WA = 0x4000000;  // Enable write access watch in PREGION[1]
    static constexpr uint32_t REGIONENSET_PRGN1RA = 0x8000000;  // Enable read access watch in PREGION[1]

    static constexpr uint32_t REGIONENCLR_RGN0WA = 0x1;         // Disable write access watch in region[0]
    static constexpr uint32_t REGIONENCLR_RGN0RA = 0x2;         // Disable read access watch in region[0]
    static constexpr uint32_t REGIONENCLR_RGN1WA = 0x4;         // Disable write access watch in region[1]
    static constexpr uint32_t REGIONENCLR_RGN1RA = 0x8;         // Disable read access watch in region[1]
    static constexpr uint32_t REGIONENCLR_RGN2WA = 0x10;        // Disable write access watch in region[2]
    static constexpr uint32_t REGIONENCLR_RGN2RA = 0x20;        // Disable read access watch in region[2]
    static constexpr uint32_t REGIONENCLR_RGN3WA = 0x40;        // Disable write access watch in region[3]
    static constexpr uint32_t REGIONENCLR_RGN3RA = 0x80;        // Disable read access watch in region[3]
    static constexpr uint32_t REGIONENCLR_PRGN0WA = 0x1000000;  // Disable write access watch in PREGION[0]
    static constexpr uint32_t REGIONENCLR_PRGN0RA = 0x2000000;  // Disable read access watch in PREGION[0]
    static constexpr uint32_t REGIONENCLR_PRGN1WA = 0x4000000;  // Disable write access watch in PREGION[1]
    static constexpr uint32_t REGIONENCLR_PRGN1RA = 0x8000000;  // Disable read access watch in PREGION[1]

    static constexpr uint8_t MWU = 32; // 
};

static mwu_t& MWU = *reinterpret_cast<mwu_t*>(0x40020000);

#define HAVE_PERIPHERAL_MWU


////
//
//    Pulse Width Modulation Unit 0
//
////

struct pwm1_t
{
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stops PWM pulse generation on all channels at the end of current PWM period, and stops sequence playback
    volatile uint32_t    TASKS_SEQSTART[s];    // [Write-only] Description collection[0]: Loads the first PWM value on all enabled channels from sequence 0, and starts playing that sequence at the rate defined in SEQ[0]REFRESH and/or DECODER.MODE. Causes PWM generation to start it was not running.
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_NEXTSTEP;       // [Write-only] Steps by one value in the current sequence on all enabled channels if DECODER.MODE=NextStep. Does not cause PWM generation to start it was not running.
    reserved_t<60>       _1;
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] Response to STOP task, emitted when PWM pulses are no longer generated
    volatile uint32_t    EVENTS_SEQSTARTED[s]; // [Read-write] Description collection[0]: First PWM period started on sequence 0
    reserved_t<1>        _2;
    volatile uint32_t    EVENTS_SEQEND[s];     // [Read-write] Description collection[0]: Emitted at end of every sequence 0, when last value from RAM has been applied to wave counter
    reserved_t<1>        _3;
    volatile uint32_t    EVENTS_PWMPERIODEND;  // [Read-write] Emitted at the end of each PWM period
    volatile uint32_t    EVENTS_LOOPSDONE;     // [Read-write] Concatenated sequences have been played the amount of times defined in LOOP.CNT
    reserved_t<56>       _4;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<63>       _5;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<125>      _6;
    volatile uint32_t    ENABLE;               // [Read-write] PWM module enable register
    volatile uint32_t    MODE;                 // [Read-write] Selects operating mode of the wave counter
    volatile uint32_t    COUNTERTOP;           // [Read-write] Value up to which the pulse generator counter counts
    volatile uint32_t    PRESCALER;            // [Read-write] Configuration for PWM_CLK
    volatile uint32_t    DECODER;              // [Read-write] Configuration of the decoder
    volatile uint32_t    LOOP;                 // [Read-write] Amount of playback of a loop









    static constexpr uint32_t SHORTS_SEQEND0_STOP = 0x1;   // Shortcut between EVENTS_SEQEND[0] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_SEQEND1_STOP = 0x2;   // Shortcut between EVENTS_SEQEND[1] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_LOOPSDONE_SEQSTART0 = 0x4;// Shortcut between EVENTS_LOOPSDONE event and TASKS_SEQSTART[0] task
    static constexpr uint32_t SHORTS_LOOPSDONE_SEQSTART1 = 0x8;// Shortcut between EVENTS_LOOPSDONE event and TASKS_SEQSTART[1] task
    static constexpr uint32_t SHORTS_LOOPSDONE_STOP = 0x10;// Shortcut between EVENTS_LOOPSDONE event and TASKS_STOP task

    static constexpr uint32_t INTEN_STOPPED = 0x2;        // Enable or disable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTEN_SEQSTARTED0 = 0x4;    // Enable or disable interrupt on EVENTS_SEQSTARTED[0] event
    static constexpr uint32_t INTEN_SEQSTARTED1 = 0x8;    // Enable or disable interrupt on EVENTS_SEQSTARTED[1] event
    static constexpr uint32_t INTEN_SEQEND0 = 0x10;       // Enable or disable interrupt on EVENTS_SEQEND[0] event
    static constexpr uint32_t INTEN_SEQEND1 = 0x20;       // Enable or disable interrupt on EVENTS_SEQEND[1] event
    static constexpr uint32_t INTEN_PWMPERIODEND = 0x40;  // Enable or disable interrupt on EVENTS_PWMPERIODEND event
    static constexpr uint32_t INTEN_LOOPSDONE = 0x80;     // Enable or disable interrupt on EVENTS_LOOPSDONE event

    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_SEQSTARTED0 = 0x4;    // Write '1' to Enable interrupt on EVENTS_SEQSTARTED[0] event
    static constexpr uint32_t INTENSET_SEQSTARTED1 = 0x8;    // Write '1' to Enable interrupt on EVENTS_SEQSTARTED[1] event
    static constexpr uint32_t INTENSET_SEQEND0 = 0x10;       // Write '1' to Enable interrupt on EVENTS_SEQEND[0] event
    static constexpr uint32_t INTENSET_SEQEND1 = 0x20;       // Write '1' to Enable interrupt on EVENTS_SEQEND[1] event
    static constexpr uint32_t INTENSET_PWMPERIODEND = 0x40;  // Write '1' to Enable interrupt on EVENTS_PWMPERIODEND event
    static constexpr uint32_t INTENSET_LOOPSDONE = 0x80;     // Write '1' to Enable interrupt on EVENTS_LOOPSDONE event

    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_SEQSTARTED0 = 0x4;    // Write '1' to Clear interrupt on EVENTS_SEQSTARTED[0] event
    static constexpr uint32_t INTENCLR_SEQSTARTED1 = 0x8;    // Write '1' to Clear interrupt on EVENTS_SEQSTARTED[1] event
    static constexpr uint32_t INTENCLR_SEQEND0 = 0x10;       // Write '1' to Clear interrupt on EVENTS_SEQEND[0] event
    static constexpr uint32_t INTENCLR_SEQEND1 = 0x20;       // Write '1' to Clear interrupt on EVENTS_SEQEND[1] event
    static constexpr uint32_t INTENCLR_PWMPERIODEND = 0x40;  // Write '1' to Clear interrupt on EVENTS_PWMPERIODEND event
    static constexpr uint32_t INTENCLR_LOOPSDONE = 0x80;     // Write '1' to Clear interrupt on EVENTS_LOOPSDONE event

    static constexpr uint32_t ENABLE_ENABLE = 0x1;         // Enable or disable PWM module
    static const uint32_t ENABLE_RESET_VALUE = 0x0;

    static constexpr uint32_t MODE_UPDOWN = 0x1;         // Selects up or up and down as wave counter mode
    static const uint32_t MODE_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t COUNTERTOP_COUNTERTOP =          // Value up to which the pulse generator counter counts. This register is ignored when DECODER.MODE=WaveForm and only values from RAM will be used. (15 bits)
        bit_field_t<0, 0x7fff>::value<X>();
    static const uint32_t COUNTERTOP_RESET_VALUE = 0x3ff;

    template<uint32_t X>
    static constexpr uint32_t PRESCALER_PRESCALER =           // Pre-scaler of PWM_CLK (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t PRESCALER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DECODER_LOAD =                // How a sequence is read from RAM and spread to the compare register (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t DECODER_MODE = 0x100;         // Selects source for advancing the active sequence
    static const uint32_t DECODER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t LOOP_CNT =                 // Amount of playback of pattern cycles (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t LOOP_RESET_VALUE = 0x0;

    static constexpr uint8_t PWM1 = 33; // 
};

static pwm1_t& PWM1 = *reinterpret_cast<pwm1_t*>(0x40021000);

#define HAVE_PERIPHERAL_PWM1


////
//
//    Pulse Width Modulation Unit 0
//
////

struct pwm2_t
{
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stops PWM pulse generation on all channels at the end of current PWM period, and stops sequence playback
    volatile uint32_t    TASKS_SEQSTART[s];    // [Write-only] Description collection[0]: Loads the first PWM value on all enabled channels from sequence 0, and starts playing that sequence at the rate defined in SEQ[0]REFRESH and/or DECODER.MODE. Causes PWM generation to start it was not running.
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_NEXTSTEP;       // [Write-only] Steps by one value in the current sequence on all enabled channels if DECODER.MODE=NextStep. Does not cause PWM generation to start it was not running.
    reserved_t<60>       _1;
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] Response to STOP task, emitted when PWM pulses are no longer generated
    volatile uint32_t    EVENTS_SEQSTARTED[s]; // [Read-write] Description collection[0]: First PWM period started on sequence 0
    reserved_t<1>        _2;
    volatile uint32_t    EVENTS_SEQEND[s];     // [Read-write] Description collection[0]: Emitted at end of every sequence 0, when last value from RAM has been applied to wave counter
    reserved_t<1>        _3;
    volatile uint32_t    EVENTS_PWMPERIODEND;  // [Read-write] Emitted at the end of each PWM period
    volatile uint32_t    EVENTS_LOOPSDONE;     // [Read-write] Concatenated sequences have been played the amount of times defined in LOOP.CNT
    reserved_t<56>       _4;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<63>       _5;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<125>      _6;
    volatile uint32_t    ENABLE;               // [Read-write] PWM module enable register
    volatile uint32_t    MODE;                 // [Read-write] Selects operating mode of the wave counter
    volatile uint32_t    COUNTERTOP;           // [Read-write] Value up to which the pulse generator counter counts
    volatile uint32_t    PRESCALER;            // [Read-write] Configuration for PWM_CLK
    volatile uint32_t    DECODER;              // [Read-write] Configuration of the decoder
    volatile uint32_t    LOOP;                 // [Read-write] Amount of playback of a loop









    static constexpr uint32_t SHORTS_SEQEND0_STOP = 0x1;   // Shortcut between EVENTS_SEQEND[0] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_SEQEND1_STOP = 0x2;   // Shortcut between EVENTS_SEQEND[1] event and TASKS_STOP task
    static constexpr uint32_t SHORTS_LOOPSDONE_SEQSTART0 = 0x4;// Shortcut between EVENTS_LOOPSDONE event and TASKS_SEQSTART[0] task
    static constexpr uint32_t SHORTS_LOOPSDONE_SEQSTART1 = 0x8;// Shortcut between EVENTS_LOOPSDONE event and TASKS_SEQSTART[1] task
    static constexpr uint32_t SHORTS_LOOPSDONE_STOP = 0x10;// Shortcut between EVENTS_LOOPSDONE event and TASKS_STOP task

    static constexpr uint32_t INTEN_STOPPED = 0x2;        // Enable or disable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTEN_SEQSTARTED0 = 0x4;    // Enable or disable interrupt on EVENTS_SEQSTARTED[0] event
    static constexpr uint32_t INTEN_SEQSTARTED1 = 0x8;    // Enable or disable interrupt on EVENTS_SEQSTARTED[1] event
    static constexpr uint32_t INTEN_SEQEND0 = 0x10;       // Enable or disable interrupt on EVENTS_SEQEND[0] event
    static constexpr uint32_t INTEN_SEQEND1 = 0x20;       // Enable or disable interrupt on EVENTS_SEQEND[1] event
    static constexpr uint32_t INTEN_PWMPERIODEND = 0x40;  // Enable or disable interrupt on EVENTS_PWMPERIODEND event
    static constexpr uint32_t INTEN_LOOPSDONE = 0x80;     // Enable or disable interrupt on EVENTS_LOOPSDONE event

    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_SEQSTARTED0 = 0x4;    // Write '1' to Enable interrupt on EVENTS_SEQSTARTED[0] event
    static constexpr uint32_t INTENSET_SEQSTARTED1 = 0x8;    // Write '1' to Enable interrupt on EVENTS_SEQSTARTED[1] event
    static constexpr uint32_t INTENSET_SEQEND0 = 0x10;       // Write '1' to Enable interrupt on EVENTS_SEQEND[0] event
    static constexpr uint32_t INTENSET_SEQEND1 = 0x20;       // Write '1' to Enable interrupt on EVENTS_SEQEND[1] event
    static constexpr uint32_t INTENSET_PWMPERIODEND = 0x40;  // Write '1' to Enable interrupt on EVENTS_PWMPERIODEND event
    static constexpr uint32_t INTENSET_LOOPSDONE = 0x80;     // Write '1' to Enable interrupt on EVENTS_LOOPSDONE event

    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_SEQSTARTED0 = 0x4;    // Write '1' to Clear interrupt on EVENTS_SEQSTARTED[0] event
    static constexpr uint32_t INTENCLR_SEQSTARTED1 = 0x8;    // Write '1' to Clear interrupt on EVENTS_SEQSTARTED[1] event
    static constexpr uint32_t INTENCLR_SEQEND0 = 0x10;       // Write '1' to Clear interrupt on EVENTS_SEQEND[0] event
    static constexpr uint32_t INTENCLR_SEQEND1 = 0x20;       // Write '1' to Clear interrupt on EVENTS_SEQEND[1] event
    static constexpr uint32_t INTENCLR_PWMPERIODEND = 0x40;  // Write '1' to Clear interrupt on EVENTS_PWMPERIODEND event
    static constexpr uint32_t INTENCLR_LOOPSDONE = 0x80;     // Write '1' to Clear interrupt on EVENTS_LOOPSDONE event

    static constexpr uint32_t ENABLE_ENABLE = 0x1;         // Enable or disable PWM module
    static const uint32_t ENABLE_RESET_VALUE = 0x0;

    static constexpr uint32_t MODE_UPDOWN = 0x1;         // Selects up or up and down as wave counter mode
    static const uint32_t MODE_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t COUNTERTOP_COUNTERTOP =          // Value up to which the pulse generator counter counts. This register is ignored when DECODER.MODE=WaveForm and only values from RAM will be used. (15 bits)
        bit_field_t<0, 0x7fff>::value<X>();
    static const uint32_t COUNTERTOP_RESET_VALUE = 0x3ff;

    template<uint32_t X>
    static constexpr uint32_t PRESCALER_PRESCALER =           // Pre-scaler of PWM_CLK (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static const uint32_t PRESCALER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t DECODER_LOAD =                // How a sequence is read from RAM and spread to the compare register (3 bits)
        bit_field_t<0, 0x7>::value<X>();
    static constexpr uint32_t DECODER_MODE = 0x100;         // Selects source for advancing the active sequence
    static const uint32_t DECODER_RESET_VALUE = 0x0;

    template<uint32_t X>
    static constexpr uint32_t LOOP_CNT =                 // Amount of playback of pattern cycles (16 bits)
        bit_field_t<0, 0xffff>::value<X>();
    static const uint32_t LOOP_RESET_VALUE = 0x0;

    static constexpr uint8_t PWM2 = 34; // 
};

static pwm2_t& PWM2 = *reinterpret_cast<pwm2_t*>(0x40022000);

#define HAVE_PERIPHERAL_PWM2


////
//
//    Serial Peripheral Interface Master with EasyDMA 0
//
////

struct spim2_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start SPI transaction
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop SPI transaction
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_SUSPEND;        // [Write-only] Suspend SPI transaction
    volatile uint32_t    TASKS_RESUME;         // [Write-only] Resume SPI transaction
    reserved_t<56>       _1;
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] SPI transaction has stopped
    reserved_t<2>        _2;
    volatile uint32_t    EVENTS_ENDRX;         // [Read-write] End of RXD buffer reached
    reserved_t<1>        _3;
    volatile uint32_t    EVENTS_END;           // [Read-write] End of RXD buffer and TXD buffer reached
    reserved_t<1>        _4;
    volatile uint32_t    EVENTS_ENDTX;         // [Read-write] End of TXD buffer reached
    reserved_t<10>       _5;
    volatile uint32_t    EVENTS_STARTED;       // [Read-write] Transaction started
    reserved_t<44>       _6;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _7;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<125>      _8;
    volatile uint32_t    ENABLE;               // [Read-write] Enable SPIM
    reserved_t<8>        _9;
    volatile uint32_t    FREQUENCY;            // [Read-write] SPI frequency
    reserved_t<11>       _10;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register
    reserved_t<26>       _11;
    volatile uint32_t    ORC;                  // [Read-write] Over-read character. Character clocked out in case and over-read of the TXD buffer.










    static constexpr uint32_t SHORTS_END_START = 0x20000;  // Shortcut between EVENTS_END event and TASKS_START task

    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_ENDRX = 0x10;         // Write '1' to Enable interrupt on EVENTS_ENDRX event
    static constexpr uint32_t INTENSET_END = 0x40;           // Write '1' to Enable interrupt on EVENTS_END event
    static constexpr uint32_t INTENSET_ENDTX = 0x100;        // Write '1' to Enable interrupt on EVENTS_ENDTX event
    static constexpr uint32_t INTENSET_STARTED = 0x80000;    // Write '1' to Enable interrupt on EVENTS_STARTED event

    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_ENDRX = 0x10;         // Write '1' to Clear interrupt on EVENTS_ENDRX event
    static constexpr uint32_t INTENCLR_END = 0x40;           // Write '1' to Clear interrupt on EVENTS_END event
    static constexpr uint32_t INTENCLR_ENDTX = 0x100;        // Write '1' to Clear interrupt on EVENTS_ENDTX event
    static constexpr uint32_t INTENCLR_STARTED = 0x80000;    // Write '1' to Clear interrupt on EVENTS_STARTED event

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable SPIM (4 bits)
        bit_field_t<0, 0xf>::value<X>();


    static const uint32_t FREQUENCY_RESET_VALUE = 0x4000000;

    static constexpr uint32_t CONFIG_ORDER = 0x1;          // Bit order
    static constexpr uint32_t CONFIG_CPHA = 0x2;           // Serial clock (SCK) phase
    static constexpr uint32_t CONFIG_CPOL = 0x4;           // Serial clock (SCK) polarity

    template<uint32_t X>
    static constexpr uint32_t ORC_ORC =                 // Over-read character. Character clocked out in case and over-read of the TXD buffer. (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    static constexpr uint8_t SPIM2_SPIS2_SPI2 = 35; // 
};

static spim2_t& SPIM2 = *reinterpret_cast<spim2_t*>(0x40023000);

#define HAVE_PERIPHERAL_SPIM2


////
//
//    SPI Slave 0
//
////

struct spis2_t
{
    volatile uint32_t    TASKS_ACQUIRE;        // [Write-only] Acquire SPI semaphore
    volatile uint32_t    TASKS_RELEASE;        // [Write-only] Release SPI semaphore, enabling the SPI slave to acquire it
    reserved_t<54>       _0;
    volatile uint32_t    EVENTS_END;           // [Read-write] Granted transaction completed
    reserved_t<8>        _1;
    volatile uint32_t    EVENTS_ACQUIRED;      // [Read-write] Semaphore acquired
    reserved_t<53>       _2;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _3;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<61>       _4;
    volatile uint32_t    SEMSTAT;              // [Read-only] Semaphore status register
    reserved_t<15>       _5;
    volatile uint32_t    STATUS;               // [Read-write] Status from last transaction
    reserved_t<47>       _6;
    volatile uint32_t    ENABLE;               // [Read-write] Enable SPI slave
    reserved_t<20>       _7;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register
    reserved_t<1>        _8;
    volatile uint32_t    DEF;                  // [Read-write] Default character. Character clocked out in case of an ignored transaction.
    reserved_t<24>       _9;
    volatile uint32_t    ORC;                  // [Read-write] Over-read character





    static constexpr uint32_t SHORTS_END_ACQUIRE = 0x4;    // Shortcut between EVENTS_END event and TASKS_ACQUIRE task

    static constexpr uint32_t INTENSET_END = 0x2;            // Write '1' to Enable interrupt on EVENTS_END event
    static constexpr uint32_t INTENSET_ACQUIRED = 0x400;     // Write '1' to Enable interrupt on EVENTS_ACQUIRED event

    static constexpr uint32_t INTENCLR_END = 0x2;            // Write '1' to Clear interrupt on EVENTS_END event
    static constexpr uint32_t INTENCLR_ACQUIRED = 0x400;     // Write '1' to Clear interrupt on EVENTS_ACQUIRED event

    template<uint32_t X>
    static constexpr uint32_t SEMSTAT_SEMSTAT =             // Semaphore status (2 bits)
        bit_field_t<0, 0x3>::value<X>();
    static const uint32_t SEMSTAT_RESET_VALUE = 0x1;

    static constexpr uint32_t STATUS_OVERREAD = 0x1;       // TX buffer over-read detected, and prevented
    static constexpr uint32_t STATUS_OVERFLOW = 0x2;       // RX buffer overflow detected, and prevented

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable SPI slave (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    static constexpr uint32_t CONFIG_ORDER = 0x1;          // Bit order
    static constexpr uint32_t CONFIG_CPHA = 0x2;           // Serial clock (SCK) phase
    static constexpr uint32_t CONFIG_CPOL = 0x4;           // Serial clock (SCK) polarity

    template<uint32_t X>
    static constexpr uint32_t DEF_DEF =                 // Default character. Character clocked out in case of an ignored transaction. (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t ORC_ORC =                 // Over-read character. Character clocked out after an over-read of the transmit buffer. (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    static constexpr uint8_t SPIM2_SPIS2_SPI2 = 35; // 
};

static spis2_t& SPIS2 = *reinterpret_cast<spis2_t*>(0x40023000);

#define HAVE_PERIPHERAL_SPIS2


////
//
//    Serial Peripheral Interface 0
//
////

struct spi2_t
{
    volatile uint32_t    EVENTS_READY;         // [Read-write] TXD byte sent and RXD byte received
    reserved_t<126>      _0;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<125>      _1;
    volatile uint32_t    ENABLE;               // [Read-write] Enable SPI
    reserved_t<5>        _2;
    volatile uint32_t    RXD;                  // [Read-only] RXD register
    volatile uint32_t    TXD;                  // [Read-write] TXD register
    reserved_t<1>        _3;
    volatile uint32_t    FREQUENCY;            // [Read-write] SPI frequency
    reserved_t<11>       _4;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration register


    static constexpr uint32_t INTENSET_READY = 0x4;          // Write '1' to Enable interrupt on EVENTS_READY event

    static constexpr uint32_t INTENCLR_READY = 0x4;          // Write '1' to Clear interrupt on EVENTS_READY event

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable SPI (4 bits)
        bit_field_t<0, 0xf>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t RXD_RXD =                 // RX data received. Double buffered (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t TXD_TXD =                 // TX data to send. Double buffered (8 bits)
        bit_field_t<0, 0xff>::value<X>();


    static const uint32_t FREQUENCY_RESET_VALUE = 0x4000000;

    static constexpr uint32_t CONFIG_ORDER = 0x1;          // Bit order
    static constexpr uint32_t CONFIG_CPHA = 0x2;           // Serial clock (SCK) phase
    static constexpr uint32_t CONFIG_CPOL = 0x4;           // Serial clock (SCK) polarity

    static constexpr uint8_t SPIM2_SPIS2_SPI2 = 35; // 
};

static spi2_t& SPI2 = *reinterpret_cast<spi2_t*>(0x40023000);

#define HAVE_PERIPHERAL_SPI2


////
//
//    Real time counter 0
//
////

struct rtc2_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Start RTC COUNTER
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop RTC COUNTER
    volatile uint32_t    TASKS_CLEAR;          // [Write-only] Clear RTC COUNTER
    volatile uint32_t    TASKS_TRIGOVRFLW;     // [Write-only] Set COUNTER to 0xFFFFF0
    reserved_t<60>       _0;
    volatile uint32_t    EVENTS_TICK;          // [Read-write] Event on COUNTER increment
    volatile uint32_t    EVENTS_OVRFLW;        // [Read-write] Event on COUNTER overflow
    reserved_t<14>       _1;
    volatile uint32_t    EVENTS_COMPARE[s];    // [Read-write] Description collection[0]: Compare event on CC[0] match
    reserved_t<112>      _2;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<13>       _3;
    volatile uint32_t    EVTEN;                // [Read-write] Enable or disable event routing
    volatile uint32_t    EVTENSET;             // [Read-write] Enable event routing
    volatile uint32_t    EVTENCLR;             // [Read-write] Disable event routing
    reserved_t<110>      _4;
    volatile uint32_t    COUNTER;              // [Read-only] Current COUNTER value
    volatile uint32_t    PRESCALER;            // [Read-write] 12 bit prescaler for COUNTER frequency (32768/(PRESCALER+1)).Must be written when RTC is stopped
    reserved_t<13>       _5;
    volatile uint32_t    CC[s];                // [Read-write] Description collection[0]: Compare register 0








    static constexpr uint32_t INTENSET_TICK = 0x1;           // Write '1' to Enable interrupt on EVENTS_TICK event
    static constexpr uint32_t INTENSET_OVRFLW = 0x2;         // Write '1' to Enable interrupt on EVENTS_OVRFLW event
    static constexpr uint32_t INTENSET_COMPARE0 = 0x10000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENSET_COMPARE1 = 0x20000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENSET_COMPARE2 = 0x40000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENSET_COMPARE3 = 0x80000;   // Write '1' to Enable interrupt on EVENTS_COMPARE[3] event

    static constexpr uint32_t INTENCLR_TICK = 0x1;           // Write '1' to Clear interrupt on EVENTS_TICK event
    static constexpr uint32_t INTENCLR_OVRFLW = 0x2;         // Write '1' to Clear interrupt on EVENTS_OVRFLW event
    static constexpr uint32_t INTENCLR_COMPARE0 = 0x10000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[0] event
    static constexpr uint32_t INTENCLR_COMPARE1 = 0x20000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[1] event
    static constexpr uint32_t INTENCLR_COMPARE2 = 0x40000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[2] event
    static constexpr uint32_t INTENCLR_COMPARE3 = 0x80000;   // Write '1' to Clear interrupt on EVENTS_COMPARE[3] event

    static constexpr uint32_t EVTEN_TICK = 0x1;           // Enable or disable event routing on EVENTS_TICK event
    static constexpr uint32_t EVTEN_OVRFLW = 0x2;         // Enable or disable event routing on EVENTS_OVRFLW event
    static constexpr uint32_t EVTEN_COMPARE0 = 0x10000;   // Enable or disable event routing on EVENTS_COMPARE[0] event
    static constexpr uint32_t EVTEN_COMPARE1 = 0x20000;   // Enable or disable event routing on EVENTS_COMPARE[1] event
    static constexpr uint32_t EVTEN_COMPARE2 = 0x40000;   // Enable or disable event routing on EVENTS_COMPARE[2] event
    static constexpr uint32_t EVTEN_COMPARE3 = 0x80000;   // Enable or disable event routing on EVENTS_COMPARE[3] event

    static constexpr uint32_t EVTENSET_TICK = 0x1;           // Write '1' to Enable event routing on EVENTS_TICK event
    static constexpr uint32_t EVTENSET_OVRFLW = 0x2;         // Write '1' to Enable event routing on EVENTS_OVRFLW event
    static constexpr uint32_t EVTENSET_COMPARE0 = 0x10000;   // Write '1' to Enable event routing on EVENTS_COMPARE[0] event
    static constexpr uint32_t EVTENSET_COMPARE1 = 0x20000;   // Write '1' to Enable event routing on EVENTS_COMPARE[1] event
    static constexpr uint32_t EVTENSET_COMPARE2 = 0x40000;   // Write '1' to Enable event routing on EVENTS_COMPARE[2] event
    static constexpr uint32_t EVTENSET_COMPARE3 = 0x80000;   // Write '1' to Enable event routing on EVENTS_COMPARE[3] event

    static constexpr uint32_t EVTENCLR_TICK = 0x1;           // Write '1' to Clear event routing on EVENTS_TICK event
    static constexpr uint32_t EVTENCLR_OVRFLW = 0x2;         // Write '1' to Clear event routing on EVENTS_OVRFLW event
    static constexpr uint32_t EVTENCLR_COMPARE0 = 0x10000;   // Write '1' to Clear event routing on EVENTS_COMPARE[0] event
    static constexpr uint32_t EVTENCLR_COMPARE1 = 0x20000;   // Write '1' to Clear event routing on EVENTS_COMPARE[1] event
    static constexpr uint32_t EVTENCLR_COMPARE2 = 0x40000;   // Write '1' to Clear event routing on EVENTS_COMPARE[2] event
    static constexpr uint32_t EVTENCLR_COMPARE3 = 0x80000;   // Write '1' to Clear event routing on EVENTS_COMPARE[3] event

    template<uint32_t X>
    static constexpr uint32_t COUNTER_COUNTER =             // Counter value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t PRESCALER_PRESCALER =           // Prescaler value (12 bits)
        bit_field_t<0, 0xfff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t CC[%s]_COMPARE =             // Compare value (24 bits)
        bit_field_t<0, 0xffffff>::value<X>();

    static constexpr uint8_t RTC2 = 36; // 
};

static rtc2_t& RTC2 = *reinterpret_cast<rtc2_t*>(0x40024000);

#define HAVE_PERIPHERAL_RTC2


////
//
//    Inter-IC Sound
//
////

struct i2s_t
{
    volatile uint32_t    TASKS_START;          // [Write-only] Starts continuous I&lt;sup&gt;2&lt;/sup&gt;S transfer. Also starts MCK generator when this is enabled.
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stops I&lt;sup&gt;2&lt;/sup&gt;S transfer. Also stops MCK generator. Triggering this task will cause the STOPPED event to be generated.
    reserved_t<63>       _0;
    volatile uint32_t    EVENTS_RXPTRUPD;      // [Read-write] The RXD.PTR register has been copied to internal double-buffers. When the I2S module is started and RX is enabled, this event will be generated for every RXTXD.MAXCNT words that are received on the SDIN pin.
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] I&lt;sup&gt;2&lt;/sup&gt;S transfer stopped.
    reserved_t<2>        _1;
    volatile uint32_t    EVENTS_TXPTRUPD;      // [Read-write] The TDX.PTR register has been copied to internal double-buffers. When the I2S module is started and TX is enabled, this event will be generated for every RXTXD.MAXCNT words that are sent on the SDOUT pin.
    reserved_t<122>      _2;
    volatile uint32_t    INTEN;                // [Read-write] Enable or disable interrupt
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<125>      _3;
    volatile uint32_t    ENABLE;               // [Read-write] Enable I&lt;sup&gt;2&lt;/sup&gt;S module.






    static constexpr uint32_t INTEN_RXPTRUPD = 0x2;       // Enable or disable interrupt on EVENTS_RXPTRUPD event
    static constexpr uint32_t INTEN_STOPPED = 0x4;        // Enable or disable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTEN_TXPTRUPD = 0x20;      // Enable or disable interrupt on EVENTS_TXPTRUPD event

    static constexpr uint32_t INTENSET_RXPTRUPD = 0x2;       // Write '1' to Enable interrupt on EVENTS_RXPTRUPD event
    static constexpr uint32_t INTENSET_STOPPED = 0x4;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_TXPTRUPD = 0x20;      // Write '1' to Enable interrupt on EVENTS_TXPTRUPD event

    static constexpr uint32_t INTENCLR_RXPTRUPD = 0x2;       // Write '1' to Clear interrupt on EVENTS_RXPTRUPD event
    static constexpr uint32_t INTENCLR_STOPPED = 0x4;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_TXPTRUPD = 0x20;      // Write '1' to Clear interrupt on EVENTS_TXPTRUPD event

    static constexpr uint32_t ENABLE_ENABLE = 0x1;         // Enable I&lt;sup&gt;2&lt;/sup&gt;S module.
    static const uint32_t ENABLE_RESET_VALUE = 0x0;

    static constexpr uint8_t I2S = 37; // 
};

static i2s_t& I2S = *reinterpret_cast<i2s_t*>(0x40025000);

#define HAVE_PERIPHERAL_I2S


////
//
//    GPIO Port 1
//
////

struct p0_t
{
    volatile uint32_t    OUT;                  // [Read-write] Write GPIO port
    volatile uint32_t    OUTSET;               // [Read-write] Set individual bits in GPIO port
    volatile uint32_t    OUTCLR;               // [Read-write] Clear individual bits in GPIO port
    volatile uint32_t    IN;                   // [Read-only] Read GPIO port
    volatile uint32_t    DIR;                  // [Read-write] Direction of GPIO pins
    volatile uint32_t    DIRSET;               // [Read-write] DIR set register
    volatile uint32_t    DIRCLR;               // [Read-write] DIR clear register
    volatile uint32_t    LATCH;                // [Read-write] Latch indicating which GPIO pins have met the criteria set in PIN_CNF[n].SENSE register
    volatile uint32_t    DETECTMODE;           // [Read-write] Select between default DETECT signal behaviour and LDETECT mode
    reserved_t<118>      _0;
    volatile uint32_t    PIN_CNF[s];           // [Read-write] Description collection[0]: Configuration of GPIO pins

    static constexpr uint32_t OUT_PIN0 = 0x1;           // P0.0 pin
    static constexpr uint32_t OUT_PIN1 = 0x2;           // P0.1 pin
    static constexpr uint32_t OUT_PIN2 = 0x4;           // P0.2 pin
    static constexpr uint32_t OUT_PIN3 = 0x8;           // P0.3 pin
    static constexpr uint32_t OUT_PIN4 = 0x10;          // P0.4 pin
    static constexpr uint32_t OUT_PIN5 = 0x20;          // P0.5 pin
    static constexpr uint32_t OUT_PIN6 = 0x40;          // P0.6 pin
    static constexpr uint32_t OUT_PIN7 = 0x80;          // P0.7 pin
    static constexpr uint32_t OUT_PIN8 = 0x100;         // P0.8 pin
    static constexpr uint32_t OUT_PIN9 = 0x200;         // P0.9 pin
    static constexpr uint32_t OUT_PIN10 = 0x400;        // P0.10 pin
    static constexpr uint32_t OUT_PIN11 = 0x800;        // P0.11 pin
    static constexpr uint32_t OUT_PIN12 = 0x1000;       // P0.12 pin
    static constexpr uint32_t OUT_PIN13 = 0x2000;       // P0.13 pin
    static constexpr uint32_t OUT_PIN14 = 0x4000;       // P0.14 pin
    static constexpr uint32_t OUT_PIN15 = 0x8000;       // P0.15 pin
    static constexpr uint32_t OUT_PIN16 = 0x10000;      // P0.16 pin
    static constexpr uint32_t OUT_PIN17 = 0x20000;      // P0.17 pin
    static constexpr uint32_t OUT_PIN18 = 0x40000;      // P0.18 pin
    static constexpr uint32_t OUT_PIN19 = 0x80000;      // P0.19 pin
    static constexpr uint32_t OUT_PIN20 = 0x100000;     // P0.20 pin
    static constexpr uint32_t OUT_PIN21 = 0x200000;     // P0.21 pin
    static constexpr uint32_t OUT_PIN22 = 0x400000;     // P0.22 pin
    static constexpr uint32_t OUT_PIN23 = 0x800000;     // P0.23 pin
    static constexpr uint32_t OUT_PIN24 = 0x1000000;    // P0.24 pin
    static constexpr uint32_t OUT_PIN25 = 0x2000000;    // P0.25 pin
    static constexpr uint32_t OUT_PIN26 = 0x4000000;    // P0.26 pin
    static constexpr uint32_t OUT_PIN27 = 0x8000000;    // P0.27 pin
    static constexpr uint32_t OUT_PIN28 = 0x10000000;   // P0.28 pin
    static constexpr uint32_t OUT_PIN29 = 0x20000000;   // P0.29 pin
    static constexpr uint32_t OUT_PIN30 = 0x40000000;   // P0.30 pin
    static constexpr uint32_t OUT_PIN31 = 0x80000000;   // P0.31 pin

    static constexpr uint32_t OUTSET_PIN0 = 0x1;           // P0.0 pin
    static constexpr uint32_t OUTSET_PIN1 = 0x2;           // P0.1 pin
    static constexpr uint32_t OUTSET_PIN2 = 0x4;           // P0.2 pin
    static constexpr uint32_t OUTSET_PIN3 = 0x8;           // P0.3 pin
    static constexpr uint32_t OUTSET_PIN4 = 0x10;          // P0.4 pin
    static constexpr uint32_t OUTSET_PIN5 = 0x20;          // P0.5 pin
    static constexpr uint32_t OUTSET_PIN6 = 0x40;          // P0.6 pin
    static constexpr uint32_t OUTSET_PIN7 = 0x80;          // P0.7 pin
    static constexpr uint32_t OUTSET_PIN8 = 0x100;         // P0.8 pin
    static constexpr uint32_t OUTSET_PIN9 = 0x200;         // P0.9 pin
    static constexpr uint32_t OUTSET_PIN10 = 0x400;        // P0.10 pin
    static constexpr uint32_t OUTSET_PIN11 = 0x800;        // P0.11 pin
    static constexpr uint32_t OUTSET_PIN12 = 0x1000;       // P0.12 pin
    static constexpr uint32_t OUTSET_PIN13 = 0x2000;       // P0.13 pin
    static constexpr uint32_t OUTSET_PIN14 = 0x4000;       // P0.14 pin
    static constexpr uint32_t OUTSET_PIN15 = 0x8000;       // P0.15 pin
    static constexpr uint32_t OUTSET_PIN16 = 0x10000;      // P0.16 pin
    static constexpr uint32_t OUTSET_PIN17 = 0x20000;      // P0.17 pin
    static constexpr uint32_t OUTSET_PIN18 = 0x40000;      // P0.18 pin
    static constexpr uint32_t OUTSET_PIN19 = 0x80000;      // P0.19 pin
    static constexpr uint32_t OUTSET_PIN20 = 0x100000;     // P0.20 pin
    static constexpr uint32_t OUTSET_PIN21 = 0x200000;     // P0.21 pin
    static constexpr uint32_t OUTSET_PIN22 = 0x400000;     // P0.22 pin
    static constexpr uint32_t OUTSET_PIN23 = 0x800000;     // P0.23 pin
    static constexpr uint32_t OUTSET_PIN24 = 0x1000000;    // P0.24 pin
    static constexpr uint32_t OUTSET_PIN25 = 0x2000000;    // P0.25 pin
    static constexpr uint32_t OUTSET_PIN26 = 0x4000000;    // P0.26 pin
    static constexpr uint32_t OUTSET_PIN27 = 0x8000000;    // P0.27 pin
    static constexpr uint32_t OUTSET_PIN28 = 0x10000000;   // P0.28 pin
    static constexpr uint32_t OUTSET_PIN29 = 0x20000000;   // P0.29 pin
    static constexpr uint32_t OUTSET_PIN30 = 0x40000000;   // P0.30 pin
    static constexpr uint32_t OUTSET_PIN31 = 0x80000000;   // P0.31 pin

    static constexpr uint32_t OUTCLR_PIN0 = 0x1;           // P0.0 pin
    static constexpr uint32_t OUTCLR_PIN1 = 0x2;           // P0.1 pin
    static constexpr uint32_t OUTCLR_PIN2 = 0x4;           // P0.2 pin
    static constexpr uint32_t OUTCLR_PIN3 = 0x8;           // P0.3 pin
    static constexpr uint32_t OUTCLR_PIN4 = 0x10;          // P0.4 pin
    static constexpr uint32_t OUTCLR_PIN5 = 0x20;          // P0.5 pin
    static constexpr uint32_t OUTCLR_PIN6 = 0x40;          // P0.6 pin
    static constexpr uint32_t OUTCLR_PIN7 = 0x80;          // P0.7 pin
    static constexpr uint32_t OUTCLR_PIN8 = 0x100;         // P0.8 pin
    static constexpr uint32_t OUTCLR_PIN9 = 0x200;         // P0.9 pin
    static constexpr uint32_t OUTCLR_PIN10 = 0x400;        // P0.10 pin
    static constexpr uint32_t OUTCLR_PIN11 = 0x800;        // P0.11 pin
    static constexpr uint32_t OUTCLR_PIN12 = 0x1000;       // P0.12 pin
    static constexpr uint32_t OUTCLR_PIN13 = 0x2000;       // P0.13 pin
    static constexpr uint32_t OUTCLR_PIN14 = 0x4000;       // P0.14 pin
    static constexpr uint32_t OUTCLR_PIN15 = 0x8000;       // P0.15 pin
    static constexpr uint32_t OUTCLR_PIN16 = 0x10000;      // P0.16 pin
    static constexpr uint32_t OUTCLR_PIN17 = 0x20000;      // P0.17 pin
    static constexpr uint32_t OUTCLR_PIN18 = 0x40000;      // P0.18 pin
    static constexpr uint32_t OUTCLR_PIN19 = 0x80000;      // P0.19 pin
    static constexpr uint32_t OUTCLR_PIN20 = 0x100000;     // P0.20 pin
    static constexpr uint32_t OUTCLR_PIN21 = 0x200000;     // P0.21 pin
    static constexpr uint32_t OUTCLR_PIN22 = 0x400000;     // P0.22 pin
    static constexpr uint32_t OUTCLR_PIN23 = 0x800000;     // P0.23 pin
    static constexpr uint32_t OUTCLR_PIN24 = 0x1000000;    // P0.24 pin
    static constexpr uint32_t OUTCLR_PIN25 = 0x2000000;    // P0.25 pin
    static constexpr uint32_t OUTCLR_PIN26 = 0x4000000;    // P0.26 pin
    static constexpr uint32_t OUTCLR_PIN27 = 0x8000000;    // P0.27 pin
    static constexpr uint32_t OUTCLR_PIN28 = 0x10000000;   // P0.28 pin
    static constexpr uint32_t OUTCLR_PIN29 = 0x20000000;   // P0.29 pin
    static constexpr uint32_t OUTCLR_PIN30 = 0x40000000;   // P0.30 pin
    static constexpr uint32_t OUTCLR_PIN31 = 0x80000000;   // P0.31 pin

    static constexpr uint32_t IN_PIN0 = 0x1;           // P0.0 pin
    static constexpr uint32_t IN_PIN1 = 0x2;           // P0.1 pin
    static constexpr uint32_t IN_PIN2 = 0x4;           // P0.2 pin
    static constexpr uint32_t IN_PIN3 = 0x8;           // P0.3 pin
    static constexpr uint32_t IN_PIN4 = 0x10;          // P0.4 pin
    static constexpr uint32_t IN_PIN5 = 0x20;          // P0.5 pin
    static constexpr uint32_t IN_PIN6 = 0x40;          // P0.6 pin
    static constexpr uint32_t IN_PIN7 = 0x80;          // P0.7 pin
    static constexpr uint32_t IN_PIN8 = 0x100;         // P0.8 pin
    static constexpr uint32_t IN_PIN9 = 0x200;         // P0.9 pin
    static constexpr uint32_t IN_PIN10 = 0x400;        // P0.10 pin
    static constexpr uint32_t IN_PIN11 = 0x800;        // P0.11 pin
    static constexpr uint32_t IN_PIN12 = 0x1000;       // P0.12 pin
    static constexpr uint32_t IN_PIN13 = 0x2000;       // P0.13 pin
    static constexpr uint32_t IN_PIN14 = 0x4000;       // P0.14 pin
    static constexpr uint32_t IN_PIN15 = 0x8000;       // P0.15 pin
    static constexpr uint32_t IN_PIN16 = 0x10000;      // P0.16 pin
    static constexpr uint32_t IN_PIN17 = 0x20000;      // P0.17 pin
    static constexpr uint32_t IN_PIN18 = 0x40000;      // P0.18 pin
    static constexpr uint32_t IN_PIN19 = 0x80000;      // P0.19 pin
    static constexpr uint32_t IN_PIN20 = 0x100000;     // P0.20 pin
    static constexpr uint32_t IN_PIN21 = 0x200000;     // P0.21 pin
    static constexpr uint32_t IN_PIN22 = 0x400000;     // P0.22 pin
    static constexpr uint32_t IN_PIN23 = 0x800000;     // P0.23 pin
    static constexpr uint32_t IN_PIN24 = 0x1000000;    // P0.24 pin
    static constexpr uint32_t IN_PIN25 = 0x2000000;    // P0.25 pin
    static constexpr uint32_t IN_PIN26 = 0x4000000;    // P0.26 pin
    static constexpr uint32_t IN_PIN27 = 0x8000000;    // P0.27 pin
    static constexpr uint32_t IN_PIN28 = 0x10000000;   // P0.28 pin
    static constexpr uint32_t IN_PIN29 = 0x20000000;   // P0.29 pin
    static constexpr uint32_t IN_PIN30 = 0x40000000;   // P0.30 pin
    static constexpr uint32_t IN_PIN31 = 0x80000000;   // P0.31 pin

    static constexpr uint32_t DIR_PIN0 = 0x1;           // P0.0 pin
    static constexpr uint32_t DIR_PIN1 = 0x2;           // P0.1 pin
    static constexpr uint32_t DIR_PIN2 = 0x4;           // P0.2 pin
    static constexpr uint32_t DIR_PIN3 = 0x8;           // P0.3 pin
    static constexpr uint32_t DIR_PIN4 = 0x10;          // P0.4 pin
    static constexpr uint32_t DIR_PIN5 = 0x20;          // P0.5 pin
    static constexpr uint32_t DIR_PIN6 = 0x40;          // P0.6 pin
    static constexpr uint32_t DIR_PIN7 = 0x80;          // P0.7 pin
    static constexpr uint32_t DIR_PIN8 = 0x100;         // P0.8 pin
    static constexpr uint32_t DIR_PIN9 = 0x200;         // P0.9 pin
    static constexpr uint32_t DIR_PIN10 = 0x400;        // P0.10 pin
    static constexpr uint32_t DIR_PIN11 = 0x800;        // P0.11 pin
    static constexpr uint32_t DIR_PIN12 = 0x1000;       // P0.12 pin
    static constexpr uint32_t DIR_PIN13 = 0x2000;       // P0.13 pin
    static constexpr uint32_t DIR_PIN14 = 0x4000;       // P0.14 pin
    static constexpr uint32_t DIR_PIN15 = 0x8000;       // P0.15 pin
    static constexpr uint32_t DIR_PIN16 = 0x10000;      // P0.16 pin
    static constexpr uint32_t DIR_PIN17 = 0x20000;      // P0.17 pin
    static constexpr uint32_t DIR_PIN18 = 0x40000;      // P0.18 pin
    static constexpr uint32_t DIR_PIN19 = 0x80000;      // P0.19 pin
    static constexpr uint32_t DIR_PIN20 = 0x100000;     // P0.20 pin
    static constexpr uint32_t DIR_PIN21 = 0x200000;     // P0.21 pin
    static constexpr uint32_t DIR_PIN22 = 0x400000;     // P0.22 pin
    static constexpr uint32_t DIR_PIN23 = 0x800000;     // P0.23 pin
    static constexpr uint32_t DIR_PIN24 = 0x1000000;    // P0.24 pin
    static constexpr uint32_t DIR_PIN25 = 0x2000000;    // P0.25 pin
    static constexpr uint32_t DIR_PIN26 = 0x4000000;    // P0.26 pin
    static constexpr uint32_t DIR_PIN27 = 0x8000000;    // P0.27 pin
    static constexpr uint32_t DIR_PIN28 = 0x10000000;   // P0.28 pin
    static constexpr uint32_t DIR_PIN29 = 0x20000000;   // P0.29 pin
    static constexpr uint32_t DIR_PIN30 = 0x40000000;   // P0.30 pin
    static constexpr uint32_t DIR_PIN31 = 0x80000000;   // P0.31 pin

    static constexpr uint32_t DIRSET_PIN0 = 0x1;           // Set as output pin 0
    static constexpr uint32_t DIRSET_PIN1 = 0x2;           // Set as output pin 1
    static constexpr uint32_t DIRSET_PIN2 = 0x4;           // Set as output pin 2
    static constexpr uint32_t DIRSET_PIN3 = 0x8;           // Set as output pin 3
    static constexpr uint32_t DIRSET_PIN4 = 0x10;          // Set as output pin 4
    static constexpr uint32_t DIRSET_PIN5 = 0x20;          // Set as output pin 5
    static constexpr uint32_t DIRSET_PIN6 = 0x40;          // Set as output pin 6
    static constexpr uint32_t DIRSET_PIN7 = 0x80;          // Set as output pin 7
    static constexpr uint32_t DIRSET_PIN8 = 0x100;         // Set as output pin 8
    static constexpr uint32_t DIRSET_PIN9 = 0x200;         // Set as output pin 9
    static constexpr uint32_t DIRSET_PIN10 = 0x400;        // Set as output pin 10
    static constexpr uint32_t DIRSET_PIN11 = 0x800;        // Set as output pin 11
    static constexpr uint32_t DIRSET_PIN12 = 0x1000;       // Set as output pin 12
    static constexpr uint32_t DIRSET_PIN13 = 0x2000;       // Set as output pin 13
    static constexpr uint32_t DIRSET_PIN14 = 0x4000;       // Set as output pin 14
    static constexpr uint32_t DIRSET_PIN15 = 0x8000;       // Set as output pin 15
    static constexpr uint32_t DIRSET_PIN16 = 0x10000;      // Set as output pin 16
    static constexpr uint32_t DIRSET_PIN17 = 0x20000;      // Set as output pin 17
    static constexpr uint32_t DIRSET_PIN18 = 0x40000;      // Set as output pin 18
    static constexpr uint32_t DIRSET_PIN19 = 0x80000;      // Set as output pin 19
    static constexpr uint32_t DIRSET_PIN20 = 0x100000;     // Set as output pin 20
    static constexpr uint32_t DIRSET_PIN21 = 0x200000;     // Set as output pin 21
    static constexpr uint32_t DIRSET_PIN22 = 0x400000;     // Set as output pin 22
    static constexpr uint32_t DIRSET_PIN23 = 0x800000;     // Set as output pin 23
    static constexpr uint32_t DIRSET_PIN24 = 0x1000000;    // Set as output pin 24
    static constexpr uint32_t DIRSET_PIN25 = 0x2000000;    // Set as output pin 25
    static constexpr uint32_t DIRSET_PIN26 = 0x4000000;    // Set as output pin 26
    static constexpr uint32_t DIRSET_PIN27 = 0x8000000;    // Set as output pin 27
    static constexpr uint32_t DIRSET_PIN28 = 0x10000000;   // Set as output pin 28
    static constexpr uint32_t DIRSET_PIN29 = 0x20000000;   // Set as output pin 29
    static constexpr uint32_t DIRSET_PIN30 = 0x40000000;   // Set as output pin 30
    static constexpr uint32_t DIRSET_PIN31 = 0x80000000;   // Set as output pin 31

    static constexpr uint32_t DIRCLR_PIN0 = 0x1;           // Set as input pin 0
    static constexpr uint32_t DIRCLR_PIN1 = 0x2;           // Set as input pin 1
    static constexpr uint32_t DIRCLR_PIN2 = 0x4;           // Set as input pin 2
    static constexpr uint32_t DIRCLR_PIN3 = 0x8;           // Set as input pin 3
    static constexpr uint32_t DIRCLR_PIN4 = 0x10;          // Set as input pin 4
    static constexpr uint32_t DIRCLR_PIN5 = 0x20;          // Set as input pin 5
    static constexpr uint32_t DIRCLR_PIN6 = 0x40;          // Set as input pin 6
    static constexpr uint32_t DIRCLR_PIN7 = 0x80;          // Set as input pin 7
    static constexpr uint32_t DIRCLR_PIN8 = 0x100;         // Set as input pin 8
    static constexpr uint32_t DIRCLR_PIN9 = 0x200;         // Set as input pin 9
    static constexpr uint32_t DIRCLR_PIN10 = 0x400;        // Set as input pin 10
    static constexpr uint32_t DIRCLR_PIN11 = 0x800;        // Set as input pin 11
    static constexpr uint32_t DIRCLR_PIN12 = 0x1000;       // Set as input pin 12
    static constexpr uint32_t DIRCLR_PIN13 = 0x2000;       // Set as input pin 13
    static constexpr uint32_t DIRCLR_PIN14 = 0x4000;       // Set as input pin 14
    static constexpr uint32_t DIRCLR_PIN15 = 0x8000;       // Set as input pin 15
    static constexpr uint32_t DIRCLR_PIN16 = 0x10000;      // Set as input pin 16
    static constexpr uint32_t DIRCLR_PIN17 = 0x20000;      // Set as input pin 17
    static constexpr uint32_t DIRCLR_PIN18 = 0x40000;      // Set as input pin 18
    static constexpr uint32_t DIRCLR_PIN19 = 0x80000;      // Set as input pin 19
    static constexpr uint32_t DIRCLR_PIN20 = 0x100000;     // Set as input pin 20
    static constexpr uint32_t DIRCLR_PIN21 = 0x200000;     // Set as input pin 21
    static constexpr uint32_t DIRCLR_PIN22 = 0x400000;     // Set as input pin 22
    static constexpr uint32_t DIRCLR_PIN23 = 0x800000;     // Set as input pin 23
    static constexpr uint32_t DIRCLR_PIN24 = 0x1000000;    // Set as input pin 24
    static constexpr uint32_t DIRCLR_PIN25 = 0x2000000;    // Set as input pin 25
    static constexpr uint32_t DIRCLR_PIN26 = 0x4000000;    // Set as input pin 26
    static constexpr uint32_t DIRCLR_PIN27 = 0x8000000;    // Set as input pin 27
    static constexpr uint32_t DIRCLR_PIN28 = 0x10000000;   // Set as input pin 28
    static constexpr uint32_t DIRCLR_PIN29 = 0x20000000;   // Set as input pin 29
    static constexpr uint32_t DIRCLR_PIN30 = 0x40000000;   // Set as input pin 30
    static constexpr uint32_t DIRCLR_PIN31 = 0x80000000;   // Set as input pin 31



    static constexpr uint32_t DETECTMODE_DETECTMODE = 0x1;     // Select between default DETECT signal behaviour and LDETECT mode

    static constexpr uint32_t PIN_CNF[%s]_DIR = 0x1;            // Pin direction
    static constexpr uint32_t PIN_CNF[%s]_INPUT = 0x2;          // Connect or disconnect input buffer
    template<uint32_t X>
    static constexpr uint32_t PIN_CNF[%s]_PULL =                // Pull configuration (2 bits)
        bit_field_t<2, 0x3>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PIN_CNF[%s]_DRIVE =               // Drive configuration (3 bits)
        bit_field_t<8, 0x7>::value<X>();
    template<uint32_t X>
    static constexpr uint32_t PIN_CNF[%s]_SENSE =               // Pin sensing mechanism (2 bits)
        bit_field_t<16, 0x3>::value<X>();
    static const uint32_t PIN_CNF[%s]_RESET_VALUE = 0x2;
};

static p0_t& P0 = *reinterpret_cast<p0_t*>(0x50000000);

#define HAVE_PERIPHERAL_P0


////
//
//    I2C compatible Two-Wire Interface 0
//
////

struct twi1_t
{
    volatile uint32_t    TASKS_STARTRX;        // [Write-only] Start TWI receive sequence
    reserved_t<1>        _0;
    volatile uint32_t    TASKS_STARTTX;        // [Write-only] Start TWI transmit sequence
    reserved_t<2>        _1;
    volatile uint32_t    TASKS_STOP;           // [Write-only] Stop TWI transaction
    reserved_t<1>        _2;
    volatile uint32_t    TASKS_SUSPEND;        // [Write-only] Suspend TWI transaction
    volatile uint32_t    TASKS_RESUME;         // [Write-only] Resume TWI transaction
    reserved_t<56>       _3;
    volatile uint32_t    EVENTS_STOPPED;       // [Read-write] TWI stopped
    volatile uint32_t    EVENTS_RXDREADY;      // [Read-write] TWI RXD byte received
    reserved_t<4>        _4;
    volatile uint32_t    EVENTS_TXDSENT;       // [Read-write] TWI TXD byte sent
    reserved_t<1>        _5;
    volatile uint32_t    EVENTS_ERROR;         // [Read-write] TWI error
    reserved_t<4>        _6;
    volatile uint32_t    EVENTS_BB;            // [Read-write] TWI byte boundary, generated before each byte that is sent or received
    reserved_t<3>        _7;
    volatile uint32_t    EVENTS_SUSPENDED;     // [Read-write] TWI entered the suspended state
    reserved_t<45>       _8;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _9;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<110>      _10;
    volatile uint32_t    ERRORSRC;             // [Read-write] Error source
    reserved_t<14>       _11;
    volatile uint32_t    ENABLE;               // [Read-write] Enable TWI
    reserved_t<1>        _12;
    volatile uint32_t    PSELSCL;              // [Read-write] Pin select for SCL
    volatile uint32_t    PSELSDA;              // [Read-write] Pin select for SDA
    reserved_t<2>        _13;
    volatile uint32_t    RXD;                  // [Read-only] RXD register
    volatile uint32_t    TXD;                  // [Read-write] TXD register
    reserved_t<1>        _14;
    volatile uint32_t    FREQUENCY;            // [Read-write] TWI frequency
    reserved_t<24>       _15;
    volatile uint32_t    ADDRESS;              // [Read-write] Address used in the TWI transfer












    static constexpr uint32_t SHORTS_BB_SUSPEND = 0x1;     // Shortcut between EVENTS_BB event and TASKS_SUSPEND task
    static constexpr uint32_t SHORTS_BB_STOP = 0x2;        // Shortcut between EVENTS_BB event and TASKS_STOP task

    static constexpr uint32_t INTENSET_STOPPED = 0x2;        // Write '1' to Enable interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENSET_RXDREADY = 0x4;       // Write '1' to Enable interrupt on EVENTS_RXDREADY event
    static constexpr uint32_t INTENSET_TXDSENT = 0x80;       // Write '1' to Enable interrupt on EVENTS_TXDSENT event
    static constexpr uint32_t INTENSET_ERROR = 0x200;        // Write '1' to Enable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENSET_BB = 0x4000;          // Write '1' to Enable interrupt on EVENTS_BB event
    static constexpr uint32_t INTENSET_SUSPENDED = 0x40000;  // Write '1' to Enable interrupt on EVENTS_SUSPENDED event

    static constexpr uint32_t INTENCLR_STOPPED = 0x2;        // Write '1' to Clear interrupt on EVENTS_STOPPED event
    static constexpr uint32_t INTENCLR_RXDREADY = 0x4;       // Write '1' to Clear interrupt on EVENTS_RXDREADY event
    static constexpr uint32_t INTENCLR_TXDSENT = 0x80;       // Write '1' to Clear interrupt on EVENTS_TXDSENT event
    static constexpr uint32_t INTENCLR_ERROR = 0x200;        // Write '1' to Clear interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENCLR_BB = 0x4000;          // Write '1' to Clear interrupt on EVENTS_BB event
    static constexpr uint32_t INTENCLR_SUSPENDED = 0x40000;  // Write '1' to Clear interrupt on EVENTS_SUSPENDED event

    static constexpr uint32_t ERRORSRC_OVERRUN = 0x1;        // Overrun error
    static constexpr uint32_t ERRORSRC_ANACK = 0x2;          // NACK received after sending the address (write '1' to clear)
    static constexpr uint32_t ERRORSRC_DNACK = 0x4;          // NACK received after sending a data byte (write '1' to clear)

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable TWI (4 bits)
        bit_field_t<0, 0xf>::value<X>();


    static const uint32_t PSELSCL_RESET_VALUE = 0xffffffff;


    static const uint32_t PSELSDA_RESET_VALUE = 0xffffffff;

    template<uint32_t X>
    static constexpr uint32_t RXD_RXD =                 // RXD register (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t TXD_TXD =                 // TXD register (8 bits)
        bit_field_t<0, 0xff>::value<X>();


    static const uint32_t FREQUENCY_RESET_VALUE = 0x4000000;

    template<uint32_t X>
    static constexpr uint32_t ADDRESS_ADDRESS =             // Address used in the TWI transfer (7 bits)
        bit_field_t<0, 0x7f>::value<X>();

    static constexpr uint8_t SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1 = 4; // 
};

static twi1_t& TWI1 = *reinterpret_cast<twi1_t*>(0x40004000);

#define HAVE_PERIPHERAL_TWI1


////
//
//    Universal Asynchronous Receiver/Transmitter
//
////

struct uart0_t
{
    volatile uint32_t    TASKS_STARTRX;        // [Write-only] Start UART receiver
    volatile uint32_t    TASKS_STOPRX;         // [Write-only] Stop UART receiver
    volatile uint32_t    TASKS_STARTTX;        // [Write-only] Start UART transmitter
    volatile uint32_t    TASKS_STOPTX;         // [Write-only] Stop UART transmitter
    reserved_t<3>        _0;
    volatile uint32_t    TASKS_SUSPEND;        // [Write-only] Suspend UART
    reserved_t<56>       _1;
    volatile uint32_t    EVENTS_CTS;           // [Read-write] CTS is activated (set low). Clear To Send.
    volatile uint32_t    EVENTS_NCTS;          // [Read-write] CTS is deactivated (set high). Not Clear To Send.
    volatile uint32_t    EVENTS_RXDRDY;        // [Read-write] Data received in RXD
    reserved_t<4>        _2;
    volatile uint32_t    EVENTS_TXDRDY;        // [Read-write] Data sent from TXD
    reserved_t<1>        _3;
    volatile uint32_t    EVENTS_ERROR;         // [Read-write] Error detected
    reserved_t<7>        _4;
    volatile uint32_t    EVENTS_RXTO;          // [Read-write] Receiver timeout
    reserved_t<46>       _5;
    volatile uint32_t    SHORTS;               // [Read-write] Shortcut register
    reserved_t<64>       _6;
    volatile uint32_t    INTENSET;             // [Read-write] Enable interrupt
    volatile uint32_t    INTENCLR;             // [Read-write] Disable interrupt
    reserved_t<93>       _7;
    volatile uint32_t    ERRORSRC;             // [Read-write] Error source
    reserved_t<31>       _8;
    volatile uint32_t    ENABLE;               // [Read-write] Enable UART
    reserved_t<1>        _9;
    volatile uint32_t    PSELRTS;              // [Read-write] Pin select for RTS
    volatile uint32_t    PSELTXD;              // [Read-write] Pin select for TXD
    volatile uint32_t    PSELCTS;              // [Read-write] Pin select for CTS
    volatile uint32_t    PSELRXD;              // [Read-write] Pin select for RXD
    volatile uint32_t    RXD;                  // [Read-only] RXD register
    volatile uint32_t    TXD;                  // [Write-only] TXD register
    reserved_t<1>        _10;
    volatile uint32_t    BAUDRATE;             // [Read-write] Baud rate
    reserved_t<17>       _11;
    volatile uint32_t    CONFIG;               // [Read-write] Configuration of parity and hardware flow control












    static constexpr uint32_t SHORTS_CTS_STARTRX = 0x8;    // Shortcut between EVENTS_CTS event and TASKS_STARTRX task
    static constexpr uint32_t SHORTS_NCTS_STOPRX = 0x10;   // Shortcut between EVENTS_NCTS event and TASKS_STOPRX task

    static constexpr uint32_t INTENSET_CTS = 0x1;            // Write '1' to Enable interrupt on EVENTS_CTS event
    static constexpr uint32_t INTENSET_NCTS = 0x2;           // Write '1' to Enable interrupt on EVENTS_NCTS event
    static constexpr uint32_t INTENSET_RXDRDY = 0x4;         // Write '1' to Enable interrupt on EVENTS_RXDRDY event
    static constexpr uint32_t INTENSET_TXDRDY = 0x80;        // Write '1' to Enable interrupt on EVENTS_TXDRDY event
    static constexpr uint32_t INTENSET_ERROR = 0x200;        // Write '1' to Enable interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENSET_RXTO = 0x20000;       // Write '1' to Enable interrupt on EVENTS_RXTO event

    static constexpr uint32_t INTENCLR_CTS = 0x1;            // Write '1' to Clear interrupt on EVENTS_CTS event
    static constexpr uint32_t INTENCLR_NCTS = 0x2;           // Write '1' to Clear interrupt on EVENTS_NCTS event
    static constexpr uint32_t INTENCLR_RXDRDY = 0x4;         // Write '1' to Clear interrupt on EVENTS_RXDRDY event
    static constexpr uint32_t INTENCLR_TXDRDY = 0x80;        // Write '1' to Clear interrupt on EVENTS_TXDRDY event
    static constexpr uint32_t INTENCLR_ERROR = 0x200;        // Write '1' to Clear interrupt on EVENTS_ERROR event
    static constexpr uint32_t INTENCLR_RXTO = 0x20000;       // Write '1' to Clear interrupt on EVENTS_RXTO event

    static constexpr uint32_t ERRORSRC_OVERRUN = 0x1;        // Overrun error
    static constexpr uint32_t ERRORSRC_PARITY = 0x2;         // Parity error
    static constexpr uint32_t ERRORSRC_FRAMING = 0x4;        // Framing error occurred
    static constexpr uint32_t ERRORSRC_BREAK = 0x8;          // Break condition

    template<uint32_t X>
    static constexpr uint32_t ENABLE_ENABLE =              // Enable or disable UART (4 bits)
        bit_field_t<0, 0xf>::value<X>();


    static const uint32_t PSELRTS_RESET_VALUE = 0xffffffff;


    static const uint32_t PSELTXD_RESET_VALUE = 0xffffffff;


    static const uint32_t PSELCTS_RESET_VALUE = 0xffffffff;


    static const uint32_t PSELRXD_RESET_VALUE = 0xffffffff;

    template<uint32_t X>
    static constexpr uint32_t RXD_RXD =                 // RX data received in previous transfers, double buffered (8 bits)
        bit_field_t<0, 0xff>::value<X>();

    template<uint32_t X>
    static constexpr uint32_t TXD_TXD =                 // TX data to be transferred (8 bits)
        bit_field_t<0, 0xff>::value<X>();


    static const uint32_t BAUDRATE_RESET_VALUE = 0x4000000;

    static constexpr uint32_t CONFIG_HWFC = 0x1;           // Hardware flow control
    template<uint32_t X>
    static constexpr uint32_t CONFIG_PARITY =              // Parity (3 bits)
        bit_field_t<1, 0x7>::value<X>();

    static constexpr uint8_t UARTE0_UART0 = 2; // 
};

static uart0_t& UART0 = *reinterpret_cast<uart0_t*>(0x40002000);

#define HAVE_PERIPHERAL_UART0


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
    , POWER_CLOCK = 0
    , RADIO = 1
    , UARTE0_UART0 = 2
    , SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0 = 3
    , SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1 = 4
    , NFCT = 5
    , GPIOTE = 6
    , SAADC = 7
    , TIMER0 = 8
    , TIMER1 = 9
    , TIMER2 = 10
    , RTC0 = 11
    , TEMP = 12
    , RNG = 13
    , ECB = 14
    , CCM_AAR = 15
    , WDT = 16
    , RTC1 = 17
    , QDEC = 18
    , COMP_LPCOMP = 19
    , SWI0_EGU0 = 20
    , SWI1_EGU1 = 21
    , SWI2_EGU2 = 22
    , SWI3_EGU3 = 23
    , SWI4_EGU4 = 24
    , SWI5_EGU5 = 25
    , TIMER3 = 26
    , TIMER4 = 27
    , PWM0 = 28
    , PDM = 29
    , MWU = 32
    , PWM1 = 33
    , PWM2 = 34
    , SPIM2_SPIS2_SPI2 = 35
    , RTC2 = 36
    , I2S = 37
    };
};
