#pragma once

#include <gpio.h>

namespace hal
{

namespace i2s
{

using namespace device;
using namespace gpio;

enum i2s_standard_t
    { philips_i2s       = 0x0
    , left_justified    = 0x1
    , right_justified   = 0x2
    , pcm_standard      = 0x3
    };

enum i2s_clock_polarity_t { low_level, high_level };

enum i2s_format_t
    { format_16_16 = 0x0
    , format_16_32 = 0x4
    , format_24_32 = 0x1
    , format_32_32 = 0x2
    };

template<int NO> struct i2s_traits {};

template<> struct i2s_traits<1>
{
    typedef spi1_t T;
    static inline T& I2S() { return SPI1; }
    static const gpio::internal::alternate_function_t ck = gpio::internal::I2S1_CK;
    static const gpio::internal::alternate_function_t mck = gpio::internal::I2S1_MCK;
    static const gpio::internal::alternate_function_t sd = gpio::internal::I2S1_SD;
    static const gpio::internal::alternate_function_t ws = gpio::internal::I2S1_WS;
};

template<> struct i2s_traits<2>
{
    typedef spi2_t T;
    static inline T& I2S() { return SPI2; }
    static const gpio::internal::alternate_function_t ck = gpio::internal::I2S2_CK;
    static const gpio::internal::alternate_function_t mck = gpio::internal::I2S2_MCK;
    static const gpio::internal::alternate_function_t sd = gpio::internal::I2S2_SD;
    static const gpio::internal::alternate_function_t ws = gpio::internal::I2S2_WS;
};

template<int NO, gpio_pin_t CK, gpio_pin_t SD, gpio_pin_t WS> struct i2s_t
{
private:
    typedef typename i2s_traits<NO>::T _;

public:
    template
        < i2s_standard_t        standard
        , i2s_clock_polarity_t  polarity
        , i2s_format_t          format
        , uint8_t               divider
        , output_speed_t        speed = high_speed
        >
    static inline void setup()
    {
        using namespace gpio::internal;

        alternate_t<SD, i2s_traits<NO>::sd>::template setup<speed>();
        alternate_t<WS, i2s_traits<NO>::ws>::template setup<speed>();
        alternate_t<CK, i2s_traits<NO>::ck>::template setup<speed>();

        peripheral_traits<_>::enable();                                 // enable i2s clock

        I2S().CR1 = _::CR1_RESET_VALUE;                                 // reset control register 1
        I2S().CR2 = _::CR2_RESET_VALUE;                                 // reset control register 2
        I2S().I2SCFGR = _::I2SCFGR_RESET_VALUE                          // reset i2s configuration register
                      | _::I2SCFGR_I2SMOD                               // enable i2s mode
                      | _::template I2SCFGR_I2SCFG<0x2>                 // master transmit
                      | _::template I2SCFGR_I2SSTD<standard>            // standard selection
                      | (polarity == high_level ? _::I2SCFGR_CKPOL : 0) // enable i2s mode
                      | _::template I2SCFGR_DATLEN<format & 0x3>        // data length
                      | ((format & 0x4) ? _::I2SCFGR_CHLEN : 0)         // 32-bit channel width
                      ;

        static_assert(divider > 3, "I2S clock division must be strictly larger than 3");

        I2S().I2SPR = _::template I2SPR_I2SDIV<(divider >> 1)>          // linear prescaler
                    | ((divider & 0x1) ? _::I2SPR_ODD : 0)              // odd prescaler
                    ;                                                   // FIXME: master clock output enable option

        I2S().I2SCFGR |= _::I2SCFGR_I2SE;                               // enable i2s peripheral

        // note dma and interrupt enable flags are in CR2
    }

    __attribute__((always_inline))
    static inline void write16(uint16_t x)
    {
        while (!(I2S().SR & _::SR_TXE));        // wait until tx buffer is empty
        I2S().DR = x;
    }

    __attribute__((always_inline))
    static inline void write24(uint32_t x)
    {
        while (!(I2S().SR & _::SR_TXE));        // wait until tx buffer is empty
        I2S().DR = x >> 8;
        while (!(I2S().SR & _::SR_TXE));        // wait until tx buffer is empty
        I2S().DR = (x & 0xffff) << 8;
    }

    __attribute__((always_inline))
    static inline void write32(uint32_t x)
    {
        while (!(I2S().SR & _::SR_TXE));        // wait until tx buffer is empty
        I2S().DR = x & 0xffff;
        while (!(I2S().SR & _::SR_TXE));        // wait until tx buffer is empty
        I2S().DR = x >> 16;
    }

    __attribute__((always_inline))
    static inline bool busy()
    {
        return I2S().SR & _::SR_BSY;
    }

    __attribute__((always_inline))
    static inline void wait_idle()
    {
        while ((I2S().SR & (_::SR_BSY | _::SR_TXE)) != _::SR_TXE);
    }

    enum interrupt_t
        { err_interrupt = _::CR2_ERRIE
        , rx_interrupt = _::CR2_RXNEIE
        , tx_interrupt = _::CR2_TXEIE
        };

    static inline void interrupt_enable(uint32_t flags)
    {
        static const uint32_t mask = err_interrupt | rx_interrupt | tx_interrupt;

        I2S().CR2 &= ~mask;
        I2S().CR2 |= flags & mask;
    }

private:
    static inline typename i2s_traits<NO>::T& I2S() { return i2s_traits<NO>::I2S(); }
};

}

}

