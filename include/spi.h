#pragma once

#include <gpio.h>

namespace stm32f0
{

namespace spi
{

using namespace device;
using namespace gpio;

enum spi_bit_order_t { msb_first, lsb_first };

template<int NO> struct spi_traits {};

template<> struct spi_traits<1>
{
    typedef spi1_t T;
    static inline T& SPI() { return SPI1; }
    static inline void rcc_enable() { RCC.APB2ENR |= BV(rcc_t::APB2ENR_SPI1EN); }
    static inline void nvic_enable() { NVIC.ISER |= BV(25); }
};

template<> struct spi_traits<2>
{
    typedef spi2_t T;
    static inline T& SPI() { return SPI2; }
    static inline void rcc_enable() { RCC.APB1ENR |= BV(rcc_t::APB1ENR_SPI2EN); }
    static inline void nvic_enable() { NVIC.ISER |= BV(26); }
};

template<int NO> struct spi_t
{
private:
    // strangely this can't sit at the bottom
    typedef typename spi_traits<NO>::T _;

public:
    template<port_enum_t PORT, int PIN>
    static inline void setup_nss()
    {
        typedef output_t<PORT, PIN> nss;

        nss::template setup<push_pull>();
    }

    template<spi_bit_order_t BO = msb_first>
    static inline void setup()
    {
        spi_traits<NO>::rcc_enable();
        SPI().CR1 = _::CR1_RESET_VALUE; 
        SPI().CR2 = _::CR2_RESET_VALUE;
        if (BO == lsb_first)
            SPI().CR1 |= BV(_::CR1_LSBFIRST);
        SPI().CR1 |= BV(_::CR1_SPE);
    }

    enum interrupt_t
        { err_interrupt = BV(_::CR2_ERRIE)
        , rx_interrupt = BV(_::CR2_RXNEIE)
        , tx_interrupt = BV(_::CR2_TXEIE)
        };

    static inline void interrupt_enable(uint32_t flags)
    {
        static const uint32_t mask = err_interrupt | rx_interrupt | tx_interrupt;

        SPI().CR2 &= ~mask;
        SPI().CR2 |= flags & mask;
        spi_traits<NO>::nvic_enable();
    }

private:
    static inline typename spi_traits<NO>::T& SPI() { return spi_traits<NO>::SPI(); }
};

}

}

