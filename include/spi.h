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
    static const internal::alternate_function_t sck = internal::SPI1_SCK;
    static const internal::alternate_function_t mosi = internal::SPI1_MOSI;
    static const internal::alternate_function_t miso = internal::SPI1_MISO;
    static const internal::alternate_function_t nss = internal::SPI1_NSS;
};

template<> struct spi_traits<2>
{
    typedef spi2_t T;
    static inline T& SPI() { return SPI2; }
    static inline void rcc_enable() { RCC.APB1ENR |= BV(rcc_t::APB1ENR_SPI2EN); }
    static inline void nvic_enable() { NVIC.ISER |= BV(26); }
    static const internal::alternate_function_t sck = internal::SPI2_SCK;
    static const internal::alternate_function_t mosi = internal::SPI2_MOSI;
    static const internal::alternate_function_t miso = internal::SPI2_MISO;
    static const internal::alternate_function_t nss = internal::SPI2_NSS;
};

template<int NO, gpio_pin_t SCK, gpio_pin_t MOSI> struct spi_t
{
private:
    typedef typename spi_traits<NO>::T _;

public:
    template<spi_bit_order_t BO = msb_first, output_speed_t speed = low_speed>
    static inline void setup()
    {
        internal::alternate_t<SCK, spi_traits<NO>::sck>::template setup<speed>();
        internal::alternate_t<MOSI, spi_traits<NO>::mosi>::template setup<speed>();

        spi_traits<NO>::rcc_enable();           // enable spi clock
        SPI().CR1 = _::CR1_RESET_VALUE;         // reset control register 1
        SPI().CR2 = _::CR2_RESET_VALUE;         // reset control register 2
        SPI().CR1 |= BV(_::CR1_MSTR);           // master mode
        SPI().CR2 |= BV(_::CR2_SSOE);           // ss output enable
        //SPI().CR1 |= (0x7 << _::CR1_BR);        // clock divider
        SPI().CR1 |= BV(_::CR1_BIDIMODE);       // simplex transmission
        SPI().CR1 |= BV(_::CR1_BIDIOE);         // simplex output enabled
        SPI().CR2 |= BV(_::CR2_FRXTH);          // fifo 1/4 (8-bit)
        SPI().CR2 |= 0x7 << _::CR2_DS;          // 8-bit data size
        if (BO == lsb_first)                    // choose bit order
            SPI().CR1 |= BV(_::CR1_LSBFIRST);   // lsb first
        SPI().CR1 |= BV(_::CR1_SPE);            // enable spi
    }

    static inline void write(uint8_t x)
    {
        while (!(SPI().SR & BV(_::SR_TXE)));    // wait until tx buffer is empty
        *reinterpret_cast<volatile uint8_t*>(&SPI().DR) = x;
    }

    static inline bool busy()
    {
        return SPI().SR & BV(_::SR_BSY);
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

