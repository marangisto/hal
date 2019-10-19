#pragma once

#include <gpio.h>
#include <fifo.h>

namespace hal
{

namespace usart
{

using namespace device;
using namespace gpio;

template<int NO> struct usart_traits {};

template<> struct usart_traits<1>
{
    typedef usart1_t T;
    static inline T& USART() { return USART1; }
    static const gpio::internal::alternate_function_t tx = gpio::internal::USART1_TX;
    static const gpio::internal::alternate_function_t rx = gpio::internal::USART1_RX;
};

template<> struct usart_traits<2>
{
    typedef usart2_t T;
    static inline T& USART() { return USART2; }
    static const gpio::internal::alternate_function_t tx = gpio::internal::USART2_TX;
    static const gpio::internal::alternate_function_t rx = gpio::internal::USART2_RX;
};

#if defined(HAVE_PERIPHERAL_USART3)
template<> struct usart_traits<3>
{
    typedef usart3_t T;
    static inline T& USART() { return USART3; }
    static const gpio::internal::alternate_function_t tx = gpio::internal::USART3_TX;
    static const gpio::internal::alternate_function_t rx = gpio::internal::USART3_RX;
};
#endif

#if defined(HAVE_PERIPHERAL_USART4)
template<> struct usart_traits<4>
{
    typedef usart4_t T;
    static inline T& USART() { return USART4; }
    static const gpio::internal::alternate_function_t tx = gpio::internal::USART4_TX;
    static const gpio::internal::alternate_function_t rx = gpio::internal::USART4_RX;
};
#endif

template<int NO, gpio_pin_t TX, gpio_pin_t RX> struct usart_t
{
private:
    typedef typename usart_traits<NO>::T _;
    typedef fifo_t<char, NO, 16> fifo;

public:
    template
        < uint32_t              baud_rate = 9600
        , uint8_t               data_bits = 8
        , uint8_t               stop_bits = 1
        , bool                  parity = false
        , output_speed_t        speed = high_speed
        >
    static inline void setup()
    {
        using namespace gpio::internal;

        alternate_t<TX, usart_traits<NO>::tx>::template setup<speed>();
        alternate_t<RX, usart_traits<NO>::rx>::template setup<pull_up>();

        peripheral_traits<_>::enable();                 // enable usart peripheral clock
        USART().BRR = sys_clock::freq() / baud_rate;    // set baud-rate FIXME: need clock reference!
        USART().CR1 |= _::CR1_RESET_VALUE               // reset control register 1
                    | _::CR1_TE                         // enable transmitter
                    | _::CR1_RE                         // enable receiver
                    | _::CR1_RXNEIE                     // interrupt on rx buffer not empty
                    | _::CR1_UE                         // enable usart itself
                    ;
        USART().CR2 |= _::CR2_RESET_VALUE;              // reset control register 2
        USART().CR3 |= _::CR3_RESET_VALUE;              // reset control register 3
    }

    __attribute__((always_inline))
    static inline void write(uint8_t x)
    {
        while (!tx_empty());
#if defined(STM32F411) || defined(STM32F103)
        USART().DR = x;
#else
        USART().TDR = x;
#endif
    }

    static inline void write(const char *s)
    {
        while (*s)
            write(*s++);
    }

    static uint32_t write(const char *buf, uint32_t len)
    {
        for (uint32_t i = 0; i < len; ++i)
            write(*buf++);
        return len;
    }

    // call isr in relevant handler
    __attribute__((always_inline))
    static inline void isr()
    {
#if defined(STM32F411) || defined(STM32F103)
        fifo::put(USART().DR);
#else
        fifo::put(USART().RDR);
#endif
    }

    __attribute__((always_inline))
    static inline bool read(char &c)
    {
        return fifo::get(c);
    }

    __attribute__((always_inline))
    static inline bool tx_empty()
    {
#if defined(STM32F411) || defined(STM32F103)
        return USART().SR & _::SR_TXE;
#else
        return USART().ISR & _::ISR_TXE;
#endif
    }

    __attribute__((always_inline))
    static inline bool rx_not_empty()
    {
#if defined(STM32F411) || defined(STM32F103)
        return USART().SR & _::SR_RXNE;
#else
        return USART().ISR & _::ISR_RXNE;
#endif
    }

private:
    static inline typename usart_traits<NO>::T& USART() { return usart_traits<NO>::USART(); }
};

} // namespace usart

} // namespace hal

