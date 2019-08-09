#pragma once

#include "hal.h"

namespace hal
{

namespace dma
{

template<uint8_t NO> struct dma_traits {};

template<> struct dma_traits<1>
{
    typedef device::dma1_t T;
    static inline T& DMA() { return device::DMA1; }
};

template<uint8_t NO, uint8_t CH> struct dma_channel_traits {};

template<uint8_t NO> struct dma_channel_traits<NO, 1>
{
    typedef typename dma_traits<NO>::T _;
    static inline typename dma_traits<NO>::T& DMA() { return dma_traits<NO>::DMA(); }

    static constexpr uint32_t ISR_TEIF = _::ISR_TEIF1;
    static constexpr uint32_t ISR_HTIF = _::ISR_HTIF1;
    static constexpr uint32_t ISR_TCIF = _::ISR_TCIF1;
    static constexpr uint32_t ISR_GIF = _::ISR_GIF1;

    static constexpr uint32_t IFCR_TEIF = _::IFCR_TEIF1;
    static constexpr uint32_t IFCR_HTIF = _::IFCR_HTIF1;
    static constexpr uint32_t IFCR_TCIF = _::IFCR_TCIF1;
    static constexpr uint32_t IFCR_GIF = _::IFCR_GIF1;

    static inline volatile uint32_t& CCR() { return DMA().CCR1; }
    static inline volatile uint32_t& CNDTR() { return DMA().CNDTR1; }
    static inline volatile uint32_t& CPAR() { return DMA().CPAR1; }
    static inline volatile uint32_t& CMAR() { return DMA().CMAR1; }
};

template<uint8_t W> struct dma_size_bits {};

template<> struct dma_size_bits<1> { static constexpr uint32_t BITS = 0x0; };
template<> struct dma_size_bits<2> { static constexpr uint32_t BITS = 0x1; };
template<> struct dma_size_bits<4> { static constexpr uint32_t BITS = 0x2; };

template<typename T>
static constexpr uint32_t dma_type_size() { return dma_size_bits<sizeof(T)>::BITS; }

template<uint8_t NO>
struct dma_t
{
    typedef typename dma_traits<NO>::T _;
    static inline typename dma_traits<NO>::T& DMA() { return dma_traits<NO>::DMA(); }

    static void setup()
    {
        device::peripheral_traits<_>::enable();                 // enable dma clock

        //DMA().DMA_CR = _::DMA_CR_RESET_VALUE;                 // reset control register
        //DMA().DMA_MCR = _::DMA_MCR_RESET_VALUE;               // reset mode control register
    }

    template<uint8_t CH, typename T>
    static inline void mem_to_periph(const T *source, uint16_t nelem, volatile uint32_t *dest)
    {
        typedef dma_channel_traits<NO, CH> __;

        // constants are the same across all channels, hence we use definitons for 1!
 
        __::CCR() = _::CCR1_RESET_VALUE                         // reset channel configuration register
                  | _::CCR1_MINC                                // set memory increment mode
                  | _::CCR1_CIRC                                // use circular mode
                  | _::template CCR1_MSIZE<dma_type_size<T>()>  // set memory item size
                  | _::template CCR1_PSIZE<dma_type_size<T>()>  // set peripheral item size FIXME: how do we differ from T?
                  ;

        __::CNDTR() = nelem;
        __::CMAR() = reinterpret_cast<uint32_t>(source);
        __::CPAR() = reinterpret_cast<uint32_t>(dest);
    }

    template<uint8_t CH>
    static inline void enable_interrupt()
    {
        dma_channel_traits<NO, CH>::CCR() |= _::CCR1_TEIE;      // interrupt on transfer error
        dma_channel_traits<NO, CH>::CCR() |= _::CCR1_HTIE;      // interrupt on half transfer
        dma_channel_traits<NO, CH>::CCR() |= _::CCR1_TCIE;      // interrupt on transfer complete
    }

    template<uint8_t CH>
    static inline void clear_interrupt_flag()
    {
        DMA().IFCR |= dma_channel_traits<NO, CH>::IFCR_TEIF;    // clear transfer error flag
        DMA().IFCR |= dma_channel_traits<NO, CH>::IFCR_HTIF;    // clear half transfer flag
        DMA().IFCR |= dma_channel_traits<NO, CH>::IFCR_TCIF;    // clear transfer complete flag
        DMA().IFCR |= dma_channel_traits<NO, CH>::IFCR_GIF;     // clear general interrupt flag
    }

    template<uint8_t CH>
    static inline void enable()
    {
        dma_channel_traits<NO, CH>::CCR() |= _::CCR1_EN;        // enable dma channel
    }
};

} // namespace adc

} // namespace hal

