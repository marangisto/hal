#pragma once

#include "hal.h"

namespace hal
{

namespace dma
{

template<uint8_t NO, uint8_t CH> struct dmamux_traits {};

template<> struct dmamux_traits<1, 1> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C0CR; } };
template<> struct dmamux_traits<1, 2> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C1CR; } };
template<> struct dmamux_traits<1, 3> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C2CR; } };
template<> struct dmamux_traits<1, 4> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C3CR; } };
template<> struct dmamux_traits<1, 5> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C4CR; } };
template<> struct dmamux_traits<1, 6> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C5CR; } };
template<> struct dmamux_traits<1, 7> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C6CR; } };
template<> struct dmamux_traits<1, 8> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C7CR; } };
template<> struct dmamux_traits<2, 1> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C8CR; } };
template<> struct dmamux_traits<2, 2> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C9CR; } };
template<> struct dmamux_traits<2, 3> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C10CR; } };
template<> struct dmamux_traits<2, 4> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C11CR; } };
template<> struct dmamux_traits<2, 5> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C12CR; } };
template<> struct dmamux_traits<2, 6> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C13CR; } };
template<> struct dmamux_traits<2, 7> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C14CR; } };
template<> struct dmamux_traits<2, 8> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C15CR; } };

template<uint8_t NO> struct dma_traits {};

template<> struct dma_traits<1>
{
    typedef device::dma1_t T;
    static inline T& DMA() { return device::DMA1; }
};

template<> struct dma_traits<2>
{
    typedef device::dma2_t T;
    static inline T& DMA() { return device::DMA2; }
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
    static constexpr uint8_t INST = NO;
    typedef typename dma_traits<NO>::T _;
    static inline typename dma_traits<NO>::T& DMA() { return dma_traits<NO>::DMA(); }

    typedef typename device::dmamux_t MUX;
    static inline MUX& DMAMUX() { return device::DMAMUX; }

    static void setup()
    {
        device::peripheral_traits<MUX>::enable();               // enable dma multiplexer
        device::peripheral_traits<_>::enable();                 // enable dma clock
    }

    template<uint8_t CH, typename T>
    static inline void mem_to_periph(const T *source, uint16_t nelem, volatile uint32_t *dest)
    {
        typedef dma_channel_traits<NO, CH> __;

        DMAMUX().CFR &= ~(1 << (CH-1));                                 // clear synchronization overrun flag
        clear_interrupt_flags<CH>();                                    // clear all interrupt flags
        __::CNDTR() = nelem;                                            // set number of data elements
        __::CPAR() = reinterpret_cast<uint32_t>(dest);
        __::CMAR() = reinterpret_cast<uint32_t>(source);
  
        __::CCR() = _::CCR1_RESET_VALUE                                 // reset channel configuration register
                  | _::CCR1_DIR                                         // direction read from memory, write periphal
                  | _::CCR1_MINC                                        // set memory increment mode
                  | _::CCR1_CIRC                                        // use circular mode
                  | _::template CCR1_MSIZE<dma_type_size<T>()>          // set memory item size
                  | _::template CCR1_PSIZE<dma_type_size<uint32_t>()>   // set peripheral register size to 32-bits
                  ;
    }

    template<uint8_t CH, bool HALF = false>
    static inline void enable_interrupt()
    {
        dma_channel_traits<NO, CH>::CCR() |= _::CCR1_TEIE               // interrupt on transfer error
                                          |  _::CCR1_TCIE               // interrupt on transfer complete
                                          |  (HALF ? _::CCR1_HTIE : 0)  // interrupt on half transfer
                                          ;
    }

    template<uint8_t CH>
    static inline void disable_interrupt()
    {
        dma_channel_traits<NO, CH>::CCR() &= ~(_::CCR1_TEIE | _::CCR1_HTIE | _::CCR1_TCIE);
    }

    template<uint8_t CH>
    static inline void clear_interrupt_flags()
    {
        DMA().IFCR |= dma_channel_traits<NO, CH>::IFCR_GIF;     // clear general interrupt flag
    }

    template<uint8_t CH>
    static inline void enable()
    {
        dma_channel_traits<NO, CH>::CCR() |= _::CCR1_EN;        // enable dma channel
    }

    template<uint8_t CH>
    static inline void disable()
    {
        dma_channel_traits<NO, CH>::CCR() &= ~_::CCR1_EN;       // disable dma channel
    }

    template<uint8_t CH>
    static inline void abort()
    {
        disable_interrupt<CH>();                                // disable dma channel interrupts
        dmamux_traits<NO, CH>::CCR() &= ~MUX::C0CR_SOIE;        // disable synchronization overrun interrupt
        disable<CH>();                                          // disable dma channel
        clear_interrupt_flags<CH>();                            // clear all interrupt flags
        DMAMUX().CFR &= ~(1 << (CH-1));                         // clear synchronization overrun flag
    }
};

} // namespace adc

} // namespace hal

