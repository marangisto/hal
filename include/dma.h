#pragma once

#include "hal.h"

namespace hal
{

namespace dma
{

enum dma_interrupt_status
    { dma_global_interrupt  = 0x1
    , dma_transfer_complete = 0x2
    , dma_half_transfer     = 0x4
    , dma_transfer_error    = 0x8
    };

template<uint8_t NO, uint8_t CH> struct dmamux_traits {};

template<> struct dmamux_traits<1, 1> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C0CR; } };
template<> struct dmamux_traits<1, 2> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C1CR; } };
template<> struct dmamux_traits<1, 3> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C2CR; } };
template<> struct dmamux_traits<1, 4> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C3CR; } };
template<> struct dmamux_traits<1, 5> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C4CR; } };
template<> struct dmamux_traits<1, 6> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C5CR; } };
template<> struct dmamux_traits<1, 7> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C6CR; } };
#if defined(STM32G431)
template<> struct dmamux_traits<1, 8> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C7CR; } };
template<> struct dmamux_traits<2, 1> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C8CR; } };
template<> struct dmamux_traits<2, 2> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C9CR; } };
template<> struct dmamux_traits<2, 3> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C10CR; } };
template<> struct dmamux_traits<2, 4> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C11CR; } };
template<> struct dmamux_traits<2, 5> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C12CR; } };
template<> struct dmamux_traits<2, 6> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C13CR; } };
template<> struct dmamux_traits<2, 7> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C14CR; } };
template<> struct dmamux_traits<2, 8> { static inline volatile uint32_t& CCR() { return device::DMAMUX.C15CR; } };
#endif

template<uint8_t NO> struct dma_traits {};

#if defined(HAVE_PERIPHERAL_DMA2)
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
#else
template<> struct dma_traits<1>
{
    typedef device::dma_t T;
    static inline T& DMA() { return device::DMA; }
};
#endif

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

template<uint8_t NO> struct dma_channel_traits<NO, 2>
{
    typedef typename dma_traits<NO>::T _;
    static inline typename dma_traits<NO>::T& DMA() { return dma_traits<NO>::DMA(); }

    static constexpr uint32_t ISR_TEIF = _::ISR_TEIF2;
    static constexpr uint32_t ISR_HTIF = _::ISR_HTIF2;
    static constexpr uint32_t ISR_TCIF = _::ISR_TCIF2;
    static constexpr uint32_t ISR_GIF = _::ISR_GIF2;

    static constexpr uint32_t IFCR_TEIF = _::IFCR_TEIF2;
    static constexpr uint32_t IFCR_HTIF = _::IFCR_HTIF2;
    static constexpr uint32_t IFCR_TCIF = _::IFCR_TCIF2;
    static constexpr uint32_t IFCR_GIF = _::IFCR_GIF2;

    static inline volatile uint32_t& CCR() { return DMA().CCR2; }
    static inline volatile uint32_t& CNDTR() { return DMA().CNDTR2; }
    static inline volatile uint32_t& CPAR() { return DMA().CPAR2; }
    static inline volatile uint32_t& CMAR() { return DMA().CMAR2; }
};

template<uint8_t NO> struct dma_channel_traits<NO, 3>
{
    typedef typename dma_traits<NO>::T _;
    static inline typename dma_traits<NO>::T& DMA() { return dma_traits<NO>::DMA(); }

    static constexpr uint32_t ISR_TEIF = _::ISR_TEIF3;
    static constexpr uint32_t ISR_HTIF = _::ISR_HTIF3;
    static constexpr uint32_t ISR_TCIF = _::ISR_TCIF3;
    static constexpr uint32_t ISR_GIF = _::ISR_GIF3;

    static constexpr uint32_t IFCR_TEIF = _::IFCR_TEIF3;
    static constexpr uint32_t IFCR_HTIF = _::IFCR_HTIF3;
    static constexpr uint32_t IFCR_TCIF = _::IFCR_TCIF3;
    static constexpr uint32_t IFCR_GIF = _::IFCR_GIF3;

    static inline volatile uint32_t& CCR() { return DMA().CCR3; }
    static inline volatile uint32_t& CNDTR() { return DMA().CNDTR3; }
    static inline volatile uint32_t& CPAR() { return DMA().CPAR3; }
    static inline volatile uint32_t& CMAR() { return DMA().CMAR3; }
};

template<uint8_t NO> struct dma_channel_traits<NO, 4>
{
    typedef typename dma_traits<NO>::T _;
    static inline typename dma_traits<NO>::T& DMA() { return dma_traits<NO>::DMA(); }

    static constexpr uint32_t ISR_TEIF = _::ISR_TEIF4;
    static constexpr uint32_t ISR_HTIF = _::ISR_HTIF4;
    static constexpr uint32_t ISR_TCIF = _::ISR_TCIF4;
    static constexpr uint32_t ISR_GIF = _::ISR_GIF4;

    static constexpr uint32_t IFCR_TEIF = _::IFCR_TEIF4;
    static constexpr uint32_t IFCR_HTIF = _::IFCR_HTIF4;
    static constexpr uint32_t IFCR_TCIF = _::IFCR_TCIF4;
    static constexpr uint32_t IFCR_GIF = _::IFCR_GIF4;

    static inline volatile uint32_t& CCR() { return DMA().CCR4; }
    static inline volatile uint32_t& CNDTR() { return DMA().CNDTR4; }
    static inline volatile uint32_t& CPAR() { return DMA().CPAR4; }
    static inline volatile uint32_t& CMAR() { return DMA().CMAR4; }
};

template<uint8_t NO> struct dma_channel_traits<NO, 5>
{
    typedef typename dma_traits<NO>::T _;
    static inline typename dma_traits<NO>::T& DMA() { return dma_traits<NO>::DMA(); }

    static constexpr uint32_t ISR_TEIF = _::ISR_TEIF5;
    static constexpr uint32_t ISR_HTIF = _::ISR_HTIF5;
    static constexpr uint32_t ISR_TCIF = _::ISR_TCIF5;
    static constexpr uint32_t ISR_GIF = _::ISR_GIF5;

    static constexpr uint32_t IFCR_TEIF = _::IFCR_TEIF5;
    static constexpr uint32_t IFCR_HTIF = _::IFCR_HTIF5;
    static constexpr uint32_t IFCR_TCIF = _::IFCR_TCIF5;
    static constexpr uint32_t IFCR_GIF = _::IFCR_GIF5;

    static inline volatile uint32_t& CCR() { return DMA().CCR5; }
    static inline volatile uint32_t& CNDTR() { return DMA().CNDTR5; }
    static inline volatile uint32_t& CPAR() { return DMA().CPAR5; }
    static inline volatile uint32_t& CMAR() { return DMA().CMAR5; }
};

template<uint8_t NO> struct dma_channel_traits<NO, 6>
{
    typedef typename dma_traits<NO>::T _;
    static inline typename dma_traits<NO>::T& DMA() { return dma_traits<NO>::DMA(); }

    static constexpr uint32_t ISR_TEIF = _::ISR_TEIF6;
    static constexpr uint32_t ISR_HTIF = _::ISR_HTIF6;
    static constexpr uint32_t ISR_TCIF = _::ISR_TCIF6;
    static constexpr uint32_t ISR_GIF = _::ISR_GIF6;

    static constexpr uint32_t IFCR_TEIF = _::IFCR_TEIF6;
    static constexpr uint32_t IFCR_HTIF = _::IFCR_HTIF6;
    static constexpr uint32_t IFCR_TCIF = _::IFCR_TCIF6;
    static constexpr uint32_t IFCR_GIF = _::IFCR_GIF6;

    static inline volatile uint32_t& CCR() { return DMA().CCR6; }
    static inline volatile uint32_t& CNDTR() { return DMA().CNDTR6; }
    static inline volatile uint32_t& CPAR() { return DMA().CPAR6; }
    static inline volatile uint32_t& CMAR() { return DMA().CMAR6; }
};

template<uint8_t NO> struct dma_channel_traits<NO, 7>
{
    typedef typename dma_traits<NO>::T _;
    static inline typename dma_traits<NO>::T& DMA() { return dma_traits<NO>::DMA(); }

    static constexpr uint32_t ISR_TEIF = _::ISR_TEIF7;
    static constexpr uint32_t ISR_HTIF = _::ISR_HTIF7;
    static constexpr uint32_t ISR_TCIF = _::ISR_TCIF7;
    static constexpr uint32_t ISR_GIF = _::ISR_GIF7;

    static constexpr uint32_t IFCR_TEIF = _::IFCR_TEIF7;
    static constexpr uint32_t IFCR_HTIF = _::IFCR_HTIF7;
    static constexpr uint32_t IFCR_TCIF = _::IFCR_TCIF7;
    static constexpr uint32_t IFCR_GIF = _::IFCR_GIF7;

    static inline volatile uint32_t& CCR() { return DMA().CCR7; }
    static inline volatile uint32_t& CNDTR() { return DMA().CNDTR7; }
    static inline volatile uint32_t& CPAR() { return DMA().CPAR7; }
    static inline volatile uint32_t& CMAR() { return DMA().CMAR7; }
};

template<uint8_t NO> struct dma_channel_traits<NO, 8>
{
    typedef typename dma_traits<NO>::T _;
    static inline typename dma_traits<NO>::T& DMA() { return dma_traits<NO>::DMA(); }

    static constexpr uint32_t ISR_TEIF = _::ISR_TEIF8;
    static constexpr uint32_t ISR_HTIF = _::ISR_HTIF8;
    static constexpr uint32_t ISR_TCIF = _::ISR_TCIF8;
    static constexpr uint32_t ISR_GIF = _::ISR_GIF8;

    static constexpr uint32_t IFCR_TEIF = _::IFCR_TEIF8;
    static constexpr uint32_t IFCR_HTIF = _::IFCR_HTIF8;
    static constexpr uint32_t IFCR_TCIF = _::IFCR_TCIF8;
    static constexpr uint32_t IFCR_GIF = _::IFCR_GIF8;

    static inline volatile uint32_t& CCR() { return DMA().CCR8; }
    static inline volatile uint32_t& CNDTR() { return DMA().CNDTR8; }
    static inline volatile uint32_t& CPAR() { return DMA().CPAR8; }
    static inline volatile uint32_t& CMAR() { return DMA().CMAR8; }
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
#if !defined(STM32G070)                                         // no separate dmamux clock enable
        device::peripheral_traits<MUX>::enable();               // enable dma multiplexer
#endif
        device::peripheral_traits<_>::enable();                 // enable dma clock
    }

    template<uint8_t CH, typename T>
    static inline void periph_to_mem(volatile uint32_t *source, volatile T *dest, uint16_t nelem)
    {
        typedef dma_channel_traits<NO, CH> __;

        DMAMUX().CFR &= ~(1 << (CH-1));                                 // clear synchronization overrun flag
        clear_interrupt_flags<CH>();                                    // clear all interrupt flags
        __::CNDTR() = nelem;                                            // set number of data elements
        __::CPAR() = reinterpret_cast<uint32_t>(source);
        __::CMAR() = reinterpret_cast<uint32_t>(dest);

        __::CCR() = _::CCR1_RESET_VALUE                                 // reset channel configuration register
                  | _::CCR1_MINC                                        // set memory increment mode
                  | _::CCR1_CIRC                                        // use circular mode
                  | _::template CCR1_MSIZE<dma_type_size<T>()>          // set memory item size
                  | _::template CCR1_PSIZE<dma_type_size<uint32_t>()>   // set peripheral register size to 32-bits
                  ;
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
    static inline uint32_t interrupt_status()
    {
        uint32_t x = DMA().ISR;

        return ((x & dma_channel_traits<NO, CH>::ISR_GIF)  ? dma_global_interrupt  : 0)
             | ((x & dma_channel_traits<NO, CH>::ISR_TCIF) ? dma_transfer_complete : 0)
             | ((x & dma_channel_traits<NO, CH>::ISR_HTIF) ? dma_half_transfer     : 0)
             | ((x & dma_channel_traits<NO, CH>::ISR_TEIF) ? dma_transfer_error    : 0)
             ;
    }

    template<uint8_t CH>
    static inline void clear_interrupt_flags()
    {
#if defined(STM32G431)
        DMA().IFCR |= dma_channel_traits<NO, CH>::IFCR_GIF;     // clear general interrupt flag
#else
        // FIXME!
#endif
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

