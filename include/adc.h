#pragma once

#include "hal.h"
#include "dma.h"

namespace hal
{

namespace adc
{

template<int NO, template<int> typename IMPL>
struct adc_api_t: private IMPL<NO>
{
    typedef IMPL<NO> impl;
    static constexpr uint8_t nulch = impl::nulch;

    template<uint16_t PRESCALE = 4>
    static void setup() { impl::template setup<PRESCALE>(); }

    static void enable() { impl::enable(); }

    template<uint16_t K>
    static void oversample() { impl::template oversample<K>(); }

    template<uint8_t X>
    static void sample_time() { impl::template sample_time<X>(); }

    template< uint8_t S1, uint8_t S2 = nulch, uint8_t S3 = nulch, uint8_t S4 = nulch
            , uint8_t S5 = nulch, uint8_t S6 = nulch, uint8_t S7 = nulch, uint8_t S8 = nulch>
    static void sequence() { impl:: template sequence<S1, S2, S3, S4, S5, S6, S7, S8>(); }

    template<typename DMA, uint8_t DMACH, typename T>
    static void dma(volatile T *dest, uint16_t nelem) { impl::template dma<DMA, DMACH, T>(dest, nelem); }

    template<uint8_t SEL>
    static void trigger() { impl::template trigger<SEL>(); }

    static void start_conversion() { impl::start_conversion(); }

    static uint16_t read() { return impl::read(); }
};

namespace internal
{

template<uint8_t NO> struct adc_traits {};

#if defined(HAVE_PERIPHERAL_ADC2)
template<> struct adc_traits<1>
{
    typedef device::adc1_t T;
    typedef device::adc12_common_t C;
    static inline T& ADC() { return device::ADC1; }
#if defined(HAVE_PERIPHERAL_ADC12_COMMON)
    static inline C& COMMON() { return device::ADC12_COMMON; }
#endif
};

template<> struct adc_traits<2>
{
    typedef device::adc2_t T;
    typedef device::adc12_common_t C;
    static inline T& ADC() { return device::ADC2; }
    static inline C& COMMON() { return device::ADC12_COMMON; }
};
#else
template<> struct adc_traits<1>
{
    typedef device::adc_t T;
    static inline T& ADC() { return device::ADC; }
};
#endif

template<uint16_t> struct prescale_traits {};

template<> struct prescale_traits<1> { static const uint8_t presc = 0x0; };
template<> struct prescale_traits<2> { static const uint8_t presc = 0x1; };
template<> struct prescale_traits<4> { static const uint8_t presc = 0x2; };
template<> struct prescale_traits<6> { static const uint8_t presc = 0x3; };
template<> struct prescale_traits<8> { static const uint8_t presc = 0x4; };
template<> struct prescale_traits<10> { static const uint8_t presc = 0x5; };
template<> struct prescale_traits<12> { static const uint8_t presc = 0x6; };
template<> struct prescale_traits<16> { static const uint8_t presc = 0x7; };
template<> struct prescale_traits<32> { static const uint8_t presc = 0x8; };
template<> struct prescale_traits<64> { static const uint8_t presc = 0x9; };
template<> struct prescale_traits<128> { static const uint8_t presc = 0xa; };
template<> struct prescale_traits<256> { static const uint8_t presc = 0xb; };

template<uint16_t> struct oversampling_traits {};

template<> struct oversampling_traits<2> { static const uint8_t ratio = 0x0; };
template<> struct oversampling_traits<4> { static const uint8_t ratio = 0x1; };
template<> struct oversampling_traits<8> { static const uint8_t ratio = 0x2; };
template<> struct oversampling_traits<16> { static const uint8_t ratio = 0x3; };
template<> struct oversampling_traits<32> { static const uint8_t ratio = 0x4; };
template<> struct oversampling_traits<64> { static const uint8_t ratio = 0x5; };
template<> struct oversampling_traits<128> { static const uint8_t ratio = 0x6; };
template<> struct oversampling_traits<256> { static const uint8_t ratio = 0x7; };

} // namespace internal

#if defined(STM32G070)
#include "adc/g0.h"
template<int NO> using adc_t = adc_api_t<NO, internal::adc_impl_g0>;
#elif defined(STM32G431)
#include "adc/g4.h"
template<int NO> using adc_t = adc_api_t<NO, internal::adc_impl_g4>;
#else
        static_assert(false, "ADC driver not implemented for this MCU");
#endif

} // namespace adc

} // namespace hal

