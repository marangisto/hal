#include <stdlib.h>
#include <usart.h>
#include <redirect.h>
#include <cordic.h>
#include <timer.h>
#include <button.h>
#include <gpio.h>
#include <adc.h>
#include <dac.h>
#include <dma.h>

namespace std
{
void __throw_bad_function_call() { for (;;); }
}
#include <functional>

using namespace hal::cordic;
using namespace hal::timer;
using namespace hal::gpio;
using namespace hal::adc;
using namespace hal::dac;
using namespace hal::dma;
using hal::sys_clock;

typedef usart_t<2, PA2, PA3> serial;
typedef hal::timer::timer_t<6> tim6;
typedef hal::timer::timer_t<3> aux;

typedef adc_t<1> adc;
typedef dac_t<1> dac;
typedef dma_t<1> dac_dma;
typedef hal::cordic::cordic_t cordic;   // FIXME: leaking device into here?

constexpr uint8_t dac_dma_ch = 1;

constexpr uint32_t sample_freq = 96000;
constexpr uint16_t half_buffer_size = 32;
constexpr uint16_t buffer_size = half_buffer_size * 2;

static uint16_t output_buffer[buffer_size];

typedef button_t<PC13> btn;
typedef output_t<PA5> led;
typedef output_t<PA10> probe;
typedef analog_t<PA0> ain;

template<> void handler<interrupt::USART2>()
{
    serial::isr();
}

template<> void handler<interrupt::TIM3>()
{
    aux::clear_uif();
    btn::update();
}

template<> void handler<interrupt::TIM6_DACUNDER>()
{
    tim6::clear_uif();
    probe::set();
    probe::clear();
}

static volatile float osc_freq = 440.;

static inline uint16_t sine_sample()
{
    static float phi = -1.;                         // normalized phase angle [-1, 1]
    float dphi = 2. * osc_freq / sample_freq;       // angular increment per sample
    float s = q31tof(cordic::compute(ftoq31(phi))); // cordic sine
    uint16_t y = (s + 1.) * 2020.;                  // compute signal value FIXME: why 2047 truncates?

    if ((phi += dphi) >= 1.)                        // advance and wrap around
        phi -= 2.;
    return y;
}

template<typename WAVEGEN>
class signal_generator_t
{
public:
    void setup(float freq = 440.)
    {
        m_phi = -1.;
        set_freq(freq);
    }

    void set_freq(float freq)
    {
        m_dphi = 2. * freq / sample_freq;
    }

    float sample()
    {
        float s = WAVEGEN::value(m_phi);                // generate signal value

        if ((m_phi += m_dphi) >= 1.)                    // advance and wrap around
            m_phi -= 2.;
        return s;
    }

private:
    float           m_phi;
    volatile float  m_dphi;
};

struct sine
{
    static inline float value(float phi)
    {
        return q31tof(cordic::compute(ftoq31(phi)));
    }
};

struct triangle
{
    static inline float value(float phi)
    {
        if (phi < -.5)
            return 2. * phi + 2.;
        else if (phi < .5)
            return -2. * phi;
        else
            return 2 * phi - 2.;
        return phi;
    }
};

struct sawtooth
{
    static inline float value(float phi)
    {
        return phi;
    }
};

struct square
{
    static inline float value(float phi)
    {
        return phi < 0 ? 1. : -1.;
    }
};

static signal_generator_t<sine> sig_gen;

template<> void handler<interrupt::DMA1_CH1>()
{
    uint32_t sts = dac_dma::interrupt_status<dac_dma_ch>();

    dac_dma::clear_interrupt_flags<dac_dma_ch>();

    if (sts & (dma_half_transfer | dma_transfer_complete))
    {
        uint16_t *p = output_buffer + (sts & dma_transfer_complete ? half_buffer_size : 0);

        probe::set();
        for (uint16_t i = 0; i < half_buffer_size; ++i)
            *p++ = (sig_gen.sample() + 1.) * 2020.;            // FIXME: correct for clipping
        probe::clear();
    }
}

int main()
{
    interrupt::enable();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    stdio_t::bind<serial>();
    printf("Welcome to the STM32G431!\n");

    btn::setup<pull_down>();
    probe::setup();
    led::setup();

    cordic::setup<cordic::sine, 4>();

    sig_gen.setup();

    adc::setup();
    ain::setup();

    aux::setup(100, 1000);
    aux::update_interrupt_enable();
    hal::nvic<interrupt::TIM3>::enable();

    tim6::setup(0, sys_clock::freq() / sample_freq - 1);
    tim6::master_mode<tim6::mm_update>();

    // enable for sampling frequency probe
    //tim6::update_interrupt_enable();
    //hal::nvic<interrupt::TIM6_DACUNDER>::enable();

    dac_dma::setup();
    hal::nvic<interrupt::DMA1_CH1>::enable();

    dac::setup();
    dac::enable_trigger<1, 7>();    // FIXME: use constant for TIM6_TRGO
    dac::enable_dma<1, dac_dma, dac_dma_ch, uint16_t>(output_buffer, buffer_size);
    dac_dma::enable_interrupt<dac_dma_ch, true>();

    for (;;)
    {
        if (btn::read())
        {    
            sig_gen.set_freq(led::read() ? 440 : 4186.009); // A4 : C8
            led::toggle();
        }
    }
}

