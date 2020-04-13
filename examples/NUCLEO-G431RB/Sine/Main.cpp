#include <timer.h>
#include <gpio.h>
#include <dac.h>
#include <dma.h>
#include <math.h>

using namespace hal::timer;
using namespace hal::gpio;
using namespace hal::dac;
using namespace hal::dma;
using hal::sys_clock;
using hal::sys_tick;

typedef hal::timer::timer_t<6> tim6;

typedef dac_t<1> dac;
typedef dma_t<1> dac_dma;

constexpr uint8_t dac_dma_ch = 1;

constexpr uint32_t sample_freq = 96000;
constexpr uint16_t half_buffer_size = 32;
constexpr uint16_t buffer_size = half_buffer_size * 2;

static uint16_t output_buffer[buffer_size];

typedef output_t<PA5> led;

template<> void handler<interrupt::TIM6_DACUNDER>()
{
    tim6::clear_uif();
}

struct sine_gen_t
{
    void setup(float freq)
    {
        m_phi = -1.;
        m_dphi = two_pi * freq / sample_freq;
    }

    float sample()
    {
        if ((m_phi += m_dphi) >= two_pi)
            m_phi -= two_pi;
        return sin(m_phi);
    }

    static constexpr float  two_pi = 2. * M_PI;
    float                   m_phi;
    float                   m_dphi;
};

static sine_gen_t sig_gen;

template<> void handler<interrupt::DMA1_CH1>()
{
    uint32_t sts = dac_dma::interrupt_status<dac_dma_ch>();

    dac_dma::clear_interrupt_flags<dac_dma_ch>();

    if (sts & (dma_half_transfer | dma_transfer_complete))
    {
        uint16_t *p = output_buffer + (sts & dma_transfer_complete ? half_buffer_size : 0);

        for (uint16_t i = 0; i < half_buffer_size; ++i)
            *p++ = (1. + 0.97 * sig_gen.sample()) * 0x7ff;  // DAC clipping!
            //*p++ = (1. + sig_gen.sample()) * 0x7ff;
    }
}

int main()
{
    sig_gen.setup(50.0);

    tim6::setup(0, sys_clock::freq() / sample_freq - 1);
    tim6::master_mode<tim6::mm_update>();

    dac_dma::setup();
    hal::nvic<interrupt::DMA1_CH1>::enable();

    dac::setup();
    dac::enable_trigger<1, 7>();    // FIXME: use constant for TIM6_TRGO
    dac::enable_dma<1, dac_dma, dac_dma_ch, uint16_t>(output_buffer, buffer_size);
    dac_dma::enable_interrupt<dac_dma_ch, true>();

    led::setup();

    for (;;)
    {
        led::toggle();
        sys_tick::delay_ms(500);
    }
}

