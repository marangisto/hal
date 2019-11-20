#include <stdlib.h>
#include <math.h>
#include <usart.h>
#include <redirect.h>
#include <cordic.h>
#include <fixed.h>
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

using namespace hal::usart;
using namespace hal::cordic;
using namespace hal::timer;
using namespace hal::gpio;
using namespace hal::adc;
using namespace hal::dac;
using namespace hal::dma;
using namespace fixed;
using hal::sys_clock;

typedef usart_t<2, PA2, PA3> serial;
typedef hal::timer::timer_t<6> dac_tim;
typedef hal::timer::timer_t<7> aux_tim;
typedef hal::timer::timer_t<4> adc_tim;

typedef adc_t<1> adc;
typedef dac_t<1> dac;
typedef dma_t<1> adc_dma;
typedef dma_t<2> dac_dma;
typedef hal::cordic::cordic_t cordic;   // FIXME: leaking device into here?

constexpr uint8_t adc_dma_ch = 1;
constexpr uint8_t dac_dma_ch = 1;

constexpr uint32_t dac_sample_freq = 96000;
constexpr uint16_t half_buffer_size = 32;
constexpr uint16_t buffer_size = half_buffer_size * 2;

static uint16_t output_buffer[buffer_size];

constexpr uint32_t adc_sample_freq = 10000;
static const uint8_t adc_buf_size = 3;
static uint16_t adc_buf[adc_buf_size] = { 2047, 2047, 2047 };

typedef output_t<PB8> led;
typedef output_t<PA10> probe;
typedef analog_t<PA0> ain1;
typedef analog_t<PA1> ain2;
typedef analog_t<PB0> ain3;

typedef encoder_t<3, PA6, PA7> encoder;
typedef button_t<PB5> encoder_btn;
typedef button_t<PB3> push_btn;

template<> void handler<interrupt::USART2>()
{
    serial::isr();
}

template<> void handler<interrupt::TIM7>()
{
    aux_tim::clear_uif();
    encoder_btn::update();
    push_btn::update();
}

template<> void handler<interrupt::TIM4>()
{
    adc_tim::clear_uif();
    probe::set();
}

template<> void handler<interrupt::ADC1_2>()
{
    device::ADC1.ISR |= device::adc1_t::ISR_EOS; // clear end of sequence flag
    probe::clear();
}

template<> void handler<interrupt::TIM6_DACUNDER>()
{
    dac_tim::clear_uif();
    //probe::set();
    //probe::clear();
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
        m_dphi = 2. * freq / dac_sample_freq;
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

struct mixed
{
    static inline float value(float phi)
    {
        switch (m_mode)
        {
            case 0: return sine::value(phi);
            case 1: return triangle::value(phi);
            case 2: return sawtooth::value(phi);
            case 3: return square::value(phi);
            default: return 0;
        }
    }

    static uint8_t m_mode;
};

uint8_t mixed::m_mode = 0;

static signal_generator_t<mixed> sig_gen;

template<> void handler<interrupt::DMA2_CH1>()
{
    uint32_t sts = dac_dma::interrupt_status<dac_dma_ch>();

    dac_dma::clear_interrupt_flags<dac_dma_ch>();

    if (sts & (dma_half_transfer | dma_transfer_complete))
    {
        uint16_t *p = output_buffer + (sts & dma_transfer_complete ? half_buffer_size : 0);

        //probe::set();
        for (uint16_t i = 0; i < half_buffer_size; ++i)
            *p++ = (sig_gen.sample() + 1.01) * 2010.;            // FIXME: correct for clipping
        //probe::clear();
    }
}

static float freq(uint16_t cv)
{
    // cv = [0..4096] corresponding to [-2.5..2.5]V
    static const float one_volt = 4096. / 5;

    return 440. * pow(2., (cv - 2047) / one_volt);
}

int main()
{
    interrupt::enable();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    stdio_t::bind<serial>();
    printf("Welcome to the STM32G431!\n");

    probe::setup();
    led::setup();

    aux_tim::setup(100, 1000);
    aux_tim::update_interrupt_enable();
    hal::nvic<interrupt::TIM7>::enable();

    cordic::setup<cordic::sine, 4>();

    sig_gen.setup();

    ain1::setup();
    ain2::setup();
    ain3::setup();

    encoder::setup<pull_up>(65535);
    encoder_btn::setup<pull_up>();
    push_btn::setup<pull_up>();

    adc_tim::setup(0, sys_clock::freq() / adc_sample_freq - 1);
    adc_tim::master_mode<adc_tim::mm_update>();
    //adc_tim::update_interrupt_enable();
    //hal::nvic<interrupt::TIM4>::enable();

    adc_dma::setup();
    adc::setup();
    adc::sequence<1, 2, 15>();

    device::ADC1.CFGR2 |= device::adc1_t::CFGR2_ROVSE
                       |  device::adc1_t::CFGR2_OVSR<0x3>
                       |  device::adc1_t::CFGR2_OVSS<0x4>
                       ;

    //device::ADC1.IER |= device::adc1_t::IER_EOSIE; // enable end of sequence interrupt
    //hal::nvic<interrupt::ADC1_2>::enable();

    adc::dma<adc_dma, adc_dma_ch>(adc_buf, adc_buf_size);
    adc::trigger<0xc>();        // TIM4_TRGO
    adc::enable();
    adc::start_conversion();

    dac_tim::setup(0, sys_clock::freq() / dac_sample_freq - 1);
    dac_tim::master_mode<dac_tim::mm_update>();

    // enable for sampling frequency probe
    //dac_tim::update_interrupt_enable();
    //hal::nvic<interrupt::TIM6_DACUNDER>::enable();

    dac_dma::setup();
    hal::nvic<interrupt::DMA2_CH1>::enable();

    dac::setup();
    dac::enable_trigger<1, 7>();    // FIXME: use constant for TIM6_TRGO
    dac::enable_dma<1, dac_dma, dac_dma_ch, uint16_t>(output_buffer, buffer_size);
    dac_dma::enable_interrupt<dac_dma_ch, true>();

    for (;;)
    {
        if (push_btn::read())
            mixed::m_mode = (mixed::m_mode + 1) & 0x3;

        float f = freq(adc_buf[2]) + encoder::count();
        float x = adc_buf[0] * (1./2048.) - 1.;

        sig_gen.set_freq(f * (1 + 0.5 * x));

        printf("%7.2f %7.5f %d\n", f, x, encoder::count());
    }
}

