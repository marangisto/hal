#include <timer.h>
#include <button.h>
#include <gpio.h>
#include <adc.h>
#include <dac.h>
#include <dma.h>

using namespace hal::timer;
using namespace hal::gpio;
using namespace hal::adc;
using namespace hal::dac;
using namespace hal::dma;
using hal::sys_clock;

typedef timer_t<6> tim6;
typedef timer_t<3> aux;

typedef hal::adc::adc_t<1> adc;
typedef hal::dac::dac_t<1> dac;
typedef hal::dma::dma_t<1> adc_dma;
typedef hal::dma::dma_t<2> dac_dma;

static const uint8_t dac_dma_ch = 1;
static const uint8_t adc_dma_ch = 1;
static const uint16_t half_buffer_size = 256;
static const uint16_t buffer_size = half_buffer_size * 2;
static uint16_t input_buffer[buffer_size];
static uint16_t output_buffer[buffer_size];
static const uint32_t sample_freq = 192000;

typedef button_t<PB3> btn;
typedef output_t<PB8> led;
typedef output_t<PB4> probe;
typedef analog_t<PA0> ain;

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

template<> void handler<interrupt::DMA1_CH1>()
{
    uint32_t sts = adc_dma::interrupt_status<adc_dma_ch>();

    adc_dma::clear_interrupt_flags<adc_dma_ch>();
    probe::set();

    if (sts & (dma_half_transfer | dma_transfer_complete))
    {
        uint16_t *p = input_buffer + (sts & dma_transfer_complete ? half_buffer_size : 0);
        uint16_t *q = output_buffer + (sts & dma_transfer_complete ? half_buffer_size : 0);

        for (uint16_t i = 0; i < half_buffer_size; ++i)
            *q++ = *p++;
    }

    probe::clear();
}

int main()
{
    btn::setup<pull_down>();
    probe::setup();
    led::setup();

    aux::setup(100, 1000);
    aux::update_interrupt_enable();
    hal::nvic<interrupt::TIM3>::enable();

    interrupt::enable();

    tim6::setup(0, sys_clock::freq() / sample_freq - 1);
    tim6::master_mode<tim6::mm_update>();
    // enable for sampling frequency probe
    //tim6::update_interrupt_enable();
    //hal::nvic<interrupt::TIM6_DACUNDER>::enable();

    dac_dma::setup();
    adc_dma::setup();
    hal::nvic<interrupt::DMA1_CH1>::enable();

    dac::setup();
    dac::enable_trigger<1, 7>();    // FIXME: use constant for TIM6_TRGO
    dac::enable_dma<1, dac_dma, dac_dma_ch>(output_buffer, buffer_size);

    ain::setup();
    adc::setup();
    adc::sequence<1>();
    adc::dma<adc_dma, adc_dma_ch>(input_buffer, buffer_size);
    adc::trigger<0xd>();            // FIXME: use constant for TIM6_TRGO
    adc::enable();
    adc::start_conversion();

    for (;;)
    {
        if (btn::read())
        {
        }
    }
}

