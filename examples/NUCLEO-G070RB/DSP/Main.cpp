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
typedef hal::dma::dma_t<1> dma;

static const uint8_t dac_dma_ch = 1;
static const uint8_t adc_dma_ch = 2;
static const uint16_t half_buffer_size = 32;
static const uint16_t buffer_size = half_buffer_size * 2;
static uint16_t buffer[buffer_size];
static const uint32_t sample_freq = 32000;

typedef button_t<PC13> btn;
typedef output_t<PA5> led;
typedef output_t<PA10> probe;
typedef analog_t<PA0> ain;

template<> void handler<interrupt::TIM3>()
{
    aux::clear_uif();
    btn::update();
}

template<> void handler<interrupt::TIM6_DAC_LPTIM1>()
{
    tim6::clear_uif();
    probe::set();
    probe::clear();
}

template<> void handler<interrupt::DMA_CHANNEL1>()
{
    uint32_t sts = dma::interrupt_status<dac_dma_ch>();

    dma::clear_interrupt_flags<dac_dma_ch>();

    if (sts & (dma_half_transfer | dma_transfer_complete))
        probe::write(sts & dma_transfer_complete);
}

template<> void handler<interrupt::DMA_CHANNEL2_3>()
{
    uint32_t sts = dma::interrupt_status<adc_dma_ch>();

    dma::clear_interrupt_flags<adc_dma_ch>();

    if (sts & (dma_half_transfer | dma_transfer_complete))
        led::write(sts & dma_transfer_complete);
}

int main()
{
    btn::setup<pull_down>();
    probe::setup();
    led::setup();

    adc::setup();
    ain::setup();

    aux::setup(100, 1000);
    aux::update_interrupt_enable();
    hal::nvic<interrupt::TIM3>::enable();

    tim6::setup(0, sys_clock::freq() / sample_freq - 1);
    tim6::master_mode<tim6::mm_update>();

    // enable for sampling frequency probe
    //tim6::update_interrupt_enable();
    //hal::nvic<interrupt::TIM6_DAC_LPTIM1>::enable();

    interrupt::enable();

    dma::setup();
    hal::nvic<interrupt::DMA_CHANNEL1>::enable();
    hal::nvic<interrupt::DMA_CHANNEL2_3>::enable();

    dac::setup();
    dac::enable_trigger<1, 5>();    // FIXME: use constant for TIM6_TRGO
    dac::enable_dma<1, dma, dac_dma_ch, uint16_t>(buffer, buffer_size);

    adc::setup();
    adc::sequence<0>();
    adc::dma<dma, adc_dma_ch, uint16_t>(buffer, buffer_size);
    adc::trigger<0x5>();            // FIXME: use constant for TIM6_TRGO
    adc::enable();
    adc::start_conversion();

    for (;;)
    {
        if (btn::read())
        {
        }
    }
}

