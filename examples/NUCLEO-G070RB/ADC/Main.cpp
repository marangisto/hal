#include <timer.h>
#include <dac.h>
#include <adc.h>
#include <dma.h>

using hal::sys_tick;
using hal::sys_clock;
using namespace hal::gpio;
using namespace hal::timer;
using namespace hal::adc;
using namespace hal::dma;

typedef hal::timer::timer_t<3> trig;
typedef hal::dac::dac_t<1> dac;
typedef hal::dma::dma_t<1> dma;
typedef hal::adc::adc_t<1> adc;
typedef analog_t<PA0> ain0;
typedef analog_t<PA1> ain1;
typedef output_t<PA10> probe;

static const uint8_t adc_dma_ch = 1;
static const uint16_t half_buffer_size = 32;
static const uint16_t buffer_size = half_buffer_size * 2;
static uint16_t adc_buf[buffer_size];

template<> void handler<interrupt::DMA_CHANNEL1>()
{
    uint32_t sts = dma::interrupt_status<adc_dma_ch>();

    dma::clear_interrupt_flags<adc_dma_ch>();

    if (sts & (dma_half_transfer | dma_transfer_complete))
        probe::write(sts & dma_transfer_complete);
}

int main()
{
    ain0::setup();
    ain1::setup();
    probe::setup();
    dac::setup();
    dac::enable<1>();
    dac::enable<2>();
    interrupt::enable();

    dma::setup();
    hal::nvic<interrupt::DMA_CHANNEL1>::enable();

    trig::setup(0, 64);
    trig::master_mode<trig::mm_update>();

    adc::setup();
    adc::sequence<1, 0>();
    adc::dma<dma, adc_dma_ch, uint16_t>(adc_buf, buffer_size);
    adc::trigger<0x3>();
    adc::enable();
    adc::start_conversion();

    for (;;)
    {
        dac::write<1>(adc_buf[0]);
        dac::write<2>(adc_buf[1]);
        sys_tick::delay_ms(1);
    }
}

