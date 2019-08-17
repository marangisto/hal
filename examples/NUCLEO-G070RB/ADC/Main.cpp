#include <stdlib.h>
#include <cstring>
#include <usart.h>
#include <redirect.h>
#include <timer.h>
#include <dac.h>
#include <adc.h>
#include <dma.h>

using hal::sys_tick;
using hal::sys_clock;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::timer;
using namespace hal::adc;
using namespace hal::dma;

typedef hal::timer::timer_t<3> trig;
typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA5> ld4;
typedef hal::dac::dac_t<1> dac;
typedef hal::dma::dma_t<1> dma;
typedef hal::adc::adc_t<1> adc;
typedef analog_t<PA0> ain0;
typedef analog_t<PA1> ain1;
typedef output_t<PA10> probe;

static const uint8_t adc_dma_ch = 1;
static const uint8_t adc_buf_size = 16;

constexpr uint16_t half_buffer_size = 8;
constexpr uint16_t buffer_size = half_buffer_size * 2;
static uint16_t adc_buf[buffer_size];

template<> void handler<interrupt::USART2>()
{
    ld4::toggle();
    serial::isr();
}

template<> void handler<interrupt::TIM3>()
{
    //probe::toggle();
    trig::clear_uif();
}

template<> void handler<interrupt::DMA_CHANNEL1>()
{
    uint32_t sts = dma::interrupt_status<adc_dma_ch>();

    dma::clear_interrupt_flags<adc_dma_ch>();

    if (sts & (dma_half_transfer | dma_transfer_complete))
    {
        probe::write(sts & dma_transfer_complete);
    }
}

int main()
{
    ain0::setup();
    ain1::setup();
    probe::setup();
    ld4::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    hal::nvic<interrupt::TIM3>::enable();
    stdio_t::bind<serial>();
    interrupt::enable();

    printf("Hello STM32G070!\n");

    trig::setup(100, 10000);
    trig::update_interrupt_enable();

    dac::setup();
    dac::enable<1>();
    dac::enable<2>();

    dma::setup();
    hal::nvic<interrupt::DMA_CHANNEL1>::enable();

    adc::setup();
    printf("isr = %lx\n", adc::isr());
    adc::sequence<0, 1>();
    adc::enable_dma<dma, adc_dma_ch, uint16_t>(adc_buf, buffer_size);
    dma::enable_interrupt<adc_dma_ch, true>();
    //adc::enable_trigger<0x3>();     // TIM3_TRGO

    for (;;)
    {
        adc::start_conversion();
        while (!adc::sequence_complete());

        dac::write<1>(adc_buf[0]);
        dac::write<2>(adc_buf[1]);
        //printf("isr = %lx\n", adc::isr());
        sys_tick::delay_ms(1);
        //sys_clock::delay_us(1000);
        //while (!adc::ready());
    }
}

