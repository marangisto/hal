#include <stdlib.h>
#include <cstring>
#include <usart.h>
#include <redirect.h>
#include <timer.h>
#include <adc.h>
#include <dac.h>
#include <dma.h>

using hal::sys_tick;
using hal::sys_clock;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::timer;
using namespace hal::adc;
using namespace hal::dac;
using namespace hal::dma;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA10> probe;
typedef analog_t<PA0> ain;
typedef adc_t<1> adc;
typedef dac_t<1> dac;
typedef dma_t<1> adc_dma;

constexpr uint8_t adc_dma_ch = 1;

typedef hal::timer::timer_t<6> tim6;

constexpr uint32_t sample_freq = 48000;

template<> void handler<interrupt::USART2>()
{
    serial::isr();
}

static volatile uint16_t input[8];

template<> void handler<interrupt::TIM6_DACUNDER>()
{
    tim6::clear_uif();
}

template<> void handler<interrupt::ADC1_2>()
{
    uint32_t sts = adc::interrupt_status();

    adc::clear_interrupt_flags();
    probe::set();
    printf("adc interrupt = %lx\n", sts);
    probe::clear();
}

template<> void handler<interrupt::DMA1_CH1>()
{
    uint32_t sts = adc_dma::interrupt_status<adc_dma_ch>();

    adc_dma::clear_interrupt_flags<adc_dma_ch>();
    probe::set();
    probe::clear();

    printf("dma interrupt = %lx\n", sts);
}

int main()
{
    probe::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    interrupt::enable();

    stdio_t::bind<serial>();
    printf("Welcome to the STM32G431!\n");

    tim6::setup(0, sys_clock::freq() / sample_freq - 1);
    tim6::master_mode<tim6::mm_update>();

    // enable for sampling frequency probe
    tim6::update_interrupt_enable();
    hal::nvic<interrupt::TIM6_DACUNDER>::enable();

    adc_dma::setup();
    hal::nvic<interrupt::DMA1_CH1>::enable();

    ain::setup();
    adc::setup();
    adc::enable_dma<adc_dma, adc_dma_ch, uint16_t>(input, sizeof(input) / sizeof(*input));
    adc_dma::enable_interrupt<adc_dma_ch, true>();
    hal::nvic<interrupt::ADC1_2>::enable();
    adc::enable_interrupt();

    //printf("adc = %d\n", adc::read());    // BLOCKS if interrupts are enabled and flags cleared!
    
    adc::enable_trigger<0xd>();     // TIM6_TRGO FIXME: use symbolic names for trigger selection
    adc::start_conversion();
    
    dac::setup();
    dac::enable<1>();

    for (;;)
    {
        //dac::write<1>(aval);
        dac::write<1>(input[0]);
    }
}

