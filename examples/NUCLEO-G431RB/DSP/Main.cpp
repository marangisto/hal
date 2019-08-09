#include <stdlib.h>
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
typedef hal::timer::timer_t<6> sample_timer;
typedef output_t<PA10> d2;
typedef analog_t<PA0> ain;
typedef adc_t<1> adc;
typedef dac_t<1> dac;
typedef dma_t<1> dma;

void loop();

template<> void handler<interrupt::USART2>()
{
    serial::isr();
}

template<> void handler<interrupt::TIM6_DACUNDER>()
{
    sample_timer::clear_uif();
}

template<> void handler<interrupt::DMA1_CH1>()
{
    dma::clear_interrupt_flag<1>();
    d2::set();
    d2::clear();
}

static constexpr uint32_t buffer_size = 5;
static uint16_t output_buffer[buffer_size] = { 0, 1000, 2000, 3000, 4000 };

int main()
{
    d2::setup();
 
    adc::setup();
    ain::setup();

    dac::setup();
    dac::enable_dma<1>();
    dac::enable_trigger<1, 0x7>();  // TIM6_TRGO
    dac::enable<1>();
    dac::enable<2>();

    dma::setup();
    dma::mem_to_periph<1>(output_buffer, buffer_size, &device::DAC1.DAC_DHR12R1);
    dma::enable_interrupt<1>(); 

    sample_timer::setup(8, 196);
    sample_timer::master_mode<sample_timer::mm_update>();
    sample_timer::update_interrupt_enable();

    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    hal::nvic<interrupt::TIM6_DACUNDER>::enable();
    hal::nvic<interrupt::DMA1_CH1>::enable();
    interrupt::enable();
    stdio_t::bind<serial>();

    printf("Welcome to the STM32G431!\n");

    dma::enable<1>();       // enable dma transfer on channel 1

    for (;;)
        loop();
}

void loop()
{
    adc::start_conversion();
    while (!adc::end_of_conversion());
    uint16_t y = adc::read();
    //dac::write<1>(y);
    dac::write<2>(4095-y);
}

