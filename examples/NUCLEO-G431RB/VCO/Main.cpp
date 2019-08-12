#include <stdlib.h>
#include <usart.h>
#include <redirect.h>
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

typedef usart_t<2, PA2, PA3> serial;
typedef hal::timer::timer_t<6> tim6;
typedef hal::timer::timer_t<3> aux;

typedef adc_t<1> adc;
typedef dac_t<1> dac;
typedef dma_t<1> dac_dma;

constexpr uint8_t dac_dma_ch = 1;

constexpr uint32_t sample_freq = 96000;

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

template<> void handler<interrupt::DMA1_CH1>()
{
    uint32_t sts = dac_dma::interrupt_status<dac_dma_ch>();

    dac_dma::clear_interrupt_flags<dac_dma_ch>();

    if (sts & dma_half_transfer)
    {
        probe::set();
    }
    else if (sts & dma_transfer_complete)
    {
        probe::clear();
    }

}

static uint16_t sine[60] =
    { 0x07ff,0x08cb,0x0994,0x0a5a,0x0b18,0x0bce,0x0c79,0x0d18,0x0da8,0x0e29,0x0e98,0x0ef4
    , 0x0f3e,0x0f72,0x0f92,0x0f9d,0x0f92,0x0f72,0x0f3e,0x0ef4,0x0e98,0x0e29,0x0da8,0x0d18
    , 0x0c79,0x0bce,0x0b18,0x0a5a,0x0994,0x08cb,0x07ff,0x0733,0x066a,0x05a4,0x04e6,0x0430
    , 0x0385,0x02e6,0x0256,0x01d5,0x0166,0x010a,0x00c0,0x008c,0x006c,0x0061,0x006c,0x008c
    , 0x00c0,0x010a,0x0166,0x01d5,0x0256,0x02e6,0x0385,0x0430,0x04e6,0x05a4,0x066a,0x0733
    };

int main()
{
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    stdio_t::bind<serial>();
    printf("Welcome to the STM32G431!\n");

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
    //hal::nvic<interrupt::TIM6_DACUNDER>::enable();

    interrupt::enable();

    dac_dma::setup();
    hal::nvic<interrupt::DMA1_CH1>::enable();

    dac::setup();
    dac::enable_trigger<1, 7>();    // FIXME: use constant for TIM6_TRGO
    dac::enable_dma<1, dac_dma, dac_dma_ch, uint16_t>(sine, sizeof(sine) / sizeof(*sine));
    dac_dma::enable_interrupt<dac_dma_ch, true>();

    for (;;)
    {
        if (btn::read())
            led::toggle();
    }
}

