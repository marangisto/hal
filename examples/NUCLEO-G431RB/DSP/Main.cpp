#include <timer.h>
#include <button.h>
#include <gpio.h>
#include <dac.h>
#include <dma.h>

using namespace hal::timer;
using namespace hal::gpio;
using namespace hal::dac;
using namespace hal::dma;
using hal::sys_clock;

typedef timer_t<6> tim6;
typedef timer_t<3> aux;

typedef dac_t<1> dac;
typedef dma_t<1> dac_dma;

constexpr uint8_t dac_dma_ch = 1;

typedef button_t<PC13> btn;
typedef output_t<PA5> led;
typedef output_t<PA10> probe;

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

static uint16_t sine[60] =
    { 0x07ff,0x08cb,0x0994,0x0a5a,0x0b18,0x0bce,0x0c79,0x0d18,0x0da8,0x0e29,0x0e98,0x0ef4
    , 0x0f3e,0x0f72,0x0f92,0x0f9d,0x0f92,0x0f72,0x0f3e,0x0ef4,0x0e98,0x0e29,0x0da8,0x0d18
    , 0x0c79,0x0bce,0x0b18,0x0a5a,0x0994,0x08cb,0x07ff,0x0733,0x066a,0x05a4,0x04e6,0x0430
    , 0x0385,0x02e6,0x0256,0x01d5,0x0166,0x010a,0x00c0,0x008c,0x006c,0x0061,0x006c,0x008c
    , 0x00c0,0x010a,0x0166,0x01d5,0x0256,0x02e6,0x0385,0x0430,0x04e6,0x05a4,0x066a,0x0733
    };

int main()
{
    const uint32_t sample_freq = 96000;
    btn::setup<pull_down>();
    probe::setup();
    led::setup();

    aux::setup(100, 1000);
    aux::update_interrupt_enable();
    hal::nvic<interrupt::TIM3>::enable();

    tim6::setup(0, sys_clock::freq() / sample_freq - 1);
    tim6::master_mode<tim6::mm_update>();

    // enable for sampling frequency probe
    tim6::update_interrupt_enable();
    hal::nvic<interrupt::TIM6_DACUNDER>::enable();

    interrupt::enable();

    dac_dma::setup();

    dac::setup();
    dac::enable_trigger<1, 7>();    // FIXME: use constant for TIM6_TRGO
    dac::enable_dma<1, dac_dma, dac_dma_ch, uint16_t>(sine, sizeof(sine) / sizeof(*sine));

    for (;;)
    {
        if (btn::read())
            led::toggle();
    }
}

