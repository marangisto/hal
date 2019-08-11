#include <timer.h>
#include <button.h>
#include <gpio.h>
#include <dac.h>
#include <dma.h>

using namespace hal::timer;
using namespace hal::gpio;
using namespace hal::dac;
using namespace hal::dma;

typedef timer_t<6> tim6;
typedef timer_t<3> aux;

typedef dac_t<1> dac;
typedef dma_t<1> dma;

typedef button_t<PC13> btn;
typedef output_t<PA5> led;

template<> void handler<interrupt::TIM3>()
{
    aux::clear_uif();
    btn::update();
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
    led::setup();
    btn::setup<pull_down>();

    aux::setup(100, 1000);
    aux::update_interrupt_enable();
    hal::nvic<interrupt::TIM3>::enable();

    tim6::setup(0, 2499);
    tim6::master_mode<tim6::mm_update>();

    interrupt::enable();

    dma::setup();
    dac::setup();
    dac::enable_trigger<1, 7>();    // FIXME: use constant for TIM6_TRGO
    dac::enable_dma<1, dma, 1, uint16_t>(sine, sizeof(sine) / sizeof(*sine));

    for (;;)
    {
        if (btn::read())
            led::toggle();
    }
}

