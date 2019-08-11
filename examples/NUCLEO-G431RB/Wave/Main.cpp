#include <timer.h>
#include <button.h>
#include <gpio.h>
#include <dac.h>

using namespace hal::timer;
using namespace hal::gpio;
using namespace hal::dac;

typedef timer_t<2> tim2;
typedef timer_t<6> tim6;
typedef timer_t<3> aux;

typedef dac_t<1> dac;

typedef button_t<PC13> btn;
typedef output_t<PA5> led;

template<> void handler<interrupt::TIM3>()
{
    aux::clear_uif();
    btn::update();
}

int main()
{
    led::setup();
    btn::setup<pull_down>();

    aux::setup(100, 1000);
    aux::update_interrupt_enable();
    hal::nvic<interrupt::TIM3>::enable();

    tim2::setup(149, 999);
    tim2::master_mode<tim2::mm_update>();

    tim6::setup(0, 2499);
    tim6::master_mode<tim6::mm_update>();

    interrupt::enable();

    dac::setup();
    dac::setup<1, 4, 7>();
    dac::enable_wave<1, 0, 0x444>();
    dac::enable<1>();

    for (;;)
    {
        if (btn::read())
            led::toggle();
    }
}

