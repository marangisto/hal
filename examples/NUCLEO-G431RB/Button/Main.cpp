#include <timer.h>
#include <button.h>
#include <gpio.h>

using namespace hal::timer;
using namespace hal::gpio;

typedef timer_t<3> aux;
typedef button_t<PC13> btn;
typedef output_t<PA5> led;

template<> void handler<interrupt::TIM3>()
{
    aux::clear_uif();
    btn::update();
}

int main()
{
    btn::setup<pull_down>();
    aux::setup(100, 1000);
    aux::update_interrupt_enable();
    led::setup();

    hal::nvic<interrupt::TIM3>::enable();
    interrupt::enable();

    for (;;)
    {
        if (btn::read())
            led::toggle();
    }
}

