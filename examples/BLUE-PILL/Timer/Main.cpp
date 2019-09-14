#include <timer.h>
#include <gpio.h>

using namespace hal::timer;
using namespace hal::gpio;

typedef timer_t<3> tim;

typedef output_t<PC13> led;

template<> void handler<interrupt::TIM3>()
{
    tim::clear_uif();
    led::toggle();
}

int main()
{
    tim::setup(100, 65535);
    tim::update_interrupt_enable();

    led::setup();

    hal::nvic<interrupt::TIM3>::enable();
    interrupt::enable();

    for (;;);
}

