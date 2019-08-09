#include <timer.h>
#include <gpio.h>

using namespace hal::timer;
using namespace hal::gpio;

typedef timer_t<3> tim;

typedef output_t<PA5> ld4;

template<> void handler<interrupt::TIM3>()
{
    tim::clear_uif();
    ld4::toggle();
}

int main()
{
    tim::setup(120, 65535);
    tim::update_interrupt_enable();

    ld4::setup();

    hal::nvic<interrupt::TIM3>::enable();
    interrupt::enable();

    for (;;);
}

