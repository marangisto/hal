#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;

typedef output_t<PA5> ld4;
typedef input_t<PC13> btn;

template<> void handler<interrupt::EXTI15_10>()
{
    if (btn::interrupt_pending())
        btn::clear_interrupt();
    ld4::toggle();
}

int main()
{
    ld4::setup();
    btn::setup<pull_down>();
    btn::enable_interrupt<rising_edge>();
    hal::nvic<interrupt::EXTI15_10>::enable();
    for (;;) ;
}

