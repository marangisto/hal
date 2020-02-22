#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;

typedef output_t<PC8> led_a;
typedef output_t<PC9> led_b;
typedef input_t<PA0> btn;

template<> void handler<interrupt::EXTI0_1>()
{
    if (btn::interrupt_pending())
        btn::clear_interrupt();
    led_a::toggle();
}

int main()
{
    led_a::setup();
    led_b::setup();
    led_a::set();
    btn::setup<pull_down>();
    btn::enable_interrupt<rising_edge>();
    hal::nvic<interrupt::EXTI0_1>::enable();
    for (;;) 
        led_b::write(btn::read());
}

