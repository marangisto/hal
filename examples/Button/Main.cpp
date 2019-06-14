#include <timer.h>
#include <button.h>
#include <gpio.h>

using namespace system::timer;
using namespace system::gpio;

typedef button_t<PA0> btn;
typedef timer_t<6> aux;

typedef output_t<PC8> led_a;
typedef output_t<PC9> led_b;

extern "C" void ISR_TIM6_DAC(void)
{
    aux::clear_uif();
    btn::update();
}

int main()
{
    btn::setup<pull_down>();
    aux::setup(100, 1000);
    aux::update_interrupt_enable();
    led_a::setup();
    led_b::setup();
    int i = 0;

    system::cpsie();

    for (;;)
    {
        if (btn::read())
            ++i;

        led_a::write(i & 1);
        led_b::write(i & 2);
    }
}

