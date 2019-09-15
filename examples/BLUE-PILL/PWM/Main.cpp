#include <gpio.h>
#include <timer.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::timer;

typedef timer_t<1> tim;
typedef timer_t<2> aux;

typedef output_t<PC13> led;
typedef pwm_t<tim, 4, PA11> pwm;

template<> void handler<interrupt::TIM2>()
{
    aux::clear_uif();
    led::toggle();
}

void loop();

int main()
{
    led::setup();

    tim::setup(0, 65535);
    pwm::setup();

    aux::setup(100, 65535);
    aux::update_interrupt_enable();
    hal::nvic<interrupt::TIM2>::enable();
    interrupt::enable();

    for (;;)
        loop();
}

void loop()
{
    static uint16_t duty = 0;

    pwm::set_duty(duty += 32);
    sys_tick::delay_ms(1);
}

