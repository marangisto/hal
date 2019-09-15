#include <gpio.h>
#include <timer.h>

using hal::sys_tick;
using namespace hal;
using namespace hal::gpio;
using namespace hal::timer;

typedef timer_t<1> tim;
typedef timer_t<2> aux;

typedef output_t<PC13> led;
typedef pwm_t<tim, CH1, PA8> pwm1;
typedef pwm_t<tim, CH4, PA11> pwm4;

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
    pwm1::setup();
    pwm4::setup();

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

    pwm1::set_duty(duty += 32);
    pwm4::set_duty(65535 - duty);
    sys_tick::delay_ms(1);
}

