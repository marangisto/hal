#include <gpio.h>
#include <timer.h>

using hal::sys_tick;
using namespace hal;
using namespace hal::gpio;
using namespace hal::timer;

typedef timer_t<4> tim;
typedef timer_t<1> aux;

typedef output_t<PC13> led;
typedef pwm_t<tim, CH3, PB8> pwma;
typedef pwm_t<tim, CH4, PB9> pwmb;

template<> void handler<interrupt::TIM1_UP>()
{
    aux::clear_uif();
    led::toggle();
}

void loop();

int main()
{
    led::setup();

    tim::setup(0, 65535);
    pwma::setup();
    pwmb::setup();

    aux::setup(100, 65535);
    aux::update_interrupt_enable();
    hal::nvic<interrupt::TIM1_UP>::enable();
    interrupt::enable();

    for (;;)
        loop();
}

void loop()
{
    static uint16_t duty = 0;

    pwma::set_duty(duty += 32);
    pwmb::set_duty(65535 - duty);
    sys_tick::delay_ms(1);
}

