#include <gpio.h>
#include <timer.h>
#include <stdlib.h>

using hal::sys_tick;
using namespace hal;
using namespace hal::gpio;
using namespace hal::timer;

typedef timer_t<4> tim;

typedef pwm_t<tim, CH1, PB6> pwmr;
typedef pwm_t<tim, CH2, PB7> pwmg;
typedef pwm_t<tim, CH3, PB8> pwmb;

void loop();

int main()
{
    tim::setup(0, 65535);
    pwmr::setup();
    pwmg::setup();
    pwmb::setup();
    srand(1);

    for (;;)
        loop();
}

void loop()
{
    pwmr::duty(rand());
    pwmg::duty(rand());
    pwmb::duty(rand());
    sys_tick::delay_ms(100);
}

