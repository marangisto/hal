#include <gpio.h>
#include <timer.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::timer;

typedef output_t<PC13> led;
typedef output_t<PA11> pwm;

typedef timer_t<1> tim;
typedef timer_t<2> aux;

template<> void handler<interrupt::TIM2>()
{
    aux::clear_uif();
    led::toggle();
}

static void init()
{
    using namespace device;
    typedef tim1_t _;

    GPIOA.CRH &= ~(0xF << ((11-8) * 4));
    GPIOA.CRH |= (0xB << ((11-8) * 4));
    TIM1.CCMR2 = _::CCMR2_RESET_VALUE       // reset capture compare register
               | _::CCMR2_OC4M<0x6>         // channel 4 pwm mode 1
               | _::CCMR2_OC4PE             // channel 4 preload enable
               ;
    TIM1.CCER = _::CCER_RESET_VALUE         // reset capture compare register
              | _::CCER_CC4E                // channel 4 cc output enable
              ;
    TIM1.CCR4 = 0;                          // 0% duty cycle
    TIM1.BDTR |= _::BDTR_MOE;               // main output enable
}

void loop();

int main()
{
    led::setup();
    pwm::setup();

    tim::setup(0, 65535);
    init();

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

    TIM1.CCR4 = (duty += 32);
    sys_tick::delay_ms(1);
}

