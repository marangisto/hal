#include <timer.h>
#include <button.h>
#include <gpio.h>

using namespace hal::timer;
using namespace hal::gpio;

typedef input_t<PA15> enc_1;
typedef input_t<PB3> enc_2;
typedef button_t<PB13> btn;
typedef encoder_t<2, PA15, PB3> enc;
typedef timer_t<6> aux;

typedef output_t<PC8> led_a;
typedef output_t<PC9> led_b;

extern "C" void ISR_TIM6_DAC(void)
{
    aux::clear_uif();
    btn::update();
}

static void show(uint8_t x)
{
    led_a::write(x & 1);
    led_b::write(x & 2);
}

int main()
{
    enc::setup<pull_up>(255);
    btn::setup<pull_up>();
    aux::setup(100, 1000);
    aux::update_interrupt_enable();
    led_a::setup();
    led_b::setup();
    int i = 0;

    hal::nvic<isr::TIM6_DAC>::enable();
    hal::interrupt::enable();

    for (;;)
    {
        if (btn::read())
            ++i;
        show(enc::count());
    }
}

