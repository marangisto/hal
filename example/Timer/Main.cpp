#include <timer.h>
#include <gpio.h>

using namespace stm32f0;
//using namespace timer;
using namespace gpio;
using namespace device; // FIXME: remove

typedef output_t<C, 8> led_a;
typedef output_t<C, 9> led_b;

extern "C" void ISR_TIM6_DAC(void)
{
    tim6::TIM6.SR &= ~BV(tim6::SR::UIF);
    led_a::toggle();
    led_b::write(!led_a::read());
}

void loop();

int main()
{
    led_a::setup();
    led_b::setup();

    rcc::RCC.APB1ENR |= BV(rcc::APB1ENR::TIM6EN);
    tim6::TIM6.CR1 = tim6::CR1::RESET_VALUE;
    tim6::TIM6.PSC = 100;
    tim6::TIM6.ARR = 65535;
//    tim6::TIM6.CR1 |= BV(tim6::CR1::ARPE);      // auto-reload
    tim6::TIM6.CR1 |= BV(tim6::CR1::CEN);       // enable counter
    tim6::TIM6.DIER |= BV(tim6::DIER::UIE);     // update interupt enable
    nvic::NVIC.ISER |= BV(17);                  // enable TIM6 interrupt

    cpsie();

    for (;;)
        loop();
}

void loop()
{
}

