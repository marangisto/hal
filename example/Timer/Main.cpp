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
    TIM6.SR &= ~BV(tim6_t::SR_UIF);
    led_a::toggle();
    led_b::write(!led_a::read());
}

void loop();

int main()
{
    led_a::setup();
    led_b::setup();

    RCC.APB1ENR |= BV(rcc_t::APB1ENR_TIM6EN);
    TIM6.CR1 = tim6_t::CR1_RESET_VALUE;
    TIM6.PSC = 100;
    TIM6.ARR = 65535;
//    tim6::TIM6.CR1 |= BV(tim6_t::CR1_ARPE);      // auto-reload
    TIM6.CR1 |= BV(tim6_t::CR1_CEN);       // enable counter
    TIM6.DIER |= BV(tim6_t::DIER_UIE);     // update interupt enable
    NVIC.ISER |= BV(17);                  // enable TIM6 interrupt

    cpsie();

    for (;;)
        loop();
}

void loop()
{
}

