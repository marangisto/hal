#include <stdlib.h>
#include <usart.h>
#include <timer.h>
#include <adc.h>
#include <dac.h>
#include <redirect.h>
#include <cstring>

using hal::sys_tick;
using hal::sys_clock;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::timer;
using namespace hal::adc;
using namespace hal::dac;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA10> probe;
typedef analog_t<PA0> ain;
typedef adc_t<1> adc;
typedef dac_t<1> dac;

typedef hal::timer::timer_t<6> tim6;

constexpr uint32_t sample_freq = 48000;

template<> void handler<interrupt::USART2>()
{
    serial::isr();
}

static volatile uint16_t aval = 0;

template<> void handler<interrupt::TIM6_DACUNDER>()
{
    tim6::clear_uif();
    probe::set();
    adc::start_conversion();
    while (!adc::end_of_conversion());
    probe::clear();
    aval = adc::read();
}

int main()
{
    probe::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    interrupt::enable();

    stdio_t::bind<serial>();
    printf("Welcome to the STM32G431!\n");

    tim6::setup(0, sys_clock::freq() / sample_freq - 1);
    tim6::master_mode<tim6::mm_update>();

    // enable for sampling frequency probe
    tim6::update_interrupt_enable();
    hal::nvic<interrupt::TIM6_DACUNDER>::enable();

    adc::setup();
    ain::setup();

    dac::setup();
    dac::enable<1>();

    for (;;)
    {
        dac::write<1>(aval);
    }
}

