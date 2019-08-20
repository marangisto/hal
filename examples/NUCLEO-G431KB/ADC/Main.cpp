#include <stdlib.h>
#include <cstring>
#include <usart.h>
#include <redirect.h>
#include <timer.h>
#include <adc.h>
#include <dma.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::timer;
using namespace hal::adc;
using namespace hal::dma;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PB8> led;
typedef output_t<PB4> probe;
typedef hal::timer::timer_t<3> tim;
typedef hal::adc::adc_t<1> adc;
typedef hal::dma::dma_t<1> dma;
typedef analog_t<PA0> ain1;
typedef analog_t<PA1> ain2;
typedef analog_t<PB0> ain3;

static const uint8_t buf_size = 3;
static uint16_t buf[buf_size] = { 0, 300, 600 };

template<> void handler<interrupt::USART2>()
{
    led::toggle();
    serial::isr();
}

template<> void handler<interrupt::TIM3>()
{
    tim::clear_uif();
    led::toggle();
    probe::toggle();
}

int main()
{
    led::setup();
    probe::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    hal::nvic<interrupt::TIM3>::enable();
    interrupt::enable();

    stdio_t::bind<serial>();
    printf("Welcome to the STM32G431!\n");

    ain1::setup();
    ain2::setup();
    ain3::setup();

    tim::setup(800, 10000); // approx 20Hz
    tim::master_mode<tim::mm_update>();
    tim::update_interrupt_enable();
    dma::setup();

    adc::setup();
    adc::sequence<1, 2, 15>();
    adc::dma<dma, 1, uint16_t>(buf, buf_size);
    adc::trigger<0x4>();
    adc::enable();

    adc::start_conversion();

    for (;;)
    {
        sys_tick::delay_ms(5);
        uint16_t x = buf[0];
        uint16_t y = buf[1];
        uint16_t z = buf[2];

        printf("%d %d %d\n", x, y, z);
        sys_tick::delay_ms(50);
    }
}

