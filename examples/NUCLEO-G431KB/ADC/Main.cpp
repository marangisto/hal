#include <stdlib.h>
#include <cstring>
#include <usart.h>
#include <redirect.h>
#include <adc.h>
#include <dma.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::adc;
using namespace hal::dma;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PB8> led;
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

int main()
{
    led::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    interrupt::enable();

    stdio_t::bind<serial>();
    printf("Welcome to the STM32G431!\n");

    ain1::setup();
    ain2::setup();
    ain3::setup();

    dma::setup();

    adc::setup();
    adc::sequence<1, 2, 15>();
    adc::dma<dma, 1, uint16_t>(buf, buf_size);
    adc::enable();

    for (;;)
    {
        adc::start_conversion();
        sys_tick::delay_ms(5);
        uint16_t x = buf[0];
        uint16_t y = buf[1];
        uint16_t z = buf[2];

        printf("%d %d %d\n", x, y, z);
        sys_tick::delay_ms(50);
    }
}
