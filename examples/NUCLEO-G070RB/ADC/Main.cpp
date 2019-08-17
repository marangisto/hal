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
typedef output_t<PA5> ld4;
typedef hal::dma::dma_t<1> dma;
typedef hal::adc::adc_t<1> adc;
typedef analog_t<PA0> ain0;
typedef analog_t<PA1> ain1;
typedef output_t<PA10> probe;

static const uint8_t adc_dma_ch = 1;
static const uint8_t adc_buf_size = 2;
static uint16_t adc_buf[adc_buf_size];

void loop();

template<> void handler<interrupt::USART2>()
{
    ld4::toggle();
    serial::isr();
}

int main()
{
    ain0::setup();
    ain1::setup();
    probe::setup();
    ld4::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    stdio_t::bind<serial>();
    interrupt::enable();

    printf("Hello STM32G070!\n");

    dma::setup();
    adc::setup();
    adc::sequence<0, 1>();
    adc::enable_dma<dma, adc_dma_ch, uint16_t>(adc_buf, adc_buf_size);

    for (;;)
        loop();
}

void loop()
{
    {
        probe::set();
        probe::clear();
        adc::start_conversion();
        while (!adc::conversion_complete());

        printf("%-64.*s  ", adc_buf[0] >> 6, "############################################################");
        printf("%-64.*s\n", adc_buf[1] >> 6, "############################################################");
        sys_tick::delay_ms(20);
    }
}

