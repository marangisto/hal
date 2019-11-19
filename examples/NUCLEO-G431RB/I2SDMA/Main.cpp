////
// 
//      I2S with DMA example
//
////

#include <i2s.h>
#include <gpio.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::i2s;

typedef output_t<PA5> ld4;
typedef i2s_t<2, PB13, PA11, PB12> i2s;
typedef hal::dma::dma_t<1> i2sdma;
static const uint8_t i2sdma_ch = 1;

static const uint16_t buf_size = 128;   // samples per channel
static uint16_t bufa[buf_size * 2];     // interleave both channels

void loop();

int main()
{
    ld4::setup();

    for (uint16_t i = 0; i < buf_size; ++i)
    {
        uint16_t j = i << 1;
        bufa[j] = i * (0xffff / buf_size);
        bufa[j + 1] = 0xffff - bufa[j];
    }

    i2sdma::setup();
    i2s::setup<philips_i2s, low_level, format_16_16, 27>();
    i2s::enable_dma<i2sdma, i2sdma_ch>(bufa, buf_size * 2);

    for (;;)
        loop();
}

void loop()
{
    ld4::toggle();
    sys_tick::delay_ms(100);
}

