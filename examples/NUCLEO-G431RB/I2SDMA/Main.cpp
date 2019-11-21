////
// 
//      I2S with DMA example
//
////

#include <i2s.h>
#include <gpio.h>
#include <fixed.h>
#include <math.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::i2s;
using namespace fixed;

static const float pi = 3.14159265358979323846;
typedef output_t<PA5> ld4;
//typedef i2s_t<2, PB13, PA11, PB12> i2s;
typedef i2s_t<3, PC10, PC12, PA15> i2s;
typedef hal::dma::dma_t<1> i2sdma;
static const uint8_t i2sdma_ch = 1;

static const uint16_t buf_size = 128;   // samples per channel
static int32_t bufa[buf_size * 2];      // interleave both channels

static inline uint32_t swap(uint32_t x) { return x << 16 | x >> 16; }

void loop();

int main()
{
    ld4::setup();

    for (uint16_t i = 0; i < buf_size; ++i)
    {
        uint16_t j = i << 1;
        float x = -1. + 2 * static_cast<float>(i) / static_cast<float>(buf_size);
        float y = sin(x*pi);

        bufa[j] = swap(ftoq31(y));
        bufa[j + 1] = swap(ftoq31(-y * 0.25));
    }

    i2sdma::setup();
    i2s::setup<philips_i2s, low_level, format_32_32, 27>();
    i2s::enable_dma<i2sdma, i2sdma_ch>(reinterpret_cast<uint16_t*>(bufa), buf_size * 4);

    for (;;)
        loop();
}

void loop()
{
    ld4::toggle();
    sys_tick::delay_ms(100);
}

