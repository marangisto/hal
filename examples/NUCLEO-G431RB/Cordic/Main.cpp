#include <stdlib.h>
#include <usart.h>
#include <redirect.h>
#include <cordic.h>
#include <cstring>
#include <math.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::cordic;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA5> ld4;

typedef hal::cordic::cordic_t cordic;

template<> void handler<interrupt::USART2>()
{
    ld4::toggle();
    serial::isr();
    cordic::setup<cordic::sine, 6>();
}

void loop();

int main()
{
    ld4::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    interrupt::enable();

    stdio_t::bind<serial>();

    const float pi = 3.141592654;
    const uint32_t buf_size = 500;
    float sample_freq = 500;
    float wave_freq = 2.51; // Hz
    float dt = 1. / sample_freq;
    uint32_t wrap = sample_freq / wave_freq;
    float w = wave_freq * 2 * pi;

    auto f = [w](float t) { return 4. * sin(w * t); };

    uint32_t j = 0;

    for (uint32_t i = 0; i < buf_size; ++i)
    {
        float t = dt * j;
        printf("%f\n", f(t));
        if (++j >= wrap)
            j = 0;
    }

    for (;;)
        loop();
}

void loop()
{
    char buf[256];
    auto f = [](int32_t x) { return cordic::compute(x); };

    if (fgets(buf, sizeof(buf), stdin))
    {
        buf[strcspn(buf, "\r\n")] = 0;
        float x = atof(buf);
        printf("f(%f) = %f\n", x, q31tof(f(ftoq31(x))));
    }
}

