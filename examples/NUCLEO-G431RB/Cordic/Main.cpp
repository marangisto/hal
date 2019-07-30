#include <stdlib.h>
#include <usart.h>
#include <redirect.h>
#include <cordic.h>
#include <cstring>
#include <math.h>
#include <utility>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;
using namespace hal::cordic;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA5> ld4;

typedef hal::cordic::cordic_t cordic;
typedef std::pair<float, float> freq_ampl;

template<> void handler<interrupt::USART2>()
{
    ld4::toggle();
    serial::isr();
}

void loop();

template<uint32_t SAMPLE_FREQUENCY>
class generator_t
{
public:
    static constexpr float DT = 1. / SAMPLE_FREQUENCY;

    generator_t(float f = 440): m_ampl(1.), m_phase(.0)
    {
        set_freq(f);
    }

    generator_t(const freq_ampl& fa): m_ampl(fa.second), m_phase(.0)
    {
        set_freq(fa.first);
    }

    float get_freq()
    {
        return m_freq;
    }

    void set_freq(float freq)
    {
        m_freq = freq;
        m_dp = DT * 2 * m_freq;
    }

    float get_ampl()
    {
        return m_ampl;
    }

    void set_ampl(float ampl)
    {
        m_ampl = ampl;
    }

    __attribute__((always_inline))
    float inline next_sample()
    {
        // FIXME: interleave cordic with phase update
        auto y = q31tof(cordic::compute(ftoq31(1. - m_phase)));

        m_phase += m_dp;
        if (m_phase >= 2.)
            m_phase -= 2.;
        return y * m_ampl;
    }

private:
    float m_freq;
    float m_ampl;
    float m_phase;
    float m_dp;
};

int main()
{
    ld4::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    interrupt::enable();
    stdio_t::bind<serial>();
    cordic::setup<cordic::sine, 4>();
    serial::write("startup complete!\n");

    const uint32_t buf_size = 1000;
    const uint32_t sample_freq = 96000;
    const float f = 440.;

    generator_t<sample_freq> w1[10], w2[10];

    for (uint16_t i = 0; i < sizeof(w1) / sizeof(*w1); ++i)
    {
        float k = i + 1;
        w1[i] = generator_t<sample_freq>(freq_ampl(f * k, 1. / k));
    }

    for (uint16_t i = 0; i < sizeof(w2) / sizeof(*w2); ++i)
    {
        float k = 2 * i + 1;
        w2[i] = generator_t<sample_freq>(freq_ampl(f * k, 1. / k));
    }

    static float b1[buf_size], b2[buf_size];

    uint32_t t0 = sys_tick::count();

    for (uint32_t i = 0; i < buf_size; ++i)
    {
        float s = 0;

        for (uint8_t j = 0; j < sizeof(w1) / sizeof(*w1); ++j)
            s += w1[j].next_sample();
        b1[i] = s;
    }

    uint32_t t = sys_tick::count() - t0;

    for (uint32_t i = 0; i < buf_size; ++i)
    {
        float s = 0;

        for (uint8_t j = 0; j < sizeof(w2) / sizeof(*w2); ++j)
            s += w2[j].next_sample();
        b2[i] = s;
    }

    for (uint32_t i = 0; i < buf_size; ++i)
        printf("%f %f\n", 4. * b1[i], 4. * b2[i]);

    printf("elapsed = %ld\n", t);

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

