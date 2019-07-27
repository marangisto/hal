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
}

void loop();

template<uint32_t SAMPLE_FREQUENCY>
class generator_t
{
public:
    static constexpr float DT = 1. / SAMPLE_FREQUENCY;

    generator_t(float f): m_phase(.0)
    {
        set_freq(f);
    }

    float get_freq()
    {
        return m_freq;
    }

    void set_freq(float f)
    {
        m_freq = f;
        m_dp = DT * 2 * m_freq;
    }

    float next_sample()
    {
        auto y = 4. * q31tof(cordic::compute(ftoq31(1. - m_phase)));

        m_phase += m_dp;
        if (m_phase >= 2.)
            m_phase -= 2.;
        return y;
    }

private:
    float m_freq;
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
    cordic::setup<cordic::sine, 6>();

    const uint32_t buf_size = 500;
    const uint32_t sample_freq = 96000;
    generator_t<sample_freq> c4(440.), c5(880.);

    for (uint32_t i = 0; i < buf_size; ++i)
        printf("%f %f\n", c4.next_sample(), c5.next_sample());

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

