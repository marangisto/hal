#include <stdlib.h>
#include <usart.h>
#include <redirect.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA5> ld4;

void loop();

template<> void handler<interrupt::USART2>()
{
    ld4::toggle();
    serial::isr();
}

int main()
{
    ld4::setup();
    serial::setup<230400>();
    hal::nvic<interrupt::USART2>::enable();
    interrupt::enable();

    stdio_t::bind_stdout<serial>();
    stdio_t::bind_stderr<serial>();

    for (;;)
        loop();
}

void loop()
{
    char c;

    if (serial::read(c))
        putchar(c);
/*
    static int i = 0;
    const float pi = 3.141592654;

    fprintf(stdout, "hello world! %d %f\n", i, pi * i);
    if (i % 10 == 0)
        fprintf(stderr, "error message %d\n", i);
    i++;
*/
}

