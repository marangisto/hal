#include <stdlib.h>
#include <cstring>
#include <usart.h>
#include <redirect.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PD12> ld4;

void loop();

template<> void handler<interrupt::USART2>()
{
    ld4::toggle();
    serial::isr();
}

int main()
{
    ld4::setup();
    serial::setup<115200>();
    hal::nvic<interrupt::USART2>::enable();
    stdio_t::bind<serial>();
    interrupt::enable();

    printf("Hello STM32G431!\n");

    for (;;)
        loop();
}

void loop()
{
    char buf[256];

    printf("> ");
    fflush(stdout);
    if (fgets(buf, sizeof(buf), stdin))
    {
        buf[strcspn(buf, "\r\n")] = 0;
        printf("got = '%s'\n", buf);
    }
}

