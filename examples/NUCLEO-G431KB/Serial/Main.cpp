#include <stdlib.h>
#include <usart.h>
#include <redirect.h>
#include <cstring>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PB8> led;

void loop();

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

    for (;;)
        loop();
}

void loop()
{
    char buf[256];

    printf("> \n");
    if (fgets(buf, sizeof(buf), stdin))
    {
        buf[strcspn(buf, "\r\n")] = 0;
        printf("got = '%s'\n", buf);
    }
}

