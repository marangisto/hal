#include <stdlib.h>
#include <cstring>
#include <usart.h>
#include <redirect.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;

typedef usart_t<1, PA9, PA10> serial;
//typedef usart_t<2, PA2, PA3> serial;
//typedef usart_t<3, PB10, PB11> serial;
typedef output_t<PC13> led;

void loop();

template<> void handler<interrupt::USART1>()
{
    led::toggle();
    serial::isr();
}

int main()
{
    led::setup();
    serial::setup<115200>();
    hal::nvic<interrupt::USART1>::enable();
    stdio_t::bind<serial>();
    interrupt::enable();

    printf("Hello STM32F103!\n");

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

