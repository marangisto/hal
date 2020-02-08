#include <stdlib.h>
#include <usart.h>
#include <redirect.h>
#include <cstring>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;

typedef usart_t<3, PD8, PD9> serial;
typedef output_t<PB0> led;

void loop();

template<> void handler<interrupt::USART3>()
{
    led::toggle();
    serial::isr();
}

int main()
{
    led::setup();
    serial::setup<115200>();
    hal::nvic<interrupt::USART3>::enable();
    interrupt::enable();

    stdio_t::bind<serial>();
    printf("Welcome to the STM32F767!\n");

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

