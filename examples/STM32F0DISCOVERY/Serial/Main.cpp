#include <stdlib.h>
#include <cstring>
#include <usart.h>
#include <redirect.h>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;

typedef usart_t<1, PB6, PB7> serial;
//typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PC8> led_a;
typedef output_t<PC9> led_b;

void loop();

template<> void handler<interrupt::USART1>()
{
    led_a::toggle();
    led_b::toggle();
    serial::isr();
}

int main()
{
    led_a::setup();
    led_b::setup();
    serial::setup<115200>();
    hal::nvic<interrupt::USART1>::enable();
    stdio_t::bind<serial>();
    interrupt::enable();
    led_a::set();

    printf("Hello STM32F051!\n");

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

