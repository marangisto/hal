#pragma once

#include <cstdio>

using hal::sys_tick;
using namespace hal::gpio;
using namespace hal::usart;

typedef usart_t<2, PA2, PA3> serial;
typedef output_t<PA5> ld4;

class stdio_t
{
public:
    template<typename T>
    static void bind_stdout() { m_1 = T::write; }

    template<typename T>
    static void bind_stderr() { m_2 = T::write; }

public:
    static uint32_t (*m_1)(const char *buf, uint32_t len);
    static uint32_t (*m_2)(const char *buf, uint32_t len);
};

uint32_t (*stdio_t::m_1)(const char *buf, uint32_t len) = 0;
uint32_t (*stdio_t::m_2)(const char *buf, uint32_t len) = 0;

extern "C" long _write_r(struct _reent *ptr, int fd, const void *buf, size_t cnt)
{
    const char *p = reinterpret_cast<const char*>(buf);

    switch (fd)
    {
        case 1: return stdio_t::m_1 ? stdio_t::m_1(p, cnt) : cnt;
        case 2: return stdio_t::m_2 ? stdio_t::m_2(p, cnt) : cnt;
        default: return 0;
    }
}

