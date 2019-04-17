#pragma once

#include <stdint.h>
#include <cstddef>

template<int N> class reserved_t { private: uint32_t m_pad[N]; };

static inline uint32_t BV(uint8_t x) { return 1 << x; }

static inline void cpsid(void) { __asm volatile ("cpsid i"); }
static inline void cpsie(void) { __asm volatile ("cpsie i"); }

