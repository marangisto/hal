#pragma once

namespace fixed
{
__attribute__((always_inline))
static inline float q31tof(int32_t x)
{
    constexpr float k = 1. / static_cast<float>(0x7FFFFFFF);
    return x * k;
}

__attribute__((always_inline))
static inline int32_t ftoq31(float x)
{
    constexpr float k = static_cast<float>(0x7FFFFFFF);
    return x * k;
}

} // namespace fixed

