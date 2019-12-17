#pragma once

#include <cstdint>

namespace fixed
{

template<typename>struct q_traits {};

template<>
struct q_traits<int16_t>
{
    typedef int16_t T;
    typedef int32_t T2;

    static const uint8_t Q = 15;
    static const T max_val = 0x7fff;
    static const T min_val = -0x8000;
};

template<>
struct q_traits<int32_t>
{
    typedef int32_t T;
    typedef int64_t T2;

    static const uint8_t Q = 31;
    static const T max_val = 0x7fffffff;
    static const T min_val = -0x80000000;
};

template<typename T>
struct q_t
{
    static constexpr float inv_max_val = 1. / static_cast<float>(q_traits<T>::max_val);
    static constexpr q_t<T> max_val = q_t(q_traits<T>::max_val);
    static constexpr q_t<T> min_val = q_t(q_traits<T>::min_val);

    explicit constexpr q_t(T x): q(x) {}

    constexpr q_t(float x)
    {
        if (x > 1.0)
            q = q_traits<T>::max_val;
        else if (x < 1.0)
            q = q_traits<T>::min_val;
        else
            q = static_cast<T>(x * q_traits<T>::max_val);
    }

    constexpr float to_float() const { return static_cast<float>(q) * inv_max_val; }

    static T sat(typename q_traits<T>::T2 x)
    {
        if (x > q_traits<T>::max_val)
            return q_traits<T>::max_val;
        else if (x < q_traits<T>::min_val)
            return q_traits<T>::min_val;
        else
            return static_cast<T>(x);
    }

    T q;
};

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

typedef q_t<int16_t> q15_t;
typedef q_t<int32_t> q31_t;

} // namespace fixed

