#pragma once

#include <cstddef>

#if defined(STM32F0x1)
#include <device/stm32f0x1.h>
namespace device = stm32f0x1;
#endif

namespace system
{

class sys_tick
{
public:
    static void delay_ms(uint32_t ms);
    static inline uint32_t count() { return ms_counter; }

private:
    friend inline void sys_tick_init(uint32_t n);
    friend inline void sys_tick_update();

    static void init(uint32_t reload);
    static inline void update();

    static volatile uint32_t ms_counter;
};

struct nvic
{
    static inline void enable() { __asm volatile ("cpsie i"); }
    static inline void disable() { __asm volatile ("cpsid i"); }

    enum pos_t
      { WWDG = 0
      , PVD_VDDIO2 = 1
      , RTC = 2
      , FLASH = 3
      , RCC_CRS = 4
      , EXTI0_1 = 5
      , EXTI2_3 = 6
      , EXTI4_15 = 7
      , TSC = 8
      , DMA_CH1 = 9
      , DMA_CH2_3 = 10
      , DMA_CH4_5_6_7 = 11
      , ADC_COMP = 12
      , TIM1_BRK_UP_TRG_COM = 13
      , TIM1_CC = 14
      , TIM2 = 15
      , TIM3 = 16
      , TIM6_DAC = 17
      , TIM7 = 18
      , TIM14 = 19
      , TIM15 = 20
      , TIM16 = 21
      , TIM17 = 22
      , I2C1 = 23
      , I2C2 = 24
      , SPI1 = 25
      , SPI2 = 26
      , USART1 = 27
      , USART2 = 28
      , USART3_4_5_6_7_8 = 29
      , CEC_CAN = 30
      , USB = 31
      };

    template<pos_t POS>
    static void enable()
    {
        device::NVIC.ISER |= 1 << POS;
    }
};

}

