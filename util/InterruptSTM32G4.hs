module Main where

import Data.Monoid

data Interrupt
    = WWDG
    | PVD_PVM
    | RTC_TAMP_CSS_LSE
    | RTC_WKUP
    | FLASH
    | RCC_
    | EXTI0
    | EXTI1
    | EXTI2
    | EXTI3
    | EXTI4
    | DMA_CH1
    | DMA_CH2
    | DMA_CH3
    | DMA_CH4
    | DMA_CH5
    | DMA_CH6
    | DMA_CH7
    | ADC1_2
    | USP_HP
    | USP_LP
    | FDCAN1_INTR1_IT
    | FDCAN1_INTR0_IT
    | EXTI9_5
    | TIM1_BRK_TIM15
    | TIM1_UP_TIM16
    | TIM1_TRG_COM_TIM17
    | TIM1_CC
    | TIM2
    | TIM3
    | TIM4
    | I2C1_EV
    | I2C1_ER
    | I2C2_EV
    | I2C2_ER
    | SPI1
    | SPI2
    | USART1
    | USART2
    | USART3
    | EXTI15_10
    | RTC_ALARM
    | USB_WAKE_UP
    | TIM8_BRK_TERR_IERR
    | TIM8_TRG_COM_DIR_IDX
    | TIM8_UP
    | TIM8_CC
    | ADC3
    | FMC
    | LPTIM1
    | TIM5
    | SPI3
    | UART4
    | UART5
    | TIM6_DAC1_3_UNDER
    | TIM7_DAC2_4_UNDER
    | DMA2_CH1
    | DMA2_CH2
    | DMA2_CH3
    | DMA2_CH4
    | DMA2_CH5
    | ADC4
    | ADC5
    | UCPD1
    | COMP1_2_3
    | COMP4_5_6
    | COMP7
    | HRTIM_MASTER_IRQN
    | HRTIM_TIMA_IRQN
    | HRTIM_TIMB_IRQN
    | HRTIM_TIMC_IRQN
    | HRTIM_TIMD_IRQN
    | HRTIM_TIME_IRQN
    | HRTIM_TIM_FLT_IRQN
    | HRTIM_TIMF_IRQN
    | CRS
    | SAI
    | TIM20_BRK_TERR_IERR
    | TIM20_UP
    | TIM20_TRG_COM_DIR_IDX
    | TIM20_CC
    | FPU
    | I2C4_EV
    | I2C4_ER
    | SPI4
    | AES
    | FDCAN2_INTR0
    | FDCAN2_INTR1
    | FDCAN3_INTR0
    | FDCAN3_INTR1
    | RNG
    | LPUART
    | I2C3_EV
    | I2C3_ER
    | DMAMUX_OVR
    | QUADSPI
    | DMA1_CH8
    | DMA2_CH6
    | DMA2_CH7
    | DMA2_CH8
    | CORDIC
    | FMAC
    deriving (Enum, Bounded, Show)
    
enumDecl :: [String]
enumDecl = "enum pos_t" : zipWith f [0..] [minBound..maxBound] ++ [ "    };", "" ]
    where f :: Int -> Interrupt -> String
          f k i = "    " <> s <> " " <> show i <> " = " <> show (fromEnum i)
              where s = if k == 0 then "{" else ","

weakDecl :: [String]
weakDecl = map f [minBound..maxBound]
    where f :: Interrupt -> String
          f i = "void ISR_" <> show i <> "(void) __attribute__ ((weak, alias(\"__nothing\")));"

vectorDecl :: [String]
vectorDecl = zipWith f [0..] [minBound..maxBound] ++ [ "    };", "" ]
    where f :: Int -> Interrupt -> String
          f k i = "        " <> s <> " ISR_" <> show i
              where s = if k == 0 then "{" else ","

main :: IO ()
main = do
    mapM_ putStrLn $ enumDecl
    mapM_ putStrLn $ weakDecl
    mapM_ putStrLn $ vectorDecl

