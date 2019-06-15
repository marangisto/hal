{-# LANGUAGE RecordWildCards #-}
module Main (main) where

-- based on STM32F411

data ClockTree = ClockTree
    { pllN  :: Double
    , pllM  :: Double
    , pllP  :: Double
    , pllQ  :: Double
    , fSYS  :: Double
    , fUSB  :: Double
    } deriving (Show)

main :: IO ()
main = do
    let xs = id
           . filter ((==8) . pllM)      -- force fVCO = 2MHz
           . filter ((>=40e6) . fUSB)
           . filter ((<=48e6) . fUSB)
           . filter ((>=99e6) . fSYS)
           . filter ((==100e6) . fSYS)  -- exactly 100MHz
           . filter ((<=101e6) . fSYS)
           $ genClocks 16e6 -- hsi clock
    mapM_ print xs

genClocks :: Double -> [ClockTree]
genClocks hsi =
    [ let fSYS = (hsi * pllN / pllM) / pllP
          fUSB = (hsi * pllN / pllM) / pllQ
       in ClockTree{..}
    | pllN <- [50..432]
    , pllM <- [2..63]
    , pllP <- [2, 4, 6, 8]
    , pllQ <- [2..15]
    , let pllIn = hsi / pllM
       in pllIn >= 1e6 && pllIn <= 2e6      -- recommended range!
    , let fVCO = hsi * pllN / pllM
       in fVCO >= 100e6 && fVCO <= 432e6    -- allowed range!
    ]

