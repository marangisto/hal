{-# LANGUAGE RecordWildCards #-}
module Main (main) where

-- based on STM32F411

data ClockTree = ClockTree
    { pllN  :: Double
    , pllM  :: Double
    , pllP  :: Double
    , pllR  :: Double
    , fSYS  :: Double
    , fPllP  :: Double
    , fVCO  :: Double
    } deriving (Show)

main :: IO ()
main = do
    let xs = id
           -- . filter ((>=64e6) . fPllP)
           . filter ((==64e6) . fPllP)
           . filter ((==64e6) . fSYS)  -- exactly 64MHz
           $ genClocks 16e6 -- hsi clock
    mapM_ print xs

genClocks :: Double -> [ClockTree]
genClocks hsi =
    [ let fVCO = hsi * pllN / pllM
          fSYS = fVCO / pllR
          fPllP = fVCO / pllP
       in ClockTree{..}
    | pllN <- [8..86]
    , pllM <- [1..8]
    , pllP <- [2..32]
    , pllR <- [2..8]
    , let pllIn = hsi / pllM
       in pllIn >= 4e6 && pllIn <= 16e6    -- required range!
    , let fVCO = hsi * pllN / pllM
       in fVCO >= 64e6 && fVCO <= 344e6    -- allowed range!
    ]

