{-# LANGUAGE RecordWildCards #-}
module Main (main) where

-- based on STM32F411

data ClockTree = ClockTree
    { pllN      :: Double
    , pllM      :: Double
    , pllP      :: Double
    , pllPDIV   :: Double
    , pllQ      :: Double
    , pllR      :: Double
    , fSYS      :: Double
    , fPllP     :: Double
    , fPllQ     :: Double
    , fVCO      :: Double
    } deriving (Show)

main :: IO ()
main = do
    let xs = id
           . filter ((==170e6) . fPllQ)
           . filter ((==170e6) . fPllP)
           . filter ((==170e6) . fSYS)  -- exactly 64MHz
           $ genClocks 16e6 -- hsi clock
    mapM_ print xs

genClocks :: Double -> [ClockTree]
genClocks hsi =
    [ let fVCO = hsi * pllN / pllM
          fSYS = fVCO / pllR
          fPllQ = fVCO / pllQ
          fPllP | pllPDIV == 0 = fVCO / pllP
                | otherwise = fVCO / pllPDIV
       in ClockTree{..}
    | pllN <- [8..127]
    , pllM <- [1..16]
    , pllP <- [7, 17]
    , pllPDIV <- [0, 2..31]
    , pllQ <- [2, 4, 6, 8]
    , pllR <- [2, 4, 6, 8]
    , let pllIn = hsi / pllM
       in pllIn >= 2.66e6 && pllIn <= 8e6    -- required range!
    , let fVCO = hsi * pllN / pllM
       in fVCO >= 64e6 && fVCO <= 344e6    -- allowed range!
       && fVCO / pllR <= 170e6
       && fVCO / pllQ <= 170e6
    ]

