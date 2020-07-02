{-# LANGUAGE RecordWildCards #-}
module Main (main) where

data ClockTree = ClockTree
    { pllN      :: Double
    , pllM      :: Double
    , pllP      :: Double
    , pllQ      :: Double
    , pllR      :: Double
    , fPllP     :: Double
    , fPllQ     :: Double
    , fPllR     :: Double
    , fVCO      :: Double
    , fVCOin    :: Double
    } deriving (Show)

main :: IO ()
main = do
    let xs = id
           . filter ((==48e6) . fPllQ)
           . filter ((==216e6) . fPllP)
           . filter ((==216e6) . fPllR)
           $ genClocks 16e6 -- hsi clock
    mapM_ print xs

genClocks :: Double -> [ClockTree]
genClocks hsi =
    [ let fVCO = hsi * pllN / pllM
          fPllQ = fVCO / pllQ
          fPllP = fVCO / pllP
          fPllR = fVCO / pllR
          fVCOin = hsi / pllM
       in ClockTree{..}
    | pllN <- [50..432]
    , pllM <- [2..63]
    , pllP <- [2, 4, 6, 8]
    --, pllPDIV <- [0, 2..31]
    , pllQ <- [2..15]
    , pllR <- [2..7]
    , let pllIn = hsi / pllM
       in pllIn >= 1e6 && pllIn <= 2e6    -- required range!
    , let fVCO = hsi * pllN / pllM
       in fVCO >= 100e6 && fVCO <= 432e6    -- allowed range!
       && fVCO / pllR <= 216e6
       && fVCO / pllQ <= 48e6
    ]

