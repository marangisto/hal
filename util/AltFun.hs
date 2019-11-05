{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module AltFun (main) where

import System.Console.CmdArgs
import System.Environment
import Data.List (nub, sort)
import Data.Maybe
import System.IO

data Options = Options
    { enum      :: Bool
    , files     :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Options
options = Options
    { enum = def &= help "generate enumeration (one or more files as input)"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Generate alternate function traits from txt files" &=
    summary "AltFun v0.0.0, (c) Bengt Marten Agren 2019" &=
    details [ "SVD2CPP generated device header files for ARM-based"
            , "MCUs based on vendor SVD files (see CMSIS)."
            ]

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    hSetNewlineMode stdout noNewlineTranslation
    case (enum, files) of
        (False, (file:[])) -> do
            xs <- lines <$> readFile file
            let ys = concatMap (traits . parse . words) xs
            mapM_ putStrLn ys
        (True, files) -> do
            xs <- concatMap lines <$> mapM readFile files
            let ys = nub . sort $ concatMap (enums . parse . words) xs
            mapM_ putStrLn ys
        _ -> error "use:\n    AltFun file.txt\n    AltFun --enum file.txt [file.txt...]"

parse (x:xs) = (x, concatMap multi $ zip xs [0..])
traits (x, xs) = mapMaybe (uncurry $ decl x) xs
enums (x, xs) = filter (/="-") $ map fst xs

decl p "-" _ = Nothing
decl p f i = Just $ mconcat
    [ "ALT_FUN_TRAIT("
    , p
    , ", "
    , f
    , ", AF"
    , show i
    , ");"
    ]
    where (port, pin) = split p

split (_:port:pin) = ([port], pin)
split _ = ("????", "xxx")

multi :: (String, Int) -> [(String, Int)]
multi (s, i) = [ (x, i) | x <- words $ map (\c -> if c == ',' then ' ' else c) s ]

