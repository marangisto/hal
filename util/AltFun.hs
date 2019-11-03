module AltFun (main) where

import System.Environment
import Data.Maybe
import System.IO

main :: IO ()
main = do
    hSetNewlineMode stdout noNewlineTranslation
    [file] <- getArgs
    xs <- lines <$> readFile file
    let ys = concatMap (process . words) xs
    mapM_ putStrLn ys

process (x:xs) = mapMaybe (uncurry $ decl x) $ concatMap multi $ zip xs [0..]

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

