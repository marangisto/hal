{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module AltFun (main) where

import System.Console.CmdArgs
import System.Environment
import Data.List (nub, sort)
import Data.Monoid
import Data.Maybe
import System.IO

data Options = Options
    { files     :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Options
options = Options
    { files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Generate alternate function traits from txt files" &=
    summary "AltFun v0.1.0, (c) Bengt Marten Agren 2019, 2020" &=
    details [ "SVD2CPP generated device header files for ARM-based"
            , "MCUs based on vendor SVD files (see CMSIS)."
            ]

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    hSetNewlineMode stdout noNewlineTranslation
    case files of
        (file:[]) -> do
            xs <- lines <$> readFile file
            mapM_ putStrLn $ concat
                [ funDecl 16    -- FIXME: determine arity from data!
                , [ "" ]
                , enumDecl . nub . sort $ concatMap (enums . parse . words) xs
                , [ "" ]
                , traitDecl
                , [ "" ]
                , concatMap (traits . parse . words) xs
                ]
        _ -> error "use:\n    AltFun file.txt"

parse (x:xs) = (x, concatMap multi $ zip xs [0..])
traits (x, xs) = mapMaybe (uncurry $ traitSpec x) xs
enums (x, xs) = filter (/="-") $ map fst xs

funDecl :: Int -> [String]
funDecl n = concat
    [ [ "enum alt_fun_t" ]
    , [ s <> "AF" <> show i | (s, i) <- zip ("    { " : repeat "    , ") [0..n-1] ]
    , [ "    };" ]
    ]

enumDecl :: [String] -> [String]
enumDecl xs = concat
    [ [ "enum alternate_function_t" ]
    , [ s <> x | (s, x) <- zip ("    { " : repeat "    , ") xs ]
    , [ "    };" ]
    ]

traitDecl :: [String]
traitDecl =
    [ "template<gpio_pin_t PIN, alternate_function_t ALT>"
    , "struct alt_fun_traits"
    , "{"
    , "    static_assert(always_false_i<PIN>::value, \"selected alternate function is not available on this pin!\");"
    , "};"
    ]

traitSpec p "-" _ = Nothing
traitSpec p f i = Just $ mconcat
    [ "template<> struct alt_fun_traits<"
    , p
    , ", "
    , f
    , "> { static const alt_fun_t AF = AF"
    , show i
    , "; };"
    ]
    where (port, pin) = split p

split (_:port:pin) = ([port], pin)
split _ = ("????", "xxx")

multi :: (String, Int) -> [(String, Int)]
multi (s, i) = [ (x, i) | x <- words $ map (\c -> if c == ',' then ' ' else c) s ]

