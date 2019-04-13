module Point where

import Text.ParserCombinators.ReadP
import Data.Char

data Point = Point
    { x :: Float
    , y :: Float
    } deriving (Show, Eq)

digit :: ReadP Char
digit = satisfy isDigit

pointParser :: ReadP Point
pointParser = do
    satisfy (== '(')
    x <- read <$> many1 digit
    satisfy (== ',')
    y <- read <$> many1 digit
    satisfy (== ')')
    return (Point x y)

pointParse :: String -> Point
pointParse s = fst $ head $ readP_to_S pointParser s
