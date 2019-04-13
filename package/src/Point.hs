module Point where

import Text.ParserCombinators.ReadP
import Data.Char

data Point = Point
    { x :: Float
    , y :: Float
    } deriving (Show, Eq)

pointParser :: ReadP Point
pointParser = do
    satisfy (== '(')
    x <- read <$> many1 (satisfy isDigit)
    satisfy (== ',')
    y <- read <$> many1 (satisfy isDigit)
    satisfy (== ')')
    return (Point x y)

strToPoint :: String -> Maybe Point
strToPoint s
    | not $ null $ readP_to_S pointParser s = Just $ fst $ head $ readP_to_S pointParser s
    | otherwise = Nothing
