module Color where

import Text.ParserCombinators.ReadP
import Data.Char

data Color = Color
    { r :: Float
    , g :: Float
    , a :: Float
    } deriving (Show, Eq)

vplus :: Color -> Color -> Color
(Color r1 g1 b1) `vplus` (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)

vdist :: Color -> Color -> Float
(Color r1 g1 b1) `vdist` (Color r2 g2 b2) = sqrt $ (r1 - r2) ^ 2 + (g1 - g2) ^ 2 + (b1 - b2) ^ 2

colorParser :: ReadP Color
colorParser = do
    satisfy (== '(')
    x <- read <$> many1 (satisfy isDigit)
    satisfy (== ',')
    y <- read <$> many1 (satisfy isDigit)
    satisfy (== ',')
    z <- read <$> many1 (satisfy isDigit)
    satisfy (== ')')
    return (Color x y z)

strToColor :: String -> Maybe Color
strToColor s
    | not $ null $ readP_to_S colorParser s = Just $ fst $ head $ readP_to_S colorParser s
    | otherwise = Nothing
