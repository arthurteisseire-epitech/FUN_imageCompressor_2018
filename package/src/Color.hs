module Color where

import           Data.Char
import           Text.ParserCombinators.ReadP

data Color = Color
    { r :: Float
    , g :: Float
    , b :: Float
    } deriving (Show, Eq)

vplus :: Color -> Color -> Color
(Color r1 g1 b1) `vplus` (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)

vdist :: Color -> Color -> Float
(Color r1 g1 b1) `vdist` (Color r2 g2 b2) = sqrt $ (r1 - r2) ^ 2 + (g1 - g2) ^ 2 + (b1 - b2) ^ 2

vdiv :: Color -> Int -> Color
(Color r g b) `vdiv` n = Color (r / fromIntegral n) (g / fromIntegral n) (b / fromIntegral n)

findClosestColor :: [Color] -> Color -> Color
findClosestColor colors color = findColor colors color (head colors) (<)

findColor :: [Color] -> Color -> Color -> (Float -> Float -> Bool) -> Color
findColor [] _ minColor _ = minColor
findColor (x:xs) color minColor cmp
    | (minColor `vdist` color) `cmp` (x `vdist` color) = findColor xs color minColor cmp
    | otherwise = findColor xs color x cmp

isWord8 :: Float -> Bool
isWord8 n = n >= 0 && n < 256

colorParser :: ReadP Color
colorParser = do
    satisfy (== '(')
    x <- read <$> many1 (satisfy isDigit)
    satisfy (== ',')
    y <- read <$> many1 (satisfy isDigit)
    satisfy (== ',')
    z <- read <$> many1 (satisfy isDigit)
    satisfy (== ')')
    if isWord8 x && isWord8 y && isWord8 z
        then return (Color x y z)
        else pfail

strToColor :: String -> Maybe Color
strToColor s
    | not $ null $ readP_to_S colorParser s = Just $ fst $ head $ readP_to_S colorParser s
    | otherwise = Nothing

colorToStr :: Color -> String
colorToStr color = "(" ++ show (round (r color)) ++ "," ++ show (round (g color)) ++ "," ++ show (round (b color)) ++ ")"
