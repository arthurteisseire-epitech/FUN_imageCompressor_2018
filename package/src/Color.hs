module Color
    ( findClosestColor
    , vdist
    , vplus
    , colorToStr
    , colorParser
    , strToColor
    ) where

import           Data.Char
import           Text.ParserCombinators.ReadP

data Color = Color
    { r :: Int
    , g :: Int
    , b :: Int
    } deriving (Show, Eq)

vplus :: Color -> Color -> Color
(Color r1 g1 b1) `vplus` (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)

vdist :: Color -> Color -> Float
(Color r1 g1 b1) `vdist` (Color r2 g2 b2) = sqrt $ fromIntegral $ (r1 - r2) ^ 2 + (g1 - g2) ^ 2 + (b1 - b2) ^ 2

findClosestColor :: [Color] -> Color -> Color
findClosestColor [] _         = Color 0 0 0
findClosestColor colors color = findClosestColor' colors color (head colors)

findClosestColor' :: [Color] -> Color -> Color -> Color
findClosestColor' [] _ minColor = minColor
findClosestColor' (x:xs) color minColor
    | (minColor `vdist` color) < (x `vdist` color) = findClosestColor' xs color minColor
    | otherwise = findClosestColor' xs color x

isWord8 :: Int -> Bool
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
colorToStr color = "(" ++ show (r color) ++ "," ++ show (g color) ++ "," ++ show (b color) ++ ")"
