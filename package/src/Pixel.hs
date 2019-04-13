module Pixel where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Point
import Color

data Pixel = Pixel
    { point :: Point
    , color :: Color
    } deriving (Show, Eq)

pixelParser :: ReadP Pixel
pixelParser = do
    point <- pointParser
    satisfy (== ' ')
    color <- colorParser
    return (Pixel point color)

strToPixel :: String -> Maybe Pixel
strToPixel s
    | not $ null $ readP_to_S pixelParser s = Just $ fst $ head $ readP_to_S pixelParser s
    | otherwise = Nothing

textToPixels :: String -> [Maybe Pixel]
textToPixels = map strToPixel . lines

pixelToStr :: Pixel -> String
pixelToStr pixel = pointToStr (point pixel) ++ " " ++ colorToStr (color pixel)

printPixel :: Pixel -> IO ()
printPixel = putStrLn . pixelToStr
