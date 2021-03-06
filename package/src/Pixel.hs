module Pixel where

import           Color
import           Data.Char
import           Data.List
import           Point
import           Text.ParserCombinators.ReadP

data Pixel = Pixel
    { point :: Point
    , color :: Color
    } deriving (Show, Eq)

pixelParser :: ReadP Pixel
pixelParser = do
    point <- pointParser
    satisfy (== ' ')
    Pixel point <$> colorParser

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

pixelColorEq :: Pixel -> Pixel -> Bool
pixelColorEq pixel1 pixel2 = color pixel1 == color pixel2

isUniqueColor :: [Pixel] -> Pixel -> Bool
isUniqueColor [] _ = True
isUniqueColor (x:xs) pixel
    | pixelColorEq x pixel = False
    | otherwise = isUniqueColor xs pixel

nubPixelsColor :: [Pixel] -> [Pixel]
nubPixelsColor = nubBy (\x y -> color x == color y)

getPixelsFromIndexes :: [Pixel] -> [Int] -> [Pixel]
getPixelsFromIndexes pixels = map (\x -> pixels !! x)
