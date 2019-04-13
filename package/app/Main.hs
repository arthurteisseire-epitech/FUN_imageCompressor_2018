module Main where

import           Data.Maybe
import           ParseArgs
import           Pixel
import           System.Environment
import           System.Random

main :: IO ()
main = getArgs >>= parseArgs >>= getPixels >>= compute

compute :: [Pixel] -> IO ()
compute []     = putStrLn "No valid pixels given"
compute pixels = printPixel $ head pixels

getPixels :: (Int, Float, String) -> IO [Pixel]
getPixels args = catMaybes <$> fileToPixels (third args)

fileToPixels :: String -> IO [Maybe Pixel]
fileToPixels fileName = textToPixels <$> readFile fileName
