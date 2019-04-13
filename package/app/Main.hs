module Main where

import           Data.Maybe
import           ParseArgs
import           Pixel
import           System.Environment
import           System.Random

main :: IO ()
main = getArgs >>= parseArgs >>= getPixels >>= compute

compute :: [Pixel] -> IO ()
compute []     = putStrLn "No valid pixel given"
compute pixels = getRandomPixel pixels >>= printPixel

getRandomPixel :: [Pixel] -> IO Pixel
getRandomPixel pixels = do
    gen <- getStdGen
    return $ pixels !! fst (randomR (0, length pixels - 1) gen :: (Int, StdGen))

getPixels :: (Int, Float, String) -> IO [Pixel]
getPixels args = catMaybes <$> fileToPixels (third args)

fileToPixels :: String -> IO [Maybe Pixel]
fileToPixels fileName = textToPixels <$> readFile fileName
