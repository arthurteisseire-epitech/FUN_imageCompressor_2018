module Main where

import           Cluster
import           Data.Maybe
import           ParseArgs
import           Pixel
import           System.Environment
import           System.Random

main :: IO ()
main = getArgs >>= parseArgs >>= compute

compute :: (Int, Float, String) -> IO ()
compute (n, e, fileName) = do
    pixels <- getPixels fileName
    cluster <- getRandomCluster pixels n
    printCluster cluster

getRandomCluster :: [Pixel] -> Int -> IO Cluster
getRandomCluster pixels n = clusterFromCentroid <$> getRandomPixel pixels

getRandomPixel :: [Pixel] -> IO Pixel
getRandomPixel pixels = do
    gen <- getStdGen
    return $ pixels !! fst (randomR (0, length pixels - 1) gen :: (Int, StdGen))

getPixels :: String -> IO [Pixel]
getPixels fileName = catMaybes <$> fileToPixels fileName

fileToPixels :: String -> IO [Maybe Pixel]
fileToPixels fileName = do
    pixels <- textToPixels <$> readFile fileName
    if null pixels
        then exitWithHelp
        else return pixels
