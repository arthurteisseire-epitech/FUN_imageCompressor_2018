module Main where

import           Cluster
import           Data.List
import           Data.Maybe
import           Kmean
import           ParseArgs
import           Pixel
import           System.Environment
import           System.Random

main :: IO ()
main = getArgs >>= parseArgs >>= compute

compute :: (Int, Float, String) -> IO ()
compute (n, e, fileName) = do
    pixels <- getPixels fileName n
    centroids <- getRandomClusters pixels n
    let clusters = kmean centroids pixels e
    printClusters clusters

getRandomClusters :: [Pixel] -> Int -> IO [Cluster]
getRandomClusters pixels n = clusterFromCentroids <$> getRandomCentroids pixels n

getRandomCentroids :: [Pixel] -> Int -> IO [Pixel]
getRandomCentroids pixels n = do
    gen <- getStdGen
    let indexes = take n (nub (randomRs (0, length pixels - 1) gen :: [Int]))
    return $ getPixelsFromIndexes pixels indexes

getPixelsFromIndexes :: [Pixel] -> [Int] -> [Pixel]
getPixelsFromIndexes pixels = map (\x -> pixels !! x)

getPixels :: String -> Int -> IO [Pixel]
getPixels fileName n = catMaybes <$> fileToPixels fileName n

fileToPixels :: String -> Int -> IO [Maybe Pixel]
fileToPixels fileName n = do
    pixels <- textToPixels <$> readFile fileName
    if length pixels < n
        then exitWithHelp
        else return pixels
