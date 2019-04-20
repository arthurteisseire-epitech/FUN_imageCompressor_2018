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
    printClusters $ kmean centroids pixels e

getRandomClusters :: [Pixel] -> Int -> IO [Cluster]
getRandomClusters pixels n = clusterFromCentroids <$> getRandomCentroids pixels n

getRandomCentroids :: [Pixel] -> Int -> IO [Pixel]
getRandomCentroids pixels n = do
    gen <- getStdGen
    let uniquePixels = nubPixelsColor pixels
    let infiniteIndexes = nub (randomRs (0, length uniquePixels - 1) gen :: [Int])
    let indexes = take (min n (length uniquePixels)) infiniteIndexes
    return $ getPixelsFromIndexes uniquePixels indexes

getPixels :: String -> Int -> IO [Pixel]
getPixels fileName n = catMaybes <$> fileToPixels fileName n

fileToPixels :: String -> Int -> IO [Maybe Pixel]
fileToPixels fileName n = do
    pixels <- textToPixels <$> readFile fileName
    if length pixels < n
        then exitWithHelp
        else return pixels
