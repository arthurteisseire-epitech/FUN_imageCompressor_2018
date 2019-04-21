module Kmean
    ( kmean
    ) where

import           Cluster
import           Color
import           Pixel
import           Point

kmean :: [Cluster] -> [Pixel] -> Float -> [Cluster]
kmean clusters p e
    | isEnd res p e = calcClustersMean res
    | otherwise = kmean (clearClusters $ calcClustersMean res) p e
  where
    res = addPixelsInClusters clusters p

isEnd :: [Cluster] -> [Pixel] -> Float -> Bool
isEnd [] _ _ = True
isEnd (x:xs) p e
    | mean x `vdist` mean (calcClusterMean x) <= e = isEnd xs p e
    | otherwise = False

calcClustersMean :: [Cluster] -> [Cluster]
calcClustersMean = map calcClusterMean

calcClusterMean :: Cluster -> Cluster
calcClusterMean cluster = Cluster (calcMean (pixels cluster)) (pixels cluster)

calcMean :: [Pixel] -> Color
calcMean pixels = foldr (vplus . color) (Color 0 0 0) pixels `vdiv` length pixels

clearClusters :: [Cluster] -> [Cluster]
clearClusters = map (\x -> Cluster (mean x) [])

addPixelsInClusters :: [Cluster] -> [Pixel] -> [Cluster]
addPixelsInClusters = foldl addPixelInClosestCluster

addPixelInClosestCluster :: [Cluster] -> Pixel -> [Cluster]
addPixelInClosestCluster [cluster] pixel = [Cluster (mean cluster) (pixel : pixels cluster)]
addPixelInClosestCluster (x:xs) pixel
    | mean x `vdist` color pixel > mean (head xs) `vdist` color pixel = x : addPixelInClosestCluster xs pixel
    | otherwise = head xs : addPixelInClosestCluster (x : tail xs) pixel
