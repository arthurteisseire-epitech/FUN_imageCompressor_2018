module Kmean where

import           Cluster
import           Color
import           Pixel
import           Point

kmean :: [Cluster] -> [Pixel] -> Float -> [Cluster]
kmean clusters pixels e = map calcClusterMean (addPixelsInClusters clusters pixels)

addPixelsInClusters :: [Cluster] -> [Pixel] -> [Cluster]
addPixelsInClusters = foldl addPixelInClosestCluster

addPixelInClosestCluster :: [Cluster] -> Pixel -> [Cluster]
addPixelInClosestCluster [cluster] pixel = [Cluster (mean cluster) (pixel : pixels cluster)]
addPixelInClosestCluster (x:xs) pixel
    | mean x `vdist` color pixel > mean (head xs) `vdist` color pixel = x : addPixelInClosestCluster xs pixel
    | otherwise = head xs : addPixelInClosestCluster (x : tail xs) pixel

calcClusterMean :: Cluster -> Cluster
calcClusterMean cluster = Cluster (calcMean (pixels cluster)) (pixels cluster)

calcMean :: [Pixel] -> Color
calcMean [pixel] = color pixel
calcMean (x:xs) = (color x `vplus` calcMean xs) `vdiv` length (x:xs)
