module Kmean where

import           Cluster
import           Color
import           Pixel
import           Point

kmean :: [Cluster] -> [Pixel] -> Float -> [Cluster]
kmean clusters [] _ = clusters
kmean clusters (x:xs) e = kmean (addPixelInClosestCluster clusters x) xs e

addPixelInClosestCluster :: [Cluster] -> Pixel -> [Cluster]
addPixelInClosestCluster [cluster] pixel = [Cluster (mean cluster) (pixel : pixels cluster)]
addPixelInClosestCluster (x:xs) pixel
    | mean x `vdist` color pixel > mean (head xs) `vdist` color pixel = x : addPixelInClosestCluster xs pixel
    | otherwise = head xs : addPixelInClosestCluster (x : tail xs) pixel
