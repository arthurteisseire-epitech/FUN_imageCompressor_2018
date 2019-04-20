module Kmean where

import Cluster
import Pixel
import Point
import Color

kmean :: [Cluster] -> [Pixel] -> Float -> [Cluster]
kmean clusters pixels e = addPixelInClosestCluster clusters (head pixels)

addPixelInClosestCluster :: [Cluster] -> Pixel -> [Cluster]
addPixelInClosestCluster [cluster] pixel = [Cluster (mean cluster) (pixel : pixels cluster)]
addPixelInClosestCluster (x:xs) pixel
    | mean x `vdist` color pixel > mean (head xs) `vdist` color pixel = x : addPixelInClosestCluster xs pixel
    | otherwise = head xs : addPixelInClosestCluster (x : tail xs) pixel
