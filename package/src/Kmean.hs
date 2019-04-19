module Kmean where

import Cluster
import Pixel
import Color

kmean :: [Cluster] -> [Pixel] -> Float -> [Cluster]
kmean clusters pixels e = clusters

--cpdist :: Cluster -> Pixel -> Float
--(Cluster r1 g1 b1) `vdist` (Color r2 g2 b2) = sqrt $ fromIntegral $ (r1 - r2) ^ 2 + (g1 - g2) ^ 2 + (b1 - b2) ^ 2
--
--addPixelInClosestCluster :: [Cluster] -> Pixel -> [Cluster]
--addPixelInClosestCluster (x:xs) pixel = addPixelInClosestCluster' xs pixel x

--findClosestCluster :: [Cluster] -> Pixel -> Cluster -> Cluster
--findClosestCluster clusters pixel closest
