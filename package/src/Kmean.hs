module Kmean where

import Cluster
import Pixel
import Point
import Color

kmean :: [Cluster] -> [Pixel] -> Float -> [Cluster]
kmean clusters pixels e = clusters

--addPixelInClosestCluster :: [Cluster] -> Pixel -> [Cluster]
--addPixelInClosestCluster (x:xs) pixel = findClosestCluster xs (m pixel) x
