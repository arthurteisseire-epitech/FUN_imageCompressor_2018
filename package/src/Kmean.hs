module Kmean where

import Cluster
import Pixel

kmean :: [Cluster] -> [Pixel] -> Float -> [Cluster]
kmean clusters pixels e = clusters
