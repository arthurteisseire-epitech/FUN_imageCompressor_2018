module Cluster where

import           Color
import           Pixel
import           Point

data Mean = Mean
    { red   :: Float
    , green :: Float
    , blue  :: Float
    } deriving (Show, Eq)

data Cluster = Cluster
    { mean  :: Color
    , pixel :: [Pixel]
    } deriving (Show, Eq)

clusterFromCentroid :: Pixel -> Cluster
clusterFromCentroid (Pixel (Point _ _) (Color r g b)) = Cluster (Color r g b) []

clusterFromCentroids :: [Pixel] -> [Cluster]
clusterFromCentroids = map clusterFromCentroid

clusterToStr :: Cluster -> String
clusterToStr cluster =
    "--\n" ++
    "(" ++
    show (r $ mean cluster) ++
    "," ++
    show (g $ mean cluster) ++
    "," ++ show (b $ mean cluster) ++ ")" ++ "\n-\n" ++ concatMap (\p -> pixelToStr p ++ "\n") (pixel cluster)

clustersToStr :: [Cluster] -> String
clustersToStr = concatMap clusterToStr

printCluster :: Cluster -> IO ()
printCluster = putStrLn . clusterToStr

printClusters :: [Cluster] -> IO ()
printClusters = putStrLn . clustersToStr

vdistMean :: Mean -> Mean -> Float
(Mean r1 g1 b1) `vdistMean` (Mean r2 g2 b2) = sqrt $ (r1 - r2) ^ 2 + (g1 - g2) ^ 2 + (b1 - b2) ^ 2

getMean :: Pixel -> Mean
getMean pixel = Mean (r $ color pixel) (g $ color pixel) (b $ color pixel)
