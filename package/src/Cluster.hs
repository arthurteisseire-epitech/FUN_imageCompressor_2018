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
    { mean  :: Mean
    , pixel :: [Pixel]
    } deriving (Show, Eq)

clusterFromCentroid :: Pixel -> Cluster
clusterFromCentroid (Pixel (Point _ _) (Color r g b)) = Cluster (Mean (fromIntegral r) (fromIntegral g) (fromIntegral b)) []

clusterFromCentroids :: [Pixel] -> [Cluster]
clusterFromCentroids = map clusterFromCentroid

clusterToStr :: Cluster -> String
clusterToStr cluster =
    "--\n" ++
    "(" ++
    show (red $ mean cluster) ++
    "," ++
    show (green $ mean cluster) ++
    "," ++ show (blue $ mean cluster) ++ ")" ++ "\n-\n" ++ concatMap (\p -> pixelToStr p ++ "\n") (pixel cluster)

clustersToStr :: [Cluster] -> String
clustersToStr = concatMap clusterToStr

printCluster :: Cluster -> IO ()
printCluster = putStrLn . clusterToStr

printClusters :: [Cluster] -> IO ()
printClusters = putStrLn . clustersToStr

vdistMean :: Color -> Mean -> Float
(Color r1 g1 b1) `vdistMean` (Mean r2 g2 b2) = sqrt $ (fromIntegral r1 - r2) ^ 2 + (fromIntegral g1 - g2) ^ 2 + (fromIntegral b1 - b2) ^ 2
