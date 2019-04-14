module Cluster where

import           Color
import           Pixel
import           Point

data Cluster = Cluster
    { red   :: Float
    , green :: Float
    , blue  :: Float
    , pixel :: [Pixel]
    } deriving (Show, Eq)

clusterFromCentroid :: Pixel -> Cluster
clusterFromCentroid (Pixel (Point _ _) (Color r g b)) = Cluster (fromIntegral r) (fromIntegral g) (fromIntegral b) []

clusterFromCentroids :: [Pixel] -> [Cluster]
clusterFromCentroids = map clusterFromCentroid

clusterToStr :: Cluster -> String
clusterToStr cluster =
    "--\n" ++
    "(" ++
    show (red cluster) ++
    "," ++
    show (green cluster) ++
    "," ++ show (blue cluster) ++ ")" ++
    "\n-\n" ++
    concatMap (\p -> pixelToStr p ++ "\n") (pixel cluster)

clustersToStr :: [Cluster] -> String
clustersToStr = concatMap clusterToStr

printCluster :: Cluster -> IO ()
printCluster = putStrLn . clusterToStr

printClusters :: [Cluster] -> IO ()
printClusters = putStrLn . clustersToStr
