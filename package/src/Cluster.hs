module Cluster where

import           Color
import           Numeric
import           Pixel
import           Point

data Cluster = Cluster
    { mean   :: Color
    , pixels :: [Pixel]
    } deriving (Show, Eq)

clusterFromCentroid :: Pixel -> Cluster
clusterFromCentroid (Pixel (Point _ _) (Color r g b)) = Cluster (Color r g b) []

clusterFromCentroids :: [Pixel] -> [Cluster]
clusterFromCentroids = map clusterFromCentroid

clusterToStr :: Cluster -> String
clusterToStr cluster =
    "--\n" ++
    "(" ++
    formatFloatN (r $ mean cluster) 2 ++
    "," ++
    formatFloatN (g $ mean cluster) 2 ++
    "," ++ formatFloatN (b $ mean cluster) 2 ++ ")" ++ "\n-\n" ++ concatMap (\p -> pixelToStr p ++ "\n") (pixels cluster)

formatFloatN :: Float -> Int -> String
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

clustersToStr :: [Cluster] -> String
clustersToStr = concatMap clusterToStr

printCluster :: Cluster -> IO ()
printCluster = putStrLn . clusterToStr

printClusters :: [Cluster] -> IO ()
printClusters = putStrLn . clustersToStr
