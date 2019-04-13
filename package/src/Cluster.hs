module Cluster where

import           Pixel

data Cluster = Cluster
    { r     :: Float
    , g     :: Float
    , b     :: Float
    , pixel :: Pixel
    } deriving (Show, Eq)

