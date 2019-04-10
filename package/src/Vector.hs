module Vector where

data Vector = Vector
    { x :: Int
    , y :: Int
    , z :: Int
    } deriving (Show, Eq)

vplus :: Vector -> Vector -> Vector
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

