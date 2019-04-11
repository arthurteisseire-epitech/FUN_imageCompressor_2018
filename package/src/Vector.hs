module Vector where

data Vector = Vector
    { x :: Float
    , y :: Float
    , z :: Float
    } deriving (Show, Eq)

vplus :: Vector -> Vector -> Vector
(Vector x1 y1 z1) `vplus` (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

vdist :: Vector -> Vector -> Float
(Vector x1 y1 z1) `vdist` (Vector x2 y2 z2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2)
