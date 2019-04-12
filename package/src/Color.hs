module Color where

data Color = Color
    { x :: Float
    , y :: Float
    , z :: Float
    } deriving (Show, Eq)

vplus :: Color -> Color -> Color
(Color x1 y1 z1) `vplus` (Color x2 y2 z2) = Color (x1 + x2) (y1 + y2) (z1 + z2)

vdist :: Color -> Color -> Float
(Color x1 y1 z1) `vdist` (Color x2 y2 z2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2
