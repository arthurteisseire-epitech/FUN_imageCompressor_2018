module Color where

data Color = Color
    { r :: Float
    , g :: Float
    , a :: Float
    } deriving (Show, Eq)

vplus :: Color -> Color -> Color
(Color r1 g1 b1) `vplus` (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)

vdist :: Color -> Color -> Float
(Color r1 g1 b1) `vdist` (Color r2 g2 b2) = sqrt $ (r1 - r2) ^ 2 + (g1 - g2) ^ 2 + (b1 - b2) ^ 2
