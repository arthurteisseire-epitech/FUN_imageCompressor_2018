module Pixel where

import Point
import Color

data Pixel = Pixel
    { point :: Point
    , color :: Color
    } deriving (Show, Eq)
