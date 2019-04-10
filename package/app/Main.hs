module Main where

import           Lib

data Vector = Vector
    { x :: Int
    , y :: Int
    , z :: Int
    } deriving (Show)

vplus :: Vector -> Vector -> Vector
(Vector i j k) `vplus` (Vector l m n) = Vector (i + m) (j + m) (k + n)

main :: IO ()
main = print $ show $ Vector 1 2 3 `vplus` Vector 4 5 6
