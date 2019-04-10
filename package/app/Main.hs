module Main where

import Vector

main :: IO ()
main = print $ show $ Vector 1 2 3 `vplus` Vector 4 5 6
