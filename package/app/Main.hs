module Main where

import System.Environment
import System.Exit
import Vector

main :: IO ()
main = getArgs >>= checkNbArgs >>= rFile

checkNbArgs :: [String] -> IO [String]
checkNbArgs args
    | length args == 3 = return args
    | otherwise = exitWith (ExitFailure 84)

rFile :: [String] -> IO ()
rFile (fileName:args) = putStrLn fileName
