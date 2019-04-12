module Main where

import System.Environment
import System.Exit
import Vector

main :: IO ()
main = getArgs >>= checkNbArgs >>= readInputFile

checkNbArgs :: [String] -> IO [String]
checkNbArgs args
    | length args == 3 = return args
    | otherwise = exitWith (ExitFailure 84)

readInputFile :: [String] -> IO ()
readInputFile (fileName:args) = putStrLn fileName
