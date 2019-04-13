module Main where

import System.Environment
import System.Exit
import Pixel
import Data.Maybe

main :: IO ()
main = getArgs >>= checkNbArgs >>= getPixels >>= compute

checkNbArgs :: [String] -> IO [String]
checkNbArgs args
    | length args == 3 = return args
    | otherwise = exitWithHelp

compute :: [Pixel] -> IO ()
compute [] = putStrLn "No valid pixels given"
compute pixels = printPixel $ head pixels

getPixels :: [String] -> IO [Pixel]
getPixels args = catMaybes <$> fileToPixels (head args)

fileToPixels :: String -> IO [Maybe Pixel]
fileToPixels fileName = textToPixels <$> readFile fileName

exitWithHelp :: IO a
exitWithHelp = exitWith (ExitFailure 84)
