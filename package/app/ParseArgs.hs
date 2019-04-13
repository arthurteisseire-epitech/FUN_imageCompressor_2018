module ParseArgs
    ( parseArgs
    , third
    ) where

import           Data.Bits
import           Data.Char
import           Data.Maybe
import           System.Exit
import           Text.Read

checkNbArgs :: [String] -> IO [String]
checkNbArgs args
    | length args == 3 = return args
    | otherwise = exitWithHelp

transfromArgs :: [String] -> IO (Int, Float, String)
transfromArgs args = do
    n <- getArg (head args) :: IO Int
    e <- getArg (args !! 1) :: IO Float
    let fileName = last args
    if n <= 0 || e <= 0 then
        exitWithHelp
    else
        return (n, e, fileName)

parseArgs :: [String] -> IO (Int, Float, String)
parseArgs args = checkNbArgs args >>= transfromArgs

getArg :: Read a => String -> IO a
getArg s =
    case readMaybe s of
        Just valid -> return valid
        Nothing    -> exitWithHelp

third :: (Int, Float, String) -> String
third (_, _, x) = x

exitWithHelp :: IO a
exitWithHelp = exitWith (ExitFailure 84)
