import           Test.HUnit
import ColorTest

main :: IO ()
main = do
    runTestTT tests
    return ()

tests :: Test
tests = colorTests
