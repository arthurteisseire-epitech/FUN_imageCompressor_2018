import           ColorTest
import           PixelTest
import           PointTest
import           Test.HUnit

main :: IO ()
main = do
    runTestTT tests
    return ()

tests :: Test
tests = TestList $ colorTests ++ pointTests ++ pixelTests
