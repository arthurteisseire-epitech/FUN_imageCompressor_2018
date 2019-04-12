import           Test.HUnit
import           Color

main :: IO ()
main = do
    runTestTT tests
    return ()

vectorEquals :: Test
vectorEquals = TestCase $ assertEqual "Color Equals" (Color 1 2 3) (Color 1 2 3)

vectorPlus :: Test
vectorPlus = TestCase $ assertEqual "Color Plus" (Color 2 4 6) (Color 1 2 3 `vplus` Color 1 2 3)

vectorDist :: Test
vectorDist = TestCase $ assertEqual "Color Dist" 5 (Color 3 4 5 `vdist` Color 3 8 2)

tests :: Test
tests = TestList [vectorEquals, vectorPlus, vectorDist]
