import           Test.HUnit
import           Vector

main :: IO ()
main = do
    runTestTT tests
    return ()

vectorEquals :: Test
vectorEquals = TestCase $ assertEqual "Vectors Equals" (Vector 1 2 3) (Vector 1 2 3)

vectorPlus :: Test
vectorPlus = TestCase $ assertEqual "Vector Plus" (Vector 2 4 6) (Vector 1 2 3 `vplus` Vector 1 2 3)

vectorDist :: Test
vectorDist = TestCase $ assertEqual "Vector Dist" 5 (Vector 3 4 5 `vdist` Vector 3 8 2)

tests :: Test
tests = TestList [vectorEquals, vectorPlus, vectorDist]
