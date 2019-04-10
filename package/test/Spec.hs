import Test.HUnit
import Vector

main :: IO ()
main = do
    runTestTT tests
    return ()

vectorEquals :: Test
vectorEquals = TestCase $ assertEqual "Vectors Equals" (Vector 1 2 3) (Vector 1 2 3)

vectorPlus :: Test
vectorPlus = TestCase $ assertEqual "Vector Plus" (Vector 2 4 6) (Vector 1 2 3 `vplus` Vector 1 2 3)

tests :: Test
tests = TestList [TestLabel "Vector Equals" vectorEquals, TestLabel "Vector Plus" vectorPlus]
