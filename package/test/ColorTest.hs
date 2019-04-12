module ColorTest where

import           Test.HUnit
import           Color

colorEqual :: Test
colorEqual = TestCase $ assertEqual "Color Equals" (Color 1 2 3) (Color 1 2 3)

colorPlus :: Test
colorPlus = TestCase $ assertEqual "Color Plus" (Color 2 4 6) (Color 1 2 3 `vplus` Color 1 2 3)

colorDist :: Test
colorDist = TestCase $ assertEqual "Color Dist" 5 (Color 3 4 5 `vdist` Color 3 8 2)

colorTests :: [Test]
colorTests = [colorEqual, colorPlus, colorDist]
