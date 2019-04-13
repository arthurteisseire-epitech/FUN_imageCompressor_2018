module ColorTest where

import           Test.HUnit
import           Color

colorEqual :: Test
colorEqual = TestCase $ assertEqual "Color Equals" (Color 1 2 3) (Color 1 2 3)

colorPlus :: Test
colorPlus = TestCase $ assertEqual "Color Plus" (Color 2 4 6) (Color 1 2 3 `vplus` Color 1 2 3)

colorDist :: Test
colorDist = TestCase $ assertEqual "Color Dist" 5 (Color 3 4 5 `vdist` Color 3 8 2)

colorParserTest :: Test
colorParserTest = TestCase $ assertEqual "Point parser" (Just $ Color 1 2 3) (strToColor "(1,2,3)")

colorParserTestTooBig :: Test
colorParserTestTooBig = TestCase $ assertEqual "Point parser" Nothing (strToColor "(256,2,3)")

colorParserTestMultipleDigit :: Test
colorParserTestMultipleDigit = TestCase $ assertEqual "Point parser" (Just $ Color 123 234 123) (strToColor "(123,234,123)")

colorParserTestNegative :: Test
colorParserTestNegative = TestCase $ assertEqual "Point parser" Nothing (strToColor "(-1,1,1)")

colorTests :: [Test]
colorTests = [ colorEqual
             , colorPlus
             , colorDist
             , colorParserTest
             , colorParserTestTooBig
             , colorParserTestMultipleDigit
             , colorParserTestNegative
             ]
