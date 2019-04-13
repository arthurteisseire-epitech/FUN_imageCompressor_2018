module PointTest where

import           Point
import           Test.HUnit

pointEqual :: Test
pointEqual = TestCase $ assertEqual "Point Equal" (Point 1 2) (Point 1 2)

pointParserTest :: Test
pointParserTest = TestCase $ assertEqual "Point parser" (pointParse "(1,2)") (Point 1 2)

pointParserMultipleDigitTest :: Test
pointParserMultipleDigitTest = TestCase $ assertEqual "Point parser" (pointParse "(11,22)") (Point 11 22)

pointTests :: [Test]
pointTests = [pointEqual, pointParserTest, pointParserMultipleDigitTest]
