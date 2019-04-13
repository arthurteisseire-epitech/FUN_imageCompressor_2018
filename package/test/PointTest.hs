module PointTest where

import           Point
import           Test.HUnit

pointEqual :: Test
pointEqual = TestCase $ assertEqual "Point Equal" (Point 1 2) (Point 1 2)

pointParserTest :: Test
pointParserTest = TestCase $ assertEqual "Point parser" (strToPoint "(1,2)") (Just $ Point 1 2)

pointParserTestFail :: Test
pointParserTestFail = TestCase $ assertEqual "Point parser" Nothing (strToPoint "(1,2,3)")

pointParserMultipleDigitTest :: Test
pointParserMultipleDigitTest = TestCase $ assertEqual "Point parser" (Just $ Point 11 22) (strToPoint "(11,22)")

pointTests :: [Test]
pointTests = [ pointEqual
             , pointParserTest
             , pointParserTestFail
             , pointParserMultipleDigitTest
             ]
