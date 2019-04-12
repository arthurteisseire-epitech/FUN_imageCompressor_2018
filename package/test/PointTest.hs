module PointTest where

import           Point
import           Test.HUnit

pointEqual :: Test
pointEqual = TestCase $ assertEqual "Point Equal" (Point 1 2) (Point 1 2)

pointTests :: [Test]
pointTests = [pointEqual]
