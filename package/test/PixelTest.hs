module PixelTest where

import           Color
import           Pixel
import           Point
import           Test.HUnit

pixelEqual :: Test
pixelEqual = TestCase $ assertEqual "Pixel Equal" (Pixel (Point 1 2) (Color 1 2 3)) (Pixel (Point 1 2) (Color 1 2 3))

pixelTests :: [Test]
pixelTests = [pixelEqual]
