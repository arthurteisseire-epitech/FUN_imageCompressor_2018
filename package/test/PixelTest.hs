module PixelTest where

import           Color
import           Pixel
import           Point
import           Test.HUnit

pixelEqual :: Test
pixelEqual = TestCase $ assertEqual "Pixel Equal" (Pixel (Point 1 2) (Color 1 2 3)) (Pixel (Point 1 2) (Color 1 2 3))

pixelParserTest :: Test
pixelParserTest = TestCase $ assertEqual "Point parser" (Just $ Pixel (Point 1 2) (Color 1 2 3)) (strToPixel "(1,2) (1,2,3)")

pixelParserTestFail :: Test
pixelParserTestFail = TestCase $ assertEqual "Point parser" Nothing (strToPixel "(1,2)(1,2,3)")

pixelTests :: [Test]
pixelTests = [ pixelEqual
             , pixelParserTest
             ]
