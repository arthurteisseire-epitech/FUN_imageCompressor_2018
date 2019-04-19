module PixelTest where

import           Color
import           Pixel
import           Point
import           Test.HUnit

pixelEqual :: Test
pixelEqual = TestCase $ assertEqual "Pixel Equal" (Pixel (Point 1 2) (Color 1 2 3)) (Pixel (Point 1 2) (Color 1 2 3))

pixelParserTest :: Test
pixelParserTest = TestCase $ assertEqual "Pixel parser" (Just $ Pixel (Point 1 2) (Color 1 2 3)) (strToPixel "(1,2) (1,2,3)")

pixelParserTestFail :: Test
pixelParserTestFail = TestCase $ assertEqual "Pixel Fail" Nothing (strToPixel "(1,2)(1,2,3)")

pixelTestColorEq :: Test
pixelTestColorEq = TestCase $ assertEqual "Pixel Color Eq" True (pixelColorEq (Pixel (Point 1 1) (Color 1 2 3)) (Pixel (Point 2 3) (Color 1 2 3)))

pixelTests :: [Test]
pixelTests = [ pixelEqual
             , pixelParserTest
             , pixelParserTestFail
             , pixelTestColorEq
             ]
