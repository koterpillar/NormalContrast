{-# LANGUAGE TemplateHaskell #-}

module Color where

import           Data.List

import           Lens.Micro
import           Lens.Micro.TH

import           Ansi

data Color =
  Color
    { _red   :: Int
    , _green :: Int
    , _blue  :: Int
    }
  deriving (Eq, Show)

makeLenses ''Color

components :: Traversal' Color Int
components f (Color r g b) = Color <$> f r <*> f g <*> f b

instance IsColor Color where
  setForeground c = colorSeq [38, 2, c ^. red, c ^. green, c ^. blue]
  setBackground c = colorSeq [48, 2, c ^. red, c ^. green, c ^. blue]

-- https://www.accessibility-developer-guide.com/knowledge/colours-and-contrast/how-to-calculate/
-- Luminance is not that easy, see https://stackoverflow.com/a/596241/288201
luminance :: Color -> Double
luminance c = 0.2126 * r + 0.7152 * g + 0.0722 * b
  where
    r = fromIntegral (c ^. red) / 255
    g = fromIntegral (c ^. green) / 255
    b = fromIntegral (c ^. blue) / 255

allColors :: [Color]
allColors = [Color r g b | r <- range, g <- range, b <- range]
  where
    range = [0,16 .. 255]

allColorsByLuminance :: [Color]
allColorsByLuminance = sortOn luminance allColors

contrast :: Color -> Color -> Double
contrast c1 c2 = (la + 0.05) / (lb + 0.05)
  where
    la = max l1 l2
    lb = min l1 l2
    l1 = luminance c1
    l2 = luminance c2

-- https://www.accessibility-developer-guide.com/knowledge/colours-and-contrast/text/
legibleContrast :: Double
legibleContrast = 4.5

mkGrey :: Int -> Color
mkGrey v = Color v v v

mkRed :: Int -> Color
mkRed v = Color v 0 0

mkGreen :: Int -> Color
mkGreen v = Color 0 v 0

mkBlue :: Int -> Color
mkBlue = Color 0 0

mkYellow :: Int -> Color
mkYellow v = Color v v 0

mkMagenta :: Int -> Color
mkMagenta v = Color v 0 v

mkCyan :: Int -> Color
mkCyan v = Color 0 v v
