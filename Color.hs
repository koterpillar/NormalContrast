{-# LANGUAGE RecordWildCards #-}

module Color where

import           Data.List

import           Ansi

data Color =
  Color
    { cRed   :: Int
    , cGreen :: Int
    , cBlue  :: Int
    }
  deriving (Eq, Show)

instance IsColor Color where
  setForeground Color {..} = colorSeq [38, 2, cRed, cGreen, cBlue]
  setBackground Color {..} = colorSeq [48, 2, cRed, cGreen, cBlue]

-- Luminance is not that easy, see https://stackoverflow.com/a/596241/288201
-- https://contrastchecker.com/ and
-- https://webaim.org/resources/contrastchecker/ agree on this formula
luminance :: Color -> Double
luminance Color {..} = 0.2126 * r + 0.7152 * g + 0.0722 * b
  where
    gamma v
      | v <= 0.03928 = v / 12.92
      | otherwise = ((v + 0.055) / 1.055) ** 2.4
    toRange v = fromIntegral v / 255
    r = gamma $ toRange cRed
    g = gamma $ toRange cGreen
    b = gamma $ toRange cBlue

allColors :: [Color]
allColors = [Color r g b | r <- range, g <- range, b <- range]
  where
    range = [0,16 .. 255]

allColorsByLuminance :: [Color]
allColorsByLuminance = sortOn luminance allColors

linearLuminance :: Color -> Double
linearLuminance c = log (luminance c + 0.05) + 3

contrast :: Color -> Color -> Double
contrast c1 c2 = exp $ abs (linearLuminance c1 - linearLuminance c2)

-- https://www.accessibility-developer-guide.com/knowledge/colours-and-contrast/text/
perfectContrast :: Double
perfectContrast = 4.5

goodContrast :: Double
goodContrast = 3.0

lastResortContrast :: Double
lastResortContrast = 1.7 -- experimental

colorArgMax :: Int
colorArgMax = 2048

colorArgRange :: [Int]
colorArgRange = [0 .. colorArgMax]

makeByLuminance :: (Int -> Color) -> Double -> Color
makeByLuminance mk l = makeByInternal colorArgRange luminousEnough mk
  where
    luminousEnough c = luminance c >= l

makeByContrastLight :: (Int -> Color) -> Color -> Double -> Color
makeByContrastLight = makeByContrastInternal colorArgRange (>)

makeByContrastDark :: (Int -> Color) -> Color -> Double -> Color
makeByContrastDark = makeByContrastInternal (reverse colorArgRange) (<)

makeByContrastInternal ::
     [Int]
  -> (Double -> Double -> Bool)
  -> (Int -> Color)
  -> Color
  -> Double
  -> Color
makeByContrastInternal rng lumPred mk cb v = makeByInternal rng good mk
  where
    good c = enoughContrast c cb && luminance c `lumPred` luminance cb
    enoughContrast c1 c2 = contrast c1 c2 >= v

makeByInternal :: [Int] -> (Color -> Bool) -> (Int -> Color) -> Color
makeByInternal rng pred mk = head $ filter pred $ map mk rng

mkGrey :: Int -> Color
mkGrey v0 = Color v v v
  where
    (_, v) = bleed 0 v0

-- Red is special as (255, 0, 0) is still too dark
mkRed :: Int -> Color
mkRed v0 = Color v b b
  where
    (b, v) = bleed 100 v0

mkGreen :: Int -> Color
mkGreen v0 = Color 0 v 0
  where
    (_, v) = bleed 0 v0

-- Blue is special as (0, 0, 255) is still too dark
mkBlue :: Int -> Color
mkBlue v0 = Color b b v
  where
    (b, v) = bleed 200 v0

bleed :: Int -> Int -> (Int, Int)
bleed extra v0 = result (v0 * (255 + extra) `div` colorArgMax)
  where
    result v
      | v < 255 = (0, v)
      | otherwise = (v - 255, 255)

mkYellow :: Int -> Color
mkYellow v0 = Color v v 0
  where
    (_, v) = bleed 0 v0

-- Also special (because it's green and blue?)
mkMagenta :: Int -> Color
mkMagenta v0 = Color v b v
  where
    (b, v) = bleed 50 v0

mkCyan :: Int -> Color
mkCyan v0 = Color 0 v v
  where
    (_, v) = bleed 0 v0
