{-# LANGUAGE RecordWildCards #-}

module Color where

import           Data.List

import           Ansi

import           Numeric   (showHex)

import           Utils     (lpad')

data Color =
  Color
    { cRed   :: Int
    , cGreen :: Int
    , cBlue  :: Int
    }
  deriving (Eq, Show)

cComponents :: Color -> [Int]
cComponents Color {..} = [cRed, cGreen, cBlue]

instance IsColor Color where
  setForeground c = colorSeq $ [38, 2] ++ cComponents c
  setBackground c = colorSeq $ [48, 2] ++ cComponents c

cHex :: Color -> String
cHex = ('#' :) . concatMap hexComponent . cComponents
  where
    hexComponent i = lpad' '0' 2 $ showHex i ""

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
makeByInternal rng fn mk = head $ filter fn $ map mk rng

mkGrey :: Int -> Color
mkGrey v0 = Color v v v
  where
    (_, v) = bleed 0 v0

-- Bleed colors containing red and blue into white, otherwise they are too
-- dark without green at all
mkRed :: Int -> Color
mkRed v0 = Color v b b
  where
    (b, v) = bleed 200 v0

mkGreen :: Int -> Color
mkGreen v0 = Color 0 v 0
  where
    (_, v) = bleed 0 v0

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

mkMagenta :: Int -> Color
mkMagenta v0 = Color v b v
  where
    (b, v) = bleed 200 v0

mkCyan :: Int -> Color
mkCyan v0 = Color 0 v v
  where
    (_, v) = bleed 0 v0
