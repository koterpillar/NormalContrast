{-# LANGUAGE TemplateHaskell #-}

module Color where

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
luminance :: Color -> Double
luminance c = sum vs / 3
  where
    is = c ^.. components
    vs = map fromIntegral is

contrast :: Color -> Color -> Double
contrast c1 c2 = (la + 0.05) / (lb + 0.05)
  where
    la = max l1 l2
    lb = min l1 l2
    l1 = luminance c1
    l2 = luminance c2

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
