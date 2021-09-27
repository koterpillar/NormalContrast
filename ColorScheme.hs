{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module ColorScheme where

import           Data.Char     (toLower, toUpper)
import           Data.List

import           Lens.Micro
import           Lens.Micro.TH

import           Ansi
import           AnsiColor
import           Color
import           Table

data ColorScheme =
  ColorScheme
    { _background :: Color
    , _colors     :: AnsiColor -> Color
    }

makeLenses ''ColorScheme

csColor c = colors . to ($ c)

sampleCS :: ColorScheme -> String
sampleCS cs = unwords $ map showColor ansiColors
  where
    showColor c =
      withBackFore (cs ^. background) (cs ^. csColor c) (showAnsiColor c)
    ansiToPrim (Normal c) = c
    ansiToPrim (Bright c) = c

showAnsiColor :: AnsiColor -> String
showAnsiColor (Normal c) = [toLower $ primColorSym c]
showAnsiColor (Bright c) = [toUpper $ primColorSym c]

tableCS :: ColorScheme -> String
tableCS cs =
  table (Nothing : map Just ansiColors) ansiColors showCol showRow cell
  where
    showCol Nothing  = ""
    showCol (Just c) = withForeground c $ showAnsiColor c
    showRow c = withBackground c $ showAnsiColor c
    cell Nothing c    = showD $ contrast (cs ^. background) (cs ^. csColor c)
    cell (Just c1) c2 = showD $ contrast (cs ^. csColor c1) (cs ^. csColor c2)

showD :: Double -> String
showD v = show $ (fromIntegral (round (v * 100)) :: Double) / 100

naiveCS :: ColorScheme
naiveCS =
  ColorScheme
    { _background = mkGrey 255
    , _colors =
        \case
          Normal Black -> mkGrey 0
          Normal Red -> mkRed 128
          Normal Green -> mkGreen 128
          Normal Yellow -> mkYellow 128
          Normal Blue -> mkBlue 128
          Normal Magenta -> mkMagenta 128
          Normal Cyan -> mkCyan 128
          Normal White -> mkGrey 192
          Bright Black -> mkGrey 128
          Bright Red -> mkRed 255
          Bright Green -> mkGreen 255
          Bright Yellow -> mkYellow 255
          Bright Blue -> mkBlue 255
          Bright Magenta -> mkMagenta 255
          Bright Cyan -> mkCyan 255
          Bright White -> mkGrey 255
    }
-- contrast between:
-- * background and all colors
-- * all normal and all bright, pairwise
