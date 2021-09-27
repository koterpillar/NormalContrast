{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module ColorScheme where

import           Data.List

import           Lens.Micro
import           Lens.Micro.TH

import           Ansi
import           AnsiColor
import           Color

data ColorScheme =
  ColorScheme
    { _background :: Color
    , _colors     :: AnsiColor -> Color
    }

makeLenses ''ColorScheme

sampleCS :: ColorScheme -> String
sampleCS cs = unwords $ map showColor ansiColors
  where
    showColor c =
      withBackFore
        (cs ^. background)
        (cs ^. colors . to ($ c))
        [primColorSym $ ansiToPrim c]
    ansiToPrim (Normal c) = c
    ansiToPrim (Bright c) = c

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
