{-# LANGUAGE TemplateHaskell #-}

module ColorScheme where

import           Lens.Micro
import           Lens.Micro.TH

import           AnsiColor
import           Color

data ColorScheme =
  ColorScheme
    { _background :: Color
    , _colors     :: AnsiColor -> Color
    }

makeLenses ''ColorScheme
