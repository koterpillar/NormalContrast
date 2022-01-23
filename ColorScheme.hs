module ColorScheme
  ( ColorScheme(..)
  ) where

import           AnsiColor
import           Color

data ColorScheme =
  ColorScheme
    { csName       :: String
    , csBackground :: Color
    , csForeground :: Color
    , csCursor     :: Color
    , csCursorText :: Color
    , csColor      :: AnsiColor -> Color
    }
