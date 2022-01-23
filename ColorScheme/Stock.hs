module ColorScheme.Stock where

import           AnsiColor
import           Color
import           ColorScheme

windowsXP :: ColorScheme
windowsXP =
  ColorScheme
    { csName = "Windows XP"
    , csBackground = black
    , csForeground = white
    , csCursor = white
    , csCursorText = black
    , csColor =
        \case
          Normal Black   -> Color 0 0 0
          Normal Red     -> Color 128 0 0
          Normal Green   -> Color 0 128 0
          Normal Yellow  -> Color 128 128 0
          Normal Blue    -> Color 0 0 128
          Normal Magenta -> Color 128 0 128
          Normal Cyan    -> Color 0 128 128
          Normal White   -> Color 192 192 192
          Bright Black   -> Color 128 128 128
          Bright Red     -> Color 255 0 0
          Bright Green   -> Color 0 255 0
          Bright Yellow  -> Color 255 255 0
          Bright Blue    -> Color 0 0 255
          Bright Magenta -> Color 255 0 255
          Bright Cyan    -> Color 0 255 255
          Bright White   -> Color 255 255 255
    }
  where
    black = Color 0 0 0
    white = Color 255 255 255
