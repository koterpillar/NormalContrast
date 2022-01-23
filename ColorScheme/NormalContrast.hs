module ColorScheme.NormalContrast where

import AnsiColor
import Color
import ColorScheme

normalContrastCS :: ColorScheme
normalContrastCS =
  ColorScheme
    { csName = "NormalContrast Light"
    , csBackground = white
    , csForeground = black
    , csCursor = makeByContrastLight mkMagenta black lastResortContrast
    , csCursorText = white
    , csColor = colors
    }
  where
    white = mkGrey colorArgMax
    black = mkGrey 0
    againstWhite mk = makeByContrastDark mk white goodContrast
    againstBlack mk = makeByContrastLight mk black goodContrast
    colors (Normal Black)   = black
    colors (Normal Red)     = againstBlack mkRed
    colors (Normal Green)   = againstBlack mkGreen
    colors (Normal Yellow)  = againstBlack mkYellow
    colors (Normal Blue)    = againstBlack mkBlue
    colors (Normal Magenta) = againstBlack mkMagenta
    colors (Normal Cyan)    = againstBlack mkCyan
    colors (Normal White)   = againstWhite mkGrey
    colors (Bright Black)   = againstBlack mkGrey
    colors (Bright Red)     = againstWhite mkRed
    colors (Bright Green)   = againstWhite mkGreen
    colors (Bright Yellow)  = againstWhite mkYellow
    colors (Bright Blue)    = againstWhite mkBlue
    colors (Bright Magenta) = againstWhite mkMagenta
    colors (Bright Cyan)    = againstWhite mkCyan
    colors (Bright White)   = makeByContrastDark mkGrey white lastResortContrast
