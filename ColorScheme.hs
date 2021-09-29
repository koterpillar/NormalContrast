{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module ColorScheme where

import           Control.Monad

import           Data.Char     (toLower, toUpper)
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

displayCS :: ColorScheme -> String
displayCS cs = unlines $ map colorLineCS primColors ++ [ruler]
  where
    displayWidth = 3
    square :: IsColor c => c -> String
    square c = "[" ++ withBackground c " " ++ "]"
    colorLineCS c =
      colorLine 0 $
      nub
        [ cs ^. csColor (Normal Black)
        , cs ^. csColor (Normal c)
        , cs ^. csColor (Bright c)
        , cs ^. background
        ]
    colorLine _ [] = ""
    colorLine prevPos (c:rest) = padding ++ square c ++ colorLine nextPos rest
      where
        nextPos = colorPos c + displayWidth
        padding = replicate (colorPos c - prevPos) ' '
    colorPos c = round (linearLuminance c * 40)
    darkColors = map (\c -> cs ^. csColor (Normal c)) $ delete Black primColors
    lightColors = map (\c -> cs ^. csColor (Bright c)) $ delete White primColors
    colorGroups =
      map
        (sortOn linearLuminance)
        [ [cs ^. csColor (Normal Black)]
        , darkColors
        , lightColors
        , [cs ^. csColor (Bright White)]
        , [cs ^. background]
        ]
    rulerPairs = zipWith mkColorPair colorGroups (tail colorGroups)
    mkColorPair g1 g2 = (last g1, head g2)
    ruler = join $ map (uncurry rulerPart) rulerPairs
    rulerPart = showContrast

showContrastCS :: ColorScheme -> AnsiColor -> AnsiColor -> String
showContrastCS _ (Normal _) (Normal _) = ""
showContrastCS _ (Bright _) (Bright _) = ""
showContrastCS _ (Normal c1) (Bright c2)
  | c1 == c2 = ""
showContrastCS _ (Bright c1) (Normal c2)
  | c1 == c2 = ""
showContrastCS cs c1 c2 = showContrast (cs ^. csColor c1) (cs ^. csColor c2)

showContrast :: Color -> Color -> String
showContrast c1 c2 =
  withBackFore c1 c2 "x" ++ withBackFore c2 c1 "x" ++ showD (contrast c1 c2)

showD :: Double -> String
showD v = show $ (fromIntegral (round (v * 10)) :: Double) / 10

naiveCS :: ColorScheme
naiveCS =
  ColorScheme
    { _background = mkGrey 255
    , _colors =
        \case
          Normal Black -> Color 0 0 0
          Normal Red -> Color 128 0 0
          Normal Green -> Color 0 128 0
          Normal Yellow -> Color 128 128 0
          Normal Blue -> Color 0 0 128
          Normal Magenta -> Color 128 0 128
          Normal Cyan -> Color 0 128 128
          Normal White -> Color 192 192 192
          Bright Black -> Color 128 128 128
          Bright Red -> Color 255 0 0
          Bright Green -> Color 0 255 0
          Bright Yellow -> Color 255 255 0
          Bright Blue -> Color 0 0 255
          Bright Magenta -> Color 255 0 255
          Bright Cyan -> Color 0 255 255
          Bright White -> Color 255 255 255
    }

-- contrast between:
-- * background and all colors
-- * all normal and all bright, pairwise
contrastCS :: ColorScheme
contrastCS = ColorScheme {_background = white, _colors = colors}
  where
    white = mkGrey 255
    black = mkGrey 0
    againstWhite mk = makeByContrastDark mk white goodContrast
    againstBlack mk = makeByContrastLight mk black goodContrast
    colors (Normal Black) = black
    colors (Normal Red) = againstBlack mkRed
    colors (Normal Green) = againstBlack mkGreen
    colors (Normal Yellow) = againstBlack mkYellow
    colors (Normal Blue) = mkBlue 128
    colors (Normal Magenta) = againstBlack mkMagenta
    colors (Normal Cyan) = againstBlack mkCyan
    colors (Normal White) = againstBlack mkGrey
    colors (Bright Black) = againstWhite mkGrey
    colors (Bright Red) = againstWhite mkRed
    colors (Bright Green) = againstWhite mkGreen
    colors (Bright Yellow) = againstWhite mkYellow
    colors (Bright Blue) = againstWhite mkBlue
    colors (Bright Magenta) = againstWhite mkMagenta
    colors (Bright Cyan) = againstWhite mkCyan
    colors (Bright White) = makeByContrastDark mkGrey white lastResortContrast
