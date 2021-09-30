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
import           Utils

data ColorScheme =
  ColorScheme
    { _background :: Color
    , _colors     :: AnsiColor -> Color
    }

makeLenses ''ColorScheme

csColor :: ColorScheme -> AnsiColor -> Color
csColor cs = cs ^. colors

sampleCS :: ColorScheme -> String
sampleCS cs = unwords $ map showColor ansiColors
  where
    showColor c =
      withBackFore (cs ^. background) (csColor cs c) (showAnsiColor c)
    ansiToPrim (Normal c) = c
    ansiToPrim (Bright c) = c

showAnsiColor :: AnsiColor -> String
showAnsiColor (Normal c) = [toLower $ primColorSym c]
showAnsiColor (Bright c) = [toUpper $ primColorSym c]

displayCS :: ColorScheme -> String
displayCS cs =
  unlines $ map colorLineCS primColors ++ [colorLineBG] ++ contrastLines
  where
    displayWidth = 3
    square :: IsColor c => c -> String
    square c = "[" ++ withBackground c " " ++ "]"
    colorLineCS c = colorLine [csColor cs (Normal c), csColor cs (Bright c)]
    colorLineBG = colorLine [cs ^. background]
    colorLine = colorLine' 0
    colorLine' _ [] = ""
    colorLine' prevPos (c:rest) = padding ++ square c ++ colorLine' nextPos rest
      where
        nextPos = colorPos c + displayWidth
        padding = replicate (colorPos c - prevPos) ' '
    colorPos c = round (linearLuminance c * 40)
    blackGroup = ("Black", [csColor cs (Normal Black)])
    darkGroup =
      ( "Dark"
      , map (csColor cs) $
        map Normal (delete White $ delete Black primColors) ++ [Bright Black])
    lightGroup =
      ( "Light"
      , map (csColor cs) $
        map Bright (delete White $ delete Black primColors) ++ [Normal White])
    whiteGroup = ("White", [csColor cs (Bright White)])
    backgroundGroup = ("Back", [cs ^. background])
    groupPairs =
      [ (blackGroup, lightGroup)
      , (darkGroup, backgroundGroup)
      , (blackGroup, darkGroup)
      , (darkGroup, lightGroup)
      , (lightGroup, backgroundGroup)
      , (whiteGroup, backgroundGroup)
      ]
    groups = nub $ join $ map (\(g1, g2) -> [g1, g2]) groupPairs
    maxNameLen = maximum $ map (length . fst) groups
    contrastLines = map (uncurry contrastLine) groupPairs
    contrastLine (n1, g1) (n2, g2) =
      lpad maxNameLen n1 ++
      "/" ++ rpad maxNameLen n2 ++ ": " ++ showContrast ca cb
      where
        (ca, cb) =
          head $ sortOn (uncurry contrast) [(c1, c2) | c1 <- g1, c2 <- g2]

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
    colors (Normal Blue) = againstBlack mkBlue
    colors (Normal Magenta) = againstBlack mkMagenta
    colors (Normal Cyan) = againstBlack mkCyan
    colors (Normal White) = againstWhite mkGrey
    colors (Bright Black) = againstBlack mkGrey
    colors (Bright Red) = againstWhite mkRed
    colors (Bright Green) = againstWhite mkGreen
    colors (Bright Yellow) = againstWhite mkYellow
    colors (Bright Blue) = againstWhite mkBlue
    colors (Bright Magenta) = againstWhite mkMagenta
    colors (Bright Cyan) = againstWhite mkCyan
    colors (Bright White) = makeByContrastDark mkGrey white lastResortContrast
