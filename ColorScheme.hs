module ColorScheme where

import           Control.Monad

import           Data.Char     (toLower, toUpper)
import           Data.List

import           Ansi
import           AnsiColor
import           Color
import           Utils

data ColorScheme =
  ColorScheme
    { csName       :: String
    , csBackground :: Color
    , csForeground :: Color
    , csCursor     :: Color
    , csCursorText :: Color
    , csColor      :: AnsiColor -> Color
    }

sampleCS :: ColorScheme -> String
sampleCS cs = unwords $ map showColor ansiColors
  where
    showColor c =
      withBackFore (csBackground cs) (csColor cs c) (showAnsiColor c)

showAnsiColor :: AnsiColor -> String
showAnsiColor (Normal c) = [toLower $ primColorSym c]
showAnsiColor (Bright c) = [toUpper $ primColorSym c]

displayCS :: ColorScheme -> String
displayCS cs =
  unlines $
  [csName cs] ++ map colorLineCS primColors ++ [colorLineMeta] ++ contrastLines
  where
    displayWidth = 3
    square :: IsColor c => c -> String
    square c = "[" ++ withBackground c " " ++ "]"
    colorLineCS c = colorLine [csColor cs (Normal c), csColor cs (Bright c)]
    colorLineMeta = colorLine [csForeground cs, csBackground cs, csCursor cs]
    colorLine = colorLine' 0 . sortOn linearLuminance . nub
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
    backgroundGroup = ("Back", [csBackground cs])
    foregroundGroup = ("Fore", [csForeground cs])
    groupPairs =
      [ (g, backgroundGroup)
      | g <- [blackGroup, darkGroup, lightGroup, whiteGroup]
      ] ++
      [ (blackGroup, lightGroup)
      , (blackGroup, darkGroup)
      , (darkGroup, lightGroup)
      ] ++
      [ (foregroundGroup, g)
      | g <- [blackGroup, darkGroup, lightGroup, whiteGroup]
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
showD v = show $ (fromIntegral (round (v * 10) :: Integer) :: Double) / 10

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
