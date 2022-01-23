module ColorScheme.Display
  ( displayCS
  ) where

import           Data.Char   (toLower, toUpper)
import           Data.List

import           Ansi
import           AnsiColor
import           Color
import           ColorScheme
import           Table

displayCS :: ColorScheme -> String
displayCS cs =
  unlines $
  [csName cs] ++
  map colorLineCS primColors ++
  [colorLineMeta] ++ [contrastTableGroup] ++ [contrastTable]
  where
    displayWidth = length $ square $ Color 0 0 0
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
    middleColors = delete White $ delete Black primColors
    groups =
      [ ("Black", [csColor cs (Normal Black)])
      , ("Dark", map (csColor cs) $ map Normal middleColors ++ [Bright Black])
      , ("Light", map (csColor cs) $ map Bright middleColors ++ [Normal White])
      , ("White", [csColor cs (Bright White)])
      , ("Back", [csBackground cs])
      , ("Fore", [csForeground cs])
      ]
    contrastTableGroup = tableSym groups fst contrastCellGroup
    contrastCellGroup (n1, g1) (n2, g2)
      | n1 == n2 = ""
      | otherwise = showContrast ca cb
      where
        (ca, cb) =
          head $ sortOn (uncurry contrast) [(c1, c2) | c1 <- g1, c2 <- g2]
    colors =
      [("back", csBackground cs), ("fore", csForeground cs)] ++
      [(showAnsiColor c, csColor cs c) | c <- ansiColors]
    contrastTable = tableSym colors fst contrastCell
    contrastCell (n1, c1) (n2, c2)
      | n1 == n2 = ""
      | otherwise = showContrast c1 c2

showAnsiColor :: AnsiColor -> String
showAnsiColor (Normal c) = [toLower $ primColorSym c]
showAnsiColor (Bright c) = [toUpper $ primColorSym c]

showContrast :: Color -> Color -> String
showContrast c1 c2 =
  withBackFore c1 c2 "x" ++ withBackFore c2 c1 "x" ++ showD (contrast c1 c2)

showD :: Double -> String
showD v = show $ (fromIntegral (round (v * 10) :: Integer) :: Double) / 10
