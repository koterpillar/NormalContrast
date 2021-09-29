module Table where

import           Control.Monad

import           Ansi
import Utils

table ::
     [col]
  -> [row]
  -> (col -> String)
  -> (row -> String)
  -> (col -> row -> String)
  -> String
table cols rows showCol showRow cell = stringTable $ headerRow : cellRows
  where
    headerRow = "" : map showCol cols
    cellRows = map cellRow rows
    cellRow row = showRow row : map (`cell` row) cols

tableShow ::
     (Show col, Show row, Show cell)
  => [col]
  -> [row]
  -> (col -> row -> cell)
  -> String
tableShow cols rows f = table cols rows show show (\col row -> show $ f col row)

stringTable :: [[String]] -> String
stringTable cells = unlines $ map mkRow cells
  where
    maxWidth = maximum $ map ansiLength $ join cells
    mkRow = unwords . map (rpad maxWidth)
