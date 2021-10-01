{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad

import           Data.Foldable
import           Data.List

import           Ansi
import           AnsiColor
import           Color
import           ColorScheme
import           Export

roll :: (Int -> Color) -> String
roll mk =
  join
    [withBackground (mk v) "  " | v <- [0,colorArgMax `div` 128 .. colorArgMax]]

main :: IO ()
main = do
  putStrLn $ displayCS naiveCS
  traverse_
    (putStrLn . roll)
    [mkGrey, mkRed, mkGreen, mkBlue, mkCyan, mkMagenta, mkYellow, mkGrey]
  putStrLn $ displayCS contrastCS
  exportITerm contrastCS
