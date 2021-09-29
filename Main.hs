{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad

import           Data.Foldable
import           Data.List

import           Ansi
import           AnsiColor
import           Color
import           ColorScheme

roll :: (Int -> Color) -> String
roll mk = join [withBackground (mk v) "  " | v <- [0,2 .. 255]]

main :: IO ()
main = do
  putStrLn "Naive"
  putStrLn $ displayCS naiveCS
  traverse_
    (putStrLn . roll)
    [mkGrey, mkRed, mkGreen, mkBlue, mkCyan, mkMagenta, mkYellow, mkGrey]
  putStrLn "Contrast white"
  putStrLn $ displayCS contrastCS
