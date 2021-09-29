{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad

import           Data.List

import           Ansi
import           AnsiColor
import           Color
import           ColorScheme

main :: IO ()
main = do
  putStrLn "Naive"
  putStrLn $ displayCS naiveCS
  putStrLn "Contrast white"
  putStrLn $ displayCS contrastCS
