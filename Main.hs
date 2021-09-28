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
  putStrLn $ tableCS naiveCS
  putStrLn "Contrast white"
  putStrLn $ tableCS contrastCS
