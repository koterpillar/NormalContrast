{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad

import           Data.List

import           Ansi
import           AnsiColor
import           Color
import           ColorScheme

main :: IO ()
main = do
  putStrLn $ join $ map (`withBackground` " ") allColorsByLuminance
  putStrLn $ tableCS naiveCS
