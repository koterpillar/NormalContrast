module Ansi
  ( IsColor(..)
  , colorSeq
  , sample
  ) where

import           Data.List

class IsColor a where
  setForeground :: a -> String
  setBackground :: a -> String

esc :: String
esc = "\x1b["

colorSeq :: [Int] -> String
colorSeq params = esc ++ intercalate ";" (map show params) ++ "m"

reset :: String
reset = colorSeq [0]

sample :: IsColor a => a -> String
sample c =
  setBackground c ++
  "background" ++ reset ++ " " ++ setForeground c ++ "foreground" ++ reset
