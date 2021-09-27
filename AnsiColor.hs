module AnsiColor where

import           Ansi

data PrimColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Eq, Ord, Bounded, Enum)

data AnsiColor
  = Normal PrimColor
  | Bright PrimColor
  deriving (Eq, Ord)

instance IsColor AnsiColor where
  setForeground (Normal c) = colorSeq [fromEnum c + 30]
  setForeground (Bright c) = colorSeq [fromEnum c + 90]
  setBackground (Normal c) = colorSeq [fromEnum c + 40]
  setBackground (Bright c) = colorSeq [fromEnum c + 100]
