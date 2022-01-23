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
  deriving (Eq, Ord, Bounded, Enum, Show)

primColors :: [PrimColor]
primColors = [minBound .. maxBound]

primColorCount :: Int
primColorCount = fromEnum (maxBound :: PrimColor) + 1

primColorSym :: PrimColor -> Char
primColorSym Black = 'K'
primColorSym c     = head $ show c

data AnsiColor
  = Normal PrimColor
  | Bright PrimColor
  deriving (Eq, Ord, Show)

instance Bounded AnsiColor where
  minBound = Normal minBound
  maxBound = Bright maxBound

instance Enum AnsiColor where
  fromEnum (Normal c) = fromEnum c
  fromEnum (Bright c) = primColorCount + fromEnum c
  toEnum i
    | i < primColorCount = Normal $ toEnum i
    | otherwise = Bright $ toEnum $ i - primColorCount

ansiColors :: [AnsiColor]
ansiColors = [minBound .. maxBound]

instance IsColor AnsiColor where
  setForeground (Normal c) = colorSeq [fromEnum c + 30]
  setForeground (Bright c) = colorSeq [fromEnum c + 90]
  setBackground (Normal c) = colorSeq [fromEnum c + 40]
  setBackground (Bright c) = colorSeq [fromEnum c + 100]
