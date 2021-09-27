{-# LANGUAGE TemplateHaskell #-}

import           Data.List

import           Lens.Micro
import           Lens.Micro.TH

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

data Color =
  Color
    { _red   :: Int
    , _green :: Int
    , _blue  :: Int
    }
  deriving (Eq, Show)

makeLenses ''Color

instance IsColor Color where
  setForeground c = colorSeq [38, 2, c ^. red, c ^. green, c ^. blue]
  setBackground c = colorSeq [48, 2, c ^. red, c ^. green, c ^. blue]

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

data ColorScheme =
  ColorScheme
    { _background :: Color
    , _colors     :: AnsiColor -> Color
    }

makeLenses ''ColorScheme

main :: IO ()
main = do
  putStrLn $ "24-bit yellow: " ++ sample (Color 255 255 0)
  putStrLn $ "magenta: " ++ sample (Normal Magenta)
  putStrLn $ "bright magenta: " ++ sample (Bright Magenta)
