{-# LANGUAGE TemplateHaskell #-}

module Color where

import           Lens.Micro
import           Lens.Micro.TH

import           Ansi

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
