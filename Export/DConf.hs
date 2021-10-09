{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Export.DConf where

import           Control.Monad

import           Data.List
import           Data.List.Split

import           Color

class DConf a where
  toDConf :: a -> String

instance {-# OVERLAPPABLE #-} DConf a => DConf [a] where
  toDConf = surround "[" "]" . intercalate ", " . map toDConf

instance DConf Bool where
  toDConf True  = "true"
  toDConf False = "false"

instance DConf String where
  toDConf str = "'" ++ str ++ "'"

instance DConf Color where
  toDConf Color {..} =
    "'rgb(" ++ show cRed ++ "," ++ show cGreen ++ "," ++ show cBlue ++ ")'"

dConfWrite :: DConf a => String -> a -> String
dConfWrite name value = command ["dconf", "write", name, toDConf value]

command :: [String] -> String
command = unwords . map escapeCommand

escapeCommand :: String -> String
escapeCommand = surround "'" "'" . join . map escapeChar
  where
    escapeChar '\'' = "'\"'\"'"
    escapeChar c    = [c]

dConfAppend :: DConf a => String -> a -> String
dConfAppend name value =
  command ["dconf", "write", name] ++
  " " ++
  surround
    "$("
    ")"
    (command ["dconf", "read", name] ++ " | " ++ command ["sed", "s/]/,/"]) ++
  escapeCommand (toDConf value) ++ escapeCommand "]"

surround :: String -> String -> String -> String
surround start end value = start ++ value ++ end
