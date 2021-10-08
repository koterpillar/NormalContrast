{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Export.GnomeTerminal
  ( formatGnomeTerminal
  , exportGnomeTerminal
  ) where

import qualified Codec.Binary.UTF8.String as UTF8

import           Data.List
import           Data.Maybe
import           Data.UUID                (UUID)
import qualified Data.UUID                as UUID
import qualified Data.UUID.V5             as UUID

import           AnsiColor
import           Color
import           ColorScheme

profileUUID :: UUID
profileUUID = fromJust $ UUID.fromString "8fc7265c-c30f-4337-9c01-ebfcf346ba64"

csProfileName :: ColorScheme -> String
csProfileName =
  UUID.toString . UUID.generateNamed profileUUID . UTF8.encode . csName

class DConf a where
  formatDConf :: a -> String

instance {-# OVERLAPPABLE #-} DConf a => DConf [a] where
  formatDConf items = "[" ++ intercalate ", " (map formatDConf items) ++ "]"

instance DConf Bool where
  formatDConf True  = "true"
  formatDConf False = "false"

instance DConf String where
  formatDConf str = "'" ++ str ++ "'"

instance DConf Color where
  formatDConf Color {..} =
    "'rgb(" ++ show cRed ++ "," ++ show cGreen ++ "," ++ show cBlue ++ ")'"

dConfLine :: DConf a => String -> a -> String
dConfLine name value = name ++ "=" ++ formatDConf value

formatGnomeTerminal :: ColorScheme -> String
formatGnomeTerminal cs =
  unlines
    [ "# dconf load /org/gnome/terminal/ <"
    , "[legacy/profiles:/:" ++ csProfileName cs ++ "]"
    , dConfLine "visible-name" $ csName cs
    , dConfLine "use-theme-colors" False
    , dConfLine "background-color" $ csBackground cs
    , dConfLine "foreground-color" $ csForeground cs
    , dConfLine "palette" $ map (csColor cs) ansiColors
    , dConfLine "cursor-colors-set" True
    , dConfLine "cursor-background-color" $ csCursor cs
    , dConfLine "cursor-foreground-color" $ csCursorText cs
    ]

exportGnomeTerminal :: ColorScheme -> IO ()
exportGnomeTerminal cs =
  writeFile (csName cs ++ ".dconf") (formatGnomeTerminal cs)
