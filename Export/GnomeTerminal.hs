module Export.GnomeTerminal
  ( formatGnomeTerminal
  , exportGnomeTerminal
  ) where

import qualified Codec.Binary.UTF8.String as UTF8

import           Data.Maybe
import           Data.UUID                (UUID)
import qualified Data.UUID                as UUID
import qualified Data.UUID.V5             as UUID

import           AnsiColor
import           ColorScheme
import           Export.DConf

profileUUID :: UUID
profileUUID = fromJust $ UUID.fromString "8fc7265c-c30f-4337-9c01-ebfcf346ba64"

csProfileName :: ColorScheme -> String
csProfileName =
  UUID.toString . UUID.generateNamed profileUUID . UTF8.encode . csName

formatGnomeTerminal :: ColorScheme -> String
formatGnomeTerminal cs =
  unlines
    [ "#!/bin/sh"
    , "set -e"
    , dConfAppend (profiles ++ "list") $ csProfileName cs
    , writeThemeProp "visible-name" $ csName cs
    , writeThemeProp "use-theme-colors" False
    , writeThemeProp "background-color" $ csBackground cs
    , writeThemeProp "foreground-color" $ csForeground cs
    , writeThemeProp "palette" $ map (csColor cs) ansiColors
    , writeThemeProp "cursor-colors-set" True
    , writeThemeProp "cursor-background-color" $ csCursor cs
    , writeThemeProp "cursor-foreground-color" $ csCursorText cs
    ]
  where
    writeThemeProp :: DConf a => String -> a -> String
    writeThemeProp name =
      dConfWrite $ profiles ++ ":" ++ csProfileName cs ++ "/" ++ name
    profiles = "/org/gnome/terminal/legacy/profiles:/"

exportGnomeTerminal :: ColorScheme -> IO ()
exportGnomeTerminal cs =
  writeFile (csName cs ++ ".dconf.sh") (formatGnomeTerminal cs)
