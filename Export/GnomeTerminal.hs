{-# LANGUAGE RecordWildCards #-}

module Export.GnomeTerminal
  ( formatGnomeTerminal
  , exportGnomeTerminal
  ) where

import           Data.List

import           AnsiColor
import           Color
import           ColorScheme

{-
[legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9]
background-color='rgb(255,222,0)'
foreground-color='rgb(59,0,255)'
palette=['rgb(23,20,33)', 'rgb(209,120,126)', 'rgb(38,162,105)', 'rgb(162,115,76)', 'rgb(18,72,139)', 'rgb(163,71,186)', 'rgb(42,161,179)', 'rgb(208,207,204)', 'rgb(94,92,100)', 'rgb(246,97,81)', 'rgb(51,218,122)', 'rgb(233,173,12)', 'rgb(42,123,222)', 'rgb(192,97,203)', 'rgb(51,199,222)', 'rgb(255,255,255)']
use-theme-colors=false
-}
csProfileName :: ColorScheme -> String
csProfileName _ = "b1dcc9dd-5262-4d8d-a863-c897e6d979b9" -- FIXME

class DConf a where
  formatDConf :: a -> String

instance DConf Color where
  formatDConf Color {..} =
    "'rgb(" ++ show cRed ++ "," ++ show cGreen ++ "," ++ show cBlue ++ ")'"

instance DConf Bool where
  formatDConf True  = "true"
  formatDConf False = "false"

instance DConf a => DConf [a] where
  formatDConf items = "[" ++ intercalate ", " (map formatDConf items) ++ "]"

dConfLine :: DConf a => String -> a -> String
dConfLine name value = name ++ "=" ++ formatDConf value

formatGnomeTerminal :: ColorScheme -> String
formatGnomeTerminal cs =
  unlines
    [ "# dconf load /org/gnome/terminal/ <"
    , "[legacy/profiles:/:" ++ csProfileName cs ++ "]"
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
