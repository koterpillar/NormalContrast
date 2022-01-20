module Export
  ( export
  ) where

import           ColorScheme
import           Export.GnomeTerminal
import           Export.ITerm
import           Export.Kitty
import           Export.VSCode

export :: ColorScheme -> IO ()
export cs = do
  exportGnomeTerminal cs
  exportITerm cs
  exportVSCode cs
  exportKitty cs
