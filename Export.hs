{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Export
  ( module Export.ITerm
  , module Export.GnomeTerminal
  , export
  ) where

import           ColorScheme
import           Export.GnomeTerminal
import           Export.ITerm

export :: ColorScheme -> IO ()
export cs = do
  exportGnomeTerminal cs
  exportITerm cs
