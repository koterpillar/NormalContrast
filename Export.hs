{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Export
  ( module Export.ITerm
  , export
  ) where

import           ColorScheme
import           Export.ITerm

export :: ColorScheme -> IO ()
export cs = do
  exportITerm cs
