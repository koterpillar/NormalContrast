module Export.Herdr
  ( formatHerdr
  , exportHerdr
  ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as ByteString

import qualified Data.ByteString.Lazy     as LazyByteString

import           AnsiColor
import           Color
import           ColorScheme

encodeConfig :: Config
encodeConfig = defConfig {confCompare = compare, confTrailingNewline = True}

-- Herdr app theme: ~/.config/herdr-app/themes/<name>.json
formatHerdr :: ColorScheme -> ByteString
formatHerdr cs =
  LazyByteString.toStrict $
  encodePretty' encodeConfig $
  object
    [ "background" .= cHex (csBackground cs)
    , "foreground" .= cHex (csForeground cs)
    , "cursor" .= cHex (csCursor cs)
    , "selection" .= cHex (csColor cs (Bright White))
    , "ansi" .= map (cHex . csColor cs) ansiColors
    ]

exportHerdr :: ColorScheme -> IO ()
exportHerdr cs =
  ByteString.writeFile (csName cs ++ ".herdr.json") (formatHerdr cs)
