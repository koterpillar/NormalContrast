{-# OPTIONS_GHC -fno-warn-orphans #-}

module Export.VSCode where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as ByteString

import qualified Data.ByteString.Lazy     as LazyByteString

import           Data.String              (fromString)

import           AnsiColor
import           Color
import           ColorScheme

encodeConfig :: Config
encodeConfig = defConfig {confCompare = compare, confTrailingNewline = True}

formatVSCode :: ColorScheme -> ByteString
formatVSCode = LazyByteString.toStrict . encodePretty' encodeConfig

instance ToJSON Color where
  toJSON = toJSON . cHex

-- https://code.visualstudio.com/api/references/theme-color#integrated-terminal-colors
instance ToJSON ColorScheme where
  toJSON ColorScheme {..} =
    object
      $ [ "terminal.background" .= csBackground
        , "terminal.foreground" .= csForeground
        , "terminalCursor.background" .= csCursorText
        , "terminalCursor.foreground" .= csCursor
        ]
          ++ [("terminal." <> ansiColorKey c) .= csColor c | c <- ansiColors]
    where
      ansiColorKey :: AnsiColor -> Key
      ansiColorKey (Normal c) = "ansi" <> fromString (show c)
      ansiColorKey (Bright c) = "ansiBright" <> fromString (show c)

exportVSCode :: ColorScheme -> IO ()
exportVSCode cs =
  ByteString.writeFile (csName cs ++ ".vscode.json") (formatVSCode cs)
