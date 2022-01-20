module Export.ITerm
  ( formatITerm
  , exportITerm
  ) where

import           Control.Monad

import qualified Data.Map       as Map

import           Data.String    (fromString)
import qualified Data.Text      as Text
import qualified Data.Text.Lazy

import           Text.XML       hiding (writeFile)

import           AnsiColor
import           Color
import           ColorScheme

mkText :: String -> Node
mkText = NodeContent . Text.pack

mkElem :: String -> Node -> Element
mkElem name contents = Element (fromString name) Map.empty [contents]

colorDict :: Color -> Element
colorDict c = plistDict $ zipWith mk ["Red", "Green", "Blue"] (cComponents c)
  where
    mk n v = (n ++ " Component", plistReal $ (fromIntegral v :: Double) / 255)

plistTop :: Element -> Element
plistTop el =
  Element "plist" (Map.fromList [("version", "1.0")]) [NodeElement el]

plistReal :: Double -> Element
plistReal = mkElem "real" . mkText . show

plistDocument :: Element -> Document
plistDocument el =
  Document
    (Prologue
       []
       (Just
          (Doctype
             "plist"
             (Just
                (PublicID
                   "-//Apple//DTD PLIST 1.0//EN"
                   "http://www.apple.com/DTDs/PropertyList-1.0.dtd"))))
       [])
    el
    []

plistDict :: [(String, Element)] -> Element
plistDict =
  Element "dict" Map.empty .
  map NodeElement . join . map (\(k, v) -> [mkElem "key" $ mkText k, v])

formatITerm :: ColorScheme -> String
formatITerm cs =
  Data.Text.Lazy.unpack $
  renderText def $
  plistDocument $
  plistTop $
  plistDict $
  map mk ansiColors ++
  [ ("Background Color", colorDict $ csBackground cs)
  , ("Foreground Color", colorDict $ csForeground cs)
  , ("Bold Color", colorDict $ csForeground cs)
  , ("Cursor Color", colorDict $ csCursor cs)
  , ("Cursor Text Color", colorDict $ csCursorText cs)
  ]
  where
    mk c = ("Ansi " ++ show (fromEnum c) ++ " Color", colorDict $ csColor cs c)

exportITerm :: ColorScheme -> IO ()
exportITerm cs = writeFile (csName cs ++ ".itermcolors") (formatITerm cs)
