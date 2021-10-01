{-# LANGUAGE RecordWildCards #-}
module Export (formatITerm , exportITerm) where

import Control.Monad

import Text.XML.Light

import Color
import ColorScheme
import AnsiColor

mkText :: String -> Content
mkText t = Text $ CData CDataText t Nothing

colorDict :: Color -> Element
colorDict Color{..} = plistDict $ map mk [("Red", cRed), ("Green", cGreen), ("Blue", cBlue)]
  where
    mk (n, v) = (n ++ " Component", plistReal $ (fromIntegral v :: Double) / 255)

plistTop :: Node t => t -> Element
plistTop el = add_attr (Attr (QName "version" Nothing Nothing) "1.0") $ unode "plist" el

plistReal :: Double -> Element
plistReal = unode "real" . mkText . show

plistDict :: [(String, Element)] -> Element
plistDict = unode "dict" . join . map (\(k, v) -> [unode "key" $ mkText k , v])

formatITerm :: ColorScheme -> String
formatITerm cs = ppTopElement $ plistTop $ plistDict $ map mk ansiColors
  where
    mk c = ("Ansi " ++ show (fromEnum c) ++ " Color", colorDict $ csColor cs c)

exportITerm :: ColorScheme -> IO ()
exportITerm cs = writeFile (csName cs ++ ".itermcolors") (formatITerm cs)
