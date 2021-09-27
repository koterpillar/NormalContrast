{-# LANGUAGE TemplateHaskell #-}

import           Ansi
import           AnsiColor
import           Color

main :: IO ()
main = do
  putStrLn $ "24-bit yellow: " ++ sample (Color 255 255 0)
  putStrLn $ "magenta: " ++ sample (Normal Magenta)
  putStrLn $ "bright magenta: " ++ sample (Bright Magenta)
