{-# LANGUAGE TemplateHaskell #-}

import           Ansi
import           AnsiColor
import           Color
import           ColorScheme

main :: IO ()
main = do
  putStrLn $ sampleCS naiveCS
