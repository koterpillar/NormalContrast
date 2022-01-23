import           Control.Monad

import           Data.Foldable
import           Data.List

import           Options.Applicative

import           Ansi
import           Color
import           ColorScheme
import           ColorScheme.Display
import           ColorScheme.NormalContrast
import qualified ColorScheme.Stock          as Stock
import           Export

roll :: (Int -> Color) -> IO ()
roll mk = do
  twidth <- terminalWidth
  let lineLength = twidth - 1
  let line =
        join
          [ withBackground (mk $ v * colorArgMax `div` lineLength) " "
          | v <- [0 .. lineLength]
          ]
  putStrLn line

choiceReader :: (a -> String) -> [a] -> ReadM a
choiceReader getName items =
  eitherReader $ \name ->
    case find ((==) name . getName) items of
      Just cs -> Right cs
      Nothing ->
        Left $
        "Invalid choice: " ++
        name ++ " (available: " ++ intercalate ", " (map getName items) ++ ")"

readCS :: ReadM ColorScheme
readCS =
  choiceReader
    csName
    [Stock.gnome, Stock.macOS, Stock.windows, normalContrastCS]

csOpt :: Parser ColorScheme
csOpt = option readCS (long "color-scheme" <> help "Color scheme")

mainParser :: Parser (IO ())
mainParser =
  subparser
    (command "export" (info (export <$> csOpt) idm) <>
     command "roll" (info (pure rolls) idm) <>
     command "display" (info (display <$> csOpt) idm))

rolls :: IO ()
rolls =
  traverse_
    roll
    [mkGrey, mkRed, mkGreen, mkBlue, mkCyan, mkMagenta, mkYellow, mkGrey]

display :: ColorScheme -> IO ()
display = putStrLn . displayCS

main :: IO ()
main = join $ execParser $ info mainParser idm
