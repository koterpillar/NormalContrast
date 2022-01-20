import           Control.Monad

import           Data.Foldable

import           Ansi
import           Color
import           ColorScheme
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

main :: IO ()
main = do
  putStrLn $ displayCS naiveCS
  traverse_
    roll
    [mkGrey, mkRed, mkGreen, mkBlue, mkCyan, mkMagenta, mkYellow, mkGrey]
  putStrLn $ displayCS normalContrastCS
  export normalContrastCS
