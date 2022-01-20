module Export.Kitty where

import           Data.Text    (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import           AnsiColor
import           Color
import           ColorScheme

-- https://sw.kovidgoyal.net/kitty/conf/#color-scheme
-- https://sw.kovidgoyal.net/kitty/conf/#cursor-customization
formatKitty :: ColorScheme -> Text
formatKitty cs =
  Text.unlines [Text.unwords [k, Text.pack $ cHex v] | (k, v) <- items]
  where
    items = [cursorColor, cursorTextColor, foreground, background] ++ colors
    cursorColor = ("cursor", csCursor cs)
    cursorTextColor = ("cursor_text_color", csCursorText cs)
    foreground = ("foreground", csForeground cs)
    background = ("background", csBackground cs)
    colors =
      [ ("color" <> Text.pack (show $ fromEnum c), csColor cs c)
      | c <- ansiColors
      ]

exportKitty :: ColorScheme -> IO ()
exportKitty cs = Text.writeFile (csName cs ++ ".kitty.conf") (formatKitty cs)
