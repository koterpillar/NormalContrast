module ColorScheme.Stock where

import           AnsiColor
import           Color
import           ColorScheme

stockDark :: String -> (AnsiColor -> Color) -> ColorScheme
stockDark csName csColor = ColorScheme {..}
  where
    csBackground = black
    csForeground = white
    csCursor = white
    csCursorText = black
    black = Color 0 0 0
    white = Color 255 255 255

-- via https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
windows :: ColorScheme
windows =
  stockDark "Windows" $ \case
    Normal Black   -> Color 12 12 12
    Normal Red     -> Color 197 15 31
    Normal Green   -> Color 19 161 14
    Normal Yellow  -> Color 193 156 0
    Normal Blue    -> Color 0 55 218
    Normal Magenta -> Color 136 23 152
    Normal Cyan    -> Color 58 150 221
    Normal White   -> Color 204 204 204
    Bright Black   -> Color 118 118 118
    Bright Red     -> Color 231 72 86
    Bright Green   -> Color 22 198 12
    Bright Yellow  -> Color 249 241 165
    Bright Blue    -> Color 59 120 255
    Bright Magenta -> Color 180 0 158
    Bright Cyan    -> Color 97 214 214
    Bright White   -> Color 242 242 242

-- https://github.com/GNOME/gnome-terminal/blob/51994fdb014e78f5ef2aeac07eed0f71ac30dd31/src/profile-editor.cc#L226
gnome :: ColorScheme
gnome =
  stockDark "GNOME" $ \case
    Normal Black   -> Color 0x17 0x14 0x21
    Normal Red     -> Color 0xc0 0x1c 0x28
    Normal Green   -> Color 0x26 0xa2 0x69
    Normal Yellow  -> Color 0xa2 0x73 0x4c
    Normal Blue    -> Color 0x12 0x48 0x8b
    Normal Magenta -> Color 0xa3 0x47 0xba
    Normal Cyan    -> Color 0x2a 0xa1 0xb3
    Normal White   -> Color 0xd0 0xcf 0xcc
    Bright Black   -> Color 0x5e 0x5c 0x64
    Bright Red     -> Color 0xf6 0x61 0x51
    Bright Green   -> Color 0x33 0xd1 0x7a
    Bright Yellow  -> Color 0xe9 0xad 0x0c
    Bright Blue    -> Color 0x2a 0x7b 0xde
    Bright Magenta -> Color 0xc0 0x61 0xcb
    Bright Cyan    -> Color 0x33 0xc7 0xde
    Bright White   -> Color 0xff 0xff 0xff
