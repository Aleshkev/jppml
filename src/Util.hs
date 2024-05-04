module Util where

ansiDefault :: String
ansiDefault = "\ESC[0m"

ansiRed :: String
ansiRed = "\ESC[31m"

ansiGreen :: String
ansiGreen = "\ESC[32m"

ansiYellow :: String
ansiYellow = "\ESC[33m"

ansiBlue :: String
ansiBlue = "\ESC[34m"

ansiMagenta :: String
ansiMagenta = "\ESC[35m"

ansiCyan :: String
ansiCyan = "\ESC[36m"

ansiBrightWhite :: String
ansiBrightWhite = "\ESC[97m"

fromRight :: Either a b -> b
fromRight e = case e of
  Left _ -> error "fromRight"
  Right x -> x
