module Main where

import Interpreter (transTree)

main :: IO ()
main = do
  -- putStrLn "Hello, Haskell!"
  print (show $ transTree "let a = b")
