module Main where

import qualified MyLib (someFunc)
import MP3Parser

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
