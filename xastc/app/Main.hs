module Main where

import qualified Xast (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Xast.someFunc
