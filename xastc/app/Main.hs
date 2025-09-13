{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Xast (parse)
import Data.Text (pack)

main :: IO ()
main = do
  let filePath = "test.xst"

  fileContent <- readFile filePath
  Xast.parse filePath $ pack fileContent
