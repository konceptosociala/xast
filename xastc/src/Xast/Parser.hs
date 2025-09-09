{-# LANGUAGE OverloadedStrings #-}

module Xast.Parser 
   ( Parser
   , sc
   , lexeme
   , symbol
   ) where

import Text.Megaparsec (Parsec, empty)
import Data.Void (Void)
import Data.Text (Text)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1)

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space
   space1
   (L.skipLineComment "--")
   empty