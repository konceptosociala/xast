{-# LANGUAGE OverloadedStrings #-}

module Xast.Parser
   ( Parser
   , sc
   , lexeme
   , symbol
   , endOfStmt
   , (<->)
   ) where

import Text.Megaparsec (Parsec, empty, MonadParsec (observing, try), parseError)
import Data.Void (Void)
import Data.Text (Text)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1)
import Control.Monad (void)

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

endOfStmt :: Parser ()
endOfStmt = void $ symbol ";"

(<->) :: Parser a -> Parser a -> Parser a
pa <-> pb = do
   a <- observing $ try pa
   case a of
      Right r   -> return r
      Left errA -> do
         b <- observing pb
         case b of
            Right r   -> return r
            Left errB -> parseError (errA <> errB)
