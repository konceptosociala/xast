{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Common where

import Text.Megaparsec
import Data.Text (Text)
import Data.Void (Void)
import Xast.AST (Located(Located), Location (..))
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1)
import Control.Monad (void)

type Parser = Parsec Void Text

located :: Parser a -> Parser (Located a)
located p = do
   offset1 <- getOffset
   pos <- getSourcePos
   node <- p
   offset2 <- getOffset
   pure $ Located (Location pos offset1 (offset2 - offset1)) node

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