{-# LANGUAGE OverloadedStrings #-}

module Xast.Parser
   ( Parser, Located(..), Location(..)
   , located
   , sc
   , lexeme
   , symbol
   , endOfStmt
   , (<->)
   ) where

import Text.Megaparsec (Parsec, empty, MonadParsec (observing, try), parseError, SourcePos, getSourcePos, getOffset)
import Data.Void (Void)
import Data.Text (Text)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1)
import Control.Monad (void)

data Located a = Located
   { lLocation :: Location
   , lNode     :: a
   }
   deriving (Eq, Show)

data Location = Location 
   { lPos :: SourcePos
   , lOffset   :: Int
   , lLength   :: Int
   }
   deriving (Eq, Show)

located :: Parser a -> Parser (Located a)
located p = do
  offset1 <- getOffset
  pos <- getSourcePos
  node <- p
  offset2 <- getOffset
  pure $ Located (Location pos offset1 (offset2 - offset1)) node

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
