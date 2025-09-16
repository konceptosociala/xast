{-# LANGUAGE OverloadedStrings #-}

module Xast.Parser.Ident
   ( Ident(..)
   , typeIdent
   , fnIdent
   , genericIdent
   , varIdent
   , operatorIdent
   ) where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Text.Megaparsec.Char (upperChar, lowerChar, alphaNumChar)
import Text.Megaparsec
import Xast.Parser

newtype Ident = Ident { unIdent :: Text }
   deriving (Eq, Ord, Generic)

instance Show Ident where
   show = show . unIdent

keywords :: [Text]
keywords = ["type", "fn", "let", "in", "if", "then", "else", "match", "of", "and"]

operatorIdent :: Parser Ident
operatorIdent = Ident . pack <$> some (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: String))

genericIdent :: Parser Ident
genericIdent = lexeme $ do
   c <- lowerChar
   return $ Ident (pack [c])

typeIdent :: Parser Ident
typeIdent = pascalCase

fnIdent :: Parser Ident
fnIdent = try $ do
   ident <- camelCase
   if unIdent ident `elem` keywords
      then fail "keyword cannot be used as identifier"
      else return ident

varIdent :: Parser Ident
varIdent = try $ do
   ident <- camelCase
   if unIdent ident `elem` keywords
      then fail "keyword cannot be used as identifier"
      else return ident

pascalCase :: Parser Ident
pascalCase = lexeme $ do
   first <- upperChar
   rest  <- many alphaNumChar
   return $ Ident (pack (first:rest))

camelCase :: Parser Ident
camelCase = lexeme $ do
   first <- lowerChar
   rest  <- many alphaNumChar
   return $ Ident (pack (first:rest))