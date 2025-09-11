{-# LANGUAGE OverloadedStrings #-}

module Xast.Parser.Expr
   ( Expr(..)
   , Literal(..), literal
   ) where

import Data.Text (Text, pack)
import Xast.Parser.Ident (Ident)
import Xast.Parser (Parser, lexeme, sc, symbol)
import Text.Megaparsec (choice, manyTill, between, sepBy, MonadParsec (try))
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (char)

data Expr = Expr
   deriving (Eq, Show)

data Literal
   = LitString Text
   | LitChar Char
   | LitInt Int
   | LitFloat Float
   | LitArray [Literal]
   | LitTuple [Literal]
   -- // TODO: Impl lambda
   -- | LitLambda Lambda
   deriving (Eq, Show)

literal :: Parser Literal
literal = choice
   [ tupleOrParens
   , LitString <$> stringLiteral
   , LitChar   <$> charLiteral
   , LitFloat  <$> try signedFloat
   , LitInt    <$> signedInt
   , LitArray  <$> between (symbol "[") (symbol "]") (literal `sepBy` symbol ",")
   ]

data Lambda = Lambda
   { lamArgs :: [Ident]
   , lamBody :: ()
   }
   deriving (Eq, Show)

tupleOrParens :: Parser Literal
tupleOrParens = between (symbol "(") (symbol ")") $ do
   ts <- literal `sepBy` symbol ","
   case ts of
      [] -> pure (LitTuple [])
      [t] -> pure t
      manyT -> pure (LitTuple manyT)

signedFloat :: Parser Float
signedFloat = L.signed sc floatLiteral

floatLiteral :: Parser Float
floatLiteral = lexeme L.float

signedInt :: Parser Int
signedInt = L.signed sc intLiteral

intLiteral :: Parser Int
intLiteral = lexeme L.decimal

charLiteral :: Parser Char
charLiteral = lexeme $ between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = pack <$> lexeme lit
   where lit = char '\"' *> manyTill L.charLiteral (char '\"')