{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Xast.Parser.Expr
   ( Expr(..), expr
   , Literal(..), literal
   , LetIn(..), letIn
   , stringLiteral
   ) where

import Data.Text (Text, pack)
import Xast.Parser.Ident (Ident, varIdent, typeIdent)
import Xast.Parser (Parser, lexeme, sc, symbol)
import Text.Megaparsec (choice, manyTill, between, sepBy, MonadParsec (try), some, sepBy1, optional)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (char)

type ModBind = Maybe Ident

data Expr
   = ExpVar ModBind Ident                 -- add, a
   | ExpCon ModBind Ident                 -- Nothing, Just
   | ExpLit Literal                       -- "abc", 12, ()
   | ExpLambda Lambda                     -- .\x y -> x + y
   | ExpApp Expr Expr                     -- Just 12, func a b
   | ExpLetIn LetIn                       -- let a = 1 and let b = 2 in ...
   | ExpIfThen IfThenElse                 -- if ... then ... else ...
   -- | ExpMatch Match                    -- match EXPR of 
   deriving (Eq, Show)

atomExpr :: Parser Expr
atomExpr = choice
   [ parens
   , ExpVar    <$> (optional . try $ typeIdent <* ".") <*> varIdent
   , ExpCon    <$> (optional . try $ typeIdent <* ".") <*> typeIdent
   , ExpLit    <$> literal
   , ExpLambda <$> lambda
   , ExpLetIn  <$> letIn
   , ExpIfThen <$> ifThenElse
   ]

parens :: Parser Expr
parens = between (symbol "(") (symbol ")") expr

expr :: Parser Expr
expr = do
   atoms <- some atomExpr
   pure (foldl1 ExpApp atoms)

-- data Match = Match deriving (Eq, Show)

data IfThenElse = IfThenElse
   { iteIf :: Expr
   , iteThen :: Expr
   , iteElse :: Expr
   }
   deriving (Eq, Show)

ifThenElse :: Parser IfThenElse
ifThenElse = do
   _        <- symbol "if"
   iteIf    <- expr
   _        <- symbol "then"
   iteThen  <- expr
   _        <- symbol "else"
   iteElse  <- expr

   return IfThenElse {..}

data Lambda = Lambda
   { lamArgs :: [Ident]
   , lamBody :: Expr
   }
   deriving (Eq, Show)

lambda :: Parser Lambda
lambda = do
   _        <- symbol ".\\"
   lamArgs  <- some varIdent
   _        <- symbol "->"
   lamBody  <- expr

   return Lambda {..}

data LetIn = LetIn
   { linBind :: [Let]
   , libExpr :: Expr
   }
   deriving (Eq, Show)

letIn :: Parser LetIn
letIn = do
   linBind <- let' `sepBy1` symbol "and"
   _       <- symbol "in"
   libExpr <- expr

   return LetIn {..}

data Let = Let
   { letIdent :: Ident
   , letValue :: Expr
   }
   deriving (Eq, Show)

let' :: Parser Let
let' = do
   _         <- symbol "let"
   letIdent  <- varIdent
   _         <- symbol "="
   letValue  <- expr

   return Let {..}

data Literal
   = LitString Text
   | LitChar Char
   | LitInt Int
   | LitFloat Float
   | LitList [Literal]
   | LitTuple [Literal]
   deriving (Eq, Show)

literal :: Parser Literal
literal = choice
   [ tupleOrParens
   , LitString <$> stringLiteral
   , LitChar   <$> charLiteral
   , LitFloat  <$> try signedFloat
   , LitInt    <$> signedInt
   , LitList   <$> between (symbol "[") (symbol "]") (literal `sepBy` symbol ",")
   ]

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