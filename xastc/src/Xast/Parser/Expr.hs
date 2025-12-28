{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Xast.Parser.Expr
   ( Expr(..), expr
   , Literal(..), literal
   , LetIn(..), letIn
   , stringLiteral
   ) where

import Data.Text (Text, pack)
import Xast.Parser.Ident (Ident, varIdent, typeIdent, inferIdent)
import Xast.Parser (Parser, lexeme, sc, symbol, Located(..), located)
import Text.Megaparsec (choice, manyTill, between, sepBy, MonadParsec (try), some, sepBy1, optional, getSourcePos, (<|>))
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (char)

type ModBind = Maybe Ident

data Expr
   = ExpVar ModBind Ident                 -- add, a
   | ExpCon ModBind Ident                 -- Nothing, Just
   | ExpTuple [Located Expr]              -- (pos, Event (p, pos));
   | ExpList [Located Expr]               -- [a, 12, b, c]
   | ExpLit Literal                       -- "abc", 12, ()
   | ExpLambda Lambda                     -- .\x y -> x + y
   | ExpApp (Located Expr) (Located Expr) -- Just 12, func a b
   | ExpLetIn LetIn                       -- let a = 1 and let b = 2 in ...
   | ExpIfThen IfThenElse                 -- if ... then ... else ...
   -- | ExpMatch Match                    -- match EXPR of 
   deriving (Eq, Show)

atomExpr :: Parser (Located Expr)
atomExpr = located $ choice
   [ tupleOrParens
   , ExpVar    <$> (optional . try $ typeIdent <* ".") <*> varIdent
   , ExpCon    <$> (optional . try $ typeIdent <* ".") <*> typeIdent
   , ExpList   <$> between (symbol "[") (symbol "]") (expr `sepBy` symbol ",")
   , ExpLit    <$> literal
   , ExpLambda <$> lambda
   , ExpLetIn  <$> letIn
   , ExpIfThen <$> ifThenElse
   ]

tupleOrParens :: Parser Expr
tupleOrParens = between (symbol "(") (symbol ")") $ do
   ts <- expr `sepBy` symbol ","
   case ts of
      [] -> pure (ExpTuple [])
      [Located _ t] -> pure t
      manyT -> pure (ExpTuple manyT)

expr :: Parser (Located Expr)
expr = do
   atoms <- some atomExpr
   pos <- getSourcePos
   return $ foldl1 (\l r -> Located pos (ExpApp l r)) atoms

-- data Match = Match deriving (Eq, Show)

data IfThenElse = IfThenElse
   { iteIf :: Located Expr
   , iteThen :: Located Expr
   , iteElse :: Located Expr
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
   , lamBody :: Located Expr
   }
   deriving (Eq, Show)

lambda :: Parser Lambda
lambda = do
   _        <- symbol ".\\"
   lamArgs  <- some (varIdent <|> inferIdent)
   _        <- symbol "->"
   lamBody  <- expr

   return Lambda {..}

data LetIn = LetIn
   { linBind :: [Located Let]
   , linExpr :: Located Expr
   }
   deriving (Eq, Show)

letIn :: Parser LetIn
letIn = do
   linBind <- let' `sepBy1` symbol "and"
   _       <- symbol "in"
   linExpr <- expr

   return LetIn {..}

data Let = Let
   { letIdent :: Ident
   , letValue :: Located Expr
   }
   deriving (Eq, Show)

let' :: Parser (Located Let)
let' = located $ do
   _         <- symbol "let"
   letIdent  <- varIdent <|> inferIdent
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
   [ tupleOrParensLit
   , LitString <$> stringLiteral
   , LitChar   <$> charLiteral
   , LitFloat  <$> try signedFloat
   , LitInt    <$> signedInt
   , LitList   <$> between (symbol "[") (symbol "]") (literal `sepBy` symbol ",")
   ]

tupleOrParensLit :: Parser Literal
tupleOrParensLit = between (symbol "(") (symbol ")") $ do
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