{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Xast.Parser.Expr
   ( Expr(..), expr
   , Literal(..), literal
   , LetIn(..), letIn
   , stringLiteral
   , intLiteral
   , floatLiteral
   ) where

import Data.Text (Text, pack)
import Xast.Parser.Ident (Ident (Ident), varIdent, typeIdent, inferIdent)
import Xast.Parser (Parser, lexeme, symbol, Located(..), located)
import Text.Megaparsec (choice, manyTill, between, sepBy, MonadParsec (try), some, sepBy1, optional, getSourcePos, (<|>))
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (char)
import Control.Monad.Combinators.Expr

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

term :: Parser (Located Expr)
term = do
   atoms <- some atomExpr
   pure $ foldl1 app atoms
   where
      app l@(Located pos _) r =
         Located pos (ExpApp l r)

data BuiltinOp 
   -- Math
   = OpPlus    -- +
   | OpNeg     -- -
   | OpMinus   -- -
   | OpMul     -- *
   | OpDiv     -- /
   | OpMod     -- %
   | OpPow     -- **
   -- Logical
   | OpEq      -- ==
   | OpNeq     -- !=
   | OpAnd     -- &&
   | OpOr      -- ||   
   | OpNot     -- !
   | OpPipe    -- |>
   | OpConcat  -- <>
   deriving (Eq, Show)

opIdent :: BuiltinOp -> Ident
opIdent op = case op of
   OpPlus    -> Ident "opAdd"
   OpMinus   -> Ident "opSub"
   OpMul     -> Ident "opMul"
   OpDiv     -> Ident "opDiv"
   OpMod     -> Ident "opMod"
   OpPow     -> Ident "opPow"
   OpEq      -> Ident "opEq"
   OpNeq     -> Ident "opNeq"
   OpAnd     -> Ident "opAnd"
   OpOr      -> Ident "opOr"
   OpNot     -> Ident "opNot"
   OpPipe    -> Ident "opPipe"
   OpConcat  -> Ident "opConcat"
   OpNeg     -> Ident "opNeg"

opVar :: BuiltinOp -> Expr
opVar = ExpVar Nothing . opIdent

opToken :: BuiltinOp -> Text
opToken op = case op of
   OpPlus    -> "+"
   OpMinus   -> "-"
   OpNeg     -> "-"
   OpMul     -> "*"
   OpDiv     -> "/"
   OpMod     -> "%"
   OpPow     -> "**"
   OpEq      -> "=="
   OpNeq     -> "!="
   OpAnd     -> "&&"
   OpOr      -> "||"
   OpNot     -> "!"
   OpPipe    -> "|>"
   OpConcat  -> "<>"

binOp :: BuiltinOp -> Located Expr -> Located Expr -> Located Expr
binOp op a@(Located pos _) b = 
   Located pos (ExpApp (Located pos (ExpApp (Located pos (opVar op)) a)) b)

table :: [[Operator Parser (Located Expr)]]
table =
   [  [ Prefix (unary OpNot)
      , Prefix (unary OpMinus)
      ]

   ,  [ InfixR (binary OpPow) ]

   ,  [ InfixL (binary OpMul)
      , InfixL (binary OpDiv)
      , InfixL (binary OpMod)
      ]

   ,  [ InfixL (binary OpPlus)
      , InfixL (binary OpMinus)
      ]

   ,  [ InfixN (binary OpEq)
      , InfixN (binary OpNeq)
      ]

   ,  [ InfixR (binary OpAnd) ]
   ,  [ InfixR (binary OpOr) ]

   ,  [ InfixL (binary OpPipe) ]
   ,  [ InfixL (binary OpConcat) ]
   ]

binary :: BuiltinOp -> Parser (Located Expr -> Located Expr -> Located Expr)
binary op = do
  _ <- symbol (opToken op)
  pure (binOp op)

unary :: BuiltinOp -> Parser (Located Expr -> Located Expr)
unary op = do
  _ <- symbol (opToken op)
  pos <- getSourcePos
  pure $ \x -> binOp op (Located pos (ExpLit (LitInt 0))) x

expr :: Parser (Located Expr)
expr = makeExprParser term table

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
   , LitFloat  <$> try floatLiteral
   , LitInt    <$> intLiteral
   , LitList   <$> between (symbol "[") (symbol "]") (literal `sepBy` symbol ",")
   ]

tupleOrParensLit :: Parser Literal
tupleOrParensLit = between (symbol "(") (symbol ")") $ do
   ts <- literal `sepBy` symbol ","
   case ts of
      [] -> pure (LitTuple [])
      [t] -> pure t
      manyT -> pure (LitTuple manyT)

floatLiteral :: Parser Float
floatLiteral = lexeme L.float

intLiteral :: Parser Int
intLiteral = lexeme L.decimal

charLiteral :: Parser Char
charLiteral = lexeme $ between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = pack <$> lexeme lit
   where lit = char '\"' *> manyTill L.charLiteral (char '\"')