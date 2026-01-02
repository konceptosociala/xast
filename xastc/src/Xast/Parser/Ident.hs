{-# LANGUAGE OverloadedStrings #-}

module Xast.Parser.Ident
   ( Ident(..)
   , typeIdent, fnIdent, genericIdent, varIdent, inferIdent
   , operator, opToFnIdent
   , reserved, builtin
   ) where

import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Text.Megaparsec.Char (upperChar, lowerChar, alphaNumChar)
import Text.Megaparsec
import Xast.Parser

newtype Ident = Ident { unIdent :: Text }
   deriving (Eq, Ord, Generic)

instance Show Ident where
   show :: Ident -> String
   show = unpack . unIdent

reserved :: [Text]
reserved = 
   -- Keywords
   [ "type", "fn", "let", "in", "if", "then", "else"
   , "match", "of", "and", "system", "with", "extern"
   , "event", "res"
   ]

builtin :: [Text]
builtin = 
   -- Functions
   [ "opAdd", "opSub", "opMul", "opDiv", "opMod"
   , "opPow", "opEq", "opNeq", "opAnd", "opOr"
   , "opPipe", "opConcat"
   ]

data Operator 
   -- Math
   = OpPlus    -- +
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
   -- Other
   | OpPipe    -- |>
   | OpConcat  -- <>
   deriving (Eq, Show)

operator :: Parser Operator
operator = choice
   [ OpPlus    <$ symbol "+"
   , OpMinus   <$ symbol "-"
   , OpMul     <$ symbol "*"
   , OpDiv     <$ symbol "/"
   , OpMod     <$ symbol "%"
   , OpPow     <$ symbol "**"
   , OpEq      <$ symbol "=="
   , OpNeq     <$ symbol "!="
   , OpAnd     <$ symbol "&&"
   , OpOr      <$ symbol "||"
   , OpPipe    <$ symbol "|>"
   , OpConcat  <$ symbol "<>"
   ]

opToFnIdent :: Operator -> Ident
opToFnIdent op = case op of
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
   OpPipe    -> Ident "opPipe"
   OpConcat  -> Ident "opConcat"

genericIdent :: Parser Ident
genericIdent = try $ lexeme $ do
   c <- lowerChar
   notFollowedBy alphaNumChar
   return $ Ident (pack [c])

typeIdent :: Parser Ident
typeIdent = try pascalCase

inferIdent :: Parser Ident
inferIdent = try $ Ident <$> symbol "_"

fnIdent :: Parser Ident
fnIdent = try $ do
   ident <- camelCase
   if unIdent ident `elem` reserved
      then fail ("keyword `" ++ unpack (unIdent ident) ++ "` is reserved")
      else return ident

varIdent :: Parser Ident
varIdent = try $ do
   ident <- camelCase
   if unIdent ident `elem` reserved
      then fail ("keyword `" ++ unpack (unIdent ident) ++ "` is reserved")
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