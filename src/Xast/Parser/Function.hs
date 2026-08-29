{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Function where

import Text.Megaparsec (between, sepBy, many)

import Xast.Parser.Type (type')
import Xast.Parser.Ident (fnIdent)
import Xast.Parser.Expr (expr, atomPattern')
import Xast.Parser.Common
import Xast.AST
import Xast.Parser.Modifier (fnModifier)

func :: Parser (Func Parsed)
func = (FnDef <$> funcDef) <-> (FnImpl <$> funcImpl)

funcDef :: Parser (Located FuncDef)
funcDef = located $ do
   modifiers   <- many fnModifier
   _           <- symbol "fn"
   name        <- fnIdent
   args        <- between (symbol "(") (symbol ")") (type' `sepBy` symbol ",")
   _           <- symbol "->"
   retType     <- type'
   _           <- endOfStmt

   return FuncDef {..}

funcImpl :: Parser (Located (FuncImpl Parsed))
funcImpl = located $ do
   _        <- symbol "fn"
   name     <- fnIdent
   args     <- many atomPattern'
   _        <- symbol "="
   body     <- expr
   _        <- endOfStmt

   return FuncImpl {..}