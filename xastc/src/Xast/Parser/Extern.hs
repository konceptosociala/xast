{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Xast.Parser.Extern
   ( ExternType(..), externType
   , ExternFunc(..), externFunc
   , Extern(..), extern
   ) where

import Xast.Parser.Type (Type, type')
import Xast.Parser.Ident
import Xast.Parser (endOfStmt, Parser, symbol, (<->), Located, located)
import Text.Megaparsec (sepBy, between, many)

data Extern = ExtFunc (Located ExternFunc) | ExtType (Located ExternType)
   deriving (Eq, Show)

extern :: Parser Extern
extern = (ExtFunc <$> externFunc) <-> (ExtType <$> externType)

data ExternFunc = ExternFunc
   { efnName :: Ident
   , efnArgs :: [Type]
   , efnRet :: Type
   }
   deriving (Eq, Show)

externFunc :: Parser (Located ExternFunc)
externFunc = located $ do
   _        <- symbol "extern"
   _        <- symbol "fn"
   efnName  <- fnIdent
   efnArgs  <- between (symbol "(") (symbol ")") (type' `sepBy` symbol ",")
   _        <- symbol "->"
   efnRet   <- type'
   _        <- endOfStmt

   return ExternFunc {..}

data ExternType = ExternType
   { etName :: Ident
   , etGenerics :: [Ident]
   }
   deriving (Eq, Show)

externType :: Parser (Located ExternType)
externType = located $ do
   _           <- symbol "extern"
   _           <- symbol "type"
   etName      <- typeIdent
   etGenerics  <- many genericIdent
   _           <- endOfStmt

   return ExternType {..}