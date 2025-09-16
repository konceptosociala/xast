{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Xast.Parser.Headers
   ( ImportDef(..), importDef
   , ModuleDef(..), moduleDef
   , Module(..), module'
   ) where

import Xast.Parser.Ident (Ident, typeIdent, fnIdent)
import Xast.Parser (Parser, symbol)
import Text.Megaparsec (sepBy1, between, (<|>), choice)

newtype Module = Module [Ident]
   deriving (Eq, Show)

module' :: Parser Module
module' = Module <$> typeIdent `sepBy1` "."

data ModuleDef = ModuleDef
   { mdName :: Module
   , mdExport :: [Ident]
   }
   deriving (Eq, Show)

moduleDef :: Parser ModuleDef
moduleDef = do
   _        <- symbol "module"
   mdName   <- module'
   _        <- symbol "exports"
   mdExport <- between (symbol "{") (symbol "}") ((typeIdent <|> fnIdent) `sepBy1` symbol ",")

   return ModuleDef {..}

data ImportDef = ImportDef
   { imdMod :: Module
   , imdPayload :: ImportPayload
   }
   deriving (Eq, Show)

importDef :: Parser ImportDef
importDef = do
   _           <- symbol "use"
   imdMod      <- module'
   imdPayload  <- importPayload

   return ImportDef {..}

data ImportPayload
   = IpAlias Ident
   | IpSelect [Ident]
   | IpFull
   deriving (Eq, Show)

importPayload :: Parser ImportPayload
importPayload = choice
   [ IpAlias   <$ symbol "as" <*> typeIdent
   , IpSelect  <$> between (symbol "{") (symbol "}") (importIdent `sepBy1` symbol ",")
   , IpFull    <$ symbol "*"
   ]

importIdent :: Parser Ident
importIdent = typeIdent <|> fnIdent
