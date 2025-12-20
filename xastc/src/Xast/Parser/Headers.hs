{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Xast.Parser.Headers
   ( ImportDef(..), importDef
   , ModuleDef(..), moduleDef
   , Module(..), module'
   ) where

import Xast.Parser.Ident (Ident, typeIdent, fnIdent)
import Xast.Parser (Parser, symbol, Located, located)
import Text.Megaparsec (sepBy1, between, (<|>), choice)

newtype Module = Module [Ident]
   deriving Eq

instance Show Module where
   show (Module []) = undefined
   show (Module [x]) = show x
   show (Module (x:xs)) = show x ++ "." ++ show (Module xs)

module' :: Parser Module
module' = Module <$> typeIdent `sepBy1` "."

data ModuleDef = ModuleDef
   { mdName :: Module
   , mdExport :: [Ident]
   }
   deriving (Eq, Show)

moduleDef :: Parser (Located ModuleDef)
moduleDef = located $ do
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

importDef :: Parser (Located ImportDef)
importDef = located $ do
   _           <- symbol "use"
   imdMod      <- module'
   imdPayload  <- importPayload

   return ImportDef {..}

data ImportPayload
   = ImpAlias Ident
   | ImpSelect [Ident]
   | ImpFull
   deriving (Eq, Show)

importPayload :: Parser ImportPayload
importPayload = choice
   [ ImpAlias   <$ symbol "as" <*> typeIdent
   , ImpSelect  <$> between (symbol "{") (symbol "}") (importIdent `sepBy1` symbol ",")
   , ImpFull    <$ symbol "*"
   ]

importIdent :: Parser Ident
importIdent = typeIdent <|> fnIdent
