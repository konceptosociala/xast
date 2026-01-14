{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Xast.Parser.Headers
   ( ImportDef(..), importDef
   , ModuleDef(..), moduleDef
   , Module(..), module'
   , moduleToPath 
   , ImportIntersection(..), intersectImport
   ) where

import Xast.Parser.Ident (Ident (Ident), typeIdent, fnIdent)
import Xast.Parser (Parser, symbol, Located (Located), located)
import Text.Megaparsec (sepBy1, between, (<|>), choice)
import Data.Text (unpack)

newtype Module = Module [Ident]
   deriving (Eq, Ord)

instance Show Module where
   show :: Module -> String
   show (Module []) = undefined
   show (Module [x]) = show x
   show (Module (x:xs)) = show x ++ "." ++ show (Module xs)

moduleToPath :: Module -> String
moduleToPath (Module ids) = "src/" ++ concatMap (\(Ident t) -> unpack t ++ "/") (init ids) ++ unpack (let Ident t = last ids in t) ++ ".xst"

module' :: Parser Module
module' = Module <$> typeIdent `sepBy1` "."

data ModuleDef = ModuleDef
   { mdName :: Module
   , mdExport :: ExportPayload
   }
   deriving (Eq, Show)

moduleDef :: Parser (Located ModuleDef)
moduleDef = located $ do
   _        <- symbol "module"
   mdName   <- module'
   _        <- symbol "exports"
   mdExport <- exportPayload

   return ModuleDef {..}

data ExportPayload
   = ExpFull
   | ExpSelect [Ident]
   deriving (Eq, Show)

exportPayload :: Parser ExportPayload
exportPayload = choice
   [ ExpSelect <$> between (symbol "{") (symbol "}") ((typeIdent <|> fnIdent) `sepBy1` symbol ",")
   , ExpFull   <$ symbol "*"
   ]

data ImportDef = ImportDef
   { imdMod :: Module
   , imdPayload :: ImportPayload
   }
   deriving (Eq, Show, Ord)

importDef :: Parser (Located ImportDef)
importDef = located $ do
   _           <- symbol "use"
   imdMod      <- module'
   imdPayload  <- importPayload

   return ImportDef {..}

data ImportPayload
   = ImpAlias (Located Ident)
   | ImpSelect [Located Ident]
   | ImpFull
   deriving (Eq, Show, Ord)

data ImportIntersection
   = InterModule (Located Module)
   | InterSelect Module [Located Ident]
   deriving (Eq, Show, Ord)

intersectIdents :: [Located Ident] -> [Located Ident] -> [Located Ident]
intersectIdents as bs = [b | b@(Located _ bi) <- bs, any (\(Located _ ai) -> ai == bi) as]

intersectImport
   :: Located ImportDef
   -> Located ImportDef
   -> Maybe ImportIntersection
intersectImport
   (Located locA (ImportDef moduleA impA))
   (Located locB (ImportDef moduleB impB)) =
      if moduleA == moduleB then
         case (impA, impB) of
            (ImpFull, _) ->
               Just (InterModule (Located locB moduleB))

            (_, ImpFull) ->
               Just (InterModule (Located locA moduleA))

            (ImpSelect as, ImpSelect bs) ->
               case intersectIdents as bs of
                  [] ->
                     Nothing
                  others ->
                     Just (InterSelect moduleB others)

            _ -> Nothing
      else
         Nothing

importPayload :: Parser ImportPayload
importPayload = choice
   [ ImpAlias   <$ symbol "as" <*> located typeIdent
   , ImpSelect  <$> between (symbol "{") (symbol "}") (located importIdent `sepBy1` symbol ",")
   , ImpFull    <$ symbol "*"
   ]

importIdent :: Parser Ident
importIdent = typeIdent <|> fnIdent
