{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.System where

import Control.Applicative (optional)
import Data.Maybe (isJust)
import Text.Megaparsec (between, sepBy1, many, some, MonadParsec (lookAhead), choice, sepEndBy1)

import Xast.Parser.Common
import Xast.Parser.Ident (typeIdent)
import Xast.Parser.Type (type')
import Xast.Parser.Expr (expr, atomPattern')
import Xast.AST
import Xast.Parser.Modifier (sysModifier)

system :: Parser (System Parsed)
system = do
   hasLabel <- lookAhead (optional (symbol "@label"))
   if isJust hasLabel
      then SysDef <$> systemDef
      else (SysDef <$> systemDef) <-> (SysImpl <$> systemImpl)

systemDef :: Parser (Located SystemDef)
systemDef = located $ do
   modifiers   <- many sysModifier
   _           <- symbol "system"
   name        <- typeIdent
   entities    <- many queriedEntity
   _           <- symbol "->"
   retType     <- type'
   with        <- optional with'

   _        <- endOfStmt

   return SystemDef {..}

queriedEntity :: Parser QueriedEntity
queriedEntity = QueriedEntity <$> 
   between (symbol "#(") (symbol ")") (type' `sepEndBy1` symbol ",")

with' :: Parser [WithType]
with' = symbol "with" *> (withType `sepBy1` symbol ",")
   where
      withType :: Parser WithType
      withType = choice
         [ WithEvent <$ symbol "event" <* symbol ":" <*> type'
         , WithRes   <$ symbol "res" <* symbol ":" <*> type'
         ]

systemImpl :: Parser (Located (SystemImpl Parsed))
systemImpl = located $ do
   _           <- symbol "system"
   name        <- typeIdent
   entities    <- many entityPattern
   with        <- optional $ symbol "with" *> some atomPattern'
   _           <- symbol "="
   body        <- expr
   _           <- endOfStmt

   return SystemImpl {..}

entityPattern :: Parser (EntityPattern Parsed)
entityPattern = between (symbol "#(") (symbol ")") $
   EntityPattern <$> some entPatBinding

entPatBinding :: Parser (EntPatBinding Parsed)
entPatBinding = (`EntPatBinding` AccessRead) <$> atomPattern'