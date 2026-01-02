{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Xast.Parser.System
   ( SystemDef(..), systemDef
   , SystemImpl(..), systemImpl
   , System(..), system
   ) where

import Xast.Parser
import Xast.Parser.Ident (Ident, typeIdent)
import Xast.Parser.Type (Type, type')
import Xast.Parser.Function (Pattern, pattern)
import Xast.Parser.Expr (stringLiteral, Expr, expr)
import Data.Text (Text)
import Control.Applicative (optional)
import Text.Megaparsec (between, sepBy1, (<|>), many, some, (<?>), MonadParsec (lookAhead), choice)
import Data.Maybe (isJust)

data System = SysDef (Located SystemDef) | SysImpl (Located SystemImpl)
   deriving (Eq, Show)

system :: Parser System
system = do
   hasLabel <- lookAhead (optional (symbol "@label"))
   if isJust hasLabel
      then SysDef <$> systemDef
      else (SysDef <$> systemDef) <-> (SysImpl <$> systemImpl)

data SystemDef = SystemDef
   { sysLabel :: Text
   , sysName :: Ident
   , sysEnts :: [QueriedEntity]
   , sysRet :: Type
   , sysWith :: Maybe [WithType]
   }
   deriving (Eq, Show)

systemDef :: Parser (Located SystemDef)
systemDef = located $ do
   sysLabel <- (label <|> pure "default") <?> "system label"
   _        <- symbol "system"
   sysName  <- typeIdent
   sysEnts  <- many queriedEntity
   _        <- symbol "->"
   sysRet   <- type'
   sysWith  <- optional with

   _        <- endOfStmt

   return SystemDef {..}

newtype QueriedEntity = QueriedEntity [Type]
   deriving (Eq, Show)

queriedEntity :: Parser QueriedEntity
queriedEntity = QueriedEntity <$> 
   between (symbol "#(") (symbol ")") (type' `sepBy1` symbol ",")

label :: Parser Text
label = symbol "@label" *> symbol "=" *> stringLiteral

data WithType
   = WithEvent Type
   | WithRes Type
   deriving (Eq, Show)

with :: Parser [WithType]
with = symbol "with" *> (withType `sepBy1` symbol ",")
   where
      withType :: Parser WithType
      withType = choice
         [ WithEvent <$ symbol "event" <* symbol ":" <*> type'
         , WithRes   <$ symbol "res" <* symbol ":" <*> type'
         ]

data SystemImpl = SystemImpl
   { sysImName :: Ident
   , sysImEnts :: [EntityPattern]
   , sysImWith :: Maybe [Pattern]
   , sysImBody :: Located Expr
   }
   deriving (Eq, Show)

systemImpl :: Parser (Located SystemImpl)
systemImpl = located $ do
   _           <- symbol "system"
   sysImName   <- typeIdent
   sysImEnts   <- many entityPattern
   sysImWith   <- optional $ symbol "with" *> some pattern
   _           <- symbol "="
   sysImBody   <- expr
   _           <- endOfStmt

   return SystemImpl {..}

entityPattern :: Parser EntityPattern
entityPattern = between (symbol "#(") (symbol ")") $ 
   EntityPattern <$> some pattern

newtype EntityPattern = EntityPattern [Pattern]
   deriving (Eq, Show)