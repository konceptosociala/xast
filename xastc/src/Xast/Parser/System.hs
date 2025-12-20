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
import Text.Megaparsec (between, sepBy, sepBy1, (<|>), many, some, (<?>), MonadParsec (lookAhead))
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
   , sysArgs :: [Type]
   , sysRet :: Type
   , sysWith :: Maybe [Type]
   }
   deriving (Eq, Show)

systemDef :: Parser (Located SystemDef)
systemDef = located $ do
   sysLabel <- (label <|> pure "default") <?> "system label"
   _        <- symbol "system"
   sysName  <- typeIdent
   sysArgs  <- between (symbol "(") (symbol ")") (type' `sepBy` symbol ",")
   _        <- symbol "->"
   sysRet   <- type'
   sysWith  <- optional with

   _        <- endOfStmt

   return SystemDef {..}

label :: Parser Text
label = symbol "@label" *> symbol "=" *> stringLiteral

with :: Parser [Type]
with = symbol "with" *> (event `sepBy1` symbol ",")
   where
      event :: Parser Type
      event = symbol "Event" *> type'

data SystemImpl = SystemImpl
   { sysImName :: Ident
   , sysImArgs :: [Pattern]
   , sysImWith :: Maybe [Pattern]
   , sysImBody :: Located Expr
   }
   deriving (Eq, Show)

systemImpl :: Parser (Located SystemImpl)
systemImpl = located $ do
   _           <- symbol "system"
   sysImName   <- typeIdent
   sysImArgs   <- many pattern
   sysImWith   <- optional $ symbol "with" *> some pattern
   _           <- symbol "="
   sysImBody   <- expr
   _           <- endOfStmt

   return SystemImpl {..}