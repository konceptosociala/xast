{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast (parse) where

import Data.Text (Text, unpack)
import Text.Megaparsec (runParser, MonadParsec (eof, lookAhead), errorBundlePretty, some, many, (<|>))
import Xast.Parser.Type (TypeDef, typeDef)
import Xast.Parser.Function (Func, func)
import Xast.Parser (Parser, sc, symbol)
import Xast.Parser.Headers (ModuleDef, ImportDef, moduleDef, importDef)
import Xast.Parser.Expr (stringLiteral)
import Xast.Parser.System (System, system)
import Xast.Parser.Extern

data Program = Program
   { progMode :: Mode
   , progModuleDef :: ModuleDef
   , progImports :: [ImportDef]
   , progStmts :: [Stmt]
   }
   deriving (Eq, Show)

program :: Parser Program
program = do
   progMode       <- mode <|> pure MStrict
   progModuleDef  <- moduleDef
   progImports    <- many importDef
   progStmts      <- some stmt

   return Program {..}

data Mode = MStrict | MSafe | MDynamic
   deriving (Eq, Show)

mode :: Parser Mode
mode = do
   str <- symbol "@mode" *> symbol "=" *> stringLiteral

   case str of
      "strict"  -> return MStrict
      "safe"    -> return MSafe
      "dynamic" -> return MDynamic
      other     -> fail ("Invalid mode `" ++ unpack other ++ "`; expected strict|safe|dynamic")

data Stmt
   = StmtTypeDef TypeDef
   | StmtFunc Func
   | StmtExtern Extern
   | StmtSystem System
   deriving (Eq, Show)

stmtKeyword :: Parser Text
stmtKeyword = "extern" <|> "fn" <|> "type" <|> "system"

stmt :: Parser Stmt
stmt = do
   tok <- lookAhead stmtKeyword
   case tok of
      "extern" -> StmtExtern  <$> extern
      "fn"     -> StmtFunc    <$> func
      "type"   -> StmtTypeDef <$> typeDef
      "system" -> StmtSystem  <$> system
      _        -> fail "expected \"extern\", \"fn\", \"type\", \"system\""

parse :: String -> Text -> IO ()
parse fname input = putStrLn $
   case runParser (sc *> program <* eof) fname input of
      Left e -> "Got error:\n" <> errorBundlePretty e
      Right found -> show found
