{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Program where

import Data.Text (Text, unpack)
import Text.Megaparsec (MonadParsec (lookAhead, eof), some, many, (<|>), runParser)
import Xast.Parser.Type (TypeDef, typeDef)
import Xast.Parser.Function (Func, func)
import Xast.Parser (Parser, symbol, sc, Located)
import Xast.Parser.Headers (ModuleDef, ImportDef, moduleDef, importDef)
import Xast.Parser.Expr (stringLiteral)
import Xast.Parser.System (System, system)
import Xast.Parser.Extern
import Xast.Error (XastError (XastParseError))
import Data.Bifunctor (Bifunctor(first))

parseProgram :: String -> Text -> Either XastError Program
parseProgram filename code = first XastParseError $ 
   runParser (sc *> program <* eof) filename code

data Program = Program 
   { progMode :: Mode
   , progModuleDef :: Located ModuleDef
   , progImports :: [Located ImportDef]
   , progStmts :: [Stmt]
   }
   deriving (Eq, Show)

program :: Parser Program
program = do
   progMode       <- mode <|> pure MStrict
   progModuleDef  <- moduleDef
   progImports    <- many importDef
   progStmts      <- some stmt

   return Program { .. }

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
   = StmtTypeDef (Located TypeDef)
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