{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast (parse) where

import Data.Text (Text, unpack)
import Text.Megaparsec (choice, runParser, MonadParsec (eof, try), errorBundlePretty, some, many, (<|>))
import Xast.Parser.Type (TypeDef, typeDef, ExternType, externType)
import Xast.Parser.Function (FuncDef, funcDef, FuncImpl, funcImpl, ExternFunc, externFunc)
import Xast.Parser (Parser, sc, symbol)
import Xast.Parser.Headers (ModuleDef, ImportDef, moduleDef, importDef)
import Xast.Parser.Expr (stringLiteral)
import Xast.Parser.System (SystemDef, systemDef, SystemImpl, systemImpl)

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
   | StmtExternType ExternType
   | StmtFuncDef FuncDef
   | StmtExternFunc ExternFunc
   | StmtFuncImpl FuncImpl
   | StmtSystemDef SystemDef
   | StmtSystemImpl SystemImpl
   deriving (Eq, Show)

stmt :: Parser Stmt
stmt = choice
   [ try (StmtExternType <$> externType)
   , try (StmtExternFunc <$> externFunc)
   , try (StmtTypeDef    <$> typeDef)
   , try (StmtFuncDef    <$> funcDef)
   , try (StmtFuncImpl   <$> funcImpl)
   , try (StmtSystemDef  <$> systemDef)
   , try (StmtSystemImpl <$> systemImpl)
   ]

parse :: String -> Text -> IO ()
parse fname input = putStrLn $
   case runParser (sc *> program <* eof) fname input of
      Left e -> "Got error:\n" <> errorBundlePretty e
      Right found -> show found
