{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast (parse) where

import Data.Text (Text, unpack)
import Text.Megaparsec (choice, runParser, MonadParsec (eof, try), errorBundlePretty, some, many, (<|>))
import Xast.Parser.Type (TypeDef, typedef, ExternType, externType)
import Xast.Parser.Function (FuncDef, funcdef, FuncImpl, funcImpl, ExternFunc, externFunc)
import Xast.Parser (Parser, sc, symbol)
import Xast.Parser.Headers (ModuleDef, ImportDef, moduleDef, importDef)
import Xast.Parser.Expr (stringLiteral)

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
   _     <- symbol "@mode"
   _     <- symbol "="
   str   <- stringLiteral

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
   deriving (Eq, Show)

stmt :: Parser Stmt
stmt = choice
   [ try (StmtExternType <$> externType)
   , try (StmtExternFunc <$> externFunc)
   , try (StmtTypeDef    <$> typedef)
   , try (StmtFuncDef    <$> funcdef)
   , try (StmtFuncImpl   <$> funcImpl)
   ]

parse :: String -> Text -> IO ()
parse fname input = putStrLn $
   case runParser (sc *> program <* eof) fname input of
      Left e -> "Got error:\n" <> errorBundlePretty e
      Right found -> show found
