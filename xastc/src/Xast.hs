{-# LANGUAGE RecordWildCards #-}
module Xast (parse) where

import Xast.Parser.Type (TypeDef, typedef)
import Xast.Parser.Function (FuncDef, funcdef, FuncImpl, funcImpl)
import Xast.Parser (Parser, sc)
import Text.Megaparsec (choice, runParser, MonadParsec (eof, try), errorBundlePretty, some, many)
import Data.Text (Text)
import Xast.Parser.Headers (ModuleDef, ImportDef, moduleDef, importDef)

data Program = Program
   { progModuleDef :: ModuleDef
   , progImports :: [ImportDef]
   , progStmts :: [Stmt]
   }
   deriving (Eq, Show)

program :: Parser Program
program = do
   progModuleDef  <- moduleDef
   progImports    <- many importDef
   progStmts      <- some stmt

   return Program {..}

data Stmt
   = StmtTypeDef TypeDef
   | StmtFuncDef FuncDef
   | StmtFuncImpl FuncImpl
   deriving (Eq, Show)

stmt :: Parser Stmt
stmt = choice
   [ try (StmtTypeDef    <$> typedef)
   , try (StmtFuncDef    <$> funcdef)
   , try (StmtFuncImpl   <$> funcImpl)
   ]

parse :: String -> Text -> IO ()
parse fname input = putStrLn $
   case runParser (sc *> program <* eof) fname input of
      Left e -> "Got error:\n" <> errorBundlePretty e
      Right found -> show found
