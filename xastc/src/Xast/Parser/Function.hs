{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Xast.Parser.Function
   ( FuncDef(..), funcdef
   , FuncImpl(..), funcImpl
   , Pattern(..), pattern
   ) where
      
import Xast.Parser.Type (Type, type')
import Xast.Parser.Ident (Ident, fnIdent, varIdent, typeIdent)
import Xast.Parser.Expr (Literal, literal, Expr, expr)
import Xast.Parser
import Text.Megaparsec (between, sepBy, choice, MonadParsec (try), many)

-- fn myFunc (Type1, Type2) -> TypeReturn
data FuncDef = FuncDef
   { fdName :: Ident
   , fdArgs :: [Type]
   , fdRet :: Type
   }
   deriving (Eq, Show)

funcdef :: Parser FuncDef
funcdef = do
   _        <- symbol "fn"
   fdName   <- fnIdent
   fdArgs   <- between (symbol "(") (symbol ")") (type' `sepBy` symbol ",")
   _        <- symbol "->"
   fdRet    <- type'
   _        <- endOfStmt

   return FuncDef {..}

-- fn IDENT arg1 arg2 ... argN = <IMPL>
data FuncImpl = FuncImpl
   { fnName :: Ident
   , fnArgs :: [Pattern]
   , fnBody :: Expr
   }
   deriving (Eq, Show)

funcImpl :: Parser FuncImpl
funcImpl = do
   _        <- symbol "fn"
   fnName   <- fnIdent
   fnArgs   <- many pattern
   _        <- symbol "="
   fnBody   <- expr
   _        <- endOfStmt

   return FuncImpl {..}

data Pattern
   = PatVar Ident             -- a
   | PatWildcard              -- _
   | PatLit Literal           -- "abc"
   | PatList [Pattern]        -- [a, 2, 3]
   | PatTuple [Pattern]       -- (a, _, 12)
   | PatCon Ident [Pattern]   -- Either a b
   deriving (Eq, Show)

pattern :: Parser Pattern
pattern = choice
   [ tupleOrParens
   , PatWildcard  <$ symbol "_"
   , PatVar       <$> varIdent
   , PatCon       <$> typeIdent <*> many pattern
   , PatLit       <$> try literal
   , PatList      <$> between (symbol "[") (symbol "]") (pattern `sepBy` symbol ",")
   ]

tupleOrParens :: Parser Pattern
tupleOrParens = between (symbol "(") (symbol ")") $ do
   ts <- pattern `sepBy` symbol ","
   case ts of
      [] -> pure (PatTuple [])
      [t] -> pure t
      manyT -> pure (PatTuple manyT)