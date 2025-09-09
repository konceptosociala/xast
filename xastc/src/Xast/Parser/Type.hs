{-# LANGUAGE OverloadedStrings #-}

module Xast.Parser.Type
   ( TypeDef
   , Type(..)
   -- , typeDef
   , parseType
   ) where

import Xast.Parser.Ident
import Xast.Parser (Parser, lexeme, symbol)
import Text.Megaparsec (choice, sepBy, between, some)

data TypeDef = TypeDef
   { tdName       :: Ident
   , tdGenerics   :: [Ident]
   , tdCtors      :: [Ctor]
   }

data Ctor = Ctor
   { ctorName     :: Ident
   , ctorPayload  :: Payload
   }

data Payload
   = PUnit
   | PTuple [Type]
   | PRecord [Field]

data Field = Field
   { fldName :: Ident
   , fldType :: Type
   }

data Type
   = TyGnr Ident -- a, b, c...
   | TyCon Ident -- Bool, Int, String
   | TyApp Type Type -- Maybe a, Either a Int...
   | TyTuple [Type] -- (Bool, a, Maybe String)
   deriving (Eq, Show)

parseType :: Parser Type
parseType = do
   atoms <- some (lexeme atomType)
   pure (foldl1 TyApp atoms)

atomType :: Parser Type
atomType = choice
   [ tupleOrParens
   , TyCon <$> typeIdent
   , TyGnr <$> genericIdent
   ]

tupleOrParens :: Parser Type
tupleOrParens = between (symbol "(") (symbol ")") $ do
   ts <- parseType `sepBy` symbol ","
   case ts of
      [] -> pure (TyTuple [])
      [t] -> pure t
      manyT -> pure (TyTuple manyT)