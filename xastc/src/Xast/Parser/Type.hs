{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Xast.Parser.Type
   ( TypeDef(..), typeDef
   , Type(..), type', atomType
   , Field(..), field
   , Payload(..), payload
   , Ctor(..), ctor
   ) where

import Xast.Parser.Ident
import Xast.Parser (Parser, symbol, lexeme, endOfStmt, Located, located)
import Text.Megaparsec (choice, sepBy, between, some, MonadParsec (try), many, sepBy1)
import Data.Function ((&))

data TypeDef = TypeDef
   { tdName       :: Ident
   , tdGenerics   :: [Ident]
   , tdCtors      :: [Located Ctor]
   }
   deriving (Eq, Show)

typeDef :: Parser (Located TypeDef)
typeDef = located $ do
   _           <- symbol "type"
   tdName      <- typeIdent
   tdGenerics  <- many genericIdent
   _           <- symbol "="
   tdCtors     <- ctor `sepBy1` symbol "|"
   _           <- endOfStmt

   return TypeDef {..}

data Ctor = Ctor
   { ctorName     :: Ident
   , ctorPayload  :: Payload
   }
   deriving (Eq, Show)

ctor :: Parser (Located Ctor)
ctor = located $ do
   ctorName    <- typeIdent
   ctorPayload <- payload
   return Ctor {..}

data Payload
   = PUnit
   | PTuple [Type]
   | PRecord [Field]
   deriving (Eq, Show)

payload :: Parser Payload
payload = choice
   [ PRecord   <$> between (symbol "{") (symbol "}") (field `sepBy` symbol ",")
   , PTuple    <$> try (some (lexeme atomType))
   , PUnit     & pure
   ]

data Field = Field      -- fieldOne : Int
   { fldName :: Ident   -- field2 : Maybe Bool
   , fldType :: Type
   }
   deriving (Eq, Show)

field :: Parser Field
field = do
   fldName <- varIdent
   _       <- symbol ":"
   fldType <- type'

   return Field {..}

data Type
   = TyGnr Ident        -- a, b, c...
   | TyCon Ident        -- Bool, Int, String
   | TyApp Type Type    -- Maybe a, Either a Int...
   | TyTuple [Type]     -- (Bool, a, Maybe String)
   | TyFn [Type] Type   -- fn(Type1, Type2 ... TypeN) -> TypeRet
   deriving (Eq, Show)

type' :: Parser Type
type' = do
   atoms <- some atomType
   pure (foldl1 TyApp atoms)

atomType :: Parser Type
atomType = choice
   [ tupleOrParens
   , TyFn 
      <$ symbol "fn" 
      <*> between (symbol "(") (symbol ")") (type' `sepBy` symbol ",")
      <* symbol "->"
      <*> type'
   , TyCon <$> typeIdent
   , TyGnr <$> genericIdent
   ]

tupleOrParens :: Parser Type
tupleOrParens = between (symbol "(") (symbol ")") $ do
   ts <- type' `sepBy` symbol ","
   case ts of
      [] -> pure (TyTuple [])
      [t] -> pure t
      manyT -> pure (TyTuple manyT)