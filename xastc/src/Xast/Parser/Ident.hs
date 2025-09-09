module Xast.Parser.Ident 
   ( Ident(..)
   , typeIdent
   , fnIdent
   , genericIdent
   , varIdent
   ) where
      
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Text.Megaparsec.Char (upperChar, lowerChar, alphaNumChar)
import Text.Megaparsec
import Xast.Parser

newtype Ident = Ident { unIdent :: Text }
   deriving (Eq, Ord, Show, Generic)

genericIdent :: Parser Ident
genericIdent = lexeme $ do
   c <- lowerChar
   return $ Ident (pack [c])

typeIdent :: Parser Ident
typeIdent = pascalCase

fnIdent :: Parser Ident
fnIdent = camelCase

varIdent :: Parser Ident
varIdent = camelCase

pascalCase :: Parser Ident
pascalCase = lexeme $ do
   first <- upperChar
   rest  <- many alphaNumChar
   return $ Ident (pack (first:rest))

camelCase :: Parser Ident
camelCase = lexeme $ do
   first <- lowerChar
   rest  <- many alphaNumChar
   return $ Ident (pack (first:rest))