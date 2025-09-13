module Xast (parse) where

import Xast.Parser.Type (TypeDef, typedef)
import Xast.Parser.Function (FuncDef, funcdef, FuncImpl, funcImpl)
import Xast.Parser (Parser, sc)
import Text.Megaparsec (many, choice, runParser, MonadParsec (eof, try), errorBundlePretty)
import Data.Text (Text)

data Xast
   = TypeDef TypeDef
   | FuncDef FuncDef
   | FuncImpl FuncImpl
   deriving (Eq, Show)

xast :: Parser [Xast]
xast = sc *> many (choice
   [ try (TypeDef    <$> typedef)
   , try (FuncDef    <$> funcdef)
   , try (FuncImpl   <$> funcImpl)
   ])

parse :: String -> Text -> IO ()
parse fname input = putStrLn $
   case runParser (xast <* eof) fname input of
      Left e -> "Got error:\n" <> errorBundlePretty e
      Right found -> show found
