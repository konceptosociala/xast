{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Xast.Parser.Expr where

import Control.Monad.Combinators.Expr
import Data.Text (Text, pack)
import Data.List (foldl', foldl1')
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import Xast.AST
import Xast.Parser.Ident (varIdent, typeIdent)
import Xast.Parser.Common (Parser, lexeme, symbol, located)

pattern' :: Parser (Located (Pattern Parsed))
pattern' = choice
   [ try (located (PatCon ParsedInfo <$> typeIdent <*> some atomPattern'))
   , atomPattern'
   ]

atomPattern' :: Parser (Located (Pattern Parsed))
atomPattern' = choice
   [ tupleOrParensPat
   , located (PatWildcard ParsedInfo  <$ symbol "_")
   , located (PatVar ParsedInfo       <$> varIdent)
   , located (PatCon ParsedInfo       <$> typeIdent <*> pure [])
   , located (PatList ParsedInfo      <$> between (symbol "[") (symbol "]") (pattern' `sepBy` symbol ","))
   , located (PatLit ParsedInfo       <$> literal)
   ]

tupleOrParensPat :: Parser (Located (Pattern Parsed))
tupleOrParensPat = located $ between (symbol "(") (symbol ")") $ do
   ts <- pattern' `sepBy` symbol ","
   case ts of
      [] -> pure (PatTuple ParsedInfo [])
      [t] -> pure t.node
      manyT -> pure (PatTuple ParsedInfo manyT)

atomExpr :: Parser (Located (Expr Parsed))
atomExpr = do
   base <- located $ choice
      [ tupleOrParens
      , try (ExpVar ParsedInfo           <$> optional (try (typeIdent <* symbol ".")) <*> varIdent)
      , try (ExpRecConstruct ParsedInfo  <$> recConstruct)
      , ExpCon ParsedInfo    <$> optional (try (typeIdent <* symbol ".")) <*> typeIdent
      , ExpList ParsedInfo   <$> between (symbol "[") (symbol "]") (expr `sepBy` symbol ",")
      , ExpLit ParsedInfo    <$> literal
      , ExpLambda ParsedInfo <$> lambda
      , ExpLetIn ParsedInfo  <$> letIn
      , ExpIfThen ParsedInfo <$> ifThenElse
      , ExpMatch ParsedInfo  <$> match'
      ]
   getters <- many (located (try (symbol "." *> varGetter)))
   let based = foldl' applyGetter base getters
   updates <- many (located recUpdateBlock)
   pure $ foldl' applyRecUpdate based updates
   where
      applyGetter l@(Located (Location posL offL _) _) (Located (Location _ offR lenR) getter) =
         Located (Location posL offL ((offR + lenR) - offL)) (ExpVarGetter ParsedInfo l getter)

      applyRecUpdate l@(Located (Location posL offL _) _) (Located (Location _ offR lenR) assigns) =
         Located (Location posL offL ((offR + lenR) - offL)) (ExpRecUpdate ParsedInfo (RecUpdate l assigns))

tupleOrParens :: Parser (Expr Parsed)
tupleOrParens = between (symbol "(") (symbol ")") $ do
   ts <- expr `sepBy` symbol ","
   case ts of
      [] -> pure (ExpTuple ParsedInfo [])
      [Located _ t] -> pure t
      manyT -> pure (ExpTuple ParsedInfo manyT)

recConstruct :: Parser (RecConstruct Parsed)
recConstruct = do
   bind    <- optional (try (typeIdent <* symbol "."))
   con     <- typeIdent
   assigns <- between
      (symbol "{")
      (symbol "}")
      (recAssign `sepEndBy1` symbol ",")

   return RecConstruct {..}

recAssign :: Parser (RecAssign Parsed)
recAssign = RecAssign <$> located varIdent <* symbol "=" <*> expr

recUpdateBlock :: Parser [RecAssign Parsed]
recUpdateBlock = between (symbol "{") (symbol "}") (recAssign `sepEndBy1` symbol ",")

varGetter :: Parser Getter
varGetter = choice
   [ GetTupleField <$> intLiteral
   , GetField      <$> varIdent
   ]

match' :: Parser (Match Parsed)
match' = do
   _         <- symbol "match"
   baseExpr  <- expr
   _         <- symbol "with"
   matches   <- matchWing `sepBy1` symbol ","

   return Match {..}

matchWing :: Parser (MatchWing Parsed)
matchWing = MatchWing <$> pattern' <* symbol "->" <*> expr

term :: Parser (Located (Expr Parsed))
term = do
   atoms <- some atomExpr
   pure $ foldl1' app atoms
   where
      app l@(Located (Location posL offL _) _) r@(Located (Location _ offR lenR) _) =
         Located (Location posL offL ((offR + lenR) - offL)) (ExpApp ParsedInfo l r)

opIdent :: BuiltinOp -> Ident
opIdent op = case op of
   OpPlus    -> Ident "opAdd"
   OpMinus   -> Ident "opSub"
   OpMul     -> Ident "opMul"
   OpDiv     -> Ident "opDiv"
   OpMod     -> Ident "opMod"
   OpPow     -> Ident "opPow"
   OpEq      -> Ident "opEq"
   OpNeq     -> Ident "opNeq"
   OpLt      -> Ident "opLt"
   OpGt      -> Ident "opGt"
   OpLe      -> Ident "opLe"
   OpGe      -> Ident "opGe"
   OpAnd     -> Ident "opAnd"
   OpOr      -> Ident "opOr"
   OpNot     -> Ident "opNot"
   OpPipe    -> Ident "opPipe"
   OpApply   -> Ident "opApply"
   OpConcat  -> Ident "opConcat"
   OpNeg     -> Ident "opNeg"

opVar :: BuiltinOp -> Expr Parsed
opVar = ExpVar ParsedInfo Nothing . opIdent

opToken :: BuiltinOp -> Text
opToken op = case op of
   OpPlus    -> "+"
   OpMinus   -> "-"
   OpNeg     -> "-"
   OpMul     -> "*"
   OpDiv     -> "/"
   OpMod     -> "%"
   OpPow     -> "**"
   OpEq      -> "=="
   OpNeq     -> "!="
   OpLt      -> "<"
   OpGt      -> ">"
   OpLe      -> "<="
   OpGe      -> ">="
   OpAnd     -> "&&"
   OpOr      -> "||"
   OpNot     -> "!"
   OpPipe    -> "|>"
   OpApply   -> "<|"
   OpConcat  -> "<>"

opLen :: BuiltinOp -> Int
opLen op = case op of
   OpPlus    -> 1
   OpMinus   -> 1
   OpNeg     -> 1
   OpMul     -> 1
   OpDiv     -> 1
   OpMod     -> 1
   OpPow     -> 2
   OpEq      -> 2
   OpNeq     -> 2
   OpLt      -> 1
   OpGt      -> 1
   OpLe      -> 2
   OpGe      -> 2
   OpAnd     -> 2
   OpOr      -> 2
   OpNot     -> 1
   OpPipe    -> 2
   OpApply   -> 2
   OpConcat  -> 2

binOp :: Location -> BuiltinOp -> Located (Expr Parsed) -> Located (Expr Parsed) -> Located (Expr Parsed)
binOp opLoc op a@(Located (Location posA offA _) _) b@(Located (Location _ offB lenB) _) =
   -- Span from start of a to end of b
   let totalLen = (offB + lenB) - offA
   in Located
         (Location posA offA totalLen)
         (ExpApp ParsedInfo
            (Located opLoc
            (ExpApp ParsedInfo
               (Located opLoc (opVar op))
               a
            ))
         b)

table :: [[Operator Parser (Located (Expr Parsed))]]
table =
   [  [ Prefix (unaryDirect OpNot)
      , Prefix (unary OpNeg)
      ]

   ,  [ InfixR (binary OpPow) ]

   ,  [ InfixL (binary OpMul)
      , InfixL (binary OpDiv)
      , InfixL (binary OpMod)
      ]

   ,  [ InfixL (binary OpPlus)
      , InfixL (binary OpMinus)
      ]

   ,  [ InfixN (binary OpEq)
      , InfixN (binary OpNeq)
      , InfixN (binary OpLe)
      , InfixN (binary OpGe)
      , InfixN (binaryGuarded OpLt "=|>")
      , InfixN (binaryGuarded OpGt "=")
      ]

   ,  [ InfixR (binary OpAnd) ]
   ,  [ InfixR (binary OpOr) ]

   ,  [ InfixL (binary OpPipe) ]
   ,  [ InfixL (binary OpConcat) ]

   ,  [ InfixR applyLeft ]
   ]

binary :: BuiltinOp -> Parser (Located (Expr Parsed) -> Located (Expr Parsed) -> Located (Expr Parsed))
binary op = do
   pos <- getSourcePos
   off <- getOffset
   _ <- symbol (opToken op)
   let opLoc = Location pos off (opLen op)
   pure (binOp opLoc op)

applyLeft :: Parser (Located (Expr Parsed) -> Located (Expr Parsed) -> Located (Expr Parsed))
applyLeft = do
   _ <- symbol (opToken OpApply)
   pure $ \f@(Located (Location posF offF _) _) x@(Located (Location _ offX lenX) _) ->
      Located
         (Location posF offF ((offX + lenX) - offF))
         (ExpApp ParsedInfo f x)

-- | Like `binary`, but the token must not be immediately followed by any of
-- `forbidden` — used to keep `<`/`>` from swallowing the first char of a
-- longer operator that shares their prefix (`<=`, `<|`, `<>`, `>=`).
binaryGuarded :: BuiltinOp -> [Char] -> Parser (Located (Expr Parsed) -> Located (Expr Parsed) -> Located (Expr Parsed))
binaryGuarded op forbidden = do
   pos <- getSourcePos
   off <- getOffset
   _ <- lexeme (try (string (opToken op) <* notFollowedBy (satisfy (`elem` forbidden))))
   let opLoc = Location pos off (opLen op)
   pure (binOp opLoc op)

unary :: BuiltinOp -> Parser (Located (Expr Parsed) -> Located (Expr Parsed))
unary op = do
   pos <- getSourcePos
   off <- getOffset
   _ <- symbol (opToken op)
   let opLoc = Location pos off (opLen op)
   pure $ \x -> binOp opLoc op (Located opLoc (ExpLit ParsedInfo (LitInt 0))) x

unaryDirect :: BuiltinOp -> Parser (Located (Expr Parsed) -> Located (Expr Parsed))
unaryDirect op = do
   pos <- getSourcePos
   off <- getOffset
   _ <- symbol (opToken op)
   let opLoc = Location pos off (opLen op)
   pure $ \x@(Located (Location _ offX lenX) _) ->
      Located
         (Location pos off ((offX + lenX) - off))
         (ExpApp ParsedInfo (Located opLoc (opVar op)) x)

expr :: Parser (Located (Expr Parsed))
expr = makeExprParser term table

ifThenElse :: Parser (IfThenElse Parsed)
ifThenElse = do
   _        <- symbol "if"
   ifExpr   <- expr
   _        <- symbol "then"
   thenExpr <- expr
   _        <- symbol "else"
   elseExpr <- expr

   return IfThenElse {..}

lambda :: Parser (Lambda Parsed)
lambda = do
   _        <- symbol ".\\"
   args     <- some pattern'
   _        <- symbol "->"
   body     <- expr

   return Lambda {..}

letIn :: Parser (LetIn Parsed)
letIn = do
   bindings <- let' `sepBy1` symbol "and"
   _        <- symbol "in"
   bindExpr <- expr

   return LetIn {..}

let' :: Parser (Located (Let Parsed))
let' = located $ do
   _         <- symbol "let"
   pat       <- pattern'
   _         <- symbol "="
   value     <- expr

   return Let {..}

literal :: Parser Literal
literal = choice
   [ tupleOrParensLit
   , LitString <$> stringLiteral
   , LitChar   <$> charLiteral
   , LitFloat  <$> try floatLiteral
   , LitInt    <$> intLiteral
   , LitList   <$> between (symbol "[") (symbol "]") (located literal `sepBy` symbol ",")
   ]

tupleOrParensLit :: Parser Literal
tupleOrParensLit = between (symbol "(") (symbol ")") $ do
   ts <- located literal `sepBy` symbol ","
   case ts of
      [] -> pure (LitTuple [])
      [t] -> pure t.node
      manyT -> pure (LitTuple manyT)

floatLiteral :: Parser Float
floatLiteral = lexeme L.float

intLiteral :: Parser Int
intLiteral = lexeme L.decimal

charLiteral :: Parser Char
charLiteral = lexeme $ between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = pack <$> lexeme lit
   where lit = char '\"' *> manyTill L.charLiteral (char '\"')