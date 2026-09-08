{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Xast.Parser.Modifier where

import Text.Megaparsec
import qualified Data.Set as Set
import Xast.Parser.Common (Parser, symbol)
import Xast.AST (Modifier (..), FnModifier (..), ComponentDispatchMode (..), SysModifier (..), TypeModifier (..), ExtFnModifier (ModIntrinsic), displayModifier)
import Xast.Parser.Ident (fnIdent, typeIdent)
import Xast.Parser.Expr (stringLiteral)

failAt :: Int -> String -> Parser a
failAt offset msg = region (const (FancyError offset (Set.singleton (ErrorFail msg)))) (fail msg)

modifier :: Parser Modifier
modifier = "@" *> choice
   [ FnMod <$> choice
      [ ModSharedVariant   <$ symbol "SharedVariant" <*> between (symbol "(") (symbol ")") (fnIdent <?> "shared variant function name")
      , ModMemoize         <$ symbol "Memoize"
      , ModInline          <$ symbol "Inline"
      , ModDeprecated      <$ symbol "Deprecated" <*> between (symbol "(") (symbol ")") (stringLiteral <?> "string literal")
      , ModSupUnreachable  <$ symbol "SuppressUnreachable"
      , ModCompileTime     <$ symbol "CompileTime"
      ]
   , SysMod <$> choice
      [ ModCompDispatchMode   <$ symbol "Mode" <*> between (symbol "(") (symbol ")") compDispatchMode
      , ModLabel              <$ symbol "Label" <*> between (symbol "(") (symbol ")") (typeIdent <?> "CamelCase system pipeline label ident")
      , ModParallel           <$ symbol "Parallel"
      ]
   , TypeMod <$> choice
      [ ModSingleton       <$ symbol "Singleton"
      , ModCopyable        <$ symbol "Copyable"
      , ModTag             <$ symbol "Tag"
      , ModNonExhaustive   <$ symbol "NonExhaustive"
      ]
   , ExtFnMod <$> choice
      [ ModIntrinsic       <$ symbol "Intrinsic"
      ]
   ]

extFnModifier :: Parser Modifier
extFnModifier = do
   start <- getOffset
   modif <- modifier <?> "invalid modifier"
   case modif of
      ExtFnMod _ -> return modif
      other -> failAt start $
         "invalid extern function modifier used: " ++ displayModifier other

fnModifier :: Parser Modifier
fnModifier = do
   start <- getOffset
   modif <- modifier <?> "invalid modifier"
   case modif of
      FnMod _ -> return modif
      other -> failAt start $ 
         "invalid function modifier used: " ++ displayModifier other

sysModifier :: Parser Modifier
sysModifier = do
   start <- getOffset
   modif <- modifier <?> "invalid modifier"
   case modif of
      SysMod _ -> return modif
      other -> failAt start $
         "invalid system modifier used: " ++ displayModifier other

typeModifier :: Parser Modifier
typeModifier = do
   start <- getOffset
   modif <- modifier <?> "invalid modifier"
   case modif of
      TypeMod _ -> return modif
      other -> failAt start $ 
         "invalid type modifier used: " ++ displayModifier other

compDispatchMode :: Parser ComponentDispatchMode
compDispatchMode = choice
   [ CDMDynamic   <$ symbol "Dynamic"
   , CDMSafe      <$ symbol "Safe"
   , CDMStrict    <$ symbol "Strict"
   ] <?> "a valid component dispatch mode (Dynamic, Safe, or Strict)"

modifierTag :: Modifier -> Int
modifierTag = \case
   FnMod (ModSharedVariant _)       -> 0
   FnMod ModMemoize                 -> 1
   FnMod ModInline                  -> 2
   FnMod (ModDeprecated _)          -> 3
   FnMod ModSupUnreachable          -> 4
   FnMod ModCompileTime             -> 5
   SysMod (ModCompDispatchMode _)   -> 6
   SysMod (ModLabel _)              -> 7
   SysMod ModParallel               -> 8
   TypeMod ModSingleton             -> 9
   TypeMod ModCopyable              -> 10
   TypeMod ModTag                   -> 11
   TypeMod ModNonExhaustive         -> 12
   ExtFnMod ModIntrinsic            -> 13

-- | Fails the parse if any two modifiers in the list share a kind
noRepeatedModifiers :: [Modifier] -> Parser [Modifier]
noRepeatedModifiers mods = go [] mods
   where
      go _ [] = pure mods
      go seen (m:rest)
         | modifierTag m `elem` seen = fail ("modifier `" ++ displayModifier m ++ "` cannot be used more than once")
         | otherwise = go (modifierTag m : seen) rest