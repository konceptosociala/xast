{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.HUnit
import Xast.Parser.Ident
import Xast.Parser.Type
import Xast.Parser (Parser)
import Text.Megaparsec (runParser, errorBundlePretty, MonadParsec (eof))
import Data.Text (Text)
import Control.Monad (unless)

-- Identifiers

testGenericIdent :: Test
testGenericIdent = TestCase $ do
   assertParses genericIdent "a" (Ident "a")
   assertParses genericIdent "z" (Ident "z")
   assertFails genericIdent "A"
   assertFails genericIdent "AB"
   assertFails genericIdent "0"

testTypeIdent :: Test
testTypeIdent = TestCase $ do
   assertParses typeIdent "Bool" (Ident "Bool")
   assertParses typeIdent "Int" (Ident "Int")
   assertFails typeIdent "a"
   assertFails typeIdent "0"

testFnIdent :: Test
testFnIdent = TestCase $ do
   assertParses fnIdent "fooBar" (Ident "fooBar")
   assertParses fnIdent "doStuff" (Ident "doStuff")
   assertFails fnIdent "FooBar"
   assertFails fnIdent "foo_bar"
   assertFails fnIdent "0foo"

testVarIdent :: Test
testVarIdent = TestCase $ do
   assertParses varIdent "someVar" (Ident "someVar")
   assertParses varIdent "x" (Ident "x")
   assertFails varIdent "SomeVar"
   assertFails varIdent "some_var"
   assertFails varIdent "1var"

-- Typedef

testParseType :: Test
testParseType = TestCase $ do
   assertParses parseType "a" (TyGnr (Ident "a"))
   assertParses parseType "Bool" (TyCon (Ident "Bool"))
   assertParses parseType "Maybe a" (TyApp (TyCon (Ident "Maybe")) (TyGnr (Ident "a")))
   assertParses parseType "(Bool, a, Maybe String)" 
      (TyTuple 
         [ TyCon (Ident "Bool")
         , TyGnr (Ident "a")
         , TyApp (TyCon (Ident "Maybe")) (TyCon (Ident "String"))
         ]
      )
   assertFails parseType ""
   assertFails parseType "(Bool, a, Maybe String"

-- Tests

tests :: Test
tests = TestList 
   [ TestLabel "Parse generic ident" testGenericIdent
   , TestLabel "Parse type ident" testTypeIdent
   , TestLabel "Parse fn ident" testFnIdent
   , TestLabel "Parse var ident" testVarIdent
   , TestLabel "Parse type" testParseType
   ]

main :: IO ()
main = do
   counts <- runTestTT tests
   if errors counts + failures counts == 0 
      then pure () 
      else fail "Tests failed"

assertParses :: (Eq a, Show a)
   => Parser a
   -> Text
   -> a
   -> Assertion
assertParses p input expected =
   case runParser (p <* eof) "<test>" input of
      Left e ->
         assertFailure $
            "Expected success, got error:\n" <> errorBundlePretty e

      Right found ->
         unless (found == expected) $
            assertFailure $
               "Parsed value mismatch:\n" <>
               "   expected: " <> show expected <> "\n" <>
               "   found:    " <> show found

assertFails :: Parser a -> Text -> Assertion
assertFails p input =
   case runParser (p <* eof) "<test>" input of
      Right _ -> assertFailure "Expected failure, but parsing succeeded"
      Left _  -> return ()