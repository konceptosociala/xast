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

testType :: Test
testType = TestCase $ do
   assertParses type' "a" (TyGnr (Ident "a"))
   assertParses type' "Bool" (TyCon (Ident "Bool"))
   assertParses type' "Maybe a" (TyApp (TyCon (Ident "Maybe")) (TyGnr (Ident "a")))
   assertParses type' "(Bool, a, Maybe String)" 
      (TyTuple 
         [ TyCon (Ident "Bool")
         , TyGnr (Ident "a")
         , TyApp (TyCon (Ident "Maybe")) (TyCon (Ident "String"))
         ]
      )
   assertFails type' ""
   assertFails type' "(Bool, a, Maybe String"

testParseField :: Test
testParseField = TestCase $ do
   assertParses field "fieldOne : Int"
      (Field 
         (Ident "fieldOne") 
         (TyCon (Ident "Int"))
      )
   assertParses field "field2 : Maybe Bool"
      (Field 
         (Ident "field2") 
         (TyApp (TyCon (Ident "Maybe")) (TyCon (Ident "Bool")))
      )
   assertFails field "fieldThree Int"
   assertFails field "fieldFour :"

testParsePayload :: Test
testParsePayload = TestCase $ do
   assertParses payload "" PUnit
   assertParses payload "Int (Maybe Int) Bool" 
      (PTuple 
         [ TyCon (Ident "Int")
         , TyApp (TyCon (Ident "Maybe")) (TyCon (Ident "Int"))
         , TyCon (Ident "Bool")
         ]
      )
   assertParses payload "{ fieldOne : Int, field2 : Either a b }"
      (PRecord 
         [ Field 
            (Ident "fieldOne") 
            (TyCon (Ident "Int"))
         , Field 
            (Ident "field2") 
            (TyApp 
               (TyApp (TyCon (Ident "Either")) (TyGnr (Ident "a"))) 
               (TyGnr (Ident "b"))
            )
         ]
      )

   assertFails payload "{ fieldOne Int, field2 : Either a b }"
   assertFails payload "{ fieldOne : Int, field2 : Either a b "

testParseCtor :: Test
testParseCtor = TestCase $ do
   assertParses ctor "Just a"
      (Ctor 
         (Ident "Just")
         (PTuple [TyGnr (Ident "a")])
      )
   assertParses ctor "Nothing"
      (Ctor 
         (Ident "Nothing")
         PUnit
      )
   assertParses ctor "Node { left : Maybe (Tree a), right : Maybe (Tree a) }"
      (Ctor 
         (Ident "Node")
         (PRecord 
            [ Field 
               (Ident "left")
               (TyApp 
                  (TyCon (Ident "Maybe"))
                  (TyApp 
                     (TyCon (Ident "Tree"))
                     (TyGnr (Ident "a"))
                  )
               )
            , Field 
               (Ident "right")
               (TyApp 
                  (TyCon (Ident "Maybe"))
                  (TyApp 
                     (TyCon (Ident "Tree"))
                     (TyGnr (Ident "a"))
                  )
               )
            ]
         )
      )

testParseTypeDef :: Test
testParseTypeDef = TestCase $ do
   assertParses typedef "type Maybe a = Just a | Nothing"
      (TypeDef 
         (Ident "Maybe")
         [Ident "a"]
         [ Ctor 
            (Ident "Just")
            (PTuple [TyGnr (Ident "a")])
         , Ctor 
            (Ident "Nothing")
            PUnit
         ]
      )
   assertParses typedef "type Tree a = Node { left : Maybe (Tree a), right : Maybe (Tree a) } | Leaf"
      (TypeDef 
         (Ident "Tree")
         [Ident "a"]
         [ Ctor 
            (Ident "Node")
            (PRecord 
               [ Field 
                  (Ident "left")
                  (TyApp 
                     (TyCon (Ident "Maybe"))
                     (TyApp 
                        (TyCon (Ident "Tree"))
                        (TyGnr (Ident "a"))
                     )
                  )
               , Field 
                  (Ident "right")
                  (TyApp 
                     (TyCon (Ident "Maybe"))
                     (TyApp 
                        (TyCon (Ident "Tree"))
                        (TyGnr (Ident "a"))
                     )
                  )
               ]
            )
         , Ctor 
            (Ident "Leaf")
            PUnit
         ]
      )

   assertFails typedef "type Maybe a Just a | Nothing"
   assertFails typedef "type Maybe a = Just a | "
   assertFails typedef "type Maybe a = | Nothing"

-- Tests

tests :: Test
tests = TestList 
   [ TestLabel "Parse generic ident" testGenericIdent
   , TestLabel "Parse type ident" testTypeIdent
   , TestLabel "Parse fn ident" testFnIdent
   , TestLabel "Parse var ident" testVarIdent
   , TestLabel "Parse type" testType
   , TestLabel "Parse field" testParseField
   , TestLabel "Parse payload" testParsePayload
   , TestLabel "Parse ctor" testParseCtor
   , TestLabel "Parse typedef" testParseTypeDef
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