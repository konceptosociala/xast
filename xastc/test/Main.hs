{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.HUnit
import Xast.Parser.Ident
import Xast.Parser.Function
import Xast.Parser.Type
import Xast.Parser.Expr
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
   assertParses type' "fn(Int, Bool) -> ()" (TyFn [TyCon (Ident "Int"), TyCon (Ident "Bool")] (TyTuple []))
   assertParses type' "fn(a, Maybe b) -> Bool" (TyFn [TyGnr (Ident "a"), TyApp (TyCon (Ident "Maybe")) (TyGnr (Ident "b"))] (TyCon (Ident "Bool")))
   assertParses type' "fn() -> Int" (TyFn [] (TyCon (Ident "Int")))
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
   assertFails type' "fn(Int, Bool) String"

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
   assertParses typedef "type F = F (fn(Int) -> Bool) Int Bool"
      (TypeDef
         (Ident "F")
         []
         [Ctor
            (Ident "F")
            (PTuple
               [ TyFn [TyCon (Ident "Int")] (TyCon (Ident "Bool"))
               , TyCon (Ident "Int")
               , TyCon (Ident "Bool")
               ]
            )
         ]
      )
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

   let tree = 
         TypeDef 
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
         
   assertParses typedef 
      "type Tree a \
      \   = Node {\
      \      left : Maybe (Tree a),\
      \      right : Maybe (Tree a) \
      \   } \
      \   | Leaf"
      tree
   assertParses typedef 
      "type Tree a = Node { left : Maybe (Tree a), right : Maybe (Tree a) } | Leaf"
      tree
   assertFails typedef
      "type Tree a \
      \   = Node {\
      \      left : Maybe (Tree a),\
      \      right : Maybe (Tree a) \
      \   } \
      \   | Leaf"
      

   assertFails typedef "type Maybe a Just a | Nothing"
   assertFails typedef "type Maybe a = Just a | "
   assertFails typedef "type Maybe a = | Nothing"

testParseFuncDef :: Test
testParseFuncDef = TestCase $ do
   assertParses funcdef "fn myFunc (Int, Maybe a, (Either a b, a)) -> String"
      (FuncDef
         (Ident "myFunc")
         [ TyCon (Ident "Int")
         , TyApp (TyCon (Ident "Maybe")) (TyGnr (Ident "a"))
         , TyTuple 
            [ TyApp 
               (TyApp (TyCon (Ident "Either")) (TyGnr (Ident "a"))) 
               (TyGnr (Ident "b"))
            , TyGnr (Ident "a")
            ]
         ]
         (TyCon (Ident "String"))
      )
   assertParses funcdef "fn doStuff () -> ()"
      (FuncDef
         (Ident "doStuff")
         []
         (TyTuple [])
      )
   assertFails funcdef "fn MyFunc (Int) -> Bool"      -- invalid function name (should be lowercase)
   assertFails funcdef "fn myFunc Int -> Bool"        -- missing parentheses
   assertFails funcdef "fn myFunc (Int, Bool) Bool"   -- missing '->'

testParseLiteral :: Test
testParseLiteral = TestCase $ do
   assertParses literal "\"Hello, World!\"" (LitString "Hello, World!")
   assertParses literal "'c'" (LitChar 'c')
   assertParses literal "42" (LitInt 42)
   assertParses literal "-7" (LitInt (-7))
   assertParses literal "3.14" (LitFloat 3.14)
   assertParses literal "[1, 2, 3]" (LitList [LitInt 1, LitInt 2, LitInt 3])
   assertParses literal "(\"a\", 'b', 3)" (LitTuple [LitString "a", LitChar 'b', LitInt 3])
   assertParses literal "[]" (LitList [])
   assertParses literal "()" (LitTuple [])
   assertParses literal "(12)" (LitInt 12)
   assertParses literal "[12]" (LitList [LitInt 12])
   assertFails literal "\"Unclosed string"
   assertFails literal "'ab'"
   assertFails literal "3.14.15"
   assertFails literal "abc"
   assertFails literal "[)"

testParsePattern :: Test
testParsePattern = TestCase $ do
   assertParses pattern "_" PatWildcard
   assertParses pattern "x" (PatVar (Ident "x"))
   assertParses pattern "Just x" (PatCon (Ident "Just") [PatVar (Ident "x")])
   assertParses pattern "Nothing" (PatCon (Ident "Nothing") [])
   assertParses pattern "(x, y)" (PatTuple [PatVar (Ident "x"), PatVar (Ident "y")])
   assertParses pattern "(x, Just y)" (PatTuple [PatVar (Ident "x"), PatCon (Ident "Just") [PatVar (Ident "y")]])
   assertParses pattern "[1, 2, 3]"
      (PatLit
         (LitList 
            [ LitInt 1
            , LitInt 2
            , LitInt 3
            ]
         )
      )
   assertParses pattern "[1, 2, x]"
      (PatList 
         [ PatLit (LitInt 1)
         , PatLit (LitInt 2)
         , PatVar (Ident "x")
         ]
      )
   assertParses pattern "(a, _, \"b\", True)" 
      (PatTuple 
         [ PatVar (Ident "a")
         , PatWildcard
         , PatLit (LitString "b")
         , PatCon (Ident "True") []
         ]
      )
   assertParses pattern "[]" (PatLit (LitList []))
   assertParses pattern "[x, 1, y]" 
      (PatList 
         [ PatVar (Ident "x")
         , PatLit (LitInt 1)
         , PatVar (Ident "y")
         ]
      )
   assertParses pattern "\"abc\"" (PatLit (LitString "abc"))
   assertParses pattern "42" (PatLit (LitInt 42))
   assertParses pattern "True" (PatCon (Ident "True") [])
   assertFails pattern "(a, b"
   assertFails pattern "[x, y"
   assertFails pattern "\"abc"

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
   , TestLabel "Parse funcdef" testParseFuncDef
   , TestLabel "Parse literals" testParseLiteral
   , TestLabel "Parse patterns" testParsePattern
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