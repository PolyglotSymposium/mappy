module Language.DesugarSpec (spec) where

import Test.Hspec

import Language.Ast
import Language.Desugar

import Data.Map.Strict as M

cons e r = MappyApp (MappyNamedValue "cons") [e, r]

typeHint (MappyMap (StandardMap map)) = M.lookup (MappyKeyword "__type") map

hasDepthDownKey (MappyMap (StandardMap map)) depth nextKey =
  case M.lookup nextKey map of
    Just next -> hasDepthDownKey next (depth - 1) nextKey
    Nothing -> depth == 0

hasNCons (MappyApp (MappyNamedValue "cons") [_, v]) depth = hasNCons v $ depth - 1
hasNCons (MappyNamedValue "nil") depth = depth == 0
hasNCons a _ = False

hasCharsDown (MappyApp (MappyNamedValue "cons") [v, rest]) (v':vs) =
  typeHint v == Just (MappyKeyword "char") &&
  hasDepthDownKey v v' (MappyKeyword "pred")  &&
  hasCharsDown rest vs
hasCharsDown (MappyNamedValue "nil") _ = True
hasCharsDown  _ _ = False

spec :: Spec
spec = do
  describe "desugarDef" $ do
    describe "give a sugared function definition" $ do
      let
        fnName = MappyNamedValue "fnName"
        argNames = [MappyNamedValue "param1", MappyLazyArgument "params 2"]
        body = MappyKeyword "body"
        def = SugaredFnDefinition fnName argNames body

      it "desugars to a definition of a lambda" $ do
        desugarDef def `shouldBe` MappyDef fnName (MappyLambda argNames body)

  describe "desugarExpr" $ do
    describe "given a string" $ do
      let
        string = ExprSugar $ SugaredString "012"
        desugared = desugarExpr string

      it "desugars to a list, having the correct number of elements" $ do
        hasNCons desugared 3 `shouldBe` True

      it "desugars to a list, having the correct elements" $ do
        hasCharsDown desugared [48, 49, 50] `shouldBe` True

    describe "given a character" $ do
      let
        char = ExprSugar $ SugaredChar '0'
        desugared = desugarExpr char

      it "desugars as a nat, with the same value" $ do
        hasDepthDownKey desugared 48 (MappyKeyword "pred") `shouldBe` True

      it "desugars as a nat, with the :__type :char key at the top level" $ do
        typeHint desugared `shouldBe` (Just $ MappyKeyword "char")

    describe "given an empty sugared list" $ do
      let code = ExprSugar $ SugaredList []

      it "desugars to nil" $ do
        desugarExpr code `shouldBe` MappyNamedValue "nil"

    describe "given a sugared list with some values" $ do
      let code = ExprSugar $ SugaredList [MappyKeyword "foo", MappyNamedValue "bar"]

      it "desugars to cons applied ending in nil" $ do
        desugarExpr code `shouldBe` (cons (MappyKeyword "foo") $ cons (MappyNamedValue "bar") (MappyNamedValue "nil"))

    describe "given a sugared list a nested sugared list" $ do
      let code = ExprSugar $ SugaredList [ExprSugar $ SugaredList []]

      it "desugars all lists" $ do
        desugarExpr code `shouldBe` (cons (MappyNamedValue "nil") (MappyNamedValue "nil"))
