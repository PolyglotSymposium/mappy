module Language.DesugarSpec (spec) where

import Test.Hspec

import Language.Ast
import Language.Desugar

import Data.Map.Strict as M

cons e r = MappyApp (MappyNamedValue "cons") [e, r]

hasKeyAllTheWayDown (MappyMap (StandardMap map)) key value nextKey =
  let
    hasKey = M.lookup key map == Just value
  in
    case M.lookup nextKey map of
      Just next -> hasKey && hasKeyAllTheWayDown next key value nextKey
      Nothing -> hasKey

hasDepthDownKey (MappyMap (StandardMap map)) depth nextKey =
  case M.lookup nextKey map of
    Just next -> hasDepthDownKey next (depth - 1) nextKey
    Nothing -> depth == 0

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
    describe "given a character" $ do
      let
        char = ExprSugar $ SugaredChar $ '0'
        desugared = desugarExpr char

      it "desugars as a nat, with the same value" $ do
        hasDepthDownKey desugared 48 (MappyKeyword "pred") `shouldBe` True

      it "desugars as a nat, with the :__type :char key all the way down" $ do
        hasKeyAllTheWayDown desugared (MappyKeyword "__type") (MappyKeyword "char") (MappyKeyword "pred") `shouldBe` True

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
