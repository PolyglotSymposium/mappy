module Language.DesugarSpec (spec) where

import Test.Hspec

import Language.Ast
import Language.Desugar

cons e r = MappyApp (MappyNamedValue "cons") [e, r]

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
