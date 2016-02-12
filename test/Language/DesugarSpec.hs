module Language.DesugarSpec (spec) where

import Test.Hspec

import Language.Ast
import Language.Desugar

import Data.Map.Strict as M

emptyMap = MappyMap $ StandardMap M.empty

cons e r = MappyMap $ StandardMap $ M.fromList [
  (MappyKeyword "head",  e)
  , (MappyKeyword "tail", r)
  ]

typeHint (MappyMap (StandardMap map)) = M.lookup (MappyKeyword "__type") map

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
    describe "given a string" $ do
      let
        string = ExprSugar $ SugaredString "012"
        desugared = desugarExpr string

      it "desugars to a list, having the correct elements" $ do
        desugared `shouldBe`
          (cons (mappyChar '0') $ cons (mappyChar '1') $ cons (mappyChar '2') emptyMap)

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

      it "desugars to an empty map" $ do
        desugarExpr code `shouldBe` emptyMap

    describe "given a sugared list with some values" $ do
      let code = ExprSugar $ SugaredList [MappyKeyword "foo", MappyNamedValue "bar"]

      it "desugars to a head tail structure, ending in the empty map" $ do
        desugarExpr code `shouldBe` (cons (MappyKeyword "foo") $ cons (MappyNamedValue "bar") emptyMap)

    describe "given a sugared list a nested sugared list" $ do
      let code = ExprSugar $ SugaredList [ExprSugar $ SugaredList []]

      it "desugars all lists" $ do
        desugarExpr code `shouldBe` (cons emptyMap emptyMap)
