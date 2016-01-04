module Language.ExecutorSpec (spec) where

import Test.Hspec

import Language.Ast
import Language.Executor

spec :: Spec
spec = do
  describe "exec" $ do
    describe "given some definitions without a main" $ do
      let defs = [MappyDef (MappyNamedValue "foo") (MappyKeyword "bar")]

      it "errors with a main not found error" $ do
        exec defs `shouldBe` Left [MainNotFound]
