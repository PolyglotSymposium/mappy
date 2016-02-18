module Language.Ast.PrettyPrinterSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as M

import Language.Ast
import Language.Ast.PrettyPrinter()
import Language.PrettyPrintable

spec :: Spec
spec = do
  describe "pretty" $ do
    describe "given a boolean 'true' map" $ do
      let m = MappyMap $ StandardMap $ M.singleton (MappyKeyword "truthy") (MappyKeyword "true")

      it "pretty prints as 'true'" $ do
        pretty m `shouldBe` "true"

    describe "given a boolean 'false map" $ do
      let m = MappyMap $ StandardMap $ M.singleton (MappyKeyword "truthy") (MappyKeyword "false")

      it "pretty prints as 'false'" $ do
        pretty m `shouldBe` "false"

    describe "given a map with a truthy key without the true value" $ do
      let m = MappyMap $ StandardMap $ M.singleton (MappyKeyword "truthy") (MappyKeyword "foo")

      it "pretty prints as that map" $ do
        pretty m `shouldBe` "(:truthy :foo)"
