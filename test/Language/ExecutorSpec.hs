module Language.ExecutorSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "foobar" $ do
    it "works" $ do
      4 `shouldBe` 5
