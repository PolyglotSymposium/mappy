module MappySpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Mappy
import Text.ParserCombinators.Parsec (parse)

spec :: Spec
spec = do
  describe "parsing" $ do
    describe "expressions" $ do
      let parseExpression = parse expression ""

      describe "a single map" $ do
        it "works" $ do
          parseExpression "()" `shouldBe` Right MappyMap

main :: IO ()
main = putStrLn "Hello there"
