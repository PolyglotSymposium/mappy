module MappySpec (main, spec) where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec (parse)
import Data.Either (isLeft)

import Test.Hspec
import Test.QuickCheck

import Mappy

data ArbitraryValidKeywordName =
  ValidKeywordName String
  deriving (Show)

instance Arbitrary ArbitraryValidKeywordName where
  arbitrary = do
    let validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_/-+<>!@#$%^&*\\;'\",.?"
    text <- arbitrary
    firstChar <- elements validChars
    let filtered = filter (`elem` validChars) text
    return $ ValidKeywordName (firstChar:filtered)

spec :: Spec
spec = do
  describe "the text for the expression" $ do
    let parseExpression = parse expression ""

    describe "when parsing maps" $ do
      describe "the empty map" $ do
        it "parses correctly" $ do
          parseExpression "()" `shouldBe` Right (MappyMap M.empty)

      describe "a map containing" $ do
        describe "a single association of keywords" $ do
          it "parses correctly" $ do
            parseExpression "(:a :b)" `shouldBe` Right (MappyMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b"))

        describe "a single association of maps" $ do
          it "parses correctly" $ do
            parseExpression "(() ())" `shouldBe` Right (MappyMap $ M.singleton (MappyMap M.empty) (MappyMap M.empty))

        describe "a single association of a map to a keyword" $ do
          it "parses correctly" $ do
            parseExpression "(() :foobar)" `shouldBe` Right (MappyMap $ M.singleton (MappyMap M.empty) (MappyKeyword "foobar"))

    describe "a single keyword" $ do
      it "parses correctly" $ do
        property $ \(ValidKeywordName text) -> parseExpression (':':text) `shouldBe` Right (MappyKeyword text)

    describe "a single named value" $ do
      it "parses correctly" $ do
        property $ \(ValidKeywordName text) -> parseExpression text `shouldBe` Right (MappyNamedValue text)

    describe "when parsing various empty values" $ do
      describe "the \"empty keyword\"" $ do
        it "is not a valid expression" $ do
          parseExpression ":" `shouldSatisfy` isLeft

      describe "the empty string" $ do
        it "is not a valid expression" $ do
          parseExpression "" `shouldSatisfy` isLeft

      describe "the empty application" $ do
        it "is not a valid expression" $ do
          parseExpression "[]" `shouldSatisfy` isLeft

main = putStrLn "Hello there"
