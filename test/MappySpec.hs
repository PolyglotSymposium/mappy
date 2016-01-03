module MappySpec (main, spec) where

import qualified Data.Map as M
import Data.List (intercalate)
import Text.ParserCombinators.Parsec (parse)
import Data.Either (isLeft)
import Data.Foldable

import Test.Hspec
import Test.QuickCheck

import Mappy

data ArbitraryValidKeywordName =
  ValidIdentifier String
  deriving (Show)

instance Arbitrary ArbitraryValidKeywordName where
  arbitrary = do
    let validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_/-+<>!@#$%^&*\\;'\",.?="
    text <- arbitrary
    firstChar <- elements validChars
    let filtered = filter (`elem` validChars) text
    return $ ValidIdentifier (firstChar:filtered)

binding_examples = [
    ("a keyword", ":some-keyword", MappyKeyword "some-keyword")
    ,("a map", "(:foo :bar)", MappyMap $ M.singleton (MappyKeyword "foo") (MappyKeyword "bar"))
    ,("an application", "[foo ()]", MappyApp (MappyNamedValue "foo") [MappyMap M.empty])
    ,("a named value", "foo", MappyNamedValue "foo")
  ]

spec :: Spec
spec = do
  describe "the text for definitions" $ do
    let parseDefinition = parse definition ""

    for_ binding_examples $ \(typee, boundValue, expected) ->
      describe ("binding a name to " ++ typee) $ do
        it "parses correctly" $ do
          parseDefinition ("a = " ++ boundValue) `shouldBe` Right (MappyDef (MappyNamedValue "a") expected)

  describe "the text for the expression" $ do
    let parseExpression = parse expression ""

    describe "when parsing function applications" $ do
      describe "with a single named value application" $ do
        it "parses correctly" $ do
          property $ \(ValidIdentifier name) ->
            parseExpression ("[" ++ name ++ "]") `shouldBe` Right (MappyApp (MappyNamedValue name) [])

      describe "with an arbitrary length of arguments" $ do
        it "parses correctly" $ do
          property $ \(Positive n) (ValidIdentifier name) ->
            let
              nArgs = replicate n name
              args = intercalate " " nArgs
            in
              parseExpression ("[" ++ name ++ " " ++ args ++ "]") `shouldBe` Right (MappyApp (MappyNamedValue name) $ map MappyNamedValue nArgs)

      describe "a map as the function" $ do
        it "fails to parse" $ do
          parseExpression "[()]" `shouldSatisfy` isLeft

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
        property $ \(ValidIdentifier text) -> parseExpression (':':text) `shouldBe` Right (MappyKeyword text)

    describe "a single named value" $ do
      it "parses correctly" $ do
        property $ \(ValidIdentifier text) -> parseExpression text `shouldBe` Right (MappyNamedValue text)

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
