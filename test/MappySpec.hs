module MappySpec (main, spec) where

import qualified Data.Map as M
import Data.List (intercalate)
import Text.ParserCombinators.Parsec (parse)
import Data.Either (isLeft)
import Data.Foldable

import Test.Hspec
import Test.QuickCheck

import Mappy
import Language.Ast

data ArbitraryValidKeywordName =
  ValidIdentifier String
  deriving (Show)

instance Arbitrary ArbitraryValidKeywordName where
  arbitrary = do
    let validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_/-+<>!@#$%^&*;'\",.?="
    text <- arbitrary
    firstChar <- elements validChars
    let filtered = filter (`elem` validChars) text
    return $ ValidIdentifier (firstChar:filtered)

common_examples = [
    ("a keyword", ":some-keyword", MappyKeyword "some-keyword")
    ,("a map", "(:foo :bar)", MappyMap $ M.singleton (MappyKeyword "foo") (MappyKeyword "bar"))
    ,("an application", "[foo ()]", MappyApp (MappyNamedValue "foo") [MappyMap M.empty])
    ,("a named value", "foo", MappyNamedValue "foo")
    ,("a lamba function", "\\x -> x", MappyLambda [MappyNamedValue "x"] $ MappyNamedValue "x")
  ]

expected_if_def = Right
  (MappyDef
    (MappyNamedValue "if")
    (MappyLambda
      [MappyNamedValue "cond", MappyNamedValue "then", MappyNamedValue "else"]
      (MappyApp
        (MappyNamedValue "default-take")
        [MappyApp
          (MappyNamedValue "take")
          [MappyKeyword "truthy", MappyNamedValue "cond"],
        MappyMap (M.fromList [(MappyKeyword "false",MappyNamedValue "else")]),
        MappyNamedValue "then"])))

example_src_file = "a = :a\nb = :b\n\nc = \\x -> x"

spec :: Spec
spec = do
  describe "code files" $ do
    let parseFile = parse file ""

    describe "the empty file" $ do
      it "parses to nothing" $ do
        parseFile "" `shouldBe` Right []

    describe "a file beginning with whitespace, having a single definition" $ do
      it "Parses that single definition" $ do
        length <$> parseFile "\n\nid = \\x -> x\n" `shouldBe` Right 1

    describe "a file with a single definition" $ do
      it "Parses that single definition" $ do
        length <$> parseFile "id = \\x -> x\n" `shouldBe` Right 1

    describe "a file with a single definition and no newline at the end" $ do
      it "Parses that single definition" $ do
        length <$> parseFile "id = \\x -> x" `shouldBe` Right 1

    describe "a file with multiple definitions" $ do
      it "Parses them all" $ do
        length <$> parseFile example_src_file `shouldBe` Right 3

  describe "definition text" $ do
    let parseDefinition = parse definition ""

    for_ common_examples $ \(typee, boundValue, expected) ->
      describe ("binding a name to " ++ typee) $ do
        it "parses correctly" $ do
          parseDefinition ("a = " ++ boundValue) `shouldBe` Right (MappyDef (MappyNamedValue "a") expected)
    describe "binding a non-name" $ do
        it "fails to parse" $ do
          parseDefinition (":foo = :bar") `shouldSatisfy` isLeft

    describe "the if function" $ do
      let fn = "if = \\cond then else -> [default-take [take :truthy cond] (:false else) then]"

      it "parses correctly" $ do
        parseDefinition fn `shouldBe` expected_if_def

  describe "expression text" $ do
    let parseExpression = parse expression ""

    describe "when parsing nested lambas" $ do
      it "parses correctly" $ do
        parseExpression "\\x -> \\y -> x" `shouldBe` Right (MappyLambda [MappyNamedValue "x"] (MappyLambda [MappyNamedValue "y"] $ MappyNamedValue "x"))

    describe "when parsing a lambda function" $ do
      describe "binding a non-name" $ do
          it "fails to parse" $ do
            parseExpression ("\\:foo -> :bar") `shouldSatisfy` isLeft

      describe "whose lambda an first param are space separated" $ do
        it "parses correctly" $ do
          parseExpression "\\ x -> :foo" `shouldBe` Right (MappyLambda [MappyNamedValue "x"] (MappyKeyword "foo"))

      for_ common_examples $ \(typee, body, expected) ->
        describe ("whose body is " ++ typee) $ do
          it "parses correctly" $ do
            parseExpression ("\\x -> " ++ body) `shouldBe` Right (MappyLambda [MappyNamedValue "x"] expected)

    describe "when parsing function applications" $ do
      describe "with whitespace on the ends" $ do
        it "parses correctly" $ do
          property $ \(ValidIdentifier name) ->
            parseExpression ("[  " ++ name ++ "  ]") `shouldBe` Right (MappyApp (MappyNamedValue name) [])

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
