module Language.ExecutorSpec (spec) where

import Test.Hspec

import Language.Ast
import Language.AstExamples
import qualified Language.Env as E
import Language.Error
import Language.Executor

import qualified Data.Map.Strict as M

simple_def n val = MappyDef (name n) (kwd val)

def_main = MappyDef (name "main")

truthy_value =
  MappyMap $ StandardMap $ M.singleton (kwd "truthy") (kwd "true")
falsey_value =
  MappyMap $ StandardMap $ M.singleton (kwd "truthy") (kwd "false")

kwd = MappyKeyword
name = MappyNamedValue
appl = MappyApp

empty_map = MappyMap $ StandardMap $ M.empty
single_map a = MappyMap . StandardMap . M.singleton a

spec :: Spec
spec = do
  describe "exec" $ do
    describe "given a sugared function def" $ do
      let
        code = [
          DefSugar (SugaredFnDefinition (name "first") [
            name "a"
            , name "b"
            ] $ name "a")
          , def_main $ appl (name "first") [kwd "a", kwd "b"]
          ]

      it "runs the function as if it were simply defined as a lambda" $ do
        exec code `shouldBe` Right (kwd "a")

    describe "a named value that refers to another named value" $ do
      let
        code = [
          def_main $ (name "a"),
          simple_def "b" "c",
          MappyDef (name "a") (name "b")
          ]

      it "reduces until the final value" $ do
        exec code `shouldBe` Right (kwd "c")

    describe "the application of a lambda" $ do
      let
        lambda = MappyLambda (map name ["a", "b"]) $ name "a"
        code = [
          def_main $ appl lambda [kwd "a", kwd "b"]
          ]

      it "binds the arguments and evaluates the body" $ do
        exec code `shouldBe` (Right $ kwd "a")

    describe "the application of a non-function or keyword" $ do
      let
        code appliedTo = [
          MappyDef (name "a") empty_map,
          def_main $ appl (name "a") [kwd "b"]
          ]

      it "errors saying that the value is not a function" $ do
        exec (code map) `shouldBe` (Left [NotAFunction empty_map])

    describe "the application of a keyword" $ do
      let
        map = single_map (kwd "a") (kwd "b")
        code appliedTo = [
          def_main $ appl (kwd "a") [appliedTo]
          ]

      it "applies as `take` applies" $ do
        exec (code map) `shouldBe` Right (kwd "b")

      describe "to a non-map" $ do
        it "errors with a take called on non-map error" $ do
          exec (code $ kwd "b") `shouldBe` (Left [TakeCalledOnNonMap "take" (kwd "a") (kwd "b")])

    describe "the application of a lambda" $ do
      let lambda = MappyDef (name "const") (MappyLambda [name "a", name "b"] (name "a"))

      describe "with too few arguments" $ do
        let
          code = [
            lambda
            , def_main $ appl (name "const") [kwd "foo"]
            ]

          (Right (MappyClosure argNames body (newEnvEntry:_))) = exec code

        it "reduces the number of arguments down by the number applied" $ do
          argNames `shouldBe` [name "b"]

        it "keeps the same function body" $ do
          body `shouldBe` name "a"

        it "extends the closure's environment with the applied argument name and value" $ do
          newEnvEntry `shouldBe` E.NamePair (name "a", kwd "foo")

      describe "with too many arguments" $ do
        let
          code fn = [
            lambda
            , def_main $ appl fn [kwd "a", kwd "b", kwd "c"]
            ]

        describe "and a named function" $ do
          it "errors with a WrongNumberOfArguments, carrying the name forward" $ do
            exec (code $ name "const") `shouldBe` Left [WrongNumberOfArguments "const" 2 3]

        describe "and an anonymous function" $ do
          it "errors with a WrongNumberOfArguments giving a generic name to the function" $ do
            exec (code $ MappyLambda [] $ kwd "a") `shouldBe` Left [WrongNumberOfArguments "#closure#" 0 3]

      describe "with the correct number of argument" $ do
        let
          code = [
            lambda
            , def_main $ appl (name "const") [kwd "a", kwd "b"]
            ]

        it "applies it as a function" $ do
          exec code `shouldBe` Right (kwd "a")

    describe "give" $ do
      describe "given arguments whose names are not bound" $ do
        let code = [def_main $ appl (name "give") [name "a", name "b", name "c"]]

        it "returns an error for each argument" $ do
          exec code `shouldBe` Left (map NameNotDefined ["a", "b", "c"])

      describe "given the wrong number of arguments" $ do
        let
          code = [
            def_main $ appl (name "give") [kwd "a"]
            ]

        it "errors with a WrongNumberOfArguments error" $ do
          exec code `shouldBe` Left [WrongNumberOfArguments "give" 3 1]

      describe "given a new key and value" $ do
        let
          code = [
            def_main $ appl (name "give") [kwd "a", kwd "b", empty_map]
            ]

        it "returns a map with the new key" $ do
          exec code `shouldBe` Right (single_map (kwd "a") (kwd "b"))

      describe "given an existing key" $ do
        let
          map = single_map (kwd "a") (kwd "b")
          code = [
            def_main $ appl (name "give") [kwd "a", kwd "c", map]
            ]

        it "overwrites the old value at the key" $ do
          exec code `shouldBe` Right (single_map (kwd "a") (kwd "c"))

      describe "given a new key and a non-map" $ do
        let
          code = [
            def_main $ appl (name "give") [kwd "a", kwd "b", kwd "c"]
            ]

        it "fails with a GiveCalledOnNonMap error" $ do
          exec code `shouldBe` Left [GiveCalledOnNonMap (kwd "a") (kwd "b") (kwd "c")]

    describe "default-take" $ do
      describe "given arguments whose names are not bound" $ do
        let code = [def_main $ appl (name "default-take") [name "a", name "b", name "c"]]

        it "returns an error for each argument" $ do
          exec code `shouldBe` Left (map NameNotDefined ["a", "b", "c"])

      describe "given a non-map as the second argument" $ do
        let code = [def_main $ appl (name "default-take") [kwd "a", kwd "b", kwd "c"]]

        it "errors with a take called on a non-map error" $ do
          exec code `shouldBe` (Left [TakeCalledOnNonMap "default-take" (kwd "a") (kwd "b")])

      describe "given a map with an erroring default value" $ do
        let
          code lookup = [
            def_main $ appl (name "default-take") [
              kwd lookup,
              single_map (kwd "a") (kwd "b"),
              name "name-not-known"]
            ]

        describe "when looking up a key that is not in the map" $ do
          let code' = code "b"

          it "errors because the default key cannot be found" $ do
            exec code' `shouldBe` Left [NameNotDefined "name-not-known"]

      describe "given a map with an erring value" $ do
        let
          code lookup = [
            def_main $ appl (name "default-take") [
              kwd lookup,
              MappyMap $ StandardMap $ M.fromList [
                (kwd "a", name "name-not-known"),
                (kwd "b", kwd "b-value")],
              kwd "default"]
            ]

        describe "when looking up the key of the erring value" $ do
          let code' = code "a"

          it "errors as well" $ do
            exec code' `shouldBe` Left [NameNotDefined "name-not-known"]

      describe "given the wrong number of arguments" $ do
        let
          code = [
            def_main $ appl (name "default-take") [kwd "a"]
            ]

        it "errors with a WrongNumberOfArguments error" $ do
          exec code `shouldBe` Left [WrongNumberOfArguments "default-take" 3 1]

      describe "given a key that\'s not in the map and a default" $ do
        let
          map = single_map (kwd "a") (kwd "b")
          code = [
            def_main $ appl (name "default-take") [kwd "not", map, kwd "default"]
            ]

        it "returns the default" $ do
          exec code `shouldBe` Right (kwd "default")

      describe "given a key that's in the map" $ do
        let
          map = single_map (kwd "a") (kwd "b")
          code = [
            def_main $ appl (name "default-take") [kwd "a", map, kwd "default"]
            ]

        it "finds the key in the map" $ do
          exec code `shouldBe` Right (kwd "b")

    describe "take" $ do
      describe "given arguments whose names are not bound" $ do
        let code = [def_main $ appl (name "take") [name "a", name "b"]]

        it "returns an error for each argument" $ do
          exec code `shouldBe` Left (map NameNotDefined ["a", "b"])

      describe "given the wrong number of arguments" $ do
        let
          map = single_map (kwd "a") (kwd "b")
          code = [
            def_main $ appl (name "take") [kwd "a"]
            ]

        it "errors with a WrongNumberOfArguments error" $ do
          exec code `shouldBe` Left [WrongNumberOfArguments "take" 2 1]

      describe "given a key that's not in the map" $ do
        let
          map = single_map (kwd "a") (kwd "b")
          code = [
            def_main $ appl (name "take") [kwd "not", map]
            ]

        it "errors with a KeyNotFound error" $ do
          exec code `shouldBe` Left [KeyNotFound (kwd "not") map]

      describe "given the correct arguments" $ do
        let
          map = single_map (kwd "a") (kwd "b")
          code = [
            def_main $ appl (name "take") [kwd "a", map]
            ]

        it "finds the key in the map" $ do
          exec code `shouldBe` Right (kwd "b")

    describe "given main simply binds to something else in the environment" $ do
      let
        code = [
          simple_def "foo" "bar",
          def_main (name "foo")
          ]

      it "evaluates to whatever that name evaluates to when looked up" $ do
        exec code `shouldBe` Right (kwd "bar")

    describe "given main is simply a map" $ do
      let main = [def_main map]
          map = single_map (kwd "a") (kwd "b")

      it "evaluates to just that map" $ do
        exec main `shouldBe` Right map

    describe "given main is simply a keyword" $ do
      let main = [def_main (kwd "foobar")]

      it "evaluates to just that keyword" $ do
        exec main `shouldBe` Right (kwd "foobar")

    describe "given a lot of repeats, including main" $ do
      let
        defs = [
          simple_def "foo" "bar",
          simple_def "foo" "bar",
          simple_def "spaz" "bar",
          simple_def "spaz" "bar",
          simple_def "main" "spaz",
          simple_def "main" "spaz"]

      it "errors with multiple repeated definition errors" $ do
        exec defs `shouldBe` Left [RepeatedDefinition "main", RepeatedDefinition "spaz", RepeatedDefinition "foo"]

    describe "given some definitions that are repeated, with a main" $ do
      let defs = [simple_def "foo" "bar", simple_def "foo" "bar", simple_def "main" "spaz"]

      it "errors with a repeated definition error" $ do
        exec defs `shouldBe` Left [RepeatedDefinition "foo"]

    describe "given some definitions without a main" $ do
      let defs = [MappyDef (name "foo") (kwd "bar")]

      it "errors with a main not found error" $ do
        exec defs `shouldBe` Left [MainNotFound]
