module Language.ExecutorSpec (spec) where

import Test.Hspec

import Language.Ast
import Language.AstExamples
import qualified Language.Env as E
import Language.Error
import Language.Executor

import qualified Data.Map.Strict as M

simple_def name val = MappyDef (MappyNamedValue name) (MappyKeyword val)
def_main = MappyDef (MappyNamedValue "main")
truthy_value = MappyMap $ StandardMap $ M.singleton (MappyKeyword "truthy") (MappyKeyword "true")
falsey_value = MappyMap $ StandardMap $ M.singleton (MappyKeyword "truthy") (MappyKeyword "false")

spec :: Spec
spec = do
  describe "exec" $ do
    describe "given a sugared function def" $ do
      let
        code = [
          DefSugar (SugaredFnDefinition (MappyNamedValue "first") [
            MappyNamedValue "a"
            , MappyNamedValue "b"
            ] $ MappyNamedValue "a")
          , def_main $ MappyApp (MappyNamedValue "first") [MappyKeyword "a", MappyKeyword "b"]
          ]

      it "runs the function as if it were simply defined as a lambda" $ do
        exec code `shouldBe` Right (MappyKeyword "a")

    describe "a named value that refers to another named value" $ do
      let
        code = [
          def_main $ (MappyNamedValue "a"),
          simple_def "b" "c",
          MappyDef (MappyNamedValue "a") (MappyNamedValue "b")
          ]

      it "reduces until the final value" $ do
        exec code `shouldBe` Right (MappyKeyword "c")

    describe "the application of a lambda" $ do
      let
        lambda = MappyLambda (map MappyNamedValue ["a", "b"]) $ MappyNamedValue "a"
        code = [
          def_main $ MappyApp lambda [MappyKeyword "a", MappyKeyword "b"]
          ]

      it "executes correctly" $ do
        exec code `shouldBe` (Right $ MappyKeyword "a")

    describe "the application of a non-function or keyword" $ do
      let
        code appliedTo = [
          MappyDef (MappyNamedValue "a") $ MappyMap $ StandardMap M.empty,
          def_main $ MappyApp (MappyNamedValue "a") [MappyKeyword "b"]
          ]

      it "errors saying that the keyword is not a function" $ do
        exec (code map) `shouldBe` (Left [NotAFunction $ MappyMap $ StandardMap $ M.empty])

    describe "the application of a keyword" $ do
      let
        map = MappyMap $ StandardMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
        code appliedTo = [
          def_main $ MappyApp (MappyKeyword "a") [appliedTo]
          ]

      it "applies as `take` applies" $ do
        exec (code map) `shouldBe` Right (MappyKeyword "b")

      describe "to a non-map" $ do
        it "errors" $ do
          exec (code $ MappyKeyword "b") `shouldBe` (Left [TakeCalledOnNonMap "take" (MappyKeyword "a") (MappyKeyword "b")])

    describe "the application of a lambda" $ do
      let lambda = MappyDef (MappyNamedValue "const") (MappyLambda [MappyNamedValue "a", MappyNamedValue "b"] (MappyNamedValue "a"))

      describe "with too few arguments" $ do
        let
          code = [
            lambda
            , def_main $ MappyApp (MappyNamedValue "const") [MappyKeyword "foo"]
            ]

          (Right (MappyClosure argNames body (newEnvEntry:_))) = exec code

        it "reduces the number of arguments down by the number applied" $ do
          argNames `shouldBe` [MappyNamedValue "b"]

        it "keeps the same function body" $ do
          body `shouldBe` MappyNamedValue "a"

        it "extends the closure's environment with the applied argument name and value" $ do
          newEnvEntry `shouldBe` E.NamePair (MappyNamedValue "a", MappyKeyword "foo")

      describe "with too many arguments" $ do
        let
          code fn = [
            lambda
            , def_main $ MappyApp fn [MappyKeyword "a", MappyKeyword "b", MappyKeyword "c"]
            ]

        describe "and a named function" $ do
          it "errors with a WrongNumberOfArguments, carrying the name forward" $ do
            exec (code $ MappyNamedValue "const") `shouldBe` Left [WrongNumberOfArguments "const" 2 3]

        describe "and an anonymous function" $ do
          it "errors with a WrongNumberOfArguments giving a generic name to the function" $ do
            exec (code $ MappyLambda [] $ MappyKeyword "a") `shouldBe` Left [WrongNumberOfArguments "#closure#" 0 3]

      describe "with the correct number of argument" $ do
        let
          code = [
            lambda
            , def_main $ MappyApp (MappyNamedValue "const") [MappyKeyword "a", MappyKeyword "b"]
            ]

        it "applies it as a function" $ do
          exec code `shouldBe` Right (MappyKeyword "a")

    describe "a lambda" $ do
      describe "given a new key and value" $ do
        let
          map = MappyMap $ StandardMap M.empty
          code = [
            def_main $ MappyApp (MappyNamedValue "give") [MappyKeyword "a", MappyKeyword "b", map]
            ]

        it "returns a map with the new key" $ do
          exec code `shouldBe` Right (MappyMap $ StandardMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b"))

    describe "give" $ do
      describe "given undefined arguments" $ do
        let code = [def_main $ MappyApp (MappyNamedValue "give") [MappyNamedValue "a", MappyNamedValue "b", MappyNamedValue "c"]]

        it "returns an error for each argument" $ do
          exec code `shouldBe` Left (map NameNotDefined ["a", "b", "c"])

      describe "given the wrong number of arguments" $ do
        let
          code = [
            def_main $ MappyApp (MappyNamedValue "give") [MappyKeyword "a"]
            ]

        it "errors with a WrongNumberOfArguments error" $ do
          exec code `shouldBe` Left [WrongNumberOfArguments "give" 3 1]

      describe "given a new key and value" $ do
        let
          map = MappyMap $ StandardMap M.empty
          code = [
            def_main $ MappyApp (MappyNamedValue "give") [MappyKeyword "a", MappyKeyword "b", map]
            ]

        it "returns a map with the new key" $ do
          exec code `shouldBe` Right (MappyMap $ StandardMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b"))

      describe "given an existing key" $ do
        let
          map = MappyMap $ StandardMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
          code = [
            def_main $ MappyApp (MappyNamedValue "give") [MappyKeyword "a", MappyKeyword "c", map]
            ]

        it "overwrites the old value at the key" $ do
          exec code `shouldBe` Right (MappyMap $ StandardMap $ M.singleton (MappyKeyword "a") (MappyKeyword "c"))

      describe "given a new key and a non-map" $ do
        let
          code = [
            def_main $ MappyApp (MappyNamedValue "give") [MappyKeyword "a", MappyKeyword "b", MappyKeyword "c"]
            ]

        it "fails with a GiveCalledOnNonMap error" $ do
          exec code `shouldBe` Left [GiveCalledOnNonMap (MappyKeyword "a") (MappyKeyword "b") (MappyKeyword "c")]

    describe "default-take" $ do
      describe "given undefined arguments" $ do
        let code = [def_main $ MappyApp (MappyNamedValue "default-take") [MappyNamedValue "a", MappyNamedValue "b", MappyNamedValue "c"]]

        it "returns an error for each argument" $ do
          exec code `shouldBe` Left (map NameNotDefined ["a", "b", "c"])

      describe "given a non-map as the second argument" $ do
        let code = [def_main $ MappyApp (MappyNamedValue "default-take") [MappyKeyword "a", MappyKeyword "b", MappyKeyword "c"]]

        it "errors" $ do
          exec code `shouldBe` (Left [TakeCalledOnNonMap "default-take" (MappyKeyword "a") (MappyKeyword "b")])
      describe "given a map with an erroring default value" $ do
        let
          code lookup = [
            def_main $ MappyApp (MappyNamedValue "default-take") [
              MappyKeyword lookup,
              MappyMap $ StandardMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b"),
              MappyNamedValue "name-not-known"]
            ]

        describe "when looking up a key that is not in the map" $ do
          let code' = code "b"

          it "errors because the default key cannot be found" $ do
            exec code' `shouldBe` Left [NameNotDefined "name-not-known"]

      describe "given a map with an erroring value" $ do
        let
          code lookup = [
            def_main $ MappyApp (MappyNamedValue "default-take") [
              MappyKeyword lookup,
              MappyMap $ StandardMap $ M.fromList [
                (MappyKeyword "a", MappyNamedValue "name-not-known"),
                (MappyKeyword "b", MappyKeyword "b-value")],
              MappyKeyword "default"]
            ]

        describe "when looking up the key of the erroring value" $ do
          let code' = code "a"

          it "errors" $ do
            exec code' `shouldBe` Left [NameNotDefined "name-not-known"]

      describe "given the wrong number of arguments" $ do
        let
          code = [
            def_main $ MappyApp (MappyNamedValue "default-take") [MappyKeyword "a"]
            ]

        it "errors with a WrongNumberOfArguments error" $ do
          exec code `shouldBe` Left [WrongNumberOfArguments "default-take" 3 1]

      describe "given a key that\'s not in the map and a default" $ do
        let
          map = MappyMap $ StandardMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
          code = [
            def_main $ MappyApp (MappyNamedValue "default-take") [MappyKeyword "not", map, MappyKeyword "default"]
            ]

        it "returns the default" $ do
          exec code `shouldBe` Right (MappyKeyword "default")

      describe "given a key that\'s in the map" $ do
        let
          map = MappyMap $ StandardMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
          code = [
            def_main $ MappyApp (MappyNamedValue "default-take") [MappyKeyword "a", map, MappyKeyword "default"]
            ]

        it "finds the key in the map" $ do
          exec code `shouldBe` Right (MappyKeyword "b")

    describe "take" $ do
      describe "given undefined arguments" $ do
        let code = [def_main $ MappyApp (MappyNamedValue "take") [MappyNamedValue "a", MappyNamedValue "b"]]

        it "returns an error for each argument" $ do
          exec code `shouldBe` Left (map NameNotDefined ["a", "b"])

      describe "given the wrong number of arguments" $ do
        let
          map = MappyMap $ StandardMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
          code = [
            def_main $ MappyApp (MappyNamedValue "take") [MappyKeyword "a"]
            ]

        it "errors with a WrongNumberOfArguments error" $ do
          exec code `shouldBe` Left [WrongNumberOfArguments "take" 2 1]

      describe "given a key that\'s not in the map" $ do
        let
          map = MappyMap $ StandardMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
          code = [
            def_main $ MappyApp (MappyNamedValue "take") [MappyKeyword "not", map]
            ]

        it "errors with a KeyNotFound error" $ do
          exec code `shouldBe` Left [KeyNotFound (MappyKeyword "not") map]

      describe "given the correct arguments" $ do
        let
          map = MappyMap $ StandardMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
          code = [
            def_main $ MappyApp (MappyNamedValue "take") [MappyKeyword "a", map]
            ]

        it "finds the key in the map" $ do
          exec code `shouldBe` Right (MappyKeyword "b")

    describe "given main simply binds to something else in the environment" $ do
      let
        code = [
          simple_def "foo" "bar",
          def_main (MappyNamedValue "foo")
          ]

      it "evaluates to whatever that name evaluates to when looked up" $ do
        exec code `shouldBe` Right (MappyKeyword "bar")

    describe "given main is simply a map" $ do
      let main = [def_main map]
          map = MappyMap $ StandardMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")

      it "evaluates to just that map" $ do
        exec main `shouldBe` Right map

    describe "given main is simply a keyword" $ do
      let main = [def_main (MappyKeyword "foobar")]

      it "evaluates to just that keyword" $ do
        exec main `shouldBe` Right (MappyKeyword "foobar")

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
      let defs = [MappyDef (MappyNamedValue "foo") (MappyKeyword "bar")]

      it "errors with a main not found error" $ do
        exec defs `shouldBe` Left [MainNotFound]
