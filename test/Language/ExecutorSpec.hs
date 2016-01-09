module Language.ExecutorSpec (spec) where

import Test.Hspec

import Language.Ast
import Language.AstExamples
import Language.Executor

import qualified Data.Map as M

simple_def name val = MappyDef (MappyNamedValue name) (MappyKeyword val)
def_main = MappyDef (MappyNamedValue "main")
truthy_value = MappyMap $ M.singleton (MappyKeyword "truthy") (MappyKeyword "true")
falsey_value = MappyMap $ M.singleton (MappyKeyword "truthy") (MappyKeyword "false")

spec :: Spec
spec = do
  describe "exec" $ do
    describe "laziness" $ do
      describe "using the if function, defined in terms of default-take" $ do
        let
          code thn els cond = [
            if_def,
            def_main $ MappyApp (MappyNamedValue "if") [cond, thn, els]]

        describe "when the \"then case\" has an error" $ do
          let appliedThen = code (MappyNamedValue "name-not-known")

          describe "when the \"else case\" does not have an error" $ do
            let appliedThenAndElse = appliedThen (MappyKeyword "a")

            describe "when evaluated with something truthy" $ do
              let fullyApplied = appliedThenAndElse truthy_value

              it "bubbles the error from the then case" $ do
                exec fullyApplied `shouldBe` Left [NameNotDefined "name-not-known"]

            describe "when evaluated with something falsey" $ do
              let fullyApplied = appliedThenAndElse falsey_value

              it "returns the else case" $ do
                exec fullyApplied `shouldBe` Right (MappyKeyword "a")


        describe "when the \"then case\" does not have an error" $ do
          let appliedThen = code (MappyKeyword "a")

          describe "when the \"else case\" has an error" $ do
            let appliedThenAndElse = appliedThen (MappyNamedValue "name-not-known")

            describe "when evaluated with something truthy" $ do
              let fullyApplied = appliedThenAndElse truthy_value

              it "returns the then case" $ do
                exec fullyApplied `shouldBe` Right (MappyKeyword "a")

            describe "when evaluated with something falsey" $ do
              let fullyApplied = appliedThenAndElse falsey_value

              it "bubbles the error from the else case" $ do
                exec fullyApplied `shouldBe` Left [NameNotDefined "name-not-known"]

    describe "the application of a lambda" $ do
      let
        code = [
          MappyDef (MappyNamedValue "second") (MappyLambda [MappyNamedValue "a", MappyNamedValue "b"] (MappyNamedValue "b")),
          def_main $ MappyApp (MappyNamedValue "second") [MappyKeyword "a", MappyKeyword "b"]
          ]

      it "applies it as a function" $ do
        exec code `shouldBe` Right (MappyKeyword "b")

    describe "a lambda" $ do
      describe "given the wrong number of arguments" $ do
        let
          code = [
            simple_def "a" "b",
            def_main $ MappyLambda [(MappyNamedValue "a"), (MappyNamedValue "b")] (MappyKeyword "x")
            ]

        it "evaluates to a closure containing the same information and the environment" $ do
          exec code `shouldBe` Right (MappyClosure [MappyNamedValue "a", MappyNamedValue "b"] (MappyKeyword "x") [(MappyNamedValue "a", MappyKeyword "b")])

      describe "given a new key and value" $ do
        let
          map = MappyMap M.empty
          code = [
            def_main $ MappyApp (MappyNamedValue "give") [MappyKeyword "a", MappyKeyword "b", map]
            ]

        it "returns a map with the new key" $ do
          exec code `shouldBe` Right (MappyMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b"))

    describe "give" $ do
      describe "given the wrong number of arguments" $ do
        let
          code = [
            def_main $ MappyApp (MappyNamedValue "give") [MappyKeyword "a"]
            ]

        it "errors with a WrongNumberOfArguments error" $ do
          exec code `shouldBe` Left [WrongNumberOfArguments "give" 3 1]

      describe "given a new key and value" $ do
        let
          map = MappyMap M.empty
          code = [
            def_main $ MappyApp (MappyNamedValue "give") [MappyKeyword "a", MappyKeyword "b", map]
            ]

        it "returns a map with the new key" $ do
          exec code `shouldBe` Right (MappyMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b"))

      describe "given an existing key" $ do
        let
          map = MappyMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
          code = [
            def_main $ MappyApp (MappyNamedValue "give") [MappyKeyword "a", MappyKeyword "c", map]
            ]

        it "overwrites the old value at the key" $ do
          exec code `shouldBe` Right (MappyMap $ M.singleton (MappyKeyword "a") (MappyKeyword "c"))

      describe "given a new key and a non-map" $ do
        let
          code = [
            def_main $ MappyApp (MappyNamedValue "give") [MappyKeyword "a", MappyKeyword "b", MappyKeyword "c"]
            ]

        it "fails with a GiveCalledOnNonMap error" $ do
          exec code `shouldBe` Left [GiveCalledOnNonMap (MappyKeyword "a") (MappyKeyword "b") (MappyKeyword "c")]

    describe "default-take" $ do
      describe "given a map with an erroring default value" $ do
        let
          code lookup = [
            def_main $ MappyApp (MappyNamedValue "default-take") [
              MappyKeyword lookup,
              MappyMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b"),
              MappyNamedValue "name-not-known"]
            ]

        describe "when looking up a key in the map" $ do
          let code' = code "a"

          it "fetches the value successfully" $ do
            exec code' `shouldBe` Right (MappyKeyword "b")

        describe "when looking up a key that is not in the map" $ do
          let code' = code "b"

          it "errors because the default key cannot be found" $ do
            exec code' `shouldBe` Left [NameNotDefined "name-not-known"]

      describe "given a map with an erroring value" $ do
        let
          code lookup = [
            def_main $ MappyApp (MappyNamedValue "default-take") [
              MappyKeyword lookup,
              MappyMap $ M.fromList [
                (MappyKeyword "a", MappyNamedValue "name-not-known"),
                (MappyKeyword "b", MappyKeyword "b-value")],
              MappyKeyword "default"]
            ]

        describe "when looking up the key of the erroring value" $ do
          let code' = code "a"

          it "errors" $ do
            exec code' `shouldBe` Left [NameNotDefined "name-not-known"]

        describe "when looking up a key other than the erroring one that is in the map" $ do
          let code' = code "b"

          it "returns that value alright" $ do
            exec code' `shouldBe` Right (MappyKeyword "b-value")

        describe "when looking up a key other not in the map" $ do
          let code' = code "foobar"

          it "returns the default value" $ do
            exec code' `shouldBe` Right (MappyKeyword "default")

      describe "given the wrong number of arguments" $ do
        let
          code = [
            def_main $ MappyApp (MappyNamedValue "default-take") [MappyKeyword "a"]
            ]

        it "errors with a WrongNumberOfArguments error" $ do
          exec code `shouldBe` Left [WrongNumberOfArguments "default-take" 3 1]

      describe "given a key that\'s not in the map and a default" $ do
        let
          map = MappyMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
          code = [
            def_main $ MappyApp (MappyNamedValue "default-take") [MappyKeyword "not", map, MappyKeyword "default"]
            ]

        it "returns the default" $ do
          exec code `shouldBe` Right (MappyKeyword "default")

      describe "given a key that\'s in the map" $ do
        let
          map = MappyMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
          code = [
            def_main $ MappyApp (MappyNamedValue "default-take") [MappyKeyword "a", map, MappyKeyword "default"]
            ]

        it "finds the key in the map" $ do
          exec code `shouldBe` Right (MappyKeyword "b")

    describe "take" $ do
      describe "given the wrong number of arguments" $ do
        let
          map = MappyMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
          code = [
            def_main $ MappyApp (MappyNamedValue "take") [MappyKeyword "a"]
            ]

        it "errors with a WrongNumberOfArguments error" $ do
          exec code `shouldBe` Left [WrongNumberOfArguments "take" 2 1]

      describe "given a key that\'s not in the map" $ do
        let
          map = MappyMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
          code = [
            def_main $ MappyApp (MappyNamedValue "take") [MappyKeyword "not", map]
            ]

        it "errors with a KeyNotFound error" $ do
          exec code `shouldBe` Left [KeyNotFound (MappyKeyword "not")]

      describe "given the correct arguments" $ do
        let
          map = MappyMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")
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

    describe "given a map" $ do
      describe "with unreduced values" $ do
        let code = [
              def_main map]
            map = MappyMap $ M.fromList [
              (MappyKeyword "a", MappyNamedValue "b"),
              (MappyKeyword "c", MappyNamedValue "d")]

        it "does not reduce those values" $ do
          exec code `shouldBe` Right map

      describe "with unreduced keys" $ do
        let code = [
              def_main map,
              simple_def "a" "b",
              simple_def "c" "d"]
            map = MappyMap $ M.fromList [
              (MappyNamedValue "a", MappyKeyword "b"),
              (MappyNamedValue "c", MappyKeyword "d")]

        it "reduces those keys" $ do
          exec code `shouldBe` (Right $ MappyMap $ M.fromList [(MappyKeyword "b", MappyKeyword "b"), (MappyKeyword "d", MappyKeyword "d")])

    describe "given main is simply a map" $ do
      let main = [def_main map]
          map = MappyMap $ M.singleton (MappyKeyword "a") (MappyKeyword "b")

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
