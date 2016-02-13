module Language.Error.PrettyPrinterSpec (spec) where

import Test.Hspec

import Language.Ast
import Language.Ast.PrettyPrinter()
import Language.Error
import Language.Error.PrettyPrinter()
import Language.PrettyPrintable

shouldFormatAs :: Error Expression -> String -> IO ()
shouldFormatAs err string =
  pretty err `shouldBe` string

spec :: Spec
spec = do
  describe "pretty" $ do
    describe "given a wrong number of arguments error error" $ do
      let
        fnName = "fn"
        err = WrongNumberOfArguments fnName

      describe "with more than 0 expected arguments" $ do
        let err' = err 12 15

        it "formats correctly" $ do
          err' `shouldFormatAs` "The function `fn` was applied to 15 values, but it accepts at most 12"

      describe "with 0 expected arguments" $ do
        let err' = err 0 15

        it "formats correctly" $ do
          err' `shouldFormatAs` "The function `fn` was applied to 15 values, but it accepts 0"

        describe "with 1 actual argument" $ do
          let err'' = err 0 1

          it "formats correctly" $ do
            err'' `shouldFormatAs` "The function `fn` was applied to 1 value, but it accepts 0"
