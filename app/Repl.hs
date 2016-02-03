module Repl (repl) where

import Paths_mappy
import System.Console.Haskeline
import System.IO
import Text.ParserCombinators.Parsec
import Data.List (intercalate)

import Mappy
import Language.Ast
import Language.Desugar
import Language.Error
import Language.Executor
import Language.Parser

preludePath :: IO FilePath
preludePath = getDataFileName "prelude.map"

mappyAsciiArt :: String
mappyAsciiArt =
  '\n':intercalate "\n" [
      "|  \\/  |                        "
    , "| .  . | __ _ _ __  _ __  _   _ "
    , "| |\\/| |/ _` | '_ \\| '_ \\| | | |"
    , "| |  | | (_| | |_) | |_) | |_| |"
    , "\\_|  |_/\\__,_| .__/| .__/ \\__, |"
    , "             | |   | |     __/ |"
    , "             |_|   |_|    |___/ "]

repl :: IO ()
repl = do
  prelude <- preludePath >>= readMappyFile
  putStrLn mappyAsciiArt
  repl' (fst <$> validatePreExec prelude)

repl' :: Either [Error Expression] Env -> IO ()
repl' (Left errors) = putStrLn $ show errors
repl' (Right initialEnv) = runInputT defaultSettings (go initialEnv)
  where
  go env = do
    line <- getInputLine "m> "
    case line of
      Nothing -> go env
      Just line' -> case parse defOrExpr "(unknown)" line' of
        Left err -> (outputStrLn $ show err) >> go env
        Right (Just (Left def)) ->
          let
            (MappyDef name value) = desugarEachDef def
          in
            go ((name, value):env)

        Right (Just (Right expr)) -> case eval env (desugarExpr expr) of
          Left errors -> (outputStrLn $ show errors) *> go env
          Right result -> (outputStrLn $ pretty result) *> go env

        Right Nothing -> go env
