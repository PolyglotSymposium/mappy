module Repl (repl) where

import Paths_mappy
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
repl' (Right initialEnv) = go initialEnv
  where
  go env = do
    line <- read'
    case parse defOrExpr "(unknown)" line of
      Left err -> (putStrLn $ show err) >> go env
      Right (Just (Left def)) ->
        let
          (MappyDef name value) = desugarEachDef def
        in
          go ((name, value):env)

      Right (Just (Right expr)) -> case eval env (desugarExpr expr) of
        Left errors -> (putStrLn $ show errors) *> go env
        Right result -> (putStrLn $ pretty result) *> go env

      Right Nothing -> go env

read' :: IO String
read' = do
  putStr "m> "
  hFlush stdout
  getLine
