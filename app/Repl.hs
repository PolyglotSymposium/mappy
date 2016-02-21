module Repl (repl) where

import Paths_mappy
import System.Console.ANSI
import System.Console.Haskeline
import System.Directory
import Text.ParserCombinators.Parsec
import Data.List (intercalate)

import Mappy
import Language.Ast
import Language.Desugar
import Language.Error
import Language.Error.PrettyPrinter()
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
  setSGR [SetColor Foreground Vivid Green]
  putStrLn mappyAsciiArt
  setSGR [Reset]
  putStrLn "\nUse Ctrl-C to quit"
  home <- getHomeDirectory
  repl' (fst <$> validatePreExec prelude, concat [home, "/", ".mappy_history"])

repl' :: (Either [Error Expression] Env, FilePath) -> IO ()
repl' (Left errors, _) = putStrLn $ show errors
repl' (Right initialEnv, histFile) = runInputT settings (go initialEnv)
  where
  settings = defaultSettings { historyFile = Just histFile }
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

        Right (Just (Right expr)) -> case eval env $ desugarExpr expr of
          v -> (outputStrLn $ print' v) *> go env

        Right Nothing -> go env
