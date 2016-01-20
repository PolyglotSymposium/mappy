module Repl (repl) where

import System.IO
import Text.ParserCombinators.Parsec

import Language.Ast
import Language.Desugar
import Language.Error
import Language.Executor
import Language.Parser

repl :: IO ()
repl = repl' (fst <$> initialEnvironment [dummyMain])

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

dummyMain :: Definition
dummyMain = MappyDef (MappyNamedValue "main") (MappyKeyword "main")
