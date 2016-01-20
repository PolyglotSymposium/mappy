module Main where

import Mappy
import Language.Ast
import Language.Executor
import Language.Parser

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      contents <- readFile fileName
      case parseFile contents of
        Left e -> print e
        Right decls -> case exec decls of
          Left e -> print e
          Right expr -> putStrLn ("Program returned: " ++ pretty expr)
    [] -> putStrLn "No file given!"
