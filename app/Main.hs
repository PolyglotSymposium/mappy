module Main where

import Mappy
import Repl
import Language.Ast
import Language.Executor

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] ->
      readMappyFile fileName >>= either print (putStrLn . pretty) . exec
    [] -> repl
