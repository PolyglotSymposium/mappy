module Main where

import Mappy
import Repl
import Language.Executor

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    fileNames -> readMappyFiles fileNames >>= (putStrLn . print') . exec
