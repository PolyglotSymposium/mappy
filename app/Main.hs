module Main where

import Mappy
import Language.Executor
import Language.Parser
import Language.PrettyPrint

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      contents <- readFile fileName
      case parseFile contents of
        Left e -> print e
        Right decls -> print $ pretty <$> exec decls
    [] -> putStrLn "No file given!"
