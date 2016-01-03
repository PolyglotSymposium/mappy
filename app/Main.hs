module Main where

import Mappy

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      contents <- readFile fileName
      print $ parseFile contents
    [] -> putStrLn "No file given!"
