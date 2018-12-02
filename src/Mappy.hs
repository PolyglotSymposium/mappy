module Mappy where

import Language.Ast
import Language.Error.PrettyPrinter()
import Language.Executor
import Language.Parser
import Language.PrettyPrintable

import Data.List (intercalate)

simpleConcatFiles :: [String] -> IO String
simpleConcatFiles = go []
  where
    go acc [] = pure acc
    go acc (f:fs) = do
      text <- readFile f
      go (acc ++ "\n" ++ text) fs

readMappyFiles :: [String] -> IO [Definition]
readMappyFiles fileNames =
  simpleConcatFiles fileNames >>= either (error . show) pure . parseFile

readMappyFile :: String -> IO [Definition]
readMappyFile fn = readMappyFiles [fn]

print' :: FullyEvaluated Expression -> String
print' (Left errors) = intercalate "\n" $ map ((" - " ++) . pretty) errors
print' (Right expr) = pretty expr
