module Mappy where

import Language.Ast
import Language.Error.PrettyPrinter()
import Language.Executor
import Language.Parser
import Language.PrettyPrintable

import Data.List (intercalate)

readMappyFile :: String -> IO [Definition]
readMappyFile fileName =
  readFile fileName >>= either (error . show) pure . parseFile

print' :: FullyEvaluated Expression -> String
print' (Left errors) = intercalate "\n" $ map ((" - " ++) . pretty) errors
print' (Right expr) = pretty expr
