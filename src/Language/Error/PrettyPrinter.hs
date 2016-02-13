module Language.Error.PrettyPrinter where

import Language.Error
import Language.PrettyPrintable

instance PrettyPrintable a => PrettyPrintable (Error a) where
  pretty MainNotFound = "No `main` function found"
  pretty (RepeatedDefinition def) = concat ["The name `", def, "` was defined multiple times"]
  pretty (NameNotDefined name) = concat ["The name `", name, "` is not defined"]
  pretty (WrongNumberOfArguments name expected actual) =
    concat ["The function `", name, "` was applied to ", values actual, ", but it accepts ", atMost expected]
  pretty (KeyNotFound expr) =
    concat ["The key `", pretty expr, "` was not found"]
  pretty (GiveCalledOnNonMap _ _ nonMap) =
    concat ["The `give` primitive was called with a non-map: `", pretty nonMap, "`"]
  pretty (TakeCalledOnNonMap fnName _ nonMap) =
    concat ["The `", fnName ,"` primitive was called with a non-map: `", pretty nonMap, "`"]
  pretty (NotAFunction value) =
    concat ["The value `", pretty value, "` was applied as if it were a function"]

atMost :: Int -> String
atMost 0 = "0"
atMost n = "at most " ++ show n

values :: Int -> String
values 1 = "1 value"
values n = show n ++ " values"
