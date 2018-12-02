module Language.Error where

import Data.List (intercalate)
import Language.PrettyPrintable

data Error a =
  MainNotFound
  | RepeatedDefinition String
  | NameNotDefined String
  | WrongNumberOfArguments String Int Int
  | KeyNotFound a a
  | GiveCalledOnNonMap a a a
  | TakeCalledOnNonMap String a a
  | NotAFunction a
  deriving (Show, Eq)

singleError :: a -> Either [a] b
singleError = Left . pure

-- Errors claiming a potential bug in mappy itself
errorInMappy :: String -> a
errorInMappy msg = error $ intercalate "\n" [
    msg
  , "This is likely an error in mappy itself."
  , "  Feel free to file an issue in the mappy repository:"
  , "  " ++ mappyUrl
  ]

mappyUrl :: String
mappyUrl = "https://github.com/PolyglotSymposium/mappy"

instance PrettyPrintable a => PrettyPrintable (Error a) where
  pretty MainNotFound = "No `main` function found"
  pretty (RepeatedDefinition def) = concat ["The name `", def, "` was defined multiple times"]
  pretty (NameNotDefined name) = concat ["The name `", name, "` is not defined"]
  pretty (WrongNumberOfArguments name expected actual) =
    concat ["The function `", name, "` was applied to ", values actual, ", but it accepts ", atMost expected]
  pretty (KeyNotFound expr map') =
    concat ["The key `", pretty expr, "` was not found in `", pretty map', "`"]
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
