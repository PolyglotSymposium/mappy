module Language.Error where

import Data.List (intercalate)

data Error a =
  MainNotFound
  | RepeatedDefinition String
  | NameNotDefined String
  | WrongNumberOfArguments String Int Int
  | KeyNotFound a
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
