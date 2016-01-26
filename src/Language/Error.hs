module Language.Error where

data Error a =
  MainNotFound
  | RepeatedDefinition String
  | NameNotDefined String
  | WrongNumberOfArguments String Int Int
  | KeyNotFound a
  | GiveCalledOnNonMap a a a
  | TakeCalledOnNonMap String a a
  deriving (Show, Eq)

singleError :: a -> Either [a] b
singleError = Left . pure
