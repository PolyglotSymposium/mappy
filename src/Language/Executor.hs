module Language.Executor where

import qualified Data.Set as S
import qualified Data.Map as M

import Language.Ast

data Error =
  MainNotFound
  | RepeatedDefinition String
  | NameNotDefined String
  | WrongNumberOfArguments String Int Int
  deriving (Show, Eq)

type ExecutionResult = Either [Error] Expression
type Env = [(Expression, Expression)]

exec :: [Definition] -> ExecutionResult
exec defs = do
  checkAgainstRepeatedDefs defs
  init <- initialEnvironment defs
  uncurry eval init

eval :: Env -> Expression -> Either [Error] Expression
eval env namedValue@(MappyNamedValue name) = maybe (Left [NameNotDefined name]) Right (lookup namedValue env)
eval env (MappyApp fn params) = apply env fn params
eval _ value = Right value

apply :: Env -> Expression -> [Expression] -> Either [Error] Expression
apply env (MappyNamedValue "take") (key:map:[]) = do
  key <- eval env key
  map <- eval env map
  maybe (Left $ error "TODO: Better error here") Right (mapLookup key map)
apply env (MappyNamedValue "take") args = Left [WrongNumberOfArguments "take" 2 $ length args]

mapLookup :: Expression -> Expression -> Maybe Expression
mapLookup key (MappyMap map) = M.lookup key map
mapLookup _ _ = Nothing

checkAgainstRepeatedDefs :: [Definition] -> Either [Error] [Definition]
checkAgainstRepeatedDefs defs = go (S.empty, []) defs
  where
  go (_, []) [] = Right defs
  go (_, repeats) [] = Left $ map RepeatedDefinition repeats
  go (seen, repeats) ((MappyDef (MappyNamedValue name) _):rest) = go (S.insert name seen, newRepeats seen name repeats) rest

  newRepeats seen name = (++) (if S.member name seen then [name] else [])

initialEnvironment :: [Definition] -> Either [Error] (Env, Expression)
initialEnvironment = go ([], Nothing)
  where
  go (env, Just m) [] = Right (env, m)
  go (_, Nothing) [] = Left [MainNotFound]
  go (env, _) (MappyDef (MappyNamedValue "main") mainBody:rest) = go (env, Just mainBody) rest
  go (env, maybeMain) (MappyDef name body:rest) = go ((name, body):env, maybeMain) rest
