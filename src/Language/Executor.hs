module Language.Executor where

import Language.Ast
-- TODO: Check against repeated definitions

data Error =
  MainNotFound
  deriving (Show, Eq)

type ExecutionResult = Either [Error] Expression
type Env = [(Expression, Expression)]

exec :: [Definition] -> ExecutionResult
exec defs = snd <$> initialEnvironment defs
  
initialEnvironment :: [Definition] -> Either [Error] (Env, Expression)
initialEnvironment = go ([], Nothing)
  where
  go (env, Just m) [] = Right (env, m)
  go (_, Nothing) [] = Left [MainNotFound]
  go (env, _) (MappyDef (MappyNamedValue "main") mainBody:rest) = go (env, Just mainBody) rest
  go (env, maybeMain) (MappyDef name body:rest) = go ((name, body):env, maybeMain) rest
