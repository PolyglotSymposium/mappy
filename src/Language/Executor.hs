module Language.Executor where

import qualified Data.Set as S

import Language.Ast

data Error =
  MainNotFound
  | RepeatedDefinition String
  deriving (Show, Eq)

type ExecutionResult = Either [Error] Expression
type Env = [(Expression, Expression)]

exec :: [Definition] -> ExecutionResult
exec defs = do
  checkAgainstRepeatedDefs defs
  snd <$> initialEnvironment defs

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
