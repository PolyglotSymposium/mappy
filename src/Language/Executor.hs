module Language.Executor where

import Debug.Trace
import qualified Data.Either as E
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (liftM2)

import Language.Ast
import Language.Desugar
import Language.Error
import Language.Primitives.Map as PM

type FullyEvaluated = Either [Error Expression] Expression
type Env = [(Expression, Expression)]

exec :: [Definition] -> FullyEvaluated
exec defs = do
  let desugaredDefs = map desugarEachDef defs
  checkAgainstRepeatedDefs desugaredDefs
  (env, mainExpr) <- initialEnvironment desugaredDefs
  eval env mainExpr

eval :: Env -> Expression -> FullyEvaluated
eval env namedValue@(MappyNamedValue name) = do
  result <- maybe (singleError $ NameNotDefined name) Right (Prelude.lookup namedValue env)
  eval env result
eval env (MappyApp fn params) = apply env fn params
eval env (MappyLambda args body) = Right $ MappyClosure args body env
eval env (MappyClosure args body env') = Right $ MappyClosure args body (env ++ env')
eval env (MappyMap map') = evalMap (eval env) map'
eval _ value = Right value

evalMap :: (Expression -> FullyEvaluated) -> PrimitiveMap Expression -> FullyEvaluated
evalMap evaluator (StandardMap map) = go [] (M.toList map)
  where
  go pairs [] = Right $ MappyMap $ StandardMap $ M.fromList pairs
  go pairs ((key, value):rest) = do
    key' <- evaluator key
    value' <- evaluator value
    go ((key', value'):pairs) rest
evalMap _ map = Right $ MappyMap $ map

apply :: Env -> Expression -> [Expression] -> FullyEvaluated
apply = apply'

apply' :: Env -> Expression -> [Expression] -> FullyEvaluated
apply' env (MappyNamedValue "take") (key:map:[]) = do
  key' <- eval env key
  (MappyMap map') <- eval env map
  maybe (singleError $ KeyNotFound key') Right $ PM.lookup key' map'
apply' env (MappyNamedValue "take") args =
  singleError $ WrongNumberOfArguments "take" 2 $ length args
apply' env (MappyNamedValue "default-take") (key:map:def:[]) = do
  key' <- eval env key
  def' <- eval env def
  (MappyMap map') <- eval env map
  return $ PM.findWithDefault def' key' map'
apply' env (MappyNamedValue "default-take") args =
  singleError $ WrongNumberOfArguments "default-take" 3 $ length args
apply' env (MappyNamedValue "give") (key:value:map:[]) = do
  key' <- eval env key
  map' <- eval env map
  value' <- eval env value
  maybe (singleError $ GiveCalledOnNonMap key value' map') Right (mapInsert key' value' map')
    where
    mapInsert k v (MappyMap map) = Just $ MappyMap $ PM.insert k v map
    mapInsert _ _ _ = Nothing
apply' env (MappyNamedValue "give") args =
  singleError $ WrongNumberOfArguments "give" 3 $ length args
apply' env nonPrimitive args = do
  (MappyClosure argNames body closedEnv) <- eval env nonPrimitive
  env' <- extendEnvironment argNames args closedEnv
  eval env' body

extendEnvironment :: [Expression] -> [Expression] -> Env -> Either [Error Expression] Env
extendEnvironment argNames args env =
  let
    -- Env
    unEvaluated = zip argNames args
    -- [Either [Error] Env]
    evaluated = map extend unEvaluated
    partitioned = E.partitionEithers evaluated
  in
    (liftM2 (++)) (final partitioned) (pure env)
  where
  final ([], env) = Right env
  final (errors, _) = Left $ concat errors
  extend (MappyNamedValue name, value) = do
    v' <- eval env value
    return (MappyNamedValue name, v')
  extend (MappyLazyArgument name, value) = Right (MappyNamedValue name, MappyLambda [] value)
  extend _ = error "TODO: Better error for when a fn has a non-namey name"

checkAgainstRepeatedDefs :: [Definition] -> Either [Error Expression] [Definition]
checkAgainstRepeatedDefs defs = go (S.empty, []) defs
  where
  go (_, []) [] = Right defs
  go (_, repeats) [] = Left $ map RepeatedDefinition repeats
  go (seen, repeats) ((MappyDef (MappyNamedValue name) _):rest) = go (S.insert name seen, newRepeats seen name repeats) rest

  newRepeats seen name = (++) (if S.member name seen then [name] else [])

initialEnvironment :: [Definition] -> Either [Error Expression] (Env, Expression)
initialEnvironment = go ([], Nothing)
  where
  go (env, Just m) [] = Right (env, m)
  go (_, Nothing) [] = singleError MainNotFound
  go (env, _) (MappyDef (MappyNamedValue "main") mainBody:rest) = go (env, Just mainBody) rest
  go (env, maybeMain) (MappyDef name body:rest) = go ((name, body):env, maybeMain) rest
