module Language.Executor where

import qualified Data.Either as E
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (liftM2)

import Language.Ast
import Language.Ast.PrettyPrinter()
import Language.Desugar
import Language.Error
import Language.Primitives
import Language.Primitives.Map as PM

type FullyEvaluated a = Either [Error Expression] a
type Env = [(Expression, Expression)]

validatePreExec :: [Definition] -> Either [Error Expression] (Env, Expression)
validatePreExec defs = do
  let desugaredDefs = map desugarEachDef defs
  checkAgainstRepeatedDefs desugaredDefs >>= initialEnvironment

exec :: [Definition] -> FullyEvaluated Expression
exec defs = do
  (env, mainExpr) <- validatePreExec defs
  eval env mainExpr

eval :: Env -> Expression -> FullyEvaluated Expression
eval env namedValue@(MappyNamedValue name) = do
  result <- maybe (singleError $ NameNotDefined name) Right (Prelude.lookup namedValue env)
  eval env result
eval env (MappyApp fn params) = apply env fn params
eval env (MappyLambda args body) = Right $ MappyClosure args body env
eval env (MappyMap map') = evalMap (eval env) map'
eval _ value = Right value

evalMap :: (Expression -> FullyEvaluated Expression) -> PrimitiveMap Expression -> FullyEvaluated Expression
evalMap evaluator (StandardMap map') = go [] (M.toList map')
  where
  go pairs [] = Right $ MappyMap $ StandardMap $ M.fromList pairs
  go pairs ((key, value):rest) = do
    key' <- evaluator key
    value' <- evaluator value
    go ((key', value'):pairs) rest
evalMap _ map' = Right $ MappyMap map'

apply :: Env -> Expression -> [Expression] -> FullyEvaluated Expression
apply = apply'

apply' :: Env -> Expression -> [Expression] -> FullyEvaluated Expression
apply' env (MappyNamedValue "take") (key:map':[]) = do
  [key', maybeMap] <- evalAll env [key, map']
  (MappyMap map'') <- assertMap "take" key' maybeMap
  maybe (singleError $ KeyNotFound key') Right $ PM.lookup key' map''
apply' _ (MappyNamedValue "take") args =
  singleError $ WrongNumberOfArguments "take" 2 $ length args
apply' env (MappyNamedValue "default-take") (key:map':def:[]) = do
  [key', maybeMap, def'] <- evalAll env [key, map', def]
  (MappyMap map'') <- assertMap "default-take" key' maybeMap
  return $ PM.findWithDefault def' key' map''
apply' _ (MappyNamedValue "default-take") args =
  singleError $ WrongNumberOfArguments "default-take" 3 $ length args
apply' env (MappyNamedValue "give") (key:value:map':[]) = do
  [key', value', map''] <- evalAll env [key, value, map']
  maybe (singleError $ GiveCalledOnNonMap key value' map'') Right (mapInsert key' value' map'')
    where
    mapInsert k v (MappyMap m) = Just $ MappyMap $ PM.insert k v m
    mapInsert _ _ _ = Nothing
apply' _ (MappyNamedValue "give") args =
  singleError $ WrongNumberOfArguments "give" 3 $ length args
apply' env nonPrimitive args =
  eval env nonPrimitive >>= applyNonPrim nonPrimitive args env

applyNonPrim :: Expression -> [Expression] -> Env -> Expression -> FullyEvaluated Expression
applyNonPrim nonPrim args env closure@(MappyClosure argNames body closedEnv) = do
  env' <- extendEnvironment (take n argNames) (take n args) closedEnv env
  case compare (length argNames) n of
    LT -> Left [WrongNumberOfArguments (getName nonPrim) (length argNames) n]
    GT -> return $ MappyClosure (drop n argNames) body env'
    EQ -> eval env' body
  where
    n = length args
    getName (MappyNamedValue name) = name
    getName _ = "#closure#"

applyNonPrim _ args env kwd@(MappyKeyword _) =
  eval env $ MappyApp (MappyNamedValue "take") (kwd:args)
applyNonPrim _ _ _ value = Left [NotAFunction value]

evalAll :: Env -> [Expression] -> FullyEvaluated [Expression]
evalAll env exprs = case E.partitionEithers $ map (eval env) exprs of
  ([], values) -> Right values
  (errors, _) -> Left $ concat errors

assertMap :: String -> Expression -> Expression -> FullyEvaluated Expression
assertMap _ _ m@(MappyMap _) = Right m
assertMap fn key nonMap = Left [TakeCalledOnNonMap fn key nonMap]

extendEnvironment :: [Expression] -> [Expression] -> Env -> Env -> FullyEvaluated Env
extendEnvironment argNames args toExtend env =
  let
    -- Env
    unEvaluated = zip argNames args
    -- [Either [Error] Env]
    evaluated = map extend unEvaluated
    partitioned = E.partitionEithers evaluated
  in
    liftM2 (++) (final partitioned) (pure toExtend)
  where
  final ([], env') = Right env'
  final (errors, _) = Left $ concat errors
  extend (MappyNamedValue name, value) = do
    v' <- eval env value
    return (MappyNamedValue name, v')
  extend (MappyLazyArgument name, value) = Right (MappyNamedValue name, MappyLambda [] value)
  extend _ = errorInMappy "TODO: Better error for when a fn has a non-namey name."

checkAgainstRepeatedDefs :: [Definition] -> Either [Error Expression] [Definition]
checkAgainstRepeatedDefs defs = go (S.empty, []) defs
  where
  go (_, []) [] = Right defs
  go (_, repeats) [] = Left $ map RepeatedDefinition repeats
  go (seen, repeats) (MappyDef (MappyNamedValue name) _:rest) = go (S.insert name seen, newRepeats seen name repeats) rest
  go _ _ = errorInMappy "A definition was constructed with unexpected values."

  newRepeats seen name = (++) [name | S.member name seen]

initialEnvironment :: [Definition] -> Either [Error Expression] (Env, Expression)
initialEnvironment = go ([], Nothing)
  where
  go (env, Just m) [] = Right (env ++ primitives, m)
  go (_, Nothing) [] = singleError MainNotFound
  go (env, _) (MappyDef (MappyNamedValue "main") mainBody:rest) = go (env, Just mainBody) rest
  go (env, maybeMain) (MappyDef name body:rest) = go ((name, body):env, maybeMain) rest
  go _ _ = errorInMappy "A sugared definition escaped into execution."
