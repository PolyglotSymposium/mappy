module Language.Desugar where

import Language.Ast

import qualified Data.Map as M

desugarEachDef  :: Definition -> Definition
desugarEachDef (DefSugar sugared) = desugarDef sugared
desugarEachDef (MappyDef name body) = MappyDef name $ desugarExpr body

desugarDef :: SugaredDefinition -> Definition
desugarDef (SugaredFnDefinition name args body) = MappyDef name $ MappyLambda args $ desugarExpr body

desugarExpr :: Expression -> Expression
desugarExpr (ExprSugar (SugaredLet defs body)) =
  let
    defs' = map desugarEachDef defs
    body' = desugarExpr body
  in
    defsToLambda defs' body'
desugarExpr (MappyMap map') = MappyMap $ M.fromList $ map go $ M.toList map'
  where
  go (expr1, expr2) = (desugarExpr expr1, desugarExpr expr2)
desugarExpr (MappyApp fn args) = MappyApp (desugarExpr fn) $ map desugarExpr args
desugarExpr (MappyLambda args body) = MappyLambda (map desugarExpr args) $ desugarExpr body
desugarExpr expr = expr

defsToLambda :: [Definition] -> Expression -> Expression
defsToLambda [] expr =
  expr
defsToLambda (MappyDef name value:rest) expr =
  MappyApp (MappyLambda [name] $ defsToLambda rest expr) [value]
