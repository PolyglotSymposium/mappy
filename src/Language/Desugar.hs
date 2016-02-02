module Language.Desugar where

import Language.Ast

import Data.Char (ord)
import qualified Data.Map.Strict as M

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
desugarExpr (ExprSugar (SugaredList [])) = MappyNamedValue "nil"
desugarExpr (ExprSugar (SugaredList (v:vs))) =
  MappyApp (MappyNamedValue "cons") [desugarExpr v, desugarExpr $ ExprSugar $ SugaredList vs]
desugarExpr (ExprSugar (SugaredString str)) =
  desugarExpr $ ExprSugar $ SugaredList $ map (desugarExpr . ExprSugar . SugaredChar) str
desugarExpr (ExprSugar (SugaredChar c)) =
    mappyNat (ord c) $ M.singleton (MappyKeyword "__type") (MappyKeyword "char")
desugarExpr (MappyMap (StandardMap map')) = MappyMap $ StandardMap $ M.fromList $ map go $ M.toList map'
  where
  go (expr1, expr2) = (desugarExpr expr1, desugarExpr expr2)
desugarExpr (MappyApp fn args) = MappyApp (desugarExpr fn) $ map desugarExpr args
desugarExpr (MappyLambda args body) = MappyLambda (map desugarExpr args) $ desugarExpr body
desugarExpr expr = expr

mappyNat :: Int -> M.Map Expression Expression -> Expression
mappyNat 0 extra = MappyMap $ StandardMap $ extra
mappyNat n extra = MappyMap $ StandardMap $ M.insert (MappyKeyword "pred") (mappyNat (n -1 ) extra) extra

defsToLambda :: [Definition] -> Expression -> Expression
defsToLambda [] expr =
  expr
defsToLambda (MappyDef name value:rest) expr =
  MappyApp (MappyLambda [name] $ defsToLambda rest expr) [value]
defsToLambda (DefSugar _:_) _ = error "A sugared def escaped to let! This is an error in mappy."
