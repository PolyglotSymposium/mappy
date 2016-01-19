module Language.Desugar where

import Language.Ast

desugarEachDef  :: Definition -> Definition
desugarEachDef (DefSugar sugared) = desugarDef sugared
desugarEachDef def = def

desugarDef :: SugaredDefinition -> Definition
desugarDef (SugaredFnDefinition name args body) = MappyDef name $ MappyLambda args body
