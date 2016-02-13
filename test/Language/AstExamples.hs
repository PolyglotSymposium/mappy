module Language.AstExamples where

import Language.Ast
import Language.Primitives.Map

import qualified Data.Map.Strict as M

if_def =
  MappyDef
    (MappyNamedValue "if")
    (MappyLambda
      [MappyNamedValue "cond", MappyLazyArgument "then", MappyLazyArgument "else"]
      (MappyApp
        (MappyNamedValue "default-take")
        [MappyApp
          (MappyNamedValue "take")
          [MappyKeyword "truthy", MappyNamedValue "cond"],
        MappyMap $ StandardMap $ M.fromList [(MappyKeyword "false",MappyApp (MappyNamedValue "else") [])],
        MappyApp (MappyNamedValue "then") []]))
