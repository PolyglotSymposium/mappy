module Language.AstExamples where

import Language.Ast

import qualified Data.Map as M

if_def =
  (MappyDef
    (MappyNamedValue "if")
    (MappyLambda
      [MappyNamedValue "cond", MappyNamedValue "then", MappyNamedValue "else"]
      (MappyApp
        (MappyNamedValue "default-take")
        [MappyApp
          (MappyNamedValue "take")
          [MappyKeyword "truthy", MappyNamedValue "cond"],
        MappyMap (M.fromList [(MappyKeyword "false",MappyNamedValue "else")]),
        MappyNamedValue "then"])))
