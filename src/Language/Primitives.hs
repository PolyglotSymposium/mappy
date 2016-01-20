module Language.Primitives where

import Language.Ast
import Language.Primitives.Io
import Language.Primitives.Map

primitives :: [(Expression, Expression)]
primitives = [
  (MappyNamedValue "__prim_io_map", MappyMap $ IoMap Io)
  ]
