module Language.Primitives where

import Language.Ast
import Language.Primitives.Io

primitives :: [(Expression, Expression)]
primitives = [
  (MappyNamedValue "__prim_io_map", MappyMap $ IoMap Io)
  ]
