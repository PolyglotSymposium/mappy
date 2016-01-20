module Language.Primitives where

import Language.Ast
import Language.Primitives.Io

lookupPrimitive :: Expression -> Expression
lookupPrimitive (MappyNamedValue "__prim_io_map") = ioMap
