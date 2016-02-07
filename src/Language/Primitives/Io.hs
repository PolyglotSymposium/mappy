module Language.Primitives.Io where

import Debug.Trace

import Language.PrettyPrintable
import Language.Primitives.IoAble

data Io = Io
  deriving (Eq, Show, Ord)

ioInsert :: (IoAble a, PrettyPrintable a) => a -> a -> ()
ioInsert key value = if meansPrint key
  then trace (pretty value) ()
  else ()
