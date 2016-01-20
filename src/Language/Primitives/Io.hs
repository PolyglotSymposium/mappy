module Language.Primitives.Io where

import Debug.Trace

import Language.Primitives.IoAble

data Io = Io
  deriving (Eq, Show, Ord)

ioInsert :: IoAble a => a -> a -> ()
ioInsert key value = case meansPrint key of
  True -> trace (stringify value) ()
  False -> ()
