module Language.Primitives.IoAble where

class IoAble a where
  meansPrint :: a -> Bool
