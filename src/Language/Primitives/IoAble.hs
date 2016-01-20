module Language.Primitives.IoAble where

class IoAble a where
  stringify :: a -> String
  meansPrint :: a -> Bool
