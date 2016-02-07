module Language.PrettyPrintable where

class PrettyPrintable a where
  pretty :: a -> String
