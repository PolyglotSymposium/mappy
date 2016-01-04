module Language.Ast (
  Definition(..),
  Expression(..)
  ) where

import qualified Data.Map as M

data Definition =
  MappyDef Expression Expression
  deriving (Eq, Show)

data Expression =
  MappyMap (M.Map Expression Expression)
  | MappyApp Expression [Expression]
  | MappyLambda [Expression] Expression
  | MappyKeyword String
  | MappyNamedValue String
  deriving (Eq, Show, Ord)