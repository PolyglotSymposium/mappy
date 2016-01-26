module Language.Ast (
  Definition(..)
  , Expression(..)
  , PrimitiveMap(..)
  , SugaredDefinition(..)
  , SugaredExpression(..)
  , pretty
  ) where

import Data.List (intercalate)
import qualified Data.Map.Strict as M

import Language.Primitives.IoAble
import Language.Primitives.Map

data SugaredDefinition =
  SugaredFnDefinition Expression [Expression] Expression
  deriving (Eq, Show, Ord)

data SugaredExpression =
  SugaredLet [Definition] Expression
  | SugaredList [Expression]
  deriving (Eq, Show, Ord)

data Definition =
  MappyDef Expression Expression
  | DefSugar SugaredDefinition
  deriving (Eq, Show, Ord)

data Expression =
  MappyMap (PrimitiveMap Expression)
  | MappyApp Expression [Expression]
  | MappyLambda [Expression] Expression
  | MappyClosure [Expression] Expression [(Expression, Expression)]
  | MappyKeyword String
  | MappyNamedValue String
  | MappyLazyArgument String
  | ExprSugar SugaredExpression
  deriving (Eq, Show, Ord)

instance IoAble Expression where
  stringify = pretty
  meansPrint (MappyKeyword "print") = True
  meansPrint _ = False

pretty :: Expression -> String
pretty (MappyKeyword name) = ':':name
pretty (MappyNamedValue name) = name
pretty (MappyApp fn args) = "[" ++ intercalate " " (pretty fn:map pretty args) ++ "]"
pretty (MappyMap (StandardMap map')) = "(" ++ intercalate " " (map (\(k, v) -> pretty k ++ " " ++ pretty v) $ M.toList map') ++ ")"
pretty (MappyMap (IoMap _)) = "__prim_io_map"
pretty (MappyLambda args body) = "\\" ++ intercalate " " (map pretty args) ++ " -> " ++ pretty body
pretty (MappyClosure args body _) = "#closure[...]#" ++ pretty (MappyLambda args body)
