module Language.Ast (
  Definition(..)
  , Expression(..)
  , PrimitiveMap(..)
  , SugaredDefinition(..)
  , SugaredExpression(..)
  , pretty
  ) where

import Data.Char (chr)
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
  | SugaredChar Char
  | SugaredString String
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
pretty mm@(MappyMap (StandardMap map')) =
  case classifyMap map' of
    CharAsMap -> "'" ++ [chr $ keyDepth mm $ MappyKeyword "pred"] ++ "'"
    ListAsMap -> "(|" ++ intercalate " " (sugarList $ MappyMap $ StandardMap map') ++ "|)"
    JustAMap ->
      "(" ++ intercalate " " (map (\(k, v) -> pretty k ++ " " ++ pretty v) $ M.toList map') ++ ")"
pretty (MappyMap (IoMap _)) = "__prim_io_map"
pretty (MappyLambda args body) = "\\" ++ intercalate " " (map pretty args) ++ " -> " ++ pretty body
pretty (MappyClosure args body _) = "#closure[...]#" ++ pretty (MappyLambda args body)

data MapClassification =
  CharAsMap
  | ListAsMap
  | JustAMap

classifyMap :: M.Map Expression Expression -> MapClassification
classifyMap map' =
  case M.lookup (MappyKeyword "__type") map' of
    Just (MappyKeyword "char") -> CharAsMap
    Nothing -> case map (\k -> M.lookup (MappyKeyword k) map') $ ["head", "tail"] of
      [Just _, Just _] -> ListAsMap
      _ -> JustAMap

sugarList :: Expression -> [String]
sugarList (MappyMap (StandardMap map')) =
  case map (\k -> M.lookup (MappyKeyword k) map') $ ["head", "tail"] of
    [Just v, Just r] -> pretty v:sugarList r
    [Just v, Nothing] -> [pretty v]
    _ -> []

keyDepth :: Expression -> Expression -> Int
keyDepth (MappyMap (StandardMap map')) key =
  case M.lookup key map' of
    Just next -> 1 + keyDepth next key
    Nothing -> 0
keyDepth _ _ = error "keyDepth called on non-map"
