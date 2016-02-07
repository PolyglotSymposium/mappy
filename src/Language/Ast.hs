module Language.Ast (
  Definition(..)
  , Expression(..)
  , PrimitiveMap(..)
  , SugaredDefinition(..)
  , SugaredExpression(..)
  , pretty
  ) where

import Data.Char (chr)
import qualified Data.Map.Strict as M

import Language.Error (errorInMappy)
import Language.PrettyPrintable
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

instance PrettyPrintable Expression where
  pretty (MappyKeyword name) = ':':name
  pretty (MappyNamedValue name) = name
  pretty (MappyApp fn args) = "[" ++ unwords (pretty fn:map pretty args) ++ "]"
  pretty mm@(MappyMap (StandardMap map')) =
    case classifyMap map' of
      CharAsMap -> "'" ++ charInternal mm ++ "'"
      ListAsMap -> "(|" ++ unwords (sugarList $ MappyMap $ StandardMap map') ++ "|)"
      JustAMap ->
        "(" ++ unwords (map (\(k, v) -> pretty k ++ " " ++ pretty v) $ M.toList map') ++ ")"
      StringAsMap -> "\"" ++ stringInternal mm ++ "\""
  pretty (MappyMap (IoMap _)) = "__prim_io_map"
  pretty (MappyLambda args body) = "\\" ++ unwords (map pretty args) ++ " -> " ++ pretty body
  pretty (MappyClosure args body _) = "#closure[...]#" ++ pretty (MappyLambda args body)
  pretty (MappyLazyArgument _) = errorInMappy "A lazy argument was pretty printed."
  pretty (ExprSugar _) = errorInMappy "A sugared value was pretty printed."

data MapClassification =
  CharAsMap
  | ListAsMap
  | StringAsMap
  | JustAMap
  deriving Eq

classifyMap :: M.Map Expression Expression -> MapClassification
classifyMap map' =
  case map (\k -> M.lookup (MappyKeyword k) map') ["__type", "head", "tail"] of
    [Just (MappyKeyword "char"), _, _] -> CharAsMap
    [_, Just _, Just _] ->
      if (MappyMap $ StandardMap map') `isListOf` CharAsMap then StringAsMap else ListAsMap
    _ -> JustAMap

isListOf :: Expression -> MapClassification -> Bool
isListOf (MappyMap (StandardMap map')) cls =
  case (M.size map', map (\k -> M.lookup (MappyKeyword k) map') ["head", "tail"]) of
    (2, [Just (MappyMap (StandardMap v)), Just rest]) -> classifyMap v == cls && rest `isListOf` cls
    (0, [Nothing, Nothing]) -> True
    _ -> False
isListOf _ _ = False

sugarList :: Expression -> [String]
sugarList (MappyMap (StandardMap map')) =
  case map (\k -> M.lookup (MappyKeyword k) map') ["head", "tail"] of
    [Just v, Just r] -> pretty v:sugarList r
    [Just v, Nothing] -> [pretty v]
    _ -> []
sugarList _ = errorInMappy "Attempted to sugar a non-list into a list."

charInternal :: Expression -> String
charInternal mm = [chr $ keyDepth mm $ MappyKeyword "pred"]

stringInternal :: Expression -> String
stringInternal (MappyMap (StandardMap map')) =
  case map (\k -> M.lookup (MappyKeyword k) map') ["head", "tail"] of
    [Just k, Just rest] -> charInternal k ++ stringInternal rest
    _ -> ""
stringInternal _ = errorInMappy "Attempted to sugar a non-string into a string."

keyDepth :: Expression -> Expression -> Int
keyDepth (MappyMap (StandardMap map')) key =
  case M.lookup key map' of
    Just next -> 1 + keyDepth next key
    Nothing -> 0
keyDepth _ _ = errorInMappy "keyDepth called on non-map."
