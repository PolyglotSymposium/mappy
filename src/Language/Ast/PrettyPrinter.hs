module Language.Ast.PrettyPrinter where

import Data.Bits
import Data.Char (chr)
import qualified Data.Map.Strict as M

import Language.Ast
import Language.Error (errorInMappy)
import Language.PrettyPrintable
import Language.Primitives.Map

instance PrettyPrintable Expression where
  pretty (MappyKeyword name) = ':':name
  pretty (MappyNamedValue name) = name
  pretty (MappyLazyArgument name) = "(" ++ name ++ ")"
  pretty (MappyApp fn args) = "[" ++ unwords (pretty fn:map pretty args) ++ "]"
  pretty mm@(MappyMap (StandardMap map')) =
    case classifyMap map' of
      CharAsMap -> "'" ++ charInternal mm ++ "'"
      ListAsMap -> "(|" ++ unwords (sugarList $ MappyMap $ StandardMap map') ++ "|)"
      JustAMap ->
        "(" ++ unwords (map (\(k, v) -> pretty k ++ " " ++ pretty v) $ M.toList map') ++ ")"
      StringAsMap -> "\"" ++ stringInternal mm ++ "\""
      (RatioAsMap num denom) -> pretty num ++ "/" ++ pretty denom
      (NatAsMap nat) -> show $ keyDepth nat $ MappyKeyword "pred"
      (BoolAsMap value) -> value
  pretty (MappyMap (IoMap _)) = "__prim_io_map"
  pretty (MappyLambda args body) = "\\" ++ unwords (map pretty args) ++ " -> " ++ pretty body
  pretty (MappyClosure args body _) = "#closure[...]#" ++ pretty (MappyLambda args body)
  pretty (ExprSugar _) = errorInMappy "A sugared value was pretty printed."

data MapClassification =
  CharAsMap
  | ListAsMap
  | StringAsMap
  | NatAsMap Expression
  | RatioAsMap Expression Expression
  | BoolAsMap String
  | JustAMap
  deriving Eq

classifyMap :: M.Map Expression Expression -> MapClassification
classifyMap map' =
  if M.keys map' == [MappyKeyword "pred"]
  then
    NatAsMap $ MappyMap $ StandardMap map'
  else
    case map (\k -> M.lookup (MappyKeyword k) map') ["__type", "head", "tail", "numerator", "denominator", "truthy"] of
      [Just (MappyKeyword "char"), _, _, _, _, _] -> CharAsMap
      [_, Just _, Just _, _, _, _] ->
        if (MappyMap $ StandardMap map') `isListOf` CharAsMap then StringAsMap else ListAsMap
      [_, _, _, Just num, Just denom, _] -> RatioAsMap num denom
      [_, _, _, _, _, Just (MappyKeyword v)] ->
        if v `elem` ["true", "false"] then BoolAsMap v else JustAMap
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
charInternal mm = [chr $ fromBinary mm]

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


fromBinary :: Expression -> Int
fromBinary = go 0 0
  where
  exprToBit expr = if expr == mappyZero then 0 else 1
  go pos acc (MappyMap (StandardMap map')) =
    case (M.lookup (MappyKeyword "head") map', M.lookup (MappyKeyword "tail") map') of
      (Just v, Just rest) -> go (pos + 1) ((.|.) acc $ shiftL (exprToBit v) pos) rest
      _ -> acc
