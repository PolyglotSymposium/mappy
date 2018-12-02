module Language.Ast (
  Definition(..)
  , Expression(..)
  , PrimitiveMap(..)
  , SugaredDefinition(..)
  , SugaredExpression(..)
  , mappyEmptyMap
  , mappyChar
  , mappyNat
  , mappyList
  , mappyZero
  , mappyOne
  , withTypeHint
  ) where

import Data.Bits
import Data.Char (ord, chr)
import qualified Data.Map.Strict as M

import Language.Env
import Language.Primitives.IoAble
import Language.Primitives.Map
import Language.PrettyPrintable
import Language.Error (errorInMappy)

data SugaredDefinition =
  SugaredFnDefinition Expression [Expression] Expression
  deriving (Eq, Show, Ord)

data SugaredExpression =
  SugaredLet [Definition] Expression
  | SugaredList [Expression]
  | SugaredChar Char
  | SugaredString String
  | SugaredInt Integer
  deriving (Eq, Show, Ord)

data Definition =
  MappyDef Expression Expression
  | DefSugar SugaredDefinition
  deriving (Eq, Show, Ord)

data Expression =
  MappyMap (PrimitiveMap Expression)
  | MappyApp Expression [Expression]
  | MappyLambda [Expression] Expression
  | MappyClosure [Expression] Expression (Env Expression)
  | MappyKeyword String
  | MappyNamedValue String
  | MappyLazyArgument String
  | ExprSugar SugaredExpression
  deriving (Eq, Show, Ord)

instance IoAble Expression where
  classifyOutput (MappyKeyword "print") = Just IoPrint
  classifyOutput (MappyKeyword "write-file") = Just IoWriteFile
  classifyOutput _ = Nothing
  classifyInput (MappyMap (StandardMap map')) =
    case M.lookup (MappyKeyword "read-file") map' of
      Nothing -> Nothing
      _ -> Just IoReadFile
  classifyInput _ = Nothing
  pluckInner (MappyMap (StandardMap map')) IoFilename =
    M.findWithDefault (error " - No file given in IO action") (MappyKeyword "file") map'
  pluckInner (MappyMap (StandardMap map')) IoContents =
    M.findWithDefault (error " - No file text given in IO action") (MappyKeyword "text") map'
  pluckInner (MappyMap (StandardMap map')) IoReadFileSel =
    M.findWithDefault (error " - No file given in IO action") (MappyKeyword "read-file") map'
  pluckInner _ _ = error " - Non-map given in IO action"
  fromString = mappyList id . map mappyChar

mappyList :: (Expression -> Expression) -> [Expression] -> Expression
mappyList f = MappyMap . StandardMap . go
  where
  go [] = M.empty
  go (v:vs) = M.fromList [(MappyKeyword "head", f v), (MappyKeyword "tail", MappyMap $ StandardMap $ go vs)]

withTypeHint :: Expression -> String -> Expression
withTypeHint (MappyMap (StandardMap map')) typeHint =
  MappyMap $ StandardMap $ M.union (M.singleton (MappyKeyword "__type") $ MappyKeyword typeHint) map'
withTypeHint v _ = v

mappyEmptyMap :: Expression
mappyEmptyMap = MappyMap $ StandardMap M.empty

mappyChar :: Char -> Expression
mappyChar c = toBinary (ord c) `withTypeHint` "char"

mappyNat :: Integer -> Expression
mappyNat 0 = mappyEmptyMap
mappyNat n = MappyMap $ StandardMap $ M.singleton (MappyKeyword "pred") $ mappyNat $ n - 1

toBinary :: Int -> Expression
toBinary = mappyList id . go
  where
  single 0 = mappyZero
  single _ = mappyOne
  go 0 = []
  go 1 = [mappyOne]
  go n = single (1 .&. n):go (shiftR n 1)

mappyZero :: Expression
mappyZero = MappyMap $ StandardMap M.empty

mappyOne :: Expression
mappyOne = MappyMap $ StandardMap $ M.singleton (MappyKeyword "pred") mappyZero

instance PrettyPrintable Expression where
  pretty (MappyKeyword name) = ':':name
  pretty (MappyNamedValue name) = name
  pretty (MappyLazyArgument name) = "(" ++ name ++ ")"
  pretty (MappyApp fn args) = "[" ++ unwords (pretty fn:map pretty args) ++ "]"
  pretty mm@(MappyMap (StandardMap map')) =
    case classifyMap map' of
      Zero -> "0"
      PositiveInt expr -> pretty expr
      NegativeInt expr -> "-" ++ pretty expr
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
  | Zero
  | PositiveInt Expression
  | NegativeInt Expression
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
    case map (\k -> M.lookup (MappyKeyword k) map') ["__type", "head", "tail", "numerator", "denominator", "truthy", "value"] of
      [Just (MappyKeyword "char"), _, _, _, _, _, _] -> CharAsMap
      [Just (MappyKeyword "zero"), _, _, _, _, _, _] -> Zero
      [Just (MappyKeyword "negative-int"), _, _, _, _, _, Just value] -> NegativeInt value
      [Just (MappyKeyword "positive-int"), _, _, _, _, _, Just value] -> PositiveInt value
      [_, Just _, Just _, _, _, _, _] ->
        if (MappyMap $ StandardMap map') `isListOf` CharAsMap then StringAsMap else ListAsMap
      [_, _, _, Just num, Just denom, _, _] -> RatioAsMap num denom
      [_, _, _, _, _, Just (MappyKeyword v), _] ->
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
  go _ _ _ = 99
