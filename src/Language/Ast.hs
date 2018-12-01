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
import Data.Char (ord)
import qualified Data.Map.Strict as M

import Language.Env
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
  single 1 = mappyOne
  go 0 = []
  go 1 = [mappyOne]
  go n = single (1 .&. n):go (shiftR n 1)

mappyZero :: Expression
mappyZero = MappyMap $ StandardMap M.empty

mappyOne :: Expression
mappyOne = MappyMap $ StandardMap $ M.singleton (MappyKeyword "pred") mappyZero
