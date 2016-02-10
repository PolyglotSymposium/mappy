module Language.Ast (
  Definition(..)
  , Expression(..)
  , PrimitiveMap(..)
  , SugaredDefinition(..)
  , SugaredExpression(..)
  , mappyChar
  , mappyNat
  ) where

import Data.Char (ord)
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
  fromString = mappyList . map mappyChar

mappyList :: [Expression] -> Expression
mappyList = MappyMap . StandardMap . go
  where
  go [] = M.empty
  go (v:vs) = M.fromList [(MappyKeyword "head", v), (MappyKeyword "tail", MappyMap $ StandardMap $ go vs)]

withTypeHint :: Expression -> String -> Expression
withTypeHint (MappyMap (StandardMap map')) typeHint =
  MappyMap $ StandardMap $ M.union (M.singleton (MappyKeyword "__type") $ MappyKeyword typeHint) map'
withTypeHint v _ = v

mappyChar :: Char -> Expression
mappyChar c = (mappyNat (ord c)) `withTypeHint` "char"

mappyNat :: Int -> Expression
mappyNat 0 = MappyMap $ StandardMap M.empty
mappyNat n = MappyMap $ StandardMap $ M.singleton (MappyKeyword "pred") $ mappyNat  $ n - 1
