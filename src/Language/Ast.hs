module Language.Ast (
  Definition(..)
  , Expression(..)
  , PrimitiveMap(..)
  , SugaredDefinition(..)
  , SugaredExpression(..)
  ) where

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
  classifyIo (MappyKeyword "print") = Just IoPrint
  classifyIo (MappyKeyword "write-file") = Just IoWriteFile
  classifyIo _ = Nothing
  pluckInner (MappyMap (StandardMap map')) IoFilename =
    M.findWithDefault (error " - No file given in IO action") (MappyKeyword "file") map'
  pluckInner (MappyMap (StandardMap map')) IoContents =
    M.findWithDefault (error " - No file text given in IO action") (MappyKeyword "text") map'
  pluckInner _ _ = error " - Non-map given in IO action"
