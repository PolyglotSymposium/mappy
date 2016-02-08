module Language.Primitives.Map where

import qualified Data.Map.Strict as M

import Language.PrettyPrintable
import Language.Primitives.Io
import Language.Primitives.IoAble

newtype UnifiedMap a = UnifiedMap (M.Map a a)

data PrimitiveMap a =
  IoMap Io
  | StandardMap (M.Map a a)
  deriving (Eq, Show, Ord)

lookup :: (IoAble a, PrettyPrintable a, Ord a) => a -> PrimitiveMap a -> Maybe a
lookup key (StandardMap map') = M.lookup key map'
lookup key (IoMap Io) = ioLookup key

findWithDefault :: Ord a => a -> a -> PrimitiveMap a -> a
findWithDefault def key (StandardMap map') = M.findWithDefault def key map'
findWithDefault _ _ (IoMap Io) = undefined

insert :: (IoAble a, PrettyPrintable a, Ord a) => a -> a -> PrimitiveMap a -> PrimitiveMap a
insert key value (StandardMap map') = StandardMap $ M.insert key value map'
insert key value (IoMap Io) = seq (ioInsert key value) (IoMap Io)
