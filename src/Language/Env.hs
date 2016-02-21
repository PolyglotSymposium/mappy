module Language.Env where

import qualified Data.Map.Strict as M

data EnvEntry a =
  NamePair (a, a)
  | MultiNameLookup (M.Map a a)
  deriving (Eq, Show, Ord)

type Env a = [EnvEntry a]

lookup :: Ord a => a -> Env a -> Maybe a
lookup _ [] = Nothing
lookup key (NamePair (name, value):rest) =
  if key == name
  then Just value
  else Language.Env.lookup key rest
lookup key (MultiNameLookup values:rest) =
  case M.lookup key values of
    Just value -> Just value
    Nothing -> Language.Env.lookup key rest
