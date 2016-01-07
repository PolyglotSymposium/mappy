module Language.PrettyPrint where

import Language.Ast

import qualified Data.Map as M
import Data.List (intercalate)

pretty :: Expression -> String
pretty (MappyKeyword name) = ':':name
pretty (MappyNamedValue name) = name
pretty (MappyApp fn args) = "[" ++ intercalate " " (pretty fn:map pretty args) ++ "]"
pretty (MappyMap map') = "(" ++ intercalate " " (map (\(k, v) -> pretty k ++ " " ++ pretty v) $ M.toList map') ++ ")"
pretty (MappyLambda args body) = "\\" ++ intercalate "" (map pretty args) ++ " -> " ++ pretty body
pretty (MappyClosure args body _) = "#closure[...]#" ++ pretty (MappyLambda args body)
