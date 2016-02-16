module Language.Backend.Clojure (backendToString) where

import Language.Ast
import Language.Primitives.Map

import qualified Data.Map.Strict as M

backendToString :: [Definition] -> String
backendToString [] = ""
backendToString (MappyDef name (MappyLambda args body):rest) =
  ((if hasLazyArguments args
  then lazyArgFn
  else simpleFn) name args body) ++ backendToString rest
backendToString (MappyDef name value:rest) =
  concat [
    "(def ", sanitizeName name, "\n  ", expressionToString value, ")\n\n", backendToString rest
  ] ++ backendToString rest

lazyArgFn :: Expression -> [Expression] -> Expression -> String
lazyArgFn name args body =
  let
    lazyArgRules = undefined
    newBody = replaceThroughout lazyArgRules body
  in
    simpleFn name (toNames args) newBody

replaceThroughout :: M.Map Expression (Expression -> Expression) -> Expression -> Expression
replaceThroughout = undefined
toNames = undefined
lazyArgs = undefined

simpleFn :: Expression -> [Expression] -> Expression -> String
simpleFn = undefined

hasLazyArguments :: [Expression] -> Bool
hasLazyArguments (MappyLazyArgument _:_) = True
hasLazyArguments [] = False
hasLazyArguments (_:rest) = hasLazyArguments rest

expressionToString :: Expression -> String
expressionToString (MappyMap (StandardMap map')) =
  "{" ++ unwords (map (\(k, v) -> expressionToString k ++ " " ++ expressionToString v) $ M.toList map') ++ "}"
expressionToString (MappyApp fn args) =
  concat ["(", expressionToString fn, " "] ++ unwords (map expressionToString args) ++ ")"
--expressionToString MappyLambda [Expression] Expression
--expressionToString MappyClosure [Expression] Expression [(Expression, Expression)]
--expressionToString MappyKeyword String
--expressionToString MappyNamedValue String
--expressionToString MappyLazyArgument String

--  oneOf letter <|> digit <|> oneOf "_/-+<>!@#$%^&*;.?="
sanitizeName :: Expression -> String
sanitizeName (MappyNamedValue name) = concatMap sanitize name
  where
  sanitize c = case c of
    '0' -> "$$zero"
    '1' -> "$$one"
    '2' -> "$$two"
    '3' -> "$$three"
    '4' -> "$$four"
    '5' -> "$$five"
    '6' -> "$$six"
    '7' -> "$$seven"
    '8' -> "$$eight"
    '9' -> "$$nine"
    '/' -> "$$slash"
    '-' -> "$$dash"
    '+' -> "$$plus"
    '!' -> "$$exclaim"
    '@' -> "$$at"
    '#' -> "$$hash"
    '#' -> "$$hash"
    '%' -> "$$percent"
    '^' -> "$$caret"
    '&' -> "$$amp"
    ';' -> "$$semi"
    '.' -> "$$dot"
    '?' -> "$$question"
    '=' -> "$$equal"
    x -> [x]
