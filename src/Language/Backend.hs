module Language.Backend where

import Language.Ast
import Language.Backend.Clojure as Clojure

data MappyBackend =
  ClojureBackend

backendToString :: MappyBackend -> [Definition] -> String
backendToString ClojureBackend = Clojure.backendToString
