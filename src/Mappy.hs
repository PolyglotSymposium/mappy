module Mappy where

import System.Environment (getArgs)

import Language.Ast
import Language.Parser

readMappyFile :: String -> IO [Definition]
readMappyFile fileName =
  readFile fileName >>= either (error . show) pure . parseFile
