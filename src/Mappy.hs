module Mappy where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec

data Expression =
  MappyMap (M.Map Expression Expression)
  | MappyKeyword String
  | MappyNamedValue String
  deriving (Eq, Show, Ord)

whiteSpace :: Parser String
whiteSpace = many (oneOf " \n\r\t") <?> "whitespace"

pairs :: Parser (M.Map Expression Expression)
pairs = do
  exprs <- expression `sepEndBy` whiteSpace
  return $ toMap exprs
    where
    toMap [] = M.empty
    toMap (k:v:rest) = M.insert k v $ toMap rest

map' :: Parser Expression
map' = MappyMap <$> between
  (char '(')
  (char ')')
  pairs

identifier :: Parser String
identifier = many1 $ letter <|> digit <|> oneOf "_/-+<>!@#$%^&*\\;'\",.?"

keyword :: Parser Expression
keyword = char ':' >> (MappyKeyword <$> identifier)

namedValue :: Parser Expression
namedValue = MappyNamedValue <$> identifier

expression :: Parser Expression
expression = map' <|> keyword <|> namedValue
