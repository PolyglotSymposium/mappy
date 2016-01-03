module Mappy where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec

data Definition =
  MappyDef Expression Expression
  deriving (Eq, Show)

data Expression =
  MappyMap (M.Map Expression Expression)
  | MappyApp Expression [Expression]
  | MappyKeyword String
  | MappyNamedValue String
  deriving (Eq, Show, Ord)

expression :: Parser Expression
expression = map' <|> application <|> keyword <|> namedValue

definition :: Parser Definition
definition = do
  name <- namedValue
  whiteSpace
  char '='
  whiteSpace
  expr <- expression
  return $ MappyDef name expr

pairs :: Parser (M.Map Expression Expression)
pairs = do
  exprs <- expression `sepEndBy` whiteSpace
  return $ toMap exprs
    where
    toMap [] = M.empty
    toMap (k:v:rest) = M.insert k v $ toMap rest

map' :: Parser Expression
map' = MappyMap <$>
  between (char '(') (char ')') pairs

application :: Parser Expression
application = between (char '[') (char ']') $ do
    fn <- namedValue
    whiteSpace
    args <- expression `sepEndBy` whiteSpace
    return $ MappyApp fn args

identifier :: Parser String
identifier = many1 $ letter <|> digit <|> oneOf "_/-+<>!@#$%^&*\\;'\",.?="

keyword :: Parser Expression
keyword = char ':' >> (MappyKeyword <$> identifier)

namedValue :: Parser Expression
namedValue = MappyNamedValue <$> identifier

whiteSpace :: Parser String
whiteSpace = many (oneOf " \n\r\t") <?> "whitespace"
