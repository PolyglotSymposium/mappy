module Language.Parser where

import Language.Ast
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

parseFile = parse file "(unknown)"

file :: Parser [Definition]
file = whiteSpace *> definition `sepEndBy` whiteSpace

expression :: Parser Expression
expression = map' <|> application <|> lambda <|> keyword <|> namedValue

definition :: Parser Definition
definition = do
  name <- namedValue
  whiteSpace
  char '='
  whiteSpace
  expr <- expression
  return $ MappyDef name expr

lazyArgument :: Parser Expression
lazyArgument = fmap MappyLazyArgument $ char '(' *> optional whiteSpace *> identifier <* optional whiteSpace  <* char ')'

lambda :: Parser Expression
lambda = do
  char '\\'
  optional whiteSpace
  names <- manyTill ((namedValue <|> lazyArgument) <* whiteSpace) (string "->")
  whiteSpace
  expr <- expression
  return $ MappyLambda names expr

pairs :: Parser (M.Map Expression Expression)
pairs = do
  exprs <- expression `sepEndBy` whiteSpace
  return $ toMap exprs
    where
    toMap [] = M.empty
    toMap (k:v:rest) = M.insert k v $ toMap rest

map' :: Parser Expression
map' = MappyMap <$>
  between (char '(') (char ')') (optional whiteSpace *> pairs)

application :: Parser Expression
application = between (char '[') (char ']') $ (`MappyApp` []) <$> application <|> do
    optional whiteSpace
    fn <- namedValue
    whiteSpace
    args <- expression `sepEndBy` whiteSpace
    return $ MappyApp fn args

identifier :: Parser String
identifier = many1 $ letter <|> digit <|> oneOf "_/-+<>!@#$%^&*;'\",.?="

keyword :: Parser Expression
keyword = char ':' >> (MappyKeyword <$> identifier)

namedValue :: Parser Expression
namedValue = MappyNamedValue <$> identifier

whiteSpace :: Parser String
whiteSpace = many (oneOf " \n\r\t") <?> "whitespace"
