module Language.Parser where

import Language.Ast
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

parseFile = parse file "(unknown)"

defOrExpr :: Parser (Maybe (Either Definition Expression))
defOrExpr =
  Just . Left <$> try (fullTerm definition) <|>
  Just . Right <$> fullTerm expression <|>
  eof *> pure Nothing

fullTerm :: Parser a -> Parser a
fullTerm p = p <* optional whiteSpace <* eof

file :: Parser [Definition]
file = whiteSpace *> definition `sepEndBy` whiteSpace

expression :: Parser Expression
expression = specialForm <|> map' <|> application <|> lambda <|> keyword <|> namedValue

definition :: Parser Definition
definition = do
  name <- namedValue
  whiteSpace
  (valueDefinition name <|> functionDefinition name)

valueDefinition :: Expression -> Parser Definition
valueDefinition name = do
  char '='
  whiteSpace
  expr <- expression
  return $ MappyDef name expr

functionDefinition :: Expression -> Parser Definition
functionDefinition name = do
  names <- namesEndingWith $ char '='
  whiteSpace
  expr <- expression
  return $ DefSugar $ SugaredFnDefinition name names expr

specialForm :: Parser Expression
specialForm = letExpression

letExpression :: Parser Expression
letExpression = do
  try $ (string "let" <* whiteSpace)
  firstDef <- definition <* whiteSpace
  restDefs <- manyTill (definition <* whiteSpace) $ string "in"
  whiteSpace
  expr <- expression
  return $ ExprSugar $ SugaredLet (firstDef:restDefs) expr

lazyArgument :: Parser Expression
lazyArgument = fmap MappyLazyArgument $ char '(' *> optional whiteSpace *> identifier <* optional whiteSpace  <* char ')'

lambda :: Parser Expression
lambda = do
  char '\\'
  optional whiteSpace
  names <- namesEndingWith $ string "->"
  whiteSpace
  expr <- expression
  return $ MappyLambda names expr

namesEndingWith :: Parser a -> Parser [Expression]
namesEndingWith end = manyTill ((namedValue <|> lazyArgument) <* whiteSpace) end

pairs :: Parser (M.Map Expression Expression)
pairs = do
  exprs <- expression `sepEndBy` whiteSpace
  return $ toMap exprs
    where
    toMap [] = M.empty
    toMap (k:v:rest) = M.insert k v $ toMap rest

map' :: Parser Expression
map' = MappyMap <$> StandardMap <$>
  between (char '(') (char ')') pairs

application :: Parser Expression
application = between (char '[') (char ']') $ do
    optional whiteSpace
    fn <- (namedValue <|> application)
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
