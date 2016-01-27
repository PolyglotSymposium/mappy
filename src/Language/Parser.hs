module Language.Parser where

import Language.Ast
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec

import Data.Maybe (catMaybes)

parseFile = parse file "Error parsing file"

defOrExpr :: Parser (Maybe (Either Definition Expression))
defOrExpr =
  let
    validRepl cons p = Just . cons <$> try (fullTerm p)
    fullTerm p = whiteSpace *> p <* whiteSpace <* eof
  in
    validRepl Left definition <|>
    validRepl Right expression <|>
    whiteSpace *> eof *> pure Nothing

file :: Parser [Definition]
file =
  let
    validTopLevel = choice [lineComment *> pure Nothing, Just <$> definition]
    end = eof <|> lineComment
    fileContents = catMaybes <$> (many $ validTopLevel <* whiteSpace)
  in
    whiteSpace *> fileContents <* end

lineComment :: Parser ()
lineComment = string "--" *> manyTill anyChar (newline *> pure () <|> eof) *> pure ()

expression :: Parser Expression
expression = specialForm <|> map' <|> application <|> lambda <|> keyword <|> namedValue

definition :: Parser Definition
definition = do
  name <- namedValue
  whiteSpace
  (valueDefinition name <|> functionDefinition name)

valueDefinition :: Expression -> Parser Definition
valueDefinition name = MappyDef name <$> (char '=' *> whiteSpace *> expression)

functionDefinition :: Expression -> Parser Definition
functionDefinition name = do
  names <- namesEndingWith $ char '='
  whiteSpace
  expr <- expression
  return $ DefSugar $ SugaredFnDefinition name names expr

specialForm :: Parser Expression
specialForm = letExpression <|> list

list :: Parser Expression
list = ExprSugar . SugaredList <$> between
  (try $ string "(|" <* whiteSpace)
  (string "|)")
  (expression `sepEndBy` whiteSpace)

letExpression :: Parser Expression
letExpression = do
  try $ (string "let" <* whiteSpace)
  firstDef <- definition <* whiteSpace
  restDefs <- manyTill (definition <* whiteSpace) $ string "in"
  whiteSpace
  expr <- expression
  return $ ExprSugar $ SugaredLet (firstDef:restDefs) expr

lazyArgument :: Parser Expression
lazyArgument = fmap MappyLazyArgument $ char '(' *> whiteSpace *> identifier <* whiteSpace  <* char ')'

lambda :: Parser Expression
lambda = do
  char '\\'
  whiteSpace
  names <- namesEndingWith $ string "->"
  whiteSpace
  expr <- expression
  return $ MappyLambda names expr

namesEndingWith :: Parser a -> Parser [Expression]
namesEndingWith = manyTill ((namedValue <|> lazyArgument) <* whiteSpace)

pairs :: Parser (M.Map Expression Expression)
pairs = do
  ps <- expression `sepEndBy` whiteSpace
  if even $ length ps
     then pure $ toMap ps
     else unexpected "odd number of values in literal map"
  where
  toMap [] = M.empty
  toMap (k:v:rest) = M.insert k v $ toMap rest

map' :: Parser Expression
map' = MappyMap <$> StandardMap <$>
  between (char '(') (char ')') pairs

application :: Parser Expression
application = between (char '[') (char ']') $ do
    whiteSpace
    fn <- (namedValue <|> application <|> keyword)
    whiteSpace
    args <- expression `sepEndBy` whiteSpace
    return $ MappyApp fn args

identifier :: Parser String
identifier = many1 $ letter <|> digit <|> oneOf "_/-+<>!@#$%^&*;'\".?="

keyword :: Parser Expression
keyword = char ':' >> (MappyKeyword <$> identifier)

namedValue :: Parser Expression
namedValue = MappyNamedValue <$> identifier

whiteSpace :: Parser String
whiteSpace = many (oneOf " \n\r\t,") <?> "whitespace"
