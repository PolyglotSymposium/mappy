module Language.Parser where

import Language.Ast
import Language.Error (errorInMappy)
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec
import Data.Char (digitToInt)
import Control.Monad (liftM, ap)

import Data.Maybe (catMaybes)

parseFile :: String -> Either ParseError [Definition]
parseFile = parse file "Error parsing file"

data ParsedDefOrExpr =
  Whitespace
  | ParsedDef Definition
  | ParsedExpr Expression
  deriving (Show, Eq)

defOrExpr :: Parser ParsedDefOrExpr
defOrExpr =
  let
    validRepl ctor p = ctor <$> try (fullTerm p)
    fullTerm p = whiteSpace *> p <* whiteSpace <* eof
  in
    validRepl ParsedDef definition <|>
    validRepl ParsedExpr expression <|>
    whiteSpace *> eof *> pure Whitespace

file :: Parser [Definition]
file =
  let
    validTopLevel = choice [lineComment *> pure Nothing, Just <$> definition]
    end = eof <|> lineComment
    fileContents = catMaybes <$> many ( validTopLevel <* whiteSpace)
  in
    whiteSpace *> fileContents <* end

lineComment :: Parser ()
lineComment = string "--" *> manyTill anyChar (newline *> pure () <|> eof) *> pure () <?> "line comment"

expression :: Parser Expression
expression = specialForm <|> map' <|> application <|> lambda <|> keyword <|> namedValue

definition :: Parser Definition
definition = (do
  name <- namedValue
  whiteSpace
  valueDefinition name <|> functionDefinition name) <?> "definition"

valueDefinition :: Expression -> Parser Definition
valueDefinition name = MappyDef name <$> (char '=' *> whiteSpace *> expression)

functionDefinition :: Expression -> Parser Definition
functionDefinition name = do
  names <- namesEndingWith $ char '='
  whiteSpace
  expr <- expression
  return $ DefSugar $ SugaredFnDefinition name names expr

specialForm :: Parser Expression
specialForm = letExpression <|> list <|> character <|> string' <|> integer

character :: Parser Expression
character = ExprSugar . SugaredChar <$> (char '\'' *> characterInternal <* char '\'') <?> "character"

string' :: Parser Expression
string' = ExprSugar . SugaredString <$> (char '"' *> manyTill characterInternal (char '"')) <?> "string"

integer :: Parser Expression
integer = ExprSugar . SugaredInt <$> int <?> "integer"
  where
    -- https://hackage.haskell.org/package/parsec-numbers-0.1.0/docs/src/Text-ParserCombinators-Parsec-Number.html#int
    int :: Parser Integer
    int = ap sign nat
    sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)
    nat = zeroNumber <|> decimal
    zeroNumber =
      char '0' >> (hexOrOct <|> decimal <|> return 0) <?> ""
    decimal = number (10 :: Integer) digit
    number base baseDigit = do
      n <- liftM (numberValue base) (many1 baseDigit)
      seq n (return n)
    numberValue base =
      foldl (\ x -> ((fromIntegral base * x) +) . fromIntegral . digitToInt) 0
    hexOrOct = hexadecimal <|> octal
    hexadecimal = oneOf "xX" >> hexnum
    hexnum = number (16 :: Integer) hexDigit
    octal = oneOf "oO" >> number (8 :: Integer) octDigit

characterInternal :: Parser Char
characterInternal = escapedChar <|> anyChar
  where
  escapedChar = do
    b <- char '\\'
    c <- anyChar
    return $ read $ '\'':b:c:"'"

list :: Parser Expression
list = ExprSugar . SugaredList <$> between
  (try $ string "(|" <* whiteSpace)
  (string "|)")
  (expression `sepEndBy` whiteSpace) <?> "list"

letExpression :: Parser Expression
letExpression = (do
  _ <- try $ string "let" <* whiteSpace
  firstDef <- definition <* whiteSpace
  restDefs <- manyTill (definition <* whiteSpace) $ string "in"
  whiteSpace
  expr <- expression
  return $ ExprSugar $ SugaredLet (firstDef:restDefs) expr) <?> "let expression"

lazyArgument :: Parser Expression
lazyArgument = fmap MappyLazyArgument (char '(' *> whiteSpace *> identifier <* whiteSpace  <* char ')')
  <?> "lazy argument"

lambda :: Parser Expression
lambda = lambda' <?> "lambda"
  where
  lambda' = do
    _ <- char '\\'
    whiteSpace
    names <- namesEndingWith $ string "->"
    whiteSpace
    expr <- expression
    return $ MappyLambda names expr

namesEndingWith :: Parser a -> Parser [Expression]
namesEndingWith = manyTill ((namedValue <|> lazyArgument) <* whiteSpace)

pairs :: Parser (M.Map Expression Expression)
pairs = do
  whiteSpace
  ps <- expression `sepEndBy` whiteSpace
  if even $ length ps
     then pure $ toMap ps
     else unexpected "odd number of values in literal map"
  where
  toMap [] = M.empty
  toMap [_] = errorInMappy "Impossible, odd valued map escaped guards."
  toMap (k:v:rest) = M.insert k v $ toMap rest

map' :: Parser Expression
map' = MappyMap <$> StandardMap <$>
  between (char '(') (char ')') pairs <?> "map"

application :: Parser Expression
application = between (char '[') (char ']') (do
    whiteSpace
    fn <- namedValue <|> application <|> keyword <|> lambda
    whiteSpace
    args <- expression `sepEndBy` whiteSpace
    return $ MappyApp fn args) <?> "application"

identifier :: Parser String
identifier = many1 $ letter <|> digit <|> oneOf "_/-+<>!@#$%^&*;.?="

keyword :: Parser Expression
keyword = char ':' *> (MappyKeyword <$> identifier) <?> "keyword"

namedValue :: Parser Expression
namedValue = MappyNamedValue <$> identifier <?> "name"

whiteSpace :: Parser ()
whiteSpace = many (oneOf " \n\r\t,") *> pure () <?> "whitespace"
