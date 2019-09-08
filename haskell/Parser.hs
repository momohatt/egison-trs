module Parser (
  parseEq,
  parseTerm,
  ) where

import           Data.Either
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token    as P

import           Types

lexer :: P.TokenParser ()
lexer = P.makeTokenParser haskellDef

parens = P.parens lexer

inSpaces :: Parser a -> Parser ()
inSpaces p = spaces >> p >> spaces

equation :: Parser Equation
equation = (curry Eq) <$> term <*> (inSpaces (char '=') >> term)

term :: Parser Term
term = try compound <|> var

var :: Parser Term
var = do
  char 'x'
  d <- many1 digit
  return . Var $ 'x' : d

compound :: Parser Term
compound = try mult <|> try plus <|> app

mult :: Parser Term
mult = do
  l <- atom
  inSpaces $ char '*'
  r <- try mult <|> atom
  return $ Compound "*" [l, r]

plus :: Parser Term
plus = do
  l <- atom
  inSpaces $ char '+'
  r <- try plus <|> atom
  return $ Compound "+" [l, r]

app :: Parser Term
app = do
  f <- many1 letter
  char '('
  xs <- many1 term
  char ')'
  return $ Compound f xs

atom :: Parser Term
atom = try var
    <|> try app
    <|> parens term

parseEq :: String -> Equation
parseEq input =
  case parse equation "" input of
    Left _ -> error "parser"
    Right eq -> eq

parseTerm :: String -> Term
parseTerm input =
  case parse term "" input of
    Left _ -> error "parser"
    Right t -> t
