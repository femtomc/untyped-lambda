module Parser where

import AST (Term (App, Lam, Var))
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

-- Parsing functions.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseVar :: Parser Term
parseVar = do
  x <- many (letter <|> digit)
  return $ Var $ x

parseLam :: Parser Term
parseLam = do
  bound <- parseVar
  char '.'
  spaces
  inner <- try parseApp <|> try parseLam <|> parseVar
  return $ Lam bound inner

parseApp :: Parser Term
parseApp = do
  t1 <- try parseTerm <|> parseVar
  spaces
  t2 <- try parseTerm <|> parseVar
  return $ App t1 t2

parseTerm :: Parser Term
parseTerm = do
  char '('
  x <- try parseApp <|> try parseLam <|> parseVar
  char ')'
  return x

readTerm :: String -> IO ()
readTerm input = case parse parseTerm "lambda" input of
  Left err -> putStrLn $ "No match: " ++ show err
  Right t -> putStrLn $ show t
