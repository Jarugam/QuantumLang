{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Parsec
    ( digit,
      newline,
      noneOf,
      oneOf,
      space,
      spaces,
      string,
      eof,
      many1,
      optional,
      skipMany1,
      many,
      parse,
      skipMany,
      try,
      (<|>),
      choice )
import Text.Parsec.String (Parser)

data Program = Program [Statement]
  deriving (Show, Eq)

data Statement
  = InitQubit Int
  | Hadamard Int
  deriving (Show, Eq)

programParser :: Parser Program
programParser = do
  spaces
  stmts <- many statementParser
  eof
  return $ Program stmts


statementParser :: Parser Statement
statementParser = do
  skipMany space
  stmt <- try initParser <|> try hadamardParser
  skipMany (oneOf " \t")
  optional newline
  return stmt

initParser :: Parser Statement
initParser = do
  skipMany space
  _ <- string "INIT"
  skipMany1 space
  n <- intParser
  skipMany space
  optional (try (string "//" >> many (noneOf "\n")))
  skipMany (oneOf " \t")
  optional newline
  return $ InitQubit n

hadamardParser :: Parser Statement
hadamardParser = do
  skipMany space
  _ <- string "HADAMARD"
  skipMany1 space
  n <- intParser
  optional (try (string "//" >> many (noneOf "\n")))
  skipMany (oneOf " \t")
  optional newline
  return $ Hadamard n

intParser :: Parser Int
intParser = read <$> many1 digit

exampleCode :: String
exampleCode = unlines
  [ "INIT 1", "INIT 3", "HADAMARD 2"]

main :: IO ()
main = do
  case parse programParser "" exampleCode of
    Left err -> putStrLn $ "Parse error:\n" ++ show err
    Right ast -> print ast
