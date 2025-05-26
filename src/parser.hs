{-# LANGUAGE OverloadedStrings #-}

module Parser where

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
  | PauliX Int
  | PauliY Int
  | PauliZ Int
  | CNOT Int Int
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
  stmt <- try initParser <|> try hadamardParser <|> try pauliXParser <|> try pauliYParser <|> try pauliZParser <|> try cnotParser
  skipMany (oneOf " \t")
  optional newline
  return stmt

cnotParser :: Parser Statement
cnotParser = do
  skipMany space
  _ <- string "CNOT"
  skipMany1 space
  n <- intParser
  skipMany space
  m <- intParser
  skipMany space
  optional (try (string "//" >> many (noneOf "\n")))
  skipMany (oneOf " \t")
  optional newline
  return $ CNOT n m

pauliZParser :: Parser Statement
pauliZParser = do
  skipMany space
  _ <- string "PauliZ"
  skipMany1 space
  n <- intParser
  skipMany space
  optional (try (string "//" >> many (noneOf "\n")))
  skipMany (oneOf " \t")
  optional newline
  return $ PauliZ n

pauliYParser :: Parser Statement
pauliYParser = do
  skipMany space
  _ <- string "PauliY"
  skipMany1 space
  n <- intParser
  skipMany space
  optional (try (string "//" >> many (noneOf "\n")))
  skipMany (oneOf " \t")
  optional newline
  return $ PauliY n

pauliXParser :: Parser Statement
pauliXParser = do
  skipMany space
  _ <- string "PauliX"
  skipMany1 space
  n <- intParser
  skipMany space
  optional (try (string "//" >> many (noneOf "\n")))
  skipMany (oneOf " \t")
  optional newline
  return $ PauliX n

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


intParser :: Parser Int
intParser = read <$> many1 digit

exampleCode :: String
exampleCode = unlines
  [ "INIT 1", "INIT 3 // let the force be with you, but it will be skipped", "HADAMARD 2", "PauliX 4", "PauliY 5 ", "PauliZ 6", "CNOT 3 4"]

main :: IO ()
main = do
  case parse programParser "" exampleCode of
    Left err -> putStrLn $ "Parse error:\n" ++ show err
    Right ast -> print ast

    -- these lines below are applied to parsers functions and skips comments
  --optional (try (string "//" >> many (noneOf "\n")))
  --skipMany (oneOf " \t")
  --optional newline
