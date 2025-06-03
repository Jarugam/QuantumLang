{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

data Program = Program [Statement]
  deriving (Eq)

data Statement
  = InitQubit Int
  | Hadamard Int
  | PauliX Int
  | PauliY Int
  | PauliZ Int
  | CNOT Int Int
  | Phase Double Int
  | Measure Int String
  | If String [Statement]
  | Repeat Int [Statement]
  | Print String
  deriving (Show, Eq)

instance Show Program where
  show :: Program -> String
  show (Program stmts) = unlines (map show stmts)

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style =
      emptyDef
        { Token.commentLine = "//",
          Token.reservedNames =
            [ "INIT",
              "HADAMARD",
              "PAULIX",
              "PAULIY",
              "PAULIZ",
              "CNOT",
              "PHASE",
              "MEASURE",
              "IF",
              "REPEAT",
              "PRINT"
            ],
          Token.reservedOpNames = ["->"]
        }

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

integer :: Parser Int
integer = fromIntegral <$> Token.integer lexer

double :: Parser Double
double = do 
  sign <- option id (char '-' >> return negate)
  num <- Token.float lexer
  return (sign num)

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

symbol :: String -> ParsecT String () Identity String
symbol = Token.symbol lexer

statement :: Parser Statement
statement =
  choice
    [ initStmt,
      hadamardStmt,
      paulixStmt,
      pauliyStmt,
      paulizStmt,
      cnotStmt,
      phaseStmt,
      measureStmt,
      ifStmt,
      repeatStmt,
      printStmt
    ]

initStmt :: Parser Statement
initStmt = do
  reserved "INIT"
  InitQubit <$> integer

hadamardStmt :: Parser Statement
hadamardStmt = do
  reserved "HADAMARD"
  Hadamard <$> integer

paulixStmt :: Parser Statement
paulixStmt = do
  reserved "PAULIX"
  PauliX <$> integer

pauliyStmt :: Parser Statement
pauliyStmt = do
  reserved "PAULIY"
  PauliY <$> integer

paulizStmt :: Parser Statement
paulizStmt = do
  reserved "PAULIZ"
  PauliZ <$> integer

cnotStmt :: Parser Statement
cnotStmt = do
  reserved "CNOT"
  firstVal <- integer
  CNOT firstVal <$> integer

phaseStmt :: Parser Statement
phaseStmt = do
  reserved "PHASE"
  firstVal <- double
  Phase firstVal <$> integer

measureStmt :: Parser Statement
measureStmt = do
  reserved "MEASURE"
  firstVal <- integer
  _ <- symbol "->" 
  Measure firstVal <$> identifier

ifStmt :: Parser Statement
ifStmt = do
  reserved "IF"
  measurement <- identifier <|> stringLiteral
  If measurement <$> braces (many statement)

repeatStmt :: Parser Statement
repeatStmt = do
  reserved "REPEAT"
  repeatCount <- integer
  Repeat repeatCount <$> braces (many statement)

printStmt :: Parser Statement
printStmt = do
  reserved "PRINT"
  Print <$> stringLiteral

programParser :: Parser Program
programParser = do
  whiteSpace
  stmts <- many statement
  eof
  return $ Program stmts

parseProgram :: String -> Either ParseError Program
parseProgram = parse programParser ""

exampleCode :: String
exampleCode =
  unlines
    [ "INIT 1",
      "INIT 3 // let the force be with you, but it will be skipped",
      "HADAMARD 2",
      "PauliX 4",
      "PauliY 5 ",
      "PauliZ 6",
      "CNOT 3 4",
      "PHASE 0.5 1",
      "MEASURE  1 result",
      "RINT a",
      "RINT ala ma kota",
      "IF result {",
      "  PauliX 1",
      "RINT pies ma ale",
      "}",
      "REPEAT 3 {",
      "HADAMARD 2",
      "CNOT 2 1",
      "}"
    ]

exampleCode1 :: String
exampleCode1 =
  unlines
    [ "INIT 1",
      "HADAMARD 5 // a short and witty comment",
      "PAULIY 1",
      "PAULIX   -   1 // whitespace test",
      "  PAULIZ 4",
      "CNOT 10 -4",
      "PHASE -1.5 1",
      "MEASURE 2 -> result",
      "IF result {INIT 1}",
      "REPEAT 21 {}",
      "PRINT \"Program Has Finished\""
    ]

parseCode :: IO () -- For testing purposes
parseCode = do
  case parse programParser "" exampleCode1 of
    Left err -> putStrLn $ "Parse error:\n" ++ show err
    Right ast -> print ast