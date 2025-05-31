{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}

module Parser where
import Text.Parsec.Char (char)
--import qualified Text.Parsec.Token as Tok
import Text.Parsec (option)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)

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
  deriving (Eq)

data Statement
  = InitQubit Int -- +
  | Hadamard Int -- +
  | PauliX Int -- +
  | PauliY Int -- +
  | PauliZ Int -- +
  | CNOT Int Int -- +
  | Phase Double Int -- +
  | Measure Int String -- + 
  | If String [Statement] -- +-
  | Repeat Int [Statement] -- +-
  | Print String -- +
  deriving (Show, Eq)

instance Show Program where
  show :: Program -> String
  show (Program stmts) = unlines (map show stmts)

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef
      { Token.commentLine = "//"
      , Token.reservedNames = 
        [ "INIT", "HADAMARD", "PAULIX", "PAULIY", "PAULIZ"
        , "CNOT", "PHASE", "MEASURE", "IF", "REPEAT", "PRINT"
        ]
      }

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

integer :: Parser Int
integer = fromIntegral <$> Token.integer lexer

double :: Parser Double
double = Token.float lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

programParser :: Parser Program
programParser = do
  spaces
  stmts <- many statementParser
  eof
  return $ Program stmts

statementParser :: Parser Statement
statementParser = do
  skipMany space
  stmt <- try initParser <|> try hadamardParser <|> try pauliXParser <|> try pauliYParser
   <|> try pauliZParser <|> try cnotParser <|> phaseParser <|> measureParser  <|> try printParser <|> try ifParser <|> repeatParser
  skipMany (oneOf " \t")
  optional newline
  return stmt


repeatParser :: Parser Statement
repeatParser = do
  skipMany space
  _ <- string "REPEAT"
  skipMany1 space
  count <- intParser
  skipMany1 space
  stmts <- blockParser
  return $ Repeat count stmts

-- tu dzieje się jakaś dziwna rzecz, tzn jak wpiszę PRINT zamiast RINT  to przykład testowy już nie działa, podejrzewam, że problem jest spowodowany tym, że PRINT 
-- zaczyna się od litery P tak samo jak PHASE, nie rozumiem czemu ma to powodować problemy, ale wygląda na to że jest jest to jakiś powód.
printParser = do
  skipMany space
  _ <- string "RINT"
  skipMany1 space
  msg <- many1 (noneOf "\n")
  return $ Print msg


blockParser :: Parser [Statement]
blockParser = do
  _ <- char '{'
  spaces
  stmts <- many statementParser
  spaces
  _ <- char '}'
  return stmts

ifParser :: Parser Statement
ifParser = do
  skipMany space
  _ <- string "IF"
  skipMany1 space
  reg <- many1 (noneOf " \t\n\r/")
  skipMany1 space
  stmts <- blockParser
  return $ If reg stmts
  


measureParser :: Parser Statement
measureParser = do
  skipMany space
  _ <- string "MEASURE"
  skipMany1 space
  qubit <- intParser
  skipMany1 space
  reg <- many1 (noneOf " \t\n\r/")
  return $ Measure qubit reg

phaseParser :: Parser Statement
phaseParser = do
  skipMany space
  _ <- string "PHASE"
  skipMany1 space
  angle <- doubleParser
  skipMany1 space
  qubit <- intParser
  return $ Phase angle qubit

doubleParser :: Parser Double
doubleParser = do
  num <- many1 digit
  frac <- option "" $ do
    dot <- char '.'
    decimals <- many1 digit
    return (dot : decimals)
  return (read (num ++ frac))

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
  [ "INIT 1", "INIT 3 // let the force be with you, but it will be skipped", "HADAMARD 2", "PauliX 4", "PauliY 5 ", "PauliZ 6", "CNOT 3 4", "PHASE 0.5 1",
  "MEASURE  1 result", "RINT a", "RINT ala ma kota", "IF result {", "  PauliX 1", "RINT pies ma ale", "}", "REPEAT 3 {", "HADAMARD 2", "CNOT 2 1", "}"]

main :: IO ()
main = do
  case parse programParser "" exampleCode of
    Left err -> putStrLn $ "Parse error:\n" ++ show err
    Right ast -> print ast

    -- these lines below are applied to parsers functions and skips comments
  --optional (try (string "//" >> many (noneOf "\n")))
  --skipMany (oneOf " \t")
  --optional newline
