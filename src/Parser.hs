{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}

module Parser where
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Parsec.Char (char)
import Text.Parsec (option)

import Text.Parsec
import Data.List (intercalate)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Control.Monad (void)
import Data.Functor.Identity (Identity)

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
statement = choice 
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
  measurement <- identifier
  If measurement <$> braces (many statement)

repeatStmt :: Parser Statement
repeatStmt = do
  reserved "REPEAT"
  repeatCount <- integer
  Repeat repeatCount <$> braces (many statement)

printStmt :: Parser Statement
printStmt = do
  reserved "PRINT"
  message <- stringLiteral <|> identifier
  pure $ Print message

programParser :: Parser Program
programParser = do
  whiteSpace
  stmts <- many statement
  eof
  return $ Program stmts

parseProgram :: String -> Either ParseError Program
parseProgram = parse programParser ""

-- statementParser :: Parser Statement
-- statementParser = do
--   skipMany space
--   stmt <- try initParser <|> try hadamardParser <|> try pauliXParser <|> try pauliYParser
--    <|> try pauliZParser <|> try cnotParser <|> phaseParser <|> measureParser  <|> try printParser <|> try ifParser <|> repeatParser
--   skipMany (oneOf " \t")
--   optional newline
--   return stmt

-- repeatParser :: Parser Statement
-- repeatParser = do
--   skipMany space
--   _ <- string "REPEAT"
--   skipMany1 space
--   count <- intParser
--   skipMany1 space
--   stmts <- blockParser
--   return $ Repeat count stmts

-- tu dzieje się jakaś dziwna rzecz, tzn jak wpiszę PRINT zamiast RINT  to przykład testowy już nie działa, podejrzewam, że problem jest spowodowany tym, że PRINT 
-- zaczyna się od litery P tak samo jak PHASE, nie rozumiem czemu ma to powodować problemy, ale wygląda na to że jest jest to jakiś powód.
-- printParser = do
--   skipMany space
--   _ <- string "RINT"
--   skipMany1 space
--   msg <- many1 (noneOf "\n")
--   return $ Print msg


-- blockParser :: Parser [Statement]
-- blockParser = do
--   _ <- char '{'
--   spaces
--   stmts <- many statementParser
--   spaces
--   _ <- char '}'
--   return stmts

-- ifParser :: Parser Statement
-- ifParser = do
--   skipMany space
--   _ <- string "IF"
--   skipMany1 space
--   reg <- many1 (noneOf " \t\n\r/")
--   skipMany1 space
--   stmts <- blockParser
--   return $ If reg stmts
  


-- measureParser :: Parser Statement
-- measureParser = do
--   skipMany space
--   _ <- string "MEASURE"
--   skipMany1 space
--   qubit <- intParser
--   skipMany1 space
--   reg <- many1 (noneOf " \t\n\r/")
--   return $ Measure qubit reg

-- phaseParser :: Parser Statement
-- phaseParser = do
--   skipMany space
--   _ <- string "PHASE"
--   skipMany1 space
--   angle <- doubleParser
--   skipMany1 space
--   qubit <- intParser
--   return $ Phase angle qubit

-- doubleParser :: Parser Double
-- doubleParser = do
--   num <- many1 digit
--   frac <- option "" $ do
--     dot <- char '.'
--     decimals <- many1 digit
--     return (dot : decimals)
--   return (read (num ++ frac))

-- cnotParser :: Parser Statement
-- cnotParser = do
--   skipMany space
--   _ <- string "CNOT"
--   skipMany1 space
--   n <- intParser
--   skipMany space
--   m <- intParser
--   skipMany space
--   optional (try (string "//" >> many (noneOf "\n")))
--   skipMany (oneOf " \t")
--   optional newline
--   return $ CNOT n m

-- pauliZParser :: Parser Statement
-- pauliZParser = do
--   skipMany space
--   _ <- string "PauliZ"
--   skipMany1 space
--   n <- intParser
--   skipMany space
--   optional (try (string "//" >> many (noneOf "\n")))
--   skipMany (oneOf " \t")
--   optional newline
--   return $ PauliZ n

-- pauliYParser :: Parser Statement
-- pauliYParser = do
--   skipMany space
--   _ <- string "PauliY"
--   skipMany1 space
--   n <- intParser
--   skipMany space
--   optional (try (string "//" >> many (noneOf "\n")))
--   skipMany (oneOf " \t")
--   optional newline
--   return $ PauliY n

-- pauliXParser :: Parser Statement
-- pauliXParser = do
--   skipMany space
--   _ <- string "PauliX"
--   skipMany1 space
--   n <- intParser
--   skipMany space
--   optional (try (string "//" >> many (noneOf "\n")))
--   skipMany (oneOf " \t")
--   optional newline
--   return $ PauliX n

-- hadamardParser :: Parser Statement
-- hadamardParser = do
--   skipMany space
--   _ <- string "HADAMARD"
--   skipMany1 space
--   n <- intParser
--   optional (try (string "//" >> many (noneOf "\n")))
--   skipMany (oneOf " \t")
--   optional newline
--   return $ Hadamard n

-- initParser :: Parser Statement
-- initParser = do
--   skipMany space
--   _ <- string "INIT"
--   skipMany1 space
--   n <- intParser
--   skipMany space
--   optional (try (string "//" >> many (noneOf "\n")))
--   skipMany (oneOf " \t")
--   optional newline
--   return $ InitQubit n


-- intParser :: Parser Int
-- intParser = read <$> many1 digit

exampleCode :: String
exampleCode = unlines
  [ "INIT 1", "INIT 3 // let the force be with you, but it will be skipped", "HADAMARD 2", "PauliX 4", "PauliY 5 ", "PauliZ 6", "CNOT 3 4", "PHASE 0.5 1",
  "MEASURE  1 result", "RINT a", "RINT ala ma kota", "IF result {", "  PauliX 1", "RINT pies ma ale", "}", "REPEAT 3 {", "HADAMARD 2", "CNOT 2 1", "}"]

exampleCode1 :: String
exampleCode1 = unlines
  ["INIT 1", "HADAMARD 5 // a short and witty comment", "PAULIY 1", "PAULIX   -   1 // whitespace test",
   "  PAULIZ 4", "CNOT 10 -4", "PHASE 1.5 1", "MEASURE 2 -> result", "IF result {INIT 1}",
   "REPEAT 21 {}", "PRINT ProgramHasFinished"]

-- main :: IO ()
-- main = do
--   case parse programParser "" exampleCode1 of
--     Left err -> putStrLn $ "Parse error:\n" ++ show err
--     Right ast -> print ast

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [fileName] -> do
--       content <- readFile fileName
--       case parse programParser fileName content of
--         Left err -> do
--           putStrLn $ "Blad parsowania:\n" ++ show err
--           exitFailure
--         Right ast -> do
--           putStrLn "Parsowanie zakonczone sukcesem. AST:"
--           print ast
--     _ -> do
--       putStrLn "Uzycie: ./mojProgram <sciezka/do/pliku.txt>"
--       exitFailure