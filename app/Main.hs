module Main (main) where


import System.Environment (getArgs)
import System.IO (readFile)
import Text.Parsec.Error (ParseError)
import Parser

main :: IO ()
main = parseFile

parseFile :: IO ()
parseFile = do
    args <- getArgs
    case args of
        [fileName] -> do
            contents <- readFile fileName
            case parseProgram contents of
                Left err -> do
                    putStrLn "Parse Error:"
                    print err
                Right ast -> do
                    putStrLn "Successfully parsed program:"
                    print ast
        _ -> putStrLn "Usage: parser <filename>"

parseAndPrint :: String -> IO ()
parseAndPrint input = do
    putStrLn "Input:"
    putStrLn input
    putStrLn "Result:"
    case parseProgram input of
        Left err -> print err
        Right ast -> print ast

-- Examples

examplePrograms :: [(String, String)]
examplePrograms =
    [ ("Qubit initialization", "INIT 1")
    
    , ("Hadamard gate", "HADAMARD 0")
    , ("Pauli gates", unlines["PAULIX 1", "PAULIY 0", "PAULIZ -1"])
    , ("Double input gates", unlines["CNOT -2 1", "//Comment test", "PHASE 0.0 4", "MEASURE 1 -> result"])
    , ("List input gates", unlines[ "IF \"result\" {PRINT \"test\" \nINIT 1}"
                                    ,"REPEAT 3 {HADAMARD 1 \nPAULIX 0 \nPRINT \"Iteration finished\"}"])]

runExamples :: IO ()
runExamples = do
    putStrLn "Running example programs:"
    mapM_ (\(name, prog) -> do
        putStrLn $ "\n=== " ++ name ++ " ==="
        parseAndPrint prog
        ) examplePrograms