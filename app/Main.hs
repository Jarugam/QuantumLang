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
