module Main (main) where

import Parser
import System.Environment (getArgs)

-- | Main function to parse a file
main :: IO ()
main = parseFile

parseFile :: IO ()
parseFile = do
  args <- getArgs
  case args of
    [] -> putStrLn "No file"
    (_:_:_) -> putStrLn "Only one file can be parsed"
    [fileName] -> do
      contents <- readFile fileName
      case parseProgram contents of
        Left err -> do
          putStrLn "Parse error:"
          print err
        Right ast -> do
          putStrLn "Successfully parsed program:"
          print ast
