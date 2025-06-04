module Main (main) where

import Parser
import System.Environment (getArgs)
import System.IO (readFile)
import Text.Parsec.Error (ParseError)

-- | Main function to parse a file
main :: IO ()
main = print "kapusta"

-- parseFile :: IO ()
-- parseFile = do
--   args <- getArgs
--   case args of
--     [fileName] -> do
--       contents <- readFile fileName
--       case parseGrammar contents of
--         Left err -> do
--           putStrLn "Parse error:"
--           print err
--         Right ast -> do
--           putStrLn "Successfully parsed program:"
--           print ast
--     _ -> putStrLn "Usage: parser <filename>"

-- -- | Parse a string and print the result
-- parseAndPrint :: String -> IO ()
-- parseAndPrint input = do
--   putStrLn "Input:"
--   putStrLn input
--   putStrLn ""
--   putStrLn "Result:"
--   case parseGrammar input of
--     Left err -> print err
--     Right ast -> print ast

-- -- Example programs for testing
-- examplePrograms :: [(String, String)]
-- examplePrograms =
--   [ ("Simple Variable Declaration", "let x = 42;"),
--     ( "Basic Operations",
--       unlines
--         [ "let a = 10;",
--           "let b = 20;",
--           "let sum = a + b;",
--           "let diff = a - b;",
--           "let mod = a % b;"
--         ]
--     ),
--     ( "String Operations",
--       unlines
--         [ "let name = \"Alice\";",
--           "let greeting = \"Hello, \" ++ name;",
--           "print greeting;"
--         ]
--     ),
--     ( "Control Flow",
--       unlines
--         [ "let x = 10;",
--           "if x % 2 {",
--           "  print \"x is odd\";",
--           "} else {",
--           "  print \"x is even\";",
--           "};"
--         ]
--     ),
--     ( "Resource Management",
--       unlines
--         [ "declareResource \"resource1\";",
--           "declareResource \"resource2\";",
--           "lockAll [resource1, resource2] [r1, r2];",
--           "think 10;",
--           "unlockAll [r1, r2];"
--         ]
--     ),
--     ( "Process Spawning",
--       unlines
--         [ "spawn \"worker\" {",
--           "  loop {",
--           "    think (rand 10 50);",
--           "    print \"Worker is working\";",
--           "  };",
--           "};"
--         ]
--     ),
--     ( "Iteration",
--       unlines
--         [ "let sum = 0;",
--           "foreach 1 to 10 as i {",
--           "  let sum = sum + i;",
--           "};",
--           "print \"Sum: \" ++ sum;"
--         ]
--     )
--   ]

-- -- | Function to run all example programs
-- runExamples :: IO ()
-- runExamples = do
--   putStrLn "Running example programs:"
--   mapM_
--     ( \(name, prog) -> do
--         putStrLn $ "\n=== " ++ name ++ " ==="
--         parseAndPrint prog
--     )
--     examplePrograms

-- -- Simple unit test framework
-- testParser :: IO ()
-- testParser = do
--   putStrLn "Running parser tests..."

--   -- Test expressions
--   testExpr "42" "IntLit 42"
--   testExpr "x" "Var \"x\""
--   testExpr "\"hello\"" "StringLit \"hello\""
--   testExpr "a + b" "Add (Var \"a\") (Var \"b\")"
--   testExpr "a - b" "Sub (Var \"a\") (Var \"b\")"
--   testExpr "a % b" "Mod (Var \"a\") (Var \"b\")"
--   testExpr "\"a\" ++ \"b\"" "Concat (StringLit \"a\") (StringLit \"b\")"
--   testExpr "rand 1 10" "Rand (IntLit 1) (IntLit 10)"

--   -- Test statements
--   testStmt "think 42;" "Think (IntLit 42)"
--   testStmt "print \"hello\";" "Print \"hello\""
--   testStmt "let x = 42;" "Let \"x\" (IntLit 42)"

--   -- More complex tests
--   testProgram
--     "let x = 42; think x;"
--     "Program [Let \"x\" (IntLit 42),Think (Var \"x\")]"

--   putStrLn "All tests completed."
--   where
--     testExpr input expected = do
--       case parseGrammer ("let test = " ++ input ++ ";") of
--         Left err -> putStrLn $ "FAIL: Could not parse expression: " ++ input ++ "\nError: " ++ show err
--         Right (Program [Let _ expr]) ->
--           if show expr == expected
--             then putStrLn $ "PASS: " ++ input
--             else putStrLn $ "FAIL: " ++ input ++ "\nExpected: " ++ expected ++ "\nGot: " ++ show expr
--         Right other -> putStrLn $ "FAIL: Unexpected parse result for " ++ input ++ ": " ++ show other

--     testStmt input expected = do
--       case parseGrammer input of
--         Left err -> putStrLn $ "FAIL: Could not parse statement: " ++ input ++ "\nError: " ++ show err
--         Right (Program [stmt]) ->
--           if show stmt == expected
--             then putStrLn $ "PASS: " ++ input
--             else putStrLn $ "FAIL: " ++ input ++ "\nExpected: " ++ expected ++ "\nGot: " ++ show stmt
--         Right other -> putStrLn $ "FAIL: Unexpected parse result for " ++ input ++ ": " ++ show other

--     testProgram input expected = do
--       case parseGrammar input of
--         Left err -> putStrLn $ "FAIL: Could not parse program: " ++ input ++ "\nError: " ++ show err
--         Right program ->
--           if show program == expected
--             then putStrLn $ "PASS: " ++ input
--             else putStrLn $ "FAIL: " ++ input ++ "\nExpected: " ++ expected ++ "\nGot: " ++ show program
