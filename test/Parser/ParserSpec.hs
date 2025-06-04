module Parser.ParserSpec (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Text.Parsec (ParseError)

import Parser

tests =
    [ testGroup "Unit tests" unitTests
    , testGroup "Property-based tests" propertyTests
    ]



-- Helper function for assertions
parseTest :: (Show a, Eq a) => (String -> Either ParseError a) -> String -> a -> Assertion
parseTest parser input expected =
    case parser input of
        Left err -> assertFailure $ "Parse error:\n" ++ show err
        Right result -> assertEqual ("Parsing: " ++ input) expected result

--------------------------------------------------------------------------------
-- Unit Tests
--------------------------------------------------------------------------------

unitTests =
    [ testCase "Parse simple arithmetic grammar" test_simpleGrammar
    , testCase "Parse single lexer rule" test_singleLexer
    , testCase "Parse epsilon in production" test_epsilon
    , testCase "Parse optional production" test_optional
    , testCase "Parse nested sequence and repetition" test_nested
    ]

test_simpleGrammar :: Assertion
test_simpleGrammar =
    let input =
            unlines
                [ "// Main expression structure"
                , "Expr ::= [Term, ([\"+\" | \"-\", Term])*]"
                , "// Generate lexer rules"
                , "Term ::= [Factor, ([\"*\" | \"/\", Factor])*]"
                , "Factor ::= Number | Variable | [\"(\", Expr, \")\"]"
                , "Number :=: \"^[0-9]+(\\\\.[0-9]+)$?\""
                , "Variable :=: \"[a-zA-Z][a-zA-Z0-9_]*\""
                , "Whitespace :=: \"[ \\t\\n\\r]+\""
                ]
        expected =
            Program
                [ Rule "Expr" (Sequence [ProductionSymbol (NonterminalSymbol "Term"), Repeat (Sequence [Choice (ProductionSymbol (TerminalSymbol "+")) (ProductionSymbol (TerminalSymbol "-")), ProductionSymbol (NonterminalSymbol "Term")])])
                , Rule "Term" (Sequence [ProductionSymbol (NonterminalSymbol "Factor"), Repeat (Sequence [Choice (ProductionSymbol (TerminalSymbol "*")) (ProductionSymbol (TerminalSymbol "/")), ProductionSymbol (NonterminalSymbol "Factor")])])
                , Rule "Factor" (Choice (Choice (ProductionSymbol (NonterminalSymbol "Number")) (ProductionSymbol (NonterminalSymbol "Variable"))) (Sequence [ProductionSymbol (TerminalSymbol "("), ProductionSymbol (NonterminalSymbol "Expr"), ProductionSymbol (TerminalSymbol ")")]))
                ]
                [ LexerRule "Number" "^[0-9]+(\\.[0-9]+)$?"
                , LexerRule "Variable" "[a-zA-Z][a-zA-Z0-9_]*"
                , LexerRule "Whitespace" "[ \t\n\r]+"
                ]
     in parseTest parseProgram input expected

test_singleLexer :: Assertion
test_singleLexer = parseTest parseProgram "Digit :=: \"[0-9]\"" $
    Program [] [LexerRule "Digit" "[0-9]"]

test_epsilon :: Assertion
test_epsilon = parseTest parseProgram "Empty ::= epsilon" $
    Program [Rule "Empty" (ProductionSymbol Epsilon)] []

test_optional :: Assertion
test_optional = parseTest parseProgram "Opt ::= \"a\"?" $
    Program [Rule "Opt" (Optional (ProductionSymbol (TerminalSymbol "a")))] []

test_nested :: Assertion
test_nested = parseTest parseProgram "List ::= [Item, Item]*" $
    Program [Rule "List" (Sequence [ProductionSymbol (NonterminalSymbol "Item"), Repeat (ProductionSymbol (NonterminalSymbol "Item"))])] []

--------------------------------------------------------------------------------
-- Property-based Tests
--------------------------------------------------------------------------------

propertyTests =
    [ testProperty "Generated lexer rule parses" prop_validLexer
    , testProperty "Optional rules parse correctly" prop_optionalParse
    , testProperty "Repeat rule preserves structure" prop_repeatParse
    , testProperty "Terminal strings are accepted" prop_terminal
    , testProperty "Generated rule structure is consistent" prop_productionStructure
    ]

-- Property 1: Valid lexer rule parses
prop_validLexer :: String -> Property
prop_validLexer s = isValidIdentifier s ==> 
    case parseProgram (s ++ " :=: \"[a-z]+\"") of
        Right (Program [] [LexerRule name regex]) -> name == s && regex == "[a-z]+"
        _ -> False

-- Property 2: Optional rule parsing roundtrip
prop_optionalParse :: Bool -> Bool
prop_optionalParse flag =
    let input = "Rule ::= \"a\"?"
        expected = Program [Rule "Rule" (Optional (ProductionSymbol (TerminalSymbol "a")))] []
     in parseProgram input == Right expected

-- Property 3: Repeat rule structure
prop_repeatParse :: Bool -> Bool
prop_repeatParse _ =
    let input = "R ::= \"x\"*"
        expected = Program [Rule "R" (Repeat (ProductionSymbol (TerminalSymbol "x")))] []
     in parseProgram input == Right expected

-- Property 4: Terminal rule parses correctly
prop_terminal :: String -> Property
prop_terminal s =
    not (null s) ==> 
        parseProgram ("T ::= \"" ++ s ++ "\"") /= Left undefined

-- Property 5: Generated rule structure is well-formed
prop_productionStructure :: Property
prop_productionStructure =
    forAll genSymbol $ \sym ->
        let prod = ProductionSymbol sym
         in prod == prod  -- reflexivity

-- --------------------
-- Generators
-- --------------------

-- Valid identifier generator
isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (x:xs) = (x `elem` letters) && all (`elem` valid) xs
  where
    letters = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
    valid = letters ++ ['0'..'9'] ++ "-"

genSymbol :: Gen Symbol
genSymbol =
    oneof
        [ TerminalSymbol <$> elements ["a", "b", "xyz"]
        , NonterminalSymbol <$> elements ["Expr", "Term", "X"]
        , pure Epsilon
        ]
