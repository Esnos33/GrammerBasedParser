{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser)
import Text.Parsec.Token qualified as Tok
import Text.Regex.TDFA ((=~))
import Data.Either (partitionEithers)

testRegex :: IO ()
testRegex = do
  let str = "16.531"
  -- print (str =~ "^[b-c]+[0-9]$+" :: Bool)
  print (str =~ "^[0-9]+(\\.[0-9]+)$?" :: Bool)

-- Abstract Syntax Tree -----------------------------------------------------

-- A Program is a collection of grammar rules and lexer rules
data Program = Program [Rule] [LexerRule]
  deriving (Show, Eq)

-- A grammar rule defines a nonterminal and its productions
data Rule = Rule Nonterminal Production
  deriving (Show, Eq)

-- A lexer rule defines a token pattern
data LexerRule = LexerRule String String
  deriving (Show, Eq)

-- A nonterminal symbol in the grammar
type Nonterminal = String

-- Core production types
data Production
  = Sequence [Production] -- Sequence of production
  | ProductionSymbol Symbol
  | Choice Production Production -- Alternative productions
  | Optional Production -- Optional production (0 or 1)
  | Repeat Production -- Zero or more repetitions
  | RepeatOneOrMore Production -- One or more repetitions
  deriving (Show, Eq)

-- Symbols that can appear in a production
data Symbol
  = TerminalSymbol String -- Terminal symbol
  | NonterminalSymbol Nonterminal -- Nonterminal symbol
  | Epsilon -- Empty string
  deriving (Show, Eq)

-- Lexer --------------------------------------------------------------------
lexer :: TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style =
      emptyDef
        { Tok.commentLine = "//",
          Tok.reservedOpNames = [":=:", "::=", "|", "*", "+", "?"],
          Tok.identStart = letter <|> char '_',
          Tok.identLetter = alphaNum <|> char '_' <|> char '-'
        }

identifier :: Parser String
identifier = Tok.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

-- commaSep :: Parser a -> Parser [a]
commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

nonTerminalParser :: Parser Symbol
nonTerminalParser = do
  name <- identifier
  return $ NonterminalSymbol name

terminalParser :: Parser Symbol
terminalParser = do
  name <- stringLiteral
  return $ TerminalSymbol name

epsilonParser :: Parser Symbol
epsilonParser = do
  _ <- symbol "epsilon"
  return Epsilon

symbolParser :: Parser Symbol
symbolParser = nonTerminalParser <|> terminalParser <|> epsilonParser

term :: Parser Production
term =
  parens productionParser <|>
  brackets (Sequence <$> commaSep productionParser) <|>
  ProductionSymbol <$> symbolParser

binary name fun assoc = Infix (do reservedOp name; return fun) assoc

prefix name fun = Prefix (do reservedOp name; return fun)

postfix name fun = Postfix (do reservedOp name; return fun)

productionParser :: Parser Production
productionParser = buildExpressionParser operators term
  where
    operators =
      [ [postfix "*" Repeat],
        [postfix "+" RepeatOneOrMore],
        [postfix "?" Optional],
        [binary "|" Choice AssocLeft]
      ]

ruleParser :: Parser Rule
ruleParser = do
  nt <- identifier
  reservedOp "::="
  prod <- productionParser
  return $ Rule nt prod

lexerParser = do
  nt <- identifier
  reservedOp ":=:"
  lx <- stringLiteral
  return $ LexerRule nt lx

programParser :: Parser Program
programParser = do
  whiteSpace
  items <- many (try (Left <$> ruleParser) <|> try (Right <$> lexerParser))
  eof
  let (rules, lexers) = partitionEithers items
  return $ Program rules lexers


-- Parse a string into a Program
parseProgram :: String -> Either ParseError Program
parseProgram = parse programParser ""
