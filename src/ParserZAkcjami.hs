module ParserInny where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser)
import Text.Parsec.Token qualified as Tok

-- Abstract Syntax Tree -----------------------------------------------------

-- A Program is a collection of grammar rules and lexer rules
data Program = Program [Rule] [LexerRule]
  deriving (Show, Eq)

-- A grammar rule defines a nonterminal and its productions
-- data Rule = Rule Nonterminal [Production] (Maybe Action)
--   deriving (Show, Eq)
data Rule = Rule Nonterminal [Production]
  deriving (Show, Eq)

-- A lexer rule defines a token pattern
data LexerRule = LexerRule String String
  deriving (Show, Eq)

-- A nonterminal symbol in the grammar
-- data Nonterminal = Nonterminal String
--   deriving (Show, Eq)
type Nonterminal = String

-- Core production types
data Production
  = Sequence [Production]              -- Sequence of production
  | ProductionSymbol Symbol
  | Choice [Production]            -- Alternative productions
  | Optional Production            -- Optional production (0 or 1)
  | Repeat Production              -- Zero or more repetitions
  | RepeatOneOrMore Production     -- One or more repetitions
  deriving (Show, Eq)

-- Symbols that can appear in a production
data Symbol
  = Terminal String                -- Terminal symbol
  | NonterminalSymbol Nonterminal  -- Nonterminal symbol
  -- | ActionSymbol Production Action -- Production with associated action
  | Epsilon                        -- Empty string
  deriving (Show, Eq)

-- Action associated with a rule or production
-- data Action = Action String
--   deriving (Show, Eq)
-- type Action = String

-- Lexer --------------------------------------------------------------------
lexer :: TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style =
      emptyDef
        { Tok.commentLine = "//",
          Tok.reservedOpNames = [":=:", "::=", "|", "*", "+", "?", "@", ";"],
          Tok.identStart = letter <|> char '_',
          Tok.identLetter = alphaNum <|> char '_' <|> char '-'
        }

identifier :: ParsecT String () Identity String
identifier = Tok.identifier lexer

reservedOp :: String -> ParsecT String () Identity ()
reservedOp = Tok.reservedOp lexer

stringLiteral :: ParsecT String () Identity String
stringLiteral = Tok.stringLiteral lexer

brackets :: ParsecT String () Identity a -> ParsecT String () Identity a
brackets = Tok.brackets lexer

parens :: ParsecT String () Identity a -> ParsecT String () Identity a
parens = Tok.parens lexer

semi :: ParsecT String () Identity String
semi = Tok.semi lexer

whiteSpace :: ParsecT String () Identity ()
whiteSpace = Tok.whiteSpace lexer

symbol :: String -> ParsecT String () Identity String
symbol = Tok.symbol lexer

-- actionParser :: Parser Action
-- actionParser = do
--   reservedOp "@"
--   name <- identifier
--   return $ name
