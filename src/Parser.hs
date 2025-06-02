module Parser where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser)
import Text.Parsec.Token qualified as Tok
import Text.Regex.TDFA ((=~))

przykladRegex = do
  let str = "16.531"
  -- print (str =~ "^[b-c]+[0-9]$+" :: Bool)
  print (str =~ "[0-9]+(\\.[0-9]+)?" :: Bool)

-- Abstract Syntax Tree -----------------------------------------------------

-- A Program is a collection of grammar rules and lexer rules
data Program = Program [Rule] [LexerRule]
  deriving (Show, Eq)

-- A grammar rule defines a nonterminal and its productions
-- data Rule = Rule Nonterminal [Production] (Maybe Action)
--   deriving (Show, Eq)
data Rule = Rule Nonterminal Production
  deriving (Show, Eq)

-- A lexer rule defines a token pattern
data LexerRule = LexerRule
  { lexerName   :: String      -- Name of the token (e.g. "Number")
  , lexerRegex  :: String      -- The regex/pattern (e.g. "[0-9]+")
  } deriving (Show, Eq)


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
          Tok.reservedOpNames = [":=:", "::=", "|", "*", "+", "?"],
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

whiteSpace :: ParsecT String () Identity ()
whiteSpace = Tok.whiteSpace lexer

symbol :: String -> ParsecT String () Identity String
symbol = Tok.symbol lexer

-- Przykładowy kod
-- Expr ::= Term (("+" | "-") Term)*
-- Factor ::= Number | Variable | "(" Expr ")"
-- Number :=: [0-9]+(\\.[0-9]+)?
--
-- Program[
--   Rule
--   "Expr"
--   [Sequence[
--     ProductionSymbol (NonterminalSymbol "Term"),
--     Repeat(
--       Sequence[
--         Choice[
--           (ProductionSymbol (Terminal "+")),
--           (ProductionSymbol (Terminal "-"))],
--         ProductionSymbol (Nonterminal "Term")])]],
--   Rule
--   "Factor"
--   (Choice[
--     (ProductionSymbol (Nonterminal "Number")),
--     (ProductionSymbol (Nonterminal "Variable")),
--     (Sequence[
--       (ProductionSymbol (Terminal "(")),
--       (ProductionSymbol (Nonterminal "Expr")),
--       (ProductionSymbol (Terminal ")"))])])]
--  [LexerRule
--   "Number"
--   "[0-9]+(\\.[0-9]+)?"]
