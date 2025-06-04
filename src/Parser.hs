module Parser where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser)
import Text.Parsec.Token qualified as Tok
import Text.Regex.TDFA ((=~))

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
data LexerRule = LexerRule
  { lexerName :: String, -- Name of the token (e.g. "Number")
    lexerRegex :: String -- The regex/pattern (e.g. "[0-9]+")
  }
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

brackets :: Parser () -> Parser ()
brackets = Tok.brackets lexer

parens :: Parser () -> Parser ()
parens = Tok.parens lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

-- nonTerminalParser :: Parser Production
-- nonTerminalParser = do
--   name <- identifier
--   return $ ProductionSymbol $ NonterminalSymbol name

-- ternimalParser = do
--   name <- identifier
--   return $ Terminal name

-- term :: Parser Expr
-- term =
--     parens expr
--         <|> Var <$> identifier
--         <|> StringLit <$> stringLiteral
--         <|> IntLit <$> integer
--         <|> randExpr

-- symbolParser :: Parser Symbol
-- symbolParser =
--   NonterminalSymbol <$> identifier <|>
--   TerminalSymbol <$> (symbol "\"" *> identifier <* symbol "\"") <|>
--   pure Epsilon <* symbol "epsilon"


symbolParser :: Parser Production
symbolParser = undefined

binary name fun assoc = Infix (do reservedOp name; return fun) assoc

prefix name fun = Prefix (do reservedOp name; return fun)

postfix name fun = Postfix (do reservedOp name; return fun)

productionParser :: Parser Production
productionParser = buildExpressionParser operators symbolParser
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

programParser = do
  whiteSpace
  rules <- many ruleParser
  lexers <- many lexerParser
  eof
  return $ Program rules lexers

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
