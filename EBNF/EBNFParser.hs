{- |
Module      :  EBNFParser.hs
Description :  EBNF Parser using Parsec.
Copyright   :  (C) 2011 by Xavier Ho
License     :  MIT License

Maintainer  :  contact@xavierho.com
Stability   :  unstable
Portability :  portable

Parses Andrew Rock's EBNF syntax format as specified in
http://www.ict.griffith.edu.au/arock/haskell/index.html
using Parsec.
-}

module EBNFParser where
  
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Text.Parsec.Language (emptyDef)

-----------------------------------------------------------
-- | Lexer
-----------------------------------------------------------
-- Data types
-- Taken from Andrew Rock's Syntrax for compatibility
type Nonterminal = String
type Terminal = String
type Special = String

infix 5 :&, :!
data Expression = Terminal Terminal
                | Nonterminal Nonterminal
                | Special Special
                | OR [Expression]
                | Many Expression
                | Some Expression
                | Optional Expression
                | Seq [[Expression]]
                | Expression :& Expression
                | Expression :! Expression
  deriving (Show)

data Production = Production Nonterminal Expression [(Nonterminal, Terminal)]
  deriving (Show)
  
data Grammar = Grammar [Production]
  deriving (Show)
  
-- | Language style definition
styleDef :: LanguageDef st
styleDef = emptyDef
         { commentLine = "#"
         , nestedComments = False
         , identStart = letter <|> digit <|> (oneOf "_") <|> (oneOf "'")
         , identLetter = alphaNum <|> oneOf "_"
         , opStart = oneOf "|({[+.:\\<&!;"
         , opLetter = oneOf ")}]:="
         , caseSensitive = True
         }

-- Creates token parsers
lexer :: TokenParser ()  
lexer = makeTokenParser styleDef

-----------------------------------------------------------
-- | Parsers
-----------------------------------------------------------
grammar :: Parser Grammar
grammar = do
  whiteSpace lexer
  p <- many production
  return $ Grammar p

-- Production
production :: Parser Production
production = do
  name <- identifier lexer
  symbol lexer "::="
  expr <- expression
  metas <- many metadata
  symbol lexer "."
  return $ Production name expr metas
  
metadata :: Parser (Nonterminal, Terminal)
metadata = do
  symbol lexer ";"
  n <- identifier lexer
  symbol lexer "="
  t <- stringLiteral lexer
  return (n, t)
  
-- Expression
expression :: Parser Expression
expression = do
  a <- alternate
  e <- expression'
  if length e == 0 then
    return a
  else  
    return $ OR $ a:e
  
expression' :: Parser [Expression]
expression' = 
      try (do symbol lexer "|"
              a <- alternate
              e <- expression'
              return $ a:e)
  <|> return []
  
alternate :: Parser Expression
alternate = do
  t <- terms
  a <- alternate'
  if length t > 1 then
    if length a > 0 then
      return $ Seq $ t:a
    else
      return $ Seq [t]
  else
    return $ head t

alternate' :: Parser [[Expression]]
alternate' = 
  (do t <- many alternate''
      return t)
  <|> return []
  
alternate'' :: Parser [Expression]
alternate'' = do
  symbol lexer "\\"
  t <- terms
  return t
  
-- Term
terms :: Parser [Expression]
terms = many1 term

term :: Parser Expression
term = 
      try (do s <- terminal
              return s)
  <|> try (do s <- nonterminal
              return s)
  <|> try (do symbol lexer "$"
              s <- many1 (noneOf "$")
              symbol lexer "$"
              return $ Special s)
  <|> try (do symbol lexer "("
              s <- expression
              symbol lexer ")"
              return $ s)
  <|> try (do symbol lexer "["
              s <- expression
              symbol lexer "]"
              return $ Optional s)
  <|> try (do symbol lexer "{"
              s <- expression
              symbol lexer "}+"
              return $ Many s)
  <|> try (do symbol lexer "{"
              s <- expression
              symbol lexer "}"
              return $ Some s)
  <|> try (do symbol lexer "<"
              s1 <- expression
              symbol lexer "&"
              s2 <- expression
              symbol lexer ">"
              return $ s1 :& s2)
  <|> try (do symbol lexer "<"
              s1 <- expression
              symbol lexer "!"
              s2 <- expression
              symbol lexer ">"
              return $ s1 :! s2)

-- Terminal
terminal :: Parser Expression
terminal = do
  s <- stringLiteral lexer
  return $ Terminal s

-- Nonterminal
nonterminal :: Parser Expression
nonterminal = do
  s <- identifier lexer
  return $ Nonterminal s






















