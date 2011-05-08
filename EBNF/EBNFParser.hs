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
         -- , opStart = oneOf "|({[+.:\\<&!;"
         -- , opLetter = oneOf ")}]:="
         , caseSensitive = True
         }

-- Creates token parsers
lexer :: TokenParser ()  
lexer = makeTokenParser styleDef

-----------------------------------------------------------
-- | Parsers
-----------------------------------------------------------
{-
grammar :: Parser Grammar
grammar = do
  whiteSpace lexer
  many production
-}

{-
production :: Parser Production
production = do
  name <- identifier lexer
  exp <-
-}

-- Expression
expression :: Parser Expression
expression = do
  a <- alternative
  e <- expression'
  return $ OR $ a:e
  
expression' :: Parser [Expression]
expression' = 
      (do symbol lexer "|"
          a <- alternative
          e <- expression'
          return $ a:e)
   <|> return []
  
alternative :: Parser Expression
alternative = do
  t <- many1 term
  a <- alternative'
  return $ Seq $ t:a

alternative' :: Parser [[Expression]]
alternative' = do
  symbol lexer "\\"
  t <- many1 term
  return [t]
  
-- Term
term :: Parser Expression
term = 
      try (do s <- stringLiteral lexer
              return $ Terminal s)
  <|> try (do s <- identifier lexer
              return $ Nonterminal s)
  <|> try (do symbol lexer "$"
              s <- many (noneOf "$")
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
              symbol lexer "}"
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


























