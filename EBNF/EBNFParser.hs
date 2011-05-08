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
         -- , opStart = oneOf "|(){}[]+.:=\<>&!;"
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
expression :: Parser [Expression]
expression = do
  a <- alternative
  e <- expression'
  return $ a:e
  
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
      (do s <- stringLiteral lexer
          return $ Terminal s)
  <|> (do s <- identifier lexer
          return $ Nonterminal s)
  <|> (do symbol lexer "$"
          s <- many (noneOf "$")
          symbol lexer "$"
          return $ Special s)
  <|> (do s <- parens lexer (term)
          return $ s)
  <|> (do s <- brackets lexer (term)
          return $ Optional s)
  <|> (do s <- braces lexer (term)
          symbol lexer "+"
          return $ Many s)
  <|> (do s <- braces lexer (term)
          return $ Some s)
  <|> (do symbol lexer "<"
          s1 <- many (noneOf "&")
          symbol lexer "&"
          s2 <- many (noneOf ">")
          symbol lexer ">"
          return $ (Terminal s1) :& (Terminal s2))
  <|> (do symbol lexer "<"
          s1 <- many (noneOf "!")
          symbol lexer "!"
          s2 <- many (noneOf ">")
          symbol lexer ">"
          return $ (Terminal s1) :! (Terminal s2))
        
































