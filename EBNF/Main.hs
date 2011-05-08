{- |
Module      :  Main.hs
Description :  EBNF Testing
Copyright   :  (C) 2011 by Xavier Ho
License     :  MIT License

Maintainer  :  contact@xavierho.com
Stability   :  unstable
Portability :  portable

Main module for Haector, mainly for testing for now.
This program requires Wumpus-0.43.0.
-}

module Main where

import EBNFRepr
import EBNFParser
import Text.Parsec

import Wumpus.Core
import System.Directory
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  mapM_ parseFile args

parseFile :: FilePath -> IO ()
parseFile path = do
  input <- readFile path
  result <- return (runParser grammar () path input)
  case result of
    Left err -> print err
    Right (Grammar ps) -> mapM_ outputProduction ps

outputProduction :: Production -> IO ()
outputProduction p@(Production name expr metas) = do
  print p
    
{-
parseGrammar :: Expression -> IO ()
parseGrammar e = do
  print e
  print "End."
-}
  
{-
-----------------------------------------------------------
-- | Drawing Test
-----------------------------------------------------------
tau = 2 * pi
globalTranslate = translationMatrix 0 10
globalRotate = rotationMatrix 0
globalScale = scalingMatrix 2 2
name = "test"

main :: IO ()
main = do
  let filepath = "./output/"
  createDirectoryIfMissing True filepath
  let r = transform globalScale 
        $ transform globalRotate
        $ transform globalTranslate
        $ fst
        $ drawDiagram name
        -- $ drawOptional
        -- $ drawOneOrMany
        $ drawAlternative
          [ terminal "GLADoS"
          , nonterminal "The"
          , drawExcept
            [special "cake"]
            "not"
            [ drawExcept
              [special "fuzzy dice"]
              "and"
              [special "Companion Cube"]
            ]
          , nonterminal "is"
          , nonterminal "a"
          , special "lie"
          , terminal "."
          ]

  writeSVG (filepath ++ name ++ ".svg") r
  writeEPS (filepath ++ name ++ ".eps") r
-}