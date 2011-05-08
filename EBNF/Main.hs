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
import System.Directory
import System.Environment
import Wumpus.Core ( writeSVG
                   , writeEPS
                   , DPicture
                   , transform
                   , translationMatrix
                   , rotationMatrix
                   , scalingMatrix
                   )

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
  createDirectoryIfMissing True filepath
  writeSVG (filepath ++ name ++ ".svg") pic
  writeEPS (filepath ++ name ++ ".eps") pic
  where 
    pic = drawProduction p
    filepath = "./output/"
  
drawProduction :: Production -> DPicture
drawProduction (Production name expr metas) =
    transform globalScale
  $ transform globalRotate
  $ transform globalTranslate
  $ fst
  $ drawDiagram name
  $ drawExpression expr
  where
    tau = 2 * pi
    globalTranslate = translationMatrix 0 0
    globalRotate = rotationMatrix 0
    globalScale = scalingMatrix 1 1
  
drawExpression :: Expression -> [Component]
drawExpression e = 
  case e of
    Terminal t -> [drawTerminal t]
    Nonterminal t -> [drawNonterminal t]
    Special t -> [drawSpecial t]
    OR es -> drawAlternative $ concat $ map drawExpression es
    Many e -> drawOneOrMany $ drawExpression e
    Some e -> drawZeroOrMany $ drawExpression e
    Optional e -> drawOptional $ drawExpression e
    Seq es -> drawTerminals $ concat $ map drawExpression $ concat es
    a :& b -> [drawExcept (drawExpression a) "also" (drawExpression b)]
    a :! b -> [drawExcept (drawExpression a) "not" (drawExpression b)]
  
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
        $ drawalternate
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