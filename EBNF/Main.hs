{- |
Module      :  Main.hs
Description :  Functions for drawing simple 2D geometry.
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

import Wumpus.Core
import System.Directory

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