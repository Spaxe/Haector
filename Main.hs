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

import EBNF

import Wumpus.Core
import System.Directory

-----------------------------------------------------------
-- | Drawing Test
-----------------------------------------------------------
tau = 2 * pi
globalTranslate = translationMatrix 0 10
globalRotate = rotationMatrix 0
globalScale = scalingMatrix 2 2

main :: IO ()
main = do
  let filepath = "./output/"
  createDirectoryIfMissing True filepath
  let r = transform globalScale 
        $ transform globalRotate
        $ transform globalTranslate
        $ fst $ draw
        [ hRail 10
        , terminal "+"
        ]
  writeSVG (filepath ++ "test.svg") r
  writeEPS (filepath ++ "test.eps") r
  
{-
        [ hLine black 10
        , textBox black yellow black "Mrraa"
        , hLine black 50
        , drawBranch [ vLine black (-50)
                     , textBox black yellow black "Nested"
                     , hLine black 50
                     , roundTextBox black yellow black "#"
                     , vLine black 50
                     ]
        , draw [ hLine black 150 ]
        , hLine black 25
        , textBox black yellow black "The cake is a lie"
        , hLine black 50
        , roundTextBox black yellow black "The quick brown fox jumps over the lazy dog"
        , hLine black 10
        , roundTextBox black yellow black "q"
        ]
-}