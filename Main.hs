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

import Geo

import Wumpus.Core
import Wumpus.Core.Colour (black, red, blue, green, yellow)
import System.Directory

-----------------------------------------------------------
-- | Drawing Test
-----------------------------------------------------------
main :: IO ()
main = do
  let filepath = "./output/"
  createDirectoryIfMissing True filepath
  let r = multi [
                 --fillTextBox black yellow 0 0 black "This is a text.",
                 drawRoundRect black 6 6 60 18,
                 drawRoundRect red 6 6 18 18
                ]
  writeSVG (filepath ++ "test.svg") r
  writeEPS (filepath ++ "test.eps") r