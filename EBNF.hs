{- |
Module      :  EBNF.hs
Description :  Special representation for EBNF grammar
Copyright   :  (C) 2011 by Xavier Ho
License     :  MIT License

Maintainer  :  contact@xavierho.com
Stability   :  unstable
Portability :  portable

For Representing EBNF Grammar; not officially part of this library.
-}

module EBNF
  ( terminal
  , nonterminal
  , special
  ) where

import Geo

import Wumpus.Core.Colour (RGBi, black)

-- Some nice colours
-- Green
green = RGBi 85 192 71
-- Orange
orange = RGBi 219 121 50
-- Yellow
yellow = RGBi 246 243 54
-- Red
red = RGBi 210 25 24
-- Blue
blue = RGBi 45 48 222

-- | Draws a terminal
terminal :: String -> Component
terminal = roundTextBox black green black

-- | Draws a non-terminal
nonterminal :: String -> Component
nonterminal = textBox black orange black

-- | Draws a special non-terminal
special :: String -> Component
special = textBox black yellow black