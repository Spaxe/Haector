{- |
Module      :  EBNFRepr.hs
Description :  Special representation for EBNF grammar
Copyright   :  (C) 2011 by Xavier Ho
License     :  MIT License

Maintainer  :  contact@xavierho.com
Stability   :  unstable
Portability :  portable

For Representing EBNF Grammar; not officially part of this library.
-}

module EBNFRepr
  ( terminal
  , nonterminal
  , special
  , hRail
  , vRail
  , rdRail
  , drRail
  , ruRail
  , ulRail
  , ldRail
  , label
  -- Exposing Geo methods
  , draw
  -- Extended Geo methods
  , drawTerminals
  , branchTerminals
  , drawOneOrMany
  , branchOneOrMany
  , drawDiagram
  , branchDiagram
  , drawOptional
  , branchOptional
  ) where

import Geo

import Wumpus.Core
import Wumpus.Core.Colour (RGBi(..), black)

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

-- | Draws a black text
label :: String -> Component
label = text black

-- | Draws a terminal
terminal :: String -> Component
terminal = roundTextBox black green black

-- | Draws a non-terminal
nonterminal :: String -> Component
nonterminal = textBox black orange black

-- | Draws a special non-terminal
special :: String -> Component
special = textBox black yellow black

-- | Draws a horizontal connector
hRail :: Double -> Component
hRail = hLine black

-- | Draws a vertical connector
vRail :: Double -> Component
vRail = vLine black

-- | rail turns
rdRail = rightRoundDown black
drRail = downRoundRight black
ruRail = rightRoundUp black
ulRail = upRoundLeft black
ldRail = leftRoundDown black
urRail = upRoundRight black

-- | Adds a begin and an end rail
diagram :: String -> [Component] -> [Component]
diagram s components =
  [label s]
  ++ [hRail default_font_size_px]
  ++ (terminals components)
  ++ [hRail default_font_size_px]
  
drawDiagram :: String -> [Component] -> Component
drawDiagram s = (draw . diagram s)

branchDiagram :: String -> [Component] -> Component
branchDiagram s = (drawBranch . diagram s)
  
-- | Extends the draw function to add head and tail rails
terminals :: [Component] -> [Component]
terminals (component:components) =
  terminals' components [component]
    where
    terminals' [] connected = connected
    terminals' (component:components) connected
      = terminals' components (connected ++ [hRail (default_font_size_px/2), component])
      
drawTerminals :: [Component] -> [Component]
drawTerminals c = [draw $ terminals c]

branchTerminals :: [Component] -> [Component]
branchTerminals c = [drawBranch $ terminals c]

-- | One or many repeated elements
oneOrMany :: [Component] -> [Component]
oneOrMany components =
  [ inner
  , ldRail
  , vRail (-height)
  , drRail
  , hRail width
  , ruRail
  , vRail height
  , ulRail
  ] where
    inner = drawBranch components
    width = boundaryWidth $ boundary $ fst inner
    height = (boundaryHeight $ boundary $ fst inner) - default_font_size_px / 2
    
drawOneOrMany :: [Component] -> [Component]
drawOneOrMany c = [draw $ oneOrMany c]
  
branchOneOrMany :: [Component] -> [Component]
branchOneOrMany c = [drawBranch $ oneOrMany c]

-- | Optional element
optional :: [Component] -> [Component]
optional components =
  [ optionalComponents
  , hRail outer_width
  ] where
    inner = draw components
    optionalComponents = drawBranch
      [ rdRail
      -- , vRail (-height)
      , drRail
      , inner
      , ruRail
      -- , vRail height
      , urRail
      ]
    width = boundaryWidth $ boundary $ fst inner
    outer_width = boundaryWidth $ boundary $ fst optionalComponents
    height = (boundaryHeight $ boundary $ fst inner) - default_font_size_px / 2

drawOptional :: [Component] -> [Component]
drawOptional c = [draw $ optional c]

branchOptional :: [Component] -> [Component]
branchOptional c = [drawBranch $ optional c]



























  
  
  
  
  
  
  
  
  
  
  
  
  
  
  