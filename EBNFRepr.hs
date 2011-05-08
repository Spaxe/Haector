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
  , epsilon
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
  , drawZeroOrMany
  , branchZeroOrMany
  , drawAlternative
  , branchAlternative
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

-- | Epsilon
epsilon :: Component
epsilon = hRail 0

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

-- | Zero or many element
zeroOrMany :: [Component] -> [Component]
zeroOrMany components =
  [ drawBranch [hRail width]
  , ldRail
  -- , vRail (-height)
  , drRail
  , inner
  , ruRail
  -- , vRail height
  , ulRail
  ] where
    inner = draw components
    width = (boundaryWidth $ boundary $ fst inner)
    height = (boundaryHeight $ boundary $ fst inner) - default_font_size_px / 2
    
drawZeroOrMany :: [Component] -> [Component]
drawZeroOrMany c = [draw $ zeroOrMany c]

branchZeroOrMany :: [Component] -> [Component]
branchZeroOrMany c = [drawBranch $ optional c]

-- | Alternative elements
alternative :: [Component] -> [Component]
alternative (component:components) =
  [ drawBranch [hRail default_font_size_px, component, hRail trailingWidth]
  , rdRail
  -- , vRail ((-default_font_size_px)/2)
  , draw $ alternative' components maxWidth height
  -- , vRail (default_font_size_px/2)
  , urRail
  ] where
    maxWidth = maximum $ map (boundaryWidth . boundary . fst) (component:components)
    trailingWidth = maxWidth - width + default_font_size_px
    width = boundaryWidth $ boundary $ fst component
    height = boundaryHeight $ boundary $ fst component
    -- offset = (min height default_font_size_px) + default_font_size_px/2
    
alternative' [] _ _ = [epsilon]
alternative' (component:components) maxWidth offset =
  [ vRail (-offset)
  , drawBranch $ alternative' components maxWidth offset'
  , drRail
  , component
  , hRail trailingWidth
  , ruRail
  , vRail offset
  ] where
    trailingWidth = maxWidth - width
    width = boundaryWidth $ boundary $ fst component  
    height = boundaryHeight $ boundary $ fst component
    offset' = (max height default_font_size_px) + default_font_size_px/2


drawAlternative :: [Component] -> [Component]
drawAlternative c = [draw $ alternative c]

branchAlternative :: [Component] -> [Component]
branchAlternative c = [drawBranch $ alternative c]

-- |













  
  
  
  
  
  
  
  
  
  
  
  
  
  
  