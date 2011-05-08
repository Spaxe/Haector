{- |
Module      :  Geo.hs
Description :  Functions for drawing simple 2D geometry.
Copyright   :  (C) 2011 by Xavier Ho
License     :  MIT License

Maintainer  :  contact@xavierho.com
Stability   :  unstable
Portability :  portable

The Geo module provides a series of basic shapes drawing functions.
-}

module Geo
  ( 
  -- main drawing handler
  draw
  , drawBranch
  -- component-level drawing functions
  , hLine
  , vLine
  , textBox
  , roundTextBox
  , rightRoundDown
  , downRoundRight
  , rightRoundUp
  , upRoundLeft
  , leftRoundDown
  , upRoundRight
  , text
  -- component type
  , Component
  -- basic drawing functions
  -- , drawLine
  -- , drawRect
  , drawDashedRect
  -- , fillRect
  -- , drawText
  -- , drawRoundRect
  -- , fillRoundRect
  -- metrics
  , default_font_size
  , default_line_width
  , default_font_size_px
  ) where

import Wumpus.Core
import Wumpus.Core.Colour (black, red, blue)
import Wumpus.Core.FontSize
import System.Directory

-----------------------------------------------------------
-- | Configuration Constants
-----------------------------------------------------------
default_font_size = 12
default_line_width = fromIntegral default_font_size / 12
default_font_size_px = fromIntegral default_font_size / 3 * 4
tau = 2 * pi

-----------------------------------------------------------
-- | Main Drawing Handler
-----------------------------------------------------------
-- | Draws a set of components
draw :: [Component] -> Component
draw components = draw' components identityMatrix [] (V2 0 0)

-- | Draws a set of components, but does not change the next starting point
drawBranch :: [Component] -> Component
drawBranch components = (fst $ draw' components identityMatrix [] (V2 0 0), (V2 0 0))

-- | Draw helper
draw' :: [Component] -> DMatrix3'3 -> [DPicture] -> DVec2 -> Component
draw' [] _ pictures point = (multi pictures, point)
draw' (component:components) matrix pictures (V2 x y) = 
  let shifted_picture = transform matrix $ fst component
      tx = vector_x $ snd component
      ty = vector_y $ snd component
      new_matrix = matrix * translationMatrix tx ty
  in draw' components new_matrix (pictures ++ [shifted_picture]) (V2 (x+tx) (y+ty))

-----------------------------------------------------------
-- | Component Drawing Functions
-- |
-- | All relative drawings start on the left middle point,
-- | at (0, 0). i.e.
-- |
-- |  +-------------------------+
-- |--Start                   End-------
-- |  +-------------------------+
-- | 
-- | All return types are a tuple of (Picture, Vec2).
-- | Picture is the Wumpus Picture, ready to be rendered.
-- | Point 2 is the end point of the picture, can be used
-- | to connect to the next picture.
-----------------------------------------------------------
type Component = (DPicture, DVec2)

-- | Draws a horizontal line
hLine :: RGBi -> Double -> Component
hLine rgb len = 
  ( drawLine rgb 0 0 len 0
  , V2 len 0
  )

-- | Draws a vertical line
vLine :: RGBi -> Double -> Component
vLine rgb len =
  ( drawLine rgb 0 0 0 len
  , V2 0 len
  )
  
-- | Draws text
text :: RGBi -> String -> Component
text text_rgb s =
  ( t
  , V2 0 (-height)
  ) where
    t = drawText text_rgb (P2 x y) s
    width = (boundaryWidth $ boundary t) + padding * 2
    height = (boundaryHeight $ boundary t) + padding
    x = padding + 0.5
    y = -padding
    padding = default_font_size_px / 4

-- | Draws a filled textbox
textBox :: RGBi -> RGBi -> RGBi -> String -> Component
textBox stroke_rgb fill_rgb text_rgb s =
  ( multi [box, t] 
  , V2 width 0
  ) where
    baseline = -height/2
    topline = height/2
    box = fillRect stroke_rgb fill_rgb 0 baseline width topline
    t = drawText text_rgb (P2 x y) s
    width = (boundaryWidth $ boundary t) + padding * 2
    height = (boundaryHeight $ boundary t) + padding
    x = padding + 0.5
    y = -padding
    padding = default_font_size_px / 4
   
          
-- | Draw a filled, founded textbox.
-- | The first point must be the bottom left corner, and then the top right corner.
roundTextBox :: RGBi -> RGBi -> RGBi -> String -> Component
roundTextBox stroke_rgb fill_rgb text_rgb s = 
  ( multi [box, t]
  , V2 width 0
  ) where
    baseline = -height/2
    topline = height/2
    box = fillRoundRect stroke_rgb fill_rgb 0 baseline width topline
    t = drawText text_rgb (P2 x y) s
    width = (boundaryWidth $ boundary t) + padding * 2
    height = (boundaryHeight $ boundary t) + padding
    x = padding + 0.5
    y = -padding
    padding = default_font_size_px / 4
    
-- | --+
-- |   v
-- | Draws a rounded corner
rightRoundDown :: RGBi -> Component
rightRoundDown rgb = 
  ( frame [ostroke rgb std_stroke $ path] 
  , V2 r (-r)
  ) where
    r = default_font_size_px / 2
    path = primPath (P2 0 0) [arcTo r 0 (-r) (tau/4) 0]
    
-- | |
-- | +->
-- | Draws a rounded corner
downRoundRight :: RGBi -> Component
downRoundRight rgb = 
  ( frame [ostroke rgb std_stroke $ path] 
  , V2 r (-r)
  ) where
    r = default_font_size_px / 2
    path = primPath (P2 0 0) [arcTo r r 0 (tau/2) (tau*3/4)]
    
-- |   ^
-- | --+
-- | Draws a rounded corner
rightRoundUp :: RGBi -> Component
rightRoundUp rgb = 
  ( frame [ostroke rgb std_stroke $ path] 
  , V2 r r
  ) where
    r = default_font_size_px / 2
    path = primPath (P2 0 0) [arcTo r 0 r (tau*3/4) tau]
    
-- | <-+
-- |   |
-- | Draws a rounded corner
upRoundLeft :: RGBi -> Component
upRoundLeft rgb = 
  ( frame [ostroke rgb std_stroke $ path] 
  , V2 (-r) r
  ) where
    r = default_font_size_px / 2
    path = primPath (P2 0 0) [arcTo r (-r) 0 0 (tau/4)]
    
-- | +--
-- | v 
-- | Draws a rounded corner
leftRoundDown :: RGBi -> Component
leftRoundDown rgb = 
  ( frame [ostroke rgb std_stroke $ path] 
  , V2 (-r) (-r)
  ) where
    r = default_font_size_px / 2
    path = primPath (P2 0 0) [arcTo r 0 (-r) (tau/4) (tau/2)]

-- | +->
-- | | 
-- | Draws a rounded corner
upRoundRight :: RGBi -> Component
upRoundRight rgb = 
  ( frame [ostroke rgb std_stroke $ path] 
  , V2 r r
  ) where
    r = default_font_size_px / 2
    path = primPath (P2 0 0) [arcTo r r 0 (tau/2) (tau/4)]

    
-----------------------------------------------------------
-- | Absolute Drawing Functions
-----------------------------------------------------------
-- | Draws a line
drawLine :: RGBi -> Double -> Double -> Double -> Double -> DPicture
drawLine rgb a b c d = 
  frame [ostroke rgb std_stroke $ line a b c d]

-- | Draws a rectangle
drawRect :: RGBi -> Double -> Double -> Double -> Double -> DPicture
drawRect rgb a b c d = 
  frame [cstroke rgb std_stroke $ rectangle a b c d]
  
-- | Draws a dashed rectangle
drawDashedRect :: RGBi -> Double -> Double -> Double -> Double -> DPicture
drawDashedRect rgb a b c d = 
  frame [cstroke rgb dashed_stroke $ rectangle a b c d]

-- | Draws a filled rectangle
fillRect :: RGBi -> RGBi -> Double -> Double -> Double -> Double -> DPicture
fillRect stroke_rgb fill_rgb a b c d = 
  frame [fillStroke fill_rgb std_stroke stroke_rgb (rectangle a b c d)]

-- | Draws basic text
drawText :: RGBi -> DPoint2 -> String -> DPicture
drawText rgb xy s = 
  frame [textlabel rgb stdFont s xy]

-- | Draw a rounded rectangle.
-- | The first point must be the bottom left corner, and then the top right corner.
drawRoundRect :: RGBi -> Double -> Double -> Double -> Double -> DPicture
drawRoundRect rgb a b c d = 
  frame [cstroke rgb std_stroke $ roundRectangle a b c d]

-- | Draw a filled, rounded rectangle.
-- | The first point must be the bottom left corner, and then the top right corner.
fillRoundRect :: RGBi -> RGBi -> Double -> Double -> Double -> Double -> DPicture
fillRoundRect stroke_rgb fill_rgb a b c d = 
  frame [fillStroke fill_rgb std_stroke stroke_rgb (roundRectangle a b c d)]

-----------------------------------------------------------
-- | Drawing Primitives
-----------------------------------------------------------
line :: Double -> Double -> Double -> Double -> DPrimPath
line a b c d = primPath (P2 a b) [lineTo (P2 c d)]

rectangle :: Double -> Double -> Double -> Double -> DPrimPath
rectangle a b c d = primPath (P2 a b) [ lineTo (P2 c b)
                                      , lineTo (P2 c d)
                                      , lineTo (P2 a d)]

arcTo radius x y ang1 ang2 = curveTo b c d
  where
  (a, b, c, d) = bezierArc radius ang1 ang2 (P2 x y)

-- | Draws a rectangle where the left and right edges are semi-circles.
-- | A square will yield a circle.
-- | The first point must be the bottom left corner, and then the top right corner.
roundRectangle :: Double -> Double -> Double -> Double -> DPrimPath
roundRectangle a b c d = 
  primPath (P2 (start_x+line_len) start_y) 
    [ arcTo radius (c-radius) (b+radius) (tau*3/4) tau
    , arcTo radius (c-radius) (d-radius) 0 (tau/4)
    , lineTo (P2 (start_x) d)
    , arcTo radius (a+radius) (d-radius) (tau/4) (tau/2)
    , arcTo radius (a+radius) (b+radius) (tau/2) (tau*3/4)
    ] where
      radius = abs((d-b)/2)
      start_x = a + radius
      start_y = b
      line_len = max 0 (c - a - radius*2)
                                      
                                       
-----------------------------------------------------------
-- | Defualt stroke style
-----------------------------------------------------------
std_stroke :: StrokeAttr
std_stroke = default_stroke_attr { line_width = default_line_width } 

dashed_stroke :: StrokeAttr
dashed_stroke = default_stroke_attr { line_width = default_line_width,
                                      dash_pattern = 
                                        Dash 1 [(1, 0), (0, 1)]
                                    } 

no_stroke :: StrokeAttr
no_stroke = default_stroke_attr { line_width = 0.0 } 

-----------------------------------------------------------
-- | Defualt font style
-----------------------------------------------------------
stdFont :: FontAttr
stdFont = defaultFont default_font_size
