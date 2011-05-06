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
  -- data structures and constants
  ( rectangle
  , std_stroke
  -- drawing functions
  , drawRect
  , fillRect
  , drawText
  , fillTextBox
  , drawRoundRect
  , fillRoundRect
  , fillRoundTextBox
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
deafult_font_size_px = fromIntegral default_font_size / 3 * 4
tau = 2 * pi

-----------------------------------------------------------
-- | Drawing Functions
-----------------------------------------------------------
-- | Draws a rectangle
drawRect :: RGBi -> Double -> Double -> Double -> Double -> DPicture
drawRect rgb a b c d = 
  frame [cstroke rgb std_stroke $ rectangle a b c d]

-- | Draws a filled rectangle
fillRect :: RGBi -> RGBi -> Double -> Double -> Double -> Double -> DPicture
fillRect stroke_rgb fill_rgb a b c d = 
  frame [fillStroke fill_rgb std_stroke stroke_rgb (rectangle a b c d)]

-- | Draws basic text
drawText :: RGBi -> Point2 Double -> String -> DPicture
drawText rgb xy s = 
  frame [escapedlabel rgb stdFont (escapeString s) xy]

-- | Draws a filled textbox
fillTextBox :: RGBi -> RGBi -> Double -> Double -> RGBi -> String -> DPicture
fillTextBox stroke_rgb fill_rgb a b text_rgb s =
  multi [ fillRect stroke_rgb fill_rgb a b (a+width) (b+height)
        , text
        ] where
          text = drawText text_rgb (P2 x y) s
          width = (boundaryWidth $ boundary text) + padding * 2
          height = (boundaryHeight $ boundary text) + padding
          x = a + padding
          y = b + padding
          padding = deafult_font_size_px / 4
          string = escapeString s
        
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
  
-- | Draw a filled, founded textbox.
-- | The first point must be the bottom left corner, and then the top right corner.
fillRoundTextBox :: RGBi -> RGBi -> Double -> Double -> RGBi -> String -> DPicture
fillRoundTextBox stroke_rgb fill_rgb a b text_rgb s = 
  multi [ fillRoundRect stroke_rgb fill_rgb a b (a+width) (b+height)
        , text
        ] where
          text = drawText text_rgb (P2 x y) s
          width = (boundaryWidth $ boundary text) + padding * 2
          height = (boundaryHeight $ boundary text) + padding
          x = a + padding
          y = b + padding
          padding = deafult_font_size_px / 4
          string = escapeString s
            


-----------------------------------------------------------
-- | Drawing Primitives
-----------------------------------------------------------
rectangle :: Double -> Double -> Double -> Double -> DPrimPath
rectangle a b c d = primPath (P2 a b) [ lineTo (P2 c b)
                                      , lineTo (P2 c d)
                                      , lineTo (P2 a d)]

-- topLeftArc :: Double -> Point2 Double -> AbsPathSegment Double
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

no_stroke :: StrokeAttr
no_stroke = default_stroke_attr { line_width = 0.0 } 

-----------------------------------------------------------
-- | Defualt font style
-----------------------------------------------------------
stdFont :: FontAttr
stdFont = defaultFont default_font_size
