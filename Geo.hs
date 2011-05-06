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

module Geo (
  -- data structures and constants
  rectangle,
  stdAttr,
  -- drawing functions
  drawRect,
  fillRect,
  drawText,
  fillTextBox
) where

import Wumpus.Core
import Wumpus.Core.Colour (black, red, blue)
import Wumpus.Core.FontSize
import System.Directory

-----------------------------------------------------------
-- | Configuration Constants
-----------------------------------------------------------
default_line_width = 1.5
default_font_size = 12
deafult_font_size_px = fromIntegral default_font_size / 3 * 4

-----------------------------------------------------------
-- | Drawing Functions
-----------------------------------------------------------
-- | Draws a rectangle
drawRect :: RGBi -> Double -> Double -> Double -> Double -> DPicture
drawRect rgb a b c d = frame [cstroke rgb stdAttr $ rectangle a b c d]

-- | Draws a filled rectangle
fillRect :: RGBi -> RGBi -> Double -> Double -> Double -> Double -> DPicture
fillRect stroke_rgb fill_rgb a b c d = frame [fillStroke fill_rgb stdAttr stroke_rgb (rectangle a b c d)]

-- | Draws basic text
drawText :: RGBi -> Point2 Double -> String -> DPicture
drawText rgb xy s = frame [escapedlabel rgb stdFont (escapeString s) xy]

-- | Draws a filled textbox
fillTextBox :: RGBi -> RGBi -> Double -> Double -> RGBi -> String -> DPicture
fillTextBox stroke_rgb fill_rgb a b text_rgb s =
  multi [fillRect stroke_rgb fill_rgb a b width height,
         drawText text_rgb (P2 x y) s]
    where
      width = ptSize (textWidth default_font_size (length s + 2))
      height = deafult_font_size_px
      x = a + deafult_font_size_px / 2
      y = b + deafult_font_size_px / 4
  
-----------------------------------------------------------
-- | Drawing Primitives
-----------------------------------------------------------
rectangle :: Double -> Double -> Double -> Double -> DPrimPath
rectangle a b c d = primPath (P2 a b) [lineTo (P2 c b),
                                       lineTo (P2 c d),
                                       lineTo (P2 a d)]
                                       
-----------------------------------------------------------
-- | Defualt stroke style
-----------------------------------------------------------
stdAttr :: StrokeAttr
stdAttr = default_stroke_attr { line_width = default_line_width } 

no_stroke :: StrokeAttr
no_stroke = default_stroke_attr { line_width = 0.0 } 

-----------------------------------------------------------
-- | Defualt font style
-----------------------------------------------------------
stdFont :: FontAttr
stdFont = defaultFont default_font_size
