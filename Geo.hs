{- |
Module      :  $Header$
Description :  Type definitions for 2D shapes.
Copyright   :  (C) 2011 by Xavier Ho
License     :  MIT License

Maintainer  :  contact@xavierho.com
Stability   :  experimental
Portability :  portable

Definitions for simple 2D shapes such as rectangles and lines.
-}

module Geo
  where
  
-----------------------------------------------------------
-- Vector Definition - It all starts here
-----------------------------------------------------------
data Vec3 a = Vec3 a a a
            deriving (Show, Eq)

instance (Num a) => Num (Vec3 a) where
  (Vec3 i j k) + (Vec3 u v w) = Vec3 (i+u) (j+v) (k+w)