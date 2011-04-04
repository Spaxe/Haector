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
  _ * _ = error "(*) is undefined for Vec3"
  
  
-----------------------------------------------------------
-- Unit Testing
-----------------------------------------------------------
same input expected | input == expected = (0, "Passed")
                    | otherwise         = (1, "Failed: " ++ show input ++ " == " ++ show expected)
    
diff input expected | input /= expected = (0, "Passed")
                    | otherwise         = (1, "Failed: " ++ show input ++ " /= " ++ show expected)
  
test = do
  -- Insert unit test here
  let trials = same (x+y) (y+x)
             : diff (x+y) (x+y)
             : diff (3*3) (x+y)
             : []

  --------------------------
  let failed = sum $ map fst trials
  let msgs = ["  " ++ msg | (flag, msg) <- trials, flag /= 0]
  let passed = length trials - failed
  putStrLn "Geo.hs:"
  putStr $ unlines msgs
  putStrLn ("     " ++ show passed ++ " passed, " ++ show failed ++ " failed.")
    where -- Insert variables below
      x = Vec3 1 0 0
      y = Vec3 0 1 0
      z = Vec3 0 0 1