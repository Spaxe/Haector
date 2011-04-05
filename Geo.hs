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

module Geo (
  Vec3,
  test
) where
  
-----------------------------------------------------------
-- Vector Definition - It all starts here
-----------------------------------------------------------
data Vec3 = Vec3 Double Double Double
            deriving (Eq)
            
instance Show Vec3 where
  show (Vec3 a b c) = "Vec3(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"
  
instance Num Vec3 where
  Vec3 a b c + Vec3 i j k = Vec3 (a+i) (b+j) (c+k)
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
             : diff (x+y) (x+z)
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