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
  -- Uncomment this if you need to expose unit testing
  -- test,
  Vec3,
  fromVec3,
  

) where
  
-----------------------------------------------------------
-- Vector Definition - It all starts here
-----------------------------------------------------------
-- | 3D Vector (2D Homogeneous)
data Vec3 a = Vec3 a a a
            deriving (Eq)
            
instance (Show a) => Show (Vec3 a) where
  show (Vec3 x y z) = "Vec3(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"
  
instance (Num a) => Num (Vec3 a) where
  -- | Vector addition
  Vec3 x y z + Vec3 i j k = Vec3 (x+i) (y+j) (z+k)
  -- | Vector negation / subtraction
  negate (Vec3 x y z) = Vec3 (-x) (-y) (-z) 
  
  -- | Not implemented
  _ * _ = error "(*) is not implemented for Vec3."
  -- | Not implemented
  abs _ = error "abs is not implemented for Vec3."
  -- | Not implemented
  signum _ = error "signum is not implemented for Vec3."
  -- | Not implemented
  fromInteger _ = error "fromInteger is not implemented for Vec3."
  
-- | Converts Vec3 to a list
fromVec3 :: Vec3 a -> [a]
fromVec3 (Vec3 x y z) = [x, y, z]

-----------------------------------------------------------
-- Vector Operations
-----------------------------------------------------------
-- | Vector operations
class VecOps v where
  -- | Calculates the length of the vector
  vecLength :: (Floating a) => v a -> a
  -- | Scales the vector by a factor
  vecScale :: (Num a) => v a -> a -> v a
  -- | Returns the unit vector
  vecUnit :: (Floating a) => v a -> v a
  -- | Dot product
  vecDot :: (Num a) => v a -> v a -> a
  -- | Cross product
  vecCross :: (Num a) => v a -> v a -> v a

instance VecOps Vec3 where
  vecLength (Vec3 x y z) = sqrt(x*x + y*y + z*z)
  vecScale (Vec3 x y z) s = Vec3 (x*s) (y*s) (z*s)
  vecUnit (Vec3 x y z) = Vec3 (x/s) (y/s) (z/s)
    where s = vecLength (Vec3 x y z)
  vecDot (Vec3 x y z) (Vec3 i j k) = x*i + y*j + z*k
  vecCross (Vec3 x y z) (Vec3 i j k) = Vec3 (y*k-z*j) (x*k-z*i) (x*j-y*i)

-- | Vector scaling by a factor

-----------------------------------------------------------
-- Unit Testing
-----------------------------------------------------------
same input expected | input == expected = (0, "Passed")
                    | otherwise         = (1, "Failed: " ++ show input ++ " == " ++ show expected)
    
diff input expected | input /= expected = (0, "Passed")
                    | otherwise         = (1, "Failed: " ++ show input ++ " /= " ++ show expected)
  
test = do
  -- Insert test variables here
  let x = Vec3 1 0 0
  let y = Vec3 0 1 0
  let z = Vec3 0 0 1
  let w = Vec3 2 0 0
  -- Insert unit test here
  let trials = same (x+y) (y+x)
             : diff (x+y) (x+z)
             : same (negate x) ((Vec3 0 0 0)-x)
             : same (fromVec3 x) [1, 0, 0]
             : diff (fromVec3 x) (fromVec3 y)
             : same (vecLength x) (1)
             : diff (vecLength x) (vecLength y + vecLength z)
             : same (vecUnit w) x
             : diff (vecUnit y) (vecUnit z)
             : same (vecDot x w) 2
             : diff (vecDot y z) 1
             : same (vecCross x y) z
             : diff (vecCross y x) z
             : []

  --------------------------
  let failed = sum $ map fst trials
  let msgs = ["  " ++ msg | (flag, msg) <- trials]
  let passed = length trials - failed
  putStrLn "-----------------------"
  putStrLn "Geo.hs:"
  putStr $ unlines $ [show number ++ ":\t" ++ info | (number, info) <- zip [1, 2..] msgs]
  putStrLn ("     " ++ show passed ++ " passed, " ++ show failed ++ " failed.")
  --------------------------