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
  Vector
) where
  
-----------------------------------------------------------
-- | Vector Definition - It all starts here
-----------------------------------------------------------
{-
-- 3D Vector (2D Homogeneous)
data Vec3 = Vec3 Double Double Double
            deriving (Eq)
            
instance Show Vec3 where
  show (Vec3 x y z) = "Vec3(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"
  
instance Num Vec3 where
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
fromVec3 :: Vec3 -> [Double]
fromVec3 (Vec3 x y z) = [x, y, z]
-}
-----------------------------------------------------------
-- Vector Definition and Operations
-----------------------------------------------------------
-- | For now, our vector is a specialised list of numbers.
type Vector = [Double]


-- | Returns the length of the vector in mathematics terms.
-- | That is, sqrt(a*a + b*b + ... + n*n)
vecLength :: Vector -> Double
vecLength v = sqrt $ sum $ map (** 2) v


-- | Returns a vector scaled by a factor.
-- | s * (a, b, c ...) = (s*a, s*b, s*c, ... )
vecScale :: Double -> Vector -> Vector
vecScale s v = map (* s) v


-- | Returns the unit vector.
-- | unit vector = v / |v|, which is the length of the vector.
vecUnit :: Vector -> Vector
vecUnit v = vecScale (1/vecLength v) v 


-- | Returns the dot vector.
-- | [a, b, c ...] dot [x, y, z, ...] = a*x + b*y + c*z + ...
vecDot :: Vector -> Vector -> Double
vecDot u v = sum $ zipWith (*) u v


-- | Returns the cross vector. Note, only defined for a 3-vector!
vecCross :: Vector -> Vector -> Vector
vecCross (x:y:z:u) (i:j:k:v) = [y*k-z*j, x*k-z*i, x*j-y*i]
-----------------------------------------------------------
-- | Line object
-----------------------------------------------------------
{-
data Line2 = Line2 Vec3 Vec3
  deriving (Eq)

instance Show Line2 where
  show (Line2 (Vec3 a b x) (Vec3 c d y)) | x /= 1 || y /= 1 = "Line2(" ++ full ++ ")"
                                         | otherwise        = "Line2(" ++ partial ++ ")"
    where
      full = show a ++ ", " ++ show b ++ ", " ++ show x ++ ")(" ++ show c ++ ", " ++ show d ++ ", " ++ show y
      partial = show a ++ ", " ++ show b ++ ")(" ++ show c ++ ", " ++ show d
  
-- | Creates a new Line2 object
line2 :: Double -> Double -> Double -> Double -> Line2
line2 a b c d = Line2 (Vec3 a b 1) (Vec3 c d 1)

-- | Converts line to a list of Vec3
fromLine2 :: Line2 -> [Vec3]
fromLine2 (Line2 a b) = [a, b] 
-}
-----------------------------------------------------------
-- | Line Operations
-----------------------------------------------------------
{-
class LineOps n v | n -> v where
  -- | Returns the starting vector
  lineStart :: n -> v
  -- | Returns the ending vector
  lineEnd :: n -> v
  -- | Returns the vector represented by the line
  lineVec :: n -> v
  
instance LineOps Line2 Vec3 where
  lineStart (Line2 a _) = a
  lineEnd (Line2 _ b) = b
  lineVec (Line2 a b) = b - a
-}
-----------------------------------------------------------
-- | Round Corner object
-----------------------------------------------------------
{- C-----            C   --
   |    |              --
   |    |  =====>    --
   -----O            .    O
   Represents an 90-degree arc, centred at origin O, with a radius of |C-O|.
   The orientation of the arc is in the bounding box defined by the diagram
   above. 
-}

{-
data RoundCorner = RoundCorner Vec3 Vec3
  deriving (Eq)
  
instance Show RoundCorner where
  show (RoundCorner (Vec3 ox oy oz) (Vec3 cx cy cz)) | oz /= 1 || cz /= 1 = "RoundCorner(" ++ full ++ ")"
                                                     | otherwise          = "RoundCorner(" ++ partial ++ ")"
    where
      full = show ox ++ ", " ++ show oy ++ ", " ++ show oz ++ ")(" ++ show cx ++ ", " ++ show cy ++ ", " ++ show cz
      partial = show ox ++ ", " ++ show oy ++ ")(" ++ show cx ++ ", " ++ show cy
      
-- | Creates a new RoundCorner object, at origin (ox, oy), with bounding box corner (cx, cy)
roundCorner :: Double -> Double -> Double -> Double -> RoundCorner
roundCorner ox oy cx cy = RoundCorner (Vec3 ox oy 1) (Vec3 cx cy 1)

-- | Converts the bounding box to a list of Vec3
fromRoundCorner :: RoundCorner -> [Vec3]
fromRoundCorner (RoundCorner a b) = [a, b]
-}
-----------------------------------------------------------
-- | Bounding Box Operations
-----------------------------------------------------------


-----------------------------------------------------------
-- Unit Testing
-----------------------------------------------------------
same input expected | input == expected = (0, "Passed")
                    | otherwise         = (1, "Failed: " ++ show input ++ " == " ++ show expected)
    
diff input expected | input /= expected = (0, "Passed")
                    | otherwise         = (1, "Failed: " ++ show input ++ " /= " ++ show expected)
  
test = do
  -- Insert test variables here
  let x = [1, 0, 0]
  let y = [0, 1, 0]
  let z = [0, 0, 1]
  let w = [2, 0, 0]
  --let a = line2 0 1 2 3
  --let b = line2 0 1 1 2
  --let c1 = roundCorner 0 1 2 3
  --let c2 = roundCorner 1 1 2 2
  -- Insert unit test here
  let trials = same (vecLength x) (1)
             : diff (vecLength x) (vecLength y + vecLength z)
             : same (vecScale 5 x) [5, 0, 0]
             : same (vecScale 0.5 w) x
             : same (vecUnit w) x
             : diff (vecUnit y) (vecUnit z)
             : same (vecDot x w) 2
             : diff (vecDot y z) 1
             : same (vecCross x y) z
             : diff (vecCross y x) z
             {-
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
             : same (Line2 (Vec3 0 1 1) (Vec3 1 2 1)) b
             : diff a b
             : same (lineStart a) (Vec3 0 1 1)
             : same (lineEnd b) (Vec3 1 2 1)
             : diff (lineVec a) (lineVec b)
             : same (lineVec b) (x + y)
             : same (fromRoundCorner c1) (fromLine2 a)
             : diff (fromRoundCorner c1) (fromRoundCorner c2)
             : same c1 (RoundCorner (Vec3 0 1 1) (Vec3 2 3 1))
             
             -}
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