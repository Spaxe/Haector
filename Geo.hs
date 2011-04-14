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
-- | Line
-----------------------------------------------------------
-- | A line has a starting point and an end point
-- | For a 2D Line, always use a 3rd extra component with value 1, like:
-- | Line [1, 2, 1] [5, 3, 1]
-- | For affine transformation purposes.
data Line = Line {
            lineStart :: Vector,
            lineEnd :: Vector 
            } deriving (Eq, Show)


-- | Converts the line to a list of vectors
fromLine :: Line -> [Vector]
fromLine a = [lineStart a, lineEnd a]


-- | Returns the vector that represents the line at origin.
lineVec :: Line -> Vector
lineVec (Line a b) = zipWith (-) b a
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
  let x = [1, 0, 1]
  let y = [0, 1, 1]
  let z = [0, 0, 1]
  let w = [2, 0, 1]
  let x0 = [1, 0, 0]
  let y0 = [0, 1, 0]
  let a = Line x y
  let b = Line z y
  --let c1 = roundCorner 0 1 2 3
  --let c2 = roundCorner 1 1 2 2
  -- Insert unit test here
  let trials = same (vecLength x) (sqrt(2.0))
             : diff (vecLength x) (vecLength y + vecLength z)
             : same (vecScale 5 x) [5, 0, 5]
             : same (vecScale 0.5 w) [1, 0, 0.5]
             : same (vecUnit z) z
             : diff (vecUnit y) (vecUnit z)
             : same (vecDot x w) 3
             : diff (vecDot y z) 2
             : same (vecCross x0 y0) z
             : diff (vecCross y0 x0) z
             : same (fromLine a) ([x, y])
             : same (lineVec a) [-1, 1, 0]
             : same (lineVec b) [0, 1, 0]
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