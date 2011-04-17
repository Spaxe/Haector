{-# OPTIONS -Wall #-}

{- |
Module      :  $Header$
Description :  Functions for drawing simple 2D geometry.
Copyright   :  (C) 2011 by Xavier Ho
License     :  MIT License

Maintainer  :  contact@xavierho.com
Stability   :  unstable
Portability :  portable
-}

module Geo where

import Wumpus.Core as WC



-----------------------------------------------------------
-- Unit Testing
-----------------------------------------------------------
same input expected | input == expected = (0, "Passed")
                    | otherwise         = (1, "Failed: " ++ show input ++ " == " ++ show expected)
    
diff input expected | input /= expected = (0, "Passed")
                    | otherwise         = (1, "Failed: " ++ show input ++ " /= " ++ show expected)
  
test = do
  -- Insert test variables here

  -- Insert unit test here
  let trials = []

  --------------------------
  let failed = sum $ map fst trials
  let msgs = ["  " ++ msg | (flag, msg) <- trials]
  let passed = length trials - failed
  putStrLn "-----------------------"
  putStrLn "Geo.hs:"
  putStr $ unlines $ [show number ++ ":\t" ++ info | (number, info) <- zip [1..] msgs]
  putStrLn ("     " ++ show passed ++ " passed, " ++ show failed ++ " failed.")
  --------------------------