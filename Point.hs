module Point(Point(..),dist2) where

----------------------------------------------------------------
-- Point arithmetic
----------------------------------------------------------------

data Point = P { x :: !Double, y :: !Double, z :: !Double } deriving Show


-- Compute the square of the distance between two points
dist2 :: Point -> Point -> Double
dist2 (P x1 y1 z1) (P x2 y2 z2) = 
  let x = x1 - x2
      y = y1 - y2
      z = z1 - z2
  in  x * x + y * y + z * z
  --in d `seq` d