module Vector
 ( Vector(..)
 , (<.>)          -- dot product
 , (<*>)          -- cross product
 , (<+>)          -- addition
 , (<->)          -- substraction
 , between        -- vector between two Points
 , opposite       -- opposite vector
 , len2           -- vector length squared
 , len            -- vector length
 , scale_vect     -- uniform scale
 , unit           -- get the unit vector
 , point_along    -- get a coordinate along a ray
 ) where

import Point

infixl 6 <.>
infixl 5 <*>
infixl 4 <+>,<->

----------------------------------------------------------------
-- Vector arithmetic
----------------------------------------------------------------

data Vector = V { 
  dx :: !Double, 
  dy :: !Double, 
  dz :: !Double 
} deriving Show


----------------------------------------------------------------
-- Some vector products
----------------------------------------------------------------

-- Dot product
(<.>) :: Vector -> Vector -> Double
(V x1 y1 z1) <.> (V x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

-- Cross product
(<*>) :: Vector -> Vector -> Vector
(V x1 y1 z1) <*> (V x2 y2 z2) = 
  let x = y1 * z2 - z1 * y2
      y = z1 * x2 - x1 * z2
      z = x1 * y2 - y1 * x2
  in  V x y z


----------------------------------------------------------------
-- Some arithmetic operations over vectors
----------------------------------------------------------------

-- Addition
(<+>) :: Vector -> Vector -> Vector
(V x1 y1 z1) <+> (V x2 y2 z2) = V (x1 + x2) (y1 + y2) (z1 + z2)

-- Substraction
(<->) :: Vector -> Vector -> Vector
(V x1 y1 z1) <-> (V x2 y2 z2) = V (x1 - x2) (y1 - y2) (z1 - z2)

-- For a pair of points (p,q) compute a vector z st: q = p + z
between :: Point -> Point -> Vector
between (P x1 y1 z1) (P x2 y2 z2) = V (x2 - x1) (y2 - y1) (z2 - z1)

-- Opposite vector
opposite :: Vector -> Vector
opposite (V x y z) = V (-x) (-y) (-z)

  
----------------------------------------------------------------
-- Compute vector length
----------------------------------------------------------------

len2 :: Vector -> Double
len2 (V x y z) = x * x + y * y + z * z

len :: Vector -> Double
len v = sqrt (len2 v)


----------------------------------------------------------------
-- Normalize vectors
----------------------------------------------------------------

-- Uniform scale
scale_vect :: Vector -> Double -> Vector
scale_vect (V x y z) s = V (x * s) (y * s) (z * s)

-- Get a unit vector
unit :: Vector -> Vector
unit v = scale_vect v (1 / len v)


-- Utility function: given a scalar t, an origin
-- point p and a direction vector v, compute the
-- coordinates of the point p + tv
point_along :: Point -> Vector -> Double -> Point
point_along (P px py pz) (V vx vy vz) t = 
  P (px + t * vx) (py + t * vy) (pz + t * vz)
