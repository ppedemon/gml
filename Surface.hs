module Surface(coords, face) where 

import Point
import Object 


{----------------------------------------------------------------
  This module provides the functions for translating object
  coordinates [x,y,z] into texture coordinates [u,v].
  
  Besides, a function for computing the face of the solid
  where the point [x,y,z] belongs is provided.  
----------------------------------------------------------------}


-----------------------------------------------------------------
-- Useful data for finding minimum distances
-----------------------------------------------------------------

data Assoc a b = Assoc a b deriving Show

instance Eq a => Eq (Assoc a b) where
  (Assoc a1 _) == (Assoc a2 _) = a1 == a2
  
instance Ord a => Ord (Assoc a b) where 
  compare (Assoc a1 _) (Assoc a2 _) = compare a1 a2

getkey :: Assoc a b -> a
getkey (Assoc a _) = a

getvalue :: Assoc a b -> b
getvalue (Assoc _ b) = b

getpair :: Assoc a b -> (a,b)
getpair (Assoc a b) = (a,b)


-----------------------------------------------------------------
-- Given object coordinates [x,y,z], compute texture coordinates
-----------------------------------------------------------------

atan2_clamp x z = 
  let r = atan2 x z / (2 * pi)
  in if r >= 0 then r else r + 1
  
sphere_coords :: Point -> (Int,Double,Double)
sphere_coords (P x y z) = (0, atan2_clamp x z, (y + 1) * 0.5)

cube_coords :: Point -> (Int,Double,Double)
cube_coords (P x y z) = 
  let dists = [Assoc (abs z) 0, Assoc (abs (1-z)) 1, 
               Assoc (abs x) 2, Assoc (abs (1-x)) 3, 
               Assoc (abs (1-y)) 4, Assoc (abs y) 5] 
  in case getvalue (minimum dists) of
       0 -> (0,x,y)
       1 -> (1,x,y)
       2 -> (2,z,y)
       3 -> (3,z,y)
       4 -> (4,x,z)
       5 -> (5,x,z)
       
cyl_coords :: Point -> (Int,Double,Double)
cyl_coords (P x y z) = 
  let _y    = 1 - y
      dists = [Assoc (y * y) 2, 
               Assoc (_y * _y) 1, 
               Assoc (abs (x * x + z * z - 1)) 0]
  in case getvalue (minimum dists) of
       0 -> (0, atan2_clamp x z, y)
       1 -> (1, (x + 1) * 0.5, (z + 1) * 0.5)
       2 -> (2, (x + 1) * 0.5, (z + 1) * 0.5)
       
cone_coords :: Point -> (Int,Double,Double)
cone_coords (P x y z) = 
  let _y = 1 - y in
  if _y * _y < abs (x * x + z * z - y * y)
  then (1, (x + 1) * 0.5, (z + 1) * 0.5)
  else (0, atan2_clamp x z, y)
            
plane_coords :: Point -> (Int,Double,Double)
plane_coords (P x y z) = (0, x, z)

coords :: BasicObj -> Point -> (Int,Double,Double)
coords obj p = 
  case kind obj of
    Cone     -> cone_coords   p
    Cube     -> cube_coords   p
    Cylinder -> cyl_coords    p
    Plane    -> plane_coords  p
    Sphere   -> sphere_coords p


-- Compute the solid's face where a point belongs.
-- Avoid lazy pattern matching!
face :: BasicObj -> Point -> Int
face obj p = case coords obj p of (f,_,_) -> f
