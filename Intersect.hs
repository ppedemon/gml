module Intersect(intersect_ray) where 

import Point
import Vector 
import Matrix
import Simplify
import Utils(pos_inf)
import Object hiding (union)


{---------------------------------------------------------------
  This is the core of the ray-tracing algorithm: the
  intersection solver. 

  This module implements the machinery for computing 
  intersections between objects (both atomic and composite) 
  and rays shooted from the viewer's position.
---------------------------------------------------------------}


----------------------------------------------------------------
-- First, a little bit of interval algebra. For all 
-- functions, interval lists are assumed to be ordered
----------------------------------------------------------------

data Assoc a b = Assoc a b deriving Show
data Interval a b = Int !(Assoc a b) !(Assoc a b) deriving Show

instance Eq a => Eq (Assoc a b) where
  (Assoc a1 _) == (Assoc a2 _) = a1 == a2

instance Ord a => Ord (Assoc a b) where
  compare (Assoc a1 _) (Assoc a2 _) = compare a1 a2

interval :: (a,b) -> (a,b) -> Interval a b
interval (b,v1) (e,v2) = Int (Assoc b v1) (Assoc e v2)

beg :: Interval a b -> Assoc a b
beg (Int b _) = b

end :: Interval a b -> Assoc a b
end (Int _ e) = e


-- Union of two interval lists
union :: Ord a => [Interval a b] -> [Interval a b] -> [Interval a b]
union [] l2 = l2
union l1 [] = l1
union l1@(i1:is1) l2@(i2:is2)  = 
  case compare (end i1) (beg i2) of
    LT -> i1 : union is1 l2
    _  -> case compare (end i2) (beg i1) of
            LT -> i2 : union l1 is2
            _  -> let b = min (beg i1) (beg i2)
                  in  if end i1 >= end i2
                      then union ((Int b (end i1)) : is1) is2
                      else union is1 ((Int b (end i2)) : is2)


-- Intersection of two interval lists
intersect :: Ord a => [Interval a b] -> [Interval a b] -> [Interval a b]
intersect [] _ = []
intersect _ [] = []
intersect l1@(i1:is1) l2@(i2:is2) = 
  case compare (end i1) (beg i2) of
    LT -> intersect is1 l2
    _  -> case compare (end i2) (beg i1) of
            LT -> intersect l1 is2
            _  -> let b = max (beg i1) (beg i2)
                  in  if end i1 >= end i2
                      then (Int b (end i2)) : intersect l1 is2
                      else (Int b (end i1)) : intersect is1 l2


-- Substract the second interval list to the first one
diff :: Ord a => [Interval a b] -> [Interval a b] -> [Interval a b]
diff [] _ = []
diff l1 [] = l1
diff l1@(i1:is1) l2@(i2:is2) = 
  case compare (end i1) (beg i2) of
    LT -> i1 : diff is1 l2
    _  -> case compare (end i2) (beg i1) of
            LT -> diff l1 is2
            _  -> let pre = if beg i1 < beg i2
                            then [Int (beg i1) (beg i2)]
                            else []
                  in  if end i2 < end i1
                      then pre ++ diff ((Int (end i2) (end i1)) : is1) is2
                      else pre ++ diff is1 l2


----------------------------------------------------------------
-- Compute intersections.
----------------------------------------------------------------

-- Calculate the numerically stable roots of a quadratic equation of 
-- the form ax^2 + 2bx +c. Return nothing if the equation does not 
-- have real solutions. The roots are returned in lexicographical 
-- order.
q_roots :: Double -> Double -> Double -> Maybe (Double,Double)
q_roots a b c 
  | d <= 0    = Nothing   
  | otherwise = 
      let sq  = sqrt d
          (r0,r1) = if b >= 0 
                    then (c / (-b - sq), (-b - sq) / a)
                    else (c / (-b + sq), (-b + sq) / a)
      in Just $! if r0 < r1 then (r0,r1) else (r1,r0)
  where d = b * b - a * c


-- Auxiliary: compute the interval [t0,t1] such that
-- 0 <= p + tv <= 1, where p and v are point and vector
-- projections.
intersect_proj :: BasicObj -> Double -> Double -> [Interval Double BasicObj]
intersect_proj obj p v 
  | p > 1     = if v >= 0 then [] else [interval ((1-p)/v,obj) (-p/v,obj)]
  | p < 0     = if v <= 0 then [] else [interval (-p/v,obj) ((1-p)/v,obj)]
  | otherwise = case compare v 0 of
                  LT -> [interval (0,obj) (-p/v,obj)]
                  EQ -> [interval (0,obj) (pos_inf,obj)]
                  GT -> [interval (0,obj) ((1-p)/v,obj)]
        

-- Compute the intersection between a ray and the plane y = 0
intersect_plane :: BasicObj -> Point -> Vector -> [Interval Double BasicObj]
intersect_plane obj (P _ py _) (V _ vy _)
  | py >= 0 = if vy >= 0 then [] else [interval (t,obj) (pos_inf,obj)]
  | py <  0 = if vy >= 0 then [interval (0,obj) (t,obj)]
                         else [interval (0,obj) (pos_inf,obj)]
  where t = - (py / vy)


-- Compute the intersection between a ray and a cube
intersect_cube :: BasicObj -> Point -> Vector -> [Interval Double BasicObj]
intersect_cube obj (P px py pz) (V vx vy vz) = 
  intersect (intersect_proj obj px vx) 
    (intersect (intersect_proj obj py vy) (intersect_proj obj pz vz))


-- Compute the intersection between a ray and the unit sphere
intersect_sphere :: BasicObj -> Point -> Vector -> [Interval Double BasicObj]
intersect_sphere obj (P px py pz) (V vx vy vz) = 
  let a = vx * vx + vy * vy + vz * vz
      b = px * vx + py * vy + pz * vz
      c = px * px + py * py + pz * pz - 1
  in case q_roots a b c of
       Nothing      -> []
       Just (t0,t1) -> if t1 <= 0
                       then []
                       else if t0 >= 0 
                            then [interval (t0,obj) (t1,obj)]
                            else [interval (0,obj) (t1,obj)]


-- Intersection between a ray and an infinite unit cylinder
_intersect_cyl :: BasicObj -> Point -> Vector -> [Interval Double BasicObj]
_intersect_cyl obj (P px py pz) (V vx vy vz) = 
  let a = vx * vx + vz * vz
      b = px * vx + pz * vz
      c = px * px + pz * pz - 1
  in case q_roots a b c of
       Nothing      -> []
       Just (t0,t1) -> if t1 <= 0 
                       then []
                       else if t0 >= 0 
                            then [interval (t0,obj) (t1,obj)]
                            else [interval (0,obj) (t1,obj)]

-- Intersection with the unit cylinder
intersect_cyl :: BasicObj -> Point -> Vector -> [Interval Double BasicObj]
intersect_cyl obj p@(P _ py _) v@(V _ vy _) = 
  intersect (_intersect_cyl obj p v) (intersect_proj obj py vy) 


-- Intersection between a ray and the infinite unit cone.
-- This is a bitch! There are a lot of degenerated cases 
-- to be considered here.
_intersect_cone :: BasicObj -> Point -> Vector -> [Interval Double BasicObj]
_intersect_cone obj (P px py pz) (V vx vy vz) = 
  let a = vx * vx - vy * vy + vz * vz
      b = px * vx - py * vy + pz * vz
      c = px * px - py * py + pz * pz
  in case compare a 0 of
       EQ -> case compare b 0 of
               EQ -> []
               _  -> intersect_deg c (-0.5 * c / b)
       _  -> case q_roots a b c of
               Nothing    -> []
               Just roots -> intersect_norm c roots
               
  where intersect_deg c t 
          | c < 0     = if t <= 0 then [interval (0,obj) (pos_inf,obj)] 
                                  else [interval (0,obj) (t,obj)]
          | otherwise = if t <= 0 then [] 
                                  else [interval (t,obj) (pos_inf,obj)]
       
        intersect_norm c (t0,t1)
          | t1 <= 0   = if c < 0 then [interval (0,obj) (pos_inf,obj)]
                                 else []
          | t0 >= 0   = if c < 0 then [interval (0,obj) (t0,obj), 
                                       interval (t1,obj) (pos_inf,obj)]
                                 else [interval (t0,obj) (t1,obj)]
          | otherwise = if c < 0 then [interval (0,obj) (t1,obj)]
                                 else [interval (t1,obj) (pos_inf,obj)]

-- Intersection between a ray and the unit cone
intersect_cone :: BasicObj -> Point -> Vector -> [Interval Double BasicObj]
intersect_cone obj p@(P _ py _ ) v@(V _ vy _) =
  intersect (_intersect_cone obj p v) (intersect_proj obj py vy)


-- Compute intersection against composite objects
intersect_comp :: TopObj -> Point -> Vector -> [Interval Double BasicObj]
intersect_comp obj p v
  | not (hits_bs p v obj) = []
  | otherwise = 
    case object obj of
  
      -- Basic objects
      Null    -> []
      Basic o -> let m  = w2o o
                     _p = apply_to_point  m p
                     _v = apply_to_vector m v 
                 in case kind o of
                     Cone     -> intersect_cone   o _p _v
                     Cube     -> intersect_cube   o _p _v
                     Cylinder -> intersect_cyl    o _p _v
                     Plane    -> intersect_plane  o _p _v
                     Sphere   -> intersect_sphere o _p _v
                 
      -- Compound objects
      Union o1 o2        -> 
        union (intersect_comp o1 p v) (intersect_comp o2 p v)
      Intersection o1 o2 -> 
        intersect (intersect_comp o1 p v) (intersect_comp o2 p v)
      Difference o1 o2   -> 
        diff (intersect_comp o1 p v) (intersect_comp o2 p v)
                               

----------------------------------------------------------------
-- Compute the intersection between the ray r = p + vt
-- (p and v with world coordinates) and the given scene.
--
-- Return the intersection 'time', i.e. the value t for
-- the first intersection of the ray, and the intersected
-- object.
--
-- If the boolean flag is false, we can ignore the first
-- object if we are inside, returning the second intersection.
----------------------------------------------------------------

intersect_ray :: TopObj -> Point -> Vector -> Bool -> Maybe (Double, BasicObj)
intersect_ray obj p v initial = 
  case intersect_comp obj p v of
    [] -> Nothing
    (Int (Assoc t o) _ : is) -> 
      if t <= 0 && not initial
      then case is of
             [] -> Nothing
             (Int (Assoc t o) _ : _) -> Just (t,o)
      else Just (t,o)

