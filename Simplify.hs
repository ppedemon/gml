module Simplify(simplify,hits_bs) where 

import Maybe(fromMaybe)

import Point
import Vector
import Matrix
import Object
import Utils(pos_inf)

 
{----------------------------------------------------------------
  Implement a simple bounding sphere checking.
  
  Motivation: we don't want to test ray intersection for
  EVERY point in a scene. Instead, sorround and node of the
  scene tree with a bounding sphere, and test whether the
  ray intersects the sphere.
  
  Provided that we only want to test intersection, rather
  than the intersection time, the test is very unexpensive.
----------------------------------------------------------------}


-- Define bounding spheres for basic objects.
-- We define'em by a means of tuple (center, radius)
cone_bs   = (P 0 1 0, 1)
cube_bs   = (P 0.5 0.5 0.5, sqrt 3 / 2)
cyl_bs    = (P 0 0.5 0, sqrt 5 / 2)
plane_bs  = (origin, pos_inf)
sphere_bs = (origin, 1)


-- Wrap an object inside an infinite sphere
wrap_inf :: TopObj -> TopObj
wrap_inf obj = 
  TopObj { 
    object = object obj, 
    center = origin, 
    radius = pos_inf
  }


-----------------------------------------------------------------
-- A little piece of "sphere arithmetic"
-----------------------------------------------------------------

union_bs :: TopObj -> TopObj -> TopObj
union_bs o1 o2
  | r1 >= pos_inf || r2 >= pos_inf = TopObj (Union o1 o2) origin pos_inf
  | otherwise =
    let c1   = center o1
        c2   = center o2
        c1c2 = between c1 c2
        d2   = len2 c1c2
        rr   = r2 - r1
        rr2  = rr * rr
    in if d2 <= rr2          -- One sphere inside the other
       then if r1 <= r2 
            then TopObj (Union o1 (wrap_inf o2)) c2 r2
            else TopObj (Union (wrap_inf o1) o2) c1 r1
       else let d      = sqrt d2
                alpha  = rr / (2 * d) + 0.5
                center = point_along c1 c1c2 alpha
                radius = (d + r1 + r2) * 0.5
            in TopObj (Union o1 o2) center radius
      
  where r1 = radius o1
        r2 = radius o2
        
 
-- This function is a bitch! There are a lot of cases
-- that we might want to check to avoid doing extra work
intersect_bs :: TopObj -> TopObj -> TopObj
intersect_bs o1 o2
  | r1 >= pos_inf = TopObj (Intersection o1 o2) c2 r2
  | r2 >= pos_inf = TopObj (Intersection o1 o2) c1 r1
  | otherwise =
    let c1c2 = between c1 c2
        dd2  = len2 c1c2
        rr   = r2 - r1
        rr2  = rr * rr
    in if dd2 <= rr2      -- One sphere inside the other
       then
         if r2 <= r1 
         then TopObj (Intersection o1 (wrap_inf o2)) c2 r2
         else TopObj (Intersection (wrap_inf o1) o2) c1 r1
       else 
         if dd2 > rpr * rpr  
         then null_obj    -- No intersection
         else let d      = sqrt dd2
                  t1     = r1 + d + r2
                  t2     = r1 + d - r2
                  t3     = r2 + d - r1
                  t4     = r1 + r2 - d 
                  radius = sqrt ( t1 * t2 * t3 * t4) / (2 * d)
                  alpha  = (r1 * r1 - r2 * r2) / (2 * dd2) + 0.5
                  center = point_along c1 c1c2 alpha
              in TopObj (Intersection o1 o2) center radius
                       
  where c1  = center o1
        c2  = center o2
        r1  = radius o1
        r2  = radius o2
        rpr = r1 + r2


diff_bs :: TopObj -> TopObj -> TopObj
diff_bs o1 o2 = 
  TopObj (Difference (wrap_inf o1) o2) (center o1) (radius o1)
  

-----------------------------------------------------------------
-- Compute bounding spheres, simplifying the tree.
--
-- We shall postpone top level unions with planes as much as 
-- possible, hence avoiding early appearance of infinite
-- spheres in the simplified tree.
-----------------------------------------------------------------

not_origin :: Point -> Bool
not_origin (P 0 0 0) = False
not_origin _         = True

-- Compute the union of the given object with the 
-- given list of planes
add_planes :: TopObj -> [TopObj] -> TopObj
add_planes o ps = 
  case ps of
    []     -> o
    (p:ps) -> 
      case object o of
        Null -> add_planes p ps
        _    -> add_planes (p { object = Union o p }) ps

-- Auxliary function for computing the bs for a basic object
basic_bs :: TopObj -> BasicObj -> (Point,Double) -> TopObj
basic_bs o b (c,r) = o { center = bc, radius = br }    
  where bc = apply_to_point (o2w b) c
        br = scale_fact b * r

compute_bs :: TopObj -> (Maybe TopObj, [TopObj])
compute_bs o
  | not_origin (center o) = (Just o, []) 
  | otherwise =
    case object o of
    
      Null    -> (Just o,[])
      
      Basic b -> 
        case kind b of
          Cone     -> (Just $ basic_bs o b cone_bs  , [])
          Cube     -> (Just $ basic_bs o b cube_bs  , [])
          Cylinder -> (Just $ basic_bs o b cyl_bs   , [])
          Sphere   -> (Just $ basic_bs o b sphere_bs, [])
          Plane    -> (Nothing,  [basic_bs o b plane_bs])
          
      Union o1 o2 ->
        let (m1,p1) = compute_bs o1
            (m2,p2) = compute_bs o2
        in case (m1,m2) of
             (Nothing, Nothing) -> (Nothing, p1 ++ p2)
             (Just t1, Nothing) -> (Just t1, p1 ++ p2)
             (Nothing, Just t2) -> (Just t2, p1 ++ p2)
             (Just t1, Just t2) -> (Just $ union_bs t1 t2, p1 ++ p2)
      
      Intersection o1 o2 ->
        let (m1,p1) = compute_bs o1
            (m2,p2) = compute_bs o2
            t1 = add_planes (fromMaybe null_obj m1) p1
            t2 = add_planes (fromMaybe null_obj m2) p2
        in case object t1 of
             Null -> (Nothing, [])
             _    -> case object t2 of
                       Null -> (Nothing, [])
                       _    -> (Just $ intersect_bs t1 t2, [])
                       
      Difference o1 o2 ->
        let (m1,p1) = compute_bs o1
            (m2,p2) = compute_bs o2
            t2 = add_planes (fromMaybe null_obj m2) p2
        in case object t2 of
             Null -> (m1,p1)
             _    -> let t1 = add_planes (fromMaybe null_obj m1) p1
                     in case object t1 of
                          Null -> (Nothing, [])
                          _    -> (Just $ diff_bs t1 t2, [])
     
-- Finally, simplify a given scene
simplify :: TopObj -> TopObj
simplify o = 
  case compute_bs o of
    (t,p) -> add_planes (fromMaybe null_obj t) p


-----------------------------------------------------------------
-- Test whether the given ray hits the bounding sphere of the
-- given object. This is a BIG performance enhancement, since
-- this test is very unexpensive to compute.
-----------------------------------------------------------------
    
hits_bs :: Point -> Vector -> TopObj -> Bool
hits_bs (P px py pz) (V vx vy vz) o = 
  radius o >= pos_inf || _hits_bs
    where _hits_bs = 
            case object o of
              Null -> False
              _    -> let P cx cy cz = center o
                          r = radius o
                          x = px - cx
                          y = py - cy
                          z = pz - cz
                          a = vx * vx + vy * vy + vz * vz 
                          b = x * vx + y * vy + z * vz
                          c = x * x + y * y + z * z - r * r
                      in b * b > a * c

                          
