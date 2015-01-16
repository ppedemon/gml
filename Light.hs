module Light
  ( Light             -- expoted type
  , directional       -- create directional light source
  , point             -- create point light source
  , spot              -- create a spotlight
  , lights_contrib    -- compute overall light contribution
  ) where 

import Utils
import Point
import Vector
import Object
import Intersect


{----------------------------------------------------------------
  This module deals with the distinct kind of lights, 
  providing functions for creating them, and also supplying 
  the arithmetic routines for light calculations.
----------------------------------------------------------------}

-- Three kinds of light: directional, pointlights, spotlights
data Light = Dir  { dir :: !Point, color :: !Point }
           | Pun  { org :: !Point, color :: !Point }
           | Spot { org    :: !Point,
                    at     :: !Point,
                    color  :: !Point,
                    cutoff :: !Double,
                    exp    :: !Double,
                    back   :: !Vector  -- unit vector towards origin
                  } deriving Show
                  

-----------------------------------------------------------------
-- Create lights
-----------------------------------------------------------------

directional :: Point -> Point -> Light
directional = Dir

point :: Point -> Point -> Light
point = Pun 

spot :: Point -> Point -> Point -> Double -> Double -> Light
spot org at color cutoff exp = 
  Spot org at color cutoff exp (unit (between at org))
  

-----------------------------------------------------------------
-- Compute the contribution of the lights for a given ray
-----------------------------------------------------------------

-- Null contribution
no_contrib = P 0 0 0    -- black


-- Direction from the intersection point towards a light source
get_dir :: Point -> Light -> Vector
get_dir p l = 
  case l of
    Dir  (P x y z) _   -> V (-x) (-y) (-z)
    Pun  org _         -> between p org
    Spot org _ _ _ _ _ -> between p org


-- True if the given ray og light is blocked by the scene
is_blocked :: TopObj -> Light -> Point -> Vector -> Bool
is_blocked scene l p v =
  case intersect_ray scene p v False of
    Nothing    -> False
    Just (t,_) -> case l of
                    Dir _ _ -> True
                    _       -> t < 1


light_contrib :: TopObj    -- Scene being rendered
              -> Point     -- Intersection point (world coordinates)
              -> Vector    -- Ray's direction
              -> Vector    -- Normal at intersection point
              -> Double    -- Diffuse reflection coefficient
              -> Double    -- Specular reflection coefficient
              -> Double    -- Phong exponent
              -> Light     -- Light whose contribution will be computed
              -> Point     -- Resulting color
light_contrib scene p v n kd ks exp l = 
  let dir = get_dir p l in
  if dir <.> n <= 0 
    then no_contrib 
    else if is_blocked scene l p dir
           then no_contrib
           else let lv = unit dir
                    hv = unit (lv <-> unit v)
                in  case light_int l p n lv hv of
                      Nothing -> no_contrib
                      Just i  -> 
                        let m = kd * (n <.> lv) + ks * (n <.> hv) ** exp 
                        in P (m * x i) (m * y i) (m * z i)


-- Compute the light intensity
light_int :: Light -> Point -> Vector -> Vector -> Vector -> Maybe Point
light_int l p n lv hv = 
  case l of 
  
    Dir _ color   -> Just color
    
    Pun org color -> 
      let att = 100 / (99 + dist2 p org)
      in  Just (P (x color * att) (y color * att) (z color * att))
      
    Spot org at color cutoff exp back -> 
      let dp = lv <.> back
      in if deg_acos dp >= cutoff 
         then Nothing
         else let att = (dp ** exp) * (100 / (99 + dist2 p org))
              in  Just (P (x color * att) (y color * att) (z color * att))
              

-- Contribution for the whole set of lights in a scene.
-- Same type as light_contrib, excepts for the multiple lights.
lights_contrib :: TopObj 
               -> Point 
               -> Vector 
               -> Vector 
               -> Double
               -> Double
               -> Double
               -> [Light]
               -> Point
lights_contrib scene p v n kd ks exp ls = 
  foldl sp no_contrib $ map (light_contrib scene p v n kd ks exp) ls
    where sp (P x1 y1 z1) (P x2 y2 z2) = P (x1 + x2) (y1 + y2) (z1 + z2)
