module Object 

  -- Datatypes for surface functions
  ( SurfaceRes(..)
  , SurfaceFun(..)
  
  -- Datatypes for objects
  , ObjKind(..)
  , BasicObj(..)
  , Object(..)
  , TopObj(..)
  
  -- Utility functions
  , origin
  , null_obj
  
  -- primitive object constructors
  , cone
  , cube
  , cylinder
  , plane
  , sphere
  
  -- object composition
  , union
  , intersection
  , difference
  
  -- object transformations
  , translateObj
  , scaleObj
  , uscaleObj
  , rotateObj_x
  , rotateObj_y
  , rotateObj_z
  
  -- compute the normal vector to an object
  , normalVector  
  ) where

import Array

import Point
import Vector
import Matrix


{---------------------------------------------------------------
  This module provides defintions for the objects composing
  a scene to be rendered. On the other hand, functions for
  manipulating such objects are provided as well.
---------------------------------------------------------------}

----------------------------------------------------------------
-- Surface functions
----------------------------------------------------------------

data SurfaceRes = SurfaceRes { c :: !Point, kd, ks, phong :: !Double }
                     deriving Show
                  
data SurfaceFun = Const !SurfaceRes
                | Array !(Array Int SurfaceRes)
                | Fun   (Int -> Double -> Double -> IO SurfaceRes)

instance Show SurfaceFun where 
  showsPrec p (Const s) = showString "Const" . showsPrec p s
  showsPrec p (Array a) = showString "Array" . showsPrec p a
  showsPrec p (Fun _)   = showString "<Fun>"


----------------------------------------------------------------
-- Objects
----------------------------------------------------------------
          
-- Object kind
data ObjKind = Cone | Cube | Cylinder | Plane | Sphere deriving Show

-- A primitive object
data BasicObj = BasicObj { 
  kind       :: !ObjKind,     -- kind
  sfun       :: !SurfaceFun,  -- surface function for procedural mapping
  w2o, o2w   :: !Matrix,      -- world to obj/obj to world transformations
  scale_fact :: !Double       -- overall scale factor
} 

instance Show BasicObj where
  showsPrec n b = 
    showsPrec n (kind b) . showChar '\n' .
    showsPrec n (w2o b)  . showChar '\n' .
    showsPrec n (o2w b)
    --showsPrec n (sfun b) . showChar ' ' .
    --showsPrec n (scale_fact b)


-- An object can be null, primitive, or a combination 
-- (union, intersection, difference) of two top level 
-- objects (objects held inside bounding spheres).

data Object = Null
            | Basic !BasicObj
            | Union !TopObj !TopObj
            | Intersection !TopObj !TopObj
            | Difference !TopObj !TopObj
            deriving Show
         
data TopObj = TopObj {
  object :: !Object,       -- object held inside a sphere
  center :: !Point,        -- sphere's center
  radius :: !Double        -- sphere's radius
} deriving Show


----------------------------------------------------------------
-- Create the basic objects that can be rendered
----------------------------------------------------------------

origin = P 0 0 0

null_obj :: TopObj
null_obj = TopObj { object = Null, center = origin, radius = 0 }

top_obj :: ObjKind -> SurfaceFun -> TopObj
top_obj kind sfun = null_obj { 
  object = Basic $ BasicObj {
    kind       = kind,
    sfun       = sfun,
    w2o        = id_matrix,
    o2w        = id_matrix,
    scale_fact = 1
  }
}

cone     = top_obj Cone
cube     = top_obj Cube
cylinder = top_obj Cylinder
plane    = top_obj Plane
sphere   = top_obj Sphere


----------------------------------------------------------------
-- Composition operations
----------------------------------------------------------------

union :: TopObj -> TopObj -> TopObj
union o1 o2 = null_obj { object = Union o1 o2 }

intersection :: TopObj -> TopObj -> TopObj
intersection o1 o2 = null_obj { object = Intersection o1 o2 }

difference :: TopObj -> TopObj -> TopObj
difference o1 o2 = null_obj { object = Difference o1 o2 }


----------------------------------------------------------------
-- Affine transformations
----------------------------------------------------------------

transform :: Matrix -> Matrix -> Double -> TopObj -> TopObj
transform m m_inv fact obj = 
  case object obj of
  
    -- transform basic objects
    Null        -> obj
    Basic b_obj -> 
      null_obj {
        object = Basic $ 
        b_obj {
          w2o = compose (w2o b_obj) m_inv,
          o2w = compose m (o2w b_obj),
          scale_fact = scale_fact b_obj * fact
        }       
      }             
                   
    -- transform composite objects
    Union o1 o2 -> 
      let _o1 = transform m m_inv fact o1
          _o2 = transform m m_inv fact o2
      in  obj { object = Union _o1 _o2 }
      
    Intersection o1 o2 -> 
      let _o1 = transform m m_inv fact o1
          _o2 = transform m m_inv fact o2
      in  obj { object = Intersection _o1 _o2 }
      
    Difference o1 o2 -> 
      let _o1 = transform m m_inv fact o1
          _o2 = transform m m_inv fact o2
      in  obj { object = Difference _o1 _o2 }


translateObj :: Double -> Double -> Double -> TopObj -> TopObj
translateObj tx ty tz = 
  transform (translate tx ty tz) (translate (-tx) (-ty) (-tz)) 1

scaleObj :: Double -> Double -> Double -> TopObj -> TopObj
scaleObj sx sy sz = 
  transform (scale sx sy sz) (scale (1/sx) (1/sy) (1/sz)) 
            (max sx (max sy sz))

uscaleObj :: Double -> TopObj -> TopObj
uscaleObj s = 
  let s_inv = 1/s
  in  transform (scale s s s) (scale s_inv s_inv s_inv) s
  
rotateObj_x :: Double -> TopObj -> TopObj
rotateObj_x theta = transform (rotate_x theta) (rotate_x (-theta)) 1

rotateObj_y :: Double -> TopObj -> TopObj
rotateObj_y theta = transform (rotate_y theta) (rotate_y (-theta)) 1
  
rotateObj_z :: Double -> TopObj -> TopObj
rotateObj_z theta = transform (rotate_z theta) (rotate_z (-theta)) 1


----------------------------------------------------------------
-- Compute a normal vector to an object
----------------------------------------------------------------

-- Compute a normal vector to the given object
-- at the given point. 
-- NOTE: Both the point and the vector are 
-- expressed in *object* coordinates
normal_vect :: BasicObj -> Point -> Int -> Vector
normal_vect obj (P x y z) face = 
  case kind obj of
  
    Cone -> if face == 0 
            then V x (-y) z          -- side of the cone
            else V 0 1 0             -- top of the cone
            
    Cube -> case face of
              0 -> V 0 0 (-1)        -- from face
              1 -> V 0 0 1           -- rear face
              2 -> V (-1) 0 0        -- left face
              3 -> V 1 0 0           -- right face
              4 -> V 0 1 0           -- top face
              5 -> V 0 (-1) 0        -- bottom face
              
    Cylinder -> case face of
                  0 -> V x 0 z       -- side of the cylinder
                  1 -> V 0 1 0       -- top face
                  2 -> V 0 (-1) 0    -- bottom face
                  
    Plane  -> V 0 1 0
    
    Sphere -> V x y z
    

-- Given a vector, return two non-colinear vectors in the plane
-- orthogonal to this vector. The product of the two tangent 
-- vectors must point in the same direction as the normal vector.
-- 
-- If you ain't a faithful person, calculate the cross products 
-- and convince yourself!
tangent_vects (V dx dy dz)
  | dy >  0 = (V dy (-dx) 0, V 0 dz (-dy))    -- product = y[x,y,z]
  | dy == 0 = (V dz 0 (-dx), V dz 1 (-dx))    -- product = [x,0,z] = [x,y,z]
  | dy <  0 = (V dy (-dx) 0, V 0 (-dz) dy)    -- product = (-y)[x,y,z]


-- Compute the unit normal vector to the given object,
-- at the given point. Both the point and the vector
-- are expressed in *world* coordinates.
normalVector :: BasicObj -> Point -> Int -> Vector
normalVector obj p face = 
  let _p = apply_to_point (w2o obj) p
      _n =  normal_vect obj _p face
  in case tangent_vects _n of
       (_v1,_v2) -> 
         let m  = o2w obj
             v1 = apply_to_vector m _v1
             v2 = apply_to_vector m _v2
         in unit (v1 <*> v2)
