module Matrix
  ( Matrix(..)
  , id_matrix          -- the identity matrix
  , translate          -- translation matrix
  , scale              -- scale matrix
  , rotate_x           -- rotation in the x-axis
  , rotate_y           -- rotation in the y-axis
  , rotate_z           -- rotation in the z-axis
  , compose            -- compose two transformations (bah, product)
  , apply_to_point     -- apply a transformation to a point
  , apply_to_vector    -- apply a transformation to a vector
  ) where 

import Utils
import Point(Point(..))
import Vector(Vector(..))


----------------------------------------------------------------
-- Some matrix algebra for you to enjoy
----------------------------------------------------------------

-- We are using a deliberately compact representation.
-- Since the fourth row of a transformation matrix is
-- always = [0,0,0,1] we can omit it, provided we are
-- willing to modify our product algorithm.

data Matrix = M {
  xx :: !Double, xy :: !Double, xz :: !Double, xt :: !Double,
  yx :: !Double, yy :: !Double, yz :: !Double, yt :: !Double, 
  zx :: !Double, zy :: !Double, zz :: !Double, zt :: !Double
  -- fourth row omitted!
} deriving Show


-- The identity matrix
id_matrix :: Matrix
id_matrix = M {
  xx = 1, xy = 0, xz = 0, xt = 0,
  yx = 0, yy = 1, yz = 0, yt = 0,
  zx = 0, zy = 0, zz = 1, zt = 0
}

-- A matrix for translations
translate :: Double -> Double -> Double -> Matrix
translate tx ty tz = M {
  xx = 1, xy = 0, xz = 0, xt = tx,
  yx = 0, yy = 1, yz = 0, yt = ty,
  zx = 0, zy = 0, zz = 1, zt = tz
}

-- A non-uniform scale matrix
scale :: Double -> Double -> Double -> Matrix
scale sx sy sz = M {
  xx = sx, xy = 0 , xz = 0 , xt = 0,
  yx = 0 , yy = sy, yz = 0 , yt = 0,
  zx = 0 , zy = 0 , zz = sz, zt = 0
}


-- The three rotation matrices

rotate_x :: Double -> Matrix
rotate_x theta = 
  let { c = deg_cos theta; s = deg_sin theta }
  in M {
    xx = 1, xy = 0, xz = 0 , xt = 0,
    yx = 0, yy = c, yz = -s, yt = 0,
    zx = 0, zy = s, zz = c , zt = 0
  }

rotate_y :: Double -> Matrix
rotate_y theta = 
  let { c = deg_cos theta; s = deg_sin theta }
  in M {
    xx = c , xy = 0,  xz = s, xt = 0,
    yx = 0 , yy = 1,  yz = 0, yt = 0,
    zx = -s, zy = 0,  zz = c, zt = 0
  }
  
rotate_z :: Double -> Matrix
rotate_z theta = 
  let { c = deg_cos theta; s = deg_sin theta }
  in M {
    xx = c, xy = -s, xz = 0, xt = 0,
    yx = s, yy = c , yz = 0, yt = 0,
    zx = 0, zy = 0 , zz = 1, zt = 0
  }


-- Matrix product
compose :: Matrix -> Matrix -> Matrix
compose m n = M {
  xx = xx m * xx n + xy m * yx n + xz m * zx n,
  xy = xx m * xy n + xy m * yy n + xz m * zy n,
  xz = xx m * xz n + xy m * yz n + xz m * zz n,
  xt = xx m * xt n + xy m * yt n + xz m * zt n + xt m,
  
  yx = yx m * xx n + yy m * yx n + yz m * zx n,
  yy = yx m * xy n + yy m * yy n + yz m * zy n,
  yz = yx m * xz n + yy m * yz n + yz m * zz n,
  yt = yx m * xt n + yy m * yt n + yz m * zt n + yt m,
  
  zx = zx m * xx n + zy m * yx n + zz m * zx n,
  zy = zx m * xy n + zy m * yy n + zz m * zy n,
  zz = zx m * xz n + zy m * yz n + zz m * zz n,
  zt = zx m * xt n + zy m * yt n + zz m * zt n + zt m
}


-- Apply transformations to both points and vectors.
-- Note that vectors are not modified by transformations

apply_to_point :: Matrix -> Point -> Point
apply_to_point m (P px py pz) = P { 
  x = xx m * px + xy m * py + xz m * pz + xt m,
  y = yx m * px + yy m * py + yz m * pz + yt m,
  z = zx m * px + zy m * py + zz m * pz + zt m
}

apply_to_vector :: Matrix -> Vector -> Vector
apply_to_vector m (V vx vy vz) = V {
  dx = xx m * vx + xy m * vy + xz m * vz,
  dy = yx m * vx + yy m * vy + yz m * vz,
  dz = zx m * vx + zy m * vy + zz m * vz
}
