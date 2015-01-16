module Render(render) where

import IO
import Char
import Array
import IOExts
import Monad(when)

import Utils
import Point
import Light
import Vector
import Matrix
import Object
import Surface
import Simplify
import Intersect


{----------------------------------------------------------------
  Rendering routine entry point.
----------------------------------------------------------------}

type RGB = (Double,Double,Double)

-- Useful CAF's
no_color      = (0,0,0)    -- black
horizon_color = (0,0,0)    -- black
inside_color  = (0,0,0)    -- black

-- Origin point. Also a CAF
origin_point = P 0 0 (-1)

-- Clamp double inside the [0,255] integer range
clamp :: Double -> Int
clamp d
  | i < 0     = 0
  | i > 255   = 255
  | otherwise = i `seq` i
  where i = truncate (d * 255)

-- A tail recursive monadic iterator        
loopWhile :: a -> (a -> Bool) -> (a -> a) -> (a -> IO()) -> IO ()
loopWhile curr cond next action
  | cond curr = do action curr
                   loopWhile (next curr) cond next action
  | otherwise = return ()      

-- Print to stdout a progress percent number
print_progress :: Int -> IO ()
print_progress percent = 
   do putStr (show percent ++ "%\r")
      hFlush stdout


----------------------------------------------------------------
-- Do the rendering for every pixel in the scene
----------------------------------------------------------------

-- Simplify scene, then render it
render amb ls obj depth fov w h file = 
  _render amb ls (simplify obj) depth fov w h file


_render :: Point     -- Ambient lighting
        -> [Light]   -- Lights in the scene
        -> TopObj    -- Scene to render
        -> Int       -- Rendering depth
        -> Double    -- Field of view
        -> Int       -- Image widht
        -> Int       -- Image height
        -> String    -- Output file's name
        -> IO ()
_render amb ls obj depth fov w h file = 
  do f <- openFile file WriteMode
     hSetBuffering f (BlockBuffering (Just 4096))
     c <- newIORef 0.0
     let np = fromIntegral (h * w)
     let w2 = fromIntegral w / 2
     let h2 = fromIntegral h / 2
     let sc = deg_tan (fov / 2) / w2
     hPutStrLn f "P6"
     hPutStrLn f "# Rendered by Pablo J. Pedemonte"
     hPutStrLn f $ show w ++ " " ++ show h
     hPutStrLn f "255"
     let i = (0.5 - h2) * sc
     loopWhile (h-1) (>=0) (+(-1)) 
       (\y -> render_row (i + fromIntegral y * sc) f w2 sc np c)
     hFlush f
     hClose f    
   
  where 
  
    -- Render a single row
    render_row :: Double         -- Y coordinate of the row being rendered
               -> Handle         -- File handle where to store the result
               -> Double         -- Width / 2
               -> Double         -- Scale factor
               -> Double         -- Number of point to render (as a float)
               -> IORef Double   -- Progress counter
               -> IO ()       
    render_row y f w2 sc np c =
      do n <- readIORef c
         let n' = n + fromIntegral w
         writeIORef c n'
         print_progress (ceiling (n' / np * 100.0))
         let i = (0.5 - w2) * sc
         loopWhile 0 (<w) (+1) 
           (\x -> render_pix (i + fromIntegral x * sc) y f)
        
    -- Render a single pixel
    render_pix :: Double         -- X coordinate of the pixel being rendered
               -> Double         -- Y coordinate of the pixel being rendered
               -> Handle         -- File handle where to store the result 
               -> IO ()
    render_pix x y f =
      do let v = V x y 1
         let m = (255,255,255)
         (r,g,b) <- render_ray amb ls obj depth origin_point v True m
         hPutChar f (chr (clamp r))
         hPutChar f (chr (clamp g))
         hPutChar f (chr (clamp b))
        

----------------------------------------------------------------
-- Do the rendering for a single ray
----------------------------------------------------------------

render_ray :: Point                    -- Ambient lighting
           -> [Light]                  -- Lights in the scene 
           -> TopObj                   -- Scene to render
           -> Int                      -- Rendering depth
           -> Point                    -- Ray's origin
           -> Vector                   -- Ray's direction
           -> Bool                     -- Keep first intersection flag
           -> (Double,Double,Double)   -- Depth cutoff
           -> IO RGB                   -- Result: a RGB tuple
render_ray amb ls obj depth p v initial (factx,facty,factz)
  | depth == 0 = return no_color
  | otherwise  =
    case intersect_ray obj p v initial of
      Nothing    -> return horizon_color
      Just (t,o) -> 
        let inter_w  = point_along p v t
            inter_o  = apply_to_point (w2o o) inter_w
        in case compare t 0 of
             LT -> error $ "error computing intersection: t = " ++ show t
             EQ -> return inside_color
             GT -> do (f,sr) <- analyze_surf o inter_o
                      let _n  = normalVector o inter_w f 
                      let dp  = v <.> _n
                      let s   = v <-> (scale_vect _n (2 * dp))
                      let n   = if dp <= 0 then _n else (opposite _n)
                      let _kd = kd sr
                      let _ks = ks sr
                      let pho = phong sr 
                      let il  = lights_contrib obj inter_w v n _kd _ks pho ls
                      let mx  = factx * _ks * x (c sr)
                      let my  = facty * _ks * y (c sr)
                      let mz  = factz * _ks * z (c sr)
                      (ix,iy,iz) <- 
                        if mx < 0.1 && my < 0.1 && mz < 0.1
                          then return no_color
                          else render_ray 
                                 amb ls obj (depth - 1) 
                                 inter_w s False (mx,my,mz)
                      let _x = (_kd * x amb + x il + _ks * ix) * x (c sr)
                      let _y = (_kd * y amb + y il + _ks * iy) * y (c sr)
                      let _z = (_kd * z amb + z il + _ks * iz) * z (c sr)
                      _x `seq` _y `seq` _z `seq` return (_x,_y,_z)


-- Compute object's surface results for the given
-- intersection point
analyze_surf :: BasicObj -> Point -> IO (Int,SurfaceRes)
analyze_surf obj p = 
  case sfun obj of
    Const sres -> return (face obj p, sres)
    Array a    -> let f = face obj p in return (f, a ! f)
    Fun g      -> let (f,u,v) = coords obj p 
                  in  do { sr <- g f u v ; return (f, sr) }
