module Eval(evalProg) where

import Array

import Point
import Utils
import Light
import GMLSyn
import Object
import Render


{----------------------------------------------------------------
  The GML evaluator.
----------------------------------------------------------------}

data Value = I !Int
           | B !Bool
           | R !Double
           | S !String
           | A !(Array Int Value)
           | C !(Env,Prog)
           | Light  !Light
           | Point  !Point
           | Object !TopObj
           | Dummy                 -- just for partial evaluation
           deriving Show

type Env   = [(String,Value)]
type Stack = [Value]
data ST = ST Env Stack deriving Show


-----------------------------------------------------------------
-- Environment management
-----------------------------------------------------------------

newEnv :: Env
newEnv = []

fetchEnv :: String -> Env -> Maybe Value
fetchEnv = lookup

-- Replace, add only if key not not
addEnv :: (String,Value) -> Env -> Env
addEnv a@(k,v) e = 
  case e of 
    [] -> [a]
    ((k1,v1):es) -> if k == k1 then a:es else (k1,v1) : addEnv a es


-----------------------------------------------------------------
-- The evaluation function itself
-----------------------------------------------------------------

evalProg :: Prog -> IO (Either ST String)
evalProg p = eval p (ST newEnv [])

eval :: Prog -> ST -> IO (Either ST String)
eval [] s = return $ Left s
eval (t:ts) s = 
  do r <- evalTok t s
     case r of
       Left s1 -> eval ts s1
       Right e -> return $ Right e

evalTok :: Token -> ST -> IO (Either ST String)
evalTok t st = 
  case (t,st) of
  
    -- Evaluate basic values
    (Int i, ST e vs)        -> return $ Left $ ST e ((I i):vs)
    (Boolean b, ST e vs)    -> return $ Left $ ST e ((B b):vs)
    (Real r, ST e vs)       -> return $ Left $ ST e ((R r):vs)
    (Literal l, ST e vs)    -> return $ Left $ ST e ((S l):vs)
    (Binder k, ST e (v:vs)) -> return $ Left $ ST (addEnv (k,v) e) vs 
    
    -- Fetch identitifers from the environment
    (Id k, ST e vs)         -> 
       case fetchEnv k e of
         Nothing -> return $ Right $ "unbound identifier: " ++ k
         Just v  -> return $ Left  $ ST e (v:vs)
         
    -- Add a closure to the environment
    (Function ts, ST e vs)   -> return $ Left $ ST e ((C (e,ts)):vs)
    
    -- Evaluate arrays elems
    (Arr ts, ST e vs)        -> 
      do r <- eval (elems ts) (ST e [])
         case r of
           Left (ST _ us) -> 
             let a = listArray (0,length us - 1) (reverse us) in  
             return $ Left $ ST e ((A a):vs)
           Right err      -> return $ Right err
        
    -- Function application
    (Op Op_apply, ST e (C (e1,c):vs)) ->
      do r <- eval c (ST e1 vs)
         case r of 
           Left (ST _ us) -> return $ Left $ ST e us
           Right err      -> return $ Right err
        
    -- Branch evaluation
    (Op Op_if, ST e (C(ef,cf):C(et,ct):B b:vs)) ->
      do r <- eval (if b then ct else cf) (ST (if b then et else ef) vs)
         case r of
           Left (ST _ us) -> return $ Left $ ST e us
           Right err      -> return $ Right err
    
    (Op op, ST e vs) -> 
      do r <- evalOp op vs
         case r of
           Left us   -> return $ Left $ ST e us
           Right err -> return $ Right err
    
    -- Oops! We are screwed
    _ -> return $ Right $ "error evaluating: " ++ show t


-- Evaluate operators

unimp :: Operator -> a
unimp op = error $ "unimplemented operator: " ++ show op


evalOp :: Operator -> Stack -> IO (Either Stack String)
evalOp Op_acos (R r : vs)         = return $ Left $ R (deg_acos r) : vs
evalOp Op_addi (I i1 : I i2 : vs) = return $ Left $ I (i2 + i1) : vs
evalOp Op_addf (R r1 : R r2 : vs) = return $ Left $ R (r2 + r1) : vs
evalOp Op_asin (R r : vs)         = return $ Left $ R (deg_asin r) : vs
evalOp Op_clampf (R r : vs)
  | r < 0     = return $ Left $ R 0 : vs
  | r > 1     = return $ Left $ R 1 : vs
  | otherwise = return $ Left $ R r : vs
evalOp Op_cone (C (e,p) : vs) = 
  do { sfun <- surfaceFun p e 2 ;
       return $ Left $ Object (cone sfun) : vs
  }
evalOp Op_cos (R r : vs)      = return $ Left $ R (deg_cos r) : vs
evalOp Op_cube (C (e,p) : vs) = 
  do { sfun <- surfaceFun p e 6 ;
       return $ Left $ Object (cube sfun) : vs
  }
evalOp Op_cylinder (C (e,p) : vs)  = 
  do { sfun <- surfaceFun p e 3 ;
       return $ Left $ Object (cylinder sfun) : vs
  }
evalOp Op_difference (Object o1 : Object o2 : vs) = 
  return $ Left $ Object (difference o2 o1) : vs
evalOp Op_divi (I i1 : I i2 : vs) 
 | i1 == 0   = return $ Right "division by zero"
 | otherwise = return $ Left $ I (i2 `quot` i1) : vs
evalOp Op_divf (R r1 : R r2 : vs) = return $ Left $ R (r2 / r1) : vs
evalOp Op_eqi (I i1 : I i2 : vs)  = return $ Left $ B (i1 == i2) : vs
evalOp Op_eqf (R r1 : R r2 : vs)  = return $ Left $ B (r1 == r2) : vs
evalOp Op_floor (R r : vs)        = return $ Left $ I (floor r) : vs
evalOp Op_frac (R r : vs) 
 | r < 0     = return $ Left $ R (r + fromInteger (floor (-r))) : vs
 | otherwise = return $ Left $ R (r - fromInteger (floor r)) : vs
evalOp Op_get (I i : A a : vs) = return $ Left $ (a ! i) : vs
evalOp Op_getx (Point p : vs) = return $ Left $ R (x p) : vs
evalOp Op_gety (Point p : vs) = return $ Left $ R (y p) : vs
evalOp Op_getz (Point p : vs) = return $ Left $ R (z p) : vs
evalOp Op_intersect (Object o1 : Object o2 : vs) = 
  return $ Left $ Object (intersection o2 o1) : vs
evalOp Op_length (A a : vs) = return $ Left $ (I ((snd.bounds) a + 1)) : vs
evalOp Op_lessi (I i1 : I i2 : vs) = return $ Left $ B (i2 < i1) : vs
evalOp Op_lessf (R r1 : R r2 : vs) = return $ Left $ B (r2 < r1) : vs
evalOp Op_light (Point c : Point dir : vs) = 
  return $ Left $ Light (directional dir c) : vs
evalOp Op_modi (I i1 : I i2 : vs) = return $ Left $ I (i2 `mod` i1) : vs
evalOp Op_muli (I i1 : I i2 : vs) = return $ Left $ I (i2 * i1) : vs
evalOp Op_mulf (R r1 : R r2 : vs) = return $ Left $ R (r2 * r1) : vs
evalOp Op_negi (I i : vs)  = return $ Left $ I (negate i) : vs 
evalOp Op_negf (R r : vs)  = return $ Left $ R (negate r) : vs 
evalOp Op_plane (C (e,p) : vs) = 
  do { sfun <- surfaceFun p e 1 ;
       return $ Left $ Object (plane sfun) : vs
  }
evalOp Op_point (R z : R y : R x : vs) = 
  return $ Left $ Point (P x y z) : vs
evalOp Op_pointlight (Point c : Point org : vs) = 
  return $ Left $ Light (point org c) : vs
evalOp Op_real (I i : vs) = return $ Left $ R (fromIntegral i) : vs
evalOp Op_render 
  (S s : I h : I w : R fov : I d : Object obj : A ls : Point amb : vs) = 
  let lights = map (\(Light l) -> l) (elems ls)
  in do { render amb lights obj d fov w h s ; return (Left vs) }
evalOp Op_rotatex (R theta : Object o : vs) = 
  return $ Left $ Object (rotateObj_x theta o) : vs
evalOp Op_rotatey (R theta : Object o : vs) =
  return $ Left $ Object (rotateObj_y theta o) : vs
evalOp Op_rotatez (R theta : Object o : vs) =
  return $ Left $ Object (rotateObj_z theta o) : vs
evalOp Op_scale (R sz : R sy : R sx : Object o : vs) = 
  return $ Left $ Object (scaleObj sx sy sz o) : vs
evalOp Op_sin (R r : vs) = return $ Left $ R (deg_sin r) : vs
evalOp Op_sphere (C (e,p) : vs) = 
  do { sfun <- surfaceFun p e 1 ;
       return $ Left $ Object (sphere sfun) : vs
  }
evalOp Op_spotlight 
  (R exp : R cutoff : Point c : Point at : Point org : vs) = 
  return $ Left $ Light (spot org at c cutoff exp) : vs
evalOp Op_sqrt (R r : vs)
  | r < 0     = return $ Right "negative argument passed to sqrt"
  | otherwise = return $ Left $ R (sqrt r) : vs
evalOp Op_subi (I i1 : I i2 : vs) = return $ Left $ I (i2 - i1) : vs
evalOp Op_subf (R r1 : R r2 : vs) = return $ Left $ R (r2 - r1) : vs
evalOp Op_translate (R tz : R ty : R tx : Object o : vs) = 
  return (Left (Object (translateObj tx ty tz o) : vs))
evalOp Op_union (Object o1 : Object o2 : vs) = 
  return $ Left $ Object (union o2 o1) : vs
evalOp Op_uscale (R s : Object o : vs) = 
  return $ Left $ Object (uscaleObj s o) : vs
evalOp op s  = return $ Right $ 
  "invalid operator call: " ++ show op ++ "\nStack: " ++ show s


-----------------------------------------------------------------
-- Poor man's partial evaluator for surface functions.
--
-- Since surface functions will get evaluated for EVERY
-- visible point on a solid, we must optimize their
-- evaluation whenever it possible.
-----------------------------------------------------------------

-- Wrap an evaluation inside a Maybe type
evalM :: Prog -> ST -> IO (Maybe ST)
evalM p s = 
  do r <- eval p s
     case r of
       Left s1 -> return $ Just s1
       Right _ -> return Nothing


-- Thread surface function evaluation for each face of a solid
eval_faces :: Prog -> Env -> Int -> IO (Maybe [ST])
eval_faces p env faces = 
  do l0 <- sequence 
             (map (\f -> evalM p (ST env [Dummy,Dummy,I f])) [0..faces - 1])
     let l1 = sequence l0
     return l1


-- Extract the result from the stack. Let it explode if the
-- surface function return type is not the expected
get_res :: [Value] -> SurfaceRes
get_res s =
  case s of 
    R n : R ks : R kd : Point color : [] -> SurfaceRes color kd ks n
    _ -> error $ "Invalid surface function result: " ++ show s


-- Perform a poor's man partial evaluation of surface functions.
-- If we are lucky, they might be replaced by a constant, or at
-- least, by an array of constants per face.
surfaceFun :: Prog -> Env -> Int -> IO SurfaceFun
surfaceFun p env faces = 
  do r <- eval p (ST env [Dummy,Dummy,Dummy])
     case r of
  
       -- The function ignores its parameters, hence ot is constant. Good!
       Left (ST _ s) -> return $ Const $ get_res s
    
       Right _ ->
    
          -- The function is contant per face. Still good.
          do fs <- eval_faces p env faces
             case fs of
               Just ss -> let l = map (get_res . (\(ST _ s) -> s)) ss in
                          return $ Array $ listArray (0,length l - 1) l
                   
               -- Sad but true: non constant function. Remain dynamic
               Nothing ->
                 return $ Fun $ \face u v -> 
                   do r <- eval p (ST env [R v, R u, I face]) 
                      case r of
                        Left (ST _ s) -> return $ get_res s
                        Right _       -> error "Invalid surface function"
