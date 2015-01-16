module GMLSyn where

import Array(Array)

-- The GML abstract syntax

data Operator = Op_acos
              | Op_addi 
              | Op_addf
              | Op_apply
              | Op_asin
              | Op_clampf
              | Op_cone
              | Op_cos
              | Op_cube 
              | Op_cylinder
              | Op_difference 
              | Op_divi 
              | Op_divf
              | Op_eqi
              | Op_eqf
              | Op_floor
              | Op_frac 
              | Op_get
              | Op_getx
              | Op_gety
              | Op_getz
              | Op_if
              | Op_intersect
              | Op_length
              | Op_lessi
              | Op_lessf
              | Op_light
              | Op_modi
              | Op_muli
              | Op_mulf
              | Op_negi
              | Op_negf
              | Op_plane
              | Op_point
              | Op_pointlight
              | Op_real
              | Op_render
              | Op_rotatex
              | Op_rotatey
              | Op_rotatez
              | Op_scale
              | Op_sin
              | Op_sphere
              | Op_spotlight
              | Op_sqrt
              | Op_subi
              | Op_subf
              | Op_translate
              | Op_union
              | Op_uscale
              deriving (Eq,Show)
              
data Token = Function [Token]
           | Arr (Array Int Token)
           | Op Operator
           | Id String
           | Binder String
           | Int Int
           | Real Double
           | Boolean Bool
           | Literal String
           deriving (Eq,Show)
           
type Prog = [Token]

