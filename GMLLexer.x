%{
module GMLLexer(GMLTok(..),gml_lexer,token_pos) where

import Alex
import Char(isDigit)

import GMLSyn

%}


{ ^d = 0-9       }              -- digits
{ ^l = [a-zA-Z]  }              -- alphabetic characters
{ ^c = ^p # ^"   }              -- stuff inside string literals
{ %e = [eE]^-?^d }              -- exponents
{ %i = ^l([^l^d^_^-]*)}         -- identifiers

"tokens_lx"/"tokens_acts":-

  -- White space and comments
  <>       ::= ^w+			         
  <>       ::= ^%.*			         
  
  -- Numbers
  <int>    ::= ^-?^d+            %{ int p s    = TInt p  (read  s)    %}
  <real1>  ::= ^-?^d+(^.^d+%e?)  %{ real1 p s  = TReal p (read  s)    %} 
  <real2>  ::= ^-?^d+%e          %{ real2 p s  = TReal p (_read s)    %}
  
  -- String literals
  <lit>    ::= ^"^c*^"           %{ lit p s = TLit p (unlit (tail s)) %}
  
  -- Binders
  <binder> ::= ^/%i              %{ binder p s = TBinder p (tail s)   %}
  
  -- Punctuation
  <lCurly> ::= ^{                %{ lCurly p _ = TLCurly p   %}
  <rCurly> ::= ^}                %{ rCurly p _ = TRCurly p   %}
  <lBrkt>  ::= ^[                %{ lBrkt p _  = TLBracket p %}
  <rBrkt>  ::= ^]                %{ rBrkt p _  = TRBracket p %}
    
  -- Identifiers
  <ident>  ::= %i                %{ ident = procId     %} 
  
  
%{
data GMLTok = TInt      Posn Int 
            | TReal     Posn Double
            | TBool     Posn Bool
            | TLit      Posn String
            | TBinder   Posn String
            | TId       Posn String
            | TOp       Posn Operator
            | TLCurly   Posn
            | TRCurly   Posn
            | TLBracket Posn
            | TRBracket Posn
            | TError    Posn
            | EOF
            deriving (Eq,Show)
                        
_read :: String -> Double
_read = read . reverse . snd . foldr f (False,"") . reverse
  where f c (b,s) 
         | isDigit c || c == '-' = (b, c : s) 
         | c == '.'              = (True, c : s)
         | otherwise             = if b then (b, c : s)
                                        else (True, c : '0' : '.' : s)

unlit :: String -> String
unlit (c:[])     = []
unlit (c1:c2:cs) = c1 : unlit (c2:cs)

procId :: Posn -> String -> GMLTok
procId p s 
 | s == "true"  = TBool p True
 | s == "false" = TBool p False
 | otherwise    =  
     case lookup s ops of
       Nothing -> TId p s
       Just op -> TOp p op

-- Operators
ops = [
  ("acos",Op_acos), 
  ("addi",Op_addi), 
  ("addf",Op_addf), 
  ("apply",Op_apply), 
  ("asin",Op_asin), 
  ("clampf",Op_clampf), 
  ("cone",Op_cone), 
  ("cos",Op_cos),
  ("cube",Op_cube), 
  ("cylinder",Op_cylinder), 
  ("difference",Op_difference), 
  ("divi",Op_divi), 
  ("divf",Op_divf), 
  ("eqi",Op_eqi), 
  ("eqf",Op_eqf),
  ("floor",Op_floor), 
  ("frac",Op_frac), 
  ("get",Op_get), 
  ("getx",Op_getx), 
  ("gety",Op_gety), 
  ("getz",Op_getz), 
  ("if",Op_if), 
  ("intersect",Op_intersect),
  ("length",Op_length), 
  ("lessi",Op_lessi), 
  ("lessf",Op_lessf), 
  ("light",Op_light), 
  ("modi",Op_modi), 
  ("muli",Op_muli), 
  ("mulf",Op_mulf),
  ("negi",Op_negi), 
  ("negf",Op_negf), 
  ("plane",Op_plane), 
  ("point",Op_point), 
  ("pointlight",Op_pointlight), 
  ("real",Op_real),
  ("render",Op_render),
  ("rotatex",Op_rotatex), 
  ("rotatey",Op_rotatey), 
  ("rotatez",Op_rotatez), 
  ("scale",Op_scale), 
  ("sin",Op_sin), 
  ("sphere",Op_sphere),
  ("spotlight",Op_spotlight), 
  ("sqrt",Op_sqrt), 
  ("subi",Op_subi), 
  ("subf",Op_subf), 
  ("translate",Op_translate), 
  ("union",Op_union), 
  ("uscale",Op_uscale)]

token_pos :: GMLTok -> (Int,Int)
token_pos (TInt (Pn _ r c) _)    = (r,c)
token_pos (TReal (Pn _ r c) _)   = (r,c)
token_pos (TLit (Pn _ r c) _)    = (r,c)
token_pos (TBinder (Pn _ r c) _) = (r,c)
token_pos (TId (Pn _ r c) _)     = (r,c)
token_pos (TOp (Pn _ r c) _)     = (r,c)
token_pos (TLCurly (Pn _ r c))   = (r,c)
token_pos (TRCurly (Pn _ r c))   = (r,c)
token_pos (TLBracket (Pn _ r c)) = (r,c)
token_pos (TRBracket (Pn _ r c)) = (r,c)
token_pos (TError (Pn _ r c))    = (r,c)

gml_lexer:: String -> [GMLTok]
gml_lexer = scan tokens_scan

tokens_scan:: Scan GMLTok
tokens_scan = load_scan (tokens_acts,stop_act) tokens_lx
	where
	stop_act p ""  = [EOF]
	stop_act p inp = [TError p]
%}
