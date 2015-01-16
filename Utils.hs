module Utils where

deg2rad d = d * pi / 180 
rad2deg r = r * 180 / pi

deg_cos = cos . deg2rad
deg_sin = sin . deg2rad
deg_tan = tan . deg2rad

deg_acos = rad2deg . acos
deg_asin = rad2deg . asin

pos_inf = 1/0
