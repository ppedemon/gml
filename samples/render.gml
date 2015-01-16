% try a couple of render commands

{ /v /u /face
  [ 1.0 0.0 0.0 point  % front  = red
    0.0 1.0 0.0 point  % rear   = green
    0.0 0.0 1.0 point  % left   = blue
    1.0 1.0 0.0 point  % right  = yellow
    0.0 1.0 1.0 point  % top    = cyan
    1.0 0.0 1.0 point  % bottom = magenta
  ] face get 1.0 1.0 1.0
} /sfun

% sfun cube
% -0.5 -0.5 -0.5 translate
% -45.0 rotatey
% -45.0 rotatex
% 0.0 0.0 0.75 translate
% 5 90.0 200 200 "sample.ppm" render

% sfun cylinder 
% 0.0 -0.5 0.0 translate
% 30.0 rotatex
% 0.0 -0.25 1.0 translate
% 5 90.0 200 200 "sample.ppm" render

sfun cylinder 0.5 1.0 0.5 scale /inner
sfun cylinder inner difference
0.0 -0.5 0.0 translate 
90.0 rotatex
0.0 0.0 1.0 translate
5 90.0 200 200 "sample.ppm" render

