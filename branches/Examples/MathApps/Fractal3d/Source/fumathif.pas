{*******************************************************}
{                                                       }
{                      Tachyon Unit                     }
{    Vector Raster Geographic Information Synthesis     }
{                     VOICE  ..  Tracer                 }
{                     GRIP ICE .. Tongs                 }
{                Digital Terrain Mapping                }
{               Image Locatable Holographics            }
{                          SOS MAP                      }
{  Surreal Object Synthesis Multimedia Analysis Product }
{                   Fractal3D  Life MOW                 }
{       Copyright (c) 1995,2006  Ivan Lee Herring       }
{                                                       }
{*******************************************************}
unit fumathif;

interface

uses
  Windows, Messages, SysUtils, Dialogs, Forms,
  Graphics, Classes;

{Iterated Functions}
{Change to input X and Y location
And divide to enable calling from Vista}

procedure trees(XOffset, YOffset, width, level: Integer;
  height, left_alpha, right_alpha,
  left_angle, right_angle: Extended;
  Color1, Color2, Color3, Color4: Tcolor);

procedure FernImage(xscale, yscale, xoffset, yoffset, Iterations:
  Integer;
  FTempColor: TColor);
procedure Fern3DImage(xscale, yscale, xoffset, yoffset, Iterations:
  Integer;
  Alpha, Beta, Gamma: Extended;
  FTempColor: TColor);

procedure DragOutdo(P, Q, Scale, x_offset, y_offset: Extended;
  Iterations: Integer; DColor: TColor);
{   Writeln('1.646009 0.967049'); Readln(P,Q);}

procedure gen3ddrg(alpha, beta, gamma,
  scale, x_offset, y_offset, QVal: Extended;
  Iterations: Integer; Color: TColor);

procedure image3d;
procedure Image(WhichOne: integer);
procedure sierchet3;
procedure sierchet;
procedure sierchet2;
procedure Sierpinski(level: integer);
procedure ifsdet(ifswhich: Byte);
procedure apolly(level: integer);
procedure pharoh;
{Vistas}
                   {Sedona : Oak Creek canyon }{Plate 29}
{TwoButtes -> put in a box of triangles at bottom }
procedure TwoButtes;
  {speed ok, TIME IT; could use a band at bottom drawn first
                    and a road drawn in from lower right to a
                    little higher on the left }

                     {Plate 30 }
               { change limits of y to allow filling of triangles}
procedure PuertoRico; {TIME IT, Find the line and move to bottom}


procedure Earth; {Plate 31 } {Earth viewed from Moon }
{Change landmass -> plot large triangles,
 then fill in edges to correct shape,
fill in mountains to change patterns }

procedure LightsOn; {Desert Sandia }
{CityScape against mtn range background }

procedure forest;
 { THIS IS MESSED UPPPPP Wastes time covering things just drawn
 and could do putpixel to put in a bunch of trees that look like arcs }


(***************************************************)
procedure SetNumb3DImage;
procedure Numb3DImage;
procedure SetSkyImage;
procedure SkyImage;
procedure RanFillTri(Pt1, Pt2, Pt3, LtPt: TPoint; color: TColor);
function I2S(X: integer): string;
(***************************************************)
implementation

uses fUGlobal, fMain, fGMath;

var
{y_max:Integer;}
  g_x, g_y, g_xz, g_yz: Extended;
  turtle_r, turtle_x, turtle_y, turtle_theta: Extended;

function I2S(X: integer): string;
var
  TempStr: string;
begin
  str(X, TempStr);
  I2S := TempStr;
end;

function point(x1: real; y_one: real; x2: real; y2: Extended):
  Extended;
var theta: Extended;
begin
  if (x2 - x1) = 0 then
    if y2 > y_one then
      theta := 90
    else theta := 270
  else theta := arctan((y2 - y_one) / (x2 - x1)) * 57.295779;
  if x1 > x2 then
    theta := theta + 180;
  point := theta;
end;

procedure step;
begin
  turtle_x := turtle_x + turtle_r * cos(turtle_theta * 0.017453292);
  turtle_y := turtle_y + turtle_r * sin(turtle_theta * 0.017453292);
end;

procedure turn(angle: Extended);
begin
  turtle_theta := turtle_theta + angle;
end;
(***************************************************)
(***************************************************)

(************************************************************)
(************************************************************)
procedure generate(x1, y1, x2, y2, x3, y3, level, y_max: integer;
  color1, color2: TColor); forward;

procedure ranFillOval(x, y, b: integer;
  color: TColor; aspect: Extended);
var col, row, end_x, end_y, kx, radius: integer;
  a: Extended;
  a_square, b_square, b_test: longint;
begin
  a := b / aspect;
  a_square := Round(a * a);
  radius := b;
  b := b; {(93*b) div 128;}
  b_square := b * b;
  x := x + (FYImageX div 2);
  y := (FYImageY div 2) - y; {(93*y) div 128;}
  end_x := x + Round(a);
  end_y := y + Round(b);
  for col := x - Round(a) to end_x do begin
    b_test := b_square - (b_square * (col - x) * (col - x)) div
      a_square;
    for row := y - b to end_y do begin
      kx := Random(25205 div radius);
      if ((row - y) * (row - y) <= b_test)
        and (kx < (col - x + 20)) then begin
        MainForm.Image2.Canvas.Pixels[col, row] := color;
      end;
    end;
  end;
end;

procedure midpoint(x: Extended; y: Extended);
var
  r, w: Extended;
  seed: longint;
begin
  seed := Round(FYImageY * y + x); {350 * y + x}
  RandSeed := seed;
  r := 0.33333 + Random / 3.0;
  w := 0.015 + Random / 50.0;
  if Random < 0.5 then
    w := -w;
  g_xz := r * x - (w + 0.05) * y;
  g_yz := r * y + (w + 0.05) * x;
end;

procedure node(x1: integer; y1: integer; x2: integer; y2: integer;
  x3: integer; y3: integer; x4: integer; y4: integer; x5: integer;
  y5: integer; x6: integer; y6: integer; level, y_max: integer;
  color1, color2: Tcolor);
begin
  if level <> 0 then
  begin
    generate(x1, y1, x6, y6, x4, y4, level - 1, y_max, color1,
      color2);
    generate(x2, y2, x4, y4, x5, y5, level - 1, y_max, color1,
      color2);
    generate(x3, y3, x5, y5, x6, y6, level - 1, y_max, color1,
      color2);
    generate(x4, y4, x5, y5, x6, y6, level - 1, y_max, color1,
      color2);
  end;
end;

procedure plot_triangle(x1, y1, x2, y2, x3, y3, y_max: integer;
  color1, color2: Tcolor);
var Color: TColor;
       {color: integer; }
  zt, ytt: Extended;
begin
  if y1 > y2 then ytt := y1
  else ytt := y2;
  if ytt < y3 then ytt := y3;
  zt := 1 - ((ytt + (FYImageY div 2)) * (ytt + (FYImageY div 2))) /
    ((y_max + (FYImageY div 2)) * (y_max + (FYImageY div 2)));
  if Random <= zt then color := color1
  else color := color2; {color3} {randomly set another}
  {y_max} if ytt + (FYImageY div 2) <
    {all low color, color2 := color3}
    ((y_max + (FYImageY div 2)) / 4) then color := color1;
  if ytt + (FYImageY div 2) > {all hi color, color1 := color4}
    (0.98 * (y_max + (FYImageY div 2))) then color := color2;
  Triangle[1].x := x1 + (FYImageX div 2);
  Triangle[1].y := (FYImageY div 2) - y1;
  Triangle[2].x := x2 + (FYImageX div 2);
  Triangle[2].y := (FYImageY div 2) - y2;
  Triangle[3].x := x3 + (FYImageX div 2);
  Triangle[3].y := (FYImageY div 2) - y3;
  MainForm.Image2.Canvas.Pen.Color := color;
  MainForm.Image2.Canvas.Brush.Color := color;
  MainForm.Image2.Canvas.Polygon(Triangle);
end;

procedure generate(x1, y1, x2, y2, x3, y3, level, y_max: integer;
  color1, color2: Tcolor);
var x4, x5, x6, y4, y5, y6: integer;
begin
  g_x := x2 - x1;
  g_y := y2 - y1;
  midpoint(g_x, g_y);
  x4 := x1 + Round(g_xz);
  y4 := y1 + Round(g_yz);
  g_x := x1 - x3;
  g_y := y1 - y3;
  midpoint(g_x, g_y);
  x6 := x3 + Round(g_xz);
  y6 := y3 + Round(g_yz);
  g_x := x3 - x2;
  g_y := y3 - y2;
  midpoint(g_x, g_y);
  x5 := x2 + Round(g_xz);
  y5 := y2 + Round(g_yz);
  if level = 0 then
  begin
    plot_triangle(x1, y1, x6, y6, x4, y4, y_max, color1, color2);
    plot_triangle(x2, y2, x4, y4, x5, y5, y_max, color1, color2);
    plot_triangle(x3, y3, x5, y5, x6, y6, y_max, color1, color2);
    plot_triangle(x4, y4, x5, y5, x6, y6, y_max, color1, color2);
  end
  else
    node(x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, level,
      y_max,
      color1, color2);
end;

procedure gen_quad(x1, y1, x2, y2, x3, y3, x4, y4, level, y_max:
  integer;
  color1, color2: Tcolor);
begin
  generate(x1, y1, x2, y2, x3, y3, level, y_max, color1, color2);
  generate(x1, y1, x4, y4, x3, y3, level, y_max, color1, color2);
end;
(************************************************************)

procedure trees(XOffset, YOffset, width, level: Integer;
  height, left_alpha, right_alpha,
  left_angle, right_angle: Extended;
  Color1, Color2, Color3, Color4: Tcolor);
const
  ln_2: Extended = (0.6931471);
var
  Square: array[1..4] of TPoint;
  left_width_factor, left_height_factor, right_width_factor,
  right_height_factor, x, y, x1, y1: Extended;
  Fy, Fx: Integer;
  {sub}procedure generate(x: Extended; y: Extended; width: integer;
    height: Extended; angle: Extended;
    level: integer);
  var x1, y1: Extended;
  begin
    turtle_x := x;
    turtle_y := y;
    turtle_r := height;
    step;
    x1 := turtle_x;
    y1 := turtle_y;
    dec(level);
    if level < 3 then begin
      MainForm.Image2.Canvas.Brush.Color := Color4;
      MainForm.Image2.Canvas.Pen.Color := Color4;
    end else
      if ((level > 2) and (level < 8)) then begin
        MainForm.Image2.Canvas.Brush.Color := Color3;
        MainForm.Image2.Canvas.Pen.Color := Color3;
      end else
        if ((level > 7) and (level < 12)) then begin
          MainForm.Image2.Canvas.Brush.Color := Color2;
          MainForm.Image2.Canvas.Pen.Color := Color2;
        end else begin
          MainForm.Image2.Canvas.Brush.Color := Color1;
          MainForm.Image2.Canvas.Pen.Color := Color1;
        end;
    if Abs(x - x1) > Abs(y - y1) then
    begin
      Square[1].x := Round(x + Fx);
      Square[1].y := Round(Fy - y) + width div 2;
      Square[2].x := Round(x + Fx);
      Square[2].y := Round(Fy - y) - width div 2;
      Square[3].x := Round(x1 + Fx);
      Square[3].y := Round(Fy - y1) - width div 2;
      Square[4].x := Round(x1 + Fx);
      Square[4].y := Round(Fy - y1) + width div 2;
    end
    else {175 to 240, y*0.729}
    begin
      Square[1].x := Round(x + Fx) - width div 2;
      Square[1].y := Round(Fy - y);
      Square[2].x := Round(x + Fx) + width div 2;
      Square[2].y := Round(Fy - y);
      Square[3].x := Round(x1 + Fx) + width div 2;
      Square[3].y := Round(Fy - y1);
      Square[4].x := Round(x1 + Fx) - width div 2;
      Square[4].y := Round(Fy - y1);
    end;
    MainForm.Image2.Canvas.Polygon(Square);
    if level > 0 then
    begin
      turtle_theta := point(x, y, x1, y1);
      turn(left_angle);
      generate(x1, y1, Round(left_width_factor * width),
        left_height_factor *
        height, left_angle, level);
      turtle_theta := point(x, y, x1, y1);
      turn(-right_angle);
      generate(x1, y1, Round(right_width_factor * width),
        right_height_factor *
        height, right_angle, level);
    end;
  end;
begin
  Square[1].x := 50;   Square[1].y := 100;
  Square[2].x := 100;  Square[2].y := 100;
  Square[3].x := 100;  Square[3].y := 100;
  Square[4].x := 150;  Square[4].y := 150;
  Fx := (FYImageX div 2);
  Fy := (FYImageX div 2);
  left_width_factor := exp((-ln_2 / left_alpha));
  left_height_factor := exp((-2 * ln_2) / (3 * left_alpha));
  right_width_factor := exp((-ln_2 / right_alpha));
  right_height_factor := exp((-2 * ln_2) / (3 * right_alpha));
  x := XOffset;
  y := YOffset;
  x1 := x {0}; {Allow placement of tree}
  y1 := y + height;
  if Abs(x - x1) > Abs(y - y1) then
  begin
    Square[1].x := Round(x + Fx);
    Square[1].y := Round(Fy - y) + width div 2;
    Square[2].x := Round(x + Fx);
    Square[2].y := Round(Fy - y) - width div 2;
    Square[3].x := Round(x1 + Fx);
    Square[3].y := Round(Fy - y1) - width div 2;
    Square[4].x := Round(x1 + Fx);
    Square[4].y := Round(Fy - y1) + width div 2;
  end
  else
  begin
    Square[1].x := Round(x + Fx) - width div 2;
    Square[1].y := Round(Fy - y);
    Square[2].x := Round(x + Fx) + width div 2;
    Square[2].y := Round(Fy - y);
    Square[3].x := Round(x1 + Fx) + width div 2;
    Square[3].y := Round(Fy - y1);
    Square[4].x := Round(x1 + Fx) - width div 2;
    Square[4].y := Round(Fy - y1);
  end;
  MainForm.Image2.Canvas.Brush.Color := Color1;
  MainForm.Image2.Canvas.Pen.Color := Color1;
  MainForm.Image2.Canvas.Polygon(Square);
  turtle_theta := point(x, y, x1, y1);
  turn(left_angle);
  generate(x1, y1, Round(left_width_factor * width),
    left_height_factor * height,
    left_angle, level);
  turtle_theta := point(x, y, x1, y1);
  turn(-right_angle);
  generate(x1, y1, Round(right_width_factor * width),
    right_height_factor * height,
    right_angle, level);
end; { ITREES.PAS  }
(***************************************************)
(***************************************************)

(***************************************************)
(***************************************************)

procedure FernImage(xscale, yscale, xoffset, yoffset, Iterations:
  Integer;
  FTempColor: TColor);
var a, b, c, d, e, f, p: array[0..3] of Extended;
  x, y, newx, j: Extended;
  i, k, px, py, Fx, Fy: integer;
begin
  a[0] := 0; a[1] := 0.2; a[2] := -0.15; a[3] := 0.85;
  b[0] := 0; b[1] := -0.26; b[2] := 0.28; b[3] := 0.04;
  c[0] := 0; c[1] := 0.23; c[2] := 0.26; c[3] := -0.04;
  d[0] := 0.16; d[1] := 0.22; d[2] := 0.24; d[3] := 0.85;
  e[0] := 0; e[1] := 0; e[2] := 0; e[3] := 0;
  f[0] := 0; f[1] := 0.2; f[2] := 0.2; f[3] := 0.2;
  p[0] := 0.01; p[1] := 0.08; p[2] := 0.15; p[3] := 1.0;
  Fx := (FYImageX div 2);
  Fy := (FYImageX div 2);
  x := 0;
  y := 0;
  for i := 1 to Iterations do begin
    j := Random;
    if j < p[0] then
      k := 0 else
      if (j > p[0]) and (j < p[1]) then
        k := 1 else
        if (j > p[1]) and (j < p[2]) then
          k := 2 else
          if j > p[2] then
            k := 3 else k := 2;
    newx := (a[k] * x + b[k] * y + e[k]);
    y := (c[k] * x + d[k] * y + f[k]);
    x := newx;
    px := Round(x * xscale + xoffset);
    py := Round(y * yscale + yoffset);
    if (px >= -Fx) and (px < Fx) and (py >= -Fy) and (py < Fy) then
      MainForm.Image2.Canvas.Pixels[px + Fx, Fy - py] := FTempColor;
  end;
end;
(***************************************************)

(***************************************************)

procedure Fern3DImage(xscale, yscale, xoffset, yoffset, Iterations:
  Integer;
  Alpha, Beta, Gamma: Extended;
  FTempColor: TColor);
const
{	alpha: array[0..3] of Extended = (30,45,15,95);
 beta: array[0..3] of Extended = (115,105,70,40);
 gamma: array[0..3] of Extended = (25,70,20,-30);}
  rad_per_degree: Extended = 0.0174533;
{	hues: array[0..3] of integer = (2,10,11,14);}
                              {Green, LightGreen, LightCyan, Yellow}
var Fx, Fy, i, px, py, k: integer;
  a, b, c, d, e, f, g, h, m, n, p, q, r: array[0..3] of Extended;
  vx, vy, x, y, z, newx, newy, j, ca, cb, cg, sa, sb, sg: Extended;
begin
  Fx := (FYImageX div 2);
  Fy := (FYImageX div 2);
  a[0] := 0; a[1] := 0.83; a[2] := 0.22; a[3] := -0.22;
  b[0] := 0; b[1] := 0; b[2] := -0.23; b[3] := 0.23;
  c[0] := 0; c[1] := 0; c[2] := 0; c[3] := 0;
  d[0] := 0; d[1] := 0; d[2] := 0.24; d[3] := 0.24;
  e[0] := 0.18; e[1] := 0.86; e[2] := 0.22; e[3] := 0.22;
  f[0] := 0; f[1] := 0.1; f[2] := 0; f[3] := 0;
  g[0] := 0; g[1] := 0; g[2] := 0; g[3] := 0;
  h[0] := 0; h[1] := -0.12; h[2] := 0; h[3] := 0;
  m[0] := 0; m[1] := 0.84; m[2] := 0.32; m[3] := 0.32;
  n[0] := 0; n[1] := 0; n[2] := 0; n[3] := 0;
  q[0] := 0; q[1] := 1.62; q[2] := 0.82; q[3] := 0.82;
  r[0] := 0; r[1] := 0; r[2] := 0; r[3] := 0;
  p[0] := 0.01; p[1] := 0.85; p[2] := 0.92; p[3] := 1.0;
{	xscale := 40;
 yscale := 50;
 xoffset := 60;
 yoffset := -180;}
{	for index:=0 to 3 do}
  ca := cos(alpha * 0.0174533);
  cb := cos(beta * 0.0174533);
  cg := cos(gamma * 0.0174533);
  sa := sin(alpha * 0.0174533);
  sb := sin(beta * 0.0174533);
  sg := sin(gamma * 0.0174533);
  x := 0;
  y := 0;
  z := 0;
  for i := 1 to Iterations do
  begin
    j := Random;
    if j < p[0] then k := 0 else
      if (j > p[0]) and (j < p[1]) then k := 1 else
        if (j > p[1]) and (j < p[2]) then k := 2 else
          if j > p[2] then k := 3 else k := 2;
    newx := (a[k] * x + b[k] * y + c[k] * z + n[k]);
    newy := (d[k] * x + e[k] * y + f[k] * z + q[k]);
    z := (g[k] * x + h[k] * y + m[k] * z + r[k]);
    x := newx;
    y := newy;
    vx := x * ca + y * cb + z * cg;
    vy := x * sa + y * sb + z * sg;
    px := Round(vx * xscale + xoffset);
    py := Round(vy * yscale + yoffset);
    if (px >= -Fx) and (px < Fx) and (py >= -Fy) and (py < Fy) then
      MainForm.Image2.Canvas.Pixels[px + Fx, Fy - py] := FTempColor;
  end; end;
(***************************************************)

procedure image3d;
const
  alpha: array[0..3] of Extended = (30, 45, 15, 95);
  beta: array[0..3] of Extended = (115, 105, 70, 40);
  gamma: array[0..3] of Extended = (25, 70, 20, -30);
  rad_per_degree: Extended = 0.0174533;
var TempColor: TColor;
{maxcolx,maxrowy,} k, xscale, yscale, xoffset, yoffset, index:
  integer;
  a, b, c, d, e, f, g, h, m, n, p, q, r: array[0..3] of Extended;
  x, y, z, newx, newy, j, ca, cb, cg, sa, sb, sg: Extended;
  procedure image_draw;
  var
    xme, yme, i, px, py: integer;
    vx, vy: Extended;
  begin
    xme := (FYImageX div 2);
    yme := (FYImageY div 2);
    x := 0;
    y := 0;
    z := 0;
    for i := 1 to 25000 do
    begin
      j := Random;
      if j < p[0] then
        k := 0;
      if (j > p[0]) and (j < p[1]) then
        k := 1;
      if (j > p[1]) and (j < p[2]) then
        k := 2;
      if j > p[2] then
        k := 3;
      newx := (a[k] * x + b[k] * y + c[k] * z + n[k]);
      newy := (d[k] * x + e[k] * y + f[k] * z + q[k]);
      z := (g[k] * x + h[k] * y + m[k] * z + r[k]);
      x := newx;
      y := newy;
      vx := x * ca + y * cb + z * cg;
      vy := x * sa + y * sb + z * sg;
      px := Round(vx * xscale + xoffset);
      py := Round(vy * yscale + yoffset);
      if (px >= -xme) and (px < xme) and (py >= -yme) and (py < yme)
        then
      begin
        TempColor := RGB(RGBArray[0, index],
          RGBArray[1, index],
          RGBArray[2, index]);
        MainForm.Image2.Canvas.Pixels[px + xme, yme - py] :=
          TempColor;
      end;
    end;
  end;

begin
  a[0] := 0; a[1] := 0.83; a[2] := 0.22; a[3] := -0.22;
  b[0] := 0; b[1] := 0; b[2] := -0.23; b[3] := 0.23;
  c[0] := 0; c[1] := 0; c[2] := 0; c[3] := 0;
  d[0] := 0; d[1] := 0; d[2] := 0.24; d[3] := 0.24;
  e[0] := 0.18; e[1] := 0.86; e[2] := 0.22; e[3] := 0.22;
  f[0] := 0; f[1] := 0.1; f[2] := 0; f[3] := 0;
  g[0] := 0; g[1] := 0; g[2] := 0; g[3] := 0;
  h[0] := 0; h[1] := -0.12; h[2] := 0; h[3] := 0;
  m[0] := 0; m[1] := 0.84; m[2] := 0.32; m[3] := 0.32;
  n[0] := 0; n[1] := 0; n[2] := 0; n[3] := 0;
  q[0] := 0; q[1] := 1.62; q[2] := 0.82; q[3] := 0.82;
  r[0] := 0; r[1] := 0; r[2] := 0; r[3] := 0;
  p[0] := 0.01; p[1] := 0.85; p[2] := 0.92; p[3] := 1.0;
  xscale := 40;
  yscale := 50;
  xoffset := 60;
  yoffset := -180;

  with MainForm.Image2.Canvas do begin
{	maxcolx :=(FYImageX-1);
 maxrowy := (FYImageY-1); }
{MainForm.Show;
Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,(FYImageX-1),(FYImageY-1)));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;}
    for index := 0 to 3 do
    begin
      ca := cos(alpha[index] * 0.0174533);
      cb := cos(beta[index] * 0.0174533);
      cg := cos(gamma[index] * 0.0174533);
      sa := sin(alpha[index] * 0.0174533);
      sb := sin(beta[index] * 0.0174533);
      sg := sin(gamma[index] * 0.0174533);
      image_draw;
    end;
    TextOut(10, 10, '3 D  Fern');
  end; end;
(***************************************************)
(***************************************************)


(***************************************************)

procedure DragOutdo(P, Q, Scale, x_offset, y_offset: Extended;
  Iterations: Integer; DColor: TColor);

var
  x: Extended;
  y: Extended;

var {    TempColor:TColor;}
  y_center, x_center, col, row, i: integer;
  magnitude, {temp,} temp_x, temp_y: Extended;
begin
  x := 0.50001;
  y := 0;
  x_center := (FYImageX div 2);
  y_center := (FYImageY div 2);
  magnitude := P * P + Q * Q;
  P := 4 * P / magnitude;
  Q := -4 * Q / magnitude;
  scale := x_center * scale;
  for i := 0 to Iterations do
  begin
    temp_x := x * P - y * Q;
    y := x * Q + y * P;
    temp_y := y;
    x := 1 - temp_x;
    magnitude := sqrt(x * x + y * y);
    y := sqrt((-x + magnitude) / 2);
    x := sqrt((x + magnitude) / 2);
    if temp_y < 0 then
      x := -x;
    if random < 0.5 then
    begin
      x := -x;
      y := -y;
    end;
    x := (1 - x) / 2;
    y := y / 2;
    col := Round(scale * (x - 0.5) + x_center);
    row := Round(y_center - scale * y);
    if (i > 10) and (col >= 0) and (col < FYImageX)
      and (row >= 0) and
      (row < FYImageY) then begin
      MainForm.Image2.Canvas.Pixels[col, row] := DColor;
    end;
  end;
end; { of DragOut}


procedure gen3ddrg(alpha, beta, gamma,
  scale, x_offset, y_offset, QVal: Extended;
  Iterations: Integer; Color: TColor);

var
  step_size: Extended;
  upper_limit: Extended;
  lower_limit: Extended;
  x: Extended;
  y: Extended;
  x_center, y_center, i, row, col: integer;
  z, P, Q, k, sx, cx, sy, cy, sz, cz, magnitude,
    temp_x, temp_y: Extended;
  {sub}procedure projection(x3: Extended; y3: Extended; z3: Extended);
  var temp_x, temp_y: Extended;
  begin
    temp_x := x3 * cx + y3 * cy + z3 * cz;
    temp_y := x3 * sx + y3 * sy + z3 * sz;
    col := Round(scale * (temp_x - 0.5) + x_center + x_offset);
    row := Round(y_center - scale * temp_y + y_offset);
    color := color + 16;
                {Round(abs(y3*7)) mod 7 + 1;}
    if y3 > 0 then color := color + 8;
    color := (color mod 16777214); {overflow preventer}
    if (col >= 0) and (col < FYImageX)
      and (row >= 0) and (row < FYImageY) then
      MainForm.Image2.Canvas.Pixels[col, row] := color;
  end;

  {sub}function degrees_to_radians(degrees: Extended): Extended;
  const
    rad_per_degree: Extended = 0.0174533;
  var angle: Extended;
  begin
    while degrees >= 360 do
      degrees := degrees - 360;
    while degrees < 0 do
      degrees := degrees + 360;
    angle := rad_per_degree * degrees;
    degrees_to_radians := angle;
  end; { of function d_to_r }

begin { Main block of gen3ddrag }
  step_size := 0.4;
  upper_limit := 3.0;
  lower_limit := -3.0;
  x := 0.50001;
  y := 0;
  x_center := (FYImageX div 2);
  y_center := (FYImageY div 2);
  scale := x_center * scale;
  if QVal = 0 then
  begin
    step_size := 0.1;
    upper_limit := 1.0;
    lower_limit := -1.0;
  end;
  alpha := degrees_to_radians(alpha);
  sx := sin(alpha);
  cx := cos(alpha);
  beta := degrees_to_radians(beta);
  sy := sin(beta);
  cy := cos(beta);
  gamma := degrees_to_radians(gamma);
  sz := sin(gamma);
  cz := cos(gamma);
  k := upper_limit;
  while k >= lower_limit do
  begin
    if (k < 1.0) and (k > -1.0) then
      step_size := 0.1;
    x := 0.50001;
    y := 0;
    if QVal = 0 then begin
      magnitude := 1;
      Q := 4 * sqrt(1 - k * k);
    end else begin
      magnitude := k * k + QVal * QVal;
      Q := -4 * QVal / magnitude;
    end;
    P := 4 * k / magnitude;
    for i := 0 to Iterations do begin
      temp_x := x * P - y * Q;
      y := x * Q + y * P;
      temp_y := y;
      x := 1 - temp_x;
      magnitude := sqrt(x * x + y * y);
      y := sqrt((-x + magnitude) / 2);
      x := sqrt((x + magnitude) / 2);
      if temp_y < 0 then x := -x;
      if (random < 0.5) then begin
        x := -x;
        y := -y;
      end;
      x := (1 - x) / 2;
      y := y / 2;
      z := P / 2;
      if i > 10 then projection(x, y, z);
    end;
    k := k - step_size;
  end;
end; { gen3ddrag}
(***************************************************)
(***************************************************)

procedure Image(WhichOne: Integer);
var TempColor: TColor;
{maxcolx,maxrowy,}{adapt,mode,} k, xscale, yscale, xoffset, yoffset
  {,pr}: integer;
  a, b, c, d, e, f: array[0..3] of Extended;
  x, y, newx, j: Extended;
  p {,pk}: array[0..3] of Extended;
  procedure image_draw(color: integer);
  var
    xme, yme, i, px, py: integer;
  begin
    xme := (FYImageX div 2);
    yme := (FYImageY div 2);
    x := 0;
    y := 0;
    for i := 1 to 25000 do begin
{Make this be 1 1000
FXCaption := '' (... to show and make it be seen)
Application.ProcessMessages;
1  25}
      j := Random;
      if j < p[0] then
        k := 0;
      if (j > p[0]) and (j < p[1]) then
        k := 1;
      if (j > p[1]) and (j < p[2]) then
        k := 2;
      if j > p[2] then
        k := 3;
      newx := (a[k] * x + b[k] * y + e[k]);
      y := (c[k] * x + d[k] * y + f[k]);
      x := newx;
      px := Round(x * xscale + xoffset);
      py := Round(y * yscale + yoffset);
      if (px >= -xme) and (px < xme) and (py >= -yme) and (py < yme)
        then
      begin
        TempColor := RGB(RGBArray[0, color], RGBArray[1, color],
          RGBArray[2, color]);
        MainForm.Image2.Canvas.Pixels[px + xme, yme - py] :=
          TempColor;
      end;
    end;
  end;

begin
  with MainForm.Image2.Canvas do begin
{	maxcolx :=(FYImageX);
 maxrowy := (FYImageY);}
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    case WhichOne of
      1:
        begin
          a[0] := 0; a[1] := 0.2; a[2] := -0.15; a[3] := 0.85;
          b[0] := 0; b[1] := -0.26; b[2] := 0.28; b[3] := 0.04;
          c[0] := 0; c[1] := 0.23; c[2] := 0.26; c[3] := -0.04;
          d[0] := 0.16; d[1] := 0.22; d[2] := 0.24; d[3] := 0.85;
          e[0] := 0; e[1] := 0; e[2] := 0; e[3] := 0;
          f[0] := 0; f[1] := 0.2; f[2] := 0.2; f[3] := 0.2;
          p[0] := 0.01; p[1] := 0.08; p[2] := 0.15; p[3] := 1.0;
          xscale := 300;
          yscale := 300;
          xoffset := -50;
          yoffset := -180;
          image_draw(10);
          TextOut(10, 10, 'Fern');
        end;

      2:
        begin
          a[0] := 0; a[1] := 0.1; a[2] := 0.42; a[3] := 0.42;
          b[0] := 0; b[1] := 0; b[2] := -0.42; b[3] := 0.42;
          c[0] := 0; c[1] := 0; c[2] := 0.42; c[3] := -0.42;
          d[0] := 0.5; d[1] := 0.1; d[2] := 0.42; d[3] := 0.42;
          e[0] := 0; e[1] := 0; e[2] := 0; e[3] := 0;
          f[0] := 0; f[1] := 0.2; f[2] := 0.2; f[3] := 0.2;
          p[0] := 0.05; p[1] := 0.20; p[2] := 0.6; p[3] := 1.0;
          xscale := 750;
          yscale := 750;
          xoffset := 0;
          yoffset := -160;
          image_draw(13);
          TextOut(10, 10, 'Tree');
        end;

      3:
        begin
          a[0] := 0.5; a[1] := 0.5; a[2] := 0.5; a[3] := 0;
          b[0] := 0; b[1] := 0; b[2] := 0; b[3] := 0;
          c[0] := 0; c[1] := 0; c[2] := 0; c[3] := 0;
          d[0] := 0.5; d[1] := 0.5; d[2] := 0.5; d[3] := 0;
          e[0] := 0; e[1] := 1.; e[2] := 0.5; e[3] := 0;
          f[0] := 0; f[1] := 0; f[2] := 0.5; f[3] := 0;
          p[0] := 0.33; p[1] := 0.66; p[2] := 1.0; p[3] := 1.0;
          xscale := 200;
          yscale := 200;
          xoffset := -200;
          yoffset := -160;
          image_draw(12);
          TextOut(10, 10, 'Sierpinski');
        end;

      4:
        begin
          a[0] := 0.333; a[1] := 0.333; a[2] := 0.667; a[3] := 0;
          b[0] := 0; b[1] := 0; b[2] := 0; b[3] := 0;
          c[0] := 0; c[1] := 0; c[2] := 0; c[3] := 0;
          d[0] := 0.333; d[1] := 0.333; d[2] := 0.667; d[3] := 0;
          e[0] := 0; e[1] := 1.; e[2] := 0.5; e[3] := 0;
          f[0] := 0; f[1] := 0; f[2] := 0.5; f[3] := 0;
          p[0] := 0.33; p[1] := 0.66; p[2] := 1.0; p[3] := 1.0;
          xscale := 120;
          yscale := 140;
          xoffset := -100;
          yoffset := -160;
          image_draw(14);
          TextOut(10, 10, 'Sierpinski');
        end;
    end; { of CASE }
  end;
end; { of procedure  }
(***************************************************)
(***************************************************)

(***************************************************)
(***************************************************)

procedure Sierpinski(level: integer);
var
  TempColor: TColor; maxcolx, maxrowy, x, y, switcher: integer;
  i: longint;
begin

  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
{Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    TextOut(10, 10, 'Sierpinski ' + I2s(level));

    x := random(FYImageX);
    y := random(FYImageY);
    for i := 0 to 120000 do
    begin
      switcher := random(4);
      case switcher of
        0: begin
            x := x div level;
            y := y div level;
{		x := x div 2;
  y := y div 2;}
          end;
        1: begin
            x := (x + maxcolx) * 2 div level;
            y := y div level;
{		x := (x + maxcolx) div 2;
  y := y div 2;}
          end;
        2: begin
            x := (x + maxcolx) div level;
            y := (y + maxrowy) * 2 div level;

          end;
        3: begin
            x := x div level;
            y := (y + maxrowy) * 2 div level;
{        	x := (x + (FYImageX div 2)) div 2;
  y := (y + maxrowy) div 2;}
          end;
      end;
      if i > 20 then
{TempColor:=RGB(RGBArray[0,4],RGBArray[1,4],RGBArray[2,4]);}
        Pixels[x, y] := TempColor;
    end;
  end; end;

(***************************************************)
(***************************************************)

procedure sierchet3; {Triangle }
var
  TempColor: TColor; maxcolx, maxrowy, x, y, switcher: integer;
  i: longint;
begin

  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
{Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    x := random(FYImageX);
    y := random(FYImageY);
    for i := 0 to 120000 do
    begin
      switcher := random(3);
      case switcher of
        0: begin
            x := x div 2;
            y := y div 2;
          end;
        1: begin
            x := (x + maxcolx) div 2;
            y := y div 2;
          end;
        2: begin
            x := (x + (FYImageX div 2)) div 2;
            y := (y + maxrowy) div 2;
          end;
      end;
      if i > 20 then
{TempColor:=RGB(RGBArray[0,4],RGBArray[1,4],RGBArray[2,4]);}
        Pixels[x, y] := TempColor;
    end;
    TextOut(10, 470, 'Sierchet Triangle');
  end; end;

(***************************************************)

procedure sierchet;
var
  TempColor: TColor; maxcolx, maxrowy, x, y, switcher: integer;
  i: longint;
begin
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
{Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    x := random(FYImageX);
    y := random(FYImageY);
    for i := 0 to 120000 do
    begin
      switcher := random(4);
      case switcher of
        0: begin
            x := x div 3;
            y := y div 3;
          end;
        1: begin
            x := (x + maxcolx) * 2 div 3;
            y := y div 3;
          end;
        2: begin
            x := (x + maxcolx) div 3;
            y := (y + maxrowy) * 2 div 3;
          end;
        3: begin
            x := x div 3;
            y := (y + maxrowy) * 2 div 3;
          end;
      end;
      if i > 20 then
{TempColor:=RGB(RGBArray[0,4],RGBArray[1,4],RGBArray[2,4]);}
        Pixels[x, y] := TempColor;
    end;
{Setcolor(4);}
    TextOut(10, 10, 'Sierchet Arches');
  end; end; { of   }

(***************************************************)


procedure sierchet2;
var
  TempColor: TColor;
  xme, maxcolx, maxrowy, switcher: integer;
  i, x, y: longint;
  s2: Extended;
begin

  with MainForm.Image2.Canvas do begin
    xme := ((FYImageX div 2) * (FYImageX div 2));
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
{Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    s2 := sqrt(0.5);
    x := random(FYImageX);
    y := random(FYImageY);
    for i := 0 to 120000 do
    begin
      switcher := random(3);
      case switcher of
        0: begin
            x := Round(x * s2);
            y := Round(y * s2);
          end;
        1: begin
            x := Round(sqrt((maxcolx * maxcolx + x * x) / 2));
            y := Round(y * s2);
          end;
        2: begin
            x := Round(sqrt((xme + x * x) / 2));
            y := Round(sqrt((maxrowy * maxrowy + y * y) / 2));
          end;
      end;
      if i > 20 then
        Pixels[x, y] := TempColor;
    end;
    TextOut(10, 10, 'Sierchet Forest');
  end; end; { of  Sier Forest }

(***************************************************)

procedure ifsdet(ifswhich: Byte);
var
  a: array[0..3] of Extended;
  b: array[0..3] of Extended;
  c: array[0..3] of Extended;
  d: array[0..3] of Extended;
  e: array[0..3] of Extended;
  f: array[0..3] of Extended;
  TempColor: Tcolor;
  TempChecker, i, j, k, m, iter,
    iterations, start_col, set_end, index: integer;
  Front, Back, Temparray: array of array of byte;
begin
  try
    SetLength(Front, 640, 480);
    SetLength(Back, 640, 480);
    SetLength(Temparray, 640, 480);
    for i := 0 to 640 - 1 do begin
      for j := 0 to 480 - 1 do Front[i, j] := 10; end;
    for i := 0 to 640 - 1 do begin
      for j := 0 to 480 - 1 do Back[i, j] := 10; end;
    for i := 0 to 640 - 1 do begin
      for j := 0 to 480 - 1 do Temparray[i, j] := 10; end;
    start_col := 0;
    set_end := 3;
    iterations := 8;
    if (ifswhich = 1) then begin
      a[0] := 0.5; a[1] := 0.5; a[2] := 0.5; a[3] := 0.0;
      b[0] := 0; b[1] := 0.0; b[2] := 0; b[3] := 0;
      c[0] := 0; c[1] := 0; c[2] := 0; c[3] := 0;
      d[0] := 0.5; d[1] := 0.5; d[2] := 0.5; d[3] := 0;
      e[0] := 75; e[1] := 0; e[2] := 150; e[3] := 0;
      f[0] := 0; f[1] := 150; f[2] := 150; f[3] := 0;
      start_col := 0;
      set_end := 3;
      iterations := 8;
    end else
      if (ifswhich = 2) then begin
        a[0] := 0; a[1] := 0.85; a[2] := 0.2; a[3] := -0.15;
        b[0] := 0; b[1] := 0.04; b[2] := -0.26; b[3] := 0.28;
        c[0] := 0; c[1] := -0.04; c[2] := 0.23; c[3] := 0.26;
        d[0] := 0.16; d[1] := 0.85; d[2] := 0.22; d[3] := 0.24;
        e[0] := 0; e[1] := 0; e[2] := 0; e[3] := 0;
        f[0] := 0; f[1] := 40; f[2] := 40; f[3] := 10;
        start_col := -100;
        set_end := 4;
        iterations := 32;
      end; {of displays}
    for i := 2 to 297 do begin
      Back[i + 100, 0] := 1;
      Back[i + 100, 299] := 1;
      Back[399, i] := 1;
      Back[100, i] := 1;
    end;
    for index := 0 to iterations do begin
      for i := 0 to 640 - 1 do begin
        for j := 0 to 480 - 1 do Front[i, j] := 10; end;
      for i := start_col to 300 do begin
        for j := 0 to 300 do begin
          TempChecker := Back[i + 100, j];
          if (TempChecker = 1) then begin
            iter := 0;
            while iter < set_end do begin
              k := Round(a[iter] * i + b[iter] * j + e[iter]);
              m := Round(c[iter] * i + d[iter] * j + f[iter]);
              iter := iter + 1;
              if ((k < 540) and (m < 480) and
                (k >= -100) and (m >= 0)) then begin
                Front[k + 100, m] := 1;
              end;
            end;
          end;
        end;
      end;
      temparray := front;
      front := back;
      back := temparray;
    end;
    with MainForm.Image2.Canvas do begin
      TempColor := RGB(255 - GetRValue(FBackGroundColor),
        255 - GetGValue(FBackGroundColor),
        255 - GetBValue(FBackGroundColor));
      for i := start_col to 300 do begin
        for j := 0 to 300 do begin
          if (front[i + 100, j] = 1) then
            Pixels[i + 100, j + 36] := TempColor;
        end; end; end;
  finally
    SetLength(Front, 0, 0);
    SetLength(Back, 0, 0);
    SetLength(Temparray, 0, 0);
  end;
end; { of procedure IfsDet }
(***************************************************)
(***************************************************)

(************************************************************)

procedure Apolly(level: integer);
var TempColor: TColor;
{maxcolx,maxrowy,} color: integer;
  a, b, c, xa, xb, xc, ya, yb, yc, xs, ys: Extended;
  procedure node(xa: Extended; ya: Extended; a: Extended; xb:
    Extended; yb: Extended;
    b: Extended; xc: Extended; yc: Extended; c: Extended; xs:
      Extended; ys: Extended; s: Extended;
    level: integer); forward;
  procedure gen_circle(xa: Extended; ya: Extended; a: Extended; xb:
    Extended; yb: Extended;
    b: Extended; xc: Extended; yc: Extended; c: Extended; level:
      integer);
  var
    s, temp: Extended;
  begin
    dec(level);
    s := 1 / a + 1 / b + 1 / c + 2 * (sqrt(1 / (b * c) + 1 / (c * a)
      + 1 / (a * b)));
    s := 1 / s;
    temp := (s + a) * (s + a) - (s + b) * (s + b) - xa * xa + xb * xb
      - ya * ya + yb * yb;
    ys := (temp * (xc - xa) - (xb - xa) * ((s + a) * (s + a) - (s + c)
      * (s + c) - xa * xa
      + xc * xc - ya * ya + yc * yc)) / (2 * ((yb - ya) * (xc - xa) -
        (yc - ya) *
      (xb - xa)));
    xs := (temp - 2 * ys * (yb - ya)) / (2 * (xb - xa));
    inc(color);
    TempColor := RGB(RGBArray[0, color], RGBArray[1, color],
      RGBArray[2, color]);
    MainForm.Image2.Canvas.Brush.Color := TempColor;
      {ys and s *0.7291666}
{MainForm.Image2.Canvas.Ellipse(Round(xs+320),Round(240-ys),
Round(s),Round(s));}
    MainForm.Image2.Canvas.Ellipse(Round(xs + 320 - s), Round((240 -
      ys) - s),
      Round(xs + 320 + s), Round((240 - ys) + s));
{x1=a1-a2,y1=b1-b2,x2=a1+a2,y2=b1+b2}
    if level > 0 then
      node(xa, ya, a, xb, yb, b, xc, yc, c, xs, ys, s, level);
  end;
  procedure node(xa: Extended; ya: Extended;
    a: Extended; xb: Extended; yb: Extended;
    b: Extended; xc: Extended; yc: Extended;
    c: Extended; xs: Extended; ys: Extended;
    s: Extended;
    level: integer);
  begin
    gen_circle(xa, ya, a, xb, yb, b, xs, ys, s, level);
    gen_circle(xb, yb, b, xc, yc, c, xs, ys, s, level);
    gen_circle(xa, ya, a, xc, yc, c, xs, ys, s, level);
  end;
begin
  if level < 1 then level := 1
  else if level > 8 then level := 8;
  a := 625;
  b := 375;
  c := 945;
  color := 1;
  xa := -725;
  ya := 235;
  xb := 275;
  yb := 268;
  xc := 180;
  yc := -1048;
  with MainForm.Image2.Canvas do begin
{	maxcolx :=(FYImageX-1);
 maxrowy := (FYImageY-1);}
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    TempColor := RGB(RGBArray[0, color], RGBArray[1, color],
      RGBArray[2, color]);
    Brush.Color := TempColor; Pen.Color := TempColor;
{	SetFillStyle(1,color);
 setColor(color);}
{Ellipse(Round(xa+320),Round(240-ya),Round(a),Round(a));}
    Ellipse(Round(xa + 320 - a), Round((240 - ya) - a),
      Round(xa + 320 + a), Round((240 - ya) + a));
    inc(color);
 {	SetFillStyle(1,color);
 SetColor(color);}
    TempColor := RGB(RGBArray[0, color], RGBArray[1, color],
      RGBArray[2, color]);
    Brush.Color := TempColor; Pen.Color := TempColor;
{Ellipse(Round(xb+320),Round(240-yb),Round(b),Round(b));}
    Ellipse(Round(xb + 320 - b), Round((240 - yb) - b),
      Round(xb + 320 + b), Round((240 - yb) + b));
    inc(color);
 {	SetFillStyle(1,color);
 SetColor(color);}
    TempColor := RGB(RGBArray[0, color], RGBArray[1, color],
      RGBArray[2, color]);
    Brush.Color := TempColor; Pen.Color := TempColor;
{Ellipse(Round(xc+320),Round(240-yc),Round(c),Round(c));}
    Ellipse(Round((xc + 320) - c), Round((240 - yc) - c),
      Round(xc + 320 + c), Round((240 - yc) + c));
{x1=a1-a2,y1=b1-b2,x2=a1+a2,y2=b1+b2}
    gen_circle(xa, ya, a, xb, yb, b, xc, yc, c, level);
  end; end; { of Apolly}




procedure pharoh;
var TempColor: TColor;
  GfColor, {maxcolx,maxrowy,}
    i: integer;
  a_line, b_line, x_o, y_o, radius, r_sq, height: Extended;
  xbig, ybig, rbig, rtan: Extended;
  procedure inverseOval(x: Extended; y: Extended; b: Extended;
    color: integer; aspect: Extended);
  var
    new_col, new_row: integer;
    length, new_length: Extended;
    a, a_square, b_square, two_a_square, two_b_square, four_a_square,
      four_b_square, d, row, col: longint;
  begin
    a := Round(b / aspect);
    b_square := Round(b * b);
    a_square := (a * a);
    row := Round(b);
    col := 0;
    two_a_square := a_square shl 1;
    four_a_square := a_square shl 2;
    four_b_square := b_square shl 2;
    two_b_square := b_square shl 1;
    d := two_a_square * ((row - 1) * (row)) + a_square +
      two_b_square * (1 - a_square);

    while a_square * row > b_square * col do
    begin
      length := sqrt((x_o - col - x) * (x_o - col - x) +
        (y_o - row - y) * (y_o - row - y));
      new_length := r_sq / length;
      new_col := Round(x_o - (x_o - col - x) * new_length / length);
      new_row := Round(-y_o + (y_o - row - y) * new_length / length);
{TempColor:=RGB(RGBArray[0,color],RGBArray[1,color],RGBArray[2,color]);}
      MainForm.Image2.Canvas.Pixels[new_col + 320, 240 -
        Round(new_row)] := TempColor;
      length := sqrt((x_o + col - x) * (x_o + col - x) +
        (y_o - row - y) * (y_o - row - y));
      new_length := r_sq / length;
      new_col := Round(x_o - (x_o + col - x) * new_length / length);
      new_row := Round(-y_o + (y_o - row - y) * new_length / length);
{TempColor:=RGB(RGBArray[0,color],RGBArray[1,color],RGBArray[2,color]);}
      MainForm.Image2.Canvas.Pixels[new_col + 320, 240 -
        Round(new_row)] := TempColor;
      length := sqrt((x_o - col - x) * (x_o - col - x) +
        (y_o + row - y) * (y_o + row - y));
      new_length := r_sq / length;
      new_col := Round(x_o - (x_o - col - x) * new_length / length);
      new_row := Round(-y_o + (y_o + row - y) * new_length / length);
{TempColor:=RGB(RGBArray[0,color],RGBArray[1,color],RGBArray[2,color]);}
      MainForm.Image2.Canvas.Pixels[new_col + 320, 240 -
        Round(new_row)] := TempColor;
      length := sqrt((x_o + col - x) * (x_o + col - x) +
        (y_o + row - y) * (y_o + row - y));
      new_length := r_sq / length;
      new_col := Round(x_o - (x_o + col - x) * new_length / length);
      new_row := Round(-y_o + (y_o + row - y) * new_length / length);
{TempColor:=RGB(RGBArray[0,color],RGBArray[1,color],RGBArray[2,color]);}
      MainForm.Image2.Canvas.Pixels[new_col + 320, 240 -
        Round(new_row)] := TempColor;
      if d >= 0 then
      begin
        dec(row);
        d := d - four_a_square * (row);
      end;
      d := d + two_b_square * (3 + (col shl 1));
      inc(col);
    end;

    d := two_b_square * (col + 1) * col + two_a_square * (row *
      (row - 2) + 1) + (1 - two_a_square) * b_square;
{ TempColor:=RGB(RGBArray[0,color],RGBArray[1,color],RGBArray[2,color]);}
    while row + 1 <> 0 do
    begin
      length := sqrt((x_o - col - x) * (x_o - col - x) +
        (y_o - row - y) * (y_o - row - y));
      new_length := r_sq / length;
      new_col := Round(x_o - (x_o - col - x) * new_length / length);
      new_row := Round(-y_o + (y_o - row - y) * new_length / length);
{ TempColor:=RGB(RGBArray[0,color],RGBArray[1,color],RGBArray[2,color]);}
      MainForm.Image2.Canvas.Pixels[new_col + 320, 240 -
        Round(new_row)] := TempColor;
      length := sqrt((x_o + col - x) * (x_o + col - x) +
        (y_o - row - y) * (y_o - row - y));
      new_length := r_sq / length;
      new_col := Round(x_o - (x_o + col - x) * new_length / length);
      new_row := Round(-y_o + (y_o - row - y) * new_length / length);
{TempColor:=RGB(RGBArray[0,color],RGBArray[1,color],RGBArray[2,color]);}
      MainForm.Image2.Canvas.Pixels[new_col + 320, 240 -
        Round(new_row)] := TempColor;
      length := sqrt((x_o - col - x) * (x_o - col - x) +
        (y_o + row - y) * (y_o + row - y));
      new_length := r_sq / length;
      new_col := Round(x_o - (x_o - col - x) * new_length / length);
      new_row := Round(-y_o + (y_o + row - y) * new_length / length);
{TempColor:=RGB(RGBArray[0,color],RGBArray[1,color],RGBArray[2,color]);}
      MainForm.Image2.Canvas.Pixels[new_col + 320, 240 -
        Round(new_row)] := TempColor;
      length := sqrt((x_o + col - x) * (x_o + col - x) +
        (y_o + row - y) * (y_o + row - y));
      new_length := r_sq / length;
      new_col := Round(x_o - (x_o + col - x) * new_length / length);
      new_row := Round(-y_o + (y_o + row - y) * new_length / length);
{TempColor:=RGB(RGBArray[0,color],RGBArray[1,color],RGBArray[2,color]);}
      MainForm.Image2.Canvas.Pixels[new_col + 320, 240 -
        Round(new_row)] := TempColor;
      if d <= 0 then
      begin
        inc(col);
        d := d + four_b_square * col;
      end;
      dec(row);
      d := d + two_a_square * (3 - (row shl 1));
    end;
{		b := b + 1;}
  end;

  procedure gen_circle(x: Extended; y: Extended; radius: Extended);
  begin
    inverseOval(x, y, radius, GFColor, 1.0);
    inverseOval(-x, y, radius, GFColor, 1.0);
  end;

begin
  with MainForm.Image2.Canvas do begin
{	maxcolx :=(FYImageX-1);
 maxrowy := (FYImageY-1); }
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    GfColor := 14;
    r_sq := 400000;
    xbig := 0;
    ybig := 0;
    rbig := 220;
    rtan := 140;
{	xtan := 0;}
{	ytan := ybig + rbig - rtan;}
    y_o := ybig - rbig;
    x_o := xbig;
    TempColor := RGB(RGBArray[0, 14], RGBArray[1, 14], RGBArray[2,
      14]);
    Brush.Color := TempColor; Brush.Style := bsSolid; {0,360,}
{	Ellipse(Round(xbig+320),240 - Round(ybig),
                Round(rbig),Round(rbig));}
    Ellipse(100, 20, 540, 460);
{Big first      320,240,220,220}{x1=a1-a2,y1=b1-b2,x2=a1+a2,y2=b1+b2}
    TempColor := RGB(RGBArray[0, 15], RGBArray[1, 15], RGBArray[2,
      15]);
    Brush.Color := TempColor; Brush.Style := bsSolid; {0,360,}
{	Ellipse(Round(xtan+320),240 - Round(ytan),
                Round(rtan),Round(rtan));}
    Ellipse(180, 20, 460, 300);
{Little        320,160,140,140          }
    TempColor := RGB(RGBArray[0, 0], RGBArray[1, 0], RGBArray[2, 0]);
    Pen.Color := TempColor;
    Brush.Color := TempColor; Brush.Style := bsSolid;
    MainForm.Image2.Canvas.Ellipse(271, 362, 369, 460);
    TempColor := RGB(RGBArray[0, 1], RGBArray[1, 1], RGBArray[2, 1]);
    Pen.Color := TempColor;
    Brush.Color := TempColor; Brush.Style := bsSolid;
    MainForm.Image2.Canvas.Ellipse(154, 324, 240, 410);
    MainForm.Image2.Canvas.Ellipse(400, 324, 486, 410);
    TempColor := RGB(RGBArray[0, 2], RGBArray[1, 2], RGBArray[2, 2]);
    Pen.Color := TempColor;
    Brush.Color := TempColor; Brush.Style := bsSolid;
    MainForm.Image2.Canvas.Ellipse(104, 248, 169, 312);
    MainForm.Image2.Canvas.Ellipse(472, 248, 536, 312);

    a_line := r_sq / (2 * rbig);
    b_line := r_sq / (2 * rtan);
    height := (b_line - a_line);
    radius := height / 2;
    height := radius * sqrt(2.0);
    GfColor := 14;
    for i := 0 to 15 do
    begin
      Inc(GfColor);
      if GfColor > 13 then GfColor := 0;
      TempColor := RGB(RGBArray[0, GfColor], RGBArray[1, GfColor],
        RGBArray[2, GfColor]);
      gen_circle(x_o + height * i, y_o + a_line + radius, radius);
      gen_circle(x_o + height * i, y_o + a_line + radius / 2, radius
        / 2);
      gen_circle(x_o + height * i, y_o + b_line - radius / 2, radius
        / 2);
      gen_circle(x_o + height * i + height / 2, y_o + a_line +
        3 * radius / 4, radius / 4);
      gen_circle(x_o + height * i + height / 2, y_o + b_line -
        3 * radius / 4, radius / 4);
      gen_circle(x_o + height * i + height / 2, y_o + a_line + radius
        / 8,
        radius / 8);
      gen_circle(x_o + height * i + height / 2, y_o + b_line - radius
        / 8,
        radius / 8);
      gen_circle(x_o + height * i + height / 2, y_o + a_line +
        5 * radius / 12, radius / 12);
      gen_circle(x_o + height * i + height / 2, y_o + b_line -
        5 * radius / 12, radius / 12);
      gen_circle(x_o + height * i + 0.4 * height, y_o + a_line +
        0.3 * radius, radius / 10);
      gen_circle(x_o + height * i + 0.6 * height, y_o + a_line +
        0.3 * radius, radius / 10);
      gen_circle(x_o + height * i + 0.4 * height, y_o + b_line -
        0.3 * radius, radius / 10);
      gen_circle(x_o + height * i + 0.6 * height, y_o + b_line -
        0.3 * radius, radius / 10);
    end;
  end;
end; { of procedure IPharaoh }


(***************************************************)
(***************************************************)


(************************************************************)

procedure TwoButtes; { 130 lines }
var
  T2, TempColor: TColor;

  procedure cactus(x1, y1, scale, level, y_max: integer;
    color1, color2: Tcolor);
  begin
    gen_quad(x1, y1, x1, y1 + 21 * scale, x1 + Round(1.6 * scale), y1
      + 22 * scale,
      x1 + Round(1.6 * scale), y1, level, y_max, color1, color2);
    gen_quad(x1 + Round(1.4 * scale), y1, x1 + Round(1.4 * scale), y1
      + 22 * scale, x1 + 3 * scale,
      y1 + 21 * scale, x1 + 3 * scale, y1, level, y_max, color1,
        color2);
    gen_quad(x1, y1 + 9 * scale, x1 + 7 * scale, y1 + 9 * scale, x1 +
      7 * scale, y1 + 12 * scale,
      x1, y1 + 12 * scale, 0, y_max, color1, color2);
    gen_quad(x1, y1 + 9 * scale, x1 + 6 * scale, y1 + 9 * scale, x1 +
      7 * scale, y1 + 12 * scale,
      x1, y1 + 12 * scale, level, y_max, color1, color2);
    gen_quad(x1 + 7 * scale, y1 + 9 * scale, x1 + 7 * scale, y1 + 16
      * scale,
      x1 + Round(8.5 * scale), y1 + 17 * scale, x1 + Round(8.5 *
        scale), y1 + 9 * scale,
      level, y_max, color1, color2);
    gen_quad(x1 + Round(8.4 * scale), y1 + 9 * scale, x1 + Round(8.4
      * scale), y1 + 16 * scale,
      x1 + 10 * scale, y1 + 17 * scale, x1 + 10 * scale, y1 + 10 *
        scale,
      level, y_max, color1, color2);
    gen_quad(x1, y1 + 7 * scale, x1 - 6 * scale, y1 + 7 * scale, x1 -
      6 * scale, y1 + 10 * scale,
      x1, y1 + 10 * scale, 0, y_max, color1, color2);
    gen_quad(x1, y1 + 7 * scale, x1 - 6 * scale, y1 + 7 * scale, x1 -
      6 * scale, y1 + 10 * scale,
      x1, y1 + 10 * scale, level, y_max, color1, color2);
    gen_quad(x1 - 7 * scale, y1 + 8 * scale, x1 - 7 * scale, y1 + 12
      * scale, x1 - Round(5.4 * scale),
      y1 + 13 * scale, x1 - Round(5.4 * scale), y1 + 7 * scale,
      level, y_max, color1, color2);
    gen_quad(x1 - Round(5.6 * scale), y1 + 7 * scale,
      x1 - Round(5.6 * scale), y1 + 13 * scale,
      x1 - 4 * scale, y1 + 12 * scale,
      x1 - 4 * scale, y1 + 7 * scale,
      level, y_max, color1, color2);
  end;

begin
  FractalFilename := 'H_BUTTES.BMP';
  with MainForm.Image2.Canvas do begin
{	maxcolx :=(FYImageX-1);
 maxrowy := (FYImageY-1); }
{TempColor:=RGB(0, 255, 255);
Brush.Color:=TempColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));}
    Font.Color := clBlack;
    T2 := RGB(0, 128, 128);
    Pen.Color := T2;
    Brush.Color := T2;
    Ellipse(470, 170, 520, 220);
    TextOut(10, 10, 'Hello Buttes');
    MainForm.Show;
(*sky done in by fill rect... all sky
 { sky box of 2 triangles}
 Triangle[1].x := 0;
 Triangle[1].y := 0;
 Triangle[2].x := 0;
 Triangle[2].y := 479;
 Triangle[3].x := 639;
 Triangle[3].y := 0;
Polygon(Triangle);
 Triangle[1].x := 639;
 Triangle[1].y := 0;
 Triangle[2].x := 639;
 Triangle[2].y := 479;
 Triangle[3].x := 0;
 Triangle[3].y := 479;
Polygon(Triangle);
*)
  { dirt/veg box of 2 triangles}
    Triangle[1].x := 0;
    Triangle[1].y := 400;
    Triangle[2].x := 0;
    Triangle[2].y := 479;
    Triangle[3].x := 639;
    Triangle[3].y := 400;
    TempColor := RGB(128, 128, 128);
    Pen.Color := TempColor;
    Brush.Color := TempColor;
    Polygon(Triangle);
    Triangle[1].x := 639;
    Triangle[1].y := 400;
    Triangle[2].x := 639;
    Triangle[2].y := 479;
    Triangle[3].x := 0;
    Triangle[3].y := 479;
    Polygon(Triangle); {5,7 was    }
    MainForm.HiddenFX.Caption := 'Hello.1';
    Application.ProcessMessages;
{	y_max := 280;}
{Mid} gen_quad(-330, -150, -160, 0, 0, 0, 120, -130, 4, 280,
  clPurple, T2);

{        bRotateImage:=False;
        bRotatingImage:=True;
        Repeat Application.ProcessMessages until (bRotateImage=True);
}
    gen_quad(-90, -110, 85, 50, 200, 50, 440, -180, 4, 280, clPurple,
      T2);
{        bRotateImage:=False;
        bRotatingImage:=True;
        Repeat Application.ProcessMessages until (bRotateImage=True);
}
    TempColor := RGB(12, 98, 128);
    Pen.Color := TempColor;
    Brush.Color := TempColor;
    gen_quad(-160, -10, -160, 220, -120, 220, -120, -10, 4, 280,
      TempColor, T2);
{        bRotateImage:=False;
        bRotatingImage:=True;
        Repeat Application.ProcessMessages until (bRotateImage=True);
} gen_quad(-120, -10, -120, 190, -80, 200, -80, -10, 4, 280,
  TempColor, T2);
{        bRotateImage:=False;
        bRotatingImage:=True;
        Repeat Application.ProcessMessages until (bRotateImage=True);
} gen_quad(-80, -15, -80, 230, -50, 235, -50, -15, 4, 280, TempColor,
  T2);
{        bRotateImage:=False;
        bRotatingImage:=True;
        Repeat Application.ProcessMessages until (bRotateImage=True);
} gen_quad(-50, -10, -50, 100, 0, 180, 0, -10, 4, 280, TempColor,
  T2);
{        bRotateImage:=False;
        bRotatingImage:=True;
        Repeat Application.ProcessMessages until (bRotateImage=True);
}
    gen_quad(80, 45, 100, 180, 104, 200, 104, 45, 4, 280, TempColor,
      T2);
{        bRotateImage:=False;
        bRotatingImage:=True;
        Repeat Application.ProcessMessages until (bRotateImage=True);
} gen_quad(100, 45, 100, 200, 128, 210, 128, 45, 4, 280, TempColor,
  T2);
{        bRotateImage:=False;
        bRotatingImage:=True;
        Repeat Application.ProcessMessages until (bRotateImage=True);
} gen_quad(125, 50, 125, 215, 152, 220, 152, 50, 4, 280, TempColor,
  T2);
{        bRotateImage:=False;
        bRotatingImage:=True;
        Repeat Application.ProcessMessages until (bRotateImage=True);
} gen_quad(150, 50, 150, 160, 200, 140, 200, 50, 4, 280, TempColor,
  T2);
{        bRotateImage:=False;
        bRotatingImage:=True;
        Repeat Application.ProcessMessages until (bRotateImage=True);
}
    TempColor := RGB(128, 128, 128);
    Pen.Color := TempColor;
    Brush.Color := TempColor;
 {	y_max := -40;}{-100}{5 was 4}
    gen_quad(-330, -300, -330, -110, 330, -100, 330, -300, 5, -40,
      TempColor, T2);
{        bRotateImage:=False;
        bRotatingImage:=True;
        Repeat Application.ProcessMessages until (bRotateImage=True);
}
{	y_max := 0;}
    MainForm.HiddenFX.Caption := 'Hello.2';
    Application.ProcessMessages;
    cactus(-110, -130, 3, 4, 0, clGreen, clLime);
    cactus(-200, -120, 2, 4, 0, clGreen, clLime);
    cactus(0, -160, 4, 4, 0, clGreen, clLime);
    cactus(210, -200, 6, 4, 0, clGreen, clLime);
  end;
  MainForm.HiddenFX.Caption := 'FX';
end; { Procedure Hello Buttes }
(************************************************************)


(************************************************************)

procedure PuertoRico; {130 lines }
var TempColor: TColor;
{maxcolx,maxrowy: integer;   }
begin
  FractalFilename := 'PuertoRico.BMP';
  with MainForm.Image2.Canvas do begin
{	maxcolx :=(FYImageX-1);
 maxrowy := (FYImageY-1);}
{TempColor:=RGB(0, 255,255);
Brush.Color:=TempColor;
Pen.Color := TempColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));
Font.Color:= clBlack;
MainForm.Show;}
    TextOut(35, 10, 'Puerto Rico');
    MainForm.HiddenFX.Caption := 'Puerto Rico.1';

    Application.ProcessMessages;
{	y_max := 180;}
    generate(20, 50, 160, 160, 500, -50, 6, 180, clTeal, clGray);
{	y_max := 160;}
    generate(-780, -200, 10 {30}, 130, 480, -150, 6, 160, clOlive,
      clSilver); {4,8}
{	y_max := 180;}
    generate(-480, -20, -240, 60, 0, -60, 5, 180, clTeal, clGreen);
      {5,9}
    generate(-100, -260, 240, 40, 500, -180, 5, 180, clGreen,
      clGray);

 { water level box of 2 triangles}
    TempColor := RGB(0, 123, 255);
    Brush.Color := TempColor;
    Pen.Color := TempColor;
    Triangle[1].x := 0;
    Triangle[1].y := 350;
    Triangle[2].x := 0;
    Triangle[2].y := 449;
    Triangle[3].x := 639;
    Triangle[3].y := 449;
    MainForm.Image2.Canvas.Polygon(Triangle);
    Triangle[1].x := 639;
    Triangle[1].y := 350;
    Triangle[2].x := 639;
    Triangle[2].y := 449;
    Triangle[3].x := 0;
    Triangle[3].y := 350;
    MainForm.Image2.Canvas.Polygon(Triangle);
 { veg box of 2 triangles}
    Triangle[1].x := 0;
    Triangle[1].y := 449;
    Triangle[2].x := 0;
    Triangle[2].y := 479;
    Triangle[3].x := 639;
    Triangle[3].y := 479;
    MainForm.Image2.Canvas.Polygon(Triangle);
    Triangle[1].x := 639;
    Triangle[1].y := 449;
    Triangle[2].x := 639;
    Triangle[2].y := 479;
    Triangle[3].x := 0;
    Triangle[3].y := 449;
    MainForm.Image2.Canvas.Polygon(Triangle);
    MainForm.HiddenFX.Caption := 'Puerto Rico.2';
    Application.ProcessMessages;
{	y_max :=  -100;}
    generate(-770, -300, -250, -110, 600, -300, 5, -100, clTeal,
      clGreen);
    generate(-550, -280, -60, -140, 400, -300, 5, -100, clGray,
      clTeal);
    MainForm.HiddenFX.Caption := 'Puerto Rico.3';
    Application.ProcessMessages;
    generate(-220, -280, 80, -130, 340, -300, 4, -100, clTeal,
      clGray);
    generate(-200, -280, 230, -120, 580, -300, 4, -100, clGray,
      clAqua);
    MainForm.HiddenFX.Caption := 'FX';
  end;
end; { of Procedure Puerto Rico }
(************************************************************)


(************************************************************)

procedure Earth; { 120 lines }
const
{Earth color_set:
1-blue  15 -white   14 yellow, 4 pink    11 light blue   9 DK grey?
13 light purple      12 pink   10 black   5 blue  3 brown}

  xa: array[0..32] of integer = (-82, -80, -90, -70, -50, -30, -25,
    25, 40, 42,
    20, 35, 40, 50, 60, 60, -28, 70, -25, 70, 108, 81, 60, 45, 48,
      96, 45, 38, -8, 0,
    -20, -28, 55);
  xb: array[0..32] of integer = (-70, -70, -80, -50, -30, 25, -25,
    40, 65, 65,
    40, 38, 40, 60, 60, 70, -28, 90, -20, 105, 92, 70, 56, 48, 54,
      100, 38, 46, 12,
    14, 14, 8, 62);
  xc: array[0..32] of integer = (-70, -70, -80, -50, -50, 20, 30, 40,
    40, 58,
    40, 37, 50, 50, 70, 75, 20, 90, -40, 95, 83, 70, 45, 60, 60, 54,
      106, 65, 12,
    40, 14, -8, 95);
  xd: array[0..32] of integer = (-90, -80, -90, -70, -30, 20, 20, 25,
    50, 50,
    20, 40, 50, 60, 70, 75, 20, 70, -40, 90, 81, 108, 45, 56, 96, 45,
      100, 106, 8,
    44, 0, -30, 55);
  ya: array[0..32] of integer = (52, 52, 76, 80, 76, 38, 10, 80, 90,
    55, 50, 3,
    60, 60, 52, 55, 80, 115, 38, 109, 76, 80, -130, -124, -90, -70,
      -60, -50, 0,
    -10, 10, 90, -100);
  yb: array[0..32] of integer = (52, 52, 76, 80, 80, 30, 30, 80, 70,
    70, 40, 5,
    8, 52, 38, 38, 20, 120, 78, 104, 58, 95, -124, -90, -65, -60, -50,
      -25, 0,
    -10, 10, 80, -100);
  yc: array[0..32] of integer = (60, 80, 80, 55, 56, 38, 10, 90, 80,
    70, 3, -5,
    60, 20, 38, 40, 20, 106, 78, 76, 60, 110, -124, -100, -100, -65,
      -50, -25,
    -18, -30, -18, 75, -50);
  yd: array[0..32] of integer = (60, 80, 77, 55, 38, 30, 30, 90, 60,
    60, 3, -4,
    20, 27, 38, 40, 74, 109, 43, 76, 80, 76, -124, -124, -70, -60,
      -60, -50,
    -18, -30, -18, 85, -50);
  color_value: integer = 2;
  level1: integer = 4;
var
{	Triangle: array[1..3] of PointType;}
{T3,T4,} T2, TempColor: TColor; maxcolx, maxrowy,
{Timing,	interim,i,j,}
  i, row, col {,c_type}: integer;
{	y_max,x,y,xz,yz,xp,yp: Extended;}
     {	ch1: char;
 file_no: string[3]; }
  x_center, y_center {, radius}: longint;
begin
  FractalFilename := 'EARTH000.BMP';
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
    TempColor := RGB(0, 0, 0);
    Brush.Color := TempColor;
    Pen.Color := TempColor;
    Brush.Style := bsSolid;
    FillRect(Rect(0, 0, maxcolx, maxrowy));
    Font.Color := clWhite;
{MainForm.Show;}
    TextOut(10, 10, 'Earth viewed from Moon');
    x_center := -100;
    y_center := 0;
{	radius := 150;}
    for i := 0 to 2000 do begin
      row := Random(FYImageY);
      col := Random(FYImageX);
      Pixels[col, row] := clWhite; {Stars}
    end;

    TempColor := clNavy;
    Brush.Color := TempColor; Pen.Color := TempColor;
    Ellipse(90, 100, 380, 390); {Earth}
{landscape}
{	y_max := 280;}
    TempColor := clOlive;
    T2 := clGreen;
    for i := 0 to 32 do
      gen_quad(xa[i] + x_center, ya[i] + y_center, xb[i] + x_center,
        yb[i] + y_center, xc[i] + x_center, yc[i] + y_center, xd[i] +
        x_center, yd[i] + y_center, 3, 280, TempColor, T2); {land}
    for i := 0 to 32 do
      gen_quad(xa[i] + x_center, ya[i] + y_center, xb[i] + x_center,
        yb[i] + y_center, xc[i] + x_center, yc[i] + y_center, xd[i] +
        x_center, yd[i] + y_center, level1, 280, T2, TempColor);
          {Clouds ?}
    Brush.Color := clMaroon; Pen.Color := clRed;
    Ellipse(116, 72, 132, 88); {Satellite}
    ranFillOval(-85, -3, 145, clBlack, 1.0); { Moon Shadow }
    MainForm.HiddenFX.Caption := 'Earth  ';
    Application.ProcessMessages;

 {Clouds ?}{Moon land}
    T2 := RGB(123, 123, 234);
    TempColor := clGray;
    Brush.Color := TempColor; Pen.Color := TempColor;
{	y_max := -109;}
    generate(-470, -320, -250, -100, 300, -320, 4, -109, TempColor,
      T2); {14,6}
    generate(-350, -310, -60, -170, 300, -320, 4, -109, TempColor,
      T2);
    generate(-220, -310, 80, -160, 340, -320, 4, -109, TempColor,
      T2);
    generate(-200, -310, 230, -90, 580, -320, 4, -109, TempColor,
      T2);
{Craters}
    TempColor := clBlack;
    Brush.Color := TempColor; Pen.Color := TempColor;
    Ellipse(13, 423, 27, 427);
    Ellipse(34, 465, 66, 475);
    Ellipse(127, 464, 203, 484);
    Ellipse(215, 458, 255, 472);
    Ellipse(420, 409, 427, 414);
    Ellipse(498, 472, 542, 479);
    Ellipse(572, 439, 628, 446);
  end;
  MainForm.HiddenFX.Caption := 'FX';
end; { of Procedure Earth  }
(************************************************************)



(************************************************************)

procedure LightsOn; {140 lines }
const color_value: integer = 2;
var TempColor: TColor; maxcolx, maxrowy, i, row, col: integer;
begin {'DENVERCO.PCX'; } {sandia Desert}
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
    TempColor := RGB(0, 0, 0);
    Pen.Color := TempColor;
    Brush.Color := TempColor;
    Brush.Style := bsSolid;
    FillRect(Rect(0, 0, maxcolx, maxrowy));
    Font.Color := clWhite;
    Textout(10, 10, 'The Lights are On, Nobody is Home');
{MainForm.Show;}
    TempColor := RGB(255, 255, 255);
    for i := 0 to 750 do
    begin
      row := Random(479);
      col := Random(639);
      Pixels[col, row] := TempColor;
    end;
    TempColor := RGB(128, 128, 128);
    Brush.Color := TempColor;
    Pen.Color := TempColor;
        { Moon and shadow}
    Ellipse(160, 12, 300, 170);
    ranFillOval(-100, 150 {37} {117}, 80, 0, 1.0);
{ 	y_max := 160;}
    TempColor := RGB(RGBArray[0, 5], RGBArray[1, 5], RGBArray[2, 5]);
    Brush.Color := TempColor;
    Pen.Color := TempColor;
    generate(-400, 20, -230, 150, -20, 30, 6, 160, TempColor,
      clGreen); { M1 Mountains }
    {	y_max := 180; }{ll,um,lr ? }
    generate(100, -130, 280, 110, 500, -80, 5, 180, clGray, clGreen);
      {M3}
   { Y = 290 down to 480 blanked : Town}
  { Two triangles to make a blank rectangle}
    Triangle[1].x := 0;
    Triangle[1].y := 500;
    Triangle[2].x := 0;
    Triangle[2].y := 290;
    Triangle[3].x := 639;
    Triangle[3].y := 290;
    TempColor := RGB(0, 0, 255);
    Brush.Color := TempColor;
    Pen.Color := TempColor;
    Polygon(Triangle);
    Triangle[1].x := 639;
    Triangle[1].y := 290;
    Triangle[2].x := 639;
    Triangle[2].y := 500;
    Triangle[3].x := 0;
    Triangle[3].y := 500;
    Polygon(Triangle);
    MainForm.HiddenFX.Caption := 'Night light.2';
    Application.ProcessMessages;
                 {	y_max := -100;}{Streets and lights }
    TempColor := RGB(244, 234, 34);
    Pen.Color := TempColor;
    Moveto(320, 300);
    Lineto(320, 479);
    Moveto(360, 300);
    Lineto(440, 479);
    Moveto(400, 300);
    Lineto(560, 479);
    Moveto(440, 300);
    Lineto(639, 435);
    Moveto(480, 300);
    Lineto(639, 390); {}
    Moveto(520, 300);
    Lineto(639, 362); {}
    Moveto(290, 300);
    Lineto(230, 479);
    Moveto(260, 300);
    Lineto(140, 479);
    Moveto(230, 300);
    Lineto(50, 479);
    Moveto(200, 300);
    Lineto(0, 435);
    Moveto(170, 300);
    Lineto(0, 405);
    Moveto(140, 300);
    Lineto(0, 377);
    Moveto(0, 355);
    Lineto(639, 355);
    Moveto(0, 385);
    Lineto(639, 384);
    Moveto(0, 416);
    Lineto(639, 413);
    Moveto(0, 442);
    Lineto(639, 442);
{Divide the city Mtns}
{	y_max := 160; }
    gen_quad(-780, -190, 0, 70, 210, 80, 520, -140, 6, 160, clgray,
      clmaroon); {4,8} {M2}
    MainForm.HiddenFX.Caption := 'Night light .3';
    Application.ProcessMessages;
{Circular subdivision
x1=a1-a2,y1=b1-b2,x2=a1+a2,y2=b1+b2}
    Brush.Color := TempColor;
    Brush.Style := bssolid;
    Ellipse(10, 390, 30, 400); {,660,300);}
    Ellipse(220, 405, 250, 416); {669,309);}
    Ellipse(400, 330, 420, 340); {,790,360);}
    Ellipse(567, 445, 591, 450); {,799,369);}

    TempColor := RGB(255, 255, 255);
    for i := 0 to 666 do { litle lights }
    begin
      row := random(159) + 300;
      col := random(640);
      Pixels[col, row] := TempColor;
    end;
    TempColor := RGB(255, 255, 0);
    for i := 0 to 77 do {Big lights }
    begin
      row := random(30) + 449;
      col := random(640);
      Pixels[col, row] := TempColor;
      Pixels[col, row] := TempColor;
      Pixels[col - 1, row] := TempColor;
      Pixels[col + 1, row] := TempColor;
      Pixels[col, row - 1] := TempColor;
      Pixels[col, row + 1] := TempColor;
    end;
    TempColor := RGB(255, 234, 56);
    for i := 0 to 109 do
    begin
      row := random(100) + 379;
      col := random(640);
      Pixels[col, row] := TempColor;
      Pixels[col, row] := TempColor;
      Pixels[col - 1, row] := TempColor;
      Pixels[col + 1, row] := TempColor;
      Pixels[col, row - 1] := TempColor;
      Pixels[col, row + 1] := TempColor;
    end;
  end; MainForm.HiddenFX.Caption := 'FX';
end; { of Procedure LightsOn }
(************************************************************)



(************************************************************)

procedure forest; { 250 lines}
const
  ln_2: Extended = (0.6931471);

var
  Square: array[1..4] of TPoint;
var
  TempColor: TColor;
  maxcolx, maxrowy, i, level, width, indx, jndx: integer;
{	Triangle: array[1..3] of PointType;}
  height, left_alpha, right_alpha, left_angle, right_angle,
    left_width_factor, left_height_factor, right_width_factor,
    right_height_factor, x, y, x1, y1, val1, val2: Extended;
  list: array[0..20, 0..1] of Extended;

  {sub}procedure t_generate(x: Extended; y: Extended; width: integer;
    height: Extended; angle: Extended; level: integer);
  var
    x1, y1: Extended;
  begin
    turtle_x := x;
    turtle_y := y;
    turtle_r := height;
    step;
    x1 := turtle_x;
    y1 := turtle_y;
    dec(level);
    if level < 3 then
    begin
      TempColor := RGB(0, 245, 33);
      MainForm.Image2.Canvas.Pen.Color := TempColor;
      MainForm.Image2.Canvas.Brush.Color := TempColor;
    end
    else
    begin
      TempColor := RGB(123, 134, 45);
      MainForm.Image2.Canvas.Pen.Color := TempColor;
      MainForm.Image2.Canvas.Brush.Color := TempColor;
    end;
    if Abs(x - x1) > Abs(y - y1) then
    begin
      Square[1].x := Round(x + 320);
      Square[1].y := Round(240 - y {175 - y*0.729}) + width div 2;
      Square[2].x := Round(x + 320);
      Square[2].y := Round(240 - y {175 - y*0.729}) - width div 2;
      Square[3].x := Round(x1 + 320);
      Square[3].y := Round(240 - y1 {175 - y1*0.729}) - width div 2;
      Square[4].x := Round(x1 + 320);
      Square[4].y := Round(240 - y1 {175 - y1*0.729}) + width div 2;
    end
    else
    begin
      Square[1].x := Round(x + 320) - width div 2;
      Square[1].y := Round(240 - y {175 - y*0.729});
      Square[2].x := Round(x + 320) + width div 2;
      Square[2].y := Round(240 - y {175 - y*0.729});
      Square[3].x := Round(x1 + 320) + width div 2;
      Square[3].y := Round(240 - y1 {175 - y1*0.729});
      Square[4].x := Round(x1 + 320) - width div 2;
      Square[4].y := Round(240 - y1 {175 - y1*0.729});
    end;
    MainForm.Image2.Canvas.Polygon(Square);
    if level > 0 then
    begin
      turtle_theta := point(x, y, x1, y1);
      turn(left_angle);
      t_generate(x1, y1, Round(left_width_factor * width),
        left_height_factor *
        height, left_angle, level);
      turtle_theta := point(x, y, x1, y1);
      turn(-right_angle);
      t_generate(x1, y1, Round(right_width_factor * width),
        right_height_factor *
        height, right_angle, level);
    end;
  end;

  procedure trees(x: Extended; y: Extended);
  begin
    height := Round((240 - y) / 12);
    width := Round((240 - y) / 48);
    left_alpha := 1.3 + Random;
    right_alpha := 1.3 + Random;
    left_angle := 20.0 + 5 * Random;
    right_angle := 20.0 + 5 * Random;
    level := 14;
    left_width_factor := exp((-1 / left_alpha) * ln_2);
    left_height_factor := exp((-2 / (3 * left_alpha)) * ln_2);
    right_width_factor := exp((-1 / right_alpha) * ln_2);
    right_height_factor := exp((-2 / (3 * right_alpha)) * ln_2);
    x1 := x;
    y1 := y + height;
    TempColor := RGB(123, 134, 45);
{TempColor:=RGB(0,245,33);  }{lower tree base?}
    MainForm.Image2.Canvas.Pen.Color := TempColor;
    MainForm.Image2.Canvas.Brush.Color := TempColor;
    if Abs(x - x1) > Abs(y - y1) then
    begin
      Square[1].x := Round(x + 320);
      Square[1].y := Round(240 - y {175 - y*0.729}) + width div 2;
      Square[2].x := Round(x + 320);
      Square[2].y := Round(240 - y {175 - y*0.729}) - width div 2;
      Square[3].x := Round(x1 + 320);
      Square[3].y := Round(240 - y1 {175 - y1*0.729}) - width div 2;
      Square[4].x := Round(x1 + 320);
      Square[4].y := Round(240 - y1 {175 - y1*0.729}) + width div 2;
    end
    else
    begin
      Square[1].x := Round(x + 320) - width div 2;
      Square[1].y := Round(240 - y {175 - y*0.729});
      Square[2].x := Round(x + 320) + width div 2;
      Square[2].y := Round(240 - y {175 - y*0.729});
      Square[3].x := Round(x1 + 320) + width div 2;
      Square[3].y := Round(240 - y1 {175 - y1*0.729});
      Square[4].x := Round(x1 + 320) - width div 2;
      Square[4].y := Round(240 - y1 {175 - y1*0.729});
    end;
    MainForm.Image2.Canvas.Polygon(Square);
    turtle_theta := point(x, y, x1, y1);
    turn(left_angle);
    t_generate(x1, y1, Round(left_width_factor * width),
      left_height_factor * height, left_angle, level);
    turtle_theta := point(x, y, x1, y1);
    turn(-right_angle);
    t_generate(x1, y1, Round(right_width_factor * width),
      right_height_factor * height, right_angle, level);
  end;

begin { = 'forest00.pcx';}
  Square[1].x := 50;   Square[1].y := 100;
  Square[2].x := 100;  Square[2].y := 100;
  Square[3].x := 100;  Square[3].y := 100;
  Square[4].x := 150;  Square[4].y := 150;

  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
    TempColor := RGB(0, 255, 255);
    Brush.Color := TempColor;
    Pen.Color := TempColor;
    Brush.Style := bsSolid;
    FillRect(Rect(0, 0, maxcolx, maxrowy));
    Font.Color := clBlack;
    TextOut(350, 10, 'Forest');
{MainForm.Show;}
{ sky box of 2 triangles}
(* 	Triangle[1].x := 0;
 Triangle[1].y := 0;
 Triangle[2].x := 0;
 Triangle[2].y := 479;
 Triangle[3].x := 639;
 Triangle[3].y := 0;
 Polygon(Triangle);
 Triangle[1].x := 639;
 Triangle[1].y := 0;
 Triangle[2].x := 639;
 Triangle[2].y := 479;
 Triangle[3].x := 0;
 Triangle[3].y := 479;
 Polygon(Triangle);*)
{	y_max := 180;}
    generate(-220, -240, 220, 125, 500, -40, 6, 180, clGray, clTeal);
{	y_max := 160;}
    generate(-880, -200, 30, 125, 680, -150, 6, 160, clSilver,
      clTeal); {4,5}
{	y_max := 180;}
    generate(-580, 0, -240, 100, 150, -60, 5, 180, clGray, clTeal);
    generate(-300, -260, 200, 40, 500, -180, 5, 180, clPurple,
      clTeal);
    MainForm.HiddenFX.Caption := 'Forest ';
    Application.ProcessMessages;
    Square[1].x := 0;
    Square[1].y := 300;
    Square[2].x := 639;
    Square[2].y := 300;
    Square[3].x := 639;
    Square[3].y := 479; (* 349*)
    Square[4].x := 0;
    Square[4].y := 479; (*349*)
    TempColor := RGB(0, 128, 134);
    MainForm.Image2.Canvas.Pen.Color := TempColor;
    MainForm.Image2.Canvas.Brush.Color := TempColor;
    Polygon(Square);
    for i := 0 to 20 do
    begin
      list[i, 0] := 639 * Random - 320;
      list[i, 1] := 470 * Random - 240;
    end;
    for indx := 0 to 19 do
    begin
      for jndx := indx to 20 do
      begin
        if list[jndx, 1] < list[indx, 1] then
        begin
          val1 := list[jndx, 0];
          val2 := list[jndx, 1];
          list[jndx, 0] := list[indx, 0];
          list[jndx, 1] := list[indx, 1];
          list[indx, 0] := val1;
          list[indx, 1] := val2;
        end;
      end;
    end;

    for i := {20} 5 downto 0 do
    begin
      x := list[i, 0];
      y := list[i, 1];
      trees(x, y);
    end;
  end; MainForm.HiddenFX.Caption := 'FX';
end; { of Procedure Forest  }
(************************************************************)

procedure SetNumb3DImage;
begin
  with MainForm.Image2.Canvas do begin
{	maxcolx :=(FYImageX-1);
 maxrowy := (FYImageY-1);}
{Pen.Color := FBackGroundColor;
Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,(FYImageX-1),(FYImageY-1)));
Pen.Color := RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Font.Color:= RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));}
    Textout(10, 10, 'Numb Fun 3D Image');
{MainForm.Show;}
  end; end;


procedure Numb3DImage {(NumbSkullBrain:NoBrainer)};
{(was...3dFern)
xscale,yscale,xoffset,yoffset,Iterations,Horizon:Integer;
V1,V2,V3,V4:TColor);
Alpha, Beta, Gamma,P,Q:Extended;
a,b,c,d,e,f,g,h,m,n,p,q,r: array[0..3] of Extended}
{
Procedure Fern3DImage(xscale,yscale,xoffset,yoffset,Iterations:Integer;
                        Alpha, Beta,Gamma:Extended;
                        FTempColor:TColor);}
const
{	alpha: array[0..3] of Extended = (30,45,15,95);
 beta: array[0..3] of Extended = (115,105,70,40);
 gamma: array[0..3] of Extended = (25,70,20,-30);}
  rad_per_degree: Extended = 0.0174533;
{	hues: array[0..3] of integer = (2,10,11,14);}
                              {Green, LightGreen, LightCyan, Yellow}
var Fx, Fy, i, px, py, k: integer;
{	a,b,c,d,e,f,g,h,m,n,p,q,r: array[0..3] of Extended;}
  vx, vy, x, y, z, newx, newy, j, ca, cb, cg, sa, sb, sg: Extended;
begin
{	a,b,c,d,e,f,g,h,m,n,p,q,r: array[0..3] of Extended;}

  Fx := (FYImageX div 2);
  Fy := (FYImageX div 2);
{	xscale := 40;
 yscale := 50;
 xoffset := 60;
 yoffset := -180;}
{	for index:=0 to 3 do}
  ca := cos(NumbSkullBrain.alpha * 0.0174533);
  cb := cos(NumbSkullBrain.beta * 0.0174533);
  cg := cos(NumbSkullBrain.gamma * 0.0174533);
  sa := sin(NumbSkullBrain.alpha * 0.0174533);
  sb := sin(NumbSkullBrain.beta * 0.0174533);
  sg := sin(NumbSkullBrain.gamma * 0.0174533);
  x := 0;
  y := 0;
  z := 0;
  for i := 1 to NumbSkullBrain.Iterations do
  begin
    j := Random;
    if j < NumbSkullBrain.p[0] then k := 0 else
      if (j > NumbSkullBrain.p[0])
        and (j < NumbSkullBrain.p[1])
        then k := 1 else
        if (j > NumbSkullBrain.p[1])
          and (j < NumbSkullBrain.p[2])
          then k := 2 else
          if j > NumbSkullBrain.p[2] then k := 3 else k := 2;
    newx := (NumbSkullBrain.a[k] * x + NumbSkullBrain.b[k]
      * y + NumbSkullBrain.c[k] * z + NumbSkullBrain.n[k]);
    newy := (NumbSkullBrain.d[k] * x + NumbSkullBrain.e[k]
      * y + NumbSkullBrain.f[k] * z + NumbSkullBrain.q[k]);
    z := (NumbSkullBrain.g[k] * x + NumbSkullBrain.h[k]
      * y + NumbSkullBrain.m[k] * z + NumbSkullBrain.r[k]);
    x := newx;
    y := newy;
    vx := x * ca + y * cb + z * cg;
    vy := x * sa + y * sb + z * sg;
    px := Round(vx * NumbSkullBrain.xscale
      + NumbSkullBrain.xoffset);
    py := Round(vy * NumbSkullBrain.yscale
      + NumbSkullBrain.yoffset);
    if (px >= -Fx) and (px < Fx) and (py >= -Fy) and (py < Fy) then
      MainForm.Image2.Canvas.Pixels[px + Fx, Fy - py] :=
        NumbSkullBrain.V1;
  end; end;

procedure SetSkyImage;
begin
  with MainForm.Image2.Canvas do begin
{	maxcolx :=(FYImageX-1);
 maxrowy := (FYImageY-1);}
{Pen.Color := FBackGroundColor;
Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,(FYImageX-1),(FYImageY-1)));
Pen.Color := RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Font.Color:= RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));}
    Textout(10, 10, 'Sky Image');
{MainForm.Show;}
  end; end;

procedure SkyImage;
{Procedure FernImage(xscale,yscale,xoffset,yoffset,Iterations:Integer;
                        FTempColor:TColor); }
var {a,b,c,d,e,f,p: array[0..3] of Extended; }
  x, y, newx, j: Extended;
  i, k, px, py, Fx, Fy: integer;
begin
{	a[0] :=0; a[1] := 0.2; a[2] := -0.15; a[3] := 0.85;
 b[0] := 0; b[1] := -0.26; b[2] := 0.28; b[3] := 0.04;
 c[0] := 0; c[1] := 0.23; c[2] :=0.26; c[3] := -0.04;
 d[0] := 0.16; d[1] := 0.22; d[2] := 0.24; d[3] := 0.85;
 e[0] := 0; e[1] := 0; e[2] := 0; e[3] := 0;
 f[0] := 0; f[1] := 0.2; f[2] := 0.2; f[3] := 0.2;
 p[0] := 0.01; p[1] := 0.08; p[2] := 0.15; p[3] := 1.0;}
  Fx := (FYImageX div 2);
  Fy := (FYImageX div 2);
  x := 0;
  y := 0;
  for i := 1 to SkyKing.Iterations do begin
    j := Random;
    if j < SkyKing.p[0] then
      k := 0 else
      if (j > SkyKing.p[0]) and (j < SkyKing.p[1]) then
        k := 1 else
        if (j > SkyKing.p[1]) and (j < SkyKing.p[2]) then
          k := 2 else
          if j > SkyKing.p[2] then
            k := 3 else k := 2;
    newx := (SkyKing.a[k] * x +
      SkyKing.b[k] * y + SkyKing.e[k]);
    y := (SkyKing.c[k] * x +
      SkyKing.d[k] * y + SkyKing.f[k]);
    x := newx;
    px := Round(x * SkyKing.xscale + SkyKing.xoffset);
    py := Round(y * SkyKing.yscale + SkyKing.yoffset);
    if (px >= -Fx) and (px < Fx) and (py >= -Fy) and (py < Fy) then
      MainForm.Image2.Canvas.Pixels[px + Fx, Fy - py] := SkyKing.V1;
        {FTempColor;}
  end;
end;

procedure RanFillTri(Pt1, Pt2, Pt3, LtPt: TPoint; color: TColor);
var MyRect: TRect;
  ColTall, ColWide,
    Bc, Tc, Col, ColEnd, Rows, LowStart, iCol, iRow, Chance, FiTemp,
    Bottom, Top, Distance1, Distance2, Distance3: Integer;
  WipeOut, LongOut, LongyOut,
    Wipe1, Long1, Longy1,
    Wipe2, Long2, Longy2,
    Wipe3, Long3, Longy3: Boolean;
begin
  WipeOut := False; Wipe1 := False; Wipe2 := False; Wipe3 := False;
  LongOut := False; Long1 := False; Long2 := False; Long3 := False;
  LongyOut := False; Longy1 := False; Longy2 := False; Longy3 :=
    False;
  Bottom := 0; Top := 0; {Bc:=0;Tc:=0;ColTall:=0;ColWide:=0;}
  Distance1 := abs(DIYXY.X - DIYX2Y2.X);
  Distance2 := abs(DIYX2Y2.X - DIYX3Y3.X);
  Distance3 := abs(DIYXY.X - DIYX3Y3.X);
  if ((Distance1 <= Distance2) and (Distance1 <= Distance3))
    then
      begin Wipe1 := True; Top := DIYXY.Y; Bottom := DIYX2Y2.Y end else
    if ((Distance2 <= Distance1) and (Distance2 <= Distance3))
      then
        begin Wipe2 := True; Top := DIYX2Y2.Y; Bottom := DIYX3Y3.Y end
        else
      if ((Distance3 <= Distance2) and (Distance3 <= Distance1))
        then
          begin Wipe3 := True; Top := DIYXY.Y; Bottom := DIYX3Y3.Y end
          else
        WipeOut := True;
  Distance1 := abs(DIYXY.X - DIYX2Y2.X);
  Distance2 := abs(DIYX2Y2.X - DIYX3Y3.X);
  Distance3 := abs(DIYXY.X - DIYX3Y3.X);
  if ((Distance1 >= Distance2) and (Distance1 >= Distance3)) then
    Long1 := True else
    if ((Distance2 >= Distance1) and (Distance2 >= Distance3)) then
      Long2 := True else
      if ((Distance3 >= Distance2) and (Distance3 >= Distance1)) then
        Long3 := True else
        LongOut := True;
  Distance1 := abs(DIYXY.Y - DIYX2Y2.Y);
  Distance2 := abs(DIYX2Y2.Y - DIYX3Y3.Y);
  Distance3 := abs(DIYXY.Y - DIYX3Y3.Y);
  if ((Distance1 >= Distance2) and (Distance1 >= Distance3)) then
    Longy1 := True else
    if ((Distance2 >= Distance1) and (Distance2 >= Distance3)) then
      Longy2 := True else
      if ((Distance3 >= Distance2) and (Distance3 >= Distance1)) then
        Longy3 := True else
        LongyOut := True;

  if (not (LongOut or LongyOut or WipeOut)) then begin
{Set up the Rows,columns, and which end is dark}
    Rows := 0; Col := 0; LowStart := 0; ColEnd := 0;
    if Long1 then begin Rows := DIYXY.X end else
      if Long2 then begin Rows := DIYX2Y2.X end else
        if Long3 then begin Rows := DIYX3Y3.X end;
    if Longy1 then
      begin Col := DIYXY.Y; ColEnd := DIYX2Y2.Y; end else
      if Longy2 then
        begin Col := DIYX2Y2.Y; ColEnd := DIYX3Y3.Y; end else
        if Longy3 then
          begin Col := DIYXY.Y; ColEnd := DIYX3Y3.Y; end;
    if Wipe1 then begin LowStart := DIYXY.X;
    end else
      if Wipe2 then begin LowStart := DIYX2Y2.X;
      end else
        if Wipe3 then begin LowStart := DIYX3Y3.X;
        end;
{MyRect:=Rect(LowStart,Col,Rows,ColEnd);
MainForm.Image1.Canvas.ClipRect:= MyRect;}
    if (LowStart > Rows) then
      begin FiTemp := LowStart; LowStart := Rows; Rows := FiTemp; end;
    if (Col > ColEnd) then
      begin FiTemp := Col; Col := ColEnd; ColEnd := FiTemp; end;
    if (Bottom > Top) then
      begin FiTemp := Bottom; Bottom := Top; Top := FiTemp; end;
    ColWide := Top - Bottom; BC := Bottom;
    ColTall := Rows - LowStart; TC := Top;
    Randomize; Chance := (Rows);
    for iRow := LowStart to Rows do begin
      for iCol := Col to ColEnd do begin
        if ((Random(iRow) < Chance)
          and (Random(iCol) < (iCol div 4))
          and (Random(iCol) > BC)
          and (Random(iCol) < TC)) then
          MainForm.Image2.Canvas.Pixels[iRow, iCol] := color;
      end;
      Chance := (Rows); Dec(Chance, iRow);
      Inc(BC, (ColWide div ColTall));
      Dec(TC, (ColWide div ColTall));
{    Dec(Chance,((Rows div iRow )*(Rows div iRow )));}
    end;
{ShowMessage(
'row: '+I2S(LowStart)+' , '+I2S(Rows)
+' Col: '+I2S(Col)+' , '+I2S(ColEnd));}
  end else DoMessages(30205);
end;



end.
