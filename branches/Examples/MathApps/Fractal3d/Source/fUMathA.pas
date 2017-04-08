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
unit fUMathA;

interface

uses
  Windows, Messages, SysUtils, Dialogs, Forms,
  Math, Graphics, Classes;

procedure Lorenz;
procedure Strange; {Vortex}
procedure Duffing;
procedure Rossler;
procedure Kaneko1
  (KanA {01.3}, KanD {0.1}, KanStep {0.01}: Extended;
  DisKan {30}, KanTeen {3000}: Integer);
procedure Kaneko2
  (KanA {0.3}, KanD {1.75}, KanStep {0.02}: Extended;
  DisKan {36}, KanTeen {3000}: Integer);
procedure Henon2(Target, KamStep {1.2;}: Extended; Incoming {26}:
  Integer);
{Procedure Henon2;}{Limits 333 Henons Henon Range 1 Henon  >>>>}
procedure Henon_Attractor;
procedure LimitCycles(Which: Integer);
{Procedure Rayleigh;Procedure VanderPol;Procedure Brusselator;}
procedure SetChemical;
procedure StrangeChemicalsa;
procedure StrangeChemicalsb;
procedure StrangeChemicalsc;
procedure StrangeChemicalsd;
procedure StrangeChemicalse;
procedure StrangeChemicalsf;
procedure StrangeChemicalsg;

implementation
uses fUGlobal,fMain, fGMath {from AVGAFRAK B_Strang};


procedure Lorenz;
const rad_per_degree = 0.0174533;
  third = 0.333333333;
var
  elapse, x, y, z, d0_x, d0_y, d0_z, d1_x, d1_y, d1_z, d2_x, d2_y,
    d2_z,
    d3_x, d3_y, d3_z, xt, yt, zt, dt, dt2, x_angle, y_angle, z_angle,
    sx, sy, sz, cx, cy, cz, temp_x, temp_y, old_y: Extended;
  SCol: string;
  ICol, HalfX, HalfY, maxcolx, maxrowy,
    dummy, i, j, row, Scolor, col, old_row, old_col: integer;
  ch: char; TempColor: TColor;
  Q: array[0..2305] of Extended;
  Bitmap: TBitmap;
  PixelLine: PByteArray;
  function radians_to_degrees(degrees: Extended): Extended;
  begin
    while degrees >= 360 do
      degrees := degrees - 360;
    while degrees < 0 do
      degrees := degrees + 360;
    radians_to_degrees := rad_per_degree * degrees;
  end;
begin
{MainForm.Show;}
  FractalFilename := 'A_LORENZ000.BMP';
  maxcolx := (FYImageX - 1);
  maxrowy := (FYImageY - 1);
  HalfX := (FYImageX div 2);
  HalfY := (FYImageY div 2);
  col := 0;
  ICol := 0;
  row := 0;
  old_col := 0;
  old_row := 0;
  Scolor := 1;
  x_angle := 45;
  y_angle := 0;
  z_angle := 90;
  x_angle := radians_to_degrees(x_angle);
  sx := sin(x_angle);
  cx := cos(x_angle);
  y_angle := radians_to_degrees(y_angle);
  sy := sin(y_angle);
  cy := cos(y_angle);
  z_angle := radians_to_degrees(z_angle);
  sz := sin(z_angle);
  cz := cos(z_angle);
  for j := 0 to 2 do begin
    MainForm.DoImageStart;
    with MainForm.Image2.Canvas do begin
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
{ (Left, Top, Right, Bottom: Integer);}
{FillRect(Rect(0,0,FYImageX,FYImageY));}
{TempColor:=RGB(Colors[0,1 ],
                Colors[1,1 ],
                Colors[2,1 ]);}
      TempColor := RGB(255 - GetRValue(FBackGroundColor),
        255 - GetGValue(FBackGroundColor),
        255 - GetBValue(FBackGroundColor));
      Pen.Color := TempColor;
      Font.Color := TempColor;
      TextOut(10, 10, 'Lorenz');
      if j = 0 then TextOut(10, 30, 'Z  Y  View')
      else if j = 1 then TextOut(10, 30, 'X  Y  View')
      else TextOut(10, 30, 'Z X Y  View');
      x := 0;
      y := 1;
      z := 0;
      if j = 0 then begin
        old_col := Round(y * 9 + 460);
        old_row := Round(480 - 6.56 * z);
        Moveto(0, 478); Lineto(639, 478);
   {Line(0,478,639,478);}
        Moveto(320, 2); Lineto(320, 478);
         {Line(320,2,320,478); }
        TextOut(628, 460, 'Y');
        TextOut(330, 12, 'Z');
      end;
      if j = 1 then
      begin
        old_col := Round(y * 10 + 320);
        old_row := Round(240 - 7.29 * x);
        Moveto(0, 240); Lineto(639, 240);
   {Line(0,240,639,240);}
        Moveto(320, 2); Lineto(320, 478);
   {Line(320,2,320,478);}
        TextOut(628, 225, 'Y');
        TextOut(330, 12, 'X');
      end;
      if j = 2 then
      begin
        old_col := Round(y * 9);
        old_row := Round(480 - 6.56 * z);
        Moveto(0, 478); Lineto(638, 478);
   {Line(0,478,638,478);}
        Moveto(320, 2); Lineto(320, 478);
   {Line(320,2,320,478);}
        Moveto(320, 478); Lineto(648, 170);
   {Line(320,478,648,170);}
        TextOut(628, 460, 'Y');
        TextOut(330, 12, 'Z');
        TextOut(628, 162, 'X');
      end;
        { SetLineStyle(0,$FFFF,1);}
      Moveto(old_col, old_row);
      dt := 0.01;
      dt2 := dt / 2;
      for i := 0 to 8000 do
      begin
        d0_x := 10 * (y - x) * dt2;
        d0_y := (-x * z + 28 * x - y) * dt2;
        d0_z := (x * y - 8 * z / 3) * dt2;
        xt := x + d0_x;
        yt := y + d0_y;
        zt := z + d0_z;
        d1_x := (10 * (yt - xt)) * dt2;
        d1_y := (-xt * zt + 28 * xt - yt) * dt2;
        d1_z := (xt * yt - 8 * zt / 3) * dt2;
        xt := x + d1_x;
        yt := y + d1_y;
        zt := z + d1_z;
        d2_x := (10 * (yt - xt)) * dt;
        d2_y := (-xt * zt + 28 * xt - yt) * dt;
        d2_z := (xt * yt - 8 * zt / 3) * dt;
        xt := x + d2_x;
        yt := y + d2_y;
        zt := z + d2_z;
        d3_x := (10 * (yt - xt)) * dt2;
        d3_y := (-xt * zt + 28 * xt - yt) * dt2;
        d3_z := (xt * yt - 8 * zt / 3) * dt2;
        old_y := y;
        x := x + (d0_x + d1_x + d1_x + d2_x + d3_x) * third;
        y := y + (d0_y + d1_y + d1_y + d2_y + d3_y) * third;
        z := z + (d0_z + d1_z + d1_z + d2_z + d3_z) * third;
        if j = 0 then
        begin
          col := Round(y * 15 + HalfY); {y* 9 to 15}
          row := Round(FYImageX - 11 * z); {6.56 to 11}
          if col < HalfX then
            if old_col >= HalfX then
              inc(Scolor);
          if col > HalfX then
            if old_col <= HalfX then
              inc(Scolor);

        end;
        if j = 1 then
        begin
          col := Round(y * 13.0 + HalfX); {10.0 to 13.0}
          row := Round(HalfY - 10.29 * x); {7.29 to 10.29}
          if col < HalfX then
            if old_col >= HalfX then
              inc(Scolor);
          if col > HalfX then
            if old_col <= HalfX then
              inc(Scolor);

        end;
        if j = 2 then
        begin
          temp_x := x * cx + y * cy + z * cz;
          temp_y := x * sx + y * sy + z * sz;
          col := Round(temp_x * 12 + HalfX); {x*8 to x*12}
          row := Round(FYImageY - temp_y * 7.5); {y*5 to y*7.5}
          if col < HalfX then
            if old_col >= HalfX then
              inc(Scolor);
          if col > HalfX then
            if old_col <= HalfX then
              inc(Scolor);
        end;
{Only change color when it has been changed}
        if (Scolor > ICol) then begin
          ICol := Scolor;
          TempColor := RGB(Colors[0, (Scolor div 255)],
            Colors[1, (Scolor div 255)],
            Colors[2, (Scolor div 255)]);
          Pen.Color := TempColor;
          Application.ProcessMessages;
        end;
                        { Line(old_col,old_row,col,row);}
        Moveto(old_col, old_row); Lineto(col, row);
        old_row := row;
        old_col := col;
{If Slowdown then Application.ProcessMessages; }
      end; {of 8000}
      if (J < 2) then begin
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages
           until (bRotateImage = True);
        Mainform.DoImageDone;
      end;
    end;
  end;
end; { of UNIT SLorenz.PAS }



procedure Strange;
var
  Xmax, Xmin, Ymax, Ymin, X, Y, Z, deltaX, deltaY,
    Xtemp, Ytemp, {Ztemp,} a, b, c, d, e: Extended;
   {SCol:String;} Scolor: TColor;
{   Finder,} max_col, col, max_row, row, j: integer;
  max_iterations, i: longint;
begin
  MainForm.Show;
  FractalFilename := 'A_VORTEX000.BMP';
     {   const
     max_col = 480;  639;
     max_row = 360;   479;}
  max_col := (FYImageX - 1);
  max_row := (FYImageY - 1);
  max_iterations := 50000;
  Xmax := 2.8;
  Xmin := -2.8;
  Ymax := 2.0;
  Ymin := -2.0;
  a := 2.24;
  b := 0.43;
  c := -0.65;
  d := -2.43;
  e := 1.0;
  X := 0;
  Y := 0;
  Z := 0;
  deltaX := max_col / (Xmax - Xmin);
  deltaY := max_row / (Ymax - Ymin);
  for j := 0 to 1 do begin
    MainForm.DoImageStart;
    with MainForm.Image2.Canvas do begin
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
{ (Left, Top, Right, Bottom: Integer);}
      FillRect(Rect(0, 0, FYImageX, FYImageY));
      Pen.Color := clRed;
      TextOut(10, 10, 'Vortex');
      if j = 0 then TextOut(10, 30, 'Vertical')
      else TextOut(10, 30, 'Horizontal');
      Application.ProcessMessages;
      for i := 0 to max_iterations do
      begin
{If Slowdown then Application.ProcessMessages; }
        Xtemp := sin(a * Y) - Z * cos(b * X);
        Ytemp := Z * sin(c * X) - cos(d * Y);
        Z := e * sin(X);
        X := Xtemp;
        Y := Ytemp;
        if j = 0 then
        begin
          col := Round((X - Xmin) * deltaX);
          row := Round((Y - Ymin) * deltaY);
        end
        else
        begin
          col := Round((Y - Xmin) * deltaX);
          row := Round((Z - Ymin) * deltaY);
        end;
        if col > 0 then
          if col <= max_col then
            if row > 0 then
              if row <= max_row then
              begin
                Scolor := Pixels[col, row];
                if Scolor >= 0 then begin
{If (SColor= FBackGroundColor)and (clBlack <> FBackGroundColor) then
         Scolor := clBlack else Scolor := clBlue;}
                  if (SColor = clBlack) then Scolor := clBlue else
                    if (SColor = clBlue) then
                      Scolor := clFuchsia else
                      if (SColor = clFuchsia) then
                        Scolor := clGreen else
                        if (SColor = clGreen) then
                          Scolor := clMaroon else
                          if (SColor = clMaroon) then
                            Scolor := clOlive else
                            if (SColor = clOlive) then
                              Scolor := clNavy else
                              if (SColor = clNavy) then
                                Scolor := clPurple else
                                if (SColor = clPurple) then
                                  Scolor := clRed else
                                  if (SColor = clRed) then
                                    Scolor := clLime else
                                    if (SColor = clLime) then
                                      Scolor := clAqua else
                                      if (SColor = clAqua) then
                                        Scolor := clYellow else
                                        Scolor := clWhite;
                end;
                Pixels[col, row] := Scolor;
              end;
      end;
      if j = 0 then begin
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages
          until (bRotateImage = True);
        Mainform.DoImageDone;
      end;
    end;
  end;
end; { of procedure Strange }

(*************************************************************)

(*****************************************************************)

procedure Duffing;
var
  s: string[10];
  A, B, C, F,
    D1, D2, D3,
    D, MinU, MaxU, U2, V2, X, Y, Z, U, V: Extended;
  IKan, NPts, XSize, YSize, Limit, i, j, Steps: integer;
  XPos, YPos: integer;
  XPosT, YPosT: Longint;
  Xmin, YMin, XStep, YStep, XMax, YMax: Extended;
  TT, T, Q: integer;
  DL1A, DL1B, DL1, Dl2, Dl3, Dl, Dr,
    Step, Ua1, Ua2, Va1, Va2, YTL, YTM, YTR, TM, TL, TR, BL, BR:
      Extended;
  TempColor: TColor;
begin
  FractalFilename := 'A_DUFFNG000.BMP';
  XSize := FYImageX - 1; { physical screen size }
  YSize := FYImageY - 1; { physical screen size }
  with MainForm.Image2.Canvas do begin
    Brush.Color := FBackGroundColor;
    Brush.Style := bsSolid;
    FillRect(Rect(0, 0, FYImageX, FYImageY));
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color := TempColor;
    TextOut(10, 10, 'Duffing' {s});
    MainForm.Show;
    XMax := 4.0; {3.56}
    XMin := -8.0; {-3.70}
    YMax := 6.0; {6.76   3.61}
    YMin := -10.0; {-5.14  -3.02}
    XStep := (XMax - XMin) / XSize;
    YStep := (YMax - YMin) / YSize;
    Step := 0.1 / 25;
    X := -1.0;
    Y := 1.0;
    A := 1.0; {-0.50; }
    B := 0.3; {0.2;}
    C := 0.0; {1.0;}
    F := 10.0; {1.60;}
    Z := 1.0;
    MinU := -1E+5;
    MaxU := 1E+5;
    for T := 0 to 255 do begin
      TempColor := RGB(Colors[0, T],
        Colors[1, T],
        Colors[2, T]);
      for TT := 1 to 250 do
      begin
        D1 := Y;
        DL1A := (A * X);
        DL1B := (X * X);
{If DL1A  < MinU then DL1A := MinU else
If DL1A > MaxU then DL1A := MaxU;
If DL1B  < MinU then DL1B := MinU else
If DL1B > MaxU then DL1B := MaxU;}
        DL1 := DL1A * DL1B;
        DL2 := (C * X);
        DL3 := (B * Y);
        DL := -(DL1 + DL2 + DL3);
{If DL  < MinU then DL := MinU else
If DL > MaxU then DL := MaxU;}
        DR := F * COS(Z);
        D2 := DL + DR;
        D3 := 1;
        X := ((X + D1 * Step));
        Y := ((Y + D2 * Step));
        Z := ((Z + D3 * Step)); {320  240  }
        XPosT := (XSize div 2) + round((x / XStep));
        YPosT := (YSize div 2) - round((y / YStep));
{     If XPosT > 639 then XposT := 639;
     If XPosT < 1 then XposT := 1;
     If YPosT > 479 then YposT := 479;
     If YPosT < 1 then YposT := 1;
     XPos := XPosT;
     YPos := YPosT;}
{     PutPixel( XPosT, YPosT, Round(Z) );
TempColor :=(T Div 500) +10 ;}
        Pixels[XPosT, YPosT] := TempColor;
      end; { of For T     NEXT T}
    end; { of For TT     NEXT TT}
  end; end; { of Duffing }
(*************************************************************)


(*************************************************************)

procedure Rossler;
var
{s : string[10];}
{MinU, MaxU, U2,U, V2,} X, Y, Z: Extended;
  XSize, YSize: integer; { XPos, YPos : integer;}
  XPosT, YPosT: Longint;
{ZMax,ZMin,  } Xmin, YMin, XStep, YStep, XMax, YMax: Extended;
  TT, {Colored,,Q} T: integer;
{Ua1, Ua2, Va1, Va2, YTL, YTM, YTR,
TM, TL,TR,BL,BR : Extended;}
  Step, A, B, C, D1, D2, D3, D, N: Extended;
  TempColor: TColor;
begin
  FractalFilename := 'A_ROSSLR000.BMP';
  XSize := FYImageX - 1; { physical screen size }
  YSize := FYImageY - 1; { physical screen size }
  with MainForm.Image2.Canvas do begin
    Brush.Color := FBackGroundColor;
    Brush.Style := bsSolid;
    FillRect(Rect(0, 0, FYImageX, FYImageY));
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color := TempColor;
    TextOut(10, 10, 'Rossler');
    MainForm.Show;
    MainForm.HiddenFX.Caption := 'FX';
    XMax := 10.0; {11.53}
    XMin := -14.0; {-7.07}
    YMax := 10.0; {7.85}
    YMin := -22.0; {-9}
  {ZMax := 24;   }{23.95}
 {ZMin := 3.0;   }{2.41}
    XStep := (XMax - XMin) / XSize;
    YStep := (YMax - YMin) / YSize;
    A := 0.2;
    B := 0.2;
    C := 5.7;
    X := 1;
    Y := 1;
    Z := 1;
{T1 = Start, T2 = End time;   D = Delta T (increment time amount }
{ N = number of silent computations }
    D := 0.04;
    N := 10.0;
{D := D + D;
Step := Step + Step;}
    Step := D / N;
    for T := 0 to 2000 do begin
      TempColor := RGB(Colors[0, (T div 255)],
        Colors[1, (T div 255)],
        Colors[2, (T div 255)]);
      for TT := 1 to 25 do
      begin
        D1 := -Y - Z;
        D2 := X + (Y * A);
        D3 := B + (Z * (X - C));
        X := X + D1 * Step;
        Y := Y + D2 * Step;
        Z := Z + D3 * Step;
{     If X > 01.210 then X := 01.210;
     If X < -0.750 then X := -0.750;
     If Y > 01.210 then Y := 01.210;
     If Y < -0.750 then Y := -0.750;}
      end; { of For T     NEXT T} {320  240  }
      XPosT := (XSize div 2) + round((x / XStep));
      YPosT := (YSize div 2) - round((y / YStep));
{     If XPosT > 639 then XposT := 639;
     If XPosT < 1 then XposT := 1;
     If YPosT > 479 then YposT := 479;
     If YPosT < 1 then YposT := 1;
     XPos := XPosT;
     YPos := YPosT;}
{     PutPixel( XPosT, YPosT, Round(Z)+12 );}
{     Pixels[ XPosT, YPosT]:= (T div 500)+10 ;}
      Pixels[XPosT, YPosT] := TempColor;
    end; { of For TT     NEXT TT}
  end; end; { of Rossler }
(*************************************************************)

(*************************************************************)
(*****************************************************************)

procedure Kaneko1
  (KanA {01.3}, KanD {0.1}, KanStep {0.01}: Extended;
  DisKan {30}, KanTeen {3000}: Integer);
var TempColor: TColor;
  s: string[10];
  A, D, MinU, MaxU, {U2, V2, U,  V, } X, Y: Extended;
  IKan, XSize, YSize, HalfY, HalfX, Steps: integer;
  T, Q: integer; XPos, YPos: integer;
  XPosT, YPosT: Longint;
{Xmin, YMin,, XMax, YMax} XStep, YStep: Extended;
  Ua1, Ua2, Va1, Va2, YTL, YTM, {YTR, } TM, TL {TR,,BR}: Extended;
(*************************************************************)
begin {  MAIN PROGRAM BLOCK  }
  MainForm.Show;
  FractalFilename := 'A_KANEKO000.BMP';
  MainForm.HiddenFX.Caption := 'Click image to do next view';
  HalfX := (FYImageX div 2);
  HalfY := (FYImageY div 2);
  XSize := FYImageX - 1; { physical screen size }
  YSize := FYImageY - 1; { physical screen size }
  XStep := { ( XMax - XMin )} 2.2 / XSize;
  YStep := {( YMax - YMin )} 2.2 / YSize;
  A := KanA {01.3};
  D := KanD {0.1}; {1.3 .. 1.55}
  for IKan := 1 to DisKan {30} do { range is ... 1.2 to 1.7 }
  begin {Cleardevice;}
    MainForm.DoImageStart;
    with MainForm.Image2.Canvas do begin
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));}
      TempColor := RGB(255 - GetRValue(FBackGroundColor),
        255 - GetGValue(FBackGroundColor),
        255 - GetBValue(FBackGroundColor));
      Pen.Color := TempColor;
      Font.Color := TempColor;
      Str(A: 8: 4, s);
      TextOut(10, 10, s);
      begin
{q:= 1;}
        Ua1 := 0.1;
        Va1 := 0.15;
{MinU := -1E+20;MaxU := 1E+20;}
        MinU := -1E+7;
        MaxU := 1E+7;
        for T := 1 to KanTeen {3000} do
        begin
          TL := (1 - A * Ua1 * Ua1);
          TM := (D * (Va1 - Ua1));
          if TL < MinU then TL := MinU;
          if TL > MaxU then TL := MaxU;
          Ua2 := (TL + TM); { cycle }
          YTL := (1 - A * Va1 * Va1);
          YTM := (D * (Ua1 - Va1));
          if YTL < MinU then YTL := MinU;
          if YTL > MaxU then YTL := MaxU;
          Va2 := (YTL + YTM); {cycle }
{Q := Q + 1;}
          X := Ua1;
          Y := Va1;
          Ua1 := Ua2;
          Va1 := Va2;
          if X > 01.210 then X := 01.210;
          if X < -0.750 then X := -0.750;
          if Y > 01.210 then Y := 01.210;
          if Y < -0.750 then Y := -0.750;
                {320  240  }
          XPosT := HalfY + round((x / XStep));
          YPosT := HalfX - round((y / YStep));
{     Steps := ((Q DIV 300)+2);}
          if XPosT > XSize then XposT := XSize;
          if XPosT < 1 then XposT := 1;
          if YPosT > YSize then YposT := YSize;
          if YPosT < 1 then YposT := 1;
          XPos := XPosT;
          YPos := YPosT;
{     Pixels[ XPos, YPos]:= Steps;}
{TempColor:=Colors[0,(T mod 255)]; }
          TempColor := RGB(Colors[0, (T mod 255)],
            Colors[1, (T mod 255)],
            Colors[2, (T mod 255)]);
          Pixels[XPos, YPos] := TempColor;
        end; { of For T     NEXT T}
      end; { of procedure Computit }
      A := A + KanStep {0.01}; {1/(I+3); } {0.01;}
{A := A + 1/(IKan + 1); }
    end;
    if IKan < 30 then begin
      bRotateImage := False; {in the Drawing...}
      bRotatingImage := True;
      repeat Application.ProcessMessages until
        (bRotateImage = True);
      Mainform.DoImageDone;
    end;
  end;
  MainForm.HiddenFX.Caption := 'FX';
end; { of Kaneko1 }
(*************************************************************)
(*************************************************************)



(*************************************************************)
(*************************************************************)

procedure Kaneko2(KanA {0.3}, KanD {1.75}, KanStep {0.02}: Extended;
  DisKan {36}, KanTeen {3000}: Integer);
var
  s: string[8];
  HalfX, HalfY, IK2, XSize, YSize {Limit,} { i,} {j,}: integer;
  D, MinU, MaxU, {U2,} {V2,} X, Y, { U, V,} A: Extended;
{, XMax, YMax Xmin, YMin,} XStep, YStep: Extended;
  TempColor: TColor;
  T, {Q, } XPos, YPos: integer;
  XPosT, YPosT: Longint;
  TM, TL, TR, Ua1, Ua2, Va1, Va2: Extended;
(*************************************************************)
begin {  MAIN PROGRAM BLOCK  }
{MainForm.Show;}
  FractalFilename := 'A_KANEK2000.BMP';
  MainForm.HiddenFX.Caption := 'Click image to do next view';
  XSize := FYImageX - 1; { physical screen size }
  YSize := FYImageY - 1;
  HalfX := (FYImageX div 2);
  HalfY := (FYImageY div 2); { physical screen size }
  XStep := { ( XMax - XMin )} 1.60 / XSize;
  YStep := {( YMax - YMin )} 1.60 / YSize;

  A := KanA;
  D := KanD; {1.75 .. 2.16}
  for IK2 := 1 to DisKan do { range is ... 1.2 to 1.7 }
  begin {Cleardevice;}
    MainForm.DoImageStart;
    with MainForm.Image2.Canvas do begin
      Brush.Color := FBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, FYImageX, FYImageY));
      TempColor := RGB(255 - GetRValue(FBackGroundColor),
        255 - GetGValue(FBackGroundColor),
        255 - GetBValue(FBackGroundColor));
      Pen.Color := TempColor;
      Font.Color := TempColor;
      Str(D: 6: 3, s);
      TextOut(10, 10, s);
      begin
        Ua1 := 0.1;
        Va1 := 0.15;
        MinU := -1E+5;
        MaxU := 1E+5;

        for T := 1 to KanTeen do begin
          if Va1 < MinU then Va1 := MinU else
            if Va1 > MaxU then Va1 := MaxU;
          TL := A * Ua1 + (1 - A);
          TM := Va1 * Va1;
          TR := (1 - D * TM);
          if TR < MinU then TR := MinU;
          if TR > MaxU then TR := MaxU;
          Ua2 := TL * TR;
          Va2 := Ua1;
          X := Ua1;
          Y := Va1;
          Va1 := Va2;
          Ua1 := Ua2;
          if X > 01.350 then X := 01.350;
          if X < -0.750 then X := -0.750;
          if Y > 01.350 then Y := 01.350;
          if Y < -0.750 then Y := -0.750;
                {320  240  }
          XPosT := HalfY + round(x / XStep);
          YPosT := HalfX - round(y / YStep);
{     Steps := ((T DIV 300)+2);}
          if XPosT > XSize then XposT := XSize;
          if XPosT < 1 then XposT := 1;
          if YPosT > YSize then YposT := YSize;
          if YPosT < 1 then YposT := 1;
          XPos := XPosT;
          YPos := YPosT;
{     Pixels[ XPos, YPos]:= Steps;}
          TempColor := RGB(Colors[0, (T mod 255)],
            Colors[1, (T mod 255)],
            Colors[2, (T mod 255)]);
          Pixels[XPos, YPos] := TempColor;
        end; { of For T     NEXT T}
      end; { of procedure }
      D := D + KanStep; {1/(I+3); } {0.01;} {A := A + 1/(IK2+1); }
    end;
    if IK2 < 36 then begin
      bRotateImage := False; {in the Drawing...}
      bRotatingImage := True;
      repeat Application.ProcessMessages
         until (bRotateImage = True);
      Mainform.DoImageDone;
    end;
  end;
  MainForm.HiddenFX.Caption := 'FX';
end; { of Procedure Kaneko2 }
(*************************************************************)
(*************************************************************)

(*************************************************************)

procedure Henon2(Target, KamStep {1.2;}: Extended; Incoming {26}:
  Integer);
type
  A4000R = array[1..2002] of Extended;
  A251R = array[1..251] of Extended;
var TempColor: TColor;
  Henons, s: string;
{ Xa, Ya : A4000R;}
{  deleted as an array is not needed -> putpixel a dot as calculated}
  Ua, Va: A251R;
  MinU, MaxU, U2, V2, X, Y, U, V, A: Extended;
  NPts, HalfX, HalfY: Integer;
  IC, XSize, YSize, Limit, I, I2, j, Steps: integer;
  XPos, YPos: integer;
  XPosT, YPosT: Longint;
  Xmin, YMin, XStep, YStep, XOrg, YOrg,
    XMax, YMax, XIter, YIter: Extended;
  Done: boolean;
  T, Q: integer;
  TL, TR, BL, BR,
    UV_Temp, U_Temp, S_Temp, C_Temp, Orbit: Extended;
begin {  MAIN PROcedure BLOCK  }
{MainForm.Show;}
  FractalFilename := 'A_HENON_000.BMP';
  MainForm.HiddenFX.Caption := 'Click image to do next view';
  Henons := 'Henon 2';
  XSize := FYImageX - 1; { physical screen size }
  YSize := FYImageY - 1; { physical screen size }
  HalfX := (FYImageX div 2);
  HalfY := (FYImageY div 2);
{   XOrg := -2.0;    XMax := 0.5;
   YOrg := -1.25;   YMax := 1.25;}
  XMin := -2.10;
  XMax := 1.10;
  YMin := -1.20;
  YMax := 1.20;
  XStep := (XMax - XMin) / XSize;
  YStep := (YMax - YMin) / YSize;
  A := Target; {1.2;} { range is ... 1.2 to 1.7 }
  for I := 1 to Incoming {26} do begin begin
      MainForm.DoImageStart;
      q := 1;
      Orbit := 0.1;
      MaxU := 1E+5;
      MinU := -1E+5;
      with MainForm.Image2.Canvas do begin
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));}
        TempColor := RGB(255 - GetRValue(FBackGroundColor),
          255 - GetGValue(FBackGroundColor),
          255 - GetBValue(FBackGroundColor));
        Pen.Color := TempColor;
        Font.Color := TempColor;
{TextOut(10,10,Henons);}
        Str(A: 7: 5, s);
        TextOut(10, 10, 'Henon: ' + s);
{Str(Orbit:7:5,s);
TextOut(10,30,'Orbit: '+s);}
        A := A + KamStep; { 0.02;} {A := A + 1/(I+1); }
          { other way of incrementing input }
 {FOR Orbit = .1 TO 1.6 STEP .1}
        for I2 := 1 to 24 {16} do begin
          Ua[1] := Orbit / 3.0;
          Va[1] := Ua[1];
          Orbit := Orbit + 0.1; { TO 1.6 STEP .1}
          TempColor := RGB(Colors[0, (I2 mod 255)],
            Colors[1, (I2 mod 255)],
            Colors[2, (I2 mod 255)]);
          for T := 1 to 250 do begin
            if Ua[T] < MinU then Ua[T] := MinU;
            if Ua[T] > MaxU then Ua[T] := MaxU;
            UV_Temp := Ua[T] * Ua[T];
            U_Temp := (Va[t] - UV_Temp);
            S_Temp := SIN(A);
            C_Temp := COS(A);
            TL := Ua[t] * C_Temp;
            TR := U_Temp * S_Temp;
            BL := Ua[t] * S_Temp;
            BR := U_Temp * C_Temp;
            Ua[T + 1] := TL - TR;
            Va[T + 1] := BL + BR;
{Q := Q + 1;}
            X := Ua[t];
            Y := Va[t];
            if X > 2.0 then X := 2.0;
            if X < -2.0 then X := -2.0;
            if Y > 2.0 then Y := 2.0;
            if Y < -2.0 then Y := -2.0;
            XPosT := HalfX + round(x / XStep);
            YPosT := HalfY - round(y / YStep);
{     Steps := Round(Q/250);}
            if XPosT > XSize then XposT := XSize;
            if XPosT < 1 then XposT := 1;
            if YPosT > YSize then YposT := YSize;
            if YPosT < 1 then YposT := 1;
            XPos := XPosT;
            YPos := YPosT;
     {If I = 16 then IC := 11
     Else IC := I;
        IC := I;}
            Pixels[XPos, YPos] := TempColor; {IC Steps ;}
          end; { of For T     NEXT T}
        end; end; { of FOR I    NEXT Orbit}

      {?? how  If ((I<Incoming)and (I2 < 36) then} begin
        bRotateImage := False; {in the Drawing...}
        bRotatingImage := True;
        repeat Application.ProcessMessages
          until (bRotateImage =  True);
        Mainform.DoImageDone;
      end;
    end;
  end; { of procedure }
  MainForm.HiddenFX.Caption := 'HiddenFX';
  Application.ProcessMessages;
end; { Procedure HENON2 }
(*************************************************************)
   {=======================}
   {       HENON.PAS       }
   {=======================}

procedure Henon_Attractor;
var
   {MaxColors, } MaxX, MaxY, XScale, YScale, XOff, YOff: integer;
  i, Color, XPos, YPos: integer; TempColor: TColor;
 {   Xmax, Ymax,} Xold, Xnew, Yold, Ynew, Xmin, Ymin: Extended;
begin
      {         Henon Attractor        }
      {  x := y + 1 - ( 1.4 * x * x )  }
      {  y := 0.3 * x                  }
  XScale := 1; { values to adjust scale }
  YScale := 1;
  XOff := 0; { and screen position    }
  YOff := 0;
  Xmin := 0; Ymin := 0; Xold := 0; Yold := 0;
{   Xmax := 0;Ymax := 0; Xnew := 0; Ynew := 0;}
  with MainForm.Image2.Canvas do begin
    MaxX := (FYImageX - 1);
    MaxY := (FYImageY - 1);
{   Brush.Color:=FBackGroundColor;
   Brush.Style:=bsSolid;
   FillRect(Rect(0,0,FYImageX,FYImageY));}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color := TempColor;
    TextOut(10, 10, 'Henon Attractor');
    TextOut(10, 30, 'XScale = ' + I2S(XScale));
    TextOut(10, 50, 'YScale = ' + I2S(YScale));
    TextOut(10, 70, 'XOff = ' + I2S(XOff));
    TextOut(10, 90, 'YOff = ' + I2S(YOff));
{   MainForm.Show;    }
    MainForm.HiddenFX.Caption := 'HiddenFX Henon Attractor';
    Application.ProcessMessages;
    for Color := 0 to 15 do begin
      MainForm.HiddenFX.Caption := 'HiddenFX Henon Attractor';
      Application.ProcessMessages;
      TempColor := RGB(RGBArray[0, (Color)],
        RGBArray[1, (Color)],
        RGBArray[2, (Color)]);
      for i := 1 to 1000 do { $7FFF }  begin
        Xnew := Yold + 1 - (1.4 * Xold * Xold);
        Ynew := 0.3 * Xold;
        XPos := trunc((XNew * MaxX / 3 * XScale) + MaxX / 2 + XOff);
        YPos := trunc((YNew * MaxY * YScale) + MaxY / 2 + YOff);
        if (XPos > Xmin) and (XPos < MaxX) and
          (YPos > Ymin) and (YPos < MaxY) then
        begin Pixels[XPos, YPos] := TempColor; end;
        Yold := Ynew;
        Xold := Xnew;
      end;
    end;
  end;
  MainForm.HiddenFX.Caption := 'HiddenFX';
  Application.ProcessMessages;
end; { of procedure Henon_Attractor; }


(*************************************************************)

(*****************************************************************)
{Rayleigh;VanderPol;Brusselator;}

procedure LimitCycles(Which: Integer);
var TempColor: TColor;
  TT, T, XSize, YSize { XPos, YPos,}
{NPts,Limit, i, j, Steps}: integer;
  XPosT, YPosT: Longint;
{Xmin, YMin,XMax, YMax,}
  F, D1, D2, D3, X, Y, Z, {TL,TR,BL,BR,} XStep, YStep,
    DL1A, DL1B, DL1, Dl2, Dl3, Dl, Dr, A, B, Step: Extended;
{Ua1, Ua2, Va1, Va2, YTL, YTM, YTR, TM,}
 {C, D, MinU, MaxU, U2, V2, U, V}
begin
  FractalFilename := 'A_LIMIT_000.BMP';
  XSize := (FYImageX div 2); { physical screen size }
  YSize := (FYImageY div 2); { physical screen size }
  with MainForm.Image2.Canvas do begin
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color := TempColor;
{MainForm.Show;}
{XMax := 2.67;
XMin := -5.33;
YMax := 4.0;
YMin := -6.67;}
    if ((Which = 0) or (Which = 3)) then begin
      TextOut(10, 50, 'Rayleigh' {s});
      XStep := (12.8 { XMax - XMin}) / XSize;
      YStep := (9.6 {YMax - YMin}) / YSize;
      Step := 0.1 / 25;
      X := 0; {-1.0;}
      Y := 0; {1.0;}
      F := 1.0; {1.60;}
      Z := 1.0; {MinU := -1E+5;MaxU := 1E+5;}
      for T := 1 to 255 do begin
        MainForm.HiddenFX.Caption := 'HiddenFX Rayleigh';
        Application.ProcessMessages;
        TempColor := RGB(Colors[0, T],
          Colors[1, T],
          Colors[2, T]);
        for TT := 1 to 250 do
        begin
          D1 := Y;
          DL1A := (Y * Y * Y);
          DL1B := (Y - DL1A / 3);
{If DL1A  < MinU then DL1A := MinU else
If DL1A > MaxU then DL1A := MaxU;
If DL1B  < MinU then DL1B := MinU else
If DL1B > MaxU then DL1B := MaxU;}
          DL := DL1B - X;
{If DL  < MinU then DL := MinU else
If DL > MaxU then DL := MaxU;}
          DR := F * COS(Z);
          D2 := DL + DR;
          D3 := 1;
          X := ((X + D1 * Step));
          Y := ((Y + D2 * Step));
          Z := ((Z + D3 * Step)); {320  240  }
          XPosT := XSize + round((x / XStep));
          YPosT := YSize - round((y / YStep));
{     If XPosT > 639 then XposT := 639;
     If XPosT < 1 then XposT := 1;
     If YPosT > 479 then YposT := 479;
     If YPosT < 1 then YposT := 1;
     XPos := XPosT;
     YPos := YPosT;}
{     PutPixel( XPosT, YPosT, Round(Z) );}
          Pixels[XPosT, YPosT] := TempColor; {( (T Div 200) + 10 );}
        end; { of For T     NEXT T}
      end; { of For TT     NEXT TT}
    end; { of Rayleigh }
(*************************************************************)
(*****************************************************************)
    if ((Which = 1) or (Which = 3)) then begin
      TextOut(10, 30, 'Van der Pol Oscillator' {s});
{XMax := 2.67;
XMin := -5.33;
YMax := 4.0;
YMin := -6.67;}
      XStep := (12.8 { XMax - XMin}) / XSize;
      YStep := (9.6 { YMax - YMin}) / YSize;
      Step := 0.1 / 25;
      X := 0;
      Y := 0;
 {A := 1.0;}{-0.50; }
      B := 1.0; {0.2;}
{C := 1.0;  }{1.0;}
      F := 0.55; {1.60;}
      Z := 1.0;
{MinU := -1E+5;MaxU := 1E+5;}
      for T := 1 to 255 do begin
        MainForm.HiddenFX.Caption :=
          'HiddenFX Van der Pol Oscillator';
        Application.ProcessMessages;
        TempColor := RGB(Colors[0, T],
          Colors[1, T],
          Colors[2, T]);
        for TT := 1 to 250 do
        begin
          D1 := Y;
          DL1A := (3 * B);
          DL1B := (X * X);
{If DL1A  < MinU then DL1A := MinU else
If DL1A > MaxU then DL1A := MaxU;
If DL1B  < MinU then DL1B := MinU else
If DL1B > MaxU then DL1B := MaxU;}
          DL1 := (DL1A * DL1B - 1);
          DL2 := (DL1 * Y);
          DL3 := (X + DL2);
          DL := (-1.0 * DL3);
{If DL  < MinU then DL := MinU else
If DL > MaxU then DL := MaxU;}
          DR := F * SIN(1);
          D2 := DL + DR;
          D3 := 1;
          X := ((X + D1 * Step));
          Y := ((Y + D2 * Step));
          Z := ((Z + D3 * Step)); {320  240  }
          XPosT := XSize + round((x / XStep));
          YPosT := YSize - round((y / YStep));
{     If XPosT > 639 then XposT := 639;
     If XPosT < 1 then XposT := 1;
     If YPosT > 479 then YposT := 479;
     If YPosT < 1 then YposT := 1;
     XPos := XPosT;
     YPos := YPosT;}
{     PutPixel( XPosT, YPosT, Round(Z) );}
          Pixels[XPosT, YPosT] := TempColor; {( (T Div 200) + 9 );}
        end; { of For T     NEXT T}
      end; { of For TT     NEXT TT}
    end; { of VanderPol }
(*************************************************************)
(*****************************************************************)
{procedure Brusselator;}
    if ((Which = 2) or (Which = 3)) then begin
      TextOut(10, 10, 'Brusselator' {s});
{XMax := 2.67;
XMin := -5.33;
YMax := 4.0;
YMin := -6.67;}
      XStep := (12.8 {6.4} { XMax - XMin}) / XSize;
      YStep := (9.6 {4.8} { YMax - YMin}) / YSize;
      Step := 0.1 / 25;
      X := 0;
      Y := 0;
      A := 1.0; {-0.50; }
      B := 3.0; {0.2;}
{MinU := -1E+5;MaxU := 1E+5;}
      for T := 1 to 255 do begin
        MainForm.HiddenFX.Caption := 'HiddenFX Brusselator';
        Application.ProcessMessages;
        TempColor := RGB(Colors[0, T],
          Colors[1, T],
          Colors[2, T]);
        for TT := 1 to 250 do
        begin
          D1 := (X * X * Y);
          DL1A := (A * (B + 1));
          DL1B := 1 - DL1A * X;
{If DL1A  < MinU then DL1A := MinU else
If DL1A > MaxU then DL1A := MaxU;
If DL1B  < MinU then DL1B := MinU else
If DL1B > MaxU then DL1B := MaxU;}
          D2 := DL1B + D1;
          D3 := 3 * X - D1;
          X := ((X + D2 * Step));
          Y := ((Y + D3 * Step)); {320  240  }
          XPosT := XSize + round((x / XStep));
          YPosT := YSize - round((y / YStep));
{     If XPosT > 639 then XposT := 639;
     If XPosT < 1 then XposT := 1;
     If YPosT > 479 then YposT := 479;
     If YPosT < 1 then YposT := 1;
     XPos := XPosT;
     YPos := YPosT;}
          Pixels[XPosT, YPosT] := TempColor; { ((T Div 200) + 8 );}
        end; { of For T     NEXT T}
      end; { of For TT     NEXT TT}
    end; end;
  MainForm.HiddenFX.Caption := 'HiddenFX';
  Application.ProcessMessages;
end; { of Brusselator }
(*************************************************************)
(*************************************************************)


(*************************************************************)

procedure SetChemical;
begin
  with MainForm.Image2.Canvas do begin
    Brush.Color := FBackGroundColor;
    Brush.Style := bsSolid;
    FillRect(Rect(0, 0, FYImageX, FYImageY));
  end; end;
(*************************************************************)

procedure StrangeChemicalsa;
{dx/dt = -k2x  - k3y(x/k+x) + k1  + k6z
dy/dt = -k2y  - k3x(y/k+x)  + k1  + b
dz/dt = k4y  - k5z}
var
  s: string[10];
{A,  C, F,} B, k, k1, k2, k3, k4, k5, k6,
    D1, D2, D3,
    D, MinU, MaxU, U2, V2, X, Y, Z, U, V: Extended;
  IKan, NPts, XSize, YSize, Limit, i, j, Steps: integer;
  XPos, YPos: integer;
  XPosT, YPosT: Longint;
  Xmin, YMin, XStep, YStep, XMax, YMax: Extended;
  TT, T, Q: integer;
  DL1A, DL1B, DL1, Dl2, Dl3, Dl, Dr,
    Step, Ua1, Ua2, Va1, Va2, YTL, YTM, YTR, TM, TL, TR, BL, BR:
      Extended;
  TempColor: TColor;
begin
  FractalFilename := 'A_ST_CA0.BMP';
  XSize := FYImageX - 1; { physical screen size }
  YSize := FYImageY - 1; { physical screen size }
  with MainForm.Image2.Canvas do begin
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY)); }
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color := TempColor;
    TextOut(10, 10, 'Chemical A' {s});
    MainForm.Show;
    MainForm.HiddenFX.Caption := 'A_ST_CA0.BMP';
    Application.ProcessMessages;
    XMax := 2.0; {3.56}
    XMin := -1.0; {-3.70}
    YMax := 2.0; {6.76   3.61}
    YMin := -1.0; {-5.14  -3.02}
    XStep := (XMax - XMin) / XSize;
    YStep := (YMax - YMin) / YSize;
    Step := 0.1 / 25;
    X := -1.0;
    Y := 1.2;
    Z := 1.0;
    B := 1.0;
    k := 1.0;
    k1 := 1.1;
    k2 := 1.2;
    k3 := 1.3;
    k4 := 1.4;
    k5 := 1.50;
    k6 := 1.70;
{dx/dt = -k2x  - k3y(x/k+x) + k1  + k6z
dy/dt = -k2y  - k3x(y/k+x)  + k1  + b
dz/dt = k4y  - k5z}
    MinU := -1E+5;
    MaxU := 1E+5;
    for T := 1 to 255 do begin
      MainForm.HiddenFX.Caption := 'A_ST_CA0.BMP';
      Application.ProcessMessages;
      TempColor := RGB(Colors[0, T],
        Colors[1, T],
        Colors[2, T]);
      for TT := 1 to 200 do
      begin
        DL1 := (-k2 * X);
        DL1A := X;
        DL1B := (X / k);
        DL2 := (DL1A + DL1B);
        DL3 := (k3 * y);
        DL := (DL1 - (DL2 * DL3));
        DR := (k1 + (k6 * Z));
        D1 := DL + DR;
{dx/dt = -k2x  - k3y(x/k+x) + k1  + k6z
dy/dt = -k2y  - k3x(y/k+x)  + k1  + b
dz/dt = k4y  - k5z}
        DL1 := (-k2 * Y);
        DL1A := X;
        DL1B := (Y / k);
        DL2 := (DL1A + DL1B);
        DL3 := (k3 * X);
        DL := (DL1 - (DL2 * DL3));
        DR := (k1 + (B));
        D2 := DL + DR;
        D3 := (k4 * Y) - (k5 * z);
        X := ((X + D1 * Step));
        Y := ((Y + D2 * Step));
        Z := ((Z + D3 * Step)); {320  240  }
        XPosT := (XSize div 2) + round((x / XStep));
        YPosT := (YSize div 2) - round((y / YStep));
        Pixels[XPosT, YPosT] := TempColor;
      end; { of For T     NEXT T}
    end; { of For TT     NEXT TT}
  end;
  MainForm.HiddenFX.Caption := 'HiddenFX';
  Application.ProcessMessages;
end; { of StrangeChemicalsA }
(*************************************************************)

procedure StrangeChemicalsb;
{dx/dt = k1  + k2'x  - ((k3y = k4z)x / (x+k))
dy/dt = k5x  - k6y
dz/dt = k7x  - k8'z / (z+k')}
var
  s: string[10];
{A,  C, F,} B, k, k1, k2, k3, k4, k5, k6, k7, k8,
    D1, D2, D3,
    D, MinU, MaxU, U2, V2, X, Y, Z, U, V: Extended;
  IKan, NPts, XSize, YSize, Limit, i, j, Steps: integer;
  XPos, YPos: integer;
  XPosT, YPosT: Longint;
  Xmin, YMin, XStep, YStep, XMax, YMax: Extended;
  TT, T, Q: integer;
  DL1A, DL1B, DL1, Dl2, Dl3, Dl, Dr,
    Step, Ua1, Ua2, Va1, Va2, YTL, YTM, YTR, TM, TL, TR, BL, BR:
      Extended;
  TempColor: TColor;
begin
  FractalFilename := 'A_ST_CB0.BMP';
  XSize := FYImageX - 1; { physical screen size }
  YSize := FYImageY - 1; { physical screen size }
  with MainForm.Image2.Canvas do begin
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color := TempColor;
    TextOut(10, 10, 'Chemical  B' {s});
    MainForm.Show;
    MainForm.HiddenFX.Caption := 'A_ST_CB0.BMP';
    Application.ProcessMessages;
    XMax := 42.0; {3.56}
    XMin := 32.0; {-3.70}
    YMax := 42.0; {6.76   3.61}
    YMin := 32.0; {-5.14  -3.02}
    XStep := (XMax - XMin) / XSize;
    YStep := (YMax - YMin) / YSize;
    Step := 0.1 / 25;
    X := 1.1;
    Y := 1.1;
    Z := 0.510;
    B := 1.10;
    k := 1.0;
    k1 := 1.1;
    k2 := 1.2;
    k3 := 1.3;
    k4 := 1.1;
    k5 := 1.15;
    k6 := 1.0;
    k7 := 1.0;
    k8 := 1.0;
{dx/dt = k1  + k2'x  - ((k3y + k4z)x / (x+k))
dy/dt = k5x  - k6y
dz/dt = k7x  - k8'z / (z+k')}
    MinU := -1E+5;
    MaxU := 1E+5;
    for T := 1 to 255 do begin
      MainForm.HiddenFX.Caption := 'A_ST_CB0.BMP';
      Application.ProcessMessages;
      TempColor := RGB(Colors[0, T],
        Colors[1, T],
        Colors[2, T]);
      for TT := 1 to 200 do
      begin
        DL1 := ((k2 * X));
        DL := (k1 + DL1);
        DL1A := (k3 * Y);
        DL1B := (k4 * Z);
        DL2 := ((DL1A + DL1B) * X);
        DL3 := (X + k);
        DR := (DL2 / DL3);
        D1 := DL - DR;
{dx/dt = k1  + k2'x  - ((k3y + k4z)x  / (x+k))
dy/dt = k5x  - k6y
dz/dt = k7x  - k8'z / (z+k')}
        DL := (k5 * X);
        DR := (k6 * Y);
        D2 := (DL - DR);
        DL1 := (k7 * X);
        DL1A := (k8 * Z);
        DL1B := (DL1 - DL1A); {Math correct?}
        DL2 := (Z + k);
        D3 := (DL1B / DL2);
        X := ((X + D1 * Step));
        Y := ((Y + D2 * Step));
        Z := ((Z + D3 * Step)); {320  240  }
        XPosT := (XSize div 2) + round((x / XStep));
        YPosT := (YSize div 2) - round((y / YStep));
        Pixels[XPosT, YPosT] := TempColor;
      end; { of For T     NEXT T}
    end; { of For TT     NEXT TT}
  end;
  MainForm.HiddenFX.Caption := 'HiddenFX';
  Application.ProcessMessages;
end; { of StrangeChemicalsB }
(*************************************************************)

(*************************************************************)

procedure StrangeChemicalsc;
{dx/dt = -y - z - w
dy/dt = x
dz/dt = c(z/2  - z^2)  -dw}
var
{s : string[10];}
{A,  F,B, k, k1,k2,k3,k4,k5,k6,k7,k8,}
  D1, D2, D3, C, w, D,
 {MinU, MaxU, U2, V2,} X, Y, Z {, U, V}: Extended;
{IKan, NPts,}
  HalfY, HalfX, XSize, YSize {, Limit, i, j, Steps}: integer;
{ XPos, YPos : integer;      }
  XPosT, YPosT: Longint;
  Xmin, YMin, XStep, YStep, XMax, YMax: Extended;
  TT, T {,Q}: integer;
  DL1A, DL1B, DL1, Dl2, {Dl3, Dl, Dr,}
    Step {Ua1, Ua2, Va1, Va2, YTL, YTM, YTR, TM, TL,TR,BL,BR}:
      Extended;
  TempColor: TColor;
begin
  FractalFilename := 'A_ST_CC0.BMP';
  XSize := FYImageX - 1; { physical screen size }
  YSize := FYImageY - 1; { physical screen size }
  HalfX := (FYImageX div 2);
  HalfY := ((FYImageY div 4) * 3);
  with MainForm.Image2.Canvas do begin
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color := TempColor;
    TextOut(10, 10, 'Chemical   C' {s});
    MainForm.Show;
    MainForm.HiddenFX.Caption := 'A_ST_CC0.BMP';
    Application.ProcessMessages;
    XMax := 3.320; {3.56}
    XMin := 2.0; {-3.70}
    YMax := 6.230; {6.76   3.61}
    YMin := 2.0; {-5.14  -3.02}
    XStep := (XMax - XMin) / XSize;
    YStep := ((YMax - YMin) * 4) / YSize;
    Step := 0.1 / 25;
    X := 0.12;
    Y := 0.00012;
    Z := 0.00211;
    C := 0.00120;
    w := 0.110;
    D := 0.21;
{dx/dt = -y - z - w
dy/dt = x
dz/dt = c(z/2  - z^2)  -dw}
{MinU := -1E+5;
MaxU := 1E+5;}
    for T := 1 to 255 do begin
      MainForm.HiddenFX.Caption := 'A_ST_CC0.BMP';
      Application.ProcessMessages;
      TempColor := RGB(Colors[0, T],
        Colors[1, T],
        Colors[2, T]);
      for TT := 1 to 235 do
      begin
        D1 := ((-y) - (z - w));
{dx/dt = -y - z - w
dy/dt = x
dz/dt = c(z/2  - z^2)  -dw}
        D2 := (X);
        DL1 := (Z / 2);
        DL1A := (Z * Z);
        DL1B := (c * (DL1 - DL1A));
        DL2 := (d * w);
        D3 := (DL1B - DL2);
        X := ((X + D1 * Step));
        Y := ((Y + D2 * Step));
        Z := ((Z + D3 * Step)); {320  240  }
{If DL1A  < MinU then DL1A := MinU else
If DL1A > MaxU then DL1A := MaxU;
If DL1B  < MinU then DL1B := MinU else
If DL1B > MaxU then DL1B := MaxU;}
        XPosT := HalfX + round((x / XStep));
        YPosT := FYImageY {HalfY} - round((y / YStep));
        Pixels[XPosT, YPosT] := TempColor;
      end; { of For T     NEXT T}
{in the Drawing...}
{bRotateImage:=False;
bRotatingImage:=True;
Repeat Application.ProcessMessages until (bRotateImage=True);
}
    end; { of For TT     NEXT TT}
  end;
  MainForm.HiddenFX.Caption := 'HiddenFX';
  Application.ProcessMessages;
end; { of StrangeChemicalsC }

(*************************************************************)

procedure StrangeChemicalsd;
{dx/dt = V1  - b1x  - V
dy/dt = V2 - b2y  + V  -(m(y-z))
dz/dt = ma(y-z)
V = (x - ky) / (1 + (x+y)(1 + cx^g) )}
var
{s : string[10];}
{A,  F,B, k, k1,k2,k3,k4,k5,k6,k7,k8,}
  C, g, V, V1, V2, b1, k, m, a, b2,
{MinU, MaxU,}
  D1, D2, D3, X, Y, Z: Extended;
{IKan, NPts,} XSize, YSize {, Limit, i, j, Steps}: integer;
{ XPos, YPos : integer;      }
  XPosT, YPosT: Longint;
  Xmin, YMin, XStep, YStep, XMax, YMax: Extended;
  TT, T {,Q}: integer;
  DL1A, DL1B, DL1, Dl2, Dl3, Dl, Dr,
    Step {Ua1, Ua2, Va1, Va2, YTL, YTM, YTR, TM, TL,TR,BL,BR}:
      Extended;
  TempColor: TColor;
begin
  FractalFilename := 'A_ST_CD0.BMP';
  XSize := FYImageX - 1; { physical screen size }
  YSize := FYImageY - 1; { physical screen size }
  with MainForm.Image2.Canvas do begin
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color := TempColor;
    TextOut(10, 10, 'Chemical    D' {s});
    MainForm.Show;
    MainForm.HiddenFX.Caption := 'A_ST_CD0.BMP';
    Application.ProcessMessages;
    XMax := 93.320; {3.56}
    XMin := 25.0; {-3.70}
    YMax := 96.230; {6.76   3.61}
    YMin := 25.0; {-5.14  -3.02}
    XStep := (XMax - XMin) / XSize;
    YStep := (YMax - YMin) / YSize;
    Step := 0.1 / 25;
    X := 12.2;
    Y := 12.0;
    Z := 12.1;
    V := 1.1;
    V1 := 02.20;
    V2 := 02.10;
    b1 := 1.20;
    b2 := 1.30;
    m := 1.20;
    a := 1.30;
    k := 1.20;
    g := 1.10;
    C := 1.10;
{dx/dt = V1  - b1x  - V
dy/dt = V2 - b2y  + V  -(m(y-z))
dz/dt = ma(y-z)
V = (x - ky) / (1 + (x+y)(1 + cx^g) )}
{MinU := -1E+5;
MaxU := 1E+5;}
    for T := 1 to 255 do begin
      MainForm.HiddenFX.Caption := 'A_ST_CD0.BMP';
      Application.ProcessMessages;
      TempColor := RGB(Colors[0, T],
        Colors[1, T],
        Colors[2, T]);
      for TT := 1 to 200 do
      begin
        DL1 := (b1 * X);
        D1 := (V1 - DL1 - V);
{dx/dt = V1  - b1x  - V
dy/dt = V2 - b2y  + V  -(m(y-z))
dz/dt = ma(y-z)
V = (x - ky) / (1 + (x+y)(1 + cx^g) )}
        DL1 := (b2 * y);
        DL1A := (V2 - DL1 + V);
        Dl3 := (y - z);
        Dr := (m * Dl3);
        D2 := (DL1A - Dr);
        DL1A := (m * a);
        D3 := (DL1A * Dl3);
        DL1A := (k * y);
        DL1 := (X - DL1A);
        DL1A := power(x, g);
        DL1B := (c * (DL1A));
        DL2 := (1 + DL1B);
        Dl := (X + Y);
        Dr := (DL2 * Dl);
        Dl3 := (1 + Dr);
        V := (DL1 / Dl3);
{V = (x - ky) / (1 + (x+y)(1 + cx^g) )}
        X := ((X + D1 * Step));
        Y := ((Y + D2 * Step));
        Z := ((Z + D3 * Step)); {320  240  }
{If DL1A  < MinU then DL1A := MinU else
If DL1A > MaxU then DL1A := MaxU;
If DL1B  < MinU then DL1B := MinU else
If DL1B > MaxU then DL1B := MaxU;}
        XPosT := (XSize div 2) + round((x / XStep));
        YPosT := (YSize div 2) - round((y / YStep));
        Pixels[XPosT, YPosT] := TempColor;
      end; { of For T     NEXT T}
{in the Drawing...}
{bRotateImage:=False;
bRotatingImage:=True;
Repeat Application.ProcessMessages until (bRotateImage=True);
}
    end; { of For TT     NEXT TT}
  end;
  MainForm.HiddenFX.Caption := 'HiddenFX';
  Application.ProcessMessages;
end; { of StrangeChemicals D }


(*************************************************************)

procedure StrangeChemicalse;
{dx/dt = x(a1  - k1x  - z  -y)  + k2y^2  + a3
dy/dt = y(x - k2y - a5)  + a2
dz/dt = z(a4  - x  - k5z)  + a3}
var
{s : string[10];}
{A,  F,B, k, k1,k2,k3,k4,k5,k6,k7,k8,}
  k5, k1, k2, a1, a2, a3, a4, a5,
{MinU, MaxU,}
  D1, D2, D3, X, Y, Z: Extended;
{IKan, NPts,} XSize, YSize {, Limit, i, j, Steps}: integer;
{ XPos, YPos : integer;      }
  XPosT, YPosT: Longint;
  Xmin, YMin, XStep, YStep, XMax, YMax: Extended;
  TT, T {,Q}: integer;
  DL1A, DL1B, DL1, Dl2, Dl3, Dl, Dr,
    Step {Ua1, Ua2, Va1, Va2, YTL, YTM, YTR, TM, TL,TR,BL,BR}:
      Extended;
  TempColor: TColor;
begin
  FractalFilename := 'A_ST_CE0.BMP';
  XSize := FYImageX - 1; { physical screen size }
  YSize := FYImageY - 1; { physical screen size }
  with MainForm.Image2.Canvas do begin
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color := TempColor;
    TextOut(10, 10, 'Chemical      E' {s});
    MainForm.Show;
    MainForm.HiddenFX.Caption := 'A_ST_CE0.BMP';
    Application.ProcessMessages;
    XMax := 93.320; {3.56}
    XMin := 25.0; {-3.70}
    YMax := 96.230; {6.76   3.61}
    YMin := 25.0; {-5.14  -3.02}
    XStep := (XMax - XMin) / XSize;
    YStep := (YMax - YMin) / YSize;
    Step := 0.1 / 25;
    X := 12.2;
    Y := 12.0;
    Z := 12.1;
    k5 := 1.1;
    k1 := 02.20;
    k2 := 02.10;
    a1 := 1.20;
    a2 := 1.30;
    a3 := 1.20;
    a4 := 1.30;
    a5 := 1.20;

{dx/dt = x(a1  - k1x  - z  -y)  + k2y^2  + a3
dy/dt = y(x - k2y - a5)  + a2
dz/dt = z(a4  - x  - k5z)  + a3}
{MinU := -1E+5;
MaxU := 1E+5;}
    for T := 1 to 255 do begin
      MainForm.HiddenFX.Caption := 'A_ST_CE0.BMP';
      Application.ProcessMessages;
      TempColor := RGB(Colors[0, T],
        Colors[1, T],
        Colors[2, T]);
      for TT := 1 to 200 do
      begin
        DL1 := (k1 * X);
        DL1A := (a1 - DL1 - z - y);
        DL1B := (x * (DL1A));
        Dl := (Y * Y);
        Dr := (k2 * Dl);
        D1 := (Dr + Dl1B + a3);
{dx/dt = x(a1  - k1x  - z  -y)  + k2y^2  + a3
dy/dt = y(x - k2y - a5)  + a2
dz/dt = z(a4  - x  - k5z)  + a3}
        DL1 := (k2 * y);
        DL1A := (x - DL1 - a5);
        Dl3 := (y * DL1A);
        D2 := (DL3 + a2);
        DL1 := (k5 * z);
        DL1A := (a4 - x - DL1);
        Dl3 := (z * DL1A);
        D3 := (DL3 + a3);
        X := ((X + D1 * Step));
        Y := ((Y + D2 * Step));
        Z := ((Z + D3 * Step)); {320  240  }
{If DL1A  < MinU then DL1A := MinU else
If DL1A > MaxU then DL1A := MaxU;
If DL1B  < MinU then DL1B := MinU else
If DL1B > MaxU then DL1B := MaxU;}
        XPosT := (XSize div 2) + round((x / XStep));
        YPosT := (YSize div 2) - round((y / YStep));
        Pixels[XPosT, YPosT] := TempColor;
      end; { of For T     NEXT T}
{in the Drawing...}
{bRotateImage:=False;
bRotatingImage:=True;
Repeat Application.ProcessMessages until (bRotateImage=True);
}
    end; { of For TT     NEXT TT}
  end;
  MainForm.HiddenFX.Caption := 'HiddenFX';
  Application.ProcessMessages;
end; { of StrangeChemicals e }

(*************************************************************)

procedure StrangeChemicalsf;
{dx/dt = V0  - b1x  - V
dy/dt = 1 - y - V
V =  (ay) / ( (1+ dy) (c+ x(1 + x^g)) )}
var
{s : string[10];}
{A,  F,B, k, k1,k2,k3,k4,k5,k6,k7,k8,}
  C, g, V, V0, b1, a, d,
{MinU, MaxU,}
  D1, D2, D3, X, Y, Z: Extended;
{IKan, NPts,} XSize, YSize {, Limit, i, j, Steps}: integer;
{ XPos, YPos : integer;      }
  XPosT, YPosT: Longint;
  Xmin, YMin, XStep, YStep, XMax, YMax: Extended;
  TT, T {,Q}: integer;
  DL1A, DL1B, DL1, Dl2, Dl3, Dl, Dr,
    Step {Ua1, Ua2, Va1, Va2, YTL, YTM, YTR, TM, TL,TR,BL,BR}:
      Extended;
  TempColor: TColor;
begin
  FractalFilename := 'A_ST_CF0.BMP';
  XSize := FYImageX - 1; { physical screen size }
  YSize := FYImageY - 1; { physical screen size }
  with MainForm.Image2.Canvas do begin
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color := TempColor;
    TextOut(10, 10, 'Chemical       F' {s});
    MainForm.Show;
    MainForm.HiddenFX.Caption := 'A_ST_CF0.BMP';
    Application.ProcessMessages;
    XMax := 93.320; {3.56}
    XMin := 25.0; {-3.70}
    YMax := 96.230; {6.76   3.61}
    YMin := 25.0; {-5.14  -3.02}
    XStep := (XMax - XMin) / XSize;
    YStep := (YMax - YMin) / YSize;
    Step := 0.1 / 25;
    X := 12.2;
    Y := 12.0;
    Z := 12.1;
    V := 21.1;
    V0 := 02.20;
    b1 := 1.20;
    C := 1.20;
    a := 1.30;
    d := 11.20;
    g := 11.10;
{dx/dt = V0  - b1x  - V
dy/dt = 1 - y - V
V =  (a x y) / ( (1+ dy) (c+ x(1 + x^g)) )}
{MinU := -1E+5;
MaxU := 1E+5;}
    for T := 1 to 255 do begin
      MainForm.HiddenFX.Caption := 'A_ST_CF0.BMP';
      Application.ProcessMessages;
      TempColor := RGB(Colors[0, T],
        Colors[1, T],
        Colors[2, T]);
      for TT := 1 to 200 do
      begin
        DL1 := (b1 * X);
        D1 := (V0 - DL1 - V);
{dx/dt = V0  - b1x  - V
dy/dt = 1 - y - V
V =  (a x y) / ( (1+ dy) (c+ x(1 + x^g)) )}
        D2 := (1 - y - V);
        DL1 := (a * x * y);
        DL1A := power(x, g);
        DL1B := (x * (1 + DL1A));
        DL2 := (C + DL1B);
        Dl := (d * Y);
        Dr := (1 + Dl);
        Dl3 := (Dl2 * Dr);
        V := (DL1 / Dl3);
{V = (x - ky) / (1 + (x+y)(1 + cx^g) )}
        X := ((X + D1 * Step));
        Y := ((Y + D2 * Step));
  {Z := ((Z + D3 * Step)); }{320  240  }
{If DL1A  < MinU then DL1A := MinU else
If DL1A > MaxU then DL1A := MaxU;
If DL1B  < MinU then DL1B := MinU else
If DL1B > MaxU then DL1B := MaxU;}
        XPosT := (XSize div 2) + round((x / XStep));
        YPosT := (YSize div 2) - round((y / YStep));
        Pixels[XPosT, YPosT] := TempColor;
      end; { of For T     NEXT T}
{in the Drawing...}
{bRotateImage:=False;
bRotatingImage:=True;
Repeat Application.ProcessMessages until (bRotateImage=True);
}
    end; { of For TT     NEXT TT}
  end;
  MainForm.HiddenFX.Caption := 'HiddenFX';
  Application.ProcessMessages;
end; { of StrangeChemicals  }

(*************************************************************)

procedure StrangeChemicalsg;
{Rietman p111}
{dx/dt = k1A - k2Bx + k3x^2y  - k4x
dy/dt = k2Bx  - k3 x^2y}
var
{s : string[10];}
{A,  F,B, k, k1,k2,k3,k4,k5,k6,k7,k8,}
{ C, g,V,V1,V2,b1,k,m,a,b2,  }
{MinU, MaxU,} k1, k2, k3, k4, B, A,
    D1, D2, D3, X, Y, Z: Extended;
{IKan, NPts,} XSize, YSize {, Limit, i, j, Steps}: integer;
{ XPos, YPos : integer;      }
  XPosT, YPosT: Longint;
  Xmin, YMin, XStep, YStep, XMax, YMax: Extended;
  TT, T {,Q}: integer;
  DL1A, DL1B, DL1, Dl2, Dl3, Dl, Dr,
    Step {Ua1, Ua2, Va1, Va2, YTL, YTM, YTR, TM, TL,TR,BL,BR}:
      Extended;
  TempColor: TColor;
begin
  FractalFilename := 'A_ST_CG0.BMP';
  XSize := FYImageX - 1; { physical screen size }
  YSize := FYImageY - 1; { physical screen size }
  with MainForm.Image2.Canvas do begin
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color := TempColor;
    TextOut(10, 10, 'Chemical       G' {s});
    MainForm.Show;
    MainForm.HiddenFX.Caption := 'A_ST_CG0.BMP';
    Application.ProcessMessages;
    XMax := 16.320; {3.56}
    XMin := 2.0; {-3.70}
    YMax := 16.230; {6.76   3.61}
    YMin := 2.0; {-5.14  -3.02}
    XStep := (XMax - XMin) / XSize;
    YStep := (YMax - YMin) / YSize;
    Step := 0.1 / 25;
    X := 1.2;
    Y := 1.0;
    Z := 1.1;
    k1 := 1.1;
    k2 := 01.20;
    k3 := 01.10;
    k4 := 1.20;
    B := 1.20;
    A := 1.30;
{dx/dt = k1A - k2Bx + k3x^2y  - k4x
dy/dt = k2Bx  - k3 x^2y}
{MinU := -1E+5;
MaxU := 1E+5;}
    for T := 0 to 255 do begin
      MainForm.HiddenFX.Caption := 'A_ST_CG0.BMP';
      Application.ProcessMessages;
      TempColor := RGB(Colors[0, T],
        Colors[1, T],
        Colors[2, T]);
      for TT := 1 to 200 do
      begin
{dx/dt = k1A - k2Bx + k3x^2y  - k4x
dy/dt = k2Bx  - k3 x^2y}
        DL1 := (k1 * A);
        DL1A := (k2 * B * X);
        DL1B := (X * X);
        DL2 := (k3 * DL1B * Y);
        Dl3 := (k4 * X);
        D1 := (DL1 - DL1A + DL2 - Dl3);
        DL1A := (k2 * B * X);
        DL1B := (X * X);
        DL1 := (k3 * DL1B);
        D2 := (DL1A - DL1);
        X := ((X + D1 * Step));
        Y := ((Y + D2 * Step));
{Z := ((Z + D3 * Step));    320  240  }
{If DL1A  < MinU then DL1A := MinU else
If DL1A > MaxU then DL1A := MaxU;
If DL1B  < MinU then DL1B := MinU else
If DL1B > MaxU then DL1B := MaxU;}
        XPosT := (XSize div 2) + round((x / XStep));
        YPosT := (YSize div 2) - round((y / YStep));
        Pixels[XPosT, YPosT] := TempColor;
      end; { of For T     NEXT T}
{in the Drawing...}
{bRotateImage:=False;
bRotatingImage:=True;
Repeat Application.ProcessMessages until (bRotateImage=True);
}
    end; { of For TT     NEXT TT}
  end;
  MainForm.HiddenFX.Caption := 'HiddenFX';
  Application.ProcessMessages;
end; { of StrangeChemicals g }
(*************************************************************)

(*************************************************************)
(*************************************************************)


(*************************************************************)



end.
