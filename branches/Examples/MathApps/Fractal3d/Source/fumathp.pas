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
unit fumathp;

interface
uses
  Windows, Messages, SysUtils, Dialogs, Forms,
  Math, Graphics, Classes;

procedure bifurc(Rin, Din: Extended; Which: Integer);
procedure feigen;
procedure bifbaum(Rin, Din: Extended; Which: Integer);
procedure Malthus_1;
procedure Malthus_2;

procedure BBrownian(Seed: Longint);
procedure BBrown2d(Seed: Longint);
procedure Brown_Gauss(F1, D: Extended; Time: Integer);
procedure WhiteBrown;
procedure Cellar;
procedure CellTorus;
procedure CellZap;
procedure HiFiLines;
implementation

uses fUGlobal,fMain, fGMath {from AVGAFRAK B_Strang};


 (**************************************************)

procedure bifurc(Rin, Din: Extended; Which: Integer);
var Xs: string;
  r, x, delta_r: Extended;
  maxcolx, maxrowy, i: integer;
  row, col: longint; TempColor: TColor;
begin

  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);

{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    TextOut(10, 10, 'Population Bifurcation');
    r := 0.95;
    delta_r := 0.005;
    if ((Which = 0)) then begin
      r := 0.95;
      delta_r := 0.005;
    end;
    if ((Which = 1)) then begin
      r := 3.55;
      delta_r := 0.0005;
    end;
    if ((Which = 2)) then begin
      r := Rin { 0.95};
      delta_r := Din {0.005};
    end;
    for col := 0 to maxcolx do
    begin
      x := 0.5;
      r := r + delta_r;
      i := 0;
      str(r: 8: 5, Xs);
      MainForm.HiddenFX.Caption := 'Rate: ' + Xs;
      Application.ProcessMessages;
      repeat
        x := r * x * (1 - x);
{  COMPUTATION FOR rx(1-x)}
        row := maxrowy - Round(x * FYImageY);
                     {  COMPUTATION FOR x(1-x)}{960}
{	           row := Round(maxrowy -((x/r)*1820));}
        if i > 64 then {throw out first 64}
          if row <= maxrowy then
            if row >= 0 then
              if col >= 0 then
                if col <= maxcolx then
{ TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));}
                  TempColor := RGB(Colors[0, i], Colors[1, i],
                    Colors[2, i]);
        Pixels[col, row] := TempColor;
        Inc(i);
      until (x > 1000) or (x < -1000) or (i > 255);
    end; {Columns}
  end; {End of part 1}
end;
 (**************************************************)

procedure bifbaum(Rin, Din: Extended; Which: Integer);
var Xs: string;
  r, x, delta_r: Extended;
  maxcolx, maxrowy, i, Row3: integer;
  row, col: longint; TempColor: TColor;
begin
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
    Row3 := (FYImageY * 3);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
    TextOut(10, 10, 'Fiegenbaum Bifurcation');

    r := 0.95;
    delta_r := 0.005;
    if ((Which = 0)) then begin
      r := 0.95;
      delta_r := 0.005;
    end;
    if ((Which = 1)) then begin
      r := 3.55;
      delta_r := 0.0005;
    end;
    if ((Which = 2)) then begin
      r := Rin { 0.95};
      delta_r := Din {0.005};
    end;
    for col := 0 to maxcolx do
    begin
      x := 0.5;
      r := r + delta_r;
      i := 0;
      str(r: 8: 5, Xs);
      MainForm.HiddenFX.Caption := 'Rate: ' + Xs;
      Application.ProcessMessages;
      repeat
        x := r * x * (1 - x);
{  COMPUTATION FOR rx(1-x)}
{                   row := 479 - Round(x*480);          }
                     {  COMPUTATION FOR x(1-x)}{960}
        row := Round(maxrowy - ((x / r) * Row3));
        if i > 64 then {1820}
          if row <= maxrowy then
            if row >= 0 then
              if col >= 0 then
                if col <= maxcolx then
                  TempColor := RGB(Colors[0, i],
                    Colors[1, i],
                    Colors[2, i]);
        Pixels[col, row] := TempColor;
        Inc(i);
      until (x > 1000) or (x < -1000) or (i > 255);
    end; {Columns}
  end; {End of part 1}
end;
 (**************************************************)

procedure feigen; { NOT a graphic program - Text numbers}
var
  x, lambda, step_size, old_x, test, lambda_1, lambda_2, delta,
    init_step, old_lambda {,new_step,old_step}: extended;
  i, iterations: longint;
  j, sign: integer; CodeSl, CodeSd: string;
begin
 {       ClrScr;}
  lambda := 3.0; delta := lambda;
  lambda_1 := lambda; lambda_2 := lambda;
{	Writeln('n          Lambda                 Delta');}
  str(lambda: 22: 18, CodeSl);
  str(delta: 22: 18, CodeSd);
  MainForm.HiddenFX.Caption := 'lambda: ' + CodeSl + ', delta: ' +
    CodeSd;
  MathForm.DIYMemo.Lines.Append('lambda: ' + CodeSl + ', delta: ' +
    CodeSd);
  Application.ProcessMessages;
  init_step := 1;
  for j := 1 to 19 do begin
    if j mod 2 = 0 then sign := -1
    else sign := 1;
{             GotoXY(0,15+j);}
    init_step := init_step / 4.67;
    step_size := init_step;
    iterations := 2;
    for i := 1 to j - 1 do
      iterations := iterations * 2;
    old_x := 0.5;
    lambda := lambda + step_size;
    repeat
      x := old_x;
      for i := 0 to iterations - 1 do
        x := lambda * x * (1 - x);
      test := (x - old_x) * sign;
      if test < 0 then
      begin
        lambda := lambda - step_size;
        step_size := step_size / 2;
      end;
      old_lambda := lambda;
      lambda := lambda + step_size;
{                GotoXY(1,j+3);
  write(j:2,'   ',lambda:20:18,'   ');}
      str(lambda: 22: 18, CodeSl);
      str(delta: 22: 18, CodeSd);
      MainForm.HiddenFX.Caption := 'lambda: ' + CodeSl + ', delta: '
        + CodeSd;
      MathForm.DIYMemo.Lines.Append('lambda: ' + CodeSl + ', delta: '
        + CodeSd);
      Application.ProcessMessages;
    until old_lambda >= lambda;
    if j > 2 then
    begin
      delta := (lambda_1 - lambda_2) / (lambda - lambda_1);
{		write(delta:20:18);}
      str(lambda: 22: 18, CodeSl);
      str(delta: 22: 18, CodeSd);
      MainForm.HiddenFX.Caption := 'lambda: ' + CodeSl + ', delta: '
        + CodeSd;
      MathForm.DIYMemo.Lines.Append('lambda: ' + CodeSl + ', delta: '
        + CodeSd);
      Application.ProcessMessages;

    end;
    lambda_2 := lambda_1;
    lambda_1 := lambda;
  end;
  str(lambda: 22: 18, CodeSl);
  str(delta: 22: 18, CodeSd);
  MainForm.HiddenFX.Caption := 'lambda: ' + CodeSl + ', delta: ' +
    CodeSd;
  MathForm.DIYMemo.Lines.Append('lambda: ' + CodeSl + ', delta: ' +
    CodeSd);
  Application.ProcessMessages;
end; { of Feigen }
 (**************************************************)


   {=======================}
   {     MALTHUS1.PAS      }
   {=======================}

procedure Malthus_1;
var Xs: string;
  MaxX, MaxY, MaxGen: integer;
  i, j, Color, Count, X, Y: integer;
  PopOld, PopNew, Rate: Extended; TempColor: TColor;
begin
   {                                    }
   {    Malthusean Population Growth    }
   {     Pn+1 = R * Pn * ( 1 - Pn )     }
   {                                    }
  MaxGen := 32766;
  Rate := 2.3;
  Color := 0;
  with MainForm.Image2.Canvas do begin
    MaxX := (FYImageX - 1);
    MaxY := (FYImageY - 1);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    TextOut((FYImageX div 2) {320}, 10, 'Malthusian Flux  v.1 ');

    for j := 1 to 151 do
    begin
      inc(Color);
      TempColor := RGB(Colors[0, Color mod 255],
        Colors[1, Color mod 255],
        Colors[2, Color mod 255]);
{TempColor:=RGB(RGBArray[0,(Color mod 15)],
        RGBArray[1,Color mod 15],
        RGBArray[2,Color mod 15]);}
      Count := 0;
      Rate := Rate + 0.01; { increment Rate   }
      PopOld := 0.01; { reset Population }
      str(Rate: 8: 5, Xs);
      MainForm.HiddenFX.Caption := 'Rate: ' + Xs;
      Application.ProcessMessages;
      for i := 1 to MaxGen do
      begin
        PopNew := Rate * (PopOld * (1 - PopOld));
        X := trunc(PopOld * MaxX);
        Y := trunc(MaxY - (PopNew * MaxY));
        Pixels[X, Y] := TempColor;
        if PopOld = PopNew then inc(Count)
        else Count := 0;
        if Count > 10 then Break; { i := MaxGen;}
           { stagnant -- break out }
        PopOld := PopNew;
      end;
    end;
  end;
end; { of   procedure Malthus_1; }



   {=======================}
   {     MALTHUS2.PAS      }
   {=======================}

procedure Malthus_2;
var TempColor: TColor; Xs: string;
  MaxX, MaxY, i, j, k, Color, Count: integer;
  PopOld, PopNew, X, Y, Rate: Extended;
begin
  { PopOld := 0.0;
   PopNew := 0.0;}
  Rate := 2.3;
  Y := 0;
  Color := 0;
  with MainForm.Image2.Canvas do begin
    MaxX := (FYImageX - 1);
    MaxY := (FYImageY - 1);
{   Brush.Color:=FBackGroundColor;
   Brush.Style:=bsSolid;
   FillRect(Rect(0,0,MaxX,MaxY));
   TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
   Font.Color:= TempColor;
   MainForm.Show;}
    TextOut((FYImageX div 2) {320}, 10, 'Malthusian Flux  v.2 ');

    for j := 1 to 15 do
    begin
      inc(Color);
{TempColor:=RGB(Colors[0,i],Colors[1,i],Colors[2,i]);}
      TempColor := RGB(RGBArray[0, (Color mod 15)],
        RGBArray[1, (Color mod 15)], RGBArray[2, (Color mod 15)]);
      for k := 1 to 10 do
      begin
        Count := 0;
        Rate := Rate + 0.01;
        PopOld := 0.01;
        str(Rate: 8: 5, Xs);
        MainForm.HiddenFX.Caption := 'Rate: ' + Xs;
        Application.ProcessMessages;
        for i := 1 to 10000 do
        begin
          PopNew := Rate * (PopOld * (1 - PopOld));
          X := PopNew - PopOld;
          Pixels[trunc((X * MaxX / 2) + MaxX / 2),
            trunc((MaxY / 2) - (Y * MaxY / 2))] := TempColor;
          if PopOld = PopNew then inc(Count)
          else Count := 0;
          if Count > 100 then break; {i := 10000;}
          PopOld := PopNew;
          Y := X;
        end;
      end;
    end;
  end;
end; { of procedure Malthus_2; }



(**************************************************)

procedure BBrownian(Seed: Longint);
const
  scale: Extended = 80;
  h: Extended = 0.87;
{	seed: longint = 3;  }{3456;}
  mu: Extended = 0.0;
  sigma: Extended = 1.0;
var TempColor: TColor;
  Fh: array[0..256] of Extended;
  ratio, std: Extended;
  maxcolx, maxrowy, i: integer;
  function gauss(seed: longint; mu: Extended; sigma: Extended):
    Extended;
  var
    x: Extended;
    i: integer;
  begin
    if seed <> 0 then RandSeed := seed;
    x := 0;
    for i := 0 to 11 do x := x + Random;
    x := x - 6.0;
    gauss := mu + sigma * x;
  end;
  procedure subdivide(f1: integer; f2: integer; std: Extended);
  var
    fmid: integer;
    stdmid: Extended;
  begin
    fmid := (f1 + f2) div 2;
    if (fmid <> f1) and (fmid <> f2) then
    begin
      Fh[fmid] := (Fh[f1] + Fh[f2]) / 2.0 +
        gauss(0, mu, sigma) * std;
      stdmid := std * ratio;
      subdivide(f1, fmid, stdmid);
      subdivide(fmid, f2, stdmid);
    end;
  end;
begin {Actual program}
  Randomize;
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;Pen.Width:=3;
Font.Color:= TempColor;
MainForm.Show;  }
{		Line(0,240,639,240);  }
    Moveto(0, (maxrowy div 2)); Lineto(maxcolx, (maxrowy div 2));
    TempColor := RGB(Colors[0, (seed div 255)],
      Colors[1, (seed div 255)],
      Colors[2, (seed div 255)]);
    Pen.Color := TempColor; Pen.Width := 1;
    Fh[0] := gauss(seed, mu, sigma) * scale;
    Fh[256] := gauss(0, mu, sigma) * scale;
    ratio := Exp(-0.693147 * h);
    std := scale * ratio;
    subdivide(0, 256, std);
    for i := 0 to 255 do begin
        {Line(2*i+80,(maxrowy div 2)-Round(Fh[i]),
                2*(i+1)+80,(maxrowy div 2)-Round(Fh[i+1]));}
      Moveto(2 * i + 80, (maxrowy div 2) - Round(Fh[i]));
      Lineto(2 * (i + 1) + 80, (maxrowy div 2) - Round(Fh[i + 1]));
    end;
  end;
end; { of Brownian }

(**************************************************)

procedure BBrown2d(Seed: Longint); {(level : Integer);}
const
  scale: Extended = 80;
  h: Extended = 1;//0.87;
  mu: Extended = 0.0;
  sigma: Extended = 1.0;
{	seed: longint = 3045;}
var
  Fh: array[0..256] of Extended;
  Fw: array[0..256] of Extended;
  ratio, std: Extended;
  maxcolx, maxrowy, i: integer;
  TempColor: TColor;
  function gauss(seed: longint): Extended;
  var
    x: Extended;
    i: integer;
  begin
    if seed <> 0 then RandSeed := seed;
    x := 0;
    for i := 0 to 11 do x := x + Random;
    x := x - 6.0;
    gauss := mu + sigma * x;
  end;
  procedure subdivide(f1: integer; f2: integer; std: Extended);
  var
    fmid: integer;
    stdmid: Extended;
  begin
    fmid := (f1 + f2) div 2;
    if (fmid <> f1) and (fmid <> f2) then begin
      Fh[fmid] := (Fh[f1] + Fh[f2]) / 2.0 + gauss(0) * std;
      Fw[fmid] := (Fw[f1] + Fw[f2]) / 2.0 + gauss(0) * std;
      stdmid := std * ratio;
      subdivide(f1, fmid, stdmid);
      subdivide(fmid, f2, stdmid);
    end;
  end;

begin
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX div 2);
    maxrowy := (FYImageY div 2);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));

Pen.Color := TempColor;Pen.Width:=3;
Font.Color:= TempColor;
MainForm.Show;}
    TempColor := RGB(255 - GetRValue(FBackGroundColor),
      255 - GetGValue(FBackGroundColor),
      255 - GetBValue(FBackGroundColor));
        {	Line(3,240,637,240); }
    Moveto(3, maxrowy);
    Lineto(FYImageX - 3, maxrowy);
  {Line(320,0,320,479);}
    Moveto(maxcolx, 0);
    Lineto(maxcolx, FYImageY - 1);
    Pen.Color := TempColor; Pen.Width := 1;
    Fh[0] := gauss(seed) * scale;
    Fh[256] := gauss(0) * scale;
    Fw[0] := gauss(seed) * scale;
    Fw[256] := gauss(0) * scale;
    ratio := Exp(-0.693147 * h);
    std := scale * ratio;
    subdivide(0, 256, std);
    for i := 0 to 255 do begin
 {Setcolor((i div 32) +3);}
      TempColor := RGB(Colors[0, (i div 255)],
        Colors[1, (i div 255)],
        Colors[2, (i div 255)]);
      Pen.Color := TempColor;
 {Line(Round(Fw[i])+320,240-Round(Fh[i]),
        Round(Fw[i+1])+320,240-Round(Fh[i+1]));}
      Moveto(Round(Fw[i]) + maxcolx, maxrowy - Round(Fh[i]));
      Lineto(Round(Fw[i + 1]) + maxcolx, maxrowy - Round(Fh[i + 1]));
    end;
  end;
end;
(**************************************************)

procedure Brown_Gauss(F1, D: Extended; Time: Integer);
var
  maxcolx, maxrowy, N, I, X: Integer;
  F, PH: array[0..16] of Extended;
  RHO, SSQ, AC, DUM, RN, Z, Y: Extended;
  XColor {,TempColor}: TColor; timeS: string;
begin
  SSQ := 0.0;
  AC := 0.0;
  RN := 0.0;
  Z := 0.0;
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY div 2);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));
TempColor:=  RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    str(time, timeS);
    TextOut(10, 10, 'Fractaled Brownian iaw F1, D ' + timeS);

    Randomize;
    for N := 0 to 16 do
    begin
      F[N] := (EXP(N * ln(F1)));
      PH[N] := 6.28 * Random;
    end;
{FRACTAL}
{ Pen.Color:=TempColor; }
    Moveto(0, 0);
    for X := 0 to maxcolx do begin
      Y := 0.0;
      for N := 1 to 16 do
        Y := (Y + Time * SIN(F[N] * (X / Time) + PH[N])
          * (EXP((D - 2) * LN(F[N]))));
{          * power(F[N],(D-2)) );}
{        Pixels[X, (maxrowy -(ROUND(Y)div 2)) ]:=TempColor;}
      Lineto(X, (maxrowy - (ROUND(Y) div 2)));
      SSQ := SSQ + Y * Y;
      AC := AC + Y * Z;
      Z := Y;
    end;
{ GAUSSIAN}{clGreen;}
{XColor:=(Abs(TempColor - FBackGroundColor)+1);}
    XColor := RGB(Abs(155 - GetRValue(FBackGroundColor)),
      Abs(155 - GetGValue(FBackGroundColor)),
      Abs(155 - GetBValue(FBackGroundColor)));
    Font.Color := XColor;
    TextOut(10, 30,
      'Gaussian Brownian iaw no time*(12 randoms - 6)');
    RHO := AC / SSQ;
    DUM := SQRT((1 - RHO * RHO) * (SSQ / maxcolx));
    Pen.Color := XColor;
    Moveto(0, 0);
    for X := 0 to maxcolx do begin
      Y := 0; {just to keep compiler quiet}
      for I := 0 to 11 do RN := RN + Random;
      RN := RN - 6;
      Y := Y * RHO + DUM * RN;
{        Pixels[X, (maxrowy -(ROUND(Y)div 2))]:= XColor;}
      Lineto(X, (maxrowy - (ROUND(Y) div 2)));
    end;
  end;
end; { of Procedure Brown_Gauss}

(**************************************************)

{
function RandG(Mean, StdDev: Extended): Extended;
RandG produces random numbers with
Gaussian distribution about the Mean.
var RandSeed: LongInt;
By assigning a specific value to RandSeed,
the Random function can repetitively generate
a specific sequence of random numbers.
function Random [ ( Range: Integer) ];
Random returns a random number within the range
0 <= X < Range.
procedure Randomize;
Randomize initializes the built-in random number generator
with a random value (obtained from the system clock).
}

procedure WhiteBrown;
var InNumber: Extended; I, maxcolx, maxrowy: Integer;
  FColor, BColor {,TempColor}: TColor;
begin {}
{NRand:=4;
Arand:=((power(2,31))-1);
GaussAdd:= sqrt(3*Nrand);
GaussFac:= ((2*GaussAdd) / (NRand * Arand));
random(Arand);}
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY div 2);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));
TempColor:=  RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    TextOut(10, 10, 'White , 1/f,  Brownian ');

    Randomize;
    Moveto(0, 0);
    for I := 0 to maxcolx do begin
      InNumber := (RandG(0.0, 1.0) * 10);
      Lineto(i, (maxrowy - Round(InNumber) - 100));
        {Pixels[ i, maxrowy-Round(InNumber)]:=TempColor;}
    end;

    Moveto(0, 0);
    BColor := RGB(Abs(166 - GetRValue(FBackGroundColor)),
      Abs(166 - GetGValue(FBackGroundColor)),
      Abs(166 - GetBValue(FBackGroundColor)));
    Pen.Color := BColor;
    for I := 0 to (maxcolx div 2) do begin
      InNumber := (RandG(10.0, 2.0) * 10);
      Lineto(i * 2, (maxrowy - Round(InNumber) + 100));
        {Pixels[ i, maxrowy-Round(InNumber)]:=BColor; }
    end;

    Moveto(0, 0);
    for I := 0 to (maxcolx div 4) do begin
      FColor := RGB(Colors[0, (i div 255)],
        Colors[1, (i div 255)],
        Colors[2, (i div 255)]);
      Pen.Color := FColor;
      InNumber := (RandG(10.0, 1.0) * 5);
      Lineto(i * 4, (maxrowy - Round(InNumber) + 100));
    end;
  end;
end;
{ARand
NRand
GaussAd
GaussFac
}
(**************************************************)
(**************************************************)


(**************************************************)
(**************************************************)

procedure Cellar;
var TempColor: TColor; Xs, Ys: string;
  maxcolx, maxrowy, X, Y: Integer;
begin
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show; }
    Randomize;
    X := maxcolx div 2;
    Y := maxrowy div 2;
    bRotateImage := False; {in the Drawing...}
    bRotatingImage := True;
    repeat
      begin
        X := X + (Random(3) - 1);
        Y := Y + (Random(3) - 1);
        if X > maxcolx then X := maxcolx;
        if X < 0 then X := 0;
        if Y > maxrowy then Y := maxrowy;
        if Y < 0 then Y := 0;
{Z := GetPixel(X,Y);
Inc(Z);}
        TempColor := RGB(Colors[0, Random(255)],
          Colors[1, Random(255)], Colors[2, Random(255)]);
        Pixels[X, Y] := TempColor;
        str(X, Xs); str(Y, Ys);
        MainForm.HiddenFX.Caption := 'FX: ' + Xs + ', FY: ' + Ys;
        Application.ProcessMessages;
      end;
    until (bRotateImage = True);
  end;
end; { of Procedure Cellar }

(**************************************************)

procedure CellTorus;
var
  Xs, Ys: string;
  maxcolx, maxrowy, X, Y: Integer;
{ TempColor,} SColor: TColor;
begin

  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    Randomize;
    X := maxcolx div 2;
    Y := maxrowy div 2;

    bRotateImage := False; {in the Drawing...}
    bRotatingImage := True;
    repeat
      begin
        X := X + (Random(3) - 1);
        Y := Y + (Random(3) - 1);

        if X > maxcolx then X := 0;
        if X < 0 then X := maxcolx;
        if Y > maxrowy then Y := 0;
        if Y < 0 then Y := maxrowy;
        Scolor := Pixels[X, Y];
        if Scolor >= 0 then begin
          if (SColor = clBlack) then Scolor := clBlue else
            if (SColor = clBlue) then Scolor := clFuchsia else
              if (SColor = clFuchsia) then Scolor := clGreen else
                if (SColor = clGreen) then Scolor := clMaroon else
                  if (SColor = clMaroon) then Scolor := clOlive else
                    if (SColor = clOlive) then Scolor := clNavy else
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
          Pixels[X, Y] := Scolor;
        end;

        str(X, Xs); str(Y, Ys);
        MainForm.HiddenFX.Caption := 'FX: ' + Xs + ', FY: ' + Ys;
        Application.ProcessMessages;
      end;
    until (bRotateImage = True);
  end;
end; { of Procedure CellTorus}

(**************************************************)

procedure CellZap;
var
  Color, HalfX, HalfY, maxcolx, maxrowy, Counted, X, Y: Integer;
  Xs, Ys, S_Counted: string[6];
  Z: Extended; TempColor: TColor;
begin
  Randomize;
  HalfX := (FYImageX div 2);
  HalfY := (FYImageY div 2);
  X := (FYImageX div 2);
  Y := (FYImageY div 2);
  Z := 2.0;
  Counted := 0;
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,FYImageX,FYImageY));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    TextOut(1, 1, '        ');
    bRotateImage := False; {in the Drawing...}
    bRotatingImage := True;

    repeat
      begin
        X := X + (Random(3) - 1);
        Y := Y + (Random(3) - 1);
        Z := Z + 0.005;
        if X > maxcolx then
        begin
          X := HalfX;
          Y := HalfY;
          Inc(Counted);
          Str(Counted, S_Counted);
          TextOut(1, 1, S_Counted);
        end

        else if X < 0 then
        begin
          X := HalfX;
          Y := HalfY;
          Inc(Counted);
          Str(Counted, S_Counted);
          TextOut(1, 1, S_Counted);
        end;

        if Y > maxrowy then
        begin
          X := HalfX;
          Y := HalfY;
          Inc(Counted);
          Str(Counted, S_Counted);
          TextOut(1, 1, S_Counted);
        end
        else if Y < 0 then
        begin
          X := HalfX;
          Y := HalfY;
          Inc(Counted);
          Str(Counted, S_Counted);
          TextOut(1, 1, S_Counted);
        end;

        Color := Round(Z);
{TempColor:=RGB(Colors[0,i],Colors[1,i],Colors[2,i]);}
{TempColor:=RGB(RGBArray[0,(Color mod 15)],
        RGBArray[1,(Color mod 15)],RGBArray[2,(Color mod 15)]);}
        TempColor := RGB(Colors[0, (Color mod 255)], Colors[1, (Color
          mod 255)], Colors[2, (Color mod 255)]);

        Pixels[X, Y] := TempColor; {PutPixel(X,Y,Round(Z));}

        str(X, Xs); str(Y, Ys);
        MainForm.HiddenFX.Caption := 'FX: ' + Xs + ', FY: ' + Ys;
        Application.ProcessMessages;
      end;
    until (bRotateImage = True);
  end;
end; { of Procedure CellTorus}
(**************************************************)

procedure HiFiLines;
var
  Xs, Ys: string;
  Color, maxcolx, maxrowy, X, Y: Integer;
  Z: Extended; TempColor: TColor;
begin

  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
MainForm.Show;}
    Randomize;
    Moveto((maxcolx div 2), (maxrowy div 2));
    X := (maxcolx div 2);
    Y := (maxrowy div 2);
    Z := 2.0;
    bRotateImage := False; {in the Drawing...}
    bRotatingImage := True;
    repeat
      begin
        X := (X + (5 - (Random(11))));
        Y := (Y + (5 - (Random(11))));
        Z := Z + 0.1;
        if ((X > maxcolx) or (X < 0) or
          (Y > maxrowy) or (Y < 0)) then begin
          Moveto((maxcolx div 2), (maxrowy div 2));
          X := (maxcolx div 2);
          Y := (maxrowy div 2);
        end;
        Color := Round(Z);
        TempColor := RGB(Colors[0, (Color mod 255)], Colors[1, (Color
          mod 255)], Colors[2, (Color mod 255)]);
{TempColor:=RGB(RGBArray[0,(Color mod 15)],
        RGBArray[1,(Color mod 15)],RGBArray[2,(Color mod 15)]);}
        Pen.Color := TempColor;
        Lineto(X, Y);
{Pixels[X,Y]:= TempColor; PutPixel(X,Y,Round(Z));}
        str(X, Xs); str(Y, Ys);
        MainForm.HiddenFX.Caption := 'FX: ' + Xs + ', FY: ' + Ys;
        Application.ProcessMessages;
      end;
    until (bRotateImage = True);
  end;
end; { of Procedure HiFiLines}

(**************************************************)
(**************************************************)

end.
