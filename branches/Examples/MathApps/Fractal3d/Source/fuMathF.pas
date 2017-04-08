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
unit fuMathF;

interface
uses
  Windows, Messages, SysUtils, Dialogs, Forms,
  Graphics, Classes, Math;

procedure Fra_Mtns(Fune, D: Extended; Intensity: Integer);

procedure FractalGrid(Fun, D : Extended);
procedure FoldedGlobe( Krypton : Integer);
procedure DupliGlobe(Krypton : Integer);
procedure Craters(phingphactor:Double; DeptH, Intensity : Integer);
procedure Waves( DeptH, Intensity : Integer);
procedure Rippled(RadiusG, DeptH, Intensity : Integer);
procedure Splattered(RadiusG, DeptH, Intensity : Integer);

procedure DLA(Intensity: Integer);
procedure DLAC(Intensity: Integer);
procedure DLACentroid (Intensity: Integer);
{(Freq,F_Dim: Extended;Points,}
procedure B_3D_Map(N, L, VA, HA: Integer);
procedure FallingColumn;
procedure RotatingCube;
procedure SinShadow(Plot_Angle, M_x, sign: Integer;
  View_Angle, Illum_Angle: Extended);
procedure CoSinShadow(Plot_Angle, M_x, sign: Integer;
  View_Angle, Illum_Angle: Extended);

implementation
uses fUGlobal, fMain, fGMath,fXYZ3D;

const radtheta = 1 {degrees} * 3.1415926535 {radians} / 180
{per degrees};
      {sin and cos on computers are done in radians.}

type tpointr = record {Just a record to hold 3d points}
    x, y, z: real;
  end;

var box: array[0..7] of tpointr; {The box we will manipulate}


const
  xSize = 512; {90 degrees}
  ySize = 256; {60 degrees}
  angleMask = xSize * 4 - 1;
    {xSize must be power of 2 or and's won't work}
  mapSize = 256;

var
  sinTab: array of Integer;
{sinTab:array[0..angleMask]of integer;}
 {sin(xyAngle)*$7FFF}
  tanTab: array of Integer;
{ array[0..ySize-1]of integer;}
 {tan(zAngle)*$7FFF}
  map: array of array of byte;
{ array[0..mapSize-1,0..mapSize-1]of byte;}

type
  fixed = record case boolean of
      false: (l: longint);
      true: (f: word; i: integer);
  end;

(*************************************************************)

(*************************************************************)
procedure Fra_Mtns(Fune, D: Extended; Intensity: Integer);
var
  TempColor: TColor;
  MtnRowY, MaxZDo, MaxXDo, ColXF, RowYF,TempMan,
    maxcolx, maxrowy, N, I, XCount, ZCount, C, Y_Height: Integer;
  MtnZ, View_Angle, Illum_Angle, X, Z, R, Y, H, HS: Extended;
  iss, s, ss: string;
  F_File: file of Smallint; { Integer;}
  F, A, PH, TH, CS, SN: array[0..16] of Extended;
          {ColXF}
  HM: array[0..4096] of Extended;
begin
{Save LAST array made to FractalFileName}
    if (bSaveTheMountains) then
    begin
     {if (iMadeMountains < 3) then  }
      begin
        MainForm.HintPanel.Caption := 'Saving Mountains to disk';
        Application.ProcessMessages;
        FractalFileMatrix := ChangeFileExt(FractalFilename, '.BIN');
        AssignFile(F_File, FractalFileMatrix);
        ReWrite(F_File);
        if IoResult = 0 then
        begin
          MaximumElevation := -2147483647;
          MinimumElevation := 2147483646;
          maxcolx:=  FileSizeX- 1;
          MtnRowY := FileSizeY- 1;
          for Zcount := 0 to MtnRowY do
          begin
            Mainform.ProgressBar1.Position :=
            Round((Zcount / MtnRowY) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              TempMan:=ManMatrix[Xcount, Zcount];
              if (MaximumElevation <= TempMan) then
                MaximumElevation := TempMan;
              if (MinimumElevation >= TempMan) then
                MinimumElevation := TempMan;
              Write(F_File, TempMan);
            end;
          end;
          CloseFile(F_File);
          if (IoResult <> 0) then
            DoMessages(30064);
        end else DoMessages(30065);
      end;
    end else begin

  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
    case Intensity of
      0: begin
          MtnRowY := 47;
          MtnZ := 10.0;
        end;
      1: begin
          MtnRowY := 119;
          MtnZ := 5.0;
        end;
      2: begin
          MtnRowY := 479;
          MtnZ := 1.0;
        end;
    end; {Case}
          FileSizeX := maxcolx + 1;{For maybe next time}
          FileSizeY := MtnRowY + 1;
    Randomize;
{Set up array... for next one}
      SetLength(ManMatrix, FYImageX, FYImageY);
      for N := 0 to 16 do begin
        F[N] := ((EXP(N * ln(Fune))) * 3.14 / maxcolx);
        A[N] := EXP((D - 2) * ln(F[N]));
        PH[N] := 6.28 * Random;
        TH[N] := N + 2 * Random;
        CS[N] := COS(TH[N]);
        SN[N] := SIN(TH[N]);
      end;
      Z := 0.0;
      HS := 0.0;
      for I := 0 to maxcolx do HM[I] := 0.0;
        {M_x := 20.0; }{ Magnification ratio ... see below }
        {Plot_Angle  := 90.0;}{ >90 makes steep walls of SIN <90 makes shallow}
      View_Angle := 2.0; {3.0; steep}
        {2.0  Makes an angle of 30 degrees}
      Illum_Angle := 0.5; {0.3;} {0.5  Makes an angle of 30 degrees}
     { V and I would make the plot location need to be changed}
{     str(View_Angle:5:2,s);str(Illum_Angle:5:2,ss);}
      str(Fune: 5: 2, s); str(D: 5: 2, ss); {str(Intensity,iss);}
{   str(A[0]:5:2,s);str(A[16]:5:2,ss);}{str(Intensity,iss);}
      TextOut(10, 10, 'Fractal "roughness" = ' + s +
        ',Dimension "range" ' + ss {+ ', Intensity = '+iss});
{ LOOP}
      for Zcount := 0 to MtnRowY {120} do begin
        X := 0;
        for Xcount := 0 to maxcolx do begin
          Y := 0;
          for N := 1 to 16 do
          begin
            Y := Y + PH[N] + (A[N] * SIN(F[N] * (X * CS[N] + Z *
              SN[N])));
          end;
{record array}
          ManMatrix[Xcount, Zcount] := round(Y);
          H := Y + (Z / View_Angle);
          HS := HS - Illum_Angle;
          if H < HS then C := 5 {green}
          else
          begin
            C := 10; {purple}
            HS := H;
          end;
          if Y < -20 then
          begin
            c := 1; {blue water}
            H := (z / View_Angle) - 20;
              {20;  goes with View 2.0, Illum 0.5}
          end;
          if H > HM[Xcount] then
          begin
            HM[Xcount] := H;  {Place on screen }
            Y_Height := ((maxrowy - round(H)));
            TempColor := RGB(RGBArray[0, C], RGBArray[1, C], RGBArray[2, C]);
            Pixels[Xcount, Y_Height] := TempColor;
          end;
          X := X + 1.0;
        end; {  NEXT X}
        HS := 0;
        Z := Z + MtnZ; {15}
      end; { NEXT Z}
    end; end;
end; { of Procedure Fra_Mtns(Fun, D : Extended);}
(*************************************************************)
(*************************************************************)
               {Tin Mountain}
procedure FractalGrid(Fun, D : Extended);
var
  N, maxcolx, maxrowy,  XCount, YCount, TempMan : Integer;
  F_File: file of Smallint;
  Y: Extended;
  F, A, PH, TH, CS, SN: array[0..16] of Extended;
begin
  {Save LAST array made to FractalFileName}
  if (bSaveTheMountains) then
  begin
      begin
        MainForm.HintPanel.Caption := 'Saving Mountains to disk';
        Application.ProcessMessages;
        FractalFileMatrix := ChangeFileExt(FractalFilename, '.BIN');
        AssignFile(F_File, FractalFileMatrix);
        ReWrite(F_File);
        if IoResult = 0 then
        begin
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              Write(F_File, ManMatrix[Xcount, Ycount]);
            end;
          end;
          CloseFile(F_File);
          if (IoResult <> 0) then
            DoMessages(30064);
        end else DoMessages(30065);
      end;
  end else
  begin
    maxcolx:=  FileSizeX- 1;
    maxrowy := FileSizeY- 1;
    Randomize;
    {Set up array... for next one}
    SetLength(ManMatrix, FileSizeX, FileSizeY);
      for N := 0 to 16 do begin
        F[N] := ((EXP(N * ln(Fun))) * 3.14 / maxcolx);
        A[N] := EXP({-0.8}(D - 2) * ln(F[N]));
        PH[N] := 6.28 * Random;
        TH[N] := N + 2 * Random;
        CS[N] := COS(TH[N]);
        SN[N] := SIN(TH[N]);
      end;
{ LOOP}
      for Ycount := 0 to maxrowy  do begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 50);
            Application.ProcessMessages;
        for Xcount := 0 to maxcolx do begin
          Y := 0;
          for N := 1 to 16 do
          begin
            Y := Y + PH[N] +
                 (A[N] * SIN(F[N]
                  * (Xcount * CS[N] + Ycount *    SN[N])));
          end;
{record array}
          ManMatrix[Xcount, Ycount] := round(Y);
          end;
          end;
{Gather results}
          MaximumElevation := -2147483647;
          MinimumElevation := 2147483646;
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=  50+
            Round((Ycount / maxrowy) * 50);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              TempMan:=ManMatrix[Xcount, Ycount];
              if (MaximumElevation <= TempMan) then
                MaximumElevation := TempMan;
              if (MinimumElevation >= TempMan) then
                MinimumElevation := TempMan;
            end;
          end;
  end;
    XYZ3DForm.MtnsProgressBar.Position :=0;
    Application.ProcessMessages;
end;

(*************************************************************)
 {John Olsson}{johol@lysator.liu.se}
 {Process to Intensity level Krypton 20000}
procedure FoldedGlobe(Krypton : Integer);
var
  F_File: file of Smallint;
  Twister:Boolean;
  {XCount2, YCount2,I,}XCount2,
  Gamma,Omega, Slider,{Slideto,}Quartered,Twisted,
  MaxZDo, maxcolx, maxrowy,  XCount, YCount, TempMan : Integer;
  SinPI: array of double;
  Alpha,Beta, TanB,
  YRangeDiv2, YRangeDivPI,XRangeDiv2, XRangeDivPI: Double;
begin
  {Save LAST array made to FractalFileName}
  if (bSaveTheMountains) then
  begin
      begin
        MainForm.HintPanel.Caption := 'Saving Mountains to disk';
        Application.ProcessMessages;
        FractalFileMatrix := ChangeFileExt(FractalFilename, '.BIN');
        AssignFile(F_File, FractalFileMatrix);
        ReWrite(F_File);
        if IoResult = 0 then
        begin
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              Write(F_File, ManMatrix[Xcount, Ycount]);
            end;
          end;
          CloseFile(F_File);
          if (IoResult <> 0) then
            DoMessages(30064);
        end else DoMessages(30065);
      end;
  end else
  begin
    maxcolx:=  FileSizeX- 1;
    maxrowy := FileSizeY- 1;
    Randomize;
    {Set up array... for next one}
    SetLength(ManMatrix, FileSizeX, FileSizeY);
    SetLength(SinPI, (2*FileSizeX));

    { LOOP}
    XYZ3DForm.MtnsProgressBar.Position :=10;
    Application.ProcessMessages;
{Fill Array}
XCount2:=Random(Krypton div (maxrowy*maxcolx));
    for Ycount := 0 to maxrowy do
    for Xcount := 0 to maxcolx do begin
    ManMatrix[Xcount, Ycount] :=
        round(XCount2 * (Random -Random));
    end;

    for Xcount := 0 to maxcolx do
    begin
    YRangeDiv2:= sin(Xcount*2*PI/FileSizeX );
    SinPI[Xcount] := YRangeDiv2;
    SinPI[Xcount+FileSizeX] :=  YRangeDiv2;
    end;
    YRangeDiv2:= FileSizeY/2;
    YRangeDivPI:= FileSizeY/PI;
    XRangeDiv2:= FileSizeX / 2;
    XRangeDivPI:= FileSizeX/PI;
      Quartered:=Round((FileSizeX / 4));
XCount2:=Round(FileSizeX / 2);
{Process to Intensity level Krypton 20000}
    For MaxZDo:= 0 to Krypton  do
    begin
      XYZ3DForm.MtnsProgressBar.Position :=
            Round((MaxZDo / Krypton) * 100);
      Application.ProcessMessages;
      Alpha:= Random*PI;
      Beta:=  Random*PI;
      Slider:= Random(Quartered);
{      Slideto:=XCount2+Slider;}
      Gamma:= Round( (XRangeDiv2)-(XRangeDivPI)*Beta);
      TanB:= tan(arccos(cos(Alpha)*cos(Beta)));
      If (random <{random} 0.5) then Twister:=True else Twister:=False;
      If (random <{random} 0.5) then Twisted:=-1 else Twisted:=1;
     for Xcount := {Slider to Slideto do }0 to XCount2 do
      begin
        Omega:= Round((YRangeDivPI
                      *arctan(SinPI[Gamma-Xcount+FileSizeX{-Slider}] * TanB))
                      +YRangeDiv2);
{If  (Omega<0) then showmessage('Omega small');
If (Omega>maxrowy) then showmessage('Omega big');}
If ( (Omega>=0) and (Omega <FileSizeY) and
 (Xcount>=0) and (Xcount <FileSizeX) ) then
begin
        If Twister then
        begin
          ManMatrix[Xcount+Slider, Omega] :=
          Round(ManMatrix[Xcount+Slider, Omega]+Twisted);

{          ManMatrix[Xcount, Omega] :=
            ManMatrix[Xcount, Omega]+1;}
        end else
        begin
{          ManMatrix[Xcount, Omega] :=
            ManMatrix[Xcount, Omega]-1;}
{        If ( (Xcount+XCount2-1) < FilesizeX) then
          ManMatrix[(Xcount+XCount2-1), Omega] :=
          Round(ManMatrix[(Xcount+XCount2-1), Omega]+Twisted);}

{          ManMatrix[Xcount, Omega] :=
            ManMatrix[Xcount, Omega]+Twisted;}

{          ManMatrix[Xcount, Omega] :=ManMatrix[Xcount, Omega]-1;}

(*          ManMatrix[Xcount{+Quartered-1}, Omega] :=
          Round(ManMatrix[Xcount{+Quartered-1}, Omega]+Twisted);*)

          If ((Xcount<Slider))then
          ManMatrix[(Xcount{-Slider}), Omega] :=
          Round(ManMatrix[(Xcount{-Slider}), Omega]+Twisted);

          If  (((Quartered+Xcount)+Slider)<FileSizex)then
          ManMatrix[(Quartered+Xcount)+Slider, Omega] :=
          Round(ManMatrix[(Quartered+Xcount)+Slider, Omega]+Twisted);
        end;
end;
      end;
    end;
{Gather results}
          MaximumElevation := -2147483647;
          MinimumElevation := 2147483646;
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              TempMan:=ManMatrix[Xcount, Ycount];
              if (MaximumElevation <= TempMan) then
                MaximumElevation := TempMan;
              if (MinimumElevation >= TempMan) then
                MinimumElevation := TempMan;
            end;
          end;
    SetLength(SinPI, 0);
  end;
    XYZ3DForm.MtnsProgressBar.Position :=0;
    Application.ProcessMessages;
end;

(*************************************************************)
procedure DupliGlobe(Krypton : Integer);
var
  F_File: file of Smallint;
  Twister:Boolean;
  XCount2, Gamma, Omega, {Quartered,}
  MaxZDo, maxcolx, maxrowy,  XCount, YCount, TempMan : Integer;
  Alpha,Beta, TanB,
  YRangeDiv2, YRangeDivPI, XRangeDiv2, XRangeDivPI: Double;
  SinPI: array of double;
begin
  {Save LAST array made to FractalFileName}
  if (bSaveTheMountains) then
  begin
      begin
        MainForm.HintPanel.Caption := 'Saving Mountains to disk';
        Application.ProcessMessages;
        FractalFileMatrix := ChangeFileExt(FractalFilename, '.BIN');
        AssignFile(F_File, FractalFileMatrix);
        ReWrite(F_File);
        if IoResult = 0 then
        begin
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              Write(F_File, ManMatrix[Xcount, Ycount]);
            end;
          end;
          CloseFile(F_File);
          if (IoResult <> 0) then
            DoMessages(30064);
        end else DoMessages(30065);
      end;
  end else
  begin
    maxcolx:=  FileSizeX- 1;
    maxrowy := FileSizeY- 1;
    Randomize;
    {Set up array... }
    SetLength(ManMatrix, FileSizeX, FileSizeY);
    SetLength(SinPI, (2*FileSizeX));
    XYZ3DForm.MtnsProgressBar.Position :=10;
    Application.ProcessMessages;
{Fill Array}
XCount2:=Random(Krypton div (maxrowy*maxcolx));
    for Ycount := 0 to maxrowy do
    for Xcount := 0 to maxcolx do begin
    ManMatrix[Xcount, Ycount] :=
        round(XCount2 * (Random -Random));
    end;

    for Xcount := 0 to maxcolx do
    begin
    YRangeDiv2:= sin(Xcount*2*PI/FileSizeX );
    SinPI[Xcount] := YRangeDiv2;
    SinPI[Xcount+FileSizeX] :=  YRangeDiv2;
    end;
    YRangeDiv2:= FileSizeY/2;
    YRangeDivPI:= FileSizeY/PI;
    XRangeDiv2:= FileSizeX / 2;
    XRangeDivPI:= FileSizeX/PI;
{    Quartered:=Round((FileSizeX / 4));}
    XCount2:=Round(FileSizeX / 2);
{Process to Intensity level}
    For MaxZDo:= 0 to Krypton  do
    begin
      XYZ3DForm.MtnsProgressBar.Position :=
            Round((MaxZDo / Krypton) * 100);
      Application.ProcessMessages;
      Alpha:= Random*PI;
      Beta:=  Random*PI;
      Gamma:= Round( (XRangeDiv2)-(XRangeDivPI)*Beta);
      TanB:= tan(arccos(cos(Alpha)*cos(Beta)));
      If (random <{random} 0.5) then Twister:=True else Twister:=False;
     for Xcount := 0 to XCount2 do
      begin
        Omega:= Round((YRangeDivPI
                      *arctan(SinPI[Gamma-Xcount+FileSizeX] * TanB))
                      +YRangeDiv2);
If ( (Omega>=0) and (Omega <FileSizeY) and
 (Xcount>=0) and (Xcount <FileSizeX) ) then
begin
        If Twister then
        begin
          ManMatrix[Xcount, Omega] :=
          Round(ManMatrix[Xcount, Omega]+1);
        end else
        begin
          ManMatrix[Xcount, Omega] :=
          Round(ManMatrix[Xcount, Omega]-1);
          end;
end;
      end;
    end;
{Duplicate}
    XCount2:=Round(FileSizeX / 2);
    for Ycount := 0 to maxrowy do
    begin
    Application.ProcessMessages;
    for Xcount := 0 to XCount2 do
    begin
      If ((Xcount+XCount2>=0)
      and (Xcount+XCount2 <FileSizeX) ) then
           ManMatrix[Xcount+XCount2, Ycount] :=
           ManMatrix[Xcount, Ycount];
    end;
    end;
{Gather results}
          MaximumElevation := -2147483647;
          MinimumElevation := 2147483646;
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              TempMan:=ManMatrix[Xcount, Ycount];
              if (MaximumElevation <= TempMan) then
                MaximumElevation := TempMan;
              if (MinimumElevation >= TempMan) then
                MinimumElevation := TempMan;
            end;
          end;
    SetLength(SinPI, 0);
  end;
    XYZ3DForm.MtnsProgressBar.Position :=0;
    Application.ProcessMessages;
end;

(*************************************************************)
{Random sin..cos wave craters of fractal (random various) sizes
from GRIP Filters Rough texture...}
             {R multiplier of sin..cos cones
             G width random 3 7 11 rest are little shallow
             I intensity times file width=repititions}
{Process to Intensity level   r g i       Loop
phingphactor:= r 1.4  Gradient DeptH, 9  Intensity,36 :}
procedure Craters(phingphactor:Double; DeptH, Intensity : Integer);
var
  UpDown:Boolean;
  X, Z, MaxZDo, maxcolx, maxrowy,  XCount, YCount, TempMan : Integer;
  Decider, Totaler, R, Y: Extended;
  F_File: file of Smallint;
begin
  {Save LAST array made to FractalFileName}
  if (bSaveTheMountains) then
  begin
      begin
        MainForm.HintPanel.Caption := 'Saving Mountains to disk';
        Application.ProcessMessages;
        FractalFileMatrix := ChangeFileExt(FractalFilename, '.bin');
        AssignFile(F_File, FractalFileMatrix);
        ReWrite(F_File);
        if IoResult = 0 then
        begin
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              Write(F_File, ManMatrix[Xcount, Ycount]);
            end;
          end;
          CloseFile(F_File);
          if (IoResult <> 0) then
            DoMessages(30064);
        end else DoMessages(30065);
      end;
  end else
  begin
    maxcolx:=  FileSizeX- 1;
    maxrowy := FileSizeY- 1;
    Randomize;
    {Set up array... for next one}
    SetLength(ManMatrix, FileSizeX, FileSizeY);
    XYZ3DForm.MtnsProgressBar.Position :=10;
    Application.ProcessMessages;
{Fill Array}
    for Ycount := 0 to maxrowy do begin
    for Xcount := 0 to maxcolx do begin
    ManMatrix[Xcount, Ycount] :=
    round(Random(Intensity) * (Random -Random));
    end;end;
{Process to Intensity level   r g i       Loop
phingphactor:= r 1.4  Gradient DeptH, 9  Intensity,36 :}
    For MaxZDo:= 0 to maxcolx*Intensity  do
    begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((MaxZDo / maxcolx*Intensity) * 100);
            Application.ProcessMessages;
            Xcount:= random(maxcolx);
            Ycount:= random(maxrowy);

{Place filters here}
Decider:=Random(DeptH);
If (Random > 0.5)then UpDown:=True else UpDown:=False;
  Totaler := 0;
  if (Decider <3) then
        begin
          for X := 0 to 8 do begin
            for Z := 0 to 8 do begin
              R := SQRT(sqr(X - 4) + sqr(Z - 4)) * phingphactor;
              if ((X = 4) and (Z = 4)) then Y := 0 else
              begin
                If UpDown then  Y := (SIN(R) / R)
                else Y := (COS(R) / R);
                Totaler := Totaler + Y;
              end;
              if (( Xcount < (maxcolx - X))
              and (Ycount < (maxrowy - Z))) then
              Manmatrix[Xcount+X, Ycount+Z] :=
              Manmatrix[Xcount+X, Ycount+Z]+Round(Y);
            end; end;
              if (( Xcount < (maxcolx - 4))
              and (Ycount < (maxrowy - 4))) then
           Manmatrix[Xcount+4, Ycount+4]:=
           Manmatrix[Xcount+4, Ycount+4]+Round(Totaler);
        end  else
    if (Decider<7) then
      begin
        for X := 0 to 6 do begin
          for Z := 0 to 6 do begin
            R := SQRT(sqr(X - 3) + sqr(Z - 3)) * phingphactor;
            if ((X = 3) and (Z = 3)) then Y := 0 else
            begin
              If UpDown then  Y := (SIN(R) / R)
              else Y := (COS(R) / R);
              Totaler := Totaler + Y;
            end;
              if (( Xcount < (maxcolx - X))
              and (Ycount < (maxrowy - Z))) then
              Manmatrix[Xcount+X, Ycount+Z] :=
              Manmatrix[Xcount+X, Ycount+Z]+Round(Y);
            end; end;
              if (( Xcount < (maxcolx - 3))
              and (Ycount < (maxrowy - 3))) then
           Manmatrix[Xcount+3, Ycount+3]:=
           Manmatrix[Xcount+3, Ycount+3]+Round(Totaler);
      end    else
      if (Decider<11) then
    begin
      for X := 0 to 4 do begin
        for Z := 0 to 4 do begin
          R := SQRT(sqr(X - 2) + sqr(Z - 2)) * phingphactor;
          if ((X = 2) and (Z = 2)) then Y := 0 else begin
        If UpDown then  Y := (SIN(R) / R)
          else Y := (COS(R) / R);
            Totaler := Totaler + Y;
          end;
              if (( Xcount < (maxcolx - X))
              and (Ycount < (maxrowy - Z))) then
              Manmatrix[Xcount+X, Ycount+Z] :=
              Manmatrix[Xcount+X, Ycount+Z]+Round(Y);
            end; end;
              if (( Xcount < (maxcolx - 2))
              and (Ycount < (maxrowy - 2))) then
           Manmatrix[Xcount+2, Ycount+2]:=
           Manmatrix[Xcount+2, Ycount+2]+Round(Totaler);
    end  else {9}
  begin
    for X := 0 to 2 do begin
      for Z := 0 to 2 do begin
        R := SQRT(sqr(X - 1) + sqr(Z - 1)) * phingphactor;
        if ((X = 1) and (Z = 1)) then Y := 0 else
        begin
        If UpDown then  Y := (SIN(R) / R)
          else Y := (COS(R) / R);
          Totaler := Totaler + Y;
        end;
              if (( Xcount < (maxcolx - X))
              and (Ycount < (maxrowy - Z))) then
              Manmatrix[Xcount+X, Ycount+Z] :=
              Manmatrix[Xcount+X, Ycount+Z]+Round(Y);
            end; end;
              if (( Xcount < (maxcolx - 1))
              and (Ycount < (maxrowy - 1))) then
           Manmatrix[Xcount+1, Ycount+1]:=
           Manmatrix[Xcount+1, Ycount+1]+Round(Totaler);
  end
  end;

{Gather results}
          MaximumElevation := -2147483647;
          MinimumElevation := 2147483646;
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              TempMan:=ManMatrix[Xcount, Ycount];
              if (MaximumElevation <= TempMan) then
                MaximumElevation := TempMan;
              if (MinimumElevation >= TempMan) then
                MinimumElevation := TempMan;
            end;
          end;
  end;
    XYZ3DForm.MtnsProgressBar.Position :=0;
    Application.ProcessMessages;
end;
(*************************************************************)
{Water Demo Cones Author : Jason Allen  Email : jra101@home.com}
procedure Waves( DeptH, Intensity : Integer);
var
  F_File: file of Smallint;
  MaxZDo, maxcolx, maxrowy,  XCount, YCount, TempMan : Integer;
  u, v ,  sqrx, sqry, sqrw : Integer;
  Synchronocity:Double;
begin
  {Save LAST array made to FractalFileName}
  if (bSaveTheMountains) then
  begin
      begin
        MainForm.HintPanel.Caption := 'Saving Mountains to disk';
        Application.ProcessMessages;
        FractalFileMatrix := ChangeFileExt(FractalFilename, '.BIN');
        AssignFile(F_File, FractalFileMatrix);
        ReWrite(F_File);
        if IoResult = 0 then
        begin
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              Write(F_File, ManMatrix[Xcount, Ycount]);
            end;
          end;
          CloseFile(F_File);
          if (IoResult <> 0) then
            DoMessages(30064);
        end else DoMessages(30065);
      end;
  end else
  begin
    maxcolx:=  FileSizeX- 1;
    maxrowy := FileSizeY- 1;
    Randomize;
    {Set up array... for next one}
    SetLength(ManMatrix, FileSizeX, FileSizeY);
    XYZ3DForm.MtnsProgressBar.Position :=10;
    Application.ProcessMessages;
{Fill Array}
    for Ycount := 0 to maxrowy do begin
    for Xcount := 0 to maxcolx do begin
    ManMatrix[Xcount, Ycount] :=
    round(Random(Intensity) * (Random -Random));
    end;end;
    For MaxZDo:= 0 to maxcolx*Intensity  do
    begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((MaxZDo / (maxcolx*Intensity)) * 100);
            Application.ProcessMessages;
            Xcount:= random(maxcolx);
            Ycount:= random(maxrowy);
Synchronocity:=(Random-Random);
(* //  Name : Water Demo
//  Author : Jason Allen//  Email : jra101@home.com
// Name       : PutDrop
// Purpose    : Starts a droplet in the wavemap.
//This is done by creating a
//"cone" around the x,y coordinates specified. This "cone" is
//then averaged every redraw, creating the ripple effect.
// Parameters :
//  x,y - coordinates of the grid point to start to droplet
//   w - splash height
// MulFactor - splash strength
// Returns    : none
procedure PutDrop(x , y, w, MulFactor : Integer);
var
  u, v : Integer;   // Loop counters
  sqrx, sqry, sqrw : Integer;
x=  Xcount
y= Ycount
begin*)
{Process to Intensity level
 DeptH, 22 Intensity,10}
//   w:=DeptH;  splash height   22 not his 4
 //   MulFactor:=Intensity; splash strength  Sync used not his -22
{  PutDrop(x, y, 4 ,-22);}
  sqrw := sqr(DeptH);

    // Create the initial "cone"
    for v := Ycount - DeptH to Ycount + DeptH do
    begin
      sqry := sqr(v - Ycount);
      for u := Xcount - DeptH to Xcount + DeptH do
      begin
        sqrx := sqr(u-Xcount);
        if (sqrx + sqry) <= sqrw then
        begin
        // Boundary checking, make sure we don't try to put
        //a drop next to an edge
          if (u > -1) and ( u < (FileSizeX ))
            and ( v > -1) and (v < (FileSizeY )) then
          begin
            ManMatrix[u, v] :=
              (ManMatrix[u, v]+
               Round(Synchronocity *(DeptH - sqrt(sqrx + sqry))));
          end;
        end;
      end;
    end;
    end;{of loop}

{Gather results}
          MaximumElevation := -2147483647;
          MinimumElevation := 2147483646;
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              TempMan:=ManMatrix[Xcount, Ycount];
              if (MaximumElevation <= TempMan) then
                MaximumElevation := TempMan;
              if (MinimumElevation >= TempMan) then
                MinimumElevation := TempMan;
            end;
          end;
  end;
    XYZ3DForm.MtnsProgressBar.Position :=0;
    Application.ProcessMessages;
end;
(*************************************************************)
(*************************************************************)
procedure Rippled(RadiusG, DeptH, Intensity : Integer);
var
  F_File: file of Smallint;
   XCount2, YCount2,  u, v ,  sqrx, sqry, sqrw,MaxZDo2,{MaxZDo3,}n,
  MaxZDo, maxcolx, maxrowy,  XCount, YCount, TempMan : Integer;
  Synchronocity:Double;
begin
  {Save LAST array made to FractalFileName}
  if (bSaveTheMountains) then
  begin
      begin
        MainForm.HintPanel.Caption := 'Saving Mountains to disk';
        Application.ProcessMessages;
        FractalFileMatrix := ChangeFileExt(FractalFilename, '.BIN');
        AssignFile(F_File, FractalFileMatrix);
        ReWrite(F_File);
        if IoResult = 0 then
        begin
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              Write(F_File, ManMatrix[Xcount, Ycount]);
            end;
          end;
          CloseFile(F_File);
          if (IoResult <> 0) then
            DoMessages(30064);
        end else DoMessages(30065);
      end;
  end else
  begin
    maxcolx:=  FileSizeX- 1+2;
    maxrowy := FileSizeY- 1+2;
    Randomize;
    {Set up array... 2 larger to hold filter}
    SetLength(ManMatrix, FileSizeX+2, FileSizeY+2);
    XYZ3DForm.MtnsProgressBar.Position :=10;
    Application.ProcessMessages;
{Fill Array}
    for Ycount := 0 to maxrowy do begin
    for Xcount := 0 to maxcolx do begin
    ManMatrix[Xcount, Ycount] :=
    round(Random(Intensity) * (Random -Random));
    end;end;
    For MaxZDo:= 0 to maxcolx  do begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((MaxZDo / (maxcolx)) * 100);
            Application.ProcessMessages;
    For MaxZDo2:= 0 to Intensity  do
    begin
            Application.ProcessMessages;
            Xcount:= random(maxcolx);
            Ycount:= random(maxrowy);
Synchronocity:=(Random-Random);{they got too high?...}
{Process to Intensity level
 DeptH, 22 Intensity,10}
//   w:=DeptH;  splash height
 //   MulFactor:=Intensity; splash strength
  sqrw := sqr(DeptH);
    // Create the initial "cone"
    for v := Ycount - DeptH to Ycount + DeptH do
    begin
      sqry := sqr(v - Ycount);
      for u := Xcount - DeptH to Xcount + DeptH do
      begin
        sqrx := sqr(u-Xcount);
        if (sqrx + sqry) <= sqrw then
        begin
        // Boundary checking, make sure we don't try to put
        //a drop next to an edge
          if (u > -1) and ( u < (FileSizeX ))
            and ( v > -1) and (v < (FileSizeY )) then
          begin
            ManMatrix[u, v] :=
              (ManMatrix[u, v]+
               Round(Synchronocity
               *(DeptH - sqrt(sqrx + sqry))));
          end;
        end;
      end;
    end;
    end;{loop intnesity}

    {R smoothers}
{    For MaxZDo3:= 0 to RadiusG  do}
If ( Random(100) <= RadiusG)then
    begin
    for YCount2 := 1 to maxrowy-1 do
    begin
    Application.ProcessMessages;
    for XCount2 := 1 to maxcolx-1 do
    begin
    // Calculate the current cell's new value
    // based on the surrounding cell's
      n := ( (ManMatrix[XCount2-1,YCount2] +
{              ManMatrix[XCount2,YCount2]+}
             ManMatrix[XCount2+1,YCount2] +
{             ManMatrix[XCount2-1,YCount2-1] +}
             ManMatrix[XCount2,YCount2-1]+
{             ManMatrix[XCount2+1,YCount2-1] +
             ManMatrix[XCount2-1,YCount2+1] +}
             ManMatrix[XCount2,YCount2+1]
{           +  ManMatrix[XCount2+1,YCount2+1]} ) div 2{8})
              - ManMatrix[XCount2,YCount2];
      // Equivalent to decrementing n by n divided by 2^DAMP,
      // this decreases the value in the cell,
      //making the waves fade out by Water damping amount
      // DAMP                = 4;
      Dec(n,Smallint(n shr 4));
      // Set new value into the wavemap
    ManMatrix[Xcount2, Ycount2] := n;
    end;end;
    end;{of loop 2}

    end;{of loop columns}
{Replace matrix to real size, reset variable sizes}
    for YCount2 := 1 to maxrowy-1 do
    begin
      Application.ProcessMessages;
    for XCount2 := 1 to maxcolx-1 do
    begin
      ManMatrix[Xcount2-1, Ycount2-1] :=
        ManMatrix[Xcount2, Ycount2];
    end;
    end;
    SetLength(ManMatrix, FileSizeX, FileSizeY);
{Gather results}
          MaximumElevation := -2147483647;
          MinimumElevation := 2147483646;
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              TempMan:=ManMatrix[Xcount, Ycount];
              if (MaximumElevation <= TempMan) then
                MaximumElevation := TempMan;
              if (MinimumElevation >= TempMan) then
                MinimumElevation := TempMan;
            end;
          end;
  end;
    XYZ3DForm.MtnsProgressBar.Position :=0;
    Application.ProcessMessages;
end;

(*************************************************************)

(*************************************************************)
procedure Splattered(RadiusG, DeptH, Intensity : Integer);
var
  F_File: file of Smallint;
  MaxZDo, maxcolx, maxrowy,  XCount, YCount, TempMan : Integer;
  u, v ,  sqrx, sqry, sqrw : Integer;
  Synchronocity:Double;
begin
  {Save LAST array made to FractalFileName}
  if (bSaveTheMountains) then
  begin
      begin
        MainForm.HintPanel.Caption := 'Saving Mountains to disk';
        Application.ProcessMessages;
        FractalFileMatrix := ChangeFileExt(FractalFilename, '.BIN');
        AssignFile(F_File, FractalFileMatrix);
        ReWrite(F_File);
        if IoResult = 0 then
        begin
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              Write(F_File, ManMatrix[Xcount, Ycount]);
            end;
          end;
          CloseFile(F_File);
          if (IoResult <> 0) then
            DoMessages(30064);
        end else DoMessages(30065);
      end;
  end else
  begin
    maxcolx:=  FileSizeX- 1;
    maxrowy := FileSizeY- 1;
    Randomize;
    {Set up array... for next one}
    SetLength(ManMatrix, FileSizeX, FileSizeY);
    XYZ3DForm.MtnsProgressBar.Position :=10;
    Application.ProcessMessages;
{Fill Array}
    for Ycount := 0 to maxrowy do begin
    for Xcount := 0 to maxcolx do begin
    ManMatrix[Xcount, Ycount] :=
     round(Random(Intensity) * (Random -Random));
    end;end;
    For MaxZDo:= 0 to maxcolx*Intensity  do
    begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((MaxZDo / (maxcolx*Intensity)) * 100);
            Application.ProcessMessages;
            Xcount:= random(maxcolx);
            Ycount:= random(maxrowy);
Synchronocity:=(Random-Random);{they got too high?...}
{Process to Intensity level
 DeptH, 22 Intensity,10}
//   w:=DeptH;  splash height
 //   MulFactor:=Intensity; splash strength
  sqrw := sqr(DeptH);
    // Create the initial "cone"
    for v := Ycount - DeptH to Ycount + DeptH do
    begin
      sqry := sqr(v - Ycount);
      for u := Xcount - DeptH to Xcount + DeptH do
      begin
        sqrx := sqr(u-Xcount);
        if (sqrx + sqry) <= sqrw then
        begin
        // Boundary checking, make sure we don't try to put
        //a drop next to an edge
          if (u > -1) and ( u < (FileSizeX ))
            and ( v > -1) and (v < (FileSizeY )) then
          begin
            ManMatrix[u, v] :=
              (ManMatrix[u, v]+
               Round(Synchronocity
               *(DeptH - sqrt(sqrx + sqry))));
          end;
        end;
      end;
    end;
    end;{of loop}
    {R more cones}
    For MaxZDo:= 0 to RadiusG  do
    begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((MaxZDo / (RadiusG)) * 100);
            Application.ProcessMessages;
            Xcount:= random(maxcolx);
            Ycount:= random(maxrowy);
Synchronocity:=RadiusG*(Random-Random);{they got too high?...}
  sqrw := sqr(DeptH);
    // Create the initial "cone"
    for v := Ycount - DeptH to Ycount + DeptH do
    begin
      sqry := sqr(v - Ycount);
      for u := Xcount - DeptH to Xcount + DeptH do
      begin
        sqrx := sqr(u-Xcount);
        if (sqrx + sqry) <= sqrw then
        begin
        // Boundary checking, make sure we don't try to put
        //a drop next to an edge
          if (u > -1) and ( u < (FileSizeX ))
            and ( v > -1) and (v < (FileSizeY )) then
          begin
            ManMatrix[u, v] :=
              (ManMatrix[u, v]+
               Round(Synchronocity
               *(DeptH - sqrt(sqrx + sqry))));
          end;
        end;
      end;
    end;
    end;{of loop 2}

{Gather results}
          MaximumElevation := -2147483647;
          MinimumElevation := 2147483646;
          maxcolx:=  FileSizeX- 1;
          maxrowy := FileSizeY- 1;
          for Ycount := 0 to maxrowy do
          begin
            XYZ3DForm.MtnsProgressBar.Position :=
            Round((Ycount / maxrowy) * 100);
            Application.ProcessMessages;
            for Xcount := 0 to maxcolx do
            begin
              TempMan:=ManMatrix[Xcount, Ycount];
              if (MaximumElevation <= TempMan) then
                MaximumElevation := TempMan;
              if (MinimumElevation >= TempMan) then
                MinimumElevation := TempMan;
            end;
          end;
  end;
    XYZ3DForm.MtnsProgressBar.Position :=0;
    Application.ProcessMessages;
end;

(*************************************************************)

(*************************************************************)

(*************************************************************)
{Grow along bottom of screen}

procedure DLA(Intensity: Integer);
  {(Freq,F_Dim: Extended;Intensity:Integer)}
var
  TempColor: TColor;
  Contact: Boolean;
  X, Y, HighestHigh, {CurrentX,CurrentY,} spacer,
    ICount, maxcolx, maxrowy: Integer;
  HighS, Xs, Ys: string;
begin
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
    HighestHigh := (FYImageY - 5);
{    Brush.Color:=FBackGroundColor;
    Brush.Style:=bsSolid;
    FillRect(Rect(0,0,FYImageX,FYImageY));
    TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color:= TempColor;}
    {Set up bottom line glue}
{    Moveto(0,maxrowy);
    Lineto(maxcolx,maxrowy);  }
{Set up multi color line}
    for X := 0 to maxcolx do begin
      TempColor := RGB(Colors[0, (Random(255) mod 255)],
        Colors[1, (Random(255) mod 255)],
        Colors[2, (Random(255) mod 255)]);
      Pixels[X, maxrowy] := TempColor;
    end;
{    MainForm.Show;}
    Randomize;
    bRotateImage := False; {in the Drawing...}
    bRotatingImage := True;
    maxcolx := (FYImageX - 3);
    X := random(maxcolx) + 1;
    Y := HighestHigh; {Start from HighestHigh + 3}
    str(X, Xs); str(Y, Ys); str(HighestHigh, HighS);
    MainForm.HiddenFX.Caption := 'FX: ' + Xs + ', FY: ' + Ys +
      ',  High: ' + HighS;
    Application.ProcessMessages;
    repeat
      for Spacer := 1 to 100 do begin
        X := random(maxcolx) + 1;
        Y := HighestHigh; {Start from HighestHigh + 3}
{Y:= (Random((maxrowy-(HighestHigh-2)))+HighestHigh);}
        if Y > maxrowy then Y := HighestHigh;
        Contact := True;
        ICount := 0;
        while Contact do begin {Drop bombers}
          if (FBackGroundColor <> Pixels[X + 1, Y + 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X + 1, Y + 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X - 1, Y + 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X - 1, Y + 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X, Y + 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X, Y + 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X + 1, Y]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X + 1, Y];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X - 1, Y]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X - 1, Y];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X + 1, Y - 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X + 1, Y - 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X - 1, Y - 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X - 1, Y - 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X, Y - 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X, Y - 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          X := X + (Random(3) - 1);
{Y := Y + (Random(3)-1);}{Keep in bounds}
          Y := Y + (Random(3)); {Keep in bounds.. always dropping}
          if X > maxcolx then
            X := ((maxcolx div 2) - round((random - random) * 100));
          if X < 1 then
            X := ((maxcolx div 2) + round((random - random) * 100));
          if Y > maxrowy then Y := HighestHigh;
          if Y < HighestHigh - 4 then Y := HighestHigh;
        end; {end of while}
      end; {of spacer}
      str(X, Xs); str(Y, Ys); str(HighestHigh, HighS);
      MainForm.HiddenFX.Caption := 'FX: ' + Xs + ', FY: ' + Ys +
        ',  High: ' + HighS;
      Application.ProcessMessages;
    until ((bRotateImage = True) or (HighestHigh < 7));
  end;
end;
(*************************************************************)
{Grow along bottom of screen}

procedure DLAC(Intensity: Integer);
var
  TempColor: TColor;
  Contact: Boolean;
  X, Y, HighestHigh, {CurrentX,CurrentY,} Spacer,
    ICount, maxcolx, maxrowy: Integer;
  HighS, Xs, Ys: string;
begin
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
    HighestHigh := (FYImageY - 5);
{    Brush.Color:=FBackGroundColor;
    Brush.Style:=bsSolid;
    FillRect(Rect(0,0,FYImageX,FYImageY));
    TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color:= TempColor;}
    {Set up bottom line glue}
    Moveto(0, maxrowy);
    Lineto(maxcolx, maxrowy);
{Set up multi color line}
    for X := 0 to maxcolx do begin
      TempColor := RGB(Colors[0, (Random(255) mod 255)],
        Colors[1, (Random(255) mod 255)],
        Colors[2, (Random(255) mod 255)]);
      Pixels[X, maxrowy] := TempColor;
    end;
{    MainForm.Show;}
    Randomize;
    bRotateImage := False; {in the Drawing...}
    bRotatingImage := True;
    maxcolx := (FYImageX - 3);
    X := random(maxcolx) + 1;
    Y := HighestHigh; {Start from HighestHigh + 3}
    str(X, Xs); str(Y, Ys); str(HighestHigh, HighS);
    MainForm.HiddenFX.Caption := 'FX: ' + Xs + ', FY: ' + Ys +
      ',  High: ' + HighS;
    Application.ProcessMessages;
    repeat
      for Spacer := 1 to 100 do begin
        X := (X mod 6);
        if X = 1 then
          X := ((maxcolx div 2) + round((random - random) * 100))
        else X := random(maxcolx) + 1;
{X:= (maxcolx div 2);}
        Y := HighestHigh; {Start from HighestHigh + 3}
{Y:= (Random((maxrowy-(HighestHigh-2)))+HighestHigh);}
        if Y > maxrowy then Y := HighestHigh;
        Contact := True;
        ICount := 0;
        while Contact do begin {Drop bombers}
          if (FBackGroundColor <> Pixels[X + 1, Y + 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X + 1, Y + 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X - 1, Y + 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X - 1, Y + 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X, Y + 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X, Y + 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X + 1, Y]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X + 1, Y];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X - 1, Y]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X - 1, Y];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X + 1, Y - 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X + 1, Y - 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X - 1, Y - 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X - 1, Y - 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
          if (FBackGroundColor <> Pixels[X, Y - 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then HighestHigh := Y - 3;
              TempColor := Pixels[X, Y - 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
 (*
If (  {TempColor =}
    (FBackGroundColor <> Pixels[X+1,Y+1]) or
    (FBackGroundColor <> Pixels[X-1,Y+1]) or
    (FBackGroundColor <> Pixels[X,Y+1]) or
    (FBackGroundColor <> Pixels[X+1,Y]) or
    (FBackGroundColor <> Pixels[X-1,Y]) or
    (FBackGroundColor <> Pixels[X+1,Y-1]) or
    (FBackGroundColor <> Pixels[X-1,Y-1]) or
    (FBackGroundColor <> Pixels[X,Y-1])
    )  then
        begin  {Contacted so accumulate}
        If Y = HighestHigh Then HighestHigh:=Y-3;
        Pixels[X,Y]:= TempColor;
        Contact:= False;
        end;
*)
{str(X,Xs);str(Y,Ys);
MainForm.HiddenFX.Caption:='FX: '+Xs+', FY: '+Ys;
Application.ProcessMessages;}
          X := X + (Random(3) - 1);
{Y := Y + (Random(3)-1);}{Keep in bounds}
          Y := Y + (Random(3)); {Keep in bounds.. always dropping}
{If X > maxcolx Then X := ((maxcolx div 2)- round((random -random) *100));
If X < 1 Then X := ((maxcolx div 2)+ round((random -random) *100));}
          if X > maxcolx then X := (maxcolx div 2);
          if X < 1 then X := ((maxcolx div 2));
          if Y > maxrowy then Y := HighestHigh;
          if Y < HighestHigh - 4 then Y := HighestHigh;
        end; {end of while}
      end;
      str(X, Xs); str(Y, Ys); str(HighestHigh, HighS);
      MainForm.HiddenFX.Caption := 'FX: ' + Xs + ', FY: ' + Ys +
        ',  High: ' + HighS;
      Application.ProcessMessages;
    until ((bRotateImage = True) or (HighestHigh < 7));
  end;
end;
(*************************************************************)

(*************************************************************)
{Grow at center of screen... make a box and throw at center}

procedure DLACentroid(Intensity: Integer);
  {(Freq,F_Dim: Extended;Points,Intensity:Integer)}
var
  TempColor: TColor;
  Contact: Boolean; Spacer,
  X, Y, HighestHigh, LowestLow, RightSide, LeftSide, Stepcounter,
    ICount, CenterX, CenterY, Centroid, maxcolx, maxrowy: Integer;
  HighS, Xs, Ys: string;
begin
  with MainForm.Image2.Canvas do begin
{    Brush.Color:=FBackGroundColor;
    Brush.Style:=bsSolid;
    FillRect(Rect(0,0,FYImageX,FYImageY));
    TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
    Pen.Color := TempColor;
    Font.Color:= TempColor;
    MainForm.Show;}
    {Set up bottom line glue}
    maxcolx := (10);
    maxrowy := (10);
    CenterX := (FYImageX div 2);
    CenterY := (FYImageY div 2);
{    HighestHigh:=(CenterY+5);
    LowestLow:=(CenterY-5);}
    HighestHigh := (CenterY - 5);
    LowestLow := (CenterY + 5);
    RightSide := (CenterX + 5);
    LeftSide := (CenterX - 5);
{Set up multi color line}
    for X := CenterX - 2 to CenterX + 2 do begin
      for Y := CenterY - 2 to CenterY + 2 do begin
        TempColor := RGB(Colors[0, (Random(255) mod 255)],
          Colors[1, (Random(255) mod 255)],
          Colors[2, (Random(255) mod 255)]);
        Pixels[X, Y] := TempColor;
      end;
    end;
    Randomize;
    bRotateImage := False; {in the Drawing...}
    bRotatingImage := True;
    X := CenterX;
    Y := HighestHigh;
    str(X, Xs); str(Y, Ys); str(HighestHigh, HighS);
    MainForm.HiddenFX.Caption := 'FX: ' + Xs + ', FY: ' + Ys +
      ',  High: ' + HighS;
    Application.ProcessMessages;
    repeat
      for Spacer := 1 to 100 do begin
        ICount := 0;
        Centroid := random(4);
        if Centroid = 0 then {center + (width * random + or -)}
        begin X := LeftSide;
 {    Y:=(CenterY+ ( (random(maxrowy div 2)) *(random(2) -random(2)) )  );}
          Y := (CenterY + ((random(maxrowy div 2)) - (random(maxrowy
            div 2))));

        end else
          if Centroid = 1 then {center + (width * random + or -)}
          begin X := RightSide;
{     Y:=(CenterY+ ( (random(maxrowy div 2)) *(random(2) -random(2))));}
            Y := (CenterY + ((random(maxrowy div 2)) - (random(maxrowy
              div 2))));

          end else
            if Centroid = 2 then {center + (width * random + or -)}
            begin
{     X:=(CenterX + ( (random(maxcolx div 2)) *(random(2) -random(2))));}
              X := (CenterX + ((random(maxcolx div 2)) -
                (random(maxcolx div 2))));
              Y := HighestHigh;
            end else
              if Centroid = 3 then {center + (width * random + or -)}
              begin
                X := (CenterX + ((random(maxcolx div 2)) -
                  (random(maxcolx div 2))));
                Y := LowestLow;
              end else ;{center + (width * random + or -)}
               { showmessage('oops 1');}
        Stepcounter := 0;
        Contact := True;
        while ((Contact) and (Stepcounter < 2000)) do
          begin {Drop bombers}
          inc(Stepcounter);
          case Centroid of {Keep in bounds.. always dropping}
            0: begin {Left}
                X := X + (Random(3) - 1);
                Y := Y + (Random(3) - 1); {  +(Random(2)-Random(2));}
              end;
            1: begin {Right}
                X := X + (Random(3) - 1); {  -(Random(2));}
                Y := Y + (Random(3) - 1); {  +(Random(2)-Random(2));}
              end;
            2: begin {Top}
                X := X + (Random(3) - 1);
                Y := Y + (Random(3) - 1); {+(Random(2));}
              end;
            3: begin {Bottom}
                X := X + (Random(3) - 1);
                Y := Y + (Random(3) - 1); {  -(Random(2));}
              end;
          else ;{showmessage('oops');}
          end; {of case}

          if (X < (LeftSide - 4)) then X := LeftSide;
          if (X > (RightSide + 4)) then X := RightSide;
          if (Y < (HighestHigh - 4)) then Y := HighestHigh;
          if (Y > (LowestLow + 4)) then Y := LowestLow;
{    HighestHigh:=(CenterY-5);
    LowestLow:=(CenterY+5);
    RightSide:=(CenterX+5);
    LeftSide:=(CenterX-5);}
          if (FBackGroundColor <> Pixels[X - 1, Y + 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X - 1, Y + 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X - 1, Y - 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount = Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X - 1, Y - 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X + 1, Y - 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X + 1, Y - 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X + 1, Y + 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X + 1, Y + 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X - 1, Y]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X - 1, Y];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X + 1, Y]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X + 1, Y];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X, Y + 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X, Y + 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end; { else}
          if (FBackGroundColor <> Pixels[X, Y - 1]) then
          begin {Contacted so accumulate}
            inc(ICount); if (ICount >= Intensity) then begin
              if Y = HighestHigh then
                begin HighestHigh := Y - 3; inc(maxrowy, 6); end;
              if Y = LowestLow then
                begin LowestLow := Y + 3; inc(maxrowy, 6); end;
              if X = RightSide then
                begin RightSide := X + 3; inc(maxcolx, 6); end;
              if X = LeftSide then
                begin LeftSide := X - 3; inc(maxcolx, 6); end;
              TempColor := Pixels[X, Y - 1];
              Pixels[X, Y] := TempColor;
              Contact := False;
            end; end;
{str(X,Xs);str(Y,Ys);  str(StepCounter,HighS);
MainForm.HiddenFX.Caption:='FX: '+Xs+', FY: '+Ys+',  Count: '+HighS;
Application.ProcessMessages;}
        end; {end of while}
      end; {of spacer}
      str(X, Xs); str(Y, Ys); {str(HighestHigh,HighS);}
        str(StepCounter, HighS);
      MainForm.HiddenFX.Caption := 'FX: ' + Xs + ', FY: ' + Ys +
        ',  High: ' + HighS;
      Application.ProcessMessages;
    until ((bRotateImage = True)
      or (HighestHigh < 7) or (LowestLow > FYImageY)
      or (RightSide > FYImageX) or (LeftSide < 7));
  end;
end;
(*************************************************************)
(*************************************************************)

(*************************************************************)
(***************************************************)
(***************************************************)

procedure init;
{turns on graphics and creates a cube. Since the rotation routines
 rotate around the origin, I have centered the cube on the origin, so
 that it stays in place and only spins.}
begin
  box[0].x := -75; box[0].y := -75; box[0].z := -75;
  box[1].x := 75; box[1].y := -75; box[1].z := -75;
  box[2].x := 75; box[2].y := 75; box[2].z := -75;
  box[3].x := -75; box[3].y := 75; box[3].z := -75;
  box[4].x := -75; box[4].y := -75; box[4].z := 75;
  box[5].x := 75; box[5].y := -75; box[5].z := 75;
  box[6].x := 75; box[6].y := 75; box[6].z := 75;
  box[7].x := -75; box[7].y := 75; box[7].z := 75;
end;

procedure myline(x1, y1, z1, x2, y2, z2: real);
 {Keeps the draw routine pretty.
Pixels are integers, so I round. Since the
cube is centered around 0,0 I move it over 200 to put it on screen.} begin
{if you think those real mults are slow, here's some rounds too...}

{hey, you may wonder, what happened to the stinking z coordinate? Ah, says I,
 this is the simplest of 3d viewing transforms. You just take the z coord out
 of things and boom. Looking straight down the z axis on the object.
 If I get inspired, I will add simple perspective transform to these.}
 {There, got inspired. Made mistakes. Foley et al are not very good at
 tutoring perspective and I'm kinda ready to be done and post this.}
  MainForm.Image2.Canvas.moveto(round(x1) + 200, round(y1) + 200);
  MainForm.Image2.Canvas.lineto(round(x2) + 200, round(y2) + 200);
end;

procedure draw;
{my model is hard coded. No cool things like vertex and edge and face
 lists.}
begin
  myline(box[0].x, box[0].y, box[0].z, box[1].x, box[1].y, box[1].z);
  myline(box[1].x, box[1].y, box[1].z, box[2].x, box[2].y, box[2].z);
  myline(box[2].x, box[2].y, box[2].z, box[3].x, box[3].y, box[3].z);
  myline(box[3].x, box[3].y, box[3].z, box[0].x, box[0].y, box[0].z);

  myline(box[4].x, box[4].y, box[4].z, box[5].x, box[5].y, box[5].z);
  myline(box[5].x, box[5].y, box[5].z, box[6].x, box[6].y, box[6].z);
  myline(box[6].x, box[6].y, box[6].z, box[7].x, box[7].y, box[7].z);
  myline(box[7].x, box[7].y, box[7].z, box[4].x, box[4].y, box[4].z);

  myline(box[0].x, box[0].y, box[0].z, box[4].x, box[4].y, box[4].z);
  myline(box[1].x, box[1].y, box[1].z, box[5].x, box[5].y, box[5].z);
  myline(box[2].x, box[2].y, box[2].z, box[6].x, box[6].y, box[6].z);
  myline(box[3].x, box[3].y, box[3].z, box[7].x, box[7].y, box[7].z);

  myline(box[0].x, box[0].y, box[0].z, box[5].x, box[5].y, box[5].z);
  myline(box[1].x, box[1].y, box[1].z, box[4].x, box[4].y, box[4].z);
end;

procedure rotx;
{if you know your matrix multiplication, the following equations
 are derived from
 [x   [ 1  0  0  0   [x',y',z',1]
  y     0  c -s  0 =
  z     0  s  c  0
  1]    0  0  0  1]}
var i: integer;
begin
  MainForm.Image2.Canvas.Pen.Color :=
    RGB(Colors[0, 0], Colors[1, 0], Colors[2, 0]);
  MainForm.Image2.Canvas.Brush.Color :=
    RGB(Colors[0, 0], Colors[1, 0], Colors[2, 0]);
{  setcolor(0);}
  draw;
  for i := 0 to 7 do
  begin
    box[i].x := box[i].x;
    box[i].y := box[i].y * cos(radTheta) + box[i].z * sin(radTheta);
    box[i].z := -box[i].y * sin(radTheta) + box[i].z * cos(radTheta);
  end;
{  setcolor(15);}
  MainForm.Image2.Canvas.Pen.Color :=
    RGB(Colors[0, 150], Colors[1, 150], Colors[2, 150]);
  MainForm.Image2.Canvas.Brush.Color :=
    RGB(Colors[0, 150], Colors[1, 150], Colors[2, 150]);
  draw;
end;

procedure roty;
{if you know your matrix multiplication, the following equations
 are derived from
 [x   [ c  0  s  0   [x',y',z',1]
  y     0  1  0  0 =
  z    -s  0  c  0
  1]    0  0  0  1]}
var i: integer;
begin
 { setcolor(0);}
  MainForm.Image2.Canvas.Pen.Color :=
    RGB(Colors[0, 0], Colors[1, 0], Colors[2, 0]);
  MainForm.Image2.Canvas.Brush.Color :=
    RGB(Colors[0, 0], Colors[1, 0], Colors[2, 0]);
  draw;
  for i := 0 to 7 do
  begin
    box[i].x := box[i].x * cos(radTheta) - box[i].z * sin(radTheta);
    box[i].y := box[i].y;
    box[i].z := box[i].x * sin(radTheta) + box[i].z * cos(radTheta);
  end;
{  setcolor(15); }
  MainForm.Image2.Canvas.Pen.Color :=
    RGB(Colors[0, 150], Colors[1, 150], Colors[2, 150]);
  MainForm.Image2.Canvas.Brush.Color :=
    RGB(Colors[0, 150], Colors[1, 150], Colors[2, 150]);
  draw;
end;

procedure rotz;
{if you know your matrix multiplication, the following equations
 are derived from
 [x   [ c -s  0  0   [x',y',z',1]
  y     s  c  0  0 =
  z     0  0  1  0
  1]    0  0  0  1]}
var i: integer;
begin
 { setcolor(0);}
  MainForm.Image2.Canvas.Pen.Color :=
    RGB(Colors[0, 0], Colors[1, 0], Colors[2, 0]);
  MainForm.Image2.Canvas.Brush.Color :=
    RGB(Colors[0, 0], Colors[1, 0], Colors[2, 0]);

  draw;
  for i := 0 to 7 do
  begin
    box[i].x := box[i].x * cos(radTheta) + box[i].y * sin(radTheta);
    box[i].y := -box[i].x * sin(radTheta) + box[i].y * cos(radTheta);
    box[i].z := box[i].z;
  end;
{  setcolor(15);}
  MainForm.Image2.Canvas.Pen.Color :=
    RGB(Colors[0, 150], Colors[1, 150], Colors[2, 150]);
  MainForm.Image2.Canvas.Brush.Color :=
    RGB(Colors[0, 150], Colors[1, 150], Colors[2, 150]);

  draw;
end;



procedure RotatingCube;
begin
  MainForm.DoImageStart;
  init;
 { setcolor(14);}
  MainForm.Image2.Canvas.Pen.Color :=
    RGB(Colors[0, 0], Colors[1, 0], Colors[2, 0]);
  MainForm.Image2.Canvas.Brush.Color :=
    RGB(Colors[0, 0], Colors[1, 0], Colors[2, 0]);
  draw;
  bRotateImage := False; {in the Drawing...}
  bRotatingImage := True;
  RotatorCuff := 0;
  repeat
    begin
      case RotatorCuff of
        0: rotx;
        1: roty;
        2: rotz;
      else {who gives a};
      end; {case}
      MainForm.HiddenFX.Caption := 'FX: ';
      Application.ProcessMessages;
    end;
  until (bRotateImage = True);
  Mainform.DoImageDone;
end;


(***************************************************)
(***************************************************)

procedure drawScene(x, y, z, rot: integer);
var lastTan, lastAngle, h: integer;
  mapTan: longint;
  hstr: string;
{var scrn:word;}
var {color,}  height: byte;
var xs, ys, ds: longint;
var xp, yp, dp: fixed;
begin
{ fillchar(mem[$A000:0],320*200,0);}
  for h := 0 to xSize - 1 do begin
    lastAngle := 0;
{  scrn:=h+320*(ySize-1);}
    lastTan := tanTab[lastAngle];
    xp.i := x; xp.f := 0;
    yp.i := y; yp.f := 0;
    dp.l := 0;
    xs := longint(sinTab[(h + rot - (xSize shr 1)) and angleMask]) *
      2;
    ys := longint(sinTab[(h + rot - (xSize shr 1) + xSize) and
      angleMask]) * 2; {cos}
    ds := $FFFE;
    inc(xp.l, xs * 16);
    inc(yp.l, ys * 16);
    inc(dp.l, ds * 16);
    str(h, hstr);
    MainForm.HiddenFX.Caption := 'FX: ' + hstr;
    Application.ProcessMessages;
    while lastAngle < ySize do begin
      inc(xp.l, xs * 2);
      inc(yp.l, ys * 2);
      inc(dp.l, ds * 2);
      inc(xs, xs div 32);
      inc(ys, ys div 32);
      inc(ds, ds shr 5);
      if word(xp.i) > mapSize - 1 then
        break;
      if word(yp.i) > mapSize - 1 then
        break;
      height := map[xp.i, yp.i];
      mapTan := (longint(height - z) * $7FFF) div dp.i;
{   color:=32+((z-height));}{never changes}
      while (lastTan <= mapTan) or {and}(lastAngle < ySize) do begin
{    mem[$A000:scrn]:=color;}{h+20,lastAngle+20}
        MainForm.Image2.Canvas.Pixels[lastAngle + 120, h] :=
          abs(lastTan);
{RGB(Colors[0,(color mod 255)],Colors[1,(color mod 255)],Colors[2,(color mod 255)]);}
{    dec(scrn,320);}
        inc(lastAngle);
        lastTan := tanTab[lastAngle];
{  str((z-height),hstr);
    MainForm.HiddenFX.Caption:='FX angle: '+hstr;
    Application.ProcessMessages;}
      end;
    end;
  end;
end;


procedure initTables; var i: integer; r: real; begin
  for i := 0 to angleMask do
    sinTab[i] := round(sin(i * pi / 512) * $7FFF);
  for i := 0 to ySize - 1 do begin
    r := (i - 64) * pi / (3 * ySize);
    tanTab[i] := round(sin(r) / cos(r) * $7FFF);
  end;
end;

procedure initMap; var x, y: integer; begin
  for x := 0 to 127 do
    for y := 0 to 127 do
      map[x, y] := ((longint(sinTab[(y * 21 - 12) and angleMask])
        + sinTab[(x * 31 + 296) and angleMask] div 2)
        shr 12) + 120;
end;




procedure FallingColumn;
var
  x, y, z, r {,a}: integer;
  i: word;
begin
  MainForm.DoImageStart;
  bRotateImage := False; {in the Drawing...}
  bRotatingImage := True;
  if ((RotatorCuff < 0) or (RotatorCuff > 3))
    then RotatorCuff := 0;
{ sinTab:array of Integer;}
  SetLength(sinTab, angleMask + 1);
 {:array[0..angleMask]of integer;}
 {sin(xyAngle)*$7FFF}
{ tanTab:array of Integer;}
  SetLength(tanTab, ySize);
{ array[0..ySize-1]of integer;}
 {tan(zAngle)*$7FFF}
{ map:array of array of byte;}
{ array[0..mapSize-1,0..mapSize-1]of byte;}
  SetLength(map, mapSize, mapSize);
  initTables;
  MainForm.HiddenFX.Caption := 'FX:   22';
  Application.ProcessMessages;
  initMap;

  randomize;
  x := 50 + random(29);
  y := 50 + random(29);
  z := 125 + random(10);
  r := random(angleMask);
{ a:=64;}
  repeat begin
{  drawScene(x,y,z,r);  }
      RotatorCuff := random(500);
      if tanTab[ySize - 1] < 30000 then for i := 0 to ySize - 1 do
        inc(tanTab[i], RotatorCuff)
      else if tanTab[0] > -30000 then for i := 0 to ySize - 1 do
        dec(tanTab[i], RotatorCuff);
      RotatorCuff := random(4);
{  inc(RotatorCuff);   }
      if ((RotatorCuff < 0) or (RotatorCuff > 3))
        then RotatorCuff := 0;
      case RotatorCuff of
        0: if tanTab[ySize - 1] < 30000 then
          for i := 0 to ySize - 1 do inc(tanTab[i], 500);
        1: if tanTab[0] > -30000 then for i := 0 to ySize - 1 do
          dec(tanTab[i], 100 {500});
        2: r := (r - 32) and angleMask;
        3: r := (r + 32) and angleMask;
      end;
      drawScene(x, y, z, r);
      MainForm.HiddenFX.Caption := 'FX: ';
      Application.ProcessMessages;
    end;
  until (bRotateImage = True);
  Mainform.DoImageDone;
end;


(***************************************************)
(***************************************************)

(*************************************************************)

(*************************************************************)
(*************************************************************)

procedure B_3D_Map(N, L, VA, HA: Integer);
{'---- 3-D SURFACE PLOT  ----       File: 3DMAP
DEF FNF(X,Y) = 80 + (X*Y*(X-Y)) '  Surface to be plotted}
var
  maxcolx, maxrowy, IC, JC: Integer;
  X0, Y0, XM, YM: Integer; {   :'---- Plot limits}
  I, J, CV, SV, CH, SH, XA, YA,
    X, X1, DX, DX1, STX, SX, STX1,
    Y, Y1, Y2, DY, DY1, STY, SY, STY1: Extended;
  TempColor: TColor;

  function FNF(A, B: Extended): Extended;
  begin
    FNF := (180 {(FYImageY div 2) } + (A * B * (A - B)));
 {height added +  Surface to be plotted}
  end;

begin
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
    maxrowy := (FYImageY - 1);
    X0 := -4;
    Y0 := -4;
    XM := 5;
    YM := 5; {     :'---- Plot limits}
    STX := (XM - X0) / N;
    STY := (YM - Y0) / N;
{STX1 := STX;STY1 := STY;}{VA=Vertical angle}
    CV := COS(VA / 57.3);
    SV := SIN(VA / 57.3);
    CH := COS(HA / 57.3); {HA = Horiz view angle}
    SH := SIN(HA / 57.3);
   {init graphics }{   Start position}
    SY := 400;
    SX := 380;
{SY := (FYImageY div 2);
SX := (FYImageX div 2);}

    X := SX;
    X1 := X;
    DX1 := L * SH * 3;
    DX := L * CH * 3;
    Y := SY;
    Y1 := Y;
    DY1 := L * SV * CH;
    DY := L * SV * SH;
    Moveto(Round(X), Round(Y));
    Lineto(Round(X + N * DX1), Round(Y - N * DY1));
    I := X0;
    J := Y0; {STEP STX} {  :  PSET (X,Y)}
    for IC := 1 to N + 1 do
    begin
      XA := X;
      YA := Y;
      for JC := 1 to N + 1 do {  STEP STY}
      begin
        Y2 := Y1 - CV * FNF(I, J);
        Moveto(Round(XA), Round(YA));
        Lineto(Round(X1), Round(Y2));
        XA := X1;
        YA := Y2;
        X1 := X1 + DX1;
        Y1 := Y1 - DY1;
        STY1 := JC * STY;
        J := Y0 + STY1;
      end; {NEXT J }
      J := Y0;
      X := X - DX;
      Y := Y - DY;
      X1 := X;
      Y1 := Y;
      STX1 := IC * STX;
      I := X0 + STX1;
    end; { NEXT I }
    X := SX;
    Y := SY;
    Moveto(Round(X), Round(Y));
    Lineto(Round(X - N * DX), Round(Y - N * DY));
{STX1 := (XM-X0)/N;STY1 := (YM-Y0)/N; }
    I := X0;
    J := Y0;

    for JC := 1 to N + 1 do
    begin
      XA := X;
      YA := Y;
      X1 := X;
      Y1 := Y;
      for IC := 1 to N + 1 do {STEP STX}
      begin
        Y2 := Y1 - CV * FNF(I, J);
        Moveto(Round(XA), Round(YA));
        Lineto(Round(X1), Round(Y2));
        XA := X1;
        YA := Y2;
        X1 := X1 - DX;
        Y1 := Y1 - DY;
        STX1 := IC * STX;
        I := X0 + STX1;
      end; { NEXT I}
      I := X0;
      X := X + DX1;
      Y := Y - DY1;
      STY1 := JC * STY;
      J := Y0 + STY1;
    end; end; { NEXT J }
end; { of Procedure } { of UNIT  B_B3DMAP }
(*************************************************************)

procedure SinShadow(Plot_Angle, M_x, sign: Integer;
  View_Angle, Illum_Angle: Extended);
var
  HM: array[0..4095] of Extended;
{Plot_Angle, View_Angle, Illum_Angle, M_x,}
  X, Z, R, Y, H, HS: Extended;
  InSign, HalfX, HalfY, MiniY, maxcolx {,maxrowy}, I, XCount, ZCount,
    {C,} Y_Height: Integer;
  DoColor {,TempColor}: TColor; NS, TextS: string;
begin
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
{	maxrowy := (FYImageY-1);}
    MiniY := ((FYImageY - 1) div 6);
    HalfX := ((FYImageY - 1) div 2);
    HalfY := ((FYImageY - 1) div 2);
    if (sign < 0) then Insign := -1 else Insign := 1;
{Brush.Color:=FBackGroundColor;
Brush.Style:=bsSolid;
FillRect(Rect(0,0,maxcolx,maxrowy));
TempColor:=RGB(255-GetRValue(FBackGroundColor),
                255-GetGValue(FBackGroundColor),
                255-GetBValue(FBackGroundColor));
Pen.Color := TempColor;
Font.Color:= TempColor;
Rectangle(0,0,maxcolx,maxrowy);
MainForm.Show;}
    Z := 0.0;
    HS := 0.0;
    for I := 0 to maxcolx do HM[I] := 0.0;
(*
Plot_Angle  := 90.0;   { >90 makes steep walls of SIN <90 makes shallow}
View_Angle  := 3.0; {3.0;}   {2.0  Makes an angle of 30 degrees}
Illum_Angle := 0.3; {0.3;}   {0.5  Makes an angle of 30 degrees}
M_x := 20.0;          { Magnification ratio ... see below }
    *){ V and I would make the plot location need to be changed}
{ LOOP}
    for Zcount := 0 to MiniY {74  479} do {STEP 15}
    begin
      X := 0;
      for Xcount := 0 to maxcolx do
      begin
{R := ( (SQRT( ((POWER((X-320),2.0)) +  (POWER((Z-240),2.0))) ))  / M_x );}
        R := SQRT(sqr(X - HalfX) + sqr(Z - HalfY)) / M_x;
              { ? / Magnification ... how close to the apex }
              {higher M_x = closer ... less of whole wave is seen }
              { AT a V_Angle of 3.0 and a I_Angle of 0.3 }
              { M_x := }
              { 0.0 is seemingly flat with a vertical line as apex}
              { 20.0 is ok : the hump and 3 circling waves }
              { 30.0 is the hump and 2 circling waves }
              { 40.0 is the hump and its circling wave}
              { 60.0 is only the inner hump is seen }
               {GOTOXY(1,1);  Writeln('R : ',R:40:20);}
        Y := Plot_Angle * InSign * (sin(R) / R);
               { Writeln('Y : ',Y:40:20);}
        H := Y + (Z / View_Angle);
               {  Writeln('H : ',H:40:20);  }
               { Readln; }
        HS := HS - Illum_Angle;
        if H < HS then DoColor := V1Color
        else begin
          DoColor := V2Color;
          HS := H;
        end;
        if H > HM[Xcount] then
        begin
          HM[Xcount] := H;
          Y_Height := ((HalfY - round(H)) * 2);
          Pixels[Xcount, Y_Height] := DoColor;
        end;
        X := X + 1.0;
(*      iF c=2 THEN BEGIN {DO NOTHING}END*);
      end; {  NEXT X}
      HS := 0;
      Z := Z + 10.0 {10.0}; {15}
    end; { NEXT Z}
{M_x := 20.0;
Plot_Angle  := 90.0;
View_Angle  := 3.0;
Illum_Angle := 0.3;}
    str(Plot_Angle, NS); TextS := 'Plot angle: ' + NS;
    str(View_Angle: 6: 2, NS); TextS := TextS + '  View angle: ' +
      NS;
    str(Illum_Angle: 6: 2, NS); TextS := TextS +
      '  Illumination angle: ' + NS;
    str(M_x, NS); TextS := TextS + '  Magnification: ' + NS;
    TextOut(10, 10, TextS);
{ TextOut(10,470,'Plot = 90, View = 3, I = .3, M = 20');}
  end;
end; { of procedure SinShadow }

(*************************************************************)

procedure CoSinShadow(Plot_Angle, M_x, sign: Integer;
  View_Angle, Illum_Angle: Extended);
var
  HM: array[0..4095] of Extended;
{Plot_Angle, View_Angle, Illum_Angle, M_x,}
  X, Z, R, Y, H, HS: Extended;
  Insign, HalfX, HalfY, MiniY, maxcolx {,maxrowy},
    I, XCount, ZCount, {C,} Y_Height: Integer;
  DoColor {,TempColor}: TColor; NS, TextS: string;
begin
  with MainForm.Image2.Canvas do begin
    maxcolx := (FYImageX - 1);
{	maxrowy := (FYImageY-1); }
    MiniY := ((FYImageY - 1) div 7);
    HalfX := ((FYImageY - 1) div 2);
    HalfY := ((FYImageY - 1) div 2);
    if (sign < 0) then Insign := -1 else Insign := 1;
    Z := 0.0;
    HS := 0.0;
    for I := 0 to maxcolx do HM[I] := 0.0;
(*
Plot_Angle  := 90.0;{>90 makes steep walls of SIN <90 makes shallow}
View_Angle  := 3.0; {3.0;}   {2.0  Makes an angle of 30 degrees}
Illum_Angle := 0.3; {0.3;}   {0.5  Makes an angle of 30 degrees}
M_x := 20.0;          { Magnification ratio ... see below }
    *){ V and I would make the plot location need to be changed}
{ LOOP}
    for Zcount := 0 to MiniY {74  479} do {STEP 15}
    begin
      X := 0;
      for Xcount := 0 to maxcolx do
      begin
{R := ( (SQRT( ((POWER((X-320),2.0)) +  (POWER((Z-240),2.0))) ))  / M_x );}
        R := SQRT(sqr(X - HalfX) + sqr(Z - HalfY)) / M_x;
              { ? / Magnification ... how close to the apex }
              {higher M_x = closer ... less of whole wave is seen }
              { AT a V_Angle of 3.0 and a I_Angle of 0.3 }
              { M_x := }
              { 0.0 is seemingly flat with a vertical line as apex}
              { 20.0 is ok : the hump and 3 circling waves }
              { 30.0 is the hump and 2 circling waves }
              { 40.0 is the hump and its circling wave}
              { 60.0 is only the inner hump is seen }
               {GOTOXY(1,1);  Writeln('R : ',R:40:20);}
        Y := Plot_Angle * Insign * (COS(R) / R);
               { Writeln('Y : ',Y:40:20);}
        H := Y + (Z / View_Angle);
               {  Writeln('H : ',H:40:20);  }
               { Readln; }
        HS := HS - Illum_Angle;
        if H < HS then DoColor := V1Color
        else begin
          DoColor := V2Color;
          HS := H;
        end;
        if H > HM[Xcount] then
        begin
          HM[Xcount] := H;
          Y_Height := ((HalfY - round(H)) * 2);
          Pixels[Xcount, Y_Height] := DoColor;
        end;
        X := X + 1.0;
(*      iF c=2 THEN BEGIN {DO NOTHING}END*);
      end; {  NEXT X}
      HS := 0;
      Z := Z + 10.0 {10.0}; {15}
    end; { NEXT Z}
{M_x := 20.0;
Plot_Angle  := 90.0;
View_Angle  := 3.0;
Illum_Angle := 0.3;}
    str(Plot_Angle, NS); TextS := 'Plot angle: ' + NS;
    str(View_Angle: 6: 2, NS); TextS := TextS + '  View angle: ' +
      NS;
    str(Illum_Angle: 6: 2, NS); TextS := TextS + '  Illum angle: ' +
      NS;
    str(M_x, NS); TextS := TextS + '  Magnification: ' + NS;
    TextOut(10, 10, TextS);
{ TextOut(10,470,'Plot = 90, View = 3, I = .3, M = 20');}
  end;
end; { of procedure CoSinShadow }
(**************************************************)





 (**************************************************)
 (**************************************************)


end.
