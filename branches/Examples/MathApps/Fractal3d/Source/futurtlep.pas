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
unit futurtlep;

interface

uses
  Windows, Messages, SysUtils, Dialogs,
  Forms, Graphics, Classes, Math;

function TC2S(X: Integer): string;
procedure Plot_System(Maxlevel: Integer);
function XScreen(TurtleDo: Double): Integer;
function YScreen(TurtleDo: Double): Integer;
procedure GenerateString(maxleveli: Integer);
procedure CleanUpString;
procedure GetCurveSize;
procedure UpdateTurtleState(command: Char);
procedure TurtleInterpretation;

implementation
uses fUGlobal,fMain, fGMath;


var
{axiom:String; }{String containing the axiom of OL-System}
  DoitStr: string;
{KarString: string; Kar[] character array (size num),
                inputs of production rules}
{RuleString: string; Rule[] string array (size num)
                outputs of production rules}
  Num: Integer; {Number of production rules}
  XminD, XmaxD, YminD, YmaxD: Double; {size of turtle movements ?}
  XSize, YSize: Integer; {Size of screen in pixels}
{declared in G_Math
TurtleDirN:Integer;  Number of possible directions of turtle}
  TurtleDir: Integer; {(Direction of Turtle coded as Integer)}
  TurtleX, TurtleY: Double; {Real X,Y position of Turtle}
  Factor: Double;
{var MyFlexibleArray: array of Real;
SetLength(MyFlexibleArray, 20);
To deallocate a dynamic array,
assign nil to a variable that references the array
or pass the variable to Finalize;
either of these methods disposes of the array,
provided there are no other references to it.}
  CO: array of Double; {Array of TurtleDirN cosine values;}
  SI: array of Double; {Array of TurtleDirN sine values}
  TStackX: array of Double; {Stack of turtle x,y positions}
  TStackY: array of Double;
  TStackDir: array of Double;
  TStackSize: Integer; {Size of turtle stack (counter)}
  TStackMax: Integer; {max size of turtle stack}
{Maxlevel:Integer number of cycles in string generation}
{?:=Strlen(s) returns the length of string s}
{str:= Strapp(s,t) returns string s appended to string t}
{string:= strappc(s,c)  returns string s appended by character c}
{command := getchar(s,k) returns the k-th character of string s}
{strcpy(s,t) procedure to copy string t into string s}
{The relational operators
=, <>, <, >, <=, and >= all take string operands}

function TC2S(X: Integer): string;
var
  TempStr: string;
begin
  str(X, TempStr);
  TC2S := TempStr;
end;

function C2S(X: Double): string;
var
  TempStr: string;
begin
  str(X, TempStr);
  C2S := TempStr;
end;

procedure Plot_System(Maxlevel: Integer);
var TempColor: Tcolor; I: Integer;
begin

{TAxiom:String;
TPI:Extended;
TDegrees:Extended;
TXminI,TmaxI, TYminI, TYmaxI:Integer;
ProductionRules:Array [0..127] of String;
axiom:= ;
KarString:= ;
RuleString:= ;}

  Num := Length(TKan);
{TurtleDirN:=Round(TPI);... in G_Math TurtlePresent}
  SetLength(CO, TurtleDirN);
  SetLength(SI, TurtleDirN);
  TStackSize := 0; {Size of turtle stack (counter)}
{Size of turtle stack max}
  TStackMax := ((Length(axiom)
    * Length(ProductionRules[Num])
    * Maxlevel)
    * 10); {just in case rule length differs}
                      {Get the longest rule?  instead?}
  SetLength(TStackX, TStackMax);
  SetLength(TStackY, TStackMax);
  SetLength(TStackDir, TStackMax);

  I := Maxlevel;
  TempColor := RGB(RGBArray[0, I],
    RGBArray[1, I], RGBArray[2, I]);
  MainForm.Image2.Canvas.Pen.Color := TempColor;
  MainForm.Image2.Canvas.Font.Color := TempColor;
  MainForm.Image2.Canvas.TextOut
    (10, 10, 'Turtle  ' + MathForm.TurtleFileEdit.Text + ' Level: ' +
      TC2S(I));
  GenerateString(maxlevel);
  CleanUpString;
  GetCurveSize;
  TurtleInterpretation;
  MathForm.TStrLenEdit.Text := TC2S(Length(DoitStr));
{Reset arrays to 0 length... free memory}
  DoitStr := '';
  SetLength(CO, 0);
  SetLength(SI, 0);
  SetLength(TStackX, 0);
  SetLength(TStackY, 0);
  SetLength(TStackDir, 0);
end;


procedure GenerateString(maxleveli: Integer);
var
  level: Integer; {number of string generation cycle}
  command: Char; {character}
  str0: string;
  i, k: Integer;
begin
  str0 := axiom;
  DoitStr := '';

  for Level := 1 to MaxLeveli do begin
    for k := 1 to Length(str0) do begin
      command := {getchar(} str0[k];
      i := 1;
      while ((i < num) and not (command = TKan[i])) do
      begin inc(i); end;
      if (command = TKan[i]) then
                 { strapp(str,RuleString[i]) }
        DoitStr := (DoitStr + ProductionRules[i])
      else DoitStr := (DoitStr + command);
    end;
    str0 := DoitStr; {(str0+DoitStr);} { strcpy(str0,DoitStr);}
    DoitStr := '';
  end;
  DoitStr := str0;
end;


procedure CleanUpString {str};
var
  str0: string;
  i: integer;
  c: Char;
begin
  str0 := '';
  for i := 1 to Length(DoitStr) do begin
    c := DoitStr[i];
    if ((c = 'F') or (c = 'f') or (c = '+') or (c = '-')
      or (c = '|') or (c = ']') or (c = '[')) then
      str0 := str0 + c;
  end;
  DoitStr := str0;
end;


procedure GetCurveSize;
var
  i: integer;
  command: Char;
begin
  for i := 0 to TurtleDirN - 1 do begin
    CO[i] := cos(2 * Pi * i / TurtleDirN);
    SI[i] := sin(2 * Pi * i / TurtleDirN);
  end;
  TurtleDir := 0;
  TurtleX := 0.0;
  TurtleY := 0.0;
  XminD := {0.0;} FYImageX; YminD := {0.0;} FYImageY;
  XmaxD := 0.0; YmaxD := 0.0;
  for i := 1 to Length(DoitStr) do begin
    command := DoitStr[i];
    UpdateTurtleState(command);
    if ((command = 'F') or (command = 'f')) then begin
      XMaxD := Max(TurtleX, XMaxD);
      YMaxD := Max(TurtleY, YMaxD);
      XMinD := Min(TurtleX, XMinD);
      YMinD := Min(TurtleY, YMinD);
    end;
  end;
end;


procedure UpdateTurtleState(command: Char);
begin
  if ((Command = 'F') or (Command = 'F')) then begin
    TurtleX := TurtleX + CO[TurtleDir];
    TurtleY := TurtleY + SI[TurtleDir];
  end else if (Command = '+' {'+'}) then begin
    TurtleDir := TurtleDir - 1;
    if (TurtleDir < 0) then TurtleDir := TurtleDirN - 1;
  end else if (Command = '-' {'-'}) then begin
    TurtleDir := TurtleDir + 1;
    if (TurtleDir = TurtleDirN) then TurtleDir := 0;
  end else if (Command = '|') then begin
    TurtleDir := (TurtleDir + (TurtleDirN div 2));
    if (TurtleDir > TurtleDirN) then
      TurtleDir := (TurtleDir - TurtleDirN);
  end else if (Command = '[') then begin
    if (TStackSize = TStackMax) then begin
      Halt;
    end;
    TStackX[TStackSize] := TurtleX;
    TStackY[TStackSize] := TurtleY;
    TStackDir[TStackSize] := TurtleDir;
    Inc(TStackSize);
  end else if (Command = ']') then begin
    if (TStackSize = 0) then begin
      Halt;
    end;
    Dec(TStackSize);
    TurtleX := TStackX[TStackSize];
    TurtleY := TStackY[TStackSize];
    TurtleDir := Round(TStackDir[TStackSize]);
  end;
end;

function XScreen(TurtleDo: Double): Integer;
begin
  XScreen := Round(Factor * (TurtleDo - XminD));
end;

function YScreen(TurtleDo: Double): Integer;
begin
  YScreen := Round((Factor * (TurtleDo - YminD)));
end;

procedure TurtleInterpretation;
var
  command: Char;
  TempColor: TColor;
  HalfX, HalfY, i: integer;
begin
  XSize := {(FYImageX div 2); }(FYImageX - (FYImageX div 4));
  YSize := {(FYImageY div 2);}(FYImageY - (FYImageY div 4));
  HalfX := (XSize div 5);
  HalfY := (YSize div 5);
  Factor := MIN(((XSize) / ((XMaxD - XMinD))),
    ((YSize) / ((YMaxD - YMinD))));
  TurtleDir := 0;
  TurtleX := 0.0;
  TurtleY := 0.0;

  MainForm.Image2.Canvas.MoveTo
    (XScreen(TurtleX) + HalfX, FYImageY {YSize} - (YScreen(TurtleY) +
      HalfY));
  for i := 1 to Length(DoitStr) do begin
    TempColor := RGB(Colors[0, (I mod 255)],
      Colors[1, (I mod 255)],
      Colors[2, (I mod 255)]);
    MainForm.Image2.Canvas.Pen.Color := TempColor;
    command := DoitStr[i];
    UpdateTurtleState(command);
    if (command = 'F') then
      MainForm.Image2.Canvas.LineTo
        (XScreen(TurtleX) + HalfX, FYImageY {YSize} -
          (YScreen(TurtleY) + HalfY))
    else if (command = 'f') then
      MainForm.Image2.Canvas.MoveTo
        (XScreen(TurtleX) + HalfX, FYImageY {YSize} -
          (YScreen(TurtleY) + HalfY));
  end;
end;


end.
