{-------------------------------------------------------------------------------
 Unit Name: cUtilities
 Author:    HochwimmerA
 Purpose:   Utility Functions
 $Id: cUtilities.pas,v 1.3 2003/07/16 15:04:51 hochwimmera Exp $
-------------------------------------------------------------------------------}
unit cUtilities;

interface

uses
  Winapi.Windows,
  System.Sysutils,
  Vcl.Graphics;

procedure ColourRamp(const dRatio:double;var bForward:boolean;
  var col1,col2:TColor;var rstar:double);
function GetSafeName(sOriginal:string):string;
function HexToColor(sColor: string): TColor;
function ColorToHex(Color: integer): string;
function RGBToColor(iRed,iGreen,iBlue:integer):TColor;
procedure StripString(var sLine,sNew:string);

implementation
// ----- ColourRamp ------------------------------------------------------------
procedure ColourRamp(const dRatio:double;var bForward:boolean;
  var col1,col2:TColor;var rstar:double);

var
  i:integer;

begin
  if bForward then
    i := Round(dRatio*256)
  else
    i := Round((1.0-dRatio)*256);

  if (i < 64) then
  begin
    col1 := RGBToColor(0,0,255);
    col2 := RGBToColor(0,255,255);
    rstar := i/64
  end else if (i < 128) then
  begin
    col1 := RGBToColor(0,255,255);
    col2 := RGBToColor(0,255,0);
    rstar := (i-64)/64;
  end else if (i < 192) then
  begin
    col1 := RGBToColor(0,255,0);
    col2 := RGBToColor(255,255,0);
    rstar := (i-128)/64;
  end else
  begin
    col1 := RGBToColor(255,255,0);
    col2 := RGBToColor(255,0,0);
    rstar := (i-192)/64;
  end;
end;
// ----- GetSafeName -----------------------------------------------------------
function GetSafeName(sOriginal:string):string;

var
  sGoodLabel:string;

begin
  sGoodLabel := sOriginal;
  sGoodLabel := StringReplace(sGoodLabel,' ','',[rfReplaceAll]);
  sGoodLabel := StringReplace(sGoodLabel,'%','',[rfReplaceAll]);
  sGoodLabel := StringReplace(sGoodLabel,'-','',[rfReplaceAll]);
  sGoodLabel := StringReplace(sGoodLabel,'*','',[rfReplaceAll]);
  sGoodLabel := StringReplace(sGoodLabel,':','',[rfReplaceAll]);
  sGoodLabel := StringReplace(sGoodLabel,'/','',[rfReplaceAll]);
  sGoodLabel := StringReplace(sGoodLabel,'.','',[rfReplaceAll]);
  sGoodLabel := StringReplace(sGoodLabel,',','',[rfReplaceAll]);
  result := sGoodLabel;
end;
// ------ HexToColor -----------------------------------------------------------
function HexToColor(sColor: string): TColor;
begin
  Result :=
    RGB(
    StrToInt('$' + Copy(sColor, 1, 2)),
    StrToInt('$' + Copy(sColor, 3, 2)),
    StrToInt('$' + Copy(sColor, 5, 2))
    );
end;
// ------ ColorToHex -----------------------------------------------------------
function ColorToHex(Color: integer): string;
var
  r, g, b: byte;
begin
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  Result := IntToHex(r, 2) + IntToHex(g, 2) + IntToHex(b, 2);
end;
// ------ RGBToColor -----------------------------------------------------------
{** returns a TColor from RGB values}
function RGBToColor(iRed,iGreen,iBlue:integer):TColor;

begin
  result := RGB(StrToInt('$'+IntToHex(iRed,2)),
    StrToInt('$'+IntToHex(iGreen,2)),
    StrToint('$'+IntToHex(iBlue,2)))
end;
// ----- StripString -----------------------------------------------------------
procedure StripString(var sLine,sNew:string);

var
  iPos:integer;

begin
  while (Pos(' ',sLine) = 1) do
    Delete(sLine,1,1);
  iPos := Pos(' ',sLine);
  if (iPos = 0) then
  begin
    sNew := sLine;
    sLine := '';
  end else
  begin
    sNew := Copy(sLine,0,iPos-1);
    sLine := Copy(sLine,iPos+1,Length(sLine));
  end;
end;
// =============================================================================
end.







