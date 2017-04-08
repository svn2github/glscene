{ ******************************************************* }
{ }
{ Tachyon Unit }
{ Vector Raster Geographic Information Synthesis }
{ VOICE  ..  Tracer }
{ GRIP ICE .. Tongs }
{ Digital Terrain Mapping }
{ Image Locatable Holographics }
{ SOS MAP }
{ Surreal Object Synthesis Multimedia Analysis Product }
{ Fractal3D  Life MOW }
{ Copyright (c) 1995,2006  Ivan Lee Herring }
{ }
{ ******************************************************* }
unit fUGlobal;

interface

uses Dialogs, Messages, Graphics,
  ShellAPI,
  Windows, SysUtils, Classes, Controls, Forms,
  Grids, StdCtrls, ColorGrd, Mask, Buttons,
  Menus, ComCtrls, ExtCtrls;

type
  TMatrix = array [0 .. 2, 0 .. 15] of Byte; { RGB Color sets }

  { Headcase = array[0..15, 0..3] of Double; }
  NoBrainer = record
    { Brain:Headcase; }
    xscale, yscale, xoffset, yoffset, Horizon, Iterations: integer;
    Alpha, Beta, Gamma, VPy, HQx: Extended;
    a, b, c, d, e, f, g, h, m, n, p, q, r: array [0 .. 3] of Extended;
    V1, V2, V3, V4: Tcolor;
  end;

  BrainFile = file of NoBrainer;

  Sky2D = record
    { Brain:Headcase; }
    xscale, yscale, xoffset, yoffset, Horizon, Iterations: integer;
    { Alpha, Beta, Gamma,P,Q:Extended; }
    a, b, c, d, e, f, { g,h,m,n,q,r, } p: array [0 .. 3] of Extended;
    V1, V2, V3, V4: Tcolor;
  end;

  SkyFile = file of Sky2D;

  PrefRecord = record
    { PFractalFont: TFont; }
    PIamDone: integer; { Use to Set Done selection }
    { Use to Set Size selection.. makes bad sizes later }
    { PImageX: Integer;
      PImageY: Integer; }
    PFBackGroundColor: Tcolor;
    PBombBackgroundColor: Tcolor;
    PMineBackgroundColor: Tcolor;
    PV1Color: Tcolor;
    PV2Color: Tcolor;
    PV3Color: Tcolor;
    PV4Color: Tcolor;
    PColorset16: integer; { Use to Set Colorset16 selection }
    PColor256S: integer; { Use to Set Color256S selection }
    PColor16Name: string[255]; { array [1..256] of Char; }
    PColor256Name: string[255]; { array [1..256] of Char; }
    PColor256TooName: string[255]; { array [1..256] of Char; }
    { This is my secret string ? as text? }
    PHiddenString: string[255]; { array [1..256] of Char; }
    PFractalDir: string[255];
    PDemsDir: string[255];
    PFormulasDir: string[255];
    PStartedNameNumber: string[25];
    PColorreg: integer;
    PStarted: TDateTime;
    { X Y Location of forms }
    PMainFormX, PMainFormY, PMathFormX, PMathFormY, PFractalFormX,
    PFractalFormY, PDIYStyleFormX, PDIYStyleFormY, PAnnotationFormX,
    PAnnotationFormY, PXYZ3DFormX, PXYZ3DFormY, PFractalPreviewFormX,
    PFractalPreviewFormY, PJPEGFormX, PJPEGFormY, PResizeFormX, PResizeFormY,
    PXYZGLX, PXYZGLY, PMathGLX, PMathGLY, PAboutFormX, PAboutFormY,
    PSystemInfoFormX, PSystemInfoFormY, PMessageX, PMessageY: integer;
  end;

  PrefFile = file of PrefRecord;

var
  MainFormX, MainFormY, MathFormX, MathFormY, FractalFormX, FractalFormY,
  DIYStyleFormX, DIYStyleFormY, AnnotationFormX, AnnotationFormY, XYZ3DFormX,
  XYZ3DFormY, FractalPreviewFormX, FractalPreviewFormY, JPEGFormX, JPEGFormY,
  ResizeFormX, ResizeFormY, XYZGLX, XYZGLY, MathGLX, MathGLY, AboutFormX,
  AboutFormY, SystemInfoFormX, SystemInfoFormY, MessageX, MessageY: integer;

  { FractalFont: TFont; }
  FractalDir, FractalFilename, DemsDir, FormulasDir, DIYTextStorage, DIYFilename,
     HiddenString, StartedNameNumber: string;
  Started: TDateTime;
  Colorreg: integer;
  DisplayButtonsOn, DisplayBoth, HasTrueColors, NotYetShown, isReallyGone: Boolean;

var
  NumbSkullBrain: NoBrainer;
  SkyKing: Sky2D;
  PreRcd: PrefRecord;
  CollageLevel, TurtleLevel, SkyLevel, NumbLevel: integer;
  ImageZoomRatioEx: Extended;
  FXMax: Extended;
  FXMin: Extended;
  FYMax: Extended;
  FYMin: Extended;
  FHQ: Extended;
  FVP: Extended;
  Max_Iterations, Color_Option, Start_Col: integer;

  RGBArray: TMatrix; { 16 RGB's }
  V1Color, V2Color, V3Color, V4Color: DWord;
  BombBackgroundColor, MineBackgroundColor, FBackGroundColor: Tcolor; { DWord; }
  Colorset16: integer;
  Color256S: integer; { selects 1 to be the 256 Colors Array }
  Color16Name: string;
  Color256Name: string; { Color256S:=9; FalseSets;.. load the filename }
  Color256TooName: string;
  ColorArray: array [0 .. 255] of Tcolor; { passing fancy }
  Colors: array [0 .. 2, 0 .. 255] of Byte; { 256 RGB's }

  DIYXY, DIYX2Y2, DIYX3Y3, DIYX4Y4, DIYL: TPoint;
  Triangle: array [1 .. 3] of TPoint;
  Quadtro: array [1 .. 4] of TPoint;
  ManMatrix2, ManMatrix: array of array of Smallint; { Integer; }
  MyPixelFormat: TPixelFormat; { pf24bit pf32bit }
  PixelScanSize: Byte;

  ZDAllRect, ZSARect: TRect;
  { grd.pts:array of array of single;         Manmatrix
    height,width:integer;          FileSizeY    FileSizeX
    maxz:single;                   MaximumElevation
    null:single;                   NullDemValue   MinimumElevation
    cellsize:single;               CellSizeX, CellSizeY,
    originX,OriginY:double;         DemiLeftX1e, DemiTopY1e,
    DemiRightX2e, DemiBottomY2e }
  { flm file format }
  iMadeMountains, NullDemValue: integer;
  FractalFileMatrix: String;
  MinimumElevation, MaximumElevation, ContourInterval, Fractalgorhythym,
    FileSizeX, FileSizeY, VDEM, Vzx, Vzy, Vx, Vy, Vi, Vh, Vg, Viki: integer;
  Vre, Vde: Extended;

  ILorUGridString, ProjectionString, UtmZoneString, DatumString, SpheroidString,
    ZunitsString, { MinimumElevation, MaximumElevation, }
  GridUnitString: String;
  CellSizeX, CellSizeY,
  { DUPLICATE ?  TXminI, TmaxI, TYminI, TYmaxI: Integer; }
  DemiLeftX1e, DemiTopY1e, DemiRightX2e, DemiBottomY2e: Double;
  GridOriginString, CellOriginString: String;

  RotatorCuff, Thinner, ImageZoomRatio, ZoomImageX, ZoomImageY, ZoomOriginX,
    ZoomOriginY, DrawingTool_PBrush_Width, Cropping, Warrior, ITriangling, DIYE,
    DIYZ: integer;
  LastKnownFunction, FYImageX, FYImageY: integer;
  MemoryAvailable, IamDone: integer; { Use to Set Done selection }
  ImageDoneAlarmb, IsNumbREMFast, isNumbAwake, ZoomingOn, bZoomMousing,
    WasZooming, bSaveTheMountains, IsDemLoaded, DoSanMarcosDragon, bMan3d,
    bJulia3d, bJuliaBase, Julia, bFractalMaxed, bBrushHasBitmap, DIYAnnotate,
    bVistaLive, bVista, bMousing, bLightning, bPointing, Fractaling,
    bRotatingImage, bRotateImage: Boolean;

function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: integer): THandle;
procedure Codefx(WhatString: string; Codein: integer);
function C2S(X: Extended): string;
function C2SW(X: DWord): string;
function I2S(X: integer): string;
function myLoadImage: Boolean;

procedure DoLoader;
procedure SetPreferences;
procedure DoSaver;
procedure GetPreferences;

procedure DoMessages(i: integer);
function DoMessagesOK(i: integer): Word;

implementation

uses fAbout, fMain;

function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: integer): THandle;
var
  zFileName, zParams, zDir: array [0 .. 79] of Char;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil,
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
end;

procedure Codefx(WhatString: string; Codein: integer);
var
  CodeS: string;
begin
  if Codein > 0 then
  begin
    str(Codein, CodeS);
    ShowMessage('Error in data Number: ' + #13#10 + CodeS + #13#10 +
      WhatString);
  end;
end;

function C2S(X: Extended): string;
var
  TempStr: string;
begin
  str(X: 24: 18, TempStr);
  C2S := TempStr;
end;

function C2SW(X: DWord): string;
var
  TempStr: string;
begin
  str(X, TempStr);
  C2SW := TempStr;
end;

function I2S(X: integer): string;
var
  TempStr: string;
begin
  str(X, TempStr);
  I2S := TempStr;
end;

function myLoadImage: Boolean;
var
  temp: integer;
  dc: HDC;
begin
  dc := GetDc(0);
  if ((GetDeviceCaps(dc, BITSPIXEL) = 24) or (GetDeviceCaps(dc, BITSPIXEL) = 32))
  then
  begin
    if (GetDeviceCaps(dc, BITSPIXEL) = 24) then
    begin
      MyPixelFormat := pf24bit;
      PixelScanSize := 3;
    end
    else
    begin
      MyPixelFormat := pf32bit; { pf32bit }
      PixelScanSize := 4;
    end;
    temp := ReleaseDc(0, dc); { Give back the screen dc }
    if temp <> 1 then
      Application.Terminate;
    myLoadImage := True;
  end
  else
  begin
    MessageDlg('Requires True Color (24 or 36 bit) Colors Setting',
      mtInformation, [mbOk], 0);
    temp := ReleaseDc(0, dc);
    if temp <> 1 then
      Application.Terminate;
    myLoadImage := False;
  end; { of if 24 bit }
end; { of this }
{ property PixelFormat: TPixelFormat;
  type TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit,
  pf24bit, pf32bit, pfCustom); }
(*
  function myLoadImage: Boolean;
  var
  {temp:DWord;}
  dc: HDC;
  begin
  dc := GetDc(0);
  if ((GetDeviceCaps(dc, BITSPIXEL) = 24)
  or (GetDeviceCaps(dc, BITSPIXEL) = 32)) then begin
  {dc := } ReleaseDc(0, dc); {Give back the screen dc}
  myLoadImage := True;
  end else
  begin
  {    MessageDlg('Requires True Color (24 or 32 bit) Colors Setting',
  mtInformation,[mbOk], 0); }
  {    dc := } ReleaseDc(0, dc);
  myLoadImage := False;
  end; {of if 24 bit}
  end; {of this}
*)

procedure DoLoader;
var
  P_File: PrefFile;
  PathS: string;
begin
  PathS := ExtractFilePath(ParamStr(0)) + 'fractal.pof';
  if FileExists(PathS) then
  begin
    AssignFile(P_File, PathS);
    Reset(P_File);
    if IoResult <> 0 then
      DoMessages(39984);
    Read(P_File, PreRcd);
    CloseFile(P_File);
    SetPreferences;
  end
  else
    DoMessages(39985);
end;

procedure SetPreferences;
begin { after loading }
  with PreRcd do
  begin
    Started := PStarted;
    StartedNameNumber := PStartedNameNumber;
    HiddenString := PHiddenString;
    Colorreg := PColorreg;
    { FractalFont := PFractalFont; }
    { Use to Set Size selection }
    { FYImageX := PImageX;
      FYImageY := PImageY; }
    FBackGroundColor := PFBackGroundColor;
    BombBackgroundColor := PBombBackgroundColor;
    MineBackgroundColor := PMineBackgroundColor;
    V1Color := PV1Color;
    V2Color := PV2Color;
    V3Color := PV3Color;
    V4Color := PV4Color;
    Colorset16 := PColorset16; { Use to Set Colorset16 selection }
    Color256S := PColor256S; { Use to Set Color256S selection }
    Color16Name := PColor16Name;
    Color256Name := PColor256Name;
    Color256TooName := PColor256TooName;
    FractalDir := PFractalDir;
    IamDone := PIamDone;
    DemsDir := PDemsDir;
    FormulasDir := PFormulasDir;
    AboutFormX := PAboutFormX;
    AboutFormY := PAboutFormY;
    SystemInfoFormX := PSystemInfoFormX;
    SystemInfoFormY := PSystemInfoFormY;
    MessageX := PMessageX;
    MessageY := PMessageY;
    MainFormX := PMainFormX;
    MainFormY := PMainFormY;
    MathFormX := PMathFormX;
    MathFormY := PMathFormY;
    FractalFormX := PFractalFormX;
    FractalFormY := PFractalFormY;
    DIYStyleFormX := PDIYStyleFormX;
    DIYStyleFormY := PDIYStyleFormY;
    AnnotationFormX := PAnnotationFormX;
    AnnotationFormY := PAnnotationFormY;
    XYZ3DFormX := PXYZ3DFormX;
    XYZ3DFormY := PXYZ3DFormY;
    FractalPreviewFormX := PFractalPreviewFormX;
    FractalPreviewFormY := PFractalPreviewFormY;
    JPEGFormX := PJPEGFormX;
    JPEGFormY := PJPEGFormY;
    ResizeFormX := PResizeFormX;
    ResizeFormY := PResizeFormY;
    XYZGLX := PXYZGLX;
    XYZGLY := PXYZGLY;
    MathGLX := PMathGLX;
    MathGLY := PMathGLY;
  end;
  { Check and Set the Background Menu }
  MainForm.DoMiner;
  MainForm.DoBomber;
  if (FileExists('MINE.BMP')) then
  begin
    MainForm.Mine1.Bitmap.LoadFromFile('MINE.BMP');
  end;
  if (FileExists('BOMB.BMP')) then
  begin
    MainForm.Bomb1.Bitmap.LoadFromFile('BOMB.BMP');
  end;
end;
{ --------------------------------------------------------------------- }

procedure DoSaver;
var
  P_File: PrefFile;
var
  PathS: string;
begin
  PathS := ExtractFilePath(ParamStr(0)) + 'fractal.pof';
  if (not FileExists(PathS)) then
    ShowMessage('Making Program Options File fractal.pof');
  GetPreferences;
  AssignFile(P_File, PathS);
  Rewrite(P_File);
  if IoResult <> 0 then
    DoMessages(39986)
  else
    write(P_File, PreRcd);
  CloseFile(P_File);
end;
{ --------------------------------------------------------------------- }

procedure GetPreferences;
begin { before saving }
  with PreRcd do
  begin
    { PFractalFont := FractalFont; }
    { Use to Set Size selection }
    { PImageX := FYImageX;
      PImageY := FYImageY; }
    PFBackGroundColor := FBackGroundColor;
    PBombBackgroundColor := BombBackgroundColor;
    PMineBackgroundColor := MineBackgroundColor;

    PV1Color := V1Color;
    PV2Color := V2Color;
    PV3Color := V3Color;
    PV4Color := V4Color;

    PColorset16 := Colorset16; { Use to Set Colorset16 selection }
    PColor256S := Color256S; { Use to Set Color256S selection }
    PColor16Name := Color16Name;
    PColor256Name := Color256Name;
    PColor256TooName := Color256TooName;
    PFractalDir := FractalDir;
    PDemsDir := DemsDir;
    PFormulasDir := FormulasDir;
    PIamDone := IamDone;
    { This is my secret string ? as text? }
    PHiddenString := 'fractl3d Copyright, 2003: Ivan Lee Herring';
    PColorreg := Colorreg;
    PStarted := Started;
    PStartedNameNumber := StartedNameNumber;
    PAboutFormX := AboutFormX;
    PAboutFormY := AboutFormY;
    PSystemInfoFormX := SystemInfoFormX;
    PSystemInfoFormY := SystemInfoFormY;
    PMessageX := MessageX;
    PMessageY := MessageY;
    PMainFormX := MainFormX;
    PMainFormY := MainFormY;
    PMathFormX := MathFormX;
    PMathFormY := MathFormY;
    PFractalFormX := FractalFormX;
    PFractalFormY := FractalFormY;
    PDIYStyleFormX := DIYStyleFormX;
    PDIYStyleFormY := DIYStyleFormY;
    PAnnotationFormX := AnnotationFormX;
    PAnnotationFormY := AnnotationFormY;
    PXYZ3DFormX := XYZ3DFormX;
    PXYZ3DFormY := XYZ3DFormY;
    PFractalPreviewFormX := FractalPreviewFormX;
    PFractalPreviewFormY := FractalPreviewFormY;
    PJPEGFormX := JPEGFormX;
    PJPEGFormY := JPEGFormY;
    PResizeFormX := ResizeFormX;
    PResizeFormY := ResizeFormY;
    PXYZGLX := XYZGLX;
    PXYZGLY := XYZGLY;
    PMathGLX := MathGLX;
    PMathGLY := MathGLY;
  end;
end;
{ --------------------------------------------------------------------- }

(*
  Do_Messages(
  {1..9999 mtInformation	A message box containing a blue "i".}
  {10000..19999 mtConfirmation	A message box containing a green question mark.}
  {20000..29999 mtWarning   A message box containing a yellow exclamation point symbol.}
  {30000..40000 mtError  A message box containing a red stop sign.}

  Codefx(WhatString:String;Codein:Integer);
  Do_Messages(39979);
  {12..9998 mtInformation	A message box containing a blue "i".}
  12:Messagestr:= 'Image not loaded';
  {10001..19999 mtConfirmation	A message box containing a green question mark.}
  {20001..29999 mtWarning   A message box containing a yellow exclamation point symbol.}
  {30001..39989 mtError  A message box containing a red stop sign.}
  39989:Messagestr:='Image less than 1024x1024';
*)

procedure DoMessages(i: integer);
var
  Messagestr: string;
  HelpContextl: Longint;
  { s: array[0..250] of char; }
begin
  HelpContextl := 0;
  case i of { Do_Messages(11 }
    { 1..9999 mtInformation	A message box containing a blue "i". }
    1:
      Messagestr := 'Resistance is futile';
    2:
      Messagestr := 'the Fractal3D program options file' + #13#10 +
        ExtractFilePath(ParamStr(0)) + 'fractal.pof' + #13#10 +
        'Does NOT Exist:... Will Make it';
    9:
      Messagestr := 'Under Construction: nothing happened';
    10:
      Messagestr := 'Image not loaded';
    11:
      Messagestr := 'Requires True Color (24 bit) Colors Setting';
    12:
      Messagestr := 'Requires TRUE COLOR (24 bit) Image';
    13:
      Messagestr := 'Improper Choices: nothing happened';
    14:
      Messagestr := 'I found one';
    15:
      Messagestr := 'Number input wrong,' + #13 + #10 + 'Please try again';
    16:
      Messagestr := 'Registration Number invalid';
    17:
      Messagestr := 'Not Registered!' + #13 + #10 + 'Please register Fractal3D';
    18:
      Messagestr := 'Gotta have a Distance Value';
    19:
      Messagestr := 'Invalid Distance Values';
    20:
      Messagestr := 'Cannot Change unless Loaded';
    21:
      Messagestr := 'Must be between 10 and 255';
    22:
      Messagestr := 'FastDIB MakePalette Crashed < 8bpp';
    23:
      Messagestr := 'renamed File? will procede anyway';
    24:
      Messagestr := 'Image Size not Standard';
    25:
      Messagestr := 'First Load an image to display';
    26:
      Messagestr := 'no font';
    27:
      Messagestr := 'DID NOT!! change File Name';
    28:
      Messagestr := 'Please Select an item to process first';
    { FractalFilename + #13#10 +
      'is not a ' + PixelString + ' bit BMP file' + #13#10 +
      'Please Select a different file' + #13#10 +
      'or change the images''pixel resolution') }
    { DoMessages(28); }
    { 27: Messagestr := '';
      28: Messagestr := '';
      29: Messagestr := '';
      30: Messagestr := '';
      31: Messagestr := '';
      32: Messagestr := '';
      33: Messagestr := '';
      34: Messagestr := '';
      35: Messagestr := ''; }
    36:
      Messagestr := 'Some size is wrong';
    37:
      Messagestr := 'Already Printing';
    38:
      Messagestr := 'Printing aborted';
    39:
      Messagestr := 'RGB Boolean set to Somethingthing!';
    40:
      Messagestr := 'Converts 32 to 24 bit Color images';
    41:
      Messagestr := '4bit to 24 bit bitmapped';
    42:
      Messagestr := '8bit to 24 bit bitmapped';
    43:
      Messagestr := '15bit to 24 bit bitmapped';
    44:
      Messagestr := '16bit to 24 bit bitmapped';
    45:
      Messagestr := '32bit to 24 bit bitmapped';
    46:
      Messagestr := 'There is no image currently loaded to save.';
    { DoMessages(40); }

    9999:
      Messagestr := 'Resistance is futile' + #13#10 + 'All Messages end here';
    { 10000..19999 mtConfirmation	A message box containing a green question mark. }
    10000:
      Messagestr := 'Do you wanna dance?';

    { 10001: Messagestr :=
      'a DIH Header exists, use+append+overwrite instead?'; }
    { 20000..29999 mtWarning   A message box containing a yellow exclamation point symbol. }
    20000:
      Messagestr := 'Danger Danger';
    20001:
      Messagestr := 'Order.txt Files not found';
    20002:
      Messagestr := 'License.txt Files not found';
    20003:
      Messagestr := 'fractal.html Files not found';
    20004:
      Messagestr := 'Fractal3D will fail without Registration' + #13#10 +
        'Registration Warning' + #13#10 + 'See Fractal3D Register';
    { DoMessages( 20004 }
    { 30000..40000 mtError  A message box containing a red stop sign. }
    30000:
      Messagestr := 'There is an Error in the system';
    30001:
      Messagestr := 'Fractal3D failed without Registration' + #13#10 +
        'Registration Error' + #13#10 + 'See Fractal3D Register';
    30002:
      Messagestr := 'Failed Writing Fractal Mountain File';
    30003:
      Messagestr := 'no memory to do this big';
    30004:
      Messagestr := 'Must select a DEM type';
    30005:
      Messagestr := 'DID NOT Close BIN file';
    30006:
      Messagestr := 'NO DEM FILE !?!';
    30007:
      Messagestr := 'NO DEH Header FILE made !?!';
    30008:
      Messagestr := 'Must Input Etopo Columns and Rows data';
    30009:
      Messagestr := '.gz file needs unzipping or renamed to .bin';
    30010:
      Messagestr := 'Corners switched?  Bad Corner Error';
    30011:
      Messagestr := 'NO DEH Header FILE finished !?!';
    30012:
      Messagestr := 'NO HDR FILE !?!  Bad File Error';
    30013:
      Messagestr := 'DID NOT Close Elevation file';
    30014:
      Messagestr := 'Failed Writing Fractal Mountain File';
    30015:
      Messagestr := 'First make something to save';
    30016:
      Messagestr := 'Failed Reading Fractal Mountain File';
    30017:
      Messagestr := 'Failed writing new file';
    30018:
      Messagestr := 'File Ys not equal.. stopped';
    30019:
      Messagestr := 'Must select a Combinental Divide type';
    30020:
      Messagestr := 'Bin file not yet processed';
    30021:
      Messagestr := 'Fractal MetaData file .flm is bad';
    30022:
      Messagestr := 'Mountain MetaData file .rcd is bad';
    { DoMessages(30022); }
    30031:
      Messagestr := 'lost temp file in transit ?!?';
    30041:
      Messagestr := 'No Bitmap loaded from Math Graphics';
    30061:
      Messagestr := 'Mandelbrot set Expansion requires loading' + #13#10 +
        'a Mandelbrot set to select an area from';
    30062:
      Messagestr := 'Mandelbrot set Completion requires loading' + #13#10 +
        'a Mandelbrot set to complete an area of';
    30063:
      Messagestr := 'I am not Julia ... Pat is not here';
    30064:
      Messagestr := 'DID NOT Close 3D file';
    30065:
      Messagestr := 'NO 3D FILE Created!!';
    30066:
      Messagestr := 'Julia Base point Generation requires loading' + #13#10 +
        'a Mandelbrot set to select a point from';
    30067:
      Messagestr := 'Julia Base Point Generation needs' + #13#10 +
        'To know to Mouse to select a Julia Point' + #13#10 +
        'Please use: Generate a Julia Point' + #13#10 +
        'For Generation of Starting Julia Points';
    30068:
      Messagestr := 'Dragon Base point Generation requires loading' + #13#10 +
        'a Dragon set to select a point from';
    30069:
      Messagestr := 'Dragon Base Point Generation needs' + #13#10 +
        'To know to Mouse to select a Dragon Point' + #13#10 +
        'Please use: Generate a Dragon Point' + #13#10 +
        'For Generation of Starting Dragon Points';
    30070:
      Messagestr := 'Phoenix Base point Generation requires loading' + #13#10 +
        'a Phoenix set to select a point from';
    30071:
      Messagestr := 'Phoenix Base Point Generation needs' + #13#10 +
        'To know to Mouse to select a Phoenix Point' + #13#10 +
        'Please use: Generate a Phoenix Point' + #13#10 +
        'For Generation of Starting Phoenix Points';
    30091:
      Messagestr := 'Failed Reading Color File';
    30092:
      Messagestr := 'Color File does not exist';
    30093:
      Messagestr := 'not a .BMP file';

    30101:
      Messagestr := 'Bad Record .rcd file';
    30102:
      Messagestr := 'Failed reading Record File';
    30103:
      Messagestr := 'Failed Writing Record File';
    30111:
      Messagestr := 'Please Select an Iteration amount level';
    30202:
      Messagestr := 'DIY must be set up' + #13#10 +
        'prior to running DIY Symbolizer';
    30203:
      Messagestr := 'A DIY file must be loaded' + #13#10 +
        'prior to running Gear Symbolizer';
    30204:
      Messagestr := 'Gotta enter some kinda degree';
    30205:
      Messagestr := 'Failed achieving shadow form';
    { DoMessages(30205); }
    39967:
      Messagestr := 'Header Crashed: Cancel or fix DIS Subset Width';
    39968:
      Messagestr := 'File unknown';
    39969:
      Messagestr := 'Not a Radar DAT file';
    39970:
      Messagestr := 'Header Crashed: Cancel or fix Number of Bands';
    39971:
      Messagestr := 'Header Crashed: Cancel or fix Coordinates';
    39972:
      Messagestr := 'NO HDR FILE NAME!?! ... Bad File Error';
    39973:
      Messagestr := 'Error - no files in the Zip file';
    39974:
      Messagestr := 'Disk full!?! ... Bad File Error';
    39975:
      Messagestr := 'Image Undone or crashed' + #13#10 +
        'Restart Line Profile or exit';
    39976:
      Messagestr := ' An error was detected when' + #13#10 +
        'trying to fetch registration info.';
    39977:
      Messagestr := 'You Crashed ! OUT of Resources!';
    39978:
      Messagestr := 'Image less than 1024x1024';
    39979:
      Messagestr := 'Image less than 512x512';
    39980:
      Messagestr := 'Image less than 256x256';
    39981:
      Messagestr := 'NO TMP FILE Created!!';
    39982:
      Messagestr := 'DID NOT Close TMP';
    39983:
      Messagestr := 'Filename has reached 999';
    39984:
      Messagestr := 'Failed reading Fractal.pof File';
    39985:
      Messagestr := 'Fractal.pof Does NOT Exist';
    39986:
      Messagestr := 'Failed writing Fractal.pof File';
    39987:
      Messagestr := 'Failed reading TMP File';
    39988:
      Messagestr := 'Failed erasing TMP';
    39989:
      Messagestr := 'HELP on the TMP file' + #13#10 +
        'Unable to find Image Record files ?!?' + #13#10 + 'uk File Error';
    { DoMessages(39989); }
    39999:
      Messagestr := 'There is an Error in the system';
    40000:
      Messagestr := 'FATAL Error creating bitmap... byebye';
  else
    Messagestr := 'Unknown error or message lost' + #13#10 + 'please e-mail me';
  end;

  case i of { –2,147,483,648..2,147,483,647 }
    { mtInformation	A message box containing a blue "i". }
    1 .. 9999:
      MessageDlgPos(Messagestr, mtInformation, [mbOk], { mbYesNoCancel, }
        HelpContextl, MessageX, MessageY);
    { mtConfirmation	A message box containing a green question mark. }
    10000 .. 19999:
      MessageDlgPos(Messagestr, mtConfirmation,
        { [mbOK], } mbYesNoCancel, HelpContextl, MessageX, MessageY);

    { mtWarning   A message box containing a yellow exclamation point symbol. }
    20000 .. 29999:
      MessageDlgPos(Messagestr, mtWarning, [mbOk], { mbYesNoCancel, }
        HelpContextl, MessageX, MessageY);
    { mtError  A message box containing a red stop sign. }
  else { 30000..40000: }
    begin
      (*
        if ErrorBeepOn then Beep(10000, 1000); {Beep;}
        if (ErrorVoiceOn) then
        begin
        StrPCopy(s, ExtractFilePath(ParamStr(0)) + 'ERRORV.WAV');
        {sndPlaySoundA(s, 0);}
        if (MMSysHandle <> 0) then PlaySound(s, {SND_ASYNC} 0);
        end;
      *)
      MessageDlgPos(Messagestr, mtError, [mbOk], { mbYesNoCancel, }
        HelpContextl, MessageX, MessageY);
    end;
  end;
end;

function DoMessagesOK(i: integer): Word;
var
  Messagestr: string;
  HelpContextl: Longint;
  { s: array[0..250] of char; }
begin
  Result := mrNone;
  HelpContextl := 0;
  case i of
    10001:
      Messagestr := 'Could not Print Image... too big' + #13#10 +
        'Try again somewhere else? ... not!' + #13#10 +
        'Will reduce image size while printing ?  OK?';
    10002:
      Messagestr := 'Too big for physical memory. try anyway?';
  end;
  case i of
    { mtConfirmation	A message box containing a green question mark. }
    10000 .. 19999:
      Result := MessageDlgPos(Messagestr, mtConfirmation, mbYesNoCancel,
        HelpContextl, MessageX, MessageY);
  end;
end;
(* TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel,
  mbAbort, mbRetry, mbIgnore,
  mbAll, mnNoToAll, mbYesToAll, mbHelp);
  const
  mbYesNoCancel = [mbYes, mbNo, mbCancel];
  mbOKCancel = [mbOK, mbCancel];
  mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];

  mbYes	A button with 'Yes' on its face.
  mbNo	A button the text 'No' on its face.
  mbOK	A button the text 'OK' on its face.
  mbCancel	A button with the text 'Cancel' on its face.
  mbAbort	A button with the text 'Abort' on its face
  mbRetry	A button with the text 'Retry' on its face
  mbIgnore	A button the text 'Ignore' on its face
  mbAll	A button with the text 'All' on its face
  mbNoToAll	A button with the text 'No to All' on its face
  mbYesToAll	A button with the text 'Yes to All' on its face
  mbHelp	A button with the text 'Help' on its face

  RETURN VALUES
  mrNone	 mrAbort	mrYes
  mrOk	 mrRetry	mrNo
  mrCancel mrIgnore	mrAll
*)
{ --------------------------------------------------------------------- }
(* Begin
  StrPCopy(s, 'DONE.WAV');
  {Play the sound asynchronously}
  sndPlaySound(s, 0); {see mmsystem.hlp for other values}
  {
  If you specify the SND_MEMORY flag, lpszSoundName must point to an
  in-memory image of a waveform sound.
  If the sound is stored as a resource, use
  LoadResource and LockResource to load and lock the resource and get a
  pointer to it.
  }
  {setimage calls the ImageDone}
  {Whistle is a for i:= 100 do
  go up then down then around }
  { Beep( Pitch, Duration );}
  {      Beep(1000,100);}{Order of units must make difference?}
  end; *)

end.
