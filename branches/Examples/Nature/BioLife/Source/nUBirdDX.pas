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
unit nUBirdDX;

interface

uses
  Windows, Messages, SysUtils, Classes, Buttons,
  Graphics, Controls, Forms, Dialogs, Math,
 {   MMSystem,}{Bird song}{  mmsyst = 'winmm.dll';win 98}
  StdCtrls, ExtCtrls, Menus, ComCtrls;
{  DXInput, DXClass, DXDraws;}

type
  TBirdDXForm = class(TForm)
    BirdPanel: TPanel;
    FeedLabel: TLabel;
    SizeLabel: TLabel;
    Label12: TLabel;
    SpeedLabel: TLabel;
    ScaleLabel: TLabel;
    GlideLabel: TLabel;
    ActivityLabel: TLabel;
    FerocityLabel: TLabel;
    DefendLabel: TLabel;
    AvoidLabel: TLabel;
    ViewLabel: TLabel;
    TurnLabel: TLabel;
    HoverLabel: TLabel;
    Label8: TLabel;
    Label3: TLabel;
    Label11: TLabel;
    BirdsEdit: TEdit;
    dBirdsFeedTB: TTrackBar;
    BirdsTailTB: TTrackBar;
    BirdsColorPanel: TPanel;
    BirdsSpeedTB: TTrackBar;
    BirdsScaleTB: TTrackBar;
    BirdsGlideTB: TTrackBar;
    BirdsActivityTB: TTrackBar;
    FerocityTB: TTrackBar;
    TenacityTB: TTrackBar;
    dBirdsAvoidTB: TTrackBar;
    dBirdsViewTB: TTrackBar;
    dBirdsMomentTB: TTrackBar;
    dBirdsMinVTB: TTrackBar;
    dWCopyTB: TTrackBar;
    RCopyTB: TTrackBar;
    RCentroidTB: TTrackBar;
    dWCentroidTB: TTrackBar;
    RVisualTB: TTrackBar;
    dWVisualTB: TTrackBar;
    RAvoidTB: TTrackBar;
    dWAvoidTB: TTrackBar;
    BirdsSizeRG: TRadioGroup;
    BirdsFrenzyBtn: TSpeedButton;
    HelpBtn: TSpeedButton;
    ColorDialog1: TColorDialog;
    PrintitBtn: TSpeedButton;
    DoFranticBtn: TSpeedButton;
    OpenDialog1: TOpenDialog;
    BirdSongPanel: TPanel;
    BirdsBtn: TSpeedButton;
    WingColorPanel: TPanel;
    BirdsNameEdit: TEdit;
    SaveDialog1: TSaveDialog;
    FPSCB: TCheckBox;
    BirdBGCB: TCheckBox;
    CopyLabel: TLabel;
    CenterLabel: TLabel;
    VDistanceLabel: TLabel;
    ADistanceLabel: TLabel;
    AttackBtn: TPanel;
    dWCopyTBLabel: TLabel;
    dWCentroidTBLabel: TLabel;
    dWVisualTBLabel: TLabel;
    dWAvoidTBLabel: TLabel;
    SongEdit: TEdit;
    BirdSongBtn: TSpeedButton;
    DXDraw: TImage;
    StatusBar1: TStatusBar;
    BirdFileBtn: TSpeedButton;
    BirdsSaveBtn: TSpeedButton;
    BirdsOKBtn: TSpeedButton;
    CancelBitBtn: TSpeedButton;
    ExitBtn: TSpeedButton;
    ClearBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReallyClose;

    procedure BirdseyeStartup;
    procedure DXComputeNewHeading(Which: Integer);
    procedure DXDrawBird(Which: Integer);
    procedure CentersinnStartup;
    procedure BoidPerspective(WhichBoid: Integer);
    function BoidIsOnscreen(WhichBoid: Integer): Boolean;
    procedure MoveBoid(WhichBoid: Integer);
    procedure BoidPerceiveCenter(WhichBoid: Integer);
    procedure BoidAvVel(WhichBoid: Integer);
    procedure BoidChillOut(WhichBoid: Integer);
    procedure DrawBoid(WhichBoid: Integer);
    procedure MazepoleStartup;
    procedure DXDrawMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DXDrawMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawObstacles;
    procedure DXCheckCollision;
    procedure MakeOffsetSquares;
    procedure FrilightStartup;
    procedure BoidFPerspective(WhichBoid: Integer);
    function BoidFIsOnscreen(WhichBoid: Integer): Boolean;
    procedure MoveFBoid(WhichBoid: Integer);
    procedure BoidFPerceiveCenter(WhichBoid: Integer);
    procedure BoidFAvVel(WhichBoid: Integer);
    procedure BoidFChillOut(WhichBoid: Integer);
    procedure DrawFBoid(WhichBoid: Integer);
    procedure FeedrenzyStartup;
    procedure MoveFFBoid(WhichBoid: Integer);
    procedure BoidFeed(WhichBoid: Integer);
    procedure PredrenzyStartup;
    procedure BoidSeek;
    procedure PerspectiveHunter;
    procedure DrawHunter;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
procedure RepeatUntilBirdlanded;

    procedure dBirdsFeedTBChange(Sender: TObject);
    procedure BirdsTailTBChange(Sender: TObject);
    procedure BirdsSpeedTBChange(Sender: TObject);
    procedure BirdsScaleTBChange(Sender: TObject);
    procedure BirdsGlideTBChange(Sender: TObject);
    procedure BirdsActivityTBChange(Sender: TObject);
    procedure FerocityTBChange(Sender: TObject);
    procedure TenacityTBChange(Sender: TObject);
    procedure dBirdsMinVTBChange(Sender: TObject);
    procedure dBirdsMomentTBChange(Sender: TObject);
    procedure dBirdsViewTBChange(Sender: TObject);
    procedure dBirdsAvoidTBChange(Sender: TObject);
    procedure dWCopyTBChange(Sender: TObject);
    procedure RCopyTBChange(Sender: TObject);
    procedure RCentroidTBChange(Sender: TObject);
    procedure dWCentroidTBChange(Sender: TObject);
    procedure RVisualTBChange(Sender: TObject);
    procedure dWVisualTBChange(Sender: TObject);
    procedure RAvoidTBChange(Sender: TObject);
    procedure dWAvoidTBChange(Sender: TObject);
    procedure BirdFileOpen(BirdFileName: string);
    procedure SetBirdsArraytoForm;
    procedure BirdsLoadedLabel;
    procedure SetBirdsDefault;

    procedure BirdsSaveBtnClick(Sender: TObject);
    procedure SaveBirdsFile;
    procedure ReadBirdsData;
    procedure BirdsOKBtnClick(Sender: TObject);
    procedure HideTheMenu;
    procedure CancelBitBtnClick(Sender: TObject);
    procedure EnableAll;
    procedure VisibleAll;
    procedure HelpBtnClick(Sender: TObject);
    procedure BirdsFrenzyBtnClick(Sender: TObject);
    procedure FrenzyFilesArray;
    procedure SetBirdsDatatoArray;

    procedure BirdsColorPanelClick(Sender: TObject);
    procedure BirdSongClick(Sender: TObject);
    procedure PrintitBtnClick(Sender: TObject);
    procedure AttackBtnClick(Sender: TObject);
    procedure DoFranticBtnClick(Sender: TObject);
    procedure BirdsBtnClick(Sender: TObject);
    procedure WingColorPanelClick(Sender: TObject);
    procedure BirdFileBtnClick(Sender: TObject);

    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BirdSongBtnClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
  private
     

  public
     
    function Leni(x, y: Integer): Integer;
    function Lend(x, y: Double): Double;
    function Disti(x1, y1, x2, y2: Integer): Integer;
    function Distd(x1, y1, x2, y2: Double): Double;
    function Doti(x1, y1, x2, y2: Integer): Integer;
    function Dotd(x1, y1, x2, y2: Double): Double;
    procedure NormDX(x, y: Double);
  end;

var
  BirdDXForm: TBirdDXForm;
{  FrenzyFilesLoaded: Boolean;}
  FrenzyFilesName: string;
implementation
uses nUGlobal, GlsFrenzy;
{uses MMSystem;}

{$R *.DFM}
/////////////////////////////////////////////////////
type
  Vec = record
    x, y, z: double;
  end;
  Obstacle = record
    x1, x2, y1, y2, Z: Integer;
  end;

  aBoid = record
    pos, vel: Vec;
    shadow: array[0..3] of TPoint;
    onscreen, upstroke, perching: boolean;
    tail_lX, tail_lY, tail_rX, tail_rY, tail_X, tail_Y,
      X, Y, WingLevel, PerchTimer: Integer;
  end;
  DXBoid = record
    pos, vel: Vec;
    shadow: array[0..3] of TPoint;
    onscreen, upstroke, Hungry,
      Seeker, perching: boolean;
    tail_lX, tail_lY, tail_rX, tail_rY, tail_X, tail_Y,
      X, Y, WingLevel, HungryTimer, PerchTimer: Integer;
  end;
  HunterBoid = record
    pos, vel: Vec;
    shadow: array[0..3] of TPoint;
    BirdWingColorDX, BirdsColorDX: TColor;
    onscreen, upstroke: boolean;
    tail_lX, tail_lY, tail_rX, tail_rY, tail_X, tail_Y, X, Y,
      BirdsTailDX, TailSize, TailWideSize, WingLevel: Integer;
  end;
  BirdBrainRecord = record
    BirdName: string[15];
{  BirdSong:Integer;}
    CurrentBirdSong,
      BirdBodyFile, BirdTexture: string[255];
    BirdWingColorDX,
      BirdsColorDX: TColor;
    BirdsTenacityDX,
      BirdsFerocityDX,
      BirdsActivityDX,
      BirdsSpeedDX,
      BirdsWideDX,
      BirdsHeightDX,
      BirdsDX {num},
      BirdsTailDX {len},
      TailSize, {50} TailWideSize {60},
      RCopyDX {rcopy},
      RCentroidDX {rcent},
      RAvoidDX {rvoid},
      RVisualDX {rviso}: Integer;

    dBirdxTempDX,
      dBirdyTempDX,
      dBirdsScaleDX,
      dBirdsFeedDX,
      dBirdsEnergyDX,
      dBirdsMinVDX {minv},
      dBirdsMomentDX {ddt},
      dBirdsViewDX {angle},
      dBirdsAvoidDX {vangle},
      dWCopyDX {wcopy},
      dBirdsGlideDX,
      dWCentroidDX {wcent},
      dWAvoidDX {wvoid},
      dWVisualDX {wviso}
      : Double;
    RealCenter, RealAvgvel: Vec;
  end;
{BirdBrainFile=File of BirdBrainRecord;}


var
  ChaseIt,
    DoBirdseye, DoCentersinn, DoMazepole,
    DoFrilight, DoFeedrenzy, DoPredrenzy, BirdSinging: Boolean;
var
  BirdsDXP, BirdsDYP, {BirdsDZP,}
    BirdsDXV, BirdsDYV, {BirdsDZV,}
    BirdsDXnV, BirdsDYnV {,BirdsDZnV}: array of Double;
  BirdBrainArray: array of BirdBrainRecord;
  BoidArray: array of array of DXBoid;
  Hunter: HunterBoid;
{aBirdBrainRecord:BirdBrainRecord;}

  CurrentBirdName,
    CurrentBirdBodyFile, CurrentBirdTexture, CurrentBirdSong: string;
{BirdBrains,}
{CurrentSong,}
CurrentBirdsRandomosity,
  Label1Left, Label1Top,
    WhichFlockisFlying,
    CurrentBird, FrenzyCount, CurrentFrenzy, FrenzyBirdCount,
    BirdsTenacityDX,
    BirdsFerocityDX,
    BirdsActivityDX,
    BirdsSpeedDX,
    BirdsWideDX,
    BirdsHeightDX,
    BirdsDX {num},
    BirdsTailDX {len},
{TailEndSize,}{40} TailSize, {50} TailWideSize {60},
    RCopyDX {rcopy},
    RCentroidDX {rcent},
    RAvoidDX {rvoid},
    RVisualDX {rviso}: Integer;

  BirdWingColorDX,
    BirdsColorDX: TColor;
dBirdsSexy,
dBirdsEnergy,
  dBirdxTempDX, dBirdyTempDX,
{dBirdxTemp,dBirdyTemp,}
  dBirdsFeedDX,
    dBirdsScaleDX,
    dBirdsMinVDX {minv},
    dBirdsGlideDX,
    dBirdsMomentDX {ddt},
    dBirdsViewDX {angle},
    dBirdsAvoidDX {vangle},
    dWCopyDX {wcopy},
    dWCentroidDX {wcent},
    dWAvoidDX {wvoid},
    dWVisualDX {wviso}: Double;



  Boid: array of aBoid;
  CrashCount,
    BirdsHeightGround,
    HowManyBirds: Integer;
  Feeding, Feeder,
    tail, tail_end: Vec; {BoidPerspective}
  Movecenter, CenterBias,
    MoveAvgvelocity, AvgvelBias,
    chilling: Vec; {MoveBoid}
  chill, bigchill: Vec; {BoidChillOut}
  PercAvgvel: Vec; {BoidAvVel}
  PercCenter: Vec; {BoidPerceiveCenter}
  RealCenter, RealAvgvel: Vec; {CentersinnStartup}
  outlines: array[0..33] of TColor;
  BirdWingColorDown: Tcolor;
  ObstacleCount: Integer;
  ObstacleArray: array of Obstacle;
  BackGroundStr: string;
  StartX, StartY: Integer; {MazePole mouse location}
  TerrainBuffer: array of array of Double;

  MMSysHandle: THandle;
  PlaySound: function(lpszSoundName: PAnsiChar; uFlags: UINT): BOOL;
    stdcall;
/////////////////////////////////////////////////////

function TBirdDXForm.Leni(x, y: Integer): Integer;
begin
  Leni := round(sqrt(SQR(x) + SQR(y)));
end;

function TBirdDXForm.Lend(x, y: Double): Double;
begin
  Lend := (sqrt(SQR(x) + SQR(y)));
end;

function TBirdDXForm.Disti(x1, y1, x2, y2: Integer): Integer;
begin
  Disti := LENi(((x1) - (x2)), ((y1) - (y2)));
end;

function TBirdDXForm.Distd(x1, y1, x2, y2: Double): Double;
begin
  Distd := Lend(((x1) - (x2)), ((y1) - (y2)));
end;

function TBirdDXForm.Doti(x1, y1, x2, y2: Integer): Integer;
begin
  Doti := ((x1) * (x2) + (y1) * (y2))
end;

function TBirdDXForm.Dotd(x1, y1, x2, y2: Double): Double;
begin
  Dotd := ((x1 * x2) + (y1 * y2))
end;

// Destructively normalize a vector.
procedure TBirdDXForm.NormDX(x, y: Double);
var Len: Double;
begin
  Len := LENd(x, y);
  if (len <> 0.0) then
  begin
    dBirdxTempDX := (x / len);
    dBirdyTempDX := (y / len);
  end else
  begin
    dBirdxTempDX := x;
    dBirdyTempDX := y;
  end;
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TBirdDXForm.BirdSongClick(Sender: TObject);
var
  s: array[0..245] of AnsiChar;
begin
  {If true then sing and change btn color}
  BirdSinging := (not BirdSinging);
  if BirdSinging then
  begin
    if ((FileExists(BirdLifeDir + CurrentBirdSong))
      and (lowercase(ExtractFileExt(CurrentBirdSong)) = '.wav'))
      then
    begin
      BirdSongPanel.Color := BirdsColorPanel.Color;
      BirdSinging := True;
      StrPCopy(s, BirdLifeDir + CurrentBirdSong);
        {sndPlaySound(s, 0);}
      if (MMSysHandle <> 0) then PlaySound(s, {SND_ASYNC} 0);
    end else
    begin
      BirdSongPanel.Color := clBtnFace;
      BirdSinging := False;
      ShowMessage('BirdSong File Does NOT Exist!'
        + #13#10 + BirdLifeDir + CurrentBirdSong);
    end;
  end else
  begin
    BirdSongPanel.Color := clBtnFace;
    BirdSinging := False;
  end;
end;



/////////////////////////////////////////////////////

procedure TBirdDXForm.FormCreate(Sender: TObject);

begin
  top := BirdDXFormY;
  left := BirdDXFormX;
  WhichFlockisFlying := -1;
  SetLength(BirdBrainArray, 1);
{  BackGroundStr := 'back400.bmp';
  BirdSurface := TBitMap.Create;
  BirdSurface.LoadFromFile(ExtractFilePath(ParamStr(0)) +BackGroundStr);
  DXDraw.Picture.Bitmap.Assign(BirdSurface); }
  DoBirdseye := False;
  DoCentersinn := False;
  DoMazepole := False;
  DoFrilight := False;
  DoFeedrenzy := False;
  DoPredrenzy := False;
  BirdSinging := False;
  FrenzyFilesLoaded := False;
{CurrentSong:=0;}
{CurrentBirdSong}
  CurrentBirdBodyFile := 'None yet';
  CurrentBirdTexture := 'None yet';
  SetBirdsDefault;
{ DXTimer.Enabled := False;   }
{  Saved8087CW := Default8087CW;
  Set8087CW($133f); // Disable all fpu exceptions}

  MMSysHandle := LoadLibrary('WINMM.DLL');
  if (MMSysHandle <> 0) then
  begin
    @PlaySound := GetProcAddress(MMSysHandle, 'sndPlaySoundA');
  end else
  begin
    MMSysHandle := LoadLibrary('MMSYSTEM.DLL');
    if (MMSysHandle <> 0) then
    begin
      @PlaySound := GetProcAddress(MMSysHandle, 'sndPlaySound');
    end;
  end;
  if (MMSysHandle = 0) then
  begin
    ShowMessage('Cannot locate WINMM.DLL or MMSYSTEM.DLL.. no sounds');
{SoundDllLocated:=False;}
  end {else  SoundDllLocated:=True};
end;

procedure TBirdDXForm.FormShow(Sender: TObject);
begin
  DXDraw.Canvas.Brush.Color := clBtnFace;
  DXDraw.Canvas.Brush.Style := bsSolid;
  SetBirdsArraytoForm; {Converts to decimals}
  BirdsLoadedLabel;
end;

procedure TBirdDXForm.ReallyClose;
begin
{     If BirdSinging then DXWaveList1.Items[0].WaveCollection[0].Stop;}
  BirdLandDX := True;
  SetLength(BirdsDXP, 0);
  SetLength(BirdsDYP, 0);
  SetLength(BirdsDXV, 0);
  SetLength(BirdsDYV, 0);
  SetLength(BirdsDXnV, 0);
  SetLength(BirdsDYnV, 0);
  SetLength(BirdBrainArray, 0);
  SetLength(BoidArray, 0, 0);
  Setlength(boid, 0);
  SetLength(ObstacleArray, 0);
 { BirdSurface.Free;}
{  Set8087CW(Saved8087CW);}
  Close;
end;

procedure TBirdDXForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
{  If BirdSinging then DXWaveList1.Items[0].WaveCollection[0].Stop;}
  BirdLandDX := True;
  BirdDXForm.BirdsOKBtn.Enabled := True;
  DoBirdseye := False;
  DoCentersinn := False;
  DoMazepole := False;
  DoFrilight := False;
  DoFeedrenzy := False;
  DoPredrenzy := False;
  CanClose := True;
end;

procedure TBirdDXForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
begin
  BirdLandDX := True;
  BirdDXFormY := BirdDXForm.top;
  BirdDXFormX := BirdDXForm.left;
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TBirdDXForm.FormResize(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
  Bitmap.Height:= DXDraw.Height;
  Bitmap.Width:= DXDraw.Width;
  DXDraw.Picture.Bitmap.Assign(Bitmap);
  BirdsWideDX := DXDraw.Width;
  BirdsHeightDX := DXDraw.Height;
  DXDraw.Canvas.Pen.Color := clWhite;
  DXDraw.Canvas.Brush.Color := clWhite;
	  {DXDraw.Canvas.FillRect(Canvas.ClipRect);}
  DXDraw.Canvas.FillRect(Rect(0,0,BirdsWideDX,BirdsHeightDX));
  finally
    Bitmap.Free;
  end;
  DXDraw.Invalidate;
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TBirdDXForm.BirdseyeStartup;
var I: Integer;
begin
{     dt:=3.0; }
  Randomize;
  BirdLandDX := False;
  dBirdsViewDX := dBirdsViewDX * dDegToRad;
  dBirdsAvoidDX := dBirdsAvoidDX * dDegToRad;
    // Make space for the positions, velocities, and new velocities. */
  SetLength(BirdsDXP, BirdsDX);
  SetLength(BirdsDYP, BirdsDX);
  SetLength(BirdsDXV, BirdsDX);
  SetLength(BirdsDYV, BirdsDX);
  SetLength(BirdsDXnV, BirdsDX);
  SetLength(BirdsDYnV, BirdsDX);
  // Set to random initial conditions. */
  for I := 0 to BirdsDX - 1 do
  begin
    BirdsDXP[I] := Random(BirdsWideDX - 1);
    BirdsDYP[I] := Random(BirdsHeightDX - 1);
    BirdsDXV[I] := Random(3) - 1;
    BirdsDYV[I] := Random(3) - 1;
    NormDX(BirdsDXV[I], BirdsDYV[I]);
    BirdsDXV[I] := dBirdxTempDX;
    BirdsDYV[I] := dBirdyTempDX;
  end;
  RepeatUntilBirdlanded;
end;

// Compute the heading for a particular boid
// based on its current environment.
procedure TBirdDXForm.DXComputeNewHeading(Which: Integer);
var
  i, j, k, numcent: Integer;
  mindist, mx, my, d,
  cosangle, cosvangle, costemp,
  xtemp, ytemp, maxr, u, v,
  xa, ya, xb, yb, xc, yc, xd, yd, xt, yt: double;
  da: array[1..9] of double;
begin
  numcent := 0;
  mx := 0;
  my := 0;
  // This is the maximum distance in which any rule is activated.
  maxr := MAX(RVisualDX, MAX(RCopyDX, MAX(RCentroidDX, RAvoidDX)));

  // These two values are used to see if a boid can "see" another
  // boid in various ways.
  cosangle := cos(dBirdsViewDX / 2);
  cosvangle := cos(dBirdsAvoidDX / 2);

  //* These are the accumulated change vectors for the four rules. */
 xa := 0; ya := 0; xb := 0; yb := 0; xc := 0; yc := 0; xd := 0; yd:= 0;

  //* For every boid...
  for i := 0 to BirdsDX - 1 do
  begin
    //* Don't include self for computing new heading. */
    if (i = which) then continue;
    {* Since we want boids to "see" each other around the borders of
     * the screen, we need to check if a boid on the left edge is
     * actually "close" to a boid on the right edge, etc.  We do this
     * by searching over nine relative displacements of boid(i) and
     * pick the one that is closest to boid(which).  These coordinates
     * are then used for all remaining calculations.}
    mindist := 10E10;
{    If ( (BirdsDXP[which]<10)or
         (BirdsDYP[which]< 10)or
         (BirdsDXP[which]> (BirdsWideDX-10))or
         (BirdsDYP[which]> (BirdsHeightDX- 10))   ) then}
{    begin
     da[1] := DISTd((BirdsDXP[i]),(BirdsDYP[i]),BirdsDXP[which],BirdsDYP[which]);
     d:=da[1];
     da[2] := DISTd((BirdsDXP[i] + BirdsWideDX),(BirdsDYP[i] + BirdsHeightDX),BirdsDXP[which],BirdsDYP[which]);
     da[3] := DISTd((BirdsDXP[i] +BirdsWideDX),(BirdsDYP[i] -BirdsHeightDX),BirdsDXP[which],BirdsDYP[which]);
     da[4] := DISTd((BirdsDXP[i] +BirdsWideDX),(BirdsDYP[i] ),BirdsDXP[which],BirdsDYP[which]);
     da[5] := DISTd((BirdsDXP[i] - BirdsWideDX),(BirdsDYP[i] + BirdsHeightDX),BirdsDXP[which],BirdsDYP[which]);
     da[6] := DISTd((BirdsDXP[i] -BirdsWideDX),(BirdsDYP[i] -BirdsHeightDX),BirdsDXP[which],BirdsDYP[which]);
     da[7] := DISTd((BirdsDXP[i] -BirdsWideDX),(BirdsDYP[i] ),BirdsDXP[which],BirdsDYP[which]);
     da[8] := DISTd((BirdsDXP[i] ),(BirdsDYP[i] + BirdsHeightDX),BirdsDXP[which],BirdsDYP[which]);
     da[9] := DISTd((BirdsDXP[i] ),(BirdsDYP[i] -BirdsHeightDX),BirdsDXP[which],BirdsDYP[which]);
     for j:=1 to 9 do If (d<da[j]) then d:= da[j];
     if(d < mindist) then begin
          mindist := d;
          mx := BirdsDXP[i];
          my := BirdsDYP[i];
        end;
      end else}
    begin
      d := DISTd(BirdsDXP[i], BirdsDYP[i],
        BirdsDXP[which], BirdsDYP[which]);
      if (d < mindist) then begin
        mindist := d;
        mx := BirdsDXP[i];
        my := BirdsDYP[i];
      end;
    end;

    // If that distance is farther than any of the rule radii,
    // then skip.
    if (mindist > maxr) then continue;

    //* Make a vector from boid(which) to boid(i).
    xtemp := mx - BirdsDXP[which];
    ytemp := my - BirdsDYP[which];

    //* Calculate the cosine of the velocity vector of boid(which)
     //* and the vector from boid(which) to boid(i).
    costemp := DOTd(BirdsDxv[which], BirdsDyv[which], xtemp, ytemp)
      / (LENd(BirdsDxv[which], BirdsDyv[which])
      * LENd(xtemp, ytemp));

    //* If this cosine is less than the cosine of one half
    // * of the boid's eyesight, i.e., boid(which) cannot see
   //  * boid(i), then skip.
    if (costemp < cosangle) then continue;
    //* If the distance between the two boids is within the radius
     //* of the centering rule, but outside of the radius of the
     //* avoidance rule, then attempt to center in on boid(i).
    if ((mindist <= RCentroidDX) and (mindist > RAvoidDX)) then
    begin
      xa := xa + mx - BirdsDxp[which];
      ya := ya + my - BirdsDyp[which];
      inc(numcent); {? numcent++}
    end;

    //* If we are close enough to copy, but far enough to avoid,
     //* then copy boid(i)'s velocity.
    if ((mindist <= RCopyDX) and (mindist > RAvoidDX)) then
    begin
      xb := xb + BirdsDxv[i];
      yb := yb + BirdsDyv[i];
    end;

    //* If we are within collision range, then try to avoid boid(i).
    if (mindist <= RAvoidDX) then
    begin
      //* Calculate the vector which moves boid(which) away from boid(i).
      xtemp := BirdsDxp[which] - mx;
      ytemp := BirdsDyp[which] - my;
      //* Make the length of the avoidance vector inversely proportional
      //* to the distance between the two boids.
      d := 1 / LENd(xtemp, ytemp);
      xtemp := xtemp * d;
      ytemp := ytemp * d;
      xc := xc + xtemp;
      yc := yc + ytemp;
    end;

    //* If boid(i) is within rviso distance and the angle between this boid's
     //* velocity vector and the boid(i)'s position relative to this boid is
     //* less than vangle, then try to move so that vision is restored.
    if ((mindist <= RVisualDX) and (cosvangle < costemp)) then
    begin
      //* Calculate the vector which moves boid(which) away from boid(i). */
      xtemp := BirdsDxp[which] - mx;
      ytemp := BirdsDyp[which] - my;
      //* Calculate another vector that is orthogonal to the previous,
       //* But try to make it in the same general direction of boid(which)'s
       //* direction of movement.
      u := 0;
      v := 0;
      if ((xtemp <> 0) and (ytemp <> 0)) then
      begin
        u := sqrt(SQR(ytemp / xtemp) / (1 + SQR(ytemp / xtemp)));
        v := -xtemp * u / ytemp;
      end
      else if (xtemp <> 0) then u := 1
      else if (ytemp <> 0) then v := 1;
      if ((BirdsDxv[which] * u + BirdsDyv[which] * v) < 0) then
      begin
        u := -u;
        v := -v;
      end;
      // Add the vector that moves away from boid(i). */
      u := BirdsDXP[which] - mx + u;
      v := BirdsDYP[which] - my + v;

      //* Make this vector's length inversely proportional to the
       //* distance between the two boids.

      d := LENd(xtemp, ytemp);
      if (d <> 0) then
      begin
        u := u / d;
        v := v / d;
      end;

      xd := xd + u;
      yd := yd + v;
    end; {Visually avoid}

  end; {All birds}

  //* Avoid centering on only one other boid;
   //* it makes you look aggressive!
  if (numcent < 2) then
  begin
    xa := 0;
    ya := 0;
  end;
  //* Normalize all big vectors. */
  if (LENd(xa, ya) > 1.0) then
  begin
    NormDX(xa, ya); xa := dBirdxTempDX; ya := dBirdyTempDX;
         {norm(&xa, &ya);}
  end;
  if (LENd(xb, yb) > 1.0) then
  begin
    NormDX(xb, yb); xb := dBirdxTempDX; yb := dBirdyTempDX;
       {norm(&xb, &yb);}
  end;
  if (LENd(xc, yc) > 1.0) then
  begin
    NormDX(xc, yc); xc := dBirdxTempDX; yc := dBirdyTempDX;
         {norm(&xc, &yc);}
  end;
  if (LENd(xd, yd) > 1.0) then
  begin
    NormDX(xd, yd); xd := dBirdxTempDX; yd := dBirdyTempDX;
         {norm(&xd, &yd); }
  end;

  //* Compute the composite trajectory based on all of the rules. */
  xt := xa * dWCentroidDX + xb * dWCopyDX + xc * dWAvoidDX + xd *
    dWVisualDX;
  yt := ya * dWCentroidDX + yb * dWCopyDX + yc * dWAvoidDX + yd *
    dWVisualDX;

  //* Optionally add some noise. */
{  if(dBirdsRandomDX > 0.02) then
  begin
    xt := (xt+((random(3)-1) * dBirdsRandomDX));
    yt := (yt+((random(3)-1) * dBirdsRandomDX));
  end;    }

  //* Update the velocity and renormalize if it is too small. */
  BirdsDxnv[which] := BirdsDxv[which] * dBirdsMomentDX +
                      xt * (1 - dBirdsMomentDX);
  BirdsDynv[which] := BirdsDyv[which] * dBirdsMomentDX +
                      yt * (1 -dBirdsMomentDX);
  d := LENd(BirdsDxnv[which], BirdsDynv[which]);
  if (d < dBirdsMinVDX) then
  begin
    {  if d < 0.01 then d:=0.01; }
    BirdsDxnv[which] := BirdsDxnv[which] * (dBirdsMinVDX / d);
    BirdsDynv[which] := BirdsDynv[which] * (dBirdsMinVDX / d);
  end;
end;


procedure TBirdDXForm.DXDrawBird(Which: Integer);
var wx2, wy2,
{  wx1,  wx3, wx4,wy1,wy3, wy4,}
  x1, x2, x3, x4, y1, y2, y3, y4, a, t: double;
begin
  //* Plot a line in the direction that it is heading. */
  x3 := BirdsDXV[which];
  y3 := BirdsDYV[which];
  NormDX(x3, y3); x3 := dBirdxTempDX; y3 := dBirdyTempDX;
{  norm(&x3, &y3);}
  x1 := BirdsDxp[which];
  y1 := BirdsDyp[which];
  x2 := x1 - x3 * BirdsTailDX;
  y2 := y1 - y3 * BirdsTailDX;
  wx2 := x1 - x3 * (BirdsTailDX / 2);
  wy2 := y1 - y3 * (BirdsTailDX / 2);
{    DXDraw.Surface.Canvas.Pixels[X, Y] := clBlue; }
         DXDraw.{Surface.}Canvas.MoveTo(round(x1), round(y1));
         DXDraw.{Surface.}Canvas.LineTo(round(x2), round(y2));
  //* Plot the head of the boid, with the angle of the arrow head
   //* indicating its viewing angle.

  t := (x1 - x2) / BirdsTailDX;
  {  t := (t < -1) ? -1 : (t > 1) ? 1 : t;  }
  if (t < -1) then t := -1 else if (t > 1) then t := 1;
  a := arccos(t);
  {a := (y1 - y2) < 0 ? -a : a;}
  if ((y1 - y2) < 0) then a := -a;

  //* This is for the right portion of the head. */
  x3 := x1 + cos(a + 130 {dBirdsViewDX / 2}) * BirdsTailDX {/ 3.0};
  y3 := y1 + sin(a + 130 {dBirdsViewDX / 2}) * BirdsTailDX {/ 3.0};
{  plot_line(x1, y1, x3, y3, color);}
         DXDraw.{Surface.}Canvas.MoveTo(round(x1), round(y1));
         DXDraw.{Surface.}Canvas.LineTo(round(x3), round(y3));
  //* This is for the left portion of the head. */
  x4 := x1 + cos(a - 130 {dBirdsViewDX / 2}) * BirdsTailDX {/ 3.0};
  y4 := y1 + sin(a - 130 {dBirdsViewDX / 2}) * BirdsTailDX {/ 3.0};
         DXDraw.{Surface.}Canvas.MoveTo(round(x1), round(y1));
         DXDraw.{Surface.}Canvas.LineTo(round(x4), round(y4));
         DXDraw.{Surface.}Canvas.LineTo(round(x3), round(y3));
  DXDraw.{Surface.}Canvas.Pen.Color := BirdWingColorDX;
  DXDraw.{Surface.}Canvas.Brush.Color := BirdWingColorDX;
  DXDraw.{Surface.}Canvas.Polygon([Point(round(x1), round(y1)),
    Point(round(x3), round(y3)),
      Point(round(wx2), round(wy2)),
      Point(round(x4), round(y4)),
      Point(round(x1), round(y1))]);

  DXDraw.{Surface.}Canvas.Pen.Color := BirdsColorDX;
  DXDraw.{Surface.}Canvas.Brush.Color := BirdsColorDX;
  x3 := x1 + cos(a + 135 {dBirdsViewDX / 2}) * BirdsTailDX {/ 3.0};
  y3 := y1 + sin(a + 135 {dBirdsViewDX / 2}) * BirdsTailDX {/ 3.0};
  x4 := x1 + cos(a - 135 {dBirdsViewDX / 2}) * BirdsTailDX {/ 3.0};
  y4 := y1 + sin(a - 135 {dBirdsViewDX / 2}) * BirdsTailDX {/ 3.0};
  DXDraw.{Surface.}Canvas.Polygon([Point(round(x1), round(y1)),
    Point(round(x3), round(y3)),
      Point(round(x2), round(y2)),
      Point(round(x4), round(y4)),
      Point(round(x1), round(y1))]);
end;


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TBirdDXForm.CentersinnStartup;
var
  Rd, Gd, Bd, Ri, Gi, Bi,
    i: Integer;
  R, G, B: Byte;
begin {}
  BackGroundStr := 'back400.bmp';
  Randomize;
  BirdLandDX := False;
  CrashCount := 0;
{BirdsTenacityDX ChillFactor:=10;}{Keep away distance}
{BirdsSpeedDX BoidSpeedLimit:=100;}{Max Speed}
  i := (BirdsHeightDX div 5);
  BirdsHeightGround := (i * 4);
{Scale  ??? BirdsHeight or BirdsWideDX  /zfactor
BirdsActivityDX
      boid[WhichBoid].perchtimer := (Random(20) + 30);
      perchtimer de increment amount
      boid[WhichBoid].winglevel := 30;
      +- 30 = 60 swath}
  TailWideSize := (BirdsTailDX + (BirdsTailDX div 2));
  TailSize := (BirdsTailDX + ((BirdsTailDX - TailWideSize) div 2));
{BirdsTailDX TailSize:=25;}{50}
{TailWideSize:=30;}{60}
{TailEndSize:=20;}{40}
{DefaultCenterBias:= 7; BirdsGlideDX}
{BirdsEnergyDX DefaultAvgVelocity:=     3;}
{dWAvoidDX  DefaultChilling:=    1;}
  HowManyBirds := BirdsDX;
  Setlength(boid, HowManyBirds);
  for i := 0 to HowManyBirds - 1 do
  begin
   {X and Y can be negatives around screen center}
    boid[i].pos.x := Random((BirdsWideDX * 2) - BirdsWideDX);
    boid[i].pos.y := Random((BirdsHeightDX * 2) - BirdsHeightDX);
   {Z needs to be way positive to see in perspective}
    boid[i].pos.z := Random((BirdsWideDX * 2)) + (BirdsHeightDX);
    boid[i].vel.x := Random(51) - 25; {Negative means flying left...}
    boid[i].vel.y := Random(51) - 25; {up.. down}
    boid[i].vel.z := Random(51) - 25; {front to back}
    boid[i].WingLevel := Random(TailWideSize) - 20;
    BoidPerspective(i);
  end;
  RealCenter.x := 0; RealCenter.y := 0; RealCenter.z := 0;
  RealAvgvel.x := 0; RealAvgvel.y := 0; RealAvgvel.z := 0;

  Rd := ((GetRValue(BirdWingColorDX)
    - (255 - GetRValue(BirdWingColorDX)))
    div 32);
  Gd := ((GetGValue(BirdWingColorDX)
    - (255 - GetGValue(BirdWingColorDX)))
    div 32);
  Bd := ((GetBValue(BirdWingColorDX)
    - (255 - GetBValue(BirdWingColorDX)))
    div 32);
  for i := 0 to 32 do begin
{      R:=( 255 - (i * 7));  G:=( 255 - (i * 7));  B:=( 255 - (i * 7));}
    Ri := (BirdWingColorDX - Rd);
    if Ri < 1 then Ri := 1; if Ri > 255 then Ri := 255; R := Ri;
    Gi := (BirdWingColorDX - Gd);
    if Gi < 1 then Gi := 1; if Gi > 255 then Gi := 255; G := Gi;
    Bi := (BirdWingColorDX - Bd);
    if Bi < 1 then Bi := 1; if Gi > 255 then Bi := 255; B := Bi;
    outlines[i] := RGB(R, G, B);
  end;
  R := (255 - GetRValue(BirdWingColorDX));
  G := (255 - GetGValue(BirdWingColorDX));
  B := (255 - GetBValue(BirdWingColorDX));
  BirdWingColorDown := RGB(R, G, B); {Need more of this
      maybe use the outline list for wing color.... iaw level}

  RepeatUntilBirdlanded;
end;


procedure TBirdDXForm.BoidPerspective(WhichBoid: Integer);
 {void boid_perspective(Boid boid, int W, int H)}
var
  m: Integer;
  f, zfactor, zf,
    tailx, tailz, tail_lx, tail_lz, tail_rx, tail_rz: double;
begin
{   tail:=veccopy(boid[WhichBoid].vel);}
  tail.x := boid[WhichBoid].vel.x;
  tail.y := boid[WhichBoid].vel.y;
  tail.z := boid[WhichBoid].vel.z;
{   tail_end:=veccopy(boid[WhichBoid].vel); }
  tail_end.x := boid[WhichBoid].vel.x;
  tail_end.y := boid[WhichBoid].vel.y;
  tail_end.z := boid[WhichBoid].vel.z;

  if ((boid[WhichBoid].pos.z <= 0))
    then boid[WhichBoid].onscreen := False
  else begin
    zf := BirdsWideDX / (dBirdsScaleDX / 10) { 2};
    zfactor := (boid[WhichBoid].pos.z) / zf;
    if (zfactor = 0) then
      begin zfactor := 0.001; inc(CrashCount); end;

    boid[WhichBoid].X := ((BirdsWideDX shr 1) +
      round(boid[WhichBoid].pos.x / zfactor));
    boid[WhichBoid].Y := ((BirdsHeightDX shr 1) +
      round(boid[WhichBoid].pos.y / zfactor));

    boid[WhichBoid].shadow[0].x := boid[WhichBoid].X;
    boid[WhichBoid].shadow[0].y := ((BirdsHeightDX shr 1)
      + round(BirdsHeightDX / zfactor));

{      vecsetmag(tail_end, 20);}
    m := max(round(abs(tail_end.x)), round(abs(tail_end.y)));
    m := max(m, round(abs(tail_end.z)));
    f := BirdsTailDX / m;
    tail_end.x := tail_end.x * f;
    tail_end.y := tail_end.y * f;
    tail_end.z := tail_end.z * f;
{      vecdiff(boid[WhichBoid].pos, tail_end, tail_end); }
    tail_end.x := boid[WhichBoid].pos.x - tail_end.x;
    tail_end.y := boid[WhichBoid].pos.y - tail_end.y;
    tail_end.z := boid[WhichBoid].pos.z - tail_end.z;

    zfactor := (tail_end.z) / zf;
    if (zfactor = 0) then
      begin zfactor := 0.001; inc(CrashCount); end;
    boid[WhichBoid].tail_X := (BirdsWideDX div 2) +
      round(tail_end.x / zfactor);
    boid[WhichBoid].tail_Y := (BirdsHeightDX div 2) +
      round(tail_end.y / zfactor);
    boid[WhichBoid].shadow[2].x := boid[WhichBoid].tail_X;
    boid[WhichBoid].shadow[2].y := (BirdsHeightDX div 2) +
      round(BirdsHeightDX / zfactor);

{      vecsetmag(tail, 25);}
    m := max(round(abs(tail.x)), round(abs(tail.y)));
    m := max(m, round(abs(tail.z)));
    f := TailSize / m;
    tail.x := tail.x * f;
    tail.y := tail.y * f;
    tail.z := tail.z * f;
{      vecdiff(boid[WhichBoid].pos, tail, tail);}
    tail.x := boid[WhichBoid].pos.x - tail.x;
    tail.y := boid[WhichBoid].pos.y - tail.y;
    tail.z := boid[WhichBoid].pos.z - tail.z;
    tailx := -tail.z / TailWideSize;
    tailz := tail.x / TailWideSize;
    tail_lx := tail.x - tailx;
    tail_lz := tail.z - tailz;
    tail_rx := tail.x + tailx;
    tail_rz := tail.z + tailz;
    tail.y := tail.y - boid[WhichBoid].wingLevel;

    zfactor := (tail_lz) / zf;
    if (zfactor = 0) then
      begin zfactor := 0.001; inc(CrashCount); end;
    boid[WhichBoid].tail_lX := (BirdsWideDX shr 1) + round(tail_lx /
      zfactor);
    boid[WhichBoid].tail_lY := (BirdsHeightDX shr 1) + round(tail.y /
      zfactor);
    boid[WhichBoid].shadow[1].x := boid[WhichBoid].tail_lX;
    boid[WhichBoid].shadow[1].y := (BirdsHeightDX shr 1) +
      round(BirdsHeightDX / zfactor);

    zfactor := (tail_rz) / zf;
    if (zfactor = 0) then
      begin zfactor := 0.001; inc(CrashCount); end;
    boid[WhichBoid].tail_rX := (BirdsWideDX shr 1) + round(tail_rx /
      zfactor);
    boid[WhichBoid].tail_rY := (BirdsHeightDX shr 1) + round(tail.y /
      zfactor);
    boid[WhichBoid].shadow[3].x := boid[WhichBoid].tail_rX;
    boid[WhichBoid].shadow[3].y := (BirdsHeightDX shr 1) +
      round(BirdsHeightDX / zfactor);

    boid[WhichBoid].onscreen := BoidIsOnscreen(WhichBoid);
  end;
end;

function TBirdDXForm.BoidIsOnscreen(WhichBoid: Integer): Boolean;
begin {Z Position is done above in Perspective}
  if (((boid[WhichBoid].X >= 0)
    and (boid[WhichBoid].X < (BirdsWideDX - 1)))
    and ((boid[WhichBoid].Y >= 0)
    and (boid[WhichBoid].Y < (BirdsHeightDX - 1))))
    then BoidIsOnscreen := True else BoidIsOnscreen := False;
end;


{ Move this boid, wrt allboids }
procedure TBirdDXForm.MoveBoid(WhichBoid: Integer);
var
  m: Integer;
  f: double;
begin
  if ((boid[WhichBoid].perching) and (boid[WhichBoid].perchtimer > 0))
    then
    dec(boid[WhichBoid].perchtimer) else
  begin
    boid[WhichBoid].perching := False;

{   CenterBias.x := 0;CenterBias.y := 0;CenterBias.z := 0;}
{   Movecenter := } BoidPerceiveCenter(WhichBoid);
{   vecdiff(Movecenter, boid[WhichBoid].pos, CenterBias);}
{   vecrshift(CenterBias, DEFAULT_CENTER_BIAS);}

{   AvgvelBias.x := 0;AvgvelBias.y := 0;AvgvelBias.z := 0;}
{   MoveAvgvelocity := } BoidAvVel(WhichBoid);
{   vecdiff(MoveAvgvelocity, boid[WhichBoid].vel, AvgvelBias);}
{   vecrshift(AvgvelBias, DEFAULT_AVG_VEL);}

{   chilling := } BoidChillOut(WhichBoid {, allboids, numboids});
{   vecrshift(chilling, DefaultChilling);}

{   vecadd(boid[WhichBoid].vel, CenterBias);}
    boid[WhichBoid].vel.x := boid[WhichBoid].vel.x + CenterBias.x;
    boid[WhichBoid].vel.y := boid[WhichBoid].vel.y + CenterBias.y;
    boid[WhichBoid].vel.z := boid[WhichBoid].vel.z + CenterBias.z;
{   vecadd(boid[WhichBoid].vel, AvgvelBias);}
    boid[WhichBoid].vel.x := boid[WhichBoid].vel.x + AvgvelBias.x;
    boid[WhichBoid].vel.y := boid[WhichBoid].vel.y + AvgvelBias.y;
    boid[WhichBoid].vel.z := boid[WhichBoid].vel.z + AvgvelBias.z;
{   vecadd(boid[WhichBoid].vel, chilling);}
    boid[WhichBoid].vel.x := boid[WhichBoid].vel.x + chilling.x;
    boid[WhichBoid].vel.y := boid[WhichBoid].vel.y + chilling.y;
    boid[WhichBoid].vel.z := boid[WhichBoid].vel.z + chilling.z;

{   veclimit(boid[WhichBoid].vel, 100);}
    m := max(round(abs(boid[WhichBoid].vel.x)),
      round(abs(boid[WhichBoid].vel.y)));
    m := max(m, round(abs(boid[WhichBoid].vel.z)));
    if (m > BirdsSpeedDX) then begin
      f := BirdsSpeedDX / m;
{   vecsmul(vec, f);}
      boid[WhichBoid].vel.x := boid[WhichBoid].vel.x * f;
      boid[WhichBoid].vel.y := boid[WhichBoid].vel.y * f;
      boid[WhichBoid].vel.z := boid[WhichBoid].vel.z * f;
    end;
{   vecadd(boid[WhichBoid].pos, boid[WhichBoid].vel);}
    boid[WhichBoid].pos.x := boid[WhichBoid].pos.x +
      boid[WhichBoid].vel.x;
    boid[WhichBoid].pos.y := boid[WhichBoid].pos.y +
      boid[WhichBoid].vel.y;
    boid[WhichBoid].pos.z := boid[WhichBoid].pos.z +
      boid[WhichBoid].vel.z;


    if (boid[WhichBoid].upstroke) then
    begin
      if (boid[WhichBoid].winglevel >= TailWideSize)
        then boid[WhichBoid].upstroke := False
      else
        inc(boid[WhichBoid].winglevel)
        { := boid[WhichBoid].winglevel+5};
    end
    else if (boid[WhichBoid].winglevel <= -TailWideSize)
      then boid[WhichBoid].upstroke := True
    else
      dec(boid[WhichBoid].wingLevel) {:= boid[WhichBoid].wingLevel-5};

{   /* bound world */}{BirdsWideDX    BirdsHeightDX}
    if (boid[WhichBoid].pos.x < -BirdsWideDX)
      then boid[WhichBoid].vel.x := boid[WhichBoid].vel.x + 10
    else
      if (boid[WhichBoid].pos.x > BirdsWideDX)
        then boid[WhichBoid].vel.x := boid[WhichBoid].vel.x - 10;

    if (boid[WhichBoid].pos.y < -BirdsHeightDX)
      then boid[WhichBoid].vel.y := boid[WhichBoid].vel.y + 10
    else
      if (boid[WhichBoid].pos.y > BirdsHeightGround)
        then boid[WhichBoid].vel.y := boid[WhichBoid].vel.y - 10;

    if (boid[WhichBoid].pos.y > BirdsHeightDX) then
    begin
      boid[WhichBoid].pos.y := BirdsHeightDX; {/* Hit ground!! */}
      boid[WhichBoid].perching := True;
      boid[WhichBoid].perchtimer := (Random(20) + BirdsActivityDX);
      boid[WhichBoid].winglevel := TailWideSize;
      boid[WhichBoid].vel.y := 0;
    end;

    if (boid[WhichBoid].pos.z < BirdsWideDX)
      then
      boid[WhichBoid].vel.z := boid[WhichBoid].vel.z + 10
    else
      if (boid[WhichBoid].pos.z > (BirdsWideDX * 2))
        then
        boid[WhichBoid].vel.z := boid[WhichBoid].vel.z - 10;
    boidperspective(WhichBoid);
  end; {Perching}
end; {Procedure}


procedure TBirdDXForm.BoidPerceiveCenter(WhichBoid: Integer);
{(Boid boid, Vec real_cent, int numboids)}
begin
  CenterBias.x := 0; CenterBias.y := 0; CenterBias.z := 0;
  PercCenter.x := 0; PercCenter.y := 0; PercCenter.z := 0;
{   vecdiff(RealCenter, boid[WhichBoid].pos, PercCenter); }
  PercCenter.x := RealCenter.x - boid[WhichBoid].pos.x;
  PercCenter.y := RealCenter.y - boid[WhichBoid].pos.y;
  PercCenter.z := RealCenter.z - boid[WhichBoid].pos.z;
{   vecsdiv(PercCenter, (HowManyBirds-1));}
  PercCenter.x := PercCenter.x / HowManyBirds - 1;
  PercCenter.y := PercCenter.y / HowManyBirds - 1;
  PercCenter.z := PercCenter.z / HowManyBirds - 1;
{   BoidPerceiveCenter:= PercCenter;}
  Movecenter.x := PercCenter.x;
  Movecenter.y := PercCenter.y;
  Movecenter.z := PercCenter.z;
{   vecdiff(Movecenter, boid[WhichBoid].pos, CenterBias);}
  CenterBias.x := Movecenter.x - boid[WhichBoid].pos.x;
  CenterBias.y := Movecenter.y - boid[WhichBoid].pos.y;
  CenterBias.z := Movecenter.z - boid[WhichBoid].pos.z;
{   vecrshift(CenterBias, DefaultCenterBias  BirdsGlideDX);}
  CenterBias.x := ((CenterBias.x) * dWCentroidDX);
  CenterBias.y := ((CenterBias.y) * dWCentroidDX);
  CenterBias.z := ((CenterBias.z) * dWCentroidDX);
end;

procedure TBirdDXForm.BoidAvVel(WhichBoid: Integer);
{Vec boid_av_vel(Boid boid, Vec real_avgvel, int numboids)}
begin
  PercAvgvel.x := 0; PercAvgvel.y := 0; PercAvgvel.z := 0;
  AvgvelBias.x := 0; AvgvelBias.y := 0; AvgvelBias.z := 0;
{   vecdiff(RealAvgvel, boid[WhichBoid].vel, PercAvgvel);}
  PercAvgvel.x := RealAvgvel.x - boid[WhichBoid].vel.x;
  PercAvgvel.y := RealAvgvel.y - boid[WhichBoid].vel.y;
  PercAvgvel.z := RealAvgvel.z - boid[WhichBoid].vel.z;
{   vecsdiv(PercAvgvel, (HowManyBirds-1));}
  PercAvgvel.x := PercAvgvel.x / HowManyBirds - 1;
  PercAvgvel.y := PercAvgvel.y / HowManyBirds - 1;
  PercAvgvel.z := PercAvgvel.z / HowManyBirds - 1;
{   BoidAvVel:= PercAvgvel;
   MoveAvgvelocity := BoidAvVel(WhichBoid);}
  MoveAvgvelocity.x := PercAvgvel.x;
  MoveAvgvelocity.y := PercAvgvel.y;
  MoveAvgvelocity.z := PercAvgvel.z;
{   vecdiff(MoveAvgvelocity, boid[WhichBoid].vel, AvgvelBias);}
  AvgvelBias.x := MoveAvgvelocity.x - boid[WhichBoid].vel.x;
  AvgvelBias.y := MoveAvgvelocity.y - boid[WhichBoid].vel.y;
  AvgvelBias.z := MoveAvgvelocity.z - boid[WhichBoid].vel.z;
{   vecrshift(AvgvelBias, DefaultAvgVelocity  BirdsEnergyDX);}
  AvgvelBias.x := ((AvgvelBias.x) * dWCopyDX);
  AvgvelBias.y := ((AvgvelBias.y) * dWCopyDX);
  AvgvelBias.z := ((AvgvelBias.z) * dWCopyDX);
end;

procedure TBirdDXForm.BoidChillOut(WhichBoid: Integer);
{Vec boid_chill_out(Boid boid, Boid boids[], int numboids)}
var
  i: Integer;
  dx, dy, dz, dm: double;
begin
  chill.x := 0; chill.y := 0; chill.z := 0;
  bigchill.x := 0; bigchill.y := 0; bigchill.z := 0;
  for i := 0 to HowManyBirds - 1 do begin
    if (i <> WhichBoid) then begin
      dx := boid[WhichBoid].pos.x - boid[i].pos.x;
      dy := boid[WhichBoid].pos.y - boid[i].pos.y;
      dz := boid[WhichBoid].pos.z - boid[i].pos.z;
      if (abs(dx)) > (abs(dy)) then dm := abs(dx) else dm := abs(dy);
      if (abs(dz)) > (dm) then dm := abs(dz);
      if (dm <= RAvoidDX)
{	 if(vecdist(boid[WhichBoid].pos, boid[i].pos) <= ChillFactor)}
      then begin
     {vecdiff(boid[WhichBoid].pos, boid[i].pos, chill);}
        chill.x := boid[WhichBoid].pos.x - boid[i].pos.x;
        chill.y := boid[WhichBoid].pos.y - boid[i].pos.y;
        chill.z := boid[WhichBoid].pos.z - boid[i].pos.z;
{	    vecadd(bigchill, chill); }
        bigchill.x := bigchill.x + chill.x;
        bigchill.y := bigchill.y + chill.y;
        bigchill.z := bigchill.z + chill.z;
      end;
    end;
  end;
  chilling.x := bigchill.x;
  chilling.y := bigchill.y;
  chilling.z := bigchill.z;
{   vecrshift(chilling, DefaultChilling);}
  chilling.x := ((chilling.x) * dWAvoidDX);
  chilling.y := ((chilling.y) * dWAvoidDX);
  chilling.z := ((chilling.z) * dWAvoidDX);
end;

procedure TBirdDXForm.DrawBoid(WhichBoid: Integer);
{Boid boid, Pixmap freshmap)}
var
  outline_index: Integer;
  lwing, rwing: array[0..2] of TPoint;
begin

  DXDraw.{Surface.}Canvas.Pen.Color := clGray;
  DXDraw.{Surface.}Canvas.Brush.Color := clGray;
  DXDraw.{Surface.}Canvas.Polygon([boid[WhichBoid].shadow[0],
    boid[WhichBoid].shadow[1],
      boid[WhichBoid].shadow[2],
      boid[WhichBoid].shadow[3]]);

  lwing[0].x := boid[WhichBoid].X;
  lwing[1].x := boid[WhichBoid].tail_lX;
  lwing[2].x := boid[WhichBoid].tail_X;
  lwing[0].y := boid[WhichBoid].Y;
  lwing[1].y := boid[WhichBoid].tail_lY;
  lwing[2].y := boid[WhichBoid].tail_Y;
  rwing[0].x := boid[WhichBoid].X;
  rwing[1].x := boid[WhichBoid].tail_rX;
  rwing[2].x := boid[WhichBoid].tail_X;
  rwing[0].y := boid[WhichBoid].Y;
  rwing[1].y := boid[WhichBoid].tail_rY;
  rwing[2].y := boid[WhichBoid].tail_Y;
  outline_index := (round(boid[WhichBoid].pos.z * 100) div 31);

   { if moving right => lwing behind rwing }
  if (boid[WhichBoid].vel.x > 0) then begin
    if (boid[WhichBoid].tail_lY < boid[WhichBoid].Y) then
      DXDraw.{Surface.}Canvas.Brush.Color := BirdWingColorDX
    else DXDraw.{Surface.}Canvas.Brush.Color := BirdWingColorDown;
    DXDraw.{Surface.}Canvas.Pen.Color := outlines[outline_index];
    DXDraw.{Surface.}Canvas.Polygon(lwing);
    if (boid[WhichBoid].tail_rY < boid[WhichBoid].Y) then
      DXDraw.{Surface.}Canvas.Brush.Color := BirdWingColorDown
    else DXDraw.{Surface.}Canvas.Brush.Color := BirdWingColorDX;
    DXDraw.{Surface.}Canvas.Pen.Color := outlines[outline_index];
    DXDraw.{Surface.}Canvas.Polygon(rwing);
  end else
  begin
    if (boid[WhichBoid].tail_rY < boid[WhichBoid].Y) then
      DXDraw.{Surface.}Canvas.Brush.Color := BirdWingColorDX
    else DXDraw.{Surface.}Canvas.Brush.Color := BirdWingColorDown;
    DXDraw.{Surface.}Canvas.Pen.Color := outlines[outline_index];
    DXDraw.{Surface.}Canvas.Polygon(lwing);
    if (boid[WhichBoid].tail_lY < boid[WhichBoid].Y) then
      DXDraw.{Surface.}Canvas.Brush.Color := BirdWingColorDown
    else DXDraw.{Surface.}Canvas.Brush.Color := BirdWingColorDX;
    DXDraw.{Surface.}Canvas.Pen.Color := outlines[outline_index];
    DXDraw.{Surface.}Canvas.Polygon(rwing);
  end;
end; {Procedure}

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TBirdDXForm.MazepoleStartup;
var I: Integer;
begin
{     dt:=3.0; }
  Randomize;
  BirdLandDX := False;
  dBirdsViewDX := dBirdsViewDX * dDegToRad;
  dBirdsAvoidDX := dBirdsAvoidDX * dDegToRad;
    // Make space for the positions, velocities, and new velocities. */
  SetLength(BirdsDXP, BirdsDX);
  SetLength(BirdsDYP, BirdsDX);
  SetLength(BirdsDXV, BirdsDX);
  SetLength(BirdsDYV, BirdsDX);
  SetLength(BirdsDXnV, BirdsDX);
  SetLength(BirdsDYnV, BirdsDX);
  // Set to random initial conditions. */
  for I := 0 to BirdsDX - 1 do
  begin
    BirdsDXP[I] := Random(BirdsWideDX - 1);
    BirdsDYP[I] := Random(BirdsHeightDX - 1);
    BirdsDXV[I] := Random(3) - 1;
    BirdsDYV[I] := Random(3) - 1;
    NormDX(BirdsDXV[I], BirdsDYV[I]);
    BirdsDXV[I] := dBirdxTempDX;
    BirdsDYV[I] := dBirdyTempDX;
  end;
  RepeatUntilBirdlanded;
end;

procedure TBirdDXForm.DXDrawMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if DoMazepole then begin
    StartX := X;
    StartY := Y;
  end;
end;

procedure TBirdDXForm.DXDrawMouseUp(Sender: TObject; Button:
  TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var X1: Integer;
begin
  if DoMazepole then begin
    X1 := StartX - X;
    if (X1 < X - StartX) then X1 := X - StartX;
    if (X1 < StartY - Y) then X1 := StartY - Y;
    if (X1 < Y - StartY) then X1 := Y - StartY;
    if (X1 > 15) then begin
{  X1:=(X1 div 2);}
      inc(ObstacleCount);
  {Store the locations into}
      SetLength(ObstacleArray, ObstacleCount);
      ObstacleArray[ObstacleCount - 1].X1 := StartX;
      ObstacleArray[ObstacleCount - 1].Y1 := StartY;
      ObstacleArray[ObstacleCount - 1].X2 := StartX + X1;
      ObstacleArray[ObstacleCount - 1].Y2 := StartY + X1;
      ObstacleArray[ObstacleCount - 1].Z := BirdsHeightDX;
    end else
    begin
{        X:= X+7;StartX:= StartX-7;
        Y:= StartY+7;StartY:= StartY-7; }
        {  DXDraw.Surface.Canvas.Ellipse(StartX, StartY, X, Y);}
      inc(ObstacleCount);
          {Store the locations into}
      SetLength(ObstacleArray, ObstacleCount);
      ObstacleArray[ObstacleCount - 1].X1 := X - 7;
      ObstacleArray[ObstacleCount - 1].Y1 := Y - 7;
      ObstacleArray[ObstacleCount - 1].X2 := X + 7;
      ObstacleArray[ObstacleCount - 1].Y2 := Y + 7;
      ObstacleArray[ObstacleCount - 1].Z := BirdsHeightDX;
    end;

  end;
end;

procedure TBirdDXForm.DrawObstacles;
var
  i: Integer;
begin
  for i := 0 to (ObstacleCount - 1) do
  begin
    DXDraw.{Surface.}Canvas.Pen.Color := clRed;
    DXDraw.{Surface.}Canvas.Brush.Color := clYellow;
    DXDraw.{Surface.}Canvas.Ellipse(
      ObstacleArray[i].X1,
      ObstacleArray[i].Y1,
      ObstacleArray[i].X2,
      ObstacleArray[i].Y2);
  end;
end;

procedure TBirdDXForm.DXCheckCollision;
var
  WhichBird, i: Integer;
  wx2, wy2, wx1, wy1, x1, y1, x2, y2, x3, y3: double;
begin
  for WhichBird := 0 to (BirdsDX - 1) do
  begin
  //* Plot a line in the direction that it is heading. */
    x3 := BirdsDXV[WhichBird];
    y3 := BirdsDYV[WhichBird];
    NormDX(x3, y3); x3 := dBirdxTempDX; y3 := dBirdyTempDX;
    x1 := BirdsDxp[WhichBird];
    y1 := BirdsDyp[WhichBird];
    x2 := x1 + (x3 * BirdsTailDX * 3);
    y2 := y1 + (y3 * BirdsTailDX * 3);
    if x1 > x2 then
      begin wx1 := x1; wx2 := x2; x1 := wx2; x2 := wx1; end;
    if y1 > y2 then
      begin wy1 := y1; wy2 := y2; y1 := wy2; y2 := wy1; end;

    for i := 0 to ObstacleCount - 1 do
    begin
   {  X2,Y2 is bigger than X1,Y1
    If Obstacle X,y  is within Bird X,Y then Do something}
      if (((ObstacleArray[i].X1 > x1) and (ObstacleArray[i].X1 < x2))
        and
        (((ObstacleArray[i].Y1 > y1) and (ObstacleArray[i].Y1 < y2))
          or
        ((ObstacleArray[i].Y2 > y1) and (ObstacleArray[i].Y2 < y2))))
        then
      begin
        BirdsDxp[WhichBird] := (BirdsDxp[WhichBird] -
          (random((ObstacleArray[i].X2 - ObstacleArray[i].X1)) -
          ((ObstacleArray[i].X2 - ObstacleArray[i].X1) / 3)));
        BirdsDyp[WhichBird] := (BirdsDyp[WhichBird] -
          (random((ObstacleArray[i].X2 - ObstacleArray[i].X1)) -
          ((ObstacleArray[i].Y2 - ObstacleArray[i].Y1) / 3)));
      end
      else
        if (((ObstacleArray[i].X2 > x1) and (ObstacleArray[i].X2 <
          x2)) and
          (((ObstacleArray[i].Y1 > y1) and (ObstacleArray[i].Y1 < y2))
            or
          ((ObstacleArray[i].Y2 > y1) and (ObstacleArray[i].Y2 <
            y2))))
          then
        begin
          BirdsDxp[WhichBird] := (BirdsDxp[WhichBird] +
            (random((ObstacleArray[i].X2 - ObstacleArray[i].X1)) -
            ((ObstacleArray[i].X2 - ObstacleArray[i].X1) / 3)));
          BirdsDyp[WhichBird] := (BirdsDyp[WhichBird] +
            (random((ObstacleArray[i].Y2 - ObstacleArray[i].Y1)) -
            ((ObstacleArray[i].Y2 - ObstacleArray[i].Y1) / 3)));
        end;

{   If ((ObstacleArray[i].Y1 > y1) and (ObstacleArray[i].Y1 < y2)) then
     BirdsDyp[WhichBird]:= (BirdsDyp[WhichBird]-(ObstacleArray[i].Y2-ObstacleArray[i].Y1))
     else
   If ((ObstacleArray[i].Y2 > y1) and (ObstacleArray[i].Y2 < y2)) then
     BirdsDyp[WhichBird]:= (BirdsDyp[WhichBird]+(ObstacleArray[i].Y2-ObstacleArray[i].Y1));}
    end;
  end;
end;


procedure TBirdDXForm.MakeOffsetSquares;
var
  RowOffset, SquareSize, Width: Integer;
  Range: Single;
  x1, y1, x2, y2, x3, y3: Integer;
  i1, i2, i3, i4: Single;
  p1, p2, p3, p4: Single;
begin
  { Seed the random number generator. }
{  RandSeed := PFractalParams(ConfigData)^.Seed;}
  { Start with a level playing field. }
  Setlength(TerrainBuffer, 0);
  { Generate the terrain data using the offset squares method. }
  Range := 123 {HeightRange};
  RowOffset := 0;
  Width := 123 {Buffer.XSize}; {Desired Width}
  SquareSize := Width;
  Setlength(TerrainBuffer, SquareSize, SquareSize);
  while SquareSize > 1 do
  begin
    x1 := RowOffset;
    while x1 < Width do
    begin
      y1 := RowOffset;
      while y1 < Width do
      begin
        { Get the four corner points. }
        x2 := (x1 + SquareSize) mod Width;
        y2 := (y1 + SquareSize) mod Width;
        i1 := TerrainBuffer[x1, y1];
        i2 := TerrainBuffer[x2, y1];
        i3 := TerrainBuffer[x1, y2];
        i4 := TerrainBuffer[x2, y2];
        { Obtain new points by averaging the corner points. }
        p1 := (i1 * 9 + i2 * 3 + i3 * 3 + I4) / 16.0;
        p2 := (i1 * 3 + i2 * 9 + i3 + i4 * 3) / 16.0;
        p3 := (i1 * 3 + i2 + i3 * 9 + i4 * 3) / 16.0;
        p4 := (i1 + i2 * 3 + i3 * 3 + i4 * 9) / 16.0;
        { Add a random offset to each point. }
        p1 := p1 + Random * Range - Range / 2.0;
        p2 := p2 + Random * Range - Range / 2.0;
        p3 := p3 + Random * Range - Range / 2.0;
        p4 := p4 + Random * Range - Range / 2.0;
        { Write out the generate points. }
        x3 := (x1 + SquareSize div 4) mod Width;
        y3 := (y1 + SquareSize div 4) mod Width;
        x2 := (x3 + SquareSize div 2) mod Width;
        y2 := (y3 + SquareSize div 2) mod Width;
        TerrainBuffer[x3, y3] := p1;
        TerrainBuffer[x2, y3] := p2;
        TerrainBuffer[x3, y2] := p3;
        TerrainBuffer[x2, y2] := p4;
        Inc(y1, SquareSize);
      end;
      Inc(x1, SquareSize);
    end;
    RowOffset := SquareSize div 4;
    Range := Range / 2.0;
    SquareSize := SquareSize div 2;
  end;
end;


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TBirdDXForm.FrilightStartup;
var
  k, i: Integer;
  R, G, B: Byte;
begin {}
  Randomize;
  BirdLandDX := False;
  CrashCount := 0;
  BirdsHeightGround := ((BirdsHeightDX div 5) * 4);
  for k := 0 to FrenzyCount - 1 do begin
    CurrentFrenzy := K;
    BirdBrainArray[k].TailWideSize := (BirdBrainArray[k].BirdsTailDX
      + (BirdBrainArray[k].BirdsTailDX div 2));
    BirdBrainArray[k].TailSize := (BirdBrainArray[k].BirdsTailDX
      + ((BirdBrainArray[k].BirdsTailDX
      - BirdBrainArray[k].TailWideSize) div 2));
    for i := 0 to BirdBrainArray[k].BirdsDX - 1 do
    begin
   {X and Y can be negatives around screen center}
      boidArray[k, i].pos.x := Random((BirdsWideDX * 2) -
        BirdsWideDX);
      boidArray[k, i].pos.y := Random((BirdsHeightDX * 2) -
        BirdsHeightDX);
   {Z needs to be way positive to see in perspective}
      boidArray[k, i].pos.z := Random((BirdsWideDX * 2)) +
        (BirdsHeightDX);
      boidArray[k, i].vel.x := Random(51) - 25;
        {Negative means flying left...}
      boidArray[k, i].vel.y := Random(51) - 25; {up.. down}
      boidArray[k, i].vel.z := Random(51) - 25; {front to back}
      boidArray[k, i].WingLevel :=
        Random(BirdBrainArray[k].TailWideSize) - 20;
      BoidFPerspective(i);
    end;
    BirdBrainArray[k].RealCenter.x := 0;
    BirdBrainArray[k].RealCenter.y := 0;
    BirdBrainArray[k].RealCenter.z := 0;
    BirdBrainArray[k].RealAvgvel.x := 0;
    BirdBrainArray[k].RealAvgvel.y := 0;
    BirdBrainArray[k].RealAvgvel.z := 0;
  end;

  for i := 0 to 32 do begin
    R := (255 - (i * 7)); G := (255 - (i * 7)); B := (255 - (i * 7));
    outlines[i] := RGB(R, G, B);
  end;
  RepeatUntilBirdlanded;
end;


procedure TBirdDXForm.BoidFPerspective(WhichBoid: Integer);
 {void boid_perspective(Boid boid, int W, int H)}
var
  m: Integer;
  f, zfactor, zf,
    tailx, tailz, tail_lx, tail_lz, tail_rx, tail_rz: double;
begin
{   tail:=veccopy(boid[WhichBoid].vel);}
  tail.x := boidArray[CurrentFrenzy, WhichBoid].vel.x;
  tail.y := boidArray[CurrentFrenzy, WhichBoid].vel.y;
  tail.z := boidArray[CurrentFrenzy, WhichBoid].vel.z;
{   tail_end:=veccopy(boid[WhichBoid].vel); }
  tail_end.x := boidArray[CurrentFrenzy, WhichBoid].vel.x;
  tail_end.y := boidArray[CurrentFrenzy, WhichBoid].vel.y;
  tail_end.z := boidArray[CurrentFrenzy, WhichBoid].vel.z;

  if ((boidArray[CurrentFrenzy, WhichBoid].pos.z <= 0))
    then boidArray[CurrentFrenzy, WhichBoid].onscreen := False
  else begin {Change this???}
    zf := BirdsWideDX / ({BirdBrainArray[CurrentFrenzy].}
      dBirdsScaleDX / 10) { 2};
    zfactor := (boidArray[CurrentFrenzy, WhichBoid].pos.z) / zf;
    if (zfactor = 0) then
      begin zfactor := 0.001; inc(CrashCount); end;

    boidArray[CurrentFrenzy, WhichBoid].X :=
      ((BirdsWideDX shr 1) +
      round(boidArray[CurrentFrenzy, WhichBoid].pos.x / zfactor));
    boidArray[CurrentFrenzy, WhichBoid].Y := ((BirdsHeightDX shr 1) +
      round(boidArray[CurrentFrenzy, WhichBoid].pos.y / zfactor));

    boidArray[CurrentFrenzy, WhichBoid].shadow[0].x :=
      boidArray[CurrentFrenzy, WhichBoid].X;
    boidArray[CurrentFrenzy, WhichBoid].shadow[0].y :=
      ((BirdsHeightDX shr 1)
      + round(BirdsHeightDX / zfactor));

{      vecsetmag(tail_end, 20);}
    m := max(round(abs(tail_end.x)), round(abs(tail_end.y)));
    m := max(m, round(abs(tail_end.z)));
    f := BirdBrainArray[CurrentFrenzy].BirdsTailDX / m;
    tail_end.x := tail_end.x * f;
    tail_end.y := tail_end.y * f;
    tail_end.z := tail_end.z * f;
{      vecdiff(boid[WhichBoid].pos, tail_end, tail_end); }
    tail_end.x := boidArray[CurrentFrenzy, WhichBoid].pos.x -
      tail_end.x;
    tail_end.y := boidArray[CurrentFrenzy, WhichBoid].pos.y -
      tail_end.y;
    tail_end.z := boidArray[CurrentFrenzy, WhichBoid].pos.z -
      tail_end.z;

    zfactor := (tail_end.z) / zf;
    if (zfactor = 0) then
      begin zfactor := 0.001; inc(CrashCount); end;
    boidArray[CurrentFrenzy, WhichBoid].tail_X := (BirdsWideDX div 2)
      +
      round(tail_end.x / zfactor);
    boidArray[CurrentFrenzy, WhichBoid].tail_Y := (BirdsHeightDX div
      2) +
      round(tail_end.y / zfactor);
    boidArray[CurrentFrenzy, WhichBoid].shadow[2].x :=
      boidArray[CurrentFrenzy, WhichBoid].tail_X;
    boidArray[CurrentFrenzy, WhichBoid].shadow[2].y :=
      (BirdsHeightDX div 2) + round(BirdsHeightDX / zfactor);

{      vecsetmag(tail, 25);}
    m := max(round(abs(tail.x)), round(abs(tail.y)));
    m := max(m, round(abs(tail.z)));
    f := BirdBrainArray[CurrentFrenzy].TailSize / m;
    tail.x := tail.x * f;
    tail.y := tail.y * f;
    tail.z := tail.z * f;
{      vecdiff(boid[WhichBoid].pos, tail, tail);}
    tail.x := boidArray[CurrentFrenzy, WhichBoid].pos.x - tail.x;
    tail.y := boidArray[CurrentFrenzy, WhichBoid].pos.y - tail.y;
    tail.z := boidArray[CurrentFrenzy, WhichBoid].pos.z - tail.z;
    tailx := -tail.z / BirdBrainArray[CurrentFrenzy].TailWideSize;
    tailz := tail.x / BirdBrainArray[CurrentFrenzy].TailWideSize;
    tail_lx := tail.x - tailx;
    tail_lz := tail.z - tailz;
    tail_rx := tail.x + tailx;
    tail_rz := tail.z + tailz;
    tail.y := tail.y - boidArray[CurrentFrenzy, WhichBoid].wingLevel;

    zfactor := (tail_lz) / zf;
    if (zfactor = 0) then
      begin zfactor := 0.001; inc(CrashCount); end;
    boidArray[CurrentFrenzy, WhichBoid].tail_lX := (BirdsWideDX shr 1)
      + round(tail_lx / zfactor);
    boidArray[CurrentFrenzy, WhichBoid].tail_lY := (BirdsHeightDX shr
      1) + round(tail.y / zfactor);
    boidArray[CurrentFrenzy, WhichBoid].shadow[1].x :=
      boidArray[CurrentFrenzy, WhichBoid].tail_lX;
    boidArray[CurrentFrenzy, WhichBoid].shadow[1].y := (BirdsHeightDX
      shr 1) +
      round(BirdsHeightDX / zfactor);

    zfactor := (tail_rz) / zf;
    if (zfactor = 0) then
      begin zfactor := 0.001; inc(CrashCount); end;
    boidArray[CurrentFrenzy, WhichBoid].tail_rX :=
      (BirdsWideDX shr 1) + round(tail_rx / zfactor);
    boidArray[CurrentFrenzy, WhichBoid].tail_rY :=
      (BirdsHeightDX shr 1) + round(tail.y / zfactor);
    boidArray[CurrentFrenzy, WhichBoid].shadow[3].x :=
      boidArray[CurrentFrenzy, WhichBoid].tail_rX;
    boidArray[CurrentFrenzy, WhichBoid].shadow[3].y :=
      (BirdsHeightDX shr 1) + round(BirdsHeightDX / zfactor);

    boidArray[CurrentFrenzy, WhichBoid].onscreen :=
      BoidFIsOnscreen(WhichBoid);
  end;
end;

function TBirdDXForm.BoidFIsOnscreen(WhichBoid: Integer): Boolean;
begin {Z Position is done above in Perspective}
  if (((boidArray[CurrentFrenzy, WhichBoid].X >= 0)
    and (boidArray[CurrentFrenzy, WhichBoid].X < (BirdsWideDX - 1)))
    and ((boidArray[CurrentFrenzy, WhichBoid].Y >= 0)
    and (boidArray[CurrentFrenzy, WhichBoid].Y < (BirdsHeightDX -
      1))))
    then BoidFIsOnscreen := True else BoidFIsOnscreen := False;
end;


{ Move this boid, wrt allboids }

procedure TBirdDXForm.MoveFBoid(WhichBoid: Integer);
var
  m: Integer;
  f: double;
begin
  if ((boidArray[CurrentFrenzy, WhichBoid].perching) and
    (boidArray[CurrentFrenzy, WhichBoid].perchtimer > 0)) then
    dec(boidArray[CurrentFrenzy, WhichBoid].perchtimer) else
  begin
    boidArray[CurrentFrenzy, WhichBoid].perching := False;

    BoidFPerceiveCenter(WhichBoid);
    BoidFAvVel(WhichBoid);
    BoidFChillOut(WhichBoid);
{   vecadd(boid[WhichBoid].vel, CenterBias);}
    boidArray[CurrentFrenzy, WhichBoid].vel.x :=
      boidArray[CurrentFrenzy, WhichBoid].vel.x + CenterBias.x;
    boidArray[CurrentFrenzy, WhichBoid].vel.y :=
      boidArray[CurrentFrenzy, WhichBoid].vel.y + CenterBias.y;
    boidArray[CurrentFrenzy, WhichBoid].vel.z :=
      boidArray[CurrentFrenzy, WhichBoid].vel.z + CenterBias.z;
{   vecadd(boid[WhichBoid].vel, AvgvelBias);}
    boidArray[CurrentFrenzy, WhichBoid].vel.x :=
      boidArray[CurrentFrenzy, WhichBoid].vel.x + AvgvelBias.x;
    boidArray[CurrentFrenzy, WhichBoid].vel.y :=
      boidArray[CurrentFrenzy, WhichBoid].vel.y + AvgvelBias.y;
    boidArray[CurrentFrenzy, WhichBoid].vel.z :=
      boidArray[CurrentFrenzy, WhichBoid].vel.z + AvgvelBias.z;
{   vecadd(boid[WhichBoid].vel, chilling);}
    boidArray[CurrentFrenzy, WhichBoid].vel.x :=
      boidArray[CurrentFrenzy, WhichBoid].vel.x + chilling.x;
    boidArray[CurrentFrenzy, WhichBoid].vel.y :=
      boidArray[CurrentFrenzy, WhichBoid].vel.y + chilling.y;
    boidArray[CurrentFrenzy, WhichBoid].vel.z :=
      boidArray[CurrentFrenzy, WhichBoid].vel.z + chilling.z;

{   veclimit(boid[WhichBoid].vel, 100);}
    m := max(round(abs(boidArray[CurrentFrenzy, WhichBoid].vel.x)),
      round(abs(boidArray[CurrentFrenzy, WhichBoid].vel.y)));
    m := max(m, round(abs(boidArray[CurrentFrenzy,
      WhichBoid].vel.z)));
    if (m > BirdBrainArray[CurrentFrenzy].BirdsSpeedDX) then begin
      f := BirdBrainArray[CurrentFrenzy].BirdsSpeedDX / m;
{   vecsmul(vec, f);}
      boidArray[CurrentFrenzy, WhichBoid].vel.x :=
        boidArray[CurrentFrenzy, WhichBoid].vel.x * f;
      boidArray[CurrentFrenzy, WhichBoid].vel.y :=
        boidArray[CurrentFrenzy, WhichBoid].vel.y * f;
      boidArray[CurrentFrenzy, WhichBoid].vel.z :=
        boidArray[CurrentFrenzy, WhichBoid].vel.z * f;
    end;
{   vecadd(boid[WhichBoid].pos, boid[WhichBoid].vel);}
    boidArray[CurrentFrenzy, WhichBoid].pos.x :=
      boidArray[CurrentFrenzy, WhichBoid].pos.x +
      boidArray[CurrentFrenzy, WhichBoid].vel.x;
    boidArray[CurrentFrenzy, WhichBoid].pos.y :=
      boidArray[CurrentFrenzy, WhichBoid].pos.y +
      boidArray[CurrentFrenzy, WhichBoid].vel.y;
    boidArray[CurrentFrenzy, WhichBoid].pos.z :=
      boidArray[CurrentFrenzy, WhichBoid].pos.z +
      boidArray[CurrentFrenzy, WhichBoid].vel.z;


    if (boidArray[CurrentFrenzy, WhichBoid].upstroke) then
    begin
      if (boidArray[CurrentFrenzy, WhichBoid].winglevel >=
        BirdBrainArray[CurrentFrenzy].TailWideSize)
        then boidArray[CurrentFrenzy, WhichBoid].upstroke := False
      else
        inc(boidArray[CurrentFrenzy, WhichBoid].winglevel)
        { := boid[WhichBoid].winglevel+5};
    end
    else if (boidArray[CurrentFrenzy, WhichBoid].winglevel <=
      -BirdBrainArray[CurrentFrenzy].TailWideSize)
      then boidArray[CurrentFrenzy, WhichBoid].upstroke := True
    else
      dec(boidArray[CurrentFrenzy, WhichBoid].wingLevel)
      {:= boid[WhichBoid].wingLevel-5};

{   /* bound world */}{BirdsWideDX    BirdsHeightDX}
    if (boidArray[CurrentFrenzy, WhichBoid].pos.x < -BirdsWideDX)
      then
        boidArray[CurrentFrenzy, WhichBoid].vel.x :=
        boidArray[CurrentFrenzy, WhichBoid].vel.x + 10
    else
      if (boidArray[CurrentFrenzy, WhichBoid].pos.x > BirdsWideDX)
        then
          boidArray[CurrentFrenzy, WhichBoid].vel.x :=
          boidArray[CurrentFrenzy, WhichBoid].vel.x - 10;

    if (boidArray[CurrentFrenzy, WhichBoid].pos.y < -BirdsHeightDX)
      then
        boidArray[CurrentFrenzy, WhichBoid].vel.y :=
        boidArray[CurrentFrenzy, WhichBoid].vel.y + 10
    else
      if (boidArray[CurrentFrenzy, WhichBoid].pos.y >
        BirdsHeightGround)
        then
          boidArray[CurrentFrenzy, WhichBoid].vel.y :=
          boidArray[CurrentFrenzy, WhichBoid].vel.y - 10;

    if (boidArray[CurrentFrenzy, WhichBoid].pos.y > BirdsHeightDX)
      then
    begin
      boidArray[CurrentFrenzy, WhichBoid].pos.y := BirdsHeightDX;
        {/* Hit ground!! */}
      boidArray[CurrentFrenzy, WhichBoid].perching := True;
      boidArray[CurrentFrenzy, WhichBoid].perchtimer :=
        (Random(20) + BirdBrainArray[CurrentFrenzy].BirdsActivityDX);
      boidArray[CurrentFrenzy, WhichBoid].winglevel :=
        BirdBrainArray[CurrentFrenzy].TailWideSize;
      boidArray[CurrentFrenzy, WhichBoid].vel.y := 0;
    end;

    if (boidArray[CurrentFrenzy, WhichBoid].pos.z < BirdsWideDX)
      then
      boidArray[CurrentFrenzy, WhichBoid].vel.z :=
        boidArray[CurrentFrenzy, WhichBoid].vel.z + 10
    else
      if (boidArray[CurrentFrenzy, WhichBoid].pos.z > (BirdsWideDX *
        2))
        then
        boidArray[CurrentFrenzy, WhichBoid].vel.z :=
          boidArray[CurrentFrenzy, WhichBoid].vel.z - 10;
    boidFperspective(WhichBoid);
  end; {Perching}
end; {Procedure}


procedure TBirdDXForm.BoidFPerceiveCenter(WhichBoid: Integer);
{(Boid boid, Vec real_cent, int numboids)}
begin
  CenterBias.x := 0; CenterBias.y := 0; CenterBias.z := 0;
  PercCenter.x := 0; PercCenter.y := 0; PercCenter.z := 0;
{   vecdiff(RealCenter, boid[WhichBoid].pos, PercCenter); }
  PercCenter.x := BirdBrainArray[CurrentFrenzy].RealCenter.x -
    boidArray[CurrentFrenzy, WhichBoid].pos.x;
  PercCenter.y := BirdBrainArray[CurrentFrenzy].RealCenter.y -
    boidArray[CurrentFrenzy, WhichBoid].pos.y;
  PercCenter.z := BirdBrainArray[CurrentFrenzy].RealCenter.z -
    boidArray[CurrentFrenzy, WhichBoid].pos.z;
{   vecsdiv(PercCenter, (HowManyBirds-1));}
  PercCenter.x := PercCenter.x /
    (BirdBrainArray[CurrentFrenzy].BirdsDX - 1); {HowManyBirds-1;}
  PercCenter.y := PercCenter.y /
    (BirdBrainArray[CurrentFrenzy].BirdsDX - 1);
  PercCenter.z := PercCenter.z /
    (BirdBrainArray[CurrentFrenzy].BirdsDX - 1);
{   BoidPerceiveCenter:= PercCenter;}
  Movecenter.x := PercCenter.x;
  Movecenter.y := PercCenter.y;
  Movecenter.z := PercCenter.z;
{   vecdiff(Movecenter, boid[WhichBoid].pos, CenterBias);}
  CenterBias.x := Movecenter.x - boidArray[CurrentFrenzy,
    WhichBoid].pos.x;
  CenterBias.y := Movecenter.y - boidArray[CurrentFrenzy,
    WhichBoid].pos.y;
  CenterBias.z := Movecenter.z - boidArray[CurrentFrenzy,
    WhichBoid].pos.z;
{   vecrshift(CenterBias, DefaultCenterBias  BirdsGlideDX);}
  CenterBias.x := ((CenterBias.x) *
    BirdBrainArray[CurrentFrenzy].dWCentroidDX);
  CenterBias.y := ((CenterBias.y) *
    BirdBrainArray[CurrentFrenzy].dWCentroidDX);
  CenterBias.z := ((CenterBias.z) *
    BirdBrainArray[CurrentFrenzy].dWCentroidDX);
end;

procedure TBirdDXForm.BoidFAvVel(WhichBoid: Integer);
{Vec boid_av_vel(Boid boid, Vec real_avgvel, int numboids)}
begin
  PercAvgvel.x := 0; PercAvgvel.y := 0; PercAvgvel.z := 0;
  AvgvelBias.x := 0; AvgvelBias.y := 0; AvgvelBias.z := 0;
{   vecdiff(RealAvgvel, boid[WhichBoid].vel, PercAvgvel);}
  PercAvgvel.x := BirdBrainArray[CurrentFrenzy].RealAvgvel.x
    - boidArray[CurrentFrenzy, WhichBoid].vel.x;
  PercAvgvel.y := BirdBrainArray[CurrentFrenzy].RealAvgvel.y
    - boidArray[CurrentFrenzy, WhichBoid].vel.y;
  PercAvgvel.z := BirdBrainArray[CurrentFrenzy].RealAvgvel.z
    - boidArray[CurrentFrenzy, WhichBoid].vel.z;
{   vecsdiv(PercAvgvel, (HowManyBirds-1));}
  PercAvgvel.x := PercAvgvel.x /
    (BirdBrainArray[CurrentFrenzy].BirdsDX - 1);
  PercAvgvel.y := PercAvgvel.y /
    (BirdBrainArray[CurrentFrenzy].BirdsDX - 1);
  PercAvgvel.z := PercAvgvel.z /
    (BirdBrainArray[CurrentFrenzy].BirdsDX - 1);
{   BoidAvVel:= PercAvgvel;
   MoveAvgvelocity := BoidAvVel(WhichBoid);}
  MoveAvgvelocity.x := PercAvgvel.x;
  MoveAvgvelocity.y := PercAvgvel.y;
  MoveAvgvelocity.z := PercAvgvel.z;
{   vecdiff(MoveAvgvelocity, boid[WhichBoid].vel, AvgvelBias);}
  AvgvelBias.x := MoveAvgvelocity.x - boidArray[CurrentFrenzy,
    WhichBoid].vel.x;
  AvgvelBias.y := MoveAvgvelocity.y - boidArray[CurrentFrenzy,
    WhichBoid].vel.y;
  AvgvelBias.z := MoveAvgvelocity.z - boidArray[CurrentFrenzy,
    WhichBoid].vel.z;
{   vecrshift(AvgvelBias, DefaultAvgVelocity  BirdsEnergyDX);}
  AvgvelBias.x := ((AvgvelBias.x) *
    BirdBrainArray[CurrentFrenzy].dWCopyDX);
  AvgvelBias.y := ((AvgvelBias.y) *
    BirdBrainArray[CurrentFrenzy].dWCopyDX);
  AvgvelBias.z := ((AvgvelBias.z) *
    BirdBrainArray[CurrentFrenzy].dWCopyDX);
end;

procedure TBirdDXForm.BoidFChillOut(WhichBoid: Integer);
{Vec boid_chill_out(Boid boid, Boid boids[], int numboids)}
var
  i: Integer;
  dx, dy, dz, dm: double;
begin
  chill.x := 0; chill.y := 0; chill.z := 0;
  bigchill.x := 0; bigchill.y := 0; bigchill.z := 0;
  for i := 0 to (BirdBrainArray[CurrentFrenzy].BirdsDX - 1) do begin
    if (i <> WhichBoid) then begin
      dx := boidArray[CurrentFrenzy, WhichBoid].pos.x -
        boidArray[CurrentFrenzy, i].pos.x;
      dy := boidArray[CurrentFrenzy, WhichBoid].pos.y -
        boidArray[CurrentFrenzy, i].pos.y;
      dz := boidArray[CurrentFrenzy, WhichBoid].pos.z -
        boidArray[CurrentFrenzy, i].pos.z;
      if (abs(dx)) > (abs(dy)) then dm := abs(dx) else dm := abs(dy);
      if (abs(dz)) > (dm) then dm := abs(dz);
      if (dm <= BirdBrainArray[CurrentFrenzy].RAvoidDX)
{	 if(vecdist(boid[WhichBoid].pos, boid[i].pos) <= ChillFactor)}
      then begin
     {vecdiff(boid[WhichBoid].pos, boid[i].pos, chill);}
        chill.x := boidArray[CurrentFrenzy, WhichBoid].pos.x -
          boidArray[CurrentFrenzy, i].pos.x;
        chill.y := boidArray[CurrentFrenzy, WhichBoid].pos.y -
          boidArray[CurrentFrenzy, i].pos.y;
        chill.z := boidArray[CurrentFrenzy, WhichBoid].pos.z -
          boidArray[CurrentFrenzy, i].pos.z;
{	    vecadd(bigchill, chill); }
        bigchill.x := bigchill.x + chill.x;
        bigchill.y := bigchill.y + chill.y;
        bigchill.z := bigchill.z + chill.z;
      end;
    end;
  end;
  chilling.x := bigchill.x;
  chilling.y := bigchill.y;
  chilling.z := bigchill.z;
 {   vecrshift(chilling, DefaultChilling);}{CHANGE THIS????}
  chilling.x := ((chilling.x) *
    BirdBrainArray[CurrentFrenzy].dWAvoidDX);
  chilling.y := ((chilling.y) *
    BirdBrainArray[CurrentFrenzy].dWAvoidDX);
  chilling.z := ((chilling.z) *
    BirdBrainArray[CurrentFrenzy].dWAvoidDX);
end;

procedure TBirdDXForm.DrawFBoid(WhichBoid: Integer);
{Boid boid, Pixmap freshmap)}
var
  outline_index: Integer;
  lwing, rwing: array[0..2] of TPoint;
begin

  DXDraw.{Surface.}Canvas.Pen.Color := clGray;
  DXDraw.{Surface.}Canvas.Brush.Color := clGray;
  DXDraw.{Surface.}Canvas.Polygon([boidArray[CurrentFrenzy,
    WhichBoid].shadow[0],
    boidArray[CurrentFrenzy, WhichBoid].shadow[1],
      boidArray[CurrentFrenzy, WhichBoid].shadow[2],
      boidArray[CurrentFrenzy, WhichBoid].shadow[3]]);

  lwing[0].x := boidArray[CurrentFrenzy, WhichBoid].X;
  lwing[1].x := boidArray[CurrentFrenzy, WhichBoid].tail_lX;
  lwing[2].x := boidArray[CurrentFrenzy, WhichBoid].tail_X;
  lwing[0].y := boidArray[CurrentFrenzy, WhichBoid].Y;
  lwing[1].y := boidArray[CurrentFrenzy, WhichBoid].tail_lY;
  lwing[2].y := boidArray[CurrentFrenzy, WhichBoid].tail_Y;
  rwing[0].x := boidArray[CurrentFrenzy, WhichBoid].X;
  rwing[1].x := boidArray[CurrentFrenzy, WhichBoid].tail_rX;
  rwing[2].x := boidArray[CurrentFrenzy, WhichBoid].tail_X;
  rwing[0].y := boidArray[CurrentFrenzy, WhichBoid].Y;
  rwing[1].y := boidArray[CurrentFrenzy, WhichBoid].tail_rY;
  rwing[2].y := boidArray[CurrentFrenzy, WhichBoid].tail_Y;
  outline_index := (round(boidArray[CurrentFrenzy, WhichBoid].pos.z *
    100) div 31);

   { if moving right => lwing behind rwing }
  if (boidArray[CurrentFrenzy, WhichBoid].vel.x > 0) then begin
    if (boidArray[CurrentFrenzy, WhichBoid].tail_lY <
      boidArray[CurrentFrenzy, WhichBoid].Y) then
      DXDraw.{Surface.}Canvas.Brush.Color :=
        BirdBrainArray[CurrentFrenzy].BirdWingColorDX
    else
      DXDraw.{Surface.}Canvas.Brush.Color :=
      BirdBrainArray[CurrentFrenzy].BirdsColorDX; { BirdWingColorDown;}
    DXDraw.{Surface.}Canvas.Pen.Color := outlines[outline_index];
    DXDraw.{Surface.}Canvas.Polygon(lwing);
    if (boidArray[CurrentFrenzy, WhichBoid].tail_rY <
      boidArray[CurrentFrenzy, WhichBoid].Y) then
      DXDraw.{Surface.}Canvas.Brush.Color :=
        BirdBrainArray[CurrentFrenzy].BirdsColorDX { BirdWingColorDown;}
    else
      DXDraw.{Surface.}Canvas.Brush.Color :=
      BirdBrainArray[CurrentFrenzy].BirdWingColorDX;
    DXDraw.{Surface.}Canvas.Pen.Color := outlines[outline_index];
    DXDraw.{Surface.}Canvas.Polygon(rwing);
  end else
  begin
    if (boidArray[CurrentFrenzy, WhichBoid].tail_rY <
      boidArray[CurrentFrenzy, WhichBoid].Y) then
      DXDraw.{Surface.}Canvas.Brush.Color :=
        BirdBrainArray[CurrentFrenzy].BirdWingColorDX
    else
      DXDraw.{Surface.}Canvas.Brush.Color :=
      BirdBrainArray[CurrentFrenzy].BirdsColorDX; { BirdWingColorDown;}
    DXDraw.{Surface.}Canvas.Pen.Color := outlines[outline_index];
    DXDraw.{Surface.}Canvas.Polygon(lwing);
    if (boidArray[CurrentFrenzy, WhichBoid].tail_lY <
      boidArray[CurrentFrenzy, WhichBoid].Y) then
      DXDraw.{Surface.}Canvas.Brush.Color :=
        BirdBrainArray[CurrentFrenzy].BirdsColorDX { BirdWingColorDown;}
    else
      DXDraw.{Surface.}Canvas.Brush.Color :=
      BirdBrainArray[CurrentFrenzy].BirdWingColorDX;
    DXDraw.{Surface.}Canvas.Pen.Color := outlines[outline_index];
    DXDraw.{Surface.}Canvas.Polygon(rwing);
  end;
end; {Procedure}

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TBirdDXForm.FeedrenzyStartup;
var
  k, i: Integer;
  R, G, B: Byte;
begin {}
  Randomize;
  BirdLandDX := False;
  CrashCount := 0;
  BirdsHeightGround := ((BirdsHeightDX div 5) * 4);
  for k := 0 to FrenzyCount - 1 do begin
    CurrentFrenzy := K;
    BirdBrainArray[k].TailWideSize := (BirdBrainArray[k].BirdsTailDX
      + (BirdBrainArray[k].BirdsTailDX div 2));
    BirdBrainArray[k].TailSize := (BirdBrainArray[k].BirdsTailDX
      + ((BirdBrainArray[k].BirdsTailDX
      - BirdBrainArray[k].TailWideSize) div 2));
    for i := 0 to BirdBrainArray[k].BirdsDX - 1 do
    begin
   {X and Y can be negatives around screen center}
      boidArray[k, i].pos.x := Random((BirdsWideDX * 2) -
        BirdsWideDX);
      boidArray[k, i].pos.y := Random((BirdsHeightDX * 2) -
        BirdsHeightDX);
   {Z needs to be way positive to see in perspective}
      boidArray[k, i].pos.z := Random((BirdsWideDX * 2)) +
        (BirdsHeightDX);
      boidArray[k, i].vel.x := Random(51) - 25;
        {Negative means flying left...}
      boidArray[k, i].vel.y := Random(51) - 25; {up.. down}
      boidArray[k, i].vel.z := Random(51) - 25; {front to back}
      boidArray[k, i].WingLevel :=
        Random(BirdBrainArray[k].TailWideSize) - 20;
      BoidFPerspective(i);
    end;
    BirdBrainArray[k].RealCenter.x := 0;
    BirdBrainArray[k].RealCenter.y := 0;
    BirdBrainArray[k].RealCenter.z := 0;
    BirdBrainArray[k].RealAvgvel.x := 0;
    BirdBrainArray[k].RealAvgvel.y := 0;
    BirdBrainArray[k].RealAvgvel.z := 0;
  end;

  for i := 0 to 32 do begin
    R := (255 - (i * 7)); G := (255 - (i * 7)); B := (255 - (i * 7));
    outlines[i] := RGB(R, G, B);
  end;
  RepeatUntilBirdlanded;
end;

procedure TBirdDXForm.BoidFeed(WhichBoid: Integer);
var
  DesiredVelocity, TargetOffset: Vec;
  RampedSpeed, ClippedSpeed,
    dm, dx, dy, dz: Double;
begin
{Arrival    TargetOffset = target - position}
  TargetOffset.x := Feeder.x - boidArray[CurrentFrenzy,
    WhichBoid].pos.x;
  TargetOffset.y := Feeder.y - boidArray[CurrentFrenzy,
    WhichBoid].pos.y;
  TargetOffset.z := Feeder.z - boidArray[CurrentFrenzy,
    WhichBoid].pos.z;

{    distance = length (target_offset)}
  dx := TargetOffset.x;
  dy := TargetOffset.y;
  dz := TargetOffset.z;
  if (abs(dx)) > (abs(dy)) then dm := abs(dx) else dm := abs(dy);
  if (abs(dz)) > (dm) then dm := abs(dz);
  if ((abs(dx) < 3) and (abs(dy) < 3) and (abs(dz) < 3)) then
  begin
{      boidArray[CurrentFrenzy,WhichBoid].pos.y := BirdsHeightDX; }{/* Hit ground!! */}
    boidArray[CurrentFrenzy, WhichBoid].Hungry := True;
    boidArray[CurrentFrenzy, WhichBoid].Hungrytimer :=
      (Random(200) + BirdBrainArray[CurrentFrenzy].BirdsActivityDX);
    boidArray[CurrentFrenzy, WhichBoid].winglevel :=
      BirdBrainArray[CurrentFrenzy].TailWideSize;
    boidArray[CurrentFrenzy, WhichBoid].vel.y := 0;
  end else begin
{    RampedSpeed = max_speed * (distance / slowing_distance)}
    RampedSpeed := BirdBrainArray[CurrentFrenzy].BirdsSpeedDX
      * (dm / BirdBrainArray[CurrentFrenzy].BirdsTailDX);

    if (RampedSpeed > BirdBrainArray[CurrentFrenzy].BirdsSpeedDX)
      then
      ClippedSpeed := BirdBrainArray[CurrentFrenzy].BirdsSpeedDX
    else ClippedSpeed := RampedSpeed;
{    DesiredVelocity = (clipped_speed / distance) * TargetOffset}
    DesiredVelocity.x := (ClippedSpeed / dm) * TargetOffset.x;
    DesiredVelocity.y := (ClippedSpeed / dm) * TargetOffset.y;
    DesiredVelocity.z := (ClippedSpeed / dm) * TargetOffset.z;

{    steering = desired_velocity - velocity}
    Feeding.x := DesiredVelocity.x - boidArray[CurrentFrenzy,
      WhichBoid].vel.x;
    Feeding.y := DesiredVelocity.y - boidArray[CurrentFrenzy,
      WhichBoid].vel.y;
    Feeding.z := DesiredVelocity.z - boidArray[CurrentFrenzy,
      WhichBoid].vel.z;

    Feeding.x := ((Feeding.x) * {BirdBrainArray[CurrentFrenzy].}
      dBirdsFeedDX);
    Feeding.y := ((Feeding.y) * {BirdBrainArray[CurrentFrenzy].}
      dBirdsFeedDX);
    Feeding.z := ((Feeding.z) * {BirdBrainArray[CurrentFrenzy].}
      dBirdsFeedDX);
  end;
end;

procedure TBirdDXForm.MoveFFBoid(WhichBoid: Integer);
var
  m: Integer;
  f: double;
begin
  if ((boidArray[CurrentFrenzy, WhichBoid].perching) and
    (boidArray[CurrentFrenzy, WhichBoid].perchtimer > 0)) then
    dec(boidArray[CurrentFrenzy, WhichBoid].perchtimer) else
    if ((boidArray[CurrentFrenzy, WhichBoid].Hungry) and
      (boidArray[CurrentFrenzy, WhichBoid].Hungrytimer > 0)) then
      dec(boidArray[CurrentFrenzy, WhichBoid].Hungrytimer) else
(*   begin
   Hungry:=true;
   boidArray[CurrentFrenzy,WhichBoid].perching := False;
   boidArray[CurrentFrenzy,WhichBoid].vel.x := boidArray[CurrentFrenzy,WhichBoid].vel.y+random;
   boidArray[CurrentFrenzy,WhichBoid].vel.y := boidArray[CurrentFrenzy,WhichBoid].vel.y+random;
   boidArray[CurrentFrenzy,WhichBoid].vel.z := boidArray[CurrentFrenzy,WhichBoid].vel.z+random;
   boidArray[CurrentFrenzy,WhichBoid].pos.x := 0;{boidArray[CurrentFrenzy,WhichBoid].pos.y+random(66)-30;}
   boidArray[CurrentFrenzy,WhichBoid].pos.y := (BirdsHeightDX div 2);{boidArray[CurrentFrenzy,WhichBoid].pos.y+random(66)-30;}
   boidArray[CurrentFrenzy,WhichBoid].pos.z :=(BirdsHeightDX div 2);{boidArray[CurrentFrenzy,WhichBoid].pos.z+random(66)-30;}
   end else*)
    begin
      boidArray[CurrentFrenzy, WhichBoid].perching := False;
      boidArray[CurrentFrenzy, WhichBoid].Hungry := False;
      BoidFeed(WhichBoid);
      BoidFPerceiveCenter(WhichBoid);
      BoidFAvVel(WhichBoid);
      BoidFChillOut(WhichBoid);

{   vecadd(boid[WhichBoid].vel, CenterBias);}
      boidArray[CurrentFrenzy, WhichBoid].vel.x :=
        boidArray[CurrentFrenzy, WhichBoid].vel.x + CenterBias.x;
      boidArray[CurrentFrenzy, WhichBoid].vel.y :=
        boidArray[CurrentFrenzy, WhichBoid].vel.y + CenterBias.y;
      boidArray[CurrentFrenzy, WhichBoid].vel.z :=
        boidArray[CurrentFrenzy, WhichBoid].vel.z + CenterBias.z;
{   vecadd(boid[WhichBoid].vel, AvgvelBias);}
      boidArray[CurrentFrenzy, WhichBoid].vel.x :=
        boidArray[CurrentFrenzy, WhichBoid].vel.x + AvgvelBias.x;
      boidArray[CurrentFrenzy, WhichBoid].vel.y :=
        boidArray[CurrentFrenzy, WhichBoid].vel.y + AvgvelBias.y;
      boidArray[CurrentFrenzy, WhichBoid].vel.z :=
        boidArray[CurrentFrenzy, WhichBoid].vel.z + AvgvelBias.z;
{   vecadd(boid[WhichBoid].vel, chilling);}
      boidArray[CurrentFrenzy, WhichBoid].vel.x :=
        boidArray[CurrentFrenzy, WhichBoid].vel.x + chilling.x;
      boidArray[CurrentFrenzy, WhichBoid].vel.y :=
        boidArray[CurrentFrenzy, WhichBoid].vel.y + chilling.y;
      boidArray[CurrentFrenzy, WhichBoid].vel.z :=
        boidArray[CurrentFrenzy, WhichBoid].vel.z + chilling.z;

      boidArray[CurrentFrenzy, WhichBoid].vel.x :=
        boidArray[CurrentFrenzy, WhichBoid].vel.x + Feeding.x;
      boidArray[CurrentFrenzy, WhichBoid].vel.y :=
        boidArray[CurrentFrenzy, WhichBoid].vel.y + Feeding.y;
      boidArray[CurrentFrenzy, WhichBoid].vel.z :=
        boidArray[CurrentFrenzy, WhichBoid].vel.z + Feeding.z;

{   veclimit(boid[WhichBoid].vel, 100);}
      m := max(round(abs(boidArray[CurrentFrenzy, WhichBoid].vel.x)),
        round(abs(boidArray[CurrentFrenzy, WhichBoid].vel.y)));
      m := max(m, round(abs(boidArray[CurrentFrenzy,
        WhichBoid].vel.z)));
      if (m > BirdBrainArray[CurrentFrenzy].BirdsSpeedDX) then begin
        f := BirdBrainArray[CurrentFrenzy].BirdsSpeedDX / m;
{   vecsmul(vec, f);}
        boidArray[CurrentFrenzy, WhichBoid].vel.x :=
          boidArray[CurrentFrenzy, WhichBoid].vel.x * f;
        boidArray[CurrentFrenzy, WhichBoid].vel.y :=
          boidArray[CurrentFrenzy, WhichBoid].vel.y * f;
        boidArray[CurrentFrenzy, WhichBoid].vel.z :=
          boidArray[CurrentFrenzy, WhichBoid].vel.z * f;
      end;
{   vecadd(boid[WhichBoid].pos, boid[WhichBoid].vel);}
      boidArray[CurrentFrenzy, WhichBoid].pos.x :=
        boidArray[CurrentFrenzy, WhichBoid].pos.x +
        boidArray[CurrentFrenzy, WhichBoid].vel.x;
      boidArray[CurrentFrenzy, WhichBoid].pos.y :=
        boidArray[CurrentFrenzy, WhichBoid].pos.y +
        boidArray[CurrentFrenzy, WhichBoid].vel.y;
      boidArray[CurrentFrenzy, WhichBoid].pos.z :=
        boidArray[CurrentFrenzy, WhichBoid].pos.z +
        boidArray[CurrentFrenzy, WhichBoid].vel.z;


      if (boidArray[CurrentFrenzy, WhichBoid].upstroke) then
      begin
        if (boidArray[CurrentFrenzy, WhichBoid].winglevel >=
          BirdBrainArray[CurrentFrenzy].TailWideSize)
          then boidArray[CurrentFrenzy, WhichBoid].upstroke := False
        else
          inc(boidArray[CurrentFrenzy, WhichBoid].winglevel)
          { := boid[WhichBoid].winglevel+5};
      end
      else if (boidArray[CurrentFrenzy, WhichBoid].winglevel <=
        -BirdBrainArray[CurrentFrenzy].TailWideSize)
        then boidArray[CurrentFrenzy, WhichBoid].upstroke := True
      else
        dec(boidArray[CurrentFrenzy, WhichBoid].wingLevel)
        {:= boid[WhichBoid].wingLevel-5};

{   /* bound world */}{BirdsWideDX    BirdsHeightDX}
      if (boidArray[CurrentFrenzy, WhichBoid].pos.x < -BirdsWideDX)
        then
          boidArray[CurrentFrenzy, WhichBoid].vel.x :=
          boidArray[CurrentFrenzy, WhichBoid].vel.x + 10
      else
        if (boidArray[CurrentFrenzy, WhichBoid].pos.x > BirdsWideDX)
          then
            boidArray[CurrentFrenzy, WhichBoid].vel.x :=
            boidArray[CurrentFrenzy, WhichBoid].vel.x - 10;

      if (boidArray[CurrentFrenzy, WhichBoid].pos.y < -BirdsHeightDX)
        then
          boidArray[CurrentFrenzy, WhichBoid].vel.y :=
          boidArray[CurrentFrenzy, WhichBoid].vel.y + 10
      else
        if (boidArray[CurrentFrenzy, WhichBoid].pos.y >
          BirdsHeightGround)
          then
            boidArray[CurrentFrenzy, WhichBoid].vel.y :=
            boidArray[CurrentFrenzy, WhichBoid].vel.y - 10;

      if (boidArray[CurrentFrenzy, WhichBoid].pos.y > BirdsHeightDX)
        then
      begin
        boidArray[CurrentFrenzy, WhichBoid].pos.y := BirdsHeightDX;
          {/* Hit ground!! */}
        boidArray[CurrentFrenzy, WhichBoid].perching := True;
        boidArray[CurrentFrenzy, WhichBoid].perchtimer :=
          (Random(20) +
            BirdBrainArray[CurrentFrenzy].BirdsActivityDX);
        boidArray[CurrentFrenzy, WhichBoid].winglevel :=
          BirdBrainArray[CurrentFrenzy].TailWideSize;
        boidArray[CurrentFrenzy, WhichBoid].vel.y := 0;
      end;

      if (boidArray[CurrentFrenzy, WhichBoid].pos.z < BirdsWideDX)
        then
        boidArray[CurrentFrenzy, WhichBoid].vel.z :=
          boidArray[CurrentFrenzy, WhichBoid].vel.z + 10
      else
        if (boidArray[CurrentFrenzy, WhichBoid].pos.z > (BirdsWideDX
          * 2))
          then
          boidArray[CurrentFrenzy, WhichBoid].vel.z :=
            boidArray[CurrentFrenzy, WhichBoid].vel.z - 10;
      boidFperspective(WhichBoid);
    end; {Perching}
end; {Procedure}
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TBirdDXForm.PredrenzyStartup;
var
  k, i: Integer;
  R, G, B: Byte;
  F: Textfile;
  DataString: string;  
begin {}
  Randomize;
  BirdLandDX := False;
  CrashCount := 0;
  BirdsHeightGround := ((BirdsHeightDX div 5) * 4);
  ChDir(BirdLifeDir);
  if (FileExists(ExtractFilePath(ParamStr(0))+'hunter.hk')) then
    begin
      AssignFile(F, ExtractFilePath(ParamStr(0))+'hunter.hk');
      Reset(F);
      Readln(F, DataString);
      Hunter.BirdWingColorDX := strtoint(DataString);
      Readln(F, DataString);
      Hunter.BirdsColorDX := strtoint(DataString);
      Readln(F, DataString);
      Hunter.BirdsTailDX := strtoint(DataString);
      Readln(F, DataString);
      BirdsSpeedDX := strtoint(DataString);
      Readln(F, DataString);
      {Hunter.BirdTexture := DataString;}
      CloseFile(F);
  end else
  begin
    Hunter.BirdWingColorDX := clGray; {clMaroon;}
    Hunter.BirdsColorDX := clBlack; {clRed;}
    Hunter.BirdsTailDX := 42;
    BirdsSpeedDX := 123;
  end;
  SpeedLabel.Caption := Format('Speed %d ', [BirdsSpeedDX]);
  BirdsSpeedTB.Position := BirdsSpeedDX;
  Hunter.TailWideSize := (Hunter.BirdsTailDX
    + (Hunter.BirdsTailDX div 2));
  Hunter.TailSize := (Hunter.BirdsTailDX
    + ((Hunter.BirdsTailDX
    - Hunter.TailWideSize) div 2));

  Hunter.pos.x := Random((BirdsWideDX * 2) - BirdsWideDX);
  Hunter.pos.y := Random((BirdsHeightDX * 2) - BirdsHeightDX);
  Hunter.pos.z := Random((BirdsWideDX * 2)) + (BirdsHeightDX);
  Hunter.vel.x := Random(51) - 25; {Negative means flying left...}
  Hunter.vel.y := Random(51) - 25; {up.. down}
  Hunter.vel.z := Random(51) - 25; {front to back}
  Hunter.WingLevel := Random(Hunter.TailWideSize - 5);
  Hunter.upstroke := True;
  PerspectiveHunter;
  for k := 0 to FrenzyCount - 1 do begin
    CurrentFrenzy := K;
    BirdBrainArray[k].TailWideSize := (BirdBrainArray[k].BirdsTailDX
      + (BirdBrainArray[k].BirdsTailDX div 2));
    BirdBrainArray[k].TailSize := (BirdBrainArray[k].BirdsTailDX
      + ((BirdBrainArray[k].BirdsTailDX
      - BirdBrainArray[k].TailWideSize) div 2));
    for i := 0 to BirdBrainArray[k].BirdsDX - 1 do
    begin
   {X and Y can be negatives around screen center}
      boidArray[k, i].pos.x := Random((BirdsWideDX * 2) -
        BirdsWideDX);
      boidArray[k, i].pos.y := Random((BirdsHeightDX * 2) -
        BirdsHeightDX);
   {Z needs to be way positive to see in perspective}
      boidArray[k, i].pos.z := Random((BirdsWideDX * 2)) +
        (BirdsHeightDX);
      boidArray[k, i].vel.x := Random(51) - 25;
        {Negative means flying left...}
      boidArray[k, i].vel.y := Random(51) - 25; {up.. down}
      boidArray[k, i].vel.z := Random(51) - 25; {front to back}
      boidArray[k, i].WingLevel :=
        Random(BirdBrainArray[k].TailWideSize) - 20;
      boidArray[k, i].Seeker := True;
      BoidFPerspective(i);
    end;
    BirdBrainArray[k].RealCenter.x := 0;
    BirdBrainArray[k].RealCenter.y := 0;
    BirdBrainArray[k].RealCenter.z := 0;
    BirdBrainArray[k].RealAvgvel.x := 0;
    BirdBrainArray[k].RealAvgvel.y := 0;
    BirdBrainArray[k].RealAvgvel.z := 0;
  end;

  for i := 0 to 32 do begin
    R := (255 - (i * 7)); G := (255 - (i * 7)); B := (255 - (i * 7));
    outlines[i] := RGB(R, G, B);
  end;
  RepeatUntilBirdlanded;
end;

procedure TBirdDXForm.BoidSeek;
var
  m, K, J, CurrentSeekerK, CurrentSeekerJ: Integer;
  DesiredVelocity, TargetOffset: Vec;
  zf, f, dx, dy, dz: Double;
begin
  {  If ChaseIt then } begin
{Get a bird... goofy way but should work... Set false when GOT}
    CurrentSeekerK := -1;
    CurrentSeekerJ := -1;
    for K := 0 to FrenzyCount - 1 do begin
      for J := 0 to (BirdBrainArray[K].BirdsDX - 1) do begin
{      If (boidArray[K,J].Seeker=false)then
      boidArray[K,J].Perchtimer :=2000;}
        if (boidArray[K, J].Seeker = true) then begin
          CurrentSeekerK := K;
          CurrentSeekerJ := J;
        end; end; end;
    if ((CurrentSeekerK > -1) and (CurrentSeekerJ > -1)) then begin
{Lend:=(sqrt(SQR((x1)-(x2))+ SQR((z1)-(z2)) + SQR((y1)-(y2))));}
{Chase the bird...>>>Seek}
      TargetOffset.x :=
        boidArray[CurrentSeekerK, CurrentSeekerJ].pos.x -
          Hunter.pos.x;
      TargetOffset.y :=
        boidArray[CurrentSeekerK, CurrentSeekerJ].pos.y -
          Hunter.pos.y;
      TargetOffset.z :=
        boidArray[CurrentSeekerK, CurrentSeekerJ].pos.z -
          Hunter.pos.z;
      TargetOffset.x := TargetOffset.x - Hunter.pos.x;
      TargetOffset.y := TargetOffset.y - Hunter.pos.y;
      TargetOffset.z := TargetOffset.z - Hunter.pos.z;
      TargetOffset.x := (BirdsSpeedDX / 200) * TargetOffset.x;
      TargetOffset.y := (BirdsSpeedDX / 200) * TargetOffset.y;
      TargetOffset.z := (BirdsSpeedDX / 200) * TargetOffset.z;
      DesiredVelocity.x := TargetOffset.x + Hunter.vel.x;
      DesiredVelocity.y := TargetOffset.y + Hunter.vel.y;
      DesiredVelocity.z := TargetOffset.z + Hunter.vel.z;
      TargetOffset.x :=
        boidArray[CurrentSeekerK, CurrentSeekerJ].vel.x -
          Hunter.vel.x;
      TargetOffset.y :=
        boidArray[CurrentSeekerK, CurrentSeekerJ].vel.y -
          Hunter.vel.y;
      TargetOffset.z :=
        boidArray[CurrentSeekerK, CurrentSeekerJ].vel.z -
          Hunter.vel.z;
      TargetOffset.x := TargetOffset.x - Hunter.vel.x;
      TargetOffset.y := TargetOffset.y - Hunter.vel.y;
      TargetOffset.z := TargetOffset.z - Hunter.vel.z;
      TargetOffset.x := (BirdsSpeedDX / 200) * TargetOffset.x;
      TargetOffset.y := (BirdsSpeedDX / 200) * TargetOffset.y;
      TargetOffset.z := (BirdsSpeedDX / 200) * TargetOffset.z;
      Hunter.vel.x := TargetOffset.x + Hunter.vel.x;
      Hunter.vel.y := TargetOffset.y + Hunter.vel.y;
      Hunter.vel.z := TargetOffset.z + Hunter.vel.z;
      Hunter.vel.x := DesiredVelocity.x + Hunter.vel.x;
      Hunter.vel.y := DesiredVelocity.y + Hunter.vel.y;
      Hunter.vel.z := DesiredVelocity.z + Hunter.vel.z;
      m := max(round(abs(Hunter.vel.x)),
        round(abs(Hunter.vel.y)));
      m := max(m, round(abs(Hunter.vel.z)));
      if (m > {BirdBrainArray[CurrentSeekerK].} BirdsSpeedDX) then
        begin
        f := {BirdBrainArray[CurrentSeekerK].} BirdsSpeedDX / m;
        Hunter.vel.x := Hunter.vel.x * f;
        Hunter.vel.y := Hunter.vel.y * f;
        Hunter.vel.z := Hunter.vel.z * f;
      end;
    {desired_velocity = normalize (position - target) * max_speed
    steering = desired_velocity - velocity}
(*    TargetOffset.x :=
      Hunter.pos.x-boidArray[CurrentSeekerK,CurrentSeekerJ].pos.x;
    TargetOffset.y :=
      Hunter.pos.y-boidArray[CurrentSeekerK,CurrentSeekerJ].pos.y;
    TargetOffset.z :=
      Hunter.pos.z-boidArray[CurrentSeekerK,CurrentSeekerJ].pos.z;
    Hunter.vel.x:= TargetOffset.x+Hunter.vel.x;
    Hunter.vel.y:= TargetOffset.y+Hunter.vel.y;
    Hunter.vel.z:= TargetOffset.z+Hunter.vel.z;
{Limit if needed}
   m := max(round(abs(Hunter.vel.x)),
                round(abs(Hunter.vel.y)));
   m := max(m, round(abs(Hunter.vel.z)));
   if(m > {BirdBrainArray[CurrentSeekerK].}BirdsSpeedDX) then begin
   f := {BirdBrainArray[CurrentSeekerK].}BirdsSpeedDX/m;
   Hunter.vel.x := Hunter.vel.x*f;
   Hunter.vel.y := Hunter.vel.y*f;
   Hunter.vel.z := Hunter.vel.z*f;
   end;*)
(*
{    TargetOffset.x:= (BirdsSpeedDX/100)*  TargetOffset.x;
    TargetOffset.y:= (BirdsSpeedDX/100)*  TargetOffset.y;
    TargetOffset.z:= (BirdsSpeedDX/100)*  TargetOffset.z;}
   m := max(round(abs(TargetOffset.x)),
                round(abs(TargetOffset.y)));
   m := max(m, round(abs(TargetOffset.z)));
   if(m > {BirdBrainArray[CurrentSeekerK].}BirdsSpeedDX) then begin
   f := {BirdBrainArray[CurrentSeekerK].}BirdsSpeedDX/m;
   TargetOffset.x := TargetOffset.x*f;
   TargetOffset.y := TargetOffset.y*f;
   TargetOffset.z := TargetOffset.z*f;
   end;
    Hunter.vel.x:= TargetOffset.x+Hunter.vel.x;
    Hunter.vel.y:= TargetOffset.y+Hunter.vel.y;
    Hunter.vel.z:= TargetOffset.z+Hunter.vel.z;
*)
{    DesiredVelocity.x:= Hunter.vel.x-TargetOffset.x;
    DesiredVelocity.y:= Hunter.vel.y-TargetOffset.y;
    DesiredVelocity.z:= Hunter.vel.z-TargetOffset.z;
    Hunter.vel.x:= DesiredVelocity.x+Hunter.vel.x;
    Hunter.vel.y:= DesiredVelocity.y+Hunter.vel.y;
    Hunter.vel.z:= DesiredVelocity.z+Hunter.vel.z;}
(*
{POS}
    TargetOffset.x :=
      Hunter.pos.x-boidArray[CurrentSeekerK,CurrentSeekerJ].pos.x;
    TargetOffset.y :=
      Hunter.pos.y-boidArray[CurrentSeekerK,CurrentSeekerJ].pos.y;
    TargetOffset.z :=
      Hunter.pos.z-boidArray[CurrentSeekerK,CurrentSeekerJ].pos.z;
    TargetOffset.x := Hunter.pos.x-TargetOffset.x;
    TargetOffset.y := Hunter.pos.y-TargetOffset.y;
    TargetOffset.z := Hunter.pos.z-TargetOffset.z;
    Hunter.vel.x:= TargetOffset.x+Hunter.vel.x;
    Hunter.vel.y:= TargetOffset.y+Hunter.vel.y;
    Hunter.vel.z:= TargetOffset.z+Hunter.vel.z;
{Vel}
    TargetOffset.x :=
      Hunter.vel.x-boidArray[CurrentSeekerK,CurrentSeekerJ].vel.x;
    TargetOffset.y :=
      Hunter.vel.y-boidArray[CurrentSeekerK,CurrentSeekerJ].vel.y;
    TargetOffset.z :=
      Hunter.vel.z-boidArray[CurrentSeekerK,CurrentSeekerJ].vel.z;
    DesiredVelocity.x:= Hunter.vel.x-TargetOffset.x;
    DesiredVelocity.y:= Hunter.vel.y-TargetOffset.y;
    DesiredVelocity.z:= Hunter.vel.z-TargetOffset.z;
    Hunter.vel.x:= DesiredVelocity.x+Hunter.vel.x;
    Hunter.vel.y:= DesiredVelocity.y+Hunter.vel.y;
    Hunter.vel.z:= DesiredVelocity.z+Hunter.vel.z;

{Limit if needed}
   m := max(round(abs(Hunter.vel.x)),
                round(abs(Hunter.vel.y)));
   m := max(m, round(abs(Hunter.vel.z)));
   if(m > {BirdBrainArray[CurrentSeekerK].}BirdsSpeedDX) then begin
   f := {BirdBrainArray[CurrentSeekerK].}BirdsSpeedDX/m;
   Hunter.vel.x := Hunter.vel.x*f;
   Hunter.vel.y := Hunter.vel.y*f;
   Hunter.vel.z := Hunter.vel.z*f;
   end;
*)
{Position...}
      Hunter.pos.x := Hunter.pos.x + Hunter.vel.x;
      Hunter.pos.y := Hunter.pos.y + Hunter.vel.y;
      Hunter.pos.z := Hunter.pos.z + Hunter.vel.z;

{Check if caught}

      if (ChaseIt) then
      begin
        ChaseIt := False;
        AttackBtn.Color := clBtnFace;
{  boidArray[CurrentSeekerK,CurrentSeekerJ].Seeker:=false;   }
      end;

      TargetOffset.x :=
        Hunter.pos.x - boidArray[CurrentSeekerK,
          CurrentSeekerJ].pos.x;
      TargetOffset.y :=
        Hunter.pos.y - boidArray[CurrentSeekerK,
          CurrentSeekerJ].pos.y;
      TargetOffset.z :=
        Hunter.pos.z - boidArray[CurrentSeekerK,
          CurrentSeekerJ].pos.z;
      zf := BirdsWideDX / (dBirdsScaleDX / 10) { 2};
{      zfactor:=(Hunter.pos.z)/zf;}
      dx := TargetOffset.x / zf;
      dy := TargetOffset.y / zf;
      dz := TargetOffset.z / zf;
      if ((abs(dx) < 1 {Hunter.TailSize})
        and (abs(dy) < 1 {Hunter.TailSize})
        and (abs(dz) < 1 {Hunter.TailSize})) then
      begin {Remove bird}
        boidArray[CurrentSeekerK, CurrentSeekerJ].Seeker := false;
{                dec(BirdBrainArray[CurrentSeekerK].BirdsDX);}
        Hunter.pos.x := Random((BirdsWideDX * 2) - BirdsWideDX);
        Hunter.pos.y := Random((BirdsHeightDX * 2) - BirdsHeightDX);
        Hunter.pos.z := Random((BirdsWideDX * 2)) + (BirdsHeightDX);
        Hunter.vel.x := Random(51) - 25;
          {Negative means flying left...}
        Hunter.vel.y := Random(51) - 25; {up.. down}
        Hunter.vel.z := Random(51) - 25; {front to back}
                {/* Hit ground!! */}
{      boidArray[CurrentSeekerK,CurrentSeekerJ].pos.y := BirdsHeightDX;
      boidArray[CurrentSeekerK,CurrentSeekerJ].vel.y := 0;
      boidArray[CurrentSeekerK,CurrentSeekerJ].Perching := True;
      boidArray[CurrentSeekerK,CurrentSeekerJ].Perchtimer :=2000;
      boidArray[CurrentSeekerK,CurrentSeekerJ].winglevel :=
      BirdBrainArray[CurrentSeekerK].TailWideSize;}
      end;

      if (Hunter.upstroke) then
      begin
        if (Hunter.winglevel >= Hunter.TailWideSize)
          then Hunter.upstroke := False
        else inc(Hunter.winglevel)
      end
      else if (Hunter.winglevel <= -Hunter.TailWideSize)
        then Hunter.upstroke := True
      else dec(Hunter.wingLevel);
      PerspectiveHunter;
      if (((Hunter.X >= 0)
        and (Hunter.X < (BirdsWideDX - 1)))
        and ((Hunter.Y >= 0)
        and (Hunter.Y < (BirdsHeightDX - 1))))
        then Hunter.onscreen := True else Hunter.onscreen := False;
      DrawHunter;
    end; {all Caught}
  end; {Chaseit}
end;

procedure TBirdDXForm.PerspectiveHunter;
 {void boid_perspective(Boid boid, int W, int H)}
var
  m: Integer;
  f, zfactor, zf,
    tailx, tailz, tail_lx, tail_lz, tail_rx, tail_rz: double;
begin
{   tail:=veccopy(boid[WhichBoid].vel);}
  tail.x := Hunter.vel.x;
  tail.y := Hunter.vel.y;
  tail.z := Hunter.vel.z;
{   tail_end:=veccopy(boid[WhichBoid].vel); }
  tail_end.x := Hunter.vel.x;
  tail_end.y := Hunter.vel.y;
  tail_end.z := Hunter.vel.z;

  if ((Hunter.pos.z <= 0))
    then Hunter.onscreen := False
  else begin
    zf := BirdsWideDX / (dBirdsScaleDX / 10) { 2};
    zfactor := (Hunter.pos.z) / zf;
    if (zfactor = 0) then
      begin zfactor := 0.001; inc(CrashCount); end;

    Hunter.X := ((BirdsWideDX shr 1) +
      round(Hunter.pos.x / zfactor));
    Hunter.Y := ((BirdsHeightDX shr 1) +
      round(Hunter.pos.y / zfactor));

    Hunter.shadow[0].x := Hunter.X;
    Hunter.shadow[0].y := ((BirdsHeightDX shr 1)
      + round(BirdsHeightDX / zfactor));

{      vecsetmag(tail_end, 20);}
    m := max(round(abs(tail_end.x)), round(abs(tail_end.y)));
    m := max(m, round(abs(tail_end.z)));
    f := Hunter.BirdsTailDX / m;
    tail_end.x := tail_end.x * f;
    tail_end.y := tail_end.y * f;
    tail_end.z := tail_end.z * f;
{      vecdiff(boid[WhichBoid].pos, tail_end, tail_end); }
    tail_end.x := Hunter.pos.x - tail_end.x;
    tail_end.y := Hunter.pos.y - tail_end.y;
    tail_end.z := Hunter.pos.z - tail_end.z;

    zfactor := (tail_end.z) / zf;
    if (zfactor = 0) then
      begin zfactor := 0.001; inc(CrashCount); end;
    Hunter.tail_X := (BirdsWideDX div 2) +
      round(tail_end.x / zfactor);
    Hunter.tail_Y := (BirdsHeightDX div 2) +
      round(tail_end.y / zfactor);
    Hunter.shadow[2].x := Hunter.tail_X;
    Hunter.shadow[2].y := (BirdsHeightDX div 2) +
      round(BirdsHeightDX / zfactor);

{      vecsetmag(tail, 25);}
    m := max(round(abs(tail.x)), round(abs(tail.y)));
    m := max(m, round(abs(tail.z)));
    f := Hunter.TailSize / m;
    tail.x := tail.x * f;
    tail.y := tail.y * f;
    tail.z := tail.z * f;
{      vecdiff(boid[WhichBoid].pos, tail, tail);}
    tail.x := Hunter.pos.x - tail.x;
    tail.y := Hunter.pos.y - tail.y;
    tail.z := Hunter.pos.z - tail.z;
    tailx := -tail.z / Hunter.TailWideSize;
    tailz := tail.x / Hunter.TailWideSize;
    tail_lx := tail.x - tailx;
    tail_lz := tail.z - tailz;
    tail_rx := tail.x + tailx;
    tail_rz := tail.z + tailz;
    tail.y := tail.y - Hunter.wingLevel;

    zfactor := (tail_lz) / zf;
    if (zfactor = 0) then
      begin zfactor := 0.001; inc(CrashCount); end;
    Hunter.tail_lX := (BirdsWideDX shr 1) + round(tail_lx / zfactor);
    Hunter.tail_lY := (BirdsHeightDX shr 1) + round(tail.y /
      zfactor);
    Hunter.shadow[1].x := Hunter.tail_lX;
    Hunter.shadow[1].y := (BirdsHeightDX shr 1) +
      round(BirdsHeightDX / zfactor);

    zfactor := (tail_rz) / zf;
    if (zfactor = 0) then
      begin zfactor := 0.001; inc(CrashCount); end;
    Hunter.tail_rX := (BirdsWideDX shr 1) + round(tail_rx / zfactor);
    Hunter.tail_rY := (BirdsHeightDX shr 1) + round(tail.y /
      zfactor);
    Hunter.shadow[3].x := Hunter.tail_rX;
    Hunter.shadow[3].y := (BirdsHeightDX shr 1) +
      round(BirdsHeightDX / zfactor);
  end;
end;


procedure TBirdDXForm.DrawHunter;
{Boid boid, Pixmap freshmap)}
var
  outline_index: Integer;
  lwing, rwing: array[0..2] of TPoint;
begin
  if Hunter.onscreen then begin
{  DXDraw.Surface.Canvas.Pen.Color := clGray;
  DXDraw.Surface.Canvas.Brush.Color := clGray;
  DXDraw.Surface.Canvas.Polygon([Hunter.shadow[0],
                                Hunter.shadow[1],
                                Hunter.shadow[2],
                                Hunter.shadow[3]]);}

    lwing[0].x := Hunter.X;
    lwing[1].x := Hunter.tail_lX;
    lwing[2].x := Hunter.tail_X;
    lwing[0].y := Hunter.Y;
    lwing[1].y := Hunter.tail_lY;
    lwing[2].y := Hunter.tail_Y;
    rwing[0].x := Hunter.X;
    rwing[1].x := Hunter.tail_rX;
    rwing[2].x := Hunter.tail_X;
    rwing[0].y := Hunter.Y;
    rwing[1].y := Hunter.tail_rY;
    rwing[2].y := Hunter.tail_Y;
    outline_index := (round(Hunter.pos.z * 100) div 31);

   { if moving right => lwing behind rwing }
    if (Hunter.vel.x > 0) then begin
      if (Hunter.tail_lY < Hunter.Y) then
        DXDraw.{Surface.}Canvas.Brush.Color := Hunter.BirdWingColorDX
      else DXDraw.{Surface.}Canvas.Brush.Color := Hunter.BirdsColorDX;
      DXDraw.{Surface.}Canvas.Pen.Color := outlines[outline_index];
      DXDraw.{Surface.}Canvas.Polygon(lwing);
      if (Hunter.tail_rY < Hunter.Y) then
        DXDraw.{Surface.}Canvas.Brush.Color := Hunter.BirdsColorDX
      else
        DXDraw.{Surface.}Canvas.Brush.Color := Hunter.BirdWingColorDX;
      DXDraw.{Surface.}Canvas.Pen.Color := outlines[outline_index];
      DXDraw.{Surface.}Canvas.Polygon(rwing);
    end else
    begin
      if (Hunter.tail_rY < Hunter.Y) then
        DXDraw.{Surface.}Canvas.Brush.Color := Hunter.BirdWingColorDX
      else DXDraw.{Surface.}Canvas.Brush.Color := Hunter.BirdsColorDX;
      DXDraw.{Surface.}Canvas.Pen.Color := outlines[outline_index];
      DXDraw.{Surface.}Canvas.Polygon(lwing);
      if (Hunter.tail_lY < Hunter.Y) then
        DXDraw.{Surface.}Canvas.Brush.Color := Hunter.BirdsColorDX
      else
        DXDraw.{Surface.}Canvas.Brush.Color := Hunter.BirdWingColorDX;
      DXDraw.{Surface.}Canvas.Pen.Color := outlines[outline_index];
      DXDraw.{Surface.}Canvas.Polygon(rwing);
    end;
  end;
end; {Procedure}

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TBirdDXForm.RepeatUntilBirdlanded;
var
  abc, ppp, kkk, K, J: Integer;
  zfactor, zf: Double;
{Rect:TRect;}
begin
  if BirdLandDX then
  begin
    Exit; {Close;}
  end;
Repeat
BEGIN
  {Clear the OffScreen Surface }{255}
{  if (((not DoCentersinn))
    or ((not BirdBGCB.Checked) and (DoCentersinn)))
    then
    with DXDraw.Canvas do  }
  begin
  	DXDraw.Canvas.Pen.Color := clWhite;
    DXDraw.Canvas.Brush.Color := clWhite;
	  {DXDraw.Canvas.FillRect(Canvas.ClipRect);}
    DXDraw.Canvas.FillRect(Rect(0,0,BirdsWideDX,BirdsHeightDX));
  end {else
  DXDraw.Picture.Bitmap.Assign(BirdSurface)};
{      StretchDraw(ARect, Image1.Picture.Bitmap);}

        kkk := 0;
        ppp := 0;
        abc := BirdsDX;
  { Draw Bird }
  case WhichFlockisFlying of
    0: begin
        for J := 0 to BirdsDX - 1 do
        begin
          DXComputeNewHeading(J);
        end;

        for J := 0 to BirdsDX - 1 do begin // For each boid again...
      // Update the velocity and position.
          BirdsDXV[J] := BirdsDXnV[J]; {updated in ???}
          BirdsDYV[J] := BirdsDYnV[J];
          BirdsDXP[J] := BirdsDXP[J] + (BirdsDXV[J] * (BirdsSpeedDX /10) {dt});
          BirdsDYP[J] := BirdsDYP[J] + (BirdsDYV[J] * (BirdsSpeedDX /10) {dt});

      // Wrap around the screen coordinates.
          if (BirdsDXP[j] < 0) then
            BirdsDXP[j] := (BirdsDXP[j] + BirdsWideDX - 1)
          else if (BirdsDXP[j] > BirdsWideDX - 1) then
            BirdsDXP[j] := (BirdsDXP[j] - BirdsWideDX);
          if (BirdsDYP[j] < 0) then
            BirdsDYP[j] := (BirdsDYP[j] + BirdsHeightDX - 1)
          else if (BirdsDYP[j] > BirdsHeightDX - 1) then
            BirdsDYP[j] := (BirdsDYP[j] - BirdsHeightDX);
      //* Redraw the boid.
          DXDrawBird(j);
        end;
      end;
    1: begin
        DXCheckCollision;
        for J := 0 to BirdsDX - 1 do
        begin
          DXComputeNewHeading(J);
        end;

        for J := 0 to BirdsDX - 1 do begin { For each boid again...}
      { Update the velocity and position.}
          BirdsDXV[J] := BirdsDXnV[J]; {updated in ???}
          BirdsDYV[J] := BirdsDYnV[J];
          BirdsDXP[J] := BirdsDXP[J] + (BirdsDXV[J] * (BirdsSpeedDX /
            10) {dt});
          BirdsDYP[J] := BirdsDYP[J] + (BirdsDYV[J] * (BirdsSpeedDX /
            10) {dt});

      { Wrap around the screen coordinates.}
          if (BirdsDXP[j] < 0) then
            BirdsDXP[j] := (BirdsDXP[j] + BirdsWideDX - 1)
          else if (BirdsDXP[j] > BirdsWideDX - 1) then
            BirdsDXP[j] := (BirdsDXP[j] - BirdsWideDX);
          if (BirdsDYP[j] < 0) then
            BirdsDYP[j] := (BirdsDYP[j] + BirdsHeightDX - 1)
          else if (BirdsDYP[j] > BirdsHeightDX - 1) then
            BirdsDYP[j] := (BirdsDYP[j] - BirdsHeightDX);
      {Redraw the boid.}
          DXDrawBird(j);
        end;
        DrawObstacles;

        begin
(*
{          DXInput1.Update;}
    {  Digital  }
          if isLeft in DXInput1.States then
            Label1Left := Label1Left - 10;
          if isRight in DXInput1.States then
            Label1Left := Label1Left + 10;
          if isUp in DXInput1.States then
            Label1Top := Label1Top - 10;
          if isDown in DXInput1.States then
            Label1Top := Label1Top + 10;
          DXDraw.{Surface.}Canvas.Pen.Color := clYellow;
          DXDraw.{Surface.}Canvas.Brush.Color := clBlue;
          DXDraw.{Surface.}Canvas.Ellipse(
            Label1Left - 7,
            Label1Top - 7,
            Label1Left + 7,
            Label1Top + 7);

          if ({(isButton2 in DXInput1.States)or}(isButton3 in
            DXInput1.States)) then
          begin
            Label1Left := (BirdsWideDX div 2);
            Label1Top := (BirdsHeightDX div 2);
            DXInput1.States := DXInput1.States - [isButton3];
          end;
          if isButton1 in DXInput1.States then
          begin
    {Check for HIT}
            for J := 0 to BirdsDX - 1 do
            begin
              if ((BirdsDxp[J] > (Label1Left - 10)) and
                (BirdsDxp[J] < (Label1Left + 10)) and
                (BirdsDyp[J] > (Label1Top - 10)) and
                (BirdsDyp[J] < (Label1Top + 10))
                ) then
              begin
                DXDraw.Surface.Canvas.Pen.Color := clYellow;
                DXDraw.Surface.Canvas.Brush.Color := clRed;
                DXDraw.Surface.Canvas.Ellipse(
                  Label1Left - 70,
                  Label1Top - 70,
                  Label1Left + 70,
                  Label1Top + 70);
              end;
            end;
    {  Next,  button 1 is invalidated until button 1 is pushed.  }
            DXInput1.States := DXInput1.States - [isButton1];

          end;
{    BirdDXForm.Caption := 'Birds 3D '+ Format('POV (Point of view): %d',
    [DXInput1.Joystick.Joystate.rgdwPOV[0]]);}
*)
        end;

      end;

    2: begin
        if BirdBGCB.Checked then
{          DXDraw.Surface.Draw(0, 0,
            BirdSurface.ClientRect,
            BirdSurface, True)};
        RealCenter.x := 0; RealCenter.y := 0; RealCenter.z := 0;
        RealAvgvel.x := 0; RealAvgvel.y := 0; RealAvgvel.z := 0;
        for J := 0 to HowManyBirds - 1 do
        begin
{	 vecadd(RealCenter, boid[J].pos);}
          RealCenter.x := RealCenter.x + boid[J].pos.x;
          RealCenter.y := RealCenter.y + boid[J].pos.y;
          RealCenter.z := RealCenter.z + boid[J].pos.z;
{	 vecadd(RealAvgvel, boid[J].vel); }
          RealAvgvel.x := RealAvgvel.x + boid[J].vel.x;
          RealAvgvel.y := RealAvgvel.y + boid[J].vel.y;
          RealAvgvel.z := RealAvgvel.z + boid[J].vel.z;
        end;
        for J := 0 to HowManyBirds - 1 do
        begin
          MoveBoid(J);
          if (boid[J].onscreen) then drawboid(J);
        end;
      end;
    3: begin
{BirdBrainArray[CurrentFrenzy].  boidArray[K,J]}
        for K := 0 to FrenzyCount - 1 do
        begin
          CurrentFrenzy := K;
          BirdBrainArray[K].RealCenter.x := 0;
          BirdBrainArray[K].RealCenter.y := 0;
          BirdBrainArray[K].RealCenter.z := 0;
          BirdBrainArray[K].RealAvgvel.x := 0;
          BirdBrainArray[K].RealAvgvel.y := 0;
          BirdBrainArray[K].RealAvgvel.z := 0;
          for J := 0 to (BirdBrainArray[K].BirdsDX - 1) do
          begin
            if ((boidArray[K, J].perching = False)) then
            begin
{	 vecadd(RealCenter, boid[J].pos);}
              BirdBrainArray[K].RealCenter.x :=
                BirdBrainArray[K].RealCenter.x + boidArray[K, J].pos.x;
              BirdBrainArray[K].RealCenter.y :=
                BirdBrainArray[K].RealCenter.y + boidArray[K, J].pos.y;
              BirdBrainArray[K].RealCenter.z :=
                BirdBrainArray[K].RealCenter.z + boidArray[K, J].pos.z;
{	 vecadd(RealAvgvel, boid[J].vel); }
              BirdBrainArray[K].RealAvgvel.x :=
                BirdBrainArray[K].RealAvgvel.x + boidArray[K, J].vel.x;
              BirdBrainArray[K].RealAvgvel.y :=
                BirdBrainArray[K].RealAvgvel.y + boidArray[K, J].vel.y;
              BirdBrainArray[K].RealAvgvel.z :=
                BirdBrainArray[K].RealAvgvel.z + boidArray[K, J].vel.z;
            end;
          end;
          for J := 0 to (BirdBrainArray[K].BirdsDX - 1) do
          begin
            MoveFBoid(J);
            if (boidArray[K, J].onscreen = true) then drawFboid(J);
          end;
        end; {K}
      end;
    4: begin
        kkk := 0;
        ppp := 0;
        abc := 0;
        zf := BirdsWideDX / (dBirdsScaleDX / 10) { 2};
        zfactor := (Feeder.z) / zf;
        if (zfactor = 0) then begin zfactor := 0.001; end;
        Feeder.X := ((BirdsWideDX shr 1) +
          round(Feeder.x / zfactor));
        Feeder.Y := ((BirdsHeightDX shr 1) +
          round(Feeder.y / zfactor));
        DXDraw.{Surface.}Canvas.Pen.Color := clYellow;
        DXDraw.{Surface.}Canvas.Brush.Color := clGray;
        DXDraw.{Surface.}Canvas.Ellipse(
          round(Feeder.x - 10),
          round(Feeder.y),
          round(Feeder.x + 10),
          round(Feeder.y + 100));
        for K := 0 to FrenzyCount - 1 do
        begin
          CurrentFrenzy := K;
          BirdBrainArray[K].RealCenter.x := 0;
          BirdBrainArray[K].RealCenter.y := 0;
          BirdBrainArray[K].RealCenter.z := 0;
          BirdBrainArray[K].RealAvgvel.x := 0;
          BirdBrainArray[K].RealAvgvel.y := 0;
          BirdBrainArray[K].RealAvgvel.z := 0;
          for J := 0 to (BirdBrainArray[K].BirdsDX - 1) do
          begin
            if (boidArray[K, J].Hungry = True) then inc(kkk);
            if (boidArray[K, J].perching = True) then inc(ppp);
            if (
              (boidArray[K, J].perching = False) and
              (boidArray[K, J].Hungry = False)) then begin
              inc(abc);
        {	 vecadd(RealCenter, boid[J].pos);}
              BirdBrainArray[K].RealCenter.x :=
                BirdBrainArray[K].RealCenter.x + boidArray[K, J].pos.x;
              BirdBrainArray[K].RealCenter.y :=
                BirdBrainArray[K].RealCenter.y + boidArray[K, J].pos.y;
              BirdBrainArray[K].RealCenter.z :=
                BirdBrainArray[K].RealCenter.z + boidArray[K, J].pos.z;
        {	 vecadd(RealAvgvel, boid[J].vel); }
              BirdBrainArray[K].RealAvgvel.x :=
                BirdBrainArray[K].RealAvgvel.x + boidArray[K, J].vel.x;
              BirdBrainArray[K].RealAvgvel.y :=
                BirdBrainArray[K].RealAvgvel.y + boidArray[K, J].vel.y;
              BirdBrainArray[K].RealAvgvel.z :=
                BirdBrainArray[K].RealAvgvel.z + boidArray[K, J].vel.z;
            end;
          end;
          for J := 0 to (BirdBrainArray[K].BirdsDX - 1) do
          begin
            MoveFFBoid(J);
            if (boidArray[K, J].onscreen = true) then drawFboid(J);
          end;
        end; {K}
      end;
    5: begin
        kkk := 0;
        ppp := 0;
        abc := 0;
        for K := 0 to FrenzyCount - 1 do
        begin
          CurrentFrenzy := K;
          BirdBrainArray[K].RealCenter.x := 0;
          BirdBrainArray[K].RealCenter.y := 0;
          BirdBrainArray[K].RealCenter.z := 0;
          BirdBrainArray[K].RealAvgvel.x := 0;
          BirdBrainArray[K].RealAvgvel.y := 0;
          BirdBrainArray[K].RealAvgvel.z := 0;
          for J := 0 to (BirdBrainArray[K].BirdsDX - 1) do
          begin
            if (boidArray[K, J].Seeker = False) then inc(kkk) else
              if (boidArray[K, J].perching = True) then inc(ppp);
            if ((boidArray[K, J].perching = False) {and
             (boidArray[K,J].Seeker = True)}) then
            begin
              inc(abc);
         {vecadd(RealCenter, boid[J].pos);}
              BirdBrainArray[K].RealCenter.x :=
                BirdBrainArray[K].RealCenter.x + boidArray[K, J].pos.x;
              BirdBrainArray[K].RealCenter.y :=
                BirdBrainArray[K].RealCenter.y + boidArray[K, J].pos.y;
              BirdBrainArray[K].RealCenter.z :=
                BirdBrainArray[K].RealCenter.z + boidArray[K, J].pos.z;
         {	 vecadd(RealAvgvel, boid[J].vel); }
              BirdBrainArray[K].RealAvgvel.x :=
                BirdBrainArray[K].RealAvgvel.x + boidArray[K, J].vel.x;
              BirdBrainArray[K].RealAvgvel.y :=
                BirdBrainArray[K].RealAvgvel.y + boidArray[K, J].vel.y;
              BirdBrainArray[K].RealAvgvel.z :=
                BirdBrainArray[K].RealAvgvel.z + boidArray[K, J].vel.z;
            end;
          end;
          for J := 0 to (BirdBrainArray[K].BirdsDX - 1) do
          begin
            MoveFBoid(J);
            if (boidArray[K, J].onscreen = true) then drawFboid(J);
          end;
        end; {K}
        BoidSeek;
        if (not Hunter.onscreen) then
           StatusBar1.Panels[0].Text:='Hunter Lost';
        if (FrenzyBirdCount = kkk) then BirdLandDX := True;
      end; {5}
  end; {Case}

   { Display the FrameRate }{clWhite;}
  if FPSCB.Checked then
      StatusBar1.Panels[1].Text:=' Crashed: '+ inttostr(CrashCount);
  StatusBar1.Panels[2].Text:=
          inttostr(kkk) + '  birds in the pi '
          + inttostr(ppp) + ' grounded '
          + inttostr(abc) + ' flying';
  if (CrashCount > 10) then BirdLandDX := True;
  Application.ProcessMessages;
END UNTIL BirdLandDX;
end;

procedure TBirdDXForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin  {  Application end  }
  if Key = VK_ESCAPE then Close;
end;


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////



/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TBirdDXForm.BirdsBtnClick(Sender: TObject);
var I, CodeVx, TempBirds: Integer;
begin
  val(BirdsEdit.Text, TempBirds, CodeVx);
  if (CodeVx > 0) then Codefx(BirdsEdit.Text, CodeVx);
  if (TempBirds > 99) then TempBirds := 99
  else if (TempBirds < 4) then TempBirds := 4;
  if (BirdsDX < TempBirds) then
  begin
{Add Birds}
    if (not BirdLandDX) then
    begin
      SetLength(BirdsDXP, TempBirds);
      SetLength(BirdsDYP, TempBirds);
      SetLength(BirdsDXV, TempBirds);
      SetLength(BirdsDYV, TempBirds);
      SetLength(BirdsDXnV, TempBirds);
      SetLength(BirdsDYnV, TempBirds);
  // Set to random initial conditions. */
      for I := BirdsDX - 1 to TempBirds - 1 do
      begin
        BirdsDXP[I] := Random(BirdsWideDX - 1);
        BirdsDYP[I] := Random(BirdsHeightDX - 1);
        BirdsDXV[I] := Random(3) - 1;
        BirdsDYV[I] := Random(3) - 1;
        NormDX(BirdsDXV[I], BirdsDYV[I]);
        BirdsDXV[I] := dBirdxTempDX;
        BirdsDYV[I] := dBirdyTempDX;
      end;
    end;
    BirdsDX := TempBirds;
  end else
    if (BirdsDX > TempBirds) then
    begin
{Delete Birds}
      BirdsDX := TempBirds;
      if (not BirdLandDX) then
      begin
        SetLength(BirdsDXP, TempBirds);
        SetLength(BirdsDYP, TempBirds);
        SetLength(BirdsDXV, TempBirds);
        SetLength(BirdsDYV, TempBirds);
        SetLength(BirdsDXnV, TempBirds);
        SetLength(BirdsDYnV, TempBirds);
      end;
    end;
BirdDXForm.Caption:=Format('There are now %d birds in the pi', [TempBirds]);
end;
{procedure NumericExample;
var
  Saved8087CW: Word;
begin
  Saved8087CW := Default8087CW;
  Set8087CW($133f); // Disable all fpu exceptions
  ThirdPartyRoutine; ... OPENGL!!!! 3D routines
  Set8087CW(Saved8087CW);
end;}
/////////////////////////////////////////////////////
{SONG STUFF}


procedure TBirdDXForm.BirdsColorPanelClick(Sender: TObject);
begin
  if (HasTrueColors = False) then
    ColorDialog1.Options := [cdPreventFullOpen];
  if ColorDialog1.Execute then
  begin
    BirdsColorPanel.Color := ColorDialog1.Color;
    BirdsColorDX := BirdsColorPanel.Color;
  end;
end;

procedure TBirdDXForm.WingColorPanelClick(Sender: TObject);
begin
  if (HasTrueColors = False) then
    ColorDialog1.Options := [cdPreventFullOpen];
  if ColorDialog1.Execute then
  begin
    WingColorPanel.Color := ColorDialog1.Color;
    BirdWingColorDX := WingColorPanel.Color;
  end;
end;

procedure TBirdDXForm.BirdsTailTBChange(Sender: TObject);
begin
  BirdsTailDX := (BirdsTailTB.Position);
  TailWideSize := (BirdsTailDX + (BirdsTailDX div 2));
  TailSize := (BirdsTailDX + ((BirdsTailDX - TailWideSize) div 2));
  SizeLabel.Caption := Format('Size %d ', [BirdsTailDX]);
end;

procedure TBirdDXForm.BirdsScaleTBChange(Sender: TObject);
begin
  dBirdsScaleDX := (BirdsScaleTB.Position);
  ScaleLabel.Caption := Format('Scale %d ', [round(dBirdsScaleDX)]);
end;

procedure TBirdDXForm.dBirdsFeedTBChange(Sender: TObject);
begin
  dBirdsFeedDX := (dBirdsFeedTB.Position / 100);
  FeedLabel.Caption := Format('Feed %f ', [dBirdsFeedDX]);
end;

procedure TBirdDXForm.BirdsActivityTBChange(Sender: TObject);
begin
  BirdsActivityDX := (BirdsActivityTB.Position);
  ActivityLabel.Caption := Format('Activity %d ', [BirdsActivityDX]);
end;


procedure TBirdDXForm.BirdsSpeedTBChange(Sender: TObject);
begin
  BirdsSpeedDX := (BirdsSpeedTB.Position);
  SpeedLabel.Caption := Format('Speed %d ', [BirdsSpeedDX]);
end;

procedure TBirdDXForm.dBirdsMinVTBChange(Sender: TObject);
begin
  dBirdsMinVDX := (dBirdsMinVTB.Position / 100);
  HoverLabel.Caption := Format('Hover %f ', [dBirdsMinVDX]);
end;

procedure TBirdDXForm.dWCopyTBChange(Sender: TObject);
begin
  dWCopyDX := (dWCopyTB.Position / 100);
  dWCopyTBLabel.Caption := Format('%f ', [dWCopyDX]);
end;

procedure TBirdDXForm.RCopyTBChange(Sender: TObject);
begin
  RCopyDX := (RCopyTB.Position);
  CopyLabel.Caption := Format('Copy %d ', [RCopyDX]);
end;


procedure TBirdDXForm.BirdsGlideTBChange(Sender: TObject);
begin
  dBirdsGlideDX := (BirdsGlideTB.Position / 100);
  GlideLabel.Caption := Format('Glide %f ', [dBirdsGlideDX]);
end;

procedure TBirdDXForm.dBirdsMomentTBChange(Sender: TObject);
begin
  dBirdsMomentDX := (dBirdsMomentTB.Position / 100);
  TurnLabel.Caption := Format('Turn %f ', [dBirdsMomentDX]);
end;

procedure TBirdDXForm.RCentroidTBChange(Sender: TObject);
begin
  RCentroidDX := (RCentroidTB.Position);
  CenterLabel.Caption := Format('Center %d ', [RCentroidDX]);
end;

procedure TBirdDXForm.dWCentroidTBChange(Sender: TObject);
begin
  dWCentroidDX := (dWCentroidTB.Position / 100);
  dWCentroidTBLabel.Caption := Format('%f ', [dWCentroidDX]);
end;

procedure TBirdDXForm.FerocityTBChange(Sender: TObject);
begin
  BirdsFerocityDX := (FerocityTB.Position);
  FerocityLabel.Caption := Format('Ferocity %d ', [BirdsFerocityDX]);
end;

procedure TBirdDXForm.dBirdsViewTBChange(Sender: TObject);
begin
  dBirdsViewDX := (dBirdsViewTB.Position); {Degrees}
  ViewLabel.Caption := Format('View %d ', [round(dBirdsViewDX)]);
end;

procedure TBirdDXForm.RVisualTBChange(Sender: TObject);
begin
  RVisualDX := (RVisualTB.Position);
  if (RVisualDX < BirdsTailDX) then
  begin
    RVisualDX := BirdsTailDX;
    RVisualTB.Position := RVisualDX;
  end;
  VDistanceLabel.Caption := Format('Distance %d ', [RVisualDX]);
end;

procedure TBirdDXForm.dWVisualTBChange(Sender: TObject);
begin
  dWVisualDX := (dWVisualTB.Position / 100);
  dWVisualTBLabel.Caption := Format('%f ', [dWVisualDX]);
end;

procedure TBirdDXForm.TenacityTBChange(Sender: TObject);
begin
  BirdsTenacityDX := (TenacityTB.Position);
  DefendLabel.Caption := Format('Defend %d ', [BirdsTenacityDX]);
end;

procedure TBirdDXForm.dBirdsAvoidTBChange(Sender: TObject);
begin
  dBirdsAvoidDX := (dBirdsAvoidTB.Position); {Degrees}
  AvoidLabel.Caption := Format('Avoid %d ', [round(dBirdsAvoidDX)]);
end;

procedure TBirdDXForm.RAvoidTBChange(Sender: TObject);
begin
  RAvoidDX := (RAvoidTB.Position);
  if (RAvoidDX < BirdsTailDX) then
  begin
    RAvoidDX := BirdsTailDX;
    RAvoidTB.Position := RAvoidDX;
  end;
  ADistanceLabel.Caption := Format('Distance %d ', [RAvoidDX]);
end;

procedure TBirdDXForm.dWAvoidTBChange(Sender: TObject);
begin
  dWAvoidDX := (dWAvoidTB.Position / 100);
  dWAvoidTBLabel.Caption := Format('%f ', [dWAvoidDX]);
{If DoCentersinn then dWAvoidDX:=dWAvoidDX*10; }
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TBirdDXForm.BirdsOKBtnClick(Sender: TObject);
begin
  ReadBirdsData; {Translate the Edits into Variables}
  BirdsOKBtn.Enabled := False;
  BirdLandDX := True;
  DoBirdseye := False;
  DoMazepole := False;
  DoCentersinn := False;
  DoFrilight := False;
  DoFeedrenzy := False;
  DoPredrenzy := False;
  CrashCount := 0;
  ChaseIt := False;
  AttackBtn.Color := clBtnFace;
  case BirdsSizeRG.ItemIndex of
    0: begin
        WhichFlockisFlying := 0;
        DoBirdseye := True;
        BirdsTailTB.Enabled := True;
        BirdsScaleTB.Enabled := False; BirdsScaleTB.Visible := False;
        dBirdsFeedTB.Enabled := False; dBirdsFeedTB.Visible := False;
{BirdsColorPanel.Color;
WingColorPanel.Color;  }
        BirdsActivityTB.Enabled := False;
        BirdsActivityTB.Visible := False;
        BirdsSpeedTB.Enabled := True;
        dBirdsMinVTB.Enabled := True;
        RCopyTB.Enabled := True;
        dWCopyTB.Enabled := True;
        BirdsGlideTB.Enabled := False; BirdsGlideTB.Visible := False;
        dBirdsMomentTB.Enabled := True;
        RCentroidTB.Enabled := True;
        dWCentroidTB.Enabled := True;
        FerocityTB.Enabled := False; FerocityTB.Visible := False;
        dBirdsViewTB.Enabled := True;
        RVisualTB.Enabled := True;
        dWVisualTB.Enabled := True;
        TenacityTB.Enabled := False; TenacityTB.Visible := False;
        dBirdsAvoidTB.Enabled := True;
        RAvoidTB.Enabled := True;
        dWAvoidTB.Enabled := True;
        BirdseyeStartup;
      end;
    1: begin
        Label1Left := (BirdsWideDX div 2);
        Label1Top := (BirdsHeightDX div 2);
        WhichFlockisFlying := 1;
        DoMazepole := True;
        BirdsTailTB.Enabled := True;
        BirdsScaleTB.Enabled := False; BirdsScaleTB.Visible := False;
        dBirdsFeedTB.Enabled := False; dBirdsFeedTB.Visible := False;
{BirdsColorPanel.Color;
WingColorPanel.Color;  }
        BirdsActivityTB.Enabled := False;
        BirdsActivityTB.Visible := False;
        BirdsSpeedTB.Enabled := True;
        dBirdsMinVTB.Enabled := True;
        RCopyTB.Enabled := True;
        dWCopyTB.Enabled := True;
        BirdsGlideTB.Enabled := False; BirdsGlideTB.Visible := False;
        dBirdsMomentTB.Enabled := True;
        RCentroidTB.Enabled := True;
        dWCentroidTB.Enabled := True;
        FerocityTB.Enabled := False; FerocityTB.Visible := False;
        dBirdsViewTB.Enabled := True;
        RVisualTB.Enabled := True;
        dWVisualTB.Enabled := True;
        TenacityTB.Enabled := False; TenacityTB.Visible := False;
        dBirdsAvoidTB.Enabled := True;
        RAvoidTB.Enabled := True;
        dWAvoidTB.Enabled := True;
        ObstacleCount := 1;
        SetLength(ObstacleArray, 1);
        ObstacleArray[0].X1 := (BirdsWideDX div 2) - 7;
        ObstacleArray[0].Y1 := (BirdsHeightDX div 2) - 7;
        ObstacleArray[0].X2 := (BirdsWideDX div 2) + 7;
        ObstacleArray[0].Y2 := (BirdsHeightDX div 2) + 7;
        ObstacleArray[ObstacleCount - 1].Z := BirdsHeightDX;
        MazepoleStartup;
      end;
    2: begin
        WhichFlockisFlying := 2;
        DoCentersinn := True;
        BirdsTailTB.Enabled := True;
        BirdsScaleTB.Enabled := True;
        dBirdsFeedTB.Enabled := False; dBirdsFeedTB.Visible := False;
{BirdsColorPanel.Color;
WingColorPanel.Color;  }
        BirdsActivityTB.Enabled := True;
        BirdsSpeedTB.Enabled := True;
        dBirdsMinVTB.Enabled := False; dBirdsMinVTB.Visible := False;
        RCopyTB.Enabled := False; RCopyTB.Visible := False;
        dWCopyTB.Enabled := True;
        BirdsGlideTB.Enabled := False; BirdsGlideTB.Visible := False;
        dBirdsMomentTB.Enabled := False;
        dBirdsMomentTB.Visible := False;
        RCentroidTB.Enabled := False; RCentroidTB.Visible := False;
        dWCentroidTB.Enabled := True;
        FerocityTB.Enabled := False; FerocityTB.Visible := False;
        dBirdsViewTB.Enabled := False; dBirdsViewTB.Visible := False;
        RVisualTB.Enabled := False; RVisualTB.Visible := False;
        dWVisualTB.Enabled := False; dWVisualTB.Visible := False;
        TenacityTB.Enabled := False; TenacityTB.Visible := False;
        dBirdsAvoidTB.Enabled := False;
        dBirdsAvoidTB.Visible := False;
        RAvoidTB.Enabled := True;
        dWAvoidTB.Enabled := True;
        CentersinnStartup;
      end;

    3: begin {3D}
        if FrenzyFilesLoaded then
        begin
          FrenzyFilesArray;
          WhichFlockisFlying := 3;
          DoFrilight := True;
          HideTheMenu;
          dBirdsFeedTB.Enabled := False;
          dBirdsFeedTB.Visible := False;
          FrilightStartup;
        end else
        begin
          BirdsOKBtn.Enabled := True;
          ShowMessage('Load a Frenzy file first');
        end;
      end;
    4: begin {3D}
        if FrenzyFilesLoaded then
        begin
          FrenzyFilesArray;
          WhichFlockisFlying := 4;
          Feeder.x := (BirdsWideDX div 2);
          Feeder.y := (BirdsHeightDX div 2);
          Feeder.z := (BirdsWideDX + (BirdsWideDX div 2));
          DoFeedrenzy := True;
          HideTheMenu;
          FeedrenzyStartup;
        end else
        begin
          BirdsOKBtn.Enabled := True;
          ShowMessage('Load a Frenzy file first');
        end;
      end;
    5: begin {3D}
        if FrenzyFilesLoaded then
        begin
          FrenzyFilesArray;
          WhichFlockisFlying := 5;
          DoPredrenzy := True;
          HideTheMenu;
          BirdsSpeedTB.Enabled := True; {Hunter Speed}
          BirdsSpeedTB.Visible := True;
          dBirdsFeedTB.Enabled := False; {No eating  except hunter}
          dBirdsFeedTB.Visible := False;
          PredrenzyStartup;
        end else
        begin
          BirdsOKBtn.Enabled := True;
          ShowMessage('Load a Frenzy file first');
        end;
      end;
  end; {case}
end;

procedure TBirdDXForm.HideTheMenu;
begin
  BirdsTailTB.Enabled := False;
  BirdsScaleTB.Enabled := True;
  dBirdsFeedTB.Enabled := True;
  BirdsActivityTB.Enabled := False;
  BirdsSpeedTB.Enabled := False;
  dBirdsMinVTB.Enabled := False;
  RCopyTB.Enabled := False;
  dWCopyTB.Enabled := False;
  BirdsGlideTB.Enabled := False;
  dBirdsMomentTB.Enabled := False;
  RCentroidTB.Enabled := False;
  dWCentroidTB.Enabled := False;
  FerocityTB.Enabled := False;
  dBirdsViewTB.Enabled := False;
  RVisualTB.Enabled := False;
  dWVisualTB.Enabled := False;
  TenacityTB.Enabled := False;
  dBirdsAvoidTB.Enabled := False;
  RAvoidTB.Enabled := False;
  dWAvoidTB.Enabled := False;

  BirdsTailTB.Visible := False;
  BirdsScaleTB.Visible := True;
  dBirdsFeedTB.Visible := True;
  BirdsActivityTB.Visible := False;
  BirdsSpeedTB.Visible := False;
  dBirdsMinVTB.Visible := False;
  RCopyTB.Visible := False;
  dWCopyTB.Visible := False;
  BirdsGlideTB.Visible := False;
  dBirdsMomentTB.Visible := False;
  RCentroidTB.Visible := False;
  dWCentroidTB.Visible := False;
  FerocityTB.Visible := False;
  dBirdsViewTB.Visible := False;
  RVisualTB.Visible := False;
  dWVisualTB.Visible := False;
  TenacityTB.Visible := False;
  dBirdsAvoidTB.Visible := False;
  RAvoidTB.Visible := False;
  dWAvoidTB.Visible := False;
end;

procedure TBirdDXForm.EnableAll;
begin
  BirdsTailTB.Enabled := True;
  BirdsScaleTB.Enabled := True;
  dBirdsFeedTB.Enabled := True;
{BirdsColorPanel.Color;
WingColorPanel.Color;  }
  BirdsActivityTB.Enabled := True;
  BirdsSpeedTB.Enabled := True;
  dBirdsMinVTB.Enabled := True;
  RCopyTB.Enabled := True;
  dWCopyTB.Enabled := True;
  BirdsGlideTB.Enabled := True;
  dBirdsMomentTB.Enabled := True;
  RCentroidTB.Enabled := True;
  dWCentroidTB.Enabled := True;
  FerocityTB.Enabled := True;
  dBirdsViewTB.Enabled := True;
  RVisualTB.Enabled := True;
  dWVisualTB.Enabled := True;
  TenacityTB.Enabled := True;
  dBirdsAvoidTB.Enabled := True;
  RAvoidTB.Enabled := True;
  dWAvoidTB.Enabled := True;
end;

procedure TBirdDXForm.VisibleAll;
begin
  BirdsTailTB.Visible := True;
  BirdsScaleTB.Visible := True;
  dBirdsFeedTB.Visible := True;
{BirdsColorPanel.Color;
WingColorPanel.Color;  }
  BirdsActivityTB.Visible := True;
  BirdsSpeedTB.Visible := True;
  dBirdsMinVTB.Visible := True;
  RCopyTB.Visible := True;
  dWCopyTB.Visible := True;
  BirdsGlideTB.Visible := True;
  dBirdsMomentTB.Visible := True;
  RCentroidTB.Visible := True;
  dWCentroidTB.Visible := True;
  FerocityTB.Visible := True;
  dBirdsViewTB.Visible := True;
  RVisualTB.Visible := True;
  dWVisualTB.Visible := True;
  TenacityTB.Visible := True;
  dBirdsAvoidTB.Visible := True;
  RAvoidTB.Visible := True;
  dWAvoidTB.Visible := True;
end;

procedure TBirdDXForm.CancelBitBtnClick(Sender: TObject);
begin
{  If BirdSinging then DXWaveList1.Items[0].WaveCollection[0].Stop;}
  BirdLandDX := True;
  Application.ProcessMessages;
  BirdDXForm.Caption := 'Birds 2.5 D';
  VisibleAll;
  EnableAll;
  Application.ProcessMessages;
  BirdsOKBtn.Enabled := True;
  DoBirdseye := False;
  DoCentersinn := False;
  DoMazepole := False;
  DoFrilight := False;
  DoFeedrenzy := False;
  DoPredrenzy := False;
  CrashCount := 0;
  SetLength(BirdsDXP, 0);
  SetLength(BirdsDYP, 0);
  SetLength(BirdsDXV, 0);
  SetLength(BirdsDYV, 0);
  SetLength(BirdsDXnV, 0);
  SetLength(BirdsDYnV, 0);
  Setlength(boid, 0);
  SetLength(BirdBrainArray, 0);
  SetLength(BoidArray, 0, 0);
  SetLength(ObstacleArray, 0);
end;

procedure TBirdDXForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(8000);
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TBirdDXForm.BirdsFrenzyBtnClick(Sender: TObject);
begin
  FrenzyForm.Show;
end;

procedure TBirdDXForm.FrenzyFilesArray;
var
  j, i: Integer;
  FrenzyStr: string;
begin
  FrenzyFilesLoaded := False;
  if (FrenzyForm.FrenzyLB.Items.Count > 0) then
  begin
    FrenzyCount := 0;
    FrenzyBirdCount := 0;
    ChDir(BirdLifeDir);
    j := 0;
    for i := 0 to (FrenzyForm.FrenzyLB.Items.Count - 1) do begin
      FrenzyStr := FrenzyForm.FrenzyLB.Items.Strings[i];
      if FileExists(FrenzyStr) then begin
        BirdFileOpen(FrenzyStr);
        SetLength(BirdBrainArray, (FrenzyCount + 1));
        CurrentBird := FrenzyCount;
        SetBirdsDatatoArray;
        SetLength(BoidArray, (FrenzyCount + 1));
        inc(FrenzyCount);
      end else showmessage(FrenzyStr + ' not found');
    end;
    for i := 0 to (FrenzyCount - 1) do begin
      SetLength(BoidArray[(i)], BirdBrainArray[i].BirdsDX);
      j := j + BirdBrainArray[i].BirdsDX;
      FrenzyBirdCount := (FrenzyBirdCount +
        BirdBrainArray[i].BirdsDX);
    end;
    FrenzyFilesLoaded := True;
    BirdDXForm.Caption := Format('There are now %d birds in the pi',
      [j]);
    ReadBirdsData; {Reset to prior}
  end else showmessage('Please load a Frenzy file first');
end;

procedure TBirdDXForm.SetBirdsDatatoArray;
begin
{BirdBrainArray[BirdNameCB.ItemIndex].BirdName:=BirdNameCB.Items[BirdNameCB.ItemIndex];}
  BirdBrainArray[CurrentBird].BirdsColorDX := BirdsColorDX;
  BirdBrainArray[CurrentBird].BirdWingColorDX := BirdWingColorDX;
  BirdBrainArray[CurrentBird].BirdName := CurrentBirdName;
  BirdBrainArray[CurrentBird].CurrentBirdSong := CurrentBirdSong;
  BirdBrainArray[CurrentBird].BirdsDX := BirdsDX;
  BirdBrainArray[CurrentBird].dBirdsFeedDX := dBirdsFeedDX;
  BirdBrainArray[CurrentBird].BirdsTailDX := BirdsTailDX;
  BirdBrainArray[CurrentBird].dBirdsMinVDX := dBirdsMinVDX;
  BirdBrainArray[CurrentBird].dBirdsMomentDX := dBirdsMomentDX;
  BirdBrainArray[CurrentBird].dBirdsViewDX := dBirdsViewDX;
  BirdBrainArray[CurrentBird].dBirdsAvoidDX := dBirdsAvoidDX;
  BirdBrainArray[CurrentBird].RCopyDX := RCopyDX;
  BirdBrainArray[CurrentBird].dWCopyDX := dWCopyDX;
  BirdBrainArray[CurrentBird].RCentroidDX := RCentroidDX;
  BirdBrainArray[CurrentBird].dWCentroidDX := dWCentroidDX;
  BirdBrainArray[CurrentBird].RVisualDX := RVisualDX;
  BirdBrainArray[CurrentBird].dWVisualDX := dWVisualDX;
  BirdBrainArray[CurrentBird].RAvoidDX := RAvoidDX;
  BirdBrainArray[CurrentBird].dWAvoidDX := dWAvoidDX;
  BirdBrainArray[CurrentBird].BirdsTenacityDX := BirdsTenacityDX;
  BirdBrainArray[CurrentBird].BirdsFerocityDX := BirdsFerocityDX;
  BirdBrainArray[CurrentBird].BirdsActivityDX := BirdsActivityDX;
  BirdBrainArray[CurrentBird].dBirdsGlideDX := dBirdsGlideDX;
  BirdBrainArray[CurrentBird].dBirdsEnergyDX := dBirdsScaleDX;
  BirdBrainArray[CurrentBird].BirdsSpeedDX := BirdsSpeedDX;
{Randomize initial positions}
{BirdsXP,BirdsYP,BirdsZP,
BirdsXV,BirdsYV,BirdsZV,
BirdsXnV,BirdsYnV,BirdsZnV}
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TBirdDXForm.BirdFileBtnClick(Sender: TObject);
begin
  BirdLandDX := True;
  OpenDialog1.Title := 'Bird Files';
  OpenDialog1.Filter := 'Birds (*.brd)|*.brd';
  OpenDialog1.InitialDir := BirdLifeDir;
  OpenDialog1.Filename := BirdsNameEdit.Text;
  if OpenDialog1.Execute then begin
    if ((FileExists(OpenDialog1.Filename))
      and (lowercase(ExtractFileExt(OpenDialog1.Filename)) = '.brd'))
      then
    begin
      BirdsNameEdit.Text := ExtractFileName(OpenDialog1.Filename);
      BirdFileOpen(OpenDialog1.Filename);
      SetBirdsArraytoForm;
    end else showmessage('file booboo');
  end;
  BirdsOKBtn.Enabled := True;
end;

procedure TBirdDXForm.BirdFileOpen(BirdFileName: string);
var
  F: Textfile;
  Version:Boolean;
  dtemp:Double;
  CodeVx: Integer;
  DataString: string;
begin
  AssignFile(F, BirdFileName);
  Reset(F);
  Readln(F, DataString);
  If (DataString='Version 1.0') then Version:=True
  else Version:=false;
  If Version then ;
  Readln(F, DataString);
  CurrentBirdName := DataString;
  Readln(F, DataString);
  CurrentBirdSong := DataString;
  Readln(F, DataString);
  CurrentBirdBodyFile := DataString;
  Readln(F, DataString);
  CurrentBirdTexture := DataString;
  Readln(F, DataString);
  val(DataString, CurrentBirdsRandomosity, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdsDX, CodeVx);  {BirdsCount}
  if CodeVx > 0 then Codefx(DataString, CodeVx);

  {Readln(F, DataString);
  val(DataString, dBirdsScaleDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx); }
  Readln(F, DataString);    {BirdsSize}
  val(DataString, BirdsTailDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, dBirdsFeedDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);{dBirdsSexy}
  val(DataString, dBirdsSexy, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdWingColorDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdsColorDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdsActivityDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);  {dBirdsEnergy}
  val(DataString, dBirdsEnergy, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString,dtemp , CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  BirdsSpeedDX:=Round(dtemp*100);
  Readln(F, DataString);
  val(DataString, dBirdsMinVDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, RCopyDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, dWCopyDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);

  Readln(F, DataString);
  val(DataString, dBirdsGlideDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, dBirdsMomentDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, RCentroidDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, dWCentroidDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);

  Readln(F, DataString);
  val(DataString, BirdsFerocityDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, dBirdsViewDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, RVisualDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, dWVisualDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);


  Readln(F, DataString);
  val(DataString, BirdsTenacityDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, dBirdsAvoidDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, RAvoidDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, dWAvoidDX, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  CloseFile(F);
end;

procedure TBirdDXForm.SetBirdsArraytoForm;
var BirdString: string;
begin
{CurrentBird:= BirdNameCB.ItemIndex;}
{val(BirdsEdit.Text,BirdsDX,CodeVx);
If (CodeVx > 0) then Codefx(BirdsEdit.Text,CodeVx); }
  str(BirdsDX, BirdString);
  BirdsEdit.Text := BirdString;
  dBirdsFeedTB.Position := trunc(dBirdsFeedDX * 100);
  BirdsTailTB.Position := BirdsTailDX;
  dBirdsMinVTB.Position := trunc(dBirdsMinVDX * 100);
  dBirdsMomentTB.Position := trunc(dBirdsMomentDX * 100);
  dBirdsViewTB.Position := trunc(dBirdsViewDX);
  dBirdsAvoidTB.Position := trunc(dBirdsAvoidDX);
  RCopyTB.Position := RCopyDX;
  dWCopyTB.Position := trunc(dWCopyDX * 100);
  RCentroidTB.Position := RCentroidDX;
  dWCentroidTB.Position := trunc(dWCentroidDX * 100);
  RVisualTB.Position := RVisualDX;
  dWVisualTB.Position := trunc(dWVisualDX * 100);
  RAvoidTB.Position := RAvoidDX;
  dWAvoidTB.Position := trunc(dWAvoidDX * 100);
  TenacityTB.Position := BirdsTenacityDX;
  FerocityTB.Position := BirdsFerocityDX;
  BirdsActivityTB.Position := BirdsActivityDX;
  BirdsGlideTB.Position := trunc(dBirdsGlideDX * 100);
  BirdsScaleTB.Position := trunc(dBirdsScaleDX);
  BirdsSpeedTB.Position := BirdsSpeedDX;
  BirdsNameEdit.Text := CurrentBirdName;
  SongEdit.Text := CurrentBirdSong;
{SongEdit.ItemIndex:=CurrentSong;}
{BirdsColorDX:=BirdsColorDX; }
  BirdsColorPanel.Color := BirdsColorDX;
{BirdWingColorDX:=BirdWingColorDX;}
  WingColorPanel.Color := BirdWingColorDX;
  BirdsLoadedLabel;
end;
procedure TBirdDXForm.ClearBtnClick(Sender: TObject);
begin
  SetBirdsDefault;
  SetBirdsArraytoForm;
end;

procedure TBirdDXForm.SetBirdsDefault;
begin
  BirdLandDX := True;
  FrenzyFilesLoaded:= False;
  BirdSinging := False;
  CurrentBirdName := 'Big Red.brd';
  CurrentBirdSong := 'Big Red.wav';
  CurrentBirdBodyFile := 'Big Red.3ds';
  CurrentBirdTexture := 'Big Red.bmp';
CurrentBirdsRandomosity:= 2;
  BirdsDX := 20;
  BirdsTailDX := 14;
  dBirdsFeedDX := 0.38;
dBirdsSexy := 0.24;
  BirdsColorDX := clMaroon;
  BirdWingColorDX := clRed;
  BirdsActivityDX := 88;
dBirdsEnergy:= 0.78;
  dBirdsScaleDX := 20;
  BirdsSpeedDX := 75;
  dBirdsMinVDX := 0.5;
  RCopyDX := 60;
  dWCopyDX := 0.3;
  dBirdsGlideDX := 0.7;
  dBirdsMomentDX := 0.9;
    RCentroidDX := 50;
  dWCentroidDX := 0.4;
  BirdsFerocityDX := 10;
  dBirdsViewDX := 270;
  RVisualDX := 30;
  dWVisualDX := 0.6;
  BirdsTenacityDX := 15;
  dBirdsAvoidDX := 90;
  RAvoidDX := 15;
  dWAvoidDX := 0.9;
  CurrentBird := 0;
end;

procedure TBirdDXForm.BirdsLoadedLabel;
begin
  SizeLabel.Caption := Format('Size %d ', [BirdsTailDX]);
  ScaleLabel.Caption := Format('Scale %d ', [round(dBirdsScaleDX)]);
  FeedLabel.Caption := Format('Feed %f ', [dBirdsFeedDX]);
  ActivityLabel.Caption := Format('Activity %d ', [BirdsActivityDX]);
  SpeedLabel.Caption := Format('Speed %d ', [BirdsSpeedDX]);
  HoverLabel.Caption := Format('Hover %f ', [dBirdsMinVDX]);
  dWCopyTBLabel.Caption := Format('%f ', [dWCopyDX]);
  CopyLabel.Caption := Format('Copy %d ', [RCopyDX]);
  GlideLabel.Caption := Format('Glide %f ', [dBirdsGlideDX]);
  TurnLabel.Caption := Format('Turn %f ', [dBirdsMomentDX]);
  CenterLabel.Caption := Format('Center %d ', [RCentroidDX]);
  dWCentroidTBLabel.Caption := Format('%f ', [dWCentroidDX]);
  FerocityLabel.Caption := Format('Ferocity %d ', [BirdsFerocityDX]);
  ViewLabel.Caption := Format('View %d ', [round(dBirdsViewDX)]);
  VDistanceLabel.Caption := Format('Distance %d ', [RVisualDX]);
  dWVisualTBLabel.Caption := Format('%f ', [dWVisualDX]);
  DefendLabel.Caption := Format('Defend %d ', [BirdsTenacityDX]);
  AvoidLabel.Caption := Format('Avoid %d ', [round(dBirdsAvoidDX)]);
  ADistanceLabel.Caption := Format('Distance %d ', [RAvoidDX]);
  dWAvoidTBLabel.Caption := Format('%f ', [dWAvoidDX]);
end;

procedure TBirdDXForm.BirdsSaveBtnClick(Sender: TObject);
begin
  SaveBirdsFile;
end;

procedure TBirdDXForm.SaveBirdsFile;
var
  F: Textfile;
   {PathS,} DataString, BirdString: string;
begin
  SaveDialog1.Title := 'Bird Files';
  SaveDialog1.Filter := 'Birds (*.brd)|*.brd';
  SaveDialog1.InitialDir := BirdLifeDir;
  SaveDialog1.Filename := BirdsNameEdit.Text;
  if SaveDialog1.Execute then begin
    if ((lowercase(ExtractFileExt(SaveDialog1.Filename)) = '.brd'))
      then
    begin {Load}
      SetBirdsDefault;{to also fill other type data}
      ReadBirdsData;
      AssignFile(F, SaveDialog1.Filename);
      Rewrite(F);
      writeln(F,'Version 1.0');
      BirdString := CurrentBirdName;
      writeln(F, BirdString);
      BirdString := CurrentBirdSong;
      writeln(F, BirdString);
      BirdString := CurrentBirdBodyFile;
      writeln(F, BirdString);
      BirdString := CurrentBirdTexture;
      writeln(F, BirdString);
      str(CurrentBirdsRandomosity, DataString);{BirdsRandomosity}
      BirdString := DataString;
      writeln(F, BirdString);
      str(BirdsDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      BirdString := DataString;
      str(dBirdsScaleDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(BirdsTailDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);

      str(dBirdsFeedDX: 6: 3, DataString);
      BirdString := DataString;
      writeln(F, BirdString);

      str(dBirdsSexy: 6: 3, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(BirdWingColorDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(BirdsColorDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(BirdsActivityDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);

      str(dBirdsEnergy: 6: 3, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(((BirdsSpeedDX)/100): 6: 3, DataString);
      str(BirdsSpeedDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(dBirdsMinVDX: 6: 3, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(RCopyDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(dWCopyDX: 6: 3, DataString);
      BirdString := DataString;
      writeln(F, BirdString);

      str(dBirdsGlideDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(dBirdsMomentDX: 6: 3, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(RCentroidDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(dWCentroidDX: 6: 3, DataString);
      BirdString := DataString;
      writeln(F, BirdString);

      str(BirdsFerocityDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(dBirdsViewDX: 8: 3, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(RVisualDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(dWVisualDX: 6: 3, DataString);
      BirdString := DataString;
      writeln(F, BirdString);

      str(BirdsTenacityDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(dBirdsAvoidDX: 8: 3, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(RAvoidDX, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      str(dWAvoidDX: 6: 3, DataString);
      BirdString := DataString;
      writeln(F, BirdString);
      CloseFile(F);
    end;
  end;
end;

procedure TBirdDXForm.ReadBirdsData;
var {BirdssPer,ChipPerset,}  CodeVx: Integer;
begin
{Translate the Edits into Variables}
  val(BirdsEdit.Text, BirdsDX, CodeVx);
  if (CodeVx > 0) then Codefx(BirdsEdit.Text, CodeVx);
  dBirdsFeedDX := (dBirdsFeedTB.Position / 100);
  BirdsTailDX := (BirdsTailTB.Position);
  dBirdsMinVDX := (dBirdsMinVTB.Position / 100);
  dBirdsMomentDX := (dBirdsMomentTB.Position / 100);
  dBirdsViewDX := (dBirdsViewTB.Position);
  dBirdsAvoidDX := (dBirdsAvoidTB.Position);
  RCopyDX := (RCopyTB.Position);
  dWCopyDX := (dWCopyTB.Position / 100);
  RCentroidDX := (RCentroidTB.Position);
  dWCentroidDX := (dWCentroidTB.Position / 100);
  RVisualDX := (RVisualTB.Position);
  dWVisualDX := (dWVisualTB.Position / 100);
  RAvoidDX := (RAvoidTB.Position);
  dWAvoidDX := (dWAvoidTB.Position / 100);
  BirdsTenacityDX := (TenacityTB.Position);
  BirdsFerocityDX := (FerocityTB.Position);
  BirdsActivityDX := (BirdsActivityTB.Position);
  dBirdsGlideDX := (BirdsGlideTB.Position / 100);
  dBirdsScaleDX := (BirdsScaleTB.Position);
  BirdsSpeedDX := (BirdsSpeedTB.Position);
{CurrentBird:= BirdNameCB.ItemIndex; }
  CurrentBirdName := BirdsNameEdit.Text;
  CurrentBirdSong := SongEdit.Text;
{CurrentSong:=SongEdit.ItemIndex;}
  BirdsColorDX := BirdsColorPanel.Color;
  BirdWingColorDX := WingColorPanel.Color;
end;




procedure TBirdDXForm.PrintitBtnClick(Sender: TObject);
var
  F: Textfile;
  PathS, DataString, BirdString: string;
begin
  PathS := ExtractFilePath(Application.EXEName);
  ReadBirdsData;
  AssignFile(F, PathS + 'BIRDDATA.TXT');
  Rewrite(F);
  writeln(F, ' ');
  writeln(F, 'Birds Data');
  writeln(F, ' ');
  BirdString := 'BirdName  ' + CurrentBirdName;
  writeln(F, BirdString);
  BirdString := 'Bird Body File  ' + CurrentBirdBodyFile;
  writeln(F, BirdString);
  BirdString := 'Bird Texture file  ' + CurrentBirdTexture;
  writeln(F, BirdString);
  str(dBirdsScaleDX: 6: 3, DataString);
  BirdString := 'Bird Scale  ' + DataString;
  writeln(F, BirdString);
  str(BirdsTailDX, DataString);
  BirdString := 'Bird Size  ' + DataString;
  writeln(F, BirdString);
  str(BirdsDX, DataString);
  BirdString := 'Number of Birds  ' + DataString;
  writeln(F, BirdString);
{        str(SongEdit.Itemindex,DataString); }
  BirdString := 'BirdSong  ' + CurrentBirdSong;
  writeln(F, BirdString);

  str(dBirdsFeedDX: 6: 3, DataString);
  BirdString := ' Feed   ' + DataString;
  writeln(F, BirdString);
  str(BirdsActivityDX, DataString);
  BirdString := ' Activity  ' + DataString;
  writeln(F, BirdString);
  str(BirdsColorDX, DataString);
  BirdString := 'Body Color  ' + DataString;
  writeln(F, BirdString);
  str(BirdWingColorDX, DataString);
  BirdString := ' Wing Color  ' + DataString;
  writeln(F, BirdString);
  writeln(F, ' ');
  writeln(F, ' ');
  str(BirdsSpeedDX, DataString);
  BirdString := ' Speed  ' + DataString;
  writeln(F, BirdString);
  str(dBirdsMinVDX: 6: 3, DataString);
  BirdString := ' Min Velocity  ' + DataString;
  writeln(F, BirdString);
  str(RCopyDX, DataString);
  BirdString := ' Copy Radius  ' + DataString;
  writeln(F, BirdString);
  str(dWCopyDX: 6: 3, DataString);
  BirdString := ' Copy Weight  ' + DataString;
  writeln(F, BirdString);
  writeln(F, ' ');
  str(dBirdsGlideDX: 6: 3, DataString);
  BirdString := 'Bird Glide  ' + DataString;
  writeln(F, BirdString);
  str(dBirdsMomentDX: 6: 3, DataString);
  BirdString := ' Turn Moment Velocity  ' + DataString;
  writeln(F, BirdString);
  str(RCentroidDX, DataString);
  BirdString := ' Centroid Radius  ' + DataString;
  writeln(F, BirdString);
  str(dWCentroidDX: 6: 3, DataString);
  BirdString := ' Centroid Weight  ' + DataString;
  writeln(F, BirdString);
  writeln(F, ' ');
  str(BirdsFerocityDX, DataString);
  BirdString := 'Bird Ferocity  ' + DataString;
  writeln(F, BirdString);
  str(dBirdsViewDX: 8: 3, DataString);
  BirdString := ' View Degrees  ' + DataString;
  writeln(F, BirdString);
  str(RVisualDX, DataString);
  BirdString := ' Visual Radius  ' + DataString;
  writeln(F, BirdString);
  str(dWVisualDX: 6: 3, DataString);
  BirdString := ' Visual Weight  ' + DataString;
  writeln(F, BirdString);
  writeln(F, ' ');
  str(BirdsTenacityDX, DataString);
  BirdString := 'Bird Tenacity  ' + DataString;
  writeln(F, BirdString);
  str(dBirdsAvoidDX: 8: 3, DataString);
  BirdString := ' Avoid Degrees  ' + DataString;
  writeln(F, BirdString);
  str(RAvoidDX, DataString);
  BirdString := ' Avoid Radius  ' + DataString;
  writeln(F, BirdString);
  str(dWAvoidDX: 6: 3, DataString);
  BirdString := ' Avoid Weight  ' + DataString;
  writeln(F, BirdString);
  writeln(F, ' ');
  CloseFile(F);
  ExecuteFile('BIRDDATA.TXT', '', PathS, SW_SHOW);
end;


procedure TBirdDXForm.AttackBtnClick(Sender: TObject);
begin {Send in the Clowns}
  if (not BirdLandDX) then begin
    ChaseIt := (not ChaseIt);
    if ChaseIt then AttackBtn.Color := WingColorPanel.Color
    else AttackBtn.Color := clBtnFace;
  end;
end;

procedure TBirdDXForm.DoFranticBtnClick(Sender: TObject);
var I: Integer;
begin
  {Send Frantic signal... set Avoid to max}
  if (not BirdLandDX) then
  begin
    if (DoBirdseye or DoMazepole) then
    begin
      for I := 0 to BirdsDX - 1 do
      begin
        BirdsDXP[I] := BirdsDXP[I] + Random(3) - 1;
        BirdsDYP[I] := BirdsDYP[I] + Random(3) - 1;
        BirdsDXV[I] := Random(3) - 1;
        BirdsDYV[I] := Random(3) - 1;
        NormDX(BirdsDXV[I], BirdsDYV[I]);
        BirdsDXV[I] := dBirdxTempDX;
        BirdsDYV[I] := dBirdyTempDX;
      end;
    end;
  end;
end;

procedure TBirdDXForm.BirdSongBtnClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Bird Song Files';
  OpenDialog1.Filter := 'Bird Songs (*.wav)|*.wav';
  OpenDialog1.Filename := SongEdit.Text;
  OpenDialog1.InitialDir := BirdLifeDir;
  if OpenDialog1.Execute then begin
    if ((FileExists(OpenDialog1.Filename))
      and (lowercase(ExtractFileExt(OpenDialog1.Filename)) = '.wav'))
      then
    begin
      SongEdit.Text := ExtractFileName(OpenDialog1.Filename);
      {Place into record OpenDialog1.Filename}
      CurrentBirdSong := SongEdit.Text;
    end;
  end;
end;



procedure TBirdDXForm.ExitBtnClick(Sender: TObject);
begin
Close;
end;



end.
