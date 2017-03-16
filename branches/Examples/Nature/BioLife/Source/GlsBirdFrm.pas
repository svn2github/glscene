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

unit GlsBirdFrm;
 {bird-listing1.txt for making Flocks-Groups}
{Make a LANDSCAPE for the birds to fly around in}
{Todo make an actual Ground Heightmap... for Doves, Robins,...}
{Tree instead of a Cube to Feed}  {Multiple feeders}
{Landing sites - Trees to Nest in - Birds go to make Birdetts}
{Feeding Length: Time at feeder: need a variable}
{3D code? Z..Height level AI}
{Get it Flocking like the 2D version does}
{3D .3ds files} {Birds 3ds file by Dave Kerr : AI Planet}
{smd files? Fly-flap-Glide, Land, Walk,... Peck-Attack}
{Color code}  {Names of Bird body and color are kinda reversed}
{Frenzy Flocks interaction}
interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI,
  System.SysUtils, System.Classes, System.Math,
  Vcl.Buttons, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus,  Vcl.ComCtrls,
   
  GLFile3DS, GLScene, GLObjects, GLSkydome,GLTexture,
  GLWin32Viewer, GLCadencer, GLVectorFileObjects, GLCoordinates,
  GLCrossPlatform, GLBaseClasses;

const
  dDegToRad: Double = 0.01745329251994329547; // Degrees to Radians
  dRadToDeg: Double = 57.29577951308232286465; // Radians to Degrees

type
  TAAABirdForm = class(TForm)
    BirdPanel: TPanel;
    FeedLabel: TLabel;
    SizeLabel: TLabel;
    SpeedLabel: TLabel;
    RandomosityLabel: TLabel;
    GlideLabel: TLabel;
    ActivityLabel: TLabel;
    FerocityLabel: TLabel;
    DefendLabel: TLabel;
    AvoidLabel: TLabel;
    ViewLabel: TLabel;
    TurnLabel: TLabel;
    HoverLabel: TLabel;
    Label3: TLabel;
    Label11: TLabel;
    BirdsEdit: TEdit;
    dBirdsFeedTB: TTrackBar;
    BirdsSizeTB: TTrackBar;
    BirdsColorPanel: TPanel;
    BirdsSpeedTB: TTrackBar;
    BirdsRandomosityTB: TTrackBar;
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
    BirdsFrenzyBtn: TSpeedButton;
    HelpBtn: TSpeedButton;
    ColorDialog1: TColorDialog;
    PrintitBtn: TSpeedButton;
    DoFranticBtn: TSpeedButton;
    OpenDialog1: TOpenDialog;
    BirdFileBtn: TSpeedButton;
    BirdSongPanel: TPanel;
    BirdsBtn: TSpeedButton;
    WingColorPanel: TPanel;
    BirdsNameEdit: TEdit;
    SaveDialog1: TSaveDialog;
    FPSCB: TCheckBox;
    CopyLabel: TLabel;
    CenterLabel: TLabel;
    VDistanceLabel: TLabel;
    ADistanceLabel: TLabel;
    AttackBtn: TPanel;
    dWCopyTBLabel: TLabel;
    dWCentroidTBLabel: TLabel;
    dWVisualTBLabel: TLabel;
    dWAvoidTBLabel: TLabel;
    BirdSongEdit: TEdit;
    BirdSongBtn: TSpeedButton;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLSkyDome1: TGLSkyDome;
    GLDummyCube1: TGLDummyCube;
    LittleRed: TGLSphere;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLBirdBody: TGLFreeForm;
    StatusBar1: TStatusBar;
    DistanceBar: TTrackBar;
    Label1: TLabel;
    dBirdsSexyTB: TTrackBar;
    SexyLabel: TLabel;
    FlockEdit: TEdit;
    BirdsSaveBtn: TSpeedButton;
    CancelFlightBtn: TSpeedButton;
    FlyBirdsBtn: TSpeedButton;
    ClearBtn: TSpeedButton;
    Feeder: TGLCube;
    ExitBtn: TSpeedButton;
    CadencerTB: TTrackBar;
    GLHunterBody: TGLFreeForm;
    BirdTextureBtn: TSpeedButton;
    BirdTextureEdit: TEdit;
    BirdBodyBtn: TSpeedButton;
    BirdBodyEdit: TEdit;
    dBirdsEnergyTB: TTrackBar;
    EnergyLabel: TLabel;
    AxisCB: TCheckBox;
    BirdDirectorCB: TCheckBox;
    DisplayDemoBtn: TSpeedButton;
    FlyingAreaSizeTB: TTrackBar;
    AboutBtn: TSpeedButton;
    FlockUpBtn: TSpeedButton;
    FlockDownBtn: TSpeedButton;
    dBirdsFlockTB: TTrackBar;
    dBirdsFlockLabel: TLabel;
    Label2: TLabel;
    DisplayObstaclesCB: TCheckBox;
    BirdFollowerEdit: TEdit;
    BirdFollowerUpBtn: TSpeedButton;
    BirdFollowerDownBtn: TSpeedButton;
    BirdFollowerCB: TCheckBox;
    Bird2dCB: TCheckBox;
    procedure FormCreate(Sender: TObject);
procedure ShowHint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FlyingAreaSizeTBChange(Sender: TObject);
    procedure AxisCBClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
                          Shift: TShiftState);
procedure ReallyClose;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    
    procedure DisplayDemoBtnClick(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);

    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject;
      Shift: TShiftState;  X, Y: Integer);
    procedure DistanceBarChange(Sender: TObject);

    procedure FlockUpBtnClick(Sender: TObject);
    procedure FlockDownBtnClick(Sender: TObject);
    procedure FlockUpBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FlockDownBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure CadencerTBChange(Sender: TObject);

    procedure BirdsFrenzyBtnClick(Sender: TObject);
    procedure FrenzyFilesArray;
    procedure AttackBtnClick(Sender: TObject);
    procedure DoFranticBtnClick(Sender: TObject);

    procedure BirdSongClick(Sender: TObject);
    procedure BirdSongBtnClick(Sender: TObject);
    procedure BirdTextureBtnClick(Sender: TObject);
    procedure BirdBodyBtnClick(Sender: TObject);
    procedure BirdsColorPanelClick(Sender: TObject);
    procedure WingColorPanelClick(Sender: TObject);
procedure ColorTheBirds;

    {All the TBs .. Not in order}
    procedure dBirdsSexyTBChange(Sender: TObject);
    procedure dBirdsEnergyTBChange(Sender: TObject);
    procedure dBirdsFeedTBChange(Sender: TObject);
    procedure BirdsSizeTBChange(Sender: TObject);
    procedure BirdsSpeedTBChange(Sender: TObject);
    procedure BirdsRandomosityTBChange(Sender: TObject);
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

    procedure BirdFileBtnClick(Sender: TObject);
    procedure BirdFileOpen(BirdFileName: string);
    procedure BirdsSaveBtnClick(Sender: TObject);
    procedure PrintitBtnClick(Sender: TObject);
    procedure SaveBirdsFile(InName:String; Which:Boolean);

    procedure ClearBtnClick(Sender: TObject);
    procedure SetBirdsDefault;
procedure SetBirdsData;
procedure GetBirdsData;
    procedure BirdsBtnClick(Sender: TObject);
procedure BirdsMaker(HowMany:Integer);

    procedure MakeOffsetSquares;
    procedure CancelFlightBtnClick(Sender: TObject);
    procedure FlyBirdsBtnClick(Sender: TObject);
procedure BirdStartup;
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
              const deltaTime, newTime: Double);
procedure DXComputeNewHeading(WhichBoid: Integer; deltaTime:Double);
    procedure BoidFeed(WhichBoid: Integer);
    procedure BoidCheckCollision(WhichBoid: Integer);
    procedure BoidSeek;
    procedure dBirdsFlockTBChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BirdFollowerUpBtnClick(Sender: TObject);
    procedure BirdFollowerDownBtnClick(Sender: TObject);
    procedure BirdFollowerCBClick(Sender: TObject);
    procedure BirdFollowerUpBtnMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BirdFollowerDownBtnMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Bird2dCBClick(Sender: TObject);

  private
     
  Mmx,Mmy,Mmx2,Mmy2:integer;{Mouse-Camera Movements}
  public
     
  FlyingAreaDivisor:Integer;
procedure Codefx(WhatString: string; Codein: Integer);
function ExecuteFile(const FileName, Params, DefaultDir: string;
                     ShowCmd: Integer): THandle;
    function Leni(x, y: Integer): Integer;
    function Lend(x, y: Double): Double;
    function Disti(x1, y1, x2, y2: Integer): Integer;
    function Distd(x1, y1, x2, y2: Double): Double;
    function Doti(x1, y1, x2, y2: Integer): Integer;
    function Dotd(x1, y1, x2, y2: Double): Double;
    procedure NormDX(x, y: Double);
    function Lend3(x, y, z: Double): Double;
    function Distd3(x1, y1, z1, x2, y2, z2: Double): Double;
    function Dotd3(x1, y1, z1, x2, y2, z2: Double): Double;
    procedure NormDX3(x, y, z: Double);
  end;

var
  AAABirdForm: TAAABirdForm;
/////////////////////////////////////////////////////
type
  Vec = record
    x, y, z: double;
  end;

  var
  ObstacleCount: Integer;{Meaningles for now - there are 8}
  ObstacleArray: array of Vec;{Used to locate for Collision}
  ObstacleProxies: Array of TGLSphere;{Balls of Fire?}

{Currently 1 Dummycube centered at 0,0}
{  FeederCount: Integer;
  Feeder:Vec;
  FeederProxies:Array of TGLSphere;}
{  FeederArray: array of Vec;}
Type
  HunterBoid = record
    pos, vel: Vec;
    BirdWingColor, BirdColor: TColor;
    BirdSpeed, BirdSize: Integer;
    BirdBody:TGLFreeform;
    BirdTexture:String;
  end;
  var Hunter: HunterBoid;

type
  GlsBoidRecord = record  {EACH Bird has this data}
    pos, {dir,} vel, posn: Vec;
    BirdsTenacity,{Randomized per bird adjusts-Reduces Avoid Distance}
    BirdsFerocity,{Randomized per bird adjusts-Reduces View  Distance}
    HungryTimer: Integer;{How long it sits and eats}
    {Sexy, Dave does it beter by mating birds Sexy=Male or Female}
    Hunted,  {Always True until dead}
    Hungry: boolean;  {Eating - Not flying}
    BirdsActivity,{Flock integer : now a double decremated by dBirdsFeed}
    dBirdsSexy,{Sex increment}{Sex Total}
{    dBirdsEnergy,}
    dBirdsFeed: Double; {Decrement BirdsActivity}
  end;

  BirdBrainRecord = record  {Data same for ALL in the FLOCK}
   {All files expected to be in Programs directory}
    BirdName: string[255];
    BirdSong, BirdBodyFile, BirdTexture: string[255];
    BirdsRandomosity: Integer;
    BirdsCount : Integer;
    BirdsSize  : Integer;
    dBirdsFeed,
    dBirdsSexy: Double;
    BirdsWingColor,  BirdsColor: TColor;
    BirdsActivity: Integer;
    dBirdsEnergy: Double;{used for sex capabilities}
    dBirdsSpeed,
      dBirdsMinV {minv}: Double;
      RCopy {rcopy}: Integer;
      dWCopy {wcopy}: Double;
    dBirdsGlide: Double;
      dBirdsMoment {ddt}: Double;
      RCentroid {rcent}: Integer;
      dWCentroid {wcent}: Double;
    BirdsFerocity: Integer;
      dBirdsView {angle}: Double;
      RVisual {rviso}: Integer;
      dWVisual {wviso}: Double;
    BirdsTenacity: Integer;
      dBirdsAvoid {vangle}: Double;
      RAvoid {rvoid}: Integer;
      dWAvoid {wvoid}: Double;
    dBirdsFlock: Double;
    RealCenter, RealAvgvel: Vec;
  end;

var
  {boidArray[Flock, aBird]}
  BirdBrainArray: array of BirdBrainRecord;  {each Flock}
  BoidArray: array of array of GlsBoidRecord;{Flocks of Birds data}
  BirdProxies:Array of array of TGLFreeform;
Feeding: Vec;

  FirstTime,
  ChaseIt, BirdSinging,
  BirdLanded,  NotYetShown, ReallyGone: Boolean;
  {LifeDir,}{FrenzyFilesName: string;}
  BoidFeeding,
  CurrentSeekerK, CurrentSeekerJ,
  BirdYardWide, BirdYardHeight, BirdsYardDepth,
  CurrentBirdFollowed,
  FrenzyCount,  CurrentFrenzy, WorkingFrenzy,
  FrenzyBirdCount, CrashCount: Integer;

  dBirdxTempDX,dBirdyTempDX, dBirdzTempDX:Double;{Temp twisters}
  TerrainBuffer: array of array of Double;

  MMSysHandle: THandle;
  PlaySound: function(lpszSoundName: PAnsiChar; uFlags: UINT): BOOL;
    stdcall;
/////////////////////////////////////////////////////
implementation
uses GlsFrenzy, GLSDemo, AllSplash, GlsAbout,nUGlobal, nLifeMain;

{$R *.DFM}

/////////////////////////////////////////////////////
{the other stuff here}
procedure TAAABirdForm.Codefx(WhatString: string; Codein: Integer);
var CodeS: string;
begin
  str(Codein, CodeS);
  ShowMessage('Error in data Number: ' + #13#10 +
    CodeS + #13#10 +
    WhatString);
end;
function TAAABirdForm.ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
var
  zFileName, zParams, zDir: array[0..79] of Char;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil,
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
end;
/////////////////////////////////////////////////////

function TAAABirdForm.Leni(x, y: Integer): Integer;
begin
  Leni := round(sqrt(SQR(x) + SQR(y)));
end;

function TAAABirdForm.Lend(x, y: Double): Double;
begin
  Lend := (sqrt(SQR(x) + SQR(y)));
end;

function TAAABirdForm.Disti(x1, y1, x2, y2: Integer): Integer;
begin
  Disti := LENi(((x1) - (x2)), ((y1) - (y2)));
end;

function TAAABirdForm.Distd(x1, y1, x2, y2: Double): Double;
begin
  Distd := Lend(((x1) - (x2)), ((y1) - (y2)));
end;

function TAAABirdForm.Doti(x1, y1, x2, y2: Integer): Integer;
begin
  Doti := ((x1) * (x2) + (y1) * (y2))
end;

function TAAABirdForm.Dotd(x1, y1, x2, y2: Double): Double;
begin
  Dotd := ((x1 * x2) + (y1 * y2))
end;
// Destructively normalize a vector.

procedure TAAABirdForm.NormDX(x, y: Double);
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
{Lend:=(sqrt(SQR((x1)-(x2))+ SQR((z1)-(z2)) + SQR((y1)-(y2))));}
function TAAABirdForm.Lend3(x, y, z: Double): Double;
begin
  Lend3 := (sqrt(SQR(x) + SQR(y) + SQR(z)));
end;
function TAAABirdForm.Distd3(x1, y1, z1, x2, y2, z2: Double): Double;
begin
  Distd3 := Lend3(((x1) - (x2)), ((y1) - (y2)), ((z1) - (z2)));
end;
function TAAABirdForm.Dotd3(x1, y1, z1, x2, y2, z2: Double): Double;
begin
  Dotd3 := ((x1 * x2) + (y1 * y2) + (z1 * z2))
end;
procedure TAAABirdForm.NormDX3(x, y, z: Double);
var Len: Double;
begin
  Len := LENd3(x, y, z);
  if (len <> 0.0) then
  begin
    dBirdxTempDX := (x / len);
    dBirdyTempDX := (y / len);
    dBirdzTempDX := (z / len);
  end else
  begin
    dBirdxTempDX := x;
    dBirdyTempDX := y;
    dBirdzTempDX := z;
  end;
end;

/////////////////////////////////////////////////////

procedure TAAABirdForm.FormCreate(Sender: TObject);
begin
  top := BirdsFormY;
  left := BirdsFormX;
  randomize;
  If (not FileExists(ExtractFilePath(ParamStr(0))+'Glsboid.3ds'))
  then
  begin
    showmessage('Glsboid.3ds file is missing');
    Application.Terminate;
  end;
  FlyingAreaDivisor:=10;
  FrenzyFilesLoaded:= False;
  {Get App Location to load Help}
{  Application.HelpFile := ExtractFilePath(ParamStr(0))+ 'Glsboid.hlp';
  Application.OnHint := ShowHint;  }

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
    ShowMessage('Cannot locate WINMM.DLL or MMSYSTEM.DLL.. no sounds');
  FirstTime:=True;
  ReallyGone:=False;
  SetBirdsDefault;
end;

procedure TAAABirdForm.FormShow(Sender: TObject);
begin
  Application.OnHint := AAABirdForm.ShowHint;
  Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'Glsboid.hlp';
end;
procedure TAAABirdForm.ShowHint(Sender: TObject);
begin
  StatusBar1.Panels[2].Text := Application.Hint;
end;
procedure TAAABirdForm.FormResize(Sender: TObject);
begin
  BirdYardWide := GLSceneViewer1.Width div FlyingAreaDivisor;
  BirdYardHeight := GLSceneViewer1.Height div FlyingAreaDivisor;
end;
procedure TAAABirdForm.FlyingAreaSizeTBChange(Sender: TObject);
begin
  FlyingAreaDivisor:=FlyingAreaSizeTB.Position;
  BirdYardWide := GLSceneViewer1.Width div FlyingAreaDivisor;
  BirdYardHeight := GLSceneViewer1.Height div FlyingAreaDivisor;
end;
procedure TAAABirdForm.AxisCBClick(Sender: TObject);
begin
  GLDummyCube1.ShowAxes:=AxisCB.Checked;
end;
procedure TAAABirdForm.Bird2dCBClick(Sender: TObject);
begin
    If Bird2dCB.Checked then BirdYardHeight:=1
    else BirdYardHeight:= {(BirdYardHeight+20)  * -1;}
      GLSceneViewer1.Height div FlyingAreaDivisor;
end;
procedure TAAABirdForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(8000);
end;

procedure TAAABirdForm.ExitBtnClick(Sender: TObject);
begin
  CancelFlightBtnClick(Sender);
  Close;
end;

procedure TAAABirdForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin  {  Application end  }
  if Key = VK_ESCAPE then Close;
end;

procedure TAAABirdForm.ReallyClose;
begin
  Timer1.Enabled := False;
  GLCadencer1.Enabled := False;
  BirdLanded := True;
  Application.ProcessMessages;
  ReallyGone:=True;
  FrenzyForm.ReallyClose;
  AAADemoForm.Close;
  SetLength(BirdBrainArray, 0);
  SetLength(BoidArray, 0);
  SetLength(ObstacleArray, 0);
  SetLength(BirdProxies, 0);
  GLDummyCube1.DeleteChildren;
  Application.ProcessMessages;
  AAABirdForm.Close;
end;
procedure TAAABirdForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
begin
  Application.OnHint := LifeMainForm.ShowHint;
  Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'Nlife.hlp';
  Timer1.Enabled := False;
  GLCadencer1.Enabled := False;
  BirdLanded := True;
  BirdsFormY := AAABirdForm.top;
  BirdsFormX := AAABirdForm.left;
  if ReallyGone then Action := caFree else Action := caHide;
end;
/////////////////////////////////////////////////////
procedure TAAABirdForm.DisplayDemoBtnClick(Sender: TObject);
begin
  CancelFlightBtnClick(Sender);
  AAADemoForm.Show;
end;
procedure TAAABirdForm.AboutBtnClick(Sender: TObject);
begin
   AboutBoids.Show;
end;
/////////////////////////////////////////////////////
procedure TAAABirdForm.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   Mmx:=x; Mmy:=y;
   Mmx2:=x; Mmy2:=y;
end;
procedure TAAABirdForm.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
   begin
      Mmx2:=x;
      Mmy2:=y;
     if ((Mmx<>Mmx2)or(Mmy<>Mmy2)) then
     begin
       GLCamera1.MoveAroundTarget(Mmy-Mmy2, Mmx-Mmx2);
      //move the camera around the target if mouse was dragged
       Mmx:=Mmx2; Mmy:=Mmy2;
     end;
   end;
end;
procedure TAAABirdForm.DistanceBarChange(Sender: TObject);
var
   Dist, NewDist,cx,cy,cz :single;
begin
   Dist:=GLCamera1.DistanceToTarget;
   cx:=GLCamera1.Position.x;
   cy:=GLCamera1.Position.y;
   cz:=GLCamera1.Position.z;
   NewDist:=DistanceBar.position;
   GLCamera1.Position.x:=cx/dist*NewDist;
   GLCamera1.Position.y:=cy/dist*NewDist;
   GLCamera1.Position.z:=cz/dist*NewDist;
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
procedure TAAABirdForm.FlockUpBtnClick(Sender: TObject);
begin          {0}        {2-1=1}
  If (CurrentFrenzy<(FrenzyCount-1))then
    inc(CurrentFrenzy)
    else CurrentFrenzy:=0;
    FlockEdit.Text:= inttostr(CurrentFrenzy);
    SetBirdsData;{displays data}
  If BirdFollowerCB.Checked then
  begin
    CurrentBirdFollowed:=0;
    BirdFollowerEdit.Text:= inttostr(CurrentBirdFollowed);
    GLCamera1.TargetObject:= BirdProxies[CurrentFrenzy,CurrentBirdFollowed];
  end;
  If (not BirdLanded)then
  begin
  Timer1.Enabled := True;
  GLCadencer1.Enabled := True;
  end;
end;
procedure TAAABirdForm.FlockUpBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Timer1.Enabled := False;
  GLCadencer1.Enabled := False;
end;
procedure TAAABirdForm.FlockDownBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Timer1.Enabled := False;
  GLCadencer1.Enabled := False;
end;
procedure TAAABirdForm.FlockDownBtnClick(Sender: TObject);
begin
  If (CurrentFrenzy>0)then
    dec(CurrentFrenzy)
    else CurrentFrenzy:=FrenzyCount-1;
    FlockEdit.Text:= inttostr(CurrentFrenzy);
    SetBirdsData;{displays data}
  If BirdFollowerCB.Checked then
  begin
    CurrentBirdFollowed:=0;
    BirdFollowerEdit.Text:= inttostr(CurrentBirdFollowed);
    GLCamera1.TargetObject:= BirdProxies[CurrentFrenzy,CurrentBirdFollowed];
  end;
  If (not BirdLanded)then
  begin
  Timer1.Enabled := True;
  GLCadencer1.Enabled := True;
  end;
end;

/////////////////////////////////////////////////////
procedure TAAABirdForm.BirdFollowerCBClick(Sender: TObject);
begin
  If BirdFollowerCB.Checked then
  begin
    GLCamera1.TargetObject:= BirdProxies[CurrentFrenzy,CurrentBirdFollowed];
  end else
  begin
    CurrentBirdFollowed:=0;
    BirdFollowerEdit.Text:= inttostr(CurrentBirdFollowed);
    GLCamera1.TargetObject:= GLDummyCube1;
  end;
end;


procedure TAAABirdForm.BirdFollowerUpBtnClick(Sender: TObject);
begin        {0}        {2-1=1}
  If BirdFollowerCB.Checked then
  begin
  If ((CurrentBirdFollowed<(BirdBrainArray[CurrentFrenzy].BirdsCount-1))
      and (CurrentBirdFollowed<CurrentSeekerJ))then
    inc(CurrentBirdFollowed)else
    CurrentBirdFollowed:=0;
    BirdFollowerEdit.Text:= inttostr(CurrentBirdFollowed);
    GLCamera1.TargetObject:= BirdProxies[CurrentFrenzy,CurrentBirdFollowed];
  If (not BirdLanded)then
  begin
  Timer1.Enabled := True;
  GLCadencer1.Enabled := True;
  end;
  end;
end;
procedure TAAABirdForm.BirdFollowerUpBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Timer1.Enabled := False;
  GLCadencer1.Enabled := False;
end;

procedure TAAABirdForm.BirdFollowerDownBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Timer1.Enabled := False;
  GLCadencer1.Enabled := False;
end;

procedure TAAABirdForm.BirdFollowerDownBtnClick(Sender: TObject);
begin
  If BirdFollowerCB.Checked then
  begin
  If (CurrentBirdFollowed>0)then
    dec(CurrentBirdFollowed)
    else if (CurrentSeekerJ=BirdBrainArray[CurrentFrenzy].BirdsCount-1)then
    CurrentBirdFollowed:=BirdBrainArray[CurrentFrenzy].BirdsCount-1
    else CurrentBirdFollowed:=CurrentSeekerJ;
    BirdFollowerEdit.Text:= inttostr(CurrentBirdFollowed);
    GLCamera1.TargetObject:= BirdProxies[CurrentFrenzy,CurrentBirdFollowed];
  {end;}
  If (not BirdLanded)then
  begin
  Timer1.Enabled := True;
  GLCadencer1.Enabled := True;
  end;
  end;
end;
/////////////////////////////////////////////////////

procedure TAAABirdForm.CadencerTBChange(Sender: TObject);
begin
  GLCadencer1.Sleeplength := CadencerTB.Position;
  {(CadencerTB.Max - CadencerTB.Position)-1;}
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
procedure TAAABirdForm.BirdsFrenzyBtnClick(Sender: TObject);
begin
  CancelFlightBtnClick(Sender);
  FrenzyForm.Show;
end;

procedure TAAABirdForm.FrenzyFilesArray;
var
  j, i: Integer;
  FrenzyStr: string;
begin
  FrenzyFilesLoaded := False;
  if (FrenzyForm.FrenzyLB.Items.Count > 0) then
  begin
{    FrenzyCount := 0;}
    FrenzyBirdCount := 0;
    ChDir(BirdLifeDir);
    j := 0;
    for i := 0 to (FrenzyForm.FrenzyLB.Items.Count - 1) do
    begin
      FrenzyStr := FrenzyForm.FrenzyLB.Items.Strings[i];
      if FileExists(FrenzyStr) then
      begin
        {SetLength(BirdBrainArray, (FrenzyCount + 1));
        SetLength(BoidArray, (FrenzyCount + 1));
        CurrentFrenzy := FrenzyCount;}
        {showmessage(FrenzyStr);}
        BirdFileOpen(FrenzyStr);  {inc(FrenzyCount);}
      end else showmessage(FrenzyStr + ' not found');
    end;
    for i := 0 to (FrenzyCount - 1) do begin
      {SetLength(BoidArray[(i)], BirdBrainArray[i].BirdsCount);}
      {Make Boidarray birds correct}
      j := j + BirdBrainArray[i].BirdsCount;
      FrenzyBirdCount := (FrenzyBirdCount +BirdBrainArray[i].BirdsCount);
    end;
    FrenzyFilesLoaded := True;
    AAABirdForm.Caption := Format('There are now %d birds in the pi',[j]);
    {GetBirdsData;} {Reset to prior}
  end else showmessage('Please load a Frenzy file first');
end;

procedure TAAABirdForm.AttackBtnClick(Sender: TObject);
var K,J:Integer;
begin {Send in the Clowns}
  if (not BirdLanded) then
  begin
  CurrentSeekerK := -1;
  CurrentSeekerJ := -1;
  for K := 0 to FrenzyCount - 1 do begin
    for J := 0 to (BirdBrainArray[K].BirdsCount - 1) do begin
      if (boidArray[K, J].Hunted = true) then begin
        CurrentSeekerK := K;
        CurrentSeekerJ := J;
      end;
    end;
  end;
    ChaseIt := (not ChaseIt);
    if ChaseIt then AttackBtn.Color := WingColorPanel.Color
    else AttackBtn.Color := clBtnFace;
    Hunter.BirdBody.Visible:=ChaseIt;
    StatusBar1.Panels[0].Text :=
   inttostr(CurrentSeekerK)+': '+ inttostr(CurrentSeekerJ)
  end;
end;

procedure TAAABirdForm.DoFranticBtnClick(Sender: TObject);
var
  K,I: Integer;
begin {Send Frantic signal... set Avoid to max}
  if (not BirdLanded) then
  begin
    for k := 0 to FrenzyCount - 1 do
    begin
      CurrentFrenzy := K;
      FrenzyBirdCount := FrenzyBirdCount+ BirdBrainArray[k].BirdsCount;
      for i := 0 to BirdBrainArray[k].BirdsCount - 1 do
      begin
        {X , Y ,Z can be negatives around screen center}
        boidArray[k, i].pos.x := Random(BirdYardWide ) - Random(BirdYardWide);
        boidArray[k, i].pos.y := Random(BirdYardHeight ) - Random(BirdYardHeight);
        boidArray[k, i].pos.z := Random(BirdYardWide ) - Random(BirdYardWide);
        boidArray[k, i].vel.x := Random(51) - 25; {Negative means flying left...}
        boidArray[k, i].vel.y := Random(51) - 25; {up.. down}
        boidArray[k, i].vel.z := Random(51) - 25; {front to back}
      end;
    end;
  end;
end;

{SONG STUFF}
procedure TAAABirdForm.BirdSongClick(Sender: TObject);
var
  s: array[0..245] of AnsiChar;
begin
  {If true then sing and change btn color}
  BirdSinging := (not BirdSinging);
  if BirdSinging then
  begin
    if ((FileExists(BirdLifeDir + BirdBrainArray[CurrentFrenzy].BirdSong))
      and (lowercase(ExtractFileExt(BirdBrainArray[CurrentFrenzy].BirdSong)) = '.wav'))
      then
    begin
      BirdSongPanel.Color := BirdsColorPanel.Color;
      BirdSinging := True;
      StrPCopy(s, BirdLifeDir + BirdBrainArray[CurrentFrenzy].BirdSong);
        {sndPlaySound(s, 0);}
      if (MMSysHandle <> 0) then PlaySound(s, {SND_ASYNC} 0);
    end else
    begin
      BirdSongPanel.Color := clBtnFace;
      BirdSinging := False;
      ShowMessage('BirdSong File Does NOT Exist!'
        + #13#10 + BirdLifeDir + BirdBrainArray[CurrentFrenzy].BirdSong);
    end;
  end else
  begin
    BirdSongPanel.Color := clBtnFace;
    BirdSinging := False;
  end;
end;

procedure TAAABirdForm.BirdSongBtnClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Bird Song Files';
  OpenDialog1.Filter := 'Bird Songs (*.wav)|*.wav';
  OpenDialog1.Filename := BirdSongEdit.Text;
  OpenDialog1.InitialDir := BirdLifeDir;
  if OpenDialog1.Execute then begin
    if ((FileExists(OpenDialog1.Filename))
      and (lowercase(ExtractFileExt(OpenDialog1.Filename)) = '.wav'))
      then
    begin
      BirdSongEdit.Text := ExtractFileName(OpenDialog1.Filename);
      {Place into record OpenDialog1.Filename}
      BirdBrainArray[CurrentFrenzy].BirdSong := BirdSongEdit.Text;
    end;
  end;
end;

procedure TAAABirdForm.BirdTextureBtnClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Bird Texture Files';
  OpenDialog1.Filter := 'Bird Texture (*.bmp)|*.bmp';
  OpenDialog1.Filename := BirdTextureEdit.Text;
  OpenDialog1.InitialDir := BirdLifeDir;
  if OpenDialog1.Execute then begin
    if ((FileExists(OpenDialog1.Filename))
      and (lowercase(ExtractFileExt(OpenDialog1.Filename)) = '.bmp'))
      then
    begin
      BirdTextureEdit.Text := ExtractFileName(OpenDialog1.Filename);
      {Place into record OpenDialog1.Filename}
      BirdBrainArray[CurrentFrenzy].BirdTexture := BirdTextureEdit.Text;
    end;
  end;
end;

procedure TAAABirdForm.BirdBodyBtnClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Bird Body Files';
  OpenDialog1.Filter := 'Bird Body (*.3ds)|*.3ds';
  OpenDialog1.Filename := BirdBodyEdit.Text;
  OpenDialog1.InitialDir := BirdLifeDir;
  if OpenDialog1.Execute then begin
    if ((FileExists(OpenDialog1.Filename))
      and (lowercase(ExtractFileExt(OpenDialog1.Filename)) = '.3ds'))
      then
    begin
      BirdBodyEdit.Text := ExtractFileName(OpenDialog1.Filename);
      {Place into record OpenDialog1.Filename}
      BirdBrainArray[CurrentFrenzy].BirdBodyFile := BirdBodyEdit.Text;
    end;
  end;
end;

(*
procedure TAAABirdForm.USEtheUnderBodyColorCBClick(Sender: TObject);
var k,i:Integer;
begin
  for k := 0 to FrenzyCount - 1 do
  begin
    for I := 0 to BirdBrainArray[k].BirdsCount- 1 do
    begin
      with BirdProxies[k,i] do
      begin
        If USEtheUnderBodyColorCB.Checked then
         {GLBirdBody.}Material.{BackProperties.}FaceCulling:=fcNoCull
         else {GLBirdBody.}Material.{BackProperties.}FaceCulling:=fcBufferDefault;
        TransformationChanged;
      end;
    end;
  end;
end; *)
procedure TAAABirdForm.BirdsColorPanelClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    BirdsColorPanel.Color := ColorDialog1.Color;
    BirdBrainArray[CurrentFrenzy].BirdsColor := BirdsColorPanel.Color;
    ColorTheBirds;
  end;
end;

procedure TAAABirdForm.WingColorPanelClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    WingColorPanel.Color := ColorDialog1.Color;
    BirdBrainArray[CurrentFrenzy].BirdsWingColor := WingColorPanel.Color;
    ColorTheBirds;
  end;
end;

procedure TAAABirdForm.ColorTheBirds;
var i:Integer;
begin
    for i:=0 to BirdBrainArray[CurrentFrenzy].BirdsCount-1 do
    begin
      with BirdProxies[CurrentFrenzy, i] do
      begin
{        Material.FrontProperties.Ambient:=AsWinColor(BirdBrainArray[CurrentFrenzy].BirdsColor); }

        Material.FrontProperties.Ambient.Alpha:=0.55;
        Material.FrontProperties.Ambient.Blue:= {*(1/255)}
          ((GetBValue(BirdBrainArray[CurrentFrenzy].BirdsColor)/255)/5);{0.01175;}
        Material.FrontProperties.Ambient.Green:=
          ((GetGValue(BirdBrainArray[CurrentFrenzy].BirdsColor)/255)/5);{0.01175;}
        Material.FrontProperties.Ambient.Red:=
          ((GetRValue(BirdBrainArray[CurrentFrenzy].BirdsColor)/255)/5);{0.1745;}
        Material.FrontProperties.Diffuse.Alpha:=0.55;
        Material.FrontProperties.Diffuse.Blue:=
          ((GetBValue(BirdBrainArray[CurrentFrenzy].BirdsColor)/255));{0.04136;}
        Material.FrontProperties.Diffuse.Green:=
          ((GetGValue(BirdBrainArray[CurrentFrenzy].BirdsColor)/255));{0.04136;}
        Material.FrontProperties.Diffuse.Red:=
          ((GetRValue(BirdBrainArray[CurrentFrenzy].BirdsColor)/255));{0.61424;}
        Material.FrontProperties.Specular.Alpha:=0.55;
        Material.FrontProperties.Specular.Blue:=
          ((GetRValue(BirdBrainArray[CurrentFrenzy].BirdsColor)+GetGValue(BirdBrainArray[CurrentFrenzy].BirdsColor)
          +GetBValue(BirdBrainArray[CurrentFrenzy].BirdsColor)+GetBValue(BirdBrainArray[CurrentFrenzy].BirdsColor))
          /765);{0.626959;}
        Material.FrontProperties.Specular.Green:=
          ((GetRValue(BirdBrainArray[CurrentFrenzy].BirdsColor)+GetGValue(BirdBrainArray[CurrentFrenzy].BirdsColor)
          +GetGValue(BirdBrainArray[CurrentFrenzy].BirdsColor)+GetBValue(BirdBrainArray[CurrentFrenzy].BirdsColor))
          /765);{0.626959;}
        Material.FrontProperties.Specular.Red:=
          ((GetRValue(BirdBrainArray[CurrentFrenzy].BirdsColor)+GetGValue(BirdBrainArray[CurrentFrenzy].BirdsColor)
          +GetRValue(BirdBrainArray[CurrentFrenzy].BirdsColor)+GetBValue(BirdBrainArray[CurrentFrenzy].BirdsColor))
          /765);{0.727811;}
        Material.FrontProperties.Shininess:=76;
        
      end;
    end;
    for i:=0 to BirdBrainArray[CurrentFrenzy].BirdsCount-1 do
    begin
      with BirdProxies[CurrentFrenzy, i] do
      begin
        Material.BackProperties.Ambient.Alpha:=0.55;
        Material.BackProperties.Ambient.Blue:=
          ((GetBValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)/255)/5);{0.01175;}
        Material.BackProperties.Ambient.Green:=
          ((GetGValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)/255)/5);{0.01175;}
        Material.BackProperties.Ambient.Red:=
          ((GetRValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)/255)/5);{0.1745;}
        Material.BackProperties.Diffuse.Alpha:=0.55;
        Material.BackProperties.Diffuse.Blue:=
          ((GetBValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)/255));{0.04136;}
        Material.BackProperties.Diffuse.Green:=
          ((GetGValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)/255));{0.04136;}
        Material.BackProperties.Diffuse.Red:=
          ((GetRValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)/255));{0.61424;}
        Material.BackProperties.Specular.Alpha:=0.55;
        Material.BackProperties.Specular.Blue:=
          ((GetRValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)+GetGValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)
          +GetBValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)+GetBValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor))
          /765);{0.626959;}
        Material.BackProperties.Specular.Green:=
          ((GetRValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)+GetGValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)
          +GetGValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)+GetBValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor))
          /765);{0.626959;}
        Material.BackProperties.Specular.Red:=
          ((GetRValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)+GetGValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)
          +GetRValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor)+GetBValue(BirdBrainArray[CurrentFrenzy].BirdsWingColor))
          /765);{0.727811;}
        Material.BackProperties.Shininess:=76;
      end;
    end;
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
procedure TAAABirdForm.BirdsSizeTBChange(Sender: TObject);
begin
  If BirdLanded then
  begin
  BirdBrainArray[CurrentFrenzy].BirdsSize := (BirdsSizeTB.Position);
  SizeLabel.Caption :=
    Format('[Size %d ]', [BirdBrainArray[CurrentFrenzy].BirdsSize]);
  {Re create Bird Sizes... ? }  
  end;
end;
    {BirdProxies
    boidArray[CurrentFrenzy, WhichBoid].Hungry
    BirdBrainArray[CurrentFrenzy].}
    {boidArray[CurrentFrenzy, WhichBoid].Hungry
    BirdBrainArray[CurrentFrenzy].}
procedure TAAABirdForm.BirdsRandomosityTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].BirdsRandomosity := (BirdsRandomosityTB.Position);
  RandomosityLabel.Caption :=
     Format('Rand %d ', [(BirdBrainArray[CurrentFrenzy].BirdsRandomosity)]);
end;

procedure TAAABirdForm.dBirdsFeedTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].dBirdsFeed := (dBirdsFeedTB.Position / 100);
  FeedLabel.Caption :=
   Format('Feed %f ', [BirdBrainArray[CurrentFrenzy].dBirdsFeed]);
end;
procedure TAAABirdForm.dBirdsSexyTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].dBirdsSexy := (dBirdsSexyTB.Position / 100);
  SexyLabel.Caption :=
    Format('Sexy %f ', [BirdBrainArray[CurrentFrenzy].dBirdsSexy]);
end;
procedure TAAABirdForm.dBirdsEnergyTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].dBirdsEnergy := (dBirdsEnergyTB.Position/100);
  EnergyLabel.Caption :=
  Format('Energy %f ', [BirdBrainArray[CurrentFrenzy].dBirdsEnergy]);
end;
procedure TAAABirdForm.BirdsActivityTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].BirdsActivity := (BirdsActivityTB.Position);
  ActivityLabel.Caption :=
  Format('Activity %d ', [BirdBrainArray[CurrentFrenzy].BirdsActivity]);
end;


procedure TAAABirdForm.BirdsSpeedTBChange(Sender: TObject);
begin
  If (BirdsSpeedTB.Position > dBirdsMinVTB.Position) then
  begin
    BirdBrainArray[CurrentFrenzy].dBirdsSpeed :=
      (BirdsSpeedTB.Position/ 100);
    SpeedLabel.Caption :=
      Format('Max %f ', [BirdBrainArray[CurrentFrenzy].dBirdsSpeed]);
  end else BirdsSpeedTB.Position:=dBirdsMinVTB.Position+1;
end;

procedure TAAABirdForm.dBirdsMinVTBChange(Sender: TObject);
begin
  If (BirdsSpeedTB.Position > dBirdsMinVTB.Position) then
  begin
    BirdBrainArray[CurrentFrenzy].dBirdsMinV :=
      (dBirdsMinVTB.Position / 100);
    HoverLabel.Caption :=
      Format('Hover %f ', [BirdBrainArray[CurrentFrenzy].dBirdsMinV]);
  end else dBirdsMinVTB.Position:=BirdsSpeedTB.Position-1;
end;

procedure TAAABirdForm.dWCopyTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].dWCopy := (dWCopyTB.Position / 100);
  dWCopyTBLabel.Caption :=
  Format('%f ', [BirdBrainArray[CurrentFrenzy].dWCopy]);
end;

procedure TAAABirdForm.RCopyTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].RCopy := (RCopyTB.Position);
  CopyLabel.Caption :=
   Format('Copy %d ', [BirdBrainArray[CurrentFrenzy].RCopy]);
end;


procedure TAAABirdForm.BirdsGlideTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].dBirdsGlide := (BirdsGlideTB.Position / 100);
  GlideLabel.Caption :=
  Format('Glide %f ', [BirdBrainArray[CurrentFrenzy].dBirdsGlide]);
end;

procedure TAAABirdForm.dBirdsMomentTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].dBirdsMoment := (dBirdsMomentTB.Position / 100);
  TurnLabel.Caption :=
  Format('Turn %f ', [BirdBrainArray[CurrentFrenzy].dBirdsMoment]);
end;

procedure TAAABirdForm.dBirdsFlockTBChange(Sender: TObject);
var DataString:String;
begin
  BirdBrainArray[CurrentFrenzy].dBirdsFlock := (dBirdsFlockTB.Position / 10000);
  str(BirdBrainArray[CurrentFrenzy].dBirdsFlock: 8: 6, DataString);
  dBirdsFlockLabel.Caption :=DataString;
{  Format('Flock %f ', [BirdBrainArray[CurrentFrenzy].dBirdsFlock]);}
end;

procedure TAAABirdForm.RCentroidTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].RCentroid := (RCentroidTB.Position);
  CenterLabel.Caption :=
  Format('Center %d ', [BirdBrainArray[CurrentFrenzy].RCentroid]);
end;

procedure TAAABirdForm.dWCentroidTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].dWCentroid := (dWCentroidTB.Position / 100);
  dWCentroidTBLabel.Caption :=
  Format('%f ', [BirdBrainArray[CurrentFrenzy].dWCentroid]);
end;

procedure TAAABirdForm.FerocityTBChange(Sender: TObject);
begin   {not..Attack (kitty hawk Ferocity)}
  BirdBrainArray[CurrentFrenzy].BirdsFerocity := (FerocityTB.Position);
  FerocityLabel.Caption :=
  Format('Ferocity %d ', [BirdBrainArray[CurrentFrenzy].BirdsFerocity]);
end;

procedure TAAABirdForm.dBirdsViewTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].dBirdsView := (dBirdsViewTB.Position* dDegToRad); {Degrees}
  ViewLabel.Caption :=  Format('View %d ', [dBirdsViewTB.Position]);
end;

procedure TAAABirdForm.RVisualTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].RVisual := (RVisualTB.Position);
  if (BirdBrainArray[CurrentFrenzy].RVisual < BirdBrainArray[CurrentFrenzy].BirdsSize) then
  begin
    BirdBrainArray[CurrentFrenzy].RVisual := BirdBrainArray[CurrentFrenzy].BirdsSize;
    RVisualTB.Position := BirdBrainArray[CurrentFrenzy].RVisual;
  end;
  VDistanceLabel.Caption :=
  Format('Distance %d ', [BirdBrainArray[CurrentFrenzy].RVisual]);
end;

procedure TAAABirdForm.dWVisualTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].dWVisual := (dWVisualTB.Position / 100);
  dWVisualTBLabel.Caption :=
  Format('%f ', [BirdBrainArray[CurrentFrenzy].dWVisual]);
end;

procedure TAAABirdForm.TenacityTBChange(Sender: TObject);
begin   {Reprisal Mocking bird revenge Tenacity}
  BirdBrainArray[CurrentFrenzy].BirdsTenacity := (TenacityTB.Position);
  DefendLabel.Caption :=
  Format('Defend %d ', [BirdBrainArray[CurrentFrenzy].BirdsTenacity]);
end;

procedure TAAABirdForm.dBirdsAvoidTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].dBirdsAvoid := (dBirdsAvoidTB.Position)* dDegToRad;
  AvoidLabel.Caption :=    {Degrees}
  Format('Avoid %d ', [dBirdsAvoidTB.Position]);
end;

procedure TAAABirdForm.RAvoidTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].RAvoid := (RAvoidTB.Position);
  if (BirdBrainArray[CurrentFrenzy].RAvoid < BirdBrainArray[CurrentFrenzy].BirdsSize) then
  begin
    BirdBrainArray[CurrentFrenzy].RAvoid := BirdBrainArray[CurrentFrenzy].BirdsSize;
    RAvoidTB.Position := BirdBrainArray[CurrentFrenzy].RAvoid;
  end;
  ADistanceLabel.Caption :=
  Format('Distance %d ', [BirdBrainArray[CurrentFrenzy].RAvoid]);
end;

procedure TAAABirdForm.dWAvoidTBChange(Sender: TObject);
begin
  BirdBrainArray[CurrentFrenzy].dWAvoid := (dWAvoidTB.Position / 100);
  dWAvoidTBLabel.Caption :=
   Format('%f ', [BirdBrainArray[CurrentFrenzy].dWAvoid]);
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
procedure TAAABirdForm.BirdFileBtnClick(Sender: TObject);
begin
  CancelFlightBtnClick(Sender);
  BirdLanded := True;
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
    end else showmessage('file not a .brd');
  end;
  FlyBirdsBtn.Enabled := True;
end;

procedure TAAABirdForm.BirdFileOpen(BirdFileName: string);
var
  F: Textfile;
  Version:Boolean;
  CodeVx: Integer;
  DataString: string;
begin
  Version:=False;
  inc(CurrentFrenzy);
  inc(FrenzyCount);
  FlockEdit.Text:= inttostr(CurrentFrenzy);
  SetLength(BirdBrainArray, FrenzyCount);
  AssignFile(F, BirdFileName);
  Reset(F);
  Readln(F, DataString);
  If (DataString='Version 1.0') then Version:=True;
  Readln(F, DataString);
  BirdBrainArray[CurrentFrenzy].BirdName := DataString;
  Readln(F, DataString);
  BirdBrainArray[CurrentFrenzy].BirdSong := DataString;
  Readln(F, DataString);
  BirdBrainArray[CurrentFrenzy].BirdBodyFile := DataString;
  Readln(F, DataString);
  BirdBrainArray[CurrentFrenzy].BirdTexture := DataString;

  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].BirdsRandomosity, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].BirdsCount, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  {if ((lowercase(ExtractFileName(BirdFileName)) = 'hunter.brd'))
    then BirdBrainArray[CurrentFrenzy].BirdsCount := 1
  else if (BirdBrainArray[CurrentFrenzy].BirdsCount < 4)
  then BirdBrainArray[CurrentFrenzy].BirdsCount := 4;}
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].BirdsSize, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dBirdsFeed, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dBirdsSexy, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);

  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].BirdsWingColor, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].BirdsColor, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].BirdsActivity, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dBirdsEnergy, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dBirdsSpeed, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dBirdsMinV, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].RCopy, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dWCopy, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);

  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dBirdsGlide, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dBirdsMoment, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].RCentroid, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dWCentroid, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);

  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].BirdsFerocity, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dBirdsView, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  BirdBrainArray[CurrentFrenzy].dBirdsView:=
  BirdBrainArray[CurrentFrenzy].dBirdsView* dDegToRad;
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].RVisual, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dWVisual, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);

  BirdBrainArray[CurrentFrenzy].dBirdsFlock:=0.0;

  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].BirdsTenacity, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dBirdsAvoid, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  BirdBrainArray[CurrentFrenzy].dBirdsAvoid:=
  BirdBrainArray[CurrentFrenzy].dBirdsAvoid* dDegToRad;
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].RAvoid, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  Readln(F, DataString);
  val(DataString, BirdBrainArray[CurrentFrenzy].dWAvoid, CodeVx);
  if CodeVx > 0 then Codefx(DataString, CodeVx);
  If Version then ;
  CloseFile(F);
  {ReSet to 0 so  BirdsMaker will change to BirdsCount}
  setlength(boidArray,FrenzyCount);
  setlength(BirdProxies,FrenzyCount);
  CodeVx:=BirdBrainArray[CurrentFrenzy].BirdsCount;
  BirdBrainArray[CurrentFrenzy].BirdsCount := 0;
  SetBirdsData;
  BirdsMaker(CodeVx);
end;


procedure TAAABirdForm.BirdsSaveBtnClick(Sender: TObject);
begin
  CancelFlightBtnClick(Sender);
  SaveDialog1.Title := 'Bird Files';
  SaveDialog1.Filter := 'Birds (*.brd)|*.brd';
  SaveDialog1.InitialDir := BirdLifeDir;
  SaveDialog1.Filename := BirdsNameEdit.Text;
  if SaveDialog1.Execute then
    if ((lowercase(ExtractFileExt(SaveDialog1.Filename)) = '.brd'))
      then  SaveBirdsFile(SaveDialog1.Filename,False);
end;

procedure TAAABirdForm.PrintitBtnClick(Sender: TObject);
var PathS:String;
begin
  CancelFlightBtnClick(Sender);
  PathS := ExtractFilePath(Application.EXEName);
  SaveBirdsFile(PathS + 'BIRDDATA.TXT', True);
  ExecuteFile('BIRDDATA.TXT', '', PathS, SW_SHOW);
end;

procedure TAAABirdForm.SaveBirdsFile(InName:String; Which:Boolean);
var
  F: Textfile;
  DataString, BirdString: string;
begin
  GetBirdsData;
  AssignFile(F, InName);
  Rewrite(F);
  writeln(F,'Version 1.0');
  If Which then begin
  writeln(F, ' ');
  writeln(F, 'Birds Data');
  writeln(F, ' ');
  end;
  If which then BirdString :='BirdName  ' else BirdString :='';
  BirdString := BirdString + BirdBrainArray[CurrentFrenzy].BirdName;
  writeln(F, BirdString);
  If which then BirdString :='Bird Song File  ' else BirdString :='';
  BirdString := BirdString + BirdBrainArray[CurrentFrenzy].BirdSong;
  writeln(F, BirdString);
  If which then BirdString :='Bird Body File  ' else BirdString :='';
  BirdString := BirdString + BirdBrainArray[CurrentFrenzy].BirdBodyFile;
  writeln(F, BirdString);
  If which then BirdString :='Bird Texture file  ' else BirdString :='';
  BirdString := BirdString + BirdBrainArray[CurrentFrenzy].BirdTexture;
  writeln(F, BirdString);
  str(BirdBrainArray[CurrentFrenzy].BirdsRandomosity, DataString);
  If which then BirdString :='Bird Randomosity  '+ DataString
  else  str(BirdBrainArray[CurrentFrenzy].BirdsRandomosity, BirdString);
  writeln(F, BirdString);
  str(BirdBrainArray[CurrentFrenzy].BirdsCount, DataString);
  If which then BirdString :='Number of Birds  '+ DataString
   else   str(BirdBrainArray[CurrentFrenzy].BirdsCount, BirdString);
  writeln(F, BirdString);
  If which then BirdString :='Bird Size  ' else BirdString :='';
  str(BirdBrainArray[CurrentFrenzy].BirdsSize, DataString);
  BirdString := BirdString + DataString;
  writeln(F, BirdString);
  str(BirdBrainArray[CurrentFrenzy].dBirdsFeed: 6: 3, DataString);
  If which then BirdString :=' Feed   '+ DataString
   else   str(BirdBrainArray[CurrentFrenzy].dBirdsFeed, BirdString);
  writeln(F, BirdString);
  str(BirdBrainArray[CurrentFrenzy].dBirdsSexy: 6: 3, DataString);
  If which then BirdString :=' Sexy   ' + DataString
  else   str(BirdBrainArray[CurrentFrenzy].dBirdsSexy, BirdString);
  writeln(F, BirdString);
  If which then BirdString :=' Wing Color  ' else BirdString :='';
  str(BirdBrainArray[CurrentFrenzy].BirdsWingColor, DataString);
  BirdString :=  BirdString+ DataString;
  writeln(F, BirdString);
  If which then BirdString :='Body Color  ' else BirdString :='';
  str(BirdBrainArray[CurrentFrenzy].BirdsColor, DataString);
  BirdString :=  BirdString+ DataString;
  writeln(F, BirdString);
  If which then BirdString :=' Activity  ' else BirdString :='';
  str(BirdBrainArray[CurrentFrenzy].BirdsActivity, DataString);
  BirdString :=  BirdString+ DataString;
  writeln(F, BirdString);
  str(BirdBrainArray[CurrentFrenzy].dBirdsEnergy:6:3, DataString);
  If which then BirdString :=' Energy  '+ DataString
   else   str(BirdBrainArray[CurrentFrenzy].dBirdsEnergy, BirdString);
  writeln(F, BirdString);
  If which then begin
  writeln(F, ' ');
  writeln(F, 'Flight Data: '); end;
  str(BirdBrainArray[CurrentFrenzy].dBirdsSpeed: 6: 3, DataString);
  If which then BirdString :=' Max Speed  ' + DataString
  else str(BirdBrainArray[CurrentFrenzy].dBirdsSpeed, BirdString);
  writeln(F, BirdString);
  str(BirdBrainArray[CurrentFrenzy].dBirdsMinV: 6: 3, DataString);
  If which then BirdString :=' Min Velocity  '+ DataString
   else   str(BirdBrainArray[CurrentFrenzy].dBirdsMinV, BirdString);
  writeln(F, BirdString);
  If which then BirdString :=' Copy Radius  ' else BirdString :='';
  str(BirdBrainArray[CurrentFrenzy].RCopy, DataString);
  BirdString :=  BirdString+ DataString;
  writeln(F, BirdString);
  str(BirdBrainArray[CurrentFrenzy].dWCopy: 6: 3, DataString);
  If which then BirdString :=' Copy Weight  '+ DataString
   else   str(BirdBrainArray[CurrentFrenzy].dWCopy, BirdString);
  writeln(F, BirdString);
  If which then  writeln(F, ' ');
  str(BirdBrainArray[CurrentFrenzy].dBirdsGlide: 6: 3, DataString);
  If which then BirdString :='Bird Glide  '+ DataString
   else   str(BirdBrainArray[CurrentFrenzy].dBirdsGlide, BirdString);
  writeln(F, BirdString);
  str(BirdBrainArray[CurrentFrenzy].dBirdsMoment: 6: 3, DataString);
  If which then BirdString :=' Turn Moment Velocity  ' + DataString
  else   str(BirdBrainArray[CurrentFrenzy].dBirdsMoment, BirdString);
  writeln(F, BirdString);
  If which then BirdString :=' Centroid Radius  ' else BirdString :='';
  str(BirdBrainArray[CurrentFrenzy].RCentroid, DataString);
  BirdString := BirdString + DataString;
  writeln(F, BirdString);
  str(BirdBrainArray[CurrentFrenzy].dWCentroid: 6: 3, DataString);
   If which then BirdString :=' Centroid Weight  '+ DataString
   else  str(BirdBrainArray[CurrentFrenzy].dWCentroid, BirdString);
  writeln(F, BirdString);
  If which then  writeln(F, ' ');
  If which then BirdString :='Bird Ferocity  ' else BirdString :='';
  str(BirdBrainArray[CurrentFrenzy].BirdsFerocity, DataString);
  BirdString := BirdString + DataString;
  writeln(F, BirdString);
     {Radto deg}
  str(radtodeg(BirdBrainArray[CurrentFrenzy].dBirdsView): 8: 3, DataString);
  If which then BirdString :=' View Degrees  '+ DataString
   else   str(radtodeg(BirdBrainArray[CurrentFrenzy].dBirdsView), BirdString);
  writeln(F, BirdString);
  If which then BirdString :=' Visual Radius  ' else BirdString :='';
  str(BirdBrainArray[CurrentFrenzy].RVisual, DataString);
  BirdString := BirdString + DataString;
  writeln(F, BirdString);
  str(BirdBrainArray[CurrentFrenzy].dWVisual: 6: 3, DataString);
  If which then BirdString :=' Visual Weight  ' + DataString
  else  str(BirdBrainArray[CurrentFrenzy].dWVisual, BirdString);
  writeln(F, BirdString);
  If which then writeln(F, ' ');
  If which then BirdString :='Bird Tenacity  ' else BirdString :='';
  str(BirdBrainArray[CurrentFrenzy].BirdsTenacity, DataString);
  BirdString := BirdString + DataString;
  writeln(F, BirdString);
     {Radto deg}
  str(radtodeg(BirdBrainArray[CurrentFrenzy].dBirdsAvoid): 8: 3, DataString);
  If which then BirdString :=' Avoid Degrees  '+ DataString
   else   str(radtodeg(BirdBrainArray[CurrentFrenzy].dBirdsAvoid), BirdString);
  writeln(F, BirdString);
  If which then BirdString :=' Avoid Radius  ' else BirdString :='';
  str(BirdBrainArray[CurrentFrenzy].RAvoid, DataString);
  BirdString := BirdString + DataString;
  writeln(F, BirdString);
  str(BirdBrainArray[CurrentFrenzy].dWAvoid: 6: 3, DataString);
  If which then BirdString :=' Avoid Weight  '+ DataString
   else   str(BirdBrainArray[CurrentFrenzy].dWAvoid, BirdString);
  writeln(F, BirdString);
  If which then   writeln(F, ' ');
  CloseFile(F);
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
procedure TAAABirdForm.ClearBtnClick(Sender: TObject);
begin
  CancelFlightBtnClick(Sender);
  SetBirdsDefault;{probable loss of memory somehow}
end;
procedure TAAABirdForm.SetBirdsDefault;
var
  k, I:Integer;
  F: Textfile;
  DataString: string;
begin
  Timer1.Enabled := False;
  GLCadencer1.Enabled := False;
  BirdLanded := True;
  FrenzyFilesLoaded:= False;
  BirdSinging := False;
  If FirstTime then
  begin
    MakeOffsetSquares;
    BirdYardWide :=GLSceneViewer1.Width div FlyingAreaDivisor;
    BirdsYardDepth := GLSceneViewer1.Height div FlyingAreaDivisor;
    {Ground is 20 below the 'bottom' flight deck}
    If Bird2dCB.Checked then BirdYardHeight:=1
    else BirdYardHeight:= {(BirdYardHeight+20)  * -1;}
    GLSceneViewer1.Height div FlyingAreaDivisor;

    GLBirdBody.LoadFromFile(ExtractFilePath(ParamStr(0))+'Glsboid.3ds');
    if (FileExists(BirdLifeDir+'GlsHunter.3ds')) then
    GLHunterBody.LoadFromFile(ExtractFilePath(ParamStr(0))+'GlsHunter.3ds')
    else GLHunterBody.LoadFromFile(ExtractFilePath(ParamStr(0))+'Glsboid.3ds');
    Hunter.BirdBody:=TGLFreeform.create(GLScene1.objects);
    GLScene1.Objects.AddChild(Hunter.BirdBody);
    Hunter.BirdBody.Assign(GLHunterBody);
    if (FileExists(ExtractFilePath(ParamStr(0))+'hunter.hk')) then
    begin
      AssignFile(F, ExtractFilePath(ParamStr(0))+'hunter.hk');
      Reset(F);
      Readln(F, DataString);
      Hunter.BirdWingColor := strtoint(DataString);
      Readln(F, DataString);
      Hunter.BirdColor := strtoint(DataString);
      Readln(F, DataString);
      Hunter.BirdSize := strtoint(DataString);
      Readln(F, DataString);
      Hunter.BirdSpeed := strtoint(DataString);
      Readln(F, DataString);
      Hunter.BirdTexture := DataString;
      CloseFile(F);
    end else
    begin
      Hunter.BirdWingColor:= clGray;
      Hunter.BirdColor := clMaroon;
      Hunter.BirdSize := 60;
      Hunter.BirdSpeed := 123;
      Hunter.BirdTexture :='Hunter.bmp';
    end;
    with Hunter.BirdBody do
    begin
      scale.x:= (Hunter.BirdSize /10);
      scale.y:= (Hunter.BirdSize /10);
      scale.z:= (Hunter.BirdSize /10);
      if (FileExists(BirdLifeDir+Hunter.BirdTexture)) then
      begin

      end else
      begin
        Material.FrontProperties.Ambient.Alpha:=0.55;
        Material.FrontProperties.Ambient.Blue:=
          ((GetBValue(Hunter.BirdColor)/255)/5);{0.01175;}
        Material.FrontProperties.Ambient.Green:=
          ((GetGValue(Hunter.BirdColor)/255)/5);{0.01175;}
        Material.FrontProperties.Ambient.Red:=
          ((GetRValue(Hunter.BirdColor)/255)/5);{0.1745;}
        Material.FrontProperties.Diffuse.Alpha:=0.55;
        Material.FrontProperties.Diffuse.Blue:=
          ((GetBValue(Hunter.BirdColor)/255));{0.04136;}
        Material.FrontProperties.Diffuse.Green:=
          ((GetGValue(Hunter.BirdColor)/255));{0.04136;}
        Material.FrontProperties.Diffuse.Red:=
          ((GetRValue(Hunter.BirdColor)/255));{0.61424;}
        Material.FrontProperties.Specular.Alpha:=0.55;
        Material.FrontProperties.Specular.Blue:=
          ((GetRValue(Hunter.BirdColor)+GetGValue(Hunter.BirdColor)
          +GetBValue(Hunter.BirdColor)+GetBValue(Hunter.BirdColor))
          /765);{0.626959;}
        Material.FrontProperties.Specular.Green:=
          ((GetRValue(Hunter.BirdColor)+GetGValue(Hunter.BirdColor)
          +GetGValue(Hunter.BirdColor)+GetBValue(Hunter.BirdColor))
          /765);{0.626959;}
        Material.FrontProperties.Specular.Red:=
          ((GetRValue(Hunter.BirdColor)+GetGValue(Hunter.BirdColor)
          +GetRValue(Hunter.BirdColor)+GetBValue(Hunter.BirdColor))
          /765);{0.727811;}
        Material.FrontProperties.Shininess:=76;
      end;
      Hunter.pos.x := Random(BirdYardWide ) - Random(BirdYardWide);
      Hunter.pos.y := Random(BirdYardHeight ) - Random(BirdYardHeight);
      Hunter.pos.z := Random(BirdYardWide ) - Random(BirdYardWide);
      Hunter.BirdBody.Position.x:= Hunter.pos.x;
      Hunter.BirdBody.Position.y:= Hunter.pos.y;
      Hunter.BirdBody.Position.z:= Hunter.pos.z;
      RollAngle:=GLBirdBody.RollAngle; {Random(360);}
      Direction:=GLBirdBody.Direction;
      Up:=GLBirdBody.Up;
      TransformationChanged;
      Visible:=True;{False;}
    end;

    {GLDummyCube1 Feeder leave at 0,0 as is also camera}

    {ObstacleProxies}
    ObstacleCount := 8;
    SetLength(ObstacleArray, 8);
    SetLength(ObstacleProxies, 8);
    ObstacleArray[0].X := Random(BirdYardWide div 2) ;
    ObstacleArray[0].Y := Random(BirdYardHeight div 2) ;
    ObstacleArray[0].Z := Random(BirdYardHeight div 2) ;
    ObstacleArray[1].X := Random(BirdYardWide div 2) * -1;
    ObstacleArray[1].Y := Random(BirdYardHeight div 2) ;
    ObstacleArray[1].Z := Random(BirdYardHeight div 2) ;
    ObstacleArray[2].X := Random(BirdYardWide div 2) ;
    ObstacleArray[2].Y := Random(BirdYardHeight div 2) * -1 ;
    ObstacleArray[2].Z := Random(BirdYardHeight div 2) ;
    ObstacleArray[3].X := Random(BirdYardWide div 2) * -1 ;
    ObstacleArray[3].Y := Random(BirdYardHeight div 2) * -1 ;
    ObstacleArray[3].Z := Random(BirdYardHeight div 2) ;
    ObstacleArray[4].X := Random(BirdYardWide div 2) ;
    ObstacleArray[4].Y := Random(BirdYardHeight div 2) ;
    ObstacleArray[4].Z := Random(BirdYardHeight div 2)* -1 ;
    ObstacleArray[5].X := Random(BirdYardWide div 2) * -1;
    ObstacleArray[5].Y := Random(BirdYardHeight div 2) ;
    ObstacleArray[5].Z := Random(BirdYardHeight div 2)* -1 ;
    ObstacleArray[6].X := Random(BirdYardWide div 2) ;
    ObstacleArray[6].Y := Random(BirdYardHeight div 2) * -1 ;
    ObstacleArray[6].Z := Random(BirdYardHeight div 2)* -1 ;
    ObstacleArray[7].X := Random(BirdYardWide div 2) * -1 ;
    ObstacleArray[7].Y := Random(BirdYardHeight div 2) * -1 ;
    ObstacleArray[7].Z := Random(BirdYardHeight div 2)* -1 ;
    For I:=0 to ObstacleCount-1 do
    begin
      ObstacleProxies[I]:=TGLSphere.create(GLScene1.objects);
      GLScene1.Objects.AddChild(ObstacleProxies[I]);
      ObstacleProxies[I].Assign(LittleRed);
      ObstacleProxies[I].position.x:=ObstacleArray[I].X;
      ObstacleProxies[I].position.y:=ObstacleArray[I].y;
      ObstacleProxies[I].position.z:=ObstacleArray[I].z;
      ObstacleProxies[I].Visible:=True;
    end;
  end else
  begin
    for k := 0 to FrenzyCount - 1 do
    begin
      for I :=0 to BirdBrainArray[k].BirdsCount - 1  do
      begin
        BirdProxies[k,i].Visible:=False;
      end;
    end;
    BirdsMaker(0);
  end;
  {CurrentFrenzy := 0;  FrenzyCount:=1;}
  CurrentBirdFollowed:= 0;
  BirdFollowerEdit.Text:=inttostr(CurrentBirdFollowed);
  FirstTime:=False;
  if (FileExists(BirdLifeDir+'Cardinal.brd')) then
  begin
  CurrentFrenzy := -1;
  FrenzyCount:=0;
  BirdFileOpen(BirdLifeDir+'Cardinal.brd');
  end else
  begin
  CurrentFrenzy := 0;
  FrenzyCount:=1;
  FlockEdit.Text:=inttostr(CurrentFrenzy);
  Application.ProcessMessages;
  SetLength(BirdBrainArray, 1);
  setlength(boidArray,1,20);
  setlength(BirdProxies,1,20);
  BirdBrainArray[CurrentFrenzy].BirdName := 'Cardinal.brd';
  BirdBrainArray[CurrentFrenzy].BirdSong := 'Cardinal.wav';
  BirdBrainArray[CurrentFrenzy].BirdBodyFile := 'Cardinal.3ds';
  BirdBrainArray[CurrentFrenzy].BirdTexture := 'Cardinal.bmp';
  BirdBrainArray[CurrentFrenzy].BirdsRandomosity:= 2;
  BirdBrainArray[CurrentFrenzy].BirdsCount := 20;
  BirdBrainArray[CurrentFrenzy].BirdsSize := 34;
  BirdBrainArray[CurrentFrenzy].dBirdsFeed := 0.38;
  BirdBrainArray[CurrentFrenzy].dBirdsSexy := 0.24;
  BirdBrainArray[CurrentFrenzy].BirdsWingColor := clMaroon;
  BirdBrainArray[CurrentFrenzy].BirdsColor :=clRed ;
  BirdBrainArray[CurrentFrenzy].BirdsActivity := 84;
  BirdBrainArray[CurrentFrenzy].dBirdsEnergy:= 0.78;
  BirdBrainArray[CurrentFrenzy].dBirdsSpeed := 0.35;
   BirdBrainArray[CurrentFrenzy].dBirdsMinV := 0.25;
   BirdBrainArray[CurrentFrenzy].RCopy := 60;
   BirdBrainArray[CurrentFrenzy].dWCopy := 0.3;
  BirdBrainArray[CurrentFrenzy].dBirdsGlide := 0.02;
   BirdBrainArray[CurrentFrenzy].dBirdsMoment := 0.9;
  BirdBrainArray[CurrentFrenzy].dBirdsFlock:=0.0;
   BirdBrainArray[CurrentFrenzy].RCentroid := 66;
   BirdBrainArray[CurrentFrenzy].dWCentroid := 0.4;
  BirdBrainArray[CurrentFrenzy].BirdsFerocity := 10;
   BirdBrainArray[CurrentFrenzy].dBirdsView := 270;
    BirdBrainArray[CurrentFrenzy].dBirdsView :=
     BirdBrainArray[CurrentFrenzy].dBirdsView * dDegToRad;
   BirdBrainArray[CurrentFrenzy].RVisual := 30;
   BirdBrainArray[CurrentFrenzy].dWVisual := 0.6;
  BirdBrainArray[CurrentFrenzy].BirdsTenacity := 15;
   BirdBrainArray[CurrentFrenzy].dBirdsAvoid := 90;
    BirdBrainArray[CurrentFrenzy].dBirdsAvoid :=
      BirdBrainArray[CurrentFrenzy].dBirdsAvoid * dDegToRad;
   BirdBrainArray[CurrentFrenzy].RAvoid := 35;
   BirdBrainArray[CurrentFrenzy].dWAvoid := 0.6;
   {ReSet to 0 so  BirdsMaker will change to 120}
   BirdBrainArray[CurrentFrenzy].BirdsCount := 0;
   SetBirdsData;
   BirdsMaker(20);
  end;
end;

procedure TAAABirdForm.SetBirdsData;
var DataString:String;
begin
  BirdsNameEdit.Text:=BirdBrainArray[CurrentFrenzy].BirdName;
  BirdsColorPanel.Color := BirdBrainArray[CurrentFrenzy].BirdsColor;
  WingColorPanel.Color := BirdBrainArray[CurrentFrenzy].BirdsWingColor;
  BirdBodyEdit.Text:=BirdBrainArray[CurrentFrenzy].BirdBodyFile;
  BirdTextureEdit.Text:=BirdBrainArray[CurrentFrenzy].BirdTexture;
  BirdSongEdit.Text:=BirdBrainArray[CurrentFrenzy].BirdSong;
  RandomosityLabel.Caption := Format('Rand %d ', [(BirdBrainArray[CurrentFrenzy].BirdsRandomosity)]);
  BirdsRandomosityTB.Position := (BirdBrainArray[CurrentFrenzy].BirdsRandomosity);
  BirdsEdit.Text:=inttostr(BirdBrainArray[CurrentFrenzy].BirdsCount);
  SizeLabel.Caption :=Format('[Size %d ]', [BirdBrainArray[CurrentFrenzy].BirdsSize]);
  BirdsSizeTB.Position := BirdBrainArray[CurrentFrenzy].BirdsSize;
  SexyLabel.Caption :=Format('Sexy %f ', [BirdBrainArray[CurrentFrenzy].dBirdsSexy]);
  dBirdsSexyTB.Position := Round(BirdBrainArray[CurrentFrenzy].dBirdsSexy* 100);
  FeedLabel.Caption := Format('Feed %f ', [BirdBrainArray[CurrentFrenzy].dBirdsFeed]);
  dBirdsFeedTB.Position := Round(BirdBrainArray[CurrentFrenzy].dBirdsFeed * 100);
  ActivityLabel.Caption := Format('Activity %d ', [BirdBrainArray[CurrentFrenzy].BirdsActivity]);
  BirdsActivityTB.Position := BirdBrainArray[CurrentFrenzy].BirdsActivity;
  EnergyLabel.Caption := Format('Energy %f ', [BirdBrainArray[CurrentFrenzy].dBirdsEnergy]);
  dBirdsEnergyTB.Position := Round(BirdBrainArray[CurrentFrenzy].dBirdsEnergy*100);
  SpeedLabel.Caption := Format('Max %f ', [BirdBrainArray[CurrentFrenzy].dBirdsSpeed]);
  BirdsSpeedTB.Position := Round(BirdBrainArray[CurrentFrenzy].dBirdsSpeed*100);
  HoverLabel.Caption := Format('Hover %f ', [BirdBrainArray[CurrentFrenzy].dBirdsMinV]);
   dBirdsMinVTB.Position := Round(BirdBrainArray[CurrentFrenzy].dBirdsMinV * 100);
  dWCopyTBLabel.Caption := Format('%f ', [BirdBrainArray[CurrentFrenzy].dWCopy]);
  dWCopyTB.Position := round(BirdBrainArray[CurrentFrenzy].dWCopy * 100);
  CopyLabel.Caption := Format('Copy %d ', [BirdBrainArray[CurrentFrenzy].RCopy]);
   RCopyTB.Position := BirdBrainArray[CurrentFrenzy].RCopy;
  GlideLabel.Caption := Format('Glide %f ', [BirdBrainArray[CurrentFrenzy].dBirdsGlide]);
  BirdsGlideTB.Position := Round(BirdBrainArray[CurrentFrenzy].dBirdsGlide * 100);
  TurnLabel.Caption := Format('Turn %f ', [BirdBrainArray[CurrentFrenzy].dBirdsMoment]);
   dBirdsMomentTB.Position := Round(BirdBrainArray[CurrentFrenzy].dBirdsMoment * 100);

  str(BirdBrainArray[CurrentFrenzy].dBirdsFlock: 8: 6, DataString);
  dBirdsFlockLabel.Caption :=DataString;
  dBirdsFlockTB.Position := Round(BirdBrainArray[CurrentFrenzy].dBirdsFlock* 1000);
  CenterLabel.Caption := Format('Center %d ', [BirdBrainArray[CurrentFrenzy].RCentroid]);
   RCentroidTB.Position := BirdBrainArray[CurrentFrenzy].RCentroid;
  dWCentroidTBLabel.Caption := Format('%f ', [BirdBrainArray[CurrentFrenzy].dWCentroid]);
   dWCentroidTB.Position := Round(BirdBrainArray[CurrentFrenzy].dWCentroid * 100);
  FerocityLabel.Caption := Format('Ferocity %d ', [BirdBrainArray[CurrentFrenzy].BirdsFerocity]);
  FerocityTB.Position := BirdBrainArray[CurrentFrenzy].BirdsFerocity;
   ViewLabel.Caption := Format('View %d ', [round(radtodeg(BirdBrainArray[CurrentFrenzy].dBirdsView))]);
  dBirdsViewTB.Position := Round(radtodeg(BirdBrainArray[CurrentFrenzy].dBirdsView));
  VDistanceLabel.Caption := Format('Distance %d ', [BirdBrainArray[CurrentFrenzy].RVisual]);
    RVisualTB.Position := BirdBrainArray[CurrentFrenzy].RVisual;
  dWVisualTBLabel.Caption := Format('%f ', [BirdBrainArray[CurrentFrenzy].dWVisual]);
  dWVisualTB.Position := Round(BirdBrainArray[CurrentFrenzy].dWVisual * 100);
  DefendLabel.Caption := Format('Defend %d ', [BirdBrainArray[CurrentFrenzy].BirdsTenacity]);
   TenacityTB.Position := BirdBrainArray[CurrentFrenzy].BirdsTenacity;
  AvoidLabel.Caption := Format('Avoid %d ', [round(radtodeg(BirdBrainArray[CurrentFrenzy].dBirdsAvoid))]);
  dBirdsAvoidTB.Position := Round(radtodeg(BirdBrainArray[CurrentFrenzy].dBirdsAvoid));

  ADistanceLabel.Caption := Format('Distance %d ', [BirdBrainArray[CurrentFrenzy].RAvoid]);
    RAvoidTB.Position := BirdBrainArray[CurrentFrenzy].RAvoid;
  dWAvoidTBLabel.Caption := Format('%f ', [BirdBrainArray[CurrentFrenzy].dWAvoid]);
  dWAvoidTB.Position := Round(BirdBrainArray[CurrentFrenzy].dWAvoid * 100);
end;


procedure TAAABirdForm.GetBirdsData;
var CodeVx: Integer;
begin{Translate the Edits into Variables}
  BirdBrainArray[CurrentFrenzy].BirdName := BirdsNameEdit.Text;
  BirdBrainArray[CurrentFrenzy].BirdSong := BirdSongEdit.Text;
  BirdBrainArray[CurrentFrenzy].BirdBodyFile:=BirdBodyEdit.Text;
  BirdBrainArray[CurrentFrenzy].BirdTexture:=BirdTextureEdit.Text;

  {Body Texture}
  BirdBrainArray[CurrentFrenzy].BirdsRandomosity := (BirdsRandomosityTB.Position);
  val(BirdsEdit.Text, BirdBrainArray[CurrentFrenzy].BirdsCount, CodeVx);
  if (CodeVx > 0) then Codefx(BirdsEdit.Text, CodeVx);
  BirdBrainArray[CurrentFrenzy].BirdsSize := (BirdsSizeTB.Position);
  BirdBrainArray[CurrentFrenzy].dBirdsFeed := (dBirdsFeedTB.Position / 100);
  BirdBrainArray[CurrentFrenzy].dBirdsSexy := (dBirdsSexyTB.Position / 100);
  BirdBrainArray[CurrentFrenzy].BirdsWingColor := WingColorPanel.Color;
  BirdBrainArray[CurrentFrenzy].BirdsColor := BirdsColorPanel.Color;
  BirdBrainArray[CurrentFrenzy].BirdsActivity := (BirdsActivityTB.Position);
  BirdBrainArray[CurrentFrenzy].dBirdsEnergy := (dBirdsEnergyTB.Position/100);
  BirdBrainArray[CurrentFrenzy].dBirdsSpeed := (BirdsSpeedTB.Position/100);
  BirdBrainArray[CurrentFrenzy].dBirdsMinV := (dBirdsMinVTB.Position / 100);
  BirdBrainArray[CurrentFrenzy].RCopy := (RCopyTB.Position);
  BirdBrainArray[CurrentFrenzy].dWCopy := (dWCopyTB.Position / 100);
  BirdBrainArray[CurrentFrenzy].dBirdsGlide := (BirdsGlideTB.Position / 100);
  BirdBrainArray[CurrentFrenzy].dBirdsMoment := (dBirdsMomentTB.Position / 100);

  BirdBrainArray[CurrentFrenzy].dBirdsFlock := (dBirdsFlockTB.Position / 10000);
  BirdBrainArray[CurrentFrenzy].RCentroid := (RCentroidTB.Position);
  BirdBrainArray[CurrentFrenzy].dWCentroid := (dWCentroidTB.Position / 100);
  BirdBrainArray[CurrentFrenzy].BirdsFerocity := (FerocityTB.Position);
  BirdBrainArray[CurrentFrenzy].dBirdsView :=
    (dBirdsViewTB.Position * dDegToRad);
  BirdBrainArray[CurrentFrenzy].RVisual := (RVisualTB.Position);
  BirdBrainArray[CurrentFrenzy].dWVisual := (dWVisualTB.Position / 100);
  BirdBrainArray[CurrentFrenzy].BirdsTenacity := (TenacityTB.Position);
  BirdBrainArray[CurrentFrenzy].dBirdsAvoid :=
      (dBirdsAvoidTB.Position * dDegToRad);
  BirdBrainArray[CurrentFrenzy].RAvoid := (RAvoidTB.Position);
  BirdBrainArray[CurrentFrenzy].dWAvoid := (dWAvoidTB.Position / 100);
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
procedure TAAABirdForm.BirdsBtnClick(Sender: TObject);
var  CodeVx, TempBirds: Integer;
begin
  {CancelFlightBtnClick(Sender);  }
  val(BirdsEdit.Text, TempBirds, CodeVx);
  if (CodeVx > 0) then Codefx(BirdsEdit.Text, CodeVx) else
  begin
    if (TempBirds < 1) then TempBirds := 1;
    {if (TempBirds > 50) then TempBirds := 50;}
    BirdsEdit.Text:=inttostr(TempBirds);
    BirdsMaker(TempBirds);
  end;
end;

/////////////////////////////////////////////////////
procedure TAAABirdForm.BirdsMaker(HowMany:Integer);
var
 {RandomSign,} I:Integer;
 BirdTexturePresent:Boolean;
Begin
  {RandomSign:=1;}
  if (BirdBrainArray[CurrentFrenzy].BirdsCount = HowMany) then exit;
  if (BirdBrainArray[CurrentFrenzy].BirdsCount < HowMany) then
  begin
    SetLength(BirdProxies[CurrentFrenzy], HowMany);
    setlength(boidArray[CurrentFrenzy],HowMany);
    {Do SOMETHING about loading Different .3ds files per Bird Type}
    if (FileExists(ExtractFilePath(ParamStr(0))+BirdBrainArray[CurrentFrenzy].BirdBodyFile)) then
    GLBirdBody.LoadFromFile(ExtractFilePath(ParamStr(0))+BirdBrainArray[CurrentFrenzy].BirdBodyFile)
    else GLBirdBody.LoadFromFile(ExtractFilePath(ParamStr(0))+'Glsboid.3ds');
if (FileExists(ParamStr(0)+BirdBrainArray[CurrentFrenzy].BirdTexture)) then
begin
  BirdTexturePresent:=False;
end else BirdTexturePresent:=False;

    for I := BirdBrainArray[CurrentFrenzy].BirdsCount to HowMany - 1 do
    begin
      BirdProxies[CurrentFrenzy,i]:=TGLFreeform.create(GLScene1.objects);
      GLScene1.Objects.AddChild(BirdProxies[CurrentFrenzy,i]);
      BirdProxies[CurrentFrenzy,i].Assign(GLBirdBody);
      with BirdProxies[CurrentFrenzy,i] do
      begin
        {If Random> 0.5 then RandomSign:= -1;}
        scale.x:= (BirdBrainArray[CurrentFrenzy].BirdsSize /10)
           {+(BirdBrainArray[CurrentFrenzy].BirdsRandomosity*RandomSign)};
        scale.y:= (BirdBrainArray[CurrentFrenzy].BirdsSize /10)
           {+(BirdBrainArray[CurrentFrenzy].BirdsRandomosity*RandomSign)};
        scale.z:= (BirdBrainArray[CurrentFrenzy].BirdsSize /10)
           {+(BirdBrainArray[CurrentFrenzy].BirdsRandomosity*RandomSign)};
        position.SetPoint(Random(BirdYardWide)-Random(BirdYardWide),
                            Random(BirdYardHeight)-Random(BirdYardHeight ),
                           Random(BirdsYardDepth)-Random(BirdsYardDepth ));
        If BirdTexturePresent then
        begin
          Material.Texture.Image.LoadFromFile(
          BirdLifeDir+BirdBrainArray[CurrentFrenzy].BirdTexture);
          Material.Texture.Disabled:=False;
        end else
        begin
          Material.Texture.Disabled:=True;
          Material.FrontProperties.Ambient.Assign(GLBirdBody.Material.FrontProperties.Ambient);
          Material.FrontProperties.Diffuse.Assign(GLBirdBody.Material.FrontProperties.Diffuse);
          Material.FrontProperties.Specular.Assign(GLBirdBody.Material.FrontProperties.Specular);
        end;
        Visible:=True;
        RollAngle:=GLBirdBody.RollAngle; {Random(360);}
        Direction:=GLBirdBody.Direction;
        Up:=GLBirdBody.Up;
        boidArray[CurrentFrenzy, i].pos.x := Random(BirdYardWide ) - Random(BirdYardWide);
        boidArray[CurrentFrenzy, i].pos.y := Random(BirdYardHeight ) - Random(BirdYardHeight);
        boidArray[CurrentFrenzy, i].pos.z := Random(BirdsYardDepth ) - Random(BirdsYardDepth);
        boidArray[CurrentFrenzy, i].vel.x := Random(51) - 25; {Negative means flying left...}
        boidArray[CurrentFrenzy, i].vel.y := Random(51) - 25; {up.. down}
        boidArray[CurrentFrenzy, i].vel.z := Random(51) - 25; {front to back}
        boidArray[CurrentFrenzy, i].Hunted := True;
        boidArray[CurrentFrenzy, i].Hungry := False;
        boidArray[CurrentFrenzy, i].Hungrytimer :=0;
        boidArray[CurrentFrenzy, i].BirdsActivity:=
         (BirdBrainArray[CurrentFrenzy].BirdsActivity
         +(random(BirdBrainArray[CurrentFrenzy].BirdsActivity)
         *random(BirdBrainArray[CurrentFrenzy].BirdsActivity)) );
        boidArray[CurrentFrenzy, i].dBirdsSexy:= 0;
        boidArray[CurrentFrenzy, i].BirdsTenacity:=
           Random(BirdBrainArray[CurrentFrenzy].BirdsTenacity);
        boidArray[CurrentFrenzy, i].BirdsFerocity:=
           Random(BirdBrainArray[CurrentFrenzy].BirdsFerocity);
        TransformationChanged;
      end;
    end;   Application.ProcessMessages;
    BirdBrainArray[CurrentFrenzy].BirdsCount := HowMany;Application.ProcessMessages;
    SetBirdsData;  Application.ProcessMessages;
    If (not BirdTexturePresent) then ColorTheBirds;
  end else
  begin
    for I :=BirdBrainArray[CurrentFrenzy].BirdsCount - 1 downto HowMany   do
    begin
      BirdProxies[CurrentFrenzy,i].Visible:=False;
    end; Application.ProcessMessages;
    SetLength(BirdProxies[CurrentFrenzy], HowMany);Application.ProcessMessages;
    setlength(boidArray[CurrentFrenzy],HowMany);Application.ProcessMessages;
    BirdBrainArray[CurrentFrenzy].BirdsCount := HowMany;Application.ProcessMessages;
    SetBirdsData;Application.ProcessMessages;
  end;
End;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
procedure TAAABirdForm.MakeOffsetSquares;
var
  RowOffset, SquareSize, Width: Integer;
  Range: Single;
  x1, y1, x2, y2, x3, y3: Integer;
  i1, i2, i3, i4: Single;
  p1, p2, p3, p4: Single;
begin
  { Seed the random number generator. }
  { RandSeed := PFractalParams(ConfigData)^.Seed;}
  { Start with a level playing field. }
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


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
procedure TAAABirdForm.CancelFlightBtnClick(Sender: TObject);
begin
  Timer1.Enabled := False;
  GLCadencer1.Enabled := False;
  AAABirdForm.Caption := 'Birds 3D';
  BirdLanded := True;
  CrashCount := 0;
  FlyBirdsBtn.Enabled := True;
  ChaseIt := False;
  AttackBtn.Color := clBtnFace;
  Hunter.BirdBody.Visible:=True;{ChaseIt;}
end;


procedure TAAABirdForm.FlyBirdsBtnClick(Sender: TObject);
begin
  GetBirdsData; {Translate the Edits into Variables}
  FlyBirdsBtn.Enabled := False;
  BirdLanded := True;
  CrashCount := 0;
  ChaseIt := False;
  AttackBtn.Color := clBtnFace;
  if FrenzyFilesLoaded then FrenzyFilesArray;
  BirdStartup;
  Timer1.Enabled := True;
  GLCadencer1.Enabled := True;
end;

procedure TAAABirdForm.BirdStartup;
var
  k, i: Integer;
begin
  Randomize;
  CrashCount := 0;
  CurrentSeekerJ:=BirdBrainArray[1].BirdsCount-1;
  {reset obstacles positions}
     ObstacleArray[0].X := Random(BirdYardWide div 2) ;
     ObstacleArray[0].Y := Random(BirdYardHeight div 2) ;
     ObstacleArray[0].Z := Random(BirdsYardDepth div 2) ;
     ObstacleArray[1].X := Random(BirdYardWide div 2) * -1;
     ObstacleArray[1].Y := Random(BirdYardHeight div 2) ;
     ObstacleArray[1].Z := Random(BirdsYardDepth div 2) ;
     ObstacleArray[2].X := Random(BirdYardWide div 2) ;
     ObstacleArray[2].Y := Random(BirdYardHeight div 2) * -1 ;
     ObstacleArray[2].Z := Random(BirdsYardDepth div 2) ;
     ObstacleArray[3].X := Random(BirdYardWide div 2) * -1 ;
     ObstacleArray[3].Y := Random(BirdYardHeight div 2) * -1 ;
     ObstacleArray[3].Z := Random(BirdsYardDepth div 2) ;
     ObstacleArray[4].X := Random(BirdYardWide div 2) ;
     ObstacleArray[4].Y := Random(BirdYardHeight div 2) ;
     ObstacleArray[4].Z := Random(BirdsYardDepth div 2)* -1 ;
     ObstacleArray[5].X := Random(BirdYardWide div 2) * -1;
     ObstacleArray[5].Y := Random(BirdYardHeight div 2) ;
     ObstacleArray[5].Z := Random(BirdsYardDepth div 2)* -1 ;
     ObstacleArray[6].X := Random(BirdYardWide div 2) ;
     ObstacleArray[6].Y := Random(BirdYardHeight div 2) * -1 ;
     ObstacleArray[6].Z := Random(BirdsYardDepth div 2)* -1 ;
     ObstacleArray[7].X := Random(BirdYardWide div 2) * -1 ;
     ObstacleArray[7].Y := Random(BirdYardHeight div 2) * -1 ;
     ObstacleArray[7].Z := Random(BirdsYardDepth div 2)* -1 ;
     For I:=0 to ObstacleCount-1 do
     begin
     ObstacleProxies[I].position.x:=ObstacleArray[I].X;
     ObstacleProxies[I].position.y:=ObstacleArray[I].y;
     ObstacleProxies[I].position.z:=ObstacleArray[I].z;
     If DisplayObstaclesCB.Checked then
        ObstacleProxies[I].Visible:=True
        else ObstacleProxies[I].Visible:=False;
     end;

  {reset Hunter positions}
  Hunter.pos.x := Random(BirdYardWide ) - Random(BirdYardWide);
  Hunter.pos.y := Random(BirdYardHeight ) - Random(BirdYardHeight);
  Hunter.pos.z := Random(BirdsYardDepth ) - Random(BirdsYardDepth);
  Hunter.vel.x := Random(51) - 25; {Negative means flying left...}
  Hunter.vel.y := Random(51) - 25; {up.. down}
  Hunter.vel.z := Random(51) - 25; {front to back}
  Hunter.BirdBody.Position.x:= Hunter.pos.x;
  Hunter.BirdBody.Position.y:= Hunter.pos.y;
  Hunter.BirdBody.Position.z:= Hunter.pos.z;
  Hunter.BirdBody.Visible:=ChaseIt;
  {reset Bird positions}
  FrenzyBirdCount :=0;
  for k := 0 to FrenzyCount - 1 do begin
    WorkingFrenzy := K;
    FrenzyBirdCount := FrenzyBirdCount+ BirdBrainArray[k].BirdsCount;
    for i := 0 to BirdBrainArray[k].BirdsCount - 1 do
    begin
     {X , Y ,Z can be negatives around screen center}
      boidArray[k, i].pos.x := Random(BirdYardWide ) - Random(BirdYardWide);
      boidArray[k, i].pos.y := Random(BirdYardHeight ) - Random(BirdYardHeight);
      boidArray[k, i].pos.z := Random(BirdsYardDepth ) - Random(BirdsYardDepth);
      boidArray[k, i].vel.x := Random(51) - 25; {Negative means flying left...}
      boidArray[k, i].vel.y := Random(51) - 25; {up.. down}
      boidArray[k, i].vel.z := Random(51) - 25; {front to back}
      boidArray[k, i].Hunted := True;
      boidArray[k, i].Hungry := False;
      boidArray[k, i].Hungrytimer :=0;
      boidArray[k, i].BirdsActivity:=
        (BirdBrainArray[k].BirdsActivity
        +(random(BirdBrainArray[k].BirdsActivity)
         *random(BirdBrainArray[k].BirdsActivity)) );
     boidArray[k, i].dBirdsSexy:= 0;
     boidArray[k, i].BirdsTenacity:=
           Random(BirdBrainArray[k].BirdsTenacity);
     boidArray[k, i].BirdsFerocity:=
           Random(BirdBrainArray[k].BirdsFerocity);
    end;
  end;
  BirdLanded := False;
end;

procedure TAAABirdForm.Timer1Timer(Sender: TObject);
begin   { Display the FrameRate }
  if FPSCB.Checked then
  begin
   StatusBar1.Panels[0].Text:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
  end;
end;


procedure TAAABirdForm.GLCadencer1Progress(Sender: TObject;
          const deltaTime, newTime: Double);
var
  abc, ppp, kkk, K, J: Integer;
  tx,ty,tz: Double;
begin
  if BirdLanded then
  begin
    StatusBar1.Panels[0].Text :='broken';
    StatusBar1.Panels[2].Text :='should not be possible';
    Timer1.Enabled := False;
    GLCadencer1.Enabled := False;
    Exit;{should not be possible}
  end;
        kkk := 0;
        ppp := 0;
        abc := 0;
        BoidFeeding:=0;
         {BirdBrainArray[WorkingFrenzy].  boidArray[K,J]}
        for K := 0 to FrenzyCount - 1 do
        begin
          WorkingFrenzy := K;
          BirdBrainArray[K].RealCenter.x := 0;
          BirdBrainArray[K].RealCenter.y := 0;
          BirdBrainArray[K].RealCenter.z := 0;
          BirdBrainArray[K].RealAvgvel.x := 0;
          BirdBrainArray[K].RealAvgvel.y := 0;
          BirdBrainArray[K].RealAvgvel.z := 0;
          If ChaseIt then
          begin
            BoidSeek;  {Panels 0 is FPS}
            Hunter.BirdBody.Position.x:= Hunter.pos.x;
            Hunter.BirdBody.Position.y:= Hunter.pos.y;
            Hunter.BirdBody.Position.z:= Hunter.pos.z;
          end;
          for J := 0 to (BirdBrainArray[K].BirdsCount - 1) do
          begin
            if (boidArray[K, J].Hunted = False) then inc(kkk) else
            if ((boidArray[K, J].Hungry = True)) then inc(ppp);
            if ((boidArray[K, J].Hungry = False)
             and(boidArray[K, J].Hunted = True)) then
            begin
              inc(abc);
              {vecadd(RealCenter, boid[J].pos);}
              BirdBrainArray[K].RealCenter.x :=
                BirdBrainArray[K].RealCenter.x + boidArray[K, J].pos.x;
              BirdBrainArray[K].RealCenter.y :=
                BirdBrainArray[K].RealCenter.y + boidArray[K, J].pos.y;
              BirdBrainArray[K].RealCenter.z :=
                BirdBrainArray[K].RealCenter.z + boidArray[K, J].pos.z;
              {vecadd(RealAvgvel, boid[J].vel); }
              BirdBrainArray[K].RealAvgvel.x :=
                BirdBrainArray[K].RealAvgvel.x + boidArray[K, J].vel.x;
              BirdBrainArray[K].RealAvgvel.y :=
                BirdBrainArray[K].RealAvgvel.y + boidArray[K, J].vel.y;
              BirdBrainArray[K].RealAvgvel.z :=
                BirdBrainArray[K].RealAvgvel.z + boidArray[K, J].vel.z;
            end;
          end;
          {Divide Flock Center by Bird count}
          BirdBrainArray[K].RealCenter.x :=
            (BirdBrainArray[K].RealCenter.x
            / BirdBrainArray[K].BirdsCount);
          BirdBrainArray[K].RealCenter.y :=
            (BirdBrainArray[K].RealCenter.y
            / BirdBrainArray[K].BirdsCount);
          BirdBrainArray[K].RealCenter.z :=
            (BirdBrainArray[K].RealCenter.z
            / BirdBrainArray[K].BirdsCount);
          BirdBrainArray[K].RealAvgvel.x :=
            (BirdBrainArray[K].RealAvgvel.x
             / BirdBrainArray[K].BirdsCount);
          BirdBrainArray[K].RealAvgvel.y :=
            (BirdBrainArray[K].RealAvgvel.y
             / BirdBrainArray[K].BirdsCount);
          BirdBrainArray[K].RealAvgvel.z :=
            (BirdBrainArray[K].RealAvgvel.z
             / BirdBrainArray[K].BirdsCount);
          for J := 0 to (BirdBrainArray[K].BirdsCount - 1) do
          begin
            {Get Before change}
            tx := boidArray[K, J].pos.x;
            ty := boidArray[K, J].pos.y;
            DXComputeNewHeading(J,deltaTime);

            with BirdProxies[K, J] do
            begin
              tx := ((boidArray[K, J].pos.x)-(tx) );
              ty := ((boidArray[K, J].pos.y)-(ty));
              tz:= RadToDeg(arctan2(ty,tx));
              If BirdDirectorCB.Checked then RollAngle :=tz;
              Position.X:=boidArray[K, J].pos.x;
              Position.Y:=boidArray[K, J].pos.y;
              Position.Z:=boidArray[K, J].pos.z;
            end;
          end;
        end; {K}
        if (CrashCount > 0) then
          StatusBar1.Panels[1].Text := 'Crashed: '+ inttostr(CrashCount)
          else StatusBar1.Panels[1].Text:='Hungry: '+inttostr(BoidFeeding);
                                          {floattostr(deltaTime);}
          StatusBar1.Panels[2].Text := 'Dead: '+ inttostr(kkk)
                              +' Feeding: '+ inttostr(ppp)
                              + ' Flying: '+inttostr(abc);
   {all birds dead so quit}
   if (FrenzyBirdCount = kkk) then
   begin
   BirdLanded := True;
   StatusBar1.Panels[1].Text := 'All kkkilled'+inttostr(FrenzyBirdCount);
   end else
  if (CrashCount > 10) then begin
   BirdLanded := True;
   StatusBar1.Panels[1].Text := 'All Crashed';
   end;
   GLSceneViewer1.Invalidate;
end;

// Compute the heading for a particular boid
// based on its current environment.
procedure TAAABirdForm.DXComputeNewHeading(WhichBoid: Integer;deltaTime:Double);
var
  randomSign, i, j, k, numcent: Integer;
  mindist, mz, mx, my, d,a,
  cosangle, cosvangle, costemp,
  xtemp, ytemp, ztemp, maxr, u, v,
  xa, ya, za, xb, yb, zb, xc, yc, zc, xd, yd, zd, xt, yt, zt: double;
begin
  if ((boidArray[WorkingFrenzy, WhichBoid].Hungry) and
      (boidArray[WorkingFrenzy, WhichBoid].Hungrytimer > 0)) then
      dec(boidArray[WorkingFrenzy, WhichBoid].Hungrytimer) else
  begin
    If DisplayObstaclesCB.Checked then
       BoidCheckCollision(WhichBoid); {Changes Positions}
    randomSign:= 1;
    If (Random > 0.5) then randomSign:= -1;
    {Determine if the bird is gonna be HUNGRY}
    boidArray[WorkingFrenzy, WhichBoid].Hungry := False;
    If boidArray[WorkingFrenzy, WhichBoid].BirdsActivity > 5 then
      boidArray[WorkingFrenzy, WhichBoid].BirdsActivity:=
         boidArray[WorkingFrenzy, WhichBoid].BirdsActivity-
         (BirdBrainArray[WorkingFrenzy].dBirdsFeed )
      else
      begin
        inc(BoidFeeding); {Set to Zero before each Flock Flight}
        BoidFeed(WhichBoid); {Creates a Feeding.x target...}
        boidArray[WorkingFrenzy, WhichBoid].vel.x :=Feeding.x;
        boidArray[WorkingFrenzy, WhichBoid].vel.y :=Feeding.y;
        boidArray[WorkingFrenzy, WhichBoid].vel.z :=Feeding.z;
      end;

    numcent := 0;
    mx := 0;
    my := 0;
    mz := 0;
    // This is the maximum distance in which any rule is activated.
    //BirdsRandomosity ?
    maxr := MAX(BirdBrainArray[WorkingFrenzy].RVisual,
              MAX(BirdBrainArray[WorkingFrenzy].RCopy,
                  MAX(BirdBrainArray[WorkingFrenzy].RCentroid,
                      BirdBrainArray[WorkingFrenzy].RAvoid)));

    // These two values are used to see if a boid can "see" another
    // boid in various ways.
    cosangle := cos(BirdBrainArray[WorkingFrenzy].dBirdsView / 2);
    cosvangle := cos(BirdBrainArray[WorkingFrenzy].dBirdsAvoid / 2);

    //* These are the accumulated change vectors for the four rules. */
    xa := 0; ya := 0; za := 0;
    xb := 0; yb := 0; zb := 0;
    xc := 0; yc := 0; zc := 0;
    xd := 0; yd:= 0;  zd := 0;

    //* For every boid...
    for i := 0 to BirdBrainArray[WorkingFrenzy].BirdsCount - 1 do
    begin
      //Move to Flock Centroid for ALL even Self
      // This can be 0 change if Flock set to 0
      a:=random
       * (BirdBrainArray[WorkingFrenzy].RealCenter.x
       - boidArray[WorkingFrenzy, WhichBoid].pos.x);
      boidArray[WorkingFrenzy, WhichBoid].pos.x:=
      boidArray[WorkingFrenzy, WhichBoid].pos.x+
      (BirdBrainArray[WorkingFrenzy].dBirdsFlock *a);
      a:=random
       * (BirdBrainArray[WorkingFrenzy].RealCenter.y
      - boidArray[WorkingFrenzy, WhichBoid].pos.y);
      boidArray[WorkingFrenzy, WhichBoid].pos.y:=
      boidArray[WorkingFrenzy, WhichBoid].pos.y+
      (BirdBrainArray[WorkingFrenzy].dBirdsFlock *a);
      a:=random
       * (BirdBrainArray[WorkingFrenzy].RealCenter.z
       - boidArray[WorkingFrenzy, WhichBoid].pos.z);
      boidArray[WorkingFrenzy, WhichBoid].pos.z:=
      boidArray[WorkingFrenzy, WhichBoid].pos.z+
      (BirdBrainArray[WorkingFrenzy].dBirdsFlock *a);
      a:=random
       * (BirdBrainArray[WorkingFrenzy].RealAvgvel.x
       -boidArray[WorkingFrenzy, WhichBoid].vel.x);
      boidArray[WorkingFrenzy, WhichBoid].vel.x:=
      boidArray[WorkingFrenzy, WhichBoid].vel.x+
      (BirdBrainArray[WorkingFrenzy].dBirdsFlock *a);
      a:=random
       * (BirdBrainArray[WorkingFrenzy].RealAvgvel.y
       -boidArray[WorkingFrenzy, WhichBoid].vel.y);
      boidArray[WorkingFrenzy, WhichBoid].vel.y:=
      boidArray[WorkingFrenzy, WhichBoid].vel.y+
      (BirdBrainArray[WorkingFrenzy].dBirdsFlock *a);
      a:=random
       * (BirdBrainArray[WorkingFrenzy].RealAvgvel.z
       -boidArray[WorkingFrenzy, WhichBoid].vel.z);
      boidArray[WorkingFrenzy, WhichBoid].vel.z:=
      boidArray[WorkingFrenzy, WhichBoid].vel.z+
      (BirdBrainArray[WorkingFrenzy].dBirdsFlock *a);

      //* Don't include self for computing new heading. */
      if (i = WhichBoid) then continue;
      mindist := 10E10;
      begin
        d :=
        DISTd3(boidArray[WorkingFrenzy, i].pos.x,
               boidArray[WorkingFrenzy, i].pos.y,
               boidArray[WorkingFrenzy, i].pos.z,
               boidArray[WorkingFrenzy, WhichBoid].pos.x,
               boidArray[WorkingFrenzy, WhichBoid].pos.y,
               boidArray[WorkingFrenzy, WhichBoid].pos.z);
        if (d < mindist) then
        begin
          mindist := d;
          mx := boidArray[WorkingFrenzy, i].pos.x;
          my := boidArray[WorkingFrenzy, i].pos.y;
          mz := boidArray[WorkingFrenzy, i].pos.z;
        end;
      end;

      // If that distance is farther than any of the rule radii,
      // then skip.
      if (mindist > maxr) then continue;

      //* Make a vector from boid(which) to boid(i).
      xtemp := mx - boidArray[WorkingFrenzy, WhichBoid].pos.x;
      ytemp := my - boidArray[WorkingFrenzy, WhichBoid].pos.y;
      ztemp := mz - boidArray[WorkingFrenzy, WhichBoid].pos.z;
      if (ztemp <> 0) then ;{comment quiet}
      //* Calculate the cosine of the velocity vector of boid(which)
      //* and the vector from boid(which) to boid(i).
      costemp := DOTd3(boidArray[WorkingFrenzy, WhichBoid].vel.x,
                       boidArray[WorkingFrenzy, WhichBoid].vel.y,
                       boidArray[WorkingFrenzy, WhichBoid].vel.z,
                       xtemp, ytemp, ztemp)
                 / (LENd3(boidArray[WorkingFrenzy, WhichBoid].vel.x,
                          boidArray[WorkingFrenzy, WhichBoid].vel.y,
                          boidArray[WorkingFrenzy, WhichBoid].vel.z)
                * LENd3(xtemp, ytemp, ztemp));

      //* If this cosine is less than the cosine of one half
      // * of the boid's eyesight, i.e., boid(which) cannot see
      //  * boid(i), then skip.
      if (costemp < cosangle) then continue;
      //* If the distance between the two boids is within the radius
      //* of the centering rule, but outside of the radius of the
      //* avoidance rule, then attempt to center in on boid(i).
      if (   (mindist <= BirdBrainArray[WorkingFrenzy].RCentroid)
         and (mindist > (BirdBrainArray[WorkingFrenzy].RAvoid
                       -(boidArray[WorkingFrenzy, WhichBoid].BirdsTenacity
                         /(BirdBrainArray[WorkingFrenzy].BirdsRandomosity)) ) ) )then
      begin
        xa := xa + mx - boidArray[WorkingFrenzy, WhichBoid].pos.x;
        ya := ya + my - boidArray[WorkingFrenzy, WhichBoid].pos.y;
        za := za + mz - boidArray[WorkingFrenzy, WhichBoid].pos.z;
        inc(numcent);
      end;

      //* If we are close enough to copy, but far enough to avoid,
      //* then copy boid(i)'s velocity.
      if ((mindist <= BirdBrainArray[WorkingFrenzy].RCopy)
        and (mindist > (BirdBrainArray[WorkingFrenzy].RAvoid
          -  (boidArray[WorkingFrenzy, WhichBoid].BirdsTenacity
              /(BirdBrainArray[WorkingFrenzy].BirdsRandomosity))) )) then
      begin
        xb := xb + boidArray[WorkingFrenzy, WhichBoid].vel.x;
        yb := yb + boidArray[WorkingFrenzy, WhichBoid].vel.y;
        zb := zb + boidArray[WorkingFrenzy, WhichBoid].vel.z;
      end;

      //* If we are within collision range, then try to avoid boid(i).
      if (mindist <= (BirdBrainArray[WorkingFrenzy].RAvoid
                     - boidArray[WorkingFrenzy, WhichBoid].BirdsTenacity)) then
      begin
        //Calculate the vector which moves boid(which) away from boid(i).
        xtemp := boidArray[WorkingFrenzy, WhichBoid].pos.x - mx;
        ytemp := boidArray[WorkingFrenzy, WhichBoid].pos.y - my;
        ztemp := boidArray[WorkingFrenzy, WhichBoid].pos.z - mz;
        //Make the length of the avoidance vector inversely proportional
        //to the distance between the two boids.
        d := 1 / LENd3(xtemp,ytemp, ztemp);{Z or Y}
        xtemp := xtemp * d;
        ytemp := ytemp * d;
        ztemp := ztemp * d;
        xc := xc + xtemp;
        yc := yc + ytemp;
        zc := zc + ztemp;
      end;

//If boid(i) is within rviso distance and the angle between this boid's
//velocity vector and the boid(i)'s position relative to this boid is
//less than vangle, then try to move so that vision is restored.
//This creates V flocks rather than all in a line
      if ( (mindist <= (BirdBrainArray[WorkingFrenzy].RVisual
                   -(boidArray[WorkingFrenzy, WhichBoid].BirdsFerocity
                   /BirdBrainArray[WorkingFrenzy].BirdsRandomosity)))
           and (cosvangle < costemp) ) then
      begin
        //Calculate the vector which moves boid(which) away from boid(i)
        xtemp := boidArray[WorkingFrenzy, WhichBoid].pos.x - mx;
        ytemp := boidArray[WorkingFrenzy, WhichBoid].pos.y - my;
        ztemp := boidArray[WorkingFrenzy, WhichBoid].pos.z - mz;
        //Calculate another vector that is orthogonal to the previous,
        //But try to make it in the same general direction of
        //boid(which)'s direction of movement.
        u := 0;
        v := 0;          if (ztemp <> 0) then ;
        if  ((xtemp <> 0) and (ytemp <> 0)) then
        begin {Gee do i need more 3D math here?}
          u := sqrt(SQR(ytemp / xtemp)
               / (1 + SQR(ytemp / xtemp)));
          v := -xtemp * u / ytemp;
        end
        else if (xtemp <> 0) then u := 1
        else if (ytemp <> 0) then v := 1;
        if ((boidArray[WorkingFrenzy, WhichBoid].vel.x* u
            + boidArray[WorkingFrenzy, WhichBoid].vel.y * v) < 0) then
        begin
          u := -u;
          v := -v;
        end;
        // Add the vector that moves away from boid(i). */
        u :=  boidArray[WorkingFrenzy, WhichBoid].pos.x - mx + u;
        v :=  boidArray[WorkingFrenzy, WhichBoid].pos.y - my + v;
        {v :=  boidArray[WorkingFrenzy, WhichBoid].pos.z - mz + v;}
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
        {zd := zd + v;}
      end; {Visually avoid}
    end; {All birds}

    //* Avoid centering on only one other boid;
    //* it makes you look aggressive!
    if (numcent < 2) then
    begin
      xa := 0;
      ya := 0;
      za := 0;
    end;
    //* Normalize all big vectors. */
    if (LENd3(xa, ya, za) > 1.0) then
    begin
      NormDX3(xa, ya, za); {norm(&xa, &ya);}
      xa := dBirdxTempDX;    ya := dBirdyTempDX; za :=dBirdzTempDX;
    end;
    if (LENd3(xb, yb, zb) > 1.0) then
    begin
      NormDX3(xb, yb, zb); {norm(&xb, &yb);}
      xb := dBirdxTempDX; yb := dBirdyTempDX; zb :=dBirdzTempDX;
    end;
    if (LENd3(xc, yc, zc) > 1.0) then
    begin
      NormDX3(xc, yc, zc); {norm(&xc, &yc);}
      xc := dBirdxTempDX; yc := dBirdyTempDX; zc :=dBirdzTempDX;
    end;
    if (LENd3(xd, yd, zd) > 1.0) then
    begin
      NormDX3(xd, yd, zd);  {norm(&xd, &yd); }
      xd := dBirdxTempDX; yd := dBirdyTempDX;  zd :=dBirdzTempDX;
    end;

    //* Compute the composite trajectory based on all of the rules. */
    xt := xa * BirdBrainArray[WorkingFrenzy].dWCentroid
        + xb * BirdBrainArray[WorkingFrenzy].dWCopy
        + xc * BirdBrainArray[WorkingFrenzy].dWAvoid
        + xd * BirdBrainArray[WorkingFrenzy].dWVisual
        {+deltaTime};

    yt := ya * BirdBrainArray[WorkingFrenzy].dWCentroid
        + yb * BirdBrainArray[WorkingFrenzy].dWCopy
        + yc * BirdBrainArray[WorkingFrenzy].dWAvoid
        + yd * BirdBrainArray[WorkingFrenzy].dWVisual
        {+deltaTime};

    zt := za * BirdBrainArray[WorkingFrenzy].dWCentroid
        + zb * BirdBrainArray[WorkingFrenzy].dWCopy
        + zc * BirdBrainArray[WorkingFrenzy].dWAvoid
        + zd * BirdBrainArray[WorkingFrenzy].dWVisual
        {+deltaTime};
    //add some noise
{    begin
      xt := (xt+(randomSign * BirdBrainArray[WorkingFrenzy].dBirdsRandomosity));
      yt := (yt+(randomSign * BirdBrainArray[WorkingFrenzy].dBirdsRandomosity));
      zt := (zt+(randomSign * BirdBrainArray[WorkingFrenzy].dBirdsRandomosity));
    end;}

    //* Update the velocity and renormalize if it is too small.
    boidArray[WorkingFrenzy, WhichBoid].posn.x :=
      boidArray[WorkingFrenzy, WhichBoid].vel.x
      * BirdBrainArray[WorkingFrenzy].dBirdsMoment
      + xt * (1 - BirdBrainArray[WorkingFrenzy].dBirdsMoment);

    boidArray[WorkingFrenzy, WhichBoid].posn.y :=
      boidArray[WorkingFrenzy, WhichBoid].vel.y
      * BirdBrainArray[WorkingFrenzy].dBirdsMoment
      + yt * (1 - BirdBrainArray[WorkingFrenzy].dBirdsMoment);

    boidArray[WorkingFrenzy, WhichBoid].posn.z :=
      boidArray[WorkingFrenzy, WhichBoid].vel.z
      * BirdBrainArray[WorkingFrenzy].dBirdsMoment
      + zt * (1 - BirdBrainArray[WorkingFrenzy].dBirdsMoment);

    d := LENd3(boidArray[WorkingFrenzy, WhichBoid].posn.x,
               boidArray[WorkingFrenzy, WhichBoid].posn.y,
               boidArray[WorkingFrenzy, WhichBoid].posn.z);
    if (d < BirdBrainArray[WorkingFrenzy].dBirdsMinV) then
    begin  {dBirdsGlide randomly +or- else birds will go into a corner}
      boidArray[WorkingFrenzy, WhichBoid].posn.x :=
        (boidArray[WorkingFrenzy, WhichBoid].posn.x *
        (BirdBrainArray[WorkingFrenzy].dBirdsMinV / d))
        +((randomSign *BirdBrainArray[WorkingFrenzy].dBirdsGlide ));
      boidArray[WorkingFrenzy, WhichBoid].posn.y :=
        (boidArray[WorkingFrenzy, WhichBoid].posn.y *
        (BirdBrainArray[WorkingFrenzy].dBirdsMinV / d))
        +((randomSign *BirdBrainArray[WorkingFrenzy].dBirdsGlide ));
      boidArray[WorkingFrenzy, WhichBoid].posn.z :=
        (boidArray[WorkingFrenzy, WhichBoid].posn.z *
        (BirdBrainArray[WorkingFrenzy].dBirdsMinV / d))
        +((randomSign *BirdBrainArray[WorkingFrenzy].dBirdsGlide ));
    end else
    if (d > BirdBrainArray[WorkingFrenzy].dBirdsSpeed) then
    begin
      boidArray[WorkingFrenzy, WhichBoid].posn.x :=
        boidArray[WorkingFrenzy, WhichBoid].posn.x *
        (BirdBrainArray[WorkingFrenzy].dBirdsSpeed / d);
      boidArray[WorkingFrenzy, WhichBoid].posn.y :=
        boidArray[WorkingFrenzy, WhichBoid].posn.y *
        (BirdBrainArray[WorkingFrenzy].dBirdsSpeed / d);
      boidArray[WorkingFrenzy, WhichBoid].posn.z :=
        boidArray[WorkingFrenzy, WhichBoid].posn.z *
        (BirdBrainArray[WorkingFrenzy].dBirdsSpeed / d);
    end;

    boidArray[WorkingFrenzy, WhichBoid].vel.x:=
            boidArray[WorkingFrenzy, WhichBoid].posn.x;
    boidArray[WorkingFrenzy, WhichBoid].pos.x:=
            boidArray[WorkingFrenzy, WhichBoid].pos.x
            +(boidArray[WorkingFrenzy, WhichBoid].vel.x);
    boidArray[WorkingFrenzy, WhichBoid].vel.y:=
            boidArray[WorkingFrenzy, WhichBoid].posn.y;
    boidArray[WorkingFrenzy, WhichBoid].pos.y:=
            boidArray[WorkingFrenzy, WhichBoid].pos.y
            +(boidArray[WorkingFrenzy, WhichBoid].vel.y);
    boidArray[WorkingFrenzy, WhichBoid].vel.z:=
           boidArray[WorkingFrenzy, WhichBoid].posn.z;
    boidArray[WorkingFrenzy, WhichBoid].pos.z:=
            boidArray[WorkingFrenzy, WhichBoid].pos.z
            + (boidArray[WorkingFrenzy, WhichBoid].vel.z);


    {bound world  Random makes it a 'soft' edge
     if still over next time it will again be randomly reversed
     back into the viewing area...
      could use * -1 thus is reversed BY the CURRENT Speed
      Negative become positive .. Positive become negative
       else use + 1 or -1}
       {BirdYardWide    BirdYardHeight to Flip a Torus}
    if (boidArray[WorkingFrenzy, WhichBoid].pos.x < -BirdYardWide)
    then begin{ boidArray[WorkingFrenzy, WhichBoid].pos.x :=BirdYardWide}
          boidArray[WorkingFrenzy, WhichBoid].vel.x :=
          boidArray[WorkingFrenzy, WhichBoid].vel.x +random;
          end
    else
    if (boidArray[WorkingFrenzy, WhichBoid].pos.x > BirdYardWide)
    then begin {boidArray[WorkingFrenzy, WhichBoid].pos.x := -BirdYardWide;}
            boidArray[WorkingFrenzy, WhichBoid].vel.x :=
            boidArray[WorkingFrenzy, WhichBoid].vel.x -random;
         end;

    if (boidArray[WorkingFrenzy, WhichBoid].pos.y < -BirdYardHeight)
    then {boidArray[WorkingFrenzy, WhichBoid].pos.y := BirdYardHeight}
          boidArray[WorkingFrenzy, WhichBoid].vel.y :=
          boidArray[WorkingFrenzy, WhichBoid].vel.y +random
    else
    if (boidArray[WorkingFrenzy, WhichBoid].pos.y > BirdYardHeight)
    then {boidArray[WorkingFrenzy, WhichBoid].pos.y := -BirdYardHeight;}
            boidArray[WorkingFrenzy, WhichBoid].vel.y :=
            boidArray[WorkingFrenzy, WhichBoid].vel.y  -random;

    if (boidArray[WorkingFrenzy, WhichBoid].pos.z < -BirdsYardDepth)
    then  {boidArray[WorkingFrenzy, WhichBoid].pos.z := BirdYardWide}
        boidArray[WorkingFrenzy, WhichBoid].vel.z :=
          boidArray[WorkingFrenzy, WhichBoid].vel.z +random
    else                             {BirdsHeightGround is already Negative}
    if (boidArray[WorkingFrenzy, WhichBoid].pos.z > (BirdsYardDepth))
    then {boidArray[WorkingFrenzy, WhichBoid].pos.z := (-BirdYardWide);}
          boidArray[WorkingFrenzy, WhichBoid].vel.z :=
            boidArray[WorkingFrenzy, WhichBoid].vel.z  -random;
  end;
end;


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
procedure TAAABirdForm.BoidFeed(WhichBoid: Integer);
var
  DesiredVelocity, TargetOffset: Vec;
  RampedSpeed, ClippedSpeed,
    dm, dx, dy, dz: Double;
begin
{Arrival    TargetOffset = target - position}
  TargetOffset.x := Feeder.Position.x -
    boidArray[WorkingFrenzy,WhichBoid].pos.x;
  TargetOffset.y := Feeder.Position.y -
    boidArray[WorkingFrenzy,WhichBoid].pos.y;
  TargetOffset.z := Feeder.Position.z -
    boidArray[WorkingFrenzy,WhichBoid].pos.z;

{    distance = length (target_offset)}
  dx := TargetOffset.x;
  dy := TargetOffset.y;
  dz := TargetOffset.z;
  if (abs(dx)) > (abs(dy)) then dm := abs(dx) else dm := abs(dy);
  if (abs(dz)) > (dm) then dm := abs(dz);
  if ((abs(dx) < 2) and (abs(dy) < 2) and (abs(dz) < 2)) then
  begin
{      boidArray[WorkingFrenzy,WhichBoid].pos.y := BirdsHeightDX; }{/* Hit ground!! */}
    boidArray[WorkingFrenzy, WhichBoid].Hungry := True;
    boidArray[WorkingFrenzy, WhichBoid].Hungrytimer :=
      ({Random(200)+} {100*}
      BirdBrainArray[WorkingFrenzy].BirdsActivity);
    {boidArray[WorkingFrenzy, WhichBoid].vel.y := 0;}
         {Every time it eats it gets sexier... so what}
         boidArray[WorkingFrenzy, WhichBoid].dBirdsSexy:=
           boidArray[WorkingFrenzy, WhichBoid].dBirdsSexy
           + BirdBrainArray[WorkingFrenzy].dBirdsSexy;
        If  (boidArray[WorkingFrenzy, WhichBoid].dBirdsSexy>
            BirdBrainArray[WorkingFrenzy].dBirdsEnergy) then
        begin
        boidArray[WorkingFrenzy, WhichBoid].dBirdsSexy:=0;
         {Create a New Bird}
          BirdsMaker(BirdBrainArray[WorkingFrenzy].BirdsCount+1);
        end;
       boidArray[WorkingFrenzy, WhichBoid].BirdsActivity:=
       (BirdBrainArray[WorkingFrenzy].BirdsActivity +
       (Random(BirdBrainArray[WorkingFrenzy].BirdsActivity)*
       Random(BirdBrainArray[WorkingFrenzy].BirdsActivity)));
  end else
  begin
{    RampedSpeed = max_speed * (distance / slowing_distance)}
    RampedSpeed := BirdBrainArray[WorkingFrenzy].dBirdsSpeed
      * (dm / (BirdBrainArray[WorkingFrenzy].BirdsSize
      *BirdBrainArray[WorkingFrenzy].dBirdsFeed ));

{    if (RampedSpeed > BirdBrainArray[WorkingFrenzy].dBirdsSpeed)
      then
      ClippedSpeed := BirdBrainArray[WorkingFrenzy].dBirdsSpeed
    else} ClippedSpeed := RampedSpeed;
{    DesiredVelocity = (clipped_speed / distance) * TargetOffset}
    DesiredVelocity.x := (ClippedSpeed / dm) * TargetOffset.x;
    DesiredVelocity.y := (ClippedSpeed / dm) * TargetOffset.y;
    DesiredVelocity.z := (ClippedSpeed / dm) * TargetOffset.z;

{    steering = desired_velocity - velocity}
    Feeding.x := DesiredVelocity.x -
        boidArray[WorkingFrenzy,WhichBoid].vel.x;
    Feeding.y := DesiredVelocity.y - boidArray[WorkingFrenzy,
      WhichBoid].vel.y;
    Feeding.z := DesiredVelocity.z -
    boidArray[WorkingFrenzy, WhichBoid].vel.z;

    Feeding.x := ((Feeding.x) {+ BirdBrainArray[WorkingFrenzy].dBirdsFeed} );
    Feeding.y := ((Feeding.y) {+ BirdBrainArray[WorkingFrenzy].dBirdsFeed} );
    Feeding.z := ((Feeding.z) {+ BirdBrainArray[WorkingFrenzy].dBirdsFeed} );
  end;
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
procedure TAAABirdForm.BoidCheckCollision(WhichBoid: Integer);
var
  randomSign,i: Integer;
  mindist,d: double;
begin
    randomSign:= 1;
    If (Random > 0.5) then randomSign:= -1;
    mindist := 10E10;
    for i := 0 to ObstacleCount - 1 do
    begin
      d :=
        DISTd3(ObstacleArray[i].X,
               ObstacleArray[i].y,
               ObstacleArray[i].z,
               boidArray[WorkingFrenzy, WhichBoid].pos.x,
               boidArray[WorkingFrenzy, WhichBoid].pos.y,
               boidArray[WorkingFrenzy, WhichBoid].pos.z);
      if (d < mindist) then mindist := d;
    end;
    If mindist < 3 then
    begin
       boidArray[WorkingFrenzy, WhichBoid].vel.x :=
          boidArray[WorkingFrenzy, WhichBoid].vel.x  -(random*randomSign);
       boidArray[WorkingFrenzy, WhichBoid].vel.y :=
          boidArray[WorkingFrenzy, WhichBoid].vel.y  -(random*randomSign);
       boidArray[WorkingFrenzy, WhichBoid].vel.z :=
          boidArray[WorkingFrenzy, WhichBoid].vel.z  -(random*randomSign);
    end;
end;

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

procedure TAAABirdForm.BoidSeek;
var
  DesiredVelocity, TargetOffset: Vec;
  m,zf, f, dx, dy, dz: Double;
begin
  {Get a bird... goofy way but it works... Not much of a selection}
    {desired_velocity = normalize (position - target) * max_speed
    steering = desired_velocity - velocity}
  if ((CurrentSeekerK > -1) and (CurrentSeekerJ > -1)) then
  begin

    {Chase the bird...>>>Seek}
    TargetOffset.x :=
      boidArray[CurrentSeekerK, CurrentSeekerJ].pos.x - Hunter.pos.x;
    TargetOffset.y :=
      boidArray[CurrentSeekerK, CurrentSeekerJ].pos.y - Hunter.pos.y;
    TargetOffset.z :=
      boidArray[CurrentSeekerK, CurrentSeekerJ].pos.z - Hunter.pos.z;
    TargetOffset.x := (Hunter.BirdSpeed/100) * TargetOffset.x;
    TargetOffset.y := (Hunter.BirdSpeed/100) * TargetOffset.y;
    TargetOffset.z := (Hunter.BirdSpeed/100) * TargetOffset.z;

    DesiredVelocity.x := TargetOffset.x + Hunter.vel.x;
    DesiredVelocity.y := TargetOffset.y + Hunter.vel.y;
    DesiredVelocity.z := TargetOffset.z + Hunter.vel.z;
    Hunter.vel.x := DesiredVelocity.x ;
    Hunter.vel.y := DesiredVelocity.y ;
    Hunter.vel.z := DesiredVelocity.z ;
    m := LENd3(Hunter.vel.x,Hunter.vel.y,Hunter.vel.z);
    if (m > (Hunter.BirdSpeed/100)) then
    begin
      f := (Hunter.BirdSpeed/100) / m;
      Hunter.vel.x := Hunter.vel.x * f;
      Hunter.vel.y := Hunter.vel.y * f;
      Hunter.vel.z := Hunter.vel.z * f;
    end;
    {Position...}
    Hunter.pos.x := Hunter.pos.x + Hunter.vel.x;
    Hunter.pos.y := Hunter.pos.y + Hunter.vel.y;
    Hunter.pos.z := Hunter.pos.z + Hunter.vel.z;

    {Check if caught}
    TargetOffset.x :=
        Hunter.pos.x - boidArray[CurrentSeekerK,CurrentSeekerJ].pos.x;
    TargetOffset.y :=
        Hunter.pos.y - boidArray[CurrentSeekerK,CurrentSeekerJ].pos.y;
    TargetOffset.z :=
        Hunter.pos.z - boidArray[CurrentSeekerK,CurrentSeekerJ].pos.z;
        {this is goofy - but it gets close}
    zf := {BirdYardWide} Hunter.BirdSize/100;
    dx := TargetOffset.x / zf;
    dy := TargetOffset.y / zf;
    dz := TargetOffset.z / zf;
    if ((abs(dx) < 1 )
        and (abs(dy) < 1 )
        and (abs(dz) < 1 )) then
    begin {Remove bird}
      ChaseIt := False;
      AttackBtn.Color := clBtnFace;
      Hunter.BirdBody.Visible:=ChaseIt;
      {Set false when GOT}
      boidArray[CurrentSeekerK, CurrentSeekerJ].Hunted := false;
      boidArray[CurrentSeekerK,CurrentSeekerJ].pos.x := {random(BirdYardWide);} BirdYardWide;
      boidArray[CurrentSeekerK,CurrentSeekerJ].pos.y := {random(BirdYardHeight);} BirdYardHeight;
      boidArray[CurrentSeekerK,CurrentSeekerJ].pos.z := BirdsYardDepth;
      boidArray[CurrentSeekerK,CurrentSeekerJ].vel.x := 0;
      boidArray[CurrentSeekerK,CurrentSeekerJ].vel.y := 0;
      boidArray[CurrentSeekerK,CurrentSeekerJ].vel.z := 0;
  If BirdFollowerCB.Checked then
  begin    {Counts DOWN}
  If (CurrentBirdFollowed=CurrentSeekerJ)then
  begin     {1}
    If (CurrentSeekerJ=1) then
    begin
    CurrentBirdFollowed:=0;
    BirdFollowerEdit.Text:= inttostr(CurrentBirdFollowed);
    GLCamera1.TargetObject:= GLDummyCube1;
    BirdFollowerCB.Checked:=False;
    end else
    begin
    dec(CurrentBirdFollowed);
    BirdFollowerEdit.Text:= inttostr(CurrentBirdFollowed);
    GLCamera1.TargetObject:= BirdProxies[CurrentFrenzy,CurrentBirdFollowed];
    end;
  end;
  end;
      Hunter.pos.x := Random((BirdYardWide) - BirdYardWide);
      Hunter.pos.y := Random((BirdYardHeight ) - BirdYardHeight);
      Hunter.pos.z := Random((BirdsYardDepth )) - (BirdsYardDepth);
      Hunter.vel.x := Random(51) - 25;
      Hunter.vel.y := Random(51) - 25; {up.. down}
      Hunter.vel.z := Random(51) - 25; {front to back}
    end; {all Caught}
  end; {Chaseit}
end;
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////





















end.
