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
{ This projects sets up a basic GLScene environment
  with mouse interaction:
    Drag = Rotate
    Shift + Vertical Drag = Zoom
    Control + Drag = Pan in XY plane
    Alt + Vertical Drag = Pan in Z
    F + Vertical Drag = Adjust camera focal length
    L + Vertical Drag = Adjust light intensity
    R = Reset scene
    W = Get Pixel data
    Q = Stop Processing cubes
    G = Grab a box of bytes
  You can either use it as a starting point for your projects
  or as a test bed to try out the really interesting stuff.

  Safak Cinar
  safak@shaw.ca
  http://members.shaw.ca/safak/}
{: from Basic particle system demo.
Scene Objects
|_Dummy Cube
  |_DirectOpenGL
  |_MainCam
  |_GLParticles
    |_Cube
_Light    }
Unit nlife3d;

Interface

Uses
  Windows, Messages, SysUtils, Classes,Buttons,
  Graphics, Controls, Forms, Dialogs,StdCtrls,
  ExtCtrls,ComCtrls,Menus,Math,
  OpenGL1x, GLCadencer, GLScene, GLObjects, GLParticles,
  GLWin32Viewer, XOpenGL, GLVectorFileObjects,
  GLGeometryBB, GLContext, GLCrossPlatform, GLVectorLists, GLCanvas,
  GLTexture, GLVectorGeometry, GLSkydome, GLGraph, GLMesh, GLSelection,
  GLAVIRecorder, GLVectorTypes, GLHUDObjects, GLCoordinates, GLBaseClasses;


Const
  //Default distance of the camera to the target object
  CamDistDef = 100;
  // MinMax values allowed when zooming
  CamDistMax = 5000;
  CamDistMin = 1;
  // MinMax values allowed when changing camera focus. Default is 100
  CamFocalMax = 2000;
  CamFocalMin = 5;

  //Cursor ID's
  ZoomCursor = 5;
  MoveXYCursor = 6;
  MoveZCursor = 7;
  RotateCursor = 8;
  FocusCursor = 9;
  LightCursor = 10;
  DigiCursor=11;

Type

  TViewerState = (vsRotate, vsZoom, vsPanXY, vsPanZ, vsFocus, vsLight, vsDigi, vsGrab);

  Talife3dForm = class(TForm)
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    DummyCube: TGLDummyCube;
    MainCam: TGLCamera;
    Light1: TGLLightSource;
    Cube1: TGLCube;
    aLifePanel: TPanel;
    NumberEdit: TEdit;
    HelpBtn: TSpeedButton;
    ExitBtn: TSpeedButton;
    LifeFileBtn: TSpeedButton;
    RunLifeBtn: TSpeedButton;
    StopLifeBtn: TSpeedButton;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    Size1: TMenuItem;
    lifesize10: TMenuItem;
    lifesize25: TMenuItem;
    lifesize50: TMenuItem;
    lifesize256: TMenuItem;
    SaveStartCB: TCheckBox;
    SaveEndCB: TCheckBox;
    UseSlowSteps1: TMenuItem;
    SaveDialog1: TSaveDialog;
    StatusBarred: TStatusBar;
    Torus1: TMenuItem;
    lifesize100: TMenuItem;
    lifesize200: TMenuItem;
    lifesize150: TMenuItem;
    lifesize512: TMenuItem;
    World1: TMenuItem;
    CellColorScheme1: TMenuItem;
    ColorTwoTone: TMenuItem;
    ColorAll: TMenuItem;
    ColorRGB1: TMenuItem;
    N1: TMenuItem;
    CellularRules1: TMenuItem;
    Fallover1: TMenuItem;
    Conway1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Run1: TMenuItem;
    N3: TMenuItem;
    StopLife1: TMenuItem;
    N4: TMenuItem;
    MaxRandomCellsEdit: TEdit;
    Universes: TMenuItem;
    aMinMaxPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RedMaxTrackBar: TTrackBar;
    GreenMaxTrackBar: TTrackBar;
    BlueMaxTrackBar: TTrackBar;
    RedMinTrackBar: TTrackBar;
    GreenMinTrackBar: TTrackBar;
    BlueMinTrackBar: TTrackBar;
    DisplayMinMaxBar1: TMenuItem;
    Viewliffiles1: TMenuItem;
    CellSizeTB: TTrackBar;
    Infinity1: TMenuItem;
    SizeLabel: TLabel;
    RunBtn: TSpeedButton;
    PauseStepBtn: TSpeedButton;
    lifesize300: TMenuItem;
    lifesize400: TMenuItem;
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject;
    Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
    procedure ViewerPostRender(Sender: TObject);
procedure MyGetPickedObject;

    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure StopLifeBtnClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
procedure ShowDown(HisFileName:String);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure HelpBtnClick(Sender: TObject);

procedure LoadLif(Filename:String);
procedure LoadLife(Filename:String; Sizer:Integer);
    procedure LifeFileBtnClick(Sender: TObject);
Function RunBooleanRules:string;    
procedure UniversaLife(FileTodo: string);
    procedure RunLifeBtnClick(Sender: TObject);
procedure GaiaS(FileTodo: string);


procedure AllSizesOff;
    procedure lifesize10Click(Sender: TObject);
    procedure lifesize25Click(Sender: TObject);
    procedure lifesize50Click(Sender: TObject);
    procedure lifesize100Click(Sender: TObject);
    procedure lifesize150Click(Sender: TObject);
    procedure lifesize200Click(Sender: TObject);
    procedure lifesize256Click(Sender: TObject);
    procedure lifesize512Click(Sender: TObject);

    procedure UseSlowSteps1Click(Sender: TObject);


procedure AllColorsOff;
    procedure ColorTwoToneClick(Sender: TObject);
    procedure ColorRGB1Click(Sender: TObject);
    procedure ColorAllClick(Sender: TObject);

procedure LifesEdgeOff;
    procedure Torus1Click(Sender: TObject);
    procedure Fallover1Click(Sender: TObject);

procedure CellularRulesOff;
    procedure Conway1Click(Sender: TObject);
    procedure UniversesClick(Sender: TObject);

    procedure RedMaxTrackBarChange(Sender: TObject);
    procedure RedMinTrackBarChange(Sender: TObject);
    procedure GreenMaxTrackBarChange(Sender: TObject);
    procedure GreenMinTrackBarChange(Sender: TObject);
    procedure BlueMaxTrackBarChange(Sender: TObject);
    procedure BlueMinTrackBarChange(Sender: TObject);
procedure SmashTheSliders;
    procedure DisplayMinMaxBar1Click(Sender: TObject);
    procedure Viewliffiles1Click(Sender: TObject);
    procedure CellSizeTBChange(Sender: TObject);
    procedure Infinity1Click(Sender: TObject);
    procedure RunBtnClick(Sender: TObject);
    procedure PauseStepBtnClick(Sender: TObject);
    procedure lifesize300Click(Sender: TObject);
    procedure lifesize400Click(Sender: TObject);

  Private
  {Life stuff}
  MouseSelRect:TGLRect;
  Pausing, UnPaused,
  IsSelecting:Boolean;
  IWannaQuit,  {Stop it}
  bSlowDown,
  bTorusLife, {Switch type of display}
  Busy:Boolean; {Interval stopper}
  CubeSizer:Double;
  RedXMax, RedXMin, GreenYMax,GreenYMin,BlueZMax, BlueZMin,
  LifeColorStyle,  TotalCounter:Integer;
  AAM,CLM: array of array of array of Byte;
  LifeCellMatrix: array of TGLCube;
    //This tells what we should do when the user drags the mouse,
    //based on the keys that are down
    ViewerState : TViewerState;
    //Ratio of camera target distance to focal length.
    //When changing focal length keep the ratio constant
    //so that the target object seems to stay at the same place
    //to the viewer
    CameraAspect : Double;
    //These track down the mouse events in the viewer
    MouseDown : Boolean;
    MouseX,MouseY : Integer;
    //The light follows the camera,
    //we have to call this whenever the camera position changes
    Procedure ReAdjustLightPosition;
    //Resets the camera such that it's
    //CamDistDef distance away from the target in the direction [X,Y,Z]
    //Magnitude of [X,Y,Z] is irrelevant
    Procedure ResetCamera(X,Y,Z:Double);
    //This proc handles changing the cursor
    //as well as switching the ViewerState.
    //Called from the form's keydown and keyup events
    Procedure SetGLCursor(Var Key:Word; Down:Boolean);
  Public

  End;

Var
  alife3dForm: Talife3dForm;
  BeforeaLifeMatrix: array of array of array of Byte;
  
Implementation

{$R *.DFM}
{$R Cursors.res}
uses nUGlobal, nLifeFiles, nlife3du;
{===============================================================}
{==========================================================
Check if a control (including all its parents) are visible }
Function IsControlShowing(C:TControl):Boolean;
Begin
  Result:=False;
  Try
    While Not (C is TForm) Do
          If Not C.Visible Then Exit Else C:=C.Parent;
  Except
    On Exception Do;
  End;
  Result:=True;
End;

{=============================================================
Returns true if a given screen coordinate (ie Mouse.CursorPos)
is over a given component (which must be showing)}
function PointOver(T:TGLPoint{TPoint}; C:TControl):Boolean;
Var
  T1,T2 : TGLPoint;
  TInX,TinY:Integer;
Begin
  TInX :=T.X;
  TinY :=T.Y;
  T1.X:=0;
  T1.Y:=0;
  T2.X:=C.Width;
  T2.Y:=C.Height;
  T1:=C.ClientToScreen(T1);
  T2:=C.ClientToScreen(T2);
  Result:=(IsControlShowing(C) And (TInX{T.X}>=T1.X)
           And (TInX{T.X}<=T2.X) And (TinY{T.Y}>=T1.Y) And (TinY{T.Y}<=T2.Y));
End;
{===============================================================}
Procedure Talife3dForm.FormCreate(Sender: TObject);
Var
  K : Word;
Begin
  top := life3dFormY;
  left := life3dFormX;
  SetLength(Universal,2,27);{0..1=2 , 0..26=27}
  NumberEdit.Text:= Inttostr(NumberEditStored);
  MaxRandomCellsEdit.Text:= Inttostr(MaxRandomCellsEditStored);
  TotalCounter:=0;
  CubeSizer:=1;
  Busy:=True;
  Pausing:=False;
  UnPaused:=False;
  IWannaQuit:=False;
  IsSelecting:=False;
  LifeSize:=9;
  bTorusLife:=False;
  LifeColorStyle:=0;
  CellularRulesStyle:=0;
  bSlowDown:=False;
  SmashTheSliders;
  //Load cursors
  Screen.Cursors[ZoomCursor] := LoadCursor(HInstance, 'ZoomCursor');
  Screen.Cursors[MoveXYCursor] := LoadCursor(HInstance, 'PanXYCursor');
  Screen.Cursors[MoveZCursor] := LoadCursor(HInstance, 'PanZCursor');
  Screen.Cursors[RotateCursor] := LoadCursor(HInstance, 'RotateCursor');
  Screen.Cursors[FocusCursor] := LoadCursor(HInstance, 'FocusCursor');
  Screen.Cursors[LightCursor] := LoadCursor(HInstance, 'LightCursor');
  Screen.Cursors[DigiCursor] := LoadCursor(HInstance, 'DigiCursor');

  //Reset the camera
  ResetCamera(1,1,1);
  //Set the cursors and the ViewerState
  K:=0;
  SetGlCursor(K,False);
End;

procedure Talife3dForm.ExitBtnClick(Sender: TObject);
begin
  Close;
end;
procedure Talife3dForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  IWannaQuit:=True;
  Application.ProcessMessages;
  CanClose :=True;
end;
procedure Talife3dForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SetLength(BeforeaLifeMatrix, 0, 0, 0);
  SetLength(AAM, 0, 0, 0);
  SetLength(CLM, 0, 0, 0);
  TotalCounter:=0;
  SetLength(LifeCellMatrix,0);  
  SetLength(Universal,0,0);
  NumberEditStored := strtoint(NumberEdit.Text);
  MaxRandomCellsEditStored := strtoint(MaxRandomCellsEdit.Text);
  life3dFormY := alife3dForm.top;
  life3dFormX := alife3dForm.left;
  if ReallyGone then Action := caFree else Action := caHide;
end;
{==============================================================}

procedure Talife3dForm.FormShow(Sender: TObject);
var
  v : Tvector;
begin
  Busy:=False;
  v:=MainCam.ScreenDeltaToVectorXY(0,-60,
             0.12*MainCam.DistanceToTarget/MainCam.FocalLength);
  {Dummy}Cube1.Position.Translate(v);
  MainCam.MoveAroundTarget(0, 20);
  ReAdjustLightPosition;
  MainCam.TransformationChanged;
  SetLength(BeforeaLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(AAM, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(CLM, LifeSize+1, LifeSize+1, LifeSize+1);
end;

procedure Talife3dForm.FormResize(Sender: TObject);
begin
   // change focal so the view will shrink and not just get clipped
{   MainCam.FocalLength:=50*Width/280;}   {was 200? >50 if width > 280}
end;


{====================================================================
The light follows the camera but at some distance away.
We don't want the light too close to the target object
when we zoom in.}
Procedure Talife3dForm.ReAdjustLightPosition;
Begin
  Light1.Position.X:=MainCam.Position.X*20;
  Light1.Position.Y:=MainCam.Position.Y*20;
  Light1.Position.Z:=MainCam.Position.Z*20;
End;

{=============================================================
Remember mouse position when it was clicked down }
Procedure Talife3dForm.ViewerMouseDown(Sender: TObject;
Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var
  pick : TGLCustomSceneObject;
  Value:Single;
  MyTVector4f:TVector4f;
  K : Word;
Begin
  Case ViewerState Of
    vsDigi : { if w pressed then}
    begin   {Pick an Object X Y}
    	// if an object is picked...
	    pick:=(Viewer.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
    	if Assigned(pick) then
      begin
        {RedFloater,GreenFloater,}
        MyTVector4f:=
		    pick.Material.FrontProperties.Emission.Color;
        Value:=pick.TagFloat;
       StatusBarred.Panels[1].Text :=
          'Red:'+Inttostr(Round(MyTVector4f.X*255))+
          ', Green:'+Inttostr(Round(MyTVector4f.Y*255))+
          ', Blue:'+Inttostr(Round(MyTVector4f.Z*255))+
          ', Life:'+Inttostr(Round(Value));
        IsSelecting := false;
        ViewerState:=vsRotate;
        K:=0;
        SetGlCursor(K,False);
        Viewer.Invalidate;
        Application.ProcessMessages;
	    end;
    end; {anything but nit picking}
    vsGrab:
    Begin
      {Retain Initial Mouse box}
    MouseSelRect.Left := X;
    MouseSelRect.Top :=Y;
    IsSelecting := True;
    End;
  end;{Case}
  begin
  MouseDown:=True;
  MouseX:=X;
  MouseY:=Y;
  end;
End;

{============================================================
Main routine                                     }
Procedure Talife3dForm.ViewerMouseMove(Sender: TObject;
Shift: TShiftState; X,Y: Integer);
Var
  dx, dy : Integer;
  v : TVector;
  d : Double;
Begin
  If Not MouseDown Then Exit;

  //How much the mouse has moved since last time
  dx:=MouseX-x; dy:=MouseY-y;

  Case ViewerState Of
    vsRotate :
      Begin
        //Rotate the camera around the target
        MainCam.MoveAroundTarget(dy, dx);
        ReAdjustLightPosition;
      End;
    vsZoom :
      Begin
        //0.01 is an arbitrary scale factor
        //Notice we first check if the mouse movement will result
        //in an allowable camera target distance
        //and if not, we clamp the value
        //before assigning it to camera.
        d:=(1-dy*0.01)*MainCam.DistanceToTarget;
        If d>CamDistMax Then d:=CamDistMax;
        If d<CamDistMin Then d:=CamDistMin;
        MainCam.AdjustDistanceToTarget(d/MainCam.DistanceToTarget);
        //We also update the CameraAspect
        //so that next time the focal length changes,
        //this new ratio is used
        CameraAspect:=MainCam.DistanceToTarget/MainCam.FocalLength;
        //Again, lights follow the camera
        ReAdjustLightPosition;
      End;
    vsPanXY :
      Begin
        //Convert the delta movement to GLScene translation,
        //correcting for the camera
        //target distance and the focal length
        v:=MainCam.ScreenDeltaToVectorXY(dx,-dy,
             0.12*MainCam.DistanceToTarget/MainCam.FocalLength);
    //Camera is actually a child of the dummycube (the target object).
    //This separates translation from rotation,
    //the former applied to the dummycube (and thus indirectly to the
    //camera as well), the latter to the camera only.
    //This way, instead of panning the scene,
    //we translate the dummycube+camera.
    //Such motion has side effects too. For example, the proper
    //place to attach a skydome would be as a child of the dummycube.
        Cube1{DummyCube}.Position.Translate(v);
        ReAdjustLightPosition;
        MainCam.TransformationChanged;
      End;
    vsPanZ :
      Begin
    //Same deal as above except that the motion is limited to one axis
       { v:=MainCam.ScreenDeltaToVectorXY(0,-dy,
             0.12*MainCam.DistanceToTarget/MainCam.FocalLength);}
        v.X:=0;
        v.Y:=0;
        v.Z:=-dy*0.12*MainCam.DistanceToTarget/MainCam.FocalLength;
        Cube1{DummyCube}.Position.Translate(v);
        ReAdjustLightPosition;
        MainCam.TransformationChanged;
      End;
    vsFocus :
      Begin
        //We first figure out what the new focal length would be
        d:=(1-dy*0.01)*MainCam.FocalLength;
        //then clamp it down based on the constraints.
        //Checking against CamDistMax/CameraAspect ..etc
        //at this stage makes sure we don't end up with
        //an illegal DistanceToTarget for the camera
        //since to keep the target in the same virtual location,
        //camera-target distance adjustment has
        //to immediately follow a focus adjustment
        If d>CamFocalMax Then d:=CamFocalMax;
        If d>CamDistMax/CameraAspect Then d:=CamDistMax/CameraAspect;
        If d<CamFocalMin Then d:=CamFocalMin;
        If d<CamDistMin/CameraAspect Then d:=CamDistMin/CameraAspect;
        MainCam.FocalLength:=d;
        MainCam.AdjustDistanceToTarget(MainCam.FocalLength
                               *CameraAspect/MainCam.DistanceToTarget);
      End;
    vsLight :
      Begin
        //Simply alter the light intensity.
        //Notice this time the change is additive
        d:=Light1.Diffuse.Red;
        d:=d+dy*0.001;
        If d>1 Then d:=1 Else If d<0 Then d:=0;
        Light1.Diffuse.Red:=d;
        Light1.Diffuse.Green:=d;
        Light1.Diffuse.Blue:=d;
      End;
    vsGrab:
      Begin
        if isSelecting then
        begin
          MouseSelRect.Right := x;
          MouseSelRect.Bottom := y;
          Viewer.Invalidate;
        end;
      End;
  End;

  //Reset the mouse position
  MouseX:=X;
  MouseY:=Y;
  StatusBarred.Panels[1].Text :=inttostr(Round(MainCam.FocalLength));
End;

procedure Talife3dForm.ViewerPostRender(Sender: TObject);
var
  GLCanv: TGLCanvas;
  x1, y1, x2, y2: integer;
begin
  if IsSelecting then {Draw a Moused Box - Selection Area}
  begin
    GLCanv := TGLCanvas.Create(Viewer.Width, Viewer.Height);
    GLCanv.PenColor := clWhite;
    x1 := MouseSelRect.Left;
    y1 := MouseSelRect.Top;
    x2 := MouseSelRect.Right;
    y2 := MouseSelRect.Bottom;
    GLCanv.FrameRect(x1, y1, x2, y2);
    GLCanv.Free;
  end;
end;
{==========================================================}
Procedure Talife3dForm.ViewerMouseUp(Sender: TObject;
Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var   K : Word;
Begin
  MouseDown:=False;
  If ViewerState = vsGrab then
  Begin
  IsSelecting := false;
  ViewerState:=vsRotate;
  K:=0;
  SetGlCursor(K,False);
  Viewer.Invalidate;
  MyGetPickedObject;
  End;
End;

// Fills the PickList with objects in Rect area
// Objects are sorted by depth (nearest objects first).
Procedure Talife3dForm.MyGetPickedObject;
{function (x, y : Integer) :TGLBaseSceneObject;}
var
   pkList : TGLPickList;
   PickCount:Integer;
begin
   PickCount:=64;
   pkList:=Viewer.Buffer.GetPickedObjects(MouseSelRect,PickCount);
   {Rect(x-1, y-1, x+1, y+1));}
   try
      if pkList.Count>0 then
      begin
      {Clear and Fill something}
      StatusBarred.Panels[1].Text :=
      (Inttostr(pkList.Count) + ' Objects found');
      end;
         {Result:=pkList.Hit[0] else Result:=nil;}
   finally
      pkList.Free;
   end;
end;
{=============================================================
Position the camera in it's default location..etc          }
Procedure Talife3dForm.ResetCamera(X,Y,Z:Double);
Var
  D : Double;
Begin
  D:=CamDistDef/Sqrt(Sqr(X)+Sqr(Y)+Sqr(Z));
  {Dummy}Cube1.Position.X:=0;
  {Dummy}Cube1.Position.Y:=0;
  {Dummy}Cube1.Position.Z:=0;
  MainCam.Position.X:=X*D;
  MainCam.Position.Y:=Y*D;
  MainCam.Position.Z:=Z*D;
  MainCam.FocalLength:=50;
  ReAdjustLightPosition;
  MainCam.TransformationChanged;
  CameraAspect:=MainCam.DistanceToTarget/MainCam.FocalLength;
End;

{================================================================
This proc is called from the form KeyDown events
(Form.KeyPreview is set to true)
and based on the key that went down/up.}
Procedure Talife3dForm.SetGLCursor(Var Key:Word; Down:Boolean);
Var
  K : Integer;
  B : Boolean;
  DoWhere:TGLPoint;
Begin
  DoWhere.X:=Mouse.CursorPos.X;
  DoWhere.Y:=Mouse.CursorPos.Y;
  B:=PointOver(DoWhere{Mouse.CursorPos},Viewer);

  ViewerState:=vsRotate;
  K:=RotateCursor;

  If Down Then
  Begin
    Case Key Of
      VK_ESCAPE:Close;
      VK_SHIFT : Begin ViewerState:=vsZoom; K:=ZoomCursor; End;
      VK_CONTROL : Begin ViewerState:=vsPanXY; K:=MoveXYCursor; End;
      VK_MENU : Begin ViewerState:=vsPanZ; K:=MoveZCursor; End;
      Ord('F'), Ord('f') :
           Begin ViewerState:=vsFocus; K:=FocusCursor; End;
      Ord('L'), Ord('l') :
           Begin ViewerState:=vsLight; K:=LightCursor; End;
      Ord('R'), Ord('r') : If B Then ResetCamera(1,1,1);
      Ord('W'), Ord('w') :
           Begin ViewerState:=vsDigi; K:=-20;{DigiCursor;} End;
      Ord('Q'), Ord('q'):Close;
      Ord('G'), Ord('g') :
           Begin
             ViewerState:=vsGrab;  K:=-20;
           End;
    End;
  End;

  If Viewer.Cursor<>K Then
  Begin
    Viewer.Cursor:=K;
    //This next line is necessary to update the cursor immediately
    //in the case where a key switched state
    //while a mouse button was being held down.
    //To see the problem, comment out the next line,
    //then LEFT CLICK AND HOLD, then PRESS SHIFT :
    //the mouse cursor will not update.
    If B Then Windows.SetCursor(Screen.Cursors[K]);
  End;
  //Since the ALT key has a special significance (brings up menu),
  //we disable it if pressed over the GLSceneViewer.
  //Again, to see the problem, you can comment
  //out the next line and try pressing ALT:
  //the cursor will reset to the default one, depending on the timing
  If (Key=VK_MENU) And B Then Key:=0;

End;
{===============================================================}



{================================================================}
Procedure Talife3dForm.FormKeyDown(Sender: TObject;
var Key: Word; Shift: TShiftState);
Begin
  SetGlCursor(Key,True);
End;


Procedure Talife3dForm.FormKeyUp(Sender: TObject;
var Key: Word; Shift: TShiftState);
Begin
  SetGlCursor(Key,False);
End;
{===============================================================}
procedure Talife3dForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(1800);
end;
{===============================================================}
{===============================================================}


{===============================================================}
{===============================================================}
procedure Talife3dForm.LoadLif(Filename:String);
Begin
  LifeFilesForm.DoFileLoader(FileName);
  LifeFilesForm.Do3DCellLoader;
  RunBtn.Enabled:=True;
  PauseStepBtn.Enabled:=True;
  If CellularRulesStyle=0 then GaiaS(Filename)
  else UniversaLife(Filename);
End;
procedure Talife3dForm.LoadLife(Filename:String; Sizer:Integer);
var
  i,j,k:Integer;
  LifeSizeHalf,ImageByte: Byte;
  CheckStr:String;
  fil: file of Byte;
Begin
  Case Sizer of
    3924:Begin
           {Check the file extension to be 3924}
           CheckStr := Uppercase(ExtractFileExt(Filename));
           if (CheckStr = '.LLF') then begin
           If LifeSize < 49 then
             lifesize50Click(self);{LifeSize:=49;}
             LifeSizeHalf:=LifeSize div 2;
    for I := 0 to LifeSize do {Clear all - especially the Edges}
    begin for J := 0 to LifeSize do
    begin for K := 0 to LifeSize do
    begin BeforeaLifeMatrix[I, J,K]:=0;
    end; end;end;
             AssignFile(fil, filename);
             {.$I-}reset(fil); {.$I+}
{             i := IOresult;
             if i <> 0 then begin DoMessages( 39998 ); EXIT; end else}
             begin
             for I := 0 to 38 do for J := 0 to 23 do
             begin
               read(fil, ImageByte);{49  3924}
               BeforeaLifeMatrix[i+5, j+12, LifeSizeHalf] := ImageByte;
             end;
             {.$I-}CloseFile(fil);{.$I+}
             {i := IOresult;
             if i <> 0 then Showmessage(Inttostr(IoResult));}
             end;
           end else exit;
         End;
    7848:Begin
           {Check the file extension to be 7848}
           CheckStr := Uppercase(ExtractFileExt(Filename));
           if (CheckStr = '.LMF') then begin
           If LifeSize < 99 then
             lifesize100Click(self);{LifeSize:=99;}
             LifeSizeHalf:=LifeSize div 2;
    for I := 0 to LifeSize do {Clear all - especially the Edges}
    begin for J := 0 to LifeSize do
    begin for K := 0 to LifeSize do
    begin BeforeaLifeMatrix[I, J,K]:=0;
    end; end;end;
             AssignFile(fil, filename);
             {.$I-}reset(fil); {.$I+}
             {i := IOresult;
             if i <> 0 then begin DoMessages( 39998 ); EXIT; end else}
             begin
             for I := 0 to 77 do for J := 0 to 47 do
             begin
               read(fil, ImageByte);{99  7848}
               BeforeaLifeMatrix[i+10, j+24, LifeSizeHalf] := ImageByte;
             end;
             CloseFile(fil);
             end;
           end else exit;
         End;
    15696:Begin
           {Check the file extension to be 15696}
           CheckStr := Uppercase(ExtractFileExt(Filename));
           if (CheckStr = '.LRF') then begin
           If LifeSize < 199 then
             lifesize200Click(self);{LifeSize:=199;}
             LifeSizeHalf:=LifeSize div 2;
    for I := 0 to LifeSize do {Clear all - especially the Edges}
    begin for J := 0 to LifeSize do
    begin for K := 0 to LifeSize do
    begin BeforeaLifeMatrix[I, J,K]:=0;
    end; end;end;
             AssignFile(fil, filename);
             {.$I-}reset(fil); {.$I+}
             {i := IOresult;
             if i <> 0 then begin DoMessages( 39998 ); EXIT; end else}
             begin
            for I := 0 to 155 do for J := 0 to 95 do
             begin
               read(fil, ImageByte);{199  15696}
               BeforeaLifeMatrix[i+20, j+50, LifeSizeHalf] := ImageByte;
             end;
             CloseFile(fil);
             end;
           end else exit;
          End;
  end;
  Busy:=False;
  IWannaQuit := False;
  RunBtn.Enabled:=True;
  PauseStepBtn.Enabled:=True;
  Application.ProcessMessages;
  If CellularRulesStyle=0 then GaiaS(Filename)
  else UniversaLife(Filename);
End;

procedure Talife3dForm.LifeFileBtnClick(Sender: TObject);
begin
  StopLifeBtnClick(Sender);
  OpenDialog1.Title := 'Life Files';
  OpenDialog1.Filter :=
  'Life files|*.l01;*.l02;*.l05;*.l10;*.l15;*.l20;*.l25;*.l50;*.llf;*.lmf;*.lrf;*.lif';
  {Array3924  llf}{Array7848 lmf}{Array15696 lrf}
  OpenDialog1.InitialDir := nLifeDir;
  OpenDialog1.Filename := '*.l??';
  if OpenDialog1.Execute then begin
    if ((FileExists(OpenDialog1.Filename))
      {and (lowercase(ExtractFileExt(OpenDialog1.Filename)) = '.l1x')})
      then
    begin
      nLifeDir:= ExtractFilePath(OpenDialog1.filename);
      ShowDown(OpenDialog1.Filename);
    end else showmessage('file not a .l3d');
  end;
end;
{===============================================================}
procedure Talife3dForm.ShowDown(HisFileName:String);
type
  Count3D = array[0..255] of Byte;
var
  Count3DArray: Count3D;
  Value: Byte;
  RX, GY, BZ,
  i,j,k,
  Counter,Counter2:Integer;
  fLoad: file of Count3D;
begin
  StopLifeBtnClick(self);
  alife3dForm.Caption:='Life 3D Cells: '
                           +ExtractFileName(HisFileName);
  Application.ProcessMessages;
  Counter2:=10;  Counter:=0;

  {Read em in}
  If (lowercase(ExtractFileExt(HisFileName)) = '.l01')
  then begin If LifeSize < 9 then lifesize10Click(self){LifeSize:=9}end else
  If (lowercase(ExtractFileExt(HisFileName)) = '.l02')
  then begin If LifeSize < 24 then lifesize25Click(self){LifeSize:=24}end  else
  If (lowercase(ExtractFileExt(HisFileName)) = '.l05')
  then begin If LifeSize < 49 then lifesize50Click(self){LifeSize:=49}end  else
  If (lowercase(ExtractFileExt(HisFileName)) = '.l10')
  then begin If LifeSize < 99 then lifesize100Click(self){LifeSize:=99}end  else
  If (lowercase(ExtractFileExt(HisFileName)) = '.l15')
  then begin If LifeSize < 149 then lifesize150Click(self){LifeSize:=149}end  else
    If (lowercase(ExtractFileExt(HisFileName)) = '.l20')
  then begin If LifeSize < 199 then lifesize200Click(self){LifeSize:=199}end  else
  If (lowercase(ExtractFileExt(HisFileName)) = '.l25')
  then begin If LifeSize < 254 then lifesize256Click(self){LifeSize:=254}end  else
  If (lowercase(ExtractFileExt(HisFileName)) = '.l50')
  then begin If LifeSize < 511 then lifesize512Click(self){LifeSize:=511}end  else
  {'Life files|*.l01;*.l02;*.l05;*.l10;*.l15;*.l20;*.l25;*.l50';}
{.llf;*.lmf;*.lrf;*.lif}
  If (lowercase(ExtractFileExt(HisFileName)) = '.lif')
  then begin Counter2:=0; Counter:=10;end else
  If (lowercase(ExtractFileExt(HisFileName)) = '.llf')
  then begin Counter2:=0; Counter:=11;end else
  If (lowercase(ExtractFileExt(HisFileName)) = '.lmf')
  then begin Counter2:=0;Counter:=12;end else
  If (lowercase(ExtractFileExt(HisFileName)) = '.lrf')
  then begin Counter2:=0;Counter:=13; end else
  begin
    Showmessage('FATAL FILE ERROR');
    HALT;
  end;
  {Set MENU}
  If Counter2=0 then begin
    If Counter=10 then LoadLif(HisFileName) else
    If Counter=11 then LoadLife(HisFileName,3924)  else
    If Counter=12 then LoadLife(HisFileName,7848)  else
    If Counter=13 then LoadLife(HisFileName,15696);
  end else begin
  Try
    for I := 0 to LifeSize do {Clear all - especially the Edges}
    begin for J := 0 to LifeSize do
    begin for K := 0 to LifeSize do
    begin BeforeaLifeMatrix[I, J,K]:=0;
    end; end;end;
  {$I-}
  AssignFile(fLoad, HisFileName);
  Reset(fLoad);
  for BZ := 0 to LifeSize do
  begin
    for GY := 0 to LifeSize do
    begin {Row}
      Read(fLoad, Count3DArray);
      for RX := 0 to LifeSize do
      begin {Col}
        Value := Count3DArray[RX];
        BeforeaLifeMatrix[RX, GY, BZ] := Value;
        {If Value > 0 then inc(Counter);}
      end;
    end;
  end;
    CloseFile(fLoad);
  {$I+}
  Application.ProcessMessages;
  Except
    CloseFile(fLoad);
    Busy:=False;
    IWannaQuit := True;
  end;
  {alife3dForm.Caption:='Life: '
                           +ExtractFileName(HisFileName)+
                            ', Cells:'+Inttostr(Counter);}
  Application.ProcessMessages;
  Busy:=False;
  IWannaQuit := False;
   {RULE TYPES}
  RunBtn.Enabled:=True;
  PauseStepBtn.Enabled:=True;
  If CellularRulesStyle=0 then GaiaS(HisFileName)
  else UniversaLife(HisFileName);
  end;
end;
{===============================================================}
Function Talife3dForm.RunBooleanRules:string;
var i:integer;
ResultString:String;
Begin
ResultString:='';
for i:= 0 to 25 do
If Universal[0,i] then ResultString:=ResultString+inttostr(i);
ResultString:=ResultString+'/';
for i:= 0 to 25 do
If Universal[1,i] then ResultString:=ResultString+inttostr(i);
RunBooleanRules:=ResultString;
End;
{===============================================================}
procedure Talife3dForm.UniversaLife(FileTodo: string);
var
  LifeFileExt,
  Name, Cs, Hs, s: string;
  ImageByte: Byte;
  CellActive:Boolean;
  Maxamillion,Minimillion,
  NapTime, ErrorCode, MaxRandomCells,
  OldHamming, OldCellCount, LastHamming, LastCellCount,
  Killer, Counter, CellCount, C, I, J, K, I2, J2, K2,
  Hamming, Cycle: integer;
  fil: file of Byte;
Begin
  {If IWannaQuit then exit;}
  Pausing :=False;
  UnPaused:=True;
  Application.ProcessMessages;
  IWannaQuit := False;
  RunBtn.Enabled:=True;
  PauseStepBtn.Enabled:=True;
  Size1.Enabled:=False;
  Run1.Enabled:=False;
  RunLifeBtn.Enabled:=False;
  CellularRules1.Enabled:=False;
  Viewliffiles1.Enabled:=False;
  IWannaQuit := False;
  LastHamming := 1;
  LastCellCount := 1;
  OldHamming := 1;
  OldCellCount:= 1;
  Name := 'Random';
 { NapTime := 0;}
  Hamming := 0;
  CellCount := 0;
  MaxRandomCells:=strtoint(MaxRandomCellsEdit.Text);
{doing every time allows changes to slowdown adjustment
  if bSlowDown then begin
    val(NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;}
  if (FileTodo = 'Random') then
  begin
    Randomize;
    for I := 0 to LifeSize do {Clear all - especially the Edges}
    begin for J := 0 to LifeSize do
    begin for K := 0 to LifeSize do
    begin BeforeaLifeMatrix[I, J,K]:=0;
    end; end;end;
    for I := 1 to LifeSize-1 do begin{Fill in Some - NOT Edges}
      for J := 1 to LifeSize-1 do begin
        for K := 1 to LifeSize-1 do begin
        If CellCount< MaxRandomCells then begin
        Cycle:=round(Random(2));
        BeforeaLifeMatrix[I, J, K] :=Cycle;
        If Cycle>0 then inc(CellCount);
    end;end;end;end;
  end else  Name := ExtractFileName(FileTodo);
  alife3dForm.Caption:='Life: '+Name;
  CellCount := 0;
  Cycle := 0;
  repeat
  begin
    Application.ProcessMessages;
    If Pausing then
    repeat
      begin Application.ProcessMessages; end
    until UnPaused;
    CYCLE := CYCLE + 1;  {AA := TV7848;}
    for I := 0 to LifeSize do
    begin for J := 0 to LifeSize do
      begin for K := 0 to LifeSize do
        begin AAM[I, J,K] := BeforeaLifeMatrix[I, J,K];
        CLM[I, J,K] :=123;
    end; end;end;
    {'INSERT code here for Cellular Automata rules}
    for I := 1 to LifeSize-1 do begin
      for J := 1 to LifeSize-1 do begin
        for K := 1 to LifeSize-1 do begin
          C :=0;
          for I2 := -1 to 1 do begin
            for J2 := -1 to 1 do begin
              for K2 := -1 to 1 do begin
              C :=C+ AAM[I+I2, J+J2,K+K2];
          end;end;end;
          C :=C- AAM[I,J,K];{Dont do self}
   {Rules OTHER than Conway}
   If AAM[I, J, K] =1 then CellActive:=True else CellActive:=False;
   BeforeaLifeMatrix[I, J, K] :=0;
   CLM[I, J, K]:=0;Maxamillion:=-1;Minimillion:=27;
   If CellActive then
   begin for i2:= 0 to 26 do
     begin
       If Universal[0,i2] then
       begin
         if C=i2 then BeforeaLifeMatrix[I, J, K] :=1;
         CLM[I, J, K]:=2;
       end;
     end;
   end;
   for i2:= 0 to 26 do
   begin
       If Universal[1,i2] then
       begin
         If LifeColorStyle=2 then
         begin
           If Maxamillion<i2 then Maxamillion:=i2;
           If Minimillion>i2 then Minimillion:=i2;
         end;
         if C=i2 then
         begin
           BeforeaLifeMatrix[I, J, K] :=1;
           CLM[I, J, K]:=3;
         end;
       end;
   end;
   If ((LifeColorStyle=2)and(CLM[I, J, K]=0)) then
   begin
     if C <= Minimillion then CLM[I, J, K]:=0
     else if C >= Maxamillion then CLM[I, J, K]:=4;
   end;
   {Rules OTHER than Conway}
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end;
      end;
    end;
{*************************************************}
{*************************************************}
      if (bTorusLife = true) then begin
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
{      for I := 1 to 76 do begin          for J := 1 to 46 do begin}
        I := 0;
        for J := 1 to LifeSize-1 do begin
         for K := 1 to LifeSize-1 do begin
          C :=0;
            for J2 := -1 to 1 do begin
              for K2 := -1 to 1 do begin
C :=C+ AAM[I + LifeSize, J+J2,K+K2] + AAM[I, J+J2,K+K2] + AAM[I + 1, J+J2,K+K2];
          end;end;
          C :=C- AAM[I,J,K];{Dont do self}
   {Rules OTHER than Conway}
   If AAM[I, J, K] =1 then CellActive:=True else CellActive:=False;
   BeforeaLifeMatrix[I, J, K] :=0;
   CLM[I, J, K]:=0;Maxamillion:=-1;Minimillion:=27;
   If CellActive then
   begin for i2:= 0 to 26 do
     begin
       If Universal[0,i2] then
       begin
         if C=i2 then BeforeaLifeMatrix[I, J, K] :=1;
         CLM[I, J, K]:=2;
       end;
     end;
   end;
   for i2:= 0 to 26 do
   begin
       If Universal[1,i2] then
       begin
         If LifeColorStyle=2 then
         begin
           If Maxamillion<i2 then Maxamillion:=i2;
           If Minimillion>i2 then Minimillion:=i2;
         end;
         if C=i2 then
         begin
           BeforeaLifeMatrix[I, J, K] :=1;
           CLM[I, J, K]:=3;
         end;
       end;
   end;
   If ((LifeColorStyle=2)and(CLM[I, J, K]=0)) then
   begin
     if C <= Minimillion then CLM[I, J, K]:=0
     else if C >= Maxamillion then CLM[I, J, K]:=4;
   end;
   {Rules OTHER than Conway}
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end;end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := LifeSize;
        for J := 1 to LifeSize-1 do begin
         for K := 1 to LifeSize-1 do begin
          C :=0;
            for J2 := -1 to 1 do begin
              for K2 := -1 to 1 do begin
C :=C+ AAM[I - LifeSize, J+J2,K+K2] + AAM[I, J+J2,K+K2] + AAM[I - 1, J+J2,K+K2];
          end;end;
          C :=C- AAM[I,J,K];{Dont do self}
   {Rules OTHER than Conway}
   If AAM[I, J, K] =1 then CellActive:=True else CellActive:=False;
   BeforeaLifeMatrix[I, J, K] :=0;
   CLM[I, J, K]:=0;Maxamillion:=-1;Minimillion:=27;
   If CellActive then
   begin for i2:= 0 to 26 do
     begin
       If Universal[0,i2] then
       begin
         if C=i2 then BeforeaLifeMatrix[I, J, K] :=1;
         CLM[I, J, K]:=2;
       end;
     end;
   end;
   for i2:= 0 to 26 do
   begin
       If Universal[1,i2] then
       begin
         If LifeColorStyle=2 then
         begin
           If Maxamillion<i2 then Maxamillion:=i2;
           If Minimillion>i2 then Minimillion:=i2;
         end;
         if C=i2 then
         begin
           BeforeaLifeMatrix[I, J, K] :=1;
           CLM[I, J, K]:=3;
         end;
       end;
   end;
   If ((LifeColorStyle=2)and(CLM[I, J, K]=0)) then
   begin
     if C <= Minimillion then CLM[I, J, K]:=0
     else if C >= Maxamillion then CLM[I, J, K]:=4;
   end;
   {Rules OTHER than Conway}
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end;end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0;
        for I := 1 to LifeSize-1 do begin
         for K := 1 to LifeSize-1 do begin
          C :=0;
            for I2 := -1 to 1 do begin
              for K2 := -1 to 1 do begin
C :=C+ AAM[I+I2, J+1,K+K2] + AAM[I+I2, J,K+K2] + AAM[I+I2, J+LifeSize,K+K2];
          end;end;
          C :=C- AAM[I,J,K];{Dont do self}
   {Rules OTHER than Conway}
   If AAM[I, J, K] =1 then CellActive:=True else CellActive:=False;
   BeforeaLifeMatrix[I, J, K] :=0;
   CLM[I, J, K]:=0;Maxamillion:=-1;Minimillion:=27;
   If CellActive then
   begin for i2:= 0 to 26 do
     begin
       If Universal[0,i2] then
       begin
         if C=i2 then BeforeaLifeMatrix[I, J, K] :=1;
         CLM[I, J, K]:=2;
       end;
     end;
   end;
   for i2:= 0 to 26 do
   begin
       If Universal[1,i2] then
       begin
         If LifeColorStyle=2 then
         begin
           If Maxamillion<i2 then Maxamillion:=i2;
           If Minimillion>i2 then Minimillion:=i2;
         end;
         if C=i2 then
         begin
           BeforeaLifeMatrix[I, J, K] :=1;
           CLM[I, J, K]:=3;
         end;
       end;
   end;
   If ((LifeColorStyle=2)and(CLM[I, J, K]=0)) then
   begin
     if C <= Minimillion then CLM[I, J, K]:=0
     else if C >= Maxamillion then CLM[I, J, K]:=4;
   end;
   {Rules OTHER than Conway}
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end;end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := LifeSize;
        for I := 1 to LifeSize-1 do begin
         for K := 1 to LifeSize-1 do begin
          C :=0;
            for I2 := -1 to 1 do begin
              for K2 := -1 to 1 do begin
C :=C+ AAM[I+I2, J-1,K+K2] + AAM[I+I2, J,K+K2] + AAM[I+I2, J-LifeSize,K+K2];
          end;end;
          C :=C- AAM[I,J,K];{Dont do self}
   {Rules OTHER than Conway}
   If AAM[I, J, K] =1 then CellActive:=True else CellActive:=False;
   BeforeaLifeMatrix[I, J, K] :=0;
   CLM[I, J, K]:=0;Maxamillion:=-1;Minimillion:=27;
   If CellActive then
   begin for i2:= 0 to 26 do
     begin
       If Universal[0,i2] then
       begin
         if C=i2 then BeforeaLifeMatrix[I, J, K] :=1;
         CLM[I, J, K]:=2;
       end;
     end;
   end;
   for i2:= 0 to 26 do
   begin
       If Universal[1,i2] then
       begin
         If LifeColorStyle=2 then
         begin
           If Maxamillion<i2 then Maxamillion:=i2;
           If Minimillion>i2 then Minimillion:=i2;
         end;
         if C=i2 then
         begin
           BeforeaLifeMatrix[I, J, K] :=1;
           CLM[I, J, K]:=3;
         end;
       end;
   end;
   If ((LifeColorStyle=2)and(CLM[I, J, K]=0)) then
   begin
     if C <= Minimillion then CLM[I, J, K]:=0
     else if C >= Maxamillion then CLM[I, J, K]:=4;
   end;
   {Rules OTHER than Conway}
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end;end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
{ K :=0} K := LifeSize; J := LifeSize; I := LifeSize; begin
   {J-1} C := AAM[I - 1, J - 1,K] + AAM[I, J - 1,K] + AAM[I - LifeSize, J - 1,K];
   {J} C := C + AAM[I - 1, J,K] + AAM[I - LifeSize, J,K];
     { I,J (itself) is NOT counted}
   {J+1} C := C + AAM[I - 1, J - LifeSize,K] + AAM[I, J - LifeSize,K] + AAM[I-LifeSize, J-LifeSize,K];
   {Rules OTHER than Conway}
   If AAM[I, J, K] =1 then CellActive:=True else CellActive:=False;
   BeforeaLifeMatrix[I, J, K] :=0;
   CLM[I, J, K]:=0;Maxamillion:=-1;Minimillion:=27;
   If CellActive then
   begin for i2:= 0 to 26 do
     begin
       If Universal[0,i2] then
       begin
         if C=i2 then BeforeaLifeMatrix[I, J, K] :=1;
         CLM[I, J, K]:=2;
       end;
     end;
   end;
   for i2:= 0 to 26 do
   begin
       If Universal[1,i2] then
       begin
         If LifeColorStyle=2 then
         begin
           If Maxamillion<i2 then Maxamillion:=i2;
           If Minimillion>i2 then Minimillion:=i2;
         end;
         if C=i2 then
         begin
           BeforeaLifeMatrix[I, J, K] :=1;
           CLM[I, J, K]:=3;
         end;
       end;
   end;
   If ((LifeColorStyle=2)and(CLM[I, J, K]=0)) then
   begin
     if C <= Minimillion then CLM[I, J, K]:=0
     else if C >= Maxamillion then CLM[I, J, K]:=4;
   end;
   {Rules OTHER than Conway}
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
{ K :=0} K := LifeSize;  J := LifeSize; I := 0; begin
   {J-1} C := AAM[I + LifeSize, J - 1,K] + AAM[I, J - 1,K] + AAM[I + 1, J - 1,K];
   {J} C := C + AAM[I + LifeSize, J,K] + AAM[I + 1, J,K];
     { I,J (itself) is NOT counted}
   {J+1} C := C + AAM[I + LifeSize, J - LifeSize,K] + AAM[I, J - LifeSize,K] + AAM[I + 1, J-LifeSize,K];
   {Rules OTHER than Conway}
   If AAM[I, J, K] =1 then CellActive:=True else CellActive:=False;
   BeforeaLifeMatrix[I, J, K] :=0;
   CLM[I, J, K]:=0;Maxamillion:=-1;Minimillion:=27;
   If CellActive then
   begin for i2:= 0 to 26 do
     begin
       If Universal[0,i2] then
       begin
         if C=i2 then BeforeaLifeMatrix[I, J, K] :=1;
         CLM[I, J, K]:=2;
       end;
     end;
   end;
   for i2:= 0 to 26 do
   begin
       If Universal[1,i2] then
       begin
         If LifeColorStyle=2 then
         begin
           If Maxamillion<i2 then Maxamillion:=i2;
           If Minimillion>i2 then Minimillion:=i2;
         end;
         if C=i2 then
         begin
           BeforeaLifeMatrix[I, J, K] :=1;
           CLM[I, J, K]:=3;
         end;
       end;
   end;
   If ((LifeColorStyle=2)and(CLM[I, J, K]=0)) then
   begin
     if C <= Minimillion then CLM[I, J, K]:=0
     else if C >= Maxamillion then CLM[I, J, K]:=4;
   end;
   {Rules OTHER than Conway}
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
{ K := LifeSize;}
K := 0; J := 0; I := 0; begin
 {J-1} C := AAM[I + LifeSize, J + LifeSize,K] + AAM[I, J + LifeSize,K] + AAM[I + 1, J + LifeSize,K];
   {J} C := C + AAM[I + LifeSize, J,K] + AAM[I + 1, J,K];
     { I,J (itself) is NOT counted}
 {J+1} C := C + AAM[I + LifeSize, J + 1,K] + AAM[I, J + 1,K] + AAM[I + 1, J + 1,K];
   {Rules OTHER than Conway}
   If AAM[I, J, K] =1 then CellActive:=True else CellActive:=False;
   BeforeaLifeMatrix[I, J, K] :=0;
   CLM[I, J, K]:=0;Maxamillion:=-1;Minimillion:=27;
   If CellActive then
   begin for i2:= 0 to 26 do
     begin
       If Universal[0,i2] then
       begin
         if C=i2 then BeforeaLifeMatrix[I, J, K] :=1;
         CLM[I, J, K]:=2;
       end;
     end;
   end;
   for i2:= 0 to 26 do
   begin
       If Universal[1,i2] then
       begin
         If LifeColorStyle=2 then
         begin
           If Maxamillion<i2 then Maxamillion:=i2;
           If Minimillion>i2 then Minimillion:=i2;
         end;
         if C=i2 then
         begin
           BeforeaLifeMatrix[I, J, K] :=1;
           CLM[I, J, K]:=3;
         end;
       end;
   end;
   If ((LifeColorStyle=2)and(CLM[I, J, K]=0)) then
   begin
     if C <= Minimillion then CLM[I, J, K]:=0
     else if C >= Maxamillion then CLM[I, J, K]:=4;
   end;
   {Rules OTHER than Conway}
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
{ K := LifeSize;}
K := 0; J := 0; I := LifeSize; begin
 {J-1} C := AAM[I - 1, J + LifeSize,K] + AAM[I, J + LifeSize,K] + AAM[I - LifeSize, J + LifeSize,K];
   {J} C := C + AAM[I - 1, J,K] + AAM[I - LifeSize, J,K];
     { I,J (itself) is NOT counted}
 {J+1} C := C + AAM[I - 1, J + 1,K] + AAM[I, J + 1,K] + AAM[I - LifeSize, J + 1,K];
   {Rules OTHER than Conway}
   If AAM[I, J, K] =1 then CellActive:=True else CellActive:=False;
   BeforeaLifeMatrix[I, J, K] :=0;
   CLM[I, J, K]:=0;Maxamillion:=-1;Minimillion:=27;
   If CellActive then
   begin for i2:= 0 to 26 do
     begin
       If Universal[0,i2] then
       begin
         if C=i2 then BeforeaLifeMatrix[I, J, K] :=1;
         CLM[I, J, K]:=2;
       end;
     end;
   end;
   for i2:= 0 to 26 do
   begin
       If Universal[1,i2] then
       begin
         If LifeColorStyle=2 then
         begin
           If Maxamillion<i2 then Maxamillion:=i2;
           If Minimillion>i2 then Minimillion:=i2;
         end;
         if C=i2 then
         begin
           BeforeaLifeMatrix[I, J, K] :=1;
           CLM[I, J, K]:=3;
         end;
       end;
   end;
   If ((LifeColorStyle=2)and(CLM[I, J, K]=0)) then
   begin
     if C <= Minimillion then CLM[I, J, K]:=0
     else if C >= Maxamillion then CLM[I, J, K]:=4;
   end;
   {Rules OTHER than Conway}
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end;
      end; {Of if Torus}
 {*************************************************}
{*************************************************}

{ Display is after the text so Text can be seen while image is altered }
    Str(Hamming, Hs);
    Str(Cycle, s);
    Str(Cellcount, Cs);
    StatusBarred.Panels[1].Text :=
    {StatusBar.SimpleText :=} 'Pixel Life:  '
      + 'Hamming:' + Hs + ' Cells:' + Cs+
       ' Cycle # ' + s + ' ';
      {If IWannaQuit then Exit; }
    Application.ProcessMessages;
    Counter:=0;
{RedXMax, RedXMin, GreenYMax,GreenYMin,BlueZMax, BlueZMin  LifeSize}
    for I := RedXMin to RedXMax do  begin
      Application.ProcessMessages;
      for J := GreenYMin to GreenYMax do begin
        for K := BlueZMin to BlueZMax do begin
          {if (AAM[I, J, K] <> BeforeaLifeMatrix[I, J, K]) then
                begin ... NO Have to CHANGE ALL}
          if ((BeforeaLifeMatrix[I, J, K] = 1)or
              (LifeColorStyle=2)) then
          begin
            inc(Counter);
            If Counter< MaxRandomCells then begin
            If Counter > TotalCounter then
            begin{ LifeSize TotalCounter}
              inc(TotalCounter);
              setlength(LifeCellMatrix,TotalCounter+1);
              LifeCellMatrix[TotalCounter]:=TGLCube.create(Scene.objects);
              Scene.Objects.AddChild(LifeCellMatrix[TotalCounter]);
              LifeCellMatrix[TotalCounter].Assign(Cube1);
              LifeCellMatrix[TotalCounter].Visible:=True;
            end;   { BeforeaLifeMatrix  AAM}
            with LifeCellMatrix[Counter] do
            begin
              LifeCellMatrix[Counter].Visible:=True;
              position.SetPoint(I,J,K);
              CubeDepth:=CubeSizer;{Height}
              CubeHeight:=CubeSizer;{Width}
              CubeWidth:=CubeSizer;{Depth}

              with Material.FrontProperties do
              begin
              If LifeColorStyle=0 then Emission.{Color.}AsWinColor:=Full
              else If LifeColorStyle=1 then
                   Emission.Color:=pointmake(i/(LifeSize+1),j/(LifeSize+1),k/(LifeSize+1))
              else If LifeColorStyle=2 then begin
              {Desolate Growing Full Overpopulated}
                  Case CLM[I, J, K] of
                  0:LifeCellMatrix[Counter].Visible:=False;
                  {Emission.AsWinColor:=Desolate;}
                  2:Emission.AsWinColor:=Growing;
                  3:Emission.AsWinColor:=Full;
                  4:Emission.AsWinColor:=Overpopulated;
                  else LifeCellMatrix[Counter].Visible:=False;
                  end;
                  end;
                Diffuse.Color:=Emission.Color;
                Ambient.Color:=Emission.Color;
                TagFloat:=I*J*K;
              end;
            end;
          end;{Color options done in Setit( )}
          end;
        end;
      end;
    end;
    {make non used invisible}
    If Counter < TotalCounter then
      for Killer:= Counter+1 to TotalCounter do
        LifeCellMatrix[Killer].Visible:=False;
    if (bSlowDown = true) then
    begin
      Application.ProcessMessages;
      val(NumberEdit.Text, NapTime, ErrorCode);
      if (ErrorCode > 0) then NapTime := 0;
      Application.ProcessMessages;
      Sleep(NapTime); {MsgWaitForMultipleObjects}
    end;
    if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
    begin
      if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
        {IWannaQuit := True;}
        StopLifeBtnClick(self);
      LastHamming := OldHamming; LastCellCount := OldCellCount;
    end;
    OldHamming := Hamming;
    OldCellCount := CellCount;
    Hamming := 0;
    CellCount := 0;
    If Pausing then UnPaused:=False;    
  end; { of Repeat loop  }
  until (IWannaQuit = True);
  {set OpenDialog1 filter to display Life's only}
  {Save the InImage?}
  If LifeSize=9 then   LifeFileExt := 'l01'
  else If LifeSize=24 then   LifeFileExt := 'l02'
  else If LifeSize=49 then   LifeFileExt := 'l05'
  else If LifeSize=99 then   LifeFileExt := 'l10'
  else If LifeSize=149 then   LifeFileExt := 'l15'
  else If LifeSize=199 then   LifeFileExt := 'l20'
  else If LifeSize=254 then   LifeFileExt := 'l25'
  else If LifeSize=511 then   LifeFileExt := 'l50'  ;
  SaveDialog1.InitialDir:=nLifeDir;
  SaveDialog1.filter := 'Life files|*.'+LifeFileExt;
    {  'Life files|*.l10;*.l25;*.l50;*.l1x';}
  SaveDialog1.DefaultExt := LifeFileExt;
  if SaveStartCB.Checked then begin
    SaveDialog1.filename := 'RandomLife.'+LifeFileExt;
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
        {$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
          for I := 0 to LifeSize do begin
            for J := 0 to LifeSize do begin
              for K := 0 to LifeSize do begin
                ImageByte := AAM[I, J, K];
                write(fil, ImageByte);
          end;end;end;
          CloseFile(fil);
        end;
  end; end; end;
  if SaveEndCB.Checked then begin
    SaveDialog1.filename := 'NewLife.'+LifeFileExt;
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
        {$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
          for I := 0 to LifeSize do begin
            for J := 0 to LifeSize do begin
              for K := 0 to LifeSize do begin
                ImageByte := BeforeaLifeMatrix[I, J, K];
                write(fil, ImageByte);
          end;end;end;
          CloseFile(fil);
        end;
      end;
    end;
  end;
End;
procedure Talife3dForm.RunLifeBtnClick(Sender: TObject);
begin
  If CellularRulesStyle=0 then GaiaS('Random')
  else UniversaLife('Random');
end;

{  LifeSize:=24;  TotalCounter
  SetLength(BeforeaLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(AfteraLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);}
procedure Talife3dForm.GaiaS(FileTodo: string);
var
  LifeFileExt,
  Name, Cs, Hs, s: string;
  ImageByte: Byte;
  NapTime, ErrorCode, MaxRandomCells,
  OldHamming, OldCellCount, LastHamming, LastCellCount,
  Killer, Counter, CellCount, C, I, J, K, I2, J2, K2,
  Hamming, Cycle: integer;
  fil: file of Byte;
begin
  {If IWannaQuit then exit;}
  Pausing :=False;
  UnPaused:=True;
  Application.ProcessMessages;
  IWannaQuit := False;
  RunBtn.Enabled:=True;
  PauseStepBtn.Enabled:=True;
  Size1.Enabled:=False;
  Run1.Enabled:=False;
  RunLifeBtn.Enabled:=False;
  CellularRules1.Enabled:=False;
  Viewliffiles1.Enabled:=False;
  IWannaQuit := False;
  LastHamming := 1;
  LastCellCount := 1;
  OldHamming := 1;
  OldCellCount:= 1;
  Name := 'Random';
 { NapTime := 0;}
  Hamming := 0;
  CellCount := 0;
  MaxRandomCells:=strtoint(MaxRandomCellsEdit.Text);
{doing every time allows changes to slowdown adjustment
  if bSlowDown then begin
    val(NumberEdit.Text, NapTime, ErrorCode);
    if (ErrorCode > 0) then NapTime := 0;
  end;}
  if (FileTodo = 'Random') then
  begin
    Randomize;
    for I := 0 to LifeSize do {Clear all - especially the Edges}
    begin for J := 0 to LifeSize do
    begin for K := 0 to LifeSize do
    begin BeforeaLifeMatrix[I, J,K]:=0;
    end; end;end;
    for I := 1 to LifeSize-1 do begin{Fill in Some - NOT Edges}
      for J := 1 to LifeSize-1 do begin
        for K := 1 to LifeSize-1 do begin
        If CellCount< MaxRandomCells then begin
        Cycle:=round(Random(2));
        BeforeaLifeMatrix[I, J, K] :=Cycle;
        If Cycle>0 then inc(CellCount);
    end;end;end;end;
  end else  Name := ExtractFileName(FileTodo);
  alife3dForm.Caption:='Life: '+Name;
  CellCount := 0;
  Cycle := 0;
  repeat
  begin
    Application.ProcessMessages;
    If Pausing then
    repeat
      begin Application.ProcessMessages; end
    until UnPaused;
    CYCLE := CYCLE + 1;  {AA := TV7848;}
    for I := 0 to LifeSize do
    begin for J := 0 to LifeSize do
      begin for K := 0 to LifeSize do
        begin AAM[I, J,K] := BeforeaLifeMatrix[I, J,K];
        CLM[I, J,K] :=123;
    end; end;end;
    {'INSERT code here for Cellular Automata rules}
    for I := 1 to LifeSize-1 do begin
      for J := 1 to LifeSize-1 do begin
        for K := 1 to LifeSize-1 do begin
(*   {J-1} C := AAM[I - 1, J - 1,K] + AAM[I, J - 1,K] + AAM[I + 1, J - 1,K];
   {J} C := C + AAM[I - 1, J,K] + AAM[I + 1, J,K];
     { I,J (itself) is NOT counted}
   {J+1} C := C + AAM[I - 1, J + 1,K] + AAM[I, J + 1,K] + AAM[I + 1, J + 1,K];*)
          C :=0;
          for I2 := -1 to 1 do begin
            for J2 := -1 to 1 do begin
              for K2 := -1 to 1 do begin
              C :=C+ AAM[I+I2, J+J2,K+K2];
          end;end;end;
          C :=C- AAM[I,J,K];{Dont do self}
          if C <= 1 then  begin CLM[I, J, K]:=0; BeforeaLifeMatrix[I, J,K] := 0; end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off... but in 3D i gotta draw the cubes}
          else if ((C = 2) and (BeforeaLifeMatrix[I, J,K] = 1)) then
                 begin  CLM[I, J, K]:=2; {BeforeaLifeMatrix[I, J,K] := 1;} end
          else if C = 3 then begin  CLM[I, J, K]:=3;BeforeaLifeMatrix[I, J,K] := 1; end
          else if C >= 4 then begin CLM[I, J, K]:=4; BeforeaLifeMatrix[I, J,K] := 0; end;
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end;
      end;
    end;
{*************************************************}
{*************************************************}
      if (bTorusLife = true) then begin
{ DO THE LEFT THEN THE RIGHT EDGES }{ I is across 42 J is DOWN 25 }
{      for I := 1 to 76 do begin          for J := 1 to 46 do begin}
        I := 0;
        for J := 1 to LifeSize-1 do begin
         for K := 1 to LifeSize-1 do begin
          C :=0;
            for J2 := -1 to 1 do begin
              for K2 := -1 to 1 do begin
C :=C+ AAM[I + LifeSize, J+J2,K+K2] + AAM[I, J+J2,K+K2] + AAM[I + 1, J+J2,K+K2];
          end;end;
          C :=C- AAM[I,J,K];{Dont do self}
          if C <= 1 then  begin CLM[I, J, K]:=0; BeforeaLifeMatrix[I, J,K] := 0; end
{the trick is if C = 2 then if [I,J] is on it will stay on
and if off it will stay off... but in 3D i gotta draw the cubes}
          else if ((C = 2) and (BeforeaLifeMatrix[I, J,K] = 1)) then
                 begin  CLM[I, J, K]:=2; {BeforeaLifeMatrix[I, J,K] := 1;} end
          else if C = 3 then begin  CLM[I, J, K]:=3;BeforeaLifeMatrix[I, J,K] := 1; end
          else if C >= 4 then begin CLM[I, J, K]:=4; BeforeaLifeMatrix[I, J,K] := 0; end;
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end;end;
{*************************************************}
{ DO THE RIGHT EDGE }{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        I := LifeSize;
        for J := 1 to LifeSize-1 do begin
         for K := 1 to LifeSize-1 do begin
          C :=0;
            for J2 := -1 to 1 do begin
              for K2 := -1 to 1 do begin
C :=C+ AAM[I - LifeSize, J+J2,K+K2] + AAM[I, J+J2,K+K2] + AAM[I - 1, J+J2,K+K2];
          end;end;
          C :=C- AAM[I,J,K];{Dont do self}
          if C <= 1 then  begin CLM[I, J, K]:=0; BeforeaLifeMatrix[I, J,K] := 0; end
          else if ((C = 2) and (BeforeaLifeMatrix[I, J,K] = 1)) then
                 begin  CLM[I, J, K]:=2; {BeforeaLifeMatrix[I, J,K] := 1;} end
          else if C = 3 then begin  CLM[I, J, K]:=3;BeforeaLifeMatrix[I, J,K] := 1; end
          else if C >= 4 then begin CLM[I, J, K]:=4; BeforeaLifeMatrix[I, J,K] := 0; end;
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end;end;
{*************************************************}
{TOP}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := 0;
        for I := 1 to LifeSize-1 do begin
         for K := 1 to LifeSize-1 do begin
          C :=0;
            for I2 := -1 to 1 do begin
              for K2 := -1 to 1 do begin
C :=C+ AAM[I+I2, J+1,K+K2] + AAM[I+I2, J,K+K2] + AAM[I+I2, J+LifeSize,K+K2];
          end;end;
          C :=C- AAM[I,J,K];{Dont do self}
          if C <= 1 then  begin CLM[I, J, K]:=0; BeforeaLifeMatrix[I, J,K] := 0; end
          else if ((C = 2) and (BeforeaLifeMatrix[I, J,K] = 1)) then
                 begin  CLM[I, J, K]:=2; {BeforeaLifeMatrix[I, J,K] := 1;} end
          else if C = 3 then begin  CLM[I, J, K]:=3;BeforeaLifeMatrix[I, J,K] := 1; end
          else if C >= 4 then begin CLM[I, J, K]:=4; BeforeaLifeMatrix[I, J,K] := 0; end;
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end;end;
{*************************************************}
{BOTTOM}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
        J := LifeSize;
        for I := 1 to LifeSize-1 do begin
         for K := 1 to LifeSize-1 do begin
          C :=0;
            for I2 := -1 to 1 do begin
              for K2 := -1 to 1 do begin
C :=C+ AAM[I+I2, J-1,K+K2] + AAM[I+I2, J,K+K2] + AAM[I+I2, J-LifeSize,K+K2];
          end;end;
          C :=C- AAM[I,J,K];{Dont do self}
          if C <= 1 then  begin CLM[I, J, K]:=0; BeforeaLifeMatrix[I, J,K] := 0; end
          else if ((C = 2) and (BeforeaLifeMatrix[I, J,K] = 1)) then
                 begin  CLM[I, J, K]:=2; {BeforeaLifeMatrix[I, J,K] := 1;} end
          else if C = 3 then begin  CLM[I, J, K]:=3;BeforeaLifeMatrix[I, J,K] := 1; end
          else if C >= 4 then begin CLM[I, J, K]:=4; BeforeaLifeMatrix[I, J,K] := 0; end;
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end;end;
{*************************************************}
{BOTTOMRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
{ K :=0} K := LifeSize; J := LifeSize; I := LifeSize; begin
   {J-1} C := AAM[I - 1, J - 1,K] + AAM[I, J - 1,K] + AAM[I - LifeSize, J - 1,K];
   {J} C := C + AAM[I - 1, J,K] + AAM[I - LifeSize, J,K];
     { I,J (itself) is NOT counted}
   {J+1} C := C + AAM[I - 1, J - LifeSize,K] + AAM[I, J - LifeSize,K] + AAM[I-LifeSize, J-LifeSize,K];
          if C <= 1 then  begin CLM[I, J, K]:=0; BeforeaLifeMatrix[I, J,K] := 0; end
          else if ((C = 2) and (BeforeaLifeMatrix[I, J,K] = 1)) then
                 begin  CLM[I, J, K]:=2; {BeforeaLifeMatrix[I, J,K] := 1;} end
          else if C = 3 then begin  CLM[I, J, K]:=3;BeforeaLifeMatrix[I, J,K] := 1; end
          else if C >= 4 then begin CLM[I, J, K]:=4; BeforeaLifeMatrix[I, J,K] := 0; end;
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{BOTTOMLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
{ K :=0} K := LifeSize;  J := LifeSize; I := 0; begin
   {J-1} C := AAM[I + LifeSize, J - 1,K] + AAM[I, J - 1,K] + AAM[I + 1, J - 1,K];
   {J} C := C + AAM[I + LifeSize, J,K] + AAM[I + 1, J,K];
     { I,J (itself) is NOT counted}
   {J+1} C := C + AAM[I + LifeSize, J - LifeSize,K] + AAM[I, J - LifeSize,K] + AAM[I + 1, J-LifeSize,K];
          if C <= 1 then  begin CLM[I, J, K]:=0; BeforeaLifeMatrix[I, J,K] := 0; end
          else if ((C = 2) and (BeforeaLifeMatrix[I, J,K] = 1)) then
                 begin  CLM[I, J, K]:=2; {BeforeaLifeMatrix[I, J,K] := 1;} end
          else if C = 3 then begin  CLM[I, J, K]:=3;BeforeaLifeMatrix[I, J,K] := 1; end
          else if C >= 4 then begin CLM[I, J, K]:=4; BeforeaLifeMatrix[I, J,K] := 0; end;
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopLeftCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
{ K := LifeSize;}
K := 0; J := 0; I := 0; begin
 {J-1} C := AAM[I + LifeSize, J + LifeSize,K] + AAM[I, J + LifeSize,K] + AAM[I + 1, J + LifeSize,K];
   {J} C := C + AAM[I + LifeSize, J,K] + AAM[I + 1, J,K];
     { I,J (itself) is NOT counted}
 {J+1} C := C + AAM[I + LifeSize, J + 1,K] + AAM[I, J + 1,K] + AAM[I + 1, J + 1,K];
          if C <= 1 then  begin CLM[I, J, K]:=0; BeforeaLifeMatrix[I, J,K] := 0; end
          else if ((C = 2) and (BeforeaLifeMatrix[I, J,K] = 1)) then
                 begin  CLM[I, J, K]:=2; {BeforeaLifeMatrix[I, J,K] := 1;} end
          else if C = 3 then begin  CLM[I, J, K]:=3;BeforeaLifeMatrix[I, J,K] := 1; end
          else if C >= 4 then begin CLM[I, J, K]:=4; BeforeaLifeMatrix[I, J,K] := 0; end;
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end; {*************************************************}
{TopRightCorner}{ I is across J is DOWN }{ I is across 42 J is DOWN 25 }
{ K := LifeSize;}
K := 0; J := 0; I := LifeSize; begin
 {J-1} C := AAM[I - 1, J + LifeSize,K] + AAM[I, J + LifeSize,K] + AAM[I - LifeSize, J + LifeSize,K];
   {J} C := C + AAM[I - 1, J,K] + AAM[I - LifeSize, J,K];
     { I,J (itself) is NOT counted}
 {J+1} C := C + AAM[I - 1, J + 1,K] + AAM[I, J + 1,K] + AAM[I - LifeSize, J + 1,K];
          if C <= 1 then  begin CLM[I, J, K]:=0; BeforeaLifeMatrix[I, J,K] := 0; end
          else if ((C = 2) and (BeforeaLifeMatrix[I, J,K] = 1)) then
                 begin  CLM[I, J, K]:=2; {BeforeaLifeMatrix[I, J,K] := 1;} end
          else if C = 3 then begin  CLM[I, J, K]:=3;BeforeaLifeMatrix[I, J,K] := 1; end
          else if C >= 4 then begin CLM[I, J, K]:=4; BeforeaLifeMatrix[I, J,K] := 0; end;
          if (not (AAM[I, J,K] = BeforeaLifeMatrix[I, J,K])) then
            HAMMING := (HAMMING + 1);
          if  BeforeaLifeMatrix[I, J,K] = 1 then CellCount := CellCount + 1;
        end;
      end; {Of if Torus}
 {*************************************************}
{*************************************************}

{ Display is after the text so Text can be seen while image is altered }
    Str(Hamming, Hs);
    Str(Cycle, s);
    Str(Cellcount, Cs);
    StatusBarred.Panels[1].Text :=
    {StatusBar.SimpleText :=} 'Pixel Life:  '
      + 'Hamming:' + Hs + ' Cells:' + Cs+
       ' Cycle # ' + s + ' ';
      {If IWannaQuit then Exit; }
    Application.ProcessMessages;
    Counter:=0;
{RedXMax, RedXMin, GreenYMax,GreenYMin,BlueZMax, BlueZMin  LifeSize}
    for I := RedXMin to RedXMax do  begin
      Application.ProcessMessages;
      for J := GreenYMin to GreenYMax do begin
        for K := BlueZMin to BlueZMax do begin
          {if (AAM[I, J, K] <> BeforeaLifeMatrix[I, J, K]) then
                begin ... NO Have to CHANGE ALL}
          if ((BeforeaLifeMatrix[I, J, K] = 1)or
              (LifeColorStyle=2)) then
          begin
            inc(Counter);
            If Counter< MaxRandomCells then begin
            If Counter > TotalCounter then
            begin{ LifeSize TotalCounter}
              inc(TotalCounter);
              setlength(LifeCellMatrix,TotalCounter+1);
              LifeCellMatrix[TotalCounter]:=TGLCube.create(Scene.objects);
              Scene.Objects.AddChild(LifeCellMatrix[TotalCounter]);
              LifeCellMatrix[TotalCounter].Assign(Cube1);
              LifeCellMatrix[TotalCounter].Visible:=True;
            end;   { BeforeaLifeMatrix  AAM}
            with LifeCellMatrix[Counter] do
            begin
              LifeCellMatrix[Counter].Visible:=True;
              position.SetPoint(I,J,K);
              CubeDepth:=CubeSizer;{Height}
              CubeHeight:=CubeSizer;{Width}
              CubeWidth:=CubeSizer;{Depth}

              with Material.FrontProperties do
              begin
              If LifeColorStyle=0 then Emission.{Color.}AsWinColor:=Full
              else If LifeColorStyle=1 then
                   Emission.Color:=pointmake(i/(LifeSize+1),j/(LifeSize+1),k/(LifeSize+1))
              else If LifeColorStyle=2 then begin
              {Desolate Growing Full Overpopulated}
                  Case CLM[I, J, K] of
                  0:LifeCellMatrix[Counter].Visible:=False;
                  {Emission.AsWinColor:=Desolate;}
                  2:Emission.AsWinColor:=Growing;
                  3:Emission.AsWinColor:=Full;
                  4:Emission.AsWinColor:=Overpopulated;
                  else LifeCellMatrix[Counter].Visible:=False;
                  end;
                  end;
                Diffuse.Color:=Emission.Color;
                Ambient.Color:=Emission.Color;
                TagFloat:=I*J*K;
              end;
            end;
          end;{Color options done in Setit( )}
          end;
        end;
      end;
    end;
    {make non used invisible}
    If Counter < TotalCounter then
      for Killer:= Counter+1 to TotalCounter do
        LifeCellMatrix[Killer].Visible:=False;
    if (bSlowDown = true) then
    begin
      Application.ProcessMessages;
      val(NumberEdit.Text, NapTime, ErrorCode);
      if (ErrorCode > 0) then NapTime := 0;
      Application.ProcessMessages;
      Sleep(NapTime); {MsgWaitForMultipleObjects}
    end;
    if ((OldHamming = Hamming) and (OldCellCount = CellCount)) then
    begin
      if ((LastHamming = Hamming) and (LastCellCount = CellCount)
          and
          (OldHamming = Hamming) and (OldCellCount = CellCount)) then
        {IWannaQuit := True;}
        StopLifeBtnClick(self);
      LastHamming := OldHamming; LastCellCount := OldCellCount;
    end;
    OldHamming := Hamming;
    OldCellCount := CellCount;
    Hamming := 0;
    CellCount := 0;
    If Pausing then UnPaused:=False;    
  end; { of Repeat loop  }
  until (IWannaQuit = True);
  {set OpenDialog1 filter to display Life's only}
  {Save the InImage?}
  If LifeSize=9 then   LifeFileExt := 'l01'
  else If LifeSize=24 then   LifeFileExt := 'l02'
  else If LifeSize=49 then   LifeFileExt := 'l05'
  else If LifeSize=99 then   LifeFileExt := 'l10'
  else If LifeSize=149 then   LifeFileExt := 'l15'
  else If LifeSize=199 then   LifeFileExt := 'l20'
  else If LifeSize=254 then   LifeFileExt := 'l25'
  else If LifeSize=511 then   LifeFileExt := 'l50'  ;
  SaveDialog1.InitialDir:=nLifeDir;
  SaveDialog1.filter := 'Life files|*.'+LifeFileExt;
    {  'Life files|*.l10;*.l25;*.l50;*.l1x';}
  SaveDialog1.DefaultExt := LifeFileExt;
  if SaveStartCB.Checked then begin
    SaveDialog1.filename := 'RandomLife.'+LifeFileExt;
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
        {$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
          for I := 0 to LifeSize do begin
            for J := 0 to LifeSize do begin
              for K := 0 to LifeSize do begin
                ImageByte := AAM[I, J, K];
                write(fil, ImageByte);
          end;end;end;
          CloseFile(fil);
        end;
  end; end; end;
  if SaveEndCB.Checked then begin
    SaveDialog1.filename := 'NewLife.'+LifeFileExt;
    if SaveDialog1.execute then begin
      if (not (SaveDialog1.filename = '')) then begin
        FileTodo := SaveDialog1.filename;
        AssignFile(fil, FileTodo);
        {$I-}rewrite(fil); {$I+}
        i := IOresult;
        if i <> 0 then EXIT {ReportError( i )} else
        begin
          for I := 0 to LifeSize do begin
            for J := 0 to LifeSize do begin
              for K := 0 to LifeSize do begin
                ImageByte := BeforeaLifeMatrix[I, J, K];
                write(fil, ImageByte);
          end;end;end;
          CloseFile(fil);
        end;
      end;
    end;
  end;
end; { of procedure GAIA S}
(********************************************************************)

(********************************************************************)



procedure Talife3dForm.StopLifeBtnClick(Sender: TObject);
begin
  Pausing :=False;
  UnPaused:=True;
  Application.ProcessMessages;
  IWannaQuit:=True;
  RunBtn.Enabled:=False;
  PauseStepBtn.Enabled:=False;
  Size1.Enabled:=True;
  Run1.Enabled:=True;
  RunLifeBtn.Enabled:=True;
  CellularRules1.Enabled:=True;
  Viewliffiles1.Enabled:=True;
end;
{===============================================================}
procedure Talife3dForm.AllSizesOff;
Begin
lifesize10.Checked:=False;
lifesize50.Checked:=False;
lifesize25.Checked:=False;
lifesize100.Checked:=False;
lifesize150.Checked:=False;
lifesize200.Checked:=False;
lifesize256.Checked:=False;
lifesize300.Checked:=False;
lifesize400.Checked:=False;
lifesize512.Checked:=False;
End;
{If there is not enough memory available to reallocate the variable,
 an EOutOfMemory exception is raised. }
procedure Talife3dForm.lifesize10Click(Sender: TObject);
begin
  AllSizesOff;LifeSize:=9;  lifesize10.Checked:=True;
  SetLength(BeforeaLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(AAM, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(CLM, LifeSize+1, LifeSize+1, LifeSize+1);
  SmashTheSliders;
end;
procedure Talife3dForm.lifesize25Click(Sender: TObject);
begin
  AllSizesOff;LifeSize:=24;  lifesize25.Checked:=True;
  SetLength(BeforeaLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(AAM, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(CLM, LifeSize+1, LifeSize+1, LifeSize+1);
  SmashTheSliders;
end;
procedure Talife3dForm.lifesize50Click(Sender: TObject);
begin
  AllSizesOff;LifeSize:=49;  lifesize50.Checked:=True;
  SetLength(BeforeaLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(AAM, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(CLM, LifeSize+1, LifeSize+1, LifeSize+1);
  SmashTheSliders;
end;
procedure Talife3dForm.lifesize100Click(Sender: TObject);
begin
  AllSizesOff;LifeSize:=99; lifesize100.Checked:=True;
  SetLength(BeforeaLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(AAM, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(CLM, LifeSize+1, LifeSize+1, LifeSize+1);
  SmashTheSliders;
end;
procedure Talife3dForm.lifesize150Click(Sender: TObject);
begin
  AllSizesOff;LifeSize:=149;  lifesize150.Checked:=True;
  SetLength(BeforeaLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(AAM, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(CLM, LifeSize+1, LifeSize+1, LifeSize+1);
  SmashTheSliders;
end;
procedure Talife3dForm.lifesize200Click(Sender: TObject);
begin
  AllSizesOff;LifeSize:=199;  lifesize200.Checked:=True;
  SetLength(BeforeaLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(AAM, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(CLM, LifeSize+1, LifeSize+1, LifeSize+1);
  SmashTheSliders;
end;
procedure Talife3dForm.lifesize256Click(Sender: TObject);
begin
  AllSizesOff;LifeSize:=254;  lifesize256.Checked:=True;
  SetLength(BeforeaLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(AAM, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(CLM, LifeSize+1, LifeSize+1, LifeSize+1);
  SmashTheSliders;
end;
procedure Talife3dForm.lifesize300Click(Sender: TObject);
begin
  AllSizesOff;LifeSize:=299;  lifesize300.Checked:=True;
  SetLength(BeforeaLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(AAM, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(CLM, LifeSize+1, LifeSize+1, LifeSize+1);
  SmashTheSliders;
end;

procedure Talife3dForm.lifesize400Click(Sender: TObject);
begin
  AllSizesOff;LifeSize:=399;  lifesize400.Checked:=True;
  SetLength(BeforeaLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(AAM, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(CLM, LifeSize+1, LifeSize+1, LifeSize+1);
  SmashTheSliders;
end;
procedure Talife3dForm.lifesize512Click(Sender: TObject);
begin
  AllSizesOff;LifeSize:=511;  lifesize512.Checked:=True;
  SetLength(BeforeaLifeMatrix, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(AAM, LifeSize+1, LifeSize+1, LifeSize+1);
  SetLength(CLM, LifeSize+1, LifeSize+1, LifeSize+1);
  SmashTheSliders;
end;


procedure Talife3dForm.UseSlowSteps1Click(Sender: TObject);
begin
  bSlowDown:= (not bSlowDown);
  UseSlowSteps1.Checked:=bSlowDown;
end;



procedure Talife3dForm.AllColorsOff;
Begin
  ColorTwoTone.Checked:=False;
  ColorRGB1.Checked:=False;
  ColorAll.Checked:=False;
End;
procedure Talife3dForm.ColorTwoToneClick(Sender: TObject);
begin
  AllColorsOff; LifeColorStyle:=0; ColorTwoTone.Checked:=True;
end;
procedure Talife3dForm.ColorRGB1Click(Sender: TObject);
begin
   AllColorsOff;  LifeColorStyle:=1;  ColorRGB1.Checked:=True;
end;
procedure Talife3dForm.ColorAllClick(Sender: TObject);
begin
   AllColorsOff; LifeColorStyle:=2; ColorAll.Checked:=True;
end;

procedure Talife3dForm.LifesEdgeOff;
Begin
Torus1.Checked:=False;
Fallover1.Checked:=False;
End;
procedure Talife3dForm.Fallover1Click(Sender: TObject);
begin
  LifesEdgeOff;
  bTorusLife:=False;
  Fallover1.Checked:=True;
end;
procedure Talife3dForm.Torus1Click(Sender: TObject);
begin  {Torus  Warp Around  Wrap Around }
  LifesEdgeOff;
  bTorusLife:=True;
  Torus1.Checked:=True;
end;
procedure Talife3dForm.Infinity1Click(Sender: TObject);
begin
  Application.HelpContext(1801);
  {See Help file for link to Bontes Life}
end;

procedure Talife3dForm.CellularRulesOff;
begin
  Conway1.Checked:=False;
  Universes.Checked:=False;
end;
procedure Talife3dForm.Conway1Click(Sender: TObject);
begin
  CellularRulesOff;
  Conway1.Checked:=True;
  CellularRulesStyle:=0;
  StatusBarred.Panels[0].Text := '23/3';
end;

procedure Talife3dForm.UniversesClick(Sender: TObject);
begin
  if aUniversesForm.ShowModal = mrOK then
  BEGIN
    CellularRulesOff;
    Universes.Checked:=True;
    CellularRulesStyle:=1;
    StatusBarred.Panels[0].Text :=RunBooleanRules;
  END else
  begin
    CellularRulesOff;
    Conway1.Checked:=True;
    CellularRulesStyle:=0;
    StatusBarred.Panels[0].Text := '23/3';
  end;
end;

procedure Talife3dForm.RedMaxTrackBarChange(Sender: TObject);
begin
  Label1.Caption:=Inttostr(RedMaxTrackBar.Position);
  RedXMax:=RedMaxTrackBar.Position;
  RedXMin:=RedMinTrackBar.Position;
end;

procedure Talife3dForm.RedMinTrackBarChange(Sender: TObject);
begin
  Label2.Caption:=Inttostr(RedMinTrackBar.Position);
  RedXMax:=RedMaxTrackBar.Position;
  RedXMin:=RedMinTrackBar.Position;
end;

procedure Talife3dForm.GreenMaxTrackBarChange(Sender: TObject);
begin
  Label4.Caption:=Inttostr(GreenMaxTrackBar.Position);
  GreenYMax:=GreenMaxTrackBar.Position;
  GreenYMin:=GreenMinTrackBar.Position;
end;

procedure Talife3dForm.GreenMinTrackBarChange(Sender: TObject);
begin
  Label3.Caption:=Inttostr(GreenMinTrackBar.Position);
  GreenYMax:=GreenMaxTrackBar.Position;
  GreenYMin:=GreenMinTrackBar.Position;
end;

procedure Talife3dForm.BlueMaxTrackBarChange(Sender: TObject);
begin
  Label6.Caption:=Inttostr(BlueMaxTrackBar.Position);
  BlueZMax:=BlueMaxTrackBar.Position;
  BlueZMin:=BlueMinTrackBar.Position;
end;

procedure Talife3dForm.BlueMinTrackBarChange(Sender: TObject);
begin
  Label5.Caption:=Inttostr(BlueMinTrackBar.Position);
  BlueZMax:=BlueMaxTrackBar.Position;
  BlueZMin:=BlueMinTrackBar.Position;
end;
procedure Talife3dForm.SmashTheSliders;
begin {LifeSize}
RedMaxTrackBar.Max:=LifeSize; RedMaxTrackBar.Position:=LifeSize;
GreenMaxTrackBar.Max:=LifeSize; GreenMaxTrackBar.Position:=LifeSize;
BlueMaxTrackBar.Max:=LifeSize; BlueMaxTrackBar.Position:=LifeSize;
RedMinTrackBar.Max:=LifeSize;RedMinTrackBar.Position:=0;
GreenMinTrackBar.Max:=LifeSize;GreenMinTrackBar.Position:=0;
BlueMinTrackBar.Max:=LifeSize;BlueMinTrackBar.Position:=0;
RedXMax:=RedMaxTrackBar.Position;  RedXMin:=RedMinTrackBar.Position;
GreenYMax:=GreenMaxTrackBar.Position; GreenYMin:=GreenMinTrackBar.Position;
BlueZMax:=BlueMaxTrackBar.Position; BlueZMin:=BlueMinTrackBar.Position;
End;

procedure Talife3dForm.DisplayMinMaxBar1Click(Sender: TObject);
begin
  If aMinMaxPanel.Visible then
  begin         { alClient  alTop   alNone}
    aLifePanel.Align:=alNone;
    aMinMaxPanel.Align:=alNone;
    Viewer.Align:=alNone;
    aLifePanel.Align:=alTop;
    {MinMaxPanel.Align:=alTop;}aMinMaxPanel.Visible:=False;
    Viewer.Align:=alClient;
    DisplayMinMaxBar1.Checked:=False;
  end else
  begin
    aMinMaxPanel.Visible:=True;
    aLifePanel.Align:=alNone;
    aMinMaxPanel.Align:=alNone;
    Viewer.Align:=alNone;
    aLifePanel.Align:=alTop;
    aMinMaxPanel.Align:=alTop;
    Viewer.Align:=alClient;
    DisplayMinMaxBar1.Checked:=True;
    SmashTheSliders;
  end;
end;

procedure Talife3dForm.Viewliffiles1Click(Sender: TObject);
begin
  LifeFilesForm.Show;
end;

procedure Talife3dForm.CellSizeTBChange(Sender: TObject);
begin
   CubeSizer:=(11-CellSizeTB.Position)/10;
   SizeLabel.Caption:=inttostr(11-CellSizeTB.Position);
end;

procedure Talife3dForm.RunBtnClick(Sender: TObject);
begin
  Pausing :=False;
  UnPaused:=True;  
end;

procedure Talife3dForm.PauseStepBtnClick(Sender: TObject);
begin
  Pausing :=True;
  UnPaused:=True;
end;



End.
