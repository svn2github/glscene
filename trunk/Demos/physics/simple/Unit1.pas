unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLODEManager, GLScene, GLObjects, GLCadencer,
  GLWin32Viewer, GLMisc, ODEImport, GLShadowPlane, StdCtrls, ComCtrls,
  ExtCtrls, Geometry;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    ODEObjects: TGLDummyCube;
    Panel1: TPanel;
    Button1: TButton;
    GroupBox1: TGroupBox;
    CheckBoxBounce: TCheckBox;
    TrackBarBounce: TTrackBar;
    CheckBoxSoftCFM: TCheckBox;
    TrackBarSoftCFM: TTrackBar;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    Button5: TButton;
    GLODEManager1: TGLODEManager;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure TrackBarSoftCFMChange(Sender: TObject);
    procedure TrackBarBounceChange(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    GLODEPlane1 : TGLODEPlane;
    mx,my : integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLODEManager1.Gravity.Y:=-9.81;

  GLODEPlane1:=TGLODEPlane(ODEObjects.AddNewChild(TGLODEPlane));
  with GLODEPlane1 do begin
    Manager:=GLODEManager1;
    Direction.SetVector(0,1,0);
    Position.SetPoint(0,-1,0);
    Surface.SurfaceMode:=[csmBounce];
    Surface.Bounce:=1;
    Surface.Bounce_Vel:=0.5;
    Surface.SoftCFM:=0.5;
    with TGLPlane(AddNewChild(TGLPlane)) do begin
      Width:=10;
      Height:=10;
    end;
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
const
  cStep = 0.001;
var
  i : integer;
begin
  for i:=0 to Trunc(deltaTime/cStep) do
    GLODEManager1.Step(cStep);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y,mx-x);
  mx:=x;
  my:=y;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  with TGLCube(ODEObjects.AddNewChild(TGLCube)) do begin
    Position.SetPoint(Random,Random+3,Random);
    CubeWidth:=Random+0.5;
    CubeHeight:=Random+0.5;
    CubeDepth:=Random+0.5;
    Material.FrontProperties.Diffuse.Color:=VectorMake(0.5*Random+0.5, 0.5*Random+0.5, 0.5*Random+0.5, 0.5*Random+0.5);

    with TGLODEDynamicBehaviour.Create(Behaviours) do begin
      Manager:=GLODEManager1;
      with TODEElementBox(AddNewElement(TODEElementBox)) do begin
        Position.SetPoint(0,0,0);
        BoxWidth:=CubeWidth;
        BoxHeight:=CubeHeight;
        BoxDepth:=CubeDepth;
      end;
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  with TGLODEDummy(ODEObjects.AddNewChild(TGLODEDummy)) do begin
    Manager:=GLODEManager1;
    Position.SetPoint(Random,Random+3,Random);
    Surface.RollingFrictionEnabled:=True;
    Surface.RollingFrictionCoeff:=0.0005;
    with TODEElementSphere(AddNewElement(TODEElementSphere)) do begin
      Position.SetPoint(0,0,0);
      Radius:=0.5*Random+0.25;
    end;
    Color.Red:=0.5*Random+0.5;
    Color.Green:=0.5*Random+0.5;
    Color.Blue:=0.5*Random+0.5;
    VisibleAtRuntime:=True;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  with TGLODEDummy(ODEObjects.AddNewChild(TGLODEDummy)) do begin
    Manager:=GLODEManager1;
    Position.SetPoint(Random,Random+3,Random);
    with TODEElementCapsule(AddNewElement(TODEElementCapsule)) do begin
      Position.SetPoint(0,0,0);
      Radius:=0.5*Random+0.25;
      Length:=Random+0.5;
    end;
    Color.Red:=0.5*Random+0.5;
    Color.Green:=0.5*Random+0.5;
    Color.Blue:=0.5*Random+0.5;

    VisibleAtRuntime:=True;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  with TGLCylinder(ODEObjects.AddNewChild(TGLCylinder)) do begin
    Position.SetPoint(Random,Random+3,Random);
    TopRadius:=0.5*Random+0.25;
    BottomRadius:=TopRadius;
    Height:=Random+0.5;
    Material.FrontProperties.Diffuse.Color:=VectorMake(0.5*Random+0.5, 0.5*Random+0.5, 0.5*Random+0.5, 0.5*Random+0.5);

    with TGLODEDynamicBehaviour.Create(Behaviours) do begin
      Manager:=GLODEManager1;
      with TODEElementCylinder(AddNewElement(TODEElementCylinder)) do begin
        Position.SetPoint(0,0,0);
        Radius:=TopRadius;
        Length:=Height;
      end;
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  with TGLODEDummy(ODEObjects.AddNewChild(TGLODEDummy)) do begin
    Manager:=GLODEManager1;
    Position.SetPoint(Random,Random+3,Random);
    with TODEElementBox(AddNewElement(TODEElementBox)) do begin
      Position.SetPoint(0.5*Random-0.25,0.5*Random-0.25,0.5*Random-0.25);
      BoxWidth:=Random+0.5;
      BoxHeight:=Random+0.5;
      BoxDepth:=Random+0.5;
    end;
    with TODEElementSphere(AddNewElement(TODEElementSphere)) do begin
      Position.SetPoint(0.5*Random-0.25,0.5*Random-0.25,0.5*Random-0.25);
      Radius:=0.5*Random+0.25;
    end;
    with TODEElementCapsule(AddNewElement(TODEElementCapsule)) do begin
      Position.SetPoint(0.5*Random-0.25,0.5*Random-0.25,0.5*Random-0.25);
      Radius:=0.5*Random+0.25;
      Length:=Random+0.5;
    end;
    Color.Red:=0.5*Random+0.5;
    Color.Green:=0.5*Random+0.5;
    Color.Blue:=0.5*Random+0.5;
    CalibrateCenterOfMass;

    VisibleAtRuntime:=True;
  end;
end;

procedure TForm1.CheckBoxClick(Sender: TObject);
var
  SurfaceMode : TSurfaceModes;
begin
  SurfaceMode:=[];
  if CheckBoxBounce.Checked then
    SurfaceMode:=SurfaceMode+[csmBounce];
  if CheckBoxSoftCFM.Checked then
    SurfaceMode:=SurfaceMode+[csmSoftCFM];
  GLODEPlane1.Surface.SurfaceMode:=SurfaceMode;
end;

procedure TForm1.TrackBarSoftCFMChange(Sender: TObject);
begin
  GLODEPlane1.Surface.SoftCFM:=TrackBarSoftCFM.Position/100;
end;

procedure TForm1.TrackBarBounceChange(Sender: TObject);
begin
  GLODEPlane1.Surface.Bounce:=TrackBarBounce.Position/100;
end;

end.
