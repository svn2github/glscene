unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
   
  GLScene,
  GLObjects,
  GLCoordinates,
  GLWin32Viewer,
  GLVectorGeometry,
  GLVectorTypes,
  GLKeyboard,
  GLCrossPlatform,
  GLBaseClasses,
  GLCadencer,
  GLSpline;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    dc_cam: TGLDummyCube;
    cam: TGLCamera;
    GLSphere1: TGLSphere;
    GLLightSource1: TGLLightSource;
    GLSphere2: TGLSphere;
    ship: TGLCube;
    cad: TGLCadencer;
    path: TGLLines;
    GLCube2: TGLCube;
    procedure FormResize(Sender: TObject);
    procedure vpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cadProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public

    procedure updShip;

  end;

var
  Form1: TForm1;
  m_turn: boolean;
  ship_path: TCubicSpline;
  ship_time: single;
  ship_up: TVector3f;
  ship_act: boolean;

implementation

{$R *.dfm}

//
// setup
//
procedure TForm1.FormCreate;
begin
  with path.Nodes do
  begin
    Clear;
    AddNode(-3, 0, 0);
    AddNode(-1.5, 0.85 / 4, 0);
    AddNode(0, 0.85, 0);
    AddNode(1.5, 0.85 / 4, 0);
    AddNode(3, 0, 0);
  end;

  ship_path := path.Nodes.CreateNewCubicSpline;
  ship_time := 0;
  setvector(ship_up, 0, 1, 0);
  ship_act := true;
  // cad.FixedDeltaTime := 1 / GetDeviceCaps(getDC(Handle), 116);
end;

//
// cadProgress
//
procedure TForm1.cadProgress;
begin
  if m_turn then
  begin
    cam.MoveAroundTarget((screen.Height div 2 - mouse.CursorPos.Y) * 0.2,
      (screen.Width div 2 - mouse.CursorPos.X) * 0.2);
    mouse.CursorPos := point(screen.Width div 2, screen.Height div 2);
    if (not iskeydown(vk_lbutton)) and (not iskeydown(vk_rbutton)) then
      m_turn := false;
  end;
  if ship_act then
  begin
    ship_time := ship_time + deltaTime;
    if ship_time > path.Nodes.Count - 1 then
      ship_time := 0;
  end;
  updShip;
end;

//
// updShip
//
procedure TForm1.updShip;
var
  X, Y, z: single;
begin
  if ship_path = nil then
    exit;
  ship.Position.SetPoint(ship_path.SplineVector(ship_time));
  ship.Up.setvector(ship_up);
  ship.Direction.setvector(ship_path.SplineSlopeVector(ship_time));
end;

//
// resize
//
procedure TForm1.FormResize;
begin
  vp.FieldOfView := 150;
end;

//
// rotation
//
procedure TForm1.vpMouseDown;
begin
  m_turn := true;
  mouse.CursorPos := point(screen.Width div 2, screen.Height div 2);
end;

//
// show
//
procedure TForm1.FormShow;
begin
  cad.Enabled := true;
end;

end.
