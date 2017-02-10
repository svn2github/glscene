unit u_Main;

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
  //GLS
  GLWin32Viewer,
  GLScene,
  GLCadencer,
  GLObjects,
  GLTexture,
  GLAsyncTimer,
  GLHUDObjects,
  GLShadowVolume,
  GLContext,
  GLMaterial,
  GLColor,
  GLKeyboard;


type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  public
    Scene: TGLScene;
    Camera: TGLCamera;
    Cad: TGLCadencer;
    Viewer: TGLSceneViewer;
    Ast: TGLAsyncTimer;
    Light: TGLLightSource;
    Back: TGLHUDSprite;
    Dummy: TGLDummyCube;
    Plane: TGLPlane;
    Sphere: TGLSphere;
    Cube: TGLCube;
    Shadow: TGLShadowVolume;
    Points: TGLPoints;
    Lines: TGLLines;

    procedure CadProgress(Sender: TObject; const dt,nt: double);
    procedure vpMouseDown(Sender: TObject; but: TMouseButton;
      sh: TShiftState; X,Y: Integer);
    procedure Time(Sender: TObject);
  end;


var
  Form1: TForm1;
  m_pos: TPoint;
  m_move: boolean;

implementation

{$R *.dfm}

//  FormCreate
//
procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  Color : TColor;

begin
  Scene := TGLScene.Create(self);
  Cad := TGLCadencer.Create(self);
    cad.Scene := Scene;
    cad.OnProgress := CadProgress;

  Viewer := TGLSceneViewer.Create(self);
    Viewer.Parent := self;
    Viewer.Buffer.BackgroundColor := $ffffff;
    Viewer.Buffer.ContextOptions := Viewer.Buffer.ContextOptions +
      [roNoColorBufferClear, roStencilBuffer];
    Viewer.Buffer.AntiAliasing := aa4x;
    Viewer.Align := alClient;
    Viewer.SendToBack;
    Viewer.OnMouseDown := vpMouseDown;

  Back := TGLHUDSprite.CreateAsChild(scene.Objects);
    back.Width := Width;
    back.Height := Height;
    back.Position.SetPoint(Width div 2, Height div 2, 0);
    back.Material.Texture.Image.LoadFromFile('back.bmp');
    back.Material.Texture.Disabled := false;

  Dummy := TGLDummyCube.CreateAsChild(Scene.Objects);

  Light := TGLLightSource.CreateAsChild(Scene.Objects);
  Light.Position.SetPoint(4, 10, 4);

  Sphere := TGLSphere.CreateAsChild(Dummy);
    Sphere.Position.SetPoint(0.2, 1.2, 0);
    Sphere.Radius := 0.3;
    Sphere.Slices := 10;
    Sphere.Stacks := 8;

  Cube := TGLCube.CreateAsChild(Dummy);
    cube.CubeDepth := 0.8;
    cube.CubeHeight := 0.1;
    cube.CubeWidth := 0.8;
    cube.Position.SetPoint(1, 0.2, 0);

  Plane := TGLPlane.CreateAsChild(Dummy);
    Plane.Direction.SetVector(0.3, 1, 0.1);
    Plane.Width := 3;
    Plane.Height := 4;

  Camera := TGLCamera.CreateAsChild(Dummy);
    Camera.Position.SetPoint(2, 3, 4);
    Camera.FocalLength := 75;
    Camera.TargetObject := Dummy;
    Viewer.Camera := Camera;

  Shadow := TGLShadowVolume.CreateAsChild(Dummy);
    Shadow.Mode := svmDarkening;
    Shadow.Lights.AddCaster(light);
    Shadow.Occluders.AddCaster(Sphere);
    Shadow.Occluders.AddCaster(cube);

  Ast := TGLAsyncTimer.Create(form1);
    Ast.Interval := 500;
    Ast.OnTimer := Time;
    Ast.Enabled := true;

  for I := 0 to 10000 do
  begin
    Points := TGLPoints.CreateAsChild(Dummy);
    Points.Style := psSmooth;
    Points.PointParameters.Enabled := True;
    Points.PointParameters.MinSize := 1;
    Points.PointParameters.MaxSize := 4;
    Points.PointParameters.DistanceAttenuation.SetVector(0, 0.001, 0);
    Points.Position.X := Random();
    Points.Position.Y := Random();
    Points.Position.Z := Random();
    Points.Colors.AddPoint(Random, Random, Random); // Temporarily random colors
  end;
  Lines := TGLLines.CreateAsChild(Dummy);
end;


procedure TForm1.FormResize(Sender: TObject);
begin
  Viewer.Visible := True;
end;

// cadProgress
//
procedure TForm1.CadProgress;
begin
  Dummy.turn(-dt * 10);
  if m_move then begin
    with mouse.CursorPos do
      Camera.MoveAroundTarget((m_pos.Y - y) * 0.3, (m_pos.X - x) * 0.3);
    m_pos := mouse.CursorPos;
    // MouseUp
    if not iskeydown(vk_lbutton) then
      m_move := false;
    end;
end;


//   MouseDown
//
procedure TForm1.vpMouseDown;
begin
  m_pos := mouse.CursorPos;
  m_move := true;
end;


//  Timer
//
procedure TForm1.Time(Sender:TObject);
begin
  caption := 'runTime: ' + Viewer.FramesPerSecondText(2);
  Viewer.ResetPerformanceMonitor;
end;

end.

