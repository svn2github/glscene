unit demo_main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls,
   
  GLVectorFileObjects, GLScene, GLObjects,
  GLCadencer, GLWin32Viewer, GLGraph, GLVectorGeometry,
  GLBaseClasses, GLCoordinates, GLCrossPlatform, GLFileSMD,
  GLGeomObjects, GLVectorTypes,
  NewtonImport,
  NewtonRagdoll;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    Actor1: TGLActor;
    camera_cube: TGLDummyCube;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    GLLightSource1: TGLLightSource;
    GLCube1: TGLCube;
    GLCube2: TGLCube;
    Button2: TButton;
    block: TGLCube;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;

  world: PNewtonWorld;
  plane_body, box_body, block_body: PNewtonBody;
  kicked: boolean;
  r: TNewtonRagdoll;
  mx, my: Integer;

implementation

{$R *.DFM}

procedure NewtonApplyForceAndTorqueCallback(const body: PNewtonBody;
  timestep: single; threadIndex: Integer); cdecl;
var
  M: single;
  I: TVector3f;
  F: TVector3f;
begin
  NewtonBodyGetMassMatrix(body, @M, @I.X, @I.Y, @I.Z);
  F := AffineVectorMake(0, -9.81 * M, 0);
  NewtonBodyAddForce(body, @F);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  M: TMatrix;
begin
  world := NewtonCreate(nil, nil);

  // Создаем пол
  with GLCube1 do
    plane_body := NewtonCreateBody(world, NewtonCreateBox(world, CubeWidth,
      CubeHeight, CubeDepth, nil));
  M := GLCube1.Matrix;
  NewtonBodySetMatrix(plane_body, @M);

  with GLCube2 do
    box_body := NewtonCreateBody(world, NewtonCreateBox(world, CubeWidth,
      CubeHeight, CubeDepth, nil));
  M := GLCube2.Matrix;
  NewtonBodySetMatrix(box_body, @M);

  // Создаем блок, который потом будем бросать в модель
  with block do
  begin
    block_body := NewtonCreateBody(world, NewtonCreateBox(world, CubeWidth,
      CubeHeight, CubeDepth, nil));
    NewtonBodySetMassMatrix(block_body, 1, CubeWidth, CubeHeight, CubeDepth);
  end;
  M := block.Matrix;
  NewtonBodySetMatrix(block_body, @M);
  NewtonBodySetForceAndTorqueCallback(block_body,
    NewtonApplyForceAndTorqueCallback);

  // Загружаем модель
  Actor1.LoadFromFile('1.smd');
  Actor1.AddDataFromFile('run.smd');
  Actor1.Animations[1].MakeSkeletalTranslationStatic;
  Actor1.SwitchToAnimation('run');

  kicked := false;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  I: Integer;
  v: TVector3f;
  M: TMatrix;
begin
  NewtonUpdate(world, deltaTime);
  // Пришло время ударить модель блоком
  if (not kicked) and (newTime > 0.5) then
  begin
    // Создаем регдолл и загружаем его из файла
    r := TNewtonRagdoll.Create(Actor1, world, 2);
    // r.LoadFromFile('1.rgd');
    // Сообщаем блоку импульс
    MakeVector(v, 0, 0, 300);
    NewtonBodyGetMatrix(block_body, @M);
    NewtonBodyAddImpulse(block_body, @v, @M.W);
    // Включаем регдолл
    r.Enabled := true;
    kicked := true;
  end;

  NewtonBodyGetMatrix(block_body, @M);
  block.Matrix := M;

  if r <> nil then
    r.Conform;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // Выключаем регдолл. Анимация восстановится сама
  r.Enabled := false;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Application.MessageBox('Через секунду включится регдолл!', 'Внимание');
  GLCadencer1.Enabled := true;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := false;
  GLCadencer1.Enabled := false;
  GLSceneViewer1.Free;

  r.Free;
  NewtonDestroy(world);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift <> [] then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('Newton Ragdoll -- %.1f FPS',
    [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
