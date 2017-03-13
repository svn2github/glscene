unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.jpeg,
   
  GLScene,
  GLObjects,
  GLCoordinates,
  GLCadencer,
  GLWin32Viewer,
  GLCrossPlatform,
  GLBaseClasses,
  GLColor,
  GLVectorGeometry,
  GLGeomObjects,
  GLFile3DS,
  GLMesh,
  GLState,
  GLMaterial,
  GLVectorFileObjects,
  GLVectorLists,
  GLKeyboard,

  PhysX;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Timer1: TTimer;
    cloth: TGLFreeForm;
    map: TGLFreeForm;
    hull: TGLFreeForm;
    hull2: TGLFreeForm;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
     
  public
    mx, my: Integer;
     
  end;

const
  N = 10;

  MeshObj: array [0 .. 7] of TAffineVector = ((X: - 1; Y: - 1; Z: - 1), (X: 1;
    Y: - 1; Z: - 1), (X: - 1; Y: 1; Z: - 1), (X: 1; Y: 1; Z: - 1), (X: - 1;
    Y: - 1; Z: 1), (X: 1; Y: - 1; Z: 1), (X: - 1; Y: 1; Z: 1), (X: 1;
    Y: 1; Z: 1));
  MeshFaces: array [0 .. 35] of Integer = (1, 2, 3, 0, 2, 1, 5, 4, 1, 1, 4, 0,
    1, 3, 5, 3, 7, 5, 3, 2, 7, 2, 6, 7, 2, 0, 6, 4, 6, 0, 7, 4, 5, 7, 6, 4);

  ver: array [0 .. 3] of TAffineVector = ((X: - 24.9891395568848;
    Y: - 28.4316024780273; Z: 0), (X: 31.8221054077148; Y: - 28.4316024780273;
    Z: 0), (X: - 24.9891395568848; Y: 9.85981369018555; Z: 0),
    (X: 31.8221054077148; Y: 9.85981369018555; Z: 0));

  ind: array [0 .. 5] of Integer = (2, 0, 3, 1, 3, 0);

var
  Form1: TForm1;
  nxCube, nxCapsule, nxSphere, nxCylinder: array [0 .. N - 1] of Integer;
  SceneCube: array [0 .. N - 1] of TGLCube;
  SceneCapsule: array [0 .. N - 1] of TGLCapsule;
  SceneSphere: array [0 .. N - 1] of TGLSphere;
  SceneCylinder: array [0 .. N - 1] of TGLCylinder;
  nxCloth, nxHull, nxHull2: Integer;
  arr: array [0 .. 3] of TAffineVector;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  nxCreateWorld(False);
  Randomize;
  hull.Material.FrontProperties.Diffuse.Color := VectorMake(Random(255) / 255,
    Random(255) / 255, Random(255) / 255, Random(255) / 255);
  hull2.Material.FrontProperties.Diffuse.Color := VectorMake(Random(255) / 255,
    Random(255) / 255, Random(255) / 255, Random(255) / 255);
  map.Material.FrontProperties.Diffuse.Color := VectorMake(Random(255) / 255,
    Random(255) / 255, Random(255) / 255, Random(255) / 255);
  for i := 0 to N - 1 do
  begin
    SceneCube[i] := TGLCube.CreateAsChild(GLScene1.Objects);
    SceneCapsule[i] := TGLCapsule.CreateAsChild(GLScene1.Objects);
    SceneSphere[i] := TGLSphere.CreateAsChild(GLScene1.Objects);
    SceneCylinder[i] := TGLCylinder.CreateAsChild(GLScene1.Objects);

    SceneCube[i].Material.FrontProperties.Diffuse.Color :=
      VectorMake(Random(255) / 255, Random(255) / 255, Random(255) / 255,
      Random(255) / 255);
    SceneCapsule[i].Material.FrontProperties.Diffuse.Color :=
      VectorMake(Random(255) / 255, Random(255) / 255, Random(255) / 255,
      Random(255) / 255);
    SceneSphere[i].Material.FrontProperties.Diffuse.Color :=
      VectorMake(Random(255) / 255, Random(255) / 255, Random(255) / 255,
      Random(255) / 255);
    SceneCylinder[i].Material.FrontProperties.Diffuse.Color :=
      VectorMake(Random(255) / 255, Random(255) / 255, Random(255) / 255,
      Random(255) / 255);

    nxCube[i] := nxBodyCreateCube(0.5, 0.5, 0.5, 1);
    nxCapsule[i] := nxBodyCreateCapsule(1, 0.5, 1);
    nxSphere[i] := nxBodyCreateSphere(0.5, 1);
    nxCylinder[i] := nxBodyCreateCylinder(0.5, 0.5, 16, 1);

    nxBodySetPosition(nxCube[i], 0, i * 2, 0);
    nxBodySetPosition(nxCapsule[i], 1, i * 2, 0);
    nxBodySetPosition(nxSphere[i], 2, i * 2, 0);
    nxBodySetPosition(nxCylinder[i], 3, i * 2, 0);
  end;

  cloth.LoadFromFile('media/cloth.3DS');
  nxCloth := nxCreateClothFromFreeForm(cloth);
  nxClothAttachVertexToPos(nxCloth, 0, 0, 10, 5);
  nxClothAttachVertexToPos(nxCloth, 88, 5, 10, 5);

  map.LoadFromFile('media/map.3DS');
  nxCreateTriMeshFromFreeForm(map);

  hull.LoadFromFile('media/hull.3DS');
  nxHull := nxCreateHullFromFreeForm(hull, 1);
  nxBodySetPosition(nxHull, 10, 10, 10);

  hull2.LoadFromFile('media/hull2.3DS');
  nxHull2 := nxCreateHullFromFreeForm(hull2, 1);
  nxBodySetPosition(nxHull2, 10, 20, -10);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  i: Integer;
begin
  for i := 0 to N - 1 do
  begin
    nxPositionSceneObject(SceneCube[i], nxCube[i]);
    nxPositionSceneObject(SceneCapsule[i], nxCapsule[i]);
    nxPositionSceneObject(SceneSphere[i], nxSphere[i]);
    nxPositionSceneObject(SceneCylinder[i], nxCylinder[i]);
  end;
  nxPositionSceneObject(hull, nxHull);
  nxPositionSceneObject(hull2, nxHull2);
  nxRenderPhysic(GLSceneViewer1.FramesPerSecond, 0);
  nxDrawCloth(cloth, nxCloth);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
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


procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  for i := 0 to N - 1 do
  begin
    nxBodyAddForce(nxCube[i], 0, 1, 0, 1);
    nxBodyAddForce(nxCapsule[i], 0, 1, 0, 1);
    nxBodyAddForce(nxCylinder[i], 0, 1, 0, 1);
    nxBodyAddForce(nxSphere[i], 0, 1, 0, 1);
  end;
  nxBodyAddForce(nxHull, 0, 1, 0, 1);
  nxBodyAddForce(nxHull2, 0, 1, 0, 1);
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 	GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

end.
