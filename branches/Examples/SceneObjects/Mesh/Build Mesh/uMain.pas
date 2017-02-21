unit uMain;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Vcl.Imaging.jpeg,
  Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  GLScene,
  GLObjects,
  GLCoordinates,
  GLWin32Viewer,
  GLVectorGeometry,
  GLCrossPlatform,
  GLBaseClasses,
  GLMaterial,
  GLFileOBJ,
  DDSImage,
  GLHUDObjects,

  uMap;

type
  TfMain = class(TForm)
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    Camera: TGLCamera;
    Target: TGLDummyCube;
    MatLib: TGLMaterialLibrary;
    GLSceneLogo: TGLHUDSprite;
    procedure FormCreate(Sender: TObject);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
  private
     
  public
    Map   : TMap;
    mx,my : Integer;
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.FormCreate(Sender: TObject);
begin
  Map := TMap.CreateAsChild(Scene.Objects);

  with Map do
  begin
    Init;
    GLScene                  := Scene;
    MaterialLibrary          := MatLib;
    Material.MaterialLibrary := MatLib;

    Load('House\Level1.m2');
    Build;
  end;
  GLSceneLogo.Position.X := Viewer.Width -GLSceneLogo.Width /2;
  GLSceneLogo.Position.Y := Viewer.Height-GLSceneLogo.Height/2;
end;

procedure TfMain.ViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := x; my := y;
end;

procedure TfMain.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dx, dy : Integer;
begin
  dx := mx-x;
  dy := my-y;

  if Shift = [ssRight] then
     Camera.MoveAroundTarget(dy,dx);

  if Shift = [ssMiddle] then
     Camera.MoveTargetInEyeSpace(0,dx*0.12*Camera.DistanceToTarget/Camera.FocalLength,-dy*0.12*Camera.DistanceToTarget/Camera.FocalLength);
  mx := x; my := y;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Viewer);
  FreeAndNil(Scene);
end;

procedure TfMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(PowerSingle(1.1, WheelDelta / 120));
end;

procedure TfMain.FormResize(Sender: TObject);
begin
  Viewer.Buffer.Render;

  GLSceneLogo.Position.X := Viewer.Width -GLSceneLogo.Width /2;
  GLSceneLogo.Position.Y := Viewer.Height-GLSceneLogo.Height/2;
end;

end.
