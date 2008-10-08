unit Unit1;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GLScene,
  GLObjects,
  GLTexture,
  GLWin32Viewer,
  TGA,
  GLCadencer,
  GLVectorFileObjects,
  GLFileObj,
  JPEG,
  StdCtrls,
  GLShadowVolume,
  Math,
  GLPhongShader,
  GLSLProjectedTextures,
  GLGeomObjects,
  ExtCtrls,
  GLUtils,
  GLFile3DS,
  GLFileLMTS,
  OpenGL1x,
  GLContext,
  VectorGeometry,
  GLUserShader,
  GLProjectedTextures,
  GLCrossPlatform,
  GLGraphics, GLMaterial, GLCoordinates, BaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    GLLightSource2: TGLLightSource;
    GLArrowLine1: TGLArrowLine;
    Timer1: TTimer;
    GLDummyCube3: TGLDummyCube;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLSLTextureEmitter1: TGLSLTextureEmitter;
    GLSLProjectedTextures1: TGLSLProjectedTextures;
    GLFreeForm1: TGLFreeForm;
    GLCube1: TGLCube;
    GLSLTextureEmitter2: TGLSLTextureEmitter;
    procedure GLCamera1CustomPerspective(const viewport: TRectangle; Width, Height, DPI: Integer; var viewPortRadius: Single);
    procedure GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    mx, my: Integer;
    sdir: Integer;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
var
  I: Integer;
begin
  for I := 1 to glslProjectedTextures1.Emitters.Count - 1 do
    glslProjectedTextures1.Emitters[I].Emitter.turn(DeltaTime * (I + 1) * 10);

  GLSceneViewer1.invalidate;
  GLArrowLine1.position.Y := GLArrowLine1.position.Y + sdir * DeltaTime;
  if GLArrowLine1.position.Y > 20 then
  begin
    GLArrowLine1.position.Y := 20;
    sdir := -10;
  end;
  if GLArrowLine1.position.Y < 10 then
  begin
    GLArrowLine1.position.Y := 10;
    sdir := 10;
  end;

end;

procedure TForm1.GLCamera1CustomPerspective(const viewport: TRectangle; Width, Height, DPI: Integer; var viewPortRadius: Single);
begin
  gluPerspective(GLCamera1.FocalLength, Width / Height, GLCamera1.NearPlaneBias, GLCamera1.DepthOfView);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := GLSceneViewer1.FramesPerSecondText();
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  randomize;
  sdir := -10;
  GLCamera1.CameraStyle := cscustom;

  SetCurrentDir(ExtractFilePath(ParamStr(0)) + '..\..\media\');
  glslProjectedTextures1.Material.Texture.Image.LoadFromFile('flare1.bmp');
  glslProjectedTextures1.Material.Texture.Disabled := False;
  glslProjectedTextures1.Material.Texture.TextureWrap := twNone;
  glslProjectedTextures1.Material.Texture.MinFilter := miLinear;
  glslProjectedTextures1.Material.Texture.MagFilter := maLinear;
  glslProjectedTextures1.UseLightmaps := True;
  glcube1.Material.Texture.Image.LoadFromFile('ashwood.jpg');
  glcube1.Material.Texture.Disabled := False;


  GLFreeForm1.LoadFromFile('groundtest.lmts');
  for I := 0 to GLMaterialLibrary1.Materials.Count - 1 do
    GLMaterialLibrary1.Materials.Items[I].Material.MaterialOptions := [moNoLighting];

end;

end.

