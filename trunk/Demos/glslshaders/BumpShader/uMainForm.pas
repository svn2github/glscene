{:
  GLSL Bump Shader Demo<p>

  A demo that shows how to use the TGLSLBumpShader component.

  Version history:
    12/17/07 - DaStr - Bugfixed MultiLight stuff. Other small bigfixes...
    03/04/07 - DaStr - Added more objects
    30/03/07 - DaStr - Initial version
}
unit uMainForm;

interface

{$I GLScene.inc}

uses
  // VCL
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls,

  // GLScene
  GLTexture, GLCadencer, GLWin32Viewer, GLMisc, GLScene, GLObjects, GLPolyhedron,
  GLVectorFileObjects, GLGraph, GLGeomObjects, VectorGeometry, GLSLBumpShader,
  GLCustomShader, GLSLShader, GLCrossPlatform,

  // FileFormats
  TGA, GLFileMS3D, GLFile3DS, JPEG, DDS, GLFileMD2, GLFileSMD;

type
  TGLSLTestForm = class(TForm)
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    Cadencer: TGLCadencer;
    Camera: TGLCamera;
    Timer1: TTimer;
    Light: TGLLightSource;
    LightCube: TGLDummyCube;
    GLSphere1: TGLSphere;
    GLXYZGrid1: TGLXYZGrid;
    GLArrowLine1: TGLArrowLine;
    Panel1: TPanel;
    LightMovingCheckBox: TCheckBox;
    GUICube: TGLDummyCube;
    WorldCube: TGLDummyCube;
    Fighter: TGLActor;
    Teapot: TGLActor;
    Sphere_big: TGLActor;
    Sphere_little: TGLActor;
    MaterialLibrary: TGLMaterialLibrary;
    RollPitchTurnCheckBox: TCheckBox;
    ShaderEnabledCheckBox: TCheckBox;
    GLSphere2: TGLSphere;
    Light2: TGLLightSource;
    LightCube2: TGLDummyCube;
    MultiLightShaderCheckBox: TCheckBox;
    UseSpecularTextureCheckBox: TCheckBox;
    UseNormalTextureCheckBox: TCheckBox;
    MyBumpShader: TGLSLBumpShader;
    TrinityMatlib: TGLMaterialLibrary;
    GLCube1: TGLCube;
    GLDodecahedron1: TGLDodecahedron;
    GLSphere3: TGLSphere;
    procedure FormCreate(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure LightCubeProgress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure ShaderEnabledCheckBoxClick(Sender: TObject);
    procedure MultiLightShaderCheckBoxClick(Sender: TObject);
    procedure UseSpecularTextureCheckBoxClick(Sender: TObject);
    procedure UseNormalTextureCheckBoxClick(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  GLSLTestForm: TGLSLTestForm;
  mx, my:    Integer;
  MultiLightShader: TGLSLMLBumpShader;

implementation

{$R *.dfm}

procedure TGLSLTestForm.FormCreate(Sender: TObject);
const
  FILE_PATH = '..\..\media\';
var
  I: Integer;
begin
  //First load models
  TrinityMatlib.TexturePaths := FILE_PATH;
  Fighter.LoadFromFile(FILE_PATH + 'TRINITYrage.smd'); //Fighter
  Fighter.AddDataFromFile(FILE_PATH + 'walk.smd');
  Fighter.Animations[1].MakeSkeletalTranslationStatic;
  Fighter.AddDataFromFile(FILE_PATH + 'run.smd');
  Fighter.Animations[2].MakeSkeletalTranslationStatic;
  Fighter.AddDataFromFile(FILE_PATH + 'long_jump.smd');
  Fighter.AddDataFromFile(FILE_PATH + 'jump.smd');
  Fighter.AddDataFromFile(FILE_PATH + 'look_left_right.smd');
  Fighter.Animations[5].MakeSkeletalRotationDelta;
  Fighter.SwitchToAnimation(1);
{
  Fighter.LoadFromFile(FILE_PATH + 'waste.md2'); //Fighter
  Fighter.SwitchToAnimation(0, True);
  Fighter.AnimationMode := aamLoop;
  Fighter.Scale.Scale(3);
}
  Fighter.AnimationMode := aamLoop;
  Fighter.Scale.Scale(3);
//  Fighter.MeshObjects.BuildTangentSpace;

  Teapot.LoadFromFile(FILE_PATH + 'Teapot.3ds'); //Teapot
  Teapot.Scale.Scale(0.8);
  //  Teapot.MeshObjects.BuildTangentSpace; does not have texture coordinates...

  Sphere_big.LoadFromFile(FILE_PATH + 'Sphere_big.3DS'); //Sphere_big
  Sphere_big.Scale.Scale(70);
  Sphere_big.MeshObjects.BuildTangentSpace;

  Sphere_little.LoadFromFile(FILE_PATH + 'Sphere_little.3ds'); //Sphere_little
  Sphere_little.Scale.Scale(4);
  Sphere_little.MeshObjects.BuildTangentSpace;

  // Then load textures
  MaterialLibrary.LibMaterialByName('Earth').Material.Texture.Image.LoadFromFile(FILE_PATH + 'Earth.jpg');
  MaterialLibrary.LibMaterialByName('EarthGross').Material.Texture.Image.LoadFromFile(FILE_PATH + 'EarthSpec.dds');
  MaterialLibrary.LibMaterialByName('EarthNormals').Material.Texture.Image.LoadFromFile(FILE_PATH + 'EarthNormals.jpg');

  // Create Shader
  MultiLightShader := TGLSLMLBumpShader.Create(Self);
  MultiLightShader.LightSources := [1, 2];
  MultiLightShader.LightCompensation := 0.7;
  MultiLightShader.NormalTexture := MaterialLibrary.LibMaterialByName('EarthNormals').Material.Texture;
  MultiLightShader.SpecularTexture := MaterialLibrary.LibMaterialByName('EarthGross').Material.Texture;

  // Attach shader to the material
  MaterialLibrary.LibMaterialByName('Earth').Shader := MyBumpShader;
  for I := 0 to TrinityMatlib.Materials.Count - 1 do
    TrinityMatlib.Materials[I].Shader := MyBumpShader;
end;


procedure TGLSLTestForm.CadencerProgress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  Viewer.Invalidate;

  if RollPitchTurnCheckBox.Checked then
  begin
    Sphere_big.Turn(DeltaTime * 40);
    Sphere_big.Roll(DeltaTime * 40);
    Sphere_little.Pitch(DeltaTime * 20);
    Fighter.Roll(DeltaTime * 20);
    Teapot.Roll(-DeltaTime * 10);
    GLCube1.Pitch(-DeltaTime * 10);
    GLDodecahedron1.Pitch(DeltaTime * 10);
    GLSphere3.Roll(-DeltaTime * 10);
  end;
end;


procedure TGLSLTestForm.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;


procedure TGLSLTestForm.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) and (ssLeft in Shift) then
    Camera.AdjustDistanceToTarget(Power(1.01, Y - my))
  else
  if (ssRight in Shift) or (ssLeft in Shift) then
    Camera.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;


procedure TGLSLTestForm.Timer1Timer(Sender: TObject);
begin
  Caption := 'GLSL Bump Shader Demo  -  ' + Viewer.FramesPerSecondText;
  Viewer.ResetPerformanceMonitor;
end;


procedure TGLSLTestForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Cadencer.Enabled := False;
end;


procedure TGLSLTestForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TGLSLTestForm.LightCubeProgress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  if LightMovingCheckBox.Checked then
    LightCube.MoveObjectAround(Camera.TargetObject, sin(NewTime) * DeltaTime * 10, DeltaTime * 20);
end;


procedure TGLSLTestForm.ShaderEnabledCheckBoxClick(Sender: TObject);
begin
  MyBumpShader.Enabled := ShaderEnabledCheckBox.Checked;
  MultiLightShader.Enabled := ShaderEnabledCheckBox.Checked;
end;

procedure TGLSLTestForm.MultiLightShaderCheckBoxClick(Sender: TObject);
var
  I: Integer;
begin
  if MultiLightShaderCheckBox.Checked then
  begin
    MaterialLibrary.LibMaterialByName('Earth').Shader := MultiLightShader;
    for I := 0 to TrinityMatlib.Materials.Count - 1 do
      TrinityMatlib.Materials[I].Shader := MultiLightShader;
  end
  else
  begin
    MaterialLibrary.LibMaterialByName('Earth').Shader := MyBumpShader;
    for I := 0 to TrinityMatlib.Materials.Count - 1 do
      TrinityMatlib.Materials[I].Shader := MyBumpShader;
  end;

  Light2.Shining := MultiLightShaderCheckBox.Checked;
  LightCube2.Visible := MultiLightShaderCheckBox.Checked;
end;

procedure TGLSLTestForm.UseSpecularTextureCheckBoxClick(Sender: TObject);
begin
  if UseSpecularTextureCheckBox.Checked then
  begin
    MyBumpShader.SpecularTexture := MaterialLibrary.LibMaterialByName('EarthGross').Material.Texture;
    MultiLightShader.SpecularTexture := MaterialLibrary.LibMaterialByName('EarthGross').Material.Texture;
  end
  else
  begin
    MyBumpShader.SpecularTexture := nil;
    MultiLightShader.SpecularTexture := nil;
  end;
end;

procedure TGLSLTestForm.UseNormalTextureCheckBoxClick(Sender: TObject);
begin
  if UseNormalTextureCheckBox.Checked then
  begin
    MyBumpShader.NormalTexture := MaterialLibrary.LibMaterialByName('EarthNormals').Material.Texture;
    MultiLightShader.NormalTexture := MaterialLibrary.LibMaterialByName('EarthNormals').Material.Texture;
  end
  else
  begin
    MyBumpShader.NormalTexture := nil;
    MultiLightShader.NormalTexture := nil;
  end;
end;

end.

