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

  GLScene,
  GLObjects,
  GLShadowPlane,
  GLWin32Viewer,
  GLCadencer,
  GLSkydome,
  GLAsyncTimer,
  GLVectorGeometry,
  GLGeometryBB,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLShadowPlane1: TGLShadowPlane;
    GLCube1: TGLCube;
    gdcLights: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    gdcShadowing: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLShadowPlane2: TGLShadowPlane;
    GLLightSource2: TGLLightSource;
    GLSkyDome1: TGLSkyDome;
    GLLightSource3: TGLLightSource;
    GLCube2: TGLCube;
    GLDummyCube1: TGLDummyCube;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
  private
    globalSpeed: single;
    sinusVar: single;
  public
    procedure prcLightWalk(var Light: TGLLightSource; const deltaTime: Double);
  end;

const
  spacing = 50;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);

begin
  sinusVar := sinusVar + deltaTime;
  globalSpeed := cos(sinusVar / 10) * 1.0 + 1.5;

  GLCube1.Pitch(deltaTime * 100 * globalSpeed);
  GLCube2.Roll(deltaTime * 50);
  GLCube2.Turn(deltaTime * 75);
  GLDummyCube1.Position.Y := abs(sin((GLCube1.PitchAngle) / 360 * pi * 4) *
    (GLCube1.CubeWidth));

  prcLightWalk(GLLightSource1, deltaTime * globalSpeed);
  prcLightWalk(GLLightSource2, deltaTime * globalSpeed);
  prcLightWalk(GLLightSource3, deltaTime * globalSpeed);

  Form1.Caption := FloatToStrF(GLSceneViewer1.Buffer.FramesPerSecond, ffgeneral,
    5, 1) + ' FPS';

end;

procedure TForm1.prcLightWalk(var Light: TGLLightSource;
  const deltaTime: Double);
begin
  with Light.Position do
  begin
    Z := Z + deltaTime * 15;
    if Z > spacing then
      Z := -spacing;
  end;
  with Light.Diffuse do
  begin
    Red := (spacing - abs(Light.Position.Z)) / spacing;
    Green := (spacing - abs(Light.Position.Z)) / spacing;
  end;
end;

end.
