unit GLAntiZFightShaderDemo;

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
  Vcl.Imaging.Jpeg,

  GLScene,
  GLSkydome,
  GLWin32Viewer,
  GLObjects,
  GLTexture,
  GLGeomObjects,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,

  GLAntiZFightShader;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLSkyDome1: TGLSkyDome;
    DummyTarget: TGLDummyCube;
    YellowSphere: TGLSphere;
    BlueSphere: TGLSphere;
    GLLightSource1: TGLLightSource;
    CheckBox1: TCheckBox;
    GLCylinder1: TGLCylinder;
    GLDummyCube1: TGLDummyCube;
    GLDummyCube2: TGLDummyCube;
    GLDummyCube3: TGLDummyCube;
    GLCube1: TGLCube;
    CheckBox2: TCheckBox;
    GLMaterialLibrary1: TGLMaterialLibrary;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  public
    mx, my: Integer;
    AntiZFightShader: TGLAntiZFightShader;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  TempLibMaterial: TGLLibMaterial;
  i: Integer;
begin
  Form1.Top := round((screen.Height / 2) - (Form1.Height / 2));
  Form1.left := round((screen.width / 2) - (Form1.width / 2));

  GLSkyDome1.stars.addrandomstars(5000, RGB(255, 255, 255), false);

  { Run-time creation of the shader object }
  AntiZFightShader := TGLAntiZFightShader.Create(Self);
  { For now, this is simply a value that the user provides to compensate
    for the overall dimensions of a scene.  I have no easy answer for what
    numbers are good yet: use trial and error.  Basically, if your scene is
    very large, use a large number, and vice versa }
  AntiZFightShader.Modifier := 200000000;

  { Assign the shader to the appropriate materials }
  for i := 0 to 3 do
  begin
    GLMaterialLibrary1.Materials[i].Shader := AntiZFightShader;
  end;

  { Turn off GLScene's object depth sorting, as it doesn't take into
    account manipulations of the projection matrix }
  GLDummyCube2.ObjectStyle := GLDummyCube2.ObjectStyle + [osIgnoreDepthBuffer];
  GLDummyCube3.ObjectStyle := GLDummyCube3.ObjectStyle + [osIgnoreDepthBuffer];
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
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / -300));
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  AntiZFightShader.Enabled := CheckBox1.Checked;

  GLSceneViewer1.Refresh;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
  begin
    GLDummyCube2.ObjectStyle := GLDummyCube2.ObjectStyle +
      [osIgnoreDepthBuffer];
    GLDummyCube3.ObjectStyle := GLDummyCube3.ObjectStyle +
      [osIgnoreDepthBuffer];
  end
  else
  begin
    GLDummyCube2.ObjectStyle := GLDummyCube2.ObjectStyle -
      [osIgnoreDepthBuffer];
    GLDummyCube3.ObjectStyle := GLDummyCube3.ObjectStyle -
      [osIgnoreDepthBuffer];
  end;
  GLSceneViewer1.Refresh;
end;

end.
