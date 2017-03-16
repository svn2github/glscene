unit main;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
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
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.Imaging.Jpeg,
   
  GLScene,
  GLObjects, GLCadencer, GLUtils, GLTexture, GLUserShader,
  GLContext, GLGraph, GLVectorGeometry, GLVectorTypes, GLVectorLists,
  GLFileTGA,
  GLWin32Viewer, GLAsyncTimer, GLMaterial, GLCoordinates,
  GLCrossPlatform, GLBaseClasses, GLRenderContextInfo, GLColor;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLCube1: TGLCube;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    LatticeShader1: TGLUserShader;
    MatLib1: TGLMaterialLibrary;
    Init1: TGLDirectOpenGL;
    Render1: TGLDirectOpenGL;
    GLPlane1: TGLPlane;
    GlassShader1: TGLUserShader;
    GLSphere1: TGLSphere;
    ColorDialog1: TColorDialog;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    trScaleX: TTrackBar;
    trScaleY: TTrackBar;
    trTresholdX: TTrackBar;
    trTresholdY: TTrackBar;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    DepthTrackBar2: TTrackBar;
    MixTrackBar1: TTrackBar;
    MixEdit1: TEdit;
    DepthEdit1: TEdit;
    btnColorTint1: TButton;
    AsyncTimer1: TGLAsyncTimer;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure LatticeShader1DoApply(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure LatticeShader1DoUnApply(Sender: TObject; Pass: Integer;
      var rci: TGLRenderContextInfo; var Continue: Boolean);
    procedure Init1Render(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GlassShader1DoApply(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure GlassShader1DoUnApply(Sender: TObject; Pass: Integer;
      var rci: TGLRenderContextInfo; var Continue: Boolean);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure DepthTrackBar2Change(Sender: TObject);
    procedure btnColorTint1Click(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
  private
    mx, my: Integer;
    shH, shW: Single;
  public
    programObject, glassProgram: TGLProgramHandle;
    function make3fvector(v1, v2, v3: Single): TVector3f;
    procedure Uniform2f(index: String; val1, val2: Single);
    procedure PrepareMaterials;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  fBuffSize: Integer = 512;

var
  TintColor: TVector4f;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift = [ssLeft] then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin

  GLSceneViewer1.Invalidate;
end;

procedure TForm1.LatticeShader1DoApply(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  camPos: TVector;
begin
  if Init1.Tag = 0 then
    Exit;
  programObject.UseProgramObject;
  camPos := GLCamera1.AbsolutePosition;
  programObject.Uniform3f['EyePosition'] := GLCamera1.Position.AsAffineVector;
  Uniform2f('Scale', trScaleX.Position / 10.0, trScaleY.Position / 10.0);
  Uniform2f('Threshold', trTresholdX.Position / 100.0,
    trTresholdY.Position / 100.0);
end;

procedure TForm1.LatticeShader1DoUnApply(Sender: TObject; Pass: Integer;
  var rci: TGLRenderContextInfo; var Continue: Boolean);
begin
  programObject.EndUseProgramObject;
end;

procedure TForm1.Init1Render(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  if not(GL_SHADER_OBJECT_ARB and GL_VERTEX_PROGRAM_ARB and
    GL_VERTEX_SHADER_ARB and GL_FRAGMENT_SHADER_ARB)=0 then
  begin
    ShowMessage
      ('Your hardware/driver doesn''t support GLSL and can''t execute this demo!');
    Halt;
  end;

  if Init1.Tag <> 0 then
    Exit;
  Init1.Tag := 1;
  programObject := TGLProgramHandle.CreateAndAllocate;
  programObject.AddShader(TGLVertexShaderHandle,
    LoadAnsiStringFromFile('Shaders\Lattice.vert'), True);
  programObject.AddShader(TGLFragmentShaderHandle,
    LoadAnsiStringFromFile('Shaders\Lattice.frag'), True);

  if not programObject.LinkProgram then
    raise Exception.Create(programObject.InfoLog);
  if not programObject.ValidateProgram then
    raise Exception.Create(programObject.InfoLog);

  programObject.UseProgramObject;

  programObject.Uniform3f['LightPosition'] :=
    GLLightSource1.Position.AsAffineVector;
  programObject.Uniform3f['LightColor'] := make3fvector(0.9, 0.8, 0.7);
  programObject.Uniform3f['EyePosition'] := GLCamera1.Position.AsAffineVector;
  programObject.Uniform3f['Specular'] := make3fvector(0.2, 0.2, 0.2);
  programObject.Uniform3f['Ambient'] := make3fvector(0.2, 0.2, 0.2);
  programObject.Uniform1f['Kd'] := 0.8;
  programObject.Uniform3f['SurfaceColor'] := make3fvector(0.9, 0.7, 0.25);
  Uniform2f('Scale', 10.0, 10.0);
  Uniform2f('Threshold', 0.13, 0.13);

  programObject.EndUseProgramObject;
  ///CheckOpenGLError;

  with MatLib1.Materials[0] do
  begin
    PrepareBuildList;
    GL.ActiveTexture(GL_TEXTURE0_ARB);
    GL.BindTexture(GL_TEXTURE_2D, Material.Texture.Handle);
    GL.ActiveTexture(GL_TEXTURE0_ARB);
  end; // with

  with MatLib1.Materials[1] do
  begin
    PrepareBuildList;
    GL.ActiveTexture(GL_TEXTURE1_ARB);
    GL.BindTexture(GL_TEXTURE_CUBE_MAP_ARB, Material.Texture.Handle);
    GL.ActiveTexture(GL_TEXTURE0_ARB);
  end;

  // glassProgram Init
  glassProgram := TGLProgramHandle.CreateAndAllocate;
  glassProgram.AddShader(TGLVertexShaderHandle,
    String(LoadAnsiStringFromFile('Shaders\Glass.vert')), True);
  glassProgram.AddShader(TGLFragmentShaderHandle,
    String(LoadAnsiStringFromFile('Shaders\Glass.frag')), True);

  if not glassProgram.LinkProgram then
    raise Exception.Create(glassProgram.InfoLog);
  if not glassProgram.ValidateProgram then
    raise Exception.Create(glassProgram.InfoLog);

  glassProgram.UseProgramObject;

  glassProgram.Uniform3f['LightPos'] := GLLightSource1.Position.AsAffineVector;
  glassProgram.Uniform3f['BaseColor'] := make3fvector(TintColor.X, TintColor.Y,
    TintColor.Z);
  glassProgram.Uniform1f['Depth'] := 0.1; // 0 - 0.3
  glassProgram.Uniform1f['MixRatio'] := 1.0; // 0 - 2
  glassProgram.Uniform1f['FrameWidth'] := fBuffSize * 3.125;
  glassProgram.Uniform1f['FrameHeight'] := fBuffSize * 3.125;
  glassProgram.Uniform1i['EnvMap'] := 0;
  glassProgram.Uniform1i['RefractionMap'] := 1;

  glassProgram.EndUseProgramObject;

  //CheckOpenGLError;
end;

function TForm1.make3fvector(v1, v2, v3: Single): TVector3f;
begin
  Result.X := v1;
  Result.Y := v2;
  Result.Z := v3;
end;

procedure TForm1.Uniform2f(index: String; val1, val2: Single);
begin
  with programObject do
    GL.Uniform2f(GetUniformLocation(index), val1, val2);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled := false;
  AsyncTimer1.Enabled := false;
  programObject.Free;
  glassProgram.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MixEdit1.Text := Format('%.3f', [MixTrackBar1.Position / 100.0]);
  DepthEdit1.Text := Format('%.3f', [DepthTrackBar2.Position / 100.0]);

  TintColor := ConvertWinColor(ColorDialog1.Color);

  MatLib1.Materials.GetLibMaterialByName('envMap')
    .Material.Texture.Image.LoadFromFile('Images\sealine.jpg');
  MatLib1.Materials.GetLibMaterialByName('planeMap')
    .Material.Texture.Image.LoadFromFile('Images\barts.jpg');

  // capture and create material from framebuffer
  GLSphere1.Visible := false;
  GLSceneViewer1.Buffer.CopyToTexture(MatLib1.Materials[1].Material.Texture);
  GLSphere1.Visible := True;
end;

procedure TForm1.GlassShader1DoApply(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  if Init1.Tag = 0 then
    Exit;
  GLSphere1.Visible := false;
  GLSceneViewer1.Buffer.CopyToTexture(MatLib1.Materials[1].Material.Texture);
  GLSphere1.Visible := True;
  glassProgram.UseProgramObject;
  PrepareMaterials;
  glassProgram.Uniform1f['Depth'] := DepthTrackBar2.Position / 100.0;
  glassProgram.Uniform1f['MixRatio'] := MixTrackBar1.Position / 100.0;
  glassProgram.Uniform1f['FrameWidth'] := fBuffSize * 3.125;
  glassProgram.Uniform1f['FrameHeight'] := fBuffSize * 3.125;
  glassProgram.Uniform3f['BaseColor'] := make3fvector(TintColor.X, TintColor.Y,
    TintColor.Z);
end;

procedure TForm1.GlassShader1DoUnApply(Sender: TObject; Pass: Integer;
  var rci: TGLRenderContextInfo; var Continue: Boolean);
begin
  glassProgram.EndUseProgramObject;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.PrepareMaterials;
begin
  with MatLib1.Materials[0] do
  begin
    PrepareBuildList;
    GL.ActiveTexture(GL_TEXTURE0_ARB);
    GL.BindTexture(GL_TEXTURE_2D, Material.Texture.Handle);
    GL.ActiveTexture(GL_TEXTURE0_ARB);
  end; // with

  with MatLib1.Materials[1] do
  begin
    PrepareBuildList;
    GL.ActiveTexture(GL_TEXTURE1_ARB);
    GL.BindTexture(GL_TEXTURE_2D, Material.Texture.Handle);
    GL.ActiveTexture(GL_TEXTURE0_ARB);
  end;
end;

procedure TForm1.DepthTrackBar2Change(Sender: TObject);
begin
  MixEdit1.Text := Format('%.3f', [MixTrackBar1.Position / 100.0]);
  DepthEdit1.Text := Format('%.3f', [DepthTrackBar2.Position / 100.0]);
  GLSceneViewer1.SetFocus;
end;

procedure TForm1.btnColorTint1Click(Sender: TObject);

begin
  if ColorDialog1.Execute then
  begin
    TintColor := ConvertWinColor(ColorDialog1.Color);

  end;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Caption := Format('GLSL Lattice/Glass Demo [%.2f] FPS',
    [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
