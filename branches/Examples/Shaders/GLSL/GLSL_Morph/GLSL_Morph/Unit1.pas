unit Unit1;

interface

uses
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
  Vcl.StdCtrls,
  Vcl.Imaging.Jpeg,
   
  GLTexture,
  GLCadencer,
  GLWin32Viewer,
  GLScene,
  GLObjects,
  GLFileTGA,
  GLVectorLists,
  GLVectorTypes,
  GLUserShader,
  GLUtils,
  GLContext,
  GLVectorFileObjects,
  GLFile3DS,
  GLVectorGeometry,
  GLShadowVolume,
  XOpenGL,
  GLFileMD2,
  GLFileMS3D,
  GLGeomObjects,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLRenderContextInfo;

type
  TGLSLFrm = class(TForm)
    GLScene1: TGLScene;
    GLViewer: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    Cam: TGLCamera;
    Scene: TGLDummyCube;
    Timer1: TTimer;
    CamBox: TGLDummyCube;
    MatLib: TGLMaterialLibrary;
    Light: TGLLightSource;
    SceneMesh: TGLFreeForm;
    RenderDirectGL: TGLDirectOpenGL;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure RenderDirectGLRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
  private
     
    ADir: string;
    InitDGL: Boolean;
    AltDGL: Boolean;
    mx, my: Integer;
    GLSLProg: TGLProgramHandle;
  public
     
  end;

var
  GLSLFrm: TGLSLFrm;
  InitShader: Integer;

implementation

{$R *.dfm}

procedure TGLSLFrm.FormCreate(Sender: TObject);
begin
  InitShader := 0;
  InitDGL := False;
  AltDGL := False;
  SetCurrentDir(ExtractFilePath(Application.ExeName));
  ADir := ExtractFilePath(Application.ExeName);
  SceneMesh.LoadFromFile(ADir + 'Model\oxMorph.3DS');
  GLCadencer1.Enabled := True;
  Timer1.Enabled := True;
end;

procedure TGLSLFrm.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  SceneMesh.Turn(-0.1);
  SceneMesh.Pitch(0.1);
  GLViewer.Invalidate;
end;

procedure TGLSLFrm.GLViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TGLSLFrm.GLViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (ssright in Shift) then
    Cam.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TGLSLFrm.Timer1Timer(Sender: TObject);
begin
  Caption := Format('Morph GLSL Engine Demo. [%.2f] FPS',
    [GLViewer.FramesPerSecond]);
  GLViewer.ResetPerformanceMonitor;
end;

procedure TGLSLFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AltDGL := True;
  InitDGL := False;
  Timer1.Enabled := False;
  GLCadencer1.Enabled := False;
  GLSLProg.Free;
end;

procedure TGLSLFrm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Cam.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TGLSLFrm.RenderDirectGLRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  if (not InitDGL) then
  begin
    if not(GL_SHADER_OBJECT_ARB and GL_VERTEX_PROGRAM_ARB and
      GL_VERTEX_SHADER_ARB and GL_FRAGMENT_SHADER_ARB) = 0 then
    begin
      ShowMessage
        ('Your hardware/driver doesn''t support GLSL and can''t execute this demo!');
      Halt;
    end;
    GLSLProg := TGLProgramHandle.CreateAndAllocate;
    GLSLProg.AddShader(TGLVertexShaderHandle,
      String(LoadAnsiStringFromFile('Morph.Vert')),False);
    GLSLProg.AddShader(TGLFragmentShaderHandle,
      String(LoadAnsiStringFromFile('Morph.Frag')),False);
    if (not GLSLProg.LinkProgram) then
      raise Exception.Create(GLSLProg.InfoLog);
    if (not GLSLProg.ValidateProgram) then
      raise Exception.Create(GLSLProg.InfoLog);
    // CheckOpenGLError;
    InitDGL := True;
  end;
  if (InitDGL) and (not AltDGL) then
  begin
    with GLSLProg do
    begin
      UseProgramObject;
      Uniform4f['lightDir'] := Light.SpotDirection.AsVector;
      Uniform1f['speed'] := 0.11;
      Uniform1f['lerpMin'] := -2.0;
      Uniform1f['lerpMax'] := 2.0;
      Uniform1f['time_0_X'] := GLCadencer1.GetCurrentTime;
      SceneMesh.Render(rci);
      EndUseProgramObject;
    end;
  end;
end;

end.
