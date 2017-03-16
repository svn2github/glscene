unit Unit1;

interface

uses
  Winapi.OpenGL,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Imaging.Jpeg,
   
  OpenGLTokens,
  GLMaterial, GLCadencer, GLScene, GLVectorFileObjects,
  GLObjects, GLCoordinates, GLCrossPlatform, GLBaseClasses, GLWin32Viewer,
  GLTexture, GLFileTGA,
  GLVectorLists, GLVectorTypes, GLUserShader, GLUtils, GLContext,
  GLFile3DS, GLVectorGeometry, GLShadowVolume, XOpenGL,
  GLFileMD2, GLFileMS3D, GLGeomObjects,
  GLRenderContextInfo,
  GLShaderCombiner;

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
    mx, my: Integer;
    InitDGL: Boolean;
    AltDGL: Boolean;
    Color1, Color2: TVector4f;
    GLSLProg: TGLProgramHandle;
    procedure PrepareTextures;
    procedure DummyRender(dummy: TGLDummyCube; rci: TGLRenderContextInfo);
  public
     
  end;

var
  GLSLFrm: TGLSLFrm;

implementation

{$R *.dfm}

procedure TGLSLFrm.FormCreate(Sender: TObject);
begin
  InitDGL := False;
  AltDGL := False;
  GLViewer.Buffer.FaceCulling := False;
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  ADir := ExtractFilePath(Application.ExeName);
  SceneMesh.LoadFromFile(ADir + 'Model\teapot.3ds');
  GLCadencer1.Enabled := True;
  Timer1.Enabled := True;
end;

procedure TGLSLFrm.PrepareTextures;
begin
  MatLib.Materials[0].Material.Texture.Image.LoadFromFile
    (ADir + 'image\Erosion.tga');
  Color1.X := 0.2;
  Color1.Y := 0.090411;
  Color1.Z := 0.0;
  Color1.W := 1.0;
  Color2.X := 0.8;
  Color2.Y := 0.8;
  Color2.Z := 0.8;
  Color2.W := 1.0;
end;

procedure TGLSLFrm.DummyRender(dummy: TGLDummyCube; rci: TGLRenderContextInfo);
var
  i: Integer;
begin
  if (dummy.Count > 0) then
  begin
    for i := 0 to dummy.Count - 1 do
    begin
      if TGLSceneObject(dummy.Children[i]).Tag <> 1 then
      begin
        if dummy.Children[i].Visible then
          dummy.Children[i].Visible := False;
        glPushMatrix;
        GL.MultMatrixf(PGLFloat(TGLSceneObject(dummy.Children[i])
          .AbsoluteMatrixAsAddress));
        if dummy.Children[i].Count > 0 then
          dummy.Children[i].DoRender(rci, True, True)
        else
          dummy.Children[i].DoRender(rci, True, False);
        GL.PopMatrix;
      end;
    end;
  end;
end;

procedure TGLSLFrm.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
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
  Caption := Format('Erosion GLSL Demo. [%.2f] FPS',
    [GLViewer.FramesPerSecond]);
  GLViewer.ResetPerformanceMonitor;
end;

procedure TGLSLFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AltDGL := True;
  InitDGL := False;
  GLCadencer1.Enabled := False;
  Timer1.Enabled := False;
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
   {
    if (GL_SHADER_OBJECT_ARB and GL_VERTEX_PROGRAM_ARB and
      GL_VERTEX_SHADER_ARB and GL_FRAGMENT_SHADER_ARB) = 0 then
    begin
      ShowMessage
        ('Your hardware/driver doesn''t support GLSL and can''t execute this demo!');
      Halt;
    end;
    }
    GLSLProg := TGLProgramHandle.CreateAndAllocate;
    GLSLProg.AddShader(TGLVertexShaderHandle,
      LoadAnsiStringFromFile(ADir + 'Erosion.Vert'));
    GLSLProg.AddShader(TGLFragmentShaderHandle,
      LoadAnsiStringFromFile(ADir + 'Erosion.Frag'));
    if (not GLSLProg.LinkProgram) then
      raise Exception.Create(GLSLProg.InfoLog);
    if (not GLSLProg.ValidateProgram) then
      raise Exception.Create(GLSLProg.InfoLog);
///    CheckOpenGLError;
    PrepareTextures;
    InitDGL := True;
  end;
  if (InitDGL) and (not AltDGL) then
  begin
    with GLSLProg do
    begin
      UseProgramObject;
      GL.ActiveTexture(GL_TEXTURE0_ARB);
      GL.BindTexture(GL_TEXTURE_2D, MatLib.Materials[0].Material.Texture.Handle);
      Uniform1i['Noise2d'] := 0;
      Uniform3f['LightPosition'] := Light.Position.AsAffineVector;
      Uniform1f['Scale'] := 0.02;
      Uniform1f['ErosionFactor'] := 0.35;
      Uniform1f['IntensityFactor1'] := 0.75;
      Uniform1f['IntensityFactor2'] := 1.95;
      Uniform4f['Color1'] := Color1;
      Uniform4f['Color2'] := Color2;
      DummyRender(Scene, rci);
      EndUseProgramObject;
    end;
  end;
end;

end.
