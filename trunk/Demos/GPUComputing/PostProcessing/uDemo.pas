unit uDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  // GLScene
  GLObjects, GLGeomObjects, GLScene, GLTeapot, GLCoordinates,
  GLMaterial, GLSimpleNavigation, GLCadencer, GLWin32Viewer, GLCrossPlatform,
  BaseClasses, GLFBORenderer, GLHUDObjects, GLSCUDA, GLSCUDAGraphics,
  GLSCUDACompiler, GLSCUDAContext, GL3xFactory, GLState,
  GLRenderContextInfo, GLContext, GLCustomShader, GLSLShader, GL3xObjects,
  GLTexture, ComCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLTeapot1: TGLTeapot;
    GLLightSource1: TGLLightSource;
    RenderRoot: TGLDummyCube;
    GLCylinder1: TGLCylinder;
    GLFBORenderer1: TGLFBORenderer;
    GLSCUDADevice1: TGLSCUDADevice;
    GLSCUDA1: TGLSCUDA;
    GLSCUDACompiler1: TGLSCUDACompiler;
    MainModule: TCUDAModule;
    processedTextureMapper: TCUDAGLImageResource;
    cudaProcess: TCUDAFunction;
    CallPostProcess: TGLDirectOpenGL;
    GLCapsule1: TGLCapsule;
    ResultShader: TGLSLShader;
    GL3xSprite1: TGL3xSprite;
    processedTextureArray: TCUDAMemData;
    outputBuffer: TCUDAMemData;
    inputBuffer: TCUDAMemData;
    CommonShader: TGLSLShader;
    InfoCapture: TGLDirectOpenGL;
    GLSphere1: TGLSphere;
    TrackBar1: TTrackBar;
    procedure FormResize(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure cudaProcessParameterSetup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CallPostProcessRender(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure GLSCUDA1OpenGLContextNeeded(out Context: TGLContext);
    procedure ResultShaderApply(Shader: TGLCustomGLSLShader);
    procedure GLFBORenderer1BeforeRender(Sender: TObject);
    procedure InfoCaptureRender(Sender: TObject; var rci: TRenderContextInfo);
    procedure GLFBORenderer1AfterRender(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
    pRci: PRenderContextInfo;
  public
    { Public declarations }
    Radius: Integer;
    Threshold: Single;
    Highlight: SIngle;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  OpenGL1x;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Radius := 8;
  Threshold := 0.8;
  Highlight := 0.4;
  with GLMaterialLibrary1.TextureByName('processedTexture') do
  begin
    TGLBlankImage(Image).ColorFormat := GL_RGB_INTEGER;
    Disabled := false;
  end;
end;

procedure TForm1.CallPostProcessRender(Sender: TObject;
  var rci: TRenderContextInfo);
begin
  processedTextureMapper.MapResources;
  processedTextureMapper.BindArrayToImage(processedTextureArray, 0, 0);
  processedTextureArray.CopyTo(inputBuffer);
  cudaProcess.Launch;
  outputBuffer.CopyTo(processedTextureArray);
  processedTextureMapper.UnMapResources;
end;

procedure TForm1.cudaProcessParameterSetup(Sender: TObject);
begin
  with cudaProcess do
  begin
    SharedMemorySize :=
      (BlockShape.SizeX+(2*Radius))*(BlockShape.SizeY+(2*Radius))*sizeof(Integer);
    SetParam(inputBuffer);
    SetParam(outputBuffer);
    with GLMaterialLibrary1.TextureByName('processedTexture') do
    begin
      SetParam(TexWidth);
      SetParam(TexHeight);
    end;
    SetParam(BlockShape.SizeX + 2*Radius);
    SetParam(Radius);
    SetParam(Threshold);
    SetParam(Highlight);
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  GLCamera1.SceneScale := GLSceneViewer1.Width / GLSceneViewer1.Height;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.InfoCaptureRender(Sender: TObject;
  var rci: TRenderContextInfo);
begin
  pRci := @rci;
end;

procedure TForm1.GLFBORenderer1BeforeRender(Sender: TObject);
begin
  CommonShader.Apply(pRci^, Self);
end;

procedure TForm1.GLFBORenderer1AfterRender(Sender: TObject);
begin
  CommonShader.UnApply(pRci^);
end;

procedure TForm1.GLSCUDA1OpenGLContextNeeded(out Context: TGLContext);
begin
  Context := GLSceneViewer1.Buffer.RenderingContext;
end;

procedure TForm1.ResultShaderApply(Shader: TGLCustomGLSLShader);
begin
  with CurrentGLContext.GLStates do
  begin
    Disable(stDepthTest);
    DepthWriteMask := False;
  end;
  Shader.Param['TexUnit0'].AsTexture[0] :=
    GLMaterialLibrary1.TextureByName('processedTexture');
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Radius := TrackBar1.Position;
end;

end.
