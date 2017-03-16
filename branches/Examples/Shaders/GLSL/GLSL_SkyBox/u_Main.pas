unit u_Main;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Dialogs, Vcl.Forms, Vcl.Controls,
   
  GLCadencer, GLWin32Viewer, GLScene, GLObjects, GLGeomObjects, GLTexture,
  GLTextureFormat, GLCompositeImage, GLMaterial, GLCoordinates, GLBaseClasses,
  GLCrossPlatform, GLContext, GLRenderContextInfo, GLUtils,
  OpenGL1x,

  GLFileDDS;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    cad: TGLCadencer;
    cam: TGLCamera;
    dc_cam: TGLDummyCube;
    tor: TGLTorus;
    sph: TGLSphere;
    sky: TGLSphere;
    dogl: TGLDirectOpenGL;
    procedure cadProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
  public
    procedure loadCubemap;
    procedure initGLSL;
  end;

var
  Form1: TForm1;
  glsl_sky: TGLProgramHandle;
  glsl_obj: TGLProgramHandle;
  cubemap: TGLTexture;

implementation

{$R *.dfm}

//
// setup
//
procedure TForm1.FormCreate;
begin
  loadCubemap;
  sky.Radius := cam.DepthOfView;
  sky.Material.Texture := cubemap;
  sph.Material.Texture := cubemap;
  tor.Material.Texture := cubemap;

end;

//
// loadCubemap
//
procedure TForm1.loadCubemap;
begin
  cubemap := TGLTexture.Create(self);
  with cubemap do
  begin
    ImageClassName := 'TGLCompositeImage';
    Image.LoadFromFile('data/sky.dds');
    TextureWrap := twNone;
    filteringQuality := tfAnisotropic;
    disabled := false;
  end;
end;

//
// cadProgress
//
procedure TForm1.cadProgress;
begin
  dc_cam.Turn(deltaTime * 30);
  tor.Pitch(deltaTime * 50);
  cam.Position.Y := 3 * cos(newTime / 3);
  sky.AbsolutePosition := cam.AbsolutePosition;
end;

//
// doglRender
//
procedure TForm1.doglRender;
begin
  if not cad.Enabled then
    initGLSL;
  // sky cubemap
  glsl_sky.UseProgramObject;
  sky.Render(rci);
  glsl_sky.EndUseProgramObject;

  // object cubemap
  glsl_obj.UseProgramObject;
  glsl_obj.UniformMatrix4fv['m4'] := sph.AbsoluteMatrix;
  sph.Render(rci);
  glsl_obj.UniformMatrix4fv['m4'] := tor.AbsoluteMatrix;
  tor.Render(rci);
  glsl_obj.EndUseProgramObject;
end;

//
// initGLSL
//
procedure TForm1.initGLSL;

function load(vp, fp: String): TGLProgramHandle;
begin
  result := TGLProgramHandle.CreateAndAllocate;
  result.AddShader(TGLVertexShaderHandle, LoadAnsiStringFromFile(vp));
  result.AddShader(TGLFragmentShaderHandle, LoadAnsiStringFromFile(fp));
  if not result.LinkProgram then
    raise Exception.Create(result.InfoLog);
  if not result.ValidateProgram then
    raise Exception.Create(result.InfoLog);
  CheckOpenGLError;
end;

begin
  if not(gl.ARB_shader_objects and gl.ARB_vertex_program and
    gl.ARB_vertex_shader and gl.ARB_fragment_shader and gl.ARB_texture_cube_map)
  then
  begin
    ShowMessage('shader not supported by your hardware');
    Halt;
  end;

  cad.Enabled := true;

  glsl_sky := load('data/sky.vp', 'data/sky.fp');
  glsl_obj := load('data/obj.vp', 'data/obj.fp');

end;

end.
