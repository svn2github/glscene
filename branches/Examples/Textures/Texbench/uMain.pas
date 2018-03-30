unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Graphics, Controls, Forms, Dialogs,
   
  GLCrossPlatform, GLBaseClasses,
  GLScene, GLCadencer, GLWin32Viewer, GLAsyncTimer, GLObjects, GLHUDObjects,
  GLCoordinates, GLTexture, GLCompositeImage, GLFileDDS, GLContext, GLUtils,
  GLRenderContextInfo;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    cad: TGLCadencer;
    at: TGLAsyncTimer;
    cam: TGLCamera;
    hud: TGLHUDSprite;
    dogl: TGLDirectOpenGL;
    procedure FormCreate(Sender: TObject);
    procedure atTimer(Sender: TObject);
    procedure cadProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
  end;

var
  Form1: TForm1;

  init: boolean;
  glsl: TGLProgramHandle;

  fps: single = -1;
  fcnt: integer = 25;
  fcur: integer = 0;

implementation

{$R *.dfm}


// setup
//
procedure TForm1.FormCreate(Sender: TObject);
begin

  clientWidth := 1024;
  clientHeight := 1024;

  hud.Material.Texture.Image.LoadFromFile( 'tex.dds' );

end;


// timer
//
procedure TForm1.atTimer(Sender: TObject);
var
    i: integer;
    s: string;
begin

  s := '|';
  for i := 0 to fcnt do
    if i < fcur then s := s + '|'
      else s := s + '.';
  caption := s + '|';
  inc(fcur);

  if fcur = fcnt+3 then begin
    caption := 'fps: ' + floattostr(fps);
    cad.Enabled := false;
    at.Enabled := false;
  end else
    if fps > 0 then fps := (fps + vp.FramesPerSecond) / 2
      else fps := vp.FramesPerSecond;

  vp.ResetPerformanceMonitor;

end;


// progress
//
procedure TForm1.cadProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin

  vp.Refresh;

end;


// render
//
procedure TForm1.doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
begin

  if not init then begin

    if not(gl.ARB_shader_objects and gl.ARB_vertex_program and
           gl.ARB_vertex_shader and gl.ARB_fragment_shader) then begin
      ShowMessage('shader not supported by your hardware');
      Halt;
    end;

    glsl := TGLProgramHandle.CreateAndAllocate;
    glsl.AddShader( TGLVertexShaderHandle, LoadAnsiStringFromFile('vp'));
    glsl.AddShader( TGLFragmentShaderHandle, LoadAnsiStringFromFile('fp'));
    if not glsl.LinkProgram then raise Exception.Create( glsl.InfoLog );
    if not glsl.ValidateProgram then raise Exception.Create( glsl.InfoLog );
    init := true;

  end;

  if init then
    with glsl do begin

      UseProgramObject;

      Uniform1i['tex'] := 0;
      Uniform1f['ct'] := cad.CurrentTime;

      hud.Render( rci );

      EndUseProgramObject;

    end;

end;

end.
