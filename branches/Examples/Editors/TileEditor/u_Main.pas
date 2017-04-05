{$APPTYPE CONSOLE}

unit u_Main;

interface

uses
  Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.jpeg,
   
  GLCadencer,
  GLWin32Viewer,
  GLCrossPlatform,
  GLBaseClasses,
  GLScene,
  GLMaterial,
  GLObjects,
  GLCoordinates,
  GLMesh,
  GLAsyncTimer,
  GLKeyboard,
  GLFBORenderer,
  GLRenderContextInfo,
  GLVectorGeometry,
  GLGeomObjects,
  GLFileJpeg,
  uLog,
  u_Map;


type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    cad: TGLCadencer;
    cam: TGLCamera;
    dc_cam: TGLDummyCube;
    mesh: TGLMesh;
    GLCube1: TGLCube;
    at: TGLAsyncTimer;
    dc_world: TGLDummyCube;
    dogl: TGLDirectOpenGL;
    fbo: TGLFBORenderer;
    matlib: TGLMaterialLibrary;
    dc_map: TGLDummyCube;
    back: TGLPlane;
    cam_fbo: TGLCamera;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure atTimer(Sender: TObject);
    procedure cadProgress(Sender: TObject; const deltaTime,newTime: Double);
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure fboAfterRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure FormShow(Sender: TObject);
  public
    map: c_Map;
  end;

var
  Form1: TForm1;
  map_point: TPoint;
  map_modified: boolean;

implementation

{$R *.dfm}

//
// setup
//
procedure TForm1.FormCreate;
begin
  isConsole := True;
  SetCurrentDir('data');
  log_tick;
  log('load map:', 10);
  map_point := point(0,0);
  map := c_Map.Create(self, 'map.bmp');
  resize;
  log('uploading...', 14);
  vp.Buffer.Render;
  log( 'form create:', 10 );
  log( format( '%.3f sec', [ log_delta ]));
end;


//
// cadProgress
//
procedure TForm1.cadProgress;
begin
  // animation
  with dc_cam.Position do begin
    if abs(x - map_point.X) > 9 then begin
      map_point.X := round(x);
      map_modified := true;
      end;
    if abs(Floor(Y) - map_point.Y) > 4 then begin
      map_point.Y := floor(y);
      map_modified := true;
      end;
    end;
  dc_cam.Translate(deltatime * 4, deltatime * 2, 0);
  vp.Invalidate;
  if iskeydown(vk_escape) then close;
end;


//
// doglRender
//
procedure TForm1.doglRender;
begin
  // redraw background texture
  if map_modified then
  begin
    map.redraw(mesh, map_point);
    back.Position.SetPoint(map_point.X, map_point.Y, 0);
    mesh.AbsolutePosition := dc_cam.AbsolutePosition;
    fbo.Active := true;
  end;
  map_modified := false;
end;

//
// fboAfterRender
//
procedure TForm1.fboAfterRender;
begin
  fbo.Active := false;
end;


//
// resize
//
procedure TForm1.FormResize;
var
    c1,c2: cardinal;
begin
  // resize viewport
  if (clientWidth - 40) / (clientHeight - 40) > 2 then
  begin
    c1 := clientHeight - 40;
    c2 := clientWidth div 2 - c1;
    vp.BoundsRect := rect(c2, 20, c1 * 2 + c2, c1 + 20);

  end
  else
  begin
    c1 := (clientWidth - 40) div 2;
    c2 := clientHeight div 2 - c1 div 2;
    vp.BoundsRect := rect(20, c2, c1 * 2 + 20, c2 + c1);
  end;

  // resize fbo
  c1 := 1 shl ceil(log2(vp.Width));
  if fbo.Width <> c1 then
  begin
    fbo.Width := c1;
    fbo.Height := c1 div 2;
    map_modified := true;
    log(format('new FBO size: %d x %d', [fbo.Width, fbo.Height]), 14);
  end;
end;


//
// timer
//
procedure TForm1.atTimer;
begin
  caption := 'Tiles: ' + vp.FramesPerSecondText(2);
  vp.ResetPerformanceMonitor;
end;


//
// show
//
procedure TForm1.FormShow;
begin
  cad.Enabled := true;
end;

end.
