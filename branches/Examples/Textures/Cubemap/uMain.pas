unit uMain;

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

  GLWin32Viewer,
  GLCrossPlatform,
  GLBaseClasses,
  GLScene,
  GLMaterial,
  GLCadencer,
  GLObjects,
  GLCoordinates,
  GLTexture,
  GLKeyboard,
  uDDStex;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    matlib: TGLMaterialLibrary;
    cad: TGLCadencer;
    dc_cam: TGLDummyCube;
    cam: TGLCamera;
    GLSphere1: TGLSphere;
    procedure FormCreate(Sender: TObject);
    procedure cadProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure FormShow(Sender: TObject);
  end;

var
  Form1: TForm1;
  m_pos: TPoint;

implementation

{$R *.dfm}

//
// setup
//
procedure TForm1.FormCreate;
begin
  cad.FixedDeltaTime := 1 / GetDeviceCaps(getDC(Handle), 116);
  setCursorPos(screen.width div 2, screen.height div 2);
  with DDStex(matlib, 'cubemap', 'cubemap.dds').Material do
  begin
    Texture.MappingMode := tmmCubeMapNormal;
    Texture.TextureWrap := twNone;
    Texture.MagFilter := maNearest;
  end;
end;

//
// cadProgress
//
procedure TForm1.cadProgress;
begin
  with mouse.CursorPos do
  begin
    dc_cam.TurnAngle := dc_cam.TurnAngle + (width div 2 - X) * 0.02;
    cam.PitchAngle := cam.PitchAngle + (height div 2 - Y) * 0.02;
  end;
  setCursorPos(screen.width div 2, screen.height div 2);
  if IsKeyDown(27) then
    close;
end;

//
// activate
//
procedure TForm1.FormShow;
begin
  cad.Enabled := true;
end;

end.
