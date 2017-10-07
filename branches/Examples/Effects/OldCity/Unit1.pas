unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  GLScene,
  GLObjects,
  GLGeomObjects,
  GLCadencer,
  GLWin32Viewer,
  GLTexture,
  GLFileTGA,
  GLBehaviours,
  GLContext,
  GLAsyncTimer,
  GLMovement,
  GLRenderContextInfo,
  GLVectorFileObjects,
  GLFileObj,mikmod,
  GLBitmapFont,
  GLWindowsFont,
  GLHUDObjects;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDirectOpenGL1: TGLDirectOpenGL;
    AsyncTimer1: TGLAsyncTimer;
    GLFreeForm1: TGLFreeForm;
    GLDummyCube1: TGLDummyCube;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AsyncTimer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TRGBACol=record
    r,g,b,a:byte;
  end;
  TBuf=array of dword;
  PBuf=^TBuf;

var
  Form1: TForm1;
  buf:TBuf;
  scrh,scrw,lng:integer;
  curr_effect:byte;

procedure MakeGrayEffect(buffer:PBuf;length:dword);
procedure MakeNegativeEffect(buffer:PBuf;length:dword);
procedure MakeFigZnaetKakoyEffect2(buffer:PBuf;length:dword);
procedure MakeFigZnaetKakoyEffect(buffer:PBuf;length:dword);

implementation

{$R *.dfm}

procedure MakeFigZnaetKakoyEffect(buffer:PBuf;length:dword);
var i:dword;
begin
  for i:=0 to length do
  begin
    if buffer^[i]=0 then Continue;
    TRGBACol(buffer^[i]).r:=round(TRGBACol(buffer^[i+5]).r*2);

    TRGBACol(buffer^[i]).g:=round(TRGBACol(buffer^[i]).g*1.5);

    TRGBACol(buffer^[i]).b:=round(TRGBACol(buffer^[i+5]).b*1.5);
  end;
end;

procedure MakeFigZnaetKakoyEffect2(buffer:PBuf;length:dword);
var i:dword; r,rnd:single;
begin
  for i:=0 to length do
  begin
    if buffer^[i]=0 then Continue;
    rnd:=random+1;
    r:=TRGBACol(buffer^[i]).r*1.5*rnd;
    if r>255 then r:=255;
    TRGBACol(buffer^[i]).r:=round(r);

    TRGBACol(buffer^[i]).g:=round(TRGBACol(buffer^[i]).g*rnd);

    TRGBACol(buffer^[i]).b:=round(TRGBACol(buffer^[i]).b*rnd);
  end;
end;

procedure MakeNegativeEffect(buffer:PBuf;length:dword);
var i:dword; red:byte;
begin
  for i:=0 to length do
  begin
    red:=TRGBACol(buffer^[i]).r;
    TRGBACol(buffer^[i]).r:=TRGBACol(buffer^[i]).b;
    TRGBACol(buffer^[i]).b:=red;
  end;
end;

procedure MakeGrayEffect(buffer:PBuf;length:dword);
var i:dword; gray:byte;
begin
  for i:=0 to length do
  begin
    gray:=Round( ( 0.30 * TRGBACol(buffer^[i]).r ) +
                 ( 0.59 * TRGBACol(buffer^[i]).g ) +
                 ( 0.11 * TRGBACol(buffer^[i]).b ) );
    TRGBACol(buffer^[i]).r:=gray;
    TRGBACol(buffer^[i]).g:=gray;
    TRGBACol(buffer^[i]).b:=gray;
  end;
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
if curr_effect=0 then exit;
glReadPixels(0,0,Width,Height,GL_RGBA,GL_UNSIGNED_BYTE,buf);
case curr_effect of
  1:MakeFigZnaetKakoyEffect(@buf,lng);
  2:MakeNegativeEffect(@buf,lng);
  3:MakeGrayEffect(@buf,lng);
  4:MakeFigZnaetKakoyEffect2(@buf,lng);
end;
glDrawPixels(Width,Height,GL_RGBA,GL_UNSIGNED_BYTE,buf);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MikWin_Init(44100, True, True, True, Handle, 0);
  MikWin_Load(PChar('Moving of star.s3m'));
  MikWin_Play(false);
  Randomize;
  scrw:=Width;
  scrh:=Height;
  lng:=scrw*scrh;
  SetLength(buf,lng);
  lng:=lng-1;
  curr_effect:=1;
  GLFreeForm1.LoadFromFile('town.obj');
  GetMovement(GLDummyCube1).StartPathTravel;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  buf:=nil;
  MikWin_Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=vk_escape then
  close;
  if key=VK_NUMPAD0 then
  curr_effect:=0;
  if key=VK_NUMPAD1 then
  curr_effect:=1;
  if key=VK_NUMPAD2 then
  curr_effect:=2;
  if key=VK_NUMPAD3 then
  curr_effect:=3;
  if key=VK_NUMPAD4 then
  curr_effect:=4;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
   if GLHUDText1.Position.X>-1200 then
   GLHUDText1.Position.X:=GLHUDText1.Position.X-5 else
   GLHUDText1.Position.X:=800;
   Caption:=Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
   MikWin_Update;
   if not MikWin_Playing then close;
end;

end.
