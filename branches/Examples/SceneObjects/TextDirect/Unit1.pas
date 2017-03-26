unit Unit1;

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

  GLScene,
  GLObjects,
  GLCoordinates,
  GLWin32Viewer,
  GLCrossPlatform,
  GLBaseClasses,
  GLSpaceText,
  GLRenderContextInfo,
  GLCadencer,
  GLAsyncTimer,
  GLBitmapFont,
  GLWindowsFont,
  GLHUDObjects;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    dogl: TGLDirectOpenGL;
    txt3d: TGLSpaceText;
    cad: TGLCadencer;
    at: TGLAsyncTimer;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    txt2d: TGLFlatText;
    dc_txt3d: TGLDummyCube;
    dc_txt2d: TGLDummyCube;
    txthud: TGLHUDText;
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure cadProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure atTimer(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;

  timer1: integer = 0;


implementation

{$R *.dfm}


// doglRender
//
procedure TForm1.doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
var
    i,j,k: integer;
    f: single;
begin
  RandSeed := 2014;
  for i := -2 to 2 do
    for j := -2 to 2 do
      for k := -2 to 2 do begin
        txt3d.Position.SetPoint(i, j, k);
        txt3d.Direction.SetVector(i, j, k);
        txt3d.Text := IntToStr(Round(Random * 99) + timer1 );
        txt3d.Render( rci );
    end;

  for i := 0 to 31 do begin
    f := i * 6.2832 / 32;
    txt2d.Position.SetPoint( 4 * sin( f ), 4 * cos( f ), 0 );
    txt2d.Direction.SetVector( sin( f ), cos( f ), 0 );
    txt2d.Text := IntToStr( round(random * 99) + timer1 );
    txt2d.Render( rci );
  end;
  for i := 0 to 31 do begin
    f := i * 6.2832 / 32 - cad.CurrentTime / 2;
    txthud.Position.SetPoint( vp.Width div 2 + vp.Width * sin(f) * 0.4,
      vp.Height div 2 + vp.Height * cos(f) * 0.4, 0 );
    txthud.Text := IntToStr(Round(Random * 99) + timer1 );
    txthud.Render(rci);
  end;
end;

// cadProgress
//
procedure TForm1.cadProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  dc_txt3d.TurnAngle := dc_txt3d.TurnAngle + deltatime * 12;
  dc_txt2d.RotateAbsolute( deltatime * 27, 0, deltatime * 17 );
end;


// timer
//
procedure TForm1.atTimer(Sender: TObject);
begin
  caption := vp.FramesPerSecondText(2);
  vp.ResetPerformanceMonitor;
  inc( timer1 );
end;

end.
