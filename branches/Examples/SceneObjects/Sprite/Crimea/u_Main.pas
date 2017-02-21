unit u_Main;


interface


uses

  Windows, SysUtils, Classes, Forms, Graphics, Controls, ShellApi,
  StdCtrls, ExtCtrls,

  GLWin32Viewer, GLCrossPlatform, GLBaseClasses, GLScene, GLMaterial,
  GLAsyncTimer, GLCoordinates, GLObjects, GLHUDObjects, GLCadencer, GLRenderContextInfo,
  GLTexture,

  jpeg, uDDStex;


type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    dc_cam: TGLDummyCube;
    cam: TGLCamera;
    hud_stat: TGLHUDSprite;
    cad: TGLCadencer;
    dogl: TGLDirectOpenGL;
    hud_mob: TGLHUDSprite;
    hud_dirt: TGLHUDSprite;
    at: TGLAsyncTimer;
    vp: TGLSceneViewer;
    matlib: TGLMaterialLibrary;
    procedure FormCreate(Sender: TObject);
    procedure cadProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure atTimer(Sender: TObject);
  end;

  t_mob = packed record
    pos: TPoint;
    time: single;
    end;

const
  g_tx = 136/2048;
  g_ty = 138/4096;


var
  Form1: TForm1;

  arr: array[0..39] of t_mob;


implementation


{$R *.dfm}


//
// FormCreate
//
procedure TForm1.FormCreate;
var
    a1: integer;

begin

  DDStex( matlib, 'mob', 'mob.dds' );
  DDStex( matlib, 'stat', 'stat.dds' );
  
  with matlib.LibMaterialByName('mob') do
    TextureScale.SetPoint(g_tx, g_ty, 0);

  randomize;
  for a1 := 0 to high(arr) do
    with arr[a1] do begin

      pos.y := 60 + a1 * (80 - a1) div 4;
      if a1 < 10 then pos.x := 770 - random(200)
        elSe pos.x := 770 - random(pos.y * 2);
      time := random * 342;

      end;

end;


//
// cadProgress
//
procedure TForm1.cadProgress;
begin

  vp.Invalidate;

end;


//
// doglRender
//
procedure TForm1.doglRender;

  procedure newFrame(f: integer);
  begin
    with matlib.LibMaterialByName('mob').TextureOffset do
      setPoint((f mod 15) * g_tx, 1 - (f div 15 + 1) * g_ty, 0);
    end;

var
    i: integer;

begin

  for i := 0 to high(arr) do
    with arr[i] do begin

      hud_mob.Position.SetPoint(pos.x, pos.y, 0);
      newFrame(round((time + cad.CurrentTime) * 10 - 0.5) mod 342);
      hud_mob.Render(rci);

      end;

end;


//
// atTimer
//
procedure TForm1.atTimer;
begin

  caption := 'crimea / ' + vp.FramesPerSecondText(2);
  vp.ResetPerformanceMonitor;

end;


end.
