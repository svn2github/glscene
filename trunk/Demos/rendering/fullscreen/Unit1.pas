{: Basic demo for using the FullScreen Viewer and GLCanvas in GLScene.<p>

   This demo uses no forms, but switches directly to 800x600x32bpp fullscreen
   mode and renders a teapot with a colored light, hitting 'ESC' will leave
   full screen mode.<p>

   The demo also makes use of GLCanvas to render a custom 2D overlay with
   TGLCanvas, here a yellow reticle at mouse position.
}
unit Unit1;

interface

uses
  Forms, Controls, SysUtils, Classes, GLMisc, GLScene, GLObjects, GLWin32Viewer,
  Graphics;

type
  TDataModule1 = class(TDataModule)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Teapot1: TGLTeapot;
    GLFullScreenViewer1: TGLFullScreenViewer;
    DCBlueLight: TGLDummyCube;
    GLLightSource2: TGLLightSource;
    procedure DataModuleCreate(Sender: TObject);
    procedure GLFullScreenViewer1KeyPress(Sender: TObject; var Key: Char);
    procedure GLFullScreenViewer1PostRender(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    firstPassDone : Boolean;
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.dfm}

uses Windows, OpenGL12, GLCanvas;

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
   // Adjusts Zoom to size (might have been modified in the IDE, by you, user!)
   GLCamera1.SceneScale:=GLFullScreenViewer1.Width/160;
   // Start fullscreen mode, no cursor
   GLFullScreenViewer1.Cursor:=crNone;
   GLFullScreenViewer1.Active:=True;
   while GLFullScreenViewer1.Active do begin
      // Message queue is not operational, but there may still be some messages
      Forms.Application.ProcessMessages;
      // Relinquish some of that CPU time
      Sleep(1);
      // Slowly rotate the teapot and the blue light
      Teapot1.TurnAngle:=4*Frac(Now*24)*3600;
      DCBlueLight.RollAngle:=32*Frac(Now*24)*3600;
   end;
end;

procedure TDataModule1.GLFullScreenViewer1PostRender(Sender: TObject);
var
   glc : TGLCanvas;
   x, y : Integer;
begin
   glc:=TGLCanvas.Create(GLFullScreenViewer1.Width, GLFullScreenViewer1.Height);
   with glc do begin
      x:=Mouse.CursorPos.X;
      y:=Mouse.CursorPos.Y;
      PenColor:=clYellow;

      // Alpha-transparency antialiasing:
      // we render the ellipse twice, the first pass with a very transparent
      // wide pen, and a second time with a thinner pen.
      PenAlpha:=0.4;
      PenWidth:=3;
      Ellipse(x-16, y-16, x+16, y+16);
      PenAlpha:=0.75;
      PenWidth:=2;
      Ellipse(x-16, y-16, x+16, y+16);
      // Complete the reticle
      PenAlpha:=0.3;
      PenWidth:=2;
      Line(x-32, y, x+32, y);
      Line(x, y-32, x, y+32);
   end;
   glc.Free;
end;

procedure TDataModule1.GLFullScreenViewer1KeyPress(Sender: TObject;
  var Key: Char);
begin
   // ESC leaves fullscreen mode
   if Key=#27 then begin
      GLFullScreenViewer1.Active:=False;
      Key:=#0;
   end;
end;

end.
