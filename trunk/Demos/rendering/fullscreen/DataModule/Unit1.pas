unit Unit1;

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, GLScene,
  GLFullscreenViewer,GLTeapot,GLObjects,GLCanvas,graphics, GLCadencer,
  GLCoordinates, GLCrossPlatform, BaseClasses;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLTeapot1: TGLTeapot;
    GLFullScreenViewer1: TGLFullScreenViewer;
    DCBlueLight: TGLDummyCube;
    GLLightSource2: TGLLightSource;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure GLFullScreenViewer1KeyPress(Sender: TObject; var Key: char);
    procedure GLFullScreenViewer1PostRender(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DataModule1: TDataModule1;
  Form: TForm;
  FSActive: boolean;

implementation

{$R *.dfm}

{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
   Form := TForm.Create(nil);
   Form.Show;
   GLCamera1.SceneScale:=GLFullScreenViewer1.Width/160;
   GLFullScreenViewer1.Cursor:=crNone;
   GLFullScreenViewer1.Form := Form;
   GLFullScreenViewer1.Active:=True;
   FSActive :=true;
   while FSActive  do begin
      // Relinquish some of that CPU time
      Forms.Application.ProcessMessages;
      // Relinquish some of that CPU time
      Sleep(1);
      GLTeapot1.TurnAngle:=4*Frac(Now*24)*3600;
      DCBlueLight.RollAngle:=32*Frac(Now*24)*3600;
   end;
end;

procedure TDataModule1.DataModuleDestroy(Sender: TObject);
begin
     Form.Free;
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
      Ellipse(x, y, 16, 16);
      PenAlpha:=0.75;
      PenWidth:=2;
      Ellipse(x, y, 16, 16);
      // Complete the reticle
      PenAlpha:=0.3;
      PenWidth:=2;
      Line(x-32, y, x+32, y);
      Line(x, y-32, x, y+32);
   end;
   glc.Free;
end;

procedure TDataModule1.GLFullScreenViewer1KeyPress(Sender: TObject;
  var Key: char);
begin
     // ESC leaves fullscreen mode
   if Key=#27 then begin
      FSActive := false;
      GLFullScreenViewer1.Active:=False;
      Key:=#0;
      halt;
   end;
end;


end.
