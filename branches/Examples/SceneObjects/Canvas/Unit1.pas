unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLWin32Viewer, GLScene, GLObjects, GLTeapot, GLCoordinates,
  GLCrossPlatform, GLBaseClasses, GLCanvas, GLColor;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLLightSource1: TGLLightSource;
    Teapot1: TGLTeapot;
    DCBlueLight: TGLDummyCube;
    GLLightSource2: TGLLightSource;
    GLCamera1: TGLCamera;
    GLSceneViewer1: TGLSceneViewer;
    procedure GLSceneViewer1PostRender(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
end;

procedure TForm1.GLSceneViewer1PostRender(Sender: TObject);
var
  glc: TGLCanvas;
  x, y: integer;
  c: TColor;

begin
  glc := TGLCanvas.Create(GLSceneViewer1.Width, GLSceneViewer1.Height);
  with glc do
  begin
    PenColor := Clwhite;
    FillRect(10, 200, 640, 300);
    PenColor := ClRed;
    RoundRect(10, 200, 640, 300, 10, 10);
    PenColor := ClGreen;
    for x := 20 to 630 do
    for y := 202 to 299 do
    begin
      PenColor := RGB(Random(255), Random(255), Random(255));
      PlotPixel(x, y)
    end;
  end;
  glc.Free;
end;

end.
