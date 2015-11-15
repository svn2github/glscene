unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLWin32Viewer, GLScene, GLObjects, GLGeomObjects, StdCtrls,
  GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    Socle: TGLCube;
    Plateau: TGLCube;
    Pied1: TGLCylinder;
    bord1: TGLCube;
    lampe1: TGLFrustrum;
    lum1: TGLLightSource;
    amp1: TGLSphere;
    lum2: TGLLightSource;
    CBspot1: TCheckBox;
    CBspot2: TCheckBox;
    B1: TGLSphere;
    B2: TGLSphere;
    queue: TGLCylinder;
    BtnJouer: TButton;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CBspot1Click(Sender: TObject);
    procedure CBspot2Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure BtnJouerClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my : single;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssleft in shift then
    GLCamera1.MoveAroundTarget(my-y, mx-x);
  mx := x;
  my := y;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.CBspot1Click(Sender: TObject);
begin
  lum1.Shining := CBspot1.Checked;
end;

procedure TForm1.CBspot2Click(Sender: TObject);
begin
  lum2.Shining := CBspot2.Checked;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case key of
    '-': GLCamera1.AdjustDistanceToTarget(1.1);
    '+': GLCamera1.AdjustDistanceToTarget(0.9);
  end;
end;

procedure TForm1.BtnJouerClick(Sender: TObject);
var i : integer;
begin
  for i := 1 to 10 do begin
    sleep (10);
    queue.Position.X := queue.Position.X + 0.02;
    GLSceneViewer1.Refresh;
  end;
  sleep (500);
  for i := 1 to 10 do begin
    sleep (10);
    queue.Position.X := queue.Position.X - 0.02;
    GLSceneViewer1.Refresh;
  end;
  For i := 50 to 350 do begin
    sleep (10);
    B2.Position.X := B2.Position.X - 0.1/(sqrt(i));
    GLSceneViewer1.Refresh;
  end;
end;

end.
