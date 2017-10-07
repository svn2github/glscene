unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,

  GLWin32Viewer,
  GLScene,
  GLObjects,
  GLHUDObjects,
  GLTexture,
  GLSelection,
  GLGeomObjects,
  GLCanvas,
  GLCadencer,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLAnnulus1: TGLAnnulus;
    GLTorus1: TGLTorus;
    Memo1: TMemo;
    GLCadencer1: TGLCadencer;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLSceneViewer1PostRender(Sender: TObject);
  public
    x1, y1: Integer;
    btn: boolean;
    R: TRect;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // **********************************************
  if ssLeft in Shift then
  begin
    R := rect(0, 0, 0, 0); // reset the rectangle
    x1 := X;
    y1 := Y;
    btn := true;
  end;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (btn) then
  begin
    if (X > x1) and (Y > y1) then
    begin
      R.Top := y1;
      R.Left := x1;
      R.Right := X;
      R.Bottom := Y;
    end;

    if (Y < y1) and (X > x1) then
    begin
      R.Top := Y;
      R.Left := x1;
      R.Right := X;
      R.Bottom := y1;
    end;

    if (X < x1) and (Y > y1) then
    begin
      R.Top := y1;
      R.Left := X;
      R.Right := x1;
      R.Bottom := Y;
    end;

    if (X < x1) and (Y < y1) then
    begin
      R.Top := Y;
      R.Left := X;
      R.Right := x1;
      R.Bottom := y1;
    end;

    Caption := 'Top: ' + IntToStr(R.Top) + ' Left: ' + IntToStr(R.Left) +
      ' Right: ' + IntToStr(R.Right) + ' Bottom: ' + IntToStr(R.Bottom);
  end;
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  List: TGLPickList;
  i: Integer;
begin
  btn := false;

  List := GLSceneViewer1.Buffer.GetPickedObjects(R);
  Memo1.Lines.Clear;
  for i := 0 to List.Count - 1 do
    Memo1.Lines.Add(List.Hit[i].ClassName);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLSceneViewer1PostRender(Sender: TObject);
var
  glc: TGLCanvas;
begin
  if btn then
  begin
    glc := TGLCanvas.Create(GLSceneViewer1.Width, GLSceneViewer1.Height);
    with glc do
    begin
      PenWidth := 1;
      PenColor := clBlack;
      FrameRect(R.Left + 1, R.Top + 1, R.Right, R.Bottom);
      PenColor := clLime;
      FrameRect(R.Left, R.Top, R.Right - 1, R.Bottom - 1);
    end;
    glc.Free;
  end;
end;

end.
