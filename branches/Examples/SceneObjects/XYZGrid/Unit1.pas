unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.IniFiles,
  System.Math,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.Jpeg,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,

  OpenGL1x,
  GLWin32Viewer,
  GLScene,
  GLObjects,
  GLMesh,
  GLTexture,
  GLVectorTypes,
  GLVectorGeometry,
  GLVectorFileObjects,
  GLVectorLists,
  GLMeshUtils,
  GLCadencer,
  GLCrossPlatform,
  GLContext,
  GLUtils,
  GLGraph,
  GLFile3DS,
  GLFileTGA,
  GLPolyhedron,
  GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    GLCadencer: TGLCadencer;
    CamH: TGLDummyCube;
    CamV: TGLDummyCube;
    Camera: TGLCamera;
    AmbientLight: TGLLightSource;
    GLIcosahedron1: TGLIcosahedron;
    GLXYZGrid1: TGLXYZGrid;
    procedure GLCadencerProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    mx, my: Integer;
    MousePos: TPoint;
    NZ: Integer; // number of steps along Z axes
    OffSet: Single;
    IsLeftMouseDown, IsRightMouseDown: Boolean;
    GLXYZGridArray: array of TGLXYZGrid;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;

begin
  // SetLength(GLXYZGridArray, 100);
  NZ := 10;
  OffSet := (NZ div 2) * GLXYZGrid1.ZSamplingScale.Step;
  for I := 0 to NZ - 1 do
  begin
    GLXYZGrid1.ZSamplingScale.Origin := I * 10 - OffSet;
    // GLXYZGridArray[I].ZSamplingScale.Origin := I * 10 - OffSet;
  end;
end;

procedure TForm1.GLCadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  deltax, deltay: Single;
  pt: TPoint;
begin
  if (IsLeftMouseDown or IsRightMouseDown) then
  begin
    GetCursorPos(pt);
    deltax := (MousePos.X - pt.X) / 5;
    deltay := (MousePos.Y - pt.Y) / 5;
    if (pt.X <> MousePos.X) or (pt.Y <> MousePos.Y) then
      SetCursorPos(MousePos.X, MousePos.Y);
  end;
  // rotate camera
  if IsLeftMouseDown then
  begin
    CamH.TurnAngle := CamH.TurnAngle + deltax;
    if CamH.TurnAngle >= 360 then
      CamH.TurnAngle := CamH.TurnAngle - 360;
    if CamH.TurnAngle < 0 then
      CamH.TurnAngle := CamH.TurnAngle + 360;

    // limit camera movement with upper hemisphere
    if (CamV.PitchAngle - deltay < 89) and (CamV.PitchAngle - deltay > 0) then
      CamV.PitchAngle := CamV.PitchAngle - deltay;
  end
  else
  begin
    // pan camera
    if IsRightMouseDown then
    begin
      CamH.Move(-200 * deltay * deltaTime);
      CamH.Slide(200 * deltax * deltaTime);
    end;
  end;
  Viewer.Invalidate;
end;

procedure TForm1.ViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  if (ssRight in Shift) then
    IsRightMouseDown := True;

  if (ssLeft in Shift) then
  begin
    mx := X;
    my := Y;
    IsLeftMouseDown := True;
  end;

  Screen.Cursor := crNone;
  GetCursorPos(MousePos);
end;

procedure TForm1.ViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton(mbLeft) then
    IsLeftMouseDown := False;

  if Button = TMouseButton(mbRight) then
    IsRightMouseDown := False;

  if not((ssLeft in Shift) or (ssRight in Shift)) then
    Screen.Cursor := crDefault;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SetLength(GLXYZGridArray, 0);
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  DistDelta: Single;
begin
  with Camera do
  begin
    DistDelta := Power(1.1, WheelDelta / 240);

    if (DistanceToTarget > 10) or (WheelDelta > 0) then
      AdjustDistanceToTarget(DistDelta);
  end;
end;

end.
