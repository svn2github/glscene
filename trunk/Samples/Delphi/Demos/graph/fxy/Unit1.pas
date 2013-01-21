unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ComCtrls, GLObjects, GLGraph, GLScene, VectorGeometry, VectorTypes,
  GLWin32Viewer, GLCrossPlatform, GLCoordinates, BaseClasses, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    HeightField1: TGLHeightField;
    YZGrid: TGLXYZGrid;
    XZGrid: TGLXYZGrid;
    XYGrid: TGLXYZGrid;
    Panel1: TPanel;
    CBCentered: TCheckBox;
    Label1: TLabel;
    TBXYPosition: TTrackBar;
    TBYZPosition: TTrackBar;
    TBXZPosition: TTrackBar;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CBCenteredClick(Sender: TObject);
    procedure TBXYPositionChange(Sender: TObject);
    procedure HeightField1GetHeight(const x, y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
    procedure TBYZPositionChange(Sender: TObject);
    procedure TBXZPositionChange(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.HeightField1GetHeight(const x, y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
   Z:=VectorNorm(x, y);
   Z:=cos(z*12)/(2*(z*6.28+1));
end;

procedure TForm1.CBCenteredClick(Sender: TObject);
begin
   if CBCentered.Checked then begin
      XZGrid.YSamplingScale.Origin:=0;
      YZGrid.XSamplingScale.Origin:=0;
   end else begin
      XZGrid.YSamplingScale.Origin:=-1;
      YZGrid.XSamplingScale.Origin:=-1;
   end;
end;

procedure TForm1.TBXYPositionChange(Sender: TObject);
begin
   XYGrid.ZSamplingScale.Origin:=-(TBXYPosition.Position/10);
end;



procedure TForm1.TBYZPositionChange(Sender: TObject);
begin
   YZGrid.XSamplingScale.Origin:=-(TBYZPosition.Position/10);
end;

procedure TForm1.TBXZPositionChange(Sender: TObject);
begin
   XZGrid.YSamplingScale.Origin:=-(TBXZPosition.Position/10);
end;

// following code takes care of camera movement, see camera & movement demos
// for explanations and more samples

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=X; my:=Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x; my:=y;
   end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   GLCamera1 := GLSceneViewer1.Camera;
   // Note that 1 wheel-step induces a WheelDelta of 120,
   // this code adjusts the distance to target with a 10% per wheel-step ratio
   GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;


end.
