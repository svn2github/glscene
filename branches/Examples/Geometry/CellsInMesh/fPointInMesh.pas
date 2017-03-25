unit fPointInMesh;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.mmsystem,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,

  GLScene,
  GLVectorGeometry,
  GLVectorFileObjects,
  GLCadencer,
  GLWin32Viewer,
  GLObjects,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLFile3DS, GLGeomObjects;

type
  TfrmCellsInMesh = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLFreeForm1: TGLFreeForm;
    GLLightSource1: TGLLightSource;
    GLLines1: TGLLines;
    ProgressBar1: TProgressBar;
    GLDummyCube1: TGLDummyCube;
    PanelTop: TPanel;
    cbShowHull: TCheckBox;
    StatusBar1: TStatusBar;
    rgHull: TRadioGroup;
    rgCells: TRadioGroup;
    GLCone1: TGLCone;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure cbShowHullClick(Sender: TObject);
    procedure rgHullClick(Sender: TObject);
    procedure rgCellsClick(Sender: TObject);
  private
    MousePoint: TPoint;
    procedure ShowCameraLocation;
  public
    procedure BuildGrid(Sender : TObject);
  end;

var
  frmCellsInMesh: TfrmCellsInMesh;

implementation

{$R *.dfm}

var
  mx, my : integer;

procedure TfrmCellsInMesh.FormCreate(Sender: TObject);
begin
  randomize;
  GLFreeForm1.LoadFromFile('bunny.glsm');
  GLFreeForm1.BuildOctree;
  GLFreeForm1.Visible := cbShowHull.Checked;
  Show;
  BuildGrid(nil);
end;


procedure TfrmCellsInMesh.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TfrmCellsInMesh.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y, mx-x)
  else if shift = [ssRight] then
    GLCamera1.AdjustDistanceToTarget(1.0 + (y - my) / 100);
  mx := x;
  my := y;
end;

procedure TfrmCellsInMesh.rgCellsClick(Sender: TObject);
begin
   BuildGrid(Sender);
end;

procedure TfrmCellsInMesh.rgHullClick(Sender: TObject);
begin
  case rgHull.ItemIndex of
   0:  GLFreeForm1.LoadFromFile('bunny.glsm');
   1:  GLFreeForm1.LoadFromFile('cube.3ds');
   2:  GLFreeForm1.LoadFromFile('sphere.3ds');
   3:  // Convert GLCone1 -> GLFreeForm1;
  end;
  GLFreeForm1.BuildOctree;
  Show;
  BuildGrid(nil);
end;

procedure TfrmCellsInMesh.ShowCameraLocation;
begin
  with GLCamera1.Position do
    StatusBar1.Panels[0].Text := 'Camera: '+FloatToStrF(X, ffNumber, 5, 2)+', '+
    FloatToStrF(Y, ffNumber, 5, 2)+', '+FloatToStrF(Z, ffNumber, 5, 2);
end;

procedure TfrmCellsInMesh.cbShowHullClick(Sender: TObject);
begin
  GLFreeForm1.Visible := cbShowHull.Checked;
end;

procedure TfrmCellsInMesh.BuildGrid(Sender : TObject);
const
  cResolution = 32;
var
  x, y, z, Hits, Tests : integer;
  step : single;
  Point : TVector;
  brad : single;
  StartTime : cardinal;
begin
  // Note - this could be speeded up enourmously by using a proprietary method
  // instead of OctreePointInMesh - which is recalculated for each node where
  // it could be calculated once per column (cutting down run time by aprox.
  // 1/cResolution).

  // Increasing the bounding sphere radius to generate a box that's guaranteed
  // to encompas the entire freeform
  brad := GLFreeForm1.BoundingSphereRadius*1.42;
  step := brad/(cResolution+1);
  Hits := 0; Tests := 0;
  ProgressBar1.Max := cResolution-1;
  while GLDummyCube1.Count>0 do
    GLDummyCube1.Children[GLDummyCube1.Count-1].Free;
  StartTime := timeGetTime;

  if (rgCells.ItemIndex <> 3)  then
  for x := 0 to cResolution-1 do
  begin
    ProgressBar1.Position := x;
    for y := 0 to cResolution-1 do
      for z := 0 to cResolution-1 do
      begin
        Point := VectorAdd(GLFreeForm1.Position.AsVector,
          VectorMake(x*step-brad/2, y*step-brad/2, z*step-brad/2));
        Inc(Tests);

        if GLFreeForm1.OctreePointInMesh(Point) then
        begin
          inc(Hits);
          if (rgCells.ItemIndex = 0) then  // Cube cells
            with TGLCube(GLDummyCube1.AddNewChild(TGLCube)) do
            begin
              Position.AsVector := Point;
              Material.FrontProperties.Emission.RandomColor;
              CubeDepth := step;
              CubeWidth := step;
              CubeHeight := step;
            end

          else
          if (rgCells.ItemIndex = 1) then  // Sphere cells
            with TGLSphere(GLDummyCube1.AddNewChild(TGLSphere)) do
            begin
              Position.AsVector := Point;
              Material.FrontProperties.Ambient.AsWinColor := clBlue;
              Slices := 5;
              Stacks := 5;
              Radius := step / 2 * 1.42;
            end

          else
          if (rgCells.ItemIndex = 2) then // Sprite cells
            with TGLSprite(GLDummyCube1.AddNewChild(TGLSprite)) do
            begin
              Position.AsVector := Point;
              Material.FrontProperties.Ambient.AsWinColor := clGreen;
              Height := step / 2 * 1.42;
              Width := step / 2 * 1.42;
            end//}
        end;
      end;
  end;
  ProgressBar1.Position := 0;
  Caption := Format('Cells %d ms, %d hits, %d tests',[(timeGetTime-StartTime),Hits, Tests]);
end;

end.
