unit FMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Math,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ComCtrls,
  // GLS
  GLScene,
  GLWin32Viewer,
  GLObjects,
  GLVectorFileObjects,
  GLVectorGeometry,
  GLState,
  GLMesh,
  GLColor,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLIsosurface,
  GLSpline,
  GLMaterial;

type
  TFrmMain = class(TForm)
    GLSceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
    glcMainCamera: TGLCamera;
    DCDummyCube: TGLDummyCube;
    PUSerInterface: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lblVertices: TLabel;
    lblTriangles: TLabel;
    RGAlgorithm: TRadioGroup;
    ffFreeForm: TGLFreeForm;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLLightSource1: TGLLightSource;
    RAWireFrameFill: TRadioGroup;
    tbSize: TTrackBar;
    tbIsoValue: TTrackBar;
    Label3: TLabel;
    Label4: TLabel;
    rbgShading: TRadioGroup;
    TrackBar1: TTrackBar;
    Label5: TLabel;
    rbgInterpolation: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure RGAlgorithmClick(Sender: TObject);
    procedure FillBlock;
    function MakeSphere(Rad: Integer; var ADataout: TSingle3DArray;
      var Dims: array of word; CPt: array of Integer): Integer;
    procedure rbgInvertClick(Sender: TObject);
    procedure tbSizeChange(Sender: TObject);
    procedure rbgShadingClick(Sender: TObject);
    procedure GeneratePosns;
    procedure TrackBar1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ExtractedVertices: TVertexArray; // array of TVertex
    ExtractedTriangles: TIntegerArray; // array of Integer
    Dimensions: array ['x' .. 'z'] of word;
    CenterPt: array ['x' .. 'z'] of Integer;
    CenterPts: array of array ['x' .. 'z'] of Integer;

    SingleData: TSingle3DArray; // array of array of array of Single
    mdx, mdy: Integer;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  SingleData := nil;
  Generateposns;
  FillBlock;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  SetLength(SingleData, 0, 0, 0);
end;

procedure TFrmMain.rbgInvertClick(Sender: TObject);
begin
  FillBlock;
end;

procedure TFrmMain.GeneratePosns;
var
  DataAmount: cardinal;
  i: Integer;
  X, Y, z: Integer;
begin
  // don't redraw when the trackbar is exited, already drawn when it was moved
  if TrackBar1.Position = Length(CenterPts) then
    exit;

  Dimensions['x'] := tbSize.Position;
  Dimensions['y'] := tbSize.Position;
  Dimensions['z'] := tbSize.Position;

  SetLength(CenterPts, TrackBar1.Position);
  if TrackBar1.Position = 1 then
  begin
    CenterPts[0, 'x'] := 50;
    CenterPts[0, 'y'] := 50;
    CenterPts[0, 'z'] := 50;
  end
  else
    for i := 0 to TrackBar1.Position - 1 do
    begin
      CenterPts[i, 'x'] := random(100);
      CenterPts[i, 'y'] := random(100);
      CenterPts[i, 'z'] := random(100);
    end;
end;

procedure TFrmMain.FillBlock;
var
  DataAmount: cardinal;
  i: Integer;
  X, Y, Z: Integer;
begin
  Dimensions['x'] := tbSize.Position;
  Dimensions['y'] := tbSize.Position;
  Dimensions['z'] := tbSize.Position;

  SetLength(SingleData, 0, 0, 0);
  SetLength(SingleData, tbSize.Position, tbSize.Position, tbSize.Position);

  for i := 0 to TrackBar1.Position - 1 do
  begin
    CenterPt['x'] := round(CenterPts[i, 'x'] / 100 * tbSize.Position);
    CenterPt['y'] := round(CenterPts[i, 'y'] / 100 * tbSize.Position);
    CenterPt['z'] := round(CenterPts[i, 'z'] / 100 * tbSize.Position);
    MakeSphere(round(tbSize.Position / 2.5), SingleData, Dimensions, CenterPt);
  end;

  RGAlgorithmClick(Self);
end;

function TFrmMain.MakeSphere(Rad: Integer; var ADataout: TSingle3DArray;
  var Dims: array of word; CPt: array of Integer): Integer;
var
  X, Y, Z: Integer;
  DataPoint: Byte;
  Counter: Integer;
  v: single;
begin
  // SetLength(ADataout, Dims[0], Dims[1], Dims[2]);
  Counter := 0;

  for X := -Rad to Rad do
    for Y := -Rad to Rad do
      for Z := -Rad to Rad do
      begin
        v := Sin(DegToRad(((X + Rad) / (2 * Rad)) * 180)) *
          Sin(DegToRad(((Y + Rad) / (2 * Rad)) * 180)) *
          Sin(DegToRad(((Z + Rad) / (2 * Rad)) * 180));
        if (v > 0) and ((X + CPt[0]) >= 0) and ((X + CPt[0]) <= high(ADataout))
          and ((Y + CPt[1]) >= 0) and ((Y + CPt[1]) <= high(ADataout[0])) and
          ((z + CPt[2]) >= 0) and ((z + CPt[2]) <= high(ADataout[0, 0])) and
          (ADataout[X + CPt[0], Y + CPt[1], z + CPt[2]] < v * 255) then
          ADataout[X + CPt[0], Y + CPt[1], z + CPt[2]] := v * 255;
        inc(Counter);
      end;

  Result := Counter;
end;

procedure TFrmMain.RGAlgorithmClick(Sender: TObject);
var
  IsoSurfaceEx: TIsoSurfaceExtractor;
  i: Integer;
  mo: TMeshObject;
begin
  // Create IsoSurfaceExtractor
  IsoSurfaceEx := TIsoSurfaceExtractor.Create(Dimensions['x'], Dimensions['y'],
    Dimensions['z'], SingleData);
  // Launch Calculation
  case RGAlgorithm.ItemIndex of
    0:
      IsoSurfaceEx.MarchingTetrahedra(tbIsoValue.Position, ExtractedVertices,
        ExtractedTriangles, rbgInterpolation.ItemIndex.ToBoolean);
    1:
      IsoSurfaceEx.MarchingCubes(tbIsoValue.Position, ExtractedVertices,
        ExtractedTriangles, rbgInterpolation.ItemIndex.ToBoolean);
  end;

  lblVertices.Caption := Format('%d', [length(ExtractedVertices)]);
  lblTriangles.Caption := Format('%d', [length(ExtractedTriangles) div 3]);
  IsoSurfaceEx.Free();

  ffFreeForm.MaterialLibrary := GLMaterialLibrary1;

  ffFreeForm.MeshObjects.Clear();
  mo := TMeshObject.CreateOwned(ffFreeForm.MeshObjects);
  for i := length(ExtractedTriangles) - 1 downto 0 do
    with ExtractedVertices[ExtractedTriangles[i]] do
      mo.Vertices.Add(AffineVectorMake(X - Dimensions['x'] / 2,
        Y - Dimensions['y'] / 2, z - Dimensions['z'] / 2));

  ffFreeForm.StructureChanged;
  if RAWireFrameFill.ItemIndex = 0 then
    ffFreeForm.Material.PolygonMode := pmFill
  else
    ffFreeForm.Material.PolygonMode := pmLines;
  GLSceneViewer.Invalidate();
end;

procedure TFrmMain.tbSizeChange(Sender: TObject);
begin
  FillBlock;
end;

procedure TFrmMain.TrackBar1Change(Sender: TObject);
begin
  GeneratePosns;
  FillBlock;
end;


procedure TFrmMain.rbgShadingClick(Sender: TObject);
begin
  if rbgShading.ItemIndex = 0 then
    GLSceneViewer.Buffer.ShadeModel := smFlat
  else
    GLSceneViewer.Buffer.ShadeModel := smSmooth;
end;

procedure TFrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  glcMainCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TFrmMain.GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mdx := X;
  mdy := Y;
end;

procedure TFrmMain.GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);

var
  dx, dy: Integer;
  v: TVector;
begin
  // calculate delta since last move or last mousedown
  dx := mdx - X;
  dy := mdy - Y;
  mdx := X;
  mdy := Y;
  if ssLeft in Shift then
  begin
    if ssShift in Shift then
    begin
      // right button with shift rotates the teapot
      // (rotation happens around camera's axis)
      glcMainCamera.RotateObject(DCDummyCube, dy, dx);
    end
    else
    begin
      // right button without shift changes camera angle
      // (we're moving around the parent and target dummycube)
      glcMainCamera.MoveAroundTarget(dy, dx)
    end;
  end
  else if Shift = [ssRight] then
  begin
    // left button moves our target and parent dummycube
    v := glcMainCamera.ScreenDeltaToVectorXY(dx, -dy,
      0.12 * glcMainCamera.DistanceToTarget / glcMainCamera.FocalLength);
    DCDummyCube.Position.Translate(v);
    // notify camera that its position/target has been changed
    glcMainCamera.TransformationChanged;
  end;
end;


{ procedure TFrmMain.FillBlock;
  var
  DataAmount: cardinal;
  i: Integer;
  x, y, z : integer;
  begin
  Dimensions['x'] := tbSize.Position;
  Dimensions['y'] := tbSize.Position;
  Dimensions['z'] := tbSize.Position;
  CenterPt['x'] := tbSize.Position div 2;
  CenterPt['y'] := tbSize.Position div 2;
  CenterPt['z'] := tbSize.Position div 2;

  SetLength(SingleData, 0, 0, 0);
  SetLength(SingleData, tbSize.Position, tbSize.Position, tbSize.Position);

  for i := 1 to trackbar1.Position do
  begin
  MakeSphere(round(tbSize.Position / 2.5),SingleData,Dimensions,CenterPt);
  CenterPt['x'] := random(tbsize.Position);
  CenterPt['y'] := random(tbsize.Position);
  CenterPt['z'] := random(tbsize.Position);
  end;

  RGAlgorithmClick(Self);
  end; }
end.
