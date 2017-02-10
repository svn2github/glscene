unit FMain;

interface

uses
  Winapi.OpenGL,
  Winapi.Windows,
  Winapi.Messages,
  System.Math,
  System.SysUtils,
  System.UITypes,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,

  // GLS
  GLScene,
  GLWin32Viewer,
  GLObjects,
  GLVectorFileObjects,
  GLVectorGeometry,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLIsosurface,
  GLSpline;

type
  TFrmMain = class(TForm)
    GLSceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
    OpenDialog: TOpenDialog;
    glcMainCamera: TGLCamera;
    DCDummyCube: TGLDummyCube;
    PUSerInterface: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lblVertices: TLabel;
    lblTriangles: TLabel;
    LEXDim: TLabeledEdit;
    LEYDim: TLabeledEdit;
    LEZDim: TLabeledEdit;
    LEIsoVal: TLabeledEdit;
    RGAlgorithm: TRadioGroup;
    ffFreeForm: TGLFreeForm;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    miFileOpen: TMenuItem;
    miFileExit: TMenuItem;
    N3: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
    procedure RGAlgorithmClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ExtractedVertices: TVertexArray; // array of TVertex
    ExtractedTriangles: TIntegerArray; // array of Integer
    Dimensions: array ['x' .. 'z'] of word;

    SingleData: TSingle3DArray; // array of array of array of Single
    mdx, mdy: Integer;

    // Load Data from file
    function LoadCharData(AFileName: String; out ADataout: TSingle3DArray;
      var Dims: array of word): Integer;
  public
    { Public-Deklarationen }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  SingleData := nil;
end;

function TFrmMain.LoadCharData(AFileName: String; out ADataout: TSingle3DArray;
  var Dims: array of word): Integer;
var
  DataFile: File of Byte;
  i, j, k: Integer;
  DataPoint: Byte;
  Counter: Integer;
begin
  AssignFile(DataFile, AFileName);
  Reset(DataFile);

  SetLength(ADataout, Dims[0], Dims[1], Dims[2]);

  i := 0;
  j := 0;
  k := 0;
  Counter := 0;
  try
    repeat
      Read(DataFile, DataPoint);
      ADataout[i, j, k] := DataPoint;
      inc(i);
      if (i = Dims[0]) then
      begin
        i := 0;
        inc(j);
      end;
      if (j = Dims[1]) then
      begin
        j := 0;
        inc(k);
      end;
      inc(Counter);
    until Eof(DataFile);
  finally
    Closefile(DataFile);
  end;
  Result := Counter;
end;

procedure TFrmMain.miFileOpenClick(Sender: TObject);
var
  DataAmount: cardinal;
begin
  Dimensions['x'] := StrToInt(LEXDim.Text);
  Dimensions['y'] := StrToInt(LEYDim.Text);
  Dimensions['z'] := StrToInt(LEZDim.Text);
  OpenDialog.InitialDir := ExtractFilePath(Application.exename);
  OpenDialog.Filter := 'Volumes|*.vol';
  if OpenDialog.Execute() then
  begin
    DataAmount := LoadCharData(OpenDialog.FileName, SingleData, Dimensions);
    MessageDlg(format('%d read. %dx%dx%d', [DataAmount, Dimensions['x'],
      Dimensions['y'], Dimensions['z']]), mtInformation, [mbOK], -1);
    LEXDim.Text := Format('%d', [Dimensions['x']]);
    LEYDim.Text := Format('%d', [Dimensions['y']]);
    LEZDim.Text := Format('%d', [Dimensions['z']]);
  end;
  RGAlgorithmClick(Self);
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
      IsoSurfaceEx.MarchingTetrahedra(StrToFloat(LEIsoVal.Text), ExtractedVertices,
        ExtractedTriangles, False);
    1:
      IsoSurfaceEx.MarchingCubes(StrToFloat(LEIsoVal.Text), ExtractedVertices,
        ExtractedTriangles, False);
  end;

  lblVertices.Caption := Format('%d', [Length(ExtractedVertices)]);
  lblTriangles.Caption := Format('%d', [Length(ExtractedTriangles) div 3]);
  IsoSurfaceEx.Free();

  ffFreeForm.MeshObjects.Clear();
  mo := TMeshObject.CreateOwned(ffFreeForm.MeshObjects);
  for i := 0 to Length(ExtractedTriangles) - 1 do
    mo.Vertices.Add(AffineVectorMake(ExtractedVertices[ExtractedTriangles[i]].X
      - Dimensions['x'] / 2, ExtractedVertices[ExtractedTriangles[i]].Y -
      Dimensions['y'] / 2, ExtractedVertices[ExtractedTriangles[i]].Z -
      Dimensions['z'] / 2));
  ffFreeForm.StructureChanged;
  GLSceneViewer.Invalidate();
end;

procedure TFrmMain.miFileExitClick(Sender: TObject);
begin
  SetLength(SingleData, 0, 0, 0);
  Application.Terminate();
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

end.
