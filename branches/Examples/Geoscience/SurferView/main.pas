{ -----------------------------------------------------------------------------
  Unit Name: main
  Author:    Aaron Hochwimmer (hochwimmera@pbworld.com)
  Purpose:   Simple demo for viewing Surfer Grid files
  ----------------------------------------------------------------------------- }
unit Main;

interface

uses
  Winapi.OpenGL,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
   
  GLScene,
  GLGraph,
  GLObjects,
  GLWin32Viewer,
  GLColor,
  GLTexture,
  GLVectorGeometry,
  GLNavigator,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  //
  GridImportfn,
  Commasplit;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    miClose: TMenuItem;
    pnlSurferView: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera: TGLCamera;
    DummyCube: TGLDummyCube;
    HeightField1: TGLHeightField;
    N1: TMenuItem;
    miOpenSurfer: TMenuItem;
    OpenSurfer: TOpenDialog;
    lblVerticalExaggeration: TLabel;
    Edit1: TEdit;
    TrackBarz: TTrackBar;
    lblCameraHeight: TLabel;
    Label1: TLabel;
    TrackBarx: TTrackBar;
    GLLightSource1: TGLLightSource;
    TrackBary: TTrackBar;
    Label2: TLabel;
    lblColour1: TLabel;
    lblMaxColour: TLabel;
    pnlMin: TPanel;
    pnlMax: TPanel;
    ColorDialog: TColorDialog;
    miHelp: TMenuItem;
    miAbout: TMenuItem;
    procedure miCloseClick(Sender: TObject);
    procedure miOpenSurferClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Edit1Change(Sender: TObject);
    procedure TrackBarzChange(Sender: TObject);
    procedure TrackBarxChange(Sender: TObject);
    procedure TrackBaryChange(Sender: TObject);
    procedure pnlMaxClick(Sender: TObject);
    procedure pnlMinClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
     
    vscale: single;
    mdx, mdy: Integer;
    glColour1: TGLColor;
    glColour2: TGLColor;
    contourdata: TContourGridData;
    procedure ContourZ(const X, Y: single; var z: single;
      var color: TColorVector; var texPoint: TTexPoint);
    procedure OpenSurferFile(sFileName: string);
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

// ------ TForm1.FormCreate ----------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  glColour1 := TGLColor.Create(nil);
  glColour1.AsWinColor := pnlMin.color;
  glColour2 := TGLColor.Create(nil);
  glColour2.AsWinColor := pnlMax.color;
  contourdata := TContourGridData.Create;
  vscale := 10.0;
end;

// ------ Tform1.ContourZ ------------------------------------------------------
{ ** provides z as a function of x and y. This uses the xscale and yscale vectors
  to "lookup" the correct indices of the points array }
procedure TForm1.ContourZ(const X, Y: single; var z: single;
  var color: TColorVector; var texPoint: TTexPoint);

const
  SMALL = 0.01;

var
  c, i, j: Integer;
  r: single;

begin
  { ** probably not necessary - just sets to the last row and column }
  i := contourdata.ny - 1;
  j := contourdata.nx - 1;

  for c := 0 to contourdata.nx - 1 do
  begin
    if abs(X - contourdata.xscale[c]) < SMALL then
    begin
      j := c;
      break;
    end;
  end;

  for c := 0 to contourdata.ny - 1 do
  begin
    if abs(Y - contourdata.yscale[c]) < SMALL then
    begin
      i := c;
      break;
    end;
  end;

  { ** r is a ratio between 0 and 1. Use this to ramp the colour between colour1
    and colour2 }
  r := (contourdata.points[i, j] - contourdata.zlo) / (contourdata.zrange);
  VectorLerp(glColour1.color, glColour2.color, r, color);

  { ** apply z but remember the vertical exaggeration }
  z := contourdata.points[i, j] * vscale;
end;

// ------ Tform1.OpenSurferFile ------------------------------------------------
procedure TForm1.OpenSurferFile(sFileName: string);

begin
  { ** the LoadSurferGrid function will try and open a surfer grid file in its 3
    different formats - surfer grid, gs binary, gs ascii }
  if (contourdata.LoadSurferGrid(sFileName) = 0) then
  begin
    with HeightField1 do
    begin
      { ** the 0.1 * step is to compensate for rounding }
      xSamplingScale.Min := 0.0;
      xSamplingScale.Max := contourdata.XRange + 0.1 * contourdata.XStep;
      xSamplingScale.Origin := 0.0;
      xSamplingScale.Step := contourdata.XStep;

      { ** the 0.1 * step is to compensate for rounding }
      ySamplingScale.Min := 0.0;
      ySamplingScale.Max := contourdata.yrange + 0.1 * contourdata.yStep;
      ySamplingScale.Origin := 0.0;
      ySamplingScale.Step := contourdata.yStep;
      OnGetHeight := ContourZ;
      Visible := true;
      { ** in this demo the grid is translated so xmin,ymin = 0,0. To display in real
        world coordinates just move the HeightField Position accordingly }

    end;
    { ** move the dummycube, and camera, to a reasonable location }
    DummyCube.Position.X := 0.5 * contourdata.XRange;
    DummyCube.Position.Y := 0.5 * contourdata.yrange;
    DummyCube.Position.z := contourdata.zhi;
    GLCamera.TransformationChanged;

    { ** init the trackbars }
    with TrackBarx do
    begin
      Max := 2 * Round(contourdata.XRange);
      Min := -Max;
      Position := 0;
    end;

    with TrackBary do
    begin
      Max := 2 * Round(contourdata.yrange);
      Min := -Max;
      Position := 0;
    end;

    with TrackBarz do
    begin
      Min := -Round(2 * contourdata.zlo);
      Max := Round(2 * contourdata.zhi);
      Position := Max;
    end;
    { ** apply the vertical scale }
    Edit1Change(nil);
  end;
end;

// ------ TForm1.miCloseClick --------------------------------------------------
procedure TForm1.miCloseClick(Sender: TObject);
begin
  self.close;
end;

// ------ TForm1.miOpenSurferClick ---------------------------------------------
procedure TForm1.miOpenSurferClick(Sender: TObject);
begin
  if OpenSurfer.Execute then
    OpenSurferFile(OpenSurfer.Filename);
end;

// ------ TForm1.FormDestroy ---------------------------------------------------
procedure TForm1.FormDestroy(Sender: TObject);
begin
  glColour1.free;
  glColour2.free;
  contourdata.free;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

// ------ TForm1.GLSceneViewer1MouseDown ---------------------------------------
procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mdx := X;
  mdy := Y;
end;

// ------ TForm1.GLSceneViewer1MouseMove ---------------------------------------
procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  dx, dy: Integer;

begin
  // calculate delta since last move or last mousedown
  dx := mdx - X;
  dy := mdy - Y;
  mdx := X;
  mdy := Y;
  if ssLeft in Shift then
    GLCamera.MoveAroundTarget(dy, dx);
end;

// ------ TForm1.Edit1Change ---------------------------------------------------
procedure TForm1.Edit1Change(Sender: TObject);

var
  ve: single;
  code: Integer;
begin
  val(Edit1.text, ve, code);
  If (code = 0) and (ve > 0.0) then
  begin
    vscale := ve;
    with TrackBarz do
    begin
      Min := -2 * Round(ve * contourdata.zlo);
      Max := 2 * Round(ve * contourdata.zhi);
      Position := Max;
    end;
    HeightField1.StructureChanged; // update
  end;
  GLSceneViewer1.SetFocus;
end;

// ------ TForm1.TrackBarzChange -----------------------------------------------
procedure TForm1.TrackBarzChange(Sender: TObject);
begin
  DummyCube.Position.z := TrackBarz.Position;
  GLCamera.TransformationChanged;
end;

// ------ TForm1.TrackBarxChange -----------------------------------------------
procedure TForm1.TrackBarxChange(Sender: TObject);
begin
  DummyCube.Position.X := TrackBarx.Position;
  GLCamera.TransformationChanged;
end;

// ------ TForm1.TrackBaryChange -----------------------------------------------
procedure TForm1.TrackBaryChange(Sender: TObject);
begin
  DummyCube.Position.Y := TrackBary.Position;
  GLCamera.TransformationChanged;
end;

// ------ TForm1.pnlMaxClick ---------------------------------------------------
procedure TForm1.pnlMaxClick(Sender: TObject);
begin
  ColorDialog.color := pnlMax.color;
  if ColorDialog.Execute then
  begin
    pnlMax.color := ColorDialog.color;
    glColour2.AsWinColor := pnlMax.color;
    HeightField1.StructureChanged;
  end;
end;

// ------ TForm1.pnlMinClick ---------------------------------------------------
procedure TForm1.pnlMinClick(Sender: TObject);
begin
  ColorDialog.color := pnlMin.color;
  if ColorDialog.Execute then
  begin
    pnlMin.color := ColorDialog.color;
    glColour1.AsWinColor := pnlMin.color;
    HeightField1.StructureChanged;
  end;
end;

// ------ TForm1.miAboutClick --------------------------------------------------
procedure TForm1.miAboutClick(Sender: TObject);
begin
  MessageDlg('GridGLS Viewer' + #13 + #10 +
    'based on Aaron Hochwimmer Surfer Grid Parser' + #13 + #10 +
    'Built with GLScene: http://www.glscene.org.', mtWarning, [mbOK], 0);
end;

// =============================================================================
end.
