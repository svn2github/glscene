unit MainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.jpeg,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.Samples.Gauges,
   
  GLVectorGeometry,
  GLVectorTypes,
  GLScene,
  GLObjects,
  GLGraph,
  GLTexture,
  GLAsyncTimer,
  GLMesh,
  GLVectorFileObjects,
  GLCadencer,
  GLWin32Viewer,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,

  GrdFuncs;

Const
  StepSize = 1.0; { height grid steps to move with arrow key }
  RotAngle = 6; // angle to rotate with each key press

type
  TfrmMain = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    HeightField1: TGLHeightField;
    GLLightSource1: TGLLightSource;
    AsyncTimer1: TGLAsyncTimer;
    StatusBar1: TStatusBar;
    DummyCube1: TGLDummyCube;
    Panel1: TPanel;
    rgViewMode: TRadioGroup;
    VEedit: TEdit;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    miOpenSurferGrid: TMenuItem;
    Exit1: TMenuItem;
    N2: TMenuItem;
    miOpenArcInfoGrid: TMenuItem;
    OpenDialog1: TOpenDialog;
    CheckBoxTexture: TCheckBox;
    rgShadeModel: TRadioGroup;
    rgRelief: TRadioGroup;
    procedure HeightField1GetHeight(const X, Y: Single; var Z: Single;
      var Color: TVector4f; var TexPoint: TTexPoint);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure rgViewModeClick(Sender: TObject);
    procedure VEeditChange(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure miOpenArcInfoGridClick(Sender: TObject);
    procedure miOpenSurferGridClick(Sender: TObject);
    procedure CheckBoxTextureClick(Sender: TObject);
    procedure rgShadeModelClick(Sender: TObject);
    procedure rgReliefClick(Sender: TObject);
  private
     
    Grid2D: TGrid2D;
    mx, my: Integer;
    procedure SetHeightFieldLimits;
  public

    Vertical_exaggeration: Single;
    Moving_altitude: Single;
    FileName: TFileName;
    procedure UpdateStatusBarPos;
    procedure UpdateStatusBarAngle;
    procedure CalcCameraHeight;
    procedure HandleKeys;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}


procedure TfrmMain.FormCreate(Sender: TObject);

begin
  Vertical_exaggeration := 3.0;
  Moving_altitude := 10; // height above landscape in fly mode
  Grid2D := TGrid2D.Create;
  FormatSettings.DecimalSeparator := '.';

  Grid2D.LoadFromArcinfo('Kapiti_Arcinfo.grd');
  HeightField1.Material.Texture.Image.LoadFromFile('Kapiti.jpg');
  HeightField1.Material.Texture.Disabled := False;
{}
//  Grid2D.LoadFromSurfer('Helens_Surfer.grd');

  SetHeightFieldLimits;
end;

procedure TfrmMain.SetHeightFieldLimits;
begin
  HeightField1.XSamplingScale.Min := -(Grid2D.Nx div 2);
  HeightField1.XSamplingScale.Max := (Grid2D.Nx div 2);
  HeightField1.YSamplingScale.Min := -(Grid2D.Ny div 2);
  HeightField1.YSamplingScale.Max := (Grid2D.Ny div 2);

  CalcCameraHeight; // adjust height of camera flight
  UpdateStatusBarPos;
  UpdateStatusBarAngle;
end;

// This assigns the heights from the grid in memory
//
procedure TfrmMain.HeightField1GetHeight(const X, Y: Single; var Z: Single;
  var Color: TVector4f; var TexPoint: TTexPoint);

var
  Height: Single;

begin
  Height := Grid2D.Nodes[Trunc(X) + (Grid2D.Nx div 2),
    Trunc(Y) + (Grid2D.Ny div 2)];
  if Height = Grid2D.NoData then
    Z := 0
  else
    Z := (Height / Grid2D.CellSize) * Vertical_exaggeration;
  { The equation above is based on fact that each x,y is a step on the grid and
    in real terms, each step is cellsize metres wide }
  TexPoint.S := (X - HeightField1.XSamplingScale.min) / Grid2D.Nx;
  TexPoint.T := (Y - HeightField1.YSamplingScale.min) / Grid2D.Ny;
  if rgRelief.ItemIndex = 1 then
    Z := - Z;
end;


procedure TfrmMain.miOpenArcInfoGridClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Arcinfo ASCII Grid (*.grd)|*.grd';
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  OpenDialog1.Filename:='*.grd' ;
  if OpenDialog1.Execute then
  begin
    Grid2D.LoadFromArcinfo(OpenDialog1.Filename);
  end;
  // load texture
  HeightField1.Material.Texture.Image.LoadFromFile('Kapiti.jpg');

  SetHeightFieldLimits;
end;

procedure TfrmMain.miOpenSurferGridClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Surfer ASCII Grid (*.grd)|*.grd';
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  OpenDialog1.Filename:='*.grd' ;
  if OpenDialog1.Execute then
  begin
    Grid2D.LoadFromSurfer(OpenDialog1.Filename);
    Grid2D.CellSize := Grid2D.Dy;
  end;

  // load texture
 // HeightField1.Material.Texture.Image.LoadFromFile('Hellens.jpg');

  SetHeightFieldLimits;
end;


procedure TfrmMain.FormShow(Sender: TObject);
begin
  GLSceneViewer1.SetFocus;
end;


procedure TfrmMain.HandleKeys;
var
  poschanged: Boolean;

begin
  poschanged := False;
  if GetAsyncKeyState(VK_UP) < 0 then
  begin
    GLCamera1.Move(StepSize);
    poschanged := True;
  end;

  if GetAsyncKeyState(VK_DOWN) < 0 then
  begin
    GLCamera1.Move(-StepSize);
    poschanged := True;
  end;

  if GetAsyncKeyState(VK_LEFT) < 0 then
  begin
    if GetAsyncKeyState(VK_MENU) < 0 then
    begin
      GLCamera1.Slide(-StepSize);
      poschanged := True;
    end
    else
    begin
      GLCamera1.Turn(-RotAngle);
      UpdateStatusBarangle;
    end;
  end;

  if GetAsyncKeyState(VK_RIGHT) < 0 then
  begin
    if GetAsyncKeyState(VK_MENU) < 0 then
    begin
      GLCamera1.Slide(StepSize);
      poschanged := True;
    end
    else
    begin
      GLCamera1.Turn(RotAngle);
      UpdateStatusBarangle;
    end;
  end;
  If poschanged then
  begin
    CalcCameraHeight; // adjust fly height so it above the surface
    UpdateStatusBarPos;
  end;
end;

procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TfrmMain.UpdateStatusBarPos;
begin
  // shows camera position in real world units
  StatusBar1.Panels[0].Text :=
    Format('X = %f', [Grid2D.Xo + ((Grid2D.Nx div 2) - GLCamera1.Position.Z) *
    Grid2D.CellSize]);
  StatusBar1.Panels[1].Text :=
    Format('Y = %f', [Grid2D.Yo + ((Grid2D.Ny div 2) - GLCamera1.Position.X) *
    Grid2D.CellSize]);
  StatusBar1.Panels[2].Text :=
    Format('Z = %f', [(GLCamera1.Position.Y - Moving_altitude) /
    Vertical_exaggeration * Grid2D.CellSize]);
end;

procedure TfrmMain.UpdateStatusBarAngle;
var
  angle: Single;
  X, Z: Single;
begin
  // shows camera direction in normal compass heading
  X := GLCamera1.Direction.X;
  Z := GLCamera1.Direction.Z;
  angle := (ArcTan(Z / X) * 180 / pi);
  if (X > 0) then
    angle := 180 + angle;
  if (X < 0) and (Z > 0) then
    angle := 360 + angle;
  StatusBar1.Panels[3].Text := Format('Heading = %f', [angle]);
end;

// Adjusts camera height so it is above the landscape
//
procedure TfrmMain.CalcCameraHeight;
var
  Height: Single;
  X, Z: Integer;
begin
  if Assigned(Grid2D) then
  begin
    X := Trunc(GLCamera1.Position.X); // Left
    Z := Trunc(GLCamera1.Position.Z); // Front to Back

    Height := Grid2D.Nodes[X + (Grid2D.Nx div 2), Z + (Grid2D.Ny div 2)];
    if Height = Grid2D.NoData then
      Height := 0;

    GLCamera1.Position.Y := (Height / Grid2D.CellSize) * Vertical_exaggeration +
      Moving_altitude;
  end;
end;

procedure TfrmMain.CheckBoxTextureClick(Sender: TObject);
begin
  if CheckBoxTexture.Checked then
    HeightField1.Material.Texture.Disabled := False
  else
    HeightField1.Material.Texture.Disabled := True;
  GLSceneViewer1.Invalidate;
end;

procedure TfrmMain.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TfrmMain.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);

var
  dx, dy: Integer;
  v: TVector;

begin
  // calculate delta since last move or last mousedown
  dx := mx - X;
  dy := my - Y;
  mx := X;
  my := Y;
  if ssLeft in Shift then
    // right button without shift changes camera angle
    // (we're moving around the parent and target dummycube)
    GLCamera1.MoveAroundTarget(dy, dx)
  else if Shift = [ssRight] then
  begin
    // left button moves our target and parent dummycube
    v := GLCamera1.ScreenDeltaToVectorXY(dx, -dy,
      0.12 * GLCamera1.DistanceToTarget / GLCamera1.FocalLength);
    DummyCube1.Position.Translate(v);
    // notify camera that its position/target has been changed
    GLCamera1.TransformationChanged;
  end;
  my := Y;
  mx := X;
end;

procedure TfrmMain.rgReliefClick(Sender: TObject);
begin
  HeightField1.StructureChanged;
  GLSceneViewer1.SetFocus;
end;

procedure TfrmMain.rgShadeModelClick(Sender: TObject);
begin
  if rgShadeModel.ItemIndex = 0 then
    GLSceneViewer1.Buffer.ShadeModel := smFlat
  else
    GLSceneViewer1.Buffer.ShadeModel := smSmooth;
  GLSceneViewer1.SetFocus;
end;

procedure TfrmMain.rgViewModeClick(Sender: TObject);
{ switch between fly through and examine modes }
begin
  with GLCamera1 do
  begin
    if rgViewMode.ItemIndex = 0 then
    begin
      TargetObject := nil;
      Position.X := 0;
      Position.Z := 4;
      Direction.X := 1;
      Direction.Y := 0;
      Direction.Z := 0;
      DummyCube1.Direction.X := 1;
      DummyCube1.Direction.Y := 0;
      DummyCube1.Direction.Z := 0;
      DummyCube1.TurnAngle := 0;

      CalcCameraHeight;
    end
    else
    begin
      TargetObject := DummyCube1;
      Position.Y := 150;
      DummyCube1.Direction.X := 0;
      DummyCube1.Direction.Y := 0;
      DummyCube1.Direction.Z := 1;
      DummyCube1.TurnAngle := 180;
    end;
  end;
  GLSceneViewer1.SetFocus;
end;

procedure TfrmMain.VEeditChange(Sender: TObject);
// change the vertical exaggeration
var
  ve: Single;
  code: Integer;
begin
  Val(VEedit.Text, ve, code);
  if (code = 0) and (ve > 0.0) then
  begin
    Vertical_exaggeration := ve;
    HeightField1.StructureChanged; { force getheightfield to be called again }
  end;
  GLSceneViewer1.SetFocus;
end;

procedure TfrmMain.AsyncTimer1Timer(Sender: TObject);
begin
  // only handle keys when in fly mode
  if rgViewMode.ItemIndex = 0 then
    HandleKeys;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Grid2D.Free;
end;

end.
