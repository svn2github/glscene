{ : GLGizmoEx component demo.

  Version History:

  07/10/2009 - Predator - Updated version.
  29/09/2007 - DaStr - Initial version.

}
unit DemoGizmoForm;

interface

uses
  // VCL
  SysUtils,
  Classes,
  windows,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Buttons,
  ComCtrls,

  // GLScene
  GLScene.SimpleNavigation,
  GLScene.BitmapFont,
  GLScene.BitmapFont.System,
  GLScene.Cadencer,
  GLScene.ObjectsEx,
  GLScene.Objects,
  GLScene.Core,
  GLScene.Objects.GraphPlotting,
  GLScene.Base.Coordinates,
  GLScene.Platform,
  GLScene.Viewer.VCL,
  GLScene.Base.Vector.Geometry,
  GLScene.GizmoEx;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLRootObjects: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLLightSource2: TGLLightSource;
    RootGizmo: TGLDummyCube;
    GLDodecahedron3: TGLDodecahedron;
    GLArrowLine3: TGLArrowLine;
    GLArrowLine4: TGLArrowLine;
    Label9: TLabel;
    GLSphere1: TGLSphere;
    GLCube1: TGLCube;
    GLFrustrum1: TGLFrustrum;
    GLDisk1: TGLDisk;
    GLCube2: TGLCube;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    Label12: TLabel;
    Viewer: TGLSceneViewer;
    SpeedButton6: TSpeedButton;
    Panel4: TPanel;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    RootTempObjects: TGLDummyCube;
    SpeedButton11: TSpeedButton;
    Label15: TLabel;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    edAutoZoomFactor: TEdit;
    edtGizmoThickness: TEdit;
    edtScaleCoef: TEdit;
    edzoomfactor: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OptPickMode: TRadioGroup;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    ColorBox4: TColorBox;
    Label1: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label16: TLabel;
    ComboBox3: TComboBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    Panel1: TPanel;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    CheckBox3: TCheckBox;
    edMoveCoef: TEdit;
    Label6: TLabel;
    edRotateCoef: TEdit;
    Label7: TLabel;
    CheckBox16: TCheckBox;
    TabSheet3: TTabSheet;
    Label8: TLabel;
    Edit1: TEdit;
    Label13: TLabel;
    Panel3: TPanel;
    TreeView1: TTreeView;
    GLRootUserInterface: TGLDummyCube;
    GLXYZGrid1: TGLXYZGrid;
    GLTargetCamera: TGLDummyCube;
    Camera: TGLCamera;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    Panel5: TPanel;
    GroupBox1: TGroupBox;
    SpeedButton16: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    Label17: TLabel;
    Timer1: TTimer;
    GLSystemBitmapFont1: TGLSystemBitmapFont;
    GLSimpleNavigation1: TGLSimpleNavigation;
    CoordSysButton: TButton;
    procedure GLCadencer1Progress(Sender: TObject;
      const DeltaTime, newTime: Double);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure edAutoZoomFactorKeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox12Click(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure edtGizmoThicknessChange(Sender: TObject);
    procedure OptPickModeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure UpdateTreeView;
    procedure ComboBox3Change(Sender: TObject);
    function MouseWorldPos(const X, Y: Integer; isy: boolean = false): TVector;
    procedure CoordSysClick(Sender: TObject);
    procedure SpeedButton6MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton7MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton17Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure SpeedButton16Click(Sender: TObject);
    function ObjectName(value: string): string;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my: Integer;
    MousePos, LostMousePos: TVector;
    MouseMoving: boolean;
    pos: TVector;
    FObj: TGLBaseSceneObject;

  end;

var
  Form1: TForm1;
  Gizmo: TGLGizmoEx;
  FVectorLength: Single;
  FCreationScenarious: Integer;

implementation

{$R *.dfm}

procedure SettingsObj(Obj: TGLBaseSceneObject; Step: Integer; Length: TVector);
begin
  if (Obj is TGLCube) then
    with (Obj as TGLCube) do
      case Step of
        0:
          begin
            CubeWidth := Length[0] * 2;
            CubeDepth := Length[2] * 2;
          end;
        1:
          CubeHeight := FVectorLength;
        2:
          FCreationScenarious := -1;
      end;

  if (Obj is TGLSphere) then
    with (Obj as TGLSphere) do
      case Step of
        0:
          Radius := FVectorLength;
        1:
          FCreationScenarious := -1;
      end;

  if (Obj is TGLPlane) then
    with (Obj as TGLPlane) do
      case Step of
        0:
          begin
            Width := Length[0] * 2;
            Height := Length[2] * 2;
          end;
        1:
          FCreationScenarious := -1;
      end;
end;

function TForm1.ObjectName(value: string): string;
var
  i: Integer;
begin
  Result := value;
  i := 1;
  while GLScene1.FindSceneObject(Result) <> nil do
  begin
    Result := value + IntToStr(i);
    Inc(i);
  end;
end;

function TForm1.MouseWorldPos(const X, Y: Integer;
  isy: boolean = false): TVector;
var
  v: TVector;
  InvertedY: Integer;
begin

  InvertedY := Viewer.Height - Y;

  SetVector(v, X, InvertedY, 0);
  if not isy then
    Viewer.Buffer.ScreenVectorIntersectWithPlaneXZ(v,
      GLTargetCamera.AbsolutePosition[1], Result)
  else
    Viewer.Buffer.ScreenVectorIntersectWithPlaneXY(v,
      GLTargetCamera.AbsolutePosition[2], Result)
end;

procedure TForm1.UpdateTreeView;
{ .: AddNodes :. }
  function AddNodes(ANode: TTreeNode; AObject: TGLBaseSceneObject): TTreeNode;
  var
    i: Integer;
    CurrentNode: TTreeNode;
  begin
    if IsSubComponent(AObject) then
    begin
      Result := TreeView1.Selected;
      exit;
    end
    else
    begin
      Result := TreeView1.Items.AddChildObject(ANode, AObject.Name, AObject);
      CurrentNode := Result;
      for i := 0 to AObject.Count - 1 do
        Result := AddNodes(CurrentNode, AObject[i]);
    end;
  end;

var
  i: Integer;
  ObjectNode: TTreeNode;
begin
  TreeView1.Items.Clear;
  // -- add two root nodes --
  ObjectNode := TreeView1.Items.AddFirst(nil, 'RootTempObjects');
  // -- get the object's tree --
  TreeView1.Items.BeginUpdate();
  with RootTempObjects do
  begin
    // -- objects (with children too) --
    if Assigned(RootTempObjects) then
    begin
      ObjectNode.Data := RootTempObjects;
      with RootTempObjects do
        for i := 0 to Count - 1 do
          AddNodes(ObjectNode, Children[i]);
      ObjectNode.Expand(True);
    end;
  end;
  TreeView1.Items.EndUpdate();

  // -- add two root nodes --
  ObjectNode := TreeView1.Items.AddFirst(nil, 'World');
  // -- get the object's tree --
  TreeView1.Items.BeginUpdate();
  with GLRootObjects do
  begin
    // -- objects (with children too) --
    if Assigned(GLRootObjects) then
    begin
      ObjectNode.Data := GLRootObjects;
      with GLRootObjects do
        for i := 0 to Count - 1 do
          AddNodes(ObjectNode, Children[i]);
      ObjectNode.Expand(True);
    end;
  end;
  TreeView1.Items.EndUpdate();
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const DeltaTime, newTime: Double);
begin
  Viewer.invalidate;
end;

procedure TForm1.OptPickModeClick(Sender: TObject);
begin
  Gizmo.PickMode := TGLGizmoExPickMode(OptPickMode.ItemIndex);
end;

procedure TForm1.SpeedButton14Click(Sender: TObject);
begin
  Gizmo.Enabled := not((SpeedButton18.Down or SpeedButton15.Down) or
    SpeedButton14.Down);
  CheckBox12.Checked := Gizmo.Enabled;
end;

procedure TForm1.SpeedButton16Click(Sender: TObject);
begin
  Gizmo.Enabled := not(Sender as TSpeedButton).Down;
end;

procedure TForm1.SpeedButton17Click(Sender: TObject);
begin
  GLTargetCamera.Position.SetPoint(0, 0, 0);
  GLTargetCamera.Direction.SetVector(0, 0, 1);
  GLTargetCamera.Up.SetVector(0, 1, 0);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  (Sender as TSpeedButton).Down := false;
  case (Sender as TSpeedButton).Tag of
    0:
      Gizmo.OperationMode := gomSelect;
    1:
      Gizmo.OperationMode := gomMove;
    2:
      Gizmo.OperationMode := gomRotate;
    3:
      Gizmo.OperationMode := gomScale;
    4:
      Gizmo.OperationMode := gomNone;
    5:
      Gizmo.Undo;
    6:
      Gizmo.Redo;
    7:
      Gizmo.RemoveSelectedObjects;
  end;
  UpdateTreeView;

  // Disable buttons Camera Translate
  SpeedButton18.Down := false;
  SpeedButton15.Down := false;
  SpeedButton14.Down := false;
  SpeedButton17.Down := false;
end;

procedure TForm1.SpeedButton6MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Panel4.Visible := not Panel4.Visible;
  Panel4.Left := SpeedButton6.Left;
  Panel4.Top := SpeedButton6.Top + SpeedButton6.Height;
end;

procedure TForm1.SpeedButton7MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case (Sender as TSpeedButton).Tag of
    20:
      begin
        Gizmo.SelectionRegion := gsrRectangular;
        SpeedButton6.Glyph.Assign(SpeedButton7.Glyph);
      end;
    21:
      begin
        Gizmo.SelectionRegion := gsrCircular;
        SpeedButton6.Glyph.Assign(SpeedButton8.Glyph);
      end;
    22:
      begin
        Gizmo.SelectionRegion := gsrFence;
        SpeedButton6.Glyph.Assign(SpeedButton9.Glyph);
      end;
    23:
      begin
        Gizmo.SelectionRegion := gsrLasso;
        SpeedButton6.Glyph.Assign(SpeedButton10.Glyph);
      end;
  end;
  Panel4.Visible := false;
  SpeedButton6.Down := false;
end;

procedure TForm1.ViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  Gizmo.ViewerMouseDown(X, Y);
  if SpeedButton15.Down or SpeedButton18.Down then
  begin
    if SpeedButton15.Down then
      LostMousePos := MouseWorldPos(X, Y)
    else
      LostMousePos := MouseWorldPos(X, Y, True);
    pos := GLTargetCamera.Position.AsVector;
    MouseMoving := True;
  end;

  // Create Cube With Mouse
  if SpeedButton16.Down then
  begin
    LostMousePos := MouseWorldPos(X, Y);
    MouseMoving := True;

    if FCreationScenarious = -1 then
    begin
      FObj := TGLCube.CreateAsChild(GLRootObjects);
      (FObj as TGLCube).CubeDepth := 0;
      (FObj as TGLCube).CubeHeight := 0;
      (FObj as TGLCube).CubeWidth := 0.1;
      FObj.Position.AsVector := LostMousePos;
      FObj.Name := ObjectName('GLCube');
      UpdateTreeView;
      FCreationScenarious := 0;
    end;
  end;

  // Create Sphere With Mouse
  if SpeedButton19.Down then
  begin
    LostMousePos := MouseWorldPos(X, Y);
    MouseMoving := True;

    if FCreationScenarious = -1 then
    begin
      FObj := TGLSphere.CreateAsChild(GLRootObjects);
      (FObj as TGLSphere).Radius := 0;
      FObj.Position.AsVector := LostMousePos;
      FObj.Name := ObjectName('GLSphere');
      UpdateTreeView;
      FCreationScenarious := 0;
    end;
  end;
  // Create Sphere With Mouse
  if SpeedButton20.Down then
  begin
    LostMousePos := MouseWorldPos(X, Y);
    MouseMoving := True;

    if FCreationScenarious = -1 then
    begin
      FObj := TGLPlane.CreateAsChild(GLRootObjects);
      (FObj as TGLPlane).Height := 0;
      (FObj as TGLPlane).Width := 0;
      FObj.Position.AsVector := LostMousePos;
      FObj.PitchAngle := 90;
      FObj.Name := ObjectName('GLPlane');
      UpdateTreeView;
      FCreationScenarious := 0;
    end;
  end;

end;

procedure TForm1.ViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  Gizmo.MoveCoef := Camera.DistanceToTarget / 1000;

  if Shift <> [ssRight] then
  begin
    Gizmo.ViewerMouseMove(X, Y);

    if MouseMoving then
    begin
      if SpeedButton15.Down or SpeedButton18.Down then
      begin
        if SpeedButton15.Down then
          MousePos := MouseWorldPos(X, Y)
        else
          MousePos := MouseWorldPos(X, Y, True);
        MousePos := VectorSubtract(LostMousePos, MousePos);
        MousePos[0] := -MousePos[0] * 0.4;
        MousePos[2] := -MousePos[2] * 0.4;
        GLTargetCamera.Position.AsVector := Vectoradd(pos, MousePos);
      end;
      if SpeedButton16.Down or SpeedButton19.Down or SpeedButton20.Down then
      begin
        MousePos := MouseWorldPos(X, Y);
        FVectorLength := VectorLength(VectorSubtract(LostMousePos, MousePos));
        pos := VectorSubtract(LostMousePos, MousePos);
        pos := VectorAbs(pos);
        SettingsObj(FObj, FCreationScenarious, pos)
      end;
    end;

  end;

  mx := X;
  my := Y;
end;

procedure TForm1.ViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

  MouseMoving := false;
  Gizmo.ViewerMouseUp(X, Y);

  if TGLMouseButton(Button) = mbRight then
  begin
    if Gizmo.CursorSelectingRegion then
    begin
      Gizmo.LooseCursorSelection;
    end;
    if SpeedButton15.Down or SpeedButton14.Down or SpeedButton18.Down then
    begin
      SpeedButton15.Down := false;
      SpeedButton14.Down := false;
      SpeedButton18.Down := false;
      Gizmo.Enabled := True;
      CheckBox12.Checked := True;
    end;
    if SpeedButton16.Down or SpeedButton19.Down or SpeedButton20.Down then
    begin
      if FCreationScenarious >= 0 then
        FreeAndNil(FObj);
      Gizmo.Enabled := True;
      FCreationScenarious := -1;
      SpeedButton16.Down := false;
      SpeedButton19.Down := false;
      SpeedButton20.Down := false;
      UpdateTreeView;
    end;

  end;

  if SpeedButton16.Down or SpeedButton19.Down or SpeedButton20.Down then
  begin
    if (FCreationScenarious = 0) and
      (VectorLength(VectorSubtract(LostMousePos, MousePos)) < 0.1) then
    begin
      FreeAndNil(FObj);
      SpeedButton16.Down := false;
      FCreationScenarious := -1;
    end;
    LostMousePos := MouseWorldPos(X, Y);
    FCreationScenarious := FCreationScenarious + 1;
    MouseMoving := True;
  end;

end;

procedure TForm1.ViewerMouseWheel(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  Gizmo.UpdateGizmo;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Viewer.SetFocus;
  Gizmo.RootGizmo := RootGizmo;
  Gizmo.RootObjects := GLRootObjects;
  Gizmo.GizmoTmpRoot := RootTempObjects;
  Gizmo.ExcludeObjects := True;
  Gizmo.ExcludeObjectsList.Add('GLXYZGrid1');
  Gizmo.ExcludeObjectsList.Add('GLHUDText1');
  Camera.TurnAngle := 45;
  Camera.PitchAngle := -45;
  UpdateTreeView;
end;

procedure TForm1.edAutoZoomFactorKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.',
    ',']) then
    Key := #0;
end;

procedure TForm1.CheckBox12Click(Sender: TObject);
var
  check: boolean;
begin
  check := (Sender as TCheckBox).Checked;
  case (Sender as TCheckBox).Tag of
    0:
      Gizmo.Enabled := check;
    1:
      Gizmo.ExcludeObjects := check;
    2:
      Gizmo.ExcludeClassname := check;
    3:
      Gizmo.EnableLoopCursorMoving := check;
    4:
      Gizmo.EnableMultiSelection := check;

    5:
      Gizmo.EnableActionHistory := check;
    6:
      Gizmo.ShowBoundingBox := check;
    7:
      Gizmo.ShowAxisLabel := check;
    8:
      begin
        Gizmo.ShowObjectInfos := not Gizmo.ShowObjectInfos;
        CheckBox7.Enabled := check;
        CheckBox8.Enabled := check;
        CheckBox9.Enabled := check;
      end;
    9:
      if check then
        Gizmo.VisibleInfoLabels := Gizmo.VisibleInfoLabels + [vliName]
      else
        Gizmo.VisibleInfoLabels := Gizmo.VisibleInfoLabels - [vliName];
    10:
      if check then
        Gizmo.VisibleInfoLabels := Gizmo.VisibleInfoLabels + [vliOperation]
      else
        Gizmo.VisibleInfoLabels := Gizmo.VisibleInfoLabels - [vliOperation];
    11:
      if check then
        Gizmo.VisibleInfoLabels := Gizmo.VisibleInfoLabels + [vliCoords]
      else
        Gizmo.VisibleInfoLabels := Gizmo.VisibleInfoLabels - [vliCoords];

    12:
      Gizmo.NoZWrite := check;
    13:
      Gizmo.AntiAliasedLines := check;
    14:
      Gizmo.CanChangeWithChildren := check;
    15:
      begin
        Gizmo.AutoZoom := check;
        if check then
        begin
          edAutoZoomFactor.Enabled := True;
          edzoomfactor.Enabled := false;
        end
        else
        begin
          edAutoZoomFactor.Enabled := false;
          edzoomfactor.Enabled := True;
        end;
      end;
  end;
end;

procedure TForm1.edtGizmoThicknessChange(Sender: TObject);
var
  value: Single;
begin

  tryStrToFloat((Sender as TEdit).Text, value);
  if value > 0 then
    case (Sender as TEdit).Tag of
      1:
        Gizmo.GizmoThickness := value;
      2:
        Gizmo.ScaleCoef := value;
      3:
        Gizmo.MoveCoef := value;
      4:
        Gizmo.RotationCoef := value;
      5:
        Gizmo.HistoryStepsCount := Round(value);
      6:
        Gizmo.AutoZoomFactor := value;
      7:
        Gizmo.ZoomFactor := value;
    end;
end;

procedure TForm1.ColorBox1Change(Sender: TObject);
begin
  case (Sender as TColorBox).Tag of
    0:
      Gizmo.BoundingBoxColor.AsWinColor := ColorBox1.Selected;
    1:
      Gizmo.VisibleInfoLabelsColor.AsWinColor := ColorBox2.Selected;
    2:
      Gizmo.SelectedColor.AsWinColor := ColorBox3.Selected;
    3:
      Gizmo.SelectionRegionColor.AsWinColor := ColorBox4.Selected;
  end;
end;

procedure TForm1.ComboBox3Change(Sender: TObject);
begin
  case ComboBox3.ItemIndex of
    0:
      Gizmo.InfoLabelCoordType := ilcChanging;
    1:
      Gizmo.InfoLabelCoordType := ilcChangeRate;
  end;
end;

procedure TForm1.CoordSysClick(Sender: TObject);
var
  s: string;
begin
  Gizmo.ReferenceCoordSystem := TGLGizmoExReferenceCoordinateSystem
    (CoordSysButton.Tag);
  CoordSysButton.Tag := 1 - CoordSysButton.Tag;
  s := CoordSysButton.Caption;
  CoordSysButton.Caption := CoordSysButton.Hint;
  CoordSysButton.Hint := s;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Gizmo := TGLGizmoEx.Create(Self);
  Gizmo.LabelFont := GLSystemBitmapFont1;
  Gizmo.Viewer := Viewer;
  Gizmo.ExcludeClassnameList.Add('TGLSphere');
  Gizmo.AutoZoomFactor := 10;
  FCreationScenarious := -1;
  GLSimpleNavigation1.OnMouseWheel := Self.ViewerMouseWheel;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Gizmo.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Gizmo.CanAddObjToSelectionList := (Key = VK_Control);
  Gizmo.CanRemoveObjFromSelectionList := (Key = VK_MENU);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Gizmo.CanAddObjToSelectionList := false;
  Gizmo.CanRemoveObjFromSelectionList := false;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Viewer.FramesPerSecondText();
  Viewer.ResetPerformanceMonitor;

  if GLScene1.IsUpdating then
    UpdateTreeView;
end;

end.
