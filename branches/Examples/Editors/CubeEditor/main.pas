unit Main;

interface

uses
  Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.Imaging.jpeg,

  GLCadencer,
  GLObjects,
  GLScene,
  GLGraph,
  GLWin32Viewer,
  GLKeyboard,
  GLNavigator,
  GLTexture,
  GLVectorFileObjects,
  GLMesh,
  GLVectorGeometry,
  GLVectorTypes,
  GLMeshUtils,
  GLVectorLists,
  GLPersistentClasses,
  GLMaterial,
  GLColor,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    GLScene1: TGLScene;
    dcBlock: TGLDummyCube;
    Block: TGLCube;
    GLDummyCube2: TGLDummyCube;
    Camera: TGLCamera;
    grdExtruder: TGLXYZGrid;
    GLCadencer1: TGLCadencer;
    GLUserInterface1: TGLUserInterface;
    GLNavigator1: TGLNavigator;
    grdField: TGLXYZGrid;
    Label8: TLabel;
    XLabel: TLabel;
    Label10: TLabel;
    YLabel: TLabel;
    Label12: TLabel;
    ZLabel: TLabel;
    GLFreeForm1: TGLFreeForm;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Edit1: TEdit;
    glsViewer: TGLSceneViewer;
    GLNavigator2: TGLNavigator;
    pnRight: TPanel;
    Memo1: TMemo;
    cbBottom: TCheckBox;
    btAddCubes: TButton;
    cbBack: TCheckBox;
    cbFront: TCheckBox;
    cbRight: TCheckBox;
    cbLeft: TCheckBox;
    cbTop: TCheckBox;
    LabelParts: TLabel;
    DepthEdit: TEdit;
    BackHeightEdit: TEdit;
    FrontHeightedit: TEdit;
    WidthEdit: TEdit;
    SpeedButton10: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton1: TSpeedButton;
    LabelDepth: TLabel;
    LabelBHeight: TLabel;
    LabelWidth: TLabel;
    LabelFHeight: TLabel;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure glsViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure glsViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure glsViewerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rRBClick(Sender: TObject);
    procedure PartsCBClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure btAddCubesClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    OldPt: TPoint; // used to keep track of where the mouse button was pressed
    RotAngle: Integer;
    AddExtraVertex: Boolean;
  public
    Material: Word;
    { X,Y,Z - position coordinates,
      Fh - Front height, Bh - Back height,
      D - , Yoffs - ,
      Rot - angle of rotation,
      Front, Back, Top, Bottom, Left, Right - show sides,
      AddExtraVertex - no }
    procedure BuildCube(X, Y, Z, W, Fh, Bh, D, Yoffs, Rot: Single;
      Front, Back, Top, Bottom, Left, Right, AddExtraVertex: Boolean; AMaterial: Word);
    procedure DeleteObject;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function s2f(s: string): Single;
var
  fs: TFormatSettings;
begin
  fs.DecimalSeparator := ',';
  if not trystrtofloat(s, result, fs) then
  begin
    fs.DecimalSeparator := '.';
    if not trystrtofloat(s, result, fs) then
      result := 0;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  dir: string;
begin
  dir := extractfilepath(paramstr(0));
  // load the materials

  GLMaterialLibrary1.AddTextureMaterial('DefaultFront', dir + 'media\fronttex.jpg');
  GLMaterialLibrary1.AddTextureMaterial('DefaultBack', dir + 'media\backtex.jpg');
  GLMaterialLibrary1.AddTextureMaterial('DefaultTop', dir + 'media\toptex.jpg');
  GLMaterialLibrary1.AddTextureMaterial('DefaultBottom', dir + 'media\bottex.jpg');
  GLMaterialLibrary1.AddTextureMaterial('DefaultLeft', dir + 'media\lefttex.jpg');
  GLMaterialLibrary1.AddTextureMaterial('DefaultRight', dir + 'media\righttex.jpg');

  GLFreeForm1.Material.FrontProperties.Emission.color := clrYellow;
  // don't want lighting on these textures
  for i := 0 to GLMaterialLibrary1.Materials.Count - 1 do
    GLMaterialLibrary1.Materials[i].Material.MaterialOptions := [moNoLighting];
  // init globals
  RotAngle := 0;
  AddExtraVertex := false;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // used to take focus away from any of the other controls when moving around the scene
  Edit1.SetFocus;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  i: Integer;
label
  update;
begin
  // camera movement n stuff
  i := 0;
  // delete a cube
  if IsKeyDown(vk_Delete) then
  begin
    DeleteObject;
    Edit1.SetFocus;
    while IsKeyDown(vk_Delete) and (i < 5) do
    begin
      Inc(i);
      Sleep(100);
      Application.ProcessMessages;
    end;
    goto update;
  end;

  // move forward
  if IsKeyDown(VK_UP) then
  begin
    Edit1.SetFocus;
    dcBlock.Move(1);
    GLDummyCube2.Position := dcBlock.Position;
    while IsKeyDown(VK_UP) and (i < 2) do
    begin
      Inc(i);
      Sleep(100);
      Application.ProcessMessages;
    end;
    goto update;
  end;

  // move back
  if IsKeyDown(VK_DOWN) then
  begin
    Edit1.SetFocus;
    dcBlock.Move(-1);
    GLDummyCube2.Position := dcBlock.Position;
    while IsKeyDown(VK_DOWN) and (i < 2) do
    begin
      Inc(i);
      Sleep(100);
      Application.ProcessMessages;
    end;
    goto update;
  end;

  // move left
  if IsKeyDown(VK_LEFT) then
  begin
    Edit1.SetFocus;
    dcBlock.Slide(-1);
    GLDummyCube2.Position := dcBlock.Position;
    while IsKeyDown(VK_LEFT) and (i < 2) do
    begin
      Inc(i);
      Sleep(100);
      Application.ProcessMessages;
    end;
    goto update;
  end;

  // move right
  if IsKeyDown(VK_RIGHT) then
  begin
    Edit1.SetFocus;
    dcBlock.Slide(1);
    GLDummyCube2.Position := dcBlock.Position;
    while IsKeyDown(VK_RIGHT) and (i < 2) do
    begin
      Inc(i);
      Sleep(100);
      Application.ProcessMessages;
    end;
    goto update;
  end;

  // move down
  if IsKeyDown(VK_NEXT) then
  begin
    Edit1.SetFocus;
    dcBlock.Lift(-1);
    GLDummyCube2.Position := dcBlock.Position;
    while IsKeyDown(VK_NEXT) and (i < 2) do
    begin
      Inc(i);
      Sleep(100);
      Application.ProcessMessages;
    end;
    goto update;
  end;

  // move up
  if IsKeyDown(VK_PRIOR) then
  begin
    Edit1.SetFocus;
    dcBlock.Lift(1);
    GLDummyCube2.Position := dcBlock.Position;
    while IsKeyDown(VK_PRIOR) and (i < 2) do
    begin
      Inc(i);
      Sleep(100);
      Application.ProcessMessages;
    end;
    goto update;
  end;

  // add a cube
  if IsKeyDown(VK_INSERT) then
  begin
    Edit1.SetFocus;
    // delete a cube that is in the currently selected
    // area before adding another one.
    DeleteObject;
    with dcBlock.Position do
      BuildCube(X, Y, Z,
        s2f(WidthEdit.text),
        s2f(FrontHeightedit.text),
        s2f(BackHeightEdit.text),
        s2f(DepthEdit.text), 0.5, RotAngle,
        cbFront.checked, cbBack.checked, cbTop.checked, cbBottom.checked,
        cbLeft.checked, cbRight.checked,
        AddExtraVertex, Material);
    while IsKeyDown(VK_SPACE) and (i < 5) do
    begin
      Inc(i);
      Sleep(100);
      Application.ProcessMessages;
    end;
    goto update;
  end;
  // update the mouselook
  GLUserInterface1.MouseLook;
  GLUserInterface1.MouseUpdate;

update:
  // makes the movement a little less confusing as the view is rotated
  // Get GLNavigator.pas from CVS with the CurrentHAngle property added
  dcBlock.TurnAngle := round((GLNavigator2.CurrentHAngle / 90)) * 90;
  // update the coord status
  XLabel.Caption := Format('%.2f', [dcBlock.Position.X]);
  YLabel.Caption := Format('%.2f', [dcBlock.Position.Y]);
  ZLabel.Caption := Format('%.2f', [dcBlock.Position.Z]);
end;

procedure TForm1.DeleteObject;
var
  i: Integer;
  v1, v2, v3, v4, v5, v6: TVector3f;
begin
  // a silly little procedure to detect and delete cubes from the freeform that are
  // occupying the currently selected area
  for i := GLFreeForm1.MeshObjects.count - 1 downto 0 do
  begin
    with dcBlock.Position do
    begin
      MakeVector(v1, (X * 10) + (dcBlock.CubeSize * 5) - 0.1, (Y * 10), (Z * 10));
      MakeVector(v2, (X * 10) - (dcBlock.CubeSize * 5) + 0.1, (Y * 10), (Z * 10));
      MakeVector(v3, (X * 10), (Y * 10) + (dcBlock.CubeSize * 5) - 0.1, (Z * 10));
      MakeVector(v4, (X * 10), (Y * 10) - (dcBlock.CubeSize * 5) + 0.1, (Z * 10));
      MakeVector(v5, (X * 10), (Y * 10), (Z * 10) + (dcBlock.CubeSize * 5) - 0.1);
      MakeVector(v6, (X * 10), (Y * 10), (Z * 10) - (dcBlock.CubeSize * 5) + 0.1);
    end;
    with GLFreeForm1.MeshObjects[i] do
      if PointInObject(v1) or PointInObject(v2) or PointInObject(v3) or
        PointInObject(v4) or PointInObject(v5) or PointInObject(v6) then
      begin
        GLFreeForm1.MeshObjects.Delete(i);
        GLFreeForm1.StructureChanged;
      end;
  end;
end;

procedure TForm1.glsViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton.mbleft then
  begin
    // take the focus away from the other controls,
    Edit1.SetFocus;
    // store old mouse pos
    OldPt := glsViewer.clienttoscreen(point(X, Y));
    // and activate the mouse look
    GLUserInterface1.MouseLookActivate;
  end;
end;

procedure TForm1.glsViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // deactivate the mouse look and restore the old mouse pos
  if Button = TMouseButton.mbleft then
  begin
    GLUserInterface1.MouseLookDeActivate;
    SetCursorPos(OldPt.X, OldPt.Y);
  end;
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
begin
  // zoom in
  if Camera.Position.Z < -2 then
    Camera.Position.Z := Camera.Position.Z + 1;
end;

procedure TForm1.SpeedButton8Click(Sender: TObject);
begin
  // zoom out
  if Camera.Position.Z > -16 then
    Camera.Position.Z := Camera.Position.Z - 1;
end;

procedure TForm1.BuildCube(X, Y, Z, W, Fh, Bh, D, Yoffs, Rot: Single;
  Front, Back, Top, Bottom, Left, Right, AddExtraVertex: Boolean; AMaterial: Word);
var
  FaceGroup: TFGVertexIndexList;
  i: Integer;
  vect: TVector3f;
  Obj: TMeshObject;
  n: Integer; // number of vertices added
begin
  // scale the dimensions with the freeform scale
  W := (W / GLFreeForm1.Scale.X) / 2;
  // remove all the {} in this proc to centre the cube vertically
  Fh := (Fh / GLFreeForm1.Scale.Y) { / 2 };
  // remove all the {} in this proc to centre the cube vertically
  Bh := (Bh / GLFreeForm1.Scale.Y) { / 2 };
  D := (D / GLFreeForm1.Scale.Z) / 2;

  X := (X / GLFreeForm1.Scale.X);
  Y := (Y / GLFreeForm1.Scale.Y) - (Yoffs / GLFreeForm1.Scale.Y);
  Z := (Z / GLFreeForm1.Scale.Z);

  // create a mesh object to work with
  Obj := TMeshObject.CreateOwned(GLFreeForm1.MeshObjects);
  // work in facegroup mode
  Obj.Mode := momFaceGroups;

  n := 0;
  if Front then
  begin
    // front
    with Obj.Vertices do
    begin
      // one for each corner of the quad
      Add(-W, 0 { -fh } , -D);
      Add(-W, Fh, -D);
      Add(W, Fh, -D);
      Add(W, 0 { -fh } , -D);
    end;
    // set the tex coords to cover the whole quad
    with Obj.TexCoords do
    begin
      // bottom left, bottom right, top right, top left
      Add(1, 0);
      Add(1, 1);
      Add(0, 1);
      Add(0, 0);
    end;

    // add a new facegroup
    FaceGroup := TFGVertexIndexList.CreateOwned(Obj.FaceGroups);

    // and set the material
    FaceGroup.MaterialName := 'DefaultFront';
    // triangle 1
    // vertex 0, vertex 1, vertex 2
    FaceGroup.Add(0);
    FaceGroup.Add(1);
    FaceGroup.Add(2);
    // triangle 2
    // vertex 2, vertex 3, vertex 0
    FaceGroup.Add(2);
    FaceGroup.Add(3);
    FaceGroup.Add(0);
    // 4 vertices added so far
    Inc(n, 4);
  end;

  // all the other parts are added in exactly the same way.
  // I had to experiment until the faces appeared how I wanted them.
  // The only way I could change the normals was by changing the order I added
  // to the facegroup
  if Back then
  begin
    // back
    with Obj.Vertices do
    begin
      Add(W, 0 { - bh } , D);
      Add(W, Bh, D);
      Add(-W, Bh, D);
      Add(-W, 0 { - bh } , D);
    end;
    with Obj.TexCoords do
    begin
      Add(1, 0);
      Add(1, 1);
      Add(0, 1);
      Add(0, 0);
    end;
    FaceGroup := TFGVertexIndexList.CreateOwned(Obj.FaceGroups);
    FaceGroup.MaterialName := 'DefaultBack';
    FaceGroup.Add(n);
    FaceGroup.Add(1 + n);
    FaceGroup.Add(2 + n);
    FaceGroup.Add(2 + n);
    FaceGroup.Add(3 + n);
    FaceGroup.Add(n);
    Inc(n, 4);
  end;

  if Top then
  begin
    // top
    with Obj.Vertices do
    begin
      Add(-W, Bh, D);
      Add(W, Bh, D);
      Add(W, Fh, -D);
      Add(-W, Fh, -D);
    end;
    with Obj.TexCoords do
    begin
      Add(1, 1);
      Add(0, 1);
      Add(0, 0);
      Add(1, 0);
    end;
    FaceGroup := TFGVertexIndexList.CreateOwned(Obj.FaceGroups);
    FaceGroup.MaterialName := 'DefaultTop';
    FaceGroup.Add(n);
    FaceGroup.Add(1 + n);
    FaceGroup.Add(2 + n);
    FaceGroup.Add(2 + n);
    FaceGroup.Add(3 + n);
    FaceGroup.Add(n);
    Inc(n, 4);
  end;

  if Bottom then
  begin
    // bottom
    with Obj.Vertices do
    begin
      Add(-W, 0 { - fh } , -D);
      Add(W, 0 { - fh } , -D);
      Add(W, 0 { - bh } , D);
      Add(-W, 0 { - bh } , D);
    end;
    with Obj.TexCoords do
    begin
      Add(1, 1);
      Add(0, 1);
      Add(0, 0);
      Add(1, 0);
    end;
    FaceGroup := TFGVertexIndexList.CreateOwned(Obj.FaceGroups);
    FaceGroup.MaterialName := 'DefaultBottom';
    FaceGroup.Add(n);
    FaceGroup.Add(1 + n);
    FaceGroup.Add(2 + n);
    FaceGroup.Add(2 + n);
    FaceGroup.Add(3 + n);
    FaceGroup.Add(n);
    Inc(n, 4);
  end;

  if Left then
  begin
    // left
    with Obj.Vertices do
    begin
      Add(W, Bh, D);
      Add(W, 0 { - bh } , D);
      Add(W, 0 { - fh } , -D);
      Add(W, Fh, -D);
    end;
    with Obj.TexCoords do
    begin
      Add(0, 1);
      Add(0, 0);
      Add(1, 0);
      Add(1, 1);
    end;
    FaceGroup := TFGVertexIndexList.CreateOwned(Obj.FaceGroups);
    FaceGroup.MaterialName := 'DefaultLeft';
    FaceGroup.Add(n);
    FaceGroup.Add(1 + n);
    FaceGroup.Add(2 + n);
    FaceGroup.Add(2 + n);
    FaceGroup.Add(3 + n);
    FaceGroup.Add(n);
    Inc(n, 4);
  end;

  if Right then
  begin
    // right
    with Obj.Vertices do
    begin
      Add(-W, Bh, D);
      Add(-W, Fh, -D);
      Add(-W, 0 { - fh } , -D);
      Add(-W, 0 { - bh } , D);
    end;
    with Obj.TexCoords do
    begin
      Add(1, 1);
      Add(0, 1);
      Add(0, 0);
      Add(1, 0);
    end;
    FaceGroup := TFGVertexIndexList.CreateOwned(Obj.FaceGroups);
    FaceGroup.MaterialName := 'DefaultRight';
    FaceGroup.Add(n);
    FaceGroup.Add(1 + n);
    FaceGroup.Add(2 + n);
    FaceGroup.Add(2 + n);
    FaceGroup.Add(3 + n);
    FaceGroup.Add(n);
  end;

  FaceGroup.Mode := fgmmTriangles;
  // I dunno if this is the correct way to rotate vertices, but it works for this purpose
  if Rot <> 0 then
    for i := 0 to GLFreeForm1.MeshObjects[GLFreeForm1.MeshObjects.count - 1]
      .Vertices.count - 1 do
    begin
      vect := GLFreeForm1.MeshObjects[GLFreeForm1.MeshObjects.count - 1].Vertices[i];
      RotateVectorAroundY(vect, DegToRadian(Rot));
      GLFreeForm1.MeshObjects[GLFreeForm1.MeshObjects.count - 1].Vertices[i] := vect;
    end;
  // move the object into position
  MakeVector(vect, X, Y, Z);
  GLFreeForm1.MeshObjects[GLFreeForm1.MeshObjects.count - 1].Translate(vect);

  // add the dummy vertex if requested
  if AddExtraVertex then
    Obj.Vertices.Add(X, Y + Yoffs, Z);

///  Obj.Colors
  GLFreeForm1.Material.FrontProperties.Emission.color := clrYellow;
  // update
  GLFreeForm1.StructureChanged;
end;

procedure TForm1.glsViewerClick(Sender: TObject);
begin
  // deactivate the mouse look when the mouse button is released
  GLUserInterface1.MouseLookDeActivate;
end;


procedure TForm1.rRBClick(Sender: TObject);
begin
  // get the rotation from the radio button's caption (0,90,180 or 270)
  RotAngle := round(s2f(TRadiobutton(Sender).Caption));
  Edit1.SetFocus;
end;

procedure TForm1.PartsCBClick(Sender: TObject);
var
  count: Integer;
begin
  count := 0;
  if cbFront.checked then
    Inc(count);
  if cbBack.checked then
    Inc(count);
  if cbTop.checked then
    Inc(count);
  if cbBottom.checked then
    Inc(count);
  if cbLeft.checked then
    Inc(count);
  if cbRight.checked then
    Inc(count);
  // if only one part is selected add a dummy vertex, so the cube can be detected by the deleteobject procedure
  AddExtraVertex := count < 2;
  // we need at least 1 part in the cube :)
  if count < 1 then
    TCheckBox(Sender).checked := True;
  Edit1.SetFocus;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // set focus on the hidden edit box
  Edit1.SetFocus;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  // ignore any keypresses in the edit box
  Key := #0;
end;

procedure TForm1.btAddCubesClick(Sender: TObject);
var
  i, j, k: Integer;
const
  NX: Word = 9; // horizontal
  NY: Word = 4; // vertical
  NZ: Word = 9; // depth
begin
  // add cubes
  for i := 0 to NX do
    for j := 0 to NY do
      for k := 0 to NZ do
        BuildCube(i + dcBlock.Position.X, j + dcBlock.Position.Y,
          k + dcBlock.Position.Z, s2f(WidthEdit.text),
          s2f(FrontHeightedit.text), s2f(BackHeightEdit.text),
          s2f(DepthEdit.text), 0.5, RotAngle, cbFront.checked, cbBack.checked,
          cbTop.checked, cbBottom.checked, cbLeft.checked, cbRight.checked,
          AddExtraVertex,
          Random(Material));
  Edit1.SetFocus;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  // these edit boxes should be replaced by spin edits.
  // I was using jvSpinEdit, but left it out for compatibility sake
  case TSpeedButton(Sender).tag of
    0: if s2f(WidthEdit.text) < 1.01 then
        WidthEdit.text := Format('%.2f', [s2f(WidthEdit.text) + 0.01]);
    1: if s2f(FrontHeightedit.text) < 1.01 then
        FrontHeightedit.text :=
          Format('%.2f', [s2f(FrontHeightedit.text) + 0.01]);
    2: if s2f(BackHeightEdit.text) < 1.01 then
        BackHeightEdit.text :=
          Format('%.2f', [s2f(BackHeightEdit.text) + 0.01]);
    3: if s2f(DepthEdit.text) < 1.01 then
        DepthEdit.text := Format('%.2f', [s2f(DepthEdit.text) + 0.01]);
  end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  case TSpeedButton(Sender).tag of
    0: if s2f(WidthEdit.text) > 0.01 then
        WidthEdit.text := Format('%.2f', [s2f(WidthEdit.text) - 0.01]);
    1: if s2f(FrontHeightedit.text) > 0.01 then
        FrontHeightedit.text :=
          Format('%.2f', [s2f(FrontHeightedit.text) - 0.01]);
    2: if s2f(BackHeightEdit.text) > 0.01 then
        BackHeightEdit.text :=
          Format('%.2f', [s2f(BackHeightEdit.text) - 0.01]);
    3: if s2f(DepthEdit.text) > 0.01 then
        DepthEdit.text := Format('%.2f', [s2f(DepthEdit.text) - 0.01]);
  end;
end;

end.
