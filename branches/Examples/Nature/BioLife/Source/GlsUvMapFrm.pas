unit GlsUvMapFrm;

{ CapeRaven mesh demo.
  Changing mesh vertex data, normals and striping redundent data.
  Custom cube class declared for vertex point identification.
  On moving these vertex modifiers, the apointed vertex follows. }

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ExtDlgs,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Buttons,

  OpenGL1x,
  GLScene,
  GLState,
  GLColor,
  GLVectorFileObjects,
  GLWin32Viewer,
  GLVectorLists,
  GLVectorGeometry,
  GLTexture,
  GLObjects,
  GLFileOBJ,
  GLVectorTypes,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLPersistentClasses,
  GLMeshUtils,
  GLBaseClasses;

type
  TDrawingTool = (dtLine, dtRectangle, dtEllipse, dtRoundRect);

type
  TModifierCube = class(TGLCube)
  public
    FVectorIndex: Integer;
    FMeshObjIndex: Integer;
    constructor Create(AOwner: TComponent); override;
  end;

  TGLS3dUvForm = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLFreeForm1: TGLFreeForm;
    ScrollPanel: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Map1: TMenuItem;
    N1: TMenuItem;
    ApplyTexture1: TMenuItem;
    Open3ds1: TMenuItem;
    Save3ds1: TMenuItem;
    N2: TMenuItem;
    Savemeshasbmp1: TMenuItem;
    LoadTexture1: TMenuItem;
    Help1: TMenuItem;
    Help2: TMenuItem;
    About1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    N256x2561: TMenuItem;
    N512x5121: TMenuItem;
    StatusBar1: TStatusBar;
    N3: TMenuItem;
    Mesh1: TMenuItem;
    N4: TMenuItem;
    Exit1: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    ComboBox2: TComboBox;
    ComboBox1: TComboBox;
    ScrollBox1: TScrollBox;
    Image: TImage;
    PenEdit: TEdit;
    PenWidth: TUpDown;
    MeshSLABtn: TSpeedButton;
    BrushColorBtn: TSpeedButton;
    ScrollBox2: TScrollBox;
    Panel2: TPanel;
    btnGroups: TBitBtn;
    Button2: TButton;
    btnTextcoords: TBitBtn;
    btnVertex: TBitBtn;
    btnNormals: TBitBtn;
    Label1: TLabel;
    cbPolygonMode: TComboBox;
    chbViewPoints: TCheckBox;
    GroupBox1: TGroupBox;
    Bevel1: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    chbShowAxis: TCheckBox;
    Label6: TLabel;
    tbPos: TTrackBar;
    GroupBox2: TGroupBox;
    rbXY: TRadioButton;
    rbZY: TRadioButton;
    MeshDataListBox: TListBox;
    Splitter1: TSplitter;
    PaintPot1: TMenuItem;
    Pen1: TMenuItem;
    Brush1: TMenuItem;
    SolidPen: TMenuItem;
    DashPen: TMenuItem;
    DotPen: TMenuItem;
    DashDotPen: TMenuItem;
    DashDotDotPen: TMenuItem;
    ClearPen: TMenuItem;
    SolidBrush: TMenuItem;
    ClearBrush: TMenuItem;
    HorizontalBrush: TMenuItem;
    VerticalBrush: TMenuItem;
    FDiagonalBrush: TMenuItem;
    BDiagonalBrush: TMenuItem;
    CrossBrush: TMenuItem;
    DiagCrossBrush: TMenuItem;
    BrushStyle1: TMenuItem;
    Line1: TMenuItem;
    Rectangle1: TMenuItem;
    Ellipse1: TMenuItem;
    RoundRect1: TMenuItem;
    BrushColor: TSpeedButton;
    PenColor: TSpeedButton;
    SaveTexture1: TMenuItem;
    SaveAsTexture1: TMenuItem;
    N5: TMenuItem;
    N1024x10241: TMenuItem;
    N128x1281: TMenuItem;
    Clear1: TMenuItem;
    Image2: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    ColorDialog1: TColorDialog;
    dcModifiers: TGLDummyCube;
    GLMaterialLibrary1: TGLMaterialLibrary;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ApplyTexture1Click(Sender: TObject);
    procedure Open3ds1Click(Sender: TObject);
    procedure Save3ds1Click(Sender: TObject);
    procedure LoadTexture1Click(Sender: TObject);
    procedure Savemeshasbmp1Click(Sender: TObject);
    procedure Help2Click(Sender: TObject);
    procedure N256x2561Click(Sender: TObject);
    procedure N512x5121Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PenEditChange(Sender: TObject);
    procedure PenColorClick(Sender: TObject);
    procedure BrushColorClick(Sender: TObject);
    procedure SetBrushStyle(Sender: TObject);
    procedure SetPenStyle(Sender: TObject);
    procedure SaveStyles;
    procedure RestoreStyles;
    procedure Line1Click(Sender: TObject);
    procedure Rectangle1Click(Sender: TObject);
    procedure Ellipse1Click(Sender: TObject);
    procedure RoundRect1Click(Sender: TObject);
    procedure DrawShape(TopLeft, BottomRight: TPoint; AMode: TPenMode);
    procedure SaveTexture1Click(Sender: TObject);
    procedure SaveAsTexture1Click(Sender: TObject);
    procedure N128x1281Click(Sender: TObject);
    procedure N1024x10241Click(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);

    { Meshedit }
    procedure cbPolygonModeChange(Sender: TObject);
    procedure SetVertexModifiers;
    procedure chbViewPointsClick(Sender: TObject);
    procedure ShowModifierStatus(const aObj: TModifierCube);
    function MouseWorldPos(X, Y: Integer): TVector;
    procedure ChangeMeshVector(const aObj: TModifierCube; const aPos: TVector4f);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chbShowAxisClick(Sender: TObject);
    procedure tbPosChange(Sender: TObject);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
    procedure btnVertexClick(Sender: TObject);
    procedure btnNormalsClick(Sender: TObject);
    procedure btnTextcoordsClick(Sender: TObject);
    procedure btnGroupsClick(Sender: TObject);
    procedure StripAndRecalc;
  private
    { Meshedit }
    function GetPolygonMode: TPolygonMode;
    procedure SetPolygonMode(const Value: TPolygonMode);
    property PolygonMode: TPolygonMode read GetPolygonMode write SetPolygonMode;
  private

    { Meshedit }
    FOldX, FOldY: Integer;
    FModifierList: TObjectList;
    FSelectedModifier: TModifierCube;
    FMoveZ: Boolean;
    FOldMouseWorldPos: TVector;

    procedure ShowMeshData(const aList: TStringList);
  public

    { Painting }
    BrushStyle: TBrushStyle;
    PenStyle: TPenStyle;
    PenWide: Integer;
    MeshEditing, Drawing: Boolean;
    Origin, MovePt: TPoint;
    DrawingTool: TDrawingTool;
    CurrentFile: string;
    { Tex Edit }
    mx, my: Integer;
    procedure GetTexCoordsWireframe;
    procedure DisplayHint(Sender: TObject);
  end;

  TAxis = 0 .. 2;

var
  GLS3dUvForm: TGLS3dUvForm;
  Filename3DS: String;

procedure UVPlanarMapping(Vertices, TexCoords: TAffineVectorList;
  Axis: TAxis = 2);
procedure UVCubicMapping(Vertices, TexCoords: TAffineVectorList;
  Axis: TAxis = 2);
procedure UVCylindricalMapping(Vertices, TexCoords: TAffineVectorList;
  Axis: TAxis = 2);
procedure UVSphericalMapping(Vertices, TexCoords: TAffineVectorList;
  Axis: TAxis = 2);

implementation

uses
  Clipbrd,
  GlsUvAboutFrm;

{$R *.dfm}

const
  { Default combobox index for startup }
  CLinePolyMode = 1;
  { Scale dimention }
  CModifierDim = 0.04;

var
  { Modifier colors }
  CModColorNormal: TColorVector;
  CModColorSelect: TColorVector;

constructor TModifierCube.Create(AOwner: TComponent);
begin
  inherited;
  { Set the modifiers initial size and color }
  CubeWidth := CModifierDim;
  CubeHeight := CModifierDim;
  CubeDepth := CModifierDim;
  Material.FrontProperties.Diffuse.Color := CModColorNormal;
end;

procedure TGLS3dUvForm.FormCreate(Sender: TObject);
var
  s: single;
  Bitmap: TBitmap;
begin
  MeshEditing := False;
  Application.OnHint := DisplayHint;
  Bitmap := nil;
  try
    Bitmap := TBitmap.Create;
    Bitmap.Width := 256;
    Bitmap.Height := 256;
    Bitmap.PixelFormat := pf24bit;
    Image.Picture.Graphic := Bitmap;
  finally
    Bitmap.Free;
  end;
  Image.Canvas.FillRect(Rect(0, 0, Image.Width, Image.Height));
  { Assertion Error  GLVectorFileObjects line 4190 }
  { obj' }
  If (not FileExists(ExtractFilePath(ParamStr(0)) + 'cube.3ds')) then
  begin
    showmessage('cube.3ds file is missing');
    Application.Terminate;
  end;
  GLFreeForm1.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'cube.3ds');
  StripAndRecalc;
  SetVertexModifiers;
  { GLFreeForm1.LoadFromFile('teapot.3ds'); }
  GLFreeForm1.Material.Texture.Image.LoadFromFile('grid.bmp');
  GLFreeForm1.Material.Texture.Disabled := False;
  GLFreeForm1.Scale.SetVector(0.05, 0.05, 0.05);
  s := 4 / (GLFreeForm1.BoundingSphereRadius);
  GLFreeForm1.Scale.SetVector(s, s, s);
  GLFreeForm1.Pitch(90);
  ComboBox2.Itemindex := 0;
  ComboBox1.Itemindex := 0;
  ComboBox1Change(nil);
  FModifierList := TObjectList.Create;
  CModColorNormal := clrCoral;
  CModColorSelect := clrSkyBlue;
  cbPolygonMode.Itemindex := CLinePolyMode;
end;

procedure TGLS3dUvForm.DisplayHint(Sender: TObject);
begin { Both parts if not xx|yyy }
  StatusBar1.SimpleText := GetLongHint(Application.Hint);
end;

procedure TGLS3dUvForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TGLS3dUvForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  { }
end;

procedure TGLS3dUvForm.FormDestroy(Sender: TObject);
begin
  FModifierList.Clear;
  FreeAndNil(FModifierList);
end;

procedure UVPlanarMapping(Vertices, TexCoords: TAffineVectorList;
  Axis: TAxis = 2);
var
  i, j: Integer;
  P, center, min, max: TAffineVector;
  u, v: single;
begin
  u := 0;
  v := 0; { Compiler shutup }
  for i := 0 to Vertices.Count - 1 do
  begin
    P := Vertices.Items[i];
    for j := 0 to 2 do
    begin
      if i = 0 then
      begin
        min.V[j] := P.V[j];
        max.V[j] := P.V[j];
      end
      else
      begin
        if P.V[j] < min.V[j] then
          min.V[j] := P.V[j];
        if P.V[j] > max.V[j] then
          max.V[j] := P.V[j];
      end;
    end;
  end;
  center := VectorScale(VectorAdd(max, min), 0.5);
  Vertices.Translate(VectorNegate(center));
  min := VectorSubtract(min, center);
  max := VectorSubtract(max, center);

  // Generate texture coordinates
  TexCoords.Clear;
  for i := 0 to Vertices.Count do
  begin
    P := Vertices.Items[i];
    case Axis of
      0:
        begin
          u := (P.Y / (max.Y - min.Y));
          v := (-P.Z / (max.Z - min.Z));
        end;
      1:
        begin
          u := (P.X / (max.X - min.X));
          v := (-P.Z / (max.Z - min.Z));
        end;
      2:
        begin
          u := (P.X / (max.X - min.X));
          v := (-P.Y / (max.Y - min.Y));
        end;
    end;
    u := u + 0.5;
    v := v + 0.5;
    TexCoords.Add(u, v, 0);
  end;
end;

procedure UVCubicMapping(Vertices, TexCoords: TAffineVectorList;
  Axis: TAxis = 2);
begin
  // This one is a little more complicated...I'm working on it
end;

procedure UVCylindricalMapping(Vertices, TexCoords: TAffineVectorList;
  Axis: TAxis = 2);
var
  i, j: Integer;
  P, Pn, center, min, max: TAffineVector;
  { temp, } u, v: single;
begin
  u := 0;
  v := 0; { Compiler shutup }
  // Get the center
  for i := 0 to Vertices.Count - 1 do
  begin
    P := Vertices.Items[i];
    for j := 0 to 2 do
    begin
      if i = 0 then
      begin
        min.V[j] := P.V[j];
        max.V[j] := P.V[j];
      end
      else
      begin
        if P.V[j] < min.V[j] then
          min.V[j] := P.V[j];
        if P.V[j] > max.V[j] then
          max.V[j] := P.V[j];
      end;
    end;
  end;
  center := VectorScale(VectorAdd(max, min), 0.5);
  Vertices.Translate(VectorNegate(center));
  min := VectorSubtract(min, center);
  max := VectorSubtract(max, center);

  // Generate texture coordinates
  TexCoords.Clear;
  for i := 0 to Vertices.Count do
  begin
    P := Vertices.Items[i];
    Pn := VectorNormalize(P);
    case Axis of
      0:
        begin
          u := ArcTan2(Pn.Z, -Pn.Y) / 2;
          v := (P.X / (max.X - min.X));
        end;
      1:
        begin
          u := arctan2(-Pn.X, Pn.Z) / 2;
          v := (P.Y / (max.Y - min.Y));
        end;
      2:
        begin
          u := arctan2(-Pn.X, -Pn.Y) / 2;
          v := (P.Z / (max.Z - min.Z));
        end;
    end;
    u := 0.5 - u / Pi;
    u := u - floor(u);
    v := v + 0.5;
    TexCoords.Add(u, v, 0);
  end;
end;

procedure UVSphericalMapping(Vertices, TexCoords: TAffineVectorList;
  Axis: TAxis = 2);
var
  i, j: Integer;
  P, center, min, max: TAffineVector;
  { radius, }
  { temp, } u, v: single;
begin
  u := 0;
  v := 0; { Compiler shutup }
  // Get the center
  for i := 0 to Vertices.Count - 1 do
  begin
    P := Vertices.Items[i];
    for j := 0 to 2 do
    begin
      if i = 0 then
      begin
        min.V[j] := P.V[j];
        max.V[j] := P.V[j];
      end
      else
      begin
        if P.V[j] < min.V[j] then
          min.V[j] := P.V[j];
        if P.V[j] > max.V[j] then
          max.V[j] := P.V[j];
      end;
    end;
  end;
  center := VectorScale(VectorAdd(max, min), 0.5);
  Vertices.Translate(VectorNegate(center));

  // Generate texture coordinates
  TexCoords.Clear;
  for i := 0 to Vertices.Count do
  begin
    P := VectorNormalize(Vertices.Items[i]);
    case Axis of
      0:
        begin
          u := arctan2(P.Z, -P.Y);
          v := arctan(P.X / sqrt(P.Y * P.Y + P.Z * P.Z));
        end;
      1:
        begin
          u := arctan2(-P.X, P.Z);
          v := arctan(P.Y / sqrt(P.X * P.X + P.Z * P.Z));
        end;
      2:
        begin
          u := arctan2(-P.X, -P.Y);
          v := arctan(P.Z / sqrt(P.X * P.X + P.Y * P.Y));
        end;
    end;
    u := 1 - u / (2 * Pi);
    v := abs(0.5 - v / Pi);
    u := u - floor(u);
    TexCoords.Add(u, v, 0);
  end;
end;

procedure TGLS3dUvForm.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  lCurrentPos: TVector;
  lOldV: TVector3f;
  lDiff: TVector4f;
begin
  If MeshEditing then
  begin
    { If ctrl is not in use, move around freeform }
    if (ssLeft in Shift) and (not(ssCtrl in Shift)) then
    begin
      GLCamera1.MoveAroundTarget(FOldY - Y, FOldX - X);
      FOldX := X;
      FOldY := Y;
      Exit;
    end;

    { Move modifier and change relevant vertex data }
    if (ssLeft in Shift) then
    begin
      FMoveZ := rbZY.Checked;

      lCurrentPos := MouseWorldPos(X, Y);
      if Assigned(FSelectedModifier) and (VectorNorm(FOldMouseWorldPos) <> 0)
      then
      begin
        MakeVector(lOldV, FSelectedModifier.Position.X,
          FSelectedModifier.Position.Y, FSelectedModifier.Position.Z);
        lDiff := VectorSubtract(lCurrentPos, FOldMouseWorldPos);
        FSelectedModifier.Position.Translate(lDiff);
        ChangeMeshVector(FSelectedModifier, lDiff);
      end;
      FOldMouseWorldPos := lCurrentPos;
    end;
  end
  else
  begin
    if ssLeft in Shift then
      GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

procedure TGLS3dUvForm.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lObj: TGLBaseSceneObject;
begin
  If MeshEditing then
  begin
    FOldX := X;
    FOldY := Y;
    { If selecting a different modifier, change the last one's color back to default }
    if Assigned(FSelectedModifier) then
      FSelectedModifier.Material.FrontProperties.Diffuse.Color :=
        CModColorNormal;
    { Get selected objects }
    if not(ssCtrl in Shift) then
      Exit;
    { Check if selected object is a modifier.
      If so, change modifiers color as to indicated selected modifier. }
    lObj := GLSceneViewer1.Buffer.GetPickedObject(X, Y);
    if (lObj is TModifierCube) then
    begin
      FSelectedModifier := TModifierCube(lObj);
      FSelectedModifier.Material.FrontProperties.Diffuse.Color :=
        CModColorSelect;
      FSelectedModifier.NotifyChange(FSelectedModifier);
      ShowModifierStatus(TModifierCube(lObj));

      FMoveZ := rbZY.Checked;
      FOldMouseWorldPos := MouseWorldPos(X, Y);
    end;
  end
  else
  begin
    mx := X;
    my := Y;
  end;
end;

procedure TGLS3dUvForm.GLSceneViewer1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If MeshEditing then
  begin
    if Assigned(FSelectedModifier) then
    begin
      FSelectedModifier.Material.FrontProperties.Diffuse.Color :=
        CModColorNormal;
      FSelectedModifier := nil;
      { Recalculate structure and redraw freeform }
      StripAndRecalc;
      { Reset vertex modifiers and their data. }
      SetVertexModifiers;
    end;
  end;
end;

procedure TGLS3dUvForm.GetTexCoordsWireframe;
var
  i, j, X, Y, x0, y0: Integer;
  fg: TFGVertexIndexList;
begin
  x0 := 0;
  y0 := 0; { Compiler shut up }
  GLS3dUvForm.Image.Canvas.FillRect(Rect(0, 0, GLS3dUvForm.Image.Width,
    GLS3dUvForm.Image.Height));
  GLS3dUvForm.Image.Canvas.Pen.Color := clBlack;
  with GLS3dUvForm.GLFreeForm1.MeshObjects.Items[0] do
  begin
    for i := 0 to FaceGroups.Count - 1 do
    begin
      fg := TFGVertexIndexList(FaceGroups.Items[i]);
      for j := 0 to fg.VertexIndices.Count - 1 do
      begin
        if j = 0 then
        begin
          x0 := round(TexCoords.Items[fg.VertexIndices.Items[j]].X *
            GLS3dUvForm.Image.Width);
          y0 := round(TexCoords.Items[fg.VertexIndices.Items[j]].Y *
            GLS3dUvForm.Image.Height);
          Image.Canvas.MoveTo(x0, y0);
        end
        else
        begin
          X := round(TexCoords.Items[fg.VertexIndices.Items[j]].X *
            GLS3dUvForm.Image.Width);
          Y := round(TexCoords.Items[fg.VertexIndices.Items[j]].Y *
            GLS3dUvForm.Image.Height);
          Image.Canvas.LineTo(X, Y);
        end;
      end;
      Image.Canvas.LineTo(x0, y0);
    end;
  end;
end;

procedure TGLS3dUvForm.ApplyTexture1Click(Sender: TObject);
begin
  ApplyTexture1.Checked := (not ApplyTexture1.Checked);
  If ApplyTexture1.Checked then
    GLFreeForm1.Material.Texture.Disabled := False
  else
    GLFreeForm1.Material.Texture.Disabled := True;
end;

procedure TGLS3dUvForm.Open3ds1Click(Sender: TObject);
var
  s: single;
begin
  If OpenDialog1.Execute then
  begin
    Image.Canvas.FillRect(Rect(0, 0, Image.Width, Image.Height));
    GLFreeForm1.LoadFromFile(OpenDialog1.Filename { 'teapot.3ds' } );
    Filename3DS := OpenDialog1.Filename;
    { Need a Autoscale function here
      GLFreeForm1.Scale.SetVector(0.05,0.05,0.05); }
    s := 4 / (GLFreeForm1.BoundingSphereRadius);
    GLFreeForm1.Scale.SetVector(s, s, s);
    ComboBox1Change(nil);
    GLFreeForm1.Pitch(90);
    StripAndRecalc;
    SetVertexModifiers;
  end;
end;

procedure TGLS3dUvForm.Save3ds1Click(Sender: TObject);
begin
  { Save as WHATEVER GLSCENE USES that has UV Coords... }
  { SaveDialog1.Filter:='*.obj'; }
  SaveDialog1.Filename := ChangeFileExt(OpenDialog1.Filename, '.obj');
  If SaveDialog1.Execute then
    GLFreeForm1.SaveToFile(SaveDialog1.Filename);
end;

procedure TGLS3dUvForm.LoadTexture1Click(Sender: TObject);
begin
  If OpenPictureDialog1.Execute then
  begin
    GLFreeForm1.Material.Texture.Image.LoadFromFile
      (OpenPictureDialog1.Filename);
    GLFreeForm1.Material.Texture.Disabled := False;
    CurrentFile := OpenPictureDialog1.Filename;
    SaveStyles;
    Image.Picture.LoadFromFile(CurrentFile);
    Image.Picture.Bitmap.PixelFormat := pf24bit;
    RestoreStyles;
  end;
end;

procedure TGLS3dUvForm.SaveTexture1Click(Sender: TObject);
begin
  if CurrentFile <> EmptyStr then
    Image.Picture.SaveToFile(CurrentFile)
  else
    SaveAsTexture1Click(Sender);
end;

procedure TGLS3dUvForm.SaveAsTexture1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    CurrentFile := SaveDialog1.Filename;
    SaveTexture1Click(Sender);
  end;
end;

procedure TGLS3dUvForm.Clear1Click(Sender: TObject);
begin
  Image.Canvas.FillRect(Rect(0, 0, Image.Width, Image.Height));
  GetTexCoordsWireframe;
end;

procedure TGLS3dUvForm.N128x1281Click(Sender: TObject);
begin
  Image.Picture.Bitmap.Width := 128;
  Image.Picture.Bitmap.Height := 128;
  Image.Canvas.FillRect(Rect(0, 0, Image.Width, Image.Height));
  GetTexCoordsWireframe;
end;

procedure TGLS3dUvForm.N256x2561Click(Sender: TObject);
begin
  Image.Picture.Bitmap.Width := 256;
  Image.Picture.Bitmap.Height := 256;
  Image.Canvas.FillRect(Rect(0, 0, Image.Width, Image.Height));
  GetTexCoordsWireframe;
end;

procedure TGLS3dUvForm.N512x5121Click(Sender: TObject);
begin
  Image.Picture.Bitmap.Width := 512;
  Image.Picture.Bitmap.Height := 512;
  Image.Canvas.FillRect(Rect(0, 0, Image.Width, Image.Height));
  GetTexCoordsWireframe;
end;

procedure TGLS3dUvForm.N1024x10241Click(Sender: TObject);
begin
  Image.Picture.Bitmap.Width := 1024;
  Image.Picture.Bitmap.Height := 1024;
  Image.Canvas.FillRect(Rect(0, 0, Image.Width, Image.Height));
  GetTexCoordsWireframe;
end;

procedure TGLS3dUvForm.Savemeshasbmp1Click(Sender: TObject);
begin { ? 32 bit for transparency ? or leave it to them? }
  Image.Picture.Bitmap.PixelFormat := pf24bit;
  Image.Picture.Bitmap.SaveToFile(ChangeFileExt(Filename3DS, '.bmp'))
end;

procedure TGLS3dUvForm.Help2Click(Sender: TObject);
begin
  showmessage('Under Construction');
end;

procedure TGLS3dUvForm.ComboBox1Change(Sender: TObject);
begin
  with GLFreeForm1.MeshObjects.Items[0] do
  begin
    case ComboBox2.Itemindex of
      0:
        UVPlanarMapping(Vertices, TexCoords, ComboBox1.Itemindex);
      1:
        UVCubicMapping(Vertices, TexCoords, ComboBox1.Itemindex);
      2:
        UVCylindricalMapping(Vertices, TexCoords, ComboBox1.Itemindex);
      3:
        UVSphericalMapping(Vertices, TexCoords, ComboBox1.Itemindex);
    end;
  end;
  GetTexCoordsWireframe;
  GLFreeForm1.StructureChanged;
end;

procedure TGLS3dUvForm.ComboBox2Change(Sender: TObject);
begin
  ComboBox1Change(nil);
end;

procedure TGLS3dUvForm.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    GLFreeForm1.NormalsOrientation := mnoInvert
  else
    GLFreeForm1.NormalsOrientation := mnoDefault;
end;

procedure TGLS3dUvForm.About1Click(Sender: TObject);
begin
  AboutBox.ShowModal;
end;
(* ******************************************** *)
(* ******************************************** *)

(* ******************************************** *)
(* ******************************************** *)
procedure TGLS3dUvForm.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Drawing := True;
  Image.Canvas.MoveTo(X, Y);
  Origin := Point(X, Y);
  MovePt := Origin;
  StatusBar1.Panels[0].Text := Format('Origin: (%d, %d)', [X, Y]);
end;

procedure TGLS3dUvForm.ImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Drawing then
  begin
    DrawShape(Origin, MovePt, pmNotXor);
    MovePt := Point(X, Y);
    DrawShape(Origin, MovePt, pmNotXor);
  end;
  StatusBar1.Panels[1].Text := Format('Current: (%d, %d)', [X, Y]);
end;

procedure TGLS3dUvForm.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Drawing then
  begin
    DrawShape(Origin, Point(X, Y), pmCopy);
    Drawing := False;
  end;
end;

(* ******************************************** *)
(* ******************************************** *)

procedure TGLS3dUvForm.PenEditChange(Sender: TObject);
begin
  Image.Canvas.Pen.Width := PenWidth.Position;
end;

procedure TGLS3dUvForm.PenColorClick(Sender: TObject);
begin
  ColorDialog1.Color := Image.Canvas.Pen.Color;
  if ColorDialog1.Execute then
    Image.Canvas.Pen.Color := ColorDialog1.Color;
end;

procedure TGLS3dUvForm.BrushColorClick(Sender: TObject);
begin
  ColorDialog1.Color := Image.Canvas.Brush.Color;
  if ColorDialog1.Execute then
    Image.Canvas.Brush.Color := ColorDialog1.Color;
end;

procedure TGLS3dUvForm.SetPenStyle(Sender: TObject);
begin
  with Image.Canvas.Pen do
  begin
    if Sender = SolidPen then
      Style := psSolid
    else if Sender = DashPen then
      Style := psDash
    else if Sender = DotPen then
      Style := psDot
    else if Sender = DashDotPen then
      Style := psDashDot
    else if Sender = DashDotDotPen then
      Style := psDashDotDot
    else if Sender = ClearPen then
      Style := psClear;
  end;
end;

procedure TGLS3dUvForm.SetBrushStyle(Sender: TObject);
begin
  with Image.Canvas.Brush do
  begin
    if Sender = SolidBrush then
      Style := bsSolid
    else if Sender = ClearBrush then
      Style := bsClear
    else if Sender = HorizontalBrush then
      Style := bsHorizontal
    else if Sender = VerticalBrush then
      Style := bsVertical
    else if Sender = FDiagonalBrush then
      Style := bsFDiagonal
    else if Sender = BDiagonalBrush then
      Style := bsBDiagonal
    else if Sender = CrossBrush then
      Style := bsCross
    else if Sender = DiagCrossBrush then
      Style := bsDiagCross;
  end;
end;

procedure TGLS3dUvForm.SaveStyles;
begin
  with Image.Canvas do
  begin
    BrushStyle := Brush.Style;
    PenStyle := Pen.Style;
    PenWide := Pen.Width;
  end;
end;

procedure TGLS3dUvForm.RestoreStyles;
begin
  with Image.Canvas do
  begin
    Brush.Style := BrushStyle;
    Pen.Style := PenStyle;
    Pen.Width := PenWide;
  end;
end;

procedure TGLS3dUvForm.Line1Click(Sender: TObject);
begin
  DrawingTool := dtLine;
end;

procedure TGLS3dUvForm.Rectangle1Click(Sender: TObject);
begin
  DrawingTool := dtRectangle;
end;

procedure TGLS3dUvForm.Ellipse1Click(Sender: TObject);
begin
  DrawingTool := dtEllipse;
end;

procedure TGLS3dUvForm.RoundRect1Click(Sender: TObject);
begin
  DrawingTool := dtRoundRect;
end;

procedure TGLS3dUvForm.DrawShape(TopLeft, BottomRight: TPoint; AMode: TPenMode);
begin
  with Image.Canvas do
  begin
    Pen.Mode := AMode;
    case DrawingTool of
      dtLine:
        begin
          Image.Canvas.MoveTo(TopLeft.X, TopLeft.Y);
          Image.Canvas.LineTo(BottomRight.X, BottomRight.Y);
        end;
      dtRectangle:
        Image.Canvas.Rectangle(TopLeft.X, TopLeft.Y, BottomRight.X,
          BottomRight.Y);
      dtEllipse:
        Image.Canvas.Ellipse(TopLeft.X, TopLeft.Y, BottomRight.X,
          BottomRight.Y);
      dtRoundRect:
        Image.Canvas.RoundRect(TopLeft.X, TopLeft.Y, BottomRight.X,
          BottomRight.Y, (TopLeft.X - BottomRight.X) div 2,
          (TopLeft.Y - BottomRight.Y) div 2);
    end;
  end;
end;

procedure TGLS3dUvForm.Cut1Click(Sender: TObject);
var
  ARect: TRect;
begin
  Copy1Click(Sender);
  with Image.Canvas do
  begin
    CopyMode := cmWhiteness;
    ARect := Rect(0, 0, Image.Width, Image.Height);
    CopyRect(ARect, Image.Canvas, ARect);
    CopyMode := cmSrcCopy;
  end;
end;

procedure TGLS3dUvForm.Copy1Click(Sender: TObject);
begin
  Clipboard.Assign(Image.Picture);
end;

procedure TGLS3dUvForm.Paste1Click(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Assign(Clipboard);
      Image.Canvas.Draw(0, 0, Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;
(* ******************************************** *)
(* ******************************************** *)

(* ******************************************** *)
(* ******************************************** *)
function TGLS3dUvForm.GetPolygonMode: TPolygonMode;
begin
  Result := GLS3dUvForm.GLFreeForm1.Material.PolygonMode;
end;

procedure TGLS3dUvForm.SetPolygonMode(const Value: TPolygonMode);
begin
  GLS3dUvForm.GLFreeForm1.Material.PolygonMode := Value;
end;

procedure TGLS3dUvForm.cbPolygonModeChange(Sender: TObject);
begin
  PolygonMode := TPolygonMode(cbPolygonMode.Itemindex);
end;

procedure TGLS3dUvForm.SetVertexModifiers;
  procedure ScaleVector(var V1, V2: TVector3f);
  begin
    V1.X := V1.X * V2.X;
    V1.Y := V1.Y * V2.Y;
    V1.Z := V1.Z * V2.Z;
  end;

var
  i, j: Integer;
  lVector, lScale: TVector3f;
  lModifier: TModifierCube;
begin
  FModifierList.Clear;
  GLScene1.BeginUpdate;
  try
    with GLFreeForm1.MeshObjects do
    begin
      for i := 0 to Count - 1 do
        for j := 0 to Items[i].Vertices.Count - 1 do
        begin
          lVector := Items[i].Vertices.Items[j];
          lModifier := TModifierCube.Create(nil);
          lModifier.FVectorIndex := j;
          lModifier.FMeshObjIndex := i;

          FModifierList.Add(lModifier);
          GLScene1.Objects.AddChild(lModifier);

          lScale := GLFreeForm1.Scale.AsAffineVector;
          ScaleVector(lVector, lScale);
          lModifier.Position.Translate(lVector);
        end;
    end;
  finally
    GLScene1.EndUpdate;
  end;
end;

procedure TGLS3dUvForm.chbViewPointsClick(Sender: TObject);
var
  i: Integer;
begin
  GLScene1.BeginUpdate;
  try
    for i := 0 to FModifierList.Count - 1 do
      TModifierCube(FModifierList.Items[i]).Visible := chbViewPoints.Checked;
  finally
    GLScene1.EndUpdate;
  end;
end;

procedure TGLS3dUvForm.ShowModifierStatus(const aObj: TModifierCube);
begin
  if aObj = nil then
    StatusBar1.Panels[0].Text := ''
  else
    StatusBar1.Panels[0].Text := Format('Modifier vector index [%d]',
      [aObj.FVectorIndex]);
end;

function TGLS3dUvForm.MouseWorldPos(X, Y: Integer): TVector;
var
  v: TVector;
begin
  Y := GLSceneViewer1.Height - Y;

  if Assigned(FSelectedModifier) then
  begin
    SetVector(v, X, Y, 0);
    if FMoveZ then
      GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlaneXZ(v,
        FSelectedModifier.Position.Y, Result)
    else
      GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlaneXY(v,
        FSelectedModifier.Position.Z, Result);
  end
  else
    SetVector(Result, NullVector);
end;

procedure TGLS3dUvForm.ChangeMeshVector(const aObj: TModifierCube;
  const aPos: TVector4f);
var
  lVIndex, lMIndex: Integer;
  v: TVector3f;
begin
  if aObj = nil then
    Exit;

  lVIndex := aObj.FVectorIndex;
  lMIndex := aObj.FMeshObjIndex;

  { Get new vertex position, keep freeform scale in mind and redraw freeform. }
  MakeVector(v, aPos.X / CModifierDim, aPos.Y / CModifierDim, aPos.Z / CModifierDim);
  GLFreeForm1.MeshObjects.Items[lMIndex].Vertices.TranslateItem(lVIndex, v);
  GLFreeForm1.StructureChanged;
end;

procedure TGLS3dUvForm.chbShowAxisClick(Sender: TObject);
begin
  dcModifiers.ShowAxes := TCheckBox(Sender).Checked;
end;

procedure TGLS3dUvForm.tbPosChange(Sender: TObject);
begin
  GLCamera1.Position.Z := tbPos.Position;
end;

procedure TGLS3dUvForm.GLSceneViewer1BeforeRender(Sender: TObject);
begin
  glEnable(GL_NORMALIZE);
end;

procedure TGLS3dUvForm.btnVertexClick(Sender: TObject);
var
  i, j: Integer;
  lList: TStringList;
  lVector: TVector3f;
begin
  lList := TStringList.Create;
  try
    with GLFreeForm1.MeshObjects do
      for i := 0 to Count - 1 do
      begin
        lList.Add('For mesh object ' + IntToStr(i));
        for j := 0 to Items[i].Vertices.Count - 1 do
        begin
          lVector := Items[i].Vertices.Items[j];
          lList.Add(Format('%f %f %f', [lVector.X, lVector.Y, lVector.Z]));
        end;
      end;
    ShowMeshData(lList);
  finally
    FreeAndNil(lList);
  end;
end;

procedure TGLS3dUvForm.btnNormalsClick(Sender: TObject);
var
  i, j: Integer;
  lList: TStringList;
  lVector: TVector3f;
begin
  lList := TStringList.Create;
  try
    with GLFreeForm1.MeshObjects do
      for i := 0 to Count - 1 do
      begin
        lList.Add('For mesh object ' + IntToStr(i));
        for j := 0 to Items[i].Normals.Count - 1 do
        begin
          lVector := Items[i].Normals.Items[j];
          lList.Add(Format('%f %f %f', [lVector.X, lVector.Y, lVector.Z]));
        end;
      end;
    ShowMeshData(lList);
  finally
    FreeAndNil(lList);
  end;
end;

procedure TGLS3dUvForm.btnTextcoordsClick(Sender: TObject);
var
  i, j: Integer;
  lList: TStringList;
  lVector: TVector3f;
begin
  lList := TStringList.Create;
  try
    with GLFreeForm1.MeshObjects do
      for i := 0 to Count - 1 do
      begin
        lList.Add('For mesh object ' + IntToStr(i));
        for j := 0 to Items[i].TexCoords.Count - 1 do
        begin
          lVector := Items[i].TexCoords.Items[j];
          lList.Add(Format('%f %f %f', [lVector.X, lVector.Y, lVector.Z]));
        end;
      end;
    ShowMeshData(lList);
  finally
    FreeAndNil(lList);
  end;
end;

procedure TGLS3dUvForm.btnGroupsClick(Sender: TObject);
var
  i: Integer;
  lList: TStringList;
begin
  lList := TStringList.Create;
  try
    with GLFreeForm1.MeshObjects do
      for i := 0 to Count - 1 do
      begin
        lList.Add('For mesh object ' + IntToStr(i));
        lList.Add(IntToStr(Items[i].TriangleCount));
      end;
    ShowMeshData(lList);
  finally
    FreeAndNil(lList);
  end;
end;

procedure TGLS3dUvForm.StripAndRecalc;
var
  lTrigList, lNormals: TAffineVectorList;
  lIndices: TIntegerList;
  lObj: TMeshObject;
  lStrips: TPersistentObjectList;

  lFaceGroup: TFGVertexIndexList;
  i: Integer;
begin
  // Extract raw triangle data to work with.
  lTrigList := GLFreeForm1.MeshObjects.ExtractTriangles;

  // Builds a vector-count optimized indices list.
  lIndices := BuildVectorCountOptimizedIndices(lTrigList);
  // Alter reference/indice pair and removes unused reference values.
  RemapAndCleanupReferences(lTrigList, lIndices);
  // Calculate normals.
  lNormals := BuildNormals(lTrigList, lIndices);

  // Strip where posible.
  lStrips := StripifyMesh(lIndices, lTrigList.Count, True);

  // Clear current mesh object data.
  GLFreeForm1.MeshObjects.Clear;

  // Setup new mesh object.
  lObj := TMeshObject.CreateOwned(GLFreeForm1.MeshObjects);
  lObj.Vertices := lTrigList;
  lObj.Mode := momFaceGroups;
  lObj.Normals := lNormals;

  for i := 0 to lStrips.Count - 1 do
  begin
    lFaceGroup := TFGVertexIndexList.CreateOwned(lObj.FaceGroups);
    lFaceGroup.VertexIndices := (lStrips[i] as TIntegerList);
    if i > 0 then
      lFaceGroup.Mode := fgmmTriangleStrip
    else
      lFaceGroup.Mode := fgmmTriangles;
    lFaceGroup.MaterialName := IntToStr(i and 15);
  end;
  // Redraw freeform
  GLFreeForm1.StructureChanged;

  lTrigList.Free;
  lNormals.Free;
  lIndices.Free;
end;

procedure TGLS3dUvForm.ShowMeshData(const aList: TStringList);
Begin
  MeshDataListBox.{ Lines. } Assign(aList);
End;

end.
