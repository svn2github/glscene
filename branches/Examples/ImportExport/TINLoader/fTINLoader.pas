unit fTINLoader;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.Jpeg,

  GLScene,
  GLMesh,
  GLCadencer,
  GLTexture,
  GLWin32Viewer,
  GLObjects,
  GLVectorGeometry,
  GLVectorFileObjects,
  GLPersistentClasses,
  GLVectorLists,
  GLKeyboard,
  GLRenderContextInfo,
  GLContext,
  GLState,
  GLColor,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TMainForm = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    World: TGLDummyCube;
    DGLContourLines: TGLDirectOpenGL;
    DummyTerrain: TGLDummyCube;
    MapImage: TImage;

    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure DGLContourLinesRender(var rci: TGLRenderContextInfo);

    procedure LoadVerticesFromFile(filename: String);
    procedure AddTriangle(const p1, p2, p3: TAffineVector;
      const color: TColorVector);
    function WordPosition(const N: Integer; const S: string;
      const WordDelims: TSysCharSet): Integer;
   { This function lifted straight out of RXlib }
    function ExtractWord(N: Integer; const S: string;
      const WordDelims: TSysCharSet): string;
    procedure BuildTerrainMesh(MeshFileName: String; MapFileName: String);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    mx, my, NumVertexY, NumGroup: Integer;
    Xmin, Xmax, Ymin, Ymax, Zmin, Zmax: Single;
    ID: String;
    nVertice: Integer;
    ptVertex: array of array [0 .. 2] of Single; // Contour Data Array
    ptDT: array of array [0 .. 2] of Single; // Delaunay Triangulated Data Array
    Group: array [0 .. 10000] of Integer; // Contour Line Group
    TerrainMesh: TGLMesh;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

// Mouse move event handler
procedure TMainForm.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

function TMainForm.WordPosition(const N: Integer; const S: string;
  const WordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do
  begin
    while (I <= Length(S)) and (S[I] in WordDelims) do
      Inc(I);
    if I <= Length(S) then
      Inc(Count);
    if Count <> N then
      while (I <= Length(S)) and not(S[I] in WordDelims) do
        Inc(I)
    else
      Result := I;
  end;
end;

function TMainForm.ExtractWord(N: Integer; const S: string;
  const WordDelims: TSysCharSet): string;
var
  I: Integer;
  Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then
    while (I <= Length(S)) and not(S[I] in WordDelims) do
    begin
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;

procedure TMainForm.LoadVerticesFromFile(filename: String);
var
  f: TStringList;
  linestring, strX, strY, strZ: String;
  row, N, nGroup, nVerticePerGroup: Integer;

  // Read 1 line from file
  function ReadLine: String;
  begin
    Result := f[N];
    Inc(N);
  end;

begin
  f := TStringList.create;
  f.LoadFromFile(filename);

  row := 0;
  nVertice := 0;
  nGroup := 0;
  nVerticePerGroup := 0;

  SetLength(ptVertex, f.Count * 3); // Dynamic alloc.

  try
    // Get header information
    linestring := ReadLine();
    Xmin := StrToFloat(ExtractWord(1, linestring, ['~']));
    Xmax := StrToFloat(ExtractWord(2, linestring, ['~']));
    linestring := ReadLine();
    Ymin := StrToFloat(ExtractWord(1, linestring, ['~']));
    Ymax := StrToFloat(ExtractWord(2, linestring, ['~']));
    linestring := ReadLine();
    Zmin := StrToFloat(ExtractWord(1, linestring, ['~']));
    Zmax := StrToFloat(ExtractWord(2, linestring, ['~']));

    // Parse ID and vertice coord data
    while (row < f.Count - 3) do
    begin
      linestring := ReadLine();
      if (copy(linestring, 1, 2) <> 'ID') then
      begin
        strX := ExtractWord(1, linestring, [',']);
        strY := ExtractWord(2, linestring, [',']);
        strZ := ExtractWord(3, linestring, [',']);
        if (strX <> '') and (strY <> '') and (strZ <> '') then
        begin
          ptVertex[nVertice][0] := StrToFloat(strX);
          ptVertex[nVertice][1] := StrToFloat(strY);
          ptVertex[nVertice][2] := StrToFloat(strZ);
        end;
        Inc(nVertice);
        Inc(nVerticePerGroup);
      end
      else
      begin
        ID := ExtractWord(2, linestring, [':']);
        if (row <> 0) then
        begin
          Group[nGroup] := nVerticePerGroup;
          Inc(nGroup);
          nVerticePerGroup := 0;
        end;
      end;
      Inc(row);
    end;
    NumVertexY := nVertice;
    NumGroup := nGroup;
  finally
    f.free;
  end;

end;

procedure TMainForm.AddTriangle(const p1, p2, p3: TAffineVector;
  const color: TColorVector);
begin
  with TerrainMesh.Vertices do
  begin
    AddVertex(p1, NullVector, color);
    AddVertex(p2, NullVector, color);
    AddVertex(p3, NullVector, color);
  end;
end;

procedure TMainForm.BuildTerrainMesh(MeshFileName: String; MapFileName: String);
var
  av1, av2, av3: TAffineVector;
  I, nRemainder: Integer;
  Devider: Integer;
  XSize, YSize: Single;
  CurrentDir: TFileName;

begin
  NumGroup := 0;
  NumVertexY := 0;

  CurrentDir := GetCurrentDir();
  LoadVerticesFromFile(Format('%s\%s', [CurrentDir, MeshFileName]));
  TerrainMesh := TGLMesh(World.AddNewChild(TGLMesh));
  MapImage.Visible := FALSE;

  with TerrainMesh do
  begin
    Material.PolygonMode := pmFill; // pmLines;
    Material.FaceCulling := fcNoCull;
    Material.Texture.MappingMode := tmmObjectLinear; // tmmEyeLinear, tmmUser - no texture
    Material.Texture.Image.LoadFromFile(CurrentDir + '\' + MapFileName);
    Material.Texture.Disabled := False;

    Mode := mmTriangles;
    Vertices.Clear;

    nRemainder := NumVertexY mod 3;
    XSize := (Xmax - Xmin) / 2;
    YSize := (Ymax - Ymin) / 2;
    Devider := 500;

    /// ///////////////////////////////////////////////////////////////
    // make terrain mesh object
    /// ///////////////////////////////////////////////////////////////
    for I := 0 to Round(nVertice / 3) - 1 do
    begin
      av1 := AffineVectorMake((ptVertex[I * 3, 0] - Xmin - XSize) / Devider,
        (ptVertex[I * 3, 1] - Ymin - YSize) / Devider,
        (ptVertex[I * 3, 2] - Zmin) / Devider);
      av2 := AffineVectorMake((ptVertex[I * 3 + 1, 0] - Xmin - XSize) / Devider,
        (ptVertex[I * 3 + 1, 1] - Ymin - YSize) / Devider,
        (ptVertex[I * 3 + 1, 2] - Zmin) / Devider);
      av3 := AffineVectorMake((ptVertex[I * 3 + 2, 0] - Xmin - XSize) / Devider,
        (ptVertex[I * 3 + 2, 1] - Ymin - YSize) / Devider,
        (ptVertex[I * 3 + 2, 2] - Zmin) / Devider);

      AddTriangle(av1, av2, av3, clrAqua);

      Vertices.VertexTexCoord[I * 3] := texpointmake(av1.X / (Xmax - Xmin),
        av1.Y / (Ymax - Ymin));
      Vertices.VertexTexCoord[I * 3 + 1] := texpointmake(av2.X / (Xmax - Xmin),
        av2.Y / (Ymax - Ymin));
      Vertices.VertexTexCoord[I * 3 + 2] := texpointmake(av3.X / (Xmax - Xmin),
        av3.Y / (Ymax - Ymin));
    end;

    if (nRemainder <> 0) then
    begin
      // Caption := 'Error';
    end;
    CalcNormals(fwCounterClockWise);
    /// Overlay MapFileName
    StructureChanged;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  BuildTerrainMesh('Terrain_1.tri', 'Map_1.jpg');
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TMainForm.DGLContourLinesRender(var rci: TGLRenderContextInfo);
var
  I, nGroup: Integer;
  XSize, YSize: Single;
  Devider: Integer;

begin
  XSize := (Xmax - Xmin) / 2;
  YSize := (Ymax - Ymin) / 2;
  Devider := 10000;

  glPushMatrix();
  for nGroup := 0 to NumGroup do
  begin
    glBegin(GL_LINE_STRIP);
    for I := 0 to (Group[nGroup]) - 1 do
    begin
      glVertex3f((ptVertex[I * 3, 0] - Xmin - XSize) / Devider,
        (ptVertex[I * 3, 1] - Ymin - YSize) / Devider,
        (ptVertex[I * 3, 2] - Zmin) / Devider);
    end;
    glEnd();
  end;
  glPopMatrix();
end;

procedure TMainForm.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if (IsKeyDown(VK_HOME)) then
  begin
    DummyTerrain.Translate(0, -1, 0);
    World.Translate(0, -1, 0);
  end;
  if (IsKeyDown(VK_END)) then
  begin
    DummyTerrain.Translate(0, 1, 0);
    World.Translate(0, 1, 0);
  end;
  if (IsKeyDown(VK_DELETE)) then
  begin
    DummyTerrain.Translate(1, 0, 0);
    World.Translate(1, 0, 0);
  end;
  if (IsKeyDown(VK_NEXT)) then
  begin
    DummyTerrain.Translate(-1, 0, 0);
    World.Translate(-1, 0, 0);
  end;
  if (IsKeyDown(VK_ADD)) then
  begin
    World.Scale.X := World.Scale.X * 1.1;
    World.Scale.Y := World.Scale.Y * 1.1;
    World.Scale.Z := World.Scale.Z * 1.1;
    DummyTerrain.Scale.X := DummyTerrain.Scale.X * 1.1;
    DummyTerrain.Scale.Y := DummyTerrain.Scale.Y * 1.1;
    DummyTerrain.Scale.Z := DummyTerrain.Scale.Z * 1.1;
  end;
  if (IsKeyDown(VK_SUBTRACT)) then
  begin
    World.Scale.X := World.Scale.X / 1.1;
    World.Scale.Y := World.Scale.Y / 1.1;
    World.Scale.Z := World.Scale.Z / 1.1;
    DummyTerrain.Scale.X := DummyTerrain.Scale.X / 1.1;
    DummyTerrain.Scale.Y := DummyTerrain.Scale.Y / 1.1;
    DummyTerrain.Scale.Z := DummyTerrain.Scale.Z / 1.1;
  end;
  if (IsKeyDown(VK_F1)) then
  begin
    DummyTerrain.RotateAbsolute(0, 0, 1);
    World.RotateAbsolute(0, 0, 1);
  end;
  if (IsKeyDown(VK_F2)) then
  begin
    DummyTerrain.RotateAbsolute(0, 0, -1);
    World.RotateAbsolute(0, 0, -1);
  end;
  if (IsKeyDown(VK_F3)) then
  begin
    DummyTerrain.RotateAbsolute(0, 1, 0);
    World.RotateAbsolute(0, 1, 0);
  end;
  if (IsKeyDown(VK_F4)) then
  begin
    DummyTerrain.RotateAbsolute(0, -1, 0);
    World.RotateAbsolute(0, -1, 0);
  end;
  if (IsKeyDown(VK_F5)) then
  begin
    DummyTerrain.RotateAbsolute(1, 0, 0);
    World.RotateAbsolute(1, 0, 0);
  end;
  if (IsKeyDown(VK_F6)) then
  begin
    DummyTerrain.RotateAbsolute(-1, 0, 0);
    World.RotateAbsolute(-1, 0, 0);
  end;
end;

end.
