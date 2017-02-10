unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  System.IniFiles,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,
  // GLS
  GLScene,
  GLObjects,
  GLMesh,
  GLTexture,
  GLVectorTypes,
  GLVectorGeometry,
  GLVectorFileObjects,
  GLVectorLists,
  GLMeshUtils,
  GLCadencer,
  GLCrossPlatform,
  GLContext,
  GLHUDObjects,
  GLFile3DS,
  GLWin32Viewer,
  GLMaterial,
  GLCoordinates,
  GLBaseClasses,
  GLRenderContextInfo,
  GLState,
  GLTriangulation;

type
  TForm1 = class(TForm)
    GLScene: TGLScene;
    GLLightSource1: TGLLightSource;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    SmoothTB: TTrackBar;
    SubdivideBtn: TButton;
    GroupBox2: TGroupBox;
    WireframeCB: TCheckBox;
    Timer1: TTimer;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer: TGLCadencer;
    TimeLabel: TLabel;
    TexturedCB: TCheckBox;
    GLDirectOpenGL1: TGLDirectOpenGL;
    MatLib: TGLMaterialLibrary;
    Windrose: TGLFreeForm;
    CamH: TGLDummyCube;
    CamV: TGLDummyCube;
    Camera: TGLCamera;
    ffTerrain: TGLFreeForm;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SubdivideBtnClick(Sender: TObject);
    procedure WireframeCBClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure TexturedCBClick(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    mx, my: Integer;
    MousePos: TPoint;
    t: Int64;

    IsLeftMouseDown, IsRightMouseDown: Boolean;

    procedure BuildMesh(TargetMesh: TGLFreeForm; D: TGLDelaunay2D);
    procedure SubdivideMesh(TargetMesh: TGLFreeForm; Smooth: Single);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MeshObj: TMeshObject;
  Delaunay2D: TGLDelaunay2D;

  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
  i: Integer;
  X, Y, z, u, v: Single;
  ExePath: TFileName;
  AMatIndex: Integer;

begin
  ffTerrain.Material.Texture.Image.LoadFromFile('terr3.jpg');
  ExePath := ExtractFilePath(Application.ExeName);
  Ini := TIniFile.Create(ExePath + 'Vertexes.ini');
  Delaunay2D := TGLDelaunay2D.Create;

  for i := 0 to 559 do
  begin
    X := Ini.ReadFloat(IntToStr(i), 'X', 0.0);
    Y := Ini.ReadFloat(IntToStr(i), 'Y', 0.0);
    z := Ini.ReadFloat(IntToStr(i), 'Z', 0.0);
    u := Ini.ReadFloat(IntToStr(i), 'U', 0.0);
    v := Ini.ReadFloat(IntToStr(i), 'V', 0.0);
    AMatIndex := 0; // no material
    Delaunay2D.AddPoint(X, Y, z, u, v, AMatIndex);
  end;

  t := StartPrecisionTimer;

  Delaunay2D.Mesh(True);
  BuildMesh(ffTerrain, Delaunay2D);
  Ini.Free;

  TimeLabel.Caption := Format('Delaunay triangulation completed in %.1f ms' +
    #10#13, [StopPrecisionTimer(t) * 1000]);

  Windrose.LoadFromFile('windrose.3ds');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Delaunay2D.Free;
end;

procedure TForm1.BuildMesh(TargetMesh: TGLFreeForm; D: TGLDelaunay2D);
var
  i: Integer;
  vert1, vert2, vert3: DVertex;
  FaceGroup: TFGVertexIndexList;
begin
  TargetMesh.MeshObjects.Clear;

  MeshObj := TMeshObject.CreateOwned(TargetMesh.MeshObjects);
  MeshObj.Mode := momFaceGroups;

  FaceGroup := TFGVertexIndexList.CreateOwned(MeshObj.FaceGroups);

  for i := 1 to D.HowMany do
  begin
    vert1 := D.Vertex[D.Triangle[i].vv0];
    vert2 := D.Vertex[D.Triangle[i].vv1];
    vert3 := D.Vertex[D.Triangle[i].vv2];

    MeshObj.Vertices.Add(vert1.X, vert1.Y, vert1.z);
    MeshObj.Vertices.Add(vert2.X, vert2.Y, vert2.z);
    MeshObj.Vertices.Add(vert3.X, vert3.Y, vert3.z);

    MeshObj.Colors.Add(1.0, 0.0, 0.0, 1.0);
    MeshObj.Colors.Add(0.0, 1.0, 0.0, 1.0);
    MeshObj.Colors.Add(0.0, 0.0, 1.0, 1.0);

    MeshObj.TexCoords.Add(vert1.u, vert1.v);
    MeshObj.TexCoords.Add(vert2.u, vert2.v);
    MeshObj.TexCoords.Add(vert3.u, vert3.v);

    FaceGroup.Add(MeshObj.Vertices.Count - 1);
    FaceGroup.Add(MeshObj.Vertices.Count - 2);
    FaceGroup.Add(MeshObj.Vertices.Count - 3);
  end;

  MeshObj.BuildNormals(FaceGroup.VertexIndices, momTriangles);

  TargetMesh.StructureChanged;
end;

procedure TForm1.SubdivideMesh(TargetMesh: TGLFreeForm; Smooth: Single);
var
  i: Integer;
  tris, norms, tex, buf: TAffineVectorList;
  indices, texIndices: TIntegerlist;
  MatName: String;
begin
  for i := 0 to TargetMesh.MeshObjects.Count - 1 do
  begin
    tex := TAffineVectorList.Create;
    with TargetMesh.MeshObjects[i] do
    begin
      tris := ExtractTriangles(tex);
    end;
    indices := BuildVectorCountOptimizedIndices(tris);
    RemapAndCleanupReferences(tris, indices);

    norms := BuildNormals(tris, indices);

    // subdivide geometry
    SubdivideTriangles(Smooth, tris, indices, norms);

    texIndices := BuildVectorCountOptimizedIndices(tex);
    RemapAndCleanupReferences(tex, texIndices);

    // subdivide texture space
    SubdivideTriangles(0, tex, texIndices);

    // Re-expand everything
    buf := TAffineVectorList.Create;
    try
      ConvertIndexedListToList(tris, indices, buf);
      tris.Assign(buf);
      buf.Count := 0;
      ConvertIndexedListToList(norms, indices, buf);
      norms.Assign(buf);
      buf.Count := 0;
      ConvertIndexedListToList(tex, texIndices, buf);
      tex.Assign(buf);
    finally
      buf.Free;
    end;

    // Pack & Optimize the expanded stuff

    indices.Free;
    indices := BuildVectorCountOptimizedIndices(tris, norms, tex);

    RemapReferences(norms, indices);
    RemapReferences(tex, indices);
    RemapAndCleanupReferences(tris, indices);

    IncreaseCoherency(indices, 13);

    with TargetMesh.MeshObjects[i] do
    begin
      Vertices := tris;
      Normals := norms;
      TexCoords := tex;

      MatName := FaceGroups.Items[0].MaterialName;
      FaceGroups.Clear;
      with TFGVertexIndexList.CreateOwned(FaceGroups) do
      begin
        MaterialName := MatName;
        VertexIndices := indices;
        Mode := fgmmTriangles;
      end;
    end;

    texIndices.Free;
    tex.Free;
    indices.Free;
    norms.Free;
    tris.Free;
  end;

  TargetMesh.StructureChanged;
end;

procedure TForm1.SubdivideBtnClick(Sender: TObject);
begin
  t := StartPrecisionTimer;
  SubdivideMesh(ffTerrain, SmoothTB.Position / 10);
  TimeLabel.Caption := TimeLabel.Caption +
    Format('Subdivide completed in %.1f ms' + #10#13,
    [StopPrecisionTimer(t) * 1000]);
end;

procedure TForm1.WireframeCBClick(Sender: TObject);
begin
  if (WireframeCB.Checked) then
    ffTerrain.Material.PolygonMode := pmLines
  else
  begin
    ffTerrain.Material.PolygonMode := pmFill;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS -  %d Triangles', [GLSceneViewer1.FramesPerSecond,
    ffTerrain.MeshObjects.TriangleCount]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  deltax, deltay: Single;
  pt: TPoint;
begin
  if (IsLeftMouseDown or IsRightMouseDown) then
  begin
    GetCursorPos(pt);
    deltax := (MousePos.X - pt.X) / 5;
    deltay := (MousePos.Y - pt.Y) / 5;
    if (pt.X <> MousePos.X) or (pt.Y <> MousePos.Y) then
      SetCursorPos(MousePos.X, MousePos.Y);
  end;

  // rotate
  if IsLeftMouseDown then
  begin
    CamH.TurnAngle := CamH.TurnAngle + deltax;
    if CamH.TurnAngle >= 360 then
      CamH.TurnAngle := CamH.TurnAngle - 360;
    if CamH.TurnAngle < 0 then
      CamH.TurnAngle := CamH.TurnAngle + 360;

    // rotation of camera in half sphere
    if (CamV.PitchAngle - deltay < 89) and (CamV.PitchAngle - deltay > 0) then
      CamV.PitchAngle := CamV.PitchAngle - deltay;
  end
  else
  begin
    // moving camera (pan)
    if IsRightMouseDown then
    begin
      CamH.Move(-50 * deltay * deltaTime);
      CamH.Slide(50 * deltax * deltaTime);
    end;
  end;

  GLSceneViewer1.Invalidate;
end;

procedure TForm1.TexturedCBClick(Sender: TObject);
begin
  ffTerrain.Material.Texture.Enabled := TexturedCB.Checked;
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  size: Single;
begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  glOrtho(0, GLSceneViewer1.Width, 0, GLSceneViewer1.Height, -1000, 1000);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  GLScene.SetupLights(GLScene.CurrentBuffer.LimitOf[limLights]);

  size := GLSceneViewer1.Height / 6;

  Windrose.ResetRotations;
  Windrose.PitchAngle := -CamV.PitchAngle + 90;
  Windrose.RollAngle := -CamH.TurnAngle;

  Windrose.Position.AsVector := VectorMake(size, size, 0);
  Windrose.Scale.AsVector := VectorMake(size, size, size);

  Windrose.Render(rci);

  glMatrixMode(GL_PROJECTION);
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopAttrib;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) then
    IsRightMouseDown := True;

  if (ssLeft in Shift) then
  begin
    mx := X;
    my := Y;
    IsLeftMouseDown := True;
  end;

  Screen.Cursor := crNone;
  GetCursorPos(MousePos);
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton(mbLeft) then
    IsLeftMouseDown := False;

  if Button = TMouseButton(mbRight) then
    IsRightMouseDown := False;

  if not((ssLeft in Shift) or (ssRight in Shift)) then
    Screen.Cursor := crDefault;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  DistDelta: Single;
begin
  with Camera do
  begin
    DistDelta := Power(1.1, WheelDelta / 240);

    if (DistanceToTarget > 10) or (WheelDelta > 0) then
      AdjustDistanceToTarget(DistDelta);
  end;
end;

end.
