unit FMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.IniFiles, System.Win.Registry,
  System.ImageList, System.Math, System.Actions, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ActnList, Vcl.Menus, Vcl.ImgList, Vcl.ToolWin,
  Vcl.ComCtrls, Vcl.ExtDlgs, Vcl.ExtCtrls,
  Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ActnMenus, Vcl.StdActns, Vcl.BandActn,
  Vcl.PlatformDefaultStyleActnCtrls,

  // GLS
  GLMaterial, GLScene, GLWin32Viewer, GLVectorFileObjects, GLObjects,
  GLVectorGeometry,  GLTexture, GLContext, GLVectorLists, GLCadencer, GLCoordinates,
  GLCrossPlatform,  GLBaseClasses, GLMeshOptimizer, GLState, GLRenderContextInfo,
  GLTextureFormat, GLColor, GLKeyBoard, GLGraphics, GLPersistentClasses, GLMeshUtils,
  GLVectorTypes,

  //GLSViewer
  FGLForm, FGLAbout, GNUgettext, FGLOptions, DGLSViewer;

type
  TMainForm = class(TGLForm)
    ImageList: TImageList;
    StatusBar: TStatusBar;
    GLScene: TGLScene;
    FreeForm: TGLFreeForm;
    GLLightSource: TGLLightSource;
    GLMaterialLibrary: TGLMaterialLibrary;
    CubeExtents: TGLCube;
    DCTarget: TGLDummyCube;
    GLCamera: TGLCamera;
    DCAxis: TGLDummyCube;
    GLCadencer: TGLCadencer;
    Timer: TTimer;
    GLLightmapLibrary: TGLMaterialLibrary;
    GLSceneViewer: TGLSceneViewer;
    ActionManager: TActionManager;
    acOptimizeMesh: TAction;
    acProcessInvertNormals: TAction;
    acReverseRendering: TAction;
    acConvertToTriangles: TAction;
    acProcessStripify: TAction;
    acToolsOptions: TAction;
    acToolsFaceCulling: TAction;
    acToolsTexturing: TAction;
    acToolsLighting: TAction;
    acToolsCustomize: TCustomizeActionBars;
    acToolsShowFPS: TAction;
    acViewSmoothShading: TAction;
    acViewFlatShading: TAction;
    acViewFlatLines: TAction;
    acViewHiddenLines: TAction;
    acViewWireFrame: TAction;
    acViewZoomIn: TAction;
    acViewZoomOut: TAction;
    acViewReset: TAction;
    acFileOpen: TAction;
    acFilePick: TAction;
    acFileOpenTexLib: TAction;
    acFileSaveAs: TAction;
    acFileSaveTextures: TAction;
    acFileExit: TAction;
    acHelpContents: THelpContents;
    acHelpTopicSearch: THelpTopicSearch;
    acHelpOnHelp: THelpOnHelp;
    acHelpGLSHomePage: TAction;
    acHelpAbout: TAction;
    acAADefault: TAction;
    acAA2X: TAction;
    acAA4X: TAction;
    acEditUndo: TEditUndo;
    acEditCut: TEditCut;
    acEditCopy: TEditCopy;
    acEditPaste: TEditPaste;
    acEditSelectAll: TEditSelectAll;
    acEditDelete: TEditDelete;
    ImageListMenu: TImageList;
    acViewAxes: TAction;
    ControlBar: TControlBar;
    amMenuBar: TActionMainMenuBar;
    acAA8X: TAction;
    acAA16X: TAction;
    acCSA8X: TAction;
    acCSA16X: TAction;
    atbTools: TActionToolBar;
    atbView: TActionToolBar;
    atbFile: TActionToolBar;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerBeforeRender(Sender: TObject);
    procedure GLSceneViewerAfterRender(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GLMaterialLibraryTextureNeeded(Sender: TObject;
      var textureFileName: String);
    procedure acInvertNormalsExecute(Sender: TObject);
    procedure acSaveAsUpdate(Sender: TObject);
    procedure acReverseRenderingOrderExecute(Sender: TObject);
    procedure acConvertToIndexedTrianglesExecute(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure TimerTimer(Sender: TObject);
    procedure acOptimizeExecute(Sender: TObject);
    procedure acStripifyExecute(Sender: TObject);
    procedure acHelpAboutExecute(Sender: TObject);
    procedure acFilePickExecute(Sender: TObject);
    procedure acFileOpenTexLibExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFileSaveAsExecute(Sender: TObject);
    procedure acFileSaveTexturesExecute(Sender: TObject);
    procedure acFileExitExecute(Sender: TObject);
    procedure acToolsOptionsExecute(Sender: TObject);
    procedure acToolsTexturingExecute(Sender: TObject);
    procedure acToolsFaceCullingExecute(Sender: TObject);
    procedure acToolsLightingExecute(Sender: TObject);
    procedure acToolsShowFPSExecute(Sender: TObject);
    procedure acAADefaultExecute(Sender: TObject);
    procedure acViewSmoothShadingExecute(Sender: TObject);
    procedure acViewFlatShadingExecute(Sender: TObject);
    procedure acViewFlatLinesExecute(Sender: TObject);
    procedure acViewHiddenLinesExecute(Sender: TObject);
    procedure acViewWireFrameExecute(Sender: TObject);
    procedure acViewAxesExecute(Sender: TObject);
    procedure acViewResetExecute(Sender: TObject);
    procedure acViewZoomOutExecute(Sender: TObject);
    procedure acViewZoomInExecute(Sender: TObject);
  private
    { Private declarations }
    procedure DoResetCamera;
    procedure SetupFreeFormShading;
    procedure ApplyShadeModeToMaterial(aMaterial: TGLMaterial);
    procedure ApplyShadeMode;
    procedure ApplyFSAA;
    procedure ApplyFaceCull;
    procedure ApplyTexturing;
    procedure ApplyFPS;

    procedure DoOpen(const fileName: String);
  public
    { Public declarations }
    md, nthShow: Boolean;
    mx, my: Integer;
    hlShader: TGLShader;
    lastFileName: String;
    lastLoadWithTextures: Boolean;
    procedure ApplyBgColor;
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  GLFileOBJ, GLFileSTL, GLFileLWO, GLFileQ3BSP, GLFileOCT, GLFileMS3D,
  GLFileNMF, GLFileMD3, GLFile3DS, GLFileMD2, GLFileSMD, GLFilePLY, GLFileGTS,
  GLFileVRML, GLFileMD5, GLFileTIN, GLFileDXF, GLFileGRD;

type
  // Hidden line shader (specific implem for the viewer, *not* generic)
  THiddenLineShader = class(TGLShader)
  private
    LinesColor: TColorVector;
    BackgroundColor: TColorVector;
    PassCount: Integer;
  public
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  end;

procedure THiddenLineShader.DoApply(var rci: TRenderContextInfo;
  Sender: TObject);
begin
  PassCount := 1;
  with rci.GLStates do
  begin
    PolygonMode := pmFill;
    GL.Color3fv(@BackgroundColor);
    ActiveTextureEnabled[ttTexture2D] := False;
    Enable(stPolygonOffsetFill);
    PolygonOffsetFactor := 1;
    PolygonOffsetUnits := 2;
  end;
end;

function THiddenLineShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  case PassCount of
    1:
      with rci.GLStates do
      begin
        PassCount := 2;
        PolygonMode := pmLines;
        GL.Color3fv(@LinesColor);
        Disable(stLighting);
        Result := True;
      end;
    2:
      begin
        rci.GLStates.Disable(stPolygonOffsetFill);
        Result := False;
      end;
  else
    // doesn't hurt to be cautious
    Assert(False);
    Result := False;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);

begin
  inherited;
 // instantiate our specific hidden-lines shader
  hlShader := THiddenLineShader.Create(Self);

  FreeForm.IgnoreMissingTextures := True;

end;

procedure TMainForm.FormShow(Sender: TObject);
var
  i: Integer;
begin
  if not nthShow then
  begin
    dmGLSViewer.OpenDialog.Filter := VectorFileFormatsFilter;
    dmGLSViewer.SaveDialog.Filter := VectorFileFormatsSaveFilter;
    ApplyFSAA;
    ApplyFaceCull;
    ApplyBgColor;
    ApplyFPS;
    if ParamCount > 0 then
      DoOpen(ParamStr(1));
    nthShow := True;
  end;
end;

procedure TMainForm.acFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.acFileOpenExecute(Sender: TObject);
begin
  if dmGLSViewer.OpenDialog.Execute then
    DoOpen(dmGLSViewer.OpenDialog.fileName);
end;

procedure TMainForm.acFileOpenTexLibExecute(Sender: TObject);
var
  I: Integer;
begin
  if dmGLSViewer.ODTextures.Execute then
    with GLMaterialLibrary do
    begin
      LoadFromFile(dmGLSViewer.ODTextures.fileName);
      for I := 0 to Materials.Count - 1 do
        with Materials[i].Material do
          BackProperties.Assign(FrontProperties);
      ApplyShadeMode;
      ApplyTexturing;
    end;
end;

procedure TMainForm.acFilePickExecute(Sender: TObject);
begin
  if dmGLSViewer.opDialog.Execute then
  begin
    with GLMaterialLibrary.Materials do
    begin
      with Items[Count - 1] do
      begin
        Tag := 1;
        Material.Texture.Image.LoadFromFile
          (dmGLSViewer.opDialog.fileName);
        Material.Texture.Enabled := True;
      end;
    end;
    ApplyTexturing;
  end;
end;

procedure TMainForm.acFileSaveAsExecute(Sender: TObject);
var
  ext : String;
begin
  if dmGLSViewer.SaveDialog.Execute then
  begin
    ext := ExtractFileExt(dmGLSViewer.SaveDialog.fileName);
    if ext = '' then
      dmGLSViewer.SaveDialog.fileName :=
        ChangeFileExt(dmGLSViewer.SaveDialog.fileName,
        '.' + GetVectorFileFormats.FindExtByIndex
        (dmGLSViewer.SaveDialog.FilterIndex, False, True));
    if GetVectorFileFormats.FindFromFileName(dmGLSViewer.SaveDialog.fileName) = nil
    then
      ShowMessage(_('Unsupported or unspecified file extension.'))
    else
      FreeForm.SaveToFile(dmGLSViewer.SaveDialog.fileName);
  end;
end;

procedure TMainForm.acFileSaveTexturesExecute(Sender: TObject);
begin
  if dmGLSViewer.SDTextures.Execute then
    GLMaterialLibrary.SaveToFile(dmGLSViewer.SDTextures.fileName);
end;


procedure TMainForm.GLSceneViewerBeforeRender(Sender: TObject);
begin
  THiddenLineShader(hlShader).LinesColor := VectorMake(107 / 256, 123 / 256,
    173 / 256, 1);
  THiddenLineShader(hlShader).BackgroundColor :=
    ConvertWinColor(GLSceneViewer.Buffer.BackgroundColor);
  if not GL.ARB_multisample then
  begin
    acAADefault.Checked := True;
    acAA2X.Enabled := False;
    acAA4X.Enabled := False;
    acAA8X.Enabled := False;
    acAA16X.Enabled := False;
    acCSA8X.Enabled := False;
    acCSA16X.Enabled := False;
  end;
end;

procedure TMainForm.GLSceneViewerAfterRender(Sender: TObject);
begin
  ApplyFSAA;
  Screen.Cursor := crDefault;
end;

procedure TMainForm.DoResetCamera;
var
  objSize: Single;
begin
  DCTarget.Position.AsVector := NullHmgPoint;
  GLCamera.Position.SetPoint(7, 3, 5);
  FreeForm.Position.AsVector := NullHmgPoint;
  FreeForm.Up.Assign(DCAxis.Up);
  FreeForm.Direction.Assign(DCAxis.Direction);

  objSize := FreeForm.BoundingSphereRadius;
  if objSize > 0 then
  begin
    if objSize < 1 then
    begin
      GLCamera.SceneScale := 1 / objSize;
      objSize := 1;
    end
    else
      GLCamera.SceneScale := 1;
    GLCamera.AdjustDistanceToTarget(objSize * 0.27);
    GLCamera.DepthOfView := 1.5 * GLCamera.DistanceToTarget + 2 * objSize;
  end;
end;

procedure TMainForm.ApplyShadeModeToMaterial(aMaterial: TGLMaterial);
begin
  with aMaterial do
  begin
    if acViewSmoothShading.Checked then
    begin
      GLSceneViewer.Buffer.Lighting := True;
      GLSceneViewer.Buffer.ShadeModel := smSmooth;
      aMaterial.PolygonMode := pmFill;
    end
    else if acViewFlatShading.Checked then
    begin
      GLSceneViewer.Buffer.Lighting := True;
      GLSceneViewer.Buffer.ShadeModel := smFlat;
      aMaterial.PolygonMode := pmFill;
    end
    else if acViewFlatLines.Checked then
    begin
      GLSceneViewer.Buffer.Lighting := True;
      GLSceneViewer.Buffer.ShadeModel := smFlat;
      aMaterial.PolygonMode := pmLines;
    end
    else if acViewHiddenLines.Checked then
    begin
      GLSceneViewer.Buffer.Lighting := False;
      GLSceneViewer.Buffer.ShadeModel := smSmooth;
      aMaterial.PolygonMode := pmLines;
    end
    else if acViewWireframe.Checked then
    begin
      GLSceneViewer.Buffer.Lighting := False;
      GLSceneViewer.Buffer.ShadeModel := smSmooth;
      aMaterial.PolygonMode := pmLines;
    end;
  end;
end;

procedure TMainForm.ApplyShadeMode;
var
  i: Integer;
begin
  with GLMaterialLibrary.Materials do
    for i := 0 to Count - 1 do
    begin
      ApplyShadeModeToMaterial(Items[i].Material);
      if (acViewHiddenLines.Checked) or (acViewFlatLines.Checked) then
        Items[i].Shader := hlShader
      else
        Items[i].Shader := nil;
    end;
  GLSceneViewer.Buffer.Lighting := acToolsLighting.Checked;
  FreeForm.StructureChanged;
end;

procedure TMainForm.ApplyFSAA;
begin
  with GLSceneViewer.Buffer do
  begin
    if acAADefault.Checked then
      AntiAliasing := aaDefault
    else if acAA2X.Checked then
      AntiAliasing := aa2x
    else if acAA4X.Checked then
      AntiAliasing := aa4x
    else if acAA8X.Checked then
      AntiAliasing := aa8x
    else if acAA16X.Checked then
      AntiAliasing := aa16x
    else if acCSA8X.Checked then
      AntiAliasing := csa8x
    else if acCSA16X.Checked then
      AntiAliasing := csa16x;
  end;
end;

procedure TMainForm.ApplyFaceCull;
begin
  with GLSceneViewer.Buffer do
  begin
    if acToolsFaceCulling.Checked then
    begin
      FaceCulling := True;
      ContextOptions := ContextOptions - [roTwoSideLighting];
    end
    else
    begin
      FaceCulling := False;
      ContextOptions := ContextOptions + [roTwoSideLighting];
    end;
  end;
end;

procedure TMainForm.ApplyBgColor;
var
  bmp: TBitmap;
  col: TColor;
begin
  bmp := TBitmap.Create;
  try
    bmp.Width := 16;
    bmp.Height := 16;
    col := ColorToRGB(dmGLSViewer.ColorDialog.Color);
    GLSceneViewer.Buffer.BackgroundColor := col;
    with bmp.Canvas do
    begin
      Pen.Color := col xor $FFFFFF;
      Brush.Color := col;
      Rectangle(0, 0, 16, 16);
    end;
  finally
    bmp.Free;
  end;
end;

procedure TMainForm.ApplyTexturing;
var
  i: Integer;
begin
  with GLMaterialLibrary.Materials do
    for i := 0 to Count - 1 do
    begin
      with Items[i].Material.Texture do
      begin
        if Enabled then
          Items[i].Tag := Integer(True);
        Enabled := Boolean(Items[i].Tag) and
          acToolsTexturing.Checked;
      end;
    end;
  FreeForm.StructureChanged;
end;

procedure TMainForm.ApplyFPS;
begin
  if acToolsShowFPS.Checked then
  begin
    Timer.Enabled := True;
    GLCadencer.Enabled := True;
  end
  else
  begin
    Timer.Enabled := False;
    GLCadencer.Enabled := False;
    StatusBar.Panels[1].Text := '--- FPS';
  end;
end;

procedure TMainForm.SetupFreeFormShading;
var
  i: Integer;
  libMat: TGLLibMaterial;
begin
  with GLMaterialLibrary do
  begin
    if Materials.Count = 0 then
    begin
      FreeForm.Material.MaterialLibrary := GLMaterialLibrary;
      libMat := Materials.Add;
      FreeForm.Material.LibMaterialName := libMat.Name;
      libMat.Material.FrontProperties.Diffuse.Red := 0;
    end;
    for i := 0 to Materials.Count - 1 do
      with Materials[i].Material do
        BackProperties.Assign(FrontProperties);
  end;
  ApplyShadeMode;
  ApplyTexturing;
  ApplyFPS;
end;

procedure TMainForm.DoOpen(const fileName: String);
var
  min, max: TAffineVector;
begin
  if not FileExists(fileName) then
    Exit;

  Screen.Cursor := crHourGlass;

  Caption := 'GLSViewer - ' + ExtractFileName(fileName);

  GLMaterialLibrary.Materials.Clear;

  FreeForm.MeshObjects.Clear;
  FreeForm.LoadFromFile(fileName);

  SetupFreeFormShading;

  StatusBar.Panels[0].Text := IntToStr(FreeForm.MeshObjects.TriangleCount)
    + ' tris';
  StatusBar.Panels[2].Text := fileName;
  acFileSaveTextures.Enabled := (GLMaterialLibrary.Materials.Count > 0);
  acFileOpenTexLib.Enabled := (GLMaterialLibrary.Materials.Count > 0);
  lastFileName := fileName;
  lastLoadWithTextures := acToolsTexturing.Enabled;

  FreeForm.GetExtents(min, max);
  with CubeExtents do
  begin
    CubeWidth := max.X - min.X;
    CubeHeight := max.Y - min.Y;
    CubeDepth := max.Z - min.Z;
    Position.AsAffineVector := VectorLerp(min, max, 0.5);
  end;
  DoResetCamera;
end;

procedure TMainForm.GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  md := True;
end;

procedure TMainForm.GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  d: Single;
begin
  if md and (Shift <> []) then
  begin
    if ssLeft in Shift then
      if ssShift in Shift then
        GLCamera.MoveAroundTarget((my - Y) * 0.1, (mx - X) * 0.1)
      else
        GLCamera.MoveAroundTarget(my - Y, mx - X)
    else if ssRight in Shift then
    begin
      d := GLCamera.DistanceToTarget * 0.01 * (X - mx + Y - my);
      if IsKeyDown('x') then
        FreeForm.Translate(d, 0, 0)
      else if IsKeyDown('y') then
        FreeForm.Translate(0, d, 0)
      else if IsKeyDown('z') then
        FreeForm.Translate(0, 0, d)
      else
      begin
        if ssShift in Shift then
          GLCamera.RotateObject(FreeForm, (my - Y) * 0.1, (mx - X) * 0.1)
        else
          GLCamera.RotateObject(FreeForm, my - Y, mx - X);
      end;
    end;
    mx := X;
    my := Y;
  end;
end;

procedure TMainForm.GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  md := False;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if FreeForm.MeshObjects.Count > 0 then
  begin
    GLCamera.AdjustDistanceToTarget(Power(1.05, WheelDelta / 120));
    GLCamera.DepthOfView := 2 * GLCamera.DistanceToTarget + 2 *
      FreeForm.BoundingSphereRadius;
  end;
  Handled := True;
end;

procedure TMainForm.GLMaterialLibraryTextureNeeded(Sender: TObject;
  var textureFileName: String);
begin
  if not acToolsTexturing.Enabled then
    textureFileName := '';
end;

procedure TMainForm.acInvertNormalsExecute(Sender: TObject);
var
  i: Integer;
begin
  with FreeForm.MeshObjects do
    for i := 0 to Count - 1 do
      Items[i].Normals.Scale(-1);
  FreeForm.StructureChanged;
end;

procedure TMainForm.acReverseRenderingOrderExecute(Sender: TObject);
var
  i, j, n: Integer;
  fg: TFaceGroup;
begin
  with FreeForm.MeshObjects do
  begin
    // invert meshobjects order
    for i := 0 to (Count div 2) do
      Exchange(i, Count - 1 - i);
    // for each mesh object
    for i := 0 to Count - 1 do
      with Items[i] do
      begin
        // invert facegroups order
        n := FaceGroups.Count;
        for j := 0 to (n div 2) do
          Exchange(j, n - 1 - j);
        // for each facegroup
        for j := 0 to n - 1 do
        begin
          fg := FaceGroups[j];
          fg.Reverse;
        end;
      end;
  end;
  FreeForm.StructureChanged;
end;

procedure TMainForm.acSaveAsUpdate(Sender: TObject);
begin
  acFileSaveAs.Enabled := (FreeForm.MeshObjects.Count > 0);
end;

procedure TMainForm.acHelpAboutExecute(Sender: TObject);
begin
  with TGLAbout.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.acAADefaultExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := True;
  ApplyFSAA;
end;

procedure TMainForm.acConvertToIndexedTrianglesExecute(Sender: TObject);
var
  v: TAffineVectorList;
  i: TIntegerList;
  m: TMeshObject;
  fg: TFGVertexIndexList;
begin
 v := FreeForm.MeshObjects.ExtractTriangles;
  try
    i := BuildVectorCountOptimizedIndices(v);
    try
      RemapAndCleanupReferences(v, i);
      IncreaseCoherency(i, 12);
      i.Capacity := i.Count;
      FreeForm.MeshObjects.Clean;
      m := TMeshObject.CreateOwned(FreeForm.MeshObjects);
      m.Vertices := v;
      m.BuildNormals(i, momTriangles);
      m.Mode := momFaceGroups;
      fg := TFGVertexIndexList.CreateOwned(m.FaceGroups);
      fg.VertexIndices := i;
      fg.Mode := fgmmTriangles;
      FreeForm.StructureChanged;
    finally
      i.Free;
    end;
  finally
    v.Free;
  end;
  GLMaterialLibrary.Materials.Clear;
  SetupFreeFormShading;
end;

procedure TMainForm.acStripifyExecute(Sender: TObject);
var
  i: Integer;
  mo: TMeshObject;
  fg: TFGVertexIndexList;
  strips: TPersistentObjectList;
begin
  acConvertToTriangles.Execute;
  mo := FreeForm.MeshObjects[0];
  fg := (mo.FaceGroups[0] as TFGVertexIndexList);
  strips := StripifyMesh(fg.VertexIndices, mo.Vertices.Count, True);
  try
    fg.Free;
    for i := 0 to strips.Count - 1 do
    begin
      fg := TFGVertexIndexList.CreateOwned(mo.FaceGroups);
      fg.VertexIndices := (strips[i] as TIntegerList);
      if i = 0 then
        fg.Mode := fgmmTriangles
      else
        fg.Mode := fgmmTriangleStrip;
    end;
  finally
    strips.Free;
  end;
end;

procedure TMainForm.acViewAxesExecute(Sender: TObject);
begin
  (Sender as TGLOptions).CheckBoxAxisClick(nil);
end;

procedure TMainForm.acViewFlatShadingExecute(Sender: TObject);
begin
  ApplyShadeMode;
end;

procedure TMainForm.acViewHiddenLinesExecute(Sender: TObject);
begin
  ApplyShadeMode;
end;

procedure TMainForm.acViewResetExecute(Sender: TObject);
begin
  DoResetCamera;
end;

procedure TMainForm.acViewFlatLinesExecute(Sender: TObject);
begin
  ApplyShadeMode;
end;

procedure TMainForm.acViewSmoothShadingExecute(Sender: TObject);
begin
  ApplyShadeMode;
end;

procedure TMainForm.acViewWireFrameExecute(Sender: TObject);
begin
  ApplyShadeMode;
end;

procedure TMainForm.acViewZoomInExecute(Sender: TObject);
var
  h: Boolean;
begin
  FormMouseWheel(Self, [], -120 * 4, Point(0, 0), h);
end;

procedure TMainForm.acViewZoomOutExecute(Sender: TObject);
var
  h: Boolean;
begin
  FormMouseWheel(Self, [], 120 * 4, Point(0, 0), h);
end;

procedure TMainForm.acOptimizeExecute(Sender: TObject);
begin
  OptimizeMesh(FreeForm.MeshObjects, [mooVertexCache, mooSortByMaterials]);
  FreeForm.StructureChanged;
  SetupFreeFormShading;
end;

procedure TMainForm.acToolsOptionsExecute(Sender: TObject);
begin
  with TGLOptions.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.acToolsFaceCullingExecute(Sender: TObject);
begin
  acToolsFaceCulling.Checked := not acToolsFaceCulling.Checked;
  ApplyFaceCull;
end;

procedure TMainForm.acToolsLightingExecute(Sender: TObject);
begin
  acToolsLighting.Checked := not acToolsLighting.Checked;
  // TBLighting
  ApplyShadeMode;
end;

procedure TMainForm.acToolsShowFPSExecute(Sender: TObject);
begin
  acToolsShowFPS.Checked := not acToolsShowFPS.Checked;
  ApplyFPS;
end;

procedure TMainForm.acToolsTexturingExecute(Sender: TObject);
begin
  acToolsTexturing.Checked := not acToolsTexturing.Checked;
  if acToolsTexturing.Checked then
    if lastLoadWithTextures then
      ApplyTexturing
    else
    begin
      DoOpen(lastFileName);
    end
  else
    ApplyTexturing;
end;

procedure TMainForm.GLCadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if Self.Focused then
    GLSceneViewer.Invalidate;
end;


procedure TMainForm.TimerTimer(Sender: TObject);
begin
  StatusBar.Panels[1].Text := Format('%.1f FPS',
    [GLSceneViewer.FramesPerSecond]);
  GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TMainForm.ReadIniFile;
begin
  inherited;
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  with IniFile do
    try
      Top := ReadInteger(Name, 'Top', 100);
      Left := ReadInteger(Name, 'Left', 200);
      if ReadBool(Name, 'InitMax', False) then
        WindowState := wsMaximized
      else
        WindowState := wsNormal;
    finally
      IniFile.Free;
    end;
end;

procedure TMainForm.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  with IniFile do
    try
      WriteInteger(Name, 'Top', Top);
      WriteInteger(Name, 'Left', Left);
      WriteBool(Name, 'InitMax', WindowState = wsMaximized);
    finally
      IniFile.Free;
    end;
  inherited;
end;

end.
