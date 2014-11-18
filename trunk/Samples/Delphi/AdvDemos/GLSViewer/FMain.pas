unit FMain;

interface

{$I GLScene.inc}

uses
  {$IFDEF GLS_DELPHI_XE2_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.IniFiles, System.Win.Registry,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ImgList, Vcl.ToolWin, Vcl.ComCtrls,
  Vcl.ExtDlgs, Vcl.ExtCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Classes, IniFiles, Registry, Graphics, Controls,
  Forms, Dialogs, Actions, ActnList, Menus, ImgList, ToolWin, ComCtrls,
  ExtDlgs, ExtCtrls,
  {$ENDIF}

  //GLScene
  GLMaterial, GLScene, GLWin32Viewer, GLVectorFileObjects, GLObjects, GLVectorGeometry,
  GLTexture, GLContext,  GLVectorLists, GLCadencer,  GLCoordinates,  GLCrossPlatform,
  GLBaseClasses, GLMeshOptimizer, GLState, GLRenderContextInfo, GLTextureFormat,
  GLColor, GLKeyBoard, GLGraphics, GLPersistentClasses, GLMeshUtils,

  FGLForm, FGLAbout, GNUgettext, FGLOptions, DGLSViewer;

type
  TMain = class(TGLForm)
    MainMenu: TMainMenu;
    ActionList: TActionList;
    ImageList: TImageList;
    ToolBar: TToolBar;
    miFile: TMenuItem;
    miAbout: TMenuItem;
    acOpen: TAction;
    acExit: TAction;
    miOpen: TMenuItem;
    N1: TMenuItem;
    miExit: TMenuItem;
    ToolButton1: TToolButton;
    StatusBar: TStatusBar;
    GLScene: TGLScene;
    miOption: TMenuItem;
    miAntiAlias: TMenuItem;
    miAADefault: TMenuItem;
    miMSAA2X: TMenuItem;
    miMSAA4X: TMenuItem;
    acSaveAs: TAction;
    acZoomIn: TAction;
    acZoomOut: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    miView: TMenuItem;
    miZoomIn: TMenuItem;
    miZoomOut: TMenuItem;
    FreeForm: TGLFreeForm;
    GLLightSource: TGLLightSource;
    GLMaterialLibrary: TGLMaterialLibrary;
    CubeExtents: TGLCube;
    acResetView: TAction;
    miResetview: TMenuItem;
    ToolButton5: TToolButton;
    acShadeSmooth: TAction;
    acFlatShading: TAction;
    acWireframe: TAction;
    acHiddenLines: TAction;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    N2: TMenuItem;
    miSmoothshading: TMenuItem;
    miFlatshading: TMenuItem;
    miHiddenlines: TMenuItem;
    miWireframe: TMenuItem;
    ToolButton10: TToolButton;
    acCullFace: TAction;
    miFaceculling: TMenuItem;
    N3: TMenuItem;
    MITexturing: TMenuItem;
    acTexturing: TAction;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    miPickTexture: TMenuItem;
    DCTarget: TGLDummyCube;
    GLCamera: TGLCamera;
    DCAxis: TGLDummyCube;
    acFlatLined: TAction;
    ToolButton13: TToolButton;
    miFlatShadingwithlines: TMenuItem;
    acInvertNormals: TAction;
    miActions: TMenuItem;
    miInvertNormals: TMenuItem;
    N4: TMenuItem;
    miSaveas: TMenuItem;
    acReverseRenderingOrder: TAction;
    miReverseRenderingOrder: TMenuItem;
    acConvertToIndexedTriangles: TAction;
    miConverttoIndexedTriangles: TMenuItem;
    acFPS: TAction;
    miFPS: TMenuItem;
    GLCadencer: TGLCadencer;
    Timer: TTimer;
    GLLightmapLibrary: TGLMaterialLibrary;
    acSaveTextures: TAction;
    miSavetextures: TMenuItem;
    miOpenTexLib: TMenuItem;
    miOptimize: TMenuItem;
    N5: TMenuItem;
    acOptimize: TAction;
    miStripify: TMenuItem;
    acStripify: TAction;
    N6: TMenuItem;
    acLighting: TAction;
    Lighting1: TMenuItem;
    TBLighting: TToolButton;
    miMSAA8X: TMenuItem;
    miMSAA16X: TMenuItem;
    miCSAA8X: TMenuItem;
    miCSAA16X: TMenuItem;
    miHelp: TMenuItem;
    miTools: TMenuItem;
    acOption: TAction;
    GLSceneViewer: TGLSceneViewer;
    acShowAxes: TAction;
    procedure miAboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure acZoomInExecute(Sender: TObject);
    procedure acZoomOutExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acShadeSmoothExecute(Sender: TObject);
    procedure GLSceneViewerBeforeRender(Sender: TObject);
    procedure miAADefaultClick(Sender: TObject);
    procedure GLSceneViewerAfterRender(Sender: TObject);
    procedure acResetViewExecute(Sender: TObject);
    procedure acCullFaceExecute(Sender: TObject);
    procedure miBgColorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GLMaterialLibraryTextureNeeded(Sender: TObject;
      var textureFileName: String);
    procedure acTexturingExecute(Sender: TObject);
    procedure miPickTextureClick(Sender: TObject);
    procedure miFileClick(Sender: TObject);
    procedure acInvertNormalsExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acSaveAsUpdate(Sender: TObject);
    procedure acReverseRenderingOrderExecute(Sender: TObject);
    procedure acConvertToIndexedTrianglesExecute(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure acFPSExecute(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure acSaveTexturesExecute(Sender: TObject);
    procedure miOpenTexLibClick(Sender: TObject);
    procedure acOptimizeExecute(Sender: TObject);
    procedure acStripifyExecute(Sender: TObject);
    procedure acLightingExecute(Sender: TObject);
    procedure acOptionExecute(Sender: TObject);
    procedure acShowAxesExecute(Sender: TObject);
  private
    { Private declarations }
    procedure DoResetCamera;
    procedure SetupFreeFormShading;
    procedure ApplyShadeModeToMaterial(aMaterial : TGLMaterial);
    procedure ApplyShadeMode;
    procedure ApplyFSAA;
    procedure ApplyFaceCull;
    procedure ApplyTexturing;
    procedure ApplyFPS;

    procedure DoOpen(const fileName : String);
  public
    { Public declarations }
    md, nthShow : Boolean;
    mx, my : Integer;
    hlShader : TGLShader;
    lastFileName : String;
    lastLoadWithTextures : Boolean;
    procedure ApplyBgColor;
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

uses
   GLFileOBJ, GLFileSTL, GLFileLWO, GLFileQ3BSP, GLFileOCT, GLFileMS3D,
   GLFileNMF, GLFileMD3, GLFile3DS, GLFileMD2, GLFileSMD, GLFilePLY, GLFileGTS,
   GLFileVRML, GLFileMD5, GLFileTIN, GLFileDXF, GLFileGRD;

type

   // Hidden line shader (specific implem for the viewer, *not* generic)
   THiddenLineShader = class (TGLShader)
      private
         LinesColor : TColorVector;
         BackgroundColor : TColorVector;
         PassCount : Integer;
      public
         procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
         function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
   end;

procedure THiddenLineShader.DoApply(var rci : TRenderContextInfo; Sender : TObject);
begin
   PassCount:=1;
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

function THiddenLineShader.DoUnApply(var rci : TRenderContextInfo) : Boolean;
begin
   case PassCount of
      1 : with rci.GLStates do
      begin
         PassCount:=2;
         PolygonMode := pmLines;
         GL.Color3fv(@LinesColor);
         Disable(stLighting);
         Result:=True;
      end;
      2 :
      begin
         rci.GLStates.Disable(stPolygonOffsetFill);
         Result:=False;
      end;
   else
      // doesn't hurt to be cautious
      Assert(False);
      Result:=False;
   end;
end;

procedure TMain.FormCreate(Sender: TObject);
var
   reg : TRegistry;
   shellCmd : String;
   keyOkay : Boolean;
const
   cKeyName : String = 'Applications\GLSViewer.exe\shell\open\command';
   cFriendlyKeyName : String = 'Applications\GLSViewer.exe';
begin
   inherited;
   Main.WindowState := wsMaximized;
   // instantiate our specific hidden-lines shader
   hlShader:=THiddenLineShader.Create(Self);

   FreeForm.IgnoreMissingTextures:=True;

   // register as an application that handles arbitrary file classes
   try
      reg:=TRegistry.Create;
      try
         shellCmd:='"'+Application.ExeName+'" "%1"';
         reg.RootKey:=HKEY_CLASSES_ROOT;
         keyOkay:=False;
         if reg.OpenKeyReadOnly(cKeyName) then
            keyOkay:=(reg.ReadString('')=shellCmd);
         if not keyOkay then
         begin
            reg.CloseKey;
            if reg.OpenKey(cKeyName, True) then
               reg.WriteString('', shellCmd);
            reg.CloseKey;
            if reg.OpenKey(cFriendlyKeyName, True) then
               reg.WriteString('FriendlyAppName', 'GLSViewer, OpenGL 3D Files Viewer');
         end;
      finally
         reg.Free;
      end;
   except
      // ignore all registry issues (not critical)
   end;
end;

procedure TMain.FormShow(Sender: TObject);
var
   i : Integer;
begin
   if not nthShow then
   begin
      dmGLSViewer.OpenDialog.Filter:=VectorFileFormatsFilter;
      dmGLSViewer.SaveDialog.Filter:=VectorFileFormatsSaveFilter;
      with MainMenu do
      with ActionList do for i:=0 to ActionCount-1 do
         if Actions[i] is TCustomAction then
            with TCustomAction(Actions[i]) do Hint:=Caption;
      ApplyFSAA;
      ApplyFaceCull;
      ApplyBgColor;
      ApplyFPS;
      if ParamCount>0 then
         DoOpen(ParamStr(1));
      nthShow:=True;
   end;
end;

procedure TMain.GLSceneViewerBeforeRender(Sender: TObject);
begin
   THiddenLineShader(hlShader).LinesColor:=VectorMake(107/256, 123/256, 173/256, 1);
   THiddenLineShader(hlShader).BackgroundColor:=ConvertWinColor(GLSceneViewer.Buffer.BackgroundColor);
   if not GL.ARB_multisample then
   begin
      MIAADefault.Checked:=True;
      miMSAA2x.Enabled:=False;
      miMSAA4X.Enabled:=False;
      miMSAA8X.Enabled:=False;
      miMSAA16X.Enabled:=False;
      miCSAA8X.Enabled:=False;
      miCSAA16X.Enabled:=False;
   end;
end;

procedure TMain.GLSceneViewerAfterRender(Sender: TObject);
begin
   ApplyFSAA;
   Screen.Cursor:=crDefault;
end;

procedure TMain.miAboutClick(Sender: TObject);
begin
  with TGLAbout.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMain.DoResetCamera;
var
   objSize : Single;
begin
   DCTarget.Position.AsVector:=NullHmgPoint;
   GLCamera.Position.SetPoint(7, 3, 5);
   FreeForm.Position.AsVector:=NullHmgPoint;
   FreeForm.Up.Assign(DCAxis.Up);
   FreeForm.Direction.Assign(DCAxis.Direction);

   objSize:=FreeForm.BoundingSphereRadius;
   if objSize>0 then
   begin
      if objSize<1 then
      begin
         GLCamera.SceneScale:=1/objSize;
         objSize:=1;
      end
      else
         GLCamera.SceneScale:=1;
      GLCamera.AdjustDistanceToTarget(objSize*0.27);
      GLCamera.DepthOfView:=1.5*GLCamera.DistanceToTarget+2*objSize;
   end;
end;

procedure TMain.ApplyShadeModeToMaterial(aMaterial : TGLMaterial);
begin
   with aMaterial do
   begin
      if ACShadeSmooth.Checked then
      begin
         GLSceneViewer.Buffer.Lighting:=True;
         GLSceneViewer.Buffer.ShadeModel:=smSmooth;
         aMaterial.PolygonMode:=pmFill;
      end else if ACFlatShading.Checked then
      begin
         GLSceneViewer.Buffer.Lighting:=True;
         GLSceneViewer.Buffer.ShadeModel:=smFlat;
         aMaterial.PolygonMode:=pmFill;
      end else if ACFlatLined.Checked then
      begin
         GLSceneViewer.Buffer.Lighting:=True;
         GLSceneViewer.Buffer.ShadeModel:=smFlat;
         aMaterial.PolygonMode:=pmLines;
      end else if ACHiddenLines.Checked then
      begin
         GLSceneViewer.Buffer.Lighting:=False;
         GLSceneViewer.Buffer.ShadeModel:=smSmooth;
         aMaterial.PolygonMode:=pmLines;
      end else if ACWireframe.Checked then
      begin
         GLSceneViewer.Buffer.Lighting:=False;
         GLSceneViewer.Buffer.ShadeModel:=smSmooth;
         aMaterial.PolygonMode:=pmLines;
      end;
   end;
end;

procedure TMain.ApplyShadeMode;
var
   i : Integer;
begin
   with GLMaterialLibrary.Materials do for i:=0 to Count-1 do
   begin
      ApplyShadeModeToMaterial(Items[i].Material);
      if (ACHiddenLines.Checked) or (ACFlatLined.Checked) then
         Items[i].Shader:=hlShader
      else
         Items[i].Shader:=nil;
   end;
   GLSceneViewer.Buffer.Lighting:=ACLighting.Checked;
   FreeForm.StructureChanged;
end;

procedure TMain.ApplyFSAA;
begin
   with GLSceneViewer.Buffer do
   begin
      if MIAADefault.Checked then
         AntiAliasing:=aaDefault
      else if miMSAA2X.Checked then
         AntiAliasing:=aa2x
      else if miMSAA4X.Checked then
         AntiAliasing:=aa4x
      else if miMSAA8X.Checked then
         AntiAliasing:=aa8x
      else if miMSAA16X.Checked then
         AntiAliasing:=aa16x
      else if miCSAA8X.Checked then
         AntiAliasing:=csa8x
      else if miCSAA16X.Checked then
         AntiAliasing:=csa16x;
   end;
end;

procedure TMain.ApplyFaceCull;
begin
   with GLSceneViewer.Buffer do
   begin
      if ACCullFace.Checked then
      begin
         FaceCulling:=True;
         ContextOptions:=ContextOptions-[roTwoSideLighting];
      end else begin
         FaceCulling:=False;
         ContextOptions:=ContextOptions+[roTwoSideLighting];
      end;
   end;
end;

procedure TMain.ApplyBgColor;
var
   bmp : TBitmap;
   col : TColor;
begin
   bmp:=TBitmap.Create;
   try
      bmp.Width:=16;
      bmp.Height:=16;
      col:=ColorToRGB(dmGLSViewer.ColorDialog.Color);
      GLSceneViewer.Buffer.BackgroundColor:=col;
      with bmp.Canvas do
      begin
         Pen.Color:=col xor $FFFFFF;
         Brush.Color:=col;
         Rectangle(0, 0, 16, 16);
      end;
   finally
      bmp.Free;
   end;
end;

procedure TMain.ApplyTexturing;
var
   i : Integer;
begin
   with GLMaterialLibrary.Materials do for i:=0 to Count-1 do
   begin
      with Items[i].Material.Texture do
      begin
         if Enabled then
            Items[i].Tag:=Integer(True);
         Enabled:=Boolean(Items[i].Tag) and ACTexturing.Checked;
      end;
   end;
   FreeForm.StructureChanged;
end;

procedure TMain.ApplyFPS;
begin
   if ACFPS.Checked then
   begin
      Timer.Enabled:=True;
      GLCadencer.Enabled:=True;
   end else
   begin
      Timer.Enabled:=False;
      GLCadencer.Enabled:=False;
      StatusBar.Panels[1].Text:='--- FPS';
   end;
end;

procedure TMain.SetupFreeFormShading;
var
   i : Integer;
   libMat : TGLLibMaterial;
begin
   with GLMaterialLibrary do
   begin
      if Materials.Count=0 then
      begin
         FreeForm.Material.MaterialLibrary:=GLMaterialLibrary;
         libMat:=Materials.Add;
         FreeForm.Material.LibMaterialName:=libMat.Name;
         libMat.Material.FrontProperties.Diffuse.Red:=0;
      end;
      for i:=0 to Materials.Count-1 do
         with Materials[i].Material do BackProperties.Assign(FrontProperties);
   end;
   ApplyShadeMode;
   ApplyTexturing;
   ApplyFPS;
end;

procedure TMain.DoOpen(const fileName : String);
var
   min, max : TAffineVector;
begin
   if not FileExists(fileName) then Exit;

   Screen.Cursor:=crHourGlass;

   Caption:='GLSViewer - '+ExtractFileName(fileName);

   FreeForm.MeshObjects.Clear;
   GLMaterialLibrary.Materials.Clear;

   FreeForm.LoadFromFile(fileName);

   SetupFreeFormShading;

   StatusBar.Panels[0].Text:=IntToStr(FreeForm.MeshObjects.TriangleCount)+' tris';
   StatusBar.Panels[2].Text:=fileName;
   ACSaveTextures.Enabled:=(GLMaterialLibrary.Materials.Count>0);
   MIOpenTexLib.Enabled:=(GLMaterialLibrary.Materials.Count>0);
   lastFileName:=fileName;
   lastLoadWithTextures:=ACTexturing.Enabled;

   FreeForm.GetExtents(min, max);
   with CubeExtents do
   begin
      CubeWidth:=max.X-min.X;
      CubeHeight:=max.Y-min.Y;
      CubeDepth:=max.Z-min.Z;
      Position.AsAffineVector:=VectorLerp(min, max, 0.5);
   end;
   DoResetCamera;
end;

procedure TMain.acOpenExecute(Sender: TObject);
begin
   if dmGLSViewer.OpenDialog.Execute then
      DoOpen(dmGLSViewer.OpenDialog.FileName);
end;

procedure TMain.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx := X; my := Y;
   md := True;
end;

procedure TMain.GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
   d : Single;
begin
   if md and (Shift<>[]) then
   begin
      if ssLeft in Shift then
         if ssShift in Shift then
            GLCamera.MoveAroundTarget((my-y)*0.1, (mx-x)*0.1)
         else GLCamera.MoveAroundTarget(my-y, mx-x)
      else if ssRight in Shift then
      begin
         d:=GLCamera.DistanceToTarget*0.01*(x-mx+y-my);
         if IsKeyDown('x') then
            FreeForm.Translate(d, 0, 0)
         else if IsKeyDown('y') then
            FreeForm.Translate(0, d, 0)
         else if IsKeyDown('z') then
            FreeForm.Translate(0, 0, d)
         else
         begin
            if ssShift in Shift then
               GLCamera.RotateObject(FreeForm, (my-Y)*0.1, (mx-X)*0.1)
            else GLCamera.RotateObject(FreeForm, my-Y, mx-X);
         end;
      end;
      mx:=X; my:=Y;
   end;
end;

procedure TMain.GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   md := False;
end;

procedure TMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   if FreeForm.MeshObjects.Count>0 then
   begin
      GLCamera.AdjustDistanceToTarget(Power(1.05, WheelDelta/120));
      GLCamera.DepthOfView:=2*GLCamera.DistanceToTarget+2*FreeForm.BoundingSphereRadius;
   end;
   Handled := True;
end;

procedure TMain.acZoomInExecute(Sender: TObject);
var
   h : Boolean;
begin
   FormMouseWheel(Self, [], -120*4, Point(0, 0), h);
end;

procedure TMain.acZoomOutExecute(Sender: TObject);
var
   h : Boolean;
begin
   FormMouseWheel(Self, [], 120*4, Point(0, 0), h);
end;

procedure TMain.acExitExecute(Sender: TObject);
begin
   Close;
end;

procedure TMain.acShadeSmoothExecute(Sender: TObject);
begin
   ApplyShadeMode;
end;

procedure TMain.acShowAxesExecute(Sender: TObject);
begin
  DCAxis.ShowAxes := (Sender as TfmOptions).CheckBoxAxis.Checked;
end;

procedure TMain.miAADefaultClick(Sender: TObject);
begin
   (Sender as TMenuItem).Checked:=True;
   ApplyFSAA;
end;

procedure TMain.acResetViewExecute(Sender: TObject);
begin
   DoResetCamera;
end;

procedure TMain.acCullFaceExecute(Sender: TObject);
begin
   ACCullFace.Checked:=not ACCullFace.Checked;
   ApplyFaceCull;
end;

procedure TMain.miBgColorClick(Sender: TObject);
begin
   if dmGLSViewer.ColorDialog.Execute then
      ApplyBgColor;
end;

procedure TMain.GLMaterialLibraryTextureNeeded(Sender: TObject;
  var textureFileName: String);
begin
   if not ACTexturing.Enabled then
      textureFileName:='';
end;

procedure TMain.acTexturingExecute(Sender: TObject);
begin
   ACTexturing.Checked:=not ACTexturing.Checked;
   if ACTexturing.Checked then
      if lastLoadWithTextures then
         ApplyTexturing
      else begin
         DoOpen(lastFileName);
      end
   else ApplyTexturing;
end;

procedure TMain.miFileClick(Sender: TObject);
begin
   MIPickTexture.Enabled:=(GLMaterialLibrary.Materials.Count>0);
end;

procedure TMain.miPickTextureClick(Sender: TObject);
begin
   if dmGLSViewer.OpenPictureDialog.Execute then
   begin
      with GLMaterialLibrary.Materials do
      begin
         with Items[Count-1] do begin
            Tag:=1;
            Material.Texture.Image.LoadFromFile(dmGLSViewer.OpenPictureDialog.FileName);
            Material.Texture.Enabled:=True;
         end;
      end;
      ApplyTexturing;
   end;
end;

procedure TMain.miOpenTexLibClick(Sender: TObject);
var
   i : Integer;
begin
   if dmGLSViewer.ODTextures.Execute then with GLMaterialLibrary do
   begin
      LoadFromFile(dmGLSViewer.ODTextures.FileName);
      for i:=0 to Materials.Count-1 do
         with Materials[i].Material do BackProperties.Assign(FrontProperties);
      ApplyShadeMode;
      ApplyTexturing;
   end;
end;

procedure TMain.acInvertNormalsExecute(Sender: TObject);
var
   i : Integer;
begin
   with FreeForm.MeshObjects do
      for i:=0 to Count-1 do
         Items[i].Normals.Scale(-1);
   FreeForm.StructureChanged;
end;

procedure TMain.acReverseRenderingOrderExecute(Sender: TObject);
var
   i, j, n : Integer;
   fg : TFaceGroup;
begin
   with FreeForm.MeshObjects do
   begin
      // invert meshobjects order
      for i:=0 to (Count div 2) do
         Exchange(i, Count-1-i);
      // for each mesh object
      for i:=0 to Count-1 do with Items[i] do
      begin
         // invert facegroups order
         n:=FaceGroups.Count;
         for j:=0 to (n div 2) do
            Exchange(j, n-1-j);
         // for each facegroup
         for j:=0 to n-1 do begin
            fg:=FaceGroups[j];
            fg.Reverse;
         end;
      end;
   end;
   FreeForm.StructureChanged;
end;

procedure TMain.acSaveAsExecute(Sender: TObject);
var
   ext : String;
begin
   if dmGLSViewer.SaveDialog.Execute then
   begin
      ext:=ExtractFileExt(dmGLSViewer.SaveDialog.FileName);
      if ext='' then
         dmGLSViewer.SaveDialog.FileName:=ChangeFileExt(dmGLSViewer.SaveDialog.FileName,
            '.'+GetVectorFileFormats.FindExtByIndex(dmGLSViewer.SaveDialog.FilterIndex, False, True));
      if GetVectorFileFormats.FindFromFileName(dmGLSViewer.SaveDialog.FileName)=nil then
         ShowMessage(_('Unsupported or unspecified file extension.'))
      else FreeForm.SaveToFile(dmGLSViewer.SaveDialog.FileName);
   end;
end;

procedure TMain.acSaveAsUpdate(Sender: TObject);
begin
   ACSaveAs.Enabled:=(FreeForm.MeshObjects.Count>0);
end;

procedure TMain.acConvertToIndexedTrianglesExecute(Sender: TObject);
var
   v : TAffineVectorList;
   i : TIntegerList;
   m : TMeshObject;
   fg : TFGVertexIndexList;
begin
   v:=FreeForm.MeshObjects.ExtractTriangles;
   try
      i:=BuildVectorCountOptimizedIndices(v);
      try
         RemapAndCleanupReferences(v, i);
         IncreaseCoherency(i, 12);
         i.Capacity:=i.Count;
         FreeForm.MeshObjects.Clean;
         m:=TMeshObject.CreateOwned(FreeForm.MeshObjects);
         m.Vertices:=v;
         m.BuildNormals(i, momTriangles);
         m.Mode:=momFaceGroups;
         fg:=TFGVertexIndexList.CreateOwned(m.FaceGroups);
         fg.VertexIndices:=i;
         fg.Mode:=fgmmTriangles;
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

procedure TMain.acStripifyExecute(Sender: TObject);
var
   i : Integer;
   mo : TMeshObject;
   fg : TFGVertexIndexList;
   strips : TPersistentObjectList;
begin
   ACConvertToIndexedTriangles.Execute;
   mo:=FreeForm.MeshObjects[0];
   fg:=(mo.FaceGroups[0] as TFGVertexIndexList);
   strips:=StripifyMesh(fg.VertexIndices, mo.Vertices.Count, True);
   try
      fg.Free;
      for i:=0 to strips.Count-1 do
      begin
         fg:=TFGVertexIndexList.CreateOwned(mo.FaceGroups);
         fg.VertexIndices:=(strips[i] as TIntegerList);
         if i=0 then
            fg.Mode:=fgmmTriangles
         else fg.Mode:=fgmmTriangleStrip;
      end;
   finally
      strips.Free;
   end;
end;

procedure TMain.acOptimizeExecute(Sender: TObject);
begin
   OptimizeMesh(FreeForm.MeshObjects, [mooVertexCache, mooSortByMaterials]);
   FreeForm.StructureChanged;
   SetupFreeFormShading;
end;

procedure TMain.acOptionExecute(Sender: TObject);
begin
  with TfmOptions.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMain.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   if Self.Focused then
      GLSceneViewer.Invalidate;
end;

procedure TMain.acFPSExecute(Sender: TObject);
begin
   ACFPS.Checked:=not ACFPS.Checked;
   ApplyFPS;
end;

procedure TMain.acLightingExecute(Sender: TObject);
begin
   ACLighting.Checked:=not ACLighting.Checked;
//   TBLighting
   ApplyShadeMode;
end;

procedure TMain.TimerTimer(Sender: TObject);
begin
   StatusBar.Panels[1].Text:=Format('%.1f FPS', [GLSceneViewer.FramesPerSecond]);
   GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TMain.ReadIniFile;
begin
  inherited;
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  with IniFile do
    try
      Top  := ReadInteger(Name, 'Top', 100);
      Left := ReadInteger(Name, 'Left', 200);
      if ReadBool(Name, 'InitMax', False) then
        WindowState := wsMaximized
      else
        WindowState := wsNormal;
    finally
      IniFile.Free;
    end;
end;

procedure TMain.WriteIniFile;
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

procedure TMain.acSaveTexturesExecute(Sender: TObject);
begin
   if dmGLSViewer.SDTextures.Execute then
      GLMaterialLibrary.SaveToFile(dmGLSViewer.SDTextures.FileName);
end;

end.
