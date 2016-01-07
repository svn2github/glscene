//
// VKScene project based on GLScene library, http://glscene.sourceforge.net 
//
{
  Registration unit for GLScene library components, property editors and
  IDE experts. 
  
}
unit VKS.SceneRegister;

interface

{$I VKScene.inc}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Graphics,


  { TODO : F1026 Files not found: 'ToolsAPI' etc.}
  (*need to create instead a custom PropertyEditor like it described in -> *)
  (*ms-help://embarcadero.rs_xe7/rad/Creating_a_Component_Editor_and_a_Property_Editor_for_FireMonkey_Components.html*)
  (*
  ToolsAPI,
  DesignIntf,
  DesignEditors,
  VCLEditors,
  *)

  VKS.Scene,
  VKS.Context,
  VKS.Color,
  VKS.CrossPlatform,
  VKS.ObjectManager;

type
  // TVKLibMaterialNameProperty
  //
  TVKLibMaterialNameProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TVKSceneViewerEditor
  //
  TVKSceneViewerEditor = class(TComponentEditor)
  public
    { Public Declarations }
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  // TVKSceneEditor
  //
  TVKSceneEditor = class(TComponentEditor)
  public
    { Public Declarations }
    procedure Edit; override;

    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  // TResolutionProperty
  //
  TResolutionProperty = class(TPropertyEditor)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  // TClassProperty
  //
  TVKTextureProperty = class(TClassProperty)
  public
    { Protected Declarations }
    function GetAttributes: TPropertyAttributes; override;
  end;

  // TVKTextureImageProperty
  //
  TVKTextureImageProperty = class(TClassProperty)
  public
    { Protected Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TVKImageClassProperty
  //
  TVKImageClassProperty = class(TClassProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  // TVKColorProperty
  //
  TVKColorProperty = class(TClassProperty, ICustomPropertyDrawing,
    ICustomPropertyListDrawing)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    function ColorToBorderColor(aColor: TColorVector;
      selected: Boolean): TColor;

  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Edit; override;

    // ICustomPropertyListDrawing  stuff
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TVKRect; ASelected: Boolean);
    // CustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TVKRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TVKRect;
      ASelected: Boolean);
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  // TSoundFileProperty
  //
  TSoundFileProperty = class(TClassProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  // TSoundNameProperty
  //
  TSoundNameProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  // TVKCoordinatesProperty
  //
  TVKCoordinatesProperty = class(TClassProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TVKMaterialProperty
  //
  TVKMaterialProperty = class(TClassProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TVKGUILayoutEditor
  //
  TVKGUILayoutEditor = class(TComponentEditor)
  public
    { Public Declarations }
    procedure Edit; override;

    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  // TReuseableDefaultEditor
  //
  { Editor copied from DsgnIntf. 
    Could have been avoided, if only that guy at Borland didn't chose to
    publish only half of the stuff (and that's not the only class with
    that problem, most of the subitems handling code in TVKSceneBaseObject is
    here for the same reason...), the "protected" wasn't meant just to lure
    programmers into code they can't reuse... Arrr! and he did that again
    in D6! Grrr... }
  TReuseableDefaultEditor = class(TComponentEditor, IDefaultEditor)
  protected
    { Protected Declarations }
    FFirst: IProperty;
    FBest: IProperty;
    FContinue: Boolean;
    procedure CheckEdit(const Prop: IProperty);
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); virtual;

  public
    { Public Declarations }
    procedure Edit; override;
  end;

  // TVKMaterialLibraryEditor
  //
  { Editor for material library.  }
  TVKMaterialLibraryEditor = class(TReuseableDefaultEditor, IDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  // TVKAnimationNameProperty
  //
  TVKAnimationNameProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { Selection editor for TVKSoundLibrary. 
    Allows units to be added to the uses clause automatically when
    sound files are loaded into a TVKSoundLibrary at design-time. }
  TVKSoundLibrarySelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { Selection editor for TVKBaseSceneObject. 
    Allows units to be added to the uses clause automatically when
    behaviours/effects are added to a TVKBaseSceneObject at design-time. }
  TVKBaseSceneObjectSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  // TVKSArchiveManagerEditor
  //
  { Editor for GLScene Archive Manager.  }
  TVKSArchiveManagerEditor = class(TReuseableDefaultEditor, IDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  // TVKMaterialComponentNameProperty
  //

  TVKMaterialComponentNameProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TVKLibTextureNameProperty = class(TVKMaterialComponentNameProperty)
  public
    { Public Declarations }
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVKLibSamplerNameProperty = class(TVKMaterialComponentNameProperty)
  public
    { Public Declarations }
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVKLibCombinerNameProperty = class(TVKMaterialComponentNameProperty)
  public
    { Public Declarations }
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVKLibShaderNameProperty = class(TVKMaterialComponentNameProperty)
  public
    { Public Declarations }
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVKLibAttachmentNameProperty = class(TVKMaterialComponentNameProperty)
  public
    { Public Declarations }
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVKLibAsmProgNameProperty = class(TVKMaterialComponentNameProperty)
  public
    { Public Declarations }
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  // TPictureFileProperty
  //
  TPictureFileProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TShaderFileProperty
  //
  TShaderFileProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TAsmProgFileProperty
  //
  TAsmProgFileProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TUniformAutoSetProperty
  //
  TUniformAutoSetProperty = class(TPropertyEditor)
  private
    procedure PassUniform(const S: string);
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TVKShaderEditorProperty = class(TClassProperty)
  protected
    { Protected declarations }
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
    procedure OnShaderCheck(Sender: TObject);
  public
    { Public declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

resourcestring
  { OpenGL property category }
  sOpenGLCategoryName = 'OpenGL';

procedure Register;

// : Auto-create for object manager
function ObjectManager: TObjectManager;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  FLibMaterialPicker,
  FGUILayoutEditor,
  FMaterialEditorForm,
  FShaderMemo,
  FShaderUniformEditor,
  FVectorEditor,
  VKS.AnimatedSprite,
  VKS.ApplicationFileIO,
  VKS.AsmShader,
  VKS.AsyncHDS,
  VKS.AsyncTimer,
  VKS.Atmosphere,
  VKS.AVIRecorder,
  VKS.BaseClasses,
  VKS.BitmapFont,
  VKS.Blur,
  VKS.BumpmapHDS,
  VKS.BumpShader,
  VKS.Cadencer,
  VKS.CameraController,
  VKS.CelShader,
  VKS.Collision,
  VKS.CompositeImage,
  VKS.Console,
  VKS.Coordinates,
  VKS.DCE,
  VKS.DynamicTexture,
  VKS.EParticleMasksManager,
  VKS.ExplosionFx,
  VKS.Extrusion,
  VKS.FBORenderer,
  VKS.Feedback,
  VKS.FireFX,
  VKS.FPSMovement,
  VKS.GameMenu,
  VKS.GeomObjects,
  VKS.Gizmo,
  VKS.Graph,
  VKS.Graphics,
  VKS.Gui,
  VKS.HeightData,
  VKS.HeightTileFileHDS,
  VKS.HiddenLineShader,
  VKS.HUDObjects,
  VKS.Imposter,
  VKS.LensFlare,
  VKS.LinePFX,
  VKS.Material,
  VKS.MaterialEx,
  VKS.MaterialMultiProxy,
  VKS.MaterialScript,
  VKS.Mesh,
  VKS.Mirror,
  VKS.MultiMaterialShader,
  VKS.MultiPolygon,
  VKS.MultiProxy,
  VKS.Navigator,
  VKS.Nodes,
  VKS.Objects,
  VKS.OutlineShader,
  VKS.ParticleFX,
  VKS.Particles,
  VKS.Perlin,
  VKS.PerlinPFX,
  VKS.PhongShader,
  VKS.Polyhedron,
  VKS.Portal,
  VKS.PostEffects,
  VKS.ProjectedTextures,
  VKS.ProxyObjects,
  VKS.RenderContextInfo,
  VKS.ArchiveManager,
  VKS.SceneEdit,
  VKS.Screen,
  VKS.ScriptBase,
  VKS.ShaderCombiner,
  VKS.ShadowHDS,
  VKS.ShadowPlane,
  VKS.ShadowVolume,
  VKS.SimpleNavigation,
  VKS.SkyBox,
  VKS.Skydome,
  VKS.Language,
  GLSL.BumpShader,
  GLSL.DiffuseSpecularShader,
  GLSL.PostBlurShader,
  GLSL.ProjectedTextures,
  GLSL.Shader,
  VKS.SmoothNavigator,
  VKS.SMWaveOut,
  VKS.State,
  VKS.Strings,
  VKS.Teapot,
  VKS.TerrainRenderer,
  VKS.TexCombineShader,
  VKS.TexLensFlare,
  VKS.Texture,
  VKS.TexturedHDS,
  VKS.TextureImageEditors,
  VKS.TextureSharingShader,
  VKS.ThorFX,
  VKS.TilePlane,
  VKS.TimeEventsMgr,
  VKS.Trail,
  VKS.Tree,
  VKS.Types,
  VKS.FileTIN,
  VKS.UserShader,
  VKS.Utils,
  VKS.VectorFileObjects,
  VKS.VfsPAK,
  VKS.WaterPlane,
  VKS.Windows,
  VKS.WindowsFont,
  VKS.zBuffer,
  VKS.VectorTypes,
  VKS.VectorGeometry,
  // Image file formats
  DDSImage,
  TGA,
  // Vector file formats
  VKS.File3DS,
  VKS.FileASE,
  VKS.FileB3D,
  VKS.FileGL2,
  VKS.FileGTS,
  VKS.FileLMTS,
  VKS.FileLWO,
  VKS.FileMD2,
  VKS.FileMD3,
  VKS.FileMD5,
  VKS.FileMDC,
  VKS.FileMS3D,
  VKS.FileNMF,
  VKS.FileNurbs,
  VKS.FileObj,
  VKS.FileOCT,
  VKS.FilePLY,
  VKS.FileQ3BSP,
  VKS.FileSMD,
  VKS.FileSTL,
  VKS.FileVRML,

  // Sound file formats
  VKS.FileWAV,
  VKS.FileMP3,

  // Raster file format
  VKS.FileDDS,
  VKS.FileO3TC,
  VKS.FileHDR,
  VKS.FileJPEG,
  VKS.FilePNG,
  VKS.FileBMP,
  VKS.FileTGA,

  VKS.Sound,
  VKS.SoundFileObjects,
  VKS.SpaceText,
  VKS.Joystick,
  VKS.ScreenSaver,
  VKS.FullScreenViewer,
  VKS.Log;

var
  vObjectManager: TObjectManager;

function ObjectManager: TObjectManager;
begin
  if not Assigned(vObjectManager) then
    vObjectManager := TObjectManager.Create(nil);
  Result := vObjectManager;
end;

{$IFDEF VKS_REGION}{$REGION 'TOpenGLCategory'}{$ENDIF}
{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKSceneViewerEditor'}{$ENDIF}
// ExecuteVerb
//

procedure TVKSceneViewerEditor.ExecuteVerb(Index: Integer);
var
  source: TVKSceneViewer;
begin
  source := Component as TVKSceneViewer;
  case Index of
    0:
      source.Buffer.ShowInfo;
  end;
end;

// GetVerb
//

function TVKSceneViewerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show context info';
  end;
end;

// GetVerbCount
//

function TVKSceneViewerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKSceneEditor'}{$ENDIF}
// Edit
//

procedure TVKSceneEditor.Edit;
begin
  with GLSceneEditorForm do
  begin
    SetScene(Self.Component as TVKScene, Self.Designer);
    Show;
  end;
end;

// ExecuteVerb
//

procedure TVKSceneEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

// GetVerb
//

function TVKSceneEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Scene Editor';
  end;
end;

// GetVerbCount
//

function TVKSceneEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TResolutionProperty'}{$ENDIF}
// GetAttributes
//

function TResolutionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValue
//

function TResolutionProperty.GetValue: string;
begin
  Result := vVideoModes[GetOrdValue].Description;
end;

// GetValues
//

procedure TResolutionProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := 0 to vNumberVideoModes - 1 do
    Proc(vVideoModes[i].Description);
end;

// SetValue
//

procedure TResolutionProperty.SetValue(const Value: string);

const
  Nums = ['0' .. '9'];

var
  XRes, YRes, BPP: Integer;
  Pos, SLength: Integer;
  TempStr: string;

begin
  if CompareText(Value, 'default') <> 0 then
  begin
    // initialize scanning
    TempStr := Trim(Value) + '|'; // ensure at least one delimiter
    SLength := Length(TempStr);
    XRes := 0;
    YRes := 0;
    BPP := 0;
    // contains the string something?
    if SLength > 1 then
    begin
      // determine first number
      for Pos := 1 to SLength do
        if not(AnsiChar(TempStr[Pos]) in Nums) then
          Break;
      if Pos <= SLength then
      begin
        // found a number?
        XRes := StrToInt(Copy(TempStr, 1, Pos - 1));
        // search for following non-numerics
        for Pos := Pos to SLength do
          if AnsiChar(TempStr[Pos]) in Nums then
            Break;
        Delete(TempStr, 1, Pos - 1); // take it out of the String
        SLength := Length(TempStr); // rest length of String
        if SLength > 1 then // something to scan?
        begin
          // determine second number
          for Pos := 1 to SLength do
            if not(AnsiChar(TempStr[Pos]) in Nums) then
              Break;
          if Pos <= SLength then
          begin
            YRes := StrToInt(Copy(TempStr, 1, Pos - 1));
            // search for following non-numerics
            for Pos := Pos to SLength do
              if AnsiChar(TempStr[Pos]) in Nums then
                Break;
            Delete(TempStr, 1, Pos - 1); // take it out of the String
            SLength := Length(TempStr); // rest length of String
            if SLength > 1 then
            begin
              for Pos := 1 to SLength do
                if not(AnsiChar(TempStr[Pos]) in Nums) then
                  Break;
              if Pos <= SLength then
                BPP := StrToInt(Copy(TempStr, 1, Pos - 1));
            end;
          end;
        end;
      end;
    end;
    SetOrdValue(GetIndexFromResolution(XRes, YRes, BPP));
  end
  else
    SetOrdValue(0);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKTextureProperty'}{$ENDIF}

function TVKTextureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties];
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKTextureImageProperty'}{$ENDIF}
// GetAttributes
//

function TVKTextureImageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// Edit
//

procedure TVKTextureImageProperty.Edit;
begin
  if EditGLTextureImage(TVKTextureImage(GetOrdValue)) then
    Designer.Modified;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKImageClassProperty'}{$ENDIF}
// GetAttributes
//

function TVKImageClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValues
//

procedure TVKImageClassProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  sl: TStrings;
begin
  sl := GetGLTextureImageClassesAsStrings;
  try
    for i := 0 to sl.Count - 1 do
      Proc(sl[i]);
  finally
    sl.Free;
  end;
end;

// GetValue
//

function TVKImageClassProperty.GetValue: string;
begin
  Result := FindGLTextureImageClass(GetStrValue).FriendlyName;
end;

// SetValue
//

procedure TVKImageClassProperty.SetValue(const Value: string);
var
  tic: TVKTextureImageClass;
begin
  tic := FindGLTextureImageClassByFriendlyName(Value);
  if Assigned(tic) then
    SetStrValue(tic.ClassName)
  else
    SetStrValue('');
  Modified;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKColorProperty'}{$ENDIF}

procedure TVKColorProperty.Edit;
var
  colorDialog: TColorDialog;
  VKS.Color: TVKColor;
begin
  colorDialog := TColorDialog.Create(nil);
  try
    VKS.Color := TVKColor(GetOrdValue);
{$IFDEF WIN32}
    colorDialog.Options := [cdFullOpen];
{$ENDIF}
    colorDialog.Color := ConvertColorVector(VKS.Color.Color);
    if colorDialog.Execute then
    begin
      VKS.Color.Color := ConvertWinColor(colorDialog.Color);
      Modified;
    end;
  finally
    colorDialog.Free;
  end;
end;

function TVKColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paValueList, paDialog];
end;

procedure TVKColorProperty.GetValues(Proc: TGetStrProc);
begin
  ColorManager.EnumColors(Proc);
end;

function TVKColorProperty.GetValue: string;
begin
  Result := ColorManager.GetColorName(TVKColor(GetOrdValue).Color);
end;

procedure TVKColorProperty.SetValue(const Value: string);
begin
  TVKColor(GetOrdValue).Color := ColorManager.GetColor(Value);
  Modified;
end;

// ColorToBorderColor
//

function TVKColorProperty.ColorToBorderColor(aColor: TColorVector;
  selected: Boolean): TColor;
begin
  if (aColor.V[0] > 0.75) or (aColor.V[1] > 0.75) or (aColor.V[2] > 0.75) then
    Result := clBlack
  else if selected then
    Result := clWhite
  else
    Result := ConvertColorVector(aColor);
end;

procedure TVKColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TVKColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  vRight: Integer;
  vOldPenColor, vOldBrushColor: TColor;
  Color: TColorVector;
begin
  vRight := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
    try
      vOldPenColor := Pen.Color;
      vOldBrushColor := Brush.Color;

      Pen.Color := Brush.Color;
      Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

      Color := ColorManager.GetColor(Value);
      Brush.Color := ConvertColorVector(Color);
      Pen.Color := ColorToBorderColor(Color, ASelected);

      Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

      Brush.Color := vOldBrushColor;
      Pen.Color := vOldPenColor;
    finally
      DefaultPropertyListDrawValue(Value, ACanvas,
        Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom), ASelected);
    end;
end;

procedure TVKColorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M');
end;

procedure TVKColorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // Nothing
end;

procedure TVKColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TSoundFileProperty'}{$ENDIF}
// GetAttributes
//

function TSoundFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// GetValue
//

function TSoundFileProperty.GetValue: string;
var
  sample: TVKSoundSample;
begin
  sample := GetComponent(0) as TVKSoundSample;
  if sample.Data <> nil then
    Result := '(' + sample.Data.ClassName + ')'
  else
    Result := '(empty)';
end;

// Edit
//

procedure TSoundFileProperty.Edit;
var
  ODialog: TOpenDialog;
  sample: TVKSoundSample;
  Desc, F: string;
begin
  sample := GetComponent(0) as TVKSoundSample;
  ODialog := TOpenDialog.Create(nil);
  try
    GetGLSoundFileFormats.BuildFilterStrings(TVKSoundFile, Desc, F);
    ODialog.Filter := Desc;
    if ODialog.Execute then
    begin
      sample.LoadFromFile(ODialog.FileName);
      Modified;
    end;
  finally
    ODialog.Free;
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TSoundNameProperty'}{$ENDIF}
// GetAttributes
//

function TSoundNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValues
//

procedure TSoundNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  source: TVKBaseSoundSource;
begin
  source := (GetComponent(0) as TVKBaseSoundSource);
  if Assigned(source.SoundLibrary) then
    with source.SoundLibrary do
      for i := 0 to Samples.Count - 1 do
        Proc(Samples[i].Name);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKCoordinatesProperty'}{$ENDIF}
// GetAttributes
//

function TVKCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

// Edit;
//

procedure TVKCoordinatesProperty.Edit;
var
  glc: TVKCoordinates;
  x, y, z: Single;
begin
  glc := TVKCoordinates(GetOrdValue);
  x := glc.x;
  y := glc.y;
  z := glc.z;
  if VectorEditorForm.Execute(x, y, z) then
  begin
    glc.AsVector := VectorMake(x, y, z);
    Modified;
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKMaterialProperty'}{$ENDIF}
// GetAttributes
//

function TVKMaterialProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

// Edit
//

procedure TVKMaterialProperty.Edit;
begin
  if FMaterialEditorForm.MaterialEditorForm.Execute(TVKMaterial(GetOrdValue))
  then
    Modified;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKGUILayoutEditor'}{$ENDIF}

procedure TVKGUILayoutEditor.Edit;
begin
  GUILayoutEditorForm.Execute(TVKGuiLayout(Self.Component));
end;

procedure TVKGUILayoutEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

function TVKGUILayoutEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Layout Editor';
  end;
end;

function TVKGUILayoutEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TReuseableDefaultEditor'}{$ENDIF}
// CheckEdit
//

procedure TReuseableDefaultEditor.CheckEdit(const Prop: IProperty);
begin
  if FContinue then
    EditProperty(Prop, FContinue);
end;

// EditProperty
//
procedure TReuseableDefaultEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
var
  PropName: string;
  BestName: string;
  MethodProperty: IMethodProperty;

  procedure ReplaceBest;
  begin
    FBest := Prop;
    if FFirst = FBest then
      FFirst := nil;
  end;

begin
  if not Assigned(FFirst) and Supports(Prop, IMethodProperty, MethodProperty)
  then
    FFirst := Prop;
  PropName := Prop.GetName;
  BestName := '';
  if Assigned(FBest) then
    BestName := FBest.GetName;
  if CompareText(PropName, 'ONCREATE') = 0 then
    ReplaceBest
  else if CompareText(BestName, 'ONCREATE') <> 0 then
    if CompareText(PropName, 'ONCHANGE') = 0 then
      ReplaceBest
    else if CompareText(BestName, 'ONCHANGE') <> 0 then
      if CompareText(PropName, 'ONCLICK') = 0 then
        ReplaceBest;
end;

// Edit
//
procedure TReuseableDefaultEditor.Edit;
var
  Components: IDesignerSelections;
begin
  Components := TDesignerSelections.Create;
  FContinue := True;
  Components.Add(Component);
  FFirst := nil;
  FBest := nil;
  try
    GetComponentProperties(Components, tkAny, Designer, CheckEdit);
    if FContinue then
      if Assigned(FBest) then
        FBest.Edit
      else if Assigned(FFirst) then
        FFirst.Edit;
  finally
    FFirst := nil;
    FBest := nil;
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKMaterialLibraryEditor'}{$ENDIF}

// EditProperty
//
procedure TVKMaterialLibraryEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
begin
  if CompareText(Prop.GetName, 'MATERIALS') = 0 then
  begin
    FBest := Prop;
  end;
end;

procedure TVKMaterialLibraryEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

function TVKMaterialLibraryEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Material Library Editor';
  end;
end;

function TVKMaterialLibraryEditor.GetVerbCount: Integer;
begin
  Result := 1
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKLibMaterialNameProperty'}{$ENDIF}
// GetAttributes
//

function TVKLibMaterialNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// Edit
//

procedure TVKLibMaterialNameProperty.Edit;
var
  buf: string;
  ml: TVKAbstractMaterialLibrary;
  obj: TPersistent;
  Int: IGLMaterialLibrarySupported;
begin
  buf := GetStrValue;
  obj := GetComponent(0);
  if Supports(obj, IGLMaterialLibrarySupported, Int) then
    ml := Int.GetMaterialLibrary
  else
  begin
    ml := nil;
    Assert(False, 'oops, unsupported...');
  end;
  if not Assigned(ml) then
    ShowMessage('Select the material library first.')
  else if LibMaterialPicker.Execute(buf, ml) then
    SetStrValue(buf);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKAnimationNameProperty'}{$ENDIF}
// GetAttributes
//

function TVKAnimationNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValues
//

procedure TVKAnimationNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  animControler: TVKAnimationControler;
  actor: TVKActor;
begin
  animControler := (GetComponent(0) as TVKAnimationControler);
  if Assigned(animControler) then
  begin
    actor := animControler.actor;
    if Assigned(actor) then
      with actor.Animations do
      begin
        for i := 0 to Count - 1 do
          Proc(Items[i].Name);
      end;
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKBaseSceneObjectSelectionEditor'}{$ENDIF}

procedure TVKBaseSceneObjectSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  i, j: Integer;
  comp: TVKBaseSceneObject;
begin
  if (Designer = nil) or (Designer.Root = nil) then
    Exit;

  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if (Designer.Root.Components[i] is TVKBaseSceneObject) then
    begin
      comp := TVKBaseSceneObject(Designer.Root.Components[i]);
      for j := 0 to comp.Behaviours.Count - 1 do
        Proc(FindUnitName(comp.Behaviours[j]));
      for j := 0 to comp.Effects.Count - 1 do
        Proc(FindUnitName(comp.Effects[j]));
    end;
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKSoundLibrarySelectionEditor'}{$ENDIF}

procedure TVKSoundLibrarySelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  i, j: Integer;
  comp: TVKSoundLibrary;
begin
  if (Designer = nil) or (Designer.Root = nil) then
    Exit;

  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if (Designer.Root.Components[i] is TVKSoundLibrary) then
    begin
      comp := TVKSoundLibrary(Designer.Root.Components[i]);
      for j := 0 to comp.Samples.Count - 1 do
        if Assigned(comp.Samples[j].Data) then
          Proc(FindUnitName(comp.Samples[j].Data));
    end;
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKSArchiveManagerEditor'}{$ENDIF}

procedure TVKSArchiveManagerEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
begin
  if CompareText(Prop.GetName, 'ARCHIVES') = 0 then
  begin
    FBest := Prop;
  end;
end;

procedure TVKSArchiveManagerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

function TVKSArchiveManagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Archive Manager Editor';
  end;
end;

function TVKSArchiveManagerEditor.GetVerbCount: Integer;
begin
  Result := 1
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKMaterialComponentNameProperty'}{$ENDIF}

procedure TVKMaterialComponentNameProperty.Edit;
var
  LOwner: IGLMaterialLibrarySupported;
  LItem: TVKBaseMaterialCollectionItem;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    LItem := TVKMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .Components.GetItemByName(GetStrValue);
    if Assigned(LItem) then
      Designer.SelectComponent(LItem);
    Modified;
  end;
end;

function TVKMaterialComponentNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TVKLibTextureNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    TVKMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TVKTextureImageEx);
    TVKMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TVKFrameBufferAttachment);
  end;
end;

procedure TVKLibSamplerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TVKMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TVKTextureSampler);
end;

procedure TVKLibCombinerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TVKMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TVKTextureCombiner);
end;

procedure TVKLibShaderNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TVKMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc, TVKShaderEx);
end;

procedure TVKLibAttachmentNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TVKMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TVKFrameBufferAttachment);
end;

procedure TVKLibAsmProgNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TVKMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TVKASMVertexProgram);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TPictureFileProperty'}{$ENDIF}

function TPictureFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TPictureFileProperty.Edit;
var
  LFileName: string;
begin
  if OpenPictureDialog(LFileName) then
  begin
    SetStrValue(RelativePath(LFileName));
  end;
  Modified;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TShaderFileProperty'}{$ENDIF}

procedure TShaderFileProperty.Edit;
var
  ODialog: TOpenDialog;
begin
  ODialog := TOpenDialog.Create(nil);
  try
    ODialog.Filter := '*.glsl';
    if ODialog.Execute then
    begin
      SetStrValue(RelativePath(ODialog.FileName));
      Modified;
    end;
  finally
    ODialog.Free;
  end;
end;

function TShaderFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TAsmProgFileProperty'}{$ENDIF}

procedure TAsmProgFileProperty.Edit;
var
  ODialog: TOpenDialog;
begin
  ODialog := TOpenDialog.Create(nil);
  try
    ODialog.Filter := '*.asm';
    if ODialog.Execute then
    begin
      SetStrValue(RelativePath(ODialog.FileName));
      Modified;
    end;
  finally
    ODialog.Free;
  end;
end;

function TAsmProgFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TUniformAutoSetProperty'}{$ENDIF}

function TUniformAutoSetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paFullWidthName];
end;

procedure TUniformAutoSetProperty.PassUniform(const S: string);
begin
  ShaderUniformEditor.AddUniform(TVKBaseShaderModel(GetComponent(0))
    .Uniforms[S]);
end;

procedure TUniformAutoSetProperty.Edit;
var
  LOwner: TVKBaseShaderModel;
begin
  LOwner := TVKBaseShaderModel(GetComponent(0));
  if LOwner.Enabled and LOwner.IsValid then
  begin
    with ShaderUniformEditor do
    begin
      Clear;
      LOwner.MaterialLibrary.GetNames(AddTextureName, TVKTextureImageEx);
      LOwner.MaterialLibrary.GetNames(AddTextureName, TVKFrameBufferAttachment);
      LOwner.MaterialLibrary.GetNames(AddSamplerName, TVKTextureSampler);
      LOwner.GetUniformNames(PassUniform);
      Execute;
    end;
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TVKShaderEditorProperty'}{$ENDIF}

function TVKShaderEditorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

function TVKShaderEditorProperty.GetStrings: TStrings;
begin
  Result := TStrings(GetOrdValue);
end;

procedure TVKShaderEditorProperty.OnShaderCheck(Sender: TObject);
var
  LShader: TVKShaderEx;
  LContext: TVKContext;
begin
  SetStrings(GLShaderEditorForm.GLSLMemo.Lines);
  LShader := TVKShaderEx(GetComponent(0));
  LContext := LShader.Handle.RenderingContext;
  if Assigned(LContext) then
  begin
    LContext.Activate;
    try
      LShader.DoOnPrepare(LContext);
      GLShaderEditorForm.CompilatorLog.Lines.Add(LShader.InfoLog);
    finally
      LContext.Deactivate;
    end;
  end
  else
    GLShaderEditorForm.CompilatorLog.Lines.Add
      ('There is no any rendering context for work with OpenGL');
end;

procedure TVKShaderEditorProperty.SetStrings(const Value: TStrings);
begin
  SetOrdValue(Longint(Value));
end;

procedure TVKShaderEditorProperty.Edit;
begin
  with GLShaderEditorForm do
  begin
    OnCheck := OnShaderCheck;
    GLSLMemo.Lines.Assign(GetStrings);
    GLSLMemo.CurX := 0;
    GLSLMemo.CurY := 0;
    if ShowModal = mrOk then
    begin
      SetStrings(GLSLMemo.Lines);
      Modified;
    end;
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
// ******************************************************************************

procedure GLRegisterPropertiesInCategories;
begin

  { GL.SceneViewer }
  // property types
{$IFDEF WIN32}
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(TVKCamera), TypeInfo(TVKSceneBuffer), TypeInfo(TVSyncMode),
    TypeInfo(TVKScreenDepth)]); // TVKScreenDepth in GLWin32FullScreenViewer
{$ENDIF}
  // TVKSceneViewer
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKSceneViewer,
    ['*Render']);

  { VKS.Scene }
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(TVKx.ObjectsSorting), TypeInfo(TVKProgressEvent),
    TypeInfo(TVKBehaviours), TypeInfo(TVKObjectEffects),
    TypeInfo(TDirectRenderEvent), TypeInfo(TVKCameraStyle),
    TypeInfo(TOnCustomPerspective), TypeInfo(TVKScene)]);
  RegisterPropertiesInCategory(sLayoutCategoryName,
    [TypeInfo(TVKx.ObjectsSorting), TypeInfo(TNormalDirection)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TVKVisibilityCulling), TypeInfo(TLightStyle), TypeInfo(TVKColor),
    TypeInfo(TNormalDirection), TypeInfo(TVKCameraStyle)]);
  // TVKBaseSceneObject
  RegisterPropertiesInCategory(sVisualCategoryName, TVKBaseSceneObject,
    ['Rotation', 'Direction', 'Position', 'Up', 'Scale', '*Angle', 'ShowAxes',
    'FocalLength']);
  // TVKSceneObject
  RegisterPropertiesInCategory(sVisualCategoryName, TVKSceneObject, ['Parts']);
  // TVKDirectOpenGL
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKDirectOpenGL,
    ['UseBuildList']);
  // TVKProxyObject
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(TVKProxyObjectOptions)]);
  // TVKLightSource
  RegisterPropertiesInCategory(sVisualCategoryName, TVKLightSource,
    ['*Attenuation', 'Shining', 'Spot*']);
  // TVKCamera
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKCamera,
    ['TargetObject']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKCamera,
    ['DepthOfView', 'SceneScale']);
  // TVKNonVisualViewer
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKNonVisualViewer,
    ['*Render']);

  { VKS.Objects }
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(TVKLinesNodes), TypeInfo(TLineNodesAspect),
    TypeInfo(TLineSplineMode), TypeInfo(TLinesOptions)]);
{$IFDEF WIN32} // unit GLSpaceText
  RegisterPropertiesInCategory(sLayoutCategoryName, [TypeInfo(TVKTextAdjust)]);
  RegisterPropertiesInCategory(sLocalizableCategoryName,
    [TypeInfo(TSpaceTextCharRange)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TLineSplineMode),
    TypeInfo(TCapType), TypeInfo(TNormalSmoothing),
    TypeInfo(TArrowHeadStackingStyle), TypeInfo(TVKTextAdjust)]);
{$ENDIF}
  // TVKDummyCube
  RegisterPropertiesInCategory(sLayoutCategoryName, TVKDummyCube,
    ['VisibleAtRunTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKDummyCube,
    ['CubeSize', 'VisibleAtRunTime']);
  // TVKPlane
  RegisterPropertiesInCategory(sVisualCategoryName, TVKPlane,
    ['*Offset', '*Tiles']);
  // TVKSprite
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKSprite, ['NoZWrite']);
  RegisterPropertiesInCategory(sLayoutCategoryName, TVKSprite, ['NoZWrite']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKSprite,
    ['AlphaChannel', 'Rotation']);
  // TVKNode
  RegisterPropertiesInCategory(sVisualCategoryName, TVKNode, ['X', 'Y', 'Z']);
  // TVKLines
  RegisterPropertiesInCategory(sVisualCategoryName, TVKLines,
    ['Antialiased', 'Division', 'Line*', 'NodeSize']);
  // TVKCube
  RegisterPropertiesInCategory(sVisualCategoryName, TVKCube, ['Cube*']);
  // TVKFrustrum
  RegisterPropertiesInCategory(sVisualCategoryName, TVKFrustrum,
    ['ApexHeight', 'Base*']);
  // TVKSpaceText
{$IFDEF WIN32} // unit GLSpaceText
  RegisterPropertiesInCategory(sVisualCategoryName, TVKSpaceText,
    ['AllowedDeviation', 'AspectRatio', 'Extrusion', 'Oblique', 'TextHeight']);
{$ENDIF}
  // TVKSphere
  RegisterPropertiesInCategory(sVisualCategoryName, TVKSphere,
    ['Bottom', 'Radius', 'Slices', 'Stacks', 'Start', 'Stop']);
  // TVKDisk
  RegisterPropertiesInCategory(sVisualCategoryName, TVKDisk,
    ['*Radius', 'Loops', 'Slices']);
  // TVKCone
  RegisterPropertiesInCategory(sVisualCategoryName, TVKCone,
    ['BottomRadius', 'Loops', 'Slices', 'Stacks']);
  // TVKCylinder
  RegisterPropertiesInCategory(sVisualCategoryName, TVKCylinder,
    ['*Radius', 'Loops', 'Slices', 'Stacks']);
  // TVKCapsule
  RegisterPropertiesInCategory(sVisualCategoryName, TVKCapsule,
    ['*Radius', 'Loops', 'Slices', 'Stacks']);
  // TVKAnnulus
  RegisterPropertiesInCategory(sVisualCategoryName, TVKAnnulus,
    ['Bottom*', 'Loops', 'Slices', 'Stacks', 'Top*']);
  // TVKTorus
  RegisterPropertiesInCategory(sVisualCategoryName, TVKTorus,
    ['*Radius', 'Rings', 'Sides']);
  // TVKArrowLine
  RegisterPropertiesInCategory(sVisualCategoryName, TVKArrowLine,
    ['Bottom*', 'Loops', 'Slices', 'Stacks', 'Top*']);
  // TVKPolygon
  RegisterPropertiesInCategory(sVisualCategoryName, TVKPolygon, ['Division']);

  { VKS.MultiPolygon }
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TVKContourNodes),
    TypeInfo(TVKContours)]);
  // TVKMultiPolygon
  RegisterPropertiesInCategory(sVisualCategoryName, TVKContour, ['Division']);

  { VKS.Extrusion }
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TVKNodes), TypeInfo(TPipeNodesColorMode)]);
  // TVKRevolutionSolid
  RegisterPropertiesInCategory(sVisualCategoryName, TVKRevolutionSolid,
    ['Division', 'Slices', 'YOffsetPerTurn']);
  // TVKExtrusionSolid
  RegisterPropertiesInCategory(sVisualCategoryName, TVKExtrusionSolid,
    ['Stacks']);
  // TVKPipe
  RegisterPropertiesInCategory(sVisualCategoryName, TVKPipeNode,
    ['RadiusFactor']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKPipe,
    ['Division', 'Radius', 'Slices']);

  { VKS.VectorFileObjects }
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(TActorAnimationMode), TypeInfo(TActorAnimations),
    TypeInfo(TMeshAutoCenterings), TypeInfo(TActorFrameInterpolation),
    TypeInfo(TActorAnimationReference), TypeInfo(TVKActor)]);
  RegisterPropertiesInCategory(sLayoutCategoryName,
    [TypeInfo(TMeshNormalsOrientation)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TMeshAutoCenterings), TypeInfo(TActorAnimationReference),
    TypeInfo(TMeshNormalsOrientation)]);
  // TVKFreeForm
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKFreeForm,
    ['UseMeshmaterials']);
  // TVKAnimationControler
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKAnimationControler,
    ['AnimationName']);
  RegisterPropertiesInCategory(sLinkageCategoryName, TVKAnimationControler,
    ['AnimationName']);
  // TVKActor
  RegisterPropertiesInCategory(sOpenGLCategoryName, TActorAnimation,
    ['*Frame']);
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKActor,
    ['*Frame*', 'Interval', 'OverlaySkeleton', 'UseMeshmaterials']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKActor,
    ['OverlaySkeleton']);

  { VKS.Mesh }
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(TMeshMode), TypeInfo(TVertexMode)]);

  { VKS.Graph }
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(THeightFieldOptions)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(THeightFieldColorMode), TypeInfo(TVKSamplingScale),
    TypeInfo(TXYZGridLinesStyle), TypeInfo(TXYZGridParts)]);
  // TVKXYZGrid
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKXYZGrid,
    ['Antialiased']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKXYZGrid,
    ['Antialiased', 'Line*']);

  { VKS.Particles }
  // TVKParticles
  RegisterPropertiesInCategory(sLayoutCategoryName, TVKParticles,
    ['VisibleAtRunTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKParticles,
    ['*Size', 'VisibleAtRunTime']);

  { VKS.Skydome }
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(TSkyDomeBands), TypeInfo(TSkyDomeOptions),
    TypeInfo(TSkyDomeStars)]);
  // TSkyDomeBand
  RegisterPropertiesInCategory(sVisualCategoryName, TSkyDomeBand,
    ['Slices', 'Stacks', '*Angle']);
  // TSkyDomeStar
  RegisterPropertiesInCategory(sVisualCategoryName, TSkyDomeStar,
    ['Dec', 'Magnitude', 'RA']);
  // TVKEarthSkyDome
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKEarthSkyDome,
    ['Slices', 'Stacks', 'SunElevation', 'Turbidity']);

  { VKS.Mirror }
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TMirrorOptions),
    TypeInfo(TVKBaseSceneObject)]);

  { VKS.ParticleFX }
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TBlendingMode)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TBlendingMode), TypeInfo(TPFXLifeColors),
    TypeInfo(TSpriteColorMode)]);
  // TVKParticleFXRenderer
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKParticleFXRenderer,
    ['ZWrite']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKParticleFXRenderer,
    ['ZWrite']);
  // TPFXLifeColor
  RegisterPropertiesInCategory(sOpenGLCategoryName, TPFXLifeColor,
    ['LifeTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TPFXLifeColor,
    ['LifeTime']);
  // TVKLifeColoredPFXManager
  RegisterPropertiesInCategory(sVisualCategoryName, TVKLifeColoredPFXManager,
    ['Acceleration', 'ParticleSize']);
  // GLPolygonPFXManager
  RegisterPropertiesInCategory(sVisualCategoryName, TVKPolygonPFXManager,
    ['NbSides']);
  // TVKPointLightPFXManager
  RegisterPropertiesInCategory(sVisualCategoryName, TVKPointLightPFXManager,
    ['TexMapSize']);

  { VKS.TerrainRenderer }
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(THeightDataSource)]);
  // TVKTerrainRenderer
  RegisterPropertiesInCategory(sVisualCategoryName, TVKTerrainRenderer,
    ['*CLOD*', 'QualityDistance', 'Tile*']);

  { VKS.zBuffer }
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TVKMemoryViewer),
    TypeInfo(TVKSceneViewer), TypeInfo(TOptimise)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TOptimise)]);

  // TVKZShadows
  RegisterPropertiesInCategory(sVisualCategoryName, TVKZShadows,
    ['DepthFade', '*Shadow', 'Soft', 'Tolerance']);

  { VKS.HUDObjects }
  RegisterPropertiesInCategory(sLayoutCategoryName, [TypeInfo(TTextLayout)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TVKBitmapFont), TypeInfo(TTextLayout)]);

  RegisterPropertiesInCategory(sLocalizableCategoryName,
    [TypeInfo(TVKBitmapFont)]);

  { VKS.Texture }
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(TVKMaterial), TypeInfo(TVKMaterialLibrary),
    TypeInfo(TVKLibMaterials), TypeInfo(TTextureNeededEvent)]);
  // TVKLibMaterial
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKLibMaterial,
    ['Texture2Name']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKLibMaterial,
    ['TextureOffset', 'TextureScale']);
  // TVKMaterialLibrary
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKMaterialLibrary,
    ['TexturePaths']);

  { VKS.Cadencer }
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TVKCadencer)]);

  { VKS.Collision }
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(TObjectCollisionEvent)]);

  { VKS.FireFX }
  // TVKFireFXManager
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKFireFXManager,
    ['MaxParticles', 'NoZWrite', 'Paused', 'UseInterval']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKFireFXManager,
    ['Fire*', 'InitialDir', 'NoZWrite', 'Particle*', 'Paused']);

  { VKS.ThorFX }
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(TCalcPointEvent)]);
  // GLThorFXManager
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKThorFXManager,
    ['Maxpoints', 'Paused']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKThorFXManager,
    ['Core', 'Glow*', 'Paused', 'Target', 'Vibrate', 'Wildness']);

  { VKS.BitmapFont }
  RegisterPropertiesInCategory(sOpenGLCategoryName,
    [TypeInfo(TVKMagFilter), TypeInfo(TVKMinFilter)]);
  RegisterPropertiesInCategory(sLocalizableCategoryName,
    [TypeInfo(TBitmapFontRanges)]);
  // TBitmapFontRange
  RegisterPropertiesInCategory(sLocalizableCategoryName, TBitmapFontRange,
    ['*ASCII']);
  // TVKBitmapFont
  RegisterPropertiesInCategory(sLayoutCategoryName, TVKBitmapFont,
    ['Char*', '*Interval*', '*Space']);
  RegisterPropertiesInCategory(sLocalizableCategoryName, TVKBitmapFont,
    ['Glyphs']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKBitmapFont,
    ['Char*', '*Interval*', '*Space', 'Glyphs']);

  { VKS.HeightData }
  // TVKBitmapHDS
  RegisterPropertiesInCategory(sOpenGLCategoryName, TVKBitmapHDS,
    ['MaxPoolSize']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVKBitmapHDS, ['Picture']);

end;

procedure Register;
begin
  RegisterComponents('GLScene', [TVKScene, TVKSceneViewer, TVKMemoryViewer,
    TVKMaterialLibrary, TVKMaterialLibraryEx, TVKCadencer, TVKGuiLayout,
    TVKBitmapFont, TVKWindowsBitmapFont, TVKScriptLibrary, TVKSoundLibrary,
    TVKSMWaveOut, TVKFullScreenViewer]);

  RegisterComponents('GLScene PFX', [TVKCustomPFXManager, TVKPolygonPFXManager,
    TVKPointLightPFXManager, TVKCustomSpritePFXManager, TVKPerlinPFXManager,
    TVKLinePFXManager, TVKFireFXManager, TVKThorFXManager,
    TVKEParticleMasksManager]);

  RegisterComponents('GLScene Utils', [TVKAsyncTimer, TVKStaticImposterBuilder,
    TVKCollisionManager, TVKAnimationControler, TVKAVIRecorder, TVKDCEManager,
    TVKFPSMovementManager, TVKMaterialScripter, TVKUserInterface, TVKNavigator,
    TVKSmoothNavigator, TVKSmoothUserInterface, TVKTimeEventsMGR,
    TVKApplicationFileIO, TVKVfsPAK, TVKSimpleNavigation, TVKGizmo,
    TVKCameraController, TVKSLanguage, TVKSLogger, TVKSArchiveManager,
    TVKJoystick, TVKScreenSaver, TVKSSynHiMemo]);

  RegisterComponents('GLScene Terrain', [TVKBitmapHDS, TVKCustomHDS,
    TVKHeightTileFileHDS, TVKBumpmapHDS, TVKPerlinHDS, TVKTexturedHDS,
    TVKAsyncHDS, TVKShadowHDS]);

  RegisterComponents('GLScene Shaders', [TVKTexCombineShader, TVKPhongShader,
    TVKUserShader, TVKHiddenLineShader, TVKCelShader, TVKOutlineShader,
    TVKMultiMaterialShader, TVKBumpShader, TVKSLShader,
    TVKSLDiffuseSpecularShader, TVKSLBumpShader, TVKAsmShader,
    TVKShaderCombiner, TVKTextureSharingShader, TVKSLPostBlurShader]);

  RegisterComponentEditor(TVKSceneViewer, TVKSceneViewerEditor);
  RegisterComponentEditor(TVKScene, TVKSceneEditor);
  RegisterComponentEditor(TVKMaterialLibrary, TVKMaterialLibraryEditor);
  RegisterComponentEditor(TVKMaterialLibraryEx, TVKMaterialLibraryEditor);
  RegisterComponentEditor(TVKSArchiveManager, TVKSArchiveManagerEditor);

  GLRegisterPropertiesInCategories;

  RegisterPropertyEditor(TypeInfo(TResolution), nil, '', TResolutionProperty);
  RegisterPropertyEditor(TypeInfo(TVKTexture), TVKMaterial, '',
    TVKTextureProperty);
  RegisterPropertyEditor(TypeInfo(TVKTextureImage), TVKTexture, '',
    TVKTextureImageProperty);
  RegisterPropertyEditor(TypeInfo(string), TVKTexture, 'ImageClassName',
    TVKImageClassProperty);

  RegisterPropertyEditor(TypeInfo(TVKSoundFile), TVKSoundSample, '',
    TSoundFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TVKBaseSoundSource, 'SoundName',
    TSoundNameProperty);

  RegisterPropertyEditor(TypeInfo(TVKCoordinates), nil, '',
    TVKCoordinatesProperty);

  RegisterPropertyEditor(TypeInfo(TVKColor), nil, '', TVKColorProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterial), nil, '', TVKMaterialProperty);
  RegisterComponentEditor(TVKGuiLayout, TVKGUILayoutEditor);

  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName), TVKMaterial, '',
    TVKLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName), TVKLibMaterial,
    'Texture2Name', TVKLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName), TVKSkyBox, '',
    TVKLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName), TVKEParticleMask, '',
    TVKLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName), TVKGameMenu, '',
    TVKLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName),
    TVKMaterialMultiProxyMaster, '', TVKLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName), TVKSLBumpShader, '',
    TVKLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName), TSpriteAnimation, '',
    TVKLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName), TVKMaterialProxy, '',
    TVKLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName), TVKActorProxy, '',
    TVKLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName), TVKFBORenderer, '',
    TVKLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TActorAnimationName), TVKAnimationControler,
    '', TVKAnimationNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName),
    TVKTextureSharingShaderMaterial, 'LibMaterialName',
    TVKLibMaterialNameProperty);
  RegisterSelectionEditor(TVKBaseSceneObject,
    TVKBaseSceneObjectSelectionEditor);
  RegisterSelectionEditor(TVKSoundLibrary, TVKSoundLibrarySelectionEditor);
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName), TVKLibMaterialProperty,
    'NextPass', TVKLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName),
    TVKTextureProperties, 'LibTextureName', TVKLibTextureNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName),
    TVKTextureProperties, 'LibSamplerName', TVKLibSamplerNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName),
    TVKMultitexturingProperties, 'LibCombinerName', TVKLibCombinerNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName),
    TVKMultitexturingProperties, 'LibAsmProgName', TVKLibAsmProgNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName), TVKShaderModel3,
    'LibVertexShaderName', TVKLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName), TVKShaderModel3,
    'LibFragmentShaderName', TVKLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName), TVKShaderModel4,
    'LibVertexShaderName', TVKLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName), TVKShaderModel4,
    'LibFragmentShaderName', TVKLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName), TVKShaderModel4,
    'LibGeometryShaderName', TVKLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName), TVKShaderModel5,
    'LibVertexShaderName', TVKLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName), TVKShaderModel5,
    'LibFragmentShaderName', TVKLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName), TVKShaderModel5,
    'LibGeometryShaderName', TVKLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName), TVKShaderModel5,
    'LibTessControlShaderName', TVKLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVKMaterialComponentName), TVKShaderModel5,
    'LibTessEvalShaderName', TVKLibShaderNameProperty);

  RegisterPropertyEditor(TypeInfo(string), TVKTextureImageEx, 'SourceFile',
    TPictureFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TVKShaderEx, 'SourceFile',
    TShaderFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TVKASMVertexProgram, 'SourceFile',
    TAsmProgFileProperty);

  RegisterPropertyEditor(TypeInfo(Boolean), TVKBaseShaderModel,
    'AutoFillOfUniforms', TUniformAutoSetProperty);
  RegisterPropertyEditor(TypeInfo(TStringList), TVKShaderEx, 'Source',
    TVKShaderEditorProperty);
end;

function GetGLSceneVersion: string;
var
  LProject: IOTAProject;
  LExePath, LProjectPath, LSVN, LRevision: string;
begin
  LRevision := Copy(GLSCENE_REVISION, 12, 4);

  // will be assigned after project compilation
  // after each compilation get it from file \.svn\entries in 4-th line
  // and write to file GLSceneRevision
  // in both fail (no \.svn\entries or GLSceneRevision file) get a version value from GLScene.pas
  LProject := GetActiveProject;
  LExePath := ExtractFilePath(ParamStr(0));
  if Assigned(LProject) then
  begin
    LProjectPath := ExtractFilePath(LProject.FileName);
    LSVN := LProjectPath + '.svn\entries';
    if FileExists(LSVN) then
      with TStringList.Create do
        try
          // Load
          LoadFromFile(LSVN);
          if (Count >= 4) and (Trim(Strings[3]) <> '') and
            IsDirectoryWriteable(LExePath) then
          begin
            LRevision := Trim(Strings[3]);
            // Save
            Clear;
            Add(LRevision);
            SaveToFile(LExePath + 'GLSceneRevision');
          end;
        finally
          Free;
        end;
  end
  else if FileExists(LExePath + 'GLSceneRevision') then
    try
      with TStringList.Create do
        try
          LoadFromFile(LExePath + 'GLSceneRevision');
          if (Count >= 1) and (Trim(Strings[0]) <> '') then
            LRevision := Trim(Strings[0]);
        finally
          Free;
        end;
    except
    end;

  // Finally
  Result := Format(GLSCENE_VERSION, [LRevision]);
end;

function GetProjectTargetName: string;
var
  Project: IOTAProject;
begin
  Result := '';
  Project := GetActiveProject;
  if Assigned(Project) then
  begin
    Result := Project.ProjectOptions.TargetName;
    if Length(Result) > 0 then
      ForceDirectories(ExtractFilePath(Result));
  end;
end;

initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

SplashScreenServices.AddPluginBitmap(GetGLSceneVersion,
  LoadBitmap(HInstance, 'TVKScene'), False, 'MPL 1.1 license', 'SVN version');

VKS.CrossPlatform.IsDesignTime := True;
VKS.CrossPlatform.vProjectTargetName := GetProjectTargetName;
VKS.Color.vUseDefaultColorSets := True;
VKS.Coordinates.vUseDefaultCoordinateSets := True;
ReadVideoModes;

with ObjectManager do
begin
  CreateDefaultObjectIcons(HInstance);
  RegisterSceneObject(TVKCamera, 'Camera', '', HInstance);
  RegisterSceneObject(TVKLightSource, 'LightSource', '', HInstance);
  RegisterSceneObject(TVKDummyCube, 'DummyCube', '', HInstance);

  // Basic geometry
  RegisterSceneObject(TVKSprite, 'Sprite', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TVKPoints, 'Points', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TVKLines, 'Lines', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TVKPlane, 'Plane', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TVKPolygon, 'Polygon', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TVKCube, 'Cube', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TVKFrustrum, 'Frustrum', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TVKSphere, 'Sphere', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TVKDisk, 'Disk', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TVKCone, 'Cone', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TVKCylinder, 'Cylinder', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TVKCapsule, 'Capsule', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TVKDodecahedron, 'Dodecahedron', glsOCBasicGeometry,
    HInstance);
  RegisterSceneObject(TVKIcosahedron, 'Icosahedron', glsOCBasicGeometry,
    HInstance);
  RegisterSceneObject(TVKOctahedron, 'Octahedron', glsOCBasicGeometry,
    HInstance);
  RegisterSceneObject(TVKTetrahedron, 'Tetrahedron', glsOCBasicGeometry,
    HInstance);
  RegisterSceneObject(TVKSuperellipsoid, 'Superellipsoid', glsOCBasicGeometry,
    HInstance);

  // Advanced geometry
  RegisterSceneObject(TVKAnimatedSprite, 'Animated Sprite',
    glsOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TVKArrowLine, 'ArrowLine', glsOCAdvancedGeometry,
    HInstance);
  RegisterSceneObject(TVKArrowArc, 'ArrowArc', glsOCAdvancedGeometry,
    HInstance);
  RegisterSceneObject(TVKAnnulus, 'Annulus', glsOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TVKExtrusionSolid, 'ExtrusionSolid',
    glsOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TVKMultiPolygon, 'MultiPolygon', glsOCAdvancedGeometry,
    HInstance);
  RegisterSceneObject(TVKPipe, 'Pipe', glsOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TVKRevolutionSolid, 'RevolutionSolid',
    glsOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TVKTorus, 'Torus', glsOCAdvancedGeometry, HInstance);

  // Mesh objects
  RegisterSceneObject(TVKActor, 'Actor', glsOCMeshObjects, HInstance);
  RegisterSceneObject(TVKFreeForm, 'FreeForm', glsOCMeshObjects, HInstance);
  RegisterSceneObject(TVKMesh, 'Mesh', glsOCMeshObjects, HInstance);
  RegisterSceneObject(TVKTilePlane, 'TilePlane', glsOCMeshObjects, HInstance);
  RegisterSceneObject(TVKPortal, 'Portal', glsOCMeshObjects, HInstance);
  RegisterSceneObject(TVKTerrainRenderer, 'TerrainRenderer', glsOCMeshObjects,
    HInstance);

  // Graph-plotting objects
  RegisterSceneObject(TVKFlatText, 'FlatText', glsOCGraphPlottingObjects,
    HInstance);
  RegisterSceneObject(TVKHeightField, 'HeightField', glsOCGraphPlottingObjects,
    HInstance);
  RegisterSceneObject(TVKXYZGrid, 'XYZGrid', glsOCGraphPlottingObjects,
    HInstance);

  // Particle systems
  RegisterSceneObject(TVKParticles, 'Particles', glsOCParticleSystems,
    HInstance);
  RegisterSceneObject(TVKParticleFXRenderer, 'PFX Renderer',
    glsOCParticleSystems, HInstance);

  // Environment objects
  RegisterSceneObject(TVKEarthSkyDome, 'EarthSkyDome', glsOCEnvironmentObjects,
    HInstance);
  RegisterSceneObject(TVKSkyDome, 'SkyDome', glsOCEnvironmentObjects,
    HInstance);
  RegisterSceneObject(TVKSkyBox, 'SkyBox', glsOCEnvironmentObjects, HInstance);
  RegisterSceneObject(TVKAtmosphere, 'Atmosphere', glsOCEnvironmentObjects,
    HInstance);

  // HUD objects.
  RegisterSceneObject(TVKHUDSprite, 'HUD Sprite', glsOCHUDObjects, HInstance);
  RegisterSceneObject(TVKHUDText, 'HUD Text', glsOCHUDObjects, HInstance);
  RegisterSceneObject(TVKResolutionIndependantHUDText,
    'Resolution Independant HUD Text', glsOCHUDObjects, HInstance);
  RegisterSceneObject(TVKAbsoluteHUDText, 'Absolute HUD Text', glsOCHUDObjects,
    HInstance);
  RegisterSceneObject(TVKGameMenu, 'GameMenu', glsOCHUDObjects, HInstance);
  RegisterSceneObject(TVKConsole, 'Console', glsOCHUDObjects, HInstance);

  // GUI objects.
  RegisterSceneObject(TVKBaseControl, 'Root Control', glsOCGuiObjects,
    HInstance);
  RegisterSceneObject(TVKPopupMenu, 'GLPopupMenu', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TVKForm, 'GLForm', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TVKPanel, 'GLPanel', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TVKButton, 'GLButton', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TVKCheckBox, 'GLCheckBox', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TVKEdit, 'GLEdit', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TVKLabel, 'GLLabel', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TVKAdvancedLabel, 'GLAdvancedLabel', glsOCGuiObjects,
    HInstance);
  RegisterSceneObject(TVKScrollbar, 'GLScrollbar', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TGLStringGrid, 'GLStringGrid', glsOCGuiObjects,
    HInstance);
  RegisterSceneObject(TVKCustomControl, 'GLBitmapControl', glsOCGuiObjects,
    HInstance);

  // Special objects
  RegisterSceneObject(TVKLensFlare, 'LensFlare', glsOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TVKTextureLensFlare, 'TextureLensFlare',
    glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TVKMirror, 'Mirror', glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TVKShadowPlane, 'ShadowPlane', glsOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TVKShadowVolume, 'ShadowVolume', glsOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TVKZShadows, 'ZShadows', glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TVKSLTextureEmitter, 'GLSL Texture Emitter',
    glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TVKSLProjectedTextures, 'GLSL Projected Textures',
    glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TVKTextureEmitter, 'Texture Emitter', glsOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TVKProjectedTextures, 'Projected Textures',
    glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TVKBlur, 'Blur', glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TVKMotionBlur, 'MotionBlur', glsOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TVKSpaceText, 'SpaceText', glsOCDoodad, HInstance);
  RegisterSceneObject(TVKTrail, 'GLTrail', glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TVKPostEffect, 'PostEffect', glsOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TVKPostShaderHolder, 'PostShaderHolder',
    glsOCSpecialObjects, HInstance);

  // Doodad objects.
  RegisterSceneObject(TVKTeapot, 'Teapot', glsOCDoodad, HInstance);
  RegisterSceneObject(TVKTree, 'Tree', glsOCDoodad, HInstance);
  RegisterSceneObject(TVKWaterPlane, 'WaterPlane', glsOCDoodad, HInstance);

  // Proxy objects.
  RegisterSceneObject(TVKProxyObject, 'ProxyObject', glsOCProxyObjects,
    HInstance);
  RegisterSceneObject(TVKColorProxy, 'ColorProxy', glsOCProxyObjects,
    HInstance);
  RegisterSceneObject(TVKFreeFormProxy, 'FreeFormProxy', glsOCProxyObjects,
    HInstance);
  RegisterSceneObject(TVKMaterialProxy, 'MaterialProxy', glsOCProxyObjects,
    HInstance);
  RegisterSceneObject(TVKActorProxy, 'ActorProxy', glsOCProxyObjects,
    HInstance);
  RegisterSceneObject(TVKMultiProxy, 'MultiProxy', glsOCProxyObjects,
    HInstance);
  RegisterSceneObject(TVKMaterialMultiProxy, 'MaterialMultiProxy',
    glsOCProxyObjects, HInstance);

  // Other objects.
  RegisterSceneObject(TVKDirectOpenGL, 'Direct OpenGL', '', HInstance);
  RegisterSceneObject(TVKRenderPoint, 'Render Point', '', HInstance);
  RegisterSceneObject(TVKImposter, 'Imposter Sprite', '', HInstance);
  RegisterSceneObject(TVKFeedback, 'OpenGL Feedback', '', HInstance);
  RegisterSceneObject(TVKFBORenderer, 'OpenGL FrameBuffer', '', HInstance);
end;

finalization

ObjectManager.Free;

end.
