//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Registration unit for GLScene library components, property editors and
  IDE experts. 
  
}
unit GLX.SceneRegister;

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
  GLX.Strings,
  GLX.Scene,
  GLX.Context,
  GLX.Color,
  GLX.CrossPlatform,
  GLX.ObjectManager;

type
  // TGLLibMaterialNameProperty
  //
  TGLLibMaterialNameProperty = class(TStringProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TGLSceneViewerEditor
  //
  TGLSceneViewerEditor = class(TComponentEditor)
  public
    
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  // TGLSceneEditor
  //
  TGLSceneEditor = class(TComponentEditor)
  public
    
    procedure Edit; override;

    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  // TResolutionProperty
  //
  TResolutionProperty = class(TPropertyEditor)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  // TClassProperty
  //
  TGLTextureProperty = class(TClassProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
  end;

  // TGLTextureImageProperty
  //
  TGLTextureImageProperty = class(TClassProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TGLImageClassProperty
  //
  TGLImageClassProperty = class(TClassProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  // TGLColorProperty
  //
  TGLColorProperty = class(TClassProperty, ICustomPropertyDrawing,
    ICustomPropertyListDrawing)
  private
    

  protected
    
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
      const ARect: TGLRect; ASelected: Boolean);
    // CustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TGLRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TGLRect;
      ASelected: Boolean);
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  // TSoundFileProperty
  //
  TSoundFileProperty = class(TClassProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  // TSoundNameProperty
  //
  TSoundNameProperty = class(TStringProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  // TGLCoordinatesProperty
  //
  TGLCoordinatesProperty = class(TClassProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TGLMaterialProperty
  //
  TGLMaterialProperty = class(TClassProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TGLGUILayoutEditor
  //
  TGLGUILayoutEditor = class(TComponentEditor)
  public
    
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
    that problem, most of the subitems handling code in TGLSceneBaseObject is
    here for the same reason...), the "protected" wasn't meant just to lure
    programmers into code they can't reuse... Arrr! and he did that again
    in D6! Grrr... }
  TReuseableDefaultEditor = class(TComponentEditor, IDefaultEditor)
  protected
    
    FFirst: IProperty;
    FBest: IProperty;
    FContinue: Boolean;
    procedure CheckEdit(const Prop: IProperty);
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); virtual;

  public
    
    procedure Edit; override;
  end;

  // TGLMaterialLibraryEditor
  //
  { Editor for material library.  }
  TGLMaterialLibraryEditor = class(TReuseableDefaultEditor, IDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  // TGLAnimationNameProperty
  //
  TGLAnimationNameProperty = class(TStringProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { Selection editor for TGLSoundLibrary. 
    Allows units to be added to the uses clause automatically when
    sound files are loaded into a TGLSoundLibrary at design-time. }
  TGLSoundLibrarySelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { Selection editor for TGLBaseSceneObject. 
    Allows units to be added to the uses clause automatically when
    behaviours/effects are added to a TGLBaseSceneObject at design-time. }
  TGLBaseSceneObjectSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  // TGLSArchiveManagerEditor
  //
  { Editor for GLScene Archive Manager.  }
  TGLSArchiveManagerEditor = class(TReuseableDefaultEditor, IDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  // TGLMaterialComponentNameProperty
  //

  TGLMaterialComponentNameProperty = class(TStringProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TGLLibTextureNameProperty = class(TGLMaterialComponentNameProperty)
  public
    
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibSamplerNameProperty = class(TGLMaterialComponentNameProperty)
  public
    
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibCombinerNameProperty = class(TGLMaterialComponentNameProperty)
  public
    
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibShaderNameProperty = class(TGLMaterialComponentNameProperty)
  public
    
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibAttachmentNameProperty = class(TGLMaterialComponentNameProperty)
  public
    
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibAsmProgNameProperty = class(TGLMaterialComponentNameProperty)
  public
    
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  // TPictureFileProperty
  //
  TPictureFileProperty = class(TStringProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TShaderFileProperty
  //
  TShaderFileProperty = class(TStringProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TAsmProgFileProperty
  //
  TAsmProgFileProperty = class(TStringProperty)
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TUniformAutoSetProperty
  //
  TUniformAutoSetProperty = class(TPropertyEditor)
  private
    procedure PassUniform(const S: string);
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TGLShaderEditorProperty = class(TClassProperty)
  protected
    
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
    procedure OnShaderCheck(Sender: TObject);
  public
    
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

// : Auto-create for object manager
function ObjectManager: TGLObjectManager;

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
  GLX.AnimatedSprite,
  GLX.ApplicationFileIO,
  GLX.AsmShader,
  GLX.AsyncHDS,
  GLX.AsyncTimer,
  GLX.Atmosphere,
  GLX.AVIRecorder,
  GLX.BaseClasses,
  GLX.BitmapFont,
  GLX.Blur,
  GLX.BumpmapHDS,
  GLX.BumpShader,
  GLX.Cadencer,
  GLX.CameraController,
  GLX.CelShader,
  GLX.Collision,
  GLX.CompositeImage,
  GLX.Console,
  GLX.Coordinates,
  GLX.DCE,
  GLX.DynamicTexture,
  GLX.EParticleMasksManager,
  GLX.ExplosionFx,
  GLX.Extrusion,
  GLX.FBORenderer,
  GLX.Feedback,
  GLX.FireFX,
  GLX.FPSMovement,
  GLX.GameMenu,
  GLX.GeomObjects,
  GLX.Gizmo,
  GLX.Graph,
  GLX.Graphics,
  GLX.Gui,
  GLX.HeightData,
  GLX.HeightTileFileHDS,
  GLX.HiddenLineShader,
  GLX.HUDObjects,
  GLX.Imposter,
  GLX.LensFlare,
  GLX.LinePFX,
  GLX.Material,
  GLX.MaterialEx,
  GLX.MaterialMultiProxy,
  GLX.MaterialScript,
  GLX.Mesh,
  GLX.Mirror,
  GLX.MultiMaterialShader,
  GLX.MultiPolygon,
  GLX.MultiProxy,
  GLX.Navigator,
  GLX.Nodes,
  GLX.Objects,
  GLX.OutlineShader,
  GLX.ParticleFX,
  GLX.Particles,
  GLX.Perlin,
  GLX.PerlinPFX,
  GLX.PhongShader,
  GLX.Polyhedron,
  GLX.Portal,
  GLX.PostEffects,
  GLX.ProjectedTextures,
  GLX.ProxyObjects,
  GLX.RenderContextInfo,
  GLX.ArchiveManager,
  GLX.SceneEdit,
  GLX.Screen,
  GLX.ScriptBase,
  GLX.ShaderCombiner,
  GLX.ShadowHDS,
  GLX.ShadowPlane,
  GLX.ShadowVolume,
  GLX.SimpleNavigation,
  GLX.SkyBox,
  GLX.Skydome,
  GLX.Language,
  GLSL.BumpShader,
  GLSL.DiffuseSpecularShader,
  GLSL.PostBlurShader,
  GLSL.ProjectedTextures,
  GLSL.Shader,
  GLX.SmoothNavigator,
  GLX.SMWaveOut,
  GLX.State,
  GLX.Strings,
  GLX.Teapot,
  GLX.TerrainRenderer,
  GLX.TexCombineShader,
  GLX.TexLensFlare,
  GLX.Texture,
  GLX.TexturedHDS,
  GLX.TextureImageEditors,
  GLX.TextureSharingShader,
  GLX.ThorFX,
  GLX.TilePlane,
  GLX.TimeEventsMgr,
  GLX.Trail,
  GLX.Tree,
  GLX.Types,
  GLX.FileTIN,
  GLX.UserShader,
  GLX.Utils,
  GLX.VectorFileObjects,
  GLX.VfsPAK,
  GLX.WaterPlane,
  GLX.Windows,
  GLX.WindowsFont,
  GLX.zBuffer,
  GLX.VectorTypes,
  GLX.VectorGeometry,
  // Image file formats
  DDSImage,
  TGA,
  // Vector file formats
  GLX.File3DS,
  GLX.FileASE,
  GLX.FileB3D,
  GLX.FileGL2,
  GLX.FileGTS,
  GLX.FileLMTS,
  GLX.FileLWO,
  GLX.FileMD2,
  GLX.FileMD3,
  GLX.FileMD5,
  GLX.FileMDC,
  GLX.FileMS3D,
  GLX.FileNMF,
  GLX.FileNurbs,
  GLX.FileObj,
  GLX.FileOCT,
  GLX.FilePLY,
  GLX.FileQ3BSP,
  GLX.FileSMD,
  GLX.FileSTL,
  GLX.FileVRML,

  // Sound file formats
  GLX.FileWAV,
  GLX.FileMP3,

  // Raster file format
  GLX.FileDDS,
  GLX.FileO3TC,
  GLX.FileHDR,
  GLX.FileJPEG,
  GLX.FilePNG,
  GLX.FileBMP,
  GLX.FileTGA,

  GLX.Sound,
  GLX.SoundFileObjects,
  GLX.SpaceText,
  GLX.Joystick,
  GLX.ScreenSaver,
  GLX.FullScreenViewer,
  GLX.Log;

var
  vObjectManager: TGLObjectManager;

function ObjectManager: TGLObjectManager;
begin
  if not Assigned(vObjectManager) then
    vObjectManager := TGLObjectManager.Create(nil);
  Result := vObjectManager;
end;

{$IFDEF VKS_REGION}{$REGION 'TOpenGLCategory'}{$ENDIF}
{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TGLSceneViewerEditor'}{$ENDIF}
// ExecuteVerb
//

procedure TGLSceneViewerEditor.ExecuteVerb(Index: Integer);
var
  source: TGLSceneViewer;
begin
  source := Component as TGLSceneViewer;
  case Index of
    0:
      source.Buffer.ShowInfo;
  end;
end;

// GetVerb
//

function TGLSceneViewerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show context info';
  end;
end;

// GetVerbCount
//

function TGLSceneViewerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TGLSceneEditor'}{$ENDIF}
// Edit
//

procedure TGLSceneEditor.Edit;
begin
  with GLSceneEditorForm do
  begin
    SetScene(Self.Component as TGLScene, Self.Designer);
    Show;
  end;
end;

// ExecuteVerb
//

procedure TGLSceneEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

// GetVerb
//

function TGLSceneEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Scene Editor';
  end;
end;

// GetVerbCount
//

function TGLSceneEditor.GetVerbCount: Integer;
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
{$IFDEF VKS_REGION}{$REGION 'TGLTextureProperty'}{$ENDIF}

function TGLTextureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties];
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TGLTextureImageProperty'}{$ENDIF}
// GetAttributes
//

function TGLTextureImageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// Edit
//

procedure TGLTextureImageProperty.Edit;
begin
  if EditTextureImage(TGLTextureImage(GetOrdValue)) then
    Designer.Modified;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TGLImageClassProperty'}{$ENDIF}
// GetAttributes
//

function TGLImageClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValues
//

procedure TGLImageClassProperty.GetValues(Proc: TGetStrProc);
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

function TGLImageClassProperty.GetValue: string;
begin
  Result := FindGLTextureImageClass(GetStrValue).FriendlyName;
end;

// SetValue
//

procedure TGLImageClassProperty.SetValue(const Value: string);
var
  tic: TGLTextureImageClass;
begin
  tic := FindGLTextureImageClassByFriendlyName(Value);
  if Assigned(tic) then
    SetStrValue(tic.ClassName)
  else
    SetStrValue('');
  Modified;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TGLColorProperty'}{$ENDIF}

procedure TGLColorProperty.Edit;
var
  colorDialog: TColorDialog;
  GLX.Color: TGLColor;
begin
  colorDialog := TColorDialog.Create(nil);
  try
    GLX.Color := TGLColor(GetOrdValue);
{$IFDEF WIN32}
    colorDialog.Options := [cdFullOpen];
{$ENDIF}
    colorDialog.Color := ConvertColorVector(GLX.Color.Color);
    if colorDialog.Execute then
    begin
      GLX.Color.Color := ConvertWinColor(colorDialog.Color);
      Modified;
    end;
  finally
    colorDialog.Free;
  end;
end;

function TGLColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paValueList, paDialog];
end;

procedure TGLColorProperty.GetValues(Proc: TGetStrProc);
begin
  ColorManager.EnumColors(Proc);
end;

function TGLColorProperty.GetValue: string;
begin
  Result := ColorManager.GetColorName(TGLColor(GetOrdValue).Color);
end;

procedure TGLColorProperty.SetValue(const Value: string);
begin
  TGLColor(GetOrdValue).Color := ColorManager.GetColor(Value);
  Modified;
end;

// ColorToBorderColor
//

function TGLColorProperty.ColorToBorderColor(aColor: TColorVector;
  selected: Boolean): TColor;
begin
  if (aColor.X > 0.75) or (aColor.Y > 0.75) or (aColor.Z > 0.75) then
    Result := clBlack
  else if selected then
    Result := clWhite
  else
    Result := ConvertColorVector(aColor);
end;

procedure TGLColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TGLColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
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

procedure TGLColorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M');
end;

procedure TGLColorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // Nothing
end;

procedure TGLColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
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
  sample: TGLSoundSample;
begin
  sample := GetComponent(0) as TGLSoundSample;
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
  sample: TGLSoundSample;
  Desc, F: string;
begin
  sample := GetComponent(0) as TGLSoundSample;
  ODialog := TOpenDialog.Create(nil);
  try
    GetGLSoundFileFormats.BuildFilterStrings(TGLSoundFile, Desc, F);
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
  source: TGLBaseSoundSource;
begin
  source := (GetComponent(0) as TGLBaseSoundSource);
  if Assigned(source.SoundLibrary) then
    with source.SoundLibrary do
      for i := 0 to Samples.Count - 1 do
        Proc(Samples[i].Name);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TGLCoordinatesProperty'}{$ENDIF}
// GetAttributes
//

function TGLCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

// Edit;
//

procedure TGLCoordinatesProperty.Edit;
var
  glc: TGLCoordinates;
  x, y, z: Single;
begin
  glc := TGLCoordinates(GetOrdValue);
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
{$IFDEF VKS_REGION}{$REGION 'TGLMaterialProperty'}{$ENDIF}
// GetAttributes
//

function TGLMaterialProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

// Edit
//

procedure TGLMaterialProperty.Edit;
begin
  if FMaterialEditorForm.MaterialEditorForm.Execute(TGLMaterial(GetOrdValue))
  then
    Modified;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TGLGUILayoutEditor'}{$ENDIF}

procedure TGLGUILayoutEditor.Edit;
begin
  GUILayoutEditorForm.Execute(TGLGuiLayout(Self.Component));
end;

procedure TGLGUILayoutEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

function TGLGUILayoutEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Layout Editor';
  end;
end;

function TGLGUILayoutEditor.GetVerbCount: Integer;
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
{$IFDEF VKS_REGION}{$REGION 'TGLMaterialLibraryEditor'}{$ENDIF}

// EditProperty
//
procedure TGLMaterialLibraryEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
begin
  if CompareText(Prop.GetName, 'MATERIALS') = 0 then
  begin
    FBest := Prop;
  end;
end;

procedure TGLMaterialLibraryEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

function TGLMaterialLibraryEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Material Library Editor';
  end;
end;

function TGLMaterialLibraryEditor.GetVerbCount: Integer;
begin
  Result := 1
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TGLLibMaterialNameProperty'}{$ENDIF}
// GetAttributes
//

function TGLLibMaterialNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// Edit
//

procedure TGLLibMaterialNameProperty.Edit;
var
  buf: string;
  ml: TGLAbstractMaterialLibrary;
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
{$IFDEF VKS_REGION}{$REGION 'TGLAnimationNameProperty'}{$ENDIF}
// GetAttributes
//

function TGLAnimationNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValues
//

procedure TGLAnimationNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  animControler: TGLAnimationControler;
  actor: TGLActor;
begin
  animControler := (GetComponent(0) as TGLAnimationControler);
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
{$IFDEF VKS_REGION}{$REGION 'TGLBaseSceneObjectSelectionEditor'}{$ENDIF}

procedure TGLBaseSceneObjectSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  i, j: Integer;
  comp: TGLBaseSceneObject;
begin
  if (Designer = nil) or (Designer.Root = nil) then
    Exit;

  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if (Designer.Root.Components[i] is TGLBaseSceneObject) then
    begin
      comp := TGLBaseSceneObject(Designer.Root.Components[i]);
      for j := 0 to comp.Behaviours.Count - 1 do
        Proc(FindUnitName(comp.Behaviours[j]));
      for j := 0 to comp.Effects.Count - 1 do
        Proc(FindUnitName(comp.Effects[j]));
    end;
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TGLSoundLibrarySelectionEditor'}{$ENDIF}

procedure TGLSoundLibrarySelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  i, j: Integer;
  comp: TGLSoundLibrary;
begin
  if (Designer = nil) or (Designer.Root = nil) then
    Exit;

  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if (Designer.Root.Components[i] is TGLSoundLibrary) then
    begin
      comp := TGLSoundLibrary(Designer.Root.Components[i]);
      for j := 0 to comp.Samples.Count - 1 do
        if Assigned(comp.Samples[j].Data) then
          Proc(FindUnitName(comp.Samples[j].Data));
    end;
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TGLSArchiveManagerEditor'}{$ENDIF}

procedure TGLSArchiveManagerEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
begin
  if CompareText(Prop.GetName, 'ARCHIVES') = 0 then
  begin
    FBest := Prop;
  end;
end;

procedure TGLSArchiveManagerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

function TGLSArchiveManagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Archive Manager Editor';
  end;
end;

function TGLSArchiveManagerEditor.GetVerbCount: Integer;
begin
  Result := 1
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TGLMaterialComponentNameProperty'}{$ENDIF}

procedure TGLMaterialComponentNameProperty.Edit;
var
  LOwner: IGLMaterialLibrarySupported;
  LItem: TGLBaseMaterialCollectionItem;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    LItem := TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .Components.GetItemByName(GetStrValue);
    if Assigned(LItem) then
      Designer.SelectComponent(LItem);
    Modified;
  end;
end;

function TGLMaterialComponentNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TGLLibTextureNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TGLTextureImageEx);
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TGLFrameBufferAttachment);
  end;
end;

procedure TGLLibSamplerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TGLTextureSampler);
end;

procedure TGLLibCombinerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TGLTextureCombiner);
end;

procedure TGLLibShaderNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc, TGLShaderEx);
end;

procedure TGLLibAttachmentNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TGLFrameBufferAttachment);
end;

procedure TGLLibAsmProgNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TGLASMVertexProgram);
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
  ShaderUniformEditor.AddUniform(TGLBaseShaderModel(GetComponent(0))
    .Uniforms[S]);
end;

procedure TUniformAutoSetProperty.Edit;
var
  LOwner: TGLBaseShaderModel;
begin
  LOwner := TGLBaseShaderModel(GetComponent(0));
  if LOwner.Enabled and LOwner.IsValid then
  begin
    with ShaderUniformEditor do
    begin
      Clear;
      LOwner.MaterialLibrary.GetNames(AddTextureName, TGLTextureImageEx);
      LOwner.MaterialLibrary.GetNames(AddTextureName, TGLFrameBufferAttachment);
      LOwner.MaterialLibrary.GetNames(AddSamplerName, TGLTextureSampler);
      LOwner.GetUniformNames(PassUniform);
      Execute;
    end;
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VKS_REGION}{$REGION 'TGLShaderEditorProperty'}{$ENDIF}

function TGLShaderEditorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

function TGLShaderEditorProperty.GetStrings: TStrings;
begin
  Result := TStrings(GetOrdValue);
end;

procedure TGLShaderEditorProperty.OnShaderCheck(Sender: TObject);
var
  LShader: TGLShaderEx;
  LContext: TGLContext;
begin
  SetStrings(GLShaderEditorForm.GLSLMemo.Lines);
  LShader := TGLShaderEx(GetComponent(0));
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

procedure TGLShaderEditorProperty.SetStrings(const Value: TStrings);
begin
  SetOrdValue(Longint(Value));
end;

procedure TGLShaderEditorProperty.Edit;
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
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TGLCamera), TypeInfo(TGLSceneBuffer), TypeInfo(TVSyncMode),
    TypeInfo(TGLScreenDepth)]); // TGLScreenDepth in GLWin32FullScreenViewer
{$ENDIF}
  // TGLSceneViewer
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLSceneViewer,
    ['*Render']);

  { GLX.Scene }
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TGLx.ObjectsSorting), TypeInfo(TGLProgressEvent),
    TypeInfo(TGLBehaviours), TypeInfo(TGLObjectEffects),
    TypeInfo(TDirectRenderEvent), TypeInfo(TGLCameraStyle),
    TypeInfo(TOnCustomPerspective), TypeInfo(TGLScene)]);
  RegisterPropertiesInCategory(sLayoutCategoryName,
    [TypeInfo(TGLx.ObjectsSorting), TypeInfo(TNormalDirection)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TGLVisibilityCulling), TypeInfo(TLightStyle), TypeInfo(TGLColor),
    TypeInfo(TNormalDirection), TypeInfo(TGLCameraStyle)]);
  // TGLBaseSceneObject
  RegisterPropertiesInCategory(sVisualCategoryName, TGLBaseSceneObject,
    ['Rotation', 'Direction', 'Position', 'Up', 'Scale', '*Angle', 'ShowAxes',
    'FocalLength']);
  // TGLSceneObject
  RegisterPropertiesInCategory(sVisualCategoryName, TGLSceneObject, ['Parts']);
  // TGLDirectVulkan
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLDirectVulkan,
    ['UseBuildList']);
  // TGLProxyObject
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TGLProxyObjectOptions)]);
  // TGLLightSource
  RegisterPropertiesInCategory(sVisualCategoryName, TGLLightSource,
    ['*Attenuation', 'Shining', 'Spot*']);
  // TGLCamera
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLCamera,
    ['TargetObject']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLCamera,
    ['DepthOfView', 'SceneScale']);
  // TGLNonVisualViewer
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLNonVisualViewer,
    ['*Render']);

  { GLX.Objects }
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TGLLinesNodes), TypeInfo(TLineNodesAspect),
    TypeInfo(TLineSplineMode), TypeInfo(TLinesOptions)]);
{$IFDEF WIN32} // unit GLSpaceText
  RegisterPropertiesInCategory(sLayoutCategoryName, [TypeInfo(TGLTextAdjust)]);
  RegisterPropertiesInCategory(sLocalizableCategoryName,
    [TypeInfo(TSpaceTextCharRange)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TLineSplineMode),
    TypeInfo(TCapType), TypeInfo(TNormalSmoothing),
    TypeInfo(TArrowHeadStackingStyle), TypeInfo(TGLTextAdjust)]);
{$ENDIF}
  // TGLDummyCube
  RegisterPropertiesInCategory(sLayoutCategoryName, TGLDummyCube,
    ['VisibleAtRunTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLDummyCube,
    ['CubeSize', 'VisibleAtRunTime']);
  // TGLPlane
  RegisterPropertiesInCategory(sVisualCategoryName, TGLPlane,
    ['*Offset', '*Tiles']);
  // TGLSprite
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLSprite, ['NoZWrite']);
  RegisterPropertiesInCategory(sLayoutCategoryName, TGLSprite, ['NoZWrite']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLSprite,
    ['AlphaChannel', 'Rotation']);
  // TGLNode
  RegisterPropertiesInCategory(sVisualCategoryName, TGLNode, ['X', 'Y', 'Z']);
  // TGLLines
  RegisterPropertiesInCategory(sVisualCategoryName, TGLLines,
    ['Antialiased', 'Division', 'Line*', 'NodeSize']);
  // TGLCube
  RegisterPropertiesInCategory(sVisualCategoryName, TGLCube, ['Cube*']);
  // TGLFrustrum
  RegisterPropertiesInCategory(sVisualCategoryName, TGLFrustrum,
    ['ApexHeight', 'Base*']);
  // TGLSpaceText
{$IFDEF WIN32} // unit GLSpaceText
  RegisterPropertiesInCategory(sVisualCategoryName, TGLSpaceText,
    ['AllowedDeviation', 'AspectRatio', 'Extrusion', 'Oblique', 'TextHeight']);
{$ENDIF}
  // TGLSphere
  RegisterPropertiesInCategory(sVisualCategoryName, TGLSphere,
    ['Bottom', 'Radius', 'Slices', 'Stacks', 'Start', 'Stop']);
  // TGLDisk
  RegisterPropertiesInCategory(sVisualCategoryName, TGLDisk,
    ['*Radius', 'Loops', 'Slices']);
  // TGLCone
  RegisterPropertiesInCategory(sVisualCategoryName, TGLCone,
    ['BottomRadius', 'Loops', 'Slices', 'Stacks']);
  // TGLCylinder
  RegisterPropertiesInCategory(sVisualCategoryName, TGLCylinder,
    ['*Radius', 'Loops', 'Slices', 'Stacks']);
  // TGLCapsule
  RegisterPropertiesInCategory(sVisualCategoryName, TGLCapsule,
    ['*Radius', 'Loops', 'Slices', 'Stacks']);
  // TGLAnnulus
  RegisterPropertiesInCategory(sVisualCategoryName, TGLAnnulus,
    ['Bottom*', 'Loops', 'Slices', 'Stacks', 'Top*']);
  // TGLTorus
  RegisterPropertiesInCategory(sVisualCategoryName, TGLTorus,
    ['*Radius', 'Rings', 'Sides']);
  // TGLArrowLine
  RegisterPropertiesInCategory(sVisualCategoryName, TGLArrowLine,
    ['Bottom*', 'Loops', 'Slices', 'Stacks', 'Top*']);
  // TGLPolygon
  RegisterPropertiesInCategory(sVisualCategoryName, TGLPolygon, ['Division']);

  { GLX.MultiPolygon }
  RegisterPropertiesInCategory(sVulkanCategoryName, [TypeInfo(TGLContourNodes),
    TypeInfo(TGLContours)]);
  // TGLMultiPolygon
  RegisterPropertiesInCategory(sVisualCategoryName, TGLContour, ['Division']);

  { GLX.Extrusion }
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TGLNodes), TypeInfo(TPipeNodesColorMode)]);
  // TGLRevolutionSolid
  RegisterPropertiesInCategory(sVisualCategoryName, TGLRevolutionSolid,
    ['Division', 'Slices', 'YOffsetPerTurn']);
  // TGLExtrusionSolid
  RegisterPropertiesInCategory(sVisualCategoryName, TGLExtrusionSolid,
    ['Stacks']);
  // TGLPipe
  RegisterPropertiesInCategory(sVisualCategoryName, TGLPipeNode,
    ['RadiusFactor']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLPipe,
    ['Division', 'Radius', 'Slices']);

  { GLX.VectorFileObjects }
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TGLActorAnimationMode), TypeInfo(TGLActorAnimations),
    TypeInfo(TMeshAutoCenterings), TypeInfo(TActorFrameInterpolation),
    TypeInfo(TGLActorAnimationReference), TypeInfo(TGLActor)]);
  RegisterPropertiesInCategory(sLayoutCategoryName,
    [TypeInfo(TMeshNormalsOrientation)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TMeshAutoCenterings), TypeInfo(TGLActorAnimationReference),
    TypeInfo(TMeshNormalsOrientation)]);
  // TGLFreeForm
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLFreeForm,
    ['UseMeshmaterials']);
  // TGLAnimationControler
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLAnimationControler,
    ['AnimationName']);
  RegisterPropertiesInCategory(sLinkageCategoryName, TGLAnimationControler,
    ['AnimationName']);
  // TGLActor
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLActorAnimation,
    ['*Frame']);
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLActor,
    ['*Frame*', 'Interval', 'OverlaySkeleton', 'UseMeshmaterials']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLActor,
    ['OverlaySkeleton']);

  { GLX.Mesh }
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TMeshMode), TypeInfo(TVertexMode)]);

  { GLX.Graph }
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TGLHeightFieldOptions)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TGLHeightFieldColorMode), TypeInfo(TGLSamplingScale),
    TypeInfo(TXYZGridLinesStyle), TypeInfo(TXYZGridParts)]);
  // TGLXYZGrid
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLXYZGrid,
    ['Antialiased']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLXYZGrid,
    ['Antialiased', 'Line*']);

  { GLX.Particles }
  // TGLParticles
  RegisterPropertiesInCategory(sLayoutCategoryName, TGLParticles,
    ['VisibleAtRunTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLParticles,
    ['*Size', 'VisibleAtRunTime']);

  { GLX.Skydome }
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TGLSkyDomeBands), TypeInfo(TGLSkyDomeOptions),
    TypeInfo(TGLSkyDomeStars)]);
  // TGLSkyDomeBand
  RegisterPropertiesInCategory(sVisualCategoryName, TGLSkyDomeBand,
    ['Slices', 'Stacks', '*Angle']);
  // TGLSkyDomeStar
  RegisterPropertiesInCategory(sVisualCategoryName, TGLSkyDomeStar,
    ['Dec', 'Magnitude', 'RA']);
  // TGLEarthSkyDome
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLEarthSkyDome,
    ['Slices', 'Stacks', 'SunElevation', 'Turbidity']);

  { GLX.Mirror }
  RegisterPropertiesInCategory(sVulkanCategoryName, [TypeInfo(TMirrorOptions),
    TypeInfo(TGLBaseSceneObject)]);

  { GLX.ParticleFX }
  RegisterPropertiesInCategory(sVulkanCategoryName, [TypeInfo(TBlendingMode)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TBlendingMode), TypeInfo(TPFXLifeColors),
    TypeInfo(TSpriteColorMode)]);
  // TGLParticleFXRenderer
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLParticleFXRenderer,
    ['ZWrite']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLParticleFXRenderer,
    ['ZWrite']);
  // TPFXLifeColor
  RegisterPropertiesInCategory(sVulkanCategoryName, TPFXLifeColor,
    ['LifeTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TPFXLifeColor,
    ['LifeTime']);
  // TGLLifeColoredPFXManager
  RegisterPropertiesInCategory(sVisualCategoryName, TGLLifeColoredPFXManager,
    ['Acceleration', 'ParticleSize']);
  // GLPolygonPFXManager
  RegisterPropertiesInCategory(sVisualCategoryName, TGLPolygonPFXManager,
    ['NbSides']);
  // TGLPointLightPFXManager
  RegisterPropertiesInCategory(sVisualCategoryName, TGLPointLightPFXManager,
    ['TexMapSize']);

  { GLX.TerrainRenderer }
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TGLHeightDataSource)]);
  // TGLTerrainRenderer
  RegisterPropertiesInCategory(sVisualCategoryName, TGLTerrainRenderer,
    ['*CLOD*', 'QualityDistance', 'Tile*']);

  { GLX.zBuffer }
  RegisterPropertiesInCategory(sVulkanCategoryName, [TypeInfo(TGLMemoryViewer),
    TypeInfo(TGLSceneViewer), TypeInfo(TOptimise)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TOptimise)]);

  // TGLZShadows
  RegisterPropertiesInCategory(sVisualCategoryName, TGLZShadows,
    ['DepthFade', '*Shadow', 'Soft', 'Tolerance']);

  { GLX.HUDObjects }
  RegisterPropertiesInCategory(sLayoutCategoryName, [TypeInfo(TTextLayout)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TGLBitmapFont), TypeInfo(TTextLayout)]);

  RegisterPropertiesInCategory(sLocalizableCategoryName,
    [TypeInfo(TGLBitmapFont)]);

  { GLX.Texture }
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TGLMaterial), TypeInfo(TGLMaterialLibrary),
    TypeInfo(TGLLibMaterials), TypeInfo(TTextureNeededEvent)]);
  // TGLLibMaterial
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLLibMaterial,
    ['Texture2Name']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLLibMaterial,
    ['TextureOffset', 'TextureScale']);
  // TGLMaterialLibrary
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLMaterialLibrary,
    ['TexturePaths']);

  { GLX.Cadencer }
  RegisterPropertiesInCategory(sVulkanCategoryName, [TypeInfo(TGLCadencer)]);

  { GLX.Collision }
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TObjectCollisionEvent)]);

  { GLX.FireFX }
  // TGLFireFXManager
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLFireFXManager,
    ['MaxParticles', 'NoZWrite', 'Paused', 'UseInterval']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLFireFXManager,
    ['Fire*', 'InitialDir', 'NoZWrite', 'Particle*', 'Paused']);

  { GLX.ThorFX }
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TCalcPointEvent)]);
  // GLThorFXManager
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLThorFXManager,
    ['Maxpoints', 'Paused']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLThorFXManager,
    ['Core', 'Glow*', 'Paused', 'Target', 'Vibrate', 'Wildness']);

  { GLX.BitmapFont }
  RegisterPropertiesInCategory(sVulkanCategoryName,
    [TypeInfo(TGLMagFilter), TypeInfo(TGLMinFilter)]);
  RegisterPropertiesInCategory(sLocalizableCategoryName,
    [TypeInfo(TGLBitmapFontRanges)]);
  // TGLBitmapFontRange
  RegisterPropertiesInCategory(sLocalizableCategoryName, TGLBitmapFontRange,
    ['*ASCII']);
  // TGLBitmapFont
  RegisterPropertiesInCategory(sLayoutCategoryName, TGLBitmapFont,
    ['Char*', '*Interval*', '*Space']);
  RegisterPropertiesInCategory(sLocalizableCategoryName, TGLBitmapFont,
    ['Glyphs']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLBitmapFont,
    ['Char*', '*Interval*', '*Space', 'Glyphs']);

  { GLX.HeightData }
  // TGLBitmapHDS
  RegisterPropertiesInCategory(sVulkanCategoryName, TGLBitmapHDS,
    ['MaxPoolSize']);
  RegisterPropertiesInCategory(sVisualCategoryName, TGLBitmapHDS, ['Picture']);

end;

procedure Register;
begin
  RegisterComponents('GLScene', [TGLScene, TGLSceneViewer, TGLMemoryViewer,
    TGLMaterialLibrary, TGLMaterialLibraryEx, TGLCadencer, TGLGuiLayout,
    TGLBitmapFont, TGLWindowsBitmapFont, TGLScriptLibrary, TGLSoundLibrary,
    TGLFullScreenViewer]);

  RegisterComponents('GLScene PFX', [TGLCustomPFXManager, TGLPolygonPFXManager,
    TGLPointLightPFXManager, TGLCustomSpritePFXManager, TGLPerlinPFXManager,
    TGLLinePFXManager, TGLFireFXManager, TGLThorFXManager,
    TGLEParticleMasksManager]);

  RegisterComponents('GLScene Utils', [TGLAsyncTimer, TGLStaticImposterBuilder,
    TGLCollisionManager, TGLAnimationControler, TGLAVIRecorder, TGLDCEManager,
    TGLFPSMovementManager, TGLMaterialScripter, TGLUserInterface, TGLNavigator,
    TGLSmoothNavigator, TGLSmoothUserInterface, TGLTimeEventsMGR,
    TGLApplicationFileIO, TGLVfsPAK, TGLSimpleNavigation, TGLGizmo,
    TGLCameraController, TGLSLanguage, TGLSLogger, TGLSArchiveManager,
    TGLJoystick, TGLScreenSaver, TGLSSynHiMemo]);

  RegisterComponents('GLScene Terrain', [TGLBitmapHDS, TGLCustomHDS,
    TGLHeightTileFileHDS, TGLBumpmapHDS, TGLPerlinHDS, TGLTexturedHDS,
    TGLAsyncHDS, TGLShadowHDS]);

  RegisterComponents('GLScene Shaders', [TGLTexCombineShader, TGLPhongShader,
    TGLUserShader, TGLHiddenLineShader, TGLCelShader, TGLOutlineShader,
    TGLMultiMaterialShader, TGLBumpShader, TGLGLSLShader,
    TGLSLDiffuseSpecularShader, TGLSLBumpShader, TGLAsmShader,
    TGLShaderCombiner, TGLTextureSharingShader, TGLSLPostBlurShader]);

  RegisterComponentEditor(TGLSceneViewer, TGLSceneViewerEditor);
  RegisterComponentEditor(TGLScene, TGLSceneEditor);
  RegisterComponentEditor(TGLMaterialLibrary, TGLMaterialLibraryEditor);
  RegisterComponentEditor(TGLMaterialLibraryEx, TGLMaterialLibraryEditor);
  RegisterComponentEditor(TGLSArchiveManager, TGLSArchiveManagerEditor);

  GLRegisterPropertiesInCategories;

  RegisterPropertyEditor(TypeInfo(TResolution), nil, '', TResolutionProperty);
  RegisterPropertyEditor(TypeInfo(TGLTexture), TGLMaterial, '',
    TGLTextureProperty);
  RegisterPropertyEditor(TypeInfo(TGLTextureImage), TGLTexture, '',
    TGLTextureImageProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLTexture, 'ImageClassName',
    TGLImageClassProperty);

  RegisterPropertyEditor(TypeInfo(TGLSoundFile), TGLSoundSample, '',
    TSoundFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLBaseSoundSource, 'SoundName',
    TSoundNameProperty);

  RegisterPropertyEditor(TypeInfo(TGLCoordinates), nil, '',
    TGLCoordinatesProperty);

  RegisterPropertyEditor(TypeInfo(TGLColor), nil, '', TGLColorProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterial), nil, '', TGLMaterialProperty);
  RegisterComponentEditor(TGLGuiLayout, TGLGUILayoutEditor);

  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterial, '',
    TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLLibMaterial,
    'Texture2Name', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSkyBox, '',
    TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLEParticleMask, '',
    TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLGameMenu, '',
    TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName),
    TGLMaterialMultiProxyMaster, '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSLBumpShader, '',
    TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TSpriteAnimation, '',
    TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterialProxy, '',
    TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLActorProxy, '',
    TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLFBORenderer, '',
    TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLActorAnimationName), TGLAnimationControler,
    '', TGLAnimationNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName),
    TGLTextureSharingShaderMaterial, 'LibMaterialName',
    TGLLibMaterialNameProperty);
  RegisterSelectionEditor(TGLBaseSceneObject,
    TGLBaseSceneObjectSelectionEditor);
  RegisterSelectionEditor(TGLSoundLibrary, TGLSoundLibrarySelectionEditor);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLLibMaterialProperty,
    'NextPass', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName),
    TGLTextureProperties, 'LibTextureName', TGLLibTextureNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName),
    TGLTextureProperties, 'LibSamplerName', TGLLibSamplerNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName),
    TGLMultitexturingProperties, 'LibCombinerName', TGLLibCombinerNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName),
    TGLMultitexturingProperties, 'LibAsmProgName', TGLLibAsmProgNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel3,
    'LibVertexShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel3,
    'LibFragmentShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel4,
    'LibVertexShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel4,
    'LibFragmentShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel4,
    'LibGeometryShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibVertexShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibFragmentShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibGeometryShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibTessControlShaderName', TGLLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterialComponentName), TGLShaderModel5,
    'LibTessEvalShaderName', TGLLibShaderNameProperty);

  RegisterPropertyEditor(TypeInfo(string), TGLTextureImageEx, 'SourceFile',
    TPictureFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLShaderEx, 'SourceFile',
    TShaderFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLASMVertexProgram, 'SourceFile',
    TAsmProgFileProperty);

  RegisterPropertyEditor(TypeInfo(Boolean), TGLBaseShaderModel,
    'AutoFillOfUniforms', TUniformAutoSetProperty);
  RegisterPropertyEditor(TypeInfo(TStringList), TGLShaderEx, 'Source',
    TGLShaderEditorProperty);
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
  LoadBitmap(HInstance, 'TGLScene'), False, 'MPL 2 license', 'SVN version');

GLX.CrossPlatform.IsDesignTime := True;
GLX.CrossPlatform.vProjectTargetName := GetProjectTargetName;
GLX.Color.vUseDefaultColorSets := True;
GLX.Coordinates.vUseDefaultCoordinateSets := True;
ReadVideoModes;

with ObjectManager do
begin
  CreateDefaultObjectIcons(HInstance);
  RegisterSceneObject(TGLCamera, 'Camera', '', HInstance);
  RegisterSceneObject(TGLLightSource, 'LightSource', '', HInstance);
  RegisterSceneObject(TGLDummyCube, 'DummyCube', '', HInstance);

  // Basic geometry
  RegisterSceneObject(TGLSprite, 'Sprite', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLPoints, 'Points', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLLines, 'Lines', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLPlane, 'Plane', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLPolygon, 'Polygon', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLCube, 'Cube', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLFrustrum, 'Frustrum', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLSphere, 'Sphere', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLDisk, 'Disk', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLCone, 'Cone', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLCylinder, 'Cylinder', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLCapsule, 'Capsule', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TGLDodecahedron, 'Dodecahedron', glsOCBasicGeometry,
    HInstance);
  RegisterSceneObject(TGLIcosahedron, 'Icosahedron', glsOCBasicGeometry,
    HInstance);
  RegisterSceneObject(TGLOctahedron, 'Octahedron', glsOCBasicGeometry,
    HInstance);
  RegisterSceneObject(TGLTetrahedron, 'Tetrahedron', glsOCBasicGeometry,
    HInstance);
  RegisterSceneObject(TGLSuperellipsoid, 'Superellipsoid', glsOCBasicGeometry,
    HInstance);

  // Advanced geometry
  RegisterSceneObject(TGLAnimatedSprite, 'Animated Sprite',
    glsOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLArrowLine, 'ArrowLine', glsOCAdvancedGeometry,
    HInstance);
  RegisterSceneObject(TGLArrowArc, 'ArrowArc', glsOCAdvancedGeometry,
    HInstance);
  RegisterSceneObject(TGLAnnulus, 'Annulus', glsOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLExtrusionSolid, 'ExtrusionSolid',
    glsOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLMultiPolygon, 'MultiPolygon', glsOCAdvancedGeometry,
    HInstance);
  RegisterSceneObject(TGLPipe, 'Pipe', glsOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLRevolutionSolid, 'RevolutionSolid',
    glsOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TGLTorus, 'Torus', glsOCAdvancedGeometry, HInstance);

  // Mesh objects
  RegisterSceneObject(TGLActor, 'Actor', glsOCMeshObjects, HInstance);
  RegisterSceneObject(TGLFreeForm, 'FreeForm', glsOCMeshObjects, HInstance);
  RegisterSceneObject(TGLMesh, 'Mesh', glsOCMeshObjects, HInstance);
  RegisterSceneObject(TGLTilePlane, 'TilePlane', glsOCMeshObjects, HInstance);
  RegisterSceneObject(TGLPortal, 'Portal', glsOCMeshObjects, HInstance);
  RegisterSceneObject(TGLTerrainRenderer, 'TerrainRenderer', glsOCMeshObjects,
    HInstance);

  // Graph-plotting objects
  RegisterSceneObject(TGLFlatText, 'FlatText', glsOCGraphPlottingObjects,
    HInstance);
  RegisterSceneObject(TGLHeightField, 'HeightField', glsOCGraphPlottingObjects,
    HInstance);
  RegisterSceneObject(TGLXYZGrid, 'XYZGrid', glsOCGraphPlottingObjects,
    HInstance);

  // Particle systems
  RegisterSceneObject(TGLParticles, 'Particles', glsOCParticleSystems,
    HInstance);
  RegisterSceneObject(TGLParticleFXRenderer, 'PFX Renderer',
    glsOCParticleSystems, HInstance);

  // Environment objects
  RegisterSceneObject(TGLEarthSkyDome, 'EarthSkyDome', glsOCEnvironmentObjects,
    HInstance);
  RegisterSceneObject(TGLSkyDome, 'SkyDome', glsOCEnvironmentObjects,
    HInstance);
  RegisterSceneObject(TGLSkyBox, 'SkyBox', glsOCEnvironmentObjects, HInstance);
  RegisterSceneObject(TGLAtmosphere, 'Atmosphere', glsOCEnvironmentObjects,
    HInstance);

  // HUD objects.
  RegisterSceneObject(TGLHUDSprite, 'HUD Sprite', glsOCHUDObjects, HInstance);
  RegisterSceneObject(TGLHUDText, 'HUD Text', glsOCHUDObjects, HInstance);
  RegisterSceneObject(TGLResolutionIndependantHUDText,
    'Resolution Independant HUD Text', glsOCHUDObjects, HInstance);
  RegisterSceneObject(TGLAbsoluteHUDText, 'Absolute HUD Text', glsOCHUDObjects,
    HInstance);
  RegisterSceneObject(TGLGameMenu, 'GameMenu', glsOCHUDObjects, HInstance);
  RegisterSceneObject(TGLConsole, 'Console', glsOCHUDObjects, HInstance);

  // GUI objects.
  RegisterSceneObject(TGLBaseControl, 'Root Control', glsOCGuiObjects,
    HInstance);
  RegisterSceneObject(TGLPopupMenu, 'GLPopupMenu', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TGLForm, 'GLForm', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TGLPanel, 'GLPanel', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TGLButton, 'GLButton', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TGLCheckBox, 'GLCheckBox', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TGLEdit, 'GLEdit', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TGLLabel, 'GLLabel', glsOCGuiObjects, HInstance);
  RegisterSceneObject(TGLAdvancedLabel, 'GLAdvancedLabel', glsOCGuiObjects,
    HInstance);
  RegisterSceneObject(TGLScrollbar, 'GLScrollbar', glsOCGuiObjects, HInstance);
  RegisterSceneObject(StringGrid, 'GLStringGrid', glsOCGuiObjects,
    HInstance);
  RegisterSceneObject(TGLCustomControl, 'GLBitmapControl', glsOCGuiObjects,
    HInstance);

  // Special objects
  RegisterSceneObject(TGLLensFlare, 'LensFlare', glsOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TGLTextureLensFlare, 'TextureLensFlare',
    glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLMirror, 'Mirror', glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLShadowPlane, 'ShadowPlane', glsOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TGLShadowVolume, 'ShadowVolume', glsOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TGLZShadows, 'ZShadows', glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLSLTextureEmitter, 'GLSL Texture Emitter',
    glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLSLProjectedTextures, 'GLSL Projected Textures',
    glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLTextureEmitter, 'Texture Emitter', glsOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TGLProjectedTextures, 'Projected Textures',
    glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLBlur, 'Blur', glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLMotionBlur, 'MotionBlur', glsOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TGLSpaceText, 'SpaceText', glsOCDoodad, HInstance);
  RegisterSceneObject(TGLTrail, 'Trail', glsOCSpecialObjects, HInstance);
  RegisterSceneObject(TGLPostEffect, 'PostEffect', glsOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TGLPostShaderHolder, 'PostShaderHolder',
    glsOCSpecialObjects, HInstance);

  // Doodad objects.
  RegisterSceneObject(TGLTeapot, 'Teapot', glsOCDoodad, HInstance);
  RegisterSceneObject(TGLTree, 'Tree', glsOCDoodad, HInstance);
  RegisterSceneObject(TGLWaterPlane, 'WaterPlane', glsOCDoodad, HInstance);

  // Proxy objects.
  RegisterSceneObject(TGLProxyObject, 'ProxyObject', glsOCProxyObjects,
    HInstance);
  RegisterSceneObject(TGLColorProxy, 'ColorProxy', glsOCProxyObjects,
    HInstance);
  RegisterSceneObject(TGLFreeFormProxy, 'FreeFormProxy', glsOCProxyObjects,
    HInstance);
  RegisterSceneObject(TGLMaterialProxy, 'MaterialProxy', glsOCProxyObjects,
    HInstance);
  RegisterSceneObject(TGLActorProxy, 'ActorProxy', glsOCProxyObjects,
    HInstance);
  RegisterSceneObject(TGLMultiProxy, 'MultiProxy', glsOCProxyObjects,
    HInstance);
  RegisterSceneObject(TGLMaterialMultiProxy, 'MaterialMultiProxy',
    glsOCProxyObjects, HInstance);

  // Other objects.
  RegisterSceneObject(TGLDirectVulkan, 'Direct Vulkan', '', HInstance);
  RegisterSceneObject(TGLRenderPoint, 'Render Point', '', HInstance);
  RegisterSceneObject(TGLImposter, 'Imposter Sprite', '', HInstance);
  RegisterSceneObject(TGLFeedback, 'Vulkan Feedback', '', HInstance);
  RegisterSceneObject(TGLFBORenderer, 'Vulkan FrameBuffer', '', HInstance);
end;

finalization

ObjectManager.Free;

end.
