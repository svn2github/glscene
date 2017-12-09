//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Registration unit for library components, property editors and
  IDE experts. 
  
}
unit VXS.SceneRegister;

interface

{$I VXScene.inc}

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

  { TODO : F1026 Files not found: 'VCLEditors' etc.}
  (*need to create instead a custom PropertyEditor like it described in -> *)
  (*ms-help://embarcadero.rs_xe7/rad/Creating_a_Component_Editor_and_a_Property_Editor_for_FireMonkey_Components.html*)
  (*
  VCLEditors,
  ToolsAPI,
  DesignIntf,
  DesignEditors,
  *)

  VXS.Strings,
  VXS.Scene,
  VXS.Context,
  VXS.Color,
  VXS.CrossPlatform,
  VXS.ObjectManager;

type
  TVXLibMaterialNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TVXSceneViewerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TVXSceneEditor = class(TComponentEditor)
  public

    procedure Edit; override;

    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TResolutionProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TVXTextureProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TVXTextureImageProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TVXImageClassProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TVXColorProperty = class(TClassProperty, ICustomPropertyDrawing,
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
      const ARect: TVXRect; ASelected: Boolean);
    // CustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TVXRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TVXRect;
      ASelected: Boolean);
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TSoundFileProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TSoundNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVXCoordinatesProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TVXMaterialProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TVXGUILayoutEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { Editor copied from DsgnIntf.
    Could have been avoided, if only that guy at Borland didn't chose to
    publish only half of the stuff (and that's not the only class with
    that problem, most of the subitems handling code in TVXSceneBaseObject is
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

  { Editor for material library.  }
  TVXMaterialLibraryEditor = class(TReuseableDefaultEditor, IDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TVXAnimationNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { Selection editor for TVXSoundLibrary.
    Allows units to be added to the uses clause automatically when
    sound files are loaded into a TVXSoundLibrary at design-time. }
  TVXSoundLibrarySelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { Selection editor for TVXBaseSceneObject.
    Allows units to be added to the uses clause automatically when
    behaviours/effects are added to a TVXBaseSceneObject at design-time. }
  TVXBaseSceneObjectSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { Editor for Archive Manager.  }
  TVXSArchiveManagerEditor = class(TReuseableDefaultEditor, IDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TVXMaterialComponentNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TVXLibTextureNameProperty = class(TVXMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVXLibSamplerNameProperty = class(TVXMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVXLibCombinerNameProperty = class(TVXMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVXLibShaderNameProperty = class(TVXMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVXLibAttachmentNameProperty = class(TVXMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVXLibAsmProgNameProperty = class(TVXMaterialComponentNameProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TPictureFileProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TShaderFileProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TAsmProgFileProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TUniformAutoSetProperty = class(TPropertyEditor)
  private
    procedure PassUniform(const S: string);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TVXShaderEditorProperty = class(TClassProperty)
  protected
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
    procedure OnShaderCheck(Sender: TObject);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

// Auto-create for object manager
function ObjectManager: TVXObjectManager;

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
  FSceneEditor,
  VXS.AnimatedSprite,
  VXS.ApplicationFileIO,
  VXS.AsmShader,
  VXS.AsyncHDS,
  VXS.AsyncTimer,
  VXS.Atmosphere,
  VXS.AVIRecorder,
  VXS.BaseClasses,
  VXS.BitmapFont,
  VXS.Blur,
  VXS.BumpmapHDS,
  VXS.BumpShader,
  VXS.Cadencer,
  VXS.CameraController,
  VXS.CelShader,
  VXS.Collision,
  VXS.CompositeImage,
  VXS.Console,
  VXS.Coordinates,
  VXS.DCE,
  VXS.DynamicTexture,
  VXS.EParticleMasksManager,
  VXS.ExplosionFx,
  VXS.Extrusion,
  VXS.FBORenderer,
  VXS.Feedback,
  VXS.FireFX,
  VXS.FPSMovement,
  VXS.GameMenu,
  VXS.GeomObjects,
  VXS.Gizmo,
  VXS.Graph,
  VXS.Graphics,
  VXS.Gui,
  VXS.HeightData,
  VXS.HeightTileFileHDS,
  VXS.HiddenLineShader,
  VXS.HUDObjects,
  VXS.Imposter,
  VXS.LensFlare,
  VXS.LinePFX,
  VXS.Material,
  VXS.MaterialEx,
  VXS.MaterialMultiProxy,
  VXS.MaterialScript,
  VXS.Mesh,
  VXS.Mirror,
  VXS.MultiMaterialShader,
  VXS.MultiPolygon,
  VXS.MultiProxy,
  VXS.Navigator,
  VXS.Nodes,
  VXS.Objects,
  VXS.OutlineShader,
  VXS.ParticleFX,
  VXS.Particles,
  VXS.Perlin,
  VXS.PerlinPFX,
  VXS.PhongShader,
  VXS.Polyhedron,
  VXS.Portal,
  VXS.PostEffects,
  VXS.ProjectedTextures,
  VXS.ProxyObjects,
  VXS.RenderContextInfo,
  VXS.ArchiveManager,
  VXS.Screen,
  VXS.ScriptBase,
  VXS.ShaderCombiner,
  VXS.ShadowHDS,
  VXS.ShadowPlane,
  VXS.ShadowVolume,
  VXS.SimpleNavigation,
  VXS.SkyBox,
  VXS.Skydome,
  VXS.Language,
  VXS.GLSLBumpShader,
  VXS.GLSLDiffuseSpecularShader,
  VXS.GLSLPostShaders,
  VXS.GLSLProjectedTextures,
  VXS.GLSLShader,
  VXS.SmoothNavigator,
  VXS.SMWaveOut,
  VXS.State,
  VXS.Strings,
  VXS.Teapot,
  VXS.TerrainRenderer,
  VXS.TexCombineShader,
  VXS.TexLensFlare,
  VXS.Texture,
  VXS.TexturedHDS,
  VXS.TextureImageEditors,
  VXS.TextureSharingShader,
  VXS.ThorFX,
  VXS.TilePlane,
  VXS.TimeEventsMgr,
  VXS.Trail,
  VXS.Tree,
  VXS.Types,
  VXS.FileTIN,
  VXS.UserShader,
  VXS.Utils,
  VXS.VectorFileObjects,
  VXS.VfsPAK,
  VXS.WaterPlane,
  VXS.Windows,
  VXS.WindowsFont,
  VXS.zBuffer,
  VXS.VectorTypes,
  VXS.VectorGeometry,
  // Image file formats
  DDSImage,
  VXS.FileTGA,
  // Vector file formats
  VXS.File3DS,
  VXS.FileASE,
  VXS.FileB3D,
  VXS.FileGL2,
  VXS.FileGTS,
  VXS.FileLMTS,
  VXS.FileLWO,
  VXS.FileMD2,
  VXS.FileMD3,
  VXS.FileMD5,
  VXS.FileMDC,
  VXS.FileMS3D,
  VXS.FileNMF,
  VXS.FileNurbs,
  VXS.FileObj,
  VXS.FileOCT,
  VXS.FilePLY,
  VXS.FileQ3BSP,
  VXS.FileSMD,
  VXS.FileSTL,
  VXS.FileVRML,

  // Sound file formats
  VXS.FileWAV,
  VXS.FileMP3,

  // Raster file format
  VXS.FileDDS,
  VXS.FileO3TC,
  VXS.FileHDR,
  VXS.FileJPEG,
  VXS.FilePNG,
  VXS.FileBMP,
  VXS.FileTGA,

  VXS.Sound,
  VXS.SoundFileObjects,
  VXS.SpaceText,
  VXS.Joystick,
  VXS.ScreenSaver,
  VXS.FullScreenViewer,
  VXS.Log;

var
  vObjectManager: TVXObjectManager;

function ObjectManager: TVXObjectManager;
begin
  if not Assigned(vObjectManager) then
    vObjectManager := TVXObjectManager.Create(nil);
  Result := vObjectManager;
end;

{$IFDEF VXS_REGION}{$REGION 'TOpenGLCategory'}{$ENDIF}
{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TVXSceneViewerEditor'}{$ENDIF}
// ExecuteVerb
//

procedure TVXSceneViewerEditor.ExecuteVerb(Index: Integer);
var
  source: TVXSceneViewer;
begin
  source := Component as TVXSceneViewer;
  case Index of
    0:
      source.Buffer.ShowInfo;
  end;
end;

// GetVerb
//

function TVXSceneViewerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show context info';
  end;
end;

// GetVerbCount
//

function TVXSceneViewerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TVXSceneEditor'}{$ENDIF}

procedure TVXSceneEditor.Edit;
begin
  with VXSceneEditorForm do
  begin
    SetScene(Self.Component as TVXScene, Self.Designer);
    Show;
  end;
end;

procedure TVXSceneEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

function TVXSceneEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Scene Editor';
  end;
end;

function TVXSceneEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TResolutionProperty'}{$ENDIF}

function TResolutionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;


function TResolutionProperty.GetValue: string;
begin
  Result := vVideoModes[GetOrdValue].Description;
end;

procedure TResolutionProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := 0 to vNumberVideoModes - 1 do
    Proc(vVideoModes[i].Description);
end;

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

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}

function TVXTextureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties];
end;


function TVXTextureImageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TVXTextureImageProperty.Edit;
begin
  if EditTextureImage(TVXTextureImage(GetOrdValue)) then
    Designer.Modified;
end;

{$IFDEF VXS_REGION}{$REGION 'TVXImageClassProperty'}{$ENDIF}

function TVXImageClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TVXImageClassProperty.GetValues(Proc: TGetStrProc);
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

function TVXImageClassProperty.GetValue: string;
begin
  Result := FindGLTextureImageClass(GetStrValue).FriendlyName;
end;

procedure TVXImageClassProperty.SetValue(const Value: string);
var
  tic: TVXTextureImageClass;
begin
  tic := FindGLTextureImageClassByFriendlyName(Value);
  if Assigned(tic) then
    SetStrValue(tic.ClassName)
  else
    SetStrValue('');
  Modified;
end;

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TVXColorProperty'}{$ENDIF}

procedure TVXColorProperty.Edit;
var
  colorDialog: TColorDialog;
  VXS.Color: TVXColor;
begin
  colorDialog := TColorDialog.Create(nil);
  try
    VXS.Color := TVXColor(GetOrdValue);
{$IFDEF WIN32}
    colorDialog.Options := [cdFullOpen];
{$ENDIF}
    colorDialog.Color := ConvertColorVector(VXS.Color.Color);
    if colorDialog.Execute then
    begin
      VXS.Color.Color := ConvertWinColor(colorDialog.Color);
      Modified;
    end;
  finally
    colorDialog.Free;
  end;
end;

function TVXColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paValueList, paDialog];
end;

procedure TVXColorProperty.GetValues(Proc: TGetStrProc);
begin
  ColorManager.EnumColors(Proc);
end;

function TVXColorProperty.GetValue: string;
begin
  Result := ColorManager.GetColorName(TVXColor(GetOrdValue).Color);
end;

procedure TVXColorProperty.SetValue(const Value: string);
begin
  TVXColor(GetOrdValue).Color := ColorManager.GetColor(Value);
  Modified;
end;

function TVXColorProperty.ColorToBorderColor(aColor: TColorVector;
  selected: Boolean): TColor;
begin
  if (aColor.X > 0.75) or (aColor.Y > 0.75) or (aColor.Z > 0.75) then
    Result := clBlack
  else if selected then
    Result := clWhite
  else
    Result := ConvertColorVector(aColor);
end;

procedure TVXColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TVXColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
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

procedure TVXColorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M');
end;

procedure TVXColorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // Nothing
end;

procedure TVXColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TSoundFileProperty'}{$ENDIF}

function TSoundFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TSoundFileProperty.GetValue: string;
var
  sample: TVXSoundSample;
begin
  sample := GetComponent(0) as TVXSoundSample;
  if sample.Data <> nil then
    Result := '(' + sample.Data.ClassName + ')'
  else
    Result := '(empty)';
end;


procedure TSoundFileProperty.Edit;
var
  ODialog: TOpenDialog;
  sample: TVXSoundSample;
  Desc, F: string;
begin
  sample := GetComponent(0) as TVXSoundSample;
  ODialog := TOpenDialog.Create(nil);
  try
    GetGLSoundFileFormats.BuildFilterStrings(TVXSoundFile, Desc, F);
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

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}

//---------------------------------------------------------

function TSoundNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TSoundNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  source: TVXBaseSoundSource;
begin
  source := (GetComponent(0) as TVXBaseSoundSource);
  if Assigned(source.SoundLibrary) then
    with source.SoundLibrary do
      for i := 0 to Samples.Count - 1 do
        Proc(Samples[i].Name);
end;

//---------------------------------------------------------
{ TVXCoordinatesProperty }
//--------------------------------------------------------
function TVXCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

procedure TVXCoordinatesProperty.Edit;
var
  glc: TVXCoordinates;
  x, y, z: Single;
begin
  glc := TVXCoordinates(GetOrdValue);
  x := glc.x;
  y := glc.y;
  z := glc.z;
  if VectorEditorForm.Execute(x, y, z) then
  begin
    glc.AsVector := VectorMake(x, y, z);
    Modified;
  end;
end;

//--------------------------------------------------------

{$IFDEF VXS_REGION}{$REGION 'TVXMaterialProperty'}{$ENDIF}

function TVXMaterialProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

procedure TVXMaterialProperty.Edit;
begin
  if FMaterialEditorForm.MaterialEditorForm.Execute(TVXMaterial(GetOrdValue))
  then
    Modified;
end;

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TVXGUILayoutEditor'}{$ENDIF}

procedure TVXGUILayoutEditor.Edit;
begin
  GUILayoutEditorForm.Execute(TVXGuiLayout(Self.Component));
end;

procedure TVXGUILayoutEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

function TVXGUILayoutEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Layout Editor';
  end;
end;

function TVXGUILayoutEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TReuseableDefaultEditor'}{$ENDIF}
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

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TVXMaterialLibraryEditor'}{$ENDIF}

// EditProperty
//
procedure TVXMaterialLibraryEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
begin
  if CompareText(Prop.GetName, 'MATERIALS') = 0 then
  begin
    FBest := Prop;
  end;
end;

procedure TVXMaterialLibraryEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

function TVXMaterialLibraryEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Material Library Editor';
  end;
end;

function TVXMaterialLibraryEditor.GetVerbCount: Integer;
begin
  Result := 1
end;

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TVXLibMaterialNameProperty'}{$ENDIF}
// GetAttributes
//

function TVXLibMaterialNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// Edit
//

procedure TVXLibMaterialNameProperty.Edit;
var
  buf: string;
  ml: TVXAbstractMaterialLibrary;
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

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TVXAnimationNameProperty'}{$ENDIF}
// GetAttributes
//

function TVXAnimationNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValues
//

procedure TVXAnimationNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  animControler: TVXAnimationControler;
  actor: TVXActor;
begin
  animControler := (GetComponent(0) as TVXAnimationControler);
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

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TVXBaseSceneObjectSelectionEditor'}{$ENDIF}

procedure TVXBaseSceneObjectSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  i, j: Integer;
  comp: TVXBaseSceneObject;
begin
  if (Designer = nil) or (Designer.Root = nil) then
    Exit;

  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if (Designer.Root.Components[i] is TVXBaseSceneObject) then
    begin
      comp := TVXBaseSceneObject(Designer.Root.Components[i]);
      for j := 0 to comp.Behaviours.Count - 1 do
        Proc(FindUnitName(comp.Behaviours[j]));
      for j := 0 to comp.Effects.Count - 1 do
        Proc(FindUnitName(comp.Effects[j]));
    end;
  end;
end;

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TVXSoundLibrarySelectionEditor'}{$ENDIF}

procedure TVXSoundLibrarySelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  i, j: Integer;
  comp: TVXSoundLibrary;
begin
  if (Designer = nil) or (Designer.Root = nil) then
    Exit;

  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if (Designer.Root.Components[i] is TVXSoundLibrary) then
    begin
      comp := TVXSoundLibrary(Designer.Root.Components[i]);
      for j := 0 to comp.Samples.Count - 1 do
        if Assigned(comp.Samples[j].Data) then
          Proc(FindUnitName(comp.Samples[j].Data));
    end;
  end;
end;

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TVXSArchiveManagerEditor'}{$ENDIF}

procedure TVXSArchiveManagerEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
begin
  if CompareText(Prop.GetName, 'ARCHIVES') = 0 then
  begin
    FBest := Prop;
  end;
end;

procedure TVXSArchiveManagerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      Edit;
  end;
end;

function TVXSArchiveManagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show Archive Manager Editor';
  end;
end;

function TVXSArchiveManagerEditor.GetVerbCount: Integer;
begin
  Result := 1
end;

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TVXMaterialComponentNameProperty'}{$ENDIF}

procedure TVXMaterialComponentNameProperty.Edit;
var
  LOwner: IGLMaterialLibrarySupported;
  LItem: TVXBaseMaterialCollectionItem;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    LItem := TVXMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .Components.GetItemByName(GetStrValue);
    if Assigned(LItem) then
      Designer.SelectComponent(LItem);
    Modified;
  end;
end;

function TVXMaterialComponentNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TVXLibTextureNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    TVXMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TVXTextureImageEx);
    TVXMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TVXFrameBufferAttachment);
  end;
end;

procedure TVXLibSamplerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TVXMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TVXTextureSampler);
end;

procedure TVXLibCombinerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TVXMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TVXTextureCombiner);
end;

procedure TVXLibShaderNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TVXMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc, TVXShaderEx);
end;

procedure TVXLibAttachmentNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TVXMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TVXFrameBufferAttachment);
end;

procedure TVXLibAsmProgNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TVXMaterialLibraryEx(LOwner.GetMaterialLibrary)
      .GetNames(Proc, TVXASMVertexProgram);
end;

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TPictureFileProperty'}{$ENDIF}

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

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TShaderFileProperty'}{$ENDIF}

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

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TAsmProgFileProperty'}{$ENDIF}

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

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TUniformAutoSetProperty'}{$ENDIF}

function TUniformAutoSetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paFullWidthName];
end;

procedure TUniformAutoSetProperty.PassUniform(const S: string);
begin
  ShaderUniformEditor.AddUniform(TVXBaseShaderModel(GetComponent(0))
    .Uniforms[S]);
end;

procedure TUniformAutoSetProperty.Edit;
var
  LOwner: TVXBaseShaderModel;
begin
  LOwner := TVXBaseShaderModel(GetComponent(0));
  if LOwner.Enabled and LOwner.IsValid then
  begin
    with ShaderUniformEditor do
    begin
      Clear;
      LOwner.MaterialLibrary.GetNames(AddTextureName, TVXTextureImageEx);
      LOwner.MaterialLibrary.GetNames(AddTextureName, TVXFrameBufferAttachment);
      LOwner.MaterialLibrary.GetNames(AddSamplerName, TVXTextureSampler);
      LOwner.GetUniformNames(PassUniform);
      Execute;
    end;
  end;
end;

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF VXS_REGION}{$REGION 'TVXShaderEditorProperty'}{$ENDIF}

function TVXShaderEditorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

function TVXShaderEditorProperty.GetStrings: TStrings;
begin
  Result := TStrings(GetOrdValue);
end;

procedure TVXShaderEditorProperty.OnShaderCheck(Sender: TObject);
var
  LShader: TVXShaderEx;
  LContext: TVXContext;
begin
  SetStrings(GLShaderEditorForm.GLSLMemo.Lines);
  LShader := TVXShaderEx(GetComponent(0));
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

procedure TVXShaderEditorProperty.SetStrings(const Value: TStrings);
begin
  SetOrdValue(Longint(Value));
end;

procedure TVXShaderEditorProperty.Edit;
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

{$IFDEF VXS_REGION}{$ENDREGION}{$ENDIF}
// ******************************************************************************

procedure GLRegisterPropertiesInCategories;
begin

  { VXS.SceneViewer }
  // property types
{$IFDEF WIN32}
  RegisterPropertiesInCategory(sOpenVXCategoryName,
    [TypeInfo(TVXCamera), TypeInfo(TVXSceneBuffer), TypeInfo(TVSyncMode),
    TypeInfo(TVXScreenDepth)]); // TVXScreenDepth in GLWin32FullScreenViewer
{$ENDIF}
  // TVXSceneViewer
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXSceneViewer,
    ['*Render']);

  { VXS.Scene }
  RegisterPropertiesInCategory(sOpenVXCategoryName,
    [TypeInfo(TVXx.ObjectsSorting), TypeInfo(TVXProgressEvent),
    TypeInfo(TVXBehaviours), TypeInfo(TVXObjectEffects),
    TypeInfo(TDirectRenderEvent), TypeInfo(TVXCameraStyle),
    TypeInfo(TOnCustomPerspective), TypeInfo(TVXScene)]);
  RegisterPropertiesInCategory(sLayoutCategoryName,
    [TypeInfo(TVXx.ObjectsSorting), TypeInfo(TNormalDirection)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TVXVisibilityCulling), TypeInfo(TLightStyle), TypeInfo(TVXColor),
    TypeInfo(TNormalDirection), TypeInfo(TVXCameraStyle)]);
  // TVXBaseSceneObject
  RegisterPropertiesInCategory(sVisualCategoryName, TVXBaseSceneObject,
    ['Rotation', 'Direction', 'Position', 'Up', 'Scale', '*Angle', 'ShowAxes',
    'FocalLength']);
  // TVXSceneObject
  RegisterPropertiesInCategory(sVisualCategoryName, TVXSceneObject, ['Parts']);
  // TVXDirectOpenVX
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXDirectOpenVX,
    ['UseBuildList']);
  // TVXProxyObject
  RegisterPropertiesInCategory(sOpenVXCategoryName,
    [TypeInfo(TVXProxyObjectOptions)]);
  // TVXLightSource
  RegisterPropertiesInCategory(sVisualCategoryName, TVXLightSource,
    ['*Attenuation', 'Shining', 'Spot*']);
  // TVXCamera
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXCamera,
    ['TargetObject']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXCamera,
    ['DepthOfView', 'SceneScale']);
  // TVXNonVisualViewer
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXNonVisualViewer,
    ['*Render']);

  { VXS.Objects }
  RegisterPropertiesInCategory(sOpenVXCategoryName,
    [TypeInfo(TVXLinesNodes), TypeInfo(TLineNodesAspect),
    TypeInfo(TLineSplineMode), TypeInfo(TLinesOptions)]);
{$IFDEF WIN32} // unit GLSpaceText
  RegisterPropertiesInCategory(sLayoutCategoryName, [TypeInfo(TVXTextAdjust)]);
  RegisterPropertiesInCategory(sLocalizableCategoryName,
    [TypeInfo(TSpaceTextCharRange)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TLineSplineMode),
    TypeInfo(TCapType), TypeInfo(TNormalSmoothing),
    TypeInfo(TArrowHeadStackingStyle), TypeInfo(TVXTextAdjust)]);
{$ENDIF}
  // TVXDummyCube
  RegisterPropertiesInCategory(sLayoutCategoryName, TVXDummyCube,
    ['VisibleAtRunTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXDummyCube,
    ['CubeSize', 'VisibleAtRunTime']);
  // TVXPlane
  RegisterPropertiesInCategory(sVisualCategoryName, TVXPlane,
    ['*Offset', '*Tiles']);
  // TVXSprite
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXSprite, ['NoZWrite']);
  RegisterPropertiesInCategory(sLayoutCategoryName, TVXSprite, ['NoZWrite']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXSprite,
    ['AlphaChannel', 'Rotation']);
  // TVXNode
  RegisterPropertiesInCategory(sVisualCategoryName, TVXNode, ['X', 'Y', 'Z']);
  // TVXLines
  RegisterPropertiesInCategory(sVisualCategoryName, TVXLines,
    ['Antialiased', 'Division', 'Line*', 'NodeSize']);
  // TVXCube
  RegisterPropertiesInCategory(sVisualCategoryName, TVXCube, ['Cube*']);
  // TVXFrustrum
  RegisterPropertiesInCategory(sVisualCategoryName, TVXFrustrum,
    ['ApexHeight', 'Base*']);
  // TVXSpaceText
{$IFDEF WIN32} // unit GLSpaceText
  RegisterPropertiesInCategory(sVisualCategoryName, TVXSpaceText,
    ['AllowedDeviation', 'AspectRatio', 'Extrusion', 'Oblique', 'TextHeight']);
{$ENDIF}
  RegisterPropertiesInCategory(sVisualCategoryName, TVXSphere,
    ['Bottom', 'Radius', 'Slices', 'Stacks', 'Start', 'Stop']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXDisk,
    ['*Radius', 'Loops', 'Slices']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXCone,
    ['BottomRadius', 'Loops', 'Slices', 'Stacks']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXCylinder,
    ['*Radius', 'Loops', 'Slices', 'Stacks']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXCapsule,
    ['*Radius', 'Loops', 'Slices', 'Stacks']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXAnnulus,
    ['Bottom*', 'Loops', 'Slices', 'Stacks', 'Top*']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXTorus,
    ['*Radius', 'Rings', 'Sides']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXArrowLine,
    ['Bottom*', 'Loops', 'Slices', 'Stacks', 'Top*']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXPolygon, ['Division']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, [TypeInfo(TVXContourNodes),
    TypeInfo(TVXContours)]);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXContour, ['Division']);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TVXNodes), TypeInfo(TPipeNodesColorMode)]);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXRevolutionSolid,
    ['Division', 'Slices', 'YOffsetPerTurn']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXExtrusionSolid,
    ['Stacks']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXPipeNode,
    ['RadiusFactor']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXPipe,
    ['Division', 'Radius', 'Slices']);
  RegisterPropertiesInCategory(sOpenVXCategoryName,
    [TypeInfo(TVXActorAnimationMode), TypeInfo(TVXActorAnimations),
    TypeInfo(TMeshAutoCenterings), TypeInfo(TActorFrameInterpolation),
    TypeInfo(TVXActorAnimationReference), TypeInfo(TVXActor)]);
  RegisterPropertiesInCategory(sLayoutCategoryName,
    [TypeInfo(TMeshNormalsOrientation)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TMeshAutoCenterings), TypeInfo(TVXActorAnimationReference),
    TypeInfo(TMeshNormalsOrientation)]);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXFreeForm,
    ['UseMeshmaterials']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXAnimationControler,
    ['AnimationName']);
  RegisterPropertiesInCategory(sLinkageCategoryName, TVXAnimationControler,
    ['AnimationName']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXActorAnimation, ['*Frame']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXActor,
    ['*Frame*', 'Interval', 'OverlaySkeleton', 'UseMeshmaterials']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXActor,
    ['OverlaySkeleton']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, [TypeInfo(TMeshMode), TypeInfo(TVertexMode)]);
  RegisterPropertiesInCategory(sOpenVXCategoryName, [TypeInfo(TVXHeightFieldOptions)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TVXHeightFieldColorMode),
    TypeInfo(TVXSamplingScale), TypeInfo(TXYZGridLinesStyle), TypeInfo(TXYZGridParts)]);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXXYZGrid, ['Antialiased']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXXYZGrid, ['Antialiased', 'Line*']);

  RegisterPropertiesInCategory(sLayoutCategoryName, TVXParticles, ['VisibleAtRunTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXParticles,
    ['*Size', 'VisibleAtRunTime']);

  RegisterPropertiesInCategory(sOpenVXCategoryName,
    [TypeInfo(TVXSkyDomeBands), TypeInfo(TVXSkyDomeOptions),
    TypeInfo(TVXSkyDomeStars)]);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXSkyDomeBand,
    ['Slices', 'Stacks', '*Angle']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXSkyDomeStar,
    ['Dec', 'Magnitude', 'RA']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXEarthSkyDome,
    ['Slices', 'Stacks', 'SunElevation', 'Turbidity']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, [TypeInfo(TMirrorOptions),
    TypeInfo(TVXBaseSceneObject)]);
  RegisterPropertiesInCategory(sOpenVXCategoryName, [TypeInfo(TBlendingMode)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TBlendingMode), TypeInfo(TPFXLifeColors),
    TypeInfo(TSpriteColorMode)]);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXParticleFXRenderer,
    ['ZWrite']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXParticleFXRenderer,
    ['ZWrite']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TPFXLifeColor,
    ['LifeTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TPFXLifeColor,
    ['LifeTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXLifeColoredPFXManager,
    ['Acceleration', 'ParticleSize']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXPolygonPFXManager,
    ['NbSides']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXPointLightPFXManager,
    ['TexMapSize']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, [TypeInfo(TVXHeightDataSource)]);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXTerrainRenderer,
    ['*CLOD*', 'QualityDistance', 'Tile*']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, [TypeInfo(TVXMemoryViewer),
    TypeInfo(TVXSceneViewer), TypeInfo(TOptimise)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TOptimise)]);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXZShadows,
    ['DepthFade', '*Shadow', 'Soft', 'Tolerance']);
  RegisterPropertiesInCategory(sLayoutCategoryName, [TypeInfo(TTextLayout)]);
  RegisterPropertiesInCategory(sVisualCategoryName,
    [TypeInfo(TVXBitmapFont), TypeInfo(TTextLayout)]);
  RegisterPropertiesInCategory(sLocalizableCategoryName,
    [TypeInfo(TVXBitmapFont)]);
  RegisterPropertiesInCategory(sOpenVXCategoryName,
    [TypeInfo(TVXMaterial), TypeInfo(TVXMaterialLibrary),
    TypeInfo(TVXLibMaterials), TypeInfo(TTextureNeededEvent)]);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXLibMaterial,
    ['Texture2Name']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXLibMaterial,
    ['TextureOffset', 'TextureScale']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXMaterialLibrary,
    ['TexturePaths']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, [TypeInfo(TVXCadencer)]);
  RegisterPropertiesInCategory(sOpenVXCategoryName,
    [TypeInfo(TObjectCollisionEvent)]);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXFireFXManager,
    ['MaxParticles', 'NoZWrite', 'Paused', 'UseInterval']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXFireFXManager,
    ['Fire*', 'InitialDir', 'NoZWrite', 'Particle*', 'Paused']);
  RegisterPropertiesInCategory(sOpenVXCategoryName,
    [TypeInfo(TCalcPointEvent)]);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXThorFXManager,
    ['Maxpoints', 'Paused']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXThorFXManager,
    ['Core', 'Glow*', 'Paused', 'Target', 'Vibrate', 'Wildness']);
  RegisterPropertiesInCategory(sOpenVXCategoryName,
    [TypeInfo(TVXMagFilter), TypeInfo(TVXMinFilter)]);
  RegisterPropertiesInCategory(sLocalizableCategoryName,
    [TypeInfo(TVXBitmapFontRanges)]);
  RegisterPropertiesInCategory(sLocalizableCategoryName, TVXBitmapFontRange,
    ['*ASCII']);
  RegisterPropertiesInCategory(sLayoutCategoryName, TVXBitmapFont,
    ['Char*', '*Interval*', '*Space']);
  RegisterPropertiesInCategory(sLocalizableCategoryName, TVXBitmapFont,
    ['Glyphs']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXBitmapFont,
    ['Char*', '*Interval*', '*Space', 'Glyphs']);
  RegisterPropertiesInCategory(sOpenVXCategoryName, TVXBitmapHDS,
    ['MaxPoolSize']);
  RegisterPropertiesInCategory(sVisualCategoryName, TVXBitmapHDS, ['Picture']);
end;

procedure Register;
begin
  RegisterComponents('VXScene', [TVXScene, TVXSceneViewer, TVXMemoryViewer,
    TVXMaterialLibrary, TVXMaterialLibraryEx, TVXCadencer, TVXGuiLayout,
    TVXBitmapFont, TVXWindowsBitmapFont, TVXScriptLibrary, TVXSoundLibrary,
    TVXFullScreenViewer]);

  RegisterComponents('VXScene PFX', [TVXCustomPFXManager, TVXPolygonPFXManager,
    TVXPointLightPFXManager, TVXCustomSpritePFXManager, TVXPerlinPFXManager,
    TVXLinePFXManager, TVXFireFXManager, TVXThorFXManager,
    TVXEParticleMasksManager]);

  RegisterComponents('VXScene Utils', [TVXAsyncTimer, TVXStaticImposterBuilder,
    TVXCollisionManager, TVXAnimationControler, TVXAVIRecorder, TVXDCEManager,
    TVXFPSMovementManager, TVXMaterialScripter, TVXUserInterface, TVXNavigator,
    TVXSmoothNavigator, TVXSmoothUserInterface, TVXTimeEventsMGR,
    TVXApplicationFileIO, TVXVfsPAK, TVXSimpleNavigation, TVXGizmo,
    TVXCameraController, TVXSLanguage, TVXSLogger, TVXSArchiveManager,
    TVXJoystick, TVXScreenSaver, TVXSSynHiMemo]);

  RegisterComponents('VXScene Terrain', [TVXBitmapHDS, TVXCustomHDS,
    TVXHeightTileFileHDS, TVXBumpmapHDS, TVXPerlinHDS, TVXTexturedHDS,
    TVXAsyncHDS, TVXShadowHDS]);

  RegisterComponents('VXScene Shaders', [TVXTexCombineShader, TVXPhongShader,
    TVXUserShader, TVXHiddenLineShader, TVXCelShader, TVXOutlineShader,
    TVXMultiMaterialShader, TVXBumpShader, TVXGLSLShader,
    TVXSLDiffuseSpecularShader, TVXSLBumpShader, TVXAsmShader,
    TVXShaderCombiner, TVXTextureSharingShader, TVXSLPostBlurShader]);

  RegisterComponentEditor(TVXSceneViewer, TVXSceneViewerEditor);
  RegisterComponentEditor(TVXScene, TVXSceneEditor);
  RegisterComponentEditor(TVXMaterialLibrary, TVXMaterialLibraryEditor);
  RegisterComponentEditor(TVXMaterialLibraryEx, TVXMaterialLibraryEditor);
  RegisterComponentEditor(TVXSArchiveManager, TVXSArchiveManagerEditor);

  VKRegisterPropertiesInCategories;

  RegisterPropertyEditor(TypeInfo(TResolution), nil, '', TResolutionProperty);
  RegisterPropertyEditor(TypeInfo(TVXTexture), TVXMaterial, '',
    TVXTextureProperty);
  RegisterPropertyEditor(TypeInfo(TVXTextureImage), TVXTexture, '',
    TVXTextureImageProperty);
  RegisterPropertyEditor(TypeInfo(string), TVXTexture, 'ImageClassName',
    TVXImageClassProperty);

  RegisterPropertyEditor(TypeInfo(TVXSoundFile), TVXSoundSample, '',
    TSoundFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TVXBaseSoundSource, 'SoundName',
    TSoundNameProperty);

  RegisterPropertyEditor(TypeInfo(TVXCoordinates), nil, '',
    TVXCoordinatesProperty);

  RegisterPropertyEditor(TypeInfo(TVXColor), nil, '', TVXColorProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterial), nil, '', TVXMaterialProperty);
  RegisterComponentEditor(TVXGuiLayout, TVXGUILayoutEditor);

  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName), TVXMaterial, '',
    TVXLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName), TVXLibMaterial,
    'Texture2Name', TVXLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName), TVXSkyBox, '',
    TVXLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName), TVXEParticleMask, '',
    TVXLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName), TVXGameMenu, '',
    TVXLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName),
    TVXMaterialMultiProxyMaster, '', TVXLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName), TVXSLBumpShader, '',
    TVXLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName), TSpriteAnimation, '',
    TVXLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName), TVXMaterialProxy, '',
    TVXLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName), TVXActorProxy, '',
    TVXLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName), TVXFBORenderer, '',
    TVXLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXActorAnimationName), TVXAnimationControler,
    '', TVXAnimationNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName),
    TVXTextureSharingShaderMaterial, 'LibMaterialName',
    TVXLibMaterialNameProperty);
  RegisterSelectionEditor(TVXBaseSceneObject,
    TVXBaseSceneObjectSelectionEditor);
  RegisterSelectionEditor(TVXSoundLibrary, TVXSoundLibrarySelectionEditor);
  RegisterPropertyEditor(TypeInfo(TVXLibMaterialName), TVXLibMaterialProperty,
    'NextPass', TVXLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName),
    TVXTextureProperties, 'LibTextureName', TVXLibTextureNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName),
    TVXTextureProperties, 'LibSamplerName', TVXLibSamplerNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName),
    TVXMultitexturingProperties, 'LibCombinerName', TVXLibCombinerNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName),
    TVXMultitexturingProperties, 'LibAsmProgName', TVXLibAsmProgNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName), TVXShaderModel3,
    'LibVertexShaderName', TVXLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName), TVXShaderModel3,
    'LibFragmentShaderName', TVXLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName), TVXShaderModel4,
    'LibVertexShaderName', TVXLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName), TVXShaderModel4,
    'LibFragmentShaderName', TVXLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName), TVXShaderModel4,
    'LibGeometryShaderName', TVXLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName), TVXShaderModel5,
    'LibVertexShaderName', TVXLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName), TVXShaderModel5,
    'LibFragmentShaderName', TVXLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName), TVXShaderModel5,
    'LibGeometryShaderName', TVXLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName), TVXShaderModel5,
    'LibTessControlShaderName', TVXLibShaderNameProperty);
  RegisterPropertyEditor(TypeInfo(TVXMaterialComponentName), TVXShaderModel5,
    'LibTessEvalShaderName', TVXLibShaderNameProperty);

  RegisterPropertyEditor(TypeInfo(string), TVXTextureImageEx, 'SourceFile',
    TPictureFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TVXShaderEx, 'SourceFile',
    TShaderFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TVXASMVertexProgram, 'SourceFile',
    TAsmProgFileProperty);

  RegisterPropertyEditor(TypeInfo(Boolean), TVXBaseShaderModel,
    'AutoFillOfUniforms', TUniformAutoSetProperty);
  RegisterPropertyEditor(TypeInfo(TStringList), TVXShaderEx, 'Source',
    TVXShaderEditorProperty);
end;

function GetVXSceneVersion: string;
var
  LProject: IOTAProject;
  LExePath, LProjectPath, LSVN, LRevision: string;
begin
  LRevision := Copy(VXScene_REVISION, 12, 4);

  // will be assigned after project compilation
  // after each compilation get it from file \.svn\entries in 4-th line
  // and write to file VXSceneRevision
  // in both fail (no \.svn\entries or VXSceneRevision file) get a version value from GLScene.pas
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
            SaveToFile(LExePath + 'VXSceneRevision');
          end;
        finally
          Free;
        end;
  end
  else if FileExists(LExePath + 'VXSceneRevision') then
    try
      with TStringList.Create do
        try
          LoadFromFile(LExePath + 'VXSceneRevision');
          if (Count >= 1) and (Trim(Strings[0]) <> '') then
            LRevision := Trim(Strings[0]);
        finally
          Free;
        end;
    except
    end;

  // Finally
  Result := Format(VXScene_VERSION, [LRevision]);
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

SplashScreenServices.AddPluginBitmap(GetVXSceneVersion,
  LoadBitmap(HInstance, 'TVXScene'), False, 'MPL 2 license', 'SVN version');

VXS.CrossPlatform.IsDesignTime := True;
VXS.CrossPlatform.vProjectTargetName := GetProjectTargetName;
VXS.Color.vUseDefaultColorSets := True;
VXS.Coordinates.vUseDefaultCoordinateSets := True;
ReadVideoModes;

with ObjectManager do
begin
  CreateDefaultObjectIcons(HInstance);
  RegisterSceneObject(TVXCamera, 'Camera', '', HInstance);
  RegisterSceneObject(TVXLightSource, 'LightSource', '', HInstance);
  RegisterSceneObject(TVXDummyCube, 'DummyCube', '', HInstance);

  // Basic geometry
  RegisterSceneObject(TVXSprite, 'Sprite', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TVXPoints, 'Points', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TVXLines, 'Lines', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TVXPlane, 'Plane', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TVXPolygon, 'Polygon', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TVXCube, 'Cube', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TVXFrustrum, 'Frustrum', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TVXSphere, 'Sphere', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TVXDisk, 'Disk', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TVXCone, 'Cone', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TVXCylinder, 'Cylinder', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TVXCapsule, 'Capsule', strOCBasicGeometry, HInstance);
  RegisterSceneObject(TVXDodecahedron, 'Dodecahedron', strOCBasicGeometry,
    HInstance);
  RegisterSceneObject(TVXIcosahedron, 'Icosahedron', strOCBasicGeometry,
    HInstance);
  RegisterSceneObject(TVXOctahedron, 'Octahedron', strOCBasicGeometry,
    HInstance);
  RegisterSceneObject(TVXTetrahedron, 'Tetrahedron', strOCBasicGeometry,
    HInstance);
  RegisterSceneObject(TVXSuperellipsoid, 'Superellipsoid', strOCBasicGeometry,
    HInstance);

  // Advanced geometry
  RegisterSceneObject(TVXAnimatedSprite, 'Animated Sprite',
    strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TVXArrowLine, 'ArrowLine', strOCAdvancedGeometry,
    HInstance);
  RegisterSceneObject(TVXArrowArc, 'ArrowArc', strOCAdvancedGeometry,
    HInstance);
  RegisterSceneObject(TVXAnnulus, 'Annulus', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TVXExtrusionSolid, 'ExtrusionSolid',
    strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TVXMultiPolygon, 'MultiPolygon', strOCAdvancedGeometry,
    HInstance);
  RegisterSceneObject(TVXPipe, 'Pipe', strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TVXRevolutionSolid, 'RevolutionSolid',
    strOCAdvancedGeometry, HInstance);
  RegisterSceneObject(TVXTorus, 'Torus', strOCAdvancedGeometry, HInstance);

  // Mesh objects
  RegisterSceneObject(TVXActor, 'Actor', strOCMeshObjects, HInstance);
  RegisterSceneObject(TVXFreeForm, 'FreeForm', strOCMeshObjects, HInstance);
  RegisterSceneObject(TVXMesh, 'Mesh', strOCMeshObjects, HInstance);
  RegisterSceneObject(TVXTilePlane, 'TilePlane', strOCMeshObjects, HInstance);
  RegisterSceneObject(TVXPortal, 'Portal', strOCMeshObjects, HInstance);
  RegisterSceneObject(TVXTerrainRenderer, 'TerrainRenderer', strOCMeshObjects,
    HInstance);

  // Graph-plotting objects
  RegisterSceneObject(TVXFlatText, 'FlatText', strOCGraphPlottingObjects,
    HInstance);
  RegisterSceneObject(TVXHeightField, 'HeightField', strOCGraphPlottingObjects,
    HInstance);
  RegisterSceneObject(TVXXYZGrid, 'XYZGrid', strOCGraphPlottingObjects,
    HInstance);

  // Particle systems
  RegisterSceneObject(TVXParticles, 'Particles', strOCParticleSystems,
    HInstance);
  RegisterSceneObject(TVXParticleFXRenderer, 'PFX Renderer',
    strOCParticleSystems, HInstance);

  // Environment objects
  RegisterSceneObject(TVXEarthSkyDome, 'EarthSkyDome', strOCEnvironmentObjects,
    HInstance);
  RegisterSceneObject(TVXSkyDome, 'SkyDome', strOCEnvironmentObjects,
    HInstance);
  RegisterSceneObject(TVXSkyBox, 'SkyBox', strOCEnvironmentObjects, HInstance);
  RegisterSceneObject(TVXAtmosphere, 'Atmosphere', strOCEnvironmentObjects,
    HInstance);

  // HUD objects.
  RegisterSceneObject(TVXHUDSprite, 'HUD Sprite', strOCHUDObjects, HInstance);
  RegisterSceneObject(TVXHUDText, 'HUD Text', strOCHUDObjects, HInstance);
  RegisterSceneObject(TVXResolutionIndependantHUDText,
    'Resolution Independant HUD Text', strOCHUDObjects, HInstance);
  RegisterSceneObject(TVXAbsoluteHUDText, 'Absolute HUD Text', strOCHUDObjects,
    HInstance);
  RegisterSceneObject(TVXGameMenu, 'GameMenu', strOCHUDObjects, HInstance);
  RegisterSceneObject(TVXConsole, 'Console', strOCHUDObjects, HInstance);

  // GUI objects.
  RegisterSceneObject(TVXBaseControl, 'Root Control', strOCGuiObjects,
    HInstance);
  RegisterSceneObject(TVXPopupMenu, 'GLPopupMenu', strOCGuiObjects, HInstance);
  RegisterSceneObject(TVXForm, 'GLForm', strOCGuiObjects, HInstance);
  RegisterSceneObject(TVXPanel, 'GLPanel', strOCGuiObjects, HInstance);
  RegisterSceneObject(TVXButton, 'GLButton', strOCGuiObjects, HInstance);
  RegisterSceneObject(TVXCheckBox, 'GLCheckBox', strOCGuiObjects, HInstance);
  RegisterSceneObject(TVXEdit, 'GLEdit', strOCGuiObjects, HInstance);
  RegisterSceneObject(TVXLabel, 'GLLabel', strOCGuiObjects, HInstance);
  RegisterSceneObject(TVXAdvancedLabel, 'GLAdvancedLabel', strOCGuiObjects,
    HInstance);
  RegisterSceneObject(TVXScrollbar, 'GLScrollbar', strOCGuiObjects, HInstance);
  RegisterSceneObject(StringGrid, 'GLStringGrid', strOCGuiObjects,
    HInstance);
  RegisterSceneObject(TVXCustomControl, 'GLBitmapControl', strOCGuiObjects,
    HInstance);

  // Special objects
  RegisterSceneObject(TVXLensFlare, 'LensFlare', strOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TVXTextureLensFlare, 'TextureLensFlare',
    strOCSpecialObjects, HInstance);
  RegisterSceneObject(TVXMirror, 'Mirror', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TVXShadowPlane, 'ShadowPlane', strOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TVXShadowVolume, 'ShadowVolume', strOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TVXZShadows, 'ZShadows', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TVXSLTextureEmitter, 'GLSL Texture Emitter',
    strOCSpecialObjects, HInstance);
  RegisterSceneObject(TVXSLProjectedTextures, 'GLSL Projected Textures',
    strOCSpecialObjects, HInstance);
  RegisterSceneObject(TVXTextureEmitter, 'Texture Emitter', strOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TVXProjectedTextures, 'Projected Textures',
    strOCSpecialObjects, HInstance);
  RegisterSceneObject(TVXBlur, 'Blur', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TVXMotionBlur, 'MotionBlur', strOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TVXSpaceText, 'SpaceText', strOCDoodad, HInstance);
  RegisterSceneObject(TVXTrail, 'Trail', strOCSpecialObjects, HInstance);
  RegisterSceneObject(TVXPostEffect, 'PostEffect', strOCSpecialObjects,
    HInstance);
  RegisterSceneObject(TVXPostShaderHolder, 'PostShaderHolder',
    strOCSpecialObjects, HInstance);

  // Doodad objects.
  RegisterSceneObject(TVXTeapot, 'Teapot', strOCDoodad, HInstance);
  RegisterSceneObject(TVXTree, 'Tree', strOCDoodad, HInstance);
  RegisterSceneObject(TVXWaterPlane, 'WaterPlane', strOCDoodad, HInstance);

  // Proxy objects.
  RegisterSceneObject(TVXProxyObject, 'ProxyObject', strOCProxyObjects,
    HInstance);
  RegisterSceneObject(TVXColorProxy, 'ColorProxy', strOCProxyObjects,
    HInstance);
  RegisterSceneObject(TVXFreeFormProxy, 'FreeFormProxy', strOCProxyObjects,
    HInstance);
  RegisterSceneObject(TVXMaterialProxy, 'MaterialProxy', strOCProxyObjects,
    HInstance);
  RegisterSceneObject(TVXActorProxy, 'ActorProxy', strOCProxyObjects,
    HInstance);
  RegisterSceneObject(TVXMultiProxy, 'MultiProxy', strOCProxyObjects,
    HInstance);
  RegisterSceneObject(TVXMaterialMultiProxy, 'MaterialMultiProxy',
    strOCProxyObjects, HInstance);

  // Other objects.
  RegisterSceneObject(TVXDirectOpenVX, 'Direct OpenVX', '', HInstance);
  RegisterSceneObject(TVXRenderPoint, 'Render Point', '', HInstance);
  RegisterSceneObject(TVXImposter, 'Imposter Sprite', '', HInstance);
  RegisterSceneObject(TVXFeedback, 'OpenVX Feedback', '', HInstance);
  RegisterSceneObject(TVXFBORenderer, 'OpenVX FrameBuffer', '', HInstance);
end;

//=================================================================
finalization
//=================================================================

ObjectManager.Free;

end.
