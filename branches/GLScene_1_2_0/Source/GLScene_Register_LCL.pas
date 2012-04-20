
// This unit is part of the GLScene Project, http://glscene.org

{: GLSceneRegisterLCL<p>

   Registration unit for GLScene library components, property editors and
      IDE experts for Lazarus.<p>

   <b>History :</b><font size=-1><ul>
      <li>04/06/10 - Yar - Added GLSArchiveManager
                           Fixes for Linux x64
      <li>20/04/10 - Yar - Added GLSLanguage
      <li>08/04/10 - Yar - Added code belonged section GLS_EXPERIMENTAL
      <li>22/01/10 - Yar - Added GLCompositeImage, GLFileDDS, GLFileO3TC, GLFileHDR to uses
      <li>07/01/10 - DaStr - Added GLLCLFullScreenViewer and improved
                              TResolutionProperty (thanks Predator)
      <li>24/11/09 - DanB - Removed some more windows only units
      <li>22/11/09 - DaStr - Improved Unix compatibility (again)
      <li>17/11/09 - DaStr - Improved Unix compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
      <li>24/03/08 - DaStr - Initial version
   </ul></font>
}
unit GLScene_Register_LCL;

interface

{$I GLScene.inc}

uses
  Classes, GLScene_Manager_Objects, ComponentEditors, PropEdits, LResources, LCLType,
  LazIDEIntf, ProjectIntf, ProjectResourcesIntf, MacroIntf, resource, Laz_XMLCfg,
  Forms;

{$IFNDEF GLS_OPENGL_ES}
type
  // TGLLibMaterialNameProperty

  TGLLibMaterialNameProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
{$ENDIF}

procedure Register;

//: Auto-create for object manager
function ObjectManager: TObjectManager;

implementation

uses
  SysUtils, Dialogs, Graphics,
  // GLScene units
  GLScene_Base_Vector_Geometry, GLScene_Core, GLScene_Viewer,
  GLScene_Viewer_FullScreen, GLScene_Base_Classes,
  GLScene_Base_Strings, GLScene_Base_Coordinates, GLScene_Texture,
  GLScene_Material, GLScene_Screen,
  GLScene_Cadencer, GLScene_PropertyEditor_TextureImage, GLScene_Base_Color,
  GLScene_Platform, GLScene_MaterialEx,
  // GLScene - basic geometry
  GLScene_Objects, GLScene_ObjectsEx,
  // GLScene - advanced geometry
  GLScene_Objects_AnimatedSprite, GLScene_Objects_Extrusion,
  GLScene_Objects_MultiPolygon,
  // GLScene - mesh
  GLScene_Objects_VectorFile, GLScene_Objects_Mesh, GLScene_Objects_TilePlane,
  GLScene_Portal,
  // GLScene - terrain
  GLScene_Objects_Terrain, GLScene_HeightData, GLScene_HDS_HeightTileFile,
  GLScene_HDS_Bumpmap, GLScene_Perlin,
  GLScene_HDS_Textured, GLScene_HDS_Async, GLScene_HDS_Shadow,
  // GLScene - graph plotting
  GLScene_BitmapFont, GLScene_Objects_GraphPlotting, GLScene_BitmapFont_System,

  // GLScene - particles
  GLScene_Particles, GLScene_Fx_Particle, GLScene_PFX_Perlin,
  GLScene_PFX_Line, GLScene_Fx_Fire, GLScene_FX_Thor,
  GLScene_ParticleManager_Masks,
  // GLScene_Core - environment
  GLScene_Objects_Skydome, GLScene_Objects_SkyBox, GLScene_Objects_Atmosphere,
  // GLScene - hud
  GLScene_Objects_HUD, GLScene_GameMenu,
  GLScene_GUI_Console,
  // GLScene - gui
  GLScene_GUI_Windows, GLScene_GUI,
  // GLScene - special
  GLScene_LensFlare, GLScene_LensFlare_Textured, GLScene_Mirror,
  GLScene_ShadowPlane, GLScene_ShadowVolume,
  GLScene_ZBuffer, GLScene_Texture_Projected_GLSL, GLScene_Texture_Projected,
  GLScene_Blur,
  GLScene_Objects_Trail, GLScene_PostEffects,
  // GLScene - doodad
  GLScene_Objects_Teapot, GLScene_Objects_Tree, GLScene_Objects_WaterPlane,
  // GLScene - proxy
  GLScene_Objects_Proxy, GLScene_Objects_MultiProxy, GLScene_Material_MultiProxy,
  // GLScene - shaders
  GLScene_Shader_TextureCombiner, GLScene_Shader_Phong, GLScene_Shader_User,
  GLScene_Shader_GLSL_Components,
  GLScene_Shader_HiddenLine, GLScene_Shader_Cel, GLScene_Shader_Outline,
  GLScene_Shader_MultiMaterial,
  GLScene_Shader_Bump, GLScene_Shader_GLSL_DiffuseSpecular,
  GLScene_Shader_GLSL_Bump, GLScene_Shader_GLSL_PostBlur,
  GLScene_Shader_Assm, GLScene_Shader_Combiner, GLScene_Shader_TextureSharing,
  // GLScene - other
  GLScene_Imposter, GLScene_Objects_Feedback, GLScene_Collision,
  GLScene_Base_Script, GLScene_AsyncTimer, GLScene_DCE,
  GLScene_Movement_FPS, GLScene_Material_Script, GLScene_Navigator,
  GLScene_SmoothNavigator,
  GLScene_TimeEvents_Manager, GLScene_Base_FileIO, GLScene_VfsPAK,
  GLScene_SimpleNavigation,
  GLScene_CameraController, GLScene_Gizmo, GLScene_GizmoEx, GLScene_Objects_FrameBuffer,
  GLScene_Sound_FileObjects, GLScene_Sound, GLScene_Image_Composite,
  GLScene_Base_Log, GLScene_Language,
  GLScene_Archive_Manager,

  // Image file formats
  GLScene_Image_DDS, GLScene_Image_HDR, GLScene_Image_O3TC,

  // Vector file formats
  GLScene_Files__3DS, GLScene_Files_ASE, GLScene_Files_B3D, GLScene_Files_GL2,
  GLScene_Files_GTS, GLScene_Files_LMTS,
  GLScene_Files_LWO, GLScene_Files_MD2, GLScene_Files_MD3, GLScene_Files_MD5,
  GLScene_Files_MDC, GLScene_Files_MS3D, GLScene_Files_NMF,
  GLScene_Files_Nurbs, GLScene_Files_OBJ, GLScene_Files_PLY,
  GLScene_Files_SMD, GLScene_Files_STL,
  GLScene_Files_TIN, GLScene_Files_VRML, GLScene_Files_X,

  // Sound file formats
  GLScene_Files_WAV, GLScene_Files_MP3,

  // Raster file format
  GLScene_Files_DDS, GLScene_Files_O3TC, GLScene_Files_HDR, GLScene_Files_BMP,
  GLScene_Files_TGA,

  // Property editor forms
  GLScene_Edit_LCL, GLScene_PropertyEditor_Vector_LCL,
  GLScene_PropertyEditor_Material_LCL, GLScene_PropertyEditor_MaterialPreview_LCL,
  GLScene_PropertyEditor_MaterialPicker_LCL, GLScene_PropertyEditor_Texture_LCL,
  GLScene_PropertyEditor_Face_LCL,
  GLScene_PropertyEditor_Color_LCL, GLScene_PropertyEditor_TrackBar_LCL,
  GLScene_PropertyEditor_Uniform_VCL, GLScene_GUI_LayoutEditor,
  GLScene_Utils;


var
  vObjectManager: TObjectManager;

function ObjectManager: TObjectManager;
begin
  if not Assigned(vObjectManager) then
    vObjectManager := TObjectManager.Create(nil);
  Result := vObjectManager;
end;

{$IFNDEF GLS_OPENGL_ES}
type
  // TGLSceneViewerEditor

  TGLSceneViewerEditor = class(TComponentEditor)
  public
    { Public Declarations }
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
  end;

  // TGLSceneEditor

  TGLSceneEditor = class(TComponentEditor)
  public
    { Public Declarations }
    procedure Edit; override;

    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
  end;

  // TResolutionProperty

  TResolutionProperty = class(TPropertyEditor)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  // TClassProperty

  TGLTextureProperty = class(TClassProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
  end;

  // TGLTextureImageProperty

  TGLTextureImageProperty = class(TClassProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TGLImageClassProperty

  TGLImageClassProperty = class(TClassProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TGLColorProperty = class(TClassProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Edit; override;

    function ColorToBorderColor(aColor: TColorVector; selected: boolean): TColor;
    procedure ListMeasureWidth(const AValue: ansistring; Index: integer;
      ACanvas: TCanvas; var AWidth: integer); override;
    procedure ListMeasureHeight(const AValue: ansistring; Index: integer;
      ACanvas: TCanvas; var AHeight: integer); override;
    procedure ListDrawValue(const AValue: ansistring; Index: integer;
      ACanvas: TCanvas; const ARect: TRect; AState: TPropEditDrawState); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState: TPropEditDrawState); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  // TSoundFileProperty

  TSoundFileProperty = class(TClassProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  // TSoundNameProperty

  TSoundNameProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  // TGLCoordinatesProperty

  TGLCoordinatesProperty = class(TClassProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TGLMaterialProperty

  TGLMaterialProperty = class(TClassProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TReuseableDefaultEditor

   {: Editor copied from DsgnIntf.<p>
      Could have been avoided, if only that guy at Borland didn't chose to
      publish only half of the stuff (and that's not the only class with
      that problem, most of the subitems handling code in TGLSceneBaseObject is
      here for the same reason...), the "protected" wasn't meant just to lure
      programmers into code they can't reuse... Arrr! and he did that again
      in D6! Grrr... }

  // TGLMaterialLibraryEditor

  {: Editor for material library.<p> }

  TGLMaterialLibraryEditor = class(TDefaultComponentEditor)
  public
    { Public Declarations }
    procedure EditProperty(const Prop: TPropertyEditor; var Continue: boolean);
      override;
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
  end;

  // TGLAnimationNameProperty

  TGLAnimationNameProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(proc: TGetStrProc); override;
  end;

  // TGLSArchiveManagerEditor

  TGLSArchiveManagerEditor = class(TDefaultComponentEditor)
  public
    { Public Declarations }
    procedure Edit; override;
    procedure EditProperty(const Prop: TPropertyEditor; var Continue: boolean);
      override;
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
  end;

  // TGLMaterialComponentNameProperty


  TGLMaterialComponentNameProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TGLLibTextureNameProperty = class(TGLMaterialComponentNameProperty)
  public
    { Public Declarations }
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibSamplerNameProperty = class(TGLMaterialComponentNameProperty)
  public
    { Public Declarations }
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibCombinerNameProperty = class(TGLMaterialComponentNameProperty)
  public
    { Public Declarations }
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibShaderNameProperty = class(TGLMaterialComponentNameProperty)
  public
    { Public Declarations }
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibAttachmentNameProperty = class(TGLMaterialComponentNameProperty)
  public
    { Public Declarations }
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TGLLibAsmProgNameProperty = class(TGLMaterialComponentNameProperty)
  public
    { Public Declarations }
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  // TPictureFileProperty

  TPictureFileProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TShaderFileProperty

  TShaderFileProperty = class(TStringProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TUniformAutoSetProperty

  TUniformAutoSetProperty = class(TPropertyEditor)
  private
    procedure PassUniform(const S: string);
    procedure PassBlock(const S: string);
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  // TGLGUILayoutEditor

  TGLGUILayoutEditor = class(TComponentEditor)
  public
    { Public Declarations }
    procedure Edit; override;

    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
  end;

//----------------- TGLSceneViewerEditor ---------------------------------------

// ExecuteVerb

procedure TGLSceneViewerEditor.ExecuteVerb(Index: integer);
var
  Source: TGLSceneViewer;
begin
  Source := Component as TGLSceneViewer;
  case Index of
    0: Source.Buffer.ShowInfo;
  end;
end;

// GetVerb

function TGLSceneViewerEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Show context info';
  end;
end;

// GetVerbCount

function TGLSceneViewerEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

//----------------- TGLSceneEditor ---------------------------------------------

// Edit

procedure TGLSceneEditor.Edit;
begin
  with GLSceneEditorForm do
  begin
    SetScene(Self.Component as TGLScene, TComponentEditorDesigner(Self.Designer));
    Show;
  end;
end;

// ExecuteVerb

procedure TGLSceneEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: Edit;
  end;
end;

// GetVerb

function TGLSceneEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Show Scene Editor';
  end;
end;

// GetVerbCount

function TGLSceneEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

//----------------- TResolutionProperty ----------------------------------------

// GetAttributes

function TResolutionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValue

function TResolutionProperty.GetValue: string;
begin
{$IFDEF MSWINDOWS}
  Result := vVideoModes[GetOrdValue].Description;
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
  //Testing!!!
  with vVideoModes[GetOrdValue]^ do
    Result := IntToStr(hdisplay) + ' x ' + IntToStr(vdisplay) + ', ' + '0 bpp';
{$ENDIF}
{$IFDEF Darwin}
  Result := '';
{$MESSAGE Warn 'Needs to be implemented'}
 {$ENDIF}

end;

// GetValues

procedure TResolutionProperty.GetValues(Proc: TGetStrProc);
var
  i: integer;
begin
{$IFDEF MSWINDOWS}
  for i := 0 to vNumberVideoModes - 1 do
    Proc(vVideoModes[i].Description);
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
  for i := 0 to vNumberVideoModes - 1 do
    with vVideoModes[i]^ do
      Proc(IntToStr(hdisplay) + 'x' + IntToStr(vdisplay) + 'x' + '0');
{$ENDIF}
{$IFDEF Darwin}
{$MESSAGE Warn 'Needs to be implemented'}
{$ENDIF}
end;

// SetValue

procedure TResolutionProperty.SetValue(const Value: string);

const
  Nums = ['0'..'9'];

var
  XRes, YRes, BPP: integer;
  Pos, SLength: integer;
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
        if not (TempStr[Pos] in Nums) then
          Break;
      if Pos <= SLength then
      begin
        // found a number?
        XRes := StrToInt(Copy(TempStr, 1, Pos - 1));
        // search for following non-numerics
        for Pos := Pos to SLength do
          if TempStr[Pos] in Nums then
            Break;
        Delete(TempStr, 1, Pos - 1); // take it out of the String
        SLength := Length(TempStr); // rest length of String
        if SLength > 1 then // something to scan?
        begin
          // determine second number
          for Pos := 1 to SLength do
            if not (TempStr[Pos] in Nums) then
              Break;
          if Pos <= SLength then
          begin
            YRes := StrToInt(Copy(TempStr, 1, Pos - 1));
            // search for following non-numerics
            for Pos := Pos to SLength do
              if TempStr[Pos] in Nums then
                Break;
            Delete(TempStr, 1, Pos - 1); // take it out of the String
            SLength := Length(TempStr); // rest length of String
            if SLength > 1 then
            begin
              for Pos := 1 to SLength do
                if not (TempStr[Pos] in Nums) then
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

//----------------- TGLTextureProperty -----------------------------------------

function TGLTextureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties];
end;

//----------------- TGLTextureImageProperty ------------------------------------

// GetAttributes

function TGLTextureImageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// Edit

procedure TGLTextureImageProperty.Edit;
begin
  if EditGLTextureImage(TGLTextureImage(GetObjectValue)) then
    Modified;
end;

//----------------- TGLImageClassProperty --------------------------------------

// GetAttributes

function TGLImageClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValues

procedure TGLImageClassProperty.GetValues(proc: TGetStrProc);
var
  i: integer;
  sl: TStrings;
begin
  sl := GetGLTextureImageClassesAsStrings;
  try
    for i := 0 to sl.Count - 1 do
      proc(sl[i]);
  finally
    sl.Free;
  end;
end;

// GetValue

function TGLImageClassProperty.GetValue: string;
begin
  Result := FindGLTextureImageClass(GetStrValue).FriendlyName;
end;

// SetValue

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

//----------------- TGLColorproperty -----------------------------------------------------------------------------------

procedure TGLColorProperty.Edit;
var
  colorDialog: TColorDialog;
  glColor: TGLColor;
begin
  colorDialog := TColorDialog.Create(nil);
  try
    glColor := TGLColor(GetObjectValue);
{$IFNDEF FPC}{$IFDEF WIN32}
    colorDialog.Options := [cdFullOpen];
{$ENDIF}{$ENDIF}
    colorDialog.Color := ConvertColorVector(glColor.Color);
    if colorDialog.Execute then
    begin
      glColor.Color := ConvertWinColor(colorDialog.Color);
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
  Result := ColorManager.GetColorName(TGLColor(GetObjectValue).Color);
end;

procedure TGLColorProperty.SetValue(const Value: string);
begin
  TGLColor(GetObjectValue).Color := ColorManager.GetColor(Value);
  Modified;
end;

// ColorToBorderColor

function TGLColorProperty.ColorToBorderColor(aColor: TColorVector;
  selected: boolean): TColor;
begin
  if (aColor[0] > 0.75) or (aColor[1] > 0.75) or (aColor[2] > 0.75) then
    Result := clBlack
  else if selected then
    Result := clWhite
  else
    Result := ConvertColorVector(AColor);
end;

procedure TGLColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  AState: TPropEditDrawState);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, GetOrdValue, ACanvas, ARect, [pedsSelected]);
end;

procedure TGLColorProperty.ListMeasureWidth(const AValue: ansistring;
  Index: integer; ACanvas: TCanvas; var AWidth: integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M');
end;

procedure TGLColorProperty.ListMeasureHeight(const AValue: ansistring;
  Index: integer; ACanvas: TCanvas; var AHeight: integer);
begin
  // Nothing
end;

procedure TGLColorProperty.ListDrawValue(const AValue: ansistring;
  Index: integer; ACanvas: TCanvas; const ARect: TRect; AState: TPropEditDrawState);
var
  vRight: integer;
  vOldPenColor, vOldBrushColor: TColor;
  Color: TColorVector;
begin
  vRight := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
  begin
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;

    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

    Color := ColorManager.GetColor(AValue);
    Brush.Color := ConvertColorVector(Color);
    Pen.Color := ColorToBorderColor(Color, pedsSelected in AState);

    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

    Brush.Color := vOldBrushColor;
    Pen.Color := vOldPenColor;
  end;
end;

// GetAttributes

function TSoundFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// GetValue

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

//----------------- TSoundNameProperty -----------------------------------------

// GetAttributes

function TSoundNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValues

procedure TSoundNameProperty.GetValues(Proc: TGetStrProc);
var
  i: integer;
  Source: TGLBaseSoundSource;
begin
  Source := (GetComponent(0) as TGLBaseSoundSource);
  if Assigned(Source.SoundLibrary) then
    with Source.SoundLibrary do
      for i := 0 to Samples.Count - 1 do
        Proc(Samples[i].Name);
end;

//----------------- TGLCoordinatesProperty -------------------------------------

// GetAttributes

function TGLCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

// Edit;

procedure TGLCoordinatesProperty.Edit;
var
  glc: TGLCoordinates;
  x, y, z: single;
begin
  glc := TGLCoordinates(GetObjectValue);
  x := glc.x;
  y := glc.y;
  z := glc.z;
  if VectorEditorForm.Execute(x, y, z) then
  begin
    glc.AsVector := VectorMake(x, y, z);
    Modified;
  end;
end;

//----------------- TGLMaterialProperty --------------------------------------------------------------------------------

// GetAttributes

function TGLMaterialProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

// Edit

procedure TGLMaterialProperty.Edit;
var
  ml: TGLMaterial;
begin
  ml := TGLMaterial(GetObjectValue);
  if GLScene_PropertyEditor_Material_LCL.MaterialEditorForm.Execute(ml) then
    Modified;
end;

//----------------- TGLMaterialLibraryEditor --------------------------------------------------------------------------------

// EditProperty

procedure TGLMaterialLibraryEditor.EditProperty(const Prop: TPropertyEditor;
  var Continue: boolean);
begin
  BestEditEvent := 'MATERIALS';
  inherited;
end;

// ExecuteVerb

procedure TGLMaterialLibraryEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: Edit;
  end;
end;

// GetVerb

function TGLMaterialLibraryEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Show Material Library Editor';
  end;
end;

//----------------- TGLLibMaterialNameProperty ---------------------------------

// GetAttributes

function TGLLibMaterialNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// Edit

procedure TGLLibMaterialNameProperty.Edit;
var
  buf: string;
  ml: TGLAbstractMaterialLibrary;
  obj: TPersistent;
  Int: IGLMaterialLibrarySupported;
begin

  buf := GetStrValue;
  obj := GetComponent(0);
  if Supports(Obj, IGLMaterialLibrarySupported, Int) then
    ml := Int.GetMaterialLibrary
  else
  begin
    ml := nil;
    Assert(False, 'oops, unsupported...');
  end;
  if not Assigned(ml) then
    ShowMessage('Select the material library first.')
  else if LibMaterialPicker.Execute(buf, ml) then
  begin
    SetStrValue(buf);
    Modified;

  end;
end;

//----------------- TGLAnimationNameProperty -----------------------------------

// GetAttributes

function TGLAnimationNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

// GetValues

procedure TGLAnimationNameProperty.GetValues(proc: TGetStrProc);
var
  i: integer;
  animControler: TGLAnimationControler;
  actor: TGLActor;
begin
  animControler := (GetComponent(0) as TGLAnimationControler);
  if Assigned(animControler) then
  begin
    actor := animControler.Actor;
    if Assigned(actor) then
      with actor.Animations do
      begin
        for i := 0 to Count - 1 do
          proc(Items[i].Name);
      end;
  end;
end;

//----------------- TGLSArchiveManagerEditor --------------------------------------------------------------------------------

// EditProperty

procedure TGLSArchiveManagerEditor.EditProperty(const Prop: TPropertyEditor;
  var Continue: boolean);
begin
  BestEditEvent := 'ARCHIVES';
  inherited;
end;
// Edit

procedure TGLSArchiveManagerEditor.Edit;
begin
  inherited;
end;

// ExecuteVerb

procedure TGLSArchiveManagerEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: Edit;
  end;
end;

// GetVerb

function TGLSArchiveManagerEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Show Archive Manager Editor';
  end;
end;

{$REGION 'TGLMaterialComponentNameProperty'}

procedure TGLMaterialComponentNameProperty.Edit;
var
  LOwner: IGLMaterialLibrarySupported;
  LItem: TGLBaseMaterialCollectionItem;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
  begin
    LItem := TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).Components.GetItemByName(
      GetStrValue);
    if Assigned(LItem) then
      GlobalDesignHook.SelectOnlyThis(LItem);
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
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc, TGLTextureImageEx);
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc,
      TGLFrameBufferAttachment);
  end;
end;

procedure TGLLibSamplerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc, TGLTextureSampler);
end;

procedure TGLLibCombinerNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc, TGLTextureCombiner);
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
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc,
      TGLFrameBufferAttachment);
end;

procedure TGLLibAsmProgNameProperty.GetValues(Proc: TGetStrProc);
var
  LOwner: IGLMaterialLibrarySupported;
begin
  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
    TGLMaterialLibraryEx(LOwner.GetMaterialLibrary).GetNames(Proc,
      TGLASMVertexProgram);
end;

{$ENDREGION}

{$REGION 'TPictureFileProperty'}

function TPictureFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TPictureFileProperty.Edit;
var
  LFileName: string;
begin
  LFileName := TGLTextureImageEx(GetComponent(0)).SourceFile;
  if OpenPictureDialog(LFileName) then
  begin
    SetStrValue(RelativePath(LFileName));
  end;
  Modified;
end;

{$ENDREGION}

{$REGION 'TPictureFileProperty'}

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

{$ENDREGION}

{$REGION 'TUniformAutoSetProperty'}

function TUniformAutoSetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paFullWidthName];
end;

procedure TUniformAutoSetProperty.PassUniform(const S: string);
begin
  ShaderUniformEditor.AddUniform(TGLBaseShaderModel(GetComponent(0)).Uniforms[S]);
end;

procedure TUniformAutoSetProperty.PassBlock(const S: string);
begin
  ShaderUniformEditor.AddUniformBlock(TGLBaseShaderModel(GetComponent(0)).UniformBlocks[S]);
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
      LOwner.GetUniformNames(PassUniform, PassBlock);
      Execute;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TGLGUILayoutEditor'}

procedure TGLGUILayoutEditor.Edit;
begin
  GUILayoutEditorForm.Execute(TGLGuiLayout(Self.Component));
end;

procedure TGLGUILayoutEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TGLGUILayoutEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Show Layout Editor';
  end;
end;

function TGLGUILayoutEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

{$ENDREGION}
{$ENDIF}

function GetProjectTargetName: string;
begin
  Result := '$(TargetFile)';
  if not Assigned(IDEMacros) or not IDEMacros.SubstituteMacros(Result) then
    Result := '';
end;

procedure Register;
begin
  RegisterComponents('GLScene',
    [TGLScene, TGLSceneViewer, TGLMemoryViewer, TGLMaterialLibrary,
    TGLCadencer, TGLGuiLayout, TGLBitmapFont, TGLSystemBitmapFont,
    TGLScriptLibrary, TGLSoundLibrary,
    TGLFullScreenViewer, TGLMaterialLibraryEx]);

  RegisterComponents('GLScene PFX',
    [TGLCustomPFXManager, TGLPolygonPFXManager, TGLPointLightPFXManager,
    TGLCustomSpritePFXManager, TGLPerlinPFXManager, TGLLinePFXManager,
    TGLFireFXManager, TGLThorFXManager, TGLEParticleMasksManager]);

  RegisterComponents('GLScene Utils',
    [TAsyncTimer, TGLStaticImposterBuilder, TCollisionManager,
    TGLAnimationControler, TGLDCEManager, TGLFPSMovementManager,
    TGLMaterialScripter, TGLUserInterface, TGLNavigator, TGLSmoothNavigator,
    TGLSmoothUserInterface, TGLTimeEventsMGR, TApplicationFileIO,
    TGLVfsPAK, TGLSimpleNavigation, TGLCameraController, TGLGizmo,
    TGLGizmoEx, TGLSLogger, TGLSLanguage, TGLSArchiveManager]);

  RegisterComponents('GLScene Terrain',
    [TGLBitmapHDS, TGLCustomHDS, TGLHeightTileFileHDS, TGLBumpmapHDS,
    TGLPerlinHDS, TGLTexturedHDS, TGLAsyncHDS, TGLShadowHDS]);

  RegisterComponents('GLScene Shaders',
    [TGLTexCombineShader, TGLPhongShader, TGLUserShader, TGLHiddenLineShader,
    TGLCelShader, TGLOutlineShader, TGLMultiMaterialShader, TGLBumpShader,
    TGLSLShader, TGLSLDiffuseSpecularShader, TGLSLBumpShader, TGLAsmShader,
    TGLShaderCombiner, TGLTextureSharingShader, TGLSLPostBlurShader]);

  {$IFNDEF GLS_OPENGL_ES}
  RegisterComponentEditor(TGLSceneViewer, TGLSceneViewerEditor);
  RegisterComponentEditor(TGLScene, TGLSceneEditor);
  RegisterComponentEditor(TGLGUILayout, TGLGUILayoutEditor);
  {$ENDIF}

  RegisterClasses([TGLCoordinates]);

  {$IFNDEF GLS_OPENGL_ES}
  RegisterComponentEditor(TGLMaterialLibrary, TGLMaterialLibraryEditor);
  RegisterComponentEditor(TGLSArchiveManager, TGLSArchiveManagerEditor);

  RegisterPropertyEditor(TypeInfo(TResolution), nil, '', TResolutionProperty);
  RegisterPropertyEditor(TypeInfo(TGLTexture), TGLMaterial, '', TGLTextureProperty);
  RegisterPropertyEditor(TypeInfo(TGLTextureImage), TGLTexture, '',
    TGLTextureImageProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLTexture, 'ImageClassName',
    TGLImageClassProperty);

  RegisterPropertyEditor(TypeInfo(TGLSoundFile), TGLSoundSample, '',
    TSoundFileProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLBaseSoundSource,
    'SoundName', TSoundNameProperty);

  RegisterPropertyEditor(TypeInfo(TGLCoordinates), nil, '', TGLCoordinatesProperty);

  RegisterPropertyEditor(TypeInfo(TGLColor), nil, '', TGLColorProperty);
  RegisterPropertyEditor(TypeInfo(TGLMaterial), nil, '', TGLMaterialProperty);

  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterial,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLLibMaterial,
    'Texture2Name', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSkyBox,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLEParticleMask,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLGameMenu,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterialMultiProxyMaster,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSLBumpShader,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TSpriteAnimation,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterialProxy,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLActorProxy,
    '', TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLFBORenderer, '',
    TGLLibMaterialNameProperty);
  RegisterPropertyEditor(TypeInfo(TActorAnimationName), TGLAnimationControler,
    '', TGLAnimationNameProperty);
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName),
    TGLTextureSharingShaderMaterial, 'LibMaterialName', TGLLibMaterialNameProperty);

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
  RegisterPropertyEditor(TypeInfo(boolean), TGLBaseShaderModel,
    'AutoFillOfUniforms', TUniformAutoSetProperty);
  {$ENDIF}

  with ObjectManager do
  begin
    RegisterSceneObject(TGLCamera, 'Camera', '', HInstance);
    RegisterSceneObject(TGLLightSource, 'LightSource', '', HInstance);
    RegisterSceneObject(TGLDummyCube, 'DummyCube', '', HInstance);

    // Basic Geometry
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
    RegisterSceneObject(TGLIcosahedron, 'Icosahedron', glsOCBasicGeometry, HInstance);

    //Advanced geometry
    RegisterSceneObject(TGLAnimatedSprite, 'Animated Sprite',
      glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLArrowLine, 'ArrowLine', glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLAnnulus, 'Annulus', glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLExtrusionSolid, 'ExtrusionSolid',
      glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLMultiPolygon, 'MultiPolygon',
      glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLPipe, 'Pipe', glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLRevolutionSolid, 'RevolutionSolid',
      glsOCAdvancedGeometry, HInstance);
    RegisterSceneObject(TGLTorus, 'Torus', glsOCAdvancedGeometry, HInstance);

    //Mesh objects
    RegisterSceneObject(TGLActor, 'Actor', glsOCMeshObjects, HInstance);
    RegisterSceneObject(TGLFreeForm, 'FreeForm', glsOCMeshObjects, HInstance);
    RegisterSceneObject(TGLMesh, 'Mesh', glsOCMeshObjects, HInstance);
    RegisterSceneObject(TGLTilePlane, 'TilePlane', glsOCMeshObjects, HInstance);
    RegisterSceneObject(TGLPortal, 'Portal', glsOCMeshObjects, HInstance);
    RegisterSceneObject(TGLTerrainRenderer, 'TerrainRenderer',
      glsOCMeshObjects, HInstance);

    //Graph-plotting objects
    RegisterSceneObject(TGLFlatText, 'FlatText', glsOCGraphPlottingObjects, HInstance);
    RegisterSceneObject(TGLHeightField, 'HeightField',
      glsOCGraphPlottingObjects, HInstance);
    RegisterSceneObject(TGLXYZGrid, 'XYZGrid', glsOCGraphPlottingObjects, HInstance);

    //Particle systems
    RegisterSceneObject(TGLParticles, 'Particles', glsOCParticleSystems, HInstance);
    RegisterSceneObject(TGLParticleFXRenderer, 'PFX Renderer',
      glsOCParticleSystems, HInstance);

    //Environment objects
    RegisterSceneObject(TGLEarthSkyDome, 'EarthSkyDome',
      glsOCEnvironmentObjects, HInstance);
    RegisterSceneObject(TGLSkyDome, 'SkyDome', glsOCEnvironmentObjects, HInstance);
    RegisterSceneObject(TGLSkyBox, 'SkyBox', glsOCEnvironmentObjects, HInstance);
    RegisterSceneObject(TGLAtmosphere, 'Atmosphere', glsOCEnvironmentObjects,
      HInstance);

    // HUD objects.
    RegisterSceneObject(TGLHUDSprite, 'HUD Sprite', glsOCHUDObjects, HInstance);
    RegisterSceneObject(TGLHUDText, 'HUD Text', glsOCHUDObjects, HInstance);
    RegisterSceneObject(TGLResolutionIndependantHUDText,
      'Resolution Independant HUD Text', glsOCHUDObjects, HInstance);
    RegisterSceneObject(TGLAbsoluteHUDText, 'Absolute HUD Text',
      glsOCHUDObjects, HInstance);
    RegisterSceneObject(TGLGameMenu, 'GameMenu', glsOCHUDObjects, HInstance);
    RegisterSceneObject(TGLConsole, 'Console', glsOCHUDObjects, HInstance);

    // GUI objects.
    RegisterSceneObject(TGLBaseControl, 'Root Control', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLPopupMenu, 'GLPopupMenu', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLForm, 'GLForm', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLPanel, 'GLPanel', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLButton, 'GLButton', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLCheckBox, 'GLCheckBox', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLEdit, 'GLEdit', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLLabel, 'GLLabel', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLAdvancedLabel, 'GLAdvancedLabel',
      glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLScrollbar, 'GLScrollbar', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLStringGrid, 'GLStringGrid', glsOCGuiObjects, HInstance);
    RegisterSceneObject(TGLCustomControl, 'GLBitmapControl',
      glsOCGuiObjects, HInstance);

    //Special objects
    RegisterSceneObject(TGLLensFlare, 'LensFlare', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLTextureLensFlare, 'TextureLensFlare',
      glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLMirror, 'Mirror', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLShadowPlane, 'ShadowPlane', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLShadowVolume, 'ShadowVolume',
      glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLZShadows, 'ZShadows', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLSLTextureEmitter, 'GLSL Texture Emitter',
      glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLSLProjectedTextures, 'GLSL Projected Textures',
      glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLTextureEmitter, 'Texture Emitter',
      glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLProjectedTextures, 'Projected Textures',
      glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLBlur, 'Blur', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLMotionBlur, 'MotionBlur', glsOCSpecialObjects, HInstance);

    RegisterSceneObject(TGLTrail, 'GLTrail', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLPostEffect, 'PostEffect', glsOCSpecialObjects, HInstance);
    RegisterSceneObject(TGLPostShaderHolder, 'PostShaderHolder',
      glsOCSpecialObjects, HInstance);

    // Doodad objects.
    RegisterSceneObject(TGLTeapot, 'Teapot', glsOCDoodad, HInstance);
    RegisterSceneObject(TGLTree, 'Tree', glsOCDoodad, HInstance);
    RegisterSceneObject(TGLWaterPlane, 'WaterPlane', glsOCDoodad, HInstance);

    // Proxy objects.
    RegisterSceneObject(TGLProxyObject, 'ProxyObject', glsOCProxyObjects, HInstance);
    RegisterSceneObject(TGLColorProxy, 'ColorProxy', glsOCProxyObjects, HInstance);
    RegisterSceneObject(TGLFreeFormProxy, 'FreeFormProxy',
      glsOCProxyObjects, HInstance);
    RegisterSceneObject(TGLMaterialProxy, 'MaterialProxy',
      glsOCProxyObjects, HInstance);
    RegisterSceneObject(TGLActorProxy, 'ActorProxy', glsOCProxyObjects, HInstance);
    RegisterSceneObject(TGLMultiProxy, 'MultiProxy', glsOCProxyObjects, HInstance);
    RegisterSceneObject(TGLMaterialMultiProxy, 'MaterialMultiProxy',
      glsOCProxyObjects, HInstance);

    // Other objects.
    RegisterSceneObject(TGLDirectOpenGL, 'Direct OpenGL', '', HInstance);
    RegisterSceneObject(TGLRenderPoint, 'Render Point', '', HInstance);
    RegisterSceneObject(TGLImposter, 'Imposter Sprite', '', HInstance);
    RegisterSceneObject(TGLFeedback, 'OpenGL Feedback', '', HInstance);
    RegisterSceneObject(TGLFBORenderer, 'OpenGL FrameBuffer', '', HInstance);

  end;
end;

initialization

{$I GLSceneLCL.lrs}

  GLScene_Base_Color.vUseDefaultColorSets := True;
  GLScene_Base_Coordinates.vUseDefaultCoordinateSets := True;
  GLScene_Platform.IsDesignTime := True;
  GLScene_Platform.vProjectTargetName := GetProjectTargetName;

finalization

  ObjectManager.Free;

end.

