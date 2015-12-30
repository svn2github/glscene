//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : GLSceneRegister<p>

  Registration unit for GLScene library components, property editors and
  IDE experts.<p>

  <b>History : </b><font size=-1><ul>
  <li>28/12/15 - EG - Imported and Updated Form GLScene
  </ul></font>
}
unit DGLEngineRegister;

interface

{$I DGLEngine.inc}

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  VCL.Forms,
  VCL.Dialogs,
  VCL.Controls,
  VCL.StdCtrls,
  VCL.Graphics,

  ToolsAPI,
  DesignIntf,
  DesignEditors,
  VCLEditors,

  // DGLE
  DGLCrossPlatform,
  DGLResStrings,
  DGLTypes,
  DGLScene,
  DGLContext,
  DGLColor,
  DGLObjectManager;

type

  // TReuseableDefaultEditor
  //
  { : Editor copied from DsgnIntf.<p>
    Could have been avoided, if only that guy at Borland didn't chose to
    publish only half of the stuff (and that's not the only class with
    that problem, most of the subitems handling code in TDGLSceneBaseObject is
    here for the same reason...), the "protected" wasn't meant just to lure
    programmers into code they can't reuse... Arrr! and he did that again
    in D6! Grrr... }
  TReuseableDefaultEditor = class(TComponentEditor, IDefaultEditor)
  protected
    { Protected Declarations }
    FFirst:    IProperty;
    FBest:     IProperty;
    FContinue: Boolean;
    procedure CheckEdit(const Prop: IProperty);
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); virtual;

  public
    { Public Declarations }
    procedure Edit; override;
  end;

  //========================================================================================
  //======================================== MATERIAL ======================================
  //========================================================================================

//  // TDGLLibMaterialNameProperty
//  //
//  TDGLLibMaterialNameProperty = class(TStringProperty)
//  public
//    { Public Declarations }
//    function GetAttributes: TPropertyAttributes; override;
//    procedure Edit; override;
//  end;
//
//  // TDGLMaterialProperty
//  //
//  TDGLMaterialProperty = class(TClassProperty)
//  public
//    { Public Declarations }
//    function GetAttributes: TPropertyAttributes; override;
//    procedure Edit; override;
//  end;
//
//  // TDGLMaterialLibraryEditor
//  //
//  { : Editor for material library.<p> }
//  TDGLMaterialLibraryEditor = class(TReuseableDefaultEditor, IDefaultEditor)
//  protected
//    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
//  public
//    procedure ExecuteVerb(Index: Integer); override;
//    function GetVerb(Index: Integer): string; override;
//    function GetVerbCount: Integer; override;
//  end;
//
//  // TDGLMaterialComponentNameProperty
//  //
//  TDGLMaterialComponentNameProperty = class(TStringProperty)
//  public
//    { Public Declarations }
//    function GetAttributes: TPropertyAttributes; override;
//    procedure Edit; override;
//  end;
//
//  TDGLLibTextureNameProperty = class(TDGLMaterialComponentNameProperty)
//  public
//    { Public Declarations }
//    procedure GetValues(Proc: TGetStrProc); override;
//  end;
//
//  TDGLLibSamplerNameProperty = class(TDGLMaterialComponentNameProperty)
//  public
//    { Public Declarations }
//    procedure GetValues(Proc: TGetStrProc); override;
//  end;
//
//  TDGLLibCombinerNameProperty = class(TDGLMaterialComponentNameProperty)
//  public
//    { Public Declarations }
//    procedure GetValues(Proc: TGetStrProc); override;
//  end;
//
//  TDGLLibAttachmentNameProperty = class(TDGLMaterialComponentNameProperty)
//  public
//    { Public Declarations }
//    procedure GetValues(Proc: TGetStrProc); override;
//  end;
//
//  // TTextureProperty
//  //
//  TDGLTextureProperty = class(TClassProperty)
//  public
//    { Protected Declarations }
//    function GetAttributes: TPropertyAttributes; override;
//  end;
//
//  // TDGLTextureImageProperty
//  //
//  TDGLTextureImageProperty = class(TClassProperty)
//  public
//    { Protected Declarations }
//    function GetAttributes: TPropertyAttributes; override;
//    procedure Edit; override;
//  end;
//
//  // TPictureFileProperty
//  //
//  TPictureFileProperty = class(TStringProperty)
//  public
//    { Public Declarations }
//    function GetAttributes: TPropertyAttributes; override;
//    procedure Edit; override;
//  end;

  //========================================================================================
  //======================================== SHADER ========================================
  //========================================================================================

//  // TDGLLibMaterialNameProperty
//  //
//  TDGLLibShaderNameProperty = class(TStringProperty)
//  public
//    { Public Declarations }
//    function GetAttributes: TPropertyAttributes; override;
//    procedure Edit; override;
//  end;
//
//  // TDGLMaterialProperty
//  //
////  TDGLShaderProperty = class(TClassProperty)
////  public
////    { Public Declarations }
////    function GetAttributes: TPropertyAttributes; override;
////    procedure Edit; override;
////  end;
//
//  // TDGLMaterialLibraryEditor
//  //
//  { : Editor for material library.<p> }
//  TDGLShaderLibraryEditor = class(TReuseableDefaultEditor, IDefaultEditor)
//  protected
//    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
//  public
//    procedure ExecuteVerb(Index: Integer); override;
//    function GetVerb(Index: Integer): string; override;
//    function GetVerbCount: Integer; override;
//  end;
//
//  // TDGLMaterialComponentNameProperty
//  //
//  TDGLShaderComponentNameProperty = class(TStringProperty)
//  public
//    { Public Declarations }
//    function GetAttributes: TPropertyAttributes; override;
//    procedure Edit; override;
//  end;
//
//  // TShaderFileProperty
//  //
//  TShaderFileProperty = class(TStringProperty)
//  public
//    { Public Declarations }
//    function GetAttributes: TPropertyAttributes; override;
//    procedure Edit; override;
//  end;
//
//  // TUniformAutoSetProperty
//  //
//  TUniformAutoSetProperty = class(TPropertyEditor)
//  private
//    procedure PassUniform(const S: string);
//  public
//    { Public Declarations }
//    function GetAttributes: TPropertyAttributes; override;
//    procedure Edit; override;
//  end;
//
//  TDGLShaderEditorProperty = class(TClassProperty)
//  protected
//    { Protected declarations }
//    function GetStrings: TStrings;
//    procedure SetStrings(const Value: TStrings);
//    procedure OnShaderCheck(Sender: TObject);
//  public
//    { Public declarations }
//    function GetAttributes: TPropertyAttributes; override;
//    procedure Edit; override;
//  end;

  //========================================================================================
  //======================================== SCENE =========================================
  //========================================================================================

  // TDGLSceneViewerEditor
  //
  TDGLSceneViewerEditor = class(TComponentEditor)
  public
    { Public Declarations }
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  // TDGLSceneEditor
  //
  TDGLSceneEditor = class(TComponentEditor)
  public
    { Public Declarations }
    procedure Edit; override;

    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { : Selection editor for TDGLBaseSceneObject.<p>
    Allows units to be added to the uses clause automatically when
    behaviours/effects are added to a TDGLBaseSceneObject at design-time. }
  TDGLBaseSceneObjectSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  //========================================================================================
  //======================================= COMMON =========================================
  //========================================================================================
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

  // TDGLColorProperty
  //
  TDGLColorProperty = class(TClassProperty, ICustomPropertyDrawing, ICustomPropertyListDrawing)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    function ColorToBorderColor(aColor: TColorVector; selected: Boolean): TColor;

  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Edit; override;

    // ICustomPropertyListDrawing  stuff
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    // CustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  // TDGLCoordinatesProperty
  //
  TDGLCoordinatesProperty = class(TClassProperty)
  public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


  //========================================================================================
  //======================================== SOUND =========================================
  //========================================================================================
  // TSoundFileProperty
  //
  // TSoundFileProperty = class(TClassProperty)
  // public
  // { Public Declarations }
  // function GetAttributes: TPropertyAttributes; override;
  // function GetValue: string; override;
  // procedure Edit; override;
  // end;

  // TSoundNameProperty
  //
  // TSoundNameProperty = class(TStringProperty)
  // public
  // { Public Declarations }
  // function GetAttributes: TPropertyAttributes; override;
  // procedure GetValues(Proc: TGetStrProc); override;
  // end;


  //========================================================================================
  //======================================= OTHERS =========================================
  //========================================================================================


//******************************************************************************************

procedure Register;

// : Auto-create for object manager
function ObjectManager: TObjectManager;

resourcestring
  { OpenGL property category }
  sOpenGLCategoryName = 'OpenGL';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------


uses
  // DesignTime Editors
  //FDGLLibMaterialPicker,
  //FDGLLibShaderPicker,
  //FDGLShaderMemo,
  FDGLShaderUniformEditor,
  FDGLVectorEditor,
  DGLSceneEdit,

  // Base Units
  DGLBaseClasses,
  DGLRenderContextInfo,
  DGLState,
  DGLUtils,
  DGLApplicationFileIO,
  DGLGraphics,

  // Geometry Units
  DGLVectorTypes,
  DGLVectorMaths,
  DGLCoordinates,

  // Tools units
  DGLSLog,
  DGLCadencer,
  DGLSMemo,

  // Main Units
  DGLScreen,
  DGLViewer,
  DGLMaterial,
  DGLShader,
  DGLObjects,

  // Image file formats

  DGLDDSImage,
  DGLTGA,
  DGLDXTC,
  DGLLibPNG,
  DGLHDRImage,
  DGLSJPG,

  // Raster file format
  DGLFileDDS,
  DGLFileO3TC,
  DGLFileHDR,
  DGLFileJPEG,
  DGLFilePNG,
  DGLFileBMP,
  DGLFileTGA;

  // Vector file formats
//  GLFile3DS,
//  GLFileASE,
//  GLFileB3D,
//  GLFileGL2,
//  GLFileGTS,
//  GLFileLMTS,
//  GLFileLWO,
//  GLFileMD2,
//  GLFileMD3,
//  GLFileMD5,
//  GLFileMDC,
//  GLFileMS3D,
//  GLFileNMF,
//  GLFileNurbs,
//  GLFileObj,
//  GLFileOCT,
//  GLFilePLY,
//  GLFileQ3BSP,
//  GLFileSMD,
//  GLFileSTL,
//  GLFileVRML,

  // Sound file formats
//  GLFileWAV,
//  GLFileMP3,

//-------------------------------------------------------------------------------------------
var
  vObjectManager: TObjectManager;

function ObjectManager: TObjectManager;
begin
  if not Assigned(vObjectManager) then
    vObjectManager := TObjectManager.Create(nil);
  Result           := vObjectManager;
end;

//-------------------------------------------------------------------------------------------

// ------------------
{ TReuseableDefaultEditor }
{$IFDEF GLS_REGION}{$REGION 'TReuseableDefaultEditor'}{$ENDIF}

procedure TReuseableDefaultEditor.CheckEdit(const Prop: IProperty);
begin
  if FContinue then
    EditProperty(Prop, FContinue);
end;

procedure TReuseableDefaultEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
var
  PropName:       string;
  BestName:       string;
  MethodProperty: IMethodProperty;

  procedure ReplaceBest;
  begin
    FBest := Prop;
    if FFirst = FBest then
      FFirst := nil;
  end;

begin
  if not Assigned(FFirst) and Supports(Prop, IMethodProperty, MethodProperty) then
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

procedure TReuseableDefaultEditor.Edit;
var
  Components: IDesignerSelections;
begin
  Components := TDesignerSelections.Create;
  FContinue  := True;
  Components.Add(Component);
  FFirst := nil;
  FBest  := nil;
  try
    GetComponentProperties(Components, tkAny, Designer, CheckEdit);
    if FContinue then
      if Assigned(FBest) then
        FBest.Edit
      else if Assigned(FFirst) then
        FFirst.Edit;
  finally
    FFirst := nil;
    FBest  := nil;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

//======================================== MATERIAL ======================================

//// ------------------
//{ TDGLMaterialLibraryEditor }
//{$IFDEF GLS_REGION}{$REGION 'TDGLMaterialLibraryEditor'}{$ENDIF}
//
//procedure TDGLMaterialLibraryEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
//begin
//  if CompareText(Prop.GetName, 'MATERIALS') = 0 then
//  begin
//    FBest := Prop;
//  end;
//end;
//
//procedure TDGLMaterialLibraryEditor.ExecuteVerb(Index: Integer);
//begin
//  case Index of
//    0:
//      Edit;
//  end;
//end;
//
//function TDGLMaterialLibraryEditor.GetVerb(Index: Integer): string;
//begin
//  case Index of
//    0:
//      Result := 'Show Material Library Editor';
//  end;
//end;
//
//function TDGLMaterialLibraryEditor.GetVerbCount: Integer;
//begin
//  Result := 1
//end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TDGLLibMaterialNameProperty }
//{$IFDEF GLS_REGION}{$REGION 'TDGLLibMaterialNameProperty'}{$ENDIF}
//
//function TDGLLibMaterialNameProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paDialog];
//end;
//
//procedure TDGLLibMaterialNameProperty.Edit;
//var
//  buf: string;
//  ml:  TDGLAbstractMaterialLibrary;
//  obj: TPersistent;
//  Int: IDGLMaterialLibrarySupported;
//begin
//  buf := GetStrValue;
//  obj := GetComponent(0);
//  if Supports(obj, IDGLMaterialLibrarySupported, Int) then
//    ml := Int.GetMaterialLibrary
//  else
//  begin
//    ml := nil;
//    Assert(False, 'oops, unsupported...');
//  end;
//  if not Assigned(ml) then
//    ShowMessage('Select the material library first.')
//  else if DGLLibMaterialPicker.Execute(buf, ml) then
//    SetStrValue(buf);
//end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TDGLMaterialProperty }
//{$IFDEF GLS_REGION}{$REGION 'TDGLMaterialProperty'}{$ENDIF}
//
//function TDGLMaterialProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paDialog, paSubProperties];
//end;
//
//procedure TDGLMaterialProperty.Edit;
//begin
////  if FMaterialEditorForm.MaterialEditorForm.Execute(TDGLMaterial(GetOrdValue)) then Modified;
//end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TDGLMaterialComponentNameProperty }
//{$IFDEF GLS_REGION}{$REGION 'TDGLMaterialComponentNameProperty'}{$ENDIF}
//
//procedure TDGLMaterialComponentNameProperty.Edit;
//var
//  LOwner: IDGLMaterialLibrarySupported;
//  LItem:  TDGLBaseMaterialCollectionItem;
//begin
//  if Supports(GetComponent(0), IDGLMaterialLibrarySupported, LOwner) then
//  begin
//    LItem := TDGLMaterialLibrary(LOwner.GetMaterialLibrary).Components.GetItemByName(GetStrValue);
//    if Assigned(LItem) then
//      Designer.SelectComponent(LItem);
//    Modified;
//  end;
//end;
//
//function TDGLMaterialComponentNameProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paValueList];
//end;
//
//procedure TDGLLibTextureNameProperty.GetValues(Proc: TGetStrProc);
//var
//  LOwner: IDGLMaterialLibrarySupported;
//begin
//  if Supports(GetComponent(0), IDGLMaterialLibrarySupported, LOwner) then
//  begin
//    TDGLMaterialLibrary(LOwner.GetMaterialLibrary).GetNames(Proc, TDGLTexture);
//    TDGLMaterialLibrary(LOwner.GetMaterialLibrary).GetNames(Proc, TDGLFrameBufferAttachment);
//  end;
//end;
//
//procedure TDGLLibSamplerNameProperty.GetValues(Proc: TGetStrProc);
//var
//  LOwner: IDGLMaterialLibrarySupported;
//begin
//  if Supports(GetComponent(0), IDGLMaterialLibrarySupported, LOwner) then
//    TDGLMaterialLibrary(LOwner.GetMaterialLibrary).GetNames(Proc, TDGLTextureSampler);
//end;
//
//procedure TDGLLibCombinerNameProperty.GetValues(Proc: TGetStrProc);
//var
//  LOwner: IDGLMaterialLibrarySupported;
//begin
//  if Supports(GetComponent(0), IDGLMaterialLibrarySupported, LOwner) then
//    TDGLMaterialLibrary(LOwner.GetMaterialLibrary).GetNames(Proc, TDGLTextureCombiner);
//end;
//
////procedure TDGLLibShaderNameProperty.GetValues(Proc: TGetStrProc);
////var
////  LOwner: IGLMaterialLibrarySupported;
////begin
////  if Supports(GetComponent(0), IGLMaterialLibrarySupported, LOwner) then
////    TDGLMaterialLibrary(LOwner.GetMaterialLibrary).GetNames(Proc, TDGLLibShader);
////end;
//
//procedure TDGLLibAttachmentNameProperty.GetValues(Proc: TGetStrProc);
//var
//  LOwner: IDGLMaterialLibrarySupported;
//begin
//  if Supports(GetComponent(0), IDGLMaterialLibrarySupported, LOwner) then
//    TDGLMaterialLibrary(LOwner.GetMaterialLibrary).GetNames(Proc, TDGLFrameBufferAttachment);
//end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TPictureFileProperty }
//{$IFDEF GLS_REGION}{$REGION 'TPictureFileProperty'}{$ENDIF}
//
//function TPictureFileProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paDialog];
//end;
//
//procedure TPictureFileProperty.Edit;
//var
//  LFileName: string;
//begin
//  if OpenPictureDialog(LFileName) then
//  begin
//    SetStrValue(RelativePath(LFileName));
//  end;
//  Modified;
//end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TDGLTextureProperty }
//{$IFDEF GLS_REGION}{$REGION 'TDGLTextureProperty'}{$ENDIF}
//
//function TDGLTextureProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paSubProperties];
//end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TDGLTextureImageProperty }
//{$IFDEF GLS_REGION}{$REGION 'TDGLTextureImageProperty'}{$ENDIF}
//
//function TDGLTextureImageProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paDialog];
//end;
//
//procedure TDGLTextureImageProperty.Edit;
//begin
////  if EditDGLTextureImage(TDGLTexture(GetOrdValue)) then
////    Designer.Modified;
//end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TDGLImageClassProperty }
//{$IFDEF GLS_REGION}{$REGION 'TDGLImageClassProperty'}{$ENDIF}
//
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//

//======================================== SHADER ========================================

//// ------------------
//{ TDGLShaderLibraryEditor }
//{$IFDEF GLS_REGION}{$REGION 'TDGLShaderLibraryEditor'}{$ENDIF}
//
//
//procedure TDGLShaderLibraryEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
//begin
//  if CompareText(Prop.GetName, 'SHADERS') = 0 then
//  begin
//    FBest := Prop;
//  end;
//end;
//
//procedure TDGLShaderLibraryEditor.ExecuteVerb(Index: Integer);
//begin
//  case Index of
//    0: Edit;
//  end;
//end;
//
//function TDGLShaderLibraryEditor.GetVerb(Index: Integer): string;
//begin
//  case Index of
//    0: Result := 'Show Shader Library Editor';
//  end;
//end;
//
//function TDGLShaderLibraryEditor.GetVerbCount: Integer;
//begin
//  Result := 1
//end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TDGLLibShaderNameProperty }
//{$IFDEF GLS_REGION}{$REGION 'TDGLLibShaderNameProperty'}{$ENDIF}
//
//function TDGLLibShaderNameProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paDialog];
//end;
//
//procedure TDGLLibShaderNameProperty.Edit;
//var
//  buf: string;
//  ml:  TDGLAbstractShaderLibrary;
//  obj: TPersistent;
//  Int: IDGLShaderLibrarySupported;
//begin
//  buf := GetStrValue;
//  obj := GetComponent(0);
//  if Supports(obj, IDGLShaderLibrarySupported, Int) then
//    ml := Int.GetShaderLibrary
//  else
//  begin
//    ml := nil;
//    Assert(False, 'oops, unsupported...');
//  end;
//  if not Assigned(ml) then
//    ShowMessage('Select the Shader library first.')
//  else if DGLLibShaderPicker.Execute(buf, ml) then
//    SetStrValue(buf);
//end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TDGLShaderProperty }
//{$IFDEF GLS_REGION}{$REGION 'TDGLShaderProperty'}{$ENDIF}
//
////function TDGLShaderProperty.GetAttributes: TPropertyAttributes;
////begin
////  Result := [paDialog, paSubProperties];
////end;
////
////procedure TDGLShaderProperty.Edit;
////begin
////  if FShaderEditorForm.ShaderEditorForm.Execute(TDGLLibShader(GetOrdValue)) then
////    Modified;
////end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TDGLShaderComponentNameProperty }
//{$IFDEF GLS_REGION}{$REGION 'TDGLShaderComponentNameProperty'}{$ENDIF}
//
//procedure TDGLShaderComponentNameProperty.Edit;
//var
//  LOwner: IDGLShaderLibrarySupported;
//  LItem:  TDGLBaseShaderCollectionItem;
//begin
//  if Supports(GetComponent(0), IDGLShaderLibrarySupported, LOwner) then
//  begin
//    LItem := TDGLShaderLibrary(LOwner.GetShaderLibrary).Components.GetItemByName(GetStrValue);
//    if Assigned(LItem) then
//      Designer.SelectComponent(LItem);
//    Modified;
//  end;
//end;
//
//function TDGLShaderComponentNameProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paValueList];
//end;
//
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//
//// ------------------
//{  }
//{$IFDEF GLS_REGION}{$REGION 'TShaderFileProperty'}{$ENDIF}
//
//procedure TShaderFileProperty.Edit;
//var
//  ODialog: TOpenDialog;
//begin
//  ODialog := TOpenDialog.Create(nil);
//  try
//    ODialog.Filter := '*.glsl';
//    if ODialog.Execute then
//    begin
//      SetStrValue(RelativePath(ODialog.FileName));
//      Modified;
//    end;
//  finally
//    ODialog.Free;
//  end;
//end;
//
//function TShaderFileProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paDialog];
//end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{  }
//{$IFDEF GLS_REGION}{$REGION 'TUniformAutoSetProperty'}{$ENDIF}
//
//function TUniformAutoSetProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paDialog, paFullWidthName];
//end;
//
//procedure TUniformAutoSetProperty.PassUniform(const S: string);
//begin
//  DGLShaderUniformEditor.AddUniform(TDGLBaseShaderModel(GetComponent(0)).Uniforms[S]);
//end;
//
//procedure TUniformAutoSetProperty.Edit;
//var
//  LOwner: TDGLBaseShaderModel;
//begin
//  LOwner := TDGLBaseShaderModel(GetComponent(0));
//  if LOwner.Enabled and LOwner.IsValid then
//  begin
//    with DGLShaderUniformEditor do
//    begin
//      Clear;
//      LOwner.MaterialLibrary.GetNames(AddTextureName, TDGLTexture);
//      LOwner.MaterialLibrary.GetNames(AddTextureName, TDGLFrameBufferAttachment);
//      LOwner.MaterialLibrary.GetNames(AddSamplerName, TDGLTextureSampler);
//      LOwner.GetUniformNames(PassUniform);
//      Execute;
//    end;
//  end;
//end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{  }
//{$IFDEF GLS_REGION}{$REGION 'TDGLShaderEditorProperty'}{$ENDIF}
//
//function TDGLShaderEditorProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
//end;
//
//function TDGLShaderEditorProperty.GetStrings: TStrings;
//begin
//  Result := TStrings(GetOrdValue);
//end;
//
//procedure TDGLShaderEditorProperty.OnShaderCheck(Sender: TObject);
////var
////  LShader:  TDGLLibShader;
////  LContext: TDGLContext;
//begin
////  SetStrings(GLShaderEditorForm.GLSLMemo.Lines);
////  LShader  := TDGLLibShader(GetComponent(0));
////  LContext := LShader.ShaderModel.Handle.RenderingContext;
////  if Assigned(LContext) then
////  begin
////    LContext.Activate;
////    try
////      LShader.ShaderModel.DoOnPrepare(LContext);
////      GLShaderEditorForm.CompilatorLog.Lines.Add(LShader.ShaderModel.InfoLog);
////    finally
////      LContext.Deactivate;
////    end;
////  end
////  else
////    DGLShaderEditorForm.CompilatorLog.Lines.Add('There is no any rendering context for work with OpenGL');
//end;
//
//procedure TDGLShaderEditorProperty.SetStrings(const Value: TStrings);
//begin
//  SetOrdValue(Longint(Value));
//end;
//
//procedure TDGLShaderEditorProperty.Edit;
//begin
////  with DGLShaderEditorForm do
////  begin
////    OnCheck := OnShaderCheck;
////    GLSLMemo.Lines.Assign(GetStrings);
////    GLSLMemo.CurX := 0;
////    GLSLMemo.CurY := 0;
////    if ShowModal = mrOk then
////    begin
////      SetStrings(GLSLMemo.Lines);
////      Modified;
////    end;
////  end;
//end;
//
//{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
//

//======================================== SCENE =========================================

// ------------------
{ TDGLSceneViewerEditor }
{$IFDEF GLS_REGION}{$REGION 'TDGLSceneViewerEditor'}{$ENDIF}

procedure TDGLSceneViewerEditor.ExecuteVerb(Index: Integer);
var
  source: TDGLSceneViewer;
begin
  source := Component as TDGLSceneViewer;
  case Index of
    0:
      source.Buffer.ShowInfo;
  end;
end;

function TDGLSceneViewerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Show context info';
  end;
end;

function TDGLSceneViewerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSceneEditor }
{$IFDEF GLS_REGION}{$REGION 'TDGLSceneEditor'}{$ENDIF}

procedure TDGLSceneEditor.Edit;
begin
  with DGLSceneEditorForm do
  begin
    SetScene(Self.Component as TDGLScene, Self.Designer);
    Show;
  end;
end;

procedure TDGLSceneEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TDGLSceneEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show Scene Editor';
  end;
end;

function TDGLSceneEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBaseSceneObjectSelectionEditor }
{$IFDEF GLS_REGION}{$REGION 'TDGLBaseSceneObjectSelectionEditor'}{$ENDIF}

procedure TDGLBaseSceneObjectSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  i, j: Integer;
  comp: TDGLBaseSceneObject;
begin
  if (Designer = nil) or (Designer.Root = nil) then
    Exit;

  for i := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if (Designer.Root.Components[i] is TDGLBaseSceneObject) then
    begin
      comp  := TDGLBaseSceneObject(Designer.Root.Components[i]);
      for j := 0 to comp.Behaviours.Count - 1 do Proc(FindUnitName(comp.Behaviours[j]));
      for j := 0 to comp.Effects.Count - 1 do Proc(FindUnitName(comp.Effects[j]));
    end;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

//======================================= COMMON =========================================

// ------------------
{ TResolutionProperty }
{$IFDEF GLS_REGION}{$REGION 'TResolutionProperty'}{$ENDIF}

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
  for i := 0 to vNumberVideoModes - 1 do Proc(vVideoModes[i].Description);
end;

procedure TResolutionProperty.SetValue(const Value: string);
const
  Nums = ['0' .. '9'];
var
  XRes, YRes, BPP: Integer;
  Pos, SLength:    Integer;
  TempStr:         string;

begin
  if CompareText(Value, 'default') <> 0 then
  begin
    // initialize scanning
    TempStr := Trim(Value) + '|'; // ensure at least one delimiter
    SLength := Length(TempStr);
    XRes    := 0;
    YRes    := 0;
    BPP     := 0;
    // contains the string something?
    if SLength > 1 then
    begin
      // determine first number
      for Pos := 1 to SLength do
        if not(AnsiChar(TempStr[Pos]) in Nums) then Break;
      if Pos <= SLength then
      begin
        // found a number?
        XRes := StrToInt(Copy(TempStr, 1, Pos - 1));
        // search for following non-numerics
        for Pos := Pos to SLength do
          if AnsiChar(TempStr[Pos]) in Nums then Break;
        Delete(TempStr, 1, Pos - 1); // take it out of the String
        SLength := Length(TempStr); // rest length of String
        if SLength > 1 then // something to scan?
        begin
          // determine second number
          for Pos := 1 to SLength do
            if not(AnsiChar(TempStr[Pos]) in Nums) then Break;
          if Pos <= SLength then
          begin
            YRes := StrToInt(Copy(TempStr, 1, Pos - 1));
            // search for following non-numerics
            for Pos := Pos to SLength do
              if AnsiChar(TempStr[Pos]) in Nums then Break;
            Delete(TempStr, 1, Pos - 1); // take it out of the String
            SLength := Length(TempStr); // rest length of String
            if SLength > 1 then
            begin
              for Pos := 1 to SLength do
                if not(AnsiChar(TempStr[Pos]) in Nums) then Break;
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

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLColorProperty }
{$IFDEF GLS_REGION}{$REGION 'TDGLColorProperty'}{$ENDIF}

procedure TDGLColorProperty.Edit;
var
  colorDialog: TColorDialog;
  GLColor:     TDGLColor;
begin
  colorDialog := TColorDialog.Create(nil);
  try
    GLColor := TDGLColor(GetOrdValue);
    {$IFDEF WIN32}
    colorDialog.Options := [cdFullOpen];
    {$ENDIF}
    colorDialog.Color := ConvertColorVector(GLColor.Color);
    if colorDialog.Execute then
    begin
      GLColor.Color := ConvertWinColor(colorDialog.Color);
      Modified;
    end;
  finally
    colorDialog.Free;
  end;
end;

function TDGLColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paValueList, paDialog];
end;

procedure TDGLColorProperty.GetValues(Proc: TGetStrProc);
begin
  ColorManager.EnumColors(Proc);
end;

function TDGLColorProperty.GetValue: string;
begin
  Result := ColorManager.GetColorName(TDGLColor(GetOrdValue).Color);
end;

procedure TDGLColorProperty.SetValue(const Value: string);
begin
  TDGLColor(GetOrdValue).Color := ColorManager.GetColor(Value);
  Modified;
end;

function TDGLColorProperty.ColorToBorderColor(aColor: TColorVector; selected: Boolean): TColor;
begin
  if (aColor.V[0] > 0.75) or (aColor.V[1] > 0.75) or (aColor.V[2] > 0.75) then
    Result := clBlack
  else if selected then
    Result := clWhite
  else
    Result := ConvertColorVector(aColor);
end;

procedure TDGLColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TDGLColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  vRight:                       Integer;
  vOldPenColor, vOldBrushColor: TColor;
  Color:                        TColorVector;
begin
  vRight := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
    try
      vOldPenColor   := Pen.Color;
      vOldBrushColor := Brush.Color;

      Pen.Color := Brush.Color;
      Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

      Color       := ColorManager.GetColor(Value);
      Brush.Color := ConvertColorVector(Color);
      Pen.Color   := ColorToBorderColor(Color, ASelected);

      Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

      Brush.Color := vOldBrushColor;
      Pen.Color   := vOldPenColor;
    finally
      DefaultPropertyListDrawValue(Value, ACanvas, Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom), ASelected);
    end;
end;

procedure TDGLColorProperty.ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M');
end;

procedure TDGLColorProperty.ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
begin
  // Nothing
end;

procedure TDGLColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLCoordinatesProperty }
{$IFDEF GLS_REGION}{$REGION 'TDGLCoordinatesProperty'}{$ENDIF}

function TDGLCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

procedure TDGLCoordinatesProperty.Edit;
var
  glc:     TDGLCoordinates;
  x, y, z: Single;
begin
  glc := TDGLCoordinates(GetOrdValue);
  x   := glc.x;
  y   := glc.y;
  z   := glc.z;
  if DGLVectorEditorForm.Execute(x, y, z) then
  begin
    glc.AsVector := VectorMake(x, y, z);
    Modified;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

//======================================== SOUND =========================================

// ------------------
{ TSoundFileProperty }
{$IFDEF GLS_REGION}{$REGION 'TSoundFileProperty'}{$ENDIF}
// GetAttributes
//
//
//function TSoundFileProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paDialog];
//end;
//
//// GetValue
////
//
//function TSoundFileProperty.GetValue: string;
//var
//  sample: TDGLSoundSample;
//begin
//  sample := GetComponent(0) as TDGLSoundSample;
//  if sample.Data <> nil then
//    Result := '(' + sample.Data.ClassName + ')'
//  else
//    Result := '(empty)';
//end;
//
//// Edit
////
//
//procedure TSoundFileProperty.Edit;
//var
//  ODialog: TOpenDialog;
//  sample:  TDGLSoundSample;
//  Desc, F: string;
//begin
//  sample  := GetComponent(0) as TDGLSoundSample;
//  ODialog := TOpenDialog.Create(nil);
//  try
//    GeTDGLSoundFileFormats.BuildFilterStrings(TDGLSoundFile, Desc, F);
//    ODialog.Filter := Desc;
//    if ODialog.Execute then
//    begin
//      sample.LoadFromFile(ODialog.FileName);
//      Modified;
//    end;
//  finally
//    ODialog.Free;
//  end;
//end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TSoundNameProperty }
{$IFDEF GLS_REGION}{$REGION 'TSoundNameProperty'}{$ENDIF}
// GetAttributes
//

//function TSoundNameProperty.GetAttributes: TPropertyAttributes;
//begin
//  Result := [paValueList];
//end;
//
// GetValues
//
//
//procedure TSoundNameProperty.GetValues(Proc: TGetStrProc);
//var
//  i:      Integer;
//  source: TDGLBaseSoundSource;
//begin
//  source := (GetComponent(0) as TDGLBaseSoundSource);
//  if Assigned(source.SoundLibrary) then
//    with source.SoundLibrary do
//      for i := 0 to Samples.Count - 1 do
//        Proc(Samples[i].Name);
//end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSoundLibrarySelectionEditor }
{$IFDEF GLS_REGION}{$REGION 'TDGLSoundLibrarySelectionEditor'}{$ENDIF}

//procedure TDGLSoundLibrarySelectionEditor.RequiresUnits(Proc: TGetStrProc);
//var
//  i, j: Integer;
//  comp: TDGLSoundLibrary;
//begin
//  if (Designer = nil) or (Designer.Root = nil) then
//    Exit;
//
//  for i := 0 to Designer.Root.ComponentCount - 1 do
//  begin
//    if (Designer.Root.Components[i] is TDGLSoundLibrary) then
//    begin
//      comp  := TDGLSoundLibrary(Designer.Root.Components[i]);
//      for j := 0 to comp.Samples.Count - 1 do
//        if Assigned(comp.Samples[j].Data) then
//          Proc(FindUnitName(comp.Samples[j].Data));
//    end;
//  end;
//end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

//======================================= OTHERS =========================================

// ------------------
{ EMPTY }
{$IFDEF GLS_REGION}{$REGION 'EMPTY'}{$ENDIF}

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}


// **************************************************************************************

procedure GLRegisterPropertiesInCategories;
begin

  { GLViewer }
  // property types
  {$IFDEF WIN32}
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TDGLCamera), TypeInfo(TDGLSceneBuffer), TypeInfo(TVSyncMode)]);
  //TypeInfo(TDGLScreenDepth)]); // TDGLScreenDepth in GLWin32FullScreenViewer
  {$ENDIF}
  // TDGLSceneViewer
  RegisterPropertiesInCategory(sOpenGLCategoryName, TDGLSceneViewer, ['*Render']);

  { GLScene }
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TDGLObjectsSorting), TypeInfo(TDGLProgressEvent), TypeInfo(TDGLBehaviours), TypeInfo(TDGLObjectEffects), TypeInfo(TDirectRenderEvent), TypeInfo(TDGLCameraStyle), TypeInfo(TOnCustomPerspective),
    TypeInfo(TDGLScene)]);
  RegisterPropertiesInCategory(sLayoutCategoryName, [TypeInfo(TDGLObjectsSorting), TypeInfo(TNormalDirection)]);
  RegisterPropertiesInCategory(sVisualCategoryName, [TypeInfo(TDGLVisibilityCulling), TypeInfo(TLightStyle), TypeInfo(TDGLColor), TypeInfo(TNormalDirection), TypeInfo(TDGLCameraStyle)]);
  // TDGLBaseSceneObject
  RegisterPropertiesInCategory(sVisualCategoryName, TDGLBaseSceneObject, ['Rotation', 'Direction', 'Position', 'Up', 'Scale', '*Angle', 'ShowAxes', 'FocalLength']);
  // TDGLSceneObject
  RegisterPropertiesInCategory(sVisualCategoryName, TDGLSceneObject, ['Parts']);
  // TDGLDirectOpenGL
  RegisterPropertiesInCategory(sOpenGLCategoryName, TDGLDirectOpenGL, ['UseBuildList']);
  // TDGLProxyObject
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TDGLProxyObjectOptions)]);
  // TDGLLightSource
  RegisterPropertiesInCategory(sVisualCategoryName, TDGLLightSource, ['*Attenuation', 'Shining', 'Spot*']);
  // TDGLCamera
  RegisterPropertiesInCategory(sOpenGLCategoryName, TDGLCamera, ['TargetObject']);
  RegisterPropertiesInCategory(sVisualCategoryName, TDGLCamera, ['DepthOfView', 'SceneScale']);

  { GLObjects }
  // TDGLDummyCube
  RegisterPropertiesInCategory(sLayoutCategoryName, TDGLDummyCube, ['VisibleAtRunTime']);
  RegisterPropertiesInCategory(sVisualCategoryName, TDGLDummyCube, ['CubeSize', 'VisibleAtRunTime']);
  // TDGLPlane
  RegisterPropertiesInCategory(sVisualCategoryName, TDGLPlane, ['*Offset', '*Tiles']);
  // TDGLSprite
  RegisterPropertiesInCategory(sOpenGLCategoryName, TDGLSprite, ['NoZWrite']);
  RegisterPropertiesInCategory(sLayoutCategoryName, TDGLSprite, ['NoZWrite']);
  RegisterPropertiesInCategory(sVisualCategoryName, TDGLSprite, ['AlphaChannel', 'Rotation']);
  // TDGLCube
  RegisterPropertiesInCategory(sVisualCategoryName, TDGLCube, ['Cube*']);
  // TDGLSphere
  RegisterPropertiesInCategory(sVisualCategoryName, TDGLSphere, ['Bottom', 'Radius', 'Slices', 'Stacks', 'Start', 'Stop']);
  // TDGLDisk
  RegisterPropertiesInCategory(sVisualCategoryName, TDGLDisk, ['*Radius', 'Loops', 'Slices']);


  { GLHUDObjects }
//  RegisterPropertiesInCategory(sLayoutCategoryName, [TypeInfo(TTextLayout)]);

  { GLTexture }
//  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TDGLMaterial), TypeInfo(TDGLMaterialLibrary), TypeInfo(TDGLLibMaterials), TypeInfo(TTextureNeededEvent)]);
  // TDGLLibMaterial
//  RegisterPropertiesInCategory(sOpenGLCategoryName, TDGLLibMaterial, ['Texture2Name']);
//  RegisterPropertiesInCategory(sVisualCategoryName, TDGLLibMaterial, ['TextureOffset', 'TextureScale']);
  // TDGLMaterialLibrary
//  RegisterPropertiesInCategory(sOpenGLCategoryName, TDGLMaterialLibrary, ['TexturePaths']);

  { GLCadencer }
  RegisterPropertiesInCategory(sOpenGLCategoryName, [TypeInfo(TDGLCadencer)]);


end;

procedure Register;
begin
  //-------------- COMPONENTS -----------------------------------------
  RegisterComponents('DGLEngine', [TDGLScene, TDGLSceneViewer, TDGLMaterialLibrary, TDGLShaderLibrary, TDGLCadencer]);
  RegisterComponents('DGLEngine Tools', [TDGLApplicationFileIO, TDGLSLogger,  TDGLSSynHiMemo]);

  //========================================= DESIGNTIME EDITORS ==============================================================

  //-------------- MATERIAL ------------------------------------------------
//  RegisterComponentEditor(TDGLMaterialLibrary, TDGLMaterialLibraryEditor);
//
//  RegisterPropertyEditor(TypeInfo(TDGLLibMaterialName), TDGLMaterial, '', TDGLLibMaterialNameProperty);
//  RegisterPropertyEditor(TypeInfo(TDGLMaterial), nil, '', TDGLMaterialProperty);
//  RegisterPropertyEditor(TypeInfo(TDGLTexture), TDGLMaterial, '', TDGLTextureProperty);
//  RegisterPropertyEditor(TypeInfo(TDGLMaterialComponentName), TDGLTextureProperties, 'LibTextureName', TDGLLibTextureNameProperty);
//  RegisterPropertyEditor(TypeInfo(TDGLMaterialComponentName), TDGLTextureProperties, 'LibSamplerName', TDGLLibSamplerNameProperty);
//  RegisterPropertyEditor(TypeInfo(TDGLMaterialComponentName), TDGLMultitexturingProperties, 'LibCombinerName', TDGLLibCombinerNameProperty);
//  RegisterPropertyEditor(TypeInfo(string), TDGLTexture, 'SourceFile', TPictureFileProperty);

  //-------------- SHADER ------------------------------------------------
//  RegisterComponentEditor(TDGLShaderLibrary, TDGLShaderLibraryEditor);
//
//  RegisterPropertyEditor(TypeInfo(TDGLMaterialComponentName), TDGLShaderModel, 'LibVertexShaderName', TDGLLibShaderNameProperty);
//  RegisterPropertyEditor(TypeInfo(TDGLMaterialComponentName), TDGLShaderModel, 'LibFragmentShaderName', TDGLLibShaderNameProperty);
//  RegisterPropertyEditor(TypeInfo(TDGLMaterialComponentName), TDGLShaderModel, 'LibGeometryShaderName', TDGLLibShaderNameProperty);
//  RegisterPropertyEditor(TypeInfo(TDGLMaterialComponentName), TDGLShaderModel, 'LibTessControlShaderName', TDGLLibShaderNameProperty);
//  RegisterPropertyEditor(TypeInfo(TDGLMaterialComponentName), TDGLShaderModel, 'LibTessEvalShaderName', TDGLLibShaderNameProperty);
//  RegisterPropertyEditor(TypeInfo(string), TDGLLibShader, 'SourceFile', TShaderFileProperty);
//  RegisterPropertyEditor(TypeInfo(TDGLLibShaderName), TDGLLibShaderProperty, 'NextPass', TDGLLibShaderNameProperty);
//  RegisterPropertyEditor(TypeInfo(Boolean), TDGLBaseShaderModel, 'AutoFillOfUniforms', TUniformAutoSetProperty);
//  RegisterPropertyEditor(TypeInfo(TStringList), TDGLLibShader, 'Source', TDGLShaderEditorProperty);


  //-------------- SCENE -----------------------------------------------
  RegisterComponentEditor(TDGLSceneViewer, TDGLSceneViewerEditor);
  RegisterComponentEditor(TDGLScene, TDGLSceneEditor);

  RegisterSelectionEditor(TDGLBaseSceneObject, TDGLBaseSceneObjectSelectionEditor);
  GLRegisterPropertiesInCategories;

  //-------------- COMMON ----------------------------------------------
  RegisterPropertyEditor(TypeInfo(TResolution), nil, '', TResolutionProperty);
  RegisterPropertyEditor(TypeInfo(TDGLCoordinates), nil, '', TDGLCoordinatesProperty);
  RegisterPropertyEditor(TypeInfo(TDGLColor), nil, '', TDGLColorProperty);

  //-------------- SOUND -----------------------------------------------

  //-------------- OTHERS ----------------------------------------------
end;


function GetDGLSceneVersion: string;
var
  LProject:                                IOTAProject;
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
    LSVN         := LProjectPath + '.svn\entries';
    if FileExists(LSVN) then
      with TStringList.Create do
        try
          // Load
          LoadFromFile(LSVN);
          if (Count >= 4) and (Trim(Strings[3]) <> '') and IsDirectoryWriteable(LExePath) then
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
  Result  := '';
  Project := GetActiveProject;
  if Assigned(Project) then
  begin
    Result := Project.ProjectOptions.TargetName;
    if Length(Result) > 0 then
      ForceDirectories(ExtractFilePath(Result));
  end;
end;



// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

SplashScreenServices.AddPluginBitmap(GeTDGLSceneVersion, LoadBitmap(HInstance, 'TDGLScene'), False, 'MPL 1.1 license', 'SVN version');
DGLCrossPlatform.IsDesignTime            := True;
DGLCrossPlatform.vProjectTargetName      := GetProjectTargetName;
DGLColor.vUseDefaultColorSets            := True;
DGLCoordinates.vUseDefaultCoordinateSets := True;
ReadVideoModes;

with ObjectManager do
begin
  CreateDefaultObjectIcons(HInstance);
  RegisterSceneObject(TDGLCamera, 'Camera', '', HInstance);
  RegisterSceneObject(TDGLLightSource, 'LightSource', '', HInstance);
  RegisterSceneObject(TDGLDummyCube, 'DummyCube', '', HInstance);

  // Basic geometry
  RegisterSceneObject(TDGLSprite, 'Sprite', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TDGLPlane, 'Plane', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TDGLCube, 'Cube', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TDGLSphere, 'Sphere', glsOCBasicGeometry, HInstance);
  RegisterSceneObject(TDGLDisk, 'Disk', glsOCBasicGeometry, HInstance);

  // Other objects.
  RegisterSceneObject(TDGLDirectOpenGL, 'Direct OpenGL', '', HInstance);
  RegisterSceneObject(TDGLRenderPoint, 'Render Point', '', HInstance);

  // Special Object
//  RegisterSceneObject(TDGLFBORenderer, 'OpenGL FrameBuffer', '', HInstance);
end;

finalization

ObjectManager.Free;

end.
