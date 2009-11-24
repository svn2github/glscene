//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSceneRegisterLCL<p>

   Registration unit for GLScene library components, property editors and
      IDE experts for Lazarus.<p>

   <b>History :</b><font size=-1><ul>
      <li>24/11/09 - DanB - Removed some more windows only units
      <li>22/11/09 - DaStr - Improved Unix compatibility (again)
      <li>17/11/09 - DaStr - Improved Unix compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
      <li>24/03/08 - DaStr - Initial version
   </ul></font>
}
unit GLSceneRegisterLCL;

interface

{$I GLScene.inc}

uses
   Classes, GLObjectManager, ComponentEditors, PropEdits, LResources;

 type
  // TGLLibMaterialNameProperty
  //
  TGLLibMaterialNameProperty = class(TStringProperty)
    public
    { Public Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

//: Auto-create for object manager
function ObjectManager : TObjectManager;

implementation

uses
   SysUtils, Dialogs, Graphics,
   // GLScene units
   VectorGeometry, GLScene, GLViewer,
   GLStrings, GLCoordinates, GLTexture, GLMaterial, GLScreen,
   GLCadencer, GLTextureImageEditors,GLColor, GLCrossPlatform,
   // GLScene - basic geometry
   GLObjects, GLGeomObjects, GLPolyhedron,
   // GLScene - advanced geometry
   GLAnimatedSprite, GLExtrusion, GLMultiPolygon,
   // GLScene - mesh
   GLVectorFileObjects, GLMesh, GLTilePlane, GLPortal,
   // GLScene - terrain
   GLTerrainRenderer, GLHeightData, GLHeightTileFileHDS, GLBumpmapHDS, GLPerlin,
   GLTexturedHDS, GLAsyncHDS, GLShadowHDS,
   // GLScene - graph plotting
   GLBitmapFont, GLGraph, GLWindowsFont,

   // GLScene - particles
   GLParticles, GLParticleFX, GLPerlinPFX, GLLinePFX, GLFireFX, GLThorFX,
   GLEParticleMasksManager,
   // GLScene - environment
   GLSkydome, GLSkyBox, GLAtmosphere,
   // GLScene - hud
   GLHUDObjects, GLGameMenu,
   GLConsole,
   // GLScene - gui
   GLWindows, GLGui,
   // GLScene - special
   GLLensFlare, GLTexLensFlare, GLMirror, GLShadowPlane, GLShadowVolume,
   GLzBuffer, GLSLProjectedTextures, GLProjectedTextures, GLBlur,
   GLTrail, GLPostEffects,
   // GLScene - doodad
   GLTeapot, GLTree, GLWaterPlane,
   // GLScene - proxy
   GLProxyObjects, GLMultiProxy, GLMaterialMultiProxy,
   // GLScene - shaders
   GLTexCombineShader, GLPhongShader, GLUserShader, GLSLShader,
   GLHiddenLineShader, GLCelShader, GLOutlineShader, GLMultiMaterialShader,
   GLBumpShader, GLSLDiffuseSpecularShader, GLSLBumpShader, GLSLPostBlurShader,
   GLAsmShader, GLShaderCombiner, GLTextureSharingShader,
   // GLScene - other
   GLImposter, GLFeedback, GLCollision, GLScriptBase, AsyncTimer, GLDCE,
   GLFPSMovement, GLMaterialScript, GLNavigator, GLSmoothNavigator,
   GLTimeEventsMgr, ApplicationFileIO, GLVfsPAK, GLSimpleNavigation,
   GLCameraController, GLGizmo, GLGizmoEx, GLKeyboard, GLFBORenderer,
   GLSoundFileObjects, GLSound,
   // Vector file formats
   GLFile3DS, GLFileASE, GLFileB3D, GLFileGL2, GLFileGTS, GLFileLMTS,
   GLFileLWO, GLFileMD2, GLFileMD3, GLFileMD5, GLFileMDC, GLFileMS3D, GLFileNMF,
   GLFileNurbs, GLFileObj,  GLFilePLY, GLFileSMD, GLFileSTL,
   GLFileTIN, GLFileVRML, GlFileX,

   // Sound file formats
   GLFileWAV, GLFileMP3,


  // Property editor forms
  GLSceneEditLCL, FVectorEditorLCL, FMaterialEditorFormLCL, FRMaterialPreviewLCL, 
  FLibMaterialPickerLCL, FRTextureEditLCL, FRFaceEditorLCL,
  FRColorEditorLCL, FRTrackBarEditLCL;

var
   vObjectManager : TObjectManager;

function ObjectManager : TObjectManager;
begin
   if not Assigned(vObjectManager) then
      vObjectManager:=TObjectManager.Create(nil);
   Result:=vObjectManager;
end;

type
   // TGLSceneViewerEditor
   //
   TGLSceneViewerEditor = class(TComponentEditor)
      public
         { Public Declarations }
         procedure ExecuteVerb(Index: Integer); override;
         function GetVerb(Index: Integer): String; override;
         function GetVerbCount: Integer; override;
   end;

   // TGLSceneEditor
   //
   TGLSceneEditor = class (TComponentEditor)
      public
         { Public Declarations }
         procedure Edit; override;

         procedure ExecuteVerb(Index: Integer); override;
         function GetVerb(Index: Integer): String; override;
         function GetVerbCount: Integer; override;
   end;

   // TResolutionProperty
   //
   TResolutionProperty = class (TPropertyEditor)
      public
         { Public Declarations }
         function GetAttributes: TPropertyAttributes; override;
         function GetValue : String; override;
         procedure GetValues(Proc: TGetStrProc); override;
         procedure SetValue(const Value: String); override;
   end;

      // TClassProperty
   //
   TGLTextureProperty = class (TClassProperty)
      protected
     { Protected Declarations }
       function GetAttributes: TPropertyAttributes; override;
  end;

   // TGLTextureImageProperty
  //
  TGLTextureImageProperty = class(TClassProperty)
      protected
      { Protected Declarations }
      function GetAttributes: TPropertyAttributes; override;
      procedure Edit; override;
  end;

   // TGLImageClassProperty
   //
   TGLImageClassProperty = class(TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes : TPropertyAttributes; override;
         procedure GetValues(proc : TGetStrProc); override;

      public
        { Public Declarations }
        function GetValue : String; override;
        procedure SetValue(const value : String); override;
   end;

   TGLColorProperty = class (TClassProperty)
      private
        { Private Declarations }
      protected
        { Protected Declarations }
	function GetAttributes: TPropertyAttributes; override;
	procedure GetValues(Proc: TGetStrProc); override;
	procedure Edit; override;

        function ColorToBorderColor(aColor: TColorVector; selected : Boolean) : TColor;

      public
(*	      {$ifdef GLS_COMPILER_5}
	      procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
	      procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
	      {$endif}
	      {$ifdef GLS_COMPILER_6_UP}
         // Well, i don't know why it's doesn't work with Kylix ...
         {$ifdef WIN32}
         // ICustomPropertyListDrawing  stuff
         procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
         procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
         procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TGLRect; ASelected: Boolean);
         // CustomPropertyDrawing
         procedure PropDrawName(ACanvas: TCanvas; const ARect: TGLRect; ASelected: Boolean);
         procedure PropDrawValue(ACanvas: TCanvas; const ARect: TGLRect; ASelected: Boolean);
         {$endif}
         {$endif}
*)
        function GetValue: String; override;
	procedure SetValue(const Value: string); override;
   end;

   // TVectorFileProperty
   //
   TVectorFileProperty = class (TClassProperty)
      protected
         { Public Declarations }
         function GetAttributes: TPropertyAttributes; override;
         function GetValue: String; override;
         procedure Edit; override;
         procedure SetValue(const Value: string); override;
   end;

   // TSoundFileProperty
   //
   TSoundFileProperty = class (TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes : TPropertyAttributes; override;
         function GetValue: String; override;
         procedure Edit; override;
   end;

   // TSoundNameProperty
   //
   TSoundNameProperty = class (TStringProperty)
      protected
         { Protected Declarations }
         function GetAttributes : TPropertyAttributes; override;
      	procedure GetValues(Proc: TGetStrProc); override;
   end;


   // TGLCoordinatesProperty
   //
   TGLCoordinatesProperty = class(TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
         procedure Edit; override;
   end;

   // TGLMaterialProperty
   //
   TGLMaterialProperty = class(TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
         procedure Edit; override;
   end;

   // TReuseableDefaultEditor
   //
   {: Editor copied from DsgnIntf.<p>
      Could have been avoided, if only that guy at Borland didn't chose to
      publish only half of the stuff (and that's not the only class with
      that problem, most of the subitems handling code in TGLSceneBaseObject is
      here for the same reason...), the "protected" wasn't meant just to lure
      programmers into code they can't reuse... Arrr! and he did that again
      in D6! Grrr... }

   // TGLMaterialLibraryEditor
   //
   {: Editor for material library.<p> }

   TGLMaterialLibraryEditor = class(TDefaultComponentEditor)
      protected
      //
      { procedure Edit;
       procedure EditProperty(const Prop: TPropertyEditor;      var Continue: Boolean); override;
         procedure ExecuteVerb(Index: Integer); override;
         function GetVerb(Index: Integer): String; override; }
   end;

   // TGLAnimationNameProperty
   //
   TGLAnimationNameProperty = class(TStringProperty)
      protected
         { Protected Declarations }
         function GetAttributes : TPropertyAttributes; override;
         procedure GetValues(proc : TGetStrProc); override;

      public
         { Public Declarations }
   end;

//----------------- TGLSceneViewerEditor ---------------------------------------

// ExecuteVerb
//
procedure TGLSceneViewerEditor.ExecuteVerb(Index : Integer);
var
  source : TGLSceneViewer;
begin
  source:=Component as TGLSceneViewer;
  case Index of
    0 : source.Buffer.ShowInfo;
  end;
end;

// GetVerb
//
function TGLSceneViewerEditor.GetVerb(Index : Integer) : String;
begin
  case Index of
    0 : Result:='Show context info';
  end;
end;

// GetVerbCount
//
function TGLSceneViewerEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;


//----------------- TGLSceneEditor ---------------------------------------------

// Edit
//
procedure TGLSceneEditor.Edit;
begin
   with GLSceneEditorForm do begin
      SetScene(Self.Component as TGLScene, TComponentEditorDesigner(Self.Designer));
      Show;
   end;
end;

// ExecuteVerb
//
procedure TGLSceneEditor.ExecuteVerb(Index : Integer);
begin
   case Index of
      0 : Edit;
   end;
end;

// GetVerb
//
function TGLSceneEditor.GetVerb(Index : Integer) : String;
begin
   case Index of
      0 : Result:='Show Scene Editor';
   end;
end;

// GetVerbCount
//
function TGLSceneEditor.GetVerbCount: Integer;
begin
   Result:=1;
end;

//----------------- TResolutionProperty ----------------------------------------

// GetAttributes
//
function TResolutionProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paValueList];
end;

// GetValue
//
function TResolutionProperty.GetValue : String;
begin
   Result:=vVideoModes[GetOrdValue].Description;
end;

// GetValues
//
procedure TResolutionProperty.GetValues(Proc: TGetStrProc);
var
   i : Integer;
begin
   for i:=0 to vNumberVideoModes-1 do
      Proc(vVideoModes[i].Description);
end;

// SetValue
//
procedure TResolutionProperty.SetValue(const Value: String);

const Nums = ['0'..'9'];

var XRes,YRes,BPP : Integer;
    Pos, SLength  : Integer;
    TempStr       : String;

begin
  if CompareText(Value,'default') <> 0 then
  begin
    // initialize scanning
    TempStr:=Trim(Value)+'|'; // ensure at least one delimiter
    SLength:=Length(TempStr);
    XRes:=0; YRes:=0; BPP:=0;
    // contains the string something?
    if SLength > 1 then
    begin
      // determine first number
      for Pos:=1 to SLength do
        if not (TempStr[Pos] in Nums) then Break;
      if Pos <= SLength then
      begin
        // found a number?
        XRes:=StrToInt(Copy(TempStr,1,Pos-1));
        // search for following non-numerics
        for Pos:=Pos to SLength do
          if TempStr[Pos] in Nums then Break;
        Delete(TempStr,1,Pos-1); // take it out of the String
        SLength:=Length(TempStr); // rest length of String
        if SLength > 1 then // something to scan?
        begin
          // determine second number
          for Pos:=1 to SLength do
            if not (TempStr[Pos] in Nums) then Break;
          if Pos <= SLength then
          begin
            YRes:=StrToInt(Copy(TempStr,1,Pos-1));
            // search for following non-numerics
            for Pos:=Pos to SLength do
              if TempStr[Pos] in Nums then Break;
            Delete(TempStr,1,Pos-1); // take it out of the String
            SLength:=Length(TempStr); // rest length of String
            if SLength > 1 then
            begin
              for Pos:=1 to SLength do
                if not (TempStr[Pos] in Nums) then Break;
              if Pos <= SLength then BPP:=StrToInt(Copy(TempStr,1,Pos-1));
            end;
          end;
        end;
      end;
    end;
    SetOrdValue(GetIndexFromResolution(XRes,YRes,BPP));
  end
  else SetOrdValue(0);
end;


//----------------- TGLTextureProperty -----------------------------------------

function TGLTextureProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paSubProperties];
end;

//----------------- TGLTextureImageProperty ------------------------------------

// GetAttributes
//
function TGLTextureImageProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paDialog];
end;

// Edit
//
procedure TGLTextureImageProperty.Edit;
begin
  if EditGLTextureImage(TGLTextureImage(GetOrdValue)) then
       Modified;
end;

//----------------- TGLImageClassProperty --------------------------------------

// GetAttributes
//
function TGLImageClassProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paValueList];
end;

// GetValues
//
procedure TGLImageClassProperty.GetValues(proc: TGetStrProc);
var
	i : Integer;
	sl : TStrings;
begin
	sl:=GetGLTextureImageClassesAsStrings;
	try
		for i:=0 to sl.Count-1 do proc(sl[i]);
	finally
		sl.Free;
	end;
end;

// GetValue
//
function TGLImageClassProperty.GetValue : String;
begin
	Result:=FindGLTextureImageClass(GetStrValue).FriendlyName;
end;

// SetValue
//
procedure TGLImageClassProperty.SetValue(const value : String);
var
	tic : TGLTextureImageClass;
begin
	tic:=FindGLTextureImageClassByFriendlyName(value);
	if Assigned(tic) then
		SetStrValue(tic.ClassName)
	else SetStrValue('');
	Modified;
end;

//----------------- TGLColorproperty -----------------------------------------------------------------------------------

procedure TGLColorProperty.Edit;
var
   colorDialog : TColorDialog;
   glColor : TGLColor;
begin
   colorDialog:=TColorDialog.Create(nil);
   try
      glColor:=TGLColor(GetObjectValue);
      {$IFNDEF FPC}{$ifdef WIN32}
      colorDialog.Options:=[cdFullOpen];
      {$endif}{$ENDIF}
      colorDialog.Color:=ConvertColorVector(glColor.Color);
      if colorDialog.Execute then begin
         glColor.Color:=ConvertWinColor(colorDialog.Color);
         Modified;
      end;
   finally
      colorDialog.Free;
   end;
end;

function TGLColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paSubProperties, paValueList, paDialog];
end;

procedure TGLColorProperty.GetValues(Proc: TGetStrProc);
begin
  ColorManager.EnumColors(Proc);
end;

function TGLColorProperty.GetValue: String;
begin
  Result:=ColorManager.GetColorName(TGLColor(GetOrdValue).Color);
end;

procedure TGLColorProperty.SetValue(const Value: string);
begin
  TGLColor(GetOrdValue).Color:=ColorManager.GetColor(Value);
  Modified;
end;

// ColorToBorderColor
//
function TGLColorProperty.ColorToBorderColor(aColor: TColorVector; selected : Boolean) : TColor;
begin
   if (aColor[0]>0.75) or (aColor[1]>0.75) or (aColor[2]>0.75) then
      Result:=clBlack
   else if selected then
      Result:=clWhite
   else Result:=ConvertColorVector(AColor);
end;
(*
{$ifdef GLS_COMPILER_5}
procedure TGLColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
                                         const ARect: TRect; ASelected: Boolean);
var
   vRight: Integer;
   vOldPenColor,
   vOldBrushColor: TColor;
   Color: TColorVector;
begin
   vRight:=(ARect.Bottom - ARect.Top) + ARect.Left;
   with ACanvas do try
      vOldPenColor:=Pen.Color;
      vOldBrushColor:=Brush.Color;

      Pen.Color:=Brush.Color;
      Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

      Color:=ColorManager.GetColor(Value);
      Brush.Color:=ConvertColorVector(Color);
      Pen.Color:=ColorToBorderColor(Color, ASelected);

      Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

      Brush.Color:=vOldBrushColor;
      Pen.Color:=vOldPenColor;
   finally
      inherited ListDrawValue(Value, ACanvas, Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom), ASelected);
   end;
end;

procedure TGLColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
   // draws the small color rectangle in the object inspector
   if GetVisualValue<>'' then
      ListDrawValue(GetVisualValue, ACanvas, ARect, True)
   else inherited PropDrawValue(ACanvas, ARect, ASelected);
end;
{$endif}

{$ifdef GLS_COMPILER_6_UP}
procedure TGLColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
   if GetVisualValue <> '' then
      ListDrawValue(GetVisualValue, ACanvas, ARect, True)
   else DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TGLColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
                                         const ARect: TRect; ASelected: Boolean);
var
   vRight: Integer;
   vOldPenColor,
   vOldBrushColor: TColor;
   Color: TColorVector;
begin
   vRight:=(ARect.Bottom - ARect.Top) + ARect.Left;
   with ACanvas do try
      vOldPenColor:=Pen.Color;
      vOldBrushColor:=Brush.Color;

      Pen.Color:=Brush.Color;
      Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

      Color:=ColorManager.GetColor(Value);
      Brush.Color:=ConvertColorVector(Color);
      Pen.Color:=ColorToBorderColor(Color, ASelected);

      Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

      Brush.Color:=vOldBrushColor;
      Pen.Color:=vOldPenColor;
   finally
      DefaultPropertyListDrawValue(Value, ACanvas, Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom),
                                   ASelected);
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
{$endif GLS_COMPILER_6_UP} *)

//----------------- TVectorFileProperty ----------------------------------------

// GetAttributes
//
function TVectorFileProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog];
end;

// GetValue
//
function TVectorFileProperty.GetValue: String;
begin
   Result:=GetStrValue;
end;

// Edit
//
procedure TVectorFileProperty.Edit;
var
   ODialog   : TOpenDialog;
   Component : TGLFreeForm;
   Desc, F    : String;
begin
   Component:=GetComponent(0) as TGLFreeForm;
   ODialog:=TOpenDialog.Create(nil);
   try
      GetVectorFileFormats.BuildFilterStrings(TVectorFile, Desc, F);
      ODialog.Filter:=Desc;
      if ODialog.Execute then begin
         Component.LoadFromFile(ODialog.FileName);
         Modified;
      end;
   finally
      ODialog.Free;
   end;
end;

// SetValue
//
procedure TVectorFileProperty.SetValue(const Value: string);
begin
   SetStrValue(Value);
end;

//----------------- TSoundFileProperty -----------------------------------------

// GetAttributes
//
function TSoundFileProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog];
end;

// GetValue
//
function TSoundFileProperty.GetValue: String;
var
   sample : TGLSoundSample;
begin
   sample:=GetComponent(0) as TGLSoundSample;
   if sample.Data<>nil then
      Result:='('+sample.Data.ClassName+')'
   else Result:='(empty)';
end;

// Edit
//
procedure TSoundFileProperty.Edit;
var
   ODialog   : TOpenDialog;
   sample : TGLSoundSample;
   Desc, F    : String;
begin
   sample:=GetComponent(0) as TGLSoundSample;
   ODialog:=TOpenDialog.Create(nil);
   try
      GetGLSoundFileFormats.BuildFilterStrings(TGLSoundFile, Desc, F);
      ODialog.Filter:=Desc;
      if ODialog.Execute then begin
         sample.LoadFromFile(ODialog.FileName);
         Modified;
      end;
   finally
      ODialog.Free;
   end;
end;

//----------------- TSoundNameProperty -----------------------------------------

// GetAttributes
//
function TSoundNameProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paValueList];
end;

// GetValues
//
procedure TSoundNameProperty.GetValues(Proc: TGetStrProc);
var
   i : Integer;
   source : TGLBaseSoundSource;
begin
   source:=(GetComponent(0) as TGLBaseSoundSource);
   if Assigned(source.SoundLibrary) then with source.SoundLibrary do
      for i:=0 to Samples.Count-1 do Proc(Samples[i].Name);
end;

//----------------- TGLCoordinatesProperty -------------------------------------

// GetAttributes
//
function TGLCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog, paSubProperties];
end;

// Edit;
//
procedure TGLCoordinatesProperty.Edit;
var
   glc : TGLCoordinates;
   x, y, z : Single;
begin
   glc:=TGLCoordinates(GetOrdValue);
   x:=glc.x;
   y:=glc.y;
   z:=glc.z;
   if VectorEditorForm.Execute(x, y, z) then begin
      glc.AsVector:=VectorMake(x, y, z);
      Modified;
   end;
end;

//----------------- TGLMaterialProperty --------------------------------------------------------------------------------

// GetAttributes
//
function TGLMaterialProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog, paSubProperties];
end;

// Edit
//
procedure TGLMaterialProperty.Edit;
var ml:TGLMaterial;
begin
   ml:= TGLMaterial(GetOrdValue);
   if MaterialEditorForm.Execute(ml) then
     Modified;
end;

//----------------- TGLMaterialLibraryEditor --------------------------------------------------------------------------------

// EditProperty
//
{procedure TGLMaterialLibraryEditor.EditProperty(const Prop: TPropertyEditor;
                                                      var Continue: Boolean);
begin
   if CompareText(Prop.GetName, 'MATERIALS') = 0 then begin
   self.BestEditEvent:=Prop.GetName
    //  FBest.Free;
     // FBest:=Prop;
     // FreeEditor:=False;
   end;
end;
// Edit
//
procedure TGLMaterialLibraryEditor.Edit;
begin
  inherited;
end;

// ExecuteVerb
//
procedure TGLMaterialLibraryEditor.ExecuteVerb(Index : Integer);
begin
   case Index of
      0 : Edit;
   end;
end;

// GetVerb
//
function TGLMaterialLibraryEditor.GetVerb(Index : Integer) : String;
begin
   case Index of
      0 : Result:='Show Material Library Editor';
   end;
end;   }

//----------------- TGLLibMaterialNameProperty ---------------------------------

// GetAttributes
//
function TGLLibMaterialNameProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog];
end;

// Edit
//
procedure TGLLibMaterialNameProperty.Edit;
var
  buf: string;
  ml: TGLMaterialLibrary;
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
//
function TGLAnimationNameProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paValueList];
end;

// GetValues
//
procedure TGLAnimationNameProperty.GetValues(proc: TGetStrProc);
var
	i : Integer;
   animControler : TGLAnimationControler;
   actor : TGLActor;
begin
   animControler:=(GetComponent(0) as TGLAnimationControler);
   if Assigned(animControler) then begin
      actor:=animControler.Actor;
      if Assigned(actor) then with actor.Animations do begin
         for i:=0 to Count-1 do
            proc(Items[i].Name);
      end;
	end;
end;




procedure Register;
begin
   RegisterComponents('GLScene',
                      [TGLScene,
                       TGLSceneViewer, TGLMemoryViewer,
                       TGLMaterialLibrary,
                       TGLCadencer,
                       TGLGuiLayout,
                       TGLBitmapFont, TGLWindowsBitmapFont, TGLStoredBitmapFont,
                       TGLScriptLibrary, TGLSoundLibrary

                      ]);

   RegisterComponents('GLScene PFX',
                      [
                       TGLCustomPFXManager,
                       TGLPolygonPFXManager, TGLPointLightPFXManager,
                       TGLCustomSpritePFXManager,
                       TGLPerlinPFXManager, TGLLinePFXManager,
                       TGLFireFXManager, TGLThorFXManager,
                       TGLEParticleMasksManager
                      ]);

   RegisterComponents('GLScene Utils',
                      [TAsyncTimer, TGLStaticImposterBuilder,
                       TCollisionManager, TGLAnimationControler,
                       TGLDCEManager, TGLFPSMovementManager,
                       TGLMaterialScripter, TGLUserInterface, TGLNavigator,
                       TGLSmoothNavigator, TGLSmoothUserInterface,
                       TGLTimeEventsMGR, TApplicationFileIO, TGLVfsPAK,
                       TGLSimpleNavigation, TGLCameraController,
                       TGLGizmo,TGLGizmoEx
                      ]);

   RegisterComponents('GLScene Terrain',
                      [TGLBitmapHDS, TGLCustomHDS, TGLHeightTileFileHDS,
                       TGLBumpmapHDS, TGLPerlinHDS, TGLTexturedHDS, TGLAsyncHDS,
                       TGLShadowHDS
                      ]);

   RegisterComponents('GLScene Shaders',
                      [ TGLTexCombineShader, TGLPhongShader, TGLUserShader,
                        TGLHiddenLineShader, TGLCelShader, TGLOutlineShader,
                        TGLMultiMaterialShader, TGLBumpShader,
                        TGLSLShader, TGLSLDiffuseSpecularShader, TGLSLBumpShader,
                        TGLAsmShader,TGLShaderCombiner,TGLTextureSharingShader,
                        TGLSLPostBlurShader
                      ]);

   RegisterComponentEditor(TGLSceneViewer, TGLSceneViewerEditor);
   RegisterComponentEditor(TGLScene, TGLSceneEditor);

   RegisterClasses([TGLCoordinates]);

   RegisterComponentEditor(TGLMaterialLibrary, TGLMaterialLibraryEditor);

   RegisterPropertyEditor(TypeInfo(TResolution), nil, '', TResolutionProperty);
   RegisterPropertyEditor(TypeInfo(TGLTexture), TGLMaterial, '', TGLTextureProperty);
   RegisterPropertyEditor(TypeInfo(TGLTextureImage), TGLTexture, '', TGLTextureImageProperty);
   RegisterPropertyEditor(TypeInfo(String), TGLTexture, 'ImageClassName', TGLImageClassProperty);

   RegisterPropertyEditor(TypeInfo(TGLSoundFile), TGLSoundSample, '', TSoundFileProperty);
   RegisterPropertyEditor(TypeInfo(String), TGLBaseSoundSource, 'SoundName', TSoundNameProperty);

   RegisterPropertyEditor(TypeInfo(TGLCoordinates), nil, '', TGLCoordinatesProperty);

   RegisterPropertyEditor(TypeInfo(TGLColor), nil, '', TGLColorProperty);
   RegisterPropertyEditor(TypeInfo(TGLMaterial), nil, '', TGLMaterialProperty);

   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterial, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLLibMaterial, 'Texture2Name', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSkyBox, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLEParticleMask, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLGameMenu, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterialMultiProxyMaster, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSLBumpShader, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TSpriteAnimation, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterialProxy, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLActorProxy, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLFBORenderer, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TActorAnimationName), TGLAnimationControler, '', TGLAnimationNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLTextureSharingShaderMaterial, 'LibMaterialName', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TFileName), TGLFreeForm, 'FileName', TVectorFileProperty);

   with ObjectManager do begin
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
      RegisterSceneObject(TGLDodecahedron, 'Dodecahedron', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLIcosahedron, 'Icosahedron', glsOCBasicGeometry, HInstance);

      //Advanced geometry
      RegisterSceneObject(TGLAnimatedSprite, 'Animated Sprite', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLArrowLine, 'ArrowLine', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLAnnulus, 'Annulus', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLExtrusionSolid, 'ExtrusionSolid', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLMultiPolygon, 'MultiPolygon', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLPipe, 'Pipe', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLRevolutionSolid, 'RevolutionSolid', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLTorus, 'Torus', glsOCAdvancedGeometry, HInstance);

      //Mesh objects
      RegisterSceneObject(TGLActor, 'Actor', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLFreeForm, 'FreeForm', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLMesh, 'Mesh', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLTilePlane, 'TilePlane', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLPortal, 'Portal', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLTerrainRenderer, 'TerrainRenderer', glsOCMeshObjects, HInstance);

      //Graph-plotting objects
      RegisterSceneObject(TGLFlatText, 'FlatText', glsOCGraphPlottingObjects, HInstance);
      RegisterSceneObject(TGLHeightField, 'HeightField', glsOCGraphPlottingObjects, HInstance);
      RegisterSceneObject(TGLXYZGrid, 'XYZGrid', glsOCGraphPlottingObjects, HInstance);

      //Particle systems
      RegisterSceneObject(TGLParticles, 'Particles', glsOCParticleSystems, HInstance);
      RegisterSceneObject(TGLParticleFXRenderer, 'PFX Renderer', glsOCParticleSystems, HInstance);

      //Environment objects
      RegisterSceneObject(TGLEarthSkyDome, 'EarthSkyDome', glsOCEnvironmentObjects, HInstance);
      RegisterSceneObject(TGLSkyDome, 'SkyDome', glsOCEnvironmentObjects, HInstance);
      RegisterSceneObject(TGLSkyBox, 'SkyBox', glsOCEnvironmentObjects, HInstance);
      RegisterSceneObject(TGLAtmosphere, 'Atmosphere', glsOCEnvironmentObjects, HInstance);

      // HUD objects.
      RegisterSceneObject(TGLHUDSprite, 'HUD Sprite', glsOCHUDObjects, HInstance);
      RegisterSceneObject(TGLHUDText, 'HUD Text', glsOCHUDObjects, HInstance);
      RegisterSceneObject(TGLResolutionIndependantHUDText, 'Resolution Independant HUD Text', glsOCHUDObjects, HInstance);
      RegisterSceneObject(TGLAbsoluteHUDText, 'Absolute HUD Text', glsOCHUDObjects, HInstance);
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
      RegisterSceneObject(TGLAdvancedLabel, 'GLAdvancedLabel', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLScrollbar, 'GLScrollbar', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLStringGrid, 'GLStringGrid', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLCustomControl, 'GLBitmapControl', glsOCGuiObjects, HInstance);

      //Special objects
      RegisterSceneObject(TGLLensFlare, 'LensFlare', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLTextureLensFlare, 'TextureLensFlare', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLMirror, 'Mirror', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLShadowPlane, 'ShadowPlane', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLShadowVolume, 'ShadowVolume', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLZShadows, 'ZShadows', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLSLTextureEmitter, 'GLSL Texture Emitter', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLSLProjectedTextures, 'GLSL Projected Textures', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLTextureEmitter, 'Texture Emitter', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLProjectedTextures, 'Projected Textures', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLBlur, 'Blur', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLMotionBlur, 'MotionBlur', glsOCSpecialObjects, HInstance);

      RegisterSceneObject(TGLTrail, 'GLTrail', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLPostEffect, 'PostEffect', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLPostShaderHolder, 'PostShaderHolder', glsOCSpecialObjects, HInstance);

      // Doodad objects.
      RegisterSceneObject(TGLTeapot, 'Teapot', glsOCDoodad, HInstance);
      RegisterSceneObject(TGLTree, 'Tree', glsOCDoodad, HInstance);
      RegisterSceneObject(TGLWaterPlane, 'WaterPlane', glsOCDoodad, HInstance);

      // Proxy objects.
      RegisterSceneObject(TGLProxyObject, 'ProxyObject', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLColorProxy, 'ColorProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLFreeFormProxy, 'FreeFormProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLMaterialProxy, 'MaterialProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLActorProxy, 'ActorProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLMultiProxy, 'MultiProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLMaterialMultiProxy, 'MaterialMultiProxy', glsOCProxyObjects, HInstance);

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

   GLColor.vUseDefaultColorSets:=True;
   GLCoordinates.vUseDefaultCoordinateSets:=True;

   //ReadVideoModes;

finalization

   ObjectManager.Free;

end.
