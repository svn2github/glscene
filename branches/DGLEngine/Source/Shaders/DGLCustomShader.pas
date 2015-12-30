// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : DGLCustomShader<p>

  A collection of pure abstract classes - descendants of TGLShader, which are
  used for purpose of not having to write the same stuff all over and over
  again in your own shader classes.
  It also contains a procedures and function that can be used in all shaders.<p>

  <b>History : </b><font size=-1><ul>

  <li>24/12/15 - JD - Imported and updated from GLScene
  <ul><font>

}
unit DGLCustomShader;

interface

{$I DGLEngine.inc}

uses
  System.Classes, System.SysUtils,

  // GLS
  DGLResStrings, DGLCrossPlatform, dglOpenGL, DGLTypes, DGLBaseClasses,
  DGLContext, DGLContextHandles, DGLRenderContextInfo, DGLState,
  DGLVectorMaths, DGLVectorTypes, DGLVectorLists,
  DGLTextureFormat,
  DGLSLParameters;

const
  glsShaderMaxLightSources = 50;

type
  TGLShaderFogSupport      = (sfsEnabled, sfsDisabled, sfsAuto);
  TGLTransformFeedBackMode = (tfbmInterleaved, tfbmSeparate);

  EGLShaderException       = class(Exception);
  EGLCustomShaderException = class(EGLShaderException);

  TDGLCustomShader    = class;
  TDGLVertexProgram   = class;
  TDGLFragmentProgram = class;
  TDGLGeometryProgram = class;

  TGLShaderEvent       = procedure(Shader: TDGLCustomShader) of object;
  TGLShaderUnAplyEvent = procedure(Shader: TDGLCustomShader; var ThereAreMorePasses: Boolean) of object;

//  TGLLightSourceEnum = 0 .. glsShaderMaxLightSources-1;
//  TGLLightSourceSet  = set of TGLLightSourceEnum;

  { : This interface describes user shaders, in order to be able to access them
    via a unified interface. If user shader does not support some option, don't
    raise an axception, just ignore it.
  }
  // A PAsser autre part
//  IGLShaderDescription = interface
//    ['{04089C64-60C2-43F5-AC9C-38ED46264812}']
//    procedure SetShaderTextures(const Textures: array of TDGLTexture);
//    procedure GetShaderTextures(var Textures: array of TDGLTexture);
//
//    procedure SetShaderColorParams(const AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);
//    procedure GetShaderColorParams(var AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);
//
//    procedure SetShaderMiscParameters(const ACadencer: TDGLCadencer; const AMatLib: TDGLMaterialLibrary; const ALightSources: TGLLightSourceSet);
//    procedure GetShaderMiscParameters(var ACadencer: TDGLCadencer; var AMatLib: TDGLMaterialLibrary; var ALightSources: TGLLightSourceSet);
//
//    function GetShaderAlpha: Single;
//    procedure SetShaderAlpha(const Value: Single);
//
//    function GetShaderDescription: string;
//  end;

//  { : Used in the TGLPostShaderHolder component. }
//  IGLPostShader = interface
//    ['{68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}']
//    { : Called on every pass. }
//    procedure DoUseTempTexture(const TempTexture: TDGLTextureHandle; TextureTarget: TDGLTextureTarget);
//    { : Called to determine if it is compatible. }
//    function GetTextureTarget: TDGLTextureTarget;
//  end;

  // TDGLAbstractShader
  //
  { : Generic, abstract shader class.<p>
    Shaders are modeled here as an abstract entity with
    transaction-like behaviour. The base class provides basic context and user
    tracking, as well as setup/application facilities.<br>
    Subclasses are expected to provide implementation for DoInitialize,
    DoApply, DoUnApply and DoFinalize. }
  TDGLAbstractShader = class(TDGLUpdateAbleComponent)
  private
    { Private Declarations }
    FEnabled:          Boolean;
//    FLibMatUsers:      TList;
    FVirtualHandle:    TDGLVirtualHandle;
    FShaderStyle:      TDGLShaderStyle;
    FUpdateCount:      Integer;
    FShaderActive:     Boolean;
    FFailedInitAction: TDGLShaderFailedInitAction;

  protected
    { Protected Declarations }
    { : Invoked once, before the first call to DoApply.<p>
      The call happens with the OpenGL context being active. }
    procedure DoInitialize(var rci: TRenderContextInfo; Sender: TObject); dynamic;
    { : Request to apply the shader.<p>
      Always followed by a DoUnApply when the shader is no longer needed. }
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject);  virtual;
    { : Request to un-apply the shader.<p>
      Subclasses can assume the shader has been applied previously.<br>
      Return True to request a multipass. }
    function DoUnApply(var rci: TRenderContextInfo): Boolean;  virtual;
    { : Invoked once, before the destruction of context or release of shader.<p>
      The call happens with the OpenGL context being active. }
    procedure DoFinalize; dynamic;

    function GetShaderInitialized: Boolean;
    procedure InitializeShader(var rci: TRenderContextInfo; Sender: TObject);
    procedure FinalizeShader;
    procedure OnVirtualHandleAllocate(Sender: TDGLVirtualHandle; var handle: Cardinal);
    procedure OnVirtualHandleDestroy(Sender: TDGLVirtualHandle; var handle: Cardinal);
    procedure SetEnabled(val: Boolean);

    property ShaderInitialized: Boolean read GetShaderInitialized;
    property ShaderActive: Boolean read FShaderActive;

//    procedure RegisterUser(libMat: TDGLLibMaterial);
//    procedure UnRegisterUser(libMat: TDGLLibMaterial);

    { : Used by the DoInitialize procedure of descendant classes to raise errors. }
    procedure HandleFailedInitialization(const LastErrorMessage: string = ''); virtual;

    { : May be this should be a function inside HandleFailedInitialization... }
    function GetStardardNotSupportedMessage: string; virtual;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { : Subclasses should invoke this function when shader properties are altered.
      This procedure can also be used to reset/recompile the shader. }
    procedure NotifyChange(Sender: TObject); override;
    procedure BeginUpdate;
    procedure EndUpdate;

    { : Apply shader to OpenGL state machine. }
    procedure Apply(var rci: TRenderContextInfo; Sender: TObject);
    { : UnApply shader.<p>
      When returning True, the caller is expected to perform a multipass
      rendering by re-rendering then invoking UnApply again, until a
      "False" is returned. }
    function UnApply(var rci: TRenderContextInfo): Boolean;

    { : Shader application style (default is ssLowLevel). }
    property ShaderStyle: TDGLShaderStyle read FShaderStyle write FShaderStyle default ssLowLevel;

    procedure Assign(Source: TPersistent); override;

    { : Defines if shader is supported by hardware/drivers.
      Default - always supported. Descendants are encouraged to override
      this function. }
    function ShaderSupported: Boolean; virtual;

    { : Defines what to do if for some reason shader failed to initialize.
      Note, that in some cases it cannon be determined by just checking the
      required OpenGL extentions. You need to try to compile and link the
      shader - only at that stage you might catch an error }
    property FailedInitAction: TDGLShaderFailedInitAction read FFailedInitAction write FFailedInitAction default fiaRaiseStandardException;

  published
    { Published Declarations }
    { : Turns on/off shader application.<p>
      Note that this only turns on/off the shader application, if the
      ShaderStyle is ssReplace, the material won't be applied even if
      the shader is disabled. }
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TDGLShaderClass = class of TDGLAbstractShader;

  { : A pure abstract class, must be overriden. }
  TDGLCustomShader = class(TDGLAbstractShader)
  private
    FFragmentProgram: TDGLFragmentProgram;
    FVertexProgram:   TDGLVertexProgram;
    FGeometryProgram: TDGLGeometryProgram;

    FTagObject: TObject;
    procedure SetFragmentProgram(const Value: TDGLFragmentProgram);
    procedure SetGeometryProgram(const Value: TDGLGeometryProgram);
    procedure SetVertexProgram(const Value: TDGLVertexProgram);
    function StoreFragmentProgram: Boolean;
    function StoreGeometryProgram: Boolean;
    function StoreVertexProgram: Boolean;
  protected
    FDebugMode: Boolean;
    procedure SetDebugMode(const Value: Boolean); virtual;

    property FragmentProgram: TDGLFragmentProgram read FFragmentProgram write SetFragmentProgram stored StoreFragmentProgram;
    property VertexProgram: TDGLVertexProgram read FVertexProgram write SetVertexProgram stored StoreVertexProgram;
    property GeometryProgram: TDGLGeometryProgram read FGeometryProgram write SetGeometryProgram stored StoreGeometryProgram;

    { : Treats warnings as errors and displays this error,
      instead of a general shader-not-supported message. }
    property DebugMode: Boolean read FDebugMode write SetDebugMode default False;
    property TagObject: TObject read FTagObject write FTagObject default nil;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadShaderPrograms(const VPFilename, FPFilename: string; GPFilename: string = '');
  end;

  { : A custom shader program. }
  TDGLShaderProgram = class(TPersistent)
  private
    FParent:  TDGLCustomShader;
    FEnabled: Boolean;
    FCode:    TStrings;
    procedure SetCode(const Value: TStrings);
    procedure SetEnabled(const Value: Boolean);
    procedure OnChangeCode(Sender: TObject);
  protected
    function GetOwner: TPersistent; override;
  public
    procedure LoadFromFile(const AFileName: string);
    procedure Apply; virtual;
    constructor Create(const AParent: TDGLCustomShader); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Code:    TStrings read FCode write SetCode;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
  end;

  TDGLVertexProgram = class(TDGLShaderProgram)
  published
    property Code;
    property Enabled;
  end;

  TDGLFragmentProgram = class(TDGLShaderProgram)
  published
    property Code;
    property Enabled;
  end;

  TDGLGeometryProgram = class(TDGLShaderProgram)
  private
    FInputPrimitiveType:  TDGLgsInTypes;
    FOutputPrimitiveType: TDGLgsOutTypes;
    FVerticesOut:         TGLint;
    procedure SetInputPrimitiveType(const Value: TDGLgsInTypes);
    procedure SetOutputPrimitiveType(const Value: TDGLgsOutTypes);
    procedure SetVerticesOut(const Value: TGLint);
  public
    constructor Create(const AParent: TDGLCustomShader); override;
  published
    property Code;
    property Enabled;

    property InputPrimitiveType:  TDGLgsInTypes read FInputPrimitiveType write SetInputPrimitiveType default gsInPoints;
    property OutputPrimitiveType: TDGLgsOutTypes read FOutputPrimitiveType write SetOutputPrimitiveType default gsOutPoints;
    property VerticesOut:         TGLint read FVerticesOut write SetVerticesOut default 0;
  end;

  { : Wrapper around a Attrib of the main program. }
  TDGLCustomShaderAttrib = class(TObject)
  private
    { Private Declarations }
  protected
    { Protected Declarations }
  public

    procedure SetAsVector1f(const Value: Single); virtual; abstract;
    procedure SetAsVector2f(const Value: TVector2f); virtual; abstract;
    procedure SetAsVector3f(const Value: TVector3f); virtual; abstract;
    procedure SetAsVector4f(const Value: TVector4f); virtual; abstract;

    procedure SetAsVector4fArray(const Value: TAffineVectorList); virtual; abstract;

    procedure SetAsVector1i(const Value: Integer); virtual; abstract;
    procedure SetAsVector2i(const Value: TVector2i); virtual; abstract;
    procedure SetAsVector3i(const Value: TVector3i); virtual; abstract;
    procedure SetAsVector4i(const Value: TVector4i); virtual; abstract;

    procedure SetAsVector1ui(const Value: GLuint); virtual; abstract;
    procedure SetAsVector2ui(const Value: TVector2ui); virtual; abstract;
    procedure SetAsVector3ui(const Value: TVector3ui); virtual; abstract;
    procedure SetAsVector4ui(const Value: TVector4ui); virtual; abstract;

    procedure SetAsMatrix2f(const Value: TMatrix2f); virtual; abstract;
    procedure SetAsMatrix3f(const Value: TMatrix3f); virtual; abstract;
    procedure SetAsMatrix4f(const Value: TMatrix4f); virtual; abstract;

  end;

  { : Wrapper around a parameter of the main program. }
//  TDGLCustomShaderParameter = class(TObject)
//  private
//    { Private Declarations }
//  protected
//    { Protected Declarations }
//    function GetAsVector1f: Single; virtual; abstract;
//    function GetAsVector2f: TVector2f; virtual; abstract;
//    function GetAsVector3f: TVector3f; virtual; abstract;
//    function GetAsVector4f: TVector; virtual; abstract;
//
//    function GetAsVector1i: Integer; virtual; abstract;
//    function GetAsVector2i: TVector2i; virtual; abstract;
//    function GetAsVector3i: TVector3i; virtual; abstract;
//    function GetAsVector4i: TVector4i; virtual; abstract;
//
//    function GetAsVector1ui: GLuint; virtual; abstract;
//    function GetAsVector2ui: TVector2ui; virtual; abstract;
//    function GetAsVector3ui: TVector3ui; virtual; abstract;
//    function GetAsVector4ui: TVector4ui; virtual; abstract;
//
//    procedure SetAsVector1f(const Value: Single); virtual; abstract;
//    procedure SetAsVector2f(const Value: TVector2f); virtual; abstract;
//    procedure SetAsVector3f(const Value: TVector3f); virtual; abstract;
//    procedure SetAsVector4f(const Value: TVector4f); virtual; abstract;
//
//    procedure SetAsVector1i(const Value: Integer); virtual; abstract;
//    procedure SetAsVector2i(const Value: TVector2i); virtual; abstract;
//    procedure SetAsVector3i(const Value: TVector3i); virtual; abstract;
//    procedure SetAsVector4i(const Value: TVector4i); virtual; abstract;
//
//    procedure SetAsVector1ui(const Value: GLuint); virtual; abstract;
//    procedure SetAsVector2ui(const Value: TVector2ui); virtual; abstract;
//    procedure SetAsVector3ui(const Value: TVector3ui); virtual; abstract;
//    procedure SetAsVector4ui(const Value: TVector4ui); virtual; abstract;
//
//    function GetAsMatrix2f: TMatrix2f; virtual; abstract;
//    function GetAsMatrix3f: TMatrix3f; virtual; abstract;
//    function GetAsMatrix4f: TMatrix4f; virtual; abstract;
//    procedure SetAsMatrix2f(const Value: TMatrix2f); virtual; abstract;
//    procedure SetAsMatrix3f(const Value: TMatrix3f); virtual; abstract;
//    procedure SetAsMatrix4f(const Value: TMatrix4f); virtual; abstract;

//    procedure SetAsTexture(const TextureIndex: Integer; const Value: TDGLTexture);
//    procedure SetAsTexture1D(const TextureIndex: Integer; const Value: TDGLTexture);
//    procedure SetAsTexture2D(const TextureIndex: Integer; const Value: TDGLTexture);
//    procedure SetAsTexture3D(const TextureIndex: Integer; const Value: TDGLTexture);
//    procedure SetAsTextureCube(const TextureIndex: Integer; const Value: TDGLTexture);
//    procedure SetAsTextureRect(const TextureIndex: Integer; const Value: TDGLTexture);
//
//    function GetAsCustomTexture(const TextureIndex: Integer; TextureTarget: TDGLTextureTarget): Cardinal; virtual; abstract;
//    procedure SetAsCustomTexture(const TextureIndex: Integer; TextureTarget: TDGLTextureTarget; const Value: Cardinal); virtual; abstract;

//    function GetAsUniformBuffer: GLenum; virtual; abstract;
//    procedure SetAsUniformBuffer(UBO: GLenum); virtual; abstract;
//  public
//    { Public Declarations }
//
//    { : This overloaded SetAsVector accepts open array as input. e.g.
//      SetAsVectorF([0.1, 0.2]). Array length must between 1-4. }
//    procedure SetAsVectorF(const Values: array of Single); overload;
//    procedure SetAsVectorI(const Values: array of Integer); overload;
//
//    { : SetToTextureOf determines texture type on-the-fly. }
////    procedure SetToTextureOf(const LibMaterial: TDGLLibMaterial; const TextureIndex: Integer); overload;
////    procedure SetToTextureOf(const Texture: TDGLTexture; const TextureIndex: Integer); overload;
//
//    // : GLScene-friendly properties.
//    property AsVector: TVector read GetAsVector4f write SetAsVector4f;
//    property AsAffineVector: TAffineVector read GetAsVector3f write SetAsVector3f;
//
//    // : Standard types.
//    property AsFloat: Single read GetAsVector1f write SetAsVector1f;
//    property AsInteger: Integer read GetAsVector1i write SetAsVector1i;
//
//    // : Float vector types.
//    property AsVector1f: Single read GetAsVector1f write SetAsVector1f;
//    property AsVector2f: TVector2f read GetAsVector2f write SetAsVector2f;
//    property AsVector3f: TVector3f read GetAsVector3f write SetAsVector3f;
//    property AsVector4f: TVector4f read GetAsVector4f write SetAsVector4f;
//
//    // : Integer vector  types.
//    property AsVector1i: Integer read GetAsVector1i write SetAsVector1i;
//    property AsVector2i: TVector2i read GetAsVector2i write SetAsVector2i;
//    property AsVector3i: TVector3i read GetAsVector3i write SetAsVector3i;
//    property AsVector4i: TVector4i read GetAsVector4i write SetAsVector4i;
//
//    // : Unsigned integer vector  types.
//    property AsVector1ui: GLuint read GetAsVector1ui write SetAsVector1ui;
//    property AsVector2ui: TVector2ui read GetAsVector2ui write SetAsVector2ui;
//    property AsVector3ui: TVector3ui read GetAsVector3ui write SetAsVector3ui;
//    property AsVector4ui: TVector4ui read GetAsVector4ui write SetAsVector4ui;
//
//    // : Matrix Types.
//    property AsMatrix2f: TMatrix2f read GetAsMatrix2f write SetAsMatrix2f;
//    property AsMatrix3f: TMatrix3f read GetAsMatrix3f write SetAsMatrix3f;
//    property AsMatrix4f: TMatrix4f read GetAsMatrix4f write SetAsMatrix4f;

//    // : Texture Types.
//    property AsTexture[const TextureIndex: Integer]: TDGLTexture write SetAsTexture;
//    property AsTexture1D[const TextureIndex: Integer]: TDGLTexture write SetAsTexture1D;
//    property AsTexture2D[const TextureIndex: Integer]: TDGLTexture write SetAsTexture2D;
//    property AsTexture3D[const TextureIndex: Integer]: TDGLTexture write SetAsTexture3D;
//    property AsTextureRect[const TextureIndex: Integer]: TDGLTexture write SetAsTextureRect;
//    property AsTextureCube[const TextureIndex: Integer]: TDGLTexture write SetAsTextureCube;
//
//    property AsCustomTexture[const TextureIndex: Integer; TextureTarget: TDGLTextureTarget]: Cardinal read GetAsCustomTexture write SetAsCustomTexture;

//    property AsUniformBuffer: GLenum read GetAsUniformBuffer write SetAsUniformBuffer;
//  end;


  // Exported procedures.
procedure ApplyBlendingModeEx(const BlendingMode: TDGLBlendingModeEx);
procedure UnApplyBlendingModeEx;
procedure InitTexture(const TextureHandle: Cardinal; const TextureSize: TDGLSize; const TextureTarget: TDGLTextureTarget = ttTexture2D);

procedure CopyScreentoTexture(const ViewPortSize: TDGLSize; const TextureTarget: Word = GL_TEXTURE_2D);
procedure CopyScreentoTexture2(const ViewPortSize: TDGLSize; const TextureTarget: Word = GL_TEXTURE_2D);

//function IsFogEnabled(const AFogSupportMode: TGLShaderFogSupport; var rci: TRenderContextInfo): Boolean;
//procedure GetActiveLightsList(const ALightIDs: TIntegerList);

implementation


// ------------------
{ Helpers Functions }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

//procedure GetActiveLightsList(const ALightIDs: TIntegerList);
//var
//  I: Integer;
//begin
//  ALightIDs.Clear;
//  with CurrentDGLContext.GLStates do
//  begin
//    for I := 0 to MaxLights - 1 do
//    begin
//      if LightEnabling[I] then
//        ALightIDs.Add(I);
//    end;
//  end;
//end;

//function IsFogEnabled(const AFogSupportMode: TGLShaderFogSupport; var rci: TRenderContextInfo): Boolean;
//begin
//  case AFogSupportMode of
//    sfsEnabled:
//      Result := True;
//    sfsDisabled:
//      Result := False;
//    sfsAuto:
//      Result := TDGLSceneBuffer(rci.buffer).FogEnable;
//  else
//    Result := False;
//    Assert(False, glsUnknownType);
//  end;
//end;

procedure CopyScreentoTexture(const ViewPortSize: TDGLSize; const TextureTarget: Word = GL_TEXTURE_2D);
begin
  glCopyTexSubImage2D(TextureTarget, 0, 0, 0, 0, 0, ViewPortSize.cx, ViewPortSize.cy);
end;

procedure CopyScreentoTexture2(const ViewPortSize: TDGLSize; const TextureTarget: Word = GL_TEXTURE_2D);
begin
  glCopyTexImage2D(TextureTarget, 0, GL_RGB, 0, 0, ViewPortSize.cx, ViewPortSize.cy, 0);
end;

procedure ApplyBlendingModeEx(const BlendingMode: TDGLBlendingModeEx);
begin
  with CurrentDGLContext.GLStates do
  begin
    Enable(stBlend);

    case BlendingMode of
      bmxOpaque:
        SetBlendFunc(bfSRCALPHA, bfONE);
      bmxTransparency:
        SetBlendFunc(bfSRCALPHA, bfONEMINUSSRCALPHA);
      bmxAdditive:
        SetBlendFunc(bfSRCALPHA, bfONE);
//      bmxAlphaTest50:
//        SetGLAlphaFunction(cfGEQUAL, 0.5);
//      bmxAlphaTest100:
//        SetGLAlphaFunction(cfGEQUAL, 1.0);
      bmxModulate:
        SetBlendFunc(bfDSTCOLOR, bfZERO);
      bmxDestColorOne:
        SetBlendFunc(bfDSTCOLOR, bfONE);
      bmxDestAlphaOne:
        SetBlendFunc(bfDSTALPHA, bfONE);
    else
      Assert(False, glsErrorEx + glsUnknownType);
    end;
  end;
end;

procedure UnApplyBlendingModeEx;
begin
end;


procedure InitTexture(const TextureHandle: Cardinal; const TextureSize: TDGLSize; const TextureTarget: TDGLTextureTarget = ttTexture2D);
var
  glTarget: TGLEnum;
begin
  with CurrentDGLContext.GLStates do
  begin
    TextureBinding[ActiveTexture, TextureTarget] := TextureHandle;
  end;
  glTarget := DecodeGLTextureTarget(TextureTarget);
  glTexParameteri(glTarget, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(glTarget, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(glTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(glTarget, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glCopyTexImage2D(glTarget, 0, GL_RGBA8, 0, 0, TextureSize.cx, TextureSize.cy, 0);
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLAbstractShader }
{$IFDEF GLS_REGION}{$REGION ' TDGLAbstractShader'}{$ENDIF}

constructor TDGLAbstractShader.Create(AOwner: TComponent);
begin
  FVirtualHandle            := TDGLVirtualHandle.Create;
  FVirtualHandle.OnAllocate := OnVirtualHandleAllocate;
  FVirtualHandle.OnDestroy  := OnVirtualHandleDestroy;
  FShaderStyle              := ssLowLevel;
  FEnabled                  := True;
  FFailedInitAction         := fiaRaiseStandardException;
  inherited;
end;

destructor TDGLAbstractShader.Destroy;
begin
  FVirtualHandle.DestroyHandle;
  FinalizeShader;
  inherited;
  FVirtualHandle.Free;
end;

procedure TDGLAbstractShader.NotifyChange(Sender: TObject);
begin
  if FUpdateCount = 0 then
  begin
    FinalizeShader;
  end;
end;

procedure TDGLAbstractShader.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDGLAbstractShader.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyChange(Self);
end;

procedure TDGLAbstractShader.DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
begin
  // nothing here
end;

procedure TDGLAbstractShader.DoFinalize;
begin
  // nothing here
end;

function TDGLAbstractShader.GetShaderInitialized: Boolean;
begin
  Result := (FVirtualHandle.handle <> 0);
end;

procedure TDGLAbstractShader.InitializeShader(var rci: TRenderContextInfo; Sender: TObject);
begin
  FVirtualHandle.AllocateHandle;
  if FVirtualHandle.IsDataNeedUpdate then
  begin
    DoInitialize(rci, Sender);
    FVirtualHandle.NotifyDataUpdated;
  end;
end;

procedure TDGLAbstractShader.FinalizeShader;
begin
  FVirtualHandle.NotifyChangesOfData;
  DoFinalize;
end;

procedure TDGLAbstractShader.DoApply(var Rci: TRenderContextInfo; Sender: TObject);
begin
end;

function TDGLAbstractShader.DoUnApply(var Rci: TRenderContextInfo): Boolean;
begin
  Result := True;
end;

procedure TDGLAbstractShader.Apply(var rci: TRenderContextInfo; Sender: TObject);
begin
  // Need to check it twice, because shader may refuse to initialize
  // and choose to disable itself during initialization.
  if FEnabled then
    if FVirtualHandle.IsDataNeedUpdate then
      InitializeShader(rci, Sender);

  if FEnabled then
    DoApply(rci, Sender);

  FShaderActive := True;
end;

function TDGLAbstractShader.UnApply(var rci: TRenderContextInfo): Boolean;
begin
  if Enabled then
  begin
    Result := DoUnApply(rci);
    if not Result then
      FShaderActive := False;
  end
  else
  begin
    FShaderActive := False;
    Result        := False;
  end;
end;

procedure TDGLAbstractShader.OnVirtualHandleDestroy(Sender: TDGLVirtualHandle; var handle: Cardinal);
begin
  handle := 0;
end;

procedure TDGLAbstractShader.OnVirtualHandleAllocate(Sender: TDGLVirtualHandle; var handle: Cardinal);
begin
  handle := 1;
end;

procedure TDGLAbstractShader.SetEnabled(val: Boolean);
begin
  if val <> FEnabled then
  begin
    FEnabled := val;
    NotifyChange(Self);
  end;
end;


procedure TDGLAbstractShader.Assign(Source: TPersistent);
begin
  if Source is TDGLAbstractShader then
  begin
    FShaderStyle      := TDGLAbstractShader(Source).FShaderStyle;
    FFailedInitAction := TDGLAbstractShader(Source).FFailedInitAction;
    Enabled           := TDGLAbstractShader(Source).FEnabled;
  end
  else
    inherited Assign(Source); // to the pit of doom ;)
end;

function TDGLAbstractShader.ShaderSupported: Boolean;
begin
  Result := True;
end;

procedure TDGLAbstractShader.HandleFailedInitialization(const LastErrorMessage: string = '');
begin
  case FailedInitAction of
    fiaSilentdisable:
      ; // Do nothing ;)
    fiaRaiseHandledException:
      try
        raise EGLShaderException.Create(GetStardardNotSupportedMessage);
      except
      end;
    fiaRaiseStandardException:
      raise EGLShaderException.Create(GetStardardNotSupportedMessage);
    fiaReRaiseException:
      begin
        if LastErrorMessage <> '' then
          raise EGLShaderException.Create(LastErrorMessage)
        else
          raise EGLShaderException.Create(GetStardardNotSupportedMessage)
      end;
    // fiaGenerateEvent:; // Do nothing. Event creation is left up to user shaders
    // // which may choose to override this procedure.
  else
    Assert(False, glsErrorEx + glsUnknownType);
  end;
end;

function TDGLAbstractShader.GetStardardNotSupportedMessage: string;
begin
  if Name <> '' then
    Result := 'Your hardware/driver doesn''t support shader "' + Name + '"!'
  else
    Result := 'Your hardware/driver doesn''t support shader "' + ClassName + '"!';
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLShaderProgram }
{$IFDEF GLS_REGION}{$REGION 'Shader Program'}{$ENDIF}

procedure TDGLShaderProgram.Apply;
begin
  FParent.FinalizeShader;
end;

procedure TDGLShaderProgram.Assign(Source: TPersistent);
begin
  if Source = nil then
    Exit;

  if (Source is TDGLShaderProgram) then
  begin
    FEnabled := TDGLShaderProgram(Source).FEnabled;
    FCode.Assign(TDGLShaderProgram(Source).FCode);
  end
  else
    inherited; // die, die, die!!!
end;

constructor TDGLShaderProgram.Create(const AParent: TDGLCustomShader);
begin
  FParent                     := AParent;
  FCode                       := TStringList.Create;
  TStringList(FCode).OnChange := OnChangeCode;
  FEnabled                    := False;
end;

destructor TDGLShaderProgram.Destroy;
begin
  FCode.Destroy;
end;

function TDGLShaderProgram.GetOwner: TPersistent;
begin
  Result := FParent;
end;

procedure TDGLShaderProgram.LoadFromFile(const AFileName: string);
begin
  FCode.LoadFromFile(AFileName);
  FEnabled := True;
end;

procedure TDGLShaderProgram.OnChangeCode(Sender: TObject);
begin
  FEnabled := True;
  FParent.NotifyChange(Self);
end;

procedure TDGLShaderProgram.SetCode(const Value: TStrings);
begin
  FCode.Assign(Value);
  FParent.NotifyChange(Self);
end;

procedure TDGLShaderProgram.SetEnabled(const Value: Boolean);
begin
  if Value = FEnabled then
    Exit;
  FEnabled := Value;
  if FEnabled then
    FParent.FinalizeShader;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// -----------------
{ TDGLCustomShader }
{$IFDEF GLS_REGION}{$REGION 'Custom Shader'}{$ENDIF}

procedure TDGLCustomShader.Assign(Source: TPersistent);
begin
  if Source is TDGLCustomShader then
  begin
    FFragmentProgram.Assign(TDGLCustomShader(Source).FFragmentProgram);
    FVertexProgram.Assign(TDGLCustomShader(Source).FVertexProgram);
    FGeometryProgram.Assign(TDGLCustomShader(Source).FGeometryProgram);
    FTagObject := TDGLCustomShader(Source).FTagObject;
  end;
  inherited;
end;

constructor TDGLCustomShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDebugMode       := False;
  FFragmentProgram := TDGLFragmentProgram.Create(Self);
  FVertexProgram   := TDGLVertexProgram.Create(Self);
  FGeometryProgram := TDGLGeometryProgram.Create(Self);
end;

destructor TDGLCustomShader.Destroy;
begin
  FFragmentProgram.Destroy;
  FVertexProgram.Destroy;
  FGeometryProgram.Destroy;

  inherited;
end;

procedure TDGLCustomShader.LoadShaderPrograms(const VPFilename, FPFilename: string; GPFilename: string = '');
begin
  If VPFilename <> '' then
    VertexProgram.LoadFromFile(VPFilename);
  If FPFilename <> '' then
    FragmentProgram.LoadFromFile(FPFilename);
  If GPFilename <> '' then
    GeometryProgram.LoadFromFile(GPFilename);
end;

procedure TDGLCustomShader.SetDebugMode(const Value: Boolean);
begin
  if FDebugMode <> Value then
  begin
    FDebugMode := Value;

    if FDebugMode then
      FailedInitAction := fiaReRaiseException
    else
      FailedInitAction := fiaRaiseStandardException;
  end;
end;

procedure TDGLCustomShader.SetFragmentProgram(const Value: TDGLFragmentProgram);
begin
  FFragmentProgram.Assign(Value);
end;

procedure TDGLCustomShader.SetGeometryProgram(const Value: TDGLGeometryProgram);
begin
  FGeometryProgram.Assign(Value);
end;

procedure TDGLCustomShader.SetVertexProgram(const Value: TDGLVertexProgram);
begin
  FVertexProgram.Assign(Value);
end;

function TDGLCustomShader.StoreFragmentProgram: Boolean;
begin
  Result := FFragmentProgram.Enabled or (FFragmentProgram.Code.Text <> '')
end;

function TDGLCustomShader.StoreGeometryProgram: Boolean;
begin
  Result := FGeometryProgram.Enabled or (FGeometryProgram.Code.Text <> '')
end;

function TDGLCustomShader.StoreVertexProgram: Boolean;
begin
  Result := FVertexProgram.Enabled or (FVertexProgram.Code.Text <> '')
end;

{ TGLCustomShaderParameter }

//procedure TDGLCustomShaderParameter.SetAsTexture(const TextureIndex: Integer; const Value: TDGLTexture);
//begin
//  SetAsCustomTexture(TextureIndex, Value.handle.Target, Value.handle);
//end;
//
//procedure TDGLCustomShaderParameter.SetAsTexture1D(const TextureIndex: Integer; const Value: TDGLTexture);
//begin
//  SetAsCustomTexture(TextureIndex, ttTexture1D, Value.handle);
//end;
//
//procedure TDGLCustomShaderParameter.SetAsTexture2D(const TextureIndex: Integer; const Value: TDGLTexture);
//begin
//  SetAsCustomTexture(TextureIndex, ttTexture2D, Value.handle);
//end;
//
//procedure TDGLCustomShaderParameter.SetAsTexture3D(const TextureIndex: Integer; const Value: TDGLTexture);
//begin
//  SetAsCustomTexture(TextureIndex, ttTexture3D, Value.handle);
//end;
//
//procedure TDGLCustomShaderParameter.SetAsTextureCube(const TextureIndex: Integer; const Value: TDGLTexture);
//begin
//  SetAsCustomTexture(TextureIndex, ttTextureCube, Value.handle);
//end;
//
//procedure TDGLCustomShaderParameter.SetAsTextureRect(const TextureIndex: Integer; const Value: TDGLTexture);
//begin
//  SetAsCustomTexture(TextureIndex, ttTextureRect, Value.handle);
//end;

//procedure TDGLCustomShaderParameter.SetAsVectorF(const Values: array of Single);
//begin
//  case Length(Values) of
//    1:
//      SetAsVector1f(Values[0]);
//    2:
//      SetAsVector2f(Vector2fMake(Values[0], Values[1]));
//    3:
//      SetAsVector3f(Vector3fMake(Values[0], Values[1], Values[2]));
//    4:
//      SetAsVector4f(Vector4fMake(Values[0], Values[1], Values[2], Values[3]));
//  else
//    Assert(False, 'Vector length must be between 1 to 4');
//  end;
//end;
//
//procedure TDGLCustomShaderParameter.SetAsVectorI(const Values: array of Integer);
//begin
//  case Length(Values) of
//    1:
//      SetAsVector1i(Values[0]);
//    2:
//      SetAsVector2i(Vector2iMake(Values[0], Values[1]));
//    3:
//      SetAsVector3i(Vector3iMake(Values[0], Values[1], Values[2]));
//    4:
//      SetAsVector4i(Vector4iMake(Values[0], Values[1], Values[2], Values[3]));
//  else
//    Assert(False, 'Vector length must be between 1 to 4');
//  end;
//end;

//procedure TDGLCustomShaderParameter.SetToTextureOf(const LibMaterial: TDGLLibMaterial; const TextureIndex: Integer);
//begin
//  SetToTextureOf(LibMaterial.Material.Texture, TextureIndex);
//end;
//
//procedure TDGLCustomShaderParameter.SetToTextureOf(const Texture: TDGLTexture; const TextureIndex: Integer);
//begin
//  SetAsCustomTexture(TextureIndex, Texture.Image.NativeTextureTarget, Texture.handle);
//end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// -----------------
{ TGLGeometryProgram }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

constructor TDGLGeometryProgram.Create(const AParent: TDGLCustomShader);
begin
  inherited Create(AParent);
  FInputPrimitiveType  := gsInPoints;
  FOutputPrimitiveType := gsOutPoints;
  FVerticesOut         := 0;
end;

procedure TDGLGeometryProgram.SetInputPrimitiveType(const Value: TDGLgsInTypes);
begin
  if Value <> FInputPrimitiveType then
  begin
    FInputPrimitiveType := Value;
    FParent.NotifyChange(Self);
  end;
end;

procedure TDGLGeometryProgram.SetOutputPrimitiveType(const Value: TDGLgsOutTypes);
begin
  if Value <> FOutputPrimitiveType then
  begin
    FOutputPrimitiveType := Value;
    FParent.NotifyChange(Self);
  end;
end;

procedure TDGLGeometryProgram.SetVerticesOut(const Value: TGLint);
begin
  if Value <> FVerticesOut then
  begin
    FVerticesOut := Value;
    FParent.NotifyChange(Self);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

initialization

RegisterClasses([TDGLCustomShader, TDGLShaderProgram, TDGLVertexProgram, TDGLFragmentProgram, TDGLGeometryProgram]);

end.
