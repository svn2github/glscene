//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : DGLMaterial
  @HTML (
  <p>
  Handles Materials via TDGLMaterilLibrary<br>
  All the colors and texture stuff, all the materials and it components: textures, samplers, combiners, FBO Texture and etc.
  </p>
  <p>
  Features:
  <ul>
    <li> Material can contain different level of applying accordingly to hardware i.e. Features scaling.</li>
    <li> If automatically or by user selected level failed, material down to lower level.</li>
    <li> Direct state access can be used for uniforms setting. (material is linked with shader in ShaderLibrary)</li>
    <li> Economy mode for texture binding to active units<br>
         i.e. if textures less than maximum units may be not one binding occur per frame.
    </li>
  </ul>

  <p>
  <b>History : </b><font size=-1><ul>
  <li>24/12/15 - JD - Imported, Updated and Improved From GLScene (GLMaterialEx)
  </ul></font></p>

  <p>
  <b>Status : </b>In Progress<br>
  <b>Todo : </b>
  <ul>
      <li>Support for Pixel Buffer Object (PBO)</li>
      <li>Add TDGLTextureCube = class(TDGLTexture)</li>
      <li>Add TDGLTexture3D = class(TDGLTexture)</li>
      <li>Add TDGLProceduralTexture = class(TDGLTexture)</li>
      <li>Add Stream Loading Without Windows Services Context (see DGLGraphic)</li>
  </ul></p> )
}
unit DGLMaterial;

interface

{$I DGLEngine.inc}

uses
  System.Classes,System.SysUtils,System.Types,
  // GLS
  DGLCrossPlatform,DGLResStrings,DGLSLog,dglOpenGL,
  DGLTypes,DGLBaseClasses,DGLPersistentClasses,DGLXCollection,DGLApplicationFileIO,DGLUtils,
  DGLContext,DGLContextHandles,DGLState,DGLRenderContextInfo,
  DGLVectorTypes,DGLVectorMaths,DGLCoordinates,
  DGLGraphics,DGLTextureFormat,DGLColor,DGLImageUtils,DGLTextureCombiners;

type
  // ****************************************************************************************
  ETexture = class(Exception);

  TDGLMaterialComponentName = string;
  TDGLLibMaterialName        = string;

  // TDGLFaceProperties = class;

  // TDGLMaterial = class;
  TDGLAbstractMaterialLibrary = class;
  TDGLMaterialLibrary         = class;
  TDGLAbstractLibMaterial     = class;
  TDGLLibMaterial             = class;
  TDGLTexture = class;

  // ****************************************************************************************
  // an interface for proper TDGLLibMaterialNameProperty support
  IDGLMaterialLibrarySupported = interface(IInterface)
    ['{F33BE60E-98ED-4452-8224-0182145B19A7}']
    function GetMaterialLibrary: TDGLAbstractMaterialLibrary;
  end;

  TShininess       = 0 .. 128;
  TMaterialOptions = set of TMaterialOption;

  // ****************************************************************************************
  // TDGLAbstractLibMaterial
  //
  TDGLAbstractLibMaterial = class( TCollectionItem, IDGLMaterialLibrarySupported, IDGLNotifyAble)
  protected
    { Protected Declarations }
    FUserList: TList;
    FName: TDGLLibMaterialName;
    FNameHashKey: Integer;
    FTag: Integer;
    FNotifying: Boolean; // used for recursivity protection
    //implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TDGLAbstractMaterialLibrary;
    //implementing IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    { Protected Declarations }
    function GetDisplayName: string; override;
    class function ComputeNameHashKey(const name: string): Integer;
    procedure SetName(const val: TDGLLibMaterialName);
    procedure Loaded; virtual;

  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Apply(var ARci: TRenderContextInfo); virtual;
    //: Restore non-standard material states that were altered
    function UnApply(var ARci: TRenderContextInfo): Boolean; virtual;

    procedure RegisterUser(obj: TDGLUpdateAbleObject); overload;
    procedure UnregisterUser(obj: TDGLUpdateAbleObject); overload;
    procedure RegisterUser(comp: TDGLUpdateAbleComponent); overload;
    procedure UnregisterUser(comp: TDGLUpdateAbleComponent); overload;
    procedure RegisterUser(libMaterial: TDGLLibMaterial); overload;
    procedure UnregisterUser(libMaterial: TDGLLibMaterial); overload;
    procedure NotifyUsers;
    function IsUsed: boolean; //returns true if the texture has registed users
    property NameHashKey: Integer read FNameHashKey;
    procedure NotifyChange(Sender: TObject); virtual;
    function Blended: Boolean; virtual;
    property MaterialLibrary: TDGLAbstractMaterialLibrary read GetMaterialLibrary;
  published
    { Published Declarations }
    property Name: TDGLLibMaterialName read FName write SetName;
    property Tag: Integer read FTag write FTag;
  end;

  // ****************************************************************************************
  // TDGLAbstractLibMaterials
  //
  TDGLAbstractLibMaterials = class(TOwnedCollection)
  protected
    { Protected Declarations }
    procedure Loaded;
    function GetMaterial(const AName: TDGLLibMaterialName): TDGLAbstractLibMaterial; {$IFDEF GLS_INLINE}inline;{$ENDIF}
  public
    function MakeUniqueName(const nameRoot: TDGLLibMaterialName): TDGLLibMaterialName; virtual;
  end;

  // ****************************************************************************************
  // TGLAbstractMaterialLibrary
  //
  TDGLAbstractMaterialLibrary = class(TDGLCadenceAbleComponent)
  protected
    { Protected Declarations }
    FMaterials: TDGLAbstractLibMaterials;
    FLastAppliedMaterial: TDGLAbstractLibMaterial;
    FTexturePaths: string;
    FTexturePathList: TStringList;
    procedure SetTexturePaths(const val: string);
    property TexturePaths: string read FTexturePaths write SetTexturePaths;
    procedure Loaded; override;
  public
    { Public Declarations }

    procedure SetNamesToTStrings(AStrings: TStrings);
    { @HTML ( Applies the material of given name.<p>
       Returns False if the material could not be found. ake sure this
       call is balanced with a corresponding UnApplyMaterial (or an
       assertion will be triggered in the destructor).<br>
       If a material is already applied, and has not yet been unapplied,
       an assertion will be triggered. }
    function ApplyMaterial(const AName: string; var ARci: TRenderContextInfo): Boolean; virtual;
    { @HTML ( Un-applies the last applied material.<p>
       Use this function in conjunction with ApplyMaterial.<br>
       If no material was applied, an assertion will be triggered. }
    function UnApplyMaterial(var ARci: TRenderContextInfo): Boolean; virtual;
  end;

  // ****************************************************************************************
  // TDGLLibMaterials
  //
  TDGLLibMaterials = class(TDGLAbstractLibMaterials)
  protected
    procedure SetItems(AIndex: Integer; const AValue: TDGLLibMaterial);
    function GetItems(AIndex: Integer): TDGLLibMaterial;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);

    function MaterialLibrary: TDGLMaterialLibrary;

    function IndexOf(const Item: TDGLLibMaterial): Integer;
    function Add: TDGLLibMaterial;
    function FindItemID(ID: Integer): TDGLLibMaterial;
    property Items[index: Integer]: TDGLLibMaterial read GetItems write SetItems; default;
    function GetLibMaterialByName(const AName: TDGLLibMaterialName): TDGLLibMaterial;
  end;

  // ****************************************************************************************
  // TGLFaceProperties
  //
  { : Stores basic face lighting properties.<p>
    The lighting is described with the standard ambient/diffuse/emission/specular
    properties that behave like those of most rendering tools.<br>
    You also have control over shininess (governs specular lighting) and
    polygon mode (lines / fill). }
  TDGLFaceProperties = class(TDGLUpdateAbleObject)
  private
    { Private Declarations }
    FAmbient, FDiffuse, FSpecular, FEmission: TDGLColor;
    FShininess:                               TShininess;

  protected
    { Protected Declarations }
    procedure SetAmbient(AValue: TDGLColor);
    procedure SetDiffuse(AValue: TDGLColor);
    procedure SetEmission(AValue: TDGLColor);
    procedure SetSpecular(AValue: TDGLColor);
    procedure SetShininess(AValue: TShininess);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Apply(var rci: TRenderContextInfo; aFace: TCullFaceMode);
    procedure ApplyNoLighting(var rci: TRenderContextInfo; aFace: TCullFaceMode);
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property Ambient:   TDGLColor read FAmbient write SetAmbient;
    property Diffuse:   TDGLColor read FDiffuse write SetDiffuse;
    property Emission:  TDGLColor read FEmission write SetEmission;
    property Shininess: TShininess read FShininess write SetShininess default 0;
    property Specular:  TDGLColor read FSpecular write SetSpecular;
  end;

  // ****************************************************************************************
  // TDGLDepthParameters
  //
  TDGLDepthProperties = class(TDGLUpdateAbleObject)
  private
    { Private Declarations }
    FDepthTest:    boolean;
    FDepthWrite:   boolean;
    FZNear, FZFar: Single;
    FCompareFunc:  TDepthfunction;
    FDepthClamp:   boolean;
  protected
    { Protected Declarations }
    procedure SetZNear(Value: Single);
    procedure SetZFar(Value: Single);
    procedure SetCompareFunc(Value: TDGLDepthCompareFunc);
    procedure SetDepthTest(Value: boolean);
    procedure SetDepthWrite(Value: boolean);
    procedure SetDepthClamp(Value: boolean);

    function StoreZNear: boolean;
    function StoreZFar: boolean;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;

    procedure Apply(var rci: TRenderContextInfo);
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    { : Specifies the mapping of the near clipping plane to
      window coordinates.  The initial value is 0. }
    property ZNear: Single read FZNear write SetZNear stored StoreZNear;
    { : Specifies the mapping of the far clipping plane to
      window coordinates.  The initial value is 1. }
    property ZFar: Single read FZFar write SetZFar stored StoreZFar;
    { : Specifies the function used to compare each
      incoming pixel depth value with the depth value present in
      the depth buffer. }
    property DepthCompareFunction: TDepthfunction read FCompareFunc write SetCompareFunc default cfLequal;
    { : DepthTest enabling.<p>
      When DepthTest is enabled, objects closer to the camera will hide
      farther ones (via use of Z-Buffering).<br>
      When DepthTest is disabled, the latest objects drawn/rendered overlap
      all previous objects, whatever their distance to the camera.<br>
      Even when DepthTest is enabled, objects may chose to ignore depth
      testing through the osIgnoreDepthBuffer of their ObjectStyle property. }
    property DepthTest: boolean read FDepthTest write SetDepthTest default True;
    { : If True, object will not write to Z-Buffer. }
    property DepthWrite: boolean read FDepthWrite write SetDepthWrite default True;
    { : Enable clipping depth to the near and far planes }
    property DepthClamp: boolean read FDepthClamp write SetDepthClamp default False;
  end;

  // ****************************************************************************************
  // TDGLBlendingParameters
  //
  TDGLBlendingParameters = class(TDGLUpdateAbleObject)
  private
//    FUseAlphaFunc:          boolean;
    FUseBlendFunc:          boolean;
    FSeparateBlendFunc:     boolean;
//    FAlphaFuncType:         TDGlAlphaFunc;
//    FAlphaFuncRef:          TGLclampf;
    FBlendFuncSFactor:      TBlendFunction;
    FBlendFuncDFactor:      TBlendFunction;
    FAlphaBlendFuncSFactor: TBlendFunction;
    FAlphaBlendFuncDFactor: TBlendFunction;
//    procedure SetUseAlphaFunc(const Value: boolean);
    procedure SetUseBlendFunc(const Value: boolean);
    procedure SetSeparateBlendFunc(const Value: boolean);
//    procedure SetAlphaFuncRef(const Value: TGLclampf);
//    procedure SetAlphaFuncType(const Value: TDGlAlphaFunc);
    procedure SetBlendFuncDFactor(const Value: TBlendFunction);
    procedure SetBlendFuncSFactor(const Value: TBlendFunction);
    procedure SetAlphaBlendFuncDFactor(const Value: TBlendFunction);
    procedure SetAlphaBlendFuncSFactor(const Value: TBlendFunction);
//    function StoreAlphaFuncRef: boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Apply(var rci: TRenderContextInfo);
  published
//    property UseAlphaFunc:   boolean read FUseAlphaFunc write SetUseAlphaFunc default False;
//    property AlphaFunctType: TDGlAlphaFunc read FAlphaFuncType write SetAlphaFuncType default cfGreater;
//    property AlphaFuncRef:   TGLclampf read FAlphaFuncRef write SetAlphaFuncRef stored StoreAlphaFuncRef;

    property UseBlendFunc:          boolean read FUseBlendFunc write SetUseBlendFunc default True;
    property SeparateBlendFunc:     boolean read FSeparateBlendFunc write SetSeparateBlendFunc default False;
    property BlendFuncSFactor:      TBlendFunction read FBlendFuncSFactor write SetBlendFuncSFactor default bfSrcAlpha;
    property BlendFuncDFactor:      TBlendFunction read FBlendFuncDFactor write SetBlendFuncDFactor default bfOneMinusSrcAlpha;
    property AlphaBlendFuncSFactor: TBlendFunction read FAlphaBlendFuncSFactor write SetAlphaBlendFuncSFactor default bfSrcAlpha;
    property AlphaBlendFuncDFactor: TBlendFunction read FAlphaBlendFuncDFactor write SetAlphaBlendFuncDFactor default bfOneMinusSrcAlpha;
  end;

  // ****************************************************************************************
  // TDGLTextureSwizzling
  //
  { : Swizzle the components of a texture fetches in
    shader or fixed-function pipeline. }
  TDGLTextureSwizzling = class(TDGLUpdateAbleObject)
  private
    { Private Declarations }
    FSwizzles: TSwizzleVector;
    function GetSwizzle(AIndex: Integer): TDGLTextureSwizzle;
    procedure SetSwizzle(AIndex: Integer; AValue: TDGLTextureSwizzle);
    function StoreSwizzle(AIndex: Integer): Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    procedure WriteToFiler(AWriter: TWriter);
    procedure ReadFromFiler(AReader: TReader);
  published
    { Published Declarations }
    property RedFrom:   TDGLTextureSwizzle index 0 read GetSwizzle write SetSwizzle stored StoreSwizzle;
    property GreenFrom: TDGLTextureSwizzle index 1 read GetSwizzle write SetSwizzle stored StoreSwizzle;
    property BlueFrom:  TDGLTextureSwizzle index 2 read GetSwizzle write SetSwizzle stored StoreSwizzle;
    property AlphaFrom: TDGLTextureSwizzle index 3 read GetSwizzle write SetSwizzle stored StoreSwizzle;
  end;

  // ****************************************************************************************
  // TGLBaseMaterialCollectionItem
  //
  TDGLBaseMaterialCollectionItem = class(TDGLXCollectionItem, IDGLMaterialLibrarySupported)
  private
    { Private Declarations }
    FNameHashKey:  Integer;
    FUserList:     TDGLPersistentObjectList;
    FDefferedInit: Boolean;
    FNotifying:    Boolean;
    FIsValid:      Boolean;
    function GetUserList: TDGLPersistentObjectList;
    function GetMaterialLib: TDGLMaterialLibrary;
  protected
    { Protected Declarations }
    procedure SetName(const AValue: TDGLMaterialComponentName); override;
    procedure NotifyChange(Sender: TObject); virtual;
    property UserList: TDGLPersistentObjectList read GetUserList;
    procedure DoOnPrepare(Sender: TDGLContext); virtual; abstract;
  public
    { Public Declarations }
    destructor Destroy; override;

    procedure RegisterUser(AUser: TDGLUpdateAbleObject);
    procedure UnregisterUser(AUser: TDGLUpdateAbleObject);
    function GetUserCount: Integer;
    function GetMaterialLibrary: TDGLAbstractMaterialLibrary;

    property MaterialLibrary: TDGLMaterialLibrary read GetMaterialLib;
    property IsValid: Boolean read FIsValid;
  published
    { Published Declarations }
    property Name: TDGLMaterialComponentName read GetName write SetName;
    { : Run-time flag, indicate that resource
      should initialize in case of failure material's level. }
    property DefferedInit: Boolean read FDefferedInit write FDefferedInit default False;
  end;

  CGLBaseMaterialCollectionItem = class of TDGLBaseMaterialCollectionItem;

  // ****************************************************************************************
  // TGLLibMaterialProperty
  //
  TDGLLibMaterialProperty = class(TDGLUpdateAbleObject, IDGLMaterialLibrarySupported)
  protected
    { Protected Declarations }
    FEnabled:      Boolean;
    //FNextPassName: TDGLLibMaterialName;
    function GetMaterial: TDGLLibMaterial ;
    function GetMaterialLib: TDGLMaterialLibrary;
    procedure SetEnabled(AValue: Boolean); virtual;
    //procedure SetNextPass(const AValue: TDGLLibMaterialName);
    procedure Loaded; virtual;
    //property NextPass: TDGLLibMaterialName read FNextPassName write SetNextPass;
  public
    { Public Declarations }
    procedure NotifyChange(Sender: TObject); override;
    function GetMaterialLibrary: TDGLAbstractMaterialLibrary;

    property MaterialLibrary: TDGLMaterialLibrary read GetMaterialLib;
  published
    { Published Declarations }
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  // ****************************************************************************************
  // TDGLTextureSampler
  //
  TDGLTextureSampler = class(TDGLBaseMaterialCollectionItem)
  protected
    { Protected Declarations }
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    { Private Declarations }
    FHandle:           TDGLSamplerHandle;
    FMinFilter:        TDGLMinFilter;
    FMagFilter:        TDGLMagFilter;
    FFilteringQuality: TDGLTextureFilteringQuality;
    FLODBias:          Integer;
    FLODBiasFract:     Single;
    FWrap:             array [0 .. 2] of TDGLSeparateTextureWrap;
    FBorderColor:      TDGLColor;
    FCompareMode:      TDGLTextureCompareMode;
    FCompareFunc:      TDepthFunction;
    FDecodeSRGB:       Boolean;
    procedure SetMagFilter(AValue: TDGLMagFilter);
    procedure SetMinFilter(AValue: TDGLMinFilter);
    procedure SetLODBias(AValue: Integer);
    procedure SetFilteringQuality(AValue: TDGLTextureFilteringQuality);
    function GetWrap(Index: Integer): TDGLSeparateTextureWrap;
    procedure SetWrap(Index: Integer; AValue: TDGLSeparateTextureWrap);
    procedure SetBorderColor(const AValue: TDGLColor);
    procedure SetCompareMode(AValue: TDGLTextureCompareMode);
    procedure SetCompareFunc(AValue: TDepthFunction);
    procedure SetDecodeSRGB(AValue: Boolean);
  public
    { Public Declarations }
    constructor Create(AOwner: TDGLXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure NotifyChange(Sender: TObject); override;

    procedure DoOnPrepare(Sender: TDGLContext); override;
    procedure Apply(var ARci: TRenderContextInfo);
    procedure UnApply(var ARci: TRenderContextInfo);

    class function FriendlyName: string; override;

    property Handle: TDGLSamplerHandle read FHandle;
    property LodBiasFract: Single read FLODBiasFract;
  published
    { Published Declarations }

    { : Texture magnification filter. }
    property MagFilter: TDGLMagFilter read FMagFilter write SetMagFilter default maLinear;
    { : Texture minification filter. }
    property MinFilter:        TDGLMinFilter read FMinFilter write SetMinFilter default miLinearMipMapLinear;
    property FilteringQuality: TDGLTextureFilteringQuality read FFilteringQuality write SetFilteringQuality default tfAnisotropic;
    { : Texture LOD bias. }
    property LodBias: Integer read FLODBias write SetLODBias default 0;
    { : Address mode for the texture. }
    property WrapX: TDGLSeparateTextureWrap index 0 read GetWrap write SetWrap default twRepeat;
    property WrapY: TDGLSeparateTextureWrap index 1 read GetWrap write SetWrap default twRepeat;
    property WrapZ: TDGLSeparateTextureWrap index 2 read GetWrap write SetWrap default twRepeat;
    { : Texture border color. }
    property BorderColor: TDGLColor read FBorderColor write SetBorderColor;
    { : Compare mode and function for depth texture. }
    property CompareMode: TDGLTextureCompareMode read FCompareMode write SetCompareMode default tcmNone;
    property CompareFunc: TDepthFunction read FCompareFunc write SetCompareFunc default cfLEqual;
    { : Force retrieving the undecoded sRGB data from the
      texture and manipulate that directly. }
    property sRGB_Encode: Boolean read FDecodeSRGB write SetDecodeSRGB default True;
  end;

  // ****************************************************************************************
  // TDGLTextureCombiner
  //
  TDGLTextureCombiner = class(TDGLBaseMaterialCollectionItem)
  protected
    { Protected Declarations }
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    { Private Declarations }
    FHandle:       TDGLVirtualHandle;
    FScript:       TStringList;
    FCommandCache: TCombinerCache;
    procedure SetScript(AValue: TStringList);
    procedure DoAllocate(Sender: TDGLVirtualHandle; var Handle: TGLUint);
    procedure DoDeallocate(Sender: TDGLVirtualHandle; var Handle: TGLUint);
  public
    { Public Declarations }
    constructor Create(AOwner: TDGLXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure NotifyChange(Sender: TObject); override;

    procedure DoOnPrepare(Sender: TDGLContext); override;

    class function FriendlyName: string; override;
  published
    { Published Declarations }
    property Script: TStringList read FScript write SetScript;
  end;

  // ****************************************************************************************
  // TGLAbstractTexture
  //
  TDGLAbstractTexture = class(TDGLBaseMaterialCollectionItem)
  protected
    { Protected Declarations }
    FHandle:            TDGLTextureHandle;
    FInternalFormat:    TDGLInternalFormat;
    FWidth:             Integer;
    FHeight:            Integer;
    FDepth:             Integer;
    FSwizzles:          TSwizzleVector;
    FApplicableSampler: TDGLTextureSampler;
    FLastSampler:       TDGLTextureSampler;


    function GetTextureTarget: TDGLTextureTarget;
    procedure Apply(var ARci: TRenderContextInfo); virtual; abstract;
    procedure UnApply(var ARci: TRenderContextInfo); virtual; abstract;

    procedure SetSwizzles(const AIndex:Integer; const Value: TDGLTextureSwizzle);
    function GetSwizzles(const AIndex: Integer):TDGLTextureSwizzle;
  protected
    FFullSize         : Integer;
    FPixelSize        : Cardinal;
  public
    { Public Declarations }
    property InternalFormat:    TDGLInternalFormat read FInternalFormat write FInternalFormat;
    property Width:             Integer read FWidth write FWidth;
    property Height:            Integer read FHeight Write FHeight;
    property Depth:             Integer read FDepth write FDepth;
    property Swizzles[const AIndex: Integer]: TDGLTextureSwizzle read GetSwizzles Write SetSwizzles;

    property Handle: TDGLTextureHandle read FHandle;
    property Sampler : TDGLTextureSampler read FApplicableSampler write FApplicableSampler;
    property LastSampler : TDGLTextureSampler read FLastSampler write FLastSampler;
    property Format : TDGLInternalFormat read FInternalFormat Write FInternalFormat;
    property PixelSize : Cardinal read FPixelSize Write FPixelSize;
    property FullSize : Integer read FFUllSize write FFullSize;
  published
    { Published Declarations }
    property Shape: TDGLTextureTarget read GetTextureTarget;

  end;

  TDGLTextureCompression = TDGLInternalCompression;

  // TDGLTextureImage
  //
  // TTextureNeededEvent
  //
  TTextureNeededEvent = procedure(Sender: TObject; var textureFileName: string) of object;

  TDGLTextureChange = (tcImage, tcParams);
  TDGLTextureChanges = set of TDGLTextureChange;

  // ****************************************************************************************
  // TDGLTextureImageEx
  //
  TDGLTexture = class(TDGLAbstractTexture)
  protected
    { Protected Declarations }
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    { Private Declarations }
    FCompression:         TDGLTextureCompression;
    FImage:               TDGLBaseImage;
    FImageAlpha:          TDGLTextureImageAlpha;
    FImageBrightness:     Single;
    FImageGamma:          Single;
    FHeightToNormalScale: Single;
    FSourceFile:          string;
    FApplyCounter:        Integer;
    FInternallyStored:    Boolean;
    FMipGenMode:          TMipmapGenerationMode;
    FUseStreaming:        Boolean;
    FBaseLevel:           Integer;
    FMaxLevel:            Integer;
    FLastTime:            Double;
//      FUsePBO :                      Boolean;
//      pboRBId,pboWBId :              GLUInt;


//    function GetReadPBO: GLUint;
//    function GetWritePBO: GLUint;
    procedure SetInternalFormat(const AValue: TDGLInternalFormat);
    procedure SetImageAlpha(const AValue: TDGLTextureImageAlpha);
    procedure SetImageBrightness(const AValue: Single);
    function StoreBrightness: Boolean;
    procedure SetImageGamma(const AValue: Single);
    function StoreGamma: Boolean;
    procedure SetNormalMapScale(const AValue: Single);
    function StoreNormalMapScale: Boolean;
    procedure SetCompression(const AValue: TDGLTextureCompression);
    procedure SetSourceFile(AValue: string);
    procedure SetInternallyStored(const AValue: Boolean);
    procedure SetMipGenMode(const AValue: TMipmapGenerationMode);
    procedure SetUseStreaming(const AValue: Boolean);
    procedure PrepareImage;
    procedure FullTransfer;
    procedure StreamTransfer;
    procedure CalcLODRange(out AFirstLOD, ALastLOD: Integer);

//    procedure DoOnTextureNeeded(Sender: TObject; var textureFileName: string);
//    function StoreImageClassName: Boolean;
    //: Shows a special image that indicates an error
//    procedure SetTextureErrorImage;
//    function GetOpenGLHandle: TGLuint; virtual;
  public
    { Public Declarations }
    constructor Create(AOwner: TDGLXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure NotifyChange(Sender: TObject); override;

    procedure DoOnPrepare(Sender: TDGLContext); override;
    procedure Apply(var ARci: TRenderContextInfo); override;
    procedure UnApply(var ARci: TRenderContextInfo); override;

//    function  ReadFromPBO(pboId: GLUInt): boolean;
//    function  WriteToPBO(pboId: GLUInt): boolean;
//    procedure SetPixel(x,y: integer; color: TVector);
//    function  ReadPixel(x,y: integer): TVector;
//    procedure FreePBO;
//    procedure UploadData(Data: pointer; UsePBO: boolean=false);
//    function  DownloadData(var Datas: pointer; UsePBO: boolean=false): boolean;

    class function FriendlyName: string; override;

    property Image :  TDGLBaseImage read FImage write FImage;
//    property PBOReadBuffer: GLUint read GetReadPBO;
//    property PBOWriteBuffer: GLUint read GetWritePBO;
    property ApplyCounter : Integer read FApplyCounter Write FApplyCounter;
    property UseStreaming: Boolean read FUseStreaming write SetUseStreaming default False;
  published
    { Published Declarations }

    // Factual texture properties
    property InternalWidth:  Integer read FWidth;
    property InternalHeight: Integer read FHeight;
    property InternalDepth:  Integer read FDepth;
    property InternalFormat: TDGLInternalFormat read FInternalFormat write SetInternalFormat default tfRGBA8;
   
    { : Automatic Image Alpha setting.<p>
      Allows to control how and if the image's Alpha channel (transparency)
      is computed. }
    property ImageAlpha: TDGLTextureImageAlpha read FImageAlpha write SetImageAlpha default tiaDefault;
    { : Texture brightness correction.<p>
      This correction is applied upon loading a TDGLTextureImage, it's a
      simple saturating scaling applied to the RGB components of
      the 32 bits image, before it is passed to OpenGL, and before
      gamma correction (if any). }
    property ImageBrightness: Single read FImageBrightness write SetImageBrightness stored StoreBrightness;
    { : Texture gamma correction.<p>
      The gamma correction is applied upon loading a TDGLTextureImage,
      applied to the RGB components of the 32 bits image, before it is
      passed to OpenGL, after brightness correction (if any). }
    property ImageGamma: Single read FImageGamma write SetImageGamma stored StoreGamma;
    { : Texture compression control.<p>
      If True the compressed TextureFormat variant (the OpenGL ICD must
      support GL_ARB_texture_compression, or this option is ignored). }
    property Compression: TDGLTextureCompression read FCompression write SetCompression default tcDefault;
    { : Normal Map scaling.<p>
      Force normal map generation from height map and controls
      the intensity of the bumps. }
    property HeightToNormalScale: Single read FHeightToNormalScale write SetNormalMapScale stored StoreNormalMapScale;
    { : Source file path and name. }
    property SourceFile: string read FSourceFile write SetSourceFile;
    { : Force to store image levels in separate files in ready to transfer format. }
    property InternallyStored: Boolean read FInternallyStored write SetInternallyStored default False;
    { : Mipmap generation mode. }
    property MipGenMode: TMipmapGenerationMode read FMipGenMode write SetMipGenMode default mgmOnFly;
    { : Enable streaming loading. }

    { @HTML ( Native opengl texture target.<p> }
    property NativeTextureTarget: TDGLTextureTarget read GetTextureTarget;
  end;

  // ****************************************************************************************
  // TDGLFrameBufferAttachment
  //
  TDGLFrameBufferAttachment = class(TDGLAbstractTexture)
  protected
    { Protected Declarations }
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    { Private Declarations }
    FRenderBufferHandle:   TDGLRenderbufferHandle;
    FLayered:              Boolean;
    FCubeMap:              Boolean;
    FSamples:              Integer;
    FOnlyWrite:            Boolean;
    FFixedSamplesLocation: Boolean;
    procedure SetWidth(AValue: Integer);
    procedure SetHeight(AValue: Integer);
    procedure SetDepth(AValue: Integer);
    procedure SetInternalFormat(const AValue: TDGLInternalFormat);
    procedure SetOnlyWrite(AValue: Boolean);
    procedure SetLayered(AValue: Boolean);
    procedure SetCubeMap(AValue: Boolean);
    procedure SetSamples(AValue: Integer);
    procedure SetFixedSamplesLocation(AValue: Boolean);
  public
    { Public Declarations }
    constructor Create(AOwner: TDGLXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure NotifyChange(Sender: TObject); override;

    procedure DoOnPrepare(Sender: TDGLContext); override;
    procedure Apply(var ARci: TRenderContextInfo); override;
    procedure UnApply(var ARci: TRenderContextInfo); override;

    class function FriendlyName: string; override;
  published
    { Published Declarations }
    property InternalWidth:  Integer read FWidth write SetWidth default 256;
    property InternalHeight: Integer read FHeight write SetHeight default 256;
    property InternalDepth:  Integer read FDepth write SetDepth default 0;
    property InternalFormat: TDGLInternalFormat read FInternalFormat write SetInternalFormat default tfRGBA8;
    { : This flag makes use render buffer as target which makes
      it impossible to read it as texture, but improves efficiency. }
    property OnlyWrite: Boolean read FOnlyWrite write SetOnlyWrite default False;
    { : Force target be texture array. }
    property Layered: Boolean read FLayered write SetLayered default False;
    { : Force target be cube map. }
    property CubeMap: Boolean read FCubeMap write SetCubeMap default False;
    { : Number of samples. Positive value makes texture be multisample. }
    property Samples: Integer read FSamples write SetSamples default -1;
    { : FixedSamplesLocation flag makes image will use identical
      sample locations and the same number of samples for all texels in
      the image, and the sample locations will not depend on the
      internalformat or size of the image. }
    property FixedSamplesLocation: Boolean read FFixedSamplesLocation write SetFixedSamplesLocation default False;
  end;

  // ****************************************************************************************
  // TDGLTextureProperties
  //
  TDGLTextureProperties = class(TDGLLibMaterialProperty)
  private
    { Private Declarations }
    FLibTextureName:               TDGLMaterialComponentName;
    FLibSamplerName:               TDGLMaterialComponentName;
    FLibTexture:                   TDGLAbstractTexture;
    FLibSampler:                   TDGLTextureSampler;
//    FRequiredMemorySize: Integer;
    FTextureOffset, FTextureScale: TDGLCoordinates;
    FTextureRotate:                Single;
    FTextureMatrixIsIdentity:      Boolean;
    FTextureOverride:              Boolean;
    FTextureMatrix:                TMatrix;
    FMappingMode:                  TDGLTextureMappingMode;
    FEnvColor:                     TDGLColor;
    FMapSCoordinates:              TDGLCoordinates4;
    FMapTCoordinates:              TDGLCoordinates4;
    FMapRCoordinates:              TDGLCoordinates4;
    FMapQCoordinates:              TDGLCoordinates4;
    FSwizzling:                    TDGLTextureSwizzling;

    function GetLibTextureName: TDGLMaterialComponentName;
    function GetLibSamplerName: TDGLMaterialComponentName;
    procedure SetLibTextureName(const AValue: TDGLMaterialComponentName);
    procedure SetLibSamplerName(const AValue: TDGLMaterialComponentName);
    function GetTextureOffset: TDGLCoordinates;
    procedure SetTextureOffset(const AValue: TDGLCoordinates);
    function StoreTextureOffset: Boolean;
    function GetTextureScale: TDGLCoordinates;
    procedure SetTextureScale(const AValue: TDGLCoordinates);
    function StoreTextureScale: Boolean;
    procedure SetTextureMatrix(const AValue: TMatrix);
    procedure SetTextureRotate(AValue: Single);
    function StoreTextureRotate: Boolean;
    procedure SetMappingMode(const AValue: TDGLTextureMappingMode);
    function GetMappingSCoordinates: TDGLCoordinates4;
    procedure SetMappingSCoordinates(const AValue: TDGLCoordinates4);
    function StoreMappingSCoordinates: Boolean;
    function GetMappingTCoordinates: TDGLCoordinates4;
    procedure SetMappingTCoordinates(const AValue: TDGLCoordinates4);
    function StoreMappingTCoordinates: Boolean;
    function GetMappingRCoordinates: TDGLCoordinates4;
    procedure SetMappingRCoordinates(const AValue: TDGLCoordinates4);
    function StoreMappingRCoordinates: Boolean;
    function GetMappingQCoordinates: TDGLCoordinates4;
    procedure SetMappingQCoordinates(const AValue: TDGLCoordinates4);
    function StoreMappingQCoordinates: Boolean;
    procedure SetSwizzling(const AValue: TDGLTextureSwizzling);
    function StoreSwizzling: Boolean;
    procedure SetEnvColor(const AValue: TDGLColor);

    procedure CalculateTextureMatrix;
//    procedure ApplyMappingMode;
//    procedure UnApplyMappingMode;
  protected
    procedure Loaded; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure NotifyChange(Sender: TObject); override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;

    function IsValid: Boolean;
    procedure Apply(var ARci: TRenderContextInfo);
    procedure UnApply(var ARci: TRenderContextInfo);

    { @HTML ( Returns the OpenGL memory used by the texture.<p>
      The compressed size is returned if, and only if texture compression
      if active and possible, and the texture has been allocated (Handle
      is defined), otherwise the estimated size (from TextureFormat
      specification) is returned. }
//    function TextureImageRequiredMemory: Integer;

    property TextureMatrix: TMatrix read FTextureMatrix write SetTextureMatrix;
  published
    { Published Declarations }
    property TextureName: TDGLMaterialComponentName read GetLibTextureName write SetLibTextureName;
    property SamplerName: TDGLMaterialComponentName read GetLibSamplerName write SetLibSamplerName;
    property TextureOffset:  TDGLCoordinates read GetTextureOffset write SetTextureOffset stored StoreTextureOffset;
    { : Texture coordinates scaling.<p>
      Scaling is applied before applying the offset, and is applied
      to the texture coordinates, meaning that a scale factor of (2, 2, 2)
      will make your texture look twice <i>smaller</i>. }
    property TextureScale: TDGLCoordinates read GetTextureScale write SetTextureScale stored StoreTextureScale;
    { : Texture coordinates rotating.<p>
      Rotating is applied after applying offset and scale,
      and rotate ST direction around R axis. }
    property TextureRotate: Single read FTextureRotate write SetTextureRotate stored StoreTextureRotate;
    { : Texture Environment color. }
    property EnvColor: TDGLColor read FEnvColor write SetEnvColor;
    { : Texture coordinates mapping mode.<p>
      This property controls automatic texture coordinates generation. }
    property MappingMode: TDGLTextureMappingMode read FMappingMode write SetMappingMode default tmmUser;
    { : Texture mapping coordinates mode for S, T, R and Q axis.<p>
      This property stores the coordinates for automatic texture
      coordinates generation. }
    property MappingSCoordinates: TDGLCoordinates4 read GetMappingSCoordinates write SetMappingSCoordinates stored StoreMappingSCoordinates;
    property MappingTCoordinates: TDGLCoordinates4 read GetMappingTCoordinates write SetMappingTCoordinates stored StoreMappingTCoordinates;
    property MappingRCoordinates: TDGLCoordinates4 read GetMappingRCoordinates write SetMappingRCoordinates stored StoreMappingRCoordinates;
    property MappingQCoordinates: TDGLCoordinates4 read GetMappingQCoordinates write SetMappingQCoordinates stored StoreMappingQCoordinates;
    { : Texture color fetching parameters. }
    property Swizzling: TDGLTextureSwizzling read FSwizzling write SetSwizzling stored StoreSwizzling;
  end;

  // ****************************************************************************************
  // TDGLAdvancedProperties
  //
  TDGLAdvancedProperties = class(TDGLLibMaterialProperty)
  private
    { Private Declarations }
    FDiffuseLightMap : TDGLTextureProperties;
    FAmbientMap : TDGLTextureProperties;
    FSpecularMap : TDGLTextureProperties;
    FBumpMapHeight : TDGLTextureProperties;
    FBumpMapNormal : TDGLTextureProperties;
    FAlphaMap : TDGLTextureProperties;
   // FAlphaThresHold : Single;
    FReflectionMap : TDGLTextureProperties;
    FRefractionMap : TDGLTextureProperties;
    FDiffuseLightMode : TDGLDiffuseLightMode;
    FSpecularLightMode : TDGLSpecularLightMode;
    //FFresnel : Boolean; // RefactionMode : TDGLRefractionMode;
    //FBumpMapping :Boolean;

    function GetDiffuseLightMap:TDGLTextureProperties;
    function GetAmbientMap:TDGLTextureProperties;
    function GetSpecularMap:TDGLTextureProperties;
    function GetBumpMapHeight:TDGLTextureProperties;
    function GetBumpMapNormal:TDGLTextureProperties;
    function GetAlphaMap:TDGLTextureProperties;
    function GetReflectionMap:TDGLTextureProperties;
    function GetRefractionMap:TDGLTextureProperties;

    procedure SetDiffuseLightMap(AValue: TDGLTextureProperties);
    procedure SetAmbientMap(AValue: TDGLTextureProperties);
    procedure SetSpecularMap(AValue: TDGLTextureProperties);
    procedure SetBumpMapHeight(AValue: TDGLTextureProperties);
    procedure SetBumpMapNormal(AValue: TDGLTextureProperties);
    procedure SetAlphaMap(AValue: TDGLTextureProperties);
    procedure SetReflectionMap(AValue: TDGLTextureProperties);
    procedure SetRefractionMap(AValue: TDGLTextureProperties);
//    procedure SetTexProp(AValue: TDGLTextureProperties);
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    //procedure Apply(var ARci: TRenderContextInfo);
//    procedure UnApply(var ARci: TRenderContextInfo);

  published
    { Published Declarations }
    property DiffuseLightMap : TDGLTextureProperties read getDiffuseLightMap write setDiffuseLightMap;
    property AmbientMap : TDGLTextureProperties read getAmbientMap write setAmbientMap;
    property SpecularMap : TDGLTextureProperties read getSpecularMap write setSpecularMap;
    property BumpMapHeight : TDGLTextureProperties read getBumpMapHeight write setBumpMapHeight;
    property BumpMapNormal : TDGLTextureProperties read getBumpMapNormal write setBumpMapNormal;
    property AlphaMap : TDGLTextureProperties read getAlphaMap write setAlphaMap;
    property ReflectionMap : TDGLTextureProperties read getReflectionMap write setReflectionMap;
//    FReflectionCubeMap : TDGLTextureProperties;
    property RefractionMap : TDGLTextureProperties read getRefractionMap write setRefractionMap;
    property DiffuseLightMode : TDGLDiffuseLightMode read FDiffuseLightMode write FDiffuseLightMode;
    property SpecularLightMode : TDGLSpecularLightMode read FSpecularLightMode write FSpecularLightMode;

  end;

  // ****************************************************************************************
  // TDGLBaseMaterialProperties
  //
  TDGLBaseMaterialProperties = class(TDGLLibMaterialProperty)
  private
    { Private Declarations }
    FFaceProperties: TDGLFaceProperties;
    FAdvancedProperties:  TDGLAdvancedProperties;
    FDepthProperties: TDGLDepthProperties;
    FBlendingMode:    TBlendingMode;
    FBlendingParams:  TDGLBlendingParameters;
    FTexProp:         TDGLTextureProperties;
    //FMaterialOptions: TMaterialOptions;
    FFaceCulling:     TFaceCulling;
    FPolygonMode:     TPolygonMode;
    FTextureMode:     TDGLTextureMode;
//    function GetAdvancedProperties: TDGLAdvancedProperties;
    procedure SetAdvancedProperties(AValues: TDGLAdvancedProperties);
    procedure SetFaceProperties(AValues: TDGLFaceProperties);
    procedure SetDepthProperties(AValues: TDGLDepthProperties);
    procedure SetBlendingMode(const AValue: TBlendingMode);
    //procedure SetMaterialOptions(const AValue: TMaterialOptions);
    procedure SetFaceCulling(const AValue: TFaceCulling);
    procedure SetPolygonMode(AValue: TPolygonMode);
    procedure SetBlendingParams(const AValue: TDGLBlendingParameters);
    procedure SetTexProp(AValue: TDGLTextureProperties);
    procedure SetTextureMode(AValue: TDGLTextureMode);
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Apply(var ARci: TRenderContextInfo);
    procedure UnApply(var ARci: TRenderContextInfo);
    { : Returns True if the material is blended.<p> }
    function Blended: Boolean;

  published
    { Published Declarations }
    //property MaterialOptions: TMaterialOptions read FMaterialOptions write SetMaterialOptions default [];

    property AdvancedProperties:  TDGLAdvancedProperties read FAdvancedProperties write SetAdvancedProperties;
    property FaceProperties: TDGLFaceProperties read FFaceProperties write SetFaceProperties;
    property DepthProperties: TDGLDepthProperties read FDepthProperties write SetDepthProperties;
    property BlendingMode:    TBlendingMode read FBlendingMode write SetBlendingMode default bmOpaque;
    property BlendingParams:  TDGLBlendingParameters read FBlendingParams write SetBlendingParams;

    property FaceCulling: TFaceCulling read FFaceCulling write SetFaceCulling default fcBufferDefault;
    property PolygonMode: TPolygonMode read FPolygonMode write SetPolygonMode default pmFill;
    property Texture:     TDGLTextureProperties read FTexProp write SetTexProp;
    { : Texture application mode. }
    property TextureMode: TDGLTextureMode read FTextureMode write SetTextureMode default tmDecal;
    { : Next pass of FFP. }
    //property NextPass;
  end;

  // ****************************************************************************************
  // TGLMultitexturingProperties
  //
  TLightDir2TexEnvColor = (l2eNone, l2eEnvColor0, l2eEnvColor1, l2eEnvColor2, l2eEnvColor3);
  TDGLMultitexturingProperties = class(TDGLLibMaterialProperty)
  private
    FLibCombiner:      TDGLTextureCombiner;
    FLibCombinerName:  TDGLMaterialComponentName;
    FTexProps:         array [0 .. 3] of TDGLTextureProperties;
    FTextureMode:      TDGLTextureMode;
    FLightDir:         TLightDir2TexEnvColor;
    FLightSourceIndex: Integer;
    function GetLibCombinerName: string;
    procedure SetLibCombinerName(const AValue: string);
    function GetTexProps(AIndex: Integer): TDGLTextureProperties;
    procedure SetTexProps(AIndex: Integer; AValue: TDGLTextureProperties);
    procedure SetTextureMode(AValue: TDGLTextureMode);
    procedure SetLightSourceIndex(AValue: Integer);
  protected
    procedure Loaded; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Notification(Sender: TObject; Operation: TOperation); override;

    function IsValid: Boolean;
    procedure Apply(var ARci: TRenderContextInfo);
    procedure UnApply(var ARci: TRenderContextInfo);
  published
    { Published Declarations }
    property CombinerName: string read GetLibCombinerName write SetLibCombinerName;
    property Texture0:        TDGLTextureProperties index 0 read GetTexProps write SetTexProps;
    property Texture1:        TDGLTextureProperties index 1 read GetTexProps write SetTexProps;
    property Texture2:        TDGLTextureProperties index 2 read GetTexProps write SetTexProps;
    property Texture3:        TDGLTextureProperties index 3 read GetTexProps write SetTexProps;
    { : Texture application mode. }
    property TextureMode: TDGLTextureMode read FTextureMode write SetTextureMode default tmDecal;
    { : Pass light source direction to enviroment color of choosen texture.
      Vector in model space. }
    property LightDirTo: TLightDir2TexEnvColor read FLightDir write FLightDir default l2eNone;
    { : Specify index of light source for LightDirTo. }
    property LightSourceIndex: Integer read FLightSourceIndex write SetLightSourceIndex default 0;
    { : Next pass of combiner. }
//    property NextPass;
  end;

  // ****************************************************************************************
  // TDGLLibMaterial
  //
  TDGLLibMaterial = class(TDGLAbstractLibMaterial)
  private
    { Private Declarations }
    FHandle:              TDGLVirtualHandle;
//    FApplicableLevel:     TDGLMaterialLevel;
//    FSelectedLevel:       TDGLMaterialLevel;
    FBaseMaterial:           TDGLbaseMaterialProperties;
    FMultitexturing:      TDGLMultitexturingProperties;
    // FAdvancedMaterial : TDGLAdvancedMaterialProperties

    FStoreAmalgamating:   Boolean;

    procedure SetBaseMaterial(AValue: TDGLBaseMaterialProperties);
    procedure SetMultitexturing(AValue: TDGLMultitexturingProperties);

    procedure DoAllocate(Sender: TDGLVirtualHandle; var Handle: TGLUint);
    procedure DoDeallocate(Sender: TDGLVirtualHandle; var Handle: TGLUint);
  protected
    procedure Loaded; override;
    procedure RemoveDefferedInit;
    procedure DoOnPrepare(Sender: TDGLContext);
  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;

    procedure Apply(var ARci: TRenderContextInfo); override;
    function UnApply(var ARci: TRenderContextInfo): Boolean; override;

    function Blended: Boolean; override;
  published
    { Published Declarations }
//    property ApplicableLevel: TDGLMaterialLevel read FApplicableLevel write SetLevel default mlAuto;
//    property SelectedLevel:   TDGLMaterialLevel read FSelectedLevel;
    property BaseMaterial:   TDGLBaseMaterialProperties read FBaseMaterial write SetBaseMaterial;
    property Multitexturing:  TDGLMultitexturingProperties read FMultitexturing write SetMultitexturing;
    // property ShaderLibrary
    // property ShaderName

  end;

  // ****************************************************************************************
  // TDGLMatLibComponents
  //
  TDGLMatLibComponents = class(TDGLXCollection)
  protected
    { Protected Declarations }
    function GetItems(Index: Integer): TDGLBaseMaterialCollectionItem;
  public
    { Public Declarations }
    function GetNamePath: string; override;
    class function ItemsClass: TDGLXCollectionItemClass; override;
    property Items[index: Integer]: TDGLBaseMaterialCollectionItem read GetItems; default;

    function GetItemByName(const AName: TDGLMaterialComponentName): TDGLBaseMaterialCollectionItem;
    function GetTextureByName(const AName: TDGLMaterialComponentName): TDGLAbstractTexture;
    function GetAttachmentByName(const AName: TDGLMaterialComponentName): TDGLFrameBufferAttachment;
    function GetSamplerByName(const AName: TDGLMaterialComponentName): TDGLTextureSampler;
    function GetCombinerByName(const AName: TDGLMaterialComponentName): TDGLTextureCombiner;

    function MakeUniqueName(const AName: TDGLMaterialComponentName): TDGLMaterialComponentName;
  end;

  // ****************************************************************************************
  // TDGLMaterialLibrary
  //
  TDGLMaterialLibrary = class(TDGLAbstractMaterialLibrary)
  private
    { Private Declarations }
    FComponents: TDGLMatLibComponents;
  protected
    { Protected Declarations }
    procedure Loaded; override;
    function GetMaterials: TDGLLibMaterials;
    procedure SetMaterials(AValue: TDGLLibMaterials);
    function StoreMaterials: Boolean;
    procedure SetComponents(AValue: TDGLMatLibComponents);

    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteComponents(AStream: TStream);
    procedure ReadComponents(AStream: TStream);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetNames(Proc: TGetStrProc; AClass: CGLBaseMaterialCollectionItem); overload;

    function AddTexture(const AName: TDGLMaterialComponentName): TDGLTexture;
    function AddAttachment(const AName: TDGLMaterialComponentName): TDGLFrameBufferAttachment;
    function AddSampler(const AName: TDGLMaterialComponentName): TDGLTextureSampler;
    function AddCombiner(const AName: TDGLMaterialComponentName): TDGLTextureCombiner;
    // function AddLight(const AName: TDGLMaterialComponentName): TDGLLight; ????

//    procedure SetLevelForAll(const ALevel: TDGLMaterialLevel);
  published
    { Published Declarations }
    { : The materials collection. }
    property Materials:  TDGLLibMaterials read GetMaterials write SetMaterials stored StoreMaterials;
    property Components: TDGLMatLibComponents read FComponents write SetComponents;
    property TexturePaths;
  end;

  TDGLMaterial = TDGLLibMaterial;

// ****************************************************************************************

function CreateGraphicFromFile(const fileName: string): TDGLGraphic;

procedure RegisterGLMaterialNameChangeEvent(AEvent: TNotifyEvent);
procedure DeRegisterGLMaterialNameChangeEvent(AEvent: TNotifyEvent);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses Math,
     DGLPictureRegisteredFormats;

type
  TFriendlyImage = class(TDGLBaseImage);

var
  vGLMaterialNameChangeEvent:    TNotifyEvent;
  vTGraphicFileExtension: array of string;
  vTGraphicClass: array of TGraphicClass;

// ------------------
{ Helpers Functions }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

procedure RegisterGLMaterialNameChangeEvent(AEvent: TNotifyEvent);
begin
  vGLMaterialNameChangeEvent := AEvent;
end;

procedure DeRegisterGLMaterialNameChangeEvent(AEvent: TNotifyEvent);
begin
  vGLMaterialNameChangeEvent := nil;
end;



procedure Div2(var Value: Integer);
{$IFDEF GLS_INLINE} inline; {$ENDIF}
begin
  Value := Value div 2;
  if Value = 0 then
    Value := 1;
end;

function CalcTextureLevelNumber(ATarget: TDGLTextureTarget; w, h, d: Integer): Integer;
begin
  Result := 0;

  case ATarget of

    ttNoShape:
      ;

    ttTexture1D, ttTexture1DArray, ttTextureCube, ttTextureCubeArray:
      repeat
        Inc(Result);
        Div2(w);
      until w <= 1;

      ttTexture2D, ttTexture2DArray: repeat Inc(Result);
    Div2(w);
    Div2(h);
    until (w <= 1) and (h <= 1);

    ttTexture3D:
      repeat
        Inc(Result);
        Div2(w);
        Div2(h);
        Div2(d);
      until (w <= 1) and (h <= 1) and (d <= 1);

      ttTextureRect, ttTextureBuffer, ttTexture2DMultisample, ttTexture2DMultisampleArray: Result := 1;
  end;
end;


  // RegisterTGraphicClassFileExtension
  //

//procedure RegisterTGraphicClassFileExtension(const extension: string; const aClass: TGraphicClass);
//var
//  n: Integer;
//begin
//  n := Length(vTGraphicFileExtension);
//  SetLength(vTGraphicFileExtension, n + 1);
//  SetLength(vTGraphicClass, n + 1);
//  vTGraphicFileExtension[n] := LowerCase(extension);
//  vTGraphicClass[n] := aClass;
//end;
//
// CreateGraphicFromFile
//

function CreateGraphicFromFile(const fileName: string): TDGLGraphic;
var
  i: Integer;
  ext: string;
  fs: TStream;
  graphicClass: TGraphicClass;
begin
  Result := nil;
  if FileStreamExists(fileName) then
  begin
    graphicClass := nil;
    ext := LowerCase(ExtractFileExt(fileName));
    for i := 0 to High(vTGraphicFileExtension) do
    begin
      if vTGraphicFileExtension[i] = ext then
      begin
        graphicClass := TGraphicClass(vTGraphicClass[i]);
        Break;
      end;
    end;
    if graphicClass = nil then
      graphicClass := GraphicClassForExtension(ext);
    if graphicClass <> nil then
    begin
      Result := graphicClass.Create;
      try
        fs := CreateFileStream(fileName, fmOpenRead);
        try
          Result.LoadFromStream(fs);
        finally
          fs.Free;
        end;
      except
        FreeAndNil(Result);
        raise;
      end;
    end;
  end;
end;
//
//// RegisterGLTextureImageClass
////
//
//procedure RegisterGLTextureImageClass(textureImageClass: TDGLTextureImageClass);
//begin
//  if not Assigned(vGLTextureImageClasses) then
//    vGLTextureImageClasses := TList.Create;
//  vGLTextureImageClasses.Add(textureImageClass);
//end;
//
//// FindGLTextureImageClass
////
//
//function FindGLTextureImageClass(const className: string): TDGLTextureImageClass;
//var
//  i: Integer;
//  tic: TDGLTextureImageClass;
//begin
//  Result := nil;
//  if Assigned(vGLTextureImageClasses) then
//    for i := 0 to vGLTextureImageClasses.Count - 1 do
//    begin
//      tic := TDGLTextureImageClass(vGLTextureImageClasses[i]);
//      if tic.ClassName = className then
//      begin
//        Result := tic;
//        Break;
//      end;
//    end;
//
//end;
//
//// FindGLTextureImageClassByFriendlyName
////
//
//function FindGLTextureImageClassByFriendlyName(const friendlyName: string): TDGLTextureImageClass;
//var
//  i: Integer;
//  tic: TDGLTextureImageClass;
//begin
//  Result := nil;
//  if Assigned(vGLTextureImageClasses) then
//    for i := 0 to vGLTextureImageClasses.Count - 1 do
//    begin
//      tic := TDGLTextureImageClass(vGLTextureImageClasses[i]);
//      if tic.FriendlyName = friendlyName then
//      begin
//        Result := tic;
//        Break;
//      end;
//    end;
//end;
//
//// SeTDGLTextureImageClassesToStrings
////
//
//procedure SeTDGLTextureImageClassesToStrings(aStrings: TStrings);
//var
//  i: Integer;
//  tic: TDGLTextureImageClass;
//begin
//  with aStrings do
//  begin
//    BeginUpdate;
//    Clear;
//    if Assigned(vGLTextureImageClasses) then
//      for i := 0 to vGLTextureImageClasses.Count - 1 do
//      begin
//        tic := TDGLTextureImageClass(vGLTextureImageClasses[i]);
//        AddObject(tic.FriendlyName, TObject(Pointer(tic)));
//      end;
//    EndUpdate;
//  end;
//end;
//
//// GeTDGLTextureImageClassesAsStrings
////
//
//function GeTDGLTextureImageClassesAsStrings: TStrings;
//begin
//  Result := TStringList.Create;
//  SeTDGLTextureImageClassesToStrings(Result);
//end;


{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLAbstractLibMaterial }
{$IFDEF GLS_REGION}{$REGION 'TDGLAbstractLibMaterial'}{$ENDIF}
constructor TDGLAbstractLibMaterial.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FUserList := TList.Create;
  if Assigned(ACollection) then
  begin
    FName := TDGLAbstractLibMaterials(ACollection).MakeUniqueName('LibMaterial');
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

destructor TDGLAbstractLibMaterial.Destroy;
begin
  FUserList.Free;
  inherited Destroy;
end;

procedure TDGLAbstractLibMaterial.Assign(Source: TPersistent);
begin
  if Source is TDGLAbstractLibMaterial then
  begin
    FName :=
      TDGLLibMaterials(Collection).MakeUniqueName(TDGLLibMaterial(Source).Name);
    FNameHashKey := ComputeNameHashKey(FName);
  end
  else
    inherited; // Raise AssignError
end;


function TDGLAbstractLibMaterial.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;


function TDGLAbstractLibMaterial._AddRef: Integer; stdcall;
begin
  Result := -1; //ignore
end;

function TDGLAbstractLibMaterial._Release: Integer; stdcall;
begin
  Result := -1; //ignore
end;

procedure TDGLAbstractLibMaterial.RegisterUser(obj: TDGLUpdateAbleObject);
begin
  Assert(FUserList.IndexOf(obj) < 0);
  FUserList.Add(obj);
end;

procedure TDGLAbstractLibMaterial.UnRegisterUser(obj: TDGLUpdateAbleObject);
begin
  FUserList.Remove(obj);
end;

procedure TDGLAbstractLibMaterial.RegisterUser(comp: TDGLUpdateAbleComponent);
begin
  Assert(FUserList.IndexOf(comp) < 0);
  FUserList.Add(comp);
end;

procedure TDGLAbstractLibMaterial.UnRegisterUser(comp: TDGLUpdateAbleComponent);
begin
  FUserList.Remove(comp);
end;

procedure TDGLAbstractLibMaterial.RegisterUser(libMaterial: TDGLLibMaterial);
begin
  Assert(FUserList.IndexOf(libMaterial) < 0);
  FUserList.Add(libMaterial);
end;

procedure TDGLAbstractLibMaterial.UnRegisterUser(libMaterial: TDGLLibMaterial);
begin
  FUserList.Remove(libMaterial);
end;

procedure TDGLAbstractLibMaterial.NotifyChange(Sender: TObject);
begin
  NotifyUsers();
end;

procedure TDGLAbstractLibMaterial.NotifyUsers;
var
  i: Integer;
  obj: TObject;
begin
  if FNotifying then
    Exit;
  FNotifying := True;
  try
    for i := 0 to FUserList.Count - 1 do
    begin
      obj := TObject(FUserList[i]);
      if obj is TDGLUpdateAbleObject then
        TDGLUpdateAbleObject(FUserList[i]).NotifyChange(Self)
      else if obj is TDGLUpdateAbleComponent then
        TDGLUpdateAbleComponent(FUserList[i]).NotifyChange(Self)
      else
      begin
        Assert(obj is TDGLAbstractLibMaterial);
        TDGLAbstractLibMaterial(FUserList[i]).NotifyUsers;
      end;
    end;
  finally
    FNotifying := False;
  end;
end;

function TDGLAbstractLibMaterial.IsUsed: Boolean;
begin
  Result := Assigned(Self) and (FUserlist.Count > 0);
end;

function TDGLAbstractLibMaterial.GetDisplayName: string;
begin
  Result := Name;
end;

function TDGLAbstractLibMaterial.GetMaterialLibrary: TDGLAbstractMaterialLibrary;
var
  LOwner: TPersistent;
begin
  Result := nil;
  if Assigned(Collection) then
  begin
    LOwner := TDGLAbstractLibMaterials(Collection).Owner;
    if LOwner is TDGLAbstractMaterialLibrary then
      Result := TDGLAbstractMaterialLibrary(LOwner);
  end;
end;

function TDGLAbstractLibMaterial.Blended: Boolean;
begin
  Result := False;
end;

class function TDGLAbstractLibMaterial.ComputeNameHashKey(const name: string): Integer;
var
  i, n: Integer;
begin
  n := Length(name);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(name[i]);
end;

procedure TDGLAbstractLibMaterial.SetName(const val: TDGLLibMaterialName);
begin
  if val <> FName then
  begin
    if not (csLoading in TComponent(Collection.Owner).ComponentState) then
    begin
      if TDGLLibMaterials(Collection).GetLibMaterialByName(val) <> Self then
        FName := TDGLLibMaterials(Collection).MakeUniqueName(val)
      else
        FName := val;
    end
    else
      FName := val;
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

procedure TDGLAbstractLibMaterial.Loaded;
begin
end;

procedure TDGLAbstractLibMaterial.Apply(var ARci: TRenderContextInfo);
begin
end;

function TDGLAbstractLibMaterial.UnApply(var ARci: TRenderContextInfo): Boolean;
begin
  Result := True;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLAbstractLibMaterials}
{$IFDEF GLS_REGION}{$REGION 'TDGLAbstractLibMaterials'}{$ENDIF}
function TDGLAbstractLibMaterials.GetMaterial(const AName: TDGLLibMaterialName):TDGLAbstractLibMaterial;
var
  i, hk: Integer;
  lm: TDGLAbstractLibMaterial;
begin
  hk := TDGLAbstractLibMaterial.ComputeNameHashKey(AName);
  for i := 0 to Count - 1 do
  begin
    lm := TDGLAbstractLibMaterial(inherited Items[i]);
    if (lm.NameHashKey = hk) and (lm.Name = AName) then
    begin
      Result := lm;
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TDGLAbstractLibMaterials.Loaded;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    TDGLAbstractLibMaterial(Items[I]).Loaded;
end;

function TDGLAbstractLibMaterials.MakeUniqueName(const nameRoot: TDGLLibMaterialName): TDGLLibMaterialName;
var
  i: Integer;
begin
  Result := nameRoot;
  i := 1;
  while GetMaterial(Result) <> nil do
  begin
    Result := nameRoot + IntToStr(i);
    Inc(i);
  end;
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLAbstractMaterialLibrary }
{$IFDEF GLS_REGION}{$REGION 'TDGLAbstractMaterialLibrary'}{$ENDIF}

procedure TDGLAbstractMaterialLibrary.SetTexturePaths(const val: string);
var
  i, lp: Integer;

  procedure AddCurrent;
  var
    buf: string;
  begin
    buf := Trim(Copy(val, lp + 1, i - lp - 1));
    if Length(buf) > 0 then
    begin
      // make sure '\' is the terminator
      buf := IncludeTrailingPathDelimiter(buf);
      FTexturePathList.Add(buf);
    end;
  end;

begin
  FTexturePathList.Free;
  FTexturePathList := nil;
  FTexturePaths := val;
  if val <> '' then
  begin
    FTexturePathList := TStringList.Create;
    lp := 0;
    for i := 1 to Length(val) do
    begin
      if val[i] = ';' then
      begin
        AddCurrent;
        lp := i;
      end;
    end;
    i := Length(val) + 1;
    AddCurrent;
  end;
end;

function TDGLAbstractMaterialLibrary.ApplyMaterial(const AName: string;
  var ARci: TRenderContextInfo): Boolean;
begin
  FLastAppliedMaterial := FMaterials.GetMaterial(AName);
  Result := Assigned(FLastAppliedMaterial);
  if Result then
    FLastAppliedMaterial.Apply(ARci);
end;

function TDGLAbstractMaterialLibrary.UnApplyMaterial(
  var ARci: TRenderContextInfo): Boolean;
begin
  if Assigned(FLastAppliedMaterial) then
  begin
    Result := FLastAppliedMaterial.UnApply(ARci);
    if not Result then
      FLastAppliedMaterial := nil;
  end
  else
    Result := False;
end;

procedure TDGLAbstractMaterialLibrary.SetNamesToTStrings(AStrings: TStrings);
var
  i: Integer;
  lm: TDGLAbstractLibMaterial;
begin
  with AStrings do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to FMaterials.Count - 1 do
    begin
      lm := TDGLAbstractLibMaterial(FMaterials.Items[i]);
      AddObject(lm.Name, lm);
    end;
    EndUpdate;
  end;
end;

procedure TDGLAbstractMaterialLibrary.Loaded;
begin
  inherited;
  FMaterials.Loaded;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLLibMaterials }
{$IFDEF GLS_REGION}{$REGION 'TDGLLibMaterials'}{$ENDIF}

function TDGLLibMaterials.Add: TDGLLibMaterial;
begin
  Result := (inherited Add) as TDGLLibMaterial;
end;

constructor TDGLLibMaterials.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TDGLLibMaterial);
end;

function TDGLLibMaterials.FindItemID(ID: Integer): TDGLLibMaterial;
begin
  Result := (inherited FindItemID(ID)) as TDGLLibMaterial;
end;

function TDGLLibMaterials.GetItems(AIndex: Integer): TDGLLibMaterial;
begin
  Result := TDGLLibMaterial(inherited Items[AIndex]);
end;

function TDGLLibMaterials.GetLibMaterialByName(const AName: string): TDGLLibMaterial;
var
  LMaterial: TDGLAbstractLibMaterial;
begin
  LMaterial := GetMaterial(AName);
  if Assigned(LMaterial) and (LMaterial is TDGLLibMaterial) then
    Result := TDGLLibMaterial(LMaterial)
  else
    Result := nil;
end;

function TDGLLibMaterials.IndexOf(const Item: TDGLLibMaterial): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Count <> 0 then
    for i := 0 to Count - 1 do
      if GetItems(i) = Item then
      begin
        Result := i;
        exit;
      end;
end;

function TDGLLibMaterials.MaterialLibrary: TDGLMaterialLibrary;
begin
  Result := TDGLMaterialLibrary(GetOwner);
end;

procedure TDGLLibMaterials.SetItems(AIndex: Integer; const AValue: TDGLLibMaterial);
begin
  inherited Items[AIndex] := AValue;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLFaceProperties }
{$IFDEF GLS_REGION}{$REGION 'TDGLFaceProperties'}{$ENDIF}
constructor TDGLFaceProperties.Create(aOwner: TPersistent);
begin
  inherited;
  // OpenGL default colors
  FAmbient := TDGLColor.CreateInitialized(Self, clrGray20);
  FDiffuse := TDGLColor.CreateInitialized(Self, clrGray80);
  FEmission := TDGLColor.Create(Self);
  FSpecular := TDGLColor.Create(Self);
  FShininess := 0;
end;

// Destroy
//
destructor TDGLFaceProperties.Destroy;
begin
  FAmbient.Free;
  FDiffuse.Free;
  FEmission.Free;
  FSpecular.Free;
  inherited Destroy;
end;

// Apply
//
procedure TDGLFaceProperties.Apply(var rci: TRenderContextInfo; aFace: TCullFaceMode);
begin
  with rci.GLStates do
  begin
    SetGLMaterialColors(aFace, Emission.Color, Ambient.Color, Diffuse.Color, Specular.Color, FShininess);
  end;
end;

// ApplyNoLighting
//
procedure TDGLFaceProperties.ApplyNoLighting(var rci: TRenderContextInfo; aFace: TCullFaceMode);
begin
//  GL.Color4fv(Diffuse.AsAddress);
end;

// Assign
//
procedure TDGLFaceProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TDGLFaceProperties) then
  begin
    FAmbient.DirectColor := TDGLFaceProperties(Source).Ambient.Color;
    FDiffuse.DirectColor := TDGLFaceProperties(Source).Diffuse.Color;
    FEmission.DirectColor := TDGLFaceProperties(Source).Emission.Color;
    FSpecular.DirectColor := TDGLFaceProperties(Source).Specular.Color;
    FShininess := TDGLFaceProperties(Source).Shininess;
    NotifyChange(Self);
  end;
end;

// SetAmbient
//

procedure TDGLFaceProperties.SetAmbient(AValue: TDGLColor);
begin
  FAmbient.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

// SetDiffuse
//

procedure TDGLFaceProperties.SetDiffuse(AValue: TDGLColor);
begin
  FDiffuse.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

// SetEmission
//

procedure TDGLFaceProperties.SetEmission(AValue: TDGLColor);
begin
  FEmission.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

// SetSpecular
//

procedure TDGLFaceProperties.SetSpecular(AValue: TDGLColor);
begin
  FSpecular.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

// SetShininess
//

procedure TDGLFaceProperties.SetShininess(AValue: TShininess);
begin
  if FShininess <> AValue then
  begin
    FShininess := AValue;
    NotifyChange(Self);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLDepthProperties }
{$IFDEF GLS_REGION}{$REGION 'TGLDepthProperties'}{$ENDIF}

constructor TDGLDepthProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDepthTest := True;
  FDepthWrite := True;
  FZNear := 0;
  FZFar := 1;
  FCompareFunc := cfLequal;
  FDepthClamp := False;
end;

procedure TDGLDepthProperties.Apply(var rci: TRenderContextInfo);
begin
  with rci.GLStates do
  begin
    if FDepthTest and rci.bufferDepthTest then
      Enable(stDepthTest)
    else
      Disable(stDepthTest);
    DepthWriteMask := FDepthWrite;
    DepthFunc := FCompareFunc;
    SetDepthRange(FZNear, FZFar);
    if dglCheckExtension('ARB_depth_clamp') then
      if FDepthClamp then
        Enable(stDepthClamp)
      else
        Disable(stDepthClamp);
  end;
end;

procedure TDGLDepthProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TDGLDepthProperties) then
  begin
    FDepthTest := TDGLDepthProperties(Source).FDepthTest;
    FDepthWrite := TDGLDepthProperties(Source).FDepthWrite;
    FZNear := TDGLDepthProperties(Source).FZNear;
    FZFar := TDGLDepthProperties(Source).FZFar;
    FCompareFunc := TDGLDepthProperties(Source).FCompareFunc;
    NotifyChange(Self);
  end;
end;

procedure TDGLDepthProperties.SetZNear(Value: Single);
begin
  Value := ClampValue(Value, 0, 1);
  if Value <> FZNear then
  begin
    FZNear := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLDepthProperties.SetZFar(Value: Single);
begin
  Value := ClampValue(Value, 0, 1);
  if Value <> FZFar then
  begin
    FZFar := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLDepthProperties.SetCompareFunc(Value: TDepthFunction);
begin
  if Value <> FCompareFunc then
  begin
    FCompareFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLDepthProperties.SetDepthTest(Value: boolean);
begin
  if Value <> FDepthTest then
  begin
    FDepthTest := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLDepthProperties.SetDepthWrite(Value: boolean);
begin
  if Value <> FDepthWrite then
  begin
    FDepthWrite := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLDepthProperties.SetDepthClamp(Value: boolean);
begin
  if Value <> FDepthClamp then
  begin
    FDepthClamp := Value;
    NotifyChange(Self);
  end;
end;

function TDGLDepthProperties.StoreZNear: Boolean;
begin
  Result := FZNear <> 0.0;
end;

function TDGLDepthProperties.StoreZFar: Boolean;
begin
  Result := FZFar <> 1.0;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBlendingParameters }
{$IFDEF GLS_REGION}{$REGION 'TDGLBlendingParameters'}{$ENDIF}

procedure TDGLBlendingParameters.Apply(var rci: TRenderContextInfo);
begin
//  if FUseAlphaFunc then
//  begin
//    //rci.GLStates.Enable(stAlphaTest);
//    rci.GLStates.SetGLAlphaFunction(FAlphaFuncType, FAlphaFuncRef);
//  end
//  else
//    rci.GLStates.Disable(stAlphaTest);
  if FUseBlendFunc then
  begin
    rci.GLStates.Enable(stBlend);
    if FSeparateBlendFunc then
      rci.GLStates.SetBlendFuncSeparate(FBlendFuncSFactor, FBlendFuncDFactor,
        FAlphaBlendFuncSFactor, FAlphaBlendFuncDFactor)
    else
      rci.GLStates.SetBlendFunc(FBlendFuncSFactor, FBlendFuncDFactor);
  end
  else
    rci.GLStates.Disable(stBlend);
end;

constructor TDGLBlendingParameters.Create(AOwner: TPersistent);
begin
  inherited;
//  FUseAlphaFunc := False;
//  FAlphaFuncType := cfGreater;
//  FAlphaFuncRef := 0;

  FUseBlendFunc := True;
  FSeparateBlendFunc := False;
  FBlendFuncSFactor := bfSrcAlpha;
  FBlendFuncDFactor := bfOneMinusSrcAlpha;
  FAlphaBlendFuncSFactor := bfSrcAlpha;
  FAlphaBlendFuncDFactor := bfOneMinusSrcAlpha;
end;

//procedure TDGLBlendingParameters.SetAlphaFuncRef(const Value: TGLclampf);
//begin
//  if (FAlphaFuncRef <> Value) then
//  begin
//    FAlphaFuncRef := Value;
//    NotifyChange(Self);
//  end;
//end;

//procedure TDGLBlendingParameters.SetAlphaFuncType( const Value: TGlAlphaFunc);
//begin
//  if (FAlphaFuncType <> Value) then
//  begin
//    FAlphaFuncType := Value;
//    NotifyChange(Self);
//  end;
//end;

procedure TDGLBlendingParameters.SetBlendFuncDFactor( const Value: TBlendFunction);
begin
  if (FBlendFuncDFactor <> Value) then
  begin
    FBlendFuncDFactor := Value;
    if not FSeparateBlendFunc then
      FAlphaBlendFuncDFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLBlendingParameters.SetBlendFuncSFactor( const Value: TBlendFunction);
begin
  if (FBlendFuncSFactor <> Value) then
  begin
    FBlendFuncSFactor := Value;
    if not FSeparateBlendFunc then
      FAlphaBlendFuncSFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLBlendingParameters.SetAlphaBlendFuncDFactor(const Value: TBlendFunction);
begin
  if FSeparateBlendFunc and (FAlphaBlendFuncDFactor <> Value) then
  begin
    FAlphaBlendFuncDFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLBlendingParameters.SetAlphaBlendFuncSFactor(const Value: TBlendFunction);
begin
  if FSeparateBlendFunc and (FAlphaBlendFuncSFactor <> Value) then
  begin
    FAlphaBlendFuncSFactor := Value;
    NotifyChange(Self);
  end;
end;

//procedure TDGLBlendingParameters.SetUseAlphaFunc(const Value: Boolean);
//begin
//  if (FUseAlphaFunc <> Value) then
//  begin
//    FUseAlphaFunc := Value;
//    NotifyChange(Self);
//  end;
//end;

procedure TDGLBlendingParameters.SetUseBlendFunc(const Value: Boolean);
begin
  if (FUseBlendFunc <> Value) then
  begin
    FUseBlendFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLBlendingParameters.SetSeparateBlendFunc(const Value: Boolean);
begin
  if (FSeparateBlendFunc <> Value) then
  begin
    FSeparateBlendFunc := Value;
    if not Value then
    begin
      FAlphaBlendFuncSFactor := FBlendFuncSFactor;
      FAlphaBlendFuncDFactor := FBlendFuncDFactor;
    end;
    NotifyChange(Self);
  end;
end;

//function TDGLBlendingParameters.StoreAlphaFuncRef: Boolean;
//begin
////  Result := (Abs(AlphaFuncRef) > 0.001);
//end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLTextureSwizzling }
{$IFDEF GLS_REGION}{$REGION 'TDGLTextureSwizzling'}{$ENDIF}

procedure TDGLTextureSwizzling.Assign(Source: TPersistent);
var
  LSwizzling: TDGLTextureSwizzling;
begin
  if Source is TDGLTextureSwizzling then
  begin
    LSwizzling   := TDGLTextureSwizzling(Source);
    FSwizzles := LSwizzling.FSwizzles; //[0];
//    FSwizzles := LSwizzling.FSwizzles.g;
//    FSwizzles := LSwizzling.FSwizzles.b;
//    FSwizzles := LSwizzling.FSwizzles.a;
  end;
  inherited;
end;

constructor TDGLTextureSwizzling.Create(AOwner: TPersistent);
begin
  inherited;
  FSwizzles := cDefaultSwizzleVector;
end;

function TDGLTextureSwizzling.GetSwizzle(AIndex: Integer): TDGLTextureSwizzle;
begin
  Result := FSwizzles.r;
  case AIndex of
    0: Result := FSwizzles.r;
    1: Result := FSwizzles.g;
    2: Result := FSwizzles.b;
    3: Result := FSwizzles.a;
  end;

end;

procedure TDGLTextureSwizzling.ReadFromFiler(AReader: TReader);
begin
  with AReader do
  begin
    ReadInteger;
    FSwizzles.r:= TDGLTextureSwizzle(ReadInteger);
    FSwizzles.g := TDGLTextureSwizzle(ReadInteger);
    FSwizzles.b := TDGLTextureSwizzle(ReadInteger);
    FSwizzles.a := TDGLTextureSwizzle(ReadInteger);
  end;
end;

procedure TDGLTextureSwizzling.SetSwizzle(AIndex: Integer; AValue: TDGLTextureSwizzle);
begin
  case AIndex of
    0: if AValue <> FSwizzles.r then FSwizzles.r:=AValue;
    1: if AValue <> FSwizzles.g then FSwizzles.g:=AValue;
    2: if AValue <> FSwizzles.b then FSwizzles.b:=AValue;
    3: if AValue <> FSwizzles.b then FSwizzles.a:=AValue;
  end;
    NotifyChange(Self);
end;

function TDGLTextureSwizzling.StoreSwizzle(AIndex: Integer): Boolean;
begin
  Result := false;
  case AIndex of
    0: Result := (FSwizzles.r <> cDefaultSwizzleVector.r);
    1: Result := (FSwizzles.g <> cDefaultSwizzleVector.g);
    2: Result := (FSwizzles.b <> cDefaultSwizzleVector.b);
    3: Result := (FSwizzles.a <> cDefaultSwizzleVector.a);
  end;
end;

procedure TDGLTextureSwizzling.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0);
    WriteInteger(Integer(FSwizzles.r));
    WriteInteger(Integer(FSwizzles.g));
    WriteInteger(Integer(FSwizzles.b));
    WriteInteger(Integer(FSwizzles.a));
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBaseMaterialCollectionItem }
{$IFDEF GLS_REGION}{$REGION 'TDGLBaseMaterialCollectionItem'}{$ENDIF}

destructor TDGLBaseMaterialCollectionItem.Destroy;
var
  i: Integer;
begin
  if Assigned(FUserList) then
  begin
    FNotifying := True;
    for i      := FUserList.Count - 1 downto 0 do
      TDGLLibMaterialProperty(FUserList[i]).Notification(Self, opRemove);
    FreeAndNil(FUserList);
  end;
  inherited;
end;

function TDGLBaseMaterialCollectionItem.GetMaterialLib: TDGLMaterialLibrary;
begin
  Result := TDGLMaterialLibrary(TDGLMatLibComponents(Owner).Owner);
end;

function TDGLBaseMaterialCollectionItem.GetMaterialLibrary: TDGLAbstractMaterialLibrary;
begin
  Result := TDGLAbstractMaterialLibrary(TDGLMatLibComponents(Owner).Owner);
end;

function TDGLBaseMaterialCollectionItem.GetUserCount: Integer;
begin
  if Assigned(FUserList) then
    Result := FUserList.Count
  else
    Result := 0;
end;

function TDGLBaseMaterialCollectionItem.GetUserList: TDGLPersistentObjectList;
begin
  if FUserList = nil then
  begin
    FUserList  := TDGLPersistentObjectList.Create;
    FNotifying := False;
  end;
  Result := FUserList;
end;

procedure TDGLBaseMaterialCollectionItem.NotifyChange(Sender: TObject);
var
  i: Integer;
begin
  if FNotifying then
    exit;
  FNotifying := True;
  if GetUserCount > 0 then
    for i := 0 to FUserList.Count - 1 do
      TDGLUpdateAbleObject(FUserList[i]).NotifyChange(Self);
  FNotifying := False;
end;

procedure TDGLBaseMaterialCollectionItem.RegisterUser(AUser: TDGLUpdateAbleObject);
begin
  if not FNotifying and (UserList.IndexOf(AUser) < 0) then
    UserList.Add(AUser);
end;

procedure TDGLBaseMaterialCollectionItem.UnregisterUser(AUser: TDGLUpdateAbleObject);
begin
  if not FNotifying then
    UserList.Remove(AUser);
end;

procedure TDGLBaseMaterialCollectionItem.SetName(const AValue: string);
begin
  if AValue <> Name then
  begin
    if not IsValidIdent(AValue) then
    begin
      if IsDesignTime then
        InformationDlg(AValue + ' - is not valid component name');
      exit;
    end;
    if not(csLoading in MaterialLibrary.ComponentState) then
    begin
      if TDGLMatLibComponents(Owner).GetItemByName(AValue) <> Self then
        inherited SetName(TDGLMatLibComponents(Owner).MakeUniqueName(AValue))
      else
        inherited SetName(AValue);
    end
    else
      inherited SetName(AValue);
    FNameHashKey := ComputeNameHashKey(Name);
    // Notify users
    NotifyChange(Self);
    // Notify designer
    if Assigned(vGLMaterialNameChangeEvent) then
      vGLMaterialNameChangeEvent(Self);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLLibMaterialProperty }
{$IFDEF GLS_REGION}{$REGION 'TDGLLibMaterialProperty'}{$ENDIF}

function TDGLLibMaterialProperty.GetMaterial: TDGLLibMaterial;
begin
  if Owner is TDGLLibMaterial then
    Result := TDGLLibMaterial(Owner)
  else if Owner is TDGLLibMaterialProperty then
    Result := TDGLLibMaterialProperty(Owner).GetMaterial
  else
    Result := nil;
end;

function TDGLLibMaterialProperty.GetMaterialLib: TDGLMaterialLibrary;
begin
  if Owner is TDGLBaseMaterialCollectionItem then
    Result := TDGLMaterialLibrary( TDGLBaseMaterialCollectionItem(Owner).GetMaterialLibrary)
  else
    Result := TDGLMaterialLibrary(GetMaterial.GetMaterialLibrary);
end;

function TDGLLibMaterialProperty.GetMaterialLibrary: TDGLAbstractMaterialLibrary;
begin
  if Owner is TDGLBaseMaterialCollectionItem then
    Result := TDGLBaseMaterialCollectionItem(Owner).GetMaterialLibrary
  else
    Result := TDGLAbstractMaterialLibrary(GetMaterial.GetMaterialLibrary);
end;

//procedure TDGLLibMaterialProperty.SetNextPass(const AValue: TDGLLibMaterialName);
//begin
//  if AValue <> FNextPassName then
//  begin
//    FNextPassName := AValue;
//    NotifyChange(Self);
//  end;
//end;

procedure TDGLLibMaterialProperty.Loaded;
begin
end;

procedure TDGLLibMaterialProperty.NotifyChange(Sender: TObject);
var
  NA: IDGLNotifyAble;
begin
  if Assigned(Owner) then
  begin
    if Supports(Owner, IDGLNotifyAble, NA) then
      NA.NotifyChange(Self)
  end;
  if Assigned(OnNotifyChange) then
    OnNotifyChange(Self);
end;

procedure TDGLLibMaterialProperty.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if Owner is TDGLLibMaterial then
      GetMaterial.NotifyChange(Self);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLTextureSampler }
{$IFDEF GLS_REGION}{$REGION 'TDGLTextureSampler'}{$ENDIF}

procedure TDGLTextureSampler.Apply(var ARci: TRenderContextInfo);
begin
  if FIsValid then
    ARci.GLStates.SamplerBinding[ARci.GLStates.ActiveTexture] := FHandle.Handle;
end;

procedure TDGLTextureSampler.Assign(Source: TPersistent);
var
  LSampler: TDGLTextureSampler;
begin
  if Source is TDGLTextureSampler then
  begin
    LSampler          := TDGLTextureSampler(Source);
    FMinFilter        := LSampler.FMinFilter;
    FMagFilter        := LSampler.FMagFilter;
    FFilteringQuality := LSampler.FFilteringQuality;
    FLODBias          := LSampler.FLODBias;
    FLODBiasFract     := 0;
    FBorderColor.Assign(LSampler.FBorderColor);
    FWrap        := LSampler.FWrap;
    FCompareMode := LSampler.FCompareMode;
    FCompareFunc := LSampler.FCompareFunc;
    FDecodeSRGB  := LSampler.FDecodeSRGB;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TDGLTextureSampler.Create(AOwner: TDGLXCollection);
begin
  inherited;
  FDefferedInit     := False;
  FHandle           := TDGLSamplerHandle.Create;
  FHandle.OnPrepare := DoOnPrepare;
  FMagFilter        := maLinear;
  FMinFilter        := miLinearMipMapLinear;
  FFilteringQuality := tfAnisotropic;
  FLODBias          := 0;
  FLODBiasFract     := 0;
  FWrap[0]          := twRepeat;
  FWrap[1]          := twRepeat;
  FWrap[2]          := twRepeat;
  FBorderColor      := TDGLColor.CreateInitialized(Self, clrTransparent);
  FCompareMode      := tcmNone;
  FCompareFunc      := cfLEqual;
  FDecodeSRGB       := True;
  Name              := TDGLMatLibComponents(AOwner).MakeUniqueName('Sampler');
end;

destructor TDGLTextureSampler.Destroy;
begin
  FHandle.Destroy;
  FBorderColor.Destroy;
  inherited;
end;

function TDGLTextureSampler.GetWrap(Index: Integer): TDGLSeparateTextureWrap;
begin
  Result := FWrap[Index];
end;

procedure TDGLTextureSampler.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TDGLTextureSampler.DoOnPrepare(Sender: TDGLContext);
var
  ID: TGLUint;
begin
  if IsDesignTime and FDefferedInit then
    exit;
  try
    if FHandle.IsSupported then
    begin
      FHandle.AllocateHandle;
      ID := FHandle.Handle;
      if FHandle.IsDataNeedUpdate then
        begin
          glSamplerParameterfv(ID, GL_TEXTURE_BORDER_COLOR, FBorderColor.AsAddress);
          glSamplerParameteri(ID, GL_TEXTURE_WRAP_S, cTextureWrapMode[FWrap[0]]);
          glSamplerParameteri(ID, GL_TEXTURE_WRAP_T, cTextureWrapMode[FWrap[1]]);
          glSamplerParameteri(ID, GL_TEXTURE_WRAP_R, cTextureWrapMode[FWrap[2]]);
          glSamplerParameterf(ID, GL_TEXTURE_LOD_BIAS, FLODBias + FLODBiasFract);
          glSamplerParameteri(ID, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FMinFilter]);
          glSamplerParameteri(ID, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);

          if dglCheckExtension('EXT_texture_filter_anisotropic') then
          begin
            if FFilteringQuality = tfAnisotropic then
              glSamplerParameteri(ID, GL_TEXTURE_MAX_ANISOTROPY_EXT, CurrentDGLContext.GLStates.MaxTextureAnisotropy)
            else
              glSamplerParameteri(ID, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
          end;

          glSamplerParameteri(ID, GL_TEXTURE_COMPARE_MODE, cTextureCompareMode[FCompareMode]);
          glSamplerParameteri(ID, GL_TEXTURE_COMPARE_FUNC, cGLComparisonFunctionToGLEnum[FCompareFunc]);

          if dglCheckExtension('EXT_texture_sRGB_decode') then
          begin
            if FDecodeSRGB then
              glSamplerParameteri(ID, GL_TEXTURE_SRGB_DECODE_EXT, GL_DECODE_EXT)
            else
              glSamplerParameteri(ID, GL_TEXTURE_SRGB_DECODE_EXT, GL_SKIP_DECODE_EXT);
          end;
          {$IFDEF GLS_OPENGL_DEBUG}
          CheckOpenGLError;
          {$ENDIF}
          FHandle.NotifyDataUpdated;
        end;
      FIsValid := True;
    end
    else
      FIsValid := False;

  except
    FIsValid := False;
  end;
end;

class function TDGLTextureSampler.FriendlyName: string;
begin
  Result := 'Texture Sampler';
end;

procedure TDGLTextureSampler.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name              := ReadString;
      FDefferedInit     := ReadBoolean;
      FMinFilter        := TDGLMinFilter(ReadInteger);
      FMagFilter        := TDGLMagFilter(ReadInteger);
      FFilteringQuality := TDGLTextureFilteringQuality(ReadInteger);
      FLODBias          := ReadInteger;
      FWrap[0]          := TDGLSeparateTextureWrap(ReadInteger);
      FWrap[1]          := TDGLSeparateTextureWrap(ReadInteger);
      FWrap[2]          := TDGLSeparateTextureWrap(ReadInteger);
      Read(FBorderColor.AsAddress^, SizeOf(TColorVector));
      FCompareMode := TDGLTextureCompareMode(ReadInteger);
      FCompareFunc := TDepthFunction(ReadInteger);
      FDecodeSRGB  := ReadBoolean;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TDGLTextureSampler.SetBorderColor(const AValue: TDGLColor);
begin
  FBorderColor.Assign(AValue);
  NotifyChange(Self);
end;

procedure TDGLTextureSampler.SetCompareFunc(AValue: TDepthFunction);
begin
  if FCompareFunc <> AValue then
  begin
    FCompareFunc := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLTextureSampler.SetCompareMode(AValue: TDGLTextureCompareMode);
begin
  if FCompareMode <> AValue then
  begin
    FCompareMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLTextureSampler.SetDecodeSRGB(AValue: Boolean);
begin
  if FDecodeSRGB <> AValue then
  begin
    FDecodeSRGB := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLTextureSampler.SetFilteringQuality(AValue: TDGLTextureFilteringQuality);
begin
  if FFilteringQuality <> AValue then
  begin
    FFilteringQuality := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLTextureSampler.SetLODBias(AValue: Integer);
begin
  if FLODBias <> AValue then
  begin
    FLODBias := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLTextureSampler.SetMagFilter(AValue: TDGLMagFilter);
begin
  if FMagFilter <> AValue then
  begin
    FMagFilter := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLTextureSampler.SetMinFilter(AValue: TDGLMinFilter);
begin
  if FMinFilter <> AValue then
  begin
    FMinFilter := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLTextureSampler.SetWrap(Index: Integer; AValue: TDGLSeparateTextureWrap);
begin
  if FWrap[Index] <> AValue then
  begin
    FWrap[Index] := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLTextureSampler.UnApply(var ARci: TRenderContextInfo);
begin
  if FHandle.IsSupported then
    with ARci.GLStates do
      SamplerBinding[ActiveTexture] := 0;
end;

procedure TDGLTextureSampler.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    WriteInteger(Integer(FMinFilter));
    WriteInteger(Integer(FMagFilter));
    WriteInteger(Integer(FFilteringQuality));
    WriteInteger(FLODBias);
    WriteInteger(Integer(FWrap[0]));
    WriteInteger(Integer(FWrap[1]));
    WriteInteger(Integer(FWrap[2]));
    Write(FBorderColor.AsAddress^, SizeOf(TColorVector));
    WriteInteger(Integer(FCompareMode));
    WriteInteger(Integer(FCompareFunc));
    WriteBoolean(FDecodeSRGB);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLTextureCombiner }
{$IFDEF GLS_REGION}{$REGION 'TDGLTextureCombiner'}{$ENDIF}

procedure TDGLTextureCombiner.Assign(Source: TPersistent);
var
  LCombiner: TDGLTextureCombiner;
begin
  if Source is TDGLTextureCombiner then
  begin
    LCombiner := TDGLTextureCombiner(Source);
    FScript.Assign(LCombiner.FScript);
  end;
  inherited;
end;

constructor TDGLTextureCombiner.Create(AOwner: TDGLXCollection);
begin
  inherited;
  FDefferedInit      := False;
  FHandle            := TDGLVirtualHandle.Create;
  FHandle.OnAllocate := DoAllocate;
  FHandle.OnDestroy  := DoDeallocate;
  FHandle.OnPrepare  := DoOnPrepare;
  FScript            := TStringList.Create;
  FScript.OnChange   := NotifyChange;
  FIsValid           := True;
  Name               := TDGLMatLibComponents(AOwner).MakeUniqueName('Combiner');
end;

destructor TDGLTextureCombiner.Destroy;
begin
  FHandle.Destroy;
  FScript.Destroy;
  inherited;
end;

procedure TDGLTextureCombiner.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TDGLTextureCombiner.DoAllocate(Sender: TDGLVirtualHandle; var Handle: TGLUint);
begin
  Handle := 1;
end;

procedure TDGLTextureCombiner.DoDeallocate(Sender: TDGLVirtualHandle; var Handle: TGLUint);
begin
  Handle := 0;
end;

procedure TDGLTextureCombiner.DoOnPrepare(Sender: TDGLContext);
begin
  if IsDesignTime and FDefferedInit then exit;
  if dglCheckExtension('ARB_multitexture') then
  begin
    FHandle.AllocateHandle;
    if FHandle.IsDataNeedUpdate then
    begin
      try
        FCommandCache := GetTextureCombiners(FScript);
        FIsValid      := True;
      except
        on E: Exception do
        begin
          FIsValid := False;
          if IsDesignTime then
            InformationDlg(E.ClassName + ': ' + E.Message)
          else
            DGLSLogger.LogError(E.ClassName + ': ' + E.Message);
        end;
      end;
      FHandle.NotifyDataUpdated;
    end;
  end
  else
    FIsValid := False;
end;

class function TDGLTextureCombiner.FriendlyName: string;
begin
  Result := 'Texture Combiner';
end;

procedure TDGLTextureCombiner.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name          := ReadString;
      FDefferedInit := ReadBoolean;
      FScript.Text  := ReadString;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TDGLTextureCombiner.SetScript(AValue: TStringList);
begin
  FScript.Assign(AValue);
  NotifyChange(Self);
end;

procedure TDGLTextureCombiner.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    WriteString(FScript.Text);
  end;
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TGLAbstractTexture }
{$IFDEF GLS_REGION}{$REGION 'TDGLAbstractTexture'}{$ENDIF}

function TDGLAbstractTexture.GetTextureTarget: TDGLTextureTarget;
begin
  Result := FHandle.Target;
end;
function TDGLAbstractTexture.GetSwizzles(const AIndex: Integer):TDGLTextureSwizzle;
begin
  Result := FSwizzles.V[AIndex];
end;

procedure TDGLAbstractTexture.SetSwizzles(const AIndex:Integer; const Value: TDGLTextureSwizzle);
begin

  FSwizzles.V[AIndex] := Value;
  NotifyChange(Self);
//  if (FSwizzles = Value) then exit;

//  FSwizzles.r := Value.r;
//  FSwizzles.g := Value.g;
//  FSwizzles.b := Value.b;
//  FSwizzles.a := Value.a;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// { TDGLTextureImage }
{$IFDEF GLS_REGIONS}{$REGION 'TDGLTextureImage'}{$ENDIF}

//constructor TDGLTextureImage.Create(AOwner: TPersistent);
//begin
//  inherited;
//  FOwnerTexture := (AOwner as TDGLTexture);
//end;
//
//destructor TDGLTextureImage.Destroy;
//begin
//  inherited Destroy;
//end;
//
//class function TDGLTextureImage.FriendlyDescription: string;
//begin
//  Result := FriendlyName;
//end;
//
//procedure TDGLTextureImage.Invalidate;
//begin
//  ReleaseBitmap32;
//  NotifyChange(Self);
//end;
//
//procedure TDGLTextureImage.ReleaseBitmap32;
//begin
//  // nothing here.
//end;
//
//// AsBitmap : Returns the TextureImage as a TBitmap
//// WARNING: This Creates a new bitmap. Remember to free it, to prevent leaks.
//// If possible, rather use AssignToBitmap.
////
//
//function TDGLTextureImage.AsBitmap: TGLBitmap;
//begin
//  result := self.GetBitmap32.Create32BitsBitmap;
//end;
//
//procedure TDGLTextureImage.AssignToBitmap(aBitmap: TGLBitmap);
//begin
//  Self.GetBitmap32.AssignToBitmap(aBitmap);
//end;
//
//procedure TDGLTextureImage.NotifyChange(Sender: TObject);
//begin
//  if Assigned(FOwnerTexture) then
//  begin
//    FOwnerTexture.Handle.NotifyChangesOfData;
//    FOwnerTexture.Sampler.Handle.NotifyChangesOfData;
//    // Check for texture target change
//    GetTextureTarget;
//    FOwnerTexture.NotifyChange(Self);
//  end;
//end;
//
//procedure TDGLTextureImage.LoadFromFile(const fileName: string);
//var
//  buf: string;
//begin
//  if Assigned(FOnTextureNeeded) then
//  begin
//    buf := fileName;
//    FOnTextureNeeded(Self, buf);
//  end;
//end;
//
//function TDGLTextureImage.GetResourceName: string;
//begin
//  Result := FResourceFile;
//end;
//
//class function TDGLTextureImage.IsSelfLoading: Boolean;
//begin
//  Result := False;
//end;
//
//procedure TDGLTextureImage.LoadTexture(AInternalFormat: TDGLInternalFormat);
//begin
//end;
//
//{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
//
//// ------------------
//// { TDGLBlankImage }
//{$IFDEF GLS_REGIONS}{$REGION 'TDGLBlankImage'}{$ENDIF}
//
//constructor TDGLBlankImage.Create(AOwner: TPersistent);
//begin
//  inherited;
//  fWidth := 256;
//  fHeight := 256;
//  fDepth := 0;
//  fColorFormat := GL_RGBA;
//end;
//
//destructor TDGLBlankImage.Destroy;
//begin
//  ReleaseBitmap32;
//  inherited Destroy;
//end;
//
//procedure TDGLBlankImage.Assign(Source: TPersistent);
//var
//  img: TDGLBlankImage;
//begin
//  if Assigned(Source) then
//  begin
//    if (Source is TDGLBlankImage) then
//    begin
//      img := Source as TDGLBlankImage;
//      FWidth := img.Width;
//      FHeight := img.Height;
//      FDepth := img.Depth;
//      FCubeMap := img.fCubeMap;
//      FArray := img.fArray;
//      fColorFormat := img.ColorFormat;
//      FResourceFile := img.ResourceName;
//      Invalidate;
//    end
//    else
//      GetBitmap32.Assign(Source);
//    NotifyChange(Self);
//  end
//  else
//    inherited;
//end;
//
//procedure TDGLBlankImage.SetWidth(val: Integer);
//begin
//  if val <> FWidth then
//  begin
//    FWidth := val;
//    if FWidth < 1 then
//      FWidth := 1;
//    Invalidate;
//  end;
//end;
//
//function TDGLBlankImage.GetWidth: Integer;
//begin
//  Result := FWidth;
//end;
//
//procedure TDGLBlankImage.SetHeight(val: Integer);
//begin
//  if val <> FHeight then
//  begin
//    FHeight := val;
//    if FHeight < 1 then
//      FHeight := 1;
//    Invalidate;
//  end;
//end;
//
//function TDGLBlankImage.GetHeight: Integer;
//begin
//  Result := FHeight;
//end;
//
//procedure TDGLBlankImage.SetDepth(val: Integer);
//begin
//  if val <> FDepth then
//  begin
//    FDepth := val;
//    if FDepth < 0 then
//      FDepth := 0;
//    Invalidate;
//  end;
//end;
//
//function TDGLBlankImage.GetDepth: Integer;
//begin
//  Result := fDepth;
//end;
//
//procedure TDGLBlankImage.SetCubeMap(const val: Boolean);
//begin
//  if val <> fCubeMap then
//  begin
//    fCubeMap := val;
//    Invalidate;
//  end;
//end;
//
//procedure TDGLBlankImage.SetArray(const val: Boolean);
//begin
//  if val <> fArray then
//  begin
//    fArray := val;
//    Invalidate;
//  end;
//end;
//
//function TDGLBlankImage.GetBitmap32: TDGLImage;
//begin
//  if not Assigned(FBitmap) then
//  begin
//    fBitmap := TDGLImage.Create;
//    fBitmap.Width := FWidth;
//    fBitmap.Height := FHeight;
//    fBitmap.Depth := FDepth;
//    fBitmap.CubeMap := FCubeMap;
//    fBitmap.TextureArray := FArray;
//    fBitmap.SetColorFormatDataType(FColorFormat, GL_UNSIGNED_BYTE);
//  end;
//  Result := FBitmap;
//end;
//
//procedure TDGLBlankImage.ReleaseBitmap32;
//begin
//  if Assigned(FBitmap) then
//  begin
//    FBitmap.Free;
//    FBitmap := nil;
//  end;
//end;
//
//procedure TDGLBlankImage.SaveToFile(const fileName: string);
//begin
//  SaveAnsiStringToFile(fileName, AnsiString(
//    '[BlankImage]'#13#10'Width=' + IntToStr(Width) +
//    #13#10'Height=' + IntToStr(Height) +
//    #13#10'Depth=' + IntToStr(Depth)));
//end;
//
//procedure TDGLBlankImage.LoadFromFile(const fileName: string);
//var
//  sl: TStringList;
//  buf, temp: string;
//begin
//  buf := fileName;
//  if Assigned(FOnTextureNeeded) then
//    FOnTextureNeeded(Self, buf);
//  if FileExists(buf) then
//  begin
//    sl := TStringList.Create;
//    try
//      sl.LoadFromFile(buf, TEncoding.ASCII);
//      FWidth := StrToInt(sl.Values['Width']);
//      FHeight := StrToInt(sl.Values['Height']);
//      temp := sl.Values['Depth'];
//      if Length(temp) > 0 then
//        FDepth := StrToInt(temp)
//      else
//        FDepth := 1;
//    finally
//      sl.Free;
//    end;
//  end
//  else
//  begin
//    Assert(False, Format(glsFailedOpenFile, [fileName]));
//  end;
//end;
//
//class function TDGLBlankImage.FriendlyName: string;
//begin
//  Result := 'Blank Image';
//end;
//
//class function TDGLBlankImage.FriendlyDescription: string;
//begin
//  Result := 'Blank Image (Width x Height x Depth)';
//end;
//
//function TDGLBlankImage.GetTextureTarget: TDGLTextureTarget;
//begin
//  Result := ttTexture2D;
//  // Choose a texture target
//  if Assigned(fBitmap) then
//  begin
//    FWidth := fBitmap.Width;
//    FHeight := fBitmap.Height;
//    FDepth := fBitmap.Depth;
//    FCubeMap := fBitmap.CubeMap;
//    FArray := fBitmap.TextureArray;
//  end;
//
//  if FHeight = 1 then
//    Result := ttTexture1D;
//  if FCubeMap then
//    Result := ttTextureCube;
//  if FDepth > 0 then
//    Result := ttTexture3D;
//  if FArray then
//  begin
//    if FDepth < 2 then
//      Result := ttTexture1DArray
//    else
//      Result := ttTexture2DArray;
//    if FCubeMap then
//      Result := ttTextureCubeArray;
//  end;
//
//  if Assigned(FOwnerTexture) then
//  begin
//    if ((FOwnerTexture.Format >= tfR16F)
//      and (FOwnerTexture.Format <= tfRGBA32F)) then
//      Result := ttTextureRect;
//  end;
//end;
//
//{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TDGLPictureImage }
//{$IFDEF GLS_REGIONS}{$REGION 'TDGLPictureImage'}{$ENDIF}
//
//constructor TDGLPictureImage.Create(AOwner: TPersistent);
//begin
//  inherited;
//end;
//
//destructor TDGLPictureImage.Destroy;
//begin
//  ReleaseBitmap32;
//  FGLPicture.Free;
//  inherited Destroy;
//end;
//
//procedure TDGLPictureImage.Assign(Source: TPersistent);
//var
//  bmp: TGLBitmap;
//begin
//  if Assigned(Source) then
//  begin
//    if (Source is TDGLPersistentImage) then
//      Picture.Assign(TDGLPersistentImage(Source).Picture)
//    else if (Source is TDGLGraphic) then
//      Picture.Assign(Source)
//    else if (Source is TDGLPicture) then
//      Picture.Assign(Source)
//    else if (Source is TDGLImage) then
//    begin
//      bmp := TDGLImage(Source).Create32BitsBitmap;
//      Picture.Graphic := bmp;
//      bmp.Free;
//      FResourceFile := TDGLImage(Source).ResourceName;
//    end
//    else
//      inherited;
//  end
//  else
//    inherited;
//end;
//
//procedure TDGLPictureImage.BeginUpdate;
//begin
//  Inc(FUpdateCounter);
//  Picture.OnChange := nil;
//end;
//
//procedure TDGLPictureImage.EndUpdate;
//begin
//  Assert(FUpdateCounter > 0, ClassName + ': Unbalanced Begin/EndUpdate');
//  Dec(FUpdateCounter);
//  Picture.OnChange := PictureChanged;
//  if FUpdateCounter = 0 then
//    PictureChanged(Picture);
//end;
//
//function TDGLPictureImage.GetHeight: Integer;
//begin
//  Result := Picture.Height;
//end;
//
//function TDGLPictureImage.GetWidth: Integer;
//begin
//  Result := Picture.Width;
//end;
//
//function TDGLPictureImage.GetDepth: Integer;
//begin
//  Result := 0;
//end;
//
//function TDGLPictureImage.GetBitmap32: TDGLImage;
//begin
//  if not Assigned(FBitmap) then
//  begin
//    FBitmap := TDGLImage.Create;
//    // we need to deactivate OnChange, due to a "glitch" in some TGraphics,
//    // for instance, TJPegImage triggers an OnChange when it is drawn...
//    if Assigned(Picture.Graphic) then
//    begin
//      if Assigned(Picture.OnChange) then
//      begin
//        Picture.OnChange := nil;
//        try
//          FBitmap.Assign(Picture.Graphic);
//        finally
//          Picture.OnChange := PictureChanged;
//        end;
//      end
//      else
//        FBitmap.Assign(Picture.Graphic);
//    end
//    else
//      FBitmap.SetErrorImage;
//  end;
//  Result := FBitmap;
//end;
//
//procedure TDGLPictureImage.ReleaseBitmap32;
//begin
//  if Assigned(FBitmap) then
//  begin
//    FBitmap.Free;
//    FBitmap := nil;
//  end;
//end;
//
//procedure TDGLPictureImage.PictureChanged(Sender: TObject);
//begin
//  Invalidate;
//end;
//
//function TDGLPictureImage.GetPicture: TDGLPicture;
//begin
//  if not Assigned(FGLPicture) then
//  begin
//    FGLPicture := TDGLPicture.Create;
//    FGLPicture.OnChange := PictureChanged;
//  end;
//  Result := FGLPicture;
//end;
//
//procedure TDGLPictureImage.SetPicture(const aPicture: TDGLPicture);
//begin
//  Picture.Assign(aPicture);
//end;
//
//function TDGLPictureImage.GetTextureTarget: TDGLTextureTarget;
//begin
//  Result := ttTexture2D;
//end;
//
//{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
//
//// ------------------
//// { TDGLPersistentImage }
//{$IFDEF GLS_REGIONS}{$REGION 'TDGLPersistentImage'}{$ENDIF}
//
//constructor TDGLPersistentImage.Create(AOwner: TPersistent);
//begin
//  inherited;
//end;
//
//destructor TDGLPersistentImage.Destroy;
//begin
//  inherited Destroy;
//end;
//
//procedure TDGLPersistentImage.SaveToFile(const fileName: string);
//begin
//  Picture.SaveToFile(fileName);
//  FResourceFile := fileName;
//end;
//
//procedure TDGLPersistentImage.LoadFromFile(const fileName: string);
//var
//  buf: string;
//  gr: TDGLGraphic;
//begin
//  buf := fileName;
//  FResourceFile := fileName;
//  if Assigned(FOnTextureNeeded) then
//    FOnTextureNeeded(Self, buf);
//  if ApplicationFileIODefined then
//  begin
//    gr := CreateGraphicFromFile(buf);
//    if Assigned(gr) then
//    begin
//      Picture.Graphic := gr;
//      gr.Free;
//      Exit;
//    end;
//  end
//  else if FileExists(buf) then
//  begin
//    Picture.LoadFromFile(buf);
//    Exit;
//  end;
//  Picture.Graphic := nil;
//  raise ETexture.CreateFmt(glsFailedOpenFile, [fileName]);
//end;
//
//// FriendlyName
////
//
//class function TDGLPersistentImage.FriendlyName: string;
//begin
//  Result := 'Persistent Image';
//end;
//
//// FriendlyDescription
////
//
//class function TDGLPersistentImage.FriendlyDescription: string;
//begin
//  Result := 'Image data is stored in its original format with other form resources,'
//    + 'ie. in the DFM at design-time, and embedded in the EXE at run-time.';
//end;
//
//{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TDGLPicFileImage }
//{$IFDEF GLS_REGIONS}{$REGION 'TDGLPicFileImage'}{$ENDIF}
//
//constructor TDGLPicFileImage.Create(AOwner: TPersistent);
//begin
//  inherited;
//end;
//
//destructor TDGLPicFileImage.Destroy;
//begin
//  inherited;
//end;
//
//procedure TDGLPicFileImage.Assign(Source: TPersistent);
//begin
//  if Source is TDGLPicFileImage then
//  begin
//    FPictureFileName := TDGLPicFileImage(Source).FPictureFileName;
//    FResourceFile := TDGLPicFileImage(Source).ResourceName;
//  end
//  else
//    inherited;
//end;
//
//procedure TDGLPicFileImage.SetPictureFileName(const val: string);
//begin
//  if val <> FPictureFileName then
//  begin
//    FPictureFileName := val;
//    FResourceFile := val;
//    FAlreadyWarnedAboutMissingFile := False;
//    Invalidate;
//  end;
//end;
//
//procedure TDGLPicFileImage.Invalidate;
//begin
//  Picture.OnChange := nil;
//  try
//    Picture.Assign(nil);
//    FBitmap := nil;
//  finally
//    Picture.OnChange := PictureChanged;
//  end;
//  inherited;
//end;
//
//function TDGLPicFileImage.GetHeight: Integer;
//begin
//  Result := FHeight;
//end;
//
//function TDGLPicFileImage.GetWidth: Integer;
//begin
//  Result := FWidth;
//end;
//
//function TDGLPicFileImage.GetDepth: Integer;
//begin
//  Result := 0;
//end;
//
//function TDGLPicFileImage.GetBitmap32: TDGLImage;
//var
//  buf: string;
//  gr: TDGLGraphic;
//begin
//  if (GetWidth <= 0) and (PictureFileName <> '') then
//  begin
//    Picture.OnChange := nil;
//    try
//      buf := PictureFileName;
//      SetExeDirectory;
//      if Assigned(FOnTextureNeeded) then
//        FOnTextureNeeded(Self, buf);
//      if FileStreamExists(buf) then
//      begin
//        gr := CreateGraphicFromFile(buf);
//        Picture.Graphic := gr;
//        gr.Free;
//      end
//      else
//      begin
//        Picture.Graphic := nil;
//        if not FAlreadyWarnedAboutMissingFile then
//        begin
//          FAlreadyWarnedAboutMissingFile := True;
//          GLOKMessageBox(Format(glsFailedOpenFileFromCurrentDir, [PictureFileName, GetCurrentDir]),glsError);
//        end;
//      end;
//      Result := inherited GetBitmap32;
//      FWidth := Result.Width;
//      FHeight := Result.Height;
//      Picture.Graphic := nil;
//    finally
//      Picture.OnChange := PictureChanged;
//    end;
//  end
//  else
//    Result := inherited GetBitmap32;
//end;
//
//procedure TDGLPicFileImage.SaveToFile(const fileName: string);
//begin
//  FResourceFile := fileName;
//  SaveAnsiStringToFile(fileName, AnsiString(PictureFileName));
//end;
//
//procedure TDGLPicFileImage.LoadFromFile(const fileName: string);
//var
//  buf: string;
//begin
//  inherited;
//  // attempt to autodetect if we are pointed to a file containing
//  // a filename or directly to an image
//  if SizeOfFile(fileName) < 512 then
//  begin
//    buf := string(LoadAnsiStringFromFile(fileName));
//    if Pos(#0, buf) > 0 then
//      PictureFileName := fileName
//    else
//      PictureFileName := buf;
//  end
//  else
//    PictureFileName := fileName;
//  FResourceFile := FPictureFileName;
//end;
//
//class function TDGLPicFileImage.FriendlyName: string;
//begin
//  Result := 'PicFile Image';
//end;
//
//class function TDGLPicFileImage.FriendlyDescription: string;
//begin
//  Result := 'Image data is retrieved from a file.';
//end;
//
//{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
//
//// ------------------
//{ TDGLCubeMapImage }
//{$IFDEF GLS_REGIONS}{$REGION 'TDGLCubeMapImage'}{$ENDIF}
//
//constructor TDGLCubeMapImage.Create(AOwner: TPersistent);
//var
//  i: TGLCubeMapTarget;
//begin
//  inherited;
//  for i := Low(FPicture) to High(FPicture) do
//  begin
//    FPicture[i] := TDGLPicture.Create;
//    FPicture[i].OnChange := PictureChanged;
//  end;
//end;
//
//destructor TDGLCubeMapImage.Destroy;
//var
//  i: TGLCubeMapTarget;
//begin
//  ReleaseBitmap32;
//  for i := Low(FPicture) to High(FPicture) do
//    FPicture[i].Free;
//  inherited Destroy;
//end;
//
//procedure TDGLCubeMapImage.Assign(Source: TPersistent);
//var
//  i: TGLCubeMapTarget;
//begin
//  if Assigned(Source) then
//  begin
//    if (Source is TDGLCubeMapImage) then
//    begin
//      for i := Low(FPicture) to High(FPicture) do
//        FPicture[i].Assign(TDGLCubeMapImage(Source).FPicture[i]);
//      Invalidate;
//    end
//    else
//      inherited;
//  end
//  else
//    inherited;
//end;
//
//function TDGLCubeMapImage.GetWidth: Integer;
//begin
//  Result := FPicture[cmtPX].Width;
//end;
//
//function TDGLCubeMapImage.GetHeight: Integer;
//begin
//  Result := FPicture[cmtPX].Height;
//end;
//
//function TDGLCubeMapImage.GetDepth: Integer;
//begin
//  Result := 0;
//end;
//
//function TDGLCubeMapImage.GetBitmap32: TDGLImage;
//var
//  I: Integer;
//  LImage: TDGLImage;
//begin
//  if Assigned(FImage) then FImage.Free;
//  LImage := TDGLImage.Create;
//  LImage.VerticalReverseOnAssignFromBitmap := True;
//
//  try
//    for I := 0 to 5 do
//    begin
//      FPicture[TGLCubeMapTarget(I)].OnChange := nil;
//      try
//        LImage.Assign(FPicture[TGLCubeMapTarget(I)].Graphic);
//        if not Assigned(FImage) then
//        begin
//          FImage := TDGLImage.Create;
//          FImage.Blank := True;
//          FImage.Width := LImage.Width;
//          FImage.Height := LImage.Height;
//          FImage.SetColorFormatDataType(LImage.ColorFormat, LImage.DataType);
//          FImage.CubeMap := True;
//          FImage.Blank := False;
//        end;
//        Move(LImage.Data^, TFriendlyImage(FImage).GetLevelAddress(0, I)^, LImage.LevelSizeInByte[0]);
//      finally
//        FPicture[TGLCubeMapTarget(I)].OnChange := PictureChanged;
//      end;
//    end;
//  finally
//    LImage.Destroy;
//  end;
//  Result := FImage;
//end;
//
//procedure TDGLCubeMapImage.ReleaseBitmap32;
//begin
//  if Assigned(FImage) then
//  begin
//    FImage.Free;
//    FImage := nil;
//  end;
//end;
//
//procedure TDGLCubeMapImage.BeginUpdate;
//var
//  i: TGLCubeMapTarget;
//begin
//  Inc(FUpdateCounter);
//  for i := Low(FPicture) to High(FPicture) do
//    FPicture[i].OnChange := nil;
//end;
//
//procedure TDGLCubeMapImage.EndUpdate;
//var
//  i: TGLCubeMapTarget;
//begin
//  Assert(FUpdateCounter > 0, ClassName + ': Unbalanced Begin/EndUpdate');
//  Dec(FUpdateCounter);
//  for i := Low(FPicture) to High(FPicture) do
//    FPicture[i].OnChange := PictureChanged;
//  if FUpdateCounter = 0 then
//    PictureChanged(FPicture[cmtPX]);
//end;
//
//procedure TDGLCubeMapImage.SaveToFile(const fileName: string);
//var
//  fs: TFileStream;
//  bmp: TGLBitmap;
//  i: TGLCubeMapTarget;
//  version: Word;
//begin
//  fs := TFileStream.Create(fileName, fmCreate);
//  bmp := TGLBitmap.Create;
//  try
//    version := $0100;
//    fs.Write(version, 2);
//    for i := Low(FPicture) to High(FPicture) do
//    begin
//      bmp.Assign(FPicture[i].Graphic);
//      bmp.SaveToStream(fs);
//    end;
//  finally
//    bmp.Free;
//    fs.Free;
//  end;
//end;
//
//procedure TDGLCubeMapImage.LoadFromFile(const fileName: string);
//var
//  fs: TFileStream;
//  bmp: TGLBitmap;
//  i: TGLCubeMapTarget;
//  version: Word;
//begin
//  fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyWrite);
//  bmp := TGLBitmap.Create;
//  try
//    fs.Read(version, 2);
//    Assert(version = $0100);
//    for i := Low(FPicture) to High(FPicture) do
//    begin
//      bmp.LoadFromStream(fs);
//      FPicture[i].Graphic := bmp;
//    end;
//  finally
//    bmp.Free;
//    fs.Free;
//  end;
//end;
//
//class function TDGLCubeMapImage.FriendlyName: string;
//begin
//  Result := 'CubeMap Image';
//end;
//
//class function TDGLCubeMapImage.FriendlyDescription: string;
//begin
//  Result := 'Image data is contain 6 pictures of cubemap faces.';
//end;
//
//procedure TDGLCubeMapImage.PictureChanged(Sender: TObject);
//begin
//  Invalidate;
//end;
//
//function TDGLCubeMapImage.GetTextureTarget: TDGLTextureTarget;
//begin
//  Result := ttTextureCube;
//end;
//
//procedure TDGLCubeMapImage.SetPicture(index: TGLCubeMapTarget; const val:
//  TDGLPicture);
//begin
//  FPicture[index].Assign(val);
//end;
//
//function TDGLCubeMapImage.GetPicture(index: TGLCubeMapTarget): TDGLPicture;
//begin
//  Result := FPicture[index];
//end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// -----------------
{ TDGLTexture }
{$IFDEF GLS_REGION}{$REGION 'TDGLTexture'}{$ENDIF}


procedure TDGLTexture.Apply(var ARci: TRenderContextInfo);
begin
  if FIsValid then
  begin
    // Just bind
    with ARci.GLStates do
    begin
      TextureBinding[ActiveTexture, FHandle.Target] := FHandle.Handle;
      ActiveTextureEnabled[FHandle.Target]          := True;
    end;

    if not IsDesignTime then
    begin
      if not FUseStreaming and Assigned(FImage) then
      begin
        Inc(FApplyCounter);
        if FApplyCounter > 16 then
          FreeAndNil(FImage);
      end;

      if FUseStreaming then
      begin
        StreamTransfer;
      end;
    end;
  end
  else
    with ARci.GLStates do
      TextureBinding[ActiveTexture, FHandle.Target] := 0;
end;

procedure TDGLTexture.Assign(Source: TPersistent);
var
  LTexture: TDGLTexture;
begin
  if Source is TDGLTexture then
  begin
    LTexture     := TDGLTexture(Source);
    FCompression := LTexture.FCompression;
    if Assigned(LTexture.FImage) then
    begin
      if not Assigned(FImage) then
        FImage := TDGLImage.Create;
      FImage.Assign(LTexture.FImage);
    end
    else
      FreeAndNil(FImage);
    FImageAlpha          := LTexture.FImageAlpha;
    FImageBrightness     := LTexture.FImageBrightness;
    FImageGamma          := LTexture.FImageGamma;
    FHeightToNormalScale := LTexture.FHeightToNormalScale;
    FSourceFile          := LTexture.FSourceFile;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TDGLTexture.Create(AOwner: TDGLXCollection);
begin
  inherited;
  FDefferedInit        := False;
  FHandle              := TDGLTextureHandle.Create;
  FHandle.OnPrepare    := DoOnPrepare;
  FImage               := TDGLImage.Create;
//  FImage.OnTextureNeeded := DoOnTextureNeeded;
  FCompression         := tcDefault;
  FImageAlpha          := tiaDefault;
  FImageBrightness     := 1.0;
  FImageGamma          := 1.0;
  FHeightToNormalScale := 1.0;
  FInternalFormat      := tfRGBA8;
  FInternallyStored    := False;
  FMipGenMode          := mgmOnFly;
  FUseStreaming        := False;
  Name                 := TDGLMatLibComponents(AOwner).MakeUniqueName('Texture');
//  FUsePBO              := False;
//  pboRBId:=0;
//  pboWBId:=0;
  FFullSize:=-1;
  FPixelSize:=0;
//  FKeepImageAfterTransfer := False;
end;


//function TDGLTexture.DownloadData(var Datas: pointer; UsePBO: boolean): boolean;
//var t:pointer;
//begin
//  if not FTexture.Created then begin result:=false;exit;
//  end else result:=true;
//  assert(FTexture.Target<>GL_TEXTURE_CUBE_MAP, 'Not working with Cubemap textures');
//  ReallocMem(Datas,FTexture.FullSize);
//  FTexture.Data:=datas;
//  FTexture.UsePBO:=UsePBO;
//  with FTexture do begin
//     glEnable(Target); glBindTexture(Target, Id);
//     if not UsePBO then glGetTexImage(Target, 0, ColorChanels,Precision, Datas)
//     else begin
//        if pboRBId=0 then begin
//          glGenBuffers(1,@pboRBId);
//          glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
//          glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
////          glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
//        end;
//        glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
//        glGetTexImage(Target,0,ColorChanels,Precision, nil);
////        glGetTexImage(Target,0,GL_BGRA,Precision, nil);
//        t := glMapBuffer(GL_PIXEL_PACK_BUFFER, GL_READ_ONLY);
//        CopyMemory(Datas,t,FullSize);
//        glUnmapBuffer(GL_PIXEL_PACK_BUFFER);
//        glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
//     end;
//     glBindTexture(Target, 0); glDisable(Target);
//  end;
//end;

//procedure TDGLTexture.UpLoadData(Data: pointer; UsePBO: boolean);
//begin
//  assert(FTexture.Target<>GL_TEXTURE_CUBE_MAP, 'Not working with Cubemap textures');
//  FTexture.Data:=Data;
//  FTexture.UsePBO:=UsePBO;
//  if Data=nil then exit;
//  if FTexture.Created then begin
//     glBindTexture(FTexture.Target,Ftexture.Id);
//     UploadTexture;
//     glBindTexture(FTexture.Target,0);
//  end;
//  FSettedParams:=FSettedParams+[stData];
//end;

//procedure TDGLTexture.FreePBO;
//begin
//
//     glBindBuffer(GL_PIXEL_UNPACK_BUFFER,0);
//     glBindBuffer(GL_PIXEL_PACK_BUFFER,0);
//     if pboRBId<>0 then glDeleteBuffers(1,@pboRBId);
//     if pboWBId<>0 then glDeleteBuffers(1,@pboWBId);
//     pboRBId:=0; pboWBId:=0;
//
//end;

destructor TDGLTexture.Destroy;
begin
  //FreePBO;
  FHandle.Destroy;
  FImage.Free;
  inherited;
end;

//function TDGLTexture.ReadFromPBO(pboId: GLUInt): boolean;
//begin
// if (not FTexture.Created) or (pboId<=0) then begin result:=false;exit;
// end else result:=true;
// with FTexture do begin
//  glEnable(Target); glBindTexture(Target, Id);
//  glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboId);
//  case Target of
//     GL_TEXTURE_1D: glTexSubImage1D(Target,0,0,Width,ColorChanels,Precision,nil);
//     GL_TEXTURE_2D,GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP..GL_TEXTURE_CUBE_MAP_NEGATIVE_Z:
//           glTexSubImage2D(Target,0,0,0,Width,Height,ColorChanels,Precision,nil);
//     GL_TEXTURE_3D: glTexSubImage3D(Target,0,0,0,0,Width,Height,Depth,ColorChanels,Precision,nil);
//     Else assert(false,'Unsupported uploading target or method');
//  end;
//  glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
//  if GenerateMipMaps then glGenerateMipmapEXT(Target);
// end;
//end;

//function TDGLTexture.WriteToPBO(pboId: GLUInt): boolean;
//begin
//  if (not FTexture.Created) or (pboId<=0) then begin result:=false;exit;
//  end else result:=true;
//  with FTexture do begin
//    glEnable(Target); glBindTexture(Target, Id);
//    glBindBuffer(GL_PIXEL_PACK_BUFFER, pboId);
//    glGetTexImage(Target,0,ColorChanels,Precision, nil);
//    glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
//    glBindTexture(Target, 0); glDisable(Target);
//  end;
//end;

//function TDGLTexture.ReadPixel(x, y: integer): TVector;
//var t:pointer;
//    i,offs: integer;
//begin
//  if not FTexture.Created then exit;
//  with FTexture do begin
//     glEnable(Target); glBindTexture(Target, Id);
//     if pboRBId=0 then begin
//          glGenBuffers(1,@pboRBId);
//          glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
//          glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
//     end else glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
//     glGetTexImage(Target,0,ColorChanels,Precision, nil);
//     t := glMapBuffer(GL_PIXEL_PACK_BUFFER, GL_READ_ONLY);
//     offs:=(y*Width+x)*PixelSize;
//     for i:=0 to PixelSize-1 do begin
//       case Precision of
//         GL_UNSIGNED_BYTE: begin
//                result[i]:=PByteArray(t)[offs+i];
//              end;
//         GL_FLOAT: begin
//                result[i]:=PSingleArray(t)[offs+i];
//              end;
//       end;
//     end;
//     glUnmapBuffer(GL_PIXEL_PACK_BUFFER);
//     glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
//     glBindTexture(Target, 0); glDisable(Target);
//  end;
//end;

//function TDGLTexture.GetReadPBO: GLUint;
//begin
//  if pboRBId=0 then
//  begin
//     glGenBuffers(1,@pboRBId);
//     glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
//     glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
//     glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
//  end;
//  result:=pboRBId;
//end;


//function TDGLTexture.GetWritePBO: GLUint;
//begin
//  if pboWBId=0 then
//  begin
//     glGenBuffers(1,@pboWBId);
//     glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
//     glBufferData(GL_PIXEL_UNPACK_BUFFER, FullSize, nil, GL_STREAM_READ);
//     glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
//  end;
//  result:=pboWBId;
//end;

//procedure TDGLTexture.SetPixel(x, y: integer; color: TVector);
//var t:pointer;
//    i,offs: integer;
//begin
//  if (not FTexture.Created) then exit;
//  with FTexture do begin
//    glEnable(Target); glBindTexture(Target, Id);
//    if pboWBId=0 then begin
//         glGenBuffers(1,@pboWBId);
//         glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
//         glBufferData(GL_PIXEL_UNPACK_BUFFER, FullSize, nil, GL_STREAM_READ);
//    end else glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
//
//    t := glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
//    offs:=(y*(Width-1)+x)*PixelSize;
//    for i:=0 to PixelSize-1 do begin
//      case Precision of
//         GL_UNSIGNED_BYTE:     PByteArray(t)[offs+i]:=trunc(color[i]);
//         GL_FLOAT:             PSingleArray(t)[offs+i]:=color[i];
//      end;
//    end;
//    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
//    glTexSubImage2D(Target,0,x,y,1,1,ColorChanels,Precision,pointer(offs));
//    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
//    glBindTexture(Target, 0); glDisable(Target);
//  end;
//end;

procedure TDGLTexture.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TDGLTexture.DoOnPrepare(Sender: TDGLContext);
var
  LTarget: TDGLTextureTarget;
//  tmpGLImage : TDGLImage;
  rowSize: Integer;
begin
  if IsDesignTime and FDefferedInit then exit;

  FHandle.AllocateHandle;
  if not FHandle.IsDataNeedUpdate then exit;


  try
    PrepareImage;

    // Target
    LTarget := FImage.GetTextureTarget;

    // Check supporting
    if not IsTargetSupported(LTarget) or not IsFormatSupported(FInternalFormat) then Abort;

    PixelSize:=GetTextureElementSize(FInternalFormat);
//    if FFullSize<0 then
    FFullSize:=width*height*depth*PixelSize;

    if (FHandle.Target <> LTarget) and (FHandle.Target <> ttNoShape) then
    begin
      FHandle.DestroyHandle;
      FHandle.AllocateHandle;
    end;
    FHandle.Target := LTarget;

    // Check streaming support
    if not IsDesignTime then
    begin
      FUseStreaming := FUseStreaming and TDGLUnpackPBOHandle.IsSupported;
//      FUseStreaming := FUseStreaming and IsServiceContextAvaible;
      FUseStreaming := FUseStreaming and (LTarget = ttTexture2D);
    end;

    with Sender.GLStates do
    begin
      ActiveTextureEnabled[FHandle.Target]          := True;
      TextureBinding[ActiveTexture, FHandle.Target] := FHandle.Handle;

      packRowLength := 0; //glPixelStorei ( GL_PACK_ROW_LENGTH,  0 );
      packAlignment := 1; //glPixelStorei ( GL_PACK_ALIGNMENT,   1 );
      packSkipRows  := 0; //glPixelStorei ( GL_PACK_SKIP_ROWS,   0 );
      packSkipPixels := 0; //glPixelStorei ( GL_PACK_SKIP_PIXELS, 0 );
      UnpackRowLength                               := 0; //glPixelStorei ( GL_UNPACK_ROW_LENGTH,  0 );
      UnpackSkipRows                                := 0; //glPixelStorei ( GL_UNPACK_SKIP_ROWS,   0 );
      UnpackSkipPixels                              := 0; //glPixelStorei ( GL_UNPACK_SKIP_PIXELS, 0 );
      rowSize                                       := FImage.LevelWidth[0] * FImage.ElementSize;
      if (rowSize mod 8 = 0) and (FImage.ElementSize > 4) then UnpackAlignment := 8
      else if rowSize mod 4 = 0 then UnpackAlignment := 4
      else if rowSize mod 2 = 0 then UnpackAlignment := 2
      else UnpackAlignment := 1; //glPixelStorei ( GL_UNPACK_ALIGNMENT,   1 );
    end;

//    if FUsePBO then
//    begin
//      glGenBuffers(1,@pboRBId);
//      glGenBuffers(1,@pboWBId);
//      Sender.GLStates.PixelUnPackBufferBinding:=pboWBID; //glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboWBId);
//      glBufferData(GL_PIXEL_UNPACK_BUFFER, FullSize, nil, GL_STREAM_READ);
//      Sender.GLStates.PixelUnPackBufferBinding:=0; // glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
//      Sender.GLStates.PixelPackBufferBinding:=pboRBID; // glBindBuffer(GL_PIXEL_PACK_BUFFER, pboRBId);
//      glBufferData(GL_PIXEL_PACK_BUFFER, FullSize, nil, GL_STREAM_DRAW);
//      Sender.GLStates.PixelPackBufferBinding:=0; // glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
//    end;

    if not IsDesignTime and FUseStreaming then
    begin
      TFriendlyImage(FImage).StartStreaming;
      FLastTime := GLSTime;
      StreamTransfer;
      FHandle.NotifyDataUpdated;
    end
    else
      FullTransfer;

    Sender.GLStates.ActiveTextureEnabled[FHandle.Target] := False;

    FApplyCounter := 0;
    FIsValid      := True;
  except
    FIsValid := False;
  end;
end;

procedure TDGLTexture.FullTransfer;
var
  LCompression: TDGLTextureCompression;
  glFormat:     TGLEnum;
begin
    if dglCheckExtension('ARB_texture_compression') then
    begin
      if Compression = tcDefault then
        if vDefaultTextureCompression = tcDefault then
          LCompression := tcNone
        else
          LCompression := vDefaultTextureCompression
      else
        LCompression := Compression;
    end
    else
      LCompression := tcNone;

    if LCompression <> tcNone then
      with CurrentDGLContext.GLStates do
      begin
        case LCompression of
          tcStandard:
            TextureCompressionHint := hintDontCare;
          tcHighQuality:
            TextureCompressionHint := hintNicest;
          tcHighSpeed:
            TextureCompressionHint := hintFastest;
        else
          Assert(False, glsErrorEx + glsUnknownType);
        end;
        if not GetGenericCompressedFormat(FInternalFormat, FImage.ColorFormat, glFormat) then
          glFormat := InternalFormatToOpenGLFormat(FInternalFormat);
      end
    else
      glFormat := InternalFormatToOpenGLFormat(FInternalFormat);

    FImage.RegisterAsOpenGLTexture(FHandle, FMipGenMode = mgmOnFly, glFormat, FWidth, FHeight, FDepth);

    if glGetError <> GL_NO_ERROR then
    begin
      ClearOpenGLError;
      CurrentDGLContext.GLStates.ActiveTextureEnabled[FHandle.Target] := False;
      DGLSLogger.LogErrorFmt('Unable to create texture "%s"', [Self.Name]);
      Abort;
    end
    else
      FHandle.NotifyDataUpdated;

end;

procedure TDGLTexture.CalcLODRange(out AFirstLOD, ALastLOD: Integer);
var
  i, MaxLODSize, MinLODSize, MaxLODZSize: Integer;
begin
  case FHandle.Target of
    ttTexture3D:
      begin
        MaxLODSize  := CurrentDGLContext.GLStates.Max3DTextureSize;
        MaxLODZSize := MaxLODSize;
      end;

    ttTextureCube:
      begin
        MaxLODSize  := CurrentDGLContext.GLStates.MaxCubeTextureSize;
        MaxLODZSize := 0;
      end;

    ttTexture1DArray, ttTexture2DArray, ttTextureCubeArray, ttTexture2DMultisampleArray:
      begin
        MaxLODSize  := CurrentDGLContext.GLStates.MaxTextureSize;
        MaxLODZSize := CurrentDGLContext.GLStates.MaxArrayTextureSize;
      end;

  else
    begin
      MaxLODSize  := CurrentDGLContext.GLStates.MaxTextureSize;
      MaxLODZSize := 0;
    end;
  end;

  MinLODSize := 1;

  AFirstLOD := 0;

  for i := 0 to High(TDGLImagePiramid) do
  begin
    if (FImage.LevelWidth[i] <= MaxLODSize) and (FImage.LevelHeight[i] <= MaxLODSize) and
       (FImage.LevelDepth[i] <= MaxLODZSize) then break;
    Inc(AFirstLOD);
  end;

  AFirstLOD := MinInteger(AFirstLOD, FImage.LevelCount - 1);
  ALastLOD  := AFirstLOD;

  for i := AFirstLOD to High(TDGLImagePiramid) do
  begin
    if (FImage.LevelWidth[i] < MinLODSize) or (FImage.LevelHeight[i] < MinLODSize) then
      break;
    Inc(ALastLOD);
  end;
  ALastLOD := MinInteger(ALastLOD, FImage.LevelCount - 1);
end;

procedure TDGLTexture.StreamTransfer;
var
  LImage:              TFriendlyImage;
  bContinueStreaming:  Boolean;
  OldBaseLevel, level: Integer;
  newTime:             Double;
  glInternalFormat:    TGLEnum;
  transferMethod:      0 .. 3;
begin
  LImage       := TFriendlyImage(FImage);
  OldBaseLevel := FBaseLevel;
  CalcLODRange(FBaseLevel, FMaxLevel);

  // Select transfer method
  if FImage.IsCompressed then
    transferMethod := 1
  else
    transferMethod := 0;
  if dglCheckExtension('EXT_direct_state_access') then
    transferMethod := transferMethod + 2;

  bContinueStreaming := False;
  for level          := FMaxLevel downto FBaseLevel do
  begin

    case LImage.LevelStreamingState[level] of

      ssKeeping:
        begin
          if FBaseLevel < level then
            FBaseLevel                      := FMaxLevel;
          LImage.LevelStreamingState[level] := ssLoading;
          LImage.DoStreaming;
          bContinueStreaming := True;
        end;

      ssLoading:
        begin
          LImage.DoStreaming;
          bContinueStreaming := True;
          if FBaseLevel < level then
            FBaseLevel := FMaxLevel;
        end;

      ssLoaded:
        begin
          LImage.LevelPixelBuffer[level].AllocateHandle;
          LImage.LevelPixelBuffer[level].Bind;
          glInternalFormat := InternalFormatToOpenGLFormat(FInternalFormat);
          case transferMethod of
            0:
              glTexImage2D(GL_TEXTURE_2D, level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.ColorFormat, FImage.DataType, nil);
            1:
              glCompressedTexImage2D(GL_TEXTURE_2D, level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.LevelSizeInByte[level], nil);
            2:
              glTextureImage2DEXT(FHandle.Handle, GL_TEXTURE_2D, level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.ColorFormat, FImage.DataType, nil);
            3:
              glCompressedTextureImage2DEXT(FHandle.Handle, GL_TEXTURE_2D, level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.LevelSizeInByte[level], nil);
          end;
          LImage.LevelPixelBuffer[level].UnBind;
          LImage.LevelStreamingState[level] := ssTransfered;
          //DGLSLogger.LogDebug(Format('Texture "%s" level %d loaded', [Name, level]));
        end;

      ssTransfered:
        begin
          if LImage.LevelPixelBuffer[level].IsAllocatedForContext then
            LImage.LevelPixelBuffer[level].DestroyHandle;
          FBaseLevel := level;
        end;
    end; // of case

    if bContinueStreaming then
      break;
  end; // for level

  if bContinueStreaming then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, FMaxLevel);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, FBaseLevel);
  end;

//  if FUSePBO then
//  begin
//
//  end;

  // Smooth transition between levels
  if Assigned(FApplicableSampler) then
    with FApplicableSampler do
    begin
      newTime := GLSTime;
      if FLODBiasFract > 0 then
        FLODBiasFract := FLODBiasFract - 0.05 * (newTime - FLastTime)
      else if FLODBiasFract < 0 then
        FLODBiasFract := 0;
      FLastTime       := newTime;
      if OldBaseLevel > FBaseLevel then
        FLODBiasFract := FLODBiasFract + (OldBaseLevel - FBaseLevel);

      if FApplicableSampler.IsValid then
        glSamplerParameterf(FApplicableSampler.Handle.Handle, GL_TEXTURE_LOD_BIAS, FLODBias + FLODBiasFract)
      else
        // To refrash texture parameters when sampler object not supported
        FLastSampler := nil;
    end;
end;

class function TDGLTexture.FriendlyName: string;
begin
  Result := 'Texture Image';
end;

// DoOnTextureNeeded
//

//procedure TDGLTexture.DoOnTextureNeeded(Sender: TObject; var textureFileName:
//  string);
//begin
//  if Assigned(FOnTextureNeeded) then FOnTextureNeeded(Sender, textureFileName);
//end;

// AllocateHandle
//

//function TDGLTexture.AllocateHandle: TGLuint;
//var
//  vTarget: TDGLTextureTarget;
//begin
//  vTarget := Image.NativeTextureTarget;
//  if (vTarget <> ttNoShape) and (Handle.Target <> vTarget) then Handle.DestroyHandle;
//
//  Result := Handle.Handle;
//  if Result = 0 then
//  begin
//    Handle.AllocateHandle;
//    Result := Handle.Handle;
//  end;
//  if Handle.IsDataNeedUpdate then
//  begin
//    Handle.Target := vTarget;
//    Sampler.Handle.NotifyChangesOfData;
//  end;
//  if Sampler.Handle = nil then Sampler.Handle.AllocateHandle;
//
//  // bind texture
//  if (Handle.Target <> ttNoShape) and IsTargetSupported(Handle.Target) then
//  begin
//    if Sampler.Handle.IsDataNeedUpdate then
//    begin
//      with CurrentDGLContext.GLStates do
//        TextureBinding[ActiveTexture, Handle.Target] := Result;
//      //PrepareParams(DecodeGLTextureTarget(Handle.Target));
//      Sampler.Handle.NotifyDataUpdated;
//    end;
//  end
//  else
//    Result := 0;
//end;

// IsHandleAllocated
//

//function TDGLTexture.IsHandleAllocated: Boolean;
//begin
//  Result := (Handle.Handle <> 0);
//end;

//procedure TDGLTexture.SetTextureErrorImage;
//var
//  img: TDGLImage;
//begin
//  img := TDGLImage.Create;
//  img.SetErrorImage;
//
//  ImageClassName := TDGLBlankImage.className;
//  TDGLBlankImage(Image).Assign(img);
//  img.Free;
//
////  MagFilter := maNearest;
////  MinFilter := miNearest;
////  TextureWrap := twBoth;
////  MappingMode := tmmUser;
//  Compression := tcNone;
////  AllocateHandle;
//end;

// PrepareImage
//

//procedure TGLTexture.PrepareImage(target: TGLUInt);
//var
//  bitmap32: TDGLImage;
//  texComp: TGLTextureCompression;
//  glFormat: TGLEnum;
//begin
//  if Image.IsSelfLoading then
//  begin
//    Image.LoadTexture(FTextureFormat);
//  end
//  else
//  begin
//
//    bitmap32 := Image.GetBitmap32;
//
//    if (bitmap32 = nil) or bitmap32.IsEmpty then
//      Exit;
//
//    if TextureFormat = tfNormalMap then
//      bitmap32.GrayScaleToNormalMap(NormalMapScale,
//        TextureWrap in [twBoth, twHorizontal],
//        TextureWrap in [twBoth, twVertical]);
//    // prepare AlphaChannel
//    case ImageAlpha of
//      tiaDefault: ; // nothing to do
//      tiaAlphaFromIntensity:
//        bitmap32.SetAlphaFromIntensity;
//      tiaSuperBlackTransparent:
//        bitmap32.SetAlphaTransparentForColor($000000);
//      tiaLuminance:
//        bitmap32.SetAlphaFromIntensity;
//      tiaLuminanceSqrt:
//        begin
//          bitmap32.SetAlphaFromIntensity;
//          bitmap32.SqrtAlpha;
//        end;
//      tiaOpaque:
//        bitmap32.SetAlphaToValue(255);
//      tiaTopLeftPointColorTransparent:
//        begin
//          bitmap32.Narrow;
//          bitmap32.SetAlphaTransparentForColor(bitmap32.Data^[0]);
//        end;
//      tiaInverseLuminance:
//        begin
//          bitmap32.SetAlphaFromIntensity;
//          bitmap32.InvertAlpha;
//        end;
//      tiaInverseLuminanceSqrt:
//        begin
//          bitmap32.SetAlphaFromIntensity;
//          bitmap32.SqrtAlpha;
//          bitmap32.InvertAlpha;
//        end;
//      tiaBottomRightPointColorTransparent:
//        begin
//          bitmap32.Narrow;
//          bitmap32.SetAlphaTransparentForColor(bitmap32.Data^[bitmap32.Width - 1]);
//        end;
//    else
//      Assert(False);
//    end;
//    // apply brightness correction
//    if FImageBrightness <> 1.0 then
//      bitmap32.BrightnessCorrection(FImageBrightness);
//    // apply gamma correction
//    if FImageGamma <> 1.0 then
//      bitmap32.GammaCorrection(FImageGamma);
//
//    if GL.ARB_texture_compression
//      and (TextureFormat <> tfExtended) then
//    begin
//      if Compression = tcDefault then
//        if vDefaultTextureCompression = tcDefault then
//          texComp := tcNone
//        else
//          texComp := vDefaultTextureCompression
//      else
//        texComp := Compression;
//      if IsFloatType then
//        texComp := tcNone;
//
//    end
//    else
//      texComp := tcNone;
//
//    if (texComp <> tcNone) and (TextureFormat <= tfNormalMap) then
//      with CurrentGLContext.GLStates do
//      begin
//        case texComp of
//          tcStandard: TextureCompressionHint := hintDontCare;
//          tcHighQuality: TextureCompressionHint := hintNicest;
//          tcHighSpeed: TextureCompressionHint := hintFastest;
//        else
//          Assert(False, glsErrorEx + glsUnknownType);
//        end;
//        glFormat := CompressedInternalFormatToOpenGL(FTextureFormat);
//      end
//    else
//      glFormat := InternalFormatToOpenGLFormat(FTextureFormat);
//
//    bitmap32.RegisterAsOpenGLTexture(
//      FTextureHandle,
//      not (FMinFilter in [miNearest, miLinear]),
//      glFormat,
//      FTexWidth,
//      FTexHeight,
//      FTexDepth);
//  end;
//
//  if GL.GetError <> GL_NO_ERROR then
//  begin
//    GL.ClearError;
//    SetTextureErrorImage;
//  end
//  else
//  begin
//    FRequiredMemorySize := -1;
//    TextureImageRequiredMemory;
//    if not IsDesignTime and not FKeepImageAfterTransfer then
//      Image.ReleaseBitmap32;
//  end;
//end;

procedure TDGLTexture.PrepareImage;
const
  cAlphaProc: array [TDGLTextureImageAlpha] of TImageAlphaProc = (nil, ImageAlphaFromIntensity, ImageAlphaSuperBlackTransparent, ImageAlphaLuminance, ImageAlphaLuminanceSqrt, ImageAlphaOpaque, ImageAlphaTopLeftPointColorTransparent,
    ImageAlphaInverseLuminance, ImageAlphaInverseLuminanceSqrt, ImageAlphaBottomRightPointColorTransparent);

var
  ext, filename:             string;
  BaseImageClass:            TDGLBaseImageClass;
  LPicture:                  TDGLPicture;
  LGraphic:                  TDGLGraphic;
  LImage:                    TDGLImage;
  level:                     Integer;
  glColorFormat, glDataType: TGLEnum;
  bReadFromSource:           Boolean;
  LStream:                   TStream;
  ptr:                       PByte;

  procedure ReplaceImageClass;
  begin
    if not(FImage is TDGLImage) then
    begin
      LImage := TDGLImage.Create;
      LImage.Assign(FImage);
      FImage.Destroy;
      FImage :=TDGLImage(LImage);
    end
    else
      LImage := TDGLImage(FImage);;
  end;

begin
  if not Assigned(FImage) then
  begin
    try
      SetExeDirectory;
      bReadFromSource := True;

      if FInternallyStored and not IsDesignTime then
      begin
        filename := Name + '.image';
        if FileStreamExists(filename) then
        begin
          FImage              := TDGLImage.Create;
          FImage.ResourceName := filename;
          TFriendlyImage(FImage).LoadHeader;
          if not FUseStreaming then
          begin
            ReallocMem(TFriendlyImage(FImage).fData, FImage.DataSize);
            for level :=FImage.LevelCount - 1 downto 0 do
            begin
              LStream := CreateFileStream(filename + IntToHex(level, 2), fmOpenRead);
              ptr     := PByte(TFriendlyImage(FImage).GetLevelAddress(level));
              LStream.Read(ptr^, FImage.LevelSizeInByte[level]);
              LStream.Destroy;
            end;
          end;
          bReadFromSource := False;
        end
        else
        begin
          FInternallyStored := False;
          FUseStreaming     := False;
        end;
      end;

      if bReadFromSource then
      begin
        if (Length(FSourceFile) > 0) and FileStreamExists(FSourceFile) then
        begin
          // At first check moder image file loaders
          ext := ExtractFileExt(FSourceFile);
          System.Delete(ext, 1, 1);
          BaseImageClass := GetRasterFileFormats.FindExt(ext);

          if Assigned(BaseImageClass) then
          begin
            FImage := BaseImageClass.Create;
            FImage.LoadFromFile(FSourceFile);
          end
          else
          begin
            // Check old loaders
            FImage := TDGLImage.Create;
            if ApplicationFileIODefined then
            begin
              LGraphic := CreateGraphicFromFile(FSourceFile);
              FImage.Assign(LGraphic);
              LGraphic.Free;
            end
            else
            begin
              LPicture := TDGLPicture.Create;
              LPicture.LoadFromFile(FSourceFile);
              FImage.Assign(LPicture.Graphic);
              LPicture.Destroy;
            end;
          end;

          if FInternalFormat <> FImage.InternalFormat then
          begin
            ReplaceImageClass;
            FindCompatibleDataFormat(FInternalFormat, glColorFormat, glDataType);
            TDGLImage(FImage).SetColorFormatDataType(glColorFormat, glDataType);
            TFriendlyImage(FImage).FInternalFormat := FInternalFormat;
          end;

          if (ImageAlpha <> tiaDefault) or (FImageBrightness <> 1.0) or (FImageGamma <> 1.0) then
          begin
            ReplaceImageClass;
            for level := 0 to FImage.LevelCount - 1 do
            begin
              AlphaGammaBrightCorrection(TFriendlyImage(FImage).GetLevelAddress(level), FImage.ColorFormat, FImage.DataType, FImage.LevelWidth[level], FImage.LevelHeight[level], cAlphaProc[ImageAlpha], FImageBrightness, FImageGamma);
            end;
          end
          else if FHeightToNormalScale <> 1.0 then
          begin
            ReplaceImageClass;
            // HeightToNormalMap();
            {$MESSAGE Hint 'TDGLTextureImage.HeightToNormalScale not yet implemented' }
          end;

          case FMipGenMode of
            mgmNoMip:
              FImage.UnMipmap;

            mgmLeaveExisting, mgmOnFly:
              ;

            mgmBoxFilter:
              FImage.GenerateMipmap(ImageBoxFilter);

            mgmTriangleFilter:
              FImage.GenerateMipmap(ImageTriangleFilter);

            mgmHermiteFilter:
              FImage.GenerateMipmap(ImageHermiteFilter);

            mgmBellFilter:
              FImage.GenerateMipmap(ImageBellFilter);

            mgmSplineFilter:
              FImage.GenerateMipmap(ImageSplineFilter);

            mgmLanczos3Filter:
              FImage.GenerateMipmap(ImageLanczos3Filter);

            mgmMitchellFilter:
              FImage.GenerateMipmap(ImageMitchellFilter);
          end;

          // Store cooked image
          if FInternallyStored and IsDesignTime then
          begin
            filename            := Name + '.image';
            FImage.ResourceName := filename;
            TFriendlyImage(FImage).SaveHeader;
            for level := FImage.LevelCount - 1 downto 0 do
            begin
              LStream := CreateFileStream(filename + IntToHex(level, 2), fmOpenWrite or fmCreate);
              ptr     := PByte(TFriendlyImage(FImage).GetLevelAddress(level));
              LStream.Write(ptr^, FImage.LevelSizeInByte[level]);
              LStream.Destroy;
            end;
          end;

        end
        else
        begin // no SourceFile
          FImage := TDGLImage.Create;
          FImage.SetErrorImage;
          DGLSLogger.LogErrorFmt('Source file of texture "%s" image not found', [Self.Name]);
        end;
      end; // if bReadFromSource

    except
      on E: Exception do
      begin
        FImage.Free;
        FImage := TDGLImage.Create;
        FImage.SetErrorImage;
        if IsDesignTime then
          InformationDlg(Self.Name + ' - ' + E.ClassName + ': ' + E.Message)
        else
          DGLSLogger.LogError(Self.Name + ' - ' + E.ClassName + ': ' + E.Message);
      end;
    end;
  end; // of not Assigned
end;

procedure TDGLTexture.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name                 := ReadString;
      FDefferedInit        := ReadBoolean;
      FInternalFormat      := TDGLInternalFormat(ReadInteger);
      FCompression         := TDGLTextureCompression(ReadInteger);
      FImageAlpha          := TDGLTextureImageAlpha(ReadInteger);
      FImageBrightness     := ReadFloat;
      FImageBrightness     := ReadFloat;
      FImageGamma          := ReadFloat;
      FHeightToNormalScale := ReadFloat;
      FSourceFile          := ReadString;
      FInternallyStored    := ReadBoolean;
      FMipGenMode          := TMipmapGenerationMode(ReadInteger);
      FUseStreaming        := ReadBoolean;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TDGLTexture.SetCompression(const AValue: TDGLTextureCompression);
begin
  if AValue <> FCompression then
  begin
    FCompression := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLTexture.SetImageAlpha(const AValue: TDGLTextureImageAlpha);
begin
  if FImageAlpha <> AValue then
  begin
    FImageAlpha := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TDGLTexture.SetImageBrightness(const AValue: Single);
begin
  if FImageBrightness <> AValue then
  begin
    FImageBrightness := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TDGLTexture.SetImageGamma(const AValue: Single);
begin
  if FImageGamma <> AValue then
  begin
    FImageGamma := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TDGLTexture.SetInternalFormat(const AValue: TDGLInternalFormat);
begin
  if AValue <> FInternalFormat then
  begin
    FInternalFormat := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TDGLTexture.SetInternallyStored(const AValue: Boolean);
begin
  if FInternallyStored <> AValue then
  begin
    FInternallyStored := AValue;
    if not AValue then
      FUseStreaming := AValue
    else
      FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TDGLTexture.SetMipGenMode(const AValue: TMipmapGenerationMode);
begin
  if FMipGenMode <> AValue then
  begin
    FMipGenMode := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TDGLTexture.SetNormalMapScale(const AValue: Single);
begin
  if AValue <> FHeightToNormalScale then
  begin
    FHeightToNormalScale := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLTexture.SetSourceFile(AValue: string);
begin
  FixPathDelimiter(AValue);
  if FSourceFile <> AValue then
  begin
    FSourceFile   := AValue;
    FUseStreaming := False;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TDGLTexture.SetUseStreaming(const AValue: Boolean);
begin
  if AValue <> FUseStreaming then
  begin
    if AValue then
    begin
      if not Assigned(FImage) then
        exit;
      if FImage.LevelCount = 1 then
      begin
        if IsDesignTime then
          InformationDlg('Image must be more than one level');
        exit;
      end;
      FInternallyStored := True;
    end;
    FUseStreaming := AValue;
    NotifyChange(Self);
  end;
end;

function TDGLTexture.StoreBrightness: Boolean;
begin
  Result := (FImageBrightness <> 1.0);
end;

function TDGLTexture.StoreGamma: Boolean;
begin
  Result := (FImageGamma <> 1.0);
end;

function TDGLTexture.StoreNormalMapScale: Boolean;
begin
  Result := (FHeightToNormalScale <> cDefaultNormalMapScale);
end;

procedure TDGLTexture.UnApply(var ARci: TRenderContextInfo);
begin
  ARci.GLStates.ActiveTextureEnabled[FHandle.Target] := False;
end;

// UnApply
//
// ApplyMappingMode
//

//procedure TGLTexture.UnApplyMappingMode;
//begin
//  if MappingMode <> tmmUser then
//  begin
//    GL.Disable(GL_TEXTURE_GEN_S);
//    GL.Disable(GL_TEXTURE_GEN_T);
//    if GL.EXT_texture3D or GL.ARB_texture_cube_map then
//    begin
//      GL.Disable(GL_TEXTURE_GEN_R);
//      GL.Disable(GL_TEXTURE_GEN_Q);
//    end;
//  end;
//end;
//procedure TGLTexture.UnApply(var rci: TRenderContextInfo);
//begin
//  if not Disabled
//    and not rci.GLStates.ForwardContext then
//  begin
//    // Multisample image do not work with FFP
//    if FTextureHandle.Target in [ttNoShape, ttTexture2DMultisample, ttTexture2DMultisampleArray] then
//      exit;
//    with rci.GLStates do
//    begin
//      ActiveTexture := 0;
//      ActiveTextureEnabled[FTextureHandle.Target] := False;
//      if FTextureHandle.Target = ttTextureCube then
//        ResetGLTextureMatrix;
//    end;
//    UnApplyMappingMode;
//  end;
//end;

// ApplyAsTextureN
//

//procedure TDGLTexture.ApplyAsTextureN(n: Integer; var rci: TRenderContextInfo;
//  textureMatrix: PMatrix = nil);
//var
//  m: TMatrix;
//begin
// // if not Disabled then
// // begin
//    // Multisample image do not work with FFP
//    if (Handle.Target = ttTexture2DMultisample) or
//      (Handle.Target = ttTexture2DMultisampleArray) then exit;
//    with rci.GLStates do
//    begin
//      ActiveTexture := n - 1;
//      TextureBinding[n - 1, Handle.Target] := Handle;
//      ActiveTextureEnabled[Handle.Target] := True;
//      if Assigned(textureMatrix) then SetGLTextureMatrix(textureMatrix^)
//      else if Handle.Target = ttTextureCube then
//      begin
//        m := rci.PipelineTransformation.ModelViewMatrix;
//        NormalizeMatrix(m);
//        TransposeMatrix(m);
//        rci.GLStates.SetGLTextureMatrix(m);
//      end;
//      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
//      glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
//      ApplyMappingMode;
//      ActiveTexture := 0;
//
//    end;
////  end;
//end;

// UnApplyAsTextureN
//

//procedure TDGLTexture.UnApplyAsTextureN(n: Integer; var rci: TRenderContextInfo;reloadIdentityTextureMatrix: boolean);
//begin
//    // Multisample image do not work with FFP
//    if (Handle.Target = ttTexture2DMultisample) or
//      (Handle.Target = ttTexture2DMultisampleArray) then
//      exit;
//    with rci.GLStates do
//    begin
//      ActiveTexture := n - 1;
//      ActiveTextureEnabled[Handle.Target] := False;
//      //UnApplyMappingMode;
//      if (Handle.Target = ttTextureCube) or reloadIdentityTextureMatrix then ResetGLTextureMatrix;
//      ActiveTexture := 0;
//    end;
//end;

// GetHandle
//

//function TDGLTexture.GetOpenGLHandle: TGLuint;
//var
//  target: TGLUInt;
//  LBinding: array[TDGLTextureTarget] of TGLuint;
//
//  procedure StoreBindings;
//  var
//    t: TDGLTextureTarget;
//  begin
//    with CurrentDGLContext.GLStates do
//    begin
//      if TextureBinding[ActiveTexture, Handle.Target] = Handle.Handle then
//        TextureBinding[ActiveTexture, Handle.Target] := 0;
//      for t := Low(TDGLTextureTarget) to High(TDGLTextureTarget) do
//        LBinding[t] := TextureBinding[ActiveTexture, t];
//    end;
//  end;
//
//  procedure RestoreBindings;
//  var
//    t: TDGLTextureTarget;
//  begin
//    with CurrentDGLContext.GLStates do
//      for t := Low(TDGLTextureTarget) to High(TDGLTextureTarget) do
//        TextureBinding[ActiveTexture, t] := LBinding[t];
//  end;
//
//begin
//  with CurrentDGLContext.GLStates do
//  begin
//    StoreBindings;
//    try
////      Result := AllocateHandle;
//      if Handle.IsDataNeedUpdate then
//      begin
//        Handle.NotifyDataUpdated;
//        // Check supporting
//        target := DecodeGLTextureTarget(Image.NativeTextureTarget);
//        if not IsTargetSupported(target) or not IsFormatSupported(Format) then
//        begin
//          SetTextureErrorImage;
//          target := GL_TEXTURE_2D;
//        end;
//        // Load images
//        if not dglCheckExtension('EXT_direct_state_access') then TextureBinding[ActiveTexture, Handle.Target] := Result;
//        PrepareImage(target);
//      end;
//    finally
//      RestoreBindings;
//    end;
//  end;
//end;

// OpenGLTextureFormat
//

//function TDGLTexture.OpenGLTextureFormat: Integer;
//var
//  texComp: TDGLTextureCompression;
//begin
//  if dglCheckExtension('ARB_texture_compression') then
//  begin
//    if Compression = tcDefault then
//      if vDefaultTextureCompression = tcDefault then
//        texComp := tcNone
//      else
//        texComp := vDefaultTextureCompression
//    else
//      texComp := Compression;
//  end
//  else
//    texComp := tcNone;
//
////  if IsFloatType then
////    texComp := tcNone; // no compression support for float_type
//
//  if (texComp <> tcNone) and (Format <= tfNormalMap) then
//    with CurrentDGLContext.GLStates do
//    begin
//      case texComp of
//        tcStandard: TextureCompressionHint := hintDontCare;
//        tcHighQuality: TextureCompressionHint := hintNicest;
//        tcHighSpeed: TextureCompressionHint := hintFastest;
//      else
//        Assert(False);
//      end;
//      Result := CompressedInternalFormatToOpenGL(Format);
//    end
//  else
//    Result := InternalFormatToOpenGLFormat(Format);
//end;

procedure TDGLTexture.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    WriteInteger(Integer(FInternalFormat));
    WriteInteger(Integer(FCompression));
    WriteInteger(Integer(FImageAlpha));
    WriteFloat(FImageBrightness);
    WriteFloat(FImageBrightness);
    WriteFloat(FImageGamma);
    WriteFloat(FHeightToNormalScale);
    WriteString(FSourceFile);
    WriteBoolean(FInternallyStored);
    WriteInteger(Integer(FMipGenMode));
    WriteBoolean(FUseStreaming);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ----------------
{ TDGLTextureProperties }
{$IFDEF GLS_REGION}{$REGION 'TDGLTextureProperties'}{$ENDIF}
procedure TDGLTextureProperties.Apply(var ARci: TRenderContextInfo);
var
  glTarget: TGLEnum;
begin
  if Assigned(FLibTexture) then
    begin
      FLibTexture.FApplicableSampler := FLibSampler;
      FLibTexture.Apply(ARci);

      // Apply swizzling if possible
      glTarget := DecodeGLTextureTarget(FLibTexture.Shape);
      if dglCheckExtension('ARB_texture_swizzle') or dglCheckExtension('EXT_texture_swizzle') then
      begin
        if FSwizzling.FSwizzles.r <> FLibTexture.FSwizzles.r then
        begin
          FLibTexture.FSwizzles.r := FSwizzling.FSwizzles.r;
          glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_R, cTextureSwizzle[FSwizzling.FSwizzles.r]);
        end;
        if FSwizzling.FSwizzles.g <> FLibTexture.FSwizzles.g then
        begin
          FLibTexture.FSwizzles.g := FSwizzling.FSwizzles.g;
          glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_G, cTextureSwizzle[FSwizzling.FSwizzles.g]);
        end;
        if FSwizzling.FSwizzles.b <> FLibTexture.FSwizzles.b then
        begin
          FLibTexture.FSwizzles.b := FSwizzling.FSwizzles.b;
          glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_B, cTextureSwizzle[FSwizzling.FSwizzles.b]);
        end;
        if FSwizzling.FSwizzles.a <> FLibTexture.FSwizzles.a then
        begin
          FLibTexture.FSwizzles.a := FSwizzling.FSwizzles.a;
          glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_A, cTextureSwizzle[FSwizzling.FSwizzles.a]);
        end;
      end;

      if Assigned(FLibSampler) then
      begin
        if FLibSampler.IsValid then
          FLibSampler.Apply(ARci)
        else if FLibTexture.FLastSampler <> FLibSampler then
        begin
          // Sampler object not supported, lets use texture states
          glTexParameterfv(glTarget, GL_TEXTURE_BORDER_COLOR, FLibSampler.BorderColor.AsAddress);
          glTexParameteri(glTarget, GL_TEXTURE_WRAP_S, cTextureWrapMode[FLibSampler.WrapX]);
          glTexParameteri(glTarget, GL_TEXTURE_WRAP_T, cTextureWrapMode[FLibSampler.WrapY]);
          glTexParameteri(glTarget, GL_TEXTURE_WRAP_R, cTextureWrapMode[FLibSampler.WrapZ]);
          glTexParameterf(glTarget, GL_TEXTURE_LOD_BIAS, FLibSampler.LodBias + FLibSampler.FLODBiasFract);
          glTexParameteri(glTarget, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FLibSampler.MinFilter]);
          glTexParameteri(glTarget, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FLibSampler.MagFilter]);

          if dglCheckExtension('EXT_texture_filter_anisotropic') then
          begin
            if FLibSampler.FilteringQuality = tfAnisotropic then
              glTexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT, CurrentDGLContext.GLStates.MaxTextureAnisotropy)
            else
              glTexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
          end;

          glTexParameteri(glTarget, GL_TEXTURE_COMPARE_MODE, cTextureCompareMode[FLibSampler.CompareMode]);
          glTexParameteri(glTarget, GL_TEXTURE_COMPARE_FUNC, cGLComparisonFunctionToGLEnum[FLibSampler.CompareFunc]);

          if dglCheckExtension('EXT_texture_sRGB_decode') then
          begin
            if FLibSampler.sRGB_Encode then
              glTexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT, GL_DECODE_EXT)
            else
              glTexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT, GL_SKIP_DECODE_EXT);
          end;

          FLibTexture.FLastSampler := FLibSampler;
        end;
      end;

//      if not FTextureMatrixIsIdentity and (MappingMode = tmmUser) then
//        ARci.GLStates.SeTextureMatrix(FTextureMatrix);

//      if ARci.currentMaterialLevel < mlSM3 then
//      begin
//        glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
//        ApplyMappingMode;
//        if ARci.currentMaterialLevel = mlFixedFunction then
//          XGL.MapTexCoordToMain;
//      end;
    end;
end;

//function TDGLTextureProperties.TextureImageRequiredMemory: Integer;
//var
//  w, h, e, levelSize: Integer;
//begin
//  Assert(not(FLibTexture is TDGLTexture),'Texture Image Required Memory, could only be calculated with a TDGLTexture Object');
//  Abort;
//
//  if FRequiredMemorySize < 0 then
//  begin
//    if IsCompressedFormat(Format) then
//    begin
//      w := (TDGLTexture(FLibTexture).Image.Width + 3) div 4;
//      h := (TDGLTexture(FLibTexture).Image.Height + 3) div 4;
//    end
//    else
//    begin
//      w := TDGLTexture(FLibTexture).Image.Width;
//      h := TDGLTexture(FLibTexture).Image.Height;
//    end;
//
//    e := GetTextureElementSize(FLibTexture.Format);
//    FRequiredMemorySize := w * h * e;
//    if TDGLTexture(FLibTexture).Image.Depth > 0 then
//      FRequiredMemorySize := FRequiredMemorySize * TDGLTexture(FLibTexture).Image.Depth;
//
//    if not (FLibSampler.MinFilter in [miNearest, miLinear]) then
//    begin
//      levelSize := FRequiredMemorySize;
//      while e < levelSize do
//      begin
//        levelSize := levelSize div 4;
//        FRequiredMemorySize := FRequiredMemorySize + levelSize;
//      end;
//    end;
//
//    if TDGLTexture(FLibTexture).Image.NativeTextureTarget = ttTextureCube then
//      FRequiredMemorySize := FRequiredMemorySize * 6;
//  end;
//  Result := FRequiredMemorySize;
//end;

//procedure TDGLTextureProperties.ApplyMappingMode;
//var
//  R_Dim: Boolean;
//begin
//    R_Dim := dglCheckExtension('ARB_texture_cube_map') or dglCheckExtension('EXT_texture3D');
//
//    case MappingMode of
//
//      tmmUser:
//        ; // nothing to do, but checked first (common case)
//
//      tmmObjectLinear:
//        begin
//          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
//          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
//          glTexGenfv(GL_S, GL_OBJECT_PLANE, @MappingSCoordinates.DirectVector);
//          glTexGenfv(GL_T, GL_OBJECT_PLANE, @MappingTCoordinates.DirectVector);
//          glEnable(GL_TEXTURE_GEN_S);
//          glEnable(GL_TEXTURE_GEN_T);
//
//          if R_Dim then
//          begin
//            glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
//            glTexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
//            glTexGenfv(GL_R, GL_OBJECT_PLANE, @MappingRCoordinates.DirectVector);
//            glTexGenfv(GL_Q, GL_OBJECT_PLANE, @MappingQCoordinates.DirectVector);
//            glEnable(GL_TEXTURE_GEN_R);
//            glEnable(GL_TEXTURE_GEN_Q);
//          end;
//        end;
//
//      tmmEyeLinear:
//        begin
//          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
//          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
//          // specify planes in eye space, not world space
//          glMatrixMode(GL_MODELVIEW);
//          glPushMatrix;
//          glLoadIdentity;
//          glTexGenfv(GL_S, GL_EYE_PLANE, @MappingSCoordinates.DirectVector);
//          glTexGenfv(GL_T, GL_EYE_PLANE, @MappingTCoordinates.DirectVector);
//          glEnable(GL_TEXTURE_GEN_S);
//          glEnable(GL_TEXTURE_GEN_T);
//          if R_Dim then
//          begin
//            glTexGenfv(GL_R, GL_EYE_PLANE, @MappingRCoordinates.DirectVector);
//            glTexGenfv(GL_Q, GL_EYE_PLANE, @MappingQCoordinates.DirectVector);
//            glEnable(GL_TEXTURE_GEN_R);
//            glEnable(GL_TEXTURE_GEN_Q);
//          end;
//          glPopMatrix;
//          //CurrentDGLContext.Transformation.Pop;
//        end;
//
//      tmmSphere:
//        begin
//          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
//          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
//          glEnable(GL_TEXTURE_GEN_S);
//          glEnable(GL_TEXTURE_GEN_T);
//        end;
//
//      tmmCubeMapReflection, tmmCubeMapCamera:
//        if R_Dim then
//        begin
//          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
//          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
//          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
//          glEnable(GL_TEXTURE_GEN_S);
//          glEnable(GL_TEXTURE_GEN_T);
//          glEnable(GL_TEXTURE_GEN_R);
//        end;
//
//      tmmCubeMapNormal, tmmCubeMapLight0:
//        if R_Dim then
//        begin
//          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
//          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
//          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
//          glEnable(GL_TEXTURE_GEN_S);
//          glEnable(GL_TEXTURE_GEN_T);
//          glEnable(GL_TEXTURE_GEN_R);
//        end;
//    end;
//  //end;
//end;

procedure TDGLTextureProperties.Assign(Source: TPersistent);
var
  LTexProp: TDGLTextureProperties;
begin
  if Source is TDGLTextureProperties then
  begin
    LTexProp       := TDGLTextureProperties(Source);
    TextureName := LTexProp.TextureName;
    SamplerName := LTexProp.SamplerName;
    TextureOffset.Assign(LTexProp.TextureOffset);
    TextureScale.Assign(LTexProp.TextureScale);
    FTextureRotate := LTexProp.TextureRotate;
    FEnvColor.Assign(LTexProp.EnvColor);
    FMappingMode := LTexProp.MappingMode;
    MappingSCoordinates.Assign(LTexProp.MappingSCoordinates);
    MappingTCoordinates.Assign(LTexProp.MappingTCoordinates);
    MappingRCoordinates.Assign(LTexProp.MappingRCoordinates);
    MappingQCoordinates.Assign(LTexProp.MappingQCoordinates);
  end;
  inherited;
end;

procedure TDGLTextureProperties.CalculateTextureMatrix;
begin
  if not(Assigned(FTextureOffset) or Assigned(FTextureScale) or StoreTextureRotate) then
  begin
    FTextureMatrixIsIdentity := True;
    exit;
  end;

  if TextureOffset.Equals(NullHmgVector) and TextureScale.Equals(XYZHmgVector) and not StoreTextureRotate then
    FTextureMatrixIsIdentity := True
  else
  begin
    FTextureMatrixIsIdentity := False;
    FTextureMatrix           := CreateScaleAndTranslationMatrix(TextureScale.AsVector, TextureOffset.AsVector);
    if StoreTextureRotate then
      FTextureMatrix := MatrixMultiply(FTextureMatrix, CreateRotationMatrixZ(DegToRad(FTextureRotate)));
  end;
  FTextureOverride := False;
  NotifyChange(Self);
end;

constructor TDGLTextureProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FTextureRotate := 0;
  FMappingMode   := tmmUser;
  FTextureMatrix := IdentityHmgMatrix;
  FEnabled       := False;
  FSwizzling     := TDGLTextureSwizzling.Create(Self);
  FEnvColor      := TDGLColor.CreateInitialized(Self, clrTransparent);
end;

destructor TDGLTextureProperties.Destroy;
begin
  if Assigned(FLibSampler) then
    FLibSampler.UnregisterUser(Self);
  if Assigned(FLibTexture) then
    FLibTexture.UnregisterUser(Self);
  FTextureOffset.Free;
  FTextureScale.Free;
  FMapSCoordinates.Free;
  FMapTCoordinates.Free;
  FMapRCoordinates.Free;
  FMapQCoordinates.Free;
  FSwizzling.Destroy;
  FEnvColor.Destroy;
  inherited;
end;

function TDGLTextureProperties.GetLibSamplerName: TDGLMaterialComponentName;
begin
  if Assigned(FLibSampler) then
    Result := FLibSampler.Name
  else
    Result := '';
end;

function TDGLTextureProperties.GetLibTextureName: TDGLMaterialComponentName;
begin
  if Assigned(FLibTexture) then
    Result := FLibTexture.Name
  else
    Result := '';
end;

function TDGLTextureProperties.GetMappingQCoordinates: TDGLCoordinates4;
begin
  if not Assigned(FMapQCoordinates) then
    FMapQCoordinates := TDGLCoordinates4.CreateInitialized(Self, WHmgVector, csVector);
  Result             := FMapQCoordinates;
end;

function TDGLTextureProperties.GetMappingRCoordinates: TDGLCoordinates4;
begin
  if not Assigned(FMapRCoordinates) then
    FMapRCoordinates := TDGLCoordinates4.CreateInitialized(Self, ZHmgVector, csVector);
  Result             := FMapRCoordinates;
end;

function TDGLTextureProperties.GetMappingSCoordinates: TDGLCoordinates4;
begin
  if not Assigned(FMapSCoordinates) then
    FMapSCoordinates := TDGLCoordinates4.CreateInitialized(Self, XHmgVector, csVector);
  Result             := FMapSCoordinates;
end;

function TDGLTextureProperties.GetMappingTCoordinates: TDGLCoordinates4;
begin
  if not Assigned(FMapTCoordinates) then
    FMapTCoordinates := TDGLCoordinates4.CreateInitialized(Self, YHmgVector, csVector);
  Result             := FMapTCoordinates;
end;

function TDGLTextureProperties.GetTextureOffset: TDGLCoordinates;
begin
  if not Assigned(FTextureOffset) then
    FTextureOffset := TDGLCoordinates3.CreateInitialized(Self, NullHmgVector, csPoint);
  Result           := FTextureOffset;
end;

function TDGLTextureProperties.GetTextureScale: TDGLCoordinates;
begin
  if not Assigned(FTextureScale) then
    FTextureScale := TDGLCoordinates3.CreateInitialized(Self, VectorMake(1, 1, 1, 1), csVector);
  Result          := FTextureScale;
end;

function TDGLTextureProperties.IsValid: Boolean;
begin
  if Assigned(FLibTexture) then
    Result := FLibTexture.IsValid
  else
    Result := False;
end;

procedure TDGLTextureProperties.Loaded;
begin
  SetLibTextureName(FLibTextureName);
  SetLibSamplerName(FLibSamplerName);
  CalculateTextureMatrix;
end;

procedure TDGLTextureProperties.Notification(Sender: TObject; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if Sender = FLibTexture then
      FLibTexture := nil
    else if Sender = FLibSampler then
      FLibSampler := nil;
  end;
end;

procedure TDGLTextureProperties.NotifyChange(Sender: TObject);
begin
  inherited;
  if (Sender = FTextureOffset) or (Sender = FTextureScale) then
    CalculateTextureMatrix;
  if (Sender = FLibSampler) and Assigned(FLibTexture) then
    FLibTexture.FLastSampler := nil;
end;

procedure TDGLTextureProperties.SetLibSamplerName(const AValue: TDGLMaterialComponentName);
var
  LSampler: TDGLTextureSampler;
begin
  if csLoading in GetMaterialLibrary.ComponentState then
  begin
    FLibSamplerName := AValue;
    exit;
  end;

  if Assigned(FLibSampler) then
  begin
    if FLibSampler.Name = AValue then
      exit;
    FLibSampler.UnregisterUser(Self);
    FLibSampler := nil;
  end;
  LSampler := TDGLMaterialLibrary(GetMaterialLibrary).Components.GetSamplerByName(AValue);
  if Assigned(LSampler) then
  begin
    LSampler.RegisterUser(Self);
    FLibSampler := LSampler;
  end;
  NotifyChange(Self);
end;

procedure TDGLTextureProperties.SetLibTextureName(const AValue: TDGLMaterialComponentName);
var
  LTexture: TDGLAbstractTexture;
begin
  if csLoading in GetMaterialLibrary.ComponentState then
  begin
    FLibTextureName := AValue;
    exit;
  end;

  if Assigned(FLibTexture) then
  begin
    if FLibTexture.Name = AValue then
      exit;
    FLibTexture.UnregisterUser(Self);
    FLibTexture := nil;
  end;

  LTexture := TDGLMaterialLibrary(GetMaterialLibrary).Components.GetTextureByName(AValue);

  if Assigned(LTexture) then
  begin
//    if LTexture is TGLFrameBufferAttachment then
//    begin
//      if TGLFrameBufferAttachment(LTexture).OnlyWrite then
//      begin
//        if IsDesignTime then
//          InformationDlg('Can not use write only attachment as texture')
//        else
//          DGLSLogger.LogErrorFmt('Attempt to use write only attachment "%s" as texture', [LTexture.Name]);
//        NotifyChange(Self);
//        exit;
//      end;
//    end;
    LTexture.RegisterUser(Self);
    FLibTexture := LTexture;
  end;
  NotifyChange(Self);
end;

procedure TDGLTextureProperties.SetMappingMode(const AValue: TDGLTextureMappingMode);
begin
  if AValue <> FMappingMode then
  begin
    FMappingMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLTextureProperties.SetMappingQCoordinates(const AValue: TDGLCoordinates4);
begin
  MappingQCoordinates.Assign(AValue);
end;

procedure TDGLTextureProperties.SetMappingRCoordinates(const AValue: TDGLCoordinates4);
begin
  MappingRCoordinates.Assign(AValue);
end;

procedure TDGLTextureProperties.SetMappingSCoordinates(const AValue: TDGLCoordinates4);
begin
  MappingSCoordinates.Assign(AValue);
end;

procedure TDGLTextureProperties.SetMappingTCoordinates(const AValue: TDGLCoordinates4);
begin
  MappingTCoordinates.Assign(AValue);
end;

procedure TDGLTextureProperties.SetSwizzling(const AValue: TDGLTextureSwizzling);
begin
  FSwizzling.Assign(AValue);
end;

procedure TDGLTextureProperties.SetTextureMatrix(const AValue: TMatrix);
begin
  FTextureMatrixIsIdentity := CompareMem(@AValue.V[0], @IdentityHmgMatrix.V[0], SizeOf(TMatrix));
  FTextureMatrix           := AValue;
  FTextureOverride         := True;
  NotifyChange(Self);
end;

procedure TDGLTextureProperties.SetTextureOffset(const AValue: TDGLCoordinates);
begin
  TextureOffset.Assign(AValue);
  CalculateTextureMatrix;
end;

procedure TDGLTextureProperties.SetTextureRotate(AValue: Single);
begin
  if AValue <> FTextureRotate then
  begin
    FTextureRotate := AValue;
    CalculateTextureMatrix;
    NotifyChange(Self);
  end;
end;

procedure TDGLTextureProperties.SetTextureScale(const AValue: TDGLCoordinates);
begin
  TextureScale.Assign(AValue);
  CalculateTextureMatrix;
end;

function TDGLTextureProperties.StoreMappingQCoordinates: Boolean;
begin
  if Assigned(FMapQCoordinates) then
    Result := not VectorEquals(FMapQCoordinates.AsVector, WHmgVector)
  else
    Result := False;
end;

function TDGLTextureProperties.StoreMappingRCoordinates: Boolean;
begin
  if Assigned(FMapRCoordinates) then
    Result := not VectorEquals(FMapRCoordinates.AsVector, ZHmgVector)
  else
    Result := False;
end;

function TDGLTextureProperties.StoreMappingSCoordinates: Boolean;
begin
  if Assigned(FMapSCoordinates) then
    Result := not VectorEquals(FMapSCoordinates.AsVector, XHmgVector)
  else
    Result := False;
end;

function TDGLTextureProperties.StoreMappingTCoordinates: Boolean;
begin
  if Assigned(FMapTCoordinates) then
    Result := not VectorEquals(FMapTCoordinates.AsVector, YHmgVector)
  else
    Result := False;
end;

function TDGLTextureProperties.StoreSwizzling: Boolean;
begin
  Result := FSwizzling.StoreSwizzle(0);
end;

function TDGLTextureProperties.StoreTextureOffset: Boolean;
begin
  Result := Assigned(FTextureOffset);
end;

function TDGLTextureProperties.StoreTextureRotate: Boolean;
begin
  Result := Abs(FTextureRotate) > EPSILON;
end;

function TDGLTextureProperties.StoreTextureScale: Boolean;
begin
  Result := Assigned(FTextureScale);
end;

procedure TDGLTextureProperties.SetEnvColor(const AValue: TDGLColor);
begin
  FEnvColor.Assign(AValue);
  NotifyChange(Self);
end;

procedure TDGLTextureProperties.UnApply(var ARci: TRenderContextInfo);
begin
  if Assigned(FLibTexture) then
  begin
    FLibTexture.UnApply(ARci);
    if Assigned(FLibSampler) then
      FLibSampler.UnApply(ARci);

//    if ARci.currentMaterialLevel < mlSM3 then
//    begin
//      if not FTextureMatrixIsIdentity and (MappingMode = tmmUser) then
//        ARci.GLStates.SeTDGLTextureMatrix(IdentityHmgMatrix);
//      UnApplyMappingMode;
//    end;
  end;
end;

//procedure TDGLTextureProperties.UnApplyMappingMode;
//begin
//  if MappingMode <> tmmUser then
//    begin
//      glDisable(GL_TEXTURE_GEN_S);
//      glDisable(GL_TEXTURE_GEN_T);
//      if dglCheckExtension('EXT_texture3D') or dglCheckExtension('ARB_texture_cube_map') then
//      begin
//        glDisable(GL_TEXTURE_GEN_R);
//        glDisable(GL_TEXTURE_GEN_Q);
//      end;
//    end;
//end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLAdvancedProperties }
{$IFDEF GLS_REGION}{$REGION 'TDGLAdvancedProperties'}{$ENDIF}

function TDGLAdvancedProperties.GetDiffuseLightMap:TDGLTextureProperties;
begin
  if not Assigned(FDiffuseLightMap) then
    FDiffuseLightMap:= TDGLTextureProperties.Create(Self);
  Result            := FDiffuseLightMap;
end;

function TDGLAdvancedProperties.GetAmbientMap:TDGLTextureProperties;
begin
  if not Assigned(FAmbientMap) then
    FDiffuseLightMap:= TDGLTextureProperties.Create(Self);
  Result            := FAmbientMap;
end;

function TDGLAdvancedProperties.GetSpecularMap:TDGLTextureProperties;
begin
  if not Assigned(FSpecularMap) then
    FDiffuseLightMap:= TDGLTextureProperties.Create(Self);
  Result            := FSpecularMap;
end;

function TDGLAdvancedProperties.GetBumpMapHeight:TDGLTextureProperties;
begin
  if not Assigned(FBumpMapHeight) then
    FDiffuseLightMap:= TDGLTextureProperties.Create(Self);
  Result            := FBumpMapHeight;
end;

function TDGLAdvancedProperties.GetBumpMapNormal:TDGLTextureProperties;
begin
  if not Assigned(FBumpMapNormal) then
    FDiffuseLightMap:= TDGLTextureProperties.Create(Self);
  Result            := FBumpMapNormal;
end;

function TDGLAdvancedProperties.GetAlphaMap:TDGLTextureProperties;
begin
  if not Assigned(FAlphaMap) then
    FAlphaMap:= TDGLTextureProperties.Create(Self);
  Result            := FAlphaMap;
end;

function TDGLAdvancedProperties.GetReflectionMap:TDGLTextureProperties;
begin
  if not Assigned(FReflectionMap) then
    FDiffuseLightMap:= TDGLTextureProperties.Create(Self);
  Result            := FReflectionMap;
end;

function TDGLAdvancedProperties.GetRefractionMap:TDGLTextureProperties;
begin
  if not Assigned(FRefractionMap) then
    FDiffuseLightMap:= TDGLTextureProperties.Create(Self);
  Result            := FRefractionMap;
end;

procedure TDGLAdvancedProperties.SetDiffuseLightMap(AValue: TDGLTextureProperties);
begin
  FDiffuseLightMap.Assign(AValue);
end;

procedure TDGLAdvancedProperties.SetAmbientMap(AValue: TDGLTextureProperties);
begin
  FAmbientMap.Assign(AValue);
end;

procedure TDGLAdvancedProperties.SetSpecularMap(AValue: TDGLTextureProperties);
begin
  FSpecularMap.Assign(AValue);
end;

procedure TDGLAdvancedProperties.SetBumpMapHeight(AValue: TDGLTextureProperties);
begin
  FBumpMapHeight.Assign(AValue);
end;

procedure TDGLAdvancedProperties.SetBumpMapNormal(AValue: TDGLTextureProperties);
begin
  FBumpMapNormal.Assign(AValue);
end;

procedure TDGLAdvancedProperties.SetAlphaMap(AValue: TDGLTextureProperties);
begin
  FAlphaMap.Assign(AValue);
end;

procedure TDGLAdvancedProperties.SetReflectionMap(AValue: TDGLTextureProperties);
begin
  FReflectionMap.Assign(AValue);
end;

procedure TDGLAdvancedProperties.SetRefractionMap(AValue: TDGLTextureProperties);
begin
  FRefractionMap.Assign(AValue);
end;

constructor TDGLAdvancedProperties.Create(AOwner: TPersistent);
begin
  inherited;
end;

destructor TDGLAdvancedProperties.Destroy;
begin
  if Assigned(FDiffuseLightMap) then FreeAndNil(FDiffuseLightMap);
  if Assigned(FAmbientMap) then FreeAndNil(FAmbientMap);
  if Assigned(FSpecularMap) then FreeAndNil(FSpecularMap);
  if Assigned(FBumpMapHeight) then FreeAndNil(FBumpMapHeight);
  if Assigned(FBumpMapNormal) then FreeAndNil(FBumpMapNormal);
  if Assigned(FReflectionMap) then FreeAndNil(FReflectionMap);
  if Assigned(FRefractionMap) then FreeAndNil(FRefractionMap);
  inherited;
end;

procedure TDGLAdvancedProperties.Assign(Source: TPersistent);
var
  LFFP: TDGLAdvancedProperties;
begin
  if Source is TDGLAdvancedProperties then
  begin
    LFFP := TDGLAdvancedProperties(Source);
    if Assigned(LFFP.FDiffuseLightMap) then
      DiffuseLightMap.Assign(LFFP.DiffuseLightMap)
    else
      FreeAndNil(FDiffuseLightMap);

    if Assigned(LFFP.FAmbientMap) then
      AmbientMap.Assign(LFFP.AmbientMap)
    else
      FreeAndNil(FAmbientMap);

    if Assigned(LFFP.FSpecularMap) then
      SpecularMap.Assign(LFFP.SpecularMap)
    else
      FreeAndNil(FSpecularMap);

    if Assigned(LFFP.FBumpMapHeight) then
      BumpMapHeight.Assign(LFFP.BumpMapHeight)
    else
      FreeAndNil(FBumpMapHeight);

    if Assigned(LFFP.FBumpMapNormal) then
      BumpMapNormal.Assign(LFFP.BumpMapNormal)
    else
      FreeAndNil(FBumpMapNormal);

    if Assigned(LFFP.FReflectionMap) then
      ReflectionMap.Assign(LFFP.ReflectionMap)
    else
      FreeAndNil(FReflectionMap);

    if Assigned(LFFP.FRefractionMap) then
      RefractionMap.Assign(LFFP.RefractionMap)
    else
      FreeAndNil(FRefractionMap);

    NotifyChange(Self);
  end;
  inherited;
end;


{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ----------------
{ TDGLFrameBufferAttachment }
{$IFDEF GLS_REGION}{$REGION 'TDGLFrameBufferAttachment'}{$ENDIF}

procedure TDGLFrameBufferAttachment.Apply(var ARci: TRenderContextInfo);
begin
  if FIsValid and not FOnlyWrite then
  begin
    // Just bind
    with ARci.GLStates do
    begin
      ActiveTextureEnabled[FHandle.Target]          := True;
      TextureBinding[ActiveTexture, FHandle.Target] := FHandle.Handle;
    end;
  end
  else
    ARci.GLStates.TextureBinding[ARci.GLStates.ActiveTexture, FHandle.Target] := 0;
end;

procedure TDGLFrameBufferAttachment.Assign(Source: TPersistent);
var
  LAttachment: TDGLFrameBufferAttachment;
begin
  if Source is TDGLFrameBufferAttachment then
  begin
    LAttachment           := TDGLFrameBufferAttachment(Source);
    FLayered              := LAttachment.Layered;
    FCubeMap              := LAttachment.CubeMap;
    FSamples              := LAttachment.Samples;
    FOnlyWrite            := LAttachment.OnlyWrite;
    FFixedSamplesLocation := LAttachment.FixedSamplesLocation;
    FWidth                := LAttachment.InternalWidth;
    FHeight               := LAttachment.InternalHeight;
    FDepth                := LAttachment.InternalDepth;
    FInternalFormat       := LAttachment.InternalFormat;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TDGLFrameBufferAttachment.Create(AOwner: TDGLXCollection);
begin
  inherited;
  FDefferedInit                 := False;
  FHandle                       := TDGLTextureHandle.Create;
  FHandle.OnPrepare             := DoOnPrepare;
  FRenderBufferHandle           := TDGLRenderbufferHandle.Create;
  FRenderBufferHandle.OnPrepare := DoOnPrepare;
  FInternalFormat               := tfRGBA8;
  FWidth                        := 256;
  FHeight                       := 256;
  FDepth                        := 0;
  FSamples                      := -1;
  FLayered                      := False;
  FCubeMap                      := False;
  FOnlyWrite                    := False;
  FFixedSamplesLocation         := False;
  Name                          := TDGLMatLibComponents(AOwner).MakeUniqueName('FBO_Attachment');
end;

destructor TDGLFrameBufferAttachment.Destroy;
begin
  FHandle.Destroy;
  FRenderBufferHandle.Destroy;
  inherited;
end;

procedure TDGLFrameBufferAttachment.DoOnPrepare(Sender: TDGLContext);
var
  LTarget:                     TDGLTextureTarget;
  w, h, d, S, level, MaxLevel: Integer;
  glTarget, glFormat, glFace:  TGLEnum;
begin
  if IsDesignTime and FDefferedInit then
    exit;

  FHandle.AllocateHandle;
  FRenderBufferHandle.AllocateHandle;
  if not(FHandle.IsDataNeedUpdate or FRenderBufferHandle.IsDataNeedUpdate) then
    exit;

  // Target

  if FSamples < 0 then
  begin
    LTarget := ttTexture2D;
    if FHeight = 1 then
      LTarget := ttTexture1D;
    if FCubeMap then
      LTarget := ttTextureCube;
    if FDepth > 0 then
      LTarget := ttTexture3D;
    if FLayered then
    begin
      if FDepth < 2 then
        LTarget := ttTexture1DArray
      else
        LTarget := ttTexture2DArray;
      if FCubeMap then
        LTarget := ttTextureCubeArray;
    end;
  end
  else
  begin
    if FDepth > 0 then
      LTarget := ttTexture2DMultisampleArray
    else
      LTarget := ttTexture2DMultisample;
  end;

  // Check target support
  if FOnlyWrite and (LTarget = ttTexture2DMultisample) and not dglCheckExtension('EXT_framebuffer_multisample') then
  begin
    FIsValid := False;
    exit;
  end;
  if not IsTargetSupported(LTarget) then
  begin
    FIsValid := False;
    exit;
  end;

  // Adjust dimension
  w := FWidth;
  h := FHeight;
  d := FDepth;
  S := FSamples;
  if FCubeMap then
  begin
    if w > Integer(Sender.GLStates.MaxCubeTextureSize) then
      w := Sender.GLStates.MaxCubeTextureSize;
    h   := w;
    if FLayered then
    begin
      if d < 6 then
        d := 6
      else if (d mod 6) > 0 then
        d := 6 * (d div 6 + 1);
    end;
  end
  else if w > Integer(Sender.GLStates.MaxTextureSize) then
    w := Sender.GLStates.MaxTextureSize;
  if h > Integer(Sender.GLStates.MaxTextureSize) then
    h := Sender.GLStates.MaxTextureSize;
  if FLayered then
  begin
    if d > Integer(Sender.GLStates.MaxArrayTextureSize) then
      d := Sender.GLStates.MaxArrayTextureSize;
  end
  else if d > Integer(Sender.GLStates.Max3DTextureSize) then
    d := Sender.GLStates.Max3DTextureSize;
  if (S > -1) and (S > Integer(Sender.GLStates.MaxSamples)) then
    S := Sender.GLStates.MaxSamples;

  glTarget := DecodeGLTextureTarget(LTarget);

  if (FHandle.Target <> LTarget) and (FHandle.Target <> ttNoShape) then
  begin
    FHandle.DestroyHandle;
    FHandle.AllocateHandle;
  end;
  FHandle.Target := LTarget;

  glFormat := InternalFormatToOpenGLFormat(FInternalFormat);

  if FOnlyWrite and ((LTarget = ttTexture2D) or (LTarget = ttTexture2DMultisample)) and FRenderBufferHandle.IsSupported then
  begin
    if LTarget = ttTexture2D then
      FRenderBufferHandle.SetStorage(glFormat, w, h)
    else
      FRenderBufferHandle.SetStorageMultisample(glFormat, S, w, h);
  end
  else
    with Sender do
    begin
      GLStates.ActiveTextureEnabled[FHandle.Target]                   := True;
      GLStates.TextureBinding[GLStates.ActiveTexture, FHandle.Target] := FHandle.Handle;
      MaxLevel                                                        := CalcTextureLevelNumber(LTarget, w, h, d);

      case glTarget of

        GL_TEXTURE_1D:
          for level := 0 to MaxLevel - 1 do
          begin
            glTexImage1D(glTarget, level, glFormat, w, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;

        GL_TEXTURE_2D:
          for level := 0 to MaxLevel - 1 do
          begin
            glTexImage2D(glTarget, level, glFormat, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
            Div2(w);
            Div2(h);
          end;

        GL_TEXTURE_RECTANGLE:
          begin
            glTexImage2D(glTarget, 0, glFormat, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
          end;

        GL_TEXTURE_3D:
          for level := 0 to MaxLevel - 1 do
          begin
            glTexImage3D(glTarget, level, glFormat, w, h, d, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
            Div2(w);
            Div2(h);
            Div2(d);
          end;

        GL_TEXTURE_CUBE_MAP:
          for level := 0 to MaxLevel - 1 do
          begin
            for glFace := GL_TEXTURE_CUBE_MAP_POSITIVE_X to GL_TEXTURE_CUBE_MAP_NEGATIVE_Z do
              glTexImage2D(glFace, level, glFormat, w, w, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;

        GL_TEXTURE_1D_ARRAY:
          for level := 0 to MaxLevel - 1 do
          begin
            glTexImage2D(glTarget, level, glFormat, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;

        GL_TEXTURE_2D_ARRAY:
          for level := 0 to MaxLevel - 1 do
          begin
            glTexImage3D(glTarget, level, glFormat, w, h, d, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
            Div2(w);
            Div2(h);
          end;

        GL_TEXTURE_CUBE_MAP_ARRAY:
          for level := 0 to MaxLevel - 1 do
          begin
            glTexImage3D(glTarget, level, glFormat, w, w, d, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;
      end; // of case

      GLStates.ActiveTextureEnabled[FHandle.Target] := False;
      FOnlyWrite                                    := False;
    end; // of texture

  if glGetError <> GL_NO_ERROR then
  begin
    ClearOpenGLError;
    DGLSLogger.LogErrorFmt('Unable to create attachment "%s"', [Self.Name]);
    exit;
  end
  else
    FIsValid := True;

  FHandle.NotifyDataUpdated;
  FRenderBufferHandle.NotifyDataUpdated;
end;

class function TDGLFrameBufferAttachment.FriendlyName: string;
begin
  Result := 'Frame Buffer Object Attachment';
end;

procedure TDGLFrameBufferAttachment.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  FRenderBufferHandle.NotifyChangesOfData;
  inherited;
end;

procedure TDGLFrameBufferAttachment.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name                  := ReadString;
      FDefferedInit         := ReadBoolean;
      FLayered              := ReadBoolean;
      FCubeMap              := ReadBoolean;
      FSamples              := ReadInteger;
      FOnlyWrite            := ReadBoolean;
      FFixedSamplesLocation := ReadBoolean;
      FWidth                := ReadInteger;
      FHeight               := ReadInteger;
      FDepth                := ReadInteger;
      FInternalFormat       := TDGLInternalFormat(ReadInteger);
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TDGLFrameBufferAttachment.SetCubeMap(AValue: Boolean);
begin
  if FCubeMap <> AValue then
  begin
    FCubeMap := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLFrameBufferAttachment.SetDepth(AValue: Integer);
begin
  if FDepth < 0 then
    FDepth := 0
  else if FDepth > 256 then
    FDepth := 256;
  if FDepth <> AValue then
  begin
    FDepth := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLFrameBufferAttachment.SetFixedSamplesLocation(AValue: Boolean);
begin
  if FFixedSamplesLocation <> AValue then
  begin
    FFixedSamplesLocation := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLFrameBufferAttachment.SetHeight(AValue: Integer);
begin
  if FHeight < 1 then
    FHeight := 1
  else if FHeight > 8192 then
    FHeight := 8192;
  if FHeight <> AValue then
  begin
    FHeight := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLFrameBufferAttachment.SetInternalFormat(const AValue: TDGLInternalFormat);
begin
  if FInternalFormat <> AValue then
  begin
    FInternalFormat := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLFrameBufferAttachment.SetLayered(AValue: Boolean);
begin
  if FLayered <> AValue then
  begin
    FLayered := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLFrameBufferAttachment.SetOnlyWrite(AValue: Boolean);
begin
  if FOnlyWrite <> AValue then
  begin
    if AValue and ((FDepth > 0) or FLayered or FFixedSamplesLocation or FCubeMap) then
      exit;
    FOnlyWrite := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLFrameBufferAttachment.SetSamples(AValue: Integer);
begin
  if AValue < -1 then
    AValue := -1;
  if FSamples <> AValue then
  begin
    FSamples := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLFrameBufferAttachment.SetWidth(AValue: Integer);
begin
  if FWidth < 1 then
    FWidth := 1
  else if FWidth > 8192 then
    FWidth := 8192;
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLFrameBufferAttachment.UnApply(var ARci: TRenderContextInfo);
begin
  ARci.GLStates.ActiveTextureEnabled[FHandle.Target] := False;
end;

procedure TDGLFrameBufferAttachment.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    WriteBoolean(FLayered);
    WriteBoolean(FCubeMap);
    WriteInteger(FSamples);
    WriteBoolean(FOnlyWrite);
    WriteBoolean(FFixedSamplesLocation);
    WriteInteger(FWidth);
    WriteInteger(FHeight);
    WriteInteger(FDepth);
    WriteInteger(Integer(FInternalFormat));
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ----------------
{ TDGLBaseMaterialProperties }
{$IFDEF GLS_REGION}{$REGION 'TDGLBaseMaterialProperties'}{$ENDIF}

procedure TDGLBaseMaterialProperties.Apply(var ARci: TRenderContextInfo);
var ABlendingParameters : TDGLBlendingParameters;
begin
  with ARci.GLStates do
  begin
//    Disable(stColorMaterial);
    PolygonMode := FPolygonMode;

//    // Fixed functionality state
//      // Lighting switch
//      if (moNoLighting in MaterialOptions) or not ARci.bufferLighting then
//      begin
//        Disable(stLighting);
//        FFrontProperties.ApplyNoLighting(ARci, cmFront);
//      end
//      else
//      begin
//        Enable(stLighting);
//        FFrontProperties.Apply(ARci, cmFront);
//      end;
//
//      if FPolygonMode = pmLines then
//        Disable(stLineStipple);
//
//      // Fog switch
//      if (moIgnoreFog in MaterialOptions) or not ARci.bufferFog then
//        Disable(stFog)
//      else
//        Enable(stFog);


    // Apply FaceCulling and BackProperties (if needs be)
    case FFaceCulling of
      fcBufferDefault:
        begin
          if ARci.bufferFaceCull then
            Enable(stCullFace)
          else
            Disable(stCullFace);
          FaceProperties.Apply(ARci, cmBack);
        end;
      fcCull:
        Enable(stCullFace);
      fcNoCull:
        begin
          Disable(stCullFace);
          FaceProperties.Apply(ARci, cmBack);
        end;
    end;
    // note: Front + Back with different PolygonMode are no longer supported.
    // Currently state cache just ignores back facing mode changes, changes to
    // front affect both front + back PolygonMode

    // Apply Blending mode
    if not ARci.ignoreBlendingRequests then
      case FBlendingMode of
        bmOpaque:
          begin
            ABlendingParameters := TDGLBlendingParameters.Create(self);

//            Disable(stBlend);
//            Disable(stAlphaTest);
            ABlendingParameters.Apply(ARci);
            ABlendingParameters.Free;
          end;
        bmTransparency:
          begin
//            Enable(stBlend);
//            Enable(stAlphaTest);
//            SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
//            SetGLAlphaFunction(cfGreater, 0);
          end;
        bmAdditive:
          begin
//            Enable(stBlend);
//            Enable(stAlphaTest);
//            SetBlendFunc(bfSrcAlpha, bfOne);
//            SetGLAlphaFunction(cfGreater, 0);
          end;
        bmAlphaTest50:
          begin
//            Disable(stBlend);
//            Enable(stAlphaTest);
//            SetGLAlphaFunction(cfGEqual, 0.5);
          end;
        bmAlphaTest100:
          begin
//            Disable(stBlend);
//            Enable(stAlphaTest);
//            SetGLAlphaFunction(cfGEqual, 1.0);
          end;
        bmModulate:
          begin
//            Enable(stBlend);
//            Enable(stAlphaTest);
//            SetBlendFunc(bfDstColor, bfZero);
//            SetGLAlphaFunction(cfGreater, 0);
            FBlendingParams.Apply(ARci);
          end;
        bmCustom:
          begin
            FBlendingParams.Apply(ARci);
          end;
      end;

    // Apply depth properties
    if not ARci.ignoreDepthRequests then
      FDepthProperties.Apply(ARci);

    // Apply texturing
    if ARci.currentMaterialLevel = mlBaseMaterial then
    begin
      if FTexProp.Enabled and FTexProp.IsValid then
      begin
        ARci.GLStates.ActiveTexture := 0;
        FTexProp.Apply(ARci);
//        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
      end;
    end;

  end;
end;

procedure TDGLBaseMaterialProperties.Assign(Source: TPersistent);
var
  LFFP: TDGLBaseMaterialProperties;
begin
  if Source is TDGLBaseMaterialProperties then
  begin
    LFFP := TDGLBaseMaterialProperties(Source);
//    if Assigned(LFFP.FBackProperties) then
//      BackProperties.Assign(LFFP.BackProperties)
//    else
//      FreeAndNil(FBackProperties);
    FFaceProperties.Assign(LFFP.FFaceProperties);
    FAdvancedProperties.Assign(LFFP.FAdvancedProperties);
    FPolygonMode     := LFFP.FPolygonMode;
    FBlendingMode    := LFFP.FBlendingMode;
    //FMaterialOptions := LFFP.FMaterialOptions;
    FFaceCulling     := LFFP.FFaceCulling;
    FDepthProperties.Assign(LFFP.FDepthProperties);
    FTexProp.Assign(LFFP.FTexProp);
    FTextureMode := LFFP.TextureMode;
    NotifyChange(Self);
  end;
  inherited;
end;

function TDGLBaseMaterialProperties.Blended: Boolean;
begin
  Result := not(FBlendingMode in [bmOpaque, bmAlphaTest50, bmAlphaTest100, bmCustom]);
end;

constructor TDGLBaseMaterialProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FFaceProperties := TDGLFaceProperties.Create(Self);
  FAdvancedProperties := TDGLAdvancedProperties.Create(Self);
  FFaceCulling     := fcBufferDefault;
  FPolygonMode     := pmFill;
  FBlendingParams  := TDGLBlendingParameters.Create(Self);
  FDepthProperties := TDGLDepthProperties.Create(Self);
  FTexProp         := TDGLTextureProperties.Create(Self);
  FTextureMode     := tmDecal;
  FEnabled         := True;
end;

destructor TDGLBaseMaterialProperties.Destroy;
begin
  FFaceProperties.Destroy;
  FAdvancedProperties.Free;
  FDepthProperties.Destroy;
  FBlendingParams.Destroy;
  FTexProp.Destroy;
  inherited;
end;

//function TDGLBaseMaterialProperties.GetBackProperties: TDGLFaceProperties;
//begin
//  if not Assigned(FBackProperties) then
//    FBackProperties := TDGLFaceProperties.Create(Self);
//  Result            := FBackProperties;
//end;

procedure TDGLBaseMaterialProperties.SetAdvancedProperties(AValues: TDGLAdvancedProperties);
begin
  AdvancedProperties.Assign(AValues);
  NotifyChange(Self);
end;

procedure TDGLBaseMaterialProperties.SetBlendingMode(const AValue: TBlendingMode);
begin
  if AValue <> FBlendingMode then
  begin
    FBlendingMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseMaterialProperties.SetBlendingParams(const AValue: TDGLBlendingParameters);
begin
  FBlendingParams.Assign(AValue);
  NotifyChange(Self);
end;

procedure TDGLBaseMaterialProperties.SetDepthProperties(AValues: TDGLDepthProperties);
begin
  FDepthProperties.Assign(AValues);
  NotifyChange(Self);
end;

procedure TDGLBaseMaterialProperties.SetTexProp(AValue: TDGLTextureProperties);
begin
  FTexProp.Assign(AValue);
end;

procedure TDGLBaseMaterialProperties.SetTextureMode(AValue: TDGLTextureMode);
begin
  if AValue <> FTextureMode then
  begin
    FTextureMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseMaterialProperties.SetFaceCulling(const AValue: TFaceCulling);
begin
  if AValue <> FFaceCulling then
  begin
    FFaceCulling := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseMaterialProperties.SetFaceProperties(AValues: TDGLFaceProperties);
begin
  FFaceProperties.Assign(AValues);
  NotifyChange(Self);
end;

//procedure TDGLBaseMaterialProperties.SetMaterialOptions(const AValue: TMaterialOptions);
//begin
//  if AValue <> FMaterialOptions then
//  begin
//    FMaterialOptions := AValue;
//    NotifyChange(Self);
//  end;
//end;

procedure TDGLBaseMaterialProperties.SetPolygonMode(AValue: TPolygonMode);
begin
  if AValue <> FPolygonMode then
  begin
    FPolygonMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLBaseMaterialProperties.UnApply(var ARci: TRenderContextInfo);
begin
  if FTexProp.Enabled and FTexProp.IsValid then
    FTexProp.UnApply(ARci);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ----------------
{ TDGLMultitexturingProperties }
{$IFDEF GLS_REGION}{$REGION 'TDGLMultitexturingProperties'}{$ENDIF}

procedure TDGLMultitexturingProperties.Apply(var ARci: TRenderContextInfo);
var
  n: Integer;
//  U: Integer;
  LDir: TVector;
begin
  if FEnabled then
  begin
    if Assigned(FLibCombiner) and not FLibCombiner.FIsValid then exit;


//    U     := 0;
    for n := 0 to High(FTexProps) do
    begin
      if Assigned(FTexProps[n]) and FTexProps[n].Enabled then
      begin
        ARci.GLStates.ActiveTexture := n;
        FTexProps[n].Apply(ARci);
        if Ord(FLightDir) = n + 1 then
        begin
          LDir := ARci.GLStates.LightPosition[FLightSourceIndex];
          LDir := VectorTransform(LDir, ARci.PipelineTransformation.InvModelMatrix);
          NormalizeVector(LDir);
//          glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, @LDir);
        end;
//        U := U or (1 shl n);
      end;
    end;


    with ARci.GLStates do
    begin
      if Assigned(FLibCombiner) and (Length(FLibCombiner.FCommandCache) > 0) then
      begin
        for n := 0 to High(FLibCombiner.FCommandCache) do
        begin
          ActiveTexture := FLibCombiner.FCommandCache[n].ActiveUnit;
//          glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);
//          glTexEnvi(GL_TEXTURE_ENV, FLibCombiner.FCommandCache[n].Arg1, FLibCombiner.FCommandCache[n].Arg2);
        end;
      end;
//      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
      ActiveTexture := 0;
    end;

//    XGL.BeginUpdate;
//    if U > 3 then
//      XGL.MapTexCoordToArbitrary(U)
//    else if (FTexProps[0].Enabled) and (FTexProps[0].MappingMode = tmmUser) then
//      if FTexProps[1].MappingMode = tmmUser then
//        XGL.MapTexCoordToDual
//      else
//        XGL.MapTexCoordToMain
//    else if FTexProps[1].MappingMode = tmmUser then
//      XGL.MapTexCoordToSecond
//    else
//      XGL.MapTexCoordToMain;
//    XGL.EndUpdate;

  end;
end;

constructor TDGLMultitexturingProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FEnabled          := False;
  FTextureMode      := tmDecal;
  FLightDir         := l2eNone;
  FLightSourceIndex := 0;
end;

destructor TDGLMultitexturingProperties.Destroy;
begin
  if Assigned(FLibCombiner) then FLibCombiner.UnregisterUser(Self);

  FTexProps[0].Free;
  FTexProps[1].Free;
  FTexProps[2].Free;
  FTexProps[3].Free;
  inherited;
end;

function TDGLMultitexturingProperties.GetLibCombinerName: string;
begin
  if Assigned(FLibCombiner) then
    Result := FLibCombiner.Name
  else
    Result := '';
end;

function TDGLMultitexturingProperties.IsValid: Boolean;
var
  i: Integer;
begin
  Result := True;
  if Assigned(FLibCombiner) then Result := Result and FLibCombiner.IsValid;
  for i    := 0 to High(FTexProps) do
    if Assigned(FTexProps[i]) and FTexProps[i].FEnabled then
      Result := Result and FTexProps[i].IsValid;
end;

procedure TDGLMultitexturingProperties.Loaded;
var
  i: Integer;
begin
  SetLibCombinerName(FLibCombinerName);

  for i := 0 to High(FTexProps) do
    if Assigned(FTexProps[i]) then
      FTexProps[i].Loaded;
end;

procedure TDGLMultitexturingProperties.Notification(Sender: TObject; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if Sender = FLibCombiner then
      FLibCombiner := nil;
  end;
  inherited;
end;

procedure TDGLMultitexturingProperties.SetLibCombinerName(const AValue: string);
var
  LCombiner: TDGLTextureCombiner;
begin
  if csLoading in GetMaterialLibrary.ComponentState then
  begin
    FLibCombinerName := AValue;
    exit;
  end;

  if Assigned(FLibCombiner) then
  begin
    if FLibCombiner.Name = AValue then
      exit;
    FLibCombiner.UnregisterUser(Self);
    FLibCombiner := nil;
  end;
  LCombiner := TDGLMaterialLibrary(GetMaterialLibrary).Components.GetCombinerByName(AValue);
  if Assigned(LCombiner) then
  begin
    LCombiner.RegisterUser(Self);
    FLibCombiner := LCombiner;
  end;
  NotifyChange(Self);
end;

procedure TDGLMultitexturingProperties.SetLightSourceIndex(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0
  else if AValue > 7 then
    AValue          := 7;
  FLightSourceIndex := AValue;
end;

function TDGLMultitexturingProperties.GetTexProps(AIndex: Integer): TDGLTextureProperties;
begin
  if not Assigned(FTexProps[AIndex]) then
    FTexProps[AIndex] := TDGLTextureProperties.Create(Self);
  Result              := FTexProps[AIndex];
end;

procedure TDGLMultitexturingProperties.SetTexProps(AIndex: Integer; AValue: TDGLTextureProperties);
begin
  FTexProps[AIndex].Assign(AValue);
end;

procedure TDGLMultitexturingProperties.SetTextureMode(AValue: TDGLTextureMode);
begin
  if AValue <> FTextureMode then
  begin
    FTextureMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLMultitexturingProperties.UnApply(var ARci: TRenderContextInfo);
var
  n: Integer;
begin
  for n := 0 to High(FTexProps) do
  begin
    if FTexProps[n].Enabled then
    begin
      ARci.GLStates.ActiveTexture := n;
      FTexProps[n].UnApply(ARci);
    end;
  end;
  ARci.GLStates.ActiveTexture := 0;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ---------------
{ TDGLLibMaterial }
{$IFDEF GLS_REGION}{$REGION 'TDGLLibMaterial'}{$ENDIF}

procedure TDGLLibMaterial.Apply(var ARci: TRenderContextInfo);
//var
//  LevelReady:  array [TDGLMaterialLevel] of Boolean;
//  L, MaxLevel: TDGLMaterialLevel;
begin
 // if Assigned(FNextPass) then
//  begin
//    FNextPass := nil;
//    exit;
//  end;
//
//  FHandle.AllocateHandle;
//  if FHandle.IsDataNeedUpdate then
//  begin
//    // Other value than mlAuto indicates a level failure
//    // Need remove deffered initialization and reinitialize used resources
//    if not IsDesignTime and (FSelectedLevel <> mlAuto) then RemoveDefferedInit;
//    // Level selection
//    LevelReady[mlBaseMaterial]  := FBaseMaterial.Enabled;
//    LevelReady[mlMultitexturing] := FMultitexturing.Enabled and FMultitexturing.IsValid;
////    LevelReady[mlSM3]            := FSM3.Enabled and FSM3.IsValid;
////    LevelReady[mlSM4]            := FSM4.Enabled and FSM4.IsValid;
////    LevelReady[mlSM5]            := FSM5.Enabled and FSM5.IsValid;
//
//    if FApplicableLevel = mlAuto then
//      MaxLevel := mlShaderModel5
//    else
//      MaxLevel := FApplicableLevel;
//
//    FSelectedLevel := mlAuto;
//    for L          := MaxLevel downto mlBaseMaterial do
//      if LevelReady[L] then
//      begin
//        FSelectedLevel := L;
//        break;
//      end;
//
//    FStoreAmalgamating := ARci.amalgamating;
//    ARci.amalgamating  := True;
//    FHandle.NotifyDataUpdated;
//  end;
//
//  ARci.GLStates.currentMaterialLevel := FSelectedLevel;
//
//  case FSelectedLevel of
//    mlAuto:
//      ; // No one level can be used. Worst case.
//
//    mlBaseMaterial:
//      begin
//        FBaseMaterial.Apply(ARci);
//      end;
//
//    mlMultitexturing:
//      begin
//        if LevelReady[mlBaseMaterial] then
//          FBaseMaterial.Apply(ARci);
//        FMultitexturing.Apply(ARci);
//      end;
//
//
//  end;
end;

procedure TDGLLibMaterial.Assign(Source: TPersistent);
var
  LMaterial: TDGLLibMaterial;
begin
  if Source is TDGLLibMaterial then
  begin
    LMaterial := TDGLLibMaterial(Source);
    FBaseMaterial.Assign(LMaterial.FBaseMaterial);
    FMultitexturing.Assign(LMaterial.FMultitexturing);
//    FSM3.Assign(LMaterial.FSM3);
//    FSM4.Assign(LMaterial.FSM4);
//    FSM5.Assign(LMaterial.FSM5);
//    FApplicableLevel := LMaterial.FApplicableLevel;
    NotifyChange(Self);
  end;
  inherited;
end;

function TDGLLibMaterial.Blended: Boolean;
begin
  Result := FBaseMaterial.Blended;
end;

constructor TDGLLibMaterial.Create(ACollection: TCollection);
begin
  inherited;
  FHandle            := TDGLVirtualHandle.Create;
  FHandle.OnAllocate := DoAllocate;
  FHandle.OnDestroy  := DoDeallocate;
  FHandle.OnPrepare  := DoOnPrepare;
//  FApplicableLevel   := mlAuto;
//  FSelectedLevel     := mlAuto;
  FBaseMaterial      := TDGLBaseMaterialProperties.Create(Self);
  FMultitexturing    := TDGLMultitexturingProperties.Create(Self);
//  FSM3               := TGLShaderModel3.Create(Self);
//  FSM4               := TGLShaderModel4.Create(Self);
//  FSM5               := TGLShaderModel5.Create(Self);
end;

//type
//  TGLFriendlyMaterial = class(TGLMaterial);

destructor TDGLLibMaterial.Destroy;
//var
//  i:     Integer;
//  LUser: TObject;
begin
  FHandle.Destroy;
  FBaseMaterial.Destroy;
  FMultitexturing.Destroy;
//  FSM3.Destroy;
//  FSM4.Destroy;
//  FSM5.Destroy;
//  for i := 0 to FUserList.Count - 1 do
//  begin
//    LUser := TObject(FUserList[i]);
//    if LUser is TGLMaterial then
//      TGLFriendlyMaterial(LUser).NotifyLibMaterialDestruction;
//  end;
  inherited;
end;

procedure TDGLLibMaterial.DoAllocate(Sender: TDGLVirtualHandle; var Handle: TGLUint);
begin
  Handle := 1;
end;

procedure TDGLLibMaterial.DoDeallocate(Sender: TDGLVirtualHandle; var Handle: TGLUint);
begin
  Handle := 0;
end;

procedure TDGLLibMaterial.DoOnPrepare(Sender: TDGLContext);
begin
end;

procedure TDGLLibMaterial.Loaded;
begin
  FBaseMaterial.FTexProp.Loaded;
  FMultitexturing.Loaded;
//  FSM3.Loaded;
//  FSM4.Loaded;
//  FSM5.Loaded;
end;

procedure TDGLLibMaterial.NotifyChange(Sender: TObject);
begin
  inherited;
  FHandle.NotifyChangesOfData;
end;

procedure TDGLLibMaterial.RemoveDefferedInit;
var
  i:  Integer;
//  ST: TGLShaderType;
begin
  if FBaseMaterial.FTexProp.Enabled then
  begin
    if Assigned(FBaseMaterial.FTexProp.FLibTexture) then FBaseMaterial.FTexProp.FLibTexture.FDefferedInit := False;
    if Assigned(FBaseMaterial.FTexProp.FLibSampler) then FBaseMaterial.FTexProp.FLibSampler.FDefferedInit := False;
  end;

  if FMultitexturing.Enabled then
  begin
    if Assigned(FMultitexturing.FLibCombiner) then
    begin
      FMultitexturing.FLibCombiner.FDefferedInit := False;
      for i                                      := 0 to 3 do
        if Assigned(FMultitexturing.FTexProps[i]) then
          with FMultitexturing.FTexProps[i] do
          begin
            if Assigned(FLibTexture) then
              FLibTexture.FDefferedInit := False;
            if Assigned(FLibSampler) then
              FLibSampler.FDefferedInit := False;
          end;
    end;
  end;

//  if FSM3.Enabled then
//  begin
//    for ST := Low(TGLShaderType) to High(TGLShaderType) do
//      if Assigned(FSM3.FShaders[ST]) then
//        FSM3.FShaders[ST].FDefferedInit := False;
//  end;
//
//  if FSM4.Enabled then
//  begin
//    for ST := Low(TGLShaderType) to High(TGLShaderType) do
//      if Assigned(FSM4.FShaders[ST]) then
//        FSM4.FShaders[ST].FDefferedInit := False;
//  end;
//
//  if FSM5.Enabled then
//  begin
//    for ST := Low(TGLShaderType) to High(TGLShaderType) do
//      if Assigned(FSM5.FShaders[ST]) then
//        FSM5.FShaders[ST].FDefferedInit := False;
//  end;

  CurrentDGLContext.PrepareHandlesData;
end;

procedure TDGLLibMaterial.SetMultitexturing(AValue: TDGLMultitexturingProperties);
begin
  FMultitexturing.Assign(AValue);
end;

procedure TDGLLibMaterial.SetBaseMaterial(AValue: TDGLBaseMaterialProperties);
begin
  FBaseMaterial.Assign(AValue);
end;

//procedure TDGLLibMaterial.SetLevel(AValue: TDGLMaterialLevel);
//begin
//  if FApplicableLevel <> AValue then
//  begin
//    FApplicableLevel := AValue;
//    NotifyChange(Self);
//  end;
//end;

//procedure TGLLibMaterialEx.SetSM3(AValue: TGLShaderModel3);
//begin
//  FSM3.Assign(AValue);
//end;
//
//procedure TGLLibMaterialEx.SetSM4(AValue: TGLShaderModel4);
//begin
//  FSM4.Assign(AValue);
//end;
//
//procedure TGLLibMaterialEx.SetSM5(AValue: TGLShaderModel5);
//begin
//  FSM5.Assign(AValue);
//end;

function TDGLLibMaterial.UnApply(var ARci: TRenderContextInfo): Boolean;

//  procedure GetNextPass(AProp: TDGLLibMaterialProperty);
//  begin
//    if Length(AProp.NextPass) > 0 then
//      FNextPass := TDGLMaterialLibrary(GetMaterialLibrary).Materials.GetLibMaterialByName(AProp.NextPass)
//    else
//      FNextPass := nil;
//
//    if FNextPass = Self then
//    begin
//      AProp.NextPass := '';
//      FNextPass      := nil;
//    end;
//  end;

begin
  if FStoreAmalgamating <> ARci.amalgamating then
    ARci.amalgamating := FStoreAmalgamating;
   result:=true;
//  if Assigned(FNextPass) then
//  begin
//    Result := FNextPass.UnApply(ARci);
//    if Result then
//      FNextPass.Apply(ARci)
//    else
//      FNextPass := nil;
//    exit;
//  end;

//  case FSelectedLevel of
//    mlBaseMaterial:
//      begin
//        FBaseMaterial.UnApply(ARci);
////        GetNextPass(FBaseMaterial);
//      end;
//
//    mlMultitexturing:
//      begin
//        if FBaseMaterial.Enabled then FBaseMaterial.UnApply(ARci);
//        FMultitexturing.UnApply(ARci);
////        GetNextPass(FMultitexturing);
//      end;

//    mlSM3:
//      begin
//        if FFixedFunc.Enabled then
//          FFixedFunc.UnApply(ARci);
//        FSM3.UnApply(ARci);
//        GetNextPass(FSM3);
//      end;
//
//    mlSM4:
//      begin
//        if FFixedFunc.Enabled then
//          FFixedFunc.UnApply(ARci);
//        FSM4.UnApply(ARci);
//        GetNextPass(FSM4);
//      end;
//
//    mlSM5:
//      begin
//        if FFixedFunc.Enabled then
//          FFixedFunc.UnApply(ARci);
//        FSM5.UnApply(ARci);
//        GetNextPass(FSM5);
//      end;
//  else
//    FNextPass := nil;
//  end;
  ARci.GLStates.ActiveTexture := 0;

//  Result := Assigned(FNextPass);
//  if Result then
//    FNextPass.Apply(ARci);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ----------------
{ TDGLMatLibComponents }
{$IFDEF GLS_REGION}{$REGION 'TDGLMatLibComponents'}{$ENDIF}

function TDGLMatLibComponents.GetAttachmentByName(const AName: TDGLMaterialComponentName): TDGLFrameBufferAttachment;
var
  n, i: Integer;
begin
  n     := ComputeNameHashKey(AName);
  for i := 0 to Count - 1 do
  begin
    if (Items[i] is TDGLFrameBufferAttachment) and (Items[i].FNameHashKey = n) then
    begin
      if Items[i].Name = AName then
      begin
        Result := TDGLFrameBufferAttachment(Items[i]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TDGLMatLibComponents.GetCombinerByName(const AName: TDGLMaterialComponentName): TDGLTextureCombiner;
var
  n, i: Integer;
begin
  n     := ComputeNameHashKey(AName);
  for i := 0 to Count - 1 do
  begin
    if (Items[i] is TDGLTextureCombiner) and (Items[i].FNameHashKey = n) then
    begin
      if Items[i].Name = AName then
      begin
        Result := TDGLTextureCombiner(Items[i]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TDGLMatLibComponents.GetItemByName(const AName: TDGLMaterialComponentName): TDGLBaseMaterialCollectionItem;
var
  n, i: Integer;
begin
  n     := ComputeNameHashKey(AName);
  for i := 0 to Count - 1 do
  begin
    if (Items[i].FNameHashKey = n) and (Items[i].Name = AName) then
    begin
      Result := Items[i];
      exit;
    end;
  end;
  Result := nil;
end;

function TDGLMatLibComponents.GetItems(Index: Integer): TDGLBaseMaterialCollectionItem;
begin
  Result := TDGLBaseMaterialCollectionItem(inherited GetItems(index));
end;

function TDGLMatLibComponents.GetNamePath: string;
var
  S: string;
begin
  Result := ClassName;
  if GetOwner = nil then
    exit;
  S := GetOwner.GetNamePath;
  if S = '' then
    exit;
  Result := S + '.Components';
end;

function TDGLMatLibComponents.GetSamplerByName(const AName: TDGLMaterialComponentName): TDGLTextureSampler;
var
  n, i: Integer;
begin
  n     := ComputeNameHashKey(AName);
  for i := 0 to Count - 1 do
  begin
    if (Items[i] is TDGLTextureSampler) and (Items[i].FNameHashKey = n) then
    begin
      if Items[i].Name = AName then
      begin
        Result := TDGLTextureSampler(Items[i]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

//function TDGLMatLibComponents.GetShaderByName(const AName: TDGLMaterialComponentName): TDGLShaderEx;
//var
//  n, i: Integer;
//begin
//  n     := ComputeNameHashKey(AName);
//  for i := 0 to Count - 1 do
//  begin
//    if (Items[i] is TDGLShaderEx) and (Items[i].FNameHashKey = n) then
//    begin
//      if Items[i].Name = AName then
//      begin
//        Result := TGLShaderEx(Items[i]);
//        exit;
//      end;
//    end;
//  end;
//  Result := nil;
//end;

//function TDGLMatLibComponents.GetAsmProgByName(const AName: TGLMaterialComponentName): TDGLASMVertexProgram;
//var
//  n, i: Integer;
//begin
//  n     := ComputeNameHashKey(AName);
//  for i := 0 to Count - 1 do
//  begin
//    if (Items[i] is TDGLASMVertexProgram) and (Items[i].FNameHashKey = n) then
//    begin
//      if Items[i].Name = AName then
//      begin
//        Result := TDGLASMVertexProgram(Items[i]);
//        exit;
//      end;
//    end;
//  end;
//  Result := nil;
//end;

function TDGLMatLibComponents.GetTextureByName(const AName: TDGLMaterialComponentName): TDGLAbstractTexture;
var
  n, i: Integer;
begin
  n     := ComputeNameHashKey(AName);
  for i := 0 to Count - 1 do
  begin
    if (Items[i] is TDGLAbstractTexture) and (Items[i].FNameHashKey = n) then
    begin
      if Items[i].Name = AName then
      begin
        Result := TDGLTexture(Items[i]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

class function TDGLMatLibComponents.ItemsClass: TDGLXCollectionItemClass;
begin
  Result := TDGLBaseMaterialCollectionItem;
end;

function TDGLMatLibComponents.MakeUniqueName(const AName: TDGLMaterialComponentName): TDGLMaterialComponentName;
var
  i: Integer;
begin
  Result := AName;
  i      := 1;
  while GetItemByName(Result) <> nil do
  begin
    Result := AName + IntToStr(i);
    Inc(i);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ----------------
{ TDGLMaterialLibrary }
{$IFDEF GLS_REGION}{$REGION 'TDGLMaterialLibrary'}{$ENDIF}

function TDGLMaterialLibrary.AddAttachment(const AName: TDGLMaterialComponentName): TDGLFrameBufferAttachment;
begin
  Result      := TDGLFrameBufferAttachment.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TDGLMaterialLibrary.AddCombiner(const AName: TDGLMaterialComponentName): TDGLTextureCombiner;
begin
  Result      := TDGLTextureCombiner.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TDGLMaterialLibrary.AddSampler(const AName: TDGLMaterialComponentName): TDGLTextureSampler;
begin
  Result      := TDGLTextureSampler.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

//function TDGLMaterialLibrary.AddShader(const AName: TGLMaterialComponentName): TGLShaderEx;
//begin
//  Result      := TGLShaderEx.Create(Components);
//  Result.Name := AName;
//  Components.Add(Result);
//end;

//function TDGLMaterialLibrary.AddAsmProg(const AName: TGLMaterialComponentName): TGLASMVertexProgram;
//begin
//  Result      := TGLASMVertexProgram.Create(Components);
//  Result.Name := AName;
//  Components.Add(Result);
//end;

function TDGLMaterialLibrary.AddTexture(const AName: TDGLMaterialComponentName): TDGLTexture;
begin
  Result      := TDGLTexture.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

constructor TDGLMaterialLibrary.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials  := TDGLLibMaterials.Create(Self);
  FComponents := TDGLMatLibComponents.Create(Self);
end;

procedure TDGLMaterialLibrary.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('ComponentsData', ReadComponents, WriteComponents, Components.Count > 0);
  inherited;
end;

destructor TDGLMaterialLibrary.Destroy;
begin
  FMaterials.Destroy;
  FComponents.Destroy;
  inherited;
end;

function TDGLMaterialLibrary.GetMaterials: TDGLLibMaterials;
begin
  Result := TDGLLibMaterials(FMaterials);
end;

procedure TDGLMaterialLibrary.GetNames(Proc: TGetStrProc; AClass: CGLBaseMaterialCollectionItem);
var
  i: Integer;
begin
  for i := 0 to Components.Count - 1 do
    if Components[i].ClassType = AClass then
      Proc(Components[i].Name)
end;

procedure TDGLMaterialLibrary.Loaded;
begin
  inherited;
end;

procedure TDGLMaterialLibrary.ReadComponents(AStream: TStream);
var
  LReader: TReader;
begin
  LReader := TReader.Create(AStream, 16384);
  try
    Components.ReadFromFiler(LReader);
  finally
    LReader.Free;
  end;
end;

procedure TDGLMaterialLibrary.SetComponents(AValue: TDGLMatLibComponents);
begin
  FComponents.Assign(AValue);
end;

//procedure TDGLMaterialLibrary.SetLevelForAll(const ALevel: TDGLMaterialLevel);
//var
//  i: Integer;
//begin
//  for i                          := Materials.Count - 1 downto 0 do
//    Materials[i].ApplicableLevel := ALevel;
//end;

procedure TDGLMaterialLibrary.SetMaterials(AValue: TDGLLibMaterials);
begin
  FMaterials.Assign(AValue);
end;

function TDGLMaterialLibrary.StoreMaterials: Boolean;
begin
  Result := (FMaterials.Count > 0);
end;

procedure TDGLMaterialLibrary.WriteComponents(AStream: TStream);
var
  LWriter: TWriter;
begin
  LWriter := TWriter.Create(AStream, 16384);
  try
    Components.WriteToFiler(LWriter);
  finally
    LWriter.Free;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

initialization

RegisterClasses([TDGLTexture, TDGLFrameBufferAttachment, TDGLTextureSampler, TDGLTextureCombiner, TDGLMaterialLibrary]);

RegisterXCollectionItemClass(TDGLTexture);
RegisterXCollectionItemClass(TDGLTextureSampler);
RegisterXCollectionItemClass(TDGLFrameBufferAttachment);
RegisterXCollectionItemClass(TDGLTextureCombiner);

finalization

end.
