//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
  Handles extended material and it components:
  textures, samplers, combiners, shaders and etc.

  Features:
   - material can contain different level of applying accordingly to hardware i.e. Feateres scaling.
   - if automatically or by user selected level failed, material down to lower level.
   - direct state access can be used for uniforms setting.
   - economy mode for texture bindig to active units,
     i.e. if textures less than maximum units may be not one binding occur per frame.
     
}

unit GLS.MaterialEx;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils,
  //GLS
  GLS.RenderContextInfo, GLS.BaseClasses, GLS.Context, GLS.VectorTypes,
  GLS.Material, GLS.Texture, GLS.Color, GLS.Coordinates, GLS.VectorGeometry,
  GLS.Graphics, GLS.PersistentClasses, GLS.CrossPlatform, GLS.State,
  GLS.TextureFormat, GLS.XCollection, GLS.TextureCombiners, GLS.OpenGLTokens,
  GLS.GLSLParameter, GLS.ApplicationFileIO, GLS.Strings, GLS.ImageUtils,
  GLS.Utils, GLS.XOpenGL, GLS.Log;


type

  TVKMaterialComponentName = string;
  TVKMaterialLibraryEx = class;
  TVKMatLibComponents = class;
  TVKLibMaterialEx = class;
  TVKBaseShaderModel = class;
  TVKASMVertexProgram = class;

  TOnAsmProgSetting = procedure(Sender: TVKASMVertexProgram;
    var ARci: TRenderContextInfo) of object;
  TOnUniformInitialize = procedure(Sender: TVKBaseShaderModel) of object;
  TOnUniformSetting = procedure(Sender: TVKBaseShaderModel;
    var ARci: TRenderContextInfo) of object;

  // TVKBaseMaterialCollectionItem
  //

  TVKBaseMaterialCollectionItem = class(
      TVKXCollectionItem,
      IGLMaterialLibrarySupported)
  private
    { Private Declarations }
    FNameHashKey: Integer;
    FUserList: TPersistentObjectList;
    FDefferedInit: Boolean;
    FNotifying: Boolean;
    FIsValid: Boolean;
    function GetUserList: TPersistentObjectList;
    function GetMaterialLibraryEx: TVKMaterialLibraryEx;
  protected
    { Protected Declarations }
    procedure SetName(const AValue: TVKMaterialComponentName); override;
    procedure NotifyChange(Sender: TObject); virtual;
    property UserList: TPersistentObjectList read GetUserList;
    procedure DoOnPrepare(Sender: TVKContext); virtual; abstract;
  public
    { Public Declarations }
    destructor Destroy; override;

    procedure RegisterUser(AUser: TVKUpdateAbleObject);
    procedure UnregisterUser(AUser: TVKUpdateAbleObject);
    function GetUserCount: Integer;
    function GetMaterialLibrary: TVKAbstractMaterialLibrary;

    property MaterialLibrary: TVKMaterialLibraryEx read GetMaterialLibraryEx;
    property IsValid: Boolean read FIsValid;
  published
    { Published Declarations }
    property Name: TVKMaterialComponentName read GetName write SetName;
    { Run-time flag, indicate that resource
       should initialize in case of failure material's level. }
    property DefferedInit: Boolean read FDefferedInit write FDefferedInit
      default False;
  end;

  CGLBaseMaterialCollectionItem = class of TVKBaseMaterialCollectionItem;

  // TVKLibMaterialProperty
  //

  TVKLibMaterialProperty = class(
      TVKUpdateAbleObject,
      IGLMaterialLibrarySupported)
  protected
    { Protected Declarations }
    FEnabled: Boolean;
    FNextPassName: TVKLibMaterialName;
    function GetMaterial: TVKLibMaterialEx;
    function GetMaterialLibraryEx: TVKMaterialLibraryEx;
    procedure SetEnabled(AValue: Boolean); virtual;
    procedure SetNextPass(const AValue: TVKLibMaterialName);
    procedure Loaded; virtual;
    property NextPass: TVKLibMaterialName read FNextPassName write SetNextPass;
  public
    { Public Declarations }
    procedure NotifyChange(Sender: TObject); override;
    function GetMaterialLibrary: TVKAbstractMaterialLibrary;

    property MaterialLibrary: TVKMaterialLibraryEx read GetMaterialLibraryEx;
  published
    { Published Declarations }
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  // TVKTextureSampler
  //

  TVKTextureSampler = class(TVKBaseMaterialCollectionItem)
  protected
    { Protected Declarations }
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    { Private Declarations }
    FHandle: TVKSamplerHandle;
    FMinFilter: TVKMinFilter;
    FMagFilter: TVKMagFilter;
    FFilteringQuality: TVKTextureFilteringQuality;
    FLODBias: Integer;
    FLODBiasFract: Single;
    FWrap: array[0..2] of TVKSeparateTextureWrap;
    FBorderColor: TVKColor;
    FCompareMode: TVKTextureCompareMode;
    FCompareFunc: TDepthFunction;
    FDecodeSRGB: Boolean;
    procedure SetMagFilter(AValue: TVKMagFilter);
    procedure SetMinFilter(AValue: TVKMinFilter);
    procedure SetLODBias(AValue: Integer);
    procedure SetFilteringQuality(AValue: TVKTextureFilteringQuality);
    function GetWrap(Index: Integer): TVKSeparateTextureWrap;
    procedure SetWrap(Index: Integer; AValue: TVKSeparateTextureWrap);
    procedure SetBorderColor(const AValue: TVKColor);
    procedure SetCompareMode(AValue: TVKTextureCompareMode);
    procedure SetCompareFunc(AValue: TDepthFunction);
    procedure SetDecodeSRGB(AValue: Boolean);
  public
    { Public Declarations }
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure NotifyChange(Sender: TObject); override;

    procedure DoOnPrepare(Sender: TVKContext); override;
    procedure Apply(var ARci: TRenderContextInfo);
    procedure UnApply(var ARci: TRenderContextInfo);

    class function FriendlyName: string; override;

    property Handle: TVKSamplerHandle read FHandle;
  published
    { Published Declarations }

    { Texture magnification filter. }
    property MagFilter: TVKMagFilter read FMagFilter write SetMagFilter
      default maLinear;
    { Texture minification filter. }
    property MinFilter: TVKMinFilter read FMinFilter write SetMinFilter
      default miLinearMipMapLinear;
    property FilteringQuality: TVKTextureFilteringQuality read FFilteringQuality
      write SetFilteringQuality default tfAnisotropic;
    { Texture LOD bias. }
    property LodBias: Integer read FLODBias write SetLODBias default 0;
    { Address mode for the texture. }
    property WrapX: TVKSeparateTextureWrap index 0 read GetWrap write SetWrap
      default twRepeat;
    property WrapY: TVKSeparateTextureWrap index 1 read GetWrap write SetWrap
      default twRepeat;
    property WrapZ: TVKSeparateTextureWrap index 2 read GetWrap write SetWrap
      default twRepeat;
    { Texture border color. }
    property BorderColor: TVKColor read FBorderColor
      write SetBorderColor;
    { Compare mode and function for depth texture. }
    property CompareMode: TVKTextureCompareMode read FCompareMode
      write SetCompareMode default tcmNone;
    property CompareFunc: TDepthFunction read FCompareFunc
      write SetCompareFunc default cfLEqual;
    { Force retrieving the undecoded sRGB data from the
       texture and manipulate that directly. }
    property sRGB_Encode: Boolean read FDecodeSRGB write SetDecodeSRGB
      default True;
  end;

  // TVKAbstractTexture
  //

  TVKAbstractTexture = class(TVKBaseMaterialCollectionItem)
  protected
    { Protected Declarations }
    FHandle: TVKTextureHandle;
    FInternalFormat: TGLInternalFormat;
    FWidth: Integer;
    FHeight: Integer;
    FDepth: Integer;
    FSwizzles: TSwizzleVector;
    FApplicableSampler: TVKTextureSampler;
    FLastSampler: TVKTextureSampler;
    function GetTextureTarget: TVKTextureTarget;
    procedure Apply(var ARci: TRenderContextInfo); virtual; abstract;
    procedure UnApply(var ARci: TRenderContextInfo); virtual; abstract;
  public
    { Public Declarations }
    property Handle: TVKTextureHandle read FHandle;
  published
    { Published Declarations }
    property Shape: TVKTextureTarget read GetTextureTarget;
  end;

  TMipmapGenerationMode =
    (
    mgmNoMip,
    mgmLeaveExisting,
    mgmOnFly,
    mgmBoxFilter,
    mgmTriangleFilter,
    mgmHermiteFilter,
    mgmBellFilter,
    mgmSplineFilter,
    mgmLanczos3Filter,
    mgmMitchellFilter
    );

  // TVKTextureImageEx
  //

  TVKTextureImageEx = class(TVKAbstractTexture)
  protected
    { Protected Declarations }
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    { Private Declarations }
    FCompression: TVKTextureCompression;
    FImage: TVKBaseImage;
    FImageAlpha: TVKTextureImageAlpha;
    FImageBrightness: Single;
    FImageGamma: Single;
    FHeightToNormalScale: Single;
    FSourceFile: string;
    FApplyCounter: Integer;
    FInternallyStored: Boolean;
    FMipGenMode: TMipmapGenerationMode;
    FUseStreaming: Boolean;
    FBaseLevel: Integer;
    FMaxLevel: Integer;
    FLastTime: Double;
    procedure SetInternalFormat(const AValue: TGLInternalFormat);
    procedure SetImageAlpha(const AValue: TVKTextureImageAlpha);
    procedure SetImageBrightness(const AValue: Single);
    function StoreBrightness: Boolean;
    procedure SetImageGamma(const AValue: Single);
    function StoreGamma: Boolean;
    procedure SetNormalMapScale(const AValue: Single);
    function StoreNormalMapScale: Boolean;
    procedure SetCompression(const AValue: TVKTextureCompression);
    procedure SetSourceFile(AValue: string);
    procedure SetInternallyStored(const AValue: Boolean);
    procedure SetMipGenMode(const AValue: TMipmapGenerationMode);
    procedure SetUseStreaming(const AValue: Boolean);
    procedure PrepareImage;
    procedure FullTransfer;
    procedure StreamTransfer;
    procedure CalcLODRange(out AFirstLOD, ALastLOD: Integer);
  public
    { Public Declarations }
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure NotifyChange(Sender: TObject); override;

    procedure DoOnPrepare(Sender: TVKContext); override;
    procedure Apply(var ARci: TRenderContextInfo); override;
    procedure UnApply(var ARci: TRenderContextInfo); override;

    class function FriendlyName: string; override;
  published
    { Published Declarations }

    // Factual texture properties
    property InternalWidth: Integer read FWidth;
    property InternalHeight: Integer read FHeight;
    property InternalDepth: Integer read FDepth;
    property InternalFormat: TGLInternalFormat read FInternalFormat
      write SetInternalFormat default tfRGBA8;

    { Automatic Image Alpha setting. 
      Allows to control how and if the image's Alpha channel (transparency)
      is computed. }
    property ImageAlpha: TVKTextureImageAlpha read FImageAlpha write
      SetImageAlpha default tiaDefault;
    { Texture brightness correction. 
      This correction is applied upon loading a TVKTextureImage, it's a
      simple saturating scaling applied to the RGB components of
      the 32 bits image, before it is passed to OpenGL, and before
      gamma correction (if any). }
    property ImageBrightness: Single read FImageBrightness write
      SetImageBrightness stored StoreBrightness;
    { Texture gamma correction. 
      The gamma correction is applied upon loading a TVKTextureImage,
      applied to the RGB components of the 32 bits image, before it is
      passed to OpenGL, after brightness correction (if any). }
    property ImageGamma: Single read FImageGamma write SetImageGamma stored
      StoreGamma;
    { Texture compression control. 
      If True the compressed TextureFormat variant (the OpenGL ICD must
      support GL_ARB_texture_compression, or this option is ignored). }
    property Compression: TVKTextureCompression read FCompression write
      SetCompression default tcDefault;
    { Normal Map scaling. 
      Force normal map generation from height map and controls
      the intensity of the bumps. }
    property HeightToNormalScale: Single read FHeightToNormalScale
      write SetNormalMapScale stored StoreNormalMapScale;
    { Source file path and name. }
    property SourceFile: string read FSourceFile write SetSourceFile;
    { Force to store image levels in separate files in ready to transfer format. }
    property InternallyStored: Boolean read FInternallyStored
      write SetInternallyStored default False;
    { Mipmap generation mode. }
    property MipGenMode: TMipmapGenerationMode read FMipGenMode
      write SetMipGenMode default mgmOnFly;
    { Enable streaming loading. }
    property UseStreaming: Boolean read FUseStreaming
      write SetUseStreaming default False;
  end;

  // TVKFrameBufferAttachment
  //

  TVKFrameBufferAttachment = class(TVKAbstractTexture)
  protected
    { Protected Declarations }
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    { Private Declarations }
    FRenderBufferHandle: TVKRenderbufferHandle;
    FLayered: Boolean;
    FCubeMap: Boolean;
    FSamples: Integer;
    FOnlyWrite: Boolean;
    FFixedSamplesLocation: Boolean;
    procedure SetWidth(AValue: Integer);
    procedure SetHeight(AValue: Integer);
    procedure SetDepth(AValue: Integer);
    procedure SetInternalFormat(const AValue: TGLInternalFormat);
    procedure SetOnlyWrite(AValue: Boolean);
    procedure SetLayered(AValue: Boolean);
    procedure SetCubeMap(AValue: Boolean);
    procedure SetSamples(AValue: Integer);
    procedure SetFixedSamplesLocation(AValue: Boolean);
  public
    { Public Declarations }
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure NotifyChange(Sender: TObject); override;

    procedure DoOnPrepare(Sender: TVKContext); override;
    procedure Apply(var ARci: TRenderContextInfo); override;
    procedure UnApply(var ARci: TRenderContextInfo); override;

    class function FriendlyName: string; override;
  published
    { Published Declarations }
    property InternalWidth: Integer read FWidth
      write SetWidth default 256;
    property InternalHeight: Integer read FHeight
      write SetHeight default 256;
    property InternalDepth: Integer read FDepth
      write SetDepth default 0;
    property InternalFormat: TGLInternalFormat read FInternalFormat
      write SetInternalFormat default tfRGBA8;
    { This flag makes use render buffer as target which makes
        it impossible to read it as texture, but improves efficiency. }
    property OnlyWrite: Boolean read FOnlyWrite
      write SetOnlyWrite default False;
    { Force targe be texture array. }
    property Layered: Boolean read FLayered
      write SetLayered default False;
    { Force target be cube map. }
    property CubeMap: Boolean read FCubeMap
      write SetCubeMap default False;
    { Number of samples. Positive value makes texture be multisample. }
    property Samples: Integer read FSamples
      write SetSamples default -1;
    { FixedSamplesLocation flag makes image will use identical
      sample locations and the same number of samples for all texels in
      the image, and the sample locations will not depend on the
      internalformat or size of the image. }
    property FixedSamplesLocation: Boolean read FFixedSamplesLocation
      write SetFixedSamplesLocation default False;
  end;

  // TVKTextureSwizzling
  //
    { Swizzle the components of a texture fetches in
        shader or fixed-function pipeline. }
  TVKTextureSwizzling = class(TVKUpdateAbleObject)
  private
    { Private Declarations }
    FSwizzles: TSwizzleVector;
    function GetSwizzle(AIndex: Integer): TVKTextureSwizzle;
    procedure SetSwizzle(AIndex: Integer; AValue: TVKTextureSwizzle);
    function StoreSwizzle(AIndex: Integer): Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    procedure WriteToFiler(AWriter: TWriter);
    procedure ReadFromFiler(AReader: TReader);
  published
    { Published Declarations }
    property RedFrom: TVKTextureSwizzle index 0 read GetSwizzle
      write SetSwizzle stored StoreSwizzle;
    property GreenFrom: TVKTextureSwizzle index 1 read GetSwizzle
      write SetSwizzle stored StoreSwizzle;
    property BlueFrom: TVKTextureSwizzle index 2 read GetSwizzle
      write SetSwizzle stored StoreSwizzle;
    property AlphaFrom: TVKTextureSwizzle index 3 read GetSwizzle
      write SetSwizzle stored StoreSwizzle;
  end;

  // TVKTextureProperties
  //

  TVKTextureProperties = class(TVKLibMaterialProperty)
  private
    { Private Declarations }
    FLibTextureName: TVKMaterialComponentName;
    FLibSamplerName: TVKMaterialComponentName;
    FLibTexture: TVKAbstractTexture;
    FLibSampler: TVKTextureSampler;
    FTextureOffset, FTextureScale: TVKCoordinates;
    FTextureRotate: Single;
    FTextureMatrixIsIdentity: Boolean;
    FTextureOverride: Boolean;
    FTextureMatrix: TMatrix;
    FMappingMode: TVKTextureMappingMode;
    FEnvColor: TVKColor;
    FMapSCoordinates: TVKCoordinates4;
    FMapTCoordinates: TVKCoordinates4;
    FMapRCoordinates: TVKCoordinates4;
    FMapQCoordinates: TVKCoordinates4;
    FSwizzling: TVKTextureSwizzling;
    function GetLibTextureName: TVKMaterialComponentName;
    function GetLibSamplerName: TVKMaterialComponentName;
    procedure SetLibTextureName(const AValue: TVKMaterialComponentName);
    procedure SetLibSamplerName(const AValue: TVKMaterialComponentName);
    function GetTextureOffset: TVKCoordinates;
    procedure SetTextureOffset(const AValue: TVKCoordinates);
    function StoreTextureOffset: Boolean;
    function GetTextureScale: TVKCoordinates;
    procedure SetTextureScale(const AValue: TVKCoordinates);
    function StoreTextureScale: Boolean;
    procedure SetTextureMatrix(const AValue: TMatrix);
    procedure SetTextureRotate(AValue: Single);
    function StoreTextureRotate: Boolean;
    procedure SetMappingMode(const AValue: TVKTextureMappingMode);
    function GetMappingSCoordinates: TVKCoordinates4;
    procedure SetMappingSCoordinates(const AValue: TVKCoordinates4);
    function StoreMappingSCoordinates: Boolean;
    function GetMappingTCoordinates: TVKCoordinates4;
    procedure SetMappingTCoordinates(const AValue: TVKCoordinates4);
    function StoreMappingTCoordinates: Boolean;
    function GetMappingRCoordinates: TVKCoordinates4;
    procedure SetMappingRCoordinates(const AValue: TVKCoordinates4);
    function StoreMappingRCoordinates: Boolean;
    function GetMappingQCoordinates: TVKCoordinates4;
    procedure SetMappingQCoordinates(const AValue: TVKCoordinates4);
    function StoreMappingQCoordinates: Boolean;
    procedure SetSwizzling(const AValue: TVKTextureSwizzling);
    function StoreSwizzling: Boolean;
    procedure SetEnvColor(const AValue: TVKColor);

    procedure CalculateTextureMatrix;
    procedure ApplyMappingMode;
    procedure UnApplyMappingMode;
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

    property TextureMatrix: TMatrix read FTextureMatrix write SetTextureMatrix;
  published
    { Published Declarations }
    property LibTextureName: TVKMaterialComponentName read GetLibTextureName
      write SetLibTextureName;
    property LibSamplerName: TVKMaterialComponentName read GetLibSamplerName
      write SetLibSamplerName;
    property TextureOffset: TVKCoordinates read GetTextureOffset write
      SetTextureOffset stored StoreTextureOffset;
    { Texture coordinates scaling. 
       Scaling is applied before applying the offset, and is applied
       to the texture coordinates, meaning that a scale factor of (2, 2, 2)
       will make your texture look twice <i>smaller</i>. }
    property TextureScale: TVKCoordinates read GetTextureScale write
      SetTextureScale stored StoreTextureScale;
    { Texture coordinates rotating. 
       Rotating is applied after applying offset and scale,
       and rotate ST direction around R axis. }
    property TextureRotate: Single read FTextureRotate write
      SetTextureRotate stored StoreTextureRotate;
    { Texture Environment color. }
    property EnvColor: TVKColor read FEnvColor write SetEnvColor;
    { Texture coordinates mapping mode. 
    This property controls automatic texture coordinates generation. }
    property MappingMode: TVKTextureMappingMode read FMappingMode write
      SetMappingMode default tmmUser;
    { Texture mapping coordinates mode for S, T, R and Q axis. 
    This property stores the coordinates for automatic texture
    coordinates generation. }
    property MappingSCoordinates: TVKCoordinates4 read GetMappingSCoordinates
      write SetMappingSCoordinates stored StoreMappingSCoordinates;
    property MappingTCoordinates: TVKCoordinates4 read GetMappingTCoordinates
      write SetMappingTCoordinates stored StoreMappingTCoordinates;
    property MappingRCoordinates: TVKCoordinates4 read GetMappingRCoordinates
      write SetMappingRCoordinates stored StoreMappingRCoordinates;
    property MappingQCoordinates: TVKCoordinates4 read GetMappingQCoordinates
      write SetMappingQCoordinates stored StoreMappingQCoordinates;
    { Texture color fetching parameters. }
    property Swizzling: TVKTextureSwizzling read FSwizzling write
      SetSwizzling stored StoreSwizzling;
  end;

  //  TVKFixedFunctionProperties
  //
  TVKFixedFunctionProperties = class(TVKLibMaterialProperty)
  private
    { Private Declarations }
    FFrontProperties: TVKFaceProperties;
    FBackProperties: TVKFaceProperties;
    FDepthProperties: TVKDepthProperties;
    FBlendingMode: TBlendingMode;
    FBlendingParams: TVKBlendingParameters;
    FTexProp: TVKTextureProperties;
    FMaterialOptions: TMaterialOptions;
    FFaceCulling: TFaceCulling;
    FPolygonMode: TPolygonMode;
    FTextureMode: TVKTextureMode;
    function GetBackProperties: TVKFaceProperties;
    procedure SetBackProperties(AValues: TVKFaceProperties);
    procedure SetFrontProperties(AValues: TVKFaceProperties);
    procedure SetDepthProperties(AValues: TVKDepthProperties);
    procedure SetBlendingMode(const AValue: TBlendingMode);
    procedure SetMaterialOptions(const AValue: TMaterialOptions);
    procedure SetFaceCulling(const AValue: TFaceCulling);
    procedure SetPolygonMode(AValue: TPolygonMode);
    procedure SetBlendingParams(const AValue: TVKBlendingParameters);
    procedure SetTexProp(AValue: TVKTextureProperties);
    procedure SetTextureMode(AValue: TVKTextureMode);
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Apply(var ARci: TRenderContextInfo);
    procedure UnApply(var ARci: TRenderContextInfo);
    { Returns True if the material is blended.  }
    function Blended: Boolean;

  published
    { Published Declarations }
    property MaterialOptions: TMaterialOptions read FMaterialOptions write
      SetMaterialOptions default [];

    property BackProperties: TVKFaceProperties read GetBackProperties write
      SetBackProperties;
    property FrontProperties: TVKFaceProperties read FFrontProperties write
      SetFrontProperties;
    property DepthProperties: TVKDepthProperties read FDepthProperties write
      SetDepthProperties;
    property BlendingMode: TBlendingMode read FBlendingMode write SetBlendingMode
      default bmOpaque;
    property BlendingParams: TVKBlendingParameters read FBlendingParams write
      SetBlendingParams;

    property FaceCulling: TFaceCulling read FFaceCulling write SetFaceCulling
      default fcBufferDefault;
    property PolygonMode: TPolygonMode read FPolygonMode write SetPolygonMode
      default pmFill;
    property Texture: TVKTextureProperties read FTexProp write SetTexProp;
    { Texture application mode. }
    property TextureMode: TVKTextureMode read FTextureMode write SetTextureMode
      default tmDecal;
    { Next pass of FFP. }
    property NextPass;
  end;

  //  TVKTextureCombiner
  //

  TVKTextureCombiner = class(TVKBaseMaterialCollectionItem)
  protected
    { Protected Declarations }
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    { Private Declarations }
    FHandle: TVKVirtualHandle;
    FScript: TStringList;
    FCommandCache: TCombinerCache;
    procedure SetScript(AValue: TStringList);
    procedure DoAllocate(Sender: TVKVirtualHandle; var handle: TGLuint);
    procedure DoDeallocate(Sender: TVKVirtualHandle; var handle: TGLuint);
  public
    { Public Declarations }
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure NotifyChange(Sender: TObject); override;

    procedure DoOnPrepare(Sender: TVKContext); override;

    class function FriendlyName: string; override;
  published
    { Published Declarations }
    property Script: TStringList read FScript write SetScript;
  end;

  // TVKARBVertexProgram
  //

  TVKASMVertexProgram = class(TVKBaseMaterialCollectionItem)
  protected
    { Protected Declarations }
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    { Private Declarations }
    FHandle: TVKARBVertexProgramHandle;
    FSource: TStringList;
    FSourceFile: string;
    FInfoLog: string;
    procedure SetSource(AValue: TStringList);
    procedure SetSourceFile(AValue: string);
    function GetHandle: TVKARBVertexProgramHandle;
  public
    { Public Declarations }
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DoOnPrepare(Sender: TVKContext); override;

    class function FriendlyName: string; override;

    procedure NotifyChange(Sender: TObject); override;
    property Handle: TVKARBVertexProgramHandle read GetHandle;
  published
    { Published Declarations }
    property Source: TStringList read FSource write SetSource;
    property SourceFile: string read FSourceFile write SetSourceFile;
    property InfoLog: string read FInfoLog;
  end;

  TLightDir2TexEnvColor = (
    l2eNone,
    l2eEnvColor0,
    l2eEnvColor1,
    l2eEnvColor2,
    l2eEnvColor3
    );

  // TVKMultitexturingProperties
  //

  TVKMultitexturingProperties = class(TVKLibMaterialProperty)
  private
    FLibCombiner: TVKTextureCombiner;
    FLibAsmProg: TVKASMVertexProgram;
    FLibCombinerName: TVKMaterialComponentName;
    FLibAsmProgName: TVKMaterialComponentName;
    FTexProps: array[0..3] of TVKTextureProperties;
    FTextureMode: TVKTextureMode;
    FLightDir: TLightDir2TexEnvColor;
    FLightSourceIndex: Integer;
    function GetLibCombinerName: string;
    function GetLibAsmProgName: string;
    procedure SetLibCombinerName(const AValue: string);
    procedure SetLibAsmProgName(const AValue: string);
    function GetTexProps(AIndex: Integer): TVKTextureProperties;
    procedure SetTexProps(AIndex: Integer; AValue: TVKTextureProperties);
    procedure SetTextureMode(AValue: TVKTextureMode);
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
    property LibCombinerName: string read GetLibCombinerName
      write SetLibCombinerName;
    property LibAsmProgName: string read GetLibAsmProgName
      write SetLibAsmProgName;
    property Texture0: TVKTextureProperties index 0 read GetTexProps write
      SetTexProps;
    property Texture1: TVKTextureProperties index 1 read GetTexProps write
      SetTexProps;
    property Texture2: TVKTextureProperties index 2 read GetTexProps write
      SetTexProps;
    property Texture3: TVKTextureProperties index 3 read GetTexProps write
      SetTexProps;
    { Texture application mode. }
    property TextureMode: TVKTextureMode read FTextureMode write SetTextureMode
      default tmDecal;
    { Pass light source direction to enviroment color of choosen texture.
       Vector in model space. }
    property LightDirTo: TLightDir2TexEnvColor read FLightDir
      write FLightDir default l2eNone;
    { Specify index of light source for LightDirTo. }
    property LightSourceIndex: Integer read FLightSourceIndex
      write SetLightSourceIndex default 0;
    { Next pass of combiner. }
    property NextPass;
  end;

  TVKShaderType =
    (
    shtVertex,
    shtControl,
    shtEvaluation,
    shtGeometry,
    shtFragment
    );

  // TVKSLShaderEx
  //

  TVKShaderEx = class(TVKBaseMaterialCollectionItem)
  protected
    { Protected Declarations }
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    { Private Declarations }
    FHandle: array[TVKShaderType] of TVKShaderHandle;
    FSource: TStringList;
    FSourceFile: string;
    FShaderType: TVKShaderType;
    FInfoLog: string;
    FGeometryInput: TVKgsInTypes;
    FGeometryOutput: TVKgsOutTypes;
    FGeometryVerticesOut: TGLint;
    procedure SetSource(AValue: TStringList);
    procedure SetSourceFile(AValue: string);
    procedure SetShaderType(AValue: TVKShaderType);
    procedure SetGeometryInput(AValue: TVKgsInTypes);
    procedure SetGeometryOutput(AValue: TVKgsOutTypes);
    procedure SetGeometryVerticesOut(AValue: TGLint);
    function GetHandle: TVKShaderHandle;
  public
    { Public Declarations }
    constructor Create(AOwner: TVKXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DoOnPrepare(Sender: TVKContext); override;

    class function FriendlyName: string; override;

    procedure NotifyChange(Sender: TObject); override;
    property Handle: TVKShaderHandle read GetHandle;
  published
    { Published Declarations }
    property Source: TStringList read FSource write SetSource;
    property SourceFile: string read FSourceFile write SetSourceFile;
    property ShaderType: TVKShaderType read FShaderType
      write SetShaderType default shtVertex;
    property InfoLog: string read FInfoLog;
    property GeometryInput: TVKgsInTypes read FGeometryInput
      write SetGeometryInput default gsInPoints;
    property GeometryOutput: TVKgsOutTypes read FGeometryOutput
      write SetGeometryOutput default gsOutPoints;
    property GeometryVerticesOut: TGLint read FGeometryVerticesOut
      write SetGeometryVerticesOut default 1;
  end;

  // TVKAbstractShaderUniform
  //

  TVKAbstractShaderUniform = class(TVKUpdateAbleObject, IShaderParameter)
  protected
    { Protected Declarations }
    FName: string;
    FNameHashCode: Integer;
    FType: TVKSLDataType;
    FSamplerType: TVKSLSamplerType;

    function GetName: string;
    function GetGLSLType: TVKSLDataType;
    function GetGLSLSamplerType: TVKSLSamplerType;

    function GetAutoSetMethod: string; virtual;
    function GetTextureName: string; virtual;
    function GetSamplerName: string; virtual;
    function GetTextureSwizzle: TSwizzleVector; virtual;
    procedure SetTextureName(const AValue: string); virtual;
    procedure SetSamplerName(const AValue: string); virtual;
    procedure SetAutoSetMethod(const AValue: string); virtual;
    procedure SetTextureSwizzle(const AValue: TSwizzleVector); virtual;

    function GetFloat: Single; virtual;
    function GetVec2: TVector2f; virtual;
    function GetVec3: TVector3f; virtual;
    function GetVec4: TVector; virtual;

    function GetInt: TGLint; virtual;
    function GetIVec2: TVector2i; virtual;
    function GetIVec3: TVector3i; virtual;
    function GetIVec4: TVector4i; virtual;

    function GetUInt: TGLuint; virtual;
    function GetUVec2: TVector2ui; virtual;
    function GetUVec3: TVector3ui; virtual;
    function GetUVec4: TVector4ui; virtual;

    procedure SetFloat(const Value: TGLfloat); virtual;
    procedure SetVec2(const Value: TVector2f); virtual;
    procedure SetVec3(const Value: TVector3f); virtual;
    procedure SetVec4(const Value: TVector4f); virtual;

    procedure SetInt(const Value: Integer); virtual;
    procedure SetIVec2(const Value: TVector2i); virtual;
    procedure SetIVec3(const Value: TVector3i); virtual;
    procedure SetIVec4(const Value: TVector4i); virtual;

    procedure SetUInt(const Value: GLuint); virtual;
    procedure SetUVec2(const Value: TVector2ui); virtual;
    procedure SetUVec3(const Value: TVector3ui); virtual;
    procedure SetUVec4(const Value: TVector4ui); virtual;

    function GetMat2: TMatrix2f; virtual;
    function GetMat3: TMatrix3f; virtual;
    function GetMat4: TMatrix4f; virtual;
    procedure SetMat2(const Value: TMatrix2f); virtual;
    procedure SetMat3(const Value: TMatrix3f); virtual;
    procedure SetMat4(const Value: TMatrix4f); virtual;

    procedure SetFloatArray(const Values: PGLFloat; Count: Integer); virtual;
    procedure SetIntArray(const Values: PGLInt; Count: Integer); virtual;
    procedure SetUIntArray(const Values: PGLUInt; Count: Integer); virtual;

    procedure WriteToFiler(AWriter: TWriter); virtual;
    procedure ReadFromFiler(AReader: TReader); virtual;
    procedure Apply(var ARci: TRenderContextInfo); virtual;
  end;

  CGLAbstractShaderUniform = class of TVKAbstractShaderUniform;

  // TVKShaderUniform
  //

  TVKShaderUniform = class(TVKAbstractShaderUniform, IShaderParameter)
  protected
    { Protected Declarations }
    FLocation: TGLint;
    FStoreProgram: TGLuint;
    FAutoSet: TUniformAutoSetMethod;
    function GetProgram: TGLuint;
{$IFDEF VKS_INLINE} inline;
{$ENDIF}
    procedure PushProgram;
{$IFDEF VKS_INLINE} inline;
{$ENDIF}
    procedure PopProgram;
{$IFDEF VKS_INLINE} inline;
{$ENDIF}

    function GetFloat: Single; override;
    function GetVec2: TVector2f; override;
    function GetVec3: TVector3f; override;
    function GetVec4: TVector; override;

    function GetInt: TGLint; override;
    function GetIVec2: TVector2i; override;
    function GetIVec3: TVector3i; override;
    function GetIVec4: TVector4i; override;

    function GetUInt: TGLuint; override;
    function GetUVec2: TVector2ui; override;
    function GetUVec3: TVector3ui; override;
    function GetUVec4: TVector4ui; override;

    procedure SetFloat(const Value: TGLfloat); override;
    procedure SetVec2(const Value: TVector2f); override;
    procedure SetVec3(const Value: TVector3f); override;
    procedure SetVec4(const Value: TVector4f); override;

    procedure SetInt(const Value: Integer); override;
    procedure SetIVec2(const Value: TVector2i); override;
    procedure SetIVec3(const Value: TVector3i); override;
    procedure SetIVec4(const Value: TVector4i); override;

    procedure SetUInt(const Value: GLuint); override;
    procedure SetUVec2(const Value: TVector2ui); override;
    procedure SetUVec3(const Value: TVector3ui); override;
    procedure SetUVec4(const Value: TVector4ui); override;

    function GetMat2: TMatrix2f; override;
    function GetMat3: TMatrix3f; override;
    function GetMat4: TMatrix4f; override;
    procedure SetMat2(const Value: TMatrix2f); override;
    procedure SetMat3(const Value: TMatrix3f); override;
    procedure SetMat4(const Value: TMatrix4f); override;

    function GetAutoSetMethod: string; override;
    procedure SetAutoSetMethod(const AValue: string); override;

    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  public
    { Public Declarations }
    procedure SetFloatArray(const Values: PGLFloat; Count: Integer); override;
    procedure SetIntArray(const Values: PGLInt; Count: Integer); override;
    procedure SetUIntArray(const Values: PGLUInt; Count: Integer); override;

    procedure Assign(Source: TPersistent); override;
    procedure Apply(var ARci: TRenderContextInfo); override;

    property Name: string read GetName;
    property Location: TGLint read FLocation;
    property GLSLType: TVKSLDataType read GetGLSLType;
  end;

  // TVKShaderUniformDSA
  //

  TVKShaderUniformDSA = class(TVKShaderUniform)
  protected
    { Protected Declarations }
    procedure SetFloat(const Value: TGLfloat); override;
    procedure SetVec2(const Value: TVector2f); override;
    procedure SetVec3(const Value: TVector3f); override;
    procedure SetVec4(const Value: TVector4f); override;

    procedure SetInt(const Value: Integer); override;
    procedure SetIVec2(const Value: TVector2i); override;
    procedure SetIVec3(const Value: TVector3i); override;
    procedure SetIVec4(const Value: TVector4i); override;

    procedure SetUInt(const Value: GLuint); override;
    procedure SetUVec2(const Value: TVector2ui); override;
    procedure SetUVec3(const Value: TVector3ui); override;
    procedure SetUVec4(const Value: TVector4ui); override;

    procedure SetMat2(const Value: TMatrix2f); override;
    procedure SetMat3(const Value: TMatrix3f); override;
    procedure SetMat4(const Value: TMatrix4f); override;
  public
    { Public Declarations }
    procedure SetFloatArray(const Values: PGLFloat; Count: Integer); override;
    procedure SetIntArray(const Values: PGLInt; Count: Integer); override;
    procedure SetUIntArray(const Values: PGLUInt; Count: Integer); override;
  end;

  // TVKUniformTexture
  //

  TVKShaderUniformTexture = class(TVKShaderUniform)
  private
    { Private Declarations }
    FLibTexture: TVKAbstractTexture;
    FLibSampler: TVKTextureSampler;
    FTarget: TVKTextureTarget;
    FSwizzling: TSwizzleVector;
  protected
    { Protected Declarations }
    FLibTexureName: TVKMaterialComponentName;
    FLibSamplerName: TVKMaterialComponentName;
    function GetTextureName: string; override;
    function GetSamplerName: string; override;
    function GetTextureSwizzle: TSwizzleVector; override;
    procedure SetTextureName(const AValue: string); override;
    procedure SetSamplerName(const AValue: string); override;
    procedure SetTextureSwizzle(const AValue: TSwizzleVector); override;

    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
    procedure Loaded;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;

    procedure Apply(var ARci: TRenderContextInfo); override;

    property LibTextureName: TVKMaterialComponentName read GetTextureName
      write SetTextureName;
    property LibSamplerName: TVKMaterialComponentName read GetSamplerName
      write SetSamplerName;
    property GLSLSampler: TVKSLSamplerType read GetGLSLSamplerType;
    property Swizzling: TSwizzleVector read GetTextureSwizzle write
      SetTextureSwizzle;
  end;

  // TVKBaseShaderModel
  //

  TVKBaseShaderModel = class(TVKLibMaterialProperty)
  protected
    { Protected Declarations }
    FHandle: TVKProgramHandle;
    FLibShaderName: array[TVKShaderType] of string;
    FShaders: array[TVKShaderType] of TVKShaderEx;
    FIsValid: Boolean;
    FInfoLog: string;
    FUniforms: TPersistentObjectList;
    FAutoFill: Boolean;

    function GetLibShaderName(AType: TVKShaderType): string;
    procedure SetLibShaderName(AType: TVKShaderType; const AValue: string);

    function GetUniform(const AName: string): IShaderParameter;
    class procedure ReleaseUniforms(AList: TPersistentObjectList);

    property LibVertexShaderName: TVKMaterialComponentName index shtVertex
      read GetLibShaderName write SetLibShaderName;
    property LibFragmentShaderName: TVKMaterialComponentName index shtFragment
      read GetLibShaderName write SetLibShaderName;
    property LibGeometryShaderName: TVKMaterialComponentName index shtGeometry
      read GetLibShaderName write SetLibShaderName;
    property LibTessEvalShaderName: TVKMaterialComponentName index shtEvaluation
      read GetLibShaderName write SetLibShaderName;
    property LibTessControlShaderName: TVKMaterialComponentName index shtControl
      read GetLibShaderName write SetLibShaderName;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadUniforms(AStream: TStream);
    procedure WriteUniforms(AStream: TStream);
    procedure Loaded; override;
    class function IsSupported: Boolean; virtual; abstract;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure NotifyChange(Sender: TObject); override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;

    procedure DoOnPrepare(Sender: TVKContext);
    procedure Apply(var ARci: TRenderContextInfo); virtual;
    procedure UnApply(var ARci: TRenderContextInfo); virtual;

    procedure GetUniformNames(Proc: TGetStrProc);

    property Handle: TVKProgramHandle read FHandle;
    property IsValid: Boolean read FIsValid;
    property Uniforms[const AName: string]: IShaderParameter read GetUniform;
  published
    { Published Declarations }
    // Compilation info log for design time
    property InfoLog: string read FInfoLog;
    // Turn on autofill of uniforms
    property AutoFillOfUniforms: Boolean read FAutoFill
      write FAutoFill stored False;
    property NextPass;
  end;

  TVKShaderModel3 = class(TVKBaseShaderModel)
  public
    { Public Declarations }
    class function IsSupported: Boolean; override;
  published
    { Published Declarations }
    property LibVertexShaderName;
    property LibFragmentShaderName;
  end;

  TVKShaderModel4 = class(TVKBaseShaderModel)
  public
    { Public Declarations }
    class function IsSupported: Boolean; override;
  published
    { Published Declarations }
    property LibVertexShaderName;
    property LibGeometryShaderName;
    property LibFragmentShaderName;
  end;

  TVKShaderModel5 = class(TVKBaseShaderModel)
  public
    { Public Declarations }
    procedure Apply(var ARci: TRenderContextInfo); override;
    procedure UnApply(var ARci: TRenderContextInfo); override;
    class function IsSupported: Boolean; override;
  published
    { Published Declarations }
    property LibTessControlShaderName;
    property LibTessEvalShaderName;
    property LibVertexShaderName;
    property LibGeometryShaderName;
    property LibFragmentShaderName;
  end;

  // TVKLibMaterialEx
  //

  TVKLibMaterialEx = class(TVKAbstractLibMaterial)
  private
    { Private Declarations }
    FHandle: TVKVirtualHandle;
    FApplicableLevel: TVKMaterialLevel;
    FSelectedLevel: TVKMaterialLevel;
    FFixedFunc: TVKFixedFunctionProperties;
    FMultitexturing: TVKMultitexturingProperties;
    FSM3: TVKShaderModel3;
    FSM4: TVKShaderModel4;
    FSM5: TVKShaderModel5;
    FOnAsmProgSetting: TOnAsmProgSetting;
    FOnSM3UniformInit: TOnUniformInitialize;
    FOnSM3UniformSetting: TOnUniformSetting;
    FOnSM4UniformInit: TOnUniformInitialize;
    FOnSM4UniformSetting: TOnUniformSetting;
    FOnSM5UniformInit: TOnUniformInitialize;
    FOnSM5UniformSetting: TOnUniformSetting;
    FNextPass: TVKLibMaterialEx;
    FStoreAmalgamating: Boolean;
    procedure SetLevel(AValue: TVKMaterialLevel);
    procedure SetFixedFunc(AValue: TVKFixedFunctionProperties);
    procedure SetMultitexturing(AValue: TVKMultitexturingProperties);
    procedure SetSM3(AValue: TVKShaderModel3);
    procedure SetSM4(AValue: TVKShaderModel4);
    procedure SetSM5(AValue: TVKShaderModel5);
    procedure DoAllocate(Sender: TVKVirtualHandle; var handle: TGLuint);
    procedure DoDeallocate(Sender: TVKVirtualHandle; var handle: TGLuint);
  protected
    procedure Loaded; override;
    procedure RemoveDefferedInit;
    procedure DoOnPrepare(Sender: TVKContext);
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
    property ApplicableLevel: TVKMaterialLevel read FApplicableLevel write
      SetLevel
      default mlAuto;
    property SelectedLevel: TVKMaterialLevel read FSelectedLevel;
    property FixedFunction: TVKFixedFunctionProperties
      read FFixedFunc write SetFixedFunc;
    property Multitexturing: TVKMultitexturingProperties
      read FMultitexturing write SetMultitexturing;
    property ShaderModel3: TVKShaderModel3 read FSM3 write SetSM3;
    property ShaderModel4: TVKShaderModel4 read FSM4 write SetSM4;
    property ShaderModel5: TVKShaderModel5 read FSM5 write SetSM5;

    // Asm vertex program event
    property OnAsmProgSetting: TOnAsmProgSetting read FOnAsmProgSetting
      write FOnAsmProgSetting;
    // Shader model 3 event
    property OnSM3UniformInitialize: TOnUniformInitialize read FOnSM3UniformInit
      write FOnSM3UniformInit;
    property OnSM3UniformSetting: TOnUniformSetting read FOnSM3UniformSetting
      write FOnSM3UniformSetting;
    // Shader model 4 event
    property OnSM4UniformInitialize: TOnUniformInitialize read FOnSM4UniformInit
      write FOnSM4UniformInit;
    property OnSM4UniformSetting: TOnUniformSetting read FOnSM4UniformSetting
      write FOnSM4UniformSetting;
    // Shader model 5 event
    property OnSM5UniformInitialize: TOnUniformInitialize read FOnSM5UniformInit
      write FOnSM5UniformInit;
    property OnSM5UniformSetting: TOnUniformSetting read FOnSM5UniformSetting
      write FOnSM5UniformSetting;
  end;

  // TVKLibMaterialsEx
  //

  TVKLibMaterialsEx = class(TVKAbstractLibMaterials)
  protected
    procedure SetItems(AIndex: Integer; const AValue: TVKLibMaterialEx);
    function GetItems(AIndex: Integer): TVKLibMaterialEx;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);

    function MaterialLibrary: TVKMaterialLibraryEx;

    function IndexOf(const Item: TVKLibMaterialEx): Integer;
    function Add: TVKLibMaterialEx;
    function FindItemID(ID: Integer): TVKLibMaterialEx;
    property Items[index: Integer]: TVKLibMaterialEx read GetItems
    write SetItems; default;
    function GetLibMaterialByName(const AName: TVKLibMaterialName):
      TVKLibMaterialEx;
  end;

  // TVKMatLibComponents
  //

  TVKMatLibComponents = class(TVKXCollection)
  protected
    { Protected Declarations }
    function GetItems(index: Integer): TVKBaseMaterialCollectionItem;
  public
    { Public Declarations }
    function GetNamePath: string; override;
    class function ItemsClass: TVKXCollectionItemClass; override;
    property Items[index: Integer]: TVKBaseMaterialCollectionItem
    read GetItems; default;

    function GetItemByName(const AName: TVKMaterialComponentName):
      TVKBaseMaterialCollectionItem;
    function GetTextureByName(const AName: TVKMaterialComponentName):
      TVKAbstractTexture;
    function GetAttachmentByName(const AName: TVKMaterialComponentName):
      TVKFrameBufferAttachment;
    function GetSamplerByName(const AName: TVKMaterialComponentName):
      TVKTextureSampler;
    function GetCombinerByName(const AName: TVKMaterialComponentName):
      TVKTextureCombiner;
    function GetShaderByName(const AName: TVKMaterialComponentName):
      TVKShaderEx;
    function GetAsmProgByName(const AName: TVKMaterialComponentName):
      TVKASMVertexProgram;
    function MakeUniqueName(const AName: TVKMaterialComponentName):
      TVKMaterialComponentName;
  end;

  // TVKMaterialLibraryEx
  //

  TVKMaterialLibraryEx = class(TVKAbstractMaterialLibrary)
  private
    { Private Declarations }
    FComponents: TVKMatLibComponents;
  protected
    { Protected Declarations }
    procedure Loaded; override;
    function GetMaterials: TVKLibMaterialsEx;
    procedure SetMaterials(AValue: TVKLibMaterialsEx);
    function StoreMaterials: Boolean;
    procedure SetComponents(AValue: TVKMatLibComponents);

    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteComponents(AStream: TStream);
    procedure ReadComponents(AStream: TStream);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetNames(Proc: TGetStrProc;
      AClass: CGLBaseMaterialCollectionItem); overload;

    function AddTexture(const AName: TVKMaterialComponentName):
      TVKTextureImageEx;
    function AddAttachment(const AName: TVKMaterialComponentName):
      TVKFrameBufferAttachment;
    function AddSampler(const AName: TVKMaterialComponentName):
      TVKTextureSampler;
    function AddCombiner(const AName: TVKMaterialComponentName):
      TVKTextureCombiner;
    function AddShader(const AName: TVKMaterialComponentName): TVKShaderEx;
    function AddAsmProg(const AName: TVKMaterialComponentName):
      TVKASMVertexProgram;

    procedure SetLevelForAll(const ALevel: TVKMaterialLevel);
  published
    { Published Declarations }
      { The materials collection. }
    property Materials: TVKLibMaterialsEx read GetMaterials write SetMaterials
      stored StoreMaterials;
    property Components: TVKMatLibComponents read FComponents
      write SetComponents;
    property TexturePaths;
  end;

procedure RegisterGLMaterialExNameChangeEvent(AEvent: TNotifyEvent);
procedure DeRegisterGLMaterialExNameChangeEvent(AEvent: TNotifyEvent);

implementation

const
  cTextureMagFilter: array[maNearest..maLinear] of TGLenum =
    (GL_NEAREST, GL_LINEAR);
  cTextureMinFilter: array[miNearest..miLinearMipmapLinear] of TGLenum =
    (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_LINEAR);
  cTextureWrapMode: array[twRepeat..twMirrorClampToBorder] of TGLenum =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER,
    GL_MIRRORED_REPEAT, GL_MIRROR_CLAMP_TO_EDGE_ATI,
      GL_MIRROR_CLAMP_TO_BORDER_EXT);
  cTextureCompareMode: array[tcmNone..tcmCompareRtoTexture] of TGLenum =
    (GL_NONE, GL_COMPARE_R_TO_TEXTURE);
  cSamplerToTexture: array[TVKSLSamplerType] of TVKTextureTarget =
    (
    ttNoShape,
    ttTexture1D,
    ttTexture2D,
    ttTexture3D,
    ttTextureCube,
    ttTexture1D,
    ttTexture2D,
    ttTexture1DArray,
    ttTexture2DArray,
    ttTexture1DArray,
    ttTexture1DArray,
    ttTextureCube,
    ttTexture1D,
    ttTexture2D,
    ttTexture3D,
    ttTextureCube,
    ttTexture1DArray,
    ttTexture2DArray,
    ttTexture1D,
    ttTexture2D,
    ttTexture3D,
    ttTextureCube,
    ttTexture1DArray,
    ttTexture2DArray,
    ttTextureRect,
    ttTextureRect,
    ttTextureBuffer,
    ttTextureRect,
    ttTextureBuffer,
    ttTextureRect,
    ttTextureBuffer,
    ttTexture2DMultisample,
    ttTexture2DMultisample,
    ttTexture2DMultisample,
    ttTexture2DMultisampleArray,
    ttTexture2DMultisampleArray,
    ttTexture2DMultisample
    );

  cTextureSwizzle: array[TVKTextureSwizzle] of TGLenum =
    (
    GL_RED,
    GL_GREEN,
    GL_BLUE,
    GL_ALPHA,
    GL_ZERO,
    GL_ONE
    );

const
  cTextureMode: array[TVKTextureMode] of TGLenum =
    (GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE, GL_ADD);

const
  cShaderTypeName: array[TVKShaderType] of string =
    ('vertex', 'control', 'evaluation', 'geomtery', 'fragment');

type
  TFriendlyImage = class(TVKBaseImage);

  TStandartUniformAutoSetExecutor = class
  public
    constructor Create;
    procedure SetModelMatrix(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetViewMatrix(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetProjectionMatrix(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetInvModelMatrix(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetModelViewMatrix(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetNormalModelMatrix(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetInvModelViewMatrix(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetViewProjectionMatrix(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetWorldViewProjectionMatrix(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetCameraPosition(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    // Lighting
    procedure SetLightSource0Position(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    // Material
    procedure SetMaterialFrontAmbient(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetMaterialFrontDiffuse(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetMaterialFrontSpecular(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetMaterialFrontEmission(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetMaterialFrontShininess(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetMaterialBackAmbient(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetMaterialBackDiffuse(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetMaterialBackSpecular(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetMaterialBackShininess(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
    procedure SetMaterialBackEmission(Sender: IShaderParameter; var ARci:
      TRenderContextInfo);
  end;

var
  vGLMaterialExNameChangeEvent: TNotifyEvent;
  vStandartUniformAutoSetExecutor: TStandartUniformAutoSetExecutor;
  vStoreBegin: procedure(mode: TGLenum);
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}

procedure RegisterGLMaterialExNameChangeEvent(AEvent: TNotifyEvent);
begin
  vGLMaterialExNameChangeEvent := AEvent;
end;

procedure DeRegisterGLMaterialExNameChangeEvent(AEvent: TNotifyEvent);
begin
  vGLMaterialExNameChangeEvent := nil;
end;

function ComputeNameHashKey(
  const AName: string): Integer;
var
  i, n: Integer;
begin
  n := Length(AName);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(AName[i]);
end;

procedure Div2(var Value: Integer);
{$IFDEF VKS_INLINE} inline;
{$ENDIF}
begin
  Value := Value div 2;
  if Value = 0 then
    Value := 1;
end;

function CalcTextureLevelNumber(ATarget: TVKTextureTarget; w, h, d: Integer):
  Integer;
begin
  Result := 0;

  case ATarget of

    ttNoShape: ;

    ttTexture1D, ttTexture1DArray, ttTextureCube, ttTextureCubeArray:
      repeat
        Inc(Result);
        Div2(w);
      until w <= 1;

    ttTexture2D, ttTexture2DArray:
      repeat
        Inc(Result);
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

    ttTextureRect, ttTextureBuffer,
      ttTexture2DMultisample, ttTexture2DMultisampleArray:
      Result := 1;
  end;
end;

{$IFDEF VKS_REGION}{$REGION 'TVKBaseMaterialCollectionItem'}{$ENDIF}

destructor TVKBaseMaterialCollectionItem.Destroy;
var
  I: Integer;
begin
  if Assigned(FUserList) then
  begin
    FNotifying := True;
    for I := FUserList.Count - 1 downto 0 do
      TVKLibMaterialProperty(FUserList[I]).Notification(Self, opRemove);
    FreeAndNil(FUserList);
  end;
  inherited;
end;

function TVKBaseMaterialCollectionItem.GetMaterialLibrary:
  TVKAbstractMaterialLibrary;
begin
  Result := TVKAbstractMaterialLibrary(TVKMatLibComponents(Owner).Owner);
end;

function TVKBaseMaterialCollectionItem.GetMaterialLibraryEx:
  TVKMaterialLibraryEx;
begin
  Result := TVKMaterialLibraryEx(TVKMatLibComponents(Owner).Owner);
end;

function TVKBaseMaterialCollectionItem.GetUserCount: Integer;
begin
  if Assigned(FUserList) then
    Result := FUserList.Count
  else
    Result := 0;
end;

function TVKBaseMaterialCollectionItem.GetUserList: TPersistentObjectList;
begin
  if FUserList = nil then
  begin
    FUserList := TPersistentObjectList.Create;
    FNotifying := False;
  end;
  Result := FUserList;
end;

procedure TVKBaseMaterialCollectionItem.NotifyChange(Sender: TObject);
var
  I: Integer;
begin
  if FNotifying then
    exit;
  FNotifying := True;
  if GetUserCount > 0 then
    for I := 0 to FUserList.Count - 1 do
      TVKUpdateAbleObject(FUserList[I]).NotifyChange(Self);
  FNotifying := False;
end;

procedure TVKBaseMaterialCollectionItem.RegisterUser(
  AUser: TVKUpdateAbleObject);
begin
  if not FNotifying and (UserList.IndexOf(AUser) < 0) then
    UserList.Add(AUser);
end;

procedure TVKBaseMaterialCollectionItem.UnregisterUser(
  AUser: TVKUpdateAbleObject);
begin
  if not FNotifying then
    UserList.Remove(AUser);
end;

procedure TVKBaseMaterialCollectionItem.SetName(const AValue: string);
begin
  if AValue <> Name then
  begin
    if not IsValidIdent(AValue) then
    begin
      if IsDesignTime then
        InformationDlg(AValue + ' - is not valid component name');
      exit;
    end;
    if not (csLoading in MaterialLibrary.ComponentState) then
    begin
      if TVKMatLibComponents(Owner).GetItemByName(AValue) <> Self then
        inherited SetName(TVKMatLibComponents(Owner).MakeUniqueName(AValue))
      else
        inherited SetName(AValue);
    end
    else
      inherited SetName(AValue);
    FNameHashKey := ComputeNameHashKey(Name);
    // Notify users
    NotifyChange(Self);
    // Notify designer
    if Assigned(vGLMaterialExNameChangeEvent) then
      vGLMaterialExNameChangeEvent(Self);
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKFixedFunctionProperties'}{$ENDIF}

procedure TVKFixedFunctionProperties.Apply(var ARci: TRenderContextInfo);
begin
  with ARci.GLStates do
  begin
    Disable(stColorMaterial);
    PolygonMode := FPolygonMode;

    // Fixed functionality state
    if not ARci.GLStates.ForwardContext then
    begin
      // Lighting switch
      if (moNoLighting in MaterialOptions) or not ARci.bufferLighting then
      begin
        Disable(stLighting);
        FFrontProperties.ApplyNoLighting(ARci, cmFront);
      end
      else
      begin
        Enable(stLighting);
        FFrontProperties.Apply(ARci, cmFront);
      end;

      if FPolygonMode = pmLines then
        Disable(stLineStipple);

      // Fog switch
      if (moIgnoreFog in MaterialOptions) or not ARci.bufferFog then
        Disable(stFog)
      else
        Enable(stFog);
    end;

    // Apply FaceCulling and BackProperties (if needs be)
    case FFaceCulling of
      fcBufferDefault:
        begin
          if ARci.bufferFaceCull then
            Enable(stCullFace)
          else
            Disable(stCullFace);
          BackProperties.Apply(ARci, cmBack);
        end;
      fcCull: Enable(stCullFace);
      fcNoCull:
        begin
          Disable(stCullFace);
          BackProperties.Apply(ARci, cmBack);
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
            Disable(stBlend);
            Disable(stAlphaTest);
          end;
        bmTransparency:
          begin
            Enable(stBlend);
            Enable(stAlphaTest);
            SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
            SetGLAlphaFunction(cfGreater, 0);
          end;
        bmAdditive:
          begin
            Enable(stBlend);
            Enable(stAlphaTest);
            SetBlendFunc(bfSrcAlpha, bfOne);
            SetGLAlphaFunction(cfGreater, 0);
          end;
        bmAlphaTest50:
          begin
            Disable(stBlend);
            Enable(stAlphaTest);
            SetGLAlphaFunction(cfGEqual, 0.5);
          end;
        bmAlphaTest100:
          begin
            Disable(stBlend);
            Enable(stAlphaTest);
            SetGLAlphaFunction(cfGEqual, 1.0);
          end;
        bmModulate:
          begin
            Enable(stBlend);
            Enable(stAlphaTest);
            SetBlendFunc(bfDstColor, bfZero);
            SetGLAlphaFunction(cfGreater, 0);
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
    if ARci.currentMaterialLevel = mlFixedFunction then
    begin
      if FTexProp.Enabled and FTexProp.IsValid then
      begin
        ARci.GLStates.ActiveTexture := 0;
        FTexProp.Apply(ARci);
        GL.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE,
          cTextureMode[FTextureMode]);
      end;
    end;

  end;
end;

procedure TVKFixedFunctionProperties.Assign(Source: TPersistent);
var
  LFFP: TVKFixedFunctionProperties;
begin
  if Source is TVKFixedFunctionProperties then
  begin
    LFFP := TVKFixedFunctionProperties(Source);
    if Assigned(LFFP.FBackProperties) then
      BackProperties.Assign(LFFP.BackProperties)
    else
      FreeAndNil(FBackProperties);
    FFrontProperties.Assign(LFFP.FFrontProperties);
    FPolygonMode := LFFP.FPolygonMode;
    FBlendingMode := LFFP.FBlendingMode;
    FMaterialOptions := LFFP.FMaterialOptions;
    FFaceCulling := LFFP.FFaceCulling;
    FDepthProperties.Assign(LFFP.FDepthProperties);
    FTexProp.Assign(LFFP.FTexProp);
    FTextureMode := LFFP.TextureMode;
    NotifyChange(Self);
  end;
  inherited;
end;

function TVKFixedFunctionProperties.Blended: Boolean;
begin
  Result := not (FBlendingMode in [bmOpaque, bmAlphaTest50, bmAlphaTest100, bmCustom]);
end;

constructor TVKFixedFunctionProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FFrontProperties := TVKFaceProperties.Create(Self);
  FFaceCulling := fcBufferDefault;
  FPolygonMode := pmFill;
  FBlendingParams := TVKBlendingParameters.Create(Self);
  FDepthProperties := TVKDepthProperties.Create(Self);
  FTexProp := TVKTextureProperties.Create(Self);
  FTextureMode := tmDecal;
  FEnabled := True;
end;

destructor TVKFixedFunctionProperties.Destroy;
begin
  FFrontProperties.Destroy;
  FBackProperties.Free;
  FDepthProperties.Destroy;
  FBlendingParams.Destroy;
  FTexProp.Destroy;
  inherited;
end;

function TVKFixedFunctionProperties.GetBackProperties: TVKFaceProperties;
begin
  if not Assigned(FBackProperties) then
    FBackProperties := TVKFaceProperties.Create(Self);
  Result := FBackProperties;
end;

procedure TVKFixedFunctionProperties.SetBackProperties(AValues:
  TVKFaceProperties);
begin
  BackProperties.Assign(AValues);
  NotifyChange(Self);
end;

procedure TVKFixedFunctionProperties.SetBlendingMode(const AValue:
  TBlendingMode);
begin
  if AValue <> FBlendingMode then
  begin
    FBlendingMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKFixedFunctionProperties.SetBlendingParams(const AValue:
  TVKBlendingParameters);
begin
  FBlendingParams.Assign(AValue);
  NotifyChange(Self);
end;

procedure TVKFixedFunctionProperties.SetDepthProperties(AValues:
  TVKDepthProperties);
begin
  FDepthProperties.Assign(AValues);
  NotifyChange(Self);
end;

procedure TVKFixedFunctionProperties.SetTexProp(AValue: TVKTextureProperties);
begin
  FTexProp.Assign(AValue);
end;

procedure TVKFixedFunctionProperties.SetTextureMode(AValue: TVKTextureMode);
begin
  if AValue <> FTextureMode then
  begin
    FTextureMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKFixedFunctionProperties.SetFaceCulling(const AValue: TFaceCulling);
begin
  if AValue <> FFaceCulling then
  begin
    FFaceCulling := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKFixedFunctionProperties.SetFrontProperties(AValues:
  TVKFaceProperties);
begin
  FFrontProperties.Assign(AValues);
  NotifyChange(Self);
end;

procedure TVKFixedFunctionProperties.SetMaterialOptions(const AValue:
  TMaterialOptions);
begin
  if AValue <> FMaterialOptions then
  begin
    FMaterialOptions := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKFixedFunctionProperties.SetPolygonMode(AValue: TPolygonMode);
begin
  if AValue <> FPolygonMode then
  begin
    FPolygonMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKFixedFunctionProperties.UnApply(var ARci: TRenderContextInfo);
begin
  if FTexProp.Enabled and FTexProp.IsValid then
    FTexProp.UnApply(ARci);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKAbstractTexture'}{$ENDIF}

function TVKAbstractTexture.GetTextureTarget: TVKTextureTarget;
begin
  Result := FHandle.Target;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKTextureImageEx'}{$ENDIF}

procedure TVKTextureImageEx.Apply(var ARci: TRenderContextInfo);
begin
  if FIsValid then
  begin
    // Just bind
    with ARci.GLStates do
    begin
      TextureBinding[ActiveTexture, FHandle.Target] := FHandle.Handle;
      ActiveTextureEnabled[FHandle.Target] := True;
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
  else with ARci.GLStates do
    TextureBinding[ActiveTexture, FHandle.Target] := 0;
end;

procedure TVKTextureImageEx.Assign(Source: TPersistent);
var
  LTexture: TVKTextureImageEx;
begin
  if Source is TVKTextureImageEx then
  begin
    LTexture := TVKTextureImageEx(Source);
    FCompression := LTexture.FCompression;
    if Assigned(LTexture.FImage) then
    begin
      if not Assigned(FImage) then
        FImage := TVKImage.Create;
      FImage.Assign(LTexture.FImage);
    end
    else
      FreeAndNil(FImage);
    FImageAlpha := LTexture.FImageAlpha;
    FImageBrightness := LTexture.FImageBrightness;
    FImageGamma := LTexture.FImageGamma;
    FHeightToNormalScale := LTexture.FHeightToNormalScale;
    FSourceFile := LTexture.FSourceFile;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TVKTextureImageEx.Create(AOwner: TVKXCollection);
begin
  inherited;
  FDefferedInit := False;
  FHandle := TVKTextureHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FCompression := tcDefault;
  FImageAlpha := tiaDefault;
  FImageBrightness := 1.0;
  FImageGamma := 1.0;
  FHeightToNormalScale := 1.0;
  FInternalFormat := tfRGBA8;
  FInternallyStored := False;
  FMipGenMode := mgmOnFly;
  FUseStreaming := False;
  Name := TVKMatLibComponents(AOwner).MakeUniqueName('Texture');
end;

destructor TVKTextureImageEx.Destroy;
begin
  FHandle.Destroy;
  FImage.Free;
  inherited;
end;

procedure TVKTextureImageEx.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TVKTextureImageEx.DoOnPrepare(Sender: TVKContext);
var
  LTarget: TVKTextureTarget;
  rowSize: Integer;
begin
  if IsDesignTime and FDefferedInit then
    exit;

  FHandle.AllocateHandle;
  if not FHandle.IsDataNeedUpdate then
    exit;

  try
    PrepareImage;

    // Target
    LTarget := FImage.GetTextureTarget;

    // Check supporting
    if not IsTargetSupported(LTarget)
      or not IsFormatSupported(FInternalFormat) then
      Abort;

    if (FHandle.Target <> LTarget)
      and (FHandle.Target <> ttNoShape) then
    begin
      FHandle.DestroyHandle;
      FHandle.AllocateHandle;
    end;
    FHandle.Target := LTarget;

    // Check streaming support
    if not IsDesignTime then
    begin
      FUseStreaming := FUseStreaming and TVKUnpackPBOHandle.IsSupported;
      FUseStreaming := FUseStreaming and IsServiceContextAvaible;
      FUseStreaming := FUseStreaming and (LTarget = ttTexture2D);
    end;

    with Sender.GLStates do
    begin
      ActiveTextureEnabled[FHandle.Target] := True;
      TextureBinding[ActiveTexture, FHandle.Target] := FHandle.Handle;
      UnpackRowLength := 0;
      UnpackSkipRows := 0;
      UnpackSkipPixels := 0;
      rowSize := FImage.LevelWidth[0] * FImage.ElementSize;
      if (rowSize mod 8 = 0) and (FImage.ElementSize > 4) then
        UnpackAlignment := 8
      else
      if rowSize mod 4 = 0 then
        UnpackAlignment := 4
      else if rowSize mod 2 = 0 then
        UnpackAlignment := 2
      else
        UnpackAlignment := 1;
    end;

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
    FIsValid := True;
  except
    FIsValid := False;
  end;
end;

procedure TVKTextureImageEx.FullTransfer;
var
  LCompression: TVKTextureCompression;
  glFormat: TGLenum;
begin
  with GL do
  begin
    if ARB_texture_compression then
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
      with CurrentGLContext.GLStates do
      begin
        case LCompression of
          tcStandard: TextureCompressionHint := hintDontCare;
          tcHighQuality: TextureCompressionHint := hintNicest;
          tcHighSpeed: TextureCompressionHint := hintFastest;
        else
          Assert(False, vksErrorEx + vksUnknownType);
        end;
        if not GetGenericCompressedFormat(
          FInternalFormat,
          FImage.ColorFormat,
          glFormat) then
          glFormat := InternalFormatToOpenGLFormat(FInternalFormat);
      end
    else
      glFormat := InternalFormatToOpenGLFormat(FInternalFormat);

    FImage.RegisterAsOpenGLTexture(
      FHandle,
      FMipGenMode = mgmOnFly,
      glFormat,
      FWidth,
      FHeight,
      FDepth);

    if GetError <> GL_NO_ERROR then
    begin
      ClearError;
      CurrentGLContext.GLStates.ActiveTextureEnabled[FHandle.Target] := False;
      GLSLogger.LogErrorFmt('Unable to create texture "%s"', [Self.Name]);
      Abort;
    end
    else
      FHandle.NotifyDataUpdated;
  end;
end;

procedure TVKTextureImageEx.CalcLODRange(out AFirstLOD, ALastLOD: Integer);
var
  I, MaxLODSize, MinLODSize, MaxLODZSize: Integer;
begin
  case FHandle.Target of
    ttTexture3D:
      begin
        MaxLODSize := CurrentGLContext.GLStates.Max3DTextureSize;
        MaxLODZSize := MaxLODSize;
      end;

    ttTextureCube:
      begin
        MaxLODSize := CurrentGLContext.GLStates.MaxCubeTextureSize;
        MaxLODZSize := 0;
      end;

    ttTexture1DArray,
      ttTexture2DArray,
      ttTextureCubeArray,
      ttTexture2DMultisampleArray:
      begin
        MaxLODSize := CurrentGLContext.GLStates.MaxTextureSize;
        MaxLODZSize := CurrentGLContext.GLStates.MaxArrayTextureSize;
      end;

  else
    begin
      MaxLODSize := CurrentGLContext.GLStates.MaxTextureSize;
      MaxLODZSize := 0;
    end;
  end;

  MinLODSize := 1;

  AFirstLOD := 0;

  for I := 0 to High(TVKImagePiramid) do
  begin
    if (FImage.LevelWidth[I] <= MaxLODSize)
      and (FImage.LevelHeight[I] <= MaxLODSize)
      and (FImage.LevelDepth[I] <= MaxLODZSize) then
      break;
    Inc(AFirstLOD);
  end;

  AFirstLOD := MinInteger(AFirstLOD, FImage.LevelCount - 1);
  ALastLOD := AFirstLOD;

  for I := AFirstLOD to High(TVKImagePiramid) do
  begin
    if (FImage.LevelWidth[I] < MinLODSize)
      or (FImage.LevelHeight[I] < MinLODSize) then
      break;
    Inc(ALastLOD);
  end;
  ALastLOD := MinInteger(ALastLOD, FImage.LevelCount - 1);
end;

procedure TVKTextureImageEx.StreamTransfer;
var
  LImage: TFriendlyImage;
  bContinueStreaming: Boolean;
  OldBaseLevel, level: Integer;
  newTime: Double;
  glInternalFormat: TGLenum;
  transferMethod: 0..3;
begin
  LImage := TFriendlyImage(FImage);
  OldBaseLevel := FBaseLevel;
  CalcLODRange(FBaseLevel, FMaxLevel);

  // Select transfer method
  if FImage.IsCompressed then
    transferMethod := 1
  else
    transferMethod := 0;
  if GL.EXT_direct_state_access then
    transferMethod := transferMethod + 2;

  bContinueStreaming := False;
  for level := FMaxLevel downto FBaseLevel do
  begin

    case LImage.LevelStreamingState[level] of

      ssKeeping:
        begin
          if FBaseLevel < Level then
            FBaseLevel := FMaxLevel;
          LImage.LevelStreamingState[Level] := ssLoading;
          LImage.DoStreaming;
          bContinueStreaming := True;
        end;

      ssLoading:
        begin
          LImage.DoStreaming;
          bContinueStreaming := True;
          if FBaseLevel < Level then
            FBaseLevel := FMaxLevel;
        end;

      ssLoaded:
        with GL do
        begin
          LImage.LevelPixelBuffer[Level].AllocateHandle;
          LImage.LevelPixelBuffer[Level].Bind;
          glInternalFormat := InternalFormatToOpenGLFormat(FInternalFormat);
          case transferMethod of
            0: TexImage2D(GL_TEXTURE_2D, Level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.ColorFormat, FImage.DataType, nil);
            1: CompressedTexImage2D(GL_TEXTURE_2D, Level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.LevelSizeInByte[Level], nil);
            2: TextureImage2D(FHandle.Handle, GL_TEXTURE_2D, Level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.ColorFormat, FImage.DataType, nil);
            3: CompressedTextureImage2D(FHandle.Handle, GL_TEXTURE_2D, Level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.LevelSizeInByte[Level], nil);
          end;
          LImage.LevelPixelBuffer[Level].UnBind;
          LImage.LevelStreamingState[Level] := ssTransfered;
          GLSLogger.LogDebug(Format('Texture "%s" level %d loaded', [Name, Level]));
        end;

      ssTransfered:
        begin
          if LImage.LevelPixelBuffer[Level].IsAllocatedForContext then
            LImage.LevelPixelBuffer[Level].DestroyHandle;
          FBaseLevel := Level;
        end;
    end; // of case

    if bContinueStreaming then
      break;
  end; // for level

  if bContinueStreaming then
  with GL do
  begin
    TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, FMaxLevel);
    TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, FBaseLevel);
  end;


  // Smooth transition between levels
  if Assigned(FApplicableSampler) then
  with FApplicableSampler do
  begin
    newTime := GLSTime;
    if FLODBiasFract > 0 then
      FLODBiasFract := FLODBiasFract - 0.05 * (newTime - FLastTime)
    else if FLODBiasFract < 0 then
      FLODBiasFract := 0;
    FLastTime := newTime;
    if OldBaseLevel > FBaseLevel then
      FLODBiasFract := FLODBiasFract + (OldBaseLevel - FBaseLevel);

    if FApplicableSampler.IsValid then
      GL.SamplerParameterf(FApplicableSampler.Handle.Handle,
        GL_TEXTURE_LOD_BIAS, FLODBias + FLODBiasFract)
    else
      // To refrash texture parameters when sampler object not supported
      FLastSampler := nil;
  end;
end;

class function TVKTextureImageEx.FriendlyName: string;
begin
  Result := 'Texture Image';
end;

procedure TVKTextureImageEx.PrepareImage;
const
  cAlphaProc: array[TVKTextureImageAlpha] of TImageAlphaProc =
    (
    nil,
    ImageAlphaFromIntensity,
    ImageAlphaSuperBlackTransparent,
    ImageAlphaLuminance,
    ImageAlphaLuminanceSqrt,
    ImageAlphaOpaque,
    ImageAlphaTopLeftPointColorTransparent,
    ImageAlphaInverseLuminance,
    ImageAlphaInverseLuminanceSqrt,
    ImageAlphaBottomRightPointColorTransparent
    );

var
  ext, filename: string;
  BaseImageClass: TVKBaseImageClass;
  LPicture: TVKPicture;
  LGraphic: TVKGraphic;
  LImage: TVKImage;
  level: Integer;
  glColorFormat, glDataType: TGLenum;
  bReadFromSource: Boolean;
  LStream: TStream;
  ptr: PByte;

  procedure ReplaceImageClass;
  begin
    if not (FImage is TVKImage) then
    begin
      LImage := TVKImage.Create;
      LImage.Assign(FImage);
      FImage.Destroy;
      FImage := LImage;
    end
    else
      LImage := TVKImage(FImage);
  end;

begin
  if not Assigned(FImage) then
  begin
    try
      SetExeDirectory;
      bReadFromSource := True;

      if FInternallyStored and not IsDesignTime then
      begin
        filename := Name+'.image';
        if FileStreamExists(filename) then
        begin
          FImage := TVKImage.Create;
          FImage.ResourceName := filename;
          TFriendlyImage(FImage).LoadHeader;
          if not FUseStreaming then
          begin
            ReallocMem(TFriendlyImage(FImage).fData, FImage.DataSize);
            for level := FImage.LevelCount - 1 downto 0 do
            begin
              LStream := CreateFileStream(filename + IntToHex(level, 2), fmOpenRead);
              ptr := PByte(TFriendlyImage(FImage).GetLevelAddress(level));
              LStream.Read(ptr^, FImage.LevelSizeInByte[level]);
              LStream.Destroy;
            end;
          end;
          bReadFromSource := False;
        end
        else
        begin
          FInternallyStored := False;
          FUseStreaming := False;
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
            FImage := TVKImage.Create;
            if ApplicationFileIODefined then
            begin
              LGraphic := CreateGraphicFromFile(FSourceFile);
              FImage.Assign(LGraphic);
              LGraphic.Free;
            end
            else
            begin
               { TODO : E2035 Not enough actual parameters }
              (*LPicture := TVKPicture.Create;*)
              LPicture.Bitmap.LoadFromFile(FSourceFile);
              FImage.Assign(LPicture.Bitmap);
              LPicture.Destroy;
            end;
          end;

          if FInternalFormat <> FImage.InternalFormat then
          begin
            ReplaceImageClass;
            FindCompatibleDataFormat(FInternalFormat, glColorFormat, glDataType);
            TVKImage(FImage).SetColorFormatDataType(glColorFormat, glDataType);
            TFriendlyImage(FImage).fInternalFormat := FInternalFormat;
          end;

          if (ImageAlpha <> tiaDefault)
            or (FImageBrightness <> 1.0)
            or (FImageGamma <> 1.0) then
          begin
            ReplaceImageClass;
            for level := 0 to FImage.LevelCount - 1 do
            begin
              AlphaGammaBrightCorrection(
                TFriendlyImage(FImage).GetLevelAddress(level),
                FImage.ColorFormat,
                FImage.DataType,
                FImage.LevelWidth[level],
                FImage.LevelHeight[level],
                cAlphaProc[ImageAlpha],
                FImageBrightness,
                FImageGamma);
            end;
          end
          else if FHeightToNormalScale <> 1.0 then
          begin
            ReplaceImageClass;
  //          HeightToNormalMap();
  {$Message Hint 'TVKTextureImageEx.HeightToNormalScale not yet implemented' }
          end;

          case FMipGenMode of
            mgmNoMip:
              FImage.UnMipmap;

            mgmLeaveExisting, mgmOnFly: ;

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
            filename := Name+'.image';
            FImage.ResourceName := filename;
            TFriendlyImage(FImage).SaveHeader;
            for level := FImage.LevelCount - 1 downto 0 do
            begin
              LStream := CreateFileStream(filename + IntToHex(level, 2),
                fmOpenWrite or fmCreate);
              ptr := PByte(TFriendlyImage(FImage).GetLevelAddress(level));
              LStream.Write(ptr^, FImage.LevelSizeInByte[level]);
              LStream.Destroy;
            end;
          end;

        end
        else
        begin // no SourceFile
          FImage := TVKImage.Create;
          FImage.SetErrorImage;
          GLSLogger.LogErrorFmt('Source file of texture "%s" image not found',
            [Self.Name]);
        end;
      end; // if bReadFromSource

    except
      on E: Exception do
      begin
        FImage.Free;
        FImage := TVKImage.Create;
        FImage.SetErrorImage;
        if IsDesignTime then
          InformationDlg(Self.Name + ' - ' + E.ClassName + ': ' + E.Message)
        else
          GLSLogger.LogError(Self.Name + ' - ' + E.ClassName + ': ' +
            E.Message);
      end;
    end;
  end; // of not Assigned
end;

procedure TVKTextureImageEx.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name := ReadString;
      FDefferedInit := ReadBoolean;
      FInternalFormat := TGLInternalFormat(ReadInteger);
      FCompression := TVKTextureCompression(ReadInteger);
      FImageAlpha := TVKTextureImageAlpha(ReadInteger);
      FImageBrightness := ReadFloat;
      FImageBrightness := ReadFloat;
      FImageGamma := ReadFloat;
      FHeightToNormalScale := ReadFloat;
      FSourceFile := ReadString;
      FInternallyStored := ReadBoolean;
      FMipGenMode := TMipmapGenerationMode(ReadInteger);
      FUseStreaming := ReadBoolean;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TVKTextureImageEx.SetCompression(const AValue: TVKTextureCompression);
begin
  if AValue <> FCompression then
  begin
    FCompression := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKTextureImageEx.SetImageAlpha(const AValue: TVKTextureImageAlpha);
begin
  if FImageAlpha <> AValue then
  begin
    FImageAlpha := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TVKTextureImageEx.SetImageBrightness(const AValue: Single);
begin
  if FImageBrightness <> AValue then
  begin
    FImageBrightness := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TVKTextureImageEx.SetImageGamma(const AValue: Single);
begin
  if FImageGamma <> AValue then
  begin
    FImageGamma := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TVKTextureImageEx.SetInternalFormat(const AValue: TGLInternalFormat);
begin
  if AValue <> FInternalFormat then
  begin
    FInternalFormat := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TVKTextureImageEx.SetInternallyStored(const AValue: Boolean);
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

procedure TVKTextureImageEx.SetMipGenMode(const AValue: TMipmapGenerationMode);
begin
  if FMipGenMode <> AValue then
  begin
    FMipGenMode := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TVKTextureImageEx.SetNormalMapScale(const AValue: Single);
begin
  if AValue <> FHeightToNormalScale then
  begin
    FHeightToNormalScale := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKTextureImageEx.SetSourceFile(AValue: string);
begin
  FixPathDelimiter(AValue);
  if FSourceFile <> AValue then
  begin
    FSourceFile := AValue;
    FUseStreaming := False;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TVKTextureImageEx.SetUseStreaming(const AValue: Boolean);
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

function TVKTextureImageEx.StoreBrightness: Boolean;
begin
  Result := (FImageBrightness <> 1.0);
end;

function TVKTextureImageEx.StoreGamma: Boolean;
begin
  Result := (FImageGamma <> 1.0);
end;

function TVKTextureImageEx.StoreNormalMapScale: Boolean;
begin
  Result := (FHeightToNormalScale <> cDefaultNormalMapScale);
end;

procedure TVKTextureImageEx.UnApply(var ARci: TRenderContextInfo);
begin
  ARci.GLStates.ActiveTextureEnabled[FHandle.Target] := False;
end;

procedure TVKTextureImageEx.WriteToFiler(AWriter: TWriter);
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

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKTextureSampler'}{$ENDIF}

procedure TVKTextureSampler.Apply(var ARci: TRenderContextInfo);
begin
  if FIsValid then
    ARci.GLStates.SamplerBinding[ARci.GLStates.ActiveTexture] := FHandle.Handle;
end;

procedure TVKTextureSampler.Assign(Source: TPersistent);
var
  LSampler: TVKTextureSampler;
begin
  if Source is TVKTextureSampler then
  begin
    LSampler := TVKTextureSampler(Source);
    FMinFilter := LSampler.FMinFilter;
    FMagFilter := LSampler.FMagFilter;
    FFilteringQuality := LSampler.FFilteringQuality;
    FLODBias := LSampler.FLODBias;
    FLODBiasFract := 0;
    FBorderColor.Assign(LSampler.FBorderColor);
    FWrap := LSampler.FWrap;
    FCompareMode := LSampler.FCompareMode;
    FCompareFunc := LSampler.FCompareFunc;
    FDecodeSRGB := LSampler.FDecodeSRGB;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TVKTextureSampler.Create(AOwner: TVKXCollection);
begin
  inherited;
  FDefferedInit := False;
  FHandle := TVKSamplerHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FMagFilter := maLinear;
  FMinFilter := miLinearMipMapLinear;
  FFilteringQuality := tfAnisotropic;
  FLODBias := 0;
  FLODBiasFract := 0;
  FWrap[0] := twRepeat;
  FWrap[1] := twRepeat;
  FWrap[2] := twRepeat;
  FBorderColor := TVKColor.CreateInitialized(Self, clrTransparent);
  FCompareMode := tcmNone;
  FCompareFunc := cfLequal;
  FDecodeSRGB := True;
  Name := TVKMatLibComponents(AOwner).MakeUniqueName('Sampler');
end;

destructor TVKTextureSampler.Destroy;
begin
  FHandle.Destroy;
  FBorderColor.Destroy;
  inherited;
end;

function TVKTextureSampler.GetWrap(Index: Integer): TVKSeparateTextureWrap;
begin
  Result := FWrap[Index];
end;

procedure TVKTextureSampler.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TVKTextureSampler.DoOnPrepare(Sender: TVKContext);
var
  ID: TGLuint;
begin
  if IsDesignTime and FDefferedInit then
    exit;
  try
    if FHandle.IsSupported then
    begin
      FHandle.AllocateHandle;
      ID := FHandle.Handle;
      if FHandle.IsDataNeedUpdate then
        with Sender.GL do
        begin
          SamplerParameterfv(ID, GL_TEXTURE_BORDER_COLOR,
            FBorderColor.AsAddress);
          SamplerParameteri(ID, GL_TEXTURE_WRAP_S, cTextureWrapMode[FWrap[0]]);
          SamplerParameteri(ID, GL_TEXTURE_WRAP_T, cTextureWrapMode[FWrap[1]]);
          SamplerParameteri(ID, GL_TEXTURE_WRAP_R, cTextureWrapMode[FWrap[2]]);
          SamplerParameterf(ID, GL_TEXTURE_LOD_BIAS, FLODBias + FLODBiasFract);
          SamplerParameteri(ID, GL_TEXTURE_MIN_FILTER,
            cTextureMinFilter[FMinFilter]);
          SamplerParameteri(ID, GL_TEXTURE_MAG_FILTER,
            cTextureMagFilter[FMagFilter]);

          if EXT_texture_filter_anisotropic then
          begin
            if FFilteringQuality = tfAnisotropic then
              SamplerParameteri(ID, GL_TEXTURE_MAX_ANISOTROPY_EXT,
                CurrentGLContext.GLStates.MaxTextureAnisotropy)
            else
              SamplerParameteri(ID, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
          end;

          SamplerParameteri(ID, GL_TEXTURE_COMPARE_MODE,
            cTextureCompareMode[FCompareMode]);
          SamplerParameteri(ID, GL_TEXTURE_COMPARE_FUNC,
            cGLComparisonFunctionToGLEnum[FCompareFunc]);

          if EXT_texture_sRGB_decode then
          begin
            if FDecodeSRGB then
              SamplerParameteri(ID, GL_TEXTURE_SRGB_DECODE_EXT, GL_DECODE_EXT)
            else
              SamplerParameteri(ID, GL_TEXTURE_SRGB_DECODE_EXT,
                GL_SKIP_DECODE_EXT);
          end;
{$IFDEF VKS_OPENGL_DEBUG}
          CheckError;
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

class function TVKTextureSampler.FriendlyName: string;
begin
  Result := 'Texture Sampler';
end;

procedure TVKTextureSampler.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name := ReadString;
      FDefferedInit := ReadBoolean;
      FMinFilter := TVKMinFilter(ReadInteger);
      FMagFilter := TVKMagFilter(ReadInteger);
      FFilteringQuality := TVKTextureFilteringQuality(ReadInteger);
      FLODBias := ReadInteger;
      FWrap[0] := TVKSeparateTextureWrap(ReadInteger);
      FWrap[1] := TVKSeparateTextureWrap(ReadInteger);
      FWrap[2] := TVKSeparateTextureWrap(ReadInteger);
      Read(FBorderColor.AsAddress^, SizeOf(TColorVector));
      FCompareMode := TVKTextureCompareMode(ReadInteger);
      FCompareFunc := TDepthFunction(ReadInteger);
      FDecodeSRGB := ReadBoolean;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TVKTextureSampler.SetBorderColor(const AValue: TVKColor);
begin
  FBorderColor.Assign(AValue);
  NotifyChange(Self);
end;

procedure TVKTextureSampler.SetCompareFunc(AValue: TDepthFunction);
begin
  if FCompareFunc <> AValue then
  begin
    FCompareFunc := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKTextureSampler.SetCompareMode(AValue: TVKTextureCompareMode);
begin
  if FCompareMode <> AValue then
  begin
    FCompareMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKTextureSampler.SetDecodeSRGB(AValue: Boolean);
begin
  if FDecodeSRGB <> AValue then
  begin
    FDecodeSRGB := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKTextureSampler.SetFilteringQuality(
  AValue: TVKTextureFilteringQuality);
begin
  if FFilteringQuality <> AValue then
  begin
    FFilteringQuality := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKTextureSampler.SetLODBias(AValue: Integer);
begin
  if FLODBias <> AValue then
  begin
    FLODBias := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKTextureSampler.SetMagFilter(AValue: TVKMagFilter);
begin
  if FMagFilter <> AValue then
  begin
    FMagFilter := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKTextureSampler.SetMinFilter(AValue: TVKMinFilter);
begin
  if FMinFilter <> AValue then
  begin
    FMinFilter := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKTextureSampler.SetWrap(Index: Integer;
  AValue: TVKSeparateTextureWrap);
begin
  if FWrap[Index] <> AValue then
  begin
    FWrap[Index] := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKTextureSampler.UnApply(var ARci: TRenderContextInfo);
begin
  if FHandle.IsSupported then
    with ARci.GLStates do
      SamplerBinding[ActiveTexture] := 0;
end;

procedure TVKTextureSampler.WriteToFiler(AWriter: TWriter);
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

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKTextureCombiner'}{$ENDIF}

procedure TVKTextureCombiner.Assign(Source: TPersistent);
var
  LCombiner: TVKTextureCombiner;
begin
  if Source is TVKTextureCombiner then
  begin
    LCombiner := TVKTextureCombiner(Source);
    FScript.Assign(LCombiner.FScript);
  end;
  inherited;
end;

constructor TVKTextureCombiner.Create(AOwner: TVKXCollection);
begin
  inherited;
  FDefferedInit := False;
  FHandle := TVKVirtualHandle.Create;
  FHandle.OnAllocate := DoAllocate;
  FHandle.OnDestroy := DoDeallocate;
  FHandle.OnPrapare := DoOnPrepare;
  FScript := TStringList.Create;
  FScript.OnChange := NotifyChange;
  FIsValid := True;
  Name := TVKMatLibComponents(AOwner).MakeUniqueName('Combiner');
end;

destructor TVKTextureCombiner.Destroy;
begin
  FHandle.Destroy;
  FScript.Destroy;
  inherited;
end;

procedure TVKTextureCombiner.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TVKTextureCombiner.DoAllocate(Sender: TVKVirtualHandle;
  var handle: TGLuint);
begin
  handle := 1;
end;

procedure TVKTextureCombiner.DoDeallocate(Sender: TVKVirtualHandle;
  var handle: TGLuint);
begin
  handle := 0;
end;

procedure TVKTextureCombiner.DoOnPrepare(Sender: TVKContext);
begin
  if IsDesignTime and FDefferedInit then
    exit;
  if Sender.GL.ARB_multitexture then
  begin
    FHandle.AllocateHandle;
    if FHandle.IsDataNeedUpdate then
    begin
      try
        FCommandCache := GetTextureCombiners(FScript);
        FIsValid := True;
      except
        on E: Exception do
        begin
          FIsValid := False;
          if IsDesignTime then
            InformationDlg(E.ClassName + ': ' + E.Message)
          else
            GLSLogger.LogError(E.ClassName + ': ' + E.Message);
        end;
      end;
      FHandle.NotifyDataUpdated;
    end;
  end
  else
    FIsValid := False;
end;

class function TVKTextureCombiner.FriendlyName: string;
begin
  Result := 'Texture Combiner';
end;

procedure TVKTextureCombiner.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name := ReadString;
      FDefferedInit := ReadBoolean;
      FScript.Text := ReadString;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TVKTextureCombiner.SetScript(AValue: TStringList);
begin
  FScript.Assign(AValue);
  NotifyChange(Self);
end;

procedure TVKTextureCombiner.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    WriteString(FScript.Text);
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKLibMaterialEx'}{$ENDIF}

procedure TVKLibMaterialEx.Apply(var ARci: TRenderContextInfo);
var
  LevelReady: array[TVKMaterialLevel] of Boolean;
  L, MaxLevel: TVKMaterialLevel;
begin
  if Assigned(FNextPass) then
  begin
    FNextPass := nil;
    exit;
  end;

  FHandle.AllocateHandle;
  if FHandle.IsDataNeedUpdate then
  begin
    // Other value than mlAuto indicates a level failure
    // Need remove deffered initialization and reinitialize used resources
    if not IsDesignTime and (FSelectedLevel <> mlAuto) then
      RemoveDefferedInit;
    // Level selection
    LevelReady[mlFixedFunction] := FFixedFunc.Enabled;
    LevelReady[mlMultitexturing] := FMultitexturing.Enabled and
      FMultitexturing.IsValid;
    LevelReady[mlSM3] := FSM3.Enabled and FSM3.IsValid;
    LevelReady[mlSM4] := FSM4.Enabled and FSM4.IsValid;
    LevelReady[mlSM5] := FSM5.Enabled and FSM5.IsValid;

    if FApplicableLevel = mlAuto then
      MaxLevel := mlSM5
    else
      MaxLevel := FApplicableLevel;

    FSelectedLevel := mlAuto;
    for L := MaxLevel downto mlFixedFunction do
      if LevelReady[L] then
      begin
        FSelectedLevel := L;
        break;
      end;

    FStoreAmalgamating := ARci.amalgamating;
    ARci.amalgamating := True;
    FHandle.NotifyDataUpdated;
  end;

  ARci.currentMaterialLevel := FSelectedLevel;

  case FSelectedLevel of
    mlAuto: ; // No one level can be used. Worst case.

    mlFixedFunction:
      begin
        FFixedFunc.Apply(ARci);
      end;

    mlMultitexturing:
      begin
        if LevelReady[mlFixedFunction] then
          FFixedFunc.Apply(ARci);
        FMultitexturing.Apply(ARci);
      end;

    mlSM3:
      begin
        if LevelReady[mlFixedFunction] then
          FFixedFunc.Apply(ARci);
        FSM3.Apply(ARci);
      end;

    mlSM4:
      begin
        if LevelReady[mlFixedFunction] then
          FFixedFunc.Apply(ARci);
        FSM4.Apply(ARci);
      end;

    mlSM5:
      begin
        if LevelReady[mlFixedFunction] then
          FFixedFunc.Apply(ARci);
        FSM5.Apply(ARci);
      end;
  end;
end;

procedure TVKLibMaterialEx.Assign(Source: TPersistent);
var
  LMaterial: TVKLibMaterialEx;
begin
  if Source is TVKLibMaterialEx then
  begin
    LMaterial := TVKLibMaterialEx(Source);
    FFixedFunc.Assign(LMaterial.FFixedFunc);
    FMultitexturing.Assign(LMaterial.FMultitexturing);
    FSM3.Assign(LMaterial.FSM3);
    FSM4.Assign(LMaterial.FSM4);
    FSM5.Assign(LMaterial.FSM5);
    FApplicableLevel := LMaterial.FApplicableLevel;
    NotifyChange(Self);
  end;
  inherited;
end;

function TVKLibMaterialEx.Blended: Boolean;
begin
  Result := FFixedFunc.Blended;
end;

constructor TVKLibMaterialEx.Create(ACollection: TCollection);
begin
  inherited;
  FHandle := TVKVirtualHandle.Create;
  FHandle.OnAllocate := DoAllocate;
  FHandle.OnDestroy := DoDeallocate;
  FHandle.OnPrapare := DoOnPrepare;
  FApplicableLevel := mlAuto;
  FSelectedLevel := mlAuto;
  FFixedFunc := TVKFixedFunctionProperties.Create(Self);
  FMultitexturing := TVKMultitexturingProperties.Create(Self);
  FSM3 := TVKShaderModel3.Create(Self);
  FSM4 := TVKShaderModel4.Create(Self);
  FSM5 := TVKShaderModel5.Create(Self);
end;

type
  TVKFreindlyMaterial = class(TVKMaterial);

destructor TVKLibMaterialEx.Destroy;
var
  I: Integer;
  LUser: TObject;
begin
  FHandle.Destroy;
  FFixedFunc.Destroy;
  FMultitexturing.Destroy;
  FSM3.Destroy;
  FSM4.Destroy;
  FSM5.Destroy;
  for I := 0 to FUserList.Count - 1 do
  begin
    LUser := TObject(FUserList[i]);
    if LUser is TVKMaterial then
      TVKFreindlyMaterial(LUser).NotifyLibMaterialDestruction;
  end;
  inherited;
end;

procedure TVKLibMaterialEx.DoAllocate(Sender: TVKVirtualHandle;
  var handle: TGLuint);
begin
  handle := 1;
end;

procedure TVKLibMaterialEx.DoDeallocate(Sender: TVKVirtualHandle;
  var handle: TGLuint);
begin
  handle := 0;
end;

procedure TVKLibMaterialEx.DoOnPrepare(Sender: TVKContext);
begin
end;

procedure TVKLibMaterialEx.Loaded;
begin
  FFixedFunc.FTexProp.Loaded;
  FMultitexturing.Loaded;
  FSM3.Loaded;
  FSM4.Loaded;
  FSM5.Loaded;
end;

procedure TVKLibMaterialEx.NotifyChange(Sender: TObject);
begin
  inherited;
  FHandle.NotifyChangesOfData;
end;

procedure TVKLibMaterialEx.RemoveDefferedInit;
var
  I: Integer;
  ST: TVKShaderType;
begin
  if FFixedFunc.FTexProp.Enabled then
  begin
    if Assigned(FFixedFunc.FTexProp.FLibTexture) then
      FFixedFunc.FTexProp.FLibTexture.FDefferedInit := False;
    if Assigned(FFixedFunc.FTexProp.FLibSampler) then
      FFixedFunc.FTexProp.FLibSampler.FDefferedInit := False;
  end;

  if FMultitexturing.Enabled then
  begin
    if Assigned(FMultitexturing.FLibCombiner) then
    begin
      FMultitexturing.FLibCombiner.FDefferedInit := False;
      for I := 0 to 3 do
        if Assigned(FMultitexturing.FTexProps[I]) then
          with FMultitexturing.FTexProps[I] do
          begin
            if Assigned(FLibTexture) then
              FLibTexture.FDefferedInit := False;
            if Assigned(FLibSampler) then
              FLibSampler.FDefferedInit := False;
          end;
    end;
  end;

  if FSM3.Enabled then
  begin
    for ST := Low(TVKShaderType) to High(TVKShaderType) do
      if Assigned(FSM3.FShaders[ST]) then
        FSM3.FShaders[ST].FDefferedInit := False;
  end;

  if FSM4.Enabled then
  begin
    for ST := Low(TVKShaderType) to High(TVKShaderType) do
      if Assigned(FSM4.FShaders[ST]) then
        FSM4.FShaders[ST].FDefferedInit := False;
  end;

  if FSM5.Enabled then
  begin
    for ST := Low(TVKShaderType) to High(TVKShaderType) do
      if Assigned(FSM5.FShaders[ST]) then
        FSM5.FShaders[ST].FDefferedInit := False;
  end;

  CurrentGLContext.PrepareHandlesData;
end;

procedure TVKLibMaterialEx.SetMultitexturing(AValue:
  TVKMultitexturingProperties);
begin
  FMultitexturing.Assign(AValue);
end;

procedure TVKLibMaterialEx.SetFixedFunc(AValue: TVKFixedFunctionProperties);
begin
  FFixedFunc.Assign(AValue);
end;

procedure TVKLibMaterialEx.SetLevel(AValue: TVKMaterialLevel);
begin
  if FApplicableLevel <> AValue then
  begin
    FApplicableLevel := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKLibMaterialEx.SetSM3(AValue: TVKShaderModel3);
begin
  FSM3.Assign(AValue);
end;

procedure TVKLibMaterialEx.SetSM4(AValue: TVKShaderModel4);
begin
  FSM4.Assign(AValue);
end;

procedure TVKLibMaterialEx.SetSM5(AValue: TVKShaderModel5);
begin
  FSM5.Assign(AValue);
end;

function TVKLibMaterialEx.UnApply(var ARci: TRenderContextInfo): Boolean;

  procedure GetNextPass(AProp: TVKLibMaterialProperty);
  begin
    if Length(AProp.NextPass) > 0 then
      FNextPass :=
        TVKMaterialLibraryEx(GetMaterialLibrary).Materials.GetLibMaterialByName(AProp.NextPass)
    else
      FNextPass := nil;

    if FNextPass = Self then
    begin
      AProp.NextPass := '';
      FNextPass := nil;
    end;
  end;

begin
  if FStoreAmalgamating <> ARci.amalgamating then
    ARci.amalgamating := FStoreAmalgamating;

  if Assigned(FNextPass) then
  begin
    Result := FNextPass.UnApply(ARci);
    if Result then
      FNextPass.Apply(ARci)
    else
      FNextPass := nil;
    exit;
  end;

  case FSelectedLevel of
    mlFixedFunction:
      begin
        FFixedFunc.UnApply(ARci);
        GetNextPass(FFixedFunc);
      end;

    mlMultitexturing:
      begin
        if FFixedFunc.Enabled then
          FFixedFunc.UnApply(ARci);
        FMultitexturing.UnApply(ARci);
        GetNextPass(FMultitexturing);
      end;

    mlSM3:
      begin
        if FFixedFunc.Enabled then
          FFixedFunc.UnApply(ARci);
        FSM3.UnApply(ARci);
        GetNextPass(FSM3);
      end;

    mlSM4:
      begin
        if FFixedFunc.Enabled then
          FFixedFunc.UnApply(ARci);
        FSM4.UnApply(ARci);
        GetNextPass(FSM4);
      end;

    mlSM5:
      begin
        if FFixedFunc.Enabled then
          FFixedFunc.UnApply(ARci);
        FSM5.UnApply(ARci);
        GetNextPass(FSM5);
      end;
  else
    FNextPass := nil;
  end;
  ARci.GLStates.ActiveTexture := 0;

  Result := Assigned(FNextPass);
  if Result then
    FNextPass.Apply(ARCi);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKMultitexturingProperties'}{$ENDIF}

procedure TVKMultitexturingProperties.Apply(var ARci: TRenderContextInfo);
var
  N, U: Integer;
  LDir: TVector;
begin
  if FEnabled then
  begin
    if Assigned(FLibCombiner) and not FLibCombiner.FIsValid then
      exit;
    if Assigned(FLibAsmProg) and not FLibAsmProg.FIsValid then
      exit;

    U := 0;
    for N := 0 to High(FTexProps) do
    begin
      if Assigned(FTexProps[N]) and FTexProps[N].Enabled then
      begin
        ARci.GLStates.ActiveTexture := N;
        FTexProps[N].Apply(ARci);
        if Ord(FLightDir) = N+1 then
        begin
          LDir := ARci.GLStates.LightPosition[FLightSourceIndex];
          LDir := VectorTransform(LDir, ARci.PipelineTransformation.InvModelMatrix);
          NormalizeVector(LDir);
          GL.TexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, @LDir);
        end;
        U := U or (1 shl N);
      end;
    end;

    if Assigned(FLibAsmProg) then
    begin
      FLibAsmProg.Handle.Bind;
      GL.Enable(GL_VERTEX_PROGRAM_ARB);
      if Assigned(GetMaterial.FOnAsmProgSetting) then
        GetMaterial.FOnAsmProgSetting(Self.FLibAsmProg, ARci);
    end;

    with GL, ARci.GLStates do
    begin
      if Assigned(FLibCombiner) and (Length(FLibCombiner.FCommandCache) > 0)
        then
      begin
        for N := 0 to High(FLibCombiner.FCommandCache) do
        begin
          ActiveTexture := FLibCombiner.FCommandCache[N].ActiveUnit;
          TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);
          TexEnvi(GL_TEXTURE_ENV,
            FLibCombiner.FCommandCache[N].Arg1,
            FLibCombiner.FCommandCache[N].Arg2);
        end;
      end;
      TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
      ActiveTexture := 0;

    end;

    XGL.BeginUpdate;
    if U > 3 then
      XGL.MapTexCoordToArbitrary(U)
    else if (FTexProps[0].Enabled)
      and (FTexProps[0].MappingMode = tmmUser) then
      if FTexProps[1].MappingMode = tmmUser then
        XGL.MapTexCoordToDual
      else
        XGL.MapTexCoordToMain
    else if FTexProps[1].MappingMode = tmmUser then
      XGL.MapTexCoordToSecond
    else
      XGL.MapTexCoordToMain;
    XGL.EndUpdate;

  end;
end;

constructor TVKMultitexturingProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FEnabled := False;
  FTextureMode := tmDecal;
  FLightDir := l2eNone;
  FLightSourceIndex := 0;
end;

destructor TVKMultitexturingProperties.Destroy;
begin
  if Assigned(FLibCombiner) then
    FLibCombiner.UnregisterUser(Self);
  if Assigned(FLibAsmProg) then
    FLibAsmProg.UnregisterUser(Self);
  FTexProps[0].Free;
  FTexProps[1].Free;
  FTexProps[2].Free;
  FTexProps[3].Free;
  inherited;
end;

function TVKMultitexturingProperties.GetLibCombinerName: string;
begin
  if Assigned(FLibCombiner) then
    Result := FLibCombiner.Name
  else
    Result := '';
end;

function TVKMultitexturingProperties.GetLibAsmProgName: string;
begin
  if Assigned(FLibAsmProg) then
    Result := FLibAsmProg.Name
  else
    Result := '';
end;

function TVKMultitexturingProperties.IsValid: Boolean;
var
  I: Integer;
begin
  Result := True;
  if Assigned(FLibCombiner) then
    Result := Result and FLibCombiner.IsValid;
  if Assigned(FLibAsmProg) then
    Result := Result and FLibAsmProg.IsValid;
  for I := 0 to High(FTexProps) do
    if Assigned(FTexProps[I]) and FTexProps[I].FEnabled then
      Result := Result and FTexProps[I].IsValid;
end;

procedure TVKMultitexturingProperties.Loaded;
var
  I: Integer;
begin
  SetLibCombinerName(FLibCombinerName);
  SetLibAsmProgName(FLibAsmProgName);
  for I := 0 to High(FTexProps) do
    if Assigned(FTexProps[I]) then
      FTexProps[I].Loaded;
end;

procedure TVKMultitexturingProperties.Notification(Sender: TObject; Operation:
  TOperation);
begin
  if Operation = opRemove then
  begin
    if Sender = FLibCombiner then
      FLibCombiner := nil;
    if Sender = FLibAsmProg then
      FLibAsmProg := nil;
  end;
  inherited;
end;

procedure TVKMultitexturingProperties.SetLibCombinerName(const AValue: string);
var
  LCombiner: TVKTextureCombiner;
begin
  if csLoading in GetMaterialLibraryEx.ComponentState then
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
  LCombiner := GetMaterialLibraryEx.Components.GetCombinerByName(AValue);
  if Assigned(LCombiner) then
  begin
    LCombiner.RegisterUser(Self);
    FLibCombiner := LCombiner;
  end;
  NotifyChange(Self);
end;

procedure TVKMultitexturingProperties.SetLightSourceIndex(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0
  else if AValue > 7 then
    AValue := 7;
  FLightSourceIndex := AValue;
end;

procedure TVKMultitexturingProperties.SetLibAsmProgName(const AValue: string);
var
  LProg: TVKASMVertexProgram;
begin
  if csLoading in GetMaterialLibraryEx.ComponentState then
  begin
    FLibAsmProgName := AValue;
    exit;
  end;

  if Assigned(FLibAsmProg) then
  begin
    if FLibAsmProg.Name = AValue then
      exit;
    FLibAsmProg.UnregisterUser(Self);
    FLibAsmProg := nil;
  end;
  LProg := GetMaterialLibraryEx.Components.GetAsmProgByName(AValue);
  if Assigned(LProg) then
  begin
    LProg.RegisterUser(Self);
    FLibAsmProg := LProg;
  end;
  NotifyChange(Self);
end;

function TVKMultitexturingProperties.GetTexProps(AIndex: Integer):
  TVKTextureProperties;
begin
  if not Assigned(FTexProps[AIndex]) then
    FTexProps[AIndex] := TVKTextureProperties.Create(Self);
  Result := FTexProps[AIndex];
end;

procedure TVKMultitexturingProperties.SetTexProps(AIndex: Integer;
  AValue: TVKTextureProperties);
begin
  FTexProps[AIndex].Assign(AValue);
end;

procedure TVKMultitexturingProperties.SetTextureMode(AValue: TVKTextureMode);
begin
  if AValue <> FTextureMode then
  begin
    FTextureMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKMultitexturingProperties.UnApply(var ARci: TRenderContextInfo);
var
  N: Integer;
begin
  for N := 0 to High(FTexProps) do
  begin
    if FTexProps[N].Enabled then
    begin
      ARci.GLStates.ActiveTexture := N;
      FTexProps[N].UnApply(ARci);
    end;
  end;
  ARci.GLStates.ActiveTexture := 0;

  if Assigned(FLibAsmProg) then
    GL.Disable(GL_VERTEX_PROGRAM_ARB);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKTextureProperties'}{$ENDIF}

procedure TVKTextureProperties.Apply(var ARci: TRenderContextInfo);
var
  glTarget: TGLenum;
begin
  if Assigned(FLibTexture) then
    with GL do
    begin
      FLibTexture.FApplicableSampler := FLibSampler;
      FLibTexture.Apply(ARci);

      // Apply swizzling if possible
      glTarget := DecodeGLTextureTarget(FLibTexture.Shape);
      if ARB_texture_swizzle or EXT_texture_swizzle then
      begin
        if FSwizzling.FSwizzles[0] <> FLibTexture.FSwizzles[0] then
        begin
          FLibTexture.FSwizzles[0] := FSwizzling.FSwizzles[0];
          TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_R,
            cTextureSwizzle[FSwizzling.FSwizzles[0]]);
        end;
        if FSwizzling.FSwizzles[1] <> FLibTexture.FSwizzles[1] then
        begin
          FLibTexture.FSwizzles[1] := FSwizzling.FSwizzles[1];
          TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_G,
            cTextureSwizzle[FSwizzling.FSwizzles[1]]);
        end;
        if FSwizzling.FSwizzles[2] <> FLibTexture.FSwizzles[2] then
        begin
          FLibTexture.FSwizzles[2] := FSwizzling.FSwizzles[2];
          TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_B,
            cTextureSwizzle[FSwizzling.FSwizzles[2]]);
        end;
        if FSwizzling.FSwizzles[3] <> FLibTexture.FSwizzles[3] then
        begin
          FLibTexture.FSwizzles[3] := FSwizzling.FSwizzles[3];
          TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_A,
            cTextureSwizzle[FSwizzling.FSwizzles[3]]);
        end;
      end;

      if Assigned(FLibSampler) then
      begin
        if FLibSampler.IsValid then
          FLibSampler.Apply(ARci)
        else if FLibTexture.FLastSampler <> FLibSampler then
        begin
          // Sampler object not supported, lets use texture states
          TexParameterfv(glTarget, GL_TEXTURE_BORDER_COLOR,
            FLibSampler.BorderColor.AsAddress);
          TexParameteri(glTarget, GL_TEXTURE_WRAP_S,
            cTextureWrapMode[FLibSampler.WrapX]);
          TexParameteri(glTarget, GL_TEXTURE_WRAP_T,
            cTextureWrapMode[FLibSampler.WrapY]);
          TexParameteri(glTarget, GL_TEXTURE_WRAP_R,
            cTextureWrapMode[FLibSampler.WrapZ]);
          TexParameterf(glTarget, GL_TEXTURE_LOD_BIAS, FLibSampler.LODBias +
            FLibSampler.FLODBiasFract);
          TexParameteri(glTarget, GL_TEXTURE_MIN_FILTER,
            cTextureMinFilter[FLibSampler.MinFilter]);
          TexParameteri(glTarget, GL_TEXTURE_MAG_FILTER,
            cTextureMagFilter[FLibSampler.MagFilter]);

          if EXT_texture_filter_anisotropic then
          begin
            if FLibSampler.FilteringQuality = tfAnisotropic then
              TexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT,
                CurrentGLContext.GLStates.MaxTextureAnisotropy)
            else
              TexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
          end;

          TexParameteri(glTarget, GL_TEXTURE_COMPARE_MODE,
            cTextureCompareMode[FLibSampler.CompareMode]);
          TexParameteri(glTarget, GL_TEXTURE_COMPARE_FUNC,
            cGLComparisonFunctionToGLEnum[FLibSampler.CompareFunc]);

          if EXT_texture_sRGB_decode then
          begin
            if FLibSampler.sRGB_Encode then
              TexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT, GL_DECODE_EXT)
            else
              TexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT,
                GL_SKIP_DECODE_EXT);
          end;

          FLibTexture.FLastSampler := FLibSampler;
        end;
      end;

      if not FTextureMatrixIsIdentity and (MappingMode = tmmUser) then
        ARci.GLStates.SetGLTextureMatrix(FTextureMatrix);

      if ARci.currentMaterialLevel < mlSM3 then
      begin
        GL.TexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
        ApplyMappingMode;
        if ARci.currentMaterialLevel = mlFixedFunction then
          XGL.MapTexCoordToMain;
      end;
    end;
end;

procedure TVKTextureProperties.ApplyMappingMode;
var
  R_Dim: Boolean;
begin
  with GL do
  begin
    R_Dim := ARB_texture_cube_map or EXT_texture3D;

    case MappingMode of

      tmmUser: ; // nothing to do, but checked first (common case)

      tmmObjectLinear:
        begin
          TexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          TexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          TexGenfv(GL_S, GL_OBJECT_PLANE, @MappingSCoordinates.DirectVector);
          TexGenfv(GL_T, GL_OBJECT_PLANE, @MappingTCoordinates.DirectVector);
          Enable(GL_TEXTURE_GEN_S);
          Enable(GL_TEXTURE_GEN_T);

          if R_Dim then
          begin
            TexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            TexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            TexGenfv(GL_R, GL_OBJECT_PLANE, @MappingRCoordinates.DirectVector);
            TexGenfv(GL_Q, GL_OBJECT_PLANE, @MappingQCoordinates.DirectVector);
            Enable(GL_TEXTURE_GEN_R);
            Enable(GL_TEXTURE_GEN_Q);
          end;
        end;

      tmmEyeLinear:
        begin
          TexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
          TexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
          // specify planes in eye space, not world space
          MatrixMode(GL_MODELVIEW);
          PushMatrix;
          LoadIdentity;
          TexGenfv(GL_S, GL_EYE_PLANE, @MappingSCoordinates.DirectVector);
          TexGenfv(GL_T, GL_EYE_PLANE, @MappingTCoordinates.DirectVector);
          Enable(GL_TEXTURE_GEN_S);
          Enable(GL_TEXTURE_GEN_T);
          if R_Dim then
          begin
            TexGenfv(GL_R, GL_EYE_PLANE, @MappingRCoordinates.DirectVector);
            TexGenfv(GL_Q, GL_EYE_PLANE, @MappingQCoordinates.DirectVector);
            Enable(GL_TEXTURE_GEN_R);
            Enable(GL_TEXTURE_GEN_Q);
          end;
          PopMatrix;
        end;

      tmmSphere:
        begin
          TexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
          TexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
          Enable(GL_TEXTURE_GEN_S);
          Enable(GL_TEXTURE_GEN_T);
        end;

      tmmCubeMapReflection, tmmCubeMapCamera:
        if R_Dim then
        begin
          TexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
          TexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
          TexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
          Enable(GL_TEXTURE_GEN_S);
          Enable(GL_TEXTURE_GEN_T);
          Enable(GL_TEXTURE_GEN_R);
        end;

      tmmCubeMapNormal, tmmCubeMapLight0:
        if R_Dim then
        begin
          TexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
          TexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
          TexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
          Enable(GL_TEXTURE_GEN_S);
          Enable(GL_TEXTURE_GEN_T);
          Enable(GL_TEXTURE_GEN_R);
        end;
    end;
  end;
end;

procedure TVKTextureProperties.Assign(Source: TPersistent);
var
  LTexProp: TVKTextureProperties;
begin
  if Source is TVKTextureProperties then
  begin
    LTexProp := TVKTextureProperties(Source);
    LibTextureName := LTexProp.LibTextureName;
    LibSamplerName := LTexProp.LibSamplerName;
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

procedure TVKTextureProperties.CalculateTextureMatrix;
begin
  if not (Assigned(FTextureOffset) or Assigned(FTextureScale)
    or StoreTextureRotate) then
  begin
    FTextureMatrixIsIdentity := True;
    exit;
  end;

  if TextureOffset.Equals(NullHmgVector)
    and TextureScale.Equals(XYZHmgVector)
    and not StoreTextureRotate then
    FTextureMatrixIsIdentity := True
  else
  begin
    FTextureMatrixIsIdentity := False;
    FTextureMatrix := CreateScaleAndTranslationMatrix(
      TextureScale.AsVector,
      TextureOffset.AsVector);
    if StoreTextureRotate then
      FTextureMatrix := MatrixMultiply(FTextureMatrix,
        CreateRotationMatrixZ(DegToRadian(FTextureRotate)));
  end;
  FTextureOverride := False;
  NotifyChange(Self);
end;

constructor TVKTextureProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FTextureRotate := 0;
  FMappingMode := tmmUser;
  FTextureMatrix := IdentityHmgMatrix;
  FEnabled := False;
  FSwizzling := TVKTextureSwizzling.Create(Self);
  FEnvColor := TVKColor.CreateInitialized(Self, clrTransparent);
end;

destructor TVKTextureProperties.Destroy;
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

function TVKTextureProperties.GetLibSamplerName: TVKMaterialComponentName;
begin
  if Assigned(FLibSampler) then
    Result := FLibSampler.Name
  else
    Result := '';
end;

function TVKTextureProperties.GetLibTextureName: TVKMaterialComponentName;
begin
  if Assigned(FLibTexture) then
    Result := FLibTexture.Name
  else
    Result := '';
end;

function TVKTextureProperties.GetMappingQCoordinates: TVKCoordinates4;
begin
  if not Assigned(FMapQCoordinates) then
    FMapQCoordinates := TVKCoordinates4.CreateInitialized(Self, WHmgVector,
      csVector);
  Result := FMapQCoordinates;
end;

function TVKTextureProperties.GetMappingRCoordinates: TVKCoordinates4;
begin
  if not Assigned(FMapRCoordinates) then
    FMapRCoordinates := TVKCoordinates4.CreateInitialized(Self, ZHmgVector,
      csVector);
  Result := FMapRCoordinates;
end;

function TVKTextureProperties.GetMappingSCoordinates: TVKCoordinates4;
begin
  if not Assigned(FMapSCoordinates) then
    FMapSCoordinates := TVKCoordinates4.CreateInitialized(Self, XHmgVector,
      csVector);
  Result := FMapSCoordinates;
end;

function TVKTextureProperties.GetMappingTCoordinates: TVKCoordinates4;
begin
  if not Assigned(FMapTCoordinates) then
    FMapTCoordinates := TVKCoordinates4.CreateInitialized(Self, YHmgVector,
      csVector);
  Result := FMapTCoordinates;
end;

function TVKTextureProperties.GetTextureOffset: TVKCoordinates;
begin
  if not Assigned(FTextureOffset) then
    FTextureOffset :=
      TVKCoordinates3.CreateInitialized(Self, NullHmgVector, csPoint);
  Result := FTextureOffset;
end;

function TVKTextureProperties.GetTextureScale: TVKCoordinates;
begin
  if not Assigned(FTextureScale) then
    FTextureScale :=
      TVKCoordinates3.CreateInitialized(Self, VectorMake(1, 1, 1, 1), csVector);
  Result := FTextureScale;
end;

function TVKTextureProperties.IsValid: Boolean;
begin
  if Assigned(FLibTexture) then
    Result := FLibTexture.IsValid
  else
    Result := False;
end;

procedure TVKTextureProperties.Loaded;
begin
  SetLibTextureName(FLibTextureName);
  SetLibSamplerName(FLibSamplerName);
  CalculateTextureMatrix;
end;

procedure TVKTextureProperties.Notification(Sender: TObject;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if Sender = FLibTexture then
      FLibTexture := nil
    else if Sender = FLibSampler then
      FLibSampler := nil;
  end;
end;

procedure TVKTextureProperties.NotifyChange(Sender: TObject);
begin
  inherited;
  if (Sender = FTextureOffset) or (Sender = FTextureScale) then
    CalculateTextureMatrix;
  if (Sender = FLibSampler) and Assigned(FLibTexture) then
    FLibTexture.FLastSampler := nil;
end;

procedure TVKTextureProperties.SetLibSamplerName(const AValue:
  TVKMaterialComponentName);
var
  LSampler: TVKTextureSampler;
begin
  if csLoading in GetMaterialLibraryEx.ComponentState then
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
  LSampler := GetMaterialLibraryEx.Components.GetSamplerByName(AValue);
  if Assigned(LSampler) then
  begin
    LSampler.RegisterUser(Self);
    FLibSampler := LSampler;
  end;
  NotifyChange(Self);
end;

procedure TVKTextureProperties.SetLibTextureName(const AValue:
  TVKMaterialComponentName);
var
  LTexture: TVKAbstractTexture;
begin
  if csLoading in GetMaterialLibraryEx.ComponentState then
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

  LTexture := GetMaterialLibraryEx.Components.GetTextureByName(AValue);

  if Assigned(LTexture) then
  begin
    if LTexture is TVKFrameBufferAttachment then
    begin
      if TVKFrameBufferAttachment(LTexture).OnlyWrite then
      begin
        if IsDesignTime then
          InformationDlg('Can not use write only attachment as texture')
        else
          GLSLogger.LogErrorFmt('Attempt to use write only attachment "%s" as texture',
            [LTexture.Name]);
        NotifyChange(Self);
        exit;
      end;
    end;
    LTexture.RegisterUser(Self);
    FLibTexture := LTexture;
  end;
  NotifyChange(Self);
end;

procedure TVKTextureProperties.SetMappingMode(
  const AValue: TVKTextureMappingMode);
begin
  if AValue <> FMappingMode then
  begin
    FMappingMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKTextureProperties.SetMappingQCoordinates(
  const AValue: TVKCoordinates4);
begin
  MappingQCoordinates.Assign(AValue);
end;

procedure TVKTextureProperties.SetMappingRCoordinates(
  const AValue: TVKCoordinates4);
begin
  MappingRCoordinates.Assign(AValue);
end;

procedure TVKTextureProperties.SetMappingSCoordinates(
  const AValue: TVKCoordinates4);
begin
  MappingSCoordinates.Assign(AValue);
end;

procedure TVKTextureProperties.SetMappingTCoordinates(
  const AValue: TVKCoordinates4);
begin
  MappingTCoordinates.Assign(AValue);
end;

procedure TVKTextureProperties.SetSwizzling(const AValue: TVKTextureSwizzling);
begin
  FSwizzling.Assign(AValue);
end;

procedure TVKTextureProperties.SetTextureMatrix(const AValue: TMatrix);
begin
  FTextureMatrixIsIdentity := CompareMem(@AValue.V[0], @IdentityHmgMatrix.V[0],
    SizeOf(TMatrix));
  FTextureMatrix := AValue;
  FTextureOverride := True;
  NotifyChange(Self);
end;

procedure TVKTextureProperties.SetTextureOffset(const AValue: TVKCoordinates);
begin
  TextureOffset.Assign(AValue);
  CalculateTextureMatrix;
end;

procedure TVKTextureProperties.SetTextureRotate(AValue: Single);
begin
  if AValue <> FTextureRotate then
  begin
    FTextureRotate := AValue;
    CalculateTextureMatrix;
    NotifyChange(Self);
  end;
end;

procedure TVKTextureProperties.SetTextureScale(const AValue: TVKCoordinates);
begin
  TextureScale.Assign(AValue);
  CalculateTextureMatrix;
end;

function TVKTextureProperties.StoreMappingQCoordinates: Boolean;
begin
  if Assigned(FMapQCoordinates) then
    Result := not VectorEquals(FMapQCoordinates.AsVector, WHmgVector)
  else
    Result := false;
end;

function TVKTextureProperties.StoreMappingRCoordinates: Boolean;
begin
  if Assigned(FMapRCoordinates) then
    Result := not VectorEquals(FMapRCoordinates.AsVector, ZHmgVector)
  else
    Result := false;
end;

function TVKTextureProperties.StoreMappingSCoordinates: Boolean;
begin
  if Assigned(FMapSCoordinates) then
    Result := not VectorEquals(FMapSCoordinates.AsVector, XHmgVector)
  else
    Result := false;
end;

function TVKTextureProperties.StoreMappingTCoordinates: Boolean;
begin
  if Assigned(FMapTCoordinates) then
    Result := not VectorEquals(FMapTCoordinates.AsVector, YHmgVector)
  else
    Result := false;
end;

function TVKTextureProperties.StoreSwizzling: Boolean;
begin
  Result := FSwizzling.StoreSwizzle(0);
end;

function TVKTextureProperties.StoreTextureOffset: Boolean;
begin
  Result := Assigned(FTextureOffset);
end;

function TVKTextureProperties.StoreTextureRotate: Boolean;
begin
  Result := Abs(FTextureRotate) > EPSILON;
end;

function TVKTextureProperties.StoreTextureScale: Boolean;
begin
  Result := Assigned(FTextureScale);
end;

procedure TVKTextureProperties.SetEnvColor(const AValue:
  TVKColor);
begin
  FEnvColor.Assign(AValue);
  NotifyChange(Self);
end;

procedure TVKTextureProperties.UnApply(var ARci: TRenderContextInfo);
begin
  if Assigned(FLibTexture) then
  begin
    FLibTexture.UnApply(ARci);
    if Assigned(FLibSampler) then
      FLibSampler.UnApply(ARci);

    if ARci.currentMaterialLevel < mlSM3 then
    begin
      if not FTextureMatrixIsIdentity and (MappingMode = tmmUser) then
        ARci.GLStates.SetGLTextureMatrix(IdentityHmgMatrix);
      UnApplyMappingMode;
    end;
  end;
end;

procedure TVKTextureProperties.UnApplyMappingMode;
begin
  if MappingMode <> tmmUser then
    with GL do
    begin
      Disable(GL_TEXTURE_GEN_S);
      Disable(GL_TEXTURE_GEN_T);
      if EXT_texture3D or ARB_texture_cube_map then
      begin
        Disable(GL_TEXTURE_GEN_R);
        Disable(GL_TEXTURE_GEN_Q);
      end;
    end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKShaderEx'}{$ENDIF}

procedure TVKShaderEx.Assign(Source: TPersistent);
var
  LShader: TVKShaderEx;
begin
  if Source is TVKShaderEx then
  begin
    LShader := TVKShaderEx(Source);
    FSource.Assign(LShader.Source);
    FShaderType := LShader.FShaderType;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TVKShaderEx.Create(AOwner: TVKXCollection);
const
  cShaderClasses: array[TVKShaderType] of TVKShaderHandleClass =
    (
    TVKVertexShaderHandle,
    TVKTessControlShaderHandle,
    TVKTessEvaluationShaderHandle,
    TVKGeometryShaderHandle,
    TVKFragmentShaderHandle
    );
var
  S: TVKShaderType;
begin
  inherited;
  FDefferedInit := False;
  for S := Low(TVKShaderType) to High(TVKShaderType) do
  begin
    FHandle[S] := cShaderClasses[S].Create;
    FHandle[S].OnPrapare := DoOnPrepare;
  end;
  FSource := TStringList.Create;
  FSource.OnChange := NotifyChange;
  FShaderType := shtVertex;
  FGeometryInput := gsInPoints;
  FGeometryOutput := gsOutPoints;
  FGeometryVerticesOut := 1;
  Name := TVKMatLibComponents(AOwner).MakeUniqueName('Shader');
end;

destructor TVKShaderEx.Destroy;
var
  S: TVKShaderType;
begin
  for S := Low(TVKShaderType) to High(TVKShaderType) do
    FHandle[S].Destroy;
  FSource.Destroy;
  inherited;
end;

procedure TVKShaderEx.NotifyChange(Sender: TObject);
var
  S: TVKShaderType;
begin
  for S := Low(TVKShaderType) to High(TVKShaderType) do
    FHandle[S].NotifyChangesOfData;

  if (Sender = FSource) and IsDesignTime and (Length(FSourceFile) > 0) then
    FSource.SaveToFile(FSourceFile);

  inherited;
end;

procedure TVKShaderEx.DoOnPrepare(Sender: TVKContext);
begin
  if not IsDesignTime and FDefferedInit then
    exit;
  try
    if FHandle[FShaderType].IsSupported then
    begin
      FHandle[FShaderType].AllocateHandle;
      if FHandle[FShaderType].IsDataNeedUpdate then
      begin
        SetExeDirectory;
        if (Length(FSourceFile) > 0) and FileStreamExists(FSourceFile) then
          FSource.LoadFromFile(FSourceFile);
        FHandle[FShaderType].ShaderSource(AnsiString(FSource.Text));
        FIsValid := FHandle[FShaderType].CompileShader;
        if IsDesignTime then
        begin
          FInfoLog := FHandle[FShaderType].InfoLog;
          if (Length(FInfoLog) = 0) and FIsValid then
            FInfoLog := 'Compilation successful';
        end
        else if FIsValid then
          GLSLogger.LogInfoFmt('Shader "%s" compilation successful - %s',
            [Name, FHandle[FShaderType].InfoLog])
        else
          GLSLogger.LogErrorFmt('Shader "%s" compilation failed - %s',
            [Name, FHandle[FShaderType].InfoLog]);
        FHandle[FShaderType].NotifyDataUpdated;
      end;
    end
    else
    begin
      FIsValid := False;
      if IsDesignTime then
        FInfoLog := 'Not supported by hardware';
    end;
  except
    on E: Exception do
    begin
      FIsValid := False;
      if IsDesignTime then
        InformationDlg(E.ClassName + ': ' + E.Message)
      else
        GLSLogger.LogError(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

class function TVKShaderEx.FriendlyName: string;
begin
  Result := 'GLSL Shader';
end;

function TVKShaderEx.GetHandle: TVKShaderHandle;
begin
  Result := FHandle[FShaderType];
end;

procedure TVKShaderEx.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name := ReadString;
      FDefferedInit := ReadBoolean;
      FSource.Text := ReadString;
      FSourceFile := ReadString;
      FShaderType := TVKShaderType(ReadInteger);
      FGeometryInput := TVKgsInTypes(ReadInteger);
      FGeometryOutput := TVKgsOutTypes(ReadInteger);
      FGeometryVerticesOut := ReadInteger;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TVKShaderEx.SetGeometryInput(AValue: TVKgsInTypes);
begin
  if AValue <> FGeometryInput then
  begin
    FGeometryInput := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKShaderEx.SetGeometryOutput(AValue: TVKgsOutTypes);
begin
  if AValue <> FGeometryOutput then
  begin
    FGeometryOutput := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKShaderEx.SetGeometryVerticesOut(AValue: TGLint);
begin
  if AValue < 1 then
    AValue := 1
  else if AValue > 1024 then
    AValue := 1024;

  if AValue <> FGeometryVerticesOut then
  begin
    FGeometryVerticesOut := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKShaderEx.SetShaderType(AValue: TVKShaderType);
begin
  if FShaderType <> AValue then
  begin
    FShaderType := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKShaderEx.SetSource(AValue: TStringList);
begin
  FSource.Assign(AValue);
end;

procedure TVKShaderEx.SetSourceFile(AValue: string);
begin
  FixPathDelimiter(AValue);
  if FSourceFile <> AValue then
  begin
    FSourceFile := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKShaderEx.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    if Length(FSourceFile) = 0 then
      WriteString(FSource.Text)
    else
      WriteString('');
    WriteString(FSourceFile);
    WriteInteger(Integer(FShaderType));
    WriteInteger(Integer(FGeometryInput));
    WriteInteger(Integer(FGeometryOutput));
    WriteInteger(FGeometryVerticesOut);
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKLibMaterialProperty'}{$ENDIF}

function TVKLibMaterialProperty.GetMaterial: TVKLibMaterialEx;
begin
  if Owner is TVKLibMaterialEx then
    Result := TVKLibMaterialEx(Owner)
  else if Owner is TVKLibMaterialProperty then
    Result := TVKLibMaterialProperty(Owner).GetMaterial
  else
    Result := nil;
end;

function TVKLibMaterialProperty.GetMaterialLibrary: TVKAbstractMaterialLibrary;
begin
  if Owner is TVKBaseMaterialCollectionItem then
    Result := TVKBaseMaterialCollectionItem(Owner).GetMaterialLibrary
  else
    Result := GetMaterial.GetMaterialLibrary;
end;

function TVKLibMaterialProperty.GetMaterialLibraryEx: TVKMaterialLibraryEx;
begin
  if Owner is TVKBaseMaterialCollectionItem then
    Result := TVKBaseMaterialCollectionItem(Owner).GetMaterialLibraryEx
  else
    Result := TVKMaterialLibraryEx(GetMaterial.GetMaterialLibrary);
end;

procedure TVKLibMaterialProperty.SetNextPass(const AValue: TVKLibMaterialName);
begin
  if AValue <> FNextPassName then
  begin
    FNextPassName := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKLibMaterialProperty.Loaded;
begin
end;

procedure TVKLibMaterialProperty.NotifyChange(Sender: TObject);
var
  NA: IGLNotifyAble;
begin
  if Assigned(Owner) then
  begin
    if Supports(Owner, IGLNotifyAble, NA) then
      NA.NotifyChange(Self)
  end;
  if Assigned(OnNotifyChange) then
    OnNotifyChange(Self);
end;

procedure TVKLibMaterialProperty.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if Owner is TVKLibMaterialEx then
      GetMaterial.NotifyChange(Self);
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKLibMaterialsEx'}{$ENDIF}

function TVKLibMaterialsEx.Add: TVKLibMaterialEx;
begin
  Result := (inherited Add) as TVKLibMaterialEx;
end;

constructor TVKLibMaterialsEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TVKLibMaterialEx);
end;

function TVKLibMaterialsEx.FindItemID(ID: Integer): TVKLibMaterialEx;
begin
  Result := (inherited FindItemID(ID)) as TVKLibMaterialEx;
end;

function TVKLibMaterialsEx.GetItems(AIndex: Integer): TVKLibMaterialEx;
begin
  Result := TVKLibMaterialEx(inherited Items[AIndex]);
end;

function TVKLibMaterialsEx.GetLibMaterialByName(
  const AName: string): TVKLibMaterialEx;
var
  LMaterial: TVKAbstractLibMaterial;
begin
  LMaterial := GetMaterial(AName);
  if Assigned(LMaterial) and (LMaterial is TVKLibMaterialEx) then
    Result := TVKLibMaterialEx(LMaterial)
  else
    Result := nil;
end;

function TVKLibMaterialsEx.IndexOf(const Item: TVKLibMaterialEx): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I) = Item then
      begin
        Result := I;
        Exit;
      end;
end;

function TVKLibMaterialsEx.MaterialLibrary: TVKMaterialLibraryEx;
begin
  Result := TVKMaterialLibraryEx(GetOwner);
end;

procedure TVKLibMaterialsEx.SetItems(AIndex: Integer;
  const AValue: TVKLibMaterialEx);
begin
  inherited Items[AIndex] := AValue;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKBaseShaderModel'}{$ENDIF}

procedure TVKBaseShaderModel.Apply(var ARci: TRenderContextInfo);
var
  I: Integer;
  LEvent: TOnUniformSetting;
begin
  if FIsValid then
  begin
    FHandle.UseProgramObject;
    if FAutoFill then
      for I := FUniforms.Count - 1 downto 0 do
        TVKAbstractShaderUniform(FUniforms[I]).Apply(ARci);

    if Self is TVKShaderModel3 then
      LEvent := GetMaterial.FOnSM3UniformSetting
    else if Self is TVKShaderModel4 then
      LEvent := GetMaterial.FOnSM4UniformSetting
    else if Self is TVKShaderModel5 then
      LEvent := GetMaterial.FOnSM5UniformSetting
    else
      LEvent := nil;

    if Assigned(LEvent) then
      LEvent(Self, ARci);
  end;
end;

procedure TVKBaseShaderModel.Assign(Source: TPersistent);
var
  SM: TVKBaseShaderModel;
begin
  if Source is TVKBaseShaderModel then
  begin
    SM := TVKBaseShaderModel(Source);
    LibVertexShaderName := SM.LibVertexShaderName;
    LibFragmentShaderName := SM.LibFragmentShaderName;
    LibGeometryShaderName := SM.LibGeometryShaderName;
    LibTessControlShaderName := SM.LibTessControlShaderName;
    LibTessEvalShaderName := SM.LibTessEvalShaderName;
  end;
  inherited;
end;

constructor TVKBaseShaderModel.Create(AOwner: TPersistent);
begin
  inherited;
  FHandle := TVKProgramHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FEnabled := False;
  FUniforms := TPersistentObjectList.Create;
  FAutoFill := True;
end;

procedure TVKBaseShaderModel.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty(
    'Uniforms',
    ReadUniforms,
    WriteUniforms,
    FUniforms.Count > 0);
end;

destructor TVKBaseShaderModel.Destroy;
begin
  FHandle.Destroy;
  LibVertexShaderName := '';
  LibFragmentShaderName := '';
  LibGeometryShaderName := '';
  LibTessControlShaderName := '';
  LibTessEvalShaderName := '';
  FUniforms.CleanFree;
  inherited;
end;

procedure TVKBaseShaderModel.DoOnPrepare(Sender: TVKContext);
var
  T: TVKShaderType;
  LUniforms: TPersistentObjectList;
  LUniform, LUniform2: TVKShaderUniform;
  ID: TGLuint;
  I, J, C: Integer;
  buff: array[0..255] of AnsiChar;
  Size: TGLInt;
  Len: GLsizei;
  Loc: TGLint;
  AType: GLenum;
  UName: string;
  GLSLData: TVKSLDataType;
  GLSLSampler: TVKSLSamplerType;
  bSampler: Boolean;
  bNew: Boolean;
  LEvent: TOnUniformInitialize;
begin
  if FEnabled then
    try
      if IsSupported and FHandle.IsSupported then
      begin
        FHandle.AllocateHandle;
        if FHandle.IsDataNeedUpdate then
        begin
          // Validate shaders
          for T := Low(TVKShaderType) to High(TVKShaderType) do
            if Assigned(FShaders[T]) then
            begin
              FShaders[T].DoOnPrepare(Sender);
              if not FShaders[T].IsValid then
              begin
                if IsDesignTime then
                  FInfoLog := Format('%s shader "%s" is invalid',
                    [cShaderTypeName[FShaders[T].ShaderType],
                      FShaders[T].Name]);
                FIsValid := False;
                exit;
              end;
            end;
          // Gather shader
          FHandle.DetachAllObject;
          for T := Low(TVKShaderType) to High(TVKShaderType) do
            if Assigned(FShaders[T]) then
              FHandle.AttachObject(FShaders[T].Handle);
          ID := FHandle.Handle;

          with GL do
          begin
            // Can be override by layouts in shader
            if Assigned(FShaders[shtGeometry]) then
            begin
              ProgramParameteri(ID, GL_GEOMETRY_INPUT_TYPE_EXT,
                cGLgsInTypes[FShaders[shtGeometry].GeometryInput]);
              ProgramParameteri(ID, GL_GEOMETRY_OUTPUT_TYPE_EXT,
                cGLgsOutTypes[FShaders[shtGeometry].GeometryOutput]);
              ProgramParameteri(ID, GL_GEOMETRY_VERTICES_OUT_EXT,
                FShaders[shtGeometry].GeometryVerticesOut);
            end;

            if FHandle.LinkProgram then
            begin

              // Get final values
              if Assigned(FShaders[shtGeometry]) then
              begin
                GetProgramiv(ID, GL_GEOMETRY_INPUT_TYPE_EXT, @AType);
                case AType of
                  GL_POINTS: FShaders[shtGeometry].FGeometryInput := gsInPoints;
                  GL_LINES: FShaders[shtGeometry].FGeometryInput := gsInLines;
                  GL_LINES_ADJACENCY_EXT: FShaders[shtGeometry].FGeometryInput
                    := gsInAdjLines;
                  GL_TRIANGLES: FShaders[shtGeometry].FGeometryInput :=
                    gsInTriangles;
                  GL_TRIANGLES_ADJACENCY_EXT:
                    FShaders[shtGeometry].FGeometryInput := gsInAdjTriangles;
                end;
                GetProgramiv(ID, GL_GEOMETRY_OUTPUT_TYPE_EXT, @AType);
                case AType of
                  GL_POINTS: FShaders[shtGeometry].FGeometryOutput :=
                    gsOutPoints;
                  GL_LINE_STRIP: FShaders[shtGeometry].FGeometryOutput :=
                    gsOutLineStrip;
                  GL_TRIANGLE_STRIP: FShaders[shtGeometry].FGeometryOutput :=
                    sOutTriangleStrip;
                end;
                GetProgramiv(ID, GL_GEOMETRY_VERTICES_OUT_EXT, @I);
                if I > 0 then
                  FShaders[shtGeometry].FGeometryVerticesOut := I;
                ClearError;
              end;

              // Get uniforms
              LUniforms := TPersistentObjectList.Create;

              GL.GetProgramiv(ID, GL_ACTIVE_UNIFORMS, @C);
              for I := 0 to C - 1 do
              begin
                GetActiveUniform(
                  ID,
                  TGLuint(I),
                  Length(buff),
                  @Len,
                  @Size,
                  @AType,
                  @buff[0]);
                Loc := GetUniformLocation(ID, @buff[0]);
                if Loc < 0 then
                  continue;
                UName := Copy(string(buff), 0, Len);
                GLSLData := GLSLTypeUndefined;
                GLSLSampler := GLSLSamplerUndefined;
                case AType of
                  GL_FLOAT: GLSLData := GLSLType1F;
                  GL_FLOAT_VEC2: GLSLData := GLSLType2F;
                  GL_FLOAT_VEC3: GLSLData := GLSLType3F;
                  GL_FLOAT_VEC4: GLSLData := GLSLType4F;
                  GL_INT: GLSLData := GLSLType1I;
                  GL_INT_VEC2: GLSLData := GLSLType2I;
                  GL_INT_VEC3: GLSLData := GLSLType3I;
                  GL_INT_VEC4: GLSLData := GLSLType4I;
                  GL_UNSIGNED_INT: GLSLData := GLSLType1UI;
                  GL_UNSIGNED_INT_VEC2: GLSLData := GLSLType2UI;
                  GL_UNSIGNED_INT_VEC3: GLSLData := GLSLType3UI;
                  GL_UNSIGNED_INT_VEC4: GLSLData := GLSLType4UI;
                  GL_BOOL: GLSLData := GLSLType1I;
                  GL_BOOL_VEC2: GLSLData := GLSLType2I;
                  GL_BOOL_VEC3: GLSLData := GLSLType3I;
                  GL_BOOL_VEC4: GLSLData := GLSLType4I;
                  GL_FLOAT_MAT2: GLSLData := GLSLTypeMat2F;
                  GL_FLOAT_MAT3: GLSLData := GLSLTypeMat3F;
                  GL_FLOAT_MAT4: GLSLData := GLSLTypeMat4F;
                  //------------------------------------------------------------------------------
                  GL_SAMPLER_1D: GLSLSampler := GLSLSampler1D;
                  GL_SAMPLER_2D: GLSLSampler := GLSLSampler2D;
                  GL_SAMPLER_3D: GLSLSampler := GLSLSampler3D;
                  GL_SAMPLER_CUBE: GLSLSampler := GLSLSamplerCube;
                  GL_SAMPLER_1D_SHADOW: GLSLSampler := GLSLSampler1DShadow;
                  GL_SAMPLER_2D_SHADOW: GLSLSampler := GLSLSampler2DShadow;
                  GL_SAMPLER_2D_RECT: GLSLSampler := GLSLSamplerRect;
                  GL_SAMPLER_2D_RECT_SHADOW: GLSLSampler :=
                    GLSLSamplerRectShadow;
                  GL_SAMPLER_BUFFER: GLSLSampler := GLSLSamplerBuffer;
                  GL_INT_SAMPLER_2D_RECT: GLSLSampler :=
                    GLSLIntSamplerRect;
                  GL_INT_SAMPLER_BUFFER: GLSLSampler :=
                    GLSLIntSamplerBuffer;
                  GL_UNSIGNED_INT_SAMPLER_1D: GLSLSampler :=
                    GLSLUIntSampler1D;
                  GL_UNSIGNED_INT_SAMPLER_2D: GLSLSampler :=
                    GLSLUIntSampler2D;
                  GL_UNSIGNED_INT_SAMPLER_3D: GLSLSampler :=
                    GLSLUIntSampler3D;
                  GL_UNSIGNED_INT_SAMPLER_CUBE: GLSLSampler :=
                    GLSLUIntSamplerCube;
                  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY: GLSLSampler :=
                    GLSLUIntSampler1DArray;
                  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY: GLSLSampler :=
                    GLSLUIntSampler2DArray;
                  GL_UNSIGNED_INT_SAMPLER_2D_RECT: GLSLSampler :=
                    GLSLUIntSamplerRect;
                  GL_UNSIGNED_INT_SAMPLER_BUFFER: GLSLSampler :=
                    GLSLUIntSamplerBuffer;
                  GL_SAMPLER_2D_MULTISAMPLE: GLSLSampler :=
                    GLSLSamplerMS;
                  GL_INT_SAMPLER_2D_MULTISAMPLE: GLSLSampler :=
                    GLSLIntSamplerMS;
                  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE: GLSLSampler :=
                    GLSLUIntSamplerMS;
                  GL_SAMPLER_2D_MULTISAMPLE_ARRAY: GLSLSampler :=
                    GLSLSamplerMSArray;
                  GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY: GLSLSampler :=
                    GLSLIntSamplerMSArray;
                  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY: GLSLSampler :=
                    GLSLUIntSamplerMSArray;
                end;

                bSampler := False;
                if (GLSLData = GLSLTypeUndefined) and (GLSLSampler =
                  GLSLSamplerUndefined) then
                begin
                  GLSLogger.LogWarningFmt(
                    'Detected active uniform "%s" with unknown type', [UName]);
                  continue;
                end
                else if GLSLData <> GLSLTypeUndefined then
                begin
                  GLSLogger.LogInfoFmt('Detected active uniform: %s %s',
                    [cGLSLTypeString[GLSLData], UName]);
                end
                else
                begin
                  bSampler := True;
                  GLSLogger.LogInfoFmt('Detected active uniform: %s %s',
                    [cGLSLSamplerString[GLSLSampler], UName]);
                end;

                // Find already existing uniform
                bNew := True;
                for J := 0 to FUniforms.Count - 1 do
                begin
                  if not (FUniforms[J] is TVKShaderUniform) then
                    continue;
                  LUniform := TVKShaderUniform(FUniforms[J]);
                  if not Assigned(LUniform) then
                    continue;
                  if LUniform.Name = UName then
                  begin
                    if bSampler and (LUniform is TVKShaderUniformTexture) then
                    begin
                      if TVKShaderUniformTexture(LUniform).FSamplerType =
                        GLSLSampler then
                      begin
                        LUniform.FLocation := Loc;
                        LUniform.FType := GLSLType1I;
                        TVKShaderUniformTexture(LUniform).FTarget :=
                          cSamplerToTexture[GLSLSampler];
                        LUniforms.Add(LUniform);
                        FUniforms[J] := nil;
                        bNew := False;
                        break;
                      end
                    end
                    else
                    begin
                      if LUniform.FType = GLSLData then
                      begin
                        if (LUniform is TVKShaderUniformDSA)
                          and not EXT_direct_state_access then
                        begin
                          LUniform2 := LUniform;
                          LUniform := TVKShaderUniform.Create(Self);
                          LUniform._AddRef;
                          LUniform.Assign(LUniform2);
                          LUniform2._Release;
                        end;
                        LUniform.FLocation := Loc;
                        LUniforms.Add(LUniform);
                        FUniforms[J] := nil;
                        bNew := False;
                        break;
                      end;
                    end;
                  end;
                end; // for J

                if bNew then
                begin
                  // Create new uniform
                  if bSampler then
                  begin
                    LUniform := TVKShaderUniformTexture.Create(Self);
                    LUniform.FType := GLSLType1I;
                    TVKShaderUniformTexture(LUniform).FSamplerType :=
                      GLSLSampler;
                    TVKShaderUniformTexture(LUniform).FTarget :=
                      cSamplerToTexture[GLSLSampler];
                  end
                  else
                  begin
                    if EXT_direct_state_access then
                      LUniform := TVKShaderUniformDSA.Create(Self)
                    else
                      LUniform := TVKShaderUniform.Create(Self);
                    LUniform.FType := GLSLData;
                  end;
                  LUniform._AddRef;
                  LUniform.FName := UName;
                  LUniform.FNameHashCode := ComputeNameHashKey(UName);
                  LUniform.FLocation := Loc;
                  LUniforms.Add(LUniform);
                end;
              end; // for I

              // Clean old unused uniforms
              ReleaseUniforms(FUniforms);
              // Assign new one
              FUniforms := LUniforms;

              FHandle.NotifyDataUpdated;
              FIsValid := True;

              if Self is TVKShaderModel3 then
                LEvent := GetMaterial.FOnSM3UniformInit
              else if Self is TVKShaderModel4 then
                LEvent := GetMaterial.FOnSM4UniformInit
              else if Self is TVKShaderModel5 then
                LEvent := GetMaterial.FOnSM5UniformInit
              else
                LEvent := nil;

              if Assigned(LEvent) then
                LEvent(Self);

            end // if LinkProgram
            else
              FIsValid := False;
          end; // with GL

          if IsDesignTime then
          begin
            FInfoLog := FHandle.InfoLog;
            if (Length(FInfoLog) = 0) and FIsValid then
              FInfoLog := 'Link successful';
          end
          else if FIsValid then
            GLSLogger.LogInfoFmt('Program "%s" link successful - %s',
              [GetMaterial.Name, FHandle.InfoLog])
          else
            GLSLogger.LogErrorFmt('Program "%s" link failed! - %s',
              [GetMaterial.Name, FHandle.InfoLog]);
        end;
      end
      else
      begin
        if IsDesignTime then
          FInfoLog := 'Not supported by hardware';
        FIsValid := False;
      end;

    except
      on E: Exception do
      begin
        FIsValid := False;
        if IsDesignTime then
          InformationDlg(E.ClassName + ': ' + E.Message)
        else
          GLSLogger.LogError(E.ClassName + ': ' + E.Message);
      end;
    end;
end;

procedure TVKBaseShaderModel.Notification(Sender: TObject; Operation:
  TOperation);
var
  st: TVKShaderType;
begin
  if Operation = opRemove then
  begin
    for st := Low(TVKShaderType) to High(TVKShaderType) do
      if FShaders[st] = Sender then
      begin
        FShaders[st] := nil;
        FLibShaderName[st] := '';
        NotifyChange(Self);
        exit;
      end;
  end;
end;

procedure TVKBaseShaderModel.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TVKBaseShaderModel.ReadUniforms(AStream: TStream);
var
  LReader: TReader;
  N, I: Integer;
  str: string;
  LUniform: TVKAbstractShaderUniform;
  LClass: CGLAbstractShaderUniform;
begin
  LReader := TReader.Create(AStream, 16384);
  try
    N := LReader.ReadInteger;
    for I := 0 to N - 1 do
    begin
      str := LReader.ReadString;
      LClass := CGLAbstractShaderUniform(FindClass(str));
      LUniform := LClass.Create(Self);
      LUniform._AddRef;
      LUniform.ReadFromFiler(LReader);
      FUniforms.Add(LUniform);
    end;
  finally
    LReader.Free;
  end;
end;

class procedure TVKBaseShaderModel.ReleaseUniforms(
  AList: TPersistentObjectList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    if Assigned(AList[I]) then
      TVKAbstractShaderUniform(AList[I])._Release;
  AList.Destroy;
end;

function TVKBaseShaderModel.GetLibShaderName(AType: TVKShaderType): string;
begin
  if Assigned(FShaders[AType]) then
    Result := FShaders[AType].Name
  else
    Result := '';
end;

function TVKBaseShaderModel.GetUniform(const AName: string): IShaderParameter;
var
  H, I: Integer;
  U: TVKAbstractShaderUniform;
begin
  Result := nil;
  H := ComputeNameHashKey(AName);
  for I := 0 to FUniforms.Count - 1 do
  begin
    U := TVKAbstractShaderUniform(FUniforms[I]);
    if (U.FNameHashCode = H) and (U.FName = AName) then
    begin
      Result := U;
      exit;
    end;
  end;

  if not IsDesignTime then
  begin
    GLSLogger.LogErrorFmt('Attempt to use unknow uniform "%s" for material "%s"',
      [AName, GetMaterial.Name]);
    U := TVKAbstractShaderUniform.Create(Self);
    U._AddRef;
    U.FName := AName;
    U.FNameHashCode := H;
    FUniforms.Add(U);
    Result := U;
  end;
end;

procedure TVKBaseShaderModel.Loaded;
var
  T: TVKShaderType;
  I: Integer;
begin
  for T := Low(TVKShaderType) to High(TVKShaderType) do
    SetLibShaderName(T, FLibShaderName[T]);
  for I := 0 to FUniforms.Count - 1 do
    if FUniforms[I] is TVKShaderUniformTexture then
      TVKShaderUniformTexture(FUniforms[I]).Loaded;
end;

procedure TVKBaseShaderModel.GetUniformNames(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to FUniforms.Count - 1 do
    Proc(TVKAbstractShaderUniform(FUniforms[I]).FName);
end;

procedure TVKBaseShaderModel.SetLibShaderName(AType: TVKShaderType;
  const AValue: string);
var
  LShader: TVKShaderEx;
begin
  if csLoading in GetMaterialLibraryEx.ComponentState then
  begin
    FLibShaderName[AType] := AValue;
    exit;
  end;

  if Assigned(FShaders[AType]) then
  begin
    FShaders[AType].UnregisterUser(Self);
    FShaders[AType] := nil;
    FLibShaderName[AType] := '';
  end;

  LShader := GetMaterialLibraryEx.Components.GetShaderByName(AValue);
  if Assigned(LShader) then
  begin
    if LShader.ShaderType <> AType then
    begin
      if IsDesignTime then
        InformationDlg(Format('Incompatible shader type, need %s shader',
          [cShaderTypeName[AType]]));
      exit;
    end;
    LShader.RegisterUser(Self);
    FShaders[AType] := LShader;
    FLibShaderName[AType] := AValue;
  end;
  NotifyChange(Self);
end;

procedure TVKBaseShaderModel.UnApply(var ARci: TRenderContextInfo);
begin
  if FIsValid and not ARci.GLStates.ForwardContext then
    FHandle.EndUseProgramObject;
end;

procedure TVKBaseShaderModel.WriteUniforms(AStream: TStream);
var
  LWriter: TWriter;
  I: Integer;
begin
  LWriter := TWriter.Create(AStream, 16384);
  try
    LWriter.WriteInteger(FUniforms.Count);
    for I := 0 to FUniforms.Count - 1 do
    begin
      LWriter.WriteString(FUniforms[I].ClassName);
      TVKAbstractShaderUniform(FUniforms[I]).WriteToFiler(LWriter);
    end;
  finally
    LWriter.Free;
  end;
end;

class function TVKShaderModel3.IsSupported: Boolean;
begin
  Result := GL.ARB_shader_objects;
end;

class function TVKShaderModel4.IsSupported: Boolean;
begin
  Result := GL.EXT_gpu_shader4;
end;

class function TVKShaderModel5.IsSupported: Boolean;
begin
  Result := GL.ARB_gpu_shader5;
end;

procedure BeginPatch(mode: TGLenum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  if mode = GL_PATCHES then
    vStoreBegin(GL_PATCHES)
  else if (mode = GL_TRIANGLES)
    or (mode = GL_TRIANGLE_STRIP)
    or (mode = GL_TRIANGLE_FAN)
    or (mode = GL_QUADS) then
  begin
    if mode = GL_QUADS then
      GL.PatchParameteri(GL_PATCH_VERTICES, 4)
    else
      GL.PatchParameteri(GL_PATCH_VERTICES, 3);
    vStoreBegin(GL_PATCHES);
  end
  else
  begin
    GL.Begin_ := vStoreBegin;
    GLSLogger.LogError('glBegin called with unsupported primitive for tessellation');
    Abort;
  end;
end;

procedure TVKShaderModel5.Apply(var ARci: TRenderContextInfo);
begin
  if Assigned(FShaders[shtControl]) or Assigned(FShaders[shtEvaluation]) then
  begin
    vStoreBegin := GL.Begin_;
    GL.Begin_ := BeginPatch;
    ARci.amalgamating := True;
  end;
  inherited;
end;

procedure TVKShaderModel5.UnApply(var ARci: TRenderContextInfo);
begin
  inherited;
  if Assigned(FShaders[shtControl]) or Assigned(FShaders[shtEvaluation]) then
    GL.Begin_ := vStoreBegin;
  ARci.amalgamating := False;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKMatLibComponents'}{$ENDIF}

function TVKMatLibComponents.GetAttachmentByName(
  const AName: TVKMaterialComponentName): TVKFrameBufferAttachment;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TVKFrameBufferAttachment) and (Items[I].FNameHashKey = N)
      then
    begin
      if Items[I].Name = AName then
      begin
        Result := TVKFrameBufferAttachment(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TVKMatLibComponents.GetCombinerByName(
  const AName: TVKMaterialComponentName): TVKTextureCombiner;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TVKTextureCombiner) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TVKTextureCombiner(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TVKMatLibComponents.GetItemByName(
  const AName: TVKMaterialComponentName): TVKBaseMaterialCollectionItem;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I].FNameHashKey = N) and (Items[I].Name = AName) then
    begin
      Result := Items[I];
      exit;
    end;
  end;
  Result := nil;
end;

function TVKMatLibComponents.GetItems(
  index: Integer): TVKBaseMaterialCollectionItem;
begin
  Result := TVKBaseMaterialCollectionItem(inherited GetItems(index));
end;

function TVKMatLibComponents.GetNamePath: string;
var
  s: string;
begin
  Result := ClassName;
  if GetOwner = nil then
    Exit;
  s := GetOwner.GetNamePath;
  if s = '' then
    Exit;
  Result := s + '.Components';
end;

function TVKMatLibComponents.GetSamplerByName(
  const AName: TVKMaterialComponentName): TVKTextureSampler;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TVKTextureSampler) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TVKTextureSampler(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TVKMatLibComponents.GetShaderByName(
  const AName: TVKMaterialComponentName): TVKShaderEx;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TVKShaderEx) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TVKShaderEx(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TVKMatLibComponents.GetAsmProgByName(
  const AName: TVKMaterialComponentName): TVKASMVertexProgram;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TVKASMVertexProgram) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TVKASMVertexProgram(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TVKMatLibComponents.GetTextureByName(
  const AName: TVKMaterialComponentName): TVKAbstractTexture;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TVKAbstractTexture) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TVKTextureImageEx(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

class function TVKMatLibComponents.ItemsClass: TVKXCollectionItemClass;
begin
  Result := TVKBaseMaterialCollectionItem;
end;

function TVKMatLibComponents.MakeUniqueName(const AName:
  TVKMaterialComponentName): TVKMaterialComponentName;
var
  I: Integer;
begin
  Result := AName;
  I := 1;
  while GetItemByName(Result) <> nil do
  begin
    Result := AName + IntToStr(i);
    Inc(i);
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKMaterialLibraryEx'}{$ENDIF}

function TVKMaterialLibraryEx.AddAttachment(
  const AName: TVKMaterialComponentName): TVKFrameBufferAttachment;
begin
  Result := TVKFrameBufferAttachment.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TVKMaterialLibraryEx.AddCombiner(
  const AName: TVKMaterialComponentName): TVKTextureCombiner;
begin
  Result := TVKTextureCombiner.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TVKMaterialLibraryEx.AddSampler(
  const AName: TVKMaterialComponentName): TVKTextureSampler;
begin
  Result := TVKTextureSampler.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TVKMaterialLibraryEx.AddShader(
  const AName: TVKMaterialComponentName): TVKShaderEx;
begin
  Result := TVKShaderEx.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TVKMaterialLibraryEx.AddAsmProg(
  const AName: TVKMaterialComponentName): TVKASMVertexProgram;
begin
  Result := TVKASMVertexProgram.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TVKMaterialLibraryEx.AddTexture(
  const AName: TVKMaterialComponentName): TVKTextureImageEx;
begin
  Result := TVKTextureImageEx.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

constructor TVKMaterialLibraryEx.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TVKLibMaterialsEx.Create(Self);
  FComponents := TVKMatLibComponents.Create(Self);
end;

procedure TVKMaterialLibraryEx.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty(
    'ComponentsData',
    ReadComponents,
    WriteComponents,
    Components.Count > 0);
  inherited;
end;

destructor TVKMaterialLibraryEx.Destroy;
begin
  FMaterials.Destroy;
  FComponents.Destroy;
  inherited;
end;

function TVKMaterialLibraryEx.GetMaterials: TVKLibMaterialsEx;
begin
  Result := TVKLibMaterialsEx(FMaterials);
end;

procedure TVKMaterialLibraryEx.GetNames(Proc: TGetStrProc;
  AClass: CGLBaseMaterialCollectionItem);
var
  I: Integer;
begin
  for I := 0 to Components.Count - 1 do
    if Components[I].ClassType = AClass then
      Proc(Components[I].Name)
end;

procedure TVKMaterialLibraryEx.Loaded;
begin
  inherited;
end;

procedure TVKMaterialLibraryEx.ReadComponents(AStream: TStream);
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

procedure TVKMaterialLibraryEx.SetComponents(AValue: TVKMatLibComponents);
begin
  FComponents.Assign(AValue);
end;

procedure TVKMaterialLibraryEx.SetLevelForAll(const ALevel: TVKMaterialLevel);
var
  I: Integer;
begin
  for I := Materials.Count - 1 downto 0 do
    Materials[I].ApplicableLevel := ALevel;
end;

procedure TVKMaterialLibraryEx.SetMaterials(AValue: TVKLibMaterialsEx);
begin
  FMaterials.Assign(AValue);
end;

function TVKMaterialLibraryEx.StoreMaterials: Boolean;
begin
  Result := (FMaterials.Count > 0);
end;

procedure TVKMaterialLibraryEx.WriteComponents(AStream: TStream);
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

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKShaderUniformTexture'}{$ENDIF}

procedure TVKShaderUniformTexture.Apply(var ARci: TRenderContextInfo);

  function FindHotActiveUnit: Boolean;
  var
    ID: TGLuint;
    I, J: Integer;
    bindTime, minTime: Double;
    LTex: TVKTextureImageEx;
  begin
    with ARci.GLStates do
    begin
      if Assigned(FLibTexture) and FLibTexture.IsValid then
      begin
        ID := FLibTexture.FHandle.Handle;
        // Yar: may be need exract this to new method of TVKTextureImageEx ???
        if FLibTexture is TVKTextureImageEx then
        begin
          LTex := TVKTextureImageEx(FLibTexture);
          Inc(LTex.FApplyCounter);
          if LTex.FApplyCounter > 16 then
            FreeAndNil(LTex.FImage);
        end;
      end
      else
        ID := 0;

      // Find alredy binded texture unit
      for I := 0 to MaxTextureImageUnits - 1 do
      begin
        if TextureBinding[I, FTarget] = ID then
        begin
          GL.Uniform1i(FLocation, I);
          ActiveTexture := I;
          Result := True;
          exit;
        end;
      end;
      // Find unused texture unit
      for I := 0 to MaxTextureImageUnits - 1 do
      begin
        if TextureBinding[I, FTarget] = 0 then
        begin
          TextureBinding[I, FTarget] := ID;
          GL.Uniform1i(FLocation, I);
          ActiveTexture := I;
          Result := True;
          exit;
        end;
      end;
      // Find most useless texture unit
      minTime := GLSTime;
      J := 0;
      for I := 0 to MaxTextureImageUnits - 1 do
      begin
        bindTime := TextureBindingTime[I, FTarget];
        if bindTime < minTime then
        begin
          minTime := bindTime;
          J := I;
        end;
      end;

      TextureBinding[J, FTarget] := ID;
      ActiveTexture := J;
      GL.Uniform1i(FLocation, J);
      Result := True;
      exit;
    end;
    Result := False;
  end;

var
  glTarget: TGLenum;
begin
  if FLocation > -1 then
  begin
    if FindHotActiveUnit and Assigned(FLibTexture) and Assigned(FLibSampler)
      then
      with GL do
      begin
        // Apply swizzling if possible
        glTarget := DecodeGLTextureTarget(FLibTexture.Shape);
        if ARB_texture_swizzle or EXT_texture_swizzle then
        begin

          if FSwizzling[0] <> FLibTexture.FSwizzles[0] then
          begin
            FLibTexture.FSwizzles[0] := FSwizzling[0];
            TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_R,
              cTextureSwizzle[FSwizzling[0]]);
          end;
          if FSwizzling[1] <> FLibTexture.FSwizzles[1] then
          begin
            FLibTexture.FSwizzles[1] := FSwizzling[1];
            TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_G,
              cTextureSwizzle[FSwizzling[1]]);
          end;
          if FSwizzling[2] <> FLibTexture.FSwizzles[2] then
          begin
            FLibTexture.FSwizzles[2] := FSwizzling[2];
            TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_B,
              cTextureSwizzle[FSwizzling[2]]);
          end;
          if FSwizzling[3] <> FLibTexture.FSwizzles[3] then
          begin
            FLibTexture.FSwizzles[3] := FSwizzling[3];
            TexParameteri(glTarget, GL_TEXTURE_SWIZZLE_A,
              cTextureSwizzle[FSwizzling[3]]);
          end;
        end;

        if FLibSampler.IsValid then
          FLibSampler.Apply(ARci)
        else if FLibTexture.FLastSampler <> FLibSampler then
        begin
          // Sampler object not supported, lets use texture states
          TexParameterfv(glTarget, GL_TEXTURE_BORDER_COLOR,
            FLibSampler.BorderColor.AsAddress);
          TexParameteri(glTarget, GL_TEXTURE_WRAP_S,
            cTextureWrapMode[FLibSampler.WrapX]);
          TexParameteri(glTarget, GL_TEXTURE_WRAP_T,
            cTextureWrapMode[FLibSampler.WrapY]);
          TexParameteri(glTarget, GL_TEXTURE_WRAP_R,
            cTextureWrapMode[FLibSampler.WrapZ]);
          TexParameterf(glTarget, GL_TEXTURE_LOD_BIAS, FLibSampler.LODBias +
            FLibSampler.FLODBiasFract);
          TexParameteri(glTarget, GL_TEXTURE_MIN_FILTER,
            cTextureMinFilter[FLibSampler.MinFilter]);
          TexParameteri(glTarget, GL_TEXTURE_MAG_FILTER,
            cTextureMagFilter[FLibSampler.MagFilter]);

          if EXT_texture_filter_anisotropic then
          begin
            if FLibSampler.FilteringQuality = tfAnisotropic then
              TexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT,
                CurrentGLContext.GLStates.MaxTextureAnisotropy)
            else
              TexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
          end;

          TexParameteri(glTarget, GL_TEXTURE_COMPARE_MODE,
            cTextureCompareMode[FLibSampler.CompareMode]);
          TexParameteri(glTarget, GL_TEXTURE_COMPARE_FUNC,
            cGLComparisonFunctionToGLEnum[FLibSampler.CompareFunc]);

          if EXT_texture_sRGB_decode then
          begin
            if FLibSampler.sRGB_Encode then
              TexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT, GL_DECODE_EXT)
            else
              TexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT,
                GL_SKIP_DECODE_EXT);
          end;

          FLibTexture.FLastSampler := FLibSampler;
        end;

      end; // with GL
  end;
end;

procedure TVKShaderUniformTexture.Assign(Source: TPersistent);
var
  LUniform: TVKShaderUniformTexture;
begin
  if Source is TVKShaderUniformTexture then
  begin
    LUniform := TVKShaderUniformTexture(Source);
    LibTextureName := LUniform.LibTextureName;
    LibSamplerName := LUniform.LibSamplerName;
  end;
  inherited;
end;

constructor TVKShaderUniformTexture.Create(AOwner: TPersistent);
begin
  inherited;
  FSwizzling := cDefaultSwizzleVector;
end;

destructor TVKShaderUniformTexture.Destroy;
begin
  LibTextureName := '';
  LibSamplerName := '';
  inherited;
end;

function TVKShaderUniformTexture.GetSamplerName: string;
begin
  if Assigned(FLibSampler) then
    Result := FLibSampler.Name
  else
    Result := rstrNothing;
end;

function TVKShaderUniformTexture.GetTextureName: string;
begin
  if Assigned(FLibTexture) then
    Result := FLibTexture.Name
  else
    Result := rstrNothing;
end;

function TVKShaderUniformTexture.GetTextureSwizzle: TSwizzleVector;
begin
  Result := FSwizzling;
end;

procedure TVKShaderUniformTexture.Loaded;
begin
  SetTextureName(FLibTexureName);
  SetSamplerName(FLibSamplerName);
end;

procedure TVKShaderUniformTexture.Notification(Sender: TObject;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if Sender = FLibTexture then
      FLibTexture := nil
    else if Sender = FLibSampler then
      FLibSampler := nil;
  end;
end;

procedure TVKShaderUniformTexture.ReadFromFiler(AReader: TReader);
begin
  with AReader do
  begin
    inherited;
    LibTextureName := ReadString;
    LibSamplerName := ReadString;
    FSwizzling[0] := TVKTextureSwizzle(ReadInteger);
    FSwizzling[1] := TVKTextureSwizzle(ReadInteger);
    FSwizzling[2] := TVKTextureSwizzle(ReadInteger);
    FSwizzling[3] := TVKTextureSwizzle(ReadInteger);
  end;
end;

procedure TVKShaderUniformTexture.SetTextureName(
  const AValue: string);
var
  LTexture: TVKAbstractTexture;
begin
  if csLoading in TVKBaseShaderModel(Owner).GetMaterialLibraryEx.ComponentState
    then
  begin
    FLibTexureName := AValue;
    exit;
  end;

  if Assigned(FLibTexture) then
  begin
    if FLibTexture.Name = AValue then
      exit;
    FLibTexture.UnregisterUser(Self);
    FLibTexture := nil;
  end;

  LTexture :=
    TVKBaseShaderModel(Owner).GetMaterialLibraryEx.Components.GetTextureByName(AValue);

  if Assigned(LTexture) then
  begin
    if LTexture is TVKFrameBufferAttachment then
    begin
      if TVKFrameBufferAttachment(LTexture).OnlyWrite then
      begin
        if IsDesignTime then
          InformationDlg('Can not use write only attachment as texture')
        else
          GLSLogger.LogErrorFmt('Attempt to write only attachment "%s" for uniform "%s"',
            [LTexture.Name, Name]);
        NotifyChange(Self);
        exit;
      end;
    end;
    LTexture.RegisterUser(Self);
    FLibTexture := LTexture;
  end;
  NotifyChange(Self);
end;

procedure TVKShaderUniformTexture.SetSamplerName(const AValue: string);
var
  LSampler: TVKTextureSampler;
begin
  if csLoading in TVKBaseShaderModel(Owner).GetMaterialLibraryEx.ComponentState
    then
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

  LSampler :=
    TVKBaseShaderModel(Owner).GetMaterialLibraryEx.Components.GetSamplerByName(AValue);

  if Assigned(LSampler) then
  begin
    LSampler.RegisterUser(Self);
    FLibSampler := LSampler;
  end;

  NotifyChange(Self);
end;

procedure TVKShaderUniformTexture.SetTextureSwizzle(const AValue:
  TSwizzleVector);
begin
  FSwizzling := AValue;
end;

procedure TVKShaderUniformTexture.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    inherited;
    WriteString(LibTextureName);
    WriteString(LibSamplerName);
    WriteInteger(Integer(FSwizzling[0]));
    WriteInteger(Integer(FSwizzling[1]));
    WriteInteger(Integer(FSwizzling[2]));
    WriteInteger(Integer(FSwizzling[3]));
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKAbstractShaderUniform'}{$ENDIF}

function TVKAbstractShaderUniform.GetFloat: Single;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetGLSLSamplerType: TVKSLSamplerType;
begin
  Result := FSamplerType;
end;

function TVKAbstractShaderUniform.GetGLSLType: TVKSLDataType;
begin
  Result := FType;
end;

function TVKAbstractShaderUniform.GetInt: TGLint;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetIVec2: TVector2i;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetIVec3: TVector3i;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetIVec4: TVector4i;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetMat2: TMatrix2f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetMat3: TMatrix3f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetMat4: TMatrix4f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetName: string;
begin
  Result := FName;
end;

function TVKAbstractShaderUniform.GetSamplerName: string;
begin
  Result := rstrNothing;
end;

procedure TVKAbstractShaderUniform.Apply(var ARci: TRenderContextInfo);
begin
end;

function TVKAbstractShaderUniform.GetAutoSetMethod: string;
begin
  Result := rstrNothing;
end;

function TVKAbstractShaderUniform.GetTextureName: string;
begin
  Result := rstrNothing;
end;

function TVKAbstractShaderUniform.GetTextureSwizzle: TSwizzleVector;
begin
  Result := cDefaultSwizzleVector;
end;

function TVKAbstractShaderUniform.GetUInt: TGLuint;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetUVec2: TVector2ui;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetUVec3: TVector3ui;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetUVec4: TVector4ui;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetVec2: TVector2f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetVec3: TVector3f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVKAbstractShaderUniform.GetVec4: TVector;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

procedure TVKAbstractShaderUniform.ReadFromFiler(AReader: TReader);
begin
end;

procedure TVKAbstractShaderUniform.SetFloat(const Value: TGLfloat);
begin
end;

procedure TVKAbstractShaderUniform.SetFloatArray(const Values: PGLFloat;
  Count: Integer);
begin
end;

procedure TVKAbstractShaderUniform.SetInt(const Value: Integer);
begin
end;

procedure TVKAbstractShaderUniform.SetIntArray(const Values: PGLInt;
  Count: Integer);
begin
end;

procedure TVKAbstractShaderUniform.SetIVec2(const Value: TVector2i);
begin
end;

procedure TVKAbstractShaderUniform.SetIVec3(const Value: TVector3i);
begin
end;

procedure TVKAbstractShaderUniform.SetIVec4(const Value: TVector4i);
begin
end;

procedure TVKAbstractShaderUniform.SetMat2(const Value: TMatrix2f);
begin
end;

procedure TVKAbstractShaderUniform.SetMat3(const Value: TMatrix3f);
begin
end;

procedure TVKAbstractShaderUniform.SetMat4(const Value: TMatrix4f);
begin
end;

procedure TVKAbstractShaderUniform.SetSamplerName(const AValue: string);
begin
end;

procedure TVKAbstractShaderUniform.SetAutoSetMethod(const AValue: string);
begin
end;

procedure TVKAbstractShaderUniform.SetTextureName(const AValue: string);
begin
end;

procedure TVKAbstractShaderUniform.SetTextureSwizzle(const AValue:
  TSwizzleVector);
begin
end;

procedure TVKAbstractShaderUniform.SetUInt(const Value: GLuint);
begin
end;

procedure TVKAbstractShaderUniform.SetUIntArray(const Values: PGLUInt;
  Count: Integer);
begin
end;

procedure TVKAbstractShaderUniform.SetUVec2(const Value: TVector2ui);
begin
end;

procedure TVKAbstractShaderUniform.SetUVec3(const Value: TVector3ui);
begin
end;

procedure TVKAbstractShaderUniform.SetUVec4(const Value: TVector4ui);
begin
end;

procedure TVKAbstractShaderUniform.SetVec2(const Value: TVector2f);
begin
end;

procedure TVKAbstractShaderUniform.SetVec3(const Value: TVector3f);
begin
end;

procedure TVKAbstractShaderUniform.SetVec4(const Value: TVector4f);
begin
end;

procedure TVKAbstractShaderUniform.WriteToFiler(AWriter: TWriter);
begin
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKShaderUniform'}{$ENDIF}

function TVKShaderUniform.GetFloat: Single;
begin
  // TODO: Type checking
  GL.GetUniformfv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetInt: TGLint;
begin
  GL.GetUniformiv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetIVec2: TVector2i;
begin
  GL.GetUniformiv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetIVec3: TVector3i;
begin
  GL.GetUniformiv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetIVec4: TVector4i;
begin
  GL.GetUniformiv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetMat2: TMatrix2f;
begin
  GL.GetUniformfv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetMat3: TMatrix3f;
begin
  GL.GetUniformfv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetMat4: TMatrix4f;
begin
  GL.GetUniformfv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetProgram: TGLuint;
begin
  Result := TVKBaseShaderModel(Owner).FHandle.Handle;
end;

procedure TVKShaderUniform.Apply(var ARci: TRenderContextInfo);
begin
  if Assigned(FAutoSet) then
    FAutoSet(Self, ARci);
end;

procedure TVKShaderUniform.Assign(Source: TPersistent);
var
  LUniform: TVKShaderUniform;
begin
  if Source is TVKShaderUniform then
  begin
    LUniform := TVKShaderUniform(Source);
    FName := LUniform.Name;
    FNameHashCode := LUniform.FNameHashCode;
    FType := LUniform.FType;
    FSamplerType := LUniform.FSamplerType;
    FAutoSet := LUniform.FAutoSet;
  end;
  inherited;
end;

function TVKShaderUniform.GetAutoSetMethod: string;
begin
  Result := GetUniformAutoSetMethodName(FAutoSet);
end;

function TVKShaderUniform.GetUInt: TGLuint;
begin
  GL.GetUniformuiv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetUVec2: TVector2ui;
begin
  GL.GetUniformuiv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetUVec3: TVector3ui;
begin
  GL.GetUniformuiv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetUVec4: TVector4ui;
begin
  GL.GetUniformuiv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetVec2: TVector2f;
begin
  GL.GetUniformfv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetVec3: TVector3f;
begin
  GL.GetUniformfv(GetProgram, FLocation, @Result);
end;

function TVKShaderUniform.GetVec4: TVector;
begin
  GL.GetUniformfv(GetProgram, FLocation, @Result);
end;

procedure TVKShaderUniform.PopProgram;
begin
  CurrentGLContext.GLStates.CurrentProgram := FStoreProgram;
end;

procedure TVKShaderUniform.PushProgram;
begin
  with CurrentGLContext.GLStates do
  begin
    FStoreProgram := CurrentProgram;
    CurrentProgram := GetProgram;
  end;
end;

procedure TVKShaderUniform.ReadFromFiler(AReader: TReader);
begin
  with AReader do
  begin
    FName := ReadString;
    FNameHashCode := ComputeNameHashKey(FName);
    FType := TVKSLDataType(ReadInteger);
    FSamplerType := TVKSLSamplerType(ReadInteger);
    SetAutoSetMethod(ReadString);
  end;
end;

procedure TVKShaderUniform.SetFloat(const Value: TGLfloat);
begin
  PushProgram;
  GL.Uniform1f(FLocation, Value);
  PopProgram;
end;

procedure TVKShaderUniform.SetFloatArray(const Values: PGLFloat;
  Count: Integer);
begin
  PushProgram;
  GL.Uniform1fv(FLocation, Count, Values);
  PopProgram;
end;

procedure TVKShaderUniform.SetInt(const Value: Integer);
begin
  PushProgram;
  GL.Uniform1i(FLocation, Value);
  PopProgram;
end;

procedure TVKShaderUniform.SetIntArray(const Values: PGLInt; Count: Integer);
begin
  PushProgram;
  GL.Uniform1iv(FLocation, Count, Values);
  PopProgram;
end;

procedure TVKShaderUniform.SetIVec2(const Value: TVector2i);
begin
  PushProgram;
  GL.Uniform2i(FLocation, Value.V[0], Value.V[1]);
  PopProgram;
end;

procedure TVKShaderUniform.SetIVec3(const Value: TVector3i);
begin
  PushProgram;
  GL.Uniform3i(FLocation, Value.V[0], Value.V[1], Value.V[2]);
  PopProgram;
end;

procedure TVKShaderUniform.SetIVec4(const Value: TVector4i);
begin
  PushProgram;
  GL.Uniform4i(FLocation, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  PopProgram;
end;

procedure TVKShaderUniform.SetMat2(const Value: TMatrix2f);
begin
  PushProgram;
  GL.UniformMatrix2fv(FLocation, 1, False, @Value);
  PopProgram;
end;

procedure TVKShaderUniform.SetMat3(const Value: TMatrix3f);
begin
  PushProgram;
  GL.UniformMatrix2fv(FLocation, 1, False, @Value);
  PopProgram;
end;

procedure TVKShaderUniform.SetMat4(const Value: TMatrix4f);
begin
  PushProgram;
  GL.UniformMatrix4fv(FLocation, 1, False, @Value);
  PopProgram;
end;

procedure TVKShaderUniform.SetAutoSetMethod(const AValue: string);
begin
  FAutoSet := GetUniformAutoSetMethod(AValue);
end;

procedure TVKShaderUniform.SetUInt(const Value: GLuint);
begin
  PushProgram;
  GL.Uniform1ui(FLocation, Value);
  PopProgram;
end;

procedure TVKShaderUniform.SetUIntArray(const Values: PGLUInt; Count: Integer);
begin
  PushProgram;
  GL.Uniform1uiv(FLocation, Count, Values);
  PopProgram;
end;

procedure TVKShaderUniform.SetUVec2(const Value: TVector2ui);
begin
  PushProgram;
  GL.Uniform2ui(FLocation, Value.V[0], Value.V[1]);
  PopProgram;
end;

procedure TVKShaderUniform.SetUVec3(const Value: TVector3ui);
begin
  PushProgram;
  GL.Uniform3ui(FLocation, Value.V[0], Value.V[1], Value.V[2]);
  PopProgram;
end;

procedure TVKShaderUniform.SetUVec4(const Value: TVector4ui);
begin
  PushProgram;
  GL.Uniform4ui(FLocation, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  PopProgram;
end;

procedure TVKShaderUniform.SetVec2(const Value: TVector2f);
begin
  PushProgram;
  GL.Uniform2f(FLocation, Value.V[0], Value.V[1]);
  PopProgram;
end;

procedure TVKShaderUniform.SetVec3(const Value: TVector3f);
begin
  PushProgram;
  GL.Uniform3f(FLocation, Value.V[0], Value.V[1], Value.V[2]);
  PopProgram;
end;

procedure TVKShaderUniform.SetVec4(const Value: TVector4f);
begin
  PushProgram;
  GL.Uniform4f(FLocation, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  PopProgram;
end;

procedure TVKShaderUniform.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteString(FName);
    WriteInteger(Integer(FType));
    WriteInteger(Integer(FSamplerType));
    WriteString(GetAutoSetMethod);
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKShaderUniformDSA'}{$ENDIF}

procedure TVKShaderUniformDSA.SetFloat(const Value: TGLfloat);
begin
  GL.ProgramUniform1f(GetProgram, FLocation, Value);
end;

procedure TVKShaderUniformDSA.SetFloatArray(const Values: PGLFloat;
  Count: Integer);
begin
  GL.ProgramUniform1fv(GetProgram, FLocation, Count, Values);
end;

procedure TVKShaderUniformDSA.SetInt(const Value: Integer);
begin
  GL.ProgramUniform1i(GetProgram, FLocation, Value);
end;

procedure TVKShaderUniformDSA.SetIntArray(const Values: PGLInt; Count: Integer);
begin
  GL.ProgramUniform1iv(GetProgram, FLocation, Count, Values);
end;

procedure TVKShaderUniformDSA.SetIVec2(const Value: TVector2i);
begin
  GL.ProgramUniform2i(GetProgram, FLocation, Value.V[0], Value.V[1]);
end;

procedure TVKShaderUniformDSA.SetIVec3(const Value: TVector3i);
begin
  GL.ProgramUniform3i(GetProgram, FLocation, Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TVKShaderUniformDSA.SetIVec4(const Value: TVector4i);
begin
  GL.ProgramUniform4i(GetProgram, FLocation, Value.V[0], Value.V[1], Value.V[2],
    Value.V[3]);
end;

procedure TVKShaderUniformDSA.SetMat2(const Value: TMatrix2f);
begin
  GL.ProgramUniformMatrix2fv(GetProgram, FLocation, 1, False, @Value);
end;

procedure TVKShaderUniformDSA.SetMat3(const Value: TMatrix3f);
begin
  GL.ProgramUniformMatrix3fv(GetProgram, FLocation, 1, False, @Value);
end;

procedure TVKShaderUniformDSA.SetMat4(const Value: TMatrix4f);
begin
  GL.ProgramUniformMatrix4fv(GetProgram, FLocation, 1, False, @Value);
end;

procedure TVKShaderUniformDSA.SetUInt(const Value: GLuint);
begin
  GL.ProgramUniform1ui(GetProgram, FLocation, Value);
end;

procedure TVKShaderUniformDSA.SetUIntArray(const Values: PGLUInt;
  Count: Integer);
begin
  GL.ProgramUniform1uiv(GetProgram, FLocation, Count, Values);
end;

procedure TVKShaderUniformDSA.SetUVec2(const Value: TVector2ui);
begin
  GL.ProgramUniform2ui(GetProgram, FLocation, Value.V[0], Value.V[1]);
end;

procedure TVKShaderUniformDSA.SetUVec3(const Value: TVector3ui);
begin
  GL.ProgramUniform3ui(GetProgram, FLocation, Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TVKShaderUniformDSA.SetUVec4(const Value: TVector4ui);
begin
  GL.ProgramUniform4ui(GetProgram, FLocation, Value.V[0], Value.V[1], Value.V[2],
    Value.V[3]);
end;

procedure TVKShaderUniformDSA.SetVec2(const Value: TVector2f);
begin
  GL.ProgramUniform2f(GetProgram, FLocation, Value.V[0], Value.V[1]);
end;

procedure TVKShaderUniformDSA.SetVec3(const Value: TVector3f);
begin
  GL.ProgramUniform3f(GetProgram, FLocation, Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TVKShaderUniformDSA.SetVec4(const Value: TVector4f);
begin
  GL.ProgramUniform4f(GetProgram, FLocation, Value.V[0], Value.V[1], Value.V[2],
    Value.V[3]);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKTextureSwizzling'}{$ENDIF}

procedure TVKTextureSwizzling.Assign(Source: TPersistent);
var
  LSwizzling: TVKTextureSwizzling;
begin
  if Source is TVKTextureSwizzling then
  begin
    LSwizzling := TVKTextureSwizzling(Source);
    FSwizzles[0] := LSwizzling.FSwizzles[0];
    FSwizzles[1] := LSwizzling.FSwizzles[1];
    FSwizzles[2] := LSwizzling.FSwizzles[2];
    FSwizzles[3] := LSwizzling.FSwizzles[3];
  end;
  inherited;
end;

constructor TVKTextureSwizzling.Create(AOwner: TPersistent);
begin
  inherited;
  FSwizzles := cDefaultSwizzleVector;
end;

function TVKTextureSwizzling.GetSwizzle(AIndex: Integer): TVKTextureSwizzle;
begin
  Result := FSwizzles[AIndex];
end;

procedure TVKTextureSwizzling.ReadFromFiler(AReader: TReader);
begin
  with AReader do
  begin
    ReadInteger;
    FSwizzles[0] := TVKTextureSwizzle(ReadInteger);
    FSwizzles[1] := TVKTextureSwizzle(ReadInteger);
    FSwizzles[2] := TVKTextureSwizzle(ReadInteger);
    FSwizzles[3] := TVKTextureSwizzle(ReadInteger);
  end;
end;

procedure TVKTextureSwizzling.SetSwizzle(AIndex: Integer;
  AValue: TVKTextureSwizzle);
begin
  if AValue <> FSwizzles[AIndex] then
  begin
    FSwizzles[AIndex] := AValue;
    NotifyChange(Self);
  end;
end;

function TVKTextureSwizzling.StoreSwizzle(AIndex: Integer): Boolean;
begin
  Result := (FSwizzles[AIndex] <> cDefaultSwizzleVector[AIndex]);
end;

procedure TVKTextureSwizzling.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0);
    WriteInteger(Integer(FSwizzles[0]));
    WriteInteger(Integer(FSwizzles[1]));
    WriteInteger(Integer(FSwizzles[2]));
    WriteInteger(Integer(FSwizzles[3]));
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKFrameBufferAttachment'}{$ENDIF}

procedure TVKFrameBufferAttachment.Apply(var ARci: TRenderContextInfo);
begin
  if FIsValid and not FOnlyWrite then
  begin
    // Just bind
    with ARci.GLStates do
    begin
      ActiveTextureEnabled[FHandle.Target] := True;
      TextureBinding[ActiveTexture, FHandle.Target] := FHandle.Handle;
    end;
  end
  else
    ARci.GLStates.TextureBinding[ARci.GLStates.ActiveTexture, FHandle.Target] :=
      0;
end;

procedure TVKFrameBufferAttachment.Assign(Source: TPersistent);
var
  LAttachment: TVKFrameBufferAttachment;
begin
  if Source is TVKFrameBufferAttachment then
  begin
    LAttachment := TVKFrameBufferAttachment(Source);
    FLayered := LAttachment.Layered;
    FCubeMap := LAttachment.CubeMap;
    FSamples := LAttachment.Samples;
    FOnlyWrite := LAttachment.OnlyWrite;
    FFixedSamplesLocation := LAttachment.FixedSamplesLocation;
    FWidth := LAttachment.InternalWidth;
    FHeight := LAttachment.InternalHeight;
    FDepth := LAttachment.InternalDepth;
    FInternalFormat := LAttachment.InternalFormat;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TVKFrameBufferAttachment.Create(AOwner: TVKXCollection);
begin
  inherited;
  FDefferedInit := False;
  FHandle := TVKTextureHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FRenderBufferHandle := TVKRenderbufferHandle.Create;
  FRenderBufferHandle.OnPrapare := DoOnPrepare;
  FInternalFormat := tfRGBA8;
  FWidth := 256;
  FHeight := 256;
  FDepth := 0;
  FSamples := -1;
  FLayered := False;
  FCubeMap := False;
  FOnlyWrite := False;
  FFixedSamplesLocation := False;
  Name := TVKMatLibComponents(AOwner).MakeUniqueName('Attachment');
end;

destructor TVKFrameBufferAttachment.Destroy;
begin
  FHandle.Destroy;
  FRenderBufferHandle.Destroy;
  inherited;
end;

procedure TVKFrameBufferAttachment.DoOnPrepare(Sender: TVKContext);
var
  LTarget: TVKTextureTarget;
  w, h, d, s, Level, MaxLevel: Integer;
  glTarget, glFormat, glFace: TGLenum;
begin
  if IsDesignTime and FDefferedInit then
    exit;

  FHandle.AllocateHandle;
  FRenderBufferHandle.AllocateHandle;
  if not (FHandle.IsDataNeedUpdate or FRenderBufferHandle.IsDataNeedUpdate) then
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
  if FOnlyWrite and (LTarget = ttTexture2DMultisample)
    and not Sender.GL.EXT_framebuffer_multisample then
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
  s := FSamples;
  if FCubeMap then
  begin
    if w > Integer(Sender.GLStates.MaxCubeTextureSize) then
      w := Sender.GLStates.MaxCubeTextureSize;
    h := w;
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
  if (s > -1) and (s > Integer(Sender.GLStates.MaxSamples)) then
    s := Sender.GLStates.MaxSamples;

  glTarget := DecodeGLTextureTarget(LTarget);

  if (FHandle.Target <> LTarget)
    and (FHandle.Target <> ttNoShape) then
  begin
    FHandle.DestroyHandle;
    FHandle.AllocateHandle;
  end;
  FHandle.Target := LTarget;

  glFormat := InternalFormatToOpenGLFormat(FInternalFormat);

  if FOnlyWrite and ((LTarget = ttTexture2D) or (LTarget =
    ttTexture2DMultisample))
    and FRenderBufferHandle.IsSupported then
  begin
    if LTarget = ttTexture2D then
      FRenderBufferHandle.SetStorage(glFormat, w, h)
    else
      FRenderBufferHandle.SetStorageMultisample(glFormat, s, w, h);
  end
  else
    with Sender do
    begin
      GLStates.ActiveTextureEnabled[FHandle.Target] := True;
      GLStates.TextureBinding[GLStates.ActiveTexture, FHandle.Target] :=
        FHandle.Handle;
      MaxLevel := CalcTextureLevelNumber(LTarget, w, h, d);

      case glTarget of

        GL_TEXTURE_1D:
          for Level := 0 to MaxLevel - 1 do
          begin
            GL.TexImage1D(glTarget, Level, glFormat, w, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;

        GL_TEXTURE_2D:
          for Level := 0 to MaxLevel - 1 do
          begin
            GL.TexImage2D(glTarget, Level, glFormat, w, h, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
            Div2(h);
          end;

        GL_TEXTURE_RECTANGLE:
          begin
            GL.TexImage2D(glTarget, 0, glFormat, w, h, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
          end;

        GL_TEXTURE_3D:
          for Level := 0 to MaxLevel - 1 do
          begin
            GL.TexImage3D(glTarget, Level, glFormat, w, h, d, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
            Div2(h);
            Div2(d);
          end;

        GL_TEXTURE_CUBE_MAP:
          for Level := 0 to MaxLevel - 1 do
          begin
            for glFace := GL_TEXTURE_CUBE_MAP_POSITIVE_X to
              GL_TEXTURE_CUBE_MAP_NEGATIVE_Z do
              GL.TexImage2D(glFace, Level, glFormat, w, w, 0, GL_RGBA,
                GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;

        GL_TEXTURE_1D_ARRAY:
          for Level := 0 to MaxLevel - 1 do
          begin
            GL.TexImage2D(glTarget, Level, glFormat, w, h, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;

        GL_TEXTURE_2D_ARRAY:
          for Level := 0 to MaxLevel - 1 do
          begin
            GL.TexImage3D(glTarget, Level, glFormat, w, h, d, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
            Div2(h);
          end;

        GL_TEXTURE_CUBE_MAP_ARRAY:
          for Level := 0 to MaxLevel - 1 do
          begin
            GL.TexImage3D(glTarget, Level, glFormat, w, w, d, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;
      end; // of case

      GLStates.ActiveTextureEnabled[FHandle.Target] := False;
      FOnlyWrite := False;
    end; // of texture

  if GL.GetError <> GL_NO_ERROR then
  begin
    GL.ClearError;
    GLSLogger.LogErrorFmt('Unable to create attachment "%s"', [Self.Name]);
    exit;
  end
  else
    FIsValid := True;

  FHandle.NotifyDataUpdated;
  FRenderBufferHandle.NotifyDataUpdated;
end;

class function TVKFrameBufferAttachment.FriendlyName: string;
begin
  Result := 'Framebuffer Attachment';
end;

procedure TVKFrameBufferAttachment.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  FRenderBufferHandle.NotifyChangesOfData;
  inherited;
end;

procedure TVKFrameBufferAttachment.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name := ReadString;
      FDefferedInit := ReadBoolean;
      FLayered := ReadBoolean;
      FCubeMap := ReadBoolean;
      FSamples := ReadInteger;
      FOnlyWrite := ReadBoolean;
      FFixedSamplesLocation := ReadBoolean;
      FWidth := ReadInteger;
      FHeight := ReadInteger;
      FDepth := ReadInteger;
      FInternalFormat := TGLInternalFormat(ReadInteger);
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TVKFrameBufferAttachment.SetCubeMap(AValue: Boolean);
begin
  if FCubeMap <> AValue then
  begin
    FCubeMap := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKFrameBufferAttachment.SetDepth(AValue: Integer);
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

procedure TVKFrameBufferAttachment.SetFixedSamplesLocation(AValue: Boolean);
begin
  if FFixedSamplesLocation <> AValue then
  begin
    FFixedSamplesLocation := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKFrameBufferAttachment.SetHeight(AValue: Integer);
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

procedure TVKFrameBufferAttachment.SetInternalFormat(
  const AValue: TGLInternalFormat);
begin
  if FInternalFormat <> AValue then
  begin
    FInternalFormat := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKFrameBufferAttachment.SetLayered(AValue: Boolean);
begin
  if FLayered <> AValue then
  begin
    FLayered := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKFrameBufferAttachment.SetOnlyWrite(AValue: Boolean);
begin
  if FOnlyWrite <> AValue then
  begin
    if AValue
      and ((FDepth > 0) or FLayered or FFixedSamplesLocation or FCubeMap) then
      exit;
    FOnlyWrite := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKFrameBufferAttachment.SetSamples(AValue: Integer);
begin
  if AValue < -1 then
    AValue := -1;
  if FSamples <> AValue then
  begin
    FSamples := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKFrameBufferAttachment.SetWidth(AValue: Integer);
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

procedure TVKFrameBufferAttachment.UnApply(var ARci: TRenderContextInfo);
begin
  ARci.GLStates.ActiveTextureEnabled[FHandle.Target] := False;
end;

procedure TVKFrameBufferAttachment.WriteToFiler(AWriter: TWriter);
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

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TStandartUniformAutoSetExecutor'}{$ENDIF}

constructor TStandartUniformAutoSetExecutor.Create;
begin
  RegisterUniformAutoSetMethod('Camera world position', GLSLType4F,
    SetCameraPosition);
  RegisterUniformAutoSetMethod('LightSource[0] world position', GLSLType4F,
    SetLightSource0Position);
  RegisterUniformAutoSetMethod('World (model) matrix', GLSLTypeMat4F,
    SetModelMatrix);
  RegisterUniformAutoSetMethod('WorldView matrix', GLSLTypeMat4F,
    SetModelViewMatrix);
  RegisterUniformAutoSetMethod('WorldNormal matrix', GLSLTypeMat3F,
    SetNormalModelMatrix);
  RegisterUniformAutoSetMethod('Inverse World matrix', GLSLTypeMat4F,
    SetInvModelMatrix);
  RegisterUniformAutoSetMethod('View matrix', GLSLTypeMat4F, SetViewMatrix);
  RegisterUniformAutoSetMethod('Inverse WorldView matrix', GLSLTypeMat4F,
    SetInvModelViewMatrix);
  RegisterUniformAutoSetMethod('Projection matrix', GLSLTypeMat4F,
    SetProjectionMatrix);
  RegisterUniformAutoSetMethod('ViewProjection matrix', GLSLTypeMat4F,
    SetViewProjectionMatrix);
  RegisterUniformAutoSetMethod('WorldViewProjection matrix', GLSLTypeMat4F,
    SetWorldViewProjectionMatrix);
  RegisterUniformAutoSetMethod('Material front face emission', GLSLType4F,
    SetMaterialFrontEmission);
  RegisterUniformAutoSetMethod('Material front face ambient', GLSLType4F,
    SetMaterialFrontAmbient);
  RegisterUniformAutoSetMethod('Material front face diffuse', GLSLType4F,
    SetMaterialFrontDiffuse);
  RegisterUniformAutoSetMethod('Material front face specular', GLSLType4F,
    SetMaterialFrontSpecular);
  RegisterUniformAutoSetMethod('Material front face shininess', GLSLType1F,
    SetMaterialFrontShininess);
  RegisterUniformAutoSetMethod('Material back face emission', GLSLType4F,
    SetMaterialBackEmission);
  RegisterUniformAutoSetMethod('Material back face ambient', GLSLType4F,
    SetMaterialBackAmbient);
  RegisterUniformAutoSetMethod('Material back face diffuse', GLSLType4F,
    SetMaterialBackDiffuse);
  RegisterUniformAutoSetMethod('Material back face specular', GLSLType4F,
    SetMaterialBackSpecular);
  RegisterUniformAutoSetMethod('Material back face shininess', GLSLType1F,
    SetMaterialBackShininess)
end;

procedure TStandartUniformAutoSetExecutor.SetCameraPosition(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.cameraPosition;
end;

procedure TStandartUniformAutoSetExecutor.SetInvModelMatrix(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.InvModelMatrix;
end;

procedure TStandartUniformAutoSetExecutor.SetInvModelViewMatrix(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.InvModelViewMatrix;
end;

procedure TStandartUniformAutoSetExecutor.SetLightSource0Position(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.LightPosition[0];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackAmbient(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialAmbient[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackDiffuse(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialDiffuse[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackEmission(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialEmission[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackShininess(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.float := ARci.GLStates.MaterialShininess[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackSpecular(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialSpecular[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontAmbient(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialAmbient[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontDiffuse(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialDiffuse[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontEmission(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialEmission[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontShininess(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.float := ARci.GLStates.MaterialShininess[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontSpecular(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialSpecular[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetModelMatrix(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ModelMatrix;
end;

procedure TStandartUniformAutoSetExecutor.SetModelViewMatrix(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ModelViewMatrix;
end;

procedure TStandartUniformAutoSetExecutor.SetNormalModelMatrix(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat3 := ARci.PipelineTransformation.NormalModelMatrix;
end;

procedure TStandartUniformAutoSetExecutor.SetProjectionMatrix(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ProjectionMatrix;
end;

procedure TStandartUniformAutoSetExecutor.SetViewMatrix(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ViewMatrix;
end;

procedure TStandartUniformAutoSetExecutor.SetViewProjectionMatrix(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ViewProjectionMatrix;
end;

procedure TStandartUniformAutoSetExecutor.SetWorldViewProjectionMatrix(Sender:
  IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := MatrixMultiply(
    ARci.PipelineTransformation.ModelViewMatrix,
    ARci.PipelineTransformation.ProjectionMatrix);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKASMVertexProgram'}{$ENDIF}

procedure TVKASMVertexProgram.Assign(Source: TPersistent);
var
  LProg: TVKASMVertexProgram;
begin
  if Source is TVKASMVertexProgram then
  begin
    LProg := TVKASMVertexProgram(Source);
    FSource.Assign(LProg.FSource);
  end;
  inherited;
end;

constructor TVKASMVertexProgram.Create(AOwner: TVKXCollection);
begin
  inherited;
  FHandle := TVKARBVertexProgramHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FSource := TStringList.Create;
  FSource.OnChange := NotifyChange;
  Name := TVKMatLibComponents(AOwner).MakeUniqueName('VertexProg');
end;

destructor TVKASMVertexProgram.Destroy;
begin
  FHandle.Destroy;
  FSource.Destroy;
  inherited;
end;

procedure TVKASMVertexProgram.DoOnPrepare(Sender: TVKContext);
begin
  if FDefferedInit and not IsDesignTime then
    exit;
  try
    if FHandle.IsSupported then
    begin
      FHandle.AllocateHandle;
      if FHandle.IsDataNeedUpdate then
      begin
        SetExeDirectory;
        if (Length(FSourceFile) > 0) and FileStreamExists(FSourceFile) then
          FSource.LoadFromFile(FSourceFile);
        if FSource.Count > 0 then
        begin
          FHandle.LoadARBProgram(FSource.Text);
          FIsValid := FHandle.Ready;
          if IsDesignTime then
          begin
            FInfoLog := FHandle.InfoLog;
            if (Length(FInfoLog) = 0) and FIsValid then
              FInfoLog := 'Compilation successful';
          end
          else if FIsValid then
            GLSLogger.LogInfoFmt('Program "%s" compilation successful - %s',
              [Name, FHandle.InfoLog])
          else
            GLSLogger.LogErrorFmt('Program "%s" compilation failed - %s',
              [Name, FHandle.InfoLog]);
          FHandle.NotifyDataUpdated;
        end
        else
        begin
          if IsDesignTime then
            FInfoLog := 'No source'
          else
            GLSLogger.LogInfoFmt('Program "%s" has no source code', [Name]);
          FIsValid := False;
        end;
      end;
    end
    else
    begin
      FIsValid := False;
      if IsDesignTime then
        FInfoLog := 'Not supported by hardware';
    end;
  except
    on E: Exception do
    begin
      FIsValid := False;
      if IsDesignTime then
        InformationDlg(E.ClassName + ': ' + E.Message)
      else
        GLSLogger.LogError(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

class function TVKASMVertexProgram.FriendlyName: string;
begin
  Result := 'ASM Vertex Program';
end;

function TVKASMVertexProgram.GetHandle: TVKARBVertexProgramHandle;
begin
  Result := FHandle;
end;

procedure TVKASMVertexProgram.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TVKASMVertexProgram.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name := ReadString;
      FDefferedInit := ReadBoolean;
      FSource.Text := ReadString;
      FSourceFile := ReadString;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TVKASMVertexProgram.SetSource(AValue: TStringList);
begin
  FSource.Assign(AValue);
end;

procedure TVKASMVertexProgram.SetSourceFile(AValue: string);
begin
  FixPathDelimiter(AValue);
  if FSourceFile <> AValue then
  begin
    FSourceFile := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVKASMVertexProgram.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    if Length(FSourceFile) = 0 then
      WriteString(FSource.Text)
    else
      WriteString('');
    WriteString(FSourceFile);
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

initialization

  RegisterClasses(
    [
    TVKTextureImageEx,
      TVKFrameBufferAttachment,
      TVKTextureSampler,
      TVKTextureCombiner,
      TVKShaderEx,
      TVKASMVertexProgram,
      TVKMaterialLibraryEx,
      TVKShaderUniform,
      TVKShaderUniformDSA,
      TVKShaderUniformTexture
      ]);

  RegisterXCollectionItemClass(TVKTextureImageEx);
  RegisterXCollectionItemClass(TVKTextureSampler);
  RegisterXCollectionItemClass(TVKFrameBufferAttachment);
  RegisterXCollectionItemClass(TVKTextureCombiner);
  RegisterXCollectionItemClass(TVKShaderEx);
  RegisterXCollectionItemClass(TVKASMVertexProgram);

  vStandartUniformAutoSetExecutor := TStandartUniformAutoSetExecutor.Create;

finalization

  vStandartUniformAutoSetExecutor.Destroy;

end.

