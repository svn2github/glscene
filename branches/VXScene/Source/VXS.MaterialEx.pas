//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
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

unit VXS.MaterialEx;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  FMX.Dialogs,

  VXS.OpenGLAdapter,
  VXS.XOpenGL,
  VXS.RenderContextInfo,
  VXS.BaseClasses,
  VXS.Context,
  VXS.VectorTypes,
  VXS.Material,
  VXS.Texture,
  VXS.Color,
  VXS.Coordinates,
  VXS.VectorGeometry,
  VXS.Graphics,
  VXS.PersistentClasses,
  VXS.CrossPlatform,
  VXS.State,
  VXS.PipelineTransformation,
  VXS.TextureFormat,
  VXS.XCollection,
  VXS.TextureCombiners,
  VXS.GLSLParameter,
  VXS.ApplicationFileIO,
  VXS.Strings,
  VXS.ImageUtils,
  VXS.Utils;

type

  TVXMaterialComponentName = string;
  TVXMaterialLibraryEx = class;
  TVXMatLibComponents = class;
  TVXLibMaterialEx = class;
  TVXBaseShaderModel = class;
  TVXASMVertexProgram = class;

  TOnAsmProgSetting = procedure(Sender: TVXASMVertexProgram;
    var ARci: TVXRenderContextInfo) of object;
  TOnUniformInitialize = procedure(Sender: TVXBaseShaderModel) of object;
  TOnUniformSetting = procedure(Sender: TVXBaseShaderModel;
    var ARci: TVXRenderContextInfo) of object;

  TVXBaseMaterialCollectionItem = class(
      TVXXCollectionItem,
      IGLMaterialLibrarySupported)
  private
    FNameHashKey: Integer;
    FUserList: TPersistentObjectList;
    FDefferedInit: Boolean;
    FNotifying: Boolean;
    FIsValid: Boolean;
    function GetUserList: TPersistentObjectList;
    function GetMaterialLibraryEx: TVXMaterialLibraryEx;
  protected
    procedure SetName(const AValue: TVXMaterialComponentName); override;
    procedure NotifyChange(Sender: TObject); virtual;
    property UserList: TPersistentObjectList read GetUserList;
    procedure DoOnPrepare(Sender: TVXContext); virtual; abstract;
  public
    destructor Destroy; override;
    procedure RegisterUser(AUser: TVXUpdateAbleObject);
    procedure UnregisterUser(AUser: TVXUpdateAbleObject);
    function GetUserCount: Integer;
    function GetMaterialLibrary: TVXAbstractMaterialLibrary;
    property MaterialLibrary: TVXMaterialLibraryEx read GetMaterialLibraryEx;
    property IsValid: Boolean read FIsValid;
  published
    property Name: TVXMaterialComponentName read GetName write SetName;
    { Run-time flag, indicate that resource
       should initialize in case of failure material's level. }
    property DefferedInit: Boolean read FDefferedInit write FDefferedInit
      default False;
  end;

  CGLBaseMaterialCollectionItem = class of TVXBaseMaterialCollectionItem;

  TVXLibMaterialProperty = class(TVXUpdateAbleObject, IGLMaterialLibrarySupported)
  protected
    FEnabled: Boolean;
    FNextPassName: TVXLibMaterialName;
    function GetMaterial: TVXLibMaterialEx;
    function GetMaterialLibraryEx: TVXMaterialLibraryEx;
    procedure SetEnabled(AValue: Boolean); virtual;
    procedure SetNextPass(const AValue: TVXLibMaterialName);
    procedure Loaded; virtual;
    property NextPass: TVXLibMaterialName read FNextPassName write SetNextPass;
  public
    procedure NotifyChange(Sender: TObject); override;
    function GetMaterialLibrary: TVXAbstractMaterialLibrary;
    property MaterialLibrary: TVXMaterialLibraryEx read GetMaterialLibraryEx;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  TVXTextureSampler = class(TVXBaseMaterialCollectionItem)
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    FHandle: TVXSamplerHandle;
    FMinFilter: TVXMinFilter;
    FMagFilter: TVXMagFilter;
    FFilteringQuality: TVXTextureFilteringQuality;
    FLODBias: Integer;
    FLODBiasFract: Single;
    FWrap: array[0..2] of TVXSeparateTextureWrap;
    FBorderColor: TVXColor;
    FCompareMode: TVXTextureCompareMode;
    FCompareFunc: TDepthFunction;
    FDecodeSRGB: Boolean;
    procedure SetMagFilter(AValue: TVXMagFilter);
    procedure SetMinFilter(AValue: TVXMinFilter);
    procedure SetLODBias(AValue: Integer);
    procedure SetFilteringQuality(AValue: TVXTextureFilteringQuality);
    function GetWrap(Index: Integer): TVXSeparateTextureWrap;
    procedure SetWrap(Index: Integer; AValue: TVXSeparateTextureWrap);
    procedure SetBorderColor(const AValue: TVXColor);
    procedure SetCompareMode(AValue: TVXTextureCompareMode);
    procedure SetCompareFunc(AValue: TDepthFunction);
    procedure SetDecodeSRGB(AValue: Boolean);
  public
    constructor Create(AOwner: TVXXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure DoOnPrepare(Sender: TVXContext); override;
    procedure Apply(var ARci: TVXRenderContextInfo);
    procedure UnApply(var ARci: TVXRenderContextInfo);
    class function FriendlyName: string; override;
    property Handle: TVXSamplerHandle read FHandle;
  published
    { Texture magnification filter. }
    property MagFilter: TVXMagFilter read FMagFilter write SetMagFilter
      default maLinear;
    { Texture minification filter. }
    property MinFilter: TVXMinFilter read FMinFilter write SetMinFilter
      default miLinearMipMapLinear;
    property FilteringQuality: TVXTextureFilteringQuality read FFilteringQuality
      write SetFilteringQuality default tfAnisotropic;
    { Texture LOD bias. }
    property LodBias: Integer read FLODBias write SetLODBias default 0;
    { Address mode for the texture. }
    property WrapX: TVXSeparateTextureWrap index 0 read GetWrap write SetWrap
      default twRepeat;
    property WrapY: TVXSeparateTextureWrap index 1 read GetWrap write SetWrap
      default twRepeat;
    property WrapZ: TVXSeparateTextureWrap index 2 read GetWrap write SetWrap
      default twRepeat;
    { Texture border color. }
    property BorderColor: TVXColor read FBorderColor
      write SetBorderColor;
    { Compare mode and function for depth texture. }
    property CompareMode: TVXTextureCompareMode read FCompareMode
      write SetCompareMode default tcmNone;
    property CompareFunc: TDepthFunction read FCompareFunc
      write SetCompareFunc default cfLEqual;
    { Force retrieving the undecoded sRGB data from the
       texture and manipulate that directly. }
    property sRGB_Encode: Boolean read FDecodeSRGB write SetDecodeSRGB
      default True;
  end;

  TVXAbstractTexture = class(TVXBaseMaterialCollectionItem)
  protected
    
    FHandle: TVXTextureHandle;
    FInternalFormat: TVXInternalFormat;
    FWidth: Integer;
    FHeight: Integer;
    FDepth: Integer;
    FSwizzles: TSwizzleVector;
    FApplicableSampler: TVXTextureSampler;
    FLastSampler: TVXTextureSampler;
    function GetTextureTarget: TVXTextureTarget;
    procedure Apply(var ARci: TVXRenderContextInfo); virtual; abstract;
    procedure UnApply(var ARci: TVXRenderContextInfo); virtual; abstract;
  public
    property Handle: TVXTextureHandle read FHandle;
  published
    property Shape: TVXTextureTarget read GetTextureTarget;
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

  TVXTextureImageEx = class(TVXAbstractTexture)
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    FCompression: TVXTextureCompression;
    FImage: TVXBaseImage;
    FImageAlpha: TVXTextureImageAlpha;
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
    procedure SetInternalFormat(const AValue: TVXInternalFormat);
    procedure SetImageAlpha(const AValue: TVXTextureImageAlpha);
    procedure SetImageBrightness(const AValue: Single);
    function StoreBrightness: Boolean;
    procedure SetImageGamma(const AValue: Single);
    function StoreGamma: Boolean;
    procedure SetNormalMapScale(const AValue: Single);
    function StoreNormalMapScale: Boolean;
    procedure SetCompression(const AValue: TVXTextureCompression);
    procedure SetSourceFile(AValue: string);
    procedure SetInternallyStored(const AValue: Boolean);
    procedure SetMipGenMode(const AValue: TMipmapGenerationMode);
    procedure SetUseStreaming(const AValue: Boolean);
    procedure PrepareImage;
    procedure FullTransfer;
    procedure StreamTransfer;
    procedure CalcLODRange(out AFirstLOD, ALastLOD: Integer);
  public
    constructor Create(AOwner: TVXXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure DoOnPrepare(Sender: TVXContext); override;
    procedure Apply(var ARci: TVXRenderContextInfo); override;
    procedure UnApply(var ARci: TVXRenderContextInfo); override;
    class function FriendlyName: string; override;
  published
    // Factual texture properties
    property InternalWidth: Integer read FWidth;
    property InternalHeight: Integer read FHeight;
    property InternalDepth: Integer read FDepth;
    property InternalFormat: TVXInternalFormat read FInternalFormat
      write SetInternalFormat default tfRGBA8;
    { Automatic Image Alpha setting. 
      Allows to control how and if the image's Alpha channel (transparency)
      is computed. }
    property ImageAlpha: TVXTextureImageAlpha read FImageAlpha write
      SetImageAlpha default tiaDefault;
    { Texture brightness correction. 
      This correction is applied upon loading a TVXTextureImage, it's a
      simple saturating scaling applied to the RGB components of
      the 32 bits image, before it is passed to OpenVX, and before
      gamma correction (if any). }
    property ImageBrightness: Single read FImageBrightness write
      SetImageBrightness stored StoreBrightness;
    { Texture gamma correction. 
      The gamma correction is applied upon loading a TVXTextureImage,
      applied to the RGB components of the 32 bits image, before it is
      passed to OpenVX, after brightness correction (if any). }
    property ImageGamma: Single read FImageGamma write SetImageGamma stored
      StoreGamma;
    { Texture compression control. 
      If True the compressed TextureFormat variant (the OpenVX ICD must
      support GL_ARB_texture_compression, or this option is ignored). }
    property Compression: TVXTextureCompression read FCompression write
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

  TVXFrameBufferAttachment = class(TVXAbstractTexture)
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    FRenderBufferHandle: TVXRenderbufferHandle;
    FLayered: Boolean;
    FCubeMap: Boolean;
    FSamples: Integer;
    FOnlyWrite: Boolean;
    FFixedSamplesLocation: Boolean;
    procedure SetWidth(AValue: Integer);
    procedure SetHeight(AValue: Integer);
    procedure SetDepth(AValue: Integer);
    procedure SetInternalFormat(const AValue: TVXInternalFormat);
    procedure SetOnlyWrite(AValue: Boolean);
    procedure SetLayered(AValue: Boolean);
    procedure SetCubeMap(AValue: Boolean);
    procedure SetSamples(AValue: Integer);
    procedure SetFixedSamplesLocation(AValue: Boolean);
  public
    constructor Create(AOwner: TVXXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure DoOnPrepare(Sender: TVXContext); override;
    procedure Apply(var ARci: TVXRenderContextInfo); override;
    procedure UnApply(var ARci: TVXRenderContextInfo); override;
    class function FriendlyName: string; override;
  published
    property InternalWidth: Integer read FWidth
      write SetWidth default 256;
    property InternalHeight: Integer read FHeight
      write SetHeight default 256;
    property InternalDepth: Integer read FDepth
      write SetDepth default 0;
    property InternalFormat: TVXInternalFormat read FInternalFormat
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

    { Swizzle the components of a texture fetches in
        shader or fixed-function pipeline. }
  TVXTextureSwizzling = class(TVXUpdateAbleObject)
  private
    FSwizzles: TSwizzleVector;
    function GetSwizzle(AIndex: Integer): TVXTextureSwizzle;
    procedure SetSwizzle(AIndex: Integer; AValue: TVXTextureSwizzle);
    function StoreSwizzle(AIndex: Integer): Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(AWriter: TWriter);
    procedure ReadFromFiler(AReader: TReader);
  published
    property RedFrom: TVXTextureSwizzle index 0 read GetSwizzle
      write SetSwizzle stored StoreSwizzle;
    property GreenFrom: TVXTextureSwizzle index 1 read GetSwizzle
      write SetSwizzle stored StoreSwizzle;
    property BlueFrom: TVXTextureSwizzle index 2 read GetSwizzle
      write SetSwizzle stored StoreSwizzle;
    property AlphaFrom: TVXTextureSwizzle index 3 read GetSwizzle
      write SetSwizzle stored StoreSwizzle;
  end;

  TVXTextureProperties = class(TVXLibMaterialProperty)
  private
    FLibTextureName: TVXMaterialComponentName;
    FLibSamplerName: TVXMaterialComponentName;
    FLibTexture: TVXAbstractTexture;
    FLibSampler: TVXTextureSampler;
    FTextureOffset, FTextureScale: TVXCoordinates;
    FTextureRotate: Single;
    FTextureMatrixIsIdentity: Boolean;
    FTextureOverride: Boolean;
    FTextureMatrix: TMatrix;
    FMappingMode: TVXTextureMappingMode;
    FEnvColor: TVXColor;
    FMapSCoordinates: TVXCoordinates4;
    FMapTCoordinates: TVXCoordinates4;
    FMapRCoordinates: TVXCoordinates4;
    FMapQCoordinates: TVXCoordinates4;
    FSwizzling: TVXTextureSwizzling;
    function GetLibTextureName: TVXMaterialComponentName;
    function GetLibSamplerName: TVXMaterialComponentName;
    procedure SetLibTextureName(const AValue: TVXMaterialComponentName);
    procedure SetLibSamplerName(const AValue: TVXMaterialComponentName);
    function GetTextureOffset: TVXCoordinates;
    procedure SetTextureOffset(const AValue: TVXCoordinates);
    function StoreTextureOffset: Boolean;
    function GetTextureScale: TVXCoordinates;
    procedure SetTextureScale(const AValue: TVXCoordinates);
    function StoreTextureScale: Boolean;
    procedure SetTextureMatrix(const AValue: TMatrix);
    procedure SetTextureRotate(AValue: Single);
    function StoreTextureRotate: Boolean;
    procedure SetMappingMode(const AValue: TVXTextureMappingMode);
    function GetMappingSCoordinates: TVXCoordinates4;
    procedure SetMappingSCoordinates(const AValue: TVXCoordinates4);
    function StoreMappingSCoordinates: Boolean;
    function GetMappingTCoordinates: TVXCoordinates4;
    procedure SetMappingTCoordinates(const AValue: TVXCoordinates4);
    function StoreMappingTCoordinates: Boolean;
    function GetMappingRCoordinates: TVXCoordinates4;
    procedure SetMappingRCoordinates(const AValue: TVXCoordinates4);
    function StoreMappingRCoordinates: Boolean;
    function GetMappingQCoordinates: TVXCoordinates4;
    procedure SetMappingQCoordinates(const AValue: TVXCoordinates4);
    function StoreMappingQCoordinates: Boolean;
    procedure SetSwizzling(const AValue: TVXTextureSwizzling);
    function StoreSwizzling: Boolean;
    procedure SetEnvColor(const AValue: TVXColor);
    procedure CalculateTextureMatrix;
    procedure ApplyMappingMode;
    procedure UnApplyMappingMode;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;
    function IsValid: Boolean;
    procedure Apply(var ARci: TVXRenderContextInfo);
    procedure UnApply(var ARci: TVXRenderContextInfo);
    property TextureMatrix: TMatrix read FTextureMatrix write SetTextureMatrix;
  published
    property LibTextureName: TVXMaterialComponentName read GetLibTextureName
      write SetLibTextureName;
    property LibSamplerName: TVXMaterialComponentName read GetLibSamplerName
      write SetLibSamplerName;
    property TextureOffset: TVXCoordinates read GetTextureOffset write
      SetTextureOffset stored StoreTextureOffset;
    { Texture coordinates scaling. 
       Scaling is applied before applying the offset, and is applied
       to the texture coordinates, meaning that a scale factor of (2, 2, 2)
       will make your texture look twice <i>smaller</i>. }
    property TextureScale: TVXCoordinates read GetTextureScale write
      SetTextureScale stored StoreTextureScale;
    { Texture coordinates rotating. 
       Rotating is applied after applying offset and scale,
       and rotate ST direction around R axis. }
    property TextureRotate: Single read FTextureRotate write
      SetTextureRotate stored StoreTextureRotate;
    { Texture Environment color. }
    property EnvColor: TVXColor read FEnvColor write SetEnvColor;
    { Texture coordinates mapping mode. 
    This property controls automatic texture coordinates generation. }
    property MappingMode: TVXTextureMappingMode read FMappingMode write
      SetMappingMode default tmmUser;
    { Texture mapping coordinates mode for S, T, R and Q axis. 
    This property stores the coordinates for automatic texture
    coordinates generation. }
    property MappingSCoordinates: TVXCoordinates4 read GetMappingSCoordinates
      write SetMappingSCoordinates stored StoreMappingSCoordinates;
    property MappingTCoordinates: TVXCoordinates4 read GetMappingTCoordinates
      write SetMappingTCoordinates stored StoreMappingTCoordinates;
    property MappingRCoordinates: TVXCoordinates4 read GetMappingRCoordinates
      write SetMappingRCoordinates stored StoreMappingRCoordinates;
    property MappingQCoordinates: TVXCoordinates4 read GetMappingQCoordinates
      write SetMappingQCoordinates stored StoreMappingQCoordinates;
    { Texture color fetching parameters. }
    property Swizzling: TVXTextureSwizzling read FSwizzling write
      SetSwizzling stored StoreSwizzling;
  end;

  TVXFixedFunctionProperties = class(TVXLibMaterialProperty)
  private
    FFrontProperties: TVXFaceProperties;
    FBackProperties: TVXFaceProperties;
    FDepthProperties: TVXDepthProperties;
    FBlendingMode: TBlendingMode;
    FBlendingParams: TVXBlendingParameters;
    FTexProp: TVXTextureProperties;
    FMaterialOptions: TMaterialOptions;
    FFaceCulling: TFaceCulling;
    FPolygonMode: TPolygonMode;
    FTextureMode: TVXTextureMode;
    function GetBackProperties: TVXFaceProperties;
    procedure SetBackProperties(AValues: TVXFaceProperties);
    procedure SetFrontProperties(AValues: TVXFaceProperties);
    procedure SetDepthProperties(AValues: TVXDepthProperties);
    procedure SetBlendingMode(const AValue: TBlendingMode);
    procedure SetMaterialOptions(const AValue: TMaterialOptions);
    procedure SetFaceCulling(const AValue: TFaceCulling);
    procedure SetPolygonMode(AValue: TPolygonMode);
    procedure SetBlendingParams(const AValue: TVXBlendingParameters);
    procedure SetTexProp(AValue: TVXTextureProperties);
    procedure SetTextureMode(AValue: TVXTextureMode);
  public
    
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Apply(var ARci: TVXRenderContextInfo);
    procedure UnApply(var ARci: TVXRenderContextInfo);
    { Returns True if the material is blended.  }
    function Blended: Boolean;
  published
    property MaterialOptions: TMaterialOptions read FMaterialOptions write
      SetMaterialOptions default [];
    property BackProperties: TVXFaceProperties read GetBackProperties write
      SetBackProperties;
    property FrontProperties: TVXFaceProperties read FFrontProperties write
      SetFrontProperties;
    property DepthProperties: TVXDepthProperties read FDepthProperties write
      SetDepthProperties;
    property BlendingMode: TBlendingMode read FBlendingMode write SetBlendingMode
      default bmOpaque;
    property BlendingParams: TVXBlendingParameters read FBlendingParams write
      SetBlendingParams;
    property FaceCulling: TFaceCulling read FFaceCulling write SetFaceCulling
      default fcBufferDefault;
    property PolygonMode: TPolygonMode read FPolygonMode write SetPolygonMode
      default pmFill;
    property Texture: TVXTextureProperties read FTexProp write SetTexProp;
    { Texture application mode. }
    property TextureMode: TVXTextureMode read FTextureMode write SetTextureMode
      default tmDecal;
    { Next pass of FFP. }
    property NextPass;
  end;

  TVXTextureCombiner = class(TVXBaseMaterialCollectionItem)
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    FHandle: TVXVirtualHandle;
    FScript: TStringList;
    FCommandCache: TCombinerCache;
    procedure SetScript(AValue: TStringList);
    procedure DoAllocate(Sender: TVXVirtualHandle; var handle: GLuint);
    procedure DoDeallocate(Sender: TVXVirtualHandle; var handle: GLuint);
  public
    constructor Create(AOwner: TVXXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure DoOnPrepare(Sender: TVXContext); override;
    class function FriendlyName: string; override;
  published
    property Script: TStringList read FScript write SetScript;
  end;

  TVXASMVertexProgram = class(TVXBaseMaterialCollectionItem)
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    FHandle: TVXVertexProgramHandle;
    FSource: TStringList;
    FSourceFile: string;
    FInfoLog: string;
    procedure SetSource(AValue: TStringList);
    procedure SetSourceFile(AValue: string);
    function GetHandle: TVXVertexProgramHandle;
  public
    constructor Create(AOwner: TVXXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoOnPrepare(Sender: TVXContext); override;
    class function FriendlyName: string; override;
    procedure NotifyChange(Sender: TObject); override;
    property Handle: TVXVertexProgramHandle read GetHandle;
  published
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

  TVXMultitexturingProperties = class(TVXLibMaterialProperty)
  private
    FLibCombiner: TVXTextureCombiner;
    FLibAsmProg: TVXASMVertexProgram;
    FLibCombinerName: TVXMaterialComponentName;
    FLibAsmProgName: TVXMaterialComponentName;
    FTexProps: array[0..3] of TVXTextureProperties;
    FTextureMode: TVXTextureMode;
    FLightDir: TLightDir2TexEnvColor;
    FLightSourceIndex: Integer;
    function GetLibCombinerName: string;
    function GetLibAsmProgName: string;
    procedure SetLibCombinerName(const AValue: string);
    procedure SetLibAsmProgName(const AValue: string);
    function GetTexProps(AIndex: Integer): TVXTextureProperties;
    procedure SetTexProps(AIndex: Integer; AValue: TVXTextureProperties);
    procedure SetTextureMode(AValue: TVXTextureMode);
    procedure SetLightSourceIndex(AValue: Integer);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;
    function IsValid: Boolean;
    procedure Apply(var ARci: TVXRenderContextInfo);
    procedure UnApply(var ARci: TVXRenderContextInfo);
  published
    property LibCombinerName: string read GetLibCombinerName
      write SetLibCombinerName;
    property LibAsmProgName: string read GetLibAsmProgName
      write SetLibAsmProgName;
    property Texture0: TVXTextureProperties index 0 read GetTexProps write
      SetTexProps;
    property Texture1: TVXTextureProperties index 1 read GetTexProps write
      SetTexProps;
    property Texture2: TVXTextureProperties index 2 read GetTexProps write
      SetTexProps;
    property Texture3: TVXTextureProperties index 3 read GetTexProps write
      SetTexProps;
    { Texture application mode. }
    property TextureMode: TVXTextureMode read FTextureMode write SetTextureMode
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

  TVXShaderType =
    (
    shtVertex,
    shtControl,
    shtEvaluation,
    shtGeometry,
    shtFragment
    );

  TVXShaderEx = class(TVXBaseMaterialCollectionItem)
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    
    FHandle: array[TVXShaderType] of TVXShaderHandle;
    FSource: TStringList;
    FSourceFile: string;
    FShaderType: TVXShaderType;
    FInfoLog: string;
    FGeometryInput: TVXgsInTypes;
    FGeometryOutput: TVXgsOutTypes;
    FGeometryVerticesOut: GLint;
    procedure SetSource(AValue: TStringList);
    procedure SetSourceFile(AValue: string);
    procedure SetShaderType(AValue: TVXShaderType);
    procedure SetGeometryInput(AValue: TVXgsInTypes);
    procedure SetGeometryOutput(AValue: TVXgsOutTypes);
    procedure SetGeometryVerticesOut(AValue: GLint);
    function GetHandle: TVXShaderHandle;
  public
    constructor Create(AOwner: TVXXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoOnPrepare(Sender: TVXContext); override;
    class function FriendlyName: string; override;
    procedure NotifyChange(Sender: TObject); override;
    property Handle: TVXShaderHandle read GetHandle;
  published
    property Source: TStringList read FSource write SetSource;
    property SourceFile: string read FSourceFile write SetSourceFile;
    property ShaderType: TVXShaderType read FShaderType
      write SetShaderType default shtVertex;
    property InfoLog: string read FInfoLog;
    property GeometryInput: TVXgsInTypes read FGeometryInput
      write SetGeometryInput default gsInPoints;
    property GeometryOutput: TVXgsOutTypes read FGeometryOutput
      write SetGeometryOutput default gsOutPoints;
    property GeometryVerticesOut: GLint read FGeometryVerticesOut
      write SetGeometryVerticesOut default 1;
  end;


  TVXAbstractShaderUniform = class(TVXUpdateAbleObject, IShaderParameter)
  protected
    FName: string;
    FNameHashCode: Integer;
    FType: TVXSLDataType;
    FSamplerType: TVXSLSamplerType;
    function GetName: string;
    function GetGLSLType: TVXSLDataType;
    function GetGLSLSamplerType: TVXSLSamplerType;
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
    function GetInt: GLint; virtual;
    function GetIVec2: TVector2i; virtual;
    function GetIVec3: TVector3i; virtual;
    function GetIVec4: TVector4i; virtual;
    function GetUInt: GLuint; virtual;
    function GetUVec2: TVector2ui; virtual;
    function GetUVec3: TVector3ui; virtual;
    function GetUVec4: TVector4ui; virtual;
    procedure SetFloat(const Value: GLfloat); virtual;
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
    procedure Apply(var ARci: TVXRenderContextInfo); virtual;
  end;

  CGLAbstractShaderUniform = class of TVXAbstractShaderUniform;

  TVXShaderUniform = class(TVXAbstractShaderUniform, IShaderParameter)
  protected
    FLocation: GLint;
    FStoreProgram: GLuint;
    FAutoSet: TUniformAutoSetMethod;
    function GetProgram: GLuint; inline;
    procedure PushProgram; inline;
    procedure PopProgram; inline;
    function GetFloat: Single; override;
    function GetVec2: TVector2f; override;
    function GetVec3: TVector3f; override;
    function GetVec4: TVector; override;
    function GetInt: GLint; override;
    function GetIVec2: TVector2i; override;
    function GetIVec3: TVector3i; override;
    function GetIVec4: TVector4i; override;
    function GetUInt: GLuint; override;
    function GetUVec2: TVector2ui; override;
    function GetUVec3: TVector3ui; override;
    function GetUVec4: TVector4ui; override;
    procedure SetFloat(const Value: GLfloat); override;
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
    procedure SetFloatArray(const Values: PGLFloat; Count: Integer); override;
    procedure SetIntArray(const Values: PGLInt; Count: Integer); override;
    procedure SetUIntArray(const Values: PGLUInt; Count: Integer); override;
    procedure Assign(Source: TPersistent); override;
    procedure Apply(var ARci: TVXRenderContextInfo); override;
    property Name: string read GetName;
    property Location: GLint read FLocation;
    property GLSLType: TVXSLDataType read GetGLSLType;
  end;

  TVXShaderUniformDSA = class(TVXShaderUniform)
  protected
    procedure SetFloat(const Value: GLfloat); override;
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
    procedure SetFloatArray(const Values: PGLFloat; Count: Integer); override;
    procedure SetIntArray(const Values: PGLInt; Count: Integer); override;
    procedure SetUIntArray(const Values: PGLUInt; Count: Integer); override;
  end;

  TVXShaderUniformTexture = class(TVXShaderUniform)
  private
    FLibTexture: TVXAbstractTexture;
    FLibSampler: TVXTextureSampler;
    FTarget: TVXTextureTarget;
    FSwizzling: TSwizzleVector;
  protected
    FLibTexureName: TVXMaterialComponentName;
    FLibSamplerName: TVXMaterialComponentName;
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
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;
    procedure Apply(var ARci: TVXRenderContextInfo); override;
    property LibTextureName: TVXMaterialComponentName read GetTextureName
      write SetTextureName;
    property LibSamplerName: TVXMaterialComponentName read GetSamplerName
      write SetSamplerName;
    property GLSLSampler: TVXSLSamplerType read GetGLSLSamplerType;
    property Swizzling: TSwizzleVector read GetTextureSwizzle write
      SetTextureSwizzle;
  end;


  TVXBaseShaderModel = class(TVXLibMaterialProperty)
  protected
    FHandle: TVXProgramHandle;
    FLibShaderName: array[TVXShaderType] of string;
    FShaders: array[TVXShaderType] of TVXShaderEx;
    FIsValid: Boolean;
    FInfoLog: string;
    FUniforms: TPersistentObjectList;
    FAutoFill: Boolean;
    function GetLibShaderName(AType: TVXShaderType): string;
    procedure SetLibShaderName(AType: TVXShaderType; const AValue: string);
    function GetUniform(const AName: string): IShaderParameter;
    class procedure ReleaseUniforms(AList: TPersistentObjectList);
    property LibVertexShaderName: TVXMaterialComponentName index shtVertex
      read GetLibShaderName write SetLibShaderName;
    property LibFragmentShaderName: TVXMaterialComponentName index shtFragment
      read GetLibShaderName write SetLibShaderName;
    property LibGeometryShaderName: TVXMaterialComponentName index shtGeometry
      read GetLibShaderName write SetLibShaderName;
    property LibTessEvalShaderName: TVXMaterialComponentName index shtEvaluation
      read GetLibShaderName write SetLibShaderName;
    property LibTessControlShaderName: TVXMaterialComponentName index shtControl
      read GetLibShaderName write SetLibShaderName;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadUniforms(AStream: TStream);
    procedure WriteUniforms(AStream: TStream);
    procedure Loaded; override;
    class function IsSupported: Boolean; virtual; abstract;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;
    procedure DoOnPrepare(Sender: TVXContext);
    procedure Apply(var ARci: TVXRenderContextInfo); virtual;
    procedure UnApply(var ARci: TVXRenderContextInfo); virtual;
    procedure GetUniformNames(Proc: TGetStrProc);
    property Handle: TVXProgramHandle read FHandle;
    property IsValid: Boolean read FIsValid;
    property Uniforms[const AName: string]: IShaderParameter read GetUniform;
  published
    // Compilation info log for design time
    property InfoLog: string read FInfoLog;
    // Turn on autofill of uniforms
    property AutoFillOfUniforms: Boolean read FAutoFill
      write FAutoFill stored False;
    property NextPass;
  end;

  TVXShaderModel3 = class(TVXBaseShaderModel)
  public
    class function IsSupported: Boolean; override;
  published
    property LibVertexShaderName;
    property LibFragmentShaderName;
  end;

  TVXShaderModel4 = class(TVXBaseShaderModel)
  public
    class function IsSupported: Boolean; override;
  published
    property LibVertexShaderName;
    property LibGeometryShaderName;
    property LibFragmentShaderName;
  end;

  TVXShaderModel5 = class(TVXBaseShaderModel)
  public
    procedure Apply(var ARci: TVXRenderContextInfo); override;
    procedure UnApply(var ARci: TVXRenderContextInfo); override;
    class function IsSupported: Boolean; override;
  published
    property LibTessControlShaderName;
    property LibTessEvalShaderName;
    property LibVertexShaderName;
    property LibGeometryShaderName;
    property LibFragmentShaderName;
  end;

  TVXLibMaterialEx = class(TVXAbstractLibMaterial)
  private
    FHandle: TVXVirtualHandle;
    FApplicableLevel: TVXMaterialLevel;
    FSelectedLevel: TVXMaterialLevel;
    FFixedFunc: TVXFixedFunctionProperties;
    FMultitexturing: TVXMultitexturingProperties;
    FSM3: TVXShaderModel3;
    FSM4: TVXShaderModel4;
    FSM5: TVXShaderModel5;
    FOnAsmProgSetting: TOnAsmProgSetting;
    FOnSM3UniformInit: TOnUniformInitialize;
    FOnSM3UniformSetting: TOnUniformSetting;
    FOnSM4UniformInit: TOnUniformInitialize;
    FOnSM4UniformSetting: TOnUniformSetting;
    FOnSM5UniformInit: TOnUniformInitialize;
    FOnSM5UniformSetting: TOnUniformSetting;
    FNextPass: TVXLibMaterialEx;
    FStoreAmalgamating: Boolean;
    procedure SetLevel(AValue: TVXMaterialLevel);
    procedure SetFixedFunc(AValue: TVXFixedFunctionProperties);
    procedure SetMultitexturing(AValue: TVXMultitexturingProperties);
    procedure SetSM3(AValue: TVXShaderModel3);
    procedure SetSM4(AValue: TVXShaderModel4);
    procedure SetSM5(AValue: TVXShaderModel5);
    procedure DoAllocate(Sender: TVXVirtualHandle; var handle: GLuint);
    procedure DoDeallocate(Sender: TVXVirtualHandle; var handle: GLuint);
  protected
    procedure Loaded; override;
    procedure RemoveDefferedInit;
    procedure DoOnPrepare(Sender: TVXContext);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure Apply(var ARci: TVXRenderContextInfo); override;
    function UnApply(var ARci: TVXRenderContextInfo): Boolean; override;
    function Blended: Boolean; override;
  published
    property ApplicableLevel: TVXMaterialLevel read FApplicableLevel write SetLevel default mlAuto;
    property SelectedLevel: TVXMaterialLevel read FSelectedLevel;
    property FixedFunction: TVXFixedFunctionProperties read FFixedFunc write SetFixedFunc;
    property Multitexturing: TVXMultitexturingProperties read FMultitexturing write SetMultitexturing;
    property ShaderModel3: TVXShaderModel3 read FSM3 write SetSM3;
    property ShaderModel4: TVXShaderModel4 read FSM4 write SetSM4;
    property ShaderModel5: TVXShaderModel5 read FSM5 write SetSM5;
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


  TVXLibMaterialsEx = class(TVXAbstractLibMaterials)
  protected
    procedure SetItems(AIndex: Integer; const AValue: TVXLibMaterialEx);
    function GetItems(AIndex: Integer): TVXLibMaterialEx;
  public
    constructor Create(AOwner: TComponent);
    function MaterialLibrary: TVXMaterialLibraryEx;
    function IndexOf(const Item: TVXLibMaterialEx): Integer;
    function Add: TVXLibMaterialEx;
    function FindItemID(ID: Integer): TVXLibMaterialEx;
    property Items[index: Integer]: TVXLibMaterialEx read GetItems
    write SetItems; default;
    function GetLibMaterialByName(const AName: TVXLibMaterialName):
      TVXLibMaterialEx;
  end;

  TVXMatLibComponents = class(TVXXCollection)
  protected
    function GetItems(index: Integer): TVXBaseMaterialCollectionItem;
  public
    function GetNamePath: string; override;
    class function ItemsClass: TVXXCollectionItemClass; override;
    property Items[index: Integer]: TVXBaseMaterialCollectionItem
    read GetItems; default;

    function GetItemByName(const AName: TVXMaterialComponentName):
      TVXBaseMaterialCollectionItem;
    function GetTextureByName(const AName: TVXMaterialComponentName):
      TVXAbstractTexture;
    function GetAttachmentByName(const AName: TVXMaterialComponentName):
      TVXFrameBufferAttachment;
    function GetSamplerByName(const AName: TVXMaterialComponentName):
      TVXTextureSampler;
    function GetCombinerByName(const AName: TVXMaterialComponentName):
      TVXTextureCombiner;
    function GetShaderByName(const AName: TVXMaterialComponentName):
      TVXShaderEx;
    function GetAsmProgByName(const AName: TVXMaterialComponentName):
      TVXASMVertexProgram;
    function MakeUniqueName(const AName: TVXMaterialComponentName):
      TVXMaterialComponentName;
  end;

  TVXMaterialLibraryEx = class(TVXAbstractMaterialLibrary)
  private
    FComponents: TVXMatLibComponents;
  protected
    procedure Loaded; override;
    function GetMaterials: TVXLibMaterialsEx;
    procedure SetMaterials(AValue: TVXLibMaterialsEx);
    function StoreMaterials: Boolean;
    procedure SetComponents(AValue: TVXMatLibComponents);

    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteComponents(AStream: TStream);
    procedure ReadComponents(AStream: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetNames(Proc: TGetStrProc;
      AClass: CGLBaseMaterialCollectionItem); overload;
    function AddTexture(const AName: TVXMaterialComponentName):
      TVXTextureImageEx;
    function AddAttachment(const AName: TVXMaterialComponentName):
      TVXFrameBufferAttachment;
    function AddSampler(const AName: TVXMaterialComponentName):
      TVXTextureSampler;
    function AddCombiner(const AName: TVXMaterialComponentName):
      TVXTextureCombiner;
    function AddShader(const AName: TVXMaterialComponentName): TVXShaderEx;
    function AddAsmProg(const AName: TVXMaterialComponentName):
      TVXASMVertexProgram;
    procedure SetLevelForAll(const ALevel: TVXMaterialLevel);
  published
      { The materials collection. }
    property Materials: TVXLibMaterialsEx read GetMaterials write SetMaterials
      stored StoreMaterials;
    property Components: TVXMatLibComponents read FComponents
      write SetComponents;
    property TexturePaths;
  end;

procedure RegisterGLMaterialExNameChangeEvent(AEvent: TNotifyEvent);
procedure DeRegisterGLMaterialExNameChangeEvent(AEvent: TNotifyEvent);

//=================================================================
implementation
//=================================================================

const
  cTextureMagFilter: array[maNearest..maLinear] of Cardinal =
    (GL_NEAREST, GL_LINEAR);
  cTextureMinFilter: array[miNearest..miLinearMipmapLinear] of Cardinal =
    (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_LINEAR);
  cTextureWrapMode: array[twRepeat..twMirrorClampToBorder] of Cardinal =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER,
    GL_MIRRORED_REPEAT, GL_MIRROR_CLAMP_TO_EDGE_ATI,
      GL_MIRROR_CLAMP_TO_BORDER_EXT);
  cTextureCompareMode: array[tcmNone..tcmCompareRtoTexture] of Cardinal =
    (GL_NONE, GL_COMPARE_R_TO_TEXTURE);
  cSamplerToTexture: array[TVXSLSamplerType] of TVXTextureTarget =
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

  cTextureSwizzle: array[TVXTextureSwizzle] of GLEnum =
    (
    GL_RED,
    GL_GREEN,
    GL_BLUE,
    GL_ALPHA,
    GL_ZERO,
    GL_ONE
    );

const
  cTextureMode: array[TVXTextureMode] of GLEnum =
    (GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE, GL_ADD);

const
  cShaderTypeName: array[TVXShaderType] of string =
    ('vertex', 'control', 'evaluation', 'geomtery', 'fragment');

type
  TFriendlyImage = class(TVXBaseImage);

  TStandartUniformAutoSetExecutor = class
  public
    constructor Create;
    procedure SetModelMatrix(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetViewMatrix(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetProjectionMatrix(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetInvModelMatrix(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetModelViewMatrix(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetNormalModelMatrix(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetInvModelViewMatrix(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetViewProjectionMatrix(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetWorldViewProjectionMatrix(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetCameraPosition(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    // Lighting
    procedure SetLightSource0Position(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    // Material
    procedure SetMaterialFrontAmbient(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetMaterialFrontDiffuse(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetMaterialFrontSpecular(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetMaterialFrontEmission(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetMaterialFrontShininess(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetMaterialBackAmbient(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetMaterialBackDiffuse(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetMaterialBackSpecular(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetMaterialBackShininess(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
    procedure SetMaterialBackEmission(Sender: IShaderParameter; var ARci: TVXRenderContextInfo);
  end;

var
  vGLMaterialExNameChangeEvent: TNotifyEvent;
  vStandartUniformAutoSetExecutor: TStandartUniformAutoSetExecutor;
  vStoreBegin: procedure(mode: GLEnum); {$IFDEF MSWINDOWS}stdcall;{$ENDIF}{$IFDEF UNIX}cdecl;{$ENDIF}

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

procedure Div2(var Value: Integer); inline;
begin
  Value := Value div 2;
  if Value = 0 then
    Value := 1;
end;

function CalcTextureLevelNumber(ATarget: TVXTextureTarget; w, h, d: Integer):
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

destructor TVXBaseMaterialCollectionItem.Destroy;
var
  I: Integer;
begin
  if Assigned(FUserList) then
  begin
    FNotifying := True;
    for I := FUserList.Count - 1 downto 0 do
      TVXLibMaterialProperty(FUserList[I]).Notification(Self, opRemove);
    FreeAndNil(FUserList);
  end;
  inherited;
end;

function TVXBaseMaterialCollectionItem.GetMaterialLibrary:
  TVXAbstractMaterialLibrary;
begin
  Result := TVXAbstractMaterialLibrary(TVXMatLibComponents(Owner).Owner);
end;

function TVXBaseMaterialCollectionItem.GetMaterialLibraryEx:
  TVXMaterialLibraryEx;
begin
  Result := TVXMaterialLibraryEx(TVXMatLibComponents(Owner).Owner);
end;

function TVXBaseMaterialCollectionItem.GetUserCount: Integer;
begin
  if Assigned(FUserList) then
    Result := FUserList.Count
  else
    Result := 0;
end;

function TVXBaseMaterialCollectionItem.GetUserList: TPersistentObjectList;
begin
  if FUserList = nil then
  begin
    FUserList := TPersistentObjectList.Create;
    FNotifying := False;
  end;
  Result := FUserList;
end;

procedure TVXBaseMaterialCollectionItem.NotifyChange(Sender: TObject);
var
  I: Integer;
begin
  if FNotifying then
    exit;
  FNotifying := True;
  if GetUserCount > 0 then
    for I := 0 to FUserList.Count - 1 do
      TVXUpdateAbleObject(FUserList[I]).NotifyChange(Self);
  FNotifying := False;
end;

procedure TVXBaseMaterialCollectionItem.RegisterUser(
  AUser: TVXUpdateAbleObject);
begin
  if not FNotifying and (UserList.IndexOf(AUser) < 0) then
    UserList.Add(AUser);
end;

procedure TVXBaseMaterialCollectionItem.UnregisterUser(
  AUser: TVXUpdateAbleObject);
begin
  if not FNotifying then
    UserList.Remove(AUser);
end;

procedure TVXBaseMaterialCollectionItem.SetName(const AValue: string);
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
      if TVXMatLibComponents(Owner).GetItemByName(AValue) <> Self then
        inherited SetName(TVXMatLibComponents(Owner).MakeUniqueName(AValue))
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

procedure TVXFixedFunctionProperties.Apply(var ARci: TVXRenderContextInfo);
begin
  with ARci.VxStates do
  begin
    Disable(stColorMaterial);
    PolygonMode := FPolygonMode;

    // Fixed functionality state
    if True{ not ARci.VXStates.ForwardContext} then
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
            SetAlphaFunction(cfGreater, 0);
          end;
        bmAdditive:
          begin
            Enable(stBlend);
            Enable(stAlphaTest);
            SetBlendFunc(bfSrcAlpha, bfOne);
            SetAlphaFunction(cfGreater, 0);
          end;
        bmAlphaTest50:
          begin
            Disable(stBlend);
            Enable(stAlphaTest);
            SetAlphaFunction(cfGEqual, 0.5);
          end;
        bmAlphaTest100:
          begin
            Disable(stBlend);
            Enable(stAlphaTest);
            SetAlphaFunction(cfGEqual, 1.0);
          end;
        bmModulate:
          begin
            Enable(stBlend);
            Enable(stAlphaTest);
            SetBlendFunc(bfDstColor, bfZero);
            SetAlphaFunction(cfGreater, 0);
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
        ARci.VXStates.ActiveTexture := 0;
        FTexProp.Apply(ARci);
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE,
          cTextureMode[FTextureMode]);
      end;
    end;

  end;
end;

procedure TVXFixedFunctionProperties.Assign(Source: TPersistent);
var
  LFFP: TVXFixedFunctionProperties;
begin
  if Source is TVXFixedFunctionProperties then
  begin
    LFFP := TVXFixedFunctionProperties(Source);
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

function TVXFixedFunctionProperties.Blended: Boolean;
begin
  Result := not (FBlendingMode in [bmOpaque, bmAlphaTest50, bmAlphaTest100, bmCustom]);
end;

constructor TVXFixedFunctionProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FFrontProperties := TVXFaceProperties.Create(Self);
  FFaceCulling := fcBufferDefault;
  FPolygonMode := pmFill;
  FBlendingParams := TVXBlendingParameters.Create(Self);
  FDepthProperties := TVXDepthProperties.Create(Self);
  FTexProp := TVXTextureProperties.Create(Self);
  FTextureMode := tmDecal;
  FEnabled := True;
end;

destructor TVXFixedFunctionProperties.Destroy;
begin
  FFrontProperties.Destroy;
  FBackProperties.Free;
  FDepthProperties.Destroy;
  FBlendingParams.Destroy;
  FTexProp.Destroy;
  inherited;
end;

function TVXFixedFunctionProperties.GetBackProperties: TVXFaceProperties;
begin
  if not Assigned(FBackProperties) then
    FBackProperties := TVXFaceProperties.Create(Self);
  Result := FBackProperties;
end;

procedure TVXFixedFunctionProperties.SetBackProperties(AValues:
  TVXFaceProperties);
begin
  BackProperties.Assign(AValues);
  NotifyChange(Self);
end;

procedure TVXFixedFunctionProperties.SetBlendingMode(const AValue:
  TBlendingMode);
begin
  if AValue <> FBlendingMode then
  begin
    FBlendingMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXFixedFunctionProperties.SetBlendingParams(const AValue:
  TVXBlendingParameters);
begin
  FBlendingParams.Assign(AValue);
  NotifyChange(Self);
end;

procedure TVXFixedFunctionProperties.SetDepthProperties(AValues:
  TVXDepthProperties);
begin
  FDepthProperties.Assign(AValues);
  NotifyChange(Self);
end;

procedure TVXFixedFunctionProperties.SetTexProp(AValue: TVXTextureProperties);
begin
  FTexProp.Assign(AValue);
end;

procedure TVXFixedFunctionProperties.SetTextureMode(AValue: TVXTextureMode);
begin
  if AValue <> FTextureMode then
  begin
    FTextureMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXFixedFunctionProperties.SetFaceCulling(const AValue: TFaceCulling);
begin
  if AValue <> FFaceCulling then
  begin
    FFaceCulling := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXFixedFunctionProperties.SetFrontProperties(AValues:
  TVXFaceProperties);
begin
  FFrontProperties.Assign(AValues);
  NotifyChange(Self);
end;

procedure TVXFixedFunctionProperties.SetMaterialOptions(const AValue:
  TMaterialOptions);
begin
  if AValue <> FMaterialOptions then
  begin
    FMaterialOptions := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXFixedFunctionProperties.SetPolygonMode(AValue: TPolygonMode);
begin
  if AValue <> FPolygonMode then
  begin
    FPolygonMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXFixedFunctionProperties.UnApply(var ARci: TVXRenderContextInfo);
begin
  if FTexProp.Enabled and FTexProp.IsValid then
    FTexProp.UnApply(ARci);
end;

function TVXAbstractTexture.GetTextureTarget: TVXTextureTarget;
begin
  Result := FHandle.Target;
end;

procedure TVXTextureImageEx.Apply(var ARci: TVXRenderContextInfo);
begin
  if FIsValid then
  begin
    // Just bind
    with ARci.VxStates do
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
  else with ARci.VxStates do
    TextureBinding[ActiveTexture, FHandle.Target] := 0;
end;

procedure TVXTextureImageEx.Assign(Source: TPersistent);
var
  LTexture: TVXTextureImageEx;
begin
  if Source is TVXTextureImageEx then
  begin
    LTexture := TVXTextureImageEx(Source);
    FCompression := LTexture.FCompression;
    if Assigned(LTexture.FImage) then
    begin
      if not Assigned(FImage) then
        FImage := TVXImage.Create;
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

constructor TVXTextureImageEx.Create(AOwner: TVXXCollection);
begin
  inherited;
  FDefferedInit := False;
  FHandle := TVXTextureHandle.Create;
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
  Name := TVXMatLibComponents(AOwner).MakeUniqueName('Texture');
end;

destructor TVXTextureImageEx.Destroy;
begin
  FHandle.Destroy;
  FImage.Free;
  inherited;
end;

procedure TVXTextureImageEx.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TVXTextureImageEx.DoOnPrepare(Sender: TVXContext);
var
  LTarget: TVXTextureTarget;
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
      FUseStreaming := FUseStreaming and (TVXUnpackPBOHandle.IsSupported > False) ;
      FUseStreaming := FUseStreaming and IsServiceContextAvaible;
      FUseStreaming := FUseStreaming and (LTarget = ttTexture2D);
    end;

    with Sender.VxStates do
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

    Sender.VXStates.ActiveTextureEnabled[FHandle.Target] := False;

    FApplyCounter := 0;
    FIsValid := True;
  except
    FIsValid := False;
  end;
end;

procedure TVXTextureImageEx.FullTransfer;
var
  LCompression: TVXTextureCompression;
  glFormat: GLEnum;
begin
  begin
    if GL_ARB_texture_compression then
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
      with CurrentVXContext.VxStates do
      begin
        case LCompression of
          tcStandard: TextureCompressionHint := hintDontCare;
          tcHighQuality: TextureCompressionHint := hintNicest;
          tcHighSpeed: TextureCompressionHint := hintFastest;
        else
          Assert(False, strErrorEx + strUnknownType);
        end;
        if not GetGenericCompressedFormat(
          FInternalFormat,
          FImage.ColorFormat,
          glFormat) then
          glFormat := InternalFormatToOpenVXFormat(FInternalFormat);
      end
    else
      glFormat := InternalFormatToOpenVXFormat(FInternalFormat);

    FImage.RegisterAsOpenVXTexture(
      FHandle,
      FMipGenMode = mgmOnFly,
      glFormat,
      FWidth,
      FHeight,
      FDepth);

    if glGetError <> GL_NO_ERROR then
    begin
      ClearOpenGLError;
      CurrentVXContext.VXStates.ActiveTextureEnabled[FHandle.Target] := False;
      ShowMessage(Format('Unable to create texture "%s"', [Self.Name]));
      Abort;
    end
    else
      FHandle.NotifyDataUpdated;
  end;
end;

procedure TVXTextureImageEx.CalcLODRange(out AFirstLOD, ALastLOD: Integer);
var
  I, MaxLODSize, MinLODSize, MaxLODZSize: Integer;
begin
  case FHandle.Target of
    ttTexture3D:
      begin
        MaxLODSize := CurrentVXContext.VXStates.Max3DTextureSize;
        MaxLODZSize := MaxLODSize;
      end;

    ttTextureCube:
      begin
        MaxLODSize := CurrentVXContext.VXStates.MaxCubeTextureSize;
        MaxLODZSize := 0;
      end;

    ttTexture1DArray,
      ttTexture2DArray,
      ttTextureCubeArray,
      ttTexture2DMultisampleArray:
      begin
        MaxLODSize := CurrentVXContext.VXStates.MaxTextureSize;
        MaxLODZSize := CurrentVXContext.VXStates.MaxArrayTextureSize;
      end;

  else
    begin
      MaxLODSize := CurrentVXContext.VXStates.MaxTextureSize;
      MaxLODZSize := 0;
    end;
  end;

  MinLODSize := 1;

  AFirstLOD := 0;

  for I := 0 to High(TVXImagePiramid) do
  begin
    if (FImage.LevelWidth[I] <= MaxLODSize)
      and (FImage.LevelHeight[I] <= MaxLODSize)
      and (FImage.LevelDepth[I] <= MaxLODZSize) then
      break;
    Inc(AFirstLOD);
  end;

  AFirstLOD := MinInteger(AFirstLOD, FImage.LevelCount - 1);
  ALastLOD := AFirstLOD;

  for I := AFirstLOD to High(TVXImagePiramid) do
  begin
    if (FImage.LevelWidth[I] < MinLODSize)
      or (FImage.LevelHeight[I] < MinLODSize) then
      break;
    Inc(ALastLOD);
  end;
  ALastLOD := MinInteger(ALastLOD, FImage.LevelCount - 1);
end;

procedure TVXTextureImageEx.StreamTransfer;
var
  LImage: TFriendlyImage;
  bContinueStreaming: Boolean;
  OldBaseLevel, level: Integer;
  newTime: Double;
  glInternalFormat: GLEnum;
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
  if GL_EXT_direct_state_access then
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
        begin
          LImage.LevelPixelBuffer[Level].AllocateHandle;
          LImage.LevelPixelBuffer[Level].Bind;
          glInternalFormat := InternalFormatToOpenVXFormat(FInternalFormat);
          case transferMethod of
            0: glTexImage2D(GL_TEXTURE_2D, Level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.ColorFormat, FImage.DataType, nil);
            1: glCompressedTexImage2D(GL_TEXTURE_2D, Level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.LevelSizeInByte[Level], nil);
            2: glTextureImage2DEXT(FHandle.Handle, GL_TEXTURE_2D, Level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.ColorFormat, FImage.DataType, nil);
            3: glCompressedTextureImage2DEXT(FHandle.Handle, GL_TEXTURE_2D, Level, glInternalFormat, FImage.LevelWidth[level], FImage.LevelHeight[level], 0, FImage.LevelSizeInByte[Level], nil);
          end;
          LImage.LevelPixelBuffer[Level].UnBind;
          LImage.LevelStreamingState[Level] := ssTransfered;
          ShowMessage(Format('Texture "%s" level %d loaded', [Name, Level]));
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
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, FMaxLevel);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, FBaseLevel);
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
      glSamplerParameterf(FApplicableSampler.Handle.Handle,
        GL_TEXTURE_LOD_BIAS, FLODBias + FLODBiasFract)
    else
      // To refrash texture parameters when sampler object not supported
      FLastSampler := nil;
  end;
end;

class function TVXTextureImageEx.FriendlyName: string;
begin
  Result := 'Texture Image';
end;

procedure TVXTextureImageEx.PrepareImage;
const
  cAlphaProc: array[TVXTextureImageAlpha] of TImageAlphaProc =
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
  BaseImageClass: TVXBaseImageClass;
  LPicture: TVXPicture;
  LGraphic: TVXGraphic;
  LImage: TVXImage;
  level: Integer;
  glColorFormat, glDataType: GLEnum;
  bReadFromSource: Boolean;
  LStream: TStream;
  ptr: PByte;

  procedure ReplaceImageClass;
  begin
    if not (FImage is TVXImage) then
    begin
      LImage := TVXImage.Create;
      LImage.Assign(FImage);
      FImage.Destroy;
      FImage := LImage;
    end
    else
      LImage := TVXImage(FImage);
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
          FImage := TVXImage.Create;
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
            FImage := TVXImage.Create;
            if ApplicationFileIODefined then
            begin
              LGraphic := CreateGraphicFromFile(FSourceFile);
              FImage.Assign(LGraphic);
              LGraphic.Free;
            end
            else
            begin
               { TODO : E2035 Not enough actual parameters }
              (*LPicture := TPicture.Create;*)
              LPicture.Bitmap.LoadFromFile(FSourceFile);
              FImage.Assign(LPicture.Bitmap);
              LPicture.Destroy;
            end;
          end;

          if FInternalFormat <> FImage.InternalFormat then
          begin
            ReplaceImageClass;
            FindCompatibleDataFormat(FInternalFormat, glColorFormat, glDataType);
            TVXImage(FImage).SetColorFormatDataType(glColorFormat, glDataType);
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
  {$Message Hint 'TVXTextureImageEx.HeightToNormalScale not yet implemented' }
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
          FImage := TVXImage.Create;
          FImage.SetErrorImage;
          ShowMessage(Format('Source file of texture "%s" image not found',
            [Self.Name]));
        end;
      end; // if bReadFromSource

    except
      on E: Exception do
      begin
        FImage.Free;
        FImage := TVXImage.Create;
        FImage.SetErrorImage;
        if IsDesignTime then
          InformationDlg(Self.Name + ' - ' + E.ClassName + ': ' + E.Message)
        else
          ShowMessage(Self.Name + ' - ' + E.ClassName + ': ' + E.Message);
      end;
    end;
  end; // of not Assigned
end;

procedure TVXTextureImageEx.ReadFromFiler(AReader: TReader);
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
      FInternalFormat := TVXInternalFormat(ReadInteger);
      FCompression := TVXTextureCompression(ReadInteger);
      FImageAlpha := TVXTextureImageAlpha(ReadInteger);
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

procedure TVXTextureImageEx.SetCompression(const AValue: TVXTextureCompression);
begin
  if AValue <> FCompression then
  begin
    FCompression := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXTextureImageEx.SetImageAlpha(const AValue: TVXTextureImageAlpha);
begin
  if FImageAlpha <> AValue then
  begin
    FImageAlpha := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TVXTextureImageEx.SetImageBrightness(const AValue: Single);
begin
  if FImageBrightness <> AValue then
  begin
    FImageBrightness := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TVXTextureImageEx.SetImageGamma(const AValue: Single);
begin
  if FImageGamma <> AValue then
  begin
    FImageGamma := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TVXTextureImageEx.SetInternalFormat(const AValue: TVXInternalFormat);
begin
  if AValue <> FInternalFormat then
  begin
    FInternalFormat := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TVXTextureImageEx.SetInternallyStored(const AValue: Boolean);
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

procedure TVXTextureImageEx.SetMipGenMode(const AValue: TMipmapGenerationMode);
begin
  if FMipGenMode <> AValue then
  begin
    FMipGenMode := AValue;
    FreeAndNil(FImage);
    NotifyChange(Self);
  end;
end;

procedure TVXTextureImageEx.SetNormalMapScale(const AValue: Single);
begin
  if AValue <> FHeightToNormalScale then
  begin
    FHeightToNormalScale := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXTextureImageEx.SetSourceFile(AValue: string);
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

procedure TVXTextureImageEx.SetUseStreaming(const AValue: Boolean);
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

function TVXTextureImageEx.StoreBrightness: Boolean;
begin
  Result := (FImageBrightness <> 1.0);
end;

function TVXTextureImageEx.StoreGamma: Boolean;
begin
  Result := (FImageGamma <> 1.0);
end;

function TVXTextureImageEx.StoreNormalMapScale: Boolean;
begin
  Result := (FHeightToNormalScale <> cDefaultNormalMapScale);
end;

procedure TVXTextureImageEx.UnApply(var ARci: TVXRenderContextInfo);
begin
  ARci.VXStates.ActiveTextureEnabled[FHandle.Target] := False;
end;

procedure TVXTextureImageEx.WriteToFiler(AWriter: TWriter);
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

procedure TVXTextureSampler.Apply(var ARci: TVXRenderContextInfo);
begin
  if FIsValid then
    ARci.VXStates.SamplerBinding[ARci.VXStates.ActiveTexture] := FHandle.Handle;
end;

procedure TVXTextureSampler.Assign(Source: TPersistent);
var
  LSampler: TVXTextureSampler;
begin
  if Source is TVXTextureSampler then
  begin
    LSampler := TVXTextureSampler(Source);
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

constructor TVXTextureSampler.Create(AOwner: TVXXCollection);
begin
  inherited;
  FDefferedInit := False;
  FHandle := TVXSamplerHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FMagFilter := maLinear;
  FMinFilter := miLinearMipMapLinear;
  FFilteringQuality := tfAnisotropic;
  FLODBias := 0;
  FLODBiasFract := 0;
  FWrap[0] := twRepeat;
  FWrap[1] := twRepeat;
  FWrap[2] := twRepeat;
  FBorderColor := TVXColor.CreateInitialized(Self, clrTransparent);
  FCompareMode := tcmNone;
  FCompareFunc := cfLequal;
  FDecodeSRGB := True;
  Name := TVXMatLibComponents(AOwner).MakeUniqueName('Sampler');
end;

destructor TVXTextureSampler.Destroy;
begin
  FHandle.Destroy;
  FBorderColor.Destroy;
  inherited;
end;

function TVXTextureSampler.GetWrap(Index: Integer): TVXSeparateTextureWrap;
begin
  Result := FWrap[Index];
end;

procedure TVXTextureSampler.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TVXTextureSampler.DoOnPrepare(Sender: TVXContext);
var
  ID: GLuint;
begin
  if IsDesignTime and FDefferedInit then
    exit;
  try
    if FHandle.IsSupported > False then
    begin
      FHandle.AllocateHandle;
      ID := FHandle.Handle;
      if FHandle.IsDataNeedUpdate then
        begin
          glSamplerParameterfv(ID, GL_TEXTURE_BORDER_COLOR,
            FBorderColor.AsAddress);
          glSamplerParameteri(ID, GL_TEXTURE_WRAP_S, cTextureWrapMode[FWrap[0]]);
          glSamplerParameteri(ID, GL_TEXTURE_WRAP_T, cTextureWrapMode[FWrap[1]]);
          glSamplerParameteri(ID, GL_TEXTURE_WRAP_R, cTextureWrapMode[FWrap[2]]);
          glSamplerParameterf(ID, GL_TEXTURE_LOD_BIAS, FLODBias + FLODBiasFract);
          glSamplerParameteri(ID, GL_TEXTURE_MIN_FILTER,
            cTextureMinFilter[FMinFilter]);
          glSamplerParameteri(ID, GL_TEXTURE_MAG_FILTER,
            cTextureMagFilter[FMagFilter]);

          if GL_EXT_texture_filter_anisotropic then
          begin
            if FFilteringQuality = tfAnisotropic then
              glSamplerParameteri(ID, GL_TEXTURE_MAX_ANISOTROPY_EXT,
                CurrentVXContext.VXStates.MaxTextureAnisotropy)
            else
              glSamplerParameteri(ID, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
          end;

          glSamplerParameteri(ID, GL_TEXTURE_COMPARE_MODE,
            cTextureCompareMode[FCompareMode]);
          glSamplerParameteri(ID, GL_TEXTURE_COMPARE_FUNC,
            cGLComparisonFunctionToGLEnum[FCompareFunc]);

          if GL_EXT_texture_sRGB_decode then
          begin
            if FDecodeSRGB then
              glSamplerParameteri(ID, GL_TEXTURE_SRGB_DECODE_EXT, GL_DECODE_EXT)
            else
              glSamplerParameteri(ID, GL_TEXTURE_SRGB_DECODE_EXT,
                GL_SKIP_DECODE_EXT);
          end;
          {$IFDEF USE_OPENGL_DEBUG}
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

class function TVXTextureSampler.FriendlyName: string;
begin
  Result := 'Texture Sampler';
end;

procedure TVXTextureSampler.ReadFromFiler(AReader: TReader);
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
      FMinFilter := TVXMinFilter(ReadInteger);
      FMagFilter := TVXMagFilter(ReadInteger);
      FFilteringQuality := TVXTextureFilteringQuality(ReadInteger);
      FLODBias := ReadInteger;
      FWrap[0] := TVXSeparateTextureWrap(ReadInteger);
      FWrap[1] := TVXSeparateTextureWrap(ReadInteger);
      FWrap[2] := TVXSeparateTextureWrap(ReadInteger);
      Read(FBorderColor.AsAddress^, SizeOf(TColorVector));
      FCompareMode := TVXTextureCompareMode(ReadInteger);
      FCompareFunc := TDepthFunction(ReadInteger);
      FDecodeSRGB := ReadBoolean;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TVXTextureSampler.SetBorderColor(const AValue: TVXColor);
begin
  FBorderColor.Assign(AValue);
  NotifyChange(Self);
end;

procedure TVXTextureSampler.SetCompareFunc(AValue: TDepthFunction);
begin
  if FCompareFunc <> AValue then
  begin
    FCompareFunc := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXTextureSampler.SetCompareMode(AValue: TVXTextureCompareMode);
begin
  if FCompareMode <> AValue then
  begin
    FCompareMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXTextureSampler.SetDecodeSRGB(AValue: Boolean);
begin
  if FDecodeSRGB <> AValue then
  begin
    FDecodeSRGB := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXTextureSampler.SetFilteringQuality(
  AValue: TVXTextureFilteringQuality);
begin
  if FFilteringQuality <> AValue then
  begin
    FFilteringQuality := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXTextureSampler.SetLODBias(AValue: Integer);
begin
  if FLODBias <> AValue then
  begin
    FLODBias := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXTextureSampler.SetMagFilter(AValue: TVXMagFilter);
begin
  if FMagFilter <> AValue then
  begin
    FMagFilter := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXTextureSampler.SetMinFilter(AValue: TVXMinFilter);
begin
  if FMinFilter <> AValue then
  begin
    FMinFilter := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXTextureSampler.SetWrap(Index: Integer;
  AValue: TVXSeparateTextureWrap);
begin
  if FWrap[Index] <> AValue then
  begin
    FWrap[Index] := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXTextureSampler.UnApply(var ARci: TVXRenderContextInfo);
begin
  if FHandle.IsSupported > False then
    with ARci.VxStates do
      SamplerBinding[ActiveTexture] := 0;
end;

procedure TVXTextureSampler.WriteToFiler(AWriter: TWriter);
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

{ TVXTextureCombiner }

procedure TVXTextureCombiner.Assign(Source: TPersistent);
var
  LCombiner: TVXTextureCombiner;
begin
  if Source is TVXTextureCombiner then
  begin
    LCombiner := TVXTextureCombiner(Source);
    FScript.Assign(LCombiner.FScript);
  end;
  inherited;
end;

constructor TVXTextureCombiner.Create(AOwner: TVXXCollection);
begin
  inherited;
  FDefferedInit := False;
  FHandle := TVXVirtualHandle.Create;
  FHandle.OnAllocate := DoAllocate;
  FHandle.OnDestroy := DoDeallocate;
  FHandle.OnPrapare := DoOnPrepare;
  FScript := TStringList.Create;
  FScript.OnChange := NotifyChange;
  FIsValid := True;
  Name := TVXMatLibComponents(AOwner).MakeUniqueName('Combiner');
end;

destructor TVXTextureCombiner.Destroy;
begin
  FHandle.Destroy;
  FScript.Destroy;
  inherited;
end;

procedure TVXTextureCombiner.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TVXTextureCombiner.DoAllocate(Sender: TVXVirtualHandle;
  var handle: GLuint);
begin
  handle := 1;
end;

procedure TVXTextureCombiner.DoDeallocate(Sender: TVXVirtualHandle;
  var handle: GLuint);
begin
  handle := 0;
end;

procedure TVXTextureCombiner.DoOnPrepare(Sender: TVXContext);
begin
  if IsDesignTime and FDefferedInit then
    exit;
  if GL_ARB_multitexture then
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
            ShowMessage(E.ClassName + ': ' + E.Message);
        end;
      end;
      FHandle.NotifyDataUpdated;
    end;
  end
  else
    FIsValid := False;
end;

class function TVXTextureCombiner.FriendlyName: string;
begin
  Result := 'Texture Combiner';
end;

procedure TVXTextureCombiner.ReadFromFiler(AReader: TReader);
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

procedure TVXTextureCombiner.SetScript(AValue: TStringList);
begin
  FScript.Assign(AValue);
  NotifyChange(Self);
end;

procedure TVXTextureCombiner.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
    WriteBoolean(FDefferedInit);
    WriteString(FScript.Text);
  end;
end;

{ TVXLibMaterialEx }

procedure TVXLibMaterialEx.Apply(var ARci: TVXRenderContextInfo);
var
  LevelReady: array[TVXMaterialLevel] of Boolean;
  L, MaxLevel: TVXMaterialLevel;
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

procedure TVXLibMaterialEx.Assign(Source: TPersistent);
var
  LMaterial: TVXLibMaterialEx;
begin
  if Source is TVXLibMaterialEx then
  begin
    LMaterial := TVXLibMaterialEx(Source);
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

function TVXLibMaterialEx.Blended: Boolean;
begin
  Result := FFixedFunc.Blended;
end;

constructor TVXLibMaterialEx.Create(ACollection: TCollection);
begin
  inherited;
  FHandle := TVXVirtualHandle.Create;
  FHandle.OnAllocate := DoAllocate;
  FHandle.OnDestroy := DoDeallocate;
  FHandle.OnPrapare := DoOnPrepare;
  FApplicableLevel := mlAuto;
  FSelectedLevel := mlAuto;
  FFixedFunc := TVXFixedFunctionProperties.Create(Self);
  FMultitexturing := TVXMultitexturingProperties.Create(Self);
  FSM3 := TVXShaderModel3.Create(Self);
  FSM4 := TVXShaderModel4.Create(Self);
  FSM5 := TVXShaderModel5.Create(Self);
end;

type
  TVXFreindlyMaterial = class(TVXMaterial);

destructor TVXLibMaterialEx.Destroy;
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
    if LUser is TVXMaterial then
      TVXFreindlyMaterial(LUser).NotifyLibMaterialDestruction;
  end;
  inherited;
end;

procedure TVXLibMaterialEx.DoAllocate(Sender: TVXVirtualHandle;
  var handle: GLuint);
begin
  handle := 1;
end;

procedure TVXLibMaterialEx.DoDeallocate(Sender: TVXVirtualHandle;
  var handle: GLuint);
begin
  handle := 0;
end;

procedure TVXLibMaterialEx.DoOnPrepare(Sender: TVXContext);
begin
end;

procedure TVXLibMaterialEx.Loaded;
begin
  FFixedFunc.FTexProp.Loaded;
  FMultitexturing.Loaded;
  FSM3.Loaded;
  FSM4.Loaded;
  FSM5.Loaded;
end;

procedure TVXLibMaterialEx.NotifyChange(Sender: TObject);
begin
  inherited;
  FHandle.NotifyChangesOfData;
end;

procedure TVXLibMaterialEx.RemoveDefferedInit;
var
  I: Integer;
  ST: TVXShaderType;
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
    for ST := Low(TVXShaderType) to High(TVXShaderType) do
      if Assigned(FSM3.FShaders[ST]) then
        FSM3.FShaders[ST].FDefferedInit := False;
  end;

  if FSM4.Enabled then
  begin
    for ST := Low(TVXShaderType) to High(TVXShaderType) do
      if Assigned(FSM4.FShaders[ST]) then
        FSM4.FShaders[ST].FDefferedInit := False;
  end;

  if FSM5.Enabled then
  begin
    for ST := Low(TVXShaderType) to High(TVXShaderType) do
      if Assigned(FSM5.FShaders[ST]) then
        FSM5.FShaders[ST].FDefferedInit := False;
  end;

  CurrentVXContext.PrepareHandlesData;
end;

procedure TVXLibMaterialEx.SetMultitexturing(AValue:
  TVXMultitexturingProperties);
begin
  FMultitexturing.Assign(AValue);
end;

procedure TVXLibMaterialEx.SetFixedFunc(AValue: TVXFixedFunctionProperties);
begin
  FFixedFunc.Assign(AValue);
end;

procedure TVXLibMaterialEx.SetLevel(AValue: TVXMaterialLevel);
begin
  if FApplicableLevel <> AValue then
  begin
    FApplicableLevel := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXLibMaterialEx.SetSM3(AValue: TVXShaderModel3);
begin
  FSM3.Assign(AValue);
end;

procedure TVXLibMaterialEx.SetSM4(AValue: TVXShaderModel4);
begin
  FSM4.Assign(AValue);
end;

procedure TVXLibMaterialEx.SetSM5(AValue: TVXShaderModel5);
begin
  FSM5.Assign(AValue);
end;

function TVXLibMaterialEx.UnApply(var ARci: TVXRenderContextInfo): Boolean;

  procedure GetNextPass(AProp: TVXLibMaterialProperty);
  begin
    if Length(AProp.NextPass) > 0 then
      FNextPass :=
        TVXMaterialLibraryEx(GetMaterialLibrary).Materials.GetLibMaterialByName(AProp.NextPass)
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
  ARci.VXStates.ActiveTexture := 0;

  Result := Assigned(FNextPass);
  if Result then
    FNextPass.Apply(ARCi);
end;

{ TVXMultitexturingProperties }

procedure TVXMultitexturingProperties.Apply(var ARci: TVXRenderContextInfo);
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
        ARci.VXStates.ActiveTexture := N;
        FTexProps[N].Apply(ARci);
        if Ord(FLightDir) = N+1 then
        begin
          LDir := ARci.VXStates.LightPosition[FLightSourceIndex];
          LDir := VectorTransform(LDir, ARci.PipelineTransformation.InvModelMatrix^);
          NormalizeVector(LDir);
          glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, @LDir);
        end;
        U := U or (1 shl N);
      end;
    end;

    if Assigned(FLibAsmProg) then
    begin
      FLibAsmProg.Handle.Bind;
      glEnable(GL_VERTEX_PROGRAM_ARB);
      if Assigned(GetMaterial.FOnAsmProgSetting) then
        GetMaterial.FOnAsmProgSetting(Self.FLibAsmProg, ARci);
    end;

    with ARci.VxStates do
    begin
      if Assigned(FLibCombiner) and (Length(FLibCombiner.FCommandCache) > 0)
        then
      begin
        for N := 0 to High(FLibCombiner.FCommandCache) do
        begin
          ActiveTexture := FLibCombiner.FCommandCache[N].ActiveUnit;
          glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);
          glTexEnvi(GL_TEXTURE_ENV,
            FLibCombiner.FCommandCache[N].Arg1,
            FLibCombiner.FCommandCache[N].Arg2);
        end;
      end;
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
      ActiveTexture := 0;

    end;

    XGL.BeginUpdate;
    if U > 3 then
      xgl.MapTexCoordToArbitrary(U)
    else if (FTexProps[0].Enabled)
      and (FTexProps[0].MappingMode = tmmUser) then
      if FTexProps[1].MappingMode = tmmUser then
        xgl.MapTexCoordToDual
      else
        xgl.MapTexCoordToMain
    else if FTexProps[1].MappingMode = tmmUser then
      xgl.MapTexCoordToSecond
    else
      xgl.MapTexCoordToMain;
    XGL.EndUpdate;

  end;
end;

constructor TVXMultitexturingProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FEnabled := False;
  FTextureMode := tmDecal;
  FLightDir := l2eNone;
  FLightSourceIndex := 0;
end;

destructor TVXMultitexturingProperties.Destroy;
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

function TVXMultitexturingProperties.GetLibCombinerName: string;
begin
  if Assigned(FLibCombiner) then
    Result := FLibCombiner.Name
  else
    Result := '';
end;

function TVXMultitexturingProperties.GetLibAsmProgName: string;
begin
  if Assigned(FLibAsmProg) then
    Result := FLibAsmProg.Name
  else
    Result := '';
end;

function TVXMultitexturingProperties.IsValid: Boolean;
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

procedure TVXMultitexturingProperties.Loaded;
var
  I: Integer;
begin
  SetLibCombinerName(FLibCombinerName);
  SetLibAsmProgName(FLibAsmProgName);
  for I := 0 to High(FTexProps) do
    if Assigned(FTexProps[I]) then
      FTexProps[I].Loaded;
end;

procedure TVXMultitexturingProperties.Notification(Sender: TObject; Operation:
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

procedure TVXMultitexturingProperties.SetLibCombinerName(const AValue: string);
var
  LCombiner: TVXTextureCombiner;
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

procedure TVXMultitexturingProperties.SetLightSourceIndex(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0
  else if AValue > 7 then
    AValue := 7;
  FLightSourceIndex := AValue;
end;

procedure TVXMultitexturingProperties.SetLibAsmProgName(const AValue: string);
var
  LProg: TVXASMVertexProgram;
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

function TVXMultitexturingProperties.GetTexProps(AIndex: Integer):
  TVXTextureProperties;
begin
  if not Assigned(FTexProps[AIndex]) then
    FTexProps[AIndex] := TVXTextureProperties.Create(Self);
  Result := FTexProps[AIndex];
end;

procedure TVXMultitexturingProperties.SetTexProps(AIndex: Integer;
  AValue: TVXTextureProperties);
begin
  FTexProps[AIndex].Assign(AValue);
end;

procedure TVXMultitexturingProperties.SetTextureMode(AValue: TVXTextureMode);
begin
  if AValue <> FTextureMode then
  begin
    FTextureMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXMultitexturingProperties.UnApply(var ARci: TVXRenderContextInfo);
var
  N: Integer;
begin
  for N := 0 to High(FTexProps) do
  begin
    if FTexProps[N].Enabled then
    begin
      ARci.VXStates.ActiveTexture := N;
      FTexProps[N].UnApply(ARci);
    end;
  end;
  ARci.VXStates.ActiveTexture := 0;

  if Assigned(FLibAsmProg) then
    glDisable(GL_VERTEX_PROGRAM_ARB);
end;

{ TVXTextureProperties }

procedure TVXTextureProperties.Apply(var ARci: TVXRenderContextInfo);
var
  glTarget: GLEnum;
begin
  if Assigned(FLibTexture) then
    begin
      FLibTexture.FApplicableSampler := FLibSampler;
      FLibTexture.Apply(ARci);

      // Apply swizzling if possible
      glTarget := DecodeTextureTarget(FLibTexture.Shape);
      if GL_ARB_texture_swizzle or GL_EXT_texture_swizzle then
      begin
        if FSwizzling.FSwizzles[0] <> FLibTexture.FSwizzles[0] then
        begin
          FLibTexture.FSwizzles[0] := FSwizzling.FSwizzles[0];
          glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_R,
            cTextureSwizzle[FSwizzling.FSwizzles[0]]);
        end;
        if FSwizzling.FSwizzles[1] <> FLibTexture.FSwizzles[1] then
        begin
          FLibTexture.FSwizzles[1] := FSwizzling.FSwizzles[1];
          glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_G,
            cTextureSwizzle[FSwizzling.FSwizzles[1]]);
        end;
        if FSwizzling.FSwizzles[2] <> FLibTexture.FSwizzles[2] then
        begin
          FLibTexture.FSwizzles[2] := FSwizzling.FSwizzles[2];
          glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_B,
            cTextureSwizzle[FSwizzling.FSwizzles[2]]);
        end;
        if FSwizzling.FSwizzles[3] <> FLibTexture.FSwizzles[3] then
        begin
          FLibTexture.FSwizzles[3] := FSwizzling.FSwizzles[3];
          glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_A,
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
          glTexParameterfv(glTarget, GL_TEXTURE_BORDER_COLOR,
            FLibSampler.BorderColor.AsAddress);
          glTexParameteri(glTarget, GL_TEXTURE_WRAP_S,
            cTextureWrapMode[FLibSampler.WrapX]);
          glTexParameteri(glTarget, GL_TEXTURE_WRAP_T,
            cTextureWrapMode[FLibSampler.WrapY]);
          glTexParameteri(glTarget, GL_TEXTURE_WRAP_R,
            cTextureWrapMode[FLibSampler.WrapZ]);
          glTexParameterf(glTarget, GL_TEXTURE_LOD_BIAS, FLibSampler.LODBias +
            FLibSampler.FLODBiasFract);
          glTexParameteri(glTarget, GL_TEXTURE_MIN_FILTER,
            cTextureMinFilter[FLibSampler.MinFilter]);
          glTexParameteri(glTarget, GL_TEXTURE_MAG_FILTER,
            cTextureMagFilter[FLibSampler.MagFilter]);

          if GL_EXT_texture_filter_anisotropic then
          begin
            if FLibSampler.FilteringQuality = tfAnisotropic then
              glTexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT,
                CurrentVXContext.VXStates.MaxTextureAnisotropy)
            else
              glTexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
          end;

          glTexParameteri(glTarget, GL_TEXTURE_COMPARE_MODE,
            cTextureCompareMode[FLibSampler.CompareMode]);
          glTexParameteri(glTarget, GL_TEXTURE_COMPARE_FUNC,
            cGLComparisonFunctionToGLEnum[FLibSampler.CompareFunc]);

          if GL_EXT_texture_sRGB_decode then
          begin
            if FLibSampler.sRGB_Encode then
              glTexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT, GL_DECODE_EXT)
            else
              glTexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT,
                GL_SKIP_DECODE_EXT);
          end;

          FLibTexture.FLastSampler := FLibSampler;
        end;
      end;

      if not FTextureMatrixIsIdentity and (MappingMode = tmmUser) then
        ARci.VXStates.SetTextureMatrix(FTextureMatrix);

      if ARci.currentMaterialLevel < mlSM3 then
      begin
        glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
        ApplyMappingMode;
        if ARci.currentMaterialLevel = mlFixedFunction then
          xgl.MapTexCoordToMain;
      end;
    end;
end;

procedure TVXTextureProperties.ApplyMappingMode;
var
  R_Dim: Boolean;
begin
  begin
    R_Dim := GL_ARB_texture_cube_map or GL_EXT_texture3D;

    case MappingMode of

      tmmUser: ; // nothing to do, but checked first (common case)

      tmmObjectLinear:
        begin
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenfv(GL_S, GL_OBJECT_PLANE, @MappingSCoordinates.DirectVector);
          glTexGenfv(GL_T, GL_OBJECT_PLANE, @MappingTCoordinates.DirectVector);
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);

          if R_Dim then
          begin
            glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenfv(GL_R, GL_OBJECT_PLANE, @MappingRCoordinates.DirectVector);
            glTexGenfv(GL_Q, GL_OBJECT_PLANE, @MappingQCoordinates.DirectVector);
            glEnable(GL_TEXTURE_GEN_R);
            glEnable(GL_TEXTURE_GEN_Q);
          end;
        end;

      tmmEyeLinear:
        begin
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
          // specify planes in eye space, not world space
          glMatrixMode(GL_MODELVIEW);
          glPushMatrix;
          glLoadIdentity;
          glTexGenfv(GL_S, GL_EYE_PLANE, @MappingSCoordinates.DirectVector);
          glTexGenfv(GL_T, GL_EYE_PLANE, @MappingTCoordinates.DirectVector);
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
          if R_Dim then
          begin
            glTexGenfv(GL_R, GL_EYE_PLANE, @MappingRCoordinates.DirectVector);
            glTexGenfv(GL_Q, GL_EYE_PLANE, @MappingQCoordinates.DirectVector);
            glEnable(GL_TEXTURE_GEN_R);
            glEnable(GL_TEXTURE_GEN_Q);
          end;
          glPopMatrix;
        end;

      tmmSphere:
        begin
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
        end;

      tmmCubeMapReflection, tmmCubeMapCamera:
        if R_Dim then
        begin
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
          glEnable(GL_TEXTURE_GEN_R);
        end;

      tmmCubeMapNormal, tmmCubeMapLight0:
        if R_Dim then
        begin
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
          glEnable(GL_TEXTURE_GEN_R);
        end;
    end;
  end;
end;

procedure TVXTextureProperties.Assign(Source: TPersistent);
var
  LTexProp: TVXTextureProperties;
begin
  if Source is TVXTextureProperties then
  begin
    LTexProp := TVXTextureProperties(Source);
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

procedure TVXTextureProperties.CalculateTextureMatrix;
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

constructor TVXTextureProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FTextureRotate := 0;
  FMappingMode := tmmUser;
  FTextureMatrix := IdentityHmgMatrix;
  FEnabled := False;
  FSwizzling := TVXTextureSwizzling.Create(Self);
  FEnvColor := TVXColor.CreateInitialized(Self, clrTransparent);
end;

destructor TVXTextureProperties.Destroy;
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

function TVXTextureProperties.GetLibSamplerName: TVXMaterialComponentName;
begin
  if Assigned(FLibSampler) then
    Result := FLibSampler.Name
  else
    Result := '';
end;

function TVXTextureProperties.GetLibTextureName: TVXMaterialComponentName;
begin
  if Assigned(FLibTexture) then
    Result := FLibTexture.Name
  else
    Result := '';
end;

function TVXTextureProperties.GetMappingQCoordinates: TVXCoordinates4;
begin
  if not Assigned(FMapQCoordinates) then
    FMapQCoordinates := TVXCoordinates4.CreateInitialized(Self, WHmgVector,
      csVector);
  Result := FMapQCoordinates;
end;

function TVXTextureProperties.GetMappingRCoordinates: TVXCoordinates4;
begin
  if not Assigned(FMapRCoordinates) then
    FMapRCoordinates := TVXCoordinates4.CreateInitialized(Self, ZHmgVector,
      csVector);
  Result := FMapRCoordinates;
end;

function TVXTextureProperties.GetMappingSCoordinates: TVXCoordinates4;
begin
  if not Assigned(FMapSCoordinates) then
    FMapSCoordinates := TVXCoordinates4.CreateInitialized(Self, XHmgVector,
      csVector);
  Result := FMapSCoordinates;
end;

function TVXTextureProperties.GetMappingTCoordinates: TVXCoordinates4;
begin
  if not Assigned(FMapTCoordinates) then
    FMapTCoordinates := TVXCoordinates4.CreateInitialized(Self, YHmgVector,
      csVector);
  Result := FMapTCoordinates;
end;

function TVXTextureProperties.GetTextureOffset: TVXCoordinates;
begin
  if not Assigned(FTextureOffset) then
    FTextureOffset :=
      TVXCoordinates3.CreateInitialized(Self, NullHmgVector, csPoint);
  Result := FTextureOffset;
end;

function TVXTextureProperties.GetTextureScale: TVXCoordinates;
begin
  if not Assigned(FTextureScale) then
    FTextureScale :=
      TVXCoordinates3.CreateInitialized(Self, VectorMake(1, 1, 1, 1), csVector);
  Result := FTextureScale;
end;

function TVXTextureProperties.IsValid: Boolean;
begin
  if Assigned(FLibTexture) then
    Result := FLibTexture.IsValid
  else
    Result := False;
end;

procedure TVXTextureProperties.Loaded;
begin
  SetLibTextureName(FLibTextureName);
  SetLibSamplerName(FLibSamplerName);
  CalculateTextureMatrix;
end;

procedure TVXTextureProperties.Notification(Sender: TObject;
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

procedure TVXTextureProperties.NotifyChange(Sender: TObject);
begin
  inherited;
  if (Sender = FTextureOffset) or (Sender = FTextureScale) then
    CalculateTextureMatrix;
  if (Sender = FLibSampler) and Assigned(FLibTexture) then
    FLibTexture.FLastSampler := nil;
end;

procedure TVXTextureProperties.SetLibSamplerName(const AValue:
  TVXMaterialComponentName);
var
  LSampler: TVXTextureSampler;
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

procedure TVXTextureProperties.SetLibTextureName(const AValue:
  TVXMaterialComponentName);
var
  LTexture: TVXAbstractTexture;
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
    if LTexture is TVXFrameBufferAttachment then
    begin
      if TVXFrameBufferAttachment(LTexture).OnlyWrite then
      begin
        if IsDesignTime then
          InformationDlg('Can not use write only attachment as texture')
        else
          ShowMessage(Format('Attempt to use write only attachment "%s" as texture',
            [LTexture.Name]));
        NotifyChange(Self);
        exit;
      end;
    end;
    LTexture.RegisterUser(Self);
    FLibTexture := LTexture;
  end;
  NotifyChange(Self);
end;

procedure TVXTextureProperties.SetMappingMode(
  const AValue: TVXTextureMappingMode);
begin
  if AValue <> FMappingMode then
  begin
    FMappingMode := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXTextureProperties.SetMappingQCoordinates(
  const AValue: TVXCoordinates4);
begin
  MappingQCoordinates.Assign(AValue);
end;

procedure TVXTextureProperties.SetMappingRCoordinates(
  const AValue: TVXCoordinates4);
begin
  MappingRCoordinates.Assign(AValue);
end;

procedure TVXTextureProperties.SetMappingSCoordinates(
  const AValue: TVXCoordinates4);
begin
  MappingSCoordinates.Assign(AValue);
end;

procedure TVXTextureProperties.SetMappingTCoordinates(
  const AValue: TVXCoordinates4);
begin
  MappingTCoordinates.Assign(AValue);
end;

procedure TVXTextureProperties.SetSwizzling(const AValue: TVXTextureSwizzling);
begin
  FSwizzling.Assign(AValue);
end;

procedure TVXTextureProperties.SetTextureMatrix(const AValue: TMatrix);
begin
  FTextureMatrixIsIdentity := CompareMem(@AValue.X, @IdentityHmgMatrix.X,
    SizeOf(TMatrix));
  FTextureMatrix := AValue;
  FTextureOverride := True;
  NotifyChange(Self);
end;

procedure TVXTextureProperties.SetTextureOffset(const AValue: TVXCoordinates);
begin
  TextureOffset.Assign(AValue);
  CalculateTextureMatrix;
end;

procedure TVXTextureProperties.SetTextureRotate(AValue: Single);
begin
  if AValue <> FTextureRotate then
  begin
    FTextureRotate := AValue;
    CalculateTextureMatrix;
    NotifyChange(Self);
  end;
end;

procedure TVXTextureProperties.SetTextureScale(const AValue: TVXCoordinates);
begin
  TextureScale.Assign(AValue);
  CalculateTextureMatrix;
end;

function TVXTextureProperties.StoreMappingQCoordinates: Boolean;
begin
  if Assigned(FMapQCoordinates) then
    Result := not VectorEquals(FMapQCoordinates.AsVector, WHmgVector)
  else
    Result := false;
end;

function TVXTextureProperties.StoreMappingRCoordinates: Boolean;
begin
  if Assigned(FMapRCoordinates) then
    Result := not VectorEquals(FMapRCoordinates.AsVector, ZHmgVector)
  else
    Result := false;
end;

function TVXTextureProperties.StoreMappingSCoordinates: Boolean;
begin
  if Assigned(FMapSCoordinates) then
    Result := not VectorEquals(FMapSCoordinates.AsVector, XHmgVector)
  else
    Result := false;
end;

function TVXTextureProperties.StoreMappingTCoordinates: Boolean;
begin
  if Assigned(FMapTCoordinates) then
    Result := not VectorEquals(FMapTCoordinates.AsVector, YHmgVector)
  else
    Result := false;
end;

function TVXTextureProperties.StoreSwizzling: Boolean;
begin
  Result := FSwizzling.StoreSwizzle(0);
end;

function TVXTextureProperties.StoreTextureOffset: Boolean;
begin
  Result := Assigned(FTextureOffset);
end;

function TVXTextureProperties.StoreTextureRotate: Boolean;
begin
  Result := Abs(FTextureRotate) > EPSILON;
end;

function TVXTextureProperties.StoreTextureScale: Boolean;
begin
  Result := Assigned(FTextureScale);
end;

procedure TVXTextureProperties.SetEnvColor(const AValue:
  TVXColor);
begin
  FEnvColor.Assign(AValue);
  NotifyChange(Self);
end;

procedure TVXTextureProperties.UnApply(var ARci: TVXRenderContextInfo);
begin
  if Assigned(FLibTexture) then
  begin
    FLibTexture.UnApply(ARci);
    if Assigned(FLibSampler) then
      FLibSampler.UnApply(ARci);

    if ARci.currentMaterialLevel < mlSM3 then
    begin
      if not FTextureMatrixIsIdentity and (MappingMode = tmmUser) then
        ARci.VXStates.SetTextureMatrix(IdentityHmgMatrix);
      UnApplyMappingMode;
    end;
  end;
end;

procedure TVXTextureProperties.UnApplyMappingMode;
begin
  if MappingMode <> tmmUser then
    begin
      glDisable(GL_TEXTURE_GEN_S);
      glDisable(GL_TEXTURE_GEN_T);
      if GL_EXT_texture3D or GL_ARB_texture_cube_map then
      begin
        glDisable(GL_TEXTURE_GEN_R);
        glDisable(GL_TEXTURE_GEN_Q);
      end;
    end;
end;

{ TVXShaderEx }

procedure TVXShaderEx.Assign(Source: TPersistent);
var
  LShader: TVXShaderEx;
begin
  if Source is TVXShaderEx then
  begin
    LShader := TVXShaderEx(Source);
    FSource.Assign(LShader.Source);
    FShaderType := LShader.FShaderType;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TVXShaderEx.Create(AOwner: TVXXCollection);
const
  cShaderClasses: array[TVXShaderType] of TVXShaderHandleClass =
    (
    TVXVertexShaderHandle,
    TVXTessControlShaderHandle,
    TVXTessEvaluationShaderHandle,
    TVXGeometryShaderHandle,
    TVXFragmentShaderHandle
    );
var
  S: TVXShaderType;
begin
  inherited;
  FDefferedInit := False;
  for S := Low(TVXShaderType) to High(TVXShaderType) do
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
  Name := TVXMatLibComponents(AOwner).MakeUniqueName('Shader');
end;

destructor TVXShaderEx.Destroy;
var
  S: TVXShaderType;
begin
  for S := Low(TVXShaderType) to High(TVXShaderType) do
    FHandle[S].Destroy;
  FSource.Destroy;
  inherited;
end;

procedure TVXShaderEx.NotifyChange(Sender: TObject);
var
  S: TVXShaderType;
begin
  for S := Low(TVXShaderType) to High(TVXShaderType) do
    FHandle[S].NotifyChangesOfData;

  if (Sender = FSource) and IsDesignTime and (Length(FSourceFile) > 0) then
    FSource.SaveToFile(FSourceFile);

  inherited;
end;

procedure TVXShaderEx.DoOnPrepare(Sender: TVXContext);
begin
  if not IsDesignTime and FDefferedInit then
    exit;
  try
    if FHandle[FShaderType].IsSupported > False then
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
          ShowMessage(Format('Shader "%s" compilation successful - %s',
            [Name, FHandle[FShaderType].InfoLog]))
        else
          ShowMessage(Format('Shader "%s" compilation failed - %s',
            [Name, FHandle[FShaderType].InfoLog]));
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
        ShowMessage(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

class function TVXShaderEx.FriendlyName: string;
begin
  Result := 'GLSL Shader';
end;

function TVXShaderEx.GetHandle: TVXShaderHandle;
begin
  Result := FHandle[FShaderType];
end;

procedure TVXShaderEx.ReadFromFiler(AReader: TReader);
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
      FShaderType := TVXShaderType(ReadInteger);
      FGeometryInput := TVXgsInTypes(ReadInteger);
      FGeometryOutput := TVXgsOutTypes(ReadInteger);
      FGeometryVerticesOut := ReadInteger;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TVXShaderEx.SetGeometryInput(AValue: TVXgsInTypes);
begin
  if AValue <> FGeometryInput then
  begin
    FGeometryInput := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXShaderEx.SetGeometryOutput(AValue: TVXgsOutTypes);
begin
  if AValue <> FGeometryOutput then
  begin
    FGeometryOutput := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXShaderEx.SetGeometryVerticesOut(AValue: GLint);
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

procedure TVXShaderEx.SetShaderType(AValue: TVXShaderType);
begin
  if FShaderType <> AValue then
  begin
    FShaderType := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXShaderEx.SetSource(AValue: TStringList);
begin
  FSource.Assign(AValue);
end;

procedure TVXShaderEx.SetSourceFile(AValue: string);
begin
  FixPathDelimiter(AValue);
  if FSourceFile <> AValue then
  begin
    FSourceFile := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXShaderEx.WriteToFiler(AWriter: TWriter);
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

{ TVXLibMaterialProperty }

function TVXLibMaterialProperty.GetMaterial: TVXLibMaterialEx;
begin
  if Owner is TVXLibMaterialEx then
    Result := TVXLibMaterialEx(Owner)
  else if Owner is TVXLibMaterialProperty then
    Result := TVXLibMaterialProperty(Owner).GetMaterial
  else
    Result := nil;
end;

function TVXLibMaterialProperty.GetMaterialLibrary: TVXAbstractMaterialLibrary;
begin
  if Owner is TVXBaseMaterialCollectionItem then
    Result := TVXBaseMaterialCollectionItem(Owner).GetMaterialLibrary
  else
    Result := GetMaterial.GetMaterialLibrary;
end;

function TVXLibMaterialProperty.GetMaterialLibraryEx: TVXMaterialLibraryEx;
begin
  if Owner is TVXBaseMaterialCollectionItem then
    Result := TVXBaseMaterialCollectionItem(Owner).GetMaterialLibraryEx
  else
    Result := TVXMaterialLibraryEx(GetMaterial.GetMaterialLibrary);
end;

procedure TVXLibMaterialProperty.SetNextPass(const AValue: TVXLibMaterialName);
begin
  if AValue <> FNextPassName then
  begin
    FNextPassName := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXLibMaterialProperty.Loaded;
begin
end;

procedure TVXLibMaterialProperty.NotifyChange(Sender: TObject);
var
  NA: IVKNotifyAble;
begin
  if Assigned(Owner) then
  begin
    if Supports(Owner, IVKNotifyAble, NA) then
      NA.NotifyChange(Self)
  end;
  if Assigned(OnNotifyChange) then
    OnNotifyChange(Self);
end;

procedure TVXLibMaterialProperty.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if Owner is TVXLibMaterialEx then
      GetMaterial.NotifyChange(Self);
  end;
end;


{ TVXLibMaterialsEx }

function TVXLibMaterialsEx.Add: TVXLibMaterialEx;
begin
  Result := (inherited Add) as TVXLibMaterialEx;
end;

constructor TVXLibMaterialsEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TVXLibMaterialEx);
end;

function TVXLibMaterialsEx.FindItemID(ID: Integer): TVXLibMaterialEx;
begin
  Result := (inherited FindItemID(ID)) as TVXLibMaterialEx;
end;

function TVXLibMaterialsEx.GetItems(AIndex: Integer): TVXLibMaterialEx;
begin
  Result := TVXLibMaterialEx(inherited Items[AIndex]);
end;

function TVXLibMaterialsEx.GetLibMaterialByName(
  const AName: string): TVXLibMaterialEx;
var
  LMaterial: TVXAbstractLibMaterial;
begin
  LMaterial := GetMaterial(AName);
  if Assigned(LMaterial) and (LMaterial is TVXLibMaterialEx) then
    Result := TVXLibMaterialEx(LMaterial)
  else
    Result := nil;
end;

function TVXLibMaterialsEx.IndexOf(const Item: TVXLibMaterialEx): Integer;
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

function TVXLibMaterialsEx.MaterialLibrary: TVXMaterialLibraryEx;
begin
  Result := TVXMaterialLibraryEx(GetOwner);
end;

procedure TVXLibMaterialsEx.SetItems(AIndex: Integer;
  const AValue: TVXLibMaterialEx);
begin
  inherited Items[AIndex] := AValue;
end;

{ TVXBaseShaderModel }

procedure TVXBaseShaderModel.Apply(var ARci: TVXRenderContextInfo);
var
  I: Integer;
  LEvent: TOnUniformSetting;
begin
  if FIsValid then
  begin
    FHandle.UseProgramObject;
    if FAutoFill then
      for I := FUniforms.Count - 1 downto 0 do
        TVXAbstractShaderUniform(FUniforms[I]).Apply(ARci);

    if Self is TVXShaderModel3 then
      LEvent := GetMaterial.FOnSM3UniformSetting
    else if Self is TVXShaderModel4 then
      LEvent := GetMaterial.FOnSM4UniformSetting
    else if Self is TVXShaderModel5 then
      LEvent := GetMaterial.FOnSM5UniformSetting
    else
      LEvent := nil;

    if Assigned(LEvent) then
      LEvent(Self, ARci);
  end;
end;

procedure TVXBaseShaderModel.Assign(Source: TPersistent);
var
  SM: TVXBaseShaderModel;
begin
  if Source is TVXBaseShaderModel then
  begin
    SM := TVXBaseShaderModel(Source);
    LibVertexShaderName := SM.LibVertexShaderName;
    LibFragmentShaderName := SM.LibFragmentShaderName;
    LibGeometryShaderName := SM.LibGeometryShaderName;
    LibTessControlShaderName := SM.LibTessControlShaderName;
    LibTessEvalShaderName := SM.LibTessEvalShaderName;
  end;
  inherited;
end;

constructor TVXBaseShaderModel.Create(AOwner: TPersistent);
begin
  inherited;
  FHandle := TVXProgramHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FEnabled := False;
  FUniforms := TPersistentObjectList.Create;
  FAutoFill := True;
end;

procedure TVXBaseShaderModel.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty(
    'Uniforms',
    ReadUniforms,
    WriteUniforms,
    FUniforms.Count > 0);
end;

destructor TVXBaseShaderModel.Destroy;
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

procedure TVXBaseShaderModel.DoOnPrepare(Sender: TVXContext);
var
  T: TVXShaderType;
  LUniforms: TPersistentObjectList;
  LUniform, LUniform2: TVXShaderUniform;
  ID: GLuint;
  I, J, C: Integer;
  buff: array[0..255] of AnsiChar;
  Size: GLint;
  Len: GLsizei;
  Loc: GLint;
  AType: GLenum;
  UName: string;
  GLSLData: TVXSLDataType;
  GLSLSampler: TVXSLSamplerType;
  bSampler: Boolean;
  bNew: Boolean;
  LEvent: TOnUniformInitialize;
begin
  if FEnabled then
    try
      if IsSupported and (FHandle.IsSupported > False) then
      begin
        FHandle.AllocateHandle;
        if FHandle.IsDataNeedUpdate then
        begin
          // Validate shaders
          for T := Low(TVXShaderType) to High(TVXShaderType) do
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
          for T := Low(TVXShaderType) to High(TVXShaderType) do
            if Assigned(FShaders[T]) then
              FHandle.AttachObject(FShaders[T].Handle);
          ID := FHandle.Handle;
          begin
            // Can be override by layouts in shader
            if Assigned(FShaders[shtGeometry]) then
            begin
              glProgramParameteri(ID, GL_GEOMETRY_INPUT_TYPE_EXT,
                cGLgsInTypes[FShaders[shtGeometry].GeometryInput]);
              glProgramParameteri(ID, GL_GEOMETRY_OUTPUT_TYPE_EXT,
                cGLgsOutTypes[FShaders[shtGeometry].GeometryOutput]);
              glProgramParameteri(ID, GL_GEOMETRY_VERTICES_OUT_EXT,
                FShaders[shtGeometry].GeometryVerticesOut);
            end;

            if FHandle.LinkProgram then
            begin

              // Get final values
              if Assigned(FShaders[shtGeometry]) then
              begin
                glGetProgramiv(ID, GL_GEOMETRY_INPUT_TYPE_EXT, @AType);
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
                glGetProgramiv(ID, GL_GEOMETRY_OUTPUT_TYPE_EXT, @AType);
                case AType of
                  GL_POINTS: FShaders[shtGeometry].FGeometryOutput :=
                    gsOutPoints;
                  GL_LINE_STRIP: FShaders[shtGeometry].FGeometryOutput :=
                    gsOutLineStrip;
                  GL_TRIANGLE_STRIP: FShaders[shtGeometry].FGeometryOutput :=
                    sOutTriangleStrip;
                end;
                glGetProgramiv(ID, GL_GEOMETRY_VERTICES_OUT_EXT, @I);
                if I > 0 then
                  FShaders[shtGeometry].FGeometryVerticesOut := I;
                ClearOpenGLError;
              end;

              // Get uniforms
              LUniforms := TPersistentObjectList.Create;

              glGetProgramiv(ID, GL_ACTIVE_UNIFORMS, @C);
              for I := 0 to C - 1 do
              begin
                glGetActiveUniform(
                  ID,
                  GLuint(I),
                  Length(buff),
                  @Len,
                  @Size,
                  @AType,
                  @buff[0]);
                Loc := glGetUniformLocation(ID, @buff[0]);
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
                  ShowMessage(Format('Detected active uniform "%s" with unknown type',
                    [UName]));
                  continue;
                end
                else if GLSLData <> GLSLTypeUndefined then
                begin
                  ShowMessage(Format('Detected active uniform: %s %s',
                    [cGLSLTypeString[GLSLData], UName]));
                end
                else
                begin
                  bSampler := True;
                  ShowMessage(Format('Detected active uniform: %s %s',
                    [cGLSLSamplerString[GLSLSampler], UName]));
                end;

                // Find already existing uniform
                bNew := True;
                for J := 0 to FUniforms.Count - 1 do
                begin
                  if not (FUniforms[J] is TVXShaderUniform) then
                    continue;
                  LUniform := TVXShaderUniform(FUniforms[J]);
                  if not Assigned(LUniform) then
                    continue;
                  if LUniform.Name = UName then
                  begin
                    if bSampler and (LUniform is TVXShaderUniformTexture) then
                    begin
                      if TVXShaderUniformTexture(LUniform).FSamplerType =
                        GLSLSampler then
                      begin
                        LUniform.FLocation := Loc;
                        LUniform.FType := GLSLType1I;
                        TVXShaderUniformTexture(LUniform).FTarget :=
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
                        if (LUniform is TVXShaderUniformDSA)
                          and not GL_EXT_direct_state_access then
                        begin
                          LUniform2 := LUniform;
                          LUniform := TVXShaderUniform.Create(Self);
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
                    LUniform := TVXShaderUniformTexture.Create(Self);
                    LUniform.FType := GLSLType1I;
                    TVXShaderUniformTexture(LUniform).FSamplerType :=
                      GLSLSampler;
                    TVXShaderUniformTexture(LUniform).FTarget :=
                      cSamplerToTexture[GLSLSampler];
                  end
                  else
                  begin
                    if GL_EXT_direct_state_access then
                      LUniform := TVXShaderUniformDSA.Create(Self)
                    else
                      LUniform := TVXShaderUniform.Create(Self);
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

              if Self is TVXShaderModel3 then
                LEvent := GetMaterial.FOnSM3UniformInit
              else if Self is TVXShaderModel4 then
                LEvent := GetMaterial.FOnSM4UniformInit
              else if Self is TVXShaderModel5 then
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
            ShowMessage(Format('Program "%s" link successful - %s',
              [GetMaterial.Name, FHandle.InfoLog]))
          else
            ShowMessage(Format('Program "%s" link failed! - %s',
              [GetMaterial.Name, FHandle.InfoLog]));
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
          ShowMessage(E.ClassName + ': ' + E.Message);
      end;
    end;
end;

procedure TVXBaseShaderModel.Notification(Sender: TObject; Operation:
  TOperation);
var
  st: TVXShaderType;
begin
  if Operation = opRemove then
  begin
    for st := Low(TVXShaderType) to High(TVXShaderType) do
      if FShaders[st] = Sender then
      begin
        FShaders[st] := nil;
        FLibShaderName[st] := '';
        NotifyChange(Self);
        exit;
      end;
  end;
end;

procedure TVXBaseShaderModel.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TVXBaseShaderModel.ReadUniforms(AStream: TStream);
var
  LReader: TReader;
  N, I: Integer;
  str: string;
  LUniform: TVXAbstractShaderUniform;
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

class procedure TVXBaseShaderModel.ReleaseUniforms(
  AList: TPersistentObjectList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    if Assigned(AList[I]) then
      TVXAbstractShaderUniform(AList[I])._Release;
  AList.Destroy;
end;

function TVXBaseShaderModel.GetLibShaderName(AType: TVXShaderType): string;
begin
  if Assigned(FShaders[AType]) then
    Result := FShaders[AType].Name
  else
    Result := '';
end;

function TVXBaseShaderModel.GetUniform(const AName: string): IShaderParameter;
var
  H, I: Integer;
  U: TVXAbstractShaderUniform;
begin
  Result := nil;
  H := ComputeNameHashKey(AName);
  for I := 0 to FUniforms.Count - 1 do
  begin
    U := TVXAbstractShaderUniform(FUniforms[I]);
    if (U.FNameHashCode = H) and (U.FName = AName) then
    begin
      Result := U;
      exit;
    end;
  end;

  if not IsDesignTime then
  begin
    ShowMessage(Format('Attempt to use unknow uniform "%s" for material "%s"',
      [AName, GetMaterial.Name]));
    U := TVXAbstractShaderUniform.Create(Self);
    U._AddRef;
    U.FName := AName;
    U.FNameHashCode := H;
    FUniforms.Add(U);
    Result := U;
  end;
end;

procedure TVXBaseShaderModel.Loaded;
var
  T: TVXShaderType;
  I: Integer;
begin
  for T := Low(TVXShaderType) to High(TVXShaderType) do
    SetLibShaderName(T, FLibShaderName[T]);
  for I := 0 to FUniforms.Count - 1 do
    if FUniforms[I] is TVXShaderUniformTexture then
      TVXShaderUniformTexture(FUniforms[I]).Loaded;
end;

procedure TVXBaseShaderModel.GetUniformNames(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to FUniforms.Count - 1 do
    Proc(TVXAbstractShaderUniform(FUniforms[I]).FName);
end;

procedure TVXBaseShaderModel.SetLibShaderName(AType: TVXShaderType;
  const AValue: string);
var
  LShader: TVXShaderEx;
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

procedure TVXBaseShaderModel.UnApply(var ARci: TVXRenderContextInfo);
begin
  if FIsValid and not ARci.VXStates.ForwardContext then
    FHandle.EndUseProgramObject;
end;

procedure TVXBaseShaderModel.WriteUniforms(AStream: TStream);
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
      TVXAbstractShaderUniform(FUniforms[I]).WriteToFiler(LWriter);
    end;
  finally
    LWriter.Free;
  end;
end;

class function TVXShaderModel3.IsSupported: Boolean;
begin
  Result := GL_ARB_shader_objects;
end;

class function TVXShaderModel4.IsSupported: Boolean;
begin
  Result := GL_EXT_gpu_shader4;
end;

class function TVXShaderModel5.IsSupported: Boolean;
begin
  Result := GL_ARB_gpu_shader5;
end;

procedure BeginPatch(mode: GLEnum);
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if mode = GL_PATCHES then
    vStoreBegin(GL_PATCHES)
  else if (mode = GL_TRIANGLES)
    or (mode = GL_TRIANGLE_STRIP)
    or (mode = GL_TRIANGLE_FAN)
    or (mode = GL_QUADS) then
  begin
    if mode = GL_QUADS then
      glPatchParameteri(GL_PATCH_VERTICES, 4)
    else
      glPatchParameteri(GL_PATCH_VERTICES, 3);
    vStoreBegin(GL_PATCHES);
  end
  else
  begin
    ShowMessage('glBegin called with unsupported primitive for tessellation');
    Abort;
  end;
end;

procedure TVXShaderModel5.Apply(var ARci: TVXRenderContextInfo);
begin
  if Assigned(FShaders[shtControl]) or Assigned(FShaders[shtEvaluation]) then
  begin
    vStoreBegin := glBegin;
    ///? glBegin := BeginPatch;
    ARci.amalgamating := True;
  end;
  inherited;
end;

procedure TVXShaderModel5.UnApply(var ARci: TVXRenderContextInfo);
begin
  inherited;
  if Assigned(FShaders[shtControl]) or Assigned(FShaders[shtEvaluation]) then
    //? glBegin := vStoreBegin;
  ARci.amalgamating := False;
end;


{ TVXMatLibComponents }

function TVXMatLibComponents.GetAttachmentByName(
  const AName: TVXMaterialComponentName): TVXFrameBufferAttachment;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TVXFrameBufferAttachment) and (Items[I].FNameHashKey = N)
      then
    begin
      if Items[I].Name = AName then
      begin
        Result := TVXFrameBufferAttachment(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TVXMatLibComponents.GetCombinerByName(
  const AName: TVXMaterialComponentName): TVXTextureCombiner;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TVXTextureCombiner) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TVXTextureCombiner(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TVXMatLibComponents.GetItemByName(
  const AName: TVXMaterialComponentName): TVXBaseMaterialCollectionItem;
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

function TVXMatLibComponents.GetItems(
  index: Integer): TVXBaseMaterialCollectionItem;
begin
  Result := TVXBaseMaterialCollectionItem(inherited GetItems(index));
end;

function TVXMatLibComponents.GetNamePath: string;
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

function TVXMatLibComponents.GetSamplerByName(
  const AName: TVXMaterialComponentName): TVXTextureSampler;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TVXTextureSampler) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TVXTextureSampler(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TVXMatLibComponents.GetShaderByName(
  const AName: TVXMaterialComponentName): TVXShaderEx;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TVXShaderEx) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TVXShaderEx(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TVXMatLibComponents.GetAsmProgByName(
  const AName: TVXMaterialComponentName): TVXASMVertexProgram;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TVXASMVertexProgram) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TVXASMVertexProgram(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TVXMatLibComponents.GetTextureByName(
  const AName: TVXMaterialComponentName): TVXAbstractTexture;
var
  N, I: Integer;
begin
  N := ComputeNameHashKey(AName);
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TVXAbstractTexture) and (Items[I].FNameHashKey = N) then
    begin
      if Items[I].Name = AName then
      begin
        Result := TVXTextureImageEx(Items[I]);
        exit;
      end;
    end;
  end;
  Result := nil;
end;

class function TVXMatLibComponents.ItemsClass: TVXXCollectionItemClass;
begin
  Result := TVXBaseMaterialCollectionItem;
end;

function TVXMatLibComponents.MakeUniqueName(const AName:
  TVXMaterialComponentName): TVXMaterialComponentName;
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


{ TVXMaterialLibraryEx }

function TVXMaterialLibraryEx.AddAttachment(
  const AName: TVXMaterialComponentName): TVXFrameBufferAttachment;
begin
  Result := TVXFrameBufferAttachment.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TVXMaterialLibraryEx.AddCombiner(
  const AName: TVXMaterialComponentName): TVXTextureCombiner;
begin
  Result := TVXTextureCombiner.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TVXMaterialLibraryEx.AddSampler(
  const AName: TVXMaterialComponentName): TVXTextureSampler;
begin
  Result := TVXTextureSampler.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TVXMaterialLibraryEx.AddShader(
  const AName: TVXMaterialComponentName): TVXShaderEx;
begin
  Result := TVXShaderEx.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TVXMaterialLibraryEx.AddAsmProg(
  const AName: TVXMaterialComponentName): TVXASMVertexProgram;
begin
  Result := TVXASMVertexProgram.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

function TVXMaterialLibraryEx.AddTexture(
  const AName: TVXMaterialComponentName): TVXTextureImageEx;
begin
  Result := TVXTextureImageEx.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

constructor TVXMaterialLibraryEx.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TVXLibMaterialsEx.Create(Self);
  FComponents := TVXMatLibComponents.Create(Self);
end;

procedure TVXMaterialLibraryEx.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty(
    'ComponentsData',
    ReadComponents,
    WriteComponents,
    Components.Count > 0);
  inherited;
end;

destructor TVXMaterialLibraryEx.Destroy;
begin
  FMaterials.Destroy;
  FComponents.Destroy;
  inherited;
end;

function TVXMaterialLibraryEx.GetMaterials: TVXLibMaterialsEx;
begin
  Result := TVXLibMaterialsEx(FMaterials);
end;

procedure TVXMaterialLibraryEx.GetNames(Proc: TGetStrProc;
  AClass: CGLBaseMaterialCollectionItem);
var
  I: Integer;
begin
  for I := 0 to Components.Count - 1 do
    if Components[I].ClassType = AClass then
      Proc(Components[I].Name)
end;

procedure TVXMaterialLibraryEx.Loaded;
begin
  inherited;
end;

procedure TVXMaterialLibraryEx.ReadComponents(AStream: TStream);
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

procedure TVXMaterialLibraryEx.SetComponents(AValue: TVXMatLibComponents);
begin
  FComponents.Assign(AValue);
end;

procedure TVXMaterialLibraryEx.SetLevelForAll(const ALevel: TVXMaterialLevel);
var
  I: Integer;
begin
  for I := Materials.Count - 1 downto 0 do
    Materials[I].ApplicableLevel := ALevel;
end;

procedure TVXMaterialLibraryEx.SetMaterials(AValue: TVXLibMaterialsEx);
begin
  FMaterials.Assign(AValue);
end;

function TVXMaterialLibraryEx.StoreMaterials: Boolean;
begin
  Result := (FMaterials.Count > 0);
end;

procedure TVXMaterialLibraryEx.WriteComponents(AStream: TStream);
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


{ TVXShaderUniformTexture }

procedure TVXShaderUniformTexture.Apply(var ARci: TVXRenderContextInfo);

  function FindHotActiveUnit: Boolean;
  var
    ID: GLuint;
    I, J: Integer;
    bindTime, minTime: Double;
    LTex: TVXTextureImageEx;
  begin
    with ARci.VxStates do
    begin
      if Assigned(FLibTexture) and FLibTexture.IsValid then
      begin
        ID := FLibTexture.FHandle.Handle;
        // Yar: may be need exract this to new method of TVXTextureImageEx ???
        if FLibTexture is TVXTextureImageEx then
        begin
          LTex := TVXTextureImageEx(FLibTexture);
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
          glUniform1i(FLocation, I);
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
          glUniform1i(FLocation, I);
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
      glUniform1i(FLocation, J);
      Result := True;
      exit;
    end;
    Result := False;
  end;

var
  glTarget: GLEnum;
begin
  if FLocation > -1 then
  begin
    if FindHotActiveUnit and Assigned(FLibTexture) and Assigned(FLibSampler)
      then
      begin
        // Apply swizzling if possible
        glTarget := DecodeTextureTarget(FLibTexture.Shape);
        if GL_ARB_texture_swizzle or GL_EXT_texture_swizzle then
        begin

          if FSwizzling[0] <> FLibTexture.FSwizzles[0] then
          begin
            FLibTexture.FSwizzles[0] := FSwizzling[0];
            glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_R,
              cTextureSwizzle[FSwizzling[0]]);
          end;
          if FSwizzling[1] <> FLibTexture.FSwizzles[1] then
          begin
            FLibTexture.FSwizzles[1] := FSwizzling[1];
            glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_G,
              cTextureSwizzle[FSwizzling[1]]);
          end;
          if FSwizzling[2] <> FLibTexture.FSwizzles[2] then
          begin
            FLibTexture.FSwizzles[2] := FSwizzling[2];
            glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_B,
              cTextureSwizzle[FSwizzling[2]]);
          end;
          if FSwizzling[3] <> FLibTexture.FSwizzles[3] then
          begin
            FLibTexture.FSwizzles[3] := FSwizzling[3];
            glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_A,
              cTextureSwizzle[FSwizzling[3]]);
          end;
        end;

        if FLibSampler.IsValid then
          FLibSampler.Apply(ARci)
        else if FLibTexture.FLastSampler <> FLibSampler then
        begin
          // Sampler object not supported, lets use texture states
          glTexParameterfv(glTarget, GL_TEXTURE_BORDER_COLOR,
            FLibSampler.BorderColor.AsAddress);
          glTexParameteri(glTarget, GL_TEXTURE_WRAP_S,
            cTextureWrapMode[FLibSampler.WrapX]);
          glTexParameteri(glTarget, GL_TEXTURE_WRAP_T,
            cTextureWrapMode[FLibSampler.WrapY]);
          glTexParameteri(glTarget, GL_TEXTURE_WRAP_R,
            cTextureWrapMode[FLibSampler.WrapZ]);
          glTexParameterf(glTarget, GL_TEXTURE_LOD_BIAS, FLibSampler.LODBias +
            FLibSampler.FLODBiasFract);
          glTexParameteri(glTarget, GL_TEXTURE_MIN_FILTER,
            cTextureMinFilter[FLibSampler.MinFilter]);
          glTexParameteri(glTarget, GL_TEXTURE_MAG_FILTER,
            cTextureMagFilter[FLibSampler.MagFilter]);

          if GL_EXT_texture_filter_anisotropic then
          begin
            if FLibSampler.FilteringQuality = tfAnisotropic then
              glTexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT,
                CurrentVXContext.VXStates.MaxTextureAnisotropy)
            else
              glTexParameteri(glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
          end;

          glTexParameteri(glTarget, GL_TEXTURE_COMPARE_MODE,
            cTextureCompareMode[FLibSampler.CompareMode]);
          glTexParameteri(glTarget, GL_TEXTURE_COMPARE_FUNC,
            cGLComparisonFunctionToGLEnum[FLibSampler.CompareFunc]);

          if GL_EXT_texture_sRGB_decode then
          begin
            if FLibSampler.sRGB_Encode then
              glTexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT, GL_DECODE_EXT)
            else
              glTexParameteri(glTarget, GL_TEXTURE_SRGB_DECODE_EXT,
                GL_SKIP_DECODE_EXT);
          end;

          FLibTexture.FLastSampler := FLibSampler;
        end;

      end; // with GL
  end;
end;

procedure TVXShaderUniformTexture.Assign(Source: TPersistent);
var
  LUniform: TVXShaderUniformTexture;
begin
  if Source is TVXShaderUniformTexture then
  begin
    LUniform := TVXShaderUniformTexture(Source);
    LibTextureName := LUniform.LibTextureName;
    LibSamplerName := LUniform.LibSamplerName;
  end;
  inherited;
end;

constructor TVXShaderUniformTexture.Create(AOwner: TPersistent);
begin
  inherited;
  FSwizzling := cDefaultSwizzleVector;
end;

destructor TVXShaderUniformTexture.Destroy;
begin
  LibTextureName := '';
  LibSamplerName := '';
  inherited;
end;

function TVXShaderUniformTexture.GetSamplerName: string;
begin
  if Assigned(FLibSampler) then
    Result := FLibSampler.Name
  else
    Result := strNothing;
end;

function TVXShaderUniformTexture.GetTextureName: string;
begin
  if Assigned(FLibTexture) then
    Result := FLibTexture.Name
  else
    Result := strNothing;
end;

function TVXShaderUniformTexture.GetTextureSwizzle: TSwizzleVector;
begin
  Result := FSwizzling;
end;

procedure TVXShaderUniformTexture.Loaded;
begin
  SetTextureName(FLibTexureName);
  SetSamplerName(FLibSamplerName);
end;

procedure TVXShaderUniformTexture.Notification(Sender: TObject;
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

procedure TVXShaderUniformTexture.ReadFromFiler(AReader: TReader);
begin
  with AReader do
  begin
    inherited;
    LibTextureName := ReadString;
    LibSamplerName := ReadString;
    FSwizzling[0] := TVXTextureSwizzle(ReadInteger);
    FSwizzling[1] := TVXTextureSwizzle(ReadInteger);
    FSwizzling[2] := TVXTextureSwizzle(ReadInteger);
    FSwizzling[3] := TVXTextureSwizzle(ReadInteger);
  end;
end;

procedure TVXShaderUniformTexture.SetTextureName(
  const AValue: string);
var
  LTexture: TVXAbstractTexture;
begin
  if csLoading in TVXBaseShaderModel(Owner).GetMaterialLibraryEx.ComponentState
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
    TVXBaseShaderModel(Owner).GetMaterialLibraryEx.Components.GetTextureByName(AValue);

  if Assigned(LTexture) then
  begin
    if LTexture is TVXFrameBufferAttachment then
    begin
      if TVXFrameBufferAttachment(LTexture).OnlyWrite then
      begin
        if IsDesignTime then
          InformationDlg('Can not use write only attachment as texture')
        else
          ShowMessage(Format('Attempt to write only attachment "%s" for uniform "%s"',
            [LTexture.Name, Name]));
        NotifyChange(Self);
        exit;
      end;
    end;
    LTexture.RegisterUser(Self);
    FLibTexture := LTexture;
  end;
  NotifyChange(Self);
end;

procedure TVXShaderUniformTexture.SetSamplerName(const AValue: string);
var
  LSampler: TVXTextureSampler;
begin
  if csLoading in TVXBaseShaderModel(Owner).GetMaterialLibraryEx.ComponentState
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
    TVXBaseShaderModel(Owner).GetMaterialLibraryEx.Components.GetSamplerByName(AValue);

  if Assigned(LSampler) then
  begin
    LSampler.RegisterUser(Self);
    FLibSampler := LSampler;
  end;

  NotifyChange(Self);
end;

procedure TVXShaderUniformTexture.SetTextureSwizzle(const AValue:
  TSwizzleVector);
begin
  FSwizzling := AValue;
end;

procedure TVXShaderUniformTexture.WriteToFiler(AWriter: TWriter);
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


{ TVXAbstractShaderUniform }

function TVXAbstractShaderUniform.GetFloat: Single;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetGLSLSamplerType: TVXSLSamplerType;
begin
  Result := FSamplerType;
end;

function TVXAbstractShaderUniform.GetGLSLType: TVXSLDataType;
begin
  Result := FType;
end;

function TVXAbstractShaderUniform.GetInt: GLint;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetIVec2: TVector2i;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetIVec3: TVector3i;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetIVec4: TVector4i;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetMat2: TMatrix2f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetMat3: TMatrix3f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetMat4: TMatrix4f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetName: string;
begin
  Result := FName;
end;

function TVXAbstractShaderUniform.GetSamplerName: string;
begin
  Result := strNothing;
end;

procedure TVXAbstractShaderUniform.Apply(var ARci: TVXRenderContextInfo);
begin
end;

function TVXAbstractShaderUniform.GetAutoSetMethod: string;
begin
  Result := strNothing;
end;

function TVXAbstractShaderUniform.GetTextureName: string;
begin
  Result := strNothing;
end;

function TVXAbstractShaderUniform.GetTextureSwizzle: TSwizzleVector;
begin
  Result := cDefaultSwizzleVector;
end;

function TVXAbstractShaderUniform.GetUInt: GLuint;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetUVec2: TVector2ui;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetUVec3: TVector3ui;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetUVec4: TVector4ui;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetVec2: TVector2f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetVec3: TVector3f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TVXAbstractShaderUniform.GetVec4: TVector;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

procedure TVXAbstractShaderUniform.ReadFromFiler(AReader: TReader);
begin
end;

procedure TVXAbstractShaderUniform.SetFloat(const Value: GLfloat);
begin
end;

procedure TVXAbstractShaderUniform.SetFloatArray(const Values: PGLFloat;
  Count: Integer);
begin
end;

procedure TVXAbstractShaderUniform.SetInt(const Value: Integer);
begin
end;

procedure TVXAbstractShaderUniform.SetIntArray(const Values: PGLInt;
  Count: Integer);
begin
end;

procedure TVXAbstractShaderUniform.SetIVec2(const Value: TVector2i);
begin
end;

procedure TVXAbstractShaderUniform.SetIVec3(const Value: TVector3i);
begin
end;

procedure TVXAbstractShaderUniform.SetIVec4(const Value: TVector4i);
begin
end;

procedure TVXAbstractShaderUniform.SetMat2(const Value: TMatrix2f);
begin
end;

procedure TVXAbstractShaderUniform.SetMat3(const Value: TMatrix3f);
begin
end;

procedure TVXAbstractShaderUniform.SetMat4(const Value: TMatrix4f);
begin
end;

procedure TVXAbstractShaderUniform.SetSamplerName(const AValue: string);
begin
end;

procedure TVXAbstractShaderUniform.SetAutoSetMethod(const AValue: string);
begin
end;

procedure TVXAbstractShaderUniform.SetTextureName(const AValue: string);
begin
end;

procedure TVXAbstractShaderUniform.SetTextureSwizzle(const AValue:
  TSwizzleVector);
begin
end;

procedure TVXAbstractShaderUniform.SetUInt(const Value: GLuint);
begin
end;

procedure TVXAbstractShaderUniform.SetUIntArray(const Values: PGLUInt;
  Count: Integer);
begin
end;

procedure TVXAbstractShaderUniform.SetUVec2(const Value: TVector2ui);
begin
end;

procedure TVXAbstractShaderUniform.SetUVec3(const Value: TVector3ui);
begin
end;

procedure TVXAbstractShaderUniform.SetUVec4(const Value: TVector4ui);
begin
end;

procedure TVXAbstractShaderUniform.SetVec2(const Value: TVector2f);
begin
end;

procedure TVXAbstractShaderUniform.SetVec3(const Value: TVector3f);
begin
end;

procedure TVXAbstractShaderUniform.SetVec4(const Value: TVector4f);
begin
end;

procedure TVXAbstractShaderUniform.WriteToFiler(AWriter: TWriter);
begin
end;

{ TVXShaderUniform }

function TVXShaderUniform.GetFloat: Single;
begin
  // TODO: Type checking
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetInt: GLint;
begin
  glGetUniformiv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetIVec2: TVector2i;
begin
  glGetUniformiv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetIVec3: TVector3i;
begin
  glGetUniformiv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetIVec4: TVector4i;
begin
  glGetUniformiv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetMat2: TMatrix2f;
begin
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetMat3: TMatrix3f;
begin
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetMat4: TMatrix4f;
begin
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetProgram: GLuint;
begin
  Result := TVXBaseShaderModel(Owner).FHandle.Handle;
end;

procedure TVXShaderUniform.Apply(var ARci: TVXRenderContextInfo);
begin
  if Assigned(FAutoSet) then
    FAutoSet(Self, ARci);
end;

procedure TVXShaderUniform.Assign(Source: TPersistent);
var
  LUniform: TVXShaderUniform;
begin
  if Source is TVXShaderUniform then
  begin
    LUniform := TVXShaderUniform(Source);
    FName := LUniform.Name;
    FNameHashCode := LUniform.FNameHashCode;
    FType := LUniform.FType;
    FSamplerType := LUniform.FSamplerType;
    FAutoSet := LUniform.FAutoSet;
  end;
  inherited;
end;

function TVXShaderUniform.GetAutoSetMethod: string;
begin
  Result := GetUniformAutoSetMethodName(FAutoSet);
end;

function TVXShaderUniform.GetUInt: GLuint;
begin
  glGetUniformuiv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetUVec2: TVector2ui;
begin
  glGetUniformuiv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetUVec3: TVector3ui;
begin
  glGetUniformuiv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetUVec4: TVector4ui;
begin
  glGetUniformuiv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetVec2: TVector2f;
begin
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetVec3: TVector3f;
begin
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

function TVXShaderUniform.GetVec4: TVector;
begin
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

procedure TVXShaderUniform.PopProgram;
begin
  CurrentVXContext.VXStates.CurrentProgram := FStoreProgram;
end;

procedure TVXShaderUniform.PushProgram;
begin
  with CurrentVXContext.VxStates do
  begin
    FStoreProgram := CurrentProgram;
    CurrentProgram := GetProgram;
  end;
end;

procedure TVXShaderUniform.ReadFromFiler(AReader: TReader);
begin
  with AReader do
  begin
    FName := ReadString;
    FNameHashCode := ComputeNameHashKey(FName);
    FType := TVXSLDataType(ReadInteger);
    FSamplerType := TVXSLSamplerType(ReadInteger);
    SetAutoSetMethod(ReadString);
  end;
end;

procedure TVXShaderUniform.SetFloat(const Value: GLfloat);
begin
  PushProgram;
  glUniform1f(FLocation, Value);
  PopProgram;
end;

procedure TVXShaderUniform.SetFloatArray(const Values: PGLFloat;
  Count: Integer);
begin
  PushProgram;
  glUniform1fv(FLocation, Count, Values);
  PopProgram;
end;

procedure TVXShaderUniform.SetInt(const Value: Integer);
begin
  PushProgram;
  glUniform1i(FLocation, Value);
  PopProgram;
end;

procedure TVXShaderUniform.SetIntArray(const Values: PGLInt; Count: Integer);
begin
  PushProgram;
  glUniform1iv(FLocation, Count, Values);
  PopProgram;
end;

procedure TVXShaderUniform.SetIVec2(const Value: TVector2i);
begin
  PushProgram;
  glUniform2i(FLocation, Value.X, Value.Y);
  PopProgram;
end;

procedure TVXShaderUniform.SetIVec3(const Value: TVector3i);
begin
  PushProgram;
  glUniform3i(FLocation, Value.X, Value.Y, Value.Z);
  PopProgram;
end;

procedure TVXShaderUniform.SetIVec4(const Value: TVector4i);
begin
  PushProgram;
  glUniform4i(FLocation, Value.X, Value.Y, Value.Z, Value.W);
  PopProgram;
end;

procedure TVXShaderUniform.SetMat2(const Value: TMatrix2f);
begin
  PushProgram;
  glUniformMatrix2fv(FLocation, 1, GLboolean(False), @Value);
  PopProgram;
end;

procedure TVXShaderUniform.SetMat3(const Value: TMatrix3f);
begin
  PushProgram;
  glUniformMatrix2fv(FLocation, 1, 0, @Value);
  PopProgram;
end;

procedure TVXShaderUniform.SetMat4(const Value: TMatrix4f);
begin
  PushProgram;
  glUniformMatrix4fv(FLocation, 1, 0, @Value);
  PopProgram;
end;

procedure TVXShaderUniform.SetAutoSetMethod(const AValue: string);
begin
  FAutoSet := GetUniformAutoSetMethod(AValue);
end;

procedure TVXShaderUniform.SetUInt(const Value: GLuint);
begin
  PushProgram;
  glUniform1ui(FLocation, Value);
  PopProgram;
end;

procedure TVXShaderUniform.SetUIntArray(const Values: PGLUInt; Count: Integer);
begin
  PushProgram;
  glUniform1uiv(FLocation, Count, Values);
  PopProgram;
end;

procedure TVXShaderUniform.SetUVec2(const Value: TVector2ui);
begin
  PushProgram;
  glUniform2ui(FLocation, Value.X, Value.Y);
  PopProgram;
end;

procedure TVXShaderUniform.SetUVec3(const Value: TVector3ui);
begin
  PushProgram;
  glUniform3ui(FLocation, Value.X, Value.Y, Value.Z);
  PopProgram;
end;

procedure TVXShaderUniform.SetUVec4(const Value: TVector4ui);
begin
  PushProgram;
  glUniform4ui(FLocation, Value.X, Value.Y, Value.Z, Value.W);
  PopProgram;
end;

procedure TVXShaderUniform.SetVec2(const Value: TVector2f);
begin
  PushProgram;
  glUniform2f(FLocation, Value.X, Value.Y);
  PopProgram;
end;

procedure TVXShaderUniform.SetVec3(const Value: TVector3f);
begin
  PushProgram;
  glUniform3f(FLocation, Value.X, Value.Y, Value.Z);
  PopProgram;
end;

procedure TVXShaderUniform.SetVec4(const Value: TVector4f);
begin
  PushProgram;
  glUniform4f(FLocation, Value.X, Value.Y, Value.Z, Value.W);
  PopProgram;
end;

procedure TVXShaderUniform.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteString(FName);
    WriteInteger(Integer(FType));
    WriteInteger(Integer(FSamplerType));
    WriteString(GetAutoSetMethod);
  end;
end;


{ TVXShaderUniformDSA }

procedure TVXShaderUniformDSA.SetFloat(const Value: GLfloat);
begin
  glProgramUniform1f(GetProgram, FLocation, Value);
end;

procedure TVXShaderUniformDSA.SetFloatArray(const Values: PGLFloat;
  Count: Integer);
begin
  glProgramUniform1fv(GetProgram, FLocation, Count, Values);
end;

procedure TVXShaderUniformDSA.SetInt(const Value: Integer);
begin
  glProgramUniform1i(GetProgram, FLocation, Value);
end;

procedure TVXShaderUniformDSA.SetIntArray(const Values: PGLInt; Count: Integer);
begin
  glProgramUniform1iv(GetProgram, FLocation, Count, Values);
end;

procedure TVXShaderUniformDSA.SetIVec2(const Value: TVector2i);
begin
  glProgramUniform2i(GetProgram, FLocation, Value.X, Value.Y);
end;

procedure TVXShaderUniformDSA.SetIVec3(const Value: TVector3i);
begin
  glProgramUniform3i(GetProgram, FLocation, Value.X, Value.Y, Value.Z);
end;

procedure TVXShaderUniformDSA.SetIVec4(const Value: TVector4i);
begin
  glProgramUniform4i(GetProgram, FLocation, Value.X, Value.Y, Value.Z,
    Value.W);
end;

procedure TVXShaderUniformDSA.SetMat2(const Value: TMatrix2f);
begin
  glProgramUniformMatrix2fv(GetProgram, FLocation, 1, 0, @Value);
end;

procedure TVXShaderUniformDSA.SetMat3(const Value: TMatrix3f);
begin
  glProgramUniformMatrix3fv(GetProgram, FLocation, 1, 0, @Value);
end;

procedure TVXShaderUniformDSA.SetMat4(const Value: TMatrix4f);
begin
  glProgramUniformMatrix4fv(GetProgram, FLocation, 1, 0, @Value);
end;

procedure TVXShaderUniformDSA.SetUInt(const Value: GLuint);
begin
  glProgramUniform1ui(GetProgram, FLocation, Value);
end;

procedure TVXShaderUniformDSA.SetUIntArray(const Values: PGLUInt;
  Count: Integer);
begin
  glProgramUniform1uiv(GetProgram, FLocation, Count, Values);
end;

procedure TVXShaderUniformDSA.SetUVec2(const Value: TVector2ui);
begin
  glProgramUniform2ui(GetProgram, FLocation, Value.X, Value.Y);
end;

procedure TVXShaderUniformDSA.SetUVec3(const Value: TVector3ui);
begin
  glProgramUniform3ui(GetProgram, FLocation, Value.X, Value.Y, Value.Z);
end;

procedure TVXShaderUniformDSA.SetUVec4(const Value: TVector4ui);
begin
  glProgramUniform4ui(GetProgram, FLocation, Value.X, Value.Y, Value.Z,
    Value.W);
end;

procedure TVXShaderUniformDSA.SetVec2(const Value: TVector2f);
begin
  glProgramUniform2f(GetProgram, FLocation, Value.X, Value.Y);
end;

procedure TVXShaderUniformDSA.SetVec3(const Value: TVector3f);
begin
  glProgramUniform3f(GetProgram, FLocation, Value.X, Value.Y, Value.Z);
end;

procedure TVXShaderUniformDSA.SetVec4(const Value: TVector4f);
begin
  glProgramUniform4f(GetProgram, FLocation, Value.X, Value.Y, Value.Z,
    Value.W);
end;


{ TVXTextureSwizzling }

procedure TVXTextureSwizzling.Assign(Source: TPersistent);
var
  LSwizzling: TVXTextureSwizzling;
begin
  if Source is TVXTextureSwizzling then
  begin
    LSwizzling := TVXTextureSwizzling(Source);
    FSwizzles[0] := LSwizzling.FSwizzles[0];
    FSwizzles[1] := LSwizzling.FSwizzles[1];
    FSwizzles[2] := LSwizzling.FSwizzles[2];
    FSwizzles[3] := LSwizzling.FSwizzles[3];
  end;
  inherited;
end;

constructor TVXTextureSwizzling.Create(AOwner: TPersistent);
begin
  inherited;
  FSwizzles := cDefaultSwizzleVector;
end;

function TVXTextureSwizzling.GetSwizzle(AIndex: Integer): TVXTextureSwizzle;
begin
  Result := FSwizzles[AIndex];
end;

procedure TVXTextureSwizzling.ReadFromFiler(AReader: TReader);
begin
  with AReader do
  begin
    ReadInteger;
    FSwizzles[0] := TVXTextureSwizzle(ReadInteger);
    FSwizzles[1] := TVXTextureSwizzle(ReadInteger);
    FSwizzles[2] := TVXTextureSwizzle(ReadInteger);
    FSwizzles[3] := TVXTextureSwizzle(ReadInteger);
  end;
end;

procedure TVXTextureSwizzling.SetSwizzle(AIndex: Integer;
  AValue: TVXTextureSwizzle);
begin
  if AValue <> FSwizzles[AIndex] then
  begin
    FSwizzles[AIndex] := AValue;
    NotifyChange(Self);
  end;
end;

function TVXTextureSwizzling.StoreSwizzle(AIndex: Integer): Boolean;
begin
  Result := (FSwizzles[AIndex] <> cDefaultSwizzleVector[AIndex]);
end;

procedure TVXTextureSwizzling.WriteToFiler(AWriter: TWriter);
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

{ TVXFrameBufferAttachment }

procedure TVXFrameBufferAttachment.Apply(var ARci: TVXRenderContextInfo);
begin
  if FIsValid and not FOnlyWrite then
  begin
    // Just bind
    with ARci.VxStates do
    begin
      ActiveTextureEnabled[FHandle.Target] := True;
      TextureBinding[ActiveTexture, FHandle.Target] := FHandle.Handle;
    end;
  end
  else
    ARci.VXStates.TextureBinding[ARci.VXStates.ActiveTexture, FHandle.Target] :=
      0;
end;

procedure TVXFrameBufferAttachment.Assign(Source: TPersistent);
var
  LAttachment: TVXFrameBufferAttachment;
begin
  if Source is TVXFrameBufferAttachment then
  begin
    LAttachment := TVXFrameBufferAttachment(Source);
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

constructor TVXFrameBufferAttachment.Create(AOwner: TVXXCollection);
begin
  inherited;
  FDefferedInit := False;
  FHandle := TVXTextureHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FRenderBufferHandle := TVXRenderbufferHandle.Create;
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
  Name := TVXMatLibComponents(AOwner).MakeUniqueName('Attachment');
end;

destructor TVXFrameBufferAttachment.Destroy;
begin
  FHandle.Destroy;
  FRenderBufferHandle.Destroy;
  inherited;
end;

procedure TVXFrameBufferAttachment.DoOnPrepare(Sender: TVXContext);
var
  LTarget: TVXTextureTarget;
  w, h, d, s, Level, MaxLevel: Integer;
  glTarget, glFormat, glFace: GLEnum;
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
    and not GL_EXT_framebuffer_multisample then
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
    if w > Integer(Sender.VXStates.MaxCubeTextureSize) then
      w := Sender.VXStates.MaxCubeTextureSize;
    h := w;
    if FLayered then
    begin
      if d < 6 then
        d := 6
      else if (d mod 6) > 0 then
        d := 6 * (d div 6 + 1);
    end;
  end
  else if w > Integer(Sender.VXStates.MaxTextureSize) then
    w := Sender.VXStates.MaxTextureSize;
  if h > Integer(Sender.VXStates.MaxTextureSize) then
    h := Sender.VXStates.MaxTextureSize;
  if FLayered then
  begin
    if d > Integer(Sender.VXStates.MaxArrayTextureSize) then
      d := Sender.VXStates.MaxArrayTextureSize;
  end
  else if d > Integer(Sender.VXStates.Max3DTextureSize) then
    d := Sender.VXStates.Max3DTextureSize;
  if (s > -1) and (s > Integer(Sender.VXStates.MaxSamples)) then
    s := Sender.VXStates.MaxSamples;

  glTarget := DecodeTextureTarget(LTarget);

  if (FHandle.Target <> LTarget)
    and (FHandle.Target <> ttNoShape) then
  begin
    FHandle.DestroyHandle;
    FHandle.AllocateHandle;
  end;
  FHandle.Target := LTarget;

  glFormat := InternalFormatToOpenVXFormat(FInternalFormat);

  if FOnlyWrite and ((LTarget = ttTexture2D) or (LTarget =
    ttTexture2DMultisample))
    and (FRenderBufferHandle.IsSupported > False) then
  begin
    if LTarget = ttTexture2D then
      FRenderBufferHandle.SetStorage(glFormat, w, h)
    else
      FRenderBufferHandle.SetStorageMultisample(glFormat, s, w, h);
  end
  else
    with Sender do
    begin
      VXStates.ActiveTextureEnabled[FHandle.Target] := True;
      VXStates.TextureBinding[VXStates.ActiveTexture, FHandle.Target] :=
        FHandle.Handle;
      MaxLevel := CalcTextureLevelNumber(LTarget, w, h, d);

      case glTarget of

        GL_TEXTURE_1D:
          for Level := 0 to MaxLevel - 1 do
          begin
            glTexImage1D(glTarget, Level, glFormat, w, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;

        GL_TEXTURE_2D:
          for Level := 0 to MaxLevel - 1 do
          begin
            glTexImage2D(glTarget, Level, glFormat, w, h, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
            Div2(h);
          end;

        GL_TEXTURE_RECTANGLE:
          begin
            glTexImage2D(glTarget, 0, glFormat, w, h, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
          end;

        GL_TEXTURE_3D:
          for Level := 0 to MaxLevel - 1 do
          begin
            glTexImage3D(glTarget, Level, glFormat, w, h, d, 0, GL_RGBA,
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
              glTexImage2D(glFace, Level, glFormat, w, w, 0, GL_RGBA,
                GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;

        GL_TEXTURE_1D_ARRAY:
          for Level := 0 to MaxLevel - 1 do
          begin
            glTexImage2D(glTarget, Level, glFormat, w, h, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;

        GL_TEXTURE_2D_ARRAY:
          for Level := 0 to MaxLevel - 1 do
          begin
            glTexImage3D(glTarget, Level, glFormat, w, h, d, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
            Div2(h);
          end;

        GL_TEXTURE_CUBE_MAP_ARRAY:
          for Level := 0 to MaxLevel - 1 do
          begin
            glTexImage3D(glTarget, Level, glFormat, w, w, d, 0, GL_RGBA,
              GL_UNSIGNED_BYTE, nil);
            Div2(w);
          end;
      end; // of case

      VXStates.ActiveTextureEnabled[FHandle.Target] := False;
      FOnlyWrite := False;
    end; // of texture

  if glGetError <> GL_NO_ERROR then
  begin
    ClearOpenGLError;
    ShowMessage(Format('Unable to create attachment "%s"', [Self.Name]));
    exit;
  end
  else
    FIsValid := True;

  FHandle.NotifyDataUpdated;
  FRenderBufferHandle.NotifyDataUpdated;
end;

class function TVXFrameBufferAttachment.FriendlyName: string;
begin
  Result := 'Framebuffer Attachment';
end;

procedure TVXFrameBufferAttachment.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  FRenderBufferHandle.NotifyChangesOfData;
  inherited;
end;

procedure TVXFrameBufferAttachment.ReadFromFiler(AReader: TReader);
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
      FInternalFormat := TVXInternalFormat(ReadInteger);
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TVXFrameBufferAttachment.SetCubeMap(AValue: Boolean);
begin
  if FCubeMap <> AValue then
  begin
    FCubeMap := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXFrameBufferAttachment.SetDepth(AValue: Integer);
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

procedure TVXFrameBufferAttachment.SetFixedSamplesLocation(AValue: Boolean);
begin
  if FFixedSamplesLocation <> AValue then
  begin
    FFixedSamplesLocation := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXFrameBufferAttachment.SetHeight(AValue: Integer);
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

procedure TVXFrameBufferAttachment.SetInternalFormat(
  const AValue: TVXInternalFormat);
begin
  if FInternalFormat <> AValue then
  begin
    FInternalFormat := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXFrameBufferAttachment.SetLayered(AValue: Boolean);
begin
  if FLayered <> AValue then
  begin
    FLayered := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXFrameBufferAttachment.SetOnlyWrite(AValue: Boolean);
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

procedure TVXFrameBufferAttachment.SetSamples(AValue: Integer);
begin
  if AValue < -1 then
    AValue := -1;
  if FSamples <> AValue then
  begin
    FSamples := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXFrameBufferAttachment.SetWidth(AValue: Integer);
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

procedure TVXFrameBufferAttachment.UnApply(var ARci: TVXRenderContextInfo);
begin
  ARci.VXStates.ActiveTextureEnabled[FHandle.Target] := False;
end;

procedure TVXFrameBufferAttachment.WriteToFiler(AWriter: TWriter);
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

{ TStandartUniformAutoSetExecutor} 

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
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.vec4 := ARci.cameraPosition;
end;

procedure TStandartUniformAutoSetExecutor.SetInvModelMatrix(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.InvModelMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetInvModelViewMatrix(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.InvModelViewMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetLightSource0Position(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.vec4 := ARci.VXStates.LightPosition[0];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackAmbient(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.vec4 := ARci.VXStates.MaterialAmbient[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackDiffuse(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.vec4 := ARci.VXStates.MaterialDiffuse[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackEmission(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.vec4 := ARci.VXStates.MaterialEmission[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackShininess(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.float := ARci.VXStates.MaterialShininess[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialBackSpecular(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.vec4 := ARci.VXStates.MaterialSpecular[cmBack];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontAmbient(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.vec4 := ARci.VXStates.MaterialAmbient[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontDiffuse(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.vec4 := ARci.VXStates.MaterialDiffuse[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontEmission(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.vec4 := ARci.VXStates.MaterialEmission[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontShininess(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.float := ARci.VXStates.MaterialShininess[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetMaterialFrontSpecular(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.vec4 := ARci.VXStates.MaterialSpecular[cmFront];
end;

procedure TStandartUniformAutoSetExecutor.SetModelMatrix(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ModelMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetModelViewMatrix(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ModelViewMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetNormalModelMatrix(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.mat3 := ARci.PipelineTransformation.NormalModelMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetProjectionMatrix(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ProjectionMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetViewMatrix(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ViewMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetViewProjectionMatrix(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ViewProjectionMatrix^;
end;

procedure TStandartUniformAutoSetExecutor.SetWorldViewProjectionMatrix(Sender:
  IShaderParameter; var ARci: TVXRenderContextInfo);
begin
  Sender.mat4 := MatrixMultiply(
    ARci.PipelineTransformation.ModelViewMatrix^,
    ARci.PipelineTransformation.ProjectionMatrix^);
end;


{ TVXASMVertexProgram }

procedure TVXASMVertexProgram.Assign(Source: TPersistent);
var
  LProg: TVXASMVertexProgram;
begin
  if Source is TVXASMVertexProgram then
  begin
    LProg := TVXASMVertexProgram(Source);
    FSource.Assign(LProg.FSource);
  end;
  inherited;
end;

constructor TVXASMVertexProgram.Create(AOwner: TVXXCollection);
begin
  inherited;
  FHandle := TVXVertexProgramHandle.Create;
  FHandle.OnPrapare := DoOnPrepare;
  FSource := TStringList.Create;
  FSource.OnChange := NotifyChange;
  Name := TVXMatLibComponents(AOwner).MakeUniqueName('VertexProg');
end;

destructor TVXASMVertexProgram.Destroy;
begin
  FHandle.Destroy;
  FSource.Destroy;
  inherited;
end;

procedure TVXASMVertexProgram.DoOnPrepare(Sender: TVXContext);
begin
  if FDefferedInit and not IsDesignTime then
    exit;
  try
    if FHandle.IsSupported > False then
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
            ShowMessage(Format('Program "%s" compilation successful - %s',
              [Name, FHandle.InfoLog]))
          else
            ShowMessage(Format('Program "%s" compilation failed - %s',
              [Name, FHandle.InfoLog]));
          FHandle.NotifyDataUpdated;
        end
        else
        begin
          if IsDesignTime then
            FInfoLog := 'No source'
          else
            ShowMessage(Format('Program "%s" has no source code', [Name]));
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
        ShowMessage(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

class function TVXASMVertexProgram.FriendlyName: string;
begin
  Result := 'ASM Vertex Program';
end;

function TVXASMVertexProgram.GetHandle: TVXVertexProgramHandle;
begin
  Result := FHandle;
end;

procedure TVXASMVertexProgram.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TVXASMVertexProgram.ReadFromFiler(AReader: TReader);
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

procedure TVXASMVertexProgram.SetSource(AValue: TStringList);
begin
  FSource.Assign(AValue);
end;

procedure TVXASMVertexProgram.SetSourceFile(AValue: string);
begin
  FixPathDelimiter(AValue);
  if FSourceFile <> AValue then
  begin
    FSourceFile := AValue;
    NotifyChange(Self);
  end;
end;

procedure TVXASMVertexProgram.WriteToFiler(AWriter: TWriter);
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

initialization

  RegisterClasses(
    [
    TVXTextureImageEx,
      TVXFrameBufferAttachment,
      TVXTextureSampler,
      TVXTextureCombiner,
      TVXShaderEx,
      TVXASMVertexProgram,
      TVXMaterialLibraryEx,
      TVXShaderUniform,
      TVXShaderUniformDSA,
      TVXShaderUniformTexture
      ]);

  RegisterXCollectionItemClass(TVXTextureImageEx);
  RegisterXCollectionItemClass(TVXTextureSampler);
  RegisterXCollectionItemClass(TVXFrameBufferAttachment);
  RegisterXCollectionItemClass(TVXTextureCombiner);
  RegisterXCollectionItemClass(TVXShaderEx);
  RegisterXCollectionItemClass(TVXASMVertexProgram);

  vStandartUniformAutoSetExecutor := TStandartUniformAutoSetExecutor.Create;

finalization

  vStandartUniformAutoSetExecutor.Destroy;

end.

