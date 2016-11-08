//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
   Handles all the color and texture stuff.

}
unit VKS.Texture;

interface

{$I VKScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes, System.SysUtils, System.Types,
  FMX.Graphics, FMX.Objects,
  //VKS
  VKS.OpenGLAdapter,
  VKS.Strings, VKS.CrossPlatform, VKS.BaseClasses,
  VKS.VectorGeometry, VKS.Graphics, VKS.Context, VKS.State, VKS.Color, VKS.Coordinates,
  VKS.RenderContextInfo, VKS.TextureFormat, VKS.ApplicationFileIO, VKS.Utils;

const
  cDefaultNormalMapScale = 0.125;

  CmtPX = 0;
  CmtNX = 1;
  CmtPY = 2;
  CmtNY = 3;
  CmtPZ = 4;
  CmtNZ = 5;

type
  TVKMinFilter =
  (
    miNearest,
    miLinear,
    miNearestMipmapNearest,
    miLinearMipmapNearest,
    miNearestMipmapLinear,
    miLinearMipmapLinear
  );

  TVKMagFilter = (maNearest, maLinear);

  TVKTextureMode = (tmDecal, tmModulate, tmBlend, tmReplace, tmAdd);
  TVKTextureWrap = (twBoth, twNone, twVertical, twHorizontal, twSeparate);

  // Specifies how depth values should be treated
  // during filtering and texture application
  TVKDepthTextureMode = (dtmLuminance, dtmIntensity, dtmAlpha);

  // Specifies the depth comparison function.
  TVKDepthCompareFunc = TDepthFunction;

  { Texture format for Vulkan (rendering) use. 
  Internally, VKScene handles all "base" images as 32 Bits RGBA, but you can
  specify a generic format to reduce Vulkan texture memory use: }
  TVKTextureFormat = (
    tfDefault,
    tfRGB, // = tfRGB8
    tfRGBA, // = tfRGBA8
    tfRGB16, // = tfRGB5
    tfRGBA16, // = tfRGBA4
    tfAlpha, // = tfALPHA8
    tfLuminance, // = tfLUMINANCE8
    tfLuminanceAlpha, // = tfLUMINANCE8_ALPHA8
    tfIntensity, // = tfINTENSITY8
    tfNormalMap, // = tfRGB8
    tfRGBAFloat16, // = tfRGBA_FLOAT16_ATI
    tfRGBAFloat32, // = tfRGBA_FLOAT32_ATI
    tfExtended);

  // TVKTextureCompression
  //
  TVKTextureCompression = VKinternalCompression;

  TVKTexture = class;

  IVKTextureNotifyAble = interface(IVKNotifyAble)
    ['{0D9DC0B0-ECE4-4513-A8A1-5AE7022C9426}']
    procedure NotifyTexMapChange(Sender: TObject);
  end;

  // TTextureNeededEvent
  //
  TTextureNeededEvent = procedure(Sender: TObject; var textureFileName: string)
    of object;

  TVKTextureChange = (tcImage, tcParams);
  TVKTextureChanges = set of TVKTextureChange;

  { Defines how and if Alpha channel is defined for a texture image. 
    tiaDefault : uses the alpha channel in the image if any
    tiaAlphaFromIntensity : the alpha channel value is deduced from other
    RGB components intensity (the brighter, the more opaque)
    tiaSuperBlackTransparent : pixels with a RGB color of (0, 0, 0) are
    completely transparent, others are completely opaque
    tiaLuminance : the luminance value is calculated for each pixel
    and used for RGB and Alpha values
    tiaLuminanceSqrt : same as tiaLuminance but with an Sqrt(Luminance)
        tiaOpaque : alpha channel is uniformously set to 1.0
        tiaTopLeftPointColorTransparent : points of the same color as the
          top left point of the bitmap are transparent, others are opaque.
        
    }
  TVKTextureImageAlpha =
  (
    tiaDefault,
    tiaAlphaFromIntensity,
    tiaSuperBlackTransparent,
    tiaLuminance,
    tiaLuminanceSqrt,
    tiaOpaque,
    tiaTopLeftPointColorTransparent,
    tiaInverseLuminance,
    tiaInverseLuminanceSqrt,
    tiaBottomRightPointColorTransparent
  );

  // TVKTextureImage
  //
  { Base class for texture image data. 
   Basicly, subclasses are to be considered as different ways of getting
   a HBitmap (interfacing the actual source). 
   SubClasses should be registered using RegisterGLTextureImageClass to allow
   proper persistence and editability in the IDE experts. }
  TVKTextureImage = class(TVKUpdateAbleObject)
  private
    function GetResourceName: string;
  protected
    FOwnerTexture: TVKTexture;
    FOnTextureNeeded: TTextureNeededEvent;
    FResourceFile: string;
    class function IsSelfLoading: Boolean; virtual;
    procedure LoadTexture(AInternalFormat: GLinternalFormat); virtual;
    function GetTextureTarget: TVKTextureTarget; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
    function GetDepth: Integer; virtual; abstract;

    property OnTextureNeeded: TTextureNeededEvent read FOnTextureNeeded write
      FOnTextureNeeded;
  public
    { Public Properties }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    property OwnerTexture: TVKTexture read FOwnerTexture write FOwnerTexture;
    procedure NotifyChange(Sender: TObject); override;

    { Save textureImage to file. 
     This may not save a picture, but for instance, parameters, if the
     textureImage is a procedural texture. }
    procedure SaveToFile(const fileName: string); dynamic; abstract;
    { Load textureImage from a file. 
     This may not load a picture, but for instance, parameters, if the
     textureImage is a procedural texture. 
             Subclasses should invoke inherited which will take care of the
             "OnTextureNeeded" stuff. }
    procedure LoadFromFile(const fileName: string); dynamic;
    { Returns a user-friendly denomination for the class. 
     This denomination is used for picking a texture image class
     in the IDE expert. }
    class function FriendlyName: string; virtual; abstract;
    { Returns a user-friendly description for the class. 
     This denomination is used for helping the user when picking a
     texture image class in the IDE expert. If it's not overriden,
     takes its value from FriendlyName. }
    class function FriendlyDescription: string; virtual;

    { Request reload/refresh of data upon next use. }
    procedure Invalidate; dynamic;

    { Returns image's bitmap handle. 
     If the actual image is not a windows bitmap (BMP), descendants should
     take care of properly converting to bitmap. }
    function GetBitmap32: TVKImage; virtual; abstract;
    { Request for unloading bitmapData, to free some memory. 
     This one is invoked when GLScene no longer needs the Bitmap data
     it got through a call to GetHBitmap. 
     Subclasses may ignore this call if the HBitmap was obtained at
     no particular memory cost. }
    procedure ReleaseBitmap32; virtual;
    //{ AsBitmap : Returns the TextureImage as a TBitmap }
    function AsBitmap: TVKBitmap;
    procedure AssignToBitmap(aBitmap: TVKBitmap);

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Depth: Integer read GetDepth;
    { Native opengl texture target.  }
    property NativeTextureTarget: TVKTextureTarget read GetTextureTarget;
    property ResourceName: string read GetResourceName;
  end;

  TVKTextureImageClass = class of TVKTextureImage;

  // TVKBlankImage
  //
  { A texture image with no specified content, only a size. 
       This texture image type is of use if the context of your texture is
       calculated at run-time (with a TVKMemoryViewer for instance). }
  TVKBlankImage = class(TVKTextureImage)
  private
    { Private Declarations }
    procedure SetWidth(val: Integer);
    procedure SetHeight(val: Integer);
    procedure SetDepth(val: Integer);
    procedure SetCubeMap(const val: Boolean);
    procedure SetArray(const val: Boolean);
  protected
    { Protected Declarations }
    fBitmap: TVKImage;

    fWidth, fHeight, fDepth: Integer;
    { Store a icolor format, because fBitmap is not always defined}
    fColorFormat: GLenum;
    { Blank Cube Map }
    fCubeMap: Boolean;
    { Flag to interparate depth as layer }
    fArray: Boolean;

    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: TVKTextureTarget; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function GetBitmap32: TVKImage; override;
    procedure ReleaseBitmap32; override;

    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

  published
    { Published Declarations }
    { Width, heigth and depth of the blank image (for memory allocation). }
    property Width: Integer read GetWidth write SetWidth default 256;
    property Height: Integer read GetHeight write SetHeight default 256;
    property Depth: Integer read GetDepth write SetDepth default 0;
    property CubeMap: Boolean read fCubeMap write SetCubeMap default false;
    property TextureArray: Boolean read fArray write SetArray default false;
    property ColorFormat: GLenum read fColorFormat write fColorFormat;
  end;

  // TVKPictureImage
  //
  { Base class for image data classes internally based on a TPicture. }
  TVKPictureImage = class(TVKTextureImage)
  private
    { Private Declarations }
    FBitmap: TVKImage;
    FGLPicture: TVKPicture;
    FUpdateCounter: Integer;

  protected
    { Protected Declarations }
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: TVKTextureTarget; override;

    function GetPicture: TVKPicture;
    procedure SetPicture(const aPicture: TVKPicture);
    procedure PictureChanged(Sender: TObject);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    { Use this function if you are going to modify the Picture directly. 
     Each invokation MUST be balanced by a call to EndUpdate. }
    procedure BeginUpdate;
    { Ends a direct picture modification session. 
       Follows a BeginUpdate. }
    procedure EndUpdate;
    function GetBitmap32: TVKImage; override;
    procedure ReleaseBitmap32; override;

    { Holds the image content. }
    property Picture: TVKPicture read GetPicture write SetPicture;
  end;

  // TVKPersistentImage
  //
  { Stores any image compatible with Delphi's TPicture mechanism. 
   The picture's data is actually stored into the DFM, the original
   picture name or path is not remembered. It is similar in behaviour
   to Delphi's TImage. 
   Note that if original image is for instance JPEG format, only the JPEG
   data will be stored in the DFM (compact) }
  TVKPersistentImage = class(TVKPictureImage)
  private

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;
  published
    { Published Declarations }
    property Picture;
  end;

  // TVKPicFileImage
  //
  { Uses a picture whose data is found in a file (only filename is stored). 
       The image is unloaded after upload to OpenGL. }
  TVKPicFileImage = class(TVKPictureImage)
  private
    FPictureFileName: string;
    FAlreadyWarnedAboutMissingFile: Boolean;
    FWidth: Integer;
    FHeight: Integer;

  protected
    procedure SetPictureFileName(const val: string);
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    //: Only picture file name is saved
    procedure SaveToFile(const fileName: string); override;
    { Load picture file name or use fileName as picture filename. 
       The autodetection is based on the filelength and presence of zeros. }
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;

    function GetBitmap32: TVKImage; override;
    procedure Invalidate; override;

  published
    { Filename of the picture to use. }
    property PictureFileName: string read FPictureFileName write
      SetPictureFileName;
  end;


  // TVKCubeMapTarget
  //
 TVKCubeMapTarget = Integer;

  // TVKCubeMapImage
  //
  { A texture image used for specifying and stroing a cube map. 
       Not unlike TVKPictureImage, but storing 6 of them instead of just one. 
       Saving & loading as a whole currently not supported. }
  TVKCubeMapImage = class(TVKTextureImage)
  private
    { Private Declarations }
    FImage: TVKImage;
    FUpdateCounter: Integer;
    FPicture: array[cmtPX..cmtNZ] of TVKPicture;
  protected
    { Protected Declarations }
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    procedure SetPicture(index: TVKCubeMapTarget; const val: TVKPicture);
    function GetPicture(index: TVKCubeMapTarget): TVKPicture;
    function GetTextureTarget: TVKTextureTarget; override;

    procedure PictureChanged(Sender: TObject);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function GetBitmap32: TVKImage; override;
    procedure ReleaseBitmap32; override;

    { Use this function if you are going to modify the Picture directly. 
     Each invokation MUST be balanced by a call to EndUpdate. }
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;

    { Indexed access to the cube map's sub pictures. }
    property Picture[index: TVKCubeMapTarget]: TVKPicture read GetPicture write
    SetPicture;

  published
    { Public Declarations }
    property PicturePX: TVKPicture index cmtPX read GetPicture write SetPicture;
    property PictureNX: TVKPicture index cmtNX read GetPicture write SetPicture;
    property PicturePY: TVKPicture index cmtPY read GetPicture write SetPicture;
    property PictureNY: TVKPicture index cmtNY read GetPicture write SetPicture;
    property PicturePZ: TVKPicture index cmtPZ read GetPicture write SetPicture;
    property PictureNZ: TVKPicture index cmtNZ read GetPicture write SetPicture;
  end;

  // TVKTextureMappingMode
  //
  TVKTextureMappingMode = (tmmUser, tmmObjectLinear, tmmEyeLinear, tmmSphere,
    tmmCubeMapReflection, tmmCubeMapNormal,
    tmmCubeMapLight0, tmmCubeMapCamera);

  // TVKTexture
  //
    { Defines basic texturing properties. 
       You can control texture wrapping, smoothing/filtering and of course define
       the texture map (note that texturing is disabled by default). 
       A built-in mechanism (through ImageAlpha) allows auto-generation of an
       Alpha channel for all bitmaps (see TVKTextureImageAlpha). }
  TVKTexture = class(TVKUpdateAbleObject)
  private
    { Private Declarations }
    FTextureHandle: TVKTextureHandle;
    FSamplerHandle: TVKVirtualHandle;
    FTextureFormat: GLinternalFormat;
    FTextureMode: TVKTextureMode;
    FTextureWrap: TVKTextureWrap;
    FMinFilter: TVKMinFilter;
    FMagFilter: TVKMagFilter;
    FDisabled: Boolean;
    FImage: TVKTextureImage;
    FImageAlpha: TVKTextureImageAlpha;
    FImageBrightness: Single;
    FImageGamma: Single;
    FMappingMode: TVKTextureMappingMode;
    FMapSCoordinates: TVKCoordinates4;
    FMapTCoordinates: TVKCoordinates4;
    FMapRCoordinates: TVKCoordinates4;
    FMapQCoordinates: TVKCoordinates4;
    FOnTextureNeeded: TTextureNeededEvent;
    FCompression: TVKTextureCompression;
    FRequiredMemorySize: Integer;
    FFilteringQuality: TVKTextureFilteringQuality;
    FTexWidth: Integer;
    FTexHeight: Integer;
    FTexDepth: Integer;
    FEnvColor: TVKColor;
    FBorderColor: TVKColor;
    FNormalMapScale: Single;
    FTextureWrapS: TVKSeparateTextureWrap;
    FTextureWrapT: TVKSeparateTextureWrap;
    FTextureWrapR: TVKSeparateTextureWrap;
    fTextureCompareMode: TVKTextureCompareMode;
    fTextureCompareFunc: TVKDepthCompareFunc;
    fDepthTextureMode: TVKDepthTextureMode;
    FKeepImageAfterTransfer: Boolean;
  protected
    { Protected Declarations }
    procedure SetImage(AValue: TVKTextureImage);
    procedure SetImageAlpha(const val: TVKTextureImageAlpha);
    procedure SetImageBrightness(const val: Single);
    function StoreBrightness: Boolean;
    procedure SetImageGamma(const val: Single);
    function StoreGamma: Boolean;
    procedure SetMagFilter(AValue: TVKMagFilter);
    procedure SetMinFilter(AValue: TVKMinFilter);
    procedure SetTextureMode(AValue: TVKTextureMode);
    procedure SetTextureWrap(AValue: TVKTextureWrap);
    procedure SetTextureWrapS(AValue: TVKSeparateTextureWrap);
    procedure SetTextureWrapT(AValue: TVKSeparateTextureWrap);
    procedure SetTextureWrapR(AValue: TVKSeparateTextureWrap);
    function GetTextureFormat: TVKTextureFormat;
    procedure SetTextureFormat(const val: TVKTextureFormat);
    procedure SetTextureFormatEx(const val: GLinternalFormat);
    function StoreTextureFormatEx: Boolean;
    procedure SetCompression(const val: TVKTextureCompression);
    procedure SetFilteringQuality(const val: TVKTextureFilteringQuality);
    procedure SetMappingMode(const val: TVKTextureMappingMode);
    function GetMappingSCoordinates: TVKCoordinates4;
    procedure SetMappingSCoordinates(const val: TVKCoordinates4);
    function StoreMappingSCoordinates: Boolean;
    function GetMappingTCoordinates: TVKCoordinates4;
    procedure SetMappingTCoordinates(const val: TVKCoordinates4);
    function StoreMappingTCoordinates: Boolean;
    function GetMappingRCoordinates: TVKCoordinates4;
    procedure SetMappingRCoordinates(const val: TVKCoordinates4);
    function StoreMappingRCoordinates: Boolean;
    function GetMappingQCoordinates: TVKCoordinates4;
    procedure SetMappingQCoordinates(const val: TVKCoordinates4);
    function StoreMappingQCoordinates: Boolean;
    procedure SetDisabled(AValue: Boolean);
    procedure SetEnabled(const val: Boolean);
    function GetEnabled: Boolean;
    procedure SetEnvColor(const val: TVKColor);
    procedure SetBorderColor(const val: TVKColor);
    procedure SetNormalMapScale(const val: Single);
    procedure SetTextureCompareMode(const val: TVKTextureCompareMode);
    procedure SetTextureCompareFunc(const val: TVKDepthCompareFunc);
    procedure SetDepthTextureMode(const val: TVKDepthTextureMode);
    function StoreNormalMapScale: Boolean;

    function StoreImageClassName: Boolean;

    function GetHandle: GLuint; virtual;
    //: Load texture to OpenGL subsystem
    procedure PrepareImage(target: GLuint); virtual;
    //: Setup OpenGL texture parameters
    procedure PrepareParams(target: GLuint); virtual;

    procedure DoOnTextureNeeded(Sender: TObject; var textureFileName: string);
    procedure OnSamplerAllocate(Sender: TVKVirtualHandle; var Handle: Cardinal);
    procedure OnSamplerDestroy(Sender: TVKVirtualHandle; var Handle: Cardinal);
    //: Shows a special image that indicates an error
    procedure SetTextureErrorImage;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    property OnTextureNeeded: TTextureNeededEvent read FOnTextureNeeded write
      FOnTextureNeeded;

    procedure PrepareBuildList;
    procedure ApplyMappingMode;
    procedure UnApplyMappingMode;
    procedure Apply(var rci: TVKRenderContextInfo);
    procedure UnApply(var rci: TVKRenderContextInfo);
    { Applies to TEXTURE1 }
    procedure ApplyAsTexture2(var rci: TVKRenderContextInfo; textureMatrix: PMatrix
      = nil);
    procedure UnApplyAsTexture2(var rci: TVKRenderContextInfo;
      reloadIdentityTextureMatrix: boolean);
    { N=1 for TEXTURE0, N=2 for TEXTURE1, etc. }
    procedure ApplyAsTextureN(n: Integer; var rci: TVKRenderContextInfo;
      textureMatrix: PMatrix = nil);
    procedure UnApplyAsTextureN(n: Integer; var rci: TVKRenderContextInfo;
      reloadIdentityTextureMatrix: boolean);

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure NotifyImageChange;
    procedure NotifyParamsChange;

    procedure DestroyHandles;

    procedure SetImageClassName(const val: string);
    function GetImageClassName: string;

    { Returns the OpenGL memory used by the texture. 
      The compressed size is returned if, and only if texture compression
      if active and possible, and the texture has been allocated (Handle
      is defined), otherwise the estimated size (from TextureFormat
      specification) is returned. }
    function TextureImageRequiredMemory: Integer;
    { Allocates the texture handle if not already allocated. 
      The texture is binded and parameters are setup, but no image data
      is initialized by this call - for expert use only. }
    function AllocateHandle: GLuint;
    function IsHandleAllocated: Boolean;
    { Returns OpenGL texture format corresponding to current options. }
    function VulkanTextureFormat: Integer;
    { Returns if of float data type}
    function IsFloatType: Boolean;
    { Is the texture enabled?. 
      Always equals to 'not Disabled'. }
    property Enabled: Boolean read GetEnabled write SetEnabled;
    { Handle to the Vulkan texture object. 
      If the handle hasn't already been allocated, it will be allocated
      by this call (ie. do not use if no OpenGL context is active!) }
    property Handle: GLuint read GetHandle;
    property TextureHandle: TVKTextureHandle read FTextureHandle;

    { Actual width, height and depth used for last texture
      specification binding. }
    property TexWidth: Integer read FTexWidth;
    property TexHeight: Integer read FTexHeight;
    property TexDepth: Integer read FTexDepth;
    { Give texture rendering context }
  published
    { Published Declarations }

    { Image ClassName for enabling True polymorphism. 
    This is ugly, but since the default streaming mechanism does a
    really bad job at storing	polymorphic owned-object properties,
    and neither TFiler nor TPicture allow proper use of the built-in
    streaming, that's the only way I found to allow a user-extensible
    mechanism. }
    property ImageClassName: string read GetImageClassName write
      SetImageClassName stored StoreImageClassName;
    { Image data for the texture.  }
    property Image: TVKTextureImage read FImage write SetImage;

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

    { Texture magnification filter. }
    property MagFilter: TVKMagFilter read FMagFilter write SetMagFilter default
      maLinear;
    { Texture minification filter. }
    property MinFilter: TVKMinFilter read FMinFilter write SetMinFilter default
      miLinearMipMapLinear;
    { Texture application mode. }
    property TextureMode: TVKTextureMode read FTextureMode write SetTextureMode
      default tmDecal;
    { Wrapping mode for the texture. }
    property TextureWrap: TVKTextureWrap read FTextureWrap write SetTextureWrap
      default twBoth;
    { Wrapping mode for the texture when TextureWrap=twSeparate. }
    property TextureWrapS: TVKSeparateTextureWrap read FTextureWrapS write
      SetTextureWrapS default twRepeat;
    property TextureWrapT: TVKSeparateTextureWrap read FTextureWrapT write
      SetTextureWrapT default twRepeat;
    property TextureWrapR: TVKSeparateTextureWrap read FTextureWrapR write
      SetTextureWrapR default twRepeat;

    { Texture format for use by the renderer. 
    See TVKTextureFormat for details. }
    property TextureFormat: TVKTextureFormat read GetTextureFormat write
      SetTextureFormat default tfDefault;
    property TextureFormatEx: GLinternalFormat read FTextureFormat write
      SetTextureFormatEx stored StoreTextureFormatEx;

    { Texture compression control. 
    If True the compressed TextureFormat variant (the Vulkan ICD must
    support GL_ARB_texture_compression, or this option is ignored). }
    property Compression: TVKTextureCompression read FCompression write
      SetCompression default tcDefault;
    { Specifies texture filtering quality. 
    You can choose between bilinear and trilinear filetring (anisotropic). 
    The Vulkan ICD must support GL_EXT_texture_filter_anisotropic or
    this property is ignored. }
    property FilteringQuality: TVKTextureFilteringQuality read FFilteringQuality
      write SetFilteringQuality default tfIsotropic;

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

    { Texture Environment color. }
    property EnvColor: TVKColor read FEnvColor write SetEnvColor;
    { Texture Border color. }
    property BorderColor: TVKColor read FBorderColor write SetBorderColor;
    { If true, the texture is disabled (not used). }
    property Disabled: Boolean read FDisabled write SetDisabled default True;

    { Normal Map scaling. 
    Only applies when TextureFormat is tfNormalMap, this property defines
    the scaling that is applied during normal map generation (ie. controls
    the intensity of the bumps). }
    property NormalMapScale: Single read FNormalMapScale write SetNormalMapScale
      stored StoreNormalMapScale;

    property TextureCompareMode: TVKTextureCompareMode read fTextureCompareMode
      write SetTextureCompareMode default tcmNone;
    property TextureCompareFunc: TVKDepthCompareFunc read fTextureCompareFunc
      write SetTextureCompareFunc default cfLequal;
    property DepthTextureMode: TVKDepthTextureMode read fDepthTextureMode write
      SetDepthTextureMode default dtmLuminance;

    { Disable image release after transfering it to VGA. }
    property KeepImageAfterTransfer: Boolean read FKeepImageAfterTransfer
      write FKeepImageAfterTransfer default False;
  end;

  // TVKTextureExItem
  //
  TVKTextureExItem = class(TCollectionItem, IVKTextureNotifyAble)
  private
    { Private Decalarations }
    FTexture: TVKTexture;
    FTextureIndex: Integer;
    FTextureOffset, FTextureScale: TVKCoordinates;
    FTextureMatrixIsIdentity: Boolean;
    FTextureMatrix: TMatrix;
    FApplied: Boolean;

    //implementing IInterface
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
  protected
    { Protected Decalarations }
    function GetDisplayName: string; override;
    function GetOwner: TPersistent; override;
    procedure SetTexture(const Value: TVKTexture);
    procedure SetTextureIndex(const Value: Integer);
    procedure SetTextureOffset(const Value: TVKCoordinates);
    procedure SetTextureScale(const Value: TVKCoordinates);
    procedure NotifyTexMapChange(Sender: TObject);

    procedure CalculateTextureMatrix;

    procedure OnNotifyChange(Sender: TObject);

  public
    { Public Decalarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject);

    procedure Apply(var rci: TVKRenderContextInfo);
    procedure UnApply(var rci: TVKRenderContextInfo);

  published
    { Published Decalarations }
    property Texture: TVKTexture read FTexture write SetTexture;
    property TextureIndex: Integer read FTextureIndex write SetTextureIndex;
    property TextureOffset: TVKCoordinates read FTextureOffset write
      SetTextureOffset;
    property TextureScale: TVKCoordinates read FTextureScale write
      SetTextureScale;

  end;

  // TVKTextureEx
  //
  TVKTextureEx = class(TCollection)
  private
    FOwner: TVKUpdateAbleObject;

  protected
    { Protected Decalarations }
    procedure SetItems(index: Integer; const Value: TVKTextureExItem);
    function GetItems(index: Integer): TVKTextureExItem;
    function GetOwner: TPersistent; override;
  public
    { Public Decalarations }
    constructor Create(AOwner: TVKUpdateAbleObject);

    procedure NotifyChange(Sender: TObject);
    procedure Apply(var rci: TVKRenderContextInfo);
    procedure UnApply(var rci: TVKRenderContextInfo);
    function IsTextureEnabled(Index: Integer): Boolean;

    function Add: TVKTextureExItem;

    property Items[index: Integer]: TVKTextureExItem read GetItems write
    SetItems; default;
    procedure Loaded;
  end;

  ETexture = class(Exception);
  EGLShaderException = class(Exception);

  //: Register a TVKTextureImageClass (used for persistence and IDE purposes)
procedure RegisterTextureImageClass(textureImageClass: TVKTextureImageClass);
//: Finds a registerer TVKTextureImageClass using its classname
function FindTextureImageClass(const className: string): TVKTextureImageClass;
//: Finds a registerer TVKTextureImageClass using its FriendlyName
function FindTextureImageClassByFriendlyName(const friendlyName: string):
  TVKTextureImageClass;
//: Defines a TStrings with the list of registered TVKTextureImageClass.
procedure SetTextureImageClassesToStrings(aStrings: TStrings);
{ Creates a TStrings with the list of registered TVKTextureImageClass.
 To be freed by caller. }
function GetTextureImageClassesAsStrings: TStrings;

procedure RegisterTGraphicClassFileExtension(const extension: string;
  const aClass: TGraphicClass);
function CreateGraphicFromFile(const fileName: string): TVKGraphic;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// TODO: remove dependancy on VKS.Scene.pas unit (related to tmmCubeMapLight0)

uses
  VKS.Scene,
  VKS.XOpenGL,
  VKS.PictureRegisteredFormats,
  VKS.VectorTypes;

const
  cTextureMode: array[tmDecal..tmAdd] of GLEnum =
    (GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE, GL_ADD);

  cOldTextureFormatToInternalFormat: array[tfRGB..tfRGBAFloat32] of
    GLinternalFormat = (
    tfRGB8,
    tfRGBA8,
    tfRGB5,
    tfRGBA4,
    tfALPHA8,
    tfLUMINANCE8,
    tfLUMINANCE8_ALPHA8,
    tfINTENSITY8,
    tfRGB8,
    tfRGBA_FLOAT16,
    tfRGBA_FLOAT32);

var
  vGLTextureImageClasses: TList;
  vTGraphicFileExtension: array of string;
  vTGraphicClass: array of TGraphicClass;

type
  TFriendlyImage = class(TVKBaseImage);


{$IFDEF VKS_REGIONS}{$REGION 'Helper functions'}{$ENDIF}

  // RegisterTGraphicClassFileExtension
  //

procedure RegisterTGraphicClassFileExtension(const extension: string;
  const aClass: TGraphicClass);
var
  n: Integer;
begin
  n := Length(vTGraphicFileExtension);
  SetLength(vTGraphicFileExtension, n + 1);
  SetLength(vTGraphicClass, n + 1);
  vTGraphicFileExtension[n] := LowerCase(extension);
  vTGraphicClass[n] := aClass;
end;

// CreateGraphicFromFile
//

function CreateGraphicFromFile(const FileName: string): TVKGraphic;
var
  i: Integer;
  ext: string;
  fs: TStream;
  GraphicClass: TGraphicClass;
  img: TImage;
begin
  Result := nil;
  if FileStreamExists(fileName) then
  begin
    GraphicClass := nil;
    ext := LowerCase(ExtractFileExt(FileName));
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
      { TODO -oPW : not enouch parameters in GraphicClass.Create(); }
      (*Result := GraphicClass.Create(img);*)
      try
        fs := CreateFileStream(FileName, fmOpenRead);
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

// RegisterTextureImageClass
//

procedure RegisterTextureImageClass(textureImageClass: TVKTextureImageClass);
begin
  if not Assigned(vGLTextureImageClasses) then
    vGLTextureImageClasses := TList.Create;
  vGLTextureImageClasses.Add(textureImageClass);
end;

// FindTextureImageClass
//

function FindTextureImageClass(const className: string): TVKTextureImageClass;
var
  i: Integer;
  tic: TVKTextureImageClass;
begin
  Result := nil;
  if Assigned(vGLTextureImageClasses) then
    for i := 0 to vGLTextureImageClasses.Count - 1 do
    begin
      tic := TVKTextureImageClass(vGLTextureImageClasses[i]);
      if tic.ClassName = className then
      begin
        Result := tic;
        Break;
      end;
    end;

end;

// FindTextureImageClassByFriendlyName
//

function FindTextureImageClassByFriendlyName(const friendlyName: string):
  TVKTextureImageClass;
var
  i: Integer;
  tic: TVKTextureImageClass;
begin
  Result := nil;
  if Assigned(vGLTextureImageClasses) then
    for i := 0 to vGLTextureImageClasses.Count - 1 do
    begin
      tic := TVKTextureImageClass(vGLTextureImageClasses[i]);
      if tic.FriendlyName = friendlyName then
      begin
        Result := tic;
        Break;
      end;
    end;
end;

// SetTextureImageClassesToStrings
//

procedure SetTextureImageClassesToStrings(aStrings: TStrings);
var
  i: Integer;
  tic: TVKTextureImageClass;
begin
  with aStrings do
  begin
    BeginUpdate;
    Clear;
    if Assigned(vGLTextureImageClasses) then
      for i := 0 to vGLTextureImageClasses.Count - 1 do
      begin
        tic := TVKTextureImageClass(vGLTextureImageClasses[i]);
        AddObject(tic.FriendlyName, TObject(Pointer(tic)));
      end;
    EndUpdate;
  end;
end;

// GetTextureImageClassesAsStrings
//

function GetTextureImageClassesAsStrings: TStrings;
begin
  Result := TStringList.Create;
  SetTextureImageClassesToStrings(Result);
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TVKTextureImage ------------------
// ------------------

{$IFDEF VKS_REGIONS}{$REGION 'TVKTextureImage'}{$ENDIF}

// Create
//

constructor TVKTextureImage.Create(AOwner: TPersistent);
begin
  inherited;
  FOwnerTexture := (AOwner as TVKTexture);
end;

// Destroy
//

destructor TVKTextureImage.Destroy;
begin
  inherited Destroy;
end;

// FriendlyDescription
//

class function TVKTextureImage.FriendlyDescription: string;
begin
  Result := FriendlyName;
end;

// Invalidate
//

procedure TVKTextureImage.Invalidate;
begin
  ReleaseBitmap32;
  NotifyChange(Self);
end;

// ReleaseBitmap32
//

procedure TVKTextureImage.ReleaseBitmap32;
begin
  // nothing here.
end;

// AsBitmap : Returns the TextureImage as a TBitmap
// WARNING: This Creates a new bitmap. Remember to free it, to prevent leaks.
// If possible, rather use AssignToBitmap.
//

function TVKTextureImage.AsBitmap: TVKBitmap;
begin
  result := self.GetBitmap32.Create32BitsBitmap;
end;

// AssignToBitmap
//

procedure TVKTextureImage.AssignToBitmap(aBitmap: TVKBitmap);
begin
  Self.GetBitmap32.AssignToBitmap(aBitmap);
end;

// NotifyChange
//

procedure TVKTextureImage.NotifyChange(Sender: TObject);
begin
  if Assigned(FOwnerTexture) then
  begin
    FOwnerTexture.FTextureHandle.NotifyChangesOfData;
    FOwnerTexture.FSamplerHandle.NotifyChangesOfData;
    // Check for texture target change
    GetTextureTarget;
    FOwnerTexture.NotifyChange(Self);
  end;
end;

// LoadFromFile
//

procedure TVKTextureImage.LoadFromFile(const fileName: string);
var
  buf: string;
begin
  if Assigned(FOnTextureNeeded) then
  begin
    buf := fileName;
    FOnTextureNeeded(Self, buf);
  end;
end;

// GetResourceFile
//

function TVKTextureImage.GetResourceName: string;
begin
  Result := FResourceFile;
end;

class function TVKTextureImage.IsSelfLoading: Boolean;
begin
  Result := False;
end;

procedure TVKTextureImage.LoadTexture(AInternalFormat: GLinternalFormat);
begin
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TVKBlankImage ------------------
// ------------------

{$IFDEF VKS_REGIONS}{$REGION 'TVKBlankImage'}{$ENDIF}

// Create
//

constructor TVKBlankImage.Create(AOwner: TPersistent);
begin
  inherited;
  fWidth := 256;
  fHeight := 256;
  fDepth := 0;
  fColorFormat := GL_RGBA;
end;

// Destroy
//

destructor TVKBlankImage.Destroy;
begin
  ReleaseBitmap32;
  inherited Destroy;
end;

// Assign
//

procedure TVKBlankImage.Assign(Source: TPersistent);
var
  img: TVKBlankImage;
begin
  if Assigned(Source) then
  begin
    if (Source is TVKBlankImage) then
    begin
      img := Source as TVKBlankImage;
      FWidth := img.Width;
      FHeight := img.Height;
      FDepth := img.Depth;
      FCubeMap := img.fCubeMap;
      FArray := img.fArray;
      fColorFormat := img.ColorFormat;
      FResourceFile := img.ResourceName;
      Invalidate;
    end
    else
      GetBitmap32.Assign(Source);
    NotifyChange(Self);
  end
  else
    inherited;
end;

// SetWidth
//

procedure TVKBlankImage.SetWidth(val: Integer);
begin
  if val <> FWidth then
  begin
    FWidth := val;
    if FWidth < 1 then
      FWidth := 1;
    Invalidate;
  end;
end;

// GetWidth
//

function TVKBlankImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

// SetHeight
//

procedure TVKBlankImage.SetHeight(val: Integer);
begin
  if val <> FHeight then
  begin
    FHeight := val;
    if FHeight < 1 then
      FHeight := 1;
    Invalidate;
  end;
end;

// GetHeight
//

function TVKBlankImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

// SetDepth
//

procedure TVKBlankImage.SetDepth(val: Integer);
begin
  if val <> FDepth then
  begin
    FDepth := val;
    if FDepth < 0 then
      FDepth := 0;
    Invalidate;
  end;
end;

// GetDepth
//

function TVKBlankImage.GetDepth: Integer;
begin
  Result := fDepth;
end;

// SetCubeMap
//

procedure TVKBlankImage.SetCubeMap(const val: Boolean);
begin
  if val <> fCubeMap then
  begin
    fCubeMap := val;
    Invalidate;
  end;
end;

// SetArray
//

procedure TVKBlankImage.SetArray(const val: Boolean);
begin
  if val <> fArray then
  begin
    fArray := val;
    Invalidate;
  end;
end;

// GetBitmap32
//

function TVKBlankImage.GetBitmap32: TVKImage;
begin
  if not Assigned(FBitmap) then
  begin
    fBitmap := TVKImage.Create;
    fBitmap.Width := FWidth;
    fBitmap.Height := FHeight;
    fBitmap.Depth := FDepth;
    fBitmap.CubeMap := FCubeMap;
    fBitmap.TextureArray := FArray;
    fBitmap.SetColorFormatDataType(FColorFormat, GL_UNSIGNED_BYTE);
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//

procedure TVKBlankImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

// SaveToFile
//

procedure TVKBlankImage.SaveToFile(const fileName: string);
begin
  SaveAnsiStringToFile(fileName, AnsiString(
    '[BlankImage]'#13#10'Width=' + IntToStr(Width) +
    #13#10'Height=' + IntToStr(Height) +
    #13#10'Depth=' + IntToStr(Depth)));
end;

// LoadFromFile
//

procedure TVKBlankImage.LoadFromFile(const fileName: string);
var
  sl: TStringList;
  buf, temp: string;
begin
  buf := fileName;
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Self, buf);
  if FileExists(buf) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(buf, TEncoding.ASCII);
      FWidth := StrToInt(sl.Values['Width']);
      FHeight := StrToInt(sl.Values['Height']);
      temp := sl.Values['Depth'];
      if Length(temp) > 0 then
        FDepth := StrToInt(temp)
      else
        FDepth := 1;
    finally
      sl.Free;
    end;
  end
  else
  begin
    Assert(False, Format(strFailedOpenFile, [fileName]));
  end;
end;

// FriendlyName
//

class function TVKBlankImage.FriendlyName: string;
begin
  Result := 'Blank Image';
end;

// FriendlyDescription
//

class function TVKBlankImage.FriendlyDescription: string;
begin
  Result := 'Blank Image (Width x Height x Depth)';
end;

// GetTextureTarget
//

function TVKBlankImage.GetTextureTarget: TVKTextureTarget;
begin
  Result := ttTexture2D;
  // Choose a texture target
  if Assigned(fBitmap) then
  begin
    FWidth := fBitmap.Width;
    FHeight := fBitmap.Height;
    FDepth := fBitmap.Depth;
    FCubeMap := fBitmap.CubeMap;
    FArray := fBitmap.TextureArray;
  end;

  if FHeight = 1 then
    Result := ttTexture1D;
  if FCubeMap then
    Result := ttTextureCube;
  if FDepth > 0 then
    Result := ttTexture3D;
  if FArray then
  begin
    if FDepth < 2 then
      Result := ttTexture1DArray
    else
      Result := ttTexture2DArray;
    if FCubeMap then
      Result := ttTextureCubeArray;
  end;

  if Assigned(FOwnerTexture) then
  begin
    if ((FOwnerTexture.FTextureFormat >= tfFLOAT_R16)
      and (FOwnerTexture.FTextureFormat <= tfFLOAT_RGBA32)) then
      Result := ttTextureRect;
  end;
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TVKPictureImage ------------------
// ------------------

{$IFDEF VKS_REGIONS}{$REGION 'TVKPictureImage'}{$ENDIF}

// Create
//

constructor TVKPictureImage.Create(AOwner: TPersistent);
begin
  inherited;
end;

// Destroy
//

destructor TVKPictureImage.Destroy;
begin
  ReleaseBitmap32;
  FGLPicture.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVKPictureImage.Assign(Source: TPersistent);
var
  bmp: TVKBitmap;
begin
  if Assigned(Source) then
  begin
    if (Source is TVKPersistentImage) then
      Picture.Assign(TVKPersistentImage(Source).Picture)
    else if (Source is TVKGraphic) then
      Picture.Assign(Source)
    else if (Source is TVKPicture) then
      Picture.Assign(Source)
    else if (Source is TVKImage) then
    begin
      bmp := TVKImage(Source).Create32BitsBitmap;
      Picture.Bitmap := bmp;
      bmp.Free;
      FResourceFile := TVKImage(Source).ResourceName;
    end
    else
      inherited;
  end
  else
    inherited;
end;

// BeginUpdate
//

procedure TVKPictureImage.BeginUpdate;
begin
  Inc(FUpdateCounter);
  Picture.Bitmap.OnChange := nil;
end;

// EndUpdate
//

procedure TVKPictureImage.EndUpdate;
begin
  Assert(FUpdateCounter > 0, ClassName + ': Unbalanced Begin/EndUpdate');
  Dec(FUpdateCounter);
  Picture.Bitmap.OnChange := PictureChanged;
  if FUpdateCounter = 0 then
    PictureChanged(Picture);
end;

// GetHeight
//

function TVKPictureImage.GetHeight: Integer;
begin
  Result := Round(Picture.Height);
end;

// GetWidth
//

function TVKPictureImage.GetWidth: Integer;
begin
  Result := Round(Picture.Width);
end;

// GetDepth
//

function TVKPictureImage.GetDepth: Integer;
begin
  Result := 0;
end;

// GetBitmap32
//

function TVKPictureImage.GetBitmap32: TVKImage;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TVKImage.Create;
    // we need to deactivate OnChange, due to a "glitch" in some TGraphics,
    // for instance, TJPegImage triggers an OnChange when it is drawn...
    if Assigned(Picture.Bitmap) then
    begin
      if Assigned(Picture.Bitmap.OnChange) then
      begin
        Picture.Bitmap.OnChange := nil;
        try
          FBitmap.Assign(Picture.Bitmap);
        finally
          Picture.Bitmap.OnChange := PictureChanged;
        end;
      end
      else
        FBitmap.Assign(Picture.Bitmap);
    end
    else
      FBitmap.SetErrorImage;
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//

procedure TVKPictureImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

// PictureChanged
//

procedure TVKPictureImage.PictureChanged(Sender: TObject);
begin
  Invalidate;
end;

// GetPicture
//

function TVKPictureImage.GetPicture: TVKPicture;
begin
  if not Assigned(FGLPicture) then
  begin
   { TODO -oPW : E2035 Not enough actual parameters }
    (*FGLPicture := TVKPicture.Create;*)
    FGLPicture.Bitmap.OnChange := PictureChanged;
  end;
  Result := FGLPicture;
end;

// SetPicture
//

procedure TVKPictureImage.SetPicture(const aPicture: TVKPicture);
begin
  Picture.Assign(aPicture);
end;

// GetTextureTarget
//

function TVKPictureImage.GetTextureTarget: TVKTextureTarget;
begin
  Result := ttTexture2D;
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TVKPersistentImage ------------------
// ------------------

{$IFDEF VKS_REGIONS}{$REGION 'TVKPersistentImage'}{$ENDIF}

// Create
//

constructor TVKPersistentImage.Create(AOwner: TPersistent);
begin
  inherited;
end;

// Destroy
//

destructor TVKPersistentImage.Destroy;
begin
  inherited Destroy;
end;

// SaveToFile
//

procedure TVKPersistentImage.SaveToFile(const FileName: string);
begin
  Picture.Bitmap.SaveToFile(fileName);
  FResourceFile := FileName;
end;

// LoadFromFile
//

procedure TVKPersistentImage.LoadFromFile(const fileName: string);
var
  buf: string;
  gr: TVKGraphic;
begin
  buf := fileName;
  FResourceFile := fileName;
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Self, buf);
  if ApplicationFileIODefined then
  begin
    gr := CreateGraphicFromFile(buf);
    if Assigned(gr) then
    begin
      Picture.Bitmap := gr;
      gr.Free;
      Exit;
    end;
  end
  else if FileExists(buf) then
  begin
    Picture.Bitmap.LoadFromFile(buf);
    Exit;
  end;
  Picture.Bitmap := nil;
  raise ETexture.CreateFmt(strFailedOpenFile, [fileName]);
end;

// FriendlyName
//

class function TVKPersistentImage.FriendlyName: string;
begin
  Result := 'Persistent Image';
end;

// FriendlyDescription
//

class function TVKPersistentImage.FriendlyDescription: string;
begin
  Result := 'Image data is stored in its original format with other form resources,'
    + 'ie. in the DFM at design-time, and embedded in the EXE at run-time.';
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TVKPicFileImage ------------------
// ------------------

{$IFDEF VKS_REGIONS}{$REGION 'TVKPicFileImage'}{$ENDIF}

// Create
//

constructor TVKPicFileImage.Create(AOwner: TPersistent);
begin
  inherited;
end;

// Destroy
//

destructor TVKPicFileImage.Destroy;
begin
  inherited;
end;

// Assign
//

procedure TVKPicFileImage.Assign(Source: TPersistent);
begin
  if Source is TVKPicFileImage then
  begin
    FPictureFileName := TVKPicFileImage(Source).FPictureFileName;
    FResourceFile := TVKPicFileImage(Source).ResourceName;
  end
  else
    inherited;
end;

// SetPictureFileName
//

procedure TVKPicFileImage.SetPictureFileName(const val: string);
begin
  if val <> FPictureFileName then
  begin
    FPictureFileName := val;
    FResourceFile := val;
    FAlreadyWarnedAboutMissingFile := False;
    Invalidate;
  end;
end;

// Invalidate
//

procedure TVKPicFileImage.Invalidate;
begin
  Picture.OnClick := nil;
  try
    Picture.Assign(nil);
    FBitmap := nil;
  finally
    Picture.Bitmap.OnChange := PictureChanged;
  end;
  inherited;
end;

// GetHeight
//

function TVKPicFileImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

// GetWidth
//

function TVKPicFileImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

// GetDepth
//

function TVKPicFileImage.GetDepth: Integer;
begin
  Result := 0;
end;

// GetBitmap32
//

function TVKPicFileImage.GetBitmap32: TVKImage;
var
  buf: string;
  gr: TVKGraphic;
begin
  if (GetWidth <= 0) and (PictureFileName <> '') then
  begin
    Picture.Bitmap.OnChange := nil;
    try
      buf := PictureFileName;
      SetExeDirectory;
      if Assigned(FOnTextureNeeded) then
        FOnTextureNeeded(Self, buf);
      if FileStreamExists(buf) then
      begin
        gr := CreateGraphicFromFile(buf);
        Picture.Bitmap := gr;
        gr.Free;
      end
      else
      begin
        Picture.Bitmap := nil;
        if not FAlreadyWarnedAboutMissingFile then
        begin
          FAlreadyWarnedAboutMissingFile := True;
          GLOKMessageBox(Format(strFailedOpenFileFromCurrentDir, [PictureFileName, GetCurrentDir]),strError);
        end;
      end;
      Result := inherited GetBitmap32;
      FWidth := Result.Width;
      FHeight := Result.Height;
      Picture.Bitmap := nil;
    finally
      Picture.Bitmap.OnChange := PictureChanged;
    end;
  end
  else
    Result := inherited GetBitmap32;
end;

// SaveToFile
//

procedure TVKPicFileImage.SaveToFile(const fileName: string);
begin
  FResourceFile := fileName;
  SaveAnsiStringToFile(fileName, AnsiString(PictureFileName));
end;

// LoadFromFile
//

procedure TVKPicFileImage.LoadFromFile(const fileName: string);
var
  buf: string;
begin
  inherited;
  // attempt to autodetect if we are pointed to a file containing
  // a filename or directly to an image
  if SizeOfFile(fileName) < 512 then
  begin
    buf := string(LoadAnsiStringFromFile(fileName));
    if Pos(#0, buf) > 0 then
      PictureFileName := fileName
    else
      PictureFileName := buf;
  end
  else
    PictureFileName := fileName;
  FResourceFile := FPictureFileName;
end;

// FriendlyName
//

class function TVKPicFileImage.FriendlyName: string;
begin
  Result := 'PicFile Image';
end;

// FriendlyDescription
//

class function TVKPicFileImage.FriendlyDescription: string;
begin
  Result := 'Image data is retrieved from a file.';
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TVKCubeMapImage ------------------
// ------------------

{$IFDEF VKS_REGIONS}{$REGION 'TVKCubeMapImage'}{$ENDIF}

// Create
//

constructor TVKCubeMapImage.Create(AOwner: TPersistent);
var
  i: TVKCubeMapTarget;
begin
  inherited;
  for i := Low(FPicture) to High(FPicture) do
  begin
    { TODO -oPW : E2035 Not enough actual parameters }
    (*FPicture[i] := TVKPicture.Create();*)
    FPicture[i].Bitmap.OnChange := PictureChanged;
  end;
end;

// Destroy
//

destructor TVKCubeMapImage.Destroy;
var
  i: TVKCubeMapTarget;
begin
  ReleaseBitmap32;
  for i := Low(FPicture) to High(FPicture) do
    FPicture[i].Free;
  inherited Destroy;
end;

// Assign
//

procedure TVKCubeMapImage.Assign(Source: TPersistent);
var
  i: TVKCubeMapTarget;
begin
  if Assigned(Source) then
  begin
    if (Source is TVKCubeMapImage) then
    begin
      for i := Low(FPicture) to High(FPicture) do
        FPicture[i].Assign(TVKCubeMapImage(Source).FPicture[i]);
      Invalidate;
    end
    else
      inherited;
  end
  else
    inherited;
end;

// GetWidth
//

function TVKCubeMapImage.GetWidth: Integer;
begin
  Result := Round(FPicture[cmtPX].Width);
end;

// GetHeight
//

function TVKCubeMapImage.GetHeight: Integer;
begin
  Result := Round(FPicture[cmtPX].Height);
end;

// GetDepth
//

function TVKCubeMapImage.GetDepth: Integer;
begin
  Result := 0;
end;

// GetBitmap32
//

function TVKCubeMapImage.GetBitmap32: TVKImage;
var
  I: Integer;
  LImage: TVKImage;
begin
  if Assigned(FImage) then
    FImage.Free;
  LImage := TVKImage.Create;
  LImage.VerticalReverseOnAssignFromBitmap := True;

  try
    for I := 0 to 5 do
    begin
      FPicture[TVKCubeMapTarget(I)].Bitmap.OnChange := nil;
      try
        LImage.Assign(FPicture[TVKCubeMapTarget(I)].Bitmap);
        if not Assigned(FImage) then
        begin
          FImage := TVKImage.Create;
          FImage.Blank := True;
          FImage.Width := LImage.Width;
          FImage.Height := LImage.Height;
          FImage.SetColorFormatDataType(LImage.ColorFormat, LImage.DataType);
          FImage.CubeMap := True;
          FImage.Blank := False;
        end;
        Move(LImage.Data^, TFriendlyImage(FImage).GetLevelAddress(0, I)^, LImage.LevelSizeInByte[0]);
      finally
        FPicture[TVKCubeMapTarget(I)].OnClick := PictureChanged;
      end;
    end;
  finally
    LImage.Destroy;
  end;
  Result := FImage;
end;

// ReleaseBitmap32
//

procedure TVKCubeMapImage.ReleaseBitmap32;
begin
  if Assigned(FImage) then
  begin
    FImage.Free;
    FImage := nil;
  end;
end;

// BeginUpdate
//

procedure TVKCubeMapImage.BeginUpdate;
var
  i: TVKCubeMapTarget;
begin
  Inc(FUpdateCounter);
  for i := Low(FPicture) to High(FPicture) do
    FPicture[i].Bitmap.OnChange := nil;
end;

// EndUpdate
//

procedure TVKCubeMapImage.EndUpdate;
var
  i: TVKCubeMapTarget;
begin
  Assert(FUpdateCounter > 0, ClassName + ': Unbalanced Begin/EndUpdate');
  Dec(FUpdateCounter);
  for i := Low(FPicture) to High(FPicture) do
    FPicture[i].Bitmap.OnChange := PictureChanged;
  if FUpdateCounter = 0 then
    PictureChanged(FPicture[cmtPX]);
end;

// SaveToFile
//

procedure TVKCubeMapImage.SaveToFile(const fileName: string);
var
  fs: TFileStream;
  bmp: TVKBitmap;
  i: TVKCubeMapTarget;
  version: Word;
begin
  fs := TFileStream.Create(fileName, fmCreate);
  bmp := TVKBitmap.Create;
  try
    version := $0100;
    fs.Write(version, 2);
    for i := Low(FPicture) to High(FPicture) do
    begin
      bmp.Assign(FPicture[i].Bitmap);
      bmp.SaveToStream(fs);
    end;
  finally
    bmp.Free;
    fs.Free;
  end;
end;

// LoadFromFile
//

procedure TVKCubeMapImage.LoadFromFile(const fileName: string);
var
  fs: TFileStream;
  bmp: TVKBitmap;
  i: TVKCubeMapTarget;
  version: Word;
begin
  fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyWrite);
  bmp := TVKBitmap.Create;
  try
    fs.Read(version, 2);
    Assert(version = $0100);
    for i := Low(FPicture) to High(FPicture) do
    begin
      bmp.LoadFromStream(fs);
      FPicture[i].Bitmap := bmp;
    end;
  finally
    bmp.Free;
    fs.Free;
  end;
end;

// FriendlyName
//

class function TVKCubeMapImage.FriendlyName: string;
begin
  Result := 'CubeMap Image';
end;

// FriendlyDescription
//

class function TVKCubeMapImage.FriendlyDescription: string;
begin
  Result := 'Image data is contain 6 pictures of cubemap faces.';
end;

// PictureChanged
//

procedure TVKCubeMapImage.PictureChanged(Sender: TObject);
begin
  Invalidate;
end;

// GetTextureTarget
//

function TVKCubeMapImage.GetTextureTarget: TVKTextureTarget;
begin
  Result := ttTextureCube;
end;

// SetPicture
//

procedure TVKCubeMapImage.SetPicture(index: TVKCubeMapTarget; const val:
  TVKPicture);
begin
  FPicture[index].Assign(val);
end;

// GetPicture
//

function TVKCubeMapImage.GetPicture(index: TVKCubeMapTarget): TVKPicture;
begin
  Result := FPicture[index];
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TVKTexture ------------------
// ------------------

{$IFDEF VKS_REGIONS}{$REGION 'TVKTexture'}{$ENDIF}

// Create
//

constructor TVKTexture.Create(AOwner: TPersistent);
begin
  inherited;
  FDisabled := True;
  FImage := TVKPersistentImage.Create(Self);
  FImage.OnTextureNeeded := DoOnTextureNeeded;
  FImageAlpha := tiaDefault;
  FImageBrightness := 1.0;
  FImageGamma := 1.0;
  FMagFilter := maLinear;
  FMinFilter := miLinearMipMapLinear;
  FFilteringQuality := tfIsotropic;
  FRequiredMemorySize := -1;
  FTextureHandle := TVKTextureHandle.Create;
  FSamplerHandle := TVKVirtualHandle.Create;
  FSamplerHandle.OnAllocate := OnSamplerAllocate;
  FSamplerHandle.OnDestroy := OnSamplerDestroy;
  FMappingMode := tmmUser;
  FEnvColor := TVKColor.CreateInitialized(Self, clrTransparent);
  FBorderColor := TVKColor.CreateInitialized(Self, clrTransparent);
  FNormalMapScale := cDefaultNormalMapScale;
  FTextureCompareMode := tcmNone;
  FTextureCompareFunc := cfLequal;
  FDepthTextureMode := dtmLuminance;
  TextureFormat := tfDefault;
  FCompression := tcDefault;
  FKeepImageAfterTransfer := False;
end;

// Destroy
//

destructor TVKTexture.Destroy;
begin
  FEnvColor.Free;
  FBorderColor.Free;
  FMapSCoordinates.Free;
  FMapTCoordinates.Free;
  FMapRCoordinates.Free;
  FMapQCoordinates.Free;
  DestroyHandles;
  FTextureHandle.Free;
  FSamplerHandle.Free;
  FImage.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVKTexture.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if (Source is TVKTexture) then
    begin
      if Source <> Self then
      begin
        FImageAlpha := TVKTexture(Source).FImageAlpha;
        FTextureMode := TVKTexture(Source).FTextureMode;
        FTextureWrap := TVKTexture(Source).FTextureWrap;
        FTextureFormat := TVKTexture(Source).FTextureFormat;
        FCompression := TVKTexture(Source).FCompression;
        FMinFilter := TVKTexture(Source).FMinFilter;
        FMagFilter := TVKTexture(Source).FMagFilter;
        FMappingMode := TVKTexture(Source).FMappingMode;
        MappingSCoordinates.Assign(TVKTexture(Source).MappingSCoordinates);
        MappingTCoordinates.Assign(TVKTexture(Source).MappingTCoordinates);
        MappingRCoordinates.Assign(TVKTexture(Source).MappingRCoordinates);
        MappingQCoordinates.Assign(TVKTexture(Source).MappingQCoordinates);
        FDisabled := TVKTexture(Source).FDisabled;
        SetImage(TVKTexture(Source).FImage);
        FImageBrightness := TVKTexture(Source).FImageBrightness;
        FImageGamma := TVKTexture(Source).FImageGamma;
        FFilteringQuality := TVKTexture(Source).FFilteringQuality;
        FEnvColor.Assign(TVKTexture(Source).FEnvColor);
        FBorderColor.Assign(TVKTexture(Source).FBorderColor);
        FNormalMapScale := TVKTexture(Source).FNormalMapScale;
        // Probably don't need to assign these....
        // FOnTextureNeeded := TVKTexture(Source).FImageGamma;
        // FRequiredMemorySize  : Integer;
        // FTexWidth, FTexHeight : Integer;
        FTextureHandle.NotifyChangesOfData;
        FSamplerHandle.NotifyChangesOfData;
      end;
    end
    else if (Source is TVKGraphic) then
      Image.Assign(Source)
    else if (Source is TVKPicture) then
      Image.Assign(TVKPicture(Source).Bitmap)
    else
      inherited Assign(Source);
  end
  else
  begin
    FDisabled := True;
    SetImage(nil);
    FTextureHandle.NotifyChangesOfData;
    FSamplerHandle.NotifyChangesOfData;
  end;
end;

// NotifyChange
//

procedure TVKTexture.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
  begin
    if Owner is TVKTextureExItem then
      TVKTextureExItem(Owner).NotifyChange(Self);
  end;
  if Sender is TVKTextureImage then
    FTextureHandle.NotifyChangesOfData;

  inherited;
end;

// NotifyImageChange
//

procedure TVKTexture.NotifyImageChange;
begin
  FTextureHandle.NotifyChangesOfData;
  NotifyChange(Self);
end;

// NotifyParamsChange
//

procedure TVKTexture.NotifyParamsChange;
begin
  FSamplerHandle.NotifyChangesOfData;
  NotifyChange(Self);
end;

// SetImage
//

procedure TVKTexture.SetImage(AValue: TVKTextureImage);
begin
  if Assigned(aValue) then
  begin
    if FImage.ClassType <> AValue.ClassType then
    begin
      FImage.Free;
      FImage := TVKTextureImageClass(AValue.ClassType).Create(Self);
      FImage.OnTextureNeeded := DoOnTextureNeeded;
    end;
    FImage.Assign(AValue);
  end
  else
  begin
    FImage.Free;
    FImage := TVKPersistentImage.Create(Self);
    FImage.OnTextureNeeded := DoOnTextureNeeded;
  end;
end;

// SetImageClassName
//

procedure TVKTexture.SetImageClassName(const val: string);
var
  newImage: TVKTextureImage;
  newImageClass: TVKTextureImageClass;
begin
  if val <> '' then
    if FImage.ClassName <> val then
    begin
      newImageClass := FindTextureImageClass(val);
      Assert(newImageClass <> nil, 'Make sure you include the unit for ' + val +
        ' in your uses clause');
      if newImageClass = nil then
        exit;
      newImage := newImageClass.Create(Self);
      newImage.OnTextureNeeded := DoOnTextureNeeded;
      FImage.Free;
      FImage := newImage;
    end;
end;

// GetImageClassName
//

function TVKTexture.GetImageClassName: string;
begin
  Result := FImage.ClassName;
end;

// TextureImageRequiredMemory
//

function TVKTexture.TextureImageRequiredMemory: Integer;
var
  w, h, e, levelSize: Integer;
begin
  if FRequiredMemorySize < 0 then
  begin
    if IsCompressedFormat(fTextureFormat) then
    begin
      w := (Image.Width + 3) div 4;
      h := (Image.Height + 3) div 4;
    end
    else
    begin
      w := Image.Width;
      h := Image.Height;
    end;

    e := GetTextureElementSize(fTextureFormat);
    FRequiredMemorySize := w * h * e;
    if Image.Depth > 0 then
      FRequiredMemorySize := FRequiredMemorySize * Image.Depth;

    if not (MinFilter in [miNearest, miLinear]) then
    begin
      levelSize := FRequiredMemorySize;
      while e < levelSize do
      begin
        levelSize := levelSize div 4;
        FRequiredMemorySize := FRequiredMemorySize + levelSize;
      end;
    end;

    if Image.NativeTextureTarget = ttTextureCube then
      FRequiredMemorySize := FRequiredMemorySize * 6;
  end;
  Result := FRequiredMemorySize;
end;

// SetImageAlpha
//

procedure TVKTexture.SetImageAlpha(const val: TVKTextureImageAlpha);
begin
  if FImageAlpha <> val then
  begin
    FImageAlpha := val;
    NotifyImageChange;
  end;
end;

// SetImageBrightness
//

procedure TVKTexture.SetImageBrightness(const val: Single);
begin
  if FImageBrightness <> val then
  begin
    FImageBrightness := val;
    NotifyImageChange;
  end;
end;

// StoreBrightness
//

function TVKTexture.StoreBrightness: Boolean;
begin
  Result := (FImageBrightness <> 1.0);
end;

// SetImageGamma
//

procedure TVKTexture.SetImageGamma(const val: Single);
begin
  if FImageGamma <> val then
  begin
    FImageGamma := val;
    NotifyImageChange;
  end;
end;

// StoreGamma
//

function TVKTexture.StoreGamma: Boolean;
begin
  Result := (FImageGamma <> 1.0);
end;

// SetMagFilter
//

procedure TVKTexture.SetMagFilter(AValue: TVKMagFilter);
begin
  if AValue <> FMagFilter then
  begin
    FMagFilter := AValue;
    NotifyParamsChange;
  end;
end;

// SetMinFilter
//

procedure TVKTexture.SetMinFilter(AValue: TVKMinFilter);
begin
  if AValue <> FMinFilter then
  begin
    FMinFilter := AValue;
    NotifyParamsChange;
  end;
end;

// SetTextureMode
//

procedure TVKTexture.SetTextureMode(AValue: TVKTextureMode);
begin
  if AValue <> FTextureMode then
  begin
    FTextureMode := AValue;
    NotifyParamsChange;
  end;
end;

// SetDisabled
//

procedure TVKTexture.SetDisabled(AValue: Boolean);
var
  intf: IVKTextureNotifyAble;
begin
  if AValue <> FDisabled then
  begin
    FDisabled := AValue;
    if Supports(Owner, IVKTextureNotifyAble, intf) then
      intf.NotifyTexMapChange(Self)
    else
      NotifyChange(Self);
  end;
end;

// SetEnabled
//

procedure TVKTexture.SetEnabled(const val: Boolean);
begin
  Disabled := not val;
end;

// GetEnabled
//

function TVKTexture.GetEnabled: Boolean;
begin
  Result := not Disabled;
end;

// SetEnvColor
//

procedure TVKTexture.SetEnvColor(const val: TVKColor);
begin
  FEnvColor.Assign(val);
  NotifyParamsChange;
end;

// SetBorederColor
//

procedure TVKTexture.SetBorderColor(const val: TVKColor);
begin
  FBorderColor.Assign(val);
  NotifyParamsChange;
end;

// SetNormalMapScale
//

procedure TVKTexture.SetNormalMapScale(const val: Single);
begin
  if val <> FNormalMapScale then
  begin
    FNormalMapScale := val;
    if TextureFormat = tfNormalMap then
      NotifyImageChange;
  end;
end;

// StoreNormalMapScale
//

function TVKTexture.StoreNormalMapScale: Boolean;
begin
  Result := (FNormalMapScale <> cDefaultNormalMapScale);
end;

// SetTextureWrap
//

procedure TVKTexture.SetTextureWrap(AValue: TVKTextureWrap);
begin
  if AValue <> FTextureWrap then
  begin
    FTextureWrap := AValue;
    NotifyParamsChange;
  end;
end;

// SetTextureWrapS
//

procedure TVKTexture.SetTextureWrapS(AValue: TVKSeparateTextureWrap);
begin
  if AValue <> FTextureWrapS then
  begin
    FTextureWrapS := AValue;
    NotifyParamsChange;
  end;
end;

// SetTextureWrapT
//

procedure TVKTexture.SetTextureWrapT(AValue: TVKSeparateTextureWrap);
begin
  if AValue <> FTextureWrapT then
  begin
    FTextureWrapT := AValue;
    NotifyParamsChange;
  end;
end;

// SetTextureWrapR
//

procedure TVKTexture.SetTextureWrapR(AValue: TVKSeparateTextureWrap);
begin
  if AValue <> FTextureWrapR then
  begin
    FTextureWrapR := AValue;
    NotifyParamsChange;
  end;
end;

// GetTextureFormat
//

function TVKTexture.GetTextureFormat: TVKTextureFormat;
var
  i: TVKTextureFormat;
begin
  if vDefaultTextureFormat = FTextureFormat then
  begin
    Result := tfDefault;
    Exit;
  end;
  for i := tfRGB to tfRGBAFloat32 do
  begin
    if cOldTextureFormatToInternalFormat[i] = FTextureFormat then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := tfExtended;
end;

// SetTextureFormat
//

procedure TVKTexture.SetTextureFormat(const val: TVKTextureFormat);
begin
  if val = tfDefault then
  begin
    FTextureFormat := vDefaultTextureFormat;
  end
  else if val < tfExtended then
  begin
    FTextureFormat := cOldTextureFormatToInternalFormat[val];
  end;
end;

// SetTextureFormat
//

procedure TVKTexture.SetTextureFormatEx(const val: GLinternalFormat);
begin
  if val <> FTextureFormat then
  begin
    FTextureFormat := val;
    NotifyImageChange;
  end;
end;

// StoreTextureFormatEx
//

function TVKTexture.StoreTextureFormatEx: Boolean;
begin
  Result := GetTextureFormat >= tfExtended;
end;

// SetCompression
//

procedure TVKTexture.SetCompression(const val: TVKTextureCompression);
begin
  if val <> FCompression then
  begin
    FCompression := val;
    NotifyParamsChange;
  end;
end;

// SetFilteringQuality
//

procedure TVKTexture.SetFilteringQuality(const val: TVKTextureFilteringQuality);
begin
  if val <> FFilteringQuality then
  begin
    FFilteringQuality := val;
    NotifyParamsChange;
  end;
end;

// SetMappingMode
//

procedure TVKTexture.SetMappingMode(const val: TVKTextureMappingMode);
var
  texMapChange: Boolean;
  intf: IVKTextureNotifyAble;
begin
  if val <> FMappingMode then
  begin
    texMapChange := ((val = tmmUser) and (FMappingMode <> tmmUser))
      or ((val = tmmUser) and (FMappingMode <> tmmUser));
    FMappingMode := val;
    if texMapChange then
    begin
      // when switching between texGen modes and user mode, the geometry
      // must be rebuilt in whole (to specify/remove texCoord data!)
      if Supports(Owner, IVKTextureNotifyAble, intf) then
        intf.NotifyTexMapChange(Self);
    end
    else
      NotifyChange(Self);
  end;
end;

// SetMappingSCoordinates
//

procedure TVKTexture.SetMappingSCoordinates(const val: TVKCoordinates4);
begin
  MappingSCoordinates.Assign(val);
end;

// GetMappingSCoordinates
//

function TVKTexture.GetMappingSCoordinates: TVKCoordinates4;
begin
  if not Assigned(FMapSCoordinates) then
    FMapSCoordinates := TVKCoordinates4.CreateInitialized(Self, XHmgVector,
      csVector);
  Result := FMapSCoordinates;
end;

// StoreMappingSCoordinates
//

function TVKTexture.StoreMappingSCoordinates: Boolean;
begin
  if Assigned(FMapSCoordinates) then
    Result := not VectorEquals(FMapSCoordinates.AsVector, XHmgVector)
  else
    Result := false;
end;

// SetMappingTCoordinates
//

procedure TVKTexture.SetMappingTCoordinates(const val: TVKCoordinates4);
begin
  MappingTCoordinates.Assign(val);
end;

// GetMappingTCoordinates
//

function TVKTexture.GetMappingTCoordinates: TVKCoordinates4;
begin
  if not Assigned(FMapTCoordinates) then
    FMapTCoordinates := TVKCoordinates4.CreateInitialized(Self, YHmgVector,
      csVector);
  Result := FMapTCoordinates;
end;

// StoreMappingTCoordinates
//

function TVKTexture.StoreMappingTCoordinates: Boolean;
begin
  if Assigned(FMapTCoordinates) then
    Result := not VectorEquals(FMapTCoordinates.AsVector, YHmgVector)
  else
    Result := false;
end;

// SetMappingRCoordinates
//

procedure TVKTexture.SetMappingRCoordinates(const val: TVKCoordinates4);
begin
  MappingRCoordinates.Assign(val);
end;

// GetMappingRCoordinates
//

function TVKTexture.GetMappingRCoordinates: TVKCoordinates4;
begin
  if not Assigned(FMapRCoordinates) then
    FMapRCoordinates := TVKCoordinates4.CreateInitialized(Self, ZHmgVector,
      csVector);
  Result := FMapRCoordinates;
end;

// StoreMappingRCoordinates
//

function TVKTexture.StoreMappingRCoordinates: Boolean;
begin
  if Assigned(FMapRCoordinates) then
    Result := not VectorEquals(FMapRCoordinates.AsVector, ZHmgVector)
  else
    Result := false;
end;

// SetMappingQCoordinates
//

procedure TVKTexture.SetMappingQCoordinates(const val: TVKCoordinates4);
begin
  MappingQCoordinates.Assign(val);
end;

// GetMappingQCoordinates
//

function TVKTexture.GetMappingQCoordinates: TVKCoordinates4;
begin
  if not Assigned(FMapQCoordinates) then
    FMapQCoordinates := TVKCoordinates4.CreateInitialized(Self, WHmgVector,
      csVector);
  Result := FMapQCoordinates;
end;

// StoreMappingQCoordinates
//

function TVKTexture.StoreMappingQCoordinates: Boolean;
begin
  if Assigned(FMapQCoordinates) then
    Result := not VectorEquals(FMapQCoordinates.AsVector, WHmgVector)
  else
    Result := false;
end;

// StoreImageClassName
//

function TVKTexture.StoreImageClassName: Boolean;
begin
  Result := (FImage.ClassName <> TVKPersistentImage.ClassName);
end;

// SetTextureCompareMode
//

procedure TVKTexture.SetTextureCompareMode(const val: TVKTextureCompareMode);
begin
  if val <> fTextureCompareMode then
  begin
    fTextureCompareMode := val;
    NotifyParamsChange;
  end;
end;

// SetTextureCompareFunc
//

procedure TVKTexture.SetTextureCompareFunc(const val: TVKDepthCompareFunc);
begin
  if val <> fTextureCompareFunc then
  begin
    fTextureCompareFunc := val;
    NotifyParamsChange;
  end;
end;

// SetDepthTextureMode
//

procedure TVKTexture.SetDepthTextureMode(const val: TVKDepthTextureMode);
begin
  if val <> fDepthTextureMode then
  begin
    fDepthTextureMode := val;
    NotifyParamsChange;
  end;
end;

// PrepareBuildList
//

procedure TVKTexture.PrepareBuildList;
begin
  GetHandle;
end;

// ApplyMappingMode
//

procedure TVKTexture.ApplyMappingMode;
var
  R_Dim: Boolean;
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
    tmmCubeMapReflection, tmmCubeMapCamera: if GL_ARB_texture_cube_map then
      begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
        glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glEnable(GL_TEXTURE_GEN_R);
      end;
    tmmCubeMapNormal, tmmCubeMapLight0: if GL_ARB_texture_cube_map then
      begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
        glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glEnable(GL_TEXTURE_GEN_R);
      end;
  else
    Assert(False);
  end;
end;

// ApplyMappingMode
//

procedure TVKTexture.UnApplyMappingMode;
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

// Apply
//

procedure TVKTexture.Apply(var rci: TVKRenderContextInfo);

  procedure SetCubeMapTextureMatrix;
  var
    m, mm: TMatrix;
  begin
    // compute model view matrix for proper viewing
    case MappingMode of
      tmmCubeMapReflection, tmmCubeMapNormal:
        begin
          m := rci.PipelineTransformation.ViewMatrix;
          NormalizeMatrix(m);
          TransposeMatrix(m);
          rci.VKStates.SetTextureMatrix(m);
        end;
      tmmCubeMapLight0:
        begin
          with TVKScene(rci.scene).Lights do
            if Count > 0 then
            begin
              m := TVKLightSource(Items[0]).AbsoluteMatrix;
              NormalizeMatrix(m);
              mm := rci.PipelineTransformation.ViewMatrix;
              NormalizeMatrix(mm);
              TransposeMatrix(mm);
              m := MatrixMultiply(m, mm);
              rci.VKStates.SetTextureMatrix(m);
            end;
        end;
      tmmCubeMapCamera:
        begin
          m.X := VectorCrossProduct(rci.cameraUp, rci.cameraDirection);
          m.Y := VectorNegate(rci.cameraDirection);
          m.Z := rci.cameraUp;
          m.W := WHmgPoint;
          mm := rci.PipelineTransformation.ViewMatrix;
          NormalizeMatrix(mm);
          TransposeMatrix(mm);
          m := MatrixMultiply(m, mm);
          rci.VKStates.SetTextureMatrix(m);
        end;
    end;
  end;
var
  H : GLuint;
begin
  // Multisample image do not work with FFP
  if (FTextureHandle.Target = ttTexture2DMultisample) or
    (FTextureHandle.Target = ttTexture2DMultisampleArray) then
    exit;

  H := Handle;
  if not Disabled and (H > 0) then
  begin
    with rci.VKStates do
    begin
      ActiveTexture := 0;
      TextureBinding[0, FTextureHandle.Target] := H;
      ActiveTextureEnabled[FTextureHandle.Target] := True;
    end;

    if not rci.VKStates.ForwardContext then
    begin
      if FTextureHandle.Target = ttTextureCube then
        SetCubeMapTextureMatrix;
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE,
        cTextureMode[FTextureMode]);
      glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
      ApplyMappingMode;
      xgl.MapTexCoordToMain;
    end;
  end
  else if not rci.VKStates.ForwardContext then
  begin // default
    xgl.MapTexCoordToMain;
  end;
end;

// UnApply
//

procedure TVKTexture.UnApply(var rci: TVKRenderContextInfo);
begin
  if not Disabled
    and not rci.VKStates.ForwardContext then
  begin
    // Multisample image do not work with FFP
    if FTextureHandle.Target in [ttNoShape, ttTexture2DMultisample, ttTexture2DMultisampleArray] then
      exit;
    with rci.VKStates do
    begin
      ActiveTexture := 0;
      ActiveTextureEnabled[FTextureHandle.Target] := False;
      if FTextureHandle.Target = ttTextureCube then
        ResetTextureMatrix;
    end;
    UnApplyMappingMode;
  end;
end;

// ApplyAsTexture2
//

procedure TVKTexture.ApplyAsTexture2(var rci: TVKRenderContextInfo; textureMatrix:
  PMatrix = nil);
begin
  ApplyAsTextureN(2, rci, textureMatrix);
end;

// UnApplyAsTexture2
//

procedure TVKTexture.UnApplyAsTexture2(var rci: TVKRenderContextInfo;
  reloadIdentityTextureMatrix: boolean);
begin
  UnApplyAsTextureN(2, rci, reloadIdentityTextureMatrix);
end;

// ApplyAsTextureN
//

procedure TVKTexture.ApplyAsTextureN(n: Integer; var rci: TVKRenderContextInfo;
  textureMatrix: PMatrix = nil);
var
  m: TMatrix;
begin
  if not Disabled then
  begin
    // Multisample image do not work with FFP
    if (FTextureHandle.Target = ttTexture2DMultisample) or
      (FTextureHandle.Target = ttTexture2DMultisampleArray) then
      exit;
    with rci.VKStates do
    begin
      ActiveTexture := n - 1;
      TextureBinding[n - 1, FTextureHandle.Target] := Handle;
      ActiveTextureEnabled[FTextureHandle.Target] := True;
      if Assigned(textureMatrix) then
        SetTextureMatrix(textureMatrix^)
      else if FTextureHandle.Target = ttTextureCube then
      begin
        m := rci.PipelineTransformation.ModelViewMatrix;
        NormalizeMatrix(m);
        TransposeMatrix(m);
        rci.VKStates.SetTextureMatrix(m);
      end;

      if not ForwardContext then
      begin
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
        glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
        ApplyMappingMode;
        ActiveTexture := 0;
      end;
    end;
  end;
end;

// UnApplyAsTextureN
//

procedure TVKTexture.UnApplyAsTextureN(n: Integer; var rci: TVKRenderContextInfo;
  reloadIdentityTextureMatrix: boolean);
begin
  if not rci.VKStates.ForwardContext then
  begin
    // Multisample image do not work with FFP
    if (FTextureHandle.Target = ttTexture2DMultisample) or
      (FTextureHandle.Target = ttTexture2DMultisampleArray) then
      exit;
    with rci.VKStates do
    begin
      ActiveTexture := n - 1;
      ActiveTextureEnabled[FTextureHandle.Target] := False;
      UnApplyMappingMode;
      if (FTextureHandle.Target = ttTextureCube) or reloadIdentityTextureMatrix then
        ResetTextureMatrix;
      ActiveTexture := 0;
    end;
  end;
end;

// AllocateHandle
//

function TVKTexture.AllocateHandle: GLuint;
var
  vTarget: TVKTextureTarget;
begin
  vTarget := Image.NativeTextureTarget;
  if (vTarget <> ttNoShape) and (FTextureHandle.Target <> vTarget) then
    FTextureHandle.DestroyHandle;

  Result := FTextureHandle.Handle;
  if Result = 0 then
  begin
    FTextureHandle.AllocateHandle;
    Result := FTextureHandle.Handle;
  end;
  if FTextureHandle.IsDataNeedUpdate then
  begin
    FTextureHandle.Target := vTarget;
    FSamplerHandle.NotifyChangesOfData;
  end;
  if FSamplerHandle.Handle = 0 then
    FSamplerHandle.AllocateHandle;

  // bind texture
  if (FTextureHandle.Target <> ttNoShape) and
    IsTargetSupported(FTextureHandle.Target) then
  begin
    if FSamplerHandle.IsDataNeedUpdate then
    begin
      with CurrentVKContext.VKStates do
        TextureBinding[ActiveTexture, FTextureHandle.Target] := Result;
      PrepareParams(DecodeTextureTarget(FTextureHandle.Target));
      FSamplerHandle.NotifyDataUpdated;
    end;
  end
  else
    Result := 0;
end;

// IsHandleAllocated
//

function TVKTexture.IsHandleAllocated: Boolean;
begin
  Result := (FTextureHandle.Handle <> 0);
end;

// GetHandle
//

function TVKTexture.GetHandle: GLuint;
var
  target: GLuint;
  LBinding: array[TVKTextureTarget] of GLuint;

  procedure StoreBindings;
  var
    t: TVKTextureTarget;
  begin
    with CurrentVKContext.VKStates do
    begin
      if TextureBinding[ActiveTexture, FTextureHandle.Target] = FTextureHandle.Handle then
        TextureBinding[ActiveTexture, FTextureHandle.Target] := 0;
      for t := Low(TVKTextureTarget) to High(TVKTextureTarget) do
        LBinding[t] := TextureBinding[ActiveTexture, t];
    end;
  end;

  procedure RestoreBindings;
  var
    t: TVKTextureTarget;
  begin
    with CurrentVKContext.VKStates do
      for t := Low(TVKTextureTarget) to High(TVKTextureTarget) do
        TextureBinding[ActiveTexture, t] := LBinding[t];
  end;

begin
  with CurrentVKContext.VKStates do
  begin
    StoreBindings;
    try
      Result := AllocateHandle;
      if FTextureHandle.IsDataNeedUpdate then
      begin
        FTextureHandle.NotifyDataUpdated;
        // Check supporting
        target := DecodeTextureTarget(Image.NativeTextureTarget);
        if not IsTargetSupported(target)
          or not IsFormatSupported(TextureFormatEx) then
        begin
          SetTextureErrorImage;
          target := GL_TEXTURE_2D;
        end;
        // Load images
        if not GL_EXT_direct_state_access then
          TextureBinding[ActiveTexture, FTextureHandle.Target] := Result;
        PrepareImage(target);
      end;
    finally
      RestoreBindings;
    end;
  end;
end;

// DestroyHandles
//

procedure TVKTexture.DestroyHandles;
begin
  FTextureHandle.DestroyHandle;
  FSamplerHandle.DestroyHandle;
  FRequiredMemorySize := -1;
end;

// IsFloatType
//

function TVKTexture.IsFloatType: Boolean;
begin
  Result := IsFloatFormat(TextureFormatEx);
end;

// VulkanTextureFormat
//

function TVKTexture.VulkanTextureFormat: Integer;
var
  texComp: TVKTextureCompression;
begin
  if GL_ARB_texture_compression then
  begin
    if Compression = tcDefault then
      if vDefaultTextureCompression = tcDefault then
        texComp := tcNone
      else
        texComp := vDefaultTextureCompression
    else
      texComp := Compression;
  end
  else
    texComp := tcNone;

  if IsFloatType then
    texComp := tcNone; // no compression support for float_type

  if (texComp <> tcNone) and (TextureFormat <= tfNormalMap) then
    with CurrentVKContext.VKStates do
    begin
      case texComp of
        tcStandard: TextureCompressionHint := hintDontCare;
        tcHighQuality: TextureCompressionHint := hintNicest;
        tcHighSpeed: TextureCompressionHint := hintFastest;
      else
        Assert(False);
      end;
      Result := CompressedInternalFormatToVulkan(TextureFormatEx);
    end
  else
    Result := InternalFormatToVulkanFormat(TextureFormatEx);
end;

// PrepareImage
//

procedure TVKTexture.PrepareImage(target: GLuint);
var
  bitmap32: TVKImage;
  texComp: TVKTextureCompression;
  glFormat: GLEnum;
begin
  if Image.IsSelfLoading then
  begin
    Image.LoadTexture(FTextureFormat);
  end
  else
  begin

    bitmap32 := Image.GetBitmap32;

    if (bitmap32 = nil) or bitmap32.IsEmpty then
      Exit;

    if TextureFormat = tfNormalMap then
      bitmap32.GrayScaleToNormalMap(NormalMapScale,
        TextureWrap in [twBoth, twHorizontal],
        TextureWrap in [twBoth, twVertical]);
    // prepare AlphaChannel
    case ImageAlpha of
      tiaDefault: ; // nothing to do
      tiaAlphaFromIntensity:
        bitmap32.SetAlphaFromIntensity;
      tiaSuperBlackTransparent:
        bitmap32.SetAlphaTransparentForColor($000000);
      tiaLuminance:
        bitmap32.SetAlphaFromIntensity;
      tiaLuminanceSqrt:
        begin
          bitmap32.SetAlphaFromIntensity;
          bitmap32.SqrtAlpha;
        end;
      tiaOpaque:
        bitmap32.SetAlphaToValue(255);
      tiaTopLeftPointColorTransparent:
        begin
          bitmap32.Narrow;
          bitmap32.SetAlphaTransparentForColor(bitmap32.Data^[0]);
        end;
      tiaInverseLuminance:
        begin
          bitmap32.SetAlphaFromIntensity;
          bitmap32.InvertAlpha;
        end;
      tiaInverseLuminanceSqrt:
        begin
          bitmap32.SetAlphaFromIntensity;
          bitmap32.SqrtAlpha;
          bitmap32.InvertAlpha;
        end;
      tiaBottomRightPointColorTransparent:
        begin
          bitmap32.Narrow;
          bitmap32.SetAlphaTransparentForColor(bitmap32.Data^[bitmap32.Width - 1]);
        end;
    else
      Assert(False);
    end;
    // apply brightness correction
    if FImageBrightness <> 1.0 then
      bitmap32.BrightnessCorrection(FImageBrightness);
    // apply gamma correction
    if FImageGamma <> 1.0 then
      bitmap32.GammaCorrection(FImageGamma);

    if GL_ARB_texture_compression
      and (TextureFormat <> tfExtended) then
    begin
      if Compression = tcDefault then
        if vDefaultTextureCompression = tcDefault then
          texComp := tcNone
        else
          texComp := vDefaultTextureCompression
      else
        texComp := Compression;
      if IsFloatType then
        texComp := tcNone;

    end
    else
      texComp := tcNone;

    if (texComp <> tcNone) and (TextureFormat <= tfNormalMap) then
      with CurrentVKContext.VKStates do
      begin
        case texComp of
          tcStandard: TextureCompressionHint := hintDontCare;
          tcHighQuality: TextureCompressionHint := hintNicest;
          tcHighSpeed: TextureCompressionHint := hintFastest;
        else
          Assert(False, strErrorEx + strUnknownType);
        end;
        glFormat := CompressedInternalFormatToVulkan(FTextureFormat);
      end
    else
      glFormat := InternalFormatToVulkanFormat(FTextureFormat);

    bitmap32.RegisterAsVulkanTexture(
      FTextureHandle,
      not (FMinFilter in [miNearest, miLinear]),
      glFormat,
      FTexWidth,
      FTexHeight,
      FTexDepth);
  end;

  if glGetError <> GL_NO_ERROR then
  begin
    ClearOpenGLError;
    SetTextureErrorImage;
  end
  else
  begin
    FRequiredMemorySize := -1;
    TextureImageRequiredMemory;
    if not IsDesignTime and not FKeepImageAfterTransfer then
      Image.ReleaseBitmap32;
  end;
end;

// PrepareParams
//

procedure TVKTexture.PrepareParams(target: GLuint);
const
  cTextureSWrap: array[twBoth..twHorizontal] of GLEnum =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, GL_REPEAT);
  cTextureTWrap: array[twBoth..twHorizontal] of GLEnum =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_REPEAT, GL_CLAMP_TO_EDGE);
  cTextureRWrap: array[twBoth..twHorizontal] of GLEnum =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_REPEAT, GL_CLAMP_TO_EDGE);
  cTextureSWrapOld: array[twBoth..twHorizontal] of GLEnum =
    (GL_REPEAT, GL_CLAMP, GL_CLAMP, GL_REPEAT);
  cTextureTWrapOld: array[twBoth..twHorizontal] of GLEnum =
    (GL_REPEAT, GL_CLAMP, GL_REPEAT, GL_CLAMP);
  cTextureMagFilter: array[maNearest..maLinear] of GLEnum =
    (GL_NEAREST, GL_LINEAR);
  cTextureMinFilter: array[miNearest..miLinearMipmapLinear] of GLEnum =
    (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_LINEAR);
  cFilteringQuality: array[tfIsotropic..tfAnisotropic] of Integer = (1, 2);
  cSeparateTextureWrap: array[twRepeat..twMirrorClampToBorder] of GLEnum =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER,
    GL_MIRRORED_REPEAT, GL_MIRROR_CLAMP_TO_EDGE_ATI, GL_MIRROR_CLAMP_TO_BORDER_EXT);
  cTextureCompareMode: array[tcmNone..tcmCompareRtoTexture] of GLEnum =
    (GL_NONE, GL_COMPARE_R_TO_TEXTURE);
  cDepthTextureMode: array[dtmLuminance..dtmAlpha] of GLEnum =
    (GL_LUMINANCE, GL_INTENSITY, GL_ALPHA);

var
  R_Dim: Boolean;
  lMinFilter: TVKMinFilter;
begin
  if (target = GL_TEXTURE_2D_MULTISAMPLE)
    or (target = GL_TEXTURE_2D_MULTISAMPLE_ARRAY) then
    Exit;

  R_Dim := GL_ARB_texture_cube_map or GL_EXT_texture3D;

  with CurrentVKContext.VKStates do
  begin
    UnpackAlignment := 1;
    UnpackRowLength := 0;
    UnpackSkipRows := 0;
    UnpackSkipPixels := 0;
  end;

  glTexParameterfv(target, GL_TEXTURE_BORDER_COLOR, FBorderColor.AsAddress);

  if (GL_VERSION_1_2 or GL_EXT_texture_edge_clamp) then
  begin
    if FTextureWrap = twSeparate then
    begin
      glTexParameteri(target, GL_TEXTURE_WRAP_S,
        cSeparateTextureWrap[FTextureWrapS]);
      glTexParameteri(target, GL_TEXTURE_WRAP_T,
        cSeparateTextureWrap[FTextureWrapT]);
      if R_Dim then
        glTexParameteri(target, GL_TEXTURE_WRAP_R,
          cSeparateTextureWrap[FTextureWrapR]);
    end
    else
    begin
      glTexParameteri(target, GL_TEXTURE_WRAP_S, cTextureSWrap[FTextureWrap]);
      glTexParameteri(target, GL_TEXTURE_WRAP_T, cTextureTWrap[FTextureWrap]);
      if R_Dim then
        glTexParameteri(target, GL_TEXTURE_WRAP_R, cTextureRWrap[FTextureWrap]);
    end;
  end
  else
  begin
    glTexParameteri(target, GL_TEXTURE_WRAP_S, cTextureSWrapOld[FTextureWrap]);
    glTexParameteri(target, GL_TEXTURE_WRAP_T, cTextureTWrapOld[FTextureWrap]);
  end;

  lMinFilter := FMinFilter;
  // Down paramenter to rectangular texture supported
  if (target = GL_TEXTURE_RECTANGLE)
    or not (GL_EXT_texture_lod or GL_SGIS_texture_lod) then
  begin
    if lMinFilter in [miNearestMipmapNearest, miNearestMipmapLinear] then
      lMinFilter := miNearest;
    if FMinFilter in [miLinearMipmapNearest, miLinearMipmapLinear] then
      lMinFilter := miLinear;
  end;

  glTexParameteri(target, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[lMinFilter]);
  glTexParameteri(target, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);

  if GL_EXT_texture_filter_anisotropic then
    glTexParameteri(target, GL_TEXTURE_MAX_ANISOTROPY_EXT,
      cFilteringQuality[FFilteringQuality]);

  if IsDepthFormat(fTextureFormat) then
  begin
    glTexParameteri(target, GL_TEXTURE_COMPARE_MODE,
      cTextureCompareMode[fTextureCompareMode]);
    glTexParameteri(target, GL_TEXTURE_COMPARE_FUNC,
      cGLComparisonFunctionToGLEnum[fTextureCompareFunc]);
    if not FTextureHandle.RenderingContext.VKStates.ForwardContext then
      glTexParameteri(target, GL_DEPTH_TEXTURE_MODE,
        cDepthTextureMode[fDepthTextureMode]);
  end;
end;

// DoOnTextureNeeded
//

procedure TVKTexture.DoOnTextureNeeded(Sender: TObject; var textureFileName:
  string);
begin
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Sender, textureFileName);
end;

procedure TVKTexture.OnSamplerAllocate(Sender: TVKVirtualHandle; var Handle: Cardinal);
begin
  Handle := 1;
end;

procedure TVKTexture.OnSamplerDestroy(Sender: TVKVirtualHandle; var Handle: Cardinal);
begin
  Handle := 0;
end;

procedure TVKTexture.SetTextureErrorImage;
var
  img: TVKImage;
begin
  img := TVKImage.Create;
  img.SetErrorImage;

  ImageClassName := TVKBlankImage.className;
  TVKBlankImage(Image).Assign(img);
  img.Free;

  MagFilter := maNearest;
  MinFilter := miNearest;
  TextureWrap := twBoth;
  MappingMode := tmmUser;
  Compression := tcNone;
  AllocateHandle;
end;


{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

// ---------------
// --------------- TVKTextureExItem ---------------
// ---------------

{$IFDEF VKS_REGIONS}{$REGION 'TVKTextureExItem'}{$ENDIF}

// Create
//

constructor TVKTextureExItem.Create(ACollection: TCollection);
begin
  inherited;

  FTexture := TVKTexture.Create(Self);
  FTextureOffset := TVKCoordinates.CreateInitialized(Self, NullHMGVector,
    csPoint);
  FTextureOffset.OnNotifyChange := OnNotifyChange;
  FTextureScale := TVKCoordinates.CreateInitialized(Self, XYZHmgVector,
    csPoint);
  FTextureScale.OnNotifyChange := OnNotifyChange;

  FTextureIndex := ID;
  FTextureMatrix := IdentityHMGMatrix;

  //DanB - hmmm, not very flexible code, assumes it's owned by a material,
  // that has a Texture property, but may need to re-implement it somehow
{  if ACollection is TVKTextureEx then
    if TVKTextureEx(ACollection).FOwner <> nil then
      FTexture.OnTextureNeeded := TVKTextureEx(ACollection).FOwner.Texture.OnTextureNeeded;
      }
end;

// Destroy
//

destructor TVKTextureExItem.Destroy;
begin
  FTexture.Free;
  FTextureOffset.Free;
  FTextureScale.Free;

  inherited;
end;



// QueryInterface
//

function TVKTextureExItem.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

// _AddRef
//
function TVKTextureExItem._AddRef: Integer; stdcall;
begin
  Result := -1; //ignore
end;

// _Release
//
function TVKTextureExItem._Release: Integer; stdcall;
begin
  Result := -1; //ignore
end;

// Assign
//

procedure TVKTextureExItem.Assign(Source: TPersistent);
begin
  if Source is TVKTextureExItem then
  begin
    Texture := TVKTextureExItem(Source).Texture;
    TextureIndex := TVKTextureExItem(Source).TextureIndex;
    TextureOffset := TVKTextureExItem(Source).TextureOffset;
    TextureScale := TVKTextureExItem(Source).TextureScale;
    NotifyChange(Self);
  end
  else
    inherited;
end;

// NotifyChange
//

procedure TVKTextureExItem.NotifyChange(Sender: TObject);
begin
  if Assigned(Collection) then
    TVKTextureEx(Collection).NotifyChange(Self);
end;

// Apply
//

procedure TVKTextureExItem.Apply(var rci: TVKRenderContextInfo);
begin
  FApplied := False;
  if FTexture.Enabled then
  begin
    rci.VKStates.ActiveTexture := FTextureIndex;
    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    if FTextureMatrixIsIdentity then
      glLoadIdentity
    else
      glLoadMatrixf(@FTextureMatrix.X.X);
    glMatrixMode(GL_MODELVIEW);
    rci.VKStates.ActiveTexture := 0;
    if FTextureIndex = 0 then
      FTexture.Apply(rci)
    else if FTextureIndex = 1 then
      FTexture.ApplyAsTexture2(rci, nil)
    else if FTextureIndex >= 2 then
      FTexture.ApplyAsTextureN(FTextureIndex + 1, rci, nil);
    FApplied := True;
  end;
end;

// UnApply
//

procedure TVKTextureExItem.UnApply(var rci: TVKRenderContextInfo);
begin
  if FApplied then
  begin
    if FTextureIndex = 0 then
      FTexture.UnApply(rci)
    else if FTextureIndex = 1 then
      FTexture.UnApplyAsTexture2(rci, false)
    else if FTextureIndex >= 2 then
      FTexture.UnApplyAsTextureN(FTextureIndex + 1, rci, false);
    rci.VKStates.ActiveTexture := FTextureIndex;
    glMatrixMode(GL_TEXTURE);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    rci.VKStates.ActiveTexture := 0;
    FApplied := False;
  end;
end;

// GetDisplayName
//

function TVKTextureExItem.GetDisplayName: string;
begin
  Result := Format('Tex [%d]', [FTextureIndex]);
end;

// GetOwner
//

function TVKTextureExItem.GetOwner: TPersistent;
begin
  Result := Collection;
end;

// NotifyTexMapChange
//

procedure TVKTextureExItem.NotifyTexMapChange(Sender: TObject);
var
  intf: IVKTextureNotifyAble;
begin
  if Supports(TObject(TVKTextureEx(Collection).FOwner), IVKTextureNotifyAble,
    intf) then
    intf.NotifyTexMapChange(Sender);
end;

// SetTexture
//

procedure TVKTextureExItem.SetTexture(const Value: TVKTexture);
begin
  FTexture.Assign(Value);
  NotifyChange(Self);
end;

// SetTextureIndex
//

procedure TVKTextureExItem.SetTextureIndex(const Value: Integer);
var
  temp: Integer;
begin
  temp := Value;
  if temp < 0 then
    temp := 0;
  if temp <> FTextureIndex then
  begin
    FTextureIndex := temp;
    NotifyChange(Self);
  end;
end;

// SetTextureOffset
//

procedure TVKTextureExItem.SetTextureOffset(const Value: TVKCoordinates);
begin
  FTextureOffset.Assign(Value);
  NotifyChange(Self);
end;

// SetTextureScale
//

procedure TVKTextureExItem.SetTextureScale(const Value: TVKCoordinates);
begin
  FTextureScale.Assign(Value);
  NotifyChange(Self);
end;

// CalculateTextureMatrix
//

procedure TVKTextureExItem.CalculateTextureMatrix;
begin
  if TextureOffset.Equals(NullHmgVector) and TextureScale.Equals(XYZHmgVector) then
    FTextureMatrixIsIdentity := True
  else
  begin
    FTextureMatrixIsIdentity := False;
    FTextureMatrix := CreateScaleAndTranslationMatrix(TextureScale.AsVector,
      TextureOffset.AsVector);
  end;
  NotifyChange(Self);
end;

// OnNotifyChange
//

procedure TVKTextureExItem.OnNotifyChange(Sender: TObject);
begin
  CalculateTextureMatrix;
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

// ---------------
// --------------- TVKTextureEx ---------------
// ---------------

{$IFDEF VKS_REGIONS}{$REGION 'TVKTextureEx'}{$ENDIF}

// Create
//

constructor TVKTextureEx.Create(AOwner: TVKUpdateAbleObject);
begin
  inherited Create(TVKTextureExItem);

  FOwner := AOwner;
end;

// NotifyChange
//

procedure TVKTextureEx.NotifyChange(Sender: TObject);
begin
  if Assigned(FOwner) then
    FOwner.NotifyChange(Self);
end;

// Apply
//

procedure TVKTextureEx.Apply(var rci: TVKRenderContextInfo);
var
  i, texUnits: Integer;
  units: Cardinal;
begin
  if not GL_ARB_multitexture then
    exit;

  units := 0;
  glGetIntegerv(GL_MAX_TEXTURE_UNITS, @texUnits);
  for i := 0 to Count - 1 do
  begin
    if Items[i].TextureIndex < texUnits then
    begin
      Items[i].Apply(rci);
      if Items[i].FApplied then
        if (Items[i].TextureIndex > 0) and (Items[i].Texture.MappingMode =
          tmmUser) then
          units := units or (1 shl Items[i].TextureIndex);
    end;
  end;
  if units > 0 then
    xgl.MapTexCoordToArbitraryAdd(units);
end;

// UnApply
//

procedure TVKTextureEx.UnApply(var rci: TVKRenderContextInfo);
var
  i: Integer;
begin
  if not GL_ARB_multitexture then
    exit;
  for i := 0 to Count - 1 do
    Items[i].UnApply(rci);
end;

// Add
//

function TVKTextureEx.Add: TVKTextureExItem;
begin
  Result := TVKTextureExItem(inherited Add);
end;

// Loaded
//

procedure TVKTextureEx.Loaded;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].CalculateTextureMatrix;
end;

// GetOwner
//

function TVKTextureEx.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// SetItems
//

procedure TVKTextureEx.SetItems(index: Integer; const Value: TVKTextureExItem);
begin
  inherited SetItem(index, Value);
end;

// GetItems
//

function TVKTextureEx.GetItems(index: Integer): TVKTextureExItem;
begin
  Result := TVKTextureExItem(inherited GetItem(index));
end;

// IsTextureEnabled
//

function TVKTextureEx.IsTextureEnabled(Index: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Self = nil then
    Exit;
  for i := 0 to Count - 1 do
    if Items[i].TextureIndex = Index then
      Result := Result or Items[i].Texture.Enabled;
end;

{$IFDEF VKS_REGIONS}{$ENDREGION}{$ENDIF}

initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterTextureImageClass(TVKBlankImage);
  RegisterTextureImageClass(TVKPersistentImage);
  RegisterTextureImageClass(TVKPicFileImage);
  RegisterTextureImageClass(TVKCubeMapImage);

  RegisterTGraphicClassFileExtension('.bmp', TVKBitmap);

finalization

  vGLTextureImageClasses.Free;
  vGLTextureImageClasses := nil;

end.
