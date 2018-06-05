//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Handles all the material + material library stuff.
}

unit VXS.Material;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Types,
  FMX.Dialogs,
  FMX.Graphics,

  VXS.OpenGL,
  VXS.VectorTypes,
  VXS.RenderContextInfo,
  VXS.BaseClasses,
  VXS.Context,
  VXS.Texture,
  VXS.Color,
  VXS.Coordinates,
  VXS.VectorGeometry,
  VXS.PersistentClasses,
  VXS.CrossPlatform,
  VXS.State,
  VXS.TextureFormat,
  VXS.Strings,
  VXS.ApplicationFileIO,
  VXS.Graphics,
  VXS.Utils,
  VXS.XOpenGL;

{$UNDEF USE_MULTITHREAD}

type
  TVXFaceProperties = class;
  TVXMaterial = class;
  TVXAbstractMaterialLibrary = class;
  TVXMaterialLibrary = class;

  // an interface for proper TVXLibMaterialNameProperty support
  IVXMaterialLibrarySupported = interface(IInterface)
    ['{8E442AF9-D212-4A5E-8A88-92F798BABFD1}']
    function GetMaterialLibrary: TVXAbstractMaterialLibrary;
  end;

  TVXAbstractLibMaterial = class;
  TVXLibMaterial = class;

  { Define VXShader style application relatively to a material.
    ssHighLevel: shader is applied before material application, and unapplied
         after material unapplication
    ssLowLevel: shader is applied after material application, and unapplied
         before material unapplication
    ssReplace: shader is applied in place of the material (and material
         is completely ignored) }
  TVXShaderStyle = (ssHighLevel, ssLowLevel, ssReplace);

  { Defines what to do if for some reason shader failed to initialize.
    fiaSilentdisable:          just disable it
    fiaRaiseHandledException:  raise an exception, and handle it right away
    (usefull, when debigging within Delphi)
    fiaRaiseStardardException: raises the exception with a string from this
    function GetStardardNotSupportedMessage
    fiaReRaiseException:       Re-raises the exception
    fiaGenerateEvent:          Handles the exception, but generates an event
    that user can respond to. For example, he can
    try to compile a substitude shader, or replace
    it by a material.
    Note: HandleFailedInitialization does *not*
    create this event, it is left to user shaders
    which may chose to override this procedure.
    Commented out, because not sure if this
    option should exist, let other generations of
    developers decide ;)
  }
  TVXShaderFailedInitAction = (fiaSilentDisable, fiaRaiseStandardException, fiaRaiseHandledException, fiaReRaiseException
    { ,fiaGenerateEvent } );

  { Generic, abstract shader class.
    Shaders are modeled here as an abstract material-altering entity with
    transaction-like behaviour. The base class provides basic context and user
    tracking, as well as setup/application facilities.
    Subclasses are expected to provide implementation for DoInitialize,
    DoApply, DoUnApply and DoFinalize. }
  TVXShader = class(TVXUpdateAbleComponent)
  private
    FEnabled: Boolean;
    FLibMatUsers: TList;
    FVirtualHandle: TVXVirtualHandle;
    FShaderStyle: TVXShaderStyle;
    FUpdateCount: Integer;
    FShaderActive: Boolean;
    FFailedInitAction: TVXShaderFailedInitAction;
  protected
    { Invoked once, before the first call to DoApply.
      The call happens with the OpenVX context being active. }
    procedure DoInitialize(var rci: TVXRenderContextInfo; Sender: TObject); virtual;
    { Request to apply the shader.
      Always followed by a DoUnApply when the shader is no longer needed. }
    procedure DoApply(var rci: TVXRenderContextInfo; Sender: TObject); virtual; abstract;
    { Request to un-apply the shader.
      Subclasses can assume the shader has been applied previously.
      Return True to request a multipass. }
    function DoUnApply(var rci: TVXRenderContextInfo): Boolean; virtual; abstract;
    { Invoked once, before the destruction of context or release of shader.
      The call happens with the OpenVX context being active. }
    procedure DoFinalize; virtual;
    function GetShaderInitialized: Boolean;
    procedure InitializeShader(var rci: TVXRenderContextInfo; Sender: TObject);
    procedure FinalizeShader;
    procedure OnVirtualHandleAllocate(Sender: TVXVirtualHandle; var handle: Cardinal);
    procedure OnVirtualHandleDestroy(Sender: TVXVirtualHandle; var handle: Cardinal);
    procedure SetEnabled(val: Boolean);
    property ShaderInitialized: Boolean read GetShaderInitialized;
    property ShaderActive: Boolean read FShaderActive;
    procedure RegisterUser(libMat: TVXLibMaterial);
    procedure UnRegisterUser(libMat: TVXLibMaterial);
    { Used by the DoInitialize procedure of descendant classes to raise errors. }
    procedure HandleFailedInitialization(const LastErrorMessage: string = ''); virtual;
    { May be this should be a function inside HandleFailedInitialization... }
    function GetStardardNotSupportedMessage: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Subclasses should invoke this function when shader properties are altered.
      This procedure can also be used to reset/recompile the shader. }
    procedure NotifyChange(Sender: TObject); override;
    procedure BeginUpdate;
    procedure EndUpdate;

    { Apply shader to OpenGL state machine. }
    procedure Apply(var rci: TVXRenderContextInfo; Sender: TObject);
    { UnApply shader.
      When returning True, the caller is expected to perform a multipass
      rendering by re-rendering then invoking UnApply again, until a
      "False" is returned. }
    function UnApply(var rci: TVXRenderContextInfo): Boolean;
    { Shader application style (default is ssLowLevel). }
    property ShaderStyle: TVXShaderStyle read FShaderStyle write FShaderStyle default ssLowLevel;
    procedure Assign(Source: TPersistent); override;
    { Defines if shader is supported by hardware/drivers.
      Default - always supported. Descendants are encouraged to override
      this function. }
    function ShaderSupported: Boolean; virtual;
    { Defines what to do if for some reason shader failed to initialize.
      Note, that in some cases it cannon be determined by just checking the
      required OpenVX extentions. You need to try to compile and link the
      shader - only at that stage you might catch an error }
    property FailedInitAction: TVXShaderFailedInitAction read FFailedInitAction write FFailedInitAction
      default fiaRaiseStandardException;
  published
    { Turns on/off shader application.
      Note that this only turns on/off the shader application, if the
      ShaderStyle is ssReplace, the material won't be applied even if
      the shader is disabled. }
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TVXShaderClass = class of TVXShader;

  TShininess = 0 .. 128;

  { Stores basic face lighting properties.
    The lighting is described with the standard ambient/diffuse/emission/specular
    properties that behave like those of most rendering tools.
    You also have control over shininess (governs specular lighting) and
    polygon mode (lines / fill). }
  TVXFaceProperties = class(TVXUpdateAbleObject)
  private
    FAmbient, FDiffuse, FSpecular, FEmission: TVXColor;
    FShininess: TShininess;
  protected
    procedure SetAmbient(AValue: TVXColor);
    procedure SetDiffuse(AValue: TVXColor);
    procedure SetEmission(AValue: TVXColor);
    procedure SetSpecular(AValue: TVXColor);
    procedure SetShininess(AValue: TShininess);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Apply(var rci: TVXRenderContextInfo; AFace: TVXCullFaceMode);
    procedure ApplyNoLighting(var rci: TVXRenderContextInfo; AFace: TVXCullFaceMode);
    procedure Assign(Source: TPersistent); override;
  published
    property Ambient: TVXColor read FAmbient write SetAmbient;
    property Diffuse: TVXColor read FDiffuse write SetDiffuse;
    property Emission: TVXColor read FEmission write SetEmission;
    property Shininess: TShininess read FShininess write SetShininess default 0;
    property Specular: TVXColor read FSpecular write SetSpecular;
  end;

  TVXDepthProperties = class(TVXUpdateAbleObject)
  private
    FDepthTest: Boolean;
    FDepthWrite: Boolean;
    FZNear, FZFar: Single;
    FCompareFunc: TVXDepthfunction;
    FDepthClamp: Boolean;
  protected
    procedure SetZNear(Value: Single);
    procedure SetZFar(Value: Single);
    procedure SetCompareFunc(Value: TVXDepthCompareFunc);
    procedure SetDepthTest(Value: Boolean);
    procedure SetDepthWrite(Value: Boolean);
    procedure SetDepthClamp(Value: Boolean);
    function StoreZNear: Boolean;
    function StoreZFar: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Apply(var rci: TVXRenderContextInfo);
    procedure Assign(Source: TPersistent); override;
  published
    { Specifies the mapping of the near clipping plane to
      window coordinates.  The initial value is 0. }
    property ZNear: Single read FZNear write SetZNear stored StoreZNear;
    { Specifies the mapping of the far clipping plane to
      window coordinates.  The initial value is 1. }
    property ZFar: Single read FZFar write SetZFar stored StoreZFar;
    { Specifies the function used to compare each
      incoming pixel depth value with the depth value present in
      the depth buffer. }
    property DepthCompareFunction: TVXDepthfunction read FCompareFunc write SetCompareFunc default cfLequal;
    { DepthTest enabling.
      When DepthTest is enabled, objects closer to the camera will hide
      farther ones (via use of Z-Buffering).
      When DepthTest is disabled, the latest objects drawn/rendered overlap
      all previous objects, whatever their distance to the camera.
      Even when DepthTest is enabled, objects may chose to ignore depth
      testing through the osIgnoreDepthBuffer of their ObjectStyle property. }
    property DepthTest: Boolean read FDepthTest write SetDepthTest default True;
    { If True, object will not write to Z-Buffer. }
    property DepthWrite: Boolean read FDepthWrite write SetDepthWrite default False;
    { Enable clipping depth to the near and far planes }
    property DepthClamp: Boolean read FDepthClamp write SetDepthClamp default False;
  end;

  TVXLibMaterialName = string;

  (* If you write smth like af_GL_NEVER = GL_NEVER in the definition,
    it won't show up in the Dephi 7 design-time editor. So I had to add
    vTGlAlphaFuncValues and vTVXBlendFuncFactorValues arrays. *)
  TGlAlphaFunc = TVXComparisonFunction;

  TVXBlendingParameters = class(TVXUpdateAbleObject)
  private
    FUseAlphaFunc: Boolean;
    FUseBlendFunc: Boolean;
    FSeparateBlendFunc: Boolean;
    FAlphaFuncType: TGlAlphaFunc;
    FAlphaFuncRef: TGLclampf;
    FBlendFuncSFactor: TVXBlendFunction;
    FBlendFuncDFactor: TVXBlendFunction;
    FAlphaBlendFuncSFactor: TVXBlendFunction;
    FAlphaBlendFuncDFactor: TVXBlendFunction;
    procedure SetUseAlphaFunc(const Value: Boolean);
    procedure SetUseBlendFunc(const Value: Boolean);
    procedure SetSeparateBlendFunc(const Value: Boolean);
    procedure SetAlphaFuncRef(const Value: TGLclampf);
    procedure SetAlphaFuncType(const Value: TGlAlphaFunc);
    procedure SetBlendFuncDFactor(const Value: TVXBlendFunction);
    procedure SetBlendFuncSFactor(const Value: TVXBlendFunction);
    procedure SetAlphaBlendFuncDFactor(const Value: TVXBlendFunction);
    procedure SetAlphaBlendFuncSFactor(const Value: TVXBlendFunction);
    function StoreAlphaFuncRef: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Apply(var rci: TVXRenderContextInfo);
  published
    property UseAlphaFunc: Boolean read FUseAlphaFunc write SetUseAlphaFunc default False;
    property AlphaFunctType: TGlAlphaFunc read FAlphaFuncType write SetAlphaFuncType default cfGreater;
    property AlphaFuncRef: TGLclampf read FAlphaFuncRef write SetAlphaFuncRef stored StoreAlphaFuncRef;

    property UseBlendFunc: Boolean read FUseBlendFunc write SetUseBlendFunc default True;
    property SeparateBlendFunc: Boolean read FSeparateBlendFunc write SetSeparateBlendFunc default False;
    property BlendFuncSFactor: TVXBlendFunction read FBlendFuncSFactor write SetBlendFuncSFactor default bfSrcAlpha;
    property BlendFuncDFactor: TVXBlendFunction read FBlendFuncDFactor write SetBlendFuncDFactor default bfOneMinusSrcAlpha;
    property AlphaBlendFuncSFactor: TVXBlendFunction read FAlphaBlendFuncSFactor write SetAlphaBlendFuncSFactor
      default bfSrcAlpha;
    property AlphaBlendFuncDFactor: TVXBlendFunction read FAlphaBlendFuncDFactor write SetAlphaBlendFuncDFactor
      default bfOneMinusSrcAlpha;
  end;

  { Simplified blending options.
    bmOpaque : disable blending
    bmTransparency : uses standard alpha blending
    bmAdditive : activates additive blending (with saturation)
    bmAlphaTest50 : uses opaque blending, with alpha-testing at 50% (full
    transparency if alpha is below 0.5, full opacity otherwise)
    bmAlphaTest100 : uses opaque blending, with alpha-testing at 100%
    bmModulate : uses modulation blending
    bmCustom : uses TVXBlendingParameters options }
  TBlendingMode = (bmOpaque, bmTransparency, bmAdditive, bmAlphaTest50, bmAlphaTest100, bmModulate, bmCustom);

  TFaceCulling = (fcBufferDefault, fcCull, fcNoCull);

  { Control special rendering options for a material.
    moIgnoreFog : fog is deactivated when the material is rendered }
  TMaterialOption = (moIgnoreFog, moNoLighting);
  TMaterialOptions = set of TMaterialOption;

  { Describes a rendering material.
    A material is basicly a set of face properties (front and back) that take
    care of standard material rendering parameters (diffuse, ambient, emission
    and specular) and texture mapping.
    An instance of this class is available for almost all objects in GLScene
    to allow quick definition of material properties. It can link to a
    TVXLibMaterial (taken for a material library).
    The TVXLibMaterial has more adavanced properties (like texture transforms)
    and provides a standard way of sharing definitions and texture maps. }
  TVXMaterial = class(TVXUpdateAbleObject, IVXMaterialLibrarySupported, IVXNotifyAble, IVXTextureNotifyAble)
  private
    FFrontProperties, FBackProperties: TVXFaceProperties;
    FDepthProperties: TVXDepthProperties;
    FBlendingMode: TBlendingMode;
    FBlendingParams: TVXBlendingParameters;
    FTexture: TVXTexture;
    FTextureEx: TVXTextureEx;
    FMaterialLibrary: TVXAbstractMaterialLibrary;
    FLibMaterialName: TVXLibMaterialName;
    FMaterialOptions: TMaterialOptions;
    FFaceCulling: TFaceCulling;
    FPolygonMode: TVXPolygonMode;
    currentLibMaterial: TVXAbstractLibMaterial;
    (* Implementing IVXMaterialLibrarySupported. *)
    function GetMaterialLibrary: TVXAbstractMaterialLibrary;
  protected
    function GetBackProperties: TVXFaceProperties;
    procedure SetBackProperties(Values: TVXFaceProperties);
    procedure SetFrontProperties(Values: TVXFaceProperties);
    procedure SetDepthProperties(Values: TVXDepthProperties);
    procedure SetBlendingMode(const val: TBlendingMode);
    procedure SetMaterialOptions(const val: TMaterialOptions);
    function GetTexture: TVXTexture;
    procedure SetTexture(ATexture: TVXTexture);
    procedure SetMaterialLibrary(const val: TVXAbstractMaterialLibrary);
    procedure SetLibMaterialName(const val: TVXLibMaterialName);
    procedure SetFaceCulling(const val: TFaceCulling);
    procedure SetPolygonMode(AValue: TVXPolygonMode);
    function GetTextureEx: TVXTextureEx;
    procedure SetTextureEx(const Value: TVXTextureEx);
    function StoreTextureEx: Boolean;
    procedure SetBlendingParams(const Value: TVXBlendingParameters);
    procedure NotifyLibMaterialDestruction;
    // Back, Front, Texture and blending not stored if linked to a LibMaterial
    function StoreMaterialProps: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure PrepareBuildList;
    procedure Apply(var rci: TVXRenderContextInfo);
    { Restore non-standard material states that were altered;
      A return value of True is a multipass request. }
    function UnApply(var rci: TVXRenderContextInfo): Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure NotifyTexMapChange(Sender: TObject);
    procedure DestroyHandles;
    procedure Loaded;
    { Returns True if the material is blended.
      Will return the libmaterial's blending if it is linked to a material  library. }
    function Blended: Boolean;
    // True if the material has a secondary texture
    function HasSecondaryTexture: Boolean;
    // True if the material comes from the library instead of the texture property
    function MaterialIsLinkedToLib: Boolean;
    // Gets the primary texture either from material library or the texture property
    function GetActualPrimaryTexture: TVXTexture;
    // Gets the primary Material either from material library or the texture property
    function GetActualPrimaryMaterial: TVXMaterial;
    // Return the LibMaterial (see LibMaterialName)
    function GetLibMaterial: TVXLibMaterial;
    procedure QuickAssignMaterial(const MaterialLibrary: TVXMaterialLibrary; const Material: TVXLibMaterial);
  published
    property BackProperties: TVXFaceProperties read GetBackProperties write SetBackProperties stored StoreMaterialProps;
    property FrontProperties: TVXFaceProperties read FFrontProperties write SetFrontProperties stored StoreMaterialProps;
    property DepthProperties: TVXDepthProperties read FDepthProperties write SetDepthProperties stored StoreMaterialProps;
    property BlendingMode: TBlendingMode read FBlendingMode write SetBlendingMode stored StoreMaterialProps default bmOpaque;
    property BlendingParams: TVXBlendingParameters read FBlendingParams write SetBlendingParams;
    property MaterialOptions: TMaterialOptions read FMaterialOptions write SetMaterialOptions default [];
    property Texture: TVXTexture read GetTexture write SetTexture stored StoreMaterialProps;
    property FaceCulling: TFaceCulling read FFaceCulling write SetFaceCulling default fcBufferDefault;
    property MaterialLibrary: TVXAbstractMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property LibMaterialName: TVXLibMaterialName read FLibMaterialName write SetLibMaterialName;
    property TextureEx: TVXTextureEx read GetTextureEx write SetTextureEx stored StoreTextureEx;
    property PolygonMode: TVXPolygonMode read FPolygonMode write SetPolygonMode default pmFill;
  end;

  TVXAbstractLibMaterial = class(TCollectionItem, IVXMaterialLibrarySupported, IVXNotifyAble)
  protected
    FUserList: TList;
    FName: TVXLibMaterialName;
    FNameHashKey: Integer;
    FTag: Integer;
    FNotifying: Boolean; // used for recursivity protection
    // implementing IVXMaterialLibrarySupported
    function GetMaterialLibrary: TVXAbstractMaterialLibrary;
    // implementing IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    function GetDisplayName: string; override;
    class function ComputeNameHashKey(const name: string): Integer;
    procedure SetName(const val: TVXLibMaterialName);
    procedure Loaded; virtual; abstract;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Apply(var ARci: TVXRenderContextInfo); virtual; abstract;
    // Restore non-standard material states that were altered
    function UnApply(var ARci: TVXRenderContextInfo): Boolean; virtual; abstract;
    procedure RegisterUser(Obj: TVXUpdateAbleObject); overload;
    procedure UnRegisterUser(Obj: TVXUpdateAbleObject); overload;
    procedure RegisterUser(comp: TVXUpdateAbleComponent); overload;
    procedure UnRegisterUser(comp: TVXUpdateAbleComponent); overload;
    procedure RegisterUser(libMaterial: TVXLibMaterial); overload;
    procedure UnRegisterUser(libMaterial: TVXLibMaterial); overload;
    procedure NotifyUsers;
    function IsUsed: Boolean; // returns true if the texture has registed users
    property NameHashKey: Integer read FNameHashKey;
    procedure NotifyChange(Sender: TObject); virtual;
    function Blended: Boolean; virtual;
    property MaterialLibrary: TVXAbstractMaterialLibrary read GetMaterialLibrary;
  published
    property Name: TVXLibMaterialName read FName write SetName;
    property Tag: Integer read FTag write FTag;
  end;

  { Material in a material library.
    Introduces Texture transformations (offset and scale). Those transformations
    are available only for lib materials to minimize the memory cost of basic
    materials (which are used in almost all objects). }
  TVXLibMaterial = class(TVXAbstractLibMaterial, IVXTextureNotifyAble)
  private
    FMaterial: TVXMaterial;
    FTextureOffset, FTextureScale: TVXCoordinates;
    FTextureRotate: Single;
    FTextureMatrixIsIdentity: Boolean;
    FTextureOverride: Boolean;
    FTextureMatrix: TMatrix;
    FTexture2Name: TVXLibMaterialName;
    FShader: TVXShader;
    libMatTexture2: TVXLibMaterial; // internal cache
  protected
    procedure Loaded; override;
    procedure SetMaterial(const val: TVXMaterial);
    procedure SetTextureOffset(const val: TVXCoordinates);
    procedure SetTextureScale(const val: TVXCoordinates);
    procedure SetTextureMatrix(const Value: TMatrix);
    procedure SetTexture2Name(const val: TVXLibMaterialName);
    procedure SetShader(const val: TVXShader);
    procedure SetTextureRotate(Value: Single);
    function StoreTextureRotate: Boolean;
    procedure CalculateTextureMatrix;
    procedure DestroyHandles;
    procedure DoOnTextureNeeded(Sender: TObject; var textureFileName: string);
    procedure OnNotifyChange(Sender: TObject);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure PrepareBuildList;
    procedure Apply(var ARci: TVXRenderContextInfo); override;
    // Restore non-standard material states that were altered
    function UnApply(var ARci: TVXRenderContextInfo): Boolean; override;
    procedure NotifyUsersOfTexMapChange;
    property TextureMatrix: TMatrix read FTextureMatrix write SetTextureMatrix;
    property TextureMatrixIsIdentity: Boolean read FTextureMatrixIsIdentity;
    procedure NotifyTexMapChange(Sender: TObject);
    function Blended: Boolean; override;
  published
    property Material: TVXMaterial read FMaterial write SetMaterial;
    { Texture offset in texture coordinates.
      The offset is applied <i>after</i> scaling. }
    property TextureOffset: TVXCoordinates read FTextureOffset write SetTextureOffset;
    { Texture coordinates scaling.
      Scaling is applied <i>before</i> applying the offset, and is applied
      to the texture coordinates, meaning that a scale factor of (2, 2, 2)
      will make your texture look twice <i>smaller</i>. }
    property TextureScale: TVXCoordinates read FTextureScale write SetTextureScale;
    property TextureRotate: Single read FTextureRotate write SetTextureRotate stored StoreTextureRotate;
    { Reference to the second texture.
      The referred LibMaterial *must* be in the same material library.
      Second textures are supported only through ARB multitexturing (ignored
      if not supported). }
    property Texture2Name: TVXLibMaterialName read FTexture2Name write SetTexture2Name;

    { Optionnal shader for the material. }
    property Shader: TVXShader read FShader write SetShader;
  end;

  TVXAbstractLibMaterials = class(TOwnedCollection)
  protected
    procedure Loaded;
    function GetMaterial(const AName: TVXLibMaterialName): TVXAbstractLibMaterial; inline;
  public
    function MakeUniqueName(const nameRoot: TVXLibMaterialName): TVXLibMaterialName; virtual;
  end;

  { A collection of materials, mainly used in material libraries. }
  TVXLibMaterials = class(TVXAbstractLibMaterials)
  protected
    procedure SetItems(index: Integer; const val: TVXLibMaterial);
    function GetItems(index: Integer): TVXLibMaterial;
    procedure DestroyHandles;
  public
    constructor Create(AOwner: TComponent);
    function Owner: TPersistent;
    function IndexOf(const Item: TVXLibMaterial): Integer;
    function Add: TVXLibMaterial;
    function FindItemID(ID: Integer): TVXLibMaterial;
    property Items[index: Integer]: TVXLibMaterial read GetItems write SetItems; default;
    function GetLibMaterialByName(const AName: TVXLibMaterialName): TVXLibMaterial;
    { Returns index of this Texture if it exists. }
    function GetTextureIndex(const Texture: TVXTexture): Integer;
    { Returns index of this Material if it exists. }
    function GetMaterialIndex(const Material: TVXMaterial): Integer;
    { Returns name of this Texture if it exists. }
    function GetNameOfTexture(const Texture: TVXTexture): TVXLibMaterialName;
    { Returns name of this Material if it exists. }
    function GetNameOfLibMaterial(const Material: TVXLibMaterial): TVXLibMaterialName;
    procedure PrepareBuildList;
    { Deletes all the unused materials in the collection.
      A material is considered unused if no other material or updateable object references it.
      WARNING: For this to work, objects that use the textuere, have to REGISTER to the texture. }
    procedure DeleteUnusedMaterials;
  end;

  TVXAbstractMaterialLibrary = class(TVXCadenceAbleComponent)
  protected
    FMaterials: TVXAbstractLibMaterials;
    FLastAppliedMaterial: TVXAbstractLibMaterial;
    FTexturePaths: string;
    FTexturePathList: TStringList;
    procedure SetTexturePaths(const val: string);
    property TexturePaths: string read FTexturePaths write SetTexturePaths;
    procedure Loaded; override;
  public
    procedure SetNamesToTStrings(AStrings: TStrings);
    { Applies the material of given name.
      Returns False if the material could not be found. ake sure this
      call is balanced with a corresponding UnApplyMaterial (or an
      assertion will be triggered in the destructor).
      If a material is already applied, and has not yet been unapplied,
      an assertion will be triggered. }
    function ApplyMaterial(const AName: string; var ARci: TVXRenderContextInfo): Boolean; virtual;
    { Un-applies the last applied material.
      Use this function in conjunction with ApplyMaterial.
      If no material was applied, an assertion will be triggered. }
    function UnApplyMaterial(var ARci: TVXRenderContextInfo): Boolean; virtual;
  end;

  { Stores a set of materials, to be used and shared by scene objects.
    Use a material libraries for storing commonly used materials, it provides
    an efficient way to share texture and material data among many objects,
    thus reducing memory needs and rendering time.
    Materials in a material library also feature advanced control properties
    like texture coordinates transforms. }
  TVXMaterialLibrary = class(TVXAbstractMaterialLibrary)
  private
    FDoNotClearMaterialsOnLoad: Boolean;
    FOnTextureNeeded: TVXTextureNeededEvent;
  protected
    function GetMaterials: TVXLibMaterials;
    procedure SetMaterials(const val: TVXLibMaterials);
    function StoreMaterials: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyHandles;
    procedure WriteToFiler(writer: TVirtualWriter);
    procedure ReadFromFiler(reader: TVirtualReader);
    procedure SaveToStream(aStream: TStream); virtual;
    procedure LoadFromStream(aStream: TStream); virtual;
    procedure AddMaterialsFromStream(aStream: TStream);
    { Save library content to a file.
      Recommended extension : .GLML
      Currently saves only texture, ambient, diffuse, emission
      and specular colors. }
    procedure SaveToFile(const fileName: string);
    procedure LoadFromFile(const fileName: string);
    procedure AddMaterialsFromFile(const fileName: string);
    { Add a "standard" texture material.
      "standard" means linear texturing mode with mipmaps and texture
      modulation mode with default-strength color components.
      If persistent is True, the image will be loaded persistently in memory
      (via a TVXPersistentImage), if false, it will be unloaded after upload
      to OpenVX (via TVXPicFileImage). }
    function AddTextureMaterial(const MaterialName, fileName: string; persistent: Boolean = True): TVXLibMaterial; overload;
    { Add a "standard" texture material.
      TVXGraphic based variant. }
    function AddTextureMaterial(const MaterialName: string; Graphic: TVXGraphic): TVXLibMaterial; overload;
    { Returns libMaterial of given name if any exists. }
    function LibMaterialByName(const AName: TVXLibMaterialName): TVXLibMaterial;
    { Returns Texture of given material's name if any exists. }
    function TextureByName(const LibMatName: TVXLibMaterialName): TVXTexture;
    { Returns name of texture if any exists. }
    function GetNameOfTexture(const Texture: TVXTexture): TVXLibMaterialName;
    { Returns name of Material if any exists. }
    function GetNameOfLibMaterial(const libMat: TVXLibMaterial): TVXLibMaterialName;
  published
    { The materials collection. }
    property Materials: TVXLibMaterials read GetMaterials write SetMaterials stored StoreMaterials;
    { This event is fired whenever a texture needs to be loaded from disk.
      The event is triggered before even attempting to load the texture,
      and before TexturePaths is used. }
    property OnTextureNeeded: TVXTextureNeededEvent read FOnTextureNeeded write FOnTextureNeeded;
    { Paths to lookup when attempting to load a texture.
      You can specify multiple paths when loading a texture, the separator
      being the semi-colon ';' character. Directories are looked up from
      first to last, the first file name match is used.
      The current directory is always implicit and checked last.
      Note that you can also use the OnTextureNeeded event to provide a
      filename. }
    property TexturePaths;
  end;

// ------------------------------------------------------------------------------
implementation
// ------------------------------------------------------------------------------

// ------------------
// ------------------ TVXFaceProperties ------------------
// ------------------
constructor TVXFaceProperties.Create(AOwner: TPersistent);
begin
  inherited;
  // OpenVX default colors
  FAmbient := TVXColor.CreateInitialized(Self, clrGray20);
  FDiffuse := TVXColor.CreateInitialized(Self, clrGray80);
  FEmission := TVXColor.Create(Self);
  FSpecular := TVXColor.Create(Self);
  FShininess := 0;
end;

destructor TVXFaceProperties.Destroy;
begin
  FAmbient.Free;
  FDiffuse.Free;
  FEmission.Free;
  FSpecular.Free;
  inherited Destroy;
end;

procedure TVXFaceProperties.Apply(var rci: TVXRenderContextInfo; aFace: TVXCullFaceMode);
begin
  with rci.VxStates do
  begin
    SetMaterialColors(aFace, Emission.Color, Ambient.Color, Diffuse.Color, Specular.Color, FShininess);
  end;
end;

procedure TVXFaceProperties.ApplyNoLighting(var rci: TVXRenderContextInfo; aFace: TVXCullFaceMode);
begin
  glColor4fv(Diffuse.AsAddress);
end;

procedure TVXFaceProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVXFaceProperties) then
  begin
    FAmbient.DirectColor := TVXFaceProperties(Source).Ambient.Color;
    FDiffuse.DirectColor := TVXFaceProperties(Source).Diffuse.Color;
    FEmission.DirectColor := TVXFaceProperties(Source).Emission.Color;
    FSpecular.DirectColor := TVXFaceProperties(Source).Specular.Color;
    FShininess := TVXFaceProperties(Source).Shininess;
    NotifyChange(Self);
  end;
end;

procedure TVXFaceProperties.SetAmbient(AValue: TVXColor);
begin
  FAmbient.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

procedure TVXFaceProperties.SetDiffuse(AValue: TVXColor);
begin
  FDiffuse.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

procedure TVXFaceProperties.SetEmission(AValue: TVXColor);
begin
  FEmission.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

procedure TVXFaceProperties.SetSpecular(AValue: TVXColor);
begin
  FSpecular.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

procedure TVXFaceProperties.SetShininess(AValue: TShininess);
begin
  if FShininess <> AValue then
  begin
    FShininess := AValue;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TVXDepthProperties ------------------
// ------------------

constructor TVXDepthProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDepthTest := True;
  FDepthWrite := False;
  FZNear := 0;
  FZFar := 1;
  FCompareFunc := cfLequal;
  FDepthClamp := False;
end;

procedure TVXDepthProperties.Apply(var rci: TVXRenderContextInfo);
begin
  with rci.VxStates do
  begin
    if FDepthTest and rci.bufferDepthTest then
      Enable(stDepthTest)
    else
      Disable(stDepthTest);
    DepthWriteMask := FDepthWrite;
    DepthFunc := FCompareFunc;
    SetDepthRange(FZNear, FZFar);
    if FDepthClamp then
      Enable(stDepthClamp)
    else
      Disable(stDepthClamp);
  end;
end;

procedure TVXDepthProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVXDepthProperties) then
  begin
    FDepthTest := TVXDepthProperties(Source).FDepthTest;
    FDepthWrite := TVXDepthProperties(Source).FDepthWrite;
    FZNear := TVXDepthProperties(Source).FZNear;
    FZFar := TVXDepthProperties(Source).FZFar;
    FCompareFunc := TVXDepthProperties(Source).FCompareFunc;
    NotifyChange(Self);
  end;
end;

procedure TVXDepthProperties.SetZNear(Value: Single);
begin
  Value := ClampValue(Value, 0, 1);
  if Value <> FZNear then
  begin
    FZNear := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXDepthProperties.SetZFar(Value: Single);
begin
  Value := ClampValue(Value, 0, 1);
  if Value <> FZFar then
  begin
    FZFar := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXDepthProperties.SetCompareFunc(Value: TVXDepthfunction);
begin
  if Value <> FCompareFunc then
  begin
    FCompareFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXDepthProperties.SetDepthTest(Value: Boolean);
begin
  if Value <> FDepthTest then
  begin
    FDepthTest := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXDepthProperties.SetDepthWrite(Value: Boolean);
begin
  if Value <> FDepthWrite then
  begin
    FDepthWrite := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXDepthProperties.SetDepthClamp(Value: Boolean);
begin
  if Value <> FDepthClamp then
  begin
    FDepthClamp := Value;
    NotifyChange(Self);
  end;
end;

function TVXDepthProperties.StoreZNear: Boolean;
begin
  Result := FZNear <> 0.0;
end;

function TVXDepthProperties.StoreZFar: Boolean;
begin
  Result := FZFar <> 1.0;
end;

// ------------------
// ------------------ TVXShader ------------------
// ------------------

// Create
//

constructor TVXShader.Create(AOwner: TComponent);
begin
  FLibMatUsers := TList.Create;
  FVirtualHandle := TVXVirtualHandle.Create;
  FVirtualHandle.OnAllocate := OnVirtualHandleAllocate;
  FVirtualHandle.OnDestroy := OnVirtualHandleDestroy;
  FShaderStyle := ssLowLevel;
  FEnabled := True;
  FFailedInitAction := fiaRaiseStandardException;
  inherited;
end;

// Destroy
//

destructor TVXShader.Destroy;
var
  i: Integer;
  list: TList;
begin
  FVirtualHandle.DestroyHandle;
  FinalizeShader;
  inherited;
  list := FLibMatUsers;
  FLibMatUsers := nil;
  for i := list.Count - 1 downto 0 do
    TVXLibMaterial(list[i]).Shader := nil;
  list.Free;
  FVirtualHandle.Free;
end;

// NotifyChange
//

procedure TVXShader.NotifyChange(Sender: TObject);
var
  i: Integer;
begin
  if FUpdateCount = 0 then
  begin
    for i := FLibMatUsers.Count - 1 downto 0 do
      TVXLibMaterial(FLibMatUsers[i]).NotifyUsers;
    FinalizeShader;
  end;
end;

// BeginUpdate
//

procedure TVXShader.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// EndUpdate
//

procedure TVXShader.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyChange(Self);
end;

// DoInitialize
//

procedure TVXShader.DoInitialize(var rci: TVXRenderContextInfo; Sender: TObject);
begin
  // nothing here
end;

// DoFinalize
//

procedure TVXShader.DoFinalize;
begin
  // nothing here
end;

// GetShaderInitialized
//

function TVXShader.GetShaderInitialized: Boolean;
begin
  Result := (FVirtualHandle.handle <> 0);
end;

// InitializeShader
//

procedure TVXShader.InitializeShader(var rci: TVXRenderContextInfo; Sender: TObject);
begin
  FVirtualHandle.AllocateHandle;
  if FVirtualHandle.IsDataNeedUpdate then
  begin
    DoInitialize(rci, Sender);
    FVirtualHandle.NotifyDataUpdated;
  end;
end;

// FinalizeShader
//

procedure TVXShader.FinalizeShader;
begin
  FVirtualHandle.NotifyChangesOfData;
  DoFinalize;
end;

// Apply
//

procedure TVXShader.Apply(var rci: TVXRenderContextInfo; Sender: TObject);
begin
{$IFNDEF USE_MULTITHREAD}
  Assert(not FShaderActive, 'Unbalanced shader application.');
{$ENDIF}
  // Need to check it twice, because shader may refuse to initialize
  // and choose to disable itself during initialization.
  if FEnabled then
    if FVirtualHandle.IsDataNeedUpdate then
      InitializeShader(rci, Sender);

  if FEnabled then
    DoApply(rci, Sender);

  FShaderActive := True;
end;

// UnApply
//

function TVXShader.UnApply(var rci: TVXRenderContextInfo): Boolean;
begin
{$IFNDEF USE_MULTITHREAD}
  Assert(FShaderActive, 'Unbalanced shader application.');
{$ENDIF}
  if Enabled then
  begin
    Result := DoUnApply(rci);
    if not Result then
      FShaderActive := False;
  end
  else
  begin
    FShaderActive := False;
    Result := False;
  end;
end;

// OnVirtualHandleDestroy
//

procedure TVXShader.OnVirtualHandleDestroy(Sender: TVXVirtualHandle; var handle: Cardinal);
begin
  handle := 0;
end;

// OnVirtualHandleAllocate
//

procedure TVXShader.OnVirtualHandleAllocate(Sender: TVXVirtualHandle; var handle: Cardinal);
begin
  handle := 1;
end;

// SetEnabled
//

procedure TVXShader.SetEnabled(val: Boolean);
begin
{$IFNDEF USE_MULTITHREAD}
  Assert(not FShaderActive, 'Shader is active.');
{$ENDIF}
  if val <> FEnabled then
  begin
    FEnabled := val;
    NotifyChange(Self);
  end;
end;

// RegisterUser
//

procedure TVXShader.RegisterUser(libMat: TVXLibMaterial);
var
  i: Integer;
begin
  i := FLibMatUsers.IndexOf(libMat);
  if i < 0 then
    FLibMatUsers.Add(libMat);
end;

// UnRegisterUser
//

procedure TVXShader.UnRegisterUser(libMat: TVXLibMaterial);
begin
  if Assigned(FLibMatUsers) then
    FLibMatUsers.Remove(libMat);
end;

// Assign
//

procedure TVXShader.Assign(Source: TPersistent);
begin
  if Source is TVXShader then
  begin
    FShaderStyle := TVXShader(Source).FShaderStyle;
    FFailedInitAction := TVXShader(Source).FFailedInitAction;
    Enabled := TVXShader(Source).FEnabled;
  end
  else
    inherited Assign(Source); // to the pit of doom ;)
end;

// Assign
//

function TVXShader.ShaderSupported: Boolean;
begin
  Result := True;
end;

// HandleFailedInitialization
//

procedure TVXShader.HandleFailedInitialization(const LastErrorMessage: string = '');
begin
  case FailedInitAction of
    fiaSilentDisable:
      ; // Do nothing ;)
    fiaRaiseHandledException:
      try
        raise EVXShaderException.Create(GetStardardNotSupportedMessage);
      except
      end;
    fiaRaiseStandardException:
      raise EVXShaderException.Create(GetStardardNotSupportedMessage);
    fiaReRaiseException:
      begin
        if LastErrorMessage <> '' then
          raise EVXShaderException.Create(LastErrorMessage)
        else
          raise EVXShaderException.Create(GetStardardNotSupportedMessage)
      end;
    // fiaGenerateEvent:; // Do nothing. Event creation is left up to user shaders
    // // which may choose to override this procedure.
  else
    Assert(False, strErrorEx + strUnknownType);
  end;
end;

// GetStardardNotSupportedMessage
//

function TVXShader.GetStardardNotSupportedMessage: string;
begin
  if Name <> '' then
    Result := 'Your hardware/driver doesn''t support shader "' + Name + '"!'
  else
    Result := 'Your hardware/driver doesn''t support shader "' + ClassName + '"!';
end;

// ----------------- TVXMaterial --------------------------------------------------

constructor TVXMaterial.Create(AOwner: TPersistent);
begin
  inherited;
  FFrontProperties := TVXFaceProperties.Create(Self);
  FTexture := nil; // AutoCreate
  FFaceCulling := fcBufferDefault;
  FPolygonMode := pmFill;
  FBlendingParams := TVXBlendingParameters.Create(Self);
  FDepthProperties := TVXDepthProperties.Create(Self)
end;

destructor TVXMaterial.Destroy;
begin
  if Assigned(currentLibMaterial) then
    currentLibMaterial.UnRegisterUser(Self);
  FBackProperties.Free;
  FFrontProperties.Free;
  FDepthProperties.Free;
  FTexture.Free;
  FTextureEx.Free;
  FBlendingParams.Free;
  inherited Destroy;
end;

function TVXMaterial.GetMaterialLibrary: TVXAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVXMaterial.SetBackProperties(Values: TVXFaceProperties);
begin
  BackProperties.Assign(Values);
  NotifyChange(Self);
end;

function TVXMaterial.GetBackProperties: TVXFaceProperties;
begin
  if not Assigned(FBackProperties) then
    FBackProperties := TVXFaceProperties.Create(Self);
  Result := FBackProperties;
end;

procedure TVXMaterial.SetFrontProperties(Values: TVXFaceProperties);
begin
  FFrontProperties.Assign(Values);
  NotifyChange(Self);
end;

procedure TVXMaterial.SetDepthProperties(Values: TVXDepthProperties);
begin
  FDepthProperties.Assign(Values);
  NotifyChange(Self);
end;

procedure TVXMaterial.SetBlendingMode(const val: TBlendingMode);
begin
  if val <> FBlendingMode then
  begin
    FBlendingMode := val;
    NotifyChange(Self);
  end;
end;

procedure TVXMaterial.SetMaterialOptions(const val: TMaterialOptions);
begin
  if val <> FMaterialOptions then
  begin
    FMaterialOptions := val;
    NotifyChange(Self);
  end;
end;

function TVXMaterial.GetTexture: TVXTexture;
begin
  if not Assigned(FTexture) then
    FTexture := TVXTexture.Create(Self);
  Result := FTexture;
end;

procedure TVXMaterial.SetTexture(ATexture: TVXTexture);
begin
  if Assigned(ATexture) then
    Texture.Assign(ATexture)
  else
    FreeAndNil(FTexture);
end;

procedure TVXMaterial.SetFaceCulling(const val: TFaceCulling);
begin
  if val <> FFaceCulling then
  begin
    FFaceCulling := val;
    NotifyChange(Self);
  end;
end;

procedure TVXMaterial.SetMaterialLibrary(const val: TVXAbstractMaterialLibrary);
begin
  FMaterialLibrary := val;
  SetLibMaterialName(LibMaterialName);
end;

procedure TVXMaterial.SetLibMaterialName(const val: TVXLibMaterialName);
var
  oldLibrary: TVXMaterialLibrary;

  function MaterialLoopFrom(curMat: TVXLibMaterial): Boolean;
  var
    loopCount: Integer;
  begin
    loopCount := 0;
    while Assigned(curMat) and (loopCount < 16) do
    begin
      with curMat.Material do
      begin
        if Assigned(oldLibrary) then
          curMat := oldLibrary.Materials.GetLibMaterialByName(LibMaterialName)
        else
          curMat := nil;
      end;
      Inc(loopCount)
    end;
    Result := (loopCount >= 16);
  end;

var
  newLibMaterial: TVXAbstractLibMaterial;
begin
  // locate new libmaterial
  if Assigned(FMaterialLibrary) then
    newLibMaterial := FMaterialLibrary.FMaterials.GetMaterial(val)
  else
    newLibMaterial := nil;

  // make sure new won't trigger an infinite loop
  if FMaterialLibrary is TVXMaterialLibrary then
  begin
    oldLibrary := TVXMaterialLibrary(FMaterialLibrary);
    if MaterialLoopFrom(TVXLibMaterial(newLibMaterial)) then
    begin
      if IsDesignTime then
        InformationDlg(Format(strCyclicRefMat, [val]))
      else
        ShowMessage(Format(strCyclicRefMat, [val]));
      exit;
    end;
  end;

  FLibMaterialName := val;
  // unregister if required
  if newLibMaterial <> currentLibMaterial then
  begin
    // unregister from old
    if Assigned(currentLibMaterial) then
      currentLibMaterial.UnRegisterUser(Self);
    currentLibMaterial := newLibMaterial;
    // register with new
    if Assigned(currentLibMaterial) then
      currentLibMaterial.RegisterUser(Self);
    NotifyTexMapChange(Self);
  end;
end;

function TVXMaterial.GetTextureEx: TVXTextureEx;
begin
  if not Assigned(FTextureEx) then
    FTextureEx := TVXTextureEx.Create(Self);
  Result := FTextureEx;
end;

procedure TVXMaterial.SetTextureEx(const Value: TVXTextureEx);
begin
  if Assigned(Value) or Assigned(FTextureEx) then
    TextureEx.Assign(Value);
end;

function TVXMaterial.StoreTextureEx: Boolean;
begin
  Result := (Assigned(FTextureEx) and (TextureEx.Count > 0));
end;

procedure TVXMaterial.SetBlendingParams(const Value: TVXBlendingParameters);
begin
  FBlendingParams.Assign(Value);
  NotifyChange(Self);
end;

procedure TVXMaterial.NotifyLibMaterialDestruction;
begin
  FMaterialLibrary := nil;
  FLibMaterialName := '';
  currentLibMaterial := nil;
end;

procedure TVXMaterial.Loaded;
begin
  inherited;
  if Assigned(FTextureEx) then
    TextureEx.Loaded;
end;

function TVXMaterial.StoreMaterialProps: Boolean;
begin
  Result := not Assigned(currentLibMaterial);
end;

procedure TVXMaterial.PrepareBuildList;
begin
  if Assigned(FTexture) and (not FTexture.Disabled) then
    FTexture.PrepareBuildList;
end;

procedure TVXMaterial.Apply(var rci: TVXRenderContextInfo);
begin
  if Assigned(currentLibMaterial) then
    currentLibMaterial.Apply(rci)
  else
    with rci.VxStates do
    begin
      Disable(stColorMaterial);
      PolygonMode := FPolygonMode;
      if FPolygonMode = pmLines then
        Disable(stLineStipple);

      // Lighting switch
      if (moNoLighting in MaterialOptions) or not rci.bufferLighting then
      begin
        Disable(stLighting);
        FFrontProperties.ApplyNoLighting(rci, cmFront);
      end
      else
      begin
        Enable(stLighting);
        FFrontProperties.Apply(rci, cmFront);
      end;

      // Apply FaceCulling and BackProperties (if needs be)
      case FFaceCulling of
        fcBufferDefault:
          begin
            if rci.bufferFaceCull then
              Enable(stCullFace)
            else
              Disable(stCullFace);
            BackProperties.Apply(rci, cmBack);
          end;
        fcCull:
          Enable(stCullFace);
        fcNoCull:
          begin
            Disable(stCullFace);
            BackProperties.Apply(rci, cmBack);
          end;
      end;
      // note: Front + Back with different PolygonMode are no longer supported.
      // Currently state cache just ignores back facing mode changes, changes to
      // front affect both front + back PolygonMode

      // Apply Blending mode
      if not rci.ignoreBlendingRequests then
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
              FBlendingParams.Apply(rci);
            end;
        end;

      // Fog switch
      if (moIgnoreFog in MaterialOptions) or not rci.bufferFog then
        Disable(stFog)
      else
        Enable(stFog);

      if not Assigned(FTextureEx) then
      begin
        if Assigned(FTexture) then
          FTexture.Apply(rci)
      end
      else
      begin
        if Assigned(FTexture) and not FTextureEx.IsTextureEnabled(0) then
          FTexture.Apply(rci)
        else if FTextureEx.Count > 0 then
          FTextureEx.Apply(rci);
      end;

      // Apply depth properties
      if not rci.ignoreDepthRequests then
        FDepthProperties.Apply(rci);
    end;
end;

function TVXMaterial.UnApply(var rci: TVXRenderContextInfo): Boolean;
begin
  if Assigned(currentLibMaterial) then
    Result := currentLibMaterial.UnApply(rci)
  else
  begin
    if Assigned(FTexture) and (not FTexture.Disabled) and (not FTextureEx.IsTextureEnabled(0)) then
      FTexture.UnApply(rci)
    else if Assigned(FTextureEx) then
      FTextureEx.UnApply(rci);
    Result := False;
  end;
end;

procedure TVXMaterial.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVXMaterial) then
  begin
    if Assigned(TVXMaterial(Source).FBackProperties) then
      BackProperties.Assign(TVXMaterial(Source).BackProperties)
    else
      FreeAndNil(FBackProperties);
    FFrontProperties.Assign(TVXMaterial(Source).FFrontProperties);
    FPolygonMode := TVXMaterial(Source).FPolygonMode;
    FBlendingMode := TVXMaterial(Source).FBlendingMode;
    FMaterialOptions := TVXMaterial(Source).FMaterialOptions;
    if Assigned(TVXMaterial(Source).FTexture) then
      Texture.Assign(TVXMaterial(Source).FTexture)
    else
      FreeAndNil(FTexture);
    FFaceCulling := TVXMaterial(Source).FFaceCulling;
    FMaterialLibrary := TVXMaterial(Source).MaterialLibrary;
    SetLibMaterialName(TVXMaterial(Source).LibMaterialName);
    TextureEx.Assign(TVXMaterial(Source).TextureEx);
    FDepthProperties.Assign(TVXMaterial(Source).DepthProperties);
    NotifyChange(Self);
  end
  else
    inherited;
end;

procedure TVXMaterial.NotifyChange(Sender: TObject);
var
  intf: IVXNotifyAble;
begin
  if Supports(Owner, IVXNotifyAble, intf) then
    intf.NotifyChange(Self);
end;

procedure TVXMaterial.NotifyTexMapChange(Sender: TObject);
var
  intf: IVXTextureNotifyAble;
begin
  if Supports(Owner, IVXTextureNotifyAble, intf) then
    intf.NotifyTexMapChange(Self)
  else
    NotifyChange(Self);
end;

procedure TVXMaterial.DestroyHandles;
begin
  if Assigned(FTexture) then
    FTexture.DestroyHandles;
end;

function TVXMaterial.Blended: Boolean;
begin
  if Assigned(currentLibMaterial) then
  begin

    Result := currentLibMaterial.Blended
  end
  else
    Result := not(BlendingMode in [bmOpaque, bmAlphaTest50, bmAlphaTest100, bmCustom]);
end;

function TVXMaterial.HasSecondaryTexture: Boolean;
begin
  Result := Assigned(currentLibMaterial) and (currentLibMaterial is TVXLibMaterial) and
    Assigned(TVXLibMaterial(currentLibMaterial).libMatTexture2);
end;

function TVXMaterial.MaterialIsLinkedToLib: Boolean;
begin
  Result := Assigned(currentLibMaterial);
end;

function TVXMaterial.GetActualPrimaryTexture: TVXTexture;
begin
  if Assigned(currentLibMaterial) and (currentLibMaterial is TVXLibMaterial) then
    Result := TVXLibMaterial(currentLibMaterial).Material.Texture
  else
    Result := Texture;
end;

function TVXMaterial.GetActualPrimaryMaterial: TVXMaterial;
begin
  if Assigned(currentLibMaterial) and (currentLibMaterial is TVXLibMaterial) then
    Result := TVXLibMaterial(currentLibMaterial).Material
  else
    Result := Self;
end;

function TVXMaterial.GetLibMaterial: TVXLibMaterial;
begin
  if Assigned(currentLibMaterial) and (currentLibMaterial is TVXLibMaterial) then
    Result := TVXLibMaterial(currentLibMaterial)
  else
    Result := nil;
end;

procedure TVXMaterial.QuickAssignMaterial(const MaterialLibrary: TVXMaterialLibrary; const Material: TVXLibMaterial);
begin
  FMaterialLibrary := MaterialLibrary;
  FLibMaterialName := Material.FName;

  if Material <> currentLibMaterial then
  begin
    // unregister from old
    if Assigned(currentLibMaterial) then
      currentLibMaterial.UnRegisterUser(Self);
    currentLibMaterial := Material;
    // register with new
    if Assigned(currentLibMaterial) then
      currentLibMaterial.RegisterUser(Self);

    NotifyTexMapChange(Self);
  end;
end;

procedure TVXMaterial.SetPolygonMode(AValue: TVXPolygonMode);
begin
  if AValue <> FPolygonMode then
  begin
    FPolygonMode := AValue;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TVXAbstractLibMaterial ------------------
// ------------------

constructor TVXAbstractLibMaterial.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FUserList := TList.Create;
  if Assigned(ACollection) then
  begin
    FName := TVXAbstractLibMaterials(ACollection).MakeUniqueName('LibMaterial');
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

destructor TVXAbstractLibMaterial.Destroy;
begin
  FUserList.Free;
  inherited Destroy;
end;

procedure TVXAbstractLibMaterial.Assign(Source: TPersistent);
begin
  if Source is TVXAbstractLibMaterial then
  begin
    FName := TVXLibMaterials(Collection).MakeUniqueName(TVXLibMaterial(Source).name);
    FNameHashKey := ComputeNameHashKey(FName);
  end
  else
    inherited; // Raise AssignError
end;

function TVXAbstractLibMaterial.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TVXAbstractLibMaterial._AddRef: Integer; stdcall;
begin
  Result := -1; // ignore
end;

function TVXAbstractLibMaterial._Release: Integer; stdcall;
begin
  Result := -1; // ignore
end;

procedure TVXAbstractLibMaterial.RegisterUser(Obj: TVXUpdateAbleObject);
begin
  Assert(FUserList.IndexOf(Obj) < 0);
  FUserList.Add(Obj);
end;

procedure TVXAbstractLibMaterial.UnRegisterUser(Obj: TVXUpdateAbleObject);
begin
  FUserList.Remove(Obj);
end;

procedure TVXAbstractLibMaterial.RegisterUser(comp: TVXUpdateAbleComponent);
begin
  Assert(FUserList.IndexOf(comp) < 0);
  FUserList.Add(comp);
end;

procedure TVXAbstractLibMaterial.UnRegisterUser(comp: TVXUpdateAbleComponent);
begin
  FUserList.Remove(comp);
end;

procedure TVXAbstractLibMaterial.RegisterUser(libMaterial: TVXLibMaterial);
begin
  Assert(FUserList.IndexOf(libMaterial) < 0);
  FUserList.Add(libMaterial);
end;

procedure TVXAbstractLibMaterial.UnRegisterUser(libMaterial: TVXLibMaterial);
begin
  FUserList.Remove(libMaterial);
end;

procedure TVXAbstractLibMaterial.NotifyChange(Sender: TObject);
begin
  NotifyUsers();
end;

procedure TVXAbstractLibMaterial.NotifyUsers;
var
  i: Integer;
  Obj: TObject;
begin
  if FNotifying then
    exit;
  FNotifying := True;
  try
    for i := 0 to FUserList.Count - 1 do
    begin
      Obj := TObject(FUserList[i]);
      if Obj is TVXUpdateAbleObject then
        TVXUpdateAbleObject(FUserList[i]).NotifyChange(Self)
      else if Obj is TVXUpdateAbleComponent then
        TVXUpdateAbleComponent(FUserList[i]).NotifyChange(Self)
      else
      begin
        Assert(Obj is TVXAbstractLibMaterial);
        TVXAbstractLibMaterial(FUserList[i]).NotifyUsers;
      end;
    end;
  finally
    FNotifying := False;
  end;
end;

function TVXAbstractLibMaterial.IsUsed: Boolean;
begin
  Result := Assigned(Self) and (FUserList.Count > 0);
end;

function TVXAbstractLibMaterial.GetDisplayName: string;
begin
  Result := Name;
end;

function TVXAbstractLibMaterial.GetMaterialLibrary: TVXAbstractMaterialLibrary;
var
  LOwner: TPersistent;
begin
  Result := nil;
  if Assigned(Collection) then
  begin
    LOwner := TVXAbstractLibMaterials(Collection).Owner;
    if LOwner is TVXAbstractMaterialLibrary then
      Result := TVXAbstractMaterialLibrary(LOwner);
  end;
end;

function TVXAbstractLibMaterial.Blended: Boolean;
begin
  Result := False;
end;

class function TVXAbstractLibMaterial.ComputeNameHashKey(const name: string): Integer;
var
  i, n: Integer;
begin
  n := Length(name);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(name[i]);
end;

procedure TVXAbstractLibMaterial.SetName(const val: TVXLibMaterialName);
begin
  if val <> FName then
  begin
    if not(csLoading in TComponent(Collection.Owner).ComponentState) then
    begin
      if TVXLibMaterials(Collection).GetLibMaterialByName(val) <> Self then
        FName := TVXLibMaterials(Collection).MakeUniqueName(val)
      else
        FName := val;
    end
    else
      FName := val;
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

// ------------------
// ------------------ TVXLibMaterial ------------------
// ------------------

constructor TVXLibMaterial.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMaterial := TVXMaterial.Create(Self);
  FMaterial.Texture.OnTextureNeeded := DoOnTextureNeeded;
  FTextureOffset := TVXCoordinates.CreateInitialized(Self, NullHmgVector, csPoint);
  FTextureOffset.OnNotifyChange := OnNotifyChange;
  FTextureScale := TVXCoordinates.CreateInitialized(Self, XYZHmgVector, csPoint);
  FTextureScale.OnNotifyChange := OnNotifyChange;
  FTextureRotate := 0;
  FTextureOverride := False;
  FTextureMatrixIsIdentity := True;
end;

destructor TVXLibMaterial.Destroy;
var
  i: Integer;
  matObj: TObject;
begin
  Shader := nil; // drop dependency
  Texture2Name := ''; // drop dependency
  for i := 0 to FUserList.Count - 1 do
  begin
    matObj := TObject(FUserList[i]);
    if matObj is TVXMaterial then
      TVXMaterial(matObj).NotifyLibMaterialDestruction
    else if matObj is TVXLibMaterial then
    begin
      TVXLibMaterial(matObj).libMatTexture2 := nil;
      TVXLibMaterial(matObj).FTexture2Name := '';
    end;
  end;
  FMaterial.Free;
  FTextureOffset.Free;
  FTextureScale.Free;
  inherited;
end;

procedure TVXLibMaterial.Assign(Source: TPersistent);
begin
  if Source is TVXLibMaterial then
  begin
    FMaterial.Assign(TVXLibMaterial(Source).Material);
    FTextureOffset.Assign(TVXLibMaterial(Source).TextureOffset);
    FTextureScale.Assign(TVXLibMaterial(Source).TextureScale);
    FTextureRotate := TVXLibMaterial(Source).TextureRotate;
    TextureMatrix := TVXLibMaterial(Source).TextureMatrix;
    FTextureOverride := TVXLibMaterial(Source).FTextureOverride;
    FTexture2Name := TVXLibMaterial(Source).Texture2Name;
    FShader := TVXLibMaterial(Source).Shader;
  end;
  inherited;
end;

function TVXLibMaterial.Blended: Boolean;
begin
  Result := Material.Blended;
end;

procedure TVXLibMaterial.PrepareBuildList;
begin
  if Assigned(Self) then
    Material.PrepareBuildList;
end;

procedure TVXLibMaterial.Apply(var ARci: TVXRenderContextInfo);
var
  multitextured: Boolean;
begin
  xglBeginUpdate;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssHighLevel:
        Shader.Apply(ARci, Self);
      ssReplace:
        begin
          Shader.Apply(ARci, Self);
          exit;
        end;
    end;
  end
  else
    ARci.VxStates.CurrentProgram := 0;
  if (Texture2Name <> '') and (not vSecondTextureUnitForbidden) then
  begin
    if not Assigned(libMatTexture2) then
    begin
      libMatTexture2 := TVXLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
      if Assigned(libMatTexture2) then
        libMatTexture2.RegisterUser(Self)
      else
        FTexture2Name := '';
    end;
    multitextured := Assigned(libMatTexture2) and (not libMatTexture2.Material.Texture.Disabled);
  end
  else
    multitextured := False;
  if not multitextured then
  begin
    // no multitexturing ("standard" mode)
    if not FTextureMatrixIsIdentity then
      ARci.VxStates.SetTextureMatrix(FTextureMatrix);
    Material.Apply(ARci);
  end
  else
  begin
    // multitexturing is ON
    if not FTextureMatrixIsIdentity then
      ARci.VxStates.SetTextureMatrix(FTextureMatrix);
    Material.Apply(ARci);

    if not libMatTexture2.FTextureMatrixIsIdentity then
      libMatTexture2.Material.Texture.ApplyAsTexture2(ARci, @libMatTexture2.FTextureMatrix.X.X)
    else
      libMatTexture2.Material.Texture.ApplyAsTexture2(ARci);

    if (not Material.Texture.Disabled) and (Material.Texture.MappingMode = tmmUser) then
      if libMatTexture2.Material.Texture.MappingMode = tmmUser then
        xglMapTexCoordToDual
      else
        xglMapTexCoordToMain
    else if libMatTexture2.Material.Texture.MappingMode = tmmUser then
      xglMapTexCoordToSecond
    else
      xglMapTexCoordToMain;

  end;

  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssLowLevel:
        Shader.Apply(ARci, Self);
    end;
  end;
  xglEndUpdate;
end;

function TVXLibMaterial.UnApply(var ARci: TVXRenderContextInfo): Boolean;
begin
  Result := False;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssLowLevel:
        Result := Shader.UnApply(ARci);
      ssReplace:
        begin
          Result := Shader.UnApply(ARci);
          exit;
        end;
    end;
  end;

  if not Result then
  begin
    if Assigned(libMatTexture2) and (not vSecondTextureUnitForbidden) then
    begin
      libMatTexture2.Material.Texture.UnApplyAsTexture2(ARci, (not libMatTexture2.TextureMatrixIsIdentity));
      xglMapTexCoordToMain;
    end;
    Material.UnApply(ARci);
    if not Material.Texture.Disabled then
      if not FTextureMatrixIsIdentity then
        ARci.VxStates.ResetTextureMatrix;
    if Assigned(FShader) then
    begin
      case Shader.ShaderStyle of
        ssHighLevel:
          Result := Shader.UnApply(ARci);
      end;
    end;
  end;
end;

procedure TVXLibMaterial.NotifyTexMapChange(Sender: TObject);
begin
  NotifyUsersOfTexMapChange();
end;

procedure TVXLibMaterial.NotifyUsersOfTexMapChange;
var
  i: Integer;
  Obj: TObject;
begin
  if FNotifying then
    exit;
  FNotifying := True;
  try
    for i := 0 to FUserList.Count - 1 do
    begin
      Obj := TObject(FUserList[i]);
      if Obj is TVXMaterial then
        TVXMaterial(FUserList[i]).NotifyTexMapChange(Self)
      else if Obj is TVXLibMaterial then
        TVXLibMaterial(FUserList[i]).NotifyUsersOfTexMapChange
      else if Obj is TVXUpdateAbleObject then
        TVXUpdateAbleObject(FUserList[i]).NotifyChange(Self)
      else if Obj is TVXUpdateAbleComponent then
        TVXUpdateAbleComponent(FUserList[i]).NotifyChange(Self);
    end;
  finally
    FNotifying := False;
  end;
end;

procedure TVXLibMaterial.Loaded;
begin
  CalculateTextureMatrix;
  Material.Loaded;
end;

procedure TVXLibMaterial.SetMaterial(const val: TVXMaterial);
begin
  FMaterial.Assign(val);
end;

procedure TVXLibMaterial.SetTextureOffset(const val: TVXCoordinates);
begin
  FTextureOffset.AsVector := val.AsVector;
  CalculateTextureMatrix;
end;

procedure TVXLibMaterial.SetTextureScale(const val: TVXCoordinates);
begin
  FTextureScale.AsVector := val.AsVector;
  CalculateTextureMatrix;
end;

procedure TVXLibMaterial.SetTextureMatrix(const Value: TMatrix);
begin
  FTextureMatrixIsIdentity := CompareMem(@Value.X, @IdentityHmgMatrix.X, SizeOf(TMatrix));
  FTextureMatrix := Value;
  FTextureOverride := True;
  NotifyUsers;
end;

procedure TVXLibMaterial.SetTextureRotate(Value: Single);
begin
  if Value <> FTextureRotate then
  begin
    FTextureRotate := Value;
    CalculateTextureMatrix;
  end;
end;

function TVXLibMaterial.StoreTextureRotate: Boolean;
begin
  Result := Abs(FTextureRotate) > EPSILON;
end;

procedure TVXLibMaterial.SetTexture2Name(const val: TVXLibMaterialName);
begin
  if val <> Texture2Name then
  begin
    if Assigned(libMatTexture2) then
    begin
      libMatTexture2.UnRegisterUser(Self);
      libMatTexture2 := nil;
    end;
    FTexture2Name := val;
    NotifyUsers;
  end;
end;

procedure TVXLibMaterial.SetShader(const val: TVXShader);
begin
  if val <> FShader then
  begin
    if Assigned(FShader) then
      FShader.UnRegisterUser(Self);
    FShader := val;
    if Assigned(FShader) then
      FShader.RegisterUser(Self);
    NotifyUsers;
  end;
end;

procedure TVXLibMaterial.CalculateTextureMatrix;
begin
  if TextureOffset.Equals(NullHmgVector) and TextureScale.Equals(XYZHmgVector) and not StoreTextureRotate then
    FTextureMatrixIsIdentity := True
  else
  begin
    FTextureMatrixIsIdentity := False;
    FTextureMatrix := CreateScaleAndTranslationMatrix(TextureScale.AsVector, TextureOffset.AsVector);
    if StoreTextureRotate then
      FTextureMatrix := MatrixMultiply(FTextureMatrix, CreateRotationMatrixZ(DegToRadian(FTextureRotate)));
  end;
  FTextureOverride := False;
  NotifyUsers;
end;

procedure TVXLibMaterial.DestroyHandles;
var
  libMat: TVXLibMaterial;
begin
  FMaterial.DestroyHandles;
  if FTexture2Name <> '' then
  begin
    libMat := TVXLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
    if Assigned(libMat) then
      libMat.DestroyHandles;
  end;
end;

procedure TVXLibMaterial.OnNotifyChange(Sender: TObject);
begin
  CalculateTextureMatrix;
end;

procedure TVXLibMaterial.DoOnTextureNeeded(Sender: TObject; var textureFileName: string);
var
  mLib: TVXMaterialLibrary;
  i: Integer;
  tryName: string;
begin
  if not Assigned(Collection) then
    exit;
  mLib := TVXMaterialLibrary((Collection as TVXLibMaterials).GetOwner);
  with mLib do
    if Assigned(FOnTextureNeeded) then
      FOnTextureNeeded(mLib, textureFileName);
  // if a ':' is present, or if it starts with a '\', consider it as an absolute path
  if (Pos(':', textureFileName) > 0) or (Copy(textureFileName, 1, 1) = PathDelim) then
    exit;
  // ok, not an absolute path, try given paths
  with mLib do
  begin
    if FTexturePathList <> nil then
      for i := 0 to FTexturePathList.Count - 1 do
      begin
        tryName := IncludeTrailingPathDelimiter(FTexturePathList[i]) + textureFileName;
        if (Assigned(vAFIOCreateFileStream) and FileStreamExists(tryName)) or FileExists(tryName) then
        begin
          textureFileName := tryName;
          Break;
        end;
      end;
  end;
end;

// ------------------
// ------------------ TVXLibMaterials ------------------
// ------------------

function TVXAbstractLibMaterials.GetMaterial(const AName: TVXLibMaterialName): TVXAbstractLibMaterial;
var
  i, hk: Integer;
  lm: TVXAbstractLibMaterial;
begin
  hk := TVXAbstractLibMaterial.ComputeNameHashKey(AName);
  for i := 0 to Count - 1 do
  begin
    lm := TVXAbstractLibMaterial(inherited Items[i]);
    if (lm.NameHashKey = hk) and (lm.name = AName) then
    begin
      Result := lm;
      exit;
    end;
  end;
  Result := nil;
end;


procedure TVXAbstractLibMaterials.Loaded;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    TVXAbstractLibMaterial(Items[i]).Loaded;
end;

function TVXAbstractLibMaterials.MakeUniqueName(const nameRoot: TVXLibMaterialName): TVXLibMaterialName;
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


constructor TVXLibMaterials.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TVXLibMaterial);
end;

procedure TVXLibMaterials.SetItems(index: Integer; const val: TVXLibMaterial);
begin
  inherited Items[index] := val;
end;

function TVXLibMaterials.GetItems(index: Integer): TVXLibMaterial;
begin
  Result := TVXLibMaterial(inherited Items[index]);
end;

procedure TVXLibMaterials.DestroyHandles;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].DestroyHandles;
end;

function TVXLibMaterials.Owner: TPersistent;
begin
  Result := GetOwner;
end;

function TVXLibMaterials.Add: TVXLibMaterial;
begin
  Result := (inherited Add) as TVXLibMaterial;
end;

function TVXLibMaterials.FindItemID(ID: Integer): TVXLibMaterial;
begin
  Result := (inherited FindItemID(ID)) as TVXLibMaterial;
end;

function TVXLibMaterials.GetLibMaterialByName(const AName: TVXLibMaterialName): TVXLibMaterial;
var
  LMaterial: TVXAbstractLibMaterial;
begin
  LMaterial := GetMaterial(AName);
  if Assigned(LMaterial) and (LMaterial is TVXLibMaterial) then
    Result := TVXLibMaterial(LMaterial)
  else
    Result := nil;
end;

function TVXLibMaterials.GetTextureIndex(const Texture: TVXTexture): Integer;
var
  i: Integer;
begin
  if Count <> 0 then
    for i := 0 to Count - 1 do
      if GetItems(i).Material.Texture = Texture then
      begin
        Result := i;
        exit;
      end;
  Result := -1;
end;

function TVXLibMaterials.GetMaterialIndex(const Material: TVXMaterial): Integer;
var
  i: Integer;
begin
  if Count <> 0 then
    for i := 0 to Count - 1 do
      if GetItems(i).Material = Material then
      begin
        Result := i;
        exit;
      end;
  Result := -1;
end;

function TVXLibMaterials.GetNameOfTexture(const Texture: TVXTexture): TVXLibMaterialName;
var
  MatIndex: Integer;
begin
  MatIndex := GetTextureIndex(Texture);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).name
  else
    Result := '';
end;

function TVXLibMaterials.GetNameOfLibMaterial(const Material: TVXLibMaterial): TVXLibMaterialName;
var
  MatIndex: Integer;
begin
  MatIndex := IndexOf(Material);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).name
  else
    Result := '';
end;

function TVXLibMaterials.IndexOf(const Item: TVXLibMaterial): Integer;
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

procedure TVXLibMaterials.PrepareBuildList;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TVXLibMaterial(inherited Items[i]).PrepareBuildList;
end;

procedure TVXLibMaterials.DeleteUnusedMaterials;
var
  i: Integer;
  gotNone: Boolean;
begin
  BeginUpdate;
  repeat
    gotNone := True;
    for i := Count - 1 downto 0 do
    begin
      if TVXLibMaterial(inherited Items[i]).FUserList.Count = 0 then
      begin
        TVXLibMaterial(inherited Items[i]).Free;
        gotNone := False;
      end;
    end;
  until gotNone;
  EndUpdate;
end;

procedure TVXAbstractMaterialLibrary.SetTexturePaths(const val: string);
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

function TVXAbstractMaterialLibrary.ApplyMaterial(const AName: string; var ARci: TVXRenderContextInfo): Boolean;
begin
  FLastAppliedMaterial := FMaterials.GetMaterial(AName);
  Result := Assigned(FLastAppliedMaterial);
  if Result then
    FLastAppliedMaterial.Apply(ARci);
end;

function TVXAbstractMaterialLibrary.UnApplyMaterial(var ARci: TVXRenderContextInfo): Boolean;
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

procedure TVXAbstractMaterialLibrary.SetNamesToTStrings(AStrings: TStrings);
var
  i: Integer;
  lm: TVXAbstractLibMaterial;
begin
  with AStrings do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to FMaterials.Count - 1 do
    begin
      lm := TVXAbstractLibMaterial(FMaterials.Items[i]);
      AddObject(lm.name, lm);
    end;
    EndUpdate;
  end;
end;

procedure TVXAbstractMaterialLibrary.Loaded;
begin
  inherited;
  FMaterials.Loaded;
end;

// ------------------
// ------------------ TVXMaterialLibrary ------------------
// ------------------

constructor TVXMaterialLibrary.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TVXLibMaterials.Create(Self);
end;

destructor TVXMaterialLibrary.Destroy;
begin
  Assert(FLastAppliedMaterial = nil, 'Unbalanced material application');
  FTexturePathList.Free;
  FMaterials.Free;
  FMaterials := nil;
  inherited;
end;

procedure TVXMaterialLibrary.DestroyHandles;
begin
  if Assigned(FMaterials) then
    Materials.DestroyHandles;
end;

procedure TVXMaterialLibrary.SetMaterials(const val: TVXLibMaterials);
begin
  FMaterials.Assign(val);
end;

function TVXMaterialLibrary.StoreMaterials: Boolean;
begin
  Result := (FMaterials.Count > 0);
end;

procedure TVXMaterialLibrary.WriteToFiler(writer: TVirtualWriter);
var
  i, j: Integer;
  libMat: TVXLibMaterial;
  tex: TVXTexture;
  img: TVXTextureImage;
  pim: TVXPersistentImage;
  ss: TStringStream;
  bmp: TBitmap;
  texExItem: TVXTextureExItem;
begin
  with writer do
  begin
    WriteInteger(4); // archive version 0, texture persistence only
    // archive version 1, libmat properties
    // archive version 2, Material.TextureEx properties
    // archive version 3, Material.Texture properties
    // archive version 4, Material.TextureRotate
    WriteInteger(Materials.Count);
    for i := 0 to Materials.Count - 1 do
    begin
      // version 0
      libMat := Materials[i];
      WriteString(libMat.name);
      tex := libMat.Material.Texture;
      img := tex.Image;
      pim := TVXPersistentImage(img);
      if tex.Enabled and (img is TVXPersistentImage) and (pim.Picture.Bitmap <> nil) then
      begin
        WriteBoolean(True);
        ss := TStringStream.Create('');
        try
          bmp := TBitmap.Create;
          try
            bmp.Assign(pim.Picture.Bitmap);
            bmp.SaveToStream(ss);
          finally
            bmp.Free;
          end;
          WriteString(ss.DataString);
        finally
          ss.Free;
        end;

        // version 3
        with libMat.Material.Texture do
        begin
          Write(BorderColor.AsAddress^, SizeOf(Single) * 4);
          WriteInteger(Integer(Compression));
          WriteInteger(Integer(DepthTextureMode));
          Write(EnvColor.AsAddress^, SizeOf(Single) * 4);
          WriteInteger(Integer(FilteringQuality));
          WriteInteger(Integer(ImageAlpha));
          WriteFloat(ImageBrightness);
          WriteFloat(ImageGamma);
          WriteInteger(Integer(MagFilter));
          WriteInteger(Integer(MappingMode));
          Write(MappingSCoordinates.AsAddress^, SizeOf(Single) * 4);
          Write(MappingTCoordinates.AsAddress^, SizeOf(Single) * 4);
          Write(MappingRCoordinates.AsAddress^, SizeOf(Single) * 4);
          Write(MappingQCoordinates.AsAddress^, SizeOf(Single) * 4);
          WriteInteger(Integer(MinFilter));
          WriteFloat(NormalMapScale);
          WriteInteger(Integer(TextureCompareFunc));
          WriteInteger(Integer(TextureCompareMode));
          WriteInteger(Integer(TextureFormat));
          WriteInteger(Integer(TextureMode));
          WriteInteger(Integer(TextureWrap));
          WriteInteger(Integer(TextureWrapR));
          WriteInteger(Integer(TextureWrapS));
          WriteInteger(Integer(TextureWrapT));
        end;
        // version 3 end

      end
      else
        WriteBoolean(False);
      with libMat.Material.FrontProperties do
      begin
        Write(Ambient.AsAddress^, SizeOf(Single) * 3);
        Write(Diffuse.AsAddress^, SizeOf(Single) * 4);
        Write(Emission.AsAddress^, SizeOf(Single) * 3);
        Write(Specular.AsAddress^, SizeOf(Single) * 3);
      end;

      // version 1
      with libMat.Material.FrontProperties do
      begin
        Write(FShininess, 1);
        WriteInteger(Integer(libMat.Material.PolygonMode));
      end;
      with libMat.Material.BackProperties do
      begin
        Write(Ambient.AsAddress^, SizeOf(Single) * 3);
        Write(Diffuse.AsAddress^, SizeOf(Single) * 4);
        Write(Emission.AsAddress^, SizeOf(Single) * 3);
        Write(Specular.AsAddress^, SizeOf(Single) * 3);
        Write(Byte(FShininess), 1);
        WriteInteger(Integer(libMat.Material.PolygonMode));
      end;
      WriteInteger(Integer(libMat.Material.BlendingMode));

      // version 3
      with libMat.Material do
      begin
        if BlendingMode = bmCustom then
        begin
          WriteBoolean(True);
          with BlendingParams do
          begin
            WriteFloat(AlphaFuncRef);
            WriteInteger(Integer(AlphaFunctType));
            WriteInteger(Integer(BlendFuncDFactor));
            WriteInteger(Integer(BlendFuncSFactor));
            WriteBoolean(UseAlphaFunc);
            WriteBoolean(UseBlendFunc);
          end;
        end
        else
          WriteBoolean(False);

        WriteInteger(Integer(FaceCulling));
      end;
      // version 3 end

      WriteInteger(SizeOf(TMaterialOptions));
      Write(libMat.Material.MaterialOptions, SizeOf(TMaterialOptions));
      Write(libMat.TextureOffset.AsAddress^, SizeOf(Single) * 3);
      Write(libMat.TextureScale.AsAddress^, SizeOf(Single) * 3);
      WriteString(libMat.Texture2Name);

      // version 4
      WriteFloat(libMat.TextureRotate);

      // version 2
      WriteInteger(libMat.Material.TextureEx.Count);
      for j := 0 to libMat.Material.TextureEx.Count - 1 do
      begin
        texExItem := libMat.Material.TextureEx[j];
        img := texExItem.Texture.Image;
        pim := TVXPersistentImage(img);
        if texExItem.Texture.Enabled and (img is TVXPersistentImage) and (pim.Picture.Bitmap <> nil) then
        begin
          WriteBoolean(True);
          ss := TStringStream.Create('');
          try
            bmp := TBitmap.Create;
            try
              bmp.Assign(pim.Picture.Bitmap);
              bmp.SaveToStream(ss);
            finally
              bmp.Free;
            end;
            WriteString(ss.DataString);
          finally
            ss.Free;
          end;
        end
        else
          WriteBoolean(False);
        WriteInteger(texExItem.TextureIndex);
        Write(texExItem.TextureOffset.AsAddress^, SizeOf(Single) * 3);
        Write(texExItem.TextureScale.AsAddress^, SizeOf(Single) * 3);
      end;
    end;
  end;
end;

procedure TVXMaterialLibrary.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
  libMat: TVXLibMaterial;
  i, n, size, tex, texCount: Integer;
  LName: string;
  ss: TStringStream;
  /// ->  bmp: TBitmap;
  texExItem: TVXTextureExItem;
begin
  archiveVersion := reader.ReadInteger;
  if (archiveVersion >= 0) and (archiveVersion <= 4) then
    with reader do
    begin
      if not FDoNotClearMaterialsOnLoad then
        Materials.Clear;
      n := ReadInteger;
      for i := 0 to n - 1 do
      begin
        // version 0
        LName := ReadString;
        if FDoNotClearMaterialsOnLoad then
          libMat := LibMaterialByName(LName)
        else
          libMat := nil;
        if ReadBoolean then
        begin
          ss := TStringStream.Create(ReadString);
          try
            /// ->            bmp := TBitmap.Create;
            try
              /// ->              bmp.LoadFromStream(ss);
              if libMat = nil then
                { TODO : E2250 There is no overloaded version of 'AddTextureMaterial' that can be called with these arguments }
                (* libMat := AddTextureMaterial(LName, bmp) *)
              else
                /// ->                libMat.Material.Texture.Image.Assign(bmp);
              finally
                /// ->              bmp.Free;
              end;
            finally
              ss.Free;
            end;

            // version 3
            if archiveVersion >= 3 then
              with libMat.Material.Texture do
              begin
                Read(BorderColor.AsAddress^, SizeOf(Single) * 4);
                Compression := TVXTextureCompression(ReadInteger);
                DepthTextureMode := TVXDepthTextureMode(ReadInteger);
                Read(EnvColor.AsAddress^, SizeOf(Single) * 4);
                FilteringQuality := TVXTextureFilteringQuality(ReadInteger);
                ImageAlpha := TVXTextureImageAlpha(ReadInteger);
                ImageBrightness := ReadFloat;
                ImageGamma := ReadFloat;
                MagFilter := TVXMagFilter(ReadInteger);
                MappingMode := TVXTextureMappingMode(ReadInteger);
                Read(MappingSCoordinates.AsAddress^, SizeOf(Single) * 4);
                Read(MappingTCoordinates.AsAddress^, SizeOf(Single) * 4);
                Read(MappingRCoordinates.AsAddress^, SizeOf(Single) * 4);
                Read(MappingQCoordinates.AsAddress^, SizeOf(Single) * 4);
                MinFilter := TVXMinFilter(ReadInteger);
                NormalMapScale := ReadFloat;
                TextureCompareFunc := TVXDepthCompareFunc(ReadInteger);
                TextureCompareMode := TVXTextureCompareMode(ReadInteger);
                TextureFormat := TVXTextureFormat(ReadInteger);
                TextureMode := TVXTextureMode(ReadInteger);
                TextureWrap := TVXTextureWrap(ReadInteger);
                TextureWrapR := TVXSeparateTextureWrap(ReadInteger);
                TextureWrapS := TVXSeparateTextureWrap(ReadInteger);
                TextureWrapT := TVXSeparateTextureWrap(ReadInteger);
              end;
            // version 3 end

          end
        else
        begin
          if libMat = nil then
          begin
            libMat := Materials.Add;
            libMat.name := LName;
          end;
        end;
        with libMat.Material.FrontProperties do
        begin
          Read(Ambient.AsAddress^, SizeOf(Single) * 3);
          Read(Diffuse.AsAddress^, SizeOf(Single) * 4);
          Read(Emission.AsAddress^, SizeOf(Single) * 3);
          Read(Specular.AsAddress^, SizeOf(Single) * 3);
        end;

        // version 1
        if archiveVersion >= 1 then
        begin
          with libMat.Material.FrontProperties do
          begin
            Read(FShininess, 1);
            libMat.Material.PolygonMode := TVXPolygonMode(ReadInteger);
          end;
          with libMat.Material.BackProperties do
          begin
            Read(Ambient.AsAddress^, SizeOf(Single) * 3);
            Read(Diffuse.AsAddress^, SizeOf(Single) * 4);
            Read(Emission.AsAddress^, SizeOf(Single) * 3);
            Read(Specular.AsAddress^, SizeOf(Single) * 3);
            Read(FShininess, 1);
            { PolygonMode := TPolygonMode( } ReadInteger;
          end;
          libMat.Material.BlendingMode := TBlendingMode(ReadInteger);

          // version 3
          if archiveVersion >= 3 then
          begin
            if ReadBoolean then
              with libMat.Material.BlendingParams do
              begin
                AlphaFuncRef := ReadFloat;
                AlphaFunctType := TGlAlphaFunc(ReadInteger);
                BlendFuncDFactor := TVXBlendFunction(ReadInteger);
                BlendFuncSFactor := TVXBlendFunction(ReadInteger);
                UseAlphaFunc := ReadBoolean;
                UseBlendFunc := ReadBoolean;
              end;

            libMat.Material.FaceCulling := TFaceCulling(ReadInteger);
          end;
          // version 3 end

          size := ReadInteger;
          Read(libMat.Material.FMaterialOptions, size);
          Read(libMat.TextureOffset.AsAddress^, SizeOf(Single) * 3);
          Read(libMat.TextureScale.AsAddress^, SizeOf(Single) * 3);
          libMat.Texture2Name := ReadString;

          // version 4
          if archiveVersion >= 4 then
            libMat.TextureRotate := ReadFloat;
        end;

        // version 2
        if archiveVersion >= 2 then
        begin
          texCount := ReadInteger;
          for tex := 0 to texCount - 1 do
          begin
            texExItem := libMat.Material.TextureEx.Add;
            if ReadBoolean then
            begin
              ss := TStringStream.Create(ReadString);
              /// ->              bmp := TBitmap.Create;
              try
                /// ->                bmp.LoadFromStream(ss);
                /// ->                texExItem.Texture.Image.Assign(bmp);
                texExItem.Texture.Enabled := True;
              finally
                /// ->                bmp.Free;
                ss.Free;
              end;
            end;
            texExItem.TextureIndex := ReadInteger;
            Read(texExItem.TextureOffset.AsAddress^, SizeOf(Single) * 3);
            Read(texExItem.TextureScale.AsAddress^, SizeOf(Single) * 3);
          end;
        end;
      end;
    end
  else
    RaiseFilerException(Self.ClassType, archiveVersion);
end;

procedure TVXMaterialLibrary.SaveToStream(aStream: TStream);
var
  wr: TBinaryWriter;
begin
  wr := TBinaryWriter.Create(aStream);
  try
    Self.WriteToFiler(wr);
  finally
    wr.Free;
  end;
end;

procedure TVXMaterialLibrary.LoadFromStream(aStream: TStream);
var
  rd: TBinaryReader;
begin
  rd := TBinaryReader.Create(aStream);
  try
    Self.ReadFromFiler(rd);
  finally
    rd.Free;
  end;
end;

procedure TVXMaterialLibrary.AddMaterialsFromStream(aStream: TStream);
begin
  FDoNotClearMaterialsOnLoad := True;
  try
    LoadFromStream(aStream);
  finally
    FDoNotClearMaterialsOnLoad := False;
  end;
end;

procedure TVXMaterialLibrary.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TVXMaterialLibrary.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyNone);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TVXMaterialLibrary.AddMaterialsFromFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyNone);
  try
    AddMaterialsFromStream(fs);
  finally
    fs.Free;
  end;
end;

function TVXMaterialLibrary.AddTextureMaterial(const MaterialName, fileName: string; persistent: Boolean = True)
  : TVXLibMaterial;
begin
  Result := Materials.Add;
  with Result do
  begin
    Name := MaterialName;
    with Material.Texture do
    begin
      MinFilter := miLinearMipmapLinear;
      MagFilter := maLinear;
      TextureMode := tmModulate;
      Disabled := False;
      if persistent then
      begin
        ImageClassName := TVXPersistentImage.ClassName;
        if fileName <> '' then
          Image.LoadFromFile(fileName);
      end
      else
      begin
        ImageClassName := TVXPicFileImage.ClassName;
        TVXPicFileImage(Image).PictureFileName := fileName;
      end;
    end;
  end;
end;

function TVXMaterialLibrary.AddTextureMaterial(const MaterialName: string; Graphic: TVXGraphic): TVXLibMaterial;
begin
  Result := Materials.Add;
  with Result do
  begin
    Name := MaterialName;
    with Material.Texture do
    begin
      MinFilter := miLinearMipmapLinear;
      MagFilter := maLinear;
      TextureMode := tmModulate;
      Disabled := False;
      Image.Assign(Graphic);
    end;
  end;
end;

function TVXMaterialLibrary.LibMaterialByName(const AName: TVXLibMaterialName): TVXLibMaterial;
begin
  if Assigned(Self) then
    Result := Materials.GetLibMaterialByName(AName)
  else
    Result := nil;
end;

function TVXMaterialLibrary.TextureByName(const LibMatName: TVXLibMaterialName): TVXTexture;
var
  libMat: TVXLibMaterial;
begin
  if Self = nil then
    raise ETexture.Create(strErrorEx + strMatLibNotDefined)
  else if LibMatName = '' then
    Result := nil
  else
  begin
    libMat := LibMaterialByName(LibMatName);
    if libMat = nil then
      raise ETexture.CreateFmt(strErrorEx + strMaterialNotFoundInMatlibEx, [LibMatName])
    else
      Result := libMat.Material.Texture;
  end;
end;

function TVXMaterialLibrary.GetNameOfTexture(const Texture: TVXTexture): TVXLibMaterialName;
begin
  if (Self = nil) or (Texture = nil) then
    Result := ''
  else
    Result := Materials.GetNameOfTexture(Texture);
end;

function TVXMaterialLibrary.GetMaterials: TVXLibMaterials;
begin
  Result := TVXLibMaterials(FMaterials);
end;

function TVXMaterialLibrary.GetNameOfLibMaterial(const libMat: TVXLibMaterial): TVXLibMaterialName;
begin
  if (Self = nil) or (libMat = nil) then
    Result := ''
  else
    Result := Materials.GetNameOfLibMaterial(libMat);
end;

{ TVXBlendingParameters }

procedure TVXBlendingParameters.Apply(var rci: TVXRenderContextInfo);
begin
  if FUseAlphaFunc then
  begin
    rci.VxStates.Enable(stAlphaTest);
    rci.VxStates.SetAlphaFunction(FAlphaFuncType, FAlphaFuncRef);
  end
  else
    rci.VxStates.Disable(stAlphaTest);
  if FUseBlendFunc then
  begin
    rci.VxStates.Enable(stBlend);
    if FSeparateBlendFunc then
      rci.VxStates.SetBlendFuncSeparate(FBlendFuncSFactor, FBlendFuncDFactor, FAlphaBlendFuncSFactor, FAlphaBlendFuncDFactor)
    else
      rci.VxStates.SetBlendFunc(FBlendFuncSFactor, FBlendFuncDFactor);
  end
  else
    rci.VxStates.Disable(stBlend);
end;

constructor TVXBlendingParameters.Create(AOwner: TPersistent);
begin
  inherited;
  FUseAlphaFunc := False;
  FAlphaFuncType := cfGreater;
  FAlphaFuncRef := 0;

  FUseBlendFunc := True;
  FSeparateBlendFunc := False;
  FBlendFuncSFactor := bfSrcAlpha;
  FBlendFuncDFactor := bfOneMinusSrcAlpha;
  FAlphaBlendFuncSFactor := bfSrcAlpha;
  FAlphaBlendFuncDFactor := bfOneMinusSrcAlpha;
end;

procedure TVXBlendingParameters.SetAlphaFuncRef(const Value: Single);
begin
  if (FAlphaFuncRef <> Value) then
  begin
    FAlphaFuncRef := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXBlendingParameters.SetAlphaFuncType(const Value: TGlAlphaFunc);
begin
  if (FAlphaFuncType <> Value) then
  begin
    FAlphaFuncType := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXBlendingParameters.SetBlendFuncDFactor(const Value: TVXBlendFunction);
begin
  if (FBlendFuncDFactor <> Value) then
  begin
    FBlendFuncDFactor := Value;
    if not FSeparateBlendFunc then
      FAlphaBlendFuncDFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXBlendingParameters.SetBlendFuncSFactor(const Value: TVXBlendFunction);
begin
  if (FBlendFuncSFactor <> Value) then
  begin
    FBlendFuncSFactor := Value;
    if not FSeparateBlendFunc then
      FAlphaBlendFuncSFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXBlendingParameters.SetAlphaBlendFuncDFactor(const Value: TVXBlendFunction);
begin
  if FSeparateBlendFunc and (FAlphaBlendFuncDFactor <> Value) then
  begin
    FAlphaBlendFuncDFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXBlendingParameters.SetAlphaBlendFuncSFactor(const Value: TVXBlendFunction);
begin
  if FSeparateBlendFunc and (FAlphaBlendFuncSFactor <> Value) then
  begin
    FAlphaBlendFuncSFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXBlendingParameters.SetUseAlphaFunc(const Value: Boolean);
begin
  if (FUseAlphaFunc <> Value) then
  begin
    FUseAlphaFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXBlendingParameters.SetUseBlendFunc(const Value: Boolean);
begin
  if (FUseBlendFunc <> Value) then
  begin
    FUseBlendFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TVXBlendingParameters.SetSeparateBlendFunc(const Value: Boolean);
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

function TVXBlendingParameters.StoreAlphaFuncRef: Boolean;
begin
  Result := (Abs(AlphaFuncRef) > 0.001);
end;

//-------------------------------------------------
initialization
//-------------------------------------------------

RegisterClasses([TVXMaterialLibrary, TVXMaterial, TVXShader]);

end.
