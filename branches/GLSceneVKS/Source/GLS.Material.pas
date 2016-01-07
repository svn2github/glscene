//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
  Handles all the material + material library stuff. 
}

unit GLS.Material;

interface

uses
  System.Classes, System.SysUtils, System.Types,
  //GLS
  GLS.RenderContextInfo, GLS.BaseClasses, GLS.OpenGLTokens, GLS.Context,
  GLS.Texture, GLS.Color, GLS.Coordinates, GLS.VectorGeometry, GLS.PersistentClasses,
  GLS.CrossPlatform, GLS.State, GLS.TextureFormat, GLS.Strings, GLS.XOpenGL,
  GLS.ApplicationFileIO, GLS.Graphics, GLS.Utils, GLS.Log;

{$I GLScene.inc}
{$UNDEF VKS_MULTITHREAD}
type
  TVKFaceProperties = class;
  TVKMaterial = class;
  TVKAbstractMaterialLibrary = class;
  TVKMaterialLibrary = class;

  //an interface for proper TVKLibMaterialNameProperty support
  IGLMaterialLibrarySupported = interface(IInterface)
    ['{8E442AF9-D212-4A5E-8A88-92F798BABFD1}']
    function GetMaterialLibrary: TVKAbstractMaterialLibrary;
  end;

  TVKAbstractLibMaterial = class;
  TVKLibMaterial = class;

  // TVKShaderStyle
  //
  { Define GLShader style application relatively to a material. 
      ssHighLevel: shader is applied before material application, and unapplied
           after material unapplication
      ssLowLevel: shader is applied after material application, and unapplied
           before material unapplication
      ssReplace: shader is applied in place of the material (and material
           is completely ignored)
       }
  TVKShaderStyle = (ssHighLevel, ssLowLevel, ssReplace);

  // TVKShaderFailedInitAction
  //
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
  TVKShaderFailedInitAction = (
    fiaSilentDisable, fiaRaiseStandardException,
    fiaRaiseHandledException, fiaReRaiseException
    {,fiaGenerateEvent});

  // TVKShader
  //
  { Generic, abstract shader class. 
     Shaders are modeled here as an abstract material-altering entity with
     transaction-like behaviour. The base class provides basic context and user
     tracking, as well as setup/application facilities. 
     Subclasses are expected to provide implementation for DoInitialize,
     DoApply, DoUnApply and DoFinalize. }
  TVKShader = class(TVKUpdateAbleComponent)
  private
    { Private Declarations }
    FEnabled: Boolean;
    FLibMatUsers: TList;
    FVirtualHandle: TVKVirtualHandle;
    FShaderStyle: TVKShaderStyle;
    FUpdateCount: Integer;
    FShaderActive: Boolean;
    FFailedInitAction: TVKShaderFailedInitAction;

  protected
    { Protected Declarations }
          { Invoked once, before the first call to DoApply. 
             The call happens with the OpenGL context being active. }
    procedure DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
      dynamic;
    { Request to apply the shader. 
       Always followed by a DoUnApply when the shader is no longer needed. }
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); virtual;
       {$IFNDEF VKS_CPPB} abstract; {$ENDIF}
    { Request to un-apply the shader. 
       Subclasses can assume the shader has been applied previously. 
       Return True to request a multipass. }
    function DoUnApply(var rci: TRenderContextInfo): Boolean; virtual;
       {$IFNDEF VKS_CPPB} abstract; {$ENDIF}
    { Invoked once, before the destruction of context or release of shader. 
       The call happens with the OpenGL context being active. }
    procedure DoFinalize; dynamic;

    function GetShaderInitialized: Boolean;
    procedure InitializeShader(var rci: TRenderContextInfo; Sender: TObject);
    procedure FinalizeShader;
    procedure OnVirtualHandleAllocate(sender: TVKVirtualHandle; var handle:
      Cardinal);
    procedure OnVirtualHandleDestroy(sender: TVKVirtualHandle; var handle:
      Cardinal);
    procedure SetEnabled(val: Boolean);

    property ShaderInitialized: Boolean read GetShaderInitialized;
    property ShaderActive: Boolean read FShaderActive;

    procedure RegisterUser(libMat: TVKLibMaterial);
    procedure UnRegisterUser(libMat: TVKLibMaterial);

    { Used by the DoInitialize procedure of descendant classes to raise errors. }
    procedure HandleFailedInitialization(const LastErrorMessage: string = '');
      virtual;

    { May be this should be a function inside HandleFailedInitialization... }
    function GetStardardNotSupportedMessage: string; virtual;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Subclasses should invoke this function when shader properties are altered.
        This procedure can also be used to reset/recompile the shader. }
    procedure NotifyChange(Sender: TObject); override;
    procedure BeginUpdate;
    procedure EndUpdate;

    { Apply shader to OpenGL state machine.}
    procedure Apply(var rci: TRenderContextInfo; Sender: TObject);
    { UnApply shader. 
       When returning True, the caller is expected to perform a multipass
       rendering by re-rendering then invoking UnApply again, until a
       "False" is returned. }
    function UnApply(var rci: TRenderContextInfo): Boolean;

    { Shader application style (default is ssLowLevel). }
    property ShaderStyle: TVKShaderStyle read FShaderStyle write FShaderStyle
      default ssLowLevel;

    procedure Assign(Source: TPersistent); override;

    { Defines if shader is supported by hardware/drivers.
       Default - always supported. Descendants are encouraged to override
       this function. }
    function ShaderSupported: Boolean; virtual;

    { Defines what to do if for some reason shader failed to initialize.
       Note, that in some cases it cannon be determined by just checking the
       required OpenGL extentions. You need to try to compile and link the
       shader - only at that stage you might catch an error }
    property FailedInitAction: TVKShaderFailedInitAction
      read FFailedInitAction write FFailedInitAction default
      fiaRaiseStandardException;

  published
    { Published Declarations }
      { Turns on/off shader application. 
         Note that this only turns on/off the shader application, if the
         ShaderStyle is ssReplace, the material won't be applied even if
         the shader is disabled. }
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TVKShaderClass = class of TVKShader;

  TShininess = 0..128;

  // TVKFaceProperties
  //
  { Stores basic face lighting properties. 
     The lighting is described with the standard ambient/diffuse/emission/specular
     properties that behave like those of most rendering tools. 
     You also have control over shininess (governs specular lighting) and
     polygon mode (lines / fill). }
  TVKFaceProperties = class(TVKUpdateAbleObject)
  private
    { Private Declarations }
    FAmbient, FDiffuse, FSpecular, FEmission: TVKColor;
    FShininess: TShininess;

  protected
    { Protected Declarations }
    procedure SetAmbient(AValue: TVKColor);
    procedure SetDiffuse(AValue: TVKColor);
    procedure SetEmission(AValue: TVKColor);
    procedure SetSpecular(AValue: TVKColor);
    procedure SetShininess(AValue: TShininess);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Apply(var rci: TRenderContextInfo; aFace: TCullFaceMode);
    procedure ApplyNoLighting(var rci: TRenderContextInfo; aFace:
      TCullFaceMode);
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property Ambient: TVKColor read FAmbient write SetAmbient;
    property Diffuse: TVKColor read FDiffuse write SetDiffuse;
    property Emission: TVKColor read FEmission write SetEmission;
    property Shininess: TShininess read FShininess write SetShininess default 0;
    property Specular: TVKColor read FSpecular write SetSpecular;
  end;

  TVKDepthProperties = class(TVKUpdateAbleObject)
  private
    { Private Declarations }
    FDepthTest: boolean;
    FDepthWrite: boolean;
    FZNear, FZFar: Single;
    FCompareFunc: TDepthfunction;
    FDepthClamp: Boolean;
  protected
    { Protected Declarations }
    procedure SetZNear(Value: Single);
    procedure SetZFar(Value: Single);
    procedure SetCompareFunc(Value: TVKDepthCompareFunc);
    procedure SetDepthTest(Value: boolean);
    procedure SetDepthWrite(Value: boolean);
    procedure SetDepthClamp(Value: boolean);

    function StoreZNear: Boolean;
    function StoreZFar: Boolean;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;

    procedure Apply(var rci: TRenderContextInfo);
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    { Specifies the mapping of the near clipping plane to
       window coordinates.  The initial value is 0.  }
    property ZNear: Single read FZNear write SetZNear stored StoreZNear;
    { Specifies the mapping of the far clipping plane to
       window coordinates.  The initial value is 1. }
    property ZFar: Single read FZFar write SetZFar stored StoreZFar;
    { Specifies the function used to compare each
      incoming pixel depth value with the depth value present in
      the depth buffer. }
    property DepthCompareFunction: TDepthFunction
      read FCompareFunc write SetCompareFunc default cfLequal;
    { DepthTest enabling. 
       When DepthTest is enabled, objects closer to the camera will hide
       farther ones (via use of Z-Buffering). 
       When DepthTest is disabled, the latest objects drawn/rendered overlap
       all previous objects, whatever their distance to the camera. 
       Even when DepthTest is enabled, objects may chose to ignore depth
       testing through the osIgnoreDepthBuffer of their ObjectStyle property. }
    property DepthTest: boolean read FDepthTest write SetDepthTest default True;
    { If True, object will not write to Z-Buffer. }
    property DepthWrite: boolean read FDepthWrite write SetDepthWrite default
      True;
    { Enable clipping depth to the near and far planes }
    property DepthClamp: Boolean read FDepthClamp write SetDepthClamp default
      False;
  end;

  TVKLibMaterialName = string;

  //
  // DaStr: if you write smth like af_GL_NEVER = GL_NEVER in the definition,
  // it won't show up in the Dephi 7 design-time editor. So I had to add
  // vTGlAlphaFuncValues and vTVKBlendFuncFactorValues arrays.
  //
  TGlAlphaFunc = TComparisonFunction;

  // TVKBlendingParameters
  //
  TVKBlendingParameters = class(TVKUpdateAbleObject)
  private
    FUseAlphaFunc: Boolean;
    FUseBlendFunc: Boolean;
    FSeparateBlendFunc: Boolean;
    FAlphaFuncType: TGlAlphaFunc;
    FAlphaFuncRef: TGLclampf;
    FBlendFuncSFactor: TBlendFunction;
    FBlendFuncDFactor: TBlendFunction;
    FAlphaBlendFuncSFactor: TBlendFunction;
    FAlphaBlendFuncDFactor: TBlendFunction;
    procedure SetUseAlphaFunc(const Value: Boolean);
    procedure SetUseBlendFunc(const Value: Boolean);
    procedure SetSeparateBlendFunc(const Value: Boolean);
    procedure SetAlphaFuncRef(const Value: TGLclampf);
    procedure SetAlphaFuncType(const Value: TGlAlphaFunc);
    procedure SetBlendFuncDFactor(const Value: TBlendFunction);
    procedure SetBlendFuncSFactor(const Value: TBlendFunction);
    procedure SetAlphaBlendFuncDFactor(const Value: TBlendFunction);
    procedure SetAlphaBlendFuncSFactor(const Value: TBlendFunction);
    function StoreAlphaFuncRef: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Apply(var rci: TRenderContextInfo);
  published
    property UseAlphaFunc: Boolean read FUseAlphaFunc write SetUseAlphaFunc
      default False;
    property AlphaFunctType: TGlAlphaFunc read FAlphaFuncType write
      SetAlphaFuncType default cfGreater;
    property AlphaFuncRef: TGLclampf read FAlphaFuncRef write SetAlphaFuncRef
      stored StoreAlphaFuncRef;

    property UseBlendFunc: Boolean read FUseBlendFunc write SetUseBlendFunc
      default True;
    property SeparateBlendFunc: Boolean read FSeparateBlendFunc write SetSeparateBlendFunc
      default False;
    property BlendFuncSFactor: TBlendFunction read FBlendFuncSFactor write
      SetBlendFuncSFactor default bfSrcAlpha;
    property BlendFuncDFactor: TBlendFunction read FBlendFuncDFactor write
      SetBlendFuncDFactor default bfOneMinusSrcAlpha;
    property AlphaBlendFuncSFactor: TBlendFunction read FAlphaBlendFuncSFactor write
      SetAlphaBlendFuncSFactor default bfSrcAlpha;
    property AlphaBlendFuncDFactor: TBlendFunction read FAlphaBlendFuncDFactor write
      SetAlphaBlendFuncDFactor default bfOneMinusSrcAlpha;
  end;

  // TBlendingMode
  //
  { Simplified blending options. 
     bmOpaque : disable blending 
     bmTransparency : uses standard alpha blending 
     bmAdditive : activates additive blending (with saturation) 
     bmAlphaTest50 : uses opaque blending, with alpha-testing at 50% (full
        transparency if alpha is below 0.5, full opacity otherwise) 
     bmAlphaTest100 : uses opaque blending, with alpha-testing at 100% 
     bmModulate : uses modulation blending 
     bmCustom : uses TVKBlendingParameters options
     }
  TBlendingMode = (bmOpaque, bmTransparency, bmAdditive,
    bmAlphaTest50, bmAlphaTest100, bmModulate, bmCustom);

  // TFaceCulling
  //
  TFaceCulling = (fcBufferDefault, fcCull, fcNoCull);

  // TMaterialOptions
  //
  { Control special rendering options for a material. 
     moIgnoreFog : fog is deactivated when the material is rendered }
  TMaterialOption = (moIgnoreFog, moNoLighting);
  TMaterialOptions = set of TMaterialOption;

  // TVKMaterial
   //
   { Describes a rendering material. 
      A material is basicly a set of face properties (front and back) that take
      care of standard material rendering parameters (diffuse, ambient, emission
      and specular) and texture mapping. 
      An instance of this class is available for almost all objects in GLScene
      to allow quick definition of material properties. It can link to a
      TVKLibMaterial (taken for a material library). 
      The TVKLibMaterial has more adavanced properties (like texture transforms)
      and provides a standard way of sharing definitions and texture maps. }
  TVKMaterial = class(TVKUpdateAbleObject, IGLMaterialLibrarySupported,
      IGLNotifyAble, IGLTextureNotifyAble)
  private
    { Private Declarations }
    FFrontProperties, FBackProperties: TVKFaceProperties;
    FDepthProperties: TVKDepthProperties;
    FBlendingMode: TBlendingMode;
    FBlendingParams: TVKBlendingParameters;
    FTexture: TVKTexture;
    FTextureEx: TVKTextureEx;
    FMaterialLibrary: TVKAbstractMaterialLibrary;
    FLibMaterialName: TVKLibMaterialName;
    FMaterialOptions: TMaterialOptions;
    FFaceCulling: TFaceCulling;
    FPolygonMode: TPolygonMode;
    currentLibMaterial: TVKAbstractLibMaterial;

    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TVKAbstractMaterialLibrary;
  protected
    { Protected Declarations }
    function GetBackProperties: TVKFaceProperties;
    procedure SetBackProperties(Values: TVKFaceProperties);
    procedure SetFrontProperties(Values: TVKFaceProperties);
    procedure SetDepthProperties(Values: TVKDepthProperties);
    procedure SetBlendingMode(const val: TBlendingMode);
    procedure SetMaterialOptions(const val: TMaterialOptions);
    function GetTexture: TVKTexture;
    procedure SetTexture(ATexture: TVKTexture);
    procedure SetMaterialLibrary(const val: TVKAbstractMaterialLibrary);
    procedure SetLibMaterialName(const val: TVKLibMaterialName);
    procedure SetFaceCulling(const val: TFaceCulling);
    procedure SetPolygonMode(AValue: TPolygonMode);
    function GetTextureEx: TVKTextureEx;
    procedure SetTextureEx(const value: TVKTextureEx);
    function StoreTextureEx: Boolean;
    procedure SetBlendingParams(const Value: TVKBlendingParameters);

    procedure NotifyLibMaterialDestruction;
    //: Back, Front, Texture and blending not stored if linked to a LibMaterial
    function StoreMaterialProps: Boolean;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure PrepareBuildList;
    procedure Apply(var rci: TRenderContextInfo);
    { Restore non-standard material states that were altered; 
       A return value of True is a multipass request. }
    function UnApply(var rci: TRenderContextInfo): Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure NotifyTexMapChange(Sender: TObject);
    procedure DestroyHandles;

    procedure Loaded;

    { Returns True if the material is blended. 
       Will return the libmaterial's blending if it is linked to a material
       library. }
    function Blended: Boolean;

    //: True if the material has a secondary texture
    function HasSecondaryTexture: Boolean;

    //: True if the material comes from the library instead of the texture property
    function MaterialIsLinkedToLib: Boolean;

    //: Gets the primary texture either from material library or the texture property
    function GetActualPrimaryTexture: TVKTexture;

    //: Gets the primary Material either from material library or the texture property
    function GetActualPrimaryMaterial: TVKMaterial;

    //: Return the LibMaterial (see LibMaterialName)
    function GetLibMaterial: TVKLibMaterial;

    procedure QuickAssignMaterial(const MaterialLibrary: TVKMaterialLibrary;
      const Material: TVKLibMaterial);
  published
    { Published Declarations }
    property BackProperties: TVKFaceProperties read GetBackProperties write
      SetBackProperties stored StoreMaterialProps;
    property FrontProperties: TVKFaceProperties read FFrontProperties write
      SetFrontProperties stored StoreMaterialProps;
    property DepthProperties: TVKDepthProperties read FDepthProperties write
      SetDepthProperties stored StoreMaterialProps;
    property BlendingMode: TBlendingMode read FBlendingMode write SetBlendingMode
      stored StoreMaterialProps default bmOpaque;
    property BlendingParams: TVKBlendingParameters read FBlendingParams write
      SetBlendingParams;

    property MaterialOptions: TMaterialOptions read FMaterialOptions write
      SetMaterialOptions default [];
    property Texture: TVKTexture read GetTexture write SetTexture stored
      StoreMaterialProps;
    property FaceCulling: TFaceCulling read FFaceCulling write SetFaceCulling
      default fcBufferDefault;

    property MaterialLibrary: TVKAbstractMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    property LibMaterialName: TVKLibMaterialName read FLibMaterialName write
      SetLibMaterialName;
    property TextureEx: TVKTextureEx read GetTextureEx write SetTextureEx stored
      StoreTextureEx;
    property PolygonMode: TPolygonMode read FPolygonMode write SetPolygonMode
      default pmFill;
  end;

  // TVKBaseLibMaterial
  //

  TVKAbstractLibMaterial = class(
    TCollectionItem,
    IGLMaterialLibrarySupported,
    IGLNotifyAble)
  protected
    { Protected Declarations }
    FUserList: TList;
    FName: TVKLibMaterialName;
    FNameHashKey: Integer;
    FTag: Integer;
    FNotifying: Boolean; // used for recursivity protection
    //implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TVKAbstractMaterialLibrary;
    //implementing IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    { Protected Declarations }
    function GetDisplayName: string; override;
    class function ComputeNameHashKey(const name: string): Integer;
    procedure SetName(const val: TVKLibMaterialName);
    procedure Loaded; virtual;{$IFNDEF VKS_CPPB} abstract; {$ENDIF}

  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Apply(var ARci: TRenderContextInfo); virtual; {$IFNDEF VKS_CPPB} abstract; {$ENDIF}
    //: Restore non-standard material states that were altered
    function UnApply(var ARci: TRenderContextInfo): Boolean; virtual; {$IFNDEF VKS_CPPB} abstract; {$ENDIF}

    procedure RegisterUser(obj: TVKUpdateAbleObject); overload;
    procedure UnregisterUser(obj: TVKUpdateAbleObject); overload;
    procedure RegisterUser(comp: TVKUpdateAbleComponent); overload;
    procedure UnregisterUser(comp: TVKUpdateAbleComponent); overload;
    procedure RegisterUser(libMaterial: TVKLibMaterial); overload;
    procedure UnregisterUser(libMaterial: TVKLibMaterial); overload;
    procedure NotifyUsers;
    function IsUsed: boolean; //returns true if the texture has registed users
    property NameHashKey: Integer read FNameHashKey;
    procedure NotifyChange(Sender: TObject); virtual;
    function Blended: Boolean; virtual;
    property MaterialLibrary: TVKAbstractMaterialLibrary read GetMaterialLibrary;
  published
    { Published Declarations }
    property Name: TVKLibMaterialName read FName write SetName;
    property Tag: Integer read FTag write FTag;
  end;

  // TVKLibMaterial
  //
    { Material in a material library. 
       Introduces Texture transformations (offset and scale). Those transformations
       are available only for lib materials to minimize the memory cost of basic
       materials (which are used in almost all objects). }
  TVKLibMaterial = class(TVKAbstractLibMaterial, IGLTextureNotifyAble)
  private
    { Private Declarations }
    FMaterial: TVKMaterial;
    FTextureOffset, FTextureScale: TVKCoordinates;
    FTextureRotate: Single;
    FTextureMatrixIsIdentity: Boolean;
    FTextureOverride: Boolean;
    FTextureMatrix: TMatrix;
    FTexture2Name: TVKLibMaterialName;
    FShader: TVKShader;
    libMatTexture2: TVKLibMaterial; // internal cache
  protected
    { Protected Declarations }
    procedure Loaded; override;
    procedure SetMaterial(const val: TVKMaterial);
    procedure SetTextureOffset(const val: TVKCoordinates);
    procedure SetTextureScale(const val: TVKCoordinates);
    procedure SetTextureMatrix(const Value: TMatrix);
    procedure SetTexture2Name(const val: TVKLibMaterialName);
    procedure SetShader(const val: TVKShader);
    procedure SetTextureRotate(Value: Single);
    function StoreTextureRotate: Boolean;

    procedure CalculateTextureMatrix;
    procedure DestroyHandles;
    procedure DoOnTextureNeeded(Sender: TObject; var textureFileName: string);
    procedure OnNotifyChange(Sender: TObject);
  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure PrepareBuildList;
    procedure Apply(var ARci: TRenderContextInfo); override;
    //: Restore non-standard material states that were altered
    function UnApply(var ARci: TRenderContextInfo): Boolean; override;

    procedure NotifyUsersOfTexMapChange;
    property TextureMatrix: TMatrix read FTextureMatrix write SetTextureMatrix;
    property TextureMatrixIsIdentity: boolean read FTextureMatrixIsIdentity;
    procedure NotifyTexMapChange(Sender: TObject);
    function Blended: Boolean; override;
  published
    { Published Declarations }
    property Material: TVKMaterial read FMaterial write SetMaterial;

    { Texture offset in texture coordinates. 
       The offset is applied <i>after</i> scaling. }
    property TextureOffset: TVKCoordinates read FTextureOffset write
      SetTextureOffset;
    { Texture coordinates scaling. 
       Scaling is applied <i>before</i> applying the offset, and is applied
       to the texture coordinates, meaning that a scale factor of (2, 2, 2)
       will make your texture look twice <i>smaller</i>. }
    property TextureScale: TVKCoordinates read FTextureScale write
      SetTextureScale;

    property TextureRotate: Single read FTextureRotate write
      SetTextureRotate stored StoreTextureRotate;
    { Reference to the second texture. 
       The referred LibMaterial *must* be in the same material library. 
       Second textures are supported only through ARB multitexturing (ignored
       if not supported). }
    property Texture2Name: TVKLibMaterialName read FTexture2Name write
      SetTexture2Name;

    { Optionnal shader for the material. }
    property Shader: TVKShader read FShader write SetShader;
  end;

  // TVKAbstractLibMaterials
  //

  TVKAbstractLibMaterials = class(TOwnedCollection)
  protected
    { Protected Declarations }
    procedure Loaded;
    function GetMaterial(const AName: TVKLibMaterialName): TVKAbstractLibMaterial;
    {$IFDEF VKS_INLINE}inline;{$ENDIF}
  public
    function MakeUniqueName(const nameRoot: TVKLibMaterialName):
      TVKLibMaterialName; virtual;
  end;

  // TVKLibMaterials
  //
    { A collection of materials, mainly used in material libraries. }

  TVKLibMaterials = class(TVKAbstractLibMaterials)
  protected
    { Protected Declarations }
    procedure SetItems(index: Integer; const val: TVKLibMaterial);
    function GetItems(index: Integer): TVKLibMaterial;
    procedure DestroyHandles;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);

    function Owner: TPersistent;

    function IndexOf(const Item: TVKLibMaterial): Integer;
    function Add: TVKLibMaterial;
    function FindItemID(ID: Integer): TVKLibMaterial;
    property Items[index: Integer]: TVKLibMaterial read GetItems write SetItems;
    default;

    function GetLibMaterialByName(const AName: TVKLibMaterialName):
      TVKLibMaterial;
    { Returns index of this Texture if it exists. }
    function GetTextureIndex(const Texture: TVKTexture): Integer;

    { Returns index of this Material if it exists. }
    function GetMaterialIndex(const Material: TVKMaterial): Integer;

    { Returns name of this Texture if it exists. }
    function GetNameOfTexture(const Texture: TVKTexture): TVKLibMaterialName;

    { Returns name of this Material if it exists. }
    function GetNameOfLibMaterial(const Material: TVKLibMaterial):
      TVKLibMaterialName;

    procedure PrepareBuildList;
    { Deletes all the unused materials in the collection. 
       A material is considered unused if no other material or updateable object references it.
       WARNING: For this to work, objects that use the textuere, have to REGISTER to the texture.}
    procedure DeleteUnusedMaterials;
  end;

  // TVKAbstractMaterialLibrary
  //

  TVKAbstractMaterialLibrary = class(TVKCadenceAbleComponent)
  protected
    { Protected Declarations }
    FMaterials: TVKAbstractLibMaterials;
    FLastAppliedMaterial: TVKAbstractLibMaterial;
    FTexturePaths: string;
    FTexturePathList: TStringList;
    procedure SetTexturePaths(const val: string);
    property TexturePaths: string read FTexturePaths write SetTexturePaths;
    procedure Loaded; override;
  public
    { Public Declarations }

    procedure SetNamesToTStrings(AStrings: TStrings);
    { Applies the material of given name. 
       Returns False if the material could not be found. ake sure this
       call is balanced with a corresponding UnApplyMaterial (or an
       assertion will be triggered in the destructor). 
       If a material is already applied, and has not yet been unapplied,
       an assertion will be triggered. }
    function ApplyMaterial(const AName: string;
      var ARci: TRenderContextInfo): Boolean; virtual;
    { Un-applies the last applied material. 
       Use this function in conjunction with ApplyMaterial. 
       If no material was applied, an assertion will be triggered. }
    function UnApplyMaterial(var ARci: TRenderContextInfo): Boolean; virtual;
  end;

  // TVKMaterialLibrary
  //
  { Stores a set of materials, to be used and shared by scene objects. 
     Use a material libraries for storing commonly used materials, it provides
     an efficient way to share texture and material data among many objects,
     thus reducing memory needs and rendering time. 
     Materials in a material library also feature advanced control properties
     like texture coordinates transforms. }
  TVKMaterialLibrary = class(TVKAbstractMaterialLibrary)
  private
    { Private Declarations }
    FDoNotClearMaterialsOnLoad: Boolean;
    FOnTextureNeeded: TTextureNeededEvent;
  protected
    { Protected Declarations }
    function GetMaterials: TVKLibMaterials;
    procedure SetMaterials(const val: TVKLibMaterials);
    function StoreMaterials: Boolean;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyHandles;

    procedure WriteToFiler(writer: TVirtualWriter);
    procedure ReadFromFiler(reader: TVirtualReader);
    procedure SaveToStream(aStream: TStream); dynamic;
    procedure LoadFromStream(aStream: TStream); dynamic;
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
       (via a TVKPersistentImage), if false, it will be unloaded after upload
       to OpenGL (via TVKPicFileImage). }
    function AddTextureMaterial(const MaterialName, FileName: string;
      persistent: Boolean = True): TVKLibMaterial; overload;
    { Add a "standard" texture material. 
       TVKGraphic based variant. }
    function AddTextureMaterial(const MaterialName: string; Graphic:
      TVKGraphic): TVKLibMaterial; overload;

    { Returns libMaterial of given name if any exists. }
    function LibMaterialByName(const AName: TVKLibMaterialName): TVKLibMaterial;

    { Returns Texture of given material's name if any exists. }
    function TextureByName(const LibMatName: TVKLibMaterialName): TVKTexture;

    { Returns name of texture if any exists. }
    function GetNameOfTexture(const Texture: TVKTexture): TVKLibMaterialName;

    { Returns name of Material if any exists. }
    function GetNameOfLibMaterial(const LibMat: TVKLibMaterial):
      TVKLibMaterialName;

  published
    { Published Declarations }
      { The materials collection. }
    property Materials: TVKLibMaterials read GetMaterials write SetMaterials stored
      StoreMaterials;
    { This event is fired whenever a texture needs to be loaded from disk. 
       The event is triggered before even attempting to load the texture,
       and before TexturePaths is used. }
    property OnTextureNeeded: TTextureNeededEvent read FOnTextureNeeded write
      FOnTextureNeeded;
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
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------


resourcestring
  strCyclicRefMat = 'Cyclic reference detected in material "%s"';


  // Dummy methods for CPP
  //
{$IFDEF VKS_CPPB}
procedure TVKShader.DoApply(var Rci: TRenderContextInfo; Sender: TObject);
begin
end;

function TVKShader.DoUnApply(var Rci: TRenderContextInfo): Boolean;
begin
  Result := True;
end;

procedure TVKAbstractLibMaterial.Loaded;
begin
end;

procedure TVKAbstractLibMaterial.Apply(var ARci: TRenderContextInfo);
begin
end;

function TVKAbstractLibMaterial.UnApply(var ARci: TRenderContextInfo): Boolean;
begin
  Result := True;
end;
{$ENDIF}


  // ------------------
  // ------------------ TVKFaceProperties ------------------
  // ------------------

  // Create
  //

constructor TVKFaceProperties.Create(aOwner: TPersistent);
begin
  inherited;
  // OpenGL default colors
  FAmbient := TVKColor.CreateInitialized(Self, clrGray20);
  FDiffuse := TVKColor.CreateInitialized(Self, clrGray80);
  FEmission := TVKColor.Create(Self);
  FSpecular := TVKColor.Create(Self);
  FShininess := 0;
end;

// Destroy
//
destructor TVKFaceProperties.Destroy;
begin
  FAmbient.Free;
  FDiffuse.Free;
  FEmission.Free;
  FSpecular.Free;
  inherited Destroy;
end;

// Apply
//
procedure TVKFaceProperties.Apply(var rci: TRenderContextInfo;
  aFace: TCullFaceMode);
begin
  with rci.GLStates do
  begin
    SetGLMaterialColors(aFace,
    Emission.Color, Ambient.Color, Diffuse.Color, Specular.Color, FShininess);
  end;
end;

// ApplyNoLighting
//
procedure TVKFaceProperties.ApplyNoLighting(var rci: TRenderContextInfo;
  aFace: TCullFaceMode);
begin
  GL.Color4fv(Diffuse.AsAddress);
end;

// Assign
//
procedure TVKFaceProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKFaceProperties) then
  begin
    FAmbient.DirectColor := TVKFaceProperties(Source).Ambient.Color;
    FDiffuse.DirectColor := TVKFaceProperties(Source).Diffuse.Color;
    FEmission.DirectColor := TVKFaceProperties(Source).Emission.Color;
    FSpecular.DirectColor := TVKFaceProperties(Source).Specular.Color;
    FShininess := TVKFaceProperties(Source).Shininess;
    NotifyChange(Self);
  end;
end;

// SetAmbient
//

procedure TVKFaceProperties.SetAmbient(AValue: TVKColor);
begin
  FAmbient.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

// SetDiffuse
//

procedure TVKFaceProperties.SetDiffuse(AValue: TVKColor);
begin
  FDiffuse.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

// SetEmission
//

procedure TVKFaceProperties.SetEmission(AValue: TVKColor);
begin
  FEmission.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

// SetSpecular
//

procedure TVKFaceProperties.SetSpecular(AValue: TVKColor);
begin
  FSpecular.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

// SetShininess
//

procedure TVKFaceProperties.SetShininess(AValue: TShininess);
begin
  if FShininess <> AValue then
  begin
    FShininess := AValue;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TVKDepthProperties ------------------
// ------------------

constructor TVKDepthProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDepthTest := True;
  FDepthWrite := True;
  FZNear := 0;
  FZFar := 1;
  FCompareFunc := cfLequal;
  FDepthClamp := False;
end;

procedure TVKDepthProperties.Apply(var rci: TRenderContextInfo);
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
    if GL.ARB_depth_clamp then
      if FDepthClamp then
        Enable(stDepthClamp)
      else
        Disable(stDepthClamp);
  end;
end;

procedure TVKDepthProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKDepthProperties) then
  begin
    FDepthTest := TVKDepthProperties(Source).FDepthTest;
    FDepthWrite := TVKDepthProperties(Source).FDepthWrite;
    FZNear := TVKDepthProperties(Source).FZNear;
    FZFar := TVKDepthProperties(Source).FZFar;
    FCompareFunc := TVKDepthProperties(Source).FCompareFunc;
    NotifyChange(Self);
  end;
end;

procedure TVKDepthProperties.SetZNear(Value: Single);
begin
  Value := ClampValue(Value, 0, 1);
  if Value <> FZNear then
  begin
    FZNear := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKDepthProperties.SetZFar(Value: Single);
begin
  Value := ClampValue(Value, 0, 1);
  if Value <> FZFar then
  begin
    FZFar := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKDepthProperties.SetCompareFunc(Value: TDepthFunction);
begin
  if Value <> FCompareFunc then
  begin
    FCompareFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKDepthProperties.SetDepthTest(Value: boolean);
begin
  if Value <> FDepthTest then
  begin
    FDepthTest := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKDepthProperties.SetDepthWrite(Value: boolean);
begin
  if Value <> FDepthWrite then
  begin
    FDepthWrite := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKDepthProperties.SetDepthClamp(Value: boolean);
begin
  if Value <> FDepthClamp then
  begin
    FDepthClamp := Value;
    NotifyChange(Self);
  end;
end;

function TVKDepthProperties.StoreZNear: Boolean;
begin
  Result := FZNear <> 0.0;
end;

function TVKDepthProperties.StoreZFar: Boolean;
begin
  Result := FZFar <> 1.0;
end;

// ------------------
// ------------------ TVKShader ------------------
// ------------------

// Create
//

constructor TVKShader.Create(AOwner: TComponent);
begin
  FLibMatUsers := TList.Create;
  FVirtualHandle := TVKVirtualHandle.Create;
  FVirtualHandle.OnAllocate := OnVirtualHandleAllocate;
  FVirtualHandle.OnDestroy := OnVirtualHandleDestroy;
  FShaderStyle := ssLowLevel;
  FEnabled := True;
  FFailedInitAction := fiaRaiseStandardException;
  inherited;
end;

// Destroy
//

destructor TVKShader.Destroy;
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
    TVKLibMaterial(list[i]).Shader := nil;
  list.Free;
  FVirtualHandle.Free;
end;

// NotifyChange
//

procedure TVKShader.NotifyChange(Sender: TObject);
var
  i: Integer;
begin
  if FUpdateCount = 0 then
  begin
    for i := FLibMatUsers.Count - 1 downto 0 do
      TVKLibMaterial(FLibMatUsers[i]).NotifyUsers;
    FinalizeShader;
  end;
end;

// BeginUpdate
//

procedure TVKShader.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// EndUpdate
//

procedure TVKShader.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyChange(Self);
end;

// DoInitialize
//

procedure TVKShader.DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
begin
  // nothing here
end;

// DoFinalize
//

procedure TVKShader.DoFinalize;
begin
  // nothing here
end;

// GetShaderInitialized
//

function TVKShader.GetShaderInitialized: Boolean;
begin
  Result := (FVirtualHandle.Handle <> 0);
end;

// InitializeShader
//

procedure TVKShader.InitializeShader(var rci: TRenderContextInfo; Sender:
  TObject);
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

 procedure TVKShader.FinalizeShader;
begin
  FVirtualHandle.NotifyChangesOfData;
  DoFinalize;
end;

// Apply
//

procedure TVKShader.Apply(var rci: TRenderContextInfo; Sender: TObject);
begin
{$IFNDEF VKS_MULTITHREAD}
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

function TVKShader.UnApply(var rci: TRenderContextInfo): Boolean;
begin
{$IFNDEF VKS_MULTITHREAD}
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

procedure TVKShader.OnVirtualHandleDestroy(sender: TVKVirtualHandle; var handle:
  Cardinal);
begin
  handle := 0;
end;

// OnVirtualHandleAllocate
//

procedure TVKShader.OnVirtualHandleAllocate(sender: TVKVirtualHandle; var
  handle: Cardinal);
begin
  handle := 1;
end;

// SetEnabled
//

procedure TVKShader.SetEnabled(val: Boolean);
begin
{$IFNDEF VKS_MULTITHREAD}
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

procedure TVKShader.RegisterUser(libMat: TVKLibMaterial);
var
  i: Integer;
begin
  i := FLibMatUsers.IndexOf(libMat);
  if i < 0 then
    FLibMatUsers.Add(libMat);
end;

// UnRegisterUser
//

procedure TVKShader.UnRegisterUser(libMat: TVKLibMaterial);
begin
  if Assigned(FLibMatUsers) then
    FLibMatUsers.Remove(libMat);
end;

// Assign
//

procedure TVKShader.Assign(Source: TPersistent);
begin
  if Source is TVKShader then
  begin
    FShaderStyle := TVKShader(Source).FShaderStyle;
    FFailedInitAction := TVKShader(Source).FFailedInitAction;
    Enabled := TVKShader(Source).FEnabled;
  end
  else
    inherited Assign(Source); //to the pit of doom ;)
end;

// Assign
//

function TVKShader.ShaderSupported: Boolean;
begin
  Result := True;
end;

// HandleFailedInitialization
//

procedure TVKShader.HandleFailedInitialization(const LastErrorMessage: string =
  '');
begin
  case FailedInitAction of
    fiaSilentdisable: ; // Do nothing ;)
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
    //    fiaGenerateEvent:; // Do nothing. Event creation is left up to user shaders
    //                       // which may choose to override this procedure.
  else
    Assert(False, vksErrorEx + vksUnknownType);
  end;
end;

// GetStardardNotSupportedMessage
//

function TVKShader.GetStardardNotSupportedMessage: string;
begin
  if Name <> '' then
    Result := 'Your hardware/driver doesn''t support shader "' + Name + '"!'
  else
    Result := 'Your hardware/driver doesn''t support shader "' + ClassName +
      '"!';
end;

//----------------- TVKMaterial --------------------------------------------------

// Create
//

constructor TVKMaterial.Create(AOwner: TPersistent);
begin
  inherited;
  FFrontProperties := TVKFaceProperties.Create(Self);
  FTexture := nil; // AutoCreate
  FFaceCulling := fcBufferDefault;
  FPolygonMode := pmFill;
  FBlendingParams := TVKBlendingParameters.Create(Self);
  FDepthProperties := TVKDepthProperties.Create(Self)
end;

// Destroy
//

destructor TVKMaterial.Destroy;
begin
  if Assigned(currentLibMaterial) then
    currentLibMaterial.UnregisterUser(Self);
  FBackProperties.Free;
  FFrontProperties.Free;
  FDepthProperties.Free;
  FTexture.Free;
  FTextureEx.Free;
  FBlendingParams.Free;
  inherited Destroy;
end;

// GetMaterialLibrary
//

function TVKMaterial.GetMaterialLibrary: TVKAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

// SetBackProperties
//

procedure TVKMaterial.SetBackProperties(Values: TVKFaceProperties);
begin
  BackProperties.Assign(Values);
  NotifyChange(Self);
end;

// GetBackProperties
//

function TVKMaterial.GetBackProperties: TVKFaceProperties;
begin
  if not Assigned(FBackProperties) then
    FBackProperties := TVKFaceProperties.Create(Self);
  Result := FBackProperties;
end;

// SetFrontProperties
//

procedure TVKMaterial.SetFrontProperties(Values: TVKFaceProperties);
begin
  FFrontProperties.Assign(Values);
  NotifyChange(Self);
end;

// TVKMaterial
//

procedure TVKMaterial.SetDepthProperties(Values: TVKDepthProperties);
begin
  FDepthProperties.Assign(Values);
  NotifyChange(Self);
end;

// SetBlendingMode
//

procedure TVKMaterial.SetBlendingMode(const val: TBlendingMode);
begin
  if val <> FBlendingMode then
  begin
    FBlendingMode := val;
    NotifyChange(Self);
  end;
end;

// SetMaterialOptions
//

procedure TVKMaterial.SetMaterialOptions(const val: TMaterialOptions);
begin
  if val <> FMaterialOptions then
  begin
    FMaterialOptions := val;
    NotifyChange(Self);
  end;
end;

// GetTexture
//

function TVKMaterial.GetTexture: TVKTexture;
begin
  if not Assigned(FTexture) then
    FTexture := TVKTexture.Create(Self);
  Result := FTexture;
end;

// SetTexture
//

procedure TVKMaterial.SetTexture(aTexture: TVKTexture);
begin
  if Assigned(aTexture) then
    Texture.Assign(ATexture)
  else
    FreeAndNil(FTexture);
end;

// SetFaceCulling
//

procedure TVKMaterial.SetFaceCulling(const val: TFaceCulling);
begin
  if val <> FFaceCulling then
  begin
    FFaceCulling := val;
    NotifyChange(Self);
  end;
end;

// SetMaterialLibrary
//

procedure TVKMaterial.SetMaterialLibrary(const val: TVKAbstractMaterialLibrary);
begin
  FMaterialLibrary := val;
  SetLibMaterialName(LibMaterialName);
end;

// SetLibMaterialName
//

procedure TVKMaterial.SetLibMaterialName(const val: TVKLibMaterialName);
var
  oldLibrary: TVKMaterialLibrary;

  function MaterialLoopFrom(curMat: TVKLibMaterial): Boolean;
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
  newLibMaterial: TVKAbstractLibMaterial;
begin
  // locate new libmaterial
  if Assigned(FMaterialLibrary) then
    newLibMaterial := FMaterialLibrary.FMaterials.GetMaterial(val)
  else
    newLibMaterial := nil;

   // make sure new won't trigger an infinite loop
  if FMaterialLibrary is TVKMaterialLibrary then
  begin
    oldLibrary := TVKMaterialLibrary(FMaterialLibrary);
    if MaterialLoopFrom(TVKLibMaterial(newLibMaterial)) then
    begin
      if IsDesignTime then
        InformationDlg(Format(strCyclicRefMat, [val]))
      else
        GLSLogger.LogErrorFmt(strCyclicRefMat, [val]);
      exit;
    end;
  end;

  FLibMaterialName := val;
  // unregister if required
  if newLibMaterial <> currentLibMaterial then
  begin
    // unregister from old
    if Assigned(currentLibMaterial) then
      currentLibMaterial.UnregisterUser(Self);
    currentLibMaterial := newLibMaterial;
    // register with new
    if Assigned(currentLibMaterial) then
      currentLibMaterial.RegisterUser(Self);
    NotifyTexMapChange(Self);
  end;
end;

// GetTextureEx
//

function TVKMaterial.GetTextureEx: TVKTextureEx;
begin
  if not Assigned(FTextureEx) then
    FTextureEx := TVKTextureEx.Create(Self);
  Result := FTextureEx;
end;

// SetTextureEx
//

procedure TVKMaterial.SetTextureEx(const Value: TVKTextureEx);
begin
  if Assigned(Value) or Assigned(FTextureEx) then
    TextureEx.Assign(Value);
end;

// StoreTextureEx
//

function TVKMaterial.StoreTextureEx: Boolean;
begin
  Result := (Assigned(FTextureEx) and (TextureEx.Count > 0));
end;

// SetBlendingParams
//

procedure TVKMaterial.SetBlendingParams(const Value: TVKBlendingParameters);
begin
  FBlendingParams.Assign(Value);
  NotifyChange(Self);
end;

// NotifyLibMaterialDestruction
//

procedure TVKMaterial.NotifyLibMaterialDestruction;
begin
  FMaterialLibrary := nil;
  FLibMaterialName := '';
  currentLibMaterial := nil;
end;

// Loaded
//

procedure TVKMaterial.Loaded;
begin
  inherited;
  if Assigned(FTextureEx) then
    TextureEx.Loaded;
end;

// StoreMaterialProps
//

function TVKMaterial.StoreMaterialProps: Boolean;
begin
  Result := not Assigned(currentLibMaterial);
end;

// PrepareBuildList
//

procedure TVKMaterial.PrepareBuildList;
begin
  if Assigned(FTexture) and (not FTexture.Disabled) then
    FTexture.PrepareBuildList;
end;

// Apply
//

procedure TVKMaterial.Apply(var rci: TRenderContextInfo);
begin
  if Assigned(currentLibMaterial) then
    currentLibMaterial.Apply(rci)
  else
  with rci.GLStates do
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
      fcCull: Enable(stCullFace);
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

// UnApply
//

function TVKMaterial.UnApply(var rci: TRenderContextInfo): Boolean;
begin
  if Assigned(currentLibMaterial) then
    Result := currentLibMaterial.UnApply(rci)
  else
  begin
    if Assigned(FTexture) and (not FTexture.Disabled) and (not
      FTextureEx.IsTextureEnabled(0)) then
      FTexture.UnApply(rci)
    else if Assigned(FTextureEx) then
      FTextureEx.UnApply(rci);
    Result := False;
  end;
end;

// Assign
//

procedure TVKMaterial.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKMaterial) then
  begin
    if Assigned(TVKMaterial(Source).FBackProperties) then
      BackProperties.Assign(TVKMaterial(Source).BackProperties)
    else
      FreeAndNil(FBackProperties);
    FFrontProperties.Assign(TVKMaterial(Source).FFrontProperties);
    FPolygonMode := TVKMaterial(Source).FPolygonMode;
    FBlendingMode := TVKMaterial(Source).FBlendingMode;
    FMaterialOptions := TVKMaterial(Source).FMaterialOptions;
    if Assigned(TVKMaterial(Source).FTexture) then
      Texture.Assign(TVKMaterial(Source).FTexture)
    else
      FreeAndNil(FTexture);
    FFaceCulling := TVKMaterial(Source).FFaceCulling;
    FMaterialLibrary := TVKMaterial(Source).MaterialLibrary;
    SetLibMaterialName(TVKMaterial(Source).LibMaterialName);
    TextureEx.Assign(TVKMaterial(Source).TextureEx);
    FDepthProperties.Assign(TVKMaterial(Source).DepthProperties);
    NotifyChange(Self);
  end
  else
    inherited;
end;

// NotifyChange
//

procedure TVKMaterial.NotifyChange(Sender: TObject);
var
  intf: IGLNotifyAble;
begin
  if Supports(Owner, IGLNotifyAble, intf) then
    intf.NotifyChange(Self);
end;

// NotifyTexMapChange
//

procedure TVKMaterial.NotifyTexMapChange(Sender: TObject);
var
  intf: IGLTextureNotifyAble;
begin
  if Supports(Owner, IGLTextureNotifyAble, intf) then
    intf.NotifyTexMapChange(Self)
  else
    NotifyChange(Self);
end;

// DestroyHandles
//

procedure TVKMaterial.DestroyHandles;
begin
  if Assigned(FTexture) then
    FTexture.DestroyHandles;
end;

// Blended
//

function TVKMaterial.Blended: Boolean;
begin
  if Assigned(currentLibMaterial) then
  begin

    Result := currentLibMaterial.Blended
  end
  else
    Result := not (BlendingMode in [bmOpaque, bmAlphaTest50, bmAlphaTest100, bmCustom]);
end;

// HasSecondaryTexture
//

function TVKMaterial.HasSecondaryTexture: Boolean;
begin
  Result := Assigned(currentLibMaterial)
    and (currentLibMaterial is TVKLibMaterial)
    and Assigned(TVKLibMaterial(currentLibMaterial).libMatTexture2);
end;

// MaterialIsLinkedToLib
//

function TVKMaterial.MaterialIsLinkedToLib: Boolean;
begin
  Result := Assigned(currentLibMaterial);
end;

// GetActualPrimaryTexture
//

function TVKMaterial.GetActualPrimaryTexture: TVKTexture;
begin
  if Assigned(currentLibMaterial) and (currentLibMaterial is TVKLibMaterial) then
    Result := TVKLibMaterial(currentLibMaterial).Material.Texture
  else
    Result := Texture;
end;

// GetActualPrimaryTexture
//

function TVKMaterial.GetActualPrimaryMaterial: TVKMaterial;
begin
  if Assigned(currentLibMaterial) and (currentLibMaterial is TVKLibMaterial) then
    Result := TVKLibMaterial(currentLibMaterial).Material
  else
    Result := Self;
end;

// QuickAssignMaterial
//

function TVKMaterial.GetLibMaterial: TVKLibMaterial;
begin
  if Assigned(currentLibMaterial) and (currentLibMaterial is TVKLibMaterial) then
    Result := TVKLibMaterial(currentLibMaterial)
  else
    Result := nil;
end;

// QuickAssignMaterial
//

procedure TVKMaterial.QuickAssignMaterial(const MaterialLibrary:
  TVKMaterialLibrary; const Material: TVKLibMaterial);
begin
  FMaterialLibrary := MaterialLibrary;
  FLibMaterialName := Material.FName;

  if Material <> CurrentLibMaterial then
  begin
    // unregister from old
    if Assigned(CurrentLibMaterial) then
      currentLibMaterial.UnregisterUser(Self);
    CurrentLibMaterial := Material;
    // register with new
    if Assigned(CurrentLibMaterial) then
      CurrentLibMaterial.RegisterUser(Self);

    NotifyTexMapChange(Self);
  end;
end;

// SetPolygonMode
//

procedure TVKMaterial.SetPolygonMode(AValue: TPolygonMode);
begin
  if AValue <> FPolygonMode then
  begin
    FPolygonMode := AValue;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TVKAbstractLibMaterial ------------------
// ------------------
{$IFDEF VKS_REGION}{$REGION 'TVKAbstractLibMaterial'}{$ENDIF}

// Create
//
constructor TVKAbstractLibMaterial.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FUserList := TList.Create;
  if Assigned(ACollection) then
  begin
    FName := TVKAbstractLibMaterials(ACollection).MakeUniqueName('LibMaterial');
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

// Destroy
//

destructor TVKAbstractLibMaterial.Destroy;
begin
  FUserList.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVKAbstractLibMaterial.Assign(Source: TPersistent);
begin
  if Source is TVKAbstractLibMaterial then
  begin
    FName :=
      TVKLibMaterials(Collection).MakeUniqueName(TVKLibMaterial(Source).Name);
    FNameHashKey := ComputeNameHashKey(FName);
  end
  else
    inherited; // Raise AssignError
end;

// QueryInterface
//

function TVKAbstractLibMaterial.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

// _AddRef
//
function TVKAbstractLibMaterial._AddRef: Integer; stdcall;
begin
  Result := -1; //ignore
end;

// _Release
//
function TVKAbstractLibMaterial._Release: Integer; stdcall;
begin
  Result := -1; //ignore
end;

// RegisterUser
//

procedure TVKAbstractLibMaterial.RegisterUser(obj: TVKUpdateAbleObject);
begin
  Assert(FUserList.IndexOf(obj) < 0);
  FUserList.Add(obj);
end;

// UnregisterUser
//

procedure TVKAbstractLibMaterial.UnRegisterUser(obj: TVKUpdateAbleObject);
begin
  FUserList.Remove(obj);
end;

// RegisterUser
//

procedure TVKAbstractLibMaterial.RegisterUser(comp: TVKUpdateAbleComponent);
begin
  Assert(FUserList.IndexOf(comp) < 0);
  FUserList.Add(comp);
end;

// UnregisterUser
//

procedure TVKAbstractLibMaterial.UnRegisterUser(comp: TVKUpdateAbleComponent);
begin
  FUserList.Remove(comp);
end;

// RegisterUser
//

procedure TVKAbstractLibMaterial.RegisterUser(libMaterial: TVKLibMaterial);
begin
  Assert(FUserList.IndexOf(libMaterial) < 0);
  FUserList.Add(libMaterial);
end;

// UnregisterUser
//

procedure TVKAbstractLibMaterial.UnRegisterUser(libMaterial: TVKLibMaterial);
begin
  FUserList.Remove(libMaterial);
end;

// NotifyUsers
//

procedure TVKAbstractLibMaterial.NotifyChange(Sender: TObject);
begin
  NotifyUsers();
end;

// NotifyUsers
//

procedure TVKAbstractLibMaterial.NotifyUsers;
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
      if obj is TVKUpdateAbleObject then
        TVKUpdateAbleObject(FUserList[i]).NotifyChange(Self)
      else if obj is TVKUpdateAbleComponent then
        TVKUpdateAbleComponent(FUserList[i]).NotifyChange(Self)
      else
      begin
        Assert(obj is TVKAbstractLibMaterial);
        TVKAbstractLibMaterial(FUserList[i]).NotifyUsers;
      end;
    end;
  finally
    FNotifying := False;
  end;
end;

// IsUsed
//

function TVKAbstractLibMaterial.IsUsed: Boolean;
begin
  Result := Assigned(Self) and (FUserlist.Count > 0);
end;

// GetDisplayName
//

function TVKAbstractLibMaterial.GetDisplayName: string;
begin
  Result := Name;
end;

// GetMaterialLibrary
//

function TVKAbstractLibMaterial.GetMaterialLibrary: TVKAbstractMaterialLibrary;
var
  LOwner: TPersistent;
begin
  Result := nil;
  if Assigned(Collection) then
  begin
    LOwner := TVKAbstractLibMaterials(Collection).Owner;
    if LOwner is TVKAbstractMaterialLibrary then
      Result := TVKAbstractMaterialLibrary(LOwner);
  end;
end;

// Blended
//

function TVKAbstractLibMaterial.Blended: Boolean;
begin
  Result := False;
end;

// ComputeNameHashKey
//

class function TVKAbstractLibMaterial.ComputeNameHashKey(const name: string): Integer;
var
  i, n: Integer;
begin
  n := Length(name);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(name[i]);
end;

// SetName
//

procedure TVKAbstractLibMaterial.SetName(const val: TVKLibMaterialName);
begin
  if val <> FName then
  begin
    if not (csLoading in TComponent(Collection.Owner).ComponentState) then
    begin
      if TVKLibMaterials(Collection).GetLibMaterialByName(val) <> Self then
        FName := TVKLibMaterials(Collection).MakeUniqueName(val)
      else
        FName := val;
    end
    else
      FName := val;
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TVKLibMaterial ------------------
// ------------------
{$IFDEF VKS_REGION}{$REGION 'TVKLibMaterial'}{$ENDIF}

// Create
//

constructor TVKLibMaterial.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMaterial := TVKMaterial.Create(Self);
  FMaterial.Texture.OnTextureNeeded := DoOnTextureNeeded;
  FTextureOffset := TVKCoordinates.CreateInitialized(Self, NullHmgVector, csPoint);
  FTextureOffset.OnNotifyChange := OnNotifyChange;
  FTextureScale := TVKCoordinates.CreateInitialized(Self, XYZHmgVector, csPoint);
  FTextureScale.OnNotifyChange := OnNotifyChange;
  FTextureRotate := 0;
  FTextureOverride := False;
  FTextureMatrixIsIdentity := True;
end;

// Destroy
//

destructor TVKLibMaterial.Destroy;
var
  i: Integer;
  matObj: TObject;
begin
  Shader := nil; // drop dependency
  Texture2Name := ''; // drop dependency
  for i := 0 to FUserList.Count - 1 do
  begin
    matObj := TObject(FUserList[i]);
    if matObj is TVKMaterial then
      TVKMaterial(matObj).NotifyLibMaterialDestruction
    else if matObj is TVKLibMaterial then
    begin
      TVKLibMaterial(matObj).libMatTexture2 := nil;
      TVKLibMaterial(matObj).FTexture2Name := '';
    end;
  end;
  FMaterial.Free;
  FTextureOffset.Free;
  FTextureScale.Free;
  inherited;
end;

// Assign
//

procedure TVKLibMaterial.Assign(Source: TPersistent);
begin
  if Source is TVKLibMaterial then
  begin
    FMaterial.Assign(TVKLibMaterial(Source).Material);
    FTextureOffset.Assign(TVKLibMaterial(Source).TextureOffset);
    FTextureScale.Assign(TVKLibMaterial(Source).TextureScale);
    FTextureRotate := TVKLibMaterial(Source).TextureRotate;
    TextureMatrix := TVKLibMaterial(Source).TextureMatrix;
    FTextureOverride := TVKLibMaterial(Source).FTextureOverride;
    FTexture2Name := TVKLibMaterial(Source).Texture2Name;
    FShader := TVKLibMaterial(Source).Shader;
  end;
  inherited;
end;

function TVKLibMaterial.Blended: Boolean;
begin
  Result := Material.Blended;
end;

// PrepareBuildList
//

procedure TVKLibMaterial.PrepareBuildList;
begin
  if Assigned(Self) then
    Material.PrepareBuildList;
end;

// Apply
//

procedure TVKLibMaterial.Apply(var ARci: TRenderContextInfo);
var
  multitextured: Boolean;
begin
  xgl.BeginUpdate;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssHighLevel: Shader.Apply(ARci, Self);
      ssReplace:
        begin
          Shader.Apply(ARci, Self);
          Exit;
        end;
    end;
  end
  else
    ARci.GLStates.CurrentProgram := 0;
  if (Texture2Name <> '') and GL.ARB_multitexture and (not
    xgl.SecondTextureUnitForbidden) then
  begin
    if not Assigned(libMatTexture2) then
    begin
      libMatTexture2 :=
        TVKLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
      if Assigned(libMatTexture2) then
        libMatTexture2.RegisterUser(Self)
      else
        FTexture2Name := '';
    end;
    multitextured := Assigned(libMatTexture2)
      and (not libMatTexture2.Material.Texture.Disabled);
  end
  else
    multitextured := False;
  if not multitextured then
  begin
    // no multitexturing ("standard" mode)
    if not FTextureMatrixIsIdentity then
        ARci.GLStates.SetGLTextureMatrix(FTextureMatrix);
    Material.Apply(ARci);
  end
  else
  begin
    // multitexturing is ON
    if not FTextureMatrixIsIdentity then
      ARci.GLStates.SetGLTextureMatrix(FTextureMatrix);
    Material.Apply(ARci);

    if not libMatTexture2.FTextureMatrixIsIdentity then
      libMatTexture2.Material.Texture.ApplyAsTexture2(ARci,
        @libMatTexture2.FTextureMatrix.V[0].V[0])
    else
      libMatTexture2.Material.Texture.ApplyAsTexture2(ARci);

    if (not Material.Texture.Disabled) and (Material.Texture.MappingMode =
      tmmUser) then
      if libMatTexture2.Material.Texture.MappingMode = tmmUser then
        xgl.MapTexCoordToDual
      else
        xgl.MapTexCoordToMain
    else if libMatTexture2.Material.Texture.MappingMode = tmmUser then
      xgl.MapTexCoordToSecond
    else
      xgl.MapTexCoordToMain;

  end;
 
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssLowLevel: Shader.Apply(ARci, Self);
    end;
  end;
  xgl.EndUpdate;
end;

// UnApply
//

function TVKLibMaterial.UnApply(var ARci: TRenderContextInfo): Boolean;
begin
  Result := False;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssLowLevel: Result := Shader.UnApply(ARci);
      ssReplace:
        begin
          Result := Shader.UnApply(ARci);
          Exit;
        end;
    end;
  end;

  if not Result then
  begin
    if Assigned(libMatTexture2) and GL.ARB_multitexture and (not
      xgl.SecondTextureUnitForbidden) then
    begin
      libMatTexture2.Material.Texture.UnApplyAsTexture2(ARci, (not
        libMatTexture2.TextureMatrixIsIdentity));
      xgl.MapTexCoordToMain;
    end;
    Material.UnApply(ARci);
    if not Material.Texture.Disabled then
      if not FTextureMatrixIsIdentity then
        ARci.GLStates.ResetGLTextureMatrix;
    if Assigned(FShader) then
    begin
      case Shader.ShaderStyle of
        ssHighLevel: Result := Shader.UnApply(ARci);
      end;
    end;
  end;
end;

procedure TVKLibMaterial.NotifyTexMapChange(Sender: TObject);
begin
  NotifyUsersOfTexMapChange();
end;

// NotifyUsersOfTexMapChange
//

procedure TVKLibMaterial.NotifyUsersOfTexMapChange;
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
      if obj is TVKMaterial then
        TVKMaterial(FUserList[i]).NotifyTexMapChange(Self)
      else if obj is TVKLibMaterial then
        TVKLibMaterial(FUserList[i]).NotifyUsersOfTexMapChange
      else if obj is TVKUpdateAbleObject then
        TVKUpdateAbleObject(FUserList[i]).NotifyChange(Self)
      else if obj is TVKUpdateAbleComponent then
        TVKUpdateAbleComponent(FUserList[i]).NotifyChange(Self);
    end;
  finally
    FNotifying := False;
  end;
end;

// Loaded
//

procedure TVKLibMaterial.Loaded;
begin
  CalculateTextureMatrix;
  Material.Loaded;
end;

// SetMaterial
//

procedure TVKLibMaterial.SetMaterial(const val: TVKMaterial);
begin
  FMaterial.Assign(val);
end;

// SetTextureOffset
//

procedure TVKLibMaterial.SetTextureOffset(const val: TVKCoordinates);
begin
  FTextureOffset.AsVector := val.AsVector;
  CalculateTextureMatrix;
end;

// SetTextureScale
//

procedure TVKLibMaterial.SetTextureScale(const val: TVKCoordinates);
begin
  FTextureScale.AsVector := val.AsVector;
  CalculateTextureMatrix;
end;

// SetTextureMatrix
//

procedure TVKLibMaterial.SetTextureMatrix(const Value: TMatrix);
begin
  FTextureMatrixIsIdentity := CompareMem(@Value.V[0], @IdentityHmgMatrix.V[0], SizeOf(TMatrix));
  FTextureMatrix := Value;
  FTextureOverride := True;
  NotifyUsers;
end;

procedure TVKLibMaterial.SetTextureRotate(Value: Single);
begin
  if Value <> FTextureRotate then
  begin
    FTextureRotate := Value;
    CalculateTextureMatrix;
  end;
end;

function TVKLibMaterial.StoreTextureRotate: Boolean;
begin
  Result := Abs(FTextureRotate) > EPSILON;
end;

// SetTexture2
//

procedure TVKLibMaterial.SetTexture2Name(const val: TVKLibMaterialName);
begin
  if val <> Texture2Name then
  begin
    if Assigned(libMatTexture2) then
    begin
      libMatTexture2.UnregisterUser(Self);
      libMatTexture2 := nil;
    end;
    FTexture2Name := val;
    NotifyUsers;
  end;
end;

// SetShader
//

procedure TVKLibMaterial.SetShader(const val: TVKShader);
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

// CalculateTextureMatrix
//

procedure TVKLibMaterial.CalculateTextureMatrix;
begin
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
  NotifyUsers;
end;

// DestroyHandles
//

procedure TVKLibMaterial.DestroyHandles;
var
  libMat: TVKLibMaterial;
begin
  FMaterial.DestroyHandles;
  if FTexture2Name <> '' then
  begin
    libMat := TVKLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
    if Assigned(libMat) then
      libMat.DestroyHandles;
  end;
end;

// OnNotifyChange
//

procedure TVKLibMaterial.OnNotifyChange(Sender: TObject);
begin
  CalculateTextureMatrix;
end;

// DoOnTextureNeeded
//

procedure TVKLibMaterial.DoOnTextureNeeded(Sender: TObject; var textureFileName:
  string);
var
  mLib: TVKMaterialLibrary;
  i: Integer;
  tryName: string;
begin
  if not Assigned(Collection) then
    exit;
  mLib := TVKMaterialLibrary((Collection as TVKLibMaterials).GetOwner);
  with mLib do
    if Assigned(FOnTextureNeeded) then
      FOnTextureNeeded(mLib, textureFileName);
  // if a ':' is present, or if it starts with a '\', consider it as an absolute path
  if (Pos(':', textureFileName) > 0) or (Copy(textureFileName, 1, 1) = PathDelim)
    then
    Exit;
  // ok, not an absolute path, try given paths
  with mLib do
  begin
    if FTexturePathList <> nil then
      for i := 0 to FTexturePathList.Count - 1 do
      begin
        tryName := IncludeTrailingPathDelimiter(FTexturePathList[i]) +
          textureFileName;
        if (Assigned(vAFIOCreateFileStream) and FileStreamExists(tryName)) or
          FileExists(tryName) then
        begin
          textureFileName := tryName;
          Break;
        end;
      end;
  end;
end;
{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TVKLibMaterials ------------------
// ------------------
 {$IFDEF VKS_REGION}{$REGION 'TVKLibMaterials'}{$ENDIF}

 // MakeUniqueName
//

function TVKAbstractLibMaterials.GetMaterial(const AName: TVKLibMaterialName):
  TVKAbstractLibMaterial;
var
  i, hk: Integer;
  lm: TVKAbstractLibMaterial;
begin
  hk := TVKAbstractLibMaterial.ComputeNameHashKey(AName);
  for i := 0 to Count - 1 do
  begin
    lm := TVKAbstractLibMaterial(inherited Items[i]);
    if (lm.NameHashKey = hk) and (lm.Name = AName) then
    begin
      Result := lm;
      Exit;
    end;
  end;
  Result := nil;
end;

// Loaded
//

procedure TVKAbstractLibMaterials.Loaded;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    TVKAbstractLibMaterial(Items[I]).Loaded;
end;

function TVKAbstractLibMaterials.MakeUniqueName(const nameRoot: TVKLibMaterialName):
  TVKLibMaterialName;
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

// Create
//

constructor TVKLibMaterials.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TVKLibMaterial);
end;

// SetItems
//

procedure TVKLibMaterials.SetItems(index: Integer; const val: TVKLibMaterial);
begin
  inherited Items[index] := val;
end;

// GetItems
//

function TVKLibMaterials.GetItems(index: Integer): TVKLibMaterial;
begin
  Result := TVKLibMaterial(inherited Items[index]);
end;

// DestroyHandles
//

procedure TVKLibMaterials.DestroyHandles;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].DestroyHandles;
end;

// Owner
//

function TVKLibMaterials.Owner: TPersistent;
begin
  Result := GetOwner;
end;

// Add
//

function TVKLibMaterials.Add: TVKLibMaterial;
begin
  Result := (inherited Add) as TVKLibMaterial;
end;

// FindItemID
//

function TVKLibMaterials.FindItemID(ID: Integer): TVKLibMaterial;
begin
  Result := (inherited FindItemID(ID)) as TVKLibMaterial;
end;

// GetLibMaterialByName
//

function TVKLibMaterials.GetLibMaterialByName(const AName: TVKLibMaterialName):
  TVKLibMaterial;
var
  LMaterial: TVKAbstractLibMaterial;
begin
  LMaterial := GetMaterial(AName);
  if Assigned(LMaterial) and (LMaterial is TVKLibMaterial) then
    Result := TVKLibMaterial(LMaterial)
  else
    Result := nil;
end;

// GetTextureIndex
//

function TVKLibMaterials.GetTextureIndex(const Texture: TVKTexture): Integer;
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I).Material.Texture = Texture then
      begin
        Result := I;
        Exit;
      end;
  Result := -1;
end;

// GetMaterialIndex
//

function TVKLibMaterials.GetMaterialIndex(const Material: TVKMaterial): Integer;
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I).Material = Material then
      begin
        Result := I;
        Exit;
      end;
  Result := -1;
end;

// GetMaterialIndex
//

function TVKLibMaterials.GetNameOfTexture(const Texture: TVKTexture):
  TVKLibMaterialName;
var
  MatIndex: Integer;
begin
  MatIndex := GetTextureIndex(Texture);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).Name
  else
    Result := '';
end;

// GetNameOfMaterial
//

function TVKLibMaterials.GetNameOfLibMaterial(const Material: TVKLibMaterial):
  TVKLibMaterialName;
var
  MatIndex: Integer;
begin
  MatIndex := IndexOf(Material);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).Name
  else
    Result := '';
end;

// IndexOf
//

function TVKLibMaterials.IndexOf(const Item: TVKLibMaterial): Integer;
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

// PrepareBuildList
//

procedure TVKLibMaterials.PrepareBuildList;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TVKLibMaterial(inherited Items[i]).PrepareBuildList;
end;

// DeleteUnusedMaterials
//

procedure TVKLibMaterials.DeleteUnusedMaterials;
var
  i: Integer;
  gotNone: Boolean;
begin
  BeginUpdate;
  repeat
    gotNone := True;
    for i := Count - 1 downto 0 do
    begin
      if TVKLibMaterial(inherited Items[i]).FUserList.Count = 0 then
      begin
        TVKLibMaterial(inherited Items[i]).Free;
        gotNone := False;
      end;
    end;
  until gotNone;
  EndUpdate;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF VKS_REGION}{$REGION 'TVKAbstractMaterialLibrary'}{$ENDIF}

// SetTexturePaths
//

procedure TVKAbstractMaterialLibrary.SetTexturePaths(const val: string);
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

// ApplyMaterial
//

function TVKAbstractMaterialLibrary.ApplyMaterial(const AName: string;
  var ARci: TRenderContextInfo): Boolean;
begin
  FLastAppliedMaterial := FMaterials.GetMaterial(AName);
  Result := Assigned(FLastAppliedMaterial);
  if Result then
    FLastAppliedMaterial.Apply(ARci);
end;

// UnApplyMaterial
//

function TVKAbstractMaterialLibrary.UnApplyMaterial(
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

// SetNamesToTStrings
//

procedure TVKAbstractMaterialLibrary.SetNamesToTStrings(AStrings: TStrings);
var
  i: Integer;
  lm: TVKAbstractLibMaterial;
begin
  with AStrings do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to FMaterials.Count - 1 do
    begin
      lm := TVKAbstractLibMaterial(FMaterials.Items[i]);
      AddObject(lm.Name, lm);
    end;
    EndUpdate;
  end;
end;

// Loaded
//

procedure TVKAbstractMaterialLibrary.Loaded;
begin
  inherited;
  FMaterials.Loaded;
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TVKMaterialLibrary ------------------
// ------------------

{$IFDEF VKS_REGION}{$REGION 'TVKMaterialLibrary'}{$ENDIF}

// Create
//

constructor TVKMaterialLibrary.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TVKLibMaterials.Create(Self);
end;

// Destroy
//

destructor TVKMaterialLibrary.Destroy;
begin
  Assert(FLastAppliedMaterial = nil, 'Unbalanced material application');
  FTexturePathList.Free;
  FMaterials.Free;
  FMaterials := nil;
  inherited;
end;

// DestroyHandles
//

procedure TVKMaterialLibrary.DestroyHandles;
begin
  if Assigned(FMaterials) then
    Materials.DestroyHandles;
end;

// SetMaterials
//

procedure TVKMaterialLibrary.SetMaterials(const val: TVKLibMaterials);
begin
  FMaterials.Assign(val);
end;

// StoreMaterials
//

function TVKMaterialLibrary.StoreMaterials: Boolean;
begin
  Result := (FMaterials.Count > 0);
end;

// WriteToFiler
//

procedure TVKMaterialLibrary.WriteToFiler(writer: TVirtualWriter);
var
  i, j: Integer;
  libMat: TVKLibMaterial;
  tex: TVKTexture;
  img: TVKTextureImage;
  pim: TVKPersistentImage;
  ss: TStringStream;
  bmp: TVKBitmap;
  texExItem: TVKTextureExItem;
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
      WriteString(libMat.Name);
      tex := libMat.Material.Texture;
      img := tex.Image;
      pim := TVKPersistentImage(img);
      if tex.Enabled and
             (img is TVKPersistentImage) and (pim.Picture.Bitmap <>
        nil) then
      begin
        WriteBoolean(true);
        ss := TStringStream.Create('');
        try
          bmp := TVKBitmap.Create;
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

      //version 1
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
          WriteBoolean(TRUE);
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
          WriteBoolean(FALSE);

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
        pim := TVKPersistentImage(img);
        if texExItem.Texture.Enabled and (img is TVKPersistentImage)
          and (pim.Picture.Bitmap <> nil) then
        begin
          WriteBoolean(True);
          ss := TStringStream.Create('');
          try
            bmp := TVKBitmap.Create;
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

// ReadFromFiler
//

procedure TVKMaterialLibrary.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
  libMat: TVKLibMaterial;
  i, n, size, tex, texCount: Integer;
  LName: string;
  ss: TStringStream;
  bmp: TVKBitmap;
  texExItem: TVKTextureExItem;
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
            bmp := TVKBitmap.Create;
            try
              bmp.LoadFromStream(ss);
              if libMat = nil then
                { TODO : E2250 There is no overloaded version of 'AddTextureMaterial' that can be called with these arguments }
                (*libMat := AddTextureMaterial(LName, bmp)*)
              else
                libMat.Material.Texture.Image.Assign(bmp);
            finally
              bmp.Free;
            end;
          finally
            ss.Free;
          end;

          // version 3
          if archiveVersion >= 3 then
            with libMat.Material.Texture do
            begin
              Read(BorderColor.AsAddress^, SizeOf(Single) * 4);
              Compression := TVKTextureCompression(ReadInteger);
              DepthTextureMode := TVKDepthTextureMode(ReadInteger);
              Read(EnvColor.AsAddress^, SizeOf(Single) * 4);
              FilteringQuality := TVKTextureFilteringQuality(ReadInteger);
              ImageAlpha := TVKTextureImageAlpha(ReadInteger);
              ImageBrightness := ReadFloat;
              ImageGamma := ReadFloat;
              MagFilter := TVKMagFilter(ReadInteger);
              MappingMode := TVKTextureMappingMode(ReadInteger);
              Read(MappingSCoordinates.AsAddress^, SizeOf(Single) * 4);
              Read(MappingTCoordinates.AsAddress^, SizeOf(Single) * 4);
              Read(MappingRCoordinates.AsAddress^, SizeOf(Single) * 4);
              Read(MappingQCoordinates.AsAddress^, SizeOf(Single) * 4);
              MinFilter := TVKMinFilter(ReadInteger);
              NormalMapScale := ReadFloat;
              TextureCompareFunc := TVKDepthCompareFunc(ReadInteger);
              TextureCompareMode := TVKTextureCompareMode(ReadInteger);
              TextureFormat := TVKTextureFormat(ReadInteger);
              TextureMode := TVKTextureMode(ReadInteger);
              TextureWrap := TVKTextureWrap(ReadInteger);
              TextureWrapR := TVKSeparateTextureWrap(ReadInteger);
              TextureWrapS := TVKSeparateTextureWrap(ReadInteger);
              TextureWrapT := TVKSeparateTextureWrap(ReadInteger);
            end;
          // version 3 end

        end
        else
        begin
          if libMat = nil then
          begin
            libMat := Materials.Add;
            libMat.Name := LName;
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
            libMat.Material.PolygonMode := TPolygonMode(ReadInteger);
          end;
          with libMat.Material.BackProperties do
          begin
            Read(Ambient.AsAddress^, SizeOf(Single) * 3);
            Read(Diffuse.AsAddress^, SizeOf(Single) * 4);
            Read(Emission.AsAddress^, SizeOf(Single) * 3);
            Read(Specular.AsAddress^, SizeOf(Single) * 3);
            Read(FShininess, 1);
            { PolygonMode := TPolygonMode(} ReadInteger;
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
                BlendFuncDFactor := TBlendFunction(ReadInteger);
                BlendFuncSFactor := TBlendFunction(ReadInteger);
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
              bmp := TVKBitmap.Create;
              try
                bmp.LoadFromStream(ss);
                texExItem.Texture.Image.Assign(bmp);
                texExItem.Texture.Enabled := True;
              finally
                bmp.Free;
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

// SaveToStream
//

procedure TVKMaterialLibrary.SaveToStream(aStream: TStream);
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

// LoadFromStream
//

procedure TVKMaterialLibrary.LoadFromStream(aStream: TStream);
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

// AddMaterialsFromStream
//

procedure TVKMaterialLibrary.AddMaterialsFromStream(aStream: TStream);
begin
  FDoNotClearMaterialsOnLoad := True;
  try
    LoadFromStream(aStream);
  finally
    FDoNotClearMaterialsOnLoad := False;
  end;
end;

// SaveToFile
//

procedure TVKMaterialLibrary.SaveToFile(const fileName: string);
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

// LoadFromFile
//

procedure TVKMaterialLibrary.LoadFromFile(const fileName: string);
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

// AddMaterialsFromFile
//

procedure TVKMaterialLibrary.AddMaterialsFromFile(const fileName: string);
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

// AddTextureMaterial
//

function TVKMaterialLibrary.AddTextureMaterial(const MaterialName, FileName:
  string;
  persistent: Boolean = True): TVKLibMaterial;
begin
  Result := Materials.Add;
  with Result do
  begin
    Name := materialName;
    with Material.Texture do
    begin
      MinFilter := miLinearMipmapLinear;
      MagFilter := maLinear;
      TextureMode := tmModulate;
      Disabled := False;
      if persistent then
      begin
        ImageClassName := TVKPersistentImage.ClassName;
        if fileName <> '' then
          Image.LoadFromFile(fileName);
      end
      else
      begin
        ImageClassName := TVKPicFileImage.ClassName;
        TVKPicFileImage(Image).PictureFileName := fileName;
      end;
    end;
  end;
end;

// AddTextureMaterial
//

function TVKMaterialLibrary.AddTextureMaterial(const materialName: string;
  graphic: TVKGraphic): TVKLibMaterial;
begin
  Result := Materials.Add;
  with Result do
  begin
    Name := materialName;
    with Material.Texture do
    begin
      MinFilter := miLinearMipmapLinear;
      MagFilter := maLinear;
      TextureMode := tmModulate;
      Disabled := False;
      Image.Assign(graphic);
    end;
  end;
end;

// LibMaterialByName
//

function TVKMaterialLibrary.LibMaterialByName(const AName: TVKLibMaterialName):
  TVKLibMaterial;
begin
  if Assigned(Self) then
    Result := Materials.GetLibMaterialByName(AName)
  else
    Result := nil;
end;

// TextureByName
//

function TVKMaterialLibrary.TextureByName(const LibMatName: TVKLibMaterialName):
  TVKTexture;
var
  LibMat: TVKLibMaterial;
begin
  if Self = nil then
    raise ETexture.Create(vksErrorEx + vksMatLibNotDefined)
  else if LibMatName = '' then
    Result := nil
  else
  begin
    LibMat := LibMaterialByName(LibMatName);
    if LibMat = nil then
      raise ETexture.CreateFmt(vksErrorEx + vksMaterialNotFoundInMatlibEx,
        [LibMatName])
    else
      Result := LibMat.Material.Texture;
  end;
end;

// GetNameOfTexture
//

function TVKMaterialLibrary.GetNameOfTexture(const Texture: TVKTexture):
  TVKLibMaterialName;
begin
  if (Self = nil) or (Texture = nil) then
    Result := ''
  else
    Result := Materials.GetNameOfTexture(Texture);
end;

// GetMaterials
//

function TVKMaterialLibrary.GetMaterials: TVKLibMaterials;
begin
  Result := TVKLibMaterials(FMaterials);
end;

// GetNameOfMaterial
//

function TVKMaterialLibrary.GetNameOfLibMaterial(const LibMat: TVKLibMaterial):
  TVKLibMaterialName;
begin
  if (Self = nil) or (LibMat = nil) then
    Result := ''
  else
    Result := Materials.GetNameOfLibMaterial(LibMat);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

{ TVKBlendingParameters }

{$IFDEF VKS_REGION}{$REGION 'TVKBlendingParameters'}{$ENDIF}

procedure TVKBlendingParameters.Apply(var rci: TRenderContextInfo);
begin
  if FUseAlphaFunc then
  begin
    rci.GLStates.Enable(stAlphaTest);
    rci.GLStates.SetGLAlphaFunction(FAlphaFuncType, FAlphaFuncRef);
  end
  else
    rci.GLStates.Disable(stAlphaTest);
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

constructor TVKBlendingParameters.Create(AOwner: TPersistent);
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

procedure TVKBlendingParameters.SetAlphaFuncRef(const Value: TGLclampf);
begin
  if (FAlphaFuncRef <> Value) then
  begin
    FAlphaFuncRef := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKBlendingParameters.SetAlphaFuncType(
  const Value: TGlAlphaFunc);
begin
  if (FAlphaFuncType <> Value) then
  begin
    FAlphaFuncType := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKBlendingParameters.SetBlendFuncDFactor(
  const Value: TBlendFunction);
begin
  if (FBlendFuncDFactor <> Value) then
  begin
    FBlendFuncDFactor := Value;
    if not FSeparateBlendFunc then
      FAlphaBlendFuncDFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKBlendingParameters.SetBlendFuncSFactor(
  const Value: TBlendFunction);
begin
  if (FBlendFuncSFactor <> Value) then
  begin
    FBlendFuncSFactor := Value;
    if not FSeparateBlendFunc then
      FAlphaBlendFuncSFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKBlendingParameters.SetAlphaBlendFuncDFactor(const Value: TBlendFunction);
begin
  if FSeparateBlendFunc and (FAlphaBlendFuncDFactor <> Value) then
  begin
    FAlphaBlendFuncDFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKBlendingParameters.SetAlphaBlendFuncSFactor(const Value: TBlendFunction);
begin
  if FSeparateBlendFunc and (FAlphaBlendFuncSFactor <> Value) then
  begin
    FAlphaBlendFuncSFactor := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKBlendingParameters.SetUseAlphaFunc(const Value: Boolean);
begin
  if (FUseAlphaFunc <> Value) then
  begin
    FUseAlphaFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKBlendingParameters.SetUseBlendFunc(const Value: Boolean);
begin
  if (FUseBlendFunc <> Value) then
  begin
    FUseBlendFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKBlendingParameters.SetSeparateBlendFunc(const Value: Boolean);
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

function TVKBlendingParameters.StoreAlphaFuncRef: Boolean;
begin
  Result := (Abs(AlphaFuncRef) > 0.001);
end;

{$IFDEF VKS_REGION}{$ENDREGION}{$ENDIF}

initialization

  RegisterClasses([TVKMaterialLibrary, TVKMaterial, TVKShader]);

end.