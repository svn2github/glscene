//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : DGLShader
  @HTML (
  <p>
  Handles all Shader Programs and Uniforms parameters
  Shader is the base of the object rendering. Shader is attached to an object. </p>
  <p>
  <ul>
  <li>The Shader is linked with a Material from a Material Library.</li>
  <li>The Shader is linked with Lights form a Light Library</li>
  <li>The Shader can apply automatically or manually uniforms parameters</li>
  </ul>
  The MVP matrix is automacilly retreive from the current RCI
  <br>
  The Uniforms "In_Vertex", "In_VertexColors", "In_TexCoord", "In_Normals", "In_Tagents" are send
  to the right uniform location by the object itself.
  These parameters are automatically send to GPU via the object's VAOs.
  <br>
  This library support OpenGL 3.3 (GLSL version #330 Core) minimum.
  <br>
  It's support DSA (Direct Shader Access)</p>

  <p>
  Shader have a multipass mechanism with the "NextPass" Parameters, so you can apply more than 1 shader to an object
  <br>
  Sample of the minimum GLSL Shader Script required :
  </p><p><br>
  - Vertex Program :<br>

     #VERSION 330 Core<br>
     attrib (location=0) vec3 In_Vertex<br>
     uniform mat4 ModelViewProjection;<br>
     out vec4 Position<br>
     void main() [<br>
         Position = ModelViewProjection * In_Vertex;<br>
     ]<br>
   </p><p>
  - Fragment Program :<br>
    <br>
    #VERSION 330 Core<br>
    in vec4 Position;<br>
    uniform vec4 DiffuseColor;<br>
    out vec4 FragColor;<br>
    void main() [<br>
      out FragColor = DiffuseColor;<br>
    ]<br>
    </p>
  <p>
  <b>History : </b><font size=-1><ul>
    <li>25/12/15 - JD - Created</li>
  </ul></font></p>
  <p>
  <ul>
    <li> Status : In progress </li>
    <li> Todo :
    <ul>
      <li>Add a ShaderModel's Collection in LibShader with a Register/Unregister function<br>
          Like this will can including and using our own Customs Shaders without having growing list of components<br>
          in palette components</li>
      <li>OpenGL 4.0 Atomic Counter support</li>
      <li>OpenGL 4.3 SubRoutine support</li>
      <li>OpenGL 4.4 SSBO (Sub Shader Buffer Object) support</li>
      <li>OpenGL 4.5 Compute program support</li>
    </ul></li>
  </ul>
  </p> )
}
unit DGLShader;

interface

{$I DGLEngine.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  // GLS
  DGLCrossPlatform,DGLResStrings,DGLSLog,dglOpenGL,
  DGLTypes,DGLBaseClasses,DGLPersistentClasses,DGLXCollection,DGLApplicationFileIO,DGLUtils,
  DGLContext,DGLContextHandles,DGLState,DGLRenderContextInfo,
  DGLVectorTypes,DGLVectorMaths,DGLCoordinates,DGLSLParameters,
  DGLTextureFormat,DGLMaterial;

Type
  // ****************************************************************************************
  TDGLShaderComponentName = string;
  TDGLLibShaderName        = string;
  TDGLAbstractShaderLibrary = class;
  TDGLLibShader             = class;
  TDGLShaderLibrary         = class;
  EDGLShaderException = class(Exception);

  // ****************************************************************************************
  // an interface for proper TGLLibShaderNameProperty support
  IDGLShaderLibrarySupported = interface(IInterface)
    ['{53129B31-5505-4F5D-B950-3A8DA7581630}']
    function GetShaderLibrary: TDGLAbstractShaderLibrary;
  end;

  // ****************************************************************************************
  // TDGLAbstractLibShader
  //
  TDGLAbstractLibShader = class( TCollectionItem,IDGLShaderLibrarySupported, IDGLNotifyAble)
  protected
    { Protected Declarations }
    FUserList: TList;
    FName: TDGLLibShaderName;
    FNameHashKey: Integer;
    FTag: Integer;
    FNotifying: Boolean; // used for recursivity protection

    //implementing IDGLShaderLibrarySupported
    function GetShaderLibrary: TDGLAbstractShaderLibrary;
    //implementing IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

  protected
    { Protected Declarations }
    function GetDisplayName: string; override;
    class function ComputeNameHashKey(const name: string): Integer;
    procedure SetName(const val: TDGLLibShaderName);
    procedure Loaded; virtual;

  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Apply(var ARci: TRenderContextInfo); virtual;
    //: Restore non-standard Shader states that were altered
    function UnApply(var ARci: TRenderContextInfo): Boolean; virtual;

    procedure RegisterUser(obj: TDGLUpdateAbleObject); overload;
    procedure UnregisterUser(obj: TDGLUpdateAbleObject); overload;
    procedure RegisterUser(comp: TDGLUpdateAbleComponent); overload;
    procedure UnregisterUser(comp: TDGLUpdateAbleComponent); overload;
    procedure RegisterUser(libMaterial: TDGLLibMaterial); overload;
    procedure UnregisterUser(libMaterial: TDGLLibMaterial); overload;
    procedure NotifyUsers;

    property NameHashKey: Integer read FNameHashKey;
    procedure NotifyChange(Sender: TObject); virtual;

    property ShaderLibrary: TDGLAbstractShaderLibrary read GetShaderLibrary;

  published
    { Published Declarations }
    property Name: TDGLLibShaderName read FName write SetName;
    property Tag: Integer read FTag write FTag;
  end;

  // ****************************************************************************************
  // TDGLAbstractLibShaders
  //
  TDGLAbstractLibShaders = class(TOwnedCollection)
  protected
    { Protected Declarations }
    procedure Loaded;
    function GetShader(const AName: TDGLLibShaderName): TDGLAbstractLibShader; {$IFDEF GLS_INLINE}inline;{$ENDIF}
  public
    function MakeUniqueName(const nameRoot: TDGLLibShaderName): TDGLLibShaderName; virtual;
  end;

  // ****************************************************************************************
  // TGLAbstractShaderLibrary
  //
  TDGLAbstractShaderLibrary = class(TDGLCadenceAbleComponent)
  protected
    { Protected Declarations }
    FShaders: TDGLAbstractLibShaders;
    FLastAppliedShader: TDGLAbstractLibShader;
    FShaderPaths: string;
    FShaderPathList: TStringList;
    procedure SetShaderPaths(const val: string);
    property ShaderPaths: string read FShaderPaths write SetShaderPaths;
    procedure Loaded; override;
  public
    { Public Declarations }

    procedure SetNamesToTStrings(AStrings: TStrings);
    { @HTML ( Applies the Shader of given name.<p>
       Returns False if the Shader could not be found. ake sure this
       call is balanced with a corresponding UnApplyShader (or an
       assertion will be triggered in the destructor).<br>
       If a material is already applied, and has not yet been unapplied,
       an assertion will be triggered. }
    function ApplyShader(const AName: string; var ARci: TRenderContextInfo): Boolean; virtual;
    { @HTML ( Un-applies the last applied material.<p>
       Use this function in conjunction with ApplyShader.<br>
       If no material was applied, an assertion will be triggered. }
    function UnApplyShader(var ARci: TRenderContextInfo): Boolean; virtual;
  end;

  // ****************************************************************************************
  // TDGLLibShaders
  //
  TDGLLibShaders = class(TDGLAbstractLibShaders)
  protected
    procedure SetItems(AIndex: Integer; const AValue: TDGLLibShader);
    function GetItems(AIndex: Integer): TDGLLibShader;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);

    function ShaderLibrary: TDGLShaderLibrary;

    function IndexOf(const Item: TDGLLibShader): Integer;
    function Add: TDGLLibShader;
    function FindItemID(ID: Integer): TDGLLibShader;
    property Items[index: Integer]: TDGLLibShader read GetItems write SetItems; default;
    function GetLibShaderByName(const AName: TDGLLibShaderName): TDGLLibShader;
  end;

  // ****************************************************************************************
  // TGLBaseShaderCollectionItem
  //
  TDGLBaseShaderComponentItem = class(TDGLXCollectionItem, IDGLShaderLibrarySupported)
  private
    { Private Declarations }
    FNameHashKey:  Integer;
    FUserList:     TDGLPersistentObjectList;

    FNotifying:    Boolean;
    FIsValid:      Boolean;
    function GetUserList: TDGLPersistentObjectList;
    function GetShaderLibEx: TDGLShaderLibrary;
  protected
    { Protected Declarations }
    procedure SetName(const AValue: TDGLShaderComponentName); override;
    procedure NotifyChange(Sender: TObject); virtual;
    property UserList: TDGLPersistentObjectList read GetUserList;
    procedure DoOnPrepare(Sender: TDGLContext); virtual; abstract;
  public
    { Public Declarations }
    destructor Destroy; override;

    procedure RegisterUser(AUser: TDGLUpdateAbleObject);
    procedure UnregisterUser(AUser: TDGLUpdateAbleObject);
    function GetUserCount: Integer;
    function GetShaderLibrary: TDGLAbstractShaderLibrary;

    property ShaderLibrary: TDGLShaderLibrary read GetShaderLibEx;
    property IsValid: Boolean read FIsValid;
  published
    { Published Declarations }
    property Name: TDGLShaderComponentName read GetName write SetName;
  end;

  CDGLBaseShaderCollectionItem = class of TDGLBaseShaderComponentItem;

  // ****************************************************************************************
  // TGLLibShaderProperty
  //
  TDGLLibShaderProperty = class(TDGLUpdateAbleObject, IDGLShaderLibrarySupported)
  protected
    { Protected Declarations }
    FEnabled:      Boolean;
    FNextPassName: TDGLLibShaderName;
    FNextPass: TDGLLibShader;

    function GetShader: TDGLLibShader;
    function GetShaderLibraryEx: TDGLShaderLibrary;
    procedure SetEnabled(AValue: Boolean); virtual;
    procedure SetNextPass(const AValue: TDGLLibShaderName);
    procedure Loaded; virtual;
    property NextPass: TDGLLibShaderName read FNextPassName write SetNextPass;
  public
    { Public Declarations }
    procedure NotifyChange(Sender: TObject); override;
    function GetShaderLibrary: TDGLAbstractShaderLibrary;

    property ShaderLibrary: TDGLShaderLibrary read GetShaderLibraryEx;
  published
    { Published Declarations }
    { @HTML ( Turns on/off shader application.}
    property Enabled: Boolean read FEnabled write SetEnabled;

  end;

  // ****************************************************************************************
  // TDGLSLShaderScript
  //
  TDGLSLShaderScript = class(TDGLBaseShaderComponentItem)
  protected
    { Protected Declarations }
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  private
    { Private Declarations }
    FHandle:              array [TDGLShaderType] of TDGLShaderHandle;
    FSource:              TStringList;
    FSourceFile:          string;
    FShaderType:          TDGLShaderType;
    FInfoLog:             string;
    FGeometryInput:       TDGLgsInTypes;
    FGeometryOutput:      TDGLgsOutTypes;
    FGeometryVerticesOut: TGLint;
    procedure SetSource(AValue: TStringList);
    procedure SetSourceFile(AValue: string);
    procedure SetShaderType(AValue: TDGLShaderType);
    procedure SetGeometryInput(AValue: TDGLgsInTypes);
    procedure SetGeometryOutput(AValue: TDGLgsOutTypes);
    procedure SetGeometryVerticesOut(AValue: TGLint);
    function GetHandle: TDGLShaderHandle;
  public
    { Public Declarations }
    constructor Create(AOwner: TDGLXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DoOnPrepare(Sender: TDGLContext); override;

    class function FriendlyName: string; override;

    procedure NotifyChange(Sender: TObject); override;
    property Handle: TDGLShaderHandle read GetHandle;
  published
    { Published Declarations }
    property Source:              TStringList read FSource write SetSource;
    property SourceFile:          string read FSourceFile write SetSourceFile;
    property ShaderType:          TDGLShaderType read FShaderType write SetShaderType default shtVertex;
    property InfoLog:             string read FInfoLog;
    property GeometryInput:       TDGLgsInTypes read FGeometryInput write SetGeometryInput default gsInPoints;
    property GeometryOutput:      TDGLgsOutTypes read FGeometryOutput write SetGeometryOutput default gsOutPoints;
    property GeometryVerticesOut: TGLint read FGeometryVerticesOut write SetGeometryVerticesOut default 1;
  end;

  // ****************************************************************************************
  // TGLAbstractShaderUniform
  //
  TDGLAbstractShaderUniform = class(TDGLUpdateAbleObject, IShaderParameter)
  protected
    { Protected Declarations }
    FName:         string;
    FNameHashCode: Integer;
    FType:         TDGLSLDataType;
    FSamplerType:  TDGLSLSamplerType;

    function GetName: string;
    function GetGLSLType: TDGLSLDataType;
    function GetGLSLSamplerType: TDGLSLSamplerType;

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

    function GetUInt: TGLUint; virtual;
    function GetUVec2: TVector2ui; virtual;
    function GetUVec3: TVector3ui; virtual;
    function GetUVec4: TVector4ui; virtual;

    procedure SetFloat(const Value: TGLFloat); virtual;
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

  CGLAbstractShaderUniform = class of TDGLAbstractShaderUniform;

  // ****************************************************************************************
  // TDGLShaderUniform
  //
  TDGLShaderUniform = class(TDGLAbstractShaderUniform, IShaderParameter)
  protected
    { Protected Declarations }
    FLocation:     TGLint;
    FStoreProgram: TGLUint;
    FAutoSet:      TUniformAutoSetMethod;
    function GetProgram: TGLUint;
    {$IFDEF GLS_INLINE} inline; {$ENDIF}
    procedure PushProgram;
    {$IFDEF GLS_INLINE} inline; {$ENDIF}
    procedure PopProgram;
    {$IFDEF GLS_INLINE} inline; {$ENDIF}
    function GetFloat: Single; override;
    function GetVec2: TVector2f; override;
    function GetVec3: TVector3f; override;
    function GetVec4: TVector; override;

    function GetInt: TGLint; override;
    function GetIVec2: TVector2i; override;
    function GetIVec3: TVector3i; override;
    function GetIVec4: TVector4i; override;

    function GetUInt: TGLUint; override;
    function GetUVec2: TVector2ui; override;
    function GetUVec3: TVector3ui; override;
    function GetUVec4: TVector4ui; override;

    procedure SetFloat(const Value: TGLFloat); override;
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
    property GLSLType: TDGLSLDataType read GetGLSLType;
  end;

  // ****************************************************************************************
  // TDGLShaderUniformDSA
  //
  TDGLShaderUniformDSA = class(TDGLShaderUniform)
  protected
    { Protected Declarations }
    procedure SetFloat(const Value: TGLFloat); override;
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

  // ****************************************************************************************
  // TDGLShaderUniformTexture
  //
  TDGLShaderUniformTexture = class(TDGLShaderUniform)
  private
    { Private Declarations }

    FLibTexture: TDGLAbstractTexture;
    FLibSampler: TDGLTextureSampler;
    FTarget:     TDGLTextureTarget;
    FSwizzling:  TSwizzleVector;
  protected
    { Protected Declarations }
    FLibTexureName:  TDGLMaterialComponentName;
    FLibSamplerName: TDGLMaterialComponentName;
    FMaterialLibrary  : TDGLMaterialLibrary;

    function GetTextureName: string; override;
    function GetSamplerName: string; override;
    function GetTextureSwizzle: TSwizzleVector; override;
    procedure SetTextureName(const AValue: string); override;
    procedure SetSamplerName(const AValue: string); override;
    procedure SetTextureSwizzle(const AValue: TSwizzleVector); override;

    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
    procedure Loaded;
    // implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TDGLMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TDGLMaterialLibrary);
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;

    procedure Apply(var ARci: TRenderContextInfo); override;
    // property MaterialLibrary
    // property LibMaterialName
    property MaterialLibrary: TDGLMaterialLibrary read GetMaterialLibrary write SetMaterialLibrary;
    property LibTextureName: TDGLMaterialComponentName read GetTextureName write SetTextureName;
    property LibSamplerName: TDGLMaterialComponentName read GetSamplerName write SetSamplerName;
    property GLSLSampler: TDGLSLSamplerType read GetGLSLSamplerType;
    property Swizzling: TSwizzleVector read GetTextureSwizzle write SetTextureSwizzle;
  end;

  // @TODO ADD a TDGLShaderUniformLight  ?????

  // ****************************************************************************************
  // TDGLShaderFailedInitAction
  //
  { @HTML ( Defines what to do if for some reason shader failed to initialize.<ul>
     <li>fiaSilentdisable:          just disable it
     <li>fiaRaiseHandledException:  raise an exception, and handle it right away
                                    (usefull, when debigging within Delphi)
     <li>fiaRaiseStardardException: raises the exception with a string from this
                                      function GetStardardNotSupportedMessage
     <li>fiaReRaiseException:       Re-raises the exception
     <li>fiaGenerateEvent:          Handles the exception, but generates an event
                                    that user can respond to. For example, he can
                                    try to compile a substitude shader, or replace
                                    it by a material.
                                    Note: HandleFailedInitialization does *not*
                                    create this event, it is left to user shaders
                                    which may chose to override this procedure.
                                    Commented out, because not sure if this
                                    option should exist, let other generations of
                                    developers decide ;)
     </ul> }
  TDGLShaderFailedInitAction = (
    fiaSilentDisable, fiaRaiseStandardException,
    fiaRaiseHandledException, fiaReRaiseException,fiaGenerateEvent);

  // ****************************************************************************************
  // TGLBaseShaderModel
  //
  { @HTML ( @HTML( Generic, abstract shader Model class.<p>
     Shaders are modeled here as an abstract material-altering entity with
     transaction-like behaviour. The base class provides basic context and user
     tracking, as well as setup/application facilities.<br>
     Subclasses are expected to provide implementation for DoInitialize,
     DoApply, DoUnApply and DoFinalize.</p> ) }
  TDGLAbstractShaderModel = class(TDGLLibShaderProperty)
  private
     FFailedInitAction: TDGLShaderFailedInitAction;
     FVirtualHandle: TDGLVirtualHandle;
     FUpdateCount: Integer;
     FShaderActive: Boolean;
  protected
    { Protected Declarations }

    { @HTML ( Invoked once, before the first call to DoApply.
       The call happens with the OpenGL context being active. }
    procedure DoInitialize(var rci: TRenderContextInfo); dynamic;
    { @HTML ( Request to apply the shader.
       Always followed by a DoUnApply when the shader is no longer needed. }
    procedure DoApply(var rci: TRenderContextInfo); virtual;
    { @HTML ( Request to un-apply the shader.
       Subclasses can assume the shader has been applied previously.<br>
       Return True to request a multipass. }
    function DoUnApply(var rci: TRenderContextInfo): Boolean; virtual;
    { @HTML ( Invoked once, before the destruction of context or release of shader.
       The call happens with the OpenGL context being active. }
    procedure DoFinalize; dynamic;
    procedure DoOnPrepare(Sender: TDGLContext);virtual;

     { @HTML ( Defines if shader is supported by hardware/drivers.
       Default - always supported. Descendants are encouraged to override
       this function. }
    class function IsSupported: Boolean; virtual; abstract;

    function GetShaderInitialized: Boolean;
    procedure InitializeShader(var rci: TRenderContextInfo);
    procedure FinalizeShader;
    procedure OnVirtualHandleAllocate(sender: TDGLVirtualHandle; var handle:Cardinal);
    procedure OnVirtualHandleDestroy(sender: TDGLVirtualHandle; var handle: Cardinal);

    { @HTML ( Used by the DoInitialize procedure of descendant classes to raise errors. }
    procedure HandleFailedInitialization(const LastErrorMessage: string = ''); virtual;

    { @HTML ( May be this should be a function inside HandleFailedInitialization... }
    function GetStandardNotSupportedMessage: string; virtual;

    //property ComputeShaderName : TDGLShaderComponentName index shtCompute read GetLibShaderName write SetLibShaderName;
    property ShaderInitialized: Boolean read GetShaderInitialized;
    property ShaderActive: Boolean read FShaderActive;
    { @HTML ( Defines what to do if for some reason shader failed to initialize.
       Note, that in some cases it cannon be determined by just checking the
       required OpenGL extentions. You need to try to compile and link the
       shader - only at that stage you might catch an error }
    property FailedInitAction: TDGLShaderFailedInitAction read FFailedInitAction write FFailedInitAction default fiaRaiseStandardException;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    { @HTML ( Subclasses should invoke this function when shader properties are altered.
        This procedure can also be used to reset/recompile the shader. }
    procedure NotifyChange(Sender: TObject); override;
    procedure BeginUpdate;
    procedure EndUpdate;

    { @HTML ( Apply shader to OpenGL state machine.}
    procedure Apply(var Rci: TRenderContextInfo); //virtual;
    { @HTML ( UnApply shader.
       When returning True, the caller is expected to perform a multipass
       rendering by re-rendering then invoking UnApply again, until a
       "False" is returned. }
    function UnApply(var Rci: TRenderContextInfo):Boolean; //virtual;


  published
    { Published Declarations }

    property NextPass;
  end;

  // ****************************************************************************************
  // TDGLCustomGLSLShaderModel
  //
  TDGLCustomGLSLShaderModel = class(TDGLAbstractShaderModel, IDGLMaterialLibrarySupported)
  private
    FHandle:        TDGLProgramHandle;
  protected

    FShaders:       array [TDGLShaderType] of TDGLSLShaderScript;
    FLibShaderName: array [TDGLShaderType] of string;

    FIsValid:       Boolean;
    FInfoLog:       string;
    FUniforms:      TDGLPersistentObjectList;
    FAutoFill:      Boolean;
    FMaterialName : TDGLLibMaterialName;
    Fmaterial : TDGLMaterial;
    FMaterialLibrary  : TDGLMaterialLibrary;


    // implementing IGLMaterialLibrarySupported
    function GetMaterialLib: TDGLMaterialLibrary;
    function GetMaterialLibrary: TDGLAbstractMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TDGLMaterialLibrary);
    procedure SetMaterialName(const value : TDGLLibMaterialName);

    function GetLibShaderName(AType: TDGLShaderType): string;
    procedure SetLibShaderName(AType: TDGLShaderType; const AValue: string);

    function GetUniform(const AName: string): IShaderParameter;
    class procedure ReleaseUniforms(AList: TDGLPersistentObjectList);

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadUniforms(AStream: TStream);
    procedure WriteUniforms(AStream: TStream);
    procedure Loaded; override;


  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Notification(Sender: TObject; Operation: TOperation); override;
    procedure NotifyChange(Sender: TObject); override;


    procedure DoApply(var ARci: TRenderContextInfo); override;
    function DoUnApply(var ARci: TRenderContextInfo):boolean; override;
    class function IsSupported: Boolean; override;

    procedure DoOnPrepare(Sender: TDGLContext);override;

    property VertexShaderName: TDGLShaderComponentName index shtVertex read GetLibShaderName write SetLibShaderName;
    property FragmentShaderName: TDGLShaderComponentName index shtFragment read GetLibShaderName write SetLibShaderName;
    property GeometryShaderName: TDGLShaderComponentName index shtGeometry read GetLibShaderName write SetLibShaderName;
    property TessEvalShaderName: TDGLShaderComponentName index shtEvaluation read GetLibShaderName write SetLibShaderName;
    property TessControlShaderName: TDGLShaderComponentName index shtControl read GetLibShaderName write SetLibShaderName;

    property Uniforms[const AName: string]: IShaderParameter read GetUniform;
    property Material : TDGLMaterial read FMaterial;

  published
    { Published Declarations }

    procedure GetUniformNames(Proc: TGetStrProc);

    property Handle: TDGLProgramHandle read FHandle;
    property IsValid: Boolean read FIsValid;


    // Compilation info log for design time
    property InfoLog: string read FInfoLog;
    // Turn on autofill of uniforms
    property MaterialLibrary: TDGLMaterialLibrary read GetMaterialLib write SetMaterialLibrary;
    property MaterialName: TDGLLibMaterialName read FMaterialName write SetMaterialName;

    property AutoUniforms: Boolean read FAutoFill write FAutoFill stored False;

  end;

  // ****************************************************************************************
  // TDGLBaseGLSLShaderModel
  //
  TDGLBaseGLSLShaderModel = class(TDGLCustomGLSLShaderModel)
  private

  protected

  public

  published
    { Published Declarations }

    property TessControlShaderName;
    property TessEvalShaderName;
    property VertexShaderName;
    property GeometryShaderName;
    property FragmentShaderName;
  end;

  // ****************************************************************************************
  // TGLLibShader
  //
  TOnUniformInitialize = procedure(Sender: TDGLAbstractShaderModel) of object;
  TOnUniformSetting    = procedure(Sender: TDGLAbstractShaderModel; var ARci: TRenderContextInfo) of object;

  TDGLLibShader = class(TDGLAbstractLibShader)
  private
    { Private Declarations }
    FHandle:             TDGLVirtualHandle;
    FSM:                 TDGLBaseGLSLShaderModel; //TDGLCustomGLSLShaderModel; @TODO Replace by a Collection
    FOnSMUniformInit:    TOnUniformInitialize;
    FOnSMUniformSetting: TOnUniformSetting;
    FApplicableLevel:     TDGLShaderMaterialLevel;
    FSelectedLevel:       TDGLShaderMaterialLevel;
    FNextPass:            TDGLLibShader;
    // FOnShaderUniformSetting: TOnUniformSetting;
    procedure SetLevel(AValue: TDGLShaderMaterialLevel);
    procedure SetSM(AValue: TDGLBaseGLSLShaderModel);
    procedure DoAllocate(Sender: TDGLVirtualHandle; var Handle: TGLUint);
    procedure DoDeallocate(Sender: TDGLVirtualHandle; var Handle: TGLUint);
  protected
    procedure Loaded; override;
//    procedure RemoveDefferedInit;
    procedure DoOnPrepare(Sender: TDGLContext);
  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;

    procedure Apply(var ARci: TRenderContextInfo); override;
    function UnApply(var ARci: TRenderContextInfo): Boolean; override;

  published
    { Published Declarations }
    property ShaderModel:    TDGLBaseGLSLShaderModel read FSM write SetSM;
    property ApplicableLevel: TDGLShaderMaterialLevel read FApplicableLevel write SetLevel default mlAuto;
    property SelectedLevel:   TDGLShaderMaterialLevel read FSelectedLevel;

    property OnUniformInitialize: TOnUniformInitialize read FOnSMUniformInit write FOnSMUniformInit;
    property OnUniformSetting:    TOnUniformSetting read FOnSMUniformSetting write FOnSMUniformSetting;
    // property OnShaderUniformSetting
    //property NextPass;
  end;

  // ****************************************************************************************
  // TGLShaderLibComponents
  //
  TDGLShaderLibComponents = class(TDGLXCollection)
  protected
    { Protected Declarations }
    function GetItems(Index: Integer): TDGLBaseShaderComponentItem;
  public
    { Public Declarations }
    function GetNamePath: string; override;
    class function ItemsClass: TDGLXCollectionItemClass; override;
    property Items[index: Integer]: TDGLBaseShaderComponentItem read GetItems; default;

    function GetItemByName(const AName: TDGLShaderComponentName): TDGLBaseShaderComponentItem;
    function MakeUniqueName(const AName: TDGLShaderComponentName): TDGLShaderComponentName;
  end;

  // ****************************************************************************************
  // TGLShaderLibrary
  //
  TDGLShaderLibrary = class(TDGLAbstractShaderLibrary)
  private
    { Private Declarations }
    FComponents: TDGLShaderLibComponents;
  protected
    { Protected Declarations }
    procedure Loaded; override;
    function GetShaders: TDGLLibShaders;
    procedure SetShaders(AValue: TDGLLibShaders);
    function StoreShaders: Boolean;
    procedure SetComponents(AValue: TDGLShaderLibComponents);

    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteComponents(AStream: TStream);
    procedure ReadComponents(AStream: TStream);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetNames(Proc: TGetStrProc; AClass: CDGLBaseShaderCollectionItem); overload;

    function AddShader(const AName: TDGLShaderComponentName): TDGLSLShaderScript;

  published
    { Published Declarations }
    { : The materials collection. }
    property Shaders:  TDGLLibShaders read GetShaders write SetShaders stored StoreShaders;
    property Components: TDGLShaderLibComponents read FComponents write SetComponents;
    property ShaderPaths;
  end;

  // ****************************************************************************************
  // TStandardUniformAutoSetExecutor
  //
  TStandardUniformAutoSetExecutor = class
  public
    constructor Create;
    procedure SetModelMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetViewMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetProjectionMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetInvModelMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetModelViewMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetNormalModelMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetInvModelViewMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetViewProjectionMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetWorldViewProjectionMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetCameraPosition(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    // Lighting
    procedure SetLightSource0Position(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    // Material
    procedure SetMaterialFrontAmbient(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetMaterialFrontDiffuse(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetMaterialFrontSpecular(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetMaterialFrontEmission(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetMaterialFrontShininess(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetMaterialBackAmbient(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetMaterialBackDiffuse(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetMaterialBackSpecular(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetMaterialBackShininess(Sender: IShaderParameter; var ARci: TRenderContextInfo);
    procedure SetMaterialBackEmission(Sender: IShaderParameter; var ARci: TRenderContextInfo);
  end;

// ****************************************************************************************

procedure RegisterGLShaderNameChangeEvent(AEvent: TNotifyEvent);
procedure DeRegisterGLShaderNameChangeEvent(AEvent: TNotifyEvent);

// ****************************************************************************************

Var
  vStandartUniformAutoSetExecutor: TStandardUniformAutoSetExecutor;

// ****************************************************************************************

Const
  DefaultShader_vp: AnsiString =
    '#version 330 Core' + #10#13 +
    'layout (location=0) vec3 Position;' + #10#13 +
    'layout (location=1) vec3 Normal;' + #10#13 +
    'layout (location=2) vec2 TexCoord0;' + #10#13 +
    'out float diffuse;' + #10#13 +
    'out vec2 texcoord;' + #10#13 +
    'uniform mat4 ModelMatrix;' + #10#13 +
    'uniform mat4 ViewProjectionMatrix;' + #10#13 +
    'uniform vec4 LightSourcePos;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    '	vec4 vertex    = ModelMatrix * vec4(Position, 1.0);' + #10#13 +
    '	vec4 direction = normalize(LightSourcePos - vertex);' + #10#13 +
    '	vec3 normal = normalize(mat3(ModelMatrix) * Normal);' + #10#13 +
    '	diffuse = clamp(dot(normal, direction.xyz), 0.0, 1.0);' + #10#13 +
    '	texcoord = TexCoord0;' + #10#13 +
    '	gl_Position = ViewProjectionMatrix * vertex;' + #10#13 +
    '}';

  DefaultShader_fp: AnsiString =
    '#version 330 Core' + #10#13 +
    'in float diffuse;' + #10#13 +
    'in vec2 texcoord;' + #10#13 +
    'out vec4 FragColor;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec2 tc = fract(texcoord);' + #10#13 +
    ' float df = sign(diffuse+0.01);' + #10#13 +
    '	FragColor = vec4(tc.s*df, tc.t*df, 0.0, 1.0);' + #10#13 +
    '}';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
  vGLShaderNameChangeEvent:    TNotifyEvent;


procedure RegisterGLShaderNameChangeEvent(AEvent: TNotifyEvent);
begin
  vGLShaderNameChangeEvent := AEvent;
end;

procedure DeRegisterGLShaderNameChangeEvent(AEvent: TNotifyEvent);
begin
  vGLShaderNameChangeEvent := nil;
end;

// ------------------
{ TDGLAbstractLibShader }
{$IFDEF GLS_REGION}{$REGION 'TDGLAbstractLibShader'}{$ENDIF}
constructor TDGLAbstractLibShader.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FUserList := TList.Create;
  if Assigned(ACollection) then
  begin
    FName := TDGLAbstractLibShaders(ACollection).MakeUniqueName('LibShader');
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

destructor TDGLAbstractLibShader.Destroy;
begin
  FUserList.Free;
  inherited Destroy;
end;

procedure TDGLAbstractLibShader.Assign(Source: TPersistent);
begin
  if Source is TDGLAbstractLibShader then
  begin
    FName :=
      TDGLLibShaders(Collection).MakeUniqueName(TDGLLibShader(Source).Name);
    FNameHashKey := ComputeNameHashKey(FName);
  end
  else
    inherited; // Raise AssignError
end;


function TDGLAbstractLibShader.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;


function TDGLAbstractLibShader._AddRef: Integer; stdcall;
begin
  Result := -1; //ignore
end;

function TDGLAbstractLibShader._Release: Integer; stdcall;
begin
  Result := -1; //ignore
end;

procedure TDGLAbstractLibShader.RegisterUser(obj: TDGLUpdateAbleObject);
begin
  Assert(FUserList.IndexOf(obj) < 0);
  FUserList.Add(obj);
end;

procedure TDGLAbstractLibShader.UnRegisterUser(obj: TDGLUpdateAbleObject);
begin
  FUserList.Remove(obj);
end;

procedure TDGLAbstractLibShader.RegisterUser(comp: TDGLUpdateAbleComponent);
begin
  Assert(FUserList.IndexOf(comp) < 0);
  FUserList.Add(comp);
end;

procedure TDGLAbstractLibShader.UnRegisterUser(comp: TDGLUpdateAbleComponent);
begin
  FUserList.Remove(comp);
end;

procedure TDGLAbstractLibShader.RegisterUser(libMaterial: TDGLLibMaterial);
begin
  Assert(FUserList.IndexOf(libMaterial) < 0);
  FUserList.Add(libMaterial);
end;

procedure TDGLAbstractLibShader.UnRegisterUser(libMaterial: TDGLLibMaterial);
begin
  FUserList.Remove(libMaterial);
end;

procedure TDGLAbstractLibShader.NotifyChange(Sender: TObject);
begin
  NotifyUsers();
end;

procedure TDGLAbstractLibShader.NotifyUsers;
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
        Assert(obj is TDGLAbstractLibShader);
        TDGLAbstractLibShader(FUserList[i]).NotifyUsers;
      end;
    end;
  finally
    FNotifying := False;
  end;
end;



function TDGLAbstractLibShader.GetDisplayName: string;
begin
  Result := Name;
end;

function TDGLAbstractLibShader.GetShaderLibrary: TDGLAbstractShaderLibrary;
var
  LOwner: TPersistent;
begin
  Result := nil;
  if Assigned(Collection) then
  begin
    LOwner := TDGLAbstractLibShaders(Collection).Owner;
    if LOwner is TDGLAbstractShaderLibrary then
      Result := TDGLAbstractShaderLibrary(LOwner);
  end;
end;

//function TDGLAbstractLibShader.GetMaterialLibrary: TDGLMaterialLibrary;
//begin
// if assigned(FMaterialLibrary) then
//   result := FMaterialLibrary
// else result:=nil;
//end;
//
//procedure TDGLAbstractLibShader.SetMaterialLibrary(const Value: TDGLMaterialLibrary);
//begin
//  if FMaterialLibrary = Value then exit;
//  FMaterialLibrary := Value;
//end;
//
//procedure TDGLAbstractLibShader.SetMaterialName(const Value: TGLLibMaterialName);
//var
//  LMat: TDGLMaterial;
//begin
//  if csLoading in GetMaterialLibrary.ComponentState then
//  begin
//    FMaterialName := Value;
//    exit;
//  end;
//
//  if Assigned(FMaterial) then
//  begin
//    if FMaterial.Name = Value then exit;
//    FMaterial.UnregisterUser(Self);
//    FMaterial := nil;
//  end;
//
//  LMat := GetMaterialLibrary.Materials.GetLibMaterialByName(Value);
//
//  if Assigned(LMat) then
//  begin
//    if LMat is TDGLMaterial then
//    begin
//      LMat.RegisterUser(Self);
//      FMaterial := LMat;
//    end;
//  end;
//  NotifyChange(Self);
//end;

class function TDGLAbstractLibShader.ComputeNameHashKey(const name: string): Integer;
var
  i, n: Integer;
begin
  n := Length(name);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(name[i]);
end;

procedure TDGLAbstractLibShader.SetName(const val: TDGLLibMaterialName);
begin
  if val <> FName then
  begin
    if not (csLoading in TComponent(Collection.Owner).ComponentState) then
    begin
      if TDGLLibShaders(Collection).GetLibShaderByName(val) <> Self then
        FName := TDGLLibShaders(Collection).MakeUniqueName(val)
      else
        FName := val;
    end
    else
      FName := val;
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

procedure TDGLAbstractLibShader.Loaded;
begin
end;

procedure TDGLAbstractLibShader.Apply(var ARci: TRenderContextInfo);
begin
end;

function TDGLAbstractLibShader.UnApply(var ARci: TRenderContextInfo): Boolean;
begin
  Result := True;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLAbstractLibShaders}
{$IFDEF GLS_REGION}{$REGION 'TDGLAbstractLibShaders'}{$ENDIF}
function TDGLAbstractLibShaders.GetShader(const AName: TDGLLibShaderName):TDGLAbstractLibShader;
var
  i, hk: Integer;
  lm: TDGLAbstractLibShader;
begin
  hk := TDGLAbstractLibShader.ComputeNameHashKey(AName);
  for i := 0 to Count - 1 do
  begin
    lm := TDGLAbstractLibShader(inherited Items[i]);
    if (lm.NameHashKey = hk) and (lm.Name = AName) then
    begin
      Result := lm;
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TDGLAbstractLibShaders.Loaded;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    TDGLAbstractLibShader(Items[I]).Loaded;
end;

function TDGLAbstractLibShaders.MakeUniqueName(const nameRoot: TDGLLibMaterialName): TDGLLibMaterialName;
var
  i: Integer;
begin
  Result := nameRoot;
  i := 1;
  while GetShader(Result) <> nil do
  begin
    Result := nameRoot + IntToStr(i);
    Inc(i);
  end;
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLAbstractShaderLibrary }
{$IFDEF GLS_REGION}{$REGION 'TDGLAbstractShaderLibrary'}{$ENDIF}

procedure TDGLAbstractShaderLibrary.SetShaderPaths(const val: string);
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
      FShaderPathList.Add(buf);
    end;
  end;

begin
  FShaderPathList.Free;
  FShaderPathList := nil;
  FShaderPaths := val;
  if val <> '' then
  begin
    FShaderPathList := TStringList.Create;
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

function TDGLAbstractShaderLibrary.ApplyShader(const AName: string;
  var ARci: TRenderContextInfo): Boolean;
begin
  FLastAppliedShader := FShaders.GetShader(AName);
  Result := Assigned(FLastAppliedShader);
  if Result then FLastAppliedShader.Apply(ARci);
end;

function TDGLAbstractShaderLibrary.UnApplyShader(
  var ARci: TRenderContextInfo): Boolean;
begin
  if Assigned(FLastAppliedShader) then
  begin
    Result := FLastAppliedShader.UnApply(ARci);
    if not Result then FLastAppliedShader := nil;
  end
  else
    Result := False;
end;

procedure TDGLAbstractShaderLibrary.SetNamesToTStrings(AStrings: TStrings);
var
  i: Integer;
  lm: TDGLAbstractLibShader;
begin
  with AStrings do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to FShaders.Count - 1 do
    begin
      lm := TDGLAbstractLibShader(FShaders.Items[i]);
      AddObject(lm.Name, lm);
    end;
    EndUpdate;
  end;
end;

procedure TDGLAbstractShaderLibrary.Loaded;
begin
  inherited;
  FShaders.Loaded;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLLibShaders }
{$IFDEF GLS_REGION}{$REGION 'TDGLLibShaders'}{$ENDIF}

function TDGLLibShaders.Add: TDGLLibShader;
begin
  Result := (inherited Add) as TDGLLibShader;
end;

constructor TDGLLibShaders.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TDGLLibShader);
end;

function TDGLLibShaders.FindItemID(ID: Integer): TDGLLibShader;
begin
  Result := (inherited FindItemID(ID)) as TDGLLibShader;
end;

function TDGLLibShaders.GetItems(AIndex: Integer): TDGLLibShader;
begin
  Result := TDGLLibShader(inherited Items[AIndex]);
end;

function TDGLLibShaders.GetLibShaderByName(const AName: string): TDGLLibShader;
var
  LShader: TDGLAbstractLibShader;
begin
  LShader := GetShader(AName);
  if Assigned(LShader) and (LShader is TDGLLibShader) then
    Result := TDGLLibShader(LShader)
  else
    Result := nil;
end;

function TDGLLibShaders.IndexOf(const Item: TDGLLibShader): Integer;
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

function TDGLLibShaders.ShaderLibrary: TDGLShaderLibrary;
begin
  Result := TDGLShaderLibrary(GetOwner);
end;

procedure TDGLLibShaders.SetItems(AIndex: Integer; const AValue: TDGLLibShader);
begin
  inherited Items[AIndex] := AValue;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBaseShaderComponentItem }
{$IFDEF GLS_REGION}{$REGION 'TDGLBaseShaderComponentItem'}{$ENDIF}

destructor TDGLBaseShaderComponentItem.Destroy;
var
  i: Integer;
begin
  if Assigned(FUserList) then
  begin
    FNotifying := True;
    for i      := FUserList.Count - 1 downto 0 do
      TDGLLibShaderProperty(FUserList[i]).Notification(Self, opRemove);
    FreeAndNil(FUserList);
  end;
  inherited;
end;

function TDGLBaseShaderComponentItem.GetShaderLibEx: TDGLShaderLibrary;
begin
  Result := TDGLShaderLibrary(TDGLMatLibComponents(Owner).Owner);
end;

function TDGLBaseShaderComponentItem.GetShaderLibrary: TDGLAbstractShaderLibrary;
begin
  Result := TDGLAbstractShaderLibrary(TDGLMatLibComponents(Owner).Owner);
end;

function TDGLBaseShaderComponentItem.GetUserCount: Integer;
begin
  if Assigned(FUserList) then
    Result := FUserList.Count
  else
    Result := 0;
end;

function TDGLBaseShaderComponentItem.GetUserList: TDGLPersistentObjectList;
begin
  if FUserList = nil then
  begin
    FUserList  := TDGLPersistentObjectList.Create;
    FNotifying := False;
  end;
  Result := FUserList;
end;

procedure TDGLBaseShaderComponentItem.NotifyChange(Sender: TObject);
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

procedure TDGLBaseShaderComponentItem.RegisterUser(AUser: TDGLUpdateAbleObject);
begin
  if not FNotifying and (UserList.IndexOf(AUser) < 0) then
    UserList.Add(AUser);
end;

procedure TDGLBaseShaderComponentItem.UnregisterUser(AUser: TDGLUpdateAbleObject);
begin
  if not FNotifying then
    UserList.Remove(AUser);
end;

procedure TDGLBaseShaderComponentItem.SetName(const AValue: string);
begin
  if AValue <> Name then
  begin
    if not IsValidIdent(AValue) then
    begin
      if IsDesignTime then
        InformationDlg(AValue + ' - is not valid component name');
      exit;
    end;
    if not(csLoading in ShaderLibrary.ComponentState) then
    begin
      if TDGLShaderLibComponents(Owner).GetItemByName(AValue) <> Self then
        inherited SetName(TDGLShaderLibComponents(Owner).MakeUniqueName(AValue))
      else
        inherited SetName(AValue);
    end
    else
      inherited SetName(AValue);
    FNameHashKey := ComputeNameHashKey(Name);
    // Notify users
    NotifyChange(Self);
    // Notify designer
    if Assigned(vGLShaderNameChangeEvent) then
      vGLShaderNameChangeEvent(Self);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLLibShaderProperty }
{$IFDEF GLS_REGION}{$REGION 'TDGLLibShaderProperty'}{$ENDIF}

function TDGLLibShaderProperty.GetShader: TDGLLibShader;
begin
  if Owner is TDGLLibShader then
    Result := TDGLLibShader(Owner)
  else if Owner is TDGLLibShaderProperty then
    Result := TDGLLibShaderProperty(Owner).GetShader
  else
    Result := nil;
end;

function TDGLLibShaderProperty.GetShaderLibraryEx: TDGLShaderLibrary;
begin
//  if Owner is TDGLBaseShaderComponentItem then
//    Result := TDGLBaseShaderComponentItem(Owner).GetShaderLibraryEx
//  else
    Result := TDGLShaderLibrary(GetShader.GetShaderLibrary);
end;

function TDGLLibShaderProperty.GetShaderLibrary: TDGLAbstractShaderLibrary;
begin
  if Owner is TDGLBaseShaderComponentItem then
    Result := TDGLBaseShaderComponentItem(Owner).GetShaderLibrary
  else
    Result := GetShader.GetShaderLibrary;
end;

procedure TDGLLibShaderProperty.SetNextPass(const AValue: TDGLLibShaderName);
begin
  if AValue <> FNextPassName then
  begin
    FNextPassName := AValue;
//    NotifyChange(Self);
  end;
end;

procedure TDGLLibShaderProperty.Loaded;
begin
end;

procedure TDGLLibShaderProperty.NotifyChange(Sender: TObject);
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

procedure TDGLLibShaderProperty.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if Owner is TDGLLibShader then
      GetShader.NotifyChange(Self);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSLShaderScript }
{$IFDEF GLS_REGION}{$REGION 'TDGLSLShaderScript'}{$ENDIF}

procedure TDGLSLShaderScript.Assign(Source: TPersistent);
var
  LShader: TDGLSLShaderScript;
begin
  if Source is TDGLSLShaderScript then
  begin
    LShader := TDGLSLShaderScript(Source);
    FSource.Assign(LShader.Source);
    FShaderType := LShader.FShaderType;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TDGLSLShaderScript.Create(AOwner: TDGLXCollection);
const
  cShaderClasses: array [TDGLShaderType] of TDGLShaderHandleClass = (TDGLVertexShaderHandle, TDGLTessControlShaderHandle,
                                                                     TDGLTessEvaluationShaderHandle, TDGLGeometryShaderHandle,
                                                                     TDGLFragmentShaderHandle); //TDGLComputeShaderHandle
var
  S: TDGLShaderType;
begin
  inherited;

  for S         := Low(TDGLShaderType) to High(TDGLShaderType) do
  begin
    FHandle[S]           := cShaderClasses[S].Create;
    FHandle[S].OnPrepare := DoOnPrepare;
  end;
  FSource              := TStringList.Create;
  FSource.OnChange     := NotifyChange;
  FShaderType          := shtVertex;
  FGeometryInput       := gsInPoints;
  FGeometryOutput      := gsOutPoints;
  FGeometryVerticesOut := 1;
  Name                 := TDGLShaderLibComponents(AOwner).MakeUniqueName('Shader');
end;

destructor TDGLSLShaderScript.Destroy;
var
  S: TDGLShaderType;
begin
  for S := Low(TDGLShaderType) to High(TDGLShaderType) do
    FHandle[S].Destroy;
  FSource.Destroy;
  inherited;
end;

procedure TDGLSLShaderScript.NotifyChange(Sender: TObject);
var
  S: TDGLShaderType;
begin
  for S := Low(TDGLShaderType) to High(TDGLShaderType) do
    FHandle[S].NotifyChangesOfData;

  if (Sender = FSource) and IsDesignTime and (Length(FSourceFile) > 0) then
    FSource.SaveToFile(FSourceFile);

  inherited;
end;

procedure TDGLSLShaderScript.DoOnPrepare(Sender: TDGLContext);
begin
//  if not IsDesignTime and FDefferedInit then exit;
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
          DGLSLogger.LogInfoFmt('Shader "%s" compilation successful - %s', [Name, FHandle[FShaderType].InfoLog])
        else
          DGLSLogger.LogErrorFmt('Shader "%s" compilation failed - %s', [Name, FHandle[FShaderType].InfoLog]);
        FHandle[FShaderType].NotifyDataUpdated;
      end;
    end
    else
    begin
      FIsValid := False;
      if IsDesignTime then FInfoLog := 'Not supported by hardware';
    end;
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
end;

class function TDGLSLShaderScript.FriendlyName: string;
begin
  Result := 'Shader Script';
end;

function TDGLSLShaderScript.GetHandle: TDGLShaderHandle;
begin
  Result := FHandle[FShaderType];
end;

procedure TDGLSLShaderScript.ReadFromFiler(AReader: TReader);
var
  archiveVersion: Integer;
begin
  with AReader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion = 0 then
    begin
      Name                 := ReadString;
//      FDefferedInit        := ReadBoolean;
      FSource.Text         := ReadString;
      FSourceFile          := ReadString;
      FShaderType          := TDGLShaderType(ReadInteger);
      FGeometryInput       := TDGLgsInTypes(ReadInteger);
      FGeometryOutput      := TDGLgsOutTypes(ReadInteger);
      FGeometryVerticesOut := ReadInteger;
    end
    else
      RaiseFilerException(archiveVersion);
  end;
end;

procedure TDGLSLShaderScript.SetGeometryInput(AValue: TDGLgsInTypes);
begin
  if AValue <> FGeometryInput then
  begin
    FGeometryInput := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLSLShaderScript.SetGeometryOutput(AValue: TDGLgsOutTypes);
begin
  if AValue <> FGeometryOutput then
  begin
    FGeometryOutput := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLSLShaderScript.SetGeometryVerticesOut(AValue: TGLint);
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

procedure TDGLSLShaderScript.SetShaderType(AValue: TDGLShaderType);
begin
  if FShaderType <> AValue then
  begin
    FShaderType := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLSLShaderScript.SetSource(AValue: TStringList);
begin
  FSource.Assign(AValue);
end;

procedure TDGLSLShaderScript.SetSourceFile(AValue: string);
begin
  FixPathDelimiter(AValue);
  if FSourceFile <> AValue then
  begin
    FSourceFile := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLSLShaderScript.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteInteger(0); // archive version
    WriteString(Name);
//    WriteBoolean(FDefferedInit);
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

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLAbstractShaderUniform }
{$IFDEF GLS_REGION}{$REGION 'TDGLAbstractShaderUniform'}{$ENDIF}

function TDGLAbstractShaderUniform.GetFloat: Single;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetGLSLSamplerType: TDGLSLSamplerType;
begin
  Result := FSamplerType;
end;

function TDGLAbstractShaderUniform.GetGLSLType: TDGLSLDataType;
begin
  Result := FType;
end;

function TDGLAbstractShaderUniform.GetInt: TGLint;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetIVec2: TVector2i;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetIVec3: TVector3i;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetIVec4: TVector4i;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetMat2: TMatrix2f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetMat3: TMatrix3f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetMat4: TMatrix4f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetName: string;
begin
  Result := FName;
end;

function TDGLAbstractShaderUniform.GetSamplerName: string;
begin
  Result := rstrNothing;
end;

procedure TDGLAbstractShaderUniform.Apply(var ARci: TRenderContextInfo);
begin
end;

function TDGLAbstractShaderUniform.GetAutoSetMethod: string;
begin
  Result := rstrNothing;
end;

function TDGLAbstractShaderUniform.GetTextureName: string;
begin
  Result := rstrNothing;
end;

function TDGLAbstractShaderUniform.GetTextureSwizzle: TSwizzleVector;
begin
  Result := cDefaultSwizzleVector;
end;

function TDGLAbstractShaderUniform.GetUInt: TGLUint;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetUVec2: TVector2ui;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetUVec3: TVector3ui;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetUVec4: TVector4ui;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetVec2: TVector2f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetVec3: TVector3f;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

function TDGLAbstractShaderUniform.GetVec4: TVector;
begin
  FillChar(Result, SizeOf(Result), $00);
end;

procedure TDGLAbstractShaderUniform.ReadFromFiler(AReader: TReader);
begin
end;

procedure TDGLAbstractShaderUniform.SetFloat(const Value: TGLFloat);
begin
end;

procedure TDGLAbstractShaderUniform.SetFloatArray(const Values: PGLFloat; Count: Integer);
begin
end;

procedure TDGLAbstractShaderUniform.SetInt(const Value: Integer);
begin
end;

procedure TDGLAbstractShaderUniform.SetIntArray(const Values: PGLInt; Count: Integer);
begin
end;

procedure TDGLAbstractShaderUniform.SetIVec2(const Value: TVector2i);
begin
end;

procedure TDGLAbstractShaderUniform.SetIVec3(const Value: TVector3i);
begin
end;

procedure TDGLAbstractShaderUniform.SetIVec4(const Value: TVector4i);
begin
end;

procedure TDGLAbstractShaderUniform.SetMat2(const Value: TMatrix2f);
begin
end;

procedure TDGLAbstractShaderUniform.SetMat3(const Value: TMatrix3f);
begin
end;

procedure TDGLAbstractShaderUniform.SetMat4(const Value: TMatrix4f);
begin
end;

procedure TDGLAbstractShaderUniform.SetSamplerName(const AValue: string);
begin
end;

procedure TDGLAbstractShaderUniform.SetAutoSetMethod(const AValue: string);
begin
end;

procedure TDGLAbstractShaderUniform.SetTextureName(const AValue: string);
begin
end;

procedure TDGLAbstractShaderUniform.SetTextureSwizzle(const AValue: TSwizzleVector);
begin
end;

procedure TDGLAbstractShaderUniform.SetUInt(const Value: GLuint);
begin
end;

procedure TDGLAbstractShaderUniform.SetUIntArray(const Values: PGLUInt; Count: Integer);
begin
end;

procedure TDGLAbstractShaderUniform.SetUVec2(const Value: TVector2ui);
begin
end;

procedure TDGLAbstractShaderUniform.SetUVec3(const Value: TVector3ui);
begin
end;

procedure TDGLAbstractShaderUniform.SetUVec4(const Value: TVector4ui);
begin
end;

procedure TDGLAbstractShaderUniform.SetVec2(const Value: TVector2f);
begin
end;

procedure TDGLAbstractShaderUniform.SetVec3(const Value: TVector3f);
begin
end;

procedure TDGLAbstractShaderUniform.SetVec4(const Value: TVector4f);
begin
end;

procedure TDGLAbstractShaderUniform.WriteToFiler(AWriter: TWriter);
begin
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLShaderUniform }
{$IFDEF GLS_REGION}{$REGION 'TDGLShaderUniform'}{$ENDIF}

function TDGLShaderUniform.GetFloat: Single;
begin
  // TODO: Type checking
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetInt: TGLint;
begin
  glGetUniformiv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetIVec2: TVector2i;
begin
  glGetUniformiv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetIVec3: TVector3i;
begin
  glGetUniformiv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetIVec4: TVector4i;
begin
  glGetUniformiv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetMat2: TMatrix2f;
begin
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetMat3: TMatrix3f;
begin
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetMat4: TMatrix4f;
begin
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetProgram: TGLUint;
begin
  Result := TDGLCustomGLSLShaderModel(Owner).FHandle.Handle;
end;

procedure TDGLShaderUniform.Apply(var ARci: TRenderContextInfo);
begin
  if Assigned(FAutoSet) then
    FAutoSet(Self, ARci);
end;

procedure TDGLShaderUniform.Assign(Source: TPersistent);
var
  LUniform: TDGLShaderUniform;
begin
  if Source is TDGLShaderUniform then
  begin
    LUniform      := TDGLShaderUniform(Source);
    FName         := LUniform.Name;
    FNameHashCode := LUniform.FNameHashCode;
    FType         := LUniform.FType;
    FSamplerType  := LUniform.FSamplerType;
    FAutoSet      := LUniform.FAutoSet;
  end;
  inherited;
end;

function TDGLShaderUniform.GetAutoSetMethod: string;
begin
  Result := GetUniformAutoSetMethodName(FAutoSet);
end;

function TDGLShaderUniform.GetUInt: TGLUint;
begin
  glGetUniformuiv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetUVec2: TVector2ui;
begin
  glGetUniformuiv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetUVec3: TVector3ui;
begin
  glGetUniformuiv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetUVec4: TVector4ui;
begin
  glGetUniformuiv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetVec2: TVector2f;
begin
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetVec3: TVector3f;
begin
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

function TDGLShaderUniform.GetVec4: TVector;
begin
  glGetUniformfv(GetProgram, FLocation, @Result);
end;

procedure TDGLShaderUniform.PopProgram;
begin
  CurrentDGLContext.GLStates.CurrentProgram := FStoreProgram;
end;

procedure TDGLShaderUniform.PushProgram;
begin
  with CurrentDGLContext.GLStates do
  begin
    FStoreProgram  := CurrentProgram;
    CurrentProgram := GetProgram;
  end;
end;

procedure TDGLShaderUniform.ReadFromFiler(AReader: TReader);
begin
  with AReader do
  begin
    FName         := ReadString;
    FNameHashCode := ComputeNameHashKey(FName);
    FType         := TDGLSLDataType(ReadInteger);
    FSamplerType  := TDGLSLSamplerType(ReadInteger);
    SetAutoSetMethod(ReadString);
  end;
end;

procedure TDGLShaderUniform.SetFloat(const Value: TGLFloat);
begin
  PushProgram;
  glUniform1f(FLocation, Value);
  PopProgram;
end;

procedure TDGLShaderUniform.SetFloatArray(const Values: PGLFloat; Count: Integer);
begin
  PushProgram;
  glUniform1fv(FLocation, Count, Values);
  PopProgram;
end;

procedure TDGLShaderUniform.SetInt(const Value: Integer);
begin
  PushProgram;
  glUniform1i(FLocation, Value);
  PopProgram;
end;

procedure TDGLShaderUniform.SetIntArray(const Values: PGLInt; Count: Integer);
begin
  PushProgram;
  glUniform1iv(FLocation, Count, Values);
  PopProgram;
end;

procedure TDGLShaderUniform.SetIVec2(const Value: TVector2i);
begin
  PushProgram;
  glUniform2i(FLocation, Value.V[0], Value.V[1]);
  PopProgram;
end;

procedure TDGLShaderUniform.SetIVec3(const Value: TVector3i);
begin
  PushProgram;
  glUniform3i(FLocation, Value.V[0], Value.V[1], Value.V[2]);
  PopProgram;
end;

procedure TDGLShaderUniform.SetIVec4(const Value: TVector4i);
begin
  PushProgram;
  glUniform4i(FLocation, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  PopProgram;
end;

procedure TDGLShaderUniform.SetMat2(const Value: TMatrix2f);
begin
  PushProgram;
  glUniformMatrix2fv(FLocation, 1, False, @Value);
  PopProgram;
end;

procedure TDGLShaderUniform.SetMat3(const Value: TMatrix3f);
begin
  PushProgram;
  glUniformMatrix2fv(FLocation, 1, False, @Value);
  PopProgram;
end;

procedure TDGLShaderUniform.SetMat4(const Value: TMatrix4f);
begin
  PushProgram;
  glUniformMatrix4fv(FLocation, 1, False, @Value);
  PopProgram;
end;

procedure TDGLShaderUniform.SetAutoSetMethod(const AValue: string);
begin
  FAutoSet := GetUniformAutoSetMethod(AValue);
end;

procedure TDGLShaderUniform.SetUInt(const Value: GLuint);
begin
  PushProgram;
  glUniform1ui(FLocation, Value);
  PopProgram;
end;

procedure TDGLShaderUniform.SetUIntArray(const Values: PGLUInt; Count: Integer);
begin
  PushProgram;
  glUniform1uiv(FLocation, Count, Values);
  PopProgram;
end;

procedure TDGLShaderUniform.SetUVec2(const Value: TVector2ui);
begin
  PushProgram;
  glUniform2ui(FLocation, Value.V[0], Value.V[1]);
  PopProgram;
end;

procedure TDGLShaderUniform.SetUVec3(const Value: TVector3ui);
begin
  PushProgram;
  glUniform3ui(FLocation, Value.V[0], Value.V[1], Value.V[2]);
  PopProgram;
end;

procedure TDGLShaderUniform.SetUVec4(const Value: TVector4ui);
begin
  PushProgram;
  glUniform4ui(FLocation, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  PopProgram;
end;

procedure TDGLShaderUniform.SetVec2(const Value: TVector2f);
begin
  PushProgram;
  glUniform2f(FLocation, Value.V[0], Value.V[1]);
  PopProgram;
end;

procedure TDGLShaderUniform.SetVec3(const Value: TVector3f);
begin
  PushProgram;
  glUniform3f(FLocation, Value.V[0], Value.V[1], Value.V[2]);
  PopProgram;
end;

procedure TDGLShaderUniform.SetVec4(const Value: TVector4f);
begin
  PushProgram;
  glUniform4f(FLocation, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
  PopProgram;
end;

procedure TDGLShaderUniform.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    WriteString(FName);
    WriteInteger(Integer(FType));
    WriteInteger(Integer(FSamplerType));
    WriteString(GetAutoSetMethod);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

 // ------------------
{ TDGLShaderUniformDSA }
{$IFDEF GLS_REGION}{$REGION 'TDGLShaderUniformDSA'}{$ENDIF}

procedure TDGLShaderUniformDSA.SetFloat(const Value: TGLFloat);
begin
  glProgramUniform1f(GetProgram, FLocation, Value);
end;

procedure TDGLShaderUniformDSA.SetFloatArray(const Values: PGLFloat; Count: Integer);
begin
  glProgramUniform1fv(GetProgram, FLocation, Count, Values);
end;

procedure TDGLShaderUniformDSA.SetInt(const Value: Integer);
begin
  glProgramUniform1i(GetProgram, FLocation, Value);
end;

procedure TDGLShaderUniformDSA.SetIntArray(const Values: PGLInt; Count: Integer);
begin
  glProgramUniform1iv(GetProgram, FLocation, Count, Values);
end;

procedure TDGLShaderUniformDSA.SetIVec2(const Value: TVector2i);
begin
  glProgramUniform2i(GetProgram, FLocation, Value.V[0], Value.V[1]);
end;

procedure TDGLShaderUniformDSA.SetIVec3(const Value: TVector3i);
begin
  glProgramUniform3i(GetProgram, FLocation, Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TDGLShaderUniformDSA.SetIVec4(const Value: TVector4i);
begin
  glProgramUniform4i(GetProgram, FLocation, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

procedure TDGLShaderUniformDSA.SetMat2(const Value: TMatrix2f);
begin
  glProgramUniformMatrix2fv(GetProgram, FLocation, 1, False, @Value);
end;

procedure TDGLShaderUniformDSA.SetMat3(const Value: TMatrix3f);
begin
  glProgramUniformMatrix3fv(GetProgram, FLocation, 1, False, @Value);
end;

procedure TDGLShaderUniformDSA.SetMat4(const Value: TMatrix4f);
begin
  glProgramUniformMatrix4fv(GetProgram, FLocation, 1, False, @Value);
end;

procedure TDGLShaderUniformDSA.SetUInt(const Value: GLuint);
begin
  glProgramUniform1ui(GetProgram, FLocation, Value);
end;

procedure TDGLShaderUniformDSA.SetUIntArray(const Values: PGLUInt; Count: Integer);
begin
  glProgramUniform1uiv(GetProgram, FLocation, Count, Values);
end;

procedure TDGLShaderUniformDSA.SetUVec2(const Value: TVector2ui);
begin
  glProgramUniform2ui(GetProgram, FLocation, Value.V[0], Value.V[1]);
end;

procedure TDGLShaderUniformDSA.SetUVec3(const Value: TVector3ui);
begin
  glProgramUniform3ui(GetProgram, FLocation, Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TDGLShaderUniformDSA.SetUVec4(const Value: TVector4ui);
begin
  glProgramUniform4ui(GetProgram, FLocation, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

procedure TDGLShaderUniformDSA.SetVec2(const Value: TVector2f);
begin
  glProgramUniform2f(GetProgram, FLocation, Value.V[0], Value.V[1]);
end;

procedure TDGLShaderUniformDSA.SetVec3(const Value: TVector3f);
begin
  glProgramUniform3f(GetProgram, FLocation, Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TDGLShaderUniformDSA.SetVec4(const Value: TVector4f);
begin
  glProgramUniform4f(GetProgram, FLocation, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLShaderUniformTexture }
{$IFDEF GLS_REGION}{$REGION 'TDGLShaderUniformTexture'}{$ENDIF}

procedure TDGLShaderUniformTexture.Apply(var ARci: TRenderContextInfo);

  function FindHotActiveUnit: Boolean;
  var
    ID:                TGLUint;
    i, J:              Integer;
    bindTime, minTime: Double;
    LTex:              TDGLTexture;
  begin
    with ARci.GLStates do
    begin
      if Assigned(FMaterialLibrary) and Assigned(FLibTexture) and FLibTexture.IsValid then
      begin
        ID := FLibTexture.Handle.Handle;
        // Yar: may be need exract this to new method of TGLTextureImageEx ???
        if FLibTexture is TDGLTexture then
        begin
          LTex := TDGLTexture(FLibTexture);
          LTex.ApplyCounter:=TDGLTexture(FLibTexture).ApplyCounter+1;
          if LTex.ApplyCounter > 16 then
          begin
           LTex.Image.Free;
           LTex.Image := nil;
          end;
        end;
      end
      else
        ID := 0;

      // Find alredy binded texture unit
      for i := 0 to MaxTextureImageUnits - 1 do
      begin
        if TextureBinding[i, FTarget] = ID then
        begin
          glUniform1i(FLocation, i);
          ActiveTexture := i;
          Result        := True;
          exit;
        end;
      end;
      // Find unused texture unit
      for i := 0 to MaxTextureImageUnits - 1 do
      begin
        if TextureBinding[i, FTarget] = 0 then
        begin
          TextureBinding[i, FTarget] := ID;
          glUniform1i(FLocation, i);
          ActiveTexture := i;
          Result        := True;
          exit;
        end;
      end;
      // Find most useless texture unit
      minTime := GLSTime;
      J       := 0;
      for i   := 0 to MaxTextureImageUnits - 1 do
      begin
        bindTime := TextureBindingTime[i, FTarget];
        if bindTime < minTime then
        begin
          minTime := bindTime;
          J       := i;
        end;
      end;

      TextureBinding[J, FTarget] := ID;
      ActiveTexture              := J;
      glUniform1i(FLocation, J);
      Result := True;
      exit;
    end;
    Result := False;
  end;

var
  glTarget: TGLEnum;
begin
  if FLocation > -1 then
  begin
    if FindHotActiveUnit and Assigned(FLibTexture) and Assigned(FLibSampler) then
   //   with GL do
      begin
        // Apply swizzling if possible
        glTarget := DecodeGLTextureTarget(FLibTexture.Shape);
        if dglCheckextension('ARB_texture_swizzle') or dglCheckExtension('EXT_texture_swizzle') then
        begin

          if FSwizzling.R <> FLibTexture.Swizzles[0] then
          begin
            FLibTexture.Swizzles[0] := FSwizzling.r;
            glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_R, cTextureSwizzle[FSwizzling.r]);
          end;
          if FSwizzling.G <> FLibTexture.Swizzles[1] then
          begin
            FLibTexture.Swizzles[1] := FSwizzling.G;
            glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_G, cTextureSwizzle[FSwizzling.b]);
          end;
          if FSwizzling.B <> FLibTexture.Swizzles[2] then
          begin
            FLibTexture.Swizzles[2] := Swizzling.B;
            glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_B, cTextureSwizzle[FSwizzling.b]);
          end;
          if FSwizzling.A <> FLibTexture.Swizzles[3] then
          begin
            FLibTexture.Swizzles[3] := FSwizzling.A;
            glTexParameteri(glTarget, GL_TEXTURE_SWIZZLE_A, cTextureSwizzle[FSwizzling.a]);
          end;
        end;

        if FLibSampler.IsValid then
          FLibSampler.Apply(ARci)
        else if FLibTexture.LastSampler <> FLibSampler then
        begin
          // Sampler object not supported, lets use texture states
          glTexParameterfv(glTarget, GL_TEXTURE_BORDER_COLOR, FLibSampler.BorderColor.AsAddress);
          glTexParameteri(glTarget, GL_TEXTURE_WRAP_S, cTextureWrapMode[FLibSampler.WrapX]);
          glTexParameteri(glTarget, GL_TEXTURE_WRAP_T, cTextureWrapMode[FLibSampler.WrapY]);
          glTexParameteri(glTarget, GL_TEXTURE_WRAP_R, cTextureWrapMode[FLibSampler.WrapZ]);
          glTexParameterf(glTarget, GL_TEXTURE_LOD_BIAS, FLibSampler.LodBias + FLibSampler.LODBiasFract);
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

          FLibTexture.LastSampler := FLibSampler;
        end;

      end; // with GL
  end;
end;

procedure TDGLShaderUniformTexture.Assign(Source: TPersistent);
var
  LUniform: TDGLShaderUniformTexture;
begin
  if Source is TDGLShaderUniformTexture then
  begin
    LUniform       := TDGLShaderUniformTexture(Source);
    LibTextureName := LUniform.LibTextureName;
    LibSamplerName := LUniform.LibSamplerName;
  end;
  inherited;
end;

constructor TDGLShaderUniformTexture.Create(AOwner: TPersistent);
begin
  inherited;
  FSwizzling := cDefaultSwizzleVector;
end;

destructor TDGLShaderUniformTexture.Destroy;
begin
  LibTextureName := '';
  LibSamplerName := '';
  inherited;
end;

function TDGLShaderUniformTexture.GetMaterialLibrary: TDGLMaterialLibrary;
begin
 if assigned(FMaterialLibrary) then
   result := FMaterialLibrary
 else result:=nil;
end;

procedure TDGLShaderUniformTexture.SetMaterialLibrary(const Value: TDGLMaterialLibrary);
begin
  if FMaterialLibrary = Value then exit;
  FMaterialLibrary := Value;
end;

function TDGLShaderUniformTexture.GetSamplerName: string;
begin
  if Assigned(FLibSampler) then
    Result := FLibSampler.Name
  else
    Result := rstrNothing;
end;

function TDGLShaderUniformTexture.GetTextureName: string;
begin
  if Assigned(FLibTexture) then
    Result := FLibTexture.Name
  else
    Result := rstrNothing;
end;

function TDGLShaderUniformTexture.GetTextureSwizzle: TSwizzleVector;
begin
  Result := FSwizzling;
end;

procedure TDGLShaderUniformTexture.Loaded;
begin
  SetTextureName(FLibTexureName);
  SetSamplerName(FLibSamplerName);
end;

procedure TDGLShaderUniformTexture.Notification(Sender: TObject; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if Sender = FLibTexture then
      FLibTexture := nil
    else if Sender = FLibSampler then
      FLibSampler := nil;
  end;
end;

procedure TDGLShaderUniformTexture.ReadFromFiler(AReader: TReader);
begin
  with AReader do
  begin
    inherited;
    LibTextureName := ReadString;
    LibSamplerName := ReadString;
    FSwizzling.r  := TDGLTextureSwizzle(ReadInteger);
    FSwizzling.g  := TDGLTextureSwizzle(ReadInteger);
    FSwizzling.b  := TDGLTextureSwizzle(ReadInteger);
    FSwizzling.a  := TDGLTextureSwizzle(ReadInteger);
  end;
end;

procedure TDGLShaderUniformTexture.SetTextureName(const AValue: string);
var
  LTexture: TDGLAbstractTexture;
begin
  if csLoading in TDGLAbstractShaderModel(Owner).GetShaderLibrary.ComponentState then
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

  LTexture := TDGLCustomGLSLShaderModel(Owner).MaterialLibrary.Components.GetTextureByName(AValue);

  if Assigned(LTexture) then
  begin
    if LTexture is TDGLFrameBufferAttachment then
    begin
      if TDGLFrameBufferAttachment(LTexture).OnlyWrite then
      begin
        if IsDesignTime then
          InformationDlg('Can not use write only attachment as texture')
        else
          DGLSLogger.LogErrorFmt('Attempt to write only attachment "%s" for uniform "%s"', [LTexture.Name, Name]);
        NotifyChange(Self);
        exit;
      end;
    end;
    LTexture.RegisterUser(Self);
    FLibTexture := LTexture;
  end;
  NotifyChange(Self);
end;

procedure TDGLShaderUniformTexture.SetSamplerName(const AValue: string);
var
  LSampler: TDGLTextureSampler;
begin
  if csLoading in TDGLCustomGLSLShaderModel(Owner).GetMaterialLibrary.ComponentState then
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

  LSampler := TDGLCustomGLSLShaderModel(Owner).MaterialLibrary.Components.GetSamplerByName(AValue);

  if Assigned(LSampler) then
  begin
    LSampler.RegisterUser(Self);
    FLibSampler := LSampler;
  end;

  NotifyChange(Self);
end;

procedure TDGLShaderUniformTexture.SetTextureSwizzle(const AValue: TSwizzleVector);
begin
  FSwizzling := AValue;
end;

procedure TDGLShaderUniformTexture.WriteToFiler(AWriter: TWriter);
begin
  with AWriter do
  begin
    inherited;
    WriteString(LibTextureName);
    WriteString(LibSamplerName);
    WriteInteger(Integer(FSwizzling.r));
    WriteInteger(Integer(FSwizzling.g));
    WriteInteger(Integer(FSwizzling.b));
    WriteInteger(Integer(FSwizzling.a));
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLAbstractShaderModel }
{$IFDEF GLS_REGION}{$REGION 'TDGLAbstractShaderModel'}{$ENDIF}

constructor TDGLAbstractShaderModel.Create(AOwner: TPersistent);
begin
  inherited;
  FVirtualHandle := TDGLVirtualHandle.Create;
  FVirtualHandle.OnAllocate := OnVirtualHandleAllocate;
  FVirtualHandle.OnDestroy := OnVirtualHandleDestroy;
//  FShaderStyle := ssLowLevel;
  FEnabled := True;
  FFailedInitAction := fiaRaiseStandardException;
end;

destructor TDGLAbstractShaderModel.Destroy;
begin
  FVirtualHandle.DestroyHandle;
  FinalizeShader;
  inherited;
  FVirtualHandle.Free;
end;

procedure TDGLAbstractShaderModel.OnVirtualHandleDestroy(sender: TDGLVirtualHandle; var handle: Cardinal);
begin
  handle := 0;
end;

procedure TDGLAbstractShaderModel.OnVirtualHandleAllocate(sender: TDGLVirtualHandle; var handle: Cardinal);
begin
  handle := 1;
end;

procedure TDGLAbstractShaderModel.Apply(var rci : TRenderContextInfo);
begin
 // Need to check it twice, because shader may refuse to initialize
  // and choose to disable itself during initialization.
  if Enabled then
    if FVirtualHandle.IsDataNeedUpdate then
      InitializeShader(rci);

  if Enabled then DoApply(rci);

  FShaderActive := True;
end;

function TDGLAbstractShaderModel.UnApply(var rci: TRenderContextInfo): Boolean;
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
    Result := False;
  end;
end;

procedure TDGLAbstractShaderModel.HandleFailedInitialization(const LastErrorMessage: string = '');
begin
  case FailedInitAction of
    fiaSilentdisable: ; // Do nothing ;)
    fiaRaiseHandledException:
      try
        raise EDGLShaderException.Create(GetStandardNotSupportedMessage);
      except
      end;
    fiaRaiseStandardException:
      raise EDGLShaderException.Create(GetStandardNotSupportedMessage);
    fiaReRaiseException:
      begin
        if LastErrorMessage <> '' then
          raise EDGLShaderException.Create(LastErrorMessage)
        else
          raise EDGLShaderException.Create(GetStandardNotSupportedMessage)
      end;
    //    fiaGenerateEvent:; // Do nothing. Event creation is left up to user shaders
    //                       // which may choose to override this procedure.
  else
    Assert(False, glsErrorEx + glsUnknownType);
  end;
end;

function TDGLAbstractShaderModel.GetStandardNotSupportedMessage: string;
begin
    Result := 'Your hardware/driver doesn''t support shader "' + ClassName + '"!';
end;

procedure TDGLAbstractShaderModel.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDGLAbstractShaderModel.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyChange(Self);
end;

procedure TDGLAbstractShaderModel.DoOnPrepare(Sender: TDGLContext);
begin
  // nothing here
end;

procedure TDGLAbstractShaderModel.DoInitialize(var rci: TRenderContextInfo);
begin
  // nothing here
end;

procedure TDGLAbstractShaderModel.DoFinalize;
begin
  // nothing here
end;

procedure TDGLAbstractShaderModel.DoApply(var rci: TRenderContextInfo);
begin
  // nothing here
end;

function TDGLAbstractShaderModel.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  // nothing here
  result:=true;
end;

procedure TDGLAbstractShaderModel.Assign(Source: TPersistent);
begin
  if Source is TDGLAbstractShaderModel then
  begin
//    FShaderStyle := TGLShader(Source).FShaderStyle;
    FFailedInitAction := TDGLAbstractShaderModel(Source).FFailedInitAction;
    Enabled := TDGLAbstractShaderModel(Source).FEnabled;
  end
  else
    inherited Assign(Source); //to the pit of doom ;)
end;

function TDGLAbstractShaderModel.GetShaderInitialized: Boolean;
begin
  Result := (FVirtualHandle.Handle <> 0);
end;

procedure TDGLAbstractShaderModel.InitializeShader(var rci: TRenderContextInfo);
begin
  FVirtualHandle.AllocateHandle;
  if FVirtualHandle.IsDataNeedUpdate then
  begin
    DoInitialize(rci);
    FVirtualHandle.NotifyDataUpdated;
  end;
end;

procedure TDGLAbstractShaderModel.FinalizeShader;
begin
  FVirtualHandle.NotifyChangesOfData;
  DoFinalize;
end;

procedure TDGLAbstractShaderModel.NotifyChange(Sender: TObject);
begin
  if FUpdateCount = 0 then
  begin
    FinalizeShader;
  End;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLCustomGLSLShaderModel }
{$IFDEF GLS_REGION}{$REGION 'TDGLCustomGLSLShaderModel'}{$ENDIF}

constructor TDGLCustomGLSLShaderModel.Create(AOwner: TPersistent);
begin
  inherited;
  FHandle           := TDGLProgramHandle.Create;
  FHandle.OnPrepare := DoOnPrepare;
  FEnabled          := False;
  FUniforms         := TDGLPersistentObjectList.Create;
  FAutoFill         := True;
end;

destructor TDGLCustomGLSLShaderModel.Destroy;
begin
  FHandle.Destroy;
  VertexShaderName      := '';
  FragmentShaderName    := '';
  GeometryShaderName    := '';
  TessControlShaderName := '';
  TessEvalShaderName    := '';
  FUniforms.CleanFree;
  inherited;
end;

procedure TDGLCustomGLSLShaderModel.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Uniforms', ReadUniforms, WriteUniforms, FUniforms.Count > 0);
end;

class function TDGLCustomGLSLShaderModel.IsSupported: Boolean;
begin
//dglCheckExtension('ARB_shader_objects');
  Result := dglCheckExtension('EXT_gpu_shader4') or dglCheckExtension('ARB_gpu_shader5');
end;

procedure TDGLCustomGLSLShaderModel.Loaded;
var
  T: TDGLShaderType;
  i: Integer;
begin
  for T := Low(TDGLShaderType) to High(TDGLShaderType) do
    SetLibShaderName(T, FLibShaderName[T]);
  for i := 0 to FUniforms.Count - 1 do
    if FUniforms[i] is TDGLShaderUniformTexture then
      TDGLShaderUniformTexture(FUniforms[i]).Loaded;
end;

procedure BeginPatch; //(mode: TGLEnum);
begin
//  if mode = GL_PATCHES then
//    glBegin(GL_PATCHES)
//  else
//  if (mode = GL_TRIANGLES) or (mode = GL_TRIANGLE_STRIP) or (mode = GL_TRIANGLE_FAN)  then
//  begin
//    if mode = GL_QUADS then
//      GL.PatchParameteri(GL_PATCH_VERTICES, 4)
//    else
    glPatchParameteri(GL_PATCH_VERTICES, 3);
//    vStoreBegin(GL_PATCHES);
//  end
//  else
//  begin
////   // glBegin := vStoreBegin;
//    DGLSLogger.LogError('BeginPatch called with unsupported primitive for tessellation');
//    Abort;
//  end;
end;

procedure TDGLCustomGLSLShaderModel.Notification(Sender: TObject; Operation: TOperation);
var
  ST: TDGLShaderType;
begin
  if Operation = opRemove then
  begin
    for ST := Low(TDGLShaderType) to High(TDGLShaderType) do
      if FShaders[ST] = Sender then
      begin
        FShaders[ST]       := nil;
        FLibShaderName[ST] := '';
        NotifyChange(Self);
        exit;
      end;
  end;
end;

procedure TDGLCustomGLSLShaderModel.NotifyChange(Sender: TObject);
begin
  FHandle.NotifyChangesOfData;
  inherited;
end;

procedure TDGLCustomGLSLShaderModel.Assign(Source: TPersistent);
var
  SM: TDGLCustomGLSLShaderModel;
begin
  if Source is TDGLCustomGLSLShaderModel then
  begin
    SM                    := TDGLCustomGLSLShaderModel(Source);
    VertexShaderName      := SM.VertexShaderName;
    FragmentShaderName    := SM.FragmentShaderName;
    GeometryShaderName    := SM.GeometryShaderName;
    TessControlShaderName := SM.TessControlShaderName;
    TessEvalShaderName    := SM.TessEvalShaderName;
    //ComputeShaderName
    FFailedInitAction := SM.FFailedInitAction;
  end;
  inherited;
end;

function TDGLCustomGLSLShaderModel.GetMaterialLib: TDGLMaterialLibrary;
begin
 if assigned(FMaterialLibrary) then
   result := FMaterialLibrary
 else result:=nil;
end;

function TDGLCustomGLSLShaderModel.GetMaterialLibrary: TDGLAbstractMaterialLibrary;
begin
 if assigned(FMaterialLibrary) then
   result := TDGLAbstractMaterialLibrary(FMaterialLibrary)
 else result:=nil;
end;


procedure TDGLCustomGLSLShaderModel.SetMaterialLibrary(const Value: TDGLMaterialLibrary);
begin
  if FMaterialLibrary = Value then exit;
  FMaterialLibrary := Value;
end;

procedure TDGLCustomGLSLShaderModel.SetMaterialName(const Value: TDGLLibMaterialName);
var
  LMat: TDGLMaterial;
begin
  if csLoading in GetMaterialLibrary.ComponentState then
  begin
    FMaterialName := Value;
    exit;
  end;

  if Assigned(FMaterial) then
  begin
    if FMaterial.Name = Value then exit;
    FMaterial.UnregisterUser(Self);
    FMaterial := nil;
  end;

  LMat := GetMaterialLib.Materials.GetLibMaterialByName(Value);

  if Assigned(LMat) then
  begin
    if LMat is TDGLMaterial then
    begin
      LMat.RegisterUser(Self);
      FMaterial := LMat;
    end;
  end;
  NotifyChange(Self);
end;

procedure TDGLCustomGLSLShaderModel.DoOnPrepare(Sender: TDGLContext);
var
  T:                   TDGLShaderType;
  LUniforms:           TDGLPersistentObjectList;
  LUniform, LUniform2: TDGLShaderUniform;
  ID:                  TGLUint;
  i, J, C:             Integer;
  buff:                array [0 .. 255] of AnsiChar;
  Size:                TGLint;
  Len:                 GLsizei;
  Loc:                 TGLint;
  AType:               GLenum;
  UName:               string;
  GLSLData:            TDGLSLDataType;
  GLSLSampler:         TDGLSLSamplerType;
  bSampler:            Boolean;
  bNew:                Boolean;
  LEvent:              TOnUniformInitialize;
begin
  if FEnabled then
    try
      if IsSupported and FHandle.IsSupported then
      begin
        FHandle.AllocateHandle;
        if FHandle.IsDataNeedUpdate then
        begin
          // Validate shaders
          for T := Low(TDGLShaderType) to High(TDGLShaderType) do
            if Assigned(FShaders[T]) then
            begin
              FShaders[T].DoOnPrepare(Sender);
              if not FShaders[T].IsValid then
              begin
                if IsDesignTime then
                  FInfoLog := Format('%s shader "%s" is invalid', [cShaderTypeName[FShaders[T].ShaderType], FShaders[T].Name]);
                FIsValid   := False;
                exit;
              end;
            end;
          // Gather shader
          FHandle.DetachAllObject;
          for T := Low(TDGLShaderType) to High(TDGLShaderType) do
            if Assigned(FShaders[T]) then
              FHandle.AttachObject(FShaders[T].Handle);
          ID := FHandle.Handle;


            // Can be override by layouts in shader
            if Assigned(FShaders[shtGeometry]) then
            begin
              glProgramParameteri(ID, GL_GEOMETRY_INPUT_TYPE_EXT, cGLgsInTypes[FShaders[shtGeometry].GeometryInput]);
              glProgramParameteri(ID, GL_GEOMETRY_OUTPUT_TYPE_EXT, cGLgsOutTypes[FShaders[shtGeometry].GeometryOutput]);
              glProgramParameteri(ID, GL_GEOMETRY_VERTICES_OUT_EXT, FShaders[shtGeometry].GeometryVerticesOut);
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
                  GL_LINES_ADJACENCY_EXT: FShaders[shtGeometry].FGeometryInput := gsInAdjLines;
                  GL_TRIANGLES: FShaders[shtGeometry].FGeometryInput := gsInTriangles;
                  GL_TRIANGLES_ADJACENCY_EXT: FShaders[shtGeometry].FGeometryInput := gsInAdjTriangles;
                end;
                glGetProgramiv(ID, GL_GEOMETRY_OUTPUT_TYPE_EXT, @AType);
                case AType of
                  GL_POINTS: FShaders[shtGeometry].FGeometryOutput := gsOutPoints;
                  GL_LINE_STRIP: FShaders[shtGeometry].FGeometryOutput := gsOutLineStrip;
                  GL_TRIANGLE_STRIP: FShaders[shtGeometry].FGeometryOutput := sOutTriangleStrip;
                end;
                glGetProgramiv(ID, GL_GEOMETRY_VERTICES_OUT_EXT, @i);
                if i > 0 then FShaders[shtGeometry].FGeometryVerticesOut := i;
                ClearOpenGLError;
              end;

              // Get uniforms
              LUniforms := TDGLPersistentObjectList.Create;

              glGetProgramiv(ID, GL_ACTIVE_UNIFORMS, @C);
              for i := 0 to C - 1 do
              begin
                glGetActiveUniform(ID, TGLUint(i), Length(buff), Len, Size, AType, @buff[0]);
                Loc := glGetUniformLocation(ID, @buff[0]);
                if Loc < 0 then continue;
                UName       := Copy(string(buff), 0, Len);
                GLSLData    := GLSLTypeUndefined;
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
                  // ------------------------------------------------------------------------------
                  GL_SAMPLER_1D: GLSLSampler := GLSLSampler1D;
                  GL_SAMPLER_2D: GLSLSampler := GLSLSampler2D;
                  GL_SAMPLER_3D: GLSLSampler := GLSLSampler3D;
                  GL_SAMPLER_CUBE: GLSLSampler := GLSLSamplerCube;
                  GL_SAMPLER_1D_SHADOW: GLSLSampler := GLSLSampler1DShadow;
                  GL_SAMPLER_2D_SHADOW: GLSLSampler := GLSLSampler2DShadow;
                  GL_SAMPLER_2D_RECT: GLSLSampler := GLSLSamplerRect;
                  GL_SAMPLER_2D_RECT_SHADOW: GLSLSampler := GLSLSamplerRectShadow;
                  GL_SAMPLER_BUFFER: GLSLSampler := GLSLSamplerBuffer;
                  GL_INT_SAMPLER_2D_RECT: GLSLSampler := GLSLIntSamplerRect;
                  GL_INT_SAMPLER_BUFFER: GLSLSampler := GLSLIntSamplerBuffer;
                  GL_UNSIGNED_INT_SAMPLER_1D: GLSLSampler := GLSLUIntSampler1D;
                  GL_UNSIGNED_INT_SAMPLER_2D: GLSLSampler := GLSLUIntSampler2D;
                  GL_UNSIGNED_INT_SAMPLER_3D: GLSLSampler := GLSLUIntSampler3D;
                  GL_UNSIGNED_INT_SAMPLER_CUBE: GLSLSampler := GLSLUIntSamplerCube;
                  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY: GLSLSampler := GLSLUIntSampler1DArray;
                  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY: GLSLSampler := GLSLUIntSampler2DArray;
                  GL_UNSIGNED_INT_SAMPLER_2D_RECT: GLSLSampler := GLSLUIntSamplerRect;
                  GL_UNSIGNED_INT_SAMPLER_BUFFER: GLSLSampler := GLSLUIntSamplerBuffer;
                  GL_SAMPLER_2D_MULTISAMPLE: GLSLSampler := GLSLSamplerMS;
                  GL_INT_SAMPLER_2D_MULTISAMPLE: GLSLSampler := GLSLIntSamplerMS;
                  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE: GLSLSampler := GLSLUIntSamplerMS;
                  GL_SAMPLER_2D_MULTISAMPLE_ARRAY: GLSLSampler := GLSLSamplerMSArray;
                  GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY: GLSLSampler := GLSLIntSamplerMSArray;
                  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY: GLSLSampler := GLSLUIntSamplerMSArray;
                end;

                bSampler := False;
                if (GLSLData = GLSLTypeUndefined) and (GLSLSampler = GLSLSamplerUndefined) then
                begin
                  DGLSLogger.LogWarningFmt('Detected active uniform "%s" with unknown type', [UName]);
                  continue;
                end
                else if GLSLData <> GLSLTypeUndefined then
                begin
                  DGLSLogger.LogInfoFmt('Detected active uniform: %s %s', [cGLSLTypeString[GLSLData], UName]);
                end
                else
                begin
                  bSampler := True;
                  DGLSLogger.LogInfoFmt('Detected active uniform: %s %s', [cGLSLSamplerString[GLSLSampler], UName]);
                end;

                // Find already existing uniform
                bNew  := True;
                for J := 0 to FUniforms.Count - 1 do
                begin
                  if not(FUniforms[J] is TDGLShaderUniform) then continue;
                  LUniform := TDGLShaderUniform(FUniforms[J]);
                  if not Assigned(LUniform) then continue;
                  if LUniform.Name = UName then
                  begin
                    if bSampler and (LUniform is TDGLShaderUniformTexture) then
                    begin
                      if TDGLShaderUniformTexture(LUniform).FSamplerType = GLSLSampler then
                      begin
                        LUniform.FLocation                        := Loc;
                        LUniform.FType                            := GLSLType1I;
                        TDGLShaderUniformTexture(LUniform).FTarget := cSamplerToTexture[GLSLSampler];
                        LUniforms.Add(LUniform);
                        FUniforms[J] := nil;
                        bNew         := False;
                        break;
                      end
                    end
                    else
                    begin
                      if LUniform.FType = GLSLData then
                      begin
                        if (LUniform is TDGLShaderUniformDSA) and not dglCheckExtension('EXT_direct_state_access') then
                        begin
                          LUniform2 := LUniform;
                          LUniform  := TDGLShaderUniform.Create(Self);
                          LUniform._AddRef;
                          LUniform.Assign(LUniform2);
                          LUniform2._Release;
                        end;
                        LUniform.FLocation := Loc;
                        LUniforms.Add(LUniform);
                        FUniforms[J] := nil;
                        bNew         := False;
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
                    LUniform                                       := TDGLShaderUniformTexture.Create(Self);
                    LUniform.FType                                 := GLSLType1I;
                    TDGLShaderUniformTexture(LUniform).FSamplerType := GLSLSampler;
                    TDGLShaderUniformTexture(LUniform).FTarget      := cSamplerToTexture[GLSLSampler];
                  end
                  else
                  begin
                    if dglCheckExtension('EXT_direct_state_access') then
                      LUniform := TDGLShaderUniformDSA.Create(Self)
                    else
                      LUniform     := TDGLShaderUniform.Create(Self);
                    LUniform.FType := GLSLData;
                  end;
                  LUniform._AddRef;
                  LUniform.FName         := UName;
                  LUniform.FNameHashCode := ComputeNameHashKey(UName);
                  LUniform.FLocation     := Loc;
                  LUniforms.Add(LUniform);
                end;
              end; // for I

              // Clean old unused uniforms
              ReleaseUniforms(FUniforms);
              // Assign new one
              FUniforms := LUniforms;

              FHandle.NotifyDataUpdated;
              FIsValid := True;

//              if Self is TGLShaderModel3 then
//                LEvent := GetMaterial.FOnSM3UniformInit
//              else if Self is TGLShaderModel4 then
//                LEvent := GetMaterial.FOnSM4UniformInit
//              else
              if Self is TDGLCustomGLSLShaderModel then
                LEvent := GetShader.FOnSMUniformInit
              else
                LEvent := nil;

              if Assigned(LEvent) then
                LEvent(Self);

            end // if LinkProgram
            else
              FIsValid := False;
//          end; // with GL

          if IsDesignTime then
          begin
            FInfoLog := FHandle.InfoLog;
            if (Length(FInfoLog) = 0) and FIsValid then
              FInfoLog := 'Link successful';
          end
          else if FIsValid then
            DGLSLogger.LogInfoFmt('Program "%s" link successful - %s', [GetShader.Name, FHandle.InfoLog])
          else
            DGLSLogger.LogErrorFmt('Program "%s" link failed! - %s', [GetShader.Name, FHandle.InfoLog]);
        end;
      end
      else
      begin
        if IsDesignTime then
          FInfoLog := 'Not supported by hardware';
        FIsValid   := False;
      end;

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
end;

function TDGLCustomGLSLShaderModel.DoUnApply(var ARci: TRenderContextInfo):boolean;
begin
  ARci.amalgamating := False;
  result:=false;
  if FIsValid then
  begin
    FHandle.EndUseProgramObject;
    result:=true;
  end;
end;

procedure TDGLCustomGLSLShaderModel.DoApply(var ARci: TRenderContextInfo);
var
  i:      Integer;
  LEvent: TOnUniformSetting;
begin
  if FIsValid then
  begin
    FHandle.UseProgramObject;
    if Assigned(FShaders[shtControl]) or Assigned(FShaders[shtEvaluation]) then
    begin
      BeginPatch;
      ARci.amalgamating := True;
    end;

    if FAutoFill then
      for i := FUniforms.Count - 1 downto 0 do
        TDGLAbstractShaderUniform(FUniforms[i]).Apply(ARci);

 //   if Self is TGLShaderModel3 then
//      LEvent := GetMaterial.FOnSM3UniformSetting
//    else if Self is TGLShaderModel4 then
//      LEvent := GetMaterial.FOnSM4UniformSetting
//    else
  if Self is TDGLCustomGLSLShaderModel then
      LEvent := GetShader.FOnSMUniformSetting
    else
      LEvent := nil;

    if Assigned(LEvent) then
      LEvent(Self, ARci);
  end;
end;

procedure TDGLCustomGLSLShaderModel.ReadUniforms(AStream: TStream);
var
  LReader:  TReader;
  n, i:     Integer;
  str:      string;
  LUniform: TDGLAbstractShaderUniform;
  LClass:   CGLAbstractShaderUniform;
begin
  LReader := TReader.Create(AStream, 16384);
  try
    n     := LReader.ReadInteger;
    for i := 0 to n - 1 do
    begin
      str      := LReader.ReadString;
      LClass   := CGLAbstractShaderUniform(FindClass(str));
      LUniform := LClass.Create(Self);
      LUniform._AddRef;
      LUniform.ReadFromFiler(LReader);
      FUniforms.Add(LUniform);
    end;
  finally
    LReader.Free;
  end;
end;

class procedure TDGLCustomGLSLShaderModel.ReleaseUniforms(AList: TDGLPersistentObjectList);
var
  i: Integer;
begin
  for i := 0 to AList.Count - 1 do
    if Assigned(AList[i]) then
      TDGLAbstractShaderUniform(AList[i])._Release;
  AList.Destroy;
end;

function TDGLCustomGLSLShaderModel.GetLibShaderName(AType: TDGLShaderType): string;
begin
  if Assigned(FShaders[AType]) then
    Result := FShaders[AType].Name
  else
    Result := '';
end;

function TDGLCustomGLSLShaderModel.GetUniform(const AName: string): IShaderParameter;
var
  h, i: Integer;
  U:    TDGLAbstractShaderUniform;
begin
  Result := nil;
  h      := ComputeNameHashKey(AName);
  for i  := 0 to FUniforms.Count - 1 do
  begin
    U := TDGLAbstractShaderUniform(FUniforms[i]);
    if (U.FNameHashCode = h) and (U.FName = AName) then
    begin
      Result := U;
      exit;
    end;
  end;

  if not IsDesignTime then
  begin
    DGLSLogger.LogErrorFmt('Attempt to use unknow uniform "%s" for material "%s"', [AName, GetShader.Name]);
    U := TDGLAbstractShaderUniform.Create(Self);
    U._AddRef;
    U.FName         := AName;
    U.FNameHashCode := h;
    FUniforms.Add(U);
    Result := U;
  end;
end;

procedure TDGLCustomGLSLShaderModel.GetUniformNames(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := 0 to FUniforms.Count - 1 do
    Proc(TDGLAbstractShaderUniform(FUniforms[i]).FName);
end;

procedure TDGLCustomGLSLShaderModel.SetLibShaderName(AType: TDGLShaderType; const AValue: string);
var
  LShader: TDGLSLShaderScript;
begin
  if csLoading in GetShaderLibraryEx.ComponentState then
  begin
    FLibShaderName[AType] := AValue;
    exit;
  end;

  if Assigned(FShaders[AType]) then
  begin
    FShaders[AType].UnregisterUser(Self);
    FShaders[AType]       := nil;
    FLibShaderName[AType] := '';
  end;

  LShader :=  TDGLSLShaderScript(GetShaderLibraryEx.Components.GetItemByName(AValue));
  if Assigned(LShader) then
  begin
    if LShader.ShaderType <> AType then
    begin
      if IsDesignTime then
        InformationDlg(Format('Incompatible shader type, need %s shader', [cShaderTypeName[AType]]));
      exit;
    end;
    LShader.RegisterUser(Self);
    FShaders[AType]       := LShader;
    FLibShaderName[AType] := AValue;
  end;
  NotifyChange(Self);
end;

procedure TDGLCustomGLSLShaderModel.WriteUniforms(AStream: TStream);
var
  LWriter: TWriter;
  i:       Integer;
begin
  LWriter := TWriter.Create(AStream, 16384);
  try
    LWriter.WriteInteger(FUniforms.Count);
    for i := 0 to FUniforms.Count - 1 do
    begin
      LWriter.WriteString(FUniforms[i].ClassName);
      TDGLAbstractShaderUniform(FUniforms[i]).WriteToFiler(LWriter);
    end;
  finally
    LWriter.Free;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ---------------
{ TDGLLibShader }
{$IFDEF GLS_REGION}{$REGION 'TDGLLibShader'}{$ENDIF}

procedure TDGLLibShader.Apply(var ARci: TRenderContextInfo);
var
  LevelReady:  array [TDGLShaderMaterialLevel] of Boolean;
  L, MaxLevel: TDGLShaderMaterialLevel;
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
    // Level selection
    LevelReady[mlBaseMaterial]  :=  FSM.Material.BaseMaterial.Enabled;
    LevelReady[mlMultitexturing] := FSM.Material.Multitexturing.Enabled and FSM.Material.Multitexturing.IsValid;
    LevelReady[mlShader]         := FSM.Enabled and FSM.IsValid;

    if FApplicableLevel = mlAuto then
      MaxLevel := mlShader
    else
      MaxLevel := FApplicableLevel;

    FSelectedLevel := mlAuto;
    for L          := MaxLevel downto mlBaseMaterial do
      if LevelReady[L] then
      begin
        FSelectedLevel := L;
        break;
      end;

    FHandle.NotifyDataUpdated;
  end;

  ARci.GLStates.currentShaderLevel := FSelectedLevel;

  case FSelectedLevel of
    mlAuto:
      ; // No one level can be used. Worst case.

    mlBaseMaterial :
      begin
         FSM.Material.BaseMaterial.Apply(ARci);
      end;

    mlMultitexturing:
      begin
        if LevelReady[mlShader] then
          FSM.Apply(ARci);
        FSM.Material.Multitexturing.Apply(ARci);
      end;

    mlShader:
      begin
       // if LevelReady[mlBaseMaterial] then
       //   Material.BaseMaterial.Apply(ARci);
        FSM.Apply(ARci);
        if LevelReady[mlMultitexturing] then
         FSM.Material.Multitexturing.Apply(ARci);
      end;

  end;
end;

procedure TDGLLibShader.Assign(Source: TPersistent);
var
  LShader: TDGLLibShader;
begin
  if Source is TDGLLibShader then
  begin
    LShader := TDGLLibShader(Source);

    FSM.Assign(LShader.FSM);
    FApplicableLevel := LShader.FApplicableLevel;
    NotifyChange(Self);
  end;
  inherited;
end;

constructor TDGLLibShader.Create(ACollection: TCollection);
begin
  inherited;
  FHandle            := TDGLVirtualHandle.Create;
  FHandle.OnAllocate := DoAllocate;
  FHandle.OnDestroy  := DoDeallocate;
  FHandle.OnPrepare  := DoOnPrepare;
  FApplicableLevel   := mlAuto;
  FSelectedLevel     := mlAuto;

  FSM               := TDGLBaseGLSLShaderModel.Create(Self);
end;

//type
//  TGLFriendlyShaderModel = class(TDGLCustomGLSLShader);

destructor TDGLLibShader.Destroy;
//var
//  i:     Integer;
//  LUser: TObject;
begin
  FHandle.Destroy;
  FSM.Destroy;
//  for i := 0 to FUserList.Count - 1 do
//  begin
//    LUser := TObject(FUserList[i]);
//    if LUser is TDGLShader then
//      TGLFriendlyShader(LUser).NotifyLibShaderDestruction;
//  end;
  inherited;
end;

procedure TDGLLibShader.DoAllocate(Sender: TDGLVirtualHandle; var Handle: TGLUint);
begin
  Handle := 1;
end;

procedure TDGLLibShader.DoDeallocate(Sender: TDGLVirtualHandle; var Handle: TGLUint);
begin
  Handle := 0;
end;

procedure TDGLLibShader.DoOnPrepare(Sender: TDGLContext);
begin
 FSM.DoOnPrepare(Sender);
end;

procedure TDGLLibShader.Loaded;
begin
  FSM.Loaded;
end;

procedure TDGLLibShader.NotifyChange(Sender: TObject);
begin
  inherited;
  FHandle.NotifyChangesOfData;
end;

//procedure TDGLLibShader.RemoveDefferedInit;
//var
//  i:  Integer;
////  ST: TGLShaderType;
//begin
//
//  if FSM.Enabled then
//  begin
//    for ST := Low(TDGLShaderType) to High(TDGLShaderType) do
//      if Assigned(FSM.FShaders[ST]) then
//        FSM.FShaders[ST].FDefferedInit := False;
//  end;
//
//  CurrentDGLContext.PrepareHandlesData;
//end;


procedure TDGLLibShader.SetLevel(AValue: TDGLShaderMaterialLevel);
begin
  if FApplicableLevel <> AValue then
  begin
    FApplicableLevel := AValue;
    NotifyChange(Self);
  end;
end;

procedure TDGLLibShader.SetSM(AValue: TDGLBaseGLSLShaderModel);
begin
  FSM.Assign(AValue);
end;

function TDGLLibShader.UnApply(var ARci: TRenderContextInfo): Boolean;

  procedure GetNextPass(AProp: TDGLLibShaderProperty);
  begin
    if Length(AProp.NextPass) > 0 then
      FNextPass := TDGLShaderLibrary(GetShaderLibrary).Shaders.GetLibShaderByName(AProp.NextPass)
    else
      FNextPass := nil;

    if FNextPass = Self then
    begin
      AProp.NextPass := '';
      FNextPass      := nil;
    end;
  end;

begin

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
    mlBaseMaterial :
      begin
        if FSM.Enabled then FSM.UnApply(ARci);
        FSM.Material.BaseMaterial.UnApply(ARci);
        GetNextPass(FSM);
      end;

    mlMultitexturing :
      begin
        if FSM.Enabled then FSM.UnApply(ARci);
        FSM.Material.Multitexturing.UnApply(ARci);
        GetNextPass(FSM);
      end;

    mlShader:
      begin
//        if Material.BaseMaterial.Enabled then
//          Material.BaseMaterial.UnApply(ARci);
        if FSM.Material.Multitexturing.Enabled then
          FSM.Material.Multitexturing.UnApply(ARci);
        FSM.UnApply(ARci);

        GetNextPass(FSM);
      end;

  else
    FNextPass := nil;
  end;
  ARci.GLStates.ActiveTexture := 0;

  Result := Assigned(FNextPass);
  if Result then
    FNextPass.Apply(ARci);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ----------------
{ TDGLMatLibComponents }
{$IFDEF GLS_REGION}{$REGION 'TDGLMatLibComponents'}{$ENDIF}

function TDGLShaderLibComponents.GetItemByName(const AName: TDGLShaderComponentName): TDGLBaseShaderComponentItem;
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

function TDGLShaderLibComponents.GetItems(Index: Integer): TDGLBaseShaderComponentItem;
begin
  Result := TDGLBaseShaderComponentItem(inherited GetItems(index));
end;

function TDGLShaderLibComponents.GetNamePath: string;
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

class function TDGLShaderLibComponents.ItemsClass: TDGLXCollectionItemClass;
begin
  Result := TDGLBaseShaderComponentItem;
end;

function TDGLShaderLibComponents.MakeUniqueName(const AName: TDGLShaderComponentName): TDGLShaderComponentName;
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
{ TDGLShaderLibrary }
{$IFDEF GLS_REGION}{$REGION 'TDGLShaderLibrary'}{$ENDIF}

function TDGLShaderLibrary.AddShader(const AName: TDGLShaderComponentName): TDGLSLShaderScript;
begin
  Result      := TDGLSLShaderScript.Create(Components);
  Result.Name := AName;
  Components.Add(Result);
end;

constructor TDGLShaderLibrary.Create(AOwner: TComponent);
begin
  inherited;
  FShaders  := TDGLLibShaders.Create(Self);
  FComponents := TDGLShaderLibComponents.Create(Self);
end;

procedure TDGLShaderLibrary.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('ComponentsData', ReadComponents, WriteComponents, Components.Count > 0);
  inherited;
end;

destructor TDGLShaderLibrary.Destroy;
begin
  FShaders.Destroy;
  FComponents.Destroy;
  inherited;
end;

function TDGLShaderLibrary.GetShaders: TDGLLibShaders;
begin
  Result := TDGLLibShaders(FShaders);
end;

procedure TDGLShaderLibrary.GetNames(Proc: TGetStrProc; AClass: CDGLBaseShaderCollectionItem);
var
  i: Integer;
begin
  for i := 0 to Components.Count - 1 do
    if Components[i].ClassType = AClass then
      Proc(Components[i].Name)
end;

procedure TDGLShaderLibrary.Loaded;
begin
  inherited;
end;

procedure TDGLShaderLibrary.ReadComponents(AStream: TStream);
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

procedure TDGLShaderLibrary.SetComponents(AValue: TDGLShaderLibComponents);
begin
  FComponents.Assign(AValue);
end;

//procedure TDGLShaderLibrary.SetLevelForAll(const ALevel: TDGLShaderLevel);
//var
//  i: Integer;
//begin
//  for i                          := Shaders.Count - 1 downto 0 do
//    Shaders[i].ApplicableLevel := ALevel;
//end;

procedure TDGLShaderLibrary.SetShaders(AValue: TDGLLibShaders);
begin
  FShaders.Assign(AValue);
end;

function TDGLShaderLibrary.StoreShaders: Boolean;
begin
  Result := (FShaders.Count > 0);
end;

procedure TDGLShaderLibrary.WriteComponents(AStream: TStream);
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

// ----------------
{ TStandartUniformAutoSetExecutor }
{$IFDEF GLS_REGION}{$REGION 'TStandartUniformAutoSetExecutor'}{$ENDIF}

constructor TStandardUniformAutoSetExecutor.Create;
begin
  RegisterUniformAutoSetMethod('Camera world position', GLSLType4F, SetCameraPosition);
  RegisterUniformAutoSetMethod('LightSource[0] world position', GLSLType4F, SetLightSource0Position);
  RegisterUniformAutoSetMethod('World (model) matrix', GLSLTypeMat4F, SetModelMatrix);
  RegisterUniformAutoSetMethod('WorldView matrix', GLSLTypeMat4F, SetModelViewMatrix);
  RegisterUniformAutoSetMethod('WorldNormal matrix', GLSLTypeMat3F, SetNormalModelMatrix);
  RegisterUniformAutoSetMethod('Inverse World matrix', GLSLTypeMat4F, SetInvModelMatrix);
  RegisterUniformAutoSetMethod('View matrix', GLSLTypeMat4F, SetViewMatrix);
  RegisterUniformAutoSetMethod('Inverse WorldView matrix', GLSLTypeMat4F, SetInvModelViewMatrix);
  RegisterUniformAutoSetMethod('Projection matrix', GLSLTypeMat4F, SetProjectionMatrix);
  RegisterUniformAutoSetMethod('ViewProjection matrix', GLSLTypeMat4F, SetViewProjectionMatrix);
  RegisterUniformAutoSetMethod('WorldViewProjection matrix', GLSLTypeMat4F, SetWorldViewProjectionMatrix);
  RegisterUniformAutoSetMethod('Material front face emission', GLSLType4F, SetMaterialFrontEmission);
  RegisterUniformAutoSetMethod('Material front face ambient', GLSLType4F, SetMaterialFrontAmbient);
  RegisterUniformAutoSetMethod('Material front face diffuse', GLSLType4F, SetMaterialFrontDiffuse);
  RegisterUniformAutoSetMethod('Material front face specular', GLSLType4F, SetMaterialFrontSpecular);
  RegisterUniformAutoSetMethod('Material front face shininess', GLSLType1F, SetMaterialFrontShininess);
  RegisterUniformAutoSetMethod('Material back face emission', GLSLType4F, SetMaterialBackEmission);
  RegisterUniformAutoSetMethod('Material back face ambient', GLSLType4F, SetMaterialBackAmbient);
  RegisterUniformAutoSetMethod('Material back face diffuse', GLSLType4F, SetMaterialBackDiffuse);
  RegisterUniformAutoSetMethod('Material back face specular', GLSLType4F, SetMaterialBackSpecular);
  RegisterUniformAutoSetMethod('Material back face shininess', GLSLType1F, SetMaterialBackShininess)
end;

procedure TStandardUniformAutoSetExecutor.SetCameraPosition(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.cameraPosition;
end;

procedure TStandardUniformAutoSetExecutor.SetInvModelMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.InvModelMatrix;
end;

procedure TStandardUniformAutoSetExecutor.SetInvModelViewMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.InvModelViewMatrix;
end;

procedure TStandardUniformAutoSetExecutor.SetLightSource0Position(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.LightPosition[0];
end;

procedure TStandardUniformAutoSetExecutor.SetMaterialBackAmbient(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialAmbient(cmBack);
end;

procedure TStandardUniformAutoSetExecutor.SetMaterialBackDiffuse(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialDiffuse(cmBack);
end;

procedure TStandardUniformAutoSetExecutor.SetMaterialBackEmission(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialEmission(cmBack);
end;

procedure TStandardUniformAutoSetExecutor.SetMaterialBackShininess(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.float := ARci.GLStates.MaterialShininess(cmBack);
end;

procedure TStandardUniformAutoSetExecutor.SetMaterialBackSpecular(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialSpecular(cmBack);
end;

procedure TStandardUniformAutoSetExecutor.SetMaterialFrontAmbient(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialAmbient(cmFront);
end;

procedure TStandardUniformAutoSetExecutor.SetMaterialFrontDiffuse(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialDiffuse(cmFront);
end;

procedure TStandardUniformAutoSetExecutor.SetMaterialFrontEmission(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialEmission(cmFront);
end;

procedure TStandardUniformAutoSetExecutor.SetMaterialFrontShininess(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.float := ARci.GLStates.MaterialShininess(cmFront);
end;

procedure TStandardUniformAutoSetExecutor.SetMaterialFrontSpecular(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.vec4 := ARci.GLStates.MaterialSpecular(cmFront);
end;

procedure TStandardUniformAutoSetExecutor.SetModelMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ModelMatrix;
end;

procedure TStandardUniformAutoSetExecutor.SetModelViewMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ModelViewMatrix;
end;

procedure TStandardUniformAutoSetExecutor.SetNormalModelMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat3 := ARci.PipelineTransformation.NormalModelMatrix;
end;

procedure TStandardUniformAutoSetExecutor.SetProjectionMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ProjectionMatrix;
end;

procedure TStandardUniformAutoSetExecutor.SetViewMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ViewMatrix;
end;

procedure TStandardUniformAutoSetExecutor.SetViewProjectionMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := ARci.PipelineTransformation.ViewProjectionMatrix;
end;

procedure TStandardUniformAutoSetExecutor.SetWorldViewProjectionMatrix(Sender: IShaderParameter; var ARci: TRenderContextInfo);
begin
  Sender.mat4 := MatrixMultiply(ARci.PipelineTransformation.ModelViewMatrix, ARci.PipelineTransformation.ProjectionMatrix);
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}


//----------------------------------------------------------------
//----------------------------------------------------------------
//----------------------------------------------------------------

initialization

RegisterClasses([ TDGLSLShaderScript, TDGLShaderLibrary, TDGLShaderUniform, TDGLShaderUniformDSA, TDGLShaderUniformTexture]);

RegisterXCollectionItemClass(TDGLSlShaderScript);


vStandartUniformAutoSetExecutor := TStandardUniformAutoSetExecutor.Create;

//----------------------------------------------------------------
//----------------------------------------------------------------

finalization

vStandartUniformAutoSetExecutor.Destroy;

//----------------------------------------------------------------

end.
