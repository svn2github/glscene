//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xShadersManager<p>

   Base unit to manage shader objects and program.<p>
   Can work with different rendering context and thread.<p>
   Once defined program or object can be used everywhere.<p>

   <b>History : </b><font size=-1><ul>
    <li>10/10/10 - Yar - Added binary caching. UniformSampler optimization
    <li>14/04/10 - Yar - Attributes and Uniforms becomes one class per name
                         Added multicontext and multithreading support
    <li>24/03/10 - Yar - Creation (based on uShaders.pas by Alex Karpenyuk aka Fantom)
 </ul></font>
}

unit GLShaderManager;

interface

{$I GLScene.inc}

uses
  Classes,
  SysUtils,
{$IFDEF FPC}
  LCLVersion,
{$ENDIF}
  // GLScene
  BaseClasses,
  GLCrossPlatform,
  OpenGLTokens,
  GLContext,
  GLState,
  GLTextureFormat,
  VectorLists,
  VectorGeometry,
  VectorTypes,
  GLSRedBlackTree,
  VectorGeometryEXT,
  GLSLog, GLUtils,
  GLSGenerics;

const
  GLS_PROGRAM_CACHE_SIZE = 2048;
  GLS_MAX_PROGRAM_FAIL = 3;

type

  TGLSLProgramType = (ptVertex, ptGeometry, ptFragment, ptControl, ptEvaluation);
  TGLSLProgramTypes = set of TGLSLProgramType;

const

  cObjectTypeName: array[TGLSLProgramType] of string =
    ('VERTEX', 'GEOMTERY', 'FRAGMENT', 'CONTROL', 'EVALUATION');

{$IFDEF FPC}
  {$IF (LCL_RELEASE < 31)}
    {$DEFINE GLS_GENERIC_PREFIX}
  {$IFEND}
{$ENDIF}

  cEnUsFormatSettings: TFormatSettings = (
{$IFDEF GLS_DELPHI_OR_CPPB}

{$IFDEF GLS_DELPHI_XE_UP}
    CurrencyString: '$';
    CurrencyFormat: 0;
    CurrencyDecimals: 2;
    DateSeparator: '/';
    TimeSeparator: ':';
    ListSeparator: ',';
    ShortDateFormat: 'M/d/yyyy';
    LongDateFormat: 'dddd, MMMM dd, yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'h:mm AMPM';
    LongTimeFormat: 'h:mm:ss AMPM';
    ShortMonthNames: ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
    LongMonthNames: ('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December');
    ShortDayNames: ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    LongDayNames: ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    TwoDigitYearCenturyWindow: 0;
    NegCurrFormat: 0;
{$ELSE}
    CurrencyFormat: 0;
    NegCurrFormat: 0;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '/';
    TimeSeparator: ':';
    ListSeparator: ',';
    ShortDateFormat: 'M/d/yyyy';
    LongDateFormat: 'dddd, MMMM dd, yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'h:mm AMPM';
    LongTimeFormat: 'h:mm:ss AMPM';
    ShortMonthNames: ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
    LongMonthNames: ('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December');
    ShortDayNames: ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    LongDayNames: ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
    TwoDigitYearCenturyWindow: 0;
{$ENDIF}
{$ENDIF GLS_DELPHI_OR_CPPB}

{$IFDEF FPC}
    CurrencyFormat: 0;
    NegCurrFormat: 0;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '/';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'M/d/yyyy';
    LongDateFormat: 'dddd, MMMM dd, yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'h:mm AMPM';
    LongTimeFormat: 'h:mm:ss AMPM';
    ShortMonthNames:  ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
    LongMonthNames: ('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December');
    ShortDayNames: ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    LongDayNames: ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
    TwoDigitYearCenturyWindow: 0;
{$ENDIF}
  );

type

  TGLColorComponent = (ccmRed, ccmGreen, ccmBlue, ccmAlpha, ccmWhite);
  TGLColorComponentMask = set of TGLColorComponent;

  TGLSLDataType = (
    GLSLTypeUndefined,
    GLSLTypeVoid,
    GLSLType1F,
    GLSLType2F,
    GLSLType3F,
    GLSLType4F,
    GLSLType1I,
    GLSLType2I,
    GLSLType3I,
    GLSLType4I,
    GLSLType1UI,
    GLSLType2UI,
    GLSLType3UI,
    GLSLType4UI,
    GLSLTypeMat2F,
    GLSLTypeMat3F,
    GLSLTypeMat4F,
    GLSLTypeSampler1D,
    GLSLTypeSampler2D,
    GLSLTypeSampler3D,
    GLSLTypeSamplerCube,
    GLSLTypeSampler1DShadow,
    GLSLTypeSampler2DShadow,
    GLSLTypeSampler1DArray,
    GLSLTypeSampler2DArray,
    GLSLTypeSampler1DArrayShadow,
    GLSLTypeSampler2DArrayShadow,
    GLSLTypeSamplerCubeShadow,
    GLSLTypeIntSampler1D,
    GLSLTypeIntSampler2D,
    GLSLTypeIntSampler3D,
    GLSLTypeIntSamplerCube,
    GLSLTypeIntSampler1DArray,
    GLSLTypeIntSampler2DArray,
    GLSLTypeUIntSampler1D,
    GLSLTypeUIntSampler2D,
    GLSLTypeUIntSampler3D,
    GLSLTypeUIntSamplerCube,
    GLSLTypeUIntSampler1DArray,
    GLSLTypeUIntSampler2DArray,
    GLSLTypeSamplerRect,
    GLSLTypeSamplerRectShadow,
    GLSLTypeSamplerBuffer,
    GLSLTypeIntSamplerRect,
    GLSLTypeIntSamplerBuffer,
    GLSLTypeUIntSamplerRect,
    GLSLTypeUIntSamplerBuffer,
    GLSLTypeSamplerMS,
    GLSLTypeIntSamplerMS,
    GLSLTypeUIntSamplerMS,
    GLSLTypeSamplerMSArray,
    GLSLTypeIntSamplerMSArray,
    GLSLTypeUIntSamplerMSArray,
    GLSLTypeVRec,
    GLSLTypeFRec,
    GLSLTypeLRec
    );

  TProgramCache = record
    Location: GLint;
    BindingIndex: GLuint;
    DataType: TGLSLDataType;
    WarningAbsenceLoged: Boolean;
  end;

  TProgramCacheTree = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
    GRedBlackTree < TGLProgramHandle, Integer > ;

  TGLSLBaseVariable = class
  private
    FName: string;
    FProgramCacheTree: TProgramCacheTree;
    FLastProgram: TGLProgramHandle;
    FLastCacheIndex: Integer;
    FTag: Integer;
    FTagObject: TObject;
    procedure DoCache(AProg: TGLProgramHandle; const ACache: TProgramCache);
    procedure ClearCache(AProg: TGLProgramHandle);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ResetLocationCache;

    property Name: string read FName;
    property Tag: Integer read FTag write FTag;
    property TagObject: TObject read FTagObject write FTagObject;
  end;

  // TGLSLAttribute
  //

  TGLSLAttribute = class(TGLSLBaseVariable)
  private
    function GetLocation: GLInt;
    procedure SetLocation(Index: GLInt);
    function GetDataType: TGLSLDataType;
  public
    procedure BeforeDestruction; override;
    constructor RegisterAttribute(const AName: string);
    class function GetAttribute(const AName: string): TGLSLAttribute;

    property Location: GLInt read GetLocation write SetLocation;
    property DataFormat: TGLSLDataType read GetDataType;
  end;

  TGLSLAttributeArray = array[0..GLS_VERTEX_ATTR_NUM - 1] of TGLSLAttribute;

  // TGLSLUniform
  //

  TGLSLUniform = class(TGLSLBaseVariable)
  private
    { Private Declarations }
    FBlockOffset: TGLint;
    function GetLocation: GLInt;
    function GetDataType: TGLSLDataType;
  public
    { Public Declarations }
    procedure BeforeDestruction; override;
    constructor RegisterUniform(const AName: string);
    class function GetUniform(const AName: string): TGLSLUniform;

    property Location: GLInt read GetLocation;
    property DataType: TGLSLDataType read GetDataType;

  end;

  // TGLSLUniformBlock
  //

  TGLSLUniformBlock = class(TGLSLBaseVariable)
  private
    { Private Declarations }
    FSize: GLSizei;
    FUniforms: TList;
    function GetLocation: GLInt;
    function GetBindingIndex: TGLuint;
    procedure SetBindingIndex(Value: TGLuint);
  public
    { Public Declarations }
    procedure BeforeDestruction; override;
    constructor RegisterUniformBlock(const AName: string);
    class function GetUniformBlock(const AName: string): TGLSLUniformBlock;

    property Location: GLInt read GetLocation;
    property DataSize: GLSizei read FSize;
    property BindingIndex: TGLuint read GetBindingIndex write SetBindingIndex;
  end;

  TGLSLObjectName = class(TGLAbstractName)
  protected
    function GetInheritorClass: TGLAbstractNameClass; override;
  public
    function GetManager: TGLSAbstractManagerClass; override;
  end;

  // TGLSLObject
  //

  {: Context independent shader object }
  TGLSLObject = class(TObject)
  private
    FHandles: array[TGLSLProgramType] of TGLShaderHandle;
    FName: TGLSLObjectName;
    FCode: AnsiString;
    FHashCode: Integer;
    FTypes: TGLSLProgramTypes;
  public
    constructor Create;
    destructor Destroy; override;
    {: Compile object under current context }
    function Compile: Boolean;
    property Name: TGLSLObjectName read FName;
  end;

  TGLSLProgramName = class(TGLAbstractName)
  protected
    function GetInheritorClass: TGLAbstractNameClass; override;
  public
    function GetManager: TGLSAbstractManagerClass; override;
  end;

  TGLSLObjectList = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
    GList < TGLSLObject > ;

  // TGLSLProgram
  //

  {: Context independent program object }
  TGLSLProgram = class(TObject)
  private
    FHandle: TGLProgramHandle;
    FName: TGLSLProgramName;
    FAttachedObjects: TGLSLObjectList;
    FProgramMask: TGLSLProgramTypes;
    FLinkFailureCount: Integer;
    function FindBinary: Boolean;
    procedure StoreBinary;
  public
    constructor Create;
    destructor Destroy; override;
    {: Attach objects and link program under current context }
    procedure Attach(AObject: TGLSLObject);
    procedure Detach(AObject: TGLSLObject);
    function Link: Boolean;
    property Name: TGLSLProgramName read FName;
  end;

  ShaderManager = class(TGLSAbstractManager)
  protected
    { Protected Declarations }
    class function CurrentProgram: TGLProgramHandle;
    // Design time notifications
    class procedure NotifyProjectOpened; override;
    class procedure NotifyProjectClosed; override;
    class procedure NotifyContextCreated; override;
    class procedure NotifyBeforeCompile; override;
  private
    { Private Declarations }
    class procedure Initialize;
    class procedure Finalize;
    class procedure SaveBinaryCache(ADoFree: Boolean);

    class function GetObject(const AName: IGLName): TGLSLObject;
    class function GetProgram(const AName: IGLName): TGLSLProgram;
    class procedure PushObject(AObject: TGLSLObject);
    class procedure PushProgram(AProgram: TGLSLProgram);
    class function GetTextureTarget(const AUniform: TGLSLUniform;
      out ATarget: TGLTextureTarget): Boolean;
    class procedure DeleteShaderObject(AObject: TGLSLObject); overload;
    class procedure DeleteShaderProgram(AProgram: TGLSLProgram); overload;
    {: Cache locations of attribute, uniforms and uniform bBeginWorks }
    class procedure MakeLocationCache(AProgram: TGLSLProgram);
  public
    { Public Declarations }

    class function FillResourceList(AList: TStringList): Boolean; override;
    class procedure MakeUniqueItemName(var AName: string; AClass: TGLAbstractNameClass); override;

    class function GetObjectName(const AName: string): IGLName;
    class function GetProgramName(const AName: string): IGLName;

    class procedure DefineShaderObject(var AName: IGLName; const code: AnsiString;
      ATypes: TGLSLProgramTypes; ANameRoot: string = '');
    class procedure DefineShaderProgram(var AName: IGLName;
      AProgramMask: TGLSLProgramTypes = [ptVertex, ptFragment];
      ANameRoot: string = '');
    class procedure DeleteShaderObject(var AName: IGLName); overload;
    class procedure DeleteShaderProgram(var AName: IGLName); overload;

    class procedure AttachShaderObjectToProgram(const AObject: IGLName;
      const AProgram: IGLName);
    class procedure DetachShaderObjectFromProgram(const AObject: IGLName;
      const AProgram: IGLName);

    class procedure LinkShaderProgram(const AName: IGLName);
    class procedure UseProgram(const AName: IGLName);
    class function IsProgramLinked(const AName: IGLName): Boolean;
    class function IsProgramDefined(const AName: IGLName): Boolean;
    class function IsProgramCurrent(const AName: IGLName): Boolean;
    class function GetCurrentProgram: IGLName;
    class procedure UseFixedFunctionPipeline;
    class procedure ClearShaderObject;
    class procedure ClearShaderPrograms;
    class function GetProgramAttribs(const AName: IGLName; out Attribs: TGLSLAttributeArray): Boolean;

    // Float uniforms
    class procedure Uniform1f(AUniform: TGLSLUniform; const Value: Single); overload;
    class procedure Uniform1f(AUniform: TGLSLUniform; const Value: PFloat; Count: Integer); overload;
    class procedure Uniform2f(AUniform: TGLSLUniform; const Value: TVector2f);
    class procedure Uniform3f(AUniform: TGLSLUniform; const Value: TVector3f);
    class procedure Uniform4f(AUniform: TGLSLUniform; const Value: TVector4f); overload;
    class procedure Uniform4f(AUniform: TGLSLUniform; const Value: PFloat; Count: Integer); overload;

    // Integer uniforms
    class procedure Uniform1I(AUniform: TGLSLUniform; const Value: TGLint); overload;
    class procedure Uniform1I(AUniform: TGLSLUniform; const Value: PGLInt; Count: Integer); overload;
    class procedure Uniform2I(AUniform: TGLSLUniform; const Value: TVector2I);
    class procedure Uniform3I(AUniform: TGLSLUniform; const Value: TVector3I);
    class procedure Uniform4I(AUniform: TGLSLUniform; const Value: TVector4I); overload;
    class procedure Uniform4I(AUniform: TGLSLUniform; const Value: PGLInt; Count: Integer); overload;

    // Unsigned integer uniforms
    class procedure Uniform1UI(AUniform: TGLSLUniform; const Value: TGLuint);
    class procedure Uniform2UI(AUniform: TGLSLUniform; const Value: TVector2UI);
    class procedure Uniform3UI(AUniform: TGLSLUniform; const Value: TVector3UI);
    class procedure Uniform4UI(AUniform: TGLSLUniform; const Value: TVector4UI);

    // Matrix uniforms
    class procedure UniformMat2f(AUniform: TGLSLUniform; const Value: TMatrix2f);
    class procedure UniformMat3f(AUniform: TGLSLUniform; const Value: TMatrix3f);
    class procedure UniformMat4f(AUniform: TGLSLUniform; const Value: TMatrix4f);

    // Bind texture to uniform sampler, return active unit
    class function UniformSampler(AUniform: TGLSLUniform; const Texture: GLUInt): TGLuint;
    //    procedure UniformBuffer(const ProgramName: string; AUniform: TGLSLUniform; const Buffer: GLUInt);
  end;

var
  // Always certain attributes
  attrPosition,
    attrNormal,
    attrVertexColor,
    attrTexCoord0,
    attrTexCoord1,
    attrTexCoord2,
    attrTexCoord3,
    attrTexCoord4,
    attrTexCoord5,
    attrTexCoord6,
    attrTexCoord7,
    attrTangent,
    attrBinormal,
    attrInstanceID: TGLSLAttribute;

  uniformModelMatrix,
    uniformViewProjectionMatrix,
    uniformCameraWorldPosition,
    uniformDiffuse,
    uniformTexUnit0,
    uniformTexUnit1,
    uniformTexUnit2,
    uniformTexUnit3,
    uniformTexUnit4,
    uniformTexUnit5,
    uniformTexUnit6,
    uniformTexUnit7,
    uniformInstanceID: TGLSLUniform;

function CompareProgram(const Item1, Item2: TGLProgramHandle): Integer;
function GetMaxGLSLVersion: AnsiString;
function GetGLSLTypeCast(const Arg: AnsiString; ArgType: TGLSLDataType; AMask: TGLColorComponentMask; CastType: TGLSLDataType = GLSLTypeUndefined): AnsiString;
function StrToGLSLType(const AType: AnsiString): TGLSLDataType;
function GLSLTypeToString(AType: TGLSLDataType): AnsiString;
function GLSLTypeComponentCount(AType: TGLSLDataType): Integer;
function HexToGLSL(tp: TGLSLDataType; HexValue: string): AnsiString;
function MaskToGLSLType(AMask: TGLColorComponentMask): TGLSLDataType;
function RemoveGLSLQualifier(const InputLine: string): string;
function GLSLPartToStr(APart: TGLSLProgramTypes): string;

implementation

uses
  GLStrings,
  GLVBOManager,
  ApplicationFileIO;

const
  cGLSLTypeString: array[TGLSLDataType] of AnsiString = (
    'undefined',
    'void',
    'float',
    'vec2',
    'vec3',
    'vec4',
    'int',
    'ivec2',
    'ivec3',
    'ivec4',
    'uint',
    'uivec2',
    'uivec3',
    'uivec4',
    'mat2',
    'mat3',
    'mat4',
    'sampler1D',
    'sampler2D',
    'sampler3D',
    'samplerCube',
    'sampler1DShadow',
    'sampler2DShadow',
    'sampler1DArray',
    'sampler2DArray',
    'sampler1DArrayShadow',
    'sampler2DArrayShadow',
    'samplerCubeShadow',
    'isampler1D',
    'isampler2D',
    'isampler3D',
    'isamplerCube',
    'isampler1DArray',
    'isampler2DArray',
    'usampler1D',
    'usampler2D',
    'usampler3D',
    'usamplerCube',
    'usampler1DArray',
    'usampler2DArray',
    'samplerRect',
    'samplerRectShadow',
    'samplerBuffer',
    'isamplerRect',
    'isamplerBuffer',
    'usamplerRect',
    'usamplerBuffer',
    'samplerMS',
    'isamplerMS',
    'usamplerMS',
    'samplerMSArray',
    'isamplerMSArray',
    'usamplerMSArray',
    'vrec',
    'frec',
    'lrec');

  cGLSLTypeComponents: array[TGLSLDataType] of Integer = (
    0,
    0,
    1,
    2,
    3,
    4,
    1,
    2,
    3,
    4,
    1,
    2,
    3,
    4,
    4,
    9,
    16,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    0,
    0);

const
  cObjectClass: array[TGLSLProgramType] of TGLShaderHandleClass =
    (TGLVertexShaderHandle,
    TGLGeometryShaderHandle,
    TGLFragmentShaderHandle,
    TGLTessControlShaderHandle,
    TGLTessEvaluationShaderHandle);

type
  PProgramBinary = ^TProgramBinary;
  TProgramBinary = record
    ProgramName: string;
    ObjectNames: array of string;
    ObjectHashCode: array of Integer;
    BinarySize: Integer;
    BinaryData: Pointer;
    BinaryFormat: TGLEnum;
  end;

var
  ShaderObjects: array of TGLSLObject;
  ShaderPrograms: array of TGLSLProgram;
  ShaderManagerLog: TLogSession;
  BinaryList: TList;

{$IFDEF GLS_MULTITHREAD}
threadvar
{$ENDIF}
  vCurrentProgram: TGLSLProgram;

{$REGION 'Helper functions'}

function GetMaxGLSLVersion: AnsiString;
var
  version, profile: AnsiString;
  p: Integer;
begin
  version := GL.GetString(GL_SHADING_LANGUAGE_VERSION);
  Delete(version, 5, Length(version));
  p := Pos('.', string(version));
  Delete(version, p, 1);
  Delete(version, 4, Length(version));

  if CurrentGLContext.GLStates.ForwardContext then
    profile := ' core'
  else if GL.VERSION_3_0 then
    profile := ' compatibility'
  else
    profile := '';

  Result := '#version ' + version + profile + #10#13 +
    '#define VERSION ' + version + #10#13 +
    '#if (VERSION > 120)' + #10#13 +
    'precision highp float;' + #10#13 +
    '#define VERTin in' + #10#13 +
    '#define VERTout out' + #10#13 +
    '#define FRAGin in' + #10#13 +
    '#define TEXTURE_1D texture' + #10#13 +
    '#define TEXTURE_2D texture' + #10#13 +
    '#define TEXTURE_CUBE texture' + #10#13 +
    '#else' + #10#13 +
    '#extension GL_ARB_explicit_attrib_location: enable' + #10#13 +
    '#define VERTin attribute' + #10#13 +
    '#define VERTout varying' + #10#13 +
    '#define FRAGin varying' + #10#13 +
    '#define FragData0 gl_FragData[0]' + #10#13 +
    '#define FragData1 gl_FragData[1]' + #10#13 +
    '#define FragData2 gl_FragData[2]' + #10#13 +
    '#define FragData3 gl_FragData[3]' + #10#13 +
    '#define FragData4 gl_FragData[4]' + #10#13 +
    '#define FragData5 gl_FragData[5]' + #10#13 +
    '#define FragData6 gl_FragData[6]' + #10#13 +
    '#define FragData7 gl_FragData[7]' + #10#13 +
    '#define TEXTURE_1D texture1D' + #10#13 +
    '#define TEXTURE_2D texture2D' + #10#13 +
    '#define TEXTURE_CUBE textureCube' + #10#13 +
    '#endif' + #10#13;
end;

function MaskToGLSLType(AMask: TGLColorComponentMask): TGLSLDataType;
var
  count: Integer;
begin
  count := 0;
  if ccmWhite in AMask then
    Inc(count, 3)
  else
  begin
    if ccmRed in AMask then
      Inc(count);
    if ccmGreen in AMask then
      Inc(count);
    if ccmBlue in AMask then
      Inc(count);
  end;
  if ccmAlpha in AMask then
    Inc(count);
  if count = 1 then
    Result := GLSLType1F
  else if count = 2 then
    Result := GLSLType2F
  else if count = 3 then
    Result := GLSLType3F
  else if count = 4 then
    Result := GLSLType4F
  else
  begin
    Result := GLSLTypeUndefined;
    Assert(False);
  end;
end;

function GetGLSLTypeCast(const Arg: AnsiString; ArgType: TGLSLDataType; AMask: TGLColorComponentMask; CastType: TGLSLDataType): AnsiString;
var
  sMask: AnsiString;
  dif: Integer;
begin
  if AMask <> [] then
    CastType := MaskToGLSLType(AMask);
  Result := Arg;

  if CastType < ArgType then
  begin
    if AMask <> [] then
    begin
      sMask := '';
      if ccmWhite in AMask then
        sMask := sMask + 'rgb'
      else
      begin
        if ccmRed in AMask then
          sMask := sMask + 'r';
        if ccmGreen in AMask then
          sMask := sMask + 'g';
        if ccmBlue in AMask then
          sMask := sMask + 'b';
      end;
      if ccmAlpha in AMask then
        sMask := sMask + 'a';
      Result := Result + '.' + sMask;
    end
    else
    begin
      case CastType of
        GLSLType1F: Result := Result + '.r';
        GLSLType2F: Result := Result + '.rg';
        GLSLType3F: Result := Result + '.rgb';
        GLSLType4F: ;
      else
        Assert(False);
      end;
    end;
  end
  else if CastType > ArgType then
  begin
    dif := Integer(CastType) - Integer(ArgType);
    case dif of
      1: Result := AnsiString(Format('%s(%s, 0.0)', [cGLSLTypeString[CastType], Arg], cEnUsFormatSettings));
      2: Result := AnsiString(Format('%s(%s, 0.0, 0.0)', [cGLSLTypeString[CastType], Arg], cEnUsFormatSettings));
      3: Result := AnsiString(Format('%s(%s, 0.0, 0.0, 0.0)', [cGLSLTypeString[CastType], Arg], cEnUsFormatSettings));
    else
      Assert(False);
    end;
  end
  else
    Result := Arg;
end;

function GLSLPartToStr(APart: TGLSLProgramTypes): string;
begin
  if APart = [ptVertex] then
     Result := 'V'
  else if APart = [ptFragment] then
     Result := 'F'
  else if APart = [ptGeometry] then
     Result := 'G'
  else if APart = [ptControl] then
     Result := 'C'
  else if APart = [ptEvaluation] then
     Result := 'E'
  else
    Assert(False);
end;

function GLSLTypeToString(AType: TGLSLDataType): AnsiString;
begin
  Result := cGLSLTypeString[AType];
end;

function StrToGLSLType(const AType: AnsiString): TGLSLDataType;
begin
  for Result := Low(TGLSLDataType) to High(TGLSLDataType) do
  begin
    if cGLSLTypeString[Result] = AType then
    exit;
  end;
  Result := GLSLTypeUndefined;
end;

function GLSLTypeComponentCount(AType: TGLSLDataType): Integer;
begin
  Result := cGLSLTypeComponents[AType];
end;

function HexToGLSL(tp: TGLSLDataType; HexValue: string): AnsiString;
var
  v: TVectorEXT;
  i: TIntVectorEXT;
begin
  Result := '';
  case tp of
    GLSLTypeUndefined: ;
    GLSLTypeVoid: ;
    GLSLType1F:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@v.V[0]), SizeOf(TVectorEXT));
        Result := AnsiString(Format('%.7f', [v.X], cEnUsFormatSettings));
      end;
    GLSLType2F:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@v.V[0]), SizeOf(TVectorEXT));
        Result := AnsiString(Format('vec2(%.7f, %.7f)', [v.X, v.Y], cEnUsFormatSettings));
      end;
    GLSLType3F:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@v.V[0]), SizeOf(TVectorEXT));
        Result := AnsiString(Format('vec3(%.7f, %.7f, %.7f)', [v.X, v.Y, v.Z], cEnUsFormatSettings));
      end;
    GLSLType4F:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@v.V[0]), SizeOf(TVectorEXT));
        Result := AnsiString(Format('vec4(%.7f, %.7f, %.7f, %.7f)', [v.X, v.Y, v.Z, v.W], cEnUsFormatSettings));
      end;
    GLSLType1I:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@i.V[0]), SizeOf(TIntVectorEXT));
        Result := AnsiString(Format('%d', [i.X], cEnUsFormatSettings));
      end;
    GLSLType2I:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@i.V[0]), SizeOf(TIntVectorEXT));
        Result := AnsiString(Format('ivec2(%d, %d)', [v.X, v.Y], cEnUsFormatSettings));
      end;
    GLSLType3I:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@i.V[0]), SizeOf(TIntVectorEXT));
        Result := AnsiString(Format('ivec3(%d, %d, %d)', [v.X, v.Y, v.Z], cEnUsFormatSettings));
      end;
    GLSLType4I:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@i.V[0]), SizeOf(TIntVectorEXT));
        Result := AnsiString(Format('ivec4(%d, %d, %d, %d)', [v.X, v.Y, v.Z, v.W], cEnUsFormatSettings));
      end;
    GLSLType1UI: ;
    GLSLType2UI: ;
    GLSLType3UI: ;
    GLSLType4UI: ;
    GLSLTypeMat2F: ;
    GLSLTypeMat3F: ;
    GLSLTypeMat4F: ;
  else
    Assert(False);
  end;
end;

function RemoveGLSLQualifier(const InputLine: string): string;
var
  P: Integer;
begin
  Result := InputLine;
  while True do
  begin
    P := Pos('in ', Result);
    if P > 0 then
    begin
      Delete(Result, P, 3);
      continue;
    end;
    P := Pos('inout ', Result);
    if P > 0 then
    begin
      Delete(Result, P, 6);
      continue;
    end;
    P := Pos('out ', Result);
    if P > 0 then
    begin
      Delete(Result, P, 4);
      continue;
    end;
    break;
  end;
end;

{$ENDREGION}

function CompareProgram(const Item1, Item2: TGLProgramHandle): Integer;
begin
  if PtrUInt(Item1) < PtrUInt(Item2) then
    exit(-1)
  else if (Item1 = Item2) then
    exit(0)
  else
    exit(1);
end;

{$REGION 'Shader variables registry'}
var
  ProgramCache: array of TProgramCache;
  ProgramCachPosition: Integer;
  AttributeRegistry: array of TGLSLAttribute;
  UniformRegistry: array of TGLSLUniform;
  UniformBlockRegistry: array of TGLSLUniformBlock;

procedure ClearRegistries;
begin
  while Length(AttributeRegistry) > 0 do
    AttributeRegistry[High(AttributeRegistry)].Free;
  while Length(UniformRegistry) > 0 do
    UniformRegistry[High(UniformRegistry)].Free;
  while Length(UniformBlockRegistry) > 0 do
    UniformBlockRegistry[High(UniformBlockRegistry)].Free;
end;

procedure ClearProgramCache(AProg: TGLProgramHandle);
var
  I: Integer;
begin
  for I := 0 to High(AttributeRegistry) do
    AttributeRegistry[I].ClearCache(AProg);
  for I := 0 to High(UniformRegistry) do
    UniformRegistry[I].ClearCache(AProg);
  for I := 0 to High(UniformBlockRegistry) do
    UniformBlockRegistry[I].ClearCache(AProg);
end;

// TGLSLBaseVariable
//

constructor TGLSLBaseVariable.Create;
begin
  raise
    Exception.CreateFmt('Use %s.Register*** to create instance of class', [Self.ClassName]);
end;

destructor TGLSLBaseVariable.Destroy;
begin
  FreeAndNil(FProgramCacheTree);
end;

procedure TGLSLBaseVariable.DoCache(AProg: TGLProgramHandle; const ACache: TProgramCache);
var
  I: Integer;
begin
  if FProgramCacheTree.Find(AProg, I) then
  begin
    ProgramCache[I] := ACache;
    FLastProgram := AProg;
    FLastCacheIndex := I;
  end
  else
  begin
    ProgramCache[ProgramCachPosition] := ACache;
    FProgramCacheTree.Add(AProg, ProgramCachPosition);
    FLastProgram := AProg;
    FLastCacheIndex := ProgramCachPosition;
    Inc(ProgramCachPosition);
    if Length(ProgramCache) = ProgramCachPosition then
      SetLength(ProgramCache, 2 * Length(ProgramCache));
  end;
end;

procedure TGLSLBaseVariable.ClearCache(AProg: TGLProgramHandle);
var
  I: Integer;
begin
  if FProgramCacheTree.Find(AProg, I) then
  begin
    ProgramCache[I].Location := -1;
    ProgramCache[I].BindingIndex := GL_INVALID_INDEX;  // N/A
    ProgramCache[I].DataType := GLSLTypeUndefined;
    ProgramCache[I].WarningAbsenceLoged := False;
  end;
end;

procedure TGLSLBaseVariable.ResetLocationCache;
begin
  FProgramCacheTree.Clear;
end;

// TGLSLAttribute
//

constructor TGLSLAttribute.RegisterAttribute(const AName: string);
var
  a: TGLSLAttribute;
begin
  a := GetAttribute(AName);
  if Assigned(a) then
  begin
    Self.Destroy;
    Self := a;
    exit;
  end;
  SetLength(AttributeRegistry, Length(AttributeRegistry) + 1);
  AttributeRegistry[High(AttributeRegistry)] := Self;
  FName := AName;
  FProgramCacheTree := TProgramCacheTree.Create(CompareProgram, nil);
  FLastProgram := nil;
end;

procedure TGLSLAttribute.BeforeDestruction;
var
  I, J: Integer;
begin
  if Length(FName) = 0 then
    exit;
  for I := 0 to High(AttributeRegistry) do
  begin
    if AttributeRegistry[I].Name = FName then
    begin
      for J := I to High(AttributeRegistry) - 1 do
        AttributeRegistry[J] := AttributeRegistry[J + 1];
      SetLength(AttributeRegistry, High(AttributeRegistry));
      exit;
    end;
  end;
end;

class function TGLSLAttribute.GetAttribute(const AName: string): TGLSLAttribute;
var
  I: Integer;
begin
  for I := 0 to High(AttributeRegistry) do
  begin
    if AttributeRegistry[I].Name = AName then
    begin
      Result := AttributeRegistry[I];
      exit;
    end;
  end;
  Result := nil;
end;

function TGLSLAttribute.GetLocation: GLInt;
var
  I: Integer;
begin
  Result := -1;
  if not Assigned(vCurrentProgram) then
  begin
    ShaderManagerLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
    I := -1;

  if I > -1 then
  begin
    if (ProgramCache[I].Location = -1)
      and not ProgramCache[I].WarningAbsenceLoged then
    begin
      ShaderManagerLog.LogWarning(
        Format('Using an unknown attribute "%s" in program "%s"', [FName, string(vCurrentProgram.Name.Value)]));
      ProgramCache[I].WarningAbsenceLoged := True;
    end
    else
      Result := ProgramCache[I].Location;
    FLastProgram := vCurrentProgram.FHandle;
    FLastCacheIndex := I;
  end;
end;

procedure TGLSLAttribute.SetLocation(Index: GLInt);
var
  I: Integer;
begin
  if not Assigned(vCurrentProgram) then
  begin
    ShaderManagerLog.LogError(glsNoShader);
    Abort;
  end;

  if FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
  begin
    ProgramCache[I].Location := Index;
    ProgramCache[I].WarningAbsenceLoged := False;
    GL.BindAttribLocation(vCurrentProgram.FHandle.Handle, Index, PGLChar(TGLString(FName)));
  end;
end;

function TGLSLAttribute.GetDataType: TGLSLDataType;
var
  I: Integer;
begin
  Result := GLSLTypeUndefined;
  if not Assigned(vCurrentProgram) then
  begin
    ShaderManagerLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
    I := -1;

  if I > -1 then
  begin
    Result := ProgramCache[I].DataType;
    FLastProgram := vCurrentProgram.FHandle;
    FLastCacheIndex := I;
  end;
end;

// TGLSLUniform
//

constructor TGLSLUniform.RegisterUniform(const AName: string);
var
  u: TGLSLUniform;
begin
  u := GetUniform(AName);
  if Assigned(u) then
  begin
    Self.Destroy;
    Self := u;
    exit;
  end;
  SetLength(UniformRegistry, Length(UniformRegistry) + 1);
  UniformRegistry[High(UniformRegistry)] := Self;
  FName := AName;
  FProgramCacheTree := TProgramCacheTree.Create(CompareProgram, nil);
  FLastProgram := nil;
  FBlockOffset := -1;
end;

procedure TGLSLUniform.BeforeDestruction;
var
  I, J: Integer;
begin
  if Length(FName) = 0 then
    exit;
  for I := 0 to High(UniformRegistry) do
  begin
    if UniformRegistry[I].Name = FName then
    begin
      for J := I to High(UniformRegistry) - 1 do
        UniformRegistry[J] := UniformRegistry[J + 1];
      SetLength(UniformRegistry, High(UniformRegistry));
      exit;
    end;
  end;
end;

class function TGLSLUniform.GetUniform(const AName: string): TGLSLUniform;
var
  I: Integer;
begin
  for I := 0 to High(UniformRegistry) do
  begin
    if UniformRegistry[I].FName = AName then
    begin
      Result := UniformRegistry[I];
      exit;
    end;
  end;
  Result := nil;
end;

function TGLSLUniform.GetLocation: GLInt;
var
  I: Integer;
begin
  Result := -1;
  if not Assigned(vCurrentProgram) then
  begin
    ShaderManagerLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
    I := -1;

  if I > -1 then
  begin
    if (ProgramCache[I].Location = -1)
      and not ProgramCache[I].WarningAbsenceLoged then
    begin
      ShaderManagerLog.LogWarning(
        Format('Using an unknown uniform "%s" in program "%s"', [FName, string(vCurrentProgram.Name.Value)]));
      ProgramCache[I].WarningAbsenceLoged := True;
    end
    else
      Result := ProgramCache[I].Location;
    FLastProgram := vCurrentProgram.FHandle;
    FLastCacheIndex := I;
  end;
end;

function TGLSLUniform.GetDataType: TGLSLDataType;
var
  I: Integer;
begin
  Result := GLSLTypeUndefined;
  if not Assigned(vCurrentProgram) then
  begin
    ShaderManagerLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
    I := -1;

  if I > -1 then
  begin
    Result := ProgramCache[I].DataType;
    FLastProgram := vCurrentProgram.FHandle;
    FLastCacheIndex := I;
  end;
end;

// TGLSLUniformBlock
//

constructor TGLSLUniformBlock.RegisterUniformBlock(const AName: string);
var
  ub: TGLSLUniformBlock;
begin
  ub := GetUniformBlock(AName);
  if Assigned(ub) then
  begin
    Self.Destroy;
    Self := ub;
    exit;
  end;
  SetLength(UniformBlockRegistry, Length(UniformBlockRegistry) + 1);
  UniformBlockRegistry[High(UniformBlockRegistry)] := Self;
  FName := AName;
  FUniforms := TList.Create;
  FProgramCacheTree := TProgramCacheTree.Create(CompareProgram, nil);
  FLastProgram := nil;
end;

procedure TGLSLUniformBlock.BeforeDestruction;
var
  I, J: Integer;
begin
  if Length(FName) = 0 then
    exit;
  for I := 0 to High(UniformBlockRegistry) do
  begin
    if UniformBlockRegistry[I].Name = FName then
    begin
      for J := I to High(UniformBlockRegistry) - 1 do
        UniformBlockRegistry[J] := UniformBlockRegistry[J + 1];
      SetLength(UniformBlockRegistry, High(UniformBlockRegistry));
      FUniforms.Free;
      exit;
    end;
  end;
end;

class function TGLSLUniformBlock.GetUniformBlock(const AName: string):
  TGLSLUniformBlock;
var
  I: Integer;
begin
  for I := 0 to High(UniformBlockRegistry) do
  begin
    if UniformBlockRegistry[I].Name = AName then
    begin
      Result := UniformBlockRegistry[I];
      exit;
    end;
  end;
  Result := nil;
end;

function TGLSLUniformBlock.GetLocation: GLInt;
var
  I: Integer;
begin
  Result := -1;
  if not Assigned(vCurrentProgram) then
  begin
    ShaderManagerLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
    I := -1;

  if I > -1 then
  begin
    if (ProgramCache[I].Location = -1)
      and not ProgramCache[I].WarningAbsenceLoged then
    begin
      ShaderManagerLog.LogWarning(
        Format('Using an unknown uniform block "%s" in program "%s"', [FName, string(vCurrentProgram.Name.Value)]));
      ProgramCache[I].WarningAbsenceLoged := True;
    end
    else
      Result := ProgramCache[I].Location;
    FLastProgram := vCurrentProgram.FHandle;
    FLastCacheIndex := I;
  end;
end;


function TGLSLUniformBlock.GetBindingIndex: TGLuint;
var
  I: Integer;
begin
  Result := 0;
  if not Assigned(vCurrentProgram) then
  begin
    ShaderManagerLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
    I := -1;

  if I > -1 then
  begin
    Result := ProgramCache[I].BindingIndex;
    FLastProgram := vCurrentProgram.FHandle;
    FLastCacheIndex := I;
  end;
end;

procedure TGLSLUniformBlock.SetBindingIndex(Value: TGLuint);
var
  I: Integer;
begin
  if not Assigned(vCurrentProgram) then
  begin
    ShaderManagerLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
    I := -1;

  if I > -1 then
  begin
    if ProgramCache[I].Location = -1 then
    begin
      if not ProgramCache[I].WarningAbsenceLoged then
      begin
        ShaderManagerLog.LogWarning(
          Format('Using an unknown uniform buffer "%s" in program "%s"', [FName, string(vCurrentProgram.Name.Value)]));
        ProgramCache[I].WarningAbsenceLoged := True;
      end;
    end
    else
    begin
      if ProgramCache[I].BindingIndex <> Value then
      begin
        ProgramCache[I].BindingIndex := Value;
        GL.UniformBlockBinding(vCurrentProgram.FHandle.Handle, ProgramCache[I].Location, Value);
      end;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TGLSLObject'}
// ------------------
// ------------------ TGLSLObject ------------------
// ------------------

function TGLSLObjectName.GetInheritorClass: TGLAbstractNameClass;
begin
  Result := TGLAbstractNameClass(ClassType);
end;

function TGLSLObjectName.GetManager: TGLSAbstractManagerClass;
begin
  Result := ShaderManager;
end;

constructor TGLSLObject.Create;
var
  P: TGLSLProgramType;
begin
  inherited;
  for P := Low(TGLSLProgramType) to High(TGLSLProgramType) do
    FHandles[P] := cObjectClass[P].Create;
  FName := TGLSLObjectName.Create;
  FName._AddRef;
end;

destructor TGLSLObject.Destroy;
var
  P: TGLSLProgramType;
begin
  inherited;
  for P := low(TGLSLProgramType) to high(TGLSLProgramType) do
    FHandles[P].Destroy;
  FName._Release;
end;

function TGLSLObject.Compile: Boolean;
var
  P: TGLSLProgramType;
  val, len: Integer;
  pLog: PAnsiChar;
  logstr: string;
  OK: Boolean;
begin
  Result := True;
  pLog := nil;

  for P := Low(TGLSLProgramType) to High(TGLSLProgramType) do
    if P in FTypes then
    begin
      logstr := Format('%s shader object "%s" compilation - ', [cObjectTypeName[P], string(Name.Value)]);

      if not cObjectClass[P].IsSupported then
      begin
        ShaderManagerLog.LogWarning(logstr + 'Discarded. Shader not supported.');
        continue;
      end;

      // Allocate handle associated with current context
      FHandles[P].AllocateHandle;
      if not FHandles[P].IsDataNeedUpdate then
        continue;
      // Compile object
      FHandles[P].ShaderSource(FCode);
      OK := FHandles[P].CompileShader;
      Result := Result and OK;

      if OK then
      begin
        ShaderManagerLog.LogInfo(logstr + ' Successful');
        FHandles[P].NotifyDataUpdated;
      end
      else
        ShaderManagerLog.LogError(logstr + ' Failed!');

      GL.GetShaderiv(FHandles[P].Handle, GL_INFO_LOG_LENGTH, @val);
      if val > 1 then
      begin
        if not OK then
        begin
          ShaderManagerLog.LogNotice('');
          ShaderManagerLog.LogInfo(string(FCode));
        end;
        ShaderManagerLog.LogNotice('');
        ReallocMem(pLog, val);
        GL.GetShaderInfoLog(FHandles[P].Handle, val, @len, pLog);
        ShaderManagerLog.LogInfo(string(pLog));
      end;
    end
    else
      FHandles[P].DestroyHandle;

  if pLog <> nil then
    FreeMem(pLog);
end;

{$ENDREGION}

{$REGION 'TGLSLProgram'}
// ------------------
// ------------------ TGLSLProgram ------------------
// ------------------

function TGLSLProgramName.GetInheritorClass: TGLAbstractNameClass;
begin
  Result := TGLAbstractNameClass(ClassType);
end;

function TGLSLProgramName.GetManager: TGLSAbstractManagerClass;
begin
  Result := ShaderManager;
end;

constructor TGLSLProgram.Create;
begin
  FHandle := TGLProgramHandle.Create;
  FAttachedObjects := TGLSLObjectList.Create;
  FName := TGLSLProgramName.Create;
  FName._AddRef;
end;

destructor TGLSLProgram.Destroy;
begin
  inherited;
  FHandle.Destroy;
  FAttachedObjects.Destroy;
  FName._Release;
end;

function TGLSLProgram.FindBinary: Boolean;
var
  I, J: Integer;
  pb: PProgramBinary;
  linked: GLint;
  val, len: GLsizei;
  pLog: PAnsiChar;
  logstr: string;

  procedure ProgramLog;
  begin
    GL.GetProgramiv(FHandle.Handle, GL_INFO_LOG_LENGTH, @val);
    if val > 0 then
    begin
      GetMem(pLog, val);
      GL.GetProgramInfoLog(FHandle.Handle, val, @len, pLog);
      ShaderManagerLog.LogInfo(string(pLog));
      FreeMem(pLog, val);
    end;
  end;

begin
  if not IsDesignTime
    and GL.ARB_get_program_binary
    and Assigned(BinaryList) then
    for I := 0 to BinaryList.Count - 1 do
    begin
      pb := BinaryList[I];
      if (Length(pb.ObjectNames) = FAttachedObjects.Count)
        and (pb.ProgramName = Name.GetValue) then
      begin
        for J := FAttachedObjects.Count - 1 downto 0 do
        begin
          if not ((pb.ObjectHashCode[J] = FAttachedObjects[J].FHashCode)
            and (pb.ObjectNames[J] = FAttachedObjects[J].FName.GetValue)) then
            exit(False);
        end;
        GL.ProgramBinary(FHandle.Handle, pb.BinaryFormat, pb.BinaryData, pb.BinarySize);
        GL.GetProgramiv(FHandle.Handle, GL_LINK_STATUS, @linked);
        Result := linked <> 0;
        logstr := Format('Shader Program Binary "%s" Linking - ', [Name.Value]);
        if Result then
        begin
          ShaderManagerLog.LogInfo(logstr + 'Successful');
          ProgramLog;
          FHandle.NotifyDataUpdated;
        end
        else
        begin
          ShaderManagerLog.LogInfo(logstr + 'Failed');
          ProgramLog;
        end;
        exit;
      end;
    end;
  Result := false;
end;

function TGLSLProgram.Link: Boolean;
var
  P: TGLSLProgramType;
  obj: TGLSLObject;
  I: Integer;
  val, len: GLsizei;
  pLog: PAnsiChar;
  logstr: string;
begin
  if FLinkFailureCount > GLS_MAX_PROGRAM_FAIL then
    Abort;

  FHandle.AllocateHandle;

  Result := FindBinary;
  if not Result then
  begin
    for I := 0 to FAttachedObjects.Count - 1 do
    begin
      obj := FAttachedObjects.Items[I];
      for P := low(TGLSLProgramType) to high(TGLSLProgramType) do
      begin
        if (P in obj.FTypes)
          and (P in FProgramMask)
          and obj.FHandles[P].IsSupported then
        begin
          if obj.FHandles[P].IsDataNeedUpdate then
            if not obj.Compile then
              exit;
          GL.AttachShader(FHandle.Handle, obj.FHandles[P].Handle);
          ShaderManagerLog.LogDebug(Format('%s object "%s" attached to "%s"', [LowerCase(cObjectTypeName[P]), string(obj.Name.Value), string(Self.Name.Value)]));
        end;
      end;
    end;

    if not IsDesignTime and GL.ARB_get_program_binary then
      GL.ProgramParameteri(FHandle.Handle, GL_PROGRAM_BINARY_RETRIEVABLE_HINT, GL_TRUE);

    Result := FHandle.LinkProgram;
    FHandle.DetachAllObject;

    logstr := Format('Shader Program "%s" Linking - ', [Name.Value]);
    if Result then
    begin
      ShaderManagerLog.LogInfo(logstr + ' Successful');
      FHandle.NotifyDataUpdated;
      StoreBinary;
    end
    else
    begin
      ShaderManagerLog.LogError(logstr + ' Failed!');
      for I := 0 to FAttachedObjects.Count - 1 do
      begin
        obj := FAttachedObjects.Items[I];
        for P := low(TGLSLProgramType) to high(TGLSLProgramType) do
        if (P in obj.FTypes)
          and (P in FProgramMask)
          and obj.FHandles[P].IsSupported then
          begin
            SaveAnsiStringToFile(obj.FName.GetValue+'.'+cObjectTypeName[P], obj.FCode);
          end;
        end;
    end;

    GL.GetProgramiv(FHandle.Handle, GL_INFO_LOG_LENGTH, @val);
    if val > 1 then
    begin
      GetMem(pLog, val);
      GL.GetProgramInfoLog(FHandle.Handle, val, @len, pLog);
      ShaderManagerLog.LogInfo(string(pLog));
      FreeMem(pLog, val);
      Inc(FLinkFailureCount)
    end;
  end;

  // Notify VBO managers about program changes
  StaticVBOManager.NotifyProgramChanged(FHandle);
  StreamVBOManager.NotifyProgramChanged(FHandle);
  ClearProgramCache(FHandle);
end;

procedure TGLSLProgram.StoreBinary;
var
  pb: PProgramBinary;
  I: Integer;
  val: GLsizei;
begin
  if not IsDesignTime and GL.ARB_get_program_binary then
  begin
    GL.GetProgramiv(FHandle.Handle, GL_PROGRAM_BINARY_LENGTH, @val);
    if val > 0 then
    begin
      if BinaryList = nil then
        BinaryList := TList.Create;

      New(pb);
      pb.ProgramName := Name.GetValue;
      SetLength(pb.ObjectNames, FAttachedObjects.Count);
      SetLength(pb.ObjectHashCode, FAttachedObjects.Count);
      for I := FAttachedObjects.Count - 1 downto 0 do
      begin
        pb.ObjectNames[I] := FAttachedObjects[I].FName.GetValue;
        pb.ObjectHashCode[I] := FAttachedObjects[I].FHashCode;
      end;
      GetMem(pb.BinaryData, val);
      GL.GetProgramBinary(FHandle.Handle, val, nil, @pb.BinaryFormat, pb.BinaryData);
      pb.BinarySize := val;
      BinaryList.Add(pb);
    end;
  end;
end;

procedure TGLSLProgram.Attach(AObject: TGLSLObject);
begin
  if FAttachedObjects.IndexOf(AObject) < 0 then
  begin
    FAttachedObjects.Add(AObject);
    FHandle.NotifyChangesOfData;
  end;
end;

procedure TGLSLProgram.Detach(AObject: TGLSLObject);
begin
  if FAttachedObjects.Remove(AObject) >= 0 then
    FHandle.NotifyChangesOfData;
end;

{$ENDREGION}

{$REGION 'ShadersManager'}
// ------------------
// ------------------ ShadersManager ------------------
// ------------------

const
  cBinariesCacheFile = 'ShaderBinaries.cache';

class procedure ShaderManager.Initialize;
var
  LogPath: string;
begin
  RegisterGLSceneManager(ShaderManager);

  LogPath := ExtractFilePath(ParamStr(0));
  ShaderManagerLog
    := TLogSession.Init(LogPath + 'ShaderManagerLog.log', lfNone,
    [lkNotice, lkInfo, lkWarning, lkError, lkDebug]);
  // Load binary cache
  NotifyProjectOpened;
end;

class procedure ShaderManager.Finalize;
begin
  BeginWork;
  // Binary cache in design time is very dangerous accidents in the driver
  if not IsDesignTime then
    SaveBinaryCache(True);
  ClearShaderPrograms;
  ClearShaderObject;
  EndWork;

  ShaderManagerLog.Shutdown;
end;

class procedure ShaderManager.NotifyContextCreated;
begin
end;

class procedure ShaderManager.NotifyBeforeCompile;
begin
  SaveBinaryCache(False);
end;

class function ShaderManager.FillResourceList(AList: TStringList): Boolean;
begin
  Result := False;
end;

class function ShaderManager.GetObjectName(const AName: string): IGLName;
var
  I, N, H: Integer;
begin
  CheckCall;

  N := Length(AName);
  H := N;
  for I := 1 to N do
    H := (H shl 1) + Integer(AName[i]);

  for I := 0 to High(ShaderObjects) do
  begin
    if Assigned(ShaderObjects[I]) then
    begin
      if (ShaderObjects[I].FName.HashCode = H)
        and (ShaderObjects[I].FName.Value = AName) then
        exit(ShaderObjects[I].FName);
    end;
  end;
  Result := nil;
end;

class function ShaderManager.GetProgramName(const AName: string): IGLName;
var
  I, N, H: Integer;
begin
  CheckCall;

  N := Length(AName);
  H := N;
  for I := 1 to N do
    H := (H shl 1) + Integer(AName[i]);

  for I := 0 to High(ShaderPrograms) do
  begin
    if Assigned(ShaderPrograms[I]) then
    begin
      if (ShaderPrograms[I].FName.HashCode = H)
        and (ShaderPrograms[I].FName.Value = AName) then
        exit(ShaderPrograms[I].FName);
    end;
  end;
  Result := nil;
end;

class procedure ShaderManager.MakeUniqueItemName(var AName: string; AClass: TGLAbstractNameClass);
const
  cObjName = 'ShaderObj';
  cProgName = 'ShaderProg';
var
  N: Integer;
  rName: string;
begin
  CheckCall;

  if AClass = TGLSLObjectName then
  begin
    if Length(AName) = 0 then
      rName := cObjName
    else
      rName := AName;
    N := 1;
    while GetObjectName(rName) <> nil do
    begin
      rName := AName + IntToHex(N, 2);
      Inc(N);
    end;
    AName := rName;
  end
  else if AClass = TGLSLProgramName then
  begin
    if Length(AName) = 0 then
      rName := cProgName
    else
      rName := AName;
    N := 1;
    while GetProgramName(rName) <> nil do
    begin
      rName := AName + IntToHex(N, 2);
      Inc(N);
    end;
    AName := rName;
  end;
end;

class procedure ShaderManager.NotifyProjectOpened;
var
  pb: PProgramBinary;
  I, J, Temp: Integer;
  stream: TStream;
begin
  // Clear old binary cache
  if Assigned(BinaryList) then
  begin
    for I := BinaryList.Count - 1 to 0 do
    begin
      pb := BinaryList[I];
      FreeMem(pb.BinaryData, pb.BinarySize);
      Dispose(pb);
    end;
    FreeAndNil(BinaryList);
  end;

  // Load binary cache of opened project
  if FileStreamExists(cBinariesCacheFile) then
  begin
    BinaryList := TList.Create;
    stream := CreateFileStream(cBinariesCacheFile, fmOpenRead);
    try
      stream.Read(Temp, SizeOf(Integer)); // Version
      if Temp > 0 then
      begin
        GLSLogger.LogError(Format(glsUnknownArchive, [cBinariesCacheFile, Temp]));
        Abort;
      end;
      stream.Read(Temp, SizeOf(Integer));
      for I := 0 to Temp - 1 do
        with stream do
        begin
          New(pb);
          BinaryList.Add(pb);
          Read(Temp, SizeOf(Integer));
          SetLength(pb.ProgramName, Temp);
          Read(pb.ProgramName[1], Temp * SizeOf(Char));
          Read(Temp, SizeOf(Integer));
          SetLength(pb.ObjectNames, Temp);
          SetLength(pb.ObjectHashCode, Temp);
          for J := Temp - 1 downto 0 do
          begin
            Read(Temp, SizeOf(Integer));
            SetLength(pb.ObjectNames[J], Temp);
            Read(pb.ObjectNames[J][1], Temp * SizeOf(Char));
            Read(pb.ObjectHashCode[J], SizeOf(Integer));
          end;
          Read(pb.BinaryFormat, SizeOf(TGLEnum));
          Read(pb.BinarySize, SizeOf(Integer));
          GetMem(pb.BinaryData, pb.BinarySize);
          Read(pb.BinaryData^, pb.BinarySize);
        end;
    finally
      stream.Destroy;
    end;
  end;
end;

class procedure ShaderManager.SaveBinaryCache(ADoFree: Boolean);
var
  pb: PProgramBinary;
  I, J, Count, Temp: Integer;
  stream: TStream;
begin
  if Assigned(BinaryList) then
  begin
    stream := CreateFileStream(cBinariesCacheFile, fmCreate);
    with stream do
      try
        Temp := 0;
        Write(Temp, SizeOf(Integer)); // version, just in case
        Temp := BinaryList.Count;
        Write(Temp, SizeOf(Integer));
        for I := 0 to BinaryList.Count - 1 do
        begin
          pb := BinaryList[I];
          with stream do
          begin
            Temp := Length(pb.ProgramName);
            Write(Temp, SizeOf(Integer));
            Write(pb.ProgramName[1], Temp * SizeOf(Char));
            Count := Length(pb.ObjectNames);
            Write(Count, SizeOf(Integer));
            for J := Count - 1 downto 0 do
            begin
              Temp := Length(pb.ObjectNames[J]);
              Write(Temp, SizeOf(Integer));
              Write(pb.ObjectNames[J][1], Temp * SizeOf(Char));
              Write(pb.ObjectHashCode[J], SizeOf(Integer));
            end;
            Write(pb.BinaryFormat, SizeOf(TGLEnum));
            Write(pb.BinarySize, SizeOf(Integer));
            Write(pb.BinaryData^, pb.BinarySize);
          end;
          if ADoFree then
          begin
            FreeMem(pb.BinaryData, pb.BinarySize);
            Dispose(pb);
          end;
        end;
      finally
        stream.Destroy;
        if ADoFree then
          FreeAndNil(BinaryList);
      end;
  end;
end;

class procedure ShaderManager.NotifyProjectClosed;
begin
  SaveBinaryCache(True);
end;

class function ShaderManager.GetTextureTarget(const AUniform: TGLSLUniform;
  out ATarget: TGLTextureTarget): Boolean;
const
  cGLSLTypeToTexTarget: array[GLSLTypeSampler1D..GLSLTypeUIntSamplerBuffer]
    of TGLTextureTarget = (ttTexture1D, ttTexture2D, ttTexture3D,
    ttTextureCube,
    ttTexture1D, ttTexture2D, ttTexture1DArray, ttTexture2DArray,
    ttTexture1DArray, ttTexture2DArray, ttTextureCube,
    ttTexture1D, ttTexture2D, ttTexture3D, ttTextureCube,
    ttTexture1DArray, ttTexture2DArray,
    ttTexture1D, ttTexture2D, ttTexture3D, ttTextureCube,
    ttTexture1DArray, ttTexture2DArray, ttTextureRect,
    ttTextureRect, ttTextureBuffer, ttTextureRect,
    ttTextureBuffer, ttTextureRect, ttTextureBuffer);
var
  dt: TGLSLDataType;
begin
  dt := AUniform.DataType;
  Result := dt >= GLSLTypeSampler1D;
  if not Result then
    ShaderManagerLog.LogError('Uniform "' + AUniform.Name + '" has not sampler type')
  else
    ATarget := cGLSLTypeToTexTarget[dt];
end;

class function ShaderManager.GetObject(const AName: IGLName): TGLSLObject;
var
  I: Integer;
begin
  if Assigned(AName) and (AName.GetInheritorClass = TGLSLObjectName) then
  begin
    I := AName.GetIndex;
    if I > -1 then
      exit(ShaderObjects[I]);
  end;
  Result := nil;
end;

class function ShaderManager.GetProgram(const AName: IGLName): TGLSLProgram;
var
  I: Integer;
begin
  if Assigned(AName) and (AName.GetInheritorClass = TGLSLProgramName) then
  begin
    I := AName.GetIndex;
    if I > -1 then
      exit(ShaderPrograms[I]);
  end;
  Result := nil;
end;

class function ShaderManager.CurrentProgram: TGLProgramHandle;
begin
  if Assigned(vCurrentProgram) then
    Result := vCurrentProgram.FHandle
  else
    Result := nil;
end;

class procedure ShaderManager.PushObject(AObject: TGLSLObject);
var
  I: Integer;
begin
  SetLength(ShaderObjects, Length(ShaderObjects) + 1);
  I := High(ShaderObjects);
  AObject.Name.SetIndex(I);
  ShaderObjects[I] := AObject;
end;

class procedure ShaderManager.PushProgram(AProgram: TGLSLProgram);
var
  I: Integer;
begin
  SetLength(ShaderPrograms, Length(ShaderPrograms) + 1);
  I := High(ShaderPrograms);
  AProgram.Name.SetIndex(I);
  ShaderPrograms[I] := AProgram;
end;

class procedure ShaderManager.DefineShaderObject(
  var AName: IGLName;
  const Code: AnsiString;
  ATypes: TGLSLProgramTypes;
  ANameRoot: string);
var
  Obj: TGLSLObject;
  newCode: Boolean;
  P: TGLSLProgramType;
  N, I: Integer;
begin
  CheckCall;

  Obj := GetObject(AName);
  if not Assigned(Obj) then
  begin
    with Obj do
    begin
      Obj := TGLSLObject.Create;
      Name.Value := ANameRoot;
      PushObject(Obj);
      AName := Name;
      newCode := True;
    end;
  end
  else
    newCode := (Obj.FTypes <> ATypes) or (Obj.FCode <> Code);

  if newCode then
    with Obj do
    begin
      FTypes := ATypes;
      FCode := Code;

      if GL.ARB_get_program_binary then
      begin
        N := Length(FCode);
        FHashCode := N;
        for I := 1 to N do
          FHashCode := (FHashCode shl 1) + Integer(FCode[i]);
      end;

      for P := Low(TGLSLProgramType) to High(TGLSLProgramType) do
        FHandles[P].NotifyChangesOfData;
      Compile;
    end;
end;

class procedure ShaderManager.DefineShaderProgram(
  var AName: IGLName;
  AProgramMask: TGLSLProgramTypes;
  ANameRoot: string);
var
  Prog: TGLSLProgram;
begin
  CheckCall;

  Prog := GetProgram(AName);
  if not Assigned(Prog) then
  begin
    Prog := TGLSLProgram.Create;
    Prog.Name.Value := ANameRoot;
    AName := Prog.Name;
    PushProgram(Prog);
  end
  else
  begin
    Prog.FHandle.DestroyHandle;
    Prog.FAttachedObjects.Clear;
    Prog.FLinkFailureCount := 0;
  end;
  Prog.FProgramMask := AProgramMask;
end;

class procedure ShaderManager.AttachShaderObjectToProgram(
  const AObject: IGLName; const AProgram: IGLName);
var
  obj: TGLSLObject;
  Prog: TGLSLProgram;
begin
  CheckCall;
  Prog := GetProgram(AProgram);
  obj := GetObject(AObject);
  if Assigned(obj) then
    Prog.Attach(obj);
end;

class procedure ShaderManager.DetachShaderObjectFromProgram(
  const AObject: IGLName; const AProgram: IGLName);
var
  obj: TGLSLObject;
  Prog: TGLSLProgram;
begin
  CheckCall;
  obj := GetObject(AObject);
  Prog := GetProgram(AProgram);
  Prog.Detach(obj);
end;

class procedure ShaderManager.MakeLocationCache(AProgram: TGLSLProgram);
var
  I, J: Integer;
  ProgramID: GLuint;
  buff: array[0..255] of AnsiChar;
  indices: array[0..255] of GLInt;
  offsets: array[0..255] of GLInt;
  max: GLInt;
  Size: GLInt;
  len: GLsizei;
  AType: GLenum;
  Attr: TGLSLAttribute;
  Uniform: TGLSLUniform;
  UniformBlock: TGLSLUniformBlock;
  vCache: TProgramCache;
begin
  CheckCall;
  ProgramID := AProgram.FHandle.Handle;
  // Get all active atttributes
  GL.GetProgramiv(ProgramID, GL_ACTIVE_ATTRIBUTES, @max);
  for I := 0 to max - 1 do
  begin
    GL.GetActiveAttrib(
      ProgramID,
      I,
      Length(buff),
      @len,
      @Size,
      @AType,
      @buff[0]);

    Attr := TGLSLAttribute.GetAttribute(Copy(string(buff), 0, len));
    if Assigned(Attr) then
    begin
      with Attr do
      begin
        vCache.Location := GL.GetAttribLocation(ProgramID, PGLChar(TGLString(Name)));
        case AType of
          GL_FLOAT: vCache.DataType := GLSLType1F;
          GL_FLOAT_VEC2: vCache.DataType := GLSLType2F;
          GL_FLOAT_VEC3: vCache.DataType := GLSLType3F;
          GL_FLOAT_VEC4: vCache.DataType := GLSLType4F;
          GL_INT: vCache.DataType := GLSLType1I;
          GL_INT_VEC2: vCache.DataType := GLSLType2I;
          GL_INT_VEC3: vCache.DataType := GLSLType3I;
          GL_INT_VEC4: vCache.DataType := GLSLType4I;
          GL_UNSIGNED_INT: vCache.DataType := GLSLType1UI;
          GL_UNSIGNED_INT_VEC2: vCache.DataType := GLSLType2UI;
          GL_UNSIGNED_INT_VEC3: vCache.DataType := GLSLType3UI;
          GL_UNSIGNED_INT_VEC4: vCache.DataType := GLSLType4UI;
          GL_BOOL: vCache.DataType := GLSLType1I;
          GL_BOOL_VEC2: vCache.DataType := GLSLType2I;
          GL_BOOL_VEC3: vCache.DataType := GLSLType3I;
          GL_BOOL_VEC4: vCache.DataType := GLSLType4I;
          GL_FLOAT_MAT2: vCache.DataType := GLSLTypeMat2F;
          GL_FLOAT_MAT3: vCache.DataType := GLSLTypeMat3F;
          GL_FLOAT_MAT4: vCache.DataType := GLSLTypeMat4F;
        end;
        ShaderManagerLog.LogInfo(Format('Detected active attribute: %s %s', [cGLSLTypeString[vCache.DataType], Name]));
      end;
      vCache.WarningAbsenceLoged := False;
      Attr.DoCache(AProgram.FHandle, vCache);
    end
    else
      ShaderManagerLog.LogWarning('Active attribute ' + Copy(string(buff), 0, len)
        + ' not registered');
  end;

  if GL.ARB_uniform_buffer_object then
  begin
    GL.GetProgramiv(ProgramID, GL_ACTIVE_UNIFORM_BLOCKS, @max);
    for I := 0 to max - 1 do
    begin
      GL.GetActiveUniformBlockName(
        ProgramID,
        I,
        Length(buff),
        @len,
        @buff[0]);
      if GL.GetError <> GL_NO_ERROR then
        continue;
      UniformBlock :=
        TGLSLUniformBlock.GetUniformBlock(Copy(string(buff), 0, len));

      if Assigned(UniformBlock) then
      begin
        with UniformBlock do
        begin
          ShaderManagerLog.LogInfo('Detected active uniform block: ' + Name);
          vCache.Location :=
            GL.GetUniformBlockIndex(ProgramID, PGLChar(TGLString(Name)));
          GL.GetActiveUniformBlockiv(ProgramID, vCache.Location,
            GL_UNIFORM_BLOCK_DATA_SIZE, @FSize);
          GL.GetActiveUniformBlockiv(ProgramID, vCache.Location,
            GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS, @Size);
          GL.GetActiveUniformBlockiv(ProgramID, vCache.Location,
            GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES, @indices[0]);
          GL.GetActiveUniformsiv(ProgramID, Size, @indices[0],
            GL_UNIFORM_OFFSET, @offsets[0]);
          for J := 0 to Size - 1 do
          begin
            GL.GetActiveUniform(
              ProgramID,
              indices[J],
              Length(buff),
              @len,
              @Size,
              @AType,
              @buff[0]);
            Uniform := TGLSLUniform.RegisterUniform(Copy(string(buff), 0, len));
            if FUniforms.IndexOf(Uniform) < 0 then
              FUniforms.Add(Uniform);
            Uniform.FBlockOffset := offsets[J];
          end;
        end;
        vCache.WarningAbsenceLoged := False;
        vCache.BindingIndex := GL_INVALID_INDEX;
        UniformBlock.DoCache(AProgram.FHandle, vCache);
      end
      else
      begin
        ShaderManagerLog.LogError('Active uniform block: ' +
          Copy(string(buff), 0, len) + ' not registered');
      end;
    end;
  end;
  //  GL.CheckError;
    // Get all active uniform
  GL.GetProgramiv(ProgramID, GL_ACTIVE_UNIFORMS, @max);

  for I := 0 to max - 1 do
  begin
    GL.GetActiveUniform(
      ProgramID,
      TGLuint(I),
      Length(buff),
      @len,
      @Size,
      @AType,
      @buff[0]);
    Uniform := TGLSLUniform.GetUniform(Copy(string(buff), 0, len));

    if Assigned(Uniform) then
    begin
      with Uniform do
      begin
        vCache.Location := GL.GetUniformLocation(ProgramID, PGLChar(TGLString(Name)));
        case AType of
          GL_FLOAT: vCache.DataType := GLSLType1F;
          GL_FLOAT_VEC2: vCache.DataType := GLSLType2F;
          GL_FLOAT_VEC3: vCache.DataType := GLSLType3F;
          GL_FLOAT_VEC4: vCache.DataType := GLSLType4F;
          GL_INT: vCache.DataType := GLSLType1I;
          GL_INT_VEC2: vCache.DataType := GLSLType2I;
          GL_INT_VEC3: vCache.DataType := GLSLType3I;
          GL_INT_VEC4: vCache.DataType := GLSLType4I;
          GL_UNSIGNED_INT: vCache.DataType := GLSLType1UI;
          GL_UNSIGNED_INT_VEC2: vCache.DataType := GLSLType2UI;
          GL_UNSIGNED_INT_VEC3: vCache.DataType := GLSLType3UI;
          GL_UNSIGNED_INT_VEC4: vCache.DataType := GLSLType4UI;
          GL_BOOL: vCache.DataType := GLSLType1I;
          GL_BOOL_VEC2: vCache.DataType := GLSLType2I;
          GL_BOOL_VEC3: vCache.DataType := GLSLType3I;
          GL_BOOL_VEC4: vCache.DataType := GLSLType4I;
          GL_FLOAT_MAT2: vCache.DataType := GLSLTypeMat2F;
          GL_FLOAT_MAT3: vCache.DataType := GLSLTypeMat3F;
          GL_FLOAT_MAT4: vCache.DataType := GLSLTypeMat4F;
          GL_SAMPLER_1D: vCache.DataType := GLSLTypeSampler1D;
          GL_SAMPLER_2D: vCache.DataType := GLSLTypeSampler2D;
          GL_SAMPLER_3D: vCache.DataType := GLSLTypeSampler3D;
          GL_SAMPLER_CUBE: vCache.DataType := GLSLTypeSamplerCube;
          GL_SAMPLER_1D_SHADOW: vCache.DataType := GLSLTypeSampler1DShadow;
          GL_SAMPLER_2D_SHADOW: vCache.DataType := GLSLTypeSampler2DShadow;
          GL_SAMPLER_2D_RECT: vCache.DataType := GLSLTypeSamplerRect;
          GL_SAMPLER_2D_RECT_SHADOW: vCache.DataType :=
            GLSLTypeSamplerRectShadow;
          GL_SAMPLER_BUFFER: vCache.DataType := GLSLTypeSamplerBuffer;
          GL_INT_SAMPLER_2D_RECT: vCache.DataType :=
            GLSLTypeIntSamplerRect;
          GL_INT_SAMPLER_BUFFER: vCache.DataType :=
            GLSLTypeIntSamplerBuffer;
          GL_UNSIGNED_INT_SAMPLER_1D: vCache.DataType :=
            GLSLTypeUIntSampler1D;
          GL_UNSIGNED_INT_SAMPLER_2D: vCache.DataType :=
            GLSLTypeUIntSampler2D;
          GL_UNSIGNED_INT_SAMPLER_3D: vCache.DataType :=
            GLSLTypeUIntSampler3D;
          GL_UNSIGNED_INT_SAMPLER_CUBE: vCache.DataType :=
            GLSLTypeUIntSamplerCube;
          GL_UNSIGNED_INT_SAMPLER_1D_ARRAY: vCache.DataType :=
            GLSLTypeUIntSampler1DArray;
          GL_UNSIGNED_INT_SAMPLER_2D_ARRAY: vCache.DataType :=
            GLSLTypeUIntSampler2DArray;
          GL_UNSIGNED_INT_SAMPLER_2D_RECT: vCache.DataType :=
            GLSLTypeUIntSamplerRect;
          GL_UNSIGNED_INT_SAMPLER_BUFFER: vCache.DataType :=
            GLSLTypeUIntSamplerBuffer;
          GL_SAMPLER_2D_MULTISAMPLE: vCache.DataType :=
            GLSLTypeSamplerMS;
          GL_INT_SAMPLER_2D_MULTISAMPLE: vCache.DataType :=
            GLSLTypeIntSamplerMS;
          GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE: vCache.DataType :=
            GLSLTypeUIntSamplerMS;
          GL_SAMPLER_2D_MULTISAMPLE_ARRAY: vCache.DataType :=
            GLSLTypeSamplerMSArray;
          GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY: vCache.DataType :=
            GLSLTypeIntSamplerMSArray;
          GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY: vCache.DataType :=
            GLSLTypeUIntSamplerMSArray;
        end;
        ShaderManagerLog.LogInfo(Format('Detected active uniform: %s %s', [cGLSLTypeString[vCache.DataType], Name]));
      end;
      vCache.WarningAbsenceLoged := False;
      Uniform.DoCache(AProgram.FHandle, vCache);
      if Uniform.FBlockOffset > -1 then
        ShaderManagerLog.LogInfo(Format('Block offset: %d', [Uniform.FBlockOffset]));
    end
    else
      ShaderManagerLog.LogWarning('Active uniform ' + Copy(string(buff), 0, len) +
        ' not registered');
  end;

  ShaderManagerLog.LogNotice('');
end;

class procedure ShaderManager.LinkShaderProgram(const AName: IGLName);
var
  prog: TGLSLProgram;
begin
  CheckCall;

  prog := GetProgram(AName);
  if prog.Link then
    MakeLocationCache(prog)
end;

class procedure ShaderManager.UseProgram(const AName: IGLName);
var
  prog: TGLSLProgram;
begin
  try
    BeginWork;
    prog := GetProgram(AName);

    if Assigned(prog) then
    begin
      if prog.FHandle.IsDataNeedUpdate then
      begin
        LinkShaderProgram(AName);
        if prog.FHandle.IsDataNeedUpdate then
          Abort;
      end;

      vCurrentProgram := prog;
      vCurrentProgram.FHandle.UseProgramObject;
    end
    else
    begin
      ShaderManagerLog.LogError('Used unknown program "' + string(AName.GetValue) + '"');
      Abort;
    end;

  finally
    EndWork;
  end;
end;

class function ShaderManager.IsProgramLinked(const AName: IGLName): Boolean;
var
  prog: TGLSLProgram;
begin
  Result := False;
  try
    BeginWork;
    prog := GetProgram(AName);

    if Assigned(prog) then
    begin
      prog.FHandle.AllocateHandle;
      if prog.FHandle.IsDataNeedUpdate then
        LinkShaderProgram(AName);
      Result := not prog.FHandle.IsDataNeedUpdate;
    end;
  finally
    EndWork;
  end;
end;

class function ShaderManager.IsProgramDefined(const AName: IGLName): Boolean;
begin
  CheckCall;
  Result := GetProgram(AName) <> nil;
end;

class function ShaderManager.IsProgramCurrent(const AName: IGLName): Boolean;
begin
  if vCurrentProgram = nil then
    Result := False
  else
    Result := (vCurrentProgram.Name.GetHash = AName.GetHash)
      and (vCurrentProgram.Name.GetValue = AName.GetValue);
end;

class function ShaderManager.GetCurrentProgram: IGLName;
begin
  if vCurrentProgram = nil then
    Result := nil
  else
    Result := vCurrentProgram.Name;
end;

class procedure ShaderManager.UseFixedFunctionPipeline;
begin
  vCurrentProgram := nil;
  CurrentGLContext.GLStates.CurrentProgram := 0;
end;

class procedure ShaderManager.DeleteShaderObject(AObject: TGLSLObject);
var
  I: Integer;
begin
  CheckCall;

  if Assigned(AObject) then
  begin
    for I := High(ShaderObjects) downto 0 do
      if AObject = ShaderObjects[I] then
      begin
        FreeAndNil(ShaderObjects[I]);
        exit;
      end;
  end
  else
    ShaderManagerLog.LogWarning('Attempt to delete a nil pointer of shader object');
end;

class procedure ShaderManager.DeleteShaderProgram(AProgram: TGLSLProgram);
var
  I: Integer;
begin
  CheckCall;

  if Assigned(AProgram) then
  begin
    for I := High(ShaderPrograms) downto 0 do
      if AProgram = ShaderPrograms[I] then
      begin
        FreeAndNil(ShaderPrograms[I]);
        exit;
      end;
  end
  else
    ShaderManagerLog.LogWarning('Attempt to delete a nil pointer of shader program');
end;

class procedure ShaderManager.DeleteShaderObject(var AName: IGLName);
begin
  DeleteShaderObject(GetObject(AName));
  AName := nil;
end;

class procedure ShaderManager.DeleteShaderProgram(var AName: IGLName);
begin
  DeleteShaderProgram(GetProgram(AName));
  AName := nil;
end;

class procedure ShaderManager.ClearShaderObject;
var
  I: Integer;
begin
  CheckCall;
  for I := High(ShaderObjects) downto 0 do
    FreeAndNil(ShaderObjects[I]);
end;

class procedure ShaderManager.ClearShaderPrograms;
var
  I: Integer;
begin
  CheckCall;
  for I := High(ShaderPrograms) downto 0 do
    FreeAndNil(ShaderPrograms[I]);
end;

class function ShaderManager.GetProgramAttribs(const AName: IGLName; out Attribs: TGLSLAttributeArray): Boolean;
var
  prog: TGLSLProgram;
  I, J, K: Integer;
begin
  CheckCall;
  prog := GetProgram(AName);
  if Assigned(prog) then
  begin
    J := 0;
    for I := 0 to High(AttributeRegistry) do
      if AttributeRegistry[I].FProgramCacheTree.Find(prog.FHandle, K) then
      begin
        Attribs[J] := AttributeRegistry[I];
        Inc(J);
      end;
    for I := J to High(Attribs) do
      Attribs[I] := nil;
    Result := True;
  end
  else
    Result := False;
end;

class procedure ShaderManager.Uniform1f(AUniform: TGLSLUniform; const
  Value: Single);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform1f(loc, Value);
end;

class procedure ShaderManager.Uniform1f(AUniform: TGLSLUniform;
  const Value: PFloat; Count: Integer);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform1fv(loc, Count, Value);
end;

class procedure ShaderManager.Uniform2f(AUniform: TGLSLUniform; const
  Value: TVector2f);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform2f(loc, Value[0], Value[1]);
end;

class procedure ShaderManager.Uniform3f(AUniform: TGLSLUniform; const
  Value: TVector3f);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform3f(loc, Value[0], Value[1], Value[2]);
end;

class procedure ShaderManager.Uniform4f(AUniform: TGLSLUniform; const
  Value: TVector4f);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform4f(loc, Value[0], Value[1], Value[2], Value[3]);
end;

class procedure ShaderManager.Uniform4f(AUniform: TGLSLUniform;
  const Value: PFloat; Count: Integer);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform4fv(loc, Count, Value);
end;

class procedure ShaderManager.Uniform1I(AUniform: TGLSLUniform; const
  Value: Integer);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform1i(loc, Value);
end;

class procedure ShaderManager.Uniform1I(AUniform: TGLSLUniform;
  const Value: PGLInt; Count: Integer);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform1iv(loc, Count, Value);
end;

class procedure ShaderManager.Uniform2I(AUniform: TGLSLUniform; const
  Value: TVector2I);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform2i(loc, Value[0], Value[1]);
end;

class procedure ShaderManager.Uniform3I(AUniform: TGLSLUniform; const
  Value: TVector3I);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform3i(loc, Value[0], Value[1], Value[2]);
end;

class procedure ShaderManager.Uniform4I(AUniform: TGLSLUniform; const
  Value: TVector4I);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform4i(loc, Value[0], Value[1], Value[2], Value[3]);
end;

class procedure ShaderManager.Uniform4I(AUniform: TGLSLUniform; const
  Value: PGLInt; Count: Integer);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform4iv(loc, Count, Value);
end;

class procedure ShaderManager.Uniform1UI(AUniform: TGLSLUniform; const
  Value: TGLuint);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform1ui(loc, Value);
end;

class procedure ShaderManager.Uniform2UI(AUniform: TGLSLUniform; const
  Value: TVector2UI);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform2ui(loc, Value[0], Value[1]);
end;

class procedure ShaderManager.Uniform3UI(AUniform: TGLSLUniform; const
  Value: TVector3UI);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform3ui(loc, Value[0], Value[1], Value[2]);
end;

class procedure ShaderManager.Uniform4UI(AUniform: TGLSLUniform; const
  Value: TVector4UI);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform4ui(loc, Value[0], Value[1], Value[2], Value[3]);
end;

class procedure ShaderManager.UniformMat2f(AUniform: TGLSLUniform; const
  Value: TMatrix2f);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.UniformMatrix2fv(loc, 1, False, @Value);
end;

class procedure ShaderManager.UniformMat3f(AUniform: TGLSLUniform; const
  Value: TMatrix3f);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.UniformMatrix3fv(loc, 1, False, @Value);
end;

class procedure ShaderManager.UniformMat4f(AUniform: TGLSLUniform; const
  Value: TMatrix4f);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.UniformMatrix4fv(loc, 1, False, @Value);
end;

class function ShaderManager.UniformSampler(AUniform: TGLSLUniform;
  const Texture: GLUInt): TGLuint;
var
  loc: GLInt;
  target: TGLTextureTarget;
  I, J: Integer;
  bindTime, minTime: Double;
begin
  loc := AUniform.Location;
  if (loc > -1) and GetTextureTarget(AUniform, target) then
    with CurrentGLContext.GLStates do
    begin
      // Find alredy binded texture unit
      for I := 0 to MaxTextureImageUnits - 1 do
      begin
        if TextureBinding[I, target] = Texture then
        begin
          GL.Uniform1i(loc, TGLuint(I));
          exit(I);
        end;
      end;
      // Find unused texture unit
      for I := 0 to MaxTextureImageUnits - 1 do
      begin
        if TextureBinding[I, target] = 0 then
        begin
          TextureBinding[I, target] := Texture;
          GL.Uniform1i(loc, TGLuint(I));
          exit(I);
        end;
      end;
      // Find most useless texture unit
      minTime := GLSTime;
      J := 0;
      for I := 0 to MaxTextureImageUnits - 1 do
      begin
        bindTime := TextureBindingTime[I, target];
        if bindTime < minTime then
        begin
          minTime := bindTime;
          J := I;
        end;
      end;
      TextureBinding[J, target] := Texture;
      GL.Uniform1i(loc, TGLuint(J));
      exit(J);
    end;
  Result := $FFFFFFFF;
end;

//class procedure ShadersManager.UniformBuffer(const ProgramName: string; AUniform: TGLSLUniform; const Buffer: GLUInt);
//var
//  RC: TGLContext;
//  prog: TGLSLProgram;
//  loc: GLInt;
//begin
//  if SafeCurrentGLContext(RC) then
//  begin
//    loc := AUniform.Location;
//    if loc < 0 then
//      exit;
//    BeginWork;
//
//    try
//      prog := GetShaderProgram(ProgramName);
//      if Assigned(prog) then
//      begin
//        if prog.FHandle.IsDataNeedUpdate then
//        begin
//          if not LinkShaderProgram(ProgramName) then
//            if not prog.Link then
//              Abort;
//        end;
//        GL.UniformBuffer(prog.FHandle.Handle, loc, Buffer);
//      end
//      else if RC.GLStates.CurrentProgram > 0 then
//        GL.UniformBuffer(RC.GLStates.CurrentProgram, loc, Buffer);
//
//    finally
//      EndWork;
//    end;
//  end;
//end;
{$ENDREGION}

initialization

  ShaderManager.Initialize;
  SetLength(ProgramCache, GLS_PROGRAM_CACHE_SIZE);

  {: Registration of the most common attributes. }
  attrPosition :=
    TGLSLAttribute.RegisterAttribute('Position');
  attrNormal :=
    TGLSLAttribute.RegisterAttribute('Normal');
  attrVertexColor :=
    TGLSLAttribute.RegisterAttribute('VertexColor');
  attrTexCoord0 :=
    TGLSLAttribute.RegisterAttribute('TexCoord0');
  attrTexCoord1 :=
    TGLSLAttribute.RegisterAttribute('TexCoord1');
  attrTexCoord2 :=
    TGLSLAttribute.RegisterAttribute('TexCoord2');
  attrTexCoord3 :=
    TGLSLAttribute.RegisterAttribute('TexCoord3');
  attrTexCoord4 :=
    TGLSLAttribute.RegisterAttribute('TexCoord4');
  attrTexCoord5 :=
    TGLSLAttribute.RegisterAttribute('TexCoord5');
  attrTexCoord6 :=
    TGLSLAttribute.RegisterAttribute('TexCoord6');
  attrTexCoord7 :=
    TGLSLAttribute.RegisterAttribute('TexCoord7');
  attrTangent :=
    TGLSLAttribute.RegisterAttribute('Tangent');
  attrBinormal :=
    TGLSLAttribute.RegisterAttribute('Binormal');
  attrInstanceID :=
    TGLSLAttribute.RegisterAttribute('gl_InstanceID');

  {: Registration of the most common uniforms. }
  uniformModelMatrix
    := TGLSLUniform.RegisterUniform('ModelMatrix');
  uniformViewProjectionMatrix
    := TGLSLUniform.RegisterUniform('ViewProjectionMatrix');
  uniformCameraWorldPosition
    := TGLSLUniform.RegisterUniform('CameraWorldPosition');
  uniformTexUnit0
    := TGLSLUniform.RegisterUniform('TexUnit0');
  uniformTexUnit1
    := TGLSLUniform.RegisterUniform('TexUnit1');
  uniformTexUnit2
    := TGLSLUniform.RegisterUniform('TexUnit2');
  uniformTexUnit3
    := TGLSLUniform.RegisterUniform('TexUnit3');
  uniformTexUnit4
    := TGLSLUniform.RegisterUniform('TexUnit4');
  uniformTexUnit5
    := TGLSLUniform.RegisterUniform('TexUnit5');
  uniformTexUnit6
    := TGLSLUniform.RegisterUniform('TexUnit6');
  uniformTexUnit7
    := TGLSLUniform.RegisterUniform('TexUnit7');

finalization

  ShaderManager.Finalize;
  ClearRegistries;

end.

