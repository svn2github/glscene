//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xShadersManager<p>

   Base unit to manage shader objects and program.<p>
   Can work with different rendering context and thread.<p>
   Once defined program or object can be used everywhere.<p>

   <b>History : </b><font size=-1><ul>
    <li>14/04/10 - Yar - Attributes and Uniforms becomes one class per name
                         Added multicontext and multithreading support
    <li>24/03/10 - Yar - Creation (based on uShaders.pas by Alex Karpenyuk aka Fantom)
 </ul></font>
}

// TODO: Bindless graphic

unit GLShaderManager;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  SysUtils,
{$IFNDEF FPC}
  Generics.Collections,
{$ELSE}
  Contnrs,
{$ENDIF}
  // GLScene
  GLCrossPlatform,
  OpenGLTokens,
  GLContext,
  GLState,
  GLTextureFormat,
  VectorLists,
  VectorTypes,
  GLSRedBlackTree,
  VectorGeometryEXT,
  GLSLog;

const
  GLS_PROGRAM_CACHE_SIZE = 2048;

type

  TGLSLProgramType = (ptVertex, ptGeometry, ptFragment, ptControl, ptEvaluation);
  TGLSLProgramTypes = set of TGLSLProgramType;

  TColorComponent = (ccmRed, ccmGreen, ccmBlue, ccmAlpha, ccmWhite);
  TColorComponentMask = set of TColorComponent;

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
    GLSLTypeVRec
    );

  TProgramCache = record
    Location: GLint;
    BindedBuffer: GLuint;
    DataType: TGLSLDataType;
    WarningAbsenceLoged: Boolean;
  end;

  TProgramCacheTree =
{$IFDEF FPC}specialize{$ENDIF}GRedBlackTree < TGLProgramHandle, Integer > ;

  TGLSLBaseVariable = class
  private
    FName: string;
    FProgramCacheTree: TProgramCacheTree;
    FLastProgram: TGLProgramHandle;
    FLastCacheIndex: Integer;
    FTag: Integer;
    procedure DoCache(AProg: TGLProgramHandle; const ACache: TProgramCache);
    procedure ClearCache(AProg: TGLProgramHandle);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ResetLocationCache;

    property Name: string read FName;
    property Tag: Integer read FTag write FTag;
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
    function GetLocation: GLInt;
    function GetDataType: TGLSLDataType;
    function GetBindedBuffer: TGLuint;
    procedure SetBindedBuffer(Value: TGLuint);
  public
    { Public Declarations }
    procedure BeforeDestruction; override;
    constructor RegisterUniform(const AName: string);
    class function GetUniform(const AName: string): TGLSLUniform;

    property Location: GLInt read GetLocation;
    property DataType: TGLSLDataType read GetDataType;
    property BindedBuffer: TGLuint read GetBindedBuffer write SetBindedBuffer;
  end;

  // TGLSLUniformBlock
  //

  TGLSLUniformBlock = class(TGLSLBaseVariable)
  private
    { Private Declarations }
    FSize: GLSizei;
    FUniforms: TList;
    function GetLocation: GLInt;
  public
    { Public Declarations }
    procedure BeforeDestruction; override;
    constructor RegisterUniformBlock(const AName: string);
    class function GetUniformBlock(const AName: string): TGLSLUniformBlock;

    property Location: GLInt read GetLocation;
    property DataSize: GLSizei read FSize;
  end;

  // TGLSLShaderObject
  //

  {: Context independent shader object }
  TGLSLShaderObject = class
  private
    FHandles: array[TGLSLProgramType] of TGLShaderHandle;
    FNameHashKey: Integer;
    FFriendlyName: string;
    FCode: string;
    FTypes: TGLSLProgramTypes;
  public
    constructor Create;
    destructor Destroy; override;
    {: Compile object under current context }
    function Compile: Boolean;
    property FriendlyName: string read FFriendlyName;
  end;

  // TGLSLShaderProgram
  //

  {: Context independent program object }
  TGLSLShaderProgram = class
  private
    FHandle: TGLProgramHandle;
    FNameHashKey: Integer;
    FFriendlyName: string;
    FAttachedObjects: TList{$IFNDEF FPC} < TGLSLShaderObject > {$ENDIF};
    FProgramMask: TGLSLProgramTypes;
    procedure DetachAll;
  public
    constructor Create;
    destructor Destroy; override;
    {: Attach objects and link program under current context }
    procedure Attach(AObject: TGLSLShaderObject);
    procedure Detach(AObject: TGLSLShaderObject);
    function Link: Boolean;
    property FriendlyName: string read FFriendlyName;
  end;

  TShaderObjectTree =
{$IFDEF FPC}specialize{$ENDIF}GRedBlackTree < Integer, TGLSLShaderObject > ;
  TShaderProgramTree =
{$IFDEF FPC}specialize{$ENDIF}GRedBlackTree < Integer, TGLSLShaderProgram > ;

  TGLShaderManager = class
  private
    { Private Declarations }
    FShaderObjectsTree: TShaderObjectTree;
    FShaderProgramsTree: TShaderProgramTree;
    CompilationLog: TLogSession;
    WorkLog: TLogSession;
{$IFDEF GLS_MULTITHREAD}
    FLock: TRTLCriticalSection;
{$ENDIF}
  protected
    { Protected Declarations }
    function GetShaderObject(const AName: string): TGLSLShaderObject;
    function GetShaderProgram(const AName: string): TGLSLShaderProgram;
    function GetTextureTarget(const AUniform: TGLSLUniform;
      out ATarget: TGLTextureTarget): Boolean;
    procedure DeleteShaderObject(AObject: TGLSLShaderObject); overload;
    procedure DeleteShaderProgram(AProgram: TGLSLShaderProgram); overload;
    {: Cash locations of attribute, uniforms and uniform bBeginWorks }
    procedure MakeLocationCache(AProgram: TGLSLShaderProgram);
    function ComputeNameHashKey(const AName: string): Integer;
  public
    { Public Declarations }
    constructor Create;
    destructor Destroy; override;
    {: Call this method before begin work with programs and objects. }
    procedure BeginWork;
    {: End of work with programs and objects. }
    procedure EndWork;

    procedure DefineShaderObject(const AName: string; const code: AnsiString;
      ATypes: TGLSLProgramTypes);
    procedure DefineShaderProgram(const AName: string; AProgramMask: TGLSLProgramTypes = [ptVertex, ptFragment]);
    procedure DeleteShaderObject(const AName: string); overload;
    procedure DeleteShaderProgram(const AName: string); overload;

    procedure AttachShaderObjectToProgram(const AObject: string; const AProgram:
      string);
    procedure DetachShaderObjectFromProgram(const AObject: string; const
      AProgram: string);

    function LinkShaderProgram(const AName: string): Boolean;
    procedure UseProgram(const AName: string);
    function IsProgramDefined(const AName: string): Boolean;
    function IsProgramCurrent(const AName: string): Boolean;
    function GetCurrentProgram: string;
    procedure UseFixedFunctionPipeline;
    procedure ClearShaderObject;
    procedure ClearShaderPrograms;
    function GetProgramAttribs(const AName: string; out Attribs: TGLSLAttributeArray): Boolean;

    function MakeUniqueObjectName(const NameRoot: string): string;
    function MakeUniqueProgramName(const NameRoot: string): string;

    // Float uniforms
    procedure Uniform1f(AUniform: TGLSLUniform; const Value: Single);
    procedure Uniform2f(AUniform: TGLSLUniform; const Value: TVector2f);
    procedure Uniform3f(AUniform: TGLSLUniform; const Value: TVector3f);
    procedure Uniform4f(AUniform: TGLSLUniform; const Value: TVector4f);

    // Integer uniforms
    procedure Uniform1I(AUniform: TGLSLUniform; const Value: TGLint); overload;
    procedure Uniform1I(AUniform: TGLSLUniform; const Value: PGLInt; Count: Integer); overload;
    procedure Uniform2I(AUniform: TGLSLUniform; const Value: TVector2I);
    procedure Uniform3I(AUniform: TGLSLUniform; const Value: TVector3I);
    procedure Uniform4I(AUniform: TGLSLUniform; const Value: TVector4I); overload;
    procedure Uniform4I(AUniform: TGLSLUniform; const Value: PGLInt; Count: Integer); overload;

    // Unsigned integer uniforms
    procedure Uniform1UI(AUniform: TGLSLUniform; const Value: TGLuint);
    procedure Uniform2UI(AUniform: TGLSLUniform; const Value: TVector2UI);
    procedure Uniform3UI(AUniform: TGLSLUniform; const Value: TVector3UI);
    procedure Uniform4UI(AUniform: TGLSLUniform; const Value: TVector4UI);

    // Matrix uniforms
    procedure UniformMat2f(AUniform: TGLSLUniform; const Value: TMatrix2f);
    procedure UniformMat3f(AUniform: TGLSLUniform; const Value: TMatrix3f);
    procedure UniformMat4f(AUniform: TGLSLUniform; const Value: TMatrix4f);

    // Other uniforms
    procedure UniformSampler(AUniform: TGLSLUniform; const Texture: GLUInt; TexUnit: GLUInt);
    //    procedure UniformBuffer(const ProgramName: string; AUniform: TGLSLUniform; const Buffer: GLUInt);
  end;

  TCurrentProgramGetter = class
  protected
    class function CurrentProgram: TGLProgramHandle;
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
    uniformLightWorldPosition,
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
function GetGLSLTypeCast(const Arg: AnsiString; ArgType: TGLSLDataType; AMask: TColorComponentMask; CastType: TGLSLDataType = GLSLTypeUndefined): AnsiString;
function GLSLTypeToString(AType: TGLSLDataType): AnsiString;
function GLSLTypeComponentCount(AType: TGLSLDataType): Integer;
function HexToGLSL(tp: TGLSLDataType; HexValue: string): AnsiString;
function MaskToGLSLType(AMask: TColorComponentMask): TGLSLDataType;
function RemoveGLSLQualifier(const InputLine: string): string;
function ShaderManager: TGLShaderManager;

implementation

uses
  GLStrings, GLVBOManager;

resourcestring
  glsWrongMethodCall =
    'This method must be called between BeginWork...EndWork!';

const
  cGLSLTypeString: array[TGLSLDataType] of AnsiString = (
    '',
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
    'vrec');

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
    0);

const
  cObjectClass: array[TGLSLProgramType] of TGLShaderHandleClass =
    (TGLVertexShaderHandle,
    TGLGeometryShaderHandle,
    TGLFragmentShaderHandle,
    TGLTessControlShaderHandle,
    TGLTessEvaluationShaderHandle);

  cObjectTypeName: array[TGLSLProgramType] of string =
    ('Vertex', 'Geomtery', 'Fragment', 'Control', 'Evaluation');

var
  vShaderManager: TGLShaderManager;

{$IFDEF GLS_MULTITHREAD}
threadvar
{$ENDIF}
  vCurrentProgram: TGLSLShaderProgram;
  vWorked: Boolean;

function ShaderManager: TGLShaderManager;
begin
  if vShaderManager = nil then
    vShaderManager := TGLShaderManager.Create;
  Result := vShaderManager;
end;

class function TCurrentProgramGetter.CurrentProgram: TGLProgramHandle;
begin
  if Assigned(vCurrentProgram) then
    Result := vCurrentProgram.FHandle
  else
    Result := nil;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Helper functions'}{$ENDIF}

function GetMaxGLSLVersion: AnsiString;
begin
  Result := '#version 150 core';
end;

function MaskToGLSLType(AMask: TColorComponentMask): TGLSLDataType;
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

function GetGLSLTypeCast(const Arg: AnsiString; ArgType: TGLSLDataType; AMask: TColorComponentMask; CastType: TGLSLDataType): AnsiString;
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
      1: Result := AnsiString(Format('%s(%s, 0.0)', [cGLSLTypeString[CastType], Arg]));
      2: Result := AnsiString(Format('%s(%s, 0.0, 0.0)', [cGLSLTypeString[CastType], Arg]));
      3: Result := AnsiString(Format('%s(%s, 0.0, 0.0, 0.0)', [cGLSLTypeString[CastType], Arg]));
    else
      Assert(False);
    end;
  end
  else
    Result := Arg;
end;

function GLSLTypeToString(AType: TGLSLDataType): AnsiString;
begin
  Result := cGLSLTypeString[AType];
end;

function GLSLTypeComponentCount(AType: TGLSLDataType): Integer;
begin
  Result := cGLSLTypeComponents[AType];
end;

function HexToGLSL(tp: TGLSLDataType; HexValue: string): AnsiString;
var
  v: TVectorEXT;
  i: TIntVectorEXT;
  oldDS: Char;
begin
  oldDS := DecimalSeparator;
  DecimalSeparator := '.';
  Result := '';
  case tp of
    GLSLTypeUndefined: ;
    GLSLTypeVoid: ;
    GLSLType1F:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@v.V[0]), SizeOf(TVectorEXT));
        Result := AnsiString(Format('%.7f', [v.X]));
      end;
    GLSLType2F:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@v.V[0]), SizeOf(TVectorEXT));
        Result := AnsiString(Format('vec2(%.7f, %.7f)', [v.X, v.Y]));
      end;
    GLSLType3F:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@v.V[0]), SizeOf(TVectorEXT));
        Result := AnsiString(Format('vec3(%.7f, %.7f, %.7f)', [v.X, v.Y, v.Z]));
      end;
    GLSLType4F:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@v.V[0]), SizeOf(TVectorEXT));
        Result := AnsiString(Format('vec4(%.7f, %.7f, %.7f, %.7f)', [v.X, v.Y, v.Z, v.W]));
      end;
    GLSLType1I:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@i.V[0]), SizeOf(TIntVectorEXT));
        Result := AnsiString(Format('%d', [i.X]));
      end;
    GLSLType2I:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@i.V[0]), SizeOf(TIntVectorEXT));
        Result := AnsiString(Format('ivec2(%d, %d)', [v.X, v.Y]));
      end;
    GLSLType3I:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@i.V[0]), SizeOf(TIntVectorEXT));
        Result := AnsiString(Format('ivec3(%d, %d, %d)', [v.X, v.Y, v.Z]));
      end;
    GLSLType4I:
      begin
        HexToBin(PChar(HexValue), PAnsiChar(@i.V[0]), SizeOf(TIntVectorEXT));
        Result := AnsiString(Format('ivec4(%d, %d, %d, %d)', [v.X, v.Y, v.Z, v.W]));
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
  DecimalSeparator := oldDS;
end;

function RemoveGLSLQualifier(const InputLine: string): string;
var
  P: Integer;
begin
  Result := InputLine;
  while True do
  begin
    P := Pos('in ', Result);
    if P>0 then
    begin
      Delete(Result, P, 3);
      continue;
    end;
    P := Pos('inout ', Result);
    if P>0 then
    begin
      Delete(Result, P, 6);
      continue;
    end;
    P := Pos('out ', Result);
    if P>0 then
    begin
      Delete(Result, P, 4);
      continue;
    end;
    break;
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

function CompareContext(const Item1, Item2: TGLContext): Integer;
var
  I1, I2: Cardinal;
begin
  I1 := Cardinal(Item1);
  I2 := Cardinal(Item2);
  if I1 < I2 then
  begin
    Result := -1;
  end
  else if (I1 = I2) then
  begin
    Result := 0;
  end
  else
  begin
    Result := 1;
  end
end;

function CompareProgram(const Item1, Item2: TGLProgramHandle): Integer;
begin
  if PtrUInt(Item1) < PtrUInt(Item2) then
  begin
    Result := -1;
  end
  else if (Item1 = Item2) then
  begin
    Result := 0;
  end
  else
  begin
    Result := 1;
  end
end;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Shader variables registry'}{$ENDIF}
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
  FProgramCacheTree.Free;
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
  else begin
    ProgramCache[ProgramCachPosition] := ACache;
    FProgramCacheTree.Add(AProg, ProgramCachPosition);
    FLastProgram := AProg;
    FLastCacheIndex := ProgramCachPosition;
    Inc(ProgramCachPosition);
    if Length(ProgramCache) = ProgramCachPosition then
      SetLength(ProgramCache, 2*Length(ProgramCache));
  end;
end;

procedure TGLSLBaseVariable.ClearCache(AProg: TGLProgramHandle);
var
  I: Integer;
begin
  if FProgramCacheTree.Find(AProg, I) then
  begin
    ProgramCache[I].Location := -1;
    ProgramCache[I].BindedBuffer := 0;
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
    ShaderManager.WorkLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else
    if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
      I := -1;

  if I > -1 then
  begin
    if (ProgramCache[I].Location = -1)
      and not ProgramCache[I].WarningAbsenceLoged then
    begin
      ShaderManager.WorkLog.LogWarning(
        Format('Using an unknown attribute "%s" in program "%s"', [FName, vCurrentProgram.FFriendlyName]));
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
    ShaderManager.WorkLog.LogError(glsNoShader);
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
    ShaderManager.WorkLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else
    if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
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
    ShaderManager.WorkLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else
    if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
      I := -1;

  if I > -1 then
  begin
    if (ProgramCache[I].Location = -1)
      and not ProgramCache[I].WarningAbsenceLoged then
    begin
      ShaderManager.WorkLog.LogWarning(
        Format('Using an unknown uniform "%s" in program "%s"', [FName, vCurrentProgram.FFriendlyName]));
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
    ShaderManager.WorkLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else
    if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
      I := -1;

  if I > -1 then
  begin
    Result := ProgramCache[I].DataType;
    FLastProgram := vCurrentProgram.FHandle;
    FLastCacheIndex := I;
  end;
end;

function TGLSLUniform.GetBindedBuffer: TGLuint;
var
  I: Integer;
begin
  Result := 0;
  if not Assigned(vCurrentProgram) then
  begin
    ShaderManager.WorkLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else
    if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
      I := -1;

  if I > -1 then
  begin
    Result := ProgramCache[I].BindedBuffer;
    FLastProgram := vCurrentProgram.FHandle;
    FLastCacheIndex := I;
  end;
end;

procedure TGLSLUniform.SetBindedBuffer(Value: TGLuint);
var
  I: Integer;
begin
  if not Assigned(vCurrentProgram) then
  begin
    ShaderManager.WorkLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else
    if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
      I := -1;

  if I > -1 then
  begin
    if ProgramCache[I].Location = -1 then
    begin
      if not ProgramCache[I].WarningAbsenceLoged then
      begin
        ShaderManager.WorkLog.LogWarning(
          Format('Using an unknown uniform buffer "%s" in program "%s"', [FName, vCurrentProgram.FFriendlyName]));
        ProgramCache[I].WarningAbsenceLoged := True;
      end;
    end
    else
    begin
      ProgramCache[I].BindedBuffer := Value;
      GL.UniformBuffer(vCurrentProgram.FHandle.Handle, ProgramCache[I].Location, Value);
    end;
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
    ShaderManager.WorkLog.LogError(glsNoShader);
    Abort;
  end;

  if FLastProgram = vCurrentProgram.FHandle then
    I := FLastCacheIndex
  else
    if not FProgramCacheTree.Find(vCurrentProgram.FHandle, I) then
      I := -1;

  if I > -1 then
  begin
    if (ProgramCache[I].Location = -1)
      and not ProgramCache[I].WarningAbsenceLoged then
    begin
      ShaderManager.WorkLog.LogWarning(
        Format('Using an unknown uniform block "%s" in program "%s"', [FName, vCurrentProgram.FFriendlyName]));
      ProgramCache[I].WarningAbsenceLoged := True;
    end
    else
      Result := ProgramCache[I].Location;
    FLastProgram := vCurrentProgram.FHandle;
    FLastCacheIndex := I;
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLSLShaderObject'}{$ENDIF}
// ------------------
// ------------------ TGLSLShaderObject ------------------
// ------------------

procedure ShaderHandleDestroyer(k: TGLContext; AHandle:
  TGLShaderHandle; out Flag: Boolean);
begin
  AHandle.Destroy;
  Flag := True;
end;

constructor TGLSLShaderObject.Create;
var
  P: TGLSLProgramType;
begin
  inherited;
  for P := Low(TGLSLProgramType) to High(TGLSLProgramType) do
    FHandles[P] := cObjectClass[P].Create;
end;

destructor TGLSLShaderObject.Destroy;
var
  P: TGLSLProgramType;
begin
  inherited;
  for P := low(TGLSLProgramType) to high(TGLSLProgramType) do
    FHandles[P].Destroy;
end;

function TGLSLShaderObject.Compile: Boolean;
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
      logstr := Format('%s shader object "%s" compilation - ', [cObjectTypeName[P], FriendlyName]);

      if not cObjectClass[P].IsSupported then
      begin
        vShaderManager.CompilationLog.LogWarning(logstr + 'Discarded. Shader not supported.');
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
        vShaderManager.CompilationLog.LogInfo(logstr + ' Successful');
        FHandles[P].NotifyDataUpdated;
      end
      else
        vShaderManager.CompilationLog.LogError(logstr + ' Failed!');

      GL.GetShaderiv(FHandles[P].Handle, GL_INFO_LOG_LENGTH, @val);
      if val > 1 then
      begin
        ReallocMem(pLog, val);
        GL.GetShaderInfoLog(FHandles[P].Handle, val, @len, pLog);
        vShaderManager.CompilationLog.LogInfo(string(pLog));
      end;
    end
    else
      FHandles[P].DestroyHandle;

  if pLog <> nil then
    FreeMem(pLog);
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLSLShaderProgram'}{$ENDIF}
// ------------------
// ------------------ TGLSLShaderProgram ------------------
// ------------------

constructor TGLSLShaderProgram.Create;
begin
  FHandle := TGLProgramHandle.Create;
  FAttachedObjects := TList
{$IFNDEF FPC} < TGLSLShaderObject > {$ENDIF}.Create;
end;

destructor TGLSLShaderProgram.Destroy;
begin
  inherited;
  FHandle.Destroy;
  FAttachedObjects.Destroy;
end;

function TGLSLShaderProgram.Link: Boolean;
var
  P: TGLSLProgramType;
  obj: TGLSLShaderObject;
  I: Integer;
  val, len: GLsizei;
  pLog: PAnsiChar;
  logstr: string;
begin
  FHandle.AllocateHandle;
  DetachAll;

  for I := 0 to FAttachedObjects.Count - 1 do
  begin
{$IFDEF FPC}
    obj := TGLSLShaderObject(FAttachedObjects.Items[I]);
{$ELSE}
    obj := FAttachedObjects.Items[I];
{$ENDIF};
    for P := low(TGLSLProgramType) to high(TGLSLProgramType) do
      if (P in obj.FTypes) and (P in FProgramMask) then
      begin

        if obj.FHandles[P].IsDataNeedUpdate then
        begin
          if obj.Compile then
          begin
            GL.AttachShader(FHandle.Handle, obj.FHandles[P].Handle);
            vShaderManager.WorkLog.LogDebug(Format('%s object %s attached to %s', [cObjectTypeName[P], obj.FFriendlyName, Self.FFriendlyName]));
            vShaderManager.WorkLog.LogDebug(obj.FCode);
          end
          else
            Abort;
        end
        else
        begin
          GL.AttachShader(FHandle.Handle, obj.FHandles[P].Handle);
          vShaderManager.WorkLog.LogDebug(Format('%s object %s attached to %s', [cObjectTypeName[P], obj.FFriendlyName, Self.FFriendlyName]));
          vShaderManager.WorkLog.LogDebug(obj.FCode);
        end;

      end;
  end;

  Result := FHandle.LinkProgram;

  logstr := 'Shader Program ' + FriendlyName + ' Linking - ';
  if Result then
  begin
    vShaderManager.CompilationLog.LogInfo(logstr + ' Successful');
    FHandle.NotifyDataUpdated;
  end
  else
    vShaderManager.CompilationLog.LogError(logstr + ' Failed!');

  GL.GetProgramiv(FHandle.Handle, GL_INFO_LOG_LENGTH, @val);
  if val > 0 then
  begin
    GetMem(pLog, val);
    GL.GetProgramInfoLog(FHandle.Handle, val, @len, pLog);
    vShaderManager.CompilationLog.LogInfo(string(pLog));
    FreeMem(pLog, val);
  end;

  // Notify VBO managers about program changes
  StaticVBOManager.NotifyProgramChanged(FHandle);
  StreamVBOManager.NotifyProgramChanged(FHandle);
  ClearProgramCache(FHandle);
end;

procedure TGLSLShaderProgram.Attach(AObject: TGLSLShaderObject);
begin
  if FAttachedObjects.IndexOf(AObject) < 0 then
  begin
    FAttachedObjects.Add(AObject);
    FHandle.NotifyChangesOfData;
  end;
end;

procedure TGLSLShaderProgram.Detach(AObject: TGLSLShaderObject);
begin
  if FAttachedObjects.Remove(AObject) >= 0 then
    FHandle.NotifyChangesOfData;
end;

procedure TGLSLShaderProgram.DetachAll;
var
  I: Integer;
  count: GLSizei;
  buffer: array[0..255] of TGLuint;
begin
  GL.ClearError;
  GL.GetAttachedShaders(FHandle.Handle, Length(buffer), @count, @buffer[0]);
  if GL.GetError = GL_NO_ERROR then
    for I := 0 to count - 1 do
      GL.DetachShader(FHandle.Handle, buffer[I]);
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLShadersManager'}{$ENDIF}
// ------------------
// ------------------ TGLShadersManager ------------------
// ------------------

{: Static function.
   Since Lazarus does not support static functions of classes, then do so... }

procedure ObjectDestroyer(
  k: Integer; AObject: TGLSLShaderObject; out Flag: Boolean);
begin
  AObject.Destroy;
  Flag := True;
end;

procedure ProgramDestroyer(
  k: Integer; AProgram: TGLSLShaderProgram; out Flag: Boolean);
begin
  AProgram.Destroy;
  Flag := True;
end;

constructor TGLShaderManager.Create;
var
  LogPath: string;
begin
  FShaderObjectsTree := TShaderObjectTree.Create(CompareInteger, nil);
  FShaderProgramsTree := TShaderProgramTree.Create(CompareInteger, nil);

  LogPath := ExtractFilePath(ParamStr(0));
  CompilationLog
    := TLogSession.Init(LogPath + 'ShadersCompilation.log', lfNone,
    [lkNotice, lkInfo, lkWarning, lkError]);
  WorkLog
    := TLogSession.Init(LogPath + 'ShadersWork.log', lfNone,
    [lkWarning, lkError
{$IFDEF GLS_OPENGL_DEBUG}
    ,lkDebug
{$ENDIF}
    ]);
  vWorked := False;
{$IFDEF GLS_MULTITHREAD}
  InitializeCriticalSection(FLock);
{$ENDIF}
end;

destructor TGLShaderManager.Destroy;
begin
  BeginWork;
  ClearShaderPrograms;
  ClearShaderObject;
  EndWork;
  FShaderObjectsTree.Destroy;
  FShaderProgramsTree.Destroy;
  CompilationLog.Shutdown;
  WorkLog.Shutdown;
{$IFDEF GLS_MULTITHREAD}
  DeleteCriticalSection(FLock);
{$ENDIF}
  inherited;
end;

procedure TGLShaderManager.BeginWork;
var
  bPrev: Boolean;
begin
{$IFDEF GLS_MULTITHREAD}
  EnterCriticalSection(FLock);
{$ENDIF}
  bPrev := vWorked;
  vWorked := True;
  Assert(not bPrev, 'Excessive call ShadersManager.BeginWork');
end;

procedure TGLShaderManager.EndWork;
var
  bPrev: Boolean;
begin
{$IFDEF GLS_MULTITHREAD}
  LeaveCriticalSection(FLock);
{$ENDIF}
  bPrev := vWorked;
  vWorked := False;
  Assert(bPrev, 'Excessive call ShadersManager.EndWork');
end;

function TGLShaderManager.GetShaderObject(const AName: string):
  TGLSLShaderObject;
begin
  Assert(vWorked, glsWrongMethodCall);
  if not FShaderObjectsTree.Find(ComputeNameHashKey(AName), Result) then
    Result := nil;
end;

function TGLShaderManager.GetShaderProgram(const AName: string):
  TGLSLShaderProgram;
begin
  Assert(vWorked, glsWrongMethodCall);
  if not FShaderProgramsTree.Find(ComputeNameHashKey(AName), Result) then
    Result := nil;
end;

function TGLShaderManager.ComputeNameHashKey(const AName: string): Integer;
var
  i, n: Integer;
begin
  n := Length(AName);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(AName[i]);
end;

function TGLShaderManager.GetTextureTarget(const AUniform: TGLSLUniform;
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
    WorkLog.LogError('Uniform "' + AUniform.Name + '" has not sampler type')
  else
    ATarget := cGLSLTypeToTexTarget[dt];
end;

procedure TGLShaderManager.DefineShaderObject(
  const AName: string;
  const Code: AnsiString;
  ATypes: TGLSLProgramTypes);
var
  Obj: TGLSLShaderObject;
  newCode: Boolean;
  P: TGLSLProgramType;
begin
  Assert(vWorked, glsWrongMethodCall);
  SafeCurrentGLContext;

  Obj := GetShaderObject(AName);
  if not Assigned(Obj) then
  begin
    with Obj do
    begin
      Obj := TGLSLShaderObject.Create;
      FFriendlyName := AName;
      FNameHashKey := ComputeNameHashKey(AName);
      FShaderObjectsTree.Add(FNameHashKey, Obj);
      newCode := True;
    end;
  end
  else
    newCode := (CompareStr(Obj.FCode, string(Code)) <> 0) or (Obj.FTypes <> ATypes);

  if newCode then
    with Obj do
    begin
      FTypes := ATypes;
      FCode := string(Code);
      for P := Low(TGLSLProgramType) to High(TGLSLProgramType) do
        FHandles[P].NotifyChangesOfData;
      Compile;
    end;
end;

procedure TGLShaderManager.DefineShaderProgram(const AName: string; AProgramMask: TGLSLProgramTypes);
var
  Prog: TGLSLShaderProgram;
begin
  Assert(vWorked, glsWrongMethodCall);
  SafeCurrentGLContext;

  Prog := GetShaderProgram(AName);
  if not Assigned(Prog) then
  begin
    Prog := TGLSLShaderProgram.Create;
    Prog.FFriendlyName := AName;
    Prog.FNameHashKey := ComputeNameHashKey(AName);
    FShaderProgramsTree.Add(Prog.FNameHashKey, Prog);
  end
  else
  begin
    Prog.DetachAll;
    Prog.FAttachedObjects.Clear;
  end;
  Prog.FProgramMask := AProgramMask;
end;

procedure TGLShaderManager.AttachShaderObjectToProgram(
  const AObject: string; const AProgram: string);
var
  obj: TGLSLShaderObject;
  Prog: TGLSLShaderProgram;
begin
  Assert(vWorked, glsWrongMethodCall);
  Prog := GetShaderProgram(AProgram);
  obj := GetShaderObject(AObject);
  Prog.Attach(obj);
end;

procedure TGLShaderManager.DetachShaderObjectFromProgram(
  const AObject: string; const AProgram: string);
var
  obj: TGLSLShaderObject;
  Prog: TGLSLShaderProgram;
begin
  Assert(vWorked, glsWrongMethodCall);
  obj := GetShaderObject(AObject);
  Prog := GetShaderProgram(AProgram);
  Prog.Detach(obj);
end;

procedure TGLShaderManager.MakeLocationCache(AProgram: TGLSLShaderProgram);
var
  I, J: Integer;
  ProgramID: GLuint;
  buff: array[0..127] of AnsiChar;
  indices: array[0..127] of GLInt;
  max: GLInt;
  Size: GLInt;
  len: GLsizei;
  AType: GLenum;
  Attr: TGLSLAttribute;
  Uniform: TGLSLUniform;
  UniformBlock: TGLSLUniformBlock;
  vCache: TProgramCache;
begin
  Assert(vWorked, glsWrongMethodCall);
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
        CompilationLog.LogInfo('Detected active attribute: ' + Name);
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
        else
          begin
            vCache.DataType := GLSLTypeUndefined;
            CompilationLog.LogError('Active attribute ' + Name + ' with ' +
              glsUnknownType);
          end;
        end;
      end;
      vCache.WarningAbsenceLoged := False;
      Attr.DoCache(AProgram.FHandle, vCache);
    end
    else
      CompilationLog.LogWarning('Active attribute ' + Copy(string(buff), 0, len)
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
          CompilationLog.LogInfo('Detected active uniform block: ' + Name);
          vCache.Location :=
            GL.GetUniformBlockIndex(ProgramID, PGLChar(TGLString(Name)));
          GL.GetActiveUniformBlockiv(ProgramID, vCache.Location,
            GL_UNIFORM_BLOCK_DATA_SIZE, @FSize);
          GL.GetActiveUniformBlockiv(ProgramID, vCache.Location,
            GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS, @Size);
          GL.GetActiveUniformBlockiv(ProgramID, vCache.Location,
            GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES, @indices[0]);
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
            if FUniforms.IndexOf(Uniform)<0 then
              FUniforms.Add(Uniform);
          end;
        end;
        vCache.WarningAbsenceLoged := False;
        UniformBlock.DoCache(AProgram.FHandle, vCache);
      end
      else
      begin
        CompilationLog.LogError('Active uniform block: ' +
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
      I,
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
        CompilationLog.LogInfo('Detected active uniform: ' + Name);
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
        else
          begin
            vCache.DataType := GLSLTypeUndefined;
            CompilationLog.LogError('Active uniform ' + Name + ' with ' +
              glsUnknownType);
          end;
        end;
      end;
      vCache.WarningAbsenceLoged := False;
      vCache.BindedBuffer := 0;
      Uniform.DoCache(AProgram.FHandle, vCache);
    end
    else
      CompilationLog.LogWarning('Active uniform ' + Copy(string(buff), 0, len) +
        ' not registered');
  end;

  vShaderManager.CompilationLog.LogNotice('');
end;

function TGLShaderManager.LinkShaderProgram(const AName: string): Boolean;
var
  prog: TGLSLShaderProgram;
begin
  Assert(vWorked, glsWrongMethodCall);
  SafeCurrentGLContext;

  prog := GetShaderProgram(AName);
  Result := prog.Link;
  if Result then
    MakeLocationCache(prog);
end;

procedure TGLShaderManager.UseProgram(const AName: string);
var
  prog: TGLSLShaderProgram;
begin
  SafeCurrentGLContext;
  try
    BeginWork;
    prog := GetShaderProgram(AName);

    if Assigned(prog) then
    begin
      if prog.FHandle.IsDataNeedUpdate then
      begin
        if not LinkShaderProgram(AName) then
          Abort;
      end;

      vCurrentProgram := prog;
      vCurrentProgram.FHandle.UseProgramObject;
    end
    else
    begin
      WorkLog.LogError('Used unknown program "' + AName + '"');
      Abort;
    end;

  finally
    EndWork;
  end;
end;

function TGLShaderManager.IsProgramDefined(const AName: string): Boolean;
begin
  Assert(vWorked, glsWrongMethodCall);
  Result := GetShaderProgram(AName) <> nil;
end;

function TGLShaderManager.IsProgramCurrent(const AName: string): Boolean;
begin
  if vCurrentProgram = nil then
    Result := false
  else
    Result := vCurrentProgram.FNameHashKey = ComputeNameHashKey(AName);
end;

function TGLShaderManager.GetCurrentProgram: string;
begin
  if vCurrentProgram = nil then
    Result := ''
  else
    Result := vCurrentProgram.FriendlyName;
end;

function TGLShaderManager.MakeUniqueObjectName(const NameRoot: string):
  string;
var
  I: Integer;
begin
  Assert(vWorked, glsWrongMethodCall);

  Result := NameRoot;
  if Length(Result) = 0 then
    Result := 'ShaderObj';
  I := 1;
  while GetShaderObject(Result) <> nil do
  begin
    Result := NameRoot + IntToStr(I);
    Inc(I);
  end;
end;

function TGLShaderManager.MakeUniqueProgramName(const NameRoot: string):
  string;
var
  I: Integer;
begin
  Assert(vWorked, glsWrongMethodCall);

  Result := NameRoot;
  if Length(Result) = 0 then
    Result := 'ShaderProg';
  I := 1;
  while GetShaderProgram(Result) <> nil do
  begin
    Result := NameRoot + IntToStr(I);
    Inc(I);
  end;
end;

procedure TGLShaderManager.UseFixedFunctionPipeline;
begin
  vCurrentProgram := nil;
  CurrentGLContext.GLStates.CurrentProgram := 0;
end;

procedure TGLShaderManager.DeleteShaderObject(AObject: TGLSLShaderObject);
begin
  Assert(vWorked, glsWrongMethodCall);

  if Assigned(AObject) then
  begin
    FShaderObjectsTree.Delete(AObject.FNameHashKey);
    AObject.Destroy;
  end
  else
    WorkLog.LogWarning('Attempt to delete a nil pointer of shader object');
end;

procedure TGLShaderManager.DeleteShaderProgram(AProgram: TGLSLShaderProgram);
begin
  Assert(vWorked, glsWrongMethodCall);

  if Assigned(AProgram) then
  begin
    FShaderProgramsTree.Delete(AProgram.FNameHashKey);
    AProgram.Destroy;
  end
  else
    WorkLog.LogWarning('Attempt to delete a nil pointer of shader program');
end;

procedure TGLShaderManager.DeleteShaderObject(const AName: string);
begin
  DeleteShaderObject(GetShaderObject(AName));
end;

procedure TGLShaderManager.DeleteShaderProgram(const AName: string);
begin
  DeleteShaderProgram(GetShaderProgram(AName));
end;

procedure TGLShaderManager.ClearShaderObject;
begin
  Assert(vWorked, glsWrongMethodCall);
  FShaderObjectsTree.ForEach(ObjectDestroyer);
  FShaderObjectsTree.Clear;
end;

procedure TGLShaderManager.ClearShaderPrograms;
begin
  Assert(vWorked, glsWrongMethodCall);
  FShaderProgramsTree.ForEach(ProgramDestroyer);
  FShaderProgramsTree.Clear;
end;

function TGLShaderManager.GetProgramAttribs(const AName: string; out Attribs: TGLSLAttributeArray): Boolean;
var
  prog: TGLSLShaderProgram;
  I, J, K: Integer;
begin
  Assert(vWorked, glsWrongMethodCall);
  prog := GetShaderProgram(AName);
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

procedure TGLShaderManager.Uniform1f(AUniform: TGLSLUniform; const
  Value: Single);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform1f(loc, Value);
end;

procedure TGLShaderManager.Uniform2f(AUniform: TGLSLUniform; const
  Value: TVector2f);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform2f(loc, Value[0], Value[1]);
end;

procedure TGLShaderManager.Uniform3f(AUniform: TGLSLUniform; const
  Value: TVector3f);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform3f(loc, Value[0], Value[1], Value[2]);
end;

procedure TGLShaderManager.Uniform4f(AUniform: TGLSLUniform; const
  Value: TVector4f);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform4f(loc, Value[0], Value[1], Value[2], Value[3]);
end;

procedure TGLShaderManager.Uniform1I(AUniform: TGLSLUniform; const
  Value: Integer);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform1i(loc, Value);
end;

procedure TGLShaderManager.Uniform1I(AUniform: TGLSLUniform;
  const Value: PGLInt; Count: Integer);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform1iv(loc, Count, Value);
end;

procedure TGLShaderManager.Uniform2I(AUniform: TGLSLUniform; const
  Value: TVector2I);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform2i(loc, Value[0], Value[1]);
end;

procedure TGLShaderManager.Uniform3I(AUniform: TGLSLUniform; const
  Value: TVector3I);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform3i(loc, Value[0], Value[1], Value[2]);
end;

procedure TGLShaderManager.Uniform4I(AUniform: TGLSLUniform; const
  Value: TVector4I);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform4i(loc, Value[0], Value[1], Value[2], Value[3]);
end;

procedure TGLShaderManager.Uniform4I(AUniform: TGLSLUniform; const
  Value: PGLInt; Count: Integer);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform4iv(loc, Count, Value);
end;

procedure TGLShaderManager.Uniform1UI(AUniform: TGLSLUniform; const
  Value: TGLuint);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform1ui(loc, Value);
end;

procedure TGLShaderManager.Uniform2UI(AUniform: TGLSLUniform; const
  Value: TVector2UI);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform2ui(loc, Value[0], Value[1]);
end;

procedure TGLShaderManager.Uniform3UI(AUniform: TGLSLUniform; const
  Value: TVector3UI);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform3ui(loc, Value[0], Value[1], Value[2]);
end;

procedure TGLShaderManager.Uniform4UI(AUniform: TGLSLUniform; const
  Value: TVector4UI);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform4ui(loc, Value[0], Value[1], Value[2], Value[3]);
end;

procedure TGLShaderManager.UniformMat2f(AUniform: TGLSLUniform; const
  Value: TMatrix2f);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.UniformMatrix2fv(loc, 1, False, @Value);
end;

procedure TGLShaderManager.UniformMat3f(AUniform: TGLSLUniform; const
  Value: TMatrix3f);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.UniformMatrix3fv(loc, 1, False, @Value);
end;

procedure TGLShaderManager.UniformMat4f(AUniform: TGLSLUniform; const
  Value: TMatrix4f);
var
  loc: GLInt;
begin
  loc := AUniform.Location;
  if loc > -1 then
    GL.UniformMatrix4fv(loc, 1, False, @Value);
end;

procedure TGLShaderManager.UniformSampler(AUniform: TGLSLUniform;
  const Texture: GLUInt; TexUnit: GLUInt);
var
  loc: GLInt;
  target: TGLTextureTarget;
begin
  loc := AUniform.Location;
  if (loc > -1) and GetTextureTarget(AUniform, target) then
  begin
    CurrentGLContext.GLStates.TextureBinding[TexUnit, target] := Texture;
    GL.Uniform1i(loc, TexUnit);
  end;
end;

//procedure TGLShadersManager.UniformBuffer(const ProgramName: string; AUniform: TGLSLUniform; const Buffer: GLUInt);
//var
//  RC: TGLContext;
//  prog: TGLSLShaderProgram;
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
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

initialization
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
  uniformLightWorldPosition
    := TGLSLUniform.RegisterUniform('LightWorldPosition');
  uniformDiffuse
    := TGLSLUniform.RegisterUniform('Diffuse');
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

  FreeAndNil(vShaderManager);
  ClearRegistries;

end.

