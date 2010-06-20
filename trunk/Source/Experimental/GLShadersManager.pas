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

unit GLShadersManager;

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
  OpenGL1x,
  GLContext,
  GLState,
  GLTextureFormat,
  VectorLists,
  VectorTypes,
  GLSRedBlackTree,
  GLSLog;

const
  GLS_MAX_SHADER_PROGRAM = 256;

resourcestring
  glsOutOfMaxAttrib = 'Necessary to increase the constant GLS_VERTEX_ATTR_NUM';
  glsOutOfMaxShader =
    'Necessary to increase the constant GLS_MAX_SHADER_PROGRAM';

type

  TGLSLProgramType = (ptVertex, ptGeometry, ptFragment);
  TGLSLProgramTypes = set of TGLSLProgramType;

  TGLSLDataType = (
    GLSLTypeUndefined,
    GLSLTypeCustom,
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
    GLSLTypeUIntSamplerMSArray
    );

  // TGLSLAttribute
  //

  TGLSLAttribute = class
  private
    FName: string;
    FLocation: array[1..GLS_MAX_SHADER_PROGRAM] of GLInt;
    FDataType: array[1..GLS_MAX_SHADER_PROGRAM] of TGLSLDataType;
    FWarningAbsenceLoged: array[1..GLS_MAX_SHADER_PROGRAM] of Boolean;
    FTag: Integer;
    function GetLocation: GLInt;
    procedure SetLocation(Index: GLInt);
    function GetDataType: TGLSLDataType;
  public
    constructor Create;
    procedure BeforeDestruction; override;
    constructor RegisterAttribute(const AName: string);
    class function GetAttribute(const AName: string): TGLSLAttribute;
    procedure ResetLocationCash;
    property Name: string read FName;
    property Location: GLInt read GetLocation write SetLocation;
    property DataFormat: TGLSLDataType read GetDataType;
    property Tag: Integer read FTag write FTag;
  end;

  TGLSLAttributeArray = array[0..GLS_VERTEX_ATTR_NUM - 1] of TGLSLAttribute;

  // TGLSLUniform
  //

  TGLSLUniform = class
  private
    { Private Declarations }
    FName: string;
    FLocation: array[1..GLS_MAX_SHADER_PROGRAM] of GLInt;
    FDataType: array[1..GLS_MAX_SHADER_PROGRAM] of TGLSLDataType;
    FWarningAbsenceLoged: array[1..GLS_MAX_SHADER_PROGRAM] of Boolean;
    FTag: Integer;
    function GetLocation: GLInt;
    function GetDataType: TGLSLDataType;
  public
    { Public Declarations }
    constructor Create;
    procedure BeforeDestruction; override;
    constructor RegisterUniform(const AName: string);
    class function GetUniform(const AName: string): TGLSLUniform;
    procedure ResetLocationCash;
    property Name: string read FName;
    property Location: GLInt read GetLocation;
    property DataType: TGLSLDataType read GetDataType;
    property Tag: Integer read FTag write FTag;
  end;

  TGLSLUniformClass = class of TGLSLUniform;

  // TGLSLUniformBBeginWork
  //

  TGLSLUniformBlock = class
  private
    { Private Declarations }
    FName: string;
    FLocation: array[1..GLS_MAX_SHADER_PROGRAM] of GLInt;
    FWarningAbsenceLoged: array[1..GLS_MAX_SHADER_PROGRAM] of Boolean;
    FSize: GLSizei;
    FUniforms: TList;
    FTag: Integer;
    function GetLocation: GLInt;
  public
    { Public Declarations }
    constructor Create;
    procedure BeforeDestruction; override;
    constructor RegisterUniformBlock(const AName: string);
    class function GetUniformBlock(const AName: string): TGLSLUniformBlock;
    procedure ResetLocationCash;
    property Name: string read FName;
    property Location: GLInt read GetLocation;
    property DataSize: GLSizei read FSize;
    property Tag: Integer read FTag write FTag;
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
    FCompiled: Boolean;
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
    FLinked: Boolean;
    FAttachedObjects: TList{$IFNDEF FPC} < TGLSLShaderObject > {$ENDIF};
  public
    constructor Create;
    destructor Destroy; override;
    {: Attach objects and link program under current context }
    function Link: Boolean;
    procedure Detach(AObject: TGLSLShaderObject);
    property FriendlyName: string read FFriendlyName;
  end;

  TShaderObjectTree =
{$IFDEF FPC}specialize{$ENDIF}GRedBlackTree < Integer, TGLSLShaderObject > ;
  TShaderProgramTree =
{$IFDEF FPC}specialize{$ENDIF}GRedBlackTree < Integer, TGLSLShaderProgram > ;

  TGLShadersManager = class
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
    procedure CashLocations(ProgramID: GLUInt);
    function ComputeNameHashKey(const AName: string): Integer;
  public
    { Public Declarations }
    constructor Create;
    destructor Destroy; override;
    {: Call this method before begin work with programs and objects. }
    procedure BeginWork; //inline;

    procedure DefineShaderObject(const AName: string; const code: AnsiString;
      ATypes: TGLSLProgramTypes);
    procedure DefineShaderProgram(const AName: string);
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
    {: End of work with programs and objects. }
    procedure EndWork; //inline;

    function MakeUniqueObjectName(const NameRoot: string): string;
    function MakeUniqueProgramName(const NameRoot: string): string;
    {: May called once after TGLSLShader.Apply
       for cashing shader variables location }
    procedure CashLocationsOfGLSLShader;

    procedure Uniform1f(AUniform: TGLSLUniform; const Value: Single);
    procedure Uniform2f(AUniform: TGLSLUniform; const Value: TVector2f);
    procedure Uniform3f(AUniform: TGLSLUniform; const Value: TVector3f);
    procedure Uniform4f(AUniform: TGLSLUniform; const Value: TVector4f);
    procedure Uniform1I(AUniform: TGLSLUniform; const Value: Integer);
      overload;
    procedure Uniform1I(AUniform: TGLSLUniform; const Value: PGLInt;
      Count: Integer);
      overload;
    procedure Uniform2I(AUniform: TGLSLUniform; const Value: TVector2I);
    procedure Uniform3I(AUniform: TGLSLUniform; const Value: TVector3I);
    procedure Uniform4I(AUniform: TGLSLUniform; const Value: TVector4I);
      overload;
    procedure Uniform4I(AUniform: TGLSLUniform; const Value: PGLInt;
      Count: Integer);
      overload;
    procedure UniformMat2f(AUniform: TGLSLUniform; const Value:
      TMatrix2f);
    procedure UniformMat3f(AUniform: TGLSLUniform; const Value:
      TMatrix3f);
    procedure UniformMat4f(AUniform: TGLSLUniform; const Value:
      TMatrix4f);
    procedure UniformSampler(AUniform: TGLSLUniform; const Texture:
      GLUInt; TexUnit: GLUInt);
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
    attrTangent,
    attrBinormal,
    attrIndex: TGLSLAttribute;

  uniformModelMatrix,
    uniformViewProjectionMatrix,
    uniformLightSourcePos,
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

function ShadersManager: TGLShadersManager;

implementation

uses
  GLStrings;

resourcestring
  glsWrongMethodCall =
    'This method must be called between BeginWork...EndWork!';

var
  vShadersManager: TGLShadersManager;

{$IFDEF GLS_MULTITHREAD}
threadvar
{$ENDIF}
  vCurrentProgram: TGLSLShaderProgram;
  vWorked: Boolean;

function ShadersManager: TGLShadersManager;
begin
  if vShadersManager = nil then
    vShadersManager := TGLShadersManager.Create;
  Result := vShadersManager;
end;

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

function CompareInteger(const Item1, Item2: Integer): Integer;
begin
  if Item1 < Item2 then
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

// TGLSLAttribute
//

constructor TGLSLAttribute.Create;
begin
  raise
    Exception.Create('Use RegisterAttribute to create instance of TGLSLAttribute');
end;

constructor TGLSLAttribute.RegisterAttribute(const AName: string);
var
  I: Integer;
begin
  for I := 0 to High(AttributeRegistry) do
  begin
    if AttributeRegistry[I].FName = AName then
    begin
      Self.Destroy;
      Self := AttributeRegistry[I];
      exit;
    end;
  end;
  SetLength(AttributeRegistry, Length(AttributeRegistry) + 1);
  AttributeRegistry[High(AttributeRegistry)] := Self;
  FName := AName;
  ResetLocationCash;
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
  Prog: GLUint;
begin
  Result := -1;
  Prog := CurrentGLContext.GLStates.CurrentProgram;
  if Prog = 0 then
    exit;
  if Prog > GLS_MAX_SHADER_PROGRAM then
  begin
    ShadersManager.WorkLog.LogFatalError(glsOutOfMaxShader);
    exit;
  end;

  if FLocation[Prog] = -1 then
  begin
    if not FWarningAbsenceLoged[Prog] then
    begin
      ShadersManager.WorkLog.LogWarning('Using an unknown attribute "' + FName +
        '"');
      FWarningAbsenceLoged[Prog] := True;
    end;
  end
  else
    Result := FLocation[Prog];
end;

procedure TGLSLAttribute.SetLocation(Index: GLInt);
var
  Prog: GLUint;
begin
  if Index > GLS_VERTEX_ATTR_NUM - 1 then
  begin
    ShadersManager.WorkLog.LogError('Location Index greater then GLS_VERTEX_ATTR_NUM');
    exit;
  end;
  Prog := CurrentGLContext.GLStates.CurrentProgram;
  if Prog = 0 then
    exit;
  if Prog > GLS_MAX_SHADER_PROGRAM then
  begin
    ShadersManager.WorkLog.LogFatalError(glsOutOfMaxShader);
    exit;
  end;
  glBindAttribLocation(Prog, Index, PGLChar(TGLString(FName)));
  FLocation[Prog] := Index;
end;

function TGLSLAttribute.GetDataType: TGLSLDataType;
var
  Prog: GLUint;
begin
  Result := GLSLTypeUndefined;
  Prog := CurrentGLContext.GLStates.CurrentProgram;
  if Prog = 0 then
    exit;
  if Prog > GLS_MAX_SHADER_PROGRAM then
  begin
    ShadersManager.WorkLog.LogFatalError(glsOutOfMaxShader);
    exit;
  end;
  Result := FDataType[Prog];
end;

procedure TGLSLAttribute.ResetLocationCash;
var
  I: Integer;
begin
  for I := Low(FLocation) to High(FLocation) - 1 do
  begin
    FLocation[I] := -1;
    FDataType[I] := GLSLTypeUndefined;
    FWarningAbsenceLoged[I] := False;
  end;
end;

// TGLSLUniform
//

constructor TGLSLUniform.Create;
begin
  raise
    Exception.Create('Use RegisterUniform to create instance of TGLSLUniform');
end;

constructor TGLSLUniform.RegisterUniform(const AName: string);
var
  I: Integer;
begin
  for I := 0 to High(UniformRegistry) do
  begin
    if UniformRegistry[I].FName = AName then
    begin
      Self.Destroy;
      Self := UniformRegistry[I];
      exit;
    end;
  end;
  SetLength(UniformRegistry, Length(UniformRegistry) + 1);
  UniformRegistry[High(UniformRegistry)] := Self;
  FName := AName;
  ResetLocationCash;
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
    if UniformRegistry[I].Name = AName then
    begin
      Result := UniformRegistry[I];
      exit;
    end;
  end;
  Result := nil;
end;

function TGLSLUniform.GetLocation: GLInt;
var
  Prog: GLUint;
begin
  Result := -1;
  Prog := CurrentGLContext.GLStates.CurrentProgram;
  if Prog = 0 then
    exit;
  if Prog > GLS_MAX_SHADER_PROGRAM then
  begin
    ShadersManager.WorkLog.LogFatalError(glsOutOfMaxShader);
    exit;
  end;

  if FLocation[Prog] = -1 then
  begin
    if not FWarningAbsenceLoged[Prog] then
    begin
      ShadersManager.WorkLog.LogWarning('Using an unknown uniform "' + FName +
        '" in program "' + vCurrentProgram.FFriendlyName + '"');
      FWarningAbsenceLoged[Prog] := True;
    end;
  end
  else
    Result := FLocation[Prog];
end;

function TGLSLUniform.GetDataType: TGLSLDataType;
var
  Prog: GLUint;
begin
  Result := GLSLTypeUndefined;
  Prog := CurrentGLContext.GLStates.CurrentProgram;
  if Prog = 0 then
    exit;
  if Prog > GLS_MAX_SHADER_PROGRAM then
  begin
    ShadersManager.WorkLog.LogFatalError(glsOutOfMaxShader);
    exit;
  end;
  Result := FDataType[Prog];
end;

procedure TGLSLUniform.ResetLocationCash;
var
  I: Integer;
begin
  for I := Low(FLocation) to High(FLocation) - 1 do
  begin
    FLocation[I] := -1;
    FDataType[I] := GLSLTypeUndefined;
    FWarningAbsenceLoged[I] := False;
  end;
end;

// TGLSLUniformBlock
//

constructor TGLSLUniformBlock.Create;
begin
  raise
    Exception.Create('Use RegisterUniformBlock to create instance of TGLSLUniformBlock');
end;

constructor TGLSLUniformBlock.RegisterUniformBlock(const AName: string);
var
  I: Integer;
begin
  for I := 0 to High(UniformBlockRegistry) do
  begin
    if UniformBlockRegistry[I].FName = AName then
    begin
      Self.Destroy;
      Self := UniformBlockRegistry[I];
      exit;
    end;
  end;
  SetLength(UniformBlockRegistry, Length(UniformBlockRegistry) + 1);
  UniformBlockRegistry[High(UniformBlockRegistry)] := Self;
  FName := AName;
  FUniforms := TList.Create;
  ResetLocationCash;
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
  Prog: GLUint;
begin
  Result := -1;
  Prog := CurrentGLContext.GLStates.CurrentProgram;
  if Prog = 0 then
    exit;
  if Prog > GLS_MAX_SHADER_PROGRAM then
  begin
    ShadersManager.WorkLog.LogFatalError(glsOutOfMaxShader);
    exit;
  end;

  if FLocation[Prog] = -1 then
  begin
    if not FWarningAbsenceLoged[Prog] then
    begin
      ShadersManager.WorkLog.LogWarning('Using an unknown uniform bBeginWork"' +
        FName
        + '"');
      FWarningAbsenceLoged[Prog] := True;
    end;
  end
  else
    Result := FLocation[Prog];
end;

procedure TGLSLUniformBlock.ResetLocationCash;
var
  I: Integer;
begin
  for I := Low(FLocation) to High(FLocation) - 1 do
  begin
    FLocation[I] := -1;
    FWarningAbsenceLoged[I] := False;
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
begin
end;

destructor TGLSLShaderObject.Destroy;
var
  P: TGLSLProgramType;
begin
  inherited;
  for P := low(TGLSLProgramType) to high(TGLSLProgramType) do
    FHandles[P].Free;
end;

function TGLSLShaderObject.Compile: Boolean;
const
  cObjectClass: array[TGLSLProgramType] of TGLShaderHandleClass =
    (TGLVertexShaderHandle, TGLGeometryShaderHandle, TGLFragmentShaderHandle);
  cObjectTypeName: array[TGLSLProgramType] of string =
    ('Vertex', 'Geomtery', 'Fragment');
var
  RC: TGLContext;
  P: TGLSLProgramType;
  val, len: Integer;
  pLog: PAnsiChar;
  logstr: string;
begin
  Result := False;
  if not SafeCurrentGLContext(RC) then
    exit;

  pLog := nil;
  for P := Low(TGLSLProgramType) to High(TGLSLProgramType) do
    if P in FTypes then
    begin
      // Create and allocate handle associated with current context
      if not Assigned(FHandles[P]) then
        FHandles[P] := cObjectClass[P].Create;
      if FHandles[P].Handle = 0 then
        FHandles[P].AllocateHandle;
      // Compile object
      FHandles[P].ShaderSource(FCode);
      FCompiled := FHandles[P].CompileShader;
      Result := FCompiled;

      logstr := cObjectTypeName[P] + ' shader object "' +
        FriendlyName + '" compilation - ';

      if Result then
      begin
        vShadersManager.CompilationLog.LogInfo(logstr + ' Successful');
        FHandles[P].NotifyDataUpdated;
      end
      else begin
        vShadersManager.CompilationLog.LogError(logstr + ' Failed!');
        FHandles[P].NotifyChangesOfData;
      end;

      GL.GetShaderiv(FHandles[P].Handle, GL_INFO_LOG_LENGTH, @val);
      if val > 1 then
      begin
        ReallocMem(pLog, val);
        GL.GetShaderInfoLog(FHandles[P].Handle, val, @len, pLog);
        vShadersManager.CompilationLog.LogInfo(string(pLog));
      end;
    end
    else if Assigned(FHandles[P]) then
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
  FLinked := False;
end;

destructor TGLSLShaderProgram.Destroy;
begin
  inherited;
  FHandle.Free;
  FAttachedObjects.Free;
end;

function TGLSLShaderProgram.Link: Boolean;
var
  RC: TGLContext;
  P: TGLSLProgramType;
  obj: TGLSLShaderObject;
  I: Integer;
  val, len: GLsizei;
  pLog: PAnsiChar;
  logstr: string;
begin
  Result := False;
  if not SafeCurrentGLContext(RC) then
    exit;

  if FHandle.Handle = 0 then
    FHandle.AllocateHandle;

  for I := 0 to FAttachedObjects.Count - 1 do
  begin
{$IFDEF FPC}
    obj := TGLSLShaderObject(FAttachedObjects.Items[I]);
{$ELSE}
    obj := FAttachedObjects.Items[I];
{$ENDIF};
    for P := low(TGLSLProgramType) to high(TGLSLProgramType) do
      if P in obj.FTypes then
      begin
        if Assigned(obj.FHandles[P]) and not (obj.FHandles[P].IsDataNeedUpdate) then
          GL.AttachShader(FHandle.Handle, obj.FHandles[P].Handle)
        else
        begin
          if obj.Compile then
            GL.AttachShader(FHandle.Handle, obj.FHandles[P].Handle)
          else
            exit;
        end;
      end;
  end;

  Result := FHandle.LinkProgram;
  FLinked := Result;

  logstr := 'Shader Program ' + FriendlyName + ' Linking - ';
  if FLinked then
  begin
    vShadersManager.CompilationLog.LogInfo(logstr + ' Successful');
    FHandle.NotifyDataUpdated;
  end
  else
    vShadersManager.CompilationLog.LogError(logstr + ' Failed!');

  GL.GetProgramiv(FHandle.Handle, GL_INFO_LOG_LENGTH, @val);
  if val > 0 then
  begin
    GetMem(pLog, val);
    GL.GetProgramInfoLog(FHandle.Handle, val, @len, pLog);
    vShadersManager.CompilationLog.LogInfo(string(pLog));
    FreeMem(pLog, val);
  end;

  if FLinked and (FHandle.Handle > GLS_MAX_SHADER_PROGRAM) then
  begin
    vShadersManager.WorkLog.LogFatalError(glsOutOfMaxShader);
    exit;
  end;

end;

procedure TGLSLShaderProgram.Detach(AObject: TGLSLShaderObject);
var
  RC: TGLContext;
  P: TGLSLProgramType;
begin
  if not SafeCurrentGLContext(RC) then
    exit;

  if FHandle.Handle <> 0 then
    for P := low(TGLSLProgramType) to high(TGLSLProgramType) do
      if P in AObject.FTypes then
      begin
        if Assigned(AObject.FHandles[P]) and (AObject.FHandles[P].Handle <> 0) then
        begin
          GL.DetachShader(FHandle.Handle, AObject.FHandles[P].Handle);
          FHandle.NotifyChangesOfData;
        end;
      end;
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

constructor TGLShadersManager.Create;
var
  LogPath: string;
begin
  FShaderObjectsTree := TShaderObjectTree.Create(CompareInteger);
  FShaderProgramsTree := TShaderProgramTree.Create(CompareInteger);

  LogPath := ExtractFilePath(ParamStr(0));
  CompilationLog
    := TLogSession.Init(LogPath + 'ShadersCompilation.log', lfNone,
    [lkNotice, lkInfo, lkWarning, lkError]);
  WorkLog
    := TLogSession.Init(LogPath + 'ShadersWork.log', lfNone,
    [lkWarning, lkError]);
  vWorked := False;
{$IFDEF GLS_MULTITHREAD}
  InitializeCriticalSection(FLock);
{$ENDIF}
end;

destructor TGLShadersManager.Destroy;
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

procedure TGLShadersManager.BeginWork;
begin
{$IFDEF GLS_MULTITHREAD}
  EnterCriticalSection(FLock);
  //  GLSLogger.LogDebug('ShadersManager.BeginWork');
{$ENDIF}
  Assert(not vWorked, 'Excessive call BeginWork');
  vWorked := True;
end;

procedure TGLShadersManager.EndWork;
begin
{$IFDEF GLS_MULTITHREAD}
  //  GLSLogger.LogDebug('ShadersManager.EndWork');
  LeaveCriticalSection(FLock);
{$ENDIF}
  Assert(vWorked, 'Excessive call EndWork');
  vWorked := False;
end;

function TGLShadersManager.GetShaderObject(const AName: string):
  TGLSLShaderObject;
begin
  Assert(vWorked, glsWrongMethodCall);
  if not FShaderObjectsTree.Find(ComputeNameHashKey(AName), Result) then
    Result := nil;
end;

function TGLShadersManager.GetShaderProgram(const AName: string):
  TGLSLShaderProgram;
begin
  Assert(vWorked, glsWrongMethodCall);
  if not FShaderProgramsTree.Find(ComputeNameHashKey(AName), Result) then
    Result := nil;
end;

function TGLShadersManager.ComputeNameHashKey(const AName: string): Integer;
var
  i, n: Integer;
begin
  n := Length(AName);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(AName[i]);
end;

function TGLShadersManager.GetTextureTarget(const AUniform: TGLSLUniform;
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

procedure TGLShadersManager.DefineShaderObject(
  const AName: string;
  const Code: AnsiString;
  ATypes: TGLSLProgramTypes);
var
  Obj: TGLSLShaderObject;
  newCode: Boolean;
begin
  Assert(vWorked, glsWrongMethodCall);
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
    newCode := CompareStr(Obj.FCode, string(Code)) <> 0;

  if newCode then
    with Obj do
    begin
      FTypes := ATypes;
      FCode := string(Code);
      Compile;
    end;
end;

procedure TGLShadersManager.DefineShaderProgram(const AName: string);
var
  Prog: TGLSLShaderProgram;
begin
  Assert(vWorked, glsWrongMethodCall);

  Prog := GetShaderProgram(AName);
  if not Assigned(Prog) then
  begin
    Prog := TGLSLShaderProgram.Create;
    Prog.FFriendlyName := AName;
    Prog.FNameHashKey := ComputeNameHashKey(AName);
    FShaderProgramsTree.Add(Prog.FNameHashKey, Prog);
  end;
end;

procedure TGLShadersManager.AttachShaderObjectToProgram(
  const AObject: string; const AProgram: string);
var
  obj: TGLSLShaderObject;
  Prog: TGLSLShaderProgram;
begin
  Assert(vWorked, glsWrongMethodCall);

  Prog := GetShaderProgram(AProgram);
  obj := GetShaderObject(AObject);
  if Prog.FAttachedObjects.IndexOf(obj) < 0 then
  begin
    Prog.FAttachedObjects.Add(obj);
    Prog.FHandle.NotifyChangesOfData;
  end
  else
    Prog.Detach(obj);

  Prog.FLinked := False;
end;

procedure TGLShadersManager.DetachShaderObjectFromProgram(
  const AObject: string; const AProgram: string);
var
  obj: TGLSLShaderObject;
  Prog: TGLSLShaderProgram;
begin
  Assert(vWorked, glsWrongMethodCall);

  obj := GetShaderObject(AObject);
  Prog := GetShaderProgram(AProgram);
  if Prog.FAttachedObjects.IndexOf(obj) >= 0 then
  begin
    Prog.FAttachedObjects.Remove(obj);
    Prog.Detach(obj);
    Prog.FLinked := False;
  end
  else
    CompilationLog.LogWarning('Unable to detach shader object "'
      + obj.FriendlyName + '" from shader program "' + Prog.FriendlyName + '"');
end;

procedure TGLShadersManager.CashLocationsOfGLSLShader;
var
  Prog: GLUInt;
begin
  Assert(vWorked, glsWrongMethodCall);

  Prog := CurrentGLContext.GLStates.CurrentProgram;
  if Prog > 0 then
  begin
    CompilationLog.LogNotice('Cashing variables location of TGLSLShader');
    CashLocations(Prog);
  end;
end;

procedure TGLShadersManager.CashLocations(ProgramID: GLUInt);
var
  i: Integer;
  buff: array[0..127] of AnsiChar;
  max: GLInt;
  Size: GLInt;
  len: GLsizei;
  AType: GLenum;
  Attr: TGLSLAttribute;
  Uniform: TGLSLUniform;
  UniformBlock: TGLSLUniformBlock;
begin
  Assert(vWorked, glsWrongMethodCall);
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
        FLocation[ProgramID] :=
          GL.GetAttribLocation(ProgramID, PGLChar(TGLString(Name)));
        case AType of
          GL_FLOAT: FDataType[ProgramID] := GLSLType1F;
          GL_FLOAT_VEC2: FDataType[ProgramID] := GLSLType2F;
          GL_FLOAT_VEC3: FDataType[ProgramID] := GLSLType3F;
          GL_FLOAT_VEC4: FDataType[ProgramID] := GLSLType4F;
          GL_INT: FDataType[ProgramID] := GLSLType1I;
          GL_INT_VEC2: FDataType[ProgramID] := GLSLType2I;
          GL_INT_VEC3: FDataType[ProgramID] := GLSLType3I;
          GL_INT_VEC4: FDataType[ProgramID] := GLSLType4I;
          GL_BOOL: FDataType[ProgramID] := GLSLType1I;
          GL_BOOL_VEC2: FDataType[ProgramID] := GLSLType2I;
          GL_BOOL_VEC3: FDataType[ProgramID] := GLSLType3I;
          GL_BOOL_VEC4: FDataType[ProgramID] := GLSLType4I;
          GL_FLOAT_MAT2: FDataType[ProgramID] := GLSLTypeMat2F;
          GL_FLOAT_MAT3: FDataType[ProgramID] := GLSLTypeMat3F;
          GL_FLOAT_MAT4: FDataType[ProgramID] := GLSLTypeMat4F;
        else
          begin
            FDataType[ProgramID] := GLSLTypeUndefined;
            CompilationLog.LogError('Active attribute ' + Name + ' with ' +
              glsUnknownType);
          end;
        end;
      end;
    end
    else
      CompilationLog.LogWarning('Active attribute ' + Copy(string(buff), 0, len)
        +
        ' not registered');
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
      UniformBlock :=
        TGLSLUniformBlock.GetUniformBlock(Copy(string(buff), 0, len));

      if Assigned(UniformBlock) then
        with UniformBlock do
        begin
          CompilationLog.LogInfo('Detected active uniform Block: ' + Name);
          FLocation[ProgramID] :=
            GL.GetUniformBlockIndex(ProgramID, PGLChar(TGLString(Name)));
          GL.GetActiveUniformBlockiv(ProgramID, FLocation[ProgramID],
            GL_UNIFORM_BLOCK_DATA_SIZE, @FSize);
        end;
    end;
  end;

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
        FLocation[ProgramID] :=
          GL.GetUniformLocation(ProgramID, PGLChar(TGLString(Name)));
        case AType of
          GL_FLOAT: FDataType[ProgramID] := GLSLType1F;
          GL_FLOAT_VEC2: FDataType[ProgramID] := GLSLType2F;
          GL_FLOAT_VEC3: FDataType[ProgramID] := GLSLType3F;
          GL_FLOAT_VEC4: FDataType[ProgramID] := GLSLType4F;
          GL_INT: FDataType[ProgramID] := GLSLType1I;
          GL_INT_VEC2: FDataType[ProgramID] := GLSLType2I;
          GL_INT_VEC3: FDataType[ProgramID] := GLSLType3I;
          GL_INT_VEC4: FDataType[ProgramID] := GLSLType4I;
          GL_BOOL: FDataType[ProgramID] := GLSLType1I;
          GL_BOOL_VEC2: FDataType[ProgramID] := GLSLType2I;
          GL_BOOL_VEC3: FDataType[ProgramID] := GLSLType3I;
          GL_BOOL_VEC4: FDataType[ProgramID] := GLSLType4I;
          GL_FLOAT_MAT2: FDataType[ProgramID] := GLSLTypeMat2F;
          GL_FLOAT_MAT3: FDataType[ProgramID] := GLSLTypeMat3F;
          GL_FLOAT_MAT4: FDataType[ProgramID] := GLSLTypeMat4F;
          GL_SAMPLER_1D: FDataType[ProgramID] := GLSLTypeSampler1D;
          GL_SAMPLER_2D: FDataType[ProgramID] := GLSLTypeSampler2D;
          GL_SAMPLER_3D: FDataType[ProgramID] := GLSLTypeSampler3D;
          GL_SAMPLER_CUBE: FDataType[ProgramID] := GLSLTypeSamplerCube;
          GL_SAMPLER_1D_SHADOW: FDataType[ProgramID] := GLSLTypeSampler1DShadow;
          GL_SAMPLER_2D_SHADOW: FDataType[ProgramID] := GLSLTypeSampler2DShadow;
          GL_SAMPLER_2D_RECT: FDataType[ProgramID] := GLSLTypeSamplerRect;
          GL_SAMPLER_2D_RECT_SHADOW: FDataType[ProgramID] :=
            GLSLTypeSamplerRectShadow;
          GL_SAMPLER_BUFFER: FDataType[ProgramID] := GLSLTypeSamplerBuffer;
          GL_INT_SAMPLER_2D_RECT: FDataType[ProgramID] :=
            GLSLTypeIntSamplerRect;
          GL_INT_SAMPLER_BUFFER: FDataType[ProgramID] :=
            GLSLTypeIntSamplerBuffer;
          GL_UNSIGNED_INT_SAMPLER_1D: FDataType[ProgramID] :=
            GLSLTypeUIntSampler1D;
          GL_UNSIGNED_INT_SAMPLER_2D: FDataType[ProgramID] :=
            GLSLTypeUIntSampler2D;
          GL_UNSIGNED_INT_SAMPLER_3D: FDataType[ProgramID] :=
            GLSLTypeUIntSampler3D;
          GL_UNSIGNED_INT_SAMPLER_CUBE: FDataType[ProgramID] :=
            GLSLTypeUIntSamplerCube;
          GL_UNSIGNED_INT_SAMPLER_1D_ARRAY: FDataType[ProgramID] :=
            GLSLTypeUIntSampler1DArray;
          GL_UNSIGNED_INT_SAMPLER_2D_ARRAY: FDataType[ProgramID] :=
            GLSLTypeUIntSampler2DArray;
          GL_UNSIGNED_INT_SAMPLER_2D_RECT: FDataType[ProgramID] :=
            GLSLTypeUIntSamplerRect;
          GL_UNSIGNED_INT_SAMPLER_BUFFER: FDataType[ProgramID] :=
            GLSLTypeUIntSamplerBuffer;
          GL_SAMPLER_2D_MULTISAMPLE: FDataType[ProgramID] :=
            GLSLTypeSamplerMS;
          GL_INT_SAMPLER_2D_MULTISAMPLE: FDataType[ProgramID] :=
            GLSLTypeIntSamplerMS;
          GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE: FDataType[ProgramID] :=
            GLSLTypeUIntSamplerMS;
          GL_SAMPLER_2D_MULTISAMPLE_ARRAY: FDataType[ProgramID] :=
            GLSLTypeSamplerMSArray;
          GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY: FDataType[ProgramID] :=
            GLSLTypeIntSamplerMSArray;
          GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY: FDataType[ProgramID] :=
            GLSLTypeUIntSamplerMSArray;
        else
          begin
            FDataType[ProgramID] := GLSLTypeUndefined;
            CompilationLog.LogError('Active uniform ' + Name + ' with ' +
              glsUnknownType);
          end;
        end;
      end;
    end
    else
      CompilationLog.LogWarning('Active uniform ' + Copy(string(buff), 0, len) +
        ' not registered');
  end;

  vShadersManager.CompilationLog.LogNotice('');
end;

function TGLShadersManager.LinkShaderProgram(const AName: string): Boolean;
var
  RC: TGLContext;
  prog: TGLSLShaderProgram;
begin
  Assert(vWorked, glsWrongMethodCall);

  Result := False;
  if SafeCurrentGLContext(RC) then
  begin
    prog := GetShaderProgram(AName);
    Result := prog.Link;
    if Result then
      CashLocations(prog.FHandle.Handle);
  end;
end;

procedure TGLShadersManager.UseProgram(const AName: string);
var
  RC: TGLContext;
  prog: TGLSLShaderProgram;
  bLinked: TGLboolean;
begin
  if SafeCurrentGLContext(RC) then
  begin
    BeginWork;
    try
      prog := GetShaderProgram(AName);

      if Assigned(prog) then
      begin
        if prog.FHandle.IsDataNeedUpdate then
        begin
          if not LinkShaderProgram(AName) then
            if not prog.Link then
              Abort;
        end;



        vCurrentProgram := prog;
        prog.FHandle.UseProgramObject;
      end
      else
        WorkLog.LogError('Used unknown program "' + AName + '"');

    finally
      EndWork;
    end;
  end;
end;

function TGLShadersManager.IsProgramDefined(const AName: string): Boolean;
begin
  Assert(vWorked, glsWrongMethodCall);
  Result := GetShaderProgram(AName) <> nil;
end;

function TGLShadersManager.IsProgramCurrent(const AName: string): Boolean;
begin
  if vCurrentProgram = nil then
    Result := false
  else
    Result := vCurrentProgram.FNameHashKey = ComputeNameHashKey(AName);
end;

function TGLShadersManager.GetCurrentProgram: string;
begin
  if vCurrentProgram = nil then
    Result := ''
  else
    Result := vCurrentProgram.FriendlyName;
end;

function TGLShadersManager.MakeUniqueObjectName(const NameRoot: string):
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

function TGLShadersManager.MakeUniqueProgramName(const NameRoot: string):
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

procedure TGLShadersManager.UseFixedFunctionPipeline;
begin
  vCurrentProgram := nil;
  CurrentGLContext.GLStates.CurrentProgram := 0;
end;

procedure TGLShadersManager.DeleteShaderObject(AObject: TGLSLShaderObject);
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

procedure TGLShadersManager.DeleteShaderProgram(AProgram: TGLSLShaderProgram);
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

procedure TGLShadersManager.DeleteShaderObject(const AName: string);
begin
  DeleteShaderObject(GetShaderObject(AName));
end;

procedure TGLShadersManager.DeleteShaderProgram(const AName: string);
begin
  DeleteShaderProgram(GetShaderProgram(AName));
end;

procedure TGLShadersManager.ClearShaderObject;
begin
  Assert(vWorked, glsWrongMethodCall);
  FShaderObjectsTree.ForEach(ObjectDestroyer);
  FShaderObjectsTree.Clear;
end;

procedure TGLShadersManager.ClearShaderPrograms;
begin
  Assert(vWorked, glsWrongMethodCall);
  FShaderProgramsTree.ForEach(ProgramDestroyer);
  FShaderProgramsTree.Clear;
end;

procedure TGLShadersManager.Uniform1f(AUniform: TGLSLUniform; const
  Value: Single);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform1f(loc, Value);
end;

procedure TGLShadersManager.Uniform2f(AUniform: TGLSLUniform; const
  Value: TVector2f);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform2f(loc, Value[0], Value[1]);
end;

procedure TGLShadersManager.Uniform3f(AUniform: TGLSLUniform; const
  Value: TVector3f);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform3f(loc, Value[0], Value[1], Value[2]);
end;

procedure TGLShadersManager.Uniform4f(AUniform: TGLSLUniform; const
  Value: TVector4f);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform4f(loc, Value[0], Value[1], Value[2], Value[3]);
end;

procedure TGLShadersManager.Uniform1I(AUniform: TGLSLUniform; const
  Value: Integer);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform1i(loc, Value);
end;

procedure TGLShadersManager.Uniform1I(AUniform: TGLSLUniform;
  const Value: PGLInt; Count: Integer);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform1iv(loc, Count, Value);
end;

procedure TGLShadersManager.Uniform2I(AUniform: TGLSLUniform; const
  Value: TVector2I);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform2i(loc, Value[0], Value[1]);
end;

procedure TGLShadersManager.Uniform3I(AUniform: TGLSLUniform; const
  Value: TVector3I);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform3i(loc, Value[0], Value[1], Value[2]);
end;

procedure TGLShadersManager.Uniform4I(AUniform: TGLSLUniform; const
  Value: TVector4I);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform4i(loc, Value[0], Value[1], Value[2], Value[3]);
end;

procedure TGLShadersManager.Uniform4I(AUniform: TGLSLUniform; const
  Value: PGLInt; Count: Integer);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.Uniform4iv(loc, Count, Value);
end;

procedure TGLShadersManager.UniformMat2f(AUniform: TGLSLUniform; const
  Value: TMatrix2f);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.UniformMatrix2fv(loc, 1, False, @Value);
end;

procedure TGLShadersManager.UniformMat3f(AUniform: TGLSLUniform; const
  Value: TMatrix3f);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.UniformMatrix3fv(loc, 1, False, @Value);
end;

procedure TGLShadersManager.UniformMat4f(AUniform: TGLSLUniform; const
  Value: TMatrix4f);
var
  loc: GLInt;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if loc > -1 then
    GL.UniformMatrix4fv(loc, 1, False, @Value);
end;

procedure TGLShadersManager.UniformSampler(AUniform: TGLSLUniform;
  const Texture: GLUInt; TexUnit: GLUInt);
var
  loc: GLInt;
  target: TGLTextureTarget;
begin
  Assert(vCurrentProgram <> nil);
  loc := AUniform.Location;
  if (loc > -1) and GetTextureTarget(AUniform, target) then
  begin
    CurrentGLContext.GLStates.TextureBinding[TexUnit, target] := Texture;
    GL.Uniform1i(loc, TexUnit);
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

initialization

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
  attrTangent :=
    TGLSLAttribute.RegisterAttribute('Tangent');
  attrBinormal :=
    TGLSLAttribute.RegisterAttribute('Binormal');
  attrIndex :=
    TGLSLAttribute.RegisterAttribute('Index');

  {: Registration of the most common uniforms. }
  uniformModelMatrix
    := TGLSLUniform.RegisterUniform('ModelMatrix');
  uniformViewProjectionMatrix
    := TGLSLUniform.RegisterUniform('ViewProjectionMatrix');
  uniformLightSourcePos
    := TGLSLUniform.RegisterUniform('LightSourcePos');
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
  uniformInstanceID
    := TGLSLUniform.RegisterUniform('InstanceID');

finalization

  FreeAndNil(vShadersManager);
  ClearRegistries;

end.
