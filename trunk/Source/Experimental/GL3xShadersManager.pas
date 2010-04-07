//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xShadersManager<p>

   Base unit to manage shader objects and program.<p>
   Can work with different rendering context.<p>
   If contexts shared posible to use same shader program.<p>

   <b>History : </b><font size=-1><ul>
    <li>24/03/10 - Yar - Creation (based on uShaders.pas by Alex Karpenyuk aka Fantom)
 </ul></font>
}

unit GL3xShadersManager;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
  // GLScene
  OpenGL1x, GLContext, GLState,
  VectorLists, VectorTypes,
  GLSLog;

const
  GLS_VERTEX_ATTR_NUM = 16;

type

  TGLSLProgramType = (ptVertex, ptGeometry, ptFragment);

  TGLSLDataType = (
    GLSLTypeUndefined,
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
    GLSLType4UB,
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
    GLSLTypeUIntSamplerBuffer
    );

{$IFDEF GLS_COMPILER_2006_UP}
  TGLSLAttribute = record
  private
    FID: Integer;
    FName: string;
    Location: GLInt;
  public
    Tag: Integer;
    DataType: TGLSLDataType;
    WarningAbsenceLoged: Boolean;
    property ID: Integer read FID;
    property Name: string read FName;
  end;
{$ELSE}
  TGLSLAttribute = record
    Location: GLInt;
    Tag: Integer;
    ID: Integer; // Do not change value manualy !!
    Name: string; // Do not change value at manualy !!
    FNameHashKey: Integer; // Do not change value manualy !!
    DataType: TGLSLDataType;
    WarningAbsenceLoged: Boolean;
  end;
{$ENDIF}
  PGLSLAttribute = ^TGLSLAttribute;

  TGLSLAttributeArray = array[0..GLS_VERTEX_ATTR_NUM - 1] of TGLSLAttribute;

{$IFDEF GLS_COMPILER_2006_UP}
  TGLSLUniform = record
  private
    Location: GLInt;
    FID: Integer;
    FName: string;
  public
    Tag: Integer;
    DataType: TGLSLDataType;
    WarningAbsenceLoged: Boolean;
    property ID: Integer read FID;
    property Name: string read FName;
  end;
{$ELSE}
  TGLSLUniform = record
    Location: GLInt;
    Tag: Integer;
    ID: Integer;
    Name: string;
    DataType: TGLSLDataType;
    WarningAbsenceLoged: Boolean;
  end;
{$ENDIF}
  PGLSLUniform = ^TGLSLUniform;

{$IFDEF GLS_COMPILER_2006_UP}
  TGLSLShaderObject = record
  private
    FIndex: Integer;
    FHandle: TGLShaderHandle;
    FNameHashKey: Integer;
    FFriendlyName: string;
  public
    property FriendlyName: string read FFriendlyName;
  end;
{$ELSE}
  TGLSLShaderObject = record
    FIndex: Integer; // Do not change value manualy !!
    FHandle: TGLShaderHandle; // Do not change value manualy !!
    FNameHashKey: Integer;
    FriendlyName: string;
  end;
{$ENDIF}
  PGLSLShaderObject = ^TGLSLShaderObject;

{$IFDEF GLS_COMPILER_2006_UP}
  TGLSLShaderProgram = record
  private
    FIndex: Integer;
    FHandle: TGLProgramHandle;
    FNameHashKey: Integer;
    FFriendlyName: string;
    FActiveAttrib: array of TGLSLAttribute;
    FActiveUniform: array of TGLSLUniform;
    FLinked: Boolean;
    FAttachedObjects: Integer;
  public
    property FriendlyName: string read FFriendlyName;
    property AttachedObjects: Integer read FAttachedObjects;
  end;
{$ELSE}
  TGLSLShaderProgram = record
    FIndex: Integer;
    FHandle: TGLProgramHandle;
    FNameHashKey: Integer;
    FriendlyName: string;
    FActiveAttrib: array of TGLSLAttribute;
    FActiveUniform: array of TGLSLUniform;
    FLinked: Boolean;
    AttachedObjects: Integer;
  end;
{$ENDIF}

  PGLSLShaderProgram = ^TGLSLShaderProgram;

  TGL3xShadersManager = class
  private
    { Private Declarations }
    FShaderObjectsList: TList;
    FShaderProgramsList: TList;
    FSpaces: array[0..1] of Boolean;
    FCurrentProgram: PGLSLShaderProgram;
    CompilationLog: TLogSession;
    WorkLog: TLogSession;

    function GetShaderObject(const AName: string): PGLSLShaderObject;
    function GetShaderProgram(const AName: string): PGLSLShaderProgram;
    function GetUniformLocation(var AUniform: TGLSLUniform): GLInt;
    function GetTextureTarget(const AUniform: TGLSLUniform): GLInt;
    procedure DeleteShaderObject(AObject: PGLSLShaderObject); overload;
    procedure DeleteShaderProgram(AProgram: PGLSLShaderProgram); overload;
    function ComputeNameHashKey(const AName: string): Integer;
    procedure PackLists;

  public
    { Public Declarations }
    constructor Create;
    destructor Destroy; override;

    procedure DefineShaderObject(const AName: string; const code: AnsiString;
      AType: TGLSLProgramType = ptVertex);
    procedure DefineShaderProgram(const AName: string);
    procedure DeleteShaderObject(const AName: string); overload;
    procedure DeleteShaderProgram(const AName: string); overload;

    procedure AttachShaderObjectToProgram(const AObject: string;
      const AProgram: string);
    procedure DettachShaderObjectToProgram(const AObject: string;
      const AProgram: string);

    function LinkShaderProgram(const AName: string): Boolean;
    procedure UseProgram(const AName: string);
    function IsProgramCurrent(const AName: string): Boolean;
    function GetCurrentProgram: string;
    function GetAttachedShaderToProgram(const AName: string): Integer;
    procedure UseFixedFunctionPipeline;
    procedure ClearShaderObject;
    procedure ClearShaderPrograms;

    procedure Uniform1f(var AUniform: TGLSLUniform; const Value: Single);
    procedure Uniform2f(var AUniform: TGLSLUniform; const Value: TVector2f);
    procedure Uniform3f(var AUniform: TGLSLUniform; const Value: TVector3f);
    procedure Uniform4f(var AUniform: TGLSLUniform; const Value: TVector4f);
    procedure Uniform1I(var AUniform: TGLSLUniform; const Value: Integer);
      overload;
    procedure Uniform1I(var AUniform: TGLSLUniform; const Value: PGLInt;
      Count: Integer);
      overload;
    procedure Uniform2I(var AUniform: TGLSLUniform; const Value: TVector2I);
    procedure Uniform3I(var AUniform: TGLSLUniform; const Value: TVector3I);
    procedure Uniform4I(var AUniform: TGLSLUniform; const Value: TVector4I);
      overload;
    procedure Uniform4I(var AUniform: TGLSLUniform; const Value: PGLInt;
      Count: Integer);
      overload;
    procedure UniformMat2f(var AUniform: TGLSLUniform; const Value:
      TMatrix2f);
    procedure UniformMat3f(var AUniform: TGLSLUniform; const Value:
      TMatrix3f);
    procedure UniformMat4f(var AUniform: TGLSLUniform; const Value:
      TMatrix4f);
    procedure UniformSampler(var AUniform: TGLSLUniform; const Texture:
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

  ShadersManager: TGL3xShadersManager;

function RegisterGLSLAttribute(var Attr: TGLSLAttribute; const AName: string):
  Boolean;
function GetGLSLAttribute(const AName: string; out Attr: TGLSLAttribute):
  Boolean;
function RegisterGLSLUniform(var Uniform: TGLSLUniform; const AName: string):
  Boolean;
function GetGLSLUniform(const AName: string; out Uniform: TGLSLUniform):
  Boolean;

implementation

uses
  GLStrings;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Shader variables registry'}{$ENDIF}
var
  AttributeRegistry: array[Byte] of TGLSLAttribute;
  UniformRegistry: array[Byte] of TGLSLUniform;

procedure ClearAttributeRegistry;
var
  a: Integer;
begin
  for a := 0 to High(AttributeRegistry) do
{$IFDEF GLS_COMPILER_2006_UP}
    AttributeRegistry[a].FID := 0;
{$ELSE}
    AttributeRegistry[a].ID := 0;
{$ENDIF}
end;

function RegisterGLSLAttribute(var Attr: TGLSLAttribute; const AName: string):
  Boolean;
var
  a: Integer;
begin
  Result := false;
  for a := 0 to High(AttributeRegistry) do
  begin
    if AttributeRegistry[a].ID > 0 then
      if AttributeRegistry[a].Name = AName then
      begin
        Result := true;
        exit;
      end;
  end;
  for a := 0 to High(AttributeRegistry) do
  begin
    if AttributeRegistry[a].ID = 0 then
    begin
{$IFDEF GLS_COMPILER_2006_UP}
      AttributeRegistry[a].FID := a + 1;
      AttributeRegistry[a].FName := AName;
{$ELSE}
      AttributeRegistry[a].ID := a + 1;
      AttributeRegistry[a].Name := AName;
{$ENDIF}
      AttributeRegistry[a].DataType := GLSLTypeUndefined;
      AttributeRegistry[a].WarningAbsenceLoged := False;
      Attr := AttributeRegistry[a];
      Result := true;
      exit;
    end;
  end;
end;

function GetGLSLAttribute(const AName: string; out Attr: TGLSLAttribute):
  Boolean;
var
  a: Integer;
begin
  for a := 0 to High(AttributeRegistry) do
  begin
    if AttributeRegistry[a].ID > 0 then
      if AttributeRegistry[a].Name = AName then
      begin
        Attr := AttributeRegistry[a];
        Result := true;
        exit;
      end;
  end;
  Result := false;
end;

procedure ClearUniformRegistry;
var
  u: Integer;
begin
  for u := 0 to High(UniformRegistry) do
{$IFDEF GLS_COMPILER_2006_UP}
    UniformRegistry[u].FID := 0;
{$ELSE}
    UniformRegistry[u].ID := 0;
{$ENDIF}
end;

function RegisterGLSLUniform(var Uniform: TGLSLUniform; const AName: string):
  Boolean;
var
  u: Integer;
begin
  Result := false;
  for u := 0 to High(UniformRegistry) do
  begin
    if UniformRegistry[u].ID > 0 then
      if UniformRegistry[u].Name = AName then
      begin
        Uniform := UniformRegistry[u];
        Result := true;
        exit;
      end;
  end;
  for u := 0 to High(UniformRegistry) do
  begin
    if UniformRegistry[u].ID = 0 then
    begin
{$IFDEF GLS_COMPILER_2006_UP}
      UniformRegistry[u].FID := u + 1;
      UniformRegistry[u].FName := AName;
{$ELSE}
      UniformRegistry[u].ID := u + 1;
      UniformRegistry[u].Name := AName;
{$ENDIF}
      UniformRegistry[u].DataType := GLSLTypeUndefined;
      UniformRegistry[u].WarningAbsenceLoged := False;
      Uniform := UniformRegistry[u];
      Result := true;
      exit;
    end;
  end;
end;

function GetGLSLUniform(const AName: string; out Uniform: TGLSLUniform):
  Boolean;
var
  u: Integer;
begin
  for u := 0 to High(UniformRegistry) do
  begin
    if UniformRegistry[u].ID > 0 then
      if UniformRegistry[u].Name = AName then
      begin
        Uniform := UniformRegistry[u];
        Result := true;
        exit;
      end;
  end;
  Result := false;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGL3xShadersManager'}{$ENDIF}
// ------------------
// ------------------ TGL3xShadersManager ------------------
// ------------------

constructor TGL3xShadersManager.Create;
var
  LogPath: string;
begin
  inherited;
  FShaderObjectsList := TList.Create;
  FShaderProgramsList := TList.Create;
  FSpaces[0] := False;
  FSpaces[1] := False;

  LogPath := ExtractFilePath(ParamStr(0));
  CompilationLog
    := TLogSession.Init(LogPath + 'ShadersCompilation.Log', lfNone,
    [lkInfo, lkWarning, lkError]);
  WorkLog
    := TLogSession.Init(LogPath + 'ShadersWork.Log', lfNone,
    [lkWarning, lkError]);
end;

destructor TGL3xShadersManager.Destroy;
begin
  ClearShaderPrograms;
  ClearShaderObject;
  FShaderObjectsList.Free;
  FShaderProgramsList.Free;
  CompilationLog.Shutdown;
  WorkLog.Shutdown;
  inherited;
end;

function TGL3xShadersManager.GetShaderObject(const AName: string):
  PGLSLShaderObject;
var
  I, hk: Integer;
begin
  hk := ComputeNameHashKey(AName);
  for I := 0 to FShaderObjectsList.Count - 1 do
  begin
    Result := FShaderObjectsList.Items[I];
    if Assigned(Result) and (Result.FNameHashKey = hk) then
      exit;
  end;
  Result := nil;
end;

function TGL3xShadersManager.GetShaderProgram(const AName: string):
  PGLSLShaderProgram;
var
  I: Integer;
begin
  for I := 0 to FShaderProgramsList.Count - 1 do
  begin
    Result := FShaderProgramsList.Items[I];
    if Assigned(Result) and (Result.FriendlyName = AName) then
      exit;
  end;
  Result := nil;
end;

function TGL3xShadersManager.ComputeNameHashKey(const AName: string): Integer;
var
  i, n: Integer;
begin
  n := Length(AName);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(AName[i]);
end;

function TGL3xShadersManager.GetUniformLocation(var AUniform: TGLSLUniform):
  GLInt;
var
  I: Integer;
begin
  for I := 0 to High(FCurrentProgram.FActiveUniform) do
  begin
    if FCurrentProgram.FActiveUniform[I].ID = AUniform.ID then
    begin
      Result := FCurrentProgram.FActiveUniform[I].Location;
      exit;
    end;
  end;
  Result := -1;
  if not AUniform.WarningAbsenceLoged then
  begin
    WorkLog.LogError('Using an unknown uniform "' + AUniform.Name + '"');
    AUniform.WarningAbsenceLoged := True;
  end;
end;

function TGL3xShadersManager.GetTextureTarget(const AUniform: TGLSLUniform):
  GLInt;
const
  cGLSLTypeToTexTarget: array[GLSLTypeSampler1D..GLSLTypeUIntSamplerBuffer]
    of TGLEnum = (GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D,
    GL_TEXTURE_CUBE_MAP,
    GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY,
    GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_CUBE_MAP,
    GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP,
    GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY,
    GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, GL_TEXTURE_CUBE_MAP,
    GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_RECTANGLE,
    GL_TEXTURE_RECTANGLE, GL_TEXTURE_BUFFER, GL_TEXTURE_RECTANGLE,
    GL_TEXTURE_BUFFER, GL_TEXTURE_RECTANGLE, GL_TEXTURE_BUFFER);
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FCurrentProgram.FActiveUniform) do
  begin
    if FCurrentProgram.FActiveUniform[I].ID = AUniform.ID then
    begin
      if FCurrentProgram.FActiveUniform[I].DataType < GLSLTypeSampler1D then
        WorkLog.LogError('Uniform "' + AUniform.Name +
          '" has not sampler type')
      else
        Result :=
          cGLSLTypeToTexTarget[FCurrentProgram.FActiveUniform[I].DataType];
      exit;
    end;
  end;
  WorkLog.LogError('Using an unknown uniform "' + AUniform.Name + '"');
end;

procedure TGL3xShadersManager.PackLists;
var
  I: Integer;
  Obj: PGLSLShaderObject;
  Prog: PGLSLShaderProgram;
begin
  if FSpaces[0] then
  begin
    FShaderObjectsList.Pack;
    for I := 0 to FShaderObjectsList.Count - 1 do
    begin
      Obj := FShaderObjectsList.Items[I];
      Obj.FIndex := I;
    end;
    FSpaces[0] := False;
  end;
  if FSpaces[1] then
  begin
    FShaderProgramsList.Pack;
    for I := 0 to FShaderProgramsList.Count - 1 do
    begin
      Prog := FShaderProgramsList.Items[I];
      Prog.FIndex := I;
    end;
    FSpaces[1] := False;
  end;
end;

procedure TGL3xShadersManager.DefineShaderObject(
  const AName: string;
  const Code: AnsiString;
  AType: TGLSLProgramType);
const
  cObjectClass: array[TGLSLProgramType] of TGLShaderHandleClass =
    (TGLVertexShaderHandle, TGLGeometryShaderHandle, TGLFragmentShaderHandle);
  cObjectTypeName: array[TGLSLProgramType] of string =
    ('Vertex', 'Geomtry', 'Fragment');
var
  val, len: integer;
  pLog: PAnsiChar;
  CompResult: Boolean;
  Obj: PGLSLShaderObject;
  logstr: string;
begin
  PackLists;
  Obj := GetShaderObject(AName);
  if not Assigned(Obj) then
  begin
    New(Obj);
    Obj.FHandle := cObjectClass[AType].CreateAndAllocate;
    Obj.FIndex := FShaderObjectsList.Add(Obj);
{$IFDEF GLS_COMPILER_2005_UP}
    Obj.FFriendlyName := AName;
{$ELSE}
    Obj.FriendlyName := AName;
{$ENDIF}
    Obj.FNameHashKey := ComputeNameHashKey(AName);
  end;
  Obj.FHandle.ShaderSource(string(code));
  CompResult := Obj.FHandle.CompileShader;

  logstr := cObjectTypeName[AType] + ' shader object ' +
    Obj.FriendlyName + ' compilation - ';

  if CompResult then
    CompilationLog.LogInfo(logstr + ' Successful')
  else
    CompilationLog.LogError(logstr + ' Failed!');

  glGetShaderiv(Obj.FHandle.Handle, GL_INFO_LOG_LENGTH, @val);
  GetMem(pLog, val);
  glGetShaderInfoLog(Obj.FHandle.Handle, val, @len, pLog);
  if length(pLog) > 0 then
    CompilationLog.LogInfo(string(pLog));
  FreeMem(pLog, val);
end;

procedure TGL3xShadersManager.DefineShaderProgram(const AName: string);
var
  Prog: PGLSLShaderProgram;
begin
  PackLists;
  Prog := GetShaderProgram(AName);
  if not Assigned(Prog) then
  begin
    New(Prog);
    Prog.FHandle := TGLProgramHandle.CreateAndAllocate;
{$IFDEF GLS_COMPILER_2005_UP}
    Prog.FFriendlyName := AName;
{$ELSE}
    Prog.FriendlyName := AName;
{$ENDIF}
    Prog.FNameHashKey := ComputeNameHashKey(AName);
    Prog.FIndex := FShaderProgramsList.Add(Prog);
  end;
  Prog.FLinked := False;
end;

procedure TGL3xShadersManager.AttachShaderObjectToProgram(
  const AObject: string; const AProgram: string);
var
  obj: PGLSLShaderObject;
  Prog: PGLSLShaderProgram;
begin
  obj := GetShaderObject(AObject);
  Prog := GetShaderProgram(AProgram);
  glAttachShader(prog.FHandle.Handle, obj.FHandle.Handle);
{$IFDEF GLS_COMPILER_2005_UP}
  Inc(Prog.FAttachedObjects);
{$ELSE}
  Inc(Prog.AttachedObjects);
{$ENDIF}
  Prog.FLinked := False;
end;

procedure TGL3xShadersManager.DettachShaderObjectToProgram(
  const AObject: string; const AProgram: string);
var
  obj: PGLSLShaderObject;
  Prog: PGLSLShaderProgram;
  shaders: array[Byte] of GLuint;
  count: GLsizei;
  i: Integer;
begin
  obj := GetShaderObject(AObject);
  Prog := GetShaderProgram(AProgram);
  glGetAttachedShaders(prog.FHandle.Handle, Length(shaders), @count,
    @shaders[0]);
  for i := 0 to count - 1 do
    if obj.FHandle.Handle = shaders[i] then
    begin
      glDetachShader(prog.FHandle.Handle, obj.FHandle.Handle);
{$IFDEF GLS_COMPILER_2005_UP}
      Dec(Prog.FAttachedObjects);
{$ELSE}
      Dec(Prog.AttachedObjects);
{$ENDIF}
      Prog.FLinked := False;
      exit;
    end;
  CompilationLog.LogWarning('Unable to detach shader object "'
    + obj.FriendlyName + '" from shader program "' + Prog.FriendlyName +
      '"');
end;

function TGL3xShadersManager.LinkShaderProgram(const AName: string): Boolean;
var
  prog: PGLSLShaderProgram;
  val, len: GLsizei;
  pLog: PAnsiChar;
  i: Integer;
  buff: array[0..127] of AnsiChar;
  max: GLInt;
  Size: GLInt;
  AType: GLenum;
  logstr: string;
begin
  prog := GetShaderProgram(AName);
  Result := prog.FHandle.LinkProgram;
  prog.FLinked := Result;

  logstr := 'Shader Program ' + prog.FriendlyName + ' Linking - ';
  if prog.FLinked then
    CompilationLog.LogInfo(logstr + ' Successful')
  else
    CompilationLog.LogError(logstr + ' Failed!');
  glGetProgramiv(prog.FHandle.Handle, GL_INFO_LOG_LENGTH, @val);
  GetMem(pLog, val);
  glGetProgramInfoLog(prog.FHandle.Handle, val, @len, pLog);
  if length(pLog) > 0 then
    CompilationLog.LogInfo(string(pLog));
  FreeMem(pLog, val);

  if not prog.FLinked then
    exit;
  // Get all active atttributes
  glGetProgramiv(prog.FHandle.Handle, GL_ACTIVE_ATTRIBUTES, @max);
  SetLength(prog.FActiveAttrib, max);
  for I := 0 to max - 1 do
  begin
    glGetActiveAttrib(
      prog.FHandle.Handle,
      I,
      Length(buff),
      @len,
      @Size,
      @AType,
      @buff[0]);

    if GetGLSLAttribute(Copy(string(buff), 0, len), prog.FActiveAttrib[I]) then
    begin
      with prog.FActiveAttrib[I] do
      begin
        CompilationLog.LogInfo('Detected active attribute: ' + Name);
        Location := glGetAttribLocation(prog.FHandle.Handle,
          PGLChar(TGLString(Name)));
        case AType of
          GL_FLOAT: DataType := GLSLType1F;
          GL_FLOAT_VEC2: DataType := GLSLType2F;
          GL_FLOAT_VEC3: DataType := GLSLType3F;
          GL_FLOAT_VEC4: DataType := GLSLType4F;
          GL_INT: DataType := GLSLType1I;
          GL_INT_VEC2: DataType := GLSLType2I;
          GL_INT_VEC3: DataType := GLSLType3I;
          GL_INT_VEC4: DataType := GLSLType4I;
          GL_BOOL: DataType := GLSLType1I;
          GL_BOOL_VEC2: DataType := GLSLType2I;
          GL_BOOL_VEC3: DataType := GLSLType3I;
          GL_BOOL_VEC4: DataType := GLSLType4I;
          GL_FLOAT_MAT2: DataType := GLSLTypeMat2F;
          GL_FLOAT_MAT3: DataType := GLSLTypeMat3F;
          GL_FLOAT_MAT4: DataType := GLSLTypeMat4F;
        else
          begin
            DataType := GLSLTypeUndefined;
            CompilationLog.LogError('Active attribute ' + Name + ' with ' +
              glsUnknownType);
          end;
        end;
      end;
    end;
  end;

  // Get all active uniform
  glGetProgramiv(prog.FHandle.Handle, GL_ACTIVE_UNIFORMS, @max);
  SetLength(prog.FActiveUniform, max);

  for I := 0 to max - 1 do
  begin
    glGetActiveUniform(
      prog.FHandle.Handle,
      I,
      Length(buff),
      @len,
      @Size,
      @AType,
      @buff[0]);

    if GetGLSLUniform(Copy(string(buff), 0, len), prog.FActiveUniform[I]) then
    begin
      with prog.FActiveUniform[I] do
      begin
        CompilationLog.LogInfo('Detected active uniform: ' + Name);
        Location := glGetUniformLocation(prog.FHandle.Handle,
          PGLChar(TGLString(Name)));
        case AType of
          GL_FLOAT: DataType := GLSLType1F;
          GL_FLOAT_VEC2: DataType := GLSLType2F;
          GL_FLOAT_VEC3: DataType := GLSLType3F;
          GL_FLOAT_VEC4: DataType := GLSLType4F;
          GL_INT: DataType := GLSLType1I;
          GL_INT_VEC2: DataType := GLSLType2I;
          GL_INT_VEC3: DataType := GLSLType3I;
          GL_INT_VEC4: DataType := GLSLType4I;
          GL_BOOL: DataType := GLSLType1I;
          GL_BOOL_VEC2: DataType := GLSLType2I;
          GL_BOOL_VEC3: DataType := GLSLType3I;
          GL_BOOL_VEC4: DataType := GLSLType4I;
          GL_FLOAT_MAT2: DataType := GLSLTypeMat2F;
          GL_FLOAT_MAT3: DataType := GLSLTypeMat3F;
          GL_FLOAT_MAT4: DataType := GLSLTypeMat4F;
          GL_SAMPLER_1D: DataType := GLSLTypeSampler1D;
          GL_SAMPLER_2D: DataType := GLSLTypeSampler2D;
          GL_SAMPLER_3D: DataType := GLSLTypeSampler3D;
          GL_SAMPLER_CUBE: DataType := GLSLTypeSamplerCube;
          GL_SAMPLER_1D_SHADOW: DataType := GLSLTypeSampler1DShadow;
          GL_SAMPLER_2D_SHADOW: DataType := GLSLTypeSampler2DShadow;
          GL_SAMPLER_2D_RECT: DataType := GLSLTypeSamplerRect;
          GL_SAMPLER_2D_RECT_SHADOW: DataType := GLSLTypeSamplerRectShadow;
          GL_SAMPLER_BUFFER: DataType := GLSLTypeSamplerBuffer;
          GL_INT_SAMPLER_2D_RECT: DataType := GLSLTypeIntSamplerRect;
          GL_INT_SAMPLER_BUFFER: DataType := GLSLTypeIntSamplerBuffer;
          GL_UNSIGNED_INT_SAMPLER_2D_RECT: DataType := GLSLTypeUIntSamplerRect;
          GL_UNSIGNED_INT_SAMPLER_BUFFER: DataType := GLSLTypeUIntSamplerBuffer;
        else
          begin
            DataType := GLSLTypeUndefined;
            CompilationLog.LogError('Active uniform ' + Name + ' with ' +
              glsUnknownType);
          end;
        end;
      end;
    end
    else begin
      CompilationLog.LogError('Active uniform ' + Copy(string(buff), 0, len) +
        ' not registered');
    end;
  end;

end;

procedure TGL3xShadersManager.UseProgram(const AName: string);
var
  prog: PGLSLShaderProgram;
begin
  prog := GetShaderProgram(AName);
  Assert(Assigned(prog));
  if prog.FLinked then
  begin
    FCurrentProgram := prog;
    prog.FHandle.UseProgramObject;
  end
  else
    WorkLog.LogError('Used not linked program "'+prog.FriendlyName+'"');
end;

function TGL3xShadersManager.IsProgramCurrent(const AName: string): Boolean;
begin
  if FCurrentProgram = nil then
    Result := false
  else
    Result := FCurrentProgram.FNameHashKey = ComputeNameHashKey(AName);
end;

function TGL3xShadersManager.GetCurrentProgram: string;
begin
  if FCurrentProgram = nil then
    Result := ''
  else
    Result := FCurrentProgram.FriendlyName;
end;

function TGL3xShadersManager.GetAttachedShaderToProgram(const AName: string):
  Integer;
var
  Prog: PGLSLShaderProgram;
begin
  prog := GetShaderProgram(AName);
  Assert(Assigned(prog));
  Result := Prog.AttachedObjects;
end;

procedure TGL3xShadersManager.UseFixedFunctionPipeline;
begin
  FCurrentProgram := nil;
  CurrentGLContext.GLStates.CurrentProgram := 0;
end;

procedure TGL3xShadersManager.DeleteShaderObject(AObject: PGLSLShaderObject);
begin
  if not Assigned(AObject) then
  begin
    WorkLog.LogWarning('Attempt to delete a nil pointer of shader object');
    exit;
  end;
  AObject.FHandle.Free;
  FShaderObjectsList[AObject.FIndex] := nil;
  Dispose(AObject);
  FSpaces[0] := True;
end;

procedure TGL3xShadersManager.DeleteShaderProgram(AProgram: PGLSLShaderProgram);
begin
  if not Assigned(AProgram) then
  begin
    WorkLog.LogWarning('Attempt to delete a nil pointer of shader program');
    exit;
  end;
  AProgram.FHandle.Free;
  FShaderProgramsList[AProgram.FIndex] := nil;
  Dispose(AProgram);
  FSpaces[1] := True;
end;

procedure TGL3xShadersManager.DeleteShaderObject(const AName: string);
begin
  DeleteShaderObject(GetShaderObject(AName));
end;

procedure TGL3xShadersManager.DeleteShaderProgram(const AName: string);
begin
  DeleteShaderProgram(GetShaderProgram(AName));
end;

procedure TGL3xShadersManager.ClearShaderObject;
var
  i: integer;
begin
  PackLists;
  for i := 0 to FShaderObjectsList.Count - 1 do
    DeleteShaderObject(FShaderObjectsList[i]);
end;

procedure TGL3xShadersManager.ClearShaderPrograms;
var
  i: integer;
begin
  PackLists;
  for i := 0 to FShaderProgramsList.Count - 1 do
    DeleteShaderProgram(FShaderProgramsList[i]);
end;

procedure TGL3xShadersManager.Uniform1f(var AUniform: TGLSLUniform; const
  Value: Single);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniform1f(loc, Value);
end;

procedure TGL3xShadersManager.Uniform2f(var AUniform: TGLSLUniform; const
  Value: TVector2f);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniform2f(loc, Value[0], Value[1]);
end;

procedure TGL3xShadersManager.Uniform3f(var AUniform: TGLSLUniform; const
  Value: TVector3f);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniform3f(loc, Value[0], Value[1], Value[2]);
end;

procedure TGL3xShadersManager.Uniform4f(var AUniform: TGLSLUniform; const
  Value: TVector4f);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniform4f(loc, Value[0], Value[1], Value[2], Value[3]);
end;

procedure TGL3xShadersManager.Uniform1I(var AUniform: TGLSLUniform; const
  Value: Integer);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniform1i(loc, Value);
end;

procedure TGL3xShadersManager.Uniform1I(var AUniform: TGLSLUniform;
  const Value: PGLInt; Count: Integer);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniform1iv(loc, Count, Value);
end;

procedure TGL3xShadersManager.Uniform2I(var AUniform: TGLSLUniform; const
  Value: TVector2I);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniform2i(loc, Value[0], Value[1]);
end;

procedure TGL3xShadersManager.Uniform3I(var AUniform: TGLSLUniform; const
  Value: TVector3I);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniform3i(loc, Value[0], Value[1], Value[2]);
end;

procedure TGL3xShadersManager.Uniform4I(var AUniform: TGLSLUniform; const
  Value: TVector4I);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniform4i(loc, Value[0], Value[1], Value[2], Value[3]);
end;

procedure TGL3xShadersManager.Uniform4I(var AUniform: TGLSLUniform; const
  Value: PGLInt; Count: Integer);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniform4iv(loc, Count, Value);
end;

procedure TGL3xShadersManager.UniformMat2f(var AUniform: TGLSLUniform; const
  Value: TMatrix2f);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniformMatrix2fvARB(loc, 1, False, @Value);
end;

procedure TGL3xShadersManager.UniformMat3f(var AUniform: TGLSLUniform; const
  Value: TMatrix3f);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniformMatrix3fv(loc, 1, False, @Value);
end;

procedure TGL3xShadersManager.UniformMat4f(var AUniform: TGLSLUniform; const
  Value: TMatrix4f);
var
  loc: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  loc := GetUniformLocation(AUniform);
  if loc>-1 then
    glUniformMatrix4fv(loc, 1, False, @Value);
end;

procedure TGL3xShadersManager.UniformSampler(var AUniform: TGLSLUniform;
  const Texture: GLUInt; TexUnit: GLUInt);
var
  loc, target: GLInt;
begin
  Assert(FCurrentProgram <> nil);
  Assert(AUniform.ID > 0);
  target := GetTextureTarget(AUniform);
  if target>0 then
  begin
    glActiveTexture(GL_TEXTURE0 + TexUnit);
    CurrentGLContext.GLStates.SetGLCurrentTexture(
      TexUnit, target, Texture);
    loc := GetUniformLocation(AUniform);
    if loc>-1 then
      glUniform1i(loc, TexUnit);
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

initialization

  {: Registration of the most common attributes. }
  ClearAttributeRegistry;
  RegisterGLSLAttribute(attrPosition, 'Position');
  RegisterGLSLAttribute(attrNormal, 'Normal');
  RegisterGLSLAttribute(attrVertexColor, 'VertexColor');
  RegisterGLSLAttribute(attrTexCoord0, 'TexCoord0');
  RegisterGLSLAttribute(attrTexCoord1, 'TexCoord1');
  RegisterGLSLAttribute(attrTexCoord2, 'TexCoord2');
  RegisterGLSLAttribute(attrTexCoord3, 'TexCoord3');
  RegisterGLSLAttribute(attrTangent, 'Tangent');
  RegisterGLSLAttribute(attrBinormal, 'Binormal');
  RegisterGLSLAttribute(attrIndex, 'Index');

  {: Registration of the most common uniforms. }
  ClearUniformRegistry;
  RegisterGLSLUniform(uniformModelMatrix, 'ModelMatrix');
  RegisterGLSLUniform(uniformViewProjectionMatrix, 'ViewProjectionMatrix');
  RegisterGLSLUniform(uniformDiffuse, 'Diffuse');
  RegisterGLSLUniform(uniformTexUnit0, 'TexUnit0');
  RegisterGLSLUniform(uniformTexUnit1, 'TexUnit1');
  RegisterGLSLUniform(uniformTexUnit2, 'TexUnit2');
  RegisterGLSLUniform(uniformTexUnit3, 'TexUnit3');
  RegisterGLSLUniform(uniformTexUnit4, 'TexUnit4');
  RegisterGLSLUniform(uniformTexUnit5, 'TexUnit5');
  RegisterGLSLUniform(uniformTexUnit6, 'TexUnit6');
  RegisterGLSLUniform(uniformTexUnit7, 'TexUnit7');
  RegisterGLSLUniform(uniformInstanceID, 'InstanceID');

  ShadersManager := TGL3xShadersManager.Create;

finalization

  ShadersManager.Destroy;
  ShadersManager := nil;

end.

