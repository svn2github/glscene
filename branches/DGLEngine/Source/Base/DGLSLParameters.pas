//
// This unit is part of the GLScene Project, http://glscene.org
//
{ @HTML ( DGLSLParameter<p>

	<b>History : </b><font size=-1><ul>
    <li>24/12/15 - JD - Imported from GLScene
  </ul>
}
unit DGLSLParameters;

interface

{$i DGLEngine.inc}
{$M-}

uses
  System.Classes,
  dglOpenGL,
  DGLTypes,
  DGLVectorTypes,
  DGLTextureFormat,
  DGLRenderContextInfo;

type

  IShaderParameter = interface(IInterface)
    ['{E463E047-8D96-4887-B6CA-EDCD7970F754}']
    function GetName: string;
    function GetGLSLType: TDGLSLDataType;
    function GetGLSLSamplerType: TDGLSLSamplerType;

    function GetAutoSetMethod: string;
    function GetTextureName: string;
    function GetSamplerName: string;
    function GetTextureSwizzle: TSwizzleVector;
    procedure SetTextureName(const AValue: string);
    procedure SetSamplerName(const AValue: string);
    procedure SetAutoSetMethod(const AValue: string);
    procedure SetTextureSwizzle(const AValue: TSwizzleVector);

    function GetFloat: Single;
    function GetVec2: TVector2f;
    function GetVec3: TVector3f;
    function GetVec4: TVector4f;

    function GetInt: TGLint;
    function GetIVec2: TVector2i;
    function GetIVec3: TVector3i;
    function GetIVec4: TVector4i;

    function GetUInt: TGLuint;
    function GetUVec2: TVector2ui;
    function GetUVec3: TVector3ui;
    function GetUVec4: TVector4ui;

    procedure SetFloat(const Value: TGLFloat);
    procedure SetVec2(const Value: TVector2f);
    procedure SetVec3(const Value: TVector3f);
    procedure SetVec4(const Value: TVector4f);

    procedure SetInt(const Value: Integer);
    procedure SetIVec2(const Value: TVector2i);
    procedure SetIVec3(const Value: TVector3i);
    procedure SetIVec4(const Value: TVector4i);

    procedure SetUInt(const Value: GLuint);
    procedure SetUVec2(const Value: TVector2ui);
    procedure SetUVec3(const Value: TVector3ui);
    procedure SetUVec4(const Value: TVector4ui);

    function GetMat2: TMatrix2f;
    function GetMat3: TMatrix3f;
    function GetMat4: TMatrix4f;
    procedure SetMat2(const Value: TMatrix2f);
    procedure SetMat3(const Value: TMatrix3f);
    procedure SetMat4(const Value: TMatrix4f);

    procedure SetFloatArray(const Values: PGLFloat; Count: Integer);
    procedure SetIntArray(const Values: PGLInt; Count: Integer);
    procedure SetUIntArray(const Values: PGLUInt; Count: Integer);

    property Name: string read GetName;
    property GLSLType: TDGLSLDataType read GetGLSLType;
    property GLSLSamplerType: TDGLSLSamplerType read GetGLSLSamplerType;
    { @HTML ( Scalar types.<p>}
    property float: TGLFloat read GetFloat write SetFloat;
    property int: TGLint read GetInt write SetInt;
    property uint: TGLUint read GetUInt write SetUInt;

    { @HTML ( Float vector types.<p>}
    property vec2: TVector2f read GetVec2 write SetVec2;
    property vec3: TVector3f read GetVec3 write SetVec3;
    property vec4: TVector4f read GetVec4 write SetVec4;

    { @HTML ( Integer vector  types.<p>}
    property ivec2: TVector2i read GetIVec2 write SetIVec2;
    property ivec3: TVector3i read GetIVec3 write SetIVec3;
    property ivec4: TVector4i read GetIVec4 write SetIVec4;

    { @HTML ( Unsigned integer vector  types.<p>}
    property uvec2: TVector2ui read GetUVec2 write SetUVec2;
    property uvec3: TVector3ui read GetUVec3 write SetUVec3;
    property uvec4: TVector4ui read GetUVec4 write SetUVec4;

    { @HTML ( Matrix Types.<p>}
    property mat2: TMatrix2f read GetMat2 write SetMat2;
    property mat3: TMatrix3f read GetMat3 write SetMat3;
    property mat4: TMatrix4f read GetMat4 write SetMat4;
    { @HTML ( Bindings.<p>}
    property AutoSetMethod: string read GetAutoSetMethod write SetAutoSetMethod;
    property TextureName: string read GetTextureName write SetTextureName;
    property SamplerName: string read GetSamplerName write SetSamplerName;
    property TextureSwizzle: TSwizzleVector read GetTextureSwizzle write SetTextureSwizzle;
  end;


resourcestring
  rstrNothing = '*nothing*';

type
  TUniformAutoSetMethod = procedure(Sender: IShaderParameter; var ARci: TRenderContextInfo) of object;

function GLSLTypeEnum(AType: TDGLSLDataType): TGLEnum;
function GLSLTypeComponentCount(AType: TDGLSLDataType): Integer;
procedure RegisterUniformAutoSetMethod(AMethodName: string; AType: TDGLSLDataType; AMethod: TUniformAutoSetMethod);
procedure FillUniformAutoSetMethodList(AList: TStrings;TypeFilter: TDGLSLDataType); overload;
procedure FillUniformAutoSetMethodList(AList: TStrings; TypeFilter: TDGLSLSamplerType); overload;
function GetUniformAutoSetMethod(AMethodName: string): TUniformAutoSetMethod;
function GetUniformAutoSetMethodName(AMethod: TUniformAutoSetMethod): string;

//---------------------------------------------------------------------
implementation
//---------------------------------------------------------------------

const
  cGLSLTypeComponents: array[TDGLSLDataType] of Integer =
  (
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
    4,
    9,
    16,
    0
  );

  cGLSLTypeEnum: array[TDGLSLDataType] of Integer =
  (
    0,
    GL_FLOAT,
    GL_FLOAT,
    GL_FLOAT,
    GL_FLOAT,
    GL_INT,
    GL_INT,
    GL_INT,
    GL_INT,
    GL_UNSIGNED_INT,
    GL_UNSIGNED_INT,
    GL_UNSIGNED_INT,
    GL_UNSIGNED_INT,
    GL_UNSIGNED_INT,
    GL_FLOAT,
    GL_FLOAT,
    GL_FLOAT,
    0
  );

type
  TAutoSetMethodRec = record
    Name: string;
    UniformType: TDGLSLDataType;
    SamplerType: TDGLSLSamplerType;
    Method: TUniformAutoSetMethod;
  end;

var
  vMethods: array of TAutoSetMethodRec;

function GLSLTypeEnum(AType: TDGLSLDataType): TGLEnum;
begin
  Result := cGLSLTypeEnum[AType];
end;

function GLSLTypeComponentCount(AType: TDGLSLDataType): Integer;
begin
  Result := cGLSLTypeComponents[AType];
end;

procedure RegisterUniformAutoSetMethod(AMethodName: string;
  AType: TDGLSLDataType; AMethod: TUniformAutoSetMethod);
var
  I: Integer;
begin
  for I := 0 to High(vMethods) do
    if vMethods[I].Name = AMethodName then
    begin
      vMethods[I].UniformType := AType;
      vMethods[I].Method := AMethod;
      exit;
    end;
  I := Length(vMethods);
  SetLength(vMethods, I+1);
  vMethods[I].Name := AMethodName;
  vMethods[I].UniformType := AType;
  vMethods[I].SamplerType := GLSLSamplerUndefined;
  vMethods[I].Method := AMethod;
end;

procedure FillUniformAutoSetMethodList(AList: TStrings; TypeFilter: TDGLSLDataType);
var
  I: Integer;
begin
  for I := 0 to High(vMethods) do
    if vMethods[I].UniformType = TypeFilter then
      AList.Add(vMethods[I].Name);
end;

procedure FillUniformAutoSetMethodList(AList: TStrings; TypeFilter: TDGLSLSamplerType);
var
  I: Integer;
begin
  for I := 0 to High(vMethods) do
    if vMethods[I].SamplerType = TypeFilter then
      AList.Add(vMethods[I].Name);
end;

function GetUniformAutoSetMethod(AMethodName: string): TUniformAutoSetMethod;
var
  I: Integer;
begin
  for I := 0 to High(vMethods) do
    if vMethods[I].Name = AMethodName then
    begin
      Result := vMethods[I].Method;
      exit;
    end;
  Result := nil;
end;

function GetUniformAutoSetMethodName(AMethod: TUniformAutoSetMethod): string;
var
  I: Integer;
begin
  for I := 0 to High(vMethods) do
    if @vMethods[I].Method = @AMethod then
    begin
      Result := vMethods[I].Name;
      exit;
    end;
  Result := '';
end;

end.
