//
// VKScene project based on GLScene library, http://glscene.sourceforge.net 
//
{
   TVKSLShader is a wrapper for GLS shaders. 
     
 }
unit VKS.GLSLShader;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,
  //VKS
  VKS.VectorGeometry, VKS.VectorTypes, VKS.Texture, VKS.OpenGLTokens, VKS.Context, 
  VKS.CustomShader, VKS.RenderContextInfo, VKS.TextureFormat, VKS.GLSLParameter;

type
  TVKSLShaderParameter = class;
  TVKCustomGLSLShader = class;
  EGLSLShaderException = class(EGLCustomShaderException);

  TVKSLShaderEvent = procedure(Shader: TVKCustomGLSLShader) of object;
  TVKSLShaderUnApplyEvent = procedure(Shader: TVKCustomGLSLShader;
                                     var ThereAreMorePasses: Boolean) of object;
  TVKSLShaderEventEx = procedure(Shader: TVKCustomGLSLShader;
    Sender: TObject) of object;

  TVKActiveAttrib = record
    Name: string;
    Size: GLInt;
    AType: TVKSLDataType;
    Location: Integer;
  end;

  TVKActiveAttribArray = array of TVKActiveAttrib;

  TVKCustomGLSLShader = class(TVKCustomShader)
  private
    FGLSLProg: TVKProgramHandle;
    FParam: TVKSLShaderParameter;
    FActiveVarying: TStrings;
    FTransformFeedBackMode: TVKTransformFeedBackMode;

    FOnInitialize: TVKSLShaderEvent;
    FOnApply: TVKSLShaderEvent;
    FOnUnApply: TVKSLShaderUnApplyEvent;
    FOnInitializeEx: TVKSLShaderEventEx;
    FOnApplyEx: TVKSLShaderEventEx;

    function GetParam(const Index: string): TVKSLShaderParameter;
    function GetDirectParam(const Index: Cardinal): TVKSLShaderParameter;
    procedure OnChangeActiveVarying(Sender: TObject);
  protected
    property OnApply: TVKSLShaderEvent read FOnApply write FOnApply;
    property OnUnApply: TVKSLShaderUnApplyEvent read FOnUnApply write FOnUnApply;
    property OnInitialize: TVKSLShaderEvent read FOnInitialize write FOnInitialize;
    property OnInitializeEx: TVKSLShaderEventEx read FOnInitializeEx write FOnInitializeEx;
    property OnApplyEx: TVKSLShaderEventEx read FOnApplyEx write FOnApplyEx;

    function GetGLSLProg: TVKProgramHandle; virtual;
    function GetCurrentParam: TVKSLShaderParameter; virtual;
    procedure SetActiveVarying(const Value: TStrings);
    procedure SetTransformFeedBackMode(const Value: TVKTransformFeedBackMode);
    procedure DoInitialize(var rci: TRenderContextInfo; Sender: TObject); override;
    procedure DoFinalize; override;
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ShaderSupported: Boolean; override;
    function GetActiveAttribs: TVKActiveAttribArray;

    property Param[const Index: string]: TVKSLShaderParameter read GetParam;
    property DirectParam[const Index: Cardinal]: TVKSLShaderParameter read GetDirectParam;
    property ActiveVarying: TStrings read FActiveVarying write SetActiveVarying;
    property TransformFeedBackMode: TVKTransformFeedBackMode read FTransformFeedBackMode write SetTransformFeedBackMode default tfbmInterleaved;
  end;


  { Wrapper around a parameter of a GLSL program. }
  TVKSLShaderParameter = class(TVKCustomShaderParameter)
  private
    { Private Declarations }
    FGLSLProg: TVKProgramHandle;
    FParameterID: GLInt;
  protected
    { Protected Declarations }
    function GetAsVector1f: Single; override;
    function GetAsVector2f: TVector2f; override;
    function GetAsVector3f: TVector3f; override;
    function GetAsVector4f: TVector; override;

    function GetAsVector1i: Integer; override;
    function GetAsVector2i: TVector2i; override;
    function GetAsVector3i: TVector3i; override;
    function GetAsVector4i: TVector4i; override;

    function GetAsVector1ui: GLuint; override;
    function GetAsVector2ui: TVector2ui; override;
    function GetAsVector3ui: TVector3ui; override;
    function GetAsVector4ui: TVector4ui; override;

    procedure SetAsVector1f(const Value: Single); override;
    procedure SetAsVector2f(const Value: TVector2f); override;
    procedure SetAsVector3f(const Value: TVector3f); override;
    procedure SetAsVector4f(const Value: TVector4f); override;

    procedure SetAsVector1i(const Value: Integer); override;
    procedure SetAsVector2i(const Value: TVector2i); override;
    procedure SetAsVector3i(const Value: TVector3i); override;
    procedure SetAsVector4i(const Value: TVector4i); override;

    procedure SetAsVector1ui(const Value: GLuint); override;
    procedure SetAsVector2ui(const Value: TVector2ui); override;
    procedure SetAsVector3ui(const Value: TVector3ui); override;
    procedure SetAsVector4ui(const Value: TVector4ui); override;

    function GetAsMatrix2f: TMatrix2f; override;
    function GetAsMatrix3f: TMatrix3f; override;
    function GetAsMatrix4f: TMatrix4f; override;
    procedure SetAsMatrix2f(const Value: TMatrix2f); override;
    procedure SetAsMatrix3f(const Value: TMatrix3f); override;
    procedure SetAsMatrix4f(const Value: TMatrix4f); override;

    function GetAsCustomTexture(const TextureIndex: Integer;
      TextureTarget: TVKTextureTarget): Cardinal; override;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      TextureTarget: TVKTextureTarget; const Value: Cardinal); override;

    function GetAsUniformBuffer: GLenum; override;
    procedure SetAsUniformBuffer( UBO: GLenum); override;

   public
     // Nothing here ...yet.
   end;

  TVKSLShader = class(TVKCustomGLSLShader)
  published
    property FragmentProgram;
    property VertexProgram;
    property GeometryProgram;    

    property OnApply;
    property OnApplyEx;
    property OnUnApply;
    property OnInitialize;
    property OnInitializeEx;

    property ShaderStyle;
    property FailedInitAction;

    property ActiveVarying;
    property TransformFeedBackMode;
  end;


implementation

uses
  VKS.State;

{ TVKCustomGLSLShader }

procedure TVKCustomGLSLShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
begin
  FGLSLProg.UseProgramObject;
  if Assigned(FOnApply) then
    FOnApply(Self);
  if Assigned(FOnApplyEx) then
    FOnApplyEx(Self, Sender);
end;


procedure TVKCustomGLSLShader.DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
const
  cBufferMode: array[tfbmInterleaved..tfbmSeparate] of GLenum = (
    GL_INTERLEAVED_ATTRIBS_EXT, GL_SEPARATE_ATTRIBS_EXT);
var
  i, NumVarying: Integer;
  sVaryings: array of AnsiString;
  pVaryings: array of PGLChar;
begin
  try
    if not ShaderSupported then
      HandleFailedInitialization
    else
    try
      FGLSLProg.AllocateHandle;
      if FGLSLProg.IsDataNeedUpdate then
      begin
        if Name <> '' then
          FGLSLProg.Name := Name
        else
          FGLSLProg.Name := ClassName;

        FGLSLProg.DetachAllObject;
        if VertexProgram.Enabled then
          FGLSLProg.AddShader(TVKVertexShaderHandle, VertexProgram.Code.Text, FDebugMode);
        if FragmentProgram.Enabled then
          FGLSLProg.AddShader(TVKFragmentShaderHandle, FragmentProgram.Code.Text, FDebugMode);
        if GeometryProgram.Enabled then
          FGLSLProg.AddShader(TVKGeometryShaderHandle, GeometryProgram.Code.Text, FDebugMode);

        if VertexProgram.Enabled or FragmentProgram.Enabled or GeometryProgram.Enabled then
        begin
          if GeometryProgram.Enabled then
          begin
            GL.ProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_INPUT_TYPE_EXT,
              cGLgsInTypes[GeometryProgram.InputPrimitiveType]);
            GL.ProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_OUTPUT_TYPE_EXT,
              cGLgsOutTypes[GeometryProgram.OutputPrimitiveType]);
            GL.ProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_VERTICES_OUT_EXT,
              GeometryProgram.VerticesOut);
          end;

          NumVarying := FActiveVarying.Count;
          if NumVarying > 0 then
          begin
            // Activate varying
            SetLength(sVaryings, NumVarying);
            SetLength(pVaryings, NumVarying);
            for i := 0 to NumVarying - 1 do
            begin
              sVaryings[i] := AnsiString(FActiveVarying.Strings[i]) + #0;
              pVaryings[i] := PAnsiChar( sVaryings[i] );
            end;
            GL.TransformFeedbackVaryings(
              FGLSLProg.Handle, NumVarying, @pVaryings[0],
              cBufferMode[FTransformFeedBackMode] );
          end;

          if (not FGLSLProg.LinkProgram) then
            raise EGLSLShaderException.Create(FGLSLProg.InfoLog);
        end;
        FGLSLProg.NotifyDataUpdated;
      end;
    except
      on E: Exception do
      begin
        Enabled := False;
        HandleFailedInitialization(E.Message);
      end;
    end;

  finally
    if Enabled then
    try
      if Assigned(FOnInitialize) then
      begin
        FGLSLProg.UseProgramObject;
        FOnInitialize(Self);
        FGLSLProg.EndUseProgramObject;
      end;
      if Assigned(FOnInitializeEx) then
      begin
        FGLSLProg.UseProgramObject;
        FOnInitializeEx(Self, Sender);
        FGLSLProg.EndUseProgramObject;
      end;
      if (not FGLSLProg.ValidateProgram) then
        raise EGLSLShaderException.Create(FGLSLProg.InfoLog);
    except
      on E: Exception do
      begin
        Enabled := False;
        HandleFailedInitialization(E.Message);
      end;
    end;
  end;
end;


function TVKCustomGLSLShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnUnApply) then
    FOnUnApply(Self, Result);
  if not Result then
    FGLSLProg.EndUseProgramObject;
end;


function TVKCustomGLSLShader.ShaderSupported: Boolean;
begin
  Result := (GL.ARB_shader_objects and GL.ARB_vertex_program and
             GL.ARB_vertex_shader and GL.ARB_fragment_shader);
end;

function TVKCustomGLSLShader.GetActiveAttribs: TVKActiveAttribArray;
var
  LRci: TRenderContextInfo;
  i, j: Integer;
  buff: array[0..127] of AnsiChar;
  len: GLsizei;
  max: GLInt;
  glType: GLEnum;
begin
  DoInitialize(LRci, Self);

  SetLength(Result, 16);
  j := 0;
  if FGLSLProg.Handle<>0 then
  begin
    GL.GetProgramiv(FGLSLProg.Handle, GL_ACTIVE_ATTRIBUTES, @max);
    for i := 0 to 16 - 1 do
    if i<max then
    begin
      GL.GetActiveAttrib(FGLSLProg.Handle, i, Length(buff), @len, @Result[j].Size,
        @glType, @buff[0]);
      if glType > 0 then
        with Result[j] do
        begin
          case glType of
            GL_FLOAT: AType := GLSLType1F;
            GL_FLOAT_VEC2: AType := GLSLType2F;
            GL_FLOAT_VEC3: AType := GLSLType3F;
            GL_FLOAT_VEC4: AType := GLSLType4F;
            GL_INT: AType := GLSLType1I;
            GL_INT_VEC2: AType := GLSLType2I;
            GL_INT_VEC3: AType := GLSLType3I;
            GL_INT_VEC4: AType := GLSLType4I;
            GL_UNSIGNED_INT: AType := GLSLType1UI;
            GL_UNSIGNED_INT_VEC2: AType := GLSLType2UI;
            GL_UNSIGNED_INT_VEC3: AType := GLSLType3UI;
            GL_UNSIGNED_INT_VEC4: AType := GLSLType4UI;
            GL_BOOL: AType := GLSLType1I;
            GL_BOOL_VEC2: AType := GLSLType2I;
            GL_BOOL_VEC3: AType := GLSLType3I;
            GL_BOOL_VEC4: AType := GLSLType4I;
            GL_FLOAT_MAT2: AType := GLSLTypeMat2F;
            GL_FLOAT_MAT3: AType := GLSLTypeMat3F;
            GL_FLOAT_MAT4: AType := GLSLTypeMat4F;
          end;
          Name := Copy(string(buff), 0, len);
          Location := i;
          Inc(j);
        end;
    end;
  end;
  SetLength(Result, j);
end;

procedure TVKCustomGLSLShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TVKCustomGLSLShader then
  begin
    FreeAndNil(FGLSLProg); //just free the handle for it to be recreated on next initialization
  end;
end;

procedure TVKCustomGLSLShader.DoFinalize;
begin
  inherited;
  if Assigned(FGLSLProg) then
    FGLSLProg.NotifyChangesOfData;
end;

function TVKCustomGLSLShader.GetGLSLProg: TVKProgramHandle;
begin
  Result := FGLSLProg;
end;

function TVKCustomGLSLShader.GetParam(
  const Index: string): TVKSLShaderParameter;
begin
  FParam.FParameterID := FGLSLProg.GetUniformLocation(Index);
  Result := FParam;
end;

function TVKCustomGLSLShader.GetDirectParam(
  const Index: Cardinal): TVKSLShaderParameter;
begin
  FParam.FParameterID := Index;
  Result := FParam;
end;

function TVKCustomGLSLShader.GetCurrentParam: TVKSLShaderParameter;
begin
  Result := FParam;
end;

constructor TVKCustomGLSLShader.Create(AOwner: TComponent);
begin
  inherited;
  FGLSLProg := TVKProgramHandle.Create;
  FParam := TVKSLShaderParameter.Create;
  FParam.FGLSLProg := FGLSLProg;
  FActiveVarying := TStringList.Create;
  TStringList(FActiveVarying).OnChange := OnChangeActiveVarying;
  FTransformFeedBackMode := tfbmInterleaved;
end;

destructor TVKCustomGLSLShader.Destroy;
begin
  FreeAndNil(FGLSLProg);
  FreeAndNil(FParam);
  FreeAndNil(FActiveVarying);
  inherited;
end;

procedure TVKCustomGLSLShader.SetActiveVarying(const Value: TStrings);
begin
  FActiveVarying.Assign(Value);
  NotifyChange(Self);
end;

procedure TVKCustomGLSLShader.SetTransformFeedBackMode(const Value: TVKTransformFeedBackMode);
begin
  if Value <> FTransformFeedBackMode then
  begin
    FTransformFeedBackMode := Value;
    NotifyChange(Self);
  end;
end;

procedure TVKCustomGLSLShader.OnChangeActiveVarying(Sender: TObject);
begin
  NotifyChange(Self);
end;

{ TVKSLShaderParameter }

function TVKSLShaderParameter.GetAsCustomTexture(
  const TextureIndex: Integer; TextureTarget: TVKTextureTarget): Cardinal;
begin
  GL.GetUniformiv(FGLSLProg.Handle, TextureIndex, @Result);
end;

function TVKSLShaderParameter.GetAsMatrix2f: TMatrix2f;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TVKSLShaderParameter.GetAsMatrix3f: TMatrix3f;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TVKSLShaderParameter.GetAsMatrix4f: TMatrix4f;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TVKSLShaderParameter.GetAsVector1f: Single;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TVKSLShaderParameter.GetAsVector1i: Integer;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TVKSLShaderParameter.GetAsVector2f: TVector2f;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TVKSLShaderParameter.GetAsVector2i: TVector2i;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TVKSLShaderParameter.GetAsVector3f: TVector3f;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TVKSLShaderParameter.GetAsVector3i: TVector3i;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TVKSLShaderParameter.GetAsVector4f: TVector;
begin
  GL.GetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TVKSLShaderParameter.GetAsVector4i: TVector4i;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TVKSLShaderParameter.SetAsCustomTexture(
  const TextureIndex: Integer; TextureTarget: TVKTextureTarget;
  const Value: Cardinal);
begin
  CurrentGLContext.GLStates.TextureBinding[TextureIndex, TextureTarget] := Value;
  GL.Uniform1i(FParameterID, TextureIndex);
end;

procedure TVKSLShaderParameter.SetAsMatrix2f(const Value: TMatrix2f);
begin
  GL.UniformMatrix2fv(FParameterID, 1, False, @Value);
end;

procedure TVKSLShaderParameter.SetAsMatrix3f(const Value: TMatrix3f);
begin
  GL.UniformMatrix3fv(FParameterID, 1, False, @Value);
end;

procedure TVKSLShaderParameter.SetAsMatrix4f(const Value: TMatrix4f);
begin
  GL.UniformMatrix4fv(FParameterID, 1, False, @Value);
end;

procedure TVKSLShaderParameter.SetAsVector1f(const Value: Single);
begin
  GL.Uniform1f(FParameterID, Value);
end;

procedure TVKSLShaderParameter.SetAsVector1i(const Value: Integer);
begin
  GL.Uniform1i(FParameterID, Value);
end;

procedure TVKSLShaderParameter.SetAsVector2f(const Value: TVector2f);
begin
  GL.Uniform2f(FParameterID, Value.V[0], Value.V[1]);
end;

procedure TVKSLShaderParameter.SetAsVector2i(const Value: TVector2i);
begin
  GL.Uniform2i(FParameterID, Value.V[0], Value.V[1]);
end;

procedure TVKSLShaderParameter.SetAsVector3f(const Value: TVector3f);
begin
  GL.Uniform3f(FParameterID, Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TVKSLShaderParameter.SetAsVector3i(const Value: TVector3i);
begin
  GL.Uniform3i(FParameterID, Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TVKSLShaderParameter.SetAsVector4f(const Value: TVector4f);
begin
  GL.Uniform4f(FParameterID, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

procedure TVKSLShaderParameter.SetAsVector4i(const Value: TVector4i);
begin
  GL.Uniform4i(FParameterID, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

function TVKSLShaderParameter.GetAsUniformBuffer: GLenum;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TVKSLShaderParameter.GetAsVector1ui: GLuint;
begin
  GL.GetUniformuiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TVKSLShaderParameter.SetAsVector1ui(const Value: GLuint);
begin
  GL.Uniform1ui(FParameterID, Value);
end;

function TVKSLShaderParameter.GetAsVector2ui: TVector2ui;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TVKSLShaderParameter.SetAsVector2ui(const Value: TVector2ui);
begin
  GL.Uniform2ui(FParameterID, Value.V[0], Value.V[1]);
end;

function TVKSLShaderParameter.GetAsVector3ui: TVector3ui;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TVKSLShaderParameter.SetAsVector3ui(const Value: TVector3ui);
begin
  GL.Uniform3ui(FParameterID, Value.V[0], Value.V[1], Value.V[2]);
end;

function TVKSLShaderParameter.GetAsVector4ui: TVector4ui;
begin
  GL.GetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TVKSLShaderParameter.SetAsVector4ui(const Value: TVector4ui);
begin
  GL.Uniform4ui(FParameterID, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

procedure TVKSLShaderParameter.SetAsUniformBuffer(UBO: Cardinal);
begin
  CurrentGLContext.GLStates.UniformBufferBinding := UBO;
  GL.UniformBuffer(FGLSLProg.Handle, FParameterID, UBO);
end;

initialization
  RegisterClasses([TVKCustomGLSLShader, TVKSLShader]);

end.
