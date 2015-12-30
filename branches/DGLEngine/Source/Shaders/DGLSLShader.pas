//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{: DGLSLShader<p>

    TDGLSLShader is a wrapper for GLS shaders.<p>

	<b>History : </b><font size=-1><ul>
	  <li>18/12/15 - JD - Imported and updated from GLScene
	                    - Added support OpenGL 4.0 Subroutine
							        - Added support OpenGL 4.1 AtomicCounter
                      - Added support OpenGL 4.4 SSBO
  <ul><font>
}
unit DGLSLShader;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils,

  // GLS
  DGLSLog, DGLContext, DGLContextHandles, DGLRenderContextInfo, dglOpenGL,
  DGLTypes,
  DGLVectorMaths, DGLVectorTypes, DGLVectorLists, DGLTextureFormat, DGLMaterial,
  DGLCustomShader,DGLSLParameters;


type
  TDGLSLShaderParameter = class;
  // For GLSL Version #150 and up (Opengl Version >=3.0)
 // TDGLSLShaderAttrib = class;

  TDGLCustomGLSLShader = class;
  EGLSLShaderException = class(EGLCustomShaderException);

  TDGLSLShaderEvent = procedure(Shader: TDGLCustomGLSLShader) of object;
  TDGLSLShaderUnApplyEvent = procedure(Shader: TDGLCustomGLSLShader;var ThereAreMorePasses: Boolean) of object;

  TDGLSLShaderEventEx = procedure(Shader: TDGLCustomGLSLShader;
    Sender: TObject) of object;

  TDGLActiveAttrib = record
    Name: string;
    Size: GLInt;
    AType: TDGLSLDataType;
    Location: Integer;
  end;

  TDGLActiveAttribArray = array of TDGLActiveAttrib;

  //--------------------------------------------------------------
  {GLSCENE UNIFORM BUFFER OBJECT PARAMETERS }
//  TDGLUniformBufferObjectData = record
//     CameraPosition : TAffineVector;
//     LightPosition  : TAffineVector;
//  end;

  {SHADER INIT }
  //#version 150
  // ...
  //  // it called uniform block or interface block
  //  layout (std140) uniform glscene_shader_data
  //  {
  //    vec4 camera_position;
  //    vec4 light_position;
  //  };

  //-------------------------------------------------------------

  TDGLCustomGLSLShader = class(TDGLCustomShader)
  private
    FGLSLProg: TDGLProgramHandle;
    FParam: TDGLSLShaderParameter;

    FActiveVarying: TStrings;

    FTransformFeedBackMode: TGLTransformFeedBackMode;

   // FAttrib : TDGLSLShaderAttrib;

  //  FActiveAttribArray : TGLActiveAttribArray;

    FOnInitialize: TDGLSLShaderEvent;
    FOnApply: TDGLSLShaderEvent;
    FOnUnApply: TDGLSLShaderUnApplyEvent;
    FOnInitializeEx: TDGLSLShaderEventEx;
    FOnApplyEx: TDGLSLShaderEventEx;

    FNextTexIndex : integer; // for auto texture unit indicing

    function GetParam(const Index: string): TDGLSLShaderParameter;
    //function GetAttrib(const Index: string): TDGLSLShaderAttrib;

    function GetDirectParam(const Index: Cardinal): TDGLSLShaderParameter;
    procedure OnChangeActiveVarying(Sender: TObject);
  protected
    property OnApply: TDGLSLShaderEvent read FOnApply write FOnApply;
    property OnUnApply: TDGLSLShaderUnApplyEvent read FOnUnApply write FOnUnApply;
    property OnInitialize: TDGLSLShaderEvent read FOnInitialize write FOnInitialize;
    property OnInitializeEx: TDGLSLShaderEventEx read FOnInitializeEx write FOnInitializeEx;
    property OnApplyEx: TDGLSLShaderEventEx read FOnApplyEx write FOnApplyEx;

    function GetGLSLProg: TDGLProgramHandle; virtual;
    function GetCurrentParam: TDGLSLShaderParameter; virtual;
    //function GetCurrentAttrib: TGLSLShaderAttrib; virtual;

    procedure SetActiveVarying(const Value: TStrings);

    procedure SetTransformFeedBackMode(const Value: TGLTransformFeedBackMode);

    procedure DoInitialize(var rci: TRenderContextInfo; Sender: TObject); override;
    procedure DoFinalize; override;
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ShaderSupported: Boolean; override;

    function GetAttribLocation(const aName: string): Integer;
    function GetActiveAttribs: TDGLActiveAttribArray;
    procedure BindAttribLocation(index: Integer; const aName: string);
    procedure BindFragDataLocation(index: Integer; const aName: string);
   // procedure SetAttribLocation(const Location: Integer; const Buffer: Pointer);

    // SetTex() sets texture with automatic book-keeping of texture unit indices.
    // Users can just call SetTex() in the OnApply event without keeping track of texture unit indices.
    // Call from OnApply() only.
//    procedure SetTex(TexParamName : String; Tex : TDGLTexture); overload;
//    procedure SetTex(TexParamName : String; Mat : TDGLLibMaterial); overload;
//    procedure SetTex(TexParam : TDGLSLShaderParameter; Tex : TDGLTexture); overload;
//    procedure SetTex(TexParam : TDGLSLShaderParameter; Mat : TDGLLibMaterial); overload;

    //Procedure GetAttribLocation
  //  property Attrib[const Index: string]: TDGLSLShaderAttrib read GetAttrib;
    property Param[const Index: string]: TDGLSLShaderParameter read GetParam;
    property DirectParam[const Index: Cardinal]: TDGLSLShaderParameter read GetDirectParam;
    property ActiveVarying: TStrings read FActiveVarying write SetActiveVarying;
    property TransformFeedBackMode: TGLTransformFeedBackMode read FTransformFeedBackMode write SetTransformFeedBackMode default tfbmInterleaved;
  end;

  {: Wrapper around a parameter of a GLSL program. }
//  TGLSLShaderAttrib = class(TGLCustomShaderAttrib)
//  private
//    { Private Declarations }
//    FGLSLProg: TGLProgramHandle;
//
//    FName: string;
//    FSize: GLInt;
//    FBufferIdx : GLuInt;
//    FType: TGLSLDataType;
//    FLocation: Integer;
//
//  protected
//    { Protected Declarations }
//  public
////    procedure SetAsVector4fArray(const Value:Pointer;ItemsCount:Integer);
//    procedure SetAsVector4fArray(const Value:TAffineVectorList);override;
//
//    procedure SetAsVector1f(const Value: Single); override;
//    procedure SetAsVector2f(const Value: TVector2f); override;
//    procedure SetAsVector3f(const Value: TVector3f); override;
//    procedure SetAsVector4f(const Value: TVector4f); override;
//
//    procedure SetAsVector1i(const Value: Integer); override;
//    procedure SetAsVector2i(const Value: TVector2i); override;
//    procedure SetAsVector3i(const Value: TVector3i); override;
//    procedure SetAsVector4i(const Value: TVector4i); override;
//
//    procedure SetAsVector1ui(const Value: GLuint); override;
//    procedure SetAsVector2ui(const Value: TVector2ui); override;
//    procedure SetAsVector3ui(const Value: TVector3ui); override;
//    procedure SetAsVector4ui(const Value: TVector4ui); override;
//
//    procedure SetAsMatrix2f(const Value: TMatrix2f); override;
//    procedure SetAsMatrix3f(const Value: TMatrix3f); override;
//    procedure SetAsMatrix4f(const Value: TMatrix4f); override;
//  end;

  {: Wrapper around a parameter of a GLSL program. }
  TDGLSLShaderParameter = class(TObject)//class(TDGLCustomShaderParameter) ,IShaderParameter
  private
    { Private Declarations }
    FGLSLProg: TDGLProgramHandle;
    FParameterID: GLInt;
  protected
    { Protected Declarations }
    function GetAsVector1f: Single;
    function GetAsVector2f: TVector2f;
    function GetAsVector3f: TVector3f;
    function GetAsVector4f: TVector;

    function GetAsVector1i: Integer;
    function GetAsVector2i: TVector2i;
    function GetAsVector3i: TVector3i;
    function GetAsVector4i: TVector4i;

    function GetAsVector1ui: GLuint;
    function GetAsVector2ui: TVector2ui;
    function GetAsVector3ui: TVector3ui;
    function GetAsVector4ui: TVector4ui;

    procedure SetAsVector1f(const Value: Single);
    procedure SetAsVector2f(const Value: TVector2f);
    procedure SetAsVector3f(const Value: TVector3f);
    procedure SetAsVector4f(const Value: TVector4f);

    procedure SetAsVector1i(const Value: Integer);
    procedure SetAsVector2i(const Value: TVector2i);
    procedure SetAsVector3i(const Value: TVector3i);
    procedure SetAsVector4i(const Value: TVector4i);

    procedure SetAsVector1ui(const Value: GLuint);
    procedure SetAsVector2ui(const Value: TVector2ui);
    procedure SetAsVector3ui(const Value: TVector3ui);
    procedure SetAsVector4ui(const Value: TVector4ui);

    function GetAsMatrix2f: TMatrix2f;
    function GetAsMatrix3f: TMatrix3f;
    function GetAsMatrix4f: TMatrix4f;
    procedure SetAsMatrix2f(const Value: TMatrix2f);
    procedure SetAsMatrix3f(const Value: TMatrix3f);
    procedure SetAsMatrix4f(const Value: TMatrix4f);

    function GetAsCustomTexture(const TextureIndex: Integer; TextureTarget: TDGLTextureTarget): Cardinal;
    procedure SetAsCustomTexture(const TextureIndex: Integer; TextureTarget: TDGLTextureTarget; const Value: Cardinal);

    // OpenGL 3.0 Uniform Buffer Object Support
	  function GetAsUniformBuffer: GLenum;
    procedure SetAsUniformBuffer( UBO: GLenum);
  	//procedure ConnectGLSCeneUBO(UBO:Cardinal);//bind

  	// OpenGL 4.0 SubRoutine Support
	  function GetAsSubRoutineIndex(aName:String):Cardinal;
    procedure SetAsSubRoutineIndex(Index:Cardinal);

     // @TODO
	   // OpenGL 4.1 Atomic Counter Support


   public
     // Nothing here ...yet.
     //    glscene_shader_data : TGLUniformBufferObjectData;
     //    GLSceneUBOIndex : Cardinal;
   end;

  TDGLSLShader = class(TDGLCustomGLSLShader)
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
//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------

uses
  DGLState;

{ TGLCustomGLSLShader }

procedure TDGLCustomGLSLShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
begin
  FGLSLProg.UseProgramObject;
  FNextTexIndex := 0;
  if Assigned(FOnApply) then
    FOnApply(Self);
  if Assigned(FOnApplyEx) then
    FOnApplyEx(Self, Sender);
end;


procedure TDGLCustomGLSLShader.DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
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
          FGLSLProg.AddShader(TDGLVertexShaderHandle, VertexProgram.Code.Text, FDebugMode);
        if FragmentProgram.Enabled then
          FGLSLProg.AddShader(TDGLFragmentShaderHandle, FragmentProgram.Code.Text, FDebugMode);
        if GeometryProgram.Enabled then
          FGLSLProg.AddShader(TDGLGeometryShaderHandle, GeometryProgram.Code.Text, FDebugMode);

        if VertexProgram.Enabled or FragmentProgram.Enabled or GeometryProgram.Enabled then
        begin
          if GeometryProgram.Enabled then
          begin
            glProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_INPUT_TYPE_EXT,
              cGLgsInTypes[GeometryProgram.InputPrimitiveType]);
            glProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_OUTPUT_TYPE_EXT,
              cGLgsOutTypes[GeometryProgram.OutputPrimitiveType]);
            glProgramParameteri(FGLSLProg.Handle, GL_GEOMETRY_VERTICES_OUT_EXT,
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
            glTransformFeedbackVaryings(
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


function TDGLCustomGLSLShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnUnApply) then
    FOnUnApply(Self, Result);
  if not Result then
    FGLSLProg.EndUseProgramObject;
end;


function TDGLCustomGLSLShader.ShaderSupported: Boolean;
begin
  Result := (dglCheckExtension('ARB_shader_objects') and dglCheckExtension('ARB_vertex_program') and
             dglCheckExtension('ARB_vertex_shader') and dglCheckExtension('ARB_fragment_shader'));
end;

function TDGLCustomGLSLShader.GetActiveAttribs: TDGLActiveAttribArray;
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
    glGetProgramiv(FGLSLProg.Handle, GL_ACTIVE_ATTRIBUTES, @max);
    for i := 0 to 16 - 1 do
    if i<max then
    begin
      glGetActiveAttrib(FGLSLProg.Handle, i, Length(buff), len, Result[j].Size,glType, @buff[0]);
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

procedure TDGLCustomGLSLShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TDGLCustomGLSLShader then
  begin
    FreeAndNil(FGLSLProg); //just free the handle for it to be recreated on next initialization
  end;
end;

procedure TDGLCustomGLSLShader.DoFinalize;
begin
  inherited;
  if Assigned(FGLSLProg) then
    FGLSLProg.NotifyChangesOfData;
end;

function TDGLCustomGLSLShader.GetGLSLProg: TDGLProgramHandle;
begin
  Result := FGLSLProg;
end;

function TDGLCustomGLSLShader.GetParam(const Index: string): TDGLSLShaderParameter;
begin
  FParam.FParameterID := FGLSLProg.GetUniformLocation(Index);
  Result := FParam;
end;

//function TDGLCustomGLSLShader.GetAttrib(const Index: string): TDGLSLShaderAttrib;
//var
// // LRci: TRenderContextInfo;
//  i:integer;
//  //j: Integer;
//  buff: array[0..127] of AnsiChar;
//  len: GLsizei;
////  max: GLInt;
//  glType: GLEnum;
//begin
// // DoInitialize(LRci, Self);
//  if FGLSLProg.Handle<>0 then
//  begin
//    FAttrib.FName:=Index;
//    i := glGetAttribLocation(FGLSLProg.Handle, PGLChar(TGLString(Index)));
//    glGetActiveAttrib(FGLSLProg.Handle, i, Length(buff), @len, @FAttrib.FSize,@glType, @buff[0]);
//    if glType > 0 then
//    begin
//      with FAttrib do
//      begin
//        case glType of
//          GL_FLOAT: FAttrib.FType := GLSLType1F;
//          GL_FLOAT_VEC2: FAttrib.FType := GLSLType2F;
//          GL_FLOAT_VEC3: FAttrib.FType := GLSLType3F;
//          GL_FLOAT_VEC4: FAttrib.FType := GLSLType4F;
//          GL_INT: FAttrib.FType := GLSLType1I;
//          GL_INT_VEC2: FAttrib.FType := GLSLType2I;
//          GL_INT_VEC3: FAttrib.FType := GLSLType3I;
//          GL_INT_VEC4: FAttrib.FType := GLSLType4I;
//          GL_UNSIGNED_INT: FAttrib.FType := GLSLType1UI;
//          GL_UNSIGNED_INT_VEC2: FAttrib.FType := GLSLType2UI;
//          GL_UNSIGNED_INT_VEC3: FAttrib.FType := GLSLType3UI;
//          GL_UNSIGNED_INT_VEC4: FAttrib.FType := GLSLType4UI;
//          GL_BOOL: FAttrib.FType := GLSLType1I;
//          GL_BOOL_VEC2: FAttrib.FType := GLSLType2I;
//          GL_BOOL_VEC3: FAttrib.FType := GLSLType3I;
//          GL_BOOL_VEC4: FAttrib.FType := GLSLType4I;
//          GL_FLOAT_MAT2: FAttrib.FType := GLSLTypeMat2F;
//          GL_FLOAT_MAT3: FAttrib.FType := GLSLTypeMat3F;
//          GL_FLOAT_MAT4: FAttrib.FType := GLSLTypeMat4F;
//        end;
//        FAttrib.FName := Copy(string(buff), 0, len);
//        FAttrib.FLocation := i;
//      end;
//    end;
//  end;
//  Result := FAttrib;
//  DGLSLogger.LogInfo('GLSL GetAttrib : '+FAttrib.FName+' Location : '+inttostr(FAttrib.FLocation));
//end;


function TDGLCustomGLSLShader.GetDirectParam(const Index: Cardinal): TDGLSLShaderParameter;
begin
  FParam.FParameterID := Index;
  Result := FParam;
end;

function TDGLCustomGLSLShader.GetCurrentParam: TDGLSLShaderParameter;
begin
  Result := FParam;
end;

constructor TDGLCustomGLSLShader.Create(AOwner: TComponent);
begin
  inherited;
  FGLSLProg := TDGLProgramHandle.Create;
  FParam := TDGLSLShaderParameter.Create;
  FParam.FGLSLProg := FGLSLProg;
  FActiveVarying := TStringList.Create;
  TStringList(FActiveVarying).OnChange := OnChangeActiveVarying;
  FTransformFeedBackMode := tfbmInterleaved;
end;

destructor TDGLCustomGLSLShader.Destroy;
begin
  FreeAndNil(FGLSLProg);
  FreeAndNil(FParam);
  FreeAndNil(FActiveVarying);
  inherited;
end;

procedure TDGLCustomGLSLShader.SetActiveVarying(const Value: TStrings);
begin
  FActiveVarying.Assign(Value);
  NotifyChange(Self);
end;


function TDGLCustomGLSLShader.GetAttribLocation(const aName: string): Integer;
begin
  result := FGLSLProg.GetAttribLocation(aName);
end;

procedure TDGLCustomGLSLShader.BindAttribLocation(Index:Integer;const aName:String);
begin
  FGLSLProg.BindAttribLocation(Index,aName);
end;

procedure TDGLCustomGLSLShader.BindFragDataLocation(index: Integer; const aName: string);
begin
  FGLSLProg.BindFragDataLocation(index,aName);
end;

//procedure TGLCustomGLSLShader.SetAttribLocation(const Location: Integer; const Buffer: Pointer);
//begin
//  FGLSLProg.SetAttribLocation(Location,Buffer);
//end;

procedure TDGLCustomGLSLShader.SetTransformFeedBackMode(const Value: TGLTransformFeedBackMode);
begin
  if Value <> FTransformFeedBackMode then
  begin
    FTransformFeedBackMode := Value;
    NotifyChange(Self);
  end;
end;

procedure TDGLCustomGLSLShader.OnChangeActiveVarying(Sender: TObject);
begin
  NotifyChange(Self);
end;

//procedure TDGLCustomGLSLShader.SetTex(TexParam : TDGLSLShaderParameter; Tex : TDGLTexture);
//begin
//  TexParam.AsTexture[FNextTexIndex] := Tex;
//  inc(FNextTexIndex);
//end;
//
//procedure TDGLCustomGLSLShader.SetTex(TexParam : TDGLSLShaderParameter; Mat : TDGLLibMaterial);
//begin
//  SetTex(TexParam, Mat.Material.Texture);
//end;
//
//procedure TDGLCustomGLSLShader.SetTex(TexParamName: String; Tex: TDGLTexture);
//begin
//  SetTex(Param[TexParamName], Tex);
//end;
//
//procedure TDGLCustomGLSLShader.SetTex(TexParamName: String; Mat: TDGLLibMaterial);
//begin
//  SetTex(TexParamName, Mat.Material.Texture);
//end;

{ TGLSLShaderParameter }

function TDGLSLShaderParameter.GetAsCustomTexture(const TextureIndex: Integer; TextureTarget: TDGLTextureTarget): Cardinal;
begin
  glGetUniformiv(FGLSLProg.Handle, TextureIndex, @Result);
end;

function TDGLSLShaderParameter.GetAsMatrix2f: TMatrix2f;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TDGLSLShaderParameter.GetAsMatrix3f: TMatrix3f;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TDGLSLShaderParameter.GetAsMatrix4f: TMatrix4f;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TDGLSLShaderParameter.GetAsVector1f: Single;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TDGLSLShaderParameter.GetAsVector1i: Integer;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TDGLSLShaderParameter.GetAsVector2f: TVector2f;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TDGLSLShaderParameter.GetAsVector2i: TVector2i;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TDGLSLShaderParameter.GetAsVector3f: TVector3f;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TDGLSLShaderParameter.GetAsVector3i: TVector3i;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TDGLSLShaderParameter.GetAsVector4f: TVector;
begin
  glGetUniformfv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TDGLSLShaderParameter.GetAsVector4i: TVector4i;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TDGLSLShaderParameter.SetAsCustomTexture(const TextureIndex: Integer; TextureTarget: TDGLTextureTarget;const Value: Cardinal);
begin
  CurrentDGLContext.GLStates.TextureBinding[TextureIndex, TextureTarget] := Value;
  glUniform1i(FParameterID, TextureIndex);
end;

procedure TDGLSLShaderParameter.SetAsMatrix2f(const Value: TMatrix2f);
begin
  glUniformMatrix2fv(FParameterID, 1, False, @Value);
end;

procedure TDGLSLShaderParameter.SetAsMatrix3f(const Value: TMatrix3f);
begin
  glUniformMatrix3fv(FParameterID, 1, False, @Value);
end;

procedure TDGLSLShaderParameter.SetAsMatrix4f(const Value: TMatrix4f);
begin
  glUniformMatrix4fv(FParameterID, 1, False, @Value);
end;

procedure TDGLSLShaderParameter.SetAsVector1f(const Value: Single);
begin
  glUniform1f(FParameterID, Value);

end;

procedure TDGLSLShaderParameter.SetAsVector1i(const Value: Integer);
begin
  glUniform1i(FParameterID, Value);

end;

procedure TDGLSLShaderParameter.SetAsVector2f(const Value: TVector2f);
begin
  glUniform2f(FParameterID, Value.V[0], Value.V[1]);
end;

procedure TDGLSLShaderParameter.SetAsVector2i(const Value: TVector2i);
begin
  glUniform2i(FParameterID, Value.V[0], Value.V[1]);
end;

procedure TDGLSLShaderParameter.SetAsVector3f(const Value: TVector3f);
begin
  glUniform3f(FParameterID, Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TDGLSLShaderParameter.SetAsVector3i(const Value: TVector3i);
begin
  glUniform3i(FParameterID, Value.V[0], Value.V[1], Value.V[2]);
end;

procedure TDGLSLShaderParameter.SetAsVector4f(const Value: TVector4f);
begin
  glUniform4f(FParameterID, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

procedure TDGLSLShaderParameter.SetAsVector4i(const Value: TVector4i);
begin
  glUniform4i(FParameterID, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

function TDGLSLShaderParameter.GetAsUniformBuffer: GLenum;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

function TDGLSLShaderParameter.GetAsVector1ui: GLuint;
begin
  glGetUniformuiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TDGLSLShaderParameter.SetAsVector1ui(const Value: GLuint);
begin
  glUniform1ui(FParameterID, Value);
end;

function TDGLSLShaderParameter.GetAsVector2ui: TVector2ui;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TDGLSLShaderParameter.SetAsVector2ui(const Value: TVector2ui);
begin
  glUniform2ui(FParameterID, Value.V[0], Value.V[1]);
end;

function TDGLSLShaderParameter.GetAsVector3ui: TVector3ui;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TDGLSLShaderParameter.SetAsVector3ui(const Value: TVector3ui);
begin
  glUniform3ui(FParameterID, Value.V[0], Value.V[1], Value.V[2]);
end;

function TDGLSLShaderParameter.GetAsVector4ui: TVector4ui;
begin
  glGetUniformiv(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TDGLSLShaderParameter.SetAsVector4ui(const Value: TVector4ui);
begin
  glUniform4ui(FParameterID, Value.V[0], Value.V[1], Value.V[2], Value.V[3]);
end;

//procedure TShaderProgram.SetGeomInputType(ProgramId, Value: GLUInt);
//begin
//  SetProgParam(ProgramId,GL_GEOMETRY_INPUT_TYPE_EXT,Value);
//end;
//
//procedure TShaderProgram.SetGeomOutputType(ProgramId, Value: GLUInt);
//begin
//  SetProgParam(ProgramId,GL_GEOMETRY_OUTPUT_TYPE_EXT,Value);
//end;
//
//procedure TShaderProgram.SetGeomVerticesOutCount(ProgramId, Value: GLUInt);
//var Temp: cardinal;
//begin
//  if Value=0 then begin
//    glGetIntegerv(GL_MAX_GEOMETRY_OUTPUT_VERTICES, @Temp);
//    SetProgParam(ProgramId,GL_GEOMETRY_VERTICES_OUT_EXT,Temp);
//  end;
//  SetProgParam(ProgramId,GL_GEOMETRY_VERTICES_OUT_EXT,Value);
//end;

//-*-----------------------------------------------------------------------------------
//procedure TShaders.SetGeomVerticesOutCount(ProgramId, Value: GLUInt);
//var Temp: cardinal;
//begin
//  if Value=0 then begin
//    glGetIntegerv(GL_MAX_GEOMETRY_OUTPUT_VERTICES, @Temp);
//    SetProgParam(ProgramId,GL_GEOMETRY_VERTICES_OUT_EXT,Temp);
//  end;
//  SetProgParam(ProgramId,GL_GEOMETRY_VERTICES_OUT_EXT,Value);
//end;
//


procedure TDGLSLShaderParameter.SetAsUniformBuffer(UBO: Cardinal);
begin
  CurrentDGLContext.GLStates.UniformBufferBinding := UBO;
  glUniformBufferEXT(FGLSLProg.Handle, FParameterID, UBO);  // ???? NOT SURE
end;


//procedure TGLSLShaderParameter.CreateGLSceneUBO(UBO:cardinal);
//begin
//  glGenBuffers(1, @UBO);
//  glBindBuffer(GL_UNIFORM_BUFFER, UBO);
//  glBufferData(GL_UNIFORM_BUFFER, sizeof(glscene_shader_data), @glscene_shader_data, GL_DYNAMIC_COPY);
//  glBindBuffer(GL_UNIFORM_BUFFER, 0);
//end;

//procedure TGLSLShaderParameter.UpdateGLSceneUBO(UBO:cardinal);
//var p:pointer;
//begin
//  glBindBuffer(GL_UNIFORM_BUFFER, UBO);
//  p := glMapBuffer(GL_UNIFORM_BUFFER, GL_WRITE_ONLY);
//  //fillmem(p, @glscene_shader_data, sizeof(glscene_shader_data));
//  glUnmapBuffer(GL_UNIFORM_BUFFER);
//end;
//
//function TGLSLShaderParameter.GetGLSceneUBO:Cardinal;
//begin
//  //result := glGetUniformBlockIndex(FGLSLProg.Handle, 'glscene_shader_data');
//  result:=FGLSLProg.GetUniformBlockIndex('glscene_sahder_data');
//
//end;
//
//procedure TGLSLShaderParameter.ConnectGLSCeneUBO(UBO:Cardinal);
//var binding_point_index, block_index:Cardinal;
//begin
//  binding_point_index := 2;
//  block_index := GetGLSceneUBO;
//  glBindBufferBase(GL_UNIFORM_BUFFER, binding_point_index, UBO);
//  glUniformBlockBinding(FGLSLProg.Handle, block_index, binding_point_index);
//end;


function TDGLSLShaderParameter.GetAsSubRoutineIndex(aName:String):Cardinal;
begin
  //result:=glGetSubroutineIndex(FGLSLProg.Handle, GL_FRAGMENT_SHADER,  PGLChar(TGLString(aName)));
  result:=0;
end;

procedure TDGLSLShaderParameter.SetAsSubRoutineIndex(Index:Cardinal);
begin
   glUniformSubroutinesuiv(GL_FRAGMENT_SHADER, 1, @Index);
end;

{ TGLSLShaderAttrib }



//procedure TGLSLShaderAttrib.SetAsMatrix2f(const Value: TMatrix2f);
//begin
//  Assert(FType<>GLSLTypeMat2F,'Invalid Attrib Data Type');
//
//  glVertexAttribPointer(FLocation, 4, GL_FLOAT, false, 0, nil);
//
//end;
//
//procedure TGLSLShaderAttrib.SetAsMatrix3f(const Value: TMatrix3f);
//begin
//  Assert(FType<>GLSLTypeMat3F,'Invalid Attrib Data Type');
//  glVertexAttribPointer(FLocation, 9, GL_FLOAT, false, 0, nil);
//end;
//
//procedure TGLSLShaderAttrib.SetAsMatrix4f(const Value: TMatrix4f);
//begin
//  Assert(FType<>GLSLTypeMat4F,'Invalid Attrib Data Type');
//  glVertexAttribPointer(FLocation, 16, GL_FLOAT, false, 0, nil);
//end;
//
//procedure TGLSLShaderAttrib.SetAsVector1f(const Value: Single);
//begin
//  Assert(FType<>GLSLType1F,'Invalid Attrib Data Type');
//  glVertexAttribIPointer(FLocation, 1, GL_FLOAT, 0, 0);
//end;
//
//procedure TGLSLShaderAttrib.SetAsVector2f(const Value: TVector2f);
//begin
//  Assert(FType<>GLSLType2F,'Invalid Attrib Data Type');
//  glVertexAttribIPointer(FLocation, 2, GL_FLOAT, 0, nil);
//end;
//
//procedure TGLSLShaderAttrib.SetAsVector3f(const Value: TVector3f);
//begin
//  Assert(FType<>GLSLType3F,'Invalid Attrib Data Type');
//  glVertexAttribIPointer(FLocation, 3, GL_FLOAT, 0, 0);
//end;
//
//procedure TGLSLShaderAttrib.SetAsVector4f(const Value: TVector4f);
//begin
//  Assert(FType<>GLSLType4F,'Invalid Attrib Data Type');
//  glVertexAttribIPointer(FLocation, 4, GL_FLOAT, 0, nil);
//end;
//
//// Value: GLuint; // The VBO for the vertices.
////procedure TGLSLShaderAttrib.SetAsVector4fArray(const Value:Pointer;ItemsCount:Integer);
//procedure TGLSLShaderAttrib.SetAsVector4fArray(const Value:TAffineVectorList);
//Var
////  TempBuf : Array of GLFloat;
//  ItemsCount:Integer;
//begin
//  Assert(FType<>GLSLType4F,'Invalid Attrib Data Type');
//  //  CurrentGLContext.GLStates.ArrayBufferBinding := ????;
//  ItemsCount := Value.Count;
//  glGenBuffers(1, @FBufferIdx);
//  glBindBuffer(GL_ARRAY_BUFFER, FBufferIdx);
//  glBufferData(GL_ARRAY_BUFFER, ItemsCount*4*sizeof(GLfloat), Value.List, GL_STATIC_DRAW);
//  glVertexAttribIPointer(FLocation, 4, GL_FLOAT, 0, nil);
//  glEnableVertexAttribArray(FLocation);
//
// // Normally at the end of prog we need to free our buffer
// // But actually i don't say how to do.
// // perhaps we must use a List or Collection to Store Attribs instead of accessing one by one
// // Or we just need use GLState. But for now i don't say how
// //glDeleteBuffers(1, @FBufferIdx);
// //FBufferIdx := 0;
//end;
//
//procedure TGLSLShaderAttrib.SetAsVector1i(const Value: Integer);
//begin
//  Assert(FType<>GLSLType1I,'Invalid Attrib Data Type');
//  glVertexAttribIPointer(FLocation, 1, GL_INT, 0, nil);
//end;
//
//procedure TGLSLShaderAttrib.SetAsVector2i(const Value: TVector2i);
//begin
//  Assert(FType<>GLSLType2I,'Invalid Attrib Data Type');
//  glVertexAttribIPointer(FLocation, 2, GL_INT, 0, nil);
//end;
//
//procedure TGLSLShaderAttrib.SetAsVector3i(const Value: TVector3i);
//begin
//  Assert(FType<>GLSLType3I,'Invalid Attrib Data Type');
//  glVertexAttribIPointer(FLocation, 3, GL_INT, 0, nil);
//end;
//
//procedure TGLSLShaderAttrib.SetAsVector4i(const Value: TVector4i);
//begin
//  Assert(FType<>GLSLType4I,'Invalid Attrib Data Type');
//  glVertexAttribIPointer(FLocation, 4, GL_INT, 0, nil);
//end;
//
//procedure TGLSLShaderAttrib.SetAsVector1ui(const Value: GLuint);
//begin
//  Assert(FType<>GLSLType1UI,'Invalid Attrib Data Type');
//  glVertexAttribIPointer(FLocation, 1, GL_UNSIGNED_INT, 0, nil);
//end;
//
//procedure TGLSLShaderAttrib.SetAsVector2ui(const Value: TVector2ui);
//begin
//  Assert(FType<>GLSLType2UI,'Invalid Attrib Data Type');
//  glVertexAttribIPointer(FLocation, 2, GL_UNSIGNED_INT, 0, nil);
//end;
//
//procedure TGLSLShaderAttrib.SetAsVector3ui(const Value: TVector3ui);
//begin
//  Assert(FType<>GLSLType3UI,'Invalid Attrib Data Type');
//  glVertexAttribIPointer(FLocation, 3, GL_UNSIGNED_INT, 0, nil);
//end;
//
//procedure TGLSLShaderAttrib.SetAsVector4ui(const Value: TVector4ui);
//begin
//  Assert(FType<>GLSLType4UI,'Invalid Attrib Data Type');
//  glVertexAttribIPointer(FLocation, 4, GL_UNSIGNED_INT, 0, nil);
//end;



initialization
  RegisterClasses([TDGLCustomGLSLShader, TDGLSLShader]);

end.
