 // GLCgShader
{: Base Cg shader classes.<p>

   <b>History :</b><font size=-1><ul>
      <li>01/08/03 - NelC - Simplified type checking in SetAsStateMatrix and minor
                            changes.
      <li>04/07/03 - NelC - TCustomCgShader.OnInitialize. Moved properties
                            VertexProgram & FragmentProgram of TCustomCgShader
                            to published so that we can acccess them easily from
                            OnInitialize.
      <li>02/07/03 - NelC - More value-setting functions for TCgParameter.
                            OnUnApply for shader programs.
      <li>01/07/03 - NelC - TCgProgram.ListCompilation now outputs line breaks.
      <li>27/06/03 - NelC - Value-setting functions for TCgParameter,
                            TCgProgram.DirectParamByName & DirectProfile,
                            Profile property for TCgVertexProgram &
                            TCgFragmentProgram, and some minor adjustments.
      <li>24/06/03 - NelC - Initial adoptation to Cg 1.1 Final. Now automatically
                            uses latest hardware-supported profile and use callback
                            to show error message.
      <li>29/05/03 - RoC - Cg 1.1 Depreciated_api compatible
      <li>25/09/02 - EG - Cg Beta 2/2.1 compatible, now uses ARBVP
      <li>19/06/02 - EG - Improved OO wrapper
      <li>18/06/02 - EG - Creation
   </ul></font>
}
unit GLCgShader;

interface

uses
  Classes, VectorGeometry, VectorTypes, GLTexture, GLMisc, Cg, CgGL;

type
  TCustomCgShader = class;
  TCgProgram = class;
  TCgParameter = class;

  TCgApplyEvent = procedure (Sender : TCgProgram) of object;
  TCgShaderEvent = procedure (Sender : TCustomCgShader) of object;

  TcgProgramType = (ptVertex, ptFragment);

  TCGtypes = array of TCGtype;

  // Available vertex program profile
  TCgVPProfile = ( vpDetectLatest, vp20, vp30, arbvp1);

  // Available fragment program profile
  TCgFPProfile = ( fpDetectLatest, fp20, fp30, arbfp1);

  // TCgProgram
  //
  {: Wrapper around a Cg program. }
  TCgProgram = class (TGLUpdateAbleObject)
  private
    { Private Declarations }
    FCgContext : PcgContext;
    FCode : TStrings; // the Cg program itself
    FProgramName : String;
    FHandle : PCGprogram;
    FParams : TList;

    FOnApply : TCgApplyEvent;
    FOnUnApply : TCgApplyEvent;
    FOnProgramChanged : TNotifyEvent;

    FEnabled : boolean;
    FDetectProfile : boolean;

  protected
    { Protected Declarations }
    FProgramType : TcgProgramType;
    FProfile : TcgProfile;

    procedure SetCode(const val : TStrings);
    procedure SetProgramName(const val : String);
    function GetParam(index : String) : TCgParameter;

    procedure AddParamsItem(const Param : PCGParameter);
    {: Build a list of parameters used in the shader code.<p>
       Iteratively queries all parameters so that we can manage and access them
       easily. Currently only collects leaf parameters i.e. data structure is
       not retrieved. }
    procedure BuildParamsList;
    procedure ClearParamsList;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Initialize; dynamic;
    procedure Finalize;
    procedure Apply(var rci : TRenderContextInfo);
    procedure UnApply(var rci : TRenderContextInfo);

    property Param[index : String] : TCgParameter read GetParam;
    property Params : TList read FParams;

    function ParamByName(const name : String) : TCgParameter;
    function DirectParamByName(const name : String) : PCGparameter;

    function ParamCount : Integer;
    function GetProfileString : string;

    procedure LoadFromFile(const fileName : String);

    procedure ListCompilation(Output : TStrings);
    procedure ListParameters(Output : TStrings);

    {: Direct access to the profile. <p>
       Set Profile of the sub-classes to any but DetectLatest if you want to
       specify the profile directly. }
    property DirectProfile : TcgProfile read FProfile write FProfile;

    property OnProgramChanged : TNotifyEvent read FOnProgramChanged write FOnProgramChanged;

  published
    { Published Declarations }
    property Code : TStrings read FCode write SetCode;
    property ProgramName : String read FProgramName write SetProgramName;
    property Enabled : boolean read FEnabled write FEnabled default True;
    property OnApply : TCgApplyEvent read FOnApply write FOnApply;
    property OnUnApply : TCgApplyEvent read FOnUnApply write FOnUnApply;
  end;

  // TCgParameter
  //
  {: Wrapper around a Cg parameter of the main program. }
  TCgParameter = class (TObject)
  private
    { Private Declarations }
    FOwner : TCgProgram;
    FName : String;
    FHandle      : PCGparameter;
    FValueType   : TCGtype; // e.g. CG_FLOAT
    FDirection   : TCGenum; // e.g. CG_INOUT
    FVariability : TCGenum; // e.g. CG_UNIFORM
  protected
    { Protected Declarations }
    procedure CheckValueType(aType : TCGtype); overload;
    procedure CheckValueType(const types : TCGtypes); overload;

    procedure SetAsVector2f(const val : TVector2f);
    procedure SetAsVector3f(const val : TVector3f);
    procedure SetAsVector4f(const val : TVector4f);
  public
    { Public Declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    // These value-setting methods implicitly check for data type.
    procedure SetAsScalar(const val : Single);
    procedure SetAsVector(const val : TVector2f); overload;
    procedure SetAsVector(const val : TVector3f); overload;
    procedure SetAsVector(const val : TVector4f); overload;

    procedure SetAsStateMatrix(matrix, Transform: Cardinal);

    procedure SetAsTexture1D(TextureID : Cardinal);
    procedure SetAsTexture2D(TextureID : Cardinal);
    procedure SetAsTexture3D(TextureID : Cardinal);
    procedure SetAsTextureCUBE(TextureID : Cardinal);
    procedure SetAsTextureRECT(TextureID : Cardinal);

    procedure EnableTexture;
    procedure DisableTexture;

    property Owner : TCgProgram read FOwner;
    property Name : String read FName;
    property ValueType : TCGtype read FValueType;
    property Handle : PCGparameter read FHandle write FHandle;
    property Direction : TCGenum read FDirection write FDirection;
    property Variability : TCGenum read FVariability write FVariability;

    // GLScene-friendly properties
    property AsVector : TVector write SetAsVector4f; // position f.i.
    property AsAffineVector : TAffineVector write SetAsVector3f; // normal f.i.
    property AsVector2f : TVector2f write SetAsVector2f; // texCoord f.i.
  end;

  // TCgVertexProgram
  //
  TCgVertexProgram = class (TCgProgram)
  private
    FVPProfile : TCgVPProfile;
    procedure SetVPProfile(v : TCgVPProfile);
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
  published
    property Profile : TCgVPProfile read FVPProfile write SetVPProfile default vpDetectLatest;
  end;

  // TCgFragmentProgram
  //
  TCgFragmentProgram = class (TCgProgram)
  private
    FFPProfile : TCgFPProfile;
    procedure SetFPProfile(v : TCgFPProfile);
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
  published
    property Profile : TCgFPProfile read FFPProfile write SetFPProfile default fpDetectLatest;
  end;

  // TCustomCgShader
  //
  TCustomCgShader = class (TGLShader)
  private
    { Private Declarations }
    FVertexProgram : TCgVertexProgram;
    FFragmentProgram : TCgFragmentProgram;

    FOnInitialize : TCgShaderEvent;

    FDesignEnable : Boolean;

  protected
    { Protected Declarations }
    // Vertex Program
    procedure SetVertexProgram(const val : TCgVertexProgram);
    procedure SetOnApplyVertexProgram(const val : TCgApplyEvent);
    function GetOnApplyVertexProgram : TCgApplyEvent;
    procedure SetOnUnApplyVertexProgram(const val : TCgApplyEvent);
    function GetOnUnApplyVertexProgram : TCgApplyEvent;

    // Fragment Program
    procedure SetFragmentProgram(const val : TCgFragmentProgram);
    procedure SetOnApplyFragmentProgram(const val : TCgApplyEvent);
    function GetOnApplyFragmentProgram : TCgApplyEvent;
    procedure SetOnUnApplyFragmentProgram(const val : TCgApplyEvent);
    function GetOnUnApplyFragmentProgram : TCgApplyEvent;

    // OnInitialize
    function GetOnInitialize : TCgShaderEvent;
    procedure SetOnInitialize(const val : TCgShaderEvent);

    procedure DoInitialize; override;
    procedure DoFinalize; override;
    procedure DoApply(var rci : TRenderContextInfo); override;
    function  DoUnApply(var rci : TRenderContextInfo) : Boolean; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property OnApplyVertexProgram : TCgApplyEvent read GetOnApplyVertexProgram write SetOnApplyVertexProgram;
    property OnApplyFragmentProgram : TCgApplyEvent read GetOnApplyFragmentProgram write SetOnApplyFragmentProgram;

    property OnUnApplyVertexProgram : TCgApplyEvent read GetOnUnApplyVertexProgram write SetOnUnApplyVertexProgram;
    property OnUnApplyFragmentProgram : TCgApplyEvent read GetOnUnApplyFragmentProgram write SetOnUnApplyFragmentProgram;

    {: OnInitialize can be use to set parameters that need to be set once. See demo "Cg Texture" for example.}
    property OnInitialize : TCgShaderEvent read GetOnInitialize write SetOnInitialize;

  published
    { Published Declarations }
    property DesignEnable : Boolean read FDesignEnable write FDesignEnable default False;
    property VertexProgram : TCgVertexProgram read FVertexProgram write SetVertexProgram;
    property FragmentProgram : TCgFragmentProgram read FFragmentProgram write SetFragmentProgram;
  end;

  // TCgShader
  //
  TCgShader = class (TCustomCgShader)
  published
    { Published Declarations }
    property VertexProgram;
    property FragmentProgram;

    property OnApplyVertexProgram;
    property OnApplyFragmentProgram;

    property OnUnApplyVertexProgram;
    property OnUnApplyFragmentProgram;

    property OnInitialize;
  end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x, Dialogs;

var
  vCgContextCount : Integer;
  CurCgContent : PcgContext; // for reporting error line number

procedure Register;
begin
  RegisterComponents('GLScene Shaders', [TCgShader]);
end;

procedure ErrorCallBack; cdecl;
var  Msg : string;
begin
  Msg:= cgGetErrorString(cgGetError) + #10#13 + cgGetLastListing(CurCgContent);
  raise Exception.Create(Msg);
end;

// ------------------
// ------------------ TCgProgram ------------------
// ------------------

// Create
//
constructor TCgProgram.Create(AOwner: TPersistent);
begin
  inherited;
  FCode := TStringList.Create;
  TStringList(FCode).OnChange := NotifyChange;
  FParams := TList.Create;
  FEnabled := true;
  FDetectProfile := true;
end;

// Destroy
//
destructor TCgProgram.Destroy;
begin
  inherited Destroy;
  Assert(FParams.Count=0, ClassName+': bug! params unbound!');
  FParams.Free;
  FCode.Free;
end;

// SetCode
//
procedure TCgProgram.SetCode(const val : TStrings);
begin
  FCode.Assign(val);
end;

// LoadFromFile
//
procedure TCgProgram.LoadFromFile(const fileName : String);
begin
  Code.LoadFromFile(fileName);
end;

// SetProgramName
//
procedure TCgProgram.SetProgramName(const val : String);
begin
  if val<>FProgramName then
  begin
    FProgramName:=val;
    NotifyChange(Self);
  end;
end;

// AddParamsItem
//
procedure TCgProgram.AddParamsItem(const Param: PCGParameter);
var
  newParamObj : TCgParameter;
begin
  newParamObj := TCgParameter.Create;
  with newParamObj do begin
    FOwner := Self;
    FName  := StrPas(cgGetParameterName(Param));
    FHandle := Param;
    FValueType := cgGetParameterType(Param); 
    FDirection := cgGetParameterDirection(Param); 
    FVariability := cgGetParameterVariability(Param);
  end;
  FParams.Add(newParamObj);
end;

// BuildParamsList
//
procedure TCgProgram.BuildParamsList;
var
  CurParam : PCGParameter;
begin
  ClearParamsList;
  CurParam:=cgGetFirstLeafParameter(FHandle, CG_PROGRAM);

  // build params list
  while Assigned(CurParam) do begin
    AddParamsItem(CurParam);
    CurParam:=cgGetNextLeafParameter(CurParam);
  end;
end;

// ClearParamsList
//
procedure TCgProgram.ClearParamsList;
var
  i : Integer;
begin
  for i:=FParams.Count-1 downto 0 do
    TCgParameter(FParams[i]).Free;
  FParams.Clear;
end;

// GetParam
//
function TCgProgram.GetParam(index : String) : TCgParameter;
begin
  Result := ParamByName(index);
  Assert(Result<>nil, ClassName+': Invalid parameter name "'+index+'"');
end;

// ParamByName
//
function TCgProgram.ParamByName(const name : String) : TCgParameter;
var
  i : Integer;
  list : PPointerList;
begin
  Result := nil;
  list := FParams.List;
  for i:=0 to FParams.Count-1 do begin
    if TCgParameter(list[i]).Name=name then begin
      Result := TCgParameter(list[i]);
      Exit;
    end;
  end;
end;

// DirectParamByName
//
function TCgProgram.DirectParamByName(const name: String): PCGparameter;
// Returns a handle to a Cg parameter
begin
  result:=cgGetNamedParameter(FHandle, PChar(name));
end;

// ParamCount
//
function TCgProgram.ParamCount : Integer;
begin
  Result:=FParams.Count;
end;

// Initialize
//
procedure TCgProgram.Initialize;
var
  buf : String;
begin
  Assert(FCgContext=nil);

  buf := Trim(Code.Text);

  if buf='' then exit;

  // get a new context
  FCgContext := cgCreateContext;
  CurCgContent := FCgContext;
  Inc(vCgContextCount);

  try
    if FDetectProfile then // set profile to latest supported by the hardware
      case FProgramType of
        ptVertex   : FProfile := cgGLGetLatestProfile(CG_GL_VERTEX);
        ptFragment : FProfile := cgGLGetLatestProfile(CG_GL_FRAGMENT);
      end;

    cgGLSetOptimalOptions(FProfile);

    if FProgramName='' then FProgramName:='main';

    FHandle := cgCreateProgram( FCgContext, CG_SOURCE, PChar(buf), FProfile,
                                PChar(FProgramName), nil);
    cgGLLoadProgram(FHandle);

    // build parameter list for the selected program
    BuildParamsList;
  except
    cgDestroyContext(FCgContext);
    FCgContext := nil;
    Dec(vCgContextCount);
  raise;
  end;
end;

// Finalize
//
procedure TCgProgram.Finalize;
begin
  if not Assigned(FCgContext) then exit;

  FProgramName := '';
  ClearParamsList;
  cgDestroyContext(FCgContext);
  FCgContext := nil;
  Dec(vCgContextCount);
end;

// Apply
//
procedure TCgProgram.Apply(var rci : TRenderContextInfo);
begin
  if not Assigned(FHandle) then exit;
  if not FEnabled then exit;

  CurCgContent := FCgContext;

  cgGLBindProgram(FHandle);
  cgGLEnableProfile(FProfile);

  if Assigned(FOnApply) then FOnApply(Self);
end;

// UnApply
//
procedure TCgProgram.UnApply(var rci : TRenderContextInfo);
begin
  if not Assigned(FHandle) then exit;
  if not FEnabled then exit;

  if Assigned(FOnUnApply) then FOnUnApply(Self);

  cgGLDisableProfile(FProfile);
end;

function TCgProgram.GetProfileString: string;
begin
  result:=StrPas(cgGetProfileString(FProfile));
end;

procedure TCgProgram.ListParameters(Output: TStrings);
var i : integer;
begin
  Output.clear;
  for i:=0 to ParamCount-1 do
    output.add(TCgParameter(FParams[i]).Name);
end;

procedure TCgProgram.ListCompilation(Output: TStrings);

  procedure OutputAsTStrings(s : String);
  var i : integer;
  begin
    while Length(s) > 0 do begin
      I:=Pos(#10, s);
      if I = 0 then I:=255;
      Output.Add(Copy(s, 1, I-1));
      Delete(s, 1, I);
    end;
  end;

begin
  Output.BeginUpdate;
  Output.Clear;
  OutputAsTStrings(cgGetProgramString(FHandle, CG_COMPILED_PROGRAM));
  Output.EndUpdate;
end;

// ------------------
// ------------------ TCgParameter ------------------
// ------------------

// Create
//
constructor TCgParameter.Create;
begin
   inherited;
end;

// Destroy
//
destructor TCgParameter.Destroy;
begin
   inherited;
end;

// CheckValueType
//
procedure TCgParameter.CheckValueType(aType : TCGtype);
begin
  Assert(aType=FValueType, ClassName+': Parameter type mismatch.');
end;

// CheckValueType
//
procedure TCgParameter.CheckValueType(const types : TCGtypes);
  function DoCheck : Boolean;
  var
    i : Integer;
  begin
    Result := False;
    for i:=Low(types) to High(types) do
      if FValueType=types[i] then
      begin
        Result := True;
        Break;
      end;
  end;                     
begin
  Assert(DoCheck, ClassName+': Parameter type mismatch.');
end;

procedure TCgParameter.SetAsScalar(const val : Single);
// assuming a float
begin
  CheckValueType(CG_FLOAT);
  cgGLSetParameter1f(FHandle, val);
end;

// SetAsVector*
//
procedure TCgParameter.SetAsVector2f(const val: TVector2f);
begin
  CheckValueType(CG_FLOAT2);
  cgGLSetParameter2fv(FHandle, @val);
end;

procedure TCgParameter.SetAsVector3f(const val: TVector3f);
begin
  CheckValueType(CG_FLOAT3);
  cgGLSetParameter3fv(FHandle, @val);
end;

procedure TCgParameter.SetAsVector4f(const val: TVector4f);
begin
  CheckValueType(CG_FLOAT4);
  cgGLSetParameter4fv(FHandle, @val);
end;

procedure TCgParameter.SetAsVector(const val: TVector2f);
begin
  SetAsVector2f(val);
end;

procedure TCgParameter.SetAsVector(const val: TVector3f);
begin
  SetAsVector3f(val);
end;

procedure TCgParameter.SetAsVector(const val: TVector4f);
begin
  SetAsVector4f(val);
end;

// SetAsTexture*
//
procedure TCgParameter.SetAsTexture1D(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLER1D);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTexture2D(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLER2D);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTexture3D(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLER3D);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTextureRECT(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLERRECT);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

procedure TCgParameter.SetAsTextureCUBE(TextureID: Cardinal);
begin
  CheckValueType(CG_SAMPLERCUBE);
  cgGLSetTextureParameter(FHandle, TextureID);
end;

// DisableTexture
//
procedure TCgParameter.DisableTexture;
var
  ValueTypes: array[0..4] of TCGtype;
begin
  ValueTypes[0] := CG_SAMPLER2D; // most common first
  ValueTypes[1] := CG_SAMPLER1D;
  ValueTypes[2] := CG_SAMPLERRECT;
  ValueTypes[3] := CG_SAMPLERCUBE;
  ValueTypes[4] := CG_SAMPLER3D;
  CheckValueType(@ValueTypes);

  cgGLDisableTextureParameter(FHandle);
end;

// EnableTexture
//
procedure TCgParameter.EnableTexture;
var
  ValueTypes: array[0..4] of TCGtype;
begin
  ValueTypes[0] := CG_SAMPLER2D;
  ValueTypes[1] := CG_SAMPLER1D;
  ValueTypes[2] := CG_SAMPLERRECT;
  ValueTypes[3] := CG_SAMPLERCUBE;
  ValueTypes[4] := CG_SAMPLER3D;
  CheckValueType(@ValueTypes);

  cgGLEnableTextureParameter(FHandle);
end;

// SetAsStateMatrix
//
procedure TCgParameter.SetAsStateMatrix(matrix, Transform : Cardinal);
// Assuming matrix types are contiguous to simplify the type checking
const MinTypeConst = CG_FLOAT1x1;
      MaxTypeConst = CG_FLOAT4x4;
begin
  Assert( (FValueType>=MinTypeConst) and (FValueType<=MaxTypeConst),
          ClassName+': Parameter type mismatch.');
  cgGLSetStateMatrixParameter( Fhandle, matrix, Transform);
end;

// ------------------
// ------------------ TCgVertexProgram ------------------
// ------------------

// Create
//
constructor TCgVertexProgram.Create;
begin
  inherited;
  FProgramType := ptVertex;
  FVPProfile:=vpDetectLatest;
end;

procedure TCgVertexProgram.SetVPProfile(v: TCgVPProfile);
begin
  if FVPProfile=v then exit;
  FVPProfile:=v;
  case v of
    vp20   : FProfile := CG_PROFILE_VP20;
    vp30   : FProfile := CG_PROFILE_VP30;
    arbvp1 : FProfile := CG_PROFILE_ARBVP1;
  end;

  FDetectProfile:=v=vpDetectLatest;
end;

// ------------------
// ------------------ TCgFragmentProgram ------------------
// ------------------

// Create
//
constructor TCgFragmentProgram.Create;
begin
  inherited;
  FProgramType := ptFragment;
  FFPProfile:=fpDetectLatest;
end;

procedure TCgFragmentProgram.SetFPProfile(v: TCgFPProfile);
begin
  if FFPProfile=v then exit;
  FFPProfile:=v;
  case v of
    fp20   : FProfile := CG_PROFILE_FP20;
    fp30   : FProfile := CG_PROFILE_fP30;
    arbfp1 : FProfile := CG_PROFILE_ARBFP1;
  end;

  FDetectProfile:=v=fpDetectLatest;
end;

// ------------------
// ------------------ TCustomCgShader ------------------
// ------------------

// Create
//
constructor TCustomCgShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVertexProgram := TCgVertexProgram.Create(Self);
  FFragmentProgram := TCgFragmentProgram.Create(Self);
end;

// Destroy
//
destructor TCustomCgShader.Destroy;
begin
  inherited Destroy;
  FVertexProgram.Free;
  FFragmentProgram.Free;
end;

// SetVertexProgram
//
procedure TCustomCgShader.SetVertexProgram(const val : TCgVertexProgram);
begin
  FVertexProgram.Code := val.Code;
end;

// SetFragmentProgram
//
procedure TCustomCgShader.SetFragmentProgram(const val : TCgFragmentProgram);
begin
  FFragmentProgram.Code := val.Code;
end;

// SetOnApplyVertexProgram
//
procedure TCustomCgShader.SetOnApplyVertexProgram(const val : TCgApplyEvent);
begin
  FVertexProgram.OnApply := val;
end;

// GetOnApplyVertexProgram
//
function TCustomCgShader.GetOnApplyVertexProgram : TCgApplyEvent;
begin
  Result:=FVertexProgram.OnApply;
end;

// SetOnApplyFragmentProgram
//
procedure TCustomCgShader.SetOnApplyFragmentProgram(const val : TCgApplyEvent);
begin
  FFragmentProgram.OnApply:=val;
end;

// GetOnApplyFragmentProgram
//
function TCustomCgShader.GetOnApplyFragmentProgram : TCgApplyEvent;
begin
  Result:=FFragmentProgram.OnApply;
end;

// SetOnUnApplyVertexProgram
//
procedure TCustomCgShader.SetOnUnApplyVertexProgram(const val : TCgApplyEvent);
begin
  FVertexProgram.OnUnApply := val;
end;

// GetOnUnApplyVertexProgram
//
function TCustomCgShader.GetOnUnApplyVertexProgram : TCgApplyEvent;
begin
  Result:=FVertexProgram.OnUnApply;
end;

// SetOnUnApplyFragmentProgram
//
procedure TCustomCgShader.SetOnUnApplyFragmentProgram(const val : TCgApplyEvent);
begin
  FFragmentProgram.OnUnApply:=val;
end;

// GetOnUnApplyFragmentProgram
//
function TCustomCgShader.GetOnUnApplyFragmentProgram : TCgApplyEvent;
begin
  Result:=FFragmentProgram.OnUnApply;
end;

// GetOnInitialize
//
function TCustomCgShader.GetOnInitialize: TCgShaderEvent;
begin
  Result:=FOnInitialize;
end;

// SetOnInitialize
//
procedure TCustomCgShader.SetOnInitialize(const val: TCgShaderEvent);
begin
  FOnInitialize:=val;
end;

// DoInitialize
//
procedure TCustomCgShader.DoInitialize;
begin
  if (csDesigning in ComponentState) and (not FDesignEnable) then
    Exit;
  FVertexProgram.Initialize;
  FFragmentProgram.Initialize;

  if assigned(FOnInitialize) then FOnInitialize(self);
end;

// DoApply
//
procedure TCustomCgShader.DoApply(var rci : TRenderContextInfo);
begin
  if (csDesigning in ComponentState) and (not FDesignEnable) then
    Exit;
  FVertexProgram.Apply(rci);
  FFragmentProgram.Apply(rci);
end;

// DoUnApply
//
function TCustomCgShader.DoUnApply(var rci : TRenderContextInfo) : Boolean;
begin
  if (not (csDesigning in ComponentState)) or FDesignEnable then
  begin
    FVertexProgram.UnApply(rci);
    FFragmentProgram.UnApply(rci);
  end;
  Result := False;
end;

// DoFinalize
//
procedure TCustomCgShader.DoFinalize;
begin
  FVertexProgram.Finalize;
  FFragmentProgram.Finalize;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  // class registrations
  RegisterClass(TCustomCgShader);

  cgSetErrorCallBack(ErrorCallBack);
end.

