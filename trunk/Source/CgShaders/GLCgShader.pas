// GLCgShader
{: Base Cg shader classes.<p>

   <b>History :</b><font size=-1><ul>
      <li>24/06/03 - NelC - Initial adoptation to Cg 1.1 Final. Now automatically
                            uses latest hardware-supported profile. Use Callback
                            to show error message.
      <li>29/05/03 - RC - Cg 1.1 Depreciated_api compatible
      <li>25/09/02 - EG - Cg Beta 2/2.1 compatible, now uses ARBVP
      <li>19/06/02 - EG - Improved OO wrapper
      <li>18/06/02 - EG - Creation
   </ul></font>
}
unit GLCgShader;

interface

uses
  Classes, Geometry, GLTexture, GLMisc, Cg, CgGL;

type
  TCustomCgShader = class;
  TCgProgram = class;
  TCgParameter = class;

  TCgApplyEvent = procedure (Sender : TCgProgram) of object;

  TcgProgramType = (ptVertex, ptFragment);

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
    FOnProgramChanged : TNotifyEvent;

    FEnabled : boolean;

//    procedure Compile;

  protected
    { Protected Declarations }
    FProgramType : TcgProgramType;
    FProfile : TcgProfile;

    procedure SetCode(const val : TStrings);
    procedure SetProgramName(const val : String);
    function GetParam(index : String) : TCgParameter;

    procedure AddParamsItem(const Param : PCGParameter);

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
    function ParamCount : Integer;

    function GetProfileString : string;

    procedure LoadFromFile(const fileName : String);
    procedure Load(const CodeString : TStrings);

    procedure ShowCompilation(Output : TStrings);
    procedure ListParameter(Output : TStrings);

    property OnProgramChanged : TNotifyEvent read FOnProgramChanged write FOnProgramChanged;
  published
    { Published Declarations }
    property Code : TStrings read FCode write SetCode;
    property ProgramName : String read FProgramName write SetProgramName;
    property OnApply : TCgApplyEvent read FOnApply write FOnApply;

    property cgProfile : TcgProfile read FProfile write FProfile;
    property Enabled : boolean read FEnabled write FEnabled default True;
  end;

  // TCgParameter
  //
  {: Wrapper around a Cg parameter of the main program. }
  TCgParameter = class (TObject)
  private
    { Private Declarations }
    FOwner : TCgProgram;
    FName : String;
    FValueType   : TCGtype;
    FHandle      : PCGparameter;
    FDirection   : TCGenum;
    FVariability : TCGenum;
  protected
    { Protected Declarations }

// I'm not sure if we should use plain API or to set the parameters...

//    procedure CheckValueType(aType : TcgValueType); overload;
//    procedure CheckValueType(const types : TcgValueTypes); overload;
//    procedure BindAsVector(const val : TVector);
//
//    procedure Bind;
  public
    { Public Declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    property Owner : TCgProgram read FOwner;
    property Name : String read FName;
    property ValueType : TCGtype read FValueType;
    property Handle : PCGparameter read FHandle write FHandle;
    property Direction : TCGenum read FDirection write FDirection;
    property Variability : TCGenum read FVariability write FVariability;

//    procedure BindStateMatrix(matrixType : TCgGLMatrixType; format : Integer);
//    property AsVector : TVector write BindAsVector;
  end;

  // TCgVertexProgram
  //
  TCgVertexProgram = class (TCgProgram)
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
  end;

  // TCgFragmentProgram
  //
  TCgFragmentProgram = class (TCgProgram)
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
  end;

  // TCustomCgShader
  //
  TCustomCgShader = class (TGLShader)
  private
    { Private Declarations }
    FVertexProgram : TCgVertexProgram;
    FFragmentProgram : TCgFragmentProgram;
    FDesignEnable : Boolean;
  protected
    { Protected Declarations }
    procedure SetVertexProgram(const val : TCgVertexProgram);
    procedure SetFragmentProgram(const val : TCgFragmentProgram);
    procedure SetOnApplyVertexProgram(const val : TCgApplyEvent);
    function GetOnApplyVertexProgram : TCgApplyEvent;
    procedure SetOnApplyFragmentProgram(const val : TCgApplyEvent);
    function GetOnApplyFragmentProgram : TCgApplyEvent;

    procedure DoInitialize; override;
    procedure DoApply(var rci : TRenderContextInfo); override;
    function  DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
    procedure DoFinalize; override;

    property VertexProgram : TCgVertexProgram read FVertexProgram write SetVertexProgram;
    property FragmentProgram : TCgFragmentProgram read FFragmentProgram write SetFragmentProgram;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property OnApplyVertexProgram : TCgApplyEvent read GetOnApplyVertexProgram write SetOnApplyVertexProgram;
    property OnApplyFragmentProgram : TCgApplyEvent read GetOnApplyFragmentProgram write SetOnApplyFragmentProgram;

  published
    { Published Declarations }
    property DesignEnable : Boolean read FDesignEnable write FDesignEnable default False;
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
  end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL12, Dialogs;

var
//  vCgContextCount : Integer;
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

// GetCgContext
//
{function _GetCgContext : PCgContext;
begin
   Inc(vCgContextUseCount);
   if vCgContextUseCount=1 then begin
      vCgContext:=cgCreateContext;
      Assert(Assigned(vCgContext), 'Unable to create CgContext.');
   end;
   Result:=vCgContext;
end;

// ReleaseCgContext
//
procedure _ReleaseCgContext(aContext : PCgContext);
begin
   Assert(aContext=vCgContext, 'Invalid CgContext.');
   Assert(vCgContextUseCount>0, 'Unbalanced CgContext allocations.');
   Dec(vCgContextUseCount);
   if vCgContextUseCount=0 then begin
      cgFreeContext(vCgContext);
      vCgContext:=nil;
      cgCleanup;
   end;
end;
}

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


procedure TCgProgram.Load(const CodeString: TStrings);
begin
  Finalize;
  Code.Assign(CodeString);
  // Initialize will be called later when shader is first applied
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
    FValueType := cgGetParameterType(Param); // e.g. CG_FLOAT
    FDirection := cgGetParameterDirection(Param); // e.g. CG_INOUT
    FVariability := cgGetParameterVariability(Param); // e.g. CG_UNIFORM
  end;
  FParams.Add(newParamObj);
end;

// BuildParamsList
//
procedure TCgProgram.BuildParamsList;
// Iteratively queries all parameters defined in a Cg program so that we can
// manage and access them easily.
// Currently only collects leaf parameters i.e. data structure is not retrieved.
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

  if Result=nil then begin // can't get it from our list?
    AddParamsItem(cgGetNamedParameter(FHandle, PChar(name))); // get it from Cg API
    // insert error-check here
    Result:=TCgParameter(list[FParams.Count-1]);
  end;
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
//  ret : TcgError;
//  PProgName : PChar;
begin
  Assert(FCgContext=nil);

  buf := Trim(Code.Text);
  if buf<>'' then
  begin
    // get a new context
    FCgContext := cgCreateContext;
    CurCgContent := FCgContext;
//    Inc(vCgContextCount);
    try
      // automatically set the profile to the latest supported by the hardware
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
//      Dec(vCgContextCount);
    raise;
    end;
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
//  Dec(vCgContextCount);
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

  cgGLDisableProfile(FProfile);
end;

// Compile
//
{procedure TCgProgram.Compile;
begin
  cgCompileProgram(FHandle);
end; }

function TCgProgram.GetProfileString: string;
begin
  result:=StrPas(cgGetProfileString(FProfile));
end;

procedure TCgProgram.ListParameter(Output: TStrings);
var i : integer;
begin
  Output.clear;
  for i:=0 to ParamCount-1 do
    output.add(TCgParameter(FParams[i]).Name);
end;

procedure TCgProgram.ShowCompilation(Output: TStrings);
begin
  Output.Clear;
  Output.add(cgGetProgramString(FHandle, CG_COMPILED_PROGRAM));
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
//procedure TCgParameter.CheckValueType(aType : TcgValueType);
//begin
//  Assert(aType=FValueType, ClassName+': Parameter type mismatch.');
//end;

// CheckValueType
//
//procedure TCgParameter.CheckValueType(const types : TcgValueTypes);
//  function DoCheck : Boolean;
//  var
//    i : Integer;
//  begin
//    Result := False;
//    for i:=Low(types) to High(types) do
//      if FValueType=types[i] then
//      begin
//        Result := True;
//        Break;
//      end;
//  end;
//begin
//  Assert(DoCheck, ClassName+': Parameter type mismatch.');
//end;

// Bind
//
//procedure TCgParameter.Bind;
//begin
//  if not Assigned(FBindIter) then
//  begin
//    FBindIter := cgGetBindByName(Owner.ProgramIter, PChar(Name));
//    Assert(Assigned(FBindIter), ClassName+': Failed to bind "'+Name+'"');
//  end;
//end;

// BindAsVector
//
//procedure TCgParameter.BindAsVector(const val : TVector);
//begin
//  CheckValueType(cgFloat4ValueType);
//  Bind;
//  cgGLBindUniform4fv(Owner.ProgramIter, BindIter, @val);
//end;

// BindStateMatrix
//
//procedure TCgParameter.BindStateMatrix(matrixType : TCgGLMatrixType; format : Integer);
//var
//  ValueTypes: array[0..3] of TcgValueType;
//begin
//  ValueTypes[0] := cgFloat4x4ValueType;
//  ValueTypes[1] := cgFloat4x3ValueType;
//  ValueTypes[2] := cgFloat3x4ValueType;
//  ValueTypes[3] := cgFloat3x3ValueType;
//  CheckValueType(@ValueTypes);
//  Bind;
//  cgGLBindUniformStateMatrix(Owner.ProgramIter, BindIter, matrixType, format);
//end;

// ------------------
// ------------------ TCgVertexProgram ------------------
// ------------------

// Create
//
constructor TCgVertexProgram.Create;
begin
  FProgramType := ptVertex;
  inherited;
end;

// ------------------
// ------------------ TCgFragmentProgram ------------------
// ------------------

// Create
//
constructor TCgFragmentProgram.Create;
begin
  FProgramType := ptFragment;
  inherited;
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

// DoInitialize
//
procedure TCustomCgShader.DoInitialize;
begin
  if (csDesigning in ComponentState) and (not FDesignEnable) then
    Exit;
  FVertexProgram.Initialize;
  FFragmentProgram.Initialize;
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

