// GLCgShader
{: Base Cg shader classes.<p>

   <b>History :</b><font size=-1><ul>
      <li>25/09/02 - EG - Cg Beta 2/2.1 compatible, now uses ARBVP
      <li>19/06/02 - EG - Improved OO wrapper
      <li>18/06/02 - EG - Creation
   </ul></font>
}
unit GLCgShader;

interface

uses Classes, Geometry, GLTexture, GLMisc, Cg, CgGL;

type

   TCustomCgShader = class;
   TCgProgram = class;
   TCgParameter = class;
   
   TcgValueTypes = array of TCGtype;

   TCgApplyEvent = procedure (Sender : TCgProgram; programIterator : PCGprogram) of object;

	// TCgProgram
	//
   {: Wrapper around a Cg program. }
	TCgProgram = class (TGLUpdateAbleObject)
	   private
	      { Private Declarations }
         FCgContext : PcgContext;
         FCode : TStrings;
         FProgramIter : PCGprogram;
         FProgramID : Integer;
         FOnProgramChanged : TNotifyEvent;
         FProgramName : String;
         FParams : TList;
         FOnApply : TCgApplyEvent;

	   protected
	      { Protected Declarations }
         FProfileType : TCGprofile;

         procedure SetCode(const val : TStrings);
         procedure SetProgramName(const val : String);
         function GetParam(index : String) : TCgParameter;

         procedure BuildParamsList;
         procedure ClearParams;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TPersistent); override;
         destructor Destroy; override;

         procedure Initialize; dynamic;
         procedure Finalize;
         procedure Apply(var rci : TRenderContextInfo);
         procedure UnApply(var rci : TRenderContextInfo);

         property Param[index : String] : TCgParameter read GetParam;
         function ParamByName(const name : String) : TCgParameter;
         function ParamCount : Integer;

         procedure LoadFromFile(const fileName : String);

         property ProgramIter : PCGprogram read FProgramIter;
         property ProgramID : Integer read FProgramID;
         property OnProgramChanged : TNotifyEvent read FOnProgramChanged write FOnProgramChanged;

	   published
	      { Published Declarations }
         property Code : TStrings read FCode write SetCode;
         property ProgramName : String read FProgramName write SetProgramName;
         property OnApply : TCgApplyEvent read FOnApply write FOnApply;
   end;

   // TCgParameter
   //
   {: Wrapper around a Cg parameter of the main program. }
   TCgParameter = class (TObject)
	   private
	      { Private Declarations }
         FOwner : TCgProgram;
         FName : String;
         FBindIter : PCGparameter;
         FParamType : TCGenum;
         FValueType : TCGtype;

	   protected
	      { Protected Declarations }
         procedure CheckValueType(aType : TCGtype); overload;
         procedure CheckValueType(const types : TcgValueTypes); overload;
         procedure BindAsVector(const val : TVector);

         procedure Bind;

	   public
	      { Public Declarations }
	      constructor Create; virtual;
         destructor Destroy; override;

         property Owner : TCgProgram read FOwner;
         property Name : String read FName;
         property ParamType : TCGenum read FParamType;
         property ValueType : TCGtype read FValueType;
         property BindIter : PCGparameter read FBindIter;

         procedure BindStateMatrix(matrixType : Integer; format : Integer);
         property AsVector : TVector write BindAsVector;
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
   vCgNextProgID : Integer = 1;
   vCgContextCount : Integer;

procedure Register;
begin
   RegisterComponents('GLScene-Cg', [TCgShader]);
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
// GetCgNextProgID
//
function GetCgNextProgID : Integer;
begin
   Result:=vCgNextProgID;
   Inc(vCgNextProgID);
end;

// ------------------
// ------------------ TCgProgram ------------------
// ------------------

// Create
//
constructor TCgProgram.Create(AOwner: TPersistent);
begin
	inherited;
   FCode:=TStringList.Create;
   TStringList(FCode).OnChange:=NotifyChange;
   FParams:=TList.Create;
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
   if val<>FProgramName then begin
      FProgramName:=val;
      NotifyChange(Self);
   end;
end;

// BuildParamsList
//
procedure TCgProgram.BuildParamsList;
var
   bindIter : PCGparameter;
   paramName, paramStart : String;
   newParam : TCgParameter;
begin
   ClearParams;
   paramStart:=FProgramName+'.';
   // build params list
   bindIter:=cgGetFirstParameter(FProgramIter, CG_IN);
   while Assigned(bindIter) do begin
      paramName:=StrPas(cgGetParameterName(bindIter));
      newParam:=TCgParameter.Create;
      newParam.FOwner:=Self;
      newParam.FName:=paramName;
      newParam.FParamType:=cgGetParameterDirection(bindIter);
      newParam.FValueType:=cgGetParameterType(bindIter);
      newParam.FBindIter:=nil; // late bind
      FParams.Add(newParam);
      bindIter:=cgGetNextParameter(bindIter);
   end;
end;

// ClearParams
//
procedure TCgProgram.ClearParams;
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
   Result:=ParamByName(index);
   Assert(Result<>nil, ClassName+': Invalid parameter name "'+index+'"');
end;

// ParamByName
//
function TCgProgram.ParamByName(const name : String) : TCgParameter;
var
   i : Integer;
   list : PPointerList;
   bindIter : PCGparameter;
begin
   Result:=nil;
   list:=FParams.List;
   for i:=0 to FParams.Count-1 do begin
      if TCgParameter(list[i]).Name=name then begin
         Result:=TCgParameter(list[i]);
         Exit;
      end;
   end;
   // attempt a bind (Cg beta bug workaround)
   bindIter:=cgGetNamedParameter(FProgramIter, PChar(FProgramName+'.'+name));
   if Assigned(bindIter) then begin
      Result:=TCgParameter.Create;
      FParams.Add(Result);
      Result.FBindIter:=bindIter;
      Result.FName:=name;
      Result.FParamType:=cgGetParameterDirection(bindIter);
      Result.FValueType:=cgGetParameterType(bindIter);
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
   ret : TcgError;
begin
   Assert(FCgContext=nil);

   buf:=Trim(Code.Text);
   if buf<>'' then begin
      // get a new context
      FCgContext:=cgCreateContext;
      Assert(Assigned(FCgContext), ClassName+': Unable to create CgContext.');
      Inc(vCgContextCount);
      try
         // add the program to it
         FProgramIter:=cgCreateProgram(FCgContext, CG_PROGRAM, PChar(buf),
                              FProfileType, nil, nil);
         ret:=cgGetError;
         Assert((ret=CG_NO_ERROR), ClassName+': Failed to add Program.'+#13#10
                                  +cgGetErrorString(ret));
         FProgramName:=StrPas(cgGetProgramString(FProgramIter, CG_PROGRAM));
         // load program
         glGetError;
         FProgramID:=GetCgNextProgID;
         cgCompileProgram(FProgramIter);
         ret:=cgGetError;
         Assert((ret=CG_NO_ERROR), ClassName+': Failed to compile program.'+#13#10
                                  +cgGetErrorString(ret));
         cgGLLoadProgram(FProgramIter);
         ret:=cgGetError;
         Assert((ret=CG_NO_ERROR), ClassName+': Failed to load program.'+#13#10
                                  +cgGetErrorString(ret));
         // build parameter list for the selected program
         BuildParamsList;
      except
         cgDestroyContext(FCgContext);
         FCgContext:=nil;
         Dec(vCgContextCount);
         raise;
      end;
   end;
end;

// Finalize
//
procedure TCgProgram.Finalize;
begin
   if Assigned(FCgContext) then begin
      ClearParams;
      cgDestroyProgram(FProgramIter);
      FProgramIter:=nil;
      FProgramID:=0;
      FProgramName:='';
      ClearParams;
      cgDestroyContext(FCgContext);
      FCgContext:=nil;
      Dec(vCgContextCount);
   end;
end;

// Apply
//
procedure TCgProgram.Apply(var rci : TRenderContextInfo);
var
   ret : TcgError;
begin
   if Assigned(FProgramIter) then begin
      cgGLBindProgram(FProgramIter);
      ret:=cgGetError;
      Assert(ret=CG_NO_ERROR);
      if Assigned(FOnApply) then
         FOnApply(Self, FProgramIter);
      cgGLEnableProfile(FProfileType);
   end;
end;

// UnApply
//
procedure TCgProgram.UnApply(var rci : TRenderContextInfo);
begin
   if Assigned(FProgramIter) then
      cgGLDisableProfile(FProfileType);
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
procedure TCgParameter.CheckValueType(const types : TcgValueTypes);

   function DoCheck : Boolean;
   var
      i : Integer;
   begin
      Result:=False;
      for i:=Low(types) to High(types) do
         if FValueType=types[i] then begin
            Result:=True;
            Break;
         end;
   end;

begin
   Assert(DoCheck, ClassName+': Parameter type mismatch.');
end;

// Bind
//
procedure TCgParameter.Bind;
begin
   if not Assigned(FBindIter) then begin
      FBindIter:=cgGetBindByName(Owner.ProgramIter, PChar(Name));
      Assert(Assigned(FBindIter), ClassName+': Failed to bind "'+Name+'"');
   end;
end;

// BindAsVector
//
procedure TCgParameter.BindAsVector(const val : TVector);
begin
   CheckValueType(cgFloat4ValueType);
   Bind;
   cgGLBindUniform4fv(Owner.ProgramIter, BindIter, @val);
end;

// BindStateMatrix
//
procedure TCgParameter.BindStateMatrix(matrixType : TCgGLMatrixType; format : Integer);
begin
   CheckValueType([cgFloat4x4ValueType,
                   cgFloat4x3ValueType, cgFloat3x4ValueType,
                   cgFloat3x3ValueType]);
   Bind;
   cgGLBindUniformStateMatrix(Owner.ProgramIter, BindIter, matrixType, format);
end;

// ------------------
// ------------------ TCgVertexProgram ------------------
// ------------------

// Create
//
constructor TCgVertexProgram.Create;
begin
   FProfileType:=cgARBVertexProfile;
	inherited;
end;

// ------------------
// ------------------ TCgFragmentProgram ------------------
// ------------------

// Create
//
constructor TCgFragmentProgram.Create;
begin
   FProfileType:=cgFragmentProfile;
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
   FVertexProgram:=TCgVertexProgram.Create(Self);
   FFragmentProgram:=TCgFragmentProgram.Create(Self);
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
   FVertexProgram.Code:=val.Code;
end;

// SetFragmentProgram
//
procedure TCustomCgShader.SetFragmentProgram(const val : TCgFragmentProgram);
begin
   FFragmentProgram.Code:=val.Code;
end;

// SetOnApplyVertexProgram
//
procedure TCustomCgShader.SetOnApplyVertexProgram(const val : TCgApplyEvent);
begin
   FVertexProgram.OnApply:=val;
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
   if (csDesigning in ComponentState) and (not FDesignEnable) then Exit;

   FVertexProgram.Initialize;
   FFragmentProgram.Initialize;
end;

// DoApply
//
procedure TCustomCgShader.DoApply(var rci : TRenderContextInfo);
begin
   if (csDesigning in ComponentState) and (not FDesignEnable) then Exit;

   FVertexProgram.Apply(rci);
   FFragmentProgram.Apply(rci);
end;

// DoUnApply
//
function TCustomCgShader.DoUnApply(var rci : TRenderContextInfo) : Boolean;
begin
   if (not (csDesigning in ComponentState)) or FDesignEnable then begin
      FVertexProgram.UnApply(rci);
      FFragmentProgram.UnApply(rci);
   end;
   Result:=False;
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

end.

