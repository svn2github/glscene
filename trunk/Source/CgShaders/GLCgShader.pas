// GLCgShader
{: Base Cg shader classes.<p>

   <b>History :</b><font size=-1><ul>
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
   
   TcgValueTypes = set of TcgValueType;

   TCgApplyEvent = procedure (Sender : TCgProgram; programIterator : PcgProgramIter) of object;

	// TCgProgram
	//
   {: Wrapper around a Cg program. }
	TCgProgram = class (TGLUpdateAbleObject)
	   private
	      { Private Declarations }
         FCgContext : PcgContext;
         FCode : TStrings;
         FProgramIter : PcgProgramIter;
         FProgramID : Integer;
         FOnProgramChanged : TNotifyEvent;
         FProgramName : String;
         FParams : TList;
         FOnApply : TCgApplyEvent;

	   protected
	      { Protected Declarations }
         FProfileType : TcgProfileType;

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

         property ProgramIter : PcgProgramIter read FProgramIter;
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
         FBindIter : PcgBindIter;
         FParamType : TcgParamType;
         FValueType : TcgValueType;

	   protected
	      { Protected Declarations }
         procedure CheckValueType(aType : TcgValueType); overload;
         procedure CheckValueType(types : TcgValueTypes); overload;
         procedure BindAsVector(const val : TVector);

         procedure Bind;

	   public
	      { Public Declarations }
	      constructor Create; virtual;
         destructor Destroy; override;

         property Owner : TCgProgram read FOwner;
         property Name : String read FName;
         property ParamType : TcgParamType read FParamType;
         property ValueType : TcgValueType read FValueType;
         property BindIter : PcgBindIter read FBindIter;

         procedure BindStateMatrix(matrixType : TCgGLMatrixType; format : Integer);
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
         procedure DoUnApply(var rci : TRenderContextInfo); override;
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

{$R *.dcr}

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
   bindIter, oldIter : PcgBindIter;
   paramName, paramStart : String;
   lenStart, arraySize : Integer;
   newParam : TCgParameter;
begin
   ClearParams;
   paramStart:=FProgramName+'.';
   lenStart:=Length(paramStart);
   // build params list
   oldIter:=nil;
   bindIter:=cgGetNextBind(FProgramIter, oldIter);
   while Assigned(bindIter) do begin
      paramName:=StrPas(cgGetBindParamName(bindIter));
      if Copy(paramName, 1, lenStart)=paramStart then begin
         newParam:=TCgParameter.Create;
         newParam.FOwner:=Self;
         newParam.FName:=Copy(paramName, lenStart+1, MaxInt);
         newParam.FParamType:=cgGetBindParamType(bindIter);
         newParam.FValueType:=cgGetBindValueType(bindIter, arraySize);
         newParam.FBindIter:=nil; // late bind
         FParams.Add(newParam);
      end;
      oldIter:=bindIter;
      bindIter:=cgGetNextBind(FProgramIter, bindIter);
   end;
   cgFreeBindIter(oldIter);
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
   Assert(Result<>nil, ClassName+': Invalid parameter name.');
end;

// ParamByName
//
function TCgProgram.ParamByName(const name : String) : TCgParameter;
var
   i : Integer;
   list : PPointerList;
begin
   Result:=nil;
   list:=FParams.List;
   for i:=0 to FParams.Count-1 do begin
      if TCgParameter(list[i]).Name=name then begin
         Result:=TCgParameter(list[i]);
         Break;
      end;
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
   iter : PcgProgramIter;
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
         ret:=cgAddProgram(FCgContext, PChar(buf), FProfileType, nil);
         Assert((ret=cgNoError),
                ClassName+': Failed to add Program.');
         // use last prog name as default (previous ones are probably funcs)
         iter:=nil;
         FProgramName:='';
         repeat
            if Assigned(iter) then
               FProgramName:=StrPas(cgGetProgramName(iter));
            iter:=cgGetNextProgram(FCgContext, iter);
         until iter=nil;
         Assert((FProgramName<>''),
                ClassName+': Unable to retrieve Program iterator.');
         FProgramIter:=cgProgramByName(FCgContext, PChar(FProgramName));
         // build parameter list for the selected program
         BuildParamsList;
         // load program
         glGetError;
         FProgramID:=GetCgNextProgID;
         ret:=cgGLLoadProgram(FProgramIter, FProgramID);
         Assert((ret=cgNoError), ClassName+': Failed to load VertexProgram.');
      except
         cgFreeContext(FCgContext);
         FCgContext:=nil;
         Dec(vCgContextCount);
         if vCgContextCount=0 then
            cgCleanup;
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
      cgFreeProgramIter(FProgramIter);
      FProgramIter:=nil;
      FProgramID:=0;
      FProgramName:='';
      ClearParams;
      cgFreeContext(FCgContext);
      FCgContext:=nil;
      Dec(vCgContextCount);
      if vCgContextCount=0 then
         cgCleanup;
   end;
end;

// Apply
//
procedure TCgProgram.Apply(var rci : TRenderContextInfo);
var
   ret : TcgError;
begin
   if Assigned(FProgramIter) then begin
      ret:=cgGLBindProgram(FProgramIter);
      Assert(ret=cgNoError);
      if Assigned(FOnApply) then
         FOnApply(Self, FProgramIter);
      cgGLEnableProgramType(FProfileType);
   end;
end;

// UnApply
//
procedure TCgProgram.UnApply(var rci : TRenderContextInfo);
begin
   if Assigned(FProgramIter) then
      cgGLDisableProgramType(FProfileType);
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
   if Assigned(FBindIter) then
      cgFreeBindIter(FBindIter);
   inherited;
end;

// CheckValueType
//
procedure TCgParameter.CheckValueType(aType : TcgValueType);
begin
   Assert(aType=FValueType, ClassName+': Parameter type mismatch.');
end;

// CheckValueType
//
procedure TCgParameter.CheckValueType(types : TcgValueTypes);
begin
   Assert(FValueType in types, ClassName+': Parameter type mismatch.');
end;

// Bind
//
procedure TCgParameter.Bind;
begin
   if not Assigned(FBindIter) then begin
      FBindIter:=cgGetBindByName(Owner.ProgramIter, PChar(Owner.ProgramName+'.'+Name));
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
   FProfileType:=cgVertexProfile;
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
procedure TCustomCgShader.DoUnApply(var rci : TRenderContextInfo);
begin
   if (csDesigning in ComponentState) and (not FDesignEnable) then Exit;

   FVertexProgram.UnApply(rci);
   FFragmentProgram.UnApply(rci);
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

