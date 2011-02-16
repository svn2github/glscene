//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSCUDA<p>

   <b>History : </b><font size=-1><ul>
      <li>07/04/10 - Yar - Added linear copying in TCUDAMemData.CopyTo
      <li>01/04/10 - Yar - Creation
   </ul></font><p>
}
unit GLSCUDA;

interface

{$I cuda.inc}

uses
  Classes,
  SysUtils,
  // GLScene
  PersistentClasses,
  BaseClasses,
  GLCrossPlatform,
  GLContext,
  VectorGeometry,
  VectorTypes,
  VectorLists,
  GLGraphics,
  GLS_CL_Platform,
  GLS_CUDA_API,
  GLSCUDAParser,
  GLS_CUDA_FourierTransform,
  GLSCUDACompiler,
  GLSCUDAContext;

type

  TCUDAMemData = class;
  TCUDAFunction = class;
  TCUDATexture = class;
  TGLSCUDA = class;

  TCUDAChange =
  (
    cuchDevice,
    cuchContext,
    cuchSize,
    cuchAddresMode,
    cuchFlag,
    cuchFilterMode,
    cuchArray,
    cuchFormat,
    cuchMapping
  );
  TCUDAChanges = set of TCUDAChange;

  TCuAddresMode = (amWrap, amClamp, amMirror);
  TCuFilterMode = (fmPoint, fmLinear);

  TCUDAChannelType = (
    ctUndefined,
    ctUInt8,
    ctUInt16,
    ctUInt32,
    ctInt8,
    ctInt16,
    ctInt32,
    ctHalfFloat,
    ctFloat);

  TCUDAChannelNum = (cnOne, cnTwo, cnTree, cnFour);

  TCUDAMapping = (grmDefault, grmReadOnly, grmWriteDiscard);

  TCUDAComponent = class(TCUDAHandlesMaster)
  private
    FMaster: TCUDAComponent;
    FItems: TPersistentObjectList;
    procedure SetMaster(AMaster: TCUDAComponent);
    function GetItem(const i: Integer): TCUDAComponent;
    function GetItemsCount: Integer;
  protected
    { Protected declarations }
    FStatus: TCUresult;
    FChanges: TCUDAchanges;
    function GetContext: TCUDAContext; override;
    procedure CollectStatus(AStatus: TCUresult);
    procedure GetChildren(AProc: TGetChildProc; Root: TComponent); override;
    procedure AddItem(AItem: TCUDAComponent);
    procedure RemoveItem(AItem: TCUDAComponent);
    procedure DeleteItems;
    procedure SetName(const NewName: TComponentName); override;
  public
    { Public declarations }
    destructor Destroy; override;
    procedure CuNotifyChange(AChange: TCUDAchange); virtual;

    function GetParentComponent: TComponent; override;
    procedure SetParentComponent(Value: TComponent); override;
    function HasParent: Boolean; override;
    function GetItemByName(const name: string): TCUDAComponent;
    function MakeUniqueName(const BaseName: string): string;

    property Master: TCUDAComponent read FMaster write SetMaster;
    property Context: TCUDAContext read GetContext;
    property Items[const i: Integer]: TCUDAComponent read GetItem;
    property ItemsCount: Integer read GetItemsCount;
    property Status: TCUresult read FStatus;
  end;

  TCUDAComponentClass = class of TCUDAComponent;

  TCUDAModule = class(TCUDAComponent)
  private
    { Private declarations }
    FHandle: PCUmodule;
    FCode: TStringList;
    FCodeType: TGLSCUDACompilerOutput;
    FCompiler: TGLSCUDACompiler;
    FInfo: TCUDAModuleInfo;
    FMakeRevision: Boolean;
    procedure SetCode(const Value: TStringList);
    procedure SetCompiler(const Value: TGLSCUDACompiler);
    function GetKernelFunction(const name: string): TCUDAFunction;
    function GetKernelTexture(const name: string): TCUDATexture;
  protected
    { Protected declarations }
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    procedure OnChangeCode(Sender: TObject);
    procedure Loaded; override;
    function GetContext: TCUDAContext; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromSource;
    procedure Unload;
    {: Work only in design time }
    function GetDesignerTaskList: TList;
    function GetKernelFunctionCount: Integer;
    function GetKernelTextureCount: Integer;

    property Context: TCUDAContext read GetContext;
    property CodeType: TGLSCUDACompilerOutput read fCodeType;
    property KernelFunction[const name: string]: TCUDAFunction read
    GetKernelFunction;
    property KernelTexture[const name: string]: TCUDATexture read
    GetKernelTexture;
  published
    { Published declarations }
    property Code: TStringList read fCode write SetCode;
    property Compiler: TGLSCUDACompiler read fCompiler write SetCompiler;
  end;

  TGLResourceType = (rtTexture, rtBuffer);

  {: Abstract class of graphic resources. }

  // TCUDAGraphicResource
  //

  TCUDAGraphicResource = class(TCUDAComponent)
  protected
    { Protected declaration }
    fHandle: array[0..7] of PCUgraphicsResource;
    fMapping: TCUDAMapping;
    fResourceType: TGLResourceType;
    FGLContextHandle: TGLVirtualHandle;
    FMapCounter: Integer;
    procedure OnGLHandleAllocate(Sender: TGLVirtualHandle;
      var Handle: Cardinal);
    procedure OnGLHandleDestroy(Sender: TGLVirtualHandle;
      var Handle: Cardinal);
    procedure BindArrayToTexture(var cudaArray: TCUDAMemData;
      ALeyer, ALevel: LongWord); virtual; abstract;
    procedure SetArray(var AArray: TCUDAMemData;
      AHandle: PCUarray; ForGLTexture, Volume: Boolean);
    function GetAttributeArraySize(Attr: Integer): LongWord; virtual; abstract;
    function GetAttributeArrayAddress(Attr: Integer): Pointer; virtual; abstract;
    function GetElementArrayDataSize: LongWord; virtual; abstract;
    function GetElementArrayAddress: Pointer; virtual; abstract;
    procedure SetMapping(const Value: TCUDAMapping); virtual;
    property Mapping: TCUDAMapping read fMapping write SetMapping
      default grmDefault;
  public
    procedure MapResources; virtual; abstract;
    procedure UnMapResources; virtual; abstract;
  end;

  TCUDAMemType = (mtHost, mtDevice, mtArray);

  TCUDAMemData = class(TCUDAComponent)
  private
    { Private declarations }
    fData: TCUdeviceptr;
    fHandle: PCUarray;
    fWidth: Integer;
    fHeight: Integer;
    fDepth: Integer;
    fPitch: Cardinal;
    fChannelsType: TCUDAChannelType;
    fChannelsNum: TCUDAChannelNum;
    fMemoryType: TCUDAMemType;
    fTexture: TCUDATexture;
    FOpenGLRefArray: Boolean;
    procedure SetMemoryType(const AType: TCUDAMemType);
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetDepth(const Value: Integer);
    procedure SetChannelType(const Value: TCUDAChannelType);
    procedure SetChannelNum(const Value: TCUDAChannelNum);
    function GetData: TCUdeviceptr;
    function GetArrayHandle: PCUarray;
  protected
    { Protected declaration }
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    procedure CheckAccess(
      x, y, z: Integer; cType: TCUDAChannelType;
      cNum: TCUDAChannelNum);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CuNotifyChange(AChange: TCUDAchange); override;
    procedure GetElement(out Value: Byte; const x: Integer; y: Integer = 0; z:
      Integer = 0);
      overload;
    procedure GetElement(out Value: TVector2b; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;
    procedure GetElement(out Value: TVector3b; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;
    procedure GetElement(out Value: TVector4b; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;
    procedure GetElement(out Value: Word; const x: Integer; y: Integer = 0; z:
      Integer = 0);
      overload;
    procedure GetElement(out Value: TVector2w; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;
    procedure GetElement(out Value: TVector3w; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;
    procedure GetElement(out Value: TVector4w; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;
    procedure GetElement(out Value: longint; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;
    procedure GetElement(out Value: TVector2i; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;
    procedure GetElement(out Value: TVector3i; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;
    procedure GetElement(out Value: TVector4i; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;
    procedure GetElement(out Value: Single; const x: Integer; y: Integer = 0; z:
      Integer = 0);
      overload;
    procedure GetElement(out Value: TVector2f; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;
    procedure GetElement(out Value: TVector3f; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;
    procedure GetElement(out Value: TVector4f; const x: Integer; y: Integer = 0;
      z: Integer = 0);
      overload;

    procedure SetElement(const Value: Byte; const x: Integer; y: Integer = 0; z:
      Integer = 0); overload;
    procedure SetElement(const Value: TVector2b; const x: Integer; y: Integer =
      0; z: Integer = 0); overload;
    procedure SetElement(const Value: TVector3b; const x: Integer; y: Integer =
      0; z: Integer = 0); overload;
    procedure SetElement(const Value: TVector4b; const x: Integer; y: Integer =
      0; z: Integer = 0); overload;
    procedure SetElement(const Value: Word; const x: Integer; y: Integer = 0; z:
      Integer = 0); overload;
    procedure SetElement(const Value: TVector2w; const x: Integer; y: Integer =
      0; z: Integer = 0); overload;
    procedure SetElement(const Value: TVector3w; const x: Integer; y: Integer =
      0; z: Integer = 0); overload;
    procedure SetElement(const Value: TVector4w; const x: Integer; y: Integer =
      0; z: Integer = 0); overload;
    procedure SetElement(const Value: longint; const x: Integer; y: Integer = 0;
      z: Integer = 0); overload;
    procedure SetElement(const Value: TVector2i; const x: Integer; y: Integer =
      0; z: Integer = 0); overload;
    procedure SetElement(const Value: TVector3i; const x: Integer; y: Integer =
      0; z: Integer = 0); overload;
    procedure SetElement(const Value: TVector4i; const x: Integer; y: Integer =
      0; z: Integer = 0); overload;
    procedure SetElement(const Value: Single; const x: Integer; y: Integer = 0;
      z: Integer = 0); overload;
    procedure SetElement(const Value: TVector2f; const x: Integer; y: Integer =
      0; z: Integer = 0); overload;
    procedure SetElement(const Value: TVector3f; const x: Integer; y: Integer =
      0; z: Integer = 0); overload;
    procedure SetElement(const Value: TVector4f; const x: Integer; y: Integer =
      0; z: Integer = 0); overload;
    {: Fill device data }
    procedure FillMem(const Value);

    function ElementSize: Cardinal;
    function DataSize: Cardinal;

    procedure CopyTo(const dstMemData: TCUDAMemData); overload;
    procedure CopyTo(const GLImage: TGLBitmap32); overload;
    {: Copy data to Graphic resource.
       For geometry resource Param is attribute (0..GLS_VERTEX_ATTR_NUM-1)
       or index of vertex (-1) }
    procedure CopyTo(const GLGraphic: TCUDAGraphicResource; Param: Integer = 0);
      overload;
    procedure CopyFrom(const srcMemData: TCUDAMemData); overload;
    procedure CopyFrom(const GLImage: TGLBitmap32); overload;
    procedure CopyFrom(const GLGraphic: TCUDAGraphicResource; Param: Integer =
      0);
      overload;

    property Pitch: Cardinal read fPitch;
    property Data: TCUdeviceptr read GetData;
    property ArrayHandle: PCUarray read GetArrayHandle;
  published
    { Published declarations }
    property Width: Integer read fWidth write SetWidth default 256;
    property Height: Integer read fHeight write SetHeight default 0;
    property Depth: Integer read fDepth write SetDepth default 0;
    property MemoryType: TCUDAMemType read fMemoryType write SetMemoryType
      default mtHost;
    property ChannelsType: TCUDAChannelType read fChannelsType write
      SetChannelType default ctInt8;
    property ChannelsNum: TCUDAChannelNum read fChannelsNum write SetChannelNum
      default cnOne;
  end;

  TCUDAFunction = class(TCUDAComponent)
  private
    { Private declarations }
    FKernelName: string;
    FHandle: PCUfunction;
    FAutoSync: Boolean;
    FBlockShape: TCUDADimensions;
    FGrid: TCUDADimensions;
    ParamOffset: Integer;
    FLaunching: Boolean;
    FOnParameterSetup: TNotifyEvent;
    FParams: array of TCUDAPtxNumType;
    procedure SetBlockShape(const AShape: TCUDADimensions);
    procedure SetGrid(const AGrid: TCUDADimensions);
    procedure SetKernelName(const AName: string);
    function GetHandle: PCUfunction;
    procedure SetSharedMemorySize(Value: Integer);
    function GetSharedMemorySize: Integer;
    function GetMaxThreadPerBlock: Integer;
    function GetConstMemorySize: Integer;
    function GetLocalMemorySize: Integer;
    function GetNumRegisters: Integer;
  protected
    { Protected declaration }
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetParam(Value: Integer); overload;
    procedure SetParam(Value: Cardinal); overload;
    procedure SetParam(Value: Single); overload;
    procedure SetParam(MemData: TCUDAMemData); overload;
    procedure SetParam(TexRef: TCUDATexture); overload;
    procedure SetParam(Ptr: Pointer); overload;

    procedure Launch(Grided: Boolean = true);

    property Handle: PCUfunction read GetHandle;
    property SharedMemorySize: Integer read GetSharedMemorySize
      write SetSharedMemorySize;
    property MaxThreadPerBlock: Integer read GetMaxThreadPerBlock;
    property ConstMemorySize: Integer read GetConstMemorySize;
    property LocalMemorySize: Integer read GetLocalMemorySize;
    property NumRegisters: Integer read GetNumRegisters;
  published
    { Published declarations }
    property KernelName: string read fKernelName write SetKernelName;

    property AutoSync: Boolean read fAutoSync write fAutoSync default true;
    property BlockShape: TCUDADimensions read fBlockShape write SetBlockShape;
    property Grid: TCUDADimensions read fGrid write SetGrid;
    property OnParameterSetup: TNotifyEvent read FOnParameterSetup write
      FOnParameterSetup;
  end;

  TCUDATexture = class(TCUDAComponent)
  private
    { Private declarations }
    fKernelName: string;
    fHandle: PCUtexref;
    fArray: TCUDAMemData;
    fAddressModeS,
      fAddressModeT,
      fAddressModeR: TCuAddresMode;
    fNormalizedCoord: Boolean;
    fReadAsInteger: Boolean;
    fFilterMode: TCuFilterMode;
    fFormat: TCUDAChannelType;
    fChannelNum: TCUDAChannelNum;
    procedure SetKernelName(const AName: string);
    procedure SetAddressModeS(const AMode: TCuAddresMode);
    procedure SetAddressModeT(const AMode: TCuAddresMode);
    procedure SetAddressModeR(const AMode: TCuAddresMode);
    procedure SetNormalizedCoord(const flag: Boolean);
    procedure SetReadAsInteger(const flag: Boolean);
    procedure SetFilterMode(const mode: TCuFilterMode);
    procedure SetArray(Value: TCUDAMemData);
    function GetHandle: PCUtexref;
  protected
    { Protected declaration }
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Handle: PCUtexref read GetHandle;
  published
    { Published declarations }
    property KernelName: string read fKernelName write SetKernelName;
    property AddressModeS: TCuAddresMode read fAddressModeS write
      SetAddressModeS default amClamp;
    property AddressModeT: TCuAddresMode read fAddressModeT write
      SetAddressModeT default amClamp;
    property AddressModeR: TCuAddresMode read fAddressModeR write
      SetAddressModeR default amClamp;

    property NormalizedCoord: Boolean read fNormalizedCoord write
      SetNormalizedCoord default true;
    property ReadAsInteger: Boolean read fReadAsInteger write SetReadAsInteger
      default false;
    property FilterMode: TCuFilterMode read fFilterMode write SetFilterMode
      default fmPoint;
    property Format: TCUDAChannelType read fFormat;
    property ChannelNum: TCUDAChannelNum read fChannelNum;
    property MemDataArray: TCUDAMemData read fArray write SetArray;
  end;

  TGLSCUDA = class(TCUDAComponent)
  private
    { Private declarations }
    fDevice: TGLSCUDADevice;
    fContext: TCUDAContext;
    FOnOpenGLInteropInit: TOnOpenGLInteropInit;
    procedure SetDevice(const Value: TGLSCUDADevice);
    procedure SetOnOpenGLInteropInit(AEvent: TOnOpenGLInteropInit);
    function GetModule(const i: Integer): TCUDAModule;
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function GetContext: TCUDAContext; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Context: TCUDAContext read GetContext;
    property Modules[const i: Integer]: TCUDAModule read GetModule;
  published
    { Published declarations }
    property ComputingDevice: TGLSCUDADevice read fDevice write SetDevice;
    property OnOpenGLInteropInit: TOnOpenGLInteropInit read
      FOnOpenGLInteropInit write SetOnOpenGLInteropInit;
  end;

  PDesignerTask = ^TDesignerTask;
  TDesignerTask = record
    ItemClass: TCUDAComponentClass;
    Owner: TCUDAComponent;
    KernelName: string;
    Creating: Boolean;
  end;

procedure RegisterCUDAComponentNameChangeEvent(ANotifyEvent: TNotifyEvent);
procedure DeRegisterCUDAComponentNameChangeEvent;

implementation

uses
  GLStrings,
  GLUtils,
  GLSLog;

resourcestring
  cudasModuleAbsent = 'Module is absent.';
  cudasInvalidParamType = 'Invalid parameter type.';
  cudasOnlyHostData = 'Only host data can Writen/Readen';
  cudasOutOfRange = 'Indexes out of range';
  cudasInvalidValue = 'Invalid value';
  cudasWrongParamSetup =
    'Function''s parameters must be sutup in OnParameterSetup event';
  cudasLaunchFailed = 'Kernel function "%s" launch failed.';
  cudasFuncNotConnected = '%s.Launch: Kernel function not connected';
const
  cAddressMode: array[TCuAddresMode] of TCUaddress_mode = (
    CU_TR_ADDRESS_MODE_WRAP,
    CU_TR_ADDRESS_MODE_CLAMP,
    CU_TR_ADDRESS_MODE_MIRROR);

  cFilterMode: array[TCuFilterMode] of TCUfilter_mode = (
    CU_TR_FILTER_MODE_POINT,
    CU_TR_FILTER_MODE_LINEAR);

var
  GLVirtualHandleCounter: Cardinal = 1;
  vCUDAComponentNameChangeEvent: TNotifyEvent;

procedure CUDAEnumToChannelDesc(const Fmt: TCUarray_format; const nCh: LongWord;
  out oFormat: TCUDAChannelType; out oNum: TCUDAChannelNum);
begin
  case Fmt of
    CU_AD_FORMAT_UNSIGNED_INT8: oFormat := ctUInt8;
    CU_AD_FORMAT_UNSIGNED_INT16: oFormat := ctUInt16;
    CU_AD_FORMAT_UNSIGNED_INT32: oFormat := ctUInt32;
    CU_AD_FORMAT_SIGNED_INT8: oFormat := ctUInt8;
    CU_AD_FORMAT_SIGNED_INT16: oFormat := ctUInt16;
    CU_AD_FORMAT_SIGNED_INT32: oFormat := ctUInt32;
    CU_AD_FORMAT_HALF: oFormat := ctHalfFloat;
    CU_AD_FORMAT_FLOAT: oFormat := ctFloat;
  end;
  case nCh of
    1: oNum := cnOne;
    2: oNum := cnTwo;
    3: oNum := cnTree;
    4: oNum := cnFour;
  end;
end;

procedure RegisterCUDAComponentNameChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  vCUDAComponentNameChangeEvent := ANotifyEvent;
end;

procedure DeRegisterCUDAComponentNameChangeEvent;
begin
  vCUDAComponentNameChangeEvent := nil;
end;

// ------------------
// ------------------ TGLSCUDA ------------------
// ------------------

{$IFDEF GLS_REGION}{$REGION 'TGLSCUDA'}{$ENDIF}
// Create
//

constructor TGLSCUDA.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDevice := nil;
  fContext := TCUDAContext.Create;
  fChanges := [];
end;

destructor TGLSCUDA.Destroy;
begin
  ComputingDevice := nil;
  fContext.Destroy;
  inherited;
end;

procedure TGLSCUDA.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = fDevice) then
    ComputingDevice := nil;
  inherited;
end;

procedure TGLSCUDA.SetDevice(const Value: TGLSCUDADevice);
begin
  if Value <> fDevice then
  begin
    if Assigned(Value) and not Value.Suitable then
      exit;
    if Assigned(fDevice) then
      fDevice.RemoveFreeNotification(Self);
    fDevice := Value;
    if Assigned(fDevice) then
    begin
      fDevice.FreeNotification(Self);
      CuNotifyChange(cuchDevice);
    end;
  end;
end;

procedure TGLSCUDA.SetOnOpenGLInteropInit(AEvent: TOnOpenGLInteropInit);
begin
  FOnOpenGLInteropInit := AEvent;
  CuNotifyChange(cuchContext);
end;

function TGLSCUDA.GetContext: TCUDAContext;
begin
  if cuchDevice in fChanges then
  begin
    if Assigned(FDevice) then
      FContext.Device := FDevice.Device
    else
      FContext.Device := nil;
    Exclude(fChanges, cuchDevice);
    Include(fChanges, cuchContext);
  end;

  if (cuchContext in fChanges) and Assigned(FDevice) then
  begin
    // Getting OpenGL context to make interoperability
    FContext.OnOpenGLInteropInit := FOnOpenGLInteropInit;
    CUDAContextManager.CreateContext(FContext);
    Exclude(fChanges, cuchContext);
  end;

  Result := FContext;
end;

function TGLSCUDA.GetModule(const i: Integer): TCUDAModule;
var
  j, k: Integer;
begin
  Result := nil;
  k := 0;
  for j := 0 to FItems.Count - 1 do
  begin
    if FItems[j] is TCUDAModule then
    begin
      if k = i then
        exit(TCUDAModule(FItems[j]))
      else
        Inc(k);
    end;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDAModule ------------------
// ------------------

{$IFDEF GLS_REGION}{$REGION 'TCUDAmodule'}{$ENDIF}
// Create
//

constructor TCUDAModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHandle := nil;
  fCode := TStringList.Create;
  TStringList(fCode).OnChange := OnChangeCode;
end;

destructor TCUDAModule.Destroy;
begin
  DestroyHandles;
  fCode.Destroy;
  if Assigned(fCompiler) then
    fCompiler.Code := nil;
  inherited Destroy;
end;

procedure TCUDAModule.Assign(Source: TPersistent);
var
  module: TCUDAModule;
begin
  if Source is TCUDAModule then
  begin
    DestroyHandles;
    module := TCUDAModule(Source);
    fCode.Assign(module.fCode);
    fCodeType := module.fCodeType;
    AllocateHandles;
  end;
  inherited Assign(Source);
end;

procedure TCUDAModule.SetCompiler(const Value: TGLSCUDACompiler);
begin
  if Value <> fCompiler then
  begin
    // Compiler must used by only one module
    if Assigned(Value) and Assigned(Value.Code) then
      exit;
    fCompiler := Value;
    if Assigned(fCompiler) then
      fCompiler.Code := fCode;
  end;
end;

function TCUDAModule.GetContext: TCUDAContext;
begin
  Result := TGLSCUDA(FMaster).Context;
end;

procedure TCUDAModule.Loaded;
begin
  inherited Loaded;
  FMakeRevision := True;
  AllocateHandles;
end;

procedure TCUDAModule.AllocateHandles;
var
  func: TCUDAFunction;
  tex: TCUDATexture;
  child: TCUDAComponent;
  i, j: Integer;
  useless: array of TCUDAComponent;
  TaskList: TList;
  Task: PDesignerTask;
begin
  DestroyHandles;
  ParseModule(FInfo, fCode);
  if not (FInfo.bPtx xor FInfo.bCubin) then
    exit;

  if FInfo.bPtx then
    fCodeType := codePtx;
  if FInfo.bCubin then
    fCodeType := codeCubin;

  LoadFromSource;

  //  fFunctionList.Clear;
  //  for I := 0 to FInfo.FuncCounter - 1 do
  //    fFunctionList.Add(FInfo.Func[I].Name);
  //
  //  fTextureList.Clear;
  //  for I := 0 to FInfo.TexRefCounter - 1 do
  //    fTextureList.Add(FInfo.TexRef[I].Name);

  if not FMakeRevision then
    exit;

  // Redefine function and texture with same names
  TaskList := nil;
  for i := 0 to FInfo.FuncCounter - 1 do
  begin
    func := GetKernelFunction(FInfo.Func[I].Name);
    if not Assigned(func) then
    begin
      if csDesigning in ComponentState then
      begin
        if not Assigned(TaskList) then
          TaskList := TList.Create;
        New(Task);
        Task.ItemClass := TCUDAFunction;
        Task.Owner := Self;
        Task.Creating := true;
        Task.KernelName := FInfo.Func[I].Name;
        TaskList.Add(Task);
      end
      else
      begin
        func := TCUDAFunction.Create(Self);
        func.Master := Self;
        func.fKernelName := FInfo.Func[I].Name;
        func.Name := MakeUniqueName('CUDAFunction');
        func.AllocateHandles;
      end;
    end
    else
      func.AllocateHandles;
  end;

  for i := 0 to FInfo.TexRefCounter - 1 do
  begin
    tex := GetKernelTexture(FInfo.TexRef[I].Name);
    if not Assigned(tex) then
    begin
      if csDesigning in ComponentState then
      begin
        if not Assigned(TaskList) then
          TaskList := TList.Create;
        New(Task);
        Task.ItemClass := TCUDATexture;
        Task.Owner := Self;
        Task.Creating := true;
        Task.KernelName := FInfo.TexRef[I].Name;
        TaskList.Add(Task);
      end
      else
      begin
        tex := TCUDATexture.Create(Self);
        tex.Master := Self;
        tex.fKernelName := FInfo.TexRef[I].Name;
        tex.Name := MakeUniqueName('CUDATexture');
        tex.AllocateHandles;
      end;
    end
    else
      tex.AllocateHandles;
  end;
  // Delete unused
  SetLength(useless, ItemsCount);
  j := 0;
  for i := 0 to ItemsCount - 1 do
  begin
    child := Items[i];
    if child is TCUDAFunction then
      if TCUDAFunction(child).fHandle = nil then
      begin
        useless[j] := child;
        inc(j);
      end;
    if child is TCUDATexture then
      if TCUDATexture(child).fHandle = nil then
      begin
        useless[j] := child;
        inc(j);
      end;
  end;
  for i := 0 to j - 1 do
  begin
    if csDesigning in ComponentState then
    begin
      if not Assigned(TaskList) then
        TaskList := TList.Create;
      New(Task);
      Task.Owner := useless[i];
      Task.Creating := false;
      TaskList.Add(Task);
    end
    else
      useless[i].Destroy;
  end;
  if Assigned(TaskList) and Assigned(fCompiler) then
    fCompiler.DesignerTaskList := TaskList;

end;

// DestroyHandles
//

procedure TCUDAModule.DestroyHandles;
var
  i: Integer;
  item: TComponent;
begin
  for i := 0 to ItemsCount - 1 do
  begin
    item := Items[i];
    if item is TCUDAFunction then
      TCUDAFunction(item).DestroyHandles;
    if item is TCUDATexture then
      TCUDATexture(item).DestroyHandles;
  end;
  //  fFunctionList.Clear;
  //  fTextureList.Clear;
end;

// LoadFromFile
//

procedure TCUDAModule.LoadFromFile(const AFilename: string);
var
  status: TCUresult;
  ext: string;
  AnsiFileName: AnsiString;
begin
  if FileExists(AFilename) then
  begin
    ext := ExtractFileExt(AFilename);
    System.Delete(ext, 1, 1);
    ext := AnsiLowerCase(ext);
    fCodeType := codeUndefined;
    if ext = 'ptx' then
      fCodeType := codePtx;
    if ext = 'cubin' then
      fCodeType := codeCubin;
    if ext = 'gpu' then
      fCodeType := codeGpu;

    if (fCodeType = codePtx) or (fCodeType = codeCubin) then
    begin
      Unload;
      Context.Requires;
      AnsiFileName := AnsiString(AFilename);
      status := cuModuleLoad(fHandle, PAnsiChar(AnsiFileName));
      Context.Release;
      if status <> CUDA_SUCCESS then
        Abort;
      fCode.LoadFromFile(AFilename);
      Compiler := nil;
      AllocateHandles;
    end
    else
      GLSLogger.LogErrorFmt('%s.LoadFromFile: file extension must be ptx or cubin',
        [Self.ClassName]);
  end
  else
    GLSLogger.LogErrorFmt(glsFailedOpenFile, [AFilename]);
end;

// LoadFromSource
//

procedure TCUDAModule.LoadFromSource;
var
  Text: AnsiString;
begin

  fCodeType := fCompiler.OutputCodeType;
  if (fCodeType = codePtx) or (fCodeType = codeCubin) then
  begin
    Text := AnsiString(fCode.Text);
    if Length(Text) > 0 then
    begin
      Text := Text + #00;
      Unload;
      Context.Requires;
      FStatus := cuModuleLoadData(fHandle, PAnsiChar(Text));
      Context.Release;
      if FStatus <> CUDA_SUCCESS then
        Abort;
    end;
  end
  else
    GLSLogger.LogErrorFmt('%s.LoadFromSource: source must be ptx or cubin',
      [Self.ClassName]);
end;

// Unload
//

procedure TCUDAModule.Unload;
begin
  if Assigned(fHandle) then
  begin
    Context.Requires;
    FStatus := cuModuleUnload(fHandle);
    Context.Release;
    fHandle := nil;
    DestroyHandles;
    DestroyComponents;
  end;
end;

procedure TCUDAModule.OnChangeCode(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    FMakeRevision := not (csDesigning in ComponentState)
      or (Sender is TGLSCUDACompiler);
    AllocateHandles;
  end;
end;

function TCUDAModule.GetDesignerTaskList: TList;
var
  func: TCUDAFunction;
  tex: TCUDATexture;
  child: TCUDAComponent;
  i, j: Integer;
  useless: array of TCUDAComponent;
  TaskList: TList;
  Task: PDesignerTask;
begin
  Result := nil;
  if not (csDesigning in ComponentState) then
    exit;
  FMakeRevision := False;
  AllocateHandles;

  TaskList := TList.Create;
  for i := 0 to FInfo.FuncCounter - 1 do
  begin
    func := GetKernelFunction(FInfo.Func[I].Name);
    if not Assigned(func) then
    begin
      New(Task);
      Task.ItemClass := TCUDAFunction;
      Task.Owner := Self;
      Task.Creating := true;
      Task.KernelName := FInfo.Func[I].Name;
      TaskList.Add(Task);
    end
    else
      func.AllocateHandles;
  end;

  for i := 0 to FInfo.TexRefCounter - 1 do
  begin
    tex := GetKernelTexture(FInfo.TexRef[I].Name);
    if not Assigned(tex) then
    begin
      New(Task);
      Task.ItemClass := TCUDATexture;
      Task.Owner := Self;
      Task.Creating := true;
      Task.KernelName := FInfo.TexRef[I].Name;
      TaskList.Add(Task);
    end
    else
      tex.AllocateHandles;
  end;
  // Delete unused
  SetLength(useless, ItemsCount);
  j := 0;
  for i := 0 to ItemsCount - 1 do
  begin
    child := Items[i];
    if child is TCUDAFunction then
      if TCUDAFunction(child).fHandle = nil then
      begin
        useless[j] := child;
        inc(j);
      end;
    if child is TCUDATexture then
      if TCUDATexture(child).fHandle = nil then
      begin
        useless[j] := child;
        inc(j);
      end;
  end;
  for i := 0 to j - 1 do
  begin
    New(Task);
    Task.Owner := useless[i];
    Task.Creating := false;
    TaskList.Add(Task);
  end;
  if TaskList.Count > 0 then
    Result := TaskList
  else
    TaskList.Free;
end;

procedure TCUDAModule.SetCode(const Value: TStringList);
begin
  fCode.Assign(Value);
end;

// GetKernelFunction
//

function TCUDAModule.GetKernelFunction(const name: string): TCUDAFunction;
var
  i: Integer;
  item: TComponent;
begin
  Result := nil;
  for i := 0 to Self.ItemsCount - 1 do
  begin
    item := Items[i];
    if item is TCUDAFunction then
      if TCUDAFunction(item).KernelName = name then
      begin
        Result := TCUDAFunction(item);
        exit;
      end;
  end;
end;

// GetKernelTexture
//

function TCUDAModule.GetKernelTexture(const name: string): TCUDATexture;
var
  i: Integer;
  item: TComponent;
begin
  Result := nil;
  for i := 0 to Self.ItemsCount - 1 do
  begin
    item := Items[i];
    if item is TCUDATexture then
      if TCUDATexture(item).KernelName = name then
      begin
        Result := TCUDATexture(item);
        exit;
      end;
  end;
end;

function TCUDAModule.GetKernelFunctionCount: Integer;
begin
  Result := FInfo.FuncCounter;
end;

function TCUDAModule.GetKernelTextureCount: Integer;
begin
  Result := FInfo.TexRefCounter;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDAComponent ------------------
// ------------------

{$IFDEF GLS_REGION}{$REGION 'TCUDAComponent'}{$ENDIF}

destructor TCUDAComponent.Destroy;
begin
  if Assigned(FMaster) then
    FMaster.RemoveItem(Self);
  if Assigned(FItems) then
  begin
    DeleteItems;
    FItems.Free;
  end;
  inherited;
end;

// CuNotifyChange
//

procedure TCUDAComponent.CuNotifyChange(AChange: TCUDAchange);
begin
  Include(fChanges, AChange);
end;

function TCUDAComponent.GetContext: TCUDAContext;
begin
  if Self is TGLSCUDA then
    Result := TGLSCUDA(Self).Context
  else
    Result := TGLSCUDA(FMaster).Context;
end;

procedure TCUDAComponent.CollectStatus(AStatus: TCUresult);
begin
  if AStatus <> CUDA_SUCCESS then
    FStatus := AStatus;
end;

procedure TCUDAComponent.GetChildren(AProc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  if Assigned(FItems) then
    for i := 0 to FItems.Count - 1 do
      if not IsSubComponent(TComponent(FItems.List^[i])) then
        AProc(TComponent(FItems.List^[i]));
end;

// SetParentComponent
//

procedure TCUDAComponent.SetParentComponent(Value: TComponent);
begin
  inherited;
  if Value = FMaster then
    exit;
  if Self is TGLSCUDA then
    exit;
  Master := TCUDAComponent(Value);
end;

// GetParentComponent
//

function TCUDAComponent.GetParentComponent: TComponent;
begin
  Result := FMaster;
end;

// HasParent
//

function TCUDAComponent.HasParent: Boolean;
begin
  Result := Assigned(FMaster);
end;

procedure TCUDAComponent.SetMaster(AMaster: TCUDAComponent);
begin
  if Assigned(FMaster) then
    FMaster.RemoveItem(Self);
  FMaster := AMaster;
  if Assigned(FMaster) then
    FMaster.AddItem(Self);
end;

procedure TCUDAComponent.SetName(const NewName: TComponentName);
begin
  if Name <> NewName then
  begin
    inherited SetName(NewName);
    if Assigned(vCUDAComponentNameChangeEvent) then
      vCUDAComponentNameChangeEvent(Self);
  end;
end;

procedure TCUDAComponent.AddItem(AItem: TCUDAComponent);
begin
  if not Assigned(FItems) then
    FItems := TPersistentObjectList.Create;
  FItems.Add(AItem);
end;

procedure TCUDAComponent.RemoveItem(AItem: TCUDAComponent);
begin
  if not Assigned(FItems) then
    Exit;
  if AItem.FMaster = Self then
  begin
    if AItem.Owner = Self then
      RemoveComponent(AItem);
    FItems.Remove(AItem);
    AItem.FMaster := nil;
  end;
end;

procedure TCUDAComponent.DeleteItems;
var
  child: TCUDAComponent;
begin
  if Assigned(FItems) then
    while FItems.Count > 0 do
    begin
      child := TCUDAComponent(FItems.Pop);
      child.FMaster := nil;
      child.Free;
    end;
end;

function TCUDAComponent.GetItem(const i: Integer): TCUDAComponent;
begin
  if Assigned(FItems) and (i < FItems.Count) then
    Result := TCUDAComponent(FItems[i])
  else
    Result := nil;
end;

function TCUDAComponent.GetItemsCount: Integer;
begin
  if Assigned(FItems) then
    Result := FItems.Count
  else
    Result := 0;
end;

function TCUDAComponent.GetItemByName(const name: string): TCUDAComponent;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to GetItemsCount - 1 do
  begin
    if Items[i].Name = name then
    begin
      Result := Items[i];
      exit;
    end;
  end;
end;

function TCUDAComponent.MakeUniqueName(const BaseName: string): string;
var
  i: Integer;
begin
  Result := BaseName + '1';
  i := 2;
  while GetItemByName(Result) <> nil do
  begin
    Result := BaseName + IntToStr(i);
    Inc(i);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDAFunction ------------------
// ------------------

{$IFDEF GLS_REGION}{$REGION 'TCUDAFunction'}{$ENDIF}

// Create
//

constructor TCUDAFunction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHandle := nil;
  fAutoSync := true;
  fBlockShape := TCUDADimensions.Create(Self);
  fGrid := TCUDADimensions.Create(Self);
  fLaunching := false;
end;

// Destroy
//

destructor TCUDAFunction.Destroy;
begin
  fBlockShape.Destroy;
  fGrid.Destroy;
  DestroyHandles;
  inherited;
end;

procedure TCUDAFunction.AllocateHandles;
var
  LModule: TCUDAModule;
  ansiname: AnsiString;
  pFunc: PCUfunction;
begin
  DestroyHandles;

  if not (FMaster is TCUDAModule) then
  begin
    GLSLogger.LogError(cudasModuleAbsent);
    Abort;
  end;

  if Length(fKernelName) = 0 then
    exit;

  LModule := TCUDAModule(FMaster);
  if not Assigned(LModule.fHandle) then
    exit;

  with LModule.Context.Device do
  begin
    fBlockShape.MaxSizeX := MaxThreadsDim.SizeX;
    fBlockShape.MaxSizeY := MaxThreadsDim.SizeY;
    fBlockShape.MaxSizeZ := MaxThreadsDim.SizeZ;
    fGrid.MaxSizeX := MaxGridSize.SizeX;
    fGrid.MaxSizeY := MaxGridSize.SizeY;
    fGrid.MaxSizeZ := MaxGridSize.SizeZ;
  end;

  ansiname := AnsiString(fKernelName);
  Context.Requires;
  FStatus := cuModuleGetFunction(pFunc, LModule.FHandle, PAnsiChar(ansiname));
  Context.Release;
  if FStatus = CUDA_SUCCESS then
    FHandle := pFunc
  else
    Abort;
  inherited;
end;

// DestroyHandles
//

procedure TCUDAFunction.DestroyHandles;
begin
  inherited;
  fHandle := nil;
  SetLength(FParams, 0);
end;

// SetBlockShape
//

procedure TCUDAFunction.SetBlockShape(const AShape: TCUDADimensions);
begin
  fBlockShape.Assign(AShape);
end;

// SetGrid
//

procedure TCUDAFunction.SetGrid(const AGrid: TCUDADimensions);
begin
  fGrid.Assign(AGrid);
end;

// SetKernelName
//

procedure TCUDAFunction.SetKernelName(const AName: string);
begin
  if csLoading in ComponentState then
    fKernelName := AName
  else if not Assigned(fHandle) then
  begin
    fKernelName := AName;
    AllocateHandles;
  end;
end;

procedure TCUDAFunction.SetParam(Value: Integer);
begin
  if not fLaunching then
  begin
    GLSLogger.LogError(cudasWrongParamSetup);
    Abort;
  end;
  FStatus := cuParamSeti(fHandle, ParamOffset, PCardinal(@Value)^);
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(Cardinal));
end;

procedure TCUDAFunction.SetParam(Value: Cardinal);
begin
  if not fLaunching then
  begin
    GLSLogger.LogError(cudasWrongParamSetup);
    Abort;
  end;
  FStatus := cuParamSeti(fHandle, ParamOffset, Value);
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(Cardinal));
end;

procedure TCUDAFunction.SetParam(Value: Single);
begin
  if not fLaunching then
  begin
    GLSLogger.LogError(cudasWrongParamSetup);
    Abort;
  end;
  FStatus := cuParamSetf(fHandle, ParamOffset, Value);
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(Single));
end;

procedure TCUDAFunction.SetParam(MemData: TCUDAMemData);
begin
  if not fLaunching then
  begin
    GLSLogger.LogError(cudasWrongParamSetup);
    Abort;
  end;
  FStatus := cuParamSeti(fHandle, ParamOffset, Cardinal(MemData.Data));
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(Cardinal));
end;

procedure TCUDAFunction.SetParam(TexRef: TCUDATexture);
var
  HTexRef: PCUTexRef;
begin
  if not fLaunching then
  begin
    GLSLogger.LogError(cudasWrongParamSetup);
    Abort;
  end;
  HTexRef := TexRef.Handle;
  FStatus := cuParamSetTexRef(fHandle, CU_PARAM_TR_DEFAULT, HTexRef);
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

procedure TCUDAFunction.SetParam(Ptr: Pointer);
begin
  if not fLaunching then
  begin
    GLSLogger.LogError(cudasWrongParamSetup);
    Abort;
  end;
  FStatus := cuParamSeti(fHandle, ParamOffset, Cardinal(Ptr));
  if FStatus <> CUDA_SUCCESS then
    Abort;
  Inc(ParamOffset, SizeOf(Cardinal));
end;

// Launch
//

procedure TCUDAFunction.Launch(Grided: Boolean = true);
begin
  if not (FMaster is TCUDAModule) then
  begin
    GLSLogger.LogError(cudasModuleAbsent);
    Abort;
  end;

  if not Assigned(fHandle) then
  begin
    GLSLogger.LogErrorFmt(cudasFuncNotConnected, [Self.ClassName]);
    Abort;
  end;

  if FLaunching then
    exit;

  ParamOffset := 0;

  Context.Requires;
  fLaunching := True;
  if Assigned(FOnParameterSetup) then
  try
    FOnParameterSetup(Self);
  except
    fLaunching := False;
    Context.Release;
    raise;
  end;
  fLaunching := False;

  FStatus := cuParamSetSize(fHandle, ParamOffset);
  CollectStatus(
    cuFuncSetBlockShape(fHandle,
      fBlockShape.SizeX,
      fBlockShape.SizeY,
      fBlockShape.SizeZ));

  if FStatus = CUDA_SUCCESS then
  begin
    // execute the kernel
    if grided then
      FStatus := cuLaunchGrid(fHandle, fGrid.SizeX, fGrid.SizeY)
    else
      FStatus := cuLaunch(fHandle);
    if fAutoSync then
      CollectStatus(cuCtxSynchronize);
  end;
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
  begin
    GLSLogger.LogErrorFmt(cudasLaunchFailed, [Self.Name]);
    Abort;
  end;
end;

function TCUDAFunction.GetHandle: PCUfunction;
begin
  Result := fHandle;
end;

function TCUDAFunction.GetMaxThreadPerBlock: Integer;
begin
  Context.Requires;
  FStatus := cuFuncGetAttribute(
    Result,
    CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK,
    Handle);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

function TCUDAFunction.GetSharedMemorySize: Integer;
begin
  Context.Requires;
  FStatus := cuFuncGetAttribute(
    Result,
    CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES,
    Handle);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

procedure TCUDAFunction.SetSharedMemorySize(Value: Integer);
var
  MemPerBlock: size_t;
begin
  Context.Requires;
  MemPerBlock := TGLSCUDA(TCUDAModule(FMaster).FMaster).FDevice.Device.SharedMemPerBlock;
  if Value < 0 then
    Value := 0
  else if Value > Integer(MemPerBlock) then
    Value := MemPerBlock;
  FStatus := cuFuncSetSharedSize(Handle, Value);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

function TCUDAFunction.GetConstMemorySize: Integer;
begin
  Context.Requires;
  FStatus := cuFuncGetAttribute(
    Result,
    CU_FUNC_ATTRIBUTE_CONST_SIZE_BYTES,
    Handle);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

function TCUDAFunction.GetLocalMemorySize: Integer;
begin
  Context.Requires;
  FStatus := cuFuncGetAttribute(
    Result,
    CU_FUNC_ATTRIBUTE_LOCAL_SIZE_BYTES,
    Handle);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

function TCUDAFunction.GetNumRegisters: Integer;
begin
  Context.Requires;
  FStatus := cuFuncGetAttribute(Result, CU_FUNC_ATTRIBUTE_NUM_REGS,
    Handle);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDAMemData ------------------
// ------------------

{$IFDEF GLS_REGION}{$REGION 'TCUDAMemData'}{$ENDIF}

constructor TCUDAMemData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := nil;
  FHandle := nil;
  FMemoryType := mtHost;
  FWidth := 256;
  FHeight := 0;
  FDepth := 0;
  FPitch := 0;
  FChannelsType := ctInt8;
  FChannelsNum := cnOne;
  FOpenGLRefArray := False;
end;

destructor TCUDAMemData.Destroy;
begin
  if Assigned(fTexture) then
    fTexture.MemDataArray := nil;
  DestroyHandles;
  inherited;
end;

procedure TCUDAMemData.CuNotifyChange(AChange: TCUDAchange);
begin
  inherited CuNotifyChange(AChange);
  if Assigned(fTexture) then
    fTexture.CuNotifyChange(cuchArray);
end;

procedure TCUDAMemData.SetMemoryType(const AType: TCUDAMemType);
begin
  if fMemoryType <> AType then
  begin
    fMemoryType := AType;
    CuNotifyChange(cuchArray);
  end;
end;

procedure TCUDAMemData.SetWidth(const Value: Integer);
begin
  Assert(Value > 0);
  if Value <> fWidth then
  begin
    fWidth := Value;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAMemData.SetHeight(const Value: Integer);
begin
  Assert(Value >= 0);
  if Value <> fHeight then
  begin
    fHeight := Value;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAMemData.SetDepth(const Value: Integer);
begin
  Assert(Value >= 0);
  if Value <> fDepth then
  begin
    fDepth := Value;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAMemData.SetChannelType(const Value: TCUDAChannelType);
begin
  Assert(Value <> ctUndefined);
  if Value <> fChannelsType then
  begin
    fChannelsType := Value;
    CuNotifyChange(cuchArray);
  end;
end;

procedure TCUDAMemData.SetChannelNum(const Value: TCUDAChannelNum);
begin
  if Value <> fChannelsNum then
  begin
    fChannelsNum := Value;
    CuNotifyChange(cuchArray);
  end;
end;

function TCUDAMemData.GetData: TCUdeviceptr;
begin
  if not Assigned(fData) and (fChanges <> []) then
    AllocateHandles;
  Result := fData;
end;

function TCUDAMemData.GetArrayHandle: PCUarray;
begin
  if not Assigned(fHandle) and (fChanges <> []) then
    AllocateHandles;
  Result := fHandle;
end;

procedure TCUDAMemData.AllocateHandles;
const
  cArrayFormat: array[ctUInt8..ctFloat] of TCUarray_format = (
    CU_AD_FORMAT_UNSIGNED_INT8,
    CU_AD_FORMAT_UNSIGNED_INT16,
    CU_AD_FORMAT_UNSIGNED_INT32,
    CU_AD_FORMAT_SIGNED_INT8,
    CU_AD_FORMAT_SIGNED_INT16,
    CU_AD_FORMAT_SIGNED_INT32,
    CU_AD_FORMAT_HALF,
    CU_AD_FORMAT_FLOAT);
var
  Array2DDesc: TCUDA_ARRAY_DESCRIPTOR;
  //  Array3DDesc: TCUDA_ARRAY3D_DESCRIPTOR;
  AlignedSize: Integer;
begin
  DestroyHandles;

  FStatus := CUDA_SUCCESS;
  Context.Requires;
  case fMemoryType of
    mtHost: FStatus := cuMemAllocHost(fData, DataSize);
    mtDevice:
      begin
        if fHeight > 1 then
        begin
          AlignedSize := RoundUpToPowerOf2(ElementSize);
          if AlignedSize < 4 then
            AlignedSize := 4;
          if AlignedSize > 16 then
            AlignedSize := 16;
          FStatus := cuMemAllocPitch(TCUdeviceptr(fData), fPitch,
            Cardinal(fWidth) * ElementSize, fHeight, AlignedSize);
        end
        else
          FStatus := cuMemAlloc(TCUdeviceptr(fData), DataSize);
      end;
    mtArray:
      begin
        Array2DDesc.Width := fWidth;
        Array2DDesc.Height := fHeight;
        Array2DDesc.Format := cArrayFormat[fChannelsType];
        Array2DDesc.NumChannels := Ord(fChannelsNum) + 1;
        FStatus := cuArrayCreate(fHandle, Array2DDesc);
      end;
  end;
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;

  fChanges := [];
  inherited;
end;

procedure TCUDAMemData.DestroyHandles;
begin
  inherited;
  case fMemoryType of
    mtHost, mtDevice:
      if fData = nil then
        exit;
    mtArray:
      if fHandle = nil then
        exit;
  end;

  if not FOpenGLRefArray then
  begin
    Context.Requires;
    case fMemoryType of
      mtHost:
        if Assigned(fData) then
          cuMemFreeHost(fData);

      mtDevice:
        if Assigned(fData) then
          cuMemFree(fData);

      mtArray:
        if Assigned(fHandle) then
        begin
          if Assigned(fTexture) then
            fTexture.MemDataArray := nil;
          cuArrayDestroy(fHandle);
        end;
    end;
    Context.Release;
  end;
  fHandle := nil;
  fData := nil;
  fPitch := 0;
  FOpenGLRefArray := False;
  fChanges := [];
end;

procedure TCUDAMemData.CheckAccess(x, y, z: Integer; cType:
  TCUDAChannelType; cNum: TCUDAChannelNum);
begin
  Assert(fMemoryType = mtHost, cudasOnlyHostData);
  Assert((x >= 0) and (x < fWidth), cudasOutOfRange);
  Assert((y >= 0) and ((y < fHeight) or (fHeight = 0)), cudasOutOfRange);
  Assert((z >= 0) and ((z < fHeight) or (fDepth = 0)), cudasOutOfRange);
  Assert(cType = fChannelsType, cudasInvalidValue);
  Assert(cNum = fChannelsNum, cudasInvalidValue);
end;

procedure TCUDAMemData.FillMem(const Value);
var
  Ptr: TCUdeviceptr;
  RowSize: Integer;
begin
  if fMemoryType = mtDevice then
  begin
    Ptr := TCUdeviceptr(Data);
    if Ptr <> nil then
    begin
      FStatus := CUDA_SUCCESS;
      Context.Requires;
      // 1D memory set
      if fHeight = 0 then
      begin
        case fChannelsType of
          ctUInt8, ctInt8:
            FStatus := cuMemsetD8(Ptr, Byte(Value), DataSize);
          ctUInt16, ctInt16, ctHalfFloat:
            FStatus := cuMemsetD16(Ptr, Word(Value), DataSize);
          ctUInt32, ctInt32, ctFloat:
            FStatus := cuMemsetD32(Ptr, DWord(Value), DataSize);
        end;
      end
        // 2D memory set
      else
      begin
        RowSize := (1 + Ord(FChannelsNum)) * FWidth;
        case fChannelsType of
          ctUInt8, ctInt8:
            FStatus := cuMemsetD2D8(Ptr, fPitch, Byte(Value), RowSize, fHeight);
          ctUInt16, ctInt16, ctHalfFloat:
            FStatus := cuMemsetD2D16(Ptr, fPitch, Word(Value), RowSize, fHeight);
          ctUInt32, ctInt32, ctFloat:
            FStatus := cuMemsetD2D32(Ptr, fPitch, DWord(Value), RowSize, fHeight);
        end;
      end;
      Context.Release;
      if FStatus <> CUDA_SUCCESS then
        Abort
    end;
  end;
end;

function TCUDAMemData.ElementSize: Cardinal;
const
  cTypeSize: array[TCUDAChannelType] of Cardinal =
    (0, 1, 2, 4, 1, 2, 4, 2, 4);
begin
  Result := cTypeSize[fChannelsType] * Cardinal(Ord(fChannelsNum) + 1);
end;

function TCUDAMemData.DataSize: Cardinal;
var
  h, d: Integer;
begin
  h := fHeight;
  if h = 0 then
    h := 1;
  d := fDepth;
  if d = 0 then
    d := 1;
  Result := Cardinal(Width * h * d) * ElementSize;
end;

procedure TCUDAMemData.CopyTo(const dstMemData: TCUDAMemData);
var
  copyParam2D: TCUDA_MEMCPY2D;
  //  copyParam3D: TCUDA_MEMCPY3D;
  Size: Integer;
begin
  if not Assigned(dstMemData) then
    exit;
  if (Depth > 0) or (dstMemData.Depth > 0) then
    exit;
  FStatus := CUDA_SUCCESS;

  if (Height = dstMemData.Height) and (Height = 0) then
  begin
    // 1D copying
    Size := MinInteger(DataSize, dstMemData.DataSize);
    Context.Requires;
    case MemoryType of
      mtHost:
        case dstMemData.MemoryType of
          mtHost:
            Move(Data^, dstMemData.Data^, Size);
          mtDevice:
            FStatus := cuMemcpyHtoD(dstMemData.Data, Data, Size);
          mtArray:
            FStatus := cuMemcpyHtoA(dstMemData.ArrayHandle, 0, Data, Size);
        end;

      mtDevice:
        case dstMemData.MemoryType of
          mtHost:
            FStatus := cuMemcpyDtoH(dstMemData.Data, Data, Size);
          mtDevice:
            FStatus := cuMemcpyDtoD(dstMemData.Data, Data, Size);
          mtArray:
            FStatus := cuMemcpyDtoA(dstMemData.ArrayHandle, 0, Data, Size);
        end;

      mtArray:
        case dstMemData.MemoryType of
          mtHost:
            FStatus := cuMemcpyAtoH(dstMemData.Data, ArrayHandle, 0, Size);
          mtDevice:
            FStatus := cuMemcpyAtoD(dstMemData.Data, ArrayHandle, 0, Size);
          mtArray:
            FStatus := cuMemcpyAtoA(dstMemData.ArrayHandle, 0, ArrayHandle, 0, Size);
        end;
    end;
    Context.Release;
  end
  else
  begin
    // 2D copying
    FillChar(copyParam2D, SizeOf(copyParam2D), 0);
    // Setup source copy parameters
    case MemoryType of
      mtHost:
        begin
          copyParam2D.srcMemoryType := CU_MEMORYTYPE_HOST;
          copyParam2D.srcHost := TCUdeviceptr(Data);
        end;
      mtDevice:
        begin
          copyParam2D.srcMemoryType := CU_MEMORYTYPE_DEVICE;
          copyParam2D.srcDevice := TCUdeviceptr(Data);
        end;
      mtArray:
        begin
          copyParam2D.srcMemoryType := CU_MEMORYTYPE_ARRAY;
          copyParam2D.srcArray := ArrayHandle;
        end;
    end;
    copyParam2D.srcPitch := fPitch;
    // Setup destination copy parameters
    case dstMemData.fMemoryType of
      mtHost:
        begin
          copyParam2D.dstMemoryType := CU_MEMORYTYPE_HOST;
          copyParam2D.dstHost := TCUdeviceptr(dstMemData.Data);
        end;
      mtDevice:
        begin
          copyParam2D.dstMemoryType := CU_MEMORYTYPE_DEVICE;
          copyParam2D.dstDevice := TCUdeviceptr(dstMemData.Data);
        end;
      mtArray:
        begin
          copyParam2D.dstMemoryType := CU_MEMORYTYPE_ARRAY;
          copyParam2D.dstArray := dstMemData.ArrayHandle;
        end;
    end;
    copyParam2D.dstPitch := dstMemData.fPitch;

    copyParam2D.WidthInBytes := MinInteger(ElementSize * Cardinal(Width),
      dstMemData.ElementSize * Cardinal(dstMemData.Width));
    copyParam2D.Height := MinInteger(fHeight, dstMemData.Height);

    Context.Requires;
    FStatus := cuMemcpy2D(@copyParam2D);
    Context.Release;
  end;

  if FStatus <> CUDA_SUCCESS then
    Abort
end;

procedure TCUDAMemData.CopyTo(const GLImage: TGLBitmap32);
var
  copyParam2D: TCUDA_MEMCPY2D;
  //  copyParam3D: TCUDA_MEMCPY3D;
begin
  if not Assigned(GLImage) then
    exit;

  // volume copying not yet realised
  if (fDepth > 0) or (GLImage.Depth > 0) then
    exit;

  FillChar(copyParam2D, SizeOf(copyParam2D), 0);
  // Setup source copy parameters
  case fMemoryType of
    mtHost:
      begin
        copyParam2D.srcMemoryType := CU_MEMORYTYPE_HOST;
        copyParam2D.srcHost := TCUdeviceptr(Data);
      end;
    mtDevice:
      begin
        copyParam2D.srcMemoryType := CU_MEMORYTYPE_DEVICE;
        copyParam2D.srcDevice := TCUdeviceptr(Data);
      end;
    mtArray:
      begin
        copyParam2D.srcMemoryType := CU_MEMORYTYPE_ARRAY;
        copyParam2D.srcArray := ArrayHandle;
      end;
  end;
  copyParam2D.srcPitch := fPitch;
  // Setup destination copy parameters
  copyParam2D.dstMemoryType := CU_MEMORYTYPE_HOST;
  copyParam2D.dstHost := GLImage.Data;
  copyParam2D.dstPitch := GLImage.ElementSize * GLImage.Width;

  copyParam2D.WidthInBytes := MinInteger(ElementSize * Cardinal(Width),
    Cardinal(copyParam2D.dstPitch));
  copyParam2D.Height := MinInteger(Height, GLImage.Height);

  Context.Requires;
  FStatus := cuMemcpy2D(@copyParam2D);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

procedure TCUDAMemData.CopyTo(const GLGraphic: TCUDAGraphicResource;
  Param: Integer = 0);
var
  pMap: TCUdeviceptr;
  mapSize: Cardinal;
begin
  if not Assigned(GLGraphic.FHandle[0]) then
    exit;
  //TODO: volume copying
  if Depth > 0 then
    exit;

  GLGraphic.MapResources;
  if GLGraphic.FResourceType = rtBuffer then
  begin
    if Param < 0 then
    begin
      mapSize := GLGraphic.GetElementArrayDataSize;
      pMap := GLGraphic.GetElementArrayAddress;
    end
    else
    begin
      mapSize := GLGraphic.GetAttributeArraySize(Param);
      pMap := GLGraphic.GetAttributeArrayAddress(Param);
    end;
  end
  else
    exit; //TODO: image copying

  FStatus := CUDA_SUCCESS;
  Context.Requires;
  case fMemoryType of
    mtHost:
      FStatus := cuMemcpyHtoD(pMap, Data, MinInteger(DataSize, mapSize));
    mtDevice:
      FStatus := cuMemcpyDtoD(pMap, Data, MinInteger(DataSize, mapSize));
    mtArray:
      FStatus := cuMemcpyAtoD(pMap, ArrayHandle, 0, MinInteger(DataSize,
        mapSize));
  end;
  Context.Release;
  GLGraphic.UnMapResources;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

procedure TCUDAMemData.CopyFrom(const srcMemData: TCUDAMemData);
begin
  srcMemData.CopyTo(Self);
end;

procedure TCUDAMemData.CopyFrom(const GLImage: TGLBitmap32);
var
  copyParam2D: TCUDA_MEMCPY2D;
  //  copyParam3D: TCUDA_MEMCPY3D;
begin
  if not Assigned(GLImage) then
    exit;
  //TODO: volume copying
  if (fDepth > 0) or (GLImage.Depth > 0) then
    exit;

  FillChar(copyParam2D, SizeOf(copyParam2D), 0);
  // Setup destination copy parameters
  case fMemoryType of
    mtHost:
      begin
        copyParam2D.dstMemoryType := CU_MEMORYTYPE_HOST;
        copyParam2D.dstHost := TCUdeviceptr(Data);
      end;
    mtDevice:
      begin
        copyParam2D.dstMemoryType := CU_MEMORYTYPE_DEVICE;
        copyParam2D.dstDevice := TCUdeviceptr(Data);
      end;
    mtArray:
      begin
        copyParam2D.dstMemoryType := CU_MEMORYTYPE_ARRAY;
        copyParam2D.dstArray := ArrayHandle;
      end;
  end;
  copyParam2D.dstPitch := fPitch;
  // Setup source copy parameters
  copyParam2D.srcMemoryType := CU_MEMORYTYPE_HOST;
  copyParam2D.srcHost := GLImage.Data;
  copyParam2D.srcPitch := GLImage.ElementSize * GLImage.Width;

  copyParam2D.WidthInBytes := MinInteger(ElementSize * Cardinal(fWidth),
    Cardinal(copyParam2D.srcPitch));
  copyParam2D.Height := MinInteger(fHeight, GLImage.Height);

  Context.Requires;
  FStatus := cuMemcpy2D(@copyParam2D);
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

procedure TCUDAMemData.CopyFrom(const GLGraphic: TCUDAGraphicResource;
  Param: Integer);
var
  pMap: TCUdeviceptr;
  mapSize: Cardinal;
begin
  if not Assigned(GLGraphic.FHandle[0]) then
    exit;
  // volume copying not yet realised
  if fDepth > 0 then
    exit;

  GLGraphic.MapResources;
  if GLGraphic.FResourceType = rtBuffer then
  begin
    if Param < 0 then
    begin
      mapSize := GLGraphic.GetElementArrayDataSize;
      pMap := GLGraphic.GetElementArrayAddress;
    end
    else
    begin
      mapSize := GLGraphic.GetAttributeArraySize(Param);
      pMap := GLGraphic.GetAttributeArrayAddress(Param);
    end;
  end
  else
    exit; //TODO: image copying

  FStatus := CUDA_SUCCESS;
  Context.Requires;
  case fMemoryType of
    mtHost:
      FStatus := cuMemcpyDtoH(Data, pMap,
        Cardinal(MinInteger(DataSize, mapSize)));
    mtDevice:
      FStatus := cuMemcpyDtoD(Data, pMap,
        Cardinal(MinInteger(DataSize, mapSize)));
    mtArray:
      FStatus := cuMemcpyDtoA(ArrayHandle, 0, pMap,
        Cardinal(MinInteger(DataSize, mapSize)));
  end;
  Context.Release;
  GLGraphic.UnMapResources;
  if FStatus <> CUDA_SUCCESS then
    Abort;
end;

{$IFDEF GLS_REGION}{$REGION 'GetElement'}{$ENDIF}

procedure TCUDAMemData.GetElement(out Value: Byte; const x: Integer; y: Integer;
  z: Integer);
begin
  CheckAccess(x, y, z, ctUInt8, cnOne);
  Value := PByteArray(fData)[x + y * fWidth + z * fWidth * fHeight];
end;

procedure TCUDAMemData.GetElement(out Value: TVector2b; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt8, cnTwo);
  Move(PByteArray(fData)[2 * (x + y * fWidth + z * fWidth * fHeight)], Value[0],
    2);
end;

procedure TCUDAMemData.GetElement(out Value: TVector3b; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt8, cnTree);
  Move(PByteArray(fData)[3 * (x + y * fWidth + z * fWidth * fHeight)], Value[0],
    3);
end;

procedure TCUDAMemData.GetElement(out Value: TVector4b; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt8, cnFour);
  Move(PByteArray(fData)[4 * (x + y * fWidth + z * fWidth * fHeight)], Value[0],
    4);
end;

procedure TCUDAMemData.GetElement(out Value: Word; const x: Integer; y: Integer;
  z: Integer);
begin
  CheckAccess(x, y, z, ctUInt16, cnOne);
  Value := PWordArray(fData)[x + y * fWidth + z * fWidth * fHeight];
end;

procedure TCUDAMemData.GetElement(out Value: TVector2w; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt16, cnTwo);
  Move(PWordArray(fData)[2 * (x + y * fWidth + z * fWidth * fHeight)], Value[0],
    4);
end;

procedure TCUDAMemData.GetElement(out Value: TVector3w; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt16, cnTree);
  Move(PWordArray(fData)[3 * (x + y * fWidth + z * fWidth * fHeight)], Value[0],
    6);
end;

procedure TCUDAMemData.GetElement(out Value: TVector4w; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt16, cnFour);
  Move(PWordArray(fData)[4 * (x + y * fWidth + z * fWidth * fHeight)], Value[0],
    8);
end;

procedure TCUDAMemData.GetElement(out Value: longint; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctInt32, cnOne);
  Value := PIntegerArray(fData)[x + y * fWidth + z * fWidth * fHeight];
end;

procedure TCUDAMemData.GetElement(out Value: TVector2i; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctInt32, cnTwo);
  Move(PIntegerArray(fData)[2 * (x + y * fWidth + z * fWidth * fHeight)],
    Value[0], 8);
end;

procedure TCUDAMemData.GetElement(out Value: TVector3i; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctInt32, cnTree);
  Move(PIntegerArray(fData)[3 * (x + y * fWidth + z * fWidth * fHeight)],
    Value[0], 12);
end;

procedure TCUDAMemData.GetElement(out Value: TVector4i; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctInt32, cnFour);
  Move(PIntegerArray(fData)[4 * (x + y * fWidth + z * fWidth * fHeight)],
    Value[0], 16);
end;

procedure TCUDAMemData.GetElement(out Value: Single; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctFloat, cnOne);
  Value := PSingleArray(fData)[x + y * fWidth + z * fWidth * fHeight];
end;

procedure TCUDAMemData.GetElement(out Value: TVector2f; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctFloat, cnTwo);
  Move(PSingleArray(fData)[2 * (x + y * fWidth + z * fWidth * fHeight)],
    Value[0], 8);
end;

procedure TCUDAMemData.GetElement(out Value: TVector3f; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctFloat, cnTree);
  Move(PSingleArray(fData)[3 * (x + y * fWidth + z * fWidth * fHeight)],
    Value[0], 12);
end;

procedure TCUDAMemData.GetElement(out Value: TVector4f; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctFloat, cnFour);
  Move(PSingleArray(fData)[4 * (x + y * fWidth + z * fWidth * fHeight)],
    Value[0], 16);
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$REGION 'SetElement'}{$ENDIF}

procedure TCUDAMemData.SetElement(const Value: Byte; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt8, cnOne);
  PByteArray(fData)[x + y * fWidth + z * fWidth * fHeight] := Value;
end;

procedure TCUDAMemData.SetElement(const Value: TVector2b; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt8, cnTwo);
  Move(Value, PByteArray(fData)[2 * (x + y * fWidth + z * fWidth * fHeight)],
    2);
end;

procedure TCUDAMemData.SetElement(const Value: TVector3b; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt8, cnTree);
  Move(Value, PByteArray(fData)[3 * (x + y * fWidth + z * fWidth * fHeight)],
    3);
end;

procedure TCUDAMemData.SetElement(const Value: TVector4b; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt8, cnFour);
  Move(Value, PByteArray(fData)[4 * (x + y * fWidth + z * fWidth * fHeight)],
    4);
end;

procedure TCUDAMemData.SetElement(const Value: Word; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt16, cnOne);
  PWordArray(fData)[x + y * fWidth + z * fWidth * fHeight] := Value;
end;

procedure TCUDAMemData.SetElement(const Value: TVector2w; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt16, cnTwo);
  Move(Value, PWordArray(fData)[2 * (x + y * fWidth + z * fWidth * fHeight)],
    4);
end;

procedure TCUDAMemData.SetElement(const Value: TVector3w; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt16, cnTree);
  Move(Value, PWordArray(fData)[3 * (x + y * fWidth + z * fWidth * fHeight)],
    6);
end;

procedure TCUDAMemData.SetElement(const Value: TVector4w; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctUInt16, cnFour);
  Move(Value, PWordArray(fData)[4 * (x + y * fWidth + z * fWidth * fHeight)],
    8);
end;

procedure TCUDAMemData.SetElement(const Value: longint; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctInt32, cnOne);
  PIntegerArray(fData)[x + y * fWidth + z * fWidth * fHeight] := Value;
end;

procedure TCUDAMemData.SetElement(const Value: TVector2i; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctInt32, cnTwo);
  Move(Value, PIntegerArray(fData)[2 * (x + y * fWidth + z * fWidth * fHeight)],
    8);
end;

procedure TCUDAMemData.SetElement(const Value: TVector3i; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctInt32, cnTree);
  Move(Value, PIntegerArray(fData)[3 * (x + y * fWidth + z * fWidth * fHeight)],
    12);
end;

procedure TCUDAMemData.SetElement(const Value: TVector4i; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctInt32, cnFour);
  Move(Value, PIntegerArray(fData)[4 * (x + y * fWidth + z * fWidth * fHeight)],
    16);
end;

procedure TCUDAMemData.SetElement(const Value: Single; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctFloat, cnOne);
  PSingleArray(fData)[x + y * fWidth + z * fWidth * fHeight] := Value;
end;

procedure TCUDAMemData.SetElement(const Value: TVector2f; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctFloat, cnTwo);
  Move(Value, PSingleArray(fData)[2 * (x + y * fWidth + z * fWidth * fHeight)],
    8);
end;

procedure TCUDAMemData.SetElement(const Value: TVector3f; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctFloat, cnTree);
  Move(Value, PSingleArray(fData)[3 * (x + y * fWidth + z * fWidth * fHeight)],
    12);
end;

procedure TCUDAMemData.SetElement(const Value: TVector4f; const x: Integer; y:
  Integer; z: Integer);
begin
  CheckAccess(x, y, z, ctFloat, cnFour);
  Move(Value, PSingleArray(fData)[4 * (x + y * fWidth + z * fWidth * fHeight)],
    16);
end;
{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDATexture ------------------
// ------------------

{$IFDEF GLS_REGION}{$REGION 'TCUDATexture'}{$ENDIF}

// Create
//

constructor TCUDATexture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHandle := nil;
  fArray := nil;
  AddressModeS := amClamp;
  AddressModeT := amClamp;
  AddressModeR := amClamp;
  NormalizedCoord := true;
  ReadAsInteger := false;
  FilterMode := fmPoint;
  FFormat := ctUndefined;
  FChannelNum := cnOne;
end;

// Destroy
//

destructor TCUDATexture.Destroy;
begin
  if Assigned(fArray) then
    fArray.fTexture := nil;
  DestroyHandles;
  inherited;
end;

// GetHandle
//

function TCUDATexture.GetHandle: PCUtexref;
begin
  if not Assigned(fHandle) or (fChanges <> []) then
    AllocateHandles;
  Result := fHandle;
end;

procedure TCUDATexture.AllocateHandles;
var
  pTex: PCUTexRef;
  LName: AnsiString;
  LModule: TCUDAModule;
  LFlag: Cardinal;
  LFormat: TCUarray_format;
  LChanels: Integer;
begin
  if not (FMaster is TCUDAModule) then
  begin
    GLSLogger.LogError(cudasModuleAbsent);
    Abort;
  end;

  if Length(fKernelName) = 0 then
    exit;

  LModule := TCUDAModule(FMaster);

  LName := AnsiString(fKernelName);
  Context.Requires;
  FStatus := cuModuleGetTexRef(pTex, LModule.fHandle, PAnsiChar(LName));
  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;
  fHandle := pTex;

  Context.Requires;
  // Apply changes
  if (cuchArray in fChanges) and Assigned(fArray) then
  begin
    CollectStatus(
      cuTexRefSetArray(fHandle, fArray.ArrayHandle, CU_TRSA_OVERRIDE_FORMAT));
    fArray.fTexture := Self;
    // Update format
    if cuTexRefGetFormat(LFormat, LChanels, fHandle) = CUDA_SUCCESS then
      CUDAEnumToChannelDesc(LFormat, LChanels, FFormat, FChannelNum);
  end;

  if cuchAddresMode in fChanges then
  begin
    CollectStatus(
      cuTexRefSetAddressMode(fHandle, 0, cAddressMode[fAddressModeS]));
    CollectStatus(
      cuTexRefSetAddressMode(fHandle, 1, cAddressMode[fAddressModeT]));
    CollectStatus(
      cuTexRefSetAddressMode(fHandle, 2, cAddressMode[fAddressModeR]));
  end;

  if cuchFlag in fChanges then
  begin
    LFlag := 0;
    if fNormalizedCoord then
      LFlag := LFlag or CU_TRSF_NORMALIZED_COORDINATES;
    if fReadAsInteger then
      LFlag := LFlag or CU_TRSF_READ_AS_INTEGER;
    CollectStatus(cuTexRefSetFlags(fHandle, LFlag));
  end;

  if cuchFilterMode in fChanges then
    CollectStatus(cuTexRefSetFilterMode(fHandle, cFilterMode[fFilterMode]));

  Context.Release;
  if FStatus <> CUDA_SUCCESS then
    Abort;

  fChanges := [];
  inherited;
end;

procedure TCUDATexture.DestroyHandles;
begin
  fHandle := nil;
  inherited;
end;

procedure TCUDATexture.SetKernelName(const AName: string);
begin
  if csLoading in ComponentState then
    fKernelName := AName
  else if not Assigned(fHandle) then
  begin
    fKernelName := AName;
    AllocateHandles;
  end;
end;

// SetAddressModeS
//

procedure TCUDATexture.SetAddressModeS(const AMode: TCuAddresMode);
begin
  if AMode <> fAddressModeS then
  begin
    fAddressModeS := AMode;
    CuNotifyChange(cuchAddresMode);
  end;
end;

// SetAddressModeT
//

procedure TCUDATexture.SetAddressModeT(const AMode: TCuAddresMode);
begin
  if AMode <> fAddressModeT then
  begin
    fAddressModeT := AMode;
    CuNotifyChange(cuchAddresMode);
  end;
end;

// SetAddressModeR
//

procedure TCUDATexture.SetAddressModeR(const AMode: TCuAddresMode);
begin
  if AMode <> fAddressModeR then
  begin
    fAddressModeR := AMode;
    CuNotifyChange(cuchAddresMode);
  end;
end;

// SetNormalizedCoord
//

procedure TCUDATexture.SetNormalizedCoord(const flag: Boolean);
begin
  if flag <> fNormalizedCoord then
  begin
    fNormalizedCoord := flag;
    CuNotifyChange(cuchFlag);
  end;
end;

// SetReadAsInteger
//

procedure TCUDATexture.SetReadAsInteger(const flag: Boolean);
begin
  if flag <> fReadAsInteger then
  begin
    fReadAsInteger := flag;
    CuNotifyChange(cuchFlag);
  end;
end;

// SetFilterMode
//

procedure TCUDATexture.SetFilterMode(const mode: TCuFilterMode);
begin
  if mode <> fFilterMode then
  begin
    fFilterMode := mode;
    CuNotifyChange(cuchFilterMode);
  end;
end;

// SetDataArray
//

procedure TCUDATexture.SetArray(Value: TCUDAMemData);
begin
  if Value <> fArray then
  begin
    if Assigned(fArray) then
      fArray.fTexture := nil;
    if Assigned(Value) then
    begin
      if Value.MemoryType <> mtArray then
        Value := nil
      else
      begin
        fFormat := Value.fChannelsType;
        fChannelNum := Value.fChannelsNum;
        if Assigned(Value.fTexture) then
          Value.fTexture.MemDataArray := nil;
        Value.fTexture := Self;
      end;
    end
    else
    begin
      fFormat := ctUndefined;
      fChannelNum := cnOne;
    end;
    fArray := Value;
    CuNotifyChange(cuchArray);
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDAGraphicResource ------------------
// ------------------

{$IFDEF GLS_REGION}{$REGION 'TCUDAGraphicResource'}{$ENDIF}

procedure TCUDAGraphicResource.SetMapping(const Value: TCUDAMapping);
begin
  if fMapping <> Value then
  begin
    fMapping := Value;
    CuNotifyChange(cuchMapping);
  end;
end;

procedure TCUDAGraphicResource.OnGLHandleAllocate(Sender: TGLVirtualHandle;
  var Handle: Cardinal);
begin
  Handle := GLVirtualHandleCounter;
  Inc(GLVirtualHandleCounter);
end;

procedure TCUDAGraphicResource.OnGLHandleDestroy(Sender: TGLVirtualHandle;
  var Handle: Cardinal);
begin
  DestroyHandles;
end;

procedure TCUDAGraphicResource.SetArray(var AArray: TCUDAMemData;
  AHandle: PCUarray; ForGLTexture, Volume: Boolean);
var
  Desc2D: TCUDA_ARRAY_DESCRIPTOR;
  Desc3D: TCUDA_ARRAY3D_DESCRIPTOR;
begin
  Context.Requires;
  // Get array descriptor
  if Volume then
    FStatus := cuArray3DGetDescriptor(Desc3D, AHandle)
  else
    FStatus := cuArrayGetDescriptor(Desc2D, AHandle);
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;

  // Set array parameters
  if not Assigned(AArray) then
    AArray := TCUDAMemData.Create(Owner);

  with AArray do
  begin
    if FHandle <> AHandle then
    begin
      DestroyHandles;
      FHandle := AHandle;
    end;
    FOpenGLRefArray := ForGLTexture;
    FMemoryType := mtArray;
    FPitch := 0;
    if Volume then
    begin
      FWidth := Desc3D.Width;
      FHeight := Desc3D.Height;
      FDepth := Desc3D.Depth;
      CUDAEnumToChannelDesc(Desc3D.Format, Desc3D.NumChannels,
        FChannelsType, fChannelsNum);
    end
    else
    begin
      FWidth := Desc2D.Width;
      FHeight := Desc2D.Height;
      FDepth := 0;
      CUDAEnumToChannelDesc(Desc2D.Format, Desc2D.NumChannels,
        FChannelsType, FChannelsNum);
    end;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClasses([TGLSCUDA, TGLSCUDACompiler, TCUDAModule,
    TCUDAFunction, TCUDATexture, TCUDAMemData]);

end.

