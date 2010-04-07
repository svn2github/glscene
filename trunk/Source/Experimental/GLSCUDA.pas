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
  Classes, SysUtils,
  // GLScene
  PersistentClasses, BaseClasses, GLCrossPlatform, GLContext, GLS_CL_Platform,
  VectorGeometry, VectorTypes, VectorLists,
  GLS_CUDA_API, GLSCUDAParser, GLS_CUDA_FastFourierTransformation,
  GLSCUDACompiler, GLSCUDAContext, GLGraphics;

type

  TCUDAMemData = class;
  TCUDAFunction = class;
  TCUDATexture = class;
  TGLSCUDA = class;

  TCUDAChange = (chContext, chSize, chAddresMode, chFlag,
    chFilterMode, chArray, chFormat);
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

  TCUDABaseItem = class(TComponent)
  private
    FMaster: TCUDABaseItem;
    FItems: TPersistentObjectList;
    procedure SetMaster(AMaster: TCUDABaseItem);
    function GetItem(const i: Integer): TCUDABaseItem;
    function GetItemsCount: Integer;
  protected
    { Protected declarations }
    function GetContext: TCUDAContext; virtual;
    procedure ErrorCollect(var code: TCUresult; addcode: TCUresult);
    procedure GetChildren(AProc: TGetChildProc; Root: TComponent); override;
    procedure AddItem(AItem: TCUDABaseItem);
    procedure RemoveItem(AItem: TCUDABaseItem);
    procedure DeleteItems;
    procedure OnNotifyChange(Sender: TObject); virtual;
  public
    { Public declarations }
    destructor Destroy; override;
    function GetParentComponent: TComponent; override;
    procedure SetParentComponent(Value: TComponent); override;
    function HasParent: Boolean; override;
    function GetItemByName(const name: string): TCUDABaseItem;
    function MakeUniqueName(const BaseName: string): string;

    property Master: TCUDABaseItem read FMaster write SetMaster;
    property Context: TCUDAContext read GetContext;
    property Items[const i: Integer]: TCUDABaseItem read GetItem;
    property ItemsCount: Integer read GetItemsCount;
  end;

  TCUDABaseItemClass = class of TCUDABaseItem;

  TCUDAModule = class(TCUDABaseItem)
  private
    { Private declarations }
    FHandle: PCUmodule;
    FCode: TStringList;
    FCodeType: TGLSCUDACompilerOutput;
    FCompiler: TGLSCUDACompiler;
    FInfo: TCUDAModuleInfo;
    procedure SetCode(const Value: TStringList);
    procedure SetCompiler(const Value: TGLSCUDACompiler);
    function GetKernelFunction(const name: string): TCUDAFunction;
    function GetKernelTexture(const name: string): TCUDATexture;
  protected
    { Protected declarations }
    procedure AllocateHandles(MakeRevision: Boolean = true);
    procedure DestroyHandles;
    procedure OnChangeCode(Sender: TObject);
    procedure Loaded; override;
    function GetContext: TCUDAContext; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadFromFile(const filename: string);
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

  TGLResourceType = (rtImage, rtBuffer);

  // Virtual class of graphic resources
  //

  TCUDAGraphicResource = class(TCUDABaseItem)
  protected
    { Protected declaration }
    fHandle: array[0..1] of PCUgraphicsResource;
    fMapping: TCUDAMapping;
    fResourceType: TGLResourceType;
    FGLContextHandle: TGLVirtualHandle;
    FMapCounter: Integer;
    FRegistered: Boolean;
    procedure OnGLHandleAllocate(Sender: TGLVirtualHandle;
      var Handle: Cardinal);
    procedure OnGLHandleDestroy(Sender: TGLVirtualHandle;
      var Handle: Cardinal);
    procedure AllocateHandle; virtual; abstract;
    procedure DestroyHandle; virtual; abstract;
    procedure BindArrayToImage(var cudaArray: TCUDAMemData;
      ALeyer, ALevel: LOngWord); virtual; abstract;
    procedure SetArray(var cudaArray: TCUDAMemData;
      AHandle: PCUarray; ForGLTexture, Volume: Boolean);
    function GetAttributeDataSize(Attr: Integer): LongWord; virtual; abstract;
    function GetAttributeDataAddress(Attr: Integer): Pointer; virtual; abstract;
    function GetIndexDataSize: LongWord; virtual; abstract;
    function GetIndexDataAddress: Pointer; virtual; abstract;
    procedure SetMapping(const Value: TCUDAMapping); virtual;
    property Mapping: TCUDAMapping read fMapping write SetMapping
      default grmDefault;
  public
    procedure MapResources; virtual; abstract;
    procedure UnMapResources; virtual; abstract;
  end;

  TCUDAMemType = (mtHost, mtDevice, mtArray);

  TCUDAMemData = class(TCUDABaseItem)
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
    procedure AllocateMemory;
    procedure FreeMemory;
    procedure CheckAccess(const x, y, z: Integer; cType: TCUDAChannelType; cNum:
      TCUDAChannelNum);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject);
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

  TCUDAFunction = class(TCUDABaseItem)
  private
    { Private declarations }
    FKernelName: string;
    FHandle: PCUfunction;
    FAutoSync: Boolean;
    FBlockShape: TDim3;
    FGrid: TDim3;
    ParamOffset: Integer;
    FLaunching: Boolean;
    FOnParameterSetup: TNotifyEvent;
    FParams: array of TCUDAPtxNumType;
    procedure SetBlockShape(const shape: TDim3);
    procedure SetGrid(const grid: TDim3);
    procedure SetKernelName(const name: string);
    function GetHandle: PCUfunction;
    procedure SetSharedMemorySize(Value: Integer);
    function GetSharedMemorySize: Integer;
    function GetMaxThreadPerBlock: Integer;
    function GetConstMemorySize: Integer;
    function GetLocalMemorySize: Integer;
    function GetNumRegisters: Integer;
  protected
    { Protected declaration }
    procedure AllocateHandle;
    procedure DestroyHandle;
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
    property BlockShape: TDim3 read fBlockShape write SetBlockShape;
    property Grid: TDim3 read fGrid write SetGrid;
    property OnParameterSetup: TNotifyEvent read FOnParameterSetup write
      FOnParameterSetup;
  end;

  TCUDATexture = class(TCUDABaseItem)
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
    fChanges: TCUDAChanges;
    procedure SetKernelName(const name: string);
    procedure SetAddressModeS(const mode: TCuAddresMode);
    procedure SetAddressModeT(const mode: TCuAddresMode);
    procedure SetAddressModeR(const mode: TCuAddresMode);
    procedure SetNormalizedCoord(const flag: Boolean);
    procedure SetReadAsInteger(const flag: Boolean);
    procedure SetFilterMode(const mode: TCuFilterMode);
    procedure SetArray(Value: TCUDAMemData);
    function GetHandle: PCUtexref;
  protected
    { Protected declaration }
    procedure AllocateHandle;
    procedure DestroyHandle;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NotifyChange(const change: TCUDAchange);
    property Handle: PCUtexref read GetHandle;
  published
    { Published declarations }
    property KernelName: string read fKernelName write SetKernelName;
    property AddressModeS: TCuAddresMode read fAddressModeS write
      SetAddressModeS default amWrap;
    property AddressModeT: TCuAddresMode read fAddressModeT write
      SetAddressModeT default amWrap;
    property AddressModeR: TCuAddresMode read fAddressModeR write
      SetAddressModeR default amWrap;

    property NormalizedCoord: Boolean read fNormalizedCoord write
      SetNormalizedCoord default true;
    property ReadAsInteger: Boolean read fReadAsInteger write SetReadAsInteger
      default false;
    property FilterMode: TCuFilterMode read fFilterMode write SetFilterMode
      default fmPoint;
    property Format: TCUDAChannelType read fFormat;
    property NumChannels: TCUDAChannelNum read fChannelNum;
    property MemDataArray: TCUDAMemData read fArray write SetArray;
  end;

  TGLSCUDA = class(TCUDABaseItem)
  private
    { Private declarations }
    fDevice: TGLSCUDADevice;
    fContext: TCUDAContext;
    fOpenGLInteroperability: Boolean;
    fChanges: TCUDAChanges;
    FOnOpenGLContextNeeded: TOnOpenGLContextNeeded;
    procedure SetDevice(const Value: TGLSCUDADevice);
    procedure SetOpenGLInteroperability(const Value: Boolean);
    function GetModule(const i: Integer): TCUDAModule;
  protected
    { Protected declarations }
    procedure DestroyHandles;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function GetContext: TCUDAContext; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(const change: TCUDAchange);

    property Context: TCUDAContext read GetContext;
    property Modules[const i: Integer]: TCUDAModule read GetModule;
  published
    { Published declarations }
    property ComputingDevice: TGLSCUDADevice read fDevice write SetDevice;
    property OpenGLInteroperability: Boolean read fOpenGLInteroperability write
      SetOpenGLInteroperability default true;
    property OnOpenGLContextNeeded: TOnOpenGLContextNeeded read
      FOnOpenGLContextNeeded write FOnOpenGLContextNeeded;
  end;

  PDesignerTask = ^TDesignerTask;
  TDesignerTask = record
    ItemClass: TCUDABaseItemClass;
    Owner: TCUDABaseItem;
    KernelName: string;
    Creating: Boolean;
  end;

  EGLS_CUDA = class(Exception);

implementation

uses
  GLStrings,
  GLUtils;

resourcestring
  cudasModuleAbsent = 'Module is absent.';
  cudasInvalidParamType = 'Invalid parameter type.';
  cudasOnlyHostData = 'Only host data can Writen/Readen';
  cudasOutOfRange = 'Indexes out of range';
  cudasInvalidValue = 'Invalid value';
  cudasWrongParamSetup =
    'Function''s parameters must be sutup in OnParameterSetup event';
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

  // ------------------
  // ------------------ TGLSCUDA ------------------
  // ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLSCUDA'}{$ENDIF}
  // Create
  //

constructor TGLSCUDA.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOpenGLInteroperability := true;
  fDevice := nil;
  fContext := nil;
  fChanges := [];
end;

destructor TGLSCUDA.Destroy;
begin
  DestroyHandles;
  fContext.Destroy;
  SetDevice(nil);
  inherited;
end;

procedure TGLSCUDA.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TGLSCUDA.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = fDevice) then
    ComputingDevice := nil;
  inherited;
end;

procedure TGLSCUDA.DestroyHandles;
var
  i: Integer;
  item: TComponent;
begin
  for i := 0 to ItemsCount - 1 do
  begin
    item := Items[i];
    if item is TCUDAModule then
      TCUDAModule(item).DestroyHandles;
  end;
end;

procedure TGLSCUDA.NotifyChange(const change: TCUDAchange);
begin
  Include(fChanges, change);
end;

procedure TGLSCUDA.SetDevice(const Value: TGLSCUDADevice);
begin
  if Value <> fDevice then
  begin
    if Assigned(Value) and not Value.Ready then
      exit;
    if Assigned(fDevice) then
      fDevice.RemoveFreeNotification(Self);
    fDevice := Value;
    if Assigned(fDevice) then
    begin
      fDevice.FreeNotification(Self);
      NotifyChange(chContext);
    end;
  end;
end;

procedure TGLSCUDA.SetOpenGLInteroperability(const Value: Boolean);
begin
  if Value <> fOpenGLInteroperability then
  begin
    fOpenGLInteroperability := Value;
    NotifyChange(chContext);
  end;
end;

function TGLSCUDA.GetContext: TCUDAContext;
begin
  if (chContext in fChanges) and Assigned(fContext) then
  begin
    DestroyHandles;
    FreeAndNil(fContext);
    Exclude(fChanges, chContext);
  end;

  if not Assigned(fContext) then
  begin
    fContext := TCUDAContext.Create;
    if fOpenGLInteroperability then
      fContext.CreateContext(fDevice.Device, FOnOpenGLContextNeeded)
    else
      fContext.CreateContext(fDevice.Device, nil);
    Exclude(fChanges, chContext);
  end;
  Result := fContext;
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
      begin
        Result := TCUDAModule(FItems[j]);
        exit;
      end
      else
        Inc(k);
    end;
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDAModule ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCUDAmodule'}{$ENDIF}
// Create
//

constructor TCUDAModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHandle := nil;
  fCode := TStringList.Create;
  TStringList(fCode).OnChange := OnChangeCode;
//  fFunctionList := TStringList.Create;
//  fTextureList := TStringList.Create;
end;

destructor TCUDAModule.Destroy;
begin
  DestroyHandles;
  fCode.Destroy;
//  fFunctionList.Destroy;
//  fTextureList.Destroy;
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
  if not Assigned(Result) then
    raise EGLS_CUDA.Create('TCUDAModule.Context: Context not initialized');
end;

procedure TCUDAModule.Loaded;
begin
  inherited Loaded;
  AllocateHandles;
end;

procedure TCUDAModule.AllocateHandles(MakeRevision: Boolean);
var
  func: TCUDAFunction;
  tex: TCUDATexture;
  child: TCUDABaseItem;
  i, j: Integer;
  useless: array of TCUDABaseItem;
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

  if not MakeRevision then
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
        func.AllocateHandle;
      end;
    end
    else
      func.AllocateHandle;
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
        tex.AllocateHandle;
      end;
    end
    else
      tex.AllocateHandle;
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
      TCUDAFunction(item).DestroyHandle;
    if item is TCUDATexture then
      TCUDATexture(item).DestroyHandle;
  end;
//  fFunctionList.Clear;
//  fTextureList.Clear;
end;

// LoadFromFile
//

procedure TCUDAModule.LoadFromFile(const filename: string);
var
  status: TCUresult;
  ext: string;
  AnsiFileName: AnsiString;
begin
  if FileExists(filename) then
  begin
    ext := ExtractFileExt(filename);
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
      AnsiFileName := AnsiString(filename);
      status := cuModuleLoad(fHandle, PAnsiChar(AnsiFileName));
      Context.Release;
      if status <> CUDA_SUCCESS then
        raise EGLS_CUDA.Create('TCUDAmoduleItem.LoadFromFile: ' +
          GetCUDAAPIerrorString(status));
      fCode.LoadFromFile(filename);
      Compiler := nil;
      AllocateHandles;
    end
    else
      Assert(False,
        'TCUDAmoduleItem.LoadFromFile: file extension must be ptx or cubin');
  end
  else
    Assert(False, Format(glsFailedOpenFile, [fileName]));
end;

// LoadFromSource
//

procedure TCUDAModule.LoadFromSource;
var
  status: TCUresult;
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
      status := cuModuleLoadData(fHandle, PAnsiChar(Text));
      Context.Release;
      if status <> CUDA_SUCCESS then
        raise EGLS_CUDA.Create('TCUDAmoduleItem.LoadFromSource: ' +
          GetCUDAAPIerrorString(status));
    end;
  end
  else
    Assert(False,
      'TCUDAmoduleItem.LoadFromSource: source must be ptx or cubin');
end;

// Unload
//

procedure TCUDAModule.Unload;
var
  status: TCUresult;
begin
  if Assigned(fHandle) then
  begin
    Context.Requires;
    status := cuModuleUnload(fHandle);
    Context.Release;
    if status <> CUDA_SUCCESS then
      raise EGLS_CUDA.Create('TCUDAmodule.LoadFromFile: ' +
        GetCUDAAPIerrorString(status));
    fHandle := nil;
    DestroyHandles;
    DestroyComponents;
  end;
end;

procedure TCUDAModule.OnChangeCode(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    AllocateHandles(not (csDesigning in ComponentState) or (Sender is
      TGLSCUDACompiler));
end;

function TCUDAModule.GetDesignerTaskList: TList;
var
  func: TCUDAFunction;
  tex: TCUDATexture;
  child: TCUDABaseItem;
  i, j: Integer;
  useless: array of TCUDABaseItem;
  TaskList: TList;
  Task: PDesignerTask;
begin
  Result := nil;
  if not (csDesigning in ComponentState) then
    exit;
  AllocateHandles(false);
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
      func.AllocateHandle;
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
      tex.AllocateHandle;
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

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDABaseItem ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCUDABaseItem'}{$ENDIF}

destructor TCUDABaseItem.Destroy;
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

function TCUDABaseItem.GetContext: TCUDAContext;
begin
  if Self is TGLSCUDA then
    Result := TGLSCUDA(Self).Context
  else
    Result := TGLSCUDA(FMaster).Context;
end;

procedure TCUDABaseItem.ErrorCollect(var code: TCUresult; addcode: TCUresult);
begin
  if addcode <> CUDA_SUCCESS then
    code := addcode;
end;

procedure TCUDABaseItem.GetChildren(AProc: TGetChildProc; Root: TComponent);
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

procedure TCUDABaseItem.SetParentComponent(Value: TComponent);
begin
  inherited;
  if Value = FMaster then
    exit;
  if Self is TGLSCUDA then
    exit;
  Master := TCUDABaseItem(Value);
end;

// GetParentComponent
//

function TCUDABaseItem.GetParentComponent: TComponent;
begin
  Result := FMaster;
end;

// HasParent
//

function TCUDABaseItem.HasParent: Boolean;
begin
  Result := Assigned(FMaster);
end;

procedure TCUDABaseItem.SetMaster(AMaster: TCUDABaseItem);
begin
  if Assigned(FMaster) then
    FMaster.RemoveItem(Self);
  FMaster := AMaster;
  if Assigned(FMaster) then
    FMaster.AddItem(Self);
end;

procedure TCUDABaseItem.AddItem(AItem: TCUDABaseItem);
begin
  if not Assigned(FItems) then
    FItems := TPersistentObjectList.Create;
  FItems.Add(AItem);
end;

procedure TCUDABaseItem.RemoveItem(AItem: TCUDABaseItem);
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

procedure TCUDABaseItem.DeleteItems;
var
  child: TCUDABaseItem;
begin
  if Assigned(FItems) then
    while FItems.Count > 0 do
    begin
      child := TCUDABaseItem(FItems.Pop);
      child.FMaster := nil;
      child.Free;
    end;
end;

function TCUDABaseItem.GetItem(const i: Integer): TCUDABaseItem;
begin
  if Assigned(FItems) and (i < FItems.Count) then
    Result := TCUDABaseItem(FItems[i])
  else
    Result := nil;
end;

function TCUDABaseItem.GetItemsCount: Integer;
begin
  if Assigned(FItems) then
    Result := FItems.Count
  else
    Result := 0;
end;

function TCUDABaseItem.GetItemByName(const name: string): TCUDABaseItem;
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

function TCUDABaseItem.MakeUniqueName(const BaseName: string): string;
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

procedure TCUDABaseItem.OnNotifyChange(Sender: TObject);
begin
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDAFunction ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCUDAFunction'}{$ENDIF}

// Create
//

constructor TCUDAFunction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHandle := nil;
  fAutoSync := true;
  fBlockShape := TDim3.Create;
  fGrid := TDim3.Create;
  fLaunching := false;
end;

// Destroy
//

destructor TCUDAFunction.Destroy;
begin
  fBlockShape.Destroy;
  fGrid.Destroy;
  inherited Destroy;
end;

procedure TCUDAFunction.AllocateHandle;
var
  module: TCUDAModule;
  ansiname: AnsiString;
  status: TCUresult;
  pFunc: PCUfunction;
begin
  if not (FMaster is TCUDAModule) then
    raise EGLS_CUDA.Create(cudasModuleAbsent);
  if Length(fKernelName) = 0 then
    exit;
  module := TCUDAModule(FMaster);
  if not Assigned(module.fHandle) then
    exit;

  fBlockShape.MaxSizeX := module.Context.Device.MaxThreadsDim.SizeX;
  fBlockShape.MaxSizeY := module.Context.Device.MaxThreadsDim.SizeY;
  fBlockShape.MaxSizeZ := module.Context.Device.MaxThreadsDim.SizeZ;
  fGrid.MaxSizeX := module.Context.Device.MaxGridSize.SizeX;
  fGrid.MaxSizeY := module.Context.Device.MaxGridSize.SizeY;
  fGrid.MaxSizeZ := module.Context.Device.MaxGridSize.SizeZ;

  ansiname := AnsiString(fKernelName);
  Context.Requires;
  status := cuModuleGetFunction(pFunc, module.fHandle, PAnsiChar(ansiname));
  Context.Release;
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAKernelFunction.AllocateHandle: ' +
      GetCUDAAPIerrorString(status));
  fHandle := pFunc;
end;

procedure TCUDAFunction.DestroyHandle;
begin
  fHandle := nil;
  SetLength(FParams, 0);
end;

// SetBlockShape
//

procedure TCUDAFunction.SetBlockShape(const shape: TDim3);
begin
  fBlockShape.Assign(shape);
end;

// SetGrid
//

procedure TCUDAFunction.SetGrid(const grid: TDim3);
begin
  fGrid.Assign(grid);
end;

// SetKernelName
//

procedure TCUDAFunction.SetKernelName(const name: string);
begin
  if csLoading in ComponentState then
    fKernelName := name
  else if not Assigned(fHandle) then
  begin
    fKernelName := name;
    AllocateHandle;
  end;
end;

procedure TCUDAFunction.SetParam(Value: Integer);
var
  status: TCUresult;
begin
  Assert(fLaunching, cudasWrongParamSetup);
  status := cuParamSeti(fHandle, ParamOffset, PCardinal(@Value)^);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.SetParam: ' +
      GetCUDAAPIerrorString(status));
  Inc(ParamOffset, SizeOf(Cardinal));
end;

procedure TCUDAFunction.SetParam(Value: Cardinal);
var
  status: TCUresult;
begin
  Assert(fLaunching, cudasWrongParamSetup);
  status := cuParamSeti(fHandle, ParamOffset, Value);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.SetParam: ' +
      GetCUDAAPIerrorString(status));
  Inc(ParamOffset, SizeOf(Cardinal));
end;

procedure TCUDAFunction.SetParam(Value: Single);
var
  status: TCUresult;
begin
  Assert(fLaunching, cudasWrongParamSetup);
  status := cuParamSetf(fHandle, ParamOffset, Value);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.SetParam: ' +
      GetCUDAAPIerrorString(status));
  Inc(ParamOffset, SizeOf(Single));
end;

procedure TCUDAFunction.SetParam(MemData: TCUDAMemData);
var
  status: TCUresult;
begin
  Assert(fLaunching, cudasWrongParamSetup);
  Assert(Assigned(MemData));
  status := cuParamSeti(fHandle, ParamOffset, Cardinal(MemData.Data));
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.SetParam: ' +
      GetCUDAAPIerrorString(status));
  Inc(ParamOffset, SizeOf(Cardinal));
end;

procedure TCUDAFunction.SetParam(TexRef: TCUDATexture);
var
  status: TCUresult;
  HTexRef: PCUTexRef;
begin
  Assert(fLaunching, cudasWrongParamSetup);
  Assert(Assigned(TexRef));
  HTexRef := TexRef.Handle;
  status := cuParamSetTexRef(fHandle, CU_PARAM_TR_DEFAULT, HTexRef);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.SetParam: ' +
      GetCUDAAPIerrorString(status));
end;

procedure TCUDAFunction.SetParam(Ptr: Pointer);
var
  status: TCUresult;
begin
  Assert(FLaunching, cudasWrongParamSetup);
  status := cuParamSeti(fHandle, ParamOffset, Cardinal(Ptr));
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.SetParam: ' +
      GetCUDAAPIerrorString(status));
  Inc(ParamOffset, SizeOf(Cardinal));
end;

// Launch
//

procedure TCUDAFunction.Launch(Grided: Boolean = true);
var
  status: TCUresult;
begin
  if not (FMaster is TCUDAModule) then
    raise EGLS_CUDA.Create('TCUDAKernelFunction.Launch: ' + cudasModuleAbsent);
  if not Assigned(fHandle) then
    raise
      EGLS_CUDA.Create('TCUDAKernelFunction.Launch: Kernel function not connected');
  Assert(not fLaunching);

  ParamOffset := 0;

  Context.Requires;
  fLaunching := true;
  if Assigned(FOnParameterSetup) then
    FOnParameterSetup(Self);
  fLaunching := false;

  status := cuParamSetSize(fHandle, ParamOffset);
  ErrorCollect(status, cuFuncSetBlockShape(fHandle,
    fBlockShape.SizeX,
    fBlockShape.SizeY,
    fBlockShape.SizeZ));

  if status = CUDA_SUCCESS then
  begin
    // execute the kernel
    if grided then
      status := cuLaunchGrid(fHandle, fGrid.SizeX, fGrid.SizeY)
    else
      status := cuLaunch(fHandle);
    if fAutoSync then
      ErrorCollect(status, cuCtxSynchronize);
  end;
  Context.Release;

  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.Launch: ' +
      GetCUDAAPIerrorString(status));
end;

function TCUDAFunction.GetHandle: PCUfunction;
begin
  Result := fHandle;
end;

function TCUDAFunction.GetMaxThreadPerBlock: Integer;
var
  status: TCUresult;
begin
  status := cuFuncGetAttribute(Result, CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK,
    Handle);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.GetMaxThreadPerBlock: ' +
      GetCUDAAPIerrorString(status));
end;

function TCUDAFunction.GetSharedMemorySize: Integer;
var
  status: TCUresult;
begin
  status := cuFuncGetAttribute(Result, CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES,
    Handle);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.GetSharedMemorySize: ' +
      GetCUDAAPIerrorString(status));
end;

procedure TCUDAFunction.SetSharedMemorySize(Value: Integer);
var
  status: TCUresult;
  MemPerBlock: size_t;
begin
  MemPerBlock := TGLSCUDA(TCUDAModule(FMaster).FMaster).FDevice.Device.SharedMemPerBlock;
  if Value<0 then
    Value := 0
  else if Value>Integer(MemPerBlock) then
    Value := MemPerBlock;
  status := cuFuncSetSharedSize(Handle, Value);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.SetSharedMemorySize: ' +
      GetCUDAAPIerrorString(status));
end;

function TCUDAFunction.GetConstMemorySize: Integer;
var
  status: TCUresult;
begin
  status := cuFuncGetAttribute(Result, CU_FUNC_ATTRIBUTE_CONST_SIZE_BYTES,
    Handle);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.GetConstMemorySize: ' +
      GetCUDAAPIerrorString(status));
end;

function TCUDAFunction.GetLocalMemorySize: Integer;
var
  status: TCUresult;
begin
  status := cuFuncGetAttribute(Result, CU_FUNC_ATTRIBUTE_LOCAL_SIZE_BYTES,
    Handle);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.GetLocalMemorySize: ' +
      GetCUDAAPIerrorString(status));

end;

function TCUDAFunction.GetNumRegisters: Integer;
var
  status: TCUresult;
begin
  status := cuFuncGetAttribute(Result, CU_FUNC_ATTRIBUTE_NUM_REGS,
    Handle);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAfunction.GetNumRegisters: ' +
      GetCUDAAPIerrorString(status));
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDAMemData ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCUDAMemData'}{$ENDIF}

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
  FreeMemory;
  inherited;
end;

procedure TCUDAMemData.NotifyChange(Sender: TObject);
begin
  if Assigned(fTexture) then
  begin
    if fMemoryType <> mtArray then
    begin
      fTexture.MemDataArray := nil;
      fTexture := nil;
    end
    else
      fTexture.NotifyChange(chArray);
  end;
end;

procedure TCUDAMemData.SetMemoryType(const AType: TCUDAMemType);
begin
  if fMemoryType <> AType then
  begin
    FreeMemory;
    fMemoryType := AType;
    NotifyChange(Self);
  end;
end;

procedure TCUDAMemData.SetWidth(const Value: Integer);
begin
  Assert(Value > 0);
  if Value <> fWidth then
  begin
    FreeMemory;
    fWidth := Value;
    NotifyChange(Self);
  end;
end;

procedure TCUDAMemData.SetHeight(const Value: Integer);
begin
  Assert(Value >= 0);
  if Value <> fHeight then
  begin
    FreeMemory;
    fHeight := Value;
    NotifyChange(Self);
  end;
end;

procedure TCUDAMemData.SetDepth(const Value: Integer);
begin
  Assert(Value >= 0);
  if Value <> fDepth then
  begin
    FreeMemory;
    fDepth := Value;
    NotifyChange(Self);
  end;
end;

procedure TCUDAMemData.SetChannelType(const Value: TCUDAChannelType);
begin
  Assert(Value <> ctUndefined);
  if Value <> fChannelsType then
  begin
    FreeMemory;
    fChannelsType := Value;
    NotifyChange(Self);
  end;
end;

procedure TCUDAMemData.SetChannelNum(const Value: TCUDAChannelNum);
begin
  if Value <> fChannelsNum then
  begin
    FreeMemory;
    fChannelsNum := Value;
    NotifyChange(Self);
  end;
end;

function TCUDAMemData.GetData: TCUdeviceptr;
begin
  if not Assigned(fData) and (fMemoryType <> mtArray) then
    AllocateMemory;
  Result := fData;
end;

function TCUDAMemData.GetArrayHandle: PCUarray;
begin
  if not Assigned(fHandle) and (fMemoryType = mtArray) then
    AllocateMemory;
  Result := fHandle;
end;

procedure TCUDAMemData.AllocateMemory;
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
  status: TCUresult;
begin
  FreeMemory;
  status := CUDA_SUCCESS;
  Context.Requires;
  case fMemoryType of
    mtHost: status := cuMemAllocHost(fData, DataSize);
    mtDevice:
      begin
        if fHeight > 1 then
        begin
          AlignedSize := RoundUpToPowerOf2(ElementSize);
          if AlignedSize < 4 then
            AlignedSize := 4;
          if AlignedSize > 16 then
            AlignedSize := 16;
          status := cuMemAllocPitch(TCUdeviceptr(fData), fPitch,
            Cardinal(fWidth) * ElementSize, fHeight, AlignedSize);
        end
        else
          status := cuMemAlloc(TCUdeviceptr(fData), DataSize);
      end;
    mtArray:
      begin
        Array2DDesc.Width := fWidth;
        Array2DDesc.Height := fHeight;
        Array2DDesc.Format := cArrayFormat[fChannelsType];
        Array2DDesc.NumChannels := Ord(fChannelsNum) + 1;
        status := cuArrayCreate(fHandle, Array2DDesc);
      end;
  end;
  Context.Release;
  NotifyChange(Self);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAMemData.AllocateMemory: ' +
      GetCUDAAPIerrorString(status));
end;

procedure TCUDAMemData.FreeMemory;
var
  status: TCUresult;
begin
  case fMemoryType of
    mtHost:
      if fData = nil then
        exit;
    mtDevice:
      if fData = nil then
        exit;
    mtArray:
      if fHandle = nil then
        exit;
  end;

  if not FOpenGLRefArray then
  begin
    status := CUDA_SUCCESS;
    Context.Requires;
    case fMemoryType of
      mtHost: if fData <> nil then
          status := cuMemFreeHost(fData);
      mtDevice: if fData <> nil then
          status := cuMemFree(fData);
      mtArray:
        if fHandle <> nil then
        begin
          if Assigned(fTexture) then
            fTexture.MemDataArray := nil;
          status := cuArrayDestroy(fHandle);
        end;
    end;
    Context.Release;
    if status <> CUDA_SUCCESS then
      raise EGLS_CUDA.Create('TCUDAMemData.FreeMemory: ' +
        GetCUDAAPIerrorString(status));
  end;
  fHandle := nil;
  fData := nil;
  fPitch := 0;
  FOpenGLRefArray := False;
  NotifyChange(Self);
end;

procedure TCUDAMemData.CheckAccess(const x, y, z: Integer; cType:
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
  status: TCUresult;
  Ptr: TCUdeviceptr;
  RowSize: Integer;
begin
  if fMemoryType = mtDevice then
  begin
    Ptr := TCUdeviceptr(Data);
    if Ptr<>nil then
    begin
      status := CUDA_SUCCESS;
      Context.Requires;
      // 1D memory set
      if fHeight = 0 then
      begin
        case fChannelsType of
          ctUInt8, ctInt8:
            status := cuMemsetD8(Ptr, Byte(Value), DataSize);
          ctUInt16, ctInt16, ctHalfFloat:
            status := cuMemsetD16(Ptr, Word(Value), DataSize);
          ctUInt32, ctInt32, ctFloat:
            status := cuMemsetD32(Ptr, DWord(Value), DataSize);
        end;
      end
      // 2D memory set
      else begin
        RowSize := (1 + Ord(FChannelsNum)) * FWidth;
        case fChannelsType of
          ctUInt8, ctInt8:
            status := cuMemsetD2D8(Ptr, fPitch, Byte(Value), RowSize, fHeight);
          ctUInt16, ctInt16, ctHalfFloat:
            status := cuMemsetD2D16(Ptr, fPitch, Word(Value), RowSize, fHeight);
          ctUInt32, ctInt32, ctFloat:
            status := cuMemsetD2D32(Ptr, fPitch, DWord(Value), RowSize, fHeight);
        end;
      end;
      Context.Release;
      if status <> CUDA_SUCCESS then
        raise EGLS_CUDA.Create('TCUDAMemData.FillDeviceMem: ' +
          GetCUDAAPIerrorString(status));
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
  status: TCUresult;
  Size: Integer;
begin
  if not Assigned(dstMemData) then
    exit;
  if (Depth > 0) or (dstMemData.Depth > 0) then
    exit;
  status := CUDA_SUCCESS;

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
            status := cuMemcpyHtoD(dstMemData.Data, Data, Size);
          mtArray:
            status := cuMemcpyHtoA(dstMemData.ArrayHandle, 0, Data, Size);
        end;

      mtDevice:
        case dstMemData.MemoryType of
          mtHost:
            status := cuMemcpyDtoH(dstMemData.Data, Data, Size);
          mtDevice:
            status := cuMemcpyDtoD(dstMemData.Data, Data, Size);
          mtArray:
            status := cuMemcpyDtoA(dstMemData.ArrayHandle, 0, Data, Size);
        end;

      mtArray:
        case dstMemData.MemoryType of
          mtHost:
            status := cuMemcpyAtoH(dstMemData.Data, ArrayHandle, 0, Size);
          mtDevice:
            status := cuMemcpyAtoD(dstMemData.Data, ArrayHandle, 0, Size);
          mtArray:
            status := cuMemcpyAtoA(dstMemData.ArrayHandle, 0, ArrayHandle, 0, Size);
        end;
    end;
    Context.Release;
  end
  else begin
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
    status := cuMemcpy2D(@copyParam2D);
    Context.Release;
  end;

  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAMemData.CopyTo: ' +
      GetCUDAAPIerrorString(status));
end;

procedure TCUDAMemData.CopyTo(const GLImage: TGLBitmap32);
var
  copyParam2D: TCUDA_MEMCPY2D;
  //  copyParam3D: TCUDA_MEMCPY3D;
  status: TCUresult;
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
  status := cuMemcpy2D(@copyParam2D);
  Context.Release;
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAMemData.CopyTo: ' +
      GetCUDAAPIerrorString(status));
end;

procedure TCUDAMemData.CopyTo(const GLGraphic: TCUDAGraphicResource;
  Param: Integer = 0);
var
  status: TCUresult;
  pMap: TCUdeviceptr;
  mapSize: Cardinal;
begin
  if not Assigned(GLGraphic.FHandle[0]) then
    exit;
  //TODO: volume copying not yet realised
  if Depth > 0 then
    exit;
  GLGraphic.MapResources;
  if GLGraphic.FResourceType = rtBuffer then
  begin
    if Param < 0 then
    begin
      mapSize := GLGraphic.GetIndexDataSize;
      pMap := GLGraphic.GetIndexDataAddress;
    end
    else
    begin
      mapSize := GLGraphic.GetAttributeDataSize(Param);
      pMap := GLGraphic.GetAttributeDataAddress(Param);
    end;
  end
  else
    exit; //TODO: image copying

  status := CUDA_SUCCESS;
  Context.Requires;
  case fMemoryType of
    mtHost:
      status := cuMemcpyHtoD(pMap, Data, MinInteger(DataSize, mapSize));
    mtDevice:
      status := cuMemcpyDtoD(pMap, Data, MinInteger(DataSize, mapSize));
    mtArray:
      status := cuMemcpyAtoD(pMap, ArrayHandle, 0, MinInteger(DataSize,
        mapSize));
  end;
  Context.Release;
  GLGraphic.UnMapResources;
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAMemData.CopyTo: ' +
      GetCUDAAPIerrorString(status));
end;

procedure TCUDAMemData.CopyFrom(const srcMemData: TCUDAMemData);
begin
  srcMemData.CopyTo(Self);
end;

procedure TCUDAMemData.CopyFrom(const GLImage: TGLBitmap32);
var
  copyParam2D: TCUDA_MEMCPY2D;
  //  copyParam3D: TCUDA_MEMCPY3D;
  status: TCUresult;
begin
  if not Assigned(GLImage) then
    exit;
  // volume copying not yet realised
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
  status := cuMemcpy2D(@copyParam2D);
  Context.Release;
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAMemData.CopyFrom: ' +
      GetCUDAAPIerrorString(status));
end;

procedure TCUDAMemData.CopyFrom(const GLGraphic: TCUDAGraphicResource;
  Param: Integer);
var
  status: TCUresult;
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
      mapSize := GLGraphic.GetIndexDataSize;
      pMap := GLGraphic.GetIndexDataAddress;
    end
    else
    begin
      mapSize := GLGraphic.GetAttributeDataSize(Param);
      pMap := GLGraphic.GetAttributeDataAddress(Param);
    end;
  end
  else
    exit; //TODO: image copying

  status := CUDA_SUCCESS;
  Context.Requires;
  case fMemoryType of
    mtHost:
      status := cuMemcpyDtoH(Data, pMap,
        Cardinal(MinInteger(DataSize, mapSize)));
    mtDevice:
      status := cuMemcpyDtoD(Data, pMap,
        Cardinal(MinInteger(DataSize, mapSize)));
    mtArray:
      status := cuMemcpyDtoA(ArrayHandle, 0, pMap,
        Cardinal(MinInteger(DataSize, mapSize)));
  end;
  Context.Release;
  GLGraphic.UnMapResources;
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAMemData.CopyFrom: ' +
      GetCUDAAPIerrorString(status));
end;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'GetElement'}{$ENDIF}

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
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'SetElement'}{$ENDIF}

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
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDATexture ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCUDATexture'}{$ENDIF}

// Create
//

constructor TCUDATexture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHandle := nil;
  fArray := nil;
  fAddressModeS := amWrap;
  fAddressModeT := amWrap;
  fAddressModeR := amWrap;
  fNormalizedCoord := true;
  fReadAsInteger := false;
  fFilterMode := fmPoint;
  fFormat := ctUndefined;
  fChannelNum := cnOne;
  fChanges := [chAddresMode, chFlag, chFilterMode, chArray];
end;

// Destroy
//

destructor TCUDATexture.Destroy;
begin
  if Assigned(fArray) then
    fArray.fTexture := nil;
  inherited;
end;

procedure TCUDATexture.NotifyChange(const change: TCUDAchange);
begin
  Include(fChanges, change);
end;

// GetHandle
//

function TCUDATexture.GetHandle: PCUtexref;
var
  status: TCUresult;
  flag: Cardinal;
  format: TCUarray_format;
  chanels: Integer;
begin
  if not Assigned(fHandle) then
    AllocateHandle;

  if Assigned(fHandle) and (fChanges <> []) then
  begin
    status := CUDA_SUCCESS;
    Context.Requires;
    // Apply changes
    if (chArray in fChanges) and Assigned(fArray) then
    begin
      ErrorCollect(status, cuTexRefSetArray(fHandle, fArray.ArrayHandle,
        CU_TRSA_OVERRIDE_FORMAT));
      fArray.fTexture := Self;
      // Update format
      if cuTexRefGetFormat(format, chanels, fHandle) = CUDA_SUCCESS then
        CUDAEnumToChannelDesc(format, chanels, FFormat, FChannelNum);
    end;

    if chAddresMode in fChanges then
    begin
      ErrorCollect(status, cuTexRefSetAddressMode(fHandle, 0,
        cAddressMode[fAddressModeS]));
      ErrorCollect(status, cuTexRefSetAddressMode(fHandle, 1,
        cAddressMode[fAddressModeT]));
      ErrorCollect(status, cuTexRefSetAddressMode(fHandle, 2,
        cAddressMode[fAddressModeR]));
    end;

    if chFlag in fChanges then
    begin
      flag := 0;
      if fNormalizedCoord then
        flag := flag or CU_TRSF_NORMALIZED_COORDINATES;
      if fReadAsInteger then
        flag := flag or CU_TRSF_READ_AS_INTEGER;
      ErrorCollect(status, cuTexRefSetFlags(fHandle, flag));
    end;

    if chFilterMode in fChanges then
      ErrorCollect(status, cuTexRefSetFilterMode(fHandle,
        cFilterMode[fFilterMode]));

    Context.Release;
    if status <> CUDA_SUCCESS then
      raise EGLS_CUDA.Create('TCUDAtexture.Handle: ' +
        GetCUDAAPIerrorString(status));

    fChanges := [];
  end;

  Result := fHandle;
end;

procedure TCUDATexture.AllocateHandle;
var
  status: TCUresult;
  pTex: PCUTexRef;
  ansiname: AnsiString;
  module: TCUDAModule;
begin
  if not (FMaster is TCUDAModule) then
    raise EGLS_CUDA.Create(cudasModuleAbsent);
  if Length(fKernelName) = 0 then
    exit;
  module := TCUDAModule(FMaster);

  ansiname := AnsiString(fKernelName);
  Context.Requires;
  status := cuModuleGetTexRef(pTex, module.fHandle, PAnsiChar(ansiname));
  Context.Release;
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDATexture.AllocateHandle: ' +
      GetCUDAAPIerrorString(status));
  fHandle := pTex;
end;

procedure TCUDATexture.DestroyHandle;
begin
  fHandle := nil;
end;

procedure TCUDATexture.SetKernelName(const name: string);
begin
  if csLoading in ComponentState then
    fKernelName := name
  else if not Assigned(fHandle) then
  begin
    fKernelName := name;
    AllocateHandle;
  end;
end;

// SetAddressModeS
//

procedure TCUDATexture.SetAddressModeS(const mode: TCuAddresMode);
begin
  if mode <> fAddressModeS then
  begin
    fAddressModeS := mode;
    NotifyChange(chAddresMode);
  end;
end;

// SetAddressModeT
//

procedure TCUDATexture.SetAddressModeT(const mode: TCuAddresMode);
begin
  if mode <> fAddressModeT then
  begin
    fAddressModeT := mode;
    NotifyChange(chAddresMode);
  end;
end;

// SetAddressModeR
//

procedure TCUDATexture.SetAddressModeR(const mode: TCuAddresMode);
begin
  if mode <> fAddressModeR then
  begin
    fAddressModeR := mode;
    NotifyChange(chAddresMode);
  end;
end;

// SetNormalizedCoord
//

procedure TCUDATexture.SetNormalizedCoord(const flag: Boolean);
begin
  if flag <> fNormalizedCoord then
  begin
    fNormalizedCoord := flag;
    NotifyChange(chFlag);
  end;
end;

// SetReadAsInteger
//

procedure TCUDATexture.SetReadAsInteger(const flag: Boolean);
begin
  if flag <> fReadAsInteger then
  begin
    fReadAsInteger := flag;
    NotifyChange(chFlag);
  end;
end;

// SetFilterMode
//

procedure TCUDATexture.SetFilterMode(const mode: TCuFilterMode);
begin
  if mode <> fFilterMode then
  begin
    fFilterMode := mode;
    NotifyChange(chFilterMode);
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
    NotifyChange(chArray);
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDAGraphicResource ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCUDAGraphicResource'}{$ENDIF}

procedure TCUDAGraphicResource.SetMapping(const Value: TCUDAMapping);
begin
  if fMapping <> Value then
  begin
    fMapping := Value;
    DestroyHandle;
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
  DestroyHandle;
end;

procedure TCUDAGraphicResource.SetArray(var cudaArray: TCUDAMemData;
  AHandle: PCUarray; ForGLTexture, Volume: Boolean);
var
  Desc2D: TCUDA_ARRAY_DESCRIPTOR;
  Desc3D: TCUDA_ARRAY3D_DESCRIPTOR;
  status: TCUresult;
begin
  Context.Requires;
  // Get array descriptor
  if Volume then
    status := cuArray3DGetDescriptor(Desc3D, AHandle)
  else
    status := cuArrayGetDescriptor(Desc2D, AHandle);
  Context.Release;
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA.Create('TCUDAGraphicResource.SetArray: ' +
      GetCUDAAPIerrorString(status));

  // Set array parameters
  if not Assigned(cudaArray) then
    cudaArray := TCUDAMemData.Create(Owner);

  with cudaArray do
  begin
    if FHandle <> AHandle then
    begin
      FreeMemory;
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
    else begin
      FWidth := Desc2D.Width;
      FHeight := Desc2D.Height;
      FDepth := 0;
      CUDAEnumToChannelDesc(Desc2D.Format, Desc2D.NumChannels,
        FChannelsType, FChannelsNum);
    end;
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

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

