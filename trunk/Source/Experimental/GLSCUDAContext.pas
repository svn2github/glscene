//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSCUDAContext <p>

   <b>History : </b><font size=-1><ul>
      <li>19/03/10 - Yar - Creation
   </ul></font><p>
}

unit GLSCUDAContext;

interface

uses
  Classes, GLS_CUDA_API, GLS_CUDA_RunTime, GLS_CL_Platform, GLContext,
  SysUtils;

type

  TDim3 = class(TPersistent)
  private
    { Private declarations }
    FXYZ: array[0..2] of Integer;
    FMaxXYZ: array[0..2] of Integer;
    procedure SetDimComponent(index: Integer; Value: Integer);
    procedure SetMaxDimComponent(index: Integer; Value: Integer);
  public
    { Public declarations }
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property MaxSizeX: Integer index 0 read FMaxXYZ[0] write SetMaxDimComponent;
    property MaxSizeY: Integer index 1 read FMaxXYZ[1] write SetMaxDimComponent;
    property MaxSizeZ: Integer index 2 read FMaxXYZ[2] write SetMaxDimComponent;
  published
    { Published Properties }
    property SizeX: Integer index 0 read FXYZ[0] write SetDimComponent default
      1;
    property SizeY: Integer index 1 read FXYZ[1] write SetDimComponent default
      1;
    property SizeZ: Integer index 2 read FXYZ[2] write SetDimComponent default
      1;
  end;

  TCUDAContext = class;

  TOnOpenGLContextNeeded = procedure(out Context: TGLContext) of object;

  TCUDADevice = class(TPersistent)
  private
    { Private declarations }
    fID: Integer;
    fHandle: TCUdevice;
    fGFlops: Integer;
    fDeviceProperties: TCudaDeviceProp;
    fReady: Boolean;
    fUsed: Boolean;
    fUniqueName: string;
    fMaxThreadsDim: TDim3;
    fMaxGridSize: TDim3;
  protected
    { Protected declarations }
    function GetName: string;
  public
    { Public declarations }
    constructor Create(const id: Integer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {: Returns in bytes the total amount of memory
       available on the device dev in bytes. }
    function TotalMemory: Cardinal;
  published
    { Published declarations }
    property ID: Integer read fID;
    property Name: string read GetName;
    property TotalGlobalMem: size_t read fDeviceProperties.TotalGlobalMem;
    property SharedMemPerBlock: size_t read fDeviceProperties.SharedMemPerBlock;
    property RegsPerBlock: Integer read fDeviceProperties.RegsPerBlock;
    property WarpSize: Integer read fDeviceProperties.WarpSize;
    property MemPitch: size_t read fDeviceProperties.MemPitch;
    property MaxThreadsPerBlock: Integer read
      fDeviceProperties.MaxThreadsPerBlock;
    property MaxThreadsDim: TDim3 read fMaxThreadsDim;
    property MaxGridSize: TDim3 read fMaxGridSize;
    property ClockRate: Integer read fDeviceProperties.ClockRate;
    property TotalConstMem: size_t read fDeviceProperties.TotalConstMem;
    property Major: Integer read fDeviceProperties.Major;
    property Minor: Integer read fDeviceProperties.Minor;
    property TextureAlignment: size_t read fDeviceProperties.TextureAlignment;
    property DeviceOverlap: Integer read fDeviceProperties.DeviceOverlap;
    property MultiProcessorCount: Integer read
      fDeviceProperties.MultiProcessorCount;
  end;

  TGLSCUDADevice = class(TComponent)
  private
    { Private declarations }
    fDeviceName: string;
    function GetDeviceNumber: Cardinal;
    function GetDevice: TCUDADevice;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Ready: Boolean;
  published
    { Published declarations }
    property DeviceName: string read fDeviceName;
    property DeviceNumber: Cardinal read GetDeviceNumber;
    property Device: TCUDADevice read GetDevice;
  end;

  TCUDAContext = class
  private
    { Private declarations }
    fHandle: PCUcontext;
    fDevice: TCUDADevice;
    fActivationCount: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    {: Create context on device }
    procedure CreateContext(const device: TCUDADevice; OnOpenGLNeed:
      TOnOpenGLContextNeeded);
    procedure Requires;
    procedure Release;

    function IsValid: Boolean;
    property Device: TCUDADevice read fDevice;
  end;

  EGLS_CUDA_Context = class(Exception);

implementation

resourcestring
  cudasInvalidContextReg = 'Invalid context registration.';
  cudasContextNotInit = 'Context not initialized';

type
  TCUDAContextManager = class
  private
    { Private declarations }
    fDeviceList: TList;
    fContextList: TList;
    function GetNextUnusedDevice: TCUDADevice;
  protected
    { Protected declarations }
    function GetDevice(i: Integer): TCUDADevice;
    procedure RegisterContext(aContext: TCUDAContext);
    procedure UnRegisterContext(aContext: TCUDAContext);
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    {: Return device by name }
    function GetDeviceByName(const name: string): TCUDADevice;
    {: Marks the context manager for termination. }
    procedure Terminate;
    {: Returns the number of TCUDAcontext object. }
    function ContextCount: Integer;
    {: Returns the number of CUDA compatiable devices. }
    function DeviceCount: Integer;
    {: Returns a device that has a maximum Giga flops }
    function GetMaxGflopsDevice: TCUDADevice;
    {: Access to devices list }
    property Device[i: Integer]: TCUDADevice read GetDevice;
  end;

var
  CUDAContextManager: TCUDAContextManager;

// ------------------
// ------------------ TDim3 ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TDim3'}{$ENDIF}

constructor TDim3.Create;
begin
  inherited;
  FXYZ[0] := 1;
  FXYZ[1] := 1;
  FXYZ[2] := 1;
  FMaxXYZ[0] := MaxInt;
  FMaxXYZ[1] := MaxInt;
  FMaxXYZ[2] := MaxInt;
end;

procedure TDim3.Assign(Source: TPersistent);
begin
  if Source is TDim3 then
  begin
    FMaxXYZ[0] := TDim3(Source).FMaxXYZ[0];
    FMaxXYZ[1] := TDim3(Source).FMaxXYZ[1];
    FMaxXYZ[2] := TDim3(Source).FMaxXYZ[2];
    FXYZ[0] := TDim3(Source).FXYZ[0];
    FXYZ[1] := TDim3(Source).FXYZ[1];
    FXYZ[2] := TDim3(Source).FXYZ[2];
  end;
  inherited Assign(Source);
end;

procedure TDim3.SetDimComponent(index: Integer; Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value > FMaxXYZ[index] then
    Value := FMaxXYZ[index];
  FXYZ[index] := Value;
end;

procedure TDim3.SetMaxDimComponent(index: Integer; Value: Integer);
begin
  FMaxXYZ[index] := Value;
  if FXYZ[index] > FMaxXYZ[index] then
    FXYZ[index] := FMaxXYZ[index];
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDADevice ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCUDAdevice'}{$ENDIF}

// Create
//

constructor TCUDADevice.Create(const id: Integer);
var
  status: TCUResult;
begin
  inherited Create;
  fID := id;
  status := cuDeviceGet(fHandle, id);
  fReady := status = CUDA_SUCCESS;
  fUsed := false;
  fMaxThreadsDim := TDim3.Create;
  fMaxGridSize := TDim3.Create;
  if not fReady then
    exit;
  cudaGetDeviceProperties(fDeviceProperties, id);
  fGFlops := fDeviceProperties.multiProcessorCount *
    fDeviceProperties.clockRate;
  fMaxThreadsDim.FXYZ[0] := fDeviceProperties.maxThreadsDim[0];
  fMaxThreadsDim.FXYZ[1] := fDeviceProperties.maxThreadsDim[1];
  fMaxThreadsDim.FXYZ[2] := fDeviceProperties.maxThreadsDim[2];
  fMaxGridSize.FXYZ[0] := fDeviceProperties.maxGridSize[0];
  fMaxGridSize.FXYZ[1] := fDeviceProperties.maxGridSize[1];
  fMaxGridSize.FXYZ[2] := fDeviceProperties.maxGridSize[2];
end;

// Destroy
//

destructor TCUDADevice.Destroy;
begin
  fMaxThreadsDim.Destroy;
  fMaxGridSize.Destroy;
  inherited;
end;

// Assign
//

procedure TCUDADevice.Assign(Source: TPersistent);
var
  dev: TCUDADevice;
begin
  if Source is TCUDADevice then
  begin
    dev := TCUDADevice(Source);
    fID := dev.fID;
    fHandle := dev.fHandle;
    fGFlops := dev.fGFlops;
    fDeviceProperties := dev.fDeviceProperties;
    fReady := dev.fReady;
    fMaxThreadsDim.Assign(dev.fMaxThreadsDim);
    fMaxGridSize.Assign(dev.fMaxGridSize);
  end;
  inherited Assign(Source);
end;

// GetName
//

function TCUDADevice.GetName: string;
begin
  Result := string(fDeviceProperties.name);
end;

// TotalMemory
//

function TCUDADevice.TotalMemory: Cardinal;
begin
  cuDeviceTotalMem(Result, fHandle);
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TGLSCUDADevice ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLSCUDADevice'}{$ENDIF}

// Create
//

constructor TGLSCUDADevice.Create(AOwner: TComponent);
var
  device: TCUDADevice;
begin
  inherited Create(AOwner);
  device := CUDAContextManager.GetNextUnusedDevice;
  if Assigned(device) and device.fReady then
  begin
    fDeviceName := device.fUniqueName;
    device.fUsed := true;
  end
  else
    fDeviceName := '';
end;

destructor TGLSCUDADevice.Destroy;
var
  device: TCUDADevice;
begin
  inherited;
  device := CUDAContextManager.GetDeviceByName(fDeviceName);
  if Assigned(device) then
    device.fUsed := false;
end;

function TGLSCUDADevice.GetDeviceNumber: Cardinal;
begin
  Result := CUDAContextManager.DeviceCount;
end;

function TGLSCUDADevice.GetDevice: TCUDADevice;
begin
  Result := CUDAContextManager.GetDeviceByName(fDeviceName);
end;

function TGLSCUDADevice.Ready: Boolean;
var
  device: TCUDADevice;
begin
  device := GetDevice;
  Result := Assigned(device);
  if Result then
    Result := device.fReady;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDAContextManager ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCUDAcontextManager'}{$ENDIF}
// Create
//

constructor TCUDAContextManager.Create;
var
  dCount: Integer;
  status: TCUresult;
  i, j: Integer;
  CUDAdevice: TCUDADevice;
begin
  inherited Create;
  fDeviceList := TList.Create;
  fContextList := TList.Create;
  if InitCUDA then
  begin
    dCount := 0;
    status := cuInit(0);
    if status = CUDA_SUCCESS then
      cuDeviceGetCount(dCount);

    // Fill devices list
    for i := 0 to dCount - 1 do
    begin
      CUDAdevice := TCUDADevice.Create(i);
      CUDAdevice.fUniqueName := CUDAdevice.Name;
      j := 2;
      while GetDeviceByName(CUDAdevice.Name) <> nil do
      begin
        CUDAdevice.fUniqueName := CUDAdevice.Name + ' (' + IntToStr(j) + ')';
        Inc(j);
      end;
      fDeviceList.Add(CUDAdevice);
    end;
  end;
end;

// Destroy
//

destructor TCUDAContextManager.Destroy;
var
  i: Integer;
  CUDAdevice: TCUDADevice;
  CUDAcontext: TCUDAContext;
begin
  for i := 0 to fDeviceList.Count - 1 do
  begin
    CUDAdevice := fDeviceList.Items[i];
    CUDAdevice.Free;
  end;
  for i := 0 to fContextList.Count - 1 do
  begin
    CUDAcontext := fContextList.Items[i];
    CUDAcontext.Free;
  end;
  fDeviceList.Destroy;
  fContextList.Destroy;
  CloseCUDA;
  inherited;
end;

// Terminate
//

procedure TCUDAContextManager.Terminate;
begin
  if ContextCount = 0 then
  begin
    CUDAcontextManager := nil;
    Destroy;
  end;
end;

// RegisterContext
//

procedure TCUDAContextManager.RegisterContext(aContext: TCUDAContext);
begin
  if fContextList.IndexOf(aContext) >= 0 then
    raise EGLS_CUDA_Context.Create(cudasInvalidContextReg)
  else
    fContextList.Add(aContext);
end;

// UnRegisterContext
//

procedure TCUDAContextManager.UnRegisterContext(aContext: TCUDAContext);
begin
  if fContextList.IndexOf(aContext) < 0 then
    raise EGLS_CUDA_Context.Create(cudasInvalidContextReg)
  else
    fContextList.Remove(aContext);
end;

// ContextCount
//

function TCUDAContextManager.ContextCount: Integer;
begin
  Result := fContextList.Count;
end;

// DeviceCount
//

function TCUDAContextManager.DeviceCount: Integer;
begin
  Result := fDeviceList.Count;
end;

// GetDevice
//

function TCUDAContextManager.GetDevice(i: Integer): TCUDADevice;
begin
  Result := nil;
  if i < fDeviceList.Count then
    Result := fDeviceList.Items[i];
end;

function TCUDAContextManager.GetDeviceByName(const name: string): TCUDADevice;
var
  i: Integer;
  device: TCUDADevice;
begin
  Result := nil;
  if Length(name) = 0 then
    exit;

  for i := 0 to fDeviceList.Count - 1 do
  begin
    device := fDeviceList[i];
    if device.fUniqueName = name then
    begin
      Result := device;
      exit;
    end;
  end;
end;

// GetMaxGflopsDevice
//

function TCUDAContextManager.GetMaxGflopsDevice: TCUDADevice;
var
  max_gflops: Integer;
  i: Integer;
  device: TCUDADevice;
begin
  device := nil;
  max_gflops := 0;
  for i := 0 to fDeviceList.Count - 1 do
  begin
    if max_gflops < TCUDADevice(fDeviceList.Items[i]).fGFlops then
    begin
      device := fDeviceList.Items[i];
      max_gflops := device.fGFlops;
    end;
  end;
  Result := device;
end;

function TCUDAContextManager.GetNextUnusedDevice: TCUDADevice;
var
  i: Integer;
  device: TCUDADevice;
begin
  Result := nil;
  for i := 0 to fDeviceList.Count - 1 do
  begin
    device := fDeviceList[i];
    if not device.fUsed then
    begin
      Result := device;
      exit;
    end;
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TCUDAContext ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCUDAcontext'}{$ENDIF}
// Create
//

constructor TCUDAContext.Create;
begin
  inherited Create;
  fHandle := nil;
  fDevice := nil;
  fActivationCount := 0;
end;

// Destroy
//

destructor TCUDAContext.Destroy;
begin
  inherited;
  if Assigned(fHandle) then
  begin
    while fActivationCount > 0 do
      Release;
    CUDAcontextManager.UnRegisterContext(Self);
  end;
end;

// CreateContext
//

procedure TCUDAContext.CreateContext(const device: TCUDADevice; OnOpenGLNeed:
  TOnOpenGLContextNeeded);
var
  status: TCUresult;
  cuContext: PCUcontext;
  glContext: TGLContext;
begin
  if not Assigned(device) or not device.fReady then
    exit;
  if Assigned(OnOpenGLNeed) then
  begin
    OnOpenGLNeed(glContext);
    if Assigned(glContext) then
    begin
      glContext.Activate;
      status := cuGLCtxCreate(cuContext, 0, device.fHandle);
      glContext.Deactivate;
    end
    else
      status := CUDA_ERROR_INVALID_CONTEXT;
  end
  else
    status := cuCtxCreate(cuContext, 0, device.fHandle);
  if status <> CUDA_SUCCESS then
  begin
    cuCtxDetach(cuContext);
    exit;
  end;
  CUDAContextManager.RegisterContext(Self);
  fDevice := device;
  fHandle := cuContext;
  fActivationCount := 1;
end;

procedure TCUDAContext.Requires;
var
  status: TCUresult;
  pctx: PCUcontext;
begin
  if not IsValid then
    raise EGLS_CUDA_Context.Create(cudasContextNotInit);

  status := cuCtxAttach(pctx, 0);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA_Context.Create('TCUDAcontext.Requires: ' +
      GetCUDAAPIerrorString(status));
  if pctx <> fHandle then
  begin
    cuCtxDetach(pctx);
    raise EGLS_CUDA_Context.Create('TCUDAcontext.Requires: Invalid CPU thread');
  end;
  Inc(fActivationCount);
end;

procedure TCUDAContext.Release;
var
  status: TCUresult;
begin
  if not IsValid then
    raise EGLS_CUDA_Context.Create('TCUDAcontext.Release: Context not initialized');
  // Zero value of fActivationCount cousing automaticaly destroying of context
  status := cuCtxDetach(fHandle);
  if status <> CUDA_SUCCESS then
    raise EGLS_CUDA_Context.Create('TCUDAcontext.Release: ' +
      GetCUDAAPIerrorString(status));
  Dec(fActivationCount);
end;

function TCUDAContext.IsValid: Boolean;
begin
  Result := (CUDAContextManager.DeviceCount > 0) and (fActivationCount > 0);
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  CUDAContextManager := TCUDAContextManager.Create;
  RegisterClasses([TGLSCUDADevice]);

finalization

  CUDAContextManager.Terminate;

end.
