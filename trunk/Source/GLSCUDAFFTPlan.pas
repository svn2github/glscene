//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLSCUDAFFTPlan <p>

  <b>History : </b><font size=-1><ul>
  <li>05/03/11 - Yar - Refactored
  <li>19/03/10 - Yar - Creation
  </ul></font><p>
}

unit GLSCUDAFFTPlan;

interface

uses
  Classes, SysUtils,
  GLSCUDAContext,
  GLSCUDA, GLS_CUDA_API, GLS_CUDA_FourierTransform;

type

  TCUDAFFTransform =
  (
    fftRealToComplex,
    fftComplexToReal,
    fftComplexToComplex,
    fftDoubleToDoubleComplex,
    fftDoubleComplexToDouble,
    fftDoubleComplexToDoubleComplex
  );

  TCUDAFFTdir = (fftdForward, fftdInverse);

  TCUDAFFTPlan = class(TCUDAComponent)
  private
    { Private declarations }
    FHandle: TcufftHandle;
    FWidth: Integer;
    FHeight: Integer;
    FDepth: Integer;
    FBatch: Integer;
    FPlanSize: Integer;
    FTransform: TCUDAFFTransform;
    FStatus: TcufftResult;
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetDepth(Value: Integer);
    procedure SetBatch(Value: Integer);
    procedure SetTransform(Value: TCUDAFFTransform);
  protected
    { Protected declaration }
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    class procedure CheckLib;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Execute(ASrc: TCUDAMemData; ADst: TCUDAMemData;
      const ADir: TCUDAFFTdir = fftdForward);
  published
    { Published declarations }
    property Width: Integer read fWidth write SetWidth default 256;
    property Height: Integer read FHeight write SetHeight default 0;
    property Depth: Integer read FDepth write SetDepth default 0;
    property Batch: Integer read FBatch write SetBatch default 1;
    property Transform: TCUDAFFTransform read FTransform write SetTransform
      default fftRealToComplex;
  end;

implementation

uses
  GLSLog, GLStrings;

resourcestring
  cudasRequireFreeThread = 'CUFFT functions require context-free thread';
  cudasBadPlanSize = 'MemData size less then Plan size.';

constructor TCUDAFFTPlan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := INVALID_CUFFT_HANDLE;
  fWidth := 256;
  FHeight := 0;
  FDepth := 0;
  FBatch := 1;
  FTransform := fftRealToComplex;
end;

destructor TCUDAFFTPlan.Destroy;
begin
  DestroyHandles;
  inherited;
end;

class procedure TCUDAFFTPlan.CheckLib;
begin
  if not IsCUFFTInitialized then
    if not InitCUFFT then
    begin
      GLSLogger.LogError('Can not initialize CUFFT library');
      Abort;
    end;
end;

procedure TCUDAFFTPlan.Assign(Source: TPersistent);
var
  plan: TCUDAFFTPlan;
begin
  if Source is TCUDAFFTPlan then
  begin
    DestroyHandles;
    plan := TCUDAFFTPlan(Source);
    Width := plan.fWidth;
    Height := plan.FHeight;
    Depth := plan.FDepth;
    Transform := plan.FTransform;
  end;
  inherited Assign(Source);
end;

procedure TCUDAFFTPlan.AllocateHandles;
const
  cTypeSize: array[TCUDAFFTransform] of Byte = (
  SizeOf(TcufftComplex) div 2,
  SizeOf(TcufftComplex) div 2,
  SizeOf(TcufftComplex) div 2,
  SizeOf(TcufftDoubleComplex) div 2,
  SizeOf(TcufftDoubleComplex) div 2,
  SizeOf(TcufftDoubleComplex) div 2);
var
  LType: TcufftType;
begin
  DestroyHandles;

  case FTransform of
    fftRealToComplex:
      LType := CUFFT_R2C;
    fftComplexToReal:
      LType := CUFFT_C2R;
    fftComplexToComplex:
      LType := CUFFT_C2C;
    fftDoubleToDoubleComplex:
      LType := CUFFT_D2Z;
    fftDoubleComplexToDouble:
      LType := CUFFT_Z2D;
    fftDoubleComplexToDoubleComplex:
      LType := CUFFT_Z2Z;
  else
    begin
      Assert(False, glsErrorEx + glsUnknownType);
      LType := CUFFT_R2C;
    end;
  end;

  Context.Requires;

  if (FHeight = 0) and (FDepth = 0) then
  begin
    FStatus := cufftPlan1d(FHandle, fWidth, LType, FBatch);
    FPlanSize := cTypeSize[FTransform] * FWidth;
    if FBatch > 0 then
      FPlanSize := FPlanSize * FBatch;
  end
  else if FDepth = 0 then
  begin
    FStatus := cufftPlan2d(FHandle, fWidth, FHeight, LType);
    FPlanSize := cTypeSize[FTransform] * FWidth * FHeight;
  end
  else
  begin
    FStatus := cufftPlan3d(FHandle, fWidth, FHeight, FDepth, LType);
    FPlanSize := cTypeSize[FTransform] * FWidth * FHeight * FDepth;
  end;

  Context.Release;

  if FStatus <> CUFFT_SUCCESS then
  begin
    FHandle := INVALID_CUFFT_HANDLE;
    Abort;
  end;

  FStatus := cufftSetCompatibilityMode(FHandle, CUFFT_COMPATIBILITY_FFTW_PADDING);
  fChanges := [];
  inherited;
end;

procedure TCUDAFFTPlan.DestroyHandles;
begin
  inherited;
  CheckLib;

  if FHandle <> INVALID_CUFFT_HANDLE then
  begin
    Context.Requires;
    FStatus := cufftDestroy(FHandle);
    Context.Release;
    if FStatus <> CUFFT_SUCCESS then
      Abort;
    FHandle := 0;
    FPlanSize := 0;
  end;
end;

procedure TCUDAFFTPlan.SetWidth(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> fWidth then
  begin
    fWidth := Value;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.SetHeight(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FHeight then
  begin
    FHeight := Value;
    if FHeight > 0 then
      FBatch := 1;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.SetDepth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FDepth then
  begin
    FDepth := Value;
    if FDepth > 0 then
      FBatch := 1;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.SetBatch(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> FBatch then
  begin
    FBatch := Value;
    if FBatch > 1 then
    begin
      FHeight := 0;
      FDepth := 0;
    end;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.SetTransform(Value: TCUDAFFTransform);
begin
  if Value <> FTransform then
  begin
    FTransform := Value;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.Execute(ASrc: TCUDAMemData; ADst: TCUDAMemData;
  const ADir: TCUDAFFTdir);
const
  sFFTdir: array [TCUDAFFTdir] of Integer = (CUFFT_FORWARD, CUFFT_INVERSE);
var
  SrcPtr, DstPtr: Pointer;
begin
  if (FHandle = INVALID_CUFFT_HANDLE) or (fChanges <> []) then
    AllocateHandles;

  if CUDAContextManager.GetCurrentThreadContext <> nil then
  begin
    GLSLogger.LogError(cudasRequireFreeThread);
    Abort;
  end;

  SrcPtr := ASrc.RawData;
  DstPtr := ADst.RawData;

  if (FPlanSize > ASrc.DataSize) or (FPlanSize > ADst.DataSize) then
  begin
    GLSLogger.LogError(cudasBadPlanSize);
    Abort;
  end;

  Context.Requires;

  case FTransform of
    fftRealToComplex:
      FStatus := cufftExecR2C(FHandle, SrcPtr, DstPtr);
    fftComplexToReal:
      FStatus := cufftExecC2R(FHandle, SrcPtr, DstPtr);
    fftComplexToComplex:
      FStatus := cufftExecC2C(FHandle, SrcPtr, DstPtr, sFFTdir[ADir]);
    fftDoubleToDoubleComplex:
      FStatus := cufftExecD2Z(FHandle, SrcPtr, DstPtr);
    fftDoubleComplexToDouble:
      FStatus := cufftExecZ2D(FHandle, SrcPtr, DstPtr);
    fftDoubleComplexToDoubleComplex:
      FStatus := cufftExecZ2Z(FHandle, SrcPtr, DstPtr, sFFTdir[ADir]);
  else
    FStatus := CUFFT_INVALID_VALUE;
  end;

  Context.Release;

  if FStatus <> CUFFT_SUCCESS then
    Abort;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterClasses([TCUDAFFTPlan]);

finalization

  CloseCUFFT;

end.
