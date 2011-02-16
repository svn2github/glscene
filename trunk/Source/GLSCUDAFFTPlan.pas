//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLSCUDAFFTPlan <p>

  <b>History : </b><font size=-1><ul>
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

  TCUDAFFTransform = (fftRealToComplex, fftComplexToReal, fftComplexToComplex,
    fftDoubleToDoubleComplex, fftDoubleComplexToDouble,
    fftDoubleComplexToDoubleComplex);

  TCUDAFFTdir = (fftdForward, fftdInverse);

  TCUDAFFTPlan = class(TCUDAComponent)
  private
    { Private declarations }
    fHandle: TcufftHandle;
    fWidth: Integer;
    fHeight: Integer;
    fDepth: Integer;
    fBatch: Integer;
    FPlanSize: Cardinal;
    fTransform: TCUDAFFTransform;
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
    property Height: Integer read fHeight write SetHeight default 0;
    property Depth: Integer read fDepth write SetDepth default 0;
    property Batch: Integer read fBatch write SetBatch default 1;
    property Transform: TCUDAFFTransform read fTransform write SetTransform
      default fftRealToComplex;
  end;

implementation

uses
  GLSLog, GLStrings;

resourcestring
  cudasRequireFreeThread = 'CUFFT functions require context-free thread';
  cudasBadPlanSize = 'MemData size less then Plan size.';

var FDummyPlan: TcufftHandle;

constructor TCUDAFFTPlan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHandle := 0;
  fWidth := 256;
  fHeight := 0;
  fDepth := 0;
  fBatch := 1;
  fTransform := fftRealToComplex;
  if not(csDesigning in ComponentState) then
    if InitCUFFT then
    begin
      if FDummyPlan = 0 then
        cufftPlan1d(FDummyPlan, 256, CUFFT_R2C, 1);
    end;
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
    Height := plan.fHeight;
    Depth := plan.fDepth;
    Transform := plan.fTransform;
  end;
  inherited Assign(Source);
end;

procedure TCUDAFFTPlan.AllocateHandles;
const
  cTypeSize: array[TCUDAFFTransform] of Byte = (
  SizeOf(TcufftComplex),
  SizeOf(TcufftComplex),
  SizeOf(TcufftComplex),
  SizeOf(TcufftDoubleComplex),
  SizeOf(TcufftDoubleComplex),
  SizeOf(TcufftDoubleComplex));
var
  LType: TcufftType;
begin
  DestroyHandles;

  case fTransform of
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

  if (fHeight = 0) and (fDepth = 0) then
  begin
    FStatus := cufftPlan1d(fHandle, fWidth, LType, fBatch);
    FPlanSize := cTypeSize[fTransform] * FWidth;
    if fBatch > 0 then
      FPlanSize := FPlanSize * Cardinal(fBatch);
  end
  else if fDepth = 0 then
  begin
    FStatus := cufftPlan2d(fHandle, fWidth, fHeight, LType);
    FPlanSize := cTypeSize[fTransform] * FWidth * fHeight;
  end
  else
  begin
    FStatus := cufftPlan3d(fHandle, fWidth, fHeight, fDepth, LType);
    FPlanSize := cTypeSize[fTransform] * FWidth * fHeight * fDepth;
  end;

  if FStatus <> CUFFT_SUCCESS then
    Abort;

  fChanges := [];
  inherited;
end;

procedure TCUDAFFTPlan.DestroyHandles;
begin
  inherited;
  CheckLib;
  if CUDAContextManager.GetCurrentThreadContext <> nil then
  begin
    GLSLogger.LogError(cudasRequireFreeThread);
    Abort;
  end;

  if fHandle > 0 then
  begin
    Context.Requires;
    FStatus := cufftDestroy(fHandle);
    Context.Release;
    if FStatus <> CUFFT_SUCCESS then
      Abort;
    fHandle := 0;
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
  if Value <> fHeight then
  begin
    fHeight := Value;
    if fHeight > 0 then
      fBatch := 1;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.SetDepth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> fDepth then
  begin
    fDepth := Value;
    if fDepth > 0 then
      fBatch := 1;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.SetBatch(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> fBatch then
  begin
    fBatch := Value;
    if fBatch > 1 then
    begin
      fHeight := 0;
      fDepth := 0;
    end;
    CuNotifyChange(cuchSize);
  end;
end;

procedure TCUDAFFTPlan.SetTransform(Value: TCUDAFFTransform);
begin
  if Value <> fTransform then
  begin
    fTransform := Value;
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
  if (FHandle = 0) or (fChanges <> []) then
    AllocateHandles;

  if CUDAContextManager.GetCurrentThreadContext <> nil then
  begin
    GLSLogger.LogError(cudasRequireFreeThread);
    Abort;
  end;

  SrcPtr := ASrc.Data;
  DstPtr := ADst.Data;

  if (FPlanSize > ASrc.DataSize) or (FPlanSize > ADst.DataSize) then
  begin
    GLSLogger.LogError(cudasBadPlanSize);
    Abort;
  end;

  case fTransform of
    fftRealToComplex:
      FStatus := cufftExecR2C(fHandle, SrcPtr, DstPtr);
    fftComplexToReal:
      FStatus := cufftExecC2R(fHandle, SrcPtr, DstPtr);
    fftComplexToComplex:
      FStatus := cufftExecC2C(fHandle, SrcPtr, DstPtr, sFFTdir[ADir]);
    fftDoubleToDoubleComplex:
      FStatus := cufftExecD2Z(fHandle, SrcPtr, DstPtr);
    fftDoubleComplexToDouble:
      FStatus := cufftExecZ2D(fHandle, SrcPtr, DstPtr);
    fftDoubleComplexToDoubleComplex:
      FStatus := cufftExecZ2Z(fHandle, SrcPtr, DstPtr, sFFTdir[ADir]);
  else
    FStatus := CUFFT_INVALID_VALUE;
  end;
  if True then

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

  if FDummyPlan <> 0 then
    cufftDestroy(FDummyPlan);
  CloseCUFFT;

end.
