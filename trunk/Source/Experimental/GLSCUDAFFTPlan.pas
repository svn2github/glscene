//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSCUDAFFTPlan <p>

   <b>History : </b><font size=-1><ul>
      <li>19/03/10 - Yar - Creation
   </ul></font><p>
}
unit GLSCUDAFFTPlan;

interface

uses
  Classes, SysUtils,
  GLSCUDA, GLS_CUDA_FastFourierTransformation;

type

  TCUDAFFTransform = (
    fftRealToComplex,
    fftComplexToReal,
    fftComplexToComplex,
    fftDoubleToDoubleComplex,
    fftDoubleComplexToDouble,
    fftDoubleComplexToDoubleComplex
    );

  TCUDAFFTdir = (fftdForward, fftdInverse);

  TCUDAFFTPlan = class(TCUDABaseItem)
  private
    { Private declarations }
    fHandle: TcufftHandle;
    fWidth: Integer;
    fHeight: Integer;
    fDepth: Integer;
    fBatch: Integer;
    fTransform: TCUDAFFTransform;
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetDepth(const Value: Integer);
    procedure SetBatch(const Value: Integer);
    procedure SetTransform(const Value: TCUDAFFTransform);
  protected
    { Protected declaration }
    procedure AllocateHandle;
    procedure DestroyHandle;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Execute(src: TCUDAMemData; dst: TCUDAMemData; const dir:
      TCUDAFFTdir = fftdForward);
  published
    { Published declarations }
    property Width: Integer read fWidth write SetWidth default 256;
    property Height: Integer read fHeight write SetHeight default 0;
    property Depth: Integer read fDepth write SetDepth default 0;
    property Batch: Integer read fBatch write SetBatch default 0;
    property Transform: TCUDAFFTransform read fTransform write SetTransform
      default
      fftRealToComplex;
  end;

  ECUDAFFTPlan = class(Exception);

implementation

const
  cCUDAFFT: array[TCUDAFFTransform] of Byte = (
    CUFFT_R2C,
    CUFFT_C2R,
    CUFFT_C2C,
    CUFFT_D2Z,
    CUFFT_Z2D,
    CUFFT_Z2Z
    );

constructor TCUDAFFTPlan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHandle := 0;
  fWidth := 256;
  fHeight := 0;
  fDepth := 0;
  fBatch := 0;
  fTransform := fftRealToComplex;
  if not (csDesigning in ComponentState) then
    InitCUFFT;
end;

destructor TCUDAFFTPlan.Destroy;
begin
  DestroyHandle;
  inherited;
end;

procedure TCUDAFFTPlan.Assign(Source: TPersistent);
var
  plan: TCUDAFFTPlan;
begin
  if Source is TCUDAFFTPlan then
  begin
    DestroyHandle;
    plan := TCUDAFFTPlan(Source);
    fWidth := plan.fWidth;
    fHeight := plan.fHeight;
    fDepth := plan.fDepth;
    fTransform := plan.fTransform;
    AllocateHandle;
  end;
  inherited Assign(Source);
end;

procedure TCUDAFFTPlan.AllocateHandle;
var
  status: TcufftResult;
begin
  if not IsCUFFTInitialized then
    if not InitCUFFT then
      raise ECUDAFFTPlan.Create('Can not initialize library');

  Context.Requires;
  if (fHeight = 0) and (fDepth = 0) then
  begin
    status := cufftPlan1d(fHandle, fWidth, cCUDAFFT[fTransform], fBatch);
  end
  else if fDepth = 0 then
  begin
    status := cufftPlan2d(fHandle, fWidth, fHeight, cCUDAFFT[fTransform]);
  end
  else
  begin
    status := cufftPlan3d(fHandle, fWidth, fHeight, fDepth,
      cCUDAFFT[fTransform]);
  end;
  Context.Release;

  if status <> CUFFT_SUCCESS then
    raise ECUDAFFTPlan.Create('TCUDAFFTPlan.AllocateHandle: ' +
      GetCUFFTErrorString(status));
end;

procedure TCUDAFFTPlan.DestroyHandle;
var
  status: TcufftResult;
begin
  if FHandle > 0 then
  begin
    Context.Requires;
    status := cufftDestroy(fHandle);
    Context.Release;
    if status <> CUFFT_SUCCESS then
      raise ECUDAFFTPlan.Create('TCUDAFFTPlan.DestroyHandle: ' +
        GetCUFFTErrorString(status));
    FHandle := 0;
  end;
end;

procedure TCUDAFFTPlan.SetWidth(const Value: Integer);
begin
  Assert(Value > 0);
  if Value <> fWidth then
  begin
    fWidth := Value;
    DestroyHandle;
  end;
end;

procedure TCUDAFFTPlan.SetHeight(const Value: Integer);
begin
  Assert(Value >= 0);
  if Value <> fHeight then
  begin
    fHeight := Value;
    if fHeight > 0 then
      fBatch := 0;
    DestroyHandle;
  end;
end;

procedure TCUDAFFTPlan.SetDepth(const Value: Integer);
begin
  Assert(Value >= 0);
  if Value <> fDepth then
  begin
    fDepth := Value;
    if fDepth > 0 then
      fBatch := 0;
    DestroyHandle;
  end;
end;

procedure TCUDAFFTPlan.SetBatch(const Value: Integer);
begin
  Assert(Value >= 0);
  if Value <> fBatch then
  begin
    fBatch := Value;
    if fBatch > 0 then
    begin
      fHeight := 0;
      fDepth := 0;
    end;
    DestroyHandle;
  end;
end;

procedure TCUDAFFTPlan.SetTransform(const Value: TCUDAFFTransform);
begin
  if Value <> fTransform then
  begin
    fTransform := Value;
    DestroyHandle;
  end;
end;

procedure TCUDAFFTPlan.Execute(src: TCUDAMemData; dst: TCUDAMemData; const dir:
  TCUDAFFTdir);
const
  sFFTdir: array[TCUDAFFTdir] of Integer = (CUFFT_FORWARD, CUFFT_INVERSE);
var
  status: TcufftResult;
begin
  if FHandle = 0 then
    AllocateHandle;
  if not Assigned(src) or not Assigned(dst) then
    exit;
  Context.Requires;
  case fTransform of
    fftRealToComplex: status := cufftExecR2C(fHandle, src.Data, dst.Data);
    fftComplexToReal: status := cufftExecC2R(fHandle, src.Data, dst.Data);
    fftComplexToComplex: status := cufftExecC2C(fHandle, src.Data, dst.Data,
      sFFTdir[dir]);
    fftDoubleToDoubleComplex: status := cufftExecD2Z(fHandle, src.Data,
      dst.Data);
    fftDoubleComplexToDouble: status := cufftExecZ2D(fHandle, src.Data,
      dst.Data);
    fftDoubleComplexToDoubleComplex: status := cufftExecZ2Z(fHandle, src.Data,
      dst.Data, sFFTdir[dir]);
  else
    status := CUFFT_INVALID_VALUE;
  end;
  Context.Release;
  if status <> CUFFT_SUCCESS then
    raise ECUDAFFTPlan.Create('TCUDAFFTPlan.Execute: ' +
      GetCUFFTErrorString(status));
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClasses([TCUDAFFTPlan]);

end.

