//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLS_CUDA_FastFourierTransformation<p>

   <b>History : </b><font size=-1><ul>
      <li>28/01/10 - Yar - Creation
   </ul></font>
}

///*
// * Copyright 1993-2009 NVIDIA Corporation.  All rights reserved.
// *
// * NOTICE TO USER:
// *
// * This source code is subject to NVIDIA ownership rights under U.S. and
// * international Copyright laws.  Users and possessors of this source code
// * are hereby granted a nonexclusive, royalty-free license to use this code
// * in individual and commercial software.
// *
// * NVIDIA MAKES NO REPRESENTATION ABOUT THE SUITABILITY OF THIS SOURCE
// * CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR
// * IMPLIED WARRANTY OF ANY KIND.  NVIDIA DISCLAIMS ALL WARRANTIES WITH
// * REGARD TO THIS SOURCE CODE, INCLUDING ALL IMPLIED WARRANTIES OF
// * MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR PURPOSE.
// * IN NO EVENT SHALL NVIDIA BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL,
// * OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
// * OF USE, DATA OR PROFITS,  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
// * OR OTHER TORTIOUS ACTION,  ARISING OUT OF OR IN CONNECTION WITH THE USE
// * OR PERFORMANCE OF THIS SOURCE CODE.
// *
// * U.S. Government End Users.   This source code is a "commercial item" as
// * that term is defined at  48 C.F.R. 2.101 (OCT 1995), consisting  of
// * "commercial computer  software"  and "commercial computer software
// * documentation" as such terms are  used in 48 C.F.R. 12.212 (SEPT 1995)
// * and is provided to the U.S. Government only as a commercial end item.
// * Consistent with 48 C.F.R.12.212 and 48 C.F.R. 227.7202-1 through
// * 227.7202-4 (JUNE 1995), all U.S. Government End Users acquire the
// * source code with only those rights set forth herein.
// *
// * Any use of this source code in individual and commercial software must
// * include, in the user documentation and internal comments to the code,
// * the above Disclaimer and U.S. Government End Users Notice.
// */

unit GLS_CUDA_FastFourierTransformation;

interface

uses
{$IFDEF MSWINDOWS}  Windows, {$ENDIF}
  VectorTypes;

{$I cuda.inc}

const
  CUFFTDLL = 'cufft.dll';

  {/// CUFFT API function return values }

type
  {/// CUFFT defines and supports the following data types }

  {/// cufftHandle is a handle type used to store and access CUFFT plans. }
  TcufftHandle = Cardinal;

  TcufftReal = Single;
  PcufftReal = ^TcufftReal;

  TcufftRealfloat = Single;
  PcufftDoubleReal = ^TcufftDoubleReal;
  TcufftDoubleReal = Double;

  PcufftDoubleComplex = ^TcufftDoubleComplex;
  TcufftDoubleComplex = TVector2d;

  PcufftComplex = ^TcufftComplex;
  TcufftComplex = TVector2f;

  TcufftResult = (
    CUFFT_SUCCESS {= 0x0},
    CUFFT_INVALID_PLAN {= 0x1},
    CUFFT_ALLOC_FAILED {= 0x2},
    CUFFT_INVALID_TYPE {= 0x3},
    CUFFT_INVALID_VALUE {= 0x4},
    CUFFT_INTERNAL_ERROR {= 0x5},
    CUFFT_EXEC_FAILED {= 0x6},
    CUFFT_SETUP_FAILED {= 0x7},
    CUFFT_INVALID_SIZE {= 0x8 });

  TcufftType = Cardinal;

  TcudaRoundMode = (
    cudaRoundNearest,
    cudaRoundZero,
    cudaRoundPosInf,
    cudaRoundMinInf
  );

  {/// CUFFT transform directions }
const
  CUFFT_FORWARD = -1; {// Forward FFT}
  CUFFT_INVERSE = 1; {// Inverse FFT}

  {/// CUFFT supports the following transform types }
  CUFFT_R2C = $2a;     // Real to Complex (interleaved)
  CUFFT_C2R = $2c;     // Complex (interleaved) to Real
  CUFFT_C2C = $29;     // Complex to Complex, interleaved
  CUFFT_D2Z = $6a;     // Double to Double-Complex
  CUFFT_Z2D = $6c;     // Double-Complex to Double
  CUFFT_Z2Z = $69;     // Double-Complex to Double-Complex

var

  cufftPlan1d: function(var plan: TcufftHandle;
    nx: Integer;
    atype: Byte;
    batch: Integer): TcufftResult;
  {$IFDEF CUDA_STDCALL}stdcall;
  {$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
  {$ENDIF}

  cufftPlan2d: function(var plan: TcufftHandle;
    nx: Integer;
    ny: Integer;
    atype: TcufftType): TcufftResult;
  {$IFDEF CUDA_STDCALL}stdcall;
  {$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
  {$ENDIF}

  cufftPlan3d: function(var plan: TcufftHandle;
    nx: Integer;
    ny: Integer;
    nz: Integer;
    atype: TcufftType): TcufftResult;
  {$IFDEF CUDA_STDCALL}stdcall;
  {$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
  {$ENDIF}

  cufftDestroy: function(plan: TcufftHandle): TcufftResult;
  {$IFDEF CUDA_STDCALL}stdcall;
  {$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
  {$ENDIF}

  cufftPlanMany: function(var plan: TcufftHandle;
                         rank: Integer;
                         var n: Integer;
                         var inembed: Integer;
                         istride, idist: Integer;    // Unused: pass "NULL, 1, 0"
                         var onembed: Integer;
                         ostride, odist: Integer;    // Unused: pass "NULL, 1, 0"
                         ctype: TcufftType;
                         batch: Integer): TcufftResult;
  {$IFDEF CUDA_STDCALL}stdcall;
  {$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
  {$ENDIF}

  cufftExecC2C: function(plan: TcufftHandle;
    idata: PcufftComplex;
    odata: PcufftComplex;
    direction: Integer): TcufftResult;
  {$IFDEF CUDA_STDCALL}stdcall;
  {$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
  {$ENDIF}

  cufftExecR2C: function(plan: TcufftHandle;
    idata: PcufftReal;
    odata: PcufftComplex ): TcufftResult;
  {$IFDEF CUDA_STDCALL}stdcall;
  {$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
  {$ENDIF}

  cufftExecC2R: function(plan: TcufftHandle;
    idata: PcufftComplex;
    odata: PcufftReal): TcufftResult;
  {$IFDEF CUDA_STDCALL}stdcall;
  {$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
  {$ENDIF}

  cufftExecZ2Z: function(plan: TcufftHandle;
                                    idata: PcufftDoubleComplex;
                                    odata: PcufftDoubleComplex;
                                    direction: Integer): TcufftResult;
  {$IFDEF CUDA_STDCALL}stdcall;
  {$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
  {$ENDIF}

  cufftExecD2Z: function(plan: TcufftHandle;
                               idata: PcufftDoubleReal;
                               odata: PcufftDoubleComplex): TcufftResult;
  {$IFDEF CUDA_STDCALL}stdcall;
  {$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
  {$ENDIF}

  cufftExecZ2D: function(plan: TcufftHandle;
                              idata: PcufftDoubleComplex;
                              odata: PcufftDoubleReal): TcufftResult;
  {$IFDEF CUDA_STDCALL}stdcall;
  {$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
  {$ENDIF}

  cufftSetStream: function(p: TcufftHandle; stream: Integer): TcufftResult;
  {$IFDEF CUDA_STDCALL}stdcall;
  {$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
  {$ENDIF}

function InitCUFFT: Boolean;
procedure CloseCUFFT;
function InitCUFFTFromLibrary(const LibName: WideString): Boolean;
function IsCUFFTInitialized: Boolean;
function GetCUFFTErrorString(error: TcufftResult): string;

implementation

const
  cufftResultStrings: array[TcufftResult] of string = (
    'success',
    'invalid plan',
    'alloc failed',
    'invalid type',
    'invalid value',
    'internal error',
    'exec failed',
    'setup failed',
    'invalid size'
    );

const
  INVALID_MODULEHANDLE = 0;

  // ************** Windows specific ********************
{$IFDEF MSWINDOWS}
var
  CUFFTHandle: HINST = INVALID_MODULEHANDLE;
{$ENDIF}
  // ************** UNIX specific ********************
{$IFDEF UNIX}
var
  CUFFTHandle: TLibHandle = INVALID_MODULEHANDLE;
{$ENDIF}

function CUFFTGetProcAddress(ProcName: PAnsiChar): Pointer;
begin
  result := GetProcAddress(Cardinal(CUFFTHandle), ProcName);
end;

function InitCUFFT: Boolean;
begin
  if CUFFTHandle = INVALID_MODULEHANDLE then
    Result := InitCUFFTFromLibrary(CUFFTDLL)
  else
    Result := True;
end;

procedure CloseCUFFT;
begin
  if CUFFTHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(Cardinal(CUFFTHandle));
    CUFFTHandle := INVALID_MODULEHANDLE;
  end;
end;

function InitCUFFTFromLibrary(const LibName: WideString): Boolean;
begin
  Result := False;
  CloseCUFFT;
  CUFFTHandle := LoadLibraryW(PWideChar(LibName));
  if CUFFTHandle = INVALID_MODULEHANDLE then
    Exit;
  cufftPlan1d := CUFFTGetProcAddress('cufftPlan1d');
  cufftPlan2d := CUFFTGetProcAddress('cufftPlan2d');
  cufftPlan3d := CUFFTGetProcAddress('cufftPlan3d');
  cufftDestroy := CUFFTGetProcAddress('cufftDestroy');
  cufftPlanMany := CUFFTGetProcAddress('cufftPlanMany');
  cufftExecC2C := CUFFTGetProcAddress('cufftExecC2C');
  cufftExecR2C := CUFFTGetProcAddress('cufftExecR2C');
  cufftExecC2R := CUFFTGetProcAddress('cufftExecC2R');
  cufftExecZ2Z := CUFFTGetProcAddress('cufftExecZ2Z');
  cufftExecD2Z := CUFFTGetProcAddress('cufftExecD2Z');
  cufftExecZ2D := CUFFTGetProcAddress('cufftExecZ2D');
  cufftSetStream := CUFFTGetProcAddress('cufftSetStream');
end;

function IsCUFFTInitialized: Boolean;
begin
  Result := (CUFFTHandle <> INVALID_MODULEHANDLE);
end;

function GetCUFFTErrorString(error: TcufftResult): string;
begin
  Result := cufftResultStrings[error];
end;

initialization

finalization

  CloseCUFFT;

end.

