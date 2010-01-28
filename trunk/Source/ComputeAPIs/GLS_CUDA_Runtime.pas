//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLS_CUDA_Runtime<p>

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

unit GLS_CUDA_Runtime;

interface

uses
  GLS_CL_Platform, OpenGL1x;

{$I cuda.inc}

const
  CUDARTDLL = 'cudart.dll';

  // single precision constants
  CUDART_INF_F: Single = $7F800000;
  CUDART_NAN_F: Single = $7FFFFFFF;
  CUDART_MIN_DENORM_F: Single = $00000001;
  CUDART_MAX_NORMAL_F: Single = $7F7FFFFF;
  CUDART_NEG_ZERO_F: Single = $80000000;
  CUDART_ZERO_F = 0.0;
  CUDART_ONE_F = 1.0;
  CUDART_SQRT_HALF_F = 0.707106781;
  CUDART_SQRT_TWO_F = 1.414213562;
  CUDART_THIRD_F = 0.333333333;
  CUDART_PIO4_F = 0.785398163;
  CUDART_PIO2_F = 1.570796327;
  CUDART_3PIO4_F = 2.356194490;
  CUDART_2_OVER_PI_F = 0.636619772;
  CUDART_PI_F = 3.141592654;
  CUDART_L2E_F = 1.442695041;
  CUDART_L2T_F = 3.321928094;
  CUDART_LG2_F = 0.301029996;
  CUDART_LGE_F = 0.434294482;
  CUDART_LN2_F = 0.693147181;
  CUDART_LNT_F = 2.302585093;
  CUDART_LNPI_F = 1.144729886;
  CUDART_TWO_TO_M126_F = 1.175494351e-38;
  CUDART_TWO_TO_126_F = 8.507059173e37;
  CUDART_NORM_HUGE_F = 3.402823466e38;
  CUDART_TWO_TO_23_F = 8388608.0;
  CUDART_TWO_TO_24_F = 16777216.0;
  CUDART_TWO_TO_31_F = 2147483648.0;
  CUDART_TWO_TO_32_F = 4294967296.0;
  CUDART_REMQUO_BITS_F = 3;
  CUDART_REMQUO_MASK_F = CUDART_REMQUO_BITS_F;
  CUDART_TRIG_PLOSS_F = 48039.0;

  // double precision constants */
{$IFNDEF CUDA_NO_SM_13_DOUBLE_INTRINSICS}
  CUDART_INF: Double = $7FF0000000000000;
  CUDART_NAN: Double = $FFF8000000000000;
  CUDART_NEG_ZERO: Double = $8000000000000000;
  CUDART_MIN_DENORM: Double = $0000000000000001;
{$ELSE} // not CUDA_NO_SM_13_DOUBLE_INTRINSICS
  CUDART_INF: Double = $7FF0000000000000;
  CUDART_NAN: Double = $FFF8000000000000;
  CUDART_NEG_ZERO: Double = $8000000000000000;
  CUDART_MIN_DENORM: Double = $0000000000000001;
{$ENDIF}
  CUDART_ZERO = 0.0;
  CUDART_ONE = 1.0;
  CUDART_SQRT_TWO = 1.4142135623730951e+0;
  CUDART_SQRT_HALF = 7.0710678118654757e-1;
  CUDART_THIRD = 3.3333333333333333e-1;
  CUDART_TWOTHIRD = 6.6666666666666667e-1;
  CUDART_PIO4 = 7.8539816339744828e-1;
  CUDART_PIO4_HI = 7.8539816339744828e-1;
  CUDART_PIO4_LO = 3.0616169978683830e-17;
  CUDART_PIO2 = 1.5707963267948966e+0;
  CUDART_PIO2_HI = 1.5707963267948966e+0;
  CUDART_PIO2_LO = 6.1232339957367660e-17;
  CUDART_3PIO4 = 2.3561944901923448e+0;
  CUDART_2_OVER_PI = 6.3661977236758138e-1;
  CUDART_PI = 3.1415926535897931e+0;
  CUDART_PI_HI = 3.1415926535897931e+0;
  CUDART_PI_LO = 1.2246467991473532e-16;
  CUDART_SQRT_2PI_HI = 2.5066282746310007e+0;
  CUDART_SQRT_2PI_LO = -1.8328579980459167e-16;
  CUDART_SQRT_PIO2_HI = 1.2533141373155003e+0;
  CUDART_SQRT_PIO2_LO = -9.1642899902295834e-17;
  CUDART_L2E = 1.4426950408889634e+0;
  CUDART_L2E_HI = 1.4426950408889634e+0;
  CUDART_L2E_LO = 2.0355273740931033e-17;
  CUDART_L2T = 3.3219280948873622e+0;
  CUDART_LG2 = 3.0102999566398120e-1;
  CUDART_LG2_HI = 3.0102999566398120e-1;
  CUDART_LG2_LO = -2.8037281277851704e-18;
  CUDART_LGE = 4.3429448190325182e-1;
  CUDART_LGE_HI = 4.3429448190325182e-1;
  CUDART_LGE_LO = 1.09831965021676510e-17;
  CUDART_LN2 = 6.9314718055994529e-1;
  CUDART_LN2_HI = 6.9314718055994529e-1;
  CUDART_LN2_LO = 2.3190468138462996e-17;
  CUDART_LNT = 2.3025850929940459e+0;
  CUDART_LNT_HI = 2.3025850929940459e+0;
  CUDART_LNT_LO = -2.1707562233822494e-16;
  CUDART_LNPI = 1.1447298858494002e+0;
  CUDART_LN2_X_1024 = 7.0978271289338397e+2;
  CUDART_LN2_X_1025 = 7.1047586007394398e+2;
  CUDART_LN2_X_1075 = 7.4513321910194122e+2;
  CUDART_LG2_X_1024 = 3.0825471555991675e+2;
  CUDART_LG2_X_1075 = 3.2360724533877976e+2;
  CUDART_TWO_TO_23 = 8388608.0;
  CUDART_TWO_TO_52 = 4503599627370496.0;
  CUDART_TWO_TO_54 = 18014398509481984.0;
  CUDART_TWO_TO_M54 = 5.5511151231257827e-17;
  CUDART_TWO_TO_M1022 = 2.22507385850720140e-308;
  CUDART_TRIG_PLOSS = 2147483648.0;

type
  TcudaError = (
    cudaSuccess,
    cudaErrorMissingConfiguration,
    cudaErrorMemoryAllocation,
    cudaErrorInitializationError,
    cudaErrorLaunchFailure,
    cudaErrorPriorLaunchFailure,
    cudaErrorLaunchTimeout,
    cudaErrorLaunchOutOfResources,
    cudaErrorInvalidDeviceFunction,
    cudaErrorInvalidConfiguration,
    cudaErrorInvalidDevice,
    cudaErrorInvalidValue,
    cudaErrorInvalidPitchValue,
    cudaErrorInvalidSymbol,
    cudaErrorMapBufferObjectFailed,
    cudaErrorUnmapBufferObjectFailed,
    cudaErrorInvalidHostPointer,
    cudaErrorInvalidDevicePointer,
    cudaErrorInvalidTexture,
    cudaErrorInvalidTextureBinding,
    cudaErrorInvalidChannelDescriptor,
    cudaErrorInvalidMemcpyDirection,
    cudaErrorAddressOfConstant,
    cudaErrorTextureFetchFailed,
    cudaErrorTextureNotBound,
    cudaErrorSynchronizationError,
    cudaErrorInvalidFilterSetting,
    cudaErrorInvalidNormSetting,
    cudaErrorMixedDeviceExecution,
    cudaErrorCudartUnloading,
    cudaErrorUnknown,
    cudaErrorNotYetImplemented,
    cudaErrorMemoryValueTooLarge,
    cudaErrorInvalidResourceHandle,
    cudaErrorNotReady,
    cudaErrorStartupFailure,
    cudaErrorApiFailureBase);

  {+//DEVICE_BUILTIN*/ }
  TCudaChannelFormatKind = (
    cudaChannelFormatKindSigned,
    cudaChannelFormatKindUnsigned,
    cudaChannelFormatKindFloat);

  TCudaGLMapFlags = (
    cudaGLMapFlagsNone, ///< Default; Assume resource can be read/written
    cudaGLMapFlagsReadOnly, ///< CUDA kernels will not write to this resource
    cudaGLMapFlagsWriteDiscard);
      ///< CUDA kernels will only write to and will not read from this resource

  {+//DEVICE_BUILTIN*/ }
  PcudaChannelFormatDesc = ^TCudaChannelFormatDesc;
  TCudaChannelFormatDesc = record
    x: Integer;
    y: Integer;
    z: Integer;
    w: Integer;
    f: TCudaChannelFormatKind;
  end {cudaChannelFormatDesc};

  {+//DEVICE_BUILTIN*/ }
  cudaArray = record
  end; //!ATTENTION foreward Declaration?)

  {+//DEVICE_BUILTIN*/ }
  TcudaMemcpyKind = (
    cudaMemcpyHostToHost {= 0},
    cudaMemcpyHostToDevice,
    cudaMemcpyDeviceToHost,
    cudaMemcpyDeviceToDevice);

  {+//DEVICE_BUILTIN*/ }
  TcudaPitchedPtr = record
    ptr: Pointer;
    pitch: size_t;
    xsize: size_t;
    ysize: size_t;
  end {cudaPitchedPtr};

  {+//DEVICE_BUILTIN*/ }
  TcudaExtent = record
    width: size_t;
    height: size_t;
    depth: size_t;
  end {cudaExtent};

  {+//DEVICE_BUILTIN*/ }
  TcudaPos = record
    x: size_t;
    y: size_t;
    z: size_t;
  end {cudaPos};

  {+//DEVICE_BUILTIN*/ }
  TcudaMemcpy3DParms = record
    srcArray: Pointer;
    srcPos: TcudaPos;
    srcPtr: TcudaPitchedPtr;
    dstArray: Pointer;
    dstPos: TcudaPos;
    dstPtr: TcudaPitchedPtr;
    extent: TcudaExtent;
    kind: TcudaMemcpyKind;
  end {cudaMemcpy3DParms};

  {+//DEVICE_BUILTIN*/ }
  PCudaDeviceProp = ^TCudaDeviceProp;
  TCudaDeviceProp = record
    name: array[0..256 - 1] of AnsiChar;
    totalGlobalMem: size_t;
    sharedMemPerBlock: size_t;
    regsPerBlock: Integer;
    warpSize: Integer;
    memPitch: size_t;
    maxThreadsPerBlock: Integer;
    maxThreadsDim: array[0..3 - 1] of Integer;
    maxGridSize: array[0..3 - 1] of Integer;
    clockRate: Integer;
    totalConstMem: size_t;
    major: Integer;
    minor: Integer;
    textureAlignment: size_t;
    deviceOverlap: Integer;
    multiProcessorCount: Integer;
    __cudaReserved: array[0..40 - 1] of Integer;
  end {cudaDeviceProp};

  TcudaTextureAddressMode = (cudaAddressModeWrap, cudaAddressModeClamp);

  TcudaTextureFilterMode = (cudaFilterModePoint, cudaFilterModeLinear);

  TcudaTextureReadMode = (cudaReadModeElementType, cudaReadModeNormalizedFloat);

  PTextureReference = ^TTextureReference;
  TTextureReference = record
    normalized: Integer;
    filterMode: TcudaTextureFilterMode;
    addressMode: array[0..2] of TcudaTextureAddressMode;
    channelDesc: TcudaChannelFormatDesc;
    __cudaReserved: array[0..15] of Integer;
  end;

  PcudaArray = Pointer;

  {+//****************************************************************************** }
  {-** }
  {-* SHORTHAND TYPE DEFINITION USED BY RUNTIME API* }
  {-** }
  {=*******************************************************************************/ }
  {+//DEVICE_BUILTIN*/ }
  cudaError_t = TcudaError;
  {+//DEVICE_BUILTIN*/ }
  cudaStream_t = Integer;
  {+//DEVICE_BUILTIN*/ }
  cudaEvent_t = Integer;

  {+//****************************************************************************** }
  {-** }
  {-** }
  {-** }
  {=****************************************************************************** }

function cudaBindTexture(var offset: size_t; const texref: PTextureReference; var
  devPtr: Pointer; var desc: TcudaChannelFormatDesc; size: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;
function cudaBindTexture2D(var offset: size_t; const texref: PTextureReference;
  const devPtr: Pointer; var desc: TcudaChannelFormatDesc; width, height, pitch:
  size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;
function cudaBindTextureToArray(const texref: PTextureReference; const
  cudaArray: PcudaArray): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;
function cudaUnbindTexture(const texref: PTextureReference): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;
function cudaGetTextureAlignmentOffset(offset: size_t; const texref:
  PTextureReference): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;
function cudaGetTextureReference(const texref: PTextureReference; const symbol:
  PAnsiChar): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaCreateChannelDesc(x, y, z, w: Integer; f: TCudaChannelFormatKind):
  TCudaChannelFormatDesc;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMalloc(var devPtr;
  size: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMallocHost(var ptr: Pointer;
  size: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMallocPitch(var devPtr;
  var pitch: size_t;
  width: size_t;
  height: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMallocArray(var aarray: Pointer;
  var desc: TCudaChannelFormatDesc;
  width: size_t;
  height: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaFree(devPtr: Pointer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaFreeHost(ptr: Pointer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaFreeArray(const aarray: Pointer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpy(dst: Pointer; src: Pointer; count: size_t;
  kind: TcudaMemcpyKind): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpyToArray(var dst: PcudaArray; wOffset: size_t; hOffset:
  size_t; var src; count: size_t; kind: TcudaMemcpyKind): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpyFromArray(var dst; const src: PcudaArray; wOffset: size_t;
  hOffset: size_t; count: size_t; kind: TcudaMemcpyKind): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpyArrayToArray(dst: PcudaArray; wOffsetDst: size_t; hOffsetDst:
  size_t; const src: PcudaArray; wOffsetSrc: size_t; hOffsetSrc: size_t; count:
  size_t; const kind: TcudaMemcpyKind = cudaMemcpyDeviceToDevice): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpy2D(var dst; dpitch: size_t; var src; spitch: size_t; width:
  size_t; height: size_t; kind: TcudaMemcpyKind): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpy2DToArray(dst: PcudaArray; wOffset: size_t; hOffset: size_t;
  var src; spitch: size_t; width: size_t; height: size_t; kind: TcudaMemcpyKind):
  cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpy2DFromArray(var dst; dpitch: size_t; src: PcudaArray;
  wOffset: size_t; hOffset: size_t; width: size_t; height: size_t; kind:
  TcudaMemcpyKind): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpy2DArrayToArray(dst: PcudaArray; wOffsetDst: size_t;
  hOffsetDst: size_t; src: PcudaArray; wOffsetSrc: size_t; hOffsetSrc: size_t;
  width: size_t; height: size_t; const kind: TcudaMemcpyKind =
  cudaMemcpyDeviceToDevice): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpyToSymbol(symbol: PAnsiChar; var src; count: size_t; const
  offset: size_t = 0; const kind: TcudaMemcpyKind = cudaMemcpyHostToDevice):
  cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpyFromSymbol(var dst; symbol: PAnsiChar; count: size_t; const
  offset: size_t = 0; const kind: TcudaMemcpyKind = cudaMemcpyDeviceToHost):
  cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

{+//***************************************************************************}
{-** }
{-** }
{-** }
{=*****************************************************************************}

function cudaMemcpyAsync(var dst; const src; count: size_t; kind:
  TcudaMemcpyKind; stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpyToArrayAsync(dst: PcudaArray; wOffset: size_t; hOffset:
  size_t; const src; count: size_t; kind: TcudaMemcpyKind; stream: cudaStream_t):
  cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpyFromArrayAsync(var dst; const src: PcudaArray; wOffset:
  size_t; hOffset: size_t; count: size_t; kind: TcudaMemcpyKind; stream:
  cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpy2DAsync(var dst; dpitch: size_t; const src; spitch: size_t;
  width: size_t; height: size_t; kind: TcudaMemcpyKind; stream: cudaStream_t):
  cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpy2DToArrayAsync(dst: PcudaArray; wOffset: size_t; hOffset:
  size_t; const src; spitch: size_t; width: size_t; height: size_t; kind:
  TcudaMemcpyKind; stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpy2DFromArrayAsync(var dst; dpitch: size_t; const src:
  PcudaArray; wOffset: size_t; hOffset: size_t; width: size_t; height: size_t;
  kind: TcudaMemcpyKind; stream: cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpyToSymbolAsync(const symbol: PAnsiChar; const src; count:
  size_t; offset: size_t; kind: TcudaMemcpyKind; stream: cudaStream_t):
  cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemcpyFromSymbolAsync(var dst; const symbol: PAnsiChar; count:
  size_t; offset: size_t; kind: TcudaMemcpyKind; stream: cudaStream_t):
  cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

///*****************************************************************************
//*                                                                            *
//*                                                                            *
//*                                                                            *
//*****************************************************************************/

function cudaMemset(var devPtr; value: Integer; count: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaMemset2D(var devPtr; pitch: size_t; value: Integer; width: size_t;
  height: size_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

///*****************************************************************************
//*                                                                            *
//*                                                                            *
//*                                                                            *
//*****************************************************************************/

function cudaGetSymbolAddress(var devPtr: Pointer; const symbol: PAnsiChar):
  cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaGetSymbolSize(var size: size_t; const symbol: PAnsiChar):
  cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

{+//***************************************************************************}
{-** }
{-** }
{-** }
{=*****************************************************************************}

function cudaGetDeviceCount(var count: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaGetDeviceProperties(var prop: TCudaDeviceProp;
  device: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaChooseDevice(var device: Integer;
  const prop: PcudaDeviceProp): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaSetDevice(device: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaGetDevice(var device: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

{+//****************************************************************************** }
{-** }
{-** }
{-** }
{=*******************************************************************************/ }

function cudaGetLastError: cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

{+//****************************************************************************** }
{-** }
{-** }
{-** }
{=*******************************************************************************/ }
function cudaGLSetGLDevice(device: Integer): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaGLRegisterBufferObject(bufObj: GLuint): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaGLMapBufferObject(devPtr: Pointer; bufObj: GLuint): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaGLUnmapBufferObject(bufObj: GLuint): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaGLUnregisterBufferObject(bufObj: GLuint): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaGLSetBufferObjectMapFlags(bufObj: GLuint; flags: TCudaGLMapFlags):
  cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaGLMapBufferObjectAsync(var devPtr: Pointer; bufObj: GLuint; stream:
  cudaStream_t): cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaGLUnmapBufferObjectAsync(bufObj: GLuint; stream: cudaStream_t):
  cudaError_t;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaGetErrorString(error: cudaError_t): PAnsiChar;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}external CUDARTDLL;

function cudaGetLastErrorString: string;

implementation

function cudaGetLastErrorString: string;
begin
  Result := string(cudaGetErrorString(cudaGetLastError));
end;

end.

