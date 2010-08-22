//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLS_CUDA_API<p>

   <b>History : </b><font size=-1><ul>
      <li>08/04/10 - Yar - Corrected parameters of cuMemcpyDtoH, cuMemcpyAtoH
      <li>02/03/10 - Yar - Added missing constants, correct parameters of some functions
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

unit GLS_CUDA_API;

interface

uses
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
  GLS_CL_Platform;

{$I cuda.inc}

const
  CUDAAPIDLL = 'nvcuda.dll';

type

  TCUdeviceptr = Pointer; ///< CUDA device pointer

  TCUdevice = Integer; ///< CUDA device

  PCUcontext = ^TCUcontext;
  TCUcontext = record
  end; ///< CUDA context
  PCUmodule = ^TCUmodule;
  TCUmodule = record
  end; ///< CUDA module
  PCUfunction = ^TCUfunction;
  TCUfunction = record
  end; ///< CUDA function
  PCUarray = ^TCUarray;
  TCUarray = record
  end; ///< CUDA array
  PCUtexref = ^TCUtexref;
  TCUtexref = record
  end; ///< CUDA texture reference
  PCUevent = ^TCUevent;
  TCUevent = record
  end; ///< CUDA event
  PCUstream = ^TCUstream;
  TCUstream = record
  end; ///< CUDA stream
  PPCUgraphicsResource = ^PCUgraphicsResource;
  PCUgraphicsResource = ^TCUgraphicsResource;
  TCUgraphicsResource = record
  end; ///< CUDA graphics interop resource

  // Context creation flags

  TCUctx_flags = (
    CU_CTX_SCHED_AUTO = 0, ///< Automatic scheduling
    CU_CTX_SCHED_SPIN = 1, ///< Set spin as default scheduling
    CU_CTX_SCHED_YIELD = 2, ///< Set yield as default scheduling
    CU_CTX_SCHED_MASK = 3,
    CU_CTX_BLOCKING_SYNC = 4, ///< Use blocking synchronization
    CU_CTX_MAP_HOST = 8, ///< Support mapped pinned allocations
    CU_CTX_FLAGS_MASK = 15
    );

  // Event creation flags

  TCUevent_flags = (
    CU_EVENT_DEFAULT = 0, ///< Default event flag
    CU_EVENT_BLOCKING_SYNC = 1 ///< Event uses blocking synchronization
    );

  // Array formats

  TCUarray_format = (
    CU_AD_FORMAT_UNSIGNED_INT8 = $01, ///< Unsigned 8-bit integers
    CU_AD_FORMAT_UNSIGNED_INT16 = $02, ///< Unsigned 16-bit integers
    CU_AD_FORMAT_UNSIGNED_INT32 = $03, ///< Unsigned 32-bit integers
    CU_AD_FORMAT_SIGNED_INT8 = $08, ///< Signed 8-bit integers
    CU_AD_FORMAT_SIGNED_INT16 = $09, ///< Signed 16-bit integers
    CU_AD_FORMAT_SIGNED_INT32 = $0A, ///< Signed 32-bit integers
    CU_AD_FORMAT_HALF = $10, ///< 16-bit floating point
    CU_AD_FORMAT_FLOAT = $20 ///< 32-bit floating point
    );

  // Texture reference addressing modes

  TCUaddress_mode = (
    CU_TR_ADDRESS_MODE_WRAP = 0, ///< Wrapping address mode
    CU_TR_ADDRESS_MODE_CLAMP = 1, ///< Clamp to edge address mode
    CU_TR_ADDRESS_MODE_MIRROR = 2 ///< Mirror address mode
    );

  // Texture reference filtering modes

  TCUfilter_mode = (
    CU_TR_FILTER_MODE_POINT = 0, ///< Point filter mode
    CU_TR_FILTER_MODE_LINEAR = 1 ///< Linear filter mode
    );

  // Device properties

  TCUdevice_attribute = (
    CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK = 1,
    ///< Maximum number of threads per block
    CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X = 2, ///< Maximum block dimension X
    CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y = 3, ///< Maximum block dimension Y
    CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z = 4, ///< Maximum block dimension Z
    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X = 5, ///< Maximum grid dimension X
    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y = 6, ///< Maximum grid dimension Y
    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z = 7, ///< Maximum grid dimension Z
    CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK = 8,
    ///< Maximum shared memory available per block in bytes
    CU_DEVICE_ATTRIBUTE_SHARED_MEMORY_PER_BLOCK = 8,
    ///< Deprecated, use CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK
    CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY = 9,
    ///< Memory available on device for __constant__ variables in a CUDA C kernel in bytes
    CU_DEVICE_ATTRIBUTE_WARP_SIZE = 10, ///< Warp size in threads
    CU_DEVICE_ATTRIBUTE_MAX_PITCH = 11,
    ///< Maximum pitch in bytes allowed by memory copies
    CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK = 12,
    ///< Maximum number of 32-bit registers available per block
    CU_DEVICE_ATTRIBUTE_REGISTERS_PER_BLOCK = 12,
    ///< Deprecated, use CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK
    CU_DEVICE_ATTRIBUTE_CLOCK_RATE = 13, ///< Peak clock frequency in kilohertz
    CU_DEVICE_ATTRIBUTE_TEXTURE_ALIGNMENT = 14,
    ///< Alignment requirement for textures

    CU_DEVICE_ATTRIBUTE_GPU_OVERLAP = 15,
    ///< Device can possibly copy memory and execute a kernel concurrently
    CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT = 16,
    ///< Number of multiprocessors on device
    CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT = 17,
    ///< Specifies whether there is a run time limit on kernels
    CU_DEVICE_ATTRIBUTE_INTEGRATED = 18,
    ///< Device is integrated with host memory
    CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY = 19,
    ///< Device can map host memory into CUDA address space
    CU_DEVICE_ATTRIBUTE_COMPUTE_MODE = 20
    ///< Compute mode (See ::CUcomputemode for details)
    );


(**
 * CUDA Limits
 *)
  TcudaLimit = (
    cudaLimitStackSize      = $00, ///< GPU thread stack size
    cudaLimitPrintfFifoSize = $01  ///< GPU printf FIFO size
  );

  // Legacy device properties

  TCUdevprop = record
    maxThreadsPerBlock: Integer; ///< Maximum number of threads per block
    maxThreadsDim: array[0..2] of Integer;
    ///< Maximum size of each dimension of a block
    maxGridSize: array[0..2] of Integer;
    ///< Maximum size of each dimension of a grid
    sharedMemPerBlock: Integer; ///< Shared memory available per block in bytes
    totalConstantMemory: Integer;
    ///< Constant memory available on device in bytes
    SIMDWidth: Integer; ///< Warp size in threads
    memPitch: Integer; ///< Maximum pitch in bytes allowed by memory copies
    regsPerBlock: Integer; ///< 32-bit registers available per block
    clockRate: Integer; ///< Clock frequency in kilohertz
    textureAlign: Integer; ///< Alignment requirement for textures
  end;

  // Function properties

  TCUfunction_attribute = (

    {    * The number of threads beyond which a launch of the function would fail.
         * This number depends on both the function and the device on which the
         * function is currently loaded. }

    CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK = 0,

    {    * The size in bytes of statically-allocated shared memory required by
         * this function. This does not include dynamically-allocated shared
         * memory requested by the user at runtime.     }

    CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES = 1,

    {    * The size in bytes of user-allocated constant memory required by this
         * function.     }

    CU_FUNC_ATTRIBUTE_CONST_SIZE_BYTES = 2,

    {     * The size in bytes of thread local memory used by this function.}

    CU_FUNC_ATTRIBUTE_LOCAL_SIZE_BYTES = 3,

    {     * The number of registers used by each thread of this function.}

    CU_FUNC_ATTRIBUTE_NUM_REGS = 4,

    CU_FUNC_ATTRIBUTE_MAX
    );

  // Memory types

  TCUmemorytype = (
    CU_MEMORYTYPE_HOST = $01, ///< Host memory
    CU_MEMORYTYPE_DEVICE = $02, ///< Device memory
    CU_MEMORYTYPE_ARRAY = $03 ///< Array memory
    );

  // Compute Modes

  TCUcomputemode = (
    CU_COMPUTEMODE_DEFAULT = 0,
    ///< Default compute mode (Multiple contexts allowed per device)
    CU_COMPUTEMODE_EXCLUSIVE = 1,
    ///< Compute-exclusive mode (Only one context can be present on this device at a time)
    CU_COMPUTEMODE_PROHIBITED = 2
    ///< Compute-prohibited mode (No contexts can be created on this device at this time)
    );

  // Online compiler options

  TCUjit_option = (
    {     * Max number of registers that a thread may use.}

    CU_JIT_MAX_REGISTERS = 0,

    {    * IN: Specifies minimum number of threads per block to target compilation
         * for\n
         * OUT: Returns the number of threads the compiler actually targeted.
         * This restricts the resource utilization fo the compiler (e.g. max
         * registers) such that a block with the given number of threads should be
         * able to launch based on register limitations. Note, this option does not
         * currently take into account any other resource limitations, such as
         * shared memory utilization.  }

    CU_JIT_THREADS_PER_BLOCK,

    {    * Returns a float value in the option of the wall clock time, in
         * milliseconds, spent creating the cubin}

    CU_JIT_WALL_TIME,

    {     * Pointer to a buffer in which to print any log messsages from PTXAS
         * that are informational in nature  }

    CU_JIT_INFO_LOG_BUFFER,

    {    * IN: Log buffer size in bytes.  Log messages will be capped at this size
         * (including null terminator)\n
         * OUT: Amount of log buffer filled with messages   }

    CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES,

    {    * Pointer to a buffer in which to print any log messages from PTXAS that
         * reflect errors }

    CU_JIT_ERROR_LOG_BUFFER,

    {    * IN: Log buffer size in bytes.  Log messages will be capped at this size
         * (including null terminator)\n
         * OUT: Amount of log buffer filled with messages  }

    CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES,

    {    * Level of optimizations to apply to generated code (0 - 4), with 4
         * being the default and highest level of optimizations.  }

    CU_JIT_OPTIMIZATION_LEVEL,

    {    * No option value required. Determines the target based on the current
         * attached context (default)    }

    CU_JIT_TARGET_FROM_CUCONTEXT,

    {    * Target is chosen based on supplied CUjit_target_enum.   }

    CU_JIT_TARGET,

    {    * Specifies choice of fallback strategy if matching cubin is not found.
         * Choice is based on supplied CUjit_fallback_enum. }

    CU_JIT_FALLBACK_STRATEGY

    );

  // Online compilation targets

  TCUjit_target = (

    CU_TARGET_COMPUTE_10 = 0, ///< Compute device class 1.0
    CU_TARGET_COMPUTE_11, ///< Compute device class 1.1
    CU_TARGET_COMPUTE_12, ///< Compute device class 1.2
    CU_TARGET_COMPUTE_13 ///< Compute device class 1.3
    );

  // Cubin matching fallback strategies

  TCUjit_fallback = (
    //** Prefer to compile ptx */
    CU_PREFER_PTX = 0,
    //** Prefer to fall back to compatible binary code */
    CU_PREFER_BINARY
    );

  // Flags to register a graphics resource

  TCUgraphicsRegisterFlags = (
    CU_GRAPHICS_REGISTER_FLAGS_NONE = $00000000);

  // Flags for mapping and unmapping interop resources

  TCUgraphicsMapResourceFlags = (
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_NONE          = $00000000,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_READ_ONLY     = $00000001,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_WRITE_DISCARD = $00000002);

  // Array indices for cube faces

  TCUarray_cubemap_face = (
    CU_CUBEMAP_FACE_POSITIVE_X  = $00000000, ///< Positive X face of cubemap
    CU_CUBEMAP_FACE_NEGATIVE_X  = $00000001, ///< Negative X face of cubemap
    CU_CUBEMAP_FACE_POSITIVE_Y  = $00000002, ///< Positive Y face of cubemap
    CU_CUBEMAP_FACE_NEGATIVE_Y  = $00000003, ///< Negative Y face of cubemap
    CU_CUBEMAP_FACE_POSITIVE_Z  = $00000004, ///< Positive Z face of cubemap
    CU_CUBEMAP_FACE_NEGATIVE_Z  = $00000005  ///< Negative Z face of cubemap
  );

 (*
 * CUDA function attributes
 *)

  TcudaFuncAttributes = record
    sharedSizeBytes: size_t;  ///< Size of shared memory in bytes
    constSizeBytes: size_t;   ///< Size of constant memory in bytes
    localSizeBytes: size_t;   ///< Size of local memory in bytes
    maxThreadsPerBlock: Integer;  ///< Maximum number of threads per block
    numRegs: Integer;             ///< Number of registers used
    (* \brief PTX virtual architecture version for which the function was
    *  compiled. This value is the major PTX version * 10 + the minor PTX
    *  version, so a PTX version 1.3 function would return the value 13.
    *  For device emulation kernels, this is set to 9999.
    *)
    ptxVersion: Integer;
    (** \brief Binary architecture version for which the function was compiled.
    *  This value is the major binary version * 10 + the minor binary version,
    *  so a binary version 1.3 function would return the value 13.
    *  For device emulation kernels, this is set to 9999.
    *)
    binaryVersion: Integer;
    __cudaReserved: array[0..5] of Integer;
  end;

  (**
   * CUDA function cache configurations
   *)

  TcudaFuncCache =
  (
    cudaFuncCachePreferNone   = 0,    ///< Default function cache configuration, no preference
    cudaFuncCachePreferShared = 1,    ///< Prefer larger shared memory and smaller L1 cache
    cudaFuncCachePreferL1     = 2     ///< Prefer larger L1 cache and smaller shared memory
  );

  //************************************
  // **
  // **    Error codes
  // **
  // ***********************************/

  // Error codes

  TCUresult = (

    CUDA_SUCCESS = 0, ///< No errors
    CUDA_ERROR_INVALID_VALUE = 1, ///< Invalid value
    CUDA_ERROR_OUT_OF_MEMORY = 2, ///< Out of memory
    CUDA_ERROR_NOT_INITIALIZED = 3, ///< Driver not initialized
    CUDA_ERROR_DEINITIALIZED = 4, ///< Driver deinitialized

    CUDA_ERROR_NO_DEVICE = 100, ///< No CUDA-capable device available
    CUDA_ERROR_INVALID_DEVICE = 101, ///< Invalid device

    CUDA_ERROR_INVALID_IMAGE = 200, ///< Invalid kernel image
    CUDA_ERROR_INVALID_CONTEXT = 201, ///< Invalid context
    CUDA_ERROR_CONTEXT_ALREADY_CURRENT = 202, ///< Context already current
    CUDA_ERROR_MAP_FAILED = 205, ///< Map failed
    CUDA_ERROR_UNMAP_FAILED = 206, ///< Unmap failed
    CUDA_ERROR_ARRAY_IS_MAPPED = 207, ///< Array is mapped
    CUDA_ERROR_ALREADY_MAPPED = 208, ///< Already mapped
    CUDA_ERROR_NO_BINARY_FOR_GPU = 209, ///< No binary for GPU
    CUDA_ERROR_ALREADY_ACQUIRED = 210, ///< Already acquired
    CUDA_ERROR_NOT_MAPPED = 211, ///< Not mapped
    CUDA_ERROR_NOT_MAPPED_AS_ARRAY   = 212, ///< Mapped resource not available for access as an array
    CUDA_ERROR_NOT_MAPPED_AS_POINTER = 213, ///< Mapped resource not available for access as a pointer

    CUDA_ERROR_INVALID_SOURCE = 300, ///< Invalid source
    CUDA_ERROR_FILE_NOT_FOUND = 301, ///< File not found

    CUDA_ERROR_INVALID_HANDLE = 400, ///< Invalid handle

    CUDA_ERROR_NOT_FOUND = 500, ///< Not found

    CUDA_ERROR_NOT_READY = 600, ///< CUDA not ready

    CUDA_ERROR_LAUNCH_FAILED = 700, ///< Launch failed
    CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES = 701, ///< Launch exceeded resources
    CUDA_ERROR_LAUNCH_TIMEOUT = 702, ///< Launch exceeded timeout
    CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING = 703, ///< Launch with incompatible texturing

    CUDA_ERROR_POINTER_IS_64BIT     = 800,      ///< Attempted to retrieve 64-bit pointer via 32-bit API function
    CUDA_ERROR_SIZE_IS_64BIT        = 801,      ///< Attempted to retrieve 64-bit size via 32-bit API function

    CUDA_ERROR_UNKNOWN = 999 ///< Unknown error
    );

  { * If set, host memory is portable between CUDA contexts.
   * Flag for ::cuMemHostAlloc()  }

const

  CU_MEMHOSTALLOC_PORTABLE = $01;

  {* If set, host memory is mapped into CUDA address space and
   * ::cuMemHostGetDevicePointer() may be called on the host pointer.
   * Flag for ::cuMemHostAlloc()     }

  CU_MEMHOSTALLOC_DEVICEMAP = $02;

  {* If set, host memory is allocated as write-combined - fast to write,
   * faster to DMA, slow to read except via SSE4 streaming load instruction
   * (MOVNTDQA).
   * Flag for ::cuMemHostAlloc() }

  CU_MEMHOSTALLOC_WRITECOMBINED = $04;

  // 2D memory copy parameters

type

  PCUDA_MEMCPY2D = ^TCUDA_MEMCPY2D;
  TCUDA_MEMCPY2D = record
    srcXInBytes, ///< Source X in bytes
    srcY: Cardinal; ///< Source Y

    srcMemoryType: TCUmemorytype; ///< Source memory type (host, device, array)
    srcHost: Pointer; ///< Source host pointer
    srcDevice: TCUdeviceptr; ///< Source device pointer
    srcArray: PCUarray; ///< Source array reference
    srcPitch: Cardinal; ///< Source pitch (ignored when src is array)

    dstXInBytes, ///< Destination X in bytes
    dstY: Cardinal; ///< Destination Y
    dstMemoryType: TCUmemorytype;
    ///< Destination memory type (host, device, array)
    dstHost: Pointer; ///< Destination host pointer
    dstDevice: TCUdeviceptr; ///< Destination device pointer
    dstArray: PCUarray; ///< Destination array reference
    dstPitch: Cardinal; ///< Destination pitch (ignored when dst is array)

    WidthInBytes: Cardinal; ///< Width of 2D memory copy in bytes
    Height: Cardinal; ///< Height of 2D memory copy
  end;

  // 3D memory copy parameters

  TCUDA_MEMCPY3D = record

    srcXInBytes, ///< Source X in bytes
    srcY, ///< Source Y
    srcZ: Cardinal; ///< Source Z
    srcLOD: Cardinal; ///< Source LOD
    srcMemoryType: TCUmemorytype; ///< Source memory type (host, device, array)
    srcHost: Pointer; ///< Source host pointer
    srcDevice: TCUdeviceptr; ///< Source device pointer
    srcArray: PCUarray; ///< Source array reference
    reserved0: Pointer; ///< Must be NULL
    srcPitch: Cardinal; ///< Source pitch (ignored when src is array)
    srcHeight: Cardinal;
    ///< Source height (ignored when src is array; may be 0 if Depth==1)

    dstXInBytes, ///< Destination X in bytes
    dstY, ///< Destination Y
    dstZ: Cardinal; ///< Destination Z
    dstLOD: Cardinal; ///< Destination LOD
    dstMemoryType: TCUmemorytype;
    ///< Destination memory type (host, device, array)
    dstHost: Pointer; ///< Destination host pointer
    dstDevice: TCUdeviceptr; ///< Destination device pointer
    dstArray: PCUarray; ///< Destination array reference
    reserved1: Pointer; ///< Must be NULL
    dstPitch: Cardinal; ///< Destination pitch (ignored when dst is array)
    dstHeight: Cardinal;
    ///< Destination height (ignored when dst is array; may be 0 if Depth==1)

    WidthInBytes: Cardinal; ///< Width of 3D memory copy in bytes
    Height: Cardinal; ///< Height of 3D memory copy
    Depth: Cardinal; ///< Depth of 3D memory copy
  end;

  // Array descriptor

  PCUDA_ARRAY_DESCRIPTOR = ^TCUDA_ARRAY_DESCRIPTOR;
  TCUDA_ARRAY_DESCRIPTOR = record
    Width: Cardinal; ///< Width of array
    Height: Cardinal; ///< Height of array
    Format: TCUarray_format; ///< Array format
    NumChannels: Cardinal; ///< Channels per array element
  end;

  // 3D array descriptor

  TCUDA_ARRAY3D_DESCRIPTOR = record
    Width: Cardinal; ///< Width of 3D array
    Height: Cardinal; ///< Height of 3D array
    Depth: Cardinal; ///< Depth of 3D array
    Format: TCUarray_format; ///< Array format
    NumChannels: Cardinal; ///< Channels per array element
    Flags: Cardinal; ///< Flags
  end;

  // Flags to map or unmap a resource

  TCUGLmap_flags = (
    CU_GL_MAP_RESOURCE_FLAGS_NONE,
    CU_GL_MAP_RESOURCE_FLAGS_READ_ONLY,
    CU_GL_MAP_RESOURCE_FLAGS_WRITE_DISCARD
    );

const
  {* Override the texref format with a format inferred from the array.
   * Flag for ::cuTexRefSetArray()  }

  CU_TRSA_OVERRIDE_FORMAT = $01;

  {* Read the texture as integers rather than promoting the values to floats
 * in the range [0,1].
 * Flag for ::cuTexRefSetFlags()   }

  CU_TRSF_READ_AS_INTEGER = $01;

  {* Use normalized texture coordinates in the range [0,1) instead of [0,dim).
   * Flag for ::cuTexRefSetFlags()}

  CU_TRSF_NORMALIZED_COORDINATES = $02;

  {* For texture references loaded into the module, use default texunit from
   * texture reference.   }

  CU_PARAM_TR_DEFAULT = -1;

type
  TDim3 = array[0..2] of LongWord;

{$IFDEF MSWINDOWS}
type
  HGPUNV = Pointer;
{$ENDIF}

var
  cuInit: function(Flags: Cardinal): TCUresult;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  //////////////////////////////////*/
  //// Driver Version Query          /
  //////////////////////////////////*/
  cuDriverGetVersion: function(var driverVersion: Integer): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  //////////////////////////////////////
  ////
  ////    Device management
  ////
  ////////////////////////////////////*/

  cuDeviceGet: function(var device: TCUdevice; ordinal: Integer): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuDeviceGetCount: function(var count: Integer): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuDeviceGetName: function(var name: AnsiChar; len: Integer; dev: TCUdevice):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuDeviceComputeCapability: function(var major: Integer; var minor: Integer;
    dev: TCUdevice): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuDeviceTotalMem: function(var bytes: Cardinal; dev: TCUdevice): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuDeviceGetProperties: function(var prop: TCUdevprop; dev: TCUdevice):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuDeviceGetAttribute: function(var pi: Integer; attrib: TCUdevice_attribute;
    dev: TCUdevice): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  //////////////////////////////////////
  ////
  ////    Context management
  ////
  ////////////////////////////////////*/

  cuCtxCreate: function(var pctx: PCUcontext; flags: Cardinal; dev: TCUdevice):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuCtxDestroy: function(ctx: PCUcontext): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuCtxAttach: function(var pctx: PCUcontext; flags: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuCtxDetach: function(ctx: PCUcontext): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuCtxPushCurrent: function(ctx: PCUcontext): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuCtxPopCurrent: function(var pctx: PCUcontext): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuCtxGetDevice: function(var device: TCUdevice): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuCtxSynchronize: function: TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  /////////////////////////////////////
  ////
  ////    Module management
  ////
  //////////////////////////////////*/

  cuModuleLoad: function(var module: PCUmodule; const fname: PAnsiChar):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuModuleLoadData: function(var module: PCUmodule; const image: PAnsiChar):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuModuleLoadDataEx: function(var module: PCUmodule; var image; numOptions:
    Cardinal; var options: TCUjit_option; var optionValues):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuModuleLoadFatBinary: function(var module: PCUmodule; var fatCubin):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuModuleUnload: function(hmod: PCUmodule): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuModuleGetFunction: function(var hfunc: PCUfunction; hmod: PCUmodule; const
    name: PAnsiChar): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuModuleGetGlobal: function(var dptr: TCUdeviceptr; var bytes: Cardinal; hmod:
    PCUmodule; const name: PAnsiChar): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuModuleGetTexRef: function(var pTexRef: PCUtexref; hmod: PCUmodule; const
    name: PAnsiChar): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  ///////////////////////////////////////
  ////
  ////    Memory management
  ////
  ////////////////////////////////////*/

  cuMemGetInfo: function(var free: Cardinal; var total: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemAlloc: function(var dptr: TCUdeviceptr; bytesize: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemAllocPitch: function(var dptr: TCUdeviceptr;
    var pPitch: Cardinal;
    WidthInBytes: Cardinal;
    Height: Cardinal;
    // size of biggest r/w to be performed by kernels on this memory
    // 4; 8 or 16 bytes
    ElementSizeBytes: Cardinal
    ): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemFree: function(dptr: TCUdeviceptr): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemGetAddressRange: function(var pbase: TCUdeviceptr; var psize: Cardinal;
    dptr: TCUdeviceptr): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemAllocHost: function(var pp; bytesize: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemFreeHost: function(p: Pointer): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemHostAlloc: function(var pp; bytesize: Cardinal; Flags: Cardinal):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemHostGetDevicePointer: function(var pdptr: TCUdeviceptr; var p; Flags:
    Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemHostGetFlags: function(var pFlags: Cardinal; var p): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  ///////////////////////////////////////
  ////
  ////    Synchronous Memcpy
  ////
  //// : Integerra-device memcpy's done with these functions may execute in parallel with the CPU;
  //// but if host memory is involved; they wait until the copy is done before returning.
  ////
  ////////////////////////////////////*/

  // 1D functions
        // system <-> device memory
  cuMemcpyHtoD: function(dstDevice: TCUdeviceptr; const srcHost: Pointer; ByteCount:
    Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemcpyDtoH: function(const dstHost: Pointer; srcDevice: TCUdeviceptr; ByteCount:
    Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  // device <-> device memory
  cuMemcpyDtoD: function(dstDevice: TCUdeviceptr; srcDevice: TCUdeviceptr;
    ByteCount: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  // device <-> device memory
  cuMemcpyDtoDAsync: function(dstDevice: TCUdeviceptr;
    srcDevice: TCUdeviceptr; ByteCount: Cardinal; hStream: PCUstream): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  // device <-> array memory
  cuMemcpyDtoA: function(dstArray: PCUarray; dstIndex: Cardinal; srcDevice:
    TCUdeviceptr; ByteCount: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemcpyAtoD: function(dstDevice: TCUdeviceptr; hSrc: PCUarray; SrcIndex:
    Cardinal; ByteCount: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  // system <-> array memory
  cuMemcpyHtoA: function(dstArray: PCUarray; dstIndex: Cardinal; pSrc: Pointer;
    ByteCount: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemcpyAtoH: function(dstHost: Pointer; srcArray: PCUarray; srcIndex: Cardinal;
    ByteCount: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  // array <-> array memory
  cuMemcpyAtoA: function(dstArray: PCUarray; dstIndex: Cardinal; srcArray:
    PCUarray; srcIndex: Cardinal; ByteCount: Cardinal):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  // 2D memcpy

  cuMemcpy2D: function(const pCopy: PCUDA_MEMCPY2D):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemcpy2DUnaligned: function(var pCopy: TCUDA_MEMCPY2D): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  // 3D memcpy

  cuMemcpy3D: function(var pCopy: TCUDA_MEMCPY3D): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  ///////////////////////////////////////
  ////
  ////    Asynchronous Memcpy
  ////
  //// Any host memory involved must be DMA'able (e.g.; allocated with cuMemAllocHost).
  //// memcpy's done with these functions execute in parallel with the CPU and; if
  //// the hardware is available; may execute in parallel with the GPU.
  //// Asynchronous memcpy must be accompanied by appropriate stream synchronization.
  ////
  ////////////////////////////////////*/

  // 1D functions
        // system <-> device memory
  cuMemcpyHtoDAsync: function(dstDevice: TCUdeviceptr;
    var srcHost; ByteCount: Cardinal; hStream: PCUstream): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemcpyDtoHAsync: function(var dstHost;
    srcDevice: TCUdeviceptr; ByteCount: Cardinal; hStream: PCUstream):
    TCUresult{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  // system <-> array memory
  cuMemcpyHtoAAsync: function(dstArray: PCUarray; dstIndex: Cardinal;
    var pSrc; ByteCount: Cardinal; hStream: PCUstream): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemcpyAtoHAsync: function(var dstHost; srcArray: PCUstream; srcIndex:
    Cardinal;
    ByteCount: Cardinal; hStream: PCUstream): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  // 2D memcpy
  cuMemcpy2DAsync: function(var pCopy: TCUDA_MEMCPY2D; hStream: PCUstream):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  // 3D memcpy
  cuMemcpy3DAsync: function(var pCopy: TCUDA_MEMCPY3D; hStream: PCUstream):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  ///////////////////////////////////////
  ////
  ////    Memset
  ////
  ////////////////////////////////////*/
  cuMemsetD8: function(dstDevice: TCUdeviceptr; ub: Byte; N: Cardinal):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemsetD16: function(dstDevice: TCUdeviceptr; uw: Word; N: Cardinal):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemsetD32: function(dstDevice: TCUdeviceptr; ui: Cardinal; N: Cardinal):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemsetD2D8: function(dstDevice: TCUdeviceptr; dstPitch: Cardinal; ub: Byte;
    Width: Cardinal; Height: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemsetD2D16: function(dstDevice: TCUdeviceptr; dstPitch: Cardinal; uw: Word;
    Width: Cardinal; Height: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuMemsetD2D32: function(dstDevice: TCUdeviceptr; dstPitch: Cardinal; ui:
    Cardinal; Width: Cardinal; Height: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  ///////////////////////////////////////
  ////
  ////    management
  ////
  ////////////////////////////////////*/

  cuFuncSetBlockShape: function(hfunc: PCUfunction; x: Integer; y: Integer; z:
    Integer): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuFuncSetSharedSize: function(hfunc: PCUfunction; bytes: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuFuncGetAttribute: function(var pi: Integer; attrib: TCUfunction_attribute;
    hfunc: PCUfunction): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  ///////////////////////////////////////
  ////
  ////    Array management
  ////
  ////////////////////////////////////*/

  cuArrayCreate: function(var pHandle: PCUarray; var pAllocateArray:
    TCUDA_ARRAY_DESCRIPTOR): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuArrayGetDescriptor: function(var pArrayDescriptor: TCUDA_ARRAY_DESCRIPTOR;
    hArray: PCUarray): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuArrayDestroy: function(hArray: PCUarray): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuArray3DCreate: function(var pHandle: PCUarray; var pAllocateArray:
    TCUDA_ARRAY3D_DESCRIPTOR): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuArray3DGetDescriptor: function(var pArrayDescriptor:
    TCUDA_ARRAY3D_DESCRIPTOR; hArray: PCUarray): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  ///////////////////////////////////////
  ////
  ////    Texture reference management
  ////
  ////////////////////////////////////*/
  cuTexRefCreate: function(var pTexRef: PCUtexref): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefDestroy: function(hTexRef: PCUtexref): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefSetArray: function(hTexRef: PCUtexref; hArray: PCUarray; Flags:
    Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefSetAddress: function(var ByteOffset: Cardinal; hTexRef: PCUtexref;
    dptr: TCUdeviceptr; bytes: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefSetAddress2D: function(hTexRef: PCUtexref; var desc:
    TCUDA_ARRAY_DESCRIPTOR; dptr: TCUdeviceptr; Pitch: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefSetFormat: function(hTexRef: PCUtexref; fmt: TCUarray_format;
    NumPackedComponents: Integer): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefSetAddressMode: function(hTexRef: PCUtexref; dim: Integer; am:
    TCUaddress_mode): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefSetFilterMode: function(hTexRef: PCUtexref; fm: TCUfilter_mode):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefSetFlags: function(hTexRef: PCUtexref; Flags: Cardinal):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefGetAddress: function(var pdptr: TCUdeviceptr; hTexRef: PCUtexref):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefGetArray: function(var phArray: PCUarray; hTexRef: PCUtexref):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefGetAddressMode: function(var pam: TCUaddress_mode; hTexRef: PCUtexref;
    dim: Integer): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefGetFilterMode: function(var pfm: TCUfilter_mode; hTexRef: PCUtexref):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefGetFormat: function(var pFormat: TCUarray_format; var pNumChannels:
    Integer; hTexRef: PCUtexref): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuTexRefGetFlags: function(var pFlags: Cardinal; hTexRef: PCUtexref):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  ///////////////////////////////////////
  ////
  ////    Parameter management
  ////
  ////////////////////////////////////*/

  cuParamSetSize: function(hfunc: PCUfunction; numbytes: Cardinal):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuParamSeti: function(hfunc: PCUfunction; offset: Integer; value: Cardinal):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuParamSetf: function(hfunc: PCUfunction; offset: Integer; value: Single):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuParamSetv: function(hfunc: PCUfunction; offset: Integer; var ptr; numbytes:
    Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuParamSetTexRef: function(hfunc: PCUfunction; texunit: Integer; hTexRef:
    PCUtexref): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  ///////////////////////////////////////
  ////
  ////    Launch functions
  ////
  ////////////////////////////////////*/

  cuLaunch: function(f: PCUfunction): TCUresult;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuLaunchGrid: function(f: PCUfunction; grid_width: Integer; grid_height:
    Integer): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuLaunchGridAsync: function(f: PCUfunction; grid_width: Integer; grid_height:
    Integer; hStream: PCUstream): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  ///////////////////////////////////////
  ////
  ////    Events
  ////
  ////////////////////////////////////*/
  cuEventCreate: function(var phEvent: PCUevent; Flags: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuEventRecord: function(hEvent: PCUevent; hStream: PCUstream):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuEventQuery: function(hEvent: PCUevent): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuEventSynchronize: function(hEvent: PCUevent): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuEventDestroy: function(hEvent: PCUevent): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuEventElapsedTime: function(var pMilliseconds: Single; hStart: PCUevent;
    hEnd: PCUevent): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  /////////////////////////////////////
  ////
  ////    Streams
  ////
  ///////////////////////////////////*/
  cuStreamCreate: function(var phStream: PCUstream; Flags: Cardinal):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuStreamQuery: function(hStream: PCUstream): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuStreamSynchronize: function(hStream: PCUstream): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuStreamDestroy: function(hStream: PCUstream): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuGLCtxCreate: function(var pCtx: PCUcontext; Flags: Cardinal; device:
    TCUdevice): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  /////////////////////////////////////
  ////
  ////    Graphics interop
  ////
  ///////////////////////////////////*/
  ///
  cuGraphicsGLRegisterBuffer: function(var pCudaResource: PCUgraphicsResource;
    buffer: Cardinal; Flags: TCUgraphicsMapResourceFlags): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cuGraphicsGLRegisterImage: function(var pCudaResource: PCUgraphicsResource;
    image, target: Cardinal; Flags: TCUgraphicsMapResourceFlags): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

{$IFDEF MSWINDOWS}
  cuWGLGetDevice: function(var pDevice: TCUdevice; hGpu: HGPUNV): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
{$ENDIF}

  cuGraphicsUnregisterResource: function(resource: PCUgraphicsResource):
    TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cuGraphicsSubResourceGetMappedArray: function(var pArray: PCUarray; resource:
    PCUgraphicsResource; arrayIndex: Cardinal; mipLevel: Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cuGraphicsResourceGetMappedPointer: function(var pDevPtr: TCUdeviceptr; var
    pSize: Cardinal; resource: PCUgraphicsResource): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cuGraphicsResourceSetMapFlags: function(resource: PCUgraphicsResource; flags:
    Cardinal): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cuGraphicsMapResources: function(count: Cardinal; resources:
    PPCUgraphicsResource; hStream: PCUstream): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  cuGraphicsUnmapResources: function(count: Cardinal; resources:
    PPCUgraphicsResource; hStream: PCUstream): TCUresult
{$IFDEF CUDA_STDCALL} stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}
  //
  // CUDA 2.x compatibility API. These functions are deprecated,
  // please use the ones above.
  //

  cuGLInit: procedure;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuGLRegisterBufferObject: function(buffer: Cardinal): TCUresult;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuGLMapBufferObject: function(var dptr: TCUdeviceptr; var size: Cardinal;
    buffer: Cardinal): TCUresult;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuGLUnmapBufferObject: function(buffer: Cardinal): TCUresult;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuGLUnregisterBufferObject: function(buffer: Cardinal): TCUresult;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuGLSetBufferObjectMapFlags: function(buffer: Cardinal; Flags: Cardinal):
    TCUresult;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuGLMapBufferObjectAsync: function(var dptr: TCUdeviceptr; var size: Cardinal;
    buffer: Cardinal; hStream: PCUstream): TCUresult;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

  cuGLUnmapBufferObjectAsync: function(buffer: Cardinal; hStream: PCUstream):
    TCUresult;
{$IFDEF CUDA_STDCALL}stdcall;
{$ENDIF}
{$IFDEF CUDA_CDECL}cdecl;
{$ENDIF}

function InitCUDA: Boolean;
procedure CloseCUDA;
function InitCUDAFromLibrary(const LibName: WideString): Boolean;
function IsCUDAInitialized: Boolean;
function GetCUDAAPIerrorString(err: TCUresult): string;

implementation

const
  INVALID_MODULEHANDLE = 0;

  // ************** Windows specific ********************
{$IFDEF MSWINDOWS}
var
  CUDAHandle: HINST;
{$ENDIF}
  // ************** UNIX specific ********************
{$IFDEF UNIX}
var
  CUDAHandle: TLibHandle;
{$ENDIF}

function CUDAGetProcAddress(ProcName: PAnsiChar): Pointer;
begin
  result := GetProcAddress(Cardinal(CUDAHandle), ProcName);
end;

function InitCUDA: Boolean;
begin
  if CUDAHandle = INVALID_MODULEHANDLE then
    Result := InitCUDAFromLibrary(CUDAAPIDLL)
  else
    Result := True;
end;

procedure CloseCUDA;
begin
  if CUDAHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(Cardinal(CUDAHandle));
    CUDAHandle := INVALID_MODULEHANDLE;
  end;
end;

function InitCUDAFromLibrary(const LibName: WideString): Boolean;
begin
  Result := False;
  CloseCUDA;
  CUDAHandle := LoadLibraryW(PWideChar(LibName));
  if CUDAHandle = INVALID_MODULEHANDLE then
    Exit;

  cuArray3DCreate := CUDAGetProcAddress('cuArray3DCreate');
  cuArray3DGetDescriptor := CUDAGetProcAddress('cuArray3DGetDescriptor');
  cuArrayCreate := CUDAGetProcAddress('cuArrayCreate');
  cuArrayDestroy := CUDAGetProcAddress('cuArrayDestroy');
  cuArrayGetDescriptor := CUDAGetProcAddress('cuArrayGetDescriptor');
  cuCtxAttach := CUDAGetProcAddress('cuCtxAttach');
  cuCtxCreate := CUDAGetProcAddress('cuCtxCreate');
  cuCtxDestroy := CUDAGetProcAddress('cuCtxDestroy');
  cuCtxDetach := CUDAGetProcAddress('cuCtxDetach');
  cuCtxGetDevice := CUDAGetProcAddress('cuCtxGetDevice');
  cuCtxPopCurrent := CUDAGetProcAddress('cuCtxPopCurrent');
  cuCtxPushCurrent := CUDAGetProcAddress('cuCtxPushCurrent');
  cuCtxSynchronize := CUDAGetProcAddress('cuCtxSynchronize');
  //  cuD3D10CtxCreate := CUDAGetProcAddress('cuD3D10CtxCreate');
  //  cuD3D10GetDevice := CUDAGetProcAddress('cuD3D10GetDevice');
  //  cuD3D10MapResources := CUDAGetProcAddress('cuD3D10MapResources');
  //  cuD3D10RegisterResource := CUDAGetProcAddress('cuD3D10RegisterResource');
  //  cuD3D10ResourceGetMappedArray := CUDAGetProcAddress('cuD3D10ResourceGetMappedArray');
  //  cuD3D10ResourceGetMappedPitch := CUDAGetProcAddress('cuD3D10ResourceGetMappedPitch');
  //  cuD3D10ResourceGetMappedPointer := CUDAGetProcAddress('cuD3D10ResourceGetMappedPointer');
  //  cuD3D10ResourceGetMappedSize := CUDAGetProcAddress('cuD3D10ResourceGetMappedSize');
  //  cuD3D10ResourceGetSurfaceDimensions := CUDAGetProcAddress('cuD3D10ResourceGetSurfaceDimensions');
  //  cuD3D10ResourceSetMapFlags := CUDAGetProcAddress('cuD3D10ResourceSetMapFlags');
  //  cuD3D10UnmapResources := CUDAGetProcAddress('cuD3D10UnmapResources');
  //  cuD3D10UnregisterResource := CUDAGetProcAddress('cuD3D10UnregisterResource');
  //  cuD3D11CtxCreate := CUDAGetProcAddress('cuD3D11CtxCreate');
  //  cuD3D11GetDevice := CUDAGetProcAddress('cuD3D11GetDevice');
  //  cuD3D9Begin := CUDAGetProcAddress('cuD3D9Begin');
  //  cuD3D9CtxCreate := CUDAGetProcAddress('cuD3D9CtxCreate');
  //  cuD3D9End := CUDAGetProcAddress('cuD3D9End');
  //  cuD3D9GetDevice := CUDAGetProcAddress('cuD3D9GetDevice');
  //  cuD3D9GetDirect3DDevice := CUDAGetProcAddress('cuD3D9GetDirect3DDevice');
  //  cuD3D9MapResources := CUDAGetProcAddress('cuD3D9MapResources');
  //  cuD3D9MapVertexBuffer := CUDAGetProcAddress('cuD3D9MapVertexBuffer');
  //  cuD3D9RegisterResource := CUDAGetProcAddress('cuD3D9RegisterResource');
  //  cuD3D9RegisterVertexBuffer := CUDAGetProcAddress('cuD3D9RegisterVertexBuffer');
  //  cuD3D9ResourceGetMappedArray := CUDAGetProcAddress('cuD3D9ResourceGetMappedArray');
  //  cuD3D9ResourceGetMappedPitch := CUDAGetProcAddress('cuD3D9ResourceGetMappedPitch');
  //  cuD3D9ResourceGetMappedPointer := CUDAGetProcAddress('cuD3D9ResourceGetMappedPointer');
  //  cuD3D9ResourceGetMappedSize := CUDAGetProcAddress('cuD3D9ResourceGetMappedSize');
  //  cuD3D9ResourceGetSurfaceDimensions := CUDAGetProcAddress('cuD3D9ResourceGetSurfaceDimensions');
  //  cuD3D9ResourceSetMapFlags := CUDAGetProcAddress('cuD3D9ResourceSetMapFlags');
  //  cuD3D9UnmapResources := CUDAGetProcAddress('cuD3D9UnmapResources');
  //  cuD3D9UnmapVertexBuffer := CUDAGetProcAddress('cuD3D9UnmapVertexBuffer');
  //  cuD3D9UnregisterResource := CUDAGetProcAddress('cuD3D9UnregisterResource');
  //  cuD3D9UnregisterVertexBuffer := CUDAGetProcAddress('cuD3D9UnregisterVertexBuffer');
  cuDeviceComputeCapability := CUDAGetProcAddress('cuDeviceComputeCapability');
  cuDeviceGet := CUDAGetProcAddress('cuDeviceGet');
  cuDeviceGetAttribute := CUDAGetProcAddress('cuDeviceGetAttribute');
  cuDeviceGetCount := CUDAGetProcAddress('cuDeviceGetCount');
  cuDeviceGetName := CUDAGetProcAddress('cuDeviceGetName');
  cuDeviceGetProperties := CUDAGetProcAddress('cuDeviceGetProperties');
  cuDeviceTotalMem := CUDAGetProcAddress('cuDeviceTotalMem');
  cuDriverGetVersion := CUDAGetProcAddress('cuDriverGetVersion');
  cuEventCreate := CUDAGetProcAddress('cuEventCreate');
  cuEventDestroy := CUDAGetProcAddress('cuEventDestroy');
  cuEventElapsedTime := CUDAGetProcAddress('cuEventElapsedTime');
  cuEventQuery := CUDAGetProcAddress('cuEventQuery');
  cuEventRecord := CUDAGetProcAddress('cuEventRecord');
  cuEventSynchronize := CUDAGetProcAddress('cuEventSynchronize');
  cuFuncGetAttribute := CUDAGetProcAddress('cuFuncGetAttribute');
  cuFuncSetBlockShape := CUDAGetProcAddress('cuFuncSetBlockShape');
  cuFuncSetSharedSize := CUDAGetProcAddress('cuFuncSetSharedSize');
  cuGLCtxCreate := CUDAGetProcAddress('cuGLCtxCreate');
  cuGLInit := CUDAGetProcAddress('cuGLInit');
  cuGLMapBufferObject := CUDAGetProcAddress('cuGLMapBufferObject');
  cuGLMapBufferObjectAsync := CUDAGetProcAddress('cuGLMapBufferObjectAsync');
  cuGLRegisterBufferObject := CUDAGetProcAddress('cuGLRegisterBufferObject');
  cuGLSetBufferObjectMapFlags :=
    CUDAGetProcAddress('cuGLSetBufferObjectMapFlags');
  cuGLUnmapBufferObject := CUDAGetProcAddress('cuGLUnmapBufferObject');
  cuGLUnmapBufferObjectAsync :=
    CUDAGetProcAddress('cuGLUnmapBufferObjectAsync');
  cuGLUnregisterBufferObject :=
    CUDAGetProcAddress('cuGLUnregisterBufferObject');
  //  cuGraphicsD3D10RegisterResource := CUDAGetProcAddress('cuGraphicsD3D10RegisterResource');
  //  cuGraphicsD3D11RegisterResource := CUDAGetProcAddress('cuGraphicsD3D11RegisterResource');
  //  cuGraphicsD3D9RegisterResource := CUDAGetProcAddress('cuGraphicsD3D9RegisterResource');
  cuGraphicsGLRegisterBuffer :=
    CUDAGetProcAddress('cuGraphicsGLRegisterBuffer');
  cuGraphicsGLRegisterImage := CUDAGetProcAddress('cuGraphicsGLRegisterImage');
  cuGraphicsMapResources := CUDAGetProcAddress('cuGraphicsMapResources');
  cuGraphicsResourceGetMappedPointer :=
    CUDAGetProcAddress('cuGraphicsResourceGetMappedPointer');
  cuGraphicsResourceSetMapFlags :=
    CUDAGetProcAddress('cuGraphicsResourceSetMapFlags');
  cuGraphicsSubResourceGetMappedArray :=
    CUDAGetProcAddress('cuGraphicsSubResourceGetMappedArray');
  cuGraphicsUnmapResources := CUDAGetProcAddress('cuGraphicsUnmapResources');
  cuGraphicsUnregisterResource :=
    CUDAGetProcAddress('cuGraphicsUnregisterResource');
  cuInit := CUDAGetProcAddress('cuInit');
  cuLaunch := CUDAGetProcAddress('cuLaunch');
  cuLaunchGrid := CUDAGetProcAddress('cuLaunchGrid');
  cuLaunchGridAsync := CUDAGetProcAddress('cuLaunchGridAsync');
  cuMemAlloc := CUDAGetProcAddress('cuMemAlloc');
  cuMemAllocHost := CUDAGetProcAddress('cuMemAllocHost');
  cuMemAllocPitch := CUDAGetProcAddress('cuMemAllocPitch');
  cuMemcpy2D := CUDAGetProcAddress('cuMemcpy2D');
  cuMemcpy2DAsync := CUDAGetProcAddress('cuMemcpy2DAsync');
  cuMemcpy2DUnaligned := CUDAGetProcAddress('cuMemcpy2DUnaligned');
  cuMemcpy3D := CUDAGetProcAddress('cuMemcpy3D');
  cuMemcpy3DAsync := CUDAGetProcAddress('cuMemcpy3DAsync');
  cuMemcpyAtoA := CUDAGetProcAddress('cuMemcpyAtoA');
  cuMemcpyAtoD := CUDAGetProcAddress('cuMemcpyAtoD');
  cuMemcpyAtoH := CUDAGetProcAddress('cuMemcpyAtoH');
  cuMemcpyAtoHAsync := CUDAGetProcAddress('cuMemcpyAtoHAsync');
  cuMemcpyDtoA := CUDAGetProcAddress('cuMemcpyDtoA');
  cuMemcpyDtoD := CUDAGetProcAddress('cuMemcpyDtoD');
  cuMemcpyDtoDAsync := CUDAGetProcAddress('cuMemcpyDtoDAsync');
  cuMemcpyDtoH := CUDAGetProcAddress('cuMemcpyDtoH');
  cuMemcpyDtoHAsync := CUDAGetProcAddress('cuMemcpyDtoHAsync');
  cuMemcpyHtoA := CUDAGetProcAddress('cuMemcpyHtoA');
  cuMemcpyHtoAAsync := CUDAGetProcAddress('cuMemcpyHtoAAsync');
  cuMemcpyHtoD := CUDAGetProcAddress('cuMemcpyHtoD');
  cuMemcpyHtoDAsync := CUDAGetProcAddress('cuMemcpyHtoDAsync');
  cuMemFree := CUDAGetProcAddress('cuMemFree');
  cuMemFreeHost := CUDAGetProcAddress('cuMemFreeHost');
  cuMemGetAddressRange := CUDAGetProcAddress('cuMemGetAddressRange');
  cuMemGetInfo := CUDAGetProcAddress('cuMemGetInfo');
  cuMemHostAlloc := CUDAGetProcAddress('cuMemHostAlloc');
  cuMemHostGetDevicePointer := CUDAGetProcAddress('cuMemHostGetDevicePointer');
  cuMemHostGetFlags := CUDAGetProcAddress('cuMemHostGetFlags');
  cuMemsetD16 := CUDAGetProcAddress('cuMemsetD16');
  cuMemsetD2D16 := CUDAGetProcAddress('cuMemsetD2D16');
  cuMemsetD2D32 := CUDAGetProcAddress('cuMemsetD2D32');
  cuMemsetD2D8 := CUDAGetProcAddress('cuMemsetD2D8');
  cuMemsetD32 := CUDAGetProcAddress('cuMemsetD32');
  cuMemsetD8 := CUDAGetProcAddress('cuMemsetD8');
  cuModuleGetFunction := CUDAGetProcAddress('cuModuleGetFunction');
  cuModuleGetGlobal := CUDAGetProcAddress('cuModuleGetGlobal');
  //  cuModuleGetSurfRef := CUDAGetProcAddress('cuModuleGetSurfRef');
  cuModuleGetTexRef := CUDAGetProcAddress('cuModuleGetTexRef');
  cuModuleLoad := CUDAGetProcAddress('cuModuleLoad');
  cuModuleLoadData := CUDAGetProcAddress('cuModuleLoadData');
  cuModuleLoadDataEx := CUDAGetProcAddress('cuModuleLoadDataEx');
  cuModuleLoadFatBinary := CUDAGetProcAddress('cuModuleLoadFatBinary');
  cuModuleUnload := CUDAGetProcAddress('cuModuleUnload');
  cuParamSetf := CUDAGetProcAddress('cuParamSetf');
  cuParamSeti := CUDAGetProcAddress('cuParamSeti');
  cuParamSetSize := CUDAGetProcAddress('cuParamSetSize');
  //  cuParamSetSurfRef := CUDAGetProcAddress('cuParamSetSurfRef');
  cuParamSetTexRef := CUDAGetProcAddress('cuParamSetTexRef');
  cuParamSetv := CUDAGetProcAddress('cuParamSetv');
  cuStreamCreate := CUDAGetProcAddress('cuStreamCreate');
  cuStreamDestroy := CUDAGetProcAddress('cuStreamDestroy');
  cuStreamQuery := CUDAGetProcAddress('cuStreamQuery');
  cuStreamSynchronize := CUDAGetProcAddress('cuStreamSynchronize');
  //  cuSurfRefCreate := CUDAGetProcAddress('cuSurfRefCreate');
  //  cuSurfRefDestroy := CUDAGetProcAddress('cuSurfRefDestroy');
  //  cuSurfRefGetAddress := CUDAGetProcAddress('cuSurfRefGetAddress');
  //  cuSurfRefGetArray := CUDAGetProcAddress('cuSurfRefGetArray');
  //  cuSurfRefGetFormat := CUDAGetProcAddress('cuSurfRefGetFormat');
  //  cuSurfRefSetAddress := CUDAGetProcAddress('cuSurfRefSetAddress');
  //  cuSurfRefSetArray := CUDAGetProcAddress('cuSurfRefSetArray');
  //  cuSurfRefSetFormat := CUDAGetProcAddress('cuSurfRefSetFormat');
  cuTexRefCreate := CUDAGetProcAddress('cuTexRefCreate');
  cuTexRefDestroy := CUDAGetProcAddress('cuTexRefDestroy');
  cuTexRefGetAddress := CUDAGetProcAddress('cuTexRefGetAddress');
  cuTexRefGetAddressMode := CUDAGetProcAddress('cuTexRefGetAddressMode');
  cuTexRefGetArray := CUDAGetProcAddress('cuTexRefGetArray');
  cuTexRefGetFilterMode := CUDAGetProcAddress('cuTexRefGetFilterMode');
  cuTexRefGetFlags := CUDAGetProcAddress('cuTexRefGetFlags');
  cuTexRefGetFormat := CUDAGetProcAddress('cuTexRefGetFormat');
  cuTexRefSetAddress := CUDAGetProcAddress('cuTexRefSetAddress');
  cuTexRefSetAddress2D := CUDAGetProcAddress('cuTexRefSetAddress2D');
  cuTexRefSetAddressMode := CUDAGetProcAddress('cuTexRefSetAddressMode');
  cuTexRefSetArray := CUDAGetProcAddress('cuTexRefSetArray');
  //  cuTexRefSetCPUAddress := CUDAGetProcAddress('cuTexRefSetCPUAddress');
  cuTexRefSetFilterMode := CUDAGetProcAddress('cuTexRefSetFilterMode');
  cuTexRefSetFlags := CUDAGetProcAddress('cuTexRefSetFlags');
  cuTexRefSetFormat := CUDAGetProcAddress('cuTexRefSetFormat');
  cuWGLGetDevice := CUDAGetProcAddress('cuWGLGetDevice');

  Result := True;
end;

function IsCUDAInitialized: Boolean;
begin
  Result := (CUDAHandle <> INVALID_MODULEHANDLE);
end;

function GetCUDAAPIerrorString(err: TCUresult): string;
begin
  case err of
    CUDA_SUCCESS: Result :='No errors';
    CUDA_ERROR_INVALID_VALUE: Result :='Invalid value';
    CUDA_ERROR_OUT_OF_MEMORY: Result :='Out of memory';
    CUDA_ERROR_NOT_INITIALIZED: Result :='Driver not initialized';
    CUDA_ERROR_DEINITIALIZED: Result :='Driver deinitialized';
    CUDA_ERROR_NO_DEVICE: Result :='No CUDA-capable device available';
    CUDA_ERROR_INVALID_DEVICE: Result :='Invalid device';
    CUDA_ERROR_INVALID_IMAGE: Result :='Invalid kernel image';
    CUDA_ERROR_INVALID_CONTEXT: Result :='Invalid context';
    CUDA_ERROR_CONTEXT_ALREADY_CURRENT: Result :='Context already current';
    CUDA_ERROR_MAP_FAILED: Result :='Map failed';
    CUDA_ERROR_UNMAP_FAILED: Result :='Unmap failed';
    CUDA_ERROR_ARRAY_IS_MAPPED: Result :='Array is mapped';
    CUDA_ERROR_ALREADY_MAPPED: Result :='Already mapped';
    CUDA_ERROR_NO_BINARY_FOR_GPU: Result :='No binary for GPU';
    CUDA_ERROR_ALREADY_ACQUIRED: Result :='Already acquired';
    CUDA_ERROR_NOT_MAPPED: Result :='Not mapped';
    CUDA_ERROR_NOT_MAPPED_AS_ARRAY: Result :='Not mapped as array';
    CUDA_ERROR_NOT_MAPPED_AS_POINTER: Result :='Not mapped as pointer';
    CUDA_ERROR_INVALID_SOURCE: Result :='Invalid source';
    CUDA_ERROR_FILE_NOT_FOUND: Result :='File not found';
    CUDA_ERROR_INVALID_HANDLE: Result :='Invalid handle';
    CUDA_ERROR_NOT_FOUND: Result :='Not found';
    CUDA_ERROR_NOT_READY: Result :='CUDA not ready';
    CUDA_ERROR_LAUNCH_FAILED: Result :='Launch failed';
    CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES: Result :='Launch exceeded resources';
    CUDA_ERROR_LAUNCH_TIMEOUT: Result :='Launch exceeded timeout';
    CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING: Result :='Launch with incompatible texturing';
    CUDA_ERROR_POINTER_IS_64BIT: Result :='Pointer is 64bit';
    CUDA_ERROR_SIZE_IS_64BIT: Result :='Size is 64bit';
    CUDA_ERROR_UNKNOWN: Result :='Unknown error';
  end;
end;

end.

