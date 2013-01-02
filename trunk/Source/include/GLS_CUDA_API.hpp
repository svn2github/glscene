// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS_CUDA_API.pas' rev: 24.00 (Win32)

#ifndef Gls_cuda_apiHPP
#define Gls_cuda_apiHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLS_CL_Platform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gls_cuda_api
{
//-- type declarations -------------------------------------------------------
typedef void * TCUdeviceptr;

typedef int TCUdevice;

struct DECLSPEC_DRECORD TCUcontext
{
};


typedef TCUcontext *PCUcontext;

struct DECLSPEC_DRECORD TCUmodule
{
};


typedef TCUmodule *PCUmodule;

struct DECLSPEC_DRECORD TCUfunction
{
};


typedef TCUfunction *PCUfunction;

struct DECLSPEC_DRECORD TCUarray
{
};


typedef TCUarray *PCUarray;

struct DECLSPEC_DRECORD TCUtexref
{
};


typedef TCUtexref *PCUtexref;

struct DECLSPEC_DRECORD TCUevent
{
};


typedef TCUevent *PCUevent;

struct DECLSPEC_DRECORD TCUstream
{
};


typedef TCUstream *PCUstream;

struct DECLSPEC_DRECORD TCUgraphicsResource
{
};


typedef TCUgraphicsResource *PCUgraphicsResource;

typedef PCUgraphicsResource *PPCUgraphicsResource;

enum TCUctx_flags : unsigned int { CU_CTX_SCHED_AUTO, CU_CTX_SCHED_SPIN, CU_CTX_SCHED_YIELD, CU_CTX_SCHED_MASK, CU_CTX_BLOCKING_SYNC, CU_CTX_MAP_HOST = 8, CU_CTX_FLAGS_MASK = 15 };

enum TCUevent_flags : unsigned int { CU_EVENT_DEFAULT, CU_EVENT_BLOCKING_SYNC };

enum TCUarray_format : unsigned int { CU_AD_FORMAT_UNSIGNED_INT8 = 1, CU_AD_FORMAT_UNSIGNED_INT16, CU_AD_FORMAT_UNSIGNED_INT32, CU_AD_FORMAT_SIGNED_INT8 = 8, CU_AD_FORMAT_SIGNED_INT16, CU_AD_FORMAT_SIGNED_INT32, CU_AD_FORMAT_HALF = 16, CU_AD_FORMAT_FLOAT = 32 };

enum TCUaddress_mode : unsigned int { CU_TR_ADDRESS_MODE_WRAP, CU_TR_ADDRESS_MODE_CLAMP, CU_TR_ADDRESS_MODE_MIRROR };

enum TCUfilter_mode : unsigned int { CU_TR_FILTER_MODE_POINT, CU_TR_FILTER_MODE_LINEAR };

enum TCUdevice_attribute : unsigned int { CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK = 1, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z, CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK, CU_DEVICE_ATTRIBUTE_SHARED_MEMORY_PER_BLOCK = 8, CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY, CU_DEVICE_ATTRIBUTE_WARP_SIZE, CU_DEVICE_ATTRIBUTE_MAX_PITCH, CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK, CU_DEVICE_ATTRIBUTE_REGISTERS_PER_BLOCK = 12, CU_DEVICE_ATTRIBUTE_CLOCK_RATE, CU_DEVICE_ATTRIBUTE_TEXTURE_ALIGNMENT, CU_DEVICE_ATTRIBUTE_GPU_OVERLAP, CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT, 
	CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT, CU_DEVICE_ATTRIBUTE_INTEGRATED, CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY, CU_DEVICE_ATTRIBUTE_COMPUTE_MODE };

enum TcudaLimit : unsigned int { cudaLimitStackSize, cudaLimitPrintfFifoSize };

struct DECLSPEC_DRECORD TCUdevprop
{
public:
	int maxThreadsPerBlock;
	System::StaticArray<int, 3> maxThreadsDim;
	System::StaticArray<int, 3> maxGridSize;
	int sharedMemPerBlock;
	int totalConstantMemory;
	int SIMDWidth;
	int memPitch;
	int regsPerBlock;
	int clockRate;
	int textureAlign;
};


enum TCUfunction_attribute : unsigned int { CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK, CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES, CU_FUNC_ATTRIBUTE_CONST_SIZE_BYTES, CU_FUNC_ATTRIBUTE_LOCAL_SIZE_BYTES, CU_FUNC_ATTRIBUTE_NUM_REGS, CU_FUNC_ATTRIBUTE_MAX };

enum TCUmemorytype : unsigned int { CU_MEMORYTYPE_HOST = 1, CU_MEMORYTYPE_DEVICE, CU_MEMORYTYPE_ARRAY };

enum TCUcomputemode : unsigned int { CU_COMPUTEMODE_DEFAULT, CU_COMPUTEMODE_EXCLUSIVE, CU_COMPUTEMODE_PROHIBITED };

enum TCUjit_option : unsigned int { CU_JIT_MAX_REGISTERS, CU_JIT_THREADS_PER_BLOCK, CU_JIT_WALL_TIME, CU_JIT_INFO_LOG_BUFFER, CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES, CU_JIT_ERROR_LOG_BUFFER, CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES, CU_JIT_OPTIMIZATION_LEVEL, CU_JIT_TARGET_FROM_CUCONTEXT, CU_JIT_TARGET, CU_JIT_FALLBACK_STRATEGY };

enum TCUjit_target : unsigned int { CU_TARGET_COMPUTE_10, CU_TARGET_COMPUTE_11, CU_TARGET_COMPUTE_12, CU_TARGET_COMPUTE_13 };

enum TCUjit_fallback : unsigned int { CU_PREFER_PTX, CU_PREFER_BINARY };

enum TCUgraphicsRegisterFlags : unsigned int { CU_GRAPHICS_REGISTER_FLAGS_NONE };

enum TCUgraphicsMapResourceFlags : unsigned int { CU_GRAPHICS_MAP_RESOURCE_FLAGS_NONE, CU_GRAPHICS_MAP_RESOURCE_FLAGS_READ_ONLY, CU_GRAPHICS_MAP_RESOURCE_FLAGS_WRITE_DISCARD };

enum TCUarray_cubemap_face : unsigned int { CU_CUBEMAP_FACE_POSITIVE_X, CU_CUBEMAP_FACE_NEGATIVE_X, CU_CUBEMAP_FACE_POSITIVE_Y, CU_CUBEMAP_FACE_NEGATIVE_Y, CU_CUBEMAP_FACE_POSITIVE_Z, CU_CUBEMAP_FACE_NEGATIVE_Z };

struct DECLSPEC_DRECORD TcudaFuncAttributes
{
public:
	NativeUInt sharedSizeBytes;
	NativeUInt constSizeBytes;
	NativeUInt localSizeBytes;
	int maxThreadsPerBlock;
	int numRegs;
	int ptxVersion;
	int binaryVersion;
	System::StaticArray<int, 6> __cudaReserved;
};


enum TcudaFuncCache : unsigned int { cudaFuncCachePreferNone, cudaFuncCachePreferShared, cudaFuncCachePreferL1 };

typedef unsigned TCUresult;

struct TCUDA_MEMCPY2D;
typedef TCUDA_MEMCPY2D *PCUDA_MEMCPY2D;

struct DECLSPEC_DRECORD TCUDA_MEMCPY2D
{
public:
	unsigned srcXInBytes;
	unsigned srcY;
	TCUmemorytype srcMemoryType;
	void *srcHost;
	void *srcDevice;
	TCUarray *srcArray;
	unsigned srcPitch;
	unsigned dstXInBytes;
	unsigned dstY;
	TCUmemorytype dstMemoryType;
	void *dstHost;
	void *dstDevice;
	TCUarray *dstArray;
	unsigned dstPitch;
	unsigned WidthInBytes;
	unsigned Height;
};


struct DECLSPEC_DRECORD TCUDA_MEMCPY3D
{
public:
	unsigned srcXInBytes;
	unsigned srcY;
	unsigned srcZ;
	unsigned srcLOD;
	TCUmemorytype srcMemoryType;
	void *srcHost;
	void *srcDevice;
	TCUarray *srcArray;
	void *reserved0;
	unsigned srcPitch;
	unsigned srcHeight;
	unsigned dstXInBytes;
	unsigned dstY;
	unsigned dstZ;
	unsigned dstLOD;
	TCUmemorytype dstMemoryType;
	void *dstHost;
	void *dstDevice;
	TCUarray *dstArray;
	void *reserved1;
	unsigned dstPitch;
	unsigned dstHeight;
	unsigned WidthInBytes;
	unsigned Height;
	unsigned Depth;
};


struct TCUDA_ARRAY_DESCRIPTOR;
typedef TCUDA_ARRAY_DESCRIPTOR *PCUDA_ARRAY_DESCRIPTOR;

struct DECLSPEC_DRECORD TCUDA_ARRAY_DESCRIPTOR
{
public:
	unsigned Width;
	unsigned Height;
	TCUarray_format Format;
	unsigned NumChannels;
};


struct DECLSPEC_DRECORD TCUDA_ARRAY3D_DESCRIPTOR
{
public:
	unsigned Width;
	unsigned Height;
	unsigned Depth;
	TCUarray_format Format;
	unsigned NumChannels;
	unsigned Flags;
};


enum TCUGLmap_flags : unsigned int { CU_GL_MAP_RESOURCE_FLAGS_NONE, CU_GL_MAP_RESOURCE_FLAGS_READ_ONLY, CU_GL_MAP_RESOURCE_FLAGS_WRITE_DISCARD };

typedef System::StaticArray<unsigned, 3> TDim3;

typedef void * HGPUNV;

typedef TCUresult __stdcall (*TcuInit)(unsigned Flags);

typedef TCUresult __stdcall (*TcuDriverGetVersion)(/* out */ int &driverVersion);

typedef TCUresult __stdcall (*TcuDeviceGet)(int &device, int ordinal);

typedef TCUresult __stdcall (*TcuDeviceGetCount)(int &count);

typedef TCUresult __stdcall (*TcuDeviceGetName)(char * name, int len, int dev);

typedef TCUresult __stdcall (*TcuDeviceComputeCapability)(int &major, int &minor, int dev);

typedef TCUresult __stdcall (*TcuDeviceTotalMem)(Gls_cl_platform::Psize_t bytes, int dev);

typedef TCUresult __stdcall (*TcuDeviceGetProperties)(TCUdevprop &prop, int dev);

typedef TCUresult __stdcall (*TcuDeviceGetAttribute)(Gls_cl_platform::Psize_t pi, TCUdevice_attribute attrib, int dev);

typedef TCUresult __stdcall (*TcuCtxCreate)(PCUcontext &pctx, unsigned Flags, int dev);

typedef TCUresult __stdcall (*TcuCtxDestroy)(PCUcontext ctx);

typedef TCUresult __stdcall (*TcuCtxAttach)(PCUcontext &pctx, unsigned Flags);

typedef TCUresult __stdcall (*TcuCtxDetach)(PCUcontext ctx);

typedef TCUresult __stdcall (*TcuCtxPushCurrent)(PCUcontext ctx);

typedef TCUresult __stdcall (*TcuCtxPopCurrent)(PCUcontext &pctx);

typedef TCUresult __stdcall (*TcuCtxGetDevice)(int &device);

typedef TCUresult __stdcall (*TcuCtxSynchronize)(void);

typedef TCUresult __stdcall (*TcuModuleLoad)(PCUmodule &module, const char * fname);

typedef TCUresult __stdcall (*TcuModuleLoadData)(PCUmodule &module, const char * image);

typedef TCUresult __stdcall (*TcuModuleLoadDataEx)(PCUmodule &module, void *image, unsigned numOptions, TCUjit_option &options, void *optionValues);

typedef TCUresult __stdcall (*TcuModuleLoadFatBinary)(PCUmodule &module, void *fatCubin);

typedef TCUresult __stdcall (*TcuModuleUnload)(PCUmodule hmod);

typedef TCUresult __stdcall (*TcuModuleGetFunction)(/* out */ PCUfunction &hfunc, PCUmodule hmod, const char * name);

typedef TCUresult __stdcall (*TcuModuleGetGlobal)(/* out */ void * &dptr, unsigned &bytes, PCUmodule hmod, const char * name);

typedef TCUresult __stdcall (*TcuModuleGetTexRef)(/* out */ PCUtexref &pTexRef, PCUmodule hmod, const char * name);

typedef TCUresult __stdcall (*TcuMemGetInfo)(unsigned &free, unsigned &total);

typedef TCUresult __stdcall (*TcuMemAlloc)(void * &dptr, unsigned bytesize);

typedef TCUresult __stdcall (*TcuMemAllocPitch)(void * &dptr, unsigned &pPitch, unsigned WidthInBytes, unsigned Height, unsigned ElementSizeBytes);

typedef TCUresult __stdcall (*TcuMemFree)(void * dptr);

typedef TCUresult __stdcall (*TcuMemGetAddressRange)(void * &pbase, unsigned &psize, void * dptr);

typedef TCUresult __stdcall (*TcuMemAllocHost)(void *pp, unsigned bytesize);

typedef TCUresult __stdcall (*TcuMemFreeHost)(void * p);

typedef TCUresult __stdcall (*TcuMemHostAlloc)(void * &pp, unsigned bytesize, unsigned Flags);

typedef TCUresult __stdcall (*TcuMemHostGetDevicePointer)(void * &pdptr, void * p, unsigned Flags);

typedef TCUresult __stdcall (*TcuMemHostGetFlags)(unsigned &pFlags, void *p);

typedef TCUresult __stdcall (*TcuMemcpyHtoD)(void * dstDevice, const void * srcHost, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyDtoH)(const void * dstHost, void * srcDevice, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyDtoD)(void * dstDevice, void * srcDevice, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyDtoDAsync)(void * dstDevice, void * srcDevice, unsigned ByteCount, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemcpyDtoA)(PCUarray dstArray, unsigned dstIndex, void * srcDevice, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyAtoD)(void * dstDevice, PCUarray hSrc, unsigned SrcIndex, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyHtoA)(PCUarray dstArray, unsigned dstIndex, void * pSrc, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyAtoH)(void * dstHost, PCUarray srcArray, unsigned SrcIndex, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpyAtoA)(PCUarray dstArray, unsigned dstIndex, PCUarray srcArray, unsigned SrcIndex, unsigned ByteCount);

typedef TCUresult __stdcall (*TcuMemcpy2D)(const PCUDA_MEMCPY2D pCopy);

typedef TCUresult __stdcall (*TcuMemcpy2DUnaligned)(TCUDA_MEMCPY2D &pCopy);

typedef TCUresult __stdcall (*TcuMemcpy3D)(TCUDA_MEMCPY3D &pCopy);

typedef TCUresult __stdcall (*TcuMemcpyHtoDAsync)(void * dstDevice, void *srcHost, unsigned ByteCount, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemcpyDtoHAsync)(void *dstHost, void * srcDevice, unsigned ByteCount, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemcpyHtoAAsync)(PCUarray dstArray, unsigned dstIndex, void *pSrc, unsigned ByteCount, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemcpyAtoHAsync)(void *dstHost, PCUstream srcArray, unsigned SrcIndex, unsigned ByteCount, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemcpy2DAsync)(TCUDA_MEMCPY2D &pCopy, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemcpy3DAsync)(TCUDA_MEMCPY3D &pCopy, PCUstream hStream);

typedef TCUresult __stdcall (*TcuMemsetD8)(void * dstDevice, System::Byte ub, unsigned N);

typedef TCUresult __stdcall (*TcuMemsetD16)(void * dstDevice, System::Word uw, unsigned N);

typedef TCUresult __stdcall (*TcuMemsetD32)(void * dstDevice, unsigned ui, unsigned N);

typedef TCUresult __stdcall (*TcuMemsetD2D8)(void * dstDevice, unsigned dstPitch, System::Byte ub, unsigned Width, unsigned Height);

typedef TCUresult __stdcall (*TcuMemsetD2D16)(void * dstDevice, unsigned dstPitch, System::Word uw, unsigned Width, unsigned Height);

typedef TCUresult __stdcall (*TcuMemsetD2D32)(void * dstDevice, unsigned dstPitch, unsigned ui, unsigned Width, unsigned Height);

typedef TCUresult __stdcall (*TcuFuncSetBlockShape)(PCUfunction hfunc, int x, int y, int z);

typedef TCUresult __stdcall (*TcuFuncSetSharedSize)(PCUfunction hfunc, unsigned bytes);

typedef TCUresult __stdcall (*TcuFuncGetAttribute)(int &pi, TCUfunction_attribute attrib, PCUfunction hfunc);

typedef TCUresult __stdcall (*TcuArrayCreate)(PCUarray &pHandle, TCUDA_ARRAY_DESCRIPTOR &pAllocateArray);

typedef TCUresult __stdcall (*TcuArrayGetDescriptor)(TCUDA_ARRAY_DESCRIPTOR &pArrayDescriptor, PCUarray hArray);

typedef TCUresult __stdcall (*TcuArrayDestroy)(PCUarray hArray);

typedef TCUresult __stdcall (*TcuArray3DCreate)(PCUarray &pHandle, TCUDA_ARRAY3D_DESCRIPTOR &pAllocateArray);

typedef TCUresult __stdcall (*TcuArray3DGetDescriptor)(TCUDA_ARRAY3D_DESCRIPTOR &pArrayDescriptor, PCUarray hArray);

typedef TCUresult __stdcall (*TcuTexRefCreate)(PCUtexref &pTexRef);

typedef TCUresult __stdcall (*TcuTexRefDestroy)(PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuTexRefSetArray)(PCUtexref hTexRef, PCUarray hArray, unsigned Flags);

typedef TCUresult __stdcall (*TcuTexRefSetAddress)(unsigned &ByteOffset, PCUtexref hTexRef, void * dptr, unsigned bytes);

typedef TCUresult __stdcall (*TcuTexRefSetAddress2D)(PCUtexref hTexRef, TCUDA_ARRAY_DESCRIPTOR &desc, void * dptr, unsigned Pitch);

typedef TCUresult __stdcall (*TcuTexRefSetFormat)(PCUtexref hTexRef, TCUarray_format fmt, int NumPackedComponents);

typedef TCUresult __stdcall (*TcuTexRefSetAddressMode)(PCUtexref hTexRef, int dim, TCUaddress_mode am);

typedef TCUresult __stdcall (*TcuTexRefSetFilterMode)(PCUtexref hTexRef, TCUfilter_mode fm);

typedef TCUresult __stdcall (*TcuTexRefSetFlags)(PCUtexref hTexRef, unsigned Flags);

typedef TCUresult __stdcall (*TcuTexRefGetAddress)(void * &pdptr, PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuTexRefGetArray)(PCUarray &phArray, PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuTexRefGetAddressMode)(TCUaddress_mode &pam, PCUtexref hTexRef, int dim);

typedef TCUresult __stdcall (*TcuTexRefGetFilterMode)(TCUfilter_mode &pfm, PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuTexRefGetFormat)(TCUarray_format &pFormat, int &pNumChannels, PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuTexRefGetFlags)(unsigned &pFlags, PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuParamSetSize)(PCUfunction hfunc, unsigned numbytes);

typedef TCUresult __stdcall (*TcuParamSeti)(PCUfunction hfunc, int offset, unsigned value);

typedef TCUresult __stdcall (*TcuParamSetf)(PCUfunction hfunc, int offset, float value);

typedef TCUresult __stdcall (*TcuParamSetv)(PCUfunction hfunc, int offset, void *ptr, unsigned numbytes);

typedef TCUresult __stdcall (*TcuParamSetTexRef)(PCUfunction hfunc, int texunit, PCUtexref hTexRef);

typedef TCUresult __stdcall (*TcuLaunch)(PCUfunction f);

typedef TCUresult __stdcall (*TcuLaunchGrid)(PCUfunction f, int grid_width, int grid_height);

typedef TCUresult __stdcall (*TcuLaunchGridAsync)(PCUfunction f, int grid_width, int grid_height, PCUstream hStream);

typedef TCUresult __stdcall (*TcuEventCreate)(PCUevent &phEvent, unsigned Flags);

typedef TCUresult __stdcall (*TcuEventRecord)(PCUevent hEvent, PCUstream hStream);

typedef TCUresult __stdcall (*TcuEventQuery)(PCUevent hEvent);

typedef TCUresult __stdcall (*TcuEventSynchronize)(PCUevent hEvent);

typedef TCUresult __stdcall (*TcuEventDestroy)(PCUevent hEvent);

typedef TCUresult __stdcall (*TcuEventElapsedTime)(float &pMilliseconds, PCUevent hStart, PCUevent hEnd);

typedef TCUresult __stdcall (*TcuStreamCreate)(PCUstream &phStream, unsigned Flags);

typedef TCUresult __stdcall (*TcuStreamQuery)(PCUstream hStream);

typedef TCUresult __stdcall (*TcuStreamSynchronize)(PCUstream hStream);

typedef TCUresult __stdcall (*TcuStreamDestroy)(PCUstream hStream);

typedef TCUresult __stdcall (*TcuGLCtxCreate)(PCUcontext &pctx, unsigned Flags, int device);

typedef TCUresult __stdcall (*TcuGraphicsGLRegisterBuffer)(PCUgraphicsResource &pCudaResource, unsigned buffer, TCUgraphicsMapResourceFlags Flags);

typedef TCUresult __stdcall (*TcuGraphicsGLRegisterImage)(PCUgraphicsResource &pCudaResource, unsigned image, unsigned target, TCUgraphicsMapResourceFlags Flags);

typedef TCUresult __stdcall (*TcuWGLGetDevice)(int &pDevice, void * hGpu);

typedef TCUresult __stdcall (*TcuGraphicsUnregisterResource)(PCUgraphicsResource resource);

typedef TCUresult __stdcall (*TcuGraphicsSubResourceGetMappedArray)(PCUarray &pArray, PCUgraphicsResource resource, unsigned arrayIndex, unsigned mipLevel);

typedef TCUresult __stdcall (*TcuGraphicsResourceGetMappedPointer)(void * &pDevPtr, /* out */ unsigned &psize, PCUgraphicsResource resource);

typedef TCUresult __stdcall (*TcuGraphicsResourceSetMapFlags)(PCUgraphicsResource resource, unsigned Flags);

typedef TCUresult __stdcall (*TcuGraphicsMapResources)(unsigned count, PPCUgraphicsResource resources, PCUstream hStream);

typedef TCUresult __stdcall (*TcuGraphicsUnmapResources)(unsigned count, PPCUgraphicsResource resources, PCUstream hStream);

typedef void __stdcall (*TcuGLInit)(void);

typedef TCUresult __stdcall (*TcuGLRegisterBufferObject)(unsigned buffer);

typedef TCUresult __stdcall (*TcuGLMapBufferObject)(void * &dptr, unsigned &size, unsigned buffer);

typedef TCUresult __stdcall (*TcuGLUnmapBufferObject)(unsigned buffer);

typedef TCUresult __stdcall (*TcuGLUnregisterBufferObject)(unsigned buffer);

typedef TCUresult __stdcall (*TcuGLSetBufferObjectMapFlags)(unsigned buffer, unsigned Flags);

typedef TCUresult __stdcall (*TcuGLMapBufferObjectAsync)(void * &dptr, unsigned &size, unsigned buffer, PCUstream hStream);

typedef TCUresult __stdcall (*TcuGLUnmapBufferObjectAsync)(unsigned buffer, PCUstream hStream);

//-- var, const, procedure ---------------------------------------------------
#define CUDAAPIDLL L"nvcuda.dll"
extern PACKAGE TCUresult CUDA_SUCCESS;
static const System::Int8 CUDA_ERROR_INVALID_VALUE = System::Int8(0x1);
static const System::Int8 CUDA_ERROR_OUT_OF_MEMORY = System::Int8(0x2);
static const System::Int8 CUDA_ERROR_NOT_INITIALIZED = System::Int8(0x3);
static const System::Int8 CUDA_ERROR_DEINITIALIZED = System::Int8(0x4);
static const System::Int8 CUDA_ERROR_NO_DEVICE = System::Int8(0x64);
static const System::Int8 CUDA_ERROR_INVALID_DEVICE = System::Int8(0x65);
static const System::Byte CUDA_ERROR_INVALID_IMAGE = System::Byte(0xc8);
static const System::Byte CUDA_ERROR_INVALID_CONTEXT = System::Byte(0xc9);
static const System::Byte CUDA_ERROR_CONTEXT_ALREADY_CURRENT = System::Byte(0xca);
static const System::Byte CUDA_ERROR_MAP_FAILED = System::Byte(0xcd);
static const System::Byte CUDA_ERROR_UNMAP_FAILED = System::Byte(0xce);
static const System::Byte CUDA_ERROR_ARRAY_IS_MAPPED = System::Byte(0xcf);
static const System::Byte CUDA_ERROR_ALREADY_MAPPED = System::Byte(0xd0);
static const System::Byte CUDA_ERROR_NO_BINARY_FOR_GPU = System::Byte(0xd1);
static const System::Byte CUDA_ERROR_ALREADY_ACQUIRED = System::Byte(0xd2);
static const System::Byte CUDA_ERROR_NOT_MAPPED = System::Byte(0xd3);
static const System::Byte CUDA_ERROR_NOT_MAPPED_AS_ARRAY = System::Byte(0xd4);
static const System::Byte CUDA_ERROR_NOT_MAPPED_AS_POINTER = System::Byte(0xd5);
static const System::Word CUDA_ERROR_INVALID_SOURCE = System::Word(0x12c);
static const System::Word CUDA_ERROR_FILE_NOT_FOUND = System::Word(0x12d);
static const System::Word CUDA_ERROR_INVALID_HANDLE = System::Word(0x190);
static const System::Word CUDA_ERROR_NOT_FOUND = System::Word(0x1f4);
static const System::Word CUDA_ERROR_NOT_READY = System::Word(0x258);
static const System::Word CUDA_ERROR_LAUNCH_FAILED = System::Word(0x2bc);
static const System::Word CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES = System::Word(0x2bd);
static const System::Word CUDA_ERROR_LAUNCH_TIMEOUT = System::Word(0x2be);
static const System::Word CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING = System::Word(0x2bf);
static const System::Word CUDA_ERROR_POINTER_IS_64BIT = System::Word(0x320);
static const System::Word CUDA_ERROR_SIZE_IS_64BIT = System::Word(0x321);
static const System::Word CUDA_ERROR_UNKNOWN = System::Word(0x3e7);
static const System::Int8 CU_MEMHOSTALLOC_PORTABLE = System::Int8(0x1);
static const System::Int8 CU_MEMHOSTALLOC_DEVICEMAP = System::Int8(0x2);
static const System::Int8 CU_MEMHOSTALLOC_WRITECOMBINED = System::Int8(0x4);
static const System::Int8 CU_TRSA_OVERRIDE_FORMAT = System::Int8(0x1);
static const System::Int8 CU_TRSF_READ_AS_INTEGER = System::Int8(0x1);
static const System::Int8 CU_TRSF_NORMALIZED_COORDINATES = System::Int8(0x2);
static const System::Int8 CU_PARAM_TR_DEFAULT = System::Int8(-1);
extern PACKAGE TcuInit cuInit;
extern PACKAGE TcuDriverGetVersion cuDriverGetVersion;
extern PACKAGE TcuDeviceGet cuDeviceGet;
extern PACKAGE TcuDeviceGetCount cuDeviceGetCount;
extern PACKAGE TcuDeviceGetName cuDeviceGetName;
extern PACKAGE TcuDeviceComputeCapability cuDeviceComputeCapability;
extern PACKAGE TcuDeviceTotalMem cuDeviceTotalMem;
extern PACKAGE TcuDeviceGetProperties cuDeviceGetProperties;
extern PACKAGE TcuDeviceGetAttribute cuDeviceGetAttribute;
extern PACKAGE TcuCtxCreate cuCtxCreate;
extern PACKAGE TcuCtxDestroy cuCtxDestroy;
extern PACKAGE TcuCtxAttach cuCtxAttach;
extern PACKAGE TcuCtxDetach cuCtxDetach;
extern PACKAGE TcuCtxPushCurrent cuCtxPushCurrent;
extern PACKAGE TcuCtxPopCurrent cuCtxPopCurrent;
extern PACKAGE TcuCtxGetDevice cuCtxGetDevice;
extern PACKAGE TcuCtxSynchronize cuCtxSynchronize;
extern PACKAGE TcuModuleLoad cuModuleLoad;
extern PACKAGE TcuModuleLoadData cuModuleLoadData;
extern PACKAGE TcuModuleLoadDataEx cuModuleLoadDataEx;
extern PACKAGE TcuModuleLoadFatBinary cuModuleLoadFatBinary;
extern PACKAGE TcuModuleUnload cuModuleUnload;
extern PACKAGE TcuModuleGetFunction cuModuleGetFunction;
extern PACKAGE TcuModuleGetGlobal cuModuleGetGlobal;
extern PACKAGE TcuModuleGetTexRef cuModuleGetTexRef;
extern PACKAGE TcuMemGetInfo cuMemGetInfo;
extern PACKAGE TcuMemAlloc cuMemAlloc;
extern PACKAGE TcuMemAllocPitch cuMemAllocPitch;
extern PACKAGE TcuMemFree cuMemFree;
extern PACKAGE TcuMemGetAddressRange cuMemGetAddressRange;
extern PACKAGE TcuMemAllocHost cuMemAllocHost;
extern PACKAGE TcuMemFreeHost cuMemFreeHost;
extern PACKAGE TcuMemHostAlloc cuMemHostAlloc;
extern PACKAGE TcuMemHostGetDevicePointer cuMemHostGetDevicePointer;
extern PACKAGE TcuMemHostGetFlags cuMemHostGetFlags;
extern PACKAGE TcuMemcpyHtoD cuMemcpyHtoD;
extern PACKAGE TcuMemcpyDtoH cuMemcpyDtoH;
extern PACKAGE TcuMemcpyDtoD cuMemcpyDtoD;
extern PACKAGE TcuMemcpyDtoDAsync cuMemcpyDtoDAsync;
extern PACKAGE TcuMemcpyDtoA cuMemcpyDtoA;
extern PACKAGE TcuMemcpyAtoD cuMemcpyAtoD;
extern PACKAGE TcuMemcpyHtoA cuMemcpyHtoA;
extern PACKAGE TcuMemcpyAtoH cuMemcpyAtoH;
extern PACKAGE TcuMemcpyAtoA cuMemcpyAtoA;
extern PACKAGE TcuMemcpy2D cuMemcpy2D;
extern PACKAGE TcuMemcpy2DUnaligned cuMemcpy2DUnaligned;
extern PACKAGE TcuMemcpy3D cuMemcpy3D;
extern PACKAGE TcuMemcpyHtoDAsync cuMemcpyHtoDAsync;
extern PACKAGE TcuMemcpyDtoHAsync cuMemcpyDtoHAsync;
extern PACKAGE TcuMemcpyHtoAAsync cuMemcpyHtoAAsync;
extern PACKAGE TcuMemcpyAtoHAsync cuMemcpyAtoHAsync;
extern PACKAGE TcuMemcpy2DAsync cuMemcpy2DAsync;
extern PACKAGE TcuMemcpy3DAsync cuMemcpy3DAsync;
extern PACKAGE TcuMemsetD8 cuMemsetD8;
extern PACKAGE TcuMemsetD16 cuMemsetD16;
extern PACKAGE TcuMemsetD32 cuMemsetD32;
extern PACKAGE TcuMemsetD2D8 cuMemsetD2D8;
extern PACKAGE TcuMemsetD2D16 cuMemsetD2D16;
extern PACKAGE TcuMemsetD2D32 cuMemsetD2D32;
extern PACKAGE TcuFuncSetBlockShape cuFuncSetBlockShape;
extern PACKAGE TcuFuncSetSharedSize cuFuncSetSharedSize;
extern PACKAGE TcuFuncGetAttribute cuFuncGetAttribute;
extern PACKAGE TcuArrayCreate cuArrayCreate;
extern PACKAGE TcuArrayGetDescriptor cuArrayGetDescriptor;
extern PACKAGE TcuArrayDestroy cuArrayDestroy;
extern PACKAGE TcuArray3DCreate cuArray3DCreate;
extern PACKAGE TcuArray3DGetDescriptor cuArray3DGetDescriptor;
extern PACKAGE TcuTexRefCreate cuTexRefCreate;
extern PACKAGE TcuTexRefDestroy cuTexRefDestroy;
extern PACKAGE TcuTexRefSetArray cuTexRefSetArray;
extern PACKAGE TcuTexRefSetAddress cuTexRefSetAddress;
extern PACKAGE TcuTexRefSetAddress2D cuTexRefSetAddress2D;
extern PACKAGE TcuTexRefSetFormat cuTexRefSetFormat;
extern PACKAGE TcuTexRefSetAddressMode cuTexRefSetAddressMode;
extern PACKAGE TcuTexRefSetFilterMode cuTexRefSetFilterMode;
extern PACKAGE TcuTexRefSetFlags cuTexRefSetFlags;
extern PACKAGE TcuTexRefGetAddress cuTexRefGetAddress;
extern PACKAGE TcuTexRefGetArray cuTexRefGetArray;
extern PACKAGE TcuTexRefGetAddressMode cuTexRefGetAddressMode;
extern PACKAGE TcuTexRefGetFilterMode cuTexRefGetFilterMode;
extern PACKAGE TcuTexRefGetFormat cuTexRefGetFormat;
extern PACKAGE TcuTexRefGetFlags cuTexRefGetFlags;
extern PACKAGE TcuParamSetSize cuParamSetSize;
extern PACKAGE TcuParamSeti cuParamSeti;
extern PACKAGE TcuParamSetf cuParamSetf;
extern PACKAGE TcuParamSetv cuParamSetv;
extern PACKAGE TcuParamSetTexRef cuParamSetTexRef;
extern PACKAGE TcuLaunch cuLaunch;
extern PACKAGE TcuLaunchGrid cuLaunchGrid;
extern PACKAGE TcuLaunchGridAsync cuLaunchGridAsync;
extern PACKAGE TcuEventCreate cuEventCreate;
extern PACKAGE TcuEventRecord cuEventRecord;
extern PACKAGE TcuEventQuery cuEventQuery;
extern PACKAGE TcuEventSynchronize cuEventSynchronize;
extern PACKAGE TcuEventDestroy cuEventDestroy;
extern PACKAGE TcuEventElapsedTime cuEventElapsedTime;
extern PACKAGE TcuStreamCreate cuStreamCreate;
extern PACKAGE TcuStreamQuery cuStreamQuery;
extern PACKAGE TcuStreamSynchronize cuStreamSynchronize;
extern PACKAGE TcuStreamDestroy cuStreamDestroy;
extern PACKAGE TcuGLInit cuGLInit;
extern PACKAGE TcuGLCtxCreate cuGLCtxCreate;
extern PACKAGE TcuGraphicsGLRegisterBuffer cuGraphicsGLRegisterBuffer;
extern PACKAGE TcuGraphicsGLRegisterImage cuGraphicsGLRegisterImage;
extern PACKAGE TcuWGLGetDevice cuWGLGetDevice;
extern PACKAGE TcuGraphicsUnregisterResource cuGraphicsUnregisterResource;
extern PACKAGE TcuGraphicsSubResourceGetMappedArray cuGraphicsSubResourceGetMappedArray;
extern PACKAGE TcuGraphicsResourceGetMappedPointer cuGraphicsResourceGetMappedPointer;
extern PACKAGE TcuGraphicsResourceSetMapFlags cuGraphicsResourceSetMapFlags;
extern PACKAGE TcuGraphicsMapResources cuGraphicsMapResources;
extern PACKAGE TcuGraphicsUnmapResources cuGraphicsUnmapResources;
extern PACKAGE TcuGLRegisterBufferObject cuGLRegisterBufferObject;
extern PACKAGE TcuGLMapBufferObject cuGLMapBufferObject;
extern PACKAGE TcuGLUnmapBufferObject cuGLUnmapBufferObject;
extern PACKAGE TcuGLUnregisterBufferObject cuGLUnregisterBufferObject;
extern PACKAGE TcuGLSetBufferObjectMapFlags cuGLSetBufferObjectMapFlags;
extern PACKAGE TcuGLMapBufferObjectAsync cuGLMapBufferObjectAsync;
extern PACKAGE TcuGLUnmapBufferObjectAsync cuGLUnmapBufferObjectAsync;
extern PACKAGE bool __fastcall InitCUDA(void);
extern PACKAGE void __fastcall CloseCUDA(void);
extern PACKAGE bool __fastcall InitCUDAFromLibrary(const System::WideString LibName);
extern PACKAGE bool __fastcall IsCUDAInitialized(void);
extern PACKAGE System::UnicodeString __fastcall Get_CUDA_API_Error_String(TCUresult AError);
}	/* namespace Gls_cuda_api */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_CUDA_API)
using namespace Gls_cuda_api;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_cuda_apiHPP
