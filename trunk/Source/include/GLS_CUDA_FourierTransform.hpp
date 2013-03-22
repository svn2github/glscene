// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS_CUDA_FourierTransform.pas' rev: 24.00 (Win32)

#ifndef Gls_cuda_fouriertransformHPP
#define Gls_cuda_fouriertransformHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <GLS_CUDA_API.hpp>	// Pascal unit
#include <GLS_CUDA_Runtime.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gls_cuda_fouriertransform
{
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<System::UnicodeString, 10> Gls_cuda_fouriertransform__1;

typedef unsigned TcufftHandle;

typedef float TcufftReal;

typedef float *PcufftReal;

typedef float TcufftRealfloat;

typedef double *PcufftDoubleReal;

typedef double TcufftDoubleReal;

typedef Vectortypes::TVector2d *PcufftDoubleComplex;

typedef Vectortypes::TVector2d TcufftDoubleComplex;

typedef Vectortypes::TVector2f *PcufftComplex;

typedef Vectortypes::TVector2f TcufftComplex;

typedef System::Byte TcufftResult;

typedef unsigned TcufftType;

enum TcudaRoundMode : unsigned int { cudaRoundNearest, cudaRoundZero, cudaRoundPosInf, cudaRoundMinInf };

typedef unsigned TcufftCompatibility;

typedef TcufftResult __stdcall (*TcufftPlan1d)(/* out */ TcufftHandle &plan, int nx, TcufftType atype, int batch);

typedef TcufftResult __stdcall (*TcufftPlan2d)(/* out */ TcufftHandle &plan, int nx, int ny, TcufftType atype);

typedef TcufftResult __stdcall (*TcufftPlan3d)(/* out */ TcufftHandle &plan, int nx, int ny, int nz, TcufftType atype);

typedef TcufftResult __stdcall (*TcufftDestroy)(TcufftHandle plan);

typedef TcufftResult __stdcall (*TcufftPlanMany)(/* out */ TcufftHandle &plan, int rank, int &n, int &inembed, int istride, int idist, int &onembed, int ostride, int odist, TcufftType ctype, int batch);

typedef TcufftResult __stdcall (*TcufftExecC2C)(TcufftHandle plan, PcufftComplex idata, PcufftComplex odata, int direction);

typedef TcufftResult __stdcall (*TcufftExecR2C)(TcufftHandle plan, PcufftReal idata, PcufftComplex odata);

typedef TcufftResult __stdcall (*TcufftExecC2R)(TcufftHandle plan, PcufftComplex idata, PcufftReal odata);

typedef TcufftResult __stdcall (*TcufftExecZ2Z)(TcufftHandle plan, PcufftDoubleComplex idata, PcufftDoubleComplex odata, int direction);

typedef TcufftResult __stdcall (*TcufftExecD2Z)(TcufftHandle plan, PcufftDoubleReal idata, PcufftDoubleComplex odata);

typedef TcufftResult __stdcall (*TcufftExecZ2D)(TcufftHandle plan, PcufftDoubleComplex idata, PcufftDoubleReal odata);

typedef TcufftResult __stdcall (*TcufftSetStream)(TcufftHandle p, int stream);

typedef TcufftResult __stdcall (*TcufftSetCompatibilityMode)(TcufftHandle plan, TcufftCompatibility mode);

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Gls_cuda_fouriertransform__1 CUFFTDLLNAMES;
static const unsigned INVALID_CUFFT_HANDLE = unsigned(0xffffffff);
extern PACKAGE TcufftResult CUFFT_SUCCESS;
extern PACKAGE TcufftResult CUFFT_INVALID_PLAN;
extern PACKAGE TcufftResult CUFFT_ALLOC_FAILED;
extern PACKAGE TcufftResult CUFFT_INVALID_TYPE;
extern PACKAGE TcufftResult CUFFT_INVALID_VALUE;
extern PACKAGE TcufftResult CUFFT_INTERNAL_ERROR;
extern PACKAGE TcufftResult CUFFT_EXEC_FAILED;
extern PACKAGE TcufftResult CUFFT_SETUP_FAILED;
extern PACKAGE TcufftResult CUFFT_INVALID_SIZE;
static const System::Int8 CUFFT_FORWARD = System::Int8(-1);
static const System::Int8 CUFFT_INVERSE = System::Int8(0x1);
extern PACKAGE TcufftType CUFFT_R2C;
extern PACKAGE TcufftType CUFFT_C2R;
extern PACKAGE TcufftType CUFFT_C2C;
extern PACKAGE TcufftType CUFFT_D2Z;
extern PACKAGE TcufftType CUFFT_Z2D;
extern PACKAGE TcufftType CUFFT_Z2Z;
extern PACKAGE TcufftCompatibility CUFFT_COMPATIBILITY_NORMAL;
extern PACKAGE TcufftCompatibility CUFFT_COMPATIBILITY_FFTW_PADDING;
extern PACKAGE TcufftCompatibility CUFFT_COMPATIBILITY_FFTW_C2R_ASYMMETRIC;
extern PACKAGE TcufftCompatibility CUFFT_COMPATIBILITY_FFTW;
extern PACKAGE TcufftPlan1d cufftPlan1d;
extern PACKAGE TcufftPlan2d cufftPlan2d;
extern PACKAGE TcufftPlan3d cufftPlan3d;
extern PACKAGE TcufftDestroy cufftDestroy;
extern PACKAGE TcufftPlanMany cufftPlanMany;
extern PACKAGE TcufftExecC2C cufftExecC2C;
extern PACKAGE TcufftExecR2C cufftExecR2C;
extern PACKAGE TcufftExecC2R cufftExecC2R;
extern PACKAGE TcufftExecZ2Z cufftExecZ2Z;
extern PACKAGE TcufftExecD2Z cufftExecD2Z;
extern PACKAGE TcufftExecZ2D cufftExecZ2D;
extern PACKAGE TcufftSetStream cufftSetStream;
extern PACKAGE TcufftSetCompatibilityMode cufftSetCompatibilityMode;
extern PACKAGE bool __fastcall InitCUFFT(void);
extern PACKAGE void __fastcall CloseCUFFT(void);
extern PACKAGE bool __fastcall InitCUFFTFromLibrary(const System::WideString LibName);
extern PACKAGE bool __fastcall IsCUFFTInitialized(void);
extern PACKAGE System::UnicodeString __fastcall Get_CUDA_FFT_Error_String(TcufftResult AError);
}	/* namespace Gls_cuda_fouriertransform */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_CUDA_FOURIERTRANSFORM)
using namespace Gls_cuda_fouriertransform;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_cuda_fouriertransformHPP
