// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSCUDAFFTPlan.pas' rev: 24.00 (Win32)

#ifndef GlscudafftplanHPP
#define GlscudafftplanHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLSCUDAContext.hpp>	// Pascal unit
#include <GLSCUDA.hpp>	// Pascal unit
#include <GLS_CUDA_API.hpp>	// Pascal unit
#include <GLS_CUDA_FourierTransform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glscudafftplan
{
//-- type declarations -------------------------------------------------------
enum TCUDAFFTransform : unsigned char { fftRealToComplex, fftComplexToReal, fftComplexToComplex, fftDoubleToDoubleComplex, fftDoubleComplexToDouble, fftDoubleComplexToDoubleComplex };

enum TCUDAFFTdir : unsigned char { fftdForward, fftdInverse };

class DELPHICLASS TCUDAFFTPlan;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCUDAFFTPlan : public Glscuda::TCUDAComponent
{
	typedef Glscuda::TCUDAComponent inherited;
	
private:
	Gls_cuda_fouriertransform::TcufftHandle FHandle;
	int FWidth;
	int FHeight;
	int FDepth;
	int FBatch;
	int FSize;
	int FPaddedSize;
	TCUDAFFTransform FTransform;
	Gls_cuda_fouriertransform::TcufftResult FStatus;
	void __fastcall SetWidth(int Value);
	void __fastcall SetHeight(int Value);
	void __fastcall SetDepth(int Value);
	void __fastcall SetBatch(int Value);
	void __fastcall SetTransform(TCUDAFFTransform Value);
	
protected:
	virtual void __fastcall AllocateHandles(void);
	virtual void __fastcall DestroyHandles(void);
	__classmethod void __fastcall CheckLib();
	
public:
	__fastcall virtual TCUDAFFTPlan(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCUDAFFTPlan(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Execute(Glscuda::TCUDAMemData* ASrc, Glscuda::TCUDAMemData* ADst, const TCUDAFFTdir ADir = (TCUDAFFTdir)(0x0));
	
__published:
	__property int Width = {read=FWidth, write=SetWidth, default=256};
	__property int Height = {read=FHeight, write=SetHeight, default=0};
	__property int Depth = {read=FDepth, write=SetDepth, default=0};
	__property int Batch = {read=FBatch, write=SetBatch, default=1};
	__property TCUDAFFTransform Transform = {read=FTransform, write=SetTransform, default=0};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glscudafftplan */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCUDAFFTPLAN)
using namespace Glscudafftplan;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlscudafftplanHPP
