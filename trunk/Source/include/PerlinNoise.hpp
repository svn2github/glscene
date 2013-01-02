// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PerlinNoise.pas' rev: 24.00 (Win32)

#ifndef PerlinnoiseHPP
#define PerlinnoiseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Perlinnoise
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TPerlin3DNoise;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TPerlin3DNoise : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	System::StaticArray<int, 256> FPermutations;
	System::StaticArray<float, 768> FGradients;
	float __fastcall Lattice(int ix, int iy, int iz, float fx, float fy, float fz)/* overload */;
	float __fastcall Lattice(int ix, int iy, float fx, float fy)/* overload */;
	
public:
	__fastcall TPerlin3DNoise(int randomSeed);
	void __fastcall Initialize(int randomSeed);
	float __fastcall Noise(const float x, const float y)/* overload */;
	float __fastcall Noise(const float x, const float y, const float z)/* overload */;
	float __fastcall Noise(const Vectortypes::TVector3f &v)/* overload */;
	float __fastcall Noise(const Vectortypes::TVector4f &v)/* overload */;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TPerlin3DNoise(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Word cPERLIN_TABLE_SIZE = System::Word(0x100);
}	/* namespace Perlinnoise */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_PERLINNOISE)
using namespace Perlinnoise;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PerlinnoiseHPP
