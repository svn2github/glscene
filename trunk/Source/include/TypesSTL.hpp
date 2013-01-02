// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TypesSTL.pas' rev: 24.00 (Win32)

#ifndef TypesstlHPP
#define TypesstlHPP

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

namespace Typesstl
{
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TSTLHeader
{
public:
	System::StaticArray<System::Byte, 80> dummy;
	int nbFaces;
};
#pragma pack(pop)


typedef Vectortypes::TVector3f TSTLVertex;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TSTLFace
{
public:
	Vectortypes::TVector3f normal;
	Vectortypes::TVector3f v1;
	Vectortypes::TVector3f v2;
	Vectortypes::TVector3f v3;
	System::StaticArray<System::Byte, 2> padding;
};
#pragma pack(pop)


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Typesstl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_TYPESSTL)
using namespace Typesstl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TypesstlHPP
