// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FileNMF.pas' rev: 24.00 (Win32)

#ifndef FilenmfHPP
#define FilenmfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Filenmf
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TNmHeader
{
public:
	System::StaticArray<char, 4> hdr;
	unsigned size;
};


struct DECLSPEC_DRECORD TNmRawTriangle
{
public:
	System::StaticArray<Vectortypes::TVector3f, 3> vert;
	System::StaticArray<Vectortypes::TVector3f, 3> norm;
	System::StaticArray<Vectorgeometry::TTexPoint, 3> texCoord;
};


class DELPHICLASS TFileNMF;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFileNMF : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<TNmRawTriangle> _TFileNMF__1;
	
	
public:
	TNmHeader FileHeader;
	TNmHeader TrisHeader;
	int NumTris;
	_TFileNMF__1 RawTriangles;
	void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	void __fastcall SaveToStream(System::Classes::TStream* Stream);
public:
	/* TObject.Create */ inline __fastcall TFileNMF(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TFileNMF(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define NMF_HEADER_TAG L"NMF "
#define NMF_TRIANGLE_TAG L"TRIS"
}	/* namespace Filenmf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FILENMF)
using namespace Filenmf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FilenmfHPP
