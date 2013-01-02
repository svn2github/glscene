// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FileOCT.pas' rev: 24.00 (Win32)

#ifndef FileoctHPP
#define FileoctHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <VectorLists.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fileoct
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TOCTHeader
{
public:
	int numVerts;
	int numFaces;
	int numTextures;
	int numLightmaps;
	int numLights;
};


struct DECLSPEC_DRECORD TOCTVertex
{
public:
	Vectorgeometry::TTexPoint tv;
	Vectorgeometry::TTexPoint lv;
	Vectortypes::TVector3f pos;
};


struct DECLSPEC_DRECORD TOCTFace
{
public:
	int start;
	int num;
	int id;
	int lid;
	Vectortypes::TVector4f p;
};


typedef TOCTFace *POCTFace;

struct DECLSPEC_DRECORD TOCTTexture
{
public:
	int id;
	System::StaticArray<char, 64> Name;
};


struct DECLSPEC_DRECORD TOCTLightmap
{
public:
	int id;
	System::StaticArray<System::Byte, 49152> map;
};


typedef TOCTLightmap *POCTLightmap;

struct DECLSPEC_DRECORD TOCTLight
{
public:
	Vectortypes::TVector3f pos;
	Vectortypes::TVector3f color;
	int intensity;
};


class DELPHICLASS TOCTFile;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TOCTFile : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<TOCTVertex> _TOCTFile__1;
	
	typedef System::DynamicArray<TOCTFace> _TOCTFile__2;
	
	typedef System::DynamicArray<TOCTTexture> _TOCTFile__3;
	
	typedef System::DynamicArray<TOCTLightmap> _TOCTFile__4;
	
	typedef System::DynamicArray<TOCTLight> _TOCTFile__5;
	
	
public:
	TOCTHeader Header;
	_TOCTFile__1 Vertices;
	_TOCTFile__2 Faces;
	_TOCTFile__3 Textures;
	_TOCTFile__4 Lightmaps;
	_TOCTFile__5 Lights;
	Vectortypes::TVector3f PlayerPos;
	__fastcall TOCTFile(void)/* overload */;
	__fastcall TOCTFile(System::Classes::TStream* octStream)/* overload */;
	void __fastcall SaveToStream(System::Classes::TStream* aStream);
	void __fastcall AddTriangles(Vectorlists::TAffineVectorList* vertexCoords, Vectorlists::TAffineVectorList* texMapCoords, const System::UnicodeString textureName);
	void __fastcall AddLight(const Vectortypes::TVector3f &lightPos, const Vectortypes::TVector4f &lightColor, int lightIntensity);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TOCTFile(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Fileoct */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FILEOCT)
using namespace Fileoct;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FileoctHPP
