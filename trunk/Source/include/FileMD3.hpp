// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FileMD3.pas' rev: 24.00 (Win32)

#ifndef Filemd3HPP
#define Filemd3HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Filemd3
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TMD3Tag
{
public:
	System::StaticArray<char, 64> strName;
	Vectortypes::TVector3f vPosition;
	Vectortypes::TMatrix3f rotation;
};


struct DECLSPEC_DRECORD TMD3Bone
{
public:
	Vectortypes::TVector3f mins;
	Vectortypes::TVector3f maxs;
	Vectortypes::TVector3f position;
	float scale;
	System::StaticArray<char, 16> creator;
};


struct DECLSPEC_DRECORD TMD3Triangle
{
public:
	Vectortypes::TVector3s vertex;
	Vectortypes::TVector2b normal;
};


struct DECLSPEC_DRECORD TMD3Face
{
public:
	Vectortypes::TVector3i vertexIndices;
};


struct DECLSPEC_DRECORD TMD3TexCoord
{
public:
	Vectortypes::TVector2f textureCoord;
};


struct DECLSPEC_DRECORD TMD3Skin
{
public:
	System::StaticArray<char, 64> strName;
	int shaderIndex;
};


struct DECLSPEC_DRECORD TMD3Header
{
public:
	System::StaticArray<char, 4> fileID;
	int version;
	System::StaticArray<char, 64> strFile;
	int flags;
	int numFrames;
	int numTags;
	int numMeshes;
	int numMaxSkins;
	int headerSize;
	int tagStart;
	int tagEnd;
	int fileSize;
};


struct DECLSPEC_DRECORD TMD3MeshHeader
{
public:
	System::StaticArray<char, 4> meshID;
	System::StaticArray<char, 64> strName;
	int flags;
	int numMeshFrames;
	int numSkins;
	int numVertices;
	int numTriangles;
	int triStart;
	int headerSize;
	int uvStart;
	int vertexStart;
	int meshSize;
};


struct DECLSPEC_DRECORD TMD3MeshData
{
private:
	typedef System::DynamicArray<TMD3Skin> _TMD3MeshData__1;
	
	typedef System::DynamicArray<TMD3Face> _TMD3MeshData__2;
	
	typedef System::DynamicArray<TMD3TexCoord> _TMD3MeshData__3;
	
	typedef System::DynamicArray<TMD3Triangle> _TMD3MeshData__4;
	
	
public:
	TMD3MeshHeader MeshHeader;
	_TMD3MeshData__1 Skins;
	_TMD3MeshData__2 Triangles;
	_TMD3MeshData__3 TexCoords;
	_TMD3MeshData__4 Vertices;
};


class DELPHICLASS TFileMD3;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TFileMD3 : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<TMD3Bone> _TFileMD3__1;
	
	typedef System::DynamicArray<TMD3Tag> _TFileMD3__2;
	
	typedef System::DynamicArray<TMD3MeshData> _TFileMD3__3;
	
	
public:
	TMD3Header ModelHeader;
	_TFileMD3__1 Bones;
	_TFileMD3__2 Tags;
	_TFileMD3__3 MeshData;
	void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TObject.Create */ inline __fastcall TFileMD3(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TFileMD3(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Filemd3 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FILEMD3)
using namespace Filemd3;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Filemd3HPP
