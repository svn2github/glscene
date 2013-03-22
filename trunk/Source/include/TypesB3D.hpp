// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TypesB3D.pas' rev: 24.00 (Win32)

#ifndef Typesb3dHPP
#define Typesb3dHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Typesb3d
{
//-- type declarations -------------------------------------------------------
enum TB3DChunkType : unsigned char { bctUnknown, bctHeader, bctTexture, bctBrush, bctNode, bctVertex, bctTriangle, bctMesh, bctBone, bctKeyFrame, bctAnimation };

struct TB3DChunk;
typedef TB3DChunk *PB3DChunk;

struct DECLSPEC_DRECORD TB3DChunk
{
public:
	System::StaticArray<System::WideChar, 4> chunk;
	int length;
};


struct TBB3DChunk;
typedef TBB3DChunk *PBB3DChunk;

struct DECLSPEC_DRECORD TBB3DChunk
{
public:
	int Version;
};


struct TTEXSChunk;
typedef TTEXSChunk *PTEXSChunk;

struct DECLSPEC_DRECORD TTEXSChunk
{
public:
	System::StaticArray<System::WideChar, 256> fileName;
	int flags;
	int blend;
	float x_pos;
	float y_pos;
	float x_scale;
	float y_scale;
	float rotation;
};


struct TBRUSChunk;
typedef TBRUSChunk *PBRUSChunk;

struct DECLSPEC_DRECORD TBRUSChunk
{
private:
	typedef System::DynamicArray<int> _TBRUSChunk__1;
	
	
public:
	int n_texs;
	System::StaticArray<System::WideChar, 256> name;
	float red;
	float green;
	float blue;
	float alpha;
	float shininess;
	int blend;
	int fx;
	_TBRUSChunk__1 texture_id;
};


struct TVertexData;
typedef TVertexData *PVertexData;

struct DECLSPEC_DRECORD TVertexData
{
private:
	typedef System::DynamicArray<float> _TVertexData__1;
	
	
public:
	TVertexData *next;
	float x;
	float y;
	float z;
	float nx;
	float ny;
	float nz;
	float red;
	float green;
	float blue;
	float alpha;
	_TVertexData__1 tex_coords;
};


struct TVRTSChunk;
typedef TVRTSChunk *PVRTSChunk;

struct DECLSPEC_DRECORD TVRTSChunk
{
public:
	int flags;
	int tex_coord_sets;
	int tex_coord_set_size;
	TVertexData *vertices;
};


struct TTRISChunk;
typedef TTRISChunk *PTRISChunk;

struct DECLSPEC_DRECORD TTRISChunk
{
private:
	typedef System::DynamicArray<int> _TTRISChunk__1;
	
	
public:
	TTRISChunk *next;
	int brush_id;
	_TTRISChunk__1 vertex_id;
};


struct TMESHChunk;
typedef TMESHChunk *PMESHChunk;

struct DECLSPEC_DRECORD TMESHChunk
{
public:
	int brush_id;
	TVRTSChunk vertices;
	TTRISChunk *triangles;
};


struct TBONEChunk;
typedef TBONEChunk *PBONEChunk;

struct DECLSPEC_DRECORD TBONEChunk
{
public:
	int vertex_id;
	float weight;
};


struct TKEYSChunk;
typedef TKEYSChunk *PKEYSChunk;

struct DECLSPEC_DRECORD TKEYSChunk
{
public:
	TKEYSChunk *next;
	int flags;
	int frame;
	Vectortypes::TVector3f position;
	Vectortypes::TVector3f scale;
	Vectortypes::TVector4f rotation;
};


struct TANIMChunk;
typedef TANIMChunk *PANIMChunk;

struct DECLSPEC_DRECORD TANIMChunk
{
public:
	int flags;
	int frames;
	float fps;
};


struct TNODEChunk;
typedef TNODEChunk *PNODEChunk;

struct DECLSPEC_DRECORD TNODEChunk
{
public:
	System::StaticArray<System::WideChar, 256> name;
	Vectortypes::TVector3f position;
	Vectortypes::TVector3f scale;
	Vectortypes::TVector4f rotation;
	TMESHChunk *meshes;
	TKEYSChunk *keys;
	TNODEChunk *nodes;
	TANIMChunk animation;
	TNODEChunk *next;
	int level;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Typesb3d */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_TYPESB3D)
using namespace Typesb3d;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Typesb3dHPP
