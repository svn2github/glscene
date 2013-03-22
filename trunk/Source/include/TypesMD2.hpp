// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TypesMD2.pas' rev: 24.00 (Win32)

#ifndef Typesmd2HPP
#define Typesmd2HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Typesmd2
{
//-- type declarations -------------------------------------------------------
struct TMD2VertexIndex;
typedef TMD2VertexIndex *PMD2VertexIndex;

struct DECLSPEC_DRECORD TMD2VertexIndex
{
public:
	int A;
	int B;
	int C;
	float A_S;
	float A_T;
	float B_S;
	float B_T;
	float C_S;
	float C_T;
};


struct DECLSPEC_DRECORD TMD2Triangle
{
public:
	Vectortypes::TVector3s VertexIndex;
	Vectortypes::TVector3s TextureCoordIndex;
};


struct DECLSPEC_DRECORD TMD2TriangleVertex
{
public:
	System::StaticArray<System::Byte, 3> V;
	System::Byte LightnormalIndex;
};


struct TMD2AliasFrame;
typedef TMD2AliasFrame *PMD2AliasFrame;

struct DECLSPEC_DRECORD TMD2AliasFrame
{
public:
	Vectortypes::TVector3f Scale;
	Vectortypes::TVector3f Translate;
	System::StaticArray<char, 16> Name;
	System::StaticArray<TMD2TriangleVertex, 1> Vertices;
};


struct DECLSPEC_DRECORD TMD2Header
{
public:
	int Ident;
	int Version;
	int SkinWidth;
	int SkinHeight;
	int FrameSize;
	int Num_Skins;
	int Num_Vertices;
	int Num_TextureCoords;
	int Num_VertexIndices;
	int Num_GLCommdands;
	int Num_Frames;
	int Offset_skins;
	int Offset_st;
	int Offset_tris;
	int Offset_frames;
	int Offset_glcmds;
	int Offset_end;
};


typedef System::DynamicArray<TMD2VertexIndex> TIndexList;

typedef System::DynamicArray<Vectortypes::TVector3f> Typesmd2__1;

typedef System::DynamicArray<System::DynamicArray<Vectortypes::TVector3f> > TVertexList;

//-- var, const, procedure ---------------------------------------------------
static const System::Word MAX_MD2_TRIANGLES = System::Word(0x1000);
static const System::Word MAX_MD2_VERTICES = System::Word(0x800);
static const System::Word MAX_MD2_FRAMES = System::Word(0x200);
static const System::Int8 MAX_MD2_SKINS = System::Int8(0x20);
static const System::Int8 MAX_MD2_SKINNAME = System::Int8(0x40);
}	/* namespace Typesmd2 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_TYPESMD2)
using namespace Typesmd2;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Typesmd2HPP
