// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TypesMDC.pas' rev: 24.00 (Win32)

#ifndef TypesmdcHPP
#define TypesmdcHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <VectorTypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Typesmdc
{
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<float, 3> TMDCPoint;

typedef TMDCPoint TMDCAngle;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCFileHeader
{
public:
	System::StaticArray<char, 4> Ident;
	unsigned Version;
	System::StaticArray<char, 64> Name;
	unsigned Flags;
	unsigned NumFrames;
	unsigned NumTags;
	unsigned NumSurfaces;
	unsigned NumSkins;
	unsigned OffsetBorderFrames;
	unsigned OffsetTagNames;
	unsigned OffsetTagFrames;
	unsigned OffsetSurfaces;
	unsigned OffsetEnd;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCBorderFrame
{
public:
	TMDCPoint BBMin;
	TMDCPoint BBMax;
	TMDCPoint LocalOrigin;
	float Radius;
	System::StaticArray<char, 16> Name;
};
#pragma pack(pop)


struct TMDCTagName;
typedef TMDCTagName *PMDCTagName;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCTagName
{
public:
	System::StaticArray<char, 64> Name;
};
#pragma pack(pop)


struct TMDCTagFrame;
typedef TMDCTagFrame *PMDCTagFrame;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCTagFrame
{
public:
	System::StaticArray<System::Word, 3> TagPosition;
	System::StaticArray<System::Word, 3> TagAngle;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCTag
{
public:
	TMDCTagName *TagName;
	TMDCTagFrame *TagFrame;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCSurfaceHeader
{
public:
	System::StaticArray<char, 4> Ident;
	System::StaticArray<char, 64> Name;
	unsigned Flags;
	unsigned NumCompFrames;
	unsigned NumBaseFrames;
	unsigned NumSkins;
	unsigned NumVertices;
	unsigned NumTriangles;
	unsigned OffsetTriangles;
	unsigned OffsetSkins;
	unsigned OffsetTexCoords;
	unsigned OffsetBaseVerts;
	unsigned OffsetCompVerts;
	unsigned OffsetFrameBaseFrames;
	unsigned OffsetFrameCompFrames;
	unsigned OffsetEnd;
};
#pragma pack(pop)


typedef System::StaticArray<unsigned, 3> TMDCTriangle;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCSkin
{
public:
	System::StaticArray<char, 64> Shader;
	unsigned Flags;
};
#pragma pack(pop)


typedef System::StaticArray<float, 2> TMDCTexCoord;

typedef System::StaticArray<short, 4> TMDCBaseVertex;

typedef System::StaticArray<System::Byte, 4> TMDCCompVertex;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCBaseFrame
{
private:
	typedef System::DynamicArray<TMDCBaseVertex> _TMDCBaseFrame__1;
	
	
public:
	_TMDCBaseFrame__1 BaseVertices;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TMDCCompFrame
{
private:
	typedef System::DynamicArray<TMDCCompVertex> _TMDCCompFrame__1;
	
	
public:
	_TMDCCompFrame__1 CompVertices;
};
#pragma pack(pop)


//-- var, const, procedure ---------------------------------------------------
#define MDCFILE_IDENTITY L"IDPC"
static const System::Int8 MDCFILE_VERSION = System::Int8(0x2);
static const System::Extended MDC_BASEVERTEX_FACTOR = 1.562500E-02;
static const System::Extended MDC_COMPVERTEX_FACTOR = 4.687500E-02;
}	/* namespace Typesmdc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_TYPESMDC)
using namespace Typesmdc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TypesmdcHPP
